module Pkg.Queue (
  pubsubService,
  kafkaService,
  KafkaRole (..),
  decoupledLoop,
  ConsumerEff,
  publishJSONToKafka,
  publishToDeadLetterQueue,
  runSharedProducer,
  closeSharedKafkaProducer,
  -- exposed for unit tests: the pure consumer logic
  chunksByBytes,
  PartProgress (..),
  completeOffsets,
  retryTiers,
  parkingTopicFor,
  RetryRoute (..),
  retryDestination,
) where

import Control.Concurrent.STM.TBQueue (newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception.Annotated (checkpoint)
import Control.Lens ((^?), _Just)
import Control.Lens qualified as L
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson qualified as AE
import Data.Annotation (toAnnotation)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy.Base64 qualified as LB64
import Data.Effectful.Hasql qualified as Hasql
import Data.Generics.Product (field)
import Data.HashMap.Strict qualified as HM
import Data.IntSet qualified as IntSet
import Data.List.Extra (minimum)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as LT
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError, tryError)
import Effectful.Ki qualified as Ki
import Effectful.Log (Log)
import Effectful.Time qualified as Time
import Gogol qualified as Google
import Gogol.Auth.ApplicationDefault qualified as Google
import Gogol.Data.Base64 (_Base64)
import Gogol.PubSub qualified as PubSub
import Kafka.Consumer qualified as K
import Kafka.Effectful qualified as KE
import Kafka.Effectful.Producer.Effect qualified as KEP
import Kafka.Producer qualified as KP
import Log (LogLevel (..), Logger, runLogT)
import Log qualified as LogBase
import Models.Telemetry.Telemetry qualified as Telemetry
import OpenTelemetry.Attributes qualified as OA
import OpenTelemetry.Trace (TracerProvider)
import Relude
import System.Config
import System.IO.Unsafe (unsafePerformIO)
import System.Tracing (SpanStatus (..), addEvent, setStatus, withSpan)
import System.Types (ATBackgroundCtx, ATBackgroundEffects, runBackground)
import UnliftIO (throwIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (tryAny)
import UnliftIO.MVar (modifyMVar)


-- Process-wide cached Kafka producer. librdkafka producers are heavyweight
-- (TCP, SASL handshake, idempotence PID acquisition) and leak resources if
-- created per message. We initialize one lazily and reuse for the lifetime
-- of the process. Holds an Either so a failed init is retried next call.
{-# NOINLINE sharedKafkaProducer #-}
sharedKafkaProducer :: MVar (Maybe KP.KafkaProducer)
sharedKafkaProducer = unsafePerformIO (newMVar Nothing)


getOrInitKafkaProducer :: EnvConfig -> IO KP.KafkaProducer
getOrInitKafkaProducer envCfg = modifyMVar sharedKafkaProducer $ \case
  Just p -> pure (Just p, p)
  Nothing -> do
    p <- either throwIO pure =<< KP.newProducer (kafkaProducerProps envCfg)
    pure (Just p, p)
  where
    -- librdkafka producer settings. linger.ms/batch.size are tuned aggressively
    -- because the producer is now reused process-wide (see sharedKafkaProducer);
    -- larger batches amortize SASL/TCP/idempotence overhead across many messages.
    kafkaProducerProps :: EnvConfig -> KP.ProducerProperties
    kafkaProducerProps cfg =
      KP.brokersList (map K.BrokerAddress cfg.kafkaBrokers)
        <> KP.extraProp "security.protocol" "sasl_plaintext"
        <> KP.extraProp "sasl.mechanism" "SCRAM-SHA-256"
        <> KP.extraProp "sasl.username" cfg.kafkaUsername
        <> KP.extraProp "sasl.password" cfg.kafkaPassword
        <> KP.extraProp "acks" "all"
        <> KP.extraProp "retries" "2147483647"
        <> KP.extraProp "max.in.flight.requests.per.connection" "5"
        <> KP.extraProp "enable.idempotence" "true"
        <> KP.extraProp "compression.type" "snappy"
        <> KP.extraProp "batch.size" "1048576" -- 1 MiB batches (was 16 KiB)
        <> KP.extraProp "linger.ms" "50" -- wait up to 50ms to coalesce (was 5ms)
        <> KP.extraProp "queue.buffering.max.messages" "1000000"
        <> KP.extraProp "queue.buffering.max.kbytes" "1048576" -- 1 GiB librdkafka queue cap
        <> KP.extraProp "request.timeout.ms" "30000"
        <> KP.extraProp "delivery.timeout.ms" "120000"
        -- Must track the broker's kafka_batch_max_bytes / topic max.message.bytes
        -- (64 MiB). DLQ re-publishes re-stamp headers onto messages already near
        -- the original cap; a tighter limit rejects the enqueue (MsgSizeTooLarge,
        -- a librdkafka-local per-message check) and wedges the partition.
        <> KP.extraProp "message.max.bytes" "67108864"
        <> KP.extraProp "receive.message.max.bytes" "104857600"
        <> KP.logLevel KP.KafkaLogInfo


-- | Flush any in-flight librdkafka batches and release the cached producer.
-- Safe to call multiple times. Intended for graceful shutdown.
closeSharedKafkaProducer :: IO ()
closeSharedKafkaProducer = modifyMVar sharedKafkaProducer $ \case
  Nothing -> pure (Nothing, ())
  Just p -> do
    -- 30s flush window covers delivery.timeout.ms worst-case for queued batches
    _ <- tryAny $ KP.flushProducer p
    _ <- tryAny $ KP.closeProducer p
    pure (Nothing, ())


-- | Interpret kafka-effectful's 'KE.KafkaProducer' effect against the
-- process-wide cached producer, rather than creating one per scope (the
-- librdkafka producer is heavyweight; web handlers publish per-request). All
-- producers — consumer DLQ path, PubSub, replay — go through this; tests swap in
-- a no-op interpreter. ProduceMessage throws 'K.KafkaError' via the Error effect
-- on enqueue failure, so the caller refuses to advance the offset/ack.
runSharedKafkaProducer :: (Error K.KafkaError :> es, IOE :> es) => AuthContext -> Eff (KE.KafkaProducer ': es) a -> Eff es a
runSharedKafkaProducer appCtx = interpret \_ -> \case
  KEP.ProduceMessage rec -> liftIO (getOrInitKafkaProducer appCtx.config >>= \p -> KP.produceMessage p rec) >>= maybe pass throwError
  KEP.FlushProducer -> liftIO (getOrInitKafkaProducer appCtx.config >>= KP.flushProducer)
  KEP.AskProducerHandle -> liftIO (getOrInitKafkaProducer appCtx.config)
  _ -> error "runSharedKafkaProducer: unsupported KafkaProducer operation"


-- | One-shot producer for callers not already in a producer scope (web
-- handlers): runs the action against the shared producer and surfaces a
-- KafkaError as @Left@ text. Wraps the producer + error interpreters so callers
-- need no Kafka imports.
runSharedProducer :: IOE :> es => AuthContext -> Eff (KE.KafkaProducer ': Error K.KafkaError ': es) a -> Eff es (Either Text a)
runSharedProducer appCtx act = first (toText . show) <$> runErrorNoCallStack @K.KafkaError (runSharedKafkaProducer appCtx act)


-- | Shared batch-processing span for both ingest paths: wraps the fn invocation
-- in a withSpan with batch.started/completed/write_failed events and Ok/Error
-- status. Only the span name and attribute list differ between pubsub and kafka.
runBatchSpan
  :: Text
  -> [(Text, OA.Attribute)]
  -> ATBackgroundCtx (Either Telemetry.WriteFailure ([Text], [Telemetry.PoisonMsg]))
  -> ATBackgroundCtx (Either Telemetry.WriteFailure ([Text], [Telemetry.PoisonMsg]))
runBatchSpan spanName attrs run = withSpan spanName attrs \sp -> do
  addEvent sp "batch.started" []
  res <- run
  case res of
    Right (ids, poison) -> do
      addEvent sp "batch.completed" [("processed_count", OA.toAttribute (length ids)), ("poison_count", OA.toAttribute (length poison))]
      setStatus sp Ok
      pure (Right (ids, poison))
    Left wf -> do
      let !summary = Telemetry.writeFailureSummary wf
      addEvent sp "batch.write_failed" [("write_failure", OA.toAttribute summary)]
      setStatus sp (Error summary)
      pure (Left wf)


-- pubsubService connects to the pubsub service and listens for  messages,
-- then it calls the processMessage function to process the messages, and
-- acknoleges the list message in one request.
pubsubService :: Log.Logger -> AuthContext -> TracerProvider -> [Text] -> ([(Text, ByteString)] -> HM.HashMap Text Text -> ATBackgroundCtx (Either Telemetry.WriteFailure ([Text], [Telemetry.PoisonMsg]))) -> IO ()
pubsubService appLogger appCtx tp topics fn = checkpoint "pubsubService" do
  let envConfig = appCtx.config
  env <- case envConfig.googleServiceAccountB64 of
    "" -> Google.newEnv <&> (Google.envScopes L..~ pubSubScope)
    sa -> do
      let credJSON = either error id $ LB64.decodeBase64Untyped (LT.encodeUtf8 sa)
      let credsE = Google.fromJSONCredentials credJSON
      let creds = either (error . toText) id credsE
      managerG <- Google.newManager Google.tlsManagerSettings
      Google.newEnvWith creds (\_ _ -> pass) managerG <&> (Google.envScopes L..~ pubSubScope)

  let pullReq = PubSub.newPullRequest & field @"maxMessages" L.?~ fromIntegral envConfig.messagesPerPubsubPullBatch

  forever
    $ runResourceT
    $ forM topics \topic -> do
      result <- tryAny $ checkpoint (toAnnotation $ "pubsubTopic loop: " <> topic) do
        let subscription = "projects/past-3/subscriptions/" <> topic <> "-sub"
        pullResp <- Google.send env $ PubSub.newPubSubProjectsSubscriptionsPull pullReq subscription
        let messages = fromMaybe [] (pullResp L.^. field @"receivedMessages")
        let !validMsgs =
              mapMaybe
                ( \msg -> do
                    ackId <- msg.ackId
                    b64Msg <- msg ^? field @"message" . _Just . field @"data'" . _Just . _Base64
                    Just (ackId, b64Msg)
                )
                messages
        let firstAttrs = messages ^? L.folded . field @"message" . _Just . field @"attributes" . _Just . field @"additional"
        let ceType = HM.lookup "ce-type" (maybeToMonoid firstAttrs)

        msgIds <-
          tryAny
            ( liftIO
                $ runBackground appLogger appCtx tp
                $ runBatchSpan
                  "pubsub.process_batch"
                  [ ("topic", OA.toAttribute topic)
                  , ("message_count", OA.toAttribute (length validMsgs))
                  , ("subscription", OA.toAttribute subscription)
                  , ("ce-type", OA.toAttribute (fromMaybe "" ceType))
                  ]
                  (fn validMsgs (maybeToMonoid firstAttrs))
            )
            >>= \result ->
              liftIO
                $ fromRight []
                <$> runBackground appLogger appCtx tp (runErrorNoCallStack @K.KafkaError (runSharedKafkaProducer appCtx (routeBatchOutcome appCtx "pubsub-service" topic validMsgs (maybeToMonoid firstAttrs) result)))

        let acknowlegReq = PubSub.newAcknowledgeRequest & field @"ackIds" L..~ Just msgIds
        unless (null msgIds) $ void $ PubSub.newPubSubProjectsSubscriptionsAcknowledge acknowlegReq subscription & Google.send env
      case result of
        Left e ->
          liftIO
            $ runLogT "pubsub-service" appLogger LogAttention
            $ LogBase.logAttention "pubsubService: outer loop exception" (show e)
        Right _ -> pass
  where
    pubSubScope :: Proxy '["https://www.googleapis.com/auth/pubsub"]
    pubSubScope = Proxy


-- | Publish a value to a Kafka topic verbatim, with the given headers. The
-- bytes are written exactly as given — structured-data callers must encode
-- themselves ('publishJSONToKafka'); binary callers (the DLQ) pass the original
-- message bytes so the consumer decodes them unchanged.
publishRawToKafka :: KE.KafkaProducer :> es => Text -> ByteString -> HM.HashMap Text Text -> Eff es ()
publishRawToKafka topicName messageData attributes =
  KE.produceMessage
    KP.ProducerRecord
      { prTopic = K.TopicName topicName
      , prPartition = KP.UnassignedPartition
      , prKey = Nothing
      , prValue = Just messageData
      , prHeaders = KP.headersFromList $ map (\(k, v) -> (BC.pack $ toString k, BC.pack $ toString v)) $ HM.toList attributes
      }


-- | Publish JSON-encoded structured data. ONLY for genuine JSON payloads —
-- never raw binary (protobuf): JSON-string-encoding quotes and escapes the
-- bytes into an undecodable blob (the DLQ uses 'publishRawToKafka' directly).
publishJSONToKafka :: (AE.ToJSON a, KE.KafkaProducer :> es) => Text -> a -> HM.HashMap Text Text -> Eff es ()
publishJSONToKafka topicName jsonData = publishRawToKafka topicName (BC.toStrict $ AE.encode jsonData)


-- | Which consumer loop this is. 'KafkaDlqReplay' consumes the dead-letter
-- topic under its own consumer group (\"\<kafkaGroupId\>_dlq\" — a shared group
-- would rebalance DLQ partitions onto primary consumers). Failures there
-- republish to the DLQ tail with a bumped @monoscope-attempt-count@ header and
-- commit, so every message retries in a loop and none is ever dropped; a
-- growing DLQ with rising attempt counts is the signal for an engineer to
-- inspect and prune manually.
data KafkaRole = KafkaPrimary | KafkaDlqReplay
  deriving stock (Eq)


-- | Greedy split of an offset-ordered group into sub-chunks whose serialized
-- payloads sum to ~targetBytes. Always emits at least one element per chunk, so
-- a single over-target record becomes its own chunk (no infinite loop, no
-- dropped record). Strict byte accumulator — no thunk buildup over a large batch.
--
-- >>> map (map fst) $ chunksByBytes 10 snd [('a',6),('b',5),('c',3),('d',20),('e',1)]
-- ["a","bc","d","e"]
chunksByBytes :: Int -> (a -> Int) -> [a] -> [[a]]
chunksByBytes target size = go [] 0
  where
    go acc _ [] = [reverse acc | not (null acc)]
    go acc !n (x : xs)
      | null acc = go [x] (size x) xs
      | n + size x > target = reverse acc : go [x] (size x) xs
      | otherwise = go (x : acc) (n + size x) xs


-- | Per-sub-chunk payload-byte target. One sub-chunk becomes one bulk INSERT /
-- Delta write; sizing by bytes (not record count) bounds write granularity and
-- keeps Delta file sizes off the small-files OOM path.
kafkaChunkTargetBytes :: Int
kafkaChunkTargetBytes = 64 * 1024 * 1024


data RetryRoute = RetryTier Text Int | Parking Text
  deriving stock (Eq, Show)


-- | Escalating retry tiers derived from the base DLQ topic. Tier 0 is the base
-- topic itself (first failures still land where ops already watch), then backoff
-- topics with growing min-delays (seconds). Ops must provision the -retry-*
-- and -parking topics. Attempts past the last tier are terminal: they go to the
-- parking topic and are never auto-replayed (DLQ stays a forensic archive).
retryTiers :: Text -> [(Text, Int)]
retryTiers base = [(base, 5), (base <> "-retry-60s", 60), (base <> "-retry-600s", 600)]


parkingTopicFor :: Text -> Text
parkingTopicFor base = base <> "-parking"


-- | Destination for a message that has now failed @attempt@ times (1-based):
-- the matching tier, or the parking topic once the tiers are exhausted.
--
-- >>> retryDestination (retryTiers "dlq") (parkingTopicFor "dlq") 1
-- RetryTier "dlq" 5
-- >>> retryDestination (retryTiers "dlq") (parkingTopicFor "dlq") 3
-- RetryTier "dlq-retry-600s" 600
-- >>> retryDestination (retryTiers "dlq") (parkingTopicFor "dlq") 4
-- Parking "dlq-parking"
retryDestination :: [(Text, Int)] -> Text -> Int -> RetryRoute
retryDestination tiers park attempt = maybe (Parking park) (uncurry RetryTier) (tiers !!? (attempt - 1))


-- | Per-partition commit progress for the decoupled committer. @base@ is the
-- next offset to commit (every offset below it is durable); @ahead@ holds
-- completed offsets ≥ @base@ not yet contiguous (a gap sits at @base@).
-- Self-pruning: offsets leave @ahead@ as @base@ walks over them, so one stalled
-- offset can't make the set grow without bound.
data PartProgress = PartProgress {base :: !Int64, ahead :: !IntSet.IntSet}
  deriving stock (Eq, Show)


-- | Record completed offsets, advancing @base@ across the now-contiguous run.
-- Offsets below @base@ (already committed — e.g. a redelivered duplicate) drop
-- out. Strict throughout (foldl' + IntSet): no thunk accretion over a long run.
--
-- >>> (completeOffsets [10,11,12] (PartProgress 10 mempty)).base
-- 13
-- >>> let p = completeOffsets [12,11] (PartProgress 10 mempty)
-- >>> (p.base, IntSet.toList p.ahead)
-- (10,[11,12])
-- >>> (completeOffsets [10] p).base
-- 13
completeOffsets :: [Int64] -> PartProgress -> PartProgress
completeOffsets offs (PartProgress base0 ahead0) = advance base0 (foldl' ins ahead0 offs)
  where
    ins s o = if o >= base0 then IntSet.insert (fromIntegral o) s else s
    advance b s = case IntSet.minView s of
      Just (m, s') | fromIntegral m == b -> advance (b + 1) s'
      _ -> PartProgress b s


-- | Pure committer step: for each partition whose watermark advanced past the
-- last committed offset, emit a commit at the new base and carry it forward. The
-- committed map dedupes — an unchanged partition isn't re-committed each tick.
--
-- >>> let tr = Map.fromList [(("t", K.PartitionId 0), PartProgress 5 mempty), (("t", K.PartitionId 1), PartProgress 0 mempty)]
-- >>> [(t, o) | K.TopicPartition (K.TopicName t) _ (K.PartitionOffset o) <- fst (committableCommits tr mempty)]
-- [("t",5)]
committableCommits
  :: Map (Text, K.PartitionId) PartProgress
  -> Map (Text, K.PartitionId) Int64
  -> ([K.TopicPartition], Map (Text, K.PartitionId) Int64)
committableCommits tracker committed =
  ( [K.TopicPartition (K.TopicName t) p (K.PartitionOffset b) | ((t, p), b) <- advanced]
  , foldl' (\m (tp, b) -> Map.insert tp b m) committed advanced
  )
  where
    advanced = [(tp, b) | (tp, PartProgress b _) <- Map.toList tracker, b > 0, b > Map.findWithDefault (-1) tp committed]


kafkaService :: Log.Logger -> AuthContext -> TracerProvider -> KafkaRole -> Text -> [Text] -> Int -> ([(Text, ByteString)] -> HM.HashMap Text Text -> ATBackgroundCtx (Either Telemetry.WriteFailure ([Text], [Telemetry.PoisonMsg]))) -> IO ()
kafkaService appLogger appCtx tp role label kafkaTopics batchSize fn = checkpoint "kafkaService" do
  -- client.id / group.instance.id: a human-readable label (which consumer this
  -- is) plus a short nonce to keep the two identical in-process ingest
  -- consumers distinct. This is the CLIENT-ID/INSTANCE-ID `rpk group describe`
  -- shows; the broker still appends its own UUID to form the long member.id.
  nonce <- T.take 6 . UUID.toText <$> UUID.nextRandom
  let clientId = "mono-" <> label <> "-" <> nonce
      consumerSub = K.topics (map K.TopicName kafkaTopics) <> K.offsetReset K.Earliest

  -- Single LogT context for the whole service; client_id (and other static
  -- consumer metadata) is attached via localData so every log entry below
  -- carries it without per-call duplication.
  runLogT "kafka-service" appLogger LogInfo
    $ LogBase.localData
      [ "client_id" AE..= clientId
      , "group_id" AE..= groupId
      ]
      do
        LogBase.logInfo "Starting Kafka consumer service" (AE.object ["topics" AE..= kafkaTopics, "brokers" AE..= appCtx.config.kafkaBrokers])
        -- KE.runKafkaConsumer owns connect/close and interprets the KafkaConsumer
        -- effect; the loop runs in ATBackgroundCtx extended with that effect so it
        -- can still call `fn` directly (via inject). A broker error surfaces as a
        -- Left from runErrorNoCallStack and is rethrown to the supervisor.
        liftIO
          $ either throwIO pure
          =<< runBackground appLogger appCtx tp (runErrorNoCallStack @K.KafkaError (runSharedKafkaProducer appCtx (KE.runKafkaConsumer (consumerProps appCtx.config clientId) consumerSub (decoupledLoop appLogger appCtx tp role batchSize clientId fn))))
  where
    groupId = case role of
      KafkaPrimary -> appCtx.config.kafkaGroupId
      KafkaDlqReplay -> appCtx.config.kafkaGroupId <> "_dlq"

    consumerProps :: EnvConfig -> Text -> K.ConsumerProperties
    consumerProps cfg clientId =
      K.brokersList (map K.BrokerAddress cfg.kafkaBrokers)
        <> K.groupId (K.ConsumerGroupId groupId)
        <> K.clientId (K.ClientId clientId)
        <> K.extraProp "security.protocol" "sasl_plaintext"
        <> K.extraProp "sasl.mechanism" "SCRAM-SHA-256"
        <> K.extraProp "sasl.username" cfg.kafkaUsername
        <> K.extraProp "sasl.password" cfg.kafkaPassword
        <> K.extraProp "session.timeout.ms" "45000"
        <> K.extraProp "heartbeat.interval.ms" "3000"
        <> K.extraProp "max.poll.interval.ms" "300000"
        -- Let the broker coalesce a fetch up to 64 KiB (or 250ms, whichever first)
        -- before responding, instead of returning on the first 1 KiB available.
        -- Fatter fetches fill each poll batch → larger per-partition groups →
        -- fewer, bigger bulk INSERTs (amortizes the dominant per-batch DB
        -- round-trip cost). 250ms stays well under session/poll timeouts.
        <> K.extraProp "fetch.min.bytes" "65536"
        <> K.extraProp "fetch.wait.max.ms" "250"
        <> K.extraProp "partition.assignment.strategy" "cooperative-sticky"
        <> K.extraProp "group.instance.id" clientId
        <> K.logLevel K.KafkaLogInfo


-- | The consumer loop's effect stack: ATBackgroundCtx plus the kafka-effectful
-- KafkaConsumer effect (poll/commit/pause/resume/assignment) and the Error
-- channel its interpreter throws into. Production runs it under
-- 'KE.runKafkaConsumer'; tests swap in an in-memory interpreter (the whole point
-- of going through the effect).
type ConsumerEff = Eff (KE.KafkaConsumer ': KE.KafkaProducer ': Error K.KafkaError ': ATBackgroundEffects)


-- | The Kafka consumer loop (primary ingest + DLQ replay), over the
-- 'KE.KafkaConsumer' effect. Three Ki threads share one effect env: workers
-- drain a bounded queue and process sub-chunks; the poll thread never blocks on
-- the DB and pauses partitions when in-flight bytes are high (resumes when they
-- drain) so processing latency can't stall heartbeats into a rebalance; a 1s
-- committer drains the per-partition watermark and commits the contiguous offset
-- prefix across polls (consumer metrics ride the same tick). Decision logic is
-- pure (subChunksFor / chunksByBytes / completeOffsets / committableCommits);
-- only poll/commit/pause/assignment touch Kafka. Role differs only in how a poll
-- becomes sub-chunks (subChunksFor); everything downstream is shared.
decoupledLoop
  :: Log.Logger
  -> AuthContext
  -> TracerProvider
  -> KafkaRole
  -> Int
  -> Text
  -> ([(Text, ByteString)] -> HM.HashMap Text Text -> ATBackgroundCtx (Either Telemetry.WriteFailure ([Text], [Telemetry.PoisonMsg])))
  -> ConsumerEff ()
decoupledLoop appLogger appCtx tp role batchSize clientId fn = do
  let highWaterBytes = 4 * kafkaChunkTargetBytes
      lowWaterBytes = kafkaChunkTargetBytes
      deadLetterTopic = appCtx.config.kafkaDeadLetterTopic
  workQ <- liftIO (newTBQueueIO 64)
  trackerVar <- newTVarIO (Map.empty :: Map (Text, K.PartitionId) PartProgress)
  committedVar <- newTVarIO (Map.empty :: Map (Text, K.PartitionId) Int64)
  inflightVar <- newTVarIO (0 :: Int)
  pausedVar <- newTVarIO False
  let assigned = KE.assignment <&> \m -> [(t, p) | (t, ps) <- Map.toList m, p <- ps]
      worker = forever do
        (tpKey, recc, tuples, offs, bytes) <- atomically (readTBQueue workQ)
        let topic = fst tpKey
            attrs = attrsFor deadLetterTopic topic recc
        -- `inject` lifts the ATBackgroundCtx processing action into the
        -- KafkaConsumer-extended stack; workers never touch the Kafka effect.
        result <-
          tryAny
            $ inject
            $ runBatchSpan
              "kafka.process_batch"
              [("topic", OA.toAttribute topic), ("message_count", OA.toAttribute (length tuples)), ("client_id", OA.toAttribute clientId)]
              (fn tuples attrs)
        acks <- routeBatchOutcome appCtx "kafka-service" topic tuples attrs result
        atomically do
          unless (null acks) $ modifyTVar' trackerVar (Map.adjust (completeOffsets offs) tpKey)
          modifyTVar' inflightVar (subtract bytes)
      committer = forever do
        threadDelay 1_000_000
        (commits, tracked, inflight) <- atomically do
          (cs, committed') <- committableCommits <$> readTVar trackerVar <*> readTVar committedVar
          writeTVar committedVar committed'
          (cs,,) . Map.size <$> readTVar trackerVar <*> readTVar inflightVar
        -- Commit throws KafkaError into the Error effect on failure (no commit →
        -- redelivery). Metrics emit on progress OR while work is in flight, so a
        -- stall (commits dry up but inflight_bytes stays high) is visible.
        unless (null commits) $ KE.commitPartitionsOffsets K.OffsetCommitAsync commits
        when (not (null commits) || inflight > 0)
          $ LogBase.logInfo "kafka.consumer.metrics"
          $ AE.object ["client_id" AE..= clientId, "committed_partitions" AE..= length commits, "tracked_partitions" AE..= tracked, "inflight_bytes" AE..= inflight]
      pollOnce = do
        inflight <- readTVarIO inflightVar
        paused <- readTVarIO pausedVar
        when (inflight >= highWaterBytes && not paused) do
          ps <- assigned
          unless (null ps) $ KE.pausePartitions ps
          atomically (writeTVar pausedVar True)
        when (inflight <= lowWaterBytes && paused) do
          ps <- assigned
          unless (null ps) $ KE.resumePartitions ps
          atomically (writeTVar pausedVar False)
        (pollErrs, rs) <- partitionEithers <$> KE.pollMessageBatch (K.Timeout 100) (K.BatchSize batchSize)
        unless (null pollErrs) $ LogBase.logAttention "Kafka poll returned errors" (AE.object ["error_count" AE..= length pollErrs, "errors" AE..= map show pollErrs])
        nowEpoch <- floor . utcTimeToPOSIXSeconds <$> Time.currentTime
        let byTP = Map.fromListWith (<>) [((r.crTopic.unTopicName, r.crPartition), r :| []) | r <- rs]
            subChunks = subChunksFor role nowEpoch byTP
        for_ subChunks \work@(tpKey, _, _, offs, bytes) ->
          atomically do
            modifyTVar' trackerVar (Map.insertWith (\_ old -> old) tpKey (PartProgress (minimum offs) mempty))
            modifyTVar' inflightVar (+ bytes)
            writeTBQueue workQ work
        -- DLQ backoff: records present but none due → avoid a 10Hz re-poll of
        -- not-yet-due retries. Primary never hits this (chunks ≥1 when records
        -- exist) and uses lag, not naps, as its outage signal.
        when (role == KafkaDlqReplay && null subChunks && not (null rs)) (threadDelay 5_000_000)
  Ki.scoped \scope -> do
    replicateM_ (max 1 appCtx.config.kafkaGroupConcurrency) (Ki.fork_ scope worker)
    Ki.fork_ scope committer
    forever pollOnce
  where
    attrsFor :: Text -> Text -> K.ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> HM.HashMap Text Text
    attrsFor deadLetterTopic topic r = let h = consumerRecordHeadersToHashMap r in HM.insert "ce-type" (ceTypeFor deadLetterTopic topic h) h

    consumerRecordHeadersToHashMap :: K.ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> HashMap Text Text
    consumerRecordHeadersToHashMap record = HM.fromList $ map (bimap decodeUtf8 decodeUtf8) (K.headersToList record.crHeaders)

    -- Dead-letter messages carry ce-type in headers (PubSub path) or derive from original-topic (Kafka path).
    ceTypeFor :: Text -> Text -> HM.HashMap Text Text -> Text
    ceTypeFor deadLetterTopic topic headers
      | topic == deadLetterTopic = fromMaybe (maybe "" topicToCeType (HM.lookup "original-topic" headers)) (HM.lookup "ce-type" headers)
      | otherwise = topicToCeType topic

    topicToCeType :: Text -> Text
    topicToCeType topic = case topic of
      "otlp_spans" -> "org.opentelemetry.otlp.traces.v1"
      "otlp_logs" -> "org.opentelemetry.otlp.logs.v1"
      "otlp_metrics" -> "org.opentelemetry.otlp.metrics.v1"
      _ -> ""


-- | Turn one poll's records into work items (tpKey, header-record, payloads,
-- offsets, bytes). Primary: byte-sized chunks per partition under the
-- partition-head's headers. DLQ replay: one chunk per *due* record under its own
-- headers — retry tiers interleave source topics with different ce-types, and
-- offset = failure-time = due-time order, so due records are a prefix;
-- not-yet-due ones stay uncommitted and redeliver (duplicates over loss).
subChunksFor
  :: KafkaRole
  -> Int
  -> Map (Text, K.PartitionId) (NonEmpty (K.ConsumerRecord (Maybe ByteString) (Maybe ByteString)))
  -> [((Text, K.PartitionId), K.ConsumerRecord (Maybe ByteString) (Maybe ByteString), [(Text, ByteString)], [Int64], Int)]
subChunksFor role nowEpoch byTP = case role of
  KafkaPrimary ->
    [ (tpKey, recc, consumerRecordToTuple <$> chunk, K.unOffset . (.crOffset) <$> chunk, sum (recordBytes <$> chunk))
    | (tpKey, recs@(recc :| _)) <- Map.toList byTP
    , chunk <- chunksByBytes kafkaChunkTargetBytes recordBytes (sortOn (.crOffset) (toList recs))
    ]
  KafkaDlqReplay ->
    [ (tpKey, r, [consumerRecordToTuple r], [K.unOffset r.crOffset], recordBytes r)
    | (tpKey, recs) <- Map.toList byTP
    , r <- takeWhile (isDue nowEpoch) (sortOn (.crOffset) (toList recs))
    ]
  where
    -- A retry-tier message is due once now ≥ its stamped due epoch. Missing /
    -- unparseable header ⇒ due (process rather than strand it).
    isDue :: Int -> K.ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> Bool
    isDue now r = maybe True (now >=) (readMaybe . toString . decodeUtf8 @Text . snd =<< find ((== "monoscope-next-due-at") . fst) (K.headersToList r.crHeaders))

    consumerRecordToTuple :: K.ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> (Text, ByteString)
    consumerRecordToTuple record = (record.crTopic.unTopicName, fromMaybe "" record.crValue)

    recordBytes :: K.ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> Int
    recordBytes = maybe 0 BC.length . (.crValue)


-- | Publish failed messages to dead letter queue. Returns the first Left if
-- any individual publish failed so the caller can refuse to advance the
-- upstream offset/ack — otherwise a broken DLQ silently drops poison data.
--
-- Partial-failure semantics: if N-1 messages publish and the last fails, this
-- returns Left and the caller holds the whole batch's offsets. On the next
-- poll librdkafka redelivers all N messages and the N-1 already-DLQ'd ones
-- get DLQ'd again, duplicating in DLQ. The trade — duplicate DLQ entries
-- vs. losing poison data — favors duplicates: DLQ is rare and replay tooling
-- can dedupe by 'original-ack-id'.
publishToDeadLetterQueue :: (KE.KafkaProducer :> es, Time.Time :> es) => AuthContext -> [(Text, ByteString)] -> HM.HashMap Text Text -> Text -> Eff es ()
publishToDeadLetterQueue appCtx messages attributes errorReason = do
  let base = appCtx.config.kafkaDeadLetterTopic
      tiers = retryTiers base
      park = parkingTopicFor base
  currentTime <- Time.currentTime
  let nowEpoch = floor (utcTimeToPOSIXSeconds currentTime) :: Int
  -- Throws K.KafkaError on the first enqueue failure (via the producer
  -- interpreter); the caller catches it and refuses to advance offsets.
  for_ messages \(origTopicOrAckId, msgData) -> do
    -- dlqHeaders bumps monoscope-attempt-count; route by the *new* count to the
    -- matching retry tier (or parking once exhausted) and stamp the per-message
    -- due time so the replay consumer enforces the tier's backoff.
    let baseAttrs = dlqHeaders errorReason (toText $ show currentTime) origTopicOrAckId attributes
        attempt = fromMaybe 1 (readMaybe . toString =<< HM.lookup "monoscope-attempt-count" baseAttrs)
        (topic, attrs) = case retryDestination tiers park attempt of
          RetryTier t delay -> (t, HM.insert "monoscope-next-due-at" (show (nowEpoch + delay)) baseAttrs)
          Parking t -> (t, baseAttrs)
    -- Raw message bytes verbatim — NEVER via publishJSONToKafka, which would
    -- JSON-string-encode the binary protobuf into an undecodable, re-escaping blob.
    publishRawToKafka topic msgData attrs
  where
    -- \| Headers stamped on every DLQ publish. Left-biased '<>' keeps first-failure
    -- forensics (original-topic, original-ack-id, failed-at) intact across
    -- DLQ-replay requeues, while attempt-count, error-reason and last-failed-at
    -- refresh on every pass — an engineer inspecting a growing DLQ sees how often
    -- and why each message keeps failing.
    --
    -- >>> let requeued = dlqHeaders "tf down" "t1" "otlp_logs" mempty
    -- >>> let again = dlqHeaders "pg down" "t2" "monoscope-dlq" requeued
    -- >>> (HM.lookup "monoscope-attempt-count" again, HM.lookup "error-reason" again, HM.lookup "last-failed-at" again)
    -- (Just "2",Just "pg down",Just "t2")
    -- >>> (HM.lookup "original-topic" again, HM.lookup "failed-at" again)
    -- (Just "otlp_logs",Just "t1")
    dlqHeaders :: Text -> Text -> Text -> HM.HashMap Text Text -> HM.HashMap Text Text
    dlqHeaders reason failedAt origTopicOrAckId priorHeaders =
      HM.fromList
        [ ("monoscope-attempt-count", show (1 + fromMaybe 0 (readMaybe . toString =<< HM.lookup "monoscope-attempt-count" priorHeaders) :: Int))
        , ("error-reason", reason)
        , ("last-failed-at", failedAt)
        ]
        <> priorHeaders
        <> HM.fromList
          [ ("original-topic", origTopicOrAckId)
          , ("original-ack-id", origTopicOrAckId)
          , ("failed-at", failedAt)
          ]


-- | Decide which source-topic acks to commit given a batch's outcome.
-- Returns an empty list when nothing should be committed (broker redelivers).
--
-- Semantics:
--   * Right (writeAcks, []) — happy path; commit writeAcks.
--   * Right (writeAcks, poison) — DLQ poison first; commit writeAcks ++ poisonAckIds
--     only if DLQ accepted. On DLQ failure, commit nothing → broker redelivers.
--   * Left WriteFailure — bounded retries exhausted inside retryHasqlWrite.
--     DLQ the whole batch with side-tracking headers; commit only if DLQ accepted.
--   * Left SomeException (non-write) — Hasql-transient: no commit, broker redelivers.
--     Otherwise: DLQ the whole batch as opaque poison; commit only if DLQ accepted.
routeBatchOutcome
  :: (Error K.KafkaError :> es, KE.KafkaProducer :> es, Log :> es, Time.Time :> es)
  => AuthContext
  -> Text
  -- ^ service name for log context ("pubsub-service" / "kafka-service")
  -> Text
  -- ^ topic for log context
  -> [(Text, ByteString)]
  -- ^ original validMsgs (used when we have to DLQ the whole batch)
  -> HM.HashMap Text Text
  -- ^ base attrs (ce-type etc.)
  -> Either SomeException (Either Telemetry.WriteFailure ([Text], [Telemetry.PoisonMsg]))
  -> Eff es [Text]
routeBatchOutcome appCtx svc topic validMsgs attrs = \case
  Right (Right (writeAcks, [])) -> pure writeAcks
  Right (Right (writeAcks, poison)) -> do
    let poisonBytes = [(ackId, raw) | (ackId, raw, _) <- poison]
        poisonAckIds = [ackId | (ackId, _, _) <- poison]
        firstReason = maybe "" (\(_, _, r) -> r) (listToMaybe poison)
        poisonAttrs = attrs <> one ("monoscope-poison-reason", firstReason)
    LogBase.logAttention "routeBatchOutcome: poison messages → DLQ"
      $ AE.object ["service" AE..= svc, "topic" AE..= topic, "poison_count" AE..= length poison, "write_acks" AE..= length writeAcks]
    -- DLQ publish throws KafkaError on enqueue failure; on failure commit nothing
    -- → broker redelivers (writes repeated on retry; need idempotency downstream).
    tryError @K.KafkaError (publishToDeadLetterQueue appCtx poisonBytes poisonAttrs firstReason) >>= \case
      Right () -> pure (writeAcks <> poisonAckIds)
      Left _ -> pure []
  Right (Left wf) -> do
    let summary = Telemetry.writeFailureSummary wf
        dlqAttrs = attrs <> Telemetry.writeFailureDlqHeaders wf
    LogBase.logAttention "routeBatchOutcome: write failure → DLQ"
      $ AE.object ["service" AE..= svc, "topic" AE..= topic, "write_failure" AE..= summary, "message_count" AE..= length validMsgs]
    tryError @K.KafkaError (publishToDeadLetterQueue appCtx validMsgs dlqAttrs summary) >>= \case
      Right () -> pure (map fst validMsgs)
      Left _ -> pure []
  Left e -> do
    let errText = toText (show e)
    LogBase.logAttention "routeBatchOutcome: non-write exception"
      $ AE.object ["service" AE..= svc, "topic" AE..= topic, "error" AE..= errText, "message_count" AE..= length validMsgs]
    if Hasql.isTransientException e
      then pure []
      else
        tryError @K.KafkaError (publishToDeadLetterQueue appCtx validMsgs attrs errText) >>= \case
          Right () -> pure (map fst validMsgs)
          Left _ -> pure []
