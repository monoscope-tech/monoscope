module Pkg.Queue (pubsubService, kafkaService, KafkaRole (..), publishJSONToKafka, publishToDeadLetterQueue, closeSharedKafkaProducer) where

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
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as LT
import Data.Time (getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Effectful
import Gogol qualified as Google
import Gogol.Auth.ApplicationDefault qualified as Google
import Gogol.Data.Base64 (_Base64)
import Gogol.PubSub qualified as PubSub
import Kafka.Consumer qualified as K
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
import System.Types (ATBackgroundCtx, runBackground)
import UnliftIO (throwIO)
import UnliftIO.Async (pooledForConcurrentlyN)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket, tryAny)
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
            >>= liftIO
            . routeBatchOutcome appLogger appCtx "pubsub-service" topic validMsgs (maybeToMonoid firstAttrs)

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
publishRawToKafka :: AuthContext -> Text -> ByteString -> HM.HashMap Text Text -> IO (Either Text Text)
publishRawToKafka appCtx topicName messageData attributes = checkpoint "publishRawToKafka" do
  result <- tryAny $ checkpoint (toAnnotation $ "publishRawToKafka: " <> topicName) do
    producer <- getOrInitKafkaProducer appCtx.config
    let headers = KP.headersFromList $ map (\(k, v) -> (BC.pack $ toString k, BC.pack $ toString v)) $ HM.toList attributes
    let record =
          KP.ProducerRecord
            { prTopic = K.TopicName topicName
            , prPartition = KP.UnassignedPartition
            , prKey = Nothing
            , prValue = Just messageData
            , prHeaders = headers
            }
    sendResult <- KP.produceMessage producer record
    case sendResult of
      Just er -> pure $ Left $ "Failed to send message to Kafka: " <> toText (show er)
      _ -> pure $ Right ""
  case result of
    Left e -> pure $ Left $ toText $ show e
    Right res -> pure res


-- | Publish JSON-encoded structured data. ONLY for genuine JSON payloads —
-- never raw binary (protobuf): JSON-string-encoding quotes and escapes the
-- bytes into an undecodable blob (the DLQ uses 'publishRawToKafka' directly).
publishJSONToKafka :: AE.ToJSON a => AuthContext -> Text -> a -> HM.HashMap Text Text -> IO (Either Text Text)
publishJSONToKafka appCtx topicName jsonData = publishRawToKafka appCtx topicName (BC.toStrict $ AE.encode jsonData)


-- | Which consumer loop this is. 'KafkaDlqReplay' consumes the dead-letter
-- topic under its own consumer group (\"\<kafkaGroupId\>_dlq\" — a shared group
-- would rebalance DLQ partitions onto primary consumers). Failures there
-- republish to the DLQ tail with a bumped @monoscope-attempt-count@ header and
-- commit, so every message retries in a loop and none is ever dropped; a
-- growing DLQ with rising attempt counts is the signal for an engineer to
-- inspect and prune manually.
data KafkaRole = KafkaPrimary | KafkaDlqReplay
  deriving stock (Eq)


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
        bracket
          (either throwIO pure =<< K.newConsumer (consumerProps appCtx.config clientId) consumerSub)
          K.closeConsumer
          \consumer -> do
            LogBase.logInfo_ "Kafka consumer connected, starting message polling loop"
            forever do
              (leftRecords, rightRecords) <- partitionEithers <$> K.pollMessageBatch consumer (K.Timeout 100) (K.BatchSize batchSize)

              unless (null leftRecords)
                $ LogBase.logAttention "Kafka poll returned errors"
                $ AE.object ["error_count" AE..= length leftRecords, "errors" AE..= map show leftRecords]

              -- Group per (topic, partition) so groups can run concurrently
              -- below while each tpsFor still yields exactly one TP — failed
              -- groups stall only their own partition's commits.
              -- Map (not HashMap): KP.PartitionId has Ord but no Hashable instance.
              let byTP = foldr (\r -> Map.insertWith (<>) (r.crTopic.unTopicName, r.crPartition) (r :| [])) Map.empty rightRecords
                  attrsFor topic r = let h = consumerRecordHeadersToHashMap r in HM.insert "ce-type" (ceTypeFor appCtx.config.kafkaDeadLetterTopic topic h) h
                  -- One fn invocation + outcome routing for records sharing one
                  -- header set. True ⇔ every record is durable (written or DLQ'd)
                  -- and its offset may advance.
                  runChunk topic records attrs = do
                    let ceType = HM.lookupDefault "" "ce-type" attrs
                    LogBase.localData ["topic" AE..= topic, "ce_type" AE..= ceType, "record_count" AE..= length records] do
                      result <-
                        liftIO
                          $ tryAny
                          $ runBackground appLogger appCtx tp
                          $ runBatchSpan
                            "kafka.process_batch"
                            [ ("topic", OA.toAttribute topic)
                            , ("message_count", OA.toAttribute (length records))
                            , ("ce-type", OA.toAttribute ceType)
                            , ("client_id", OA.toAttribute clientId)
                            ]
                            (fn records attrs)
                      -- Empty ack list = no commit (broker redelivers); non-empty
                      -- = durable. Kafka can't partial-ack within a partition so
                      -- processGroup commits all-or-nothing per partition group.
                      not . null <$> liftIO (routeBatchOutcome appLogger appCtx "kafka-service" topic records attrs result)
                  processGroup ((topic, _partition), neRecords@(recc :| _)) = do
                    committable <- case role of
                      KafkaPrimary -> runChunk topic (consumerRecordToTuple <$> toList neRecords) (attrsFor topic recc)
                      -- DLQ partitions interleave requeues from different source
                      -- topics (round-robin publish), so one batch-level ce-type
                      -- would mis-decode the minority and bake the wrong ce-type
                      -- into their requeued headers. Process per record under its
                      -- own headers; short-circuit on the first non-durable record
                      -- (the rest redelivers with it — duplicates over loss).
                      KafkaDlqReplay -> allM (\r -> runChunk topic [consumerRecordToTuple r] (attrsFor topic r)) (toList neRecords)
                    pure $ if committable then tpsFor topic neRecords else []
              -- Bound must stay well under the hasql pool (shared with web) —
              -- AcquisitionTimeout (→ Hasql.isTransientException → no commit →
              -- redelivery storm) is the failure mode to avoid. Single committer
              -- (this thread) below — no concurrent commitPartitionsOffsets.
              tps <- concat <$> pooledForConcurrentlyN appCtx.config.kafkaGroupConcurrency (Map.toList byTP) processGroup

              -- No poll-thread backoff on primary topics: lag growth is the outage
              -- signal under per-partition commits. A reintroduced `threadDelay`
              -- here races max.poll.interval.ms and triggers cooperative-sticky
              -- rebalances.
              unless (null tps)
                $ whenJustM (K.commitPartitionsOffsets K.OffsetCommitAsync consumer tps) throwIO

              -- DLQ replay is a requeue carousel: failures republish to the tail
              -- and commit, so on a short queue a poison message would otherwise
              -- cycle consume→fail→requeue several times a second. Pause when the
              -- poll came back short (at the tail, i.e. mostly just-requeued
              -- records); full batches — a real backlog drain — run at full
              -- speed. 30s stays well under max.poll.interval.ms (300s).
              when (role == KafkaDlqReplay && length rightRecords < batchSize)
                $ threadDelay 30_000_000
  where
    groupId = case role of
      KafkaPrimary -> appCtx.config.kafkaGroupId
      KafkaDlqReplay -> appCtx.config.kafkaGroupId <> "_dlq"

    consumerRecordToTuple :: K.ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> (Text, ByteString)
    consumerRecordToTuple record = (record.crTopic.unTopicName, fromMaybe "" record.crValue)

    consumerRecordHeadersToHashMap :: K.ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> HashMap Text Text
    consumerRecordHeadersToHashMap record = HM.fromList $ map (bimap decodeUtf8 decodeUtf8) (K.headersToList record.crHeaders)

    -- Max consumed offset + 1 per partition for a topic-group's records, shaped
    -- for K.commitPartitionsOffsets. The >= 0 guard rules out librdkafka
    -- sentinels (OffsetEnd = -1, OffsetInvalid) which shouldn't appear in
    -- pollMessageBatch output but would rewind us to the start if committed.
    tpsFor :: Foldable f => Text -> f (K.ConsumerRecord k v) -> [K.TopicPartition]
    tpsFor topic neRecords =
      [ K.TopicPartition (K.TopicName topic) p (K.PartitionOffset o)
      | (p, o) <-
          Map.toList
            $ Map.fromListWith
              max
              [(r.crPartition, K.unOffset r.crOffset + 1) | r <- toList neRecords, K.unOffset r.crOffset >= 0]
      ]

    -- Dead-letter messages carry ce-type in headers (PubSub path) or derive from original-topic (Kafka path).
    ceTypeFor :: Text -> Text -> HM.HashMap Text Text -> Text
    ceTypeFor deadLetterTopic topic headers
      | topic == deadLetterTopic = fromMaybe (maybe "" topicToCeType (HM.lookup "original-topic" headers)) (HM.lookup "ce-type" headers)
      | otherwise = topicToCeType topic

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

    topicToCeType :: Text -> Text
    topicToCeType topic = case topic of
      "otlp_spans" -> "org.opentelemetry.otlp.traces.v1"
      "otlp_logs" -> "org.opentelemetry.otlp.logs.v1"
      "otlp_metrics" -> "org.opentelemetry.otlp.metrics.v1"
      _ -> ""


-- | Headers stamped on every DLQ publish. Left-biased '<>' keeps first-failure
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
dlqHeaders errorReason failedAt origTopicOrAckId attributes =
  HM.fromList
    [ ("monoscope-attempt-count", show (1 + fromMaybe 0 (readMaybe . toString =<< HM.lookup "monoscope-attempt-count" attributes) :: Int))
    , ("error-reason", errorReason)
    , ("last-failed-at", failedAt)
    ]
    <> attributes
    <> HM.fromList
      [ ("original-topic", origTopicOrAckId)
      , ("original-ack-id", origTopicOrAckId)
      , ("failed-at", failedAt)
      ]


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
publishToDeadLetterQueue :: Log.Logger -> AuthContext -> [(Text, ByteString)] -> HM.HashMap Text Text -> Text -> IO (Either Text ())
publishToDeadLetterQueue appLogger appCtx messages attributes errorReason = do
  let deadLetterTopic = appCtx.config.kafkaDeadLetterTopic
  currentTime <- getCurrentTime
  results <- forM messages \(origTopicOrAckId, msgData) -> do
    let deadLetterAttrs = dlqHeaders errorReason (toText $ show currentTime) origTopicOrAckId attributes
    -- Raw message bytes verbatim — NEVER via publishJSONToKafka, which would
    -- JSON-string-encode the binary protobuf into an undecodable, re-escaping blob.
    result <- publishRawToKafka appCtx deadLetterTopic msgData deadLetterAttrs
    case result of
      Left err -> do
        runLogT "dlq" appLogger LogAttention
          $ LogBase.logAttention "publishToDeadLetterQueue: failed to publish"
          $ AE.object ["error" AE..= err, "topic" AE..= deadLetterTopic, "original_topic" AE..= origTopicOrAckId]
        pure (Left err)
      Right _ -> pure (Right ())
  -- sequence_ over [Either Text ()] yields the FIRST Left (and pure ()
  -- otherwise). Subsequent failures only appear in the dlq log above.
  pure $ sequence_ results


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
  :: Log.Logger
  -> AuthContext
  -> Text
  -- ^ service name for log context ("pubsub-service" / "kafka-service")
  -> Text
  -- ^ topic for log context
  -> [(Text, ByteString)]
  -- ^ original validMsgs (used when we have to DLQ the whole batch)
  -> HM.HashMap Text Text
  -- ^ base attrs (ce-type etc.)
  -> Either SomeException (Either Telemetry.WriteFailure ([Text], [Telemetry.PoisonMsg]))
  -> IO [Text]
routeBatchOutcome appLogger appCtx svc topic validMsgs attrs = \case
  Right (Right (writeAcks, [])) -> pure writeAcks
  Right (Right (writeAcks, poison)) -> do
    let poisonBytes = [(ackId, raw) | (ackId, raw, _) <- poison]
        poisonAckIds = [ackId | (ackId, _, _) <- poison]
        firstReason = maybe "" (\(_, _, r) -> r) (listToMaybe poison)
        poisonAttrs = attrs <> one ("monoscope-poison-reason", firstReason)
    runLogT svc appLogger LogAttention
      $ LogBase.logAttention "routeBatchOutcome: poison messages → DLQ"
      $ AE.object ["topic" AE..= topic, "poison_count" AE..= length poison, "write_acks" AE..= length writeAcks]
    publishToDeadLetterQueue appLogger appCtx poisonBytes poisonAttrs firstReason >>= \case
      Right _ -> pure (writeAcks <> poisonAckIds)
      Left _ -> pure [] -- broker redelivers; on retry the writes are repeated (need idempotency)
  Right (Left wf) -> do
    let summary = Telemetry.writeFailureSummary wf
        dlqAttrs = attrs <> Telemetry.writeFailureDlqHeaders wf
    runLogT svc appLogger LogAttention
      $ LogBase.logAttention "routeBatchOutcome: write failure → DLQ"
      $ AE.object ["topic" AE..= topic, "write_failure" AE..= summary, "message_count" AE..= length validMsgs]
    publishToDeadLetterQueue appLogger appCtx validMsgs dlqAttrs summary >>= \case
      Right _ -> pure (map fst validMsgs)
      Left _ -> pure []
  Left e -> do
    let errText = toText (show e)
    runLogT svc appLogger LogAttention
      $ LogBase.logAttention "routeBatchOutcome: non-write exception"
      $ AE.object ["topic" AE..= topic, "error" AE..= errText, "message_count" AE..= length validMsgs]
    if Hasql.isTransientException e
      then pure []
      else
        publishToDeadLetterQueue appLogger appCtx validMsgs attrs errText >>= \case
          Right _ -> pure (map fst validMsgs)
          Left _ -> pure []
