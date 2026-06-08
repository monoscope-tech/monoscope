module Pkg.Queue (pubsubService, kafkaService, publishJSONToKafka, publishToDeadLetterQueue, closeSharedKafkaProducer) where

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
import OpenTelemetry.Attributes qualified as OA
import OpenTelemetry.Trace (TracerProvider)
import Relude
import System.Config
import System.IO.Unsafe (unsafePerformIO)
import System.Tracing (SpanStatus (..), addEvent, setStatus, withSpan)
import System.Types (ATBackgroundCtx, runBackground)
import UnliftIO (throwIO)
import UnliftIO.Async (pooledForConcurrentlyN)
import UnliftIO.Exception (bracket, try, tryAny)
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
        <> KP.extraProp "message.max.bytes" "52428800"
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


-- pubsubService connects to the pubsub service and listens for  messages,
-- then it calls the processMessage function to process the messages, and
-- acknoleges the list message in one request.
pubsubService :: Log.Logger -> AuthContext -> TracerProvider -> [Text] -> ([(Text, ByteString)] -> HM.HashMap Text Text -> ATBackgroundCtx [Text]) -> IO ()
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
                $ withSpan
                  "pubsub.process_batch"
                  [ ("topic", OA.toAttribute topic)
                  , ("message_count", OA.toAttribute (length validMsgs))
                  , ("subscription", OA.toAttribute subscription)
                  , ("ce-type", OA.toAttribute (fromMaybe "" ceType))
                  ]
                  \sp -> do
                    addEvent sp "batch.started" []
                    result <- try $ fn validMsgs (maybeToMonoid firstAttrs)
                    case result of
                      Left (e :: SomeException) -> do
                        let !errText = toText $ show e
                        addEvent sp "batch.failed" [("error", OA.toAttribute errText)]
                        setStatus sp (Error errText)
                        throwIO e
                      Right ids -> do
                        addEvent sp "batch.completed" [("processed_count", OA.toAttribute (length ids))]
                        setStatus sp Ok
                        pure ids
            )
            >>= \case
              Left e -> do
                let !errText = toText $ show e
                liftIO
                  $ runLogT "pubsub-service" appLogger LogAttention
                  $ LogBase.logAttention "pubsubService: error processing batch"
                  $ AE.object ["error" AE..= errText, "topic" AE..= topic, "message_count" AE..= length validMsgs]
                -- Transient (Hasql infra) → ack nothing; PubSub redelivers
                -- after the ack deadline. Anything else is treated as poison
                -- → park in DLQ; ack only if DLQ accepted.
                if Hasql.isTransientException e
                  then pure []
                  else do
                    dlqRes <- liftIO $ publishToDeadLetterQueue appLogger appCtx validMsgs (maybeToMonoid firstAttrs) errText
                    case dlqRes of
                      Right _ -> pure $ map fst validMsgs
                      Left _ -> pure []
              Right ids -> pure ids

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


publishJSONToKafka :: AE.ToJSON a => AuthContext -> Text -> a -> HM.HashMap Text Text -> IO (Either Text Text)
publishJSONToKafka appCtx topicName jsonData attributes = checkpoint "publishJSONToKafka" do
  result <- tryAny $ checkpoint (toAnnotation $ "publishJSONToKafka: " <> topicName) do
    producer <- getOrInitKafkaProducer appCtx.config
    let messageData = BC.toStrict $ LT.encodeUtf8 $ LT.decodeUtf8 $ AE.encode jsonData
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


kafkaService :: Log.Logger -> AuthContext -> TracerProvider -> Text -> [Text] -> Int -> ([(Text, ByteString)] -> HM.HashMap Text Text -> ATBackgroundCtx [Text]) -> IO ()
kafkaService appLogger appCtx tp groupId kafkaTopics batchSize fn = checkpoint "kafkaService" do
  instanceUuid <- UUID.toText <$> UUID.nextRandom
  let clientId = "monoscope-" <> T.take 8 instanceUuid
      -- DLQ replay runs under a separate group (see Server.hs) so its
      -- partitions don't get rebalanced onto primary consumers. Failures on
      -- DLQ messages drop+log (below) instead of re-DLQ'ing — see processGroup.
      consumerSub = K.topics (map K.TopicName kafkaTopics) <> K.offsetReset K.Earliest
      isDlqConsumer = kafkaTopics == [appCtx.config.kafkaDeadLetterTopic]

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
                  processGroup ((topic, _partition), neRecords@(recc :| _)) = do
                    let headers = consumerRecordHeadersToHashMap recc
                        ceType = ceTypeFor appCtx.config.kafkaDeadLetterTopic topic headers
                        attributes = HM.insert "ce-type" ceType headers
                        allRecords = consumerRecordToTuple <$> toList neRecords
                        successTps = tpsFor topic neRecords
                    LogBase.localData ["topic" AE..= topic, "ce_type" AE..= ceType, "record_count" AE..= length allRecords] do
                      result <- liftIO
                        $ tryAny
                        $ runBackground appLogger appCtx tp
                        $ withSpan
                          "kafka.process_batch"
                          [ ("topic", OA.toAttribute topic)
                          , ("message_count", OA.toAttribute (length allRecords))
                          , ("ce-type", OA.toAttribute ceType)
                          , ("client_id", OA.toAttribute clientId)
                          ]
                        $ \sp -> do
                          addEvent sp "batch.started" []
                          res <- try $ fn allRecords attributes
                          case res of
                            Left (e :: SomeException) -> do
                              let !errText = toText $ show e
                              addEvent sp "batch.failed" [("error", OA.toAttribute errText)]
                              setStatus sp (Error errText)
                              throwIO e
                            Right ids -> do
                              addEvent sp "batch.completed" [("processed_count", OA.toAttribute (length ids))]
                              setStatus sp Ok
                              pure ids
                      case result of
                        Right _ -> pure successTps
                        Left e -> do
                          let !errText = toText $ show e
                          LogBase.logAttention "kafkaService: error processing batch" (AE.object ["error" AE..= errText, "attributes" AE..= attributes])
                          -- Hasql transient → don't commit (lag grows, librdkafka
                          -- redelivers). Anything else is poison → park in DLQ
                          -- and commit only if the DLQ publish succeeded.
                          if Hasql.isTransientException e
                            then pure []
                            else
                              if isDlqConsumer
                                then do
                                  -- DLQ replay: a poison message that failed on the
                                  -- primary topic AND again here is permanent. Drop +
                                  -- commit (otherwise it loops forever). Original
                                  -- error-reason / failed-at headers preserve forensics.
                                  LogBase.logAttention "kafkaService: permanent failure on DLQ replay — dropping" (AE.object ["error" AE..= errText, "attributes" AE..= attributes, "record_count" AE..= length allRecords])
                                  pure successTps
                                else do
                                  dlqRes <- liftIO $ publishToDeadLetterQueue appLogger appCtx allRecords attributes errText
                                  pure $ case dlqRes of
                                    Right _ -> successTps
                                    Left _ -> []
              -- Bound must stay well under the hasql pool (shared with web) —
              -- AcquisitionTimeout (→ Hasql.isTransientException → no commit →
              -- redelivery storm) is the failure mode to avoid. Single committer
              -- (this thread) below — no concurrent commitPartitionsOffsets.
              tps <- concat <$> pooledForConcurrentlyN appCtx.config.kafkaGroupConcurrency (Map.toList byTP) processGroup

              -- No poll-thread backoff: lag growth is the outage signal under
              -- per-partition commits. A reintroduced `threadDelay` here races
              -- max.poll.interval.ms and triggers cooperative-sticky rebalances.
              unless (null tps)
                $ whenJustM (K.commitPartitionsOffsets K.OffsetCommitAsync consumer tps) throwIO
  where
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
        <> K.extraProp "fetch.min.bytes" "1024"
        <> K.extraProp "partition.assignment.strategy" "cooperative-sticky"
        <> K.extraProp "group.instance.id" clientId
        <> K.logLevel K.KafkaLogInfo

    topicToCeType :: Text -> Text
    topicToCeType topic = case topic of
      "otlp_spans" -> "org.opentelemetry.otlp.traces.v1"
      "otlp_logs" -> "org.opentelemetry.otlp.logs.v1"
      "otlp_metrics" -> "org.opentelemetry.otlp.metrics.v1"
      _ -> ""


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
    let deadLetterAttrs =
          attributes
            <> HM.fromList
              [ ("original-topic", origTopicOrAckId)
              , ("original-ack-id", origTopicOrAckId)
              , ("error-reason", errorReason)
              , ("failed-at", toText $ show currentTime)
              ]
    result <- publishJSONToKafka appCtx deadLetterTopic (BC.unpack msgData) deadLetterAttrs
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
