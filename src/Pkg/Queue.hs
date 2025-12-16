module Pkg.Queue (pubsubService, kafkaService, publishJSONToKafka, publishToDeadLetterQueue) where

import Control.Exception.Annotated (checkpoint)
import Control.Lens ((^?), _Just)
import Control.Lens qualified as L
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson qualified as AE
import Data.Annotation (toAnnotation)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy.Base64 qualified as LB64
import Data.Generics.Product (field)
import Data.HashMap.Strict qualified as HM
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
import System.Tracing (SpanStatus (..), addEvent, setStatus, withSpan)
import System.Types (ATBackgroundCtx, runBackground)
import UnliftIO (throwIO)
import UnliftIO.Exception (bracket, try, tryAny)


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
        let msgsB64 =
              messages & map \msg -> do
                ackId <- msg.ackId
                b64Msg <- msg ^? field @"message" . _Just . field @"data'" . _Just . _Base64
                Just (ackId, b64Msg)
        let firstAttrs = messages ^? L.folded . field @"message" . _Just . field @"attributes" . _Just . field @"additional"
        let ceType = HM.lookup "ce-type" (maybeToMonoid firstAttrs)

        msgIds <-
          tryAny
            ( liftIO
                $ runBackground appLogger appCtx tp
                $ withSpan
                  "pubsub.process_batch"
                  [ ("topic", OA.toAttribute topic)
                  , ("message_count", OA.toAttribute (length (catMaybes msgsB64)))
                  , ("subscription", OA.toAttribute subscription)
                  , ("ce-type", OA.toAttribute (fromMaybe "" ceType))
                  ]
                $ \sp -> do
                  addEvent sp "batch.started" []
                  result <- try $ fn (catMaybes msgsB64) (maybeToMonoid firstAttrs)
                  case result of
                    Left (e :: SomeException) -> do
                      addEvent sp "batch.failed" [("error", OA.toAttribute $ toText $ show e)]
                      setStatus sp (Error $ toText $ show e)
                      throwIO e -- Re-throw so outer tryAny catches it
                    Right ids -> do
                      addEvent sp "batch.completed" [("processed_count", OA.toAttribute (length ids))]
                      setStatus sp Ok
                      pure ids
            )
            >>= \case
              Left e -> do
                liftIO
                  $ runLogT "monoscope" appLogger LogAttention
                  $ LogBase.logAttention "pubsubService: CAUGHT EXCEPTION - Error processing messages" (AE.object ["error" AE..= show e, "message_count" AE..= length (catMaybes msgsB64), "first_attrs" AE..= firstAttrs, "checkpoint" AE..= ("pubsubService:exception" :: String)])

                -- Send to dead letter queue unless it's an unrecoverable error
                unless (isUnrecoverableError e)
                  $ liftIO
                  $ publishToDeadLetterQueue appCtx (catMaybes msgsB64) (maybeToMonoid firstAttrs) (toText $ show e)

                -- Always return all message IDs so they're acknowledged
                pure $ map fst $ catMaybes msgsB64
              Right ids -> pure ids

        let acknowlegReq = PubSub.newAcknowledgeRequest & field @"ackIds" L..~ Just msgIds
        unless (null msgIds) $ void $ PubSub.newPubSubProjectsSubscriptionsAcknowledge acknowlegReq subscription & Google.send env
      case result of
        Left e -> do
          liftIO $ runLogT "monoscope" appLogger LogAttention $ LogBase.logAttention "Run Pubsub exception" (show e)
          pass
        Right _ -> pass
  where
    -- pubSubScope :: Proxy PubSub.Pubsub'FullControl
    pubSubScope :: Proxy '["https://www.googleapis.com/auth/pubsub"]
    pubSubScope = Proxy


publishJSONToKafka :: AE.ToJSON a => AuthContext -> Text -> a -> HM.HashMap Text Text -> IO (Either Text Text)
publishJSONToKafka appCtx topicName jsonData attributes = checkpoint "publishJSONToKafka" do
  result <- tryAny
    $ bracket
      (either throwIO pure =<< KP.newProducer (producerProps appCtx.config))
      KP.closeProducer
    $ \producer -> checkpoint (toAnnotation $ "publishJSONToKafka: " <> topicName) do
      let messageData = BC.toStrict $ LT.encodeUtf8 $ LT.decodeUtf8 $ AE.encode jsonData

      let headers = KP.headersFromList $ map (\(k, v) -> (BC.pack $ toString k, BC.pack $ toString v)) $ HM.toList attributes

      let record =
            KP.ProducerRecord
              { prTopic = K.TopicName topicName
              , prPartition = KP.UnassignedPartition -- Let Kafka choose partition
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
  where
    producerProps :: EnvConfig -> KP.ProducerProperties
    producerProps cfg =
      KP.brokersList (map K.BrokerAddress cfg.kafkaBrokers)
        <> KP.extraProp "security.protocol" "sasl_plaintext"
        <> KP.extraProp "sasl.mechanism" "SCRAM-SHA-256"
        <> KP.extraProp "sasl.username" cfg.kafkaUsername
        <> KP.extraProp "sasl.password" cfg.kafkaPassword
        <> KP.extraProp "acks" "all" -- Wait for all replicas to acknowledge
        <> KP.extraProp "retries" "2147483647" -- Max retries
        <> KP.extraProp "max.in.flight.requests.per.connection" "5"
        <> KP.extraProp "enable.idempotence" "true" -- Exactly-once semantics
        <> KP.extraProp "compression.type" "snappy" -- Compress messages
        <> KP.extraProp "batch.size" "16384" -- Batch size in bytes
        <> KP.extraProp "linger.ms" "5" -- Wait up to 5ms to batch messages
        <> KP.extraProp "request.timeout.ms" "30000" -- 30 second timeout
        <> KP.extraProp "delivery.timeout.ms" "120000" -- 2 minute total timeout
        <> KP.extraProp "max.request.size" "52428800"
        <> KP.extraProp "message.max.bytes" "52428800"
        <> KP.extraProp "receive.message.max.bytes" "104857600"
        <> KP.logLevel KP.KafkaLogInfo


kafkaService :: Log.Logger -> AuthContext -> TracerProvider -> [Text] -> ([(Text, ByteString)] -> HM.HashMap Text Text -> ATBackgroundCtx [Text]) -> IO ()
kafkaService appLogger appCtx tp kafkaTopics fn = checkpoint "kafkaService" do
  -- Generate unique client ID for logging/metrics
  instanceUuid <- UUID.toText <$> UUID.nextRandom
  let clientId = "monoscope-" <> T.take 8 instanceUuid
  -- Include dead letter topic in the list of topics to consume
  let allTopics = kafkaTopics <> [appCtx.config.kafkaDeadLetterTopic]

  runLogT "kafka-service" appLogger LogInfo
    $ LogBase.logInfo "Starting Kafka consumer service"
    $ AE.object
      [ "client_id" AE..= clientId
      , "topics" AE..= kafkaTopics
      , "all_topics" AE..= allTopics
      , "brokers" AE..= appCtx.config.kafkaBrokers
      , "group_id" AE..= appCtx.config.kafkaGroupId
      ]

  let consumerSub = K.topics (map K.TopicName allTopics) <> K.offsetReset K.Earliest
  bracket
    (either throwIO pure =<< K.newConsumer (consumerProps appCtx.config clientId) consumerSub)
    K.closeConsumer
    $ \consumer -> do
      runLogT "kafka-service" appLogger LogInfo
        $ LogBase.logInfo "Kafka consumer connected successfully, starting message polling loop"
        $ AE.object ["client_id" AE..= clientId]
      forever do
        pollResult@(leftRecords, rightRecords) <- partitionEithers <$> K.pollMessageBatch consumer (K.Timeout 100) (K.BatchSize appCtx.config.messagesPerPubsubPullBatch) -- timeout in milliseconds

        -- Log polling errors if any
        unless (null leftRecords) $ do
          runLogT "kafka-service" appLogger LogAttention
            $ LogBase.logAttention "Kafka poll returned errors"
            $ AE.object
              [ "error_count" AE..= length leftRecords
              , "errors" AE..= map show leftRecords
              ]

        case rightRecords of
          [] -> pass
          (recc : _) -> do
            let topic = recc.crTopic.unTopicName
                headers = consumerRecordHeadersToHashMap recc
                -- For dead letter queue messages, get ce-type from headers or derive from original-topic
                ceType =
                  if topic == appCtx.config.kafkaDeadLetterTopic
                    then case HM.lookup "ce-type" headers of
                      Just existingCeType -> existingCeType -- PubSub messages have ce-type
                      Nothing -> maybe "" topicToCeType (HM.lookup "original-topic" headers) -- Kafka messages need derivation
                    else topicToCeType topic
                attributes = HM.insert "ce-type" ceType headers
                allRecords = consumerRecordToTuple <$> rightRecords

            msgIds <-
              tryAny
                ( runBackground appLogger appCtx tp
                    $ withSpan
                      "kafka.process_batch"
                      [ ("topic", OA.toAttribute topic)
                      , ("message_count", OA.toAttribute (length allRecords))
                      , ("ce-type", OA.toAttribute ceType)
                      , ("client_id", OA.toAttribute clientId)
                      ]
                    $ \sp -> do
                      addEvent sp "batch.started" []
                      result <- try $ fn allRecords attributes
                      case result of
                        Left (e :: SomeException) -> do
                          addEvent sp "batch.failed" [("error", OA.toAttribute $ toText $ show e)]
                          setStatus sp (Error $ toText $ show e)
                          throwIO e -- Re-throw so outer tryAny catches it
                        Right ids -> do
                          addEvent sp "batch.completed" [("processed_count", OA.toAttribute (length ids))]
                          setStatus sp Ok
                          pure ids
                )
                >>= \case
                  Left e -> do
                    runLogT "monoscope" appLogger LogAttention
                      $ LogBase.logAttention "kafkaService: CAUGHT EXCEPTION - Error processing Kafka messages" (AE.object ["error" AE..= show e, "topic" AE..= topic, "ce-type" AE..= ceType, "record_count" AE..= length allRecords, "attributes" AE..= attributes, "checkpoint" AE..= ("kafkaService:exception" :: String)])

                    -- Send to dead letter queue unless it's an unrecoverable error
                    unless (isUnrecoverableError e)
                      $ liftIO
                      $ publishToDeadLetterQueue appCtx allRecords attributes (toText $ show e)

                    -- Always return all message IDs so they're acknowledged
                    pure $ map fst allRecords
                  Right ids -> pure ids

            whenJustM (K.commitAllOffsets K.OffsetCommitAsync consumer) throwIO
        pass
  where
    -- TODO: How should errors and retries be handled and implemented?

    consumerRecordToTuple :: K.ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> (Text, ByteString)
    consumerRecordToTuple record = (record.crTopic.unTopicName, fromMaybe "" record.crValue)

    consumerRecordHeadersToHashMap :: K.ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> HashMap Text Text
    consumerRecordHeadersToHashMap record = HM.fromList $ map headerToTuple (K.headersToList record.crHeaders)
      where
        headerToTuple :: (ByteString, ByteString) -> (Text, Text)
        headerToTuple (key, value) = (fromString $ BC.unpack key, fromString $ BC.unpack value)

    consumerProps :: EnvConfig -> Text -> K.ConsumerProperties
    consumerProps cfg clientId =
      K.brokersList (map K.BrokerAddress cfg.kafkaBrokers)
        <> K.groupId (K.ConsumerGroupId cfg.kafkaGroupId)
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

    -- \| Maps Kafka topic names to their corresponding CloudEvent types
    topicToCeType :: Text -> Text
    topicToCeType topic = case topic of
      "otlp_spans" -> "org.opentelemetry.otlp.traces.v1"
      "otlp_logs" -> "org.opentelemetry.otlp.logs.v1"
      "otlp_metrics" -> "org.opentelemetry.otlp.metrics.v1"
      _ -> ""


-- | Publish failed messages to dead letter queue
publishToDeadLetterQueue :: AuthContext -> [(Text, ByteString)] -> HM.HashMap Text Text -> Text -> IO ()
publishToDeadLetterQueue appCtx messages attributes errorReason = do
  let deadLetterTopic = appCtx.config.kafkaDeadLetterTopic
  currentTime <- getCurrentTime
  forM_ messages \(origTopicOrAckId, msgData) -> do
    let deadLetterAttrs =
          attributes
            <> HM.fromList
              [ ("original-topic", origTopicOrAckId) -- For Kafka, this stores the original topic name
              , ("original-ack-id", origTopicOrAckId) -- For PubSub, this stores the ack ID
              , ("error-reason", errorReason)
              , ("failed-at", toText $ show currentTime)
              ]
    _ <- publishJSONToKafka appCtx deadLetterTopic (BC.unpack msgData) deadLetterAttrs
    pass


-- | Check if exception is unrecoverable and should be discarded
isUnrecoverableError :: SomeException -> Bool
isUnrecoverableError e =
  let errorMsg = show e
   in any
        (`T.isInfixOf` toText errorMsg)
        [ "Unknown wire type"
        , "project API Key and project ID not available"
        , "Unexpected end of input"
        , "Invalid UTF-8 stream"
        ]
