module Pkg.Queue (pubsubService, kafkaService) where

import Control.Lens ((^?), _Just)
import Control.Lens qualified as L
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy.Base64 qualified as LB64
import Data.Generics.Product (field)
import Data.HashMap.Strict qualified as HM
import Data.Text.Lazy.Encoding qualified as LT
import Effectful
import Gogol qualified as Google
import Gogol.Auth.ApplicationDefault qualified as Google
import Gogol.Data.Base64 (_Base64)
import Gogol.PubSub qualified as PubSub
import Kafka.Consumer qualified as K
import Log qualified
import Relude
import System.Config
import System.Types (ATBackgroundCtx, runBackground)
import UnliftIO (throwIO)
import UnliftIO.Exception (tryAny)


-- pubsubService connects to the pubsub service and listens for  messages,
-- then it calls the processMessage function to process the messages, and
-- acknoleges the list message in one request.
pubsubService :: Log.Logger -> AuthContext -> [Text] -> ([(Text, ByteString)] -> HM.HashMap Text Text -> ATBackgroundCtx [Text]) -> IO ()
pubsubService appLogger appCtx topics fn = do
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
      result <- tryAny do
        let subscription = "projects/past-3/subscriptions/" <> topic <> "-sub"
        pullResp <- Google.send env $ PubSub.newPubSubProjectsSubscriptionsPull pullReq subscription
        let messages = fromMaybe [] (pullResp L.^. field @"receivedMessages")
        let msgsB64 =
              messages & map \msg -> do
                ackId <- msg.ackId
                b64Msg <- msg ^? field @"message" . _Just . field @"data'" . _Just . _Base64
                Just (ackId, b64Msg)
        let firstAttrs = messages ^? L.folded . field @"message" . _Just . field @"attributes" . _Just . field @"additional"
        msgIds <- liftIO $ runBackground appLogger appCtx $ fn (catMaybes msgsB64) (maybeToMonoid firstAttrs)
        let acknowlegReq = PubSub.newAcknowledgeRequest & field @"ackIds" L..~ Just msgIds
        unless (null msgIds) $ void $ PubSub.newPubSubProjectsSubscriptionsAcknowledge acknowlegReq subscription & Google.send env
      case result of
        Left e -> do
          liftIO $ Log.runLogT "apitoolkit" appLogger Log.LogAttention $ Log.logAttention "Run Pubsub exception" (show e)
          pass
        Right _ -> pass
  where
    -- pubSubScope :: Proxy PubSub.Pubsub'FullControl
    pubSubScope :: Proxy '["https://www.googleapis.com/auth/pubsub"]
    pubSubScope = Proxy


kafkaService :: Log.Logger -> AuthContext -> ([(Text, ByteString)] -> HM.HashMap Text Text -> ATBackgroundCtx [Text]) -> IO ()
kafkaService appLogger appCtx fn = do
  let consumerSub = K.topics (map K.TopicName appCtx.config.kafkaTopics) <> K.offsetReset K.Earliest
  consumer <- either throwIO pure =<< K.newConsumer (consumerProps appCtx.config) consumerSub
  forever do
    pollResult@(leftRecords, rightRecords) <- partitionEithers <$> K.pollMessageBatch consumer (K.Timeout 10) (K.BatchSize appCtx.config.messagesPerPubsubPullBatch) -- timeout in milliseconds
    case rightRecords of
      [] -> pass
      (rec : _) -> do
        let topic = rec.crTopic.unTopicName
            attributes = HM.insert "ce-type" (topicToCeType topic) $ consumerRecordHeadersToHashMap rec
        msgIds <- runBackground appLogger appCtx $ fn (consumerRecordToTuple <$> rightRecords) attributes
        (maybe pass throwIO) =<< K.commitAllOffsets (K.OffsetCommitAsync) consumer
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

    consumerProps :: EnvConfig -> K.ConsumerProperties
    consumerProps cfg =
      K.brokersList (map K.BrokerAddress cfg.kafkaBrokers)
        <> K.groupId (K.ConsumerGroupId cfg.kafkaGroupId)
        <> K.extraProp "security.protocol" "sasl_plaintext"
        <> K.extraProp "sasl.mechanism" "SCRAM-SHA-256"
        <> K.extraProp "sasl.username" cfg.kafkaUsername
        <> K.extraProp "sasl.password" cfg.kafkaPassword
        <> K.logLevel K.KafkaLogInfo

    -- \| Maps Kafka topic names to their corresponding CloudEvent types
    topicToCeType :: Text -> Text
    topicToCeType topic = case topic of
      "otlp_traces" -> "org.opentelemetry.otlp.traces.v1"
      "otlp_spans" -> "org.opentelemetry.otlp.traces.v1"
      "otlp_logs" -> "org.opentelemetry.otlp.logs.v1"
      "otlp_metrics" -> "org.opentelemetry.otlp.metrics.v1"
      _ -> ""
