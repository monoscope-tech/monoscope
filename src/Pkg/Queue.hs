module Pkg.Queue (pubsubService) where

import BackgroundJobs qualified
import Colourista.IO (blueMessage)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Exception.Safe qualified as Safe
import Control.Lens ((^?), _Just)
import Control.Lens qualified as L
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Base64 qualified as LB64
import Data.Generics.Product (field)
import Data.HashMap.Strict qualified as HashMap
import Data.Pool as Pool (destroyAllResources)
import Data.Text.Lazy.Encoding qualified as LT
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Fail (runFailIO)
import Effectful.Time (runTime)
import Gogol qualified as Google
import Gogol.Auth.ApplicationDefault qualified as Google
import Gogol.Data.Base64 (_Base64)
import Gogol.PubSub qualified as PubSub
import Log qualified
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setOnException,
  setPort,
 )
import Network.Wai.Log qualified as WaiLog
import Network.Wai.Middleware.Heartbeat (heartbeatMiddleware)
import Opentelemetry.OtlpServer qualified as OtlpServer
import ProcessMessage (processMessages)
import Relude
import Servant qualified
import Servant.Server.Generic (genericServeTWithContext)
import System.Config
import System.Logging qualified as Logging
import System.Types (ATBackgroundCtx, effToServantHandler, runBackground)
import UnliftIO.Exception (tryAny)
import Web.Routes qualified as Routes


-- pubsubService connects to the pubsub service and listens for  messages,
-- then it calls the processMessage function to process the messages, and
-- acknoleges the list message in one request.
pubsubService :: Log.Logger -> AuthContext -> [Text] -> ([(Text, ByteString)] -> HashMap Text Text -> ATBackgroundCtx [Text]) -> IO ()
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

        msgIds <- liftIO $ runBackground appLogger appCtx $ fn (catMaybes msgsB64) (fromMaybe mempty firstAttrs)
        let acknowlegReq = PubSub.newAcknowledgeRequest & field @"ackIds" L..~ Just msgIds
        unless (null msgIds) $ void $ PubSub.newPubSubProjectsSubscriptionsAcknowledge acknowlegReq subscription & Google.send env
      case result of
        Left (e) -> do
          liftIO $ Log.runLogT "apitoolkit" appLogger Log.LogAttention $ Log.logAttention "Run Pubsub exception" (show e)
          pass
        Right _ -> pass


-- pubSubScope :: Proxy PubSub.Pubsub'FullControl
pubSubScope :: Proxy '["https://www.googleapis.com/auth/pubsub"]
pubSubScope = Proxy
