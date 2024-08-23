{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pkg.Mail (sendSlackMessage, sendPostmarkEmail, sendDiscordNotif) where

import Control.Lens ((.~))
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Pool ()
import Effectful (
  Eff,
  IOE,
  type (:>),
 )
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (ask)
import Log qualified
import Models.Apis.Slack (SlackData (..), getProjectSlackData)
import Models.Projects.Projects qualified as Projects
import Network.Wreq (defaults, header, postWith)
import Relude hiding (ask)
import System.Config qualified as Config
import System.Types (ATBackgroundCtx)


sendPostmarkEmail :: Text -> Text -> AE.Value -> ATBackgroundCtx ()
sendPostmarkEmail reciever template templateVars = do
  appCtx <- ask @Config.AuthContext
  let url = "https://api.postmarkapp.com/email/withTemplate"
  let apiKey = encodeUtf8 appCtx.config.postmarkToken
  let payload =
        [aesonQQ|
        {
    "From": "hello@apitoolkit.io",
    "To": #{reciever},
    "TemplateAlias": #{template},
    "TemplateModel": #{templateVars}
        }
    |]
  let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Accept" .~ ["application/json"] & header "X-Postmark-Server-Token" .~ [apiKey]
  response <- liftIO $ postWith opts url payload
  pass


sendSlackMessage :: (DB :> es, Log :> es, IOE :> es) => Projects.ProjectId -> Text -> Eff es ()
sendSlackMessage pid message = do
  slackData <- getProjectSlackData pid
  case slackData of
    Just s -> liftIO $ slackPostWebhook s.webhookUrl message
    Nothing -> Log.logAttention "sendSlackMessage is not configured. But was called" (pid, message)


sendDiscordNotif :: IOE :> es => Text -> Text -> Eff es ()
sendDiscordNotif webhookUrl message = do
  let msg = object ["content" .= message]
  let opts = defaults & header "Content-Type" .~ ["application/json"]
  response <- liftIO $ postWith opts (toString webhookUrl) msg
  pass


slackPostWebhook :: Text -> Text -> IO ()
slackPostWebhook webhookUrl message = do
  let opts = defaults & header "Content-Type" .~ ["application/json"]
  let payload =
        [aesonQQ| {
                "text": #{message},
                "type":"mrkdwn"
              }
            |]
  response <- liftIO $ postWith opts (toString webhookUrl) payload
  pass
