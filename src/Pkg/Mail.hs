{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pkg.Mail (sendSlackMessage, sendPostmarkEmail, sendDiscordNotif) where

import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Pool ()
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
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


sendPostmarkEmail :: Text -> Maybe (Text, AE.Value) -> Maybe (Text, Text) -> ATBackgroundCtx ()
sendPostmarkEmail reciever tmpOptionsM subMsg = do
  appCtx <- ask @Config.AuthContext
  let url = if isJust tmpOptionsM then "https://api.postmarkapp.com/email/withTemplate" else "https://api.postmarkapp.com/email"
  let apiKey = encodeUtf8 appCtx.config.postmarkToken
  let (subject, message) = fromMaybe ("", "") subMsg
  let payload = case tmpOptionsM of
        Just (template, templateVars) ->
          [aesonQQ|
            {
        "From": "hello@apitoolkit.io",
        "To": #{reciever},
        "TemplateAlias": #{template},
        "TemplateModel": #{templateVars}
            }
        |]
        _ ->
          [aesonQQ|
            {
        "From": "hello@apitoolkit.io",
        "Subject": #{subject},
        "To": #{reciever},
        "TextBody": #{message},
        "HtmlBody": "<p>#{message}</p>",
        "MessageStream": "outbound"
            }
        |]
  let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Accept" .~ ["application/json"] & header "X-Postmark-Server-Token" .~ [apiKey]
  response <- liftIO $ postWith opts url payload
  pass


sendSlackMessage :: (DB :> es, IOE :> es, Log :> es) => Projects.ProjectId -> Text -> Eff es ()
sendSlackMessage pid message = do
  slackData <- getProjectSlackData pid
  case slackData of
    Just s -> liftIO $ slackPostWebhook s.webhookUrl message
    Nothing -> Log.logAttention "sendSlackMessage is not configured. But was called" (pid, message)


sendDiscordNotif :: Text -> Text -> ATBackgroundCtx ()
sendDiscordNotif message channelId = do
  appCtx <- ask @Config.AuthContext
  let msg = AE.object ["content" AE..= message]
      url = T.unpack $ "https://discord.com/api/v10/channels/" <> channelId <> "/messages"
      opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [TE.encodeUtf8 $ "Bot " <> appCtx.config.discordBotToken]
  response <- liftIO $ postWith opts url msg
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
