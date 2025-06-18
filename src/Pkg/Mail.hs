{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pkg.Mail (sendSlackMessage, sendPostmarkEmail, sendDiscordNotif, NotificationAlerts (..)) where

import Control.Lens ((.~))
import Data.Aeson
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Effectful.Wreq (HTTP)
import Data.Effectful.Wreq qualified as Wreq
import Data.Pool ()
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Effectful (
  Eff,
  IOE,
  type (:>),
 )
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (Reader, ask)
import Log qualified
import Models.Apis.Slack (SlackData (..), getProjectSlackData)
import Models.Projects.Projects qualified as Projects
import Network.Wreq (defaults, header, postWith)
import Relude hiding (ask)
import System.Config (AuthContext)
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
      url = toString $ "https://discord.com/api/v10/channels/" <> channelId <> "/messages"
      opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bot " <> appCtx.config.discordBotToken]
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


data NotificationAlerts
  = EndpointAlert
  | RuntimeErrorAlert
      { errorType :: Text
      , message :: Text
      , project :: Text
      , hash :: Text
      , endpoint :: Maybe Text
      , firstSeen :: Text
      , service :: Text
      }
  | ShapeAlert


sendSlackAlert :: (DB :> es, Effectful.Reader.Static.Reader AuthContext :> es, HTTP :> es) => NotificationAlerts -> Projects.ProjectId -> Eff es ()
sendSlackAlert alert pid = do
  appCtx <- ask @Config.AuthContext
  slackDataM <- getProjectSlackData pid
  whenJust slackDataM \slackData -> do
    let send = sendAlert appCtx.env.slackClientId slackData.channelId
    case alert of
      RuntimeErrorAlert{..} -> do
        let content = slackErrorAlert errorType message project hash endpoint service firstSeen
        _ <- send content
        pass
      EndpointAlert -> pass
      ShapeAlert -> pass
  where
    sendAlert :: HTTP :> es => Text -> Text -> Value -> Eff es ()
    sendAlert channelId token content = do
      let url = "api.slack.com/user" <> channelId
      let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [(TE.encodeUtf8 $ "Bearer " <> token)]
      _ <- Wreq.postWith opts (T.unpack url) content
      pass


slackErrorAlert :: Text -> Text -> Text -> Text -> Maybe Text -> Text -> Text -> Value
slackErrorAlert errType message project hash endpoint service fristSeen =
  object
    [ "blocks"
        .= Array
          ( V.fromList
              [ object ["type" .= "header", "text" .= object ["type" .= "plain_text", "text" .= ("🔴 " <> errType), "emoji" .= True]]
              , object ["type" .= "section", "text" .= object ["type" .= "mrkdwn", "text" .= ("```" <> message <> "\n```")]]
              , object
                  [ "type" .= "context"
                  , "elements" .= Array (V.fromList $ [object ["type" .= "mrkdwn", "text" .= ("*Source:* `" <> service <> "`")]] ++ maybe [] (\e -> [object ["type" .= "mrkdwn", "text" .= "*Endpoint:* `GET /user/hell world`"]]) endpoint)
                  ]
              , object
                  [ "type" .= "context"
                  , "elements"
                      .= Array (V.fromList [object ["type" .= "mrkdwn", "text" .= ("*Project:*" <> project)], object ["type" .= "mrkdwn", "text" .= "*First seen:* <!date^1718654100^{date_short_pretty} at {time}|2025-06-17 20:15C>"]])
                  ]
              , object ["type" .= "divider"]
              , object
                  [ "type" .= "actions"
                  , "elements" .= Array (V.fromList [object ["type" .= "button", "text" .= object ["type" .= "plain_text", "text" .= "🔍 Investigate", "emoji" .= Bool True], "url" .= "https://errors.example.com/explore?hash=a8f3c9d2e1b4", "style" .= "primary"]])
                  ]
              ]
          )
    ]
