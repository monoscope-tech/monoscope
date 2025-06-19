{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pkg.Mail (sendSlackMessage, sendPostmarkEmail, sendDiscordNotif, sendSlackAlert, NotificationAlerts (..), sendDiscordAlert) where

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
import Models.Apis.Slack (DiscordData (..), SlackData (..), getDiscordData, getDiscordDataByProjectId, getProjectSlackData)
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (urlEncode)
import Network.Wreq (defaults, header, postWith)
import Relude hiding (ask)
import System.Config (AuthContext (env))
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
  = EndpointAlert {project :: Text, endpoints :: V.Vector Text, endpointHash :: Text}
  | RuntimeErrorAlert
      { errorType :: Text
      , message :: Text
      , project :: Text
      , hash :: Text
      , endpoint :: Maybe Text
      , firstSeen :: Text
      , technology :: Text
      }
  | ShapeAlert


sendDiscordAlert :: NotificationAlerts -> Projects.ProjectId -> ATBackgroundCtx ()
sendDiscordAlert alert pid = do
  appCtx <- ask @Config.AuthContext
  discordDataM <- getDiscordDataByProjectId pid
  whenJust discordDataM \discordData -> do
    let send = sendAlert discordData.notifsChannelId
    let projectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
    case alert of
      RuntimeErrorAlert{..} -> send $ discordErrorAlert errorType message project hash endpoint firstSeen projectUrl
      EndpointAlert{..} -> send $ discordNewEndpointAlert project endpoints endpointHash projectUrl
      ShapeAlert -> pass
  where
    sendAlert :: Maybe Text -> Value -> ATBackgroundCtx ()
    sendAlert channelId content = do
      appCtx <- ask @Config.AuthContext
      let envCfg = appCtx.env
      let url = T.unpack $ "https://discord.com/api/v10/channels/" <> fromMaybe "" channelId <> "/messages"
      let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [TE.encodeUtf8 $ "Bot " <> envCfg.discordBotToken]
      _ <- Wreq.postWith opts url content
      pass


sendSlackAlert :: NotificationAlerts -> Projects.ProjectId -> ATBackgroundCtx ()
sendSlackAlert alert pid = do
  appCtx <- ask @Config.AuthContext
  slackDataM <- getProjectSlackData pid
  whenJust slackDataM \slackData -> do
    let projectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
    case alert of
      RuntimeErrorAlert{..} -> sendAlert $ slackErrorAlert errorType message project hash endpoint technology firstSeen slackData.channelId projectUrl
      EndpointAlert{..} -> sendAlert $ slackNewEndpointsAlert project endpoints slackData.channelId endpointHash projectUrl
      ShapeAlert -> pass
  where
    sendAlert :: Value -> ATBackgroundCtx ()
    sendAlert content = do
      appCtx <- ask @Config.AuthContext
      let envCfg = appCtx.env
      let url = "https://slack.com/api/chat.postMessage"
      let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [(TE.encodeUtf8 $ "Bearer " <> envCfg.slackBotToken)]
      _ <- Wreq.postWith opts (T.unpack url) content
      pass


slackErrorAlert :: Text -> Text -> Text -> Text -> Maybe Text -> Text -> Text -> Text -> Text -> Value
slackErrorAlert errType message project hash endpoint technology firstSeen channelId projectUrl =
  object
    [ "blocks"
        .= Array
          ( V.fromList
              [ object ["type" .= "section", "text" .= object ["type" .= "mrkdwn", "text" .= title]]
              , object ["type" .= "section", "text" .= object ["type" .= "mrkdwn", "text" .= ("```" <> message <> "\n```")]]
              , object
                  [ "type" .= "context"
                  , "elements" .= Array (V.fromList $ [object ["type" .= "mrkdwn", "text" .= ("*Stack:* `" <> technology <> "`")]] ++ maybe [] (\e -> [object ["type" .= "mrkdwn", "text" .= ("*Endpoint:* " <> enp)]]) endpoint)
                  ]
              , object
                  [ "type" .= "context"
                  , "elements"
                      .= Array (V.fromList [object ["type" .= "mrkdwn", "text" .= ("*Project:* " <> project)], object ["type" .= "mrkdwn", "text" .= ("*First seen:* " <> firstSeen)]])
                  ]
              , object ["type" .= "divider"]
              , object
                  [ "type" .= "actions"
                  , "elements" .= Array (V.fromList [object ["type" .= "button", "text" .= object ["type" .= "plain_text", "text" .= "🔍 Investigate", "emoji" .= True], "url" .= targetUrl, "style" .= "primary"]])
                  ]
              ]
          )
    , "channel" .= channelId
    ]
  where
    targetUrl = projectUrl <> "/anomalies/by_hash/" <> hash
    title = "<" <> targetUrl <> "|:red_circle: " <> errType <> ">"
    enp = maybe "" (\x -> "`" <> x <> "`") endpoint


slackNewEndpointsAlert :: Text -> V.Vector Text -> Text -> Text -> Text -> Value
slackNewEndpointsAlert projectName endpoints channelId hash projectUrl =
  object
    [ "blocks"
        .= Array
          ( V.fromList
              [ object ["type" .= "section", "text" .= object ["type" .= "mrkdwn", "text" .= ("<" <> targetUrl <> "|:large_blue_circle: New Endpoint(s) Detected>")]]
              , object ["type" .= "section", "text" .= object ["type" .= "mrkdwn", "text" .= ("We've detected *" <> T.pack (show $ length endpoints) <> "* new endpoint(s) in *" <> projectName <> "*.")]]
              , object ["type" .= "divider"]
              , object ["type" .= "section", "text" .= object ["type" .= "mrkdwn", "text" .= ("*Endpoints:*\n\n" <> enps)]]
              , object ["type" .= "context", "elements" .= Array (V.fromList [object ["type" .= "mrkdwn", "text" .= ("*Project:* " <> projectName)]])]
              , object
                  [ "type" .= "actions"
                  , "elements"
                      .= Array (V.fromList [object ["type" .= "button", "text" .= object ["type" .= "plain_text", "text" .= "View in Explorer", "emoji" .= False], "style" .= "primary", "url" .= explorerUrl]])
                  ]
              ]
          )
    , "channel" .= channelId
    ]
  where
    targetUrl = projectUrl <> "/anomalies/by_hash/" <> hash
    enp = (\x -> "\"" <> x <> "\"") <$> (\x -> T.dropWhile (\xx -> xx /= ' ') x) <$> V.toList endpoints
    query = urlEncode True $ TE.encodeUtf8 $ "attributes.http.route in (" <> T.intercalate "," enp <> ")"
    explorerUrl = projectUrl <> "/log_explorer?query=" <> TE.decodeUtf8 query
    enps = T.intercalate "\n\n" $ (\x -> "`" <> x <> "`") <$> V.toList endpoints


discordErrorAlert :: Text -> Text -> Text -> Text -> Maybe Text -> Text -> Text -> Value
discordErrorAlert errType message project hash endpoint firstSeen projectUrl =
  [aesonQQ|{
"embeds": [
 {
      "title": #{errType},
      "description": #{msg},
      "color": 16711680,
      "fields": [
        {"name": "Endpoint", "value": #{enp},"inline": true},
        {"name": "Project","value": #{project}, "inline": true},
        {"name": "First Seen", "value": #{firstSeen},"inline": true}
      ],
      "url": #{url}
    }],
  "content": "**🔴 New Runtime Error**"
}|]
  where
    url = projectUrl <> "/anomalies/by_hash/" <> hash
    msg = "```" <> message <> "```"
    enp = maybe "" (\x -> "`" <> x <> "`") endpoint


discordNewEndpointAlert :: Text -> V.Vector Text -> Text -> Text -> Value
discordNewEndpointAlert projectName endpoints hash projectUrl =
  [aesonQQ|
  {
  "embeds": [
    {
      "type": "rich",
      "description": #{description},
      "color": 263167,
      "fields": [
        {"name": "Endpoints","value": #{enps},"inline": false},
        {"name": "···","value": #{explorerLink},"inline": false}
      ],
      "footer": { "text": "{···}"},
      "url": #{url}
    }
  ],
  "content" : "🔵 New Endpoint(s) Detected"
}
  |]
  where
    endpointsCount = length endpoints
    description = "We've detected **" <> show endpointsCount <> " new endpoints** in the **" <> projectName <> "** project."
    url = projectUrl <> "/anomalies/by_hash/" <> hash
    enp = (\x -> "\"" <> x <> "\"") <$> (\x -> T.dropWhile (\xx -> xx /= ' ') x) <$> V.toList endpoints
    query = urlEncode True $ TE.encodeUtf8 $ "attributes.http.route in (" <> T.intercalate "," enp <> ")"
    explorerUrl = projectUrl <> "/log_explorer?query=" <> TE.decodeUtf8 query
    explorerLink = "[View in Explorer](" <> explorerUrl <> ")"
    enps = T.intercalate "\n\n" $ (\x -> "`" <> x <> "`") <$> V.toList endpoints
