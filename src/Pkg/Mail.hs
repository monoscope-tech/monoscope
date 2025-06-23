{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pkg.Mail (sendSlackMessage, sendPostmarkEmail, sendDiscordNotif, sendSlackAlert, NotificationAlerts (..), sendDiscordAlert) where

import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Effectful.Wreq qualified as Wreq
import Data.Pool ()
import Data.Text qualified as T
import Data.Time
import Data.Vector qualified as V
import Effectful (
  Eff,
  IOE,
  type (:>),
 )
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (ask)
import Log qualified
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Slack (DiscordData (..), SlackData (..), getDiscordDataByProjectId, getProjectSlackData)
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
  | RuntimeErrorAlert RequestDumps.ATError
  | ShapeAlert


sendDiscordAlert :: NotificationAlerts -> Projects.ProjectId -> Text -> ATBackgroundCtx ()
sendDiscordAlert alert pid pTitle = do
  appCtx <- ask @Config.AuthContext
  discordDataM <- getDiscordDataByProjectId pid
  whenJust discordDataM \discordData -> do
    let send = sendAlert discordData.notifsChannelId
    let projectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
    case alert of
      RuntimeErrorAlert a -> send $ discordErrorAlert a pTitle projectUrl
      EndpointAlert{..} -> send $ discordNewEndpointAlert project endpoints endpointHash projectUrl
      ShapeAlert -> pass
  where
    sendAlert :: Maybe Text -> AE.Value -> ATBackgroundCtx ()
    sendAlert channelId content = do
      appCtx <- ask @Config.AuthContext
      let envCfg = appCtx.env
      let url = toString $ "https://discord.com/api/v10/channels/" <> fromMaybe "" channelId <> "/messages"
      let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bot " <> envCfg.discordBotToken]
      _ <- Wreq.postWith opts url content
      pass


sendSlackAlert :: NotificationAlerts -> Projects.ProjectId -> Text -> ATBackgroundCtx ()
sendSlackAlert alert pid pTitle = do
  appCtx <- ask @Config.AuthContext
  slackDataM <- getProjectSlackData pid
  whenJust slackDataM \slackData -> do
    let projectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
    case alert of
      RuntimeErrorAlert a -> sendAlert $ slackErrorAlert a pTitle slackData.channelId projectUrl
      EndpointAlert{..} -> sendAlert $ slackNewEndpointsAlert project endpoints slackData.channelId endpointHash projectUrl
      ShapeAlert -> pass
  where
    sendAlert :: AE.Value -> ATBackgroundCtx ()
    sendAlert content = do
      appCtx <- ask @Config.AuthContext
      let envCfg = appCtx.env
      let url = "https://slack.com/api/chat.postMessage"
      let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bearer " <> envCfg.slackBotToken]
      _ <- Wreq.postWith opts (toString url) content
      pass


slackErrorAlert :: RequestDumps.ATError -> Text -> Text -> Text -> AE.Value
slackErrorAlert err project channelId projectUrl =
  AE.object
    [ "blocks"
        AE..= AE.Array
          ( V.fromList
              [ AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= title]]
              , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("```" <> err.message <> "\n```")]]
              , AE.object
                  [ "type" AE..= "context"
                  , "elements" AE..= AE.Array (V.fromList $ AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Stack:* `" <> fromMaybe "" err.stack <> "`")] : [AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Endpoint:* " <> enp)]])
                  ]
              , AE.object
                  [ "type" AE..= "context"
                  , "elements"
                      AE..= AE.Array
                        ( V.fromList
                            [ AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Trace Id:* " <> fromMaybe "" err.traceId)]
                            , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Span Id:* " <> fromMaybe "" err.spanId)]
                            , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Service:* " <> fromMaybe "" err.serviceName)]
                            ]
                        )
                  ]
              , AE.object
                  [ "type" AE..= "context"
                  , "elements"
                      AE..= AE.Array (V.fromList [AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Project:* " <> project)], AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*First seen:* " <> firstSeen)]])
                  ]
              , AE.object ["type" AE..= "divider"]
              , AE.object
                  [ "type" AE..= "actions"
                  , "elements" AE..= AE.Array (V.fromList [AE.object ["type" AE..= "button", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "🔍 Investigate", "emoji" AE..= True], "url" AE..= targetUrl, "style" AE..= "primary"]])
                  ]
              ]
          )
    , "channel" AE..= channelId
    ]
  where
    targetUrl = projectUrl <> "/anomalies/by_hash/" <> fromMaybe "" err.hash
    title = "<" <> targetUrl <> "|:red_circle: " <> err.errorType <> ">"
    method = fromMaybe "" err.requestMethod
    path = fromMaybe "" err.requestPath
    enp = "`" <> method <> " " <> path <> "`"
    firstSeen = toText $ formatTime defaultTimeLocale "%b %-e, %Y, %-l:%M:%S %p" err.when


slackNewEndpointsAlert :: Text -> V.Vector Text -> Text -> Text -> Text -> AE.Value
slackNewEndpointsAlert projectName endpoints channelId hash projectUrl =
  AE.object
    [ "blocks"
        AE..= AE.Array
          ( V.fromList
              [ AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("<" <> targetUrl <> "|:large_blue_circle: New Endpoint(s) Detected>")]]
              , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("We've detected *" <> toText (show $ length endpoints) <> "* new endpoint(s) in *" <> projectName <> "*.")]]
              , AE.object ["type" AE..= "divider"]
              , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Endpoints:*\n\n" <> enps)]]
              , AE.object
                  [ "type" AE..= "actions"
                  , "elements"
                      AE..= AE.Array (V.fromList [AE.object ["type" AE..= "button", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "View in Explorer", "emoji" AE..= False], "style" AE..= "primary", "url" AE..= explorerUrl]])
                  ]
              ]
          )
    , "channel" AE..= channelId
    ]
  where
    targetUrl = projectUrl <> "/anomalies/by_hash/" <> hash
    enp = (\x -> "\"" <> x <> "\"") . T.dropWhile (/= ' ') <$> V.toList endpoints
    query = urlEncode True $ encodeUtf8 $ "attributes.http.route in (" <> T.intercalate "," enp <> ")"
    explorerUrl = projectUrl <> "/log_explorer?query=" <> decodeUtf8 query
    enps = T.intercalate "\n\n" $ (\x -> "`" <> x <> "`") <$> V.toList endpoints


discordErrorAlert :: RequestDumps.ATError -> Text -> Text -> AE.Value
discordErrorAlert err project projectUrl =
  [aesonQQ|{
"embeds": [
 {
      "title": #{errorType},
      "description": #{msg},
      "color": 16711680,
      "fields": [
        {"name": "Endpoint", "value": #{enp},"inline": true},
        {"name": "Project","value": #{project}, "inline": true},
        {"name": "First Seen", "value": #{firstSeen},"inline": true},
        {"name": "Trace Id" , "value": #{trId},"inline": true},
        {"name": "Span Id", "value": #{spanId},"inline": true},
        {"name": "Service", "value": #{serviceName},"inline": true}
      ],
      "url": #{url}
    }],
  "content": "**🔴 New Runtime Error**"
}|]
  where
    url = projectUrl <> "/anomalies/by_hash/" <> fromMaybe "" err.hash
    msg = "```" <> err.message <> "```"
    method = fromMaybe "" err.requestMethod
    path = fromMaybe "" err.requestPath
    enp = "`" <> method <> " " <> path <> "`"
    firstSeen = toText $ formatTime defaultTimeLocale "%b %-e, %Y, %-l:%M:%S %p" err.when
    errorType = err.errorType
    trId = fromMaybe "" err.traceId
    spanId = fromMaybe "" err.spanId
    serviceName = fromMaybe "" err.serviceName


discordNewEndpointAlert :: Text -> V.Vector Text -> Text -> Text -> AE.Value
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
    enp = (\x -> "\"" <> x <> "\"") . T.dropWhile (/= ' ') <$> V.toList endpoints
    query = urlEncode True $ encodeUtf8 $ "attributes.http.route in (" <> T.intercalate "," enp <> ")"
    explorerUrl = projectUrl <> "/log_explorer?query=" <> decodeUtf8 query
    explorerLink = "[View in Explorer](" <> explorerUrl <> ")"
    enps = T.intercalate "\n\n" $ (\x -> "`" <> x <> "`") <$> V.toList endpoints
