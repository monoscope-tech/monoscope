{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pkg.Mail (sendSlackMessage, sendPostmarkEmail, sendWhatsAppAlert, sendDiscordNotif, sendSlackAlert, NotificationAlerts (..), sendDiscordAlert) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KEM
import Data.Aeson.QQ (aesonQQ)
import Data.Effectful.Notify qualified as Notify
import Data.Pool ()
import Data.Text qualified as T
import Data.Time
import Data.Vector qualified as V
import Effectful (
  Eff,
  type (:>),
 )
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (Reader, ask)
import Log qualified
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Slack (DiscordData (..), SlackData (..), getDiscordDataByProjectId, getProjectSlackData)
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (urlEncode)
import Relude hiding (Reader, ask)
import System.Config (AuthContext (env))
import System.Config qualified as Config


sendPostmarkEmail :: Notify.Notify :> es => Text -> Maybe (Text, AE.Value) -> Maybe (Text, Text) -> Eff es ()
sendPostmarkEmail receiver tmpOptionsM subMsg =
  Notify.sendNotification $ Notify.emailNotification receiver tmpOptionsM subMsg


sendSlackMessage :: (DB :> es, Log :> es, Notify.Notify :> es) => Projects.ProjectId -> Text -> Eff es ()
sendSlackMessage pid message = do
  slackData <- getProjectSlackData pid
  case slackData of
    Just s -> do
      let payload = [aesonQQ| {"text": #{message}, "type":"mrkdwn"} |]
      Notify.sendNotification $ Notify.slackNotification s.webhookUrl payload
    Nothing -> Log.logAttention "sendSlackMessage is not configured. But was called" (pid, message)


sendDiscordNotif :: Notify.Notify :> es => Text -> Text -> Eff es ()
sendDiscordNotif message channelId = do
  let msg = AE.object ["content" AE..= message]
  Notify.sendNotification $ Notify.discordNotification channelId msg


data NotificationAlerts
  = EndpointAlert {project :: Text, endpoints :: V.Vector Text, endpointHash :: Text}
  | RuntimeErrorAlert RequestDumps.ATError
  | ShapeAlert
  | ReportAlert
      { reportType :: Text
      , startTime :: Text
      , endTime :: Text
      , totalErrors :: Int
      , totalEvents :: Int
      , breakDown :: V.Vector (Text, Int, Int)
      , reportUrl :: Text
      , allChartUrl :: Text
      , errorChartUrl :: Text
      }


sendDiscordAlert :: (DB :> es, Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> Eff es ()
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
      ReportAlert{..} -> send $ discordReportAlert reportType startTime endTime totalErrors totalEvents breakDown pTitle reportUrl allChartUrl errorChartUrl
  where
    sendAlert :: Notify.Notify :> es => Maybe Text -> AE.Value -> Eff es ()
    sendAlert channelId content =
      whenJust channelId $ \cid ->
        Notify.sendNotification $ Notify.discordNotification cid content


sendSlackAlert :: (DB :> es, Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> Eff es ()
sendSlackAlert alert pid pTitle = do
  appCtx <- ask @Config.AuthContext
  slackDataM <- getProjectSlackData pid
  whenJust slackDataM \slackData -> do
    let projectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
    case alert of
      RuntimeErrorAlert a -> sendAlert slackData.webhookUrl $ slackErrorAlert a pTitle slackData.channelId projectUrl
      EndpointAlert{..} -> sendAlert slackData.webhookUrl $ slackNewEndpointsAlert project endpoints slackData.channelId endpointHash projectUrl
      ShapeAlert -> pass
      ReportAlert{..} -> sendAlert slackData.webhookUrl $ slackReportAlert reportType startTime endTime totalErrors totalEvents breakDown pTitle slackData.channelId reportUrl allChartUrl errorChartUrl
  where
    sendAlert :: Notify.Notify :> es => Text -> AE.Value -> Eff es ()
    sendAlert webhookUrl content =
      Notify.sendNotification $ Notify.slackNotification webhookUrl content


sendWhatsAppAlert :: (Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> V.Vector Text -> Eff es ()
sendWhatsAppAlert alert pid pTitle tos = do
  appCtx <- ask @Config.AuthContext
  case alert of
    RuntimeErrorAlert err -> do
      let template = appCtx.config.whatsappErrorTemplate
          url = pid.toText <> "/anomalies/by_hash/" <> fromMaybe "" err.hash
          contentVars = AE.object ["1" AE..= ("*" <> pTitle <> "*"), "2" AE..= ("*" <> err.errorType <> "*"), "3" AE..= ("`" <> err.message <> "`"), "4" AE..= url]
      sendAlert template contentVars
      pass
    EndpointAlert{..} -> do
      let template = appCtx.config.whatsappEndpointTemplate
          url = pid.toText <> "/anomalies/by_hash/" <> endpointHash
          contentVars = AE.object ["1" AE..= ("*" <> pTitle <> "*"), "2" AE..= T.intercalate "." ((\x-> "`" <> x <> "`") <$> V.toList endpoints), "3" AE..= url]
      sendAlert template contentVars
      pass
    ReportAlert{..} -> do
      let template = appCtx.config.whatsappAllReportTemplate
          templateErr = appCtx.config.whatsappErrorReportTemplate
          urlVar = fromMaybe "" $ viaNonEmpty last $ T.splitOn "/p/" reportUrl
          cUrl = fromMaybe "" $ viaNonEmpty last $ T.splitOn "?" allChartUrl
          eUrl = fromMaybe "" $ viaNonEmpty last $ T.splitOn "?" errorChartUrl
          contentVars =
            KEM.fromList
              [ "1" AE..= reportType
              , "2" AE..= ("*" <> pTitle <> "*")
              , "4" AE..= ("`" <> T.take 10 startTime <> "`")
              , "5" AE..= ("`" <> T.take 10 endTime <> "`")
              , "7" AE..= urlVar
              ]
      sendAlert template (AE.Object $ contentVars <> KEM.fromList ["3" AE..= ("*" <> show totalEvents <> "*"), "6" AE..= cUrl])
      sendAlert templateErr (AE.Object $ contentVars <> KEM.fromList ["3" AE..= ("*" <> show totalErrors <> "*"), "6" AE..= eUrl])
      pass
    ShapeAlert -> pass
  where
    sendAlert :: Notify.Notify :> es => Text -> AE.Value -> Eff es ()
    sendAlert template vars =
      forM_ tos $ \to ->
        Notify.sendNotification $ Notify.whatsappNotification template to vars


slackReportAlert :: Text -> Text -> Text -> Int -> Int -> V.Vector (Text, Int, Int) -> Text -> Text -> Text -> Text -> Text -> AE.Value
slackReportAlert reportType startTime endTime totalErrors totalEvents breakDown project channelId url allUrl errUrl =
  AE.object
    [ "blocks"
        AE..= AE.Array
          ( V.fromList
              [ AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("<" <> url <> "|" <> reportType <> " Report for " <> project <> ">")]]
              , AE.object
                  [ "type" AE..= "context"
                  , "elements" AE..= AE.Array (V.fromList $ AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*From:* " <> startTime)] : [AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*To:* " <> endTime)]])
                  ]
              , AE.object
                  [ "type" AE..= "image"
                  , "image_url" AE..= allUrl
                  , "alt_text" AE..= "Graph"
                  , "title"
                      AE..= AE.object
                        [ "type" AE..= "plain_text"
                        , "text" AE..= ("Total Events: " <> toText (show totalEvents))
                        ]
                  ]
              , AE.object
                  [ "type" AE..= "image"
                  , "image_url" AE..= errUrl
                  , "alt_text" AE..= "Graph"
                  , "title"
                      AE..= AE.object
                        [ "type" AE..= "plain_text"
                        , "text" AE..= ("Total Errors: " <> toText (show totalErrors))
                        ]
                  ]
              , AE.object ["type" AE..= "divider"]
              , AE.object ["type" AE..= "context", "elements" AE..= AE.Array sumr]
              ]
          )
    , "channel" AE..= channelId
    ]
  where
    sumr = V.take 10 $ V.map (\(name, errCount, evCount) -> AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*" <> name <> ":* Errors-" <> toText (show errCount) <> ", Total-" <> toText (show evCount))]) breakDown


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


discordReportAlert :: Text -> Text -> Text -> Int -> Int -> V.Vector (Text, Int, Int) -> Text -> Text -> Text -> Text -> AE.Value
discordReportAlert reportType startTime endTime totalErrors totalEvents breakDown project url allUrl errUrl =
  AE.object
    [ "flags" AE..= 32768
    , "components"
        AE..= AE.Array
          ( V.fromList
              [ AE.object ["type" AE..= 10, "content" AE..= ("## 📊 " <> (if reportType == "weekly" then "Weekly" else "Daily") <> " Report for " <> project)]
              , AE.object ["type" AE..= 10, "content" AE..= ("**From:** " <> T.take 10 startTime <> "  **To:** " <> T.take 10 endTime)]
              , AE.object ["type" AE..= 10, "content" AE..= ("Total Events: **" <> show totalEvents <> "**" <> (T.replicate 28 "  ") <> " Total Errors: **" <> show totalErrors <> "**")]
              , AE.object
                  [ "type" AE..= 12
                  , "items"
                      AE..= AE.Array
                        ( V.fromList
                            [ AE.object ["media" AE..= AE.object ["url" AE..= allUrl, "description" AE..= "Total events"]]
                            , AE.object ["media" AE..= AE.object ["url" AE..= errUrl, "description" AE..= "Total errors"]]
                            ]
                        )
                  ]
              , AE.object ["type" AE..= 10, "content" AE..= servicesStat]
              , AE.object
                  [ "type" AE..= 1
                  , "components" AE..= AE.Array (V.fromList [AE.object ["type" AE..= 2, "label" AE..= "Open report", "url" AE..= url, "style" AE..= 5]])
                  ]
              ]
          )
    ]
  where
    servicesStat =
      T.intercalate "\n" $ V.toList $ V.take 10 $ V.map (\(name, errCount, evCount) -> "* **" <> name <> "**: Total errors-" <> show errCount <> ", Total events-" <> show evCount) breakDown


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
