{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pkg.Mail (sendSlackMessage, sendRenderedEmail, sendWhatsAppAlert, sendSlackAlert, sendSlackAlertWith, NotificationAlerts (..), RuntimeAlertType (..), sendDiscordAlert, sendDiscordAlertWith, sendPagerdutyAlertToService, sampleAlert, sampleAlertByIssueTypeText, sampleReport, addConvertKitUser, addConvertKitUserOrganization) where

import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KEM
import Data.Aeson.QQ (aesonQQ)
import Data.Default (def)
import Data.Effectful.Notify qualified as Notify
import Data.Effectful.Wreq (HTTP, defaults, header, postWith)
import Data.Pool ()
import Data.Text qualified as T
import Data.Time
import Data.Vector qualified as V
import Effectful (
  Eff,
  type (:>),
 )
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Models.Apis.Errors qualified as Errors
import Models.Apis.Integrations (DiscordData (..), SlackData (..), getDiscordDataByProjectId, getProjectSlackData)
import Models.Apis.Issues (IssueType (..), parseIssueType)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (urlEncode)
import Relude hiding (Reader, ask)
import System.Config (AuthContext (env))
import System.Config qualified as Config
import System.Logging qualified as Log
import System.Types (DB)


sendRenderedEmail :: Notify.Notify :> es => Text -> Text -> Text -> Eff es ()
sendRenderedEmail receiver subject htmlBody =
  Notify.sendNotification $ Notify.emailNotification receiver subject htmlBody


sendSlackMessage :: (DB es, Log :> es, Notify.Notify :> es) => Projects.ProjectId -> Text -> Eff es ()
sendSlackMessage pid message = do
  slackData <- getProjectSlackData pid
  case slackData of
    Just s -> do
      let payload = [aesonQQ| {"text": #{message}, "type":"mrkdwn"} |]
      Notify.sendNotification $ Notify.slackNotification s.channelId s.botToken payload
    Nothing -> Log.logAttention "sendSlackMessage is not configured. But was called" (pid, message)


data NotificationAlerts
  = EndpointAlert {project :: Text, endpoints :: V.Vector Text, endpointHash :: Text}
  | RuntimeErrorAlert {issueId :: Text, errorData :: Errors.ATError, runtimeAlertType :: RuntimeAlertType}
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
  | MonitorsAlert
      { monitorTitle :: Text
      , monitorUrl :: Text
      }
  | LogPatternAlert
      { issueUrl :: Text
      , patternText :: Text
      , sampleMessage :: Maybe Text
      , logLevel :: Maybe Text
      , serviceName :: Maybe Text
      , sourceField :: Text
      , occurrenceCount :: Int
      }
  | LogPatternRateChangeAlert
      { issueUrl :: Text
      , patternText :: Text
      , sampleMessage :: Maybe Text
      , logLevel :: Maybe Text
      , serviceName :: Maybe Text
      , direction :: Text
      , currentRate :: Double
      , baselineMean :: Double
      , changePercent :: Double
      }


data RuntimeAlertType
  = NewRuntimeError
  | EscalatingErrors
  | RegressedErrors
  | ErrorSpike
  deriving stock (Eq, Generic, Show)


-- | Send a Discord alert, optionally threading replies under a parent message.
-- Returns the message ID if threading is enabled and the send succeeds.
sendDiscordAlert :: (DB es, Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> Maybe Text -> Eff es (Maybe Text)
sendDiscordAlert = sendDiscordAlertWith Nothing


-- | Internal: send Discord alert with optional reply-to threading
sendDiscordAlertWith :: (DB es, Notify.Notify :> es, Reader Config.AuthContext :> es) => Maybe Text -> NotificationAlerts -> Projects.ProjectId -> Text -> Maybe Text -> Eff es (Maybe Text)
sendDiscordAlertWith replyToMsgIdM alert pid pTitle channelIdM' = do
  appCtx <- ask @Config.AuthContext
  channelIdM <- maybe (getDiscordDataByProjectId pid <&> (>>= (.notifsChannelId))) (pure . Just) channelIdM'
  case channelIdM of
    Nothing -> pure Nothing
    Just cid -> do
      let projectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
          mkPayload = \case
            RuntimeErrorAlert{..} -> Just $ discordErrorAlert runtimeAlertType errorData pTitle projectUrl
            EndpointAlert{..} -> Just $ discordNewEndpointAlert project endpoints endpointHash projectUrl
            ReportAlert{..} -> Just $ discordReportAlert reportType startTime endTime totalErrors totalEvents breakDown pTitle reportUrl allChartUrl errorChartUrl
            MonitorsAlert{..} -> Just $ AE.object ["text" AE..= ("🤖 *Log Alert triggered for `" <> monitorTitle <> "`* \n<" <> monitorUrl <> "|View Monitor>")]
            LogPatternAlert{..} -> Just $ mkDiscordLogPatternPayload patternText issueUrl logLevel serviceName sourceField occurrenceCount sampleMessage pTitle
            LogPatternRateChangeAlert{..} -> Just $ mkDiscordLogPatternRateChangePayload patternText issueUrl logLevel serviceName direction currentRate baselineMean changePercent pTitle
            ShapeAlert -> Nothing
      case mkPayload alert of
        Nothing -> pure Nothing
        Just payload -> Notify.sendNotificationWithReply $ Notify.discordThreadedNotification cid payload replyToMsgIdM


-- | Send a Slack alert, optionally threading replies under a parent message.
-- Returns the thread timestamp if the send succeeds.
sendSlackAlert :: (DB es, Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> Maybe Text -> Eff es (Maybe Text)
sendSlackAlert = sendSlackAlertWith Nothing


-- | Internal: send Slack alert with optional thread-ts for threading
sendSlackAlertWith :: (DB es, Notify.Notify :> es, Reader Config.AuthContext :> es) => Maybe Text -> NotificationAlerts -> Projects.ProjectId -> Text -> Maybe Text -> Eff es (Maybe Text)
sendSlackAlertWith threadTsM alert pid pTitle channelM = do
  appCtx <- ask @Config.AuthContext
  slackData <- getProjectSlackData pid
  let channelIdM = channelM <|> fmap (.channelId) slackData
      botTokenM = (.botToken) <$> slackData
  case (channelIdM, botTokenM) of
    (Just cid, Just bt) -> do
      let projectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
          mkPayload = \case
            RuntimeErrorAlert{..} -> Just $ slackErrorAlert runtimeAlertType errorData pTitle cid projectUrl
            EndpointAlert{..} -> Just $ slackNewEndpointsAlert project endpoints cid endpointHash projectUrl
            ReportAlert{..} -> Just $ slackReportAlert reportType startTime endTime totalErrors totalEvents breakDown pTitle cid reportUrl allChartUrl errorChartUrl
            MonitorsAlert{..} -> Just $ AE.object ["blocks" AE..= AE.Array (V.fromList [AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("🤖 Alert triggered for " <> monitorTitle)]]])]
            LogPatternAlert{..} -> Just $ mkSlackLogPatternPayload patternText issueUrl logLevel serviceName sourceField occurrenceCount pTitle cid
            LogPatternRateChangeAlert{..} -> Just $ mkSlackLogPatternRateChangePayload patternText issueUrl logLevel serviceName direction currentRate baselineMean changePercent pTitle cid
            ShapeAlert -> Nothing
      case mkPayload alert of
        Nothing -> pure Nothing
        Just payload -> Notify.sendNotificationWithReply $ Notify.slackThreadedNotification cid bt payload threadTsM
    _ -> pure Nothing


sendWhatsAppAlert :: (Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> V.Vector Text -> Eff es ()
sendWhatsAppAlert alert pid pTitle tos = do
  appCtx <- ask @Config.AuthContext
  case alert of
    RuntimeErrorAlert{..} -> do
      let template = appCtx.config.whatsappErrorTemplate
          url = pid.toText <> "/anomalies/by_hash/" <> errorData.hash
          contentVars = AE.object ["1" AE..= ("*" <> pTitle <> "*"), "2" AE..= ("*" <> errorData.errorType <> "*"), "3" AE..= ("`" <> errorData.message <> "`"), "4" AE..= url]
      sendAlert template contentVars
      pass
    EndpointAlert{..} -> do
      let template = appCtx.config.whatsappEndpointTemplate
          url = pid.toText <> "/anomalies/by_hash/" <> endpointHash
          contentVars = AE.object ["1" AE..= ("*" <> pTitle <> "*"), "2" AE..= T.intercalate "." ((\x -> "`" <> x <> "`") <$> V.toList endpoints), "3" AE..= url]
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
    MonitorsAlert{} -> pass
    LogPatternAlert{} -> pass
    LogPatternRateChangeAlert{} -> pass
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


slackErrorAlert :: RuntimeAlertType -> Errors.ATError -> Text -> Text -> Text -> AE.Value
slackErrorAlert alertType err project channelId projectUrl =
  AE.object
    [ "blocks"
        AE..= AE.Array
          ( V.fromList
              [ AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= title]]
              , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("```" <> err.message <> "\n```")]]
              , AE.object
                  [ "type" AE..= "context"
                  , "elements" AE..= AE.Array (V.fromList $ AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Stack:* `" <> fromMaybe "" err.runtime <> "`")] : [AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Endpoint:* " <> enp)]])
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
    targetUrl = projectUrl <> "/anomalies/"
    title = "<" <> targetUrl <> "|" <> fst (runtimeAlertMessages alertType) <> " " <> err.errorType <> ">"
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


mkSlackLogPatternPayload :: Text -> Text -> Maybe Text -> Maybe Text -> Text -> Int -> Text -> Text -> AE.Value
mkSlackLogPatternPayload patternText issueUrl logLevel serviceName sourceField occurrenceCount project channelId =
  AE.object
    [ "blocks"
        AE..= AE.Array
          ( V.fromList
              [ AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("<" <> issueUrl <> "|:mag: New Log Pattern Detected>")]]
              , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("A new log pattern has been detected in *" <> project <> "*.")]]
              , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Pattern:*\n```" <> T.take 200 patternText <> "```")]]
              , AE.object
                  [ "type" AE..= "context"
                  , "elements"
                      AE..= AE.Array
                        ( V.fromList
                            [ AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Level:* " <> fromMaybe "—" logLevel)]
                            , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Service:* " <> fromMaybe "—" serviceName)]
                            , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Source:* " <> sourceField)]
                            , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Occurrences:* " <> show occurrenceCount)]
                            ]
                        )
                  ]
              , AE.object ["type" AE..= "divider"]
              , AE.object
                  [ "type" AE..= "actions"
                  , "elements" AE..= AE.Array (V.fromList [AE.object ["type" AE..= "button", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "🔍 Investigate", "emoji" AE..= True], "url" AE..= issueUrl, "style" AE..= "primary"]])
                  ]
              ]
          )
    , "channel" AE..= channelId
    ]


mkSlackLogPatternRateChangePayload :: Text -> Text -> Maybe Text -> Maybe Text -> Text -> Double -> Double -> Double -> Text -> Text -> AE.Value
mkSlackLogPatternRateChangePayload patternText issueUrl logLevel serviceName direction currentRate baselineMean changePercent project channelId =
  AE.object
    [ "blocks"
        AE..= AE.Array
          ( V.fromList
              [ AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("<" <> issueUrl <> "|" <> icon <> " Log Pattern Volume " <> T.toTitle direction <> ">")]]
              , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("A log pattern volume *" <> direction <> "* has been detected in *" <> project <> "*.")]]
              , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Pattern:*\n```" <> T.take 200 patternText <> "```")]]
              , AE.object
                  [ "type" AE..= "context"
                  , "elements"
                      AE..= AE.Array
                        ( V.fromList
                            [ AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Current:* " <> show (round currentRate :: Int) <> "/hr")]
                            , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Baseline:* " <> show (round baselineMean :: Int) <> "/hr")]
                            , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Change:* " <> show (round changePercent :: Int) <> "%")]
                            ]
                        )
                  ]
              , AE.object
                  [ "type" AE..= "context"
                  , "elements"
                      AE..= AE.Array
                        ( V.fromList
                            [ AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Level:* " <> fromMaybe "—" logLevel)]
                            , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Service:* " <> fromMaybe "—" serviceName)]
                            ]
                        )
                  ]
              , AE.object ["type" AE..= "divider"]
              , AE.object
                  [ "type" AE..= "actions"
                  , "elements" AE..= AE.Array (V.fromList [AE.object ["type" AE..= "button", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "🔍 Investigate", "emoji" AE..= True], "url" AE..= issueUrl, "style" AE..= "primary"]])
                  ]
              ]
          )
    , "channel" AE..= channelId
    ]
  where
    icon = if direction == "spike" then ":chart_with_upwards_trend:" else ":chart_with_downwards_trend:"


discordReportAlert :: Text -> Text -> Text -> Int -> Int -> V.Vector (Text, Int, Int) -> Text -> Text -> Text -> Text -> AE.Value
discordReportAlert reportType startTime endTime totalErrors totalEvents breakDown project url allUrl errUrl =
  AE.object
    [ "flags" AE..= 32768
    , "components"
        AE..= AE.Array
          ( V.fromList
              [ AE.object ["type" AE..= 10, "content" AE..= ("## 📊 " <> (if reportType == "weekly" then "Weekly" else "Daily") <> " Report for " <> project)]
              , AE.object ["type" AE..= 10, "content" AE..= ("**From:** " <> T.take 10 startTime <> "  **To:** " <> T.take 10 endTime)]
              , AE.object ["type" AE..= 10, "content" AE..= ("Total Events: **" <> show totalEvents <> "**" <> T.replicate 28 "  " <> " Total Errors: **" <> show totalErrors <> "**")]
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


discordErrorAlert :: RuntimeAlertType -> Errors.ATError -> Text -> Text -> AE.Value
discordErrorAlert alertType err project projectUrl =
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
  "content": #{snd (runtimeAlertMessages alertType)}
}|]
  where
    url = projectUrl <> "/anomalies/by_hash/" <> err.hash
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


mkDiscordLogPatternPayload :: Text -> Text -> Maybe Text -> Maybe Text -> Text -> Int -> Maybe Text -> Text -> AE.Value
mkDiscordLogPatternPayload patternText issueUrl logLevel serviceName sourceField occurrenceCount sampleMessage project =
  AE.object
    [ "embeds"
        AE..= AE.Array
          ( V.fromList
              [ AE.object
                  [ "title" AE..= "New Log Pattern Detected"
                  , "description" AE..= ("A new log pattern has been detected in **" <> project <> "**.\n\n```" <> T.take 200 patternText <> "```")
                  , "color" AE..= (5793266 :: Int)
                  , "fields"
                      AE..= AE.Array
                        ( V.fromList
                            $ catMaybes
                              [ Just $ AE.object ["name" AE..= "Level", "value" AE..= fromMaybe "—" logLevel, "inline" AE..= True]
                              , Just $ AE.object ["name" AE..= "Service", "value" AE..= fromMaybe "—" serviceName, "inline" AE..= True]
                              , Just $ AE.object ["name" AE..= "Source", "value" AE..= sourceField, "inline" AE..= True]
                              , Just $ AE.object ["name" AE..= "Occurrences", "value" AE..= show occurrenceCount, "inline" AE..= True]
                              , sampleMessage <&> \msg -> AE.object ["name" AE..= "Sample", "value" AE..= ("```" <> T.take 150 msg <> "```"), "inline" AE..= False]
                              ]
                        )
                  , "url" AE..= issueUrl
                  ]
              ]
          )
    , "content" AE..= "🔍 New Log Pattern"
    ]


mkDiscordLogPatternRateChangePayload :: Text -> Text -> Maybe Text -> Maybe Text -> Text -> Double -> Double -> Double -> Text -> AE.Value
mkDiscordLogPatternRateChangePayload patternText issueUrl logLevel serviceName direction currentRate baselineMean changePercent project =
  AE.object
    [ "embeds"
        AE..= AE.Array
          ( V.fromList
              [ AE.object
                  [ "title" AE..= ("Log Pattern Volume " <> T.toTitle direction)
                  , "description" AE..= ("A log pattern volume **" <> direction <> "** has been detected in **" <> project <> "**.\n\n```" <> T.take 200 patternText <> "```")
                  , "color" AE..= color
                  , "fields"
                      AE..= AE.Array
                        ( V.fromList
                            $ catMaybes
                              [ Just $ AE.object ["name" AE..= "Current Rate", "value" AE..= (show (round currentRate :: Int) <> "/hr"), "inline" AE..= True]
                              , Just $ AE.object ["name" AE..= "Baseline", "value" AE..= (show (round baselineMean :: Int) <> "/hr"), "inline" AE..= True]
                              , Just $ AE.object ["name" AE..= "Change", "value" AE..= (show (round changePercent :: Int) <> "%"), "inline" AE..= True]
                              , Just $ AE.object ["name" AE..= "Level", "value" AE..= fromMaybe "—" logLevel, "inline" AE..= True]
                              , Just $ AE.object ["name" AE..= "Service", "value" AE..= fromMaybe "—" serviceName, "inline" AE..= True]
                              ]
                        )
                  , "url" AE..= issueUrl
                  ]
              ]
          )
    , "content" AE..= (icon <> " Log Pattern Volume " <> T.toTitle direction)
    ]
  where
    color = if direction == "spike" then 16711680 :: Int else 16776960 -- Red for spike, yellow for drop
    icon = if direction == "spike" then "📈" else "📉"


sendPagerdutyAlertToService :: Notify.Notify :> es => Text -> NotificationAlerts -> Text -> Text -> Eff es ()
sendPagerdutyAlertToService integrationKey (MonitorsAlert monitorTitle monitorUrl) projectTitle _ =
  Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-alert-" <> monitorTitle) (projectTitle <> ": " <> monitorTitle) Notify.PDCritical (AE.object ["url" AE..= monitorUrl]) monitorUrl
sendPagerdutyAlertToService integrationKey (EndpointAlert project endpoints hash) projectTitle projectUrl =
  let endpointUrl = projectUrl <> "/anomalies/by_hash/" <> hash
      endpointNames = T.intercalate ", " $ V.toList endpoints
   in Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-endpoint-" <> hash) (projectTitle <> ": New Endpoints - " <> endpointNames) Notify.PDWarning (AE.object ["project" AE..= project, "endpoints" AE..= endpoints]) endpointUrl
sendPagerdutyAlertToService integrationKey RuntimeErrorAlert{issueId, errorData} projectTitle projectUrl =
  let errorUrl = projectUrl <> "/anomalies/by_hash/" <> errorData.hash
   in Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-error-" <> issueId) (projectTitle <> ": " <> errorData.errorType <> " - " <> T.take 100 errorData.message) Notify.PDError (AE.object ["error_type" AE..= errorData.errorType, "message" AE..= errorData.message]) errorUrl
sendPagerdutyAlertToService integrationKey LogPatternAlert{issueUrl, patternText, logLevel, serviceName} projectTitle _ =
  Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-logpattern-" <> T.take 40 patternText) (projectTitle <> ": New Log Pattern - " <> T.take 80 patternText) (maybe Notify.PDWarning (\l -> if l == "error" then Notify.PDCritical else Notify.PDWarning) logLevel) (AE.object ["pattern" AE..= patternText, "service" AE..= serviceName, "level" AE..= logLevel]) issueUrl
sendPagerdutyAlertToService integrationKey LogPatternRateChangeAlert{issueUrl, patternText, logLevel, serviceName, direction, currentRate, baselineMean, changePercent} projectTitle _ =
  Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-logpattern-rate-" <> T.take 40 patternText) (projectTitle <> ": Log Pattern " <> T.toTitle direction <> " - " <> T.take 60 patternText <> " (" <> show (round changePercent :: Int) <> "%)") (if direction == "spike" then Notify.PDCritical else Notify.PDWarning) (AE.object ["pattern" AE..= patternText, "direction" AE..= direction, "current_rate" AE..= currentRate, "baseline_mean" AE..= baselineMean, "service" AE..= serviceName]) issueUrl
sendPagerdutyAlertToService _ ReportAlert{} _ _ = pass
sendPagerdutyAlertToService _ ShapeAlert _ _ = pass


sampleAlert :: IssueType -> Text -> NotificationAlerts
sampleAlert = \case
  ApiChange -> \title -> EndpointAlert ("🧪 TEST: " <> title) (V.singleton "POST /api/users") "test-hash"
  RuntimeException ->
    const
      $ RuntimeErrorAlert
        "test-123"
        def
          { Errors.when = UTCTime (fromGregorian 2025 1 1) 0
          , Errors.errorType = "🧪 TEST: TypeError"
          , Errors.rootErrorType = "TypeError"
          , Errors.message = "Sample error message for testing"
          , Errors.rootErrorMessage = "Sample error"
          , Errors.stackTrace = "at sampleFunction (sample.js:42:15)"
          , Errors.hash = "test-hash-xyz"
          , Errors.technology = Just RequestDumps.JsExpress
          , Errors.requestMethod = Just "GET"
          , Errors.requestPath = Just "/api/test"
          , Errors.spanId = Just "test-span-id"
          , Errors.traceId = Just "test-trace-id"
          , Errors.serviceName = Just "api"
          , Errors.runtime = Just "nodejs"
          }
        NewRuntimeError
  QueryAlert -> const $ MonitorsAlert "🧪 TEST: High Error Rate" "https://example.com/test"
  LogPattern -> const $ MonitorsAlert "🧪 TEST: New Log Pattern" "https://example.com/test"
  LogPatternRateChange -> const $ MonitorsAlert "🧪 TEST: Log Pattern Rate Change" "https://example.com/test"


sampleRuntimeAlert :: RuntimeAlertType -> Text -> NotificationAlerts
sampleRuntimeAlert alertType title =
  let RuntimeErrorAlert{..} = sampleAlert RuntimeException title
   in RuntimeErrorAlert{runtimeAlertType = alertType, ..}


sampleAlertByIssueTypeText :: Text -> Text -> NotificationAlerts
sampleAlertByIssueTypeText issueTypeText title = case issueTypeText of
  "escalating_errors" -> sampleRuntimeAlert EscalatingErrors title
  "regressed_errors" -> sampleRuntimeAlert RegressedErrors title
  "error_spike" -> sampleRuntimeAlert ErrorSpike title
  _ -> sampleAlert (fromMaybe ApiChange $ parseIssueType issueTypeText) title


sampleReport :: Text -> NotificationAlerts
sampleReport title = ReportAlert ("🧪 TEST: " <> title) "2025-01-01" "2025-01-02" 42 1250 (V.singleton ("api", 42, 1250)) "https://example.com" "https://example.com/chart.png" "https://example.com/errors.png"


runtimeAlertMessages :: RuntimeAlertType -> (Text, Text)
runtimeAlertMessages = \case
  NewRuntimeError -> (":red_circle:", "**🔴 New Runtime Error**")
  EscalatingErrors -> (":warning: Escalating Error", "**🧵 Escalating Error Alert**")
  RegressedErrors -> (":repeat: Regressed Error", "**🧵 Regressed Error Alert**")
  ErrorSpike -> (":chart_with_upwards_trend: Error Spike", "**🧵 Error Spike Alert**")


addConvertKitUser :: HTTP :> es => Text -> Text -> Text -> Text -> Text -> Text -> Text -> Eff es ()
addConvertKitUser apiKey email firstName lastName orgId orgName plan = do
  void
    $ postWith
      (defaults & header "Content-Type" .~ ["application/json"])
      "https://api.convertkit.com/v3/forms/5502985/subscribe"
      [aesonQQ| {"api_key": #{apiKey}, "email": #{email}, "first_name": #{firstName}, "fields": {"last_name": #{lastName}}} |]


addConvertKitUserOrganization :: HTTP :> es => Text -> Text -> Text -> Text -> Text -> Eff es ()
addConvertKitUserOrganization apiKey email orgID orgName orgPlan = do
  void
    $ postWith
      (defaults & header "Content-Type" .~ ["application/json"])
      "https://api.convertkit.com/v3/tags/4059942/subscribe"
      [aesonQQ| {"api_key": #{apiKey}, "email": #{email}, "fields": {"organization_name": #{orgName}, "organization_plan": #{orgPlan}, "organization_id": #{orgID}}} |]
