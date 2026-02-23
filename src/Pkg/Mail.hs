{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pkg.Mail (sendSlackMessage, sendRenderedEmail, sendWhatsAppAlert, sendSlackAlert, sendSlackAlertThreaded, NotificationAlerts (..), RuntimeAlertType (..), sendDiscordAlert, sendDiscordAlertThreaded, sendPagerdutyAlertToService, sampleAlert, sampleAlertByIssueTypeText, sampleReport, addConvertKitUser, addConvertKitUserOrganization) where

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
      Notify.sendNotification $ Notify.slackNotification s.channelId s.webhookUrl payload
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


data RuntimeAlertType
  = NewRuntimeError
  | EscalatingErrors
  | RegressedErrors
  | ErrorSpike
  deriving stock (Eq, Generic, Show)


sendDiscordAlert :: (DB es, Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> Maybe Text -> Eff es ()
sendDiscordAlert alert pid pTitle channelIdM' = do
  appCtx <- ask @Config.AuthContext
  channelIdM <- maybe (getDiscordDataByProjectId pid <&> (>>= (.notifsChannelId))) (pure . Just) channelIdM'
  for_ channelIdM \cid -> do
    let projectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
        mkAlert = \case
          RuntimeErrorAlert{..} -> Just $ discordErrorAlert runtimeAlertType errorData pTitle projectUrl
          EndpointAlert{..} -> Just $ discordNewEndpointAlert project endpoints endpointHash projectUrl
          ReportAlert{..} -> Just $ discordReportAlert reportType startTime endTime totalErrors totalEvents breakDown pTitle reportUrl allChartUrl errorChartUrl
          MonitorsAlert{..} -> Just $ AE.object ["text" AE..= ("🤖 *Log Alert triggered for `" <> monitorTitle <> "`* \n<" <> monitorUrl <> "|View Monitor>")]
          ShapeAlert -> Nothing
    traverse_ (Notify.sendNotification . Notify.discordNotification cid) (mkAlert alert)


sendDiscordAlertThreaded :: (DB es, Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> Eff es (Maybe Text)
sendDiscordAlertThreaded alert pid pTitle channelIdM' replyToMsgIdM = do
  appCtx <- ask @Config.AuthContext
  channelIdM <- maybe (getDiscordDataByProjectId pid <&> (>>= (.notifsChannelId))) (pure . Just) channelIdM'
  case channelIdM of
    Nothing -> pure Nothing
    Just cid -> do
      let projectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
      case alert of
        RuntimeErrorAlert{..} ->
          Notify.sendNotificationWithReply $ Notify.discordThreadedNotification cid (discordErrorAlert runtimeAlertType errorData pTitle projectUrl) replyToMsgIdM
        _ -> do
          sendDiscordAlert alert pid pTitle channelIdM'
          pure Nothing


sendSlackAlert :: (DB es, Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> Maybe Text -> Eff es ()
sendSlackAlert alert pid pTitle channelM = do
  appCtx <- ask @Config.AuthContext
  slackData <- getProjectSlackData pid
  let channelIdM = channelM <|> fmap (.channelId) slackData
      webhookUrlM = (.webhookUrl) <$> slackData
  for_ ((,) <$> channelIdM <*> webhookUrlM) \(cid, wurl) -> do
    let projectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
        mkAlert = \case
          RuntimeErrorAlert{..} -> Just $ slackErrorAlert runtimeAlertType errorData pTitle cid projectUrl
          EndpointAlert{..} -> Just $ slackNewEndpointsAlert project endpoints cid endpointHash projectUrl
          ReportAlert{..} -> Just $ slackReportAlert reportType startTime endTime totalErrors totalEvents breakDown pTitle cid reportUrl allChartUrl errorChartUrl
          MonitorsAlert{..} -> Just $ AE.object ["blocks" AE..= AE.Array (V.fromList [AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("🤖 Alert triggered for " <> monitorTitle)]]])]
          ShapeAlert -> Nothing
    traverse_ (Notify.sendNotification . Notify.slackNotification cid wurl) (mkAlert alert)


sendSlackAlertThreaded :: (DB es, Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> Eff es (Maybe Text)
sendSlackAlertThreaded alert pid pTitle channelM threadTsM = do
  appCtx <- ask @Config.AuthContext
  channelIdM <- maybe (getProjectSlackData pid <&> fmap (.channelId)) (pure . Just) channelM
  case channelIdM of
    Nothing -> pure Nothing
    Just cid -> do
      let projectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
      case alert of
        RuntimeErrorAlert{..} ->
          Notify.sendNotificationWithReply $ Notify.slackThreadedNotification cid (slackErrorAlert runtimeAlertType errorData pTitle cid projectUrl) threadTsM
        _ -> do
          sendSlackAlert alert pid pTitle channelM
          pure Nothing


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
    MonitorsAlert a b -> pass
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
    title = "<" <> targetUrl <> "|" <> runtimeAlertPrefix alertType <> " " <> err.errorType <> ">"
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
  "content": #{runtimeDiscordContent alertType}
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
sendPagerdutyAlertToService _ ReportAlert{} _ _ = pass
sendPagerdutyAlertToService _ ShapeAlert _ _ = pass


sampleAlert :: IssueType -> Text -> NotificationAlerts
sampleAlert = \case
  APIChange -> \title -> EndpointAlert ("🧪 TEST: " <> title) (V.singleton "POST /api/users") "test-hash"
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
  _ -> const $ MonitorsAlert "🧪 TEST: High Error Rate" "https://example.com/test"


sampleAlertByIssueTypeText :: Text -> Text -> NotificationAlerts
sampleAlertByIssueTypeText issueTypeText title = case issueTypeText of
  "escalating_errors" -> sampleRuntimeAlert EscalatingErrors title
  "regressed_errors" -> sampleRuntimeAlert RegressedErrors title
  "error_spike" -> sampleRuntimeAlert ErrorSpike title
  _ -> sampleAlert (fromMaybe APIChange $ parseIssueType issueTypeText) title


sampleRuntimeAlert :: RuntimeAlertType -> Text -> NotificationAlerts
sampleRuntimeAlert alertType title =
  RuntimeErrorAlert
    "test-123"
    ( def
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
        , Errors.serviceName = Just ("api-" <> title)
        , Errors.runtime = Just "nodejs"
        }
    )
    alertType


sampleReport :: Text -> NotificationAlerts
sampleReport title = ReportAlert ("🧪 TEST: " <> title) "2025-01-01" "2025-01-02" 42 1250 (V.singleton ("api", 42, 1250)) "https://example.com" "https://example.com/chart.png" "https://example.com/errors.png"


runtimeAlertPrefix :: RuntimeAlertType -> Text
runtimeAlertPrefix = \case
  NewRuntimeError -> ":red_circle:"
  EscalatingErrors -> ":warning: Escalating Error"
  RegressedErrors -> ":repeat: Regressed Error"
  ErrorSpike -> ":chart_with_upwards_trend: Error Spike"


runtimeDiscordContent :: RuntimeAlertType -> Text
runtimeDiscordContent = \case
  NewRuntimeError -> "**🔴 New Runtime Error**"
  EscalatingErrors -> "**🧵 Escalating Error Alert**"
  RegressedErrors -> "**🧵 Regressed Error Alert**"
  ErrorSpike -> "**🧵 Error Spike Alert**"


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
