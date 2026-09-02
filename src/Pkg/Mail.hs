module Pkg.Mail (sendSlackMessage, sendRenderedEmail, sendWhatsAppAlert, sendSlackAlert, sendSlackAlertWith, NotificationAlerts (..), RuntimeAlertType (..), sendDiscordAlert, sendDiscordAlertWith, sendPagerdutyAlertToService, sampleAlert, sampleAlertByIssueTypeText, sampleReport, addConvertKitUser, addConvertKitUserOrganization) where

import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KEM
import Data.Aeson.QQ (aesonQQ)
import Data.Default (def)
import Data.Effectful.Notify qualified as Notify
import Data.Effectful.Wreq (HTTP, defaults, header, postWith)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time
import Data.Vector qualified as V
import Effectful (Eff, IOE, type (:>))
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Integrations (SlackData (..), getProjectSlackData)
import Models.Apis.Issues (IssueType (..), RateChangeDirection (..), parseIssueType)
import Models.Apis.LogQueries qualified as LogQueries
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (urlEncode)
import Pkg.EmailTemplates (EndpointAlertRow (..), groupedByContext, stripSummaryBadges, traceExplorerUrl)
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
  maybe
    (Log.logAttention "sendSlackMessage is not configured. But was called" (pid, message))
    (\s -> Notify.sendNotification $ Notify.slackNotification s.channelId s.botToken [aesonQQ| {"text": #{message}, "type":"mrkdwn"} |])
    slackData


data NotificationAlerts
  = EndpointAlert {project :: Text, endpoints :: V.Vector EndpointAlertRow, endpointHash :: Text}
  | RuntimeErrorAlert {issueId :: Text, issueTitle :: Text, errorData :: ErrorPatterns.ATError, runtimeAlertType :: RuntimeAlertType, chartUrl :: Maybe Text, occurrenceText :: Maybe Text, firstSeenText :: Maybe Text, ongoingFor :: Maybe Text}
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
      , chartUrl :: Maybe Text
      }
  | LogPatternAlert
      { issueUrl :: Text
      , patternText :: Text
      , sampleMessage :: Maybe Text
      , logLevel :: Maybe Text
      , serviceName :: Maybe Text
      , sourceField :: Text
      , occurrenceCount :: Int
      , isError :: Bool
      }
  | LogPatternRateChangeAlert
      { issueUrl :: Text
      , patternText :: Text
      , sampleMessage :: Maybe Text
      , logLevel :: Maybe Text
      , serviceName :: Maybe Text
      , direction :: RateChangeDirection
      , currentRate :: Double
      , baselineMean :: Double
      , changePercent :: Double
      , isError :: Bool
      }
  | MonitorsRecoveryAlert
      { monitorTitle :: Text
      , monitorUrl :: Text
      }


data RuntimeAlertType
  = NewRuntimeError
  | EscalatingErrors
  | RegressedErrors
  | ErrorSpike
  deriving stock (Eq, Generic, Show)


-- | Send a Discord alert, optionally threading replies under a parent message.
-- Returns the message ID if threading is enabled and the send succeeds.
sendDiscordAlert :: (DB es, Log :> es, Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> Maybe Text -> Eff es (Maybe Text)
sendDiscordAlert = sendDiscordAlertWith Nothing


-- | Internal: send Discord alert with optional reply-to threading
sendDiscordAlertWith :: (DB es, Log :> es, Notify.Notify :> es, Reader Config.AuthContext :> es) => Maybe Text -> NotificationAlerts -> Projects.ProjectId -> Text -> Maybe Text -> Eff es (Maybe Text)
sendDiscordAlertWith replyToMsgIdM alert pid pTitle channelIdM' = do
  appCtx <- ask @Config.AuthContext
  -- When no explicit channel is supplied, fall back to the first entry of
  -- @everyone.discord_channels (insertion order; see addDiscordChannelToEveryoneTeam).
  channelIdM <- maybe ((>>= (V.!? 0) . (.discord_channels)) <$> ProjectMembers.getEveryoneTeam pid) (pure . Just) channelIdM'
  case channelIdM of
    Nothing -> Nothing <$ Log.logAttention "Discord alert skipped: no channel configured" (AE.object ["project_id" AE..= pid])
    Just cid -> do
      let projectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
          mkPayload = \case
            RuntimeErrorAlert{..} -> Just $ discordErrorAlert runtimeAlertType errorData pTitle projectUrl chartUrl occurrenceText firstSeenText ongoingFor
            EndpointAlert{..} -> Just $ discordNewEndpointAlert project endpoints endpointHash projectUrl
            ReportAlert{..} -> Just $ discordReportAlert reportType startTime endTime totalErrors totalEvents breakDown pTitle reportUrl allChartUrl errorChartUrl
            MonitorsAlert{..} -> Just $ discordMonitorAlert monitorTitle monitorUrl chartUrl
            MonitorsRecoveryAlert{..} -> Just $ AE.object ["text" AE..= ("✅ *Alert resolved for `" <> monitorTitle <> "`* \n<" <> monitorUrl <> "|View Monitor>")]
            LogPatternAlert{..} -> Just $ mkDiscordLogPatternPayload patternText issueUrl logLevel serviceName sourceField occurrenceCount sampleMessage pTitle isError
            LogPatternRateChangeAlert{..} -> Just $ mkDiscordLogPatternRateChangePayload patternText issueUrl logLevel serviceName direction currentRate baselineMean changePercent pTitle isError
            ShapeAlert -> Nothing
      maybe (pure Nothing) (\payload -> Notify.sendNotificationWithReply $ Notify.discordThreadedNotification cid payload replyToMsgIdM) (mkPayload alert)


-- | Send a Slack alert, optionally threading replies under a parent message.
-- Returns the thread timestamp if the send succeeds.
sendSlackAlert :: (DB es, Log :> es, Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> Maybe Text -> Eff es (Maybe Text)
sendSlackAlert = sendSlackAlertWith Nothing


-- | Internal: send Slack alert with optional thread-ts for threading.
-- Routing: if the target channel matches the project's OAuth-time default
-- (apis.slack.channel_id) AND a webhook URL is on file, post via the
-- channel-bound incoming webhook — works without bot membership, essential
-- for private channels picked at install time. Otherwise post via
-- chat.postMessage which supports threading but needs bot membership.
--
-- Threading caveat: @threadTsM@ is silently dropped on the webhook branch
-- — Slack's incoming-webhook API does not accept @thread_ts@. If a feature
-- relies on threaded replies, the thread parent must have been posted via
-- chat.postMessage (i.e. the target is NOT the OAuth-default channel, or no
-- webhook URL is on file).
sendSlackAlertWith :: (DB es, Log :> es, Notify.Notify :> es, Reader Config.AuthContext :> es) => Maybe Text -> NotificationAlerts -> Projects.ProjectId -> Text -> Maybe Text -> Eff es (Maybe Text)
sendSlackAlertWith threadTsM alert pid pTitle channelM = do
  appCtx <- ask @Config.AuthContext
  slackData <- getProjectSlackData pid
  case (channelM, slackData) of
    (Just cid, Just sd) -> do
      let projectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
          mkPayload = \case
            RuntimeErrorAlert{..} -> Just $ slackErrorAlert runtimeAlertType errorData pTitle cid projectUrl chartUrl occurrenceText firstSeenText ongoingFor
            EndpointAlert{..} -> Just $ slackNewEndpointsAlert project endpoints cid endpointHash projectUrl
            ReportAlert{..} -> Just $ slackReportAlert reportType startTime endTime totalErrors totalEvents breakDown pTitle cid reportUrl allChartUrl errorChartUrl
            MonitorsAlert{..} -> Just $ slackMonitorAlert monitorTitle monitorUrl chartUrl cid
            MonitorsRecoveryAlert{..} -> Just $ slackAttachment cid "#22c55e" [slackSection ("✅ *Resolved:* <" <> monitorUrl <> "|" <> monitorTitle <> ">")]
            LogPatternAlert{..} -> Just $ mkSlackLogPatternPayload patternText issueUrl logLevel serviceName sourceField occurrenceCount sampleMessage pTitle cid isError
            LogPatternRateChangeAlert{..} -> Just $ mkSlackLogPatternRateChangePayload patternText issueUrl logLevel serviceName direction currentRate baselineMean changePercent pTitle cid isError
            ShapeAlert -> Nothing
          isDefaultChannel = cid == sd.channelId
      case mkPayload alert of
        Nothing -> pure Nothing
        Just payload -> do
          -- Legacy installs (apis.slack row created between migrations 0077 and 0080)
          -- have no webhookUrl; they silently fall through to chat.postMessage and
          -- fail for private channels. Log a distinct attention so ops can prompt a re-OAuth.
          when (isDefaultChannel && isNothing sd.webhookUrl)
            $ Log.logAttention "Slack default channel has no webhook URL; falling back to chat.postMessage (will fail for private channels — user must re-OAuth)" (AE.object ["project_id" AE..= pid, "team_id" AE..= sd.teamId, "channel_id" AE..= cid])
          let notif = case (isDefaultChannel, sd.webhookUrl) of
                (True, Just url) -> Notify.slackWebhookNotification url cid payload
                _ -> Notify.slackThreadedNotification cid sd.botToken payload threadTsM
          Notify.sendNotificationWithReply $ Notify.withSlackContext pid.toText sd.teamId notif
    _ -> Nothing <$ Log.logAttention "Slack alert skipped: missing channel or slack data" (AE.object ["project_id" AE..= pid, "has_channel" AE..= isJust channelM, "has_slack" AE..= isJust slackData])


sendWhatsAppAlert :: (IOE :> es, Log :> es, Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> V.Vector Text -> Eff es ()
sendWhatsAppAlert alert pid pTitle tos = do
  appCtx <- ask @Config.AuthContext
  case alert of
    RuntimeErrorAlert{..} -> do
      let template = appCtx.config.whatsappErrorTemplate
          url = pid.toText <> "/issues/by_hash/" <> errorData.hash
          contentVars = AE.object ["1" AE..= ("*" <> pTitle <> "*"), "2" AE..= ("*" <> stripSummaryBadges issueTitle <> "*"), "3" AE..= ("`" <> errorData.message <> "`"), "4" AE..= url]
      sendAlert template contentVars
    EndpointAlert{..} -> do
      let template = appCtx.config.whatsappEndpointTemplate
          url = pid.toText <> "/issues/by_hash/" <> endpointHash
          contentVars = AE.object ["1" AE..= ("*" <> pTitle <> "*"), "2" AE..= T.intercalate "." ((\x -> "`" <> x.label <> "`") <$> V.toList endpoints), "3" AE..= url]
      sendAlert template contentVars
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
    ShapeAlert -> pass
    MonitorsAlert{..} -> sendMonitor appCtx.config.whatsappMonitorTemplate "Alerting" monitorTitle monitorUrl
    MonitorsRecoveryAlert{..} -> sendMonitor appCtx.config.whatsappMonitorTemplate "Recovered" monitorTitle monitorUrl
    LogPatternAlert{} -> pass
    LogPatternRateChangeAlert{} -> pass
  where
    sendAlert :: Notify.Notify :> es => Text -> AE.Value -> Eff es ()
    sendAlert template vars =
      forM_ tos $ \to ->
        Notify.sendNotification $ Notify.whatsappNotification template to vars
    sendMonitor templateM status title url = case templateM of
      Nothing -> Log.logAttention "WhatsApp monitor alert skipped: WHATSAPP_MONITOR_TEMPLATE is not configured" $ AE.object ["project_id" AE..= pid, "status" AE..= status]
      Just template ->
        sendAlert (Config.twilioContentSidText template)
          $ AE.object
            [ "1" AE..= ("*" <> pTitle <> "*")
            , "2" AE..= ("*" <> title <> "*")
            , "3" AE..= status
            , "4" AE..= url
            ]


slackReportAlert :: Text -> Text -> Text -> Int -> Int -> V.Vector (Text, Int, Int) -> Text -> Text -> Text -> Text -> Text -> AE.Value
slackReportAlert reportType startTime endTime totalErrors totalEvents breakDown project channelId url allUrl errUrl =
  slackAttachment
    channelId
    "#64748b"
    [ slackSection ("<" <> url <> "|📊 *" <> T.toTitle reportType <> " report* · " <> project <> ">")
    , slackContext ["*From:* " <> startTime <> "  *To:* " <> endTime <> "  *Events:* " <> show totalEvents <> "  *Errors:* " <> show totalErrors]
    , slackImage "Events" (Just $ "Events: " <> show totalEvents) allUrl
    , slackImage "Errors" (Just $ "Errors: " <> show totalErrors) errUrl
    , slackContext sumr
    , slackActions [slackButton "Open report" (Just "primary") url]
    ]
  where
    sumr = take 10 $ V.toList breakDown <&> \(name, errCount, evCount) -> "*" <> name <> ":* " <> show evCount <> " events · " <> show errCount <> " errors"


slackErrorAlert :: RuntimeAlertType -> ErrorPatterns.ATError -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AE.Value
slackErrorAlert alertType err project channelId projectUrl chartUrlM occTextM firstSeenM ongoingForM =
  slackAttachment channelId msgs.color
    $ [slackSection title, slackSection body]
    <> [slackContext meta | not (null meta)]
    <> maybeToList (slackImage "Error trend" Nothing <$> chartUrlM)
    <> [slackActions buttons]
  where
    targetUrl = projectUrl <> "/issues/by_hash/" <> err.hash
    msgs = runtimeAlertMessages alertType
    -- When ongoingForM is set, downgrade the headline from "New error/Escalating/etc."
    -- to "Still firing · <duration>": re-notify cadence implies operators already know
    -- the error exists; what they need now is how long it's been burning.
    (titleEmoji, titleLabel) = case ongoingForM of
      Just d -> (":hourglass_flowing_sand:", "Still firing · " <> d)
      Nothing -> (msgs.slackEmoji, msgs.alertLabel)
    title = "<" <> targetUrl <> "|" <> titleEmoji <> " *" <> titleLabel <> "* · " <> err.errorType <> " in " <> project <> ">"
    body = "```" <> T.take 600 err.message <> maybe "" ("\n" <>) (topStackFrame err.stackTrace) <> "```"
    firstSeen = fromMaybe (errFirstSeen err) firstSeenM
    tidM = err.traceId >>= guarded (not . T.null)
    meta =
      (errMetaPairs "First seen" err occTextM firstSeen <> maybeToList (("Trace",) . T.take 16 <$> tidM))
        <&> \(lbl, v) -> "*" <> lbl <> ":* " <> v
    buttons =
      slackButton "🔍 Investigate" (Just "primary") targetUrl
        : maybeToList (tidM <&> \tid -> slackButton "View trace" Nothing (traceExplorerUrl projectUrl tid err.when))


-- | First non-empty line of a stack trace that isn't a repeat of the error-type header
-- (i.e. the second line when there is one). Gives alert readers the file/line where the
-- exception was thrown — the single most useful context after the error message itself.
topStackFrame :: Text -> Maybe Text
topStackFrame = fmap (T.take 160) . viaNonEmpty last . take 2 . filter (not . T.null) . map T.strip . lines


-- | "`METHOD /path`" for alert metadata; drops whichever half is missing, empty when both are.
errRoute :: ErrorPatterns.ATError -> Text
errRoute err = case filter (not . T.null) (catMaybes [err.requestMethod, err.requestPath]) of
  [] -> ""
  parts -> "`" <> T.unwords parts <> "`"


errFirstSeen :: ErrorPatterns.ATError -> Text
errFirstSeen err = toText $ formatTime defaultTimeLocale "%b %-e · %-l:%M %p" err.when


slackMonitorAlert :: Text -> Text -> Maybe Text -> Text -> AE.Value
slackMonitorAlert monitorTitle monitorUrl chartUrlM channelId =
  slackAttachment channelId "#ef4444"
    $ [slackSection ("🚨 *Monitor alerting:* <" <> monitorUrl <> "|" <> monitorTitle <> ">")]
    <> maybeToList (slackImage "Monitor trend" Nothing <$> chartUrlM)
    <> [slackActions [slackButton "🔍 View monitor" (Just "primary") monitorUrl]]


-- | Markdown bullet list shared by Slack + Discord new-endpoint renderers.
bulletList :: [Text] -> Text
bulletList = T.intercalate "\n" . fmap (\x -> "• `" <> x <> "`")


slackNewEndpointsAlert :: Text -> V.Vector EndpointAlertRow -> Text -> Text -> Text -> AE.Value
slackNewEndpointsAlert projectName endpoints channelId hash projectUrl =
  slackAttachment channelId "#3b82f6" $ [headlineBlock] <> bodyBlocks <> [actionsBlock]
  where
    n = V.length endpoints
    headline = if n == 1 then "1 new endpoint" else show n <> " new endpoints"
    targetUrl = projectUrl <> "/issues/by_hash/" <> hash
    explorerUrl = newEndpointsExplorerUrl projectUrl ((.label) <$> endpoints)
    headlineBlock = slackSection ("<" <> targetUrl <> "|:large_blue_circle: *" <> headline <> "* · " <> projectName <> ">")
    actionsBlock = slackActions [slackButton "View in Explorer" (Just "primary") explorerUrl]
    -- Option A layout: host is the primary group header (bold, with globe), service · env
    -- drops to a dimmed context caption on the same line. Bullets below.
    groupHeader hostM ctxM = case hostM of
      Just h -> Just $ ":globe_with_meridians: *" <> h <> "*" <> foldMap ("  ·  " <>) ctxM
      Nothing -> ctxM <&> \c -> "_" <> c <> "_"
    -- Single group with a header → separate context block + flat bullets (nicer Slack styling).
    -- Multiple groups → one section per group with inline header + bullets.
    bodyBlocks = case groupedByContext endpoints of
      [((hostM, ctxM), labels)]
        | Just h <- groupHeader hostM ctxM -> [slackContext [h], slackSection (bulletList labels)]
        | otherwise -> [slackSection (bulletList labels)]
      groups -> groups <&> \((hostM, ctxM), labels) -> slackSection (maybe "" (<> "\n") (groupHeader hostM ctxM) <> bulletList labels)


-- | Build an explorer URL filtering by the given "METHOD /path" endpoint strings.
-- Matches on both @attributes.http.route@ and @attributes.url.path@ because endpoint
-- discovery (see @Models.Apis.Endpoints@) keys off @COALESCE(http.route, url.path)@ —
-- different SDKs populate different fields, so we OR them to mirror that fallback.
--
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Text as T
-- >>> import Network.HTTP.Types (urlDecode)
-- >>> let decode u = decodeUtf8 @Text (urlDecode True (encodeUtf8 (T.drop (T.length "https://app/log_explorer?query=") u)))
-- >>> decode (newEndpointsExplorerUrl "https://app" (V.fromList ["GET /home"]))
-- "(attributes.http.route == \"/home\" OR attributes.url.path == \"/home\")"
-- >>> decode (newEndpointsExplorerUrl "https://app" (V.fromList ["GET /home", "POST /users"]))
-- "(attributes.http.route in (\"/home\",\"/users\") OR attributes.url.path in (\"/home\",\"/users\"))"
newEndpointsExplorerUrl :: Text -> V.Vector Text -> Text
newEndpointsExplorerUrl projectUrl endpoints =
  projectUrl <> "/log_explorer?query=" <> decodeUtf8 (urlEncode True $ encodeUtf8 expr)
  where
    paths = (\x -> "\"" <> T.drop 1 (T.dropWhile (/= ' ') x) <> "\"") <$> V.toList endpoints
    clause field = case paths of
      [p] -> field <> " == " <> p
      ps -> field <> " in (" <> T.intercalate "," ps <> ")"
    expr = "(" <> clause "attributes.http.route" <> " OR " <> clause "attributes.url.path" <> ")"


-- | Pick title (emoji + label) and Slack/Discord accent color from severity signals.
-- @isError@ is the authoritative flag (set at extraction; OR-merged on upsert);
-- @logLevel@ only gates the "warning" branch since drain-derived rows don't set it.
logPatternSeverity :: Bool -> Maybe Text -> (Text, Text, Int, Text)
logPatternSeverity isError logLevel
  | isError = ("🚨", "New error log pattern", 15278902, "#ef4444")
  | any ((`elem` (["warn", "warning"] :: [Text])) . T.toLower) logLevel =
      ("⚠️", "New warning log pattern", 15909152, "#eab308")
  | otherwise = ("🔍", "New log pattern", 3901174, "#3b82f6")


-- | Rate-change presentation: (Slack shortcode, Discord emoji, hex color, Discord int color).
-- Red when the pattern is an error, amber on spike, blue on drop.
rateChangeSeverity :: Bool -> RateChangeDirection -> (Text, Text, Text, Int)
rateChangeSeverity isError direction
  | isError = (":rotating_light:", "🚨", "#ef4444", 15278902)
  | direction == Spike = (":chart_with_upwards_trend:", "📈", "#eab308", 15381768)
  | otherwise = (":chart_with_downwards_trend:", "📉", "#3b82f6", 3901174)


mkSlackLogPatternPayload :: Text -> Text -> Maybe Text -> Maybe Text -> Text -> Int -> Maybe Text -> Text -> Text -> Bool -> AE.Value
mkSlackLogPatternPayload patternText issueUrl logLevel serviceName sourceField occurrenceCount sampleMessage project channelId isError =
  slackAttachment channelId color
    $ [ slackSection ("<" <> issueUrl <> "|" <> emoji <> " *" <> label <> "* · " <> project <> ">")
      , slackSection (snippet 200 patternText)
      ]
    <> maybeToList (sampleMessage <&> \msg -> slackSection ("*Sample:*\n" <> snippet 200 msg))
    <> [ slackContext
           [ "*Level:* " <> fromMaybe "—" logLevel
           , "*Service:* " <> fromMaybe "—" serviceName
           , "*Source:* " <> sourceField
           , "*Matched:* " <> show occurrenceCount
           ]
       , slackActions [slackButton "🔍 Investigate" (Just "primary") issueUrl]
       ]
  where
    (emoji, label, _, color) = logPatternSeverity isError logLevel


mkSlackLogPatternRateChangePayload :: Text -> Text -> Maybe Text -> Maybe Text -> RateChangeDirection -> Double -> Double -> Double -> Text -> Text -> Bool -> AE.Value
mkSlackLogPatternRateChangePayload patternText issueUrl logLevel serviceName direction currentRate baselineMean changePercent project channelId isError =
  slackAttachment
    channelId
    color
    [ slackSection ("<" <> issueUrl <> "|" <> icon <> " *" <> errPrefix <> "Log volume " <> display direction <> "* · " <> project <> ">")
    , slackSection (snippet 200 patternText)
    , slackContext
        [ "*Now:* " <> show (round currentRate :: Int) <> "/hr"
        , "*Baseline:* " <> show (round baselineMean :: Int) <> "/hr"
        , "*Change:* " <> sign <> show (round changePercent :: Int) <> "%"
        , "*Level:* " <> fromMaybe "—" logLevel
        , "*Service:* " <> fromMaybe "—" serviceName
        ]
    , slackActions [slackButton "🔍 Investigate" (Just "primary") issueUrl]
    ]
  where
    (icon, _, color, _) = rateChangeSeverity isError direction
    sign = if direction == Spike then "+" else "-"
    errPrefix = if isError then "Error " else "" :: Text


discordReportAlert :: Text -> Text -> Text -> Int -> Int -> V.Vector (Text, Int, Int) -> Text -> Text -> Text -> Text -> AE.Value
discordReportAlert reportType startTime endTime totalErrors totalEvents breakDown project url allUrl errUrl =
  AE.object
    [ "flags" AE..= 32768
    , "components"
        AE..= arr
          [ text ("## 📊 " <> (if reportType == "weekly" then "Weekly" else "Daily") <> " Report for " <> project)
          , text ("**From:** " <> T.take 10 startTime <> "  **To:** " <> T.take 10 endTime)
          , text ("Total Events: **" <> show totalEvents <> "**" <> T.replicate 28 "  " <> " Total Errors: **" <> show totalErrors <> "**")
          , AE.object
              [ "type" AE..= 12
              , "items"
                  AE..= arr
                    [ AE.object ["media" AE..= AE.object ["url" AE..= allUrl, "description" AE..= "Total events"]]
                    , AE.object ["media" AE..= AE.object ["url" AE..= errUrl, "description" AE..= "Total errors"]]
                    ]
              ]
          , text servicesStat
          , AE.object ["type" AE..= 1, "components" AE..= arr [AE.object ["type" AE..= 2, "label" AE..= "Open report", "url" AE..= url, "style" AE..= 5]]]
          ]
    ]
  where
    text t = AE.object ["type" AE..= 10, "content" AE..= (t :: Text)]
    servicesStat =
      T.intercalate "\n" $ take 10 $ V.toList breakDown <&> \(name, errCount, evCount) -> "* **" <> name <> "**: Total errors-" <> show errCount <> ", Total events-" <> show evCount


discordErrorAlert :: RuntimeAlertType -> ErrorPatterns.ATError -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AE.Value
discordErrorAlert alertType err project projectUrl chartUrlM occTextM firstSeenM ongoingForM =
  discordEmbedMsg (maybe msgs.discordContent (\d -> "⏳ **Still firing · " <> d <> "**") ongoingForM)
    $ AE.object
    $ [ "title" AE..= (err.errorType <> " · " <> project)
      , "description" AE..= ("```" <> T.take 600 err.message <> maybe "" ("\n" <>) (topStackFrame err.stackTrace) <> "```")
      , "color" AE..= msgs.discordColor
      , "fields" AE..= arr (uncurry (discordField True) <$> fields)
      , "url" AE..= url
      ]
    <> maybeToList (chartUrlM <&> \u -> "image" AE..= AE.object ["url" AE..= u])
  where
    msgs = runtimeAlertMessages alertType
    url = projectUrl <> "/issues/by_hash/" <> err.hash
    firstSeen = fromMaybe (errFirstSeen err) firstSeenM
    -- Trace is a masked link (renders in embed *field values*): same Log Explorer jump
    -- as Slack's "View trace" button, since Discord embeds have no button affordance.
    fields =
      errMetaPairs "First Seen" err occTextM firstSeen
        <> maybeToList ((err.traceId >>= guarded (not . T.null)) <&> \tid -> ("Trace", "[" <> T.take 16 tid <> "](" <> traceExplorerUrl projectUrl tid err.when <> ")"))


discordMonitorAlert :: Text -> Text -> Maybe Text -> AE.Value
discordMonitorAlert monitorTitle monitorUrl chartUrlM =
  discordEmbedMsg ("🚨 **Monitor alerting:** " <> monitorTitle)
    $ AE.object
    $ ["title" AE..= ("🚨 Monitor alerting: " <> monitorTitle), "color" AE..= (15684432 :: Int), "url" AE..= monitorUrl]
    <> maybeToList (chartUrlM <&> \u -> "image" AE..= AE.object ["url" AE..= u])


discordNewEndpointAlert :: Text -> V.Vector EndpointAlertRow -> Text -> Text -> AE.Value
discordNewEndpointAlert projectName endpoints hash projectUrl =
  discordEmbedMsg content
    $ AE.object
      [ "title" AE..= title
      , "description" AE..= description
      , "color" AE..= (3901174 :: Int)
      , "fields" AE..= arr [discordField False "\x200b" explorerLink]
      , "url" AE..= url
      ]
  where
    n = V.length endpoints
    title = (if n == 1 then "🔵 1 new endpoint" else "🔵 " <> show n <> " new endpoints") <> " · " <> projectName
    content = if n == 1 then "🔵 **New endpoint detected**" else "🔵 **" <> show n <> " new endpoints detected**"
    url = projectUrl <> "/issues/by_hash/" <> hash
    explorerLink = "[View in Explorer](" <> newEndpointsExplorerUrl projectUrl ((.label) <$> endpoints) <> ")"
    -- Group header: bold globe-prefixed host then dimmed "service · env" caption, each
    -- on its own line, then the bullets.
    description =
      T.intercalate "\n\n" $ groupedByContext endpoints <&> \((hostM, ctxM), labels) ->
        foldMap (\h -> "🌐 **" <> h <> "**\n") hostM <> foldMap (\c -> "_" <> c <> "_\n") ctxM <> bulletList labels


mkDiscordLogPatternPayload :: Text -> Text -> Maybe Text -> Maybe Text -> Text -> Int -> Maybe Text -> Text -> Bool -> AE.Value
mkDiscordLogPatternPayload patternText issueUrl logLevel serviceName sourceField occurrenceCount sampleMessage project isError =
  discordEmbedMsg (emoji <> " **" <> label <> "**")
    $ AE.object
      [ "title" AE..= (emoji <> " " <> label <> " · " <> project)
      , "description" AE..= snippet 200 patternText
      , "color" AE..= color
      , "fields"
          AE..= arr
            ( [ discordField True "Level" (fromMaybe "—" logLevel)
              , discordField True "Service" (fromMaybe "—" serviceName)
              , discordField True "Source" sourceField
              , discordField True "Occurrences" (show occurrenceCount)
              ]
                <> maybeToList (sampleMessage <&> discordField False "Sample" . snippet 150)
            )
      , "url" AE..= issueUrl
      ]
  where
    (emoji, label, color, _) = logPatternSeverity isError logLevel


mkDiscordLogPatternRateChangePayload :: Text -> Text -> Maybe Text -> Maybe Text -> RateChangeDirection -> Double -> Double -> Double -> Text -> Bool -> AE.Value
mkDiscordLogPatternRateChangePayload patternText issueUrl logLevel serviceName direction currentRate baselineMean changePercent project isError =
  discordEmbedMsg (icon <> " **" <> errPrefix <> "Log volume " <> display direction <> "**")
    $ AE.object
      [ "title" AE..= (icon <> " " <> errPrefix <> "Log volume " <> display direction <> " · " <> project)
      , "description" AE..= snippet 200 patternText
      , "color" AE..= color
      , "fields"
          AE..= arr
            [ discordField True "Now" (show (round currentRate :: Int) <> "/hr")
            , discordField True "Baseline" (show (round baselineMean :: Int) <> "/hr")
            , discordField True "Change" (sign <> show (round changePercent :: Int) <> "%")
            , discordField True "Level" (fromMaybe "—" logLevel)
            , discordField True "Service" (fromMaybe "—" serviceName)
            ]
      , "url" AE..= issueUrl
      ]
  where
    (_, icon, _, color) = rateChangeSeverity isError direction
    errPrefix = if isError then "Error " else "" :: Text
    sign = if direction == Spike then "+" else "-"


sendPagerdutyAlertToService :: Notify.Notify :> es => Text -> NotificationAlerts -> Text -> Text -> Eff es ()
sendPagerdutyAlertToService integrationKey MonitorsAlert{monitorTitle, monitorUrl, chartUrl} projectTitle _ =
  Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-alert-" <> monitorTitle) (projectTitle <> ": " <> monitorTitle) Notify.PDCritical (AE.object $ ["url" AE..= monitorUrl] <> maybeToList (("chart_url" AE..=) <$> chartUrl)) monitorUrl
sendPagerdutyAlertToService integrationKey (MonitorsRecoveryAlert monitorTitle monitorUrl) projectTitle _ =
  Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDResolve ("monoscope-alert-" <> monitorTitle) (projectTitle <> ": Resolved - " <> monitorTitle) Notify.PDInfo (AE.object ["url" AE..= monitorUrl]) monitorUrl
sendPagerdutyAlertToService integrationKey (EndpointAlert project endpoints hash) projectTitle projectUrl =
  let endpointUrl = projectUrl <> "/issues/by_hash/" <> hash
      endpointNames = T.intercalate ", " $ (.label) <$> V.toList endpoints
      rowPayload r = AE.object $ ("endpoint" AE..= r.label) : [k AE..= v | (k, Just v) <- [("host", r.host), ("service", r.service), ("environment", r.environment)]]
   in Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-endpoint-" <> hash) (projectTitle <> ": New Endpoints - " <> endpointNames) Notify.PDWarning (AE.object ["project" AE..= project, "endpoints" AE..= (rowPayload <$> endpoints)]) endpointUrl
sendPagerdutyAlertToService integrationKey RuntimeErrorAlert{issueId, issueTitle, errorData, chartUrl} projectTitle projectUrl =
  let errorUrl = projectUrl <> "/issues/by_hash/" <> errorData.hash
   in Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-error-" <> issueId) (projectTitle <> ": " <> errorData.errorType <> " - " <> stripSummaryBadges issueTitle) Notify.PDError (AE.object $ ["error_type" AE..= errorData.errorType, "message" AE..= errorData.message] <> maybeToList (("chart_url" AE..=) <$> chartUrl)) errorUrl
sendPagerdutyAlertToService integrationKey LogPatternAlert{issueUrl, patternText, logLevel, serviceName, isError} projectTitle _ =
  let pat = stripSummaryBadges patternText
      kind = if isError then "Error Log Pattern" else "Log Pattern" :: Text
      sev = if isError || logLevel == Just "error" then Notify.PDCritical else Notify.PDWarning
   in Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-logpattern-" <> T.take 40 pat) (projectTitle <> ": New " <> kind <> " - " <> T.take 80 pat) sev (AE.object ["pattern" AE..= pat, "service" AE..= serviceName, "level" AE..= logLevel, "is_error" AE..= isError]) issueUrl
sendPagerdutyAlertToService integrationKey LogPatternRateChangeAlert{issueUrl, patternText, logLevel, serviceName, direction, currentRate, baselineMean, changePercent, isError} projectTitle _ =
  let pat = stripSummaryBadges patternText
      kind = if isError then "Error Log Pattern" else "Log Pattern" :: Text
      sev = if isError || direction == Spike then Notify.PDCritical else Notify.PDWarning
   in Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-logpattern-rate-" <> T.take 40 pat) (projectTitle <> ": " <> kind <> " " <> T.toTitle (display direction) <> " - " <> T.take 60 pat <> " (" <> show (round changePercent :: Int) <> "%)") sev (AE.object ["pattern" AE..= pat, "direction" AE..= direction, "current_rate" AE..= currentRate, "baseline_mean" AE..= baselineMean, "service" AE..= serviceName, "is_error" AE..= isError]) issueUrl
sendPagerdutyAlertToService _ ReportAlert{} _ _ = pass
sendPagerdutyAlertToService _ ShapeAlert _ _ = pass


sampleAlert :: IssueType -> Text -> NotificationAlerts
sampleAlert = \case
  ApiChange -> \title -> EndpointAlert ("🧪 TEST: " <> title) (V.singleton EndpointAlertRow{label = "POST /api/users", host = Just "api.example.com", service = Just "api-service", environment = Just "production"}) "test-hash"
  RuntimeException ->
    const
      $ RuntimeErrorAlert
        "test-123"
        "TEST: TypeError - Cannot read properties of undefined"
        def
          { ErrorPatterns.when = UTCTime (fromGregorian 2025 1 1) 0
          , ErrorPatterns.errorType = "🧪 TEST · TypeError"
          , ErrorPatterns.rootErrorType = "TypeError"
          , ErrorPatterns.message = "Cannot read properties of undefined (reading 'id') at /api/v1/orders/:orderId/items"
          , ErrorPatterns.rootErrorMessage = "Cannot read properties of undefined (reading 'id')"
          , ErrorPatterns.stackTrace = "TypeError: Cannot read properties of undefined (reading 'id')\n    at getOrderItems (/app/src/routes/orders.ts:87:24)\n    at Layer.handle [as handle_request] (/app/node_modules/express/lib/router/layer.js:95:5)"
          , ErrorPatterns.hash = "test-hash-xyz"
          , ErrorPatterns.technology = Just LogQueries.JsExpress
          , ErrorPatterns.requestMethod = Just "GET"
          , ErrorPatterns.requestPath = Just "/api/v1/orders/:orderId/items"
          , ErrorPatterns.environment = Just "production"
          , ErrorPatterns.spanId = Just "a1b2c3d4e5f67890"
          , ErrorPatterns.traceId = Just "296e419eef5ed7da296e419eef5ed7da"
          , ErrorPatterns.serviceName = Just "orders-api"
          , ErrorPatterns.runtime = Just "nodejs"
          }
        NewRuntimeError
        Nothing
        (Just "4/hr")
        (Just "just now")
        Nothing
  QueryAlert -> const $ MonitorsAlert "🧪 TEST: High Error Rate" "https://example.com/test" Nothing
  LogPattern -> const $ MonitorsAlert "🧪 TEST: New Log Pattern" "https://example.com/test" Nothing
  LogPatternRateChange -> const $ MonitorsAlert "🧪 TEST: Log Pattern Rate Change" "https://example.com/test" Nothing


sampleRuntimeAlert :: RuntimeAlertType -> Text -> NotificationAlerts
sampleRuntimeAlert alertType title = case sampleAlert RuntimeException title of
  r@RuntimeErrorAlert{} -> r{runtimeAlertType = alertType}
  other -> other


sampleAlertByIssueTypeText :: Text -> Text -> NotificationAlerts
sampleAlertByIssueTypeText issueTypeText title = case issueTypeText of
  "escalating_errors" -> sampleRuntimeAlert EscalatingErrors title
  "regressed_errors" -> sampleRuntimeAlert RegressedErrors title
  "error_spike" -> sampleRuntimeAlert ErrorSpike title
  _ -> sampleAlert (fromMaybe ApiChange $ parseIssueType issueTypeText) title


sampleReport :: Text -> NotificationAlerts
sampleReport title = ReportAlert ("🧪 TEST: " <> title) "2025-01-01" "2025-01-02" 42 1250 (V.singleton ("api", 42, 1250)) "https://example.com" "https://example.com/chart.png" "https://example.com/errors.png"


-- | Per-alert presentation: Slack-flavored emoji shortcode, short label for titles,
-- Discord content-field string, bar color (hex, used in Slack attachments and Discord
-- embed color), and Discord integer color (hex → decimal).
data AlertMessages = AlertMessages {slackEmoji :: Text, alertLabel :: Text, discordContent :: Text, color :: Text, discordColor :: Int}


runtimeAlertMessages :: RuntimeAlertType -> AlertMessages
runtimeAlertMessages = \case
  NewRuntimeError -> AlertMessages ":red_circle:" "New error" "**🔴 New error**" "#ef4444" 15684432
  EscalatingErrors -> AlertMessages ":warning:" "Escalating" "**⚠️ Error rate escalating**" "#f97316" 16344086
  RegressedErrors -> AlertMessages ":repeat:" "Returned" "**↩️ Resolved error returned**" "#3b82f6" 3901174
  ErrorSpike -> AlertMessages ":chart_with_upwards_trend:" "Spiking" "**📈 Error rate spiking**" "#eab308" 15381768


-- | Wrap Slack blocks in an attachment so Slack renders a colored left border bar.
-- Channel is top-level; blocks live inside the single attachment along with the color.
-- Incoming webhooks reject attachments missing a fallback — include one unconditionally
-- so the same payload works on both chat.postMessage and the webhook transport.
slackAttachment :: Text -> Text -> [AE.Value] -> AE.Value
slackAttachment channelId color blocks =
  AE.object
    [ "channel" AE..= channelId
    , "attachments" AE..= arr [AE.object ["color" AE..= color, "fallback" AE..= ("Monoscope alert" :: Text), "blocks" AE..= arr blocks]]
    ]


arr :: [AE.Value] -> AE.Value
arr = AE.Array . V.fromList


-- | Fenced code snippet, badge-stripped and length-capped, as used in every alert body.
snippet :: Int -> Text -> Text
snippet n t = "```" <> T.take n (stripSummaryBadges t) <> "```"


mrkdwn :: Text -> AE.Value
mrkdwn t = AE.object ["type" AE..= "mrkdwn", "text" AE..= t]


slackSection :: Text -> AE.Value
slackSection t = AE.object ["type" AE..= "section", "text" AE..= mrkdwn t]


slackContext :: [Text] -> AE.Value
slackContext ts = AE.object ["type" AE..= "context", "elements" AE..= arr (mrkdwn <$> ts)]


slackImage :: Text -> Maybe Text -> Text -> AE.Value
slackImage alt titleM url =
  AE.object
    $ ["type" AE..= "image", "image_url" AE..= url, "alt_text" AE..= alt]
    <> maybeToList (titleM <&> \t -> "title" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= t])


slackActions :: [AE.Value] -> AE.Value
slackActions bs = AE.object ["type" AE..= "actions", "elements" AE..= arr bs]


-- | Slack's button @style@ is an enum of exactly "primary" and "danger". A
-- "default" style is not "no style" — it makes chat.postMessage reject the
-- ENTIRE message with invalid_attachments, so every error alert carrying a
-- trace id silently failed to any channel reached over the chat API (the
-- webhook transport renders buttons as mrkdwn links, which is why the
-- install's default channel kept working and only extra channels went dark).
-- Omit the key instead: @styleM = Nothing@ is the unstyled button.
slackButton :: Text -> Maybe Text -> Text -> AE.Value
slackButton label styleM url =
  AE.object
    $ ["type" AE..= "button", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= label, "emoji" AE..= True], "url" AE..= url]
    <> maybeToList (("style" AE..=) <$> styleM)


discordField :: Bool -> Text -> Text -> AE.Value
discordField inline n v = AE.object ["name" AE..= n, "value" AE..= v, "inline" AE..= inline]


discordEmbedMsg :: Text -> AE.Value -> AE.Value
discordEmbedMsg content embed = AE.object ["embeds" AE..= arr [embed], "content" AE..= content]


-- | Metadata rows shared by the Slack + Discord runtime-error renderers, in display
-- order, with empty values dropped. @firstSeenLabel@ differs per transport.
errMetaPairs :: Text -> ErrorPatterns.ATError -> Maybe Text -> Text -> [(Text, Text)]
errMetaPairs firstSeenLabel err occTextM firstSeen =
  filter (not . T.null . snd) ([("Service", fromMaybe "" err.serviceName), ("Environment", fromMaybe "" err.environment), ("Route", errRoute err)] <> maybeToList (("Rate",) <$> occTextM))
    <> [(firstSeenLabel, firstSeen)]
    <> maybeToList (("Runtime",) <$> (err.runtime >>= guarded (not . T.null)))


addConvertKitUser :: HTTP :> es => Text -> Text -> Text -> Text -> Eff es ()
addConvertKitUser apiKey email firstName lastName =
  void
    $ postWith
      (defaults & header "Content-Type" .~ ["application/json"])
      "https://api.convertkit.com/v3/forms/5502985/subscribe"
      [aesonQQ| {"api_key": #{apiKey}, "email": #{email}, "first_name": #{firstName}, "fields": {"last_name": #{lastName}}} |]


addConvertKitUserOrganization :: HTTP :> es => Text -> Text -> Text -> Text -> Text -> Eff es ()
addConvertKitUserOrganization apiKey email orgID orgName orgPlan =
  void
    $ postWith
      (defaults & header "Content-Type" .~ ["application/json"])
      "https://api.convertkit.com/v3/tags/4059942/subscribe"
      [aesonQQ| {"api_key": #{apiKey}, "email": #{email}, "fields": {"organization_name": #{orgName}, "organization_plan": #{orgPlan}, "organization_id": #{orgID}}} |]
