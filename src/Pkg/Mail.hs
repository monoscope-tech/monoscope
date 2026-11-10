{-# LANGUAGE OrPatterns #-}

module Pkg.Mail (AlertImpact (..), monitorDataUnavailableMessage, errorIncidentMessages, resolvedErrorMessage, activityReply, withOwner, Card, card, logPatternCard, rateChangeCard, cardMessages, uptimeMessages, cronMessages, recoveredMessages, uptimeName, cronName, RateChange (..), slackNewEndpointsAlert, slackReportAlert, monitorIncidentMessages, retainSlackSnapshot, sendSlackMessage, sendRenderedEmail, sendWhatsAppAlert, sendSlackAlert, NotificationAlerts (..), RuntimeAlertType (..), sendDiscordAlert, sendPagerdutyAlertToService, sampleAlertByIssueTypeText, sampleReport, addConvertKitUser, addConvertKitUserOrganization, arr, mrkdwn, plainTxt, slackEscape, slackSection, slackHeader, slackContext, slackImage, dcGallery, slackActions) where

import Control.Lens (Traversal', ix, (%~), (.~), (^..), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KEM
import Data.Aeson.Lens (key, nth, _Array, _String)
import Data.Aeson.QQ (aesonQQ)
import Data.Default (def)
import Data.Effectful.Notify qualified as Notify
import Data.Effectful.Wreq (HTTP, defaults, header, postWith)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Vector qualified as V
import Effectful (Eff, IOE, type (:>))
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Incidents qualified as Incidents
import Models.Apis.Integrations (SlackData (..), getProjectSlackData)
import Models.Apis.Issues (IssueType (..), RateChangeDirection (..), parseIssueType)
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogQueries qualified as LogQueries
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Pkg.EmailTemplates (EndpointAlertRow (..), NewLogPattern (..), groupedByContext, stripSummaryBadges, traceExplorerUrl)
import Relude hiding (Reader, ask)
import System.Config (AuthContext (env))
import System.Config qualified as Config
import System.Logging qualified as Log
import System.Types (DB)
import Utils (countNoun, fmtDate, formatWithCommas, hostPath, prettyPrintCount)
import Web.Links (CookieProtectedRoutes (..), IssuesRoutes' (..), LogExplorerRoutes' (..), logExplorerLinks, urls)


sendRenderedEmail :: Notify.Notify :> es => Text -> Text -> Text -> Eff es ()
sendRenderedEmail receiver subject htmlBody =
  Notify.sendNotification $ Notify.emailNotification receiver subject htmlBody


sendSlackMessage :: (DB es, IOE :> es, Log :> es, Notify.Notify :> es) => Projects.ProjectId -> Text -> Eff es ()
sendSlackMessage pid message =
  getProjectSlackData pid
    >>= maybe
      (Log.logAttention "sendSlackMessage is not configured. But was called" (pid, message))
      (\s -> Notify.sendNotification $ Notify.slackNotification s.channelId s.botToken [aesonQQ| {"text": #{message}, "type":"mrkdwn"} |])


data NotificationAlerts
  = EndpointAlert {project :: Text, endpoints :: V.Vector EndpointAlertRow, endpointHash :: Text}
  | RuntimeErrorAlert {issueId :: Text, issueTitle :: Text, errorData :: ErrorPatterns.ATError, runtimeAlertType :: RuntimeAlertType, chartUrl :: Maybe Text, occurrenceText :: Maybe Text, firstSeenText :: Maybe Text, ongoingFor :: Maybe Text}
  | ShapeAlert
  | ReportAlert
      { reportType :: Projects.ReportType
      , startTime :: Text
      , endTime :: Text
      , totalErrors :: Int
      , totalEvents :: Int
      , breakDown :: V.Vector (Text, Int, Int)
      , reportUrl :: Text
      , allChartUrl :: Maybe Text
      , errorChartUrl :: Maybe Text
      }
  | MonitorsAlert
      { monitorTitle :: Text
      , monitorUrl :: Text
      , chartUrl :: Maybe Text
      }
  | LogPatternAlert NewLogPattern
  | LogPatternRateChangeAlert RateChange
  | MonitorsRecoveryAlert
      { monitorTitle :: Text
      , monitorUrl :: Text
      }


data RateChange = RateChange
  { issueUrl :: Text
  , patternText :: Text
  , logLevel :: Maybe Text
  , serviceName :: Maybe Text
  , direction :: RateChangeDirection
  , currentRate :: Double
  , baselineMean :: Double
  , changePercent :: Double
  , isError :: Bool
  , chartUrl :: Maybe Text
  }


data RuntimeAlertType
  = NewRuntimeError
  | EscalatingErrors
  | RegressedErrors
  | ErrorSpike
  deriving stock (Eq, Generic, Show)


-- | Operational and user context for one alert. Project and tenant stay
-- distinct so the fallback context is never mislabeled as a telemetry tenant.
data AlertImpact = AlertImpact
  { projectName :: Text
  , tenant :: Maybe Text
  , userName :: Maybe Text
  , userEmail :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)


-- | Send a Discord alert, optionally as a reply under a parent message.
-- Returns the message ID if threading is enabled and the send succeeds.
sendDiscordAlert :: (DB es, IOE :> es, Log :> es, Notify.Notify :> es, Reader Config.AuthContext :> es) => Maybe Text -> NotificationAlerts -> Projects.ProjectId -> Text -> Maybe Text -> Eff es (Maybe Text)
sendDiscordAlert replyToMsgIdM alert pid pTitle channelIdM' = do
  appCtx <- ask @Config.AuthContext
  -- When no explicit channel is supplied, fall back to the first entry of
  -- @everyone.discord_channels (insertion order; see addDiscordChannelToEveryoneTeam).
  channelIdM <- maybe ((>>= (V.!? 0) . (.discord_channels)) <$> ProjectMembers.getEveryoneTeam pid) (pure . Just) channelIdM'
  case channelIdM of
    Nothing -> Nothing <$ Log.logAttention "Discord alert skipped: no channel configured" (AE.object ["project_id" AE..= pid])
    Just cid -> do
      let projectUrl = hostPath appCtx.env.hostUrl $ urls.dashboardRedirectGet pid []
          byHash h = hostPath appCtx.env.hostUrl $ (urls.issues pid).detailHashGet h Nothing Nothing Nothing Nothing Nothing
          mkPayload = \case
            RuntimeErrorAlert{..} -> Just $ discordErrorAlert runtimeAlertType errorData pTitle projectUrl (byHash errorData.hash) chartUrl occurrenceText firstSeenText ongoingFor
            EndpointAlert{..} -> Just $ discordNewEndpointAlert project endpoints (byHash endpointHash) projectUrl
            ReportAlert{..} -> Just $ discordReportAlert reportType startTime endTime totalErrors totalEvents breakDown pTitle reportUrl allChartUrl errorChartUrl
            MonitorsAlert{..} -> Just $ discordMonitorAlert monitorTitle monitorUrl chartUrl
            MonitorsRecoveryAlert{..} -> Just $ AE.object ["text" AE..= ("✅ *Alert resolved for `" <> monitorTitle <> "`* \n<" <> monitorUrl <> "|View Monitor>")]
            LogPatternAlert lp -> Just $ mkDiscordLogPatternPayload lp pTitle
            LogPatternRateChangeAlert rc -> Just $ mkDiscordLogPatternRateChangePayload rc pTitle
            ShapeAlert -> Nothing
      join <$> forM (mkPayload alert) \payload -> Notify.sendNotificationWithReply $ Notify.discordThreadedNotification cid payload replyToMsgIdM


-- | Send a Slack alert, optionally threaded under @threadTsM@; returns the thread
-- timestamp if the send succeeds.
-- Routing: if the target channel matches the project's OAuth-time default
-- (apis.slack.channel_id) AND a webhook URL is on file, post via the
-- channel-bound incoming webhook — works without bot membership, essential
-- for private channels picked at install time. Otherwise post via
-- chat.postMessage which supports threading but needs bot membership.
--
-- Both transports preserve thread context. Webhook root timestamps require
-- separate capture through Slack events or message retrieval.
sendSlackAlert :: (DB es, IOE :> es, Log :> es, Notify.Notify :> es, Reader Config.AuthContext :> es) => Maybe Text -> NotificationAlerts -> Projects.ProjectId -> Text -> Maybe Text -> Eff es (Maybe Text)
sendSlackAlert threadTsM alert pid pTitle channelM = do
  appCtx <- ask @Config.AuthContext
  slackData <- getProjectSlackData pid
  case (channelM, slackData) of
    (Just cid, Just sd) -> do
      let projectUrl = hostPath appCtx.env.hostUrl $ urls.dashboardRedirectGet pid []
          byHash h = hostPath appCtx.env.hostUrl $ (urls.issues pid).detailHashGet h Nothing Nothing Nothing Nothing Nothing
          mkPayload =
            fmap AE.toJSON . \case
              RuntimeErrorAlert{..} -> Just $ slackErrorAlert runtimeAlertType errorData pTitle projectUrl (byHash errorData.hash) chartUrl occurrenceText firstSeenText ongoingFor
              EndpointAlert{..} -> Just $ slackNewEndpointsAlert project endpoints (byHash endpointHash) projectUrl
              ReportAlert{..} -> Just $ slackReportAlert reportType startTime endTime totalErrors totalEvents breakDown pTitle reportUrl allChartUrl errorChartUrl
              MonitorsAlert{..} -> Just $ slackMonitorAlert sAlerting monitorTitle monitorUrl chartUrl
              MonitorsRecoveryAlert{..} -> Just $ slackMonitorAlert sRecovered monitorTitle monitorUrl Nothing
              LogPatternAlert lp -> Just $ card $ logPatternCard lp pTitle
              LogPatternRateChangeAlert rc -> Just $ card $ rateChangeCard rc pTitle
              ShapeAlert -> Nothing
          isDefaultChannel = cid == sd.channelId
      join <$> forM (mkPayload alert) \payload -> do
        -- Legacy installs (apis.slack row created between migrations 0077 and 0080)
        -- have no webhookUrl; they silently fall through to chat.postMessage and
        -- fail for private channels. Log a distinct attention so ops can prompt a re-OAuth.
        when (isDefaultChannel && isNothing sd.webhookUrl)
          $ Log.logAttention "Slack default channel has no webhook URL; falling back to chat.postMessage (will fail for private channels — user must re-OAuth)" (AE.object ["project_id" AE..= pid, "team_id" AE..= sd.teamId, "channel_id" AE..= cid])
        let notif = case (isDefaultChannel, sd.webhookUrl) of
              (True, Just url) -> Notify.slackWebhookNotification url cid payload threadTsM
              _ -> Notify.slackThreadedNotification cid sd.botToken payload threadTsM
        Notify.sendNotificationWithReply $ Notify.withSlackContext pid.toText sd.teamId notif
    _ -> Nothing <$ Log.logAttention "Slack alert skipped: missing channel or slack data" (AE.object ["project_id" AE..= pid, "has_channel" AE..= isJust channelM, "has_slack" AE..= isJust slackData])


sendWhatsAppAlert :: (IOE :> es, Log :> es, Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> V.Vector Text -> Eff es ()
sendWhatsAppAlert alert pid pTitle tos = do
  appCtx <- ask @Config.AuthContext
  case alert of
    RuntimeErrorAlert{..} ->
      sendAlert appCtx.config.whatsappErrorTemplate
        $ AE.object ["1" AE..= ("*" <> pTitle <> "*"), "2" AE..= ("*" <> stripSummaryBadges issueTitle <> "*"), "3" AE..= ("`" <> errorData.message <> "`"), "4" AE..= (pid.toText <> "/issues/by_hash/" <> errorData.hash)]
    EndpointAlert{..} ->
      sendAlert appCtx.config.whatsappEndpointTemplate
        $ AE.object ["1" AE..= ("*" <> pTitle <> "*"), "2" AE..= T.intercalate "." ((\x -> "`" <> x.label <> "`") <$> V.toList endpoints), "3" AE..= (pid.toText <> "/issues/by_hash/" <> endpointHash)]
    ReportAlert{..} -> do
      let template = appCtx.config.whatsappAllReportTemplate
          templateErr = appCtx.config.whatsappErrorReportTemplate
          urlVar = fromMaybe "" $ viaNonEmpty last $ T.splitOn "/p/" reportUrl
          cUrl = fromMaybe "" $ viaNonEmpty last . T.splitOn "?" =<< allChartUrl
          eUrl = fromMaybe "" $ viaNonEmpty last . T.splitOn "?" =<< errorChartUrl
          contentVars =
            KEM.fromList
              [ "1" AE..= display reportType
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
    sendAlert template vars = for_ tos \to -> Notify.sendNotification $ Notify.whatsappNotification template to vars
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


-- | First non-empty line of a stack trace that isn't a repeat of the error-type header
-- (i.e. the second line when there is one). Gives alert readers the file/line where the
-- exception was thrown — the single most useful context after the error message itself.
topStackFrame :: Text -> Maybe Text
topStackFrame = fmap (T.take 160) . viaNonEmpty last . take 2 . filter (not . T.null) . map T.strip . lines


-- | "`METHOD /path`" for alert metadata; drops whichever half is missing, empty when both are.
errRoute :: ErrorPatterns.ATError -> Text
errRoute err = case filter (not . T.null) (catMaybes [err.requestMethod, err.requestPath]) of
  [] -> ""
  parts -> "`" <> unwords parts <> "`"


errFirstSeen :: ErrorPatterns.ATError -> Text
errFirstSeen err = fmtDate "%b %-e · %-l:%M %p" err.when


-- | How loudly a status speaks. The bar colour always agrees with the emoji and
-- the word beside it, so colour is never the only signal.
data Tone = Critical | Elevated | Calm | Healthy | Neutral
  deriving stock (Eq, Show)


data Status = Status {emoji :: Text, label :: Text, tone :: Tone}
  deriving stock (Eq, Show)


-- | One anatomy for every alert: the status leads, quiet facts follow, then the
-- evidence, the chart, a footer, and at most three actions. Blocks carry stable
-- ids so a later edit of the root can replace exactly the parts that changed.
data Card = Card
  { status :: Status
  , title :: Text
  , url :: Text
  , note :: [Text]
  , facts :: [Text]
  , body :: [AE.Value]
  , chart :: Maybe (Text, Maybe Text)
  , footer :: [Text]
  , actions :: [(Text, Text)]
  }


-- | A card with only a status line; builders fill in what they have, and a root
-- edit built from one leaves the rest to 'retainSlackSnapshot'.
statusCard :: Status -> Text -> Text -> Card
statusCard status title url = Card{status, title, url, note = [], facts = [], body = [], chart = Nothing, footer = [], actions = []}


bold :: Text -> Text
bold t = "*" <> slackEscape t <> "*"


card :: Card -> Incidents.SlackPayload
card c =
  Incidents.SlackPayload $ KEM.fromList ["text" AE..= fallback, "attachments" AE..= arr [AE.object ["color" AE..= color, "fallback" AE..= fallback, "blocks" AE..= blocks]]]
  where
    fallback = c.status.label <> " · " <> T.take 160 title
    color :: Text = case c.status.tone of
      Critical -> "#E5484D"
      Elevated -> "#F5A524"
      Calm -> "#3E63DD"
      Healthy -> "#30A46C"
      Neutral -> "#8B8D98"
    title = if T.null (T.strip c.title) then "Untitled" else c.title
    blocks =
      [tagged "status" $ slackSection $ statusText c.status <> "  <" <> c.url <> "|" <> incidentHeadline title <> ">"]
        <> [tagged "note" $ slackContext [dots c.note] | not (null c.note)]
        <> [tagged "facts" $ slackContext [dots c.facts] | not (null c.facts)]
        <> c.body
        <> foldMap (\(alt, img) -> [tagged "chart" $ maybe (slackContext ["Chart unavailable · <" <> c.url <> "|open the issue>"]) (slackImage alt Nothing) img]) c.chart
        <> [tagged "footer" $ slackContext [dots c.footer] | not (null c.footer)]
        -- Slack's primary button is green; beside a red or amber bar it reads as "fine".
        <> [tagged "actions" $ slackActions $ zipWith (\i (l, u) -> slackButton l (Just "primary" <* guard (i == 0 && c.status.tone `notElem` [Critical, Elevated])) u) [0 :: Int ..] c.actions | not (null c.actions)]


-- | A card's root message, and its one-line reply for when it happens again.
cardMessages :: Card -> (Incidents.SlackPayload, Incidents.SlackPayload)
cardMessages c = (card c, reply c.status c.title c.facts)


-- | A thread reply is one line: what changed, then the numbers that say how much.
-- Its notification text names the issue, since a phone shows nothing else.
reply :: Status -> Text -> [Text] -> Incidents.SlackPayload
reply s title details = oneLine title $ dots (statusText s : details)


oneLine :: Text -> Text -> Incidents.SlackPayload
oneLine title line = Incidents.SlackPayload $ KEM.fromList ["text" AE..= (T.take 120 title <> ": " <> T.filter (/= '*') (T.takeWhile (/= '\n') line)), "blocks" AE..= arr [slackSection line]]


-- | The root states who holds the issue now, so the channel can see someone is on it.
withOwner :: Maybe Issues.Ownership -> Incidents.SlackPayload -> Incidents.SlackPayload
withOwner owner (Incidents.SlackPayload message) = Incidents.SlackPayload $ message & rootBlocks %~ place
  where
    place blocks = V.fromList $ case filter ((/= Just "owner") . blockId) (toList blocks) of
      status : rest -> let (note, others) = span ((== Just "note") . blockId) rest in status : note <> ownerBlock <> others
      [] -> []
    ownerBlock = foldMap (\o -> [tagged "owner" $ slackContext [dots $ catMaybes [o.acknowledgedUntil <&> \t -> ":eyes: Acknowledged" <> foldMap ((" by " <>) . bold) o.acknowledgedBy <> bool (" until " <> atUtc t) "" (t >= Issues.indefiniteUntil), o.assignee <&> \a -> ":bust_in_silhouette: Assigned to " <> bold a]] | isJust o.acknowledgedUntil || isJust o.assignee]) owner


statusText :: Status -> Text
statusText s = s.emoji <> " *" <> s.label <> "*"


dots :: [Text] -> Text
dots = T.intercalate "  ·  " . filter (not . T.null)


sNewError, sRegressed, sEscalating, sSpiking, sStillFailing, sAlerting, sWarning, sRecovered, sResolved, sNoData :: Status
sNewError = Status ":red_circle:" "New error" Critical
sRegressed = Status ":red_circle:" "Regressed" Critical
sEscalating = Status ":large_orange_circle:" "Escalating" Elevated
sSpiking = Status ":large_orange_circle:" "Spiking" Elevated
sStillFailing = Status ":red_circle:" "Still occurring" Critical
sAlerting = Status ":red_circle:" "Alerting" Critical
sWarning = Status ":large_yellow_circle:" "Warning" Elevated
sRecovered = Status ":large_green_circle:" "Recovered" Healthy
sResolved = Status ":white_check_mark:" "Resolved" Healthy
sNoData = Status ":white_circle:" "No data" Neutral


runtimeStatus :: RuntimeAlertType -> Status
runtimeStatus = \case
  NewRuntimeError -> sNewError
  RegressedErrors -> sRegressed
  EscalatingErrors -> sEscalating
  ErrorSpike -> sSpiking


errorCard :: Status -> ErrorPatterns.ATError -> Text -> Text -> Maybe Text -> Maybe Text -> [Text] -> Card
errorCard status err projectUrl issueUrl chartUrl rate footer =
  (statusCard status (errorAlertTitle err) issueUrl)
    { facts = errorFacts err <> foldMap (pure . ("*" <>) . (<> "*")) rate
    , -- The title already carries the message; the frames are the new evidence.
      body = [tagged "evidence" $ slackSection $ maybe (errorSnippet 600 err) (\fs -> "```" <> T.replace "```" "'''" (T.intercalate "\n" (toList fs)) <> "```") (frames <* guard (T.isInfixOf (T.take 40 $ unwords $ words err.message) (errorAlertTitle err)))]
    , chart = Just ("Error occurrences · " <> errorAlertTitle err, chartUrl)
    , footer
    , actions = ("Open issue", issueUrl) : maybeToList ((err.traceId >>= guarded (not . T.null)) <&> \tid -> ("View trace", traceExplorerUrl projectUrl tid err.when))
    }
  where
    frames = nonEmpty $ take 3 $ drop 1 $ filter (not . T.null) $ map (T.take 160 . T.strip) $ lines err.stackTrace


-- | Where an error came from: service, environment, route.
errorFacts :: ErrorPatterns.ATError -> [Text]
errorFacts err = catMaybes [bold <$> err.serviceName, slackEscape <$> err.environment, guarded (not . T.null) (errRoute err)]


-- | Readings print as people write them: grouped thousands, at most two decimals.
--
-- >>> map reading [1840.3333, 0.5, 1200, 12.0]
-- ["1,840.33","0.5","1,200","12"]
reading :: Double -> Text
reading v = formatWithCommas (fromIntegral (round (v * 100) :: Integer) / 100)


-- | "6 min", "3 h 5 min", "2 d 4 h": how long something has been going on.
--
-- >>> map (lasted . fromIntegral) [20, 360, 11100, 187200 :: Int]
-- ["under a minute","6 min","3 h 5 min","2 d 4 h"]
lasted :: NominalDiffTime -> Text
lasted d
  | s < 60 = "under a minute"
  | s < 3600 = show (s `div` 60) <> " min"
  | s < 86400 = show (s `div` 3600) <> " h" <> part ((s `mod` 3600) `div` 60) " min"
  | otherwise = show (s `div` 86400) <> " d" <> part ((s `mod` 86400) `div` 3600) " h"
  where
    s = max 0 (floor d) :: Int
    part n unit = if n == 0 then "" else " " <> show n <> unit


-- | A monitor episode as a root card and a one-line thread reply.
-- The root keeps its onset chart; replies carry only the new reading.
monitorIncidentMessages :: Text -> Monitors.QueryMonitor -> Double -> Monitors.MonitorStatus -> UTCTime -> Maybe Incidents.Episode -> Text -> Text -> Maybe Text -> (Incidents.SlackPayload, Incidents.SlackPayload)
monitorIncidentMessages project monitor value status observedAt episode issueUrl monitorUrl chart =
  ( card
      (statusCard st monitor.alertConfig.title issueUrl)
        { facts = measurement
        , chart = Just ("Recorded values for " <> incidentHeadline monitor.alertConfig.title, chart)
        , footer = ["Started " <> atUtc started, "checked every " <> show monitor.checkIntervalMins <> " min", "Project " <> slackEscape project]
        , actions = [("Open issue", issueUrl), (if isJust monitor.widgetId && isJust monitor.dashboardId then "Open widget" else "Open monitor", monitorUrl)]
        }
  , reply st monitor.alertConfig.title measurement
  )
  where
    st = case status of Monitors.MSNormal -> sRecovered; Monitors.MSWarning -> sWarning; Monitors.MSAlerting -> sAlerting
    threshold = case status of
      Monitors.MSWarning -> fromMaybe monitor.alertThreshold monitor.warningThreshold
      Monitors.MSAlerting -> monitor.alertThreshold
      Monitors.MSNormal
        | monitor.currentStatus == Monitors.MSWarning -> fromMaybe monitor.alertThreshold (monitor.warningRecoveryThreshold <|> monitor.warningThreshold)
        | otherwise -> fromMaybe monitor.alertThreshold monitor.alertRecoveryThreshold
    started = maybe observedAt (.startedAt) episode
    -- The side of the threshold the reading is on, in every state.
    comparison = if (status == Monitors.MSNormal) /= monitor.triggerLessThan then "below " else "above "
    measurement =
      ["*" <> monitorReading monitor value <> "*", comparison <> monitorReading monitor threshold]
        <> ["lasted " <> lasted (diffUTCTime observedAt started) | status == Monitors.MSNormal]


monitorReading :: Monitors.QueryMonitor -> Double -> Text
monitorReading monitor v = reading v <> foldMap ((" " <>) . slackEscape) monitor.alertConfig.unit


-- | A missing evaluation preserves the last verified reading, never a synthetic zero.
monitorDataUnavailableMessage :: Monitors.QueryMonitor -> Monitors.MeasurementFailure -> UTCTime -> Maybe (UTCTime, Double) -> Text -> Text -> (Incidents.SlackPayload, Incidents.SlackPayload)
monitorDataUnavailableMessage monitor reason now previous incidentUrl _ =
  (card (statusCard sNoData monitor.alertConfig.title incidentUrl){note = [detail]}, reply sNoData monitor.alertConfig.title [explanation, lastValue])
  where
    explanation = case reason of
      Monitors.NoMeasurements -> "no measurements in the evaluation window"
      Monitors.NonFiniteMeasurements -> "the evaluation returned a non-finite value"
      Monitors.EvaluationFailed -> "the evaluation failed"
    lastValue = maybe "no earlier reading" (\(time, value) -> "last value " <> monitorReading monitor value <> " at " <> hhmm time) previous
    detail = T.toUpper (T.take 1 explanation) <> T.drop 1 explanation <> " at " <> hhmm now <> " · recovery is unconfirmed"


-- | An error episode as a root card and a one-line reply. A reminder replies
-- "still occurring"; anything else replies with the status that changed.
errorIncidentMessages :: RuntimeAlertType -> ErrorPatterns.ATError -> UTCTime -> Maybe Incidents.Episode -> Maybe Text -> Text -> Text -> Maybe Text -> Maybe Text -> [Text] -> AlertImpact -> (Incidents.SlackPayload, Incidents.SlackPayload)
errorIncidentMessages alertType err now episode ongoing projectUrl incidentUrl chart occurrence change impact =
  ( card $ errorCard (runtimeStatus alertType) err projectUrl incidentUrl chart occurrence footer
  , reply (if isJust ongoing then sStillFailing else runtimeStatus alertType) (errorAlertTitle err) (foldMap (\r -> ["*" <> r <> "*"]) occurrence <> change <> foldMap (\d -> ["ongoing " <> d]) ongoing)
  )
  where
    -- Who it hit and for which customer; identity is opt-in per project.
    user = (\n -> n <> foldMap (\e -> " (" <> e <> ")") impact.userEmail) <$> impact.userName <|> impact.userEmail
    footer = ["Started " <> atUtc (maybe now (.startedAt) episode)] <> foldMap (\u -> ["User " <> slackEscape u]) user <> foldMap (\t -> ["Tenant " <> slackEscape t]) impact.tenant <> ["Project " <> slackEscape impact.projectName]


tagged :: Text -> AE.Value -> AE.Value
tagged identifier (AE.Object block) = AE.Object $ KEM.insert "block_id" (AE.String identifier) block
tagged _ block = block


atUtc :: UTCTime -> Text
atUtc = fmtDate "%b %-e, %H:%M UTC"


hhmm :: UTCTime -> Text
hhmm = fmtDate "%H:%M UTC"


-- | Titles reach Slack escaped and bounded, whatever the source produced.
incidentHeadline :: Text -> Text
incidentHeadline = slackEscape . T.take 160


-- | The root keeps its identity and says who resolved it and when; the reply says who.
resolvedErrorMessage :: ErrorPatterns.ATError -> Text -> UTCTime -> Text -> Maybe Text -> (Incidents.SlackPayload, Incidents.SlackPayload)
resolvedErrorMessage err actor now projectUrl issueUrl =
  (card (errorCard sResolved err projectUrl (fromMaybe (projectUrl <> "/issues") issueUrl) Nothing Nothing []){note = ["by " <> bold actor, hhmm now], body = [], chart = Nothing}, reply sResolved{label = "Resolved by " <> slackEscape actor} (errorAlertTitle err) [])


-- | A lifecycle note in an issue's thread: who did what, in one line. A person
-- is the subject; the system speaks only when nobody acted.
activityReply :: Issues.ActivityNote -> Incidents.SlackPayload
activityReply note = oneLine (fromMaybe "issue" note.issue) line
  where
    who = maybe "Someone" bold note.actor
    meta k = note.metadata >>= (^? key k . _String)
    line = case note.event of
      Issues.IEAcknowledged -> ":eyes: " <> who <> " acknowledged this  ·  " <> maybe "alerts paused" pausedUntil (meta "until" >>= iso8601ParseM . toString)
      Issues.IEUnacknowledged -> ":bell: " <> who <> " unacknowledged this  ·  alerts resumed"
      Issues.IEAckExpired -> ":bell: Acknowledgement expired  ·  alerts resumed"
      Issues.IEArchived -> ":file_cabinet: " <> who <> " archived this"
      Issues.IEUnarchived -> ":inbox_tray: " <> who <> " moved this back to the inbox"
      Issues.IEResolved -> statusText sResolved{label = "Resolved by " <> maybe "someone" slackEscape note.actor}
      Issues.IEAutoResolved -> statusText sResolved <> " automatically"
      Issues.IEReopened -> ":leftwards_arrow_with_hook: *Reopened*" <> foldMap (const $ " by " <> who) note.actor
      Issues.IEAssigned -> ":bust_in_silhouette: " <> who <> bool (" assigned this to " <> maybe "a teammate" bold note.assignee) " took this" (isJust note.actor && note.assignee == note.actor)
      Issues.IEUnassigned -> ":bust_in_silhouette: " <> who <> " unassigned this"
      Issues.IECommented -> ":speech_balloon: " <> who <> "\n" <> foldMap (T.intercalate "\n" . map ("> " <>) . lines . slackEscape . T.take 1500) (meta "body")
      Issues.IELinked -> ":link: " <> who <> " linked " <> maybe "an external issue" (\u -> "<" <> u <> "|" <> slackEscape (fromMaybe u $ meta "title") <> ">") (meta "url")
      Issues.IEMerged -> ":twisted_rightwards_arrows: " <> who <> " merged this into another issue"
      Issues.IEUnmerged -> ":arrow_heading_up: " <> who <> " unmerged this"
      Issues.IESpam -> ":no_entry_sign: " <> who <> " marked this as spam"
      Issues.IENotSpam -> ":leftwards_arrow_with_hook: " <> who <> " marked this as not spam"
      (Issues.IECreated; Issues.IERegressed; Issues.IEEscalated; Issues.IEViewed) -> ":information_source: Issue updated"
    pausedUntil t
      | t >= Issues.indefiniteUntil = "alerts paused until it regresses"
      | utctDay t == utctDay note.at = "alerts paused until " <> hhmm t
      | otherwise = "alerts paused until " <> atUtc t


-- | A root edit replaces the blocks it carries and keeps the rest of the first
-- message, except the chart: the root always shows how the incident began.
retainSlackSnapshot :: Incidents.SlackPayload -> Incidents.SlackPayload -> Incidents.SlackPayload
retainSlackSnapshot initial current@(Incidents.SlackPayload cur) = Incidents.SlackPayload $ cur & rootBlocks .~ V.fromList merged
  where
    blocksOf (Incidents.SlackPayload o) = maybe (o ^.. ix "blocks" . _Array . traverse) toList (o ^? rootBlocks)
    byId blocks i = listToMaybe [b | b <- blocksOf blocks, blockId b == Just i]
    pick i = if i == "chart" then byId initial i <|> byId current i else byId current i <|> byId initial i
    untagged = filter (isNothing . blockId) (blocksOf current)
    merged = mapMaybe pick ["status", "note", "facts", "evidence"] <> untagged <> mapMaybe pick ["chart", "footer", "actions"]


-- | A card's blocks, inside the attachment that carries its colour bar.
rootBlocks :: Traversal' (KEM.KeyMap AE.Value) (V.Vector AE.Value)
rootBlocks = ix "attachments" . nth 0 . key "blocks" . _Array


blockId :: AE.Value -> Maybe Text
blockId = (^? key "block_id" . _String)


-- | An uptime check confirmed down, and the reply when it is still down.
uptimeMessages :: Issues.UptimeData -> Text -> (Incidents.SlackPayload, Incidents.SlackPayload)
uptimeMessages d issueUrl =
  cardMessages
    (statusCard (Status ":red_circle:" "Down" Critical) (uptimeName d) issueUrl)
      { facts = [bold $ maybe "No response" (("HTTP " <>) . show) d.statusCode, "expected " <> show d.expectedStatus, show d.durationMs <> " ms"]
      , -- A status code is already in the facts; only a failed request needs its reason.
        body = [tagged "evidence" $ slackSection $ "`" <> slackEscape d.url <> "`" <> foldMap (const $ "\n" <> slackEscape d.reason) (guard (isNothing d.statusCode))]
      , footer = ["Down since " <> atUtc d.downSince]
      , actions = [("Open issue", issueUrl)]
      }


-- | A cron monitor that missed its window or reported a failed run.
cronMessages :: Issues.CronData -> Text -> (Incidents.SlackPayload, Incidents.SlackPayload)
cronMessages d issueUrl =
  cardMessages
    (statusCard (Status ":red_circle:" (case d.failure of Issues.CFMissed -> "Missed run"; Issues.CFFailed -> "Run failed") Critical) (cronName d) issueUrl)
      { facts = ["`" <> slackEscape d.slug <> "`", "expected by " <> atUtc d.expectedBy, maybe "no check-in yet" (("last check-in " <>) . atUtc) d.lastCheckinAt]
      , actions = [("Open issue", issueUrl)]
      }


-- | What a check is called: its name, or what it watches when it has none.
uptimeName :: Issues.UptimeData -> Text
uptimeName d = d.name `orIfBlank` d.url


cronName :: Issues.CronData -> Text
cronName d = d.name `orIfBlank` d.slug


orIfBlank :: Text -> Text -> Text
orIfBlank name fallback = if T.null (T.strip name) then fallback else name


-- | A failing check that passed again: the root turns green, and its facts say
-- how long it was down instead of the failing reading.
recoveredMessages :: Text -> Text -> NominalDiffTime -> (Incidents.SlackPayload, Incidents.SlackPayload)
recoveredMessages title issueUrl down = cardMessages (statusCard sRecovered title issueUrl){facts = ["back after " <> lasted down]}


slackReportAlert :: Projects.ReportType -> Text -> Text -> Int -> Int -> V.Vector (Text, Int, Int) -> Text -> Text -> Maybe Text -> Maybe Text -> Incidents.SlackPayload
slackReportAlert reportType startTime endTime totalErrors totalEvents breakDown project url allUrl errUrl =
  card
    (statusCard (Status ":white_circle:" (T.toTitle (display reportType) <> " report") Neutral) project url)
      { facts = ["*" <> prettyPrintCount totalEvents <> "* events", "*" <> prettyPrintCount totalErrors <> "* errors", day startTime <> " – " <> day endTime]
      , body = catMaybes [slackImage "Events over the period" (Just "Events") <$> allUrl, slackImage "Errors over the period" (Just "Errors") <$> errUrl]
      , footer = take 5 (V.toList breakDown) <&> \(name, errCount, evCount) -> bold name <> " " <> prettyPrintCount evCount <> " events, " <> prettyPrintCount errCount <> " errors"
      , actions = [("Open report", url)]
      }
  where
    day t = maybe (T.take 10 t) (fmtDate "%b %-e") (iso8601ParseM @Maybe @UTCTime (toString t))


slackErrorAlert :: RuntimeAlertType -> ErrorPatterns.ATError -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Incidents.SlackPayload
slackErrorAlert alertType err project projectUrl targetUrl chartUrlM occTextM firstSeenM ongoingForM =
  card $ errorCard status err projectUrl targetUrl chartUrlM occTextM ["First seen " <> fromMaybe (errFirstSeen err) firstSeenM, "Project " <> slackEscape project]
  where
    status = maybe (runtimeStatus alertType) (\d -> Status sStillFailing.emoji ("Still occurring for " <> d) Critical) ongoingForM


slackMonitorAlert :: Status -> Text -> Text -> Maybe Text -> Incidents.SlackPayload
slackMonitorAlert status monitorTitle monitorUrl chartUrlM =
  card (statusCard status monitorTitle monitorUrl){chart = Just ("Monitor values", chartUrlM) <* guard (status /= sRecovered), actions = [("Open monitor", monitorUrl)]}


slackNewEndpointsAlert :: Text -> V.Vector EndpointAlertRow -> Text -> Text -> Incidents.SlackPayload
slackNewEndpointsAlert projectName endpoints targetUrl projectUrl =
  card
    (statusCard (Status ":large_blue_circle:" (countNoun (V.length endpoints) "new endpoint") Calm) title targetUrl)
      { facts = case groups of [((host, ctx), _)] -> catMaybes [bold <$> host, ctx]; _ -> []
      , body = [tagged "evidence" $ slackSection $ T.intercalate "\n\n" $ groups <&> \((host, ctx), labels) -> foldMap (\h -> bold h <> foldMap ("  ·  " <>) ctx <> "\n") (host <* guard (length groups > 1)) <> bulletList labels]
      , footer = ["Project " <> slackEscape projectName]
      , actions = [("Open issue", targetUrl), ("View in Explorer", newEndpointsExplorerUrl projectUrl ((.label) <$> endpoints))]
      }
  where
    groups = groupedByContext endpoints
    title = maybe projectName (\e -> e.label <> bool "" (" +" <> show (V.length endpoints - 1)) (V.length endpoints > 1)) (endpoints V.!? 0)


logPatternCard :: NewLogPattern -> Text -> Card
logPatternCard NewLogPattern{..} project =
  (statusCard status (stripSummaryBadges patternText) issueUrl)
    { facts = catMaybes [bold <$> serviceName, T.toUpper . slackEscape <$> logLevel, Just $ "matched " <> countNoun occurrenceCount "time"]
    , body = [tagged "evidence" $ slackSection $ snippet 400 $ fromMaybe patternText sampleMessage]
    , footer = ["Field `" <> slackEscape sourceField <> "`" | sourceField /= "body"] <> ["Project " <> slackEscape project]
    , actions = [("Open issue", issueUrl)]
    }
  where
    status
      | isError = Status ":red_circle:" "New error log pattern" Critical
      | any ((`elem` (["warn", "warning"] :: [Text])) . T.toLower) logLevel = Status ":large_yellow_circle:" "New warning log pattern" Elevated
      | otherwise = Status ":large_blue_circle:" "New log pattern" Calm


rateChangeCard :: RateChange -> Text -> Card
rateChangeCard RateChange{..} project =
  (statusCard status (stripSummaryBadges patternText) issueUrl)
    { facts = catMaybes [bold <$> serviceName, T.toUpper . slackEscape <$> logLevel] <> ["*" <> perHour currentRate <> "*", "baseline " <> perHour baselineMean, "*" <> bool "−" "+" spike <> formatWithCommas (fromIntegral (round (abs changePercent) :: Int)) <> "%*"]
    , chart = Just ("Hourly volume against its baseline", chartUrl)
    , footer = ["Project " <> slackEscape project]
    , actions = [("Open issue", issueUrl)]
    }
  where
    spike = direction == Spike
    status
      | isError = Status ":red_circle:" ("Error log " <> bool "drop" "spike" spike) Critical
      | spike = Status ":large_orange_circle:" "Log spike" Elevated
      | otherwise = Status ":large_blue_circle:" "Log drop" Calm
    perHour r = prettyPrintCount (round r) <> "/hr"


slackEscape :: Text -> Text
slackEscape = T.replace ">" "&gt;" . T.replace "<" "&lt;" . T.replace "&" "&amp;"


-- | Raw exception messages are evidence, not the notification's hierarchy.
-- Collapse arbitrary whitespace and fence them so a multi-kilobyte HTTP request
-- or database plan cannot dominate Slack's alert view or escape its code block.
errorSnippet :: Int -> ErrorPatterns.ATError -> Text
errorSnippet limit err = "```" <> bounded <> "```"
  where
    message = unwords $ words $ T.replace "```" "'''" err.message
    withFrame = maybe message ((message <>) . ("\n" <>)) $ topStackFrame err.stackTrace
    bounded
      | T.length withFrame <= limit = withFrame
      | otherwise = T.take (max 0 $ limit - 1) withFrame <> "…"


-- | Pull the human-useful route out of the noisy @wreq@ exception rendering.
-- The full message remains below as evidence; this only makes the alert's
-- primary line answer "what failed?" at a glance.
errorAlertTitle :: ErrorPatterns.ATError -> Text
errorAlertTitle err = errorTitle err.errorType err.message


-- | "Type: first line of the message", or what a wrapped HTTP failure was calling.
--
-- >>> errorTitle "TypeError" "Cannot read 'id'\n  at x"
-- "TypeError: Cannot read 'id'"
-- >>> errorTitle "HttpException" "HttpExceptionRequest Request { host = \"api.x\" path = \"/a\" }"
-- "HTTP request failed \183 api.x/a"
errorTitle :: Text -> Text -> Text
errorTitle errorType raw
  | "HttpExceptionRequest" `T.isInfixOf` raw = "HTTP request failed" <> foldMap (" · " <>) endpoint
  | T.null message || message `T.isPrefixOf` errorType = errorType
  | otherwise = errorType <> ": " <> message
  where
    message = unwords $ words $ T.takeWhile (/= '\n') raw
    endpoint = quoted "host" <&> (<> fromMaybe "" (quoted "path"))
    quoted field = do
      let marker = field <> " = \""
      value <- T.stripPrefix marker $ snd $ T.breakOn marker raw
      guard $ not $ T.null value
      pure $ T.takeWhile (/= '\"') value


-- | Markdown bullet list shared by Slack + Discord new-endpoint renderers.
bulletList :: [Text] -> Text
bulletList = T.intercalate "\n" . fmap (\x -> "• `" <> x <> "`")


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
  projectUrl <> logExplorerLinks.logExplorerGet (Just expr) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  where
    paths = (\x -> "\"" <> T.drop 1 (T.dropWhile (/= ' ') x) <> "\"") <$> V.toList endpoints
    clause field = case paths of
      [p] -> field <> " == " <> p
      ps -> field <> " in (" <> T.intercalate "," ps <> ")"
    expr = "(" <> clause "attributes.http.route" <> " OR " <> clause "attributes.url.path" <> ")"


discordReportAlert :: Projects.ReportType -> Text -> Text -> Int -> Int -> V.Vector (Text, Int, Int) -> Text -> Text -> Maybe Text -> Maybe Text -> AE.Value
discordReportAlert reportType startTime endTime totalErrors totalEvents breakDown project url allUrl errUrl =
  AE.object
    [ "flags" AE..= 32768
    , "components"
        AE..= arr
          ( [ text ("## 📊 " <> T.toTitle (display reportType) <> " Report for " <> project)
            , text ("**From:** " <> T.take 10 startTime <> "  **To:** " <> T.take 10 endTime)
            , text ("Total Events: **" <> show totalEvents <> "**" <> T.replicate 28 "  " <> " Total Errors: **" <> show totalErrors <> "**")
            ]
              <> dcGallery [(allUrl, "Total events"), (errUrl, "Total errors")]
              <> [ text servicesStat
                 , AE.object ["type" AE..= 1, "components" AE..= arr [AE.object ["type" AE..= 2, "label" AE..= "Open report", "url" AE..= url, "style" AE..= 5]]]
                 ]
          )
    ]
  where
    text t = AE.object ["type" AE..= 10, "content" AE..= (t :: Text)]
    servicesStat =
      T.intercalate "\n" $ take 10 $ V.toList breakDown <&> \(name, errCount, evCount) -> "* **" <> name <> "**: Total errors-" <> show errCount <> ", Total events-" <> show evCount


discordErrorAlert :: RuntimeAlertType -> ErrorPatterns.ATError -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AE.Value
discordErrorAlert alertType err project projectUrl url chartUrlM occTextM firstSeenM ongoingForM =
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
discordNewEndpointAlert projectName endpoints url projectUrl =
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
    title = "🔵 " <> countNoun n "new endpoint" <> " · " <> projectName
    content = if n == 1 then "🔵 **New endpoint detected**" else "🔵 **" <> show n <> " new endpoints detected**"
    explorerLink = "[View in Explorer](" <> newEndpointsExplorerUrl projectUrl ((.label) <$> endpoints) <> ")"
    -- Group header: bold globe-prefixed host then dimmed "service · env" caption, each
    -- on its own line, then the bullets.
    description =
      T.intercalate "\n\n" $ groupedByContext endpoints <&> \((hostM, ctxM), labels) ->
        foldMap (\h -> "🌐 **" <> h <> "**\n") hostM <> foldMap (\c -> "_" <> c <> "_\n") ctxM <> bulletList labels


mkDiscordLogPatternPayload :: NewLogPattern -> Text -> AE.Value
mkDiscordLogPatternPayload NewLogPattern{..} project =
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
    -- @isError@ is authoritative; @logLevel@ only gates "warning", since drain-derived rows don't set it.
    (emoji, label, color :: Int)
      | isError = ("🚨", "New error log pattern", 15278902)
      | any ((`elem` (["warn", "warning"] :: [Text])) . T.toLower) logLevel = ("⚠️", "New warning log pattern", 15909152)
      | otherwise = ("🔍", "New log pattern", 3901174)


mkDiscordLogPatternRateChangePayload :: RateChange -> Text -> AE.Value
mkDiscordLogPatternRateChangePayload RateChange{..} project =
  discordEmbedMsg (icon <> " **" <> label <> "**")
    $ AE.object
      [ "title" AE..= (icon <> " " <> label <> " · " <> project)
      , "description" AE..= snippet 200 patternText
      , "color" AE..= color
      , "fields" AE..= arr (uncurry (discordField True) <$> rows)
      , "url" AE..= issueUrl
      ]
  where
    (icon, color :: Int)
      | isError = ("🚨", 15278902)
      | direction == Spike = ("📈", 15381768)
      | otherwise = ("📉", 3901174)
    label = (if isError then "Error " else "") <> "Log volume " <> display direction
    perHour r = show (round r :: Int) <> "/hr"
    rows =
      [ ("Now", perHour currentRate)
      , ("Baseline", perHour baselineMean)
      , ("Change", (if direction == Spike then "+" else "-") <> show (round changePercent :: Int) <> "%")
      , ("Level", fromMaybe "—" logLevel)
      , ("Service", fromMaybe "—" serviceName)
      ]


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
sendPagerdutyAlertToService integrationKey (LogPatternAlert NewLogPattern{issueUrl, patternText, logLevel, serviceName, isError}) projectTitle _ =
  let pat = stripSummaryBadges patternText
      kind = if isError then "Error Log Pattern" else "Log Pattern" :: Text
      sev = if isError || logLevel == Just "error" then Notify.PDCritical else Notify.PDWarning
   in Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-logpattern-" <> T.take 40 pat) (projectTitle <> ": New " <> kind <> " - " <> T.take 80 pat) sev (AE.object ["pattern" AE..= pat, "service" AE..= serviceName, "level" AE..= logLevel, "is_error" AE..= isError]) issueUrl
sendPagerdutyAlertToService integrationKey (LogPatternRateChangeAlert RateChange{issueUrl, patternText, logLevel, serviceName, direction, currentRate, baselineMean, changePercent, isError}) projectTitle _ =
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
  QueryAlert -> testMonitor "High Error Rate"
  LogPattern -> const $ LogPatternAlert NewLogPattern{issueUrl = "https://example.com/test", patternText = "🧪 TEST: payment provider timeout after <*> ms", sampleMessage = Just "payment provider timeout after 30000 ms (provider=adyen)", logLevel = Just "error", serviceName = Just "payments", sourceField = "body", occurrenceCount = 214, isError = True}
  LogPatternRateChange -> const $ LogPatternRateChangeAlert RateChange{issueUrl = "https://example.com/test", patternText = "🧪 TEST: connection reset by peer <*>", logLevel = Just "warn", serviceName = Just "gateway", direction = Spike, currentRate = 1920, baselineMean = 140, changePercent = 1271, isError = False, chartUrl = Nothing}
  Performance -> testMonitor "N+1 Query"
  Frontend -> testMonitor "Rage Click"
  Uptime -> testMonitor "Downtime detected"
  Cron -> testMonitor "Cron missed"
  Feedback -> testMonitor "User feedback"
  where
    testMonitor t = const $ MonitorsAlert ("🧪 TEST: " <> t) "https://example.com/test" Nothing


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


-- | The project title is supplied by the sender, not the alert, so this takes none.
sampleReport :: NotificationAlerts
sampleReport = ReportAlert Projects.RTWeekly "2025-01-01" "2025-01-02" 42 1250 (V.singleton ("api", 42, 1250)) "https://example.com" (Just "https://example.com/chart.png") (Just "https://example.com/errors.png")


data AlertMessages = AlertMessages {discordContent :: Text, discordColor :: Int}


runtimeAlertMessages :: RuntimeAlertType -> AlertMessages
runtimeAlertMessages = \case
  NewRuntimeError -> AlertMessages "**🔴 New error**" 15684432
  EscalatingErrors -> AlertMessages "**⚠️ Error rate escalating**" 16344086
  RegressedErrors -> AlertMessages "**↩️ Resolved error returned**" 3901174
  ErrorSpike -> AlertMessages "**📈 Error rate spiking**" 15381768


arr :: [AE.Value] -> AE.Value
arr = AE.Array . V.fromList


-- | Fenced code snippet, badge-stripped and length-capped, as used in every alert body.
snippet :: Int -> Text -> Text
snippet n t = "```" <> T.take n (stripSummaryBadges t) <> "```"


mrkdwn, plainTxt :: Text -> AE.Value
mrkdwn t = AE.object ["type" AE..= "mrkdwn", "text" AE..= t]
plainTxt t = AE.object ["type" AE..= "plain_text", "text" AE..= t, "emoji" AE..= True]


slackSection :: Text -> AE.Value
slackSection t = AE.object ["type" AE..= "section", "text" AE..= mrkdwn t]


slackHeader :: Text -> AE.Value
slackHeader t = AE.object ["type" AE..= "header", "text" AE..= plainTxt t]


slackContext :: [Text] -> AE.Value
slackContext ts = AE.object ["type" AE..= "context", "elements" AE..= arr (mrkdwn <$> ts)]


slackImage :: Text -> Maybe Text -> Text -> AE.Value
slackImage alt titleM url =
  AE.object
    $ ["type" AE..= "image", "image_url" AE..= url, "alt_text" AE..= alt]
    <> maybeToList (titleM <&> \t -> "title" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= t])


-- | Media gallery of the charts that rendered, one entry per (url, alt-text) pair. Discord
-- rejects an empty gallery, so with no chart there is no component at all.
--
-- >>> dcGallery [(Nothing, "events"), (Nothing, "errors")]
-- []
-- >>> dcGallery [(Just "https://x/a.png", "events"), (Nothing, "errors")]
-- [Object (fromList [("items",Array [Object (fromList [("description",String "events"),("media",Object (fromList [("url",String "https://x/a.png")]))])]),("type",Number 12.0)])]
dcGallery :: [(Maybe Text, Text)] -> [AE.Value]
dcGallery items = [AE.object ["type" AE..= (12 :: Int), "items" AE..= arr [AE.object ["media" AE..= AE.object ["url" AE..= u], "description" AE..= d] | (u, d) <- rendered]] | not (null rendered)]
  where
    rendered = [(u, d) | (Just u, d) <- items]


slackActions :: [AE.Value] -> AE.Value
slackActions bs = AE.object ["type" AE..= "actions", "elements" AE..= arr bs]


-- | Slack's button @style@ is an enum of exactly "primary" and "danger". A
-- "default" style is not "no style" — it makes Slack reject the ENTIRE message
-- with invalid_attachments, so every error alert carrying a trace id silently
-- failed. Omit the key instead: @styleM = Nothing@ is the unstyled button.
-- (This once looked like a chat.postMessage-only bug because the webhook
-- transport used to flatten buttons to mrkdwn links; both now send Block Kit.)
slackButton :: Text -> Maybe Text -> Text -> AE.Value
slackButton label styleM url =
  AE.object
    $ ["type" AE..= "button", "text" AE..= plainTxt label, "url" AE..= url]
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
