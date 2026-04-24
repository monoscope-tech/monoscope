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
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Integrations (SlackData (..), getProjectSlackData)
import Models.Apis.Issues (IssueType (..), parseIssueType)
import Models.Apis.LogQueries qualified as LogQueries
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (urlEncode)
import Pkg.EmailTemplates (EndpointAlertRow (..), groupedByContext)
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
  channelIdM <- case channelIdM' of
    Just c -> pure (Just c)
    Nothing -> do
      teamM <- ProjectMembers.getEveryoneTeam pid
      pure $ teamM >>= viaNonEmpty head . V.toList . (.discord_channels)
  case channelIdM of
    Nothing -> do
      Log.logAttention "Discord alert skipped: no channel configured" (AE.object ["project_id" AE..= pid])
      pure Nothing
    Just cid -> do
      let projectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
          mkPayload = \case
            RuntimeErrorAlert{..} -> Just $ discordErrorAlert runtimeAlertType errorData issueTitle pTitle projectUrl chartUrl occurrenceText firstSeenText ongoingFor
            EndpointAlert{..} -> Just $ discordNewEndpointAlert project endpoints endpointHash projectUrl
            ReportAlert{..} -> Just $ discordReportAlert reportType startTime endTime totalErrors totalEvents breakDown pTitle reportUrl allChartUrl errorChartUrl
            MonitorsAlert{..} -> Just $ discordMonitorAlert monitorTitle monitorUrl chartUrl
            MonitorsRecoveryAlert{..} -> Just $ AE.object ["text" AE..= ("✅ *Alert resolved for `" <> monitorTitle <> "`* \n<" <> monitorUrl <> "|View Monitor>")]
            LogPatternAlert{..} -> Just $ mkDiscordLogPatternPayload patternText issueUrl logLevel serviceName sourceField occurrenceCount sampleMessage pTitle
            LogPatternRateChangeAlert{..} -> Just $ mkDiscordLogPatternRateChangePayload patternText issueUrl logLevel serviceName direction currentRate baselineMean changePercent pTitle
            ShapeAlert -> Nothing
      case mkPayload alert of
        Nothing -> pure Nothing
        Just payload -> Notify.sendNotificationWithReply $ Notify.discordThreadedNotification cid payload replyToMsgIdM


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
            RuntimeErrorAlert{..} -> Just $ slackErrorAlert runtimeAlertType errorData issueTitle pTitle cid projectUrl chartUrl occurrenceText firstSeenText ongoingFor
            EndpointAlert{..} -> Just $ slackNewEndpointsAlert project endpoints cid endpointHash projectUrl
            ReportAlert{..} -> Just $ slackReportAlert reportType startTime endTime totalErrors totalEvents breakDown pTitle cid reportUrl allChartUrl errorChartUrl
            MonitorsAlert{..} -> Just $ slackMonitorAlert monitorTitle monitorUrl chartUrl cid
            MonitorsRecoveryAlert{..} -> Just $ slackAttachment cid "#22c55e" [AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("✅ *Resolved:* <" <> monitorUrl <> "|" <> monitorTitle <> ">")]]]
            LogPatternAlert{..} -> Just $ mkSlackLogPatternPayload patternText issueUrl logLevel serviceName sourceField occurrenceCount pTitle cid
            LogPatternRateChangeAlert{..} -> Just $ mkSlackLogPatternRateChangePayload patternText issueUrl logLevel serviceName direction currentRate baselineMean changePercent pTitle cid
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


sendWhatsAppAlert :: (Notify.Notify :> es, Reader Config.AuthContext :> es) => NotificationAlerts -> Projects.ProjectId -> Text -> V.Vector Text -> Eff es ()
sendWhatsAppAlert alert pid pTitle tos = do
  appCtx <- ask @Config.AuthContext
  case alert of
    RuntimeErrorAlert{..} -> do
      let template = appCtx.config.whatsappErrorTemplate
          url = pid.toText <> "/issues/by_hash/" <> errorData.hash
          contentVars = AE.object ["1" AE..= ("*" <> pTitle <> "*"), "2" AE..= ("*" <> issueTitle <> "*"), "3" AE..= ("`" <> errorData.message <> "`"), "4" AE..= url]
      sendAlert template contentVars
      pass
    EndpointAlert{..} -> do
      let template = appCtx.config.whatsappEndpointTemplate
          url = pid.toText <> "/issues/by_hash/" <> endpointHash
          labels = (.label) <$> V.toList endpoints
          contentVars = AE.object ["1" AE..= ("*" <> pTitle <> "*"), "2" AE..= T.intercalate "." ((\x -> "`" <> x <> "`") <$> labels), "3" AE..= url]
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
    MonitorsRecoveryAlert{} -> pass
    LogPatternAlert{} -> pass
    LogPatternRateChangeAlert{} -> pass
  where
    sendAlert :: Notify.Notify :> es => Text -> AE.Value -> Eff es ()
    sendAlert template vars =
      forM_ tos $ \to ->
        Notify.sendNotification $ Notify.whatsappNotification template to vars


slackReportAlert :: Text -> Text -> Text -> Int -> Int -> V.Vector (Text, Int, Int) -> Text -> Text -> Text -> Text -> Text -> AE.Value
slackReportAlert reportType startTime endTime totalErrors totalEvents breakDown project channelId url allUrl errUrl =
  slackAttachment
    channelId
    "#64748b"
    [ AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("<" <> url <> "|📊 *" <> T.toTitle reportType <> " report* · " <> project <> ">")]]
    , AE.object
        [ "type" AE..= "context"
        , "elements" AE..= AE.Array [AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*From:* " <> startTime <> "  *To:* " <> endTime <> "  *Events:* " <> toText (show totalEvents) <> "  *Errors:* " <> toText (show totalErrors))]]
        ]
    , AE.object ["type" AE..= "image", "image_url" AE..= allUrl, "alt_text" AE..= "Events", "title" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= ("Events: " <> toText (show totalEvents))]]
    , AE.object ["type" AE..= "image", "image_url" AE..= errUrl, "alt_text" AE..= "Errors", "title" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= ("Errors: " <> toText (show totalErrors))]]
    , AE.object ["type" AE..= "context", "elements" AE..= AE.Array sumr]
    , AE.object
        [ "type" AE..= "actions"
        , "elements" AE..= AE.Array [AE.object ["type" AE..= "button", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "Open report", "emoji" AE..= True], "url" AE..= url, "style" AE..= "primary"]]
        ]
    ]
  where
    sumr = V.take 10 $ V.map (\(name, errCount, evCount) -> AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*" <> name <> ":* " <> toText (show evCount) <> " events · " <> toText (show errCount) <> " errors")]) breakDown


slackErrorAlert :: RuntimeAlertType -> ErrorPatterns.ATError -> Text -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AE.Value
slackErrorAlert alertType err _issTitle project channelId projectUrl chartUrlM occTextM firstSeenM ongoingForM =
  slackAttachment channelId msgs.color
    $ [ AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= title]]
      , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= body]]
      ]
    <> [AE.object ["type" AE..= "context", "elements" AE..= AE.Array (V.fromList meta)] | not (null meta)]
    <> maybeToList (chartUrlM <&> \u -> AE.object ["type" AE..= "image", "image_url" AE..= u, "alt_text" AE..= "Error trend"])
    <> [AE.object ["type" AE..= "actions", "elements" AE..= AE.Array (V.fromList buttons)]]
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
    field lbl v = guard (not (T.null v)) $> AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*" <> lbl <> ":* " <> v)]
    route = case (fromMaybe "" err.requestMethod, fromMaybe "" err.requestPath) of
      ("", "") -> ""
      ("", p) -> "`" <> p <> "`"
      (m, p) -> "`" <> m <> " " <> p <> "`"
    firstSeen = fromMaybe (toText $ formatTime defaultTimeLocale "%b %-e · %-l:%M %p" err.when) firstSeenM
    meta =
      catMaybes
        [ field "Service" (fromMaybe "" err.serviceName)
        , field "Environment" (fromMaybe "" err.environment)
        , field "Route" route
        , occTextM >>= field "Rate"
        , Just $ AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*First seen:* " <> firstSeen)]
        , field "Runtime" (fromMaybe "" err.runtime)
        , field "Trace" (T.take 16 $ fromMaybe "" err.traceId)
        ]
    btn label style url = AE.object ["type" AE..= "button", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= (label :: Text), "emoji" AE..= True], "url" AE..= url, "style" AE..= (style :: Text)]
    -- Secondary "View trace" jumps into Log Explorer filtered by trace_id — complements
    -- "Investigate" (fingerprint view) with raw-event context.
    traceBtn = err.traceId >>= \tid -> guard (not (T.null tid)) $> btn "View trace" "default" (projectUrl <> "/log_explorer?query=" <> decodeUtf8 (urlEncode True $ encodeUtf8 ("trace_id == \"" <> tid <> "\"")))
    buttons = btn "🔍 Investigate" "primary" targetUrl : maybeToList traceBtn


-- | First non-empty line of a stack trace that isn't a repeat of the error-type header.
-- Gives alert readers the file/line where the exception was thrown — the single most
-- useful piece of context after the error message itself.
topStackFrame :: Text -> Maybe Text
topStackFrame stack = case filter (not . T.null) $ map T.strip $ T.lines stack of
  (_ : frame : _) -> Just $ T.take 160 frame -- skip first line (usually the error header)
  [only] -> Just $ T.take 160 only
  [] -> Nothing


slackMonitorAlert :: Text -> Text -> Maybe Text -> Text -> AE.Value
slackMonitorAlert monitorTitle monitorUrl chartUrlM channelId =
  slackAttachment channelId "#ef4444"
    $ [AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("🚨 *Monitor alerting:* <" <> monitorUrl <> "|" <> monitorTitle <> ">")]]]
    <> maybeToList (chartUrlM <&> \u -> AE.object ["type" AE..= "image", "image_url" AE..= u, "alt_text" AE..= "Monitor trend"])
    <> [AE.object ["type" AE..= "actions", "elements" AE..= AE.Array [AE.object ["type" AE..= "button", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "🔍 View monitor", "emoji" AE..= True], "url" AE..= monitorUrl, "style" AE..= "primary"]]]]


-- | Markdown bullet list shared by Slack + Discord new-endpoint renderers.
bulletList :: [Text] -> Text
bulletList = T.intercalate "\n" . fmap (\x -> "• `" <> x <> "`")


-- | Markdown group header: bold globe-prefixed host with optional dimmed
-- "service · env" caption, each on its own line. Empty when neither is set.
groupHeaderMd :: Maybe Text -> Maybe Text -> Text
groupHeaderMd hostM ctxM =
  foldMap (\h -> "🌐 **" <> h <> "**\n") hostM
    <> foldMap (\c -> "_" <> c <> "_\n") ctxM


slackNewEndpointsAlert :: Text -> V.Vector EndpointAlertRow -> Text -> Text -> Text -> AE.Value
slackNewEndpointsAlert projectName endpoints channelId hash projectUrl =
  slackAttachment channelId "#3b82f6" $ [headlineBlock] <> bodyBlocks <> [actionsBlock]
  where
    n = V.length endpoints
    headline = if n == 1 then "1 new endpoint" else show n <> " new endpoints"
    targetUrl = projectUrl <> "/issues/by_hash/" <> hash
    explorerUrl = newEndpointsExplorerUrl projectUrl ((.label) <$> endpoints)
    headlineBlock = AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("<" <> targetUrl <> "|:large_blue_circle: *" <> headline <> "* · " <> projectName <> ">")]]
    actionsBlock = AE.object ["type" AE..= "actions", "elements" AE..= AE.Array [AE.object ["type" AE..= "button", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "View in Explorer", "emoji" AE..= True], "style" AE..= "primary", "url" AE..= explorerUrl]]]
    section t = AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= t]]
    context t = AE.object ["type" AE..= "context", "elements" AE..= AE.Array [AE.object ["type" AE..= "mrkdwn", "text" AE..= t]]]
    -- Option A layout: host is the primary group header (bold, with globe), service · env
    -- drops to a dimmed context caption on the same line. Bullets below.
    groupHeader hostM ctxM = case (hostM, ctxM) of
      (Just h, Just c) -> Just $ ":globe_with_meridians: *" <> h <> "*  ·  " <> c
      (Just h, Nothing) -> Just $ ":globe_with_meridians: *" <> h <> "*"
      (Nothing, Just c) -> Just $ "_" <> c <> "_"
      (Nothing, Nothing) -> Nothing
    -- Single group with a header → separate context block + flat bullets (nicer Slack styling).
    -- Multiple groups → one section per group with inline header + bullets.
    bodyBlocks = case groupedByContext endpoints of
      [((hostM, ctxM), labels)]
        | Just h <- groupHeader hostM ctxM -> [context h, section (bulletList labels)]
        | otherwise -> [section (bulletList labels)]
      groups -> groups <&> \((hostM, ctxM), labels) -> section (maybe "" (<> "\n") (groupHeader hostM ctxM) <> bulletList labels)


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


mkSlackLogPatternPayload :: Text -> Text -> Maybe Text -> Maybe Text -> Text -> Int -> Text -> Text -> AE.Value
mkSlackLogPatternPayload patternText issueUrl logLevel serviceName sourceField occurrenceCount project channelId =
  slackAttachment
    channelId
    "#3b82f6"
    [ AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("<" <> issueUrl <> "|:mag: *New log pattern* · " <> project <> ">")]]
    , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("```" <> T.take 200 patternText <> "```")]]
    , AE.object
        [ "type" AE..= "context"
        , "elements"
            AE..= AE.Array
              [ AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Level:* " <> fromMaybe "—" logLevel)]
              , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Service:* " <> fromMaybe "—" serviceName)]
              , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Source:* " <> sourceField)]
              , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Matched:* " <> show occurrenceCount)]
              ]
        ]
    , AE.object
        [ "type" AE..= "actions"
        , "elements" AE..= AE.Array [AE.object ["type" AE..= "button", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "🔍 Investigate", "emoji" AE..= True], "url" AE..= issueUrl, "style" AE..= "primary"]]
        ]
    ]


mkSlackLogPatternRateChangePayload :: Text -> Text -> Maybe Text -> Maybe Text -> Text -> Double -> Double -> Double -> Text -> Text -> AE.Value
mkSlackLogPatternRateChangePayload patternText issueUrl logLevel serviceName direction currentRate baselineMean changePercent project channelId =
  slackAttachment
    channelId
    color
    [ AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("<" <> issueUrl <> "|" <> icon <> " *Log volume " <> direction <> "* · " <> project <> ">")]]
    , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("```" <> T.take 200 patternText <> "```")]]
    , AE.object
        [ "type" AE..= "context"
        , "elements"
            AE..= AE.Array
              [ AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Now:* " <> show (round currentRate :: Int) <> "/hr")]
              , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Baseline:* " <> show (round baselineMean :: Int) <> "/hr")]
              , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Change:* " <> sign <> show (round changePercent :: Int) <> "%")]
              , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Level:* " <> fromMaybe "—" logLevel)]
              , AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Service:* " <> fromMaybe "—" serviceName)]
              ]
        ]
    , AE.object
        [ "type" AE..= "actions"
        , "elements" AE..= AE.Array [AE.object ["type" AE..= "button", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "🔍 Investigate", "emoji" AE..= True], "url" AE..= issueUrl, "style" AE..= "primary"]]
        ]
    ]
  where
    icon = if direction == "spike" then ":chart_with_upwards_trend:" else ":chart_with_downwards_trend:"
    sign = if direction == "spike" then "+" else "-"
    color = if direction == "spike" then "#eab308" else "#3b82f6"


discordReportAlert :: Text -> Text -> Text -> Int -> Int -> V.Vector (Text, Int, Int) -> Text -> Text -> Text -> Text -> AE.Value
discordReportAlert reportType startTime endTime totalErrors totalEvents breakDown project url allUrl errUrl =
  AE.object
    [ "flags" AE..= 32768
    , "components"
        AE..= AE.Array
          [ AE.object ["type" AE..= 10, "content" AE..= ("## 📊 " <> (if reportType == "weekly" then "Weekly" else "Daily") <> " Report for " <> project)]
          , AE.object ["type" AE..= 10, "content" AE..= ("**From:** " <> T.take 10 startTime <> "  **To:** " <> T.take 10 endTime)]
          , AE.object ["type" AE..= 10, "content" AE..= ("Total Events: **" <> show totalEvents <> "**" <> T.replicate 28 "  " <> " Total Errors: **" <> show totalErrors <> "**")]
          , AE.object
              [ "type" AE..= 12
              , "items"
                  AE..= AE.Array
                    [ AE.object ["media" AE..= AE.object ["url" AE..= allUrl, "description" AE..= "Total events"]]
                    , AE.object ["media" AE..= AE.object ["url" AE..= errUrl, "description" AE..= "Total errors"]]
                    ]
              ]
          , AE.object ["type" AE..= 10, "content" AE..= servicesStat]
          , AE.object
              [ "type" AE..= 1
              , "components" AE..= AE.Array [AE.object ["type" AE..= 2, "label" AE..= "Open report", "url" AE..= url, "style" AE..= 5]]
              ]
          ]
    ]
  where
    servicesStat =
      T.intercalate "\n" $ V.toList $ V.take 10 $ V.map (\(name, errCount, evCount) -> "* **" <> name <> "**: Total errors-" <> show errCount <> ", Total events-" <> show evCount) breakDown


discordErrorAlert :: RuntimeAlertType -> ErrorPatterns.ATError -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AE.Value
discordErrorAlert alertType err _issTitle project projectUrl chartUrlM occTextM firstSeenM ongoingForM =
  AE.object
    [ "embeds"
        AE..= AE.Array
          [ AE.object
              ( [ "title" AE..= (err.errorType <> " · " <> project)
                , "description" AE..= ("```" <> T.take 600 err.message <> maybe "" ("\n" <>) (topStackFrame err.stackTrace) <> "```")
                , "color" AE..= msgs.discordColor
                , "fields" AE..= AE.Array (V.fromList fields)
                , "url" AE..= url
                ]
                  <> maybeToList (chartUrlM <&> \u -> "image" AE..= AE.object ["url" AE..= u])
              )
          ]
    , "content" AE..= maybe msgs.discordContent (\d -> "⏳ **Still firing · " <> d <> "**") ongoingForM
    ]
  where
    msgs = runtimeAlertMessages alertType
    url = projectUrl <> "/issues/by_hash/" <> err.hash
    route = case (fromMaybe "" err.requestMethod, fromMaybe "" err.requestPath) of
      ("", "") -> ""
      ("", p) -> "`" <> p <> "`"
      (m, p) -> "`" <> m <> " " <> p <> "`"
    firstSeen = fromMaybe (toText $ formatTime defaultTimeLocale "%b %-e · %-l:%M %p" err.when) firstSeenM
    field n v = guard (not (T.null v)) $> AE.object ["name" AE..= (n :: Text), "value" AE..= v, "inline" AE..= True]
    fields =
      catMaybes
        [ field "Service" (fromMaybe "" err.serviceName)
        , field "Environment" (fromMaybe "" err.environment)
        , field "Route" route
        , occTextM >>= field "Rate"
        , Just $ AE.object ["name" AE..= ("First Seen" :: Text), "value" AE..= firstSeen, "inline" AE..= True]
        , field "Runtime" (fromMaybe "" err.runtime)
        , field "Trace" (T.take 16 $ fromMaybe "" err.traceId)
        ]


discordMonitorAlert :: Text -> Text -> Maybe Text -> AE.Value
discordMonitorAlert monitorTitle monitorUrl chartUrlM =
  AE.object
    [ "embeds"
        AE..= AE.Array
          [ AE.object
              ( [ "title" AE..= ("🚨 Monitor alerting: " <> monitorTitle)
                , "color" AE..= (15684432 :: Int)
                , "url" AE..= monitorUrl
                ]
                  <> maybeToList (chartUrlM <&> \u -> "image" AE..= AE.object ["url" AE..= u])
              )
          ]
    , "content" AE..= ("🚨 **Monitor alerting:** " <> monitorTitle)
    ]


discordNewEndpointAlert :: Text -> V.Vector EndpointAlertRow -> Text -> Text -> AE.Value
discordNewEndpointAlert projectName endpoints hash projectUrl =
  [aesonQQ|
  {
  "embeds": [
    {
      "type": "rich",
      "title": #{title},
      "description": #{description},
      "color": 3901174,
      "fields": [
        {"name": "\u200b", "value": #{explorerLink}, "inline": false}
      ],
      "url": #{url}
    }
  ],
  "content" : #{content}
}
  |]
  where
    n = V.length endpoints
    title = (if n == 1 then "🔵 1 new endpoint" else "🔵 " <> show n <> " new endpoints") <> " · " <> projectName
    content = if n == 1 then "🔵 **New endpoint detected**" else "🔵 **" <> show n <> " new endpoints detected**"
    url = projectUrl <> "/issues/by_hash/" <> hash
    explorerUrl = newEndpointsExplorerUrl projectUrl ((.label) <$> endpoints)
    explorerLink = "[View in Explorer](" <> explorerUrl <> ")"
    description =
      T.intercalate "\n\n" $ groupedByContext endpoints <&> \((hostM, ctxM), labels) ->
        groupHeaderMd hostM ctxM <> bulletList labels


mkDiscordLogPatternPayload :: Text -> Text -> Maybe Text -> Maybe Text -> Text -> Int -> Maybe Text -> Text -> AE.Value
mkDiscordLogPatternPayload patternText issueUrl logLevel serviceName sourceField occurrenceCount sampleMessage project =
  AE.object
    [ "embeds"
        AE..= AE.Array
          [ AE.object
              [ "title" AE..= ("🔍 New log pattern · " <> project)
              , "description" AE..= ("```" <> T.take 200 patternText <> "```")
              , "color" AE..= (3901174 :: Int)
              , "fields"
                  AE..= AE.Array
                    ( fromList
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
    , "content" AE..= "🔍 New Log Pattern"
    ]


mkDiscordLogPatternRateChangePayload :: Text -> Text -> Maybe Text -> Maybe Text -> Text -> Double -> Double -> Double -> Text -> AE.Value
mkDiscordLogPatternRateChangePayload patternText issueUrl logLevel serviceName direction currentRate baselineMean changePercent project =
  AE.object
    [ "embeds"
        AE..= AE.Array
          [ AE.object
              [ "title" AE..= (icon <> " Log volume " <> direction <> " · " <> project)
              , "description" AE..= ("```" <> T.take 200 patternText <> "```")
              , "color" AE..= color
              , "fields"
                  AE..= AE.Array
                    ( fromList
                        [ AE.object ["name" AE..= "Now", "value" AE..= (show (round currentRate :: Int) <> "/hr"), "inline" AE..= True]
                        , AE.object ["name" AE..= "Baseline", "value" AE..= (show (round baselineMean :: Int) <> "/hr"), "inline" AE..= True]
                        , AE.object ["name" AE..= "Change", "value" AE..= (sign <> show (round changePercent :: Int) <> "%"), "inline" AE..= True]
                        , AE.object ["name" AE..= "Level", "value" AE..= fromMaybe "—" logLevel, "inline" AE..= True]
                        , AE.object ["name" AE..= "Service", "value" AE..= fromMaybe "—" serviceName, "inline" AE..= True]
                        ]
                    )
              , "url" AE..= issueUrl
              ]
          ]
    , "content" AE..= (icon <> " **Log volume " <> direction <> "**")
    ]
  where
    color = if direction == "spike" then 15381768 :: Int else 3901174 -- amber on spike, blue on drop
    icon = if direction == "spike" then "📈" else "📉"
    sign = if direction == "spike" then "+" else "-"


sendPagerdutyAlertToService :: Notify.Notify :> es => Text -> NotificationAlerts -> Text -> Text -> Eff es ()
sendPagerdutyAlertToService integrationKey MonitorsAlert{monitorTitle, monitorUrl, chartUrl} projectTitle _ =
  Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-alert-" <> monitorTitle) (projectTitle <> ": " <> monitorTitle) Notify.PDCritical (AE.object $ ["url" AE..= monitorUrl] <> maybeToList (("chart_url" AE..=) <$> chartUrl)) monitorUrl
sendPagerdutyAlertToService integrationKey (MonitorsRecoveryAlert monitorTitle monitorUrl) projectTitle _ =
  Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDResolve ("monoscope-alert-" <> monitorTitle) (projectTitle <> ": Resolved - " <> monitorTitle) Notify.PDInfo (AE.object ["url" AE..= monitorUrl]) monitorUrl
sendPagerdutyAlertToService integrationKey (EndpointAlert project endpoints hash) projectTitle projectUrl =
  let endpointUrl = projectUrl <> "/issues/by_hash/" <> hash
      labels = (.label) <$> V.toList endpoints
      endpointNames = T.intercalate ", " labels
      rowPayload r = AE.object $ ("endpoint" AE..= r.label) : [k AE..= v | (k, Just v) <- [("host", r.host), ("service", r.service), ("environment", r.environment)]]
   in Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-endpoint-" <> hash) (projectTitle <> ": New Endpoints - " <> endpointNames) Notify.PDWarning (AE.object ["project" AE..= project, "endpoints" AE..= (rowPayload <$> endpoints)]) endpointUrl
sendPagerdutyAlertToService integrationKey RuntimeErrorAlert{issueId, issueTitle, errorData, chartUrl} projectTitle projectUrl =
  let errorUrl = projectUrl <> "/issues/by_hash/" <> errorData.hash
   in Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-error-" <> issueId) (projectTitle <> ": " <> errorData.errorType <> " - " <> issueTitle) Notify.PDError (AE.object $ ["error_type" AE..= errorData.errorType, "message" AE..= errorData.message] <> maybeToList (("chart_url" AE..=) <$> chartUrl)) errorUrl
sendPagerdutyAlertToService integrationKey LogPatternAlert{issueUrl, patternText, logLevel, serviceName} projectTitle _ =
  Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-logpattern-" <> T.take 40 patternText) (projectTitle <> ": New Log Pattern - " <> T.take 80 patternText) (maybe Notify.PDWarning (\l -> if l == "error" then Notify.PDCritical else Notify.PDWarning) logLevel) (AE.object ["pattern" AE..= patternText, "service" AE..= serviceName, "level" AE..= logLevel]) issueUrl
sendPagerdutyAlertToService integrationKey LogPatternRateChangeAlert{issueUrl, patternText, logLevel, serviceName, direction, currentRate, baselineMean, changePercent} projectTitle _ =
  Notify.sendNotification $ Notify.pagerdutyNotification integrationKey Notify.PDTrigger ("monoscope-logpattern-rate-" <> T.take 40 patternText) (projectTitle <> ": Log Pattern " <> T.toTitle direction <> " - " <> T.take 60 patternText <> " (" <> show (round changePercent :: Int) <> "%)") (if direction == "spike" then Notify.PDCritical else Notify.PDWarning) (AE.object ["pattern" AE..= patternText, "direction" AE..= direction, "current_rate" AE..= currentRate, "baseline_mean" AE..= baselineMean, "service" AE..= serviceName]) issueUrl
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
    , "attachments"
        AE..= AE.Array
          ( V.singleton
              $ AE.object
                [ "color" AE..= color
                , "fallback" AE..= ("Monoscope alert" :: Text)
                , "blocks" AE..= AE.Array (V.fromList blocks)
                ]
          )
    ]


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
