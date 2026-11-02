{-# LANGUAGE OrPatterns #-}

module Pages.Bots.Utils (BotEmoji (..), BotType (..), BotReply (..), botReplyPayload, BotResponse (..), Channel (..), authHeader, contentTypeHeader, dcContainer, dcText, dcGallery, dcLinkButton, processAIQuery, processAIQueryWithTools, processAIQueryWithMode, buildIssuePrompt, verifyWidgetSignature, freshWebhookTimestamp, QueryIntent (..), detectReportIntent, BotErrorType (..), formatBotError, botEmoji, getLoadingMessage, runBotQueryWithTools, slackResponse, withBotThread, withDashboardTemplate, parseInstallState, installedResponse) where

import Control.Lens ((.~), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _Number)
import Data.ByteArray qualified as BA
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Wreq (Options, header)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Labeled (Labeled)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Effectful.Time qualified as Time
import Lucid hiding (for_)
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogQueries qualified as LogQueries
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.SchemaCatalog qualified as SchemaCatalog
import Models.Projects.DashboardTemplates (dashboardTemplatesCompiled, loadDashboardFromVM)
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.BodyWrapper (BWConfig, OnboardingStep (NotifChannel), PageCtx (..), stepUrl)
import Pages.Charts.Charts qualified as Charts
import Pages.Components (EmptyStateCfg (..), emptyState_, navBar)
import Pkg.AI qualified as AI
import Pkg.CLIFormat qualified as CLIFormat
import Pkg.Components.TimePicker qualified as TP
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..), idFromText)
import Pkg.Mail (arr, dcGallery, plainTxt, slackActions, slackContext, slackHeader, slackImage, slackSection)
import Pkg.Parser (parseQueryToAST)
import Pkg.SchemaLearning.Catalog (FacetData (..), FacetSummary (..), FacetValue (..))
import Relude hiding (Reader, ask)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Server (ServerError, err503)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Log
import System.Tracing (Tracing)
import System.Types (DB, ProjectToolCtx)
import UnliftIO.Exception (onException, throwIO)
import Utils (faSprite_, formatUTC, hostPath, toUriStr)
import Web.HttpApiData (parseUrlPiece)


data BotType = Discord | Slack | WhatsApp
  deriving (Eq, Show)


-- | Status emoji for visual indicators (always paired with text for accessibility)
data BotEmoji = EmojiSuccess | EmojiWarning | EmojiError | EmojiChart | EmojiSearch | EmojiTable


botEmoji :: BotEmoji -> Text
botEmoji = \case
  EmojiSuccess -> "🟢"
  EmojiWarning -> "🟡"
  EmojiError -> "🔴"
  EmojiChart -> "📊"
  EmojiSearch -> "🔍"
  EmojiTable -> "📋"


data BotErrorType = QueryParseError Text | NoDataError | ServiceError
  deriving (Eq, Show)


formatBotError :: BotType -> BotErrorType -> AE.Value
formatBotError target err = case target of
  Discord -> AE.object ["content" AE..= msg]
  WhatsApp -> AE.object ["body" AE..= msg]
  Slack -> AE.object ["text" AE..= msg, "response_type" AE..= "in_channel", "replace_original" AE..= True, "delete_original" AE..= True]
  where
    msg = case err of
      QueryParseError snippet -> botEmoji EmojiWarning <> " Couldn't parse query\n`" <> T.take 50 snippet <> "`\nTry: 'show errors in last hour'"
      NoDataError -> botEmoji EmojiSearch <> " No data found for your query\nTry expanding the time range or adjusting filters."
      ServiceError -> botEmoji EmojiError <> " Something went wrong\nPlease try again in a moment."


getLoadingMessage :: QueryIntent -> Text
getLoadingMessage = \case
  ReportIntent _ -> botEmoji EmojiChart <> " Fetching your report..."
  GeneralQueryIntent -> botEmoji EmojiSearch <> " Analyzing your query..."


data BotResponse
  = BotLinked (PageCtx (Text, Maybe Projects.ProjectId))
  | NoTokenFound (PageCtx ())
  | DiscordError (PageCtx ())
  | NoContent (PageCtx ())


instance ToHtml BotResponse where
  toHtml = \case
    BotLinked (PageCtx bwconf (bot, pidM)) -> toHtml $ PageCtx bwconf $ installedSuccess bot pidM
    DiscordError (PageCtx bwconf ()) -> toHtml $ PageCtx bwconf $ failure "Discord bot installation failed" "Add Discord from the integrations page, then click \"Add to Discord\" there to try again."
    NoTokenFound (PageCtx bwconf ()) -> toHtml $ PageCtx bwconf $ failure "Token not found" "No Slack access token found. Reinstall the Monoscope Slack app to try again."
    NoContent (PageCtx bwconf ()) -> toHtml $ PageCtx bwconf ("" :: Html ())
    where
      failure title desc = navBar >> emptyState_ def{icon = Just "circle-exclamation"} title desc
  toHtmlRaw = toHtml


-- | @authHeader "Bot" tok@ (Discord) / @authHeader "Bearer" tok@ (Slack).
authHeader :: Text -> Text -> Options -> Options
authHeader scheme token = header "Authorization" .~ [encodeUtf8 $ scheme <> " " <> token]


contentTypeHeader :: Text -> Options -> Options
contentTypeHeader contentType = header "Content-Type" .~ [encodeUtf8 contentType]


linkButton :: Text -> Text -> Text -> AE.Value
linkButton actionId label url = AE.object ["type" AE..= ("button" :: Text), "action_id" AE..= actionId, "text" AE..= plainTxt label, "url" AE..= url]


-- | Slack in-channel response replacing the ephemeral loading message.
slackResponse :: [AE.Value] -> AE.Value
slackResponse blocks = AE.object ["blocks" AE..= blocks, "response_type" AE..= ("in_channel" :: Text), "replace_original" AE..= True, "delete_original" AE..= True]


-- Discord "components v2" building blocks (flag 32768 opts the message into the new layout).

dcContainer :: Int -> [AE.Value] -> AE.Value
dcContainer accent components = AE.object ["flags" AE..= (32768 :: Int), "components" AE..= arr [AE.object ["type" AE..= (17 :: Int), "accent_color" AE..= accent, "components" AE..= arr components]]]


dcText :: Text -> AE.Value
dcText content = AE.object ["type" AE..= (10 :: Int), "content" AE..= content]


dcLinkButton :: Text -> Text -> AE.Value
dcLinkButton label url = AE.object ["type" AE..= (1 :: Int), "components" AE..= arr [AE.object ["type" AE..= (2 :: Int), "label" AE..= label, "url" AE..= url, "style" AE..= (5 :: Int)]]]


installedSuccess :: Text -> Maybe Projects.ProjectId -> Html ()
installedSuccess botPlatform pidM = do
  navBar
  section_ [class_ "min-h-screen flex flex-col justify-center"] do
    div_ [class_ "max-w-4xl mx-auto max-md:px-2 px-4"] do
      div_ [class_ "bg-bgBase border border-strokeWeak rounded-3xl border-fillWeak overflow-hidden"] do
        div_ [class_ "bg-gradient-to-r from-fillSuccess-weak to-fillBrand-weak px-8 py-10 text-center"] do
          div_ [class_ "inline-flex items-center justify-center w-16 h-16 bg-bgRaised rounded-full mb-4 shadow-lg"] do
            faSprite_ "check" "regular" "h-8 w-8 text-iconSuccess"
          h1_ [class_ "text-3xl font-semibold text-textStrong mb-4"] "Installation Complete!"
          p_ [class_ "text-textBrand font-semibold max-w-2xl mx-auto"] $ toHtml $ "Monoscope Bot has been successfully added to your " <> botPlatform <> " server"
        div_ [class_ "px-8 py-12"] do
          div_ [class_ "text-center mb-12"] do
            h2_ [class_ "font-semibold text-textStrong mb-4"] "You're All Set! 🚀"
            p_ [class_ "text-textWeak text-sm mx-auto max-w-2xl"] $ toHtml $ "Start receiving real-time alerts and interact with your API data directly from " <> botPlatform <> ". Your team can now stay on top of API performance without leaving your chat."
            whenJust pidM \pid ->
              div_ [class_ "mt-6"] do
                a_ [class_ "btn btn-primary", href_ $ "/p/" <> pid.toText <> "/settings/integrations"] "Go back to Integrations"
          div_ [class_ "max-w-3xl mx-auto"] do
            h3_ [class_ "font-semibold text-textStrong mb-8 text-center"] "Available Commands"
            div_ [class_ "grid gap-4 md:grid-cols-2"] do
              commandCard_ "bg-gradient-to-br from-fillWeaker to-fillBrand-weak border-strokeBrand-weak" "bg-fillBrand-strong" "bg-fillBrand-weak text-textBrand" (span_ [class_ "text-white font-bold text-lg"] "?") "/monoscope" "AI Powered"
                $ "Ask questions about your API, get reports, and get instant insights with logs and charts delivered right to "
                <> botPlatform
                <> "."
              commandCard_
                "bg-gradient-to-br from-fillSuccess-weak to-fillWeaker border-strokeSuccess-weak"
                "bg-fillSuccess-strong"
                "bg-fillSuccess-weak text-textSuccess"
                (faSprite_ "bell" "regular" "h-6 w-6 text-white")
                "/here"
                "Alerts"
                "Set up this channel to receive automated error reports, weekly summaries, and daily performance alerts."
  where
    commandCard_ :: Text -> Text -> Text -> Html () -> Text -> Text -> Text -> Html ()
    commandCard_ cardCls accentCls cmdCls icon cmd badge desc =
      div_ [class_ $ "rounded-2xl p-6 border " <> cardCls] $ div_ [class_ "flex items-start space-x-4"] do
        div_ [class_ $ "shrink-0 inline-flex items-center justify-center w-12 h-12 rounded-xl " <> accentCls] icon
        div_ [class_ "flex-1"] do
          div_ [class_ "flex items-center space-x-2 mb-3"] do
            span_ [class_ $ "monospace px-3 py-1 rounded-lg font-semibold " <> cmdCls] $ toHtml cmd
            span_ [class_ $ "text-white text-xs px-2 py-1 rounded-full " <> accentCls] $ toHtml badge
          p_ [class_ "text-textStrong text-sm"] $ toHtml desc


data Channel = Channel
  { channelName :: Text
  , channelId :: Text
  , channelType :: Maybe Int
  }
  deriving (Generic, Show)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "channel", DAE.CamelToSnake]] Channel


processAIQuery :: ProjectToolCtx es => Maybe EnvConfig -> Bool -> AI.AgentAccess -> Projects.ProjectId -> Text -> Maybe (UUIDId "conversation") -> Text -> Text -> Eff es (Either Text AI.LLMResponse)
processAIQuery sourceConfig useTf = processAIQueryWithTools sourceConfig useTf Nothing


processAIQueryWithTools :: ProjectToolCtx es => Maybe EnvConfig -> Bool -> Maybe AI.ProjectTools -> AI.AgentAccess -> Projects.ProjectId -> Text -> Maybe (UUIDId "conversation") -> Text -> Text -> Eff es (Either Text AI.LLMResponse)
processAIQueryWithTools sourceConfig useTf projectTools = processAIQueryWithMode sourceConfig useTf projectTools AI.InteractiveReadOnly


processAIQueryWithMode :: ProjectToolCtx es => Maybe EnvConfig -> Bool -> Maybe AI.ProjectTools -> AI.InvocationMode -> AI.AgentAccess -> Projects.ProjectId -> Text -> Maybe (UUIDId "conversation") -> Text -> Text -> Eff es (Either Text AI.LLMResponse)
processAIQueryWithMode sourceConfig useTf projectTools invocationMode access pid userQuery conversationId model apiKey = do
  AI.requireAgentAccess access pid
  for_ conversationId $ AI.requireEvidenceAccess access pid
  now <- Time.currentTime
  conversation <- join <$> traverse (Issues.getConversation pid) conversationId
  issuePrompt <- case conversation >>= Issues.conversationIssueId of
    Just issueId -> do
      issue <- Issues.selectIssueById pid issueId >>= maybe (throwIO AI.AgentStopped) pure
      Just <$> buildIssuePrompt pid issue now
    Nothing -> pure Nothing
  facetSummaryM <- if isNothing issuePrompt then SchemaCatalog.getFacetSummary Nothing pid "otel_logs_and_spans" (addUTCTime (-86400) now) now else pure Nothing
  let defaults = AI.defaultAgenticConfig pid
      config =
        defaults
          { AI.facetContext = facetSummaryM
          , AI.systemPromptOverride = issuePrompt
          , AI.conversationType = (.conversationType) <$> conversation
          , AI.conversationId = conversationId
          , AI.useTimefusion = useTf
          , AI.access = access
          , AI.sourceConfig = sourceConfig
          , AI.invocationMode = invocationMode
          , AI.projectTools = projectTools
          , AI.maxIterations = case access of
              AI.SlackInvestigationAccess{} -> 12
              (AI.MemberAccess{}; AI.SlackAccess{}; AI.SlackCommandAccess{}; AI.ServiceAccess) -> defaults.maxIterations
          }
  result <- AI.runAgenticChatWithHistory config userQuery model apiKey >>= either (pure . Left) (AI.completeAgenticResponse config userQuery model apiKey)
  result <$ whenLeft_ result \err -> Log.logAttention "processAIQuery failed" $ AE.object ["error" AE..= err, "userQuery" AE..= userQuery, "projectId" AE..= pid.toText]


-- | Issue context shared by HTTP previews and durable agent execution.
buildIssuePrompt :: (DB es, IOE :> es, Labeled "timefusion" Hasql.Hasql :> es, Log :> es, Reader AuthContext :> es, Time.Time :> es, Tracing :> es) => Projects.ProjectId -> Issues.Issue -> UTCTime -> Eff es Text
buildIssuePrompt pid issue now = do
  useTf <- (.env.enableTimefusionReads) <$> ask @AuthContext
  errorM <- bool (pure Nothing) (ErrorPatterns.getErrorPatternByHash pid issue.endpointHash) (issue.issueType == Issues.RuntimeException)
  (traceDataM, spans) <- maybe (pure (Nothing, V.empty)) (fetchTrace useTf) errorM
  alertContextM <- case Issues.issuePayload issue of
    Just (Issues.QueryAlertP alertData) -> do
      let twoDaysAgo = addUTCTime (-172800) now
      monitorM <- runMaybeT do
        monitorId <- hoistMaybe $ UUID.fromText alertData.queryId
        MaybeT $ Monitors.queryMonitorById (Monitors.QueryMonitorId monitorId)
      metricsData <- Charts.queryProjectMetrics Nothing (Just Charts.DTMetric) pid (Just alertData.queryExpression) Nothing Nothing (Just $ show twoDaysAgo) (Just $ show now) Nothing Nothing []
      pure $ Just (alertData, monitorM, metricsData)
    (Nothing; Just Issues.ApiChangeP{}; Just Issues.RuntimeExceptionP{}; Just Issues.LogPatternP{}; Just Issues.LogPatternRateChangeP{}; Just Issues.PerformanceP{}; Just Issues.FrontendP{}; Just Issues.FeedbackP{}; Just Issues.UptimeP{}; Just Issues.CronP{}) -> pure Nothing
  facetSummaryM <- SchemaCatalog.getFacetSummary Nothing pid "otel_logs_and_spans" (addUTCTime (-86400) now) now
  pure
    $ unlines
      [ guide
      , ""
      , "--- FACET SUMMARY ---"
      , maybe "" formatFacetSummaryForAI facetSummaryM
      , ""
      , "--- ISSUE CONTEXT ---"
      , buildAIContext issue errorM traceDataM spans alertContextM
      ]
  where
    fetchTrace useTf err =
      fromMaybe (Nothing, V.empty) <$> runMaybeT do
        tId <- hoistMaybe err.recentTraceId
        (trData, spans) <- MaybeT $ Telemetry.getTraceDetails useTf pid tId (Just $ zonedTimeToUTC err.updatedAt) now
        pure (Just trData, V.fromList spans)
    buildAIContext iss errM trDataM spans alertContextM =
      unlines
        $ [ "## Issue Details"
          , "- **Title**: " <> iss.title
          , "- **Type**: " <> show iss.issueType
          , "- **Severity**: " <> display iss.severity
          , "- **Service**: " <> Issues.serviceLabel iss.service
          , "- **Recommended Action**: " <> iss.recommendedAction
          ]
        <> catMaybes
          [ alertContextM <&> \(alertData, monitorM, metricsData) -> formatCompleteAlertContext alertData monitorM metricsData
          , errM <&> \err ->
              unlines
                [ ""
                , "## Error Details"
                , "- **Error Type**: " <> err.errorType
                , "- **Message**: " <> err.message
                , "- **Stack Trace**:"
                , "```"
                , err.errorData.stackTrace
                , "```"
                , maybe "" ("- **Service Name**: " <>) err.errorData.serviceName
                , maybe "" ("- **Request Method**: " <>) err.errorData.requestMethod
                , maybe "" ("- **Request Path**: " <>) err.errorData.requestPath
                ]
          , trDataM <&> \tr ->
              unlines
                [ ""
                , "## Trace Context"
                , "- **Trace ID**: " <> tr.traceId
                , "- **Duration**: " <> show tr.traceDurationNs <> "ns"
                , "- **Span Count**: " <> show (V.length spans)
                ]
          , guard (not $ V.null spans)
              $> unlines
                [ ""
                , "## Span Breakdown"
                , unlines $ V.toList $ flip V.map (V.take 10 spans) $ \s ->
                    "- " <> fromMaybe "unknown" s.name <> " (" <> maybe "n/a" show s.duration <> "ns)"
                ]
          ]
    formatCompleteAlertContext alertData monitorM metricsData =
      unlines
        $ [ ""
          , "## Alert Configuration"
          , ""
          , "### Query & Thresholds"
          , "- **Alert Query (KQL)**: `" <> alertData.queryExpression <> "`"
          , "- **Alert Threshold**: " <> show alertData.thresholdValue <> " (trigger when " <> display alertData.thresholdType <> ")"
          , "- **Current Value**: " <> show alertData.actualValue
          , "- **Triggered At**: " <> formatUTC alertData.triggeredAt
          ]
        -- Every remaining section is read off the monitor row, so each empties out
        -- under its own heading when the monitor has been deleted.
        <> concat
          [ ["", heading] <> foldMap body monitorM
          | (heading, body) <-
              [ ("### Execution Schedule", \m -> ["- **Check Interval**: Every " <> show m.checkIntervalMins <> " minutes", "- **Sustained Duration Required**: " <> show m.thresholdSustainedForMins <> " minutes (threshold must be exceeded for this long)", "- **Last Evaluated**: " <> maybe "never" formatUTC m.lastEvaluated])
              ,
                ( "### Trigger Conditions"
                , \m ->
                    ("- **Trigger Direction**: " <> bool "Alert when value EXCEEDS threshold (>)" "Alert when value DROPS BELOW threshold (<)" m.triggerLessThan)
                      : catMaybes
                        [ m.warningThreshold <&> \wt -> "- **Warning Threshold**: " <> show wt <> " (warning level)"
                        , m.alertRecoveryThreshold <&> \art -> "- **Alert Recovery**: Alert clears when value returns to " <> show art
                        , m.warningRecoveryThreshold <&> \wrt -> "- **Warning Recovery**: Warning clears when value returns to " <> show wrt
                        ]
                )
              ,
                ( "### Current Status"
                , \m ->
                    ("- **Status**: " <> show m.currentStatus <> " (last check: " <> show m.currentValue <> ")")
                      : catMaybes
                        [ m.alertLastTriggered <&> \t -> "- **Last Alert Triggered**: " <> formatUTC t
                        , m.warningLastTriggered <&> \t -> "- **Last Warning Triggered**: " <> formatUTC t
                        ]
                )
              ,
                ( "### Notification Configuration"
                , \m ->
                    [ "- **Alert Title**: " <> m.alertConfig.title
                    , "- **Severity**: " <> m.alertConfig.severity
                    , "- **Email Recipients**: " <> if m.alertConfig.emailAll then "All project members" else T.intercalate ", " (V.toList $ fmap CI.original m.alertConfig.emails)
                    , if V.null m.alertConfig.slackChannels then "" else "- **Slack Channels**: " <> T.intercalate ", " (V.toList m.alertConfig.slackChannels)
                    ]
                )
              ]
          ]
        <> catMaybes
          [ guard (metricsData.rowsCount > 0) $> formatQueryResults metricsData
          , guard (metricsData.rowsCount == 0 && isNothing monitorM) $> "\n_Note: Monitor record was deleted. Only basic alert data available._"
          ]
    formatQueryResults md =
      let timestampIdx = V.elemIndex "timestamp" md.headers
          formatRow = V.imap \idx -> \case
            Just n | Just idx == timestampIdx -> formatUTC $ POSIX.posixSecondsToUTCTime $ realToFrac n
            Just val -> show val
            Nothing -> "N/A"
       in unlines
            [ ""
            , "## Alert Query Results"
            , ""
            , "Recent data points from the alert query (last " <> show (floor md.rowsCount :: Int) <> " measurements):"
            , ""
            , maybe "" formatQueryStats md.stats
            , ""
            , "| " <> T.intercalate " | " (V.toList md.headers) <> " |"
            , "|" <> T.replicate (V.length md.headers) "-------|"
            , unlines $ V.toList $ flip V.map (V.take 20 md.dataset) \row ->
                "| " <> T.intercalate " | " (V.toList $ formatRow row) <> " |"
            , if md.rowsCount > 20 then "... (" <> show (floor md.rowsCount - 20 :: Int) <> " more rows)" else ""
            ]
      where
        formatQueryStats stats =
          unlines
            [ "**Statistics:**"
            , "- Min: " <> show stats.min
            , "- Max: " <> show stats.max
            , "- Mean: " <> show stats.mean <> " (average)"
            , "- Sum: " <> show stats.sum
            , "- Count: " <> show stats.count <> " data points"
            ]
    formatFacetSummaryForAI summary =
      let FacetData facetMap = summary.facetJson
          formatField (fieldName, values) =
            "- "
              <> fieldName
              <> ": "
              <> T.intercalate ", " (map (\fv -> fv.value <> " (" <> show fv.count <> ")") $ take 10 values)
              <> bool "" ", ..." (length values > 10)
          topFields = take 30 $ sortOn (Down . sum . map (.count) . snd) $ HM.toList facetMap
       in unlines
            $ "Available telemetry fields (top values by frequency):"
            : map formatField topFields
              <> ["... and " <> show (HM.size facetMap - 30) <> " more fields" | HM.size facetMap > 30]
    guide =
      unlines
        [ "You are Monoscope's issue-investigation assistant — an expert debugger embedded in the issue detail page. The user is on-call and trying to understand a specific issue. You have access to its details, errors, stack traces, and trace data, plus tools that fetch live telemetry."
        , ""
        , "Tone: precise, technical, calm. Answer like a senior SRE pairing on a debug — direct, no fluff."
        , ""
        , "## Current Context"
        , "CURRENT TIME (UTC): " <> show now
        , "Use the current time to interpret relative phrases (e.g. \"last 2 hours\" → `{\"since\": \"2H\"}`)."
        , ""
        , "## Telemetry Schema"
        , "<schema>"
        , Schema.generateSchemaForAI Schema.telemetrySchema
        , "</schema>"
        , ""
        , AI.kqlGuide
        , ""
        , AI.outputFormatInstructions
        , ""
        , "## How To Investigate"
        , "1. Identify the likely root cause from the error type, stack trace, and surrounding telemetry."
        , "2. Use the issue's service / method / path context to narrow down."
        , "3. Suggest concrete debugging steps or fixes — name files, fields, queries when possible."
        , ""
        , "## Tool-Use Policy (overrides the workflow in <output_format>)"
        , "- Analysis questions (\"What could cause this?\", \"Suggest a fix\") → answer DIRECTLY from the issue context. Do NOT call tools, and do NOT call `run_query`."
        , "- Chart / visualization requests (\"plot errors over time\", \"show a chart of...\") → build the KQL query and `widgets` config directly from <schema>. Do NOT call `get_schema`, `get_field_values`, or `run_query` — the chat panel renders the chart from the query alone."
        , "- Call tools ONLY when the answer must contain actual data values from the live store (e.g. \"top 5 services by error count\" where real numbers are required)."
        , ""
        , "## Response Format"
        , "- Lead with a single-sentence summary."
        , "- Follow with bullets or short paragraphs — never walls of text."
        , "- The chat panel is ~400px wide, so brevity matters."
        , "- For chart requests, prioritize a correct KQL query and a data-driven explanation."
        ]


-- | A webhook's signed Unix-seconds timestamp is within five minutes of @now@, past or
-- future, so a captured request cannot be replayed later.
freshWebhookTimestamp :: UTCTime -> Text -> Bool
freshWebhookTimestamp now ts =
  not (T.null ts) && T.all isDigit ts && any (\seconds -> abs (POSIX.utcTimeToPOSIXSeconds now - fromInteger seconds) <= 300) (readMaybe @Integer $ toString ts)


verifyWidgetSignature :: Text -> Projects.ProjectId -> Text -> Maybe Text -> Either LBS.ByteString ()
verifyWidgetSignature secret pid widgetJson = maybe (Left "Missing signature") \sig ->
  unless (BA.constEq (encodeUtf8 sig :: ByteString) (encodeUtf8 (Widget.signWidgetUrl secret pid widgetJson) :: ByteString)) $ Left "Invalid signature"


data QueryIntent = ReportIntent Projects.ReportType | GeneralQueryIntent deriving (Eq, Show)


-- | Recognize complete saved-report requests; other instructions use the agent.
--
-- >>> detectReportIntent "send daily report"
-- ReportIntent RTDaily
-- >>> detectReportIntent "get weekly summary"
-- ReportIntent RTWeekly
-- >>> detectReportIntent "show me the report"
-- ReportIntent RTDaily
-- >>> detectReportIntent "show errors"
-- GeneralQueryIntent
-- >>> detectReportIntent "what's my error rate"
-- GeneralQueryIntent
-- >>> detectReportIntent "report"
-- GeneralQueryIntent
-- >>> detectReportIntent "daily"
-- GeneralQueryIntent
-- >>> detectReportIntent "forget the report"
-- GeneralQueryIntent
-- >>> detectReportIntent "show me how to send an incident summary"
-- GeneralQueryIntent
-- >>> detectReportIntent "Please show me the weekly report!"
-- ReportIntent RTWeekly
detectReportIntent :: Text -> QueryIntent
detectReportIntent query = fromMaybe GeneralQueryIntent do
  action :| request <- nonEmpty $ filter (`notElem` ["please", "me", "my", "the", "a", "an"]) $ words $ T.map (\c -> if isAlphaNum c then c else ' ') $ T.toCaseFold query
  guard $ action `elem` ["send", "get", "show", "give", "fetch", "retrieve"]
  (kind, reportType) <- case request of
    [kind] -> Just (kind, Projects.RTDaily)
    [period, kind] -> (kind,) <$> rightToMaybe (parseUrlPiece period)
    _ -> Nothing
  guard $ kind `elem` ["report", "summary"]
  pure $ ReportIntent reportType


-- | A rendered bot reply. The payload is already platform-shaped; the constructor
-- records which outbound /form/ it is, because WhatsApp must select a different
-- Twilio content template for each. The pipeline knows this at every send site, so
-- it is carried in the type rather than re-derived downstream by sniffing JSON keys.
data BotReply = ReplyText {payload :: AE.Value} | ReplyChart {payload :: AE.Value}


-- | Slack and Discord post both forms the same way.
botReplyPayload :: BotReply -> AE.Value
botReplyPayload r = r.payload


data SlackTextLimit = SectionText | MessageText


-- | The shared bot ask pipeline: intent detection → report or agentic query →
-- per-platform rendering → the supplied reply sink. Slack collects replies for
-- durable publication; other adapters can send them directly.
runBotQueryWithTools
  :: ProjectToolCtx es
  => Maybe AI.ProjectTools
  -> AI.InvocationMode
  -> BotType
  -> (BotReply -> Eff es ())
  -> EnvConfig
  -> AI.AgentAccess
  -> Projects.ProjectId
  -> Text
  -> Eff es (Maybe (UUIDId "conversation"))
  -> Eff es (Either Text ())
runBotQueryWithTools projectTools invocationMode target deliver envCfg access pid userQuery resolveThread =
  AI.requireAgentAccess access pid >> case detectReportIntent userQuery of
    ReportIntent reportType ->
      fmap Right
        $ Issues.getLatestReportByType pid reportType
        >>= \case
          Nothing -> sendText $ "No " <> display reportType <> " report found. Reports are generated automatically on schedule."
          Just report -> do
            let stamp = toText . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
                png q theme = Widget.widgetPngUrl envCfg.apiKeyEncryptionSecretKey envCfg.hostUrl pid def{Widget.wType = Widget.WTTimeseries, Widget.query = Just q, Widget.theme = theme} Nothing (Just $ stamp report.startTime) (Just $ stamp report.endTime)
            eventsUrl <- png "summarize count(*) by bin_auto(timestamp), status_code" Nothing
            errorsUrl <- png "status_code == \"ERROR\" | summarize count(*) by bin_auto(timestamp), status_code" (Just "roma")
            send $ ReplyText $ formatReport report eventsUrl errorsUrl
    GeneralQueryIntent -> do
      threadM <- resolveThread
      processAIQueryWithMode (Just envCfg) envCfg.enableTimefusionReads projectTools invocationMode access pid userQuery threadM envCfg.openaiModel envCfg.openaiApiKey
        >>= either (\err -> send (ReplyText $ formatBotError target ServiceError) $> Left err) (\response -> dispatchAIResponse threadM response $> Right ())
  where
    send reply = AI.requireAgentAccess access pid >> deliver reply
    explorerUrl = envCfg.hostUrl <> "p/" <> pid.toText <> "/log_explorer?"

    textReply txt = case target of
      Discord -> AE.object ["content" AE..= txt]
      WhatsApp -> AE.object ["body" AE..= txt]
      Slack -> slackResponse $ map slackSection $ textChunks SectionText txt

    sendText txt = traverse_ (send . ReplyText . textReply) $ case target of
      Slack -> textChunks MessageText txt
      (Discord; WhatsApp) -> [txt]

    textChunks part = unfoldr $ \txt -> do
      guard $ not $ T.null txt
      let limit = case part of SectionText -> 3000; MessageText -> 24000
          (prefix, remainder) = T.splitAt limit txt
          (whole, suffix) = runIdentity $ T.spanEndM (pure . not . isSpace) prefix
      pure $ if T.null remainder || T.null whole then (prefix, remainder) else (whole, suffix <> remainder)

    -- WhatsApp goes out as a plain-text template body (Twilio has no rich-message
    -- equivalent), so it reuses the text envelope rather than embedding the chart PNGs.
    formatReport report eventsUrl errorsUrl = case target of
      Slack ->
        slackResponse
          $ [ slackHeader $ title " Report"
            , slackContext ["*Period:* " <> period " → "]
            , slackSection $ "Total Events: *" <> show totalEvents <> "*  •  Total Errors: *" <> show totalErrors <> "*"
            , AE.object ["type" AE..= ("divider" :: Text)]
            ]
          <> catMaybes [slackImage (chartAlt "Events" " showing " totalEvents) Nothing <$> eventsUrl, slackImage (chartAlt "Errors" " showing " totalErrors) Nothing <$> errorsUrl]
          <> [slackActions [linkButton "view-full-report" (botEmoji EmojiSearch <> " View Full Report") reportUrl]]
      Discord ->
        dcContainer 26879
          $ [ dcText $ botEmoji EmojiChart <> " **" <> T.toTitle (display report.reportType) <> " Report**"
            , dcText $ "**Period:** " <> period " → "
            , dcText $ "Total Events: **" <> show totalEvents <> "**  •  Total Errors: **" <> show totalErrors <> "**"
            ]
          <> dcGallery [(eventsUrl, chartAlt "Events" ": " totalEvents), (errorsUrl, chartAlt "Errors" ": " totalErrors)]
          <> [dcLinkButton (botEmoji EmojiSearch <> " View Full Report") reportUrl]
      WhatsApp -> textReply $ title " Report\nPeriod: " <> period " - " <> "\nTotal Events: " <> show totalEvents <> "\nTotal Errors: " <> show totalErrors <> "\nView: " <> reportUrl
      where
        reportUrl = envCfg.hostUrl <> "p/" <> pid.toText <> "/reports/" <> report.id.toText
        day = toText . formatTime defaultTimeLocale "%Y-%m-%d"
        period sep = day report.startTime <> sep <> day report.endTime
        title suffix = botEmoji EmojiChart <> " " <> T.toTitle (display report.reportType) <> suffix
        chartAlt what sep n = what <> " chart for " <> display report.reportType <> " report" <> sep <> show @Text n <> " total " <> T.toLower what
        total k = maybe (0 :: Int) round (report.reportJson ^? key k . key "total" . _Number)
        (totalEvents, totalErrors) = (total "events", total "errors")

    -- Use each widget's declared row/chart shape on every bot surface.
    dispatchAIResponse conversation resp = do
      AI.requireAgentAccess access pid
      now <- Time.currentTime
      let (fromTimeM, toTimeM, _) = maybe (Nothing, Nothing, Nothing) (TP.parseTimeRange now) resp.timeRange
          iso = fmap (toText . iso8601Show)
          renderWidget widget
            | (queryM, Charts.DTText) <- Widget.chartQuery widget = do
                let query = fromMaybe "" queryM
                case parseQueryToAST query of
                  Left _ -> send $ ReplyText $ formatBotError target (QueryParseError query)
                  Right query' ->
                    LogQueries.selectLogTable envCfg.enableTimefusionReads pid query' query Nothing (fromTimeM, toTimeM) [] Nothing Nothing Nothing Nothing
                      >>= send
                      . ReplyText
                      . tableReply query
            | otherwise = do
                let query = fromMaybe "" widget.query
                    chartType = Widget.mapWidgetTypeToChartType widget.wType
                    queryUrl = maybe (explorerUrl <> "viz_type=" <> chartType <> "&query=" <> toUriStr query) (\identifier -> hostPath envCfg.hostUrl $ "p/" <> pid.toText <> "/ai/" <> identifier.toText) conversation
                    viewLabel = bool "View in Log Explorer" "View in Monoscope" (isJust conversation)
                    title = fromMaybe userQuery widget.title
                imageUrl <- Widget.widgetPngUrl envCfg.apiKeyEncryptionSecretKey envCfg.hostUrl pid widget Nothing (iso fromTimeM) (iso toTimeM)
                send $ ReplyChart $ case target of
                  Slack ->
                    AE.object
                      [ "blocks"
                          AE..= arr
                            ( [slackHeader (T.take 150 $ botEmoji EmojiChart <> " " <> title)]
                                <> maybeToList (slackImage (T.take 2000 $ "Chart: " <> title) Nothing <$> mfilter ((<= 3000) . T.length) imageUrl)
                                <> [ slackContext ["*Query:* `" <> query <> "`"]
                                   , slackActions [linkButton "view-log-explorer" (botEmoji EmojiSearch <> " " <> viewLabel) queryUrl]
                                   ]
                            )
                      , "response_type" AE..= ("in_channel" :: Text)
                      , "replace_original" AE..= True
                      ]
                  Discord ->
                    dcContainer 26879
                      $ [dcText $ botEmoji EmojiChart <> " **" <> title <> "**"]
                      <> dcGallery [(imageUrl, "Chart visualization: " <> title)]
                      <> [ dcText $ "**Query:** `" <> query <> "`"
                         , dcLinkButton (botEmoji EmojiSearch <> " " <> viewLabel) queryUrl
                         ]
                  -- Twilio content-template variables: WhatsApp renders the chart itself, so
                  -- "3" is the query string the template appends to the chart endpoint, "4"
                  -- the (host-relative) explorer link.
                  WhatsApp ->
                    AE.object
                      [ "1" AE..= ("*" <> title <> "*")
                      , "2" AE..= ("`" <> query <> "`")
                      , "3" AE..= ("time=" <> toUriStr (show now) <> "&q=" <> toUriStr query <> "&p=" <> toUriStr pid.toText <> "&t=" <> toUriStr chartType <> "&from=" <> toUriStr (fold $ iso fromTimeM) <> "&to=" <> toUriStr (fold $ iso toTimeM))
                      , "4" AE..= (pid.toText <> "/log_explorer?viz_type=" <> chartType <> "&query=" <> toUriStr query)
                      ]
      let explanation = mfilter (not . T.null . T.strip) resp.explanation
      case AI.responseWidgets resp of
        [] -> sendText $ fromMaybe "No response available" explanation
        widgets -> do
          traverse_ renderWidget $ take 10 widgets
          whenJust explanation sendText

    tableReply query = \case
      Left _ -> formatBotError target (QueryParseError query)
      Right (_, _, 0) -> formatBotError target NoDataError
      Right (requestVecs, colNames, resultCount) ->
        let rows = V.toList (V.take 15 requestVecs)
            shownCount = length rows
            cells = map (map $ T.take 60 . unwords . words) $ colNames : map (map CLIFormat.valToText . V.toList) rows
            tableData = "```\n" <> T.take 1200 (unlines $ map (CLIFormat.formatRow $ CLIFormat.colWidths cells) cells) <> "\n```"
            url' = explorerUrl <> "query=" <> toUriStr query
            moreText = if resultCount > shownCount then "\n_" <> show (resultCount - shownCount) <> " more results in Log Explorer_" else ""
            content = botEmoji EmojiTable <> " **Query Results** (showing " <> show shownCount <> " of " <> show resultCount <> " rows)\n`" <> query <> "`\n" <> tableData <> moreText <> "\n"
         in case target of
              Discord -> AE.object ["content" AE..= (content <> "\n[View in Log Explorer →](" <> url' <> ")")]
              WhatsApp -> AE.object ["body" AE..= (content <> "\n" <> url')]
              Slack ->
                slackResponse
                  [ slackHeader $ botEmoji EmojiTable <> " Query Results"
                  , slackContext ["Showing *" <> show shownCount <> "* of *" <> show resultCount <> "* rows"]
                  , slackSection $ "`" <> query <> "`"
                  , AE.object ["type" AE..= ("divider" :: Text)]
                  , slackSection tableData
                  , slackActions [linkButton "view-log-explorer" (botEmoji EmojiSearch <> " View in Log Explorer") url']
                  ]


-- | Resolve a bot thread's conversation, backfilling it from the platform's own
-- thread API on first contact, and return its ID for role-preserving model history.
-- @backfill@ is the only platform-specific part: it fetches the platform's
-- messages and classifies their roles (@Nothing@ = fetch failed).
withBotThread
  :: (DB es, Error ServerError :> es, IOE :> es, Log :> es, Time.Time :> es)
  => BotType
  -> Projects.ProjectId
  -> UUIDId "conversation"
  -> Issues.ConversationType
  -> AE.Value
  -> Eff es (Maybe [(Issues.ChatMessageKind, Text)])
  -> Eff es (UUIDId "conversation")
withBotThread target pid convId convType meta backfill = do
  _ <- Issues.getOrCreateConversation pid convId convType meta
  existingHistory <- Issues.selectChatHistory pid convId
  when (null existingHistory) do
    result <- backfill `onException` Log.logAttention "Bot thread backfill failed" ctx
    maybe (Log.logAttention "Bot thread backfill failed" ctx >> throwError err503) (Issues.seedChatHistory pid convId) result
  pure convId
  where
    ctx = AE.object ["platform" AE..= show @Text target, "conv_id" AE..= show @Text convId]


-- | Resolve a dashboard id to its schema or template (else the overview), scoped to @pid@. The id
-- arrives inside client-controlled payloads (component custom_id, message
-- body), so an unscoped lookup would render another tenant's dashboard.
withDashboardTemplate :: DB es => Projects.ProjectId -> Text -> (Dashboards.Dashboard -> Eff es ()) -> Eff es ()
withDashboardTemplate pid dashboardId act = whenJust (idFromText dashboardId) \did ->
  whenJustM (Dashboards.getDashboardByProjectId pid did) \vm ->
    mapM_ act $ loadDashboardFromVM dashboardTemplatesCompiled vm <|> find ((== Just "_overview.yaml") . (.file)) dashboardTemplatesCompiled


-- | OAuth install state/redirect param: @"projectId"@ or @"projectId__onboarding"@.
parseInstallState :: Maybe Text -> (Maybe Projects.ProjectId, Bool)
parseInstallState stateM = (Projects.projectIdFromText =<< viaNonEmpty head parts, length parts > 1)
  where
    parts = maybe [] (T.splitOn "__") stateM


-- | Post-install reply: onboarding resumes in the app, otherwise the
-- "installed" confirmation page.
installedResponse :: Text -> Projects.ProjectId -> Bool -> BWConfig -> Headers '[Header "Location" Text] BotResponse
installedResponse platform pid isOnboarding bwconf
  | isOnboarding = addHeader (stepUrl pid NotifChannel) $ NoContent $ PageCtx bwconf ()
  | otherwise = addHeader "" $ BotLinked $ PageCtx bwconf (platform, Just pid)
