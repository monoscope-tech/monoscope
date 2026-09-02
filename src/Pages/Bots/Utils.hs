module Pages.Bots.Utils (BotType (..), BotReply (..), botReplyPayload, BotResponse (..), Channel (..), authHeader, contentTypeHeader, mrkdwn, plainTxt, textBlock, elemsBlock, linkButton, imageBlock, slackResponse, dcContainer, dcText, dcGallery, dcLinkButton, processAIQuery, verifyWidgetSignature, QueryIntent (..), ReportType (..), detectReportIntent, BotErrorType (..), formatBotError, botEmoji, getLoadingMessage, formatTextResponse, BotThread (..), runBotQuery, withBotThread, withDashboardTemplate, parseInstallState, installedResponse) where

import Control.Lens ((.~), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _Number)
import Data.ByteArray qualified as BA
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.LLM qualified as ELLM
import Data.Effectful.Wreq (Options, header)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Effectful.Labeled (Labeled)
import Effectful.Log (Log)
import Effectful.Time qualified as Time
import Langchain.LLM.Core qualified as LLM
import Lucid
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogQueries qualified as LogQueries
import Models.Apis.SchemaCatalog qualified as SchemaCatalog
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (urlEncode)
import Pages.BodyWrapper (BWConfig, PageCtx (..))
import Pages.Components (navBar)
import Pkg.AI qualified as AI
import Pkg.Components.TimePicker qualified as TP
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId, idFromText)
import Pkg.Parser (parseQueryToAST)
import Relude
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Server (ServerError)
import System.Config (EnvConfig (..))
import System.Logging qualified as Log
import System.Tracing (Tracing)
import System.Types (DB)
import UnliftIO.Exception (tryAny)
import Utils (faSprite_, getDurationNSMS, listToIndexHashMap, lookupVecBoolByKey, lookupVecIntByKey, lookupVecTextByKey, toUriStr)


data BotType = Discord | Slack | WhatsApp
  deriving (Eq, Show)


-- | Status emoji for visual indicators (always paired with text for accessibility)
botEmoji :: Text -> Text
botEmoji = \case
  "success" -> "🟢"
  "warning" -> "🟡"
  "error" -> "🔴"
  "chart" -> "📊"
  "search" -> "🔍"
  "table" -> "📋"
  "loading" -> "⏳"
  "bell" -> "🔔"
  _ -> ""


-- | Error types for contextual error messages
data BotErrorType = QueryParseError Text | NoDataError | ServiceError | TimeoutError
  deriving (Eq, Show)


-- | Format error messages with context and guidance
formatBotError :: BotType -> BotErrorType -> AE.Value
formatBotError target err = case target of
  Discord -> AE.object ["content" AE..= msg]
  WhatsApp -> AE.object ["body" AE..= msg]
  Slack -> AE.object ["text" AE..= msg, "response_type" AE..= "in_channel", "replace_original" AE..= True, "delete_original" AE..= True]
  where
    msg = case err of
      QueryParseError snippet -> botEmoji "warning" <> " Couldn't parse query\n`" <> T.take 50 snippet <> "`\nTry: 'show errors in last hour'"
      NoDataError -> botEmoji "search" <> " No data found for your query\nTry expanding the time range or adjusting filters."
      ServiceError -> botEmoji "error" <> " Something went wrong\nPlease try again in a moment."
      TimeoutError -> botEmoji "error" <> " Query timed out\nThe data range might be too large. Try narrowing to last 24h."


-- | Get loading message based on detected query intent
getLoadingMessage :: QueryIntent -> Text
getLoadingMessage = \case
  ReportIntent _ -> botEmoji "chart" <> " Fetching your report..."
  GeneralQueryIntent -> botEmoji "search" <> " Analyzing your query..."


data BotResponse
  = BotLinked (PageCtx (Text, Maybe Projects.ProjectId))
  | NoTokenFound (PageCtx ())
  | DiscordError (PageCtx ())
  | NoContent (PageCtx ())


instance ToHtml BotResponse where
  toHtml = \case
    BotLinked (PageCtx bwconf (bot, pidM)) -> toHtml $ PageCtx bwconf $ installedSuccess bot pidM
    DiscordError (PageCtx bwconf ()) -> toHtml $ PageCtx bwconf discordError
    NoTokenFound (PageCtx bwconf ()) -> toHtml $ PageCtx bwconf noTokenFound
    NoContent (PageCtx bwconf ()) -> toHtml $ PageCtx bwconf ("" :: Html ())
  toHtmlRaw = toHtml


-- | @authHeader "Bot" tok@ (Discord) / @authHeader "Bearer" tok@ (Slack).
authHeader :: Text -> Text -> Options -> Options
authHeader scheme token = header "Authorization" .~ [encodeUtf8 $ scheme <> " " <> token]


contentTypeHeader :: Text -> Options -> Options
contentTypeHeader contentType = header "Content-Type" .~ [encodeUtf8 contentType]


-- Slack Block Kit / Discord component builders

-- | JSON array literal; pins the element type, which OverloadedLists would otherwise leave ambiguous.
arr :: [AE.Value] -> AE.Value
arr = AE.toJSON


mrkdwn, plainTxt :: Text -> AE.Value
mrkdwn t = AE.object ["type" AE..= ("mrkdwn" :: Text), "text" AE..= t]
plainTxt t = AE.object ["type" AE..= ("plain_text" :: Text), "text" AE..= t, "emoji" AE..= True]


textBlock :: Text -> AE.Value -> AE.Value
textBlock ty t = AE.object ["type" AE..= ty, "text" AE..= t]


elemsBlock :: Text -> [AE.Value] -> AE.Value
elemsBlock ty es = AE.object ["type" AE..= ty, "elements" AE..= es]


linkButton :: Text -> Text -> Text -> AE.Value
linkButton actionId label url = AE.object ["type" AE..= ("button" :: Text), "action_id" AE..= actionId, "text" AE..= plainTxt label, "url" AE..= url]


imageBlock :: Text -> Text -> AE.Value
imageBlock url alt = AE.object ["type" AE..= ("image" :: Text), "image_url" AE..= url, "alt_text" AE..= alt]


-- | Slack in-channel response replacing the ephemeral loading message.
slackResponse :: [AE.Value] -> AE.Value
slackResponse blocks = AE.object ["blocks" AE..= blocks, "response_type" AE..= ("in_channel" :: Text), "replace_original" AE..= True, "delete_original" AE..= True]


-- Discord "components v2" building blocks (flag 32768 opts the message into the new layout).

dcContainer :: Int -> [AE.Value] -> AE.Value
dcContainer accent components = AE.object ["flags" AE..= (32768 :: Int), "components" AE..= arr [AE.object ["type" AE..= (17 :: Int), "accent_color" AE..= accent, "components" AE..= arr components]]]


dcText :: Text -> AE.Value
dcText content = AE.object ["type" AE..= (10 :: Int), "content" AE..= content]


-- | Media gallery; one entry per (url, alt-text) pair.
dcGallery :: [(Text, Text)] -> AE.Value
dcGallery items = AE.object ["type" AE..= (12 :: Int), "items" AE..= arr [AE.object ["media" AE..= AE.object ["url" AE..= u], "description" AE..= d] | (u, d) <- items]]


dcLinkButton :: Text -> Text -> AE.Value
dcLinkButton label url = AE.object ["type" AE..= (1 :: Int), "components" AE..= arr [AE.object ["type" AE..= (2 :: Int), "label" AE..= label, "url" AE..= url, "style" AE..= (5 :: Int)]]]


handleTableResponse :: BotType -> Either Text (V.Vector (V.Vector AE.Value), [Text], Int) -> EnvConfig -> Projects.ProjectId -> Text -> AE.Value
handleTableResponse target tableAsVecE envCfg projectId query =
  case tableAsVecE of
    Left _ -> formatBotError target (QueryParseError query)
    Right (requestVecs, colNames, resultCount) ->
      if resultCount == 0
        then formatBotError target NoDataError
        else
          let (tableData, shownCount) = recsVecToTableData requestVecs (listToIndexHashMap colNames)
              url' = envCfg.hostUrl <> "p/" <> projectId.toText <> "/log_explorer?query=" <> decodeUtf8 (urlEncode True $ encodeUtf8 query)
              explorerLink = "[View in Log Explorer →](" <> url' <> ")"
              moreText = if resultCount > shownCount then "\n_" <> show (resultCount - shownCount) <> " more results in Log Explorer_" else ""
              headerEmoji = botEmoji "table"
              headerText = headerEmoji <> " **Query Results** (showing " <> show shownCount <> " of " <> show resultCount <> " events)"
              content = headerText <> "\n`" <> query <> "`\n" <> tableData <> moreText <> "\n"
           in case target of
                Discord -> AE.object ["content" AE..= (content <> "\n" <> explorerLink)]
                WhatsApp -> AE.object ["body" AE..= (content <> "\n" <> url')]
                Slack ->
                  slackResponse
                    [ textBlock "header" (plainTxt $ headerEmoji <> " Query Results")
                    , elemsBlock "context" [mrkdwn $ "Showing *" <> show shownCount <> "* of *" <> show resultCount <> "* events"]
                    , textBlock "section" (mrkdwn $ "`" <> query <> "`")
                    , AE.object ["type" AE..= ("divider" :: Text)]
                    , textBlock "section" (mrkdwn tableData)
                    , elemsBlock "actions" [linkButton "view-log-explorer" (botEmoji "search" <> " View in Log Explorer") url']
                    ]


recsVecToTableData :: V.Vector (V.Vector AE.Value) -> HashMap Text Int -> (Text, Int)
recsVecToTableData recsVec colIdxMap = ("```\n" <> unlines (hd : map row rows) <> "```", length rows)
  where
    rows = V.toList (V.take 15 recsVec)
    pad n = T.take n . T.justifyLeft n ' '
    txt n v k = pad n $ fromMaybe "" $ lookupVecTextByKey v colIdxMap k
    hd = unwords [pad 20 "TIME", pad 15 "SERVICE", pad 20 "SPAN NAME", pad 8 "DURATION", "STATUS"]
    row v =
      unwords
        [ txt 18 v "timestamp"
        , txt 15 v "service"
        , txt 20 v "span_name"
        , pad 8 (toText $ getDurationNSMS $ fromIntegral $ lookupVecIntByKey v colIdxMap "duration")
        , botEmoji (if lookupVecBoolByKey v colIdxMap "errors" then "error" else "success")
        ]


noTokenFound :: Html ()
noTokenFound = do
  navBar
  section_ [class_ "h-full mt-[80px] w-[1000px] flex flex-col items-center mx-auto"] do
    h3_ [class_ "text-5xl font-semibold my-8"] "Token Not Found"
    p_ [class_ "text-2xl"] "No slack access token found, reinstall the Monoscope slack app to try again."


discordError :: Html ()
discordError = do
  navBar
  section_ [class_ "h-full mt-[80px] w-[1000px] flex flex-col items-center mx-auto"] do
    faSprite_ "circle-exclamation" "solid" "text-iconError h-10 w-10"
    h3_ [class_ "text-4xl font-bold my-6 text-textError"] "Uh-oh! Something went wrong"
    p_
      [class_ "text-xl text-textStrong text-center max-w-prose mb-4"]
      "We hit a snag while trying to install the Discord bot. Don’t worry — it happens!"
    p_
      [class_ "text-md text-textWeak text-center max-w-prose"]
      "This could be due to not adding discord from the integrations page, click on add to discord on the integrations page to try again."


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
              div_ [class_ "bg-gradient-to-br from-fillWeaker to-fillBrand-weak rounded-2xl p-6 border border-strokeBrand-weak"] do
                div_ [class_ "flex items-start space-x-4"] do
                  div_ [class_ "flex-shrink-0"] do
                    div_ [class_ "inline-flex items-center justify-center w-12 h-12 bg-fillBrand-strong rounded-xl"] do
                      span_ [class_ "text-white font-bold text-lg"] "?"
                  div_ [class_ "flex-1"] do
                    div_ [class_ "flex items-center space-x-2 mb-3"] do
                      span_ [class_ "monospace bg-fillBrand-weak text-textBrand px-3 py-1 rounded-lg font-semibold"] "/monoscope"
                      span_ [class_ "bg-fillBrand-strong text-white text-xs px-2 py-1 rounded-full"] "AI Powered"
                    p_ [class_ "text-textStrong text-sm"] $ toHtml $ "Ask questions about your API, get reports, and get instant insights with logs and charts delivered right to " <> botPlatform <> "."
              div_ [class_ "bg-gradient-to-br from-fillSuccess-weak to-fillWeaker rounded-2xl p-6 border border-strokeSuccess-weak"] do
                div_ [class_ "flex items-start space-x-4"] do
                  div_ [class_ "flex-shrink-0"] do
                    div_ [class_ "inline-flex items-center justify-center w-12 h-12 bg-fillSuccess-strong rounded-xl"] do
                      faSprite_ "bell" "regular" "h-6 w-6 text-white"
                  div_ [class_ "flex-1"] do
                    div_ [class_ "flex items-center space-x-2 mb-3"] do
                      span_ [class_ "monospace bg-fillSuccess-weak text-textSuccess px-3 py-1 rounded-lg font-semibold"] "/here"
                      span_ [class_ "bg-fillSuccess-strong text-white text-xs px-2 py-1 rounded-full"] "Alerts"
                    p_
                      [class_ "text-textStrong text-sm"]
                      "Set up this channel to receive automated error reports, weekly summaries, and daily performance alerts."


data Channel = Channel
  { channelName :: Text
  , channelId :: Text
  , channelType :: Maybe Int
  }
  deriving (Generic, Show)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "channel", DAE.CamelToSnake]] Channel


processAIQuery :: (DB es, ELLM.LLM :> es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es, Tracing :> es) => Bool -> Projects.ProjectId -> Text -> Maybe Text -> Text -> Text -> Eff es (Either Text AI.LLMResponse)
processAIQuery useTf pid userQuery threadCtx model apiKey = do
  now <- Time.currentTime
  let dayAgo = addUTCTime (-86400) now
  facetSummaryM <- SchemaCatalog.getFacetSummary pid "otel_logs_and_spans" dayAgo now
  let config = (AI.defaultAgenticConfig pid){AI.facetContext = facetSummaryM, AI.customContext = threadCtx, AI.useTimefusion = useTf}
  result <- AI.runAgenticQuery config userQuery model apiKey
  whenLeft_ result \err -> Log.logAttention "processAIQuery failed" $ AE.object ["error" AE..= err, "userQuery" AE..= userQuery, "projectId" AE..= pid.toText]
  pure result


formatHistoryAsContext :: Text -> [LLM.Message] -> Text
formatHistoryAsContext platform msgs =
  unlines
    [ "\n\nTHREADS:"
    , "- this query is part of a " <> platform <> " conversation thread. Use previous messages for additional context if needed."
    , "- the user query is the main one to answer, but earlier messages may contain important clarifications or parameters."
    , "\nPrevious thread messages:\n"
    , T.intercalate "\n" ["[" <> show (LLM.role m) <> "] " <> LLM.content m | m <- msgs]
    ]


verifyWidgetSignature :: Text -> Projects.ProjectId -> Text -> Maybe Text -> Either LBS.ByteString ()
verifyWidgetSignature secret pid widgetJson = maybe (Left "Missing signature") \sig ->
  unless (BA.constEq (encodeUtf8 sig :: ByteString) (encodeUtf8 (Widget.signWidgetUrl secret pid widgetJson) :: ByteString)) $ Left "Invalid signature"


-- | Report query intent detection
data ReportType = DailyReport | WeeklyReport deriving (Eq, Show)


data QueryIntent = ReportIntent ReportType | GeneralQueryIntent deriving (Eq, Show)


-- | Detect if user query is requesting a report. Requires action verb + "report/summary".
--
-- >>> detectReportIntent "send daily report"
-- ReportIntent DailyReport
-- >>> detectReportIntent "get weekly summary"
-- ReportIntent WeeklyReport
-- >>> detectReportIntent "show me the report"
-- ReportIntent DailyReport
-- >>> detectReportIntent "show errors"
-- GeneralQueryIntent
-- >>> detectReportIntent "what's my error rate"
-- GeneralQueryIntent
-- >>> detectReportIntent "report"
-- GeneralQueryIntent
-- >>> detectReportIntent "daily"
-- GeneralQueryIntent
detectReportIntent :: Text -> QueryIntent
detectReportIntent query =
  let q = T.toLower $ T.strip query
      has :: [Text] -> Bool
      has = any (`T.isInfixOf` q)
   in if has ["send", "get", "show", "give", "fetch", "retrieve"] && has ["report", "summary"]
        then ReportIntent $ bool DailyReport WeeklyReport ("week" `T.isInfixOf` q)
        else GeneralQueryIntent


-- | Process report query - retrieves latest report from DB
processReportQuery :: (DB es, Log :> es) => Projects.ProjectId -> ReportType -> EnvConfig -> Eff es (Either Text (Issues.Report, Text, Text))
processReportQuery pid reportType envCfg = do
  let typeTxt = case reportType of DailyReport -> "daily"; WeeklyReport -> "weekly"
  Issues.getLatestReportByType pid typeTxt >>= \case
    Nothing -> pure $ Left $ "No " <> typeTxt <> " report found. Reports are generated automatically on schedule."
    Just report -> do
      let stamp = toText . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
          png q theme = Widget.widgetPngUrl envCfg.apiKeyEncryptionSecretKey envCfg.hostUrl pid def{Widget.wType = Widget.WTTimeseries, Widget.query = Just q, Widget.theme = theme} Nothing (Just $ stamp report.startTime) (Just $ stamp report.endTime)
      fmap Right $ (,,) report <$> png "summarize count(*) by bin_auto(timestamp), status_code" Nothing <*> png "status_code == \"ERROR\" | summarize count(*) by bin_auto(timestamp), status_code" (Just "roma")


-- | Render a stored report for a platform. WhatsApp goes out as a plain-text
-- template body (Twilio has no rich-message equivalent), so it reuses the
-- text envelope rather than embedding the two signed chart PNGs.
formatReport :: BotType -> Issues.Report -> Projects.ProjectId -> EnvConfig -> Text -> Text -> AE.Value
formatReport target report pid envCfg eventsUrl errorsUrl = case target of
  Slack ->
    slackResponse
      [ textBlock "header" (plainTxt $ title " Report")
      , elemsBlock "context" [mrkdwn $ "*Period:* " <> period " → "]
      , textBlock "section" (mrkdwn $ "Total Events: *" <> show totalEvents <> "*  •  Total Errors: *" <> show totalErrors <> "*")
      , AE.object ["type" AE..= ("divider" :: Text)]
      , imageBlock eventsUrl $ chartAlt "Events" " showing " totalEvents
      , imageBlock errorsUrl $ chartAlt "Errors" " showing " totalErrors
      , elemsBlock "actions" [linkButton "view-full-report" (botEmoji "search" <> " View Full Report") reportUrl]
      ]
  Discord ->
    dcContainer
      26879
      [ dcText $ botEmoji "chart" <> " **" <> T.toTitle report.reportType <> " Report**"
      , dcText $ "**Period:** " <> period " → "
      , dcText $ "Total Events: **" <> show totalEvents <> "**  •  Total Errors: **" <> show totalErrors <> "**"
      , dcGallery [(eventsUrl, chartAlt "Events" ": " totalEvents), (errorsUrl, chartAlt "Errors" ": " totalErrors)]
      , dcLinkButton (botEmoji "search" <> " View Full Report") reportUrl
      ]
  WhatsApp -> formatTextResponse WhatsApp $ title " Report\nPeriod: " <> period " - " <> "\nTotal Events: " <> show totalEvents <> "\nTotal Errors: " <> show totalErrors <> "\nView: " <> reportUrl
  where
    reportUrl = envCfg.hostUrl <> "p/" <> pid.toText <> "/reports/" <> report.id.toText
    day = toText . formatTime defaultTimeLocale "%Y-%m-%d"
    period sep = day report.startTime <> sep <> day report.endTime
    title suffix = botEmoji "chart" <> " " <> T.toTitle report.reportType <> suffix
    chartAlt what sep n = what <> " chart for " <> report.reportType <> " report" <> sep <> show @Text n <> " total " <> T.toLower what
    (totalEvents, totalErrors) = parseReportStats report.reportJson


-- | Parse total events and errors from report JSON
parseReportStats :: AE.Value -> (Int, Int)
parseReportStats json = (getTotal "events", getTotal "errors")
  where
    getTotal k = maybe 0 round (json ^? key k . key "total" . _Number)


-- | Format text response for different bot platforms
formatTextResponse :: BotType -> Text -> AE.Value
formatTextResponse Discord txt = AE.object ["content" AE..= txt]
formatTextResponse WhatsApp txt = AE.object ["body" AE..= txt]
formatTextResponse Slack txt =
  AE.object
    [ "blocks" AE..= arr [textBlock "section" (mrkdwn txt)]
    , "response_type" AE..= ("in_channel" :: Text)
    , "replace_original" AE..= True
    , "delete_original" AE..= True
    ]


-- | Everything a per-platform chart message can be built from. Slack and
-- Discord embed the pre-signed PNG; WhatsApp's Twilio content template renders
-- the chart itself from the raw query parameters, so both forms are carried.
data ChartCtx = ChartCtx
  { question :: Text
  , query :: Text
  , chartType :: Text
  , queryUrl :: Text
  , imageUrl :: Text
  , projectId :: Projects.ProjectId
  , now :: UTCTime
  , fromTime :: Maybe UTCTime
  , toTime :: Maybe UTCTime
  }


formatChart :: BotType -> ChartCtx -> AE.Value
formatChart target c = case target of
  Slack ->
    AE.object
      [ "blocks"
          AE..= arr
            [ textBlock "header" $ plainTxt (botEmoji "chart" <> " " <> c.question)
            , imageBlock c.imageUrl ("Chart: " <> c.question)
            , elemsBlock "context" [mrkdwn ("*Query:* `" <> c.query <> "`")]
            , elemsBlock "actions" [linkButton "view-log-explorer" (botEmoji "search" <> " View in Log Explorer") c.queryUrl]
            ]
      , "response_type" AE..= ("in_channel" :: Text)
      , "replace_original" AE..= True
      ]
  Discord ->
    dcContainer
      26879
      [ dcText $ botEmoji "chart" <> " **" <> c.question <> "**"
      , dcGallery [(c.imageUrl, "Chart visualization: " <> c.question)]
      , dcText $ "**Query:** `" <> c.query <> "`"
      , dcLinkButton (botEmoji "search" <> " View in Log Explorer") c.queryUrl
      ]
  -- Twilio content-template variables; "3" is the query string the template
  -- appends to the chart endpoint, "4" the (host-relative) explorer link.
  WhatsApp ->
    AE.object
      [ "1" AE..= ("*" <> c.question <> "*")
      , "2" AE..= ("`" <> c.query <> "`")
      , "3" AE..= ("time=" <> toUriStr (show c.now) <> "&q=" <> toUriStr c.query <> "&p=" <> toUriStr c.projectId.toText <> "&t=" <> toUriStr c.chartType <> "&from=" <> toUriStr (iso c.fromTime) <> "&to=" <> toUriStr (iso c.toTime))
      , "4" AE..= (c.projectId.toText <> "/log_explorer?viz_type=" <> c.chartType <> "&query=" <> toUriStr c.query)
      ]
  where
    iso = maybe "" (toText . iso8601Show)


-- | A rendered bot reply. The payload is already platform-shaped by
-- 'formatTextResponse' / 'formatReport' / 'formatChart'; the constructor records
-- which outbound /form/ it is, because WhatsApp must select a different Twilio
-- content template for each. The pipeline knows this at every send site, so it
-- is carried in the type rather than re-derived downstream by sniffing whichever
-- JSON key the renderer happened to emit.
data BotReply = ReplyText AE.Value | ReplyChart AE.Value


-- | Slack and Discord post both forms the same way, so they discard the
-- distinction that WhatsApp needs. Spelled out per constructor rather than
-- pattern-matched with a wildcard, so a third form can't be added silently.
botReplyPayload :: BotReply -> AE.Value
botReplyPayload = \case
  ReplyText v -> v
  ReplyChart v -> v


-- | A bot conversation thread: where replies are persisted, plus the formatted
-- history handed to the model.
data BotThread = BotThread
  { convId :: UUIDId "conversation"
  , context :: Text
  }


-- | The shared bot ask pipeline: intent detection → report or agentic query →
-- per-platform rendering → outbound send. The only platform-specific inputs are
-- the 'BotType' (which selects every renderer) and @send@ (the transport, which
-- differs per platform *and* per call site: Slack response_url vs chat.postMessage,
-- Discord interaction followup, Twilio).
runBotQuery
  :: (DB es, ELLM.LLM :> es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es, Tracing :> es)
  => BotType
  -> (BotReply -> Eff es ())
  -> EnvConfig
  -> Projects.ProjectId
  -> Text
  -> Eff es (Maybe BotThread)
  -- ^ resolves the conversation thread; run only for agentic queries, so a
  -- report request never pays for a thread backfill
  -> Eff es ()
runBotQuery target send envCfg pid userQuery resolveThread = case detectReportIntent userQuery of
  ReportIntent reportType ->
    processReportQuery pid reportType envCfg
      >>= send
      . ReplyText
      . either (formatTextResponse target) (\(report, eventsUrl, errorsUrl) -> formatReport target report pid envCfg eventsUrl errorsUrl)
  GeneralQueryIntent -> do
    threadM <- resolveThread
    result <- processAIQuery envCfg.enableTimefusionReads pid userQuery ((.context) <$> threadM) envCfg.openaiModel envCfg.openaiApiKey
    whenJust threadM \t -> do
      Issues.insertChatMessage pid t.convId Issues.ChatUser userQuery Nothing Nothing
      whenRight_ result \resp -> whenJust resp.query \q -> Issues.insertChatMessage pid t.convId Issues.ChatAssistant q Nothing Nothing
    case result of
      Left _ -> send $ ReplyText $ formatBotError target ServiceError
      Right resp -> dispatchAIResponse resp
  where
    -- Renders a chart when the model picked a visualization, a result table when
    -- it produced a bare query, then any accompanying explanation.
    dispatchAIResponse resp = do
      now <- Time.currentTime
      let (fromTimeM, toTimeM, _) = maybe (Nothing, Nothing, Nothing) (TP.parseTimeRange now) resp.timeRange
      case resp.query of
        Nothing -> send $ ReplyText $ formatTextResponse target $ fromMaybe "No response available" resp.explanation
        Just query -> do
          case resp.visualization of
            Just vizType -> do
              let wType = Widget.mapChartTypeToWidgetType vizType
                  chartType = Widget.mapWidgetTypeToChartType wType
              imageUrl <- Widget.widgetPngUrl envCfg.apiKeyEncryptionSecretKey envCfg.hostUrl pid def{Widget.wType = wType, Widget.query = Just query} Nothing (toText . iso8601Show <$> fromTimeM) (toText . iso8601Show <$> toTimeM)
              send
                $ ReplyChart
                $ formatChart
                  target
                  ChartCtx
                    { question = userQuery
                    , query
                    , chartType
                    , queryUrl = envCfg.hostUrl <> "p/" <> pid.toText <> "/log_explorer?viz_type=" <> chartType <> "&query=" <> toUriStr query
                    , imageUrl
                    , projectId = pid
                    , now
                    , fromTime = fromTimeM
                    , toTime = toTimeM
                    }
            Nothing -> case parseQueryToAST query of
              Left _ -> send $ ReplyText $ formatBotError target (QueryParseError query)
              Right query' -> do
                tableAsVecE <- LogQueries.selectLogTable envCfg.enableTimefusionReads pid query' query Nothing (fromTimeM, toTimeM) [] Nothing Nothing Nothing
                send $ ReplyText $ handleTableResponse target tableAsVecE envCfg pid query
          whenJust resp.explanation (send . ReplyText . formatTextResponse target)


-- | Resolve a bot thread's conversation, backfilling it from the platform's own
-- thread API on first contact, and return the history formatted for the model.
-- @backfill@ is the only platform-specific part: it fetches the platform's
-- messages and classifies their roles (@Nothing@ = fetch failed).
withBotThread
  :: (DB es, Error ServerError :> es, Log :> es, Time.Time :> es)
  => BotType
  -> Projects.ProjectId
  -> UUIDId "conversation"
  -> Issues.ConversationType
  -> AE.Value
  -> Eff es (Maybe [(Issues.ChatRole, Text)])
  -> Eff es BotThread
withBotThread target pid convId convType meta backfill = do
  _ <- Issues.getOrCreateConversation pid convId convType meta
  existingHistory <- Issues.selectChatHistory convId
  -- Advisory lock: only the first interaction seeds the thread's history.
  when (null existingHistory) $ whenM (Issues.tryAcquireChatMigrationLock convId) do
    result <- tryAny $ backfill >>= maybe (Log.logAttention "Bot thread backfill fetch failed" ctx) (mapM_ \(role, txt) -> Issues.insertChatMessage pid convId role txt Nothing Nothing)
    Issues.releaseChatMigrationLock convId
    whenLeft_ result \err -> Log.logAttention "Bot thread backfill failed" $ AE.object ["platform" AE..= show @Text target, "conv_id" AE..= show @Text convId, "error" AE..= show @Text err]
  BotThread convId . formatHistoryAsContext (show target) . map AI.dbMessageToLLMMessage <$> Issues.selectChatHistory convId
  where
    ctx = AE.object ["platform" AE..= show @Text target, "conv_id" AE..= show @Text convId]


-- | Resolve a dashboard id to its on-disk template, scoped to @pid@. The id
-- arrives inside client-controlled payloads (component custom_id, message
-- body), so an unscoped lookup would render another tenant's dashboard.
withDashboardTemplate :: DB es => Projects.ProjectId -> Text -> (Dashboards.Dashboard -> Eff es ()) -> Eff es ()
withDashboardTemplate pid dashboardId act = whenJust (idFromText dashboardId) \did ->
  whenJustM (Dashboards.getDashboardByProjectId pid did) \dashboardVM -> do
    dashboardM <- liftIO $ Dashboards.readDashboardFile "static/public/dashboards" (toString $ fromMaybe "_overview.yaml" dashboardVM.baseTemplate)
    whenJust dashboardM act


-- | OAuth install state/redirect param: @"projectId"@ or @"projectId__onboarding"@.
parseInstallState :: Maybe Text -> (Maybe Projects.ProjectId, Bool)
parseInstallState stateM = (Projects.projectIdFromText =<< viaNonEmpty head parts, length parts > 1)
  where
    parts = maybe [] (T.splitOn "__") stateM


-- | Post-install reply: onboarding resumes in the app, otherwise the
-- "installed" confirmation page.
installedResponse :: Text -> Projects.ProjectId -> Bool -> BWConfig -> Headers '[Header "Location" Text] BotResponse
installedResponse platform pid isOnboarding bwconf
  | isOnboarding = addHeader ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel") $ NoContent $ PageCtx bwconf ()
  | otherwise = addHeader "" $ BotLinked $ PageCtx bwconf (platform, Just pid)
