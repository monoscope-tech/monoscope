{-# LANGUAGE PackageImports #-}

module Pages.Bots.Utils (handleTableResponse, BotType (..), BotResponse (..), Channel (..), widgetPngUrl, authHeader, contentTypeHeader, AIQueryResult (..), processAIQuery, formatThreadsWithMemory, formatHistoryAsContext, verifyWidgetSignature, QueryIntent (..), ReportType (..), detectReportIntent, processReportQuery, formatReportForSlack, formatReportForDiscord, formatReportForWhatsApp) where

import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KEM
import Data.ByteArray qualified as BA
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.Effectful.Wreq (Options, header)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful (Eff, IOE, (:>))
import Effectful.Log (Log)
import Effectful.Time qualified as Time
import Langchain.LLM.Core qualified as LLM
import Langchain.Memory.Core (BaseMemory (..))
import Langchain.Memory.TokenBufferMemory (TokenBufferMemory (..))
import Lucid
import Models.Apis.Fields.Facets qualified as Facets
import Models.Apis.Reports qualified as Reports
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (urlEncode)
import Pages.BodyWrapper (PageCtx (..))
import Pages.Components (navBar)
import Pkg.AI qualified as AI
import Pkg.Components.Widget qualified as Widget
import Relude
import System.Config (EnvConfig (..))
import System.Logging qualified as Log
import System.Types (DB)
import Utils (faSprite_, getDurationNSMS, listToIndexHashMap, lookupVecBoolByKey, lookupVecIntByKey, lookupVecTextByKey)
import Utils qualified
import "cryptonite" Crypto.Hash (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC


data BotType = Discord | Slack | WhatsApp
  deriving (Eq, Show)


data BotResponse
  = BotLinked (PageCtx Text)
  | NoTokenFound (PageCtx ())
  | DiscordError (PageCtx ())
  | NoContent (PageCtx ())


instance ToHtml BotResponse where
  toHtml (BotLinked (PageCtx bwconf bot)) = toHtml $ PageCtx bwconf $ installedSuccess bot
  toHtml (DiscordError (PageCtx bwconf ())) = toHtml $ PageCtx bwconf discordError
  toHtml (NoTokenFound (PageCtx bwconf ())) = toHtml $ PageCtx bwconf noTokenFound
  toHtml (NoContent (PageCtx bwconf ())) = toHtml $ PageCtx bwconf ""
  toHtmlRaw = toHtml


authHeader :: Text -> Options -> Options
authHeader token = header "Authorization" .~ [encodeUtf8 $ "Bot " <> token]


contentTypeHeader :: Text -> Options -> Options
contentTypeHeader contentType = header "Content-Type" .~ [encodeUtf8 contentType]


handleTableResponse :: BotType -> Either Text (V.Vector (V.Vector AE.Value), [Text], Int) -> EnvConfig -> Projects.ProjectId -> Text -> AE.Value
handleTableResponse target tableAsVecE envCfg projectId query =
  case tableAsVecE of
    Left err -> case target of
      Discord -> AE.object ["content" AE..= "Error processing query"]
      WhatsApp -> AE.object ["body" AE..= "Error processing query"]
      Slack -> AE.object ["text" AE..= "Error processing query: "]
    Right tableAsVec -> do
      let (requestVecs, colNames, resultCount) = tableAsVec
          colIdxMap = listToIndexHashMap colNames
          tableData = recsVecToTableData requestVecs colIdxMap
          url' = envCfg.hostUrl <> "p/" <> projectId.toText <> "/log_explorer?query=" <> decodeUtf8 (urlEncode True $ encodeUtf8 query)
          explorerLink = "[Open in log explorer](" <> url' <> ")"
          content = "**Total events (" <> show resultCount <> ")**\n**Query used:** " <> query <> "\n\n" <> tableData <> "\n"
       in case target of
            Discord -> AE.object ["content" AE..= (content <> explorerLink)]
            WhatsApp -> AE.object ["body" AE..= (content <> url')]
            Slack ->
              AE.object
                [ "blocks"
                    AE..= AE.Array
                      ( V.fromList
                          [ AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("Total events (*" <> show resultCount <> "*)")]]
                          , AE.object ["type" AE..= "context", "elements" AE..= AE.Array (V.fromList [AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Query used:* " <> query)]])]
                          , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= tableData]]
                          , AE.object
                              [ "type" AE..= "actions"
                              , "elements" AE..= AE.Array (V.fromList [AE.object ["type" AE..= "button", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "🔍 View in log explorer", "emoji" AE..= True], "url" AE..= url']])
                              ]
                          ]
                      )
                , "response_type" AE..= "in_channel"
                , "replace_original" AE..= True
                , "delete_original" AE..= True
                ]


recsVecToTableData :: V.Vector (V.Vector AE.Value) -> HashMap Text Int -> Text
recsVecToTableData recsVec colIdxMap =
  formatSpans
    $ map
      ( \v ->
          TableData
            { timestamp = fromMaybe "" $ lookupVecTextByKey v colIdxMap "timestamp"
            , servicename = fromMaybe "" $ lookupVecTextByKey v colIdxMap "service"
            , spanname = fromMaybe "" $ lookupVecTextByKey v colIdxMap "span_name"
            , duration = toText $ getDurationNSMS $ fromIntegral $ lookupVecIntByKey v colIdxMap "duration"
            , hasErrors = lookupVecBoolByKey v colIdxMap "errors"
            }
      )
      (V.toList (V.take 15 recsVec))


padRight :: Int -> Text -> Text
padRight n s = T.take n (s <> T.replicate n " ")


formatSpanRow :: TableData -> Text
formatSpanRow spn = padRight 18 spn.timestamp <> " " <> padRight 15 spn.servicename <> " " <> padRight 20 spn.spanname <> " " <> padRight 8 spn.duration <> " " <> (if spn.hasErrors then "❌" else "✅")


formatSpans :: [TableData] -> Text
formatSpans spans =
  let hd = padRight 20 "TIME" <> " " <> padRight 15 "SERVICE" <> " " <> padRight 20 "SPAN NAME" <> " " <> padRight 8 "DURATION" <> " STATUS"
      rows = map formatSpanRow spans
   in "```\n" <> unlines (hd : rows) <> "```"


-- | Construct signed URL for widget PNG endpoint. Logs error and returns empty string if URL exceeds 8000 chars.
widgetPngUrl :: (IOE :> es, Log :> es) => Text -> Text -> Projects.ProjectId -> Widget.Widget -> Maybe Text -> Maybe Text -> Maybe Text -> Eff es Text
widgetPngUrl secret hostUrl pid widget since from to =
  let widgetJson = decodeUtf8 @Text $ toStrict $ AE.encode widget
      encodedJson = decodeUtf8 @Text $ urlEncode True $ encodeUtf8 widgetJson
      sig = signWidgetUrl secret pid widgetJson
      timeParams = foldMap (\(k, mv) -> maybe "" (\v -> "&" <> k <> "=" <> v) mv) ([("since", since), ("from", from), ("to", to)] :: [(Text, Maybe Text)])
      url = hostUrl <> "p/" <> pid.toText <> "/widget.png?widgetJSON=" <> encodedJson <> timeParams <> "&sig=" <> sig
   in if T.length url > 8000
        then Log.logAttention "Widget PNG URL too large" (AE.object ["projectId" AE..= pid, "urlLength" AE..= T.length url]) >> pure ""
        else pure url


data TableData = TableData
  { timestamp :: Text
  , servicename :: Text
  , spanname :: Text
  , duration :: Text
  , hasErrors :: Bool
  }
  deriving (Generic, Show)


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
    faSprite_ "circle-exclamation" "solid" "text-textError h-10 w-10"
    h3_ [class_ "text-4xl font-bold my-6 text-textError"] "Uh-oh! Something went wrong"
    p_
      [class_ "text-xl text-textStrong text-center max-w-prose mb-4"]
      "We hit a snag while trying to install the Discord bot. Don’t worry — it happens!"
    p_
      [class_ "text-md text-textWeak text-center max-w-prose"]
      "This could be due to not adding discord from the integrations page, click on add to discord on the integrations page to try again."


installedSuccess :: Text -> Html ()
installedSuccess botPlatform = do
  navBar
  section_ [class_ "min-h-screen  flex flex-col justify-center"] do
    div_ [class_ "max-w-4xl mx-auto px-4"] do
      div_ [class_ "bg-bgBase border border-strokeWeak rounded-3xl border border-fillWeak overflow-hidden"] do
        div_ [class_ "bg-gradient-to-r from-fillSuccess-weak to-fillBrand-weak px-8 py-10 text-center"] do
          div_ [class_ "inline-flex items-center justify-center w-16 h-16 bg-bgRaised rounded-full mb-4 shadow-lg"] do
            faSprite_ "check" "regular" "h-8 w-8 text-textSuccess"
          h1_ [class_ "text-3xl font-semibold text-white mb-4"] "Installation Complete!"
          p_ [class_ "text-textBrand font-semibold max-w-2xl mx-auto"] $ toHtml $ "Monoscope Bot has been successfully added to your " <> botPlatform <> " server"
        div_ [class_ "px-8 py-12"] do
          div_ [class_ "text-center mb-12"] do
            h2_ [class_ "font-semibold text-textStrong mb-4"] "You're All Set! 🚀"
            p_ [class_ "text-textWeak text-sm mx-auto max-w-2xl "] $ toHtml $ "Start receiving real-time alerts and interact with your API data directly from " <> botPlatform <> ". Your team can now stay on top of API performance without leaving your chat."
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


data AIQueryResult = AIQueryResult
  { query :: Text
  , visualization :: Maybe Text
  , fromTime :: Maybe UTCTime
  , toTime :: Maybe UTCTime
  , timeRangeStr :: (Text, Text)
  }
  deriving (Generic, Show)


processAIQuery :: (DB es, Log :> es, Time.Time :> es) => Projects.ProjectId -> Text -> Maybe Text -> Text -> Eff es (Either Text AIQueryResult)
processAIQuery pid userQuery threadCtx apiKey = do
  now <- Time.currentTime
  let dayAgo = addUTCTime (-86400) now
  facetSummaryM <- Facets.getFacetSummary pid "otel_logs_and_spans" dayAgo now
  let config = (AI.defaultAgenticConfig pid){AI.facetContext = facetSummaryM, AI.customContext = threadCtx}
  result <- AI.runAgenticQuery config userQuery apiKey
  pure $ result <&> \AI.ChatLLMResponse{..} ->
    let from' = timeRange >>= viaNonEmpty head
        to' = timeRange >>= viaNonEmpty last
        (fromT, toT, rangeM) = Utils.parseTime from' to' Nothing now
        (from, to) = fromMaybe ("", "") rangeM
     in AIQueryResult{query, visualization, fromTime = fromT, toTime = toT, timeRangeStr = (from, to)}


formatThreadsWithMemory :: Int -> Text -> [LLM.Message] -> IO Text
formatThreadsWithMemory maxTokens platform msgs = case nonEmpty msgs of
  Nothing -> pure ""
  Just history -> do
    let memory = TokenBufferMemory{maxTokens, tokenBufferMessages = history}
    result <- messages memory
    pure $ case result of
      Left _ -> ""
      Right trimmedHistory -> formatHistoryAsContext platform (NE.toList trimmedHistory)


formatHistoryAsContext :: Text -> [LLM.Message] -> Text
formatHistoryAsContext platform msgs =
  unlines
    [ "\n\nTHREADS:"
    , "- this query is part of a " <> platform <> " conversation thread. Use previous messages for additional context if needed."
    , "- the user query is the main one to answer, but earlier messages may contain important clarifications or parameters."
    , "\nPrevious thread messages:\n"
    , T.intercalate "\n" $ map formatMessage msgs
    ]
  where
    formatMessage m = "[" <> show (LLM.role m) <> "] " <> LLM.content m


signWidgetUrl :: Text -> Projects.ProjectId -> Text -> Text
signWidgetUrl secret pid widgetJson =
  let payload = pid.toText <> ":" <> widgetJson
   in decodeUtf8 @Text $ B16.encode $ BA.convert (HMAC.hmac (encodeUtf8 secret :: ByteString) (encodeUtf8 payload :: ByteString) :: HMAC.HMAC SHA256)


verifyWidgetSignature :: Text -> Projects.ProjectId -> Text -> Maybe Text -> Either LBS.ByteString ()
verifyWidgetSignature secret pid widgetJson = \case
  Nothing -> Left "Missing signature"
  Just sig -> let expected = signWidgetUrl secret pid widgetJson in if BA.constEq (encodeUtf8 sig :: ByteString) (encodeUtf8 expected :: ByteString) then Right () else Left "Invalid signature"


-- | Report query intent detection
data ReportType = DailyReport | WeeklyReport deriving (Eq, Show)


data QueryIntent = ReportIntent ReportType | GeneralQueryIntent deriving (Eq, Show)


-- | Detect if user query is requesting a report. Requires action verb + "report/summary".
detectReportIntent :: Text -> QueryIntent
detectReportIntent query =
  let q = T.toLower $ T.strip query
      hasActionVerb = any (`T.isInfixOf` q) ["send", "get", "show", "give", "fetch", "retrieve"]
      hasReportWord = any (`T.isInfixOf` q) ["report", "summary"]
      isWeekly = any (`T.isInfixOf` q) ["weekly", "week"]
   in if hasActionVerb && hasReportWord
        then ReportIntent (if isWeekly then WeeklyReport else DailyReport)
        else GeneralQueryIntent


-- | Process report query - retrieves latest report from DB
processReportQuery :: (DB es, IOE :> es, Log :> es) => Projects.ProjectId -> ReportType -> EnvConfig -> Eff es (Either Text (Reports.Report, Text, Text))
processReportQuery pid reportType envCfg = do
  let typeTxt = case reportType of DailyReport -> "daily"; WeeklyReport -> "weekly"
  reportM <- Reports.getLatestReportByType pid typeTxt
  case reportM of
    Nothing -> pure $ Left $ "No " <> typeTxt <> " report found. Reports are generated automatically on schedule."
    Just report -> do
      let startTxt = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" report.startTime
          endTxt = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" report.endTime
      eventsUrl <- widgetPngUrl envCfg.apiKeyEncryptionSecretKey envCfg.hostUrl pid def{Widget.wType = Widget.WTTimeseries, Widget.query = Just "summarize count(*) by bin_auto(timestamp), status_code"} Nothing (Just startTxt) (Just endTxt)
      errorsUrl <- widgetPngUrl envCfg.apiKeyEncryptionSecretKey envCfg.hostUrl pid def{Widget.wType = Widget.WTTimeseries, Widget.query = Just "status_code == \"ERROR\" | summarize count(*) by bin_auto(timestamp), status_code", Widget.theme = Just "roma"} Nothing (Just startTxt) (Just endTxt)
      pure $ Right (report, eventsUrl, errorsUrl)


-- | Format report for Slack
formatReportForSlack :: Reports.Report -> Projects.ProjectId -> EnvConfig -> Text -> Text -> Text -> AE.Value
formatReportForSlack report pid envCfg eventsUrl errorsUrl channelId =
  let reportUrl = envCfg.hostUrl <> "p/" <> pid.toText <> "/reports/" <> report.id.toText
      startTxt = T.take 10 $ toText $ formatTime defaultTimeLocale "%Y-%m-%d" report.startTime
      endTxt = T.take 10 $ toText $ formatTime defaultTimeLocale "%Y-%m-%d" report.endTime
      (totalEvents, totalErrors) = parseReportStats report.reportJson
   in AE.object
        [ "blocks"
            AE..= AE.Array
              ( V.fromList
                  [ AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("<" <> reportUrl <> "|" <> T.toTitle report.reportType <> " Report>")]]
                  , AE.object ["type" AE..= "context", "elements" AE..= AE.Array (V.fromList [AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*From:* " <> startTxt)], AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*To:* " <> endTxt)]])]
                  , AE.object ["type" AE..= "image", "image_url" AE..= eventsUrl, "alt_text" AE..= "Events chart", "title" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= ("Total Events: " <> show totalEvents)]]
                  , AE.object ["type" AE..= "image", "image_url" AE..= errorsUrl, "alt_text" AE..= "Errors chart", "title" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= ("Total Errors: " <> show totalErrors)]]
                  , AE.object ["type" AE..= "actions", "elements" AE..= AE.Array (V.fromList [AE.object ["type" AE..= "button", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "View Full Report"], "url" AE..= reportUrl]])]
                  ]
              )
        , "response_type" AE..= "in_channel"
        , "replace_original" AE..= True
        , "delete_original" AE..= True
        ]


-- | Format report for Discord
formatReportForDiscord :: Reports.Report -> Projects.ProjectId -> EnvConfig -> Text -> Text -> AE.Value
formatReportForDiscord report pid envCfg eventsUrl errorsUrl =
  let reportUrl = envCfg.hostUrl <> "p/" <> pid.toText <> "/reports/" <> report.id.toText
      startTxt = T.take 10 $ toText $ formatTime defaultTimeLocale "%Y-%m-%d" report.startTime
      endTxt = T.take 10 $ toText $ formatTime defaultTimeLocale "%Y-%m-%d" report.endTime
      (totalEvents, totalErrors) = parseReportStats report.reportJson
   in AE.object
        [ "flags" AE..= (32768 :: Int)
        , "components"
            AE..= AE.Array
              ( V.fromList
                  [ AE.object ["type" AE..= (10 :: Int), "content" AE..= ("## 📊 " <> T.toTitle report.reportType <> " Report")]
                  , AE.object ["type" AE..= (10 :: Int), "content" AE..= ("**From:** " <> startTxt <> "  **To:** " <> endTxt)]
                  , AE.object ["type" AE..= (10 :: Int), "content" AE..= ("Total Events: **" <> show totalEvents <> "**" <> T.replicate 20 " " <> "Total Errors: **" <> show totalErrors <> "**")]
                  , AE.object ["type" AE..= (12 :: Int), "items" AE..= AE.Array (V.fromList [AE.object ["media" AE..= AE.object ["url" AE..= eventsUrl, "description" AE..= "Events"]], AE.object ["media" AE..= AE.object ["url" AE..= errorsUrl, "description" AE..= "Errors"]]])]
                  , AE.object ["type" AE..= (1 :: Int), "components" AE..= AE.Array (V.fromList [AE.object ["type" AE..= (2 :: Int), "label" AE..= "Open report", "url" AE..= reportUrl, "style" AE..= (5 :: Int)]])]
                  ]
              )
        ]


-- | Format report for WhatsApp
formatReportForWhatsApp :: Reports.Report -> Projects.ProjectId -> EnvConfig -> Text
formatReportForWhatsApp report pid envCfg =
  let reportUrl = envCfg.hostUrl <> "p/" <> pid.toText <> "/reports/" <> report.id.toText
      startTxt = T.take 10 $ toText $ formatTime defaultTimeLocale "%Y-%m-%d" report.startTime
      endTxt = T.take 10 $ toText $ formatTime defaultTimeLocale "%Y-%m-%d" report.endTime
      (totalEvents, totalErrors) = parseReportStats report.reportJson
   in "📊 " <> T.toTitle report.reportType <> " Report\nPeriod: " <> startTxt <> " - " <> endTxt <> "\nTotal Events: " <> show totalEvents <> "\nTotal Errors: " <> show totalErrors <> "\nView: " <> reportUrl


-- | Parse total events and errors from report JSON
parseReportStats :: AE.Value -> (Int, Int)
parseReportStats json = case json of
  AE.Object o ->
    let getTotal key = case KEM.lookup (fromText key) o of
          Just (AE.Object inner) -> case KEM.lookup "total" inner of
            Just (AE.Number n) -> round n
            _ -> 0
          _ -> 0
     in (getTotal "events", getTotal "errors")
  _ -> (0, 0)
