module Pages.Bots.Utils (handleTableResponse, BotType (..), BotResponse (..), Channel (..), chartImageUrl, chartScreenshotUrl, renderWidgetToChartUrl, authHeader, contentTypeHeader, AIQueryResult (..), processAIQuery, formatThreadsWithMemory, formatHistoryAsContext) where

import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KEMP
import Data.Effectful.Wreq (HTTP, Options, defaults, header, postWith)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.Time qualified as Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time
import Langchain.LLM.Core qualified as LLM
import Langchain.Memory.Core (BaseMemory (..))
import Langchain.Memory.TokenBufferMemory (TokenBufferMemory (..))
import Lucid
import Models.Apis.Fields.Facets qualified as Facets
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Client.Internal (responseBody, responseStatus)
import Network.HTTP.Types (urlEncode)
import Network.HTTP.Types.Status (statusCode)
import Pages.BodyWrapper (PageCtx (..))
import Pages.Charts.Charts qualified as Charts
import Pages.Components (navBar)
import Pkg.AI qualified as AI
import Pkg.Components.Widget qualified as Widget
import Relude
import Servant.Server (ServerError)
import System.Config (AuthContext, EnvConfig (..))
import System.Logging qualified as Log
import System.Types (DB)
import Utils (faSprite_, getDurationNSMS, listToIndexHashMap, lookupVecBoolByKey, lookupVecIntByKey, lookupVecTextByKey)
import Utils qualified


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


chartImageUrl :: Text -> Text -> Time.UTCTime -> Text
chartImageUrl options baseUrl now =
  let timeMs = show $ floor (utcTimeToPOSIXSeconds now * 1000)
   in baseUrl <> "?time=" <> timeMs <> options


-- | Render a widget to PNG via chartshot and return the image URL
-- Uses widget.theme for theming (defaults to "default" if not set)
-- Returns empty string on failure (graceful degradation for bots)
chartScreenshotUrl :: (HTTP :> es, IOE :> es, Log :> es) => Widget.Widget -> Text -> Eff es Text
chartScreenshotUrl widget chartShotBaseUrl
  | not (T.isPrefixOf "http" chartShotBaseUrl) = do
      Log.logAttention "chartScreenshotUrl: invalid URL" $ AE.object ["url" AE..= chartShotBaseUrl]
      pure ""
  | otherwise = do
      let echartsOpts = Widget.widgetToECharts widget
          body = AE.object ["echarts" AE..= echartsOpts, "width" AE..= (900 :: Int), "height" AE..= (300 :: Int), "theme" AE..= fromMaybe "default" widget.theme]
          url = chartShotBaseUrl <> "/render"
      resp <- postWith defaults (toString url) body
      let respBody = responseBody resp
          status = statusCode $ responseStatus resp :: Int
      if status /= 200
        then do
          Log.logAttention "chartScreenshotUrl: chartshot returned non-200" $ AE.object ["url" AE..= url, "status" AE..= status, "body" AE..= (decodeUtf8 respBody :: Text)]
          pure ""
        else case AE.decode respBody of
          Just (AE.Object o) | Just (AE.String imgUrl) <- KEMP.lookup "url" o -> pure imgUrl
          _ -> do
            Log.logAttention "chartScreenshotUrl: failed to parse response (expected {url: string})" $ AE.object ["url" AE..= url, "body" AE..= (decodeUtf8 respBody :: Text)]
            pure ""


-- | Render widget to chart URL, fetching data if needed (eager-aware)
renderWidgetToChartUrl :: (DB es, Effectful.Reader.Static.Reader AuthContext :> es, Error ServerError :> es, HTTP :> es, Log :> es, Time.Time :> es) => Widget.Widget -> Projects.ProjectId -> Text -> Text -> Text -> Eff es Text
renderWidgetToChartUrl widget pid from to chartShotUrl = do
  widget' <- case widget.dataset of
    Just _ -> pure widget
    Nothing -> do
      metricsD <- Charts.queryMetrics (Just Charts.DTMetric) (Just pid) widget.query Nothing Nothing (Just from) (Just to) Nothing []
      pure $ widget{Widget.dataset = Just $ Widget.toWidgetDataset metricsD}
  chartScreenshotUrl widget' chartShotUrl


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
                      span_ [class_ "monospace bg-fillBrand-weak text-textBrand px-3 py-1 rounded-lg font-semibold"] "/ask"
                      span_ [class_ "bg-fillBrand-strong text-white text-xs px-2 py-1 rounded-full"] "AI Powered"
                    p_ [class_ "text-textStrong text-sm"] $ toHtml $ "Ask questions about your API in natural language and get instant insights with logs and charts delivered right to " <> botPlatform <> "."
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
