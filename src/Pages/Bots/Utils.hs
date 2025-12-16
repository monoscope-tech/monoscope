module Pages.Bots.Utils (handleTableResponse, BotType (..), BotResponse (..), chartImageUrl, authHeader, contentTypeHeader) where

import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Effectful.Wreq (header)
import Data.Effectful.Wreq qualified
import Data.Text qualified as T
import Data.Time qualified as Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Vector qualified as V
import Lucid
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (urlEncode)
import Pages.BodyWrapper (PageCtx (..))
import Pages.Components (navBar)
import Relude
import System.Config (EnvConfig (..))
import Utils (faSprite_, getDurationNSMS, listToIndexHashMap, lookupVecBoolByKey, lookupVecIntByKey, lookupVecTextByKey)


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


authHeader :: Text -> Data.Effectful.Wreq.Options -> Data.Effectful.Wreq.Options
authHeader token = header "Authorization" .~ [encodeUtf8 $ "Bot " <> token]


contentTypeHeader :: Text -> Data.Effectful.Wreq.Options -> Data.Effectful.Wreq.Options
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
    div_ [class_ "max-w-4xl mx-auto px-6"] do
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
            div_ [class_ "grid gap-6 md:grid-cols-2"] do
              div_ [class_ "bg-gradient-to-br from-fillWeaker to-fillBrand-weak rounded-2xl p-6 border border-strokeBrand-weak"] do
                div_ [class_ "flex items-start space-x-4"] do
                  div_ [class_ "flex-shrink-0"] do
                    div_ [class_ "inline-flex items-center justify-center w-12 h-12 bg-fillBrand-strong rounded-xl"] do
                      span_ [class_ "text-white font-bold text-lg"] "?"
                  div_ [class_ "flex-1"] do
                    div_ [class_ "flex items-center space-x-2 mb-3"] do
                      span_ [class_ "font-mono bg-fillBrand-weak text-textBrand px-3 py-1 rounded-lg font-semibold"] "/ask"
                      span_ [class_ "bg-fillBrand-strong text-white text-xs px-2 py-1 rounded-full"] "AI Powered"
                    p_ [class_ "text-textStrong text-sm"] $ toHtml $ "Ask questions about your API in natural language and get instant insights with logs and charts delivered right to " <> botPlatform <> "."
              div_ [class_ "bg-gradient-to-br from-fillSuccess-weak to-fillWeaker rounded-2xl p-6 border border-strokeSuccess-weak"] do
                div_ [class_ "flex items-start space-x-4"] do
                  div_ [class_ "flex-shrink-0"] do
                    div_ [class_ "inline-flex items-center justify-center w-12 h-12 bg-fillSuccess-strong rounded-xl"] do
                      faSprite_ "bell" "regular" "h-6 w-6 text-white"
                  div_ [class_ "flex-1"] do
                    div_ [class_ "flex items-center space-x-2 mb-3"] do
                      span_ [class_ "font-mono bg-fillSuccess-weak text-textSuccess px-3 py-1 rounded-lg font-semibold"] "/here"
                      span_ [class_ "bg-fillSuccess-strong text-white text-xs px-2 py-1 rounded-full"] "Alerts"
                    p_ [class_ "text-textStrong text-sm"]
                      "Set up this channel to receive automated error reports, weekly summaries, and daily performance alerts."
