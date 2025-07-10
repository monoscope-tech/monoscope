module Pages.Bots.Whatsapp (whatsappIncomingPostH, TwilioWhatsAppMessage) where

import Control.Applicative ((<|>))
import Control.Lens ((?~))
import Control.Lens.Setter ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as KEYM
import Data.Aeson.KeyMap qualified as KEM
import Data.Effectful.Wreq qualified as Wreq
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time qualified as Time
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful
import Effectful.Concurrent (forkIO)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Slack (getDashboardsForWhatsapp)
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (urlEncode)
import Network.Wreq
import Network.Wreq.Types (FormParam)
import Pages.Bots.Utils (BotType (..), chartImageUrl, handleTableResponse)
import Pkg.AI (callOpenAIAPI, systemPrompt)
import Pkg.Components.Widget qualified as Widget
import Pkg.Parser (parseQueryToAST)
import Relude
import System.Config (AuthContext, EnvConfig, env)
import System.Config qualified as Config
import System.Types (ATBaseCtx)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (FromHttpApiData (..))
import Web.Internal.FormUrlEncoded


joiner :: Text
joiner = "___"


whatsappIncomingPostH :: TwilioWhatsAppMessage -> ATBaseCtx AE.Value
whatsappIncomingPostH val = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = authCtx.config
  let fromN = T.dropWhile (/= '+') val.from
  projectM <- dbtToEff $ Projects.getProjectByPhoneNumber fromN
  let bodyType = parseWhatsappBody val.body
  case projectM of
    Just p -> do
      case bodyType of
        DashboardLoad skip -> do
          dashboards' <- getDashboardsForWhatsapp fromN
          let dashboards = V.map (\(k, v) -> (k, "dash" <> joiner <> v)) dashboards'
          let contentVars = getWhatsappList "dashboard" "Please select a dashboard" dashboards 0
          sendWhatsappResponse contentVars val.from envCfg.whatsappDashboardList Nothing
        WidgetsLoad dashboardId skip -> handleDashboard dashboardId skip val p envCfg
        WidgetSelect widgetTitle dashboardId -> handleWidget widgetTitle dashboardId val p envCfg
        Prompt _ -> void $ forkIO $ handlePrompt val envCfg p
      pure $ AE.object []
    _ -> pure $ AE.object []
  where
    handleDashboard :: Text -> Int -> TwilioWhatsAppMessage -> Projects.Project -> EnvConfig -> ATBaseCtx ()
    handleDashboard dashboardId skip v p envCfg = do
      dashboardVMM <- Dashboards.getDashboardById dashboardId
      case dashboardVMM of
        Nothing -> pass
        Just dashboardVM -> do
          dashboardM <- liftIO $ Dashboards.readDashboardFile "static/public/dashboards" (toString $ fromMaybe "_overview.yaml" dashboardVM.baseTemplate)
          whenJust dashboardM $ \dashboard -> do
            let widgets' = (\w -> (fromMaybe "Untitled-" w.title, fromMaybe "Untitled-" w.title)) <$> dashboard.widgets
                widgets = V.fromList $ (\(k, id') -> (k, "widg" <> joiner <> id' <> joiner <> dashboardId)) <$> widgets'
            let contentVars = getWhatsappList ("widget" <> joiner <> dashboardId) "Please select a widget" widgets skip
            sendWhatsappResponse contentVars val.from envCfg.whatsappDashboardList Nothing
            pass

    handleWidget :: Text -> Text -> TwilioWhatsAppMessage -> Projects.Project -> EnvConfig -> ATBaseCtx ()
    handleWidget widget dashboardId reqBody project envCfg = do
      let widgetTitle = getWidgetTitle widget
      dashboardVMM <- Dashboards.getDashboardById dashboardId
      case dashboardVMM of
        Nothing -> pass
        Just dashboardVM -> do
          dashboardM <- liftIO $ Dashboards.readDashboardFile "static/public/dashboards" (toString $ fromMaybe "_overview.yaml" dashboardVM.baseTemplate)
          whenJust dashboardM $ \dashboard -> do
            let widgetM = find (\w -> (fromMaybe "Untitled-" w.title) == widgetTitle) dashboard.widgets
            whenJust widgetM $ \w -> do
              now <- Time.currentTime
              let widgetQuery = "&widget=" <> decodeUtf8 (urlEncode True (toStrict $ AE.encode $ AE.toJSON w))
                  opts = "time=" <> decodeUtf8 (urlEncode True (encodeUtf8 $ show now)) <> "&p=" <> project.id.toText <> widgetQuery
                  query_url = project.id.toText <> "/dashboards"
                  content' = getBotContent reqBody.body widget query_url opts
              _ <- sendWhatsappResponse content' reqBody.from envCfg.whatsappBotChart Nothing
              pass

    handlePrompt :: TwilioWhatsAppMessage -> EnvConfig -> Projects.Project -> ATBaseCtx ()
    handlePrompt reqBody envCfg project = do
      now <- Time.currentTime
      let fullPrompt = systemPrompt <> "\n\nUser query: " <> reqBody.body
      result <- liftIO $ callOpenAIAPI fullPrompt envCfg.openaiApiKey
      case result of
        Left err -> do
          _ <- sendWhatsappResponse (AE.object []) reqBody.from envCfg.whatsappBotChart (Just "Sorry, I couldn't proess your request")
          pass
        Right (query, vizTypeM) -> do
          case vizTypeM of
            Just vizType -> do
              let chartType = Widget.mapWidgetTypeToChartType $ Widget.mapChatTypeToWidgetType vizType
                  opts = "time=" <> decodeUtf8 (urlEncode True (encodeUtf8 $ show now)) <> "&q=" <> decodeUtf8 (urlEncode True (encodeUtf8 query)) <> "&p=" <> project.id.toText <> "&t=" <> chartType
                  query_url = project.id.toText <> "/log_explorer?viz_type=" <> chartType <> "&query=" <> (decodeUtf8 $ urlEncode True (encodeUtf8 query))
                  content' = getBotContent reqBody.body query query_url opts
              _ <- sendWhatsappResponse content' reqBody.from envCfg.whatsappBotChart Nothing
              pass
            Nothing -> do
              let queryAST = parseQueryToAST query
              case queryAST of
                Left err -> sendWhatsappResponse (AE.object []) reqBody.from envCfg.whatsappBotChart (Just "Error processing query")
                Right query' -> do
                  tableAsVecE <- RequestDumps.selectLogTable project.id query' query Nothing (Nothing, Nothing) [] Nothing Nothing
                  let content = case handleTableResponse WhatsApp tableAsVecE envCfg project.id query of
                        AE.Object o -> case KEM.lookup "body" o of
                          Just (AE.String c) -> c
                          _ -> "Error processing query"
                        _ -> "Error processing query"
                  sendWhatsappResponse (AE.object []) reqBody.from envCfg.whatsappBotText (Just content)
              pass


data BodyType
  = WidgetsLoad Text Int
  | WidgetSelect Text Text
  | DashboardLoad Int
  | Prompt Text


parseWhatsappBody :: Text -> BodyType
parseWhatsappBody body =
  case body of
    "/dashboard" -> DashboardLoad 0
    _ ->
      let parts = T.splitOn joiner body
       in case parts of
            ["dashboard", skip] -> DashboardLoad $ defaultZero skip
            ["dash", dashboardId] -> WidgetsLoad dashboardId 0
            ["widget", dashboardId, skip] -> WidgetsLoad dashboardId (defaultZero skip)
            ["widg", widgetTitle, dashboardId] -> WidgetSelect widgetTitle dashboardId
            _ -> Prompt body
  where
    defaultZero skip = fromMaybe 0 (readMaybe (toString skip))


getWidgetTitle :: Text -> Text
getWidgetTitle "Requests latency (ms)" = "Incoming request latency (ms)"
getWidgetTitle "HTTP Requests by endpoint" = "Incoming Requests by Endpoint"
getWidgetTitle "HTTP Errors (Incoming)" = "HTTP Errors (Incoming)"
getWidgetTitle _ = "All requests (Incoming)"


getBotContent :: Text -> Text -> Text -> Text -> AE.Value
getBotContent question query query_url opts =
  AE.object ["1" AE..= ("*" <> question <> "*"), "2" AE..= ("`" <> query <> "`"), "3" AE..= opts, "4" AE..= query_url]


getWhatsappList :: Text -> Text -> V.Vector (Text, Text) -> Int -> AE.Value
getWhatsappList typ body vals' skip = AE.object $ ["1" AE..= body] ++ vars
  where
    vals = V.map (\(k, v) -> (T.take 24 k, v)) (V.drop skip vals')
    paddedVals =
      let missing = 3 - V.length vals
          placeholders =
            V.generate
              missing
              ( \i ->
                  let label = "Placeholder " <> toText (show (i + 1))
                   in (label, label)
              )
       in if missing > 0
            then vals <> placeholders
            else vals
    vars
      | V.length vals > 3 =
          let firstTwo = V.take 2 paddedVals
              loadMore = ("Load More", typ <> joiner <> show (skip + 2))
              buttonPairs = V.toList $ V.imap (\i (k, v) -> [key (2 * i + 2) AE..= k, key (2 * i + 3) AE..= v]) firstTwo
              flattened = concat buttonPairs
           in flattened ++ ["6" AE..= fst loadMore, "7" AE..= snd loadMore]
      | otherwise =
          let buttonPairs = V.toList $ V.imap (\i (k, v) -> [key (2 * i + 2) AE..= k, key (2 * i + 3) AE..= v]) paddedVals
           in concat buttonPairs

    key :: Int -> AE.Key
    key = KEYM.fromText . toText . show


data TwilioWhatsAppMessage = TwilioWhatsAppMessage
  { messageSid :: Text
  , smsSid :: Text
  , smsMessageSid :: Text
  , accountSid :: Text
  , messagingServiceSid :: Maybe Text
  , from :: Text
  , to :: Text
  , body :: Text
  , numMedia :: Int
  , numSegments :: Int
  , profileName :: Maybe Text
  , waId :: Maybe Text
  , forwarded :: Maybe Bool
  , frequentlyForwarded :: Maybe Bool
  , buttonText :: Maybe Text
  }
  deriving (Eq, Generic, Show)


instance FromForm TwilioWhatsAppMessage where
  fromForm f =
    TwilioWhatsAppMessage
      <$> parseUnique' "MessageSid"
      <*> parseUnique' "SmsSid"
      <*> parseUnique' "SmsMessageSid"
      <*> parseUnique' "AccountSid"
      <*> parseOptional "MessagingServiceSid"
      <*> parseUnique' "From"
      <*> parseUnique' "To"
      <*> parseUnique' "Body"
      <*> parseFieldDefault "NumMedia" 0
      <*> parseFieldDefault "NumSegments" 1
      <*> parseOptional "ProfileName"
      <*> parseOptional "WaId"
      <*> parseOptionalBool "Forwarded"
      <*> parseOptionalBool "FrequentlyForwarded"
      <*> parseOptional "ButtonText"
    where
      parseUnique' key = case lookupMaybe key f of
        Right (Just v) -> pure v
        Right _ -> fail $ "Missing field: " ++ toString key
        Left e -> fail $ toString e
      parseOptional key = case lookupMaybe key f of
        Right v -> pure v
        _ -> pure Nothing
      parseFieldDefault key def = case (lookupMaybe key f) of
        Right v -> pure $ maybe def (fromMaybe def . readMaybeText) v
        Left e -> fail $ toString e
      parseOptionalBool key = case lookupMaybe key f of
        Right v -> pure $ parseBool =<< v
        Left e -> fail $ toString e
      readMaybeText :: Read a => Text -> Maybe a
      readMaybeText = readMaybe . toString
      parseBool :: Text -> Maybe Bool
      parseBool t =
        case t of
          "true" -> Just True
          _ -> Just False


sendWhatsappResponse :: AE.Value -> Text -> Text -> Maybe Text -> ATBaseCtx ()
sendWhatsappResponse contentVariables to template bodyM = do
  appCtx <- Effectful.Reader.Static.ask @AuthContext
  let from = appCtx.config.whatsappFromNumber
      accountSid = appCtx.config.twilioAccountSid
      token = appCtx.config.twilioAuthToken
      opts = defaults & header "Content-Type" .~ ["application/x-www-form-urlencoded"] & auth ?~ basicAuth (encodeUtf8 accountSid) (encodeUtf8 token)
      url = toString $ "https://api.twilio.com/2010-04-01/Accounts/" <> accountSid <> "/Messages.json"
      variables = toStrict $ AE.encode contentVariables
      payload :: [FormParam]
      payload = ["To" := to, "From" := ("whatsapp:" <> from)] <> maybe ["ContentSid" := template, "ContentVariables" := variables] (\x -> ["Body" := x]) bodyM
  _ <- Wreq.postWith opts url payload
  pass
