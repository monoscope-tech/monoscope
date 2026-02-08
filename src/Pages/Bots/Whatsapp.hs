module Pages.Bots.Whatsapp (whatsappIncomingPostH, TwilioWhatsAppMessage (..)) where

import Control.Lens ((?~))
import Control.Lens.Setter ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as KEYM
import Data.Aeson.KeyMap qualified as KEM
import Data.Effectful.Wreq qualified as Wreq
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful
import Effectful.Concurrent (forkIO)
import Effectful.Log qualified as Log
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Slack (getDashboardsForWhatsapp)
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (urlEncode)
import Network.Wreq
import Pages.Bots.Utils (BotType (..), QueryIntent (..), botEmoji, detectReportIntent, formatReportForWhatsApp, handleTableResponse, processAIQuery, processReportQuery)
import Pkg.AI qualified as AI
import Pkg.Components.TimePicker qualified as TP
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (idFromText)
import Pkg.Parser (parseQueryToAST)
import Relude
import System.Config (AuthContext, EnvConfig)
import System.Config qualified as Config
import System.Types (ATBaseCtx)
import Utils (toUriStr)
import Web.Internal.FormUrlEncoded


joiner :: Text
joiner = "___"


whatsappIncomingPostH :: TwilioWhatsAppMessage -> ATBaseCtx AE.Value
whatsappIncomingPostH val = do
  Log.logTrace ("WhatsApp interaction received" :: Text) $ AE.object ["from" AE..= val.from, "body" AE..= val.body]
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = authCtx.config
  let fromN = T.dropWhile (/= '+') val.from
  projectM <- Projects.getProjectByPhoneNumber fromN
  let bodyType = parseWhatsappBody val.body
  case projectM of
    Just p -> do
      case bodyType of
        DashboardLoad skip -> do
          dashboards' <- getDashboardsForWhatsapp fromN
          let dashboards = V.fromList $ map (\(k, v) -> (k, "dash" <> joiner <> v)) dashboards'
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
      dashboardVMM <- maybe (pure Nothing) Dashboards.getDashboardById (idFromText dashboardId)
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
      dashboardVMM <- maybe (pure Nothing) Dashboards.getDashboardById (idFromText dashboardId)
      case dashboardVMM of
        Nothing -> pass
        Just dashboardVM -> do
          dashboardM <- liftIO $ Dashboards.readDashboardFile "static/public/dashboards" (toString $ fromMaybe "_overview.yaml" dashboardVM.baseTemplate)
          whenJust dashboardM $ \dashboard -> do
            let widgetM = find (\w -> fromMaybe "Untitled-" w.title == widget) dashboard.widgets
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
      case detectReportIntent reqBody.body of
        ReportIntent reportType -> do
          reportResult <- processReportQuery project.id reportType envCfg
          case reportResult of
            Left err -> sendWhatsappResponse (AE.object []) reqBody.from envCfg.whatsappBotText (Just err)
            Right (report, _, _) -> sendWhatsappResponse (AE.object []) reqBody.from envCfg.whatsappBotText (Just $ formatReportForWhatsApp report project.id envCfg)
        GeneralQueryIntent -> do
          result <- processAIQuery project.id reqBody.body Nothing envCfg.openaiApiKey
          case result of
            Left _ -> sendWhatsappResponse (AE.object []) reqBody.from envCfg.whatsappBotText (Just $ botEmoji "error" <> " Something went wrong. Please try again.")
            Right resp -> do
              let (fromTimeM, toTimeM, rangeM) = maybe (Nothing, Nothing, Nothing) (TP.parseTimeRange now) resp.timeRange
                  (from, to) = fromMaybe ("", "") rangeM
                  query = fromMaybe "" resp.query
                  hasQuery = isJust resp.query
                  hasExplanation = isJust resp.explanation
              case (hasQuery, hasExplanation) of
                (False, True) -> sendWhatsappResponse (AE.object []) reqBody.from envCfg.whatsappBotText (Just $ fromMaybe "No insights available" resp.explanation)
                (True, False) -> handleWidgetResponse now reqBody envCfg project query resp.visualization from to fromTimeM toTimeM
                (True, True) -> do
                  handleWidgetResponse now reqBody envCfg project query resp.visualization from to fromTimeM toTimeM
                  whenJust resp.explanation $ sendWhatsappResponse (AE.object []) reqBody.from envCfg.whatsappBotText . Just
                (False, False) -> sendWhatsappResponse (AE.object []) reqBody.from envCfg.whatsappBotText (Just "No response available")

    handleWidgetResponse now reqBody envCfg project query visualization from to fromTimeM toTimeM = case visualization of
      Just vizType -> do
        let chartType = Widget.mapWidgetTypeToChartType $ Widget.mapChatTypeToWidgetType vizType
            opts = "time=" <> toUriStr (show now) <> "&q=" <> toUriStr query <> "&p=" <> toUriStr project.id.toText <> "&t=" <> toUriStr chartType <> "&from=" <> toUriStr from <> "&to=" <> toUriStr to
            query_url = project.id.toText <> "/log_explorer?viz_type=" <> chartType <> "&query=" <> toUriStr query
            content' = getBotContent reqBody.body query query_url opts
        _ <- sendWhatsappResponse content' reqBody.from envCfg.whatsappBotChart Nothing
        pass
      Nothing -> case parseQueryToAST query of
        Left _ -> sendWhatsappResponse (AE.object []) reqBody.from envCfg.whatsappBotText (Just $ botEmoji "warning" <> " Couldn't parse query. Try: 'show errors in last hour'")
        Right query' -> do
          tableAsVecE <- RequestDumps.selectLogTable project.id query' query Nothing (fromTimeM, toTimeM) [] Nothing Nothing
          let content = case handleTableResponse WhatsApp tableAsVecE envCfg project.id query of
                AE.Object o -> case KEM.lookup "body" o of
                  Just (AE.String c) -> c
                  _ -> "Error processing query"
                _ -> "Error processing query"
          sendWhatsappResponse (AE.object []) reqBody.from envCfg.whatsappBotText (Just content)


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
            ["dash", dashboardId, _] -> WidgetsLoad dashboardId 0
            ["widget", dashboardId, skip] -> WidgetsLoad dashboardId (defaultZero skip)
            ["widg", widgetTitle, dashboardId] -> WidgetSelect widgetTitle dashboardId
            ["widg", widgetTitle, dashboardId, _] -> WidgetSelect widgetTitle dashboardId
            _ -> Prompt body
  where
    defaultZero skip = fromMaybe 0 (readMaybe (toString skip))


getBotContent :: Text -> Text -> Text -> Text -> AE.Value
getBotContent question query query_url opts =
  AE.object ["1" AE..= ("*" <> question <> "*"), "2" AE..= ("`" <> query <> "`"), "3" AE..= opts, "4" AE..= query_url]


getWhatsappList :: Text -> Text -> V.Vector (Text, Text) -> Int -> AE.Value
getWhatsappList typ body vals' skip = AE.object $ ("1" AE..= body) : vars
  where
    vals = V.map (first (T.take 24)) (V.drop skip vals')
    paddedVals =
      let missing = 3 - V.length vals
          duplicates' = case vals V.!? 0 of
            Just v | V.length vals == 1 -> V.replicate missing v
            _ -> V.take missing vals
          duplicates = V.imap (\i (k, v) -> (k <> " " <> show (i + 1), v <> joiner <> show (i + 1))) duplicates'
       in if missing > 0
            then vals <> duplicates
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
      parseFieldDefault key def = case lookupMaybe key f of
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
  Log.logTrace ("WhatsApp response" :: Text) $ AE.object ["to" AE..= to, "template" AE..= template, "body" AE..= bodyM, "contentVariables" AE..= contentVariables]
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
