module Pages.Bots.Whatsapp (whatsappIncomingPostH, TwilioWhatsAppMessage) where

import Control.Applicative ((<|>))
import Control.Lens ((?~))
import Control.Lens.Setter ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KEM
import Data.Effectful.Wreq qualified as Wreq
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time qualified as Time
import Effectful
import Effectful.Concurrent (forkIO)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (urlEncode)
import Network.Wreq
import Network.Wreq.Types (FormParam)
import Pages.Bots.Utils (BotType (..), handleTableResponse)
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


whatsappIncomingPostH :: TwilioWhatsAppMessage -> ATBaseCtx AE.Value
whatsappIncomingPostH val = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = authCtx.config
  projectM <- dbtToEff $ Projects.getProjectByPhoneNumber (T.dropWhile (/= '+') val.from)
  case projectM of
    Just p -> do
      void $ forkIO $ handlePrompt val envCfg p

      pure $ AE.object []
    _ -> pure $ AE.object []
  where
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
                  opts = "t=" <> decodeUtf8 (urlEncode True (encodeUtf8 $ show now)) <> "&q=" <> decodeUtf8 (urlEncode True (encodeUtf8 query)) <> "&p=" <> project.id.toText <> "&t=" <> chartType
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


getBotContent :: Text -> Text -> Text -> Text -> AE.Value
getBotContent question query query_url opts =
  AE.object ["1" AE..= ("*" <> question <> "*"), "2" AE..= ("`" <> query <> "`"), "3" AE..= opts, "4" AE..= query_url]


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
