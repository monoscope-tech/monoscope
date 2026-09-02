module Pages.Bots.Whatsapp (whatsappIncomingPostH, TwilioWhatsAppMessage (..), BodyType (..), parseWhatsappBody, getWhatsappList) where

import Control.Lens ((.~), (?~))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as KEYM
import Data.Aeson.KeyMap qualified as KEM
import Data.Effectful.Wreq qualified as Wreq
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful.Log qualified as Log
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time
import Models.Apis.Integrations (getDashboardsForWhatsapp)
import Models.Projects.Dashboards (Dashboard (..))
import Models.Projects.Projects qualified as Projects
import Network.Wreq
import Pages.Bots.Utils (BotType (..), runBotQuery, withDashboardTemplate)
import Pkg.Components.Widget (Widget (..))
import Relude
import System.Config (AuthContext (backgroundScope))
import System.Config qualified as Config
import System.Tracing (forkBackground)
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
      fromN = T.dropWhile (/= '+') val.from

      -- Twilio has two outbound shapes: a plain-text template (Body) and the
      -- chart template (ContentVariables). Text-form bot replies are exactly
      -- the ones carrying a "body" key, so route on it.
      send v = case v of
        AE.Object o | Just (AE.String t) <- KEM.lookup "body" o -> sendWhatsappResponse (AE.object []) val.from envCfg.whatsappBotText (Just t)
        _ -> sendWhatsappResponse v val.from envCfg.whatsappBotChart Nothing

      handleDashboard project dashboardId skip = withDashboardTemplate project.id dashboardId $ \dashboard -> do
        let widgets = V.fromList $ (\w -> let t = fromMaybe "Untitled-" w.title in (t, "widg" <> joiner <> t <> joiner <> dashboardId)) <$> dashboard.widgets
        sendWhatsappResponse (getWhatsappList ("widget" <> joiner <> dashboardId) "Please select a widget" widgets skip) val.from envCfg.whatsappDashboardList Nothing

      handleWidget widget dashboardId project = withDashboardTemplate project.id dashboardId $ \dashboard ->
        whenJust (find (\w -> fromMaybe "Untitled-" w.title == widget) dashboard.widgets) $ \w -> do
          now <- Time.currentTime
          -- Same Twilio chart template as an AI chart reply: "3" is the query
          -- string appended to the chart endpoint, "4" the host-relative link.
          let opts = "time=" <> toUriStr (show now) <> "&p=" <> project.id.toText <> "&widget=" <> toUriStr (decodeUtf8 @Text $ AE.encode w)
          sendWhatsappResponse (AE.object ["1" AE..= ("*" <> val.body <> "*"), "2" AE..= ("`" <> widget <> "`"), "3" AE..= opts, "4" AE..= (project.id.toText <> "/dashboards")]) val.from envCfg.whatsappBotChart Nothing

  projectM <- Projects.getProjectByPhoneNumber fromN
  Log.logTrace ("WhatsApp project lookup" :: Text) $ AE.object ["fromN" AE..= fromN, "found" AE..= isJust projectM]
  whenJust projectM $ \p -> case parseWhatsappBody val.body of
    DashboardLoad skip -> do
      dashboards <- V.fromList . map (second (("dash" <> joiner) <>)) <$> getDashboardsForWhatsapp fromN
      sendWhatsappResponse (getWhatsappList "dashboard" "Please select a dashboard" dashboards skip) val.from envCfg.whatsappDashboardList Nothing
    WidgetsLoad dashboardId skip -> handleDashboard p dashboardId skip
    WidgetSelect widgetTitle dashboardId -> handleWidget widgetTitle dashboardId p
    Prompt -> forkBackground authCtx.backgroundScope ("WhatsApp prompt (" <> val.from <> ")") $ runBotQuery WhatsApp send envCfg p.id val.body (pure Nothing)
  pure $ AE.object []


data BodyType
  = WidgetsLoad Text Int
  | WidgetSelect Text Text
  | DashboardLoad Int
  | Prompt


parseWhatsappBody :: Text -> BodyType
parseWhatsappBody "/dashboard" = DashboardLoad 0
parseWhatsappBody body = case T.splitOn joiner body of
  ["dashboard", skip] -> DashboardLoad (readInt0 skip)
  ["dash", dashboardId] -> WidgetsLoad dashboardId 0
  ["dash", dashboardId, _] -> WidgetsLoad dashboardId 0
  ["widget", dashboardId, skip] -> WidgetsLoad dashboardId (readInt0 skip)
  ["widg", widgetTitle, dashboardId] -> WidgetSelect widgetTitle dashboardId
  ["widg", widgetTitle, dashboardId, _] -> WidgetSelect widgetTitle dashboardId
  _ -> Prompt
  where
    readInt0 = fromMaybe 0 . readMaybe . toString


-- | Twilio content-template variables: "1" is the prompt, then alternating label/payload
-- pairs from "2" on, with "6"/"7" reserved for the Load More button when the list overflows.
getWhatsappList :: Text -> Text -> V.Vector (Text, Text) -> Int -> AE.Value
getWhatsappList typ body vals' skip = AE.object $ ("1" AE..= body) : buttons <> loadMore
  where
    vals = V.map (first (T.take 24)) (V.drop skip vals')
    missing = 3 - V.length vals
    -- Twilio rejects templates with unfilled variables, so pad short lists with suffixed copies.
    padded
      | missing <= 0 = vals
      | otherwise = vals <> V.imap (\i (k, v) -> (k <> " " <> show (i + 1), v <> joiner <> show (i + 1))) (V.take missing $ V.concat $ replicate missing vals)
    overflow = V.length vals > 3
    buttons = concat $ V.toList $ V.imap (\i (k, v) -> [key (2 * i + 2) AE..= k, key (2 * i + 3) AE..= v]) (if overflow then V.take 2 padded else padded)
    loadMore
      | overflow = ["6" AE..= ("Load More" :: Text), "7" AE..= (typ <> joiner <> show (skip + 2))]
      | otherwise = []
    key :: Int -> AE.Key
    key = KEYM.fromText . show


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
      <$> req "MessageSid"
      <*> req "SmsSid"
      <*> req "SmsMessageSid"
      <*> req "AccountSid"
      <*> opt "MessagingServiceSid"
      <*> req "From"
      <*> req "To"
      <*> req "Body"
      <*> num "NumMedia" 0
      <*> num "NumSegments" 1
      <*> opt "ProfileName"
      <*> opt "WaId"
      <*> flag "Forwarded"
      <*> flag "FrequentlyForwarded"
      <*> opt "ButtonText"
    where
      raw k = lookupMaybe k f
      req k = raw k >>= maybe (fail $ "Missing field: " <> toString k) pure
      opt k = pure $ fromRight Nothing (raw k)
      num k d = maybe d (fromMaybe d . readMaybe . toString) <$> raw k
      flag k = fmap (== "true") <$> raw k


sendWhatsappResponse :: AE.Value -> Text -> Text -> Maybe Text -> ATBaseCtx ()
sendWhatsappResponse contentVariables to template bodyM = do
  Log.logTrace ("WhatsApp response" :: Text) $ AE.object ["to" AE..= to, "template" AE..= template, "body" AE..= bodyM, "contentVariables" AE..= contentVariables]
  appCtx <- Effectful.Reader.Static.ask @AuthContext
  let accountSid = appCtx.config.twilioAccountSid
      opts = defaults & header "Content-Type" .~ ["application/x-www-form-urlencoded"] & auth ?~ basicAuth (encodeUtf8 accountSid) (encodeUtf8 appCtx.config.twilioAuthToken)
      url = toString $ "https://api.twilio.com/2010-04-01/Accounts/" <> accountSid <> "/Messages.json"
      payload :: [FormParam]
      payload =
        ["To" := to, "From" := ("whatsapp:" <> appCtx.config.whatsappFromNumber)]
          <> maybe ["ContentSid" := template, "ContentVariables" := toStrict (AE.encode contentVariables)] (\x -> ["Body" := x]) bodyM
  void $ Wreq.postWith opts url payload
