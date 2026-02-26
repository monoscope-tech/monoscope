{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Effectful.Notify (
  -- * Effect
  Notify,
  sendNotification,
  sendNotificationWithReply,
  getNotifications,

  -- * Notification types
  Notification (..),
  EmailData (..),
  SlackData (..),
  DiscordData (..),
  WhatsAppData (..),
  PagerdutyData (..),
  PagerdutyAction (..),
  PagerdutySeverity (..),

  -- * Interpreters
  runNotifyProduction,
  runNotifyTest,

  -- * Smart constructors
  emailNotification,
  slackNotification,
  slackThreadedNotification,
  whatsappNotification,
  discordNotification,
  discordThreadedNotification,
  pagerdutyNotification,
) where

import Control.Exception (try)
import Control.Lens ((.~))
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((?~))
import Control.Retry (exponentialBackoff, limitRetries, retrying)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEK
import Data.Aeson.Lens (key, _String)
import Data.Aeson.QQ (aesonQQ)
import Data.Text.Display (Display, display)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH
import Network.HTTP.Types (statusIsSuccessful)
import Network.Mail.Mime (Address (..), Mail (..), htmlPart)
import Network.Mail.SMTP (sendMailWithLoginSTARTTLS', sendMailWithLoginTLS')
import Network.Wreq (FormParam ((:=)), auth, basicAuth, defaults, header, postWith, responseBody, responseStatus)
import Pkg.DeriveUtils (WrappedEnumSC (..))
import Relude hiding (Reader, State, ask, get, modify, put, runState)
import System.Config qualified as Config
import System.Logging qualified as Log
import System.Timeout (timeout)


-- Notification data types
data EmailData = EmailData
  { receiver :: Text
  , subject :: Text
  , htmlBody :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data SlackData = SlackData
  { channelId :: Text
  , botToken :: Text
  , payload :: AE.Value
  , threadTs :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data DiscordData = DiscordData
  { channelId :: Text
  , payload :: AE.Value
  , replyToMessageId :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data WhatsAppData = WhatsAppData
  { template :: Text
  , contentVariables :: AE.Value
  , to :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data PagerdutyAction = PDTrigger | PDResolve
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (Display) via WrappedEnumSC "PD" PagerdutyAction


data PagerdutySeverity = PDCritical | PDError | PDWarning | PDInfo
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (Display) via WrappedEnumSC "PD" PagerdutySeverity


data PagerdutyData = PagerdutyData
  { integrationKey :: Text
  , eventAction :: PagerdutyAction
  , dedupKey :: Text
  , summary :: Text
  , severity :: PagerdutySeverity
  , customDetails :: AE.Value
  , monitorUrl :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Notification
  = EmailNotification EmailData
  | SlackNotification SlackData
  | DiscordNotification DiscordData
  | WhatsAppNotification WhatsAppData
  | PagerdutyNotification PagerdutyData
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Effect definition
data Notify :: Effect where
  SendNotification :: Notification -> Notify m ()
  SendNotificationWithReply :: Notification -> Notify m (Maybe Text)
  GetNotifications :: Notify m [Notification]


type instance DispatchOf Notify = 'Dynamic


makeEffect ''Notify


-- Smart constructors
emailNotification :: Text -> Text -> Text -> Notification
emailNotification receiver subject htmlBody = EmailNotification EmailData{..}


slackNotification :: Text -> Text -> AE.Value -> Notification
slackNotification channelId botToken payload =
  let threadTs = Nothing in SlackNotification SlackData{..}


slackThreadedNotification :: Text -> Text -> AE.Value -> Maybe Text -> Notification
slackThreadedNotification channelId botToken payload threadTs =
  SlackNotification SlackData{..}


discordNotification :: Text -> AE.Value -> Notification
discordNotification channelId payload =
  let replyToMessageId = Nothing in DiscordNotification DiscordData{..}


discordThreadedNotification :: Text -> AE.Value -> Maybe Text -> Notification
discordThreadedNotification channelId payload replyToMessageId =
  DiscordNotification DiscordData{..}


whatsappNotification :: Text -> Text -> AE.Value -> Notification
whatsappNotification template to contentVariables =
  WhatsAppNotification WhatsAppData{..}


pagerdutyNotification :: Text -> PagerdutyAction -> Text -> Text -> PagerdutySeverity -> AE.Value -> Text -> Notification
pagerdutyNotification integrationKey eventAction dedupKey summary severity customDetails monitorUrl =
  PagerdutyNotification PagerdutyData{..}


-- Production interpreter
runNotifyProduction :: (IOE :> es, Log :> es, Reader Config.AuthContext :> es) => Eff (Notify ': es) a -> Eff es a
runNotifyProduction = interpret $ \_ -> \case
  SendNotification notification -> case notification of
    EmailNotification EmailData{..} -> do
      appCtx <- ask @Config.AuthContext
      let cfg = appCtx.config
          via = if cfg.smtpHost == "" then "api" :: Text else "smtp"
      Log.logTrace "Sending email notification" (AE.object ["to" AE..= receiver, "subject" AE..= subject, "via" AE..= via])
      result <-
        liftIO
          $ try @SomeException
          $ timeout 30_000_000
          $ if cfg.smtpHost == ""
            then do
              let apiKey = encodeUtf8 cfg.postmarkToken
                  fromAddress = cfg.postmarkFromEmail
                  reqPayload = [aesonQQ|{ "From": #{fromAddress}, "Subject": #{subject}, "To": #{receiver}, "HtmlBody": #{htmlBody}, "MessageStream": "outbound" }|]
                  opts = defaults & header "Content-Type" .~ ["application/json"] & header "Accept" .~ ["application/json"] & header "X-Postmark-Server-Token" .~ [apiKey]
              re <- postWith opts "https://api.postmarkapp.com/email" reqPayload
              unless (statusIsSuccessful (re ^. responseStatus)) $ fail $ "Postmark returned " <> show (re ^. responseStatus)
            else do
              let from = Address Nothing cfg.smtpSender
                  to = Address Nothing receiver
                  mail = Mail from [to] [] [] [("Subject", subject)] [[htmlPart (toLazy htmlBody)]]
                  port = fromIntegral cfg.smtpPort
                  sendMail = if cfg.smtpTls then sendMailWithLoginTLS' else sendMailWithLoginSTARTTLS'
              sendMail (toString cfg.smtpHost) port (toString cfg.smtpUsername) (toString cfg.smtpPassword) mail
      case result of
        Right (Just ()) -> Log.logTrace "Email sent successfully" (AE.object ["to" AE..= receiver, "via" AE..= via])
        Right Nothing -> Log.logAttention "Email send timed out after 30s" (AE.object ["to" AE..= receiver, "subject" AE..= subject, "via" AE..= via])
        Left ex -> Log.logAttention "Email send failed" (AE.object ["to" AE..= receiver, "subject" AE..= subject, "via" AE..= via, "error" AE..= displayException ex])
    SlackNotification slackData -> void $ sendSlack slackData
    DiscordNotification discordData -> void $ sendDiscord discordData
    WhatsAppNotification WhatsAppData{template, to, contentVariables} -> do
      appCtx <- ask @Config.AuthContext
      let from = appCtx.config.whatsappFromNumber
          accountSid = appCtx.config.twilioAccountSid
          token = appCtx.config.twilioAuthToken
          opts = defaults & header "Content-Type" .~ ["application/x-www-form-urlencoded"] & auth ?~ basicAuth (encodeUtf8 accountSid) (encodeUtf8 token)
          url = toString $ "https://api.twilio.com/2010-04-01/Accounts/" <> accountSid <> "/Messages.json"
          variables = toStrict $ AE.encode contentVariables
          payload =
            [ "To" := ("whatsapp:" <> to)
            , "From" := ("whatsapp:" <> from)
            , "ContentSid" := template
            , "ContentVariables" := variables
            ]
              :: [FormParam]
      resp <- liftIO $ postWith opts url payload
      pass
    PagerdutyNotification PagerdutyData{..} -> do
      let actionText = display eventAction
          severityText = display severity
          payload =
            if eventAction == PDTrigger
              then
                [aesonQQ|{
              "routing_key": #{integrationKey},
              "event_action": #{actionText},
              "dedup_key": #{dedupKey},
              "payload": {
                "summary": #{summary},
                "source": "monoscope",
                "severity": #{severityText},
                "custom_details": #{customDetails}
              },
              "links": [{"href": #{monitorUrl}, "text": "View Monitor"}]
            }|]
              else
                [aesonQQ|{
              "routing_key": #{integrationKey},
              "event_action": #{actionText},
              "dedup_key": #{dedupKey}
            }|]
          opts = defaults & header "Content-Type" .~ ["application/json"]
          policy = exponentialBackoff 1000000 <> limitRetries 3
      re <- liftIO $ retrying policy (\_ r -> pure $ not $ statusIsSuccessful (r ^. responseStatus)) \_ -> postWith opts "https://events.pagerduty.com/v2/enqueue" payload
      unless (statusIsSuccessful (re ^. responseStatus)) $ Log.logAttention "PagerDuty notification failed" (dedupKey, show $ re ^. responseStatus)
  SendNotificationWithReply notification -> case notification of
    SlackNotification slackData -> sendSlack slackData
    DiscordNotification discordData -> sendDiscord discordData
    _ -> pure Nothing
  GetNotifications -> pure [] -- Production doesn't store notifications
  where
    sendSlack :: (IOE :> es, Log :> es, Reader Config.AuthContext :> es) => SlackData -> Eff es (Maybe Text)
    sendSlack SlackData{..} = do
      let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bearer " <> botToken]
      case payload of
        AE.Object obj -> do
          let withThread = case threadTs of
                Just ts -> AEK.insert "thread_ts" (AE.String ts) obj
                Nothing -> obj
              msg = AE.Object $ AEK.insert "channel" (AE.String channelId) withThread
          result <- liftIO $ try @SomeException $ postWith opts "https://slack.com/api/chat.postMessage" msg
          case result of
            Right re | statusIsSuccessful (re ^. responseStatus) ->
              pure $ AE.decode (re ^. responseBody) >>= (^? key "ts" . _String)
            Right re -> do
              Log.logAttention "Slack notification failed" (channelId, show $ re ^. responseStatus)
              pure Nothing
            Left ex -> do
              Log.logAttention "Slack notification failed" (channelId, displayException ex)
              pure Nothing
        _ -> do
          Log.logAttention "Slack notification message is not an object" (channelId, show payload)
          pure Nothing

    sendDiscord :: (IOE :> es, Log :> es, Reader Config.AuthContext :> es) => DiscordData -> Eff es (Maybe Text)
    sendDiscord DiscordData{..} = do
      appCtx <- ask @Config.AuthContext
      let url = toString $ "https://discord.com/api/v10/channels/" <> channelId <> "/messages"
          opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bot " <> appCtx.config.discordBotToken]
          payloadWithReply = case (replyToMessageId, payload) of
            (Just msgId, AE.Object obj) ->
              AE.Object $ AEK.insert "message_reference" (AE.object ["message_id" AE..= msgId]) obj
            _ -> payload
      result <- liftIO $ try @SomeException $ postWith opts url payloadWithReply
      case result of
        Right re | statusIsSuccessful (re ^. responseStatus) ->
          pure $ AE.decode (re ^. responseBody) >>= (^? key "id" . _String)
        Right re -> do
          Log.logAttention "Discord notification failed" (channelId, show $ re ^. responseStatus)
          pure Nothing
        Left ex -> do
          Log.logAttention "Discord notification failed" (channelId, displayException ex)
          pure Nothing


-- Test interpreter that stores notifications in provided IORef
runNotifyTest :: (IOE :> es, Log :> es) => IORef [Notification] -> Eff (Notify ': es) a -> Eff es a
runNotifyTest ref = interpret \_ -> \case
  SendNotification notification -> do
    let notifInfo = case notification of
          EmailNotification emailData -> ("Email" :: Text, emailData.receiver, Just emailData.subject)
          SlackNotification slackData -> ("Slack" :: Text, slackData.channelId, Nothing :: Maybe Text)
          DiscordNotification discordData -> ("Discord" :: Text, discordData.channelId, Nothing :: Maybe Text)
          WhatsAppNotification whatsappData -> ("WhatsApp" :: Text, whatsappData.to, Just whatsappData.template)
          PagerdutyNotification pagerdutyData -> ("PagerDuty" :: Text, pagerdutyData.dedupKey, Just pagerdutyData.summary)
    Log.logTrace "Notification" notifInfo
    Log.logTrace "Notification payload" notification
    liftIO $ modifyIORef ref (notification :)
  SendNotificationWithReply notification -> do
    liftIO $ modifyIORef ref (notification :)
    pure Nothing
  GetNotifications -> liftIO $ reverse <$> readIORef ref
