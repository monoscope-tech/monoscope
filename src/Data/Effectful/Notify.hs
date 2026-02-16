{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Effectful.Notify (
  -- * Effect
  Notify,
  sendNotification,
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
  whatsappNotification,
  discordNotification,
  pagerdutyNotification,
) where

import Control.Exception (try)
import Control.Lens ((.~))
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((?~))
import Control.Retry (exponentialBackoff, limitRetries, retrying)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEK
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
import Network.Wreq (FormParam ((:=)), auth, basicAuth, defaults, header, postWith, responseStatus)
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
  , payload :: AE.Value
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data DiscordData = DiscordData
  { channelId :: Text
  , payload :: AE.Value
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
  GetNotifications :: Notify m [Notification]


type instance DispatchOf Notify = 'Dynamic


makeEffect ''Notify


-- Smart constructors
emailNotification :: Text -> Text -> Text -> Notification
emailNotification receiver subject htmlBody = EmailNotification EmailData{..}


slackNotification :: Text -> AE.Value -> Notification
slackNotification channelId payload =
  SlackNotification SlackData{..}


discordNotification :: Text -> AE.Value -> Notification
discordNotification channelId payload =
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
    SlackNotification SlackData{..} -> do
      appCtx <- ask @Config.AuthContext
      let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bearer " <> appCtx.config.slackBotToken]
      case payload of
        AE.Object obj -> do
          let msg = AE.Object $ AEK.insert "channel" (AE.String channelId) obj
          re <- liftIO $ postWith opts "https://slack.com/api/chat.postMessage" msg
          unless (statusIsSuccessful (re ^. responseStatus))
            $ Log.logAttention "Slack notification failed" (channelId, show $ re ^. responseStatus)
          pass
        _ -> do
          Log.logAttention "Slack notification message is not an object" (channelId, show payload)
          pass
    DiscordNotification DiscordData{..} -> do
      appCtx <- ask @Config.AuthContext
      let url = toString $ "https://discord.com/api/v10/channels/" <> channelId <> "/messages"
      let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bot " <> appCtx.config.discordBotToken]
      re <- liftIO $ postWith opts url payload
      unless (statusIsSuccessful (re ^. responseStatus))
        $ Log.logAttention "Discord notification failed" (channelId, show $ re ^. responseStatus)
      pass
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
  GetNotifications -> pure [] -- Production doesn't store notifications


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
  GetNotifications -> liftIO $ reverse <$> readIORef ref
