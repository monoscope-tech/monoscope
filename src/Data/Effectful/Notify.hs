{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
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
  PagerDutyData (..),
  PagerDutyAction (..),
  PagerDutySeverity (..),

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

import Control.Lens ((.~))
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((?~))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEK
import Data.Aeson.QQ (aesonQQ)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH
import Network.HTTP.Types (statusIsSuccessful)
import Network.Wreq (FormParam ((:=)), auth, basicAuth, defaults, header, postWith, responseStatus)
import Relude hiding (Reader, State, ask, get, modify, put, runState)
import System.Config qualified as Config
import System.Logging qualified as Log


-- Notification data types
data EmailData = EmailData
  { receiver :: Text
  , templateOptions :: Maybe (Text, AE.Value)
  , subjectMessage :: Maybe (Text, Text)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data SlackData = SlackData
  { channelId :: Text
  , message :: AE.Value
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data DiscordData = DiscordData
  { channelId :: Text
  , content :: AE.Value
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


data PagerDutyAction = PDTrigger | PDResolve
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


pdActionText :: PagerDutyAction -> Text
pdActionText = \case PDTrigger -> "trigger"; PDResolve -> "resolve"


data PagerDutySeverity = PDCritical | PDError | PDWarning | PDInfo
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


pdSeverityText :: PagerDutySeverity -> Text
pdSeverityText = \case PDCritical -> "critical"; PDError -> "error"; PDWarning -> "warning"; PDInfo -> "info"


data PagerDutyData = PagerDutyData
  { integrationKey :: Text
  , eventAction :: PagerDutyAction
  , dedupKey :: Text
  , summary :: Text
  , severity :: PagerDutySeverity
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
  | PagerDutyNotification PagerDutyData
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Effect definition
data Notify :: Effect where
  SendNotification :: Notification -> Notify m ()
  GetNotifications :: Notify m [Notification]


type instance DispatchOf Notify = 'Dynamic


makeEffect ''Notify


-- Smart constructors
emailNotification :: Text -> Maybe (Text, AE.Value) -> Maybe (Text, Text) -> Notification
emailNotification receiver templateOptions subjectMessage =
  EmailNotification EmailData{..}


slackNotification :: Text -> AE.Value -> Notification
slackNotification channelId message =
  SlackNotification SlackData{..}


discordNotification :: Text -> AE.Value -> Notification
discordNotification channelId content =
  DiscordNotification DiscordData{..}


whatsappNotification :: Text -> Text -> AE.Value -> Notification
whatsappNotification template to contentVariables =
  WhatsAppNotification WhatsAppData{..}


pagerdutyNotification :: Text -> PagerDutyAction -> Text -> Text -> PagerDutySeverity -> AE.Value -> Text -> Notification
pagerdutyNotification integrationKey eventAction dedupKey summary severity customDetails monitorUrl =
  PagerDutyNotification PagerDutyData{..}


-- Production interpreter
runNotifyProduction :: (IOE :> es, Log :> es, Reader Config.AuthContext :> es) => Eff (Notify ': es) a -> Eff es a
runNotifyProduction = interpret $ \_ -> \case
  SendNotification notification -> case notification of
    EmailNotification EmailData{..} -> do
      appCtx <- ask @Config.AuthContext
      let url = if isJust templateOptions then "https://api.postmarkapp.com/email/withTemplate" else "https://api.postmarkapp.com/email"
      let apiKey = encodeUtf8 appCtx.config.postmarkToken
      let (subject, message) = fromMaybe ("", "") subjectMessage
      let payload = case templateOptions of
            Just (template, templateVars) ->
              [aesonQQ|{
                "From": "hello@apitoolkit.io",
                "To": #{receiver},
                "TemplateAlias": #{template},
                "TemplateModel": #{templateVars}
              }|]
            _ ->
              [aesonQQ|{
                "From": "hello@apitoolkit.io",
                "Subject": #{subject},
                "To": #{receiver},
                "TextBody": #{message},
                "HtmlBody": "<p>#{message}</p>",
                "MessageStream": "outbound"
              }|]
      let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Accept" .~ ["application/json"] & header "X-Postmark-Server-Token" .~ [apiKey]
      _ <- liftIO $ postWith opts url payload
      pass
    SlackNotification SlackData{..} -> do
      appCtx <- ask @Config.AuthContext
      let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bearer " <> appCtx.config.slackBotToken]
      case message of
        AE.Object obj -> do
          let msg = AE.Object $ AEK.insert "channel" (AE.String channelId) obj
          re <- liftIO $ postWith opts "https://slack.com/api/chat.postMessage" msg
          unless (statusIsSuccessful (re ^. responseStatus))
            $ Log.logAttention "Slack notification failed" (channelId, show $ re ^. responseStatus)
          pass
        _ -> do
          Log.logAttention "Slack notification message is not an object" (channelId, show message)
          pass
    DiscordNotification DiscordData{..} -> do
      appCtx <- ask @Config.AuthContext
      let url = toString $ "https://discord.com/api/v10/channels/" <> channelId <> "/messages"
      let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bot " <> appCtx.config.discordBotToken]
      re <- liftIO $ postWith opts url content
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
    PagerDutyNotification PagerDutyData{..} -> do
      let links = [AE.object ["href" AE..= monitorUrl, "text" AE..= ("View Monitor" :: Text)]] :: [AE.Value]
          triggerPayload = AEK.fromList
            [ "payload" AE..= AE.object ["summary" AE..= summary, "source" AE..= ("monoscope" :: Text), "severity" AE..= pdSeverityText severity, "custom_details" AE..= customDetails]
            , "links" AE..= links]
          payload = AE.Object $ AEK.fromList ["routing_key" AE..= integrationKey, "event_action" AE..= pdActionText eventAction, "dedup_key" AE..= dedupKey] <> bool mempty triggerPayload (eventAction == PDTrigger)
      re <- liftIO $ postWith (defaults & header "Content-Type" .~ ["application/json"]) "https://events.pagerduty.com/v2/enqueue" payload
      unless (statusIsSuccessful (re ^. responseStatus)) $ Log.logAttention "PagerDuty notification failed" (dedupKey, show $ re ^. responseStatus)
  GetNotifications -> pure [] -- Production doesn't store notifications


-- Test interpreter
runNotifyTest :: (IOE :> es, Log :> es) => Eff (Notify ': es) a -> Eff es ([Notification], a)
runNotifyTest eff = do
  ref <- liftIO $ newIORef []
  result <-
    interpret
      ( \_ -> \case
          SendNotification notification -> do
            let notifInfo = case notification of
                  EmailNotification emailData ->
                    ("Email" :: Text, emailData.receiver, fmap fst emailData.templateOptions)
                  SlackNotification slackData ->
                    ("Slack" :: Text, slackData.channelId, Nothing :: Maybe Text)
                  DiscordNotification discordData ->
                    ("Discord" :: Text, discordData.channelId, Nothing :: Maybe Text)
                  WhatsAppNotification whatsappData ->
                    ("WhatsApp" :: Text, whatsappData.to, Just whatsappData.template)
                  PagerDutyNotification pagerdutyData ->
                    ("PagerDuty" :: Text, pagerdutyData.dedupKey, Just pagerdutyData.summary)
            Log.logInfo "Notification" notifInfo
            Log.logTrace "Notification payload" notification
            liftIO $ modifyIORef ref (notification :)
          GetNotifications -> do
            notifications <- liftIO $ readIORef ref
            pure (reverse notifications)
      )
      eff
  notifications <- liftIO $ readIORef ref
  pure (reverse notifications, result)
