{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

  -- * Interpreters
  runNotifyProduction,
  runNotifyTest,

  -- * Smart constructors
  emailNotification,
  slackNotification,
  whatsappNotification,
  discordNotification,
) where

import Control.Lens ((.~))
import Control.Lens.Setter ((?~))
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Log qualified
import Network.Wreq (FormParam ((:=)), auth, basicAuth, defaults, header, postWith)
import Relude hiding (Reader, State, ask, get, modify, put, runState)
import System.Config qualified as Config


-- Notification data types
data EmailData = EmailData
  { receiver :: Text
  , templateOptions :: Maybe (Text, AE.Value)
  , subjectMessage :: Maybe (Text, Text)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data SlackData = SlackData
  { webhookUrl :: Text
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


data Notification
  = EmailNotification EmailData
  | SlackNotification SlackData
  | DiscordNotification DiscordData
  | WhatsAppNotification WhatsAppData
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Effect definition
type role Notify phantom nominal
data Notify :: Effect where
  SendNotification :: Notification -> Notify m ()
  GetNotifications :: Notify m [Notification]


type instance DispatchOf Notify = 'Dynamic


-- Effect operations
sendNotification :: Notify :> es => Notification -> Eff es ()
sendNotification = send . SendNotification


getNotifications :: Notify :> es => Eff es [Notification]
getNotifications = send GetNotifications


-- Smart constructors
emailNotification :: Text -> Maybe (Text, AE.Value) -> Maybe (Text, Text) -> Notification
emailNotification receiver templateOptions subjectMessage =
  EmailNotification EmailData{..}


slackNotification :: Text -> AE.Value -> Notification
slackNotification webhookUrl message =
  SlackNotification SlackData{..}


discordNotification :: Text -> AE.Value -> Notification
discordNotification channelId content =
  DiscordNotification DiscordData{..}


whatsappNotification :: Text -> Text -> AE.Value -> Notification
whatsappNotification template to contentVariables =
  WhatsAppNotification WhatsAppData{..}


-- Production interpreter
runNotifyProduction :: (IOE :> es, Reader Config.AuthContext :> es) => Eff (Notify ': es) a -> Eff es a
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
      let opts = defaults & header "Content-Type" .~ ["application/json"]
      _ <- liftIO $ postWith opts (toString webhookUrl) message
      pass
    DiscordNotification DiscordData{..} -> do
      appCtx <- ask @Config.AuthContext
      let url = toString $ "https://discord.com/api/v10/channels/" <> channelId <> "/messages"
      let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bot " <> appCtx.config.discordBotToken]
      _ <- liftIO $ postWith opts url content
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
  GetNotifications -> pure [] -- Production doesn't store notifications


-- Test interpreter
runNotifyTest :: (IOE :> es, Log :> es) => Eff (Notify ': es) a -> Eff es ([Notification], a)
runNotifyTest eff = do
  ref <- liftIO $ newIORef []
  result <-
    interpret
      ( \_ -> \case
          SendNotification notification -> do
            Log.logInfo "Test notification sent" notification
            liftIO $ modifyIORef ref (notification :)
          GetNotifications -> do
            notifications <- liftIO $ readIORef ref
            pure (reverse notifications)
      )
      eff
  notifications <- liftIO $ readIORef ref
  pure (reverse notifications, result)
