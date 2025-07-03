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
  discordNotification,
) where

import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Effectful.State.Static.Local (State, get, modify)
import Log qualified
import Models.Apis.RequestDumps qualified as RequestDumps
import Network.Wreq (defaults, header, postWith)
import Relude hiding (Reader, State, ask, get, modify, put, runState)
import System.Config qualified as Config

-- Notification data types
data EmailData = EmailData
  { receiver :: Text
  , templateOptions :: Maybe (Text, AE.Value)
  , subjectMessage :: Maybe (Text, Text)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (AE.ToJSON, AE.FromJSON)

data SlackData = SlackData
  { webhookUrl :: Text
  , message :: AE.Value
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (AE.ToJSON, AE.FromJSON)

data DiscordData = DiscordData
  { channelId :: Text
  , content :: AE.Value
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (AE.ToJSON, AE.FromJSON)

data Notification
  = EmailNotification EmailData
  | SlackNotification SlackData
  | DiscordNotification DiscordData
  deriving stock (Show, Eq, Generic)
  deriving anyclass (AE.ToJSON, AE.FromJSON)

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
      
  GetNotifications -> pure []  -- Production doesn't store notifications

-- Test interpreter
runNotifyTest :: (IOE :> es, Log :> es) => Eff (Notify ': es) a -> Eff es ([Notification], a)
runNotifyTest eff = do
  ref <- liftIO $ newIORef []
  result <- interpret
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