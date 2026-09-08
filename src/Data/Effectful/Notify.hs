{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Effectful.Notify (
  -- * Effect
  Notify,
  sendNotification,
  sendNotificationWithReply,
  getNotifications,
  deliverSlack,
  replaceImages,
  SlackOperation (..),
  SlackResult (..),

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
  slackWebhookNotification,
  withSlackContext,
  whatsappNotification,
  discordNotification,
  discordThreadedNotification,
  pagerdutyNotification,
) where

import Control.Exception (try)
import Control.Lens ((.~), (^?))
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((?~))
import Control.Retry (exponentialBackoff, limitRetries, retrying)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEK
import Data.Aeson.Lens (key, _String)
import Data.Aeson.QQ (aesonQQ)
import Data.Text qualified as T
import Data.Text.Display (Display, display)
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH
import Network.HTTP.Types (statusCode, statusIsSuccessful)
import Network.Mail.Mime (Address (..), Mail (..), htmlPart)
import Network.Mail.SMTP (sendMailWithLoginSTARTTLS', sendMailWithLoginTLS')
import Network.Wreq (FormParam ((:=)), auth, basicAuth, checkResponse, defaults, header, postWith, responseBody, responseHeader, responseStatus)
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


-- | Routing info for a single Slack message.
-- When @webhookUrl@ is @Just u@, the message is posted to @u@ directly — used
-- for the OAuth-time default channel so we don't require the bot user to be a
-- member (critical for private channels picked in Slack's install consent
-- screen). Webhooks accept @threadTs@ but do not return the posted timestamp.
-- When @webhookUrl@ is @Nothing@, we use chat.postMessage with @botToken@,
-- which supports threading but requires the bot to be in @channelId@.
--
-- @projectIdCtx@ and @teamIdCtx@ are logging breadcrumbs only — when Slack
-- starts silently 404ing a webhook (app uninstalled, token revoked) we need
-- to attribute the failure back to a specific project/workspace. Empty when
-- unavailable; never used for routing.
data SlackData = SlackData
  { channelId :: Text
  , botToken :: Text
  , payload :: AE.Value
  , threadTs :: Maybe Text
  , webhookUrl :: Maybe Text
  , projectIdCtx :: Text
  , teamIdCtx :: Text
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
  deriving (Display) via WrappedEnumSC 'Nothing "PD" PagerdutyAction


data PagerdutySeverity = PDCritical | PDError | PDWarning | PDInfo
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (Display) via WrappedEnumSC 'Nothing "PD" PagerdutySeverity


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
  DeliverSlack :: SlackOperation -> SlackData -> Notify m SlackResult


type instance DispatchOf Notify = 'Dynamic


data SlackOperation = SlackPost | SlackUpdate Text
  deriving stock (Eq, Show)


data SlackResult
  = SlackSent Text
  | SlackAccepted
  | SlackRateLimited Int
  | SlackRejected Text
  | SlackAmbiguous
  deriving stock (Eq, Show)


data SlackResponse = SlackResponse {ok :: Bool, ts :: Maybe Text, error :: Maybe Text}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


makeEffect ''Notify


-- Smart constructors
emailNotification :: Text -> Text -> Text -> Notification
emailNotification receiver subject htmlBody = EmailNotification EmailData{..}


slackNotification :: Text -> Text -> AE.Value -> Notification
slackNotification channelId botToken payload =
  SlackNotification SlackData{channelId, botToken, payload, threadTs = Nothing, webhookUrl = Nothing, projectIdCtx = "", teamIdCtx = ""}


slackThreadedNotification :: Text -> Text -> AE.Value -> Maybe Text -> Notification
slackThreadedNotification channelId botToken payload threadTs =
  SlackNotification SlackData{channelId, botToken, payload, threadTs, webhookUrl = Nothing, projectIdCtx = "", teamIdCtx = ""}


-- | Post via the OAuth-time incoming webhook URL. Works for any channel the
-- user picked during install (public or private) without requiring bot
-- membership. Root timestamps must be captured separately from Slack events.
slackWebhookNotification :: Text -> Text -> AE.Value -> Maybe Text -> Notification
slackWebhookNotification webhookUrl channelId payload threadTs =
  SlackNotification SlackData{channelId, botToken = "", payload, threadTs, webhookUrl = Just webhookUrl, projectIdCtx = "", teamIdCtx = ""}


-- | Attach project + workspace ids to a Slack notification for log
-- correlation. Does not affect routing.
withSlackContext :: Text -> Text -> Notification -> Notification
withSlackContext pid tid (SlackNotification sd) = SlackNotification sd{projectIdCtx = pid, teamIdCtx = tid}
withSlackContext _ _ n = n


discordNotification :: Text -> AE.Value -> Notification
discordNotification channelId payload = discordThreadedNotification channelId payload Nothing


discordThreadedNotification :: Text -> AE.Value -> Maybe Text -> Notification
discordThreadedNotification channelId payload replyToMessageId =
  DiscordNotification DiscordData{..}


whatsappNotification :: Text -> Text -> AE.Value -> Notification
whatsappNotification template to contentVariables =
  WhatsAppNotification WhatsAppData{..}


pagerdutyNotification :: Text -> PagerdutyAction -> Text -> Text -> PagerdutySeverity -> AE.Value -> Text -> Notification
pagerdutyNotification integrationKey eventAction dedupKey summary severity customDetails monitorUrl =
  PagerdutyNotification PagerdutyData{..}


-- | Rewrite every Block Kit block list in a Slack message — the top-level
-- @blocks@ and the one nested in our legacy @attachments@ colour-bar wrapper.
overBlocks :: ([AE.Value] -> [AE.Value]) -> AE.Value -> AE.Value
overBlocks f (AE.Object o) = AE.Object $ AEK.mapWithKey rewrite o
  where
    rewrite "blocks" (AE.Array bs) = AE.Array $ V.fromList $ f $ toList bs
    rewrite "attachments" (AE.Array atts) = AE.Array $ fmap (overBlocks f) atts
    rewrite _ v = v
overBlocks _ v = v


-- | Replace unavailable images with an explicit notice in every Block Kit list.
-- Preserve the block ID so an incident's original chart remains identifiable.
--
-- >>> let img = AE.object ["type" AE..= ("image" :: Text), "image_url" AE..= ("https://chart" :: Text), "block_id" AE..= ("incident_chart" :: Text)]
-- >>> let msg = AE.object ["blocks" AE..= ([img] :: [AE.Value])]
-- >>> replaceImages (const False) msg == msg
-- True
-- >>> let rendered = AE.encode (replaceImages (const True) msg)
-- >>> all (`T.isInfixOf` decodeUtf8 (toStrict rendered)) ["Chart unavailable", "incident_chart"] && not ("image_url" `T.isInfixOf` decodeUtf8 (toStrict rendered))
-- True
replaceImages :: (Text -> Bool) -> AE.Value -> AE.Value
replaceImages unavailable = overBlocks $ map \block -> case block of
  AE.Object obj
    | Just url <- imageUrl block
    , unavailable url ->
        AE.object
          $ [ "type" AE..= ("context" :: Text)
            , "elements" AE..= ([AE.object ["type" AE..= ("plain_text" :: Text), "text" AE..= ("Chart unavailable. Use the message links to inspect the data." :: Text)]] :: [AE.Value])
            ]
          <> ["block_id" AE..= identifier | Just identifier <- [AEK.lookup "block_id" obj]]
  _ -> block


imageUrl :: AE.Value -> Maybe Text
imageUrl (AE.Object b) | Just (AE.String "image") <- AEK.lookup "type" b, Just (AE.String u) <- AEK.lookup "image_url" b = Just u
imageUrl _ = Nothing


-- | Webhooks reject a top-level @channel@ — the target is baked into the URL.
-- This is the ONLY difference between what the two transports put on the wire:
-- both carry the same Block Kit, so an alert renders identically in every
-- channel. Do not reintroduce a webhook-specific renderer here (a former
-- @flattenForWebhook@ turned buttons into mrkdwn links, giving one alert two
-- looks depending on which channel it reached).
--
-- >>> let msg extra = AE.object (extra <> ["attachments" AE..= ([AE.object ["blocks" AE..= ([] :: [AE.Value])]] :: [AE.Value])])
-- >>> dropChannel (msg ["channel" AE..= ("C1" :: Text)]) == msg []
-- True
-- >>> dropChannel (msg []) == msg []
-- True
dropChannel :: AE.Value -> AE.Value
dropChannel (AE.Object obj) = AE.Object $ AEK.delete "channel" obj
dropChannel v = v


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
    EmailNotification _ -> pure Nothing
    WhatsAppNotification _ -> pure Nothing
    PagerdutyNotification _ -> pure Nothing
  DeliverSlack operation sd -> sendSlackRequest operation sd
  GetNotifications -> pure [] -- Production doesn't store notifications
  where
    sendSlack sd = do
      result <- sendSlackRequest SlackPost sd
      pure $ case result of SlackSent ts -> Just ts; _ -> Nothing

    sendSlackRequest operation sd = case sd.payload of
      AE.Object obj -> do
        let webhook = case operation of SlackPost -> sd.webhookUrl; SlackUpdate _ -> Nothing
            apiMethod = case operation of SlackPost -> "chat.postMessage"; SlackUpdate _ -> "chat.update"
            url = fromMaybe ("https://slack.com/api/" <> apiMethod) webhook
            routed = AEK.insert "channel" (AE.String sd.channelId) obj
            body = AE.Object $ case operation of
              SlackPost -> maybe routed (\ts -> AEK.insert "thread_ts" (AE.String ts) routed) sd.threadTs
              SlackUpdate ts -> AEK.insert "ts" (AE.String ts) $ AEK.delete "thread_ts" routed
            routedPayload = if isJust webhook then dropChannel body else body
            prepared = replaceImages ((> 3000) . T.length) routedPayload
            opts = defaults & header "Content-Type" .~ ["application/json"] & checkResponse ?~ (\_ _ -> pass)
            authenticated = if isJust webhook then opts else opts & header "Authorization" .~ [encodeUtf8 $ "Bearer " <> sd.botToken]
            context = AE.object ["project_id" AE..= sd.projectIdCtx, "team_id" AE..= sd.teamIdCtx, "channel_id" AE..= sd.channelId, "transport" AE..= if isJust webhook then "webhook" else apiMethod]
            post payload = do
              response <- liftIO $ try @SomeException $ timeout 30_000_000 $ postWith authenticated (toString url) payload
              pure $ case response of
                Right (Just re)
                  | statusCode (re ^. responseStatus) == 429 ->
                      SlackRateLimited $ max 1 $ fromMaybe 60 $ readMaybe $ toString $ decodeUtf8 @Text $ re ^. responseHeader "Retry-After"
                  | statusCode (re ^. responseStatus) >= 500 -> SlackAmbiguous
                  | isJust webhook ->
                      let txt = decodeUtf8 @Text $ toStrict $ re ^. responseBody
                       in if statusIsSuccessful (re ^. responseStatus) && txt `elem` ["ok", "\"ok\""]
                            then SlackAccepted
                            else SlackRejected txt
                  | otherwise -> case AE.decode @SlackResponse (re ^. responseBody) of
                      Just SlackResponse{ok = True, ts = Just ts} | statusIsSuccessful (re ^. responseStatus) -> SlackSent ts
                      Just SlackResponse{ok = False, error = Just reason} -> SlackRejected reason
                      _ -> SlackAmbiguous
                _ -> SlackAmbiguous
        when (prepared /= routedPayload) $ Log.logAttention "Slack chart URL exceeds the image limit; using a text fallback" context
        initial <- post prepared
        result <- case initial of
          SlackRejected "invalid_attachments" -> do
            Log.logAttention "Slack rejected the chart; retrying without the image" context
            post $ replaceImages (const True) prepared
          _ -> pure initial
        case result of
          SlackSent _ -> pass
          SlackAccepted -> pass
          SlackRateLimited seconds -> Log.logAttention "Slack delivery rate limited" (context, seconds)
          SlackRejected reason -> Log.logAttention "Slack rejected the message" (context, reason)
          SlackAmbiguous -> Log.logAttention "Slack delivery outcome is unknown; reconciliation required" context
        pure result
      _ -> pure $ SlackRejected "invalid_payload"

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
        Right re
          | statusIsSuccessful (re ^. responseStatus) ->
              pure $ (AE.decode (re ^. responseBody) :: Maybe AE.Value) >>= (^? key "id" . _String)
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
    liftIO $ atomicModifyIORef' ref (\notifications -> (notification : notifications, ()))
  SendNotificationWithReply notification -> do
    idx <- liftIO $ atomicModifyIORef' ref (\notifications -> (notification : notifications, length notifications + 1))
    pure $ case notification of
      SlackNotification _ -> Just $ "test-slack-ts-" <> show idx
      DiscordNotification _ -> Just $ "test-discord-id-" <> show idx
      EmailNotification _ -> Nothing
      WhatsAppNotification _ -> Nothing
      PagerdutyNotification _ -> Nothing
  DeliverSlack operation sd -> do
    let captured = case (operation, sd.payload) of
          (SlackUpdate ts, AE.Object obj) -> (sd :: SlackData){payload = AE.Object $ AEK.insert "ts" (AE.String ts) obj}
          _ -> sd
    idx <- liftIO $ atomicModifyIORef' ref (\notifications -> (SlackNotification captured : notifications, length notifications + 1))
    let timestamp = "1788900000." <> T.justifyRight 6 '0' (show idx)
    pure $ case operation of
      SlackUpdate ts -> SlackSent ts
      SlackPost -> maybe (SlackSent timestamp) (const SlackAccepted) sd.webhookUrl
  GetNotifications -> liftIO $ reverse <$> readIORef ref
