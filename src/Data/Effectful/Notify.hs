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
import Data.Aeson.Lens (key, _Bool, _String)
import Data.Aeson.QQ (aesonQQ)
import Data.Text qualified as T
import Data.Text.Display (Display, display)
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH
import Network.HTTP.Types (statusIsSuccessful)
import Network.Mail.Mime (Address (..), Mail (..), htmlPart)
import Network.Mail.SMTP (sendMailWithLoginSTARTTLS', sendMailWithLoginTLS')
import Network.Wreq (FormParam ((:=)), auth, basicAuth, checkResponse, defaults, header, postWith, responseBody, responseStatus)
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
-- screen). Threading is unavailable via webhook; @threadTs@ is ignored.
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


type instance DispatchOf Notify = 'Dynamic


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
-- membership; does not support threading.
slackWebhookNotification :: Text -> Text -> AE.Value -> Notification
slackWebhookNotification webhookUrl channelId payload =
  SlackNotification SlackData{channelId, botToken = "", payload, threadTs = Nothing, webhookUrl = Just webhookUrl, projectIdCtx = "", teamIdCtx = ""}


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


-- | Slack rejects an @image_url@ of 3001+ characters with @invalid_attachments@
-- — and rejects the whole message with it, chart and alert together. Our signed
-- widget URLs are allowed to reach 8000 characters, so drop the image rather
-- than lose the alert.
--
-- >>> let img u = AE.object ["type" AE..= ("image" :: Text), "image_url" AE..= (u :: Text)]
-- >>> let txt = AE.object ["type" AE..= ("section" :: Text)]
-- >>> let msg bs = AE.object ["attachments" AE..= ([AE.object ["blocks" AE..= (bs :: [AE.Value])]] :: [AE.Value])]
-- >>> dropOversizedImages (msg [txt, img (T.replicate 3001 "x")]) == msg [txt]
-- True
-- >>> dropOversizedImages (msg [txt, img "https://short"]) == msg [txt, img "https://short"]
-- True
dropOversizedImages :: AE.Value -> AE.Value
dropOversizedImages = overBlocks $ filter \b -> case imageUrl b of
  Just u -> T.length u <= 3000
  Nothing -> True


-- | Strip image blocks entirely — the retry after Slack refuses to download a
-- chart it would otherwise have embedded.
--
-- >>> let img = AE.object ["type" AE..= ("image" :: Text), "image_url" AE..= ("u" :: Text)]
-- >>> let txt = AE.object ["type" AE..= ("section" :: Text)]
-- >>> let blocks bs = AE.object ["blocks" AE..= (bs :: [AE.Value])]
-- >>> dropImageBlocks (blocks [txt, img]) == blocks [txt]
-- True
dropImageBlocks :: AE.Value -> AE.Value
dropImageBlocks = overBlocks $ filter (isNothing . imageUrl)


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
  GetNotifications -> pure [] -- Production doesn't store notifications
  where
    sendSlack :: (IOE :> es, Log :> es, Reader Config.AuthContext :> es) => SlackData -> Eff es (Maybe Text)
    sendSlack sd = case sd.webhookUrl of
      Just url -> sendSlackWebhook sd url
      Nothing -> sendSlackChatApi sd

    -- chat.postMessage: requires bot membership; supports threading.
    --
    -- Slack fetches every @image_url@ while validating the request, so a chart
    -- our renderer is slow to produce (or serves with the wrong content type)
    -- comes back as @invalid_attachments@ and takes the ENTIRE alert with it.
    -- An alert without its chart still tells an on-call engineer what broke, so
    -- a rejection that mentions the attachments is retried once with the image
    -- blocks stripped rather than dropped.
    sendSlackChatApi sd = case sd.payload of
      AE.Object obj -> do
        let withThread = maybe obj (\ts -> AEK.insert "thread_ts" (AE.String ts) obj) sd.threadTs
            msg = AE.Object $ AEK.insert "channel" (AE.String sd.channelId) withThread
            opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bearer " <> sd.botToken]
            fail_ tag extra = Nothing <$ Log.logAttention ("Slack chat.postMessage " <> tag) (chatApiLog sd extra)
            -- Slack always returns HTTP 200; the real success flag is in the JSON body.
            -- not_in_channel / channel_not_found / is_archived all come back as 200 with
            -- ok:false. Treating 200 as success hides the "bot not invited" class of bug.
            post body = liftIO (try @SomeException $ postWith opts "https://slack.com/api/chat.postMessage" body)
            outcome = \case
              Right re
                | statusIsSuccessful (re ^. responseStatus) ->
                    let b = AE.decode (re ^. responseBody) :: Maybe AE.Value
                     in Right (b >>= (^? key "ok" . _Bool), b >>= (^? key "ts" . _String), fromMaybe "unknown" (b >>= (^? key "error" . _String)))
              Right re -> Left ("HTTP failure" :: Text, ["status" AE..= show @Text (re ^. responseStatus)])
              Left ex -> Left ("exception", ["error" AE..= displayException ex])
        post (dropOversizedImages msg) >>= \r -> case outcome r of
          Left (tag, extra) -> fail_ tag extra
          Right (Just True, ts, _) -> pure ts
          Right (_, _, err)
            | err == "invalid_attachments" -> do
                Log.logAttention "Slack chat.postMessage rejected the chart image; retrying without it" (chatApiLog sd ["error" AE..= err])
                post (dropImageBlocks msg) >>= \r2 -> case outcome r2 of
                  Right (Just True, ts, _) -> pure ts
                  Right (_, _, err2) -> fail_ "rejected by API" ["error" AE..= err2, "retried_without_image" AE..= True]
                  Left (tag, extra) -> fail_ tag extra
            | otherwise -> fail_ "rejected by API" ["error" AE..= err]
      _ -> Nothing <$ Log.logAttention "Slack notification message is not an object" (slackLogCtx sd [])

    -- Incoming webhook: channel-bound, no bot membership needed, no thread_ts.
    -- Slack rejects the "channel" field on webhooks, so it is stripped; the
    -- Block Kit payload is otherwise sent verbatim — identical to what
    -- chat.postMessage receives, so an alert looks the same in every channel.
    --
    -- Webhooks DO accept Block Kit nested in legacy attachments, including
    -- image/context blocks and styled buttons (probed against a live webhook
    -- 2026-09-03). An earlier `invalid_blocks` reading blamed Block Kit for
    -- what was really a rejected image URL — hence the shared image retry below.
    -- Webhook endpoints return HTTP 200 + body "ok" on success.
    sendSlackWebhook sd url = do
      -- checkResponse = nop: wreq otherwise throws on 4xx and we lose the body
      -- which is where Slack tells us which block/field it rejected.
      let opts = defaults & header "Content-Type" .~ ["application/json"] & checkResponse ?~ (\_ _ -> pass)
          stripped = dropChannel $ dropOversizedImages sd.payload
          fail_ tag extra body = Nothing <$ Log.logAttention ("Slack webhook " <> tag) (webhookLog sd url (("payload" AE..= body) : extra))
          post body = liftIO (try @SomeException $ postWith opts (toString url) body)
          -- A rejection is HTTP 400 with the reason as the bare body
          -- ("invalid_attachments"), so the status is context for the log, not
          -- the classifier — keying off it would skip the retry below entirely.
          outcome = \case
            Right re ->
              let b = decodeUtf8 @Text $ toStrict $ re ^. responseBody
                  ok = statusIsSuccessful (re ^. responseStatus) && (b == "ok" || b == "\"ok\"")
               in Right $ if ok then Nothing else Just (b, ["status" AE..= show @Text (re ^. responseStatus), "body" AE..= b])
            Left ex -> Left ("exception" :: Text, ["error" AE..= displayException ex])
      post stripped >>= \r -> case outcome r of
        Left (tag, extra) -> fail_ tag extra stripped
        Right Nothing -> pure Nothing
        -- Same failure mode as the chat API: Slack downloads image_url while
        -- validating, so a slow or wrong-typed chart takes the whole alert down.
        -- An alert without its chart still tells an on-call engineer what broke.
        Right (Just (err, extra))
          | "invalid_attachments" `T.isInfixOf` err -> do
              Log.logAttention "Slack webhook rejected the chart image; retrying without it" (webhookLog sd url extra)
              let retry = dropImageBlocks stripped
              post retry >>= \r2 -> case outcome r2 of
                Right Nothing -> pure Nothing
                Right (Just (_, extra2)) -> fail_ "rejected the alert" (("retried_without_image" AE..= True) : extra2) retry
                Left (tag, extra2) -> fail_ tag extra2 retry
          | otherwise -> fail_ "rejected the alert" extra stripped

    -- Log context builders with transport pre-tagged at the call site.
    slackLogCtx sd extra =
      AE.object
        $ ["project_id" AE..= sd.projectIdCtx, "team_id" AE..= sd.teamIdCtx, "channel_id" AE..= sd.channelId]
        <> extra
    chatApiLog sd extra = slackLogCtx sd (("transport" AE..= ("chat.postMessage" :: Text)) : extra)
    webhookLog sd url extra = slackLogCtx sd (("transport" AE..= ("webhook" :: Text)) : ("webhook_suffix" AE..= redactedWebhookSuffix url) : extra)

    -- Webhook URLs embed secrets (hooks.slack.com/services/T…/B…/SECRET).
    -- Only include a short prefix for debugging; never the secret.
    redactedWebhookSuffix url = T.take 16 (fromMaybe url $ T.stripPrefix "https://hooks.slack.com/services/" url) <> "…"

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
    liftIO $ modifyIORef ref (notification :)
  SendNotificationWithReply notification -> do
    notifications <- liftIO $ readIORef ref
    liftIO $ modifyIORef ref (notification :)
    let idx = length notifications + 1
    pure $ case notification of
      SlackNotification _ -> Just $ "test-slack-ts-" <> show idx
      DiscordNotification _ -> Just $ "test-discord-id-" <> show idx
      EmailNotification _ -> Nothing
      WhatsAppNotification _ -> Nothing
      PagerdutyNotification _ -> Nothing
  GetNotifications -> liftIO $ reverse <$> readIORef ref
