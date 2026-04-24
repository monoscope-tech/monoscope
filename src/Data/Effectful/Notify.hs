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
    sendSlack sd = case sd.webhookUrl of
      Just url -> sendSlackWebhook sd url
      Nothing -> sendSlackChatApi sd

    -- chat.postMessage: requires bot membership; supports threading.
    sendSlackChatApi sd = case sd.payload of
      AE.Object obj -> do
        let withThread = maybe obj (\ts -> AEK.insert "thread_ts" (AE.String ts) obj) sd.threadTs
            msg = AE.Object $ AEK.insert "channel" (AE.String sd.channelId) withThread
            opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bearer " <> sd.botToken]
            fail_ tag extra = Nothing <$ Log.logAttention ("Slack chat.postMessage " <> tag) (chatApiLog sd extra)
        -- Slack always returns HTTP 200; the real success flag is in the JSON body.
        -- not_in_channel / channel_not_found / is_archived all come back as 200 with
        -- ok:false. Treating 200 as success hides the "bot not invited" class of bug.
        liftIO (try @SomeException $ postWith opts "https://slack.com/api/chat.postMessage" msg) >>= \case
          Right re
            | statusIsSuccessful (re ^. responseStatus) ->
                let body = AE.decode (re ^. responseBody) :: Maybe AE.Value
                 in case body >>= (^? key "ok" . _Bool) of
                      Just True -> pure $ body >>= (^? key "ts" . _String)
                      _ -> fail_ "rejected by API" ["error" AE..= fromMaybe "unknown" (body >>= (^? key "error" . _String))]
          Right re -> fail_ "HTTP failure" ["status" AE..= show @Text (re ^. responseStatus)]
          Left ex -> fail_ "exception" ["error" AE..= displayException ex]
      _ -> Nothing <$ Log.logAttention "Slack notification message is not an object" (slackLogCtx sd [])

    -- Incoming webhook: channel-bound, no bot membership needed, no thread_ts.
    -- Slack rejects the "channel" field on webhooks. The OAuth-time webhook
    -- endpoint (hooks.slack.com/services/T.../B.../SECRET) also rejects Block
    -- Kit nested inside legacy "attachments" with invalid_attachments, so we
    -- lift the inner blocks to the top level and drop the attachment wrapper
    -- (losing only the color accent bar). Webhook endpoints return HTTP 200 +
    -- body "ok" on success; we surface semantic failures as attention logs.
    sendSlackWebhook sd url = do
      -- checkResponse = nop: wreq otherwise throws on 4xx and we lose the body
      -- which is where Slack tells us which block/field it rejected.
      let opts = defaults & header "Content-Type" .~ ["application/json"] & checkResponse ?~ (\_ _ -> pass)
          cleaned = flattenForWebhook sd.payload
          fail_ tag extra = Nothing <$ Log.logAttention ("Slack webhook " <> tag) (webhookLog sd url (("payload" AE..= cleaned) : extra))
      liftIO (try @SomeException $ postWith opts (toString url) cleaned) >>= \case
        Right re
          | statusIsSuccessful (re ^. responseStatus) ->
              let body = re ^. responseBody
               in if body == "ok" || body == "\"ok\""
                    then pure Nothing
                    else fail_ "returned non-ok body" ["body" AE..= decodeUtf8 @Text (toStrict body)]
          | otherwise -> fail_ "HTTP failure" ["status" AE..= show @Text (re ^. responseStatus), "body" AE..= decodeUtf8 @Text (toStrict (re ^. responseBody))]
        Left ex -> fail_ "exception" ["error" AE..= displayException ex]

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

    -- Incoming webhooks created via the legacy @incoming-webhook@ OAuth scope
    -- (what Slack installs during app OAuth) reject Block Kit with an opaque
    -- @invalid_blocks@ error — both nested in attachments and at the top level.
    -- They only reliably accept @text@ + legacy @attachments@ fields.
    --
    -- So for the webhook transport, we walk the Block Kit payload and render
    -- it to a single @text@ string in Slack mrkdwn (links, bold, code blocks
    -- all still work). Image blocks are lifted to the legacy attachment's
    -- @image_url@ so Slack renders the chart inline instead of auto-unfurling
    -- a bare signed URL into a file-attachment-style preview.
    flattenForWebhook :: AE.Value -> AE.Value
    flattenForWebhook payload =
      let blocks = extractBlocks payload
          rendered = T.intercalate "\n\n" $ mapMaybe renderBlock blocks
          text = if T.null rendered then "Monoscope alert" else rendered
          imageUrl = listToMaybe [u | AE.Object b <- blocks, Just (AE.String "image") <- [AEK.lookup "type" b], Just (AE.String u) <- [AEK.lookup "image_url" b]]
          color = extractColor payload
          attachment =
            AE.object
              $ catMaybes
                [ Just $ "fallback" AE..= ("Monoscope alert" :: Text)
                , ("color" AE..=) <$> color
                , ("image_url" AE..=) <$> imageUrl
                ]
          base = ["text" AE..= text]
       in AE.object $ case (imageUrl, color) of
            (Nothing, Nothing) -> base
            _ -> base <> ["attachments" AE..= AE.Array (V.singleton attachment)]

    extractColor :: AE.Value -> Maybe Text
    extractColor (AE.Object obj)
      | Just (AE.Array atts) <- AEK.lookup "attachments" obj
      , (AE.Object att : _) <- toList atts
      , Just (AE.String c) <- AEK.lookup "color" att =
          Just c
    extractColor _ = Nothing

    -- Blocks may live at top level or inside the first legacy attachment
    -- (our @slackAttachment@ color-bar wrapper).
    extractBlocks :: AE.Value -> [AE.Value]
    extractBlocks = \case
      AE.Object obj
        | Just (AE.Array bs) <- AEK.lookup "blocks" obj -> toList bs
        | Just (AE.Array atts) <- AEK.lookup "attachments" obj
        , (AE.Object att : _) <- toList atts
        , Just (AE.Array bs) <- AEK.lookup "blocks" att ->
            toList bs
      _ -> []

    renderBlock :: AE.Value -> Maybe Text
    renderBlock (AE.Object b) = case AEK.lookup "type" b of
      Just (AE.String "section") -> nestedText b
      Just (AE.String "context")
        | Just (AE.Array els) <- AEK.lookup "elements" b ->
            Just $ T.intercalate "  ·  " [t | AE.Object e <- toList els, Just (AE.String t) <- [AEK.lookup "text" e]]
      Just (AE.String "actions")
        | Just (AE.Array els) <- AEK.lookup "elements" b ->
            let linkOf (AE.Object e)
                  | Just (AE.String "button") <- AEK.lookup "type" e
                  , Just (AE.String u) <- AEK.lookup "url" e
                  , Just label <- nestedText e =
                      Just $ "<" <> u <> "|" <> label <> ">"
                linkOf _ = Nothing
             in Just $ T.intercalate " · " $ mapMaybe linkOf (toList els)
      -- Image blocks are lifted to the legacy attachment's @image_url@
      -- field in @flattenForWebhook@ — never emit the signed URL as text.
      Just (AE.String "image") -> Nothing
      _ -> Nothing
    renderBlock _ = Nothing

    -- Read the string out of a nested {text: {type, text: "..."}} shape used
    -- by section blocks and button elements.
    nestedText :: AE.Object -> Maybe Text
    nestedText o = case AEK.lookup "text" o of
      Just (AE.Object inner) | Just (AE.String t) <- AEK.lookup "text" inner -> Just t
      _ -> Nothing

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
      _ -> Nothing
  GetNotifications -> liftIO $ reverse <$> readIORef ref
