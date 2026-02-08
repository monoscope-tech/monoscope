module Pages.Bots.BotFixtures (
  -- * Slack Fixtures
  slackInteraction,
  slackEventCallback,
  slackThreadedEvent,
  slackActionPayload,

  -- * Discord Fixtures
  discordPingPayload,
  discordCommandInteraction,
  discordThreadInteraction,

  -- * WhatsApp Fixtures
  twilioWhatsAppMessage,
  twilioWhatsAppPrompt,
  twilioWhatsAppDashboard,
) where

import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Pages.Bots.Discord (DiscordInteraction)
import Pages.Bots.Slack (SlackEventPayload (..), SlackInteraction (..))
import Pages.Bots.Whatsapp (TwilioWhatsAppMessage (..))
import Relude


-- * Slack Fixtures


slackInteraction :: Text -> Text -> Text -> SlackInteraction
slackInteraction cmd query teamId =
  SlackInteraction
    { team_id = teamId
    , command = cmd
    , text = query
    , response_url = "https://hooks.slack.com/commands/" <> teamId <> "/response"
    , trigger_id = "trigger_" <> teamId
    , channel_id = "C_TEST_CHANNEL"
    , channel_name = "general"
    , user_id = "U_TEST_USER"
    }


slackEventCallback :: Text -> Text -> Text -> Text -> Maybe Text -> AE.Value
slackEventCallback teamId channelId userText ts threadTsM =
  [aesonQQ|{
    "type": "event_callback",
    "token": "test_token",
    "team_id": #{teamId},
    "api_app_id": "A_TEST_APP",
    "event": {
      "type": "message",
      "user": "U_TEST_USER",
      "text": #{userText},
      "ts": #{ts},
      "channel": #{channelId},
      "event_ts": #{ts},
      "thread_ts": #{maybeToJSON threadTsM}
    },
    "event_id": "Ev_TEST",
    "event_time": 1700000000
  }|]
  where
    maybeToJSON :: Maybe Text -> AE.Value
    maybeToJSON = maybe AE.Null AE.String


slackThreadedEvent :: Text -> Text -> Text -> Text -> Text -> AE.Value
slackThreadedEvent teamId channelId userText ts threadTs =
  slackEventCallback teamId channelId userText ts (Just threadTs)


slackActionPayload :: Text -> AE.Value
slackActionPayload actionType =
  [aesonQQ|{
    "type": #{actionType},
    "token": "test_token",
    "trigger_id": "trigger_123",
    "view": {
      "private_metadata": "C_TEST___proj123",
      "blocks": [],
      "id": "view_123",
      "state": null
    },
    "actions": null,
    "user": {
      "id": "U_TEST_USER",
      "username": "testuser",
      "team_id": "T_TEST"
    }
  }|]


-- * Discord Fixtures


discordPingPayload :: BS.ByteString
discordPingPayload = toStrict $ AE.encode [aesonQQ|{"type": 1, "id": "123", "token": "tok"}|]


discordCommandInteraction :: Text -> Text -> BS.ByteString
discordCommandInteraction commandName query =
  toStrict
    $ AE.encode
      [aesonQQ|{
    "type": 2,
    "id": "interaction_123",
    "token": "interaction_token",
    "data": {
      "name": #{commandName},
      "options": [{"name": "question", "value": #{query}}]
    },
    "channel_id": "chan_123",
    "guild_id": "guild_test_123",
    "channel": null
  }|]


discordThreadInteraction :: Text -> Text -> Text -> BS.ByteString
discordThreadInteraction commandName query threadId =
  toStrict
    $ AE.encode
      [aesonQQ|{
    "type": 2,
    "id": "interaction_123",
    "token": "interaction_token",
    "data": {
      "name": #{commandName},
      "options": [{"name": "question", "value": #{query}}]
    },
    "channel_id": #{threadId},
    "guild_id": "guild_test_123",
    "channel": {
      "id": #{threadId},
      "name": "test-thread",
      "guild_id": "guild_test_123",
      "type": 11,
      "parent_id": "chan_123",
      "owner_id": "U_TEST",
      "thread_metadata": null
    }
  }|]


-- * WhatsApp Fixtures


twilioWhatsAppMessage :: Text -> Text -> Text -> TwilioWhatsAppMessage
twilioWhatsAppMessage from to body =
  TwilioWhatsAppMessage
    { messageSid = "SM_TEST_SID"
    , smsSid = "SM_TEST_SID"
    , smsMessageSid = "SM_TEST_SID"
    , accountSid = "AC_TEST_ACCOUNT"
    , messagingServiceSid = Nothing
    , from = from
    , to = to
    , body = body
    , numMedia = 0
    , numSegments = 1
    , profileName = Just "Test User"
    , waId = Just "1234567890"
    , forwarded = Nothing
    , frequentlyForwarded = Nothing
    , buttonText = Nothing
    }


twilioWhatsAppPrompt :: Text -> Text -> TwilioWhatsAppMessage
twilioWhatsAppPrompt fromNumber query =
  twilioWhatsAppMessage ("whatsapp:" <> fromNumber) "whatsapp:+14155551234" query


twilioWhatsAppDashboard :: Text -> TwilioWhatsAppMessage
twilioWhatsAppDashboard fromNumber =
  twilioWhatsAppMessage ("whatsapp:" <> fromNumber) "whatsapp:+14155551234" "/dashboard"
