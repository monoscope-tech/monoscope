module Models.Apis.Integrations (
  SlackData (..),
  DiscordData (..),
  insertAccessToken,
  getSlackDataByTeamId,
  insertDiscordData,
  getDiscordDataByProjectId,
  getProjectSlackData,
  updateSlackDefaultChannel,
  getDashboardsForDiscord,
  getDashboardsForSlack,
  getDashboardsForWhatsapp,
  getDiscordData,
  deleteSlackData,
  deleteDiscordData,
) where

import Data.Effectful.Hasql qualified as Hasql
import Deriving.Aeson qualified as AE
import Deriving.Aeson qualified as DAE
import Effectful
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Relude
import System.Types (DB)


-- | OAuth-time Slack credentials + the channel the app was installed to.
--
-- @channelId@ and @channelName@ record the channel the user picked during
-- OAuth. @webhookUrl@ is the channel-bound incoming webhook Slack issues at
-- install time — POSTing to it delivers to that exact channel without
-- requiring the bot user to be a member. That's the only path that works for
-- private channels unless the user manually @/invite@s the bot.
--
-- Alert routing to this channel uses @webhookUrl@; routing to any additional
-- channels the user adds (via /here or the dropdown) uses chat.postMessage
-- with @botToken@ and does require bot membership. Threading is available
-- only on the chat.postMessage path — webhooks don't accept thread_ts.
data SlackData = SlackData
  { projectId :: Projects.ProjectId
  , teamId :: Text
  , teamName :: Maybe Text
  , botToken :: Text
  , channelId :: Text
  , channelName :: Maybe Text
  , webhookUrl :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (HI.DecodeRow, NFData)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SlackData


insertAccessToken :: DB es => Projects.ProjectId -> Text -> Text -> Text -> Text -> Text -> Text -> Eff es Int64
insertAccessToken pid teamId channelId teamName botToken channelName webhookUrl =
  Hasql.interpExecute
    [HI.sql|INSERT INTO apis.slack
               (project_id, team_id, channel_id, team_name, bot_token, channel_name, webhook_url)
               VALUES (#{pid},#{teamId},#{channelId},#{teamName},#{botToken},#{channelName},#{webhookUrl})
               ON CONFLICT (project_id)
               DO UPDATE SET team_id = EXCLUDED.team_id, channel_id = EXCLUDED.channel_id, team_name = EXCLUDED.team_name, bot_token = EXCLUDED.bot_token, channel_name = EXCLUDED.channel_name, webhook_url = EXCLUDED.webhook_url |]


getProjectSlackData :: DB es => Projects.ProjectId -> Eff es (Maybe SlackData)
getProjectSlackData pid = Hasql.interpOne [HI.sql|SELECT project_id, team_id, team_name, bot_token, channel_id, channel_name, webhook_url FROM apis.slack WHERE project_id = #{pid} |]


getSlackDataByTeamId :: DB es => Text -> Eff es (Maybe SlackData)
getSlackDataByTeamId teamId = Hasql.interpOne [HI.sql|SELECT project_id, team_id, team_name, bot_token, channel_id, channel_name, webhook_url FROM apis.slack WHERE team_id = #{teamId} |]


-- | Update the OAuth-time default channel cached on apis.slack.
--
-- Note: this is NOT purely display metadata. @channel_id@ is the discriminator
-- @Pkg.Mail.sendSlackAlertWith@ uses to decide webhook-vs-chat-API routing
-- (@cid == sd.channelId@). Updating @channel_id@ here without also re-issuing
-- @webhook_url@ will desync the pair — the webhook is channel-bound at
-- install time and cannot be moved. Callers that change this should either
-- re-run OAuth (which rewrites both) or clear @webhook_url@ so the alert path
-- falls through to chat.postMessage.
updateSlackDefaultChannel :: DB es => Text -> Text -> Maybe Text -> Eff es Int64
updateSlackDefaultChannel teamId channelId channelName =
  Hasql.interpExecute
    [HI.sql|UPDATE apis.slack SET channel_id = #{channelId}, channel_name = #{channelName} WHERE team_id = #{teamId}|]


data DiscordData = DiscordData
  { projectId :: Projects.ProjectId
  , guildId :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (HI.DecodeRow, NFData)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] DiscordData


insertDiscordData :: DB es => Projects.ProjectId -> Text -> Eff es Int64
insertDiscordData pid guildId =
  Hasql.interpExecute
    [HI.sql|INSERT INTO apis.discord
               (project_id, guild_id)
               VALUES (#{pid},#{guildId})
               ON CONFLICT (project_id)
               DO UPDATE SET guild_id = EXCLUDED.guild_id |]


getDiscordData :: DB es => Text -> Eff es (Maybe DiscordData)
getDiscordData guildId = Hasql.interpOne [HI.sql|SELECT project_id, guild_id FROM apis.discord WHERE guild_id = #{guildId} |]


getDiscordDataByProjectId :: DB es => Projects.ProjectId -> Eff es (Maybe DiscordData)
getDiscordDataByProjectId pid = Hasql.interpOne [HI.sql|SELECT project_id, guild_id FROM apis.discord WHERE project_id = #{pid} |]


getDashboardsForSlack :: DB es => Text -> Eff es [(Text, Text)]
getDashboardsForSlack teamId = Hasql.interp [HI.sql|SELECT d.title, d.id::text FROM projects.dashboards d JOIN apis.slack s ON d.project_id = s.project_id WHERE s.team_id = #{teamId}|]


getDashboardsForWhatsapp :: DB es => Text -> Eff es [(Text, Text)]
getDashboardsForWhatsapp number =
  Hasql.interp
    [HI.sql|SELECT d.title, d.id::text FROM projects.dashboards d
            JOIN projects.teams t ON t.project_id = d.project_id
            WHERE t.is_everyone = TRUE AND t.deleted_at IS NULL
              AND #{number} = ANY(t.phone_numbers)|]


getDashboardsForDiscord :: DB es => Text -> Eff es [(Text, Text)]
getDashboardsForDiscord guildId = Hasql.interp [HI.sql|SELECT d.title, d.id::text FROM projects.dashboards d JOIN apis.discord dd ON d.project_id = dd.project_id where guild_id=#{guildId}|]


deleteSlackData :: DB es => Projects.ProjectId -> Eff es Int64
deleteSlackData pid = Hasql.interpExecute [HI.sql|DELETE FROM apis.slack WHERE project_id = #{pid}|]


deleteDiscordData :: DB es => Projects.ProjectId -> Eff es Int64
deleteDiscordData pid = Hasql.interpExecute [HI.sql|DELETE FROM apis.discord WHERE project_id = #{pid}|]
