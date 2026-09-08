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
  SlackInstall (..),
  createSlackInstall,
  consumeSlackInstall,
) where

import Data.Effectful.Hasql qualified as Hasql
import Deriving.Aeson qualified as AE
import Deriving.Aeson.Stock qualified as DAE
import Effectful
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId)
import Relude
import System.Types (DB)


data SlackInstall = SlackInstall {projectId :: Projects.ProjectId, onboarding :: Bool}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- | Installation modifies credentials shared by the whole project. Check current
-- admin membership, rather than the browser session's cached project list.
createSlackInstall :: DB es => UUIDId "slack_install" -> Projects.UserId -> Projects.ProjectId -> Bool -> Eff es Bool
createSlackInstall stateId userId projectId onboarding = do
  affected <-
    Hasql.interpExecute
      [HI.sql|INSERT INTO apis.slack_install_requests (id, user_id, project_id, onboarding)
      SELECT #{stateId}, #{userId}, #{projectId}, #{onboarding}
      FROM projects.project_members
      WHERE project_id = #{projectId} AND user_id = #{userId}
        AND active AND deleted_at IS NULL AND permission = 'admin'|]
  pure $ affected == 1


-- | Consume before contacting Slack. A failed exchange requires a fresh request.
-- The callback must have the same authenticated Monoscope user as the initiator.
consumeSlackInstall :: DB es => UUIDId "slack_install" -> Projects.UserId -> Eff es (Maybe SlackInstall)
consumeSlackInstall stateId userId =
  Hasql.interpOne
    [HI.sql|DELETE FROM apis.slack_install_requests request
    USING projects.project_members member
    WHERE request.id = #{stateId} AND request.user_id = #{userId} AND request.expires_at > now()
      AND member.project_id = request.project_id AND member.user_id = request.user_id
      AND member.active AND member.deleted_at IS NULL AND member.permission = 'admin'
    RETURNING request.project_id, request.onboarding|]


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
-- with @botToken@ and does require bot membership. Both transports accept
-- thread_ts; only chat.postMessage returns the timestamp for a new root.
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
  deriving (AE.FromJSON) via DAE.Snake SlackData


insertAccessToken :: DB es => Projects.ProjectId -> Text -> Text -> Text -> Text -> Text -> Text -> Eff es Int64
insertAccessToken pid teamId channelId teamName botToken channelName webhookUrl =
  Hasql.interpExecute
    [HI.sql|INSERT INTO apis.slack
               (project_id, team_id, channel_id, team_name, bot_token, channel_name, webhook_url)
               VALUES (#{pid},#{teamId},#{channelId},#{teamName},#{botToken},#{channelName},#{webhookUrl})
               ON CONFLICT (project_id)
               DO UPDATE SET team_id = EXCLUDED.team_id, channel_id = EXCLUDED.channel_id, team_name = EXCLUDED.team_name, bot_token = EXCLUDED.bot_token, channel_name = EXCLUDED.channel_name, webhook_url = EXCLUDED.webhook_url |]


-- | Column order must match 'SlackData''s field order — 'HI.DecodeRow' is positional.
selectSlack :: HI.Sql
selectSlack = [HI.sql|SELECT project_id, team_id, team_name, bot_token, channel_id, channel_name, webhook_url FROM apis.slack |]


getProjectSlackData :: DB es => Projects.ProjectId -> Eff es (Maybe SlackData)
getProjectSlackData pid = Hasql.interpOne (selectSlack <> [HI.sql|WHERE project_id = #{pid}|])


getSlackDataByTeamId :: DB es => Text -> Eff es (Maybe SlackData)
getSlackDataByTeamId teamId = Hasql.interpOne (selectSlack <> [HI.sql|WHERE team_id = #{teamId}|])


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
  deriving (AE.FromJSON) via DAE.Snake DiscordData


insertDiscordData :: DB es => Projects.ProjectId -> Text -> Eff es Int64
insertDiscordData pid guildId =
  Hasql.interpExecute
    [HI.sql|INSERT INTO apis.discord
               (project_id, guild_id)
               VALUES (#{pid},#{guildId})
               ON CONFLICT (project_id)
               DO UPDATE SET guild_id = EXCLUDED.guild_id |]


selectDiscord :: HI.Sql
selectDiscord = [HI.sql|SELECT project_id, guild_id FROM apis.discord |]


getDiscordData :: DB es => Text -> Eff es (Maybe DiscordData)
getDiscordData guildId = Hasql.interpOne (selectDiscord <> [HI.sql|WHERE guild_id = #{guildId}|])


getDiscordDataByProjectId :: DB es => Projects.ProjectId -> Eff es (Maybe DiscordData)
getDiscordDataByProjectId pid = Hasql.interpOne (selectDiscord <> [HI.sql|WHERE project_id = #{pid}|])


-- | Shared (d.title, d.id::text) dashboard lookup; each caller supplies the
-- platform-specific JOIN + WHERE fragment (Slack/Discord share the same shape).
dashboardsByProjectJoin :: DB es => HI.Sql -> Eff es [(Text, Text)]
dashboardsByProjectJoin joinWhere = Hasql.interp ([HI.sql|SELECT d.title, d.id::text FROM projects.dashboards d |] <> joinWhere)


getDashboardsForSlack :: DB es => Text -> Eff es [(Text, Text)]
getDashboardsForSlack teamId = dashboardsByProjectJoin [HI.sql|JOIN apis.slack s ON d.project_id = s.project_id WHERE s.team_id = #{teamId}|]


getDashboardsForWhatsapp :: DB es => Text -> Eff es [(Text, Text)]
getDashboardsForWhatsapp number =
  dashboardsByProjectJoin
    [HI.sql|JOIN projects.teams t ON t.project_id = d.project_id
            WHERE t.is_everyone = TRUE AND t.deleted_at IS NULL
              AND #{number} = ANY(t.phone_numbers)|]


getDashboardsForDiscord :: DB es => Text -> Eff es [(Text, Text)]
getDashboardsForDiscord guildId = dashboardsByProjectJoin [HI.sql|JOIN apis.discord dd ON d.project_id = dd.project_id WHERE dd.guild_id = #{guildId}|]


deleteSlackData :: DB es => Projects.ProjectId -> Eff es Int64
deleteSlackData pid = Hasql.interpExecute [HI.sql|DELETE FROM apis.slack WHERE project_id = #{pid}|]
