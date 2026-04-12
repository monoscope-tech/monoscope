module Models.Apis.Integrations (
  SlackData (..),
  DiscordData (..),
  PagerdutyData (..),
  insertAccessToken,
  getSlackDataByTeamId,
  insertDiscordData,
  updateDiscordNotificationChannel,
  getDiscordDataByProjectId,
  getProjectSlackData,
  updateSlackNotificationChannel,
  getDashboardsForDiscord,
  getDashboardsForSlack,
  getDashboardsForWhatsapp,
  getDiscordData,
  getPagerdutyByProjectId,
  insertPagerdutyData,
  deletePagerdutyData,
  deleteSlackData,
) where

import Data.Effectful.Hasql qualified as Hasql
import Deriving.Aeson qualified as AE
import Deriving.Aeson qualified as DAE
import Effectful
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Relude
import System.Types (DB)


data SlackData = SlackData
  { projectId :: Projects.ProjectId
  , webhookUrl :: Text
  , teamId :: Text
  , channelId :: Text
  , teamName :: Maybe Text
  , botToken :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (HI.DecodeRow, NFData)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SlackData


insertAccessToken :: DB es => Projects.ProjectId -> Text -> Text -> Text -> Text -> Text -> Eff es Int64
insertAccessToken pid webhookUrl teamId channelId teamName botToken =
  Hasql.interpExecute
    [HI.sql|INSERT INTO apis.slack
               (project_id, webhook_url, team_id, channel_id, team_name, bot_token)
               VALUES (#{pid},#{webhookUrl},#{teamId},#{channelId},#{teamName},#{botToken})
               ON CONFLICT (project_id)
               DO UPDATE SET webhook_url = EXCLUDED.webhook_url, team_id = EXCLUDED.team_id, channel_id = EXCLUDED.channel_id, team_name = EXCLUDED.team_name, bot_token = EXCLUDED.bot_token |]


getProjectSlackData :: DB es => Projects.ProjectId -> Eff es (Maybe SlackData)
getProjectSlackData pid = Hasql.interpOne [HI.sql|SELECT project_id, webhook_url, team_id, channel_id, team_name, bot_token FROM apis.slack WHERE project_id = #{pid} |]


getSlackDataByTeamId :: DB es => Text -> Eff es (Maybe SlackData)
getSlackDataByTeamId teamId = Hasql.interpOne [HI.sql|SELECT project_id, webhook_url, team_id, channel_id, team_name, bot_token FROM apis.slack WHERE team_id = #{teamId} |]


data DiscordData = DiscordData
  { projectId :: Projects.ProjectId
  , guildId :: Text
  , notifsChannelId :: Maybe Text
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
getDiscordData guildId = Hasql.interpOne [HI.sql|SELECT project_id, guild_id, notifs_channel_id FROM apis.discord WHERE guild_id = #{guildId} |]


getDiscordDataByProjectId :: DB es => Projects.ProjectId -> Eff es (Maybe DiscordData)
getDiscordDataByProjectId pid = Hasql.interpOne [HI.sql|SELECT project_id, guild_id, notifs_channel_id FROM apis.discord WHERE project_id = #{pid} |]


updateDiscordNotificationChannel :: DB es => Text -> Text -> Eff es Int64
updateDiscordNotificationChannel guildId channelId = Hasql.interpExecute [HI.sql|Update apis.discord SET notifs_channel_id=#{channelId} WHERE guild_id = #{guildId} |]


updateSlackNotificationChannel :: DB es => Text -> Text -> Eff es Int64
updateSlackNotificationChannel teamId channelId = Hasql.interpExecute [HI.sql|Update apis.slack SET channel_id = #{channelId} WHERE team_id = #{teamId} |]


getDashboardsForSlack :: DB es => Text -> Eff es [(Text, Text)]
getDashboardsForSlack teamId = Hasql.interp [HI.sql|SELECT d.title, d.id::text FROM projects.dashboards d JOIN apis.slack s ON d.project_id = s.project_id WHERE s.team_id = #{teamId}|]


getDashboardsForWhatsapp :: DB es => Text -> Eff es [(Text, Text)]
getDashboardsForWhatsapp number = Hasql.interp [HI.sql|SELECT d.title, d.id::text FROM projects.dashboards d JOIN projects.projects p ON d.project_id = p.id where #{number}=Any(p.whatsapp_numbers)|]


getDashboardsForDiscord :: DB es => Text -> Eff es [(Text, Text)]
getDashboardsForDiscord guildId = Hasql.interp [HI.sql|SELECT d.title, d.id::text FROM projects.dashboards d JOIN apis.discord dd ON d.project_id = dd.project_id where guild_id=#{guildId}|]


data PagerdutyData = PagerdutyData
  { projectId :: Projects.ProjectId
  , integrationKey :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (HI.DecodeRow, NFData)


getPagerdutyByProjectId :: DB es => Projects.ProjectId -> Eff es (Maybe PagerdutyData)
getPagerdutyByProjectId pid = Hasql.interpOne [HI.sql|SELECT project_id, integration_key FROM apis.pagerduty WHERE project_id = #{pid}|]


insertPagerdutyData :: DB es => Projects.ProjectId -> Text -> Eff es Int64
insertPagerdutyData pid integrationKey =
  Hasql.interpExecute
    [HI.sql|INSERT INTO apis.pagerduty (project_id, integration_key) VALUES (#{pid}, #{integrationKey})
            ON CONFLICT (project_id) DO UPDATE SET integration_key = EXCLUDED.integration_key|]


deletePagerdutyData :: DB es => Projects.ProjectId -> Eff es Int64
deletePagerdutyData pid = Hasql.interpExecute [HI.sql|DELETE FROM apis.pagerduty WHERE project_id = #{pid}|]


deleteSlackData :: DB es => Projects.ProjectId -> Eff es Int64
deleteSlackData pid = Hasql.interpExecute [HI.sql|DELETE FROM apis.slack WHERE project_id = #{pid}|]
