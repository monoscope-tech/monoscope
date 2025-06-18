module Models.Apis.Slack (
  SlackData (..),
  DiscordData (..),
  insertAccessToken,
  getSlackDataByTeamId,
  insertDiscordData,
  updateDiscordNotificationChannel,
  getDiscordDataByProjectId,
  getProjectSlackData,
  updateSlackNotificationChannel,
  getDiscordData,
) where

import Database.PostgreSQL.Entity.DBT (execute, queryOne)
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT, executeMany)
import Deriving.Aeson qualified as AE
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Models.Projects.Projects qualified as Projects
import Relude


data SlackData = SlackData
  { projectId :: Projects.ProjectId
  , webhookUrl :: Text
  , teamId :: Text
  , channelId :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SlackData


insertAccessToken :: Projects.ProjectId -> Text -> Text -> Text -> DBT IO Int64
insertAccessToken pid webhookUrl teamId channelId = execute q params
  where
    q =
      [sql|INSERT INTO apis.slack
               (project_id, webhook_url, team_id, channel_id)
               VALUES (?,?,?,?)
               ON CONFLICT (project_id)
               DO UPDATE SET webhook_url = EXCLUDED.webhook_url, team_id = EXCLUDED.team_id, channel_id = EXCLUDED.channel_id |]
    params = (pid, webhookUrl, teamId, channelId)


getProjectSlackData :: DB :> es => Projects.ProjectId -> Eff es (Maybe SlackData)
getProjectSlackData pid = dbtToEff $ queryOne q (Only pid)
  where
    q = [sql|SELECT project_id, webhook_url, team_id, channel_id FROM apis.slack WHERE project_id =? |]


getSlackDataByTeamId :: Text -> DBT IO (Maybe SlackData)
getSlackDataByTeamId teamId = queryOne q (Only teamId)
  where
    q = [sql|SELECT project_id, webhook_url, team_id, channel_id FROM apis.slack WHERE team_id = ? |]


data DiscordData = DiscordData
  { projectId :: Projects.ProjectId
  , guildId :: Text
  , notifsChannelId :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] DiscordData


insertDiscordData :: Projects.ProjectId -> Text -> DBT IO Int64
insertDiscordData pid guildId = execute q (pid, guildId)
  where
    q =
      [sql|INSERT INTO apis.discord
               (project_id, guild_id)
               VALUES (?,?)
               ON CONFLICT (project_id)
               DO UPDATE SET guild_id = EXCLUDED.guild_id |]


getDiscordData :: DB :> es => Text -> Eff es (Maybe DiscordData)
getDiscordData guildId = dbtToEff $ queryOne q (Only guildId)
  where
    q = [sql|SELECT project_id, guild_id, notifs_channel_id FROM apis.discord WHERE guild_id =? |]


getDiscordDataByProjectId :: DB :> es => Projects.ProjectId -> Eff es (Maybe DiscordData)
getDiscordDataByProjectId pid = dbtToEff $ queryOne q (Only pid)
  where
    q = [sql|SELECT project_id, guild_id, notifs_channel_id FROM apis.discord WHERE project_id =? |]


updateDiscordNotificationChannel :: DB :> es => Text -> Text -> Eff es Int64
updateDiscordNotificationChannel guildId channelId = dbtToEff $ execute q (channelId, guildId)
  where
    q = [sql|Update apis.discord SET notifs_channel_id=? WHERE guild_id = ? |]


updateSlackNotificationChannel :: DB :> es => Text -> Text -> Eff es (Int64)
updateSlackNotificationChannel teamId channelId = dbtToEff $ execute q (channelId, teamId)
  where
    q = [sql|Update apis.slack SET channel_id =? WHERE team_id = ? |]
