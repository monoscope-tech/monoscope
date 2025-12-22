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
  getDashboardsForDiscord,
  getDashboardsForSlack,
  getDashboardsForWhatsapp,
  getDiscordData,
) where

import Data.Vector qualified as V
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Deriving.Aeson qualified as AE
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as PG
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


insertAccessToken :: (IOE :> es, WithConnection :> es) => Projects.ProjectId -> Text -> Text -> Text -> Eff es Int64
insertAccessToken pid webhookUrl teamId channelId = PG.execute q params
  where
    q =
      [sql|INSERT INTO apis.slack
               (project_id, webhook_url, team_id, channel_id)
               VALUES (?,?,?,?)
               ON CONFLICT (project_id)
               DO UPDATE SET webhook_url = EXCLUDED.webhook_url, team_id = EXCLUDED.team_id, channel_id = EXCLUDED.channel_id |]
    params = (pid, webhookUrl, teamId, channelId)


getProjectSlackData :: (IOE :> es, WithConnection :> es) => Projects.ProjectId -> Eff es (Maybe SlackData)
getProjectSlackData pid = listToMaybe <$> PG.query q (Only pid)
  where
    q = [sql|SELECT project_id, webhook_url, team_id, channel_id FROM apis.slack WHERE project_id =? |]


getSlackDataByTeamId :: (IOE :> es, WithConnection :> es) => Text -> Eff es (Maybe SlackData)
getSlackDataByTeamId teamId = listToMaybe <$> PG.query q (Only teamId)
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


insertDiscordData :: (IOE :> es, WithConnection :> es) => Projects.ProjectId -> Text -> Eff es Int64
insertDiscordData pid guildId = PG.execute q (pid, guildId)
  where
    q =
      [sql|INSERT INTO apis.discord
               (project_id, guild_id)
               VALUES (?,?)
               ON CONFLICT (project_id)
               DO UPDATE SET guild_id = EXCLUDED.guild_id |]


getDiscordData :: (IOE :> es, WithConnection :> es) => Text -> Eff es (Maybe DiscordData)
getDiscordData guildId = listToMaybe <$> PG.query q (Only guildId)
  where
    q = [sql|SELECT project_id, guild_id, notifs_channel_id FROM apis.discord WHERE guild_id =? |]


getDiscordDataByProjectId :: (IOE :> es, WithConnection :> es) => Projects.ProjectId -> Eff es (Maybe DiscordData)
getDiscordDataByProjectId pid = listToMaybe <$> PG.query q (Only pid)
  where
    q = [sql|SELECT project_id, guild_id, notifs_channel_id FROM apis.discord WHERE project_id =? |]


updateDiscordNotificationChannel :: (IOE :> es, WithConnection :> es) => Text -> Text -> Eff es Int64
updateDiscordNotificationChannel guildId channelId = PG.execute q (channelId, guildId)
  where
    q = [sql|Update apis.discord SET notifs_channel_id=? WHERE guild_id = ? |]


updateSlackNotificationChannel :: (IOE :> es, WithConnection :> es) => Text -> Text -> Eff es Int64
updateSlackNotificationChannel teamId channelId = PG.execute q (channelId, teamId)
  where
    q = [sql|Update apis.slack SET channel_id =? WHERE team_id = ? |]


getDashboardsForSlack :: (IOE :> es, WithConnection :> es) => Text -> Eff es [(Text, Text)]
getDashboardsForSlack teamId = PG.query q (Only teamId)
  where
    q = [sql|SELECT d.title, d.id::text FROM projects.dashboards d JOIN apis.slack s ON d.project_id = s.project_id WHERE  s.team_id = ?|]


getDashboardsForWhatsapp :: (IOE :> es, WithConnection :> es) => Text -> Eff es [(Text, Text)]
getDashboardsForWhatsapp number = PG.query q (Only number)
  where
    q = [sql|SELECT d.title, d.id::text FROM projects.dashboards d JOIN projects.projects p ON d.project_id = p.id where ?=Any(p.whatsapp_numbers)|]


getDashboardsForDiscord :: (IOE :> es, WithConnection :> es) => Text -> Eff es [(Text, Text)]
getDashboardsForDiscord guildId = PG.query q (Only guildId)
  where
    q = [sql|SELECT d.title, d.id::text FROM projects.dashboards d JOIN apis.discord dd ON d.project_id = dd.project_id where guild_id=?|]
