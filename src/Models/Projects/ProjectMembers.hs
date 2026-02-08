module Models.Projects.ProjectMembers (
  ProjectMembers (..),
  insertProjectMembers,
  CreateProjectMembers (..),
  ProjectMemberVM (..),
  ProjectMemberWithStatusVM (..),
  Permissions (..),
  selectActiveProjectMembers,
  selectAllProjectMembers,
  getUserPermission,
  updateProjectMembersPermissons,
  softDeleteProjectMembers,
  deactivateNonOwnerMembers,
  activateAllMembers,
  TeamDetails (..),
  createTeam,
  createEveryoneTeam,
  updateTeam,
  getTeams,
  getTeamByHandle,
  getTeamsByHandles,
  getTeamsVM,
  getEveryoneTeam,
  TeamVM (..),
  deleteTeamByHandle,
  deleteTeams,
  getTeamsById,
  Team (..),
  TeamMemberVM (..),
) where

import Data.Aeson qualified as AE
import Data.CaseInsensitive (CI)
import Data.Default.Instances ()
import Data.Text.Display (Display)
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (
  CamelToSnake,
  Entity,
  FieldModifiers,
  GenericEntity,
  PrimaryKey,
  Schema,
  TableName,
 )
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Effectful (Eff)
import Effectful.PostgreSQL qualified as PG
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import Pkg.DBUtils (WrappedEnumSC (..))
import Relude
import Servant (FromHttpApiData)
import System.Types (DB)


data Permissions
  = PAdmin
  | PView
  | PEdit
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (Display, FromField, FromHttpApiData, ToField) via WrappedEnumSC "P" Permissions


data ProjectMembers = ProjectMembers
  { id :: UUID.UUID
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , deletedAt :: Maybe ZonedTime
  , active :: Bool
  , projectId :: Projects.ProjectId
  , userId :: Users.UserId
  , permission :: Permissions
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "project_members", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ProjectMembers)


data CreateProjectMembers = CreateProjectMembers
  { projectId :: Projects.ProjectId
  , userId :: Users.UserId
  , permission :: Permissions
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "project_members", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CreateProjectMembers)


insertProjectMembers :: DB es => [CreateProjectMembers] -> Eff es Int64
insertProjectMembers [] = pure 0
insertProjectMembers members = PG.executeMany q members
  where
    q = [sql| INSERT INTO projects.project_members(project_id, user_id, permission) VALUES (?,?,?) ON CONFLICT (project_id, user_id) DO UPDATE SET active = TRUE, deleted_at = NULL |]


data ProjectMemberVM = ProjectMemberVM
  { id :: UUID.UUID
  , userId :: Users.UserId
  , permission :: Permissions
  , email :: CI Text
  , first_name :: Text
  , last_name :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData)


selectActiveProjectMembers :: DB es => Projects.ProjectId -> Eff es [ProjectMemberVM]
selectActiveProjectMembers pid = PG.query q (Only pid)
  where
    q =
      [sql| SELECT pm.id, pm.user_id, pm.permission,us.email, us.first_name, us.last_name from projects.project_members pm
                   JOIN users.users us ON (pm.user_id=us.id)
                   WHERE pm.project_id=?::uuid and pm.active=TRUE
                   ORDER BY pm.created_at ASC;
        |]


getUserPermission :: DB es => Projects.ProjectId -> Users.UserId -> Eff es (Maybe Permissions)
getUserPermission pid uid = coerce @(Maybe (Only Permissions)) @(Maybe Permissions) . listToMaybe <$> PG.query q (pid, uid)
  where
    q =
      [sql| SELECT permission FROM projects.project_members
            WHERE project_id = ? AND user_id = ? AND active = TRUE |]


updateProjectMembersPermissons :: DB es => [(UUID.UUID, Permissions)] -> Eff es ()
updateProjectMembersPermissons vals = void $ PG.executeMany q vals
  where
    q =
      [sql| UPDATE projects.project_members pm
            SET permission = c.permission::projects.project_permissions
            FROM (VALUES (?,?)) as c(id, permission)
            WHERE pm.id::uuid = c.id::uuid; |]


softDeleteProjectMembers :: DB es => NonEmpty UUID.UUID -> Eff es ()
softDeleteProjectMembers ids = void $ PG.execute q (Only $ V.fromList $ toList ids)
  where
    q =
      [sql| UPDATE projects.project_members
            SET active = FALSE, deleted_at = NOW()
            WHERE id = ANY(?::uuid[]); |]


deactivateNonOwnerMembers :: DB es => Projects.ProjectId -> Eff es Int64
deactivateNonOwnerMembers pid = PG.execute q (pid, pid)
  where
    q =
      [sql| UPDATE projects.project_members SET active = FALSE WHERE project_id = ?
              AND user_id != (SELECT user_id FROM projects.project_members WHERE project_id = ? ORDER BY created_at LIMIT 1) |]


activateAllMembers :: DB es => Projects.ProjectId -> Eff es Int64
activateAllMembers pid = PG.execute q (Only pid)
  where
    q = [sql| UPDATE projects.project_members SET active = TRUE WHERE project_id = ? |]


data ProjectMemberWithStatusVM = ProjectMemberWithStatusVM
  { id :: UUID.UUID
  , userId :: Users.UserId
  , permission :: Permissions
  , email :: CI Text
  , first_name :: Text
  , last_name :: Text
  , active :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData)


selectAllProjectMembers :: DB es => Projects.ProjectId -> Eff es [ProjectMemberWithStatusVM]
selectAllProjectMembers pid = PG.query q (Only pid)
  where
    q =
      [sql| SELECT pm.id, pm.user_id, pm.permission, us.email, us.first_name, us.last_name, pm.active
              FROM projects.project_members pm JOIN users.users us ON (pm.user_id=us.id)
              WHERE pm.project_id=?::uuid AND pm.deleted_at IS NULL AND pm.active = TRUE ORDER BY pm.created_at ASC |]


data TeamDetails = TeamDetails
  { name :: Text
  , description :: Text
  , handle :: Text
  , members :: V.Vector Users.UserId
  , notifyEmails :: V.Vector Text
  , slackChannels :: V.Vector Text
  , discordChannels :: V.Vector Text
  , phoneNumbers :: V.Vector Text
  , pagerdutyServices :: V.Vector Text
  }
  deriving stock (Eq, Generic, Show)


createTeam :: DB es => Projects.ProjectId -> Users.UserId -> TeamDetails -> Eff es Int64
createTeam pid uid TeamDetails{..}
  | handle == "everyone" = pure 0 -- Prevent creating team with reserved handle
  | otherwise = PG.execute q (pid, uid, name, description, handle, members, notifyEmails, slackChannels, discordChannels, phoneNumbers, pagerdutyServices)
  where
    q =
      [sql| INSERT INTO projects.teams
               (project_id, created_by, name, description, handle, members, notify_emails, slack_channels, discord_channels, phone_numbers, pagerduty_services)
               VALUES (?, ?, ?, ?, ?, ?::uuid[], ?, ?, ?, ?, ?)
               ON CONFLICT (project_id, handle) DO NOTHING |]


createEveryoneTeam :: DB es => Projects.ProjectId -> Eff es Int64
createEveryoneTeam pid = PG.execute q (Only pid)
  where
    q =
      [sql| INSERT INTO projects.teams
               (project_id, name, description, handle, is_everyone, members, notify_emails, slack_channels, discord_channels, phone_numbers, pagerduty_services)
               VALUES (?, 'Everyone', 'All project members and configured integrations', 'everyone', TRUE,
                       '{}'::uuid[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[])
               ON CONFLICT (project_id, handle) DO NOTHING |]


getEveryoneTeam :: DB es => Projects.ProjectId -> Eff es (Maybe Team)
getEveryoneTeam pid = listToMaybe <$> PG.query q (Only pid)
  where
    q =
      [sql|
      SELECT t.id, t.name, t.description, t.handle, t.members, t.created_by, t.created_at, t.updated_at, t.notify_emails, t.slack_channels, t.discord_channels, t.phone_numbers, t.pagerduty_services, t.is_everyone
      FROM projects.teams t
      WHERE t.project_id = ? AND t.is_everyone = TRUE AND t.deleted_at IS NULL
    |]


updateTeam :: DB es => Projects.ProjectId -> UUID.UUID -> TeamDetails -> Eff es Int64
updateTeam pid tid TeamDetails{..} = PG.execute q (name, description, handle, members, notifyEmails, slackChannels, discordChannels, phoneNumbers, pagerdutyServices, pid, tid)
  where
    q =
      [sql| UPDATE projects.teams
               SET name = ?, description = ?, handle = ?, members = ?::uuid[], notify_emails = ?, slack_channels = ?, discord_channels = ?, phone_numbers = ?, pagerduty_services = ?
               WHERE project_id = ? AND id = ? |]


data Team = Team
  { id :: UUID.UUID
  , name :: Text
  , description :: Text
  , handle :: Text
  , members :: V.Vector Users.UserId
  , created_by :: Maybe Users.UserId
  , created_at :: UTCTime
  , updated_at :: UTCTime
  , notify_emails :: V.Vector Text
  , slack_channels :: V.Vector Text
  , discord_channels :: V.Vector Text
  , phone_numbers :: V.Vector Text
  , pagerduty_services :: V.Vector Text
  , is_everyone :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData)


getTeams :: DB es => Projects.ProjectId -> Eff es [Team]
getTeams pid = PG.query q (Only pid)
  where
    q =
      [sql|
      SELECT t.id, t.name, t.description, t.handle, t.members, t.created_by, t.created_at, t.updated_at, t.notify_emails, t.slack_channels, t.discord_channels, t.phone_numbers, t.pagerduty_services, t.is_everyone
      FROM projects.teams t
      WHERE t.project_id = ? AND t.deleted_at IS NULL
    |]


getTeamsVM :: DB es => Projects.ProjectId -> Eff es [TeamVM]
getTeamsVM pid = PG.query q (Only pid)
  where
    q =
      [sql|
      SELECT
        t.id,
        t.created_at,
        t.updated_at,
        t.created_by,
        t.name,
        t.handle,
        t.description,
        t.notify_emails,
        t.slack_channels,
        t.discord_channels,
        t.phone_numbers,
        t.pagerduty_services,
        t.is_everyone,
        COALESCE(
          array_agg(
            jsonb_build_object(
              'memberId', u.id,
              'memberName', concat_ws(' ', u.first_name, u.last_name),
              'memberEmail', u.email,
              'memberAvatar', '/api/avatar/' || u.id::text
            ) ORDER BY u.first_name, u.last_name
          ) FILTER (WHERE u.id IS NOT NULL),
          '{}'
        ) AS members
      FROM projects.teams t
      LEFT JOIN unnest(t.members) AS mid ON true
      LEFT JOIN users.users u ON u.id = mid
      WHERE t.project_id = ? AND t.deleted_at IS NULL
      GROUP BY t.id, t.created_at, t.updated_at, t.created_by, t.name, t.handle,
               t.description, t.notify_emails, t.slack_channels, t.discord_channels, t.phone_numbers, t.pagerduty_services, t.is_everyone
      ORDER BY t.is_everyone DESC, t.created_at ASC
    |]


data TeamVM = TeamVM
  { id :: UUID.UUID
  , created_at :: UTCTime
  , updated_at :: UTCTime
  , created_by :: Maybe Users.UserId
  , name :: Text
  , handle :: Text
  , description :: Text
  , notify_emails :: V.Vector Text
  , slack_channels :: V.Vector Text
  , discord_channels :: V.Vector Text
  , phone_numbers :: V.Vector Text
  , pagerduty_services :: V.Vector Text
  , is_everyone :: Bool
  , members :: V.Vector TeamMemberVM
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData)


data TeamMemberVM = TeamMemberVM
  { memberId :: UUID.UUID
  , memberEmail :: Text
  , memberName :: Text
  , memberAvatar :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, NFData)
  deriving (FromField) via Aeson TeamMemberVM


getTeamByHandle :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe TeamVM)
getTeamByHandle pid handle = listToMaybe <$> PG.query q (pid, handle)
  where
    q =
      [sql|
      SELECT
        t.id,
        t.created_at,
        t.updated_at,
        t.created_by,
        t.name,
        t.handle,
        t.description,
        t.notify_emails,
        t.slack_channels,
        t.discord_channels,
        t.phone_numbers,
        t.pagerduty_services,
        t.is_everyone,
        COALESCE(
          array_agg(
            jsonb_build_object(
              'memberId', u.id,
              'memberName', concat_ws(' ', u.first_name, u.last_name),
              'memberEmail', u.email,
              'memberAvatar', '/api/avatar/' || u.id::text
            ) ORDER BY u.first_name, u.last_name
          ) FILTER (WHERE u.id IS NOT NULL),
          '{}'
        ) AS members
      FROM projects.teams t
      LEFT JOIN unnest(t.members) AS mid ON true
      LEFT JOIN users.users u ON u.id = mid
      WHERE t.project_id = ? AND t.handle = ? AND t.deleted_at IS NULL
      GROUP BY t.id, t.created_at, t.updated_at, t.created_by, t.name, t.handle,
               t.description, t.notify_emails, t.slack_channels, t.discord_channels, t.phone_numbers, t.pagerduty_services, t.is_everyone
    |]


deleteTeamByHandle :: DB es => Projects.ProjectId -> Text -> Eff es ()
deleteTeamByHandle pid handle = void $ PG.execute q (pid, handle)
  where
    q =
      [sql| UPDATE projects.teams SET deleted_at = now() WHERE project_id = ? AND handle = ? AND is_everyone = FALSE |]


deleteTeams :: DB es => Projects.ProjectId -> V.Vector UUID.UUID -> Eff es ()
deleteTeams pid tids
  | V.null tids = pass
  | otherwise = void $ PG.execute q (pid, tids)
  where
    q =
      [sql| UPDATE projects.teams SET deleted_at = now() WHERE project_id = ? AND id = ANY(?::uuid[]) AND is_everyone = FALSE |]


getTeamsById :: DB es => Projects.ProjectId -> V.Vector UUID.UUID -> Eff es [Team]
getTeamsById pid tids = if V.null tids then pure [] else PG.query q (pid, tids)
  where
    q =
      [sql|
      SELECT
        t.id,
        t.name,
        t.description,
        t.handle,
        t.members,
        t.created_by,
        t.created_at,
        t.updated_at,
        t.notify_emails,
        t.slack_channels,
        t.discord_channels,
        t.phone_numbers,
        t.pagerduty_services,
        t.is_everyone
      FROM projects.teams t
      WHERE t.project_id = ? AND t.id = ANY(?::uuid[]) AND t.deleted_at IS NULL
    |]


-- | Bulk fetch teams by handles - more efficient than mapping getTeamByHandle
getTeamsByHandles :: DB es => Projects.ProjectId -> [Text] -> Eff es [TeamVM]
getTeamsByHandles _ [] = pure []
getTeamsByHandles pid handles = PG.query q (pid, V.fromList handles)
  where
    q =
      [sql|
      SELECT
        t.id,
        t.created_at,
        t.updated_at,
        t.created_by,
        t.name,
        t.handle,
        t.description,
        t.notify_emails,
        t.slack_channels,
        t.discord_channels,
        t.phone_numbers,
        t.pagerduty_services,
        t.is_everyone,
        COALESCE(
          array_agg(
            jsonb_build_object(
              'memberId', u.id,
              'memberName', concat_ws(' ', u.first_name, u.last_name),
              'memberEmail', u.email,
              'memberAvatar', '/api/avatar/' || u.id::text
            ) ORDER BY u.first_name, u.last_name
          ) FILTER (WHERE u.id IS NOT NULL),
          '{}'
        ) AS members
      FROM projects.teams t
      LEFT JOIN unnest(t.members) AS mid ON true
      LEFT JOIN users.users u ON u.id = mid
      WHERE t.project_id = ? AND t.handle = ANY(?) AND t.deleted_at IS NULL
      GROUP BY t.id, t.created_at, t.updated_at, t.created_by, t.name, t.handle,
               t.description, t.notify_emails, t.slack_channels, t.discord_channels, t.phone_numbers, t.pagerduty_services, t.is_everyone
    |]
