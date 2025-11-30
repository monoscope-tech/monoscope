module Models.Projects.ProjectMembers (
  ProjectMembers (..),
  insertProjectMembers,
  CreateProjectMembers (..),
  ProjectMemberVM (..),
  Permissions (..),
  selectActiveProjectMembers,
  updateProjectMembersPermissons,
  softDeleteProjectMembers,
  createTeam,
  updateTeam,
  getTeams,
  getTeamByHandle,
  TeamVM (..),
  Team (..),
) where

import Control.Error (note)
import Data.Aeson qualified as AE
import Data.CaseInsensitive (CI)
import Data.Default.Instances ()
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (query)
import Database.PostgreSQL.Entity.Types (
  CamelToSnake,
  Entity,
  FieldModifiers,
  GenericEntity,
  PrimaryKey,
  Schema,
  TableName,
 )
import Database.PostgreSQL.Simple (FromRow, In, Only (Only), ResultError (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Simple.TypeInfo.Static (uuid)
import Database.PostgreSQL.Transact (DBT, execute, executeMany)
import Database.PostgreSQL.Transact qualified as PgT
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import Relude
import Servant (FromHttpApiData)
import Web.HttpApiData (parseUrlPiece)


data Permissions
  = PAdmin
  | PView
  | PEdit
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


instance FromHttpApiData Permissions where
  parseUrlPiece = note "Unable to parse" . parsePermissions


instance ToField Permissions where
  toField PAdmin = Escape "admin"
  toField PView = Escape "view"
  toField PEdit = Escape "edit"


parsePermissions :: (Eq s, IsString s) => s -> Maybe Permissions
parsePermissions "admin" = Just PAdmin
parsePermissions "view" = Just PView
parsePermissions "edit" = Just PEdit
parsePermissions _ = Nothing


instance FromField Permissions where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs ->
        case parsePermissions bs of
          Just a -> pure a
          Nothing -> returnError ConversionFailed f $ "Conversion error: Expected permission enum, got " <> decodeUtf8 bs <> " instead."


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


insertProjectMembers :: [CreateProjectMembers] -> DBT IO Int64
insertProjectMembers = PgT.executeMany q
  where
    q =
      [sql| INSERT INTO projects.project_members(project_id, user_id, permission) VALUES (?,?,?) ON CONFLICT (project_id, user_id) DO UPDATE SET active = TRUE |]


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


selectActiveProjectMembers :: Projects.ProjectId -> DBT IO (V.Vector ProjectMemberVM)
selectActiveProjectMembers = query q
  where
    q =
      [sql| SELECT pm.id, pm.user_id, pm.permission,us.email, us.first_name, us.last_name from projects.project_members pm
                   JOIN users.users us ON (pm.user_id=us.id)
                   WHERE pm.project_id=?::uuid and pm.active=TRUE 
                   ORDER BY pm.created_at ASC;
        |]


updateProjectMembersPermissons :: [(UUID.UUID, Permissions)] -> DBT IO ()
updateProjectMembersPermissons vals = void $ executeMany q vals
  where
    q =
      [sql| UPDATE projects.project_members pm
            SET permission = c.permission::projects.project_permissions
            FROM (VALUES (?,?)) as c(id, permission)
            WHERE pm.id::uuid = c.id::uuid; |]


softDeleteProjectMembers :: [UUID.UUID] -> DBT IO ()
softDeleteProjectMembers [] = pass
softDeleteProjectMembers vals = void $ execute q (Only (V.fromList vals))
  where
    q =
      [sql| UPDATE projects.project_members
            SET active = FALSE
            WHERE id = Any(?::uuid[]); |]


createTeam :: Projects.ProjectId -> Text -> Text -> Text -> V.Vector Users.UserId -> V.Vector Text -> V.Vector Text -> V.Vector Text -> DBT IO Int64
createTeam pid name description handle memberIds notifEmails slackChannels discordChannels = do
  execute q (pid, name, description, handle, memberIds, notifEmails, slackChannels, discordChannels)
  where
    q =
      [sql| INSERT INTO projects.teams
               (project_id, name, description, handle, members, notify_emails, slack_channels, discord_channels)
               VALUES (?,?,?,?,?::uuid[],?,?,?) |]


updateTeam :: Projects.ProjectId -> UUID.UUID -> Text -> Text -> Text -> V.Vector Users.UserId -> V.Vector Text -> V.Vector Text -> V.Vector Text -> DBT IO Int64
updateTeam pid tid name description handle memberIds notifEmails slackChannels discordChannels = do
  execute q (name, description, handle, memberIds, notifEmails, slackChannels, discordChannels, pid, tid)
  where
    q =
      [sql| UPDATE projects.teams
               SET name = ?, description = ?, handle = ?, members = ?::uuid[], notify_emails = ?, slack_channels = ?, discord_channels = ?
               WHERE project_id = ? AND id = ? |]


data Team = Team
  { id :: UUID.UUID
  , name :: Text
  , description :: Text
  , handle :: Text
  , members :: V.Vector Users.UserId
  , notify_emails :: V.Vector Text
  , slack_channels :: V.Vector Text
  , discord_channels :: V.Vector Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData)


getTeams :: Projects.ProjectId -> DBT IO (V.Vector Team)
getTeams pid = query q (Only pid)
  where
    q =
      [sql| SELECT id, name, description, handle, members, notify_emails, slack_channels, discord_channels
            FROM projects.teams
            WHERE project_id = ? |]


data TeamVM = TeamVM
  { id :: UUID.UUID
  , name :: Text
  , handle :: Text
  , description :: Text
  , notify_emails :: V.Vector Text
  , slack_channels :: V.Vector Text
  , discord_channels :: V.Vector Text
  , members :: V.Vector TeamMemberVM
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData)


data TeamMemberVM = TeamMemberVM
  { memberId :: UUID.UUID
  , memberEmail :: CI Text
  , memberName :: Text
  , memberAvatar :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromField, NFData)


getTeamByHandle :: Projects.ProjectId -> Text -> DBT IO (V.Vector TeamVM)
getTeamByHandle pid handle = query q (pid, handle)
  where
    q =
      [sql|
      SELECT 
        t.id,
        t.name,
        t.handle,
        t.description,
        t.notify_emails,
        t.slack_channels,
        t.discord_channels,
        COALESCE(
          (
            SELECT jsonb_agg(
              jsonb_build_object(
                'memberId', u.id,
                'memberName', u.first_name <> ' ' <> u.last_name,
                'memberEmail', u.email,
                'memberAvatar', u.avatar
              )
            )
            FROM unnest(t.members) AS mid
            JOIN users.users u ON u.id = mid
          ),
          '[]'::jsonb
        ) AS members
      FROM teams t
      WHERE t.project_id = ? 
        AND t.handle = ?
    |]
