module Models.Projects.ProjectMembers (
  ProjectMembers (..),
  insertProjectMembers,
  CreateProjectMembers (..),
  ProjectMemberVM (..),
  Permissions (..),
  selectActiveProjectMembers,
  getUserPermission,
  updateProjectMembersPermissons,
  softDeleteProjectMembers,
  TeamDetails (..),
  createTeam,
  updateTeam,
  getTeams,
  getTeamByHandle,
  getTeamsVM,
  TeamVM (..),
  deleteTeamByHandle,
  deleteTeams,
  getTeamsById,
  Team (..),
  TeamMemberVM (..),
) where

import Control.Error (note)
import Data.Aeson qualified as AE
import Data.CaseInsensitive (CI)
import Data.Default.Instances ()
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (query, queryOne)
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
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Simple.TypeInfo.Static (uuid)
import Database.PostgreSQL.Transact (DBT, execute, executeMany)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import Effectful.PostgreSQL.Transact.Effect (DB)
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


getUserPermission :: Projects.ProjectId -> Users.UserId -> DBT IO (Maybe Permissions)
getUserPermission pid uid = do
  result <- queryOne q (pid, uid)
  pure $ fmap (\(Only p) -> p) result
  where
    q =
      [sql| SELECT permission FROM projects.project_members
            WHERE project_id = ? AND user_id = ? AND active = TRUE |]


updateProjectMembersPermissons :: [(UUID.UUID, Permissions)] -> DBT IO ()
updateProjectMembersPermissons vals = void $ executeMany q vals
  where
    q =
      [sql| UPDATE projects.project_members pm
            SET permission = c.permission::projects.project_permissions
            FROM (VALUES (?,?)) as c(id, permission)
            WHERE pm.id::uuid = c.id::uuid; |]


softDeleteProjectMembers :: NonEmpty UUID.UUID -> DBT IO ()
softDeleteProjectMembers = void . execute q . Only . V.fromList . toList
  where
    q =
      [sql| UPDATE projects.project_members
            SET active = FALSE
            WHERE id = ANY(?::uuid[]); |]


data TeamDetails = TeamDetails
  { name :: Text
  , description :: Text
  , handle :: Text
  , members :: V.Vector Users.UserId
  , notifyEmails :: V.Vector Text
  , slackChannels :: V.Vector Text
  , discordChannels :: V.Vector Text
  , phoneNumbers :: V.Vector Text
  }
  deriving stock (Eq, Generic, Show)


createTeam :: Projects.ProjectId -> Users.UserId -> TeamDetails -> DBT IO Int64
createTeam pid uid TeamDetails{..} = execute q (pid, uid, name, description, handle, members, notifyEmails, slackChannels, discordChannels, phoneNumbers)
  where
    q =
      [sql| INSERT INTO projects.teams
               (project_id, created_by, name, description, handle, members, notify_emails, slack_channels, discord_channels, phone_numbers)
               VALUES (?, ?, ?, ?, ?, ?::uuid[], ?, ?, ?, ?)
               ON CONFLICT (project_id, handle) DO NOTHING |]


updateTeam :: Projects.ProjectId -> UUID.UUID -> TeamDetails -> DBT IO Int64
updateTeam pid tid TeamDetails{..} = execute q (name, description, handle, members, notifyEmails, slackChannels, discordChannels, phoneNumbers, pid, tid)
  where
    q =
      [sql| UPDATE projects.teams
               SET name = ?, description = ?, handle = ?, members = ?::uuid[], notify_emails = ?, slack_channels = ?, discord_channels = ?, phone_numbers = ?
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
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData)


getTeams :: Projects.ProjectId -> DBT IO (V.Vector Team)
getTeams pid = query q (Only pid)
  where
    q =
      [sql|
      SELECT t.id, t.name, t.description, t.handle, t.members, t.created_by, t.created_at, t.updated_at, t.notify_emails, t.slack_channels, t.discord_channels, t.phone_numbers
      FROM projects.teams t
      WHERE t.project_id = ? AND t.deleted_at IS NULL
    |]


getTeamsVM :: Projects.ProjectId -> DBT IO (V.Vector TeamVM)
getTeamsVM pid = query q (Only pid)
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
        COALESCE(
          array_agg(
            jsonb_build_object(
              'memberId', u.id,
              'memberName', concat_ws(' ', u.first_name, u.last_name),
              'memberEmail', u.email,
              'memberAvatar', u.display_image_url
            ) ORDER BY u.first_name, u.last_name
          ) FILTER (WHERE u.id IS NOT NULL),
          '{}'
        ) AS members
      FROM projects.teams t
      LEFT JOIN unnest(t.members) AS mid ON true
      LEFT JOIN users.users u ON u.id = mid
      WHERE t.project_id = ? AND t.deleted_at IS NULL
      GROUP BY t.id, t.created_at, t.updated_at, t.created_by, t.name, t.handle,
               t.description, t.notify_emails, t.slack_channels, t.discord_channels, t.phone_numbers
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


getTeamByHandle :: Projects.ProjectId -> Text -> DBT IO (Maybe TeamVM)
getTeamByHandle pid handle = queryOne q (pid, handle)
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
        COALESCE(
          array_agg(
            jsonb_build_object(
              'memberId', u.id,
              'memberName', concat_ws(' ', u.first_name, u.last_name),
              'memberEmail', u.email,
              'memberAvatar', u.display_image_url
            ) ORDER BY u.first_name, u.last_name
          ) FILTER (WHERE u.id IS NOT NULL),
          '{}'
        ) AS members
      FROM projects.teams t
      LEFT JOIN unnest(t.members) AS mid ON true
      LEFT JOIN users.users u ON u.id = mid
      WHERE t.project_id = ? AND t.handle = ? AND t.deleted_at IS NULL
      GROUP BY t.id, t.created_at, t.updated_at, t.created_by, t.name, t.handle,
               t.description, t.notify_emails, t.slack_channels, t.discord_channels, t.phone_numbers
    |]


deleteTeamByHandle :: Projects.ProjectId -> Text -> DBT IO ()
deleteTeamByHandle pid handle = void $ execute q (pid, handle)
  where
    q =
      [sql| UPDATE projects.teams SET deleted_at = now() WHERE project_id = ? AND handle = ? |]


deleteTeams :: Projects.ProjectId -> V.Vector UUID.UUID -> DBT IO ()
deleteTeams pid tids
  | V.null tids = pass
  | otherwise = void $ execute q (pid, tids)
  where
    q =
      [sql| UPDATE projects.teams SET deleted_at = now() WHERE project_id = ? AND id = ANY(?::uuid[]) |]


getTeamsById :: Projects.ProjectId -> V.Vector UUID.UUID -> DBT IO (V.Vector Team)
getTeamsById pid tids = if V.null tids then pure V.empty else query q (pid, tids)
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
        t.phone_numbers
      FROM projects.teams t
      WHERE t.project_id = ? AND t.id = ANY(?::uuid[]) AND t.deleted_at IS NULL
    |]
