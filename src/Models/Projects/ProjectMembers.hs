module Models.Projects.ProjectMembers (
  ProjectMembers (..),
  insertProjectMembers,
  CreateProjectMembers (..),
  ProjectMemberVM (..),
  Permissions (..),
  selectActiveProjectMembers,
  updateProjectMembersPermissons,
  softDeleteProjectMembers,
  selectProjectActiveMember,
) where

import Control.Error (note)
import Data.CaseInsensitive (CI)
import Data.Default.Instances ()
import Data.Time (ZonedTime)
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
import Database.PostgreSQL.Simple (FromRow, Only (Only), ResultError (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
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
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData)


selectActiveProjectMembers :: Projects.ProjectId -> DBT IO (V.Vector ProjectMemberVM)
selectActiveProjectMembers = query q
  where
    q =
      [sql| SELECT pm.id, pm.user_id, pm.permission,us.email  from projects.project_members pm
                   JOIN users.users us ON (pm.user_id=us.id)
                   WHERE pm.project_id=?::uuid and pm.active=TRUE 
                   ORDER BY pm.created_at ASC;
        |]


selectProjectActiveMember :: Projects.ProjectId -> Users.UserId -> DBT IO (Maybe ProjectMembers)
selectProjectActiveMember pid userId = queryOne q (pid, userId)
  where
    q = [sql| SELECT * from projects.project_members where project_id = ? AND user_id = ?  AND deleted_at is null AND active = true; |]


updateProjectMembersPermissons :: [(UUID.UUID, Permissions)] -> DBT IO ()
updateProjectMembersPermissons vals = void $ executeMany q vals
  where
    q =
      [sql| UPDATE projects.project_members pm
            SET permission = c.permission::projects.project_permissions
            FROM (VALUES (?,?)) as c(id, permission)
            WHERE pm.id::uuid = c.id::uuid; |]


softDeleteProjectMembers :: [UUID.UUID] -> DBT IO ()
softDeleteProjectMembers vals = void $ execute q (Only (V.fromList vals))
  where
    q =
      [sql| UPDATE projects.project_members
            SET active = FALSE
            WHERE id = Any(?::uuid[]); |]
