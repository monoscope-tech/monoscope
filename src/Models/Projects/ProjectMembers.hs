{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Projects.ProjectMembers (
  ProjectMembers (..),
  insertProjectMembers,
  CreateProjectMembers (..),
  ProjectMemberVM,
  Permissions (..),
  selectActiveProjectMembers,
  updateProjectMembersPermissons,
  softDeleteProjectMembers,
) where

import Control.Error (note)
import Data.CaseInsensitive (CI)
import Data.Default.Instances ()
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ResultError (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Transact (DBT, executeMany)
import Database.PostgreSQL.Transact qualified as PgT
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import Optics.Operators ()
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude
import Servant (FromHttpApiData)
import Web.HttpApiData (parseUrlPiece)

data Permissions
  = PAdmin
  | PView
  | PEdit
  deriving stock (Eq, Generic, Show)

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
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "project_members", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ProjectMembers)

makeFieldLabelsNoPrefix ''ProjectMembers

data CreateProjectMembers = CreateProjectMembers
  { projectId :: Projects.ProjectId
  , userId :: Users.UserId
  , permission :: Permissions
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "project_members", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CreateProjectMembers)

insertProjectMembers :: [CreateProjectMembers] -> DBT IO Int64
insertProjectMembers = PgT.executeMany q
  where
    q =
      [sql| INSERT INTO projects.project_members(project_id, user_id, permission) VALUES (?,?,?) |]

data ProjectMemberVM = ProjectMemberVM
  { id :: UUID.UUID
  , userId :: Users.UserId
  , permission :: Permissions
  , email :: CI Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow)

makeFieldLabelsNoPrefix ''ProjectMemberVM

selectActiveProjectMembers :: Projects.ProjectId -> DBT IO (Vector ProjectMemberVM)
selectActiveProjectMembers = query Select q
  where
    q =
      [sql| SELECT pm.id, pm.user_id, pm.permission,us.email  from projects.project_members pm
                   JOIN users.users us ON (pm.user_id=us.id)
                   WHERE pm.project_id=?::uuid and pm.active=TRUE;
                    
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
softDeleteProjectMembers vals = void $ executeMany q (map Only vals)
  where
    q =
      [sql| UPDATE projects.project_members  pm
            SET active = FALSE
            FROM (VALUES (?)) as c(id)
            WHERE pm.id::uuid = c.id::uuid; |]
