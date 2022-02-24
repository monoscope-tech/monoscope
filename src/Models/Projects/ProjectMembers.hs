{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Projects.ProjectMembers
  ( ProjectMembers (..),
    insertProjectMembers,
    CreateProjectMembers (..),
    Permissions (..),
    MemberPermissionForm,
    MemberPermissionFormError,
    memberPermissionFormToModel,
    memberPermissionFormV,
    updateMemberPermission,
    deleteMember,
    InvProjectMember (..),
  )
where

import Data.Default (Default)
import Data.Default.Instances ()
import Data.Text qualified as T
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Valor (Valor, check1, failIf)
import Database.PostgreSQL.Entity (delete)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ResultError (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Transact qualified as PgT
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import Optics.Operators ()
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude
import Web.FormUrlEncoded (FromForm)

data Permissions
  = PAdmin
  | PView
  | PEdit
  deriving (Eq, Generic, Show)

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
  { id :: UUID.UUID,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    deletedAt :: Maybe ZonedTime,
    active :: Bool,
    projectId :: Projects.ProjectId,
    userId :: Users.UserId,
    permission :: Permissions
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "project_members", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ProjectMembers)

makeFieldLabelsNoPrefix ''ProjectMembers

data CreateProjectMembers = CreateProjectMembers
  { projectId :: Projects.ProjectId,
    userId :: Users.UserId,
    permission :: Permissions
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "project_members", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CreateProjectMembers)

data InvProjectMember = InvProjectMember
  { projectId :: Projects.ProjectId,
    userId :: Users.UserId,
    permission :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "project_members", PrimaryKey "id", FieldModifiers '[CamelToSnake]] InvProjectMember)

makeFieldLabelsNoPrefix ''InvProjectMember

data MemberPermissionForm = MemberPermissionForm
  { permissionf :: Text
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromForm, Default)

data MemberPermissionFormError = MemberPermissionFormError
  { permissionE :: Maybe [String]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Default)

memberPermissionFormToModel :: Projects.ProjectId -> MemberPermissionForm -> CreateProjectMembers
memberPermissionFormToModel pid MemberPermissionForm {..} = CreateProjectMembers {projectId = pid, ..}

memberPermissionFormV :: Monad m => Valor MemberPermissionForm m MemberPermissionFormError
memberPermissionFormV =
  MemberPermissionFormError
    <$> check1 permissionf (failIf ["permission can't be empty"] T.null)

insertProjectMembers :: [CreateProjectMembers] -> PgT.DBT IO Int64
insertProjectMembers = PgT.executeMany q
  where
    q =
      [sql|
          INSERT INTO projects.project_members(project_id, user_id, permission) VALUES (?,?,?)
         |]

updateMemberPermission :: Projects.ProjectId -> MemberPermissionForm -> PgT.DBT IO Int64
updateMemberPermission mid pm = PgT.execute q (Only mid)
  where
    q =
      [sql|
        UPDATE projects.project_members (permission) VALUES (?)
        WHERE projects.project_members.id = mid
        OPTIONS (pm);|]

deleteMember :: UUID.UUID -> PgT.DBT IO ()
deleteMember mid = delete @ProjectMembers (Only mid)
