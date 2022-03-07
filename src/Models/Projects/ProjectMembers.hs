{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Projects.ProjectMembers
  ( ProjectMembers (..),
    insertProjectMembers,
    CreateProjectMembers (..),
    Permissions (..),
    updateMemberPermission,
    deleteMember,
    InvProjectMember (..),
    invProjectMembers,
    updateMemberPermissionFormToModel,
  )
where

import Data.Default.Instances ()
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
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

data Permissions
  = PAdmin
  | PView
  | PEdit
  deriving stock (Eq, Generic, Show)

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
  deriving stock (Show, Generic)
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
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "project_members", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CreateProjectMembers)

data InvProjectMember = InvProjectMember
  { projectId :: Projects.ProjectId,
    userId :: Users.UserId,
    permission :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "project_members", PrimaryKey "id", FieldModifiers '[CamelToSnake]] InvProjectMember)

makeFieldLabelsNoPrefix ''InvProjectMember

updateMemberPermissionFormToModel :: Projects.ProjectId -> Users.UserId -> Text -> InvProjectMember
updateMemberPermissionFormToModel pid uid perm = InvProjectMember {projectId, userId, permission}
  where
    projectId = pid
    userId = uid
    permission = perm

insertProjectMembers :: [CreateProjectMembers] -> PgT.DBT IO Int64
insertProjectMembers = PgT.executeMany q
  where
    q =
      [sql|
          INSERT INTO projects.project_members(project_id, user_id, permission) VALUES (?,?,?)
         |]

invProjectMembers :: [InvProjectMember] -> PgT.DBT IO Int64
invProjectMembers = PgT.executeMany q
  where
    q =
      [sql|
          INSERT INTO projects.project_members(project_id, user_id, permission) VALUES (?,?,?)
        |]

updateMemberPermission :: Projects.ProjectId -> UUID.UUID -> InvProjectMember -> PgT.DBT IO Int64
updateMemberPermission pid mid pm = PgT.execute q (Only pid)
  where
    q =
      [sql|
        UPDATE projects.project_members (permission) VALUES (?)
        WHERE projects.project_members.id = mid
        OPTIONS (pm);|]

deleteMember :: UUID.UUID -> PgT.DBT IO ()
deleteMember mid = delete @ProjectMembers (Only mid)
