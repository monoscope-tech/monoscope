{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Projects.ProjectMembers
  ( ProjectMembers (..),
    insertProjectMembers,
    CreateProjectMembers (..),
    Permissions (..),
  )
where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AET
import Data.Default
import Data.Default.Instances
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime)
import Data.Time.Clock (DiffTime, NominalDiffTime)
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, queryOne, query_, withPool)
import qualified Database.PostgreSQL.Entity.Types as PET
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ResultError (..), ToRow, query_)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import qualified Database.PostgreSQL.Transact as PgT
import qualified Deriving.Aeson as DAE
import GHC.Generics (Generic)
import qualified Models.Projects.Projects as Projects
import qualified Models.Users.Users as Users
import Optics.Operators
import Optics.TH
import Relude
import qualified Relude.Unsafe as Unsafe

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
  { createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    deletedAt :: Maybe ZonedTime,
    active :: Bool,
    id :: UUID.UUID,
    projectId :: Projects.ProjectId,
    userId :: Users.UserId,
    permission :: Permissions
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "projects", PET.TableName "project_members", PET.PrimaryKey "id", PET.FieldModifiers '[PET.CamelToSnake]] ProjectMembers)

makeFieldLabelsNoPrefix ''ProjectMembers

data CreateProjectMembers = CreateProjectMembers
  { projectId :: Projects.ProjectId,
    userId :: Users.UserId,
    permission :: Permissions
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "projects", PET.TableName "project_members", PET.PrimaryKey "id", PET.FieldModifiers '[PET.CamelToSnake]] CreateProjectMembers)

insertProjectMembers :: [CreateProjectMembers] -> PgT.DBT IO Int64
insertProjectMembers = PgT.executeMany q
  where
    q =
      [sql|
          INSERT INTO projects.project_members(project_id, user_id, permission) VALUES (?,?,?)
         |]
