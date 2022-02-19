{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Users.Users where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AET
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Default.Instances
import qualified Data.Text as Text
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime, getZonedTime)
import Data.Time.Clock (DiffTime, NominalDiffTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, queryOne, query_, withPool)
import Database.PostgreSQL.Entity.Internal.QQ (field)
import qualified Database.PostgreSQL.Entity.Types as PET
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow, query_)
import Database.PostgreSQL.Simple.FromField (Format (Text), FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import qualified Database.PostgreSQL.Transact as PgT
import qualified Deriving.Aeson as DAE
import GHC.Generics (Generic)
import Optics.Operators
import Optics.TH
import Relude
import qualified Relude.Unsafe as Unsafe

instance FromJSON (CI Text) where
  parseJSON = fmap CI.mk . parseJSON

instance ToJSON (CI Text) where
  toJSON = toJSON . CI.original

instance Default Bool where
  def = False

newtype UserId = UserId {getUserId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, ToJSON, FromJSON, FromField, ToField, Default)
    via UUID.UUID
  deriving anyclass (FromRow, ToRow)

data User = User
  { id :: UserId,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    deletedAt :: Maybe ZonedTime,
    active :: Bool,
    firstName :: Text,
    lastName :: Text,
    displayImageUrl :: Text,
    email :: CI Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow, Default)
  deriving
    (FromJSON, ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] User
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "users", PET.TableName "users", PET.PrimaryKey "id", PET.FieldModifiers '[PET.CamelToSnake]] User)

createUserId :: IO UserId
createUserId = UserId <$> UUIDV4.nextRandom

createUser :: Text -> Text -> Text -> Text -> IO User
createUser firstName lastName picture email = do
  uid <- createUserId
  now <- getZonedTime
  pure $
    User
      { id = uid,
        createdAt = now,
        updatedAt = now,
        deletedAt = Nothing,
        active = True,
        firstName = firstName,
        lastName = lastName,
        displayImageUrl = picture,
        email = CI.mk email
      }

makeFieldLabelsNoPrefix ''User

insertUser :: User -> PgT.DBT IO ()
insertUser = insert @User

userByEmail :: Text -> PgT.DBT IO (Maybe User)
userByEmail email = selectOneByField @User [field| email |] (Only email)

-- addUserToAllProjects is a hack for development to add the user to all projects
addUserToAllProjects :: Text -> PgT.DBT IO Int64
addUserToAllProjects email = execute Insert q values
  where
    q =
      [sql| insert into projects.project_members (active, project_id, permission, user_id)
select true::Boolean, id, 'admin'::projects.project_permissions, (select id from users.users where email=?) from projects.projects
on conflict do nothing; |]
    values = Only email
