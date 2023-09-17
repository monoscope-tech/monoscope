{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Users.Users (
  User (..),
  UserId (..),
  createUser,
  userIdByEmail,
  createUserId,
  insertUser,
  userByEmail,
  createEmptyUser,
  addUserToAllProjects,
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Default
import Data.Default.Instances ()
import Data.Time (ZonedTime, getZonedTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import GHC.Records (HasField (getField))
import Optics.TH
import Relude

instance FromJSON (CI Text) where
  parseJSON = fmap CI.mk . parseJSON

instance ToJSON (CI Text) where
  toJSON = toJSON . CI.original

instance Default Bool where
  def = False

newtype UserId = UserId {getUserId :: UUID.UUID}
  deriving stock (Generic, Show, Eq)
  deriving
    (Ord, ToJSON, FromJSON, FromField, ToField, Default)
    via UUID.UUID
  deriving anyclass (FromRow, ToRow)

instance HasField "toText" UserId Text where
  getField = UUID.toText . getUserId

data User = User
  { id :: UserId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , deletedAt :: Maybe ZonedTime
  , active :: Bool
  , firstName :: Text
  , lastName :: Text
  , displayImageUrl :: Text
  , email :: CI Text
  , phoneNumber :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, Default)
  deriving
    (FromJSON, ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] User
  deriving
    (Entity)
    via (GenericEntity '[Schema "users", TableName "users", PrimaryKey "id", FieldModifiers '[CamelToSnake]] User)

makeFieldLabelsNoPrefix ''User

createUserId :: IO UserId
createUserId = UserId <$> UUIDV4.nextRandom

createUser :: Text -> Text -> Text -> Text -> IO User
createUser firstName lastName picture email = do
  uid <- createUserId
  now <- getZonedTime
  pure $
    User
      { id = uid
      , createdAt = now
      , updatedAt = now
      , deletedAt = Nothing
      , active = True
      , firstName = firstName
      , lastName = lastName
      , displayImageUrl = picture
      , email = CI.mk email
      , phoneNumber = Nothing
      }

insertUser :: User -> PgT.DBT IO ()
insertUser = insert @User

userByEmail :: Text -> PgT.DBT IO (Maybe User)
userByEmail email = selectOneByField @User [field| email |] (Only email)

userIdByEmail :: Text -> PgT.DBT IO (Maybe UserId)
userIdByEmail email = queryOne Select q (Only email)
 where
  q = [sql|select id from users.users where email=?|]

createEmptyUser :: Text -> PgT.DBT IO (Maybe UserId)
createEmptyUser email = queryOne Insert q (Only email)
 where
  q = [sql| insert into users.users (email, active) values (?, TRUE) on conflict do nothing returning id |]

-- addUserToAllProjects is a hack for development to add the user to all projects
addUserToAllProjects :: Text -> PgT.DBT IO Int64
addUserToAllProjects email = execute Insert q values
 where
  q =
    [sql| insert into projects.project_members (active, project_id, permission, user_id)
            select true::Boolean, id, 'admin'::projects.project_permissions, (select id from users.users where email=?) from projects.projects
            on conflict do nothing; |]
  values = Only email
