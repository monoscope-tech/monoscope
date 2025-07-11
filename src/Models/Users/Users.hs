module Models.Users.Users (
  User (..),
  UserId (..),
  createUser,
  userIdByEmail,
  createUserId,
  insertUser,
  userById,
  userByEmail,
  createEmptyUser,
  addUserToAllProjects,
) where

import Data.Aeson qualified as AE
import Data.CaseInsensitive qualified as CI
import Data.Default
import Data.Default.Instances ()
import Data.Effectful.UUID (UUIDEff)
import Data.Effectful.UUID qualified as UUID
import Data.Time (UTCTime)
import Database.PostgreSQL.Entity (Entity, insert, selectById, selectOneByField)
import Database.PostgreSQL.Entity.Types (
  CamelToSnake,
  FieldModifiers,
  GenericEntity,
  PrimaryKey,
  Schema,
  TableName,
  field,
 )
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import Effectful (Eff, type (:>))
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Time (Time, currentTime)
import GHC.Records (HasField (getField))
import Relude


instance AE.FromJSON (CI.CI Text) where
  parseJSON = fmap CI.mk . AE.parseJSON


instance AE.ToJSON (CI.CI Text) where
  toJSON = AE.toJSON . CI.original


newtype UserId = UserId {getUserId :: UUID.UUID}
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData)
  deriving anyclass (FromRow, ToRow)
  deriving
    (AE.FromJSON, AE.ToJSON, Default, FromField, Ord, ToField)
    via UUID.UUID


instance HasField "toText" UserId Text where
  getField = UUID.toText . getUserId


data User = User
  { id :: UserId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , deletedAt :: Maybe UTCTime
  , active :: Bool
  , firstName :: Text
  , lastName :: Text
  , displayImageUrl :: Text
  , email :: CI.CI Text
  , phoneNumber :: Maybe Text
  , isSudo :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "users", TableName "users", PrimaryKey "id", FieldModifiers '[CamelToSnake]] User)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] User


createUserId :: UUIDEff :> es => Eff es UserId
createUserId = UserId <$> UUID.genUUID


createUser :: (Time :> es, UUIDEff :> es) => Text -> Text -> Text -> Text -> Eff es User
createUser firstName lastName picture email = do
  uid <- createUserId
  now <- currentTime
  pure
    $ User
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
      , isSudo = False
      }


insertUser :: DB :> es => User -> Eff es ()
insertUser user = dbtToEff $ insert @User user


userById :: DB :> es => UserId -> Eff es (Maybe User)
userById userId = dbtToEff $ selectById (Only userId)


userByEmail :: DB :> es => Text -> Eff es (Maybe User)
userByEmail email = dbtToEff $ selectOneByField @User [field| email |] (Only email)


-- TODO: switch to the Eff monad
userIdByEmail :: Text -> PgT.DBT IO (Maybe UserId)
userIdByEmail email = PgT.queryOne q (Only email)
  where
    q = [sql|select id from users.users where email=?|]


createEmptyUser :: Text -> PgT.DBT IO (Maybe UserId)
createEmptyUser email = PgT.queryOne q (Only email)
  where
    q = [sql| insert into users.users (email, active) values (?, TRUE) on conflict do nothing returning id |]


-- addUserToAllProjects is a hack for development to add the user to all projects
addUserToAllProjects :: Text -> PgT.DBT IO Int64
addUserToAllProjects email = PgT.execute q values
  where
    q =
      [sql| insert into projects.project_members (active, project_id, permission, user_id)
            select true::Boolean, id, 'admin'::projects.project_permissions, (select id from users.users where email=?) from projects.projects
            on conflict do nothing; |]
    values = Only email
