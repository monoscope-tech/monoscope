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
) where

import Data.Aeson qualified as AE
import Data.CaseInsensitive qualified as CI
import Data.Default
import Data.Default.Instances ()
import Data.Effectful.UUID (UUIDEff)
import Data.Effectful.UUID qualified as UUID
import Data.Time (UTCTime)
import Database.PostgreSQL.Entity (Entity, _insert, _selectWhere)
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
import Deriving.Aeson qualified as DAE
import Effectful (Eff, IOE, type (:>))
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as PG
import Effectful.Time (Time, currentTime)
import GHC.Records (HasField (getField))
import Relude
import Servant (FromHttpApiData)


instance AE.FromJSON (CI.CI Text) where
  parseJSON = fmap CI.mk . AE.parseJSON


instance AE.ToJSON (CI.CI Text) where
  toJSON = AE.toJSON . CI.original


newtype UserId = UserId {getUserId :: UUID.UUID}
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData)
  deriving anyclass (FromRow, ToRow)
  deriving
    (AE.FromJSON, AE.ToJSON, Default, FromField, FromHttpApiData, Ord, ToField)
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


insertUser :: (IOE :> es, WithConnection :> es) => User -> Eff es ()
insertUser user = void $ PG.execute (_insert @User) user


userById :: (IOE :> es, WithConnection :> es) => UserId -> Eff es (Maybe User)
userById userId = listToMaybe <$> PG.query (_selectWhere @User [[field| id |]]) (Only userId)


userByEmail :: (IOE :> es, WithConnection :> es) => Text -> Eff es (Maybe User)
userByEmail email = listToMaybe <$> PG.query (_selectWhere @User [[field| email |]]) (Only email)


userIdByEmail :: (IOE :> es, WithConnection :> es) => Text -> Eff es (Maybe UserId)
userIdByEmail email = listToMaybe <$> PG.query q (Only email)
  where
    q = [sql|select id from users.users where email=?|]


createEmptyUser :: (IOE :> es, WithConnection :> es) => Text -> Eff es (Maybe UserId)
createEmptyUser email = listToMaybe <$> PG.query q (Only email)
  where
    q = [sql| insert into users.users (email, active) values (?, TRUE) on conflict do nothing returning id |]
