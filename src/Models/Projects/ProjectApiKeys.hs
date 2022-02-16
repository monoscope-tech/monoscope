{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Models.Projects.ProjectApiKeys
  ( ProjectApiKey (..),
    ProjectApiKeyId (..),
    encryptAPIKey,
    decryptAPIKey,
    newProjectApiKeys,
    insertProjectApiKey,
    projectApiKeysByProjectId,
    getProjectApiKey,
  )
where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), IV, KeySizeSpecifier (..), makeIV, nullIV)
import Crypto.Error (CryptoError (..), CryptoFailable (..), throwCryptoError)
import qualified Crypto.Random.Types as CRT
import Data.ByteArray (ByteArray)
import Data.ByteString.Base64 as B64
import Data.Default (Default)
import Data.Time (ZonedTime)
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), queryOne)
import Database.PostgreSQL.Entity.Internal.QQ (field)
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Database.PostgreSQL.Transact (DBT)
import qualified Models.Projects.Projects as Projects
import Network.Google.Prelude (FromHttpApiData)
import Optics.TH
import Relude

newtype ProjectApiKeyId = ProjectApiKeyId {unProjectApiKeyId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID
  deriving anyclass (FromRow, ToRow)

data ProjectApiKey = ProjectApiKey
  { id :: ProjectApiKeyId,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    deletedAt :: Maybe ZonedTime,
    active :: Bool,
    projectId :: Projects.ProjectId,
    title :: Text,
    keyPrefix :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "projects", TableName "project_api_keys", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ProjectApiKey)

makeFieldLabelsNoPrefix ''ProjectApiKey

newProjectApiKeys :: Projects.ProjectId -> UUID.UUID -> Text -> Text -> IO ProjectApiKey
newProjectApiKeys projectId projectKeyUUID title keyPrefix = do
  createdAt <- Time.getZonedTime
  let updatedAt = createdAt
      deletedAt = Nothing
      active = True
      id = ProjectApiKeyId projectKeyUUID
  pure $ ProjectApiKey {..}

insertProjectApiKey :: ProjectApiKey -> DBT IO ()
insertProjectApiKey = insert @ProjectApiKey

projectApiKeysByProjectId :: Projects.ProjectId -> DBT IO (Vector ProjectApiKey)
projectApiKeysByProjectId = selectManyByField @ProjectApiKey [field| project_id |]

getProjectApiKey :: ProjectApiKeyId -> DBT IO (Maybe ProjectApiKey)
getProjectApiKey = queryOne Select q
  where
    q = [sql|select id, created_at, updated_at, deleted_at, active, project_id,  title, key_prefix from projects.project_api_keys where id=? and active=true |]

-- AES256 encryption
encryptAPIKey :: ByteString -> ByteString -> ByteString
encryptAPIKey key plainData = ctrCombine ctx nullIV plainData
  where
    ctx :: AES256
    ctx = throwCryptoError $ cipherInit key

-- | decryptAPIKey :: secretKey -> TextToDecrypt -> DecryptedText as bytestring
decryptAPIKey :: ByteString -> ByteString -> ByteString
decryptAPIKey = encryptAPIKey
