{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Models.Projects.ProjectApiKeys (
  ProjectApiKey (..),
  ProjectApiKeyId (..),
  encryptAPIKey,
  decryptAPIKey,
  newProjectApiKeys,
  insertProjectApiKey,
  projectApiKeysByProjectId,
  countProjectApiKeysByProjectId,
  revokeApiKey,
  getProjectApiKey,
) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), nullIV)
import Crypto.Error (throwCryptoError)
import Data.Default (Default)
import Data.Time (ZonedTime)
import Data.Time qualified as Time
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact (DBT)
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Servant.API (FromHttpApiData)
import Prelude hiding (id)


newtype ProjectApiKeyId = ProjectApiKeyId {unProjectApiKeyId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (FromField, ToField, FromHttpApiData, Default, NFData)
  deriving anyclass (FromRow, ToRow)


instance HasField "toText" ProjectApiKeyId Text where
  getField = UUID.toText . unProjectApiKeyId


data ProjectApiKey = ProjectApiKey
  { id :: ProjectApiKeyId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , deletedAt :: Maybe ZonedTime
  , active :: Bool
  , projectId :: Projects.ProjectId
  , title :: Text
  , keyPrefix :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving (Entity) via (GenericEntity '[Schema "projects", TableName "project_api_keys", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ProjectApiKey)


newProjectApiKeys :: Projects.ProjectId -> UUID.UUID -> Text -> Text -> IO ProjectApiKey
newProjectApiKeys projectId projectKeyUUID title keyPrefix = do
  createdAt <- Time.getZonedTime
  let updatedAt = createdAt
      deletedAt = Nothing
      active = True
      id = ProjectApiKeyId projectKeyUUID
  pure $ ProjectApiKey{..}


insertProjectApiKey :: ProjectApiKey -> DBT IO ()
insertProjectApiKey = insert @ProjectApiKey


projectApiKeysByProjectId :: Projects.ProjectId -> DBT IO (Vector ProjectApiKey)
projectApiKeysByProjectId projectId = do selectManyByField @ProjectApiKey [field| project_id |] projectId


revokeApiKey :: ProjectApiKeyId -> DBT IO Int64
revokeApiKey kid = do
  execute Update q kid
  where
    q =
      [sql| UPDATE projects.project_api_keys SET deleted_at=NOW(), active=false where id=?;|]


countProjectApiKeysByProjectId :: Projects.ProjectId -> DBT IO Int
countProjectApiKeysByProjectId pid = do
  result <- query Select q pid
  case result of
    [Only count] -> return count
    v -> return $ length v
  where
    q = [sql| SELECT count(*) FROM projects.project_api_keys WHERE project_id=? |]


getProjectApiKey :: ProjectApiKeyId -> DBT IO (Maybe ProjectApiKey)
getProjectApiKey = queryOne Select q
  where
    q = [sql|select id, created_at, updated_at, deleted_at, active, project_id,  title, key_prefix from projects.project_api_keys where id=? and active=true |]


-- AES256 encryption
encryptAPIKey :: ByteString -> ByteString -> ByteString
encryptAPIKey key = ctrCombine ctx nullIV
  where
    ctx :: AES256
    ctx = throwCryptoError $ cipherInit key


-- | decryptAPIKey :: secretKey -> TextToDecrypt -> DecryptedText as bytestring
decryptAPIKey :: ByteString -> ByteString -> ByteString
decryptAPIKey = encryptAPIKey
