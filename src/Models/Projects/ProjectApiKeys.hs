{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Models.Projects.ProjectApiKeys (
  ProjectApiKey (..),
  ProjectApiKeyId (..),
  encryptAPIKey,
  getProjectIdByApiKey,
  decryptAPIKey,
  newProjectApiKeys,
  insertProjectApiKey,
  projectApiKeysByProjectId,
  countProjectApiKeysByProjectId,
  projectIdsByProjectApiKeys,
  revokeApiKey,
  getProjectApiKey,
)
where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), nullIV)
import Crypto.Error (throwCryptoError)
import Data.Cache qualified as Cache
import Data.Default (Default)
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, query, queryOne, withPool)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact (DBT)
import Effectful (Eff, IOE, type (:>))
import Effectful.PostgreSQL.Transact.Effect (DB, getPool)
import Effectful.Reader.Static qualified as Effectful
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Relude hiding (ask, id)
import Servant.API (FromHttpApiData)
import System.Config qualified as Config


newtype ProjectApiKeyId = ProjectApiKeyId {unProjectApiKeyId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (FromField, ToField, FromHttpApiData, Default, NFData)
  deriving anyclass (FromRow, ToRow)


instance HasField "toText" ProjectApiKeyId Text where
  getField = UUID.toText . unProjectApiKeyId


data ProjectApiKey = ProjectApiKey
  { id :: ProjectApiKeyId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , deletedAt :: Maybe UTCTime
  , active :: Bool
  , projectId :: Projects.ProjectId
  , title :: Text
  , keyPrefix :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving (Entity) via (GenericEntity '[Schema "projects", TableName "project_api_keys", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ProjectApiKey)


newProjectApiKeys :: Time :> es => Projects.ProjectId -> UUID.UUID -> Text -> Text -> Eff es ProjectApiKey
newProjectApiKeys projectId projectKeyUUID title keyPrefix = do
  createdAt <- Time.currentTime
  let updatedAt = createdAt
      deletedAt = Nothing
      active = True
      id = ProjectApiKeyId projectKeyUUID
  pure $ ProjectApiKey{..}


insertProjectApiKey :: ProjectApiKey -> DBT IO ()
insertProjectApiKey = insert @ProjectApiKey


projectApiKeysByProjectId :: Projects.ProjectId -> DBT IO (V.Vector ProjectApiKey)
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


getProjectIdByApiKey :: (DB :> es, IOE :> es, Effectful.Reader Config.AuthContext :> es) => Text -> Eff es (Maybe Projects.ProjectId)
getProjectIdByApiKey projectKey = do
  pool <- getPool
  appCtx <- Effectful.ask @Config.AuthContext
  liftIO $ Cache.fetchWithCache appCtx.projectKeyCache projectKey \_ ->
    withPool pool $ queryOne Select q (Only projectKey)
  where
    q = [sql| select project_id from projects.project_api_keys where key_prefix=?|]


projectIdsByProjectApiKeys :: V.Vector Text -> DBT IO (V.Vector (Text, Projects.ProjectId, Integer))
projectIdsByProjectApiKeys projectKeys = query Select q (Only projectKeys)
  where
    q =
      [sql| 
SELECT 
  k.key_prefix, 
  k.project_id, 
  COALESCE(span_counts.daily_events_count, 0) AS daily_events_count
FROM projects.project_api_keys k
LEFT JOIN projects.projects p ON p.id = k.project_id
LEFT JOIN (
    SELECT 
      e.project_id, 
      COUNT(*) AS daily_events_count
    FROM telemetry.spans e
    JOIN projects.projects p ON p.id = e.project_id
    WHERE p.payment_plan = 'Free' 
      AND e.timestamp > NOW() - INTERVAL '1 day'
    GROUP BY e.project_id
) span_counts ON span_counts.project_id = k.project_id
WHERE k.key_prefix = ANY(?)|]


-- AES256 encryption
encryptAPIKey :: ByteString -> ByteString -> ByteString
encryptAPIKey key = ctrCombine ctx nullIV
  where
    ctx :: AES256
    ctx = throwCryptoError $ cipherInit key


-- | decryptAPIKey :: secretKey -> TextToDecrypt -> DecryptedText as bytestring
decryptAPIKey :: ByteString -> ByteString -> ByteString
decryptAPIKey = encryptAPIKey
