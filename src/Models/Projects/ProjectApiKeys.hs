module Models.Projects.ProjectApiKeys (
  ProjectApiKey (..),
  ProjectApiKeyId (..),
  encryptAPIKey,
  encodeApiKeyB64,
  getProjectIdByApiKey,
  activateApiKey,
  decryptAPIKey,
  newProjectApiKeys,
  insertProjectApiKey,
  projectApiKeysByProjectId,
  projectIdsByProjectApiKeys,
  revokeApiKey,
  getProjectApiKey,
)
where

import Control.Exception (throwIO)
import Data.Aeson qualified as AE
import Data.Base64.Types qualified as B64T
import Data.Cache qualified as Cache
import Data.Default (Default)
import Data.Effectful.Hasql qualified as Hasql
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity (..), PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Effectful (Eff, type (:>))
import Effectful.Log (Log)
import Effectful.Reader.Static qualified as Effectful
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Records (HasField (getField))
import Hasql.Interpolate qualified as HI
import Hasql.Session qualified as Session
import Models.Projects.Projects qualified as Projects
import OpenTelemetry.Instrumentation.Hasql qualified as OHasql
import Pkg.DeriveUtils (selectFrom)
import Relude hiding (ask, id)
import Servant.API (FromHttpApiData)
import System.Config qualified as Config
import System.Logging qualified as Log
import System.Types (DB)
import "base64" Data.ByteString.Base64 qualified as B64
import "cryptonite" Crypto.Cipher.AES (AES256)
import "cryptonite" Crypto.Cipher.Types (BlockCipher (..), Cipher (..), nullIV)
import "cryptonite" Crypto.Error (throwCryptoError)


newtype ProjectApiKeyId = ProjectApiKeyId {unProjectApiKeyId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, NFData, ToField, ToParamSchema, ToSchema)
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
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "projects", TableName "project_api_keys", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ProjectApiKey)


newProjectApiKeys :: Time :> es => Projects.ProjectId -> UUID.UUID -> Text -> Text -> Eff es ProjectApiKey
newProjectApiKeys projectId projectKeyUUID title keyPrefix = do
  createdAt <- Time.currentTime
  pure ProjectApiKey{id = ProjectApiKeyId projectKeyUUID, updatedAt = createdAt, deletedAt = Nothing, active = True, ..}


insertProjectApiKey :: DB es => ProjectApiKey -> Eff es ()
insertProjectApiKey ProjectApiKey{..} =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO projects.project_api_keys (id, created_at, updated_at, deleted_at, active, project_id, title, key_prefix)
           VALUES (#{id}, #{createdAt}, #{updatedAt}, #{deletedAt}, #{active}, #{projectId}, #{title}, #{keyPrefix}) |]


projectApiKeysByProjectId :: DB es => Projects.ProjectId -> Eff es [ProjectApiKey]
projectApiKeysByProjectId projectId =
  Hasql.interp
    (selectFrom @ProjectApiKey <> [HI.sql| WHERE project_id = #{projectId} |])


revokeApiKey :: (DB es, Time :> es) => ProjectApiKeyId -> Eff es Int64
revokeApiKey kid = do
  now <- Time.currentTime
  Hasql.interpExecute [HI.sql| UPDATE projects.project_api_keys SET deleted_at = #{now}, active = false WHERE id = #{kid} |]


activateApiKey :: DB es => ProjectApiKeyId -> Eff es Int64
activateApiKey kid =
  Hasql.interpExecute
    [HI.sql| UPDATE projects.project_api_keys SET deleted_at = null, active = true WHERE id = #{kid} |]


getProjectApiKey :: DB es => ProjectApiKeyId -> Eff es (Maybe ProjectApiKey)
getProjectApiKey kid = Hasql.interp (selectFrom @ProjectApiKey <> [HI.sql| WHERE id = #{kid} AND active = true |])


-- | Cache-backed lookup. DB-level failures (connection refused, pool exhausted, …)
-- throw 'Hasql.HasqlException' so the caller (ultimately 'Pkg.Queue') routes the batch
-- to the DLQ; a clean @Nothing@ stays the legitimate "key not in DB" signal.
--
-- A live key whose /project/ is switched off is a different thing from a key we
-- don't know, and it is the one that needs a human: "Engine/API Prod" ingested
-- nothing for 17 days behind this @Nothing@ while its subscription kept billing.
-- The log is emitted on the cache miss, so a rejected key costs one line per key
-- per TTL rather than one per request.
getProjectIdByApiKey :: (DB es, Effectful.Reader Config.AuthContext :> es, Log :> es) => Text -> Eff es (Maybe Projects.ProjectId)
getProjectIdByApiKey projectKey = do
  appCtx <- Effectful.ask @Config.AuthContext
  liftIO (Cache.lookup appCtx.projectKeyCache projectKey) >>= \case
    Just cached -> pure cached
    Nothing -> do
      row :: Maybe (Projects.ProjectId, Bool) <-
        liftIO
          $ OHasql.use
            appCtx.hasqlPool
            ( Session.statement
                ()
                ( HI.interp
                    True
                    [HI.sql|
      SELECT k.project_id, (p.active AND p.deleted_at IS NULL)
        FROM projects.project_api_keys k
        JOIN projects.projects p ON p.id = k.project_id
       WHERE k.key_prefix = #{projectKey} AND k.active = TRUE AND k.deleted_at IS NULL
    |]
                )
            )
          >>= either (throwIO . Hasql.HasqlException) pure
      whenJust (mfilter (not . snd) row) \(pid, _) ->
        Log.logAttention "api key rejected: project is deactivated or deleted" ("project_id", pid.toText)
      let pidM = fst <$> mfilter snd row
      liftIO $ Cache.insert appCtx.projectKeyCache projectKey pidM
      pure pidM


projectIdsByProjectApiKeys :: (DB es, Effectful.Reader Config.AuthContext :> es, Log :> es) => V.Vector Text -> Eff es (V.Vector (Text, Projects.ProjectId))
projectIdsByProjectApiKeys projectKeys =
  V.catMaybes <$> forM projectKeys \key -> fmap (key,) <$> getProjectIdByApiKey key


-- | AES256-CTR; symmetric, so 'decryptAPIKey' is the same function.
encryptAPIKey :: ByteString -> ByteString -> ByteString
encryptAPIKey key = ctrCombine ctx nullIV
  where
    ctx :: AES256
    ctx = throwCryptoError $ cipherInit key


-- | @decryptAPIKey secretKey ciphertext@ — inverse of 'encryptAPIKey'.
decryptAPIKey :: ByteString -> ByteString -> ByteString
decryptAPIKey = encryptAPIKey


-- | Encrypt a random UUID with the project's secret key, base64-encode it — the one-shot
-- plaintext token presented to the user. Reuses the same format stored in @key_prefix@.
encodeApiKeyB64 :: Text -> UUID.UUID -> Text
encodeApiKeyB64 secret keyUUID =
  B64T.extractBase64 . B64.encodeBase64 $ encryptAPIKey (encodeUtf8 secret) (encodeUtf8 $ UUID.toText keyUUID)
