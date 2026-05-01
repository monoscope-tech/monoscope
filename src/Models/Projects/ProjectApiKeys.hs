{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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

import Data.Aeson qualified as AE
import Data.Base64.Types qualified as B64T
import Data.Cache qualified as Cache
import Data.Default (Default)
import Data.Effectful.Hasql qualified as Hasql
import Data.OpenApi (ToParamSchema (..), ToSchema (..), declareNamedSchema)
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity (..), PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Effectful (Eff, type (:>))
import Effectful.Reader.Static qualified as Effectful
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Records (HasField (getField))
import Hasql.Interpolate qualified as HI
import Hasql.Pool qualified as HPool
import Hasql.Session qualified as Session
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (selectFrom)
import Relude hiding (ask, id)
import Servant.API (FromHttpApiData)
import System.Config qualified as Config
import System.Types (DB)
import "base64" Data.ByteString.Base64 qualified as B64
import "cryptonite" Crypto.Cipher.AES (AES256)
import "cryptonite" Crypto.Cipher.Types (BlockCipher (..), Cipher (..), nullIV)
import "cryptonite" Crypto.Error (throwCryptoError)


newtype ProjectApiKeyId = ProjectApiKeyId {unProjectApiKeyId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, NFData, ToField)
  deriving anyclass (FromRow, ToRow)


instance ToSchema ProjectApiKeyId where declareNamedSchema _ = declareNamedSchema (Proxy @UUID.UUID)
instance ToParamSchema ProjectApiKeyId where toParamSchema _ = toParamSchema (Proxy @UUID.UUID)


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
  let updatedAt = createdAt
      deletedAt = Nothing
      active = True
      id = ProjectApiKeyId projectKeyUUID
  pure $ ProjectApiKey{..}


insertProjectApiKey :: DB es => ProjectApiKey -> Eff es ()
insertProjectApiKey ProjectApiKey{id = kid, createdAt, updatedAt, deletedAt, active, projectId, title, keyPrefix} =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO projects.project_api_keys (id, created_at, updated_at, deleted_at, active, project_id, title, key_prefix)
           VALUES (#{kid}, #{createdAt}, #{updatedAt}, #{deletedAt}, #{active}, #{projectId}, #{title}, #{keyPrefix}) |]


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
getProjectApiKey kid =
  Hasql.interp
    (selectFrom @ProjectApiKey <> [HI.sql| WHERE id = #{kid} AND active = true |])


getProjectIdByApiKey :: (DB es, Effectful.Reader Config.AuthContext :> es) => Text -> Eff es (Maybe Projects.ProjectId)
getProjectIdByApiKey projectKey = do
  appCtx <- Effectful.ask @Config.AuthContext
  liftIO $ Cache.fetchWithCache appCtx.projectKeyCache projectKey \_ ->
    queryProjectIdByKey appCtx.hasqlPool projectKey


projectIdsByProjectApiKeys :: (DB es, Effectful.Reader Config.AuthContext :> es) => V.Vector Text -> Eff es (V.Vector (Text, Projects.ProjectId))
projectIdsByProjectApiKeys projectKeys = do
  appCtx <- Effectful.ask @Config.AuthContext
  liftIO $ do
    results <- forM (V.toList projectKeys) $ \key -> do
      maybeProjectId <- Cache.fetchWithCache appCtx.projectKeyCache key $ \k ->
        queryProjectIdByKey appCtx.hasqlPool k
      pure $ (key,) <$> maybeProjectId
    pure $ V.fromList $ catMaybes results


-- | IO-level helper for cache callbacks that need to query hasql directly.
queryProjectIdByKey :: HPool.Pool -> Text -> IO (Maybe Projects.ProjectId)
queryProjectIdByKey hpool key =
  whenRightM
    Nothing
    (HPool.use hpool (Session.statement () (HI.interp True [HI.sql| SELECT project_id FROM projects.project_api_keys WHERE key_prefix = #{key} |])))
    pure


-- AES256 encryption
encryptAPIKey :: ByteString -> ByteString -> ByteString
encryptAPIKey key = ctrCombine ctx nullIV
  where
    ctx :: AES256
    ctx = throwCryptoError $ cipherInit key


-- | decryptAPIKey :: secretKey -> TextToDecrypt -> DecryptedText as bytestring
decryptAPIKey :: ByteString -> ByteString -> ByteString
decryptAPIKey = encryptAPIKey


-- | Encrypt a random UUID with the project's secret key, base64-encode it — the one-shot
-- plaintext token presented to the user. Reuses the same format stored in @key_prefix@.
encodeApiKeyB64 :: Text -> UUID.UUID -> Text
encodeApiKeyB64 secret keyUUID =
  B64T.extractBase64 . B64.encodeBase64 $ encryptAPIKey (encodeUtf8 secret) (encodeUtf8 $ UUID.toText keyUUID)
