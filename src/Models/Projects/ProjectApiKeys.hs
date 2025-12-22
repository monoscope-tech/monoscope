{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Models.Projects.ProjectApiKeys (
  ProjectApiKey (..),
  ProjectApiKeyId (..),
  encryptAPIKey,
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

import Data.Cache qualified as Cache
import Data.Default (Default)
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (_insert, _selectWhere)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Effectful (Eff, IOE, type (:>))
import Effectful.PostgreSQL (WithConnection, withConnection)
import Effectful.PostgreSQL qualified as PG
import Effectful.Reader.Static qualified as Effectful
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Relude hiding (ask, id)
import Servant.API (FromHttpApiData)
import System.Config qualified as Config
import "cryptonite" Crypto.Cipher.AES (AES256)
import "cryptonite" Crypto.Cipher.Types (BlockCipher (..), Cipher (..), nullIV)
import "cryptonite" Crypto.Error (throwCryptoError)


newtype ProjectApiKeyId = ProjectApiKeyId {unProjectApiKeyId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (Default, FromField, FromHttpApiData, NFData, ToField)
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
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "projects", TableName "project_api_keys", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ProjectApiKey)


newProjectApiKeys :: Time :> es => Projects.ProjectId -> UUID.UUID -> Text -> Text -> Eff es ProjectApiKey
newProjectApiKeys projectId projectKeyUUID title keyPrefix = do
  createdAt <- Time.currentTime
  let updatedAt = createdAt
      deletedAt = Nothing
      active = True
      id = ProjectApiKeyId projectKeyUUID
  pure $ ProjectApiKey{..}


insertProjectApiKey :: (IOE :> es, WithConnection :> es) => ProjectApiKey -> Eff es ()
insertProjectApiKey apiKey = void $ PG.execute (_insert @ProjectApiKey) apiKey


projectApiKeysByProjectId :: (IOE :> es, WithConnection :> es) => Projects.ProjectId -> Eff es (V.Vector ProjectApiKey)
projectApiKeysByProjectId projectId = V.fromList <$> PG.query (_selectWhere @ProjectApiKey [[field| project_id |]]) (Only projectId)


revokeApiKey :: (IOE :> es, WithConnection :> es) => ProjectApiKeyId -> Eff es Int64
revokeApiKey kid = PG.execute q (Only kid)
  where
    q = [sql| UPDATE projects.project_api_keys SET deleted_at=NOW(), active=false where id=?;|]


activateApiKey :: (IOE :> es, WithConnection :> es) => ProjectApiKeyId -> Eff es Int64
activateApiKey kid = PG.execute q (Only kid)
  where
    q = [sql| UPDATE projects.project_api_keys SET deleted_at=null, active=true where id=?;|]


getProjectApiKey :: (IOE :> es, WithConnection :> es) => ProjectApiKeyId -> Eff es (Maybe ProjectApiKey)
getProjectApiKey kid = listToMaybe <$> PG.query q (Only kid)
  where
    q = [sql|select id, created_at, updated_at, deleted_at, active, project_id,  title, key_prefix from projects.project_api_keys where id=? and active=true |]


getProjectIdByApiKey :: (Effectful.Reader Config.AuthContext :> es, IOE :> es, WithConnection :> es) => Text -> Eff es (Maybe Projects.ProjectId)
getProjectIdByApiKey projectKey = do
  appCtx <- Effectful.ask @Config.AuthContext
  withConnection \conn -> liftIO
    $ Cache.fetchWithCache appCtx.projectKeyCache projectKey \_ ->
      listToMaybe <$> PGS.query conn q (Only projectKey)
  where
    q = [sql| select project_id from projects.project_api_keys where key_prefix=?|]


projectIdsByProjectApiKeys :: (Effectful.Reader Config.AuthContext :> es, IOE :> es, WithConnection :> es) => V.Vector Text -> Eff es (V.Vector (Text, Projects.ProjectId))
projectIdsByProjectApiKeys projectKeys = do
  appCtx <- Effectful.ask @Config.AuthContext
  withConnection \conn -> liftIO $ do
    results <- forM (V.toList projectKeys) $ \key -> do
      maybeProjectId <- Cache.fetchWithCache appCtx.projectKeyCache key $ \k ->
        listToMaybe <$> PGS.query conn q (Only k)
      case maybeProjectId of
        Nothing -> pure Nothing
        Just projectId -> pure $ Just (key, projectId)
    pure $ V.fromList $ catMaybes results
  where
    q = [sql| select project_id from projects.project_api_keys where key_prefix=?|]


-- AES256 encryption
encryptAPIKey :: ByteString -> ByteString -> ByteString
encryptAPIKey key = ctrCombine ctx nullIV
  where
    ctx :: AES256
    ctx = throwCryptoError $ cipherInit key


-- | decryptAPIKey :: secretKey -> TextToDecrypt -> DecryptedText as bytestring
decryptAPIKey :: ByteString -> ByteString -> ByteString
decryptAPIKey = encryptAPIKey
