module Models.Users.Sessions where

import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Pool
import Data.Text
-- import Data.Text.Display
import Data.Time
import qualified Data.Time as Time
import Data.UUID
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact
-- import Env.Generic
import Models.Users.Users (UserId)
import Optics.Core
import Relude
import Web.HttpApiData
import Data.Default

newtype PersistentSessionId = PersistentSessionId {getPersistentSessionId :: UUID}
  deriving
    (Show, Eq, FromField, ToField, FromHttpApiData, ToHttpApiData, Default)
    via UUID

-- deriving
--   (Display)
--   via ShowInstance UUID

data PersistentSession = PersistentSession
  { persistentSessionId :: PersistentSessionId,
    userId :: UserId,
    sessionData :: SessionData,
    createdAt :: ZonedTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, Default)
  deriving
    (Entity)
    via (GenericEntity '[TableName "persistent_sessions"] PersistentSession)

newtype SessionData = SessionData {getSessionData :: Map Text Text}
  deriving stock (Show, Eq, Generic)
  deriving
    (FromField, ToField)
    via Aeson (Map Text Text)
  deriving anyclass (Default)

newPersistentSessionId :: IO PersistentSessionId
newPersistentSessionId = PersistentSessionId <$> UUID.nextRandom

newPersistentSession :: UserId -> PersistentSessionId -> IO PersistentSession
newPersistentSession userId persistentSessionId = do
  createdAt <- Time.getZonedTime
  let sessionData = SessionData Map.empty
  pure PersistentSession {..}

persistSession ::
  (MonadIO m) =>
  Pool Connection ->
  PersistentSessionId ->
  UserId ->
  m PersistentSessionId
persistSession pool persistentSessionId userId = do
  persistentSession <- liftIO $ newPersistentSession userId persistentSessionId
  liftIO $ withPool pool $ insertSession persistentSession
  pure $ persistentSession ^. #persistentSessionId

insertSession :: PersistentSession -> DBT IO ()
insertSession = insert @PersistentSession

deleteSession :: PersistentSessionId -> DBT IO ()
deleteSession sessionId = delete @PersistentSession (Only sessionId)

getPersistentSession :: PersistentSessionId -> DBT IO (Maybe PersistentSession)
getPersistentSession sessionId = selectById @PersistentSession (Only sessionId)

lookup :: Text -> SessionData -> Maybe Text
lookup key (SessionData sdMap) = Map.lookup key sdMap
