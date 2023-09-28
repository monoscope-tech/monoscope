{-# LANGUAGE TemplateHaskell #-}

module Models.Users.Sessions (
  PersistentSessionId (..),
  PersistentSession (..),
  SessionData (..),
  PSUser (..),
  PSProjects (..),
  persistSession,
  insertSession,
  deleteSession,
  getPersistentSession,
  lookup,
  newPersistentSessionId,
) where

import Control.Monad.IO.Class
import Data.Default
import Data.Map.Strict qualified as Map
import Data.Pool
import Data.Text
import Data.Time
import Data.UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, queryOne, withPool)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple hiding (execute)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact hiding (execute, queryOne)
import Models.Projects.Projects qualified as Projects
import Models.Users.Users (UserId)
import Models.Users.Users qualified as Users
import Optics.TH
import Relude
import Web.HttpApiData

newtype PersistentSessionId = PersistentSessionId {getPersistentSessionId :: UUID}
  deriving
    (Show, Eq, FromField, ToField, FromHttpApiData, ToHttpApiData, Default)
    via UUID

newtype SessionData = SessionData {getSessionData :: Map Text Text}
  deriving stock (Show, Eq, Generic)
  deriving
    (FromField, ToField)
    via Aeson (Map Text Text)
  deriving anyclass (Default)

newtype PSUser = PSUser {getUser :: Users.User}
  deriving stock (Show, Generic)
  deriving
    (FromField)
    via Aeson Users.User
  deriving anyclass (Default)

newtype PSProjects = PSProjects {getProjects :: Vector.Vector Projects.Project}
  deriving stock (Show, Generic)
  deriving
    (FromField)
    via Aeson (Vector.Vector Projects.Project)
  deriving anyclass (Default)

data PersistentSession = PersistentSession
  { id :: PersistentSessionId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , userId :: UserId
  , sessionData :: SessionData
  , user :: PSUser
  , isSudo :: Bool -- super user/admin
  , projects :: PSProjects
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, Default)
  deriving
    (Entity)
    via (GenericEntity '[Schema "users", TableName "persistent_sessions", PrimaryKey "id"] PersistentSession)

makeFieldLabelsNoPrefix ''PersistentSession

newPersistentSessionId :: IO PersistentSessionId
newPersistentSessionId = PersistentSessionId <$> UUID.nextRandom

persistSession
  :: MonadIO m
  => Pool Connection
  -> PersistentSessionId
  -> UserId
  -> m PersistentSessionId
persistSession pool persistentSessionId userId = do
  liftIO $ withPool pool $ insertSession persistentSessionId userId (SessionData Map.empty)
  pure persistentSessionId

insertSession :: PersistentSessionId -> UserId -> SessionData -> DBT IO ()
insertSession pid userId sessionData = execute Insert q (pid, userId, sessionData) >> pass
  where
    q = [sql| insert into users.persistent_sessions(id, user_id, session_data) VALUES (?, ?, ?) |]

deleteSession :: PersistentSessionId -> DBT IO ()
deleteSession sessionId = delete @PersistentSession (Only sessionId)

-- TODO: getting persistent session happens very frequently, so we should create a view for this, when our user base grows.
getPersistentSession :: PersistentSessionId -> DBT IO (Maybe PersistentSession)
getPersistentSession sessionId = queryOne Select q value
  where
    q =
      [sql| select ps.id, ps.created_at, ps.updated_at, ps.user_id, ps.session_data, row_to_json(u) as user, u.is_sudo,
        COALESCE(json_agg(pp.* ORDER BY pp.updated_at DESC) FILTER (WHERE pp.id is not NULL AND pp.deleted_at IS NULL),'[]') as projects
        from users.persistent_sessions as ps 
        left join users.users u on (u.id=ps.user_id)
        left join projects.project_members ppm on (ps.user_id=ppm.user_id) 
        left join projects.projects pp on (pp.id=ppm.project_id)
        where ps.id=?
        GROUP BY ps.created_at, ps.updated_at, ps.id, ps.user_id, ps.session_data, u.* ,u.is_sudo; |]
    value = Only sessionId

lookup :: Text -> SessionData -> Maybe Text
lookup key (SessionData sdMap) = Map.lookup key sdMap
