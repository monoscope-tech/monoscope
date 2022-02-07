{-# LANGUAGE TemplateHaskell #-}

module Models.Users.Sessions where

import Control.Monad.IO.Class
import Data.Default
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Pool
import Data.Text
import Data.Time
import qualified Data.Time as Time
import Data.UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, queryOne, withPool)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple hiding (execute)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact hiding (execute, queryOne)
import qualified Models.Projects.Projects as Projects
import Models.Users.Users (UserId)
import qualified Models.Users.Users as Users
import Optics.Core
import Relude
import Web.HttpApiData

newtype PersistentSessionId = PersistentSessionId {getPersistentSessionId :: UUID}
  deriving
    (Show, Eq, FromField, ToField, FromHttpApiData, ToHttpApiData, Default)
    via UUID

data PersistentSession = PersistentSession
  { id :: PersistentSessionId,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    userId :: UserId,
    sessionData :: SessionData,
    user :: PSUser,
    projects :: PSProjects
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, Default)
  deriving
    (Entity)
    via (GenericEntity '[Schema "users", TableName "persistent_sessions", PrimaryKey "id"] PersistentSession)

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

newPersistentSessionId :: IO PersistentSessionId
newPersistentSessionId = PersistentSessionId <$> UUID.nextRandom

persistSession ::
  (MonadIO m) =>
  Pool Connection ->
  PersistentSessionId ->
  UserId ->
  m PersistentSessionId
persistSession pool persistentSessionId userId = do
  liftIO $ withPool pool $ insertSession persistentSessionId userId (SessionData Map.empty)
  pure persistentSessionId

insertSession :: PersistentSessionId -> UserId -> SessionData -> DBT IO ()
insertSession id userId sessionData = execute Insert q (id, userId, sessionData) >> pure ()
  where
    q = [sql| insert into users.persistent_sessions(id, user_id, session_data) VALUES (?, ?, ?) |]

deleteSession :: PersistentSessionId -> DBT IO ()
deleteSession sessionId = delete @PersistentSession (Only sessionId)

getPersistentSession :: PersistentSessionId -> DBT IO (Maybe PersistentSession)
getPersistentSession sessionId = queryOne Select q value
  where
    q =
      [sql| select ps.id, ps.created_at, ps.updated_at, ps.user_id, ps.session_data, row_to_json(u) as user, json_agg(pp.* ORDER BY pp.updated_at DESC) as projects
        from users.persistent_sessions as ps 
        inner join users.users u on (u.id=ps.user_id)
        join projects.project_members ppm on (ps.user_id=ppm.user_id) 
        join projects.projects pp on (pp.id=ppm.project_id)
        where ps.id=?
        GROUP BY ps.created_at, ps.updated_at, ps.id, ps.user_id, ps.session_data, u.* ; |]
    value = Only sessionId

lookup :: Text -> SessionData -> Maybe Text
lookup key (SessionData sdMap) = Map.lookup key sdMap
