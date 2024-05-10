{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Models.Users.Sessions (
  PersistentSessionId (..),
  PersistentSession (..),
  Session (..),
  sessionAndProject,
  craftSessionCookie,
  SessionData (..),
  PSUser (..),
  PSProjects (..),
  addCookie,
  emptySessionCookie,
  getSession,
  insertSession,
  deleteSession,
  getPersistentSession,
  lookup,
  newPersistentSessionId,
) where

import Data.Default
import Data.Map.Strict qualified as Map
import Data.Text.Display
import Data.Time
import Data.Vector qualified as V
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Reader.Static qualified as EffReader
import Effectful.Error.Static qualified as EffError 
import Database.PostgreSQL.Entity.DBT (QueryNature (..))
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple hiding (execute)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField
import Effectful.Error.Static
import Database.PostgreSQL.Transact hiding (DB, execute, queryOne)
import Effectful.Reader.Static (Reader, asks)
import Models.Projects.Projects qualified as Projects
import Models.Users.Users 
import Models.Users.Users qualified as Users
import Relude
import Servant (Header, Headers, addHeader, getResponse, errHeaders, err302,  ServerError)
import Web.Cookie (
  SetCookie (
    setCookieHttpOnly,
    setCookieMaxAge,
    setCookieName,
    setCookiePath,
    setCookieSameSite,
    setCookieSecure,
    setCookieValue
  ),
  defaultSetCookie,
  sameSiteLax,
 )
import Web.HttpApiData


newtype PersistentSessionId = PersistentSessionId {getPersistentSessionId :: UUID.UUID}
  deriving
    (Show, Eq, FromField, ToField, FromHttpApiData, ToHttpApiData, Default)
    via UUID.UUID
  deriving newtype (NFData)
  deriving (Display) via ShowInstance UUID.UUID


newtype SessionData = SessionData {getSessionData :: Map Text Text}
  deriving stock (Show, Eq, Generic)
  deriving
    (FromField, ToField)
    via Aeson (Map Text Text)
  deriving newtype (NFData)
  deriving anyclass (Default)


newtype PSUser = PSUser {getUser :: Users.User}
  deriving stock (Show, Generic)
  deriving
    (FromField, ToField)
    via Aeson Users.User
  deriving newtype (NFData)
  deriving anyclass (Default)


newtype PSProjects = PSProjects {getProjects :: Vector.Vector Projects.Project}
  deriving stock (Show, Generic)
  deriving
    (FromField, ToField)
    via Aeson (Vector.Vector Projects.Project)
  deriving anyclass (Default)
  deriving newtype (NFData)


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
  deriving anyclass (FromRow, ToRow, Default, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "users", TableName "persistent_sessions", PrimaryKey "id"] PersistentSession)


newPersistentSessionId :: IO PersistentSessionId
newPersistentSessionId = PersistentSessionId <$> UUID.nextRandom


insertSession :: PersistentSessionId -> UserId -> SessionData -> DBT IO ()
insertSession pid userId sessionData = DBT.execute Insert q (pid, userId, sessionData) >> pass
  where
    q = [sql| insert into users.persistent_sessions(id, user_id, session_data) VALUES (?, ?, ?) |]


deleteSession :: PersistentSessionId -> DBT IO ()
deleteSession sessionId = delete @PersistentSession (Only sessionId)


-- TODO: getting persistent session happens very frequently, so we should create a view for this, when our user base grows.
getPersistentSession :: PersistentSessionId -> DBT IO (Maybe PersistentSession)
getPersistentSession sessionId = DBT.queryOne Select q value
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


getSession
  :: Effectful.Reader.Static.Reader (Headers '[Header "Set-Cookie" SetCookie] Session) :> es
  => Eff es Session
getSession = Effectful.Reader.Static.asks (getResponse @'[Header "Set-Cookie" SetCookie])


-- | This function builds a cookie with the provided content
craftSessionCookie
  :: PersistentSessionId
  -- ^ Cookie content
  -> Bool
  -- ^ Remember the cookie for 1 week
  -> SetCookie
craftSessionCookie (PersistentSessionId content) rememberSession =
  defaultSetCookie
    { setCookieValue = UUID.toASCIIBytes content
    , setCookieName = "apitoolkit_session"
    , setCookiePath = Just "/"
    , setCookieHttpOnly = True
    , setCookieSameSite = Just sameSiteLax
    , setCookieMaxAge = if rememberSession then Just 604800 else Nothing
    , setCookieSecure = True
    }


emptySessionCookie :: SetCookie
emptySessionCookie =
  defaultSetCookie
    { setCookieName = "apitoolkit_session"
    , setCookieValue = ""
    , setCookieMaxAge = Just 0
    }


addCookie :: SetCookie -> a -> Headers '[Header "Set-Cookie" SetCookie] a
addCookie = addHeader


data Session = Session
  { sessionId :: PersistentSessionId
  , persistentSession :: PersistentSession
  , user :: Users.User
  , requestID :: Text
  , isSidebarClosed :: Bool
  }
  deriving stock (Generic, Show)



sessionAndProject
  :: (DB :> es, (EffReader.Reader (Headers '[Header "Set-Cookie" SetCookie] Session)) :> es, EffError.Error ServerError :> es )
  => Projects.ProjectId
  -> Eff es (Session, Projects.Project)
sessionAndProject pid = do
  sess <- getSession
  let projects = sess.persistentSession.projects.getProjects
  case (V.find (\v -> v.id == pid) projects) of
    Just p -> pure (sess, p)
    Nothing ->
      if pid == Projects.ProjectId UUID.nil || sess.user.isSudo
        then do
          (dbtToEff $ Projects.projectById pid) >>= \case
            Just p -> pure (sess, p)
            Nothing -> throwError $ err302{errHeaders = [("Location", "/p/?missingProjectPermission")]} 
        else throwError $ err302{errHeaders = [("Location", "/p/?missingProjectPermission")]} 
