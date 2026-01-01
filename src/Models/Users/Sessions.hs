{-# LANGUAGE FlexibleContexts #-}

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
  getPersistentSession,
  newPersistentSessionId,
) where

import Data.Default
import Data.Effectful.UUID (UUIDEff)
import Data.Effectful.UUID qualified as UUID
import Data.Text.Display
import Data.Time
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple hiding (execute)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField
import Effectful
import Effectful.Error.Static (throwError)
import Effectful.Error.Static qualified as EffError
import Effectful.PostgreSQL qualified as PG
import Effectful.Reader.Static (Reader, asks)
import Effectful.Reader.Static qualified as EffReader
import Models.Projects.Projects qualified as Projects
import Models.Users.Users
import Models.Users.Users qualified as Users
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import Servant (Header, Headers, ServerError, addHeader, err302, errHeaders, getResponse)
import System.DB (DB)
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
  deriving newtype (NFData)
  deriving (Display) via ShowInstance UUID.UUID
  deriving
    (Default, Eq, FromField, FromHttpApiData, Show, ToField, ToHttpApiData)
    via UUID.UUID


newtype SessionData = SessionData {getSessionData :: Map Text Text}
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData)
  deriving anyclass (Default)
  deriving
    (FromField, ToField)
    via Aeson (Map Text Text)


newtype PSUser = PSUser {getUser :: Users.User}
  deriving stock (Generic, Show)
  deriving newtype (NFData)
  deriving anyclass (Default)
  deriving
    (FromField, ToField)
    via Aeson Users.User


newtype PSProjects = PSProjects {getProjects :: V.Vector Projects.Project}
  deriving stock (Generic, Show)
  deriving newtype (NFData)
  deriving anyclass (Default)
  deriving
    (FromField, ToField)
    via Aeson (V.Vector Projects.Project)


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
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "users", TableName "persistent_sessions", PrimaryKey "id"] PersistentSession)


newPersistentSessionId :: UUIDEff :> es => Eff es PersistentSessionId
newPersistentSessionId = PersistentSessionId <$> UUID.genUUID


insertSession :: DB es => PersistentSessionId -> UserId -> SessionData -> Eff es ()
insertSession pid userId sessionData = void $ PG.execute q (pid, userId, sessionData)
  where
    q = [sql| insert into users.persistent_sessions(id, user_id, session_data) VALUES (?, ?, ?) |]


-- TODO: getting persistent session happens very frequently, so we should create a view for this, when our user base grows.
getPersistentSession :: DB es => PersistentSessionId -> Eff es (Maybe PersistentSession)
getPersistentSession sessionId = listToMaybe <$> PG.query q value
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
    , setCookieName = "monoscope_session"
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
  , theme :: Text
  }
  deriving stock (Generic, Show)


sessionAndProject
  :: (DB es, EffError.Error ServerError :> es, EffReader.Reader (Headers '[Header "Set-Cookie" SetCookie] Session) :> es)
  => Projects.ProjectId
  -> Eff es (Session, Projects.Project)
sessionAndProject pid = do
  sess <- getSession
  let projects = sess.persistentSession.projects.getProjects
  case V.find (\v -> v.id == pid) projects of
    -- Don't use cached data for onboarding projects as they're actively being updated
    Just p | p.paymentPlan /= "ONBOARDING" -> pure (sess, p)
    -- Fetch fresh from DB for: onboarding projects, not found in cache, nil project, or sudo users
    _
      | pid == UUIDId UUID.nil || sess.user.isSudo ->
          Projects.projectById pid >>= \case
            Just p -> pure (sess, p)
            Nothing -> throwError $ err302{errHeaders = [("Location", "/?missingProjectPermission")]}
    _ -> throwError $ err302{errHeaders = [("Location", "/?missingProjectPermission")]}
