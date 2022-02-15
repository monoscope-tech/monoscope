{-# LANGUAGE UndecidableInstances #-}

module Server (app) where

import Config (AuthContext, DashboardM, EnvConfig, HeadersTrigger, HeadersTriggerRedirect, ctxToHandler, env, pool)
import qualified Control.Lens as L
import Control.Monad.Trans.Either
import qualified Crypto.JOSE.Compact
import qualified Crypto.JOSE.JWK
import qualified Crypto.JWT
import Data.Aeson.Lens (key, _String)
import Data.Pool (Pool)
import Data.Time.LocalTime (getZonedTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Connection)
import Lucid
import qualified Models.Apis.Endpoints as Endpoints
import qualified Models.Apis.Fields as Fields
import qualified Models.Projects.Projects as Projects
import qualified Models.Users.Sessions as Sessions
import qualified Models.Users.Users as Users
import Network.Wai (Application, Request)
import Network.Wreq (FormParam ((:=)), defaults, getWith, header, post, responseBody)
import Optics.Operators
import qualified Pages.Api as Api
import qualified Pages.Dashboard as Dashboard
import qualified Pages.Endpoints.EndpointDetails as EndpointDetails
import qualified Pages.Endpoints.EndpointList as EndpointList
import qualified Pages.Projects.CreateProject as CreateProject
import qualified Pages.Projects.ListProjects as ListProjects
import Relude hiding (hoistMaybe)
import Servant
  ( AuthProtect,
    Capture,
    Context (EmptyContext, (:.)),
    FormUrlEncoded,
    Get,
    Handler,
    Header,
    Headers,
    JSON,
    NoContent (..),
    Post,
    QueryParam,
    Raw,
    ReqBody,
    ServerError (errHeaders),
    ServerT,
    StdMethod (GET),
    Verb,
    addHeader,
    hoistServer,
    hoistServerWithContext,
    noHeader,
    serve,
    serveWithContext,
    throwError,
    type (:<|>) (..),
    type (:>),
  )
import Servant.HTML.Lucid
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Servant.Server.StaticFiles
import SessionCookies (craftSessionCookie)
import Web.Auth
import Web.Cookie (SetCookie)

type GetRedirect = Verb 'GET 302

--
-- API Section
type ProtectedAPI =
  Get '[HTML] (Html ())
    :<|> "p" :> "new" :> Get '[HTML] (Html ()) -- p represents project
    :<|> "p" :> "new" :> ReqBody '[FormUrlEncoded] CreateProject.CreateProjectForm :> Post '[HTML] (HeadersTriggerRedirect (Html ()))
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "endpoints" :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "endpoints" :> Capture "endpoints_id" Endpoints.EndpointId :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "apis" :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "apis" :> ReqBody '[FormUrlEncoded] Api.GenerateAPIKeyForm :> Post '[HTML] (HeadersTrigger (Html ()))
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "fields" :> Capture "field_id" Fields.FieldId :> Get '[HTML] (Html ())

type PublicAPI =
  "login" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "logout" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "auth_callback" :> QueryParam "code" Text :> QueryParam "state" Text :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] (Html ()))
    :<|> "assets" :> Raw

type API =
  AuthProtect "apitoolkit_session" :> ProtectedAPI
    :<|> PublicAPI

type instance AuthServerData (AuthProtect "apitoolkit_session") = Sessions.PersistentSession

--
--
app :: Pool Connection -> AuthContext -> Application
app dbConn ctx = serveWithContext api (genAuthServerContext dbConn) $ hoistServerWithContext api ctxProxy (ctxToHandler ctx) server

api :: Proxy API
api = Proxy

ctxProxy :: Proxy '[AuthHandler Request Sessions.PersistentSession]
ctxProxy = Proxy

-- | Our API, where we provide all the author-supplied handlers for each end
-- point. Note that 'privateDataFunc' is a function that takes 'Account' as an
-- argument. We don't worry about the authentication instrumentation here,
-- that is taken care of by supplying context
protectedServer :: Sessions.PersistentSession -> ServerT ProtectedAPI DashboardM
protectedServer sess =
  ListProjects.listProjectsGetH sess
    :<|> CreateProject.createProjectGetH sess
    :<|> CreateProject.createProjectPostH sess
    :<|> Dashboard.dashboardGetH sess
    :<|> EndpointList.endpointListH sess
    :<|> EndpointDetails.endpointDetailsH sess
    :<|> Api.apiGetH sess
    :<|> Api.apiPostH sess
    :<|> EndpointDetails.fieldDetailsPartialH sess

publicServer :: ServerT PublicAPI DashboardM
publicServer =
  loginH
    :<|> logoutH
    :<|> authCallbackH
    :<|> serveDirectoryWebApp "./static/assets"

server :: ServerT API DashboardM
server = protectedServer :<|> publicServer
