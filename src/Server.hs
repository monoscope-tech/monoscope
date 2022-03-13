{-# LANGUAGE UndecidableInstances #-}

module Server (app) where

import Colog (LogAction)
import Config (DashboardM, HeadersTrigger, HeadersTriggerRedirect, ctxToHandler)
import Config qualified
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Lucid
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Network.Wai (Request)
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.Api qualified as Api
import Pages.Log qualified as Log
import Pages.Dashboard qualified as Dashboard
import Pages.Endpoints.EndpointDetails qualified as EndpointDetails
import Pages.Endpoints.EndpointList qualified as EndpointList
import Pages.ManualIngestion qualified as ManualIngestion
import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.ListProjects qualified as ListProjects
import Relude
import Servant
import Servant.HTML.Lucid
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData)
import Web.Auth (authCallbackH, genAuthServerContext, loginH, loginRedirectH, logoutH)
import Web.ClientMetadata qualified as ClientMetadata
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
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "manual_ingest" :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "manual_ingest" :> ReqBody '[FormUrlEncoded] ManualIngestion.RequestMessageForm :> Post '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "log" :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "bulk_seed_and_ingest" :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "bulk_seed_and_ingest" :> ReqBody '[FormUrlEncoded] DataSeeding.DataSeedingForm :> Post '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "anomalies" :> Get '[HTML] (Html ())

type PublicAPI =
  "login" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "to_login" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "logout" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "auth_callback" :> QueryParam "code" Text :> QueryParam "state" Text :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] (Html ()))
    -- on the client metadata endpoint we will be passing the authorization token directly to the handler,
    -- and perfoming all the auth logic at the handler level. This is because the clients will only call this endpoint,
    -- so it doesnt need an exclusive robust authorization middleware solution.
    :<|> "api" :> "client_metadata" :> Header "Authorization" Text :> Get '[JSON] ClientMetadata.ClientMetadata
    :<|> "assets" :> Raw

type API =
  AuthProtect "apitoolkit_session" :> ProtectedAPI
    :<|> PublicAPI

type instance AuthServerData (AuthProtect "apitoolkit_session") = Sessions.PersistentSession

--
--
app :: LogAction IO String -> Pool Connection -> Config.AuthContext -> Application
app logger dbConn ctx = serveWithContext api (genAuthServerContext logger dbConn) $ hoistServerWithContext api ctxProxy (ctxToHandler ctx) server

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
    :<|> ManualIngestion.manualIngestGetH sess
    :<|> ManualIngestion.manualIngestPostH sess
    :<|> Log.apiLog sess
    :<|> DataSeeding.dataSeedingGetH sess
    :<|> DataSeeding.dataSeedingPostH sess
    :<|> AnomalyList.anomalyListGetH sess

publicServer :: ServerT PublicAPI DashboardM
publicServer =
  loginH
    :<|> loginRedirectH
    :<|> logoutH
    :<|> authCallbackH
    :<|> ClientMetadata.clientMetadataH
    :<|> serveDirectoryWebApp "./static/assets"

server :: ServerT API DashboardM
server = protectedServer :<|> publicServer
