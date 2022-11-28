{-# LANGUAGE UndecidableInstances #-}

module Server (app) where

import Colog (LogAction)
import Config (DashboardM, ctxToHandler, pool)
import Config qualified
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Pool (Pool)
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import DataSeeding qualified
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), queryOne, withPool)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Deriving.Aeson qualified as DAE
import Lucid
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Types qualified as Fields (FieldId)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Network.Wai (Request)
import Pages.Anomalies.AnomalyList (AnomalyBulkForm)
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.Api qualified as Api
import Pages.Charts.Charts qualified as Charts
import Pages.Dashboard qualified as Dashboard
import Pages.Endpoints.EndpointDetails qualified as EndpointDetails
import Pages.Endpoints.EndpointList qualified as EndpointList
import Pages.Log qualified as Log
import Pages.ManualIngestion (RequestMessageForm)
import Pages.ManualIngestion qualified as ManualIngestion
import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.ListProjects qualified as ListProjects
import Pages.Projects.ManageMembers (ManageMembersForm)
import Pages.Projects.ManageMembers qualified as ManageMembers
import Pages.RedactedFields (RedactFieldForm)
import Pages.RedactedFields qualified as RedactedFields
import Relude
import Servant
import Servant.HTML.Lucid
import Servant.Htmx
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData)
import Utils
import Web.Auth (authCallbackH, genAuthServerContext, loginH, loginRedirectH, logoutH)
import Web.ClientMetadata qualified as ClientMetadata
import Web.Cookie (SetCookie)
import Witch (from)

type GetRedirect = Verb 'GET 302

-- When bystring is returned for json, simply return the bytestring
instance MimeRender JSON ByteString where
  mimeRender _ = from @ByteString

type QPT a = QueryParam a Text

type QPB a = QueryParam a Bool

type QPI a = QueryParam a Int

type ProjectId = Capture "projectID" Projects.ProjectId

--
-- API Section
type ProtectedAPI =
  UVerb 'GET '[HTML] GetOrRedirect
    :<|> "p" :> "new" :> Get '[HTML] (Html ()) -- p represents project
    :<|> "p" :> "new" :> ReqBody '[FormUrlEncoded] CreateProject.CreateProjectForm :> Post '[HTML] (Headers '[HXTrigger, HXRedirect] (Html ()))
    :<|> "p" :> ProjectId :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "manage_members" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "manage_members" :> ReqBody '[FormUrlEncoded] ManageMembersForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
    :<|> "p" :> ProjectId :> "endpoints" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "endpoints" :> Capture "endpoints_id" Endpoints.EndpointId :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "apis" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "apis" :> ReqBody '[FormUrlEncoded] Api.GenerateAPIKeyForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
    :<|> "p" :> ProjectId :> "fields" :> Capture "field_id" Fields.FieldId :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "manual_ingest" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "manual_ingest" :> ReqBody '[FormUrlEncoded] RequestMessageForm :> Post '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "log_explorer" :> QPT "query" :> QPT "cols" :> QPT "from" :> HXRequest :> HXBoosted :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" ZonedTime :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "bulk_seed_and_ingest" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "bulk_seed_and_ingest" :> ReqBody '[FormUrlEncoded] DataSeeding.DataSeedingForm :> Post '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "anomalies" :> QPT "layout" :> QPT "ackd" :> QPT "archived" :> QPT "sort" :> HXRequest :> HXBoosted :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "anomalies" :> "bulk_actions" :> Capture "action" Text :> ReqBody '[FormUrlEncoded] AnomalyBulkForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
    :<|> "p" :> ProjectId :> "anomalies" :> Capture "anomalyID" Anomalies.AnomalyId :> "acknowlege" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "anomalies" :> Capture "anomalyID" Anomalies.AnomalyId :> "unacknowlege" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "anomalies" :> Capture "anomalyID" Anomalies.AnomalyId :> "archive" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "anomalies" :> Capture "anomalyID" Anomalies.AnomalyId :> "unarchive" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "redacted_fields" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "redacted_fields" :> ReqBody '[FormUrlEncoded] RedactFieldForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
    :<|> "p" :> ProjectId :> "charts_html" :> "throughput" :> QPT "id" :> QPT "group_by" :> QPT "endpoint_hash" :> QPT "shape_hash" :> QPT "format_hash" :> QPI "interval" :> QPI "limit" :> QPB "show_legend" :> Get '[HTML] (Html ())

type PublicAPI =
  "login" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "to_login" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "logout" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "auth_callback" :> QPT "code" :> QPT "state" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] (Html ()))
    -- on the client metadata endpoint we will be passing the authorization token directly to the handler,
    -- and perfoming all the auth logic at the handler level. This is because the clients will only call this endpoint,
    -- so it doesnt need an exclusive robust authorization middleware solution.
    :<|> "api" :> "client_metadata" :> Header "Authorization" Text :> Get '[JSON] ClientMetadata.ClientMetadata
    :<|> "status" :> Get '[JSON] Status
    :<|> Raw

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
    :<|> ManageMembers.manageMembersGetH sess
    :<|> ManageMembers.manageMembersPostH sess
    :<|> EndpointList.endpointListH sess
    :<|> EndpointDetails.endpointDetailsH sess
    :<|> Api.apiGetH sess
    :<|> Api.apiPostH sess
    :<|> EndpointDetails.fieldDetailsPartialH sess
    :<|> ManualIngestion.manualIngestGetH sess
    :<|> ManualIngestion.manualIngestPostH sess
    :<|> Log.apiLog sess
    :<|> Log.apiLogItem sess
    :<|> DataSeeding.dataSeedingGetH sess
    :<|> DataSeeding.dataSeedingPostH sess
    :<|> AnomalyList.anomalyListGetH sess
    :<|> AnomalyList.anomalyBulkActionsPostH sess
    :<|> AnomalyList.acknowlegeAnomalyGetH sess
    :<|> AnomalyList.unAcknowlegeAnomalyGetH sess
    :<|> AnomalyList.archiveAnomalyGetH sess
    :<|> AnomalyList.unArchiveAnomalyGetH sess
    :<|> RedactedFields.redactedFieldsGetH sess
    :<|> RedactedFields.redactedFieldsPostH sess
    :<|> Charts.throughputEndpointHTML sess

publicServer :: ServerT PublicAPI DashboardM
publicServer =
  loginH
    :<|> loginRedirectH
    :<|> logoutH
    :<|> authCallbackH
    :<|> ClientMetadata.clientMetadataH
    :<|> statusH
    :<|> serveDirectoryWebApp "./static/public"

server :: ServerT API DashboardM
server = protectedServer :<|> publicServer

data Status = Status
  { ping :: Text,
    dbVersion :: Maybe Text
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Status

statusH :: DashboardM (Status)
statusH = do
  pool <- asks pool
  let query = [sql| select version(); |]
  version <- liftIO $ withPool pool $ queryOne Select query ()
  pure $
    Status
      { ping = "pong",
        dbVersion = version
      }
