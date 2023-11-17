{-# LANGUAGE UndecidableInstances #-}

module Server (app) where

import Colog (LogAction)
import Config (DashboardM, ctxToHandler, pool)
import Config qualified
import Data.Aeson (FromJSON)
import Data.Aeson qualified as AE
import Data.Aeson.Types (ToJSON)
import Data.Pool (Pool)
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import DataSeeding qualified
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), queryOne, withPool)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Deriving.Aeson qualified as DAE
import GitHash
import Lucid
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Types qualified as Fields (FieldId)
import Models.Apis.Reports qualified as Reports
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Network.Wai (Request)
import Pages.Anomalies.AnomalyList (AnomalyBulkForm)
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.Api qualified as Api
import Pages.AutoComplete qualified as AutoComplete
import Pages.Charts.Charts qualified as Charts
import Pages.Dashboard qualified as Dashboard
import Pages.Documentation (SaveSwaggerForm, SwaggerForm)
import Pages.Documentation qualified as Documentation
import Pages.Endpoints.EndpointDetails qualified as EndpointDetails
import Pages.Endpoints.EndpointList qualified as EndpointList
import Pages.GenerateSwagger qualified as GenerateSwagger
import Pages.Log qualified as Log
import Pages.ManualIngestion (RequestMessageForm)
import Pages.ManualIngestion qualified as ManualIngestion
import Pages.Onboarding qualified as Onboarding
import Pages.Outgoing
import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.ListProjects qualified as ListProjects
import Pages.Projects.ManageMembers (ManageMembersForm)
import Pages.Projects.ManageMembers qualified as ManageMembers
import Pages.RedactedFields (RedactFieldForm)
import Pages.RedactedFields qualified as RedactedFields
import Pages.Reports qualified as Reports
import Pages.Share qualified as Share
import Pages.SlackInstall qualified as SlackInstall
import Pages.Survey qualified as Survey
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


type QP a b = QueryParam a b


type QPT a = QueryParam a Text


type QPB a = QueryParam a Bool


type QPI a = QueryParam a Int


type QEID a = QueryParam a Endpoints.EndpointId


type ProjectId = Capture "projectID" Projects.ProjectId


--
-- API Section
type ProtectedAPI =
  UVerb 'GET '[HTML] GetOrRedirect
    :<|> "p" :> "new" :> Get '[HTML] (Html ()) -- p represents project
    :<|> "p" :> "new" :> ReqBody '[FormUrlEncoded] CreateProject.CreateProjectForm :> Post '[HTML] (Headers '[HXTrigger, HXRedirect] (Html ()))
    :<|> "p" :> ProjectId :> "onboarding" :> QPB "polling" :> QPB "redirected" :> QPT "current_tab" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> QPT "from" :> QPT "to" :> QPT "since" :> UVerb 'GET '[HTML] GetOrRedirect
    :<|> "p" :> ProjectId :> "settings" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "delete" :> Get '[HTML] (Headers '[HXTrigger, HXRedirect] (Html ()))
    :<|> "p" :> ProjectId :> "manage_members" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "manage_members" :> ReqBody '[FormUrlEncoded] ManageMembersForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
    :<|> "p" :> ProjectId :> "endpoints" :> QPT "layout" :> QPT "ackd" :> QPT "archived" :> QPT "host" :> QPT "project_host" :> QPT "sort" :> HXRequest :> HXBoosted :> HXCurrentURL :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "endpoints" :> Capture "endpoints_id" Endpoints.EndpointId :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "subpage" :> QPT "shape" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "apis" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "apis" :> ReqBody '[FormUrlEncoded] Api.GenerateAPIKeyForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
    :<|> "p" :> ProjectId :> "apis" :> Capture "keyID" ProjectApiKeys.ProjectApiKeyId :> Delete '[HTML] (Headers '[HXTrigger] (Html ()))
    :<|> "p" :> ProjectId :> "fields" :> Capture "field_id" Fields.FieldId :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "manual_ingest" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "manual_ingest" :> ReqBody '[FormUrlEncoded] RequestMessageForm :> Post '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "log_explorer" :> QPT "query" :> QPT "cols" :> QPT "cursor" :> QPT "since" :> QPT "from" :> QPT "to" :> HXRequest :> HXBoosted :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" ZonedTime :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" ZonedTime :> "detailed" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "log_explorer" :> "endpoint" :> Capture "endpoint_hash" Text :> Get '[HTML] (Headers '[HXRedirect] (Html ()))
    :<|> "p" :> ProjectId :> "bulk_seed_and_ingest" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "bulk_seed_and_ingest" :> ReqBody '[FormUrlEncoded] DataSeeding.DataSeedingForm :> Post '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "anomalies" :> QPT "layout" :> QPT "ackd" :> QPT "archived" :> QPT "sort" :> QPT "page" :> QPT "load_more" :> QEID "endpoint" :> HXRequest :> HXBoosted :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "anomalies" :> "bulk_actions" :> Capture "action" Text :> ReqBody '[FormUrlEncoded] AnomalyBulkForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
    :<|> "p" :> ProjectId :> "anomalies" :> Capture "anomalyID" Anomalies.AnomalyId :> "acknowlege" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "anomalies" :> Capture "anomalyID" Anomalies.AnomalyId :> "unacknowlege" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "anomalies" :> Capture "anomalyID" Anomalies.AnomalyId :> "archive" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "anomalies" :> Capture "anomalyID" Anomalies.AnomalyId :> "unarchive" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "anomaly" :> Capture "targetHash" Text :> QPT "modal" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "anomaly" :> "events" :> Capture "targetHash" Text :> Capture "anomlayType" Text :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "redacted_fields" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "redacted_fields" :> ReqBody '[FormUrlEncoded] RedactFieldForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
    :<|> "p" :> ProjectId :> "charts_html" :> "throughput" :> QPT "id" :> QPT "group_by" :> QPT "endpoint_hash" :> QPT "shape_hash" :> QPT "format_hash" :> QPT "status_code_gt" :> QPI "num_slots" :> QPI "limit" :> QPB "show_legend" :> QPT "from" :> QPT "to" :> QPT "theme" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "documentation" :> QPT "swagger_id" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "documentation" :> ReqBody '[FormUrlEncoded] SwaggerForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
    :<|> "p" :> ProjectId :> "documentation" :> "save" :> ReqBody '[JSON] SaveSwaggerForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
    :<|> "p" :> ProjectId :> "generate_swagger" :> Get '[JSON] AE.Value
    :<|> "p" :> ProjectId :> "reports" :> QPT "page" :> HXRequest :> HXBoosted :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "reports" :> Capture "report_id" Reports.ReportId :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "survey" :> ReqBody '[FormUrlEncoded] Survey.SurveyForm :> Post '[HTML] (Headers '[HXTrigger, HXRedirect] (Html ()))
    :<|> "p" :> ProjectId :> "reports_notif" :> Capture "report_type" Text :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
    :<|> "charts_html" :> QP "chart_type" Charts.ChartType :> QP "group_by" Charts.GroupBy :> QP "query_by" [Charts.QueryBy] :> QP "num_slots" Int :> QP "limit" Int :> QP "theme" Text :> QPT "id" :> QP "show_legend" Bool :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "about_project" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "share" :> ReqBody '[FormUrlEncoded] Share.ReqForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
    :<|> "p" :> ProjectId :> "outgoing" :> Get '[HTML] (Html ())
    :<|> "p" :> ProjectId :> "query_builder" :> "autocomplete" :> QPT "category" :> QPT "prefix" :> Get '[JSON] AE.Value


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
    :<|> "ping" :> Get '[PlainText] Text
    :<|> "share" :> "r" :> Capture "shareID" UUID.UUID :> Get '[HTML] (Html ())
    :<|> "slack" :> "oauth" :> "callback" :> QPT "code" :> Get '[HTML] (Html ())
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
    :<|> Onboarding.onboardingGetH sess
    :<|> Dashboard.dashboardGetH sess
    :<|> CreateProject.projectSettingsGetH sess
    :<|> CreateProject.deleteProjectGetH sess
    :<|> ManageMembers.manageMembersGetH sess
    :<|> ManageMembers.manageMembersPostH sess
    :<|> EndpointList.endpointListGetH sess
    :<|> EndpointDetails.endpointDetailsH sess
    :<|> Api.apiGetH sess
    :<|> Api.apiPostH sess
    :<|> Api.apiDeleteH sess
    :<|> EndpointDetails.fieldDetailsPartialH sess
    :<|> ManualIngestion.manualIngestGetH sess
    :<|> ManualIngestion.manualIngestPostH sess
    :<|> Log.apiLog sess
    :<|> Log.apiLogItem sess
    :<|> Log.expandAPIlogItem sess
    :<|> EndpointDetails.endpointDetailsWithHashH sess
    :<|> DataSeeding.dataSeedingGetH sess
    :<|> DataSeeding.dataSeedingPostH sess
    :<|> AnomalyList.anomalyListGetH sess
    :<|> AnomalyList.anomalyBulkActionsPostH sess
    :<|> AnomalyList.acknowlegeAnomalyGetH sess
    :<|> AnomalyList.unAcknowlegeAnomalyGetH sess
    :<|> AnomalyList.archiveAnomalyGetH sess
    :<|> AnomalyList.unArchiveAnomalyGetH sess
    :<|> AnomalyList.anomalyDetailsGetH sess
    :<|> AnomalyList.anomalyEvents sess
    :<|> RedactedFields.redactedFieldsGetH sess
    :<|> RedactedFields.redactedFieldsPostH sess
    :<|> Charts.throughputEndpointHTML sess
    :<|> Documentation.documentationGetH sess
    :<|> Documentation.documentationPostH sess
    :<|> Documentation.documentationPutH sess
    :<|> GenerateSwagger.generateGetH sess
    :<|> Reports.reportsGetH sess
    :<|> Reports.singleReportGetH sess
    :<|> Survey.surveyPutH sess
    :<|> Reports.reportsPostH sess
    :<|> Charts.chartsGetH sess
    :<|> Survey.surveyGetH sess
    :<|> Share.shareLinkPostH sess
    :<|> outgoingGetH sess
    :<|> AutoComplete.getH sess


publicServer :: ServerT PublicAPI DashboardM
publicServer =
  loginH
    :<|> loginRedirectH
    :<|> logoutH
    :<|> authCallbackH
    :<|> ClientMetadata.clientMetadataH
    :<|> statusH
    :<|> pingH
    :<|> Share.shareLinkGetH
    :<|> SlackInstall.getH
    :<|> serveDirectoryWebApp "./static/public"


server :: ServerT API DashboardM
server = protectedServer :<|> publicServer


data Status = Status
  { ping :: Text
  , dbVersion :: Maybe Text
  , gitHash :: Text
  , gitCommitDate :: Text
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Status


statusH :: DashboardM Status
statusH = do
  pool <- asks pool
  let query = [sql| select version(); |]
  version <- liftIO $ withPool pool $ queryOne Select query ()
  let gi = $$tGitInfoCwd
  pure
    $ Status
      { ping = "pong"
      , dbVersion = version
      , gitHash = toText $ giHash gi
      , gitCommitDate = toText $ giCommitDate gi
      }


pingH :: DashboardM Text
pingH = do
  pure "pong"
