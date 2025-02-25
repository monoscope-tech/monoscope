module Web.Routes (server, genAuthServerContext) where

import Data.Aeson qualified as AE
import Data.Map qualified as Map
import Data.Pool (Pool)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Deriving.Aeson qualified as DAE
import Effectful (runPureEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.PostgreSQL.Transact (queryOne_)
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local qualified as State
import GitHash (giCommitDate, giHash, tGitInfoCwd)
import Log (Logger, UTCTime)
import Lucid (Html)
import Models.Apis.Fields.Types qualified as Fields (FieldId)
import Models.Apis.Reports qualified as ReportsM
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (notFound404)
import Pages.Anomalies.Routes qualified as AnomaliesRoutes
import Pages.Anomalies.Server qualified as AnomaliesRoutes
import Pages.Api qualified as Api
import Pages.AutoComplete qualified as AutoComplete
import Pages.BodyWrapper (PageCtx (..))
import Pages.CP qualified as CP
import Pages.Charts.Charts qualified as Charts
import Pages.Dashboards qualified as Dashboards
import Pages.Endpoints.Routes qualified as EndpointsRoutes
import Pages.Endpoints.Server qualified as EndpointsRoutes
import Pages.Fields.FieldDetails qualified as FieldDetails
import Pages.IntegrationGuides qualified as IntegrationGuides
import Pages.LemonSqueezy qualified as LemonSqueezy
import Pages.LogExplorer.Routes qualified as LogExplorerRoutes
import Pages.Monitors.Routes qualified as MonitorsRoutes
import Pages.Projects.Routes qualified as ProjectsRoutes
import Pages.Projects.Server qualified as ProjectsRoutes
import Pages.Reports qualified as Reports
import Pages.Share qualified as Share
import Pages.SlackInstall qualified as SlackInstall
import Pages.Specification.GenerateSwagger qualified as GenerateSwagger
import Pages.Specification.Routes qualified as SpecificationRoutes
import Pages.Specification.Server qualified as SpecificationRoutes
import Pages.Survey qualified as Survey
import Pages.Telemetry.Routes qualified as TelemetryRoutes
import Pkg.RouteUtils
import Relude
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Htmx
import Servant.Server.Generic (AsServerT)
import System.Config (AuthContext)
import System.Types
import Web.Auth (APItoolkitAuthContext, authHandler)
import Web.Auth qualified as Auth
import Web.ClientMetadata qualified as ClientMetadata
import Web.Cookie (SetCookie)
import Web.Error


type role Routes nominal


type Routes :: Type -> Type
data Routes mode = Routes
  { public :: mode :- "public" :> Servant.Raw
  , cookieProtected :: mode :- AuthProtect "optional-cookie-auth" :> Servant.NamedRoutes CookieProtectedRoutes
  , ping :: mode :- "ping" :> Get '[PlainText] Text
  , cp :: mode :- "cp" :> Get '[HTML] (Html ())
  , status :: mode :- "status" :> Get '[JSON] Status
  , login :: mode :- "login" :> QPT "redirect_to" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
  , toLogin :: mode :- "to_login" :> QPT "redirect_to" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
  , logout :: mode :- "logout" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
  , authCallback :: mode :- "auth_callback" :> QPT "code" :> QPT "state" :> QPT "redirect_to" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] (Html ()))
  , shareLinkGet :: mode :- "share" :> "r" :> Capture "shareID" UUID.UUID :> Get '[HTML] Share.ShareLinkGet
  , slackLinkProjectGet :: mode :- "slack" :> "oauth" :> "callback" :> Capture "project_id" Projects.ProjectId :> QPT "code" :> QPT "onboarding" :> GetRedirect '[HTML] (Headers '[Header "Location" Text] SlackInstall.SlackLink)
  , clientMetadata :: mode :- "api" :> "client_metadata" :> Header "Authorization" Text :> Get '[JSON] ClientMetadata.ClientMetadata
  , lemonWebhook :: mode :- "webhook" :> "lemon-squeezy" :> Header "X-Signature" Text :> ReqBody '[JSON] LemonSqueezy.WebhookData :> Post '[HTML] (Html ())
  }
  deriving stock (Generic)


server
  :: Pool PG.Connection
  -> Routes (AsServerT ATBaseCtx)
server pool =
  Routes
    { public = Servant.serveDirectoryWebApp "./static/public"
    , ping = pingH
    , status = statusH
    , cp = CP.pageH
    , login = Auth.loginH
    , toLogin = Auth.loginRedirectH
    , logout = Auth.logoutH
    , authCallback = Auth.authCallbackH
    , shareLinkGet = Share.shareLinkGetH
    , slackLinkProjectGet = SlackInstall.linkProjectGetH
    , clientMetadata = ClientMetadata.clientMetadataH
    , lemonWebhook = LemonSqueezy.webhookPostH
    , cookieProtected = \sessionWithCookies ->
        Servant.hoistServerWithContext
          (Proxy @(Servant.NamedRoutes CookieProtectedRoutes))
          (Proxy @'[APItoolkitAuthContext])
          ( \page ->
              page
                & State.evalState Map.empty
                & State.evalState Nothing
                & Effectful.Reader.Static.runReader sessionWithCookies
          )
          cookieProtectedServer
    }


type role CookieProtectedRoutes nominal


type CookieProtectedRoutes :: Type -> Type
data CookieProtectedRoutes mode = CookieProtectedRoutes
  { dashboardGet :: mode :- "p" :> ProjectId :> GetRedirect '[HTML] (Headers '[Header "Location" Text] NoContent)
  , dashboardsGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> QPT "file" :> QPT "from" :> QPT "to" :> QPT "since" :> Get '[HTML] (RespHeaders (PageCtx Dashboards.DashboardGet))
  , dashboardsGetList :: mode :- "p" :> ProjectId :> "dashboards" :> Get '[HTML] (RespHeaders (PageCtx Dashboards.DashboardsGet))
  , dashboardsPost :: mode :- "p" :> ProjectId :> "dashboards" :> ReqBody '[FormUrlEncoded] Dashboards.DashboardForm :> Post '[HTML] (RespHeaders NoContent)
  , projects :: mode :- ProjectsRoutes.Routes
  , anomalies :: mode :- "p" :> ProjectId :> "anomalies" :> AnomaliesRoutes.Routes
  , logExplorer :: mode :- "p" :> ProjectId :> LogExplorerRoutes.Routes
  , endpoints :: mode :- "p" :> ProjectId :> EndpointsRoutes.Routes
  , monitors :: mode :- "p" :> ProjectId :> MonitorsRoutes.Routes
  , specification :: mode :- "p" :> ProjectId :> SpecificationRoutes.Routes
  , traces :: mode :- "p" :> ProjectId :> TelemetryRoutes.Routes
  , apiGet :: mode :- "p" :> ProjectId :> "apis" :> Get '[HTML] (RespHeaders Api.ApiGet)
  , apiDelete :: mode :- "p" :> ProjectId :> "apis" :> Capture "keyID" ProjectApiKeys.ProjectApiKeyId :> Delete '[HTML] (RespHeaders Api.ApiMut)
  , apiPost :: mode :- "p" :> ProjectId :> "apis" :> ReqBody '[FormUrlEncoded] Api.GenerateAPIKeyForm :> Post '[HTML] (RespHeaders Api.ApiMut)
  , slackInstallPost :: mode :- "slack" :> "link-projects" :> ReqBody '[FormUrlEncoded] SlackInstall.LinkProjectsForm :> Post '[HTML] (RespHeaders (Html ()))
  , slackUpdateWebhook :: mode :- "p" :> ProjectId :> "slack" :> "webhook" :> ReqBody '[FormUrlEncoded] SlackInstall.LinkProjectsForm :> Post '[HTML] (RespHeaders (Html ()))
  , reportsGet :: mode :- "p" :> ProjectId :> "reports" :> QPT "page" :> HXRequest :> HXBoosted :> Get '[HTML] (RespHeaders Reports.ReportsGet)
  , reportsSingleGet :: mode :- "p" :> ProjectId :> "reports" :> Capture "report_id" ReportsM.ReportId :> Get '[HTML] (RespHeaders Reports.ReportsGet)
  , reportsPost :: mode :- "p" :> ProjectId :> "reports_notif" :> Capture "report_type" Text :> Post '[HTML] (RespHeaders Reports.ReportsPost)
  , shareLinkPost :: mode :- "p" :> ProjectId :> "share" :> Capture "event_id" UUID.UUID :> Capture "createdAt" UTCTime :> QPT "event_type" :> Post '[HTML] (RespHeaders Share.ShareLinkPost)
  , queryBuilderAutocomplete :: mode :- "p" :> ProjectId :> "query_builder" :> "autocomplete" :> QPT "category" :> QPT "prefix" :> Get '[JSON] (RespHeaders AE.Value)
  , swaggerGenerateGet :: mode :- "p" :> ProjectId :> "generate_swagger" :> Get '[JSON] (RespHeaders AE.Value)
  , chartsGet :: mode :- "charts_html" :> QP "chart_type" Charts.ChartType :> QPT "query_raw" :> QPT "queryAST" :> QueryParam "pid" Projects.ProjectId :> QP "group_by" Charts.GroupBy :> QP "query_by" [Charts.QueryBy] :> QP "num_slots" Int :> QP "limit" Int :> QP "theme" Text :> QPT "id" :> QP "show_legend" Bool :> QP "show_axes" Bool :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "source" :> Get '[HTML] (RespHeaders (Html ()))
  , chartsDataGet :: mode :- "chart_data" :> QueryParam "pid" Projects.ProjectId :> QPT "query_raw" :> QPT "queryAST" :> QPT "query_sql" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "source" :> Get '[JSON] Charts.MetricsData
  , surveyPut :: mode :- "p" :> ProjectId :> "survey" :> ReqBody '[FormUrlEncoded] Survey.SurveyForm :> Post '[HTML] (RespHeaders Survey.SurveyPut)
  , surveyGet :: mode :- "p" :> ProjectId :> "about_project" :> Get '[HTML] (RespHeaders Survey.SurveyGet)
  , editField :: mode :- "p" :> ProjectId :> "fields" :> Capture "field_id" Fields.FieldId :> ReqBody '[FormUrlEncoded] FieldDetails.EditFieldForm :> Post '[HTML] (RespHeaders FieldDetails.FieldPut)
  , integrationGuides :: mode :- "p" :> ProjectId :> "integration_guides" :> QPT "sdk" :> QPT "error_reporting" :> QPT "dependency_monitoring" :> Get '[HTML] (RespHeaders IntegrationGuides.IntegrationsGet)
  , manageBillingGet :: mode :- "p" :> ProjectId :> "manage_billing" :> QPT "from" :> Get '[HTML] (RespHeaders LemonSqueezy.BillingGet)
  }
  deriving stock (Generic)


cookieProtectedServer :: Servant.ServerT (Servant.NamedRoutes CookieProtectedRoutes) ATAuthCtx
cookieProtectedServer =
  CookieProtectedRoutes
    { dashboardGet = Dashboards.entrypointGetH
    , dashboardsGet = Dashboards.dashboardGetH
    , dashboardsGetList = Dashboards.dashboardsGetH
    , dashboardsPost = Dashboards.dashboardsPostH
    , projects = ProjectsRoutes.server
    , logExplorer = LogExplorerRoutes.server
    , anomalies = AnomaliesRoutes.server
    , endpoints = EndpointsRoutes.server
    , monitors = MonitorsRoutes.server
    , specification = SpecificationRoutes.server
    , traces = TelemetryRoutes.server
    , apiGet = Api.apiGetH
    , apiDelete = Api.apiDeleteH
    , apiPost = Api.apiPostH
    , slackInstallPost = SlackInstall.postH
    , slackUpdateWebhook = SlackInstall.updateWebHook
    , reportsGet = Reports.reportsGetH
    , reportsSingleGet = Reports.singleReportGetH
    , reportsPost = Reports.reportsPostH
    , shareLinkPost = Share.shareLinkPostH
    , queryBuilderAutocomplete = AutoComplete.getH
    , swaggerGenerateGet = GenerateSwagger.generateGetH
    , chartsGet = Charts.chartsGetH
    , chartsDataGet = Charts.queryMetrics
    , surveyPut = Survey.surveyPutH
    , surveyGet = Survey.surveyGetH
    , editField = FieldDetails.fieldPutH
    , integrationGuides = IntegrationGuides.getH
    , manageBillingGet = LemonSqueezy.manageBillingGetH
    }


-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: Logger -> AuthContext -> Servant.Context '[APItoolkitAuthContext, Servant.ErrorFormatters]
genAuthServerContext logger env = authHandler logger env :. errorFormatters env :. EmptyContext


errorFormatters :: AuthContext -> Servant.ErrorFormatters
errorFormatters env =
  Servant.defaultErrorFormatters{Servant.notFoundErrorFormatter = notFoundPage env}


notFoundPage :: AuthContext -> Servant.NotFoundErrorFormatter
notFoundPage env _req =
  let result = runPureEff $ runErrorNoCallStack $ renderError env notFound404
   in case result of
        Left err -> err
        Right _ -> Servant.err404


data Status = Status
  { dbVersion :: Maybe Text
  , gitHash :: Text
  , gitCommitDate :: Text
  }
  deriving stock (Generic)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Status


statusH :: ATBaseCtx Status
statusH = do
  let query = [sql| select version(); |]
  versionM <- queryOne_ query
  let version = versionM <&> \(PG.Only v) -> v
  let gi = $$tGitInfoCwd
  pure
    Status
      { dbVersion = version
      , gitHash = toText $ giHash gi
      , gitCommitDate = toText $ giCommitDate gi
      }


pingH :: ATBaseCtx Text
pingH = do
  pure "pong"


-- When bystring is returned for json, simply return the bytestring
instance Servant.MimeRender JSON ByteString where
  mimeRender _ = fromStrict
