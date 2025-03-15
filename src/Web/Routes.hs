module Web.Routes (server, genAuthServerContext) where

import Data.Aeson qualified as AE
import Data.Map qualified as Map
import Data.Pool (Pool)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
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
import Network.Wai (Request, queryString)
import Pages.Anomalies.Routes qualified as AnomaliesRoutes
import Pages.Anomalies.Server qualified as AnomaliesRoutes
import Pages.Api qualified as Api
import Pages.AutoComplete qualified as AutoComplete
import Pages.BodyWrapper (PageCtx (..))
import Pages.CP qualified as CP
import Pages.Charts.Charts qualified as Charts
import Pages.Dashboards qualified as Dashboards
import Pages.Endpoints.ApiCatalog qualified as ApiCatalog
import Pages.Endpoints.EndpointList qualified as EndpointList
import Pages.Fields.FieldDetails qualified as FieldDetails
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
import Pages.Telemetry.Routes qualified as TelemetryRoutes
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.Components.Widget qualified as Widget
import Pkg.RouteUtils
import Relude
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Htmx
import Servant.Server.Generic (AsServerT)
import Servant.Server.Internal.Delayed (passToServer)
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
  { dashboardRedirectGet :: mode :- "p" :> ProjectId :> AllQueryParams :> GetRedirect '[HTML] (Headers '[Header "Location" Text] NoContent)
  , endpointDetailsRedirect :: mode :- "p" :> ProjectId :> "endpoints" :> "details" :> AllQueryParams :> GetRedirect '[HTML] (Headers '[Header "Location" Text] NoContent)
  , dashboardsGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> QPT "file" :> QPT "from" :> QPT "to" :> QPT "since" :> AllQueryParams :> Get '[HTML] (RespHeaders (PageCtx Dashboards.DashboardGet))
  , dashboardsGetList :: mode :- "p" :> ProjectId :> "dashboards" :> Get '[HTML] (RespHeaders (PageCtx Dashboards.DashboardsGet))
  , dashboardsPost :: mode :- "p" :> ProjectId :> "dashboards" :> ReqBody '[FormUrlEncoded] Dashboards.DashboardForm :> Post '[HTML] (RespHeaders NoContent)
  , dashboardWidgetPut :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> QPT "widget_id" :> ReqBody '[JSON] Widget.Widget :> Put '[HTML] (RespHeaders (Widget.Widget))
  , dashboardWidgetReorderPatchH :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "widgets_order" :> ReqBody '[JSON] (Map Text Dashboards.WidgetReorderItem) :> Patch '[HTML] (RespHeaders NoContent)
  , dashboardDelete :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> Delete '[HTML] (RespHeaders NoContent)
  , dashboardRenamePatch :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "rename" :> ReqBody '[FormUrlEncoded] Dashboards.DashboardRenameForm :> Patch '[HTML] (RespHeaders (Html ()))
  , dashboardDuplicatePost :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "duplicate" :> Post '[HTML] (RespHeaders NoContent)
  , dashboardMoveWidget :: mode :- "p" :> ProjectId :> "dashboards" :> "move_widget" :> ReqBody '[JSON] Dashboards.WidgetMoveForm :> Post '[HTML] (RespHeaders NoContent)
  , dashboardDuplicateWidget :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "widgets" :> Capture "widget_id" Text :> "duplicate" :> Post '[HTML] (RespHeaders Widget.Widget)
  , projects :: mode :- ProjectsRoutes.Routes
  , anomalies :: mode :- "p" :> ProjectId :> "anomalies" :> AnomaliesRoutes.Routes
  , logExplorer :: mode :- "p" :> ProjectId :> LogExplorerRoutes.Routes
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
  , chartsDataGet :: mode :- "chart_data" :> QueryParam "data_type" Charts.DataType :> QueryParam "pid" Projects.ProjectId :> QPT "query_raw" :> QPT "queryAST" :> QPT "query_sql" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "source" :> AllQueryParams :> Get '[JSON] Charts.MetricsData
  , editField :: mode :- "p" :> ProjectId :> "fields" :> Capture "field_id" Fields.FieldId :> ReqBody '[FormUrlEncoded] FieldDetails.EditFieldForm :> Post '[HTML] (RespHeaders FieldDetails.FieldPut)
  , manageBillingGet :: mode :- "p" :> ProjectId :> "manage_billing" :> QPT "from" :> Get '[HTML] (RespHeaders LemonSqueezy.BillingGet)
  , endpointListGet :: mode :- "p" :> ProjectId :> "endpoints" :> QPT "page" :> QPT "layout" :> QPT "filter" :> QPT "host" :> QPT "request_type" :> QPT "sort" :> HXRequest :> HXBoosted :> HXCurrentURL :> QPT "load_more" :> QPT "search" :> Get '[HTML] (RespHeaders EndpointList.EndpointRequestStatsVM)
  , apiCatalogGet :: mode :- "p" :> ProjectId :> "api_catalog" :> QPT "sort" :> QPT "since" :> QPT "request_type" :> Get '[HTML] (RespHeaders (PageCtx (ItemsList.ItemsPage ApiCatalog.HostEventsVM)))
  , widgetPost :: mode :- "widget" :> ReqBody '[JSON] Widget.Widget :> Post '[HTML] (RespHeaders Widget.Widget)
  }
  deriving stock (Generic)


cookieProtectedServer :: Servant.ServerT (Servant.NamedRoutes CookieProtectedRoutes) ATAuthCtx
cookieProtectedServer =
  CookieProtectedRoutes
    { dashboardRedirectGet = Dashboards.entrypointRedirectGetH "_overview.yaml" "Overview" ["overview", "http", "logs", "traces", "events"]
    , endpointDetailsRedirect = Dashboards.entrypointRedirectGetH "endpoint-stats.yaml" "Endpoint Analytics" ["endpoints", "http", "events"]
    , dashboardsGet = Dashboards.dashboardGetH
    , dashboardsGetList = Dashboards.dashboardsGetH
    , dashboardsPost = Dashboards.dashboardsPostH
    , dashboardWidgetPut = Dashboards.dashboardWidgetPutH
    , dashboardWidgetReorderPatchH = Dashboards.dashboardWidgetReorderPatchH
    , dashboardDelete = Dashboards.dashboardDeleteH
    , dashboardRenamePatch = Dashboards.dashboardRenamePatchH
    , dashboardDuplicatePost = Dashboards.dashboardDuplicatePostH
    , dashboardMoveWidget = Dashboards.dashboardMoveWidgetPostH
    , dashboardDuplicateWidget = Dashboards.dashboardDuplicateWidgetPostH
    , projects = ProjectsRoutes.server
    , logExplorer = LogExplorerRoutes.server
    , anomalies = AnomaliesRoutes.server
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
    , chartsDataGet = Charts.queryMetrics
    , editField = FieldDetails.fieldPutH
    , manageBillingGet = LemonSqueezy.manageBillingGetH
    , endpointListGet = EndpointList.endpointListGetH
    , apiCatalogGet = ApiCatalog.apiCatalogH
    , widgetPost = Widget.widgetPostH
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


-- | Our custom combinator, indicating
--   "this endpoint wants all query parameters."
data AllQueryParams


-- | We add a HasServer instance that says:
--   - The handler will receive a list of (Text, Maybe Text).
--   - We pull that out of the WAI `Request` in `route`.
instance HasServer api ctx => HasServer (AllQueryParams :> api) ctx where
  type ServerT (AllQueryParams :> api) m = [(Text, Maybe Text)] -> ServerT api m


  route _ ctx subserver = route (Proxy :: Proxy api) ctx subserver'
    where
      grabAllParams :: Request -> [(Text, Maybe Text)]
      grabAllParams req =
        [ ( TE.decodeUtf8With TE.lenientDecode k
          , (TE.decodeUtf8With TE.lenientDecode) <$> mv
          )
        | (k, mv) <- req.queryString
        ]
      subserver' = passToServer subserver grabAllParams


  hoistServerWithContext
    :: Proxy (AllQueryParams :> api)
    -> Proxy ctx
    -> (forall x. m x -> n x)
    -> ([(Text, Maybe Text)] -> ServerT api m)
    -> ([(Text, Maybe Text)] -> ServerT api n)
  hoistServerWithContext _ pc nat s = \qs -> hoistServerWithContext (Proxy :: Proxy api) pc nat (s qs)
