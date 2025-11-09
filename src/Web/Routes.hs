module Web.Routes (server, genAuthServerContext) where

-- Standard library imports
import Control.Lens
import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Pool (Pool)
import Data.UUID qualified as UUID
import Relude

-- Database imports
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL.Transact (queryOne_)

-- Effectful imports
import Effectful (runPureEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local qualified as State
import Effectful.Time qualified as Time

-- Web and server imports
import GitHash (giCommitDate, giHash, tGitInfoCwd)
import Log (Logger, UTCTime)
import Lucid (Html)
import Network.HTTP.Types (notFound404)
import Network.Wai (Request, queryString)
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Htmx
import Servant.Server.Generic (AsServerT)
import Servant.Server.Internal.Delayed (passToServer)
import Web.Cookie (SetCookie)

-- System and configuration imports
import Deriving.Aeson qualified as DAE
import System.Config (AuthContext)
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addRespHeaders)
import Web.Auth (APItoolkitAuthContext, authHandler)
import Web.Auth qualified as Auth
import Web.ClientMetadata qualified as ClientMetadata
import Web.Error

-- Model imports
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.Reports qualified as ReportsM
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema

-- Page imports
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.Api qualified as Api
import Pages.BodyWrapper (PageCtx (..))
import Pages.Bots.Discord qualified as Discord
import Pages.Bots.Slack qualified as Slack
import Pages.Bots.Utils qualified as BotUtils
import Pages.Bots.Whatsapp qualified as Whatsapp
import Pages.Charts.Charts qualified as Charts
import Pages.Dashboards qualified as Dashboards
import Pages.Endpoints.ApiCatalog qualified as ApiCatalog
import Pages.LemonSqueezy qualified as LemonSqueezy
import Pages.LogExplorer.Log qualified as Log
import Pages.LogExplorer.LogItem qualified as LogItem
import Pages.Monitors.Alerts qualified as Alerts
import Pages.Monitors.Testing qualified as Testing
import Pages.Onboarding.Onboarding qualified as Onboarding
import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.Integrations qualified as Integrations
import Pages.Projects.ListProjects qualified as ListProjects
import Pages.Projects.ManageMembers qualified as ManageMembers
import Pages.Replay qualified as Replay
import Pages.Reports qualified as Reports
import Pages.S3 qualified as S3
import Pages.Share qualified as Share
import Pages.Telemetry.Metrics qualified as Metrics
import Pages.Telemetry.Trace qualified as Trace
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.Components.Widget qualified as Widget


-- =============================================================================
-- Common query parameter type aliases
-- =============================================================================

type ProjectId = Capture "projectID" Projects.ProjectId
type GetRedirect = Verb 'GET 302


-- Query parameter types
type QPT a = QueryParam a Text
type QPU a = QueryParam a UTCTime
type QP a b = QueryParam a b
type QPB a = QueryParam a Bool
type QPI a = QueryParam a Int
type QEID a = QueryParam a Endpoints.EndpointId


-- Custom type for handling all query parameters
data AllQueryParams


-- =============================================================================
-- Custom content types
-- =============================================================================

-- Define custom RawJSON content type
data RawJSON


instance Accept RawJSON where
  contentType _ = "application/json"


instance MimeUnrender RawJSON BS.ByteString where
  mimeUnrender _ = Right . toStrict


-- When bytestring is returned for json, simply return the bytestring
instance Servant.MimeRender JSON ByteString where
  mimeRender _ = fromStrict


-- =============================================================================
-- Route Definitions
-- =============================================================================

-- Root routes
type role Routes nominal


type Routes :: Type -> Type
data Routes mode = Routes
  { public :: mode :- "public" :> Servant.Raw
  , cookieProtected :: mode :- AuthProtect "optional-cookie-auth" :> Servant.NamedRoutes CookieProtectedRoutes
  , ping :: mode :- "ping" :> Get '[PlainText] Text
  , status :: mode :- "status" :> Get '[JSON] Status
  , login :: mode :- "login" :> QPT "redirect_to" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
  , toLogin :: mode :- "to_login" :> QPT "redirect_to" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
  , logout :: mode :- "logout" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
  , authCallback :: mode :- "auth_callback" :> QPT "code" :> QPT "state" :> QPT "redirect_to" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] (Html ()))
  , shareLinkGet :: mode :- "share" :> "r" :> Capture "shareID" UUID.UUID :> Get '[HTML] Share.ShareLinkGet
  , slackLinkProjectGet :: mode :- "slack" :> "oauth" :> "callback" :> Capture "project_id" Projects.ProjectId :> QPT "code" :> QPT "onboarding" :> GetRedirect '[HTML] (Headers '[Header "Location" Text] BotUtils.BotResponse)
  , discordLinkProjectGet :: mode :- "discord" :> "oauth" :> "callback" :> QPT "state" :> QPT "code" :> QPT "guild_id" :> GetRedirect '[HTML] (Headers '[Header "Location" Text] BotUtils.BotResponse)
  , discordInteractions :: mode :- "discord" :> "interactions" :> ReqBody '[RawJSON] BS.ByteString :> Header "X-Signature-Ed25519" BS.ByteString :> Header "X-Signature-Timestamp" BS.ByteString :> Post '[JSON] AE.Value
  , slackInteractions :: mode :- "interactions" :> "slack" :> ReqBody '[FormUrlEncoded] Slack.SlackInteraction :> Post '[JSON] AE.Value
  , slackActionsPost :: mode :- "actions" :> "slack" :> ReqBody '[FormUrlEncoded] Slack.SlackActionForm :> Post '[JSON] AE.Value
  , slackEventsPost :: mode :- "slack" :> "events" :> ReqBody '[JSON] Slack.SlackEventPayload :> Post '[JSON] AE.Value
  , externalOptionsGet :: mode :- "interactions" :> "external_options" :> ReqBody '[JSON] AE.Value :> Post '[JSON] AE.Value
  , whatsappIncomingPost :: mode :- "whatsapp" :> "incoming" :> ReqBody '[FormUrlEncoded] Whatsapp.TwilioWhatsAppMessage :> Post '[JSON] AE.Value
  , clientMetadata :: mode :- "api" :> "client_metadata" :> Header "Authorization" Text :> Get '[JSON] ClientMetadata.ClientMetadata
  , lemonWebhook :: mode :- "webhook" :> "lemon-squeezy" :> Header "X-Signature" Text :> ReqBody '[JSON] LemonSqueezy.WebhookData :> Post '[HTML] (Html ())
  , chartsDataShot :: mode :- "chart_data_shot" :> QueryParam "data_type" Charts.DataType :> QueryParam "pid" Projects.ProjectId :> QPT "query" :> QPT "query_sql" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "source" :> AllQueryParams :> Get '[JSON] Charts.MetricsData
  , rrwebPost :: mode :- "rrweb" :> ProjectId :> ReqBody '[JSON] Replay.ReplayPost :> Post '[JSON] AE.Value
  }
  deriving stock (Generic)


-- Cookie Protected Routes
type role CookieProtectedRoutes nominal


type CookieProtectedRoutes :: Type -> Type
data CookieProtectedRoutes mode = CookieProtectedRoutes
  { -- Dashboard routes
    dashboardRedirectGet :: mode :- "p" :> ProjectId :> AllQueryParams :> GetRedirect '[HTML] (Headers '[Header "Location" Text] NoContent)
  , endpointDetailsRedirect :: mode :- "p" :> ProjectId :> "endpoints" :> "details" :> AllQueryParams :> GetRedirect '[HTML] (Headers '[Header "Location" Text] NoContent)
  , dashboardsGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> QPT "file" :> QPT "from" :> QPT "to" :> QPT "since" :> AllQueryParams :> Get '[HTML] (RespHeaders (PageCtx Dashboards.DashboardGet))
  , dashboardsGetList :: mode :- "p" :> ProjectId :> "dashboards" :> QPT "embedded" :> Get '[HTML] (RespHeaders (PageCtx Dashboards.DashboardsGet))
  , dashboardsPost :: mode :- "p" :> ProjectId :> "dashboards" :> ReqBody '[FormUrlEncoded] Dashboards.DashboardForm :> Post '[HTML] (RespHeaders NoContent)
  , dashboardWidgetPut :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> QPT "widget_id" :> ReqBody '[JSON] Widget.Widget :> Put '[HTML] (RespHeaders Widget.Widget)
  , dashboardWidgetReorderPatchH :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "widgets_order" :> ReqBody '[JSON] (Map Text Dashboards.WidgetReorderItem) :> Patch '[HTML] (RespHeaders NoContent)
  , dashboardDelete :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> Delete '[HTML] (RespHeaders NoContent)
  , dashboardRenamePatch :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "rename" :> ReqBody '[FormUrlEncoded] Dashboards.DashboardRenameForm :> Patch '[HTML] (RespHeaders (Html ()))
  , dashboardDuplicatePost :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "duplicate" :> Post '[HTML] (RespHeaders NoContent)
  , dashboardDuplicateWidget :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "widgets" :> Capture "widget_id" Text :> "duplicate" :> Post '[HTML] (RespHeaders Widget.Widget)
  , dashboardWidgetExpandGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "widgets" :> Capture "widget_id" Text :> "expand" :> Get '[HTML] (RespHeaders (Html ()))
  , -- API routes
    apiGet :: mode :- "p" :> ProjectId :> "apis" :> Get '[HTML] (RespHeaders Api.ApiGet)
  , apiDelete :: mode :- "p" :> ProjectId :> "apis" :> Capture "keyID" ProjectApiKeys.ProjectApiKeyId :> Delete '[HTML] (RespHeaders Api.ApiMut)
  , apiPatch :: mode :- "p" :> ProjectId :> "apis" :> Capture "keyID" ProjectApiKeys.ProjectApiKeyId :> Patch '[HTML] (RespHeaders Api.ApiMut)
  , apiPost :: mode :- "p" :> ProjectId :> "apis" :> ReqBody '[FormUrlEncoded] Api.GenerateAPIKeyForm :> Post '[HTML] (RespHeaders Api.ApiMut)
  , -- Charts and widgets
    chartsDataGet :: mode :- "chart_data" :> QueryParam "data_type" Charts.DataType :> QueryParam "pid" Projects.ProjectId :> QPT "query" :> QPT "query_sql" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "source" :> AllQueryParams :> Get '[JSON] Charts.MetricsData
  , widgetPost :: mode :- "p" :> ProjectId :> "widget" :> ReqBody '[JSON, FormUrlEncoded] Widget.Widget :> Post '[HTML] (RespHeaders Widget.Widget)
  , widgetGet :: mode :- "p" :> ProjectId :> "widget" :> QPT "widgetJSON" :> QPT "since" :> QPT "from" :> QPT "to" :> AllQueryParams :> Get '[HTML] (RespHeaders Widget.Widget)
  , -- Endpoints and fields
    endpointListGet :: mode :- "p" :> ProjectId :> "endpoints" :> QPT "page" :> QPT "layout" :> QPT "filter" :> QPT "host" :> QPT "request_type" :> QPT "sort" :> HXRequest :> HXBoosted :> HXCurrentURL :> QPT "load_more" :> QPT "search" :> Get '[HTML] (RespHeaders ApiCatalog.EndpointRequestStatsVM)
  , apiCatalogGet :: mode :- "p" :> ProjectId :> "api_catalog" :> QPT "sort" :> QPT "since" :> QPT "request_type" :> QPI "skip" :> Get '[HTML] (RespHeaders ApiCatalog.CatalogList)
  , -- Slack/Discord integration
    reportsGet :: mode :- "p" :> ProjectId :> "reports" :> QPT "page" :> HXRequest :> HXBoosted :> Get '[HTML] (RespHeaders Reports.ReportsGet)
  , reportsSingleGet :: mode :- "p" :> ProjectId :> "reports" :> Capture "report_id" ReportsM.ReportId :> HXRequest :> Get '[HTML] (RespHeaders Reports.ReportsGet)
  , reportsPost :: mode :- "p" :> ProjectId :> "reports_notif" :> Capture "report_type" Text :> Post '[HTML] (RespHeaders Reports.ReportsPost)
  , shareLinkPost :: mode :- "p" :> ProjectId :> "share" :> Capture "event_id" UUID.UUID :> Capture "createdAt" UTCTime :> QPT "event_type" :> Post '[HTML] (RespHeaders Share.ShareLinkPost)
  , -- Billing
    manageBillingGet :: mode :- "p" :> ProjectId :> "manage_billing" :> QPT "from" :> Get '[HTML] (RespHeaders LemonSqueezy.BillingGet)
  , replaySessionGet :: mode :- "p" :> ProjectId :> "replay_session" :> Capture "sessionId" UUID.UUID :> Get '[JSON] (RespHeaders AE.Value)
  , bringS3 :: mode :- "p" :> ProjectId :> "byob_s3" :> Get '[HTML] (RespHeaders (Html ()))
  , bringS3Post :: mode :- "p" :> ProjectId :> "byob_s3" :> ReqBody '[FormUrlEncoded] Projects.ProjectS3Bucket :> Post '[HTML] (RespHeaders (Html ()))
  , bringS3Remove :: mode :- "p" :> ProjectId :> "byob_s3" :> Delete '[HTML] (RespHeaders (Html ()))
  , -- Sub-route groups
    projects :: mode :- ProjectsRoutes
  , anomalies :: mode :- "p" :> ProjectId :> "anomalies" :> AnomaliesRoutes
  , logExplorer :: mode :- "p" :> ProjectId :> LogExplorerRoutes
  , monitors :: mode :- "p" :> ProjectId :> MonitorsRoutes
  , traces :: mode :- "p" :> ProjectId :> TelemetryRoutes
  }
  deriving stock (Generic)


-- Log Explorer Routes
type role LogExplorerRoutes' nominal
type LogExplorerRoutes = NamedRoutes LogExplorerRoutes'


type LogExplorerRoutes' :: Type -> Type
data LogExplorerRoutes' mode = LogExplorerRoutes'
  { logExplorerGet :: mode :- "log_explorer" :> QPT "query" :> QPT "cols" :> QPU "cursor" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "layout" :> QPT "source" :> QPT "target-spans" :> QPT "queryTitle" :> QPT "queryLibId" :> QPT "details_width" :> QPT "target_event" :> QPT "showTrace" :> HXRequest :> HXBoosted :> QPT "json" :> QPT "viz_type" :> QPT "alert" :> QPI "pattern_skip" :> QPT "pattern_target" :> Get '[HTML, JSON] (RespHeaders Log.LogsGet)
  , logExplorerItemDetailedGet :: mode :- "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" UTCTime :> "detailed" :> QPT "source" :> Get '[HTML] (RespHeaders LogItem.ApiItemDetailed)
  , aiSearchPost :: mode :- "log_explorer" :> "ai_search" :> ReqBody '[JSON] AE.Value :> Post '[JSON] (RespHeaders AE.Value)
  , schemaGet :: mode :- "log_explorer" :> "schema" :> Get '[JSON] (RespHeaders AE.Value)
  }
  deriving stock (Generic)


-- Anomalies Routes
type role AnomaliesRoutes' nominal
type AnomaliesRoutes = NamedRoutes AnomaliesRoutes'


type AnomaliesRoutes' :: Type -> Type
data AnomaliesRoutes' mode = AnomaliesRoutes'
  { acknowlegeGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "acknowlege" :> QPT "host" :> Get '[HTML] (RespHeaders AnomalyList.AnomalyAction)
  , unAcknowlegeGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "unacknowlege" :> Get '[HTML] (RespHeaders AnomalyList.AnomalyAction)
  , archiveGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "archive" :> Get '[HTML] (RespHeaders AnomalyList.AnomalyAction)
  , unarchiveGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "unarchive" :> Get '[HTML] (RespHeaders AnomalyList.AnomalyAction)
  , bulkActionsPost :: mode :- "bulk_actions" :> Capture "action" Text :> ReqBody '[FormUrlEncoded] AnomalyList.AnomalyBulkForm :> Post '[HTML] (RespHeaders AnomalyList.AnomalyAction)
  , listGet :: mode :- QPT "layout" :> QPT "filter" :> QPT "sort" :> QPT "since" :> QPT "page" :> QPT "load_more" :> QEID "endpoint" :> HXRequest :> HXBoosted :> Get '[HTML] (RespHeaders AnomalyList.AnomalyListGet)
  }
  deriving stock (Generic)


-- Telemetry Routes
type role TelemetryRoutes' nominal
type TelemetryRoutes = NamedRoutes TelemetryRoutes'


type TelemetryRoutes' :: Type -> Type
data TelemetryRoutes' mode = TelemetryRoutes'
  { tracesGet :: mode :- "traces" :> Capture "trace_id" Text :> QPU "timestamp" :> QPT "span_id" :> QPT "nav" :> Get '[HTML] (RespHeaders Trace.TraceDetailsGet)
  , metricsOVGetH :: mode :- "metrics" :> QPT "tab" :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "metric_source" :> QPT "metric_prefix" :> QPI "cursor" :> Get '[HTML] (RespHeaders Metrics.MetricsOverViewGet)
  , metricDetailsGetH :: mode :- "metrics" :> "details" :> Capture "metric_name" Text :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "metric_source" :> Get '[HTML] (RespHeaders (Html ()))
  , metricBreakdownGetH :: mode :- "metrics" :> "details" :> Capture "metric_name" Text :> "breakdown" :> QPT "label" :> Get '[HTML] (RespHeaders (Html ()))
  }
  deriving stock (Generic)


-- Monitors Routes
type role MonitorsRoutes' nominal
type MonitorsRoutes = NamedRoutes MonitorsRoutes'


type MonitorsRoutes' :: Type -> Type
data MonitorsRoutes' mode = MonitorsRoutes'
  { alertUpsertPost :: mode :- "alerts" :> ReqBody '[FormUrlEncoded] Alerts.AlertUpsertForm :> Post '[HTML] (RespHeaders Alerts.Alert)
  , alertListGet :: mode :- "alerts" :> Get '[HTML] (RespHeaders Alerts.Alert)
  , alertSingleGet :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> Get '[HTML] (RespHeaders Alerts.Alert)
  , alertSingleToggleActive :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "toggle_active" :> Post '[HTML] (RespHeaders Alerts.Alert)
  , alertOverviewGet :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "overview" :> Get '[HTML] (RespHeaders Alerts.Alert)
  , monitorListGet :: mode :- "monitors" :> QueryParam "filter" Text :> QueryParam "since" Text :> Get '[HTML] (RespHeaders (PageCtx (ItemsList.ItemsPage Testing.UnifiedMonitorItem)))
  , unifiedMonitorOverviewGet :: mode :- "monitors" :> Capture "monitor_id" Text :> "overview" :> Get '[HTML] (RespHeaders (PageCtx (Html ())))
  }
  deriving stock (Generic)


-- Projects Routes
type role ProjectsRoutes' nominal
type ProjectsRoutes = NamedRoutes ProjectsRoutes'


type ProjectsRoutes' :: Type -> Type
data ProjectsRoutes' mode = ProjectsRoutes'
  { listGet :: mode :- Get '[HTML] (RespHeaders ListProjects.ListProjectsGet)
  , onboardingProject :: mode :- "p" :> "new" :> GetRedirect '[HTML] (Headers '[Header "Location" Text] (PageCtx (Html ()))) -- p represents project
  , createPost :: mode :- "p" :> "update" :> Capture "projectId" Projects.ProjectId :> ReqBody '[FormUrlEncoded] CreateProject.CreateProjectForm :> Post '[HTML] (RespHeaders CreateProject.CreateProject)
  , settingsGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "settings" :> Get '[HTML] (RespHeaders CreateProject.CreateProject)
  , integrationGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "integrations" :> Get '[HTML] (RespHeaders (Html ()))
  , -- Project management
    deleteGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "delete" :> Get '[HTML] (RespHeaders CreateProject.CreateProject)
  , deleteProjectH :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "settings" :> "delete" :> Get '[HTML] (RespHeaders (PageCtx (Html ())))
  , deleteProjectGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "delete" :> Get '[HTML] (RespHeaders CreateProject.CreateProject)
  , -- Member management
    membersManageGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> Get '[HTML] (RespHeaders ManageMembers.ManageMembers)
  , membersManagePost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> QPT "onboarding" :> ReqBody '[FormUrlEncoded] ManageMembers.ManageMembersForm :> Post '[HTML] (RespHeaders ManageMembers.ManageMembers)
  , manageSubscriptionGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_subscription" :> Get '[HTML] (RespHeaders (Html ()))
  , -- Notifications
    notificationsUpdateChannelPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "notifications-channels" :> ReqBody '[FormUrlEncoded] Integrations.NotifListForm :> Post '[HTML] (RespHeaders (Html ()))
  , -- Onboarding routes
    onboading :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> QPT "step" :> Get '[HTML] (RespHeaders (PageCtx (Html ())))
  , onboardingInfoPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "info" :> ReqBody '[FormUrlEncoded] Onboarding.OnboardingInfoForm :> Post '[HTML] (RespHeaders (Html ()))
  , onboardingConfPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "survey" :> ReqBody '[FormUrlEncoded] Onboarding.OnboardingConfForm :> Post '[HTML] (RespHeaders (Html ()))
  , onboardingPhoneEmailsPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "phone-emails" :> ReqBody '[JSON] Onboarding.NotifChannelForm :> Post '[HTML] (RespHeaders (Html ()))
  , onboardingIntegrationCheck :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "integration-check" :> QPT "language" :> Get '[HTML] (RespHeaders (Html ()))
  , onboardingSkipped :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "skip" :> QPT "step" :> Post '[HTML] (RespHeaders (Html ()))
  , -- Pricing routes
    onboardingPricingUpdate :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "pricing" :> ReqBody '[FormUrlEncoded] CreateProject.PricingUpdateForm :> Post '[HTML] (RespHeaders (Html ()))
  , pricingUpdateGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "update_pricing" :> Get '[HTML] (RespHeaders (PageCtx (Html ())))
  , -- Proxy/landing
    proxyLanding :: mode :- "proxy" :> CaptureAll "path" Text :> Get '[PlainText] (RespHeaders Text)
  }
  deriving stock (Generic)


-- =============================================================================
-- Server Implementations
-- =============================================================================

-- Main server for the root routes
server :: Pool PG.Connection -> Routes (AsServerT ATBaseCtx)
server pool =
  Routes
    { public = Servant.serveDirectoryWebApp "./static/public"
    , ping = pingH
    , status = statusH
    , login = Auth.loginH
    , toLogin = Auth.loginRedirectH
    , logout = Auth.logoutH
    , authCallback = Auth.authCallbackH
    , shareLinkGet = Share.shareLinkGetH
    , slackLinkProjectGet = Slack.linkProjectGetH
    , discordLinkProjectGet = Discord.linkDiscordGetH
    , discordInteractions = Discord.discordInteractionsH
    , slackInteractions = Slack.slackInteractionsH
    , slackActionsPost = Slack.slackActionsH
    , slackEventsPost = Slack.slackEventsPostH
    , externalOptionsGet = Slack.externalOptionsH
    , whatsappIncomingPost = Whatsapp.whatsappIncomingPostH
    , clientMetadata = ClientMetadata.clientMetadataH
    , lemonWebhook = LemonSqueezy.webhookPostH
    , chartsDataShot = Charts.queryMetrics
    , rrwebPost = Replay.replayPostH
    , cookieProtected = \sessionWithCookies ->
        Servant.hoistServerWithContext
          (Proxy @(Servant.NamedRoutes CookieProtectedRoutes))
          (Proxy @'[APItoolkitAuthContext])
          ( \page ->
              page
                & State.evalState Map.empty -- TriggerEvents
                & State.evalState Nothing -- HXRedirectDest
                & State.evalState Nothing -- XWidgetJSON
                & Effectful.Reader.Static.runReader sessionWithCookies
          )
          cookieProtectedServer
    }


-- Cookie Protected server
cookieProtectedServer :: Servant.ServerT (Servant.NamedRoutes CookieProtectedRoutes) ATAuthCtx
cookieProtectedServer =
  CookieProtectedRoutes
    { -- Dashboard handlers
      dashboardRedirectGet = Dashboards.entrypointRedirectGetH "_overview.yaml" "Overview" ["overview", "http", "logs", "traces", "events"]
    , endpointDetailsRedirect = Dashboards.entrypointRedirectGetH "endpoint-stats.yaml" "Endpoint Analytics" ["endpoints", "http", "events"]
    , dashboardsGet = Dashboards.dashboardGetH
    , dashboardsGetList = Dashboards.dashboardsGetH
    , dashboardsPost = Dashboards.dashboardsPostH
    , dashboardWidgetPut = Dashboards.dashboardWidgetPutH
    , dashboardWidgetReorderPatchH = Dashboards.dashboardWidgetReorderPatchH
    , dashboardDelete = Dashboards.dashboardDeleteH
    , dashboardRenamePatch = Dashboards.dashboardRenamePatchH
    , dashboardDuplicatePost = Dashboards.dashboardDuplicatePostH
    , dashboardDuplicateWidget = Dashboards.dashboardDuplicateWidgetPostH
    , dashboardWidgetExpandGet = Dashboards.dashboardWidgetExpandGetH
    , -- API handlers
      apiGet = Api.apiGetH
    , apiDelete = Api.apiDeleteH
    , apiPatch = Api.apiActivateH
    , apiPost = Api.apiPostH
    , -- Chart and widget handlers
      chartsDataGet = Charts.queryMetrics
    , widgetPost = Widget.widgetPostH
    , widgetGet = widgetGetH
    , -- Slack/Discord handlers
      reportsGet = Reports.reportsGetH
    , reportsSingleGet = Reports.singleReportGetH
    , reportsPost = Reports.reportsPostH
    , shareLinkPost = Share.shareLinkPostH
    , replaySessionGet = Replay.replaySessionGetH
    , bringS3 = S3.bringS3GetH
    , bringS3Post = S3.brings3PostH
    , bringS3Remove = S3.brings3RemoveH
    , -- Billing handlers
      manageBillingGet = LemonSqueezy.manageBillingGetH
    , -- Endpoint handlers
      endpointListGet = ApiCatalog.endpointListGetH
    , apiCatalogGet = ApiCatalog.apiCatalogH
    , -- Sub-route handlers
      projects = projectsServer
    , logExplorer = logExplorerServer
    , anomalies = anomaliesServer
    , monitors = monitorsServer
    , traces = telemetryServer
    }


-- Log Explorer server
logExplorerServer :: Projects.ProjectId -> Servant.ServerT LogExplorerRoutes ATAuthCtx
logExplorerServer pid =
  LogExplorerRoutes'
    { logExplorerGet = Log.apiLogH pid
    , logExplorerItemDetailedGet = LogItem.expandAPIlogItemH pid
    , aiSearchPost = Log.aiSearchH pid
    , schemaGet = addRespHeaders Schema.telemetrySchemaJson
    }


-- Anomalies server
anomaliesServer :: Projects.ProjectId -> Servant.ServerT AnomaliesRoutes ATAuthCtx
anomaliesServer pid =
  AnomaliesRoutes'
    { acknowlegeGet = AnomalyList.acknowlegeAnomalyGetH pid
    , unAcknowlegeGet = AnomalyList.unAcknowlegeAnomalyGetH pid
    , archiveGet = AnomalyList.archiveAnomalyGetH pid
    , unarchiveGet = AnomalyList.unArchiveAnomalyGetH pid
    , bulkActionsPost = AnomalyList.anomalyBulkActionsPostH pid
    , listGet = AnomalyList.anomalyListGetH pid
    }


-- Telemetry server
telemetryServer :: Projects.ProjectId -> Servant.ServerT TelemetryRoutes ATAuthCtx
telemetryServer pid =
  TelemetryRoutes'
    { tracesGet = Trace.traceH pid
    , metricsOVGetH = Metrics.metricsOverViewGetH pid
    , metricDetailsGetH = Metrics.metricDetailsGetH pid
    , metricBreakdownGetH = Metrics.metricBreakdownGetH pid
    }


-- Monitors server
monitorsServer :: Projects.ProjectId -> Servant.ServerT MonitorsRoutes ATAuthCtx
monitorsServer pid =
  MonitorsRoutes'
    { alertUpsertPost = Alerts.alertUpsertPostH pid
    , alertListGet = Alerts.alertListGetH pid
    , alertSingleGet = Alerts.alertSingleGetH pid
    , alertSingleToggleActive = Alerts.alertSingleToggleActiveH pid
    , alertOverviewGet = Alerts.alertOverviewGetH pid
    , monitorListGet = Testing.unifiedMonitorsGetH pid
    , unifiedMonitorOverviewGet = Testing.unifiedMonitorOverviewH pid
    }


-- Projects server
projectsServer :: Servant.ServerT ProjectsRoutes ATAuthCtx
projectsServer =
  ProjectsRoutes'
    { listGet = ListProjects.listProjectsGetH
    , onboardingProject = CreateProject.projectOnboarding
    , createPost = CreateProject.createProjectPostH
    , settingsGet = CreateProject.projectSettingsGetH
    , integrationGet = Integrations.integrationsSettingsGetH
    , deleteGet = CreateProject.deleteProjectGetH
    , deleteProjectH = CreateProject.projectDeleteGetH
    , notificationsUpdateChannelPost = Integrations.updateNotificationsChannel
    , deleteProjectGet = CreateProject.deleteProjectGetH
    , membersManageGet = ManageMembers.manageMembersGetH
    , membersManagePost = ManageMembers.manageMembersPostH
    , manageSubscriptionGet = ManageMembers.manageSubGetH
    , onboading = Onboarding.onboardingGetH
    , onboardingInfoPost = Onboarding.onboardingInfoPost
    , onboardingConfPost = Onboarding.onboardingConfPost
    , onboardingPhoneEmailsPost = Onboarding.phoneEmailPostH
    , onboardingIntegrationCheck = Onboarding.checkIntegrationGet
    , onboardingPricingUpdate = CreateProject.pricingUpdateH
    , pricingUpdateGet = CreateProject.pricingUpdateGetH
    , onboardingSkipped = Onboarding.onboardingStepSkipped
    , proxyLanding = Onboarding.proxyLandingH
    }


-- =============================================================================
-- Utility handlers and helpers
-- =============================================================================

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
pingH = pure "pong"


-- Widget GET handler that accepts dashboard parameters
widgetGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Maybe Text)] -> ATAuthCtx (RespHeaders Widget.Widget)
widgetGetH pid widgetJsonM sinceStr fromDStr toDStr allParams = do
  -- Parse the widgetJSON parameter
  let v = fromMaybe "" widgetJsonM
  widget <- case AE.eitherDecode (encodeUtf8 v) of
    Right w -> pure w
    Left err -> do
      Error.throwError $ err400{errBody = "Invalid or missing widgetJSON parameter"}

  -- Get the current time for processing
  now <- Time.currentTime

  -- Process the widget with dashboard parameters (similar to how processWidget works in Dashboards)
  let widgetWithPid = widget & #_projectId ?~ pid

  -- Process eager widgets (tables, stats, etc.) with dashboard parameters
  processedWidget <-
    if widgetWithPid.eager == Just True || widgetWithPid.wType `elem` [Widget.WTTable, Widget.WTTraces, Widget.WTStat, Widget.WTAnomalies]
      then Dashboards.processEagerWidget pid now (sinceStr, fromDStr, toDStr) allParams widgetWithPid
      else pure widgetWithPid

  -- Return the processed widget
  addRespHeaders processedWidget


-- =============================================================================
-- Authentication and error handling
-- =============================================================================

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


-- =============================================================================
-- Custom HasServer instances
-- =============================================================================

-- | We add a HasServer instance that says:
--   - The handler will receive a list of (Text, Maybe Text).
--   - We pull that out of the WAI `Request` in `route`.
instance HasServer api ctx => HasServer (AllQueryParams :> api) ctx where
  type ServerT (AllQueryParams :> api) m = [(Text, Maybe Text)] -> ServerT api m


  route _ ctx subserver = route (Proxy :: Proxy api) ctx subserver'
    where
      grabAllParams :: Request -> [(Text, Maybe Text)]
      grabAllParams req =
        [ ( decodeUtf8With lenientDecode k
          , decodeUtf8With lenientDecode <$> mv
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
  hoistServerWithContext _ pc nat s = hoistServerWithContext (Proxy :: Proxy api) pc nat . s
