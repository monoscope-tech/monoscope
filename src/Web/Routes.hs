module Web.Routes (server, genAuthServerContext, KeepPrefixExp, widgetPngGetH, ApiV1Routes, apiV1OpenApiSpec) where

-- Standard library imports
import Control.Lens
import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Ord (clamp)
import Data.Pool (Pool)
import Data.UUID qualified as UUID
import GHC.TypeLits (Symbol)
import Relude hiding (ask)

-- Database imports
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL qualified as PG

-- Effectful imports
import Data.Effectful.Notify qualified as Notify
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (runReader)
import Effectful.Reader.Static qualified
import Effectful.State.Static.Local qualified as State
import Effectful.Time qualified as Time

-- Web and server imports
import Log (Logger, UTCTime)
import Lucid
import Network.Wai (Request, queryString)
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Htmx
import Servant.QueryParam.Record (RecordParam)
import Servant.QueryParam.Server.Record ()
import Servant.QueryParam.TypeLevel (Eval, Exp)
import Servant.Server.Generic (AsServerT)
import Servant.Server.Internal.Delayed (passToServer)
import Web.Cookie (SetCookie)

-- System and configuration imports

import Data.OpenApi (OpenApi, info, title, version)
import Deriving.Aeson qualified as DAE
import Pages.Bots.Utils (verifyWidgetSignature)
import Pages.CommandPalette qualified as CommandPalette
import Servant.OpenApi (toOpenApi)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Exit (ExitCode (..))
import System.Logging qualified as Log
import System.Process.Typed (byteStringInput, proc, readProcess, setStdin)
import System.Timeout (timeout)
import System.Types (ATAuthCtx, ATBaseCtx, HXRedirectDest, RespHeaders, TriggerEvents, XWidgetJSON, addRespHeaders)
import Web.Auth (APItoolkitAuthContext, ApiKeyAuthContext, apiKeyAuthHandler, authHandler, htmlServerError)
import Web.Auth qualified as Auth

-- Model imports

import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy qualified as LBS
import "base64" Data.ByteString.Base64.URL qualified as B64URL
import Data.CaseInsensitive qualified as CI
import Data.Effectful.Wreq qualified as Wreq
import Data.Text qualified as T
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import UnliftIO.Exception (handle)
import "cryptohash-md5" Crypto.Hash.MD5 qualified as MD5

-- Page imports

import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Issues qualified as Anomalies
import Pages.Anomalies qualified as AnomalyList
import Pages.BodyWrapper (PageCtx (..))
import Pages.Bots.Discord qualified as Discord
import Pages.Bots.Slack qualified as Slack
import Pages.Bots.Utils qualified as BotUtils
import Pages.Bots.Whatsapp qualified as Whatsapp
import Pages.Charts.Charts qualified as Charts
import Pages.Dashboards qualified as Dashboards
import Pages.Endpoints qualified as ApiCatalog
import Pages.GitSync qualified as GitSync
import Pages.LogExplorer.Log qualified as Log
import Pages.LogExplorer.LogItem qualified as LogItem
import Pages.Monitors qualified as Alerts
import Pages.Monitors qualified as Testing
import Pages.Onboarding qualified as Onboarding
import Pages.Projects qualified as CreateProject
import Pages.Projects qualified as Integrations
import Pages.Projects qualified as ListProjects
import Pages.Projects qualified as ManageMembers
import Pages.Replay qualified as Replay
import Pages.Reports qualified as Reports
import Pages.Settings qualified as Settings
import Pages.Share qualified as Share
import Pages.Telemetry qualified as Metrics
import Pages.Telemetry qualified as Trace
import Pkg.Components.Table qualified as Table
import Pkg.Components.Widget qualified as Widget
import Pkg.EmailTemplates qualified as ET


-- =============================================================================
-- Common query parameter type aliases
-- =============================================================================

type ProjectId = Capture "projectID" Projects.ProjectId
type GetRedirect = Verb 'GET 302


-- Query parameter types
type QPT a = QueryParam a Text
type QPU a = QueryParam a UTCTime
type QPI a = QueryParam a Int
type QEID a = QueryParam a Endpoints.EndpointId
type QPUUId a = QueryParam a UUID.UUID


-- Custom type for handling all query parameters
data AllQueryParams


-- Type-level expression for preserving field prefixes in RecordParam
type role KeepPrefixExp phantom phantom
data KeepPrefixExp :: Symbol -> Exp Symbol
type instance Eval (KeepPrefixExp sym) = sym


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
-- Public API v1 Routes
-- =============================================================================

type ApiV1Routes :: Type -> Type
type role ApiV1Routes nominal
data ApiV1Routes mode = ApiV1Routes
  { eventsSearch
      :: mode
        :- "events"
          :> QPT "query"
          :> QPT "since"
          :> QPT "from"
          :> QPT "to"
          :> QPT "source"
          :> QueryParam "limit" Int
          :> Get '[JSON] Log.LogResult
  , metricsQuery
      :: mode
        :- "metrics"
          :> QueryParam "query" Text
          :> QueryParam "data_type" Charts.DataType
          :> QPT "since"
          :> QPT "from"
          :> QPT "to"
          :> QPT "source"
          :> Get '[JSON] Charts.MetricsData
  , schemaGet :: mode :- "schema" :> Get '[JSON] Schema.Schema
  , monitorsGet :: mode :- "monitors" :> Get '[JSON] [Monitors.QueryMonitor]
  , rrwebPost :: mode :- "rrweb" :> ReqBody '[JSON] Replay.ReplayPost :> Post '[JSON] AE.Value
  }
  deriving stock (Generic)


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
  , slackLinkProjectGet :: mode :- "slack" :> "oauth" :> "callback" :> QPT "code" :> QPT "state" :> GetRedirect '[HTML] (Headers '[Header "Location" Text] BotUtils.BotResponse)
  , discordLinkProjectGet :: mode :- "discord" :> "oauth" :> "callback" :> QPT "state" :> QPT "code" :> QPT "guild_id" :> GetRedirect '[HTML] (Headers '[Header "Location" Text] BotUtils.BotResponse)
  , discordInteractions :: mode :- "discord" :> "interactions" :> ReqBody '[RawJSON] BS.ByteString :> Header "X-Signature-Ed25519" BS.ByteString :> Header "X-Signature-Timestamp" BS.ByteString :> Post '[JSON] AE.Value
  , slackInteractions :: mode :- "interactions" :> "slack" :> ReqBody '[FormUrlEncoded] Slack.SlackInteraction :> Post '[JSON] AE.Value
  , slackActionsPost :: mode :- "actions" :> "slack" :> ReqBody '[FormUrlEncoded] Slack.SlackActionForm :> Post '[JSON] AE.Value
  , slackEventsPost :: mode :- "slack" :> "events" :> ReqBody '[JSON] Slack.SlackEventPayload :> Post '[JSON] AE.Value
  , externalOptionsGet :: mode :- "interactions" :> "external_options" :> ReqBody '[JSON] AE.Value :> Post '[JSON] AE.Value
  , whatsappIncomingPost :: mode :- "whatsapp" :> "incoming" :> ReqBody '[FormUrlEncoded] Whatsapp.TwilioWhatsAppMessage :> Post '[JSON] AE.Value
  , clientMetadata :: mode :- "api" :> "client_metadata" :> Header "Authorization" Text :> Get '[JSON] Auth.ClientMetadata
  , lemonWebhook :: mode :- "webhook" :> "lemon-squeezy" :> Header "X-Signature" Text :> ReqBody '[JSON] Settings.WebhookData :> Post '[HTML] (Html ())
  , stripeWebhook :: mode :- "webhook" :> "stripe" :> Header "Stripe-Signature" Text :> ReqBody '[RawJSON] BS.ByteString :> Post '[HTML] (Html ())
  , githubWebhook :: mode :- "webhook" :> "github" :> Header "X-Hub-Signature-256" Text :> Header "X-GitHub-Event" Text :> ReqBody '[RawJSON] BS.ByteString :> Post '[JSON] AE.Value
  , chartsDataShot :: mode :- "chart_data_shot" :> QueryParam "data_type" Charts.DataType :> QueryParam "pid" Projects.ProjectId :> QPT "query" :> QPT "query_sql" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "source" :> AllQueryParams :> Get '[JSON] Charts.MetricsData
  , avatarGet :: mode :- "api" :> "avatar" :> Capture "user_id" Projects.UserId :> Get '[OctetStream] (Headers '[Header "Cache-Control" Text, Header "Content-Type" Text] LBS.ByteString)
  , widgetPngGet :: mode :- "p" :> ProjectId :> "widget.png" :> QPT "widgetJSON" :> QPT "widgetZ" :> QPT "since" :> QPT "from" :> QPT "to" :> QueryParam "width" Int :> QueryParam "height" Int :> QPT "sig" :> AllQueryParams :> Get '[OctetStream] (Headers '[Header "Cache-Control" Text, Header "Content-Type" Text] LBS.ByteString)
  , proxyLanding :: mode :- "proxy" :> CaptureAll "path" Text :> Get '[PlainText] (RespHeaders Text)
  , deviceCode :: mode :- "api" :> "device" :> "code" :> Post '[JSON] Auth.DeviceCodeResponse
  , deviceToken :: mode :- "api" :> "device" :> "token" :> QPT "device_code" :> Post '[JSON] Auth.DeviceTokenResponse
  , emailPreviewList :: mode :- "dev" :> "emails" :> Get '[HTML] (Html ())
  , emailPreview :: mode :- "dev" :> "emails" :> Capture "template" Text :> Get '[HTML] (Html ())
  , apiV1 :: mode :- "api" :> "v1" :> AuthProtect "api-key-auth" :> NamedRoutes ApiV1Routes
  , apiV1OpenApi :: mode :- "api" :> "v1" :> "openapi.json" :> Get '[JSON] OpenApi
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
  , dashboardsGetList :: mode :- "p" :> ProjectId :> "dashboards" :> QPT "sort" :> QPT "embedded" :> QPUUId "teamId" :> QPT "copy_widget_id" :> QPUUId "source_dashboard_id" :> RecordParam KeepPrefixExp Dashboards.DashboardFilters :> Get '[HTML] (RespHeaders Dashboards.DashboardsGet)
  , dashboardsPost :: mode :- "p" :> ProjectId :> "dashboards" :> ReqBody '[FormUrlEncoded] Dashboards.DashboardForm :> Post '[HTML] (RespHeaders Dashboards.DashboardRes)
  , dashboardWidgetPut :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> QPT "widget_id" :> QPT "tab" :> ReqBody '[JSON] Widget.Widget :> Put '[HTML] (RespHeaders Widget.Widget)
  , dashboardWidgetReorderPatchH :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "widgets_order" :> QPT "tab" :> ReqBody '[JSON] (Map Text Dashboards.WidgetReorderItem) :> Patch '[HTML] (RespHeaders NoContent)
  , dashboardDelete :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> Delete '[HTML] (RespHeaders Dashboards.DashboardRes)
  , dashboardRenamePatch :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "rename" :> ReqBody '[FormUrlEncoded] Dashboards.DashboardRenameForm :> Patch '[HTML] (RespHeaders Dashboards.DashboardRes)
  , dashboardDuplicatePost :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "duplicate" :> Post '[HTML] (RespHeaders Dashboards.DashboardRes)
  , dashboardStarPost :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "star" :> Post '[HTML] (RespHeaders (Html ()))
  , dashboardDuplicateWidget :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "widgets" :> Capture "widget_id" Text :> "duplicate" :> QPUUId "source_dashboard_id" :> Post '[HTML] (RespHeaders Widget.Widget)
  , dashboardWidgetExpandGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "widgets" :> Capture "widget_id" Text :> "expand" :> Get '[HTML] (RespHeaders (Html ()))
  , -- Widget alert routes
    widgetAlertUpsert :: mode :- "p" :> ProjectId :> "widgets" :> Capture "widget_id" Text :> "alert" :> QPUUId "dashboard_id" :> ReqBody '[FormUrlEncoded] Dashboards.WidgetAlertForm :> Post '[HTML] (RespHeaders (Html ()))
  , widgetAlertDelete :: mode :- "p" :> ProjectId :> "widgets" :> Capture "widget_id" Text :> "alert" :> Delete '[HTML] (RespHeaders (Html ()))
  , dashboardBulkActionPost :: mode :- "p" :> ProjectId :> "dashboards" :> "bulk_action" :> Capture "action" Text :> ReqBody '[FormUrlEncoded] Dashboards.DashboardBulkActionForm :> Post '[HTML] (RespHeaders NoContent)
  , -- Dashboard tab routes (htmx lazy loading)
    dashboardTabGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "tab" :> Capture "tab_slug" Text :> QPT "file" :> QPT "from" :> QPT "to" :> QPT "since" :> AllQueryParams :> Get '[HTML] (RespHeaders (PageCtx Dashboards.DashboardGet))
  , dashboardTabContentGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "tab" :> Capture "tab_slug" Text :> "content" :> QPT "file" :> QPT "from" :> QPT "to" :> QPT "since" :> AllQueryParams :> Get '[HTML] (RespHeaders (Html ()))
  , dashboardTabRenamePatch :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "tab" :> Capture "tab_slug" Text :> "rename" :> ReqBody '[FormUrlEncoded] Dashboards.TabRenameForm :> Patch '[HTML] (RespHeaders Dashboards.TabRenameRes)
  , dashboardYamlGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "yaml" :> Get '[HTML] (RespHeaders (Html ()))
  , dashboardYamlPut :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "yaml" :> ReqBody '[FormUrlEncoded] Dashboards.YamlForm :> Put '[HTML] (RespHeaders (Html ()))
  , -- API routes
    apiGet :: mode :- "p" :> ProjectId :> "apis" :> Get '[HTML] (RespHeaders Settings.ApiGet)
  , apiDelete :: mode :- "p" :> ProjectId :> "apis" :> Capture "keyID" ProjectApiKeys.ProjectApiKeyId :> Delete '[HTML] (RespHeaders Settings.ApiMut)
  , apiPatch :: mode :- "p" :> ProjectId :> "apis" :> Capture "keyID" ProjectApiKeys.ProjectApiKeyId :> Patch '[HTML] (RespHeaders Settings.ApiMut)
  , apiPost :: mode :- "p" :> ProjectId :> "apis" :> ReqBody '[FormUrlEncoded] Settings.GenerateAPIKeyForm :> Post '[HTML] (RespHeaders Settings.ApiMut)
  , -- Charts and widgets
    chartsDataGet :: mode :- "chart_data" :> QueryParam "data_type" Charts.DataType :> QueryParam "pid" Projects.ProjectId :> QPT "query" :> QPT "query_sql" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "source" :> AllQueryParams :> Get '[JSON] Charts.MetricsData
  , widgetPost :: mode :- "p" :> ProjectId :> "widget" :> QPT "since" :> QPT "from" :> QPT "to" :> ReqBody '[JSON, FormUrlEncoded] Widget.Widget :> Post '[HTML] (RespHeaders Widget.Widget)
  , widgetGet :: mode :- "p" :> ProjectId :> "widget" :> QPT "widgetJSON" :> QPT "since" :> QPT "from" :> QPT "to" :> AllQueryParams :> Get '[HTML] (RespHeaders Widget.Widget)
  , widgetSqlPreview :: mode :- "p" :> ProjectId :> "widget" :> "sql-preview" :> QPT "query" :> QPT "since" :> QPT "from" :> QPT "to" :> Get '[HTML] (RespHeaders (Html ()))
  , -- Endpoints and fields
    endpointListGet :: mode :- "p" :> ProjectId :> "endpoints" :> QPT "page" :> QPT "layout" :> QPT "filter" :> QPT "host" :> QPT "request_type" :> QPT "sort" :> QPT "period" :> HXRequest :> HXBoosted :> HXCurrentURL :> QPT "load_more" :> QPT "search" :> Get '[HTML] (RespHeaders ApiCatalog.EndpointRequestStatsVM)
  , apiCatalogGet :: mode :- "p" :> ProjectId :> "api_catalog" :> QPT "sort" :> QPT "since" :> QPT "request_type" :> QPT "period" :> QPI "skip" :> Get '[HTML] (RespHeaders ApiCatalog.CatalogList)
  , -- Slack/Discord integration
    reportsGet :: mode :- "p" :> ProjectId :> "reports" :> QPT "page" :> HXRequest :> HXBoosted :> Get '[HTML] (RespHeaders Reports.ReportsGet)
  , reportsLiveGet :: mode :- "p" :> ProjectId :> "reports" :> "live" :> HXRequest :> Get '[HTML] (RespHeaders Reports.ReportsGet)
  , reportsSingleGet :: mode :- "p" :> ProjectId :> "reports" :> Capture "report_id" Anomalies.ReportId :> HXRequest :> Get '[HTML] (RespHeaders Reports.ReportsGet)
  , reportsPost :: mode :- "p" :> ProjectId :> "reports_notif" :> Capture "report_type" Text :> Post '[HTML] (RespHeaders Reports.ReportsPost)
  , shareLinkPost :: mode :- "p" :> ProjectId :> "share" :> Capture "event_id" UUID.UUID :> Capture "createdAt" UTCTime :> QPT "event_type" :> Post '[HTML] (RespHeaders Share.ShareLinkPost)
  , -- Billing
    manageBillingGet :: mode :- "p" :> ProjectId :> "manage_billing" :> QPT "from" :> Get '[HTML] (RespHeaders Settings.BillingGet)
  , replaySessionGet :: mode :- "p" :> ProjectId :> "replay_session" :> Capture "sessionId" UUID.UUID :> Get '[JSON] (RespHeaders AE.Value)
  , bringS3 :: mode :- "p" :> ProjectId :> "byob_s3" :> Get '[HTML] (RespHeaders (Html ()))
  , bringS3Post :: mode :- "p" :> ProjectId :> "byob_s3" :> ReqBody '[FormUrlEncoded] Projects.ProjectS3Bucket :> Post '[HTML] (RespHeaders (Html ()))
  , bringS3Remove :: mode :- "p" :> ProjectId :> "byob_s3" :> Delete '[HTML] (RespHeaders (Html ()))
  , gitSyncSettings :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> Get '[HTML] (RespHeaders (Html ()))
  , gitSyncSettingsPost :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> ReqBody '[FormUrlEncoded] GitSync.GitSyncForm :> Post '[HTML] (RespHeaders (Html ()))
  , gitSyncSettingsDelete :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> Delete '[HTML] (RespHeaders (Html ()))
  , -- GitHub App routes
    githubAppInstall :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> "install" :> Get '[HTML] (RespHeaders (Html ()))
  , githubAppCallback :: mode :- "github" :> "callback" :> QueryParam "installation_id" Int64 :> QueryParam "setup_action" Text :> QueryParam "state" Text :> Get '[HTML] (RespHeaders (Html ()))
  , githubAppRepos :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> "repos" :> QueryParam "installationId" Int64 :> Get '[HTML] (RespHeaders (Html ()))
  , githubAppSelectRepo :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> "select" :> ReqBody '[FormUrlEncoded] GitSync.RepoSelectForm :> Post '[HTML] (RespHeaders (Html ()))
  , -- Command palette
    commandPaletteGet :: mode :- "p" :> ProjectId :> "command-palette" :> Get '[HTML] (RespHeaders (Html ()))
  , commandPaletteRecentPost :: mode :- "p" :> ProjectId :> "command-palette" :> "recents" :> ReqBody '[FormUrlEncoded] CommandPalette.RecentForm :> Post '[HTML] (RespHeaders NoContent)
  , -- Device auth
    deviceApprove :: mode :- "device" :> QPT "code" :> QPT "action" :> Get '[HTML] (RespHeaders (Html ()))
  , -- Sub-route groups
    projects :: mode :- ProjectsRoutes
  , anomalies :: mode :- "p" :> ProjectId :> "issues" :> AnomaliesRoutes
  , logExplorer :: mode :- "p" :> ProjectId :> LogExplorerRoutes
  , monitors :: mode :- "p" :> ProjectId :> "monitors" :> MonitorsRoutes
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
  , listGet :: mode :- QPT "layout" :> QPT "filter" :> QPT "sort" :> QPT "since" :> QPT "page" :> QPT "per_page" :> QPT "load_more" :> QEID "endpoint" :> QPT "period" :> QueryParams "service" Text :> QueryParams "type" Text :> HXRequest :> HXBoosted :> Get '[HTML] (RespHeaders AnomalyList.AnomalyListGet)
  , anomalyGet :: mode :- Capture "anomalyID" Anomalies.IssueId :> QPT "first_occurrence" :> QPT "since" :> Get '[HTML] (RespHeaders (PageCtx (Html ())))
  , anomalyHashGet :: mode :- "by_hash" :> Capture "anomalyHash" Text :> QPT "first_occurrence" :> QPT "since" :> Get '[HTML] (RespHeaders (PageCtx (Html ())))
  , assignErrorPost :: mode :- "errors" :> Capture "errorID" UUID.UUID :> "assign" :> ReqBody '[FormUrlEncoded] AnomalyList.AssignErrorForm :> Post '[HTML] (RespHeaders (Html ()))
  , resolveErrorPost :: mode :- "errors" :> Capture "errorID" UUID.UUID :> "resolve" :> Post '[HTML] (RespHeaders (Html ()))
  , errorSubscriptionPost :: mode :- "errors" :> Capture "errorID" UUID.UUID :> "subscribe" :> ReqBody '[FormUrlEncoded] AnomalyList.ErrorSubscriptionForm :> Post '[HTML] (RespHeaders (Html ()))
  , aiChatPost :: mode :- Capture "issueID" Anomalies.IssueId :> "ai_chat" :> ReqBody '[FormUrlEncoded] AnomalyList.AIChatForm :> Post '[HTML] (RespHeaders (Html ()))
  , aiChatHistoryGet :: mode :- Capture "issueID" Anomalies.IssueId :> "ai_chat" :> "history" :> Get '[HTML] (RespHeaders (Html ()))
  , activityGet :: mode :- Capture "issueID" Anomalies.IssueId :> "activity" :> Get '[HTML] (RespHeaders (Html ()))
  , errorGroupMembersGet :: mode :- "errors" :> Capture "errorID" UUID.UUID :> "group_members" :> Get '[HTML] (RespHeaders (Html ()))
  , errorUnmergePost :: mode :- "errors" :> Capture "errorID" UUID.UUID :> "unmerge" :> Post '[HTML] (RespHeaders (Html ()))
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
  { listGet :: mode :- QueryParam "filter" Text :> QueryParam "since" Text :> Get '[HTML] (RespHeaders (PageCtx (Table.Table Testing.UnifiedMonitorItem)))
  , overviewGet :: mode :- Capture "monitor_id" Text :> "overview" :> Get '[HTML] (RespHeaders (PageCtx (Html ())))
  , alertUpsertPost :: mode :- "alerts" :> ReqBody '[FormUrlEncoded] Alerts.AlertUpsertForm :> Post '[HTML] (RespHeaders Alerts.Alert)
  , alertSingleGet :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> Get '[HTML] (RespHeaders Alerts.Alert)
  , alertSingleToggleActive :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "toggle_active" :> Post '[HTML] (RespHeaders Alerts.Alert)
  , alertMutePost :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "mute" :> QueryParam "duration" Int :> Post '[HTML] (RespHeaders (Html ()))
  , alertUnmutePost :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "unmute" :> Post '[HTML] (RespHeaders (Html ()))
  , alertResolvePost :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "resolve" :> Post '[HTML] (RespHeaders (Html ()))
  , alertDeleteRoute :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> Delete '[HTML] (RespHeaders (Html ()))
  , teamAlertsGetH :: mode :- "alerts" :> "team" :> Capture "team_id" UUID.UUID :> Get '[HTML] (RespHeaders (Table.TableRows Testing.UnifiedMonitorItem))
  , alertTeamDeleteH :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "teams" :> Capture "team_id" UUID.UUID :> Delete '[HTML] (RespHeaders Alerts.Alert)
  , alertBulkAction :: mode :- "alerts" :> "bulk_action" :> Capture "action" Text :> ReqBody '[FormUrlEncoded] ManageMembers.TBulkActionForm :> Post '[HTML] (RespHeaders (PageCtx (Table.Table Testing.UnifiedMonitorItem)))
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
  , integrationGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "settings" :> "integrations" :> Get '[HTML] (RespHeaders (Html ()))
  , -- Project management
    deleteGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "delete" :> Delete '[HTML] (RespHeaders CreateProject.CreateProject)
  , -- Member management
    membersManageGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> Get '[HTML] (RespHeaders ManageMembers.ManageMembers)
  , membersManagePost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> QPT "onboarding" :> ReqBody '[FormUrlEncoded] ManageMembers.ManageMembersForm :> Post '[HTML] (RespHeaders ManageMembers.ManageMembers)
  , membersDeleteH :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> Capture "memberId" UUID.UUID :> Delete '[HTML] (RespHeaders (Html ()))
  , teamsManageGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_teams" :> QPT "what" :> Get '[HTML] (RespHeaders ManageMembers.ManageTeams)
  , teamsManagePost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_teams" :> ReqBody '[JSON] ManageMembers.TeamForm :> QPT "teamView" :> Post '[HTML] (RespHeaders ManageMembers.ManageTeams)
  , teamGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_teams" :> Capture "teamHandle" Text :> QPT "layout" :> Get '[HTML] (RespHeaders ManageMembers.ManageTeams)
  , teamBulkAction :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_teams" :> "bulk_action" :> Capture "action" Text :> ReqBody '[FormUrlEncoded] ManageMembers.TBulkActionForm :> QPT "teamView" :> Post '[HTML] (RespHeaders ManageMembers.ManageTeams)
  , manageSubscriptionGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_subscription" :> Get '[HTML] (RespHeaders (Html ()))
  , stripeCheckout :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "stripe_checkout" :> ReqBody '[FormUrlEncoded] CreateProject.StripeCheckoutForm :> Post '[HTML] (RespHeaders (Html ()))
  , -- Notifications
    notificationsUpdateChannelPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "notifications-channels" :> ReqBody '[FormUrlEncoded] Integrations.NotifListForm :> Post '[HTML] (RespHeaders (Html ()))
  , pagerdutyConnect :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "settings" :> "integrations" :> "pagerduty" :> ReqBody '[FormUrlEncoded] Integrations.PagerdutyConnectForm :> Post '[HTML] (RespHeaders (Html ()))
  , pagerdutyDisconnect :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "settings" :> "integrations" :> "pagerduty" :> "disconnect" :> Post '[HTML] (RespHeaders (Html ()))
  , slackDisconnect :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "settings" :> "integrations" :> "slack" :> Delete '[HTML] (RespHeaders (Html ()))
  , notificationsTestPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "settings" :> "integrations" :> "test" :> ReqBody '[FormUrlEncoded] Settings.TestForm :> Post '[HTML] (RespHeaders (Html ()))
  , notificationsTestHistoryGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "settings" :> "integrations" :> "history" :> Get '[HTML] (RespHeaders Settings.NotificationTestHistoryGet)
  , -- Onboarding routes
    onboading :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> QPT "step" :> Get '[HTML] (RespHeaders Onboarding.OnboardingGet)
  , onboardingInfoPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "info" :> ReqBody '[FormUrlEncoded] Onboarding.OnboardingInfoForm :> Post '[HTML] (RespHeaders Onboarding.OnboardingInfoPost)
  , onboardingConfPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "survey" :> ReqBody '[FormUrlEncoded] Onboarding.OnboardingConfForm :> Post '[HTML] (RespHeaders Onboarding.OnboardingConfPost)
  , onboardingPhoneEmailsPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "phone-emails" :> ReqBody '[JSON] Onboarding.NotifChannelForm :> Post '[HTML] (RespHeaders Onboarding.OnboardingPhoneEmailsPost)
  , onboardingIntegrationCheck :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "integration-check" :> QPT "language" :> Get '[HTML] (RespHeaders (Html ()))
  , onboardingDismissChecklist :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "dismiss-checklist" :> Post '[HTML] (RespHeaders (Html ()))
  , onboardingSkipped :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "skip" :> QPT "step" :> Post '[HTML] (RespHeaders (Html ()))
  , -- Pricing routes
    onboardingPricingUpdate :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "pricing" :> ReqBody '[FormUrlEncoded] CreateProject.PricingUpdateForm :> Post '[HTML] (RespHeaders (Html ()))
  }
  deriving stock (Generic)


-- =============================================================================
-- Server Implementations
-- =============================================================================

-- Main server for the root routes
server :: Pool PGS.Connection -> Routes (AsServerT ATBaseCtx)
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
    , clientMetadata = Auth.clientMetadataH
    , lemonWebhook = Settings.webhookPostH
    , stripeWebhook = Settings.stripeWebhookPostH
    , githubWebhook = GitSync.githubWebhookPostH
    , chartsDataShot = Charts.queryMetrics Nothing
    , avatarGet = avatarGetH
    , widgetPngGet = widgetPngGetH
    , proxyLanding = \path ->
        Onboarding.proxyLandingH path
          & State.evalState @TriggerEvents Map.empty
          & State.evalState @HXRedirectDest Nothing
          & State.evalState @XWidgetJSON Nothing
    , deviceCode = Auth.deviceCodeH
    , deviceToken = Auth.deviceTokenH
    , emailPreviewList = emailPreviewListH
    , emailPreview = emailPreviewH
    , apiV1 = apiV1Server
    , apiV1OpenApi = pure apiV1OpenApiSpec
    , cookieProtected = \sessionWithCookies ->
        Servant.hoistServerWithContext
          (Proxy @(Servant.NamedRoutes CookieProtectedRoutes))
          (Proxy @'[APItoolkitAuthContext])
          ( \page ->
              page
                & Notify.runNotifyProduction
                & State.evalState Map.empty -- TriggerEvents
                & State.evalState Nothing -- HXRedirectDest
                & State.evalState Nothing -- XWidgetJSON
                & Effectful.Reader.Static.runReader sessionWithCookies
          )
          cookieProtectedServer
    }


-- API v1 server
apiV1Server :: Projects.ProjectId -> Servant.ServerT (NamedRoutes ApiV1Routes) ATBaseCtx
apiV1Server pid =
  ApiV1Routes
    { eventsSearch = \queryM sinceM fromM toM sourceM limitM ->
        Log.queryEvents pid queryM sinceM fromM toM sourceM limitM
    , metricsQuery = \queryM dataTypeM sinceM fromM toM sourceM ->
        Charts.queryMetrics Nothing dataTypeM (Just pid) queryM Nothing sinceM fromM toM sourceM []
    , schemaGet = pure Schema.telemetrySchema
    , monitorsGet = Monitors.queryMonitorsAll pid
    , rrwebPost = Replay.replayPostH pid
    }


apiV1OpenApiSpec :: OpenApi
apiV1OpenApiSpec =
  toOpenApi (Proxy @(NamedRoutes ApiV1Routes))
    & info .~ (mempty & title .~ "Monoscope API" & version .~ "1.0")


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
    , dashboardStarPost = Dashboards.dashboardStarPostH
    , dashboardDuplicateWidget = Dashboards.dashboardDuplicateWidgetPostH
    , dashboardWidgetExpandGet = Dashboards.dashboardWidgetExpandGetH
    , widgetAlertUpsert = Dashboards.widgetAlertUpsertH
    , widgetAlertDelete = Dashboards.widgetAlertDeleteH
    , dashboardBulkActionPost = Dashboards.dashboardBulkActionPostH
    , dashboardTabGet = Dashboards.dashboardTabGetH
    , dashboardTabContentGet = Dashboards.dashboardTabContentGetH
    , dashboardTabRenamePatch = Dashboards.dashboardTabRenamePatchH
    , dashboardYamlGet = Dashboards.dashboardYamlGetH
    , dashboardYamlPut = Dashboards.dashboardYamlPutH
    , -- API handlers
      apiGet = Settings.apiGetH
    , apiDelete = Settings.apiDeleteH
    , apiPatch = Settings.apiActivateH
    , apiPost = Settings.apiPostH
    , -- Chart and widget handlers
      chartsDataGet = Charts.queryMetrics Nothing
    , widgetPost = Widget.widgetPostH
    , widgetGet = widgetGetH
    , widgetSqlPreview = Dashboards.widgetSqlPreviewGetH
    , -- Slack/Discord handlers
      reportsGet = Reports.reportsGetH
    , reportsLiveGet = Reports.reportsLiveGetH
    , reportsSingleGet = Reports.singleReportGetH
    , reportsPost = Reports.reportsPostH
    , shareLinkPost = Share.shareLinkPostH
    , replaySessionGet = Replay.replaySessionGetH
    , bringS3 = Settings.bringS3GetH
    , bringS3Post = Settings.brings3PostH
    , bringS3Remove = Settings.brings3RemoveH
    , gitSyncSettings = GitSync.gitSyncSettingsGetH
    , gitSyncSettingsPost = GitSync.gitSyncSettingsPostH
    , gitSyncSettingsDelete = GitSync.gitSyncSettingsDeleteH
    , githubAppInstall = GitSync.githubAppInstallH
    , githubAppCallback = GitSync.githubAppCallbackH
    , githubAppRepos = GitSync.githubAppReposH
    , githubAppSelectRepo = GitSync.githubAppSelectRepoH
    , -- Billing handlers
      manageBillingGet = Settings.manageBillingGetH
    , -- Endpoint handlers
      endpointListGet = ApiCatalog.endpointListGetH
    , apiCatalogGet = ApiCatalog.apiCatalogH
    , -- Command palette
      commandPaletteGet = CommandPalette.commandPaletteH
    , commandPaletteRecentPost = CommandPalette.commandPaletteRecentPostH
    , -- Device auth
      deviceApprove = Auth.deviceApproveH
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
    { acknowlegeGet = AnomalyList.acknowledgeAnomalyGetH pid
    , unAcknowlegeGet = AnomalyList.unAcknowledgeAnomalyGetH pid
    , archiveGet = AnomalyList.archiveAnomalyGetH pid
    , unarchiveGet = AnomalyList.unArchiveAnomalyGetH pid
    , bulkActionsPost = AnomalyList.anomalyBulkActionsPostH pid
    , listGet = AnomalyList.anomalyListGetH pid
    , anomalyGet = AnomalyList.anomalyDetailGetH pid
    , anomalyHashGet = AnomalyList.anomalyDetailHashGetH pid
    , assignErrorPost = AnomalyList.assignErrorPostH pid
    , resolveErrorPost = AnomalyList.resolveErrorPostH pid
    , errorSubscriptionPost = AnomalyList.errorSubscriptionPostH pid
    , aiChatPost = AnomalyList.aiChatPostH pid
    , aiChatHistoryGet = AnomalyList.aiChatHistoryGetH pid
    , activityGet = AnomalyList.issueActivityGetH pid
    , errorGroupMembersGet = AnomalyList.errorGroupMembersGetH pid
    , errorUnmergePost = AnomalyList.errorUnmergePostH pid
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
    { listGet = Testing.unifiedMonitorsGetH pid
    , overviewGet = Testing.unifiedMonitorOverviewH pid
    , alertUpsertPost = Alerts.alertUpsertPostH pid
    , alertSingleGet = Alerts.alertSingleGetH pid
    , alertSingleToggleActive = Alerts.alertSingleToggleActiveH pid
    , alertMutePost = Testing.alertMuteH pid
    , alertUnmutePost = Testing.alertUnmuteH pid
    , alertResolvePost = Testing.alertResolveH pid
    , alertDeleteRoute = Testing.alertDeleteH pid
    , teamAlertsGetH = Testing.teamAlertsGetH pid
    , alertTeamDeleteH = Alerts.alertTeamDeleteH pid
    , alertBulkAction = Testing.alertBulkActionH pid
    }


-- Projects server
projectsServer :: Servant.ServerT ProjectsRoutes ATAuthCtx
projectsServer =
  ProjectsRoutes'
    { listGet = ListProjects.listProjectsGetH
    , onboardingProject = CreateProject.projectOnboardingH
    , createPost = CreateProject.createProjectPostH
    , settingsGet = CreateProject.projectSettingsGetH
    , integrationGet = Integrations.integrationsSettingsGetH
    , deleteGet = CreateProject.deleteProjectGetH
    , notificationsUpdateChannelPost = Integrations.updateNotificationsChannel
    , pagerdutyConnect = Integrations.pagerdutyConnectH
    , pagerdutyDisconnect = Integrations.pagerdutyDisconnectH
    , slackDisconnect = Integrations.slackDisconnectH
    , notificationsTestPost = Settings.notificationsTestPostH
    , notificationsTestHistoryGet = Settings.notificationsTestHistoryGetH
    , membersManageGet = ManageMembers.manageMembersGetH
    , membersManagePost = ManageMembers.manageMembersPostH
    , membersDeleteH = ManageMembers.deleteMemberH
    , teamsManageGet = ManageMembers.manageTeamsGetH
    , teamsManagePost = ManageMembers.manageTeamPostH
    , teamGet = ManageMembers.teamGetH
    , teamBulkAction = ManageMembers.manageTeamBulkActionH
    , manageSubscriptionGet = ManageMembers.manageSubGetH
    , stripeCheckout = ManageMembers.stripeCheckoutInitH
    , onboading = Onboarding.onboardingGetH
    , onboardingInfoPost = Onboarding.onboardingInfoPostH
    , onboardingConfPost = Onboarding.onboardingConfPostH
    , onboardingPhoneEmailsPost = Onboarding.phoneEmailPostH
    , onboardingIntegrationCheck = Onboarding.checkIntegrationGet
    , onboardingPricingUpdate = CreateProject.pricingUpdateH
    , onboardingDismissChecklist = Onboarding.dismissChecklistH
    , onboardingSkipped = Onboarding.onboardingStepSkipped
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
  versionM <- listToMaybe <$> PG.query_ [sql| select version(); |]
  hash <- maybe "dev" toText <$> lookupEnv "GIT_HASH"
  commitDate <- maybe "dev" toText <$> lookupEnv "GIT_COMMIT_DATE"
  pure Status{dbVersion = versionM <&> \(PGS.Only v) -> v, gitHash = hash, gitCommitDate = commitDate}


pingH :: ATBaseCtx Text
pingH = pure "pong"


avatarGetH :: Projects.UserId -> ATBaseCtx (Headers '[Header "Cache-Control" Text, Header "Content-Type" Text] LBS.ByteString)
avatarGetH userId =
  Projects.userById userId >>= maybe (fetchGravatar "" "?") \user ->
    let email = CI.original user.email
        name = user.firstName <> " " <> user.lastName
     in if T.null user.displayImageUrl
          then fetchGravatar email name
          else fetchImage user.displayImageUrl "image/jpeg" "public, max-age=604800, immutable" (fetchGravatar email name)
  where
    fetchGravatar email name =
      fetchImage
        (mkGravatarUrl email name)
        "image/png"
        "public, max-age=604800"
        (pure $ addImageHeaders "image/png" "public, max-age=60" "")

    fetchImage url ct cache fallback =
      handle
        (\(_ :: SomeException) -> fallback)
        (Wreq.get (toString url) <&> addImageHeaders ct cache . (^. Wreq.responseBody))

    toMd5 msg = decodeUtf8 $ MD5.hash $ encodeUtf8 $ T.toLower msg
    mkGravatarUrl email name = "https://www.gravatar.com/avatar/" <> toMd5 email <> "?d=https%3A%2F%2Fui-avatars.com%2Fapi%2F/" <> T.replace " " "+" name <> "/128"
    addImageHeaders ct cache body = addHeader @"Cache-Control" cache $ addHeader @"Content-Type" ct body


-- Widget GET handler that accepts dashboard parameters
widgetGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Maybe Text)] -> ATAuthCtx (RespHeaders Widget.Widget)
widgetGetH pid widgetJsonM sinceStr fromDStr toDStr allParams = do
  widget <-
    AE.eitherDecode (encodeUtf8 $ fromMaybe "" widgetJsonM)
      & either (\_ -> Error.throwError err400{errBody = "Invalid or missing widgetJSON parameter"}) pure
  now <- Time.currentTime
  let widgetWithPid = widget & #_projectId ?~ pid
      isEager = widgetWithPid.eager == Just True || widgetWithPid.wType `elem` [Widget.WTTable, Widget.WTTraces, Widget.WTStat, Widget.WTAnomalies]
  processedWidget <-
    if isEager
      then Dashboards.processEagerWidget pid now (sinceStr, fromDStr, toDStr) allParams widgetWithPid
      else
        Charts.queryMetrics widgetWithPid.dbSource (Just Charts.DTMetric) (Just pid) widgetWithPid.query widgetWithPid.sql sinceStr fromDStr toDStr Nothing allParams
          <&> \m -> widgetWithPid & #dataset ?~ Widget.toWidgetDataset m
  addRespHeaders processedWidget


-- | Public endpoint for rendering widgets to PNG (for bot embeds). Requires valid HMAC signature.
widgetPngGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> [(Text, Maybe Text)] -> ATBaseCtx (Headers '[Header "Cache-Control" Text, Header "Content-Type" Text] LBS.ByteString)
widgetPngGetH pid widgetJsonM widgetZM sinceStr fromDStr toDStr widthM heightM sigM allParams = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let fallback = fromMaybe "" widgetJsonM
  v <- case widgetZM of
    Just z | Right bs <- B64URL.decodeBase64Untyped (encodeUtf8 z) ->
      handle (\(_ :: SomeException) -> pure fallback) $ liftIO $ evaluateWHNF $ decodeUtf8 @Text $ toStrict $ GZip.decompress $ fromStrict bs
    _ -> pure fallback
  let width = clamp (100, 2000) $ fromMaybe 900 widthM
      height = clamp (100, 2000) $ fromMaybe 300 heightM

  Log.logInfo "widgetPngGetH: request" $ AE.object ["widgetJson_len" AE..= T.length v]
  whenLeft_ (verifyWidgetSignature ctx.env.apiKeyEncryptionSecretKey pid v sigM) \err -> Error.throwError $ err403{errBody = err}

  widget <- either (const $ Error.throwError err400{errBody = "Invalid or missing widgetJSON parameter"}) pure $ AE.eitherDecode (encodeUtf8 v)
  processedWidget <- Dashboards.fetchWidgetData pid (sinceStr, fromDStr, toDStr) allParams $ widget & #_projectId ?~ pid & #eager ?~ True

  let input =
        AE.encode
          $ AE.object
            [ "echarts" AE..= Widget.widgetToECharts (processedWidget & #_staticRender ?~ True)
            , "width" AE..= width
            , "height" AE..= height
            , "theme" AE..= fromMaybe "default" processedWidget.theme
            ]

  pngBytes <-
    liftIO (timeout 30000000 $ readProcess $ setStdin (byteStringInput input) $ proc "./chart-cli" []) >>= \case
      Nothing -> Error.throwError err504{errBody = "Chart rendering timed out"} <* Log.logAttention "widgetPngGetH: chart render timed out" (AE.object ["widgetId" AE..= processedWidget.id, "width" AE..= width, "height" AE..= height])
      Just (ExitSuccess, bytes, _) -> pure bytes
      Just (ExitFailure code, _, errOut) -> Error.throwError err500{errBody = "Chart rendering failed"} <* Log.logAttention "widgetPngGetH: chart render failed" (AE.object ["exitCode" AE..= code, "stderr" AE..= decodeUtf8 @Text (toStrict errOut), "widgetId" AE..= processedWidget.id])

  pure $ addHeader @"Cache-Control" (bool "public, max-age=300" "public, max-age=31536000, immutable" $ isJust fromDStr && isJust toDStr) $ addHeader @"Content-Type" "image/png" pngBytes


-- flamegraph GET handler

-- =============================================================================
-- Email Preview Handlers (dev only)
-- =============================================================================

emailTemplateNames :: [Text]
emailTemplateNames = ["project-invite", "project-created", "project-deleted", "weekly-report", "runtime-errors", "anomaly-endpoint"]


guardDevOnly :: ATBaseCtx ()
guardDevOnly = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  unless (T.toUpper ctx.config.environment == "DEV") $ Error.throwError err404


emailPreviewListH :: ATBaseCtx (Html ())
emailPreviewListH = do
  guardDevOnly
  pure $ ET.emailWrapper "Email Templates" do
    ET.emailBody do
      h1_ "Email Template Previews"
      p_ "Click a template to preview:"
      ul_ $ forM_ emailTemplateNames \name ->
        li_ [style_ "margin: 8px 0;"] $ a_ [href_ $ "/dev/emails/" <> name] $ toHtml name


emailPreviewH :: Text -> ATBaseCtx (Html ())
emailPreviewH templateName = do
  guardDevOnly
  let (subject, content) = case templateName of
        "project-invite" -> ET.sampleProjectInvite
        "project-created" -> ET.sampleProjectCreated
        "project-deleted" -> ET.sampleProjectDeleted
        "weekly-report" -> ET.sampleWeeklyReport "https://placehold.co/600x200?text=Events+Chart" "https://placehold.co/600x200?text=Errors+Chart"
        "runtime-errors" -> ET.sampleRuntimeErrors
        "anomaly-endpoint" -> ET.sampleAnomalyEndpoint
        "issue-assigned" -> ET.sampleIssueAssigned
        _ -> ("Unknown", p_ "Template not found")
  pure $ ET.emailWrapper subject content


-- =============================================================================
-- Authentication and error handling
-- =============================================================================

-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: Logger -> AuthContext -> Servant.Context '[APItoolkitAuthContext, ApiKeyAuthContext, Servant.ErrorFormatters]
genAuthServerContext logger env = authHandler logger env :. apiKeyAuthHandler logger env :. errorFormatters env :. EmptyContext


errorFormatters :: AuthContext -> Servant.ErrorFormatters
errorFormatters env =
  Servant.defaultErrorFormatters
    { Servant.notFoundErrorFormatter = \_ -> htmlServerError env.config 404
    , Servant.bodyParserErrorFormatter = \_ _ _ -> htmlServerError env.config 400
    , Servant.urlParseErrorFormatter = \_ _ _ -> htmlServerError env.config 400
    }


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
