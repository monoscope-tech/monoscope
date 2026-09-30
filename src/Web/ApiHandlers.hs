{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | JSON REST handlers for the public API (Plan A).
--
-- Runs in 'ATBaseCtx' because API-key auth supplies the 'ProjectId' directly
-- (no session). Thin adapters over the existing 'Models.*' domain functions
-- so the HTML handlers (which still own toast/redirect/HTML-rendering concerns)
-- stay untouched.
module Web.ApiHandlers (
  ApiV1Routes (..),
  AllQueryParams,
  apiV1OpenApiSpec,
  apiV1Server,
  -- Monitors
  apiMonitorsList,
  apiMonitorGet,
  apiMonitorCreate,
  apiMonitorApply,
  apiMonitorYaml,
  apiMonitorUpdate,
  apiMonitorPatch,
  apiMonitorDelete,
  apiMonitorToggleActive,
  apiMonitorMute,
  apiMonitorUnmute,
  apiMonitorResolve,
  apiMonitorBulk,
  -- Dashboards
  apiDashboardsList,
  apiDashboardGet,
  apiDashboardCreate,
  apiDashboardApply,
  apiDashboardUpdate,
  apiDashboardPatch,
  apiDashboardDelete,
  apiDashboardDuplicate,
  apiDashboardStar,
  apiDashboardUnstar,
  apiDashboardYaml,
  apiDashboardWidgetUpsert,
  apiDashboardWidgetDelete,
  apiDashboardWidgetsReorder,
  apiDashboardBulk,
  -- API keys
  apiKeysList,
  apiKeyGet,
  apiKeyCreate,
  apiKeyActivate,
  apiKeyDeactivate,
  apiKeyDelete,
  -- Events query (body variant)
  apiEventsQuery,
  -- Share links
  apiShareLinkCreate,
  ShareLinkCreate (..),
  ShareLinkCreated (..),
  -- Plan B
  apiMe,
  apiProjectGet,
  apiProjectPatch,
  apiEndpointsList,
  apiEndpointGet,
  apiLogPatternsList,
  apiLogPatternGet,
  apiLogPatternAck,
  apiLogPatternsBulk,
  apiIssuesList,
  apiIssueGet,
  apiIssueAck,
  apiIssueUnack,
  apiIssueArchive,
  apiIssueUnarchive,
  apiIssuesBulk,
  apiIncidentsList,
  apiIncidentGet,
  -- Teams (B3)
  apiTeamsList,
  apiTeamGet,
  apiTeamCreate,
  apiTeamUpdate,
  apiTeamPatch,
  apiTeamDelete,
  apiTeamsBulk,
  -- Members (B4)
  apiMembersList,
  apiMemberGet,
  apiMemberAdd,
  apiMemberPatch,
  apiMemberRemove,
  -- Facets
  apiFacets,
  apiMetricsCatalog,
  -- Direct event lookup by (id, timestamp)
  apiEventGet,
  -- Internals exposed for testing
  synthStackFromSpans,
) where

import Control.Lens ((%~), (.~), (?~), _Just)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.UUID qualified as UUID
import Data.Generics.Labels ()
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.OpenApi (OpenApi, SecurityDefinitions (..), SecurityScheme (..), SecuritySchemeType (..), ToSchema (..))
import Data.OpenApi qualified as OA
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime, addUTCTime, nominalDay, zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Deriving.Aeson.Stock qualified as DAE
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import GHC.Records (HasField)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Incidents qualified as Incidents
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.SchemaCatalog qualified as SchemaCatalog
import Models.Apis.ShareEvents qualified as ShareEvents
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers qualified as PM
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.Wai (Request, queryString)
import Pages.Charts.Charts qualified as ChartsPage
import Pages.Charts.Types qualified as Charts
import Pages.Replay qualified as Replay
import Pkg.Components.TimePicker qualified as TP
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (SnakeSchema (..), UUIDId (..))
import Pkg.Parser qualified as Parser
import Pkg.Parser.Expr qualified as ParserExpr
import Pkg.SchemaLearning.Catalog qualified as Fields
import Relude hiding (ask, id)
import Servant (Capture, Delete, Get, HasServer (..), JSON, NamedRoutes, NoContent (..), Patch, Post, Put, QueryParam, ReqBody, ServerError (..), err400, err404, (:-), (:>))
import Servant.OpenApi (HasOpenApi (..), toOpenApi)
import Servant.Server.Internal.Delayed (passToServer)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATBaseCtx, useTfReads)
import Utils (hostPath)
import Web.ApiTypes hiding (count)
import Web.ApiTypes qualified as ApiT
import Web.FacetsFallback (facetsFallback)


type QPT a = QueryParam a Text


data AllQueryParams


-- =============================================================================
-- Public API v1 Routes
-- =============================================================================

type ApiV1Routes :: Type -> Type
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
          :> QueryParam "with_children" Bool
          :> QueryParam "include_attributes" Bool
          :> QPT "environment"
          :> QPT "service"
          :> Get '[JSON] LogResult
  , eventGet
      :: mode
        :- "events"
          :> Capture "event_id" UUID.UUID
          :> "time"
          :> Capture "timestamp" UTCTime
          :> Get '[JSON] AE.Value
  , metricsQuery
      :: mode
        :- "metrics"
          :> QueryParam "query" Text
          :> QueryParam "data_type" Charts.DataType
          :> QPT "since"
          :> QPT "from"
          :> QPT "to"
          :> QPT "source"
          :> QPT "environment"
          :> QPT "service"
          :> Get '[JSON] Charts.MetricsData
  , metricsCatalog
      :: mode
        :- "metrics"
          :> "catalog"
          :> QPT "service"
          :> QPT "search"
          :> QueryParam "limit" Int
          :> QueryParam "offset" Int
          :> QueryParam "active" Bool
          :> Get '[JSON] Telemetry.MetricCatalogResponse
  , schemaGet :: mode :- "schema" :> Get '[JSON] Schema.Schema
  , facetsGet
      :: mode
        :- "facets"
          :> QPT "since"
          :> QPT "from"
          :> QPT "to"
          :> QPT "field"
          :> Get '[JSON] AE.Value
  , rrwebPost :: mode :- "rrweb" :> ReqBody '[JSON] Replay.ReplayPost :> Post '[JSON] AE.Value
  , -- Monitors (CRUD + lifecycle)
    monitorsList :: mode :- "monitors" :> Get '[JSON] [Monitors.QueryMonitor]
  , monitorGet :: mode :- "monitors" :> Capture "monitor_id" Monitors.QueryMonitorId :> Get '[JSON] Monitors.QueryMonitor
  , monitorCreate :: mode :- "monitors" :> ReqBody '[JSON] ApiT.MonitorInput :> Post '[JSON] Monitors.QueryMonitor
  , monitorApply :: mode :- "monitors" :> "apply" :> ReqBody '[JSON] ApiT.MonitorInput :> Post '[JSON] Monitors.QueryMonitor
  , monitorYaml :: mode :- "monitors" :> Capture "monitor_id" Monitors.QueryMonitorId :> "yaml" :> Get '[JSON] ApiT.MonitorInput
  -- ^ /yaml returns the applyable MonitorInput as JSON (same for dashboards);
  -- the CLI renders it as YAML client-side via runYamlDump.
  , monitorUpdate :: mode :- "monitors" :> Capture "monitor_id" Monitors.QueryMonitorId :> ReqBody '[JSON] ApiT.MonitorInput :> Put '[JSON] Monitors.QueryMonitor
  , monitorPatch :: mode :- "monitors" :> Capture "monitor_id" Monitors.QueryMonitorId :> ReqBody '[JSON] ApiT.MonitorPatch :> Patch '[JSON] Monitors.QueryMonitor
  , monitorDelete :: mode :- "monitors" :> Capture "monitor_id" Monitors.QueryMonitorId :> Delete '[JSON] NoContent
  , monitorToggleActive :: mode :- "monitors" :> Capture "monitor_id" Monitors.QueryMonitorId :> "toggle_active" :> Post '[JSON] Monitors.QueryMonitor
  , monitorMute :: mode :- "monitors" :> Capture "monitor_id" Monitors.QueryMonitorId :> "mute" :> QueryParam "duration_minutes" Int :> Post '[JSON] Monitors.QueryMonitor
  , monitorUnmute :: mode :- "monitors" :> Capture "monitor_id" Monitors.QueryMonitorId :> "unmute" :> Post '[JSON] Monitors.QueryMonitor
  , monitorResolve :: mode :- "monitors" :> Capture "monitor_id" Monitors.QueryMonitorId :> "resolve" :> Post '[JSON] Monitors.QueryMonitor
  , monitorBulk :: mode :- "monitors" :> "bulk" :> ReqBody '[JSON] (ApiT.BulkAction UUID.UUID) :> Post '[JSON] (ApiT.BulkResult UUID.UUID)
  , -- Events body variant (avoids URL length limits)
    eventsQuery :: mode :- "events" :> "query" :> ReqBody '[JSON] ApiT.EventsQuery :> Post '[JSON] LogResult
  , -- Share links
    shareLinkCreate :: mode :- "share" :> ReqBody '[JSON] ShareLinkCreate :> Post '[JSON] ShareLinkCreated
  , -- Dashboards (CRUD + IaC apply + widgets)
    dashboardsList
      :: mode
        :- "dashboards"
          :> QPT "sort"
          :> QPT "team_id"
          :> Get '[JSON] [ApiT.DashboardSummary]
  , dashboardGet :: mode :- "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> Get '[JSON] ApiT.DashboardFull
  , -- Widget data resolved server-side, for clients that can't run the widget JS
    -- (the CLI renders these straight into the terminal).
    dashboardData
      :: mode
        :- "dashboards"
          :> Capture "dashboard_id" Dashboards.DashboardId
          :> "data"
          :> QPT "tab"
          :> QPT "widget"
          :> QPT "since"
          :> QPT "from"
          :> QPT "to"
          :> AllQueryParams
          :> Get '[JSON] ApiT.DashboardData
  , dashboardCreate :: mode :- "dashboards" :> ReqBody '[JSON] ApiT.DashboardInput :> Post '[JSON] ApiT.DashboardFull
  , dashboardApply :: mode :- "dashboards" :> "apply" :> ReqBody '[JSON] ApiT.DashboardYAMLDoc :> Post '[JSON] ApiT.DashboardFull
  , dashboardUpdate :: mode :- "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> ReqBody '[JSON] ApiT.DashboardInput :> Put '[JSON] ApiT.DashboardFull
  , dashboardPatch :: mode :- "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> ReqBody '[JSON] ApiT.DashboardPatch :> Patch '[JSON] ApiT.DashboardFull
  , dashboardDelete :: mode :- "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> Delete '[JSON] NoContent
  , dashboardDuplicate :: mode :- "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "duplicate" :> Post '[JSON] ApiT.DashboardFull
  , dashboardStar :: mode :- "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "star" :> Post '[JSON] ApiT.DashboardFull
  , dashboardUnstar :: mode :- "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "star" :> Delete '[JSON] NoContent
  , dashboardYaml :: mode :- "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "yaml" :> Get '[JSON] ApiT.DashboardYAMLDoc
  , dashboardWidgetUpsert :: mode :- "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "widgets" :> ReqBody '[JSON] Widget.Widget :> Put '[JSON] Widget.Widget
  , dashboardWidgetDelete :: mode :- "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "widgets" :> Capture "widget_id" Text :> Delete '[JSON] NoContent
  , dashboardWidgetsReorder :: mode :- "dashboards" :> Capture "dashboard_id" Dashboards.DashboardId :> "widgets" :> "order" :> ReqBody '[JSON] (Map Text ApiT.WidgetPosition) :> Patch '[JSON] NoContent
  , dashboardBulk :: mode :- "dashboards" :> "bulk" :> ReqBody '[JSON] (ApiT.BulkAction UUID.UUID) :> Post '[JSON] (ApiT.BulkResult UUID.UUID)
  , -- API keys
    apiKeysList :: mode :- "api_keys" :> Get '[JSON] [ApiT.ApiKeySummary]
  , apiKeyGet :: mode :- "api_keys" :> Capture "key_id" ProjectApiKeys.ProjectApiKeyId :> Get '[JSON] ApiT.ApiKeySummary
  , apiKeyCreate :: mode :- "api_keys" :> ReqBody '[JSON] ApiT.ApiKeyCreate :> Post '[JSON] ApiT.ApiKeyCreated
  , apiKeyActivate :: mode :- "api_keys" :> Capture "key_id" ProjectApiKeys.ProjectApiKeyId :> "activate" :> Post '[JSON] ApiT.ApiKeySummary
  , apiKeyDeactivate :: mode :- "api_keys" :> Capture "key_id" ProjectApiKeys.ProjectApiKeyId :> "deactivate" :> Post '[JSON] ApiT.ApiKeySummary
  , apiKeyDelete :: mode :- "api_keys" :> Capture "key_id" ProjectApiKeys.ProjectApiKeyId :> Delete '[JSON] NoContent
  , -- Plan B: /me + /project (singular) + /issues + /endpoints + /log_patterns
    meGet :: mode :- "me" :> Get '[JSON] ApiT.MeResponse
  , projectGet :: mode :- "project" :> Get '[JSON] ApiT.ProjectFull
  , projectPatch :: mode :- "project" :> ReqBody '[JSON] Projects.ProjectPatch :> Patch '[JSON] ApiT.ProjectFull
  , endpointsList
      :: mode
        :- "endpoints"
          :> QPT "search"
          :> QueryParam "outgoing" Bool
          :> QueryParam "page" Int
          :> QueryParam "per_page" Int
          :> Get '[JSON] (ApiT.Paged ApiT.EndpointSummary)
  , endpointGet :: mode :- "endpoints" :> Capture "endpoint_id" Endpoints.EndpointId :> Get '[JSON] ApiT.EndpointFull
  , logPatternsList
      :: mode
        :- "log_patterns"
          :> QueryParam "page" Int
          :> QueryParam "per_page" Int
          :> Get '[JSON] (ApiT.Paged ApiT.LogPatternSummary)
  , logPatternGet :: mode :- "log_patterns" :> Capture "pattern_id" Int64 :> Get '[JSON] ApiT.LogPatternFull
  , logPatternAck :: mode :- "log_patterns" :> Capture "pattern_id" Int64 :> "ack" :> Post '[JSON] ApiT.LogPatternFull
  , logPatternsBulk :: mode :- "log_patterns" :> "bulk" :> ReqBody '[JSON] (ApiT.BulkAction Int64) :> Post '[JSON] (ApiT.BulkResult Int64)
  , -- Issues
    issuesList
      :: mode
        :- "issues"
          :> QueryParam "status" ApiT.IssueStatus
          :> QPT "type"
          :> QPT "service"
          :> QueryParam "page" Int
          :> QueryParam "per_page" Int
          :> Get '[JSON] (ApiT.Paged ApiT.IssueApiSummary)
  , issueGet :: mode :- "issues" :> Capture "issue_id" Issues.IssueId :> Get '[JSON] ApiT.IssueApiFull
  , issueAck :: mode :- "issues" :> Capture "issue_id" Issues.IssueId :> "ack" :> QueryParam "duration_minutes" Int :> Post '[JSON] ApiT.IssueApiFull
  , issueUnack :: mode :- "issues" :> Capture "issue_id" Issues.IssueId :> "unack" :> Post '[JSON] ApiT.IssueApiFull
  , issueArchive :: mode :- "issues" :> Capture "issue_id" Issues.IssueId :> "archive" :> Post '[JSON] ApiT.IssueApiFull
  , issueUnarchive :: mode :- "issues" :> Capture "issue_id" Issues.IssueId :> "unarchive" :> Post '[JSON] ApiT.IssueApiFull
  , issuesBulk :: mode :- "issues" :> "bulk" :> ReqBody '[JSON] (ApiT.BulkAction Issues.IssueId) :> Post '[JSON] (ApiT.BulkResult Issues.IssueId)
  , incidentsList :: mode :- "incidents" :> QueryParam "phase" Incidents.EpisodePhase :> QueryParam "limit" Int :> Get '[JSON] [ApiT.IncidentSummary]
  , incidentGet :: mode :- "incidents" :> Capture "incident_id" Incidents.EpisodeId :> Get '[JSON] ApiT.IncidentSummary
  , -- Teams (B3) + Members (B4)
    teamsList :: mode :- "teams" :> Get '[JSON] [ApiT.TeamSummary]
  , teamGet :: mode :- "teams" :> Capture "team_id" ApiT.TeamId :> Get '[JSON] ApiT.TeamFull
  , teamCreate :: mode :- "teams" :> ReqBody '[JSON] ApiT.TeamInput :> Post '[JSON] ApiT.TeamFull
  , teamUpdate :: mode :- "teams" :> Capture "team_id" ApiT.TeamId :> ReqBody '[JSON] ApiT.TeamInput :> Put '[JSON] ApiT.TeamFull
  , teamPatch :: mode :- "teams" :> Capture "team_id" ApiT.TeamId :> ReqBody '[JSON] ApiT.TeamPatch :> Patch '[JSON] ApiT.TeamFull
  , teamDelete :: mode :- "teams" :> Capture "team_id" ApiT.TeamId :> Delete '[JSON] NoContent
  , teamsBulk :: mode :- "teams" :> "bulk" :> ReqBody '[JSON] (ApiT.BulkAction ApiT.TeamId) :> Post '[JSON] (ApiT.BulkResult ApiT.TeamId)
  , membersList :: mode :- "members" :> Get '[JSON] [ApiT.MemberSummary]
  , memberGet :: mode :- "members" :> Capture "user_id" UUID.UUID :> Get '[JSON] ApiT.MemberSummary
  , memberAdd :: mode :- "members" :> ReqBody '[JSON] ApiT.MemberAdd :> Post '[JSON] ApiT.MemberSummary
  , memberPatch :: mode :- "members" :> Capture "user_id" UUID.UUID :> ReqBody '[JSON] ApiT.MemberPatch :> Patch '[JSON] ApiT.MemberSummary
  , memberRemove :: mode :- "members" :> Capture "user_id" UUID.UUID :> Delete '[JSON] NoContent
  , -- Model Context Protocol endpoint (JSON-RPC; tools derived from this OpenAPI spec)
    mcp :: mode :- "mcp" :> ReqBody '[JSON] AE.Value :> Post '[JSON] AE.Value
  }
  deriving stock (Generic)


apiV1OpenApiSpec :: OpenApi
apiV1OpenApiSpec =
  toOpenApi (Proxy @(NamedRoutes ApiV1Routes))
    & OA.info
    .~ (mempty & OA.title .~ "Monoscope API" & OA.version .~ "1.0" & OA.description ?~ "Observability API for querying logs, traces, and metrics. Requires a Bearer API key and X-Project-Id header.")
      & OA.servers
    .~ [OA.Server "/api/v1" Nothing mempty]
      & OA.components
      . OA.securitySchemes
    .~ SecurityDefinitions (fromList [("BearerAuth", SecurityScheme (SecuritySchemeApiKey (OA.ApiKeyParams "Authorization" OA.ApiKeyHeader)) Nothing)])
      & OA.security
    .~ [OA.SecurityRequirement (fromList [("BearerAuth", [])])]


-- =============================================================================
-- Custom HasServer instances
-- =============================================================================

-- | We add a HasServer instance that says:
--   - The handler will receive a list of (Text, Maybe Text).
--   - We pull that out of the WAI `Request` in `route`.
instance HasServer api ctx => HasServer (AllQueryParams :> api) ctx where
  type ServerT (AllQueryParams :> api) m = [(Text, Maybe Text)] -> ServerT api m


  route _ ctx subserver = route (Proxy :: Proxy api) ctx $ passToServer subserver grabAllParams
    where
      grabAllParams :: Request -> [(Text, Maybe Text)]
      grabAllParams = map (bimap dec (fmap dec)) . queryString
      dec = decodeUtf8With lenientDecode


  hoistServerWithContext
    :: Proxy (AllQueryParams :> api)
    -> Proxy ctx
    -> (forall x. m x -> n x)
    -> ([(Text, Maybe Text)] -> ServerT api m)
    -> ([(Text, Maybe Text)] -> ServerT api n)
  hoistServerWithContext _ pc nat s = hoistServerWithContext (Proxy :: Proxy api) pc nat . s


-- | @AllQueryParams@ is a catch-all: it grabs whatever query string arrives, so
-- there is nothing specific to advertise. Documenting it as the identity keeps
-- routes that use it (currently @/dashboards/{id}/data@, which forwards
-- @var-*@/@const-*@ dashboard variables) inside the generated OpenAPI spec
-- instead of forcing them out of it.
instance HasOpenApi api => HasOpenApi (AllQueryParams :> api) where
  toOpenApi _ = toOpenApi (Proxy @api)


type EventsSearchHandler = Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> ATBaseCtx LogResult


type DashboardDataHandler = Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Maybe Text)] -> ATBaseCtx DashboardData


-- API v1 server
apiV1Server :: Projects.ProjectId -> EventsSearchHandler -> DashboardDataHandler -> (AE.Value -> ATBaseCtx AE.Value) -> ServerT (NamedRoutes ApiV1Routes) ATBaseCtx
apiV1Server pid queryEvents dashboardRenderer mcpHandler =
  ApiV1Routes
    { eventsSearch = queryEvents pid
    , eventGet = apiEventGet pid
    , -- API clients have no browser scope cookie. Omitting either boundary is the
      -- intentional cross-environment/service default; callers that need a scope
      -- state it explicitly and receive the same generated predicate as a chart.
      metricsQuery = \queryM dataTypeM sinceM fromM toM sourceM environmentM serviceM ->
        ChartsPage.queryMetrics Nothing dataTypeM (Just pid) queryM Nothing sinceM fromM toM sourceM Nothing [("environment", environmentM), ("service", serviceM)]
    , metricsCatalog = apiMetricsCatalog pid
    , -- C1: derive schema from the live introspected column set (seeded at
      -- startup) and decorate with the hand-coded descriptions / examples in
      -- 'Schema.telemetrySchema'. Live entries without a decoration get a
      -- bare {type:"text", description:"", examples:null} entry — so a new
      -- column added to otel_logs_and_spans is queryable + visible in the
      -- schema response without a code edit.
      schemaGet = pure (Schema.deriveSchema ParserExpr.flattenedOtelAttributes)
    , facetsGet = apiFacets pid
    , rrwebPost = Replay.replayPostH pid
    , -- Monitors
      monitorsList = apiMonitorsList pid
    , monitorGet = apiMonitorGet pid
    , monitorCreate = apiMonitorCreate pid
    , monitorApply = apiMonitorApply pid
    , monitorYaml = apiMonitorYaml pid
    , monitorUpdate = apiMonitorUpdate pid
    , monitorPatch = apiMonitorPatch pid
    , monitorDelete = apiMonitorDelete pid
    , monitorToggleActive = apiMonitorToggleActive pid
    , monitorMute = apiMonitorMute pid
    , monitorUnmute = apiMonitorUnmute pid
    , monitorResolve = apiMonitorResolve pid
    , monitorBulk = apiMonitorBulk pid
    , eventsQuery = apiEventsQuery queryEvents pid
    , shareLinkCreate = apiShareLinkCreate pid
    , -- Dashboards
      dashboardsList = apiDashboardsList pid
    , dashboardGet = apiDashboardGet pid
    , dashboardData = dashboardRenderer pid
    , dashboardCreate = apiDashboardCreate pid
    , dashboardApply = apiDashboardApply pid
    , dashboardUpdate = apiDashboardUpdate pid
    , dashboardPatch = apiDashboardPatch pid
    , dashboardDelete = apiDashboardDelete pid
    , dashboardDuplicate = apiDashboardDuplicate pid
    , dashboardStar = apiDashboardStar pid
    , dashboardUnstar = apiDashboardUnstar pid
    , dashboardYaml = apiDashboardYaml pid
    , dashboardWidgetUpsert = apiDashboardWidgetUpsert pid
    , dashboardWidgetDelete = apiDashboardWidgetDelete pid
    , dashboardWidgetsReorder = apiDashboardWidgetsReorder pid
    , dashboardBulk = apiDashboardBulk pid
    , -- API keys
      apiKeysList = apiKeysList pid
    , apiKeyGet = apiKeyGet pid
    , apiKeyCreate = apiKeyCreate pid
    , apiKeyActivate = apiKeyActivate pid
    , apiKeyDeactivate = apiKeyDeactivate pid
    , apiKeyDelete = apiKeyDelete pid
    , -- Plan B
      meGet = apiMe pid
    , projectGet = apiProjectGet pid
    , projectPatch = apiProjectPatch pid
    , endpointsList = apiEndpointsList pid
    , endpointGet = apiEndpointGet pid
    , logPatternsList = apiLogPatternsList pid
    , logPatternGet = apiLogPatternGet pid
    , logPatternAck = apiLogPatternAck pid
    , logPatternsBulk = apiLogPatternsBulk pid
    , -- Issues
      issuesList = apiIssuesList pid
    , issueGet = apiIssueGet pid
    , issueAck = apiIssueAck pid
    , issueUnack = apiIssueUnack pid
    , issueArchive = apiIssueArchive pid
    , issueUnarchive = apiIssueUnarchive pid
    , issuesBulk = apiIssuesBulk pid
    , incidentsList = apiIncidentsList pid
    , incidentGet = apiIncidentGet pid
    , -- Teams + Members
      teamsList = apiTeamsList pid
    , teamGet = apiTeamGet pid
    , teamCreate = apiTeamCreate pid
    , teamUpdate = apiTeamUpdate pid
    , teamPatch = apiTeamPatch pid
    , teamDelete = apiTeamDelete pid
    , teamsBulk = apiTeamsBulk pid
    , membersList = apiMembersList pid
    , memberGet = apiMemberGet pid
    , memberAdd = apiMemberAdd pid
    , memberPatch = apiMemberPatch pid
    , memberRemove = apiMemberRemove pid
    , mcp = mcpHandler
    }


-- | Return the value or throw a 404 with a given message.
notFoundOr :: Text -> Maybe a -> ATBaseCtx a
notFoundOr msg = maybe (throwError err404{errBody = encodeUtf8 msg}) pure


-- | 404 with @msg@ unless the row exists /and/ belongs to @pid@.
ownedOr :: HasField "projectId" a Projects.ProjectId => Text -> Projects.ProjectId -> Maybe a -> ATBaseCtx a
ownedOr msg pid = notFoundOr msg . (>>= guarded ((pid ==) . (.projectId)))


-- | Result an op reports back to 'bulkExec': an affected-row count
-- (all-or-nothing across @ba.ids@), the explicit list of succeeded ids (the
-- complement of @ba.ids@ is reported as "not applied"), or a full split when
-- the op knows a per-id failure reason.
data BulkOpResult a = BulkCount Int64 | BulkSucceeded [a] | BulkPartial [a] [BulkFailure a]


-- | Lift a count-returning op into a 'BulkOpResult'.
count :: Functor f => f Int64 -> f (BulkOpResult a)
count = fmap BulkCount


-- | Dispatch a 'BulkAction' against a table of named operations. A 'BulkCount'
-- op marks every id succeeded when the count is @>0@, else all "not applied";
-- a 'BulkSucceeded' op reports per-id partial success and a 'BulkPartial' op
-- carries its own failure reasons.
-- Ops that cannot report a count can @pure (BulkCount (length ba.ids))@.
bulkExec :: Eq a => BulkAction a -> [(Text, ATBaseCtx (BulkOpResult a))] -> ATBaseCtx (BulkResult a)
bulkExec ba ops = do
  r <- fromMaybe (throwError err400{errBody = encodeUtf8 $ "Unknown bulk action: " <> ba.action}) $ List.lookup (T.toLower ba.action) ops
  let mkResult ok = BulkResult{succeeded = ok, failed = [BulkFailure{id = i, error = "not applied"} | i <- ba.ids, i `notElem` ok]}
  pure $ case r of
    BulkCount n -> mkResult $ bool [] ba.ids (n > 0)
    BulkSucceeded ok -> mkResult ok
    BulkPartial ok failed -> BulkResult{succeeded = ok, failed}


-- | Paginated list helper: normalises page/per_page with caps, runs the
-- @(perPage -> offset -> (items, total))@ fetch and builds the envelope.
paged :: Maybe Int -> Maybe Int -> Int -> (Int -> Int -> ATBaseCtx ([a], Int)) -> ATBaseCtx (Paged a)
paged pageM perPageM capPerPage fetch = do
  let page = max 0 (fromMaybe 0 pageM)
      perPage = min capPerPage $ max 1 (fromMaybe 50 perPageM)
      offset = page * perPage
  (items, total) <- fetch perPage offset
  pure Paged{items, totalCount = total, page, perPage, hasMore = offset + length items < total}


apiMonitorsList :: Projects.ProjectId -> ATBaseCtx [Monitors.QueryMonitor]
apiMonitorsList = Monitors.queryMonitorsAll


apiMonitorGet :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATBaseCtx Monitors.QueryMonitor
apiMonitorGet pid mid = ownedOr "Monitor not found" pid =<< Monitors.queryMonitorById mid


-- | The compiled form of a monitor's KQL, which is what alert evaluation actually
-- runs. Shared by the create/PUT path and by PATCH: PATCH used to build its merged
-- monitor from the stored row, so it updated @log_query@ and inherited the previous
-- @log_query_as_sql@ — the monitor kept alerting on its old query. Falls back to
-- @prev@ when the query doesn't parse, so a bad edit cannot blank the compiled SQL.
compileAlertSql :: Projects.ProjectId -> Int -> Maybe Text -> Maybe Text -> Text -> Text -> Text
compileAlertSql pid windowMins environment service q prev =
  let scope = Parser.mkScopedQuery pid (Nothing, Nothing) environment service
      cfg = (Parser.applyScopedQuery scope $ Parser.defSqlQueryCfg pid Parser.fixedUTCTime Nothing Nothing){Parser.alertLookbackMins = windowMins}
   in fromMaybe prev $ (.finalAlertQuery) . snd =<< rightToMaybe (Parser.parseQueryToComponents cfg q)


-- | Build a 'QueryMonitor' from a 'MonitorInput'. Shared by create/update.
monitorFromInput :: Projects.ProjectId -> UTCTime -> Monitors.QueryMonitorId -> Maybe Monitors.QueryMonitor -> MonitorInput -> Monitors.QueryMonitor
monitorFromInput pid now mid existingM inp =
  Monitors.QueryMonitor
    { id = mid
    , projectId = pid
    , createdAt = maybe now (.createdAt) existingM
    , updatedAt = now
    , checkIntervalMins = inp.checkIntervalMins
    , alertThreshold = inp.alertThreshold
    , warningThreshold = inp.warningThreshold
    , logQuery = inp.query
    , logQueryAsSql = compileAlertSql pid inp.timeWindowMins inp.environment inp.service inp.query (foldMap (.logQueryAsSql) existingM)
    , lastEvaluated = Just now
    , warningLastTriggered = existingM >>= (.warningLastTriggered)
    , alertLastTriggered = existingM >>= (.alertLastTriggered)
    , triggerLessThan = inp.triggerLessThan
    , thresholdSustainedForMins = fromMaybe 0 inp.thresholdSustainedForMins
    , alertConfig =
        Monitors.MonitorAlertConfig
          { unit = mfilter (not . T.null) $ T.strip <$> inp.unit
          , title = inp.title
          , severity = fromMaybe "error" inp.severity
          , subject = fromMaybe inp.title inp.subject
          , message = fromMaybe "" inp.message
          , emails = V.fromList $ CI.mk <$> inp.emails
          , emailAll = fromMaybe False inp.emailAll
          , slackChannels = V.fromList inp.slackChannels
          }
    , deactivatedAt = bool (Just now) Nothing (fromMaybe True inp.active)
    , deletedAt = Nothing
    , visualizationType = fromMaybe "timeseries" inp.visualizationType
    , teams = V.fromList inp.teams
    , widgetId = existingM >>= (.widgetId)
    , dashboardId = existingM >>= (.dashboardId)
    , alertRecoveryThreshold = inp.alertRecoveryThreshold
    , warningRecoveryThreshold = inp.warningRecoveryThreshold
    , currentStatus = maybe Monitors.MSNormal (.currentStatus) existingM
    , currentValue = maybe 0 (.currentValue) existingM
    , mutedUntil = existingM >>= (.mutedUntil)
    , renotifyIntervalMins = inp.notifyAfterMins
    , stopAfterCount = inp.stopAfterCount
    , notificationCount = maybe 0 (.notificationCount) existingM
    , timeWindowMins = inp.timeWindowMins
    , environment = inp.environment
    , service = inp.service
    }


-- | Persist a monitor built from @inp@; @existingM@ carries over the fields an
-- input can't supply (createdAt, trigger history, status).
saveMonitor :: Projects.ProjectId -> Monitors.QueryMonitorId -> Maybe Monitors.QueryMonitor -> MonitorInput -> ATBaseCtx Monitors.QueryMonitor
saveMonitor pid mid existingM inp = do
  when (inp.timeWindowMins <= 0) $ throwError err400{errBody = "time_window_mins must be positive"}
  now <- Time.currentTime
  let mon = monitorFromInput pid now mid existingM inp
  mon <$ Monitors.queryMonitorUpsert mon


apiMonitorCreate :: Projects.ProjectId -> MonitorInput -> ATBaseCtx Monitors.QueryMonitor
apiMonitorCreate pid inp = UUID.genUUID >>= \uuid -> saveMonitor pid (Monitors.QueryMonitorId uuid) Nothing inp


apiMonitorUpdate :: Projects.ProjectId -> Monitors.QueryMonitorId -> MonitorInput -> ATBaseCtx Monitors.QueryMonitor
apiMonitorUpdate pid mid inp = apiMonitorGet pid mid >>= \existing -> saveMonitor pid mid (Just existing) inp


-- | Upsert keyed by alert title — the monitors-as-code analogue of
-- 'apiDashboardApply' (dashboards key on file_path; monitors on title).
-- Lookup+insert is racy under concurrent same-title applies (no unique index
-- on title to hang ON CONFLICT off); accepted for this human/CI-driven path —
-- the loser just creates a duplicate to clean up.
apiMonitorApply :: Projects.ProjectId -> MonitorInput -> ATBaseCtx Monitors.QueryMonitor
apiMonitorApply pid inp =
  Monitors.queryMonitorByTitle pid inp.title >>= \case
    Just existing -> apiMonitorUpdate pid existing.id inp
    Nothing -> apiMonitorCreate pid inp


-- | Inverse of 'monitorFromInput': dump a monitor as the applyable input
-- shape, so `monitors yaml ID > m.yaml && monitors apply m.yaml` round-trips.
apiMonitorYaml :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATBaseCtx MonitorInput
apiMonitorYaml pid mid = do
  m <- apiMonitorGet pid mid
  pure
    MonitorInput
      { unit = m.alertConfig.unit
      , title = m.alertConfig.title
      , query = m.logQuery
      , severity = Just m.alertConfig.severity
      , subject = Just m.alertConfig.subject
      , message = Just m.alertConfig.message
      , alertThreshold = m.alertThreshold
      , warningThreshold = m.warningThreshold
      , triggerLessThan = m.triggerLessThan
      , checkIntervalMins = m.checkIntervalMins
      , timeWindowMins = m.timeWindowMins
      , thresholdSustainedForMins = Just m.thresholdSustainedForMins
      , notifyAfterMins = m.renotifyIntervalMins
      , stopAfterCount = m.stopAfterCount
      , emails = V.toList (CI.original <$> m.alertConfig.emails)
      , emailAll = Just m.alertConfig.emailAll
      , slackChannels = V.toList m.alertConfig.slackChannels
      , teams = V.toList m.teams
      , visualizationType = Just m.visualizationType
      , alertRecoveryThreshold = m.alertRecoveryThreshold
      , warningRecoveryThreshold = m.warningRecoveryThreshold
      , active = Just (isNothing m.deactivatedAt)
      , environment = m.environment
      , service = m.service
      }


-- | PATCH — merge fields into existing monitor.
apiMonitorPatch :: Projects.ProjectId -> Monitors.QueryMonitorId -> MonitorPatch -> ATBaseCtx Monitors.QueryMonitor
apiMonitorPatch pid mid patch = do
  when (maybe False (<= 0) patch.timeWindowMins) $ throwError err400{errBody = "time_window_mins must be positive"}
  existing <- apiMonitorGet pid mid
  now <- Time.currentTime
  let ac = existing.alertConfig
      mergedAc =
        ac
          { Monitors.unit = mfilter (not . T.null) $ T.strip <$> (patch.unit <|> ac.unit)
          , Monitors.title = fromMaybe ac.title patch.title
          , Monitors.severity = fromMaybe ac.severity patch.severity
          , Monitors.subject = fromMaybe ac.subject patch.subject
          , Monitors.message = fromMaybe ac.message patch.message
          , Monitors.emails = maybe ac.emails (V.fromList . fmap CI.mk) patch.emails
          , Monitors.emailAll = fromMaybe ac.emailAll patch.emailAll
          , Monitors.slackChannels = maybe ac.slackChannels V.fromList patch.slackChannels
          }
      updateActive m = maybe m (\a -> m{Monitors.deactivatedAt = bool (Just now) Nothing a}) patch.active
      merged =
        updateActive
          existing
            { Monitors.updatedAt = now
            , Monitors.logQuery = fromMaybe existing.logQuery patch.query
            , -- Recompile whenever the query or its window moves: this is what alert
              -- evaluation runs, and inheriting it from `existing` left the monitor
              -- alerting on its previous query.
              Monitors.logQueryAsSql =
                compileAlertSql
                  pid
                  (fromMaybe existing.timeWindowMins patch.timeWindowMins)
                  (patch.environment <|> existing.environment)
                  (patch.service <|> existing.service)
                  (fromMaybe existing.logQuery patch.query)
                  existing.logQueryAsSql
            , Monitors.alertThreshold = fromMaybe existing.alertThreshold patch.alertThreshold
            , Monitors.warningThreshold = patch.warningThreshold <|> existing.warningThreshold
            , Monitors.triggerLessThan = fromMaybe existing.triggerLessThan patch.triggerLessThan
            , Monitors.checkIntervalMins = fromMaybe existing.checkIntervalMins patch.checkIntervalMins
            , Monitors.timeWindowMins = fromMaybe existing.timeWindowMins patch.timeWindowMins
            , Monitors.thresholdSustainedForMins = fromMaybe existing.thresholdSustainedForMins patch.thresholdSustainedForMins
            , Monitors.renotifyIntervalMins = patch.notifyAfterMins <|> existing.renotifyIntervalMins
            , Monitors.stopAfterCount = patch.stopAfterCount <|> existing.stopAfterCount
            , Monitors.alertConfig = mergedAc
            , Monitors.teams = maybe existing.teams V.fromList patch.teams
            , Monitors.visualizationType = fromMaybe existing.visualizationType patch.visualizationType
            , Monitors.alertRecoveryThreshold = patch.alertRecoveryThreshold <|> existing.alertRecoveryThreshold
            , Monitors.warningRecoveryThreshold = patch.warningRecoveryThreshold <|> existing.warningRecoveryThreshold
            , Monitors.environment = patch.environment <|> existing.environment
            , Monitors.service = patch.service <|> existing.service
            }
  merged <$ Monitors.queryMonitorUpsert merged


apiMonitorDelete :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATBaseCtx NoContent
apiMonitorDelete pid mid = withRefetchNoContent (apiMonitorGet pid mid) (Monitors.monitorsBulkUpdate pid Monitors.BADelete Nothing [mid])


-- | Verify a resource belongs to the project via @fetch@, run a mutation,
-- then re-fetch the post-mutation value. Used by mute/unmute/resolve/toggle
-- style lifecycle endpoints.
withRefetch :: Monad m => m a -> m b -> m a
withRefetch fetch mutate = fetch *> mutate *> fetch


-- | @fetch@ (ownership/validation) then @mutate@, returning 'NoContent'. The
-- delete/unstar/remove lifecycle counterpart of 'withRefetch'.
withRefetchNoContent :: Applicative m => m a -> m b -> m NoContent
withRefetchNoContent fetch mutate = fetch *> mutate $> NoContent


apiMonitorToggleActive :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATBaseCtx Monitors.QueryMonitor
apiMonitorToggleActive pid mid = withRefetch (apiMonitorGet pid mid) (Monitors.monitorToggleActiveById pid mid)


apiMonitorMute :: Projects.ProjectId -> Monitors.QueryMonitorId -> Maybe Int -> ATBaseCtx Monitors.QueryMonitor
apiMonitorMute pid mid durationM = withRefetch (apiMonitorGet pid mid) (Monitors.monitorsBulkUpdate pid Monitors.BAMute durationM [mid])


apiMonitorUnmute :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATBaseCtx Monitors.QueryMonitor
apiMonitorUnmute pid mid = withRefetch (apiMonitorGet pid mid) (Monitors.monitorsBulkUpdate pid Monitors.BAUnmute Nothing [mid])


apiMonitorResolve :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATBaseCtx Monitors.QueryMonitor
apiMonitorResolve pid mid = withRefetch (apiMonitorGet pid mid) (Monitors.monitorsBulkUpdate pid Monitors.BAResolve Nothing [mid])


apiMonitorBulk :: Projects.ProjectId -> BulkAction UUID.UUID -> ATBaseCtx (BulkResult UUID.UUID)
apiMonitorBulk pid ba =
  bulkExec
    ba
    [ (slug, count $ Monitors.monitorsBulkUpdate pid act ba.durationMinutes qIds)
    | (slug, act) <- [("delete", Monitors.BADelete), ("activate", Monitors.BAReactivate), ("deactivate", Monitors.BADeactivate), ("mute", Monitors.BAMute), ("unmute", Monitors.BAUnmute), ("resolve", Monitors.BAResolve)]
    ]
  where
    qIds = Monitors.QueryMonitorId <$> ba.ids


toSummary :: Dashboards.DashboardVM -> DashboardSummary
toSummary d =
  DashboardSummary
    { id = d.id
    , title = d.title
    , tags = d.tags
    , teams = d.teams
    , starred = isJust d.starredSince
    , createdAt = d.createdAt
    , updatedAt = d.updatedAt
    , filePath = d.filePath
    }


toFull :: Dashboards.DashboardVM -> DashboardFull
toFull d = DashboardFull{summary = toSummary d, fileSha = d.fileSha, schema = d.schema}


apiDashboardsList :: Projects.ProjectId -> Maybe Text -> Maybe Text -> ATBaseCtx [DashboardSummary]
apiDashboardsList pid sortM teamIdM = do
  ds <- case UUIDId <$> (teamIdM >>= UUID.fromText) of
    Just teamId -> Dashboards.selectDashboardsByTeam pid teamId
    Nothing -> Dashboards.selectDashboardsSortedBy pid (fromMaybe "updated_at" sortM)
  pure $ toSummary <$> ds


apiDashboardGet :: Projects.ProjectId -> Dashboards.DashboardId -> ATBaseCtx DashboardFull
apiDashboardGet pid did = toFull <$> (notFoundOr "Dashboard not found" =<< Dashboards.getDashboardByProjectId pid did)


-- | Insert a new dashboard row from an input.
insertDashboard :: Projects.ProjectId -> Projects.UserId -> UTCTime -> Dashboards.DashboardId -> Text -> Maybe [Text] -> Maybe [PM.TeamId] -> Maybe Text -> Maybe Dashboards.Dashboard -> ATBaseCtx DashboardFull
insertDashboard pid uid now did title tags teams filePath schema = do
  let d =
        (Dashboards.mkDashboardVM did pid now uid)
          { Dashboards.schema = schema
          , Dashboards.tags = V.fromList (fromMaybe [] tags)
          , Dashboards.title = title
          , Dashboards.teams = V.fromList (fromMaybe [] teams)
          , Dashboards.filePath = filePath
          }
  toFull <$> Dashboards.insert d


apiDashboardCreate :: Projects.ProjectId -> DashboardInput -> ATBaseCtx DashboardFull
apiDashboardCreate pid inp = do
  when (T.null inp.title) $ throwError err400{errBody = "title is required"}
  now <- Time.currentTime
  did <- UUIDId <$> UUID.genUUID
  -- API creation has no session so createdBy is a synthetic nil; git sync is skipped
  insertDashboard pid (Projects.UserId UUID.nil) now did inp.title inp.tags inp.teams inp.filePath inp.schema


-- | Upsert keyed by file_path.
apiDashboardApply :: Projects.ProjectId -> DashboardYAMLDoc -> ATBaseCtx DashboardFull
apiDashboardApply pid doc = do
  now <- Time.currentTime
  existingM <- Dashboards.getDashboardByFilePath pid doc.filePath
  let newTitle = fromMaybe (T.takeWhileEnd (/= '/') doc.filePath) doc.title
  case existingM of
    Just existing -> do
      _ <- Dashboards.updateSchema existing.id doc.schema (Just now)
      _ <- Dashboards.updateTitle pid existing.id newTitle
      whenJust doc.tags (void . Dashboards.updateTags pid existing.id . V.fromList)
      apiDashboardGet pid existing.id
    Nothing -> do
      did <- UUIDId <$> UUID.genUUID
      insertDashboard pid (Projects.UserId UUID.nil) now did newTitle doc.tags doc.teams (Just doc.filePath) (Just doc.schema)


apiDashboardUpdate :: Projects.ProjectId -> Dashboards.DashboardId -> DashboardInput -> ATBaseCtx DashboardFull
apiDashboardUpdate pid did inp =
  apiDashboardPatch pid did DashboardPatch{title = Just inp.title, tags = inp.tags, teams = inp.teams, filePath = inp.filePath, schema = inp.schema}


apiDashboardPatch :: Projects.ProjectId -> Dashboards.DashboardId -> DashboardPatch -> ATBaseCtx DashboardFull
apiDashboardPatch pid did patch = do
  _ <- apiDashboardGet pid did
  now <- Time.currentTime
  whenJust patch.title $ void . Dashboards.updateTitle pid did
  whenJust patch.schema $ \s -> void $ Dashboards.updateSchema did s (Just now)
  whenJust patch.tags $ void . Dashboards.updateTags pid did . V.fromList
  apiDashboardGet pid did


apiDashboardDelete :: Projects.ProjectId -> Dashboards.DashboardId -> ATBaseCtx NoContent
apiDashboardDelete pid did = withRefetchNoContent (apiDashboardGet pid did) (Dashboards.deleteDashboardsByIds pid (V.singleton did))


apiDashboardDuplicate :: Projects.ProjectId -> Dashboards.DashboardId -> ATBaseCtx DashboardFull
apiDashboardDuplicate pid did = do
  existing <- apiDashboardGet pid did
  now <- Time.currentTime
  newId <- UUIDId <$> UUID.genUUID
  let copyTitle = existing.summary.title <> " (Copy)"
      copySchema = existing.schema & _Just . #title %~ fmap (<> " (Copy)") . (<|> Just "Untitled")
  insertDashboard pid (Projects.UserId UUID.nil) now newId copyTitle (Just $ V.toList existing.summary.tags) (Just $ V.toList existing.summary.teams) Nothing copySchema


apiDashboardStar :: Projects.ProjectId -> Dashboards.DashboardId -> ATBaseCtx DashboardFull
apiDashboardStar pid did =
  withRefetch (apiDashboardGet pid did) (Time.currentTime >>= Dashboards.updateStarredSince pid did . Just)


apiDashboardUnstar :: Projects.ProjectId -> Dashboards.DashboardId -> ATBaseCtx NoContent
apiDashboardUnstar pid did = withRefetchNoContent (apiDashboardGet pid did) (Dashboards.updateStarredSince pid did Nothing)


apiDashboardYaml :: Projects.ProjectId -> Dashboards.DashboardId -> ATBaseCtx DashboardYAMLDoc
apiDashboardYaml pid did = do
  d <- apiDashboardGet pid did
  pure
    DashboardYAMLDoc
      { filePath = fromMaybe "" d.summary.filePath
      , title = Just d.summary.title
      , tags = Just (V.toList d.summary.tags)
      , teams = Just (V.toList d.summary.teams)
      , schema = fromMaybe def d.schema
      }


-- | Ownership-check the dashboard, rewrite its widget list with @f@ and persist.
withWidgets :: Projects.ProjectId -> Dashboards.DashboardId -> ([Widget.Widget] -> [Widget.Widget]) -> ATBaseCtx ()
withWidgets pid did f = do
  d <- apiDashboardGet pid did
  now <- Time.currentTime
  void $ Dashboards.updateSchema did (fromMaybe def d.schema & #widgets %~ f) (Just now)


-- | Widget upsert: replace the widget (by id) within the dashboard schema.
apiDashboardWidgetUpsert :: Projects.ProjectId -> Dashboards.DashboardId -> Widget.Widget -> ATBaseCtx Widget.Widget
apiDashboardWidgetUpsert pid did widget =
  widget <$ withWidgets pid did \ws -> case break ((== Just (fromMaybe "" widget.id)) . (.id)) ws of
    (pre, _ : post) -> pre ++ [widget] ++ post
    _ -> ws ++ [widget]


apiDashboardWidgetDelete :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> ATBaseCtx NoContent
apiDashboardWidgetDelete pid did wId = NoContent <$ withWidgets pid did (filter ((/= Just wId) . (.id)))


apiDashboardWidgetsReorder :: Projects.ProjectId -> Dashboards.DashboardId -> Map Text WidgetPosition -> ATBaseCtx NoContent
apiDashboardWidgetsReorder pid did positions = NoContent <$ withWidgets pid did (fmap applyPos)
  where
    applyPos w = maybe w update (w.id >>= (`Map.lookup` positions))
      where
        update p =
          let l = fromMaybe def w.layout
           in w{Widget.layout = Just l{Widget.x = Just p.x, Widget.y = Just p.y, Widget.w = Just p.w, Widget.h = Just p.h}}


toApiKeySummary :: ProjectApiKeys.ProjectApiKey -> ApiKeySummary
toApiKeySummary k =
  ApiKeySummary
    { id = k.id
    , title = k.title
    , keyPrefix = T.take 12 k.keyPrefix
    , active = k.active
    , createdAt = k.createdAt
    }


apiKeysList :: Projects.ProjectId -> ATBaseCtx [ApiKeySummary]
apiKeysList pid = fmap toApiKeySummary <$> ProjectApiKeys.projectApiKeysByProjectId pid


apiKeyGet :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATBaseCtx ApiKeySummary
apiKeyGet pid kid = toApiKeySummary <$> (ownedOr "API key not found" pid =<< ProjectApiKeys.getProjectApiKey kid)


apiKeyCreate :: Projects.ProjectId -> ApiKeyCreate -> ATBaseCtx ApiKeyCreated
apiKeyCreate pid inp = do
  authCtx <- ask @AuthContext
  keyUUID <- UUID.genUUID
  let encryptedKeyB64 = ProjectApiKeys.encodeApiKeyB64 authCtx.config.apiKeyEncryptionSecretKey keyUUID
  k <- ProjectApiKeys.newProjectApiKeys pid keyUUID inp.title encryptedKeyB64
  ProjectApiKeys.insertProjectApiKey k
  pure ApiKeyCreated{summary = toApiKeySummary k, key = encryptedKeyB64}


apiKeyActivate :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATBaseCtx ApiKeySummary
apiKeyActivate pid kid = withRefetch (apiKeyGet pid kid) (ProjectApiKeys.activateApiKey kid)


apiKeyDeactivate :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATBaseCtx ApiKeySummary
apiKeyDeactivate pid kid = do
  s <- apiKeyGet pid kid
  _ <- ProjectApiKeys.revokeApiKey kid
  -- getProjectApiKey filters to active keys, so return pre-fetched summary with active=False
  pure $ s & #active .~ False


apiKeyDelete :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATBaseCtx NoContent
apiKeyDelete pid kid = withRefetchNoContent (apiKeyGet pid kid) (ProjectApiKeys.revokeApiKey kid)


apiEventsQuery :: EventsSearchHandler -> Projects.ProjectId -> EventsQuery -> ATBaseCtx LogResult
apiEventsQuery queryEvents pid q = queryEvents pid q.query q.since q.from q.to q.source q.limit q.withChildren q.includeAttributes q.environment q.service


-- | Bulk action: currently supports "delete".
apiDashboardBulk :: Projects.ProjectId -> BulkAction UUID.UUID -> ATBaseCtx (BulkResult UUID.UUID)
apiDashboardBulk pid ba =
  bulkExec ba [("delete", count $ Dashboards.deleteDashboardsByIds pid (V.fromList $ UUIDId <$> ba.ids))]


-- | Payload for POST /share. event_type defaults to "request" (log/span also accepted).
data ShareLinkCreate = ShareLinkCreate
  { eventId :: UUID.UUID
  , eventCreatedAt :: UTCTime
  , eventType :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ShareLinkCreate
  deriving (ToSchema) via SnakeSchema ShareLinkCreate


-- | Response: `{ id: UUID, url: Text }`. URL lasts 48 hours.
-- Keep in sync with the SQL interval in 'Pages.Share.getShareLink' and
-- the user-facing copy in 'Pages.Share'.
data ShareLinkCreated = ShareLinkCreated
  { id :: UUID.UUID
  , url :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ShareLinkCreated
  deriving (ToSchema) via SnakeSchema ShareLinkCreated


apiShareLinkCreate :: Projects.ProjectId -> ShareLinkCreate -> ATBaseCtx ShareLinkCreated
apiShareLinkCreate pid req = do
  authCtx <- ask @AuthContext
  _ <- notFoundOr "event not found" =<< Telemetry.otelRecordByProjectAndId authCtx.env.enableTimefusionReads pid req.eventCreatedAt req.eventId
  shareId <- UUID.genUUID
  ShareEvents.createShareLink shareId pid req.eventId (fromMaybe "request" req.eventType) req.eventCreatedAt
  let url = hostPath authCtx.config.hostUrl $ "share/r/" <> UUID.toText shareId
  pure ShareLinkCreated{id = shareId, url}


toProjectSummary :: Projects.Project -> ProjectSummary
toProjectSummary p =
  ProjectSummary
    { id = p.id
    , title = p.title
    , description = p.description
    , paymentPlan = p.paymentPlan
    , timeZone = p.timeZone
    , createdAt = p.createdAt
    , updatedAt = p.updatedAt
    }


toProjectFull :: Projects.Project -> [Text] -> ProjectFull
toProjectFull p emails =
  ProjectFull
    { summary = toProjectSummary p
    , dailyNotif = p.dailyNotif
    , weeklyNotif = p.weeklyNotif
    , notifyEmails = emails
    , endpointAlerts = p.endpointAlerts
    , errorAlerts = p.errorAlerts
    }


apiMe :: Projects.ProjectId -> ATBaseCtx MeResponse
apiMe pid = do
  p <- notFoundOr "Project not found" =<< Projects.projectById pid
  ctx <- ask @AuthContext
  pure MeResponse{projectId = pid, project = toProjectSummary p, hostUrl = ctx.config.hostUrl}


apiProjectGet :: Projects.ProjectId -> ATBaseCtx ProjectFull
apiProjectGet pid = do
  p <- notFoundOr "Project not found" =<< Projects.projectById pid
  emails <- maybe [] (V.toList . (.notify_emails)) <$> PM.getEveryoneTeam pid
  pure $ toProjectFull p emails


-- | Partial update: only provided fields change. 0 rows affected ⇒ 404.
apiProjectPatch :: Projects.ProjectId -> Projects.ProjectPatch -> ATBaseCtx ProjectFull
apiProjectPatch pid patch = do
  now <- Time.currentTime
  n <- Projects.patchProjectSettings pid patch now
  when (n == 0) $ throwError err404{errBody = "Project not found"}
  apiProjectGet pid


endpointToSummary :: Endpoints.Endpoint -> EndpointSummary
endpointToSummary e =
  EndpointSummary
    { id = e.id
    , projectId = e.projectId
    , urlPath = e.urlPath
    , method = e.method
    , host = e.host
    , hash = e.hash
    , outgoing = e.outgoing
    , description = e.description
    , serviceName = e.serviceName
    , environment = e.environment
    , totalRequests = Nothing
    , lastSeen = Nothing
    }


apiEndpointsList :: Projects.ProjectId -> Maybe Text -> Maybe Bool -> Maybe Int -> Maybe Int -> ATBaseCtx (Paged EndpointSummary)
apiEndpointsList pid searchM outgoingM pageM perPageM = paged pageM perPageM 100 $ \perPage offset ->
  first (fmap endpointToSummary) <$> Endpoints.listEndpointsPaged pid (fromMaybe False outgoingM) searchM perPage offset


apiEndpointGet :: Projects.ProjectId -> Endpoints.EndpointId -> ATBaseCtx EndpointFull
apiEndpointGet pid eid = do
  e <- notFoundOr "Endpoint not found" =<< Endpoints.getEndpointById pid eid
  pure
    EndpointFull
      { summary = endpointToSummary e
      , urlParams = e.urlParams
      , createdAt = e.createdAt
      , updatedAt = e.updatedAt
      }


logPatternToSummary :: LogPatterns.LogPattern -> LogPatternSummary
logPatternToSummary lp =
  LogPatternSummary
    { id = LogPatterns.unLogPatternId lp.id
    , projectId = lp.projectId
    , patternHash = lp.patternHash
    , sourceField = lp.sourceField
    , serviceName = lp.serviceName
    , logLevel = lp.logLevel
    , state = lp.state
    , occurrenceCount = lp.occurrenceCount
    , firstSeenAt = zonedTimeToUTC lp.firstSeenAt
    , lastSeenAt = zonedTimeToUTC lp.lastSeenAt
    , isError = lp.isError
    }


apiLogPatternsList :: Projects.ProjectId -> Maybe Int -> Maybe Int -> ATBaseCtx (Paged LogPatternSummary)
apiLogPatternsList pid pageM perPageM = paged pageM perPageM 200 $ \perPage offset ->
  (,)
    . fmap logPatternToSummary
    <$> LogPatterns.getLogPatterns pid perPage offset
    <*> LogPatterns.countLogPatterns pid


fetchLogPattern :: Projects.ProjectId -> Int64 -> ATBaseCtx LogPatterns.LogPattern
fetchLogPattern pid lpid = notFoundOr "Log pattern not found" =<< LogPatterns.getLogPatternByIdScoped pid lpid


apiLogPatternGet :: Projects.ProjectId -> Int64 -> ATBaseCtx LogPatternFull
apiLogPatternGet pid lpid = do
  lp <- fetchLogPattern pid lpid
  pure
    LogPatternFull
      { summary = logPatternToSummary lp
      , logPattern = lp.logPattern
      , sampleMessage = lp.sampleMessage
      }


apiLogPatternAck :: Projects.ProjectId -> Int64 -> ATBaseCtx LogPatternFull
apiLogPatternAck pid lpid =
  fetchLogPattern pid lpid
    *> LogPatterns.setLogPatternStatesByIds pid Nothing (V.singleton lpid) LogPatterns.LPSAcknowledged
    *> apiLogPatternGet pid lpid


apiLogPatternsBulk :: Projects.ProjectId -> BulkAction Int64 -> ATBaseCtx (BulkResult Int64)
apiLogPatternsBulk pid ba =
  bulkExec
    ba
    [ ("acknowledge", setState LogPatterns.LPSAcknowledged)
    , ("ignore", setState LogPatterns.LPSIgnored)
    ]
  where
    setState st = BulkSucceeded <$> LogPatterns.setLogPatternStatesByIds pid Nothing (V.fromList ba.ids) st


issueToSummary :: Issues.Issue -> IssueApiSummary
issueToSummary i =
  IssueApiSummary
    { id = i.id
    , projectId = i.projectId
    , issueType = i.issueType
    , title = i.title
    , severity = display i.severity
    , critical = i.critical
    , service = i.service
    , affectedRequests = i.affectedRequests
    , affectedClients = i.affectedClients
    , acknowledged = isJust i.acknowledgedAt
    , acknowledgedUntil = zonedTimeToUTC <$> i.acknowledgedUntil
    , archived = isJust i.archivedAt
    , createdAt = zonedTimeToUTC i.createdAt
    , updatedAt = zonedTimeToUTC i.updatedAt
    }


issueToFull :: Issues.Issue -> IssueApiFull
issueToFull i =
  IssueApiFull
    { summary = issueToSummary i
    , recommendedAction = i.recommendedAction
    , migrationComplexity = i.migrationComplexity
    , issueData = coerce i.issueData
    }


apiIssuesList
  :: Projects.ProjectId
  -> Maybe IssueStatus -- status: open|acknowledged|archived|all
  -> Maybe Text -- issue_type filter
  -> Maybe Text -- service filter
  -> Maybe Int -- page
  -> Maybe Int -- per_page
  -> ATBaseCtx (Paged IssueApiSummary)
apiIssuesList pid statusM typeM svcM pageM perPageM = paged pageM perPageM 200 $ \perPage offset ->
  -- Map high-level status to ack/archive column filters.
  let (ackF, archF) = case fromMaybe ISOpen statusM of
        ISOpen -> (Issues.IsNull, Issues.IsNull)
        ISAcknowledged -> (Issues.IsNotNull, Issues.IsNull)
        ISArchived -> (Issues.AnyValue, Issues.IsNotNull)
        ISAll -> (Issues.AnyValue, Issues.AnyValue)
      -- An empty query param means "no filter", not "match the empty string".
      oneOf = filter (not . T.null) . maybeToList
   in first (fmap issueToSummary)
        <$> Issues.selectIssues
          pid
          Issues.PIssue
          Issues.defIssueFilters
            { Issues.ack = ackF
            , Issues.archive = archF
            , Issues.types = oneOf typeM
            , Issues.services = oneOf svcM
            , Issues.order = Just "-updated_at"
            , Issues.limit = perPage
            , Issues.offset = offset
            }


incidentToSummary :: Incidents.Episode -> IncidentSummary
incidentToSummary incident =
  IncidentSummary
    { id = incident.id
    , projectId = incident.projectId
    , issueId = incident.issueId
    , phase = incident.phase
    , startedAt = incident.startedAt
    , lastEventAt = incident.lastEventAt
    , closedAt = incident.closedAt
    }


apiIncidentsList :: Projects.ProjectId -> Maybe Incidents.EpisodePhase -> Maybe Int -> ATBaseCtx [IncidentSummary]
apiIncidentsList pid phase limit = map incidentToSummary <$> Incidents.listEpisodes pid phase (fromMaybe 20 limit)


apiIncidentGet :: Projects.ProjectId -> Incidents.EpisodeId -> ATBaseCtx IncidentSummary
apiIncidentGet pid eid = incidentToSummary <$> (notFoundOr "Incident not found" =<< Incidents.getEpisode pid eid)


fetchIssue :: Projects.ProjectId -> Issues.IssueId -> ATBaseCtx Issues.Issue
fetchIssue pid iid = notFoundOr "Issue not found" =<< Issues.selectIssueById pid iid


apiIssueGet :: Projects.ProjectId -> Issues.IssueId -> ATBaseCtx IssueApiFull
apiIssueGet pid iid = issueToFull <$> (enrichIssue pid =<< fetchIssue pid iid)


-- | Fill an empty @stack_trace@ on a runtime-exception issue with a synthesised
-- one derived from the trace's span hierarchy. GHC doesn't capture a backtrace
-- by default, so without this fallback the API/CLI surfaces an empty string.
-- TODO(perf): store the synth stack at ingestion to avoid these 2 extra queries.
enrichIssue :: Projects.ProjectId -> Issues.Issue -> ATBaseCtx Issues.Issue
enrichIssue pid issue = case Issues.issuePayload issue of
  Just (Issues.RuntimeExceptionP rd)
    | T.null rd.stackTrace -> do
        epM <- ErrorPatterns.getErrorPatternByHash pid issue.targetHash
        case epM >>= \ep -> (,zonedTimeToUTC ep.updatedAt) <$> ep.recentTraceId of
          Nothing -> pure issue
          Just (trId, ts) -> do
            useTf <- useTfReads
            now <- Time.currentTime
            synth <- synthStackFromSpans trId <$> Telemetry.getSpanRecordsByTraceId useTf pid trId (Just ts) now Nothing
            pure $ if T.null synth then issue else issue{Issues.issueData = Aeson (Issues.payloadJson (Issues.RuntimeExceptionP rd{Issues.stackTrace = synth}))}
  _ -> pure issue


-- | Build a pseudo-stacktrace from a trace's spans, ordered by start time and
-- annotated with service + span id. Errored span is marked with @!!@.
synthStackFromSpans :: Text -> [Telemetry.OtelLogsAndSpans] -> Text
synthStackFromSpans _ [] = ""
synthStackFromSpans trId spans =
  "(synthesized from trace "
    <> trId
    <> " — GHC backtrace unavailable; spans ordered by start time, !! marks the errored span)\n"
    <> T.intercalate "\n" (map formatOne (sortOn (.start_time) spans))
  where
    formatOne s =
      bool "   " "!! " (s.status_code == Just "ERROR")
        <> "at "
        <> fromMaybe "<unnamed>" s.name
        <> foldMap (\v -> " [" <> v <> "]") (Telemetry.spanServiceName s)
        <> " (span="
        <> fromMaybe "?" (s.context >>= (.span_id))
        <> ")"


issueMutate :: Projects.ProjectId -> Issues.IssueId -> ([Issues.IssueId] -> ATBaseCtx Int64) -> ATBaseCtx IssueApiFull
issueMutate pid iid op = fetchIssue pid iid *> op [iid] *> apiIssueGet pid iid


-- | Acknowledge, silencing notifications for @duration_minutes@ — or
-- indefinitely (until the issue regresses or is un-acked) when omitted.
apiIssueAck :: Projects.ProjectId -> Issues.IssueId -> Maybe Int -> ATBaseCtx IssueApiFull
apiIssueAck pid iid durationM =
  mkAckSet durationM >>= \ack -> issueMutate pid iid \ids -> Issues.setAckState pid ids (Just ack)


-- | Build an 'Issues.AckSet' for @now@ from an optional duration in minutes.
-- API-key auth carries no user, so @by@ is always unattributed.
mkAckSet :: Maybe Int -> ATBaseCtx Issues.AckSet
mkAckSet durationM = do
  now <- Time.currentTime
  pure Issues.AckSet{at = now, by = Nothing, window = maybe Issues.AckIndefinite Issues.AckFor durationM}


apiIssueUnack :: Projects.ProjectId -> Issues.IssueId -> ATBaseCtx IssueApiFull
apiIssueUnack pid iid = issueMutate pid iid $ \ids -> Issues.setAckState pid ids Nothing


apiIssueArchive :: Projects.ProjectId -> Issues.IssueId -> ATBaseCtx IssueApiFull
apiIssueArchive pid iid = do
  now <- Time.currentTime
  issueMutate pid iid $ \ids -> Issues.setArchiveState pid ids (Just (now, Issues.ArchiveIndefinite))


apiIssueUnarchive :: Projects.ProjectId -> Issues.IssueId -> ATBaseCtx IssueApiFull
apiIssueUnarchive pid iid = issueMutate pid iid $ \ids -> Issues.setArchiveState pid ids Nothing


apiIssuesBulk :: Projects.ProjectId -> BulkAction Issues.IssueId -> ATBaseCtx (BulkResult Issues.IssueId)
apiIssuesBulk pid ba = do
  now <- Time.currentTime
  ackSet <- mkAckSet ba.durationMinutes
  let ack = count $ Issues.setAckState pid ba.ids (Just ackSet)
      unack = count $ Issues.setAckState pid ba.ids Nothing
      archive = count $ Issues.setArchiveState pid ba.ids (Just (now, Issues.ArchiveIndefinite))
      unarchive = count $ Issues.setArchiveState pid ba.ids Nothing
  bulkExec
    ba
    [ ("acknowledge", ack)
    , ("ack", ack)
    , ("unack", unack)
    , ("unacknowledge", unack)
    , ("archive", archive)
    , ("unarchive", unarchive)
    ]


toTeamSummary :: PM.Team -> TeamSummary
toTeamSummary t =
  TeamSummary
    { id = t.id
    , name = t.name
    , handle = t.handle
    , description = t.description
    , isEveryone = t.is_everyone
    , memberCount = V.length t.members
    , createdAt = t.created_at
    , updatedAt = t.updated_at
    }


-- | Project the team's member UUIDs to 'UserRef's via a single users lookup.
-- ('Projects.usersByIds' already short-circuits on an empty vector.)
resolveTeamMembers :: V.Vector Projects.UserId -> ATBaseCtx [UserRef]
resolveTeamMembers uids = fmap toUserRef <$> Projects.usersByIds uids
  where
    toUserRef u =
      UserRef
        { id = u.id.unwrap
        , email = CI.original u.email
        , name = guarded (not . T.null) (T.strip (u.firstName <> " " <> u.lastName))
        }


toTeamFull :: PM.Team -> ATBaseCtx TeamFull
toTeamFull t = do
  members <- resolveTeamMembers t.members
  pure
    TeamFull
      { summary = toTeamSummary t
      , members = members
      , notifyEmails = V.toList t.notify_emails
      , slackChannels = V.toList t.slack_channels
      , discordChannels = V.toList t.discord_channels
      , phoneNumbers = V.toList t.phone_numbers
      , pagerdutyServices = V.toList t.pagerduty_services
      }


-- | Fetch a single team by id (scoped to project). 404s if missing.
fetchTeam :: Projects.ProjectId -> TeamId -> ATBaseCtx PM.Team
fetchTeam pid tid = notFoundOr "Team not found" =<< PM.getTeamById pid tid


-- | 'fetchTeam' that also rejects the everyone team, which is not editable here.
fetchEditableTeam :: Projects.ProjectId -> TeamId -> ATBaseCtx PM.Team
fetchEditableTeam pid tid = do
  t <- fetchTeam pid tid
  t <$ when t.is_everyone (throwError err400{errBody = "The everyone team cannot be updated via this endpoint"})


apiTeamsList :: Projects.ProjectId -> ATBaseCtx [TeamSummary]
apiTeamsList pid = fmap toTeamSummary <$> PM.getTeams pid


apiTeamGet :: Projects.ProjectId -> TeamId -> ATBaseCtx TeamFull
apiTeamGet pid tid = fetchTeam pid tid >>= toTeamFull


-- | Build 'PM.TeamDetails' from an input, resolving optional collections to empty.
teamDetailsFromInput :: TeamInput -> PM.TeamDetails
teamDetailsFromInput inp =
  PM.TeamDetails
    { name = inp.name
    , description = fromMaybe "" inp.description
    , handle = inp.handle
    , members = vml (fmap Projects.UserId <$> inp.members)
    , notifyEmails = vml inp.notifyEmails
    , slackChannels = vml inp.slackChannels
    , discordChannels = vml inp.discordChannels
    , phoneNumbers = vml inp.phoneNumbers
    , pagerdutyServices = vml inp.pagerdutyServices
    , disabledChannels = V.empty
    }
  where
    vml :: Maybe [a] -> V.Vector a
    vml = V.fromList . fromMaybe []


-- | Rejects the reserved "everyone" handle. Caller owns validation of @name@.
assertHandleAllowed :: Text -> ATBaseCtx ()
assertHandleAllowed h
  | T.toLower h == "everyone" =
      throwError err400{errBody = "Handle \"everyone\" is reserved"}
  | T.null h = throwError err400{errBody = "handle is required"}
  | otherwise = pass


apiTeamCreate :: Projects.ProjectId -> TeamInput -> ATBaseCtx TeamFull
apiTeamCreate pid inp = do
  when (T.null inp.name) $ throwError err400{errBody = "name is required"}
  assertHandleAllowed inp.handle
  createdM <- PM.createTeam pid Nothing (teamDetailsFromInput inp)
  maybe (throwError err400{errBody = encodeUtf8 $ "Handle \"" <> inp.handle <> "\" is already in use"}) (apiTeamGet pid) createdM


apiTeamUpdate :: Projects.ProjectId -> TeamId -> TeamInput -> ATBaseCtx TeamFull
apiTeamUpdate pid tid inp = do
  _ <- fetchEditableTeam pid tid
  when (T.null inp.name) $ throwError err400{errBody = "name is required"}
  assertHandleAllowed inp.handle
  _ <- PM.updateTeam pid tid (teamDetailsFromInput inp)
  apiTeamGet pid tid


apiTeamPatch :: Projects.ProjectId -> TeamId -> TeamPatch -> ATBaseCtx TeamFull
apiTeamPatch pid tid p = do
  t <- fetchEditableTeam pid tid
  whenJust p.handle assertHandleAllowed
  let merged =
        (PM.teamToDetails t)
          { PM.name = fromMaybe t.name p.name
          , PM.description = fromMaybe t.description p.description
          , PM.handle = fromMaybe t.handle p.handle
          , PM.members = maybe t.members (V.fromList . fmap Projects.UserId) p.members
          , PM.notifyEmails = maybe t.notify_emails V.fromList p.notifyEmails
          , PM.slackChannels = maybe t.slack_channels V.fromList p.slackChannels
          , PM.discordChannels = maybe t.discord_channels V.fromList p.discordChannels
          , PM.phoneNumbers = maybe t.phone_numbers V.fromList p.phoneNumbers
          , PM.pagerdutyServices = maybe t.pagerduty_services V.fromList p.pagerdutyServices
          }
  _ <- PM.updateTeam pid tid merged
  apiTeamGet pid tid


apiTeamDelete :: Projects.ProjectId -> TeamId -> ATBaseCtx NoContent
apiTeamDelete pid tid = do
  t <- fetchTeam pid tid
  when t.is_everyone $ throwError err400{errBody = "The everyone team cannot be deleted"}
  NoContent <$ PM.deleteTeams pid (V.singleton tid)


apiTeamsBulk :: Projects.ProjectId -> BulkAction TeamId -> ATBaseCtx (BulkResult TeamId)
apiTeamsBulk pid ba = bulkExec ba [("delete", del)]
  where
    del = do
      byId <- Map.fromList . fmap (\t -> (t.id, t)) <$> PM.getTeamsById pid (V.fromList ba.ids)
      let classify tid = case Map.lookup tid byId of
            Nothing -> Left BulkFailure{id = tid, error = "not found"}
            Just t | t.is_everyone -> Left BulkFailure{id = tid, error = "the everyone team cannot be deleted"}
            Just _ -> Right tid
          (failed, succeeded) = partitionEithers (classify <$> ba.ids)
      PM.deleteTeams pid (V.fromList succeeded)
      pure $ BulkPartial succeeded failed


toMemberSummary :: PM.ProjectMemberVM -> MemberSummary
toMemberSummary m =
  MemberSummary
    { id = m.id.unwrap
    , userId = m.userId.unwrap
    , email = CI.original m.email
    , firstName = m.first_name
    , lastName = m.last_name
    , permission = m.permission
    }


apiMembersList :: Projects.ProjectId -> ATBaseCtx [MemberSummary]
apiMembersList pid = fmap toMemberSummary <$> PM.selectActiveProjectMembers pid


-- | Locate a single member row via its user_id (project-scoped).
fetchMemberByUserId :: Projects.ProjectId -> UUID.UUID -> ATBaseCtx PM.ProjectMemberVM
fetchMemberByUserId pid uid = notFoundOr "Member not found" =<< PM.getActiveProjectMemberByUserId pid (Projects.UserId uid)


apiMemberGet :: Projects.ProjectId -> UUID.UUID -> ATBaseCtx MemberSummary
apiMemberGet pid uid = toMemberSummary <$> fetchMemberByUserId pid uid


-- | Add a member by email or user_id. Email lookup creates a stub user if no
-- matching row exists (mirrors the HTML manage-members flow).
apiMemberAdd :: Projects.ProjectId -> MemberAdd -> ATBaseCtx MemberSummary
apiMemberAdd pid req = do
  uid <- case (req.userId, req.email) of
    (Just u, _) -> do
      _ <- notFoundOr "User not found" =<< Projects.userById (Projects.UserId u)
      pure (Projects.UserId u)
    (Nothing, Just e) ->
      maybe (notFoundOr "Could not create user" =<< Projects.createEmptyUser e) pure =<< Projects.userIdByEmail e
    (Nothing, Nothing) ->
      throwError err400{errBody = "Either email or user_id is required"}
  _ <- PM.insertProjectMembers [PM.CreateProjectMembers{projectId = pid, userId = uid, permission = fromMaybe PM.PView req.permission}]
  apiMemberGet pid uid.unwrap


apiMemberPatch :: Projects.ProjectId -> UUID.UUID -> MemberPatch -> ATBaseCtx MemberSummary
apiMemberPatch pid uid p = do
  m <- fetchMemberByUserId pid uid
  PM.updateProjectMembersPermissons [(m.id, p.permission)]
  apiMemberGet pid uid


apiMemberRemove :: Projects.ProjectId -> UUID.UUID -> ATBaseCtx NoContent
apiMemberRemove pid uid = do
  m <- fetchMemberByUserId pid uid
  NoContent <$ PM.softDeleteProjectMembers (m.id :| [])


-- | GET /api/v1/facets — return the precomputed facet summary for a project.
--
-- Facets are top-N values per field, generated by the OTLP facets background
-- job and stored in @apis.facet_summaries@. They tell an agent (or human)
-- what queries are likely to /work/ — "service X has 1.2k events; status
-- code 500 has 47" — without needing to poke at the data first.
--
-- Optional query params:
--
-- * @since@ — relative window (default 24h), feeds 'parseTimeRange'.
-- * @from@/@to@ — absolute ISO timestamps; override @since@.
-- * @field@ — return only the named field's values (e.g. @resource.service.name@).
--
-- The response is always a JSON object keyed by field path, each value a
-- @[{value, count}]@ list sorted by count descending. Missing/expired
-- facets return @{}@ (not 404) — agents can rely on the shape regardless.
apiMetricsCatalog :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Bool -> ATBaseCtx Telemetry.MetricCatalogResponse
apiMetricsCatalog pid service search limit offset active = do
  now <- Time.currentTime
  Telemetry.getMetricCatalogResponse pid service search now (fromMaybe 20 limit) (fromMaybe 0 offset) (fromMaybe True active)


apiFacets :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATBaseCtx AE.Value
apiFacets pid sinceM fromM toM fieldM = do
  now <- Time.currentTime
  let (fromT, toT, _) = TP.parseTimeRange now (TP.TimePicker sinceM fromM toM)
      defaultFrom = fromMaybe (addUTCTime (negate nominalDay) now) fromT
      defaultTo = fromMaybe now toT
  summaryM <- SchemaCatalog.getFacetSummary pid "otel_logs_and_spans" defaultFrom defaultTo
  let Fields.FacetData facetMap = maybe (Fields.FacetData mempty) (.facetJson) summaryM
      -- Storage uses `___` as the path separator (raw column names like
      -- `resource___service___name`); the public API contract is dotted.
      dotKey = AEK.fromText . T.replace "___" "." . AEK.toText
      asAeson = case AE.toJSON facetMap of
        AE.Object o -> AE.Object (AEKM.mapKeyVal dotKey identity o)
        v -> v
      filtered = case (fieldM, asAeson) of
        (Just f, AE.Object o) ->
          let k = AEK.fromText f
           in AE.Object (maybe mempty (AEKM.singleton k) (AEKM.lookup k o))
        _ -> asAeson
  -- C2: when a specific @field@ was requested and the cache had nothing,
  -- compute the top values on-the-fly. Only fields confirmed by C1's
  -- introspection are admissible (prevents SQL injection via @field=...@).
  -- Tag the response so the CLI can surface "computed live."
  case (fieldM, filtered) of
    (Just f, AE.Object o) | AEKM.null o -> facetsFallback pid f defaultFrom defaultTo
    _ -> pure filtered


-- | GET /api/v1/events/{id}/time/{ts} — O(1) lookup using the timeseries
-- partition key. Both id and timestamp must be supplied; the DB resolves the
-- row via @timestamp = ts AND id = ?@ (the caller holds the exact stored
-- timestamp). Returns 404 when the event is not found.
apiEventGet :: Projects.ProjectId -> UUID.UUID -> UTCTime -> ATBaseCtx AE.Value
apiEventGet pid eid ts = do
  useTf <- useTfReads
  AE.toJSON <$> (notFoundOr "event not found" =<< Telemetry.otelRecordByProjectAndId useTf pid ts eid)
