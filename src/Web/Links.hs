{-# LANGUAGE NoFieldSelectors #-}

-- | The app's cookie-authenticated route records, below the pages, so pages can link with
-- 'urls' and a renamed or reshaped route fails to compile wherever it is linked. Page-defined
-- response and request-body types appear as 'Opaque'/'Opaque1' names: links never look at them.
-- "Web.Routes" mounts these records, resolves the names and serves them.
module Web.Links (
  Opaque,
  Opaque1,
  ProjectId,
  QPT,
  QPU,
  QPI,
  LenientQP,
  GetRedirect,
  LocationRedirect,
  SessionRedirect,
  KeepPrefixExp,
  LogDataFieldExp,
  AllQueryParams,
  PNG,
  YAML,
  SseHeaders,
  DashboardFilters (..),
  DashboardBulkAction (..),
  RepositoryTab (..),
  CookieProtectedRoutes (..),
  LogExplorerRoutes' (..),
  LogExplorerRoutes,
  IssuesRoutes' (..),
  IssuesRoutes,
  TelemetryRoutes' (..),
  TelemetryRoutes,
  MonitorsRoutes' (..),
  MonitorsRoutes,
  ProjectsRoutes' (..),
  ProjectsRoutes,
  WidgetPngGet,
  urls,
  logExplorerLinks,
  explorerUrl,
  widgetPngPath,
  ProofMediaGet,
  ProofRatingGet,
  ProofRatingPost,
  proofRatingPath,
  proofMediaPath,
  FixMediaGet,
  fixMediaPath,
) where

import Data.Aeson qualified as AE
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Default (Default)
import Data.Effectful.AgentArtifacts qualified as Artifacts
import Data.Effectful.Hasql qualified as Hasql
import Data.List (lookup)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.UUID qualified as UUID
import GHC.TypeLits (Symbol)
import Lucid (Html)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogQueries qualified as LogQueries
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.PrometheusScrapeConfigs qualified as PromCfg
import Models.Apis.ShareEvents qualified as ShareEvents
import Models.Projects.ImpactReviews qualified as ImpactReviews
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.ProjectMemory qualified as ProjectMemory
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.ContainerTypes (Scope)
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.Wai (queryString)
import Pages.Charts.Charts qualified as Charts
import Pages.Replay qualified as Replay
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Pkg.Git qualified as Git
import Pkg.LiveTail qualified as LT
import Pkg.Parser (PageDirection)
import Pkg.Parser qualified as Parser
import Relude
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Htmx (HXBoosted, HXCurrentURL, HXRequest)
import Servant.OpenApi (HasOpenApi (..))
import Servant.QueryParam.Record (RecordParam)
import Servant.QueryParam.TypeLevel (Eval, Exp)
import Servant.Server.Internal.Delayed (passToServer)
import System.Types (Action, Mime, RespHeaders, Unrefusable)
import Web.Cookie (SetCookie)
import Web.FormUrlEncoded (FromForm)


type ProjectId = Capture "projectID" Projects.ProjectId
type QPT a = QueryParam a Text
type QPU a = QueryParam a UTCTime
type QPI a = QueryParam a Int


-- | Unknown values reach the handler as 'Left' instead of a 400, for links that outlive their enum.
type LenientQP = QueryParam' '[Optional, Lenient]


-- | A type defined above this module (mostly by pages), named by its "Web.Routes" import
-- qualifier. "Web.Routes" gives each name its instance; links never need it.
type Opaque :: Symbol -> Type
type family Opaque name


type Opaque1 :: Symbol -> Type -> Type
type family Opaque1 name


type GetRedirect = Verb 'GET 302


type LocationRedirect a = GetRedirect '[HTML] (Headers '[Header "Location" Text] a)
type SessionRedirect a = GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] a)


-- Type-level expression for preserving field prefixes in RecordParam
data KeepPrefixExp :: Symbol -> Exp Symbol
type instance Eval (KeepPrefixExp sym) = sym


type family LogDataField (sym :: Symbol) :: Symbol where
  LogDataField "targetSpans" = "target-spans"
  LogDataField "serviceScope" = "service_scope"
  LogDataField sym = sym


data LogDataFieldExp :: Symbol -> Exp Symbol
type instance Eval (LogDataFieldExp sym) = LogDataField sym


-- | Every query parameter, as @(key, value)@ pairs, for routes that forward arbitrary
-- variables (dashboard @var-*@/@const-*@).
data AllQueryParams


instance HasServer api ctx => HasServer (AllQueryParams :> api) ctx where
  type ServerT (AllQueryParams :> api) m = [(Text, Maybe Text)] -> ServerT api m
  route _ ctx subserver = route (Proxy @api) ctx $ passToServer subserver (map (bimap dec (fmap dec)) . queryString)
    where
      dec = decodeUtf8With lenientDecode
  hoistServerWithContext _ pc nat s = hoistServerWithContext (Proxy @api) pc nat . s


instance HasLink api => HasLink (AllQueryParams :> api) where
  type MkLink (AllQueryParams :> api) a = [(Text, Maybe Text)] -> MkLink api a
  toLink toA _ l = toLink toA (Proxy @api) . foldl' (\acc (k, v) -> addQueryParam (maybe (FlagParam (toString k)) (SingleParam (toString k)) v) acc) l


-- | A catch-all has nothing specific to advertise; documenting it as the identity keeps its
-- routes inside the generated OpenAPI spec.
instance HasOpenApi api => HasOpenApi (AllQueryParams :> api) where
  toOpenApi _ = toOpenApi (Proxy @api)


-- servant-queryparam-core 2.0.1's 'RecordParam' links demand @HasLink (a :> sub)@ for each
-- 'Maybe'/list field type but never use it; these discharge it for our records' field types.
instance HasLink sub => HasLink (Text :> sub) where
  type MkLink (Text :> sub) b = MkLink sub b
  toLink toA _ = toLink toA (Proxy @sub)


instance HasLink sub => HasLink (UTCTime :> sub) where
  type MkLink (UTCTime :> sub) b = MkLink sub b
  toLink toA _ = toLink toA (Proxy @sub)


instance HasLink sub => HasLink (PageDirection :> sub) where
  type MkLink (PageDirection :> sub) b = MkLink sub b
  toLink toA _ = toLink toA (Proxy @sub)


instance HasLink sub => HasLink (Bool :> sub) where
  type MkLink (Bool :> sub) b = MkLink sub b
  toLink toA _ = toLink toA (Proxy @sub)


-- | PNG responses. Servant renders the Content-Type from this instance, so the
-- handler must NOT add one itself: two Content-Type headers make nginx keep the
-- first and drop the rest, and a chart served as application/octet-stream is a
-- chart Slack refuses to download — which rejects the whole alert message
-- (invalid_attachments), not just the image.
data PNG
  deriving (Accept) via Mime "image/png"


instance MimeRender PNG LBS.ByteString where
  mimeRender _ = id


-- | The learned OpenAPI spec, served as YAML. Response-only, so no 'MimeUnrender'.
data YAML
  deriving (Accept) via Mime "application/yaml"


instance MimeRender YAML Text where
  mimeRender _ = fromStrict . encodeUtf8


-- | The headers that decide whether an SSE response actually streams.
--
-- Every hop between this handler and the browser would rather buffer than forward: a CDN caches
-- it, nginx holds it in a proxy buffer, a "helpful" transforming proxy re-chunks it. The result
-- is the failure that looks like nothing at all — the tab says @live@, the connection is open,
-- and rows arrive in a clump minutes later or never.
--
-- @no-transform@ is the half people forget: @no-cache@ alone still permits an intermediary to
-- re-encode the body, which is what re-introduces buffering. @X-Accel-Buffering@ is nginx's
-- (and several ingress controllers') opt-out, ignored harmlessly everywhere else.
--
-- Heartbeats every ten seconds mask most of this, which is precisely why it went unnoticed:
-- they keep the connection alive but do nothing about a hop that batches what it forwards.
-- See also the @text/event-stream@ exclusion in "System.Server"'s gzip settings — compression
-- is the one buffering hop headers cannot talk us out of.
type SseHeaders =
  Headers
    '[ Servant.Header "Cache-Control" Text
     , Servant.Header "X-Accel-Buffering" Text
     ]


-- Filter record for dashboard list
newtype DashboardFilters = DashboardFilters
  { tag :: [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromForm)


-- | The slugs are the existing wire spellings, so live URLs are unchanged:
--
-- >>> map toUrlPiece [minBound .. maxBound :: DashboardBulkAction]
-- ["delete","add_teams","remove_teams"]
data DashboardBulkAction = BADelete | BAAddTeams | BARemoveTeams
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (FromHttpApiData, ToHttpApiData) via WrappedEnumSC 'Nothing "BA" DashboardBulkAction


data RepositoryTab = Overview | PullRequests | Fixes | Usage | Wiki
  deriving stock (Eq, Show)


instance FromHttpApiData RepositoryTab where
  parseUrlPiece value = maybeToRight "Choose repositories, reviews, fixes, usage or wiki" $ lookup value (repositoryTabs <> [("configuration", Overview)])


instance ToHttpApiData RepositoryTab where
  toUrlPiece tab = maybe "overview" fst $ find ((== tab) . snd) repositoryTabs


repositoryTabs :: [(Text, RepositoryTab)]
repositoryTabs = [("overview", Overview), ("reviews", PullRequests), ("fixes", Fixes), ("usage", Usage), ("wiki", Wiki)]


type CookieProtectedRoutes :: Type -> Type
data CookieProtectedRoutes mode = CookieProtectedRoutes
  { -- Dashboard routes
    dashboardRedirectGet :: mode :- "p" :> ProjectId :> AllQueryParams :> LocationRedirect NoContent
  , slackInstallGet :: mode :- "p" :> ProjectId :> "slack" :> "install" :> QueryFlag "onboarding" :> LocationRedirect NoContent
  , slackEvidenceGet :: mode :- "p" :> ProjectId :> "slack" :> "evidence" :> Capture "slackUser" Text :> LocationRedirect NoContent
  , slackEvidenceDisconnectPost :: mode :- "p" :> ProjectId :> "slack" :> "evidence" :> "disconnect" :> Verb 'POST 303 '[HTML] (Headers '[Header "Location" Text] NoContent)
  , slackIdentityGet :: mode :- "slack" :> "link" :> Capture "linkId" (UUIDId "slack_link") :> Get '[HTML] (RespHeaders (Html ()))
  , slackIdentityPost :: mode :- "slack" :> "link" :> Capture "linkId" (UUIDId "slack_link") :> ReqBody '[FormUrlEncoded] (Opaque "Slack.SlackLinkForm") :> Post '[HTML] (RespHeaders (Html ()))
  , slackLinkProjectGet :: mode :- "slack" :> "oauth" :> "callback" :> QPT "code" :> QPT "state" :> LocationRedirect (Opaque "BotUtils.BotResponse")
  , endpointDetailsRedirect :: mode :- "p" :> ProjectId :> "endpoints" :> "details" :> AllQueryParams :> LocationRedirect NoContent
  , dashboardViewPost :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "views" :> ReqBody '[FormUrlEncoded] (Opaque "Dashboards.DashboardViewForm") :> Action 'POST (Opaque "Dashboards.InvalidViewName") (Opaque "Dashboards.SavedViews")
  , dashboardViewDelete :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "views" :> Capture "name" Text :> AllQueryParams :> Delete '[HTML] (RespHeaders (Opaque "Dashboards.SavedViews"))
  , dashboardViewsGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "views" :> AllQueryParams :> Get '[HTML] (RespHeaders (Opaque "Dashboards.SavedViews"))
  , rumRedirect :: mode :- "p" :> ProjectId :> "rum" :> CaptureAll "view" Text :> AllQueryParams :> LocationRedirect NoContent
  , dashboardNewGet :: mode :- "p" :> ProjectId :> "dashboards" :> "new" :> Get '[HTML] (RespHeaders (Opaque "Dashboards.DashboardNew"))
  , dashboardsGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> QPT "file" :> QPT "from" :> QPT "to" :> QPT "since" :> HXRequest :> AllQueryParams :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "Dashboards.DashboardGet")))
  , dashboardsGetList :: mode :- "p" :> ProjectId :> "dashboards" :> QPT "sort" :> QPT "embedded" :> QueryParam "teamId" ProjectMembers.TeamId :> QPT "copy_widget_id" :> QueryParam "source_dashboard_id" (UUIDId "dashboard") :> QPT "new" :> RecordParam KeepPrefixExp DashboardFilters :> Get '[HTML] (RespHeaders (Opaque "Dashboards.DashboardsGet"))
  , dashboardsPost :: mode :- "p" :> ProjectId :> "dashboards" :> ReqBody '[FormUrlEncoded] (Opaque "Dashboards.DashboardForm") :> Post '[HTML] (RespHeaders (Opaque "Dashboards.DashboardRes"))
  , dashboardSettledPost :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "settled" :> ReqBody '[JSON] (Opaque "Dashboards.DashboardSettleSample") :> Post '[JSON] NoContent
  , dashboardRepositoryGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "repository" :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "GitSync.DashboardRepositoryGet")))
  , dashboardRepositoryPost :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "repository" :> ReqBody '[FormUrlEncoded] (Opaque "GitSync.DashboardRepositoryForm") :> Post '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "GitSync.DashboardRepositoryGet")))
  , dashboardWidgetPut :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> QPT "widget_id" :> QPT "tab" :> QPT "configure_monitor" :> ReqBody '[JSON] (Opaque "Widget.Widget") :> Action 'PUT (Opaque "Dashboards.WidgetMonitorRejection") (Opaque "Dashboards.WidgetSaved")
  , dashboardWidgetReorderPatchH :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "widgets_order" :> QPT "tab" :> ReqBody '[JSON] (Map Text (Opaque "Dashboards.WidgetReorderItem")) :> Action 'PATCH (Opaque "Dashboards.EditRefusal") (Opaque "Dashboards.LayoutSaved")
  , dashboardDelete :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> Delete '[HTML] (RespHeaders (Opaque "Dashboards.DashboardRes"))
  , dashboardRenamePatch :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "rename" :> ReqBody '[FormUrlEncoded] (Opaque "Dashboards.DashboardRenameForm") :> Action 'PATCH (Opaque "Dashboards.EditRefusal") (Opaque "Dashboards.DashboardRenamed")
  , dashboardDuplicatePost :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "duplicate" :> Post '[HTML] (RespHeaders (Opaque "Dashboards.DashboardRes"))
  , dashboardStarPost :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "star" :> Unrefusable 'POST (Opaque "Dashboards.StarButton")
  , dashboardDuplicateWidget :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "widgets" :> Capture "widget_id" Text :> "duplicate" :> QueryParam "source_dashboard_id" (UUIDId "dashboard") :> Action 'POST (Opaque "Dashboards.EditRefusal") (Opaque "Dashboards.WidgetCopied")
  , dashboardWidgetNewGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "widgets" :> "new" :> QPT "tab" :> QPT "range_start" :> QPT "range_end" :> Get '[HTML] (RespHeaders (Opaque "Dashboards.WidgetEditor"))
  , dashboardWidgetExpandGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "widgets" :> Capture "widget_id" Text :> "expand" :> QPT "editor_tab" :> Get '[HTML] (RespHeaders (Opaque "Dashboards.WidgetEditor"))
  , -- Widget alert routes
    widgetAlertUpsert :: mode :- "p" :> ProjectId :> "widgets" :> Capture "widget_id" Text :> "alert" :> QueryParam "dashboard_id" (UUIDId "dashboard") :> ReqBody '[FormUrlEncoded] (Opaque "Dashboards.WidgetAlertForm") :> Action 'POST (Opaque "Dashboards.WidgetMonitorRejection") (Opaque "Dashboards.WidgetMonitorSaved")
  , widgetAlertDelete :: mode :- "p" :> ProjectId :> "widgets" :> Capture "widget_id" Text :> "alert" :> QueryParam "dashboard_id" (UUIDId "dashboard") :> Unrefusable 'DELETE (Opaque "Dashboards.WidgetMonitorRemoved")
  , widgetAlertToggle :: mode :- "p" :> ProjectId :> "widgets" :> Capture "widget_id" Text :> "alert" :> "toggle_active" :> QueryParam "dashboard_id" (UUIDId "dashboard") :> Unrefusable 'POST (Opaque "Dashboards.WidgetMonitorStatus")
  , dashboardBulkActionPost :: mode :- "p" :> ProjectId :> "dashboards" :> "bulk_action" :> Capture "action" DashboardBulkAction :> ReqBody '[FormUrlEncoded] (Opaque "Dashboards.DashboardBulkActionForm") :> Post '[HTML] (RespHeaders NoContent)
  , -- Dashboard tab routes (htmx lazy loading)
    dashboardTabGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "tab" :> Capture "tab_slug" Text :> QPT "file" :> QPT "from" :> QPT "to" :> QPT "since" :> Header "HX-Target" Text :> AllQueryParams :> Get '[HTML] (RespHeaders (Opaque1 "NavigationResponse" (Opaque "Dashboards.DashboardGet")))
  , dashboardTabRenamePatch :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "tab" :> Capture "tab_slug" Text :> "rename" :> ReqBody '[FormUrlEncoded] (Opaque "Dashboards.TabRenameForm") :> Action 'PATCH (Opaque "Dashboards.EditRefusal") (Opaque "Dashboards.TabRenamed")
  , dashboardYamlGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "yaml" :> Get '[HTML] (RespHeaders (Opaque "Dashboards.DashboardYaml"))
  , dashboardSwitcherGet :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "switcher" :> Get '[HTML] (RespHeaders (Opaque "Dashboards.DashboardSwitcher"))
  , dashboardYamlPut :: mode :- "p" :> ProjectId :> "dashboards" :> Capture "dashboard_id" (UUIDId "dashboard") :> "yaml" :> ReqBody '[FormUrlEncoded] (Opaque "Dashboards.YamlForm") :> Unrefusable 'PUT (Opaque "Dashboards.YamlSave")
  , -- API routes
    apiGet :: mode :- "p" :> ProjectId :> "apis" :> Get '[HTML] (RespHeaders (Opaque "Settings.ApiGet"))
  , apiDelete :: mode :- "p" :> ProjectId :> "apis" :> Capture "keyID" ProjectApiKeys.ProjectApiKeyId :> Action 'DELETE (Opaque "Settings.ApiKeyNotFound") (Opaque "Settings.ApiMut")
  , apiPatch :: mode :- "p" :> ProjectId :> "apis" :> Capture "keyID" ProjectApiKeys.ProjectApiKeyId :> Action 'PATCH (Opaque "Settings.ApiKeyNotFound") (Opaque "Settings.ApiMut")
  , apiPost :: mode :- "p" :> ProjectId :> "apis" :> ReqBody '[FormUrlEncoded] (Opaque "Settings.GenerateAPIKeyForm") :> Unrefusable 'POST (Opaque "Settings.ApiMut")
  , -- Charts and widgets
    chartsDataGet :: mode :- "chart_data" :> QueryParam "db_source" Hasql.SqlSource :> QueryParam "data_type" Charts.DataType :> QueryParam "pid" Projects.ProjectId :> QPT "query" :> QPT "query_sql" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "source" :> QueryParam "chart_type" Parser.BinDensity :> AllQueryParams :> Get '[JSON] Charts.MetricsData
  , chartsDataStreamGet :: mode :- "chart_data" :> "stream" :> QueryParam "db_source" Hasql.SqlSource :> QueryParam "data_type" Charts.DataType :> QueryParam "pid" Projects.ProjectId :> QPT "query" :> QPT "query_sql" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "source" :> QueryParam "chart_type" Parser.BinDensity :> AllQueryParams :> StreamGet NewlineFraming Charts.ChartStream (Headers '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text] (SourceIO AE.Value))
  , widgetPost :: mode :- "p" :> ProjectId :> "widget" :> QPT "since" :> QPT "from" :> QPT "to" :> ReqBody '[JSON, FormUrlEncoded] (Opaque "Widget.Widget") :> Post '[HTML] (RespHeaders (Opaque "Widget.Widget"))
  , widgetGet :: mode :- "p" :> ProjectId :> "widget" :> QPT "widgetJSON" :> QPT "widgetZ" :> QPT "since" :> QPT "from" :> QPT "to" :> AllQueryParams :> Get '[HTML] (RespHeaders (Opaque "Widget.Widget"))
  , widgetSqlPreview :: mode :- "p" :> ProjectId :> "widget" :> "sql-preview" :> QPT "query" :> QPT "dashboard_id" :> QPT "since" :> QPT "from" :> QPT "to" :> Get '[HTML] (RespHeaders (Opaque "Dashboards.SqlPreview"))
  , widgetSqlText :: mode :- "p" :> ProjectId :> "widget" :> "sql-text" :> QPT "query" :> QPT "dashboard_id" :> QPT "since" :> QPT "from" :> QPT "to" :> Get '[PlainText] (RespHeaders Text)
  , -- Endpoints and fields
    endpointListGet :: mode :- "p" :> ProjectId :> "endpoints" :> QPT "page" :> QPT "per_page" :> QPT "layout" :> QPT "filter" :> QPT "host" :> QPT "request_type" :> QPT "sort" :> QPT "period" :> HXRequest :> HXBoosted :> HXCurrentURL :> QPT "load_more" :> QPT "search" :> QPT "stats" :> Get '[HTML] (RespHeaders ((Opaque1 "Table.TableResponse") (Opaque "ApiCatalog.EnpReqStatsVM")))
  , apiCatalogGet :: mode :- "p" :> ProjectId :> "api_catalog" :> QPT "sort" :> QPT "since" :> QPT "request_type" :> QPT "period" :> QPI "skip" :> QPT "filter" :> QPT "stats" :> Get '[HTML] (RespHeaders ((Opaque1 "Table.TableResponse") (Opaque "ApiCatalog.HostEventsVM")))
  , apiCatalogBulkAction :: mode :- "p" :> ProjectId :> "api_catalog" :> "bulk_action" :> Capture "action" Endpoints.HostBulkAction :> QPT "request_type" :> ReqBody '[FormUrlEncoded] (Opaque "ApiCatalog.HostBulkActionForm") :> Action 'POST (Opaque "ApiCatalog.CatalogBulkRejection") (Opaque "ApiCatalog.CatalogBulkAction")
  , -- The learned OpenAPI document: rendered reference, plus the raw spec in both
    -- serialisations. Static ".json"/".yaml" segments rather than content negotiation,
    -- because these URLs are meant to be pasted into curl, CI, and codegen.
    apiCatalogDocs :: mode :- "p" :> ProjectId :> "api_catalog" :> "docs" :> QPT "host" :> QPT "request_type" :> QPT "endpoint" :> Get '[HTML] (RespHeaders (Opaque "ApiCatalog.ApiDocsPage"))
  , apiCatalogSpecJson :: mode :- "p" :> ProjectId :> "api_catalog" :> "openapi.json" :> QPT "host" :> QPT "request_type" :> QPT "endpoint" :> Get '[JSON] (RespHeaders AE.Value)
  , apiCatalogSpecYaml :: mode :- "p" :> ProjectId :> "api_catalog" :> "openapi.yaml" :> QPT "host" :> QPT "request_type" :> QPT "endpoint" :> Get '[YAML] (RespHeaders Text)
  , -- Slack/Discord integration
    -- Reports are AI routines now; old report links land on them.
    reportsRedirect :: mode :- "p" :> ProjectId :> "reports" :> CaptureAll "rest" Text :> LocationRedirect NoContent
  , shareLinkPost :: mode :- "p" :> ProjectId :> "share" :> Capture "event_id" UUID.UUID :> Capture "createdAt" UTCTime :> QueryParam "event_type" ShareEvents.ShareKind :> Post '[HTML] (RespHeaders (Opaque "Share.ShareLinkPost"))
  , -- Billing
    manageBillingGet :: mode :- "p" :> ProjectId :> "manage_billing" :> Get '[HTML] (RespHeaders (Opaque "Settings.BillingGet"))
  , replaySessionGet :: mode :- "p" :> ProjectId :> "replay_session" :> Capture "sessionId" UUID.UUID :> Get '[JSON] (RespHeaders Replay.ReplaySessionResp)
  , replaySessionManifestGet :: mode :- "p" :> ProjectId :> "replay_session" :> Capture "sessionId" UUID.UUID :> "manifest" :> Get '[JSON] (RespHeaders Replay.ReplayManifest)
  , replaySessionShardGet :: mode :- "p" :> ProjectId :> "replay_session" :> Capture "sessionId" UUID.UUID :> "shard" :> QueryParam "key" Text :> Get '[JSON] (RespHeaders Replay.RawJson)
  , bringS3 :: mode :- "p" :> ProjectId :> "byob_s3" :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "Settings.StorageGet")))
  , bringS3Post :: mode :- "p" :> ProjectId :> "byob_s3" :> ReqBody '[FormUrlEncoded] Projects.ProjectS3Bucket :> Post '[HTML] (RespHeaders (Opaque "Settings.StorageGet"))
  , bringS3Remove :: mode :- "p" :> ProjectId :> "byob_s3" :> Delete '[HTML] (RespHeaders (Opaque "Settings.StorageGet"))
  , gitSyncSettings :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoriesGet")))
  , projectWikiGet :: mode :- "p" :> ProjectId :> "wiki" :> CaptureAll "path" Text :> QueryParam "after" ProjectMemory.PagePath :> QueryParam "q" Text :> QueryParam "folder" Text :> QueryFlag "edit" :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.WikiGet")))
  , projectMemoryGet :: mode :- "p" :> ProjectId :> "memories" :> QueryParam "page" (UUIDId "memory_page") :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.WikiGet")))
  , projectMemoryPost :: mode :- "p" :> ProjectId :> "memories" :> ReqBody '[FormUrlEncoded] (Opaque "PageCodeContext.MemoryForm") :> Post '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.WikiGet")))
  , repositoriesGet :: mode :- "p" :> ProjectId :> "repositories" :> QueryParam "tab" RepositoryTab :> QueryParam "sample" Text :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoriesGet")))
  , serviceRepositoriesGet :: mode :- "p" :> ProjectId :> "repositories" :> "services" :> Capture "service" Text :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoriesGet")))
  , repositoryConnectGet :: mode :- "p" :> ProjectId :> "repositories" :> "connect" :> QueryParam "credentialId" (UUIDId "github_credential") :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoryConnectGet")))
  , repositoryConnectPost :: mode :- "p" :> ProjectId :> "repositories" :> "connect" :> QueryParam "credentialId" (UUIDId "github_credential") :> ReqBody '[FormUrlEncoded] (Opaque "PageCodeContext.RepositoryConnectForm") :> Post '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoryConnectGet")))
  , repositoryTokenGet :: mode :- "p" :> ProjectId :> "repositories" :> "connect" :> "token" :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoryTokenGet")))
  , repositoryTokenPost :: mode :- "p" :> ProjectId :> "repositories" :> "connect" :> "token" :> ReqBody '[FormUrlEncoded] (Opaque "PageCodeContext.RepositoryTokenForm") :> Post '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoryTokenGet")))
  , repositoryGet :: mode :- "p" :> ProjectId :> "repositories" :> Capture "repository_id" (UUIDId "repository") :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoryGet")))
  , repositoryDelete :: mode :- "p" :> ProjectId :> "repositories" :> Capture "repository_id" (UUIDId "repository") :> Unrefusable 'DELETE (Opaque "PageCodeContext.RepositoryRemoval")
  , repositoryDashboardGet :: mode :- "p" :> ProjectId :> "repositories" :> Capture "repository_id" (UUIDId "repository") :> "dashboards" :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "GitSync.RepositoryDashboardGet")))
  , repositoryDashboardPost :: mode :- "p" :> ProjectId :> "repositories" :> Capture "repository_id" (UUIDId "repository") :> "dashboards" :> ReqBody '[FormUrlEncoded] (Opaque "GitSync.RepositoryDashboardForm") :> Post '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "GitSync.RepositoryDashboardGet")))
  , repositoryAnalyzePost :: mode :- "p" :> ProjectId :> "repositories" :> Capture "repository_id" (UUIDId "repository") :> "analyze" :> Post '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoryGet")))
  , repositoryResumePost :: mode :- "p" :> ProjectId :> "repositories" :> Capture "repository_id" (UUIDId "repository") :> "resume" :> Post '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoryGet")))
  , repositoryFolderPost :: mode :- "p" :> ProjectId :> "repositories" :> Capture "repository_id" (UUIDId "repository") :> "folder" :> ReqBody '[FormUrlEncoded] (Opaque "PageCodeContext.FolderForm") :> Post '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoryGet")))
  , repositoryConflictPost :: mode :- "p" :> ProjectId :> "repositories" :> Capture "repository_id" (UUIDId "repository") :> "resources" :> Capture "resource_id" (UUIDId "repository_resource") :> "conflict" :> ReqBody '[FormUrlEncoded] (Opaque "PageCodeContext.ConflictForm") :> Post '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoryGet")))
  , repositorySourceEnabledPost :: mode :- "p" :> ProjectId :> "repositories" :> Capture "repository_id" (UUIDId "repository") :> "source" :> "enabled" :> ReqBody '[FormUrlEncoded] (Opaque "PageCodeContext.SourceContextForm") :> Post '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoryGet")))
  , repositorySourceGet :: mode :- "p" :> ProjectId :> "repositories" :> Capture "repository_id" (UUIDId "repository") :> "source" :> QueryParam "credentialId" (UUIDId "github_credential") :> QueryParam "sample" Text :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositorySourceGet")))
  , repositorySourcePost :: mode :- "p" :> ProjectId :> "repositories" :> Capture "repository_id" (UUIDId "repository") :> "source" :> ReqBody '[FormUrlEncoded] (Opaque "PageCodeContext.CodeMappingForm") :> Action 'POST (Opaque "PageCodeContext.MappingRejection") (Opaque "PageCodeContext.MappingsPanel")
  , repositorySourceDelete :: mode :- "p" :> ProjectId :> "repositories" :> Capture "repository_id" (UUIDId "repository") :> "source" :> Capture "mapping_id" (UUIDId "CodeMapping") :> Unrefusable 'DELETE (Opaque "PageCodeContext.MappingsPanel")
  , repositoryReviewsPost :: mode :- "p" :> ProjectId :> "repositories" :> Capture "repository_id" (UUIDId "repository") :> "source" :> "reviews" :> ReqBody '[FormUrlEncoded] ImpactReviews.ReviewSettings :> Unrefusable 'POST ((Opaque1 "PageCodeContext.ReviewChange") (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoryGet")))
  , gitSyncSettingsPost :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> ReqBody '[FormUrlEncoded] (Opaque "GitSync.GitSyncForm") :> Action 'POST Git.GitConnError (Opaque "GitSync.GitSyncSettings")
  , gitSyncSettingsDelete :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> Unrefusable 'DELETE (Opaque "GitSync.GitSyncRemoval")
  , gitSyncRepositoryUpdate :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> Capture "id" (UUIDId "github_sync") :> ReqBody '[FormUrlEncoded] (Opaque "GitSync.GitSyncForm") :> Action 'POST Git.GitConnError (Opaque "GitSync.GitSyncSettings")
  , gitSyncRepositoryRetry :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> Capture "id" (UUIDId "github_sync") :> "retry" :> Unrefusable 'POST (Opaque "GitSync.GitSyncSettings")
  , gitSyncRepositoryPause :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> Capture "id" (UUIDId "github_sync") :> "pause" :> Unrefusable 'POST (Opaque "GitSync.GitSyncSettings")
  , gitSyncRepositoryDelete :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> Capture "id" (UUIDId "github_sync") :> Unrefusable 'DELETE (Opaque "GitSync.GitSyncRemoval")
  , codeMappingsSettings :: mode :- "p" :> ProjectId :> "settings" :> "code-mappings" :> QueryParam "sample" Text :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoriesGet")))
  , codeMappingsEditor :: mode :- "p" :> ProjectId :> "settings" :> "code-mappings" :> "editor" :> QueryParam "credentialId" (UUIDId "github_credential") :> QueryParam "sample" Text :> Get '[HTML] (RespHeaders (Opaque "PageCodeContext.MappingsPanel"))
  , codeMappingsPost :: mode :- "p" :> ProjectId :> "settings" :> "code-mappings" :> ReqBody '[FormUrlEncoded] (Opaque "PageCodeContext.CodeMappingForm") :> Action 'POST (Opaque "PageCodeContext.MappingRejection") (Opaque "PageCodeContext.MappingsPanel")
  , impactReviewSettingsPost :: mode :- "p" :> ProjectId :> "settings" :> "pr-reviews" :> ReqBody '[FormUrlEncoded] ImpactReviews.ReviewSettings :> Unrefusable 'POST ((Opaque1 "PageCodeContext.ReviewChange") (Opaque "PageCodeContext.MappingsPanel"))
  , issueFixGet :: mode :- "p" :> ProjectId :> "issue-fixes" :> Capture "id" (UUIDId "issue_fix") :> Get '[HTML] (Headers '[Header "Cache-Control" Text] (Html ()))
  , issueFixPackage :: mode :- "p" :> ProjectId :> "issue-fixes" :> Capture "id" (UUIDId "issue_fix") :> "publication.tar" :> RawM
  , issueFixMedia :: mode :- "p" :> ProjectId :> "issue-fixes" :> Capture "id" (UUIDId "issue_fix") :> "media" :> Capture "asset" Artifacts.Asset :> RawM
  , impactReviewRetry :: mode :- "p" :> ProjectId :> "settings" :> "pr-reviews" :> Capture "id" ImpactReviews.ReviewId :> "rerun" :> Unrefusable 'POST ((Opaque1 "PageCodeContext.ReviewChange") (Opaque1 "PageCtx" (Opaque "PageCodeContext.RepositoriesGet")))
  , codeMappingsDelete :: mode :- "p" :> ProjectId :> "settings" :> "code-mappings" :> Capture "id" (UUIDId "CodeMapping") :> Unrefusable 'DELETE (Opaque "PageCodeContext.MappingsPanel")
  , prometheusSettingsGet :: mode :- "p" :> ProjectId :> "settings" :> "prometheus" :> Get '[HTML] (RespHeaders (Opaque "Settings.PrometheusGet"))
  , prometheusSettingsPost :: mode :- "p" :> ProjectId :> "settings" :> "prometheus" :> ReqBody '[FormUrlEncoded] (Opaque "Settings.PrometheusForm") :> Post '[HTML] (RespHeaders (Opaque "Settings.PrometheusMut"))
  , prometheusSettingsTest :: mode :- "p" :> ProjectId :> "settings" :> "prometheus" :> "test" :> ReqBody '[FormUrlEncoded] (Opaque "Settings.PrometheusForm") :> Unrefusable 'POST (Opaque "Settings.ScrapeProbe")
  , prometheusSettingsEdit :: mode :- "p" :> ProjectId :> "settings" :> "prometheus" :> Capture "cfgID" PromCfg.PrometheusScrapeConfigId :> "edit" :> ReqBody '[FormUrlEncoded] (Opaque "Settings.PrometheusForm") :> Post '[HTML] (RespHeaders (Opaque "Settings.PrometheusMut"))
  , prometheusSettingsToggle :: mode :- "p" :> ProjectId :> "settings" :> "prometheus" :> Capture "cfgID" PromCfg.PrometheusScrapeConfigId :> Patch '[HTML] (RespHeaders (Opaque "Settings.PrometheusMut"))
  , prometheusSettingsDelete :: mode :- "p" :> ProjectId :> "settings" :> "prometheus" :> Capture "cfgID" PromCfg.PrometheusScrapeConfigId :> Delete '[HTML] (RespHeaders (Opaque "Settings.PrometheusMut"))
  , -- GitHub App routes
    githubAppInstall :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> "install" :> QueryParam "to" Text :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "GitSync.GitHubConnect")))
  , githubAppCallback :: mode :- "github" :> "callback" :> QueryParam "installation_id" Int64 :> QueryParam "setup_action" Text :> QueryParam "state" Text :> QueryParam "code" Text :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "GitSync.GitHubConnect")))
  , githubAppRepos :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> "repos" :> QueryParam "installationId" Int64 :> Get '[HTML] (RespHeaders (Html ()))
  , githubAppSelectRepo :: mode :- "p" :> ProjectId :> "settings" :> "git-sync" :> "select" :> ReqBody '[FormUrlEncoded] (Opaque "GitSync.RepoSelectForm") :> Action 'POST (Opaque "GitSync.SelectRejection") (Opaque "GitSync.SyncPanels")
  , -- Command palette
    commandPaletteGet :: mode :- "p" :> ProjectId :> "command-palette" :> Get '[HTML] (RespHeaders (Html ())) -- dynamic items only
  , commandPaletteRecentPost :: mode :- "p" :> ProjectId :> "command-palette" :> "recents" :> ReqBody '[FormUrlEncoded] (Opaque "CommandPalette.RecentForm") :> Post '[HTML] (RespHeaders NoContent)
  , aiRoutinesGet :: mode :- "p" :> ProjectId :> "ai" :> "routines" :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Html ())))
  , aiRoutineInstallPost :: mode :- "p" :> ProjectId :> "ai" :> "routines" :> "install" :> ReqBody '[FormUrlEncoded] (Opaque "AIThreads.RoutineTemplateForm") :> Action 'POST (Opaque "AIThreads.UnknownTemplate") (Opaque "AIThreads.AIPage")
  , aiThreadsGet :: mode :- "p" :> ProjectId :> "ai" :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Html ())))
  , aiOperationGet :: mode :- "p" :> ProjectId :> "ai" :> "operations" :> Capture "operation_id" (UUIDId "agent_operation") :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Html ())))
  , aiOperationReviewPost :: mode :- "p" :> ProjectId :> "ai" :> "operations" :> Capture "operation_id" (UUIDId "agent_operation") :> "review" :> ReqBody '[FormUrlEncoded] (Opaque "AIThreads.OperationReviewForm") :> Unrefusable 'POST (Opaque "AIThreads.OperationReviewed")
  , aiRoutineRunGet :: mode :- "p" :> ProjectId :> "ai" :> "runs" :> Capture "run_id" (UUIDId "ai_routine_run") :> QueryParam "conversation" (UUIDId "conversation") :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Html ())))
  , aiRoutineInstructionsPost :: mode :- "p" :> ProjectId :> "ai" :> Capture "conversation_id" (UUIDId "conversation") :> "routine" :> "instructions" :> ReqBody '[FormUrlEncoded] (Opaque "AIThreads.RoutineInstructionsForm") :> Action 'POST (Opaque "AIThreads.PromptError") (Opaque "AIThreads.AIPage")
  , aiDockGet :: mode :- "p" :> ProjectId :> "ai" :> "dock" :> QueryParam "conversation" (UUIDId "conversation") :> QueryParam "seen" Int :> Header "HX-Current-URL" Text :> Get '[HTML] (RespHeaders (Opaque "AIThreads.CopilotDock"))
  , aiDockPost :: mode :- "p" :> ProjectId :> "ai" :> "dock" :> ReqBody '[FormUrlEncoded] (Opaque "AIThreads.CopilotForm") :> Action 'POST (Opaque "AIThreads.TurnError") (Opaque "AIThreads.CopilotDock")
  , aiThreadGet :: mode :- "p" :> ProjectId :> "ai" :> Capture "conversation_id" (UUIDId "conversation") :> QueryParam "doc" Issues.ArtifactKey :> Header "HX-Target" Text :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Html ())))
  , aiThreadsPost :: mode :- "p" :> ProjectId :> "ai" :> ReqBody '[FormUrlEncoded] (Opaque "AIThreads.AIChatForm") :> Action 'POST (Opaque "AIThreads.NewThreadError") (Opaque "AIThreads.AIPage")
  , aiThreadPost :: mode :- "p" :> ProjectId :> "ai" :> Capture "conversation_id" (UUIDId "conversation") :> ReqBody '[FormUrlEncoded] (Opaque "AIThreads.AIChatForm") :> Action 'POST (Opaque "AIThreads.TurnError") (Opaque "AIThreads.AIPage")
  , aiThreadTitlePost :: mode :- "p" :> ProjectId :> "ai" :> Capture "conversation_id" (UUIDId "conversation") :> "title" :> ReqBody '[FormUrlEncoded] (Opaque "AIThreads.TitleForm") :> Action 'POST (Opaque "AIThreads.EmptyTitle") (Opaque "AIThreads.AIPage")
  , aiThreadDelete :: mode :- "p" :> ProjectId :> "ai" :> Capture "conversation_id" (UUIDId "conversation") :> Unrefusable 'DELETE (Opaque "AIThreads.AIPage")
  , aiRoutinePost :: mode :- "p" :> ProjectId :> "ai" :> Capture "conversation_id" (UUIDId "conversation") :> "routine" :> ReqBody '[FormUrlEncoded] (Opaque "AIThreads.RoutineForm") :> Unrefusable 'POST (Opaque "AIThreads.AIPage")
  , aiRoutinePausePost :: mode :- "p" :> ProjectId :> "ai" :> Capture "conversation_id" (UUIDId "conversation") :> "routine" :> "pause" :> Unrefusable 'POST (Opaque "AIThreads.AIPage")
  , aiRoutineCancelPost :: mode :- "p" :> ProjectId :> "ai" :> Capture "conversation_id" (UUIDId "conversation") :> "routine" :> "cancel" :> Unrefusable 'POST (Opaque "AIThreads.AIPage")
  , aiRoutineResumePost :: mode :- "p" :> ProjectId :> "ai" :> Capture "conversation_id" (UUIDId "conversation") :> "routine" :> "resume" :> QueryFlag "now" :> Unrefusable 'POST (Opaque "AIThreads.AIPage")
  , aiRoutinePolicyPost :: mode :- "p" :> ProjectId :> "ai" :> Capture "conversation_id" (UUIDId "conversation") :> "routine" :> "policy" :> ReqBody '[FormUrlEncoded] (Opaque "AIThreads.RoutinePolicyForm") :> Action 'POST (Opaque "AIThreads.NoReviewClasses") (Opaque "AIThreads.PolicySaved")
  , aiRoutineDestinationPost :: mode :- "p" :> ProjectId :> "ai" :> Capture "conversation_id" (UUIDId "conversation") :> "routine" :> "destination" :> ReqBody '[FormUrlEncoded] (Opaque "AIThreads.RoutineDestinationForm") :> Action 'POST (Opaque "AIThreads.EmailOnlyForReports") (Opaque "AIThreads.AIPage")
  , aiRoutineDelete :: mode :- "p" :> ProjectId :> "ai" :> Capture "conversation_id" (UUIDId "conversation") :> "routine" :> Unrefusable 'DELETE (Opaque "AIThreads.AIPage")
  , -- Device auth
    deviceApprove :: mode :- "device" :> QPT "code" :> Get '[HTML] (RespHeaders (Html ()))
  , deviceApprovePost :: mode :- "device" :> QPT "code" :> Post '[HTML] (RespHeaders (Html ()))
  , -- Sub-route groups
    projects :: mode :- ProjectsRoutes
  , issues :: mode :- "p" :> ProjectId :> "issues" :> IssuesRoutes
  , logExplorer :: mode :- "p" :> ProjectId :> LogExplorerRoutes
  , monitors :: mode :- "p" :> ProjectId :> "monitors" :> MonitorsRoutes
  , traces :: mode :- "p" :> ProjectId :> TelemetryRoutes
  }
  deriving stock (Generic)


type LogExplorerRoutes = NamedRoutes LogExplorerRoutes'


type LogExplorerRoutes' :: Type -> Type
data LogExplorerRoutes' mode = LogExplorerRoutes'
  { logExplorerGet :: mode :- "log_explorer" :> QPT "query" :> QPT "cols" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "source" :> QPT "target-spans" :> QPT "target_event" :> QPT "showTrace" :> QPT "viz_type" :> QPT "alert" :> QPT "pattern_target" :> QPT "sort_by" :> Get '[HTML, JSON] (RespHeaders (Opaque "Log.LogsGet"))
  , logExplorerDataGet :: mode :- "log_explorer" :> "data" :> RecordParam LogDataFieldExp LogQueries.BrowserParams :> RecordParam LogDataFieldExp LogQueries.LogDataQuery :> Get '[JSON] (RespHeaders (Opaque "ApiT.LogResult"))
  , logExplorerPatternsGet :: mode :- "log_explorer" :> "patterns" :> RecordParam LogDataFieldExp LogQueries.BrowserParams :> QPT "query" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "source" :> QPT "pattern_target" :> QPI "aggregate_skip" :> Get '[JSON] (RespHeaders (Opaque "Log.PatternsView"))
  , logExplorerSessionsGet :: mode :- "log_explorer" :> "sessions" :> RecordParam LogDataFieldExp LogQueries.BrowserParams :> QPT "query" :> QPT "since" :> QPT "from" :> QPT "to" :> QPI "aggregate_skip" :> QPT "sort_by" :> QPT "cursor" :> Get '[JSON] (RespHeaders (Opaque "Log.SessionsView"))
  , logExplorerSchemaGet :: mode :- "log_explorer" :> "schema" :> Get '[JSON] (RespHeaders AE.Value)
  , -- `source` matters: a metrics query reads a different table, so validating its fields
    -- against otel_logs_and_spans squiggles the metrics table's own columns. Every sibling
    -- endpoint here already takes it.
    logExplorerValidateGet :: mode :- "log_explorer" :> "validate" :> QPT "query" :> QPT "source" :> Get '[JSON] (RespHeaders (Opaque "Log.QueryValidation"))
  , logExplorerFacetsGet :: mode :- "log_explorer" :> "facets" :> QPT "field" :> QPT "group" :> Get '[HTML] (RespHeaders (Opaque "Log.FacetsView"))
  , queryLibraryGet :: mode :- "log_explorer" :> "queries" :> Get '[HTML] (RespHeaders (Opaque "QueryLibrary.QueryLibraryView"))
  , saveQueryPost :: mode :- "log_explorer" :> "queries" :> ReqBody '[FormUrlEncoded] (Opaque "QueryLibrary.SaveQueryForm") :> Post '[HTML] (RespHeaders (Opaque "QueryLibrary.QueryLibraryView"))
  , deleteQueryPost :: mode :- "log_explorer" :> "queries" :> Capture "id" Text :> Delete '[HTML] (RespHeaders (Opaque "QueryLibrary.QueryLibraryView"))
  , alertFormGet :: mode :- "log_explorer" :> "alert_form" :> QPT "alert" :> Get '[HTML] (RespHeaders (Opaque "Log.AlertForm"))
  , logExplorerItemDetailedGet :: mode :- "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" UTCTime :> "detailed" :> QPT "source" :> QPT "tab" :> QPT "subtab" :> QueryFlag "partial" :> Get '[HTML] (RespHeaders (Opaque "LogItem.ApiItemDetailed"))
  , logExplorerRelatedMetricsGet :: mode :- "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" UTCTime :> "related_metrics" :> Get '[HTML] (RespHeaders (Opaque "Metrics.RelatedMetrics"))
  , logExplorerExpandGet :: mode :- "log_explorer" :> "expand" :> QPT "kind" :> QPT "key" :> QPI "skip" :> QPT "query" :> QPT "since" :> QPT "from" :> QPT "to" :> Get '[JSON] (RespHeaders AE.Value)
  , aiSearchPost :: mode :- "log_explorer" :> "ai_search" :> ReqBody '[JSON] AE.Value :> Post '[JSON] (RespHeaders AE.Value)
  , liveTailGet :: mode :- "live_tail" :> Get '[HTML] (RespHeaders (Opaque "LiveTail.LiveTailGet"))
  , liveTailRecordGet :: mode :- "live_tail" :> "records" :> Capture "recordId" UUID.UUID :> Capture "timestamp" UTCTime :> Get '[JSON] (RespHeaders Telemetry.OtelLogsAndSpans)
  , liveTailRegisterPost :: mode :- "live_tail" :> "subscriptions" :> ReqBody '[JSON] LT.NewSubscription :> Post '[JSON] (RespHeaders (Opaque "LiveTail.RegisterResponse"))
  , -- Streams for as long as the browser holds it open, so it is the one route here that is
    -- not a plain request/response — see 'LiveTail.EventStream'.
    liveTailStreamGet :: mode :- "live_tail" :> "subscriptions" :> Capture "subscriptionId" Text :> "stream" :> StreamGet NoFraming (Opaque "LiveTail.EventStream") (SseHeaders (SourceIO Builder))
  , liveTailRenewPost :: mode :- "live_tail" :> "subscriptions" :> Capture "subscriptionId" Text :> "renew" :> Post '[JSON] (RespHeaders AE.Value)
  , liveTailDelete :: mode :- "live_tail" :> "subscriptions" :> Capture "subscriptionId" Text :> Delete '[JSON] (RespHeaders AE.Value)
  , -- Source around one stack frame. Its own endpoint rather than part of the detail panel
    -- because resolving it is a call out to the SCM API: a thirty-frame trace must not make
    -- the error panel wait on thirty round trips it may never need.
    codeContextGet :: mode :- "code_context" :> QPT "file" :> QPI "line" :> QPT "service" :> QPT "revision" :> Get '[HTML] (RespHeaders (Opaque "PageCodeContext.FrameSource"))
  }
  deriving stock (Generic)


type IssuesRoutes = NamedRoutes IssuesRoutes'


type IssuesRoutes' :: Type -> Type
data IssuesRoutes' mode = IssuesRoutes'
  { acknowledgeGet :: mode :- Capture "issueID" Issues.IssueId :> "acknowledge" :> QueryParam "duration" Int :> Post '[HTML] (RespHeaders (Opaque "IssuesPage.IssueAction"))
  , unAcknowledgeGet :: mode :- Capture "issueID" Issues.IssueId :> "unacknowledge" :> Post '[HTML] (RespHeaders (Opaque "IssuesPage.IssueAction"))
  , archiveGet :: mode :- Capture "issueID" Issues.IssueId :> "archive" :> QueryParam "window" Issues.ArchiveWindow :> Post '[HTML] (RespHeaders (Opaque "IssuesPage.IssueAction"))
  , resolveGet :: mode :- Capture "issueID" Issues.IssueId :> "resolve" :> Post '[HTML] (RespHeaders (Opaque "IssuesPage.IssueAction"))
  , triagePost :: mode :- Capture "issueID" Issues.IssueId :> "triage" :> ReqBody '[FormUrlEncoded] (Opaque "IssuesPage.TriageForm") :> Action 'POST (Opaque "IssuesPage.TriageRejection") (Opaque "IssuesPage.IssueTriage")
  , viewSavePost :: mode :- "views" :> ReqBody '[FormUrlEncoded] (Opaque "IssuesPage.SaveViewForm") :> Action 'POST (Opaque "IssuesPage.UnnamedView") (Opaque "IssuesPage.ViewChanged")
  , viewDeletePost :: mode :- "views" :> Capture "viewID" UUID.UUID :> "delete" :> Unrefusable 'POST (Opaque "IssuesPage.ViewChanged")
  , viewedPost :: mode :- Capture "issueID" Issues.IssueId :> "viewed" :> Unrefusable 'POST (Opaque "IssuesPage.TimelineLogged")
  , commentPost :: mode :- Capture "issueID" Issues.IssueId :> "comment" :> ReqBody '[FormUrlEncoded] (Opaque "IssuesPage.CommentForm") :> Action 'POST (Opaque "IssuesPage.TimelineRejection") (Opaque "IssuesPage.TimelineLogged")
  , linkPost :: mode :- Capture "issueID" Issues.IssueId :> "link" :> ReqBody '[FormUrlEncoded] (Opaque "IssuesPage.LinkForm") :> Action 'POST (Opaque "IssuesPage.TimelineRejection") (Opaque "IssuesPage.TimelineLogged")
  , unarchiveGet :: mode :- Capture "issueID" Issues.IssueId :> "unarchive" :> Post '[HTML] (RespHeaders (Opaque "IssuesPage.IssueAction"))
  , bulkActionsPost :: mode :- "bulk_actions" :> Capture "action" Issues.IssueBulkAction :> QueryParam "duration" Int :> QueryParam "value" Text :> ReqBody '[FormUrlEncoded] (Opaque "IssuesPage.IssueBulkForm") :> Post '[HTML] (RespHeaders (Opaque "IssuesPage.IssueAction"))
  , listGet :: mode :- QPT "filter" :> QPT "sort" :> QPT "since" :> QPT "page" :> QPT "per_page" :> QPT "load_more" :> QPT "period" :> QueryParams "service" Text :> QueryParams "type" Text :> Get '[HTML] (RespHeaders ((Opaque1 "Table.TableResponse") (Opaque "IssuesPage.IssueVM")))
  , stepGet :: mode :- Capture "issueID" Issues.IssueId :> "step" :> QueryParam "dir" Telemetry.EventStep :> QueryParam "event" Issues.EventRef :> LocationRedirect NoContent
  , detailGet :: mode :- Capture "issueID" Issues.IssueId :> QPT "first_occurrence" :> QPT "since" :> QPT "from" :> QPT "to" :> QueryParam "event" Issues.EventRef :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "IssuesPage.IssueDetail")))
  , detailHashGet :: mode :- "by_hash" :> Capture "issueHash" Text :> QPT "first_occurrence" :> QPT "since" :> QPT "from" :> QPT "to" :> QueryParam "event" Issues.EventRef :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "IssuesPage.IssueDetail")))
  , assignErrorPost :: mode :- "errors" :> Capture "errorID" UUID.UUID :> "assign" :> ReqBody '[FormUrlEncoded] (Opaque "IssuesPage.AssignErrorForm") :> Action 'POST (Opaque "IssuesPage.ErrorPatternRejection") (Opaque "IssuesPage.AssigneeChanged")
  , resolveErrorPost :: mode :- "errors" :> Capture "errorID" UUID.UUID :> "resolve" :> QueryFlag "next_release" :> Action 'POST (Opaque "IssuesPage.ErrorPatternRejection") (Opaque "IssuesPage.ErrorResolved")
  , errorSubscriptionPost :: mode :- "errors" :> Capture "errorID" UUID.UUID :> "subscribe" :> ReqBody '[FormUrlEncoded] (Opaque "IssuesPage.ErrorSubscriptionForm") :> Action 'POST (Opaque "IssuesPage.ErrorPatternRejection") (Opaque "IssuesPage.ErrorSubscribed")
  , aiChatPost :: mode :- Capture "issueID" Issues.IssueId :> "ai_chat" :> ReqBody '[FormUrlEncoded] (Opaque "IssuesPage.AIChatForm") :> Post '[HTML] (RespHeaders (Html ()))
  , aiChatHistoryGet :: mode :- Capture "issueID" Issues.IssueId :> "ai_chat" :> "history" :> Get '[HTML] (RespHeaders (Opaque "IssuesPage.IssueChatHistory"))
  , sampleGet :: mode :- Capture "issueID" Issues.IssueId :> "sample" :> QPT "since" :> QPT "from" :> QPT "to" :> Get '[HTML] (RespHeaders (Opaque "IssuesPage.IssueSample"))
  , activityGet :: mode :- Capture "issueID" Issues.IssueId :> "activity" :> QPT "trace_id" :> QPU "trace_ts" :> Get '[HTML] (RespHeaders (Opaque "IssuesPage.IssueActivityFeed"))
  , errorGroupMembersGet :: mode :- "errors" :> Capture "errorID" UUID.UUID :> "group_members" :> Get '[HTML] (RespHeaders (Opaque "IssuesPage.ErrorGroupMembers"))
  , errorUnmergePost :: mode :- "errors" :> Capture "errorID" UUID.UUID :> "unmerge" :> Unrefusable 'POST (Opaque "IssuesPage.PatternUnmerged")
  }
  deriving stock (Generic)


type TelemetryRoutes = NamedRoutes TelemetryRoutes'


type TelemetryRoutes' :: Type -> Type
data TelemetryRoutes' mode = TelemetryRoutes'
  { tracesGet :: mode :- "traces" :> Capture "trace_id" Text :> QPU "timestamp" :> QPT "span_id" :> QPT "nav" :> QPT "embed" :> QueryParam "spans" Int :> Get '[HTML] (RespHeaders (Opaque "Trace.TraceDetailsGet"))
  , metricsOVGetH :: mode :- "metrics" :> QPT "tab" :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "metric_source" :> QPT "metric_prefix" :> QPI "cursor" :> QPT "expand" :> QPT "label" :> QPT "q" :> QueryParam "append" Bool :> Get '[HTML] (RespHeaders (Opaque "Metrics.MetricsOverViewGet"))
  , dataPointCountsGetH :: mode :- "metrics" :> "datapoints" :> "counts" :> QPT "from" :> QPT "to" :> QPT "since" :> Get '[HTML] (RespHeaders (Opaque "Metrics.DataPointCounts"))
  , metricDetailsGetH :: mode :- "metrics" :> "details" :> Capture "metric_name" Text :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "metric_source" :> QPT "label" :> Get '[HTML] (RespHeaders (Opaque "Metrics.MetricDetailsGet"))
  , metricRelatedGetH :: mode :- "metrics" :> "details" :> Capture "metric_name" Text :> "related" :> QPT "metric_source" :> Get '[HTML] (RespHeaders (Opaque "Metrics.MetricFragment"))
  , metricBreakdownGetH :: mode :- "metrics" :> "details" :> Capture "metric_name" Text :> "breakdown" :> QPT "label" :> Get '[HTML] (RespHeaders (Opaque "Metrics.MetricFragment"))
  , -- HTML for the metric page's Exemplars tab, JSON for the chart's diamond overlay:
    -- one query, so the list and the markers can't disagree.
    metricExemplarsGetH :: mode :- "metrics" :> "details" :> Capture "metric_name" Text :> "exemplars" :> QPT "from" :> QPT "to" :> QPT "since" :> Get '[HTML, JSON] (RespHeaders (Opaque "Metrics.MetricExemplarsGet"))
  , metricCardGetH :: mode :- "metrics" :> "card" :> Capture "metric_name" Text :> QPT "label" :> QPT "metric_source" :> Get '[HTML] (RespHeaders (Opaque "Metrics.MetricFragment"))
  , serviceMapGetH :: mode :- "service_map" :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "env" :> QPT "endpoint_hash" :> Get '[HTML] (RespHeaders (Opaque "ServiceMap.ServiceMapGet"))
  , metricServicesGetH :: mode :- "metrics" :> "services" :> QPT "service_search" :> QPT "metric_source" :> Get '[HTML] (RespHeaders (Opaque "Metrics.MetricServices"))
  , hostsGetH :: mode :- "infrastructure" :> "hosts" :> QPT "provider" :> QPT "region" :> QPT "os" :> QPT "integration" :> QPT "group" :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "deferred" :> Get '[HTML] (RespHeaders (Opaque "Infrastructure.HostsGet"))
  , hostDetailGetH :: mode :- "infrastructure" :> "hosts" :> "detail" :> QPT "host" :> QPT "from" :> QPT "to" :> QPT "since" :> Get '[HTML] (RespHeaders (Opaque "Infrastructure.DetailGet"))
  , infrastructureContainersGetH :: mode :- "infrastructure" :> "containers" :> QPT "runtime" :> QPT "namespace" :> QPT "node" :> QPT "image" :> QPT "cluster" :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "deferred" :> Get '[HTML] (RespHeaders (Opaque "Containers.ContainersGet"))
  , infrastructureContainerDetailGetH :: mode :- "infrastructure" :> "containers" :> "detail" :> QPT "container" :> QPT "pod" :> QueryParam "scope" Scope :> QPT "cluster" :> QPT "namespace" :> QPT "node" :> QPT "from" :> QPT "to" :> QPT "since" :> Get '[HTML] (RespHeaders (Html ()))
  , imagesGetH :: mode :- "infrastructure" :> "images" :> QPT "runtime" :> QPT "registry" :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "deferred" :> Get '[HTML] (RespHeaders (Opaque "Infrastructure.ImagesGet"))
  , imageDetailGetH :: mode :- "infrastructure" :> "images" :> "detail" :> QPT "image" :> QPT "from" :> QPT "to" :> QPT "since" :> Get '[HTML] (RespHeaders (Opaque "Infrastructure.DetailGet"))
  , kubernetesGetH :: mode :- "infrastructure" :> "kubernetes" :> QPT "resource" :> QPT "cluster" :> QPT "namespace" :> QPT "status" :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "deferred" :> Get '[HTML] (RespHeaders (Opaque "Infrastructure.KubernetesGet"))
  , kubernetesDetailGetH :: mode :- "infrastructure" :> "kubernetes" :> "detail" :> QPT "resource" :> QPT "name" :> QPT "cluster" :> QPT "namespace" :> QPT "from" :> QPT "to" :> QPT "since" :> Get '[HTML] (RespHeaders (Opaque "Infrastructure.DetailGet"))
  , hostMapGetH :: mode :- "infrastructure" :> "host-map" :> QPT "fill" :> QPT "group" :> QPT "provider" :> QPT "region" :> QPT "os" :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "deferred" :> Get '[HTML] (RespHeaders (Opaque "Infrastructure.HostMapGet"))
  , -- Legacy compatibility route. Navigation and generated links use /infrastructure/containers.
    containersGetH :: mode :- "containers" :> QPT "runtime" :> QPT "namespace" :> QPT "node" :> QPT "image" :> QPT "cluster" :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "deferred" :> Get '[HTML] (RespHeaders (Opaque "Containers.ContainersGet"))
  , containerDetailGetH :: mode :- "containers" :> "detail" :> QPT "container" :> QPT "pod" :> QueryParam "scope" Scope :> QPT "cluster" :> QPT "namespace" :> QPT "node" :> QPT "from" :> QPT "to" :> QPT "since" :> Get '[HTML] (RespHeaders (Html ()))
  }
  deriving stock (Generic)


type MonitorsRoutes = NamedRoutes MonitorsRoutes'


type MonitorsRoutes' :: Type -> Type
data MonitorsRoutes' mode = MonitorsRoutes'
  { listGet :: mode :- QueryParam "filter" Text :> QueryParam "since" Text :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" ((Opaque1 "Table.Table") (Opaque "Testing.UnifiedMonitorItem"))))
  , overviewGet :: mode :- Capture "monitor_id" Text :> "overview" :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Html ())))
  , alertUpsertPost :: mode :- "alerts" :> ReqBody '[FormUrlEncoded] (Opaque "Testing.AlertUpsertForm") :> Post '[HTML] (RespHeaders (Opaque "Testing.Alert"))
  , alertSingleGet :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> Get '[HTML] (RespHeaders (Opaque "Testing.Alert"))
  , alertSingleToggleActive :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "toggle_active" :> Post '[HTML] (RespHeaders (Opaque "Testing.Alert"))
  , alertMutePost :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "mute" :> QueryParam "duration" Int :> Unrefusable 'POST (Opaque "Testing.MonitorActed")
  , alertUnmutePost :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "unmute" :> Unrefusable 'POST (Opaque "Testing.MonitorActed")
  , alertResolvePost :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "resolve" :> Unrefusable 'POST (Opaque "Testing.MonitorActed")
  , alertDeleteRoute :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> Unrefusable 'DELETE (Opaque "Testing.MonitorActed")
  , teamAlertsGetH :: mode :- "alerts" :> "team" :> Capture "team_id" ProjectMembers.TeamId :> Get '[HTML] (RespHeaders ((Opaque1 "Table.TableRows") (Opaque "Testing.UnifiedMonitorItem")))
  , alertTeamDeleteH :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "teams" :> Capture "team_id" ProjectMembers.TeamId :> Delete '[HTML] (RespHeaders (Opaque "Testing.Alert"))
  , cronGet :: mode :- "cron" :> Get '[HTML] (RespHeaders (Opaque "Testing.CronMonitors"))
  , cronPost :: mode :- "cron" :> ReqBody '[FormUrlEncoded] (Opaque "Testing.CronForm") :> Action 'POST (Opaque "Testing.CronRejection") (Opaque "Testing.CronList")
  , cronDelete :: mode :- "cron" :> Capture "monitor_id" UUID.UUID :> "delete" :> Post '[HTML] (RespHeaders (Opaque "Testing.CronList"))
  , uptimeGet :: mode :- "uptime" :> Get '[HTML] (RespHeaders (Opaque "Testing.UptimeChecks"))
  , uptimePost :: mode :- "uptime" :> ReqBody '[FormUrlEncoded] (Opaque "Testing.UptimeForm") :> Action 'POST (Opaque "Testing.UptimeRejection") (Opaque "Testing.UptimeList")
  , uptimeToggle :: mode :- "uptime" :> Capture "check_id" PromCfg.PrometheusScrapeConfigId :> "toggle" :> Post '[HTML] (RespHeaders (Opaque "Testing.UptimeList"))
  , uptimeDelete :: mode :- "uptime" :> Capture "check_id" PromCfg.PrometheusScrapeConfigId :> "delete" :> Post '[HTML] (RespHeaders (Opaque "Testing.UptimeList"))
  , alertBulkAction :: mode :- "alerts" :> "bulk_action" :> Capture "action" Monitors.MonitorBulkAction :> ReqBody '[FormUrlEncoded] (Opaque "ManageMembers.TBulkActionForm") :> Post '[HTML] (RespHeaders (Opaque1 "PageCtx" ((Opaque1 "Table.Table") (Opaque "Testing.UnifiedMonitorItem"))))
  }
  deriving stock (Generic)


type ProjectsRoutes = NamedRoutes ProjectsRoutes'


type ProjectsRoutes' :: Type -> Type
data ProjectsRoutes' mode = ProjectsRoutes'
  { listGet :: mode :- Get '[HTML] (RespHeaders (Opaque "ListProjects.ListProjectsGet"))
  , onboardingProject :: mode :- "p" :> "new" :> LocationRedirect (Opaque1 "PageCtx" (Html ())) -- p represents project
  , createPost :: mode :- "p" :> "update" :> Capture "projectId" Projects.ProjectId :> ReqBody '[FormUrlEncoded] (Opaque "CreateProject.CreateProjectForm") :> Post '[HTML] (RespHeaders (Opaque "CreateProject.CreateProject"))
  , settingsGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "settings" :> Get '[HTML] (RespHeaders (Opaque "CreateProject.CreateProject"))
  , integrationGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "settings" :> "integrations" :> Get '[HTML] (RespHeaders (Opaque1 "PageCtx" (Opaque "Integrations.IntegrationsConfig")))
  , -- Project management
    deleteGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "delete" :> Action 'DELETE (Opaque "CreateProject.DemoProjectProtected") (Opaque "CreateProject.ProjectDeleted")
  , -- Member management
    membersManageGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> Get '[HTML] (RespHeaders (Opaque "ManageMembers.ManageMembers"))
  , membersManagePost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> QPT "onboarding" :> ReqBody '[FormUrlEncoded] (Opaque "ManageMembers.ManageMembersForm") :> Post '[HTML] (RespHeaders (Opaque "ManageMembers.ManageMembers"))
  , membersDeleteH :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> Capture "memberId" ProjectMembers.ProjectMemberId :> Action 'DELETE (Opaque "ManageMembers.MemberRejection") (Opaque "ManageMembers.MemberRemoved")
  , teamsManageGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_teams" :> QPT "what" :> Get '[HTML] (RespHeaders (Opaque "ManageMembers.ManageTeams"))
  , teamsManagePost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_teams" :> ReqBody '[JSON] (Opaque "ManageMembers.TeamForm") :> QPT "teamView" :> Action 'POST (Opaque "ManageMembers.TeamRejection") (Opaque "ManageMembers.TeamSaved")
  , teamGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_teams" :> Capture "teamHandle" Text :> QPT "layout" :> Get '[HTML] (RespHeaders (Opaque "ManageMembers.ManageTeams"))
  , teamBulkAction :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_teams" :> "bulk_action" :> Capture "action" Text :> ReqBody '[FormUrlEncoded] (Opaque "ManageMembers.TBulkActionForm") :> QPT "teamView" :> Action 'POST (Opaque "ManageMembers.TeamsDeleteRejection") (Opaque "ManageMembers.TeamsDeleted")
  , manageSubscriptionGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_subscription" :> Action 'GET (Opaque "CreateProject.BillingRejection") (Opaque "CreateProject.BillingRedirect")
  , stripeCheckout :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "stripe_checkout" :> ReqBody '[FormUrlEncoded] (Opaque "CreateProject.StripeCheckoutForm") :> Action 'POST (Opaque "CreateProject.BillingRejection") (Opaque "CreateProject.BillingRedirect")
  , -- Notifications
    notificationsUpdateChannelPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "notifications-channels" :> ReqBody '[FormUrlEncoded] (Opaque "Integrations.NotifListForm") :> Action 'POST (Opaque "Integrations.ChannelsRejection") (Opaque "Integrations.IntegrationsSaved")
  , pagerdutyConnect :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "settings" :> "integrations" :> "pagerduty" :> ReqBody '[FormUrlEncoded] (Opaque "Integrations.PagerdutyConnectForm") :> Action 'POST (Opaque "Integrations.PagerdutyKeyTooShort") (Opaque "Integrations.IntegrationsSaved")
  , pagerdutyDisconnect :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "settings" :> "integrations" :> "pagerduty" :> "disconnect" :> Unrefusable 'POST (Opaque "Integrations.IntegrationsSaved")
  , slackDisconnect :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "settings" :> "integrations" :> "slack" :> Action 'DELETE (Opaque "Integrations.SlackNotInstalled") (Opaque "Integrations.IntegrationsSaved")
  , notificationsTestPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "settings" :> "integrations" :> "test" :> ReqBody '[FormUrlEncoded] (Opaque "Settings.TestForm") :> Unrefusable 'POST (Opaque "Settings.TestOutcome")
  , notificationsTestHistoryGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "settings" :> "integrations" :> "history" :> Get '[HTML] (RespHeaders (Opaque "Settings.NotificationTestHistoryGet"))
  , -- Onboarding routes
    onboading :: mode :- "p" :> ProjectId :> "onboarding" :> LenientQP "step" Projects.OnboardingStep :> Get '[HTML] (RespHeaders (Opaque "Onboarding.OnboardingGet"))
  , onboardingInfoPost :: mode :- "p" :> ProjectId :> "onboarding" :> "info" :> ReqBody '[FormUrlEncoded] (Opaque "Onboarding.OnboardingInfoForm") :> Unrefusable 'POST (Opaque "Onboarding.OnboardingStepPost")
  , onboardingConfPost :: mode :- "p" :> ProjectId :> "onboarding" :> "survey" :> ReqBody '[FormUrlEncoded] (Opaque "Onboarding.OnboardingConfForm") :> Unrefusable 'POST (Opaque "Onboarding.OnboardingStepPost")
  , onboardingPhoneEmailsPost :: mode :- "p" :> ProjectId :> "onboarding" :> "phone-emails" :> ReqBody '[JSON] (Opaque "Onboarding.NotifChannelForm") :> Post '[HTML] (RespHeaders (Opaque "Onboarding.OnboardingPhoneEmailsPost"))
  , onboardingIntegrationCheck :: mode :- "p" :> ProjectId :> "onboarding" :> "integration-check" :> QPT "language" :> Action 'GET (Opaque "Onboarding.NoEventsYet") (Opaque "Onboarding.IntegrationCheck")
  , onboardingDismissChecklist :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "dismiss-checklist" :> Unrefusable 'POST (Opaque "Onboarding.ChecklistDismissed")
  , onboardingSkipped :: mode :- "p" :> ProjectId :> "onboarding" :> "skip" :> LenientQP "step" Projects.OnboardingStep :> Unrefusable 'POST (Opaque "Onboarding.OnboardingStepPost")
  , -- Pricing routes
    onboardingPricingUpdate :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "pricing" :> ReqBody '[FormUrlEncoded] (Opaque "CreateProject.PricingUpdateForm") :> Action 'POST (Opaque "CreateProject.SubscriptionLookupFailed") (Opaque "CreateProject.PricingUpdated")
  }
  deriving stock (Generic)


-- | Served at the top level, outside the cookie-authenticated records: signed for chat unfurls.
type WidgetPngGet = "p" :> ProjectId :> "widget.png" :> QPT "widgetJSON" :> QPT "widgetZ" :> QPT "since" :> QPT "from" :> QPT "to" :> QueryParam "width" Int :> QueryParam "height" Int :> QPT "sig" :> AllQueryParams :> Get '[PNG] (Headers '[Header "Cache-Control" Text] LBS.ByteString)


-- | Recorded proof media, signed per file so GitHub's image proxy can fetch it without a session.
type ProofMediaGet = "p" :> ProjectId :> "reviews" :> Capture "review_id" (UUIDId "pr_review") :> "proofs" :> Capture "file" ImpactReviews.ProofFile :> QPT "sig" :> RawM


type ProofRatingGet = "p" :> ProjectId :> "reviews" :> Capture "review_id" (UUIDId "pr_review") :> "rating" :> Capture "rating" ImpactReviews.Rating :> QPT "sig" :> Get '[HTML] (Html ())
type ProofRatingPost = "p" :> ProjectId :> "reviews" :> Capture "review_id" (UUIDId "pr_review") :> "rating" :> Capture "rating" ImpactReviews.Rating :> QPT "sig" :> Post '[HTML] (Html ())


proofRatingPath :: Projects.ProjectId -> UUIDId "pr_review" -> ImpactReviews.Rating -> Text -> Text
proofRatingPath pid rid rating sig = safeLink' (("/" <>) . toUrlPiece) (Proxy @ProofRatingGet) (Proxy @ProofRatingGet) pid rid rating (Just sig)


-- | A verified fix's video or diagram, signed per asset so a pull request opened by the GitHub App can show it.
type FixMediaGet = "p" :> ProjectId :> "issue-fixes" :> Capture "id" (UUIDId "issue_fix") :> "shared" :> Capture "asset" Artifacts.Asset :> QPT "sig" :> RawM


fixMediaPath :: Projects.ProjectId -> UUIDId "issue_fix" -> Artifacts.Asset -> Text -> Text
fixMediaPath pid fid asset sig = safeLink' (("/" <>) . toUrlPiece) (Proxy @FixMediaGet) (Proxy @FixMediaGet) pid fid asset (Just sig)


proofMediaPath :: Projects.ProjectId -> UUIDId "pr_review" -> ImpactReviews.ProofFile -> Text -> Text
proofMediaPath pid rid file sig = safeLink' (("/" <>) . toUrlPiece) (Proxy @ProofMediaGet) (Proxy @ProofMediaGet) pid rid file (Just sig)


-- $setup
-- >>> import Data.Default (def)
-- >>> import Data.UUID qualified as UUID
-- >>> import Models.Apis.LogQueries qualified as LogQueries
-- >>> import Models.Projects.Projects qualified as Projects
-- >>> import Pkg.DeriveUtils (UUIDId (..))
-- >>> let pid = UUIDId UUID.nil :: Projects.ProjectId


-- | Links use the served route's captures, query encoding, and flags.
--
-- >>> urls.projectWikiGet pid ["runbooks", "retry & recover"] Nothing Nothing Nothing True
-- "/p/00000000-0000-0000-0000-000000000000/wiki/runbooks/retry%20%26%20recover?edit"
-- >>> urls.dashboardRedirectGet pid [("var-service", Just "api & worker"), ("deferred", Nothing)]
-- "/p/00000000-0000-0000-0000-000000000000?var-service=api%20%26%20worker&deferred"
urls :: CookieProtectedRoutes (AsLink Text)
urls = allFieldLinks' (("/" <>) . toUrlPiece)


-- | Explorer links relative to a project URL, for emails and chat messages that carry one.
logExplorerLinks :: LogExplorerRoutes' (AsLink Text)
logExplorerLinks = allFieldLinks' (("/" <>) . toUrlPiece)


-- | A log-explorer data URL. 'recent' polls for rows newer than the cursor.
--
-- >>> explorerUrl pid LogQueries.Data def{LogQueries.query = Just "status == 500", LogQueries.environment = Just "staging", LogQueries.serviceScope = Just "api"}
-- "/p/00000000-0000-0000-0000-000000000000/log_explorer/data?json=true&environment=staging&service_scope=api&query=status%20%3D%3D%20500"
explorerUrl :: Projects.ProjectId -> LogQueries.LogEndpoint -> LogQueries.ExplorerUrl -> Text
explorerUrl pid endpoint u = case endpoint of
  LogQueries.Data -> (urls.logExplorer pid).logExplorerDataGet browser LogQueries.LogDataQuery{query = u.query, cols = u.cols, cursor = iso8601ParseM . toString =<< cursor, direction = Nothing, since, from = u.from, to, source = u.source, targetSpans = Nothing, sort = Nothing}
  LogQueries.Sessions -> (urls.logExplorer pid).logExplorerSessionsGet browser u.query since u.from to Nothing Nothing cursor
  LogQueries.Patterns -> (urls.logExplorer pid).logExplorerPatternsGet browser u.query since u.from to u.source Nothing Nothing
  where
    browser = LogQueries.BrowserParams{json = Just True, layout = u.layout, environment = u.environment, serviceScope = u.serviceScope}
    cursor = if u.recent then Nothing else u.cursor
    since = if u.recent then Nothing else u.since
    to = if u.recent then LogQueries.nudgeCursorBy LogQueries.cursorEpsilon =<< u.cursor else u.to


widgetPngPath :: Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Text
widgetPngPath pid widgetZ since from to sig = safeLink' (("/" <>) . toUrlPiece) (Proxy @WidgetPngGet) (Proxy @WidgetPngGet) pid Nothing (Just widgetZ) since from to Nothing Nothing (Just sig) []
