module Main (main) where

import Relude

import CLI.Commands hiding (value)
import CLI.Config (CLIConfig (..), resolveConfig)
import CLI.Core (OutputMode (..), apiDelete, apiGetJson, apiPostJson, detectOutputMode, renderAPIError)
import CLI.Resource qualified as Resource
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Data.Effectful.Wreq (HTTP, runHTTPWreq)
import Data.Text qualified as T
import Models.Telemetry.Schema qualified as Schema
import Data.Text.IO qualified
import Data.Version (showVersion)
import Effectful
import Effectful.Environment (Environment, runEnvironment)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Options.Applicative
import Paths_monoscope qualified as Paths
import System.Environment (setEnv)


data GlobalOpts = GlobalOpts
  { jsonFlag :: Bool
  , agentFlag :: Bool
  , debugFlag :: Bool
  , outputFlag :: Maybe Text
  , projectFlag :: Maybe Text
  }


data Command
  = AuthCmd AuthCommand
  | EventsCmd (Maybe Text) EventsCommand
  | MetricsCmd MetricsCommand
  | ServicesCmd ServicesCommand
  | ConfigCmd ConfigCommand
  | MonitorsCmd MonitorsCommand
  | DashboardsCmd DashboardsCommand
  | ApiKeysCmd ApiKeysCommand
  | ShareLinkCmd ShareLinkCommand
  | MeCmd
  | ProjectCmd ProjectCommand
  | IssuesCmd IssuesCommand
  | EndpointsCmd EndpointsCommand
  | LogPatternsCmd LogPatternsCommand
  | TeamsCmd TeamsCommand
  | MembersCmd MembersCommand
  | SchemaCmd SchemaOpts
  | FacetsCmd FacetsOpts
  | CompletionCmd Text
  | VersionCmd
  | TelemetryGenCmd TelemetryGenOpts
  | SendEventCmd SendEventOpts


data ProjectCommand
  = ProjGet
  | ProjPatch FilePath
  deriving stock (Show)


data IssuesCommand
  = IssueList (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int) (Maybe Int)
  | IssueGet Text
  | IssueAck Text
  | IssueUnack Text
  | IssueArchive Text
  | IssueUnarchive Text
  | IssueBulk Text [Text]
  deriving stock (Show)


data EndpointsCommand
  = EndList (Maybe Text) (Maybe Bool) (Maybe Int) (Maybe Int)
  | EndGet Text
  deriving stock (Show)


data LogPatternsCommand
  = LPList (Maybe Int) (Maybe Int)
  | LPGet Int64
  | LPAck Int64
  | LPBulk Text [Int64]
  deriving stock (Show)


data TeamsCommand
  = TeamList
  | TeamGet Text
  | TeamCreate FilePath
  | TeamUpdate Text FilePath
  | TeamPatch Text FilePath
  | TeamDelete Text
  | TeamBulk Text [Text]
  deriving stock (Show)


data MembersCommand
  = MemberList
  | MemberGet Text
  | MemberAdd (Maybe Text) (Maybe Text) (Maybe Text) -- email, userId, permission
  | MemberPatch Text Text -- userId, permission
  | MemberRemove Text
  deriving stock (Show)


data EventsCommand
  = EvSearch EventsSearchOpts
  | EvGet EventsGetOpts
  | EvTail EventsTailOpts
  | EvContext EventsContextOpts
  deriving stock (Show)


data MetricsCommand
  = MQuery MetricsQueryOpts
  | MChart MetricsChartOpts
  deriving stock (Show)


newtype ServicesCommand = SList ServicesListOpts
  deriving stock (Show)


data ConfigCommand = CInit | CSet ConfigSetOpts | CGet ConfigGetOpts
  deriving stock (Show)


-- | C3: filter the schema dump so agents don't burn context on irrelevant
-- fields. The full schema is ~600 lines; @--search service@ trims to ~20.
data SchemaOpts = SchemaOpts
  { search :: Maybe Text
  -- ^ Substring filter applied to field names (case-insensitive).
  , schemaLimit :: Maybe Int
  -- ^ Cap on returned fields (post-filter).
  }
  deriving stock (Show)


-- | Facets — the precomputed top-N values per field. Hugely valuable for an
-- agent: "what are the popular service names? what status codes have I seen?
-- what severity levels do my logs even use?". The server already maintains
-- these in @apis.facet_summaries@ via a background job.
data FacetsOpts = FacetsOpts
  { facetField :: Maybe Text
  -- ^ Optional field path (e.g. @resource.service.name@). When set, only that
  -- field's values are returned. Without it, every faceted field is dumped.
  , facetSince :: Maybe Text
  , facetTopN :: Maybe Int
  -- ^ Cap on values per field — handy when a field has thousands of distinct
  -- values and you only care about the head.
  }
  deriving stock (Show)


-- | Plan A resource commands. 'id' is a UUID text.
data MonitorsCommand
  = MonList
  | MonGet Text
  | MonCreate FilePath
  | MonUpdate Text FilePath
  | MonPatch Text FilePath
  | MonApply FilePath
  | MonDelete Text
  | MonMute Text (Maybe Int)
  | MonUnmute Text
  | MonResolve Text
  | MonToggle Text
  | MonBulk Text [Text] (Maybe Int)
  deriving stock (Show)


data DashboardsCommand
  = DashList
  | DashGet Text
  | DashCreate FilePath
  | DashUpdate Text FilePath
  | DashPatch Text FilePath
  | DashDelete Text
  | DashStar Text
  | DashUnstar Text
  | DashDuplicate Text
  | DashYaml Text
  | DashApply FilePath
  | DashBulk Text [Text]
  | DashWidgetUpsert Text FilePath
  | DashWidgetDelete Text Text
  | DashWidgetsReorder Text (Maybe Text) FilePath
  deriving stock (Show)


data ApiKeysCommand
  = KeyList
  | KeyGet Text
  | KeyCreate Text -- title
  | KeyActivate Text
  | KeyDeactivate Text
  | KeyDelete Text
  deriving stock (Show)


data ShareLinkCommand
  = ShareLinkCreate Text Text (Maybe Text) -- eventId, createdAt, eventType
  deriving stock (Show)


globalOpts :: Parser GlobalOpts
globalOpts =
  GlobalOpts
    <$> switch (long "json" <> help "Force JSON output")
    <*> switch (long "agent" <> help "Force agent mode")
    <*> switch (long "debug" <> help "Print outgoing requests to stderr (also: MONOSCOPE_DEBUG=1)")
    <*> optional (strOption (long "output" <> short 'o' <> metavar "FORMAT" <> help "Output format: json|yaml|table"))
    <*> optional (strOption (long "project" <> short 'p' <> metavar "PID" <> help "Project UUID override"))


versionOpt :: Parser (a -> a)
versionOpt =
  infoOption
    ("monoscope " <> showVersion Paths.version)
    (long "version" <> help "Print CLI version and exit")


commandParser :: Parser (GlobalOpts, Command)
commandParser =
  (,)
    <$> globalOpts
    <*> subparser
      ( mconcat
          [ command "auth" (info (AuthCmd <$> authParser <**> helper) (progDesc "Manage authentication"))
          , command "events" (info (EventsCmd Nothing <$> eventsParser <**> helper) (progDesc "Search and inspect events"))
          , command "logs" (info (EventsCmd (Just "log") <$> eventsParser <**> helper) (progDesc "Search logs"))
          , command "traces" (info (EventsCmd (Just "trace") <$> eventsParser <**> helper) (progDesc "Search traces"))
          , command "metrics" (info (MetricsCmd <$> metricsParser <**> helper) (progDesc "Query and chart metrics"))
          , command "services" (info (ServicesCmd <$> servicesParser <**> helper) (progDesc "List services"))
          , command "config" (info (ConfigCmd <$> configParser <**> helper) (progDesc "Manage configuration"))
          , command "monitors" (info (MonitorsCmd <$> monitorsParser <**> helper) (progDesc "Manage alert monitors"))
          , command "dashboards" (info (DashboardsCmd <$> dashboardsParser <**> helper) (progDesc "Manage dashboards"))
          , command "api-keys" (info (ApiKeysCmd <$> apiKeysParser <**> helper) (progDesc "Manage API keys"))
          , command "share-link" (info (ShareLinkCmd <$> shareLinkParser <**> helper) (progDesc "Create time-limited share links"))
          , command "me" (info (pure MeCmd) (progDesc "Show current project identity"))
          , command "project" (info (ProjectCmd <$> projectParser <**> helper) (progDesc "Show or patch the current project"))
          , command "issues" (info (IssuesCmd <$> issuesParser <**> helper) (progDesc "List and triage issues"))
          , command "endpoints" (info (EndpointsCmd <$> endpointsParser <**> helper) (progDesc "Browse the API endpoint catalog"))
          , command "log-patterns" (info (LogPatternsCmd <$> logPatternsParser <**> helper) (progDesc "Triage log patterns"))
          , command "teams" (info (TeamsCmd <$> teamsParser <**> helper) (progDesc "Manage teams"))
          , command "members" (info (MembersCmd <$> membersParser <**> helper) (progDesc "Manage project members"))
          , command "schema" (info (SchemaCmd <$> schemaParser <**> helper) (progDesc "Fetch telemetry schema"))
          , command "facets" (info (FacetsCmd <$> facetsParser <**> helper) (progDesc "Discover popular field values (top-N per faceted field)" <> footer facetsExamples))
          , command "completion" (info (CompletionCmd <$> strArgument (metavar "SHELL" <> help "bash|zsh|fish") <**> helper) (progDesc "Emit shell completion script"))
          , command "version" (info (pure VersionCmd) (progDesc "Show CLI version"))
          , command "telemetrygen" (info (TelemetryGenCmd <$> telemetryGenParser <**> helper) (progDesc "Generate synthetic telemetry (traces/logs/metrics)" <> footer telemetryGenExamples))
          , command "send-event" (info (SendEventCmd <$> sendEventParser <**> helper) (progDesc "Send a real event (log/trace/error) from a script or CI" <> footer sendEventExamples))
          ]
      )


authParser :: Parser AuthCommand
authParser =
  subparser $
    mconcat
      [ command "login" (info (AuthLogin <$> optional (strOption (long "token" <> metavar "TOKEN" <> help "API token"))) (progDesc "Authenticate"))
      , command "status" (info (pure AuthStatus) (progDesc "Show auth state"))
      , command "logout" (info (pure AuthLogout) (progDesc "Remove credentials"))
      ]


eventsParser :: Parser EventsCommand
eventsParser =
  subparser $
    mconcat
      [ command "search" (info (EvSearch <$> eventsSearchParser <**> helper) (progDesc "Search events" <> footer eventsSearchExamples))
      , command "get" (info (EvGet <$> eventsGetParser <**> helper) (progDesc "Get event by ID"))
      , command "tail" (info (EvTail <$> eventsTailParser <**> helper) (progDesc "Stream events"))
      , command "context" (info (EvContext <$> eventsContextParser <**> helper) (progDesc "Context around timestamp"))
      ]


eventsSearchParser :: Parser EventsSearchOpts
eventsSearchParser =
  EventsSearchOpts
    <$> strArgument (metavar "QUERY" <> value "" <> help "KQL search query (use == for equality, see `monoscope schema`)")
    <*> optional (strOption (long "since" <> metavar "DURATION" <> help "Relative time (e.g. 1h, 30m, 7d)"))
    <*> optional (strOption (long "from" <> metavar "TIMESTAMP" <> help "Start time (ISO 8601)"))
    <*> optional (strOption (long "to" <> metavar "TIMESTAMP" <> help "End time (ISO 8601)"))
    <*> optional (strOption (long "kind" <> metavar "KIND" <> help "log|trace|span"))
    <*> optional (strOption (long "service" <> metavar "SERVICE" <> help "Shorthand for resource.service.name=='SERVICE'"))
    <*> optional (strOption (long "level" <> metavar "LEVEL" <> help "Shorthand for severity.text=='LEVEL'"))
    <*> optional (option auto (long "limit" <> short 'n' <> metavar "N" <> help "Max results"))
    <*> optional (strOption (long "fields" <> metavar "FIELDS" <> help "Comma-separated fields to keep"))
    <*> optional (strOption (long "cursor" <> metavar "CURSOR" <> help "Pagination cursor from a prior response"))
    <*> switch (long "first" <> help "Return only the first matching event")
    <*> switch (long "id-only" <> help "Print just the first event id (implies --first)")


-- | C5: Concrete KQL examples in @--help@ — agents copy from here, so keep
-- them up-to-date and runnable.
eventsSearchExamples :: String
eventsSearchExamples =
  intercalate "\n"
    [ "Examples:"
    , "  monoscope events search 'severity.text==\"error\"' --since 1h"
    , "  monoscope logs search --service checkout-api --level error --limit 50"
    , "  monoscope events search 'attributes.http.response.status_code >= 500' --since 1h"
    , "  monoscope traces search --since 30m --first --id-only   # one trace id"
    , "  monoscope events search '' --since 1h --cursor <CURSOR>  # next page"
    , ""
    , "KQL operators: == != > >= < <=  |  combine with: and or  |  parens for grouping"
    , "Run `monoscope schema --search service` to discover field names."
    ]


eventsGetParser :: Parser EventsGetOpts
eventsGetParser =
  EventsGetOpts
    <$> strArgument (metavar "ID" <> help "Event or trace ID")
    <*> switch (long "tree" <> help "Show trace tree view")
    <*> optional (strOption (long "at" <> metavar "TIMESTAMP" <> help "ISO-8601 timestamp for a fast point-in-time lookup (avoids 90d range scan)"))


eventsTailParser :: Parser EventsTailOpts
eventsTailParser =
  EventsTailOpts
    <$> optional (strOption (long "kind" <> metavar "KIND"))
    <*> optional (strOption (long "service" <> metavar "SERVICE"))
    <*> optional (strOption (long "level" <> metavar "LEVEL"))
    <*> optional (strOption (long "grep" <> metavar "PATTERN" <> help "Regex filter on message"))


eventsContextParser :: Parser EventsContextOpts
eventsContextParser =
  EventsContextOpts
    <$> strOption (long "at" <> metavar "TIMESTAMP" <> help "Center timestamp")
    <*> optional (strOption (long "service" <> metavar "SERVICE"))
    <*> optional (strOption (long "kind" <> metavar "KIND"))
    <*> optional (strOption (long "window" <> metavar "DURATION" <> help "Time window (default: 5m)"))
    <*> switch (long "summary" <> help "Add per-trace summary to JSON output (trace_id, services, span_count, error_count)")


metricsParser :: Parser MetricsCommand
metricsParser =
  subparser $
    mconcat
      [ command "query" (info (MQuery <$> metricsQueryParser <**> helper) (progDesc "Run metrics query"))
      , command "chart" (info (MChart <$> metricsChartParser <**> helper) (progDesc "Render chart"))
      ]


metricsQueryParser :: Parser MetricsQueryOpts
metricsQueryParser =
  MetricsQueryOpts
    <$> strArgument (metavar "EXPRESSION" <> help "Metrics expression")
    <*> optional (strOption (long "since" <> metavar "DURATION"))
    <*> optional (strOption (long "from" <> metavar "TIMESTAMP"))
    <*> optional (strOption (long "to" <> metavar "TIMESTAMP"))
    <*> optional (strOption (long "assert" <> metavar "CONDITION" <> help "Assert condition (e.g. '< 0.01')"))


metricsChartParser :: Parser MetricsChartOpts
metricsChartParser =
  MetricsChartOpts
    <$> strArgument (metavar "EXPRESSION" <> help "Metrics expression")
    <*> optional (strOption (long "since" <> metavar "DURATION"))
    <*> optional (strOption (long "from" <> metavar "TIMESTAMP"))
    <*> optional (strOption (long "to" <> metavar "TIMESTAMP"))
    <*> optional (strOption (long "watch" <> short 'w' <> metavar "INTERVAL" <> help "Refresh interval"))


servicesParser :: Parser ServicesCommand
servicesParser =
  subparser $
    command "list" (info (SList <$> servicesListParser <**> helper) (progDesc "List services"))


servicesListParser :: Parser ServicesListOpts
servicesListParser =
  ServicesListOpts
    <$> optional (strOption (long "since" <> metavar "DURATION"))


configParser :: Parser ConfigCommand
configParser =
  subparser $
    mconcat
      [ command "init" (info (pure CInit) (progDesc "Initialize configuration"))
      , command "set" (info (CSet <$> configSetParser <**> helper) (progDesc "Set config value"))
      , command "get" (info (CGet <$> configGetParser <**> helper) (progDesc "Get config value"))
      ]


configSetParser :: Parser ConfigSetOpts
configSetParser =
  ConfigSetOpts
    <$> strArgument (metavar "KEY" <> help "Config key")
    <*> strArgument (metavar "VALUE" <> help "Config value")


configGetParser :: Parser ConfigGetOpts
configGetParser = ConfigGetOpts <$> optional (strArgument (metavar "KEY" <> help "Config key"))


schemaParser :: Parser SchemaOpts
schemaParser =
  SchemaOpts
    <$> optional (strOption (long "search" <> metavar "TEXT" <> help "Filter fields whose name contains TEXT (case-insensitive)"))
    <*> optional (option auto (long "limit" <> metavar "N" <> help "Limit number of fields returned"))


facetsParser :: Parser FacetsOpts
facetsParser =
  FacetsOpts
    <$> optional (strArgument (metavar "FIELD" <> help "Field path to drill into (e.g. resource.service.name). Omit to dump all fields."))
    <*> optional (strOption (long "since" <> metavar "DURATION" <> help "Lookback window (default: 24h)"))
    <*> optional (option auto (long "top" <> metavar "N" <> help "Limit values per field"))


-- | Examples shown in `monoscope facets --help`. Mirrors the discovery
-- workflow most agents follow: dump-everything → drill-into-one-field →
-- pipe-into-search.
facetsExamples :: String
facetsExamples =
  intercalate "\n"
    [ "Examples:"
    , "  monoscope facets                         # all faceted fields"
    , "  monoscope facets resource.service.name   # values for one field"
    , "  monoscope facets severity.text --top 5"
    , "  monoscope facets resource.service.name | jq -r '.[\"resource.service.name\"][].value'"
    , "  monoscope facets --since 7d              # widen the lookback"
    , ""
    , "Each value carries a `count`. Pop the top result into a search:"
    , "  SVC=$(monoscope facets resource.service.name --top 1 | jq -r '.[\"resource.service.name\"][0].value')"
    , "  monoscope events search '' --service \"$SVC\" --since 1h"
    ]


-- Plan A resource parsers

idArg :: Parser Text
idArg = strArgument (metavar "ID" <> help "Resource ID (UUID)")


fileArg :: Parser FilePath
fileArg = strArgument (metavar "FILE" <> help "Path to YAML or JSON file")


idsOpt :: Parser [Text]
idsOpt = T.splitOn "," <$> strOption (long "ids" <> metavar "ID1,ID2,..." <> help "Comma-separated UUIDs")


monitorsParser :: Parser MonitorsCommand
monitorsParser =
  subparser $
    mconcat
      [ command "list" (info (pure MonList) (progDesc "List monitors"))
      , command "get" (info (MonGet <$> idArg <**> helper) (progDesc "Get a single monitor"))
      , command "create" (info (MonCreate <$> fileArg <**> helper) (progDesc "Create monitor from YAML/JSON file"))
      , command "update" (info (MonUpdate <$> idArg <*> fileArg <**> helper) (progDesc "Replace (PUT) monitor from file"))
      , command "patch" (info (MonPatch <$> idArg <*> fileArg <**> helper) (progDesc "Patch monitor fields from file"))
      , command "apply" (info (MonApply <$> strArgument (metavar "PATH" <> help "YAML/JSON file or directory") <**> helper) (progDesc "Upsert monitor(s) from file or directory"))
      , command "delete" (info (MonDelete <$> idArg <**> helper) (progDesc "Delete a monitor"))
      , command "mute" (info (MonMute <$> idArg <*> optional (option auto (long "for" <> metavar "MINUTES")) <**> helper) (progDesc "Mute a monitor"))
      , command "unmute" (info (MonUnmute <$> idArg <**> helper) (progDesc "Unmute a monitor"))
      , command "resolve" (info (MonResolve <$> idArg <**> helper) (progDesc "Resolve a monitor"))
      , command "toggle-active" (info (MonToggle <$> idArg <**> helper) (progDesc "Toggle active state"))
      , command
          "bulk"
          ( info
              ( MonBulk
                  <$> strArgument (metavar "ACTION" <> help "delete|activate|deactivate|mute|unmute|resolve")
                  <*> idsOpt
                  <*> optional (option auto (long "duration" <> metavar "MINUTES" <> help "For mute action"))
                  <**> helper
              )
              (progDesc "Run bulk action over multiple monitors")
          )
      ]


dashboardsParser :: Parser DashboardsCommand
dashboardsParser =
  subparser $
    mconcat
      [ command "list" (info (pure DashList) (progDesc "List dashboards"))
      , command "get" (info (DashGet <$> idArg <**> helper) (progDesc "Get a single dashboard"))
      , command "create" (info (DashCreate <$> fileArg <**> helper) (progDesc "Create dashboard from YAML/JSON file"))
      , command "update" (info (DashUpdate <$> idArg <*> fileArg <**> helper) (progDesc "Replace (PUT) dashboard from file"))
      , command "patch" (info (DashPatch <$> idArg <*> fileArg <**> helper) (progDesc "Patch dashboard fields from file"))
      , command "delete" (info (DashDelete <$> idArg <**> helper) (progDesc "Delete a dashboard"))
      , command "star" (info (DashStar <$> idArg <**> helper) (progDesc "Star a dashboard"))
      , command "unstar" (info (DashUnstar <$> idArg <**> helper) (progDesc "Unstar a dashboard"))
      , command "duplicate" (info (DashDuplicate <$> idArg <**> helper) (progDesc "Duplicate dashboard"))
      , command "yaml" (info (DashYaml <$> idArg <**> helper) (progDesc "Dump dashboard as YAML"))
      , command "apply" (info (DashApply <$> strArgument (metavar "PATH" <> help "YAML/JSON file or directory") <**> helper) (progDesc "Upsert dashboard(s) from file or directory"))
      , command
          "bulk"
          ( info
              ( DashBulk
                  <$> strArgument (metavar "ACTION" <> help "delete")
                  <*> idsOpt
                  <**> helper
              )
              (progDesc "Run bulk action over multiple dashboards")
          )
      , command "widget" (info (widgetParser <**> helper) (progDesc "Widget-level operations"))
      ]


widgetParser :: Parser DashboardsCommand
widgetParser =
  subparser $
    mconcat
      [ command
          "upsert"
          ( info
              ( DashWidgetUpsert
                  <$> strArgument (metavar "DASHBOARD_ID")
                  <*> fileArg
                  <**> helper
              )
              (progDesc "Insert or update a widget on a dashboard")
          )
      , command
          "delete"
          ( info
              ( DashWidgetDelete
                  <$> strArgument (metavar "DASHBOARD_ID")
                  <*> strArgument (metavar "WIDGET_ID")
                  <**> helper
              )
              (progDesc "Delete a widget from a dashboard")
          )
      , command
          "reorder"
          ( info
              ( DashWidgetsReorder
                  <$> strArgument (metavar "DASHBOARD_ID")
                  <*> optional (strOption (long "tab" <> metavar "TAB"))
                  <*> fileArg
                  <**> helper
              )
              (progDesc "Reorder widgets via {id: {x,y,w,h}} JSON/YAML")
          )
      ]


apiKeysParser :: Parser ApiKeysCommand
apiKeysParser =
  subparser $
    mconcat
      [ command "list" (info (pure KeyList) (progDesc "List API keys"))
      , command "get" (info (KeyGet <$> idArg <**> helper) (progDesc "Get API key"))
      , command "create" (info (KeyCreate <$> strArgument (metavar "TITLE") <**> helper) (progDesc "Create API key; prints plaintext once"))
      , command "activate" (info (KeyActivate <$> idArg <**> helper) (progDesc "Activate an API key"))
      , command "deactivate" (info (KeyDeactivate <$> idArg <**> helper) (progDesc "Deactivate an API key (soft)"))
      , command "revoke" (info (KeyDeactivate <$> idArg <**> helper) (progDesc "Deprecated alias for deactivate"))
      , command "delete" (info (KeyDelete <$> idArg <**> helper) (progDesc "Delete an API key"))
      ]


shareLinkParser :: Parser ShareLinkCommand
shareLinkParser =
  subparser $
    command
      "create"
      ( info
          ( ShareLinkCreate
              <$> strOption (long "event-id" <> metavar "UUID" <> help "Event ID")
              <*> strOption (long "created-at" <> metavar "ISO8601" <> help "Event created-at timestamp")
              <*> optional (strOption (long "type" <> metavar "TYPE" <> help "request|log|span (default: request)"))
              <**> helper
          )
          (progDesc "Create a 48h share link for a stored event")
      )


projectParser :: Parser ProjectCommand
projectParser =
  subparser $
    mconcat
      [ command "get" (info (pure ProjGet) (progDesc "Show the current project"))
      , command "patch" (info (ProjPatch <$> fileArg <**> helper) (progDesc "PATCH project fields from a YAML/JSON file"))
      ]


pageOpts :: Parser (Maybe Int, Maybe Int)
pageOpts =
  (,)
    <$> optional (option auto (long "page" <> metavar "N" <> help "Page number (0-based, default: 0)"))
    <*> optional (option auto (long "per-page" <> metavar "N" <> help "Items per page"))


-- | Serialise page/per_page into query-string pairs.
pageParams :: Maybe Int -> Maybe Int -> [(Text, Text)]
pageParams pM ppM = catMaybes [("page",) . show <$> pM, ("per_page",) . show <$> ppM]


issuesParser :: Parser IssuesCommand
issuesParser =
  subparser $
    mconcat
      [ command
          "list"
          ( info
              ( (\(p, pp) s t svc -> IssueList s t svc p pp)
                  <$> pageOpts
                  <*> optional (strOption (long "status" <> metavar "STATUS" <> help "open|acknowledged|archived|all"))
                  <*> optional (strOption (long "type" <> metavar "TYPE" <> help "Filter by issue_type"))
                  <*> optional (strOption (long "service" <> metavar "SERVICE" <> help "Filter by service"))
                  <**> helper
              )
              (progDesc "List issues")
          )
      , command "get" (info (IssueGet <$> idArg <**> helper) (progDesc "Get issue by ID"))
      , command "ack" (info (IssueAck <$> idArg <**> helper) (progDesc "Acknowledge issue"))
      , command "unack" (info (IssueUnack <$> idArg <**> helper) (progDesc "Un-acknowledge issue"))
      , command "archive" (info (IssueArchive <$> idArg <**> helper) (progDesc "Archive issue"))
      , command "unarchive" (info (IssueUnarchive <$> idArg <**> helper) (progDesc "Unarchive issue"))
      , command
          "bulk"
          ( info
              (IssueBulk <$> strArgument (metavar "ACTION" <> help "acknowledge|unack|archive|unarchive") <*> idsOpt <**> helper)
              (progDesc "Bulk acknowledge/archive issues")
          )
      ]


endpointsParser :: Parser EndpointsCommand
endpointsParser =
  subparser $
    mconcat
      [ command
          "list"
          ( info
              ( (\(p, pp) s o -> EndList s o p pp)
                  <$> pageOpts
                  <*> optional (strOption (long "search" <> metavar "TEXT" <> help "Substring filter"))
                  <*> optional (switch (long "outgoing" <> help "Only outgoing endpoints"))
                  <**> helper
              )
              (progDesc "List endpoints")
          )
      , command "get" (info (EndGet <$> idArg <**> helper) (progDesc "Get endpoint by ID"))
      ]


int64Arg :: Parser Int64
int64Arg = argument auto (metavar "ID" <> help "Pattern ID (integer)")


int64IdsOpt :: Parser [Int64]
int64IdsOpt =
  mapMaybe (readMaybe . toString) . T.splitOn ","
    <$> strOption (long "ids" <> metavar "ID1,ID2,..." <> help "Comma-separated pattern IDs")


logPatternsParser :: Parser LogPatternsCommand
logPatternsParser =
  subparser $
    mconcat
      [ command
          "list"
          ( info
              (uncurry LPList <$> pageOpts <**> helper)
              (progDesc "List log patterns")
          )
      , command "get" (info (LPGet <$> int64Arg <**> helper) (progDesc "Get log pattern by ID"))
      , command "ack" (info (LPAck <$> int64Arg <**> helper) (progDesc "Acknowledge log pattern"))
      , command
          "bulk"
          ( info
              ( LPBulk
                  <$> strArgument (metavar "ACTION" <> help "ack|ignore")
                  <*> int64IdsOpt
                  <**> helper
              )
              (progDesc "Run bulk action over multiple log patterns")
          )
      ]


teamsParser :: Parser TeamsCommand
teamsParser =
  subparser $
    mconcat
      [ command "list" (info (pure TeamList) (progDesc "List teams"))
      , command "get" (info (TeamGet <$> idArg <**> helper) (progDesc "Get a team by ID"))
      , command "create" (info (TeamCreate <$> fileArg <**> helper) (progDesc "Create team from YAML/JSON file"))
      , command "update" (info (TeamUpdate <$> idArg <*> fileArg <**> helper) (progDesc "Replace (PUT) team from file"))
      , command "patch" (info (TeamPatch <$> idArg <*> fileArg <**> helper) (progDesc "Patch team fields from file"))
      , command "delete" (info (TeamDelete <$> idArg <**> helper) (progDesc "Delete a team"))
      , command
          "bulk"
          ( info
              ( TeamBulk
                  <$> strArgument (metavar "ACTION" <> help "delete")
                  <*> idsOpt
                  <**> helper
              )
              (progDesc "Run bulk action over multiple teams")
          )
      ]


membersParser :: Parser MembersCommand
membersParser =
  subparser $
    mconcat
      [ command "list" (info (pure MemberList) (progDesc "List project members"))
      , command "get" (info (MemberGet <$> idArg <**> helper) (progDesc "Get a member by user ID"))
      , command
          "add"
          ( info
              ( MemberAdd
                  <$> optional (strOption (long "email" <> metavar "EMAIL" <> help "Email (creates user if missing)"))
                  <*> optional (strOption (long "user-id" <> metavar "UUID" <> help "Existing user ID"))
                  <*> optional (strOption (long "permission" <> metavar "PERM" <> help "view|edit|admin (default: view)"))
                  <**> helper
              )
              (progDesc "Add a project member (by email or user ID)")
          )
      , command
          "patch"
          ( info
              ( MemberPatch
                  <$> strArgument (metavar "USER_ID" <> help "Member's user ID")
                  <*> strArgument (metavar "PERMISSION" <> help "view|edit|admin")
                  <**> helper
              )
              (progDesc "Change a member's permission")
          )
      , command "remove" (info (MemberRemove <$> idArg <**> helper) (progDesc "Remove a project member"))
      ]


sendEventParser :: Parser SendEventOpts
sendEventParser =
  SendEventOpts
    <$> some (strOption (long "message" <> short 'm' <> metavar "TEXT" <> help "Event message (repeatable)"))
    <*> strOption (long "kind" <> metavar "KIND" <> value "log" <> showDefault <> help "log|trace|error")
    <*> strOption (long "level" <> short 'l' <> metavar "LEVEL" <> value "info" <> showDefault <> help "debug|info|warn|error")
    <*> strOption (long "service" <> metavar "NAME" <> value "monoscope-cli" <> showDefault <> help "Service name")
    <*> many (option (eitherReader parseKV) (long "tag" <> short 't' <> metavar "KEY:VALUE" <> help "Tag attribute (repeatable)"))
    <*> many (option (eitherReader parseKV) (long "extra" <> short 'e' <> metavar "KEY:VALUE" <> help "Extra attribute (repeatable)"))
    <*> many (option (eitherReader parseKV) (long "resource" <> short 'r' <> metavar "KEY:VALUE" <> help "Resource attribute (repeatable, e.g. service.version:1.2.3)"))


sendEventExamples :: String
sendEventExamples =
  intercalate "\n"
    [ "Examples:"
    , "  monoscope send-event -m \"Deploy completed\" --service api"
    , "  monoscope send-event -m \"Payment failed\" --level error -t user.id:u_123 -t plan:pro"
    , "  monoscope send-event -m \"Backup done\" -e duration:45s -e size:2.3GB"
    , "  monoscope send-event -m \"Step 1 complete\" -m \"all checks passed\" --kind trace"
    , ""
    , "Tip: pipe into CI scripts or bash error handlers to capture events automatically."
    ]


telemetryGenParser :: Parser TelemetryGenOpts
telemetryGenParser =
  TelemetryGenOpts
    <$> strOption (long "kind" <> metavar "KIND" <> value "trace" <> showDefault <> help "trace|log|metric")
    <*> option auto (long "rate" <> metavar "N" <> value 1.0 <> showDefault <> help "Events per second")
    <*> optional (option auto (long "count" <> short 'n' <> metavar "N" <> help "Total events to send (omit for continuous)"))
    <*> strOption (long "service" <> metavar "NAME" <> value "telemetrygen" <> showDefault <> help "Service name")
    <*> many (option (eitherReader parseKV) (long "resource" <> short 'r' <> metavar "KEY:VALUE" <> help "Resource attribute (repeatable, e.g. service.version:1.2.3)"))


telemetryGenExamples :: String
telemetryGenExamples =
  intercalate "\n"
    [ "Examples:"
    , "  monoscope telemetrygen --kind=trace --rate=1"
    , "  monoscope telemetrygen --kind=trace --rate=5 --count=100 --service=my-service"
    , ""
    , "OTLP endpoint is derived from the configured API URL (MONOSCOPE_API_URL)."
    ]


parserInfo :: ParserInfo (GlobalOpts, Command)
parserInfo =
  info
    (commandParser <**> versionOpt <**> helper)
    ( fullDesc
        <> progDesc "Monoscope CLI — terminal access to your observability platform"
        <> header "monoscope — Monoscope CLI"
    )


type CLIEffects = '[FileSystem, Environment, HTTP, IOE]


runCLI :: Eff CLIEffects a -> IO a
runCLI = runEff . runHTTPWreq . runEnvironment . runFileSystem


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (global, cmd) <- execParser parserInfo
  -- Make --debug equivalent to MONOSCOPE_DEBUG=1 so 'CLI.Core.printDebug'
  -- (which only sees the env) lights up either way. setEnv is process-global
  -- and not thread-safe, but we set it exactly once here, before runCLI
  -- starts any concurrent work, so the race window is empty.
  when global.debugFlag $ setEnv "MONOSCOPE_DEBUG" "1"
  runCLI $ run global cmd


-- | Apply --project override to the resolved config.
resolveCfgWith :: (FileSystem :> es, Environment :> es, IOE :> es) => GlobalOpts -> Eff es CLIConfig
resolveCfgWith g = do
  c <- resolveConfig
  pure $ case g.projectFlag of
    Nothing -> c
    Just pid -> c{projectId = Just pid}


-- | Resolve config + output mode and hand them to a subcommand handler.
-- Factors out the two-line preamble every resource branch repeats.
withCfgMode
  :: (FileSystem :> es, Environment :> es, IOE :> es)
  => GlobalOpts -> (CLIConfig -> OutputMode -> Eff es ()) -> Eff es ()
withCfgMode global k = do
  cfg <- resolveCfgWith global
  mode <- resolveMode global
  k cfg mode


run :: (FileSystem :> es, Environment :> es, HTTP :> es, IOE :> es) => GlobalOpts -> Command -> Eff es ()
run global = \case
  AuthCmd c -> runAuth c
  EventsCmd kindOverride evCmd -> withCfgMode global $ \cfg mode -> case evCmd of
    EvSearch opts ->
      let finalOpts = (opts :: EventsSearchOpts){kind = opts.kind <|> kindOverride}
       in runEventsSearch cfg finalOpts mode
    EvGet opts -> runEventsGet cfg opts mode
    EvTail opts -> runEventsTail cfg opts kindOverride
    EvContext opts -> runEventsContext cfg opts kindOverride mode
  MetricsCmd mCmd -> withCfgMode global $ \cfg mode -> case mCmd of
    MQuery opts -> runMetricsQuery cfg opts mode
    MChart opts -> runMetricsChart cfg opts
  ServicesCmd (SList opts) -> withCfgMode global $ \cfg mode -> runServicesList cfg opts mode
  ConfigCmd CInit -> runConfigInit
  ConfigCmd (CSet opts) -> runConfigSet opts
  ConfigCmd (CGet opts) -> do
    mode <- resolveMode global
    runConfigGet opts mode
  MonitorsCmd sub -> withCfgMode global $ \cfg mode -> case sub of
    MonList -> Resource.runList cfg Resource.Monitors [] mode
    MonGet i -> Resource.runGet cfg Resource.Monitors i mode
    MonCreate path -> Resource.runFromFile cfg Resource.POST (Resource.resourcePath Resource.Monitors) [] path mode
    MonUpdate i path -> Resource.runFromFile cfg Resource.PUT (Resource.resourceIdPath Resource.Monitors i) [] path mode
    MonPatch i path -> Resource.runFromFile cfg Resource.PATCH (Resource.resourceIdPath Resource.Monitors i) [] path mode
    MonApply path -> Resource.runApplyResource cfg Resource.Monitors path mode
    MonDelete i -> Resource.runDelete cfg Resource.Monitors i
    MonMute i minsM -> Resource.runLifecycle cfg Resource.Monitors i "mute" (maybe [] (\m -> [("duration_minutes", show m)]) minsM) mode
    MonUnmute i -> Resource.runLifecycle cfg Resource.Monitors i "unmute" [] mode
    MonResolve i -> Resource.runLifecycle cfg Resource.Monitors i "resolve" [] mode
    MonToggle i -> Resource.runLifecycle cfg Resource.Monitors i "toggle_active" [] mode
    MonBulk act ids durM -> Resource.runBulk cfg Resource.Monitors act ids durM mode
  DashboardsCmd sub -> withCfgMode global $ \cfg mode -> case sub of
    DashList -> Resource.runList cfg Resource.Dashboards [] mode
    DashGet i -> Resource.runGet cfg Resource.Dashboards i mode
    DashCreate path -> Resource.runFromFile cfg Resource.POST (Resource.resourcePath Resource.Dashboards) [] path mode
    DashUpdate i path -> Resource.runFromFile cfg Resource.PUT (Resource.resourceIdPath Resource.Dashboards i) [] path mode
    DashPatch i path -> Resource.runFromFile cfg Resource.PATCH (Resource.resourceIdPath Resource.Dashboards i) [] path mode
    DashDelete i -> Resource.runDelete cfg Resource.Dashboards i
    DashStar i -> Resource.runLifecycle cfg Resource.Dashboards i "star" [] mode
    DashUnstar i ->
      Resource.withResult (apiDelete cfg ("/api/v1/dashboards/" <> i <> "/star")) renderAPIError $ \() ->
        putTextLn $ "/api/v1/dashboards/" <> i <> "/star unstarred"
    DashDuplicate i -> Resource.runLifecycle cfg Resource.Dashboards i "duplicate" [] mode
    DashYaml i -> Resource.runYamlDump cfg Resource.Dashboards i
    DashApply path -> Resource.runApplyResource cfg Resource.Dashboards path mode
    DashBulk act ids -> Resource.runBulk cfg Resource.Dashboards act ids Nothing mode
    DashWidgetUpsert did path -> Resource.runFromFile cfg Resource.PUT ("/api/v1/dashboards/" <> did <> "/widgets") [] path mode
    DashWidgetDelete did wid ->
      Resource.withResult (apiDelete cfg ("/api/v1/dashboards/" <> did <> "/widgets/" <> wid)) renderAPIError $ \() ->
        putTextLn $ "/api/v1/dashboards/" <> did <> "/widgets/" <> wid <> " deleted"
    DashWidgetsReorder did tabM path ->
      Resource.runFromFile cfg Resource.PATCH ("/api/v1/dashboards/" <> did <> "/widgets/order") (maybe [] (\t -> [("tab", t)]) tabM) path mode
  ApiKeysCmd sub -> withCfgMode global $ \cfg mode -> case sub of
    KeyList -> Resource.runList cfg Resource.ApiKeys [] mode
    KeyGet i -> Resource.runGet cfg Resource.ApiKeys i mode
    KeyCreate title ->
      Resource.runAPI mode $ apiPostJson @_ @_ @AE.Value cfg "/api/v1/api_keys" [] (AE.object ["title" AE..= title])
    KeyActivate i -> Resource.runLifecycle cfg Resource.ApiKeys i "activate" [] mode
    KeyDeactivate i -> Resource.runLifecycle cfg Resource.ApiKeys i "deactivate" [] mode
    KeyDelete i -> Resource.runDelete cfg Resource.ApiKeys i
  ShareLinkCmd (ShareLinkCreate eid createdAt typeM) -> withCfgMode global $ \cfg mode ->
    Resource.runAPI mode $
      apiPostJson @_ @_ @AE.Value cfg "/api/v1/share" [] $
        AE.object
          [ "event_id" AE..= eid
          , "event_created_at" AE..= createdAt
          , "event_type" AE..= typeM
          ]
  MeCmd -> withCfgMode global $ \cfg mode ->
    Resource.runAPI mode (apiGetJson @_ @AE.Value cfg "/api/v1/me" [])
  ProjectCmd sub -> withCfgMode global $ \cfg mode -> case sub of
    ProjGet -> Resource.runAPI mode (apiGetJson @_ @AE.Value cfg "/api/v1/project" [])
    ProjPatch path -> Resource.runFromFile cfg Resource.PATCH "/api/v1/project" [] path mode
  IssuesCmd sub -> withCfgMode global $ \cfg mode -> case sub of
    IssueList statusM typeM svcM pageM perM ->
      let params = catMaybes [("status",) <$> statusM, ("type",) <$> typeM, ("service",) <$> svcM] <> pageParams pageM perM
       in Resource.runAPI mode (Resource.normalizeList <<$>> apiGetJson @_ @AE.Value cfg "/api/v1/issues" params)
    IssueGet i -> Resource.runAPI mode (apiGetJson @_ @AE.Value cfg ("/api/v1/issues/" <> i) [])
    IssueAck i -> Resource.writeJson cfg Resource.POST ("/api/v1/issues/" <> i <> "/ack") [] AE.Null mode
    IssueUnack i -> Resource.writeJson cfg Resource.POST ("/api/v1/issues/" <> i <> "/unack") [] AE.Null mode
    IssueArchive i -> Resource.writeJson cfg Resource.POST ("/api/v1/issues/" <> i <> "/archive") [] AE.Null mode
    IssueUnarchive i -> Resource.writeJson cfg Resource.POST ("/api/v1/issues/" <> i <> "/unarchive") [] AE.Null mode
    IssueBulk act ids ->
      Resource.writeJson cfg Resource.POST "/api/v1/issues/bulk" [] (AE.object ["action" AE..= act, "ids" AE..= ids]) mode
  EndpointsCmd sub -> withCfgMode global $ \cfg mode -> case sub of
    EndList searchM outgoingM pageM perM ->
      let params = catMaybes [("search",) <$> searchM, ("outgoing",) . bool "false" "true" <$> outgoingM] <> pageParams pageM perM
       in Resource.runAPI mode (Resource.normalizeList <<$>> apiGetJson @_ @AE.Value cfg "/api/v1/endpoints" params)
    EndGet i -> Resource.runAPI mode (apiGetJson @_ @AE.Value cfg ("/api/v1/endpoints/" <> i) [])
  LogPatternsCmd sub -> withCfgMode global $ \cfg mode -> case sub of
    LPList pageM perM ->
      Resource.runAPI mode (Resource.normalizeList <<$>> apiGetJson @_ @AE.Value cfg "/api/v1/log_patterns" (pageParams pageM perM))
    LPGet i -> Resource.runAPI mode (apiGetJson @_ @AE.Value cfg ("/api/v1/log_patterns/" <> show i) [])
    LPAck i -> Resource.writeJson cfg Resource.POST ("/api/v1/log_patterns/" <> show i <> "/ack") [] AE.Null mode
    LPBulk act ids ->
      Resource.writeJson cfg Resource.POST "/api/v1/log_patterns/bulk" [] (AE.object ["action" AE..= act, "ids" AE..= ids]) mode
  TeamsCmd sub -> withCfgMode global $ \cfg mode -> case sub of
    TeamList -> Resource.runList cfg Resource.Teams [] mode
    TeamGet i -> Resource.runGet cfg Resource.Teams i mode
    TeamCreate path -> Resource.runFromFile cfg Resource.POST (Resource.resourcePath Resource.Teams) [] path mode
    TeamUpdate i path -> Resource.runFromFile cfg Resource.PUT (Resource.resourceIdPath Resource.Teams i) [] path mode
    TeamPatch i path -> Resource.runFromFile cfg Resource.PATCH (Resource.resourceIdPath Resource.Teams i) [] path mode
    TeamDelete i -> Resource.runDelete cfg Resource.Teams i
    TeamBulk act ids -> Resource.runBulk cfg Resource.Teams act ids Nothing mode
  MembersCmd sub -> withCfgMode global $ \cfg mode -> case sub of
    MemberList -> Resource.runList cfg Resource.Members [] mode
    MemberGet i -> Resource.runGet cfg Resource.Members i mode
    MemberAdd emailM uidM permM ->
      let body =
            AE.object $
              catMaybes
                [ ("email" AE..=) <$> emailM
                , ("user_id" AE..=) <$> uidM
                , ("permission" AE..=) <$> permM
                ]
       in Resource.writeJson cfg Resource.POST (Resource.resourcePath Resource.Members) [] body mode
    MemberPatch uid perm ->
      Resource.writeJson cfg Resource.PATCH (Resource.resourceIdPath Resource.Members uid) [] (AE.object ["permission" AE..= perm]) mode
    MemberRemove uid -> Resource.runDelete cfg Resource.Members uid
  SchemaCmd schemaOpts -> withCfgMode global $ \cfg mode ->
    Resource.runAPI mode (filterSchema schemaOpts <<$>> apiGetJson @_ @Schema.Schema cfg "/api/v1/schema" [])
  FacetsCmd facetsOpts -> withCfgMode global $ \cfg mode -> do
    let params = catMaybes [("since",) <$> facetsOpts.facetSince, ("field",) <$> facetsOpts.facetField]
    Resource.runAPI mode (capFacets facetsOpts.facetTopN <<$>> apiGetJson @_ @AE.Value cfg "/api/v1/facets" params)
  CompletionCmd shell -> emitCompletion shell
  VersionCmd -> putTextLn $ "monoscope " <> toText (showVersion Paths.version)
  TelemetryGenCmd opts -> withCfgMode global $ \cfg _ -> runTelemetryGen cfg opts
  SendEventCmd opts -> withCfgMode global $ \cfg _ -> runSendEvent cfg opts


resolveMode :: (Environment :> es, IOE :> es) => GlobalOpts -> Eff es OutputMode
resolveMode global
  | global.agentFlag = pure OutputJSON
  | otherwise = detectOutputMode global.jsonFlag global.outputFlag


-- | C3: post-process the @/api/v1/schema@ response so agents can fetch a
-- focused slice. The full payload is huge — without filtering, an LLM agent
-- spends 600+ tokens on field names that don't matter for the current
-- investigation. Operates on the typed 'Schema.Schema' so a server-side
-- field rename surfaces as a compile error, not a silent miss.
filterSchema :: SchemaOpts -> Schema.Schema -> Schema.Schema
filterSchema opts s
  | isNothing opts.search && isNothing opts.schemaLimit = s
  | otherwise =
      let needle = T.toLower <$> opts.search
          matches k = maybe True (`T.isInfixOf` T.toLower k) needle
          filtered = Map.filterWithKey (\k _ -> matches k) s.fields
          capped = maybe filtered (`Map.take` filtered) opts.schemaLimit
       in Schema.Schema {fields = capped}


-- | Trim each field's value list to the top-N. The server already sorts by
-- count descending, so a simple @take@ keeps the most popular values.
-- The fallthrough handles @Nothing@ (no cap requested) and the @Just n@ +
-- non-Object case (server returned an unexpected shape).
capFacets :: Maybe Int -> AE.Value -> AE.Value
capFacets (Just n) (AE.Object obj) =
  AE.Object $ flip KM.map obj $ \case
    AE.Array xs -> AE.Array (V.take n xs)
    other -> other
capFacets _ v = v


-- | Emit a shell completion script via optparse-applicative's built-in support.
-- C12: Unknown shells used to silently fall back to bash, which is worse than
-- failing — a user who typed @completion fis@ would get a bash script that
-- silently doesn't work in their fish shell. Now we exit non-zero with a
-- clear message that lists the supported shells.
emitCompletion :: IOE :> es => Text -> Eff es ()
emitCompletion shell = liftIO $ case shell of
  "bash" -> emit "--bash-completion-script"
  "zsh" -> emit "--zsh-completion-script"
  "fish" -> emit "--fish-completion-script"
  other -> do
    Data.Text.IO.hPutStrLn stderr $ "error: unknown shell '" <> other <> "'; supported: bash|zsh|fish"
    exitFailure
  where
    emit shellArg = void $ handleParseResult $ execParserPure defaultPrefs parserInfo [shellArg, "monoscope"]
