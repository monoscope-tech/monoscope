-- | CLI entry point, in the library so integration tests can drive the real
-- parser + command pipeline in-process (see CLI.CLISpec). The @monoscope@
-- executable is a thin wrapper passing in its Paths_monoscope version.
module CLI.Main (cliMain, parserInfo, run, GlobalOpts (..)) where

import Relude
import Relude.Extra.Tuple (toSnd)
import System.IO (hSetEncoding, isEOF, utf8)

import CLI.Commands hiding (value)
import CLI.Config (CLIConfig (..), resolveConfig)
import CLI.Core (Ingestion, OutputMode (..), apiGetJson, detectOutputMode, dieWith, runIngestion, setOutputMode)
import CLI.Resource qualified as Resource
import CLI.UI (Picker, runPicker)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.Effectful.AgentWorkspace qualified as Workspace
import Data.Effectful.AgentWorkspace.Guest qualified as Guest
import Data.Effectful.Wreq (HTTP, runHTTPWreq)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified
import Data.Vector qualified as V
import Data.Version (Version, showVersion)
import Effectful
import Effectful.Environment (Environment, runEnvironment)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Models.Telemetry.Schema qualified as Schema
import Options.Applicative
import Options.Applicative.Help.Pretty (pretty, vsep)
import System.Environment (setEnv)
import Web.Operations qualified as Op


data GlobalOpts = GlobalOpts
  { jsonFlag :: Bool
  , yamlFlag :: Bool
  , tableFlag :: Bool
  , debugFlag :: Bool
  , projectFlag :: Maybe Text
  }


data Command
  = AuthCmd AuthCommand
  | EventsCmd (Maybe Text) EventsCommand
  | MetricsCmd MetricsCommand
  | ServicesCmd ServicesCommand
  | ConfigCmd ConfigCommand
  | ApiCmd Resource.ApiCall
  | DashRenderCmd DashboardRenderOpts
  | SchemaCmd SchemaOpts
  | FacetsCmd FacetsOpts
  | CompletionCmd Text
  | VersionCmd
  | TelemetryGenCmd TelemetryGenOpts
  | SendEventCmd SendEventOpts
  | ChartCmd ChartCmdOpts
  | StatusCmd StatusOpts
  | OpenCmd OpenOpts
  | GuestSessionCmd FilePath
  | WorkspaceCmd FilePath Workspace.Options Workspace.Operation
  | WorkspaceMaintainCmd FilePath
  | WorkspacePruneCmd FilePath Workspace.PruneOptions
  | WorkspaceLeaseCmd FilePath Workspace.Options
  | RegressionCmd FilePath Workspace.RegressionOptions


data EventsCommand
  = EvSearch EventsSearchOpts
  | EvGet EventsGetOpts
  | EvTail EventsTailOpts
  | EvContext EventsContextOpts
  deriving stock (Show)


data MetricsCommand
  = MQuery MetricsQueryOpts
  | MList MetricsListOpts
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


-- | Output is picked from three mutually-exclusive flags. Precedence when more
-- than one is set: @--json > --yaml > --table@. With none set, output is
-- auto-detected from stdout: TTY → table, pipe/redirect → JSON. This replaces
-- the older @--output FORMAT@ + @--agent@ + @MONOSCOPE_AGENT_MODE@ stack.
globalOpts :: Parser GlobalOpts
globalOpts =
  GlobalOpts
    <$> switch (long "json" <> help "Emit JSON (also forced when stdout is piped)")
    <*> switch (long "yaml" <> help "Emit YAML")
    <*> switch (long "table" <> help "Force pretty-printed table (overrides pipe auto-detect)")
    <*> switch (long "debug" <> help "Print outgoing requests to stderr (also: MONOSCOPE_DEBUG=1)")
    <*> optional (strOption (long "project" <> short 'p' <> metavar "PID" <> help "Project UUID override"))


-- | Multi-line examples in @--help@. Plain 'footer' reflows its string into a
-- paragraph, which turns a list of runnable commands into an unreadable wall —
-- so the lines are laid out explicitly and stay copy-pasteable.
examplesFooter :: [Text] -> InfoMod a
examplesFooter ls = footerDoc (Just (vsep (map (pretty . toString) ("" : ls))))


versionOpt :: Version -> Parser (a -> a)
versionOpt v =
  infoOption
    ("monoscope " <> showVersion v)
    (long "version" <> help "Print CLI version and exit")


commandParser :: Parser (GlobalOpts, Command)
commandParser =
  (,)
    <$> globalOpts
    <*> subparser
      ( mconcat
          [ cmd "workspace" workspaceParser (progDesc "Prepare or verify a private agent build cache")
          , cmd "auth" (AuthCmd <$> authParser) (progDesc "Manage authentication")
          , cmd "events" (EventsCmd Nothing <$> eventsParser) (progDesc "Search and inspect events")
          , cmd "logs" (EventsCmd (Just "log") <$> eventsParser) (progDesc "Search logs")
          , cmd "traces" (EventsCmd (Just "trace") <$> eventsParser) (progDesc "Search traces")
          , cmd "metrics" (MetricsCmd <$> metricsParser) (progDesc "Query and chart metrics")
          , cmd "chart" (ChartCmd <$> chartParser) (progDesc "Chart any KQL query in the terminal" <> examplesFooter chartExamples)
          , cmd "status" (StatusCmd <$> statusParser) (progDesc "One-screen project health: throughput, errors, open issues, alerting monitors")
          , cmd "open" (OpenCmd <$> openParser) (progDesc "Open (or print) the web UI link for a resource" <> examplesFooter openExamples)
          , cmd "services" (ServicesCmd <$> servicesParser) (progDesc "List services")
          , cmd "config" (ConfigCmd <$> configParser) (progDesc "Manage configuration")
          , cmd "schema" (SchemaCmd <$> schemaParser) (progDesc "Fetch telemetry schema")
          , cmd "facets" (FacetsCmd <$> facetsParser) (progDesc "Discover popular field values (top-N per faceted field)" <> examplesFooter facetsExamples)
          , cmd "completion" (CompletionCmd <$> strArgument (metavar "SHELL" <> help "bash|zsh|fish")) (progDesc "Emit shell completion script")
          , cmd "version" (pure VersionCmd) (progDesc "Show CLI version")
          , cmd "telemetrygen" (TelemetryGenCmd <$> telemetryGenParser) (progDesc "Generate synthetic telemetry (traces/logs/metrics)" <> examplesFooter telemetryGenExamples)
          , cmd "send-event" (SendEventCmd <$> sendEventParser) (progDesc "Send a real event (log/trace/error) from a script or CI" <> examplesFooter sendEventExamples)
          ]
          <> commandTree ((["dashboards", "render"], DashRenderCmd <$> dashRenderParser, progDesc "Draw a dashboard in the terminal" <> examplesFooter dashRenderExamples) : apiLeaves)
      )
  where
    workspaceParser =
      subparser
        $ mconcat
          [ cmd "session" (GuestSessionCmd <$> requestOption) (progDesc "Run a native agent guest session")
          , cmd "maintain" (WorkspaceMaintainCmd <$> strOption (long "config" <> metavar "FILE")) (progDesc "Archive and evict idle checkpoints to the host cache budget")
          , cmd "prune" (WorkspacePruneCmd <$> requestOption <*> (Workspace.PruneOptions <$> strOption (long "cache" <> metavar "DIRECTORY") <*> remoteOption)) (progDesc "Evict idle archived project checkpoints to an allocated-byte budget")
          , cmd "load" (options (pure Workspace.Lookup)) (progDesc "Verify a prepared workspace")
          , cmd "archive" (options (Workspace.Archive <$> remoteOption)) (progDesc "Archive a verified checkpoint in self-hosted object storage")
          , cmd "restore" (options (Workspace.Restore <$> remoteOption)) (progDesc "Restore and verify an archived checkpoint")
          , cmd "check" (RegressionCmd <$> strOption (long "request" <> metavar "FILE") <*> (Workspace.RegressionOptions <$> strOption (long "artifacts" <> metavar "DIRECTORY") <*> strOption (long "checkout" <> metavar "DIRECTORY") <*> strOption (long "image" <> metavar "IMAGE") <*> strOption (long "bridge" <> metavar "EXECUTABLE"))) (progDesc "Replay and retain a frozen regression in isolated workspaces")
          , cmd "lease" leaseOptions (progDesc "Hold a private prepared workspace until stdin closes")
          , cmd "prepare" (options (Workspace.Prepare <$> strOption (long "checkout" <> metavar "DIRECTORY"))) (progDesc "Build and atomically publish a pinned workspace")
          ]
    remoteOption = strOption (long "remote" <> metavar "FILE" <> help "Private JSON endpoint/bucket/region/accessKey/secretKey/maxBytes configuration")
    leaseOptions = WorkspaceLeaseCmd <$> requestOption <*> workspaceOptions
    options operation = WorkspaceCmd <$> requestOption <*> workspaceOptions <*> operation
    requestOption = strOption (long "request" <> metavar "FILE" <> help "Private JSON project/repository/base request")
    workspaceOptions =
      Workspace.Options
        <$> strOption (long "cache" <> metavar "DIRECTORY")
        <*> strOption (long "image" <> metavar "IMAGE")
        <*> strOption (long "bridge" <> metavar "EXECUTABLE")
        <*> strOption (long "script" <> metavar "FILE")
        <*> optional (strOption (long "dependencies" <> metavar "FILE"))


cmd :: String -> Parser a -> InfoMod a -> Mod CommandFields a
cmd name parser modifiers = command name (info (parser <**> helper) modifiers)


authParser :: Parser AuthCommand
authParser =
  subparser
    $ mconcat
      [ cmd "login" (AuthLogin <$> optional (strOption (long "token" <> metavar "TOKEN" <> help "API token"))) (progDesc "Authenticate")
      , cmd "status" (pure AuthStatus) (progDesc "Show auth state")
      , cmd "logout" (pure AuthLogout) (progDesc "Remove credentials")
      ]


eventsParser :: Parser EventsCommand
eventsParser =
  subparser
    $ mconcat
      [ cmd "search" (EvSearch <$> eventsSearchParser) (progDesc "Search events" <> examplesFooter eventsSearchExamples)
      , cmd "get" (EvGet <$> eventsGetParser) (progDesc "Get event by ID")
      , cmd "tail" (EvTail <$> eventsTailParser) (progDesc "Stream events")
      , cmd "context" (EvContext <$> eventsContextParser) (progDesc "Context around timestamp")
      ]


eventsSearchParser :: Parser EventsSearchOpts
eventsSearchParser =
  EventsSearchOpts
    <$> strArgument (metavar "QUERY" <> value "" <> help "KQL search query (use == for equality, see `monoscope schema`)")
    <*> optional (strOption (long "since" <> metavar "DURATION" <> help "Relative time (e.g. 1h, 30m, 7d)"))
    <*> optional (strOption (long "from" <> metavar "TIMESTAMP" <> help "Start time (ISO 8601)"))
    <*> optional (strOption (long "to" <> metavar "TIMESTAMP" <> help "End time (ISO 8601)"))
    <*> optional (strOption (long "kind" <> metavar "KIND" <> help "log|trace|span"))
    <*> many (strOption (long "service" <> metavar "SERVICE" <> help "Filter by service (repeatable: --service api --service worker → in (api, worker))"))
    <*> optional (strOption (long "level" <> metavar "LEVEL" <> help "Filter by severity (case-insensitive input)"))
    <*> optional (option auto (long "limit" <> short 'n' <> metavar "N" <> help "Max results"))
    <*> optional (strOption (long "fields" <> metavar "FIELDS" <> help "Comma-separated fields to keep"))
    <*> optional (strOption (long "cursor" <> metavar "CURSOR" <> help "Pagination cursor from a prior response"))
    <*> switch (long "first" <> help "Return only the first matching event")
    <*> switch (long "id-only" <> help "Print just the first event id (implies --first)")
    <*> switch (long "with-children" <> help "Also return descendants (the sub-tree) of each matched span (default: predicate hits only)")
    -- Wide --since windows are fetched in --chunk-hours slices internally (to
    -- dodge the CF 504 timeout) and merged client-side: the output is always
    -- a single envelope, identical to a single-request search.
    <*> ( (\h legacy -> h <|> (0 <$ guard legacy)) -- --no-chunk: hidden back-compat alias for --chunk-hours 0
            <$> optional (option auto (long "chunk-hours" <> metavar "H" <> help "Hours per internal fetch slice for wide --since windows (default: 1; 0 = single request)"))
            <*> switch (long "no-chunk" <> internal)
        )
    <*> formatOpt


-- | C5: Concrete KQL examples in @--help@ — agents copy from here, so keep
-- them up-to-date and runnable.
eventsSearchExamples :: [Text]
eventsSearchExamples =
  [ "Examples:"
  , "  monoscope logs search POISON_ROW_DROPPED --since 24h   # bare strings = full-text"
  , "  monoscope events search 'severity.severity_text==\"error\"' --since 1h"
  , "  monoscope logs search --service checkout-api --level error --limit 50"
  , "  monoscope events search 'attributes.http.response.status_code >= 500' --since 1h"
  , "  monoscope traces search --since 30m --first --id-only   # one trace id"
  , "  monoscope events search '' --since 1h --cursor <CURSOR>  # next page"
  , ""
  , "Bare strings (no KQL operators) are searched in body+summary automatically."
  , "KQL operators: == != > >= < <=  |  combine with: and or  |  parens for grouping"
  , "Run `monoscope schema --search service` to discover field names."
  ]


eventsGetParser :: Parser EventsGetOpts
eventsGetParser =
  EventsGetOpts
    <$> strArgument (metavar "ID" <> help "Event or trace ID")
    <*> switch (long "tree" <> help "Show trace tree view")
    <*> optional (strOption (long "at" <> metavar "TIMESTAMP" <> help "ISO-8601 timestamp for a fast point-in-time lookup (avoids 90d range scan)"))
    -- project specific fields out of the JSON blob without forcing jq.
    -- @--field body --field summary@ prints only those keys; @--show-body@
    -- is a one-flag shorthand for the common "I just want the message" case.
    <*> many (strOption (long "field" <> metavar "FIELD" <> help "Project a single field from the event JSON (repeatable)"))
    <*> switch (long "show-body" <> help "Shorthand for --field body --field summary")
    <*> optional (strOption (long "since" <> metavar "DURATION" <> help "Window to look the id up in (default: 90d). Narrow it if the store refuses the scan"))


eventsTailParser :: Parser EventsTailOpts
eventsTailParser =
  EventsTailOpts
    <$> optional (strOption (long "kind" <> metavar "KIND"))
    <*> optional (strOption (long "service" <> metavar "SERVICE"))
    <*> optional (strOption (long "level" <> metavar "LEVEL"))
    <*> optional (strOption (long "grep" <> metavar "PATTERN" <> help "Only print events whose message, span name or service contains PATTERN"))
    <*> formatOpt
    <*> optional (strOption (long "interval" <> short 'i' <> metavar "DURATION" <> help "Poll cadence (default: 2s)"))
    <*> optional (strOption (long "since" <> metavar "DURATION" <> help "Print this much history before following (like tail -n)"))


-- | Shared @--format@ flag for the event-shaped commands.
formatOpt :: Parser (Maybe Text)
formatOpt = optional (strOption (long "format" <> short 'f' <> metavar "FORMAT" <> help "line|table|logfmt (default: line). Ignored under --json/--yaml"))


eventsContextParser :: Parser EventsContextOpts
eventsContextParser =
  EventsContextOpts
    <$> strOption (long "at" <> metavar "TIMESTAMP" <> help "Center timestamp")
    <*> optional (strOption (long "service" <> metavar "SERVICE"))
    <*> optional (strOption (long "kind" <> metavar "KIND"))
    <*> optional (strOption (long "window" <> metavar "DURATION" <> help "Total time window centered on --at (default: 5m)"))
    <*> switch (long "summary" <> help "Add per-trace summary to JSON output (trace_id, services, span_count, error_count)")
    <*> formatOpt


metricsParser :: Parser MetricsCommand
metricsParser =
  subparser
    $ mconcat
      [ cmd "query" (MQuery <$> metricsQueryParser) (progDesc "Run metrics query")
      , cmd "list" (MList <$> metricsListParser) (progDesc "List recently seen metrics")
      , cmd "chart" (MChart <$> metricsChartParser) (progDesc "Render chart")
      ]


metricsQueryParser :: Parser MetricsQueryOpts
metricsQueryParser =
  MetricsQueryOpts
    <$> strArgument (metavar "EXPRESSION" <> help "Metrics expression")
    <*> optional (strOption (long "since" <> metavar "DURATION"))
    <*> optional (strOption (long "from" <> metavar "TIMESTAMP"))
    <*> optional (strOption (long "to" <> metavar "TIMESTAMP"))
    <*> optional (strOption (long "assert" <> metavar "CONDITION" <> help "Assert condition (e.g. '< 0.01')"))


metricsListParser :: Parser MetricsListOpts
metricsListParser =
  MetricsListOpts
    <$> optional (strOption (long "service" <> metavar "NAME" <> help "Exact service name"))
    <*> optional (strOption (long "search" <> metavar "TEXT" <> help "Metric name search"))
    <*> optional (option auto (long "limit" <> metavar "COUNT" <> help "Page size (max 100)"))
    <*> optional (option auto (long "offset" <> metavar "COUNT" <> help "Page offset"))
    <*> switch (long "inactive" <> help "List metrics last seen more than seven days ago")


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
  subparser
    $ cmd "list" (SList <$> servicesListParser) (progDesc "List services")


servicesListParser :: Parser ServicesListOpts
servicesListParser =
  ServicesListOpts
    <$> optional (strOption (long "since" <> metavar "DURATION"))


configParser :: Parser ConfigCommand
configParser =
  subparser
    $ mconcat
      [ cmd "init" (pure CInit) (progDesc "Initialize configuration")
      , cmd "set" (CSet <$> configSetParser) (progDesc "Set config value")
      , cmd "get" (CGet <$> configGetParser) (progDesc "Get config value")
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
    -- D1: accept --limit alongside --top so every listing shares the same flag.
    <*> optional (option auto (long "limit" <> long "top" <> short 'n' <> metavar "N" <> help "Cap values per field (alias: --top)"))


-- | Examples shown in `monoscope facets --help`. Mirrors the discovery
-- workflow most agents follow: dump-everything → drill-into-one-field →
-- pipe-into-search.
facetsExamples :: [Text]
facetsExamples =
  [ "Examples:"
  , "  monoscope facets                         # all faceted fields"
  , "  monoscope facets resource.service.name   # values for one field"
  , "  monoscope facets severity.severity_text --top 5"
  , "  monoscope facets resource.service.name | jq -r '.[\"resource.service.name\"][].value'"
  , "  monoscope facets --since 7d              # widen the lookback"
  , ""
  , "Each value carries a `count`. Pop the top result into a search:"
  , "  SVC=$(monoscope facets resource.service.name --top 1 | jq -r '.[\"resource.service.name\"][0].value')"
  , "  monoscope events search '' --service \"$SVC\" --since 1h"
  ]


-- Plan A resource parsers

idArg :: Parser (Maybe Text)
idArg = optional $ strArgument (metavar "ID" <> help "Resource ID (omit to select interactively)")


-- With one positional argument it is the value; with two they remain ID VALUE.
idValueArgs :: Text -> Parser (Maybe Text, Text)
idValueArgs valueName =
  (\arg1 arg2 -> maybe (Nothing, arg1) (Just arg1,) arg2)
    <$> strArgument (metavar ("ID_OR_" <> toString valueName))
    <*> optional (strArgument (metavar (toString valueName)))


-- | Every catalogued CLI entry as a leaf: path params after the first and
-- positional args share the positional slots, flags parse per their kind.
apiLeaves :: [([Text], Parser Command, InfoMod Command)]
apiLeaves =
  [ (toList c.command, ApiCmd <$> call, progDesc (toString o.summary))
  | o <- Op.operations
  , c <- o.cli
  , let params = Op.pathParams o.path
        slots = [Left n | n <- drop 1 params] <> [Right p | Op.Pos p <- c.args]
        positionals = case (params, slots) of
          (_ : _, [s]) -> second pure <$> idValueArgs (metaOf s)
          (_ : _, ss) -> (,) <$> idArg <*> traverse strArg ss
          ([], ss) -> (Nothing,) <$> traverse strArg ss
        call =
          (\(ident, vs) opts -> Resource.ApiCall o c.listing ident [v | (Left _, v) <- zip slots vs] ([positional p v | (Right p, v) <- zip slots vs] <> catMaybes opts))
            <$> positionals
            <*> traverse optionParser [opt | Op.Opt opt <- c.args]
  ]
  where
    metaOf = either T.toUpper (.meta)
    strArg s = strArgument (metavar (toString (metaOf s)) <> foldMap (help . toString . (.help)) (rightToMaybe s))
    positional p v = case p.target of
      Op.BodyText k -> Resource.BodyField (AK.fromText k) (AE.String v)
      Op.BodyFile -> Resource.BodyFrom (toString v)
      Op.ApplyFiles -> Resource.ApplyFrom (toString v)
    optionParser opt =
      let mods :: HasName f => Mod f x
          mods = foldMap (\n -> maybe (long (toString n)) (short . fst) (guard (T.length n == 1) *> T.uncons n)) opt.names <> help (toString opt.help)
          valued :: Mod OptionFields x
          valued = mods <> metavar (toString opt.meta)
          ids f = eitherReader (traverse f . T.splitOn "," . toText)
          -- Query-string and JSON-body forms of one parsed value; the wire picks one.
          parsed = case opt.kind of
            Op.Str -> toSnd AE.String <$> strOption valued
            Op.Int -> (\n -> (show n, AE.toJSON n)) <$> option (auto @Int64) valued
            Op.Ids -> (\xs -> (T.intercalate "," xs, AE.toJSON xs)) <$> option (ids Right) valued
            Op.IntIds -> (\xs -> (T.intercalate "," (map show xs), AE.toJSON xs)) <$> option (ids (maybeToRight "expected integer ids" . readMaybe @Int64 . toString)) valued
            Op.Switch -> ("true", AE.Bool True) <$ flag' () mods
          input (q, j) = case opt.wire of
            Op.Query k -> Resource.QueryParam k q
            Op.Body k -> Resource.BodyField (AK.fromText k) j
          need = case opt.need of
            Op.Required -> id
            Op.Optional -> (<|> pure Nothing)
       in need (Just . input <$> parsed)


-- | Nest leaves sharing a leading word under one subcommand, in first-seen order.
commandTree :: [([Text], Parser a, InfoMod a)] -> Mod CommandFields a
commandTree leaves =
  mconcat
    [ case [(rest, p, m) | (w' : rest, p, m) <- leaves, w' == w] of
        [([], p, m)] -> cmd (toString w) p m
        grouped -> cmd (toString w) (subparser (commandTree grouped)) (progDesc (toString (Map.findWithDefault w w groupDescriptions)))
    | w <- ordNub [w | (w : _, _, _) <- leaves]
    ]
  where
    groupDescriptions =
      Map.fromList
        [ ("monitors", "Manage alert monitors")
        , ("dashboards", "Manage dashboards")
        , ("widget", "Widget-level operations")
        , ("api-keys", "Manage API keys")
        , ("share-link", "Create time-limited share links")
        , ("project", "Show or patch the current project")
        , ("issues", "List and triage issues")
        , ("incidents", "Browse incident episodes")
        , ("endpoints", "Browse the API endpoint catalog")
        , ("log-patterns", "Triage log patterns")
        , ("teams", "Manage teams")
        , ("members", "Manage project members")
        ]


dashRenderParser :: Parser DashboardRenderOpts
dashRenderParser =
  DashboardRenderOpts
    <$> idArg
    <*> optional (strOption (long "tab" <> metavar "SLUG" <> help "Tab to render (default: the first one)"))
    <*> optional (strOption (long "widget" <> metavar "ID" <> help "Render a single widget, full width"))
    <*> optional (strOption (long "since" <> metavar "DURATION" <> help "Lookback window (default: 1h)"))
    <*> optional (strOption (long "from" <> metavar "TIMESTAMP"))
    <*> optional (strOption (long "to" <> metavar "TIMESTAMP"))
    <*> many (option (eitherReader (parseSepKV '=' "KEY=VALUE")) (long "var" <> metavar "KEY=VALUE" <> help "Dashboard variable (repeatable)"))
    <*> optional (strOption (long "watch" <> short 'w' <> metavar "INTERVAL" <> help "Redraw every INTERVAL (e.g. 30s)"))


dashRenderExamples :: [Text]
dashRenderExamples =
  [ "Examples:"
  , "  monoscope dashboards render                            # pick a dashboard"
  , "  monoscope dashboards render <ID>                       # whole dashboard, live layout"
  , "  monoscope dashboards render <ID> --since 24h --watch 1m"
  , "  monoscope dashboards render <ID> --widget p99_latency   # one widget, full width"
  , "  monoscope dashboards render <ID> --tab errors --var service=checkout"
  , "  monoscope dashboards render <ID> --json | jq '.widgets[].title'"
  , ""
  , "Widgets are drawn in their grid positions, so the terminal layout matches"
  , "the browser. Piping (or --json) emits the resolved data instead of ANSI."
  ]


statusParser :: Parser StatusOpts
statusParser = StatusOpts <$> optional (strOption (long "since" <> metavar "DURATION" <> help "Window to summarise (default: 1h)"))


openParser :: Parser OpenOpts
openParser =
  OpenOpts
    <$> strArgument (metavar "TARGET" <> help "logs|trace|issue|dashboard|monitors|endpoints|project")
    <*> optional (strArgument (metavar "ID_OR_QUERY" <> help "Trace/issue/dashboard id, or a KQL query for `logs`"))
    <*> optional (strOption (long "since" <> metavar "DURATION" <> help "Time range to open with"))
    <*> switch (long "print" <> help "Print the URL without launching a browser")


openExamples :: [Text]
openExamples =
  [ "Examples:"
  , "  monoscope open trace a1b2c3d4                      # the trace waterfall, in the browser"
  , "  monoscope open issue <issue-id>"
  , "  monoscope open dashboard <dashboard-id>"
  , "  monoscope open logs 'severity.severity_text==\"error\"' --since 6h"
  , "  monoscope open project --print                     # just print the link, e.g. to paste in Slack"
  , ""
  , "The host comes from the server (/api/v1/me), so self-hosted installs work."
  ]


chartParser :: Parser ChartCmdOpts
chartParser =
  ChartCmdOpts
    <$> strArgument (metavar "QUERY" <> help "KQL query, e.g. 'summarize count(*) by bin_auto(timestamp)'")
    <*> optional (strOption (long "since" <> metavar "DURATION" <> help "Lookback window (default: 1h)"))
    <*> optional (strOption (long "from" <> metavar "TIMESTAMP"))
    <*> optional (strOption (long "to" <> metavar "TIMESTAMP"))
    <*> optional (strOption (long "source" <> metavar "SOURCE" <> help "spans|metrics (default: spans)"))
    <*> optional chartKindOption
    <*> optional (option auto (long "height" <> metavar "ROWS" <> help "Plot height in rows (default: 14)"))
    <*> optional (strOption (long "watch" <> short 'w' <> metavar "INTERVAL" <> help "Redraw every INTERVAL (e.g. 10s)"))


chartExamples :: [Text]
chartExamples =
  [ "Examples:"
  , "  monoscope chart 'summarize count(*) by bin_auto(timestamp)' --since 6h"
  , "  monoscope chart 'summarize count(*) by bin_auto(timestamp), status_code' --since 1h"
  , "  monoscope chart 'summarize count(*) by resource.service.name' --type bar"
  , "  monoscope chart 'summarize p95(duration) by bin_auto(timestamp)' --since 24h --watch 30s"
  , "  monoscope chart 'summarize avg(value) by bin_auto(timestamp)' --source metrics"
  , ""
  , "Without --type: a query binned by time draws a line chart, anything else bars."
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


sendEventExamples :: [Text]
sendEventExamples =
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


telemetryGenExamples :: [Text]
telemetryGenExamples =
  [ "Examples:"
  , "  monoscope telemetrygen --kind=trace --rate=1"
  , "  monoscope telemetrygen --kind=trace --rate=5 --count=100 --service=my-service"
  , ""
  , "OTLP endpoint is derived from the configured API URL (MONOSCOPE_API_URL)."
  ]


parserInfo :: Version -> ParserInfo (GlobalOpts, Command)
parserInfo v =
  info
    (commandParser <**> versionOpt v <**> helper)
    ( fullDesc
        <> progDesc "Monoscope CLI — terminal access to your observability platform"
        <> header "monoscope — Monoscope CLI"
    )


type CLIEffects = '[Workspace.Workspace, Picker, Ingestion, FileSystem, Environment, HTTP, IOE]


runCLI :: Eff CLIEffects a -> IO a
runCLI = runEff . runHTTPWreq . runEnvironment . runFileSystem . runIngestion . runPicker . Workspace.runWorkspaceReal


cliMain :: Version -> IO ()
cliMain v = do
  hSetBuffering stdout LineBuffering
  -- Help and messages contain `—`/`→`; under a C/POSIX locale GHC would fail encoding them.
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]
  (global, parsedCommand) <- execParser (parserInfo v)
  -- Make --debug equivalent to MONOSCOPE_DEBUG=1 so 'CLI.Core.printDebug'
  -- (which only sees the env) lights up either way. setEnv is process-global
  -- and not thread-safe, but we set it exactly once here, before runCLI
  -- starts any concurrent work, so the race window is empty.
  when global.debugFlag $ setEnv "MONOSCOPE_DEBUG" "1"
  runCLI $ run v global parsedCommand


-- | Resolve config (applying the @--project@ override) + output mode and hand
-- them to a subcommand handler. Factors out the preamble every resource
-- branch repeats.
withCfgMode
  :: (Environment :> es, FileSystem :> es, IOE :> es)
  => GlobalOpts -> (CLIConfig -> OutputMode -> Eff es ()) -> Eff es ()
withCfgMode global k = do
  cfg <- resolveConfig
  mode <- resolveMode global
  k cfg{projectId = global.projectFlag <|> cfg.projectId} mode


run :: (Environment :> es, FileSystem :> es, HTTP :> es, IOE :> es, Ingestion :> es, Picker :> es, Workspace.Workspace :> es) => Version -> GlobalOpts -> Command -> Eff es ()
run version global = \case
  -- C7: resolve the output mode before auth too — 'runAuth' relies on
  -- 'isJsonOutput' to refuse the interactive device flow when piped.
  AuthCmd c -> void (resolveMode global) >> runAuth c
  EventsCmd kindOverride evCmd -> withCfgMode global $ \cfg mode -> case evCmd of
    EvSearch opts ->
      let finalOpts = (opts :: EventsSearchOpts){kind = opts.kind <|> kindOverride}
       in runEventsSearch cfg finalOpts mode
    EvGet opts -> runEventsGet cfg opts mode
    EvTail opts -> runEventsTail cfg opts kindOverride
    EvContext opts -> runEventsContext cfg opts kindOverride mode
  MetricsCmd mCmd -> withCfgMode global $ \cfg mode -> case mCmd of
    MQuery opts -> runMetricsQuery cfg opts mode
    MList opts -> runMetricsList cfg opts mode
    MChart opts -> runMetricsChart cfg opts mode
  ServicesCmd (SList opts) -> withCfgMode global $ \cfg mode -> runServicesList cfg opts mode
  ConfigCmd CInit -> resolveMode global >>= runConfigInit
  ConfigCmd (CSet opts) -> resolveMode global >>= runConfigSet opts
  ConfigCmd (CGet opts) -> do
    mode <- resolveMode global
    runConfigGet opts mode
  ApiCmd call -> withCfgMode global $ \cfg mode -> Resource.runCall cfg mode call
  DashRenderCmd ropts -> withCfgMode global $ \cfg mode -> runDashboardRender cfg ropts mode
  SchemaCmd schemaOpts -> withCfgMode global $ \cfg mode ->
    Resource.runAPI mode (filterSchema schemaOpts <<$>> apiGetJson @_ @Schema.Schema cfg "/api/v1/schema" [])
  FacetsCmd facetsOpts -> withCfgMode global $ \cfg mode -> do
    let params = catMaybes [("since",) <$> facetsOpts.facetSince, ("field",) <$> facetsOpts.facetField]
    Resource.runAPI mode (capFacets facetsOpts.facetTopN <<$>> apiGetJson @_ @AE.Value cfg "/api/v1/facets" params)
  CompletionCmd shell -> emitCompletion version shell
  VersionCmd -> putTextLn $ "monoscope " <> toText (showVersion version)
  TelemetryGenCmd opts -> withCfgMode global $ \cfg _ -> runTelemetryGen cfg opts
  SendEventCmd opts -> withCfgMode global $ \cfg _ -> runSendEvent cfg opts
  ChartCmd opts -> withCfgMode global $ \cfg mode -> runChart cfg opts mode
  StatusCmd opts -> withCfgMode global $ \cfg mode -> runStatus cfg opts mode
  OpenCmd opts -> withCfgMode global $ \cfg _ -> runOpen cfg opts
  GuestSessionCmd request -> liftIO $ Guest.runSession request
  RegressionCmd request options -> Workspace.runRegression options request >>= either (dieWith . Workspace.regressionErrorText) (putLBS . AE.encode)
  WorkspaceMaintainCmd config -> do
    report <- Workspace.maintainWorkspaces config >>= either dieWith pure
    putLBS $ AE.encode report
    unless (Workspace.maintenanceBudgetMet report) $ dieWith "Workspace cache remains over budget or maintenance was incomplete"
  WorkspacePruneCmd request options -> do
    report <- Workspace.pruneWorkspaces options request >>= either dieWith pure
    putLBS $ AE.encode report
    unless (Workspace.pruneBudgetMet report) $ dieWith "Checkpoint cache remains over budget; inspect retained entries and lease bytes"
  WorkspaceLeaseCmd request options -> do
    result <- Workspace.withWorkspace options request $ \prepared -> do
      putLBS $ AE.encode prepared <> "\n"
      liftIO $ hFlush stdout
      liftIO $ unlessM isEOF $ fail "Workspace lease expects EOF after release"
    either dieWith pure result
  WorkspaceCmd request options operation -> do
    result <- Workspace.runWorkspace options operation request
    either dieWith (putLBS . AE.encode) result


-- | Resolve and cache the output mode for this process. The cache (Core.hs
-- 'setOutputMode') lets 'printError' / 'printDebug' suppress ANSI under
-- JSON/YAML without each call site threading a mode argument.
resolveMode :: (Environment :> es, IOE :> es) => GlobalOpts -> Eff es OutputMode
resolveMode global = do
  m <- detectOutputMode global.jsonFlag global.yamlFlag global.tableFlag
  setOutputMode m
  pure m


-- | C3: post-process the @/api/v1/schema@ response so agents can fetch a
-- focused slice. The full payload is huge — without filtering, an LLM agent
-- spends 600+ tokens on field names that don't matter for the current
-- investigation. Operates on the typed 'Schema.Schema' so a server-side
-- field rename surfaces as a compile error, not a silent miss.
filterSchema :: SchemaOpts -> Schema.Schema -> Schema.Schema
filterSchema opts s = Schema.Schema{fields = maybe filtered (`Map.take` filtered) opts.schemaLimit}
  where
    needle = T.toLower <$> opts.search
    filtered = Map.filterWithKey (\k _ -> maybe True (`T.isInfixOf` T.toLower k) needle) s.fields


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
emitCompletion :: IOE :> es => Version -> Text -> Eff es ()
emitCompletion v shell
  | shell `elem` ["bash", "zsh", "fish"] =
      liftIO $ void $ handleParseResult $ execParserPure defaultPrefs (parserInfo v) ["--" <> toString shell <> "-completion-script", "monoscope"]
  | otherwise = liftIO $ do
      Data.Text.IO.hPutStrLn stderr $ "error: unknown shell '" <> shell <> "'; supported: bash|zsh|fish"
      exitFailure
