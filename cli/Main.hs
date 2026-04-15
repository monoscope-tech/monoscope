module Main (main) where

import Relude

import CLI.Commands hiding (value)
import CLI.Config (CLIConfig (..), resolveConfig)
import CLI.Core (OutputMode (..), apiGetJson, apiPostJson, detectOutputMode, printError, renderByMode)
import CLI.Resource qualified as Resource
import Data.Aeson qualified as AE
import Data.Effectful.Wreq (HTTP, runHTTPWreq)
import Data.Version (showVersion)
import Effectful
import Effectful.Environment (Environment, runEnvironment)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Options.Applicative
import Paths_monoscope qualified as Paths


data GlobalOpts = GlobalOpts
  { jsonFlag :: Bool
  , agentFlag :: Bool
  , outputFlag :: Maybe Text
  , projectFlag :: Maybe Text
  , showVersionFlag :: Bool
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
  | SchemaCmd
  | CompletionCmd Text
  | VersionCmd


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


data ServicesCommand = SList ServicesListOpts
  deriving stock (Show)


data ConfigCommand = CInit | CSet ConfigSetOpts | CGet ConfigGetOpts
  deriving stock (Show)


-- | Plan A resource commands. 'id' is a UUID text.
data MonitorsCommand
  = MonList
  | MonGet Text
  | MonDelete Text
  | MonMute Text (Maybe Int)
  | MonUnmute Text
  | MonResolve Text
  | MonToggle Text
  | MonApply FilePath
  deriving stock (Show)


data DashboardsCommand
  = DashList
  | DashGet Text
  | DashDelete Text
  | DashStar Text
  | DashDuplicate Text
  | DashYaml Text
  | DashApply FilePath
  deriving stock (Show)


data ApiKeysCommand
  = KeyList
  | KeyGet Text
  | KeyCreate Text -- title
  | KeyActivate Text
  | KeyRevoke Text
  | KeyDelete Text
  deriving stock (Show)


globalOpts :: Parser GlobalOpts
globalOpts =
  GlobalOpts
    <$> switch (long "json" <> help "Force JSON output")
    <*> switch (long "agent" <> help "Force agent mode")
    <*> optional (strOption (long "output" <> short 'o' <> metavar "FORMAT" <> help "Output format: json|yaml|table"))
    <*> optional (strOption (long "project" <> short 'p' <> metavar "PID" <> help "Project UUID override"))
    <*> switch (long "version" <> help "Print CLI version and exit")


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
          , command "schema" (info (pure SchemaCmd) (progDesc "Fetch telemetry schema"))
          , command "completion" (info (CompletionCmd <$> strArgument (metavar "SHELL" <> help "bash|zsh|fish")) (progDesc "Emit shell completion script"))
          , command "version" (info (pure VersionCmd) (progDesc "Show CLI version"))
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
      [ command "search" (info (EvSearch <$> eventsSearchParser <**> helper) (progDesc "Search events"))
      , command "get" (info (EvGet <$> eventsGetParser <**> helper) (progDesc "Get event by ID"))
      , command "tail" (info (EvTail <$> eventsTailParser <**> helper) (progDesc "Stream events"))
      , command "context" (info (EvContext <$> eventsContextParser <**> helper) (progDesc "Context around timestamp"))
      ]


eventsSearchParser :: Parser EventsSearchOpts
eventsSearchParser =
  EventsSearchOpts
    <$> strArgument (metavar "QUERY" <> value "" <> help "Search query")
    <*> optional (strOption (long "since" <> metavar "DURATION" <> help "Relative time (e.g. 1h, 30m)"))
    <*> optional (strOption (long "from" <> metavar "TIMESTAMP" <> help "Start time (ISO 8601)"))
    <*> optional (strOption (long "to" <> metavar "TIMESTAMP" <> help "End time (ISO 8601)"))
    <*> optional (strOption (long "kind" <> metavar "KIND" <> help "log or trace"))
    <*> optional (strOption (long "service" <> metavar "SERVICE"))
    <*> optional (strOption (long "level" <> metavar "LEVEL"))
    <*> optional (option auto (long "limit" <> short 'n' <> metavar "N" <> help "Max results"))
    <*> optional (strOption (long "fields" <> metavar "FIELDS" <> help "Comma-separated fields"))


eventsGetParser :: Parser EventsGetOpts
eventsGetParser =
  EventsGetOpts
    <$> strArgument (metavar "ID" <> help "Event or trace ID")
    <*> switch (long "tree" <> help "Show trace tree view")


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


-- Plan A resource parsers

idArg :: Parser Text
idArg = strArgument (metavar "ID" <> help "Resource ID (UUID)")


monitorsParser :: Parser MonitorsCommand
monitorsParser =
  subparser $
    mconcat
      [ command "list" (info (pure MonList) (progDesc "List monitors"))
      , command "get" (info (MonGet <$> idArg <**> helper) (progDesc "Get a single monitor"))
      , command "delete" (info (MonDelete <$> idArg <**> helper) (progDesc "Delete a monitor"))
      , command "mute" (info (MonMute <$> idArg <*> optional (option auto (long "for" <> metavar "MINUTES")) <**> helper) (progDesc "Mute a monitor"))
      , command "unmute" (info (MonUnmute <$> idArg <**> helper) (progDesc "Unmute a monitor"))
      , command "resolve" (info (MonResolve <$> idArg <**> helper) (progDesc "Resolve a monitor"))
      , command "toggle-active" (info (MonToggle <$> idArg <**> helper) (progDesc "Toggle active state"))
      , command "apply" (info (MonApply <$> strArgument (metavar "FILE" <> help "Monitor YAML") <**> helper) (progDesc "Apply a monitor YAML file"))
      ]


dashboardsParser :: Parser DashboardsCommand
dashboardsParser =
  subparser $
    mconcat
      [ command "list" (info (pure DashList) (progDesc "List dashboards"))
      , command "get" (info (DashGet <$> idArg <**> helper) (progDesc "Get a single dashboard"))
      , command "delete" (info (DashDelete <$> idArg <**> helper) (progDesc "Delete a dashboard"))
      , command "star" (info (DashStar <$> idArg <**> helper) (progDesc "Toggle star"))
      , command "duplicate" (info (DashDuplicate <$> idArg <**> helper) (progDesc "Duplicate dashboard"))
      , command "yaml" (info (DashYaml <$> idArg <**> helper) (progDesc "Dump dashboard as YAML"))
      , command "apply" (info (DashApply <$> strArgument (metavar "FILE" <> help "Dashboard YAML") <**> helper) (progDesc "Apply a dashboard YAML file"))
      ]


apiKeysParser :: Parser ApiKeysCommand
apiKeysParser =
  subparser $
    mconcat
      [ command "list" (info (pure KeyList) (progDesc "List API keys"))
      , command "get" (info (KeyGet <$> idArg <**> helper) (progDesc "Get API key"))
      , command "create" (info (KeyCreate <$> strArgument (metavar "TITLE") <**> helper) (progDesc "Create API key; prints plaintext once"))
      , command "activate" (info (KeyActivate <$> idArg <**> helper) (progDesc "Activate an API key"))
      , command "revoke" (info (KeyRevoke <$> idArg <**> helper) (progDesc "Revoke an API key"))
      , command "delete" (info (KeyDelete <$> idArg <**> helper) (progDesc "Delete an API key"))
      ]


parserInfo :: ParserInfo (GlobalOpts, Command)
parserInfo =
  info
    (commandParser <**> helper)
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
  if global.showVersionFlag
    then putTextLn $ "monoscope " <> toText (showVersion Paths.version)
    else runCLI $ run global cmd


-- | Apply --project override to the resolved config.
resolveCfgWith :: (FileSystem :> es, Environment :> es, IOE :> es) => GlobalOpts -> Eff es CLIConfig
resolveCfgWith g = do
  c <- resolveConfig
  pure $ case g.projectFlag of
    Nothing -> c
    Just pid -> c{projectId = Just pid}


run :: (FileSystem :> es, Environment :> es, HTTP :> es, IOE :> es) => GlobalOpts -> Command -> Eff es ()
run global = \case
  AuthCmd c -> runAuth c
  EventsCmd kindOverride evCmd -> do
    cfg <- resolveCfgWith global
    mode <- resolveMode global
    case evCmd of
      EvSearch opts ->
        let finalOpts = (opts :: EventsSearchOpts){kind = opts.kind <|> kindOverride}
         in runEventsSearch cfg finalOpts mode
      EvGet opts -> runEventsGet cfg opts mode
      EvTail opts -> runEventsTail cfg opts kindOverride
      EvContext opts -> runEventsContext cfg opts kindOverride mode
  MetricsCmd mCmd -> do
    cfg <- resolveCfgWith global
    mode <- resolveMode global
    case mCmd of
      MQuery opts -> runMetricsQuery cfg opts mode
      MChart opts -> runMetricsChart cfg opts
  ServicesCmd (SList opts) -> do
    cfg <- resolveCfgWith global
    mode <- resolveMode global
    runServicesList cfg opts mode
  ConfigCmd CInit -> runConfigInit
  ConfigCmd (CSet opts) -> runConfigSet opts
  ConfigCmd (CGet opts) -> runConfigGet opts
  MonitorsCmd sub -> do
    cfg <- resolveCfgWith global
    mode <- resolveMode global
    case sub of
      MonList -> Resource.runList cfg Resource.Monitors [] mode
      MonGet i -> Resource.runGet cfg Resource.Monitors i mode
      MonDelete i -> Resource.runDelete cfg Resource.Monitors i
      MonMute i minsM -> Resource.runLifecycle cfg Resource.Monitors i "mute" (maybe [] (\m -> [("duration_minutes", show m)]) minsM) mode
      MonUnmute i -> Resource.runLifecycle cfg Resource.Monitors i "unmute" [] mode
      MonResolve i -> Resource.runLifecycle cfg Resource.Monitors i "resolve" [] mode
      MonToggle i -> Resource.runLifecycle cfg Resource.Monitors i "toggle_active" [] mode
      MonApply path -> Resource.runApply cfg Resource.Monitors path mode
  DashboardsCmd sub -> do
    cfg <- resolveCfgWith global
    mode <- resolveMode global
    case sub of
      DashList -> Resource.runList cfg Resource.Dashboards [] mode
      DashGet i -> Resource.runGet cfg Resource.Dashboards i mode
      DashDelete i -> Resource.runDelete cfg Resource.Dashboards i
      DashStar i -> Resource.runLifecycle cfg Resource.Dashboards i "star" [] mode
      DashDuplicate i -> Resource.runLifecycle cfg Resource.Dashboards i "duplicate" [] mode
      DashYaml i -> Resource.runYamlDump cfg Resource.Dashboards i
      DashApply path -> Resource.runApply cfg Resource.Dashboards path mode
  ApiKeysCmd sub -> do
    cfg <- resolveCfgWith global
    mode <- resolveMode global
    case sub of
      KeyList -> Resource.runList cfg Resource.ApiKeys [] mode
      KeyGet i -> Resource.runGet cfg Resource.ApiKeys i mode
      KeyCreate title ->
        apiPostJson @_ @_ @AE.Value cfg "/api_keys" (AE.object ["title" AE..= title]) >>= \case
          Left err -> printError (show err) >> liftIO exitFailure
          Right v -> renderByMode mode Nothing v
      KeyActivate i -> Resource.runLifecycle cfg Resource.ApiKeys i "activate" [] mode
      KeyRevoke i -> Resource.runLifecycle cfg Resource.ApiKeys i "deactivate" [] mode
      KeyDelete i -> Resource.runDelete cfg Resource.ApiKeys i
  SchemaCmd -> do
    cfg <- resolveCfgWith global
    mode <- resolveMode global
    apiGetJson @_ @AE.Value cfg "/schema" [] >>= \case
      Left err -> printError (show err) >> liftIO exitFailure
      Right v -> renderByMode mode Nothing v
  CompletionCmd shell -> emitCompletion shell
  VersionCmd -> putTextLn $ "monoscope " <> toText (showVersion Paths.version)


resolveMode :: (Environment :> es, IOE :> es) => GlobalOpts -> Eff es OutputMode
resolveMode global
  | global.agentFlag = pure OutputJSON
  | otherwise = detectOutputMode global.jsonFlag global.outputFlag


-- | Emit a shell completion script via optparse-applicative's built-in support.
emitCompletion :: IOE :> es => Text -> Eff es ()
emitCompletion shell = liftIO $ do
  let shellArg = case shell of
        "bash" -> "--bash-completion-script"
        "zsh" -> "--zsh-completion-script"
        "fish" -> "--fish-completion-script"
        _ -> "--bash-completion-script"
  -- Invoke ourselves with the completion flag so optparse emits the script.
  void $ handleParseResult $ execParserPure defaultPrefs parserInfo [shellArg, "monoscope"]
