module Main (main) where

import Relude

import CLI.Commands hiding (value)
import CLI.Config (resolveConfig)
import CLI.Core (OutputMode (..), detectOutputMode)
import Data.Effectful.Wreq (HTTP, runHTTPWreq)
import Effectful
import Effectful.Environment (Environment, runEnvironment)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Options.Applicative

data GlobalOpts = GlobalOpts
  { jsonFlag :: Bool
  , agentFlag :: Bool
  }

data Command
  = AuthCmd AuthCommand
  | EventsCmd (Maybe Text) EventsCommand
  | MetricsCmd MetricsCommand
  | ServicesCmd ServicesCommand
  | ConfigCmd ConfigCommand

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

globalOpts :: Parser GlobalOpts
globalOpts =
  GlobalOpts
    <$> switch (long "json" <> help "Force JSON output")
    <*> switch (long "agent" <> help "Force agent mode")

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
    <*> optional (strOption (long "step" <> metavar "DURATION" <> help "Resolution step"))
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
  runCLI $ run global cmd

run :: (FileSystem :> es, Environment :> es, HTTP :> es, IOE :> es) => GlobalOpts -> Command -> Eff es ()
run global = \case
  AuthCmd c -> runAuth c
  EventsCmd kindOverride evCmd -> do
    cfg <- resolveConfig
    mode <- resolveMode global
    case evCmd of
      EvSearch opts ->
        let finalOpts = (opts :: EventsSearchOpts){kind = opts.kind <|> kindOverride}
         in runEventsSearch cfg finalOpts mode
      EvGet opts -> runEventsGet cfg opts mode
      EvTail opts -> runEventsTail cfg opts kindOverride
      EvContext opts -> runEventsContext cfg opts kindOverride mode
  MetricsCmd mCmd -> do
    cfg <- resolveConfig
    mode <- resolveMode global
    case mCmd of
      MQuery opts -> runMetricsQuery cfg opts mode
      MChart opts -> runMetricsChart cfg opts
  ServicesCmd (SList opts) -> do
    cfg <- resolveConfig
    mode <- resolveMode global
    runServicesList cfg opts mode
  ConfigCmd CInit -> runConfigInit
  ConfigCmd (CSet opts) -> runConfigSet opts
  ConfigCmd (CGet opts) -> runConfigGet opts

resolveMode :: (Environment :> es, IOE :> es) => GlobalOpts -> Eff es OutputMode
resolveMode global
  | global.agentFlag = pure OutputJSON
  | otherwise = detectOutputMode global.jsonFlag
