module CLI.Commands
  ( -- Auth
    runAuth
  , AuthCommand (..)
    -- Config commands
  , runConfigInit
  , runConfigSet
  , runConfigGet
  , ConfigSetOpts (..)
  , ConfigGetOpts (..)
    -- Services
  , runServicesList
  , ServicesListOpts (..)
    -- Events
  , runEventsSearch
  , runEventsGet
  , runEventsTail
  , runEventsContext
  , EventsSearchOpts (..)
  , EventsGetOpts (..)
  , EventsTailOpts (..)
  , EventsContextOpts (..)
    -- Metrics
  , runMetricsQuery
  , runMetricsChart
  , MetricsQueryOpts (..)
  , MetricsChartOpts (..)
  ) where

import Relude

import CLI.Config (CLIConfig (..), ConfigKey (..), allConfigKeys, configDir, configFilePath, configKeyText, parseConfigKey, removeToken, resolveConfig, saveToken, setConfigValue)
import CLI.Core (OutputMode (..), apiGet, apiPost, isInteractiveTTY, printError, renderJSON, renderTable, withAPIResult)
import CLI.UI (inputForm, selectFromList, withSpinner)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.Effectful.Wreq (HTTP, runHTTPWreq)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful
import Effectful.Environment (Environment)
import Effectful.Environment qualified as Env
import Effectful.FileSystem (FileSystem)
import Pkg.CLIFormat (evalCond, extractInt, extractNumericRows, extractRows, extractTextArray, renderSummaryItems, sparklineBar)
import System.Process (spawnProcess)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (catch, tryAny)
import Web.Auth (DeviceCodeResponse (..), DeviceTokenResponse (..), ProjectInfo (..))

-- Auth

data AuthCommand = AuthLogin (Maybe Text) | AuthStatus | AuthLogout
  deriving stock (Show)

runAuth :: (FileSystem :> es, Environment :> es, HTTP :> es, IOE :> es) => AuthCommand -> Eff es ()
runAuth = \case
  AuthLogin (Just token) -> do
    saveToken token
    dir <- configDir
    putTextLn $ "Token saved to " <> toText dir <> "/tokens.json"
  AuthLogin Nothing -> do
    cfg <- resolveConfig
    let baseUrl = cfg.apiUrl
    result <- runExceptT $ do
      bs <- ExceptT $ first show <$> apiPost baseUrl "/api/device/code" []
      resp <- ExceptT . pure $ first toText $ AE.eitherDecode @DeviceCodeResponse bs
      putTextLn $ "\nYour authorization code: " <> resp.userCode
      putTextLn $ "Opening browser to: " <> resp.verificationUri
      liftIO $ tryOpenBrowser (toString resp.verificationUri)
      tty <- lift isInteractiveTTY
      tokenResp <- ExceptT $ liftIO $ withSpinner tty "Waiting for authorization..." $
        runEff . runHTTPWreq $ maybe (Left "Authorization timed out (5 minutes)") Right <$> pollForToken baseUrl resp.deviceCode 60
      liftIO $ putStr ("\r\ESC[K" :: String) >> hFlush stdout
      sessId <- ExceptT . pure $ maybeToRight "No session received" tokenResp.sessionId
      lift $ saveToken sessId
      putTextLn "Authenticated successfully!"
      lift $ selectProject tokenResp.projects
    whenLeft_ result \e -> printError e >> liftIO exitFailure
  AuthStatus -> do
    cfg <- resolveConfig
    envKey <- Env.lookupEnv "MONO_API_KEY"
    case (envKey, cfg.apiKey) of
      (Just _, _) -> do
        putTextLn "Authenticated via MONO_API_KEY environment variable"
        putTextLn $ "API URL: " <> cfg.apiUrl
        whenJust cfg.projectId $ \p -> putTextLn $ "Project: " <> p
      (_, Just _) -> do
        putTextLn "Authenticated via stored token"
        putTextLn $ "API URL: " <> cfg.apiUrl
        whenJust cfg.projectId $ \p -> putTextLn $ "Project: " <> p
      _ -> printError "Not authenticated. Run: mono auth login --token <token>"
  AuthLogout -> do
    removeToken
    putTextLn "Logged out"

-- Config commands

data ConfigSetOpts = ConfigSetOpts {key :: Text, value :: Text}
  deriving stock (Show)

data ConfigGetOpts = ConfigGetOpts {key :: Maybe Text}
  deriving stock (Show)

runConfigInit :: (FileSystem :> es, Environment :> es, IOE :> es) => Eff es ()
runConfigInit = do
  tty <- isInteractiveTTY
  result <- liftIO $
    inputForm
      tty
      "Monoscope Configuration"
      [ ("api_url", "API URL", "https://app.monoscope.tech")
      , ("token", "API token", "")
      , ("project", "Default project ID", "")
      ]
  let field k = fromMaybe "" $ Map.lookup k result
      url = let v = field "api_url" in if T.null v then "https://app.monoscope.tech" else v
  setConfigValue CKApiUrl url
  let token = field "token"
  unless (T.null token) $ saveToken token
  let proj = field "project"
  unless (T.null proj) $ setConfigValue CKProject proj
  f <- configFilePath
  putTextLn $ "Configuration saved to " <> toText f

runConfigSet :: (FileSystem :> es, IOE :> es) => ConfigSetOpts -> Eff es ()
runConfigSet opts = case parseConfigKey opts.key of
  Just ck -> do
    setConfigValue ck opts.value
    putTextLn $ opts.key <> " = " <> opts.value
  Nothing -> do
    putTextLn $ "Unknown key: " <> opts.key
    putTextLn $ "Valid keys: " <> show (map configKeyText allConfigKeys)
    liftIO exitFailure

runConfigGet :: (FileSystem :> es, Environment :> es, IOE :> es) => ConfigGetOpts -> Eff es ()
runConfigGet opts = do
  cfg <- resolveConfig
  case opts.key of
    Nothing -> do
      putTextLn $ "api_url = " <> cfg.apiUrl
      putTextLn $ "project = " <> fromMaybe "(not set)" cfg.projectId
      putTextLn $ "api_key = " <> maybe "(not set)" (const "********") cfg.apiKey
    Just "api_url" -> putTextLn cfg.apiUrl
    Just "project" -> putTextLn $ fromMaybe "(not set)" cfg.projectId
    Just "api_key" -> putTextLn $ maybe "(not set)" (const "********") cfg.apiKey
    Just k -> putTextLn ("Unknown key: " <> k) >> liftIO exitFailure

-- Services

data ServicesListOpts = ServicesListOpts
  { since :: Maybe Text
  }
  deriving stock (Show)

runServicesList :: (HTTP :> es, IOE :> es) => CLIConfig -> ServicesListOpts -> OutputMode -> Eff es ()
runServicesList cfg opts mode = do
  let params =
        catMaybes
          [ ("since",) <$> (opts.since <|> Just "24h")
          , ("pid",) <$> cfg.projectId
          ]
  withAPIResult cfg "/log_explorer/schema" params $ \val -> case mode of
    OutputJSON -> renderJSON val
    OutputTable -> renderServicesList val

renderServicesList :: (IOE :> es) => AE.Value -> Eff es ()
renderServicesList val = case extractServices val of
  [] -> putTextLn "No services found"
  services -> renderTable ["Service"] (map (: []) services)

extractServices :: AE.Value -> [Text]
extractServices (AE.Object obj) =
  case extractTextArray (KM.lookup "service_name" obj <|> KM.lookup "services" obj) of
    [] -> case KM.lookup "fields" obj of
      Just (AE.Object fields) -> extractTextArray (KM.lookup "service_name" fields <|> KM.lookup "service" fields)
      _ -> []
    xs -> xs
extractServices _ = []

-- Events

data EventsSearchOpts = EventsSearchOpts
  { query :: Text
  , since :: Maybe Text
  , from :: Maybe Text
  , to :: Maybe Text
  , kind :: Maybe Text
  , service :: Maybe Text
  , level :: Maybe Text
  , limit :: Maybe Int
  , fields :: Maybe Text
  }
  deriving stock (Show)

data EventsGetOpts = EventsGetOpts
  { eventId :: Text
  , showTree :: Bool
  }
  deriving stock (Show)

data EventsTailOpts = EventsTailOpts
  { kind :: Maybe Text
  , service :: Maybe Text
  , level :: Maybe Text
  , grep :: Maybe Text
  }
  deriving stock (Show)

data EventsContextOpts = EventsContextOpts
  { timestamp :: Text
  , service :: Maybe Text
  , kind :: Maybe Text
  , window :: Maybe Text
  }
  deriving stock (Show)

runEventsSearch :: (HTTP :> es, IOE :> es) => CLIConfig -> EventsSearchOpts -> OutputMode -> Eff es ()
runEventsSearch cfg opts mode = do
  let params = buildSearchParams opts
  withAPIResult cfg "/log_explorer" params $ \val -> case mode of
    OutputJSON -> renderJSON val
    OutputTable -> renderEventsTable val opts.fields

runEventsGet :: (HTTP :> es, IOE :> es) => CLIConfig -> EventsGetOpts -> OutputMode -> Eff es ()
runEventsGet cfg opts mode = do
  let params = [("query", "trace_id:" <> opts.eventId), ("json", "true")]
  withAPIResult cfg "/log_explorer" params $ \val ->
    if opts.showTree
      then renderTraceTree val
      else case mode of
        OutputJSON -> renderJSON val
        OutputTable -> renderEventsTable val Nothing

runEventsTail :: (HTTP :> es, IOE :> es) => CLIConfig -> EventsTailOpts -> Maybe Text -> Eff es ()
runEventsTail cfg opts kindOverride = do
  seenRef <- newIORef (Set.empty :: Set Text)
  forever $ do
    let params =
          catMaybes
            [ Just ("query", "")
            , Just ("since", "10s")
            , ("source",) <$> (opts.kind <|> kindOverride)
            , ("service",) <$> opts.service
            , ("level",) <$> opts.level
            , Just ("json", "true")
            ]
    apiGet cfg "/log_explorer" params >>= \case
      Left err -> printError (show err)
      Right bs -> case AE.eitherDecode @AE.Value bs of
        Left err -> printError (toText err)
        Right val -> do
          seen <- readIORef seenRef
          let rows = extractRowsWithId val
              newRows = filter (\(rid, _) -> not $ Set.member rid seen) rows
              newIds = Set.fromList (map fst newRows)
              -- Cap seen set: keep most recent half when exceeding limit
              updated = let s = seen <> newIds in if Set.size s > 10000 then Set.drop (Set.size s `div` 2) s else s
          writeIORef seenRef updated
          forM_ newRows $ \(_, row) -> do
            let filtered = case opts.grep of
                  Nothing -> True
                  Just pat -> pat `T.isInfixOf` T.intercalate " " row
            when filtered $ putTextLn $ T.intercalate "  " row
    threadDelay 2_000_000

runEventsContext :: (HTTP :> es, IOE :> es) => CLIConfig -> EventsContextOpts -> Maybe Text -> OutputMode -> Eff es ()
runEventsContext cfg opts kindOverride mode = do
  let win = fromMaybe "5m" opts.window
      params =
        catMaybes
          [ Just ("query", "")
          , Just ("from", opts.timestamp)
          , Just ("window", win)
          , ("source",) <$> (opts.kind <|> kindOverride)
          , ("service",) <$> opts.service
          , Just ("json", "true")
          ]
  withAPIResult cfg "/log_explorer" params $ \val -> case mode of
    OutputJSON -> renderJSON val
    OutputTable -> renderEventsTable val Nothing

buildSearchParams :: EventsSearchOpts -> [(Text, Text)]
buildSearchParams opts =
  catMaybes
    [ Just ("query", opts.query)
    , ("since",) <$> opts.since
    , ("from",) <$> opts.from
    , ("to",) <$> opts.to
    , ("source",) <$> opts.kind
    , ("service",) <$> opts.service
    , ("level",) <$> opts.level
    , ("limit",) . show <$> opts.limit
    , Just ("json", "true")
    ]

renderEventsTable :: (IOE :> es) => AE.Value -> Maybe Text -> Eff es ()
renderEventsTable val mFields = case val of
  AE.Object obj -> do
    let cols = extractTextArray $ KM.lookup "cols" obj
        idxMap = extractColIdxMap $ KM.lookup "colIdxMap" obj
        rows = extractRows $ KM.lookup "logsData" obj
        count = extractInt $ KM.lookup "count" obj
        filteredCols = maybe cols (\f -> filter (`elem` T.splitOn "," f) cols) mFields
        colIdxs = mapMaybe (\c -> Map.lookup c idxMap) filteredCols
        summaryCols = Set.fromList [i | (c, i) <- zip filteredCols [0 :: Int ..], c `elem` ["summary", "latency_breakdown"]]
        filteredRows = map (\r ->
          let raw = map (\i -> fromMaybe "" $ listToMaybe (drop i r)) colIdxs
          in  [if Set.member ci summaryCols then renderSummaryCell cell else cell | (ci, cell) <- zip [0..] raw]
          ) rows
    renderTable filteredCols filteredRows
    putTextLn $ "\n" <> show count <> " results"
  _ -> renderJSON val

renderSummaryCell :: Text -> Text
renderSummaryCell cell = case AE.eitherDecode @[Text] (encodeUtf8 cell) of
  Right items -> renderSummaryItems items
  Left _ -> cell

extractColIdxMap :: Maybe AE.Value -> Map Text Int
extractColIdxMap = \case
  Just (AE.Object obj) -> Map.fromList [(AK.toText k, round n) | (k, AE.Number n) <- KM.toList obj]
  _ -> mempty

renderTraceTree :: (IOE :> es) => AE.Value -> Eff es ()
renderTraceTree val = case val of
  AE.Object obj -> case KM.lookup "traces" obj of
    Just traces -> renderJSON traces
    Nothing -> renderJSON val
  _ -> renderJSON val

extractRowsWithId :: AE.Value -> [(Text, [Text])]
extractRowsWithId val = case val of
  AE.Object obj ->
    let rows = extractRows $ KM.lookup "logsData" obj
     in map (\r -> (fromMaybe "" $ listToMaybe r, r)) rows
  _ -> []

-- Metrics

data MetricsQueryOpts = MetricsQueryOpts
  { expression :: Text
  , since :: Maybe Text
  , from :: Maybe Text
  , to :: Maybe Text
  , step :: Maybe Text
  , assert :: Maybe Text
  }
  deriving stock (Show)

data MetricsChartOpts = MetricsChartOpts
  { expression :: Text
  , since :: Maybe Text
  , from :: Maybe Text
  , to :: Maybe Text
  , watch :: Maybe Text
  }
  deriving stock (Show)

runMetricsQuery :: (HTTP :> es, IOE :> es) => CLIConfig -> MetricsQueryOpts -> OutputMode -> Eff es ()
runMetricsQuery cfg opts mode = do
  let params = metricsParams opts.expression opts.since opts.from opts.to opts.step cfg
  withAPIResult cfg "/chart_data" params $ \val -> do
    case mode of
      OutputJSON -> renderJSON val
      OutputTable -> renderMetricsTable val
    whenJust opts.assert $ checkAssertion val

runMetricsChart :: (HTTP :> es, IOE :> es) => CLIConfig -> MetricsChartOpts -> Eff es ()
runMetricsChart cfg opts = do
  let run = do
        let params = metricsParams opts.expression opts.since opts.from opts.to Nothing cfg
        withAPIResult cfg "/chart_data" params renderSparkline
  case opts.watch of
    Nothing -> run
    Just interval -> forever $ do
      liftIO $ putStr ("\ESC[2J\ESC[H" :: String)
      run
      threadDelay (parseDurationMs interval * 1000)

metricsParams :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> CLIConfig -> [(Text, Text)]
metricsParams expr mSince mFrom mTo mStep cfg =
  catMaybes
    [ Just ("query", expr)
    , ("since",) <$> (mSince <|> Just "1h")
    , ("from",) <$> mFrom
    , ("to",) <$> mTo
    , ("step",) <$> mStep
    , ("pid",) <$> cfg.projectId
    ]

renderMetricsTable :: (IOE :> es) => AE.Value -> Eff es ()
renderMetricsTable val = case val of
  AE.Object obj -> do
    let headers = extractTextArray $ KM.lookup "headers" obj
        rows = extractRows $ KM.lookup "dataText" obj
    if null headers
      then renderJSON val
      else renderTable headers rows
  _ -> renderJSON val

renderSparkline :: (IOE :> es) => AE.Value -> Eff es ()
renderSparkline val = case val of
  AE.Object obj -> do
    let headers = extractTextArray $ KM.lookup "headers" obj
        dataset = extractNumericRows $ KM.lookup "dataset" obj
    unless (null headers) $ putTextLn $ T.intercalate " | " headers
    forM_ dataset $ \row ->
      putTextLn $ T.concat $ map sparklineBar row
  _ -> renderJSON val

checkAssertion :: (IOE :> es) => AE.Value -> Text -> Eff es ()
checkAssertion val cond = case val of
  AE.Object obj -> case KM.lookup "dataFloat" obj of
    Just (AE.Number n) -> do
      let v = realToFrac n :: Double
      unless (evalCond v cond) $ do
        printError $ "Assertion failed: " <> show v <> " " <> cond
        liftIO exitFailure
    _ -> printError "Warning: --assert ignored, no numeric result to evaluate"
  _ -> printError "Warning: --assert ignored, unexpected response shape"

parseDurationMs :: Text -> Int
parseDurationMs t
  | Just n <- T.stripSuffix "ms" t = fromMaybe 5000 (readMaybe $ toString n)
  | Just n <- T.stripSuffix "s" t  = maybe 5000 (* 1000) (readMaybe $ toString n)
  | Just n <- T.stripSuffix "m" t  = maybe 5000 (* 60_000) (readMaybe $ toString n)
  | Just n <- T.stripSuffix "h" t  = maybe 5000 (* 3_600_000) (readMaybe $ toString n)
  | otherwise = maybe 5000 (* 1000) (readMaybe $ toString t)

-- Device auth helpers

selectProject :: (FileSystem :> es, Environment :> es, IOE :> es) => Maybe [ProjectInfo] -> Eff es ()
selectProject = \case
  Just [p] -> setConfigValue CKProject p.id >> putTextLn ("Using project: " <> p.name)
  Just ps@(_ : _ : _) -> do
    tty <- isInteractiveTTY
    let items = [(p.id, p.name <> " (" <> p.id <> ")") | p <- ps]
    liftIO (selectFromList tty "Select project" items) >>= \case
      Just pid -> do
        let name = maybe pid (.name) $ find (\p -> p.id == pid) ps
        setConfigValue CKProject pid >> putTextLn ("Using project: " <> name)
      Nothing -> putTextLn "No project selected"
  _ -> putTextLn "No projects found. Create one at your Monoscope dashboard."

pollForToken :: (HTTP :> es, IOE :> es) => Text -> Text -> Int -> Eff es (Maybe DeviceTokenResponse)
pollForToken _ _ 0 = pure Nothing
pollForToken baseUrl deviceCode remaining = do
  threadDelay 5_000_000
  apiPost baseUrl "/api/device/token" [("device_code", deviceCode)] >>= \case
    Left _ -> pollForToken baseUrl deviceCode (remaining - 1) -- transient error, keep polling
    Right bs -> case AE.eitherDecode @DeviceTokenResponse bs of
      Left _ -> pollForToken baseUrl deviceCode (remaining - 1)
      Right resp
        | resp.err == Just "authorization_pending" -> pollForToken baseUrl deviceCode (remaining - 1)
        | isJust resp.sessionId -> pure (Just resp)
        | otherwise -> pure Nothing

tryOpenBrowser :: String -> IO ()
tryOpenBrowser url = void $ tryAny $
  void (spawnProcess "open" [url]) `catch` \(_ :: SomeException) ->
    void $ spawnProcess "xdg-open" [url]
