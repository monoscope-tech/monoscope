module CLI.Commands (
  -- Auth
  runAuth,
  AuthCommand (..),
  -- Config commands
  runConfigInit,
  runConfigSet,
  runConfigGet,
  ConfigSetOpts (..),
  ConfigGetOpts (..),
  -- Services
  runServicesList,
  ServicesListOpts (..),
  -- Events
  runEventsSearch,
  runEventsGet,
  runEventsTail,
  runEventsContext,
  EventsSearchOpts (..),
  EventsGetOpts (..),
  EventsTailOpts (..),
  EventsContextOpts (..),
  -- Metrics
  runMetricsQuery,
  runMetricsChart,
  MetricsQueryOpts (..),
  MetricsChartOpts (..),
) where

import Relude

import CLI.Config (CLIConfig (..), ConfigKey (..), allConfigKeys, configDir, configFilePath, configKeyText, parseConfigKey, removeToken, resolveConfig, saveToken, setConfigValue)
import CLI.Core (OutputMode (..), apiGet, apiPostUnauth, isAgentMode, isInteractiveTTY, printDebug, printError, renderJSON, renderTable, renderWith, withAPIResult)
import CLI.UI (inputForm, selectFromList, withSpinner)
import CLI.Validate (validateAndNormalizeKind, validateDurationOrDie, validateQueryOrDie)
import Control.Lens ((%~))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens qualified as AL
import Data.Effectful.Wreq (HTTP, runHTTPWreq)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Environment (Environment)
import Effectful.Environment qualified as Env
import Effectful.FileSystem (FileSystem)
import Models.Apis.Fields qualified as Fields
import Pages.Charts.Types (MetricsData (..))
import Pkg.CLIFormat (evalCond, extractInt, extractRows, extractTextArray, renderSummaryItems, sparklineBar)
import System.Process (spawnProcess)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (catch, tryAny)
import Web.Auth (DeviceCodeResponse (..), DeviceTokenResponse (..), ProjectInfo (..))


-- Auth

data AuthCommand = AuthLogin (Maybe Text) | AuthStatus | AuthLogout
  deriving stock (Show)


-- | Stable JSON shape for @--agent auth status@. Field renames go via
-- 'CamelToSnake' so we don't write a manual instance — adding a field is
-- one line and never drifts between Haskell and the wire.
data AuthStatusJson = AuthStatusJson
  { authenticated :: Bool
  , method :: Maybe Text
  , apiUrl :: Text
  , project :: Maybe Text
  }
  deriving stock (Generic)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] AuthStatusJson


runAuth :: (Environment :> es, FileSystem :> es, HTTP :> es, IOE :> es) => AuthCommand -> Eff es ()
runAuth = \case
  AuthLogin (Just token) -> do
    saveToken token
    dir <- configDir
    putTextLn $ "Token saved to " <> toText dir <> "/tokens.json"
  AuthLogin Nothing -> do
    -- C7: Agent mode is non-interactive by design — refuse the device-code
    -- flow (5-min poll, browser launch) and direct the caller to --token.
    -- Otherwise an LLM agent will hang the session waiting for human input.
    whenM isAgentMode $ do
      printError "agent mode (MONOSCOPE_AGENT_MODE/CI/CLAUDE_CODE) requires --token; interactive login is disabled"
      liftIO exitFailure
    cfg <- resolveConfig
    let baseUrl = cfg.apiUrl
    result <- runExceptT $ do
      bs <- ExceptT $ first show <$> apiPostUnauth baseUrl "/api/device/code" []
      resp <- hoistEither $ first toText $ AE.eitherDecode @DeviceCodeResponse bs
      putTextLn $ "\nYour authorization code: " <> resp.userCode
      putTextLn $ "Opening browser to: " <> resp.verificationUri
      liftIO $ tryOpenBrowser (toString resp.verificationUri)
      tty <- lift isInteractiveTTY
      tokenResp <-
        ExceptT
          $ liftIO
          $ withSpinner tty "Waiting for authorization..."
          $ runEff
          . runHTTPWreq
          $ maybeToRight "Authorization timed out (5 minutes)"
          <$> pollForToken baseUrl resp.deviceCode 60
      liftIO $ putStr ("\r\ESC[K" :: String) >> hFlush stdout
      sessId <- hoistEither $ maybeToRight "No session received" tokenResp.sessionId
      lift $ saveToken sessId
      putTextLn "Authenticated successfully!"
      lift $ selectProject tokenResp.projects
    whenLeft_ result \e -> printError e >> liftIO exitFailure
  AuthStatus -> do
    cfg <- resolveConfig
    envKey <- Env.lookupEnv "MONOSCOPE_API_KEY"
    -- C8: agent mode emits a stable JSON shape so a script doesn't have to
    -- regex stdout. The shape is intentionally narrow — adding fields is
    -- safe; renaming or removing them is a breaking change.
    agent <- isAgentMode
    let method = case (envKey, cfg.apiKey) of
          (Just _, _) -> Just ("env" :: Text)
          (_, Just _) -> Just "token"
          _ -> Nothing
    if agent
      then
        renderJSON
          AuthStatusJson
            { authenticated = isJust method
            , method = method
            , apiUrl = cfg.apiUrl
            , project = cfg.projectId
            }
      else
        let printStatus label = do
              putTextLn label
              putTextLn $ "API URL: " <> cfg.apiUrl
              whenJust cfg.projectId $ \p -> putTextLn $ "Project: " <> p
         in case method of
              Just "env" -> printStatus "Authenticated via MONOSCOPE_API_KEY environment variable"
              Just "token" -> printStatus "Authenticated via stored token"
              _ -> printError "Not authenticated. Run: monoscope auth login --token <token>"
  AuthLogout -> do
    removeToken
    putTextLn "Logged out"


-- Config commands

data ConfigSetOpts = ConfigSetOpts {key :: Text, value :: Text}
  deriving stock (Show)


newtype ConfigGetOpts = ConfigGetOpts {key :: Maybe Text}
  deriving stock (Show)


-- | Stable JSON shape for @config get@ in JSON/YAML mode. The @api_key@ is
-- always redacted to "********" when set, 'Nothing' (→ @null@) otherwise.
data ConfigGetJson = ConfigGetJson
  { apiUrl :: Text
  , project :: Maybe Text
  , apiKey :: Maybe Text
  }
  deriving stock (Generic)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] ConfigGetJson


runConfigInit :: (Environment :> es, FileSystem :> es, IOE :> es) => Eff es ()
runConfigInit = do
  tty <- isInteractiveTTY
  result <-
    liftIO
      $ inputForm
        tty
        "Monoscope Configuration"
        [ ("api_url", "API URL", "https://api.monoscope.tech")
        , ("token", "API token", "")
        , ("project", "Default project ID", "")
        ]
  let field k = fromMaybe "" $ Map.lookup k result
      url = let v = field "api_url" in if T.null v then "https://api.monoscope.tech" else v
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


-- | B9: 'config get' respects the global output mode. Plain text is fine
-- in a terminal, but agents need JSON to round-trip the full snapshot.
-- The redacted @api_key@ is preserved across modes — never leak the raw value.
runConfigGet :: (Environment :> es, FileSystem :> es, IOE :> es) => ConfigGetOpts -> OutputMode -> Eff es ()
runConfigGet opts mode = do
  cfg <- resolveConfig
  let asJson =
        ConfigGetJson
          { apiUrl = cfg.apiUrl
          , project = cfg.projectId
          , apiKey = ("********" :: Text) <$ cfg.apiKey
          }
  case opts.key of
    Nothing -> renderWith mode asJson $ do
      putTextLn $ "api_url = " <> cfg.apiUrl
      putTextLn $ "project = " <> fromMaybe "(not set)" cfg.projectId
      putTextLn $ "api_key = " <> maybe "(not set)" (const "********") cfg.apiKey
    Just "api_url" -> putTextLn cfg.apiUrl
    Just "project" -> putTextLn $ fromMaybe "(not set)" cfg.projectId
    Just "api_key" -> putTextLn $ maybe "(not set)" (const "********") cfg.apiKey
    Just k -> putTextLn ("Unknown key: " <> k) >> liftIO exitFailure


-- Services

newtype ServicesListOpts = ServicesListOpts
  { since :: Maybe Text
  }
  deriving stock (Show)


-- | Aggregate events by 'resource.service.name' to derive the service list.
-- Backed by the precomputed @/api/v1/facets@ endpoint (one indexed lookup
-- in the server's @apis.facet_summaries@ table) — single-millisecond instead
-- of fetching 10k events to aggregate client-side. Earlier iterations went
-- via @/api/v1/metrics@ (returned empty for @summarize ... by@) then events
-- aggregation (correct but slow); facets is the right home now that the
-- background job populates them.
runServicesList :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> ServicesListOpts -> OutputMode -> Eff es ()
runServicesList cfg opts mode = do
  validateDurationOrDie "--since" opts.since
  let params = [("since", fromMaybe "24H" opts.since), ("field", "resource.service.name")]
  withAPIResult cfg "/api/v1/facets" params $ \val -> do
    let services = parseServiceFacets val
        normalized = AE.object ["services" AE..= services, "count" AE..= length services]
    renderWith mode normalized
      $ if null services
        then putTextLn "No services found"
        else renderTable ["service", "events"] [[s.name, show s.events] | s <- services]


-- | One row of @services list@ output. Field labels are camelCase in Haskell
-- but the JSON encoding is snake_case via the 'CamelToSnake' modifier so
-- adding a field can't drift between Haskell and the wire.
data ServiceRow = ServiceRow {name :: Text, events :: Int}
  deriving stock (Generic)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] ServiceRow


-- | The facets endpoint returns @{ <field>: [{value, count}, ...] }@.
-- Pull out the @resource.service.name@ array and rename @value@→@name@,
-- @count@→@events@ for the CLI surface. Decodes through the server's
-- 'Fields.FacetValue' record so a wire change shows up at compile time.
parseServiceFacets :: AE.Value -> [ServiceRow]
parseServiceFacets v = case AE.fromJSON @(Map Text [Fields.FacetValue]) v of
  AE.Success m -> [ServiceRow f.value f.count | f <- fold (Map.lookup "resource.service.name" m)]
  AE.Error _ -> []


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
  , -- C9: opaque cursor for next page; pass back the @cursor@ field from a
    -- prior response. The CLI does not synthesise it.
    cursor :: Maybe Text
  , -- C11: short-circuit "search → grab one event for the next call".
    -- @--first@ truncates the result to one event in the JSON envelope;
    -- @--id-only@ further reduces output to the bare event id on stdout.
    -- (Field is named @firstOnly@ to avoid colliding with @Relude.first@.)
    firstOnly :: Bool
  , idOnly :: Bool
  }
  deriving stock (Show)


data EventsGetOpts = EventsGetOpts
  { eventId :: Text
  , showTree :: Bool
  , at :: Maybe Text -- ^ ISO-8601 timestamp for a fast point-in-time lookup
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
  , -- C4: when set, include a per-trace summary in the JSON output —
    -- @{traces: [{trace_id, services, span_count, error_count}], events: [...]}@.
    -- For an incident agent this is the killer feature: "given a timestamp,
    -- which traces and services were impacted in this window?".
    summary :: Bool
  }
  deriving stock (Show)


runEventsSearch :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> EventsSearchOpts -> OutputMode -> Eff es ()
runEventsSearch cfg opts mode = do
  -- D5: Validate user input client-side so agents get a clear, actionable
  -- error instead of an opaque server-side HTTP 400.
  validatedOpts <- validateEventsOpts opts
  let params = buildSearchParams validatedOpts
  withAPIResult cfg "/api/v1/events" params $ \val -> do
    -- Decode once, share between the JSON projection and (when --first is
    -- set) the table projection.
    let firstOnly = validatedOpts.firstOnly || validatedOpts.idOnly
        normalized = case decodeEvents val of
          Nothing -> val
          Just d -> normalizeDecoded d validatedOpts.fields
        sliced = if firstOnly then takeFirstEvent normalized else normalized
        -- Mirror --first into the raw envelope so table mode also shows
        -- one row instead of all-of-them (the JSON path used to slice and
        -- the table path didn't, leading to inconsistent output).
        valForTable = if firstOnly then takeFirstRow val else val
    case (validatedOpts.idOnly, mode) of
      -- C11: --id-only short-circuits to a bare id on stdout — the natural
      -- input for "search → get" pipelines, no jq required.
      (True, _) -> emitFirstEventId sliced
      (False, _) -> renderWith mode sliced (renderEventsTable valForTable validatedOpts.fields)


-- | Trim the normalised events envelope to the first event only, preserving
-- @count@/@cursor@/@has_more@ semantics for downstream pagination. The
-- raw-envelope variant 'takeFirstRow' targets @logsData@ for table mode.
takeFirstEvent, takeFirstRow :: AE.Value -> AE.Value
takeFirstEvent v = v & AL.key "events" . AL._Array %~ V.take 1
takeFirstRow v = v & AL.key "logsData" . AL._Array %~ V.take 1


-- | Print the first event's @id@ to stdout; exit non-zero if there isn't one.
-- @--id-only@ is the "give me a useful next-step argument" feature an LLM
-- agent reaches for when chaining @events search → events get@.
emitFirstEventId :: IOE :> es => AE.Value -> Eff es ()
emitFirstEventId v = case v of
  AE.Object obj
    | Just (AE.Array arr) <- KM.lookup "events" obj
    , Just (AE.Object e) <- arr V.!? 0
    , Just (AE.String i) <- KM.lookup "id" e ->
        putTextLn i
  _ -> printError "no events matched" >> liftIO exitFailure


-- | Validate every flag on 'EventsSearchOpts' in one pass and apply
-- 'normalizeKind' so the wire-level @source@ value is always @log@/@span@
-- (D2). Failures print a clear message and exit non-zero (D5).
-- | Validate search options before hitting the server. Note: EventsTailOpts
-- and EventsContextOpts do not carry a user-supplied KQL query (tail uses
-- structured --service/--level flags; context uses --at + structured flags),
-- so validateQueryOrDie is not needed for those commands.
validateEventsOpts :: IOE :> es => EventsSearchOpts -> Eff es EventsSearchOpts
validateEventsOpts opts = do
  validateDurationOrDie "--since" opts.since
  validateQueryOrDie opts.query
  kindNorm <- validateAndNormalizeKind opts.kind
  -- DuplicateRecordFields makes record-update on @kind@ ambiguous
  -- (also a field on EventsTailOpts/EventsContextOpts) — bind to a typed let
  -- so GHC resolves which record we're updating.
  let opts' :: EventsSearchOpts
      opts' = opts{kind = kindNorm}
  pure opts'


-- | 'events get ID' — fetch one event (or full trace tree with --tree).
-- When --at TIMESTAMP is given, uses GET /api/v1/events/{id}/time/{ts} for an
-- O(1) timeseries point lookup. Without --at, falls back to a 90d KQL search.
runEventsGet :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> EventsGetOpts -> OutputMode -> Eff es ()
runEventsGet cfg opts mode =
  case opts.at >>= iso8601ParseM . toString of
    Just (t :: UTCTime) -> do
      -- Direct O(1) lookup: both id and timestamp known → single-partition query.
      -- Returns raw OtelLogsAndSpans JSON; always rendered as JSON (table view
      -- is not applicable for a single denormalized span record).
      let path = "/api/v1/events/" <> opts.eventId <> "/time/" <> toText (iso8601Show t)
      withAPIResult cfg path [] renderJSON
    Nothing -> do
      -- Fallback: scan 90d, also match by trace_id so bare trace IDs work.
      let eid = T.replace "\"" "\\\"" (T.replace "\\" "\\\\" opts.eventId)
          q
            | opts.showTree = "context.trace_id==\"" <> eid <> "\""
            | otherwise = "(id==\"" <> eid <> "\") or (context.trace_id==\"" <> eid <> "\")"
          params = [("query", q), ("since", "90d")]
      withAPIResult cfg "/api/v1/events" params $ \val ->
        if opts.showTree
          then renderTraceTree val
          else renderWith mode (normalizeEventsResponse val Nothing) (renderEventsTable val Nothing)


runEventsTail :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> EventsTailOpts -> Maybe Text -> Eff es ()
runEventsTail cfg opts kindOverride = do
  -- D2/D5: validate + normalize kind before entering the poll loop so a
  -- typo doesn't burn HTTP requests every 2 seconds.
  kindNorm <- validateAndNormalizeKind (opts.kind <|> kindOverride)
  seenRef <- newIORef (Set.empty :: Set Text)
  forever $ do
    let q = foldFiltersIntoQuery "" opts.service opts.level
        params =
          catMaybes
            [ if T.null q then Nothing else Just ("query", q)
            , Just ("since", "10s")
            , ("source",) <$> kindNorm
            ]
    apiGet cfg "/api/v1/events" params >>= \case
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


runEventsContext :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> EventsContextOpts -> Maybe Text -> OutputMode -> Eff es ()
runEventsContext cfg opts kindOverride mode = do
  validateDurationOrDie "--window" opts.window
  kindNorm <- validateAndNormalizeKind (opts.kind <|> kindOverride)
  let q = foldFiltersIntoQuery "" opts.service Nothing
      params =
        catMaybes
          [ if T.null q then Nothing else Just ("query", q)
          , Just ("from", opts.timestamp)
          , Just ("since", fromMaybe "5m" opts.window)
          , ("source",) <$> kindNorm
          ]
  withAPIResult cfg "/api/v1/events" params $ \val -> do
    -- Decode once, share between normalize + summary (avoids walking
    -- @logsData@ twice on large responses).
    let enriched = case decodeEvents val of
          Nothing -> val
          Just d ->
            let normalized = normalizeDecoded d Nothing
             in if opts.summary then withTraceSummary d normalized else normalized
    renderWith mode enriched (renderEventsTable val Nothing)


-- | C4: aggregate the events response into a per-trace summary. The result
-- merges into the existing normalised envelope under a @traces@ key:
--
-- @
-- { events: [...]
-- , count, has_more, cursor
-- , traces: [ {trace_id, services: [...], span_count, error_count}, ... ]
-- }
-- @
--
-- Empty @trace_id@s are dropped. Services are deduplicated and sorted.
-- Takes the pre-parsed 'DecodedEvents' so the caller doesn't pay for a
-- second walk of @logsData@.
withTraceSummary :: DecodedEvents -> AE.Value -> AE.Value
withTraceSummary d (AE.Object out) =
  let -- Pre-resolve column indices once instead of Map.lookup'ing per cell
      -- per row — matters on large @events context --summary@ payloads.
      -- "service" is the alias for resource.service.name in the curated
      -- column set (Pages.LogExplorer.Log.allCols); "errors" is a boolean
      -- rendered as "true"/"false". Service has a dotted-form fallback in
      -- case the column set is reshaped server-side.
      svcIdx = Map.lookup "service" d.idxMap <|> Map.lookup "resource.service.name" d.idxMap
      errIdx = Map.lookup "errors" d.idxMap
      tidIdx = Map.lookup "context.trace_id" d.idxMap <|> Map.lookup "trace_id" d.idxMap
      at idx r = idx >>= \i -> listToMaybe (drop i r)
      merge (s1, c1, e1) (s2, c2, e2) = (s1 <> s2, c1 + c2, e1 + e2)
      groups :: Map Text (Set Text, Int, Int)
      groups =
        Map.fromListWith
          merge
          [ (tid, (one (fromMaybe "" (at svcIdx r)), 1, if at errIdx r == Just "true" then 1 else 0))
          | r <- d.rows
          , let tid = fromMaybe "" (at tidIdx r)
          , not (T.null tid)
          ]
      traces =
        [ TraceSummary tid (sort (filter (not . T.null) (Set.toList svcs))) n errs
        | (tid, (svcs, n, errs)) <- Map.toList groups
        ]
   in AE.Object (KM.insert "traces" (AE.toJSON traces) out)
withTraceSummary _ v = v


-- | Per-trace breakdown for 'events context --summary'. Field labels are
-- camelCase in Haskell, snake_case in JSON ('CamelToSnake' from
-- @deriving-aeson@) so adding a field can't drift between Haskell and the wire.
data TraceSummary = TraceSummary
  { traceId :: Text
  , services :: [Text]
  , spanCount :: Int
  , errorCount :: Int
  }
  deriving stock (Generic)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] TraceSummary


-- | Build query params for 'events search'. Default 'since' to "1H" to match
-- the platform's TimePicker default ('Pkg.Components.TimePicker.parseTimeRange').
-- When --from/--to are explicit, send since="" so the server uses the absolute
-- range instead of its own 1H fallback.
buildSearchParams :: EventsSearchOpts -> [(Text, Text)]
buildSearchParams opts =
  let q = foldFiltersIntoQuery opts.query opts.service opts.level
      sinceParam = case opts.since of
        Just s -> s
        Nothing -> if isJust opts.from || isJust opts.to then "" else "1H"
   in catMaybes
        [ if T.null q then Nothing else Just ("query", q)
        , Just ("since", sinceParam)
        , ("from",) <$> opts.from
        , ("to",) <$> opts.to
        , ("source",) <$> opts.kind
        , ("limit",) . show <$> opts.limit
        , ("cursor",) <$> opts.cursor
        ]


-- | Fold --service/--level CLI flags into a KQL query string.
--
-- The platform's KQL parser uses '==' for equality with double-quoted strings
-- (see 'Pkg.Parser.Expr.pTerm'). The CLI previously emitted ':' which the
-- server rejected with HTTP 400 — leaving --service and --level effectively
-- broken. Filters are AND-combined with the user's positional query.
--
-- >>> foldFiltersIntoQuery "" Nothing Nothing
-- ""
-- >>> foldFiltersIntoQuery "errors > 0" Nothing Nothing
-- "errors > 0"
-- >>> foldFiltersIntoQuery "" (Just "web") Nothing
-- "resource.service.name==\"web\""
-- >>> foldFiltersIntoQuery "" Nothing (Just "warn")
-- "severity.text==\"warn\""
-- >>> foldFiltersIntoQuery "errors > 0" (Just "web") (Just "error")
-- "resource.service.name==\"web\" and severity.text==\"error\" and (errors > 0)"
-- >>> foldFiltersIntoQuery "" (Just "my app") Nothing
-- "resource.service.name==\"my app\""
foldFiltersIntoQuery :: Text -> Maybe Text -> Maybe Text -> Text
foldFiltersIntoQuery query mService mLevel
  | T.null prefix = trimmed
  | T.null trimmed = prefix
  | otherwise = prefix <> " and (" <> trimmed <> ")"
  where
    q v = "\"" <> T.replace "\"" "\\\"" v <> "\""
    eq field val = field <> "==" <> q val
    filters = catMaybes [eq "resource.service.name" <$> mService, eq "severity.text" <$> mLevel]
    prefix = T.intercalate " and " filters
    trimmed = T.strip query


-- | Single decode of the events envelope. Sharing the parsed @colIdxMap@
-- + @logsData@ between 'normalizeEventsResponse' and 'withTraceSummary'
-- avoids walking the rows array twice for large payloads ('events context
-- --summary' was the worst offender). 'normalizeEventsResponse' projects
-- the positional row representation into named-field objects so agents/jq
-- consumers don't need to thread 'colIdxMap' themselves; the wire shape is
-- stable: @{events: [...], count, has_more, cursor}@.
data DecodedEvents = DecodedEvents
  { idxMap :: Map Text Int
  , rows :: [[Text]]
  , count :: Int
  , hasMore :: AE.Value
  , cursor :: AE.Value
  }


decodeEvents :: AE.Value -> Maybe DecodedEvents
decodeEvents (AE.Object o) =
  Just
    DecodedEvents
      { idxMap = extractColIdxMap (KM.lookup "colIdxMap" o)
      , rows = extractRows (KM.lookup "logsData" o)
      , count = extractInt (KM.lookup "count" o)
      , hasMore = fromMaybe AE.Null (KM.lookup "hasMore" o)
      , cursor = fromMaybe AE.Null (KM.lookup "cursor" o)
      }
decodeEvents _ = Nothing


normalizeEventsResponse :: AE.Value -> Maybe Text -> AE.Value
normalizeEventsResponse val mFields = maybe val (`normalizeDecoded` mFields) (decodeEvents val)


-- | Project rows into named-field objects. Missing fields encode as @null@
-- (not @""@) so agents can distinguish "field present but empty" from
-- "field missing or row truncated".
normalizeDecoded :: DecodedEvents -> Maybe Text -> AE.Value
normalizeDecoded d mFields =
  let keep = case mFields of
        Nothing -> Map.keys d.idxMap
        Just s -> filter (`Map.member` d.idxMap) (T.splitOn "," s)
      cellAt r k = Map.lookup k d.idxMap >>= \i -> listToMaybe (drop i r)
      toObj r =
        AE.object
          [ AK.fromText k AE..= maybe AE.Null AE.String (cellAt r k)
          | k <- keep
          ]
      events = map toObj d.rows
   in AE.object
        [ "events" AE..= events
        , "count" AE..= d.count
        , "has_more" AE..= d.hasMore
        , "cursor" AE..= d.cursor
        ]


renderEventsTable :: IOE :> es => AE.Value -> Maybe Text -> Eff es ()
renderEventsTable val mFields = case val of
  AE.Object obj -> do
    let cols = extractTextArray $ KM.lookup "cols" obj
        idxMap = extractColIdxMap $ KM.lookup "colIdxMap" obj
        rows = extractRows $ KM.lookup "logsData" obj
        count = extractInt $ KM.lookup "count" obj
        filteredCols = maybe cols (\f -> filter (`elem` T.splitOn "," f) cols) mFields
        colIdxs = mapMaybe (`Map.lookup` idxMap) filteredCols
        summaryCols = Set.fromList [i | (c, i) <- zip filteredCols [0 :: Int ..], c `elem` ["summary", "latency_breakdown"]]
        filteredRows =
          map
            ( \r ->
                let raw = map (\i -> fromMaybe "" $ listToMaybe (drop i r)) colIdxs
                 in [if Set.member ci summaryCols then renderSummaryCell cell else cell | (ci, cell) <- zip [0 ..] raw]
            )
            rows
    renderTable filteredCols filteredRows
    putTextLn $ "\n" <> show count <> " results"
  _ -> renderJSON val


renderSummaryCell :: Text -> Text
renderSummaryCell cell = case AE.eitherDecode @[Text] (encodeUtf8 cell) of
  Right items -> renderSummaryItems items
  Left _ -> cell


extractColIdxMap :: Maybe AE.Value -> Map Text Int
extractColIdxMap = \case
  -- Column indices are non-negative integers from the server; @floor@ over
  -- @round@ documents that intent (no banker's rounding on a value that's
  -- already integral).
  Just (AE.Object obj) -> Map.fromList [(AK.toText k, floor n) | (k, AE.Number n) <- KM.toList obj]
  _ -> mempty


renderTraceTree :: IOE :> es => AE.Value -> Eff es ()
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


runMetricsQuery :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> MetricsQueryOpts -> OutputMode -> Eff es ()
runMetricsQuery cfg opts mode = do
  let params = metricsParams opts.expression opts.since opts.from opts.to
  withAPIResult cfg "/api/v1/metrics" params $ \val ->
    withMetricsData val (renderJSON val) $ \md -> do
      renderWith mode val (renderMetricsTable md)
      whenJust opts.assert $ checkAssertion md


runMetricsChart :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> MetricsChartOpts -> Eff es ()
runMetricsChart cfg opts = do
  -- Validate --watch up front so a typo doesn't silently fall back to 5s.
  validateDurationOrDie "--watch" opts.watch
  let run = do
        let params = metricsParams opts.expression opts.since opts.from opts.to
        withAPIResult cfg "/api/v1/metrics" params $ \val ->
          withMetricsData val (renderJSON val) renderSparkline
  case opts.watch of
    Nothing -> run
    Just interval -> forever $ do
      liftIO $ putStr ("\ESC[2J\ESC[H" :: String)
      run
      threadDelay (parseDurationMs interval * 1000)


metricsParams :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Text)]
metricsParams expr mSince mFrom mTo =
  catMaybes
    [ Just ("query", expr)
    , ("since",) <$> (mSince <|> Just "1h")
    , ("from",) <$> mFrom
    , ("to",) <$> mTo
    ]


-- | Decode the @/api/v1/metrics@ response into the server's typed
-- 'MetricsData'; fall back to 'onFail' (typically 'renderJSON') on a shape
-- mismatch. The aeson error is forwarded via 'printDebug' so a server
-- envelope change is visible under @--debug@ instead of vanishing into the
-- raw-JSON path.
withMetricsData :: (Environment :> es, IOE :> es) => AE.Value -> Eff es () -> (MetricsData -> Eff es ()) -> Eff es ()
withMetricsData val onFail onOk = case AE.fromJSON val of
  AE.Success md -> onOk md
  AE.Error msg -> printDebug ("metrics decode failed: " <> toText msg) >> onFail


renderMetricsTable :: IOE :> es => MetricsData -> Eff es ()
renderMetricsTable md@MetricsData{headers, dataText}
  | V.null headers = renderJSON md
  | otherwise = renderTable (V.toList headers) (V.toList (V.toList <$> dataText))


renderSparkline :: IOE :> es => MetricsData -> Eff es ()
renderSparkline MetricsData{headers, dataset} = do
  unless (V.null headers) $ putTextLn $ T.intercalate " | " (V.toList headers)
  forM_ dataset $ \row ->
    putTextLn $ T.concat $ map sparklineBar (V.toList row)


checkAssertion :: IOE :> es => MetricsData -> Text -> Eff es ()
checkAssertion MetricsData{dataFloat} cond = case dataFloat of
  Just v -> unless (evalCond v cond) $ do
    printError $ "Assertion failed: " <> show v <> " " <> cond
    liftIO exitFailure
  Nothing -> printError "Warning: --assert ignored, no numeric result to evaluate"


-- | Case-insensitive — matches 'validateDurationFor' so @--watch 5M@
-- works the same way as @--since 1H@. Falls back to the 5-second default
-- if the suffix is missing or unparseable; bare numbers are NOT accepted
-- as seconds (validateDurationFor would have rejected them at flag parse).
parseDurationMs :: Text -> Int
parseDurationMs (T.toLower -> t)
  | Just n <- T.stripSuffix "ms" t = fromMaybe 5000 (readMaybe $ toString n)
  | Just n <- T.stripSuffix "s" t = maybe 5000 (* 1000) (readMaybe $ toString n)
  | Just n <- T.stripSuffix "m" t = maybe 5000 (* 60_000) (readMaybe $ toString n)
  | Just n <- T.stripSuffix "h" t = maybe 5000 (* 3_600_000) (readMaybe $ toString n)
  | otherwise = 5000


-- Device auth helpers

selectProject :: (Environment :> es, FileSystem :> es, IOE :> es) => Maybe [ProjectInfo] -> Eff es ()
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
  apiPostUnauth baseUrl "/api/device/token" [("device_code", deviceCode)] >>= \case
    Left _ -> pollForToken baseUrl deviceCode (remaining - 1) -- transient error, keep polling
    Right bs -> case AE.eitherDecode @DeviceTokenResponse bs of
      Left _ -> pollForToken baseUrl deviceCode (remaining - 1)
      Right resp
        | resp.err == Just "authorization_pending" -> pollForToken baseUrl deviceCode (remaining - 1)
        | isJust resp.sessionId -> pure (Just resp)
        | otherwise -> pure Nothing


tryOpenBrowser :: String -> IO ()
tryOpenBrowser url =
  void
    $ tryAny
    $ void (spawnProcess "open" [url])
    `catch` \(_ :: SomeException) ->
      void $ spawnProcess "xdg-open" [url]
