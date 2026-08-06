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
  -- Charts and dashboards in the terminal
  runChart,
  ChartCmdOpts (..),
  runDashboardRender,
  DashboardRenderOpts (..),
  runOpen,
  OpenOpts (..),
  OpenTarget (..),
  parseOpenTarget,
  uiPath,
  runStatus,
  StatusOpts (..),
  -- Telemetry generation
  runTelemetryGen,
  TelemetryGenOpts (..),
  -- Send event
  runSendEvent,
  SendEventOpts (..),
  parseKV,
  parseSepKV,
) where

import Relude

import CLI.Chart qualified as Chart
import CLI.Config (CLIConfig (..), ConfigKey (..), allConfigKeys, configDir, configFilePath, configKeyText, parseConfigKey, removeToken, resolveConfig, saveToken, setConfigValue)
import CLI.Core (OutputMode (..), apiGet, apiGetJson, apiPostUnauth, isInteractiveTTY, isJsonOutput, printDebug, printError, renderAPIError, renderJSON, renderTable, renderWith, withAPIResult)
import CLI.Dashboard qualified as Dash
import CLI.LogView (EventRow (..), LogFormat (..), eventRows, parseLogFormat, renderEventLine, renderLogfmt, renderWaterfall)
import CLI.Table (termWidth)
import CLI.UI (inputForm, selectFromList, withSpinner)
import CLI.Validate (validateAndNormalizeKind, validateDurationOrDie, validateQueryOrDie)
import Control.Exception (bracket)
import Control.Lens ((%~), (^..), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens qualified as AL
import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Default (def)
import Data.Effectful.Wreq (HTTP, runHTTPWreq)
import Data.HashMap.Strict qualified as HM
import Data.List.Extra (chunksOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Data.Time qualified
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Environment (Environment)
import Effectful.Environment qualified as Env
import Effectful.FileSystem (FileSystem)
import Numeric (showHex)
import OpenTelemetry.Attributes qualified as OA
import OpenTelemetry.Context.ThreadLocal qualified as OtelCtx
import OpenTelemetry.Trace (SpanStatus (..), Tracer, TracerOptions (..), defaultSpanArguments, initializeGlobalTracerProvider, makeTracer, shutdownTracerProvider)
import OpenTelemetry.Trace qualified as Trace
import Pages.Charts.Types (MetricsData (..))
import Pkg.CLIFormat (cleanSummaryValue, evalCond, extractInt, extractRawRows, extractRows, extractTextArray, renderSummaryItems, valToText)
import System.Environment (setEnv)
import System.Process (spawnProcess)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (catch, tryAny)
import Web.Wire (DeviceCodeResponse (..), DeviceTokenResponse (..), ProjectInfo (..))
import Web.Wire qualified as Wire


-- Auth

data AuthCommand = AuthLogin (Maybe Text) | AuthStatus | AuthLogout
  deriving stock (Show)


-- | Stable JSON shape for @--agent auth status@.
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
    -- C7: When output is JSON/YAML (or stdout is piped — same signal post
    -- mode-collapse) the session is non-interactive by design: refuse the
    -- device-code flow (5-min poll, browser launch) and direct the caller to
    -- --token. Otherwise a script or LLM agent will hang waiting for human
    -- input.
    whenM isJsonOutput $ do
      printError "non-interactive output mode (--json/--yaml or piped stdout): pass --token; interactive login is disabled"
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
    -- C8: JSON/YAML output emits a stable JSON shape so a script doesn't
    -- have to regex stdout. The shape is intentionally narrow — adding
    -- fields is safe; renaming or removing them is a breaking change.
    let method = ("env" :: Text) <$ envKey <|> "token" <$ cfg.apiKey
    ifM
      isJsonOutput
      (renderJSON AuthStatusJson{authenticated = isJust method, method, apiUrl = cfg.apiUrl, project = cfg.projectId})
      $ case method of
        Nothing -> printError "Not authenticated. Run: monoscope auth login --token <token>"
        Just m -> do
          putTextLn $ "Authenticated via " <> if m == "env" then "MONOSCOPE_API_KEY environment variable" else "stored token"
          putTextLn $ "API URL: " <> cfg.apiUrl
          whenJust cfg.projectId $ \p -> putTextLn $ "Project: " <> p
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
  let field k = guarded (not . T.null) =<< Map.lookup k result
  setConfigValue CKApiUrl $ fromMaybe "https://api.monoscope.tech" (field "api_url")
  whenJust (field "token") saveToken
  whenJust (field "project") (setConfigValue CKProject)
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
-- of fetching 10k events to aggregate client-side.
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


-- | One row of @services list@ output.
data ServiceRow = ServiceRow {name :: Text, events :: Int}
  deriving stock (Generic)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] ServiceRow


-- | The facets endpoint returns @{ <field>: [{value, count}, ...] }@.
-- Pull out the @resource.service.name@ array and rename @value@→@name@,
-- @count@→@events@ for the CLI surface. Decodes through the server's
-- 'Wire.FacetValue' record so a wire change shows up at compile time.
parseServiceFacets :: AE.Value -> [ServiceRow]
parseServiceFacets v = case AE.fromJSON @(Map Text [Wire.FacetValue]) v of
  AE.Success m -> [ServiceRow f.value f.count | f <- fold (Map.lookup "resource.service.name" m)]
  AE.Error _ -> []


-- Events

data EventsSearchOpts = EventsSearchOpts
  { query :: Text
  , since :: Maybe Text
  , from :: Maybe Text
  , to :: Maybe Text
  , kind :: Maybe Text
  , service :: [Text]
  -- ^ repeatable. One value → @resource.service.name=="x"@. Multiple
  -- values → @resource.service.name in ("x", "y")@. Empty → no filter.
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
  , withChildren :: Bool
  -- ^ Also return descendants (the sub-tree) of each matched span. Off by
  -- default — predicate hits only.
  , chunkHours :: Maybe Int
  -- ^ Hours per transport slice when fetching wide --since windows (the CF
  -- edge 504s on multi-day single requests). Purely a transport knob — the
  -- output is always a single envelope. Defaults to 1; @0@ disables slicing.
  , format :: Maybe Text
  -- ^ Terminal shape: @line@ (default), @table@ or @logfmt@. Ignored under
  -- @--json@/@--yaml@, where the envelope is the output.
  }
  deriving stock (Show)


data EventsGetOpts = EventsGetOpts
  { eventId :: Text
  , showTree :: Bool
  , at :: Maybe Text
  -- ^ ISO-8601 timestamp for a fast point-in-time lookup
  , projectFields :: [Text]
  -- ^ when non-empty, project these top-level fields out of the event
  -- JSON instead of dumping the full blob.
  , showBody :: Bool
  -- ^ shorthand for @--field body --field summary@.
  }
  deriving stock (Show)


data EventsTailOpts = EventsTailOpts
  { kind :: Maybe Text
  , service :: Maybe Text
  , level :: Maybe Text
  , grep :: Maybe Text
  , format :: Maybe Text
  , interval :: Maybe Text
  -- ^ Poll cadence (default 2s). The events store is append-only and queried
  -- by time window, so following it is polling either way; this just lets a
  -- quiet project back off and a busy incident tighten up.
  , backfill :: Maybe Text
  -- ^ Print this much history before following, like @tail -n@.
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
  , format :: Maybe Text
  }
  deriving stock (Show)


runEventsSearch :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> EventsSearchOpts -> OutputMode -> Eff es ()
runEventsSearch cfg opts mode = do
  -- D5: Validate user input client-side so agents get a clear, actionable
  -- error instead of an opaque server-side HTTP 400.
  validatedOpts <- validateEventsOpts opts
  case planChunks validatedOpts of
    [] -> withAPIResult cfg "/api/v1/events" (buildSearchParams validatedOpts) (renderSearchResult validatedOpts mode)
    chunks -> runChunkedSearch cfg validatedOpts mode chunks


-- | Shared rendering tail for single and chunked searches — the output
-- contract (exactly one envelope; @--first@/@--id-only@ slicing) lives here
-- and only here.
renderSearchResult :: (Environment :> es, IOE :> es) => EventsSearchOpts -> OutputMode -> AE.Value -> Eff es ()
renderSearchResult validatedOpts mode val = do
  let firstOnly = validatedOpts.firstOnly || validatedOpts.idOnly
      normalized = normalizeEventsResponse val validatedOpts.fields
      sliced = if firstOnly then takeFirst "events" normalized else normalized
      -- Mirror --first into the raw envelope so table mode also shows
      -- one row instead of all-of-them (the JSON path used to slice and
      -- the table path didn't, leading to inconsistent output).
      valForTable = if firstOnly then takeFirst "logsData" val else val
  fmt <- resolveFormat validatedOpts.format
  if validatedOpts.idOnly
    -- C11: --id-only short-circuits to a bare id on stdout — the natural
    -- input for "search → get" pipelines, no jq required.
    then emitFirstEventId sliced
    else renderWith mode sliced (renderEventsHuman fmt validatedOpts.fields valForTable)


-- | Merge accumulator for 'runChunkedSearch'.
data ChunkMerge = ChunkMerge
  { template :: Maybe AE.Value
  -- ^ first slice's raw envelope — carries colIdxMap etc. for the merge
  , rows :: [[AE.Value]]
  -- ^ merged raw logsData rows, newest slice first
  , seen :: Set Text
  -- ^ event ids already kept (dedupes slice-boundary overlap)
  , more :: Bool
  , cur :: AE.Value
  , stopped :: Bool
  }


-- | Chunked search. Slices are an internal transport detail — one HTTP
-- request per slice so wide windows don't 504 at the CF edge — and never an
-- output shape: the result is exactly one 'renderSearchResult' envelope,
-- identical to the single-request path. Slices are fetched newest-first with
-- the remaining @--limit@ budget passed through; rows are deduped by event id
-- at slice boundaries. When a slice reports hasMore — or the budget runs out
-- with older slices unfetched — the merged envelope reports hasMore (with the
-- stopping slice's cursor when available) and older slices are skipped, so
-- following the cursor continues into them without duplicates.
runChunkedSearch
  :: (Environment :> es, HTTP :> es, IOE :> es)
  => CLIConfig -> EventsSearchOpts -> OutputMode -> [(Text, Text)] -> Eff es ()
runChunkedSearch cfg validatedOpts mode slices = do
  printDebug $ "auto-chunk: " <> show (length slices `div` 2) <> " slices (use --chunk-hours 0 to disable)"
  -- Resolve sentinel offsets ("Nh") to absolute ISO 8601 strings anchored on
  -- one clock so a slow first request can't nudge later slice boundaries.
  now <- liftIO Data.Time.getCurrentTime
  let firstOnly = validatedOpts.firstOnly || validatedOpts.idOnly
      -- sliceOffsets emits slices newest-first already (i counts down)
      newestFirst = map (resolveOffsetPair now) (chunksOf 2 slices)
      baseOpts :: EventsSearchOpts
      baseOpts = validatedOpts{since = Nothing, from = Nothing, to = Nothing}
      baseParams = filter (\(k, _) -> k `notElem` ["since", "from", "to", "limit"]) (buildSearchParams baseOpts)
  ref <- newIORef (ChunkMerge Nothing [] mempty False AE.Null False)
  forM_ newestFirst $ \chunkParams -> do
    st <- readIORef ref
    let remaining = validatedOpts.limit <&> subtract (length st.rows)
    if st.stopped || (firstOnly && not (null st.rows))
      then pass
      else case remaining of
        Just r | r <= 0 -> writeIORef ref st{stopped = True, more = True}
        _ -> do
          let params = baseParams <> chunkParams <> maybe [] (\r -> [("limit", show r)]) remaining
          withAPIResult cfg "/api/v1/events" params $ \val ->
            whenJust (decodeEvents val) $ \d -> do
              st' <- readIORef ref
              -- Server contract: /api/v1/events rows carry an id column. If it's
              -- missing, fall back to whole-row JSON as the dedup key (correct
              -- but O(rowSize) per row) and surface the violation in debug output.
              let idIdxM = Map.lookup "id" d.idxMap
                  keyOf r = maybe (decodeUtf8 (AE.encode r)) valToText (idIdxM >>= \i -> listToMaybe (drop i r))
                  fresh = filter (\r -> keyOf r `S.notMember` st'.seen) d.rawRows
                  sliceMore = d.hasMore == AE.Bool True
              when (isNothing idIdxM) $ printDebug "auto-chunk: response has no 'id' column; deduping by full-row JSON"
              writeIORef
                ref
                st'
                  { template = st'.template <|> Just val
                  , rows = st'.rows <> fresh
                  , seen = st'.seen <> S.fromList (map keyOf fresh)
                  , more = st'.more || sliceMore
                  , cur = if sliceMore then d.cursor else st'.cur
                  , stopped = sliceMore || (firstOnly && not (null fresh))
                  }
  final <- readIORef ref
  let keptRows = maybe final.rows (`take` final.rows) validatedOpts.limit
      patch =
        KM.insert "logsData" (AE.toJSON keptRows)
          . KM.insert "count" (AE.toJSON (length keptRows))
          . KM.insert "hasMore" (AE.Bool final.more)
          . KM.insert "cursor" final.cur
      merged = case final.template of
        Just (AE.Object o) -> AE.Object (patch o)
        _ -> AE.Object (patch mempty)
  renderSearchResult validatedOpts mode merged


-- | Decide whether to chunk. Returns:
--
-- - @[]@ when chunking is off, --from/--to are explicit, or --since is short.
-- - Otherwise a flat list of @("from", iso), ("to", iso), ...@ pairs, one
--   pair per slice, newest first. Timestamps are computed client-side so the
--   server endpoint can stay on its native ISO-8601 from/to parsing.
--
-- The planner stays pure by emitting *duration-relative offsets*; the caller
-- resolves them against one 'getCurrentTime' anchor.
planChunks :: EventsSearchOpts -> [(Text, Text)]
planChunks opts
  | chunkH <= 0 = []
  | isJust opts.from || isJust opts.to = []
  | otherwise = case parseDurationHours =<< opts.since of
      Just totalHours
        | totalHours > chunkH -> sliceOffsets totalHours chunkH
      _ -> []
  where
    chunkH = fromMaybe 1 opts.chunkHours


-- | @"<n><unit>"@ → seconds; units @s@/@m@/@h@/@d@, case-insensitive.
parseDurationSecs :: Text -> Maybe Int
parseDurationSecs (T.toLower . T.strip -> t) =
  asum
    [ (* mult) <$> (readMaybe . toString =<< T.stripSuffix sfx t)
    | (sfx, mult) <- [("s", 1), ("m", 60), ("h", 3600), ("d", 86400)]
    ]


parseDurationHours :: Text -> Maybe Int
parseDurationHours = fmap (`div` 3600) . parseDurationSecs


-- | Emit @[("from", "Xh"), ("to", "Yh"), …]@ sentinel pairs that the
-- chunked runner converts to absolute ISO 8601 timestamps using
-- 'getCurrentTime'. Format: @"<hours>h"@ means "now minus that many
-- hours"; this is a CLI-internal protocol, never sent to the server as-is.
sliceOffsets :: Int -> Int -> [(Text, Text)]
sliceOffsets totalH chunkH =
  let nSlices = (totalH + chunkH - 1) `div` chunkH
      upper i = totalH - i * chunkH
      lower i = max 0 (totalH - (i + 1) * chunkH)
   in concat
        [ [("from", show (upper i) <> "h"), ("to", show (lower i) <> "h")]
        | i <- [nSlices - 1, nSlices - 2 .. 0]
        ]


-- | Resolve a sentinel @"<N>h"@ offset pair to absolute ISO 8601 timestamps
-- anchored on the given 'now'. Slices use the same @now@ so a slow first
-- request can't drift boundaries on later slices.
resolveOffsetPair :: UTCTime -> [(Text, Text)] -> [(Text, Text)]
resolveOffsetPair now = map (second resolve)
  where
    resolve t = fromMaybe t $ do
      hours <- readMaybe @Int . toString =<< T.stripSuffix "h" t
      -- negate so the offset lands in the past
      pure $ toText $ iso8601Show $ addUTCTime (fromIntegral (negate hours * 3600) :: NominalDiffTime) now


-- | Trim an envelope to its first array element under key @k@, preserving
-- @count@/@cursor@/@has_more@ semantics for downstream pagination. Use
-- @"events"@ for the normalised envelope and @"logsData"@ for table mode.
takeFirst :: AE.Key -> AE.Value -> AE.Value
takeFirst k v = v & AL.key k . AL._Array %~ V.take 1


-- | Print the first event's @id@ to stdout; exit non-zero if there isn't one.
-- @--id-only@ is the "give me a useful next-step argument" feature an LLM
-- agent reaches for when chaining @events search → events get@.
emitFirstEventId :: IOE :> es => AE.Value -> Eff es ()
emitFirstEventId v =
  maybe (printError "no events matched" >> liftIO exitFailure) putTextLn
    $ v
    ^? AL.key "events" . AL.nth 0 . AL.key "id" . AL._String


-- | Validate every flag on 'EventsSearchOpts' in one pass and apply
-- 'normalizeKind' so the wire-level @source@ value is always @log@/@span@
-- (D2). Failures print a clear message and exit non-zero (D5).
--
-- Note: EventsTailOpts and EventsContextOpts do not carry a user-supplied KQL
-- query (tail uses structured --service/--level flags; context uses --at +
-- structured flags), so validateQueryOrDie is not needed for those commands.
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
-- An unparseable --at value is rejected up front so the user gets an immediate
-- error instead of a slow range-scan fallback that hides the typo.
runEventsGet :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> EventsGetOpts -> OutputMode -> Eff es ()
runEventsGet cfg opts mode = case opts.at of
  Nothing -> rangeScan
  Just raw -> case iso8601ParseM (toString raw) :: Maybe UTCTime of
    Nothing -> printError "--at: invalid ISO-8601 timestamp" >> liftIO exitFailure
    Just t
      | T.any (`elem` ("/?#%" :: [Char])) opts.eventId ->
          printError "event id must not contain any of '/', '?', '#', '%'" >> liftIO exitFailure
      | otherwise -> do
          -- Direct O(1) lookup: both id and timestamp known → single-partition query.
          -- Returns raw OtelLogsAndSpans JSON; always rendered as JSON (table view
          -- is not applicable for a single denormalized span record).
          let path = "/api/v1/events/" <> opts.eventId <> "/time/" <> toText (iso8601Show t)
          withAPIResult cfg path [] (renderJSON . applyProjection opts)
  where
    rangeScan = do
      -- Fallback: scan 90d, also match by trace_id so bare trace IDs work.
      let eid = T.replace "\"" "\\\"" (T.replace "\\" "\\\\" opts.eventId)
          q
            | opts.showTree = "context.trace_id==\"" <> eid <> "\""
            | otherwise = "(id==\"" <> eid <> "\") or (context.trace_id==\"" <> eid <> "\")"
          params = [("query", q), ("since", "90d")]
      withAPIResult cfg "/api/v1/events" params $ \val ->
        if opts.showTree
          then renderTraceTree val
          else
            let projected = applyProjection opts (normalizeEventsResponse val Nothing)
             in renderWith mode projected (renderEventsTable val Nothing)


-- | filter the top-level JSON to just @opts.projectFields@ (and/or the
-- @--show-body@ shorthand). When neither flag is set we return the value
-- unchanged so existing pipelines keep working.
applyProjection :: EventsGetOpts -> AE.Value -> AE.Value
applyProjection opts v
  | null wanted = v
  | otherwise = case v of
      AE.Object o -> AE.Object (KM.filterWithKey (\k _ -> AK.toText k `elem` wanted) o)
      _ -> v
  where
    wanted =
      opts.projectFields
        <> [b | opts.showBody, b <- ["body", "summary"]]


-- | Follow the event stream. Each poll asks for a window slightly wider than
-- the poll interval and drops ids already printed, so a row that lands in the
-- store a moment late still shows up exactly once.
runEventsTail :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> EventsTailOpts -> Maybe Text -> Eff es ()
runEventsTail cfg opts kindOverride = do
  -- D2/D5: validate + normalize kind before entering the poll loop so a
  -- typo doesn't burn HTTP requests every 2 seconds.
  kindNorm <- validateAndNormalizeKind (opts.kind <|> kindOverride)
  validateDurationOrDie "--interval" opts.interval
  validateDurationOrDie "--since" opts.backfill
  fmt <- resolveFormat opts.format
  (w, color) <- chartCanvas
  let pollMs = maybe 2000 parseDurationMs opts.interval
      -- Overlap the window with the previous poll: the store's write path can
      -- land a row a second or two after its timestamp, and a gapless window
      -- would skip it forever.
      windowSecs = max 5 ((pollMs `div` 1000) * 2)
      q = foldFiltersIntoQuery "" (maybeToList opts.service) opts.level
      poll since =
        apiGet cfg "/api/v1/events" (catMaybes [guarded (not . T.null) q <&> ("query",), Just ("since", since), ("source",) <$> kindNorm])
      emit r
        | not (all (`T.isInfixOf` haystack) opts.grep) = pass
        | otherwise = putTextLn $ if fmt == FmtLogfmt then renderLogfmt r else renderEventLine color w r
        where
          haystack = T.unwords (catMaybes [r.summary, r.spanName, r.service])
  seenRef <- newIORef (S.empty :: Set Text)
  let step since = do
        poll since >>= \case
          Left err -> printError (show err)
          Right bs -> case AE.eitherDecode @AE.Value bs of
            Left err -> printError (toText err)
            Right val -> do
              seen <- readIORef seenRef
              let rows = reverse [r | r <- eventRows val, maybe True (`S.notMember` seen) r.eventId]
                  seen' = seen <> S.fromList (mapMaybe (.eventId) rows)
              -- Bound the dedup set; the oldest half can no longer be re-served
              -- by a window this narrow.
              writeIORef seenRef $ if S.size seen' > 10000 then S.drop (S.size seen' `div` 2) seen' else seen'
              mapM_ emit rows
  whenJust opts.backfill step
  forever $ step (show windowSecs <> "s") >> threadDelay (pollMs * 1000)


runEventsContext :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> EventsContextOpts -> Maybe Text -> OutputMode -> Eff es ()
runEventsContext cfg opts kindOverride mode = do
  validateDurationOrDie "--window" opts.window
  kindNorm <- validateAndNormalizeKind (opts.kind <|> kindOverride)
  let q = foldFiltersIntoQuery "" (maybeToList opts.service) Nothing
      params =
        catMaybes
          [ if T.null q then Nothing else Just ("query", q)
          , Just ("from", opts.timestamp)
          , Just ("since", fromMaybe "5m" opts.window)
          , ("source",) <$> kindNorm
          ]
  fmt <- resolveFormat opts.format
  withAPIResult cfg "/api/v1/events" params $ \val -> do
    -- Decode once, share between normalize + summary (avoids walking
    -- @logsData@ twice on large responses).
    let enriched =
          maybe val (\d -> (if opts.summary then withTraceSummary d else Relude.id) (normalizeDecoded d Nothing)) (decodeEvents val)
    renderWith mode enriched (renderEventsHuman fmt Nothing val)


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
  let
    -- Pre-resolve column indices once instead of Map.lookup'ing per cell
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
      [ TraceSummary tid (sort (filter (not . T.null) (S.toList svcs))) n errs
      | (tid, (svcs, n, errs)) <- Map.toList groups
      ]
   in
    AE.Object (KM.insert "traces" (AE.toJSON traces) out)
withTraceSummary _ v = v


-- | Per-trace breakdown for 'events context --summary'.
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
      includeAttributes = maybe False (any ((\field -> field == "attributes" || "attributes." `T.isPrefixOf` field) . T.strip) . T.splitOn ",") opts.fields
      sinceParam = fromMaybe (if isJust opts.from || isJust opts.to then "" else "1H") opts.since
   in catMaybes
        [ if T.null q then Nothing else Just ("query", q)
        , Just ("since", sinceParam)
        , ("from",) <$> opts.from
        , ("to",) <$> opts.to
        , ("source",) <$> opts.kind
        , ("limit",) . show <$> opts.limit
        , ("cursor",) <$> opts.cursor
        , ("with_children", "true") <$ guard opts.withChildren
        , ("include_attributes", "true") <$ guard includeAttributes
        ]


-- | Fold --service/--level CLI flags into a KQL query string.
--
-- The platform's KQL parser uses '==' for equality with double-quoted strings
-- (see 'Pkg.Parser.Expr.pTerm'). The CLI previously emitted ':' which the
-- server rejected with HTTP 400 — leaving --service and --level effectively
-- broken. Filters are AND-combined with the user's positional query.
--
-- @--service@ is repeatable (D2). One value uses @==@; two or more become
-- @resource.service.name in (\"a\", \"b\")@ (the parser supports @in@; see
-- 'Pkg.Parser.Expr.pTerm').
--
-- A bareword/phrase query (no KQL operators) is rewritten to a full-text
-- search via 'rewriteBareQuery' — @POISON_ROW_DROPPED@ becomes
-- @body has "POISON_ROW_DROPPED" or summary has "POISON_ROW_DROPPED"@,
-- matching what a developer naturally types.
--
-- --level normalizes to upper-case (B2) so @--level error@ and @--level
-- ERROR@ both match the canonical OTel severity strings.
--
-- >>> foldFiltersIntoQuery "" [] Nothing
-- ""
-- >>> foldFiltersIntoQuery "errors > 0" [] Nothing
-- "errors > 0"
-- >>> foldFiltersIntoQuery "" ["web"] Nothing
-- "resource.service.name==\"web\""
-- >>> foldFiltersIntoQuery "" ["web", "worker"] Nothing
-- "resource.service.name in (\"web\", \"worker\")"
-- >>> foldFiltersIntoQuery "" [] (Just "warn")
-- "severity.text==\"WARN\""
-- >>> foldFiltersIntoQuery "errors > 0" ["web"] (Just "error")
-- "resource.service.name==\"web\" and severity.text==\"ERROR\" and (errors > 0)"
-- >>> foldFiltersIntoQuery "POISON_ROW_DROPPED" [] Nothing
-- "body has \"POISON_ROW_DROPPED\" or summary has \"POISON_ROW_DROPPED\""
foldFiltersIntoQuery :: Text -> [Text] -> Maybe Text -> Text
foldFiltersIntoQuery query services mLevel
  | T.null prefix = rewritten
  | T.null rewritten = prefix
  | otherwise = prefix <> " and (" <> rewritten <> ")"
  where
    q v = "\"" <> T.replace "\"" "\\\"" v <> "\""
    eq field val = field <> "==" <> q val
    serviceFilter = case services of
      [] -> Nothing
      [s] -> Just (eq "resource.service.name" s)
      ss -> Just ("resource.service.name in (" <> T.intercalate ", " (map q ss) <> ")")
    filters = catMaybes [serviceFilter, eq "severity.text" . T.toUpper <$> mLevel]
    prefix = T.intercalate " and " filters
    rewritten = rewriteBareQuery (T.strip query)


-- | When the user types a single bareword (or quoted phrase) without any KQL
-- operator, treat it as a full-text search across @body@ and @summary@. The
-- previous behaviour parsed the bareword as a column reference, producing a
-- useless @column "X" does not exist@ error.
--
-- A query containing any of @== != >= <= > < has and or " (@ is passed
-- through unchanged.
--
-- >>> rewriteBareQuery ""
-- ""
-- >>> rewriteBareQuery "POISON_ROW_DROPPED"
-- "body has \"POISON_ROW_DROPPED\" or summary has \"POISON_ROW_DROPPED\""
-- >>> rewriteBareQuery "errors > 0"
-- "errors > 0"
-- >>> rewriteBareQuery "context.trace_id==\"abc\""
-- "context.trace_id==\"abc\""
rewriteBareQuery :: Text -> Text
rewriteBareQuery t
  | T.null t = t
  | hasOperator t = t
  | otherwise =
      let escaped = T.replace "\"" "\\\"" t
       in "body has \"" <> escaped <> "\" or summary has \"" <> escaped <> "\""
  where
    -- Operators / grouping disable the rewrite; multi-word phrases ("foo
    -- bar") are still rewritten so a phrase search Just Works.
    hasOperator x =
      any (`T.isInfixOf` x) ["==", "!=", ">=", "<=", " has ", " and ", " or "]
        || T.any (`elem` ("><\"(" :: [Char])) x


-- | Single decode of the events envelope. Sharing the parsed @colIdxMap@
-- + @logsData@ between 'normalizeEventsResponse' and 'withTraceSummary'
-- avoids walking the rows array twice for large payloads ('events context
-- --summary' was the worst offender). 'normalizeEventsResponse' projects
-- the positional row representation into named-field objects so agents/jq
-- consumers don't need to thread 'colIdxMap' themselves; the wire shape is
-- stable: @{events: [...], count, has_more, cursor}@.
data DecodedEvents = DecodedEvents
  { idxMap :: Map Text Int
  , rawRows :: [[AE.Value]]
  -- ^ Cells with original JSON types preserved — feeds the JSON output path
  -- so numbers/bools/arrays don't get stringified.
  , rows :: [[Text]]
  -- ^ Text projection of 'rawRows' used by table rendering and trace summary.
  , count :: Int
  , hasMore :: AE.Value
  , cursor :: AE.Value
  }


decodeEvents :: AE.Value -> Maybe DecodedEvents
decodeEvents (AE.Object o) =
  let raw = extractRawRows (KM.lookup "logsData" o)
   in Just
        DecodedEvents
          { idxMap = extractColIdxMap (KM.lookup "colIdxMap" o)
          , rawRows = raw
          , rows = map (map valToText) raw
          , count = extractInt (KM.lookup "count" o)
          , hasMore = fromMaybe AE.Null (KM.lookup "hasMore" o)
          , cursor = fromMaybe AE.Null (KM.lookup "cursor" o)
          }
decodeEvents _ = Nothing


normalizeEventsResponse :: AE.Value -> Maybe Text -> AE.Value
normalizeEventsResponse val mFields = maybe val (`normalizeDecoded` mFields) (decodeEvents val)


-- | Project rows into named-field objects, preserving JSON types from the
-- raw response (numbers stay numbers, bools stay bools, the @summary@ array
-- stays an array). Empty strings collapse to @null@ so agents can tell
-- "field absent" from "field present with a value" without a special case
-- per column. The @summary@/@latency_breakdown@ columns are run through
-- 'cleanSummaryValue' to strip @field;style⇒value@ markup.
normalizeDecoded :: DecodedEvents -> Maybe Text -> AE.Value
normalizeDecoded d mFields =
  let keep = case mFields of
        Nothing -> Map.keys d.idxMap
        Just s -> filter (`Map.member` d.idxMap) (T.splitOn "," s)
      cellAt r k = Map.lookup k d.idxMap >>= \i -> listToMaybe (drop i r)
      cleanCell k v
        | k == "summary" || k == "latency_breakdown" = cleanSummaryValue v
        | AE.String "" <- v = AE.Null
        | otherwise = v
      toObj r =
        AE.object
          [ AK.fromText k AE..= maybe AE.Null (cleanCell k) (cellAt r k)
          | k <- keep
          ]
      events = map toObj d.rawRows
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
        -- column name paired with its row index, so header and cell stay aligned
        picked = [(c, i) | c <- filteredCols, Just i <- [Map.lookup c idxMap]]
        cell c i r =
          let v = fromMaybe "" $ listToMaybe (drop i r)
           in if c `elem` (["summary", "latency_breakdown"] :: [Text]) then renderSummaryCell v else v
    renderTable (map fst picked) [[cell c i r | (c, i) <- picked] | r <- rows]
    putTextLn $ "\n" <> show count <> " results"
  _ -> renderJSON val


-- | Resolve @--format@, exiting with the accepted values on a typo rather than
-- silently falling back.
resolveFormat :: IOE :> es => Maybe Text -> Eff es LogFormat
resolveFormat = maybe (pure FmtLine) (either die' pure . parseLogFormat)
  where
    die' e = printError e >> liftIO exitFailure


-- | Human-facing rendering of an events envelope. The line and logfmt forms
-- read the rows through 'CLI.LogView.eventRows'; the table form keeps the
-- existing column layout, which is what aggregate queries want.
renderEventsHuman :: (Environment :> es, IOE :> es) => LogFormat -> Maybe Text -> AE.Value -> Eff es ()
renderEventsHuman fmt mFields val = case fmt of
  FmtTable -> renderEventsTable val mFields
  FmtLogfmt -> mapM_ (putTextLn . renderLogfmt) (eventRows val)
  FmtLine -> do
    (w, color) <- chartCanvas
    let rows = eventRows val
    -- A query that aggregates has no per-event columns to lay out; fall back to
    -- the table rather than printing a column of blanks.
    if null rows then renderEventsTable val mFields else mapM_ (putTextLn . renderEventLine color w) rows


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


-- | The trace as a waterfall. Under @--json@ the caller renders the envelope
-- instead; this is only the terminal form.
renderTraceTree :: (Environment :> es, IOE :> es) => AE.Value -> Eff es ()
renderTraceTree val = do
  (w, color) <- chartCanvas
  mapM_ putTextLn (renderWaterfall color w (eventRows val))


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


runMetricsChart :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> MetricsChartOpts -> OutputMode -> Eff es ()
runMetricsChart cfg opts =
  runChart cfg ChartCmdOpts{query = opts.expression, since = opts.since, from = opts.from, to = opts.to, watch = opts.watch, chartType = Nothing, source = Nothing, height = Nothing}


-- | Ad-hoc charting of any KQL query, and the engine behind @metrics chart@.
data ChartCmdOpts = ChartCmdOpts
  { query :: Text
  , since :: Maybe Text
  , from :: Maybe Text
  , to :: Maybe Text
  , source :: Maybe Text
  , chartType :: Maybe Text
  -- ^ @line@ | @bar@ | @stat@ | @table@. Inferred from the result shape when
  -- absent: a timestamp column charts as a line, anything else as bars.
  , height :: Maybe Int
  , watch :: Maybe Text
  }
  deriving stock (Show)


runChart :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> ChartCmdOpts -> OutputMode -> Eff es ()
runChart cfg opts mode = do
  -- Validate --watch up front so a typo doesn't silently fall back to 5s.
  validateDurationOrDie "--watch" opts.watch
  validateDurationOrDie "--since" opts.since
  let params = metricsParams opts.query opts.since opts.from opts.to <> maybe [] (\s -> [("source", s)]) opts.source
      run = withAPIResult cfg "/api/v1/metrics" params \val ->
        withMetricsData val (renderJSON val) \md -> do
          renderWith mode val (drawMetrics opts md)
          -- A failed query comes back as an empty result set with `error` set.
          -- Reporting that as "no data in range" would be a lie — the range may
          -- be full of data — and a script that only checked the exit code
          -- would carry on as if the number were real. Fail in every mode; the
          -- JSON envelope (already printed) carries the reason.
          whenJust md.error \e -> printError ("query failed: " <> e) >> liftIO exitFailure
  repeatEvery opts.watch run


-- | Redraw on a timer, clearing the screen between frames. A missing interval
-- runs the action exactly once.
repeatEvery :: IOE :> es => Maybe Text -> Eff es () -> Eff es ()
repeatEvery Nothing run = run
repeatEvery (Just interval) run = forever $ do
  liftIO $ putStr ("\ESC[2J\ESC[H" :: String)
  run
  threadDelay (parseDurationMs interval * 1000)


drawMetrics :: (Environment :> es, IOE :> es) => ChartCmdOpts -> MetricsData -> Eff es ()
drawMetrics opts md = do
  (w, color) <- chartCanvas
  let series = Chart.seriesFromMetrics md
      chartOpts = Chart.defaultChartOpts{Chart.width = w, Chart.height = fromMaybe 14 opts.height, Chart.colorful = color}
      kind = fromMaybe (if isTimeseries md then "line" else "bar") opts.chartType
  mapM_ putTextLn case kind of
    "stat" -> Chart.renderStat chartOpts opts.query md.dataFloat series
    "bar" -> Chart.renderBars chartOpts (labelledRows md)
    "table" -> []
    _ -> Chart.renderTimeseries chartOpts series
  when (kind == "table") $ renderMetricsTable md


-- | A leading timestamp column is what makes a result a timeseries; the server
-- emits it whenever the query binned by time.
isTimeseries :: MetricsData -> Bool
isTimeseries md = maybe False (\h -> T.toLower h `elem` ["timestamp", "created_at", "time", "bucket"]) (md.headers V.!? 0)


-- | @(label, value)@ pairs for a bar chart: first column labels, second sizes.
labelledRows :: MetricsData -> [(Text, Double)]
labelledRows md =
  [ (fromMaybe "" (r V.!? 0), fromMaybe 0 (readMaybe . toString =<< r V.!? 1))
  | r <- V.toList md.dataText
  ]


-- | Width and colour for a chart: the real terminal width when we have a TTY,
-- a fixed 100 columns when piped (so redirected output is reproducible).
chartCanvas :: (Environment :> es, IOE :> es) => Eff es (Int, Bool)
chartCanvas = do
  tty <- isInteractiveTTY
  json <- isJsonOutput
  w <- liftIO termWidth
  pure (max 40 w, tty && not json)


newtype StatusOpts = StatusOpts {since :: Maybe Text}
  deriving stock (Show)


-- | One screen answering "is anything wrong right now": throughput and error
-- rate over the window, the noisiest services, open issues and any monitor that
-- is currently alerting. It is the first thing you want after `ssh`-ing into a
-- terminal, and it is four requests rather than four commands.
runStatus :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> StatusOpts -> OutputMode -> Eff es ()
runStatus cfg opts mode = do
  validateDurationOrDie "--since" opts.since
  let since = fromMaybe "1h" opts.since
      -- One panel failing shouldn't take the overview down, but it must not
      -- look like "nothing happened" either: the reason is reported inline.
      metric q =
        apiGetJson @_ @MetricsData cfg "/api/v1/metrics" (metricsParams q (Just since) Nothing Nothing) >>= \case
          Left err -> def{error = Just (renderAPIError err)} <$ pass
          Right md -> pure md
  volume <- metric "summarize count(*) by bin_auto(timestamp)"
  errors <- metric "summarize count(*) by bin_auto(timestamp) | where errors == true"
  byService <- metric "summarize count(*) by resource.service.name | sort by count_ desc | take 8"
  issues <- either (const AE.Null) Relude.id <$> apiGetJson @_ @AE.Value cfg "/api/v1/issues" [("status", "open"), ("per_page", "5")]
  monitors <- either (const AE.Null) Relude.id <$> apiGetJson @_ @AE.Value cfg "/api/v1/monitors" []
  let summary =
        AE.object
          [ "since" AE..= since
          , "events" AE..= sum (mapMaybe (join . (V.!? 1)) (V.toList volume.dataset))
          , "errors" AE..= sum (mapMaybe (join . (V.!? 1)) (V.toList errors.dataset))
          , -- \^.. not ^?: `length` on a Maybe counts the Just, not the array.
            "open_issues" AE..= length (issues ^.. AL.key "data" . AL._Array . traverse)
          , "alerting_monitors" AE..= alertingMonitors monitors
          ]
  renderWith mode summary do
    (w, color) <- chartCanvas
    let chartOpts = Chart.defaultChartOpts{Chart.width = w, Chart.height = 8, Chart.colorful = color}
        section t = putTextLn "" >> putTextLn (Chart.bold color t)
        panel title md draw = do
          section title
          maybe (mapM_ putTextLn (draw md)) (\e -> putTextLn (Chart.colorize color (Chart.seriesColor 6) ("  unavailable: " <> e))) md.error
    panel ("Events  ·  last " <> since) volume (Chart.renderTimeseries chartOpts . Chart.seriesFromMetrics)
    panel "Errors" errors (Chart.renderTimeseries chartOpts{Chart.height = 5} . Chart.seriesFromMetrics)
    panel "Busiest services" byService (Chart.renderBars chartOpts . labelledRows)
    section "Open issues"
    putTextLn $ case issues ^.. AL.key "data" . AL._Array . traverse . AL.key "title" . AL._String of
      [] -> Chart.dim color "  none"
      ts -> T.unlines ["  " <> Chart.ellipsize (w - 2) t | t <- take 5 ts]
    let alerting = alertingMonitors monitors
    section "Monitors"
    putTextLn
      $ if null alerting
        then Chart.dim color "  all quiet"
        else T.unlines ["  " <> Chart.colorize color (Chart.seriesColor 6) ("ALERTING  " <> t) | t <- alerting]


-- | Titles of monitors currently in an alerting state.
alertingMonitors :: AE.Value -> [Text]
alertingMonitors v =
  [ t
  | m <- v ^.. AL._Array . traverse
  , (m ^? AL.key "alert_state" . AL._String) `elem` [Just "alerting", Just "warning"]
  , Just t <- [m ^? AL.key "title" . AL._String]
  ]


-- | Somewhere in the web UI worth linking to. Enumerated rather than taking a
-- free-form path so a typo is a parse error and the URL shapes live in one
-- place ('uiPath').
data OpenTarget = OpenLogs | OpenTrace | OpenIssue | OpenDashboard | OpenMonitors | OpenEndpoints | OpenProject
  deriving stock (Bounded, Enum, Eq, Show)


-- | >>> map parseOpenTarget ["logs", "trace", "nope"]
-- [Just OpenLogs,Just OpenTrace,Nothing]
parseOpenTarget :: Text -> Maybe OpenTarget
parseOpenTarget = \case
  "logs" -> Just OpenLogs
  "trace" -> Just OpenTrace
  "issue" -> Just OpenIssue
  "dashboard" -> Just OpenDashboard
  "monitors" -> Just OpenMonitors
  "endpoints" -> Just OpenEndpoints
  "project" -> Just OpenProject
  _ -> Nothing


openTargetNames :: Text
openTargetNames = T.intercalate "|" [t | tgt <- universe, let t = targetName tgt]
  where
    targetName = \case
      OpenLogs -> "logs"
      OpenTrace -> "trace"
      OpenIssue -> "issue"
      OpenDashboard -> "dashboard"
      OpenMonitors -> "monitors"
      OpenEndpoints -> "endpoints"
      OpenProject -> "project"


-- | Path (with query string) for a target, relative to the deployment's host.
--
-- >>> uiPath "PID" OpenDashboard (Just "d1") Nothing
-- "/p/PID/dashboards/d1"
-- >>> uiPath "PID" OpenTrace (Just "abc") Nothing
-- "/p/PID/log_explorer?query=trace_id%3D%3D%22abc%22&showTrace=abc"
-- >>> uiPath "PID" OpenLogs Nothing (Just "24h")
-- "/p/PID/log_explorer?since=24h"
uiPath :: Text -> OpenTarget -> Maybe Text -> Maybe Text -> Text
uiPath pid target argM sinceM =
  "/p/" <> pid <> case target of
    OpenProject -> ""
    OpenMonitors -> "/monitors" <> qs []
    OpenEndpoints -> "/api_catalog" <> qs []
    OpenIssue -> "/issues" <> maybe "" ("/" <>) argM
    OpenDashboard -> "/dashboards" <> maybe "" ("/" <>) argM
    OpenLogs -> "/log_explorer" <> qs (foldMap (\q -> [("query", q)]) argM)
    OpenTrace -> "/log_explorer" <> qs (foldMap (\t -> [("query", "trace_id==\"" <> t <> "\""), ("showTrace", t)]) argM)
  where
    qs extra = case extra <> foldMap (\v -> [("since", v)]) sinceM of
      [] -> ""
      ps -> "?" <> T.intercalate "&" [k <> "=" <> urlEncode v | (k, v) <- ps]


-- | Percent-encode everything outside the unreserved set. Deliberately
-- conservative — these strings end up in a URL handed to a browser, and KQL is
-- full of quotes, spaces and comparison operators.
--
-- >>> urlEncode "trace_id==\"a b\""
-- "trace_id%3D%3D%22a%20b%22"
urlEncode :: Text -> Text
urlEncode = T.concatMap \c ->
  if isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("-_.~" :: [Char])
    then one c
    else T.concat ["%" <> T.justifyRight 2 '0' (T.toUpper (toText (showHex b ""))) | b <- BS.unpack (encodeUtf8 (one c :: Text))]


data OpenOpts = OpenOpts
  { target :: Text
  , arg :: Maybe Text
  , since :: Maybe Text
  , printOnly :: Bool
  }
  deriving stock (Show)


-- | Print (and, unless @--print@, launch) the web UI link for a resource. The
-- host comes from @\/api\/v1\/me@ rather than being derived from the API URL:
-- the two can differ, and a self-hosted install can put the UI anywhere.
runOpen :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> OpenOpts -> Eff es ()
runOpen cfg opts = do
  target <-
    parseOpenTarget opts.target `whenNothing` do
      printError $ "unknown target '" <> opts.target <> "'; expected one of: " <> openTargetNames
      liftIO exitFailure
  withAPIResult cfg "/api/v1/me" [] \val -> case AE.fromJSON @MeInfo val of
    AE.Error msg -> printError ("could not read /api/v1/me: " <> toText msg) >> liftIO exitFailure
    AE.Success me -> do
      let url = T.dropWhileEnd (== '/') me.hostUrl <> uiPath me.projectId target opts.arg opts.since
      putTextLn url
      unless opts.printOnly $ liftIO (tryOpenBrowser (toString url))


-- | Just the fields of @/api/v1/me@ the CLI uses.
data MeInfo = MeInfo {projectId :: Text, hostUrl :: Text}
  deriving stock (Generic)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] MeInfo


data DashboardRenderOpts = DashboardRenderOpts
  { dashboardId :: Text
  , tab :: Maybe Text
  , widget :: Maybe Text
  , since :: Maybe Text
  , from :: Maybe Text
  , to :: Maybe Text
  , vars :: [(Text, Text)]
  -- ^ @--var key=value@ pairs, forwarded as the @var-*@ params the dashboard's
  -- own variables read.
  , watch :: Maybe Text
  }
  deriving stock (Show)


-- | Draw a whole dashboard — every widget, in its grid position — in the
-- terminal. Under @--json@ the resolved payload is emitted instead, which is
-- the form an agent wants to reason over.
runDashboardRender :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> DashboardRenderOpts -> OutputMode -> Eff es ()
runDashboardRender cfg opts mode = do
  validateDurationOrDie "--watch" opts.watch
  validateDurationOrDie "--since" opts.since
  let params =
        catMaybes
          [ ("tab",) <$> opts.tab
          , ("widget",) <$> opts.widget
          , Just ("since", fromMaybe "1h" opts.since)
          , ("from",) <$> opts.from
          , ("to",) <$> opts.to
          ]
          <> [("var-" <> k, v) | (k, v) <- opts.vars]
  repeatEvery opts.watch $ withAPIResult cfg ("/api/v1/dashboards/" <> opts.dashboardId <> "/data") params \val ->
    case (mode, AE.fromJSON @Dash.DashboardData val) of
      (OutputTable, AE.Success d) -> do
        (w, color) <- chartCanvas
        mapM_ putTextLn (Dash.renderDashboard color w d)
      (_, AE.Success _) -> renderJSON val
      (_, AE.Error msg) -> printDebug ("dashboard decode failed: " <> toText msg) >> renderJSON val


metricsParams :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Text)]
metricsParams expr mSince mFrom mTo =
  catMaybes
    [ Just ("query", expr)
    , Just ("since", fromMaybe "1h" mSince)
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
parseDurationMs (T.toLower . T.strip -> t)
  | Just n <- T.stripSuffix "ms" t = fromMaybe 5000 (readMaybe $ toString n)
  | otherwise = maybe 5000 (* 1000) (parseDurationSecs t)


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
  r <- apiPostUnauth baseUrl "/api/device/token" [("device_code", deviceCode)]
  case first show r >>= AE.eitherDecode @DeviceTokenResponse of
    Right resp | resp.err /= Just "authorization_pending" -> pure $ guarded (isJust . (.sessionId)) resp
    -- transient error / undecodable body / still pending: keep polling
    _ -> pollForToken baseUrl deviceCode (remaining - 1)


tryOpenBrowser :: String -> IO ()
tryOpenBrowser url =
  void
    $ tryAny
    $ void (spawnProcess "open" [url])
    `catch` \(_ :: SomeException) ->
      void $ spawnProcess "xdg-open" [url]


-- OTel send helpers

-- | Point the OTel SDK at the configured endpoint/service/resources and run
-- @act@ with a CLI tracer, tearing the provider down on any exit path.
withCliTracer :: CLIConfig -> Text -> [(Text, Text)] -> (Tracer -> IO a) -> IO a
withCliTracer cfg service resources act = do
  -- The standard OTel env var wins over the API-URL-derived endpoint, so a
  -- collector on a non-default port (or a test server) can be targeted.
  whenNothingM_ (lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT")
    $ setEnv "OTEL_EXPORTER_OTLP_ENDPOINT" (toString (otlpFromApiUrl cfg.apiUrl))
  setEnv "OTEL_SERVICE_NAME" (toString service)
  whenJust cfg.apiKey $ \k ->
    setEnv "OTEL_EXPORTER_OTLP_HEADERS" ("x-api-key=" <> toString k)
  unless (null resources)
    $ setEnv "OTEL_RESOURCE_ATTRIBUTES"
    $ toString
    $ T.intercalate "," [k <> "=" <> v | (k, v) <- resources]
  bracket initializeGlobalTracerProvider (`shutdownTracerProvider` Nothing)
    $ \tp -> act (makeTracer tp "monoscope-cli" (TracerOptions Nothing []))


data SendEventOpts = SendEventOpts
  { messages :: [Text]
  , kind :: Text
  , level :: Text
  , service :: Text
  , tags :: [(Text, Text)]
  , extras :: [(Text, Text)]
  , resources :: [(Text, Text)]
  }
  deriving stock (Show)


-- | Parse "KEY:VALUE" pairs for --tag / --extra flags.
parseKV :: String -> Either String (Text, Text)
parseKV = parseSepKV ':' "KEY:VALUE"


-- | Split a flag argument on its first separator. Attributes use @:@ and
-- dashboard variables use @=@; both need the same "only the first one splits"
-- rule so a value may itself contain the separator.
--
-- >>> parseSepKV '=' "KEY=VALUE" "service=checkout-api"
-- Right ("service","checkout-api")
-- >>> parseSepKV ':' "KEY:VALUE" "url:https://x.dev/a"
-- Right ("url","https://x.dev/a")
-- >>> parseSepKV '=' "KEY=VALUE" "novalue"
-- Left "expected KEY=VALUE, got: novalue"
parseSepKV :: Char -> String -> String -> Either String (Text, Text)
parseSepKV sep label s = case T.break (== sep) (toText s) of
  (k, rest) | not (T.null rest) -> Right (k, T.drop 1 rest)
  _ -> Left $ "expected " <> label <> ", got: " <> s


runSendEvent :: IOE :> es => CLIConfig -> SendEventOpts -> Eff es ()
runSendEvent cfg opts = liftIO $ do
  let msg = T.intercalate "\n" opts.messages
      isError = opts.level == "error" || opts.kind == "error"
      attrs =
        HM.fromList
          $ [ ("log.message", OA.toAttribute msg)
            , ("log.severity", OA.toAttribute opts.level)
            , ("event.kind", OA.toAttribute opts.kind)
            ]
          <> map (second OA.toAttribute) (opts.tags <> opts.extras)
  withCliTracer cfg opts.service opts.resources $ \tracer -> do
    ctx <- OtelCtx.getContext
    sp <- Trace.createSpan tracer ctx (T.take 200 msg) (defaultSpanArguments{Trace.attributes = attrs})
    when isError $ Trace.setStatus sp (Error msg)
    Trace.endSpan sp Nothing
  putTextLn "Event sent."


data TelemetryGenOpts = TelemetryGenOpts
  { kind :: Text
  , rate :: Double
  , count :: Maybe Int
  , service :: Text
  , resources :: [(Text, Text)]
  }
  deriving stock (Show)


-- | Derive the OTLP gRPC endpoint from the CLI's configured API URL.
-- Strips the port (if any) and appends :4317 (the default OTLP gRPC port).
--
-- >>> otlpFromApiUrl "https://api.monoscope.tech"
-- "https://api.monoscope.tech:4317"
-- >>> otlpFromApiUrl "http://localhost:8080"
-- "http://localhost:4317"
otlpFromApiUrl :: Text -> Text
otlpFromApiUrl apiUrl =
  let scheme = if "https" `T.isPrefixOf` apiUrl then "https" else "http"
      rest = fromMaybe apiUrl (T.stripPrefix (scheme <> "://") apiUrl)
      host = T.takeWhile (\c -> c /= ':' && c /= '/') rest
   in scheme <> "://" <> host <> ":4317"


runTelemetryGen :: IOE :> es => CLIConfig -> TelemetryGenOpts -> Eff es ()
runTelemetryGen cfg opts = liftIO $ do
  let delayUs = round (1_000_000 / opts.rate) :: Int
  putTextLn $ "Generating " <> opts.kind <> " → " <> otlpFromApiUrl cfg.apiUrl <> " at " <> show opts.rate <> "/s"
  withCliTracer cfg opts.service opts.resources $ \tracer -> do
    let sendOne i = do
          ctx <- OtelCtx.getContext
          Trace.createSpan tracer ctx ("telemetrygen." <> opts.kind) defaultSpanArguments >>= (`Trace.endSpan` Nothing)
          putStrLn $ "Sent " <> show (i :: Int) <> " " <> toString opts.kind <> "(s)"
          threadDelay delayUs
    forM_ (maybe [1 ..] (enumFromTo 1) opts.count) sendOne
