-- | Generic Plan A resource commands for monitors, dashboards, api-keys.
--
-- These wrap the v1 JSON endpoints. Each subcommand is a thin shell around
-- @apiGetJson@/@apiPostJson@/@apiDelete@ from "CLI.Core" — no command-specific
-- parsing or formatting beyond picking the right URL segment.
module CLI.Resource (
  ResourceKind (..),
  WriteVerb (..),
  runList,
  runListVia,
  renderListPayload,
  runGet,
  runDelete,
  runLifecycle,
  runBulk,
  runFromFile,
  writeJson,
  writeJsonRaw,
  readYamlOrJson,
  runApplyResource,
  runYamlDump,
  resourcePath,
  resourceIdPath,
  withResult,
  runAPI,
) where

import Relude

import CLI.Config (CLIConfig (..))
import CLI.Core (APIError, OutputMode (..), apiDelete, apiGetJson, apiPatchJson, apiPostJson, apiPutJson, printError, renderAPIError, renderByMode, renderWith)
import CLI.Table (Align (..), CellStyle (..), RichTableOpts (..), brand, defaultRichTableOpts, level, muted, numeric, paginationFooter, parseSeverity, plain, renderRichTableWith)
import Control.Lens (has)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AEKM
import Data.Aeson.Lens qualified as AL
import Data.Effectful.Wreq (HTTP)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Effectful
import Effectful.Environment (Environment)
import Effectful.FileSystem (FileSystem, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Web.ApiTypes (Paged (..))


data ResourceKind
  = Monitors
  | Dashboards
  | ApiKeys
  | Teams
  | Members
  | -- | Listing-only kinds; used by 'renderListPayload' / 'runListVia' to pick
    -- a column set for resources whose CRUD lives outside this module
    -- (issues, endpoints, log-patterns). They don't appear in 'resourcePath'.
    Issues
  | Endpoints
  | LogPatterns
  deriving stock (Eq, Show)


-- | Write verb for JSON bodies sent via 'writeJson' / 'runFromFile'.
-- Kept as a small enum (not 'Network.HTTP.Types.StdMethod') so the
-- surface of 'writeJson' only admits the three verbs the CLI needs.
data WriteVerb = POST | PUT | PATCH
  deriving stock (Eq, Show)


-- | URL prefix for a given kind under /api/v1.
-- Issues / Endpoints / LogPatterns are list-only here (the call sites in
-- Main.hs hit their own URLs); 'resourcePath' returns "" for them so a stray
-- lookup is harmless rather than crashing the CLI.
resourcePath :: ResourceKind -> Text
resourcePath =
  ("/api/v1" <>) . \case
    Monitors -> "/monitors"
    Dashboards -> "/dashboards"
    ApiKeys -> "/api_keys"
    Teams -> "/teams"
    Members -> "/members"
    Issues -> "/issues"
    Endpoints -> "/endpoints"
    LogPatterns -> "/log_patterns"


-- | @resourceIdPath k id@ → @"/<kind>/<id>"@.
resourceIdPath :: ResourceKind -> Text -> Text
resourceIdPath k i = resourcePath k <> "/" <> i


-- | Run an API-producing action, exit with `printError` on Left, call
-- @onOk@ on Right. Used to dispatch API responses uniformly across
-- command handlers.
withResult :: IOE :> es => Eff es (Either e a) -> (e -> Text) -> (a -> Eff es ()) -> Eff es ()
withResult act renderErr onOk =
  act >>= \case
    Left e -> printError (renderErr e) >> liftIO exitFailure
    Right v -> onOk v


-- | Specialised `withResult` for API calls returning `APIError`, with the
-- common "render result with OutputMode, no table variant" continuation.
runAPI :: (AE.ToJSON a, IOE :> es) => OutputMode -> Eff es (Either APIError a) -> Eff es ()
runAPI mode act = withResult act renderAPIError (renderByMode mode Nothing)


-- | Every list endpoint returns the same shape from the CLI's perspective:
-- @{data: [...], pagination: {has_more, total, cursor}}@. The server still
-- returns its native envelope (bare arrays for some resources, @Paged@ for
-- others); the normalisation lives here so agents see one shape across
-- @issues list@, @monitors list@, @dashboards list@, @api-keys list@,
-- @teams list@, @members list@, @endpoints list@, @log-patterns list@.
--
-- In table mode the per-resource column set in 'buildResourceTable' decides
-- which fields show, plus a "… N more" pagination cue below.
runList :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> ResourceKind -> [(Text, Text)] -> OutputMode -> Eff es ()
runList cfg k = runListVia cfg k (resourcePath k)


-- | Fetch + normalise a list endpoint at an explicit @url@ (which may differ
-- from 'resourcePath', e.g. issues hits the legacy @apiGetJson \@_ \@AE.Value@
-- flow from Main.hs with custom query params). 'runList' delegates here with
-- @resourcePath k@. Server envelope shapes we don't recognise go to always-on
-- stderr (not printDebug): an agent expecting {data, pagination} would
-- silently get a broken shape otherwise.
runListVia :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> ResourceKind -> Text -> [(Text, Text)] -> OutputMode -> Eff es ()
runListVia cfg k url params mode =
  apiGetJson @_ @AE.Value cfg url params >>= \case
    Left e -> printError (renderAPIError e) >> liftIO exitFailure
    Right v -> case normalizeListE v of
      Right normalised -> renderListPayload k mode normalised
      Left (raw, reason) -> do
        printError $ "list " <> url <> ": " <> reason
        renderByMode mode Nothing raw


-- | Render a normalised list payload — JSON/YAML pass through to the encoder;
-- 'OutputTable' delegates to per-resource column builders below.
renderListPayload :: IOE :> es => ResourceKind -> OutputMode -> AE.Value -> Eff es ()
renderListPayload k mode payload =
  renderWith mode payload $ do
    let items = case payload of
          AE.Object o -> case AEKM.lookup "data" o of
            Just (AE.Array xs) -> toList xs
            _ -> []
          _ -> []
        (headers, rows) = buildResourceTable k items
        rightAligned = headerIsNumeric <$> headers
        opts = defaultRichTableOpts{alignments = rightAligned}
    renderRichTableWith opts headers rows
    case payload of
      AE.Object o
        | Just (AE.Object p) <- AEKM.lookup "pagination" o
        , Just (AE.Bool True) <- AEKM.lookup "has_more" p ->
            paginationFooter "… more — pass --limit / --page or use --json to see the cursor"
      _ -> pass
  where
    headerIsNumeric h
      | T.toLower h `elem` ["count", "events", "widgets", "seen", "first_seen", "last_seen"] = AlignRight
      | otherwise = AlignLeft


-- | Per-resource column set. Keep the default narrow (5-7 columns) so the
-- TITLE-ish column has room to breathe on a typical 120-col terminal.
-- Unknown resource kinds fall back to a key/value dump of the first object.
buildResourceTable :: ResourceKind -> [AE.Value] -> ([Text], [[(CellStyle, Text)]])
buildResourceTable kind items = case kind of
  Monitors ->
    ( ["state", "name", "condition", "last_triggered"]
    , [ [ pickLevelCell o ["state", "status"]
        , brand (lookupText o "name")
        , plain (lookupText o "condition")
        , muted (lookupText o "last_triggered_at")
        ]
      | AE.Object o <- items
      ]
    )
  Dashboards ->
    ( ["name", "widgets", "updated"]
    , [ [ brand (lookupText o "name")
        , numeric (lookupText o "widget_count")
        , muted (lookupText o "updated_at")
        ]
      | AE.Object o <- items
      ]
    )
  ApiKeys ->
    ( ["title", "status", "last_used"]
    , [ [ brand (lookupText o "title")
        , pickStatus o
        , muted (lookupText o "last_used_at")
        ]
      | AE.Object o <- items
      ]
    )
  Teams ->
    ( ["name", "members", "created"]
    , [ [ brand (lookupText o "name")
        , numeric (lookupText o "member_count")
        , muted (lookupText o "created_at")
        ]
      | AE.Object o <- items
      ]
    )
  Members ->
    ( ["user", "email", "permission"]
    , [ [ plain (lookupText o "name")
        , brand (lookupText o "email")
        , plain (lookupText o "permission")
        ]
      | AE.Object o <- items
      ]
    )
  Issues ->
    ( ["level", "id", "count", "seen", "title"]
    , [ [ pickLevelCell o ["severity", "level"]
        , brand (firstNonEmpty [lookupText o "short_id", lookupText o "id"])
        , numeric (lookupText o "event_count")
        , muted (firstNonEmpty [lookupText o "last_seen", lookupText o "updated_at"])
        , plain (firstNonEmpty [lookupText o "title", lookupText o "issue_type"])
        ]
      | AE.Object o <- items
      ]
    )
  Endpoints ->
    ( ["method", "host", "url_path", "service"]
    , [ [ plain (lookupText o "method")
        , brand (lookupText o "host")
        , plain (lookupText o "url_path")
        , muted (lookupText o "service")
        ]
      | AE.Object o <- items
      ]
    )
  LogPatterns ->
    ( ["count", "last", "pattern"]
    , [ [ numeric (firstNonEmpty [lookupText o "count", lookupText o "event_count"])
        , muted (lookupText o "last_seen")
        , plain (lookupText o "pattern")
        ]
      | AE.Object o <- items
      ]
    )
  where
    pickLevelCell o keys =
      let v = T.toUpper $ firstNonEmpty [lookupText o k | k <- keys]
       in case parseSeverity v of
            Just sev -> level sev v
            Nothing
              | v == "OK" || v == "RESOLVED" -> (Success, v)
              | v == "FAILED" || v == "ALERTING" -> (Failure, v)
              | T.null v -> muted "-"
              | otherwise -> plain v
    pickStatus o = case lookupText o "active" of
      "true" -> (Success, "active")
      "false" -> muted "inactive"
      _ -> plain (lookupText o "status")


lookupText :: AEKM.KeyMap AE.Value -> Text -> Text
lookupText o k = case AEKM.lookup (AK.fromText k) o of
  Just (AE.String s) -> s
  Just (AE.Number n) -> show n
  Just (AE.Bool b) -> if b then "true" else "false"
  Just AE.Null -> ""
  Just (AE.Array _) -> "…"
  Just (AE.Object _) -> "…"
  Nothing -> ""


firstNonEmpty :: [Text] -> Text
firstNonEmpty = fromMaybe "" . find (not . T.null)


-- | Normalise a list response to @{data, pagination}@. Returns 'Right' with
-- the normalised value, or 'Left' carrying @(rawValue, reason)@ when the
-- shape doesn't match any known envelope. The reason includes the aeson
-- decode error when an Object fails @Paged@ decoding so contributors see
-- *what* changed in the server envelope, not just a generic mismatch.
--
-- Recognises three shapes:
--   1. Already @{data, pagination}@ → passthrough
--   2. Server's typed @Paged@ envelope (Web.ApiTypes.Paged) → repackaged
--   3. Bare array → wrapped with empty pagination metadata
normalizeListE :: AE.Value -> Either (AE.Value, Text) AE.Value
normalizeListE v
  | hasKey "data" && hasKey "pagination" = Right v
  | AE.Array _ <- v =
      Right
        $ AE.object
          [ "data" AE..= v
          , "pagination" AE..= AE.object ["has_more" AE..= False, "total" AE..= AE.Null, "cursor" AE..= AE.Null]
          ]
  | AE.Object _ <- v = case AE.fromJSON @(Paged AE.Value) v of
      AE.Success p ->
        Right
          $ AE.object
            [ "data" AE..= p.items
            , "pagination"
                AE..= AE.object
                  [ "has_more" AE..= p.hasMore
                  , "total" AE..= p.totalCount
                  , "cursor" AE..= AE.Null
                  , "page" AE..= p.page
                  , "per_page" AE..= p.perPage
                  ]
            ]
      AE.Error msg -> Left (v, "Paged decode failed: " <> toText msg)
  | otherwise = Left (v, "response is neither Object nor Array")
  where
    hasKey k = has (AL.key k) v


runGet :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> ResourceKind -> Text -> OutputMode -> Eff es ()
runGet cfg k resId mode = runAPI mode (apiGetJson @_ @AE.Value cfg (resourceIdPath k resId) [])


runDelete :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> ResourceKind -> Text -> Eff es ()
runDelete cfg k resId =
  withResult (apiDelete cfg (resourceIdPath k resId)) renderAPIError (\_ -> putTextLn $ resourceIdPath k resId <> " deleted")


-- | Lifecycle subcommands (mute, unmute, resolve, toggle_active, activate, deactivate, star).
runLifecycle
  :: (Environment :> es, HTTP :> es, IOE :> es)
  => CLIConfig -> ResourceKind -> Text -> Text -> [(Text, Text)] -> OutputMode -> Eff es ()
runLifecycle cfg k resId verb params mode =
  runAPI mode (apiPostJson @_ @AE.Value @AE.Value cfg (resourceIdPath k resId <> "/" <> verb) params AE.Null)


-- | Non-exiting write. Returns @Either APIError AE.Value@ so callers can
-- decide whether to abort or continue (e.g. 'runApplyResource').
writeJsonRaw
  :: (Environment :> es, HTTP :> es, IOE :> es)
  => CLIConfig -> WriteVerb -> Text -> [(Text, Text)] -> AE.Value -> Eff es (Either APIError AE.Value)
writeJsonRaw cfg verb url params val = case verb of
  POST -> apiPostJson @_ @_ @AE.Value cfg url params val
  PUT -> apiPutJson @_ @_ @AE.Value cfg url params val
  PATCH -> apiPatchJson @_ @_ @AE.Value cfg url params val


-- | Send an already-parsed JSON body at @url@ with the given verb and optional query params.
writeJson
  :: (Environment :> es, HTTP :> es, IOE :> es)
  => CLIConfig -> WriteVerb -> Text -> [(Text, Text)] -> AE.Value -> OutputMode -> Eff es ()
writeJson cfg verb url params val mode = runAPI mode (writeJsonRaw cfg verb url params val)


-- | Read a YAML/JSON file and send it as @verb@ to @url@ with optional query params.
runFromFile
  :: (Environment :> es, FileSystem :> es, HTTP :> es, IOE :> es)
  => CLIConfig -> WriteVerb -> Text -> [(Text, Text)] -> FilePath -> OutputMode -> Eff es ()
runFromFile cfg verb url params path mode = readYamlOrJson path >>= \v -> writeJson cfg verb url params v mode


-- | Apply a YAML/JSON file or directory. If the path is a directory, every
-- .yaml/.yml/.json file in it is applied in sequence. Per-file failures are
-- printed but do not abort the batch; the overall command exits non-zero at
-- the end iff any file failed.
--
-- Dashboards and monitors POST to their /apply endpoints (server-side upsert
-- keyed on file_path / title respectively). Other kinds replace by id (PUT if
-- the file's root object has an @id@ field, POST otherwise).
runApplyResource
  :: (Environment :> es, FileSystem :> es, HTTP :> es, IOE :> es)
  => CLIConfig -> ResourceKind -> FilePath -> OutputMode -> Eff es ()
runApplyResource cfg k path mode = do
  paths <-
    ifM
      (doesDirectoryExist path)
      (fmap (path </>) . filter isApplyable <$> listDirectory path)
      ( ifM
          (doesFileExist path)
          (pure [path])
          (printError ("file not found: " <> toText path) >> liftIO exitFailure)
      )
  when (null paths)
    $ printError ("no applyable (.yaml/.yml/.json) files in " <> toText path)
    >> liftIO exitFailure
  failures <- sum <$> forM paths (\p -> applyOne cfg k p mode)
  when (failures > 0)
    $ printError (show failures <> " file(s) failed to apply")
    >> liftIO exitFailure


isApplyable :: FilePath -> Bool
isApplyable fp = takeExtension fp `elem` [".yaml", ".yml", ".json"]


-- | Apply a single file; returns @1@ on failure (already printed), @0@ on success.
applyOne
  :: (Environment :> es, FileSystem :> es, HTTP :> es, IOE :> es)
  => CLIConfig -> ResourceKind -> FilePath -> OutputMode -> Eff es Int
applyOne cfg k path mode = do
  val <- readYamlOrJson path
  let base = resourcePath k
  case (k, val) of
    _ | k `elem` [Dashboards, Monitors] -> runAndCount (writeJsonRaw cfg POST (base <> "/apply") [] val)
    (_, AE.Object o) ->
      let (verb, url) = case AEKM.lookup "id" o of
            Just (AE.String rid) -> (PUT, base <> "/" <> rid)
            _ -> (POST, base)
       in runAndCount (writeJsonRaw cfg verb url [] val)
    _ -> do
      printError (toText path <> ": expected a top-level object")
      pure 1
  where
    runAndCount act =
      act >>= \case
        Left e -> printError (toText path <> ": " <> renderAPIError e) $> 1
        Right v -> renderByMode mode Nothing v $> 0


-- | Read + parse a YAML or JSON file. Exits with an error message on missing file or parse failure.
-- Uses the JSON parser for @.json@ (clearer errors) and the YAML parser otherwise
-- (YAML is a JSON superset, so this handles @.yaml@/@.yml@/unsuffixed files).
readYamlOrJson :: (FileSystem :> es, IOE :> es) => FilePath -> Eff es AE.Value
readYamlOrJson path = do
  unlessM (doesFileExist path) $ do
    printError ("file not found: " <> toText path)
    liftIO exitFailure
  if takeExtension path == ".json"
    then liftIO (AE.eitherDecodeFileStrict path) >>= either (failWith "json") pure
    else liftIO (Yaml.decodeFileEither path) >>= either (failWith "yaml" . Yaml.prettyPrintParseException) pure
  where
    failWith kind err = do
      printError (toText path <> ": " <> kind <> " parse error: " <> toText err)
      liftIO exitFailure


-- | POST /<resource>/bulk {action, ids, duration_minutes?}.
runBulk
  :: (Environment :> es, HTTP :> es, IOE :> es)
  => CLIConfig -> ResourceKind -> Text -> [Text] -> Maybe Int -> OutputMode -> Eff es ()
runBulk cfg k action ids dur mode =
  runAPI
    mode
    ( apiPostJson @_ @_ @AE.Value
        cfg
        (resourcePath k <> "/bulk")
        []
        (AE.object ["action" AE..= action, "ids" AE..= ids, "duration_minutes" AE..= dur])
    )


-- | GET <resource>/:id/yaml and print as YAML — for `dashboards yaml ID`.
runYamlDump :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> ResourceKind -> Text -> Eff es ()
runYamlDump cfg k resId =
  withResult (apiGetJson @_ @AE.Value cfg (resourceIdPath k resId <> "/yaml") []) renderAPIError $ \v ->
    liftIO $ putBSLn (Yaml.encode v)
