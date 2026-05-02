-- | Generic Plan A resource commands for monitors, dashboards, api-keys.
--
-- These wrap the v1 JSON endpoints. Each subcommand is a thin shell around
-- @apiGetJson@/@apiPostJson@/@apiDelete@ from "CLI.Core" — no command-specific
-- parsing or formatting beyond picking the right URL segment.
module CLI.Resource
  ( ResourceKind (..)
  , WriteVerb (..)
  , runList
  , runGet
  , runDelete
  , runLifecycle
  , runBulk
  , runFromFile
  , writeJson
  , writeJsonRaw
  , readYamlOrJson
  , runApplyResource
  , runYamlDump
  , resourcePath
  , resourceIdPath
  , withResult
  , runAPI
  , normalizeList
  ) where

import Relude

import CLI.Config (CLIConfig (..))
import CLI.Core (APIError, OutputMode (..), apiDelete, apiGetJson, apiPatchJson, apiPostJson, apiPutJson, printDebug, printError, renderAPIError, renderByMode)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.Effectful.Wreq (HTTP)
import Data.Yaml qualified as Yaml
import Effectful
import Effectful.Environment (Environment)
import Effectful.FileSystem (FileSystem, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Web.ApiTypes (Paged (..))


data ResourceKind = Monitors | Dashboards | ApiKeys | Teams | Members
  deriving stock (Eq, Show)


-- | Write verb for JSON bodies sent via 'writeJson' / 'runFromFile'.
-- Kept as a small enum (not 'Network.HTTP.Types.StdMethod') so the
-- surface of 'writeJson' only admits the three verbs the CLI needs.
data WriteVerb = POST | PUT | PATCH
  deriving stock (Eq, Show)


-- | URL prefix for a given kind under /api/v1.
resourcePath :: ResourceKind -> Text
resourcePath = ("/api/v1" <>) . \case
  Monitors -> "/monitors"
  Dashboards -> "/dashboards"
  ApiKeys -> "/api_keys"
  Teams -> "/teams"
  Members -> "/members"


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
runAPI :: (IOE :> es, AE.ToJSON a) => OutputMode -> Eff es (Either APIError a) -> Eff es ()
runAPI mode act = withResult act renderAPIError (renderByMode mode Nothing)


-- | D4: every list endpoint returns the same shape from the CLI's perspective:
-- @{data: [...], pagination: {has_more, total, cursor}}@. This is a CLI-side
-- adapter — the server still returns its native envelope (bare arrays for some
-- resources, @Paged@ for others). Centralising the normalisation here means
-- agents can rely on a single shape across @issues list@, @monitors list@,
-- @dashboards list@, @api-keys list@, @teams list@, @members list@,
-- @endpoints list@, @log-patterns list@.
runList :: (HTTP :> es, Environment :> es, IOE :> es) => CLIConfig -> ResourceKind -> [(Text, Text)] -> OutputMode -> Eff es ()
runList cfg k params mode =
  apiGetJson @_ @AE.Value cfg (resourcePath k) params >>= \case
    Left e -> printError (renderAPIError e) >> liftIO exitFailure
    Right v -> case normalizeListE v of
      Right normalised -> renderByMode mode Nothing normalised
      Left raw -> do
        -- Server returned an envelope shape we don't recognise. Surface
        -- it via printDebug (always-on for stderr noise; agents that pipe
        -- stdout get the raw payload regardless).
        printDebug $
          "list response from " <> resourcePath k <> " did not match {data, pagination} or Paged envelope; passing through raw"
        renderByMode mode Nothing raw


-- | Normalise a list response to @{data, pagination}@. Returns 'Right' with
-- the normalised value, or 'Left' carrying the original value when the shape
-- doesn't match any known envelope (callers can surface a debug warning so
-- contributors notice a server-side wire change rather than silently emit
-- the raw payload).
--
-- Recognises three shapes:
--   1. Already @{data, pagination}@ → passthrough
--   2. Server's typed @Paged@ envelope (Web.ApiTypes.Paged) → repackaged
--   3. Bare array → wrapped with empty pagination metadata
normalizeListE :: AE.Value -> Either AE.Value AE.Value
normalizeListE v = case v of
  AE.Object obj | AEKM.member "data" obj && AEKM.member "pagination" obj -> Right v
  AE.Object _ | AE.Success (p :: Paged AE.Value) <- AE.fromJSON v ->
    Right $ AE.object
      [ "data" AE..= p.items
      , "pagination" AE..= AE.object
          [ "has_more" AE..= p.hasMore
          , "total" AE..= p.totalCount
          , "cursor" AE..= AE.Null
          , "page" AE..= p.page
          , "per_page" AE..= p.perPage
          ]
      ]
  AE.Array _ ->
    Right $ AE.object
      [ "data" AE..= v
      , "pagination" AE..= AE.object ["has_more" AE..= False, "total" AE..= AE.Null, "cursor" AE..= AE.Null]
      ]
  _ -> Left v


-- | Pure normaliser kept for direct use; falls through to the original value
-- on shape mismatch (without warning). Prefer 'normalizeListE' from new code
-- so the caller can flag unexpected payloads.
normalizeList :: AE.Value -> AE.Value
normalizeList = either id id . normalizeListE


runGet :: (HTTP :> es, Environment :> es, IOE :> es) => CLIConfig -> ResourceKind -> Text -> OutputMode -> Eff es ()
runGet cfg k resId mode = runAPI mode (apiGetJson @_ @AE.Value cfg (resourceIdPath k resId) [])


runDelete :: (HTTP :> es, Environment :> es, IOE :> es) => CLIConfig -> ResourceKind -> Text -> Eff es ()
runDelete cfg k resId =
  withResult (apiDelete cfg (resourceIdPath k resId)) renderAPIError (\_ -> putTextLn $ resourceIdPath k resId <> " deleted")


-- | Lifecycle subcommands (mute, unmute, resolve, toggle_active, activate, deactivate, star).
runLifecycle
  :: (HTTP :> es, Environment :> es, IOE :> es)
  => CLIConfig -> ResourceKind -> Text -> Text -> [(Text, Text)] -> OutputMode -> Eff es ()
runLifecycle cfg k resId verb params mode =
  runAPI mode (apiPostJson @_ @AE.Value @AE.Value cfg (resourceIdPath k resId <> "/" <> verb) params AE.Null)


-- | Non-exiting write. Returns @Either APIError AE.Value@ so callers can
-- decide whether to abort or continue (e.g. 'runApplyResource').
writeJsonRaw
  :: (HTTP :> es, Environment :> es, IOE :> es)
  => CLIConfig -> WriteVerb -> Text -> [(Text, Text)] -> AE.Value -> Eff es (Either APIError AE.Value)
writeJsonRaw cfg verb url params val = case verb of
  POST -> apiPostJson @_ @_ @AE.Value cfg url params val
  PUT -> apiPutJson @_ @_ @AE.Value cfg url params val
  PATCH -> apiPatchJson @_ @_ @AE.Value cfg url params val


-- | Send an already-parsed JSON body at @url@ with the given verb and optional query params.
writeJson
  :: (HTTP :> es, Environment :> es, IOE :> es)
  => CLIConfig -> WriteVerb -> Text -> [(Text, Text)] -> AE.Value -> OutputMode -> Eff es ()
writeJson cfg verb url params val mode = runAPI mode (writeJsonRaw cfg verb url params val)


-- | Read a YAML/JSON file and send it as @verb@ to @url@ with optional query params.
runFromFile
  :: (HTTP :> es, Environment :> es, FileSystem :> es, IOE :> es)
  => CLIConfig -> WriteVerb -> Text -> [(Text, Text)] -> FilePath -> OutputMode -> Eff es ()
runFromFile cfg verb url params path mode = readYamlOrJson path >>= \v -> writeJson cfg verb url params v mode


-- | Apply a YAML/JSON file or directory. If the path is a directory, every
-- .yaml/.yml/.json file in it is applied in sequence. Per-file failures are
-- printed but do not abort the batch; the overall command exits non-zero at
-- the end iff any file failed.
--
-- For dashboards this calls POST /dashboards/apply. For monitors, since there
-- is no dedicated apply endpoint yet, it replaces by id (PUT if the file's
-- root object has an @id@ field, POST otherwise) — there is no natural-key
-- upsert, so a monitor file without an @id@ creates a new row on each apply.
runApplyResource
  :: (HTTP :> es, Environment :> es, FileSystem :> es, IOE :> es)
  => CLIConfig -> ResourceKind -> FilePath -> OutputMode -> Eff es ()
runApplyResource cfg k path mode = do
  paths <- ifM (doesDirectoryExist path)
    (fmap (path </>) . filter isApplyable <$> listDirectory path)
    (ifM (doesFileExist path) (pure [path])
       (printError ("file not found: " <> toText path) >> liftIO exitFailure))
  when (null paths) $
    printError ("no applyable (.yaml/.yml/.json) files in " <> toText path) >> liftIO exitFailure
  failures <- sum <$> forM paths (\p -> applyOne cfg k p mode)
  when (failures > 0) $
    printError (show failures <> " file(s) failed to apply") >> liftIO exitFailure


isApplyable :: FilePath -> Bool
isApplyable fp = takeExtension fp `elem` [".yaml", ".yml", ".json"]


-- | Apply a single file; returns @1@ on failure (already printed), @0@ on success.
applyOne
  :: (HTTP :> es, Environment :> es, FileSystem :> es, IOE :> es)
  => CLIConfig -> ResourceKind -> FilePath -> OutputMode -> Eff es Int
applyOne cfg k path mode = do
  val <- readYamlOrJson path
  let base = resourcePath k
  case (k, val) of
    (Dashboards, _) -> runAndCount (writeJsonRaw cfg POST (base <> "/apply") [] val)
    (_, AE.Object o) ->
      let (verb, url) = case AEKM.lookup "id" o of
            Just (AE.String rid) -> (PUT, base <> "/" <> rid)
            _ -> (POST, base)
       in runAndCount (writeJsonRaw cfg verb url [] val)
    _ -> do
      printError (toText path <> ": expected a top-level object")
      pure 1
  where
    runAndCount act = act >>= \case
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
  :: (HTTP :> es, Environment :> es, IOE :> es)
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
runYamlDump :: (HTTP :> es, Environment :> es, IOE :> es) => CLIConfig -> ResourceKind -> Text -> Eff es ()
runYamlDump cfg k resId =
  withResult (apiGetJson @_ @AE.Value cfg (resourceIdPath k resId <> "/yaml") []) renderAPIError $ \v ->
    liftIO $ putBSLn (Yaml.encode v)
