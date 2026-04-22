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
  ) where

import Relude

import CLI.Config (CLIConfig (..))
import CLI.Core (APIError, OutputMode (..), apiDelete, apiGetJson, apiPatchJson, apiPostJson, apiPutJson, printError, renderAPIError, renderByMode)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.Effectful.Wreq (HTTP)
import Data.Yaml qualified as Yaml
import Effectful
import Effectful.FileSystem (FileSystem, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))


data ResourceKind = Monitors | Dashboards | ApiKeys | Teams | Members
  deriving stock (Eq, Show)


-- | Write verb for JSON bodies sent via 'writeJson' / 'runFromFile'.
-- Kept as a small enum (not 'Network.HTTP.Types.StdMethod') so the
-- surface of 'writeJson' only admits the three verbs the CLI needs.
data WriteVerb = POST | PUT | PATCH
  deriving stock (Eq, Show)


-- | URL prefix for a given kind (matches server routes under /api/v1).
resourcePath :: ResourceKind -> Text
resourcePath = \case
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


runList :: (HTTP :> es, IOE :> es) => CLIConfig -> ResourceKind -> [(Text, Text)] -> OutputMode -> Eff es ()
runList cfg k params mode = runAPI mode (apiGetJson @_ @AE.Value cfg (resourcePath k) params)


runGet :: (HTTP :> es, IOE :> es) => CLIConfig -> ResourceKind -> Text -> OutputMode -> Eff es ()
runGet cfg k resId mode = runAPI mode (apiGetJson @_ @AE.Value cfg (resourceIdPath k resId) [])


runDelete :: (HTTP :> es, IOE :> es) => CLIConfig -> ResourceKind -> Text -> Eff es ()
runDelete cfg k resId =
  withResult (apiDelete cfg (resourceIdPath k resId)) renderAPIError (\_ -> putTextLn $ resourceIdPath k resId <> " deleted")


-- | Lifecycle subcommands (mute, unmute, resolve, toggle_active, activate, deactivate, star).
runLifecycle
  :: (HTTP :> es, IOE :> es)
  => CLIConfig -> ResourceKind -> Text -> Text -> [(Text, Text)] -> OutputMode -> Eff es ()
runLifecycle cfg k resId verb params mode =
  runAPI mode (apiPostJson @_ @AE.Value @AE.Value cfg (resourceIdPath k resId <> "/" <> verb) params AE.Null)


-- | Non-exiting write. Returns @Either APIError AE.Value@ so callers can
-- decide whether to abort or continue (e.g. 'runApplyResource').
writeJsonRaw
  :: (HTTP :> es, IOE :> es)
  => CLIConfig -> WriteVerb -> Text -> [(Text, Text)] -> AE.Value -> Eff es (Either APIError AE.Value)
writeJsonRaw cfg verb url params val = case verb of
  POST -> apiPostJson @_ @_ @AE.Value cfg url params val
  PUT -> apiPutJson @_ @_ @AE.Value cfg url params val
  PATCH -> apiPatchJson @_ @_ @AE.Value cfg url params val


-- | Send an already-parsed JSON body at @url@ with the given verb and optional query params.
writeJson
  :: (HTTP :> es, IOE :> es)
  => CLIConfig -> WriteVerb -> Text -> [(Text, Text)] -> AE.Value -> OutputMode -> Eff es ()
writeJson cfg verb url params val mode = runAPI mode (writeJsonRaw cfg verb url params val)


-- | Read a YAML/JSON file and send it as @verb@ to @url@ with optional query params.
runFromFile
  :: (HTTP :> es, FileSystem :> es, IOE :> es)
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
  :: (HTTP :> es, FileSystem :> es, IOE :> es)
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
  :: (HTTP :> es, FileSystem :> es, IOE :> es)
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
      Left e -> printError (toText path <> ": " <> renderAPIError e) >> pure 1
      Right v -> renderByMode mode Nothing v >> pure 0


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
  :: (HTTP :> es, IOE :> es)
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
runYamlDump :: (HTTP :> es, IOE :> es) => CLIConfig -> ResourceKind -> Text -> Eff es ()
runYamlDump cfg k resId =
  withResult (apiGetJson @_ @AE.Value cfg (resourceIdPath k resId <> "/yaml") []) renderAPIError $ \v ->
    liftIO $ putBSLn (Yaml.encode v)
