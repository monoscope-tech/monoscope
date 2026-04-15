-- | Generic Plan A resource commands for monitors, dashboards, api-keys.
--
-- These wrap the v1 JSON endpoints. Each subcommand is a thin shell around
-- @apiGetJson@/@apiPostJson@/@apiDelete@ from "CLI.Core" — no command-specific
-- parsing or formatting beyond picking the right URL segment.
module CLI.Resource
  ( ResourceKind (..)
  , runList
  , runGet
  , runCreate
  , runUpdate
  , runDelete
  , runLifecycle
  , runApply
  , runYamlDump
  , resourcePath
  ) where

import Relude

import CLI.Config (CLIConfig (..))
import CLI.Core (OutputMode (..), apiDelete, apiGetJson, apiPostJson, apiPutJson, printError, renderByMode)
import Data.Aeson qualified as AE
import Data.Effectful.Wreq (HTTP)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Effectful
import Effectful.FileSystem (FileSystem, doesFileExist)


data ResourceKind = Monitors | Dashboards | ApiKeys
  deriving stock (Eq, Show)


-- | URL prefix for a given kind (matches server routes under /api/v1).
resourcePath :: ResourceKind -> Text
resourcePath = \case
  Monitors -> "/monitors"
  Dashboards -> "/dashboards"
  ApiKeys -> "/api_keys"


withResult :: IOE :> es => Eff es (Either e a) -> (e -> Text) -> (a -> Eff es ()) -> Eff es ()
withResult act renderErr onOk =
  act >>= \case
    Left e -> printError (renderErr e) >> liftIO exitFailure
    Right v -> onOk v


runList :: (HTTP :> es, IOE :> es) => CLIConfig -> ResourceKind -> [(Text, Text)] -> OutputMode -> Eff es ()
runList cfg k params mode =
  withResult (apiGetJson @_ @AE.Value cfg (resourcePath k) params) show (renderByMode mode Nothing)


runGet :: (HTTP :> es, IOE :> es) => CLIConfig -> ResourceKind -> Text -> OutputMode -> Eff es ()
runGet cfg k resId mode =
  withResult (apiGetJson @_ @AE.Value cfg (resourcePath k <> "/" <> resId) []) show (renderByMode mode Nothing)


runCreate
  :: (HTTP :> es, IOE :> es, AE.ToJSON a) => CLIConfig -> ResourceKind -> a -> OutputMode -> Eff es ()
runCreate cfg k body mode =
  withResult (apiPostJson @_ @_ @AE.Value cfg (resourcePath k) body) show (renderByMode mode Nothing)


runUpdate
  :: (HTTP :> es, IOE :> es, AE.ToJSON a)
  => CLIConfig -> ResourceKind -> Text -> a -> OutputMode -> Eff es ()
runUpdate cfg k resId body mode =
  withResult (apiPutJson @_ @_ @AE.Value cfg (resourcePath k <> "/" <> resId) body) show (renderByMode mode Nothing)


runDelete :: (HTTP :> es, IOE :> es) => CLIConfig -> ResourceKind -> Text -> Eff es ()
runDelete cfg k resId =
  withResult (apiDelete cfg (resourcePath k <> "/" <> resId)) show (\_ -> putTextLn $ resourcePath k <> "/" <> resId <> " deleted")


-- | Lifecycle subcommands (mute, unmute, resolve, toggle_active, activate, deactivate, star).
runLifecycle
  :: (HTTP :> es, IOE :> es)
  => CLIConfig -> ResourceKind -> Text -> Text -> [(Text, Text)] -> OutputMode -> Eff es ()
runLifecycle cfg k resId verb params mode =
  withResult
    (apiPostJson @_ @AE.Value @AE.Value cfg (resourcePath k <> "/" <> resId <> "/" <> verb <> renderParams params) AE.Null)
    show
    (renderByMode mode Nothing)


-- | Apply: POST a parsed YAML document to <resource>/apply.
runApply
  :: (HTTP :> es, FileSystem :> es, IOE :> es)
  => CLIConfig -> ResourceKind -> FilePath -> OutputMode -> Eff es ()
runApply cfg k path mode = do
  exists <- doesFileExist path
  unless exists $ printError ("file not found: " <> toText path) >> liftIO exitFailure
  parsed <- liftIO $ Yaml.decodeFileEither path
  case parsed of
    Left err -> printError ("yaml parse error: " <> toText (Yaml.prettyPrintParseException err)) >> liftIO exitFailure
    Right (val :: AE.Value) ->
      withResult
        (apiPostJson @_ @_ @AE.Value cfg (resourcePath k <> "/apply") val)
        show
        (renderByMode mode Nothing)


-- | GET <resource>/:id/yaml and print as YAML — for `dashboards yaml ID`.
runYamlDump :: (HTTP :> es, IOE :> es) => CLIConfig -> ResourceKind -> Text -> Eff es ()
runYamlDump cfg k resId =
  withResult (apiGetJson @_ @AE.Value cfg (resourcePath k <> "/" <> resId <> "/yaml") []) show $ \v ->
    liftIO $ putBSLn (Yaml.encode v)


renderParams :: [(Text, Text)] -> Text
renderParams [] = ""
renderParams ps = "?" <> T.intercalate "&" ((\(k, v) -> k <> "=" <> v) <$> ps)
