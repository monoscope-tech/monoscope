module CLI.Config (
  CLIConfig (..),
  Credential (..),
  parseCredential,
  credentialText,
  ConfigKey (..),
  allConfigKeys,
  configKeyText,
  parseConfigKey,
  resolveConfig,
  configDir,
  loadToken,
  saveToken,
  removeToken,
  configFilePath,
  setConfigValue,
) where

import Relude

import Data.Map.Strict qualified as Map
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Yaml qualified as Yaml
import Effectful
import Effectful.Environment (Environment)
import Effectful.Environment qualified as Env
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing, doesFileExist, getCurrentDirectory, removeFile)
import System.Directory (XdgDirectory (..), getXdgDirectory)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)


data CLIConfig = CLIConfig
  { apiUrl :: Text
  , credential :: Maybe Credential
  , projectId :: Maybe Text
  }
  deriving stock (Show)


-- | What the CLI authenticates with. Project API keys are base64 ciphertext, never
-- UUID-shaped, so a UUID token is always a login session.
--
-- >>> parseCredential "00000000-0000-0000-0000-000000000002"
-- Session 00000000-0000-0000-0000-000000000002
-- >>> parseCredential "c2VjcmV0LWtleQ=="
-- ApiKey "c2VjcmV0LWtleQ=="
-- >>> credentialText (parseCredential "00000000-0000-0000-0000-000000000002")
-- "00000000-0000-0000-0000-000000000002"
data Credential = ApiKey Text | Session UUID
  deriving stock (Eq, Show)


parseCredential :: Text -> Credential
parseCredential t = maybe (ApiKey t) Session (UUID.fromText t)


credentialText :: Credential -> Text
credentialText = \case
  ApiKey k -> k
  Session s -> UUID.toText s


data ConfigKey = CKApiUrl | CKProject | CKApiKey
  deriving stock (Bounded, Enum, Eq, Show)


allConfigKeys :: [ConfigKey]
allConfigKeys = universe


configKeyText :: ConfigKey -> Text
configKeyText = \case CKApiUrl -> "api_url"; CKProject -> "project"; CKApiKey -> "api_key"


parseConfigKey :: Text -> Maybe ConfigKey
parseConfigKey = \case "api_url" -> Just CKApiUrl; "project" -> Just CKProject; "api_key" -> Just CKApiKey; _ -> Nothing


data FileConfig = FileConfig
  { api_url :: Maybe Text
  , project :: Maybe Text
  , api_key :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Yaml.FromJSON, Yaml.ToJSON)


configDir :: (FileSystem :> es, IOE :> es) => Eff es FilePath
configDir = do
  dir <- liftIO $ getXdgDirectory XdgConfig "monoscope"
  createDirectoryIfMissing True dir
  pure dir


tokensFile :: (FileSystem :> es, IOE :> es) => Eff es FilePath
tokensFile = (</> "tokens.json") <$> configDir


resolveConfig :: (Environment :> es, FileSystem :> es, IOE :> es) => Eff es CLIConfig
resolveConfig = do
  global <- loadGlobalConfig
  projCfg <- loadLocalConfig
  envApiUrl <- Env.lookupEnv "MONOSCOPE_API_URL"
  envApiKey <- Env.lookupEnv "MONOSCOPE_API_KEY"
  envProject <- Env.lookupEnv "MONOSCOPE_PROJECT"
  storedKey <- loadToken
  let merged = mergeConfigs [global, projCfg]
  pure
    CLIConfig
      { apiUrl = fromMaybe "https://api.monoscope.tech" $ (toText <$> envApiUrl) <|> merged.api_url
      , credential = parseCredential <$> ((toText <$> envApiKey) <|> merged.api_key <|> storedKey)
      , projectId = (toText <$> envProject) <|> merged.project
      }


loadGlobalConfig :: (FileSystem :> es, IOE :> es) => Eff es FileConfig
loadGlobalConfig = do
  dir <- configDir
  loadYaml (dir </> "config.yaml")


loadLocalConfig :: (FileSystem :> es, IOE :> es) => Eff es FileConfig
loadLocalConfig = getCurrentDirectory >>= traverseUp
  where
    traverseUp dir = do
      let f = dir </> ".monoscope.yaml"
      exists <- doesFileExist f
      if exists
        then loadYaml f
        else
          let parent = takeDirectory dir
           in if parent == dir then pure emptyConfig else traverseUp parent


loadYaml :: (FileSystem :> es, IOE :> es) => FilePath -> Eff es FileConfig
loadYaml path = do
  exists <- doesFileExist path
  if exists
    then liftIO $ fromRight emptyConfig <$> Yaml.decodeFileEither path
    else pure emptyConfig


emptyConfig :: FileConfig
emptyConfig = FileConfig Nothing Nothing Nothing


mergeConfigs :: [FileConfig] -> FileConfig
mergeConfigs = foldl' merge emptyConfig
  where
    merge a b =
      FileConfig
        { api_url = b.api_url <|> a.api_url
        , project = b.project <|> a.project
        , api_key = b.api_key <|> a.api_key
        }


loadToken :: (FileSystem :> es, IOE :> es) => Eff es (Maybe Text)
loadToken = do
  f <- tokensFile
  exists <- doesFileExist f
  if exists
    then
      liftIO $ Yaml.decodeFileEither @(Map Text Text) f >>= \case
        Right m -> pure $ Map.lookup "token" m
        Left _ -> pure Nothing
    else pure Nothing


saveToken :: (FileSystem :> es, IOE :> es) => Text -> Eff es ()
saveToken token = do
  f <- tokensFile
  let m = one ("token", token) :: Map Text Text
  liftIO $ Yaml.encodeFile f m
  liftIO $ setFileMode f 0o600


removeToken :: (FileSystem :> es, IOE :> es) => Eff es ()
removeToken = do
  f <- tokensFile
  exists <- doesFileExist f
  when exists $ removeFile f


configFilePath :: (FileSystem :> es, IOE :> es) => Eff es FilePath
configFilePath = (</> "config.yaml") <$> configDir


setConfigValue :: (FileSystem :> es, IOE :> es) => ConfigKey -> Text -> Eff es ()
setConfigValue key val = do
  f <- configFilePath
  cfg <- loadYaml f
  let updated = case key of
        CKApiUrl -> cfg{api_url = Just val}
        CKProject -> cfg{project = Just val}
        CKApiKey -> cfg{api_key = Just val}
  liftIO $ Yaml.encodeFile f updated
