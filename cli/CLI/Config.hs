module CLI.Config
  ( CLIConfig (..)
  , resolveConfig
  , configDir
  , loadToken
  , saveToken
  , removeToken
  , configFilePath
  , setConfigValue
  ) where

import Relude

import Data.Map.Strict qualified as Map
import Data.Yaml qualified as Yaml
import Effectful
import Effectful.Environment (Environment)
import Effectful.Environment qualified as Env
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing, doesFileExist, getCurrentDirectory, getHomeDirectory, removeFile)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)

data CLIConfig = CLIConfig
  { apiUrl :: Text
  , apiKey :: Maybe Text
  , projectId :: Maybe Text
  }
  deriving stock (Show)

data FileConfig = FileConfig
  { api_url :: Maybe Text
  , project :: Maybe Text
  , api_key :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Yaml.FromJSON, Yaml.ToJSON)

configDir :: (FileSystem :> es) => Eff es FilePath
configDir = do
  home <- getHomeDirectory
  let dir = home </> ".config" </> "monoscope"
  createDirectoryIfMissing True dir
  pure dir

tokensFile :: (FileSystem :> es) => Eff es FilePath
tokensFile = (</> "tokens.json") <$> configDir

resolveConfig :: (FileSystem :> es, Environment :> es, IOE :> es) => Eff es CLIConfig
resolveConfig = do
  global <- loadGlobalConfig
  projCfg <- loadLocalConfig
  envApiUrl <- Env.lookupEnv "MONO_API_URL"
  envApiKey <- Env.lookupEnv "MONO_API_KEY"
  envProject <- Env.lookupEnv "MONO_PROJECT"
  storedKey <- loadToken
  let merged = mergeConfigs [global, projCfg]
  pure
    CLIConfig
      { apiUrl = fromMaybe "https://app.monoscope.tech" $ (toText <$> envApiUrl) <|> merged.api_url
      , apiKey = (toText <$> envApiKey) <|> merged.api_key <|> storedKey
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
    then liftIO $ either (const emptyConfig) id <$> Yaml.decodeFileEither path
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
    then liftIO $ Yaml.decodeFileEither @(Map Text Text) f >>= \case
      Right m -> pure $ Map.lookup "token" m
      Left _ -> pure Nothing
    else pure Nothing

saveToken :: (FileSystem :> es, IOE :> es) => Text -> Eff es ()
saveToken token = do
  f <- tokensFile
  let m = one ("token", token) :: Map Text Text
  liftIO $ Yaml.encodeFile f m
  liftIO $ setFileMode f 0o600

removeToken :: (FileSystem :> es) => Eff es ()
removeToken = do
  f <- tokensFile
  exists <- doesFileExist f
  when exists $ removeFile f

configFilePath :: (FileSystem :> es) => Eff es FilePath
configFilePath = (</> "config.yaml") <$> configDir

setConfigValue :: (FileSystem :> es, IOE :> es) => Text -> Text -> Eff es ()
setConfigValue key val = do
  f <- configFilePath
  cfg <- loadYaml f
  let updated = case key of
        "api_url" -> cfg{api_url = Just val}
        "project" -> cfg{project = Just val}
        "api_key" -> cfg{api_key = Just val}
        _ -> cfg
  liftIO $ Yaml.encodeFile f updated
