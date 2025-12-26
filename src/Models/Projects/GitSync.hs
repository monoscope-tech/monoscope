{-# LANGUAGE PackageImports #-}

module Models.Projects.GitSync (
  GitHubSync (..),
  GitHubSyncId,
  TreeEntry (..),
  SyncAction (..),
  isAppInstallation,
  getGitHubSync,
  getGitHubSyncDecrypted,
  getGitHubSyncByRepo,
  getGitHubSyncByRepoDecrypted,
  getGitHubSyncByInstallation,
  insertGitHubSync,
  insertGitHubAppSync,
  updateGitHubSync,
  updateGitHubSyncKeepToken,
  updateGitHubSyncRepo,
  updateLastTreeSha,
  deleteGitHubSync,
  getDashboardGitState,
  updateDashboardGitInfo,
  fetchGitTree,
  fetchFileContent,
  pushFileToGit,
  buildSyncPlan,
  dashboardToYaml,
  yamlToDashboard,
  titleToFilePath,
  computeContentSha,
  buildSchemaWithMeta,
  getDashboardsPath,
  detectDefaultBranch,
) where

import Control.Lens ((.~), (?~), (^.), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _Array, _Bool, _String)
import Data.Aeson.Types (parseMaybe)
import Data.Base64.Types (extractBase64)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Default (Default (..), def)
import Data.Effectful.Wreq qualified as W
import Data.Generics.Labels ()
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml
import Database.PostgreSQL.Entity (Entity, _selectWhere)
import Database.PostgreSQL.Entity.Types (CamelToSnake, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName, field)
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Deriving.Aeson qualified as DAE
import Effectful (Eff, IOE, (:>))
import Effectful.Log (Log)
import Effectful.PostgreSQL qualified as PG
import Models.Projects.Dashboards (Dashboard, DashboardId)
import Models.Projects.ProjectApiKeys (decryptAPIKey, encryptAPIKey)
import Models.Projects.Projects (ProjectId)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), responseStatus)
import Network.HTTP.Types.Status (statusCode)
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import System.DB (DB)
import System.Logging (logWarn)
import Text.Casing (fromAny, toKebab)
import UnliftIO.Exception (try)
import "base64" Data.ByteString.Base64 qualified as B64
import "cryptonite" Crypto.Hash (Digest, SHA1, hash)
import "memory" Data.ByteArray qualified as BA


type GitHubSyncId = UUIDId "github_sync"


data GitHubSync = GitHubSync
  { id :: GitHubSyncId
  , projectId :: ProjectId
  , owner :: Text
  , repo :: Text
  , branch :: Text
  , accessToken :: Maybe Text -- Encrypted PAT (for manual setup)
  , installationId :: Maybe Int64 -- GitHub App installation ID
  , pathPrefix :: Text -- Directory prefix for dashboards (default: "")
  , webhookSecret :: Maybe Text
  , lastTreeSha :: Maybe Text
  , syncEnabled :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "projects", TableName "github_sync", PrimaryKey "id", FieldModifiers '[CamelToSnake]] GitHubSync)


instance Default GitHubSync where
  def = GitHubSync (UUIDId UUID.nil) (UUIDId UUID.nil) "" "" "main" Nothing Nothing "" Nothing Nothing True epoch epoch
    where
      epoch = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)


-- | Check if sync uses GitHub App (has installation_id)
isAppInstallation :: GitHubSync -> Bool
isAppInstallation sync = isJust sync.installationId


data TreeEntry = TreeEntry
  { path :: Text
  , _teType :: Text
  , sha :: Text
  , size :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "_te", DAE.CamelToSnake]] TreeEntry


data SyncAction
  = SyncCreate {path :: Text, sha :: Text}
  | SyncUpdate {path :: Text, sha :: Text, resourceId :: DashboardId}
  | SyncDelete {path :: Text, resourceId :: DashboardId}
  deriving stock (Generic, Show)


-- Token encryption helpers
encryptToken :: ByteString -> Text -> Text
encryptToken encKey = extractBase64 . B64.encodeBase64 . encryptAPIKey encKey . encodeUtf8


-- | Decrypt an access token. Returns Left with error description if decryption fails.
-- SECURITY: Never falls back to plaintext - callers must handle Left appropriately.
decryptToken :: ByteString -> Text -> Either Text Text
decryptToken encKey encryptedB64 =
  first (("Base64 decode failed: " <>) . toText . show)
    $ decodeUtf8
    . decryptAPIKey encKey
    <$> B64.decodeBase64Untyped (encodeUtf8 encryptedB64)


-- | Decrypt a GitHubSync's access token if present. Returns Left with error if decryption fails.
-- For GitHub App installations (no accessToken), returns the sync unchanged.
decryptSync :: ByteString -> GitHubSync -> Either Text GitHubSync
decryptSync encKey sync = case sync.accessToken of
  Nothing -> Right sync -- GitHub App installation, no token to decrypt
  Just token -> decryptToken encKey token <&> \decrypted -> sync & #accessToken ?~ decrypted


-- DB Operations
getGitHubSync :: DB es => ProjectId -> Eff es (Maybe GitHubSync)
getGitHubSync pid = listToMaybe <$> PG.query (_selectWhere @GitHubSync [[field| project_id |]]) (Only pid)


-- | Helper to decrypt sync config, logging on failure
withDecryption :: (AE.ToJSON ctx, DB es, Log :> es) => ByteString -> ctx -> Eff es (Maybe GitHubSync) -> Eff es (Maybe GitHubSync)
withDecryption encKey ctx fetch =
  fetch >>= maybe (pure Nothing) (either (\err -> logWarn "GitHub sync token decryption failed" (ctx, err) $> Nothing) (pure . Just) . decryptSync encKey)


getGitHubSyncDecrypted :: (DB es, Log :> es) => ByteString -> ProjectId -> Eff es (Maybe GitHubSync)
getGitHubSyncDecrypted encKey pid = withDecryption encKey pid $ getGitHubSync pid


getGitHubSyncByRepo :: DB es => Text -> Text -> Eff es (Maybe GitHubSync)
getGitHubSyncByRepo owner repo = listToMaybe <$> PG.query (_selectWhere @GitHubSync [[field| owner |], [field| repo |]]) (owner, repo)


getGitHubSyncByRepoDecrypted :: (DB es, Log :> es) => ByteString -> Text -> Text -> Eff es (Maybe GitHubSync)
getGitHubSyncByRepoDecrypted encKey ownerVal repoVal = withDecryption encKey (ownerVal, repoVal) $ getGitHubSyncByRepo ownerVal repoVal


-- | Insert a new GitHub sync config using PAT authentication
insertGitHubSync :: DB es => ByteString -> ProjectId -> Text -> Text -> Text -> Text -> Maybe Text -> Text -> Eff es (Maybe GitHubSync)
insertGitHubSync encKey pid ownerVal repoVal branchVal token webhookSecretVal prefix =
  listToMaybe <$> PG.query q (pid, ownerVal, repoVal, branchVal, encryptToken encKey token, webhookSecretVal, prefix)
  where
    q =
      [sql| INSERT INTO projects.github_sync (project_id, owner, repo, branch, access_token, webhook_secret, path_prefix)
              VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING * |]


-- | Insert a new GitHub sync config using GitHub App installation
insertGitHubAppSync :: DB es => ProjectId -> Int64 -> Text -> Text -> Text -> Text -> Eff es (Maybe GitHubSync)
insertGitHubAppSync pid instId ownerVal repoVal branchVal prefix =
  listToMaybe <$> PG.query q (pid, ownerVal, repoVal, branchVal, instId, prefix)
  where
    q =
      [sql| INSERT INTO projects.github_sync (project_id, owner, repo, branch, installation_id, path_prefix)
              VALUES (?, ?, ?, ?, ?, ?) RETURNING * |]


-- | Get GitHub sync by installation ID (for webhook handling)
getGitHubSyncByInstallation :: DB es => Int64 -> Eff es (Maybe GitHubSync)
getGitHubSyncByInstallation instId = listToMaybe <$> PG.query q (Only instId)
  where
    q = [sql| SELECT * FROM projects.github_sync WHERE installation_id = ? |]


updateGitHubSync :: DB es => ByteString -> GitHubSyncId -> Text -> Text -> Text -> Text -> Bool -> Eff es (Maybe GitHubSync)
updateGitHubSync encKey sid ownerVal repoVal branchVal token enabled =
  listToMaybe <$> PG.query q (ownerVal, repoVal, branchVal, encryptToken encKey token, enabled, sid)
  where
    q =
      [sql| UPDATE projects.github_sync SET owner = ?, repo = ?, branch = ?, access_token = ?, sync_enabled = ?, updated_at = now()
              WHERE id = ? RETURNING * |]


updateGitHubSyncKeepToken :: DB es => GitHubSyncId -> Text -> Text -> Text -> Bool -> Eff es (Maybe GitHubSync)
updateGitHubSyncKeepToken sid ownerVal repoVal branchVal enabled =
  listToMaybe <$> PG.query q (ownerVal, repoVal, branchVal, enabled, sid)
  where
    q =
      [sql| UPDATE projects.github_sync SET owner = ?, repo = ?, branch = ?, sync_enabled = ?, updated_at = now()
              WHERE id = ? RETURNING * |]


-- | Update repo selection for GitHub App installation
updateGitHubSyncRepo :: DB es => GitHubSyncId -> Text -> Text -> Text -> Text -> Eff es (Maybe GitHubSync)
updateGitHubSyncRepo sid ownerVal repoVal branchVal prefix =
  listToMaybe <$> PG.query q (ownerVal, repoVal, branchVal, prefix, sid)
  where
    q =
      [sql| UPDATE projects.github_sync SET owner = ?, repo = ?, branch = ?, path_prefix = ?, updated_at = now()
              WHERE id = ? RETURNING * |]


updateLastTreeSha :: DB es => GitHubSyncId -> Text -> Eff es Int64
updateLastTreeSha sid sha = PG.execute q (sha, sid)
  where
    q = [sql|UPDATE projects.github_sync SET last_tree_sha = ?, updated_at = now() WHERE id = ?|]


deleteGitHubSync :: DB es => GitHubSyncId -> Eff es Int64
deleteGitHubSync sid = PG.execute q (Only sid)
  where
    q = [sql|DELETE FROM projects.github_sync WHERE id = ?|]


getDashboardGitState :: DB es => ProjectId -> Eff es (M.Map Text (DashboardId, Text))
getDashboardGitState pid = M.fromList . fmap (\(did, path, sha) -> (path, (did, sha))) <$> PG.query q (Only pid)
  where
    q = [sql|SELECT id, file_path, file_sha FROM projects.dashboards WHERE project_id = ? AND file_path IS NOT NULL AND file_sha IS NOT NULL|]


updateDashboardGitInfo :: DB es => DashboardId -> Text -> Text -> Eff es Int64
updateDashboardGitInfo did path sha = PG.execute q (path, sha, did)
  where
    q = [sql|UPDATE projects.dashboards SET file_path = ?, file_sha = ?, updated_at = now() WHERE id = ?|]


-- GitHub API Operations
fetchGitTree :: (IOE :> es, W.HTTP :> es) => Text -> GitHubSync -> Eff es (Either Text (Text, [TreeEntry]))
fetchGitTree token sync = do
  let url = "https://api.github.com/repos/" <> sync.owner <> "/" <> sync.repo <> "/git/trees/" <> sync.branch <> "?recursive=1"
  result <- try $ W.getWith (githubOpts token) (toString url)
  pure $ case result of
    Left (HttpExceptionRequest _ (StatusCodeException resp _)) | statusCode (responseStatus resp) == 404 -> Right ("", []) -- Empty repo = empty tree
    Left (err :: HttpException) -> Left $ formatHttpError err
    Right resp ->
      let body = resp ^. W.responseBody
       in case (body ^? key "sha" . _String, body ^? key "tree" . _Array) of
            (Just treeSha, Just entries) -> Right (treeSha, mapMaybe (parseMaybe AE.parseJSON) $ V.toList entries)
            _ | body ^? key "truncated" . _Bool == Just True -> Left "Repository too large (>100k files)"
            _ -> Left $ "Invalid tree response: " <> decodeUtf8 body


fetchFileContent :: (IOE :> es, W.HTTP :> es) => Text -> GitHubSync -> Text -> Eff es (Either Text ByteString)
fetchFileContent token sync path = do
  let url = "https://api.github.com/repos/" <> sync.owner <> "/" <> sync.repo <> "/contents/" <> path <> "?ref=" <> sync.branch
  result <- try $ W.getWith (githubOpts token) (toString url)
  pure $ case result of
    Left (err :: HttpException) -> Left $ formatHttpError err
    Right resp -> case resp ^. W.responseBody . key "content" . _String of
      "" -> Left "No content field"
      b64Content -> first (toText . show) $ B64.decodeBase64Untyped $ encodeUtf8 $ T.filter (/= '\n') b64Content


pushFileToGit :: (IOE :> es, W.HTTP :> es) => Text -> GitHubSync -> Text -> ByteString -> Maybe Text -> Text -> Eff es (Either Text Text)
pushFileToGit token sync path content existingSha message = do
  let url = "https://api.github.com/repos/" <> sync.owner <> "/" <> sync.repo <> "/contents/" <> path
      b64Content = extractBase64 $ B64.encodeBase64 content
      payload =
        AE.object
          $ catMaybes
            [ Just $ "message" AE..= message
            , Just $ "content" AE..= b64Content
            , Just $ "branch" AE..= sync.branch
            , ("sha" AE..=) <$> existingSha
            ]
  result <- try $ W.putWith (githubOpts token) (toString url) payload
  pure $ case result of
    Left (err :: HttpException) -> Left $ formatHttpError err
    Right resp -> maybeToRight "No sha in response" $ resp ^? W.responseBody . key "content" . key "sha" . _String


formatHttpError :: HttpException -> Text
formatHttpError (HttpExceptionRequest _ content) = "HTTP request failed: " <> toText (show content)
formatHttpError (InvalidUrlException url reason) = "Invalid URL (" <> toText url <> "): " <> toText reason


githubOpts :: Text -> W.Options
githubOpts token =
  W.defaults
    & W.header "Authorization"
    .~ [encodeUtf8 $ "Bearer " <> token]
      & W.header "Accept"
    .~ ["application/vnd.github+json"]
      & W.header "User-Agent"
    .~ ["Monoscope"]
      & W.header "X-GitHub-Api-Version"
    .~ ["2022-11-28"]


-- | Detect the default branch of a repo, or return "main" for empty repos
-- Tries to get repo info first, falls back to checking main/master branches
detectDefaultBranch :: (IOE :> es, W.HTTP :> es) => Text -> Text -> Text -> Eff es Text
detectDefaultBranch token owner repo = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repo
  result <- try $ W.getWith (githubOpts token) (toString repoUrl)
  pure $ case result of
    Left (_ :: HttpException) -> "main" -- Default to main if can't access repo
    Right resp -> fromMaybe "main" $ resp ^? W.responseBody . key "default_branch" . _String


-- Constants
yamlExtensions :: [Text]
yamlExtensions = [".yaml", ".yml"]


-- | Get the dashboards folder path including prefix
getDashboardsPath :: GitHubSync -> Text
getDashboardsPath sync
  | T.null sync.pathPrefix = "dashboards/"
  | otherwise = sync.pathPrefix <> "/dashboards/"


isDashboardFile :: Text -> TreeEntry -> Bool
isDashboardFile prefix e = e._teType == "blob" && prefix `T.isPrefixOf` e.path && any (`T.isSuffixOf` e.path) yamlExtensions


-- Sync Logic
buildSyncPlan :: Text -> [TreeEntry] -> M.Map Text (DashboardId, Text) -> [SyncAction]
buildSyncPlan prefix entries dbState = creates <> updates <> deletes
  where
    gitFiles = M.fromList [(e.path, e.sha) | e <- entries, isDashboardFile prefix e]
    creates = M.foldMapWithKey (\p s -> [SyncCreate p s]) $ gitFiles `M.difference` dbState
    updates = M.foldMapWithKey (\p s -> case M.lookup p dbState of Just (rid, oldSha) | s /= oldSha -> [SyncUpdate p s rid]; _ -> []) gitFiles
    deletes = M.foldMapWithKey (\p (rid, _) -> [SyncDelete p rid]) $ dbState `M.difference` gitFiles


dashboardToYaml :: Dashboard -> ByteString
dashboardToYaml = Yaml.encode


yamlToDashboard :: ByteString -> Either Text Dashboard
yamlToDashboard = first (toText . show) . Yaml.decodeEither'


-- | Convert dashboard title to kebab-case file path
titleToFilePath :: Text -> Text
titleToFilePath = (<> ".yaml") . toText . toKebab . fromAny . toString . T.strip


-- | Compute Git blob SHA (SHA1 of "blob <size>\0<content>")
computeContentSha :: ByteString -> Text
computeContentSha content =
  let blobHeader = "blob " <> show (BS.length content) <> "\0"
   in decodeUtf8 $ B16.encode $ BA.convert (hash (encodeUtf8 blobHeader <> content) :: Digest SHA1)


-- | Build a Dashboard schema with title, tags, and team handles populated
buildSchemaWithMeta :: Maybe Dashboard -> Text -> [Text] -> [Text] -> Dashboard
buildSchemaWithMeta schemaM title tags teamHandles =
  fromMaybe def schemaM
    & #title
    ?~ title
      & #tags
    ?~ tags
      & #teams
    ?~ teamHandles
