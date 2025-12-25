{-# LANGUAGE PackageImports #-}

module Models.Projects.GitSync (
  GitHubSync (..),
  GitHubSyncId,
  TreeEntry (..),
  SyncAction (..),
  getGitHubSync,
  getGitHubSyncDecrypted,
  getGitHubSyncByRepo,
  getGitHubSyncByRepoDecrypted,
  insertGitHubSync,
  updateGitHubSync,
  updateGitHubSyncKeepToken,
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
) where

import Control.Lens ((.~), (?~), (^.))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEK
import Data.Aeson.Types (parseMaybe)
import Data.Base64.Types (extractBase64)
import Data.Default (def)
import Data.Effectful.Wreq qualified as W
import Data.Generics.Labels ()
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Deriving.Aeson qualified as DAE
import Effectful (Eff, IOE, (:>))
import Effectful.PostgreSQL qualified as PG
import Models.Projects.Dashboards (Dashboard, DashboardId)
import Models.Projects.ProjectApiKeys (decryptAPIKey, encryptAPIKey)
import Models.Projects.Projects (ProjectId)
import Network.HTTP.Client (HttpException (..))
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import System.DB (DB)
import Text.Casing (fromAny, toKebab)
import UnliftIO.Exception (try)
import "base16-bytestring" Data.ByteString.Base16 qualified as B16
import "base64" Data.ByteString.Base64 qualified as B64
import "cryptonite" Crypto.Hash (Digest, SHA256, hash)
import "memory" Data.ByteArray qualified as BA


type GitHubSyncId = UUIDId "github_sync"


data GitHubSync = GitHubSync
  { id :: GitHubSyncId
  , projectId :: ProjectId
  , owner :: Text
  , repo :: Text
  , branch :: Text
  , accessToken :: Text
  , webhookSecret :: Maybe Text
  , lastTreeSha :: Maybe Text
  , syncEnabled :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)


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
encryptToken key token = extractBase64 $ B64.encodeBase64 $ encryptAPIKey key (encodeUtf8 token)


-- | Decrypt an access token. Returns Nothing if decryption fails (invalid format or key).
-- SECURITY: Never falls back to plaintext - callers must handle Nothing appropriately.
decryptToken :: ByteString -> Text -> Maybe Text
decryptToken key encryptedB64 = case B64.decodeBase64Untyped (encodeUtf8 encryptedB64) of
  Left _ -> Nothing
  Right encrypted -> Just $ decodeUtf8 $ decryptAPIKey key encrypted


-- | Decrypt a GitHubSync's access token. Returns Nothing if decryption fails.
decryptSync :: ByteString -> GitHubSync -> Maybe GitHubSync
decryptSync key sync = (\token -> sync{accessToken = token}) <$> decryptToken key sync.accessToken


-- DB Operations
githubSyncCols :: ByteString
githubSyncCols = "id, project_id, owner, repo, branch, access_token, webhook_secret, last_tree_sha, sync_enabled, created_at, updated_at"


getGitHubSync :: DB es => ProjectId -> Eff es (Maybe GitHubSync)
getGitHubSync pid = listToMaybe <$> PG.query (Query $ "SELECT " <> githubSyncCols <> " FROM projects.github_sync WHERE project_id = ?") (Only pid)


getGitHubSyncDecrypted :: DB es => ByteString -> ProjectId -> Eff es (Maybe GitHubSync)
getGitHubSyncDecrypted key pid = (decryptSync key =<<) <$> getGitHubSync pid


getGitHubSyncByRepo :: DB es => Text -> Text -> Eff es (Maybe GitHubSync)
getGitHubSyncByRepo owner repo = listToMaybe <$> PG.query (Query $ "SELECT " <> githubSyncCols <> " FROM projects.github_sync WHERE owner = ? AND repo = ?") (owner, repo)


getGitHubSyncByRepoDecrypted :: DB es => ByteString -> Text -> Text -> Eff es (Maybe GitHubSync)
getGitHubSyncByRepoDecrypted key owner repo = (decryptSync key =<<) <$> getGitHubSyncByRepo owner repo


insertGitHubSync :: DB es => ByteString -> ProjectId -> Text -> Text -> Text -> Text -> Maybe Text -> Eff es (Maybe GitHubSync)
insertGitHubSync key pid owner repo branch token webhookSecret =
  listToMaybe <$> PG.query (Query $ "INSERT INTO projects.github_sync (project_id, owner, repo, branch, access_token, webhook_secret) VALUES (?, ?, ?, ?, ?, ?) RETURNING " <> githubSyncCols) (pid, owner, repo, branch, encryptToken key token, webhookSecret)


updateGitHubSync :: DB es => ByteString -> GitHubSyncId -> Text -> Text -> Text -> Text -> Bool -> Eff es (Maybe GitHubSync)
updateGitHubSync key sid owner repo branch token enabled =
  listToMaybe <$> PG.query (Query $ "UPDATE projects.github_sync SET owner = ?, repo = ?, branch = ?, access_token = ?, sync_enabled = ?, updated_at = now() WHERE id = ? RETURNING " <> githubSyncCols) (owner, repo, branch, encryptToken key token, enabled, sid)


updateGitHubSyncKeepToken :: DB es => GitHubSyncId -> Text -> Text -> Text -> Bool -> Eff es (Maybe GitHubSync)
updateGitHubSyncKeepToken sid owner repo branch enabled =
  listToMaybe <$> PG.query (Query $ "UPDATE projects.github_sync SET owner = ?, repo = ?, branch = ?, sync_enabled = ?, updated_at = now() WHERE id = ? RETURNING " <> githubSyncCols) (owner, repo, branch, enabled, sid)


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
fetchGitTree :: (IOE :> es, W.HTTP :> es) => GitHubSync -> Eff es (Either Text (Text, [TreeEntry]))
fetchGitTree sync = do
  let url = "https://api.github.com/repos/" <> sync.owner <> "/" <> sync.repo <> "/git/trees/" <> sync.branch <> "?recursive=1"
  result <- try $ W.getWith (githubOpts sync.accessToken) (toString url)
  pure $ case result of
    Left (err :: HttpException) -> Left $ formatHttpError err
    Right resp -> case AE.decode (resp ^. W.responseBody) of
      Nothing -> Left "Failed to parse tree response"
      Just obj -> case (AEK.lookup "sha" obj, AEK.lookup "tree" obj) of
        (Just (AE.String treeSha), Just (AE.Array entries)) ->
          Right (treeSha, mapMaybe (parseMaybe AE.parseJSON) $ V.toList entries)
        _ -> case AEK.lookup "truncated" obj of
          Just (AE.Bool True) -> Left "Repository too large (>100k files)"
          _ -> Left $ "Invalid tree response: " <> decodeUtf8 (toStrict $ resp ^. W.responseBody)


fetchFileContent :: (IOE :> es, W.HTTP :> es) => GitHubSync -> Text -> Eff es (Either Text ByteString)
fetchFileContent sync path = do
  let url = "https://api.github.com/repos/" <> sync.owner <> "/" <> sync.repo <> "/contents/" <> path <> "?ref=" <> sync.branch
  result <- try $ W.getWith (githubOpts sync.accessToken) (toString url)
  pure $ case result of
    Left (err :: HttpException) -> Left $ formatHttpError err
    Right resp -> case AE.decode (resp ^. W.responseBody) of
      Nothing -> Left "Failed to parse content response"
      Just obj -> case AEK.lookup "content" obj of
        Just (AE.String b64Content) ->
          let cleaned = T.replace "\n" "" b64Content
           in first (toText . show) $ B64.decodeBase64Untyped (encodeUtf8 cleaned)
        _ -> Left "No content field"


pushFileToGit :: (IOE :> es, W.HTTP :> es) => GitHubSync -> Text -> ByteString -> Maybe Text -> Text -> Eff es (Either Text Text)
pushFileToGit sync path content existingSha message = do
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
  result <- try $ W.putWith (githubOpts sync.accessToken) (toString url) payload
  pure $ case result of
    Left (err :: HttpException) -> Left $ formatHttpError err
    Right resp -> case AE.decode (resp ^. W.responseBody) of
      Nothing -> Left "Failed to parse push response"
      Just obj -> case AEK.lookup "content" obj >>= \c -> AEK.lookup "sha" c of
        Just (AE.String newSha) -> Right newSha
        _ -> Left "No sha in response"


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


-- Constants
dashboardsPrefix :: Text
dashboardsPrefix = "dashboards/"


yamlExtensions :: [Text]
yamlExtensions = [".yaml", ".yml"]


isDashboardFile :: TreeEntry -> Bool
isDashboardFile e = e._teType == "blob" && dashboardsPrefix `T.isPrefixOf` e.path && any (`T.isSuffixOf` e.path) yamlExtensions


-- Sync Logic
buildSyncPlan :: [TreeEntry] -> M.Map Text (DashboardId, Text) -> [SyncAction]
buildSyncPlan entries dbState = creates <> updates <> deletes
  where
    gitFiles = M.fromList [(e.path, e.sha) | e <- entries, isDashboardFile e]
    creates = [SyncCreate p s | (p, s) <- M.toList $ gitFiles `M.difference` dbState]
    updates = [SyncUpdate p s rid | (p, s) <- M.toList gitFiles, Just (rid, oldSha) <- [M.lookup p dbState], s /= oldSha]
    deletes = [SyncDelete p rid | (p, (rid, _)) <- M.toList $ dbState `M.difference` gitFiles]


dashboardToYaml :: Dashboard -> ByteString
dashboardToYaml = Yaml.encode


yamlToDashboard :: ByteString -> Either Text Dashboard
yamlToDashboard = first (toText . show) . Yaml.decodeEither'


-- | Convert dashboard title to kebab-case file path
titleToFilePath :: Text -> Text
titleToFilePath title = toText (toKebab $ fromAny $ toString $ T.strip title) <> ".yaml"


-- | Compute SHA256 hash of content and return as hex string
computeContentSha :: ByteString -> Text
computeContentSha content = decodeUtf8 $ B16.encode $ BA.convert (hash content :: Digest SHA256)


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
