module Models.Projects.GitSync (
  GitHubSync (..),
  GitHubSyncId,
  TreeEntry (..),
  SyncAction (..),
  getGitHubSync,
  getGitHubSyncDecrypted,
  getGitHubSyncByRepo,
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
  -- GitHub App integration
  generateAppJWT,
  getInstallationToken,
  listInstallationRepos,
  GitHubRepo (..),
  InstallationToken (..),
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
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Data.Effectful.Hasql qualified as Hasql
import Data.Yaml qualified as Yaml
import Database.PostgreSQL.Entity.Types (Entity, CamelToSnake, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Deriving.Aeson qualified as DAE
import Effectful (Eff, IOE, (:>))
import Effectful.Log (Log)
import Hasql.Interpolate qualified as HI
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Jose.Jwa (JwsAlg (RS256))
import Jose.Jws qualified as Jws
import Jose.Jwt (Jwt (..))
import Models.Projects.Dashboards (Dashboard, DashboardId)
import Models.Projects.ProjectApiKeys (decryptAPIKey, encryptAPIKey)
import Models.Projects.Projects (ProjectId)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), responseStatus)
import Network.HTTP.Types.Status (statusCode)
import Pkg.DeriveUtils (DB, UUIDId (..), selectFrom)
import Relude
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Logging (logWarn)
import Text.Casing (fromAny, toKebab)
import UnliftIO.Exception (try)
import "base64" Data.ByteString.Base64 qualified as B64
import "crypton-x509" Data.X509 (PrivKey (..))
import "crypton-x509-store" Data.X509.File (readKeyFile)
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
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "projects", TableName "github_sync", PrimaryKey "id", FieldModifiers '[CamelToSnake]] GitHubSync)


instance Default GitHubSync where
  def = GitHubSync (UUIDId UUID.nil) (UUIDId UUID.nil) "" "" "main" Nothing Nothing "" Nothing Nothing True epoch epoch
    where
      epoch = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)


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
  | SyncRename {path :: Text, sha :: Text, resourceId :: DashboardId} -- File moved/renamed, same content
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
getGitHubSync pid = Hasql.interp
  (selectFrom @GitHubSync <> [HI.sql| WHERE project_id = #{pid} |])


-- | Helper to decrypt sync config, logging on failure
withDecryption :: (AE.ToJSON ctx, DB es, Log :> es) => ByteString -> ctx -> Eff es (Maybe GitHubSync) -> Eff es (Maybe GitHubSync)
withDecryption encKey ctx fetch =
  fetch >>= maybe (pure Nothing) (either (\err -> logWarn "GitHub sync token decryption failed" (ctx, err) $> Nothing) (pure . Just) . decryptSync encKey)


getGitHubSyncDecrypted :: (DB es, Log :> es) => ByteString -> ProjectId -> Eff es (Maybe GitHubSync)
getGitHubSyncDecrypted encKey pid = withDecryption encKey pid $ getGitHubSync pid


getGitHubSyncByRepo :: DB es => Text -> Text -> Eff es (Maybe GitHubSync)
getGitHubSyncByRepo owner repo = Hasql.interp
  (selectFrom @GitHubSync <> [HI.sql| WHERE owner = #{owner} AND repo = #{repo} |])


-- | Insert a new GitHub sync config using PAT authentication
insertGitHubSync :: DB es => ByteString -> ProjectId -> Text -> Text -> Text -> Text -> Maybe Text -> Text -> Eff es (Maybe GitHubSync)
insertGitHubSync encKey pid ownerVal repoVal branchVal token webhookSecretVal prefix = do
  let encToken = encryptToken encKey token
  Hasql.interp
    [HI.sql| INSERT INTO projects.github_sync (project_id, owner, repo, branch, access_token, webhook_secret, path_prefix)
             VALUES (#{pid}, #{ownerVal}, #{repoVal}, #{branchVal}, #{encToken}, #{webhookSecretVal}, #{prefix}) RETURNING * |]


-- | Insert a new GitHub sync config using GitHub App installation
insertGitHubAppSync :: DB es => ProjectId -> Int64 -> Text -> Text -> Text -> Text -> Eff es (Maybe GitHubSync)
insertGitHubAppSync pid instId ownerVal repoVal branchVal prefix = Hasql.interp
  [HI.sql| INSERT INTO projects.github_sync (project_id, owner, repo, branch, installation_id, path_prefix)
           VALUES (#{pid}, #{ownerVal}, #{repoVal}, #{branchVal}, #{instId}, #{prefix}) RETURNING * |]


updateGitHubSync :: (DB es, Time :> es) => ByteString -> GitHubSyncId -> Text -> Text -> Text -> Text -> Bool -> Eff es (Maybe GitHubSync)
updateGitHubSync encKey sid ownerVal repoVal branchVal token enabled = do
  now <- Time.currentTime
  let encToken = encryptToken encKey token
  Hasql.interp
    [HI.sql| UPDATE projects.github_sync SET owner = #{ownerVal}, repo = #{repoVal}, branch = #{branchVal}, access_token = #{encToken}, sync_enabled = #{enabled}, updated_at = #{now}
             WHERE id = #{sid} RETURNING * |]


updateGitHubSyncKeepToken :: (DB es, Time :> es) => GitHubSyncId -> Text -> Text -> Text -> Bool -> Eff es (Maybe GitHubSync)
updateGitHubSyncKeepToken sid ownerVal repoVal branchVal enabled = do
  now <- Time.currentTime
  Hasql.interp
    [HI.sql| UPDATE projects.github_sync SET owner = #{ownerVal}, repo = #{repoVal}, branch = #{branchVal}, sync_enabled = #{enabled}, updated_at = #{now}
             WHERE id = #{sid} RETURNING * |]


-- | Update repo selection for GitHub App installation
updateGitHubSyncRepo :: (DB es, Time :> es) => GitHubSyncId -> Text -> Text -> Text -> Text -> Eff es (Maybe GitHubSync)
updateGitHubSyncRepo sid ownerVal repoVal branchVal prefix = do
  now <- Time.currentTime
  Hasql.interp
    [HI.sql| UPDATE projects.github_sync SET owner = #{ownerVal}, repo = #{repoVal}, branch = #{branchVal}, path_prefix = #{prefix}, updated_at = #{now}
             WHERE id = #{sid} RETURNING * |]


updateLastTreeSha :: (DB es, Time :> es) => GitHubSyncId -> Text -> Eff es Int64
updateLastTreeSha sid treeSha = do
  now <- Time.currentTime
  Hasql.interpExecute [HI.sql| UPDATE projects.github_sync SET last_tree_sha = #{treeSha}, updated_at = #{now} WHERE id = #{sid} |]


deleteGitHubSync :: DB es => GitHubSyncId -> Eff es Int64
deleteGitHubSync sid = Hasql.interpExecute
  [HI.sql| DELETE FROM projects.github_sync WHERE id = #{sid} |]


getDashboardGitState :: DB es => ProjectId -> Eff es (M.Map Text (DashboardId, Text))
getDashboardGitState pid = M.fromList . fmap (\(did, path, fsha) -> (path, (did, fsha))) <$> Hasql.interp
  [HI.sql| SELECT id, file_path, file_sha FROM projects.dashboards WHERE project_id = #{pid} AND file_path IS NOT NULL AND file_sha IS NOT NULL |]


updateDashboardGitInfo :: (DB es, Time :> es) => DashboardId -> Text -> Text -> Eff es Int64
updateDashboardGitInfo did path fsha = do
  now <- Time.currentTime
  Hasql.interpExecute [HI.sql| UPDATE projects.dashboards SET file_path = #{path}, file_sha = #{fsha}, updated_at = #{now} WHERE id = #{did} |]


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


-- | Push a file to GitHub. Returns (fileSha, treeSha) on success.
pushFileToGit :: (IOE :> es, W.HTTP :> es) => Text -> GitHubSync -> Text -> ByteString -> Maybe Text -> Text -> Eff es (Either Text (Text, Text))
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
    Right resp ->
      let fileSha = resp ^? W.responseBody . key "content" . key "sha" . _String
          treeSha = resp ^? W.responseBody . key "commit" . key "tree" . key "sha" . _String
       in case (fileSha, treeSha) of
            (Just f, Just t) -> Right (f, t)
            _ -> Left "Missing sha in response"


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
buildSyncPlan prefix entries dbState = renames <> realCreates <> updates <> realDeletes
  where
    stripPrefix p = fromMaybe p $ T.stripPrefix prefix p
    gitFiles = M.fromList [(stripPrefix e.path, (e.path, e.sha)) | e <- entries, isDashboardFile prefix e]
    newFiles = gitFiles `M.difference` dbState
    removedFiles = dbState `M.difference` (fst <$> gitFiles)
    -- Build map of SHA -> (DashboardId, oldPath) for removed files to detect renames
    removedBySha = M.fromList [(sha, (rid, p)) | (p, (rid, sha)) <- M.toList removedFiles]
    -- Detect renames: new files whose SHA matches a removed file
    (renameActions, unmatchedCreates) = M.foldrWithKey checkRename ([], []) newFiles
    checkRename _ (fullPath, sha) (rens, crs) = case M.lookup sha removedBySha of
      Just (rid, _) -> (SyncRename fullPath sha rid : rens, crs)
      Nothing -> (rens, SyncCreate fullPath sha : crs)
    renames = renameActions
    realCreates = unmatchedCreates
    -- Remove renamed files from deletes
    renamedIds = [rid | SyncRename _ _ rid <- renames]
    realDeletes = [SyncDelete p rid | (p, (rid, _)) <- M.toList removedFiles, rid `notElem` renamedIds]
    updates = M.foldMapWithKey (\relPath (fullPath, s) -> case M.lookup relPath dbState of Just (rid, oldSha) | s /= oldSha -> [SyncUpdate fullPath s rid]; _ -> []) gitFiles


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


---------------------------------
-- GitHub App Integration

data GitHubRepo = GitHubRepo
  { id :: Int64
  , name :: Text
  , fullName :: Text
  , private :: Bool
  , defaultBranch :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] GitHubRepo


data InstallationToken = InstallationToken
  { token :: Text
  , expiresAt :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] InstallationToken


data ReposResponse = ReposResponse
  { totalCount :: Int
  , repositories :: [GitHubRepo]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] ReposResponse


generateAppJWT :: Text -> Text -> IO (Either Text Text)
generateAppJWT appId privateKeyB64 = do
  now <- round <$> getPOSIXTime :: IO Int64
  let iat = now - 60
      expTime = now + 300
      payload =
        AE.encode
          $ AE.object
            [ "iat" AE..= iat
            , "exp" AE..= expTime
            , "iss" AE..= appId
            ]

  case B64.decodeBase64Untyped (encodeUtf8 privateKeyB64) of
    Left err -> pure $ Left $ "Failed to decode base64: " <> err
    Right pemBytes ->
      withSystemTempFile "github-key.pem" $ \tmpPath h -> do
        BS.hPut h pemBytes
        hClose h
        keys <- readKeyFile tmpPath
        case keys of
          [] -> pure $ Left "No private key found in PEM"
          (PrivKeyRSA rsaKey : _) -> do
            result <- Jws.rsaEncode RS256 rsaKey (toStrict payload)
            pure $ case result of
              Left err -> Left $ "Failed to sign JWT: " <> toText (show err)
              Right (Jwt jwtBytes) -> Right $ decodeUtf8 jwtBytes
          _ -> pure $ Left "Unsupported key type (expected RSA)"


getInstallationToken :: (IOE :> es, W.HTTP :> es) => Text -> Text -> Int64 -> Eff es (Either Text InstallationToken)
getInstallationToken appId privateKeyB64 installationId = do
  jwtResult <- liftIO $ generateAppJWT appId privateKeyB64
  case jwtResult of
    Left err -> pure $ Left err
    Right jwt -> do
      let url = "https://api.github.com/app/installations/" <> show installationId <> "/access_tokens"
          opts =
            W.defaults
              & W.header "Authorization"
              .~ ["Bearer " <> encodeUtf8 jwt]
                & W.header "Accept"
              .~ ["application/vnd.github+json"]
                & W.header "X-GitHub-Api-Version"
              .~ ["2022-11-28"]
                & W.header "User-Agent"
              .~ ["Monoscope-App"]
      response <- W.postWith opts (toString url) ("" :: ByteString)
      let body = response ^. W.responseBody
      case AE.eitherDecode body of
        Left err -> pure $ Left $ "Failed to parse token response: " <> toText err
        Right token -> pure $ Right token


listInstallationRepos :: W.HTTP :> es => Text -> Eff es (Either Text [GitHubRepo])
listInstallationRepos accessToken = do
  let opts =
        W.defaults
          & W.header "Authorization"
          .~ ["Bearer " <> encodeUtf8 accessToken]
            & W.header "Accept"
          .~ ["application/vnd.github+json"]
            & W.header "X-GitHub-Api-Version"
          .~ ["2022-11-28"]
            & W.header "User-Agent"
          .~ ["Monoscope-App"]
  response <- W.getWith opts "https://api.github.com/installation/repositories?per_page=100"
  let body = response ^. W.responseBody
  case AE.eitherDecode body of
    Left err -> pure $ Left $ "Failed to parse repos response: " <> toText err
    Right (repos :: ReposResponse) -> pure $ Right repos.repositories
