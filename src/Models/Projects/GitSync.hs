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
  RepoRef (..),
  GitCreds (..),
  mkGitCreds,
  syncRepoRef,
  syncCreds,
  GitHubCredential (..),
  GitHubCredentialId,
  credentialCreds,
  getGitHubCredentials,
  getGitHubCredential,
  upsertGitHubCredential,
  buildSyncPlan,
  dashboardToYaml,
  yamlToDashboard,
  titleToFilePath,
  computeContentSha,
  buildSchemaWithMeta,
  getDashboardsPath,
  detectDefaultBranch,
  -- GitHub App integration
  githubToken,
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
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Wreq qualified as W
import Data.Generics.Labels ()
import Data.Generics.Product.Fields qualified as GL
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Deriving.Aeson qualified as DAE
import Effectful (Eff, IOE, (:>))
import Effectful.Log (Log)
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
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
import Relude.Extra.Bifunctor (bimapF, firstF)
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


type GitHubCredentialId = UUIDId "github_credential"


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
      epoch = posixSecondsToUTCTime 0


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
  bimap ("Base64 decode failed: " <>) (decodeUtf8 . decryptAPIKey encKey)
    $ B64.decodeBase64Untyped (encodeUtf8 encryptedB64)


-- | Decrypt a row's stored PAT in place. A row with no token is a GitHub App installation
-- and comes back unchanged. Generic over the row so a sync and a credential — which hold
-- the token for the same reason — do not each need their own copy of this.
-- @field'@ rather than the @#accessToken@ label: the label's instance cannot be discharged
-- against a row type that is still a variable here.
decryptAccessToken :: GL.HasField' "accessToken" a (Maybe Text) => ByteString -> a -> Either Text a
decryptAccessToken encKey row = case row ^. GL.field' @"accessToken" of
  Nothing -> Right row
  Just token -> decryptToken encKey token <&> \plain -> row & GL.field' @"accessToken" ?~ plain


-- | Drop a row whose PAT would not decrypt, rather than returning it with the ciphertext
-- still in place — that ciphertext would be sent to GitHub as a password.
decryptedOr :: (IOE :> es, Log :> es) => Text -> ProjectId -> Either Text a -> Eff es (Maybe a)
decryptedOr what pid = either (\err -> logWarn ("GitHub " <> what <> " token decryption failed") (pid, err) $> Nothing) (pure . Just)


-- | Which repo, at which revision. A 'GitHubSync' denotes one and so does a code mapping;
-- the API calls take this rather than either record, so "read a file from GitHub" has one
-- implementation instead of one per caller that happens to hold a different type.
data RepoRef = RepoRef {owner :: Text, repo :: Text, ref :: Text}
  deriving stock (Eq, Generic, Show)


-- | How to authenticate against an account. The App installation covers every repo in the
-- account, which is why this is separable from 'RepoRef' at all.
--
-- Two constructors rather than two nullable fields: the rows these come from are
-- @CHECK (installation_id IS NOT NULL OR access_token IS NOT NULL)@, so "neither" cannot
-- reach us and a token function should not have an arm claiming it can. The App wins over a
-- PAT when a row somehow carries both, and 'mkGitCreds' is the one place that decides so.
data GitCreds = AppInstallation Int64 | PersonalToken Text
  deriving stock (Eq, Generic, Show)


-- | Parse the two nullable auth columns into the two states they actually denote.
--
-- >>> mkGitCreds (Just 42) (Just "ghp_x")
-- Just (AppInstallation 42)
-- >>> mkGitCreds Nothing (Just "ghp_x")
-- Just (PersonalToken "ghp_x")
-- >>> mkGitCreds Nothing Nothing
-- Nothing
mkGitCreds :: Maybe Int64 -> Maybe Text -> Maybe GitCreds
mkGitCreds instId token = AppInstallation <$> instId <|> PersonalToken <$> token


syncRepoRef :: GitHubSync -> RepoRef
syncRepoRef s = RepoRef s.owner s.repo s.branch


syncCreds :: GitHubSync -> Maybe GitCreds
syncCreds s = mkGitCreds s.installationId s.accessToken


-- | A grant to read an account's repositories, held per project.
--
-- Separate from 'GitHubSync' because they answer different questions: a sync row is /the/
-- repo monoscope keeps its own YAML in (one per project), while a project's services live in
-- as many repos as they like. See migration 0124.
data GitHubCredential = GitHubCredential
  { id :: GitHubCredentialId
  , projectId :: ProjectId
  , account :: Text
  -- ^ The org or user login the installation covers.
  , installationId :: Maybe Int64
  , accessToken :: Maybe Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "projects", TableName "github_credentials", PrimaryKey "id", FieldModifiers '[CamelToSnake]] GitHubCredential)


credentialCreds :: GitHubCredential -> Maybe GitCreds
credentialCreds c = mkGitCreds c.installationId c.accessToken


getGitHubCredentials :: DB es => ProjectId -> Eff es [GitHubCredential]
getGitHubCredentials pid = Hasql.interp (selectFrom @GitHubCredential <> [HI.sql| WHERE project_id = #{pid} ORDER BY account |])


-- | One credential, with its PAT decrypted.
getGitHubCredential :: (DB es, Log :> es) => ByteString -> ProjectId -> GitHubCredentialId -> Eff es (Maybe GitHubCredential)
getGitHubCredential encKey pid cid =
  Hasql.interp (selectFrom @GitHubCredential <> [HI.sql| WHERE project_id = #{pid} AND id = #{cid} |])
    >>= maybe (pure Nothing) (decryptedOr "credential" pid . decryptAccessToken encKey)


-- | Record a grant for an account, or update the one already held for it. Idempotent because
-- the same installation arriving twice is the same grant, not a second one.
upsertGitHubCredential :: DB es => ByteString -> ProjectId -> Text -> Maybe Int64 -> Maybe Text -> Eff es (Maybe GitHubCredential)
upsertGitHubCredential encKey pid account instId token =
  Hasql.interp
    [HI.sql| INSERT INTO projects.github_credentials (project_id, account, installation_id, access_token)
             VALUES (#{pid}, #{account}, #{instId}, #{encryptToken encKey <$> token})
             ON CONFLICT (project_id, account)
             DO UPDATE SET installation_id = EXCLUDED.installation_id, access_token = COALESCE(EXCLUDED.access_token, projects.github_credentials.access_token), updated_at = now()
             RETURNING * |]


-- DB Operations
getGitHubSync :: DB es => ProjectId -> Eff es (Maybe GitHubSync)
getGitHubSync pid =
  Hasql.interp
    (selectFrom @GitHubSync <> [HI.sql| WHERE project_id = #{pid} |])


getGitHubSyncDecrypted :: (DB es, Log :> es) => ByteString -> ProjectId -> Eff es (Maybe GitHubSync)
getGitHubSyncDecrypted encKey pid =
  getGitHubSync pid >>= maybe (pure Nothing) (decryptedOr "sync" pid . decryptAccessToken encKey)


getGitHubSyncByRepo :: DB es => Text -> Text -> Eff es (Maybe GitHubSync)
getGitHubSyncByRepo owner repo =
  Hasql.interp
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
insertGitHubAppSync pid instId ownerVal repoVal branchVal prefix =
  Hasql.interp
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
deleteGitHubSync sid =
  Hasql.interpExecute
    [HI.sql| DELETE FROM projects.github_sync WHERE id = #{sid} |]


getDashboardGitState :: DB es => ProjectId -> Eff es (M.Map Text (DashboardId, Text))
getDashboardGitState pid =
  M.fromList
    . fmap (\(did, path, fsha) -> (path, (did, fsha)))
    <$> Hasql.interp
      [HI.sql| SELECT id, file_path, file_sha FROM projects.dashboards WHERE project_id = #{pid} AND file_path IS NOT NULL AND file_sha IS NOT NULL |]


updateDashboardGitInfo :: (DB es, Time :> es) => DashboardId -> Text -> Text -> Eff es Int64
updateDashboardGitInfo did path fsha = do
  now <- Time.currentTime
  Hasql.interpExecute [HI.sql| UPDATE projects.dashboards SET file_path = #{path}, file_sha = #{fsha}, updated_at = #{now} WHERE id = #{did} |]


-- GitHub API Operations
fetchGitTree :: (IOE :> es, W.HTTP :> es) => Text -> RepoRef -> Eff es (Either Text (Text, [TreeEntry]))
fetchGitTree token r = do
  let url = "https://api.github.com/repos/" <> r.owner <> "/" <> r.repo <> "/git/trees/" <> r.ref <> "?recursive=1"
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


-- | The blob at @path@ in @r.ref@ — a branch name or a commit sha, since the contents API
-- takes either. Reading at the sha the telemetry reported is the difference between the
-- source that threw and the source as it is today.
fetchFileContent :: (IOE :> es, W.HTTP :> es) => Text -> RepoRef -> Text -> Eff es (Either Text ByteString)
fetchFileContent token r path = do
  let url = "https://api.github.com/repos/" <> r.owner <> "/" <> r.repo <> "/contents/" <> path <> "?ref=" <> r.ref
  result <- tryHttp $ W.getWith (githubOpts token) (toString url)
  pure $ result >>= \resp -> case resp ^. W.responseBody . key "content" . _String of
    "" -> Left "No content field"
    b64Content -> B64.decodeBase64Untyped $ encodeUtf8 $ T.filter (/= '\n') b64Content


-- | The credential to read or write this project's repo with: an App installation token when
-- the App is installed, else the stored PAT. Lives here rather than beside its first caller
-- because every GitHub call in the app needs it, and a second copy would be a second place to
-- get the App-before-PAT precedence wrong. Takes the app id and key rather than the whole
-- 'System.Config.EnvConfig' so this module stays a leaf of the config graph.
githubToken :: (IOE :> es, W.HTTP :> es) => Text -> Text -> GitCreds -> Eff es (Either Text Text)
githubToken appId privateKeyB64 = \case
  AppInstallation instId ->
    getInstallationToken appId privateKeyB64 instId <&> \case
      Left err -> Left $ "Failed to get installation token: " <> err
      Right tok -> Right tok.token
  PersonalToken token -> pure $ Right token


-- | Push a file to GitHub. Returns (fileSha, treeSha) on success.
pushFileToGit :: (IOE :> es, W.HTTP :> es) => Text -> RepoRef -> Text -> ByteString -> Maybe Text -> Text -> Eff es (Either Text (Text, Text))
pushFileToGit token r path content existingSha message = do
  let url = "https://api.github.com/repos/" <> r.owner <> "/" <> r.repo <> "/contents/" <> path
      payload =
        AE.object
          $ [ "message" AE..= message
            , "content" AE..= extractBase64 (B64.encodeBase64 content)
            , "branch" AE..= r.ref
            ]
          <> maybeToList (("sha" AE..=) <$> existingSha)
  result <- tryHttp $ W.putWith (githubOpts token) (toString url) payload
  pure $ result >>= \resp ->
    maybeToRight "Missing sha in response"
      $ (,)
      <$> (resp ^? W.responseBody . key "content" . key "sha" . _String)
      <*> (resp ^? W.responseBody . key "commit" . key "tree" . key "sha" . _String)


tryHttp :: IOE :> es => Eff es a -> Eff es (Either Text a)
tryHttp = firstF formatHttpError . try


formatHttpError :: HttpException -> Text
formatHttpError (HttpExceptionRequest _ content) = "HTTP request failed: " <> toText (show content)
formatHttpError (InvalidUrlException url reason) = "Invalid URL (" <> toText url <> "): " <> toText reason


githubOpts :: Text -> W.Options
githubOpts = githubOptsUA "Monoscope"


githubOptsUA :: ByteString -> Text -> W.Options
githubOptsUA ua token =
  W.defaults
    & W.header "Authorization"
    .~ [encodeUtf8 $ "Bearer " <> token]
      & W.header "Accept"
    .~ ["application/vnd.github+json"]
      & W.header "User-Agent"
    .~ [ua]
      & W.header "X-GitHub-Api-Version"
    .~ ["2022-11-28"]


-- | Detect the default branch of a repo, or return "main" for empty repos
detectDefaultBranch :: (IOE :> es, W.HTTP :> es) => Text -> Text -> Text -> Eff es Text
detectDefaultBranch token owner repo =
  tryHttp (W.getWith (githubOpts token) (toString $ "https://api.github.com/repos/" <> owner <> "/" <> repo))
    <&> either (const "main") (fromMaybe "main" . (^? W.responseBody . key "default_branch" . _String))


-- | Get the dashboards folder path including prefix
getDashboardsPath :: GitHubSync -> Text
getDashboardsPath sync
  | T.null sync.pathPrefix = "dashboards/"
  | otherwise = sync.pathPrefix <> "/dashboards/"


isDashboardFile :: Text -> TreeEntry -> Bool
isDashboardFile prefix e = e._teType == "blob" && prefix `T.isPrefixOf` e.path && any (`T.isSuffixOf` e.path) [".yaml", ".yml"]


-- Sync Logic
buildSyncPlan :: Text -> [TreeEntry] -> M.Map Text (DashboardId, Text) -> [SyncAction]
buildSyncPlan prefix entries dbState = renames <> creates <> updates <> deletes
  where
    gitFiles = M.fromList [(fromMaybe e.path $ T.stripPrefix prefix e.path, (e.path, e.sha)) | e <- entries, isDashboardFile prefix e]
    newFiles = gitFiles `M.difference` dbState
    removedFiles = dbState `M.difference` gitFiles
    removedBySha = M.fromList [(sha, rid) | (rid, sha) <- M.elems removedFiles]
    -- A new file whose SHA matches a removed file is a rename, not a create+delete
    (renames, creates) =
      partitionEithers
        [maybe (Right $ SyncCreate p s) (Left . SyncRename p s) (M.lookup s removedBySha) | (p, s) <- M.elems newFiles]
    renamedIds = [rid | SyncRename _ _ rid <- renames]
    deletes = [SyncDelete p rid | (p, (rid, _)) <- M.toList removedFiles, rid `notElem` renamedIds]
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
  decodeUtf8 $ B16.encode $ BA.convert (hash ("blob " <> show (BS.length content) <> "\0" <> content) :: Digest SHA1)


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


newtype ReposResponse = ReposResponse {repositories :: [GitHubRepo]}
  deriving stock (Generic)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] ReposResponse


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
        readKeyFile tmpPath >>= \case
          (PrivKeyRSA rsaKey : _) ->
            bimapF (("Failed to sign JWT: " <>) . toText . show) (\(Jwt jwtBytes) -> decodeUtf8 jwtBytes)
              $ Jws.rsaEncode RS256 rsaKey (toStrict payload)
          [] -> pure $ Left "No private key found in PEM"
          _ -> pure $ Left "Unsupported key type (expected RSA)"


getInstallationToken :: (IOE :> es, W.HTTP :> es) => Text -> Text -> Int64 -> Eff es (Either Text InstallationToken)
getInstallationToken appId privateKeyB64 installationId =
  liftIO (generateAppJWT appId privateKeyB64) >>= either (pure . Left) \jwt -> do
    let url = "https://api.github.com/app/installations/" <> show installationId <> "/access_tokens"
    response <- W.postWith (githubOptsUA "Monoscope-App" jwt) (toString url) ("" :: ByteString)
    pure $ decodeGitHub "token" $ response ^. W.responseBody


listInstallationRepos :: W.HTTP :> es => Text -> Eff es (Either Text [GitHubRepo])
listInstallationRepos accessToken = do
  response <- W.getWith (githubOptsUA "Monoscope-App" accessToken) "https://api.github.com/installation/repositories?per_page=100"
  pure $ (.repositories) <$> (decodeGitHub "repos" (response ^. W.responseBody) :: Either Text ReposResponse)


decodeGitHub :: AE.FromJSON a => Text -> LByteString -> Either Text a
decodeGitHub what = first (\err -> "Failed to parse " <> what <> " response: " <> toText err) . AE.eitherDecode
