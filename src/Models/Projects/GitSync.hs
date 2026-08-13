-- | The stored side of a git integration: which repository a project syncs its YAML with,
-- and which grants it can read source through.
--
-- The wire side — how to actually talk to each host — is "Pkg.Git", which this module
-- re-exports the vocabulary of so call sites need only one import. What stays here is
-- everything that touches the database, plus the GitHub App token flow, which has no
-- equivalent on the other hosts and so never became part of the host abstraction.
module Models.Projects.GitSync (
  GitHubSync (..),
  GitHubSyncId,
  SyncAction (..),
  getGitHubSync,
  getGitHubSyncDecrypted,
  getGitSyncByRepo,
  insertGitHubSync,
  insertGitHubAppSync,
  updateGitHubSync,
  updateGitHubSyncKeepToken,
  updateGitHubSyncRepo,
  updateLastRevision,
  deleteGitHubSync,
  getDashboardGitState,
  updateDashboardGitInfo,
  GitCreds (..),
  mkGitCreds,
  syncRepoRef,
  syncCreds,
  syncConn,
  credentialConn,
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
  buildSchemaWithMeta,
  getDashboardsPath,
  -- GitHub App integration; GitHub-only by nature
  githubToken,
  generateAppJWT,
  getInstallationToken,
  getInstallationAccount,
  installationSettingsUrl,
  InstallationToken (..),
  -- Re-exported from "Pkg.Git" so a caller needs one import, not two
  Git.GitHost (..),
  Git.GitConn (..),
  Git.RepoRef (..),
  Git.TreeEntry (..),
  Git.GitRepo (..),
  Git.computeContentSha,
  Git.hostLabel,
  Git.hostSlug,
) where

import Control.Lens ((.~), (?~), (^.), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _String)
import Data.Base64.Types (extractBase64)
import Data.ByteString qualified as BS
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
import Pkg.DeriveUtils (DB, UUIDId (..), selectFrom)
import Pkg.Git qualified as Git
import Relude
import Relude.Extra.Bifunctor (bimapF)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Logging (logWarn)
import Text.Casing (fromAny, toKebab)
import "base64" Data.ByteString.Base64 qualified as B64
import "crypton-x509" Data.X509 (PrivKey (..))
import "crypton-x509-store" Data.X509.File (readKeyFile)


type GitHubSyncId = UUIDId "github_sync"


type GitHubCredentialId = UUIDId "github_credential"


-- | The one repository a project keeps its own dashboard and monitor YAML in.
--
-- Field order matches the physical column order of @projects.git_sync@, because
-- 'HI.DecodeRow' is positional and some statements return the whole row. @host@ and @apiBase@
-- are last for the same reason: 0125 appended them.
data GitHubSync = GitHubSync
  { id :: GitHubSyncId
  , projectId :: ProjectId
  , owner :: Text
  , repo :: Text
  , branch :: Text
  , accessToken :: Maybe Text -- Encrypted token
  , installationId :: Maybe Int64 -- GitHub App installation ID; GitHub only
  , pathPrefix :: Text -- Directory prefix for dashboards (default: "")
  , webhookSecret :: Maybe Text
  , lastRevision :: Maybe Text
  -- ^ Head commit at the last pull. Not a tree sha: GitLab and Bitbucket have no such thing,
  -- and a commit id answers the only question this was ever asked — has anything changed?
  , syncEnabled :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , host :: Git.GitHost
  , apiBase :: Maybe Text
  -- ^ Already-normalised API base for a self-hosted install; 'Nothing' is the host's SaaS.
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "projects", TableName "git_sync", PrimaryKey "id", FieldModifiers '[CamelToSnake]] GitHubSync)


instance Default GitHubSync where
  def = GitHubSync (UUIDId UUID.nil) (UUIDId UUID.nil) "" "" "main" Nothing Nothing "" Nothing Nothing True epoch epoch Git.GitHub Nothing
    where
      epoch = posixSecondsToUTCTime 0


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


syncRepoRef :: GitHubSync -> Git.RepoRef
syncRepoRef s = Git.RepoRef s.owner s.repo s.branch


syncCreds :: GitHubSync -> Maybe GitCreds
syncCreds s = mkGitCreds s.installationId s.accessToken


-- | Turn a row plus a resolved token into something that can be talked to.
--
-- The token is resolved separately — for GitHub it may be minted from an App installation,
-- which is an HTTP round trip — so this stays pure and total: given the token, either the row
-- describes a reachable host or it says why not.
syncConn :: GitHubSync -> Text -> Either Text Git.GitConn
syncConn s = Git.mkGitConn s.host s.apiBase


credentialConn :: GitHubCredential -> Text -> Either Text Git.GitConn
credentialConn c = Git.mkGitConn c.host c.apiBase


-- | A grant to read an account's repositories, held per project.
--
-- Separate from 'GitHubSync' because they answer different questions: a sync row is /the/
-- repo monoscope keeps its own YAML in (one per project), while a project's services live in
-- as many repos as they like. See migration 0124.
data GitHubCredential = GitHubCredential
  { id :: GitHubCredentialId
  , projectId :: ProjectId
  , account :: Text
  -- ^ The org, group, workspace or user the grant covers. Only unique within a host.
  , installationId :: Maybe Int64
  -- ^ GitHub App installation. No other host has an equivalent, and 0125's CHECK constraint
  -- refuses one on a non-GitHub row.
  , accessToken :: Maybe Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , host :: Git.GitHost
  , apiBase :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "projects", TableName "git_credentials", PrimaryKey "id", FieldModifiers '[CamelToSnake]] GitHubCredential)


credentialCreds :: GitHubCredential -> Maybe GitCreds
credentialCreds c = mkGitCreds c.installationId c.accessToken


getGitHubCredentials :: DB es => ProjectId -> Eff es [GitHubCredential]
getGitHubCredentials pid = Hasql.interp (selectFrom @GitHubCredential <> [HI.sql| WHERE project_id = #{pid} ORDER BY account |])


-- | One credential, with its PAT decrypted.
getGitHubCredential :: (DB es, Log :> es) => ByteString -> ProjectId -> GitHubCredentialId -> Eff es (Maybe GitHubCredential)
getGitHubCredential encKey pid cid =
  Hasql.interp (selectFrom @GitHubCredential <> [HI.sql| WHERE project_id = #{pid} AND id = #{cid} |])
    >>= maybe (pure Nothing) (decryptedOr "credential" pid . decryptAccessToken encKey)


-- | Record a grant for an account on a host, or update the one already held for it. Idempotent
-- because the same installation arriving twice is the same grant, not a second one.
--
-- Keyed on @(project, host, account)@: the same login on two hosts is two grants.
upsertGitHubCredential :: DB es => ByteString -> ProjectId -> Git.GitHost -> Maybe Text -> Text -> Maybe Int64 -> Maybe Text -> Eff es (Maybe GitHubCredential)
upsertGitHubCredential encKey pid host apiBase account instId token =
  Hasql.interp
    [HI.sql| INSERT INTO projects.git_credentials (project_id, host, api_base, account, installation_id, access_token)
             VALUES (#{pid}, #{host}, #{apiBase}, #{account}, #{instId}, #{encryptToken encKey <$> token})
             ON CONFLICT (project_id, host, account)
             DO UPDATE SET api_base = EXCLUDED.api_base, installation_id = EXCLUDED.installation_id, access_token = COALESCE(EXCLUDED.access_token, projects.git_credentials.access_token), updated_at = now()
             RETURNING id, project_id, account, installation_id, access_token, created_at, updated_at, host, api_base |]


-- DB Operations
getGitHubSync :: DB es => ProjectId -> Eff es (Maybe GitHubSync)
getGitHubSync pid =
  Hasql.interp
    (selectFrom @GitHubSync <> [HI.sql| WHERE project_id = #{pid} |])


getGitHubSyncDecrypted :: (DB es, Log :> es) => ByteString -> ProjectId -> Eff es (Maybe GitHubSync)
getGitHubSyncDecrypted encKey pid =
  getGitHubSync pid >>= maybe (pure Nothing) (decryptedOr "sync" pid . decryptAccessToken encKey)


-- | The sync row a webhook delivery is about.
--
-- Keyed on the host as well as the name: two hosts can both have an @acme/config@, and
-- resolving without the host would let a push to one trigger a sync of the other.
getGitSyncByRepo :: DB es => Git.GitHost -> Text -> Text -> Eff es (Maybe GitHubSync)
getGitSyncByRepo host owner repo =
  Hasql.interp
    (selectFrom @GitHubSync <> [HI.sql| WHERE host = #{host} AND owner = #{owner} AND repo = #{repo} |])


-- | Insert a sync config authenticated by a token the user created — the only option on every
-- host except GitHub, and still available there.
insertGitHubSync :: DB es => ByteString -> ProjectId -> Git.GitHost -> Maybe Text -> Text -> Text -> Text -> Text -> Maybe Text -> Text -> Eff es (Maybe GitHubSync)
insertGitHubSync encKey pid host apiBase ownerVal repoVal branchVal token webhookSecretVal prefix = do
  let encToken = encryptToken encKey token
  Hasql.interp
    [HI.sql| INSERT INTO projects.git_sync (project_id, host, api_base, owner, repo, branch, access_token, webhook_secret, path_prefix)
             VALUES (#{pid}, #{host}, #{apiBase}, #{ownerVal}, #{repoVal}, #{branchVal}, #{encToken}, #{webhookSecretVal}, #{prefix}) RETURNING * |]


-- | Insert a new GitHub sync config using GitHub App installation
insertGitHubAppSync :: DB es => ProjectId -> Int64 -> Text -> Text -> Text -> Text -> Eff es (Maybe GitHubSync)
insertGitHubAppSync pid instId ownerVal repoVal branchVal prefix =
  Hasql.interp
    [HI.sql| INSERT INTO projects.git_sync (project_id, owner, repo, branch, installation_id, path_prefix)
           VALUES (#{pid}, #{ownerVal}, #{repoVal}, #{branchVal}, #{instId}, #{prefix}) RETURNING * |]


updateGitHubSync :: (DB es, Time :> es) => ByteString -> GitHubSyncId -> Text -> Text -> Text -> Text -> Bool -> Eff es (Maybe GitHubSync)
updateGitHubSync encKey sid ownerVal repoVal branchVal token enabled = do
  now <- Time.currentTime
  let encToken = encryptToken encKey token
  Hasql.interp
    [HI.sql| UPDATE projects.git_sync SET owner = #{ownerVal}, repo = #{repoVal}, branch = #{branchVal}, access_token = #{encToken}, sync_enabled = #{enabled}, updated_at = #{now}
             WHERE id = #{sid} RETURNING * |]


updateGitHubSyncKeepToken :: (DB es, Time :> es) => GitHubSyncId -> Text -> Text -> Text -> Bool -> Eff es (Maybe GitHubSync)
updateGitHubSyncKeepToken sid ownerVal repoVal branchVal enabled = do
  now <- Time.currentTime
  Hasql.interp
    [HI.sql| UPDATE projects.git_sync SET owner = #{ownerVal}, repo = #{repoVal}, branch = #{branchVal}, sync_enabled = #{enabled}, updated_at = #{now}
             WHERE id = #{sid} RETURNING * |]


-- | Update repo selection for GitHub App installation
updateGitHubSyncRepo :: (DB es, Time :> es) => GitHubSyncId -> Text -> Text -> Text -> Text -> Eff es (Maybe GitHubSync)
updateGitHubSyncRepo sid ownerVal repoVal branchVal prefix = do
  now <- Time.currentTime
  Hasql.interp
    [HI.sql| UPDATE projects.git_sync SET owner = #{ownerVal}, repo = #{repoVal}, branch = #{branchVal}, path_prefix = #{prefix}, updated_at = #{now}
             WHERE id = #{sid} RETURNING * |]


-- | Remember the head commit we last synced to, so the next pull can tell "nothing changed"
-- from "everything was deleted".
updateLastRevision :: (DB es, Time :> es) => GitHubSyncId -> Text -> Eff es Int64
updateLastRevision sid rev = do
  now <- Time.currentTime
  Hasql.interpExecute [HI.sql| UPDATE projects.git_sync SET last_revision = #{rev}, updated_at = #{now} WHERE id = #{sid} |]


deleteGitHubSync :: DB es => GitHubSyncId -> Eff es Int64
deleteGitHubSync sid =
  Hasql.interpExecute
    [HI.sql| DELETE FROM projects.git_sync WHERE id = #{sid} |]


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


-- | Get the dashboards folder path including prefix
getDashboardsPath :: GitHubSync -> Text
getDashboardsPath sync
  | T.null sync.pathPrefix = "dashboards/"
  | otherwise = sync.pathPrefix <> "/dashboards/"


-- | A dashboard file we can actually plan against: a blob, under the prefix, ending in YAML,
-- /and/ carrying a content hash. An entry with no sha is one Bitbucket would not tell us about
-- (see 'Git.fetchTree'); planning on it would compare @Nothing@ to @Nothing@ and conclude
-- nothing ever changes.
isDashboardFile :: Text -> Git.TreeEntry -> Bool
isDashboardFile prefix e = e.isBlob && isJust e.sha && prefix `T.isPrefixOf` e.path && any (`T.isSuffixOf` e.path) [".yaml", ".yml"]


-- Sync Logic
buildSyncPlan :: Text -> [Git.TreeEntry] -> M.Map Text (DashboardId, Text) -> [SyncAction]
buildSyncPlan prefix entries dbState = renames <> creates <> updates <> deletes
  where
    -- `isDashboardFile` has already established the sha is present, so the fold below is
    -- total; the fromMaybe is unreachable and only there to keep the pattern irrefutable.
    gitFiles = M.fromList [(fromMaybe e.path $ T.stripPrefix prefix e.path, (e.path, fromMaybe "" e.sha)) | e <- entries, isDashboardFile prefix e]
    newFiles = gitFiles `M.difference` dbState
    removedFiles = dbState `M.difference` gitFiles
    removedBySha = M.fromList [(sha, rid) | (rid, sha) <- M.elems removedFiles]
    -- A new file whose SHA matches a removed file is a rename, not a create+delete
    (renames, creates) =
      partitionEithers
        [maybe (Right $ SyncCreate p s) (Left . SyncRename p s) (M.lookup s removedBySha) | (p, s) <- M.elems newFiles]
    renamedIds = [rid | SyncRename _ _ rid <- renames]
    deletes = [SyncDelete p rid | (p, (rid, _)) <- M.toList removedFiles, rid `notElem` renamedIds]
    updates = [SyncUpdate fullPath s rid | (relPath, (fullPath, s)) <- M.toList gitFiles, Just (rid, oldSha) <- [M.lookup relPath dbState], s /= oldSha]


dashboardToYaml :: Dashboard -> ByteString
dashboardToYaml = Yaml.encode


yamlToDashboard :: ByteString -> Either Text Dashboard
yamlToDashboard = first (toText . show) . Yaml.decodeEither'


-- | Convert dashboard title to kebab-case file path
titleToFilePath :: Text -> Text
titleToFilePath = (<> ".yaml") . toText . toKebab . fromAny . toString . T.strip


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

data InstallationToken = InstallationToken
  { token :: Text
  , expiresAt :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] InstallationToken


-- | The token to talk to a host with: an App installation token when GitHub's App is
-- installed, else the stored token as-is.
--
-- Still lives here rather than in "Pkg.Git" because minting an installation token is the
-- GitHub App flow, which no other host has. Every host's 'PersonalToken' path is the identity
-- function, which is exactly why the abstraction downstream takes a resolved token.
githubToken :: (IOE :> es, W.HTTP :> es) => Text -> Text -> GitCreds -> Eff es (Either Text Text)
githubToken appId privateKeyB64 = \case
  AppInstallation instId -> bimapF ("Failed to get installation token: " <>) (.token) $ getInstallationToken appId privateKeyB64 instId
  PersonalToken token -> pure $ Right token


githubAppOpts :: Text -> W.Options
githubAppOpts jwt =
  W.defaults
    & W.header "Authorization"
    .~ [encodeUtf8 $ "Bearer " <> jwt]
      & W.header "Accept"
    .~ ["application/vnd.github+json"]
      & W.header "User-Agent"
    .~ ["Monoscope-App"]
      & W.header "X-GitHub-Api-Version"
    .~ ["2022-11-28"]


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
    let url = "https://api.github.com/app/installations/" <> show @Text installationId <> "/access_tokens"
    Git.tryHttp (W.postWith (githubAppOpts jwt) (toString url) ("" :: ByteString))
      <&> (>>= first (\err -> "Failed to parse token response: " <> toText err) . AE.eitherDecode . (^. W.responseBody))


-- | The org or user login an installation covers. Recorded as a credential the moment the
-- App is installed, so reading source works without also having configured dashboard sync —
-- they are two uses of one grant, not two integrations.
getInstallationAccount :: (IOE :> es, W.HTTP :> es) => Text -> Text -> Int64 -> Eff es (Either Text Text)
getInstallationAccount appId privateKeyB64 instId =
  liftIO (generateAppJWT appId privateKeyB64) >>= either (pure . Left) \jwt ->
    Git.tryHttp (W.getWith (githubAppOpts jwt) (toString $ "https://api.github.com/app/installations/" <> show @Text instId))
      <&> (>>= maybeToRight "Installation reports no account" . (^? W.responseBody . key "account" . key "login" . _String))


-- | GitHub's own "Repository access" screen for an installation.
--
-- An installation reaches exactly the repositories it was installed on, and nothing on our
-- side widens that — the grant belongs to GitHub, so a picker that is missing a repository
-- has to link out to the one page that can add it. The @\/settings\/@ form is canonical for
-- both personal and organisation installations; GitHub redirects org-owned ones to the org's
-- equivalent page for anyone who may edit it.
--
-- >>> installationSettingsUrl 4242
-- "https://github.com/settings/installations/4242"
installationSettingsUrl :: Int64 -> Text
installationSettingsUrl instId = "https://github.com/settings/installations/" <> show instId
