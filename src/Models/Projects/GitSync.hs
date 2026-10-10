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
  Repository (..),
  RepositoryId,
  getRepositories,
  getRepository,
  connectRepository,
  removeRepository,
  enableRepositoryDashboardSync,
  SyncAction (..),
  getGitHubSync,
  getGitSyncs,
  getGitSyncsDecrypted,
  getGitSyncById,
  getGitSyncsByRepo,
  insertGitHubSync,
  updateGitHubSync,
  pauseGitSync,
  updateLastRevision,
  setAnnouncedRevision,
  clearAnnouncedRevision,
  recordSyncError,
  recordPushSuccess,
  deleteGitHubSync,
  getRepositoryDashboardState,
  assignDashboardRepository,
  DashboardRepositoryError (..),
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
  saveTokenCredential,
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
import Data.Char (isAlphaNum)
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
import Hasql.Transaction.Sessions qualified as TxS
import Jose.Jwa (JwsAlg (RS256))
import Jose.Jws qualified as Jws
import Jose.Jwt (Jwt (..))
import Models.Projects.Dashboards (Dashboard, DashboardId)
import Models.Projects.Dashboards qualified as Dashboards
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


type RepositoryId = UUIDId "repository"


-- | A project connection retained independently of its enabled capabilities.
data Repository = Repository
  { id :: RepositoryId
  , projectId :: ProjectId
  , host :: Git.GitHost
  , apiBase :: Maybe Text
  , owner :: Text
  , repo :: Text
  , createdAt :: UTCTime
  , credentialId :: Maybe GitHubCredentialId
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "projects", TableName "repositories", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Repository)


getRepositories :: DB es => ProjectId -> Eff es [Repository]
getRepositories pid = Hasql.interp (selectFrom @Repository <> [HI.sql| WHERE project_id = #{pid} ORDER BY owner, repo, host, api_base |])


getRepository :: DB es => ProjectId -> RepositoryId -> Eff es (Maybe Repository)
getRepository pid rid = Hasql.interp (selectFrom @Repository <> [HI.sql| WHERE project_id = #{pid} AND id = #{rid} |])


connectRepository :: DB es => ProjectId -> GitHubCredentialId -> Git.GitRepo -> Eff es (Maybe Repository)
connectRepository pid cid repository =
  let (owner, repo) = Git.splitFullName repository.fullName
   in Hasql.interp
        [HI.sql| INSERT INTO projects.repositories (project_id, host, api_base, owner, repo, credential_id)
                 SELECT project_id, host, api_base, #{owner}, #{repo}, id FROM projects.git_credentials
                 WHERE project_id = #{pid} AND id = #{cid}
                 ON CONFLICT (project_id, host, api_base, owner, repo) DO UPDATE SET credential_id = EXCLUDED.credential_id
                 RETURNING * |]


removeRepository :: DB es => ProjectId -> RepositoryId -> Eff es Bool
removeRepository pid rid = Hasql.transaction TxS.ReadCommitted TxS.Write do
  repositories <- Hasql.queryTx @[Repository] (selectFrom @Repository <> [HI.sql| WHERE project_id = #{pid} AND id = #{rid} FOR UPDATE |])
  case repositories of
    [] -> pure False
    repository : _ -> do
      syncs <- Hasql.queryTx @[GitHubSync] (selectFrom @GitHubSync <> [HI.sql| WHERE project_id = #{pid} AND host = #{repository.host} AND api_base IS NOT DISTINCT FROM #{repository.apiBase} AND owner = #{repository.owner} AND repo = #{repository.repo} FOR UPDATE |])
      for_ syncs \sync -> do
        Hasql.executeTx [HI.sql| UPDATE projects.dashboards SET git_sync_id = NULL, file_path = NULL, file_sha = NULL WHERE project_id = #{pid} AND git_sync_id = #{sync.id} |]
        Hasql.executeTx [HI.sql| DELETE FROM projects.git_sync WHERE project_id = #{pid} AND id = #{sync.id} |]
      when (repository.host == Git.GitHub && isNothing repository.apiBase) $
        Hasql.executeTx [HI.sql| INSERT INTO projects.pr_review_settings (project_id, owner, repo, enabled, include_evidence)
          VALUES (#{pid}, lower(#{repository.owner}), lower(#{repository.repo}), false, false)
          ON CONFLICT (project_id, owner, repo) DO UPDATE SET enabled = false |]
      Hasql.executeTx [HI.sql| DELETE FROM projects.code_mappings m USING projects.git_credentials c
        WHERE m.project_id = #{pid} AND m.credential_id = c.id AND c.project_id = m.project_id
          AND m.owner = #{repository.owner} AND m.repo = #{repository.repo}
          AND c.host = #{repository.host} AND c.api_base IS NOT DISTINCT FROM #{repository.apiBase} |]
      True <$ Hasql.executeTx [HI.sql| DELETE FROM projects.repositories WHERE project_id = #{pid} AND id = #{rid} |]


enableRepositoryDashboardSync :: DB es => ProjectId -> RepositoryId -> GitHubCredentialId -> Text -> Text -> Maybe Text -> Eff es (Maybe GitHubSync)
enableRepositoryDashboardSync pid rid cid branch prefix secret =
  Hasql.interp
    [HI.sql| WITH connected AS (
               UPDATE projects.repositories r SET credential_id = c.id
               FROM projects.git_credentials c
               WHERE r.project_id = #{pid} AND r.id = #{rid} AND c.project_id = r.project_id AND c.id = #{cid}
                 AND c.host = r.host AND c.api_base IS NOT DISTINCT FROM r.api_base
               RETURNING r.*
             )
             INSERT INTO projects.git_sync (project_id, host, api_base, owner, repo, branch, installation_id, access_token, webhook_secret, path_prefix)
             SELECT r.project_id, r.host, r.api_base, r.owner, r.repo, #{branch}, c.installation_id, c.access_token, #{secret}, #{prefix}
             FROM connected r JOIN projects.git_credentials c ON c.id = r.credential_id
             ON CONFLICT (project_id, host, api_base, owner, repo) DO NOTHING
             RETURNING * |]


-- | Dashboard file configuration for one repository.
--
-- Field order matches @projects.git_sync@: positional decoding includes columns
-- appended by migrations.
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
  , announcedRevision :: Maybe Text
  -- ^ The head a push webhook announced, held until a fetch actually returns it. Without it
  -- a fetch that answers with a stale head is indistinguishable from one that answers "no
  -- change", and the push is dropped rather than retried. See 0139.
  , announcedAt :: Maybe UTCTime
  -- ^ When that announcement arrived, so the retry it licenses cannot run forever against a
  -- sha a force-push has since made unfetchable.
  , lastError :: Maybe Text
  , lastSyncedAt :: Maybe UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "projects", TableName "git_sync", PrimaryKey "id", FieldModifiers '[CamelToSnake]] GitHubSync)


instance Default GitHubSync where
  def = GitHubSync (UUIDId UUID.nil) (UUIDId UUID.nil) "" "" "main" Nothing Nothing "" Nothing Nothing True epoch epoch Git.GitHub Nothing Nothing Nothing Nothing Nothing
    where
      epoch = posixSecondsToUTCTime 0


data SyncAction
  = SyncCreate {path :: Text, sha :: Text}
  | SyncUpdate {path :: Text, sha :: Text, resourceId :: DashboardId}
  | SyncDelete {path :: Text, resourceId :: DashboardId}
  | SyncRename {path :: Text, sha :: Text, resourceId :: DashboardId} -- File moved/renamed, same content
  | SyncConflict {path :: Text, resourceId :: DashboardId}
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
-- A sync row configures dashboard files in one repository; an account grant can supply
-- source access for several service repositories.
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


-- | A project's grants, most recently granted first.
getGitHubCredentials :: DB es => ProjectId -> Eff es [GitHubCredential]
getGitHubCredentials pid = Hasql.interp (selectFrom @GitHubCredential <> [HI.sql| WHERE project_id = #{pid} ORDER BY created_at DESC |])


-- | One credential, with its PAT decrypted.
getGitHubCredential :: (DB es, Log :> es) => ByteString -> ProjectId -> GitHubCredentialId -> Eff es (Maybe GitHubCredential)
getGitHubCredential encKey pid cid =
  Hasql.interp (selectFrom @GitHubCredential <> [HI.sql| WHERE project_id = #{pid} AND id = #{cid} |])
    >>= maybe (pure Nothing) (decryptedOr "credential" pid . decryptAccessToken encKey)


-- | Record a grant for an account on a host, or update the one already held for it. Idempotent
-- because the same installation arriving twice is the same grant, not a second one.
--
-- Keyed on project, host, server origin and account.
upsertGitHubCredential :: DB es => ByteString -> ProjectId -> Git.GitHost -> Maybe Text -> Text -> Maybe Int64 -> Maybe Text -> Eff es (Maybe GitHubCredential)
upsertGitHubCredential encKey pid host apiBase account instId token =
  Hasql.interp
    [HI.sql| INSERT INTO projects.git_credentials (project_id, host, api_base, account, installation_id, access_token)
             VALUES (#{pid}, #{host}, #{apiBase}, #{account}, #{instId}, #{encryptToken encKey <$> token})
             ON CONFLICT (project_id, host, api_base, account)
             DO UPDATE SET installation_id = EXCLUDED.installation_id, access_token = COALESCE(EXCLUDED.access_token, projects.git_credentials.access_token), updated_at = now()
             RETURNING id, project_id, account, installation_id, access_token, created_at, updated_at, host, api_base |]


-- | Create a token grant or replace the exact grant the editor inspected.
saveTokenCredential :: DB es => ByteString -> ProjectId -> Git.GitHost -> Maybe Text -> Text -> Text -> Maybe GitHubCredential -> Eff es (Maybe GitHubCredential)
saveTokenCredential encKey pid host origin account token = \case
  Nothing ->
    Hasql.interp
      [HI.sql| INSERT INTO projects.git_credentials (project_id, host, api_base, account, access_token)
             VALUES (#{pid}, #{host}, #{origin}, #{account}, #{encryptToken encKey token})
             ON CONFLICT (project_id, host, api_base, account) DO NOTHING
             RETURNING id, project_id, account, installation_id, access_token, created_at, updated_at, host, api_base |]
  Just observed ->
    Hasql.interp
      [HI.sql| UPDATE projects.git_credentials SET access_token = #{encryptToken encKey token}, updated_at = now()
             WHERE project_id = #{pid} AND id = #{observed.id} AND host = #{host} AND api_base IS NOT DISTINCT FROM #{origin} AND account = #{account}
               AND installation_id IS NULL AND updated_at = #{observed.updatedAt} AND access_token IS NOT DISTINCT FROM #{observed.accessToken}
             RETURNING id, project_id, account, installation_id, access_token, created_at, updated_at, host, api_base |]


-- DB Operations
getGitHubSync :: DB es => ProjectId -> Eff es (Maybe GitHubSync)
getGitHubSync pid =
  getGitSyncs pid <&> \case [sync] -> Just sync; _ -> Nothing


getGitSyncs :: DB es => ProjectId -> Eff es [GitHubSync]
getGitSyncs pid = Hasql.interp (selectFrom @GitHubSync <> [HI.sql| WHERE project_id = #{pid} ORDER BY created_at, id |])


getGitSyncById :: DB es => ProjectId -> GitHubSyncId -> Eff es (Maybe GitHubSync)
getGitSyncById pid sid = Hasql.interp (selectFrom @GitHubSync <> [HI.sql| WHERE project_id = #{pid} AND id = #{sid} |])


getGitSyncsDecrypted :: (DB es, Log :> es) => ByteString -> ProjectId -> Eff es [GitHubSync]
getGitSyncsDecrypted encKey pid =
  getGitSyncs pid
    >>= fmap catMaybes . traverse \sync -> case decryptAccessToken encKey sync of
      Left err -> recordSyncError sync.id ("Reconnect this repository: " <> err) >> decryptedOr "sync" pid (Left err)
      Right plain -> pure $ Just plain


-- | The sync row a webhook delivery is about.
--
-- Keyed on the host as well as the name: two hosts can both have an @acme/config@, and
-- resolving without the host would let a push to one trigger a sync of the other.
getGitSyncsByRepo :: DB es => Git.GitHost -> Text -> Text -> Eff es [GitHubSync]
getGitSyncsByRepo host owner repo = Hasql.interp (selectFrom @GitHubSync <> [HI.sql| WHERE host = #{host} AND lower(owner) = lower(#{owner}) AND lower(repo) = lower(#{repo}) |])


-- | Split the two auth states back into the two nullable columns that denote them. The sum
-- type is what makes the row's @installation_id IS NOT NULL OR access_token IS NOT NULL@
-- check hold by construction rather than by every caller remembering to fill one in.
credColumns :: ByteString -> GitCreds -> (Maybe Int64, Maybe Text)
credColumns encKey = \case
  AppInstallation i -> (Just i, Nothing)
  PersonalToken t -> (Nothing, Just (encryptToken encKey t))


-- | Insert a sync config, authenticated either by a GitHub App installation or by a token the
-- user created — the only option on every host except GitHub, and still available there.
insertGitHubSync :: DB es => ByteString -> ProjectId -> Git.GitHost -> Maybe Text -> Text -> Text -> Text -> GitCreds -> Maybe Text -> Text -> Eff es (Maybe GitHubSync)
insertGitHubSync encKey pid host apiBase ownerVal repoVal branchVal creds webhookSecretVal prefix = do
  let (instM, tokenM) = credColumns encKey creds
  Hasql.interp
    [HI.sql| WITH registered AS (
               INSERT INTO projects.repositories (project_id, host, api_base, owner, repo)
               VALUES (#{pid}, #{host}, #{apiBase}, #{ownerVal}, #{repoVal})
               ON CONFLICT (project_id, host, api_base, owner, repo) DO NOTHING
             )
             INSERT INTO projects.git_sync (project_id, host, api_base, owner, repo, branch, installation_id, access_token, webhook_secret, path_prefix)
             VALUES (#{pid}, #{host}, #{apiBase}, #{ownerVal}, #{repoVal}, #{branchVal}, #{instM}, #{tokenM}, #{webhookSecretVal}, #{prefix}) RETURNING * |]


-- | Re-point a sync row at a repository. 'Nothing' keeps what is stored: a form that left the
-- token box empty must not blank the credential, and the App path has no folder field to send.
-- Saving always re-enables sync, matching the column default a fresh connection gets.
updateGitHubSync :: (DB es, Time :> es) => ByteString -> GitHubSyncId -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Eff es (Maybe GitHubSync)
updateGitHubSync encKey sid ownerVal repoVal branchVal tokenM prefixM = do
  now <- Time.currentTime
  Hasql.interp
    [HI.sql| WITH registered AS (
               INSERT INTO projects.repositories (project_id, host, api_base, owner, repo)
               SELECT project_id, host, api_base, #{ownerVal}, #{repoVal} FROM projects.git_sync WHERE id = #{sid}
               ON CONFLICT (project_id, host, api_base, owner, repo) DO NOTHING
             )
             UPDATE projects.git_sync
             SET owner = #{ownerVal}, repo = #{repoVal}, branch = #{branchVal}, sync_enabled = true, updated_at = #{now}
               , access_token = COALESCE(#{encryptToken encKey <$> tokenM}, access_token)
               , path_prefix = COALESCE(#{prefixM}, path_prefix)
             WHERE id = #{sid} RETURNING * |]


-- | Record a pull, and retire the announcement it satisfied.
--
-- Clearing the announcement only when it matches what was actually fetched is what stops a
-- stale read from looking like a completed sync: if the host answered with an older head, the
-- announcement survives and the next attempt still knows something is outstanding.
updateLastRevision :: (DB es, Time :> es) => GitHubSyncId -> Text -> Eff es Int64
updateLastRevision sid rev = do
  now <- Time.currentTime
  Hasql.interpExecute
    [HI.sql| UPDATE projects.git_sync
             SET last_revision = #{rev}
               , last_error = NULL, last_synced_at = #{now}
               , announced_revision = CASE WHEN announced_revision = #{rev} THEN NULL ELSE announced_revision END
               , announced_at = CASE WHEN announced_revision = #{rev} THEN NULL ELSE announced_at END
               , updated_at = #{now}
             WHERE id = #{sid} |]


-- | Note the head a push delivery announced, before the job that will chase it is queued.
--
-- Written by the webhook rather than carried in the job payload so that existing queued jobs
-- keep decoding, and so a delivery that arrives while a job is already running still leaves a
-- record that something newer exists.
setAnnouncedRevision :: (DB es, Time :> es) => GitHubSyncId -> Text -> Eff es Int64
setAnnouncedRevision sid rev = do
  now <- Time.currentTime
  Hasql.interpExecute [HI.sql| UPDATE projects.git_sync SET announced_revision = #{rev}, announced_at = #{now} WHERE id = #{sid} |]


-- | Give up chasing an announcement, after 'announcedAt' has aged out.
clearAnnouncedRevision :: DB es => GitHubSyncId -> Eff es Int64
clearAnnouncedRevision sid =
  Hasql.interpExecute [HI.sql| UPDATE projects.git_sync SET announced_revision = NULL, announced_at = NULL WHERE id = #{sid} |]


pauseGitSync :: (DB es, Time :> es) => ProjectId -> GitHubSyncId -> Eff es (Maybe GitHubSync)
pauseGitSync pid sid = do
  now <- Time.currentTime
  Hasql.interp [HI.sql| UPDATE projects.git_sync SET sync_enabled = false, updated_at = #{now} WHERE project_id = #{pid} AND id = #{sid} RETURNING * |]


deleteGitHubSync :: DB es => GitHubSyncId -> Eff es Int64
deleteGitHubSync sid =
  Hasql.transaction TxS.ReadCommitted TxS.Write do
    Hasql.executeTx [HI.sql| UPDATE projects.dashboards SET git_sync_id = NULL, file_path = NULL, file_sha = NULL WHERE git_sync_id = #{sid} |]
    HI.getRowsAffected <$> Hasql.queryTx [HI.sql| DELETE FROM projects.git_sync WHERE id = #{sid} |]


recordSyncError :: DB es => GitHubSyncId -> Text -> Eff es Int64
recordSyncError sid message = Hasql.interpExecute [HI.sql| UPDATE projects.git_sync SET last_error = #{message} WHERE id = #{sid} |]


-- | A push does not verify other remote files; only a complete pull clears failures
-- and advances the repository cursor.
recordPushSuccess :: (DB es, Time :> es) => GitHubSyncId -> Eff es Int64
recordPushSuccess sid = do
  now <- Time.currentTime
  Hasql.interpExecute [HI.sql| UPDATE projects.git_sync SET last_synced_at = #{now}, updated_at = #{now} WHERE id = #{sid} |]


getRepositoryDashboardState :: DB es => ProjectId -> GitHubSyncId -> Eff es (M.Map Text (DashboardId, Maybe Text))
getRepositoryDashboardState pid sid =
  M.fromList . map (\(did, path, sha) -> (path, (did, sha)))
    <$> Hasql.interp [HI.sql| SELECT id, file_path, file_sha FROM projects.dashboards WHERE project_id = #{pid} AND git_sync_id = #{sid} AND file_path IS NOT NULL |]


data DashboardRepositoryError = DashboardMissing | RepositoryMissing | OwnedByRepository GitHubSyncId | FileOwnedByDashboard DashboardId
  deriving stock (Eq, Show)


assignDashboardRepository :: DB es => ProjectId -> DashboardId -> GitHubSyncId -> Eff es (Either DashboardRepositoryError ())
assignDashboardRepository pid did sid = Hasql.transaction TxS.ReadCommitted TxS.Write do
  repositories <- Hasql.queryTx @[GitHubSync] (selectFrom @GitHubSync <> [HI.sql| WHERE project_id = #{pid} AND id = #{sid} FOR UPDATE |])
  dashboards <- Hasql.queryTx @[Dashboards.DashboardVM] (selectFrom @Dashboards.DashboardVM <> [HI.sql| WHERE project_id = #{pid} AND id = #{did} FOR UPDATE |])
  case (listToMaybe repositories, listToMaybe dashboards) of
    (Nothing, _) -> pure $ Left RepositoryMissing
    (_, Nothing) -> pure $ Left DashboardMissing
    (Just sync, Just dash) -> case dash.gitSyncId of
      Just other | other /= sid -> pure $ Left $ OwnedByRepository other
      _ -> do
        let rawPath = fromMaybe (titleToFilePath dash.title) dash.filePath
            path = fromMaybe rawPath $ T.stripPrefix (getDashboardsPath sync) rawPath <|> T.stripPrefix "dashboards/" rawPath
        conflicts <- Hasql.queryTx @[HI.OneColumn DashboardId] [HI.sql| SELECT id FROM projects.dashboards WHERE git_sync_id = #{sid} AND file_path = #{path} AND id <> #{did} LIMIT 1 |]
        case conflicts of
          HI.OneColumn other : _ -> pure $ Left $ FileOwnedByDashboard other
          [] -> Right () <$ Hasql.executeTx [HI.sql| UPDATE projects.dashboards SET git_sync_id = #{sid}, file_path = #{path}, file_sha = CASE WHEN git_sync_id IS NULL THEN NULL ELSE file_sha END WHERE project_id = #{pid} AND id = #{did} |]


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
buildSyncPlan :: Text -> [Git.TreeEntry] -> M.Map Text (DashboardId, Maybe Text) -> [SyncAction]
buildSyncPlan prefix entries dbState = renames <> creates <> updates <> deletes <> conflicts
  where
    gitFiles = M.fromList [(fromMaybe e.path $ T.stripPrefix prefix e.path, (e.path, sha)) | e <- entries, isDashboardFile prefix e, Just sha <- [e.sha]]
    newFiles = gitFiles `M.difference` dbState
    removedFiles = dbState `M.difference` gitFiles
    removedBySha = M.fromList [(sha, rid) | (rid, Just sha) <- M.elems removedFiles]
    -- A new file whose SHA matches a removed file is a rename, not a create+delete
    (renames, creates) =
      partitionEithers
        [maybe (Right $ SyncCreate p s) (Left . SyncRename p s) (M.lookup s removedBySha) | (p, s) <- M.elems newFiles]
    renamedIds = [rid | SyncRename _ _ rid <- renames]
    deletes = [SyncDelete p rid | (p, (rid, Just _)) <- M.toList removedFiles, rid `notElem` renamedIds]
    updates = [SyncUpdate fullPath s rid | (relPath, (fullPath, s)) <- M.toList gitFiles, Just (rid, Just oldSha) <- [M.lookup relPath dbState], s /= oldSha]

    conflicts = [SyncConflict fullPath rid | (relPath, (fullPath, _)) <- M.toList gitFiles, Just (rid, Nothing) <- [M.lookup relPath dbState]]


dashboardToYaml :: Dashboard -> ByteString
dashboardToYaml = Yaml.encode


yamlToDashboard :: ByteString -> Either Text Dashboard
yamlToDashboard bytes = first (toText . show) (Yaml.decodeEither' bytes) >>= Dashboards.validateDashboard


-- | Convert dashboard title to a kebab-case file path.
--
-- Falls back to @untitled@ unless the slug has something nameable in it, which is not a
-- hypothetical: a dashboard saved with an empty title used to round-trip to a file literally
-- named @.yaml@ — a dotfile, invisible in a normal listing and meaningless in a diff.
--
-- >>> titleToFilePath "My Dashboard"
-- "my-dashboard.yaml"
--
-- >>> titleToFilePath "  Performance Stats  "
-- "performance-stats.yaml"
--
-- The invariant: whatever the title, the result names something.
--
-- >>> titleToFilePath ""
-- "untitled.yaml"
--
-- >>> titleToFilePath "   "
-- "untitled.yaml"
--
-- The guard is "contains an alphanumeric", not "is non-empty", because 'toKebab' keeps
-- punctuation rather than dropping it — a title of @!!!@ survives as @!!!@ and would have
-- produced @!!!.yaml@, which is a legal filename and a hostile one.
--
-- >>> titleToFilePath "!!!"
-- "untitled.yaml"
--
-- >>> titleToFilePath "Q3 2026"
-- "q3-2026.yaml"
titleToFilePath :: Text -> Text
titleToFilePath title = (if T.any isAlphaNum slug then slug else "untitled") <> ".yaml"
  where
    slug = toText $ toKebab $ fromAny $ toString $ T.strip title


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
