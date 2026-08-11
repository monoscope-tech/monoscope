-- | Source context for a stack frame: given @file@ and @line@, the lines around it.
--
-- This is the Datadog "source code integration" / Sentry "stack trace linking" model. The
-- source is fetched from the linked repository at read time rather than uploaded and stored:
-- an uploaded snapshot is source from whenever it was uploaded, while the repo can be read
-- at the revision that actually threw, and we already hold a GitHub App installation for
-- dashboard sync so there is nothing new to authorise.
module Models.Projects.CodeContext (
  CodeMapping (..),
  CodeMappingId,
  Snippet (..),
  getCodeMappings,
  insertCodeMapping,
  deleteCodeMapping,
  resolveRepoPath,
  sliceAround,
  fetchSnippet,
) where

import Data.Default (Default (..))
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Wreq qualified as W
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Effectful (Eff, (:>))
import Effectful.Log (Log)
import Hasql.Interpolate qualified as HI
import Models.Projects.GitSync (GitHubCredentialId, credentialCreds, fetchFileContent, getGitHubCredential, githubToken)
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.Projects (ProjectId)
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import System.Config qualified as Config
import System.Types (DB)


type CodeMappingId = UUIDId "CodeMapping"


-- | How a stack-frame path becomes a path inside a repository, and which repository that is.
--
-- Several per project rather than one root, because a project monitors several services and
-- they do not share a repo. The repo is named here rather than referenced through the
-- config-sync row: an App installation already reaches every repo in its account, so adding
-- the tenth service repo costs a mapping, not an integration. See migration 0124.
data CodeMapping = CodeMapping
  { id :: CodeMappingId
  , projectId :: ProjectId
  , credentialId :: GitHubCredentialId
  -- ^ The grant to read this repo with.
  , owner :: Text
  , repo :: Text
  , ref :: Text
  -- ^ Branch or sha to read when the telemetry does not report its own revision.
  , service :: Maybe Text
  -- ^ 'Nothing' matches any service, which is the common case.
  , pathPrefix :: Text
  -- ^ Leading segment of the frame path to strip. @""@ matches every frame.
  , sourceRoot :: Text
  -- ^ Directory inside the repo the stripped path is relative to. @""@ is the repo root.
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "projects", TableName "code_mappings", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CodeMapping)


-- | The catch-all mapping: every frame, repo root, any service. Also what a test or doctest
-- overrides one field of, rather than spelling out eleven.
instance Default CodeMapping where
  def = CodeMapping (UUIDId UUID.nil) (UUIDId UUID.nil) (UUIDId UUID.nil) "" "" "main" Nothing "" "" epoch epoch
    where
      epoch = UTCTime (fromGregorian 1970 1 1) 0


-- | A window of source around the failing line. @startLine@ is the 1-based line number of
-- the first entry in @body@, so the renderer can number the gutter without recomputing it.
data Snippet = Snippet
  { path :: Text
  , startLine :: Int
  , focusLine :: Int
  , body :: [Text]
  }
  deriving stock (Eq, Generic, Show)


getCodeMappings :: DB es => ProjectId -> Eff es [CodeMapping]
getCodeMappings pid = Hasql.interp [HI.sql|SELECT id, project_id, credential_id, owner, repo, ref, service, path_prefix, source_root, created_at, updated_at FROM projects.code_mappings WHERE project_id = #{pid.unUUIDId} ORDER BY length(path_prefix) DESC|]


insertCodeMapping :: DB es => ProjectId -> GitHubCredentialId -> GitSync.RepoRef -> Maybe Text -> Text -> Text -> Eff es ()
insertCodeMapping pid credId r svc prefix root =
  Hasql.interpExecute_
    [HI.sql|INSERT INTO projects.code_mappings (project_id, credential_id, owner, repo, ref, service, path_prefix, source_root)
            VALUES (#{pid.unUUIDId}, #{credId.unUUIDId}, #{r.owner}, #{r.repo}, #{r.ref}, #{svc}, #{prefix}, #{root})
            ON CONFLICT (project_id, service, path_prefix)
            DO UPDATE SET credential_id = EXCLUDED.credential_id, owner = EXCLUDED.owner, repo = EXCLUDED.repo, ref = EXCLUDED.ref, source_root = EXCLUDED.source_root, updated_at = now()|]


deleteCodeMapping :: DB es => ProjectId -> CodeMappingId -> Eff es ()
deleteCodeMapping pid mid = Hasql.interpExecute_ [HI.sql|DELETE FROM projects.code_mappings WHERE project_id = #{pid.unUUIDId} AND id = #{mid.unUUIDId}|]


-- | Rewrite a stack-frame path into a repo path using the best-matching mapping.
--
-- Longest prefix wins, so a specific mapping beats a catch-all no matter what order the rows
-- arrive in. A mapping bound to a service only answers for that service; an unbound mapping
-- answers for any.
--
-- >>> let m svc prefix root = def{service = svc, pathPrefix = prefix, sourceRoot = root} :: CodeMapping
-- >>> let ms = [m Nothing "/srv/app/" "", m (Just "billing") "/srv/app/vendor/" "third_party/"]
--
-- >>> snd <$> resolveRepoPath ms (Just "checkout") "/srv/app/services/checkout.py"
-- Just "services/checkout.py"
--
-- The longer prefix wins, but only for the service it is bound to:
--
-- >>> snd <$> resolveRepoPath ms (Just "billing") "/srv/app/vendor/stripe.rb"
-- Just "third_party/stripe.rb"
-- >>> snd <$> resolveRepoPath ms (Just "checkout") "/srv/app/vendor/stripe.rb"
-- Just "vendor/stripe.rb"
--
-- A frame no mapping claims resolves to nothing, rather than being guessed at from the repo
-- root — a wrong file rendered as the failing code is worse than no file:
--
-- >>> snd <$> resolveRepoPath ms Nothing "/usr/lib/python3.11/json/decoder.py"
-- Nothing
-- >>> snd <$> resolveRepoPath [] (Just "checkout") "anything.py"
-- Nothing
--
-- An empty prefix is a real catch-all, and @sourceRoot@ joins without doubling separators:
--
-- >>> snd <$> resolveRepoPath [m Nothing "" "src"] Nothing "app/main.go"
-- Just "src/app/main.go"
resolveRepoPath :: [CodeMapping] -> Maybe Text -> Text -> Maybe (CodeMapping, Text)
resolveRepoPath mappings svc path = do
  (cm, rest) <- viaNonEmpty head $ sortOn (negate . T.length . (.pathPrefix) . fst) candidates
  pure (cm, joinPath cm.sourceRoot rest)
  where
    candidates =
      [ (cm, rest)
      | cm <- mappings
      , maybe True (\s -> Just s == svc) cm.service
      , Just rest <- [T.stripPrefix cm.pathPrefix path]
      ]
    joinPath root rest
      | T.null (T.dropWhileEnd (== '/') root) = rest
      | otherwise = T.dropWhileEnd (== '/') root <> "/" <> rest


-- | The window of source to show around a line: the failing line plus @ctx@ either side,
-- clamped to the file.
--
-- >>> sliceAround 2 3 ["a", "b", "c", "d", "e", "f", "g"]
-- Just (Snippet {path = "", startLine = 1, focusLine = 3, body = ["a","b","c","d","e"]})
--
-- Clamps at both ends rather than shifting the window, so the focus line stays where the
-- gutter says it is:
--
-- >>> sliceAround 2 1 ["a", "b", "c", "d"]
-- Just (Snippet {path = "", startLine = 1, focusLine = 1, body = ["a","b","c"]})
-- >>> sliceAround 2 4 ["a", "b", "c", "d"]
-- Just (Snippet {path = "", startLine = 2, focusLine = 4, body = ["b","c","d"]})
--
-- A line number outside the file yields 'Nothing' — a build whose line numbers do not match
-- the checked-out revision must say so, not render an arbitrary window as the failure. It is
-- absence rather than an empty 'Snippet' so no caller has to know that @null body@ is how
-- this function reports failure:
--
-- >>> sliceAround 2 99 ["a", "b"]
-- Nothing
-- >>> sliceAround 2 0 ["a", "b"]
-- Nothing
sliceAround :: Int -> Int -> [Text] -> Maybe Snippet
sliceAround ctx focus ls
  | focus < 1 || focus > length ls = Nothing
  | otherwise = Just $ Snippet "" start focus (take (stop - start + 1) (drop (start - 1) ls))
  where
    start = max 1 (focus - ctx)
    stop = min (length ls) (focus + ctx)


-- | Fetch the source around @(file, line)@ for a project, or say why not.
--
-- Returns 'Left' with a reason rather than 'Nothing': "no mapping covers this path" and "the
-- repo does not have that file at this branch" are different problems with different fixes,
-- and a panel that renders both as silence sends the reader to configure something that is
-- already configured.
fetchSnippet
  :: (DB es, Log :> es, W.HTTP :> es)
  => Config.EnvConfig
  -> ProjectId
  -> Maybe Text
  -> Maybe Text
  -- ^ The revision the telemetry reported, if it reported one. Wins over the mapping's ref:
  -- the snippet must be the source that threw, not the source as it is now.
  -> Text
  -> Int
  -> Eff es (Either Text Snippet)
fetchSnippet cfg pid svc revM path lineNo = runExceptT do
  mappings <- lift $ getCodeMappings pid
  (cm, repoPath) <- hoistEither $ maybeToRight "No code mapping covers this path." $ resolveRepoPath mappings svc path
  let repoRef = GitSync.RepoRef cm.owner cm.repo (fromMaybe cm.ref revM)
  cred <-
    ExceptT
      $ maybeToRight "The GitHub credential this mapping uses is no longer configured."
      <$> getGitHubCredential (encodeUtf8 cfg.apiKeyEncryptionSecretKey) pid cm.credentialId
  creds <- hoistEither $ maybeToRight "That GitHub credential has neither an installation nor a token." $ credentialCreds cred
  token <- ExceptT $ githubToken cfg.githubAppId cfg.githubAppPrivateKey creds
  blob <-
    ExceptT
      $ first (\err -> "Could not read " <> cm.owner <> "/" <> cm.repo <> "/" <> repoPath <> " at " <> repoRef.ref <> ": " <> err)
      <$> fetchFileContent token repoRef repoPath
  snippet <-
    hoistEither
      $ maybeToRight ("Line " <> show lineNo <> " is past the end of " <> repoPath <> " at " <> repoRef.ref <> " — the build that threw is probably not this revision.")
      $ sliceAround 5 lineNo (lines (decodeUtf8 blob))
  pure snippet{path = repoPath}
