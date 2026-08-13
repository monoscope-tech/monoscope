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
  SnippetError (..),
  snippetErrorMessage,
  getCodeMappings,
  insertCodeMapping,
  deleteCodeMapping,
  resolveRepoPath,
  deriveMapping,
  sliceAround,
  fetchSnippet,
) where

import Data.Cache (Cache)
import Data.Cache qualified as Cache
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
import Models.Projects.GitSync (GitHubCredentialId, credentialConn, credentialCreds, getGitHubCredential, githubToken)
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.Projects (ProjectId)
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Git qualified as Git
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


-- | Why a frame has no source to show.
--
-- Typed rather than a sentence, because the renderer acts on the reason: 'NoMapping' is the
-- one the reader can fix from where they are standing, and it carries the path to fix it
-- with. Matching that case out of prose is how a copy edit silently removes a control.
data SnippetError
  = NoMapping Text
  | -- | The mapping's grant was deleted out from under it.
    CredentialGone
  | -- | Reaching GitHub failed: repo\/path\/ref, and what went wrong.
    ReadFailed Text Text
  | -- | The line is past the end of the file at that revision — almost always a build\/source mismatch.
    LineOutOfRange Text Int Text
  deriving stock (Eq, Generic, Show)


-- | The reason as a line of prose for the frame panel.
--
-- >>> snippetErrorMessage (NoMapping "/srv/app/checkout.py")
-- "No linked repository covers /srv/app/checkout.py."
-- >>> snippetErrorMessage (LineOutOfRange "src/checkout.py" 900 "main")
-- "Line 900 is past the end of src/checkout.py at main \8212 the build that threw is probably not this revision."
snippetErrorMessage :: SnippetError -> Text
snippetErrorMessage = \case
  NoMapping path -> "No linked repository covers " <> path <> "."
  CredentialGone -> "The GitHub credential this repository was linked with is no longer configured."
  ReadFailed what err -> "Could not read " <> what <> ": " <> err
  LineOutOfRange path n ref -> "Line " <> show n <> " is past the end of " <> path <> " at " <> ref <> " — the build that threw is probably not this revision."


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
  (cm, rest) <- listToMaybe $ sortOn (Down . T.length . (.pathPrefix) . fst) candidates
  let root = T.dropWhileEnd (== '/') cm.sourceRoot
  pure (cm, if T.null root then rest else root <> "/" <> rest)
  where
    candidates =
      [ (cm, rest)
      | cm <- mappings
      , maybe True ((== svc) . Just) cm.service
      , Just rest <- [T.stripPrefix cm.pathPrefix path]
      ]


-- | The inverse of 'resolveRepoPath': given the repo's files and one real stack-frame path,
-- work out the @(pathPrefix, sourceRoot)@ that would map the second onto the first, and the
-- repo file it matched.
--
-- Nobody knows offhand that their container mounts the app at @\/srv\/app@ and that the repo
-- keeps it under @src\/@ — but everybody can paste a line out of a stack trace they are
-- looking at. So the two cryptic fields are derived from an example and confirmed by naming
-- the file we found, rather than typed in and discovered to be wrong later.
--
-- >>> let tree = ["README.md", "src/services/checkout.py", "src/main.py"]
-- >>> deriveMapping tree "/srv/app/services/checkout.py"
-- Just ("/srv/app/","src","src/services/checkout.py")
--
-- The longest matching run of trailing segments wins, so a basename that appears twice in the
-- repo resolves to the one whose directory also lines up:
--
-- >>> deriveMapping ["a/util.go", "pkg/db/util.go"] "/build/db/util.go"
-- Just ("/build/","pkg","pkg/db/util.go")
--
-- Frames that are already repo-relative derive the catch-all mapping — empty prefix, repo
-- root — which is exactly the mapping they need:
--
-- >>> deriveMapping ["app/main.go"] "app/main.go"
-- Just ("","","app/main.go")
--
-- Windows separators are normalised, since the frame comes from whatever printed it:
--
-- >>> deriveMapping ["src/App.cs"] "C:\\build\\src\\App.cs"
-- Just ("C:/build/","","src/App.cs")
--
-- A path no file in the repo could be is 'Nothing' rather than a guess — a mapping derived
-- from a mismatch would silently render the wrong file as the failing code:
--
-- >>> deriveMapping ["src/main.py"] "/usr/lib/python3.11/json/decoder.py"
-- Nothing
-- >>> deriveMapping [] "app/main.go"
-- Nothing
deriveMapping :: [Text] -> Text -> Maybe (Text, Text, Text)
deriveMapping tree frame = do
  (k, repoPath) <- listToMaybe $ sortOn (bimap Down T.length) candidates
  let prefix = dropTail k frameRev
  pure (if T.null prefix then "" else prefix <> "/", dropTail k (segsRev repoPath), repoPath)
  where
    segsRev = reverse . T.splitOn "/"
    frameRev = segsRev (T.replace "\\" "/" frame)
    -- takes reversed segments, so dropping the matched tail is a plain 'drop'
    dropTail n = T.intercalate "/" . reverse . drop n
    candidates = [(k, p) | p <- tree, let k = length $ takeWhile identity $ zipWith (==) frameRev (segsRev p), k > 0]


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
  | focus < 1 || focus > n = Nothing
  | otherwise = Just $ Snippet "" start focus (take (stop - start + 1) (drop (start - 1) ls))
  where
    n = length ls
    start = max 1 (focus - ctx)
    stop = min n (focus + ctx)


-- | Fetch the source around @(file, line)@ for a project, or say why not.
--
-- Returns 'Left' with a reason rather than 'Nothing': "no mapping covers this path" and "the
-- repo does not have that file at this branch" are different problems with different fixes,
-- and a panel that renders both as silence sends the reader to configure something that is
-- already configured.
fetchSnippet
  :: (DB es, Log :> es, W.HTTP :> es)
  => Cache Config.CodeBlobKey ByteString
  -- ^ Blob cache. One git-host API call per frame opened without it, and a hot issue viewed
  -- repeatedly re-fetches every time; a rate-limit stall then presents as the panel silently
  -- not filling in. Cached on the blob fetch alone, so the mapping lookup, the credential
  -- decrypt and the token exchange still run — a revoked credential fails on the next view,
  -- not 15 minutes later.
  -> Config.EnvConfig
  -> ProjectId
  -> Maybe Text
  -> Maybe Text
  -- ^ The revision the telemetry reported, if it reported one. Wins over the mapping's ref:
  -- the snippet must be the source that threw, not the source as it is now.
  -> Text
  -> Int
  -> Eff es (Either SnippetError Snippet)
fetchSnippet blobCache cfg pid svc revM path lineNo = runExceptT do
  mappings <- lift $ getCodeMappings pid
  (cm, repoPath) <- hoistEither $ maybeToRight (NoMapping path) $ resolveRepoPath mappings svc path
  let repoRef = GitSync.RepoRef cm.owner cm.repo (fromMaybe cm.ref revM)
      located = cm.owner <> "/" <> cm.repo <> "/" <> repoPath <> " at " <> repoRef.ref
  cred <- ExceptT $ maybeToRight CredentialGone <$> getGitHubCredential (encodeUtf8 cfg.apiKeyEncryptionSecretKey) pid cm.credentialId
  creds <- hoistEither $ maybeToRight CredentialGone $ credentialCreds cred
  token <- ExceptT $ first (ReadFailed located) <$> githubToken cfg.githubAppId cfg.githubAppPrivateKey creds
  conn <- hoistEither $ first (ReadFailed located) $ credentialConn cred token
  let blobKey = (cm.owner, cm.repo, repoRef.ref, repoPath)
  cached <- liftIO $ Cache.lookup blobCache blobKey
  blob <- case cached of
    Just b -> pure b
    Nothing -> do
      -- Only a success is stored: caching a failure would keep a transient 502 or a
      -- just-revoked token on screen for the whole TTL.
      b <- ExceptT $ first (ReadFailed located) <$> Git.fetchFile conn repoRef repoPath
      liftIO $ Cache.insert blobCache blobKey b
      pure b
  (\s -> s{path = repoPath})
    <$> hoistEither (maybeToRight (LineOutOfRange repoPath lineNo repoRef.ref) $ sliceAround 5 lineNo (lines (decodeUtf8 blob)))
