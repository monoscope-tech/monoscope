-- | Talking to a git host's REST API, in whichever dialect it speaks.
--
-- Four hosts expose the same six operations we need — read a tree, read a file, write a file,
-- list repositories, report a default branch, sign a webhook — behind four different URL
-- shapes, three different auth headers and four different webhook signatures. This module is
-- the one place that knows the differences; every caller works in terms of a 'GitConn'.
--
-- A leaf by construction: 'W.HTTP' and the deriving helpers only, no DB and no
-- @Models.Projects.Dashboards@, so it stays out of the @GitSync → Widget → LogItem@ import
-- cycle that forced "Pages.CodeContext" into its own module.
--
-- See @plans/07-git-hosts.md@.
module Pkg.Git (
  -- * Hosts
  GitHost (..),
  hostSlug,
  parseHostSlug,
  hostLabel,
  hostOriginRule,
  OriginRule (..),

  -- * Connections
  BaseUrl,
  unBaseUrl,
  Sensitive (..),
  GitConn (..),
  mkGitConn,
  validateOrigin,
  normalizeOrigin,
  tryHttp,
  formatHttpError,

  -- * Coordinates
  RepoRef (..),
  TreeEntry (..),
  GitRepo (..),
  splitFullName,

  -- * Operations
  fetchTree,
  fetchFile,
  pushFile,
  listRepos,
  defaultBranchOf,
  computeContentSha,

  -- * Webhooks
  WebhookReq (..),
  isPushEvent,
  parseWebhookRepo,
  parseWebhookRevision,
  verifyWebhook,
) where

import Control.Lens (toListOf, (.~), (^.), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, values, _Bool, _Integer, _String)
import Data.Base64.Types (extractBase64)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Effectful.Wreq qualified as W
import Data.Text qualified as T
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Effectful (Eff, IOE, (:>))
import Hasql.Interpolate qualified as HI
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), responseStatus)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)
import Network.URI (URI (..), URIAuth (..), parseURI)
import Network.Wreq (FormParam (..))
import Pkg.DeriveUtils (enumFromField, refineText)
import Relude
import Relude.Extra.Bifunctor (firstF)

-- Relude re-exports 'Show' without its methods, so the redacting instance below has to name
-- 'showsPrec' from its defining module.
import Text.Show (showString, showsPrec)
import UnliftIO.Exception (try)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))
import "base64" Data.ByteString.Base64 qualified as B64
import "cryptonite" Crypto.Hash (Digest, SHA1, hash)
import "cryptonite" Crypto.Hash.Algorithms (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC
import "memory" Data.ByteArray qualified as BA


-- | Which REST dialect a connection speaks.
--
-- The DB stores the slug, not the constructor name: @show GitHub@ would snake-case to
-- @git_hub@, which is nobody's idea of a host name. 'hostSlug' and 'parseHostSlug' are the one
-- place that mapping lives.
data GitHost = GitHub | GitLab | Bitbucket | Gitea
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (NFData)


-- | How the host is spelled to a human — which is exactly how it is spelled as a constructor.
--
-- >>> map hostLabel [minBound .. maxBound]
-- ["GitHub","GitLab","Bitbucket","Gitea"]
hostLabel :: GitHost -> Text
hostLabel = show


-- | The stored/wire name of a host.
--
-- Derived from the constructor, which means the doctest below is load-bearing rather than
-- decorative: these four strings are also the values in migration 0125's CHECK constraint and
-- in every @git_sync.host@ row already written, so renaming a constructor would silently
-- change what we persist. The doctest turns that into a failing build.
--
-- >>> map hostSlug [minBound .. maxBound]
-- ["github","gitlab","bitbucket","gitea"]
hostSlug :: GitHost -> Text
hostSlug = T.toLower . hostLabel


-- | The slug back to a host.
--
-- >>> map parseHostSlug ["github", "gitlab", "bitbucket", "gitea"]
-- [Just GitHub,Just GitLab,Just Bitbucket,Just Gitea]
--
-- Anything else is not a host we speak, and guessing would mean sending a token somewhere:
--
-- >>> map parseHostSlug ["git_hub", "GitHub", "sourcehut", ""]
-- [Nothing,Nothing,Nothing,Nothing]
parseHostSlug :: Text -> Maybe GitHost
parseHostSlug = inverseMap hostSlug


-- | Hand-written rather than via 'WrappedEnumSC': that wrapper snake-cases the constructor,
-- which would make @GitHub@ into @git_hub@ — a value 0125's CHECK constraint rejects, and not
-- what anyone would type into a URL either. 'hostSlug' is the single source of the spelling.
instance AE.ToJSON GitHost where
  toJSON = AE.toJSON . hostSlug


instance AE.FromJSON GitHost where
  parseJSON = AE.withText "GitHost" $ maybe (fail "unknown git host") pure . parseHostSlug


instance ToField GitHost where
  toField = toField . hostSlug


instance FromField GitHost where
  fromField = enumFromField identity toString (parseHostSlug . toText)


instance HI.EncodeValue GitHost where
  encodeValue = contramap hostSlug HI.encodeValue


instance HI.DecodeValue GitHost where
  decodeValue = refineText "GitHost" parseHostSlug


instance HI.DecodeRow GitHost where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


instance FromHttpApiData GitHost where
  parseUrlPiece = maybeToRight "unknown git host" . parseHostSlug


instance ToHttpApiData GitHost where
  toUrlPiece = hostSlug


-- | A token that must never reach a log line, a rendered page, or a @Show@ instance.
--
-- The wrapper exists so that forgetting is impossible rather than merely discouraged:
-- 'GitConn' has no 'Show', and anything that does contain a token shows as @\<redacted\>@.
newtype Sensitive a = Sensitive {reveal :: a}
  deriving stock (Eq, Generic)


-- | >>> print (Sensitive @Text "glpat-hunter2")
-- <redacted>
instance Show (Sensitive a) where
  showsPrec _ _ = showString "<redacted>"


-- | A validated API base. The constructor is hidden: the only way to hold one is through
-- 'mkGitConn', which is where the URL and SSRF rules are applied.
newtype BaseUrl = BaseUrl Text
  deriving stock (Eq, Generic, Show)


unBaseUrl :: BaseUrl -> Text
unBaseUrl (BaseUrl t) = t


-- | Everything one API call needs: whose dialect, where it lives, and the token.
--
-- Deliberately no 'Show' — it holds a credential, and a derived instance is how credentials
-- end up in exception text.
data GitConn = GitConn
  { host :: GitHost
  , apiBase :: BaseUrl
  , token :: Sensitive Text
  }
  deriving stock (Generic)


-- | What a host does with a self-hosted origin.
data OriginRule
  = -- | SaaS default; an origin switches it to a self-hosted install at this API path.
    OriginOptional Text Text
  | -- | No SaaS: an origin is required, and the API lives at this path under it.
    OriginRequired Text
  | -- | Cloud only; an origin is a configuration error rather than an unsupported feature.
    OriginRejected Text
  deriving stock (Eq, Generic, Show)


-- | Where each host's API lives, and whether it can be moved.
--
-- >>> hostOriginRule Gitea
-- OriginRequired "/api/v1"
-- >>> hostOriginRule Bitbucket
-- OriginRejected "https://api.bitbucket.org/2.0"
hostOriginRule :: GitHost -> OriginRule
hostOriginRule = \case
  GitHub -> OriginOptional "https://api.github.com" "/api/v3" -- Enterprise Server
  GitLab -> OriginOptional "https://gitlab.com/api/v4" "/api/v4"
  Gitea -> OriginRequired "/api/v1" -- always self-hosted
  Bitbucket -> OriginRejected "https://api.bitbucket.org/2.0" -- Server is EOL; Cloud only


-- | Normalise and vet a user-supplied origin.
--
-- Everything a self-hosted origin is allowed to be: an @https@ scheme, a host, an optional
-- port and an optional path. Credentials in the URL, a query or a fragment are all rejected —
-- they cannot mean anything for an API base, and userinfo in particular is a secret that would
-- then be concatenated into every request URL and logged with it.
--
-- >>> normalizeOrigin "https://git.acme.dev"
-- Right "https://git.acme.dev"
--
-- A trailing slash is noise, and a path prefix is kept because reverse proxies really do
-- mount a forge under one:
--
-- >>> normalizeOrigin "https://acme.dev/gitlab/"
-- Right "https://acme.dev/gitlab"
--
-- A bare host is a typo we can fix rather than reject:
--
-- >>> normalizeOrigin "git.acme.dev"
-- Right "https://git.acme.dev"
--
-- Plaintext would send the token in the clear, so it is refused rather than upgraded — an
-- automatic upgrade would hide a misconfiguration that matters:
--
-- >>> normalizeOrigin "http://git.acme.dev"
-- Left "Use https:// \8212 an http:// server would receive your access token in clear text."
--
-- >>> normalizeOrigin "https://user:pw@git.acme.dev"
-- Left "Remove the username and password from the URL; the access token is the credential."
-- >>> normalizeOrigin "https://git.acme.dev?x=1"
-- Left "Remove the query string and fragment; this is a server address, not a link."
-- >>> normalizeOrigin ""
-- Left "Enter the URL of your server."
normalizeOrigin :: Text -> Either Text Text
normalizeOrigin raw
  | T.null trimmed = Left "Enter the URL of your server."
  | any (`T.isInfixOf` trimmed) ["?", "#"] = Left "Remove the query string and fragment; this is a server address, not a link."
  | "http://" `T.isPrefixOf` T.toLower trimmed = Left insecure
  | otherwise = do
      uri <- maybeToRight "That is not a URL we can parse." $ parseURI (toString withScheme)
      auth <- maybeToRight "That URL has no server name in it." uri.uriAuthority
      unless (uri.uriScheme == "https:") $ Left insecure
      unless (null auth.uriUserInfo) $ Left "Remove the username and password from the URL; the access token is the credential."
      unless (null uri.uriQuery && null uri.uriFragment) $ Left "Remove the query string and fragment; this is a server address, not a link."
      when (null auth.uriRegName) $ Left "That URL has no server name in it."
      pure $ T.dropWhileEnd (== '/') $ toText (uri.uriScheme <> "//" <> auth.uriRegName <> auth.uriPort <> uri.uriPath)
  where
    trimmed = T.strip raw
    withScheme = if "https://" `T.isPrefixOf` T.toLower trimmed then trimmed else "https://" <> trimmed
    insecure = "Use https:// — an http:// server would receive your access token in clear text." :: Text


-- | Build a connection, or say why the host and origin cannot make one.
--
-- The one place the "Gitea has no SaaS" and "Bitbucket has no self-hosted" rules are enforced,
-- so no operation downstream needs an arm for a host with nowhere to talk to.
--
-- >>> unBaseUrl . (.apiBase) <$> mkGitConn GitHub Nothing "t"
-- Right "https://api.github.com"
-- >>> unBaseUrl . (.apiBase) <$> mkGitConn GitLab Nothing "t"
-- Right "https://gitlab.com/api/v4"
-- >>> unBaseUrl . (.apiBase) <$> mkGitConn GitLab (Just "https://git.acme.dev/") "t"
-- Right "https://git.acme.dev/api/v4"
-- >>> unBaseUrl . (.apiBase) <$> mkGitConn Gitea (Just "code.acme.dev") "t"
-- Right "https://code.acme.dev/api/v1"
-- >>> unBaseUrl . (.apiBase) <$> mkGitConn GitHub (Just "https://gh.acme.dev") "t"
-- Right "https://gh.acme.dev/api/v3"
--
-- The two host-shaped errors:
--
-- >>> unBaseUrl . (.apiBase) <$> mkGitConn Gitea Nothing "t"
-- Left "Gitea is always self-hosted \8212 enter the URL of your server."
-- >>> unBaseUrl . (.apiBase) <$> mkGitConn Bitbucket (Just "https://bb.acme.dev") "t"
-- Left "Bitbucket Cloud only; a server URL is not supported here."
--
-- A blank token cannot authenticate anything, and failing here beats a 401 from four
-- different hosts in four different shapes:
--
-- >>> unBaseUrl . (.apiBase) <$> mkGitConn GitHub Nothing "  "
-- Left "Enter an access token."
mkGitConn :: GitHost -> Maybe Text -> Text -> Either Text GitConn
mkGitConn h originM tok = do
  when (T.null $ T.strip tok) $ Left "Enter an access token."
  base <- validateOrigin h originM
  pure $ GitConn h base (Sensitive tok)


-- | Where a host and origin say the API lives — the half of 'mkGitConn' that has nothing to
-- do with the token.
--
-- Separate so a settings form can vet a host/origin pair on an update that keeps the stored
-- token, without inventing a placeholder token for the check.
--
-- >>> unBaseUrl <$> validateOrigin GitLab (Just "https://git.acme.dev/")
-- Right "https://git.acme.dev/api/v4"
-- >>> unBaseUrl <$> validateOrigin Gitea Nothing
-- Left "Gitea is always self-hosted \8212 enter the URL of your server."
validateOrigin :: GitHost -> Maybe Text -> Either Text BaseUrl
validateOrigin h originM =
  BaseUrl <$> case (hostOriginRule h, mfilter (not . T.null . T.strip) originM) of
    (OriginOptional saas _, Nothing) -> Right saas
    (OriginOptional _ path, Just o) -> (<> path) <$> normalizeOrigin o
    (OriginRequired _, Nothing) -> Left $ hostLabel h <> " is always self-hosted — enter the URL of your server."
    (OriginRequired path, Just o) -> (<> path) <$> normalizeOrigin o
    (OriginRejected saas, Nothing) -> Right saas
    (OriginRejected _, Just _) -> Left $ hostLabel h <> " Cloud only; a server URL is not supported here."


-- | Which repo, at which revision.
data RepoRef = RepoRef {owner :: Text, repo :: Text, ref :: Text}
  deriving stock (Eq, Generic, Show)


-- | One entry in a repository listing.
--
-- @sha@ is 'Maybe' because Bitbucket's directory listing genuinely has no per-file hash: its
-- entries carry a path, a type and a size, and the @commit.hash@ on them is the commit that
-- was listed, not the one that last touched the file. Representing that as @""@ would make
-- "unchanged" and "unknown" the same value to 'buildSyncPlan', which is how a sync silently
-- stops noticing edits.
data TreeEntry = TreeEntry
  { path :: Text
  , isBlob :: Bool
  , sha :: Maybe Text
  , size :: Maybe Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | A repository as a picker needs it.
data GitRepo = GitRepo
  { fullName :: Text
  , name :: Text
  , private :: Bool
  , defaultBranch :: Text
  }
  deriving stock (Eq, Generic, Show)


-- | Split @owner/repo@ at the *last* separator.
--
-- GitLab namespaces nest arbitrarily, so the owner is everything up to the final slash rather
-- than the first path segment:
--
-- >>> splitFullName "acme/checkout"
-- ("acme","checkout")
-- >>> splitFullName "acme/platform/team/checkout"
-- ("acme/platform/team","checkout")
--
-- A name with no separator has no owner to report, rather than becoming its own owner:
--
-- >>> splitFullName "checkout"
-- ("","checkout")
splitFullName :: Text -> (Text, Text)
splitFullName = first (T.dropWhileEnd (== '/')) . T.breakOnEnd "/"


----------------------------------------------------------------------
-- URL construction
----------------------------------------------------------------------

-- | Percent-encode one path segment. @'/'@ is escaped: every caller here is placing a single
-- segment, and a slash arriving from a branch name or an owner is path traversal, not a
-- separator we meant.
--
-- >>> enc "feature/big bang"
-- "feature%2Fbig%20bang"
-- >>> enc "acme/platform/checkout"
-- "acme%2Fplatform%2Fcheckout"
enc :: Text -> Text
enc = decodeUtf8 . urlEncode True . encodeUtf8


-- | Percent-encode a path that is allowed to keep its separators — a file path inside a repo,
-- where the slashes are real.
--
-- >>> encPath "src/app/main.go"
-- "src/app/main.go"
-- >>> encPath "src/my file.go"
-- "src/my%20file.go"
encPath :: Text -> Text
encPath = T.intercalate "/" . map enc . T.splitOn "/"


-- | The URL piece that names a repository on each host.
--
-- >>> repoSegment GitHub (RepoRef "acme" "checkout" "main")
-- "repos/acme/checkout"
-- >>> repoSegment Gitea (RepoRef "acme" "checkout" "main")
-- "repos/acme/checkout"
-- >>> repoSegment Bitbucket (RepoRef "acme" "checkout" "main")
-- "repositories/acme/checkout"
--
-- GitLab addresses a project by its URL-encoded full path, which is why a nested group works
-- at all:
--
-- >>> repoSegment GitLab (RepoRef "acme/platform" "checkout" "main")
-- "projects/acme%2Fplatform%2Fcheckout"
repoSegment :: GitHost -> RepoRef -> Text
repoSegment h r = case h of
  GitHub -> "repos/" <> enc r.owner <> "/" <> enc r.repo
  Gitea -> "repos/" <> enc r.owner <> "/" <> enc r.repo
  Bitbucket -> "repositories/" <> enc r.owner <> "/" <> enc r.repo
  GitLab -> "projects/" <> enc (r.owner <> "/" <> r.repo)


url :: GitConn -> Text -> Text
url conn suffix = unBaseUrl conn.apiBase <> "/" <> suffix


repoUrl :: GitConn -> RepoRef -> Text -> Text
repoUrl conn r suffix = url conn (repoSegment conn.host r <> suffix)


----------------------------------------------------------------------
-- Requests
----------------------------------------------------------------------

-- | The auth header each host wants. Three schemes across four hosts.
gitOpts :: GitConn -> W.Options
gitOpts conn =
  foldl'
    (\o (k, v) -> o & W.header k .~ [v])
    W.defaults
    [(authName, encodeUtf8 authValue :: ByteString), ("Accept", "application/json"), ("User-Agent", "Monoscope")]
  where
    tok = conn.token.reveal
    -- Both annotations are load-bearing: without them the literals default to 'String'.
    (authName :: HeaderName, authValue :: Text) = case conn.host of
      GitHub -> ("Authorization", "Bearer " <> tok)
      Gitea -> ("Authorization", "token " <> tok)
      Bitbucket -> ("Authorization", "Bearer " <> tok)
      GitLab -> ("PRIVATE-TOKEN", tok)


-- | Run a request, turning the exception a failed one throws into the 'Left' every operation
-- here reports failure through.
tryHttp :: IOE :> es => Eff es a -> Eff es (Either Text a)
tryHttp = firstF formatHttpError . try


formatHttpError :: HttpException -> Text
formatHttpError (HttpExceptionRequest _ content) = case content of
  StatusCodeException resp _ -> "HTTP " <> show (statusCode $ responseStatus resp)
  other -> "request failed: " <> show other
formatHttpError (InvalidUrlException u reason) = "invalid URL (" <> toText u <> "): " <> toText reason


get_ :: (IOE :> es, W.HTTP :> es) => GitConn -> Text -> Eff es (Either Text LByteString)
get_ conn u = fmap (^. W.responseBody) <$> tryHttp (W.getWith (gitOpts conn) (toString u))


-- | Walk a host's pagination to the end, or fail loudly. A page shorter than @size@ is the last.
--
-- Never returns a short list quietly: a truncated repository list looks exactly like an
-- account with fewer repositories, and a truncated tree looks exactly like a deleted file —
-- which on the sync path would be read as "delete this dashboard".
paginate :: Monad m => Int -> (Int -> m (Either Text [a])) -> m (Either Text [a])
paginate size fetchPage = go 1 []
  where
    pageLimit = 20 :: Int
    go p acc
      | p > pageLimit = pure $ Left $ "More than " <> show pageLimit <> " pages of results; refusing to continue with a partial list."
      | otherwise =
          fetchPage p >>= \case
            Left e -> pure $ Left e
            Right batch
              | length batch == size -> go (p + 1) (acc <> batch)
              | otherwise -> pure $ Right (acc <> batch)


-- | Every page of a paginated JSON list: @items@ picks the array out of one response body,
-- @dec@ decodes an element and drops the ones missing a field we need.
pagedJson :: (IOE :> es, W.HTTP :> es) => GitConn -> Int -> (LByteString -> [AE.Value]) -> (AE.Value -> Maybe a) -> (Int -> Text) -> Eff es (Either Text [a])
pagedJson conn size items dec mkUrl = paginate size \p -> fmap (mapMaybe dec . items) <$> get_ conn (mkUrl p)


----------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------

-- | The resolved head commit of @r.ref@ — the revision marker every host can report.
--
-- The commits endpoint rather than the branches one, because @ref@ may already be a commit
-- sha and only the former accepts both.
headRevision :: (IOE :> es, W.HTTP :> es) => GitConn -> RepoRef -> Eff es (Either Text Text)
headRevision conn r = via $ case conn.host of
  GitHub -> ("/commits/" <> enc r.ref, key "sha" . _String)
  Gitea -> ("/git/commits/" <> enc r.ref, key "sha" . _String)
  GitLab -> ("/repository/commits?ref_name=" <> enc r.ref <> "&per_page=1", values . key "id" . _String)
  Bitbucket -> ("/commits/" <> enc r.ref <> "?pagelen=1", key "values" . values . key "hash" . _String)
  where
    via (suffix, pick) =
      get_ conn (repoUrl conn r suffix)
        <&> (>>= maybeToRight (hostLabel conn.host <> " reported no commit for " <> r.ref <> ".") . (^? pick))


-- | List a repository at @ref@, scoped to @prefix@, with the head revision.
--
-- @prefix@ is not a convenience: it is what keeps the Bitbucket sha backfill bounded (see
-- below) and it lets GitLab and Bitbucket filter server-side. Pass @""@ for the whole tree
-- when only paths are wanted.
--
-- On Bitbucket the listing carries no per-file hash, so entries under a non-empty @prefix@
-- have their sha backfilled by reading the blob and hashing it the way git would. With an
-- empty prefix that would be one request per file in the repository, so it is skipped and
-- those entries keep @sha = Nothing@ — which 'TreeEntry' makes impossible to mistake for
-- "unchanged".
fetchTree :: (IOE :> es, W.HTTP :> es) => GitConn -> RepoRef -> Text -> Eff es (Either Text (Text, [TreeEntry]))
fetchTree conn r prefix = runExceptT do
  entries <- ExceptT rawEntries
  let scoped = filter (\e -> T.null prefix || prefix `T.isPrefixOf` e.path) entries
  filled <- ExceptT $ backfill scoped
  rev <- ExceptT $ headRevision conn r
  pure (rev, filled)
  where
    rawEntries = case conn.host of
      GitHub -> githubish ("/git/trees/" <> enc r.ref <> "?recursive=1")
      Gitea -> githubish ("/git/trees/" <> enc r.ref <> "?recursive=true&per_page=1000")
      GitLab -> pagedJson conn 100 (toListOf values) gitlabEntry \p ->
        repoUrl conn r ("/repository/tree?ref=" <> enc r.ref <> "&recursive=true&per_page=100&page=" <> show p <> pathQ)
      Bitbucket -> pagedJson conn 100 (toListOf (key "values" . values)) bitbucketEntry \p ->
        repoUrl conn r ("/src/" <> enc r.ref <> "/" <> encPath prefix <> "?max_depth=100&pagelen=100&page=" <> show p)

    pathQ = if T.null prefix then "" else "&path=" <> enc (T.dropWhileEnd (== '/') prefix)

    -- GitHub and Gitea both answer with git's own tree object, so one decoder serves both.
    githubish suffix =
      get_ conn (repoUrl conn r suffix)
        <&> ( >>=
                \body ->
                  if body ^? key "truncated" . _Bool == Just True
                    then Left "Repository listing was truncated by the host; it is too large to sync."
                    else Right $ mapMaybe treeObjEntry $ toListOf (key "tree" . values) body
            )

    treeObjEntry v = do
      p <- v ^? key "path" . _String
      ty <- v ^? key "type" . _String
      pure $ TreeEntry p (ty == "blob") (v ^? key "sha" . _String) (fromIntegral <$> v ^? key "size" . _Integer)

    -- GitLab's `id` is the git object sha, so blobs arrive already content-addressed.
    gitlabEntry v = do
      p <- v ^? key "path" . _String
      ty <- v ^? key "type" . _String
      pure $ TreeEntry p (ty == "blob") (v ^? key "id" . _String) Nothing

    bitbucketEntry v = do
      p <- v ^? key "path" . _String
      ty <- v ^? key "type" . _String
      pure $ TreeEntry p (ty == "commit_file") Nothing (fromIntegral <$> v ^? key "size" . _Integer)

    backfill es
      | conn.host /= Bitbucket || T.null prefix = pure $ Right es
      | otherwise =
          sequence <$> forM es \e ->
            if not e.isBlob
              then pure $ Right e
              else fmap (\blob -> e{sha = Just (computeContentSha blob)}) <$> fetchFile conn r e.path


-- | The blob at @path@ in @r.ref@ — a branch name or a commit sha, since every host's read
-- endpoint takes either. Reading at the sha the telemetry reported is the difference between
-- the source that threw and the source as it is today.
fetchFile :: (IOE :> es, W.HTTP :> es) => GitConn -> RepoRef -> Text -> Eff es (Either Text ByteString)
fetchFile conn r path = case conn.host of
  GitHub -> contents
  Gitea -> contents
  GitLab -> raw <$> get_ conn (repoUrl conn r ("/repository/files/" <> enc path <> "/raw?ref=" <> enc r.ref))
  Bitbucket -> raw <$> get_ conn (repoUrl conn r ("/src/" <> enc r.ref <> "/" <> encPath path))
  where
    contents = b64 <$> get_ conn (repoUrl conn r ("/contents/" <> encPath path <> "?ref=" <> enc r.ref))
    raw = fmap toStrict
    b64 body = do
      content <- maybeToRight "No content field in the response." . (^? key "content" . _String) =<< body
      B64.decodeBase64Untyped $ encodeUtf8 $ T.filter (/= '\n') content


-- | Write @content@ to @path@, returning @(blob sha, head revision)@.
--
-- @existingSha@ distinguishes create from update. Only GitHub and Gitea can use it as a
-- concurrency precondition; GitLab's equivalent field wants a *commit* id rather than a blob
-- sha, so passing this one there would be a type-confusion bug that silently rejects valid
-- writes — it is omitted instead, and the weaker guarantee is the documented cost.
--
-- GitLab and Bitbucket return no sha at all, so the blob sha is computed locally with
-- 'computeContentSha' — the same value git stores and the same value GitLab's own tree
-- listing reports, so the two agree on the next pull.
pushFile :: (IOE :> es, W.HTTP :> es) => GitConn -> RepoRef -> Text -> ByteString -> Maybe Text -> Text -> Eff es (Either Text (Text, Text))
pushFile conn r path content existingSha message = runExceptT do
  shaM <- ExceptT $ case conn.host of
    GitHub -> contentsPut
    Gitea -> contentsPut
    GitLab -> do
      let body =
            AE.object
              [ "branch" AE..= r.ref
              , "content" AE..= extractBase64 (B64.encodeBase64 content)
              , "encoding" AE..= ("base64" :: Text)
              , "commit_message" AE..= message
              ]
          u = toString $ repoUrl conn r ("/repository/files/" <> enc path)
          send = maybe (W.postWith (gitOpts conn) u body) (const $ W.putWith (gitOpts conn) u body) existingSha
      fmap (const Nothing) <$> tryHttp send
    Bitbucket ->
      -- form-urlencoded rather than multipart: the field *name* is the file path, and the
      -- commit metadata rides alongside as ordinary fields.
      fmap (const Nothing)
        <$> tryHttp
          ( W.postWith
              (gitOpts conn)
              (toString $ repoUrl conn r "/src")
              ( [ (encodeUtf8 path :: ByteString) := content
                , ("message" :: ByteString) := (encodeUtf8 message :: ByteString)
                , ("branch" :: ByteString) := (encodeUtf8 r.ref :: ByteString)
                ]
                  :: [FormParam]
              )
          )
  rev <- ExceptT $ headRevision conn r
  pure (fromMaybe (computeContentSha content) shaM, rev)
  where
    contentsPut = do
      let body =
            AE.object
              $ [ "message" AE..= message
                , "content" AE..= extractBase64 (B64.encodeBase64 content)
                , "branch" AE..= r.ref
                ]
              <> maybeToList (("sha" AE..=) <$> existingSha)
      resp <- tryHttp $ W.putWith (gitOpts conn) (toString $ repoUrl conn r ("/contents/" <> encPath path)) body
      pure $ (^? W.responseBody . key "content" . key "sha" . _String) <$> resp


-- | The repositories this token reaches.
--
-- A repository-scoped token legitimately reaches one repository, or none it can enumerate, so
-- an empty list is a valid answer and the caller keeps manual entry available.
listRepos :: (IOE :> es, W.HTTP :> es) => GitConn -> Eff es (Either Text [GitRepo])
listRepos conn = case conn.host of
  GitHub -> pagedJson conn 100 (toListOf (key "repositories" . values)) ghRepo \p -> url conn ("installation/repositories?per_page=100&page=" <> show p)
  Gitea -> pagedJson conn 50 (toListOf values) ghRepo \p -> url conn ("user/repos?limit=50&page=" <> show p)
  GitLab -> pagedJson conn 100 (toListOf values) glRepo \p -> url conn ("projects?membership=true&simple=true&per_page=100&page=" <> show p)
  Bitbucket -> pagedJson conn 100 (toListOf (key "values" . values)) bbRepo \p -> url conn ("repositories?role=member&pagelen=100&page=" <> show p)
  where
    ghRepo v = do
      full <- v ^? key "full_name" . _String
      pure $ GitRepo full (snd $ splitFullName full) (v ^? key "private" . _Bool == Just True) (fromMaybe "main" $ v ^? key "default_branch" . _String)
    glRepo v = do
      full <- v ^? key "path_with_namespace" . _String
      pure $ GitRepo full (snd $ splitFullName full) (v ^? key "visibility" . _String /= Just "public") (fromMaybe "main" $ v ^? key "default_branch" . _String)
    bbRepo v = do
      full <- v ^? key "full_name" . _String
      pure $ GitRepo full (snd $ splitFullName full) (v ^? key "is_private" . _Bool == Just True) (fromMaybe "main" $ v ^? key "mainbranch" . key "name" . _String)


-- | The repository's default branch, or @main@ when the host will not say.
defaultBranchOf :: (IOE :> es, W.HTTP :> es) => GitConn -> RepoRef -> Eff es Text
defaultBranchOf conn r = get_ conn (repoUrl conn r "") <&> either (const "main") (fromMaybe "main" . pick)
  where
    pick body = case conn.host of
      Bitbucket -> body ^? key "mainbranch" . key "name" . _String
      GitHub -> dflt
      Gitea -> dflt
      GitLab -> dflt
      where
        dflt = body ^? key "default_branch" . _String


-- | Git's own blob hash: @sha1("blob <len>\\0<content>")@.
--
-- The value git stores, and the value GitLab reports as a tree entry's @id@ — which is what
-- lets a locally computed sha survive a round trip through a host that does not return one.
--
-- >>> computeContentSha "hello\n"
-- "ce013625030ba8dba906f756967f9e9ca394464a"
computeContentSha :: ByteString -> Text
computeContentSha content =
  decodeUtf8 $ B16.encode $ BA.convert (hash ("blob " <> show (BS.length content) <> "\0" <> content) :: Digest SHA1)


----------------------------------------------------------------------
-- Webhooks
----------------------------------------------------------------------

-- | An inbound delivery, before we have decided to believe it.
data WebhookReq = WebhookReq
  { event :: Maybe Text
  , signature :: Maybe Text
  -- ^ The host's signature header, whichever one it uses.
  , gitlabToken :: Maybe Text
  -- ^ @X-Gitlab-Token@ — the pre-19.1 plaintext scheme.
  , gitlabWebhookId :: Maybe Text
  , gitlabTimestamp :: Maybe Text
  , body :: ByteString
  }
  deriving stock (Generic)


-- | Is this the push event, in this host's spelling?
--
-- >>> [isPushEvent h (Just e) | (h, e) <- [(GitHub, "push"), (Gitea, "push"), (GitLab, "Push Hook"), (Bitbucket, "repo:push")]] :: [Bool]
-- [True,True,True,True]
--
-- Anything else is a hook we were sent but did not ask for, and a missing header is not a push:
--
-- >>> [isPushEvent GitHub (Just "pull_request"), isPushEvent GitLab (Just "Tag Push Hook"), isPushEvent GitHub Nothing] :: [Bool]
-- [False,False,False]
isPushEvent :: GitHost -> Maybe Text -> Bool
isPushEvent h = (== Just expected)
  where
    expected = case h of
      GitHub -> "push"
      Gitea -> "push"
      GitLab -> "Push Hook"
      Bitbucket -> "repo:push"


-- | The @owner/repo@ a delivery is about.
--
-- Parsed before verification and used only to find the row that holds the secret — never to
-- act on. Each host puts the same fact under a different key:
--
-- >>> parseWebhookRepo GitHub "{\"repository\":{\"full_name\":\"acme/checkout\"}}"
-- Just "acme/checkout"
-- >>> parseWebhookRepo GitLab "{\"project\":{\"path_with_namespace\":\"acme/team/checkout\"}}"
-- Just "acme/team/checkout"
-- >>> parseWebhookRepo Bitbucket "{\"repository\":{\"full_name\":\"acme/checkout\"}}"
-- Just "acme/checkout"
--
-- >>> parseWebhookRepo GitHub "not json"
-- Nothing
parseWebhookRepo :: GitHost -> ByteString -> Maybe Text
parseWebhookRepo h raw = case h of
  GitLab -> raw ^? key "project" . key "path_with_namespace" . _String
  GitHub -> full
  Gitea -> full
  Bitbucket -> full
  where
    full = raw ^? key "repository" . key "full_name" . _String


-- | The head commit a push delivery announces.
--
-- Kept so the sync job can tell "the host and I agree nothing changed" apart from "the host is
-- answering with a view older than the push it just told me about". Without it the second case
-- looks exactly like the first, and the push is dropped rather than retried.
--
-- GitHub, Gitea and GitLab all name the resulting head, under three different keys. Bitbucket
-- reports per-branch changes instead, so the new head is the first change's @new.target.hash@.
--
-- >>> parseWebhookRevision GitHub "{\"after\":\"abc123\"}"
-- Just "abc123"
-- >>> parseWebhookRevision Gitea "{\"after\":\"abc123\"}"
-- Just "abc123"
-- >>> parseWebhookRevision GitLab "{\"checkout_sha\":\"abc123\"}"
-- Just "abc123"
-- >>> parseWebhookRevision Bitbucket "{\"push\":{\"changes\":[{\"new\":{\"target\":{\"hash\":\"abc123\"}}}]}}"
-- Just "abc123"
--
-- A branch deletion announces the all-zero sha, which is not a revision anything can be
-- fetched at; treating it as one would pin a retry on a commit that cannot exist.
--
-- >>> parseWebhookRevision GitHub "{\"after\":\"0000000000000000000000000000000000000000\"}"
-- Nothing
-- >>> parseWebhookRevision GitHub "not json"
-- Nothing
parseWebhookRevision :: GitHost -> ByteString -> Maybe Text
parseWebhookRevision h raw = mfilter (T.any (/= '0')) $ case h of
  GitHub -> after
  Gitea -> after
  GitLab -> raw ^? key "checkout_sha" . _String
  Bitbucket -> raw ^? key "push" . key "changes" . values . key "new" . key "target" . key "hash" . _String
  where
    after = raw ^? key "after" . _String


-- | Decide whether a delivery really came from the host that owns this repository.
--
-- Three of the four are HMAC-SHA256 over the raw body, differing only in header name and
-- digest encoding. GitLab is the odd one: it signs @id.timestamp.body@ with a key that is the
-- signing token minus its @whsec_@ prefix, base64-decoded, and it may send several
-- space-separated candidate signatures during a key rotation.
--
-- >>> let payload = "{\"repository\":{\"full_name\":\"acme/checkout\"}}"
-- >>> let signed h s = WebhookReq Nothing (Just s) Nothing Nothing Nothing payload
-- >>> verifyWebhook GitHub (Just "s3cret") (signed GitHub ("sha256=" <> hmacHex "s3cret" payload))
-- Right ()
-- >>> verifyWebhook Gitea (Just "s3cret") (signed Gitea (hmacHex "s3cret" payload))
-- Right ()
-- >>> verifyWebhook Bitbucket (Just "s3cret") (signed Bitbucket ("sha256=" <> hmacHex "s3cret" payload))
-- Right ()
--
-- A wrong signature, and a signature that is simply absent, both fail — a row that has a
-- secret is a row that asked to be checked:
--
-- >>> verifyWebhook GitHub (Just "s3cret") (signed GitHub "sha256=deadbeef")
-- Left "invalid signature"
-- >>> verifyWebhook GitHub (Just "s3cret") (WebhookReq Nothing Nothing Nothing Nothing Nothing payload)
-- Left "signature required but not provided"
--
-- GitLab signs @id.timestamp.body@, tolerates several candidates during a key rotation, and
-- falls back to the pre-19.1 plaintext token when it sends no signature at all:
--
-- The signature is a fixed vector rather than one recomputed here: a doctest that reruns the
-- implementation only proves it equals itself. This is HMAC-SHA256 of
-- @"d1.1700000000." <> payload@ under @s3cret@, base64-encoded, computed independently.
--
-- >>> let glSig = "LH0LUfiRgkxfloRO3Blb6m2c/eUFLgXkWyqQFBPPiJo="
-- >>> let glReq s = WebhookReq Nothing (Just s) Nothing (Just "d1") (Just "1700000000") payload
-- >>> verifyWebhook GitLab (Just "s3cret") (glReq ("v1," <> glSig))
-- Right ()
--
-- Several candidates arrive space-separated while a signing key is being rotated, and any one
-- of them matching is enough:
--
-- >>> verifyWebhook GitLab (Just "s3cret") (glReq ("v1,rotated v1," <> glSig))
-- Right ()
--
-- The signed message includes the id and timestamp, so the same body replayed under a
-- different delivery id does not verify:
--
-- >>> verifyWebhook GitLab (Just "s3cret") (WebhookReq Nothing (Just ("v1," <> glSig)) Nothing (Just "d2") (Just "1700000000") payload)
-- Left "invalid signature"
-- >>> verifyWebhook GitLab (Just "s3cret") (glReq "v1,bm90LXJpZ2h0")
-- Left "invalid signature"
-- >>> verifyWebhook GitLab (Just "tok") (WebhookReq Nothing Nothing (Just "tok") Nothing Nothing payload)
-- Right ()
-- >>> verifyWebhook GitLab (Just "tok") (WebhookReq Nothing Nothing (Just "nope") Nothing Nothing payload)
-- Left "invalid signature"
--
-- A row with no secret cannot verify anything. That is only reachable for GitHub rows made
-- before secrets were required, and the caller logs it:
--
-- >>> verifyWebhook GitHub Nothing (WebhookReq Nothing Nothing Nothing Nothing Nothing payload)
-- Right ()
verifyWebhook :: GitHost -> Maybe Text -> WebhookReq -> Either Text ()
verifyWebhook _ Nothing _ = Right ()
verifyWebhook h (Just secret) req = case h of
  GitHub -> hmacOf "sha256="
  Bitbucket -> hmacOf "sha256="
  Gitea -> hmacOf ""
  GitLab -> gitlab
  where
    matches a b = BA.constEq (encodeUtf8 @Text @ByteString a) (encodeUtf8 @Text @ByteString b)
    eq a b = unless (matches a b) $ Left "invalid signature"

    hmacOf prefix = do
      sig <- maybeToRight "signature required but not provided" req.signature
      eq sig (prefix <> hmacHex secret req.body)

    -- The signed message is `id.timestamp.body`, so a delivery replayed later fails on the
    -- timestamp even with a valid signature.
    gitlab = case req.signature of
      Just sigHeader -> do
        wid <- maybeToRight "webhook-id required but not provided" req.gitlabWebhookId
        ts <- maybeToRight "webhook-timestamp required but not provided" req.gitlabTimestamp
        hmacKey <- gitlabKey secret
        let signed = encodeUtf8 (wid <> "." <> ts <> ".") <> req.body
            expected = extractBase64 $ B64.encodeBase64 $ BA.convert (HMAC.hmac hmacKey signed :: HMAC.HMAC SHA256)
            candidates = mapMaybe (T.stripPrefix "v1,") $ T.splitOn " " sigHeader
        unless (any (matches expected) candidates) $ Left "invalid signature"
      -- Pre-19.1 self-hosted GitLab has only the plaintext token.
      Nothing -> eq (fromMaybe "" req.gitlabToken) secret


-- | GitLab signing keys are handed out as @whsec_<base64>@; the HMAC key is the decoded bytes.
gitlabKey :: Text -> Either Text ByteString
gitlabKey secret = case T.stripPrefix "whsec_" secret of
  Nothing -> Right $ encodeUtf8 secret
  Just b64 -> first (const "webhook secret is not valid base64") $ B64.decodeBase64Untyped (encodeUtf8 b64)


-- | HMAC-SHA256 of a body under a secret, hex-encoded — the shape GitHub, Gitea and Bitbucket
-- all use.
hmacHex :: Text -> ByteString -> Text
hmacHex secret payload = decodeUtf8 $ B16.encode $ BA.convert (HMAC.hmac (encodeUtf8 secret :: ByteString) payload :: HMAC.HMAC SHA256)
