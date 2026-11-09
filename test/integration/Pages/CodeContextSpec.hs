-- | Repositories end to end: git-host accounts and connections, source context (code mappings
-- and frame source), the project wiki, deployment evidence, the configuration folder, and
-- dashboard sync — webhooks, pull/push/export jobs, their leases, and the migrations under them.
module Pages.CodeContextSpec (spec) where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AET
import Data.Base64.Types (extractBase64)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Cache qualified as Cache
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Wreq qualified as W
import Data.Map.Strict qualified as M
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Types (Query (..))
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpose, interpret)
import Effectful.Error.Static (catchError)
import Hasql.Interpolate qualified as HI
import Models.Apis.MonitorDefinitions qualified as MonitorDefinitions
import Models.Projects.CodeContext qualified as CodeContext
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ImpactReviews qualified as ImpactReviews
import Models.Projects.ProjectApiKeys qualified as Keys
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.ProjectMemory qualified as Memory
import Models.Projects.Projects qualified as Projects
import Models.Projects.RepositorySync qualified as RepositorySync
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), defaultRequest)
import Network.HTTP.Client.Internal (Response (..))
import Network.HTTP.Types qualified as H
import Network.HTTP.Types.URI (urlEncode)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Test qualified as WT
import OddJobs.ConfigBuilder (mkConfig)
import OddJobs.Job qualified as Jobs
import Pages.AIThreads qualified as AIThreads
import Pages.BodyWrapper (PageCtx (..))
import Pages.CodeContext qualified as PageCodeContext
import Pages.Components (stackTrace_)
import Pages.Dashboards qualified as DashboardPage
import Pages.GitSync qualified as GitSyncPage
import Pkg.Git qualified as Git
import Pkg.TestUtils
import Servant (ServerError (..), getHeaders, getResponse)
import Servant.Server.Generic (genericServeTWithContext)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Process.Typed (proc, readProcessStdout_, setEnv)
import System.Timeout (timeout)
import System.Types (ATBackgroundCtx, Done (..))
import UnliftIO.Async (cancel, concurrently, wait, waitCatch, withAsync)
import UnliftIO.Exception (bracket, throwIO, try)
import Web.Routes qualified as Routes
import "cryptonite" Crypto.Hash.Algorithms (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC


encKeyOf :: TestResources -> ByteString
encKeyOf tr = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey


-- | A dashboard repository on GitHub tracking @main@ without a webhook secret.
syncOf :: TestResources -> Projects.ProjectId -> Maybe Text -> Text -> Text -> GitSync.GitCreds -> Text -> IO GitSync.GitHubSync
syncOf tr pid origin owner repo creds prefix =
  runQueryEffect tr (GitSync.insertGitHubSync (encKeyOf tr) pid Git.GitHub origin owner repo "main" creds Nothing prefix) >>= maybe (fail "repository connection refused") pure


-- | The token-backed @team/dashboards@ connection the sync job specs run against.
teamDashboards :: TestResources -> IO GitSync.GitHubSync
teamDashboards tr = syncOf tr testPid Nothing "team" "dashboards" (GitSync.PersonalToken "fixture") ""


grantOf :: TestResources -> Projects.ProjectId -> Git.GitHost -> Maybe Text -> Text -> Maybe Int64 -> Maybe Text -> IO GitSync.GitHubCredential
grantOf tr pid host origin account inst token =
  runQueryEffect tr (GitSync.upsertGitHubCredential (encKeyOf tr) pid host origin account inst token) >>= maybe (fail "credential refused") pure


-- | A github.com app-installation account (installation 42) on 'testPid'.
appAccount :: TestResources -> Text -> IO GitSync.GitHubCredential
appAccount tr account = grantOf tr testPid Git.GitHub Nothing account (Just 42) Nothing


syncById :: TestResources -> GitSync.GitHubSync -> IO GitSync.GitHubSync
syncById tr sync = runQueryEffect tr (GitSync.getGitSyncById testPid sync.id) >>= maybe (fail "repository connection missing") pure


onlySync :: TestResources -> IO (Maybe GitSync.GitHubSync)
onlySync tr = runQueryEffect tr (GitSync.getGitSyncs testPid) <&> \case [sync] -> Just sync; _ -> Nothing


seedDash :: TestResources -> (Dashboards.DashboardVM -> Dashboards.DashboardVM) -> IO Dashboards.DashboardId
seedDash tr edit = do
  did <- UUIDId <$> UUID.nextRandom
  did <$ runQueryEffect tr (Dashboards.insert $ edit $ Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil))


-- | A dashboard @sync@ owns at @path@, with a schema to export.
ownedDash :: TestResources -> GitSync.GitHubSync -> Text -> Text -> IO Dashboards.DashboardId
ownedDash tr sync title path = seedDash tr \d -> d{Dashboards.title = title, Dashboards.schema = Just def, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just path}


dashOf :: TestResources -> Dashboards.DashboardId -> IO (Maybe Dashboards.DashboardVM)
dashOf tr did = runQueryEffect tr $ Dashboards.getDashboardByProjectId testPid did


allDashboards :: TestResources -> IO [Dashboards.DashboardVM]
allDashboards tr = runQueryEffect tr $ Dashboards.selectDashboardsSortedBy testPid Dashboards.Asc Dashboards.DSTitle


pendingJobs :: TestResources -> IO [BackgroundJobs.BgJobs]
pendingJobs tr = map snd . V.toList <$> getPendingBackgroundJobs tr.trATCtx


-- | Run one background job; @host@ answers for the git host and every request is returned.
runJob :: TestResources -> (ATBackgroundCtx () -> ATBackgroundCtx ()) -> BackgroundJobs.BgJobs -> IO [(Text, LBS.ByteString)]
runJob tr host job = fst <$> runTestBgRecordingHTTP frozenTime tr (host $ BackgroundJobs.processBackgroundJob tr.trATCtx job)


-- | A fake git host: the first route whose fragment the URL contains answers, else @fallback@.
gitHost :: (IOE :> es, W.HTTP :> es) => [(Text, LBS.ByteString)] -> LBS.ByteString -> Eff es a -> Eff es a
gitHost routes fallback = withHTTPResponses \_ endpoint -> pure $ Just $ maybe fallback snd $ find ((`T.isInfixOf` toText endpoint) . fst) routes


treeOf :: [(Text, Text)] -> LBS.ByteString
treeOf entries = AE.encode $ AE.object ["tree" AE..= [AE.object ["path" AE..= path, "sha" AE..= sha, "type" AE..= ("blob" :: Text)] | (path, sha) <- entries]]


-- | A contents-API answer carrying @body@.
blobOf :: Text -> LBS.ByteString
blobOf body = AE.encode $ AE.object ["content" AE..= extractBase64 (B64.encodeBase64 $ encodeUtf8 body)]


withUnavailableRepositoryAPI :: (IOE :> es, W.HTTP :> es) => Eff es a -> Eff es a
withUnavailableRepositoryAPI = interpose @W.HTTP \_ _ -> liftIO $ throwIO $ InvalidUrlException "https://git.invalid" "Fixture account is unavailable"


-- | Answer every GET with @body@ (checking the URL when one is expected); any write fails.
getOnly :: (IOE :> es, W.HTTP :> es) => Maybe String -> LBS.ByteString -> Eff es a -> Eff es a
getOnly expected body = interpose @W.HTTP \_ -> \case
  W.GetWith _ endpoint -> liftIO (traverse_ (endpoint `shouldBe`) expected) $> mockResponse body
  _ -> liftIO $ throwIO $ InvalidUrlException "https://git.invalid" "Only repository reads are expected here"


-- | Run one "Pkg.Git" operation against canned vendor replies, returning its result and each
-- request's method and URL. A route answers when every fragment is in the URL, a trailing @$@
-- anchoring it to the end: @page=1@ is in @page=11@, and a folder prefixes its files.
onHost :: Git.GitHost -> Maybe Text -> [([Text], LBS.ByteString)] -> (Git.GitConn -> Eff '[W.HTTP, IOE] a) -> IO (a, [(Text, Text)])
onHost host origin routes act = do
  seen <- newIORef []
  conn <- either (fail . show) pure $ Git.mkGitConn host origin "tok"
  let matches url fragment = maybe (fragment `T.isInfixOf` url) (`T.isSuffixOf` url) (T.stripSuffix "$" fragment)
      reply method endpoint = liftIO do
        modifyIORef' seen ((method, toText endpoint) :)
        pure $ mockResponse $ maybe "{}" snd $ find (all (matches $ toText endpoint) . fst) routes
  result <-
    runEff $ act conn & interpret \_ -> \case
      W.Get endpoint -> reply "GET" endpoint
      W.GetWith _ endpoint -> reply "GET" endpoint
      W.Post endpoint _ -> reply "POST" endpoint
      W.PostWith _ endpoint _ -> reply "POST" endpoint
      W.Put endpoint _ -> reply "PUT" endpoint
      W.PutWith _ endpoint _ -> reply "PUT" endpoint
      W.Patch endpoint _ -> reply "PATCH" endpoint
      W.PatchWith _ endpoint _ -> reply "PATCH" endpoint
      W.Delete endpoint -> reply "DELETE" endpoint
      W.DeleteWith _ endpoint -> reply "DELETE" endpoint
  (result,) . reverse <$> readIORef seen


configurationProvider :: TestResources -> Text -> ATBackgroundCtx a -> ATBackgroundCtx a
configurationProvider tr prState = withHTTPResponses \_ endpoint -> do
  syncs <- runQueryEffect tr $ GitSync.getGitSyncs testPid
  let path = toText endpoint
      connected = find (\sync -> ("/repos/" <> sync.owner <> "/" <> sync.repo <> "/") `T.isInfixOf` path) syncs
      hasProposal = isJust $ connected >>= (.pullRequestNumber)
      response
        | "/matching-refs/" `T.isInfixOf` path = if hasProposal then "[{\"ref\":\"refs/heads/monoscope-dashboards\",\"object\":{\"sha\":\"proposal-head\"}}]" else "[]"
        | "/pulls/" `T.isInfixOf` path = AE.encode $ AE.object ["number" AE..= (17 :: Int), "state" AE..= prState, "merged" AE..= False, "html_url" AE..= ("https://github.com/team/dashboards/pull/17" :: Text)]
        | "/pulls" `T.isSuffixOf` path = "{\"number\":17}"
        | "/git/trees/" `T.isInfixOf` path = "{\"tree\":[]}"
        | "/commits/main" `T.isInfixOf` path = "{\"sha\":\"head\",\"commit\":{\"tree\":{\"sha\":\"target-tree\"}}}"
        | otherwise = "{\"sha\":\"proposal-head\"}"
  pure $ Just response


-- | Replace the project's one sync row with a fresh connection on @host@, so a webhook has a row to resolve.
setupSyncOn :: TestResources -> Git.GitHost -> Text -> Text -> Maybe Text -> IO ()
setupSyncOn tr host owner repo secret = do
  void $ testServant tr $ GitSyncPage.gitSyncSettingsDeleteH testPid
  void $ call tr $ GitSyncPage.gitSyncSettingsPostH testPid
    GitSyncPage.GitSyncForm
      { host = Just host
      , apiBase = if host == Git.Gitea then Just "https://git.example.com" else Nothing
      , owner = owner
      , repo = repo
      , branch = "main"
      , accessToken = "tok"
      , webhookSecret = secret
      , pathPrefix = Nothing
      }


-- | HMAC-SHA256 hex of a body, the digest three of the four hosts send.
hmacHexOf :: Text -> ByteString -> Text
hmacHexOf secret body = decodeUtf8 $ B16.encode $ BA.convert (HMAC.hmac (encodeUtf8 secret :: ByteString) body :: HMAC.HMAC SHA256)


-- | The whole routed app, so wiki requests go through auth, routing and forms like a browser's.
wikiApp :: TestResources -> Wai.Application
wikiApp tr =
  genericServeTWithContext
    (effToServantHandlerTest tr.trTestClock tr.trUUIDRef tr.trATCtx tr.trLogger tr.trTracerProvider)
    (Routes.server (\_ _ _ -> pure $ Left "unused") (\_ _ _ -> pure $ Left "unused"))
    (Routes.genAuthServerContext tr.trLogger tr.trATCtx)


isImport :: BackgroundJobs.BgJobs -> Bool
isImport = \case BackgroundJobs.GitSyncFromRepo{} -> True; BackgroundJobs.GitSyncRepository{} -> True; _ -> False


spec :: Spec
spec = do
  -- Regression: the revision a span reports lives in its /resource/, not its attributes. Every
  -- frame silently rendered today's source while someone debugged an old release; the doctests
  -- pass 'revisionFor' a hand-built lookup, so only the wiring could be wrong. The needle omits
  -- the leading @&@: Lucid escapes it inside an attribute, so "&revision=" would never fire.
  it "stackFrameSourceUrl_carriesOnlyTheResourceShaRevision" do
    let pyStack = "Traceback (most recent call last):\n  File \"/srv/app/checkout.py\", line 88, in charge\n    raise ValueError(x)"
        lookupOf kvs k = listToMaybe [v | (k', v) <- kvs, k' == k]
        urlFor spanAttrs resourceAttrs = renderT $ stackTrace_ testPid (Just "checkout") (lookupOf spanAttrs) (lookupOf resourceAttrs) pyStack
        version = [("service.version", "1c0ffee")] :: [(Text, Text)]
    urlFor [] version `shouldSatisfy` T.isInfixOf "revision=1c0ffee"
    for_ [(version, []), ([], [("service.version", "v2.3.1")] :: [(Text, Text)])] \(spanAttrs, resourceAttrs) -> urlFor spanAttrs resourceAttrs `shouldNotSatisfy` T.isInfixOf "revision="

  it "dashboardFiles_planAgainstThePrefixAndRoundTripYaml" do
    let entry path sha = GitSync.TreeEntry path True (Just sha) Nothing
        known = M.fromList . map (\(path, sha) -> (path, (UUIDId UUID.nil, Just sha)))
        shape = map \case
          GitSync.SyncCreate p _ -> ("create", p)
          GitSync.SyncUpdate p _ _ -> ("update", p)
          GitSync.SyncDelete p _ -> ("delete", p)
          GitSync.SyncRename p _ _ -> ("rename", p)
          GitSync.SyncConflict p _ -> ("conflict" :: Text, p)
    for_
      ( [ ("dashboards/", [entry "dashboards/new.yaml" "sha1", entry "dashboards/other.yaml" "sha2"], [], [("create", "dashboards/new.yaml"), ("create", "dashboards/other.yaml")])
        , ("dashboards/", [entry "dashboards/x.yaml" "newsha"], [("x.yaml", "oldsha")], [("update", "dashboards/x.yaml")])
        , ("dashboards/", [], [("gone.yaml", "sha")], [("delete", "gone.yaml")])
        , ("dashboards/", [entry "src/main.hs" "s1", entry "dashboards/ok.yaml" "s2"], [], [("create", "dashboards/ok.yaml")])
        , ("dashboards/", [entry "dashboards/test.yml" "sha"], [], [("create", "dashboards/test.yml")])
        , ("app/dashboards/", [entry "app/dashboards/x.yaml" "sha", entry "dashboards/y.yaml" "sha2"], [], [("create", "app/dashboards/x.yaml")])
        ] ::
          [(Text, [GitSync.TreeEntry], [(Text, Text)], [(Text, Text)])]
      )
      \(prefix, entries, db, expected) -> shape (GitSync.buildSyncPlan prefix entries (known db)) `shouldBe` expected
    (.title) <$> GitSync.yamlToDashboard "title: Minimal\nwidgets: []\n" `shouldBe` Right (Just "Minimal")
    ((,) <$> (.title) <*> (.tags)) <$> GitSync.yamlToDashboard "title: Test\ntags:\n  - prod\nwidgets: []\n" `shouldBe` Right (Just "Test", Just ["prod"])
    (.title) <$> GitSync.yamlToDashboard (GitSync.dashboardToYaml (def :: Dashboards.Dashboard){Dashboards.title = Just "My Dash", Dashboards.tags = Just (["a", "b"] :: [Text])}) `shouldBe` Right (Just "My Dash")
    map (\prefix -> GitSync.getDashboardsPath def{GitSync.pathPrefix = prefix}) ["", "myapp"] `shouldBe` ["dashboards/", "myapp/dashboards/"]

  -- "Pkg.Git" against each vendor: the URL every host is asked for, and what we make of its answer.
  -- Fixtures were captured from the live public APIs on 2026-08-12 and trimmed to the decoded
  -- fields (refresh by curling e.g. api.github.com/repos/octocat/Hello-World/git/trees/master).
  describe "Git host contracts" do
    let acme = Git.RepoRef "acme" "api" "main"
        gitea = Just "https://git.acme.dev"
        ghCommit = "{\"sha\":\"c1\"}"
        commitOf = \case Git.GitLab -> "[{\"id\":\"c1\",\"short_id\":\"c1\",\"title\":\"Add latest changes\"}]"; Git.Bitbucket -> "{\"values\":[{\"type\":\"commit\",\"hash\":\"c1\"}]}"; _ -> ghCommit
        bbTree = "{\"values\":[{\"path\":\"dashboards/a.yaml\",\"type\":\"commit_file\",\"size\":49}]}"
        repos n = AE.encode $ AE.object ["repositories" AE..= replicate n (AE.object ["full_name" AE..= ("acme/r" :: Text), "default_branch" AE..= ("main" :: Text)])]
    it "reads files and pinned trees, and writes, in each host's own dialect" do
      for_
        ( [ (Git.GitHub, Nothing, "https://api.github.com/repos/acme/api/contents/dashboards/a.yaml?ref=main")
          , (Git.Gitea, gitea, "https://git.acme.dev/api/v1/repos/acme/api/contents/dashboards/a.yaml?ref=main")
          , (Git.GitLab, Nothing, "https://gitlab.com/api/v4/projects/acme%2Fapi/repository/files/dashboards%2Fa.yaml/raw?ref=main")
          , (Git.Bitbucket, Nothing, "https://api.bitbucket.org/2.0/repositories/acme/api/src/main/dashboards/a.yaml")
          ] ::
            [(Git.GitHost, Maybe Text, Text)]
        )
        \(host, origin, url) -> onHost host origin [(["contents"], "{\"content\":\"aGVsbG8K\"}"), (["/raw"], "hello\n"), (["/src/"], "hello\n")] (\c -> Git.fetchFile c acme "dashboards/a.yaml") `shouldReturn` (Right "hello\n", [("GET", url)])
      -- A tree is listed at the commit the branch resolved to. GitLab names the blob hash @id@, Gitea
      -- adds @mode@, and Bitbucket names none, so it is hashed from content under the prefix only.
      for_
        ( [ (Git.GitHub, Nothing, "git/trees", "{\"sha\":\"7fd1a60b\",\"truncated\":false,\"tree\":[{\"path\":\"dashboards/a.yaml\",\"type\":\"blob\",\"sha\":\"b1\",\"size\":13}]}", "b1")
          , (Git.Gitea, gitea, "git/trees", "{\"sha\":\"075d94cc\",\"tree\":[{\"path\":\"dashboards/a.yaml\",\"mode\":\"100755\",\"type\":\"blob\",\"size\":918,\"sha\":\"b1\"}]}", "b1")
          , (Git.GitLab, Nothing, "repository/tree", "[{\"id\":\"b1\",\"name\":\"a.yaml\",\"type\":\"blob\",\"path\":\"dashboards/a.yaml\",\"mode\":\"100644\"}]", "b1")
          , (Git.Bitbucket, Nothing, "max_depth", bbTree, Git.computeContentSha "hello\n")
          ] ::
            [(Git.GitHost, Maybe Text, Text, LBS.ByteString, Text)]
        )
        \(host, origin, treeRoute, tree, sha) -> do
          (result, requests) <- onHost host origin [(["commits"], commitOf host), ([treeRoute], tree), (["/src/c1/dashboards/a.yaml$"], "hello\n")] \c -> Git.fetchTree c acme "dashboards/"
          second (map (.sha)) <$> result `shouldBe` Right ("c1", [Just sha])
          let urls = map snd requests
          length urls `shouldBe` if host == Git.Bitbucket then 3 else 2
          take 1 urls `shouldSatisfy` all (T.isInfixOf "main")
          drop 1 urls `shouldSatisfy` all (T.isInfixOf "c1")
      -- An unscoped listing would cost a read per file in the repository, so its shas stay unknown.
      (unscoped, unscopedRequests) <- onHost Git.Bitbucket Nothing [(["max_depth"], bbTree), (["/commits/"], commitOf Git.Bitbucket)] \c -> Git.fetchTree c acme ""
      second (map (.sha)) <$> unscoped `shouldBe` Right ("c1", [Nothing])
      map snd unscopedRequests `shouldNotSatisfy` any (T.isInfixOf "a.yaml")
      -- GitLab creates with POST and updates with PUT; a host answering a write without a sha gets git's own.
      for_ ([(Git.GitLab, Nothing, "POST", Git.computeContentSha "hello\n"), (Git.GitLab, Just "b1", "PUT", Git.computeContentSha "hello\n"), (Git.Bitbucket, Nothing, "POST", Git.computeContentSha "hello\n"), (Git.GitHub, Nothing, "PUT", "server-sha")] :: [(Git.GitHost, Maybe Text, Text, Text)]) \(host, existing, method, sha) -> do
        (written, requests) <- onHost host Nothing [(["repository/files"], "{}"), (["contents"], "{\"content\":{\"sha\":\"server-sha\"}}"), (["commits"], commitOf host), (["/src"], "")] \c -> Git.pushFile c acme "dashboards/a.yaml" "hello\n" existing "msg"
        fst <$> written `shouldBe` Right sha
        map fst requests `shouldContain` [method]

    -- An empty or partial listing reads as "delete every dashboard" on the sync path, so every failure
    -- is an error. Canned replies never throw, so statuses are checked through 'Git.formatHttpError'.
    it "pages fully, and reports truncated, failed and malformed replies as errors" do
      fmap length . fst <$> onHost Git.GitHub Nothing [(["page=1$"], repos 100), (["page=2$"], repos 1)] Git.listRepos `shouldReturn` Right 101
      fst <$> onHost Git.GitHub Nothing [(["installation/repositories"], repos 100)] Git.listRepos >>= either (`shouldSatisfy` T.isInfixOf "refusing to continue with a partial list") (const $ expectationFailure "expected a page-limit error")
      fst <$> onHost Git.GitHub Nothing [(["git/trees"], "{\"sha\":\"t1\",\"truncated\":true,\"tree\":[]}"), (["/commits/"], ghCommit)] (\c -> Git.fetchTree c acme "dashboards/") `shouldReturn` Left "Repository listing was truncated by the host; it is too large to sync."
      for_ [Git.GitHub, Git.GitLab, Git.Bitbucket] \host -> fst <$> onHost host Nothing [] (\c -> Git.fetchTree c acme "dashboards/") >>= (`shouldSatisfy` isLeft)
      fst <$> onHost Git.GitHub Nothing [(["contents"], "not json at all")] (\c -> Git.fetchFile c acme "a.yaml") >>= (`shouldSatisfy` isLeft)
      -- A default branch is a nicety: connecting falls back to main rather than failing.
      fst <$> onHost Git.GitHub Nothing [] (`Git.defaultBranchOf` acme) `shouldReturn` "main"
      fst <$> onHost Git.Bitbucket Nothing [(["repositories/acme/api"], "{\"mainbranch\":{\"name\":\"trunk\"}}")] (`Git.defaultBranchOf` acme) `shouldReturn` "trunk"
      -- GitHub rate-limits as 403; a timeout or a bad URL has no status to name.
      map (\code -> Git.formatHttpError $ HttpExceptionRequest defaultRequest $ StatusCodeException ((mockResponse "" $> ()){responseStatus = H.mkStatus code ""}) "") [429, 403, 404, 500] `shouldBe` ["HTTP 429", "HTTP 403", "HTTP 404", "HTTP 500"]
      Git.formatHttpError (HttpExceptionRequest defaultRequest ResponseTimeout) `shouldSatisfy` T.isInfixOf "request failed"
      Git.formatHttpError (InvalidUrlException "http://[" "bad host") `shouldSatisfy` T.isInfixOf "invalid URL"

  -- Opt-in, never in CI: proves our URLs and decoders against a live vendor, which canned replies
  -- cannot. It writes monoscope-smoke/probe.yaml, so point it at a scratch repository. Set
  -- GIT_SMOKE_HOST (github|gitlab|bitbucket|gitea), GIT_SMOKE_TOKEN, GIT_SMOKE_OWNER, GIT_SMOKE_REPO,
  -- and optionally GIT_SMOKE_ORIGIN (required for Gitea) and GIT_SMOKE_BRANCH (default main).
  smoke <-
    runIO $ traverse (fmap (fmap toText) . lookupEnv) ["GIT_SMOKE_HOST", "GIT_SMOKE_TOKEN", "GIT_SMOKE_OWNER", "GIT_SMOKE_REPO", "GIT_SMOKE_ORIGIN", "GIT_SMOKE_BRANCH"] <&> \case
      [Just host, Just token, Just owner, Just repo, origin, branch] -> (,Git.RepoRef owner repo (fromMaybe "main" branch)) <$> (Git.parseHostSlug host >>= \h -> rightToMaybe (Git.mkGitConn h origin token))
      _ -> Nothing
  it "live git host: lists repositories, names a default branch, and round-trips a write at the sha it reported" case smoke of
    Nothing -> pendingWith "set GIT_SMOKE_HOST, GIT_SMOKE_TOKEN, GIT_SMOKE_OWNER and GIT_SMOKE_REPO to run"
    Just (conn, ref) -> do
      let live = runEff . W.runHTTPWreq
          path = "monoscope-smoke/probe.yaml"
      live (Git.listTokenRepos conn) >>= (`shouldSatisfy` isRight)
      live (Git.defaultBranchOf conn ref) >>= (`shouldSatisfy` not . T.null)
      (sha, _) <- live (Git.pushFile conn ref path "title: monoscope smoke probe\n" Nothing "monoscope smoke test") >>= either (fail . toString) pure
      -- A sha a fresh listing does not report is a phantom change on the next pull.
      (_, entries) <- live (Git.fetchTree conn ref "monoscope-smoke/") >>= either (fail . toString) pure
      ((.sha) <$> find ((== path) . (.path)) entries) `shouldBe` Just (Just sha)
      live (Git.fetchFile conn ref path) `shouldReturn` Right "title: monoscope smoke probe\n"

  around withTestResources do
    describe "Repository connections" do
      it "githubAppInstall_bindsOnlyAVerifiedSessionAttemptForTheAccountOwner" \tr -> do
        let configured = tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppId = "123", githubClientId = "fixture-client", githubClientSecret = "fixture-secret"}}}
            session = (getResponse tr.trSessAndHeader).persistentSession.id
            provider :: (IOE :> es, W.HTTP :> es) => Eff es a -> Eff es a
            provider = interpose @W.HTTP \_ -> \case
              W.PostWith _ "https://github.com/login/oauth/access_token" _ -> pure $ mockResponse "{\"access_token\":\"private-user-token\"}"
              W.GetWith _ "https://api.github.com/user/installations?per_page=100&page=1" -> pure $ mockResponse "{\"installations\":[{\"id\":42,\"app_id\":123,\"account\":{\"login\":\"team\",\"type\":\"User\"}}]}"
              W.GetWith _ "https://api.github.com/user" -> pure $ mockResponse "{\"login\":\"team\"}"
              _ -> liftIO $ expectationFailure "Unexpected authorization request" >> throwIO (InvalidUrlException "https://git.invalid" "Unexpected authorization request")
        -- The callback binds only the session's own attempt, never a forged state or a replayed nonce.
        forged <- call configured $ withUnavailableRepositoryAPI $ GitSyncPage.githubAppCallbackH (Just 42) Nothing (Just $ testPid.toText <> ":code") Nothing
        renderT forged `shouldSatisfy` T.isInfixOf "missing, expired, or already used"
        runQueryEffect tr (GitSync.getGitHubCredentials testPid) >>= (`shouldSatisfy` null)
        (installState, authorizePage) <- runAuthHandler configured do
          initialState <- GitSync.createInstallationAttempt session (GitSync.InstallationAttempt testPid GitSync.InstallCode GitSync.InstallApp)
          response <- withUnavailableRepositoryAPI $ GitSyncPage.githubAppCallbackH (Just 42) Nothing (Just $ UUID.toText initialState) Nothing
          pure (initialState, getResponse response)
        renderT authorizePage `shouldSatisfy` T.isInfixOf "https://github.com/login/oauth/authorize?client_id=fixture-client"
        runQueryEffect tr (GitSync.consumeInstallationAttempt session installState) `shouldReturn` Nothing
        Just (HI.OneColumn nonce) <- runQueryEffect tr $ Hasql.interpOne @(HI.OneColumn UUID.UUID) [HI.sql| SELECT id FROM projects.github_installation_attempts WHERE project_id = #{testPid} AND session_id = #{session} AND installation_id = 42 |]
        nonce `shouldNotBe` installState
        verified <- renderT <$> call configured (provider $ GitSyncPage.githubAppCallbackH (Just 999999) Nothing (Just $ UUID.toText nonce) (Just "private-code"))
        verified `shouldSatisfy` T.isInfixOf ("/p/" <> testPid.toText <> "/repositories/connect")
        for_ ["private-user-token", "private-code", "fixture-secret"] \secret -> verified `shouldNotSatisfy` T.isInfixOf secret
        [bound] <- runQueryEffect tr $ GitSync.getGitHubCredentials testPid
        bound.installationId `shouldBe` Just 42
        replay <- call configured $ withUnavailableRepositoryAPI $ GitSyncPage.githubAppCallbackH (Just 42) Nothing (Just $ UUID.toText nonce) (Just "private-code")
        renderT replay `shouldSatisfy` T.isInfixOf "missing, expired, or already used"
        runQueryEffect tr (GitSync.getGitSyncs testPid) >>= (`shouldSatisfy` null)

        -- installationAuthorization_requiresTheGitHubAccountOwner: only the account itself, or an
        -- active org admin (found on any membership page), may bind an installation.
        owned <- createTestProject tr "Installation owner"
        let authorize installations memberships =
              runTestBgRecordingHTTP frozenTime tr
                $ withHTTPResponses (\_ endpoint -> pure $ Just $ if endpoint == "https://github.com/login/oauth/access_token" then "{\"access_token\":\"fixture-user-token\"}" else if endpoint == "https://api.github.com/user" then "{\"login\":\"team\"}" else if "https://api.github.com/user/memberships/orgs?" `T.isPrefixOf` toText endpoint then memberships else installations)
                $ GitSync.authorizeGitHubInstallation (encKeyOf tr) owned "123" "fixture-client" "fixture-secret" "fixture-code" "http://localhost:8081/github/callback" 42
        (_, denied) <- authorize "{\"installations\":[{\"id\":43,\"account\":{\"login\":\"foreign\"}}]}" "[]"
        denied `shouldBe` Left GitSync.InstallationOwnershipRequired
        runQueryEffect tr (GitSync.getGitHubCredentials owned) >>= (`shouldSatisfy` null)
        (_, collaborator) <- authorize "{\"installations\":[{\"id\":42,\"app_id\":123,\"account\":{\"login\":\"foreign\",\"type\":\"User\"}}]}" "[]"
        collaborator `shouldBe` Left GitSync.InstallationOwnershipRequired
        let organization = "{\"installations\":[{\"id\":42,\"app_id\":123,\"account\":{\"login\":\"team\",\"type\":\"Organization\"}}]}"
        for_ ([("member", "active", False), ("admin", "pending", False), ("admin", "active", True)] :: [(Text, Text, Bool)]) \(role, membershipState, allowed) -> do
          let memberships = AE.encode ([AE.object ["state" AE..= membershipState, "role" AE..= role, "organization" AE..= AE.object ["login" AE..= ("team" :: Text)]]] :: [AE.Value])
          (_, result) <- authorize organization memberships
          result `shouldBe` if allowed then Right () else Left GitSync.InstallationOwnershipRequired
        (requests, personal) <- authorize "{\"installations\":[{\"id\":42,\"app_id\":123,\"account\":{\"login\":\"team\",\"type\":\"User\"}}]}" "[]"
        personal `shouldBe` Right ()
        map fst requests `shouldBe` ["https://github.com/login/oauth/access_token", "https://api.github.com/user/installations?per_page=100&page=1", "https://api.github.com/user"]
        let firstPage = AE.encode $ replicate 100 $ AE.object ["state" AE..= ("active" :: Text), "role" AE..= ("member" :: Text)]
            ownerPage = "[{\"state\":\"active\",\"role\":\"admin\",\"organization\":{\"login\":\"Team\"}}]"
            paging _ endpoint = pure $ Just $ if endpoint == "https://github.com/login/oauth/access_token" then "{\"access_token\":\"fixture-user-token\"}" else if "user/installations?" `T.isInfixOf` toText endpoint then "{\"installations\":[{\"id\":42,\"app_id\":123,\"account\":{\"login\":\"Team\",\"type\":\"Organization\"}}]}" else if "page=2" `T.isSuffixOf` toText endpoint then ownerPage else firstPage
        (paged, admin) <- runTestBgRecordingHTTP frozenTime tr $ withHTTPResponses paging $ GitSync.authorizeGitHubInstallation (encKeyOf tr) owned "123" "fixture-client" "fixture-secret" "fixture-code" "https://monoscope.test/github/callback" 42
        admin `shouldBe` Right ()
        map fst paged `shouldSatisfy` elem "https://api.github.com/user/memberships/orgs?per_page=100&page=2"
        [account] <- runQueryEffect tr $ GitSync.getGitHubCredentials owned
        (account.account, account.installationId, account.accessToken) `shouldBe` ("team", Just 42, Nothing)
        runQueryEffect tr (GitSync.getGitSyncs owned) >>= (`shouldSatisfy` null)

        -- installationSelection_rejectsAnInstallationWithoutAProjectGrant
        let isolated = tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppPrivateKey = ""}}}
        status <- runAuthHandler isolated $ catchError @ServerError (GitSyncPage.githubAppSelectRepoH testPid (GitSyncPage.RepoSelectForm ["foreign/private"] "main" Nothing 424242) $> 200) (\_ err -> pure err.errHTTPCode)
        status `shouldBe` 403
        runQueryEffect tr (GitSync.getGitSyncs testPid) >>= (`shouldSatisfy` null)

        -- installationAttempt_isSessionBoundSingleUseAndExpires, and expired attempts are swept.
        let attempt = GitSync.InstallationAttempt testPid GitSync.InstallCode (GitSync.AuthorizeApp 42)
        fresh <- runTestBg frozenTime tr $ GitSync.createInstallationAttempt session attempt
        stranger <- Projects.PersistentSessionId <$> UUID.nextRandom
        runQueryEffect tr (GitSync.consumeInstallationAttempt stranger fresh) `shouldReturn` Nothing
        runQueryEffect tr (GitSync.consumeInstallationAttempt session fresh) `shouldReturn` Just attempt
        runQueryEffect tr (GitSync.consumeInstallationAttempt session fresh) `shouldReturn` Nothing
        expired <- runTestBg frozenTime tr $ GitSync.createInstallationAttempt session attempt
        runTestBg (addUTCTime 901 frozenTime) tr (GitSync.consumeInstallationAttempt session expired) `shouldReturn` Nothing
        void $ runTestBg (addUTCTime 901 frozenTime) tr $ GitSync.createInstallationAttempt session attempt
        dbQuery tr "SELECT EXISTS (SELECT 1 FROM projects.github_installation_attempts WHERE id = ?)" (Only expired) `shouldReturn` [Only False]

      -- Membership is checked per handler, not by the router, and none of these checked it: any
      -- signed-in user could connect, replace or disconnect another project's repository, bind a
      -- GitHub installation to it, or add and delete its mappings.
      it "repositories_nonMember_cannotMutate" \tr -> do
        victim <- createTestProject tr "victim"
        void $ syncOf tr victim Nothing "acme" "monoscope-config" (GitSync.PersonalToken "ghp_test") ""
        let mappingForm repo prefix = PageCodeContext.CodeMappingForm (Just repo) (Just "main") Nothing Nothing (Just prefix) (Just "") Nothing
            syncForm = GitSyncPage.GitSyncForm{host = Just Git.GitHub, apiBase = Nothing, owner = "attacker", repo = "evil", branch = "main", accessToken = "ghp_x", webhookSecret = Nothing, pathPrefix = Nothing}
            mappingIds = map (.id) <$> runQueryEffect tr (CodeContext.getCodeMappings victim)
            syncState = map (\s -> (s.owner, s.repo, s.installationId)) <$> runQueryEffect tr (GitSync.getGitSyncs victim)
        _ <- call tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsPostH victim (mappingForm "checkout-service" "/srv/app/")
        seeded <- mappingIds
        length seeded `shouldBe` 1
        syncs <- syncState
        assertNonMemberDenied tr victim $ GitSyncPage.gitSyncSettingsPostH victim syncForm
        assertNonMemberDenied tr victim $ GitSyncPage.gitSyncSettingsDeleteH victim
        assertNonMemberDenied tr victim $ GitSyncPage.githubAppSelectRepoH victim (GitSyncPage.RepoSelectForm ["attacker/evil"] "main" Nothing 42)
        assertNonMemberDenied tr victim $ GitSyncPage.githubAppInstallH victim Nothing
        assertNonMemberDenied tr victim $ PageCodeContext.codeMappingsPostH victim (mappingForm "attacker-repo" "/evil/")
        forM_ seeded $ assertNonMemberDenied tr victim . PageCodeContext.codeMappingsDeleteH victim
        -- @state@ is a session-bound nonce, so a project id in it is no grant at all.
        _ <- testServant tr $ GitSyncPage.githubAppCallbackH (Just 42) Nothing (Just victim.toText) Nothing
        syncState `shouldReturn` syncs
        mappingIds `shouldReturn` seeded

      it "repositoryTokenConnection_checksAccessOnEveryHostWithoutEnablingSyncAndRequiresExplicitReplacement" \tr -> do
        let form = PageCodeContext.RepositoryTokenForm Git.GitHub Nothing "team/checkout" "fixture-token" Nothing
            metadata = getOnly (Just "https://api.github.com/repos/team/checkout") "{\"full_name\":\"team/checkout\",\"private\":true,\"default_branch\":\"trunk\"}"
        rejected <- call tr $ withUnavailableRepositoryAPI $ PageCodeContext.repositoryTokenPostH testPid form
        rejected.content.connectionError `shouldSatisfy` isJust
        rejected.content.repoFullName `shouldBe` "team/checkout"
        renderT rejected.content `shouldNotSatisfy` T.isInfixOf "fixture-token"
        incomplete <- call tr $ getOnly Nothing "{\"full_name\":\"\"}" $ PageCodeContext.repositoryTokenPostH testPid form
        incomplete.content.connectionError `shouldSatisfy` isJust
        runQueryEffect tr (GitSync.getGitHubCredentials testPid) >>= (`shouldSatisfy` null)
        connections <- replicateM 2 $ testServant tr $ metadata $ PageCodeContext.repositoryTokenPostH testPid form
        [repository] <- runQueryEffect tr $ GitSync.getRepositories testPid
        for_ connections \(response, page) -> do
          page.content.connectionError `shouldSatisfy` isNothing
          getHeaders response `shouldSatisfy` elem ("HX-Redirect", encodeUtf8 ("/p/" <> testPid.toText <> "/repositories/" <> repository.id.toText))
        runQueryEffect tr (GitSync.getGitSyncs testPid) >>= (`shouldSatisfy` null)
        runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= (`shouldSatisfy` null)
        [account] <- runQueryEffect tr $ GitSync.getGitHubCredentials testPid
        let savedToken = runQueryEffect tr (GitSync.getGitHubCredential (encKeyOf tr) testPid account.id) <&> (>>= (.accessToken))
            concurrentSave existing = runQueryEffect tr $ GitSync.saveTokenCredential (encKeyOf tr) testPid Git.GitHub Nothing "team" "concurrent-token" existing
        savedToken `shouldReturn` Just "fixture-token"
        concurrentSave Nothing >>= (`shouldSatisfy` isNothing)
        savedToken `shouldReturn` Just "fixture-token"
        conflict <- call tr $ metadata $ PageCodeContext.repositoryTokenPostH testPid form{PageCodeContext.accessToken = "replacement-token"}
        conflict.content.connectionError `shouldSatisfy` isJust
        savedToken `shouldReturn` Just "fixture-token"
        _ <- testServant tr $ metadata $ PageCodeContext.repositoryTokenPostH testPid form{PageCodeContext.accessToken = "replacement-token", PageCodeContext.replaceToken = Just True}
        savedToken `shouldReturn` Just "replacement-token"
        concurrentSave (Just account) >>= (`shouldSatisfy` isNothing)
        savedToken `shouldReturn` Just "replacement-token"
        source <- call tr $ metadata $ PageCodeContext.repositorySourceGetH testPid repository.id Nothing Nothing
        renderT source `shouldSatisfy` T.isInfixOf "value=\"trunk\""
        picker <- call tr $ getOnly (Just "https://api.github.com/user/repos?per_page=100&page=1") "[{\"full_name\":\"team/checkout\",\"private\":true,\"default_branch\":\"trunk\"}]" $ PageCodeContext.repositoryConnectGetH testPid (Just account.id)
        picker.content.repositories `shouldBe` Right [Git.GitRepo "team/checkout" "checkout" True "trunk"]
        -- An app installation is never downgraded to a token, by a concurrent save or the form.
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql| UPDATE projects.git_credentials SET installation_id = 42, access_token = NULL WHERE id = #{account.id} |]
        concurrentSave (Just account) >>= (`shouldSatisfy` isNothing)
        appDenied <- call tr $ metadata $ PageCodeContext.repositoryTokenPostH testPid form{PageCodeContext.replaceToken = Just True}
        appDenied.content.connectionError `shouldSatisfy` isJust
        Just app <- runQueryEffect tr $ GitSync.getGitHubCredential (encKeyOf tr) testPid account.id
        app.installationId `shouldBe` Just 42
        -- GitLab namespaces, Gitea and Bitbucket read their own metadata shapes.
        for_
          ([(Git.GitLab, Nothing, "team/platform/checkout", "{\"path_with_namespace\":\"team/platform/checkout\",\"visibility\":\"private\",\"default_branch\":\"stable\"}"), (Git.Gitea, Just "https://git.example.com", "team/checkout", "{\"full_name\":\"team/checkout\",\"private\":true,\"default_branch\":\"stable\"}"), (Git.Bitbucket, Nothing, "team/checkout", "{\"full_name\":\"team/checkout\",\"is_private\":true,\"mainbranch\":{\"name\":\"stable\"}}")] :: [(Git.GitHost, Maybe Text, Text, LBS.ByteString)])
          \(host, origin, name, body) -> do
            _ <- testServant tr $ getOnly Nothing body $ PageCodeContext.repositoryTokenPostH testPid (PageCodeContext.RepositoryTokenForm host origin name "fixture-token" Nothing)
            hosted <- runQueryEffect tr (GitSync.getRepositories testPid) >>= maybe (fail "Expected repository for this Git host") pure . find ((== host) . (.host))
            (hosted.owner, hosted.repo) `shouldBe` Git.splitFullName name
            hostedSource <- call tr $ getOnly Nothing body $ PageCodeContext.repositorySourceGetH testPid hosted.id Nothing Nothing
            renderT hostedSource `shouldSatisfy` T.isInfixOf "value=\"stable\""
        runQueryEffect tr (GitSync.getGitSyncs testPid) >>= (`shouldSatisfy` null)

      it "repositorySettings_readOnlyMembersSeeMappingsWithoutEditControlsOrProviderRequests" \tr -> do
        account <- appAccount tr "team"
        runQueryEffect tr $ CodeContext.insertCodeMapping testPid account.id (Git.RepoRef "team" "checkout" "main") (Just "checkout") "/app/" "src"
        [repository] <- runQueryEffect tr $ GitSync.getRepositories testPid
        dbExec tr "UPDATE projects.project_members SET permission = 'view' WHERE project_id = ?" (Only testPid)
        details <- renderT . (.content) <$> call tr (PageCodeContext.repositoryGetH testPid repository.id)
        details `shouldSatisfy` T.isInfixOf "View source context"
        details `shouldNotSatisfy` T.isInfixOf "Configure source context"
        let noProvider = interpose @W.HTTP \_ _ -> liftIO $ expectationFailure "Read-only repository settings must not contact the provider" >> throwIO (InvalidUrlException "https://git.invalid" "Unexpected provider call")
        for_ [renderT <$> call tr (noProvider $ PageCodeContext.repositoriesGetH testPid (Just PageCodeContext.Overview) Nothing), renderT <$> call tr (noProvider $ PageCodeContext.repositorySourceGetH testPid repository.id Nothing Nothing), renderT <$> call tr (noProvider $ PageCodeContext.codeMappingsEditorGetH testPid (Just account.id) Nothing)] \rendered -> do
          html <- rendered
          html `shouldSatisfy` T.isInfixOf "team/checkout"
          html `shouldNotSatisfy` T.isInfixOf "name=\"credentialId\""
          for_ ["Unlink", "Link repository", "Save review settings", "Install GitHub App", "Connect with token"] \control -> html `shouldNotSatisfy` T.isInfixOf control

      it "credentialEncryption_usesFreshNoncesAuthenticatesCiphertextAndReadsLegacyTokens" \tr -> do
        let encKey = BS.replicate 32 7
            token = "same-provider-token"
            credential cid = runQueryEffect tr $ GitSync.getGitHubCredential encKey testPid cid
        Just alpha <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "alpha" Nothing (Just token)
        Just beta <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "beta" Nothing (Just token)
        alpha.accessToken `shouldNotBe` beta.accessToken
        for_ [alpha, beta] \c -> fmap (.accessToken) <$> credential c.id `shouldReturn` Just (Just token)
        encoded <- maybe (fail "Missing encrypted credential") pure alpha.accessToken
        body <- maybe (fail "Missing authenticated envelope") pure $ T.stripPrefix "v1:" encoded
        bytes <- either (fail . toString) pure $ B64.decodeBase64Untyped $ encodeUtf8 body
        let corrupt = "v1:" <> extractBase64 (B64.encodeBase64 $ BS.map (`xor` 1) bytes)
            legacy = extractBase64 $ B64.encodeBase64 $ Keys.encryptAPIKey encKey $ encodeUtf8 token
        for_ [(corrupt, alpha.id), (legacy, beta.id)] $ dbExec tr "UPDATE projects.git_credentials SET access_token = ? WHERE id = ?"
        credential alpha.id >>= (`shouldSatisfy` isNothing)
        fmap (.accessToken) <$> credential beta.id `shouldReturn` Just (Just token)
        runQueryEffect tr (GitSync.saveTokenCredential encKey testPid Git.GitHub Nothing "empty" "" Nothing) `shouldThrow` (== Keys.EmptySecret)

      it "repositoryConnections_foldGithubCasingAndKeepNestedNamespaces" \tr -> do
        account <- appAccount tr "team"
        sameAccount <- appAccount tr "TEAM"
        sameAccount.id `shouldBe` account.id
        for_ (["Team/Checkout", "team/checkout"] :: [Text]) \name -> do
          Cache.insert tr.trATCtx.repoListCache account.id [Git.GitRepo name "checkout" True "main"]
          void $ testServant tr $ PageCodeContext.repositoryConnectPostH testPid (Just account.id) (PageCodeContext.RepositoryConnectForm [name])
        map (\r -> (r.owner, r.repo)) <$> runQueryEffect tr (GitSync.getRepositories testPid) `shouldReturn` [("team", "checkout")]
        for_ ([("Team", "Checkout"), ("team", "checkout")] :: [(Text, Text)]) \(owner, repo) ->
          void $ call tr $ GitSyncPage.gitSyncSettingsPostH testPid GitSyncPage.GitSyncForm{host = Just Git.GitHub, apiBase = Nothing, owner, repo, branch = "main", accessToken = "fixture-token", webhookSecret = Just "fixture-secret", pathPrefix = Nothing}
        syncs <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        map (\s -> (s.owner, s.repo)) syncs `shouldBe` [("team", "checkout")]
        map (.id) <$> runQueryEffect tr (GitSync.getGitSyncsByRepo Git.GitHub "TEAM" "CHECKOUT") `shouldReturn` map (.id) syncs
        runQueryEffect tr $ CodeContext.insertCodeMapping testPid account.id (Git.RepoRef "TEAM" "CHECKOUT" "main") (Just "checkout") "/app/" "src"
        map (\m -> (m.owner, m.repo)) <$> runQueryEffect tr (CodeContext.getCodeMappings testPid) `shouldReturn` [("team", "checkout")]
        -- Only GitHub folds case; other hosts keep both spellings.
        gitlab <- grantOf tr testPid Git.GitLab Nothing "Team" Nothing (Just "fixture")
        for_ (["Team/Checkout", "team/checkout"] :: [Text]) \name -> void $ runQueryEffect tr $ GitSync.connectRepository testPid gitlab.id (Git.GitRepo name "checkout" True "main")
        allRepositories <- runQueryEffect tr $ GitSync.getRepositories testPid
        sort [(r.owner, r.repo) | r <- allRepositories, r.host == Git.GitLab] `shouldBe` [("Team", "Checkout"), ("team", "checkout")]
        -- A nested namespace is kept as the owner instead of being replaced by the account name.
        Cache.insert tr.trATCtx.repoListCache gitlab.id [Git.GitRepo "team/platform/checkout" "checkout" True "trunk"]
        renderT <$> call tr (PageCodeContext.codeMappingsEditorGetH testPid (Just gitlab.id) Nothing) >>= (`shouldSatisfy` T.isInfixOf "value=\"team/platform/checkout\"")
        _ <- call tr $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just "team/platform/checkout") (Just "trunk") (Just "checkout") Nothing Nothing Nothing (Just gitlab.id))
        map (\m -> (m.owner, m.repo)) <$> runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= (`shouldMatchList` [("team", "checkout"), ("team/platform", "checkout")])

      it "repositoryPicker_connectsCodebasesWithoutEnablingCapabilitiesAndRejectsUnavailableSelections" \tr -> do
        let repositories = [Git.GitRepo "team/checkout" "checkout" True "trunk", Git.GitRepo "team/catalog" "catalog" False "main"]
            connectForm = PageCodeContext.RepositoryConnectForm
        account <- appAccount tr "team"
        Cache.insert tr.trATCtx.repoListCache account.id repositories
        -- Listing through the host (a token exchange plus a listing) made a 0.2s page take 1.5s;
        -- with the host unavailable, a listing at all proves the per-account cache answered.
        picker <- call tr $ withUnavailableRepositoryAPI $ PageCodeContext.repositoryConnectGetH testPid (Just account.id)
        picker.content.repositories `shouldBe` Right repositories
        rejected <- call tr $ PageCodeContext.repositoryConnectPostH testPid (Just account.id) (connectForm ["team/checkout", "foreign/private"])
        rejected.content.selected `shouldBe` ["team/checkout", "foreign/private"]
        rejected.content.connectionError `shouldSatisfy` isJust
        runQueryEffect tr (GitSync.getRepositories testPid) >>= (`shouldSatisfy` null)
        replicateM_ 2 do
          (response, _) <- testServant tr $ PageCodeContext.repositoryConnectPostH testPid (Just account.id) (connectForm ["team/checkout", "team/catalog", "team/checkout"])
          getHeaders response `shouldSatisfy` elem ("HX-Redirect", encodeUtf8 ("/p/" <> testPid.toText <> "/repositories"))
        connected <- runQueryEffect tr $ GitSync.getRepositories testPid
        map (\r -> (r.repo, r.credentialId)) connected `shouldBe` [("catalog", Just account.id), ("checkout", Just account.id)]
        runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= (`shouldSatisfy` null)
        runQueryEffect tr (GitSync.getGitSyncs testPid) >>= (`shouldSatisfy` null)
        otherPid <- createTestProject tr "Other picker account"
        foreignAccount <- grantOf tr otherPid Git.GitHub Nothing "foreign" (Just 43) Nothing
        Cache.insert tr.trATCtx.repoListCache foreignAccount.id repositories
        runQueryEffect tr (GitSync.connectRepository testPid foreignAccount.id (Git.GitRepo "foreign/private" "private" True "main")) >>= (`shouldSatisfy` isNothing)
        denied <- call tr $ PageCodeContext.repositoryConnectPostH testPid (Just foreignAccount.id) (connectForm ["team/catalog"])
        denied.content.connectionError `shouldSatisfy` isJust
        runQueryEffect tr (GitSync.getRepositories otherPid) >>= (`shouldSatisfy` null)
        Cache.insert tr.trATCtx.repoListCache account.id []
        emptyPicker <- call tr $ PageCodeContext.repositoryConnectGetH testPid (Just account.id)
        emptyPicker.content.repositories `shouldBe` Right []
        renderT emptyPicker.content `shouldSatisfy` T.isInfixOf "No repositories are available"
        Cache.delete tr.trATCtx.repoListCache account.id
        failed <- call tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppPrivateKey = ""}}} $ PageCodeContext.repositoryConnectGetH testPid (Just account.id)
        failed.content.repositories `shouldSatisfy` isLeft
        for_ ["Could not load repositories", "Retry"] \label -> renderT failed.content `shouldSatisfy` T.isInfixOf label
        Cache.lookup tr.trATCtx.repoListCache account.id `shouldReturn` Nothing
        dbExec tr "DELETE FROM projects.git_credentials WHERE project_id = ? AND id = ?" (testPid, account.id)
        map (.credentialId) <$> runQueryEffect tr (GitSync.getRepositories testPid) `shouldReturn` [Nothing, Nothing]

      it "repositoryIdentity_keepsTheSameNamesOnDifferentServersIndependent" \tr -> do
        let origins = [Nothing, Just "https://git.example.com"] :: [Maybe Text]
            apiBases xs = sort (map (.apiBase) xs)
        for_ origins \origin -> grantOf tr testPid Git.GitHub origin "team" Nothing (Just "fixture")
        apiBases <$> runQueryEffect tr (GitSync.getGitHubCredentials testPid) `shouldReturn` origins
        -- repositoryConnection_keepsIdenticalNamesOnDifferentServersIndependent, from a fresh project.
        formed <- createTestProject tr "Same names on two servers"
        for_ origins $ call tr . GitSyncPage.gitSyncSettingsPostH formed . \origin -> GitSyncPage.GitSyncForm (Just Git.GitHub) origin "team" "dashboards" "main" "fixture" Nothing Nothing
        apiBases <$> runQueryEffect tr (GitSync.getGitSyncs formed) `shouldReturn` origins
        apiBases <$> runQueryEffect tr (GitSync.getGitHubCredentials formed) `shouldReturn` origins
        overview <- call tr $ PageCodeContext.repositoriesGetH formed Nothing Nothing
        T.count "team/dashboards" (renderT overview) `shouldBe` 2
        -- repositoryPicker_doesNotTreatAnEnterpriseRepositoryAsItsPublicGitHubCounterpart
        picked <- createTestProject tr "Enterprise picker"
        account <- grantOf tr picked Git.GitHub Nothing "team" (Just 42) Nothing
        Cache.insert tr.trATCtx.repoListCache account.id [Git.GitRepo "team/dashboards" "dashboards" True "main"]
        void $ syncOf tr picked (Just "https://git.example.com") "team" "dashboards" (GitSync.PersonalToken "fixture") ""
        void $ call tr $ GitSyncPage.githubAppSelectRepoH picked $ GitSyncPage.RepoSelectForm ["team/dashboards"] "main" Nothing 42
        apiBases <$> runQueryEffect tr (GitSync.getGitSyncs picked) `shouldReturn` origins

      it "repositoryConnection_reportsUnreadableTokensWithoutChangingTheFailedOperation" \tr -> do
        sync <- teamDashboards tr
        dbExec tr "UPDATE projects.git_sync SET access_token = 'invalid-base64' WHERE id = ?" (Only sync.id)
        runQueryEffect tr (GitSync.getGitSyncsDecrypted (encKeyOf tr) testPid) >>= (`shouldSatisfy` null)
        (.lastError) <$> syncById tr sync >>= (`shouldSatisfy` isJust)
        did <- UUIDId <$> UUID.nextRandom
        void $ runQueryEffect tr $ GitSync.recordSyncError sync.id (GitSync.ExportDashboard did) "Export failed"
        _ <- testServant tr $ PageCodeContext.repositoryConnectGetH testPid Nothing
        (fmap (.operation) . (.lastError)) <$> syncById tr sync `shouldReturn` Just (GitSync.ExportDashboard did)

      it "repositoryPicker_connectsSeveralRepositoriesAndPreservesExistingConfiguration" \tr -> do
        account <- grantOf tr testPid Git.GitHub Nothing "team-a" (Just 42) Nothing
        Cache.insert tr.trATCtx.repoListCache account.id [Git.GitRepo "team-a/dashboards" "dashboards" True "main", Git.GitRepo "team-b/dashboards" "dashboards" True "main"]
        let selection = GitSyncPage.RepoSelectForm ["team-a/dashboards", "team-b/dashboards", "team-a/dashboards"] "trunk" (Just "observability") 42
        void $ call tr $ GitSyncPage.githubAppSelectRepoH testPid selection
        syncs <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        sort [(s.owner, s.repo, s.branch, s.pathPrefix) | s <- syncs] `shouldBe` [("team-a", "dashboards", "trunk", "observability"), ("team-b", "dashboards", "trunk", "observability")]
        void $ call tr $ GitSyncPage.githubAppSelectRepoH testPid selection{GitSyncPage.branch = "different", GitSyncPage.pathPrefix = Just "other"}
        unchanged <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        sort [(s.id, s.branch, s.pathPrefix) | s <- unchanged] `shouldBe` sort [(s.id, s.branch, s.pathPrefix) | s <- syncs]

      it "repositoryAccounts_requireAnExplicitChoiceAndOfferEveryGrant" \tr -> do
        -- repositoryAccounts_offerBothGrantsEvenWhenOneAlsoSyncsDashboards
        void $ syncOf tr testPid Nothing "zzz-ours" "monoscope-config" (GitSync.PersonalToken "ghp_test") ""
        for_ ["aaa-stray", "zzz-ours"] \name -> grantOf tr testPid Git.GitHub Nothing name (Just 222) Nothing >>= \c -> Cache.insert tr.trATCtx.repoListCache c.id []
        accounts <- forM ["team-a", "team-b"] \name -> do
          credential <- appAccount tr name
          credential <$ Cache.insert tr.trATCtx.repoListCache credential.id []
        page <- renderT . (.content) <$> call tr (withUnavailableRepositoryAPI $ PageCodeContext.repositoryConnectGetH testPid Nothing)
        for_ (["zzz-ours", "aaa-stray", "team-a", "team-b", "Choose an account"] :: [Text]) \label -> page `shouldSatisfy` T.isInfixOf label
        -- The newest grant is never picked for you.
        let mapping cred = call tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just "checkout") (Just "main") ((.account) <$> cred) Nothing Nothing Nothing ((.id) <$> cred))
        (void <$> mapping Nothing) `shouldReturn` Left PageCodeContext.NoAccount
        runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= (`shouldSatisfy` null)
        for_ accounts \account -> do
          editor <- renderT <$> call tr (PageCodeContext.codeMappingsEditorGetH testPid (Just account.id) (Just "/srv/app/main.py"))
          editor `shouldSatisfy` T.isInfixOf ("name=\"credentialId\" value=\"" <> account.id.toText <> "\"")
          editor `shouldSatisfy` T.isInfixOf "value=\"/srv/app/main.py\""
          void $ mapping (Just account)
        mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        sort [(m.owner, m.credentialId) | m <- mappings] `shouldBe` sort [(a.account, a.id) | a <- accounts]
        otherPid <- createTestProject tr "Other account project"
        void $ mapping . Just =<< grantOf tr otherPid Git.GitHub Nothing "foreign" (Just 42) Nothing
        length <$> runQueryEffect tr (CodeContext.getCodeMappings testPid) `shouldReturn` 2

    describe "Source context" do
      -- One git-host call per frame opened turns a hot issue into a rate-limit stall that reads as
      -- "the panel never fills". The cache is per blob, so a second frame of one file is free.
      it "fetches a blob once however many frames ask for it" \tr -> do
        void $ syncOf tr testPid Nothing "acme" "checkout-service" (GitSync.PersonalToken "ghp_test") ""
        _ <- call tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just "checkout-service") (Just "main") (Just "checkout") Nothing (Just "/srv/app/") (Just "") Nothing)
        -- A PAT needs no token exchange, so the one request is the blob read, and a read is a bodiless GET.
        let fetch n = runTestBgRecordingHTTP frozenTime tr $ withHTTPResponses (\_ _ -> pure $ Just $ blobOf "one\ntwo\nthree\nfour\n") $ CodeContext.fetchSnippet tr.trATCtx.codeBlobCache tr.trATCtx.config testPid (Just "checkout") Nothing "/srv/app/checkout.py" n
        (firstRequests, first_) <- fetch 2
        map snd firstRequests `shouldBe` [""]
        (secondRequests, second_) <- fetch 3
        secondRequests `shouldBe` []
        fmap (.focusLine) first_ `shouldBe` Right 2
        fmap (.focusLine) second_ `shouldBe` Right 3
        fmap (.body) second_ `shouldBe` Right ["one", "two", "three", "four"]

      it "sourceContextPages_guideAnUnconfiguredProjectFromFrameToMapping" \tr -> do
        connection <- call tr $ PageCodeContext.repositoryConnectGetH testPid Nothing
        renderT connection.content `shouldSatisfy` T.isInfixOf ("href=\"/p/" <> testPid.toText <> "/repositories/connect/token\"")
        -- Without a credential there is nothing to read repos through, so the page points at the
        -- control that fixes that rather than a form whose every submission is discarded.
        mappings <- renderT <$> call tr (PageCodeContext.repositoriesGetH testPid (Just PageCodeContext.Overview) Nothing)
        for_ ["href=\"/p/" <> testPid.toText <> "/repositories\"", "href=\"/p/" <> testPid.toText <> "/repositories/connect\"", "Pull requests", "Repositories", "Connect repositories"] \part -> mappings `shouldSatisfy` T.isInfixOf part
        mappings `shouldNotSatisfy` T.isInfixOf "Link repository"
        -- An unresolvable frame explains which of the two problems it has instead of failing the panel.
        unmapped <- call tr $ PageCodeContext.codeContextH testPid (Just "/srv/app/checkout.py") (Just 88) Nothing Nothing
        unmapped `shouldBe` PageCodeContext.SnippetUnavailable testPid (CodeContext.NoMapping "/srv/app/checkout.py")
        call tr (PageCodeContext.codeContextH testPid (Just "/srv/app/checkout.py") Nothing Nothing Nothing) `shouldReturn` PageCodeContext.NoFrameLine
        -- Once an account exists, the unmapped frame's path is carried into the mapping form.
        void $ syncOf tr testPid Nothing "acme" "monoscope-config" (GitSync.PersonalToken "ghp_test") ""
        linkable <- call tr $ PageCodeContext.codeContextH testPid (Just "/srv/app/checkout.py") (Just 88) Nothing Nothing
        renderT linkable `shouldSatisfy` T.isInfixOf "repositories?sample=%2Fsrv%2Fapp%2Fcheckout.py"
        form <- call tr $ withUnavailableRepositoryAPI $ PageCodeContext.repositoriesGetH testPid (Just PageCodeContext.Overview) (Just "/srv/app/checkout.py")
        renderT form `shouldSatisfy` T.isInfixOf "sample=%2Fsrv%2Fapp%2Fcheckout.py"

      it "maps several services onto several repos, and resolves each frame to its own" \tr -> do
        -- The config-sync repo is the project's monoscope YAML, not where the services live.
        void $ syncOf tr testPid Nothing "acme" "monoscope-config" (GitSync.PersonalToken "ghp_test") ""
        let add repo svc prefix root = call tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just repo) (Just "main") svc Nothing (Just prefix) (Just root) Nothing)
        _ <- add "checkout-service" (Just "checkout") "/srv/app/" "src"
        added <- renderT . fromRightShow <$> add "billing-service" (Just "billing") "/opt/billing/" ""
        -- The account came from the config-sync installation; the row says what it covers in frame-path terms.
        for_ ["acme/checkout-service", "acme/billing-service", "frames under /srv/app/"] \part -> added `shouldSatisfy` T.isInfixOf part
        added `shouldNotSatisfy` T.isInfixOf "monoscope-config"
        mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        sort (map (.repo) mappings) `shouldBe` ["billing-service", "checkout-service"]
        -- Each service's frame goes to its own repo; a frame no mapping claims resolves to nothing,
        -- since a snippet from the wrong repo is worse than none.
        let repoFor svc path = first (.repo) <$> CodeContext.resolveRepoPath mappings (Just svc) path
        repoFor "checkout" "/srv/app/services/checkout.py" `shouldBe` Just ("checkout-service", "src/services/checkout.py")
        repoFor "billing" "/opt/billing/invoice.rb" `shouldBe` Just ("billing-service", "invoice.rb")
        repoFor "search" "/srv/search/index.go" `shouldBe` Nothing
        cm <- maybe (fail "expected two mappings") pure $ listToMaybe mappings
        afterDelete <- call tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsDeleteH testPid cm.id
        renderT afterDelete `shouldNotSatisfy` T.isInfixOf (cm.owner <> "/" <> cm.repo)
        length <$> runQueryEffect tr (CodeContext.getCodeMappings testPid) `shouldReturn` 1

      it "sourceContext_disableRetainsServiceLinksAndDoesNotDisableReviews" \tr -> do
        account <- appAccount tr "team"
        runQueryEffect tr $ CodeContext.insertCodeMapping testPid account.id (Git.RepoRef "team" "checkout" "main") (Just "checkout") "" ""
        [repository] <- runQueryEffect tr $ GitSync.getRepositories testPid
        _ <- testServant tr $ PageCodeContext.repositorySourceEnabledPostH testPid repository.id (PageCodeContext.SourceContextForm False)
        map (.service) <$> runQueryEffect tr (CodeContext.getCodeMappings testPid) `shouldReturn` [Just "checkout"]
        runQueryEffect tr (W.runHTTPWreq $ withUnavailableRepositoryAPI $ CodeContext.fetchSnippet tr.trATCtx.codeBlobCache tr.trATCtx.config testPid (Just "checkout") Nothing "main.py" 1) `shouldReturn` Left (CodeContext.NoMapping "main.py")
        void $ runQueryEffect tr $ ImpactReviews.receiveEvent Nothing (ImpactReviews.PullRequestEvent "team" "checkout" 42 1 (T.replicate 40 "a") frozenTime ImpactReviews.Open)
        length <$> runQueryEffect tr (ImpactReviews.latestRuns testPid) `shouldReturn` 1

      it "repositoryOverview_listsEveryRepositoryOnceWithoutCallingTheProvider" \tr -> do
        -- repositoryConfiguration_usesTheSingleListInsteadOfDuplicateConnectionForms
        team <- appAccount tr "team"
        Just repository <- runQueryEffect tr $ GitSync.connectRepository testPid team.id (Git.GitRepo "team/checkout" "checkout" True "main")
        listed <- renderT <$> call tr (withUnavailableRepositoryAPI $ PageCodeContext.repositoriesGetH testPid (Just PageCodeContext.Overview) Nothing)
        for_ ["/repositories/" <> repository.id.toText, "Source context"] \part -> listed `shouldSatisfy` T.isInfixOf part
        for_ ["Connect a dashboard repository", "Link repository", ">Configuration<"] \label -> listed `shouldNotSatisfy` T.isInfixOf label
        -- Service and dashboard repositories share the list; serviceMappings_keepMultipleRepositoriesForTheSameServiceAndPath.
        acme <- grantOf tr testPid Git.GitHub Nothing "acme" (Just 42) Nothing
        for_ ["checkout-service", "shared"] \repo -> runQueryEffect tr $ CodeContext.insertCodeMapping testPid acme.id (Git.RepoRef "acme" repo "main") (Just "checkout") "/srv/app/" ""
        sort . map (.repo) <$> runQueryEffect tr (CodeContext.getCodeMappings testPid) `shouldReturn` ["checkout-service", "shared"]
        void $ syncOf tr testPid Nothing "acme" "dashboards" (GitSync.AppInstallation 42) ""
        overview <- renderT <$> call tr (withUnavailableRepositoryAPI $ PageCodeContext.repositoriesGetH testPid Nothing Nothing)
        for_ (["acme/checkout-service", "checkout", "acme/dashboards", "Awaiting first sync", "Production impact reviews"] :: [Text]) \label -> overview `shouldSatisfy` T.isInfixOf label
        reviews <- call tr $ PageCodeContext.repositoriesGetH testPid (Just PageCodeContext.PullRequests) Nothing
        renderT reviews `shouldSatisfy` T.isInfixOf "No pull requests reviewed yet"

      it "serviceRepositories_showOnlyApplicableOriginsAndExplainMissingLinksWithoutOfferingViewerEdits" \tr -> do
        let service = "team/api & jobs"
        account <- grantOf tr testPid Git.GitHub Nothing "acme" (Just 42) Nothing
        enterprise <- grantOf tr testPid Git.GitHub (Just "https://git.example.com") "acme" (Just 43) Nothing
        forM_ ([(account.id, "checkout-service", Just service, "/srv/"), (account.id, "shared-source", Nothing, "/shared/"), (account.id, "payments", Just "billing", "/payments/"), (enterprise.id, "checkout-service", Just "billing", "/enterprise/")] :: [(GitSync.GitHubCredentialId, Text, Maybe Text, Text)]) \(cid, repo, svc, prefix) ->
          runQueryEffect tr $ CodeContext.insertCodeMapping testPid cid (Git.RepoRef "acme" repo "main") svc prefix ""
        void $ syncOf tr testPid Nothing "acme" "dashboards" (GitSync.AppInstallation 42) ""
        repositories <- runQueryEffect tr $ GitSync.getRepositories testPid
        html <- renderT <$> call tr (withUnavailableRepositoryAPI $ PageCodeContext.serviceRepositoriesGetH testPid service)
        forM_ repositories \repository ->
          T.isInfixOf ("/repositories/" <> repository.id.toText <> "\"") html `shouldBe` (isNothing repository.apiBase && repository.repo `elem` ["checkout-service", "shared-source"])
        for_ ["All services", "team/api &amp; jobs", "Source mappings that apply to this service, including mappings for all services."] \part -> html `shouldSatisfy` T.isInfixOf part
        html `shouldNotSatisfy` T.isInfixOf "id=\"repository-nav\""
        dbExec tr "DELETE FROM projects.code_mappings WHERE project_id = ?" (Only testPid)
        unlinked <- renderT <$> call tr (PageCodeContext.serviceRepositoriesGetH testPid service)
        for_ ["No repositories linked to this service", "Choose a repository"] \part -> unlinked `shouldSatisfy` T.isInfixOf part
        dbExec tr "UPDATE projects.project_members SET permission = 'view' WHERE project_id = ?" (Only testPid)
        dbExec tr "DELETE FROM projects.repositories WHERE project_id = ?" (Only testPid)
        forM_ [PageCodeContext.serviceRepositoriesGetH testPid service, PageCodeContext.repositoriesGetH testPid Nothing Nothing] \handler -> do
          readonly <- renderT <$> call tr handler
          for_ ["Add repositories", "Choose a repository", "Connect repositories"] \control -> readonly `shouldNotSatisfy` T.isInfixOf control
          readonly `shouldSatisfy` T.isInfixOf "Ask a project editor"

      it "repositorySourceSetup_preservesIdentityAccountAndBranchAndScopesItsMutations" \tr -> do
        account <- appAccount tr "team"
        Cache.insert tr.trATCtx.repoListCache account.id [Git.GitRepo "team/checkout" "checkout" True "trunk", Git.GitRepo "team/catalog" "catalog" True "main"]
        Just repository <- runQueryEffect tr $ GitSync.connectRepository testPid account.id (Git.GitRepo "team/checkout" "checkout" True "trunk")
        runQueryEffect tr $ CodeContext.insertCodeMapping testPid account.id (Git.RepoRef "team" "catalog" "main") (Just "catalog") "/catalog/" ""
        html <- renderT <$> call tr (PageCodeContext.repositorySourceGetH testPid repository.id Nothing (Just "/app/main.py"))
        for_ ["value=\"team/checkout\"", "value=\"trunk\"", "value=\"/app/main.py\"", "name=\"credentialId\" value=\"" <> account.id.toText <> "\""] \value -> html `shouldSatisfy` T.isInfixOf value
        html `shouldNotSatisfy` T.isInfixOf "Unlink team/catalog"
        -- A forged owner/repo in the form cannot retarget the repository the route names.
        _ <- call tr $ PageCodeContext.repositorySourcePostH testPid repository.id (PageCodeContext.CodeMappingForm (Just "foreign/private") (Just "release") (Just "checkout") Nothing (Just "/app/") Nothing Nothing)
        mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        map (\m -> (m.owner, m.repo, m.ref, m.credentialId)) (filter ((== Just "checkout") . (.service)) mappings) `shouldBe` [("team", "checkout", "release", account.id)]
        _ <- testServant tr $ PageCodeContext.repositoryReviewsPostH testPid repository.id (ImpactReviews.ReviewSettings "foreign" "private" ImpactReviews.WhenAsked False)
        settings <- runQueryEffect tr $ ImpactReviews.repositorySettings testPid
        map (\s -> (s.owner, s.repo, s.trigger)) settings `shouldMatchList` [("team", "checkout", ImpactReviews.WhenAsked), ("team", "catalog", ImpactReviews.ReadyPullRequests)]
        for_ (filter ((== "checkout") . (.repo)) mappings) \mapping -> void $ testServant tr $ PageCodeContext.repositorySourceDeleteH testPid repository.id mapping.id
        map (.repo) <$> runQueryEffect tr (CodeContext.getCodeMappings testPid) `shouldReturn` ["catalog"]

    describe "Project wiki" do
      it "shares a project wiki with immutable revisions, conflict-safe edits and project permissions" \tr -> do
        let user = (getResponse tr.trSessAndHeader :: Projects.Session).user.id
            page = UUIDId $ UUID.fromWords 50 0 0 1 :: Memory.PageId
            duplicate = UUIDId $ UUID.fromWords 50 0 0 2 :: Memory.PageId
            app = wikiApp tr
            base pid = "/p/" <> pid.toText <> "/memories"
            wiki path = "/p/" <> testPid.toText <> "/wiki" <> path
            authorization = ("Authorization", "Bearer " <> UUID.toASCIIBytes (getResponse tr.trSessAndHeader).persistentSession.id.getPersistentSessionId)
            request method path fields =
              WT.runSession
                ( WT.srequest
                    $ WT.SRequest
                      (WT.setPath Wai.defaultRequest{Wai.requestMethod = method, Wai.requestHeaders = [authorization, ("Content-Type", "application/x-www-form-urlencoded")]} $ encodeUtf8 path)
                      (fromStrict $ BS.intercalate "&" [key <> "=" <> urlEncode True (encodeUtf8 value) | (key, value) <- fields])
                )
                app
            visit path = request H.methodGet path []
            post pageId version path title body = request H.methodPost (base testPid) [("pageId", pageId), ("version", version), ("path", path), ("title", title), ("body", body)]
            save pid expected title body = request H.methodPost (base pid) [("pageId", page.toText), ("version", show @Text expected), ("path", "runbooks/retries"), ("title", title), ("body", body)]
            html = decodeUtf8 @Text . (.simpleBody)
            status r = H.statusCode r.simpleStatus
            has r = traverse_ \part -> html r `shouldSatisfy` T.isInfixOf part
            lacks r = traverse_ \part -> html r `shouldNotSatisfy` T.isInfixOf part
            revisions = length <$> runQueryEffect tr (Memory.pageHistory testPid page)
        created <- save testPid (0 :: Word32) "Retry behavior" "Keep retry tokens. <script>alert('wiki')</script>"
        status created `shouldBe` 200
        has created ["Keep retry tokens."]
        lacks created ["<script>alert('wiki')</script>"]
        nested <- visit $ wiki "/runbooks/retries"
        status nested `shouldBe` 200
        has nested ["runbooks", "/wiki/runbooks/retries?edit", "/wiki/runbooks/retries"]
        lacks nested ["<wiki-editor"]
        editing <- visit $ wiki "/runbooks/retries?edit=true"
        has editing ["<wiki-editor", "form=\"wiki-form\""]
        -- Markdown renders, and no link, image or raw HTML can carry script or a form.
        for_
          ( zip
              [4 ..]
              [ ("**Formatted note** with *emphasis*.\n\n```haskell\nmain = pure ()\n```", ["<strong>Formatted note</strong>", "<em>emphasis</em>", "language-haskell"])
              , ("**Safe text** [unsafe](javascript:alert(42))", ["<strong>Safe text</strong>"])
              , ("**Encoded link** [unsafe](java&#x73;cript:alert(42))", ["<strong>Encoded link</strong>"])
              , ("**Mixed case** [unsafe](JaVaScRiPt:alert(42))", ["<strong>Mixed case</strong>"])
              , ("**Safe image** ![unsafe](data:text/html,alert)", ["<strong>Safe image</strong>"])
              , ("<script>alert('wiki')</script>", ["&lt;script&gt;"])
              , ("<form id=\"wiki-injection\"><input name=\"token\" type=\"password\"></form>", ["&lt;form"])
              , ("| Name | Count |\n| --- | ---: |\n| Item | 2 |\n\n- [x] Done\n\nA claim[^1].\n\n[^1]: Evidence", ["<table>", "type=\"checkbox\"", "<sup", "Evidence"])
              ]
          )
          \(n, (body, rendered)) -> do
            let formatted = UUIDId $ UUID.fromWords 50 0 0 n :: Memory.PageId
                path = "formatting/page-" <> show @Text n
            saved <- post formatted.toText "0" path "Formatted note" body
            status saved `shouldBe` 200
            readBack <- visit $ wiki $ "/" <> path
            status readBack `shouldBe` 200
            has readBack rendered
            T.toLower (html readBack) `shouldNotSatisfy` T.isInfixOf "href=\"javascript:alert"
            lacks readBack ["src=\"data:text/html", "<script>alert('wiki')</script>", "href=\"script\"", "id=\"wiki-injection\""]
            map (Memory.memoryText . (.document.body)) <$> runQueryEffect tr (Memory.pageHistory testPid formatted) `shouldReturn` [body]
        for_ ["runbooks%2Fretries", "runbooks/%252e%252e/private", "runbooks/%2e%2e/private"] \path -> status <$> visit (wiki $ "/" <> path) `shouldReturn` 404
        -- A path collision or an invalid path keeps the draft and writes nothing.
        collision <- post duplicate.toText "0" "runbooks/retries" "Conflicting path" "Preserve this draft"
        has collision ["already uses this path", "Preserve this draft"]
        runQueryEffect tr (Memory.pageHistory testPid duplicate) `shouldReturn` []
        for_ ["../private", "runbooks//retries", "runbooks/%2f", "runbooks/."] \path -> do
          invalidPath <- post page.toText "1" path "Invalid path" "Unsaved path draft"
          has invalidPath ["Use a relative path", "Unsaved path draft"]
        let legacyPage = UUIDId $ UUID.fromWords 50 0 0 3 :: Memory.PageId
        dbExec tr "INSERT INTO projects.memory_revisions (project_id, page_id, version, document, author) SELECT project_id, ?::uuid, 1, document, author FROM projects.memory_revisions WHERE project_id = ? AND page_id = ? AND version = 1" (legacyPage, testPid, page)
        status <$> visit (wiki $ "/" <> legacyPage.toText) `shouldReturn` 200
        firstHistory <- runQueryEffect tr $ Memory.pageHistory testPid page
        map (\r -> (r.version, r.author)) firstHistory `shouldBe` [(1, Memory.Member user)]
        -- Concurrent edits from one revision: exactly one wins, and history is immutable.
        (firstEdit, secondEdit) <- concurrently (save testPid 1 "Retry behavior" "First concurrent edit") (save testPid 1 "Retry behavior" "Second concurrent edit")
        length (filter (T.isInfixOf "This page changed" . html) [firstEdit, secondEdit]) `shouldBe` 1
        history <- runQueryEffect tr $ Memory.pageHistory testPid page
        map (.version) history `shouldBe` [2, 1]
        map (.document) (drop 1 history) `shouldBe` map (.document) firstHistory
        visit (base testPid) >>= (`has` ["Retry behavior"])
        viewed <- visit $ base testPid <> "?page=" <> page.toText
        has viewed ["Revision history", "Keep retry tokens. &lt;script&gt;"]
        lacks viewed ["href=\"script\""]
        typedWiki <- call tr $ PageCodeContext.memoryGetH testPid $ Just page
        (typedWiki.content.form.version, typedWiki.content.canEdit) `shouldBe` (2, True)
        stale <- save testPid 1 "Preserved draft" "Stale edit must not advance its expected revision"
        has stale ["Preserved draft", "name=\"version\" value=\"1\""]
        wikiTab <- visit $ "/p/" <> testPid.toText <> "/repositories?tab=wiki"
        status wikiTab `shouldBe` 200
        has wikiTab ["Retry behavior"]
        invalid <- save testPid 2 " " "Invalid title draft"
        has invalid ["Invalid title draft"]
        revisions `shouldReturn` 2
        for_ [(T.replicate 121 "x", "Oversized title"), ("Blank body", " "), ("Oversized body", T.replicate 16001 "x"), ("Invalid body", "bad\NULbody")] \(title, body) -> do
          rejected <- save testPid 2 title body
          status rejected `shouldBe` 200
          has rejected ["characters without null bytes"]
          revisions `shouldReturn` 2
        -- Other projects, viewers, anonymous visitors and the public demo cannot read or write it.
        other <- createTestProject tr "Isolated memory project"
        status <$> visit (base other <> "?page=" <> page.toText) `shouldReturn` 404
        runQueryEffect tr (Memory.pageHistory other page) `shouldReturn` []
        dbExec tr "UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?" (ProjectMembers.PView, testPid, user)
        readOnly <- visit $ base testPid <> "?page=" <> page.toText
        status readOnly `shouldBe` 200
        lacks readOnly ["Save page"]
        status <$> visit (wiki "/runbooks/unwritten") `shouldReturn` 404
        status <$> save testPid 2 "Retry behavior" "Viewer must not write" `shouldReturn` 403
        revisions `shouldReturn` 2
        let anonymous path = WT.runSession (WT.request $ WT.setPath Wai.defaultRequest $ encodeUtf8 path) app
        anonymous ("/p/" <> testPid.toText <> "/repositories") >>= (`lacks` ["Project wiki"])
        status <$> anonymous (base testPid) `shouldReturn` 404
        dbExec tr "UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?" (ProjectMembers.PEdit, testPid, user)
        -- Renames: blocked onto a used path, followed by the old path, derived from the title in a folder.
        status <$> post duplicate.toText "0" "services/other" "Other page" "Other notes" `shouldReturn` 200
        post page.toText "2" "services/other" "Blocked rename" "Unsaved rename draft" >>= (`has` ["already uses this path"])
        revisions `shouldReturn` 2
        post page.toText "2" "services/retries" "Retry behavior" "Renamed page body" >>= (`has` ["/wiki/services/retries"])
        status <$> visit (wiki "/services/retries") `shouldReturn` 200
        oldPath <- visit $ wiki "/runbooks/retries"
        status oldPath `shouldBe` 200
        has oldPath ["id=\"wiki-form\"", "value=\"runbooks/retries\""]
        post (UUID.toText (UUID.fromWords 50 0 0 99)) "0" "runbooks/" "Restart the Consumer" "Steps" >>= (`has` ["/wiki/runbooks/restart-the-consumer"])
        visit (wiki "?folder=runbooks") >>= (`has` ["value=\"runbooks/\""])
        -- Wiki links resolve within the project, never to a script, and are traced back and searchable.
        linking <- post duplicate.toText "1" "services/other" "Other page" "See [[services/retries|retry notes]] and [[javascript:alert(1)]]."
        has linking ["href=\"/p/" <> testPid.toText <> "/wiki/services/retries\"", ">retry notes</a>"]
        T.toLower (html linking) `shouldNotSatisfy` T.isInfixOf "href=\"javascript"
        lacks linking ["/wiki/javascript"]
        visit (wiki "/services/retries") >>= (`has` ["Linked from", "/wiki/services/other"])
        searched <- visit $ wiki "?q=RETRY%20NOTES"
        has searched ["/wiki/services/other"]
        lacks searched ["/wiki/services/retries"]
        visit (base testPid <> "?page=" <> duplicate.toText) >>= (`has` ["Changes from revision 1", "- Other notes", "+ See [[services/retries|retry notes]]"])
        status <$> visit (base testPid <> "?page=" <> page.toText) `shouldReturn` 200
        revisions `shouldReturn` 3
        -- Listings and history are capped.
        dbExec tr "INSERT INTO projects.memory_pages (project_id, page_id, path) SELECT project_id, md5(page_id::text || n::text)::uuid, md5(page_id::text || n::text) FROM projects.memory_revisions CROSS JOIN generate_series(1, 100) n WHERE project_id = ? AND page_id = ? AND version = 1" (testPid, page)
        dbExec tr "INSERT INTO projects.memory_revisions (project_id, page_id, version, document, author) SELECT project_id, md5(page_id::text || n::text)::uuid, 1, document, author FROM projects.memory_revisions CROSS JOIN generate_series(1, 100) n WHERE project_id = ? AND page_id = ? AND version = 1" (testPid, page)
        dbExec tr "INSERT INTO projects.memory_revisions (project_id, page_id, version, document, author) SELECT project_id, page_id, n, document, author FROM projects.memory_revisions CROSS JOIN generate_series(4, 102) n WHERE project_id = ? AND page_id = ? AND version = 1" (testPid, page)
        visit (base testPid <> "?page=" <> page.toText) >>= (`has` ["More pages", "Showing the 100 most recent revisions"])
        visit (wiki "?after=f") >>= (`has` ["/wiki/services/retries"])
        revisions `shouldReturn` 100

      it "wiki_saveAndNavigate_doesNotNestHtmxTargets" \tr -> do
        available <- doesFileExist "e2e/node_modules/playwright/index.mjs"
        unless available $ pendingWith "Install the repository e2e browser dependencies to run the wiki browser regression"
        environment <- filter (\(key, _) -> key `elem` ["PATH", "HOME", "TMPDIR", "XDG_CACHE_HOME", "PLAYWRIGHT_BROWSERS_PATH"]) <$> getEnvironment
        probe <- readProcessStdout_ $ setEnv environment $ proc "node" ["test/fixtures/wiki-browser.mjs", "--probe"]
        browser <- either fail pure $ AE.eitherDecode @Bool probe
        unless browser $ pendingWith "Install the repository Playwright browser to run the wiki browser regression"
        let token = UUID.toString (getResponse tr.trSessAndHeader).persistentSession.id.getPersistentSessionId
        Warp.testWithApplication (pure $ wikiApp tr) \port -> do
          output <- readProcessStdout_ $ setEnv (("MONOSCOPE_TEST_BEARER", token) : environment) $ proc "node" ["test/fixtures/wiki-browser.mjs", "http://127.0.0.1:" <> show port <> "/p/" <> toString testPid.toText <> "/memories"]
          value <- either fail pure $ AE.eitherDecode @AE.Value output
          let field :: AE.FromJSON a => AE.Key -> Either String a
              field name = AET.parseEither (AE.withObject "Browser result" (AE..: name)) value
          field "targets" `shouldBe` Right ([1, 1, 1] :: [Int])
          field "fields" `shouldBe` Right (["", "", ""] :: [Text])
          field "urls" `shouldBe` Right (["/p/" <> testPid.toText <> "/wiki/runbooks/retries", "/p/" <> testPid.toText <> "/wiki"] :: [Text])
          pages <- runQueryEffect tr $ Memory.latestPages testPid
          map (Memory.memoryText . (.document.body)) pages `shouldBe` ["**Keep caller retry tokens.**\n\n[Runbook][ref]\n\n[ref]: https://monoscope.tech\n"]

    describe "Repository deployment evidence" do
      it "keeps deployment requests, reported statuses, failures, and result limits distinct" \tr -> do
        let cfg = tr.trATCtx.config
            revision = T.replicate 40 "a"
            deployment n =
              AE.object
                [ "id" AE..= (n :: Int)
                , "sha" AE..= revision
                , "ref" AE..= ("main" :: Text)
                , "environment" AE..= ("production" :: Text)
                , "created_at" AE..= ("2025-01-01T00:00:00Z" :: Text)
                , "statuses_url" AE..= ("https://untrusted.example/statuses" :: Text)
                ]
            status reportedState at = AE.object ["state" AE..= (reportedState :: Text), "created_at" AE..= (at :: Text)]
            statusRows = [status "success" "2025-01-01T00:05:00Z", status "pending" "2025-01-01T00:02:00Z"] :: [AE.Value]
            repoUrl = "https://api.github.com/repos/acme/checkout-service"
        credential <- grantOf tr testPid Git.GitHub Nothing "acme" Nothing (Just "ghp_deployment_fixture")
        runQueryEffect tr $ CodeContext.insertCodeMapping testPid credential.id (Git.RepoRef "acme" "checkout-service" "main") (Just "checkout") "/srv/app/" ""
        linked <- runQueryEffect tr $ CodeContext.getLinkedRepositories testPid (Just "checkout")
        mapping <- case linked.entries of
          [entry] -> pure entry.mappingId
          _ -> fail "Expected one linked repository"
        otherPid <- createTestProject tr "Other deployment project"
        (deniedRequests, denied) <- runTestBgRecordingHTTP frozenTime tr $ CodeContext.fetchDeployments cfg otherPid mapping Nothing
        denied `shouldSatisfy` isLeft
        deniedRequests `shouldBe` []
        for_ ([(2, 2), (6, 12)] :: [(Int, Int)]) \(count, statusCount) -> do
          let transport = withHTTPResponses \_ endpoint -> do
                let suffix = drop (length repoUrl) endpoint
                take (length repoUrl) endpoint `shouldBe` repoUrl
                pure $ Just $ AE.encode $ case suffix of
                  "/deployments?per_page=5&environment=production" -> AE.toJSON $ map deployment [1 .. count]
                  "/deployments/2/statuses?per_page=10" -> AE.toJSON ([status "unrecognized_state" "2025-01-01T00:04:00Z"] :: [AE.Value])
                  _ -> AE.toJSON $ take statusCount $ cycle statusRows
          (requests, result) <- runTestBgRecordingHTTP frozenTime tr $ transport $ CodeContext.fetchDeployments cfg testPid mapping (Just "production")
          length requests `shouldBe` 1 + min 5 count
          page <- either (fail . show) pure result
          length page.entries `shouldBe` min 5 count
          page.limitReached `shouldBe` (count >= 5)
          case page.entries of
            firstEntry : secondEntry : _ -> do
              firstEntry.deployment.revision `shouldBe` revision
              reported <- either (fail . show) pure firstEntry.statuses
              map (.state) reported.entries `shouldBe` take (min 10 statusCount) (cycle [Git.DSSuccess, Git.DSPending])
              reported.limitReached `shouldBe` (statusCount >= 10)
              all ((> firstEntry.deployment.createdAt) . (.createdAt)) reported.entries `shouldBe` True
              secondEntry.statuses `shouldSatisfy` isLeft
            _ -> fail "Expected at least two deployments"

    describe "Repository details" do
      it "repositoryAnalysis_transientFailureAfterSetupDoesNotRestoreRemovedServiceLinks" \tr -> do
        account <- grantOf tr testPid Git.GitHub Nothing "team" Nothing (Just "fixture")
        Just repository <- runQueryEffect tr $ GitSync.connectRepository testPid account.id (Git.GitRepo "team/checkout" "checkout" True "main")
        dbExec tr "INSERT INTO apis.schema_summary (project_id, doc) VALUES (?, '{\"fields\":{},\"services\":[\"checkout\",\"billing\"],\"top_values_by_field\":{}}')" (Only testPid)
        let analyze host = void $ runJob tr host (BackgroundJobs.AnalyzeRepository testPid repository.id)
            provider :: (IOE :> es, W.HTTP :> es) => Eff es a -> Eff es a
            provider = gitHost [("/commits/", "{\"sha\":\"head\"}"), ("/git/trees/", "{\"tree\":[{\"path\":\"config/monitors\",\"type\":\"tree\",\"sha\":\"folder\"}]}")] "{\"full_name\":\"team/checkout\",\"name\":\"checkout\",\"private\":true,\"default_branch\":\"main\"}"
        analyze withUnavailableRepositoryAPI
        Just failed <- runQueryEffect tr $ GitSync.getRepositoryAnalysis testPid repository.id
        failed.error `shouldSatisfy` isJust
        analyze provider
        mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        map (.service) mappings `shouldBe` [Just "checkout"]
        [sync] <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        (sync.branch, sync.pathPrefix, sync.syncEnabled) `shouldBe` ("main", "config", True)
        -- What the user turned off stays off when a later analysis succeeds.
        _ <- testServant tr $ PageCodeContext.repositorySourceEnabledPostH testPid repository.id (PageCodeContext.SourceContextForm False)
        void $ runQueryEffect tr $ GitSync.pauseGitSync testPid sync.id
        for_ mappings \mapping -> runQueryEffect tr $ CodeContext.deleteCodeMapping testPid mapping.id
        analyze withUnavailableRepositoryAPI
        analyze provider
        Just current <- runQueryEffect tr $ GitSync.getRepository testPid repository.id
        current.sourceEnabled `shouldBe` False
        (.syncEnabled) <$> syncById tr sync `shouldReturn` False
        runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= (`shouldSatisfy` null)
        -- An ambiguous layout enables nothing.
        Just ambiguous <- runQueryEffect tr $ GitSync.connectRepository testPid account.id (Git.GitRepo "team/other" "other" True "main")
        let ambiguousProvider = withHTTPResponses \_ endpoint -> pure $ if "/git/trees/" `T.isInfixOf` toText endpoint then Just "{\"tree\":[{\"path\":\"a/dashboards\",\"type\":\"tree\",\"sha\":\"a\"},{\"path\":\"b/monitors\",\"type\":\"tree\",\"sha\":\"b\"}]}" else Nothing
        void $ runJob tr (provider . ambiguousProvider) (BackgroundJobs.AnalyzeRepository testPid ambiguous.id)
        length <$> runQueryEffect tr (GitSync.getGitSyncs testPid) `shouldReturn` 1
        runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= (`shouldSatisfy` null)

      it "repositoryConnection_missingReviewRunnerIsNotReadyAndRetainsDetails" \tr -> do
        let configured = tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppWebhookSecret = "fixture", githubAppId = "fixture", githubAppPrivateKey = "fixture", impactReviewAgent = Just "test/fixtures/impact-review-agent.mjs"}}}
            withConfig f r = r{trATCtx = r.trATCtx{config = f r.trATCtx.config}}
            withoutAgent = withConfig (\c -> c{impactReviewAgent = Nothing}) configured
        credential <- appAccount tr "team"
        runQueryEffect tr $ CodeContext.insertCodeMapping testPid credential.id (Git.RepoRef "team" "checkout" "main") (Just "checkout") "/app/" "src"
        sync <- syncOf tr testPid Nothing "team" "checkout" (GitSync.AppInstallation 42) "ops"
        [repository] <- runQueryEffect tr $ GitSync.getRepositories testPid
        let readiness r = (.content.reviewReadiness) <$> call r (PageCodeContext.repositoryGetH testPid repository.id)
        details <- call configured $ PageCodeContext.repositoryGetH testPid repository.id
        renderT details.content `shouldSatisfy` T.isInfixOf ("href=\"/p/" <> testPid.toText <> "/repositories/" <> repository.id.toText <> "/source\"")
        map (.service) details.content.mappings `shouldBe` [Just "checkout"]
        fmap (.id) details.content.dashboardSync `shouldBe` Just sync.id
        readiness configured >>= (`shouldSatisfy` \case PageCodeContext.ReviewNeedsRoutine -> True; _ -> False)
        void $ call configured $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "production-impact-review")
        readiness configured >>= (`shouldSatisfy` \case PageCodeContext.ReviewReady _ -> True; _ -> False)
        noRunner <- renderT . (.content) <$> call withoutAgent (PageCodeContext.repositoryGetH testPid repository.id)
        noRunner `shouldNotSatisfy` T.isInfixOf "Ready for pull requests"
        noRunner `shouldSatisfy` T.isInfixOf "Automatic reviews are unavailable. Ask your Monoscope administrator to finish review setup."
        readiness (withConfig (\c -> c{githubAppPrivateKey = ""}) configured) >>= (`shouldSatisfy` \case PageCodeContext.ReviewNeedsServer -> True; _ -> False)
        for_ details.content.mappings \mapping -> runQueryEffect tr $ CodeContext.deleteCodeMapping testPid mapping.id
        void $ runQueryEffect tr $ GitSync.deleteGitHubSync sync.id
        map (.id) <$> runQueryEffect tr (GitSync.getRepositories testPid) `shouldReturn` [repository.id]
        disconnected <- call configured $ PageCodeContext.repositoryGetH testPid repository.id
        disconnected.content.mappings `shouldSatisfy` null
        disconnected.content.dashboardSync `shouldSatisfy` isNothing
        disconnected.content.reviewReadiness `shouldSatisfy` \case PageCodeContext.ReviewNeedsService -> True; _ -> False
        otherPid <- createTestProject tr "Other repository details"
        runQueryEffect tr (GitSync.getRepository otherPid repository.id) >>= (`shouldSatisfy` isNothing)
        reconnected <- syncOf tr testPid Nothing "team" "checkout" (GitSync.AppInstallation 42) ""
        void $ runQueryEffect tr $ GitSync.updateGitHubSync (encKeyOf tr) reconnected.id "team" "dashboards" "main" Nothing Nothing
        map (.repo) <$> runQueryEffect tr (GitSync.getRepositories testPid) `shouldReturn` ["checkout", "dashboards"]

      it "repositoryRemoval_keepsLocalDashboardsAndHistoryWithoutRemovingOtherOriginsOrSharedAccounts" \tr -> do
        for_ ([Nothing, Just "https://git.example.com"] :: [Maybe Text]) \origin -> do
          account <- grantOf tr testPid Git.GitHub origin "team" (Just 42) Nothing
          runQueryEffect tr $ CodeContext.insertCodeMapping testPid account.id (Git.RepoRef "team" "checkout" "main") (Just $ fromMaybe "public" origin) "/app/" "src"
          sync <- syncOf tr testPid origin "team" "checkout" (GitSync.PersonalToken "fixture") "ops"
          void $ seedDash tr \d -> d{Dashboards.title = "Local overview", Dashboards.schema = Just def, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "overview.yaml", Dashboards.fileSha = Just "provider-version"}
        repository <- runQueryEffect tr (GitSync.getRepositories testPid) >>= maybe (fail "public repository missing") pure . find (isNothing . (.apiBase))
        let review n sha = void $ runQueryEffect tr $ ImpactReviews.receiveEvent Nothing (ImpactReviews.PullRequestEvent "team" "checkout" 42 n (T.replicate 40 sha) frozenTime ImpactReviews.Open)
            runIds = map (.id) <$> runQueryEffect tr (ImpactReviews.latestRuns testPid)
        review 1 "a"
        history <- runIds
        length history `shouldBe` 1
        otherPid <- createTestProject tr "Other repository removal"
        runQueryEffect tr (GitSync.removeRepository otherPid repository.id) `shouldReturn` Left GitSync.ConnectionMissing
        (removed, _) <- testServant tr $ PageCodeContext.repositoryDeleteH testPid repository.id
        getHeaders removed `shouldContain` [("HX-Redirect", encodeUtf8 $ "/p/" <> testPid.toText <> "/repositories")]
        map (.apiBase) <$> runQueryEffect tr (GitSync.getRepositories testPid) `shouldReturn` [Just "https://git.example.com"]
        map (.apiBase) <$> runQueryEffect tr (GitSync.getGitSyncs testPid) `shouldReturn` [Just "https://git.example.com"]
        map (.service) <$> runQueryEffect tr (CodeContext.getCodeMappings testPid) `shouldReturn` [Just "https://git.example.com"]
        length <$> runQueryEffect tr (GitSync.getGitHubCredentials testPid) `shouldReturn` 2
        dashboards <- runQueryEffect tr $ Dashboards.selectDashboardsSortedBy testPid Dashboards.Desc Dashboards.DSUpdatedAt
        length dashboards `shouldBe` 2
        [(d.title, d.filePath, d.fileSha) | d <- dashboards, isNothing d.gitSyncId] `shouldBe` [("Local overview", Nothing, Nothing)]
        length (filter (isJust . (.gitSyncId)) dashboards) `shouldBe` 1
        runIds `shouldReturn` history
        dbQuery tr "SELECT trigger, include_evidence FROM projects.pr_review_settings WHERE project_id = ? AND owner = 'team' AND repo = 'checkout'" (Only testPid) `shouldReturn` [("when_asked" :: Text, False)]
        runQueryEffect tr (GitSync.removeRepository testPid repository.id) `shouldReturn` Left GitSync.ConnectionMissing
        review 2 "b"
        runIds `shouldReturn` history
        -- The detached dashboard is never pushed again.
        localDashboard <- maybe (fail "local dashboard missing") pure $ find (isNothing . (.gitSyncId)) dashboards
        jobs <- pendingJobs tr
        runAuthHandler tr $ GitSyncPage.queueGitSyncPush testPid localDashboard.id
        length <$> pendingJobs tr `shouldReturn` length jobs
        (requests, _) <- runTestBgRecordingHTTP frozenTime tr $ withHTTPResponses (\_ _ -> pure $ Just "{\"content\":{\"sha\":\"written-blob\"},\"commit\":{\"sha\":\"written-head\"}}") do
          BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncPushDashboard testPid localDashboard.id.unUUIDId)
          BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncPushAllDashboards testPid)
        map fst requests `shouldSatisfy` (not . any (T.isInfixOf "local-overview.yaml"))
        fmap (.gitSyncId) <$> dashOf tr localDashboard.id `shouldReturn` Just Nothing

      it "repositoryDashboardSetup_reusesItsAccountAndQueuesOnlyItsInitialPull" \tr -> do
        let configured = tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppWebhookSecret = "fixture", githubAppId = "fixture", githubAppPrivateKey = "fixture"}}}
        account <- appAccount tr "team"
        let repo = Git.GitRepo "team/checkout" "checkout" True "trunk"
        Cache.insert tr.trATCtx.repoListCache account.id [repo]
        Just repository <- runQueryEffect tr $ GitSync.connectRepository testPid account.id repo
        initial <- call configured $ GitSyncPage.repositoryDashboardGetH testPid repository.id
        (initial.content.form.branch, initial.content.form.credentialId) `shouldBe` ("trunk", Just account.id)
        initial.content.sync `shouldSatisfy` isNothing
        let selection = GitSyncPage.RepositoryDashboardForm (Just account.id) "trunk" (Just "/ops/")
        missingConfig <- call tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppWebhookSecret = ""}}} $ GitSyncPage.repositoryDashboardPostH testPid repository.id selection
        missingConfig.content.setupError `shouldSatisfy` isJust
        missingConfig.content.form.pathPrefix `shouldBe` Just "/ops/"
        runQueryEffect tr (GitSync.getGitSyncs testPid) >>= (`shouldSatisfy` null)
        connected <- call configured $ GitSyncPage.repositoryDashboardPostH testPid repository.id selection
        sync <- maybe (fail "Expected dashboard sync") pure connected.content.sync
        (sync.owner, sync.repo, sync.branch, sync.pathPrefix, sync.installationId, sync.webhookSecret) `shouldBe` ("team", "checkout", "trunk", "ops", Just 42, Just "fixture")
        _ <- testServant configured $ GitSyncPage.repositoryDashboardPostH testPid repository.id selection{GitSyncPage.branch = "different"}
        map (.branch) <$> runQueryEffect tr (GitSync.getGitSyncs testPid) `shouldReturn` ["trunk"]
        jobs <- pendingJobs tr
        length [() | BackgroundJobs.GitSyncRepository project sid <- jobs, project == testPid, sid == sync.id] `shouldBe` 1
        otherPid <- createTestProject tr "Foreign dashboard account"
        foreignAccount <- grantOf tr otherPid Git.GitHub Nothing "foreign" (Just 43) Nothing
        runQueryEffect tr (GitSync.enableRepositoryDashboardSync testPid repository.id foreignAccount.id "main" "" Nothing) >>= (`shouldSatisfy` isNothing)
        -- A token repository detects its default branch and gets a generated webhook secret.
        tokenAccount <- grantOf tr testPid Git.GitLab (Just "https://git.example.com") "team" Nothing (Just "fixture-token")
        Just tokenRepo <- runQueryEffect tr $ GitSync.connectRepository testPid tokenAccount.id (Git.GitRepo "team/catalog" "catalog" True "main")
        unavailableBranch <- call tr $ withUnavailableRepositoryAPI $ GitSyncPage.repositoryDashboardPostH testPid tokenRepo.id (GitSyncPage.RepositoryDashboardForm (Just tokenAccount.id) "" (Just "ops"))
        unavailableBranch.content.setupError `shouldSatisfy` isJust
        unavailableBranch.content.sync `shouldSatisfy` isNothing
        tokenSetup <- call tr $ getOnly Nothing "{\"default_branch\":\"stable\"}" $ GitSyncPage.repositoryDashboardPostH testPid tokenRepo.id (GitSyncPage.RepositoryDashboardForm (Just tokenAccount.id) "" Nothing)
        tokenSync <- maybe (fail "Expected token dashboard sync") pure tokenSetup.content.sync
        tokenSync.branch `shouldBe` "stable"
        tokenSync.webhookSecret `shouldSatisfy` isJust
        decrypted <- runQueryEffect tr $ GitSync.getGitSyncsDecrypted (encKeyOf tr) testPid
        map (.accessToken) (filter ((== tokenSync.id) . (.id)) decrypted) `shouldBe` [Just "fixture-token"]
        renderT tokenSetup.content `shouldSatisfy` T.isInfixOf "Webhook secret"

    describe "Configuration folder" do
      it "configurationFolder_roundTripsDefinitionsWithoutClobberingRuntimeStateOrLaterEdits" \tr -> do
        sync <- syncOf tr testPid Nothing "team" "checkout" (GitSync.AppInstallation 42) ""
        let register pid sid kind path = do
              localId <- UUID.nextRandom
              runQueryEffect tr $ RepositorySync.registerResource pid sid kind localId path
              pure localId
            resourceIn pid sid localId = runQueryEffect tr (RepositorySync.resources pid sid) >>= maybe (fail "resource missing") pure . find ((== localId) . (.localId))
            resource = resourceIn testPid sync.id
            importDef r observed definition = runQueryEffect tr $ RepositorySync.importDefinition r observed definition
            localOf r = fmap AE.toJSON <$> runQueryEffect tr (RepositorySync.localDefinition r)
            snapshot r = runQueryEffect tr $ RepositorySync.resourceSnapshot r
            cron slug name = RepositorySync.CronDefinitionConfig $ RepositorySync.CronDefinition slug name 60 60
            query = def{MonitorDefinitions.title = "Errors", MonitorDefinitions.query = "status_code >= 500 | summarize count()", MonitorDefinitions.alertThreshold = 5, MonitorDefinitions.timeWindowMins = 5, MonitorDefinitions.checkIntervalMins = 1}
            runtimeKeys = ["current_status", "current_value", "muted_until", "last_status", "consecutive_failures", "last_checkin_at"]
            runtime = fmap \case AE.Object row -> KM.filterWithKey (\key _ -> key `elem` runtimeKeys) row; _ -> KM.empty
        -- Every definition kind round-trips, and a re-import keeps monitor runtime state.
        for_ [RepositorySync.QueryDefinition query, RepositorySync.UptimeDefinitionConfig $ RepositorySync.UptimeDefinition "Site" "https://example.com" 60 200 True, RepositorySync.CronDefinitionConfig $ RepositorySync.CronDefinition "daily-job" "Daily job" 3600 60] \definition -> do
          let kind = RepositorySync.definitionKind definition
              path = "monitors/" <> T.toLower (show kind) <> ".yaml"
          localId <- register testPid sync.id kind path
          created <- resource localId
          importDef created Nothing (Just definition) `shouldReturn` Right ()
          Just actual <- runQueryEffect tr $ RepositorySync.localDefinition created
          fmap AE.toJSON (RepositorySync.decodeDefinition path $ RepositorySync.encodeDefinition actual) `shouldBe` Right (AE.toJSON actual)
          case kind of
            RepositorySync.Query -> dbExec tr "UPDATE monitors.query_monitors SET current_status = 'alerting', current_value = 17, muted_until = now() + interval '1 hour' WHERE id = ?" (Only localId)
            RepositorySync.Uptime -> dbExec tr "UPDATE apis.prometheus_scrape_configs SET last_status = 'down', consecutive_failures = 7 WHERE id = ?" (Only localId)
            RepositorySync.Cron -> dbExec tr "UPDATE apis.cron_monitors SET last_status = 'error', last_checkin_at = now() WHERE id = ?" (Only localId)
            RepositorySync.Dashboard -> pass
          runtimeBefore <- snapshot created
          fresh <- resource localId
          importDef fresh runtimeBefore (Just actual) `shouldReturn` Right ()
          runtimeAfter <- snapshot created
          runtime runtimeAfter `shouldBe` runtime runtimeBefore
          when (kind == RepositorySync.Query) do
            beforeDelete <- resource localId
            importDef beforeDelete runtimeAfter Nothing `shouldReturn` Right ()
            deleted <- resource localId
            observed <- snapshot deleted
            importDef deleted observed (Just actual) `shouldReturn` Right ()
            localOf deleted `shouldReturn` Just (AE.toJSON actual)
        -- configurationImport_doesNotOverwriteAnEditMadeAfterItsSnapshot (nor deletes it).
        edited <- register testPid sync.id RepositorySync.Cron "monitors/job.yaml"
        let original = RepositorySync.CronDefinitionConfig $ RepositorySync.CronDefinition "job" "Original" 3600 60
        resource edited >>= \r -> importDef r Nothing (Just original) `shouldReturn` Right ()
        stale <- resource edited
        observed <- snapshot stale
        dbExec tr "UPDATE apis.cron_monitors SET name = 'Local edit' WHERE id = ?" (Only edited)
        result <- importDef stale observed (Just original)
        result `shouldSatisfy` isLeft
        Just current <- localOf stale
        current `shouldNotBe` AE.toJSON original
        importDef stale observed Nothing `shouldReturn` result
        localOf stale `shouldReturn` Just current
        -- configurationImport_reportsDuplicateCronSlugsWithoutAbortingOtherResources
        for_ (["first", "second", "third"] :: [Text]) \name -> do
          r <- resource =<< register testPid sync.id RepositorySync.Cron ("monitors/" <> name <> ".yaml")
          outcome <- importDef r Nothing (Just $ cron (if name == "second" then "first" else name) name)
          if name == "second" then outcome `shouldSatisfy` isLeft else outcome `shouldBe` Right ()
        -- configurationFolderChange_preservesLocalResourcesAndProposesThemAtTheNewPath (moves every resource, so its own project).
        moving <- createTestProject tr "Configuration folder move"
        movingSync <- syncOf tr moving Nothing "team" "checkout" (GitSync.AppInstallation 42) "old"
        jobId <- register moving movingSync.id RepositorySync.Cron "old/monitors/job.yaml"
        resourceIn moving movingSync.id jobId >>= \r -> importDef r Nothing (Just $ cron "job" "Job") `shouldReturn` Right ()
        void $ runQueryEffect tr $ GitSync.updateGitHubSync (encKeyOf tr) movingSync.id "team" "checkout" "main" Nothing (Just "new")
        moved <- resourceIn moving movingSync.id jobId
        (moved.filePath, moved.localBaseline) `shouldBe` ("new/monitors/job.yaml", Nothing)
        localOf moved `shouldReturn` Just (AE.toJSON $ cron "job" "Job")

    describe "Dashboard sync" do
      it "tokenConnection_rejectsMissingTokenAndQueuesOnlyFirstImport" \tr -> do
        let form owner repo branch token prefix = GitSyncPage.GitSyncForm{host = Just Git.GitHub, apiBase = Nothing, owner, repo, branch, accessToken = token, webhookSecret = Just "s3cret", pathPrefix = prefix}
        for_ ["", " "] \token -> do
          (void <$> call tr (GitSyncPage.gitSyncSettingsPostH testPid (form "test-owner" "test-repo" "main" token Nothing))) `shouldReturn` Left Git.TokenMissing
          runQueryEffect tr (GitSync.getGitSyncs testPid) >>= (`shouldSatisfy` null)
          pendingJobs tr >>= (`shouldSatisfy` null)
        void $ call tr $ GitSyncPage.gitSyncSettingsPostH testPid (form "test-owner" "test-repo" "main" "ghp_test" Nothing)
        sync <- onlySync tr >>= maybe (fail "Git connection was not created") pure
        (sync.owner, sync.repo, sync.branch, sync.syncEnabled) `shouldBe` ("test-owner", "test-repo", "main", True)
        let imports = filter isImport <$> pendingJobs tr
        imports >>= \jobs -> [(pid, sid) | BackgroundJobs.GitSyncRepository pid sid <- jobs] `shouldBe` [(testPid, sync.id)]
        -- The update path wrote every field but the folder, so an edit to "Folder in repo" vanished
        -- on save while the page redrew as if it had stuck. An empty token keeps the stored one.
        void $ call tr $ GitSyncPage.gitSyncSettingsPostH testPid (form "updated" "updated-repo" "dev" "" (Just "monoscope"))
        updated <- onlySync tr >>= maybe (fail "Git connection disappeared after update") pure
        (updated.owner, updated.repo, updated.branch, updated.pathPrefix) `shouldBe` ("updated", "updated-repo", "dev", "monoscope")
        updated.accessToken `shouldBe` sync.accessToken
        length <$> imports `shouldReturn` 1
        void $ testServant tr $ GitSyncPage.gitSyncSettingsDeleteH testPid
        onlySync tr >>= (`shouldSatisfy` isNothing)

      it "repositoryOwnership_isolatesIdenticalPathsQueuesOnlyAssignedPushesAndRetainsDashboardsOnDisconnect" \tr -> do
        let connect owner = syncOf tr testPid Nothing owner "dashboards" (GitSync.AppInstallation 42) ""
            assign did sync = runQueryEffect tr $ GitSync.assignDashboardRepository testPid did sync.id
            queuePush = runAuthHandler tr . GitSyncPage.queueGitSyncPush testPid
            pushesOf did = pendingJobs tr <&> \jobs -> length [() | BackgroundJobs.GitSyncPushDashboard project d <- jobs, project == testPid, d == did.unwrap]
        -- repositorySync_acceptsIndependentRepositoriesForDifferentTeams
        syncA <- connect "team-a"
        syncB <- connect "team-b"
        syncA.id `shouldNotBe` syncB.id
        (selection, _) <- testServant tr $ GitSyncPage.gitSyncSettingsDeleteH testPid
        getHeaders selection `shouldSatisfy` elem ("HX-Redirect", encodeUtf8 ("/p/" <> testPid.toText <> "/repositories"))
        length <$> runQueryEffect tr (GitSync.getGitSyncs testPid) `shouldReturn` 2
        [dashA, dashB, conflict] <- replicateM 3 $ seedDash tr \d -> d{Dashboards.title = "Overview"}
        -- queuesPushOnlyAfterDashboardRepositoryIsChosen
        queuePush dashB
        pushesOf dashB `shouldReturn` 0
        assign dashA syncA `shouldReturn` Right ()
        assign dashB syncB `shouldReturn` Right ()
        assign dashA syncB `shouldReturn` Left (GitSync.OwnedByRepository syncA.id)
        assign conflict syncA `shouldReturn` Left (GitSync.FileOwnedByDashboard dashA)
        otherPid <- createTestProject tr "Other repository project"
        runQueryEffect tr (GitSync.assignDashboardRepository otherPid dashA syncA.id) `shouldReturn` Left GitSync.RepositoryMissing
        _ <- runQueryEffect tr $ GitSync.updateDashboardGitInfo dashA "overview.yaml" "sha-a"
        _ <- runQueryEffect tr $ GitSync.updateDashboardGitInfo dashB "overview.yaml" "sha-b"
        fmap (\d -> (d.filePath, d.fileSha)) <$> dashOf tr dashA `shouldReturn` Just (Just "overview.yaml", Just "sha-a")
        M.toList <$> runQueryEffect tr (GitSync.getRepositoryDashboardState testPid syncA.id) `shouldReturn` [("overview.yaml", (dashA, Just "sha-a"))]
        M.toList <$> runQueryEffect tr (GitSync.getRepositoryDashboardState testPid syncB.id) `shouldReturn` [("overview.yaml", (dashB, Just "sha-b"))]
        queuePush dashB
        pushesOf dashB `shouldReturn` 1
        -- A paused repository queues no push.
        void $ testServant tr $ GitSyncPage.gitSyncRepositoryPauseH testPid syncB.id
        dbExec tr "DELETE FROM background_jobs WHERE payload->>'tag' LIKE 'GitSync%'" ()
        queuePush dashB
        pushesOf dashB `shouldReturn` 0
        _ <- testServant tr $ GitSyncPage.gitSyncRepositoryDeleteH testPid syncA.id
        fmap (\d -> (d.title, d.gitSyncId, d.filePath, d.fileSha)) <$> dashOf tr dashA `shouldReturn` Just ("Overview", Nothing, Nothing, Nothing)
        fmap (\d -> (d.gitSyncId, d.fileSha)) <$> dashOf tr dashB `shouldReturn` Just (Just syncB.id, Just "sha-b")
        overview <- renderT <$> call tr (PageCodeContext.repositoriesGetH testPid Nothing Nothing)
        for_ (["team-a/dashboards", "team-b/dashboards"] :: [Text]) \repo -> overview `shouldSatisfy` T.isInfixOf repo

      it "dashboardRepositoryAssignment_usesTheChosenRepositoryAndKeepsConflictsLocal" \tr -> do
        let connect project owner = syncOf tr project Nothing owner "dashboards" (GitSync.AppInstallation 42) ""
            create = seedDash tr \d -> d{Dashboards.title = "Overview", Dashboards.baseTemplate = Just "redis.yaml", Dashboards.fileSha = Just "local-checksum"}
            assign did sync = (.content) <$> call tr (GitSyncPage.dashboardRepositoryPostH testPid did (GitSyncPage.DashboardRepositoryForm sync.id))
        syncA <- connect testPid "team-a"
        syncB <- connect testPid "team-b"
        dash <- create
        initial <- call tr $ GitSyncPage.dashboardRepositoryGetH testPid dash
        length initial.content.repositories `shouldBe` 2
        owned <- (.dashboard) <$> assign dash syncB
        (owned.gitSyncId, owned.filePath, owned.fileSha, isJust owned.schema) `shouldBe` (Just syncB.id, Just "overview.yaml", Nothing, True)
        (owned.schema >>= (.file)) `shouldBe` Just "redis.yaml"
        jobs <- pendingJobs tr
        jobs `shouldSatisfy` any \case BackgroundJobs.GitSyncPushDashboard project did -> project == testPid && did == dash.unwrap; _ -> False
        (.assignmentError) <$> assign dash syncA `shouldReturn` Just (GitSync.OwnedByRepository syncB.id)
        other <- create
        conflict <- assign other syncB
        conflict.assignmentError `shouldBe` Just (GitSync.FileOwnedByDashboard dash)
        conflict.dashboard.gitSyncId `shouldBe` Nothing
        otherPid <- createTestProject tr "Other repository owner"
        foreignRepo <- connect otherPid "foreign"
        (.assignmentError) <$> assign other foreignRepo `shouldReturn` Just GitSync.RepositoryMissing

      -- A delivery queues an import only when its signature verifies against the row's own secret,
      -- the route's host matches the row's, and the push is to the tracked branch.
      it "webhooks_queueImportsOnlyForVerifiedPushesToTheTrackedRow" \tr -> do
        let pushBody owner repo = toStrict $ AE.encode $ AE.object ["push" AE..= AE.object ["changes" AE..= ([AE.object ["new" AE..= AE.object ["name" AE..= ("main" :: Text)]]] :: [AE.Value])], "ref" AE..= ("refs/heads/main" :: Text), "repository" AE..= AE.object ["full_name" AE..= (owner <> "/" <> repo :: Text)]]
            signed secret body = Just $ "sha256=" <> hmacHexOf secret body
            deliver host ev sig body = do
              dbExec tr "DELETE FROM background_jobs WHERE payload->>'tag' LIKE 'GitSync%'" ()
              void $ toBaseServantResponse tr $ GitSyncPage.gitWebhookPostH host Git.WebhookReq{event = Just ev, signature = sig, gitlabToken = Nothing, gitlabWebhookId = Nothing, gitlabTimestamp = Nothing, body}
              length . filter isImport <$> pendingJobs tr
            announced = (>>= (.announcedRevision)) <$> onlySync tr
        setupSyncOn tr Git.GitHub "webhook-owner" "webhook-repo" (Just "s3cret")
        let body = pushBody "webhook-owner" "webhook-repo"
            other = pushBody "unknown" "repo"
        -- Forged (wrong secret) or unsigned deliveries to a row holding a secret queue nothing.
        deliver Git.GitHub "push" (Just "sha256=deadbeef") body `shouldReturn` 0
        deliver Git.GitHub "push" Nothing body `shouldReturn` 0
        deliver Git.GitHub "push" (signed "s3cret" body) body >>= (`shouldSatisfy` (>= 1))
        -- Non-push events and untracked repos are ignored.
        deliver Git.GitHub "ping" (signed "s3cret" body) body `shouldReturn` 0
        deliver Git.GitHub "push" (signed "s3cret" other) other `shouldReturn` 0
        -- A push records the head it announced: a fetch answered from an older view must not read
        -- as "no change" and complete having done nothing (3 of 5 demo-project pushes were lost so).
        let announcing = toStrict $ AE.encode $ AE.object ["ref" AE..= ("refs/heads/main" :: Text), "repository" AE..= AE.object ["full_name" AE..= ("webhook-owner/webhook-repo" :: Text)], "after" AE..= ("f00dcafe" :: Text)]
        void $ deliver Git.GitHub "push" (signed "s3cret" announcing) announcing
        announced `shouldReturn` Just "f00dcafe"
        -- The announcement outlives a pull that did not reach it, or the retry it licenses is
        -- cleared by the very fetch that failed to satisfy it.
        setupSyncOn tr Git.GitHub "ann-owner" "ann-repo" (Just "s3cret")
        Just sync <- onlySync tr
        _ <- runQueryEffect tr $ GitSync.setAnnouncedRevision sync.id "newhead"
        _ <- runQueryEffect tr $ GitSync.recordImportSuccess sync.id "oldhead"
        announced `shouldReturn` Just "newhead"
        _ <- runQueryEffect tr $ GitSync.recordImportSuccess sync.id "newhead"
        settled <- onlySync tr
        (settled >>= (.announcedRevision), settled >>= (.lastRevision)) `shouldBe` (Nothing, Just "newhead")
        -- Each host verifies in its own dialect: Gitea sends the bare hex digest, Bitbucket prefixes it.
        forM_ [(Git.Gitea, "push" :: Text), (Git.Bitbucket, "repo:push")] \(host, ev) -> do
          setupSyncOn tr host "acme" "config" (Just "s3cret")
          let hosted = pushBody "acme" "config"
          deliver host ev (Just $ (if host == Git.Bitbucket then "sha256=" else "") <> hmacHexOf "s3cret" hosted) hosted >>= (`shouldSatisfy` (>= 1))
        -- importsOnlyTheTargetBranchAndIgnoresTheAutomatedPRBranch
        setupSyncOn tr Git.GitHub "branch-filter-owner" "config" (Just "s3cret")
        for_ (["monoscope-dashboards", "feature/user-pr"] :: [Text]) \branch -> do
          let branchPush = toStrict $ AE.encode $ AE.object ["repository" AE..= AE.object ["full_name" AE..= ("branch-filter-owner/config" :: Text)], "ref" AE..= ("refs/heads/" <> branch)]
          deliver Git.GitHub "push" (signed "s3cret" branchPush) branchPush `shouldReturn` 0
        let tracked = pushBody "branch-filter-owner" "config"
        deliver Git.GitHub "push" (signed "s3cret" tracked) tracked `shouldReturn` 1
        -- The same owner/repo on another host is another project's repository: a Gitea-signed
        -- delivery on the Gitea route matches no GitLab row.
        setupSyncOn tr Git.GitLab "acme" "config" (Just "gl-secret")
        let collision = pushBody "acme" "config"
        deliver Git.Gitea "push" (Just $ hmacHexOf "gl-secret" collision) collision `shouldReturn` 0

      it "repositoryExport_retriesFailuresAndUpdatesOneConfigurationPRWithoutWritingToTheTargetBranch" \tr -> do
        sync <- teamDashboards tr
        did <- ownedDash tr sync "Overview" "overview.yaml"
        let export provider = runJob tr provider (BackgroundJobs.GitSyncPushDashboard testPid did.unwrap)
            push = export . configurationProvider tr
            title = fmap (.title) <$> dashOf tr did
        -- A target branch that moves mid-export is not proposed against, and the edit stays.
        calls <- newIORef (0 :: Int)
        let advancing = withHTTPResponses \_ endpoint ->
              if "/commits/main" `T.isInfixOf` toText endpoint
                then atomicModifyIORef' calls (\n -> (n + 1, n)) <&> \n -> Just $ if n == 0 then "{\"sha\":\"head\",\"commit\":{\"tree\":{\"sha\":\"tree\"}}}" else "{\"sha\":\"new-head\",\"commit\":{\"tree\":{\"sha\":\"new-tree\"}}}"
                else pure Nothing
        export (configurationProvider tr "open" . advancing) >>= (`shouldNotSatisfy` any (\(url, body) -> not (LBS.null body) && "/git/trees" `T.isSuffixOf` url))
        (.lastError) <$> syncById tr sync >>= (`shouldSatisfy` isJust)
        title `shouldReturn` Just "Overview"
        void $ export $ gitHost [("/git/trees/", "{\"tree\":[]}")] "{\"sha\":\"head\"}"
        (.lastError) <$> syncById tr sync `shouldReturn` Just (GitSync.SyncFailure GitSync.ExportDashboards "GitHub did not return the target tree.")
        retried <- call tr $ GitSyncPage.gitSyncRepositoryRetryH testPid sync.id
        renderT retried `shouldSatisfy` T.isInfixOf "Retry export"
        pendingJobs tr >>= \queued -> [sid | BackgroundJobs.GitSyncPushRepository pid sid <- queued, pid == testPid] `shouldBe` [sync.id]
        firstRequests <- push "open"
        ((,) <$> (.pullRequestNumber) <*> (.lastError)) <$> syncById tr sync `shouldReturn` (Just 17, Nothing)
        void $ testServant tr $ DashboardPage.dashboardRenamePatchH testPid did (DashboardPage.DashboardRenameForm "Updated overview" Nothing)
        secondRequests <- push "open"
        (.pullRequestNumber) <$> syncById tr sync `shouldReturn` Just 17
        length [() | (url, _) <- firstRequests <> secondRequests, "/pulls" `T.isSuffixOf` url] `shouldBe` 1
        let writes = map fst $ filter (not . LBS.null . snd) $ firstRequests <> secondRequests
        writes `shouldSatisfy` any (T.isInfixOf "/git/trees")
        writes `shouldSatisfy` any (T.isInfixOf "/git/refs/heads/monoscope-dashboards")
        writes `shouldNotSatisfy` any (T.isInfixOf "/git/refs/heads/main")
        writes `shouldNotSatisfy` any (T.isInfixOf "/contents/")
        title `shouldReturn` Just "Updated overview"
        -- A closed proposal pauses export and keeps the local edit.
        void $ push "closed"
        (.exportPaused) <$> syncById tr sync `shouldReturn` True
        title `shouldReturn` Just "Updated overview"

      it "repositoryRevertedAddition_removesTheStaleFileFromItsOpenPR" \tr -> do
        sync <- teamDashboards tr
        did <- ownedDash tr sync "Temporary" "temporary.yaml"
        void $ runJob tr (configurationProvider tr "open") (BackgroundJobs.GitSyncPushDashboard testPid did.unwrap)
        void $ runQueryEffect tr $ Dashboards.deleteDashboardsByIds testPid (fromList [did])
        requests <- runJob tr (configurationProvider tr "open") (BackgroundJobs.GitSyncRepository testPid sync.id)
        let trees = [body | (url, body) <- requests, "/git/trees" `T.isSuffixOf` url]
        length trees `shouldBe` 1
        trees `shouldSatisfy` all (T.isInfixOf "\"tree\":[]" . decodeUtf8 . LBS.toStrict)

      it "repositoryPush_doesNotAdvanceThePullCursorPastOtherRemoteDashboards" \tr -> do
        sync <- syncOf tr testPid (Just "https://127.0.0.1:1") "team" "dashboards" (GitSync.PersonalToken "fixture") ""
        did <- seedDash tr \d -> d{Dashboards.title = "Local", Dashboards.schema = Just def, Dashboards.filePath = Just "local.yaml"}
        runQueryEffect tr (GitSync.assignDashboardRepository testPid did sync.id) `shouldReturn` Right ()
        let transport :: (IOE :> es, W.HTTP :> es) => Eff es a -> Eff es a
            transport = gitHost [("remote.yaml", blobOf "title: Remote\nwidgets: []\n"), ("/contents/", "{\"content\":{\"sha\":\"local-blob\"}}"), ("/git/trees/", treeOf [("dashboards/local.yaml", "local-blob"), ("dashboards/remote.yaml", "remote-blob")])] "{\"sha\":\"head\"}"
        for_ [BackgroundJobs.GitSyncPushDashboard testPid did.unwrap, BackgroundJobs.GitSyncRepository testPid sync.id] $ runJob tr transport
        M.keys <$> runQueryEffect tr (GitSync.getRepositoryDashboardState testPid sync.id) `shouldReturn` ["local.yaml", "remote.yaml"]

      it "repositoryPull_preservesAnAssignedDashboardBeforeFirstPushAndReportsRemotePathConflicts" \tr -> do
        sync <- syncOf tr testPid (Just "https://127.0.0.1:1") "team" "dashboards" (GitSync.PersonalToken "fixture") ""
        did <- ownedDash tr sync "Local overview" "overview.yaml"
        let pull revision tree = runJob tr (gitHost [("/git/trees/", tree), ("/contents/", blobOf "title: Remote overview\nwidgets: []\n")] (AE.encode $ AE.object ["sha" AE..= (revision :: Text)])) (BackgroundJobs.GitSyncRepository testPid sync.id)
            remote = treeOf [("dashboards/overview.yaml", "remote-blob")]
        _ <- pull "before-first-push" (treeOf [])
        fmap (\d -> (d.title, d.fileSha)) <$> dashOf tr did `shouldReturn` Just ("Local overview", Nothing)
        outcome <- try @_ @SomeException $ pull "remote-conflict" remote
        whenLeft_ outcome $ expectationFailure . ("Pull must report a first-push conflict rather than attempt a duplicate insert: " <>) . show
        (.lastRevision) <$> syncById tr sync `shouldReturn` Just "remote-conflict"
        conflicts <- runQueryEffect tr $ RepositorySync.resources testPid sync.id
        conflict <- maybe (fail "Expected the dashboard conflict") pure $ find (.conflicted) conflicts
        [repository] <- runQueryEffect tr $ GitSync.getRepositories testPid
        status <- renderT . (.content) <$> call tr (PageCodeContext.repositoryGetH testPid repository.id)
        for_ ["Both versions changed", "Keep Monoscope", "Compare versions", "Active in Monoscope"] \part -> status `shouldSatisfy` T.isInfixOf part
        void $ testServant tr $ PageCodeContext.repositoryConflictPostH testPid repository.id conflict.id (PageCodeContext.ConflictForm False)
        void $ pull "remote-conflict" remote
        fmap (.title) <$> dashOf tr did `shouldReturn` Just "Local overview"
        runQueryEffect tr (RepositorySync.resources testPid sync.id) >>= (`shouldSatisfy` all (not . (.conflicted)))

      it "repositoryPull_keepsTheTreeContentAndCursorAtOneCommitWhenTheBranchMoves" \tr -> do
        sync <- teamDashboards tr
        let transport = withHTTPResponses \_ endpoint -> do
              let path = toText endpoint
              pure $ Just $ if "/commits/" `T.isInfixOf` path then "{\"sha\":\"first-commit\"}" else if "/git/trees/" `T.isInfixOf` path then treeOf [("dashboards/overview.yaml", "first-blob")] else blobOf (if "first-commit" `T.isInfixOf` path then "title: Original overview\nwidgets: []\n" else "title: Later overview\nwidgets: []\n")
        requests <- runJob tr transport (BackgroundJobs.GitSyncRepository testPid sync.id)
        map (\d -> (d.title, d.fileSha)) <$> allDashboards tr `shouldReturn` [("Original overview", Just "first-blob")]
        map fst requests `shouldBe` map ("https://api.github.com/repos/team/dashboards" <>) ["/commits/main", "/git/trees/first-commit?recursive=1", "/contents/dashboards/overview.yaml?ref=first-commit"]
        (.lastRevision) <$> syncById tr sync `shouldReturn` Just "first-commit"

      it "repositoryPull_renamesAnOwnedDashboardWithoutLosingItsIdentity" \tr -> do
        sync <- syncOf tr testPid (Just "https://127.0.0.1:1") "team" "dashboards" (GitSync.PersonalToken "fixture") ""
        did <- seedDash tr \d -> d{Dashboards.title = "Overview", Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "old.yaml", Dashboards.fileSha = Just "same-content"}
        requests <- runJob tr (gitHost [("/git/trees/", treeOf [("dashboards/new.yaml", "same-content")])] "{\"sha\":\"new-head\"}") (BackgroundJobs.GitSyncRepository testPid sync.id)
        length requests `shouldBe` 3
        fmap (\d -> (d.filePath, d.fileSha)) <$> dashOf tr did `shouldReturn` Just (Just "new.yaml", Just "same-content")

      it "repositoryPull_reportsInvalidYamlAndRetriesTheSameRevisionAfterCorrection" \tr -> do
        sync <- syncOf tr testPid (Just "https://127.0.0.1:1") "team" "dashboards" (GitSync.PersonalToken "fixture") ""
        deleted <- seedDash tr \d -> d{Dashboards.title = "Removed", Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "removed.yaml", Dashboards.fileSha = Just "old"}
        let pull yaml = runJob tr (gitHost [("/git/trees/", treeOf [("dashboards/overview.yaml", "blob"), ("dashboards/valid.yaml", "valid")]), ("valid.yaml", blobOf "title: Valid\nwidgets: []\n"), ("/contents/", blobOf yaml)] "{\"sha\":\"head\"}") (BackgroundJobs.GitSyncRepository testPid sync.id)
            fileKeys = M.keys <$> runQueryEffect tr (GitSync.getRepositoryDashboardState testPid sync.id)
        _ <- pull "title: [invalid"
        failed <- syncById tr sync
        dashOf tr deleted >>= (`shouldSatisfy` isNothing)
        fileKeys `shouldReturn` ["valid.yaml"]
        failed.lastRevision `shouldBe` Nothing
        failed.lastError `shouldSatisfy` maybe False (T.isInfixOf "overview.yaml" . (.message))
        _ <- pull "title: Recovered\nwidgets: []\n"
        ((,) <$> (.lastRevision) <*> (.lastError)) <$> syncById tr sync `shouldReturn` (Just "head", Nothing)
        fileKeys `shouldReturn` ["overview.yaml", "valid.yaml"]

    describe "Sync workers and leases" do
      it "repositoryWorkers_deferOverlappingImportsWithoutDuplicatingDashboards" \tr -> do
        sync <- teamDashboards tr
        other <- syncOf tr testPid Nothing "another-team" "dashboards" (GitSync.PersonalToken "fixture") ""
        did <- ownedDash tr sync "Local overview" "local.yaml"
        fetching <- newEmptyMVar
        release <- newEmptyMVar
        let job = BackgroundJobs.GitSyncRepository testPid sync.id
            provider blocked = withHTTPResponses \_ endpoint -> do
              let path = toText endpoint
              if "/contents/" `T.isInfixOf` path
                then do
                  when blocked $ putMVar fetching () >> takeMVar release
                  pure $ Just $ blobOf "title: Imported overview\nwidgets: []\n"
                else pure $ Just $ if "/git/trees/" `T.isInfixOf` path then treeOf [("dashboards/overview.yaml", "blob")] else "{\"sha\":\"head\"}"
        withAsync (runJob tr (provider True) job) \worker -> do
          timeout 5000000 (takeMVar fetching) `shouldReturn` Just ()
          for_ [job, BackgroundJobs.GitSyncPushDashboard testPid did.unUUIDId, BackgroundJobs.GitSyncPushAllDashboards testPid] \overlap -> do
            outcome <- try @_ @SomeException $ runJob tr (provider False) overlap
            outcome `shouldSatisfy` isLeft
          pendingJobs tr >>= (`shouldSatisfy` null)
          independent <- runJob tr (provider False) (BackgroundJobs.GitSyncRepository testPid other.id)
          length independent `shouldBe` 3
          putMVar release ()
          void $ wait worker
        void $ runJob tr (provider False) job
        map (\d -> (d.title, d.gitSyncId)) <$> allDashboards tr >>= (`shouldMatchList` [("Imported overview", Just sync.id), ("Imported overview", Just other.id), ("Local overview", Just sync.id)])
        completed <- syncById tr sync
        completed.lastRevision `shouldBe` Just "head"
        completed.lastError `shouldBe` Just (GitSync.SyncFailure GitSync.ExportDashboards "GitHub did not return the target tree.")

      it "repositoryRemoval_waitsForActiveImportsAndExportsWithoutDetachingTheirDashboards" \tr -> do
        sync <- teamDashboards tr
        [repository] <- runQueryEffect tr $ GitSync.getRepositories testPid
        did <- seedDash tr \d -> d{Dashboards.title = "Local overview", Dashboards.schema = Just def, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "local.yaml", Dashboards.fileSha = Just "blob-local"}
        for_ [BackgroundJobs.GitSyncRepository testPid sync.id, BackgroundJobs.GitSyncPushDashboard testPid did.unUUIDId] \job -> do
          fetching <- newEmptyMVar
          release <- newEmptyMVar
          blockedOnce <- newIORef False
          let provider = withHTTPResponses \_ endpoint -> do
                let path = toText endpoint
                if "/contents/" `T.isInfixOf` path
                  then do
                    shouldBlock <- atomicModifyIORef' blockedOnce (True,)
                    unless shouldBlock $ putMVar fetching () >> takeMVar release
                    pure $ Just $ if "/local.yaml" `T.isInfixOf` path then "{\"content\":{\"sha\":\"written-blob\"}}" else blobOf "title: Imported overview\nwidgets: []\n"
                  else pure $ Just $ if "/git/trees/" `T.isInfixOf` path then treeOf [("dashboards/overview.yaml", "blob"), ("dashboards/local.yaml", "blob-local")] else "{\"sha\":\"head\"}"
          withAsync (runJob tr provider job) \worker -> do
            timeout 5000000 (takeMVar fetching) `shouldReturn` Just ()
            Done refused <- call tr $ PageCodeContext.repositoryDeleteH testPid repository.id
            refused `shouldBe` PageCodeContext.RemovalBlockedBySync
            runQueryEffect tr (GitSync.getRepository testPid repository.id) >>= (`shouldSatisfy` isJust)
            disconnected <- call tr $ GitSyncPage.gitSyncRepositoryDeleteH testPid sync.id
            renderT disconnected `shouldSatisfy` T.isInfixOf "A dashboard sync is running"
            fmap (.gitSyncId) <$> dashOf tr did `shouldReturn` Just (Just sync.id)
            putMVar release ()
            void $ wait worker
        -- An expired lease no longer blocks removal.
        bracket
          (runTestBg (addUTCTime (-181) frozenTime) tr (GitSync.claimSyncLease sync.id) >>= maybe (fail "Expected a free repository lease") pure)
          (\claimed -> runQueryEffect tr $ GitSync.releaseSyncLease claimed)
          \_ -> do
            advanceTestTime tr 181
            runQueryEffect tr (GitSync.removeRepository testPid repository.id) `shouldReturn` Right ()
        map (\d -> (d.title, d.gitSyncId, d.filePath, d.fileSha)) <$> allDashboards tr >>= (`shouldMatchList` [("Imported overview", Nothing, Nothing, Nothing), ("Local overview", Nothing, Nothing, Nothing)])
        runJob tr id (BackgroundJobs.GitSyncRepository testPid sync.id) `shouldReturn` []

      it "repositoryWorkers_releaseCancelledClaimsAndCannotReleaseANewerLease" \tr -> do
        sync <- teamDashboards tr
        fetching <- newEmptyMVar
        release <- newEmptyMVar
        let blocked = withHTTPResponses \_ _ -> putMVar fetching () >> takeMVar release $> Nothing
            acquire time = runTestBg time tr (GitSync.claimSyncLease sync.id) >>= maybe (fail "Expected a free repository lease") pure
            relinquish claimed = runQueryEffect tr $ GitSync.releaseSyncLease claimed
        withAsync (runJob tr blocked (BackgroundJobs.GitSyncRepository testPid sync.id)) \worker -> do
          timeout 5000000 (takeMVar fetching) `shouldReturn` Just ()
          cancel worker
          waitCatch worker >>= (`shouldSatisfy` isLeft)
        dbQuery tr "SELECT count(*) FROM projects.repository_sync_leases WHERE sync_id = ?" (Only sync.id) `shouldReturn` [Only (0 :: Int)]
        bracket (acquire frozenTime) relinquish \old ->
          bracket (acquire $ addUTCTime 181 frozenTime) relinquish \_ -> do
            relinquish old
            runTestBg (addUTCTime 181 frozenTime) tr (GitSync.claimSyncLease sync.id) `shouldReturn` Nothing
        bracket (acquire $ addUTCTime 181 frozenTime) relinquish (const pass)

      it "repositoryWorkers_leaveContentionToTheQueuesBoundedRetryPolicy" \tr -> do
        sync <- teamDashboards tr
        Just lease <- runTestBg frozenTime tr $ GitSync.claimSyncLease sync.id
        bracket (pure lease) (\claimed -> runQueryEffect tr $ GitSync.releaseSyncLease claimed) \_ -> do
          job <- withResource tr.trATCtx.jobsPool \conn -> Jobs.createJob conn "background_jobs" (BackgroundJobs.GitSyncRepository testPid sync.id)
          threads <- newIORef M.empty
          let runner queued = Jobs.throwParsePayload queued >>= void . runTestBgRecordingHTTP frozenTime tr . BackgroundJobs.processBackgroundJob tr.trATCtx
              env = Jobs.RunnerEnv (mkConfig (\_ _ -> pass) "background_jobs" tr.trATCtx.jobsPool (Jobs.MaxConcurrentJobs 1) runner id) threads
          for_ [1 .. 10] \attempt -> do
            withResource tr.trATCtx.jobsPool \conn -> void $ PG.execute conn "UPDATE background_jobs SET run_at = ? WHERE id = ?" (frozenTime, job.jobId)
            Just worker <- runReaderT (Jobs.pollRunJob "repository-retry-test" Nothing) env
            void $ waitCatch worker
            retried <- withResource tr.trATCtx.jobsPool (\conn -> Jobs.findJobByIdIO conn "background_jobs" job.jobId) >>= maybe (fail "Retry replaced the original job") pure
            (retried.jobAttempts, retried.jobStatus) `shouldBe` (attempt, if attempt == 10 then Jobs.Failed else Jobs.Retry)
            retried.jobLastError `shouldSatisfy` maybe False (T.isInfixOf "SyncLeaseUnavailable" . decodeUtf8 . LBS.toStrict . AE.encode)
            withResource tr.trATCtx.jobsPool (\conn -> PG.query_ conn "SELECT count(*) FROM background_jobs" :: IO [PG.Only Int]) `shouldReturn` [PG.Only 1]

      it "repositoryBulkExport_claimsOneLeasePerRepository" \tr -> do
        leases <- newIORef ([] :: [(Text, UUID.UUID)])
        for_ ["one", "two"] \repo -> do
          sync <- syncOf tr testPid Nothing "team" repo (GitSync.PersonalToken "fixture") ""
          for_ ["a.yaml", "b.yaml"] \path -> ownedDash tr sync path path
        let provider = withHTTPResponses \_ endpoint -> do
              owners <- runQueryEffect tr $ Hasql.interp @[(Text, UUID.UUID)] [HI.sql| SELECT s.repo, l.owner FROM projects.repository_sync_leases l JOIN projects.git_sync s ON s.id = l.sync_id |]
              modifyIORef' leases (<> owners)
              pure $ Just $ if "/contents/" `T.isInfixOf` toText endpoint then "{\"content\":{\"sha\":\"written\"}}" else "{\"sha\":\"head\"}"
        length <$> runJob tr provider (BackgroundJobs.GitSyncPushAllDashboards testPid) `shouldReturn` 6
        length . ordNub <$> readIORef leases `shouldReturn` 2

    describe "Migrations" do
      it "githubIdentityMigration_mergesCaseAliasesWithoutDeletingLocalDashboards" \tr -> do
        account <- appAccount tr "team"
        alias <- grantOf tr testPid Git.GitHub Nothing "other" Nothing (Just "fixture")
        for_ ([(account.id, "team"), (alias.id, "other")] :: [(GitSync.GitHubCredentialId, Text)]) \(cid, owner) ->
          runQueryEffect tr $ CodeContext.insertCodeMapping testPid cid (Git.RepoRef owner "checkout" "main") (Just owner) ("/" <> owner <> "/") "src"
        syncs <- forM (["team", "other"] :: [Text]) \owner -> do
          sync <- syncOf tr testPid Nothing owner "checkout" (GitSync.AppInstallation 42) ""
          sync <$ seedDash tr \d -> d{Dashboards.title = owner, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "overview.yaml", Dashboards.fileSha = Just "blob"}
        migration <- Query <$> readFileBS "static/migrations/0223_github_repository_identity.sql"
        -- Recreate the pre-migration case aliases the canonical-name constraints now forbid.
        withResource tr.trPool \conn -> PG.withTransaction conn do
          void $ PG.execute_ conn "ALTER TABLE projects.git_credentials DROP CONSTRAINT git_credentials_canonical_account; ALTER TABLE projects.repositories DROP CONSTRAINT repositories_canonical_name; ALTER TABLE projects.git_sync DROP CONSTRAINT git_sync_canonical_name; DROP FUNCTION projects.git_identity_name(TEXT, TEXT)"
          void $ PG.execute conn "UPDATE projects.git_credentials SET account = 'TEAM' WHERE id = ?" (PG.Only alias.id)
          void $ PG.execute conn "UPDATE projects.repositories SET owner = 'TEAM', repo = 'CHECKOUT' WHERE project_id = ? AND owner = 'other'" (PG.Only testPid)
          void $ PG.execute conn "UPDATE projects.git_sync SET owner = 'TEAM', repo = 'CHECKOUT' WHERE project_id = ? AND owner = 'other'" (PG.Only testPid)
          void $ PG.execute conn "UPDATE projects.code_mappings SET owner = 'TEAM', repo = 'CHECKOUT' WHERE credential_id = ?" (PG.Only alias.id)
          void $ PG.execute_ conn migration
        map (.id) <$> runQueryEffect tr (GitSync.getGitHubCredentials testPid) `shouldReturn` [account.id]
        map (\r -> (r.owner, r.repo, r.credentialId)) <$> runQueryEffect tr (GitSync.getRepositories testPid) `shouldReturn` [("team", "checkout", Just account.id)]
        map (\m -> (m.owner, m.repo, m.credentialId)) <$> runQueryEffect tr (CodeContext.getCodeMappings testPid) `shouldReturn` replicate 2 ("team", "checkout", account.id)
        length <$> runQueryEffect tr (GitSync.getGitSyncs testPid) `shouldReturn` 1
        retained <- allDashboards tr
        sort (map (.title) retained) `shouldBe` ["other", "team"]
        length (filter (isJust . (.gitSyncId)) retained) `shouldBe` 1
        mapMaybe (.gitSyncId) retained `shouldSatisfy` all (`elem` map (.id) syncs)

      it "repositoryMigration_normalizesLegacyPathsAndRetainsCollidingDashboards" \tr -> do
        sync <- syncOf tr testPid Nothing "team" "dashboards" (GitSync.AppInstallation 42) "ops"
        dashboards <- forM (zip [1 :: Int ..] ["overview.yaml", "dashboards/overview.yaml", "ops/dashboards/overview.yaml"]) \(n, path) ->
          seedDash tr \d -> d{Dashboards.title = "Retained overview", Dashboards.updatedAt = addUTCTime (fromIntegral n) frozenTime, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just path, Dashboards.fileSha = Just "blob"}
        migration <- Query <$> readFileBS "static/migrations/0216_repository_relative_paths.sql"
        dbExec tr migration ()
        retained <- traverse (dashOf tr) dashboards
        map (fmap (\d -> (d.title, d.gitSyncId, d.filePath, d.fileSha))) retained
          `shouldBe` [Just ("Retained overview", Nothing, Nothing, Nothing), Just ("Retained overview", Nothing, Nothing, Nothing), Just ("Retained overview", Just sync.id, Just "overview.yaml", Just "blob")]

      it "repositorySyncFailureMigration_preservesLegacyErrorsAsImportFailures" \tr -> do
        let connect owner = syncOf tr testPid Nothing owner "dashboards" (GitSync.PersonalToken "fixture") ""
        failed <- connect "failed"
        healthy <- connect "healthy"
        dbExec tr "ALTER TABLE projects.git_sync ALTER COLUMN last_error TYPE TEXT USING last_error->>'message'" ()
        dbExec tr "UPDATE projects.git_sync SET last_error = 'Legacy provider failure' WHERE id = ?" (Only failed.id)
        migration <- Query <$> readFileBS "static/migrations/0222_repository_sync_failures.sql"
        dbExec tr migration ()
        (.lastError) <$> syncById tr failed `shouldReturn` Just (GitSync.SyncFailure GitSync.ImportDashboards "Legacy provider failure")
        (.lastError) <$> syncById tr healthy `shouldReturn` Nothing
