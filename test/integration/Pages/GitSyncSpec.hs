module Pages.GitSyncSpec (spec) where

import BackgroundJobs qualified
import Control.Lens ((.~), (^.), (^?))
import Data.Generics.Product.Fields (field)
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _String)
import Data.ByteString qualified as BS
import Data.Base64.Types (extractBase64)
import Data.ByteString.Base64 qualified as B64
import Data.Default (Default (..), def)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.Quasi (uuid)
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Only (..), execute)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import OddJobs.Job (createJob)
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Client (HttpException)
import Network.Wreq qualified as Wreq
import Pages.GitSync (GitSyncForm (..))
import Pkg.Git qualified as Git
import System.Config (AuthContext (..), EnvConfig (..))
import Effectful (runEff)
import Data.Effectful.Wreq qualified as W
import Data.ByteString.Base16 qualified as B16
import Data.ByteArray qualified as BA
import "cryptonite" Crypto.Hash.Algorithms (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC
import Pages.GitSync qualified as GitSyncPage
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude hiding (head)
import Test.Hspec
import UnliftIO.Exception (try)



isGitSyncPush, isGitSyncFromRepo :: (a, BackgroundJobs.BgJobs) -> Bool
isGitSyncPush (_, BackgroundJobs.GitSyncPushDashboard{}) = True
isGitSyncPush _ = False
isGitSyncFromRepo (_, BackgroundJobs.GitSyncFromRepo{}) = True
isGitSyncFromRepo _ = False


-- ═══════════════════════════════════════════════════════════════════════════
-- E2E Test Configuration (Real GitHub API)
-- ═══════════════════════════════════════════════════════════════════════════

data GitHubTestConfig = GitHubTestConfig
  { pat :: Text
  , owner :: Text
  , repo :: Text
  , branch :: Text
  }

loadTestConfig :: IO (Maybe GitHubTestConfig)
loadTestConfig = do
  patM <- lookupEnv "GH_TEST_PAT"
  ownerM <- lookupEnv "GH_TEST_OWNER"
  repoM <- lookupEnv "GH_TEST_REPO"
  branchM <- lookupEnv "GH_TEST_BRANCH"
  pure $ (GitHubTestConfig . toText <$> patM)
    <*> (toText <$> ownerM)
    <*> (toText <$> repoM)
    <*> pure (maybe "main" toText branchM)


-- ═══════════════════════════════════════════════════════════════════════════
-- GitHub API Helpers
-- ═══════════════════════════════════════════════════════════════════════════

ghOpts :: Text -> Wreq.Options
ghOpts token = Wreq.defaults
  & Wreq.header "Authorization" .~ ["Bearer " <> encodeUtf8 token]
  & Wreq.header "Accept" .~ ["application/vnd.github+json"]
  & Wreq.header "User-Agent" .~ ["Monoscope-Test"]
  & Wreq.header "X-GitHub-Api-Version" .~ ["2022-11-28"]

ghUrl :: GitHubTestConfig -> Text -> String
ghUrl cfg path = toString $ "https://api.github.com/repos/" <> cfg.owner <> "/" <> cfg.repo <> "/contents/" <> path <> "?ref=" <> cfg.branch

-- | Create or update file in GitHub, returns (sha, created)
ghPutFile :: GitHubTestConfig -> Text -> ByteString -> Maybe Text -> IO (Either Text Text)
ghPutFile cfg path content existingSha = do
  let url = ghUrl cfg path
      payload = AE.object $ catMaybes
        [ Just $ "message" AE..= ("Test: " <> path)
        , Just $ "content" AE..= (extractBase64 $ B64.encodeBase64 content :: Text)
        , Just $ "branch" AE..= cfg.branch
        , ("sha" AE..=) <$> existingSha
        ]
  result <- try $ Wreq.putWith (ghOpts cfg.pat) url payload
  pure $ case result of
    Left (e :: HttpException) -> Left $ "HTTP error: " <> show e
    Right resp -> maybeToRight "No SHA in response" $ resp ^? Wreq.responseBody . key "content" . key "sha" . _String

-- | Get file content and SHA from GitHub
ghGetFile :: GitHubTestConfig -> Text -> IO (Either Text (ByteString, Text))
ghGetFile cfg path = do
  result <- try $ Wreq.getWith (ghOpts cfg.pat) (ghUrl cfg path)
  pure $ case result of
    Left (e :: HttpException) -> Left $ "HTTP error: " <> show e
    Right resp -> do
      let body = resp ^. Wreq.responseBody
      sha <- maybeToRight "No SHA" $ body ^? key "sha" . _String
      b64 <- maybeToRight "No content" $ body ^? key "content" . _String
      content <- first (toText . show) $ B64.decodeBase64Untyped $ encodeUtf8 $ T.filter (/= '\n') b64
      Right (content, sha)

-- | Delete file from GitHub
ghDeleteFile :: GitHubTestConfig -> Text -> Text -> IO (Either Text ())
ghDeleteFile cfg path sha = do
  let url = ghUrl cfg path
      payload = AE.object ["message" AE..= ("Delete: " <> path), "sha" AE..= sha, "branch" AE..= cfg.branch]
  result <- try $ Wreq.customPayloadMethodWith "DELETE" (ghOpts cfg.pat) url payload
  pure $ bimap (toText . show @_ @HttpException) (const ()) result

-- | Check if file exists
ghFileExists :: GitHubTestConfig -> Text -> IO Bool
ghFileExists cfg path = isRight <$> ghGetFile cfg path

-- | List files in a directory
ghListFiles :: GitHubTestConfig -> Text -> IO [Text]
ghListFiles cfg dir = do
  result <- try $ Wreq.getWith (ghOpts cfg.pat) (ghUrl cfg dir)
  pure $ case result of
    Left (_ :: HttpException) -> []
    Right resp -> case AE.decode (resp ^. Wreq.responseBody) of
      Just (files :: [AE.Value]) -> mapMaybe (^? key "name" . _String) files
      Nothing -> []

-- | Delete all files in dashboards/ for cleanup
ghCleanupDashboards :: GitHubTestConfig -> IO ()
ghCleanupDashboards cfg = do
  files <- ghListFiles cfg "dashboards"
  forM_ files \name -> do
    let path = "dashboards/" <> name
    result <- ghGetFile cfg path
    case result of
      Right (_, sha) -> void $ ghDeleteFile cfg path sha
      Left _ -> pass


-- ═══════════════════════════════════════════════════════════════════════════
-- Test Helpers
-- ═══════════════════════════════════════════════════════════════════════════

setupSync :: TestResources -> GitHubTestConfig -> Maybe Text -> IO ()
setupSync tr cfg prefixM = do
  let form = GitSyncForm
        { host = Just Git.GitHub
        , apiBase = Nothing
        , owner = cfg.owner
        , repo = cfg.repo
        , branch = cfg.branch
        , accessToken = cfg.pat
        , webhookSecret = Nothing
        , pathPrefix = prefixM
        }
  void $ testServant tr $ GitSyncPage.gitSyncSettingsPostH testPid form

-- | Connect a sync row on a given host, so a webhook test has a row to resolve against.
setupSyncOn :: TestResources -> Git.GitHost -> Text -> Text -> Maybe Text -> IO ()
setupSyncOn tr host owner repo secret = do
  -- One sync row per project, so each host under test replaces the last.
  void $ testServant tr $ GitSyncPage.gitSyncSettingsDeleteH testPid
  void $ testServant tr $ GitSyncPage.gitSyncSettingsPostH testPid
    GitSyncForm
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


clearJobs :: TestResources -> IO ()
clearJobs tr = liftIO $ withResource tr.trPool \conn -> do
  void $ execute conn [sql|DELETE FROM background_jobs WHERE payload->>'tag' LIKE 'GitSync%'|] ()

clearAllTestData :: TestResources -> IO ()
clearAllTestData tr = liftIO $ withResource tr.trPool \conn -> do
  void $ execute conn [sql|DELETE FROM background_jobs WHERE payload->>'tag' LIKE 'GitSync%'|] ()
  void $ execute conn [sql|DELETE FROM projects.dashboards WHERE project_id = ?|] (Only testPid)
  void $ execute conn [sql|DELETE FROM projects.github_sync WHERE project_id = ?|] (Only testPid)

createDash :: TestResources -> Text -> [Text] -> IO Dashboards.DashboardId
createDash tr title tags = do
  now <- getCurrentTime
  dashId <- UUIDId <$> UUID.nextRandom
  let dash = Dashboards.DashboardVM
        { Dashboards.id = dashId
        , Dashboards.projectId = testPid
        , Dashboards.createdAt = now
        , Dashboards.updatedAt = now
        , Dashboards.createdBy = Projects.UserId UUID.nil
        , Dashboards.baseTemplate = Nothing
        , Dashboards.schema = Just $ (def :: Dashboards.Dashboard) & field @"title" .~ Just title & field @"tags" .~ Just tags
        , Dashboards.starredSince = Nothing
        , Dashboards.homepageSince = Nothing
        , Dashboards.tags = V.fromList tags
        , Dashboards.title = title
        , Dashboards.teams = V.empty
        , Dashboards.filePath = Nothing
        , Dashboards.fileSha = Nothing
        }
  _ <- runTestBg frozenTime tr $ Dashboards.insert dash
  pure dashId

runSyncJobs :: TestResources -> IO ()
runSyncJobs tr = do
  withResource tr.trPool $ setBjRunAtInThePast frozenTime
  void $ runBackgroundJobsWhere frozenTime tr.trATCtx isGitSyncJob
  where
    isGitSyncJob BackgroundJobs.GitSyncPushDashboard{} = True
    isGitSyncJob BackgroundJobs.GitSyncFromRepo{} = True
    isGitSyncJob BackgroundJobs.GitSyncPushAllDashboards{} = True
    isGitSyncJob _ = False

uniquePath :: Text -> IO Text
uniquePath base = do
  uid <- UUID.nextRandom
  pure $ "dashboards/" <> base <> "-" <> T.take 8 (UUID.toText uid) <> ".yaml"


-- ═══════════════════════════════════════════════════════════════════════════
-- SPEC
-- ═══════════════════════════════════════════════════════════════════════════

spec :: Spec
-- Uses aroundAll + beforeAll (shared resource across examples), so it can't use the
-- per-test isolation the rest of the suite relies on for parallelism. Run sequentially.
spec = sequential do
  liveSmokeSpec
  -- Layer 1: Unit/Integration tests (no external deps)
  aroundAll withTestResources do
    describe "GitHub Sync Settings" do
      it "connects, updates, and disconnects a repository" \tr -> do
        let form owner repo branch token = GitSyncForm{host = Just Git.GitHub, apiBase = Nothing, owner, repo, branch, accessToken = token, webhookSecret = Just "s3cret", pathPrefix = Nothing}
        void $ testServant tr $ GitSyncPage.gitSyncSettingsPostH testPid (form "test-owner" "test-repo" "main" "ghp_test")
        sync <- maybe (fail "Git connection was not created") pure =<< runTestBg frozenTime tr (GitSync.getGitHubSync testPid)
        (sync.owner, sync.repo, sync.branch, sync.syncEnabled) `shouldBe` ("test-owner", "test-repo", "main", True)

        void $ testServant tr $ GitSyncPage.gitSyncSettingsPostH testPid (form "updated" "updated-repo" "dev" "ghp_new")
        updated <- maybe (fail "Git connection disappeared after update") pure =<< runTestBg frozenTime tr (GitSync.getGitHubSync testPid)
        (updated.owner, updated.repo, updated.branch) `shouldBe` ("updated", "updated-repo", "dev")

        void $ testServant tr $ GitSyncPage.gitSyncSettingsDeleteH testPid
        runTestBg frozenTime tr (GitSync.getGitHubSync testPid) >>= (`shouldSatisfy` isNothing)

    describe "Sync Plan Building" do
      it "creates for new files" \_ -> do
        let entries = [GitSync.TreeEntry "dashboards/new.yaml" True (Just "sha1") (Just 100), GitSync.TreeEntry "dashboards/other.yaml" True (Just "sha2") (Just 100)]
            plan = GitSync.buildSyncPlan "dashboards/" entries M.empty
        length [() | GitSync.SyncCreate{} <- plan] `shouldBe` 2

      it "updates for changed SHAs" \_ -> do
        let did = UUIDId [uuid|11111111-1111-1111-1111-111111111111|]
            entries = [GitSync.TreeEntry "dashboards/x.yaml" True (Just "newsha") (Just 100)]
            plan = GitSync.buildSyncPlan "dashboards/" entries (one ("x.yaml", (did, "oldsha")))
        length [() | GitSync.SyncUpdate{} <- plan] `shouldBe` 1

      it "deletes for removed files" \_ -> do
        let did = UUIDId [uuid|22222222-2222-2222-2222-222222222222|]
            plan = GitSync.buildSyncPlan "dashboards/" [] (one ("gone.yaml", (did, "sha")))
        length [() | GitSync.SyncDelete{} <- plan] `shouldBe` 1

      it "ignores non-dashboard files" \_ -> do
        let entries = [GitSync.TreeEntry "src/main.hs" True (Just "s1") Nothing, GitSync.TreeEntry "dashboards/ok.yaml" True (Just "s2") Nothing]
            plan = GitSync.buildSyncPlan "dashboards/" entries M.empty
        length plan `shouldBe` 1

      it "supports .yml extension" \_ -> do
        let entries = [GitSync.TreeEntry "dashboards/test.yml" True (Just "sha") Nothing]
            plan = GitSync.buildSyncPlan "dashboards/" entries M.empty
        length plan `shouldBe` 1

      it "respects path prefix" \_ -> do
        let entries = [GitSync.TreeEntry "app/dashboards/x.yaml" True (Just "sha") Nothing, GitSync.TreeEntry "dashboards/y.yaml" True (Just "sha2") Nothing]
            plan = GitSync.buildSyncPlan "app/dashboards/" entries M.empty
        length plan `shouldBe` 1
        case plan of [GitSync.SyncCreate p _] -> p `shouldBe` "app/dashboards/x.yaml"; _ -> fail "wrong"

    describe "YAML Conversion" do
      it "parses valid YAML" \_ -> do
        let yaml = "title: Test\ntags:\n  - prod\nwidgets: []\n"
        case GitSync.yamlToDashboard yaml of
          Left e -> fail $ toString e
          Right s -> (s.title, s.tags) `shouldBe` (Just "Test", Just ["prod"])

      it "serializes dashboard to YAML" \_ -> do
        let schema = (def :: Dashboards.Dashboard) & field @"title" .~ Just "My Dash" & field @"tags" .~ Just (["a", "b"] :: [Text])
        case GitSync.yamlToDashboard (GitSync.dashboardToYaml schema) of
          Left e -> fail $ toString e
          Right s -> s.title `shouldBe` Just "My Dash"

      it "handles missing optional fields" \_ -> do
        let yaml = "title: Minimal\nwidgets: []\n"
        case GitSync.yamlToDashboard yaml of
          Left e -> fail $ toString e
          Right s -> s.title `shouldBe` Just "Minimal"

    describe "File Naming" do
      it "converts title to kebab-case" \_ -> do
        GitSync.titleToFilePath "My Dashboard" `shouldBe` "my-dashboard.yaml"
        GitSync.titleToFilePath "Performance Stats" `shouldBe` "performance-stats.yaml"
        GitSync.titleToFilePath "Test 123" `shouldBe` "test-123.yaml"

    describe "Path Helpers" do
      it "getDashboardsPath without prefix" \_ -> do
        let sync = def{GitSync.pathPrefix = ""}
        GitSync.getDashboardsPath sync `shouldBe` "dashboards/"

      it "getDashboardsPath with prefix" \_ -> do
        let sync = def{GitSync.pathPrefix = "myapp"}
        GitSync.getDashboardsPath sync `shouldBe` "myapp/dashboards/"

    describe "Dashboard Git Info" do
      it "updates filePath and fileSha" \tr -> do
        dashId <- createDash tr "Git Info Test" []
        _ <- runTestBg frozenTime tr $ GitSync.updateDashboardGitInfo dashId "dashboards/test.yaml" "abc123"
        dashM <- runTestBg frozenTime tr $ Dashboards.getDashboardByProjectId testPid dashId
        (fromJust dashM).filePath `shouldBe` Just "dashboards/test.yaml"
        (fromJust dashM).fileSha `shouldBe` Just "abc123"

      it "gets git state for project" \tr -> do
        d1 <- createDash tr "State A" []
        d2 <- createDash tr "State B" []
        _ <- runTestBg frozenTime tr $ GitSync.updateDashboardGitInfo d1 "dashboards/a.yaml" "sha-a"
        _ <- runTestBg frozenTime tr $ GitSync.updateDashboardGitInfo d2 "dashboards/b.yaml" "sha-b"
        gitState <- runTestBg frozenTime tr $ GitSync.getDashboardGitState testPid
        M.lookup "dashboards/a.yaml" gitState `shouldBe` Just (d1, "sha-a")
        M.lookup "dashboards/b.yaml" gitState `shouldBe` Just (d2, "sha-b")

    describe "Job Queuing" do
      it "queues push job when sync enabled" \tr -> do
        setupSync tr GitHubTestConfig{pat = "fake", owner = "o", repo = "r", branch = "main"} Nothing
        clearJobs tr
        dashId <- UUIDId <$> liftIO UUID.nextRandom
        _ <- toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid dashId
        jobs <- getPendingBackgroundJobs tr.trATCtx
        V.length (V.filter isGitSyncPush jobs) `shouldSatisfy` (>= 1)

      it "skips push when sync disabled" \tr -> do
        void $ testServant tr $ GitSyncPage.gitSyncSettingsDeleteH testPid
        clearJobs tr
        dashId <- UUIDId <$> liftIO UUID.nextRandom
        _ <- toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid dashId
        jobs <- getPendingBackgroundJobs tr.trATCtx
        V.length (V.filter isGitSyncPush jobs) `shouldBe` 0

    describe "Webhook Handler" do
      -- One body, signed the way each host signs it. The point of these is not the JSON — it
      -- is that a delivery only queues work when its signature checks out against the secret
      -- stored on the row, and that the host in the route has to match the row's host.
      let pushBody owner repo = toStrict $ AE.encode $ AE.object ["repository" AE..= AE.object ["full_name" AE..= (owner <> "/" <> repo :: Text)]]
          req _host ev sig body = Git.WebhookReq{event = Just ev, signature = sig, gitlabToken = Nothing, gitlabWebhookId = Nothing, gitlabTimestamp = Nothing, body = body}

      it "queues sync only when the signature verifies" \tr -> do
        setupSyncOn tr Git.GitHub "webhook-owner" "webhook-repo" (Just "s3cret")
        clearJobs tr
        let body = pushBody "webhook-owner" "webhook-repo"
            signed = "sha256=" <> hmacHexOf "s3cret" body

        -- Forged: right shape, wrong secret. Must not enqueue anything.
        _ <- toBaseServantResponse tr $ GitSyncPage.gitWebhookPostH Git.GitHub (req Git.GitHub "push" (Just "sha256=deadbeef") body)
        forged <- V.length . V.filter isGitSyncFromRepo <$> getPendingBackgroundJobs tr.trATCtx
        forged `shouldBe` 0

        -- Missing entirely: a row that holds a secret asked to be checked.
        _ <- toBaseServantResponse tr $ GitSyncPage.gitWebhookPostH Git.GitHub (req Git.GitHub "push" Nothing body)
        unsigned <- V.length . V.filter isGitSyncFromRepo <$> getPendingBackgroundJobs tr.trATCtx
        unsigned `shouldBe` 0

        _ <- toBaseServantResponse tr $ GitSyncPage.gitWebhookPostH Git.GitHub (req Git.GitHub "push" (Just signed) body)
        verified <- V.length . V.filter isGitSyncFromRepo <$> getPendingBackgroundJobs tr.trATCtx
        verified `shouldSatisfy` (>= 1)

      -- Why the announcement is recorded at all: the sync job decides "nothing changed" by
      -- comparing what it already has against the head the host reports at fetch time. When
      -- the host answers with a view older than the push it just announced, that comparison
      -- says "unchanged" and the job completes having done nothing — and nothing re-queues, so
      -- the push is never applied, only swept up by whatever push comes next. Measured on the
      -- demo project: 3 of 5 pushes applied, the two misses still unapplied seven minutes on.
      -- Keeping what the webhook said is what lets the job tell those two cases apart.
      it "records the head a push announced, so a stale fetch cannot look like no change" \tr -> do
        setupSyncOn tr Git.GitHub "webhook-owner" "webhook-repo" (Just "s3cret")
        let body =
              toStrict
                $ AE.encode
                $ AE.object
                  [ "repository" AE..= AE.object ["full_name" AE..= ("webhook-owner/webhook-repo" :: Text)]
                  , "after" AE..= ("f00dcafe" :: Text)
                  ]
        _ <- toBaseServantResponse tr $ GitSyncPage.gitWebhookPostH Git.GitHub (req Git.GitHub "push" (Just $ "sha256=" <> hmacHexOf "s3cret" body) body)
        announced <- fmap (>>= (.announcedRevision)) $ runQueryEffect tr $ GitSync.getGitHubSync testPid
        announced `shouldBe` Just "f00dcafe"

      -- The announcement has to outlive a pull that did not reach it, or the retry it exists
      -- to license is cleared by the very fetch that failed to satisfy it.
      it "keeps an announcement until the revision it names is actually fetched" \tr -> do
        setupSyncOn tr Git.GitHub "ann-owner" "ann-repo" (Just "s3cret")
        Just sync <- runQueryEffect tr $ GitSync.getGitHubSync testPid
        _ <- runQueryEffect tr $ GitSync.setAnnouncedRevision sync.id "newhead"

        -- A pull that landed on an older head: the announcement must survive it.
        _ <- runQueryEffect tr $ GitSync.updateLastRevision sync.id "oldhead"
        stillPending <- fmap (>>= (.announcedRevision)) $ runQueryEffect tr $ GitSync.getGitHubSync testPid
        stillPending `shouldBe` Just "newhead"

        -- The pull that finally reaches it retires it.
        _ <- runQueryEffect tr $ GitSync.updateLastRevision sync.id "newhead"
        settled <- runQueryEffect tr $ GitSync.getGitHubSync testPid
        (settled >>= (.announcedRevision)) `shouldBe` Nothing
        (settled >>= (.lastRevision)) `shouldBe` Just "newhead"

      it "verifies each host in its own dialect" \tr -> do
        forM_ [(Git.Gitea, "push" :: Text), (Git.Bitbucket, "repo:push")] \(host, ev) -> do
          setupSyncOn tr host "acme" "config" (Just "s3cret")
          clearJobs tr
          let body = pushBody "acme" "config"
              -- Gitea sends the bare hex digest; Bitbucket prefixes it like GitHub does.
              sig = (if host == Git.Bitbucket then "sha256=" else "") <> hmacHexOf "s3cret" body
          _ <- toBaseServantResponse tr $ GitSyncPage.gitWebhookPostH host (req host ev (Just sig) body)
          n <- V.length . V.filter isGitSyncFromRepo <$> getPendingBackgroundJobs tr.trATCtx
          n `shouldSatisfy` (>= 1)

      it "does not cross hosts on a repository-name collision" \tr -> do
        -- The same owner/repo on two hosts is two different projects' repositories; a push to
        -- one must not sync the row belonging to the other.
        setupSyncOn tr Git.GitLab "acme" "config" (Just "gl-secret")
        clearJobs tr
        let body = pushBody "acme" "config"
        -- Delivered on the Gitea route, signed with Gitea's scheme, but only a GitLab row
        -- exists: nothing should match.
        _ <- toBaseServantResponse tr $ GitSyncPage.gitWebhookPostH Git.Gitea (req Git.Gitea "push" (Just $ hmacHexOf "gl-secret" body) body)
        n <- V.length . V.filter isGitSyncFromRepo <$> getPendingBackgroundJobs tr.trATCtx
        n `shouldBe` 0

      it "ignores non-push events and untracked repos" \tr -> do
        setupSyncOn tr Git.GitHub "webhook-owner" "webhook-repo" (Just "s3cret")
        clearJobs tr
        let body = pushBody "webhook-owner" "webhook-repo"
        _ <- toBaseServantResponse tr $ GitSyncPage.gitWebhookPostH Git.GitHub (req Git.GitHub "ping" (Just $ "sha256=" <> hmacHexOf "s3cret" body) body)
        let other = pushBody "unknown" "repo"
        _ <- toBaseServantResponse tr $ GitSyncPage.gitWebhookPostH Git.GitHub (req Git.GitHub "push" (Just $ "sha256=" <> hmacHexOf "s3cret" other) other)
        n <- V.length . V.filter isGitSyncFromRepo <$> getPendingBackgroundJobs tr.trATCtx
        n `shouldBe` 0

  -- Layer 2: E2E tests with real GitHub API
  configM <- runIO loadTestConfig
  case configM of
    Nothing -> describe "GitHub Sync E2E" $
      it "SKIPPED - Set GH_TEST_PAT, GH_TEST_OWNER, GH_TEST_REPO to enable" pending

    Just cfg -> aroundAll withTestResources $ beforeAll_ (ghCleanupDashboards cfg) $ afterAll_ (ghCleanupDashboards cfg) $ do

      describe "GitHub Sync E2E (Real API)" $ do

        describe "Push: Local → GitHub" $ do
          it "creates file in GitHub when dashboard pushed" \tr -> do
            clearAllTestData tr
            setupSync tr cfg Nothing
            path <- uniquePath "push-create"
            dashId <- createDash tr "Push Create Test" ["e2e"]
            _ <- runTestBg frozenTime tr $ GitSync.updateDashboardGitInfo dashId path ""  -- set path, empty sha = new file
            _ <- toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid dashId
            runSyncJobs tr
            exists <- ghFileExists cfg path
            exists `shouldBe` True
            (content, _) <- fromRightShow <$> ghGetFile cfg path
            BS.isInfixOf "Push Create Test" content `shouldBe` True

          it "updates file in GitHub when dashboard changed" \tr -> do
            clearAllTestData tr
            setupSync tr cfg Nothing
            path <- uniquePath "push-update"
            -- Create initial file
            _ <- ghPutFile cfg path "title: Original\n" Nothing
            (_, sha1) <- fromRightShow <$> ghGetFile cfg path
            -- Create dashboard with that path
            dashId <- createDash tr "Updated Title" ["updated"]
            _ <- runTestBg frozenTime tr $ GitSync.updateDashboardGitInfo dashId path sha1
            clearJobs tr
            _ <- toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid dashId
            runSyncJobs tr
            (content, _) <- fromRightShow <$> ghGetFile cfg path
            BS.isInfixOf "Updated Title" content `shouldBe` True

          it "stores filePath and fileSha after push" \tr -> do
            clearAllTestData tr
            setupSync tr cfg Nothing
            dashId <- createDash tr "SHA Test" []
            let expectedPath = "dashboards/sha-test.yaml"
            _ <- toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid dashId
            runSyncJobs tr
            dashM <- runTestBg frozenTime tr $ Dashboards.getDashboardByProjectId testPid dashId
            let dash = fromJust dashM
            dash.filePath `shouldBe` Just expectedPath
            dash.fileSha `shouldSatisfy` isJust

        describe "Pull: GitHub → Local" $ do
          it "creates dashboard from GitHub YAML" \tr -> do
            clearAllTestData tr
            setupSync tr cfg Nothing
            path <- uniquePath "pull-create"
            let yaml = "title: Created In GitHub\ntags:\n  - github\n  - pull\nwidgets: []\n"
            _ <- ghPutFile cfg path (encodeUtf8 yaml) Nothing
            -- Trigger pull by simulating webhook
            liftIO $ withResource tr.trPool \conn ->
              void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncFromRepo testPid
            runSyncJobs tr
            dashes <- runTestBg frozenTime tr $ Dashboards.selectDashboardsSortedBy testPid "title"
            case filter (\d -> d.title == "Created In GitHub") dashes of
              (d:_) -> d.filePath `shouldBe` Just path
              [] -> fail "Expected dashboard not found"

          it "updates local dashboard when GitHub file changes" \tr -> do
            clearAllTestData tr
            setupSync tr cfg Nothing
            path <- uniquePath "pull-update"
            let yaml1 = "title: V1 Title\ntags: []\nwidgets: []\n"
            _ <- ghPutFile cfg path (encodeUtf8 yaml1) Nothing
            -- Initial pull
            clearJobs tr
            liftIO $ withResource tr.trPool \conn ->
              void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncFromRepo testPid
            runSyncJobs tr
            -- Update in GitHub
            (_, sha1) <- fromRightShow <$> ghGetFile cfg path
            let yaml2 = "title: V2 Updated\ntags: []\nwidgets: []\n"
            _ <- ghPutFile cfg path (encodeUtf8 yaml2) (Just sha1)
            -- Pull again
            clearJobs tr
            liftIO $ withResource tr.trPool \conn ->
              void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncFromRepo testPid
            runSyncJobs tr
            dashes <- runTestBg frozenTime tr $ Dashboards.selectDashboardsSortedBy testPid "title"
            any (\d -> d.title == "V2 Updated") dashes `shouldBe` True

          it "deletes local dashboard when GitHub file removed" \tr -> do
            clearAllTestData tr
            setupSync tr cfg Nothing
            path <- uniquePath "pull-delete"
            let yaml = "title: Will Be Deleted\nwidgets: []\n"
            _ <- ghPutFile cfg path (encodeUtf8 yaml) Nothing
            -- Initial pull
            clearJobs tr
            liftIO $ withResource tr.trPool \conn ->
              void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncFromRepo testPid
            runSyncJobs tr
            dashesBefore <- runTestBg frozenTime tr $ Dashboards.selectDashboardsSortedBy testPid "title"
            let beforeCount = length $ filter (\d -> d.filePath == Just path) dashesBefore
            beforeCount `shouldBe` 1
            -- Delete from GitHub
            (_, sha) <- fromRightShow <$> ghGetFile cfg path
            _ <- ghDeleteFile cfg path sha
            -- Pull again
            clearJobs tr
            liftIO $ withResource tr.trPool \conn ->
              void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncFromRepo testPid
            runSyncJobs tr
            dashesAfter <- runTestBg frozenTime tr $ Dashboards.selectDashboardsSortedBy testPid "title"
            let afterCount = length $ filter (\d -> d.filePath == Just path) dashesAfter
            afterCount `shouldBe` 0

        describe "Full Sync Cycles" $ do
          it "round-trips: local → push → modify in GitHub → pull" \tr -> do
            clearAllTestData tr
            setupSync tr cfg Nothing
            path <- uniquePath "roundtrip"
            -- Create and push
            dashId <- createDash tr "Round Trip Original" ["local"]
            _ <- runTestBg frozenTime tr $ GitSync.updateDashboardGitInfo dashId path ""  -- set path for push
            _ <- toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid dashId
            runSyncJobs tr
            -- Verify in GitHub
            (content1, sha1) <- fromRightShow <$> ghGetFile cfg path
            BS.isInfixOf "Round Trip Original" content1 `shouldBe` True
            -- Modify in GitHub
            let modified = "title: Round Trip Modified\ntags:\n  - local\n  - github-added\nwidgets: []\n"
            _ <- ghPutFile cfg path (encodeUtf8 modified) (Just sha1)
            -- Pull
            clearJobs tr
            liftIO $ withResource tr.trPool \conn ->
              void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncFromRepo testPid
            runSyncJobs tr
            -- Verify local updated - use filePath matching since dashboard is updated in place
            dashes <- runTestBg frozenTime tr $ Dashboards.selectDashboardsSortedBy testPid "title"
            case filter (\d -> d.filePath == Just path) dashes of
              (dash:_) -> do
                dash.title `shouldBe` "Round Trip Modified"
                V.toList dash.tags `shouldContain` ["github-added"]
              [] -> fail $ "Dashboard not found with path: " <> toString path

          it "pushes all dashboards on initial connect" \tr -> do
            -- Clear and create dashboards
            clearAllTestData tr
            _ <- createDash tr "Bulk A" []
            _ <- createDash tr "Bulk B" []
            _ <- createDash tr "Bulk C" []
            -- Connect (should queue push-all)
            setupSync tr cfg Nothing
            -- The setupSync also queues jobs, but we need to manually queue push-all for existing dashboards
            liftIO $ withResource tr.trPool \conn ->
              void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncPushAllDashboards testPid
            runSyncJobs tr
            -- Verify all in GitHub
            files <- ghListFiles cfg "dashboards"
            any ("bulk-a" `T.isInfixOf`) files `shouldBe` True
            any ("bulk-b" `T.isInfixOf`) files `shouldBe` True
            any ("bulk-c" `T.isInfixOf`) files `shouldBe` True

        describe "Path Prefix" $ do
          it "uses prefix when pushing" \tr -> do
            clearAllTestData tr
            setupSync tr cfg (Just "test-prefix")
            dashId <- createDash tr "Prefixed Push" []
            let path = "test-prefix/dashboards/prefixed-push.yaml"
            _ <- toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid dashId
            runSyncJobs tr
            exists <- ghFileExists cfg path
            exists `shouldBe` True
            -- Cleanup
            (_, sha) <- fromRightShow <$> ghGetFile cfg path
            void $ ghDeleteFile cfg path sha

        describe "Error Handling" $ do
          it "handles SHA mismatch gracefully" \tr -> do
            clearAllTestData tr
            setupSync tr cfg Nothing
            path <- uniquePath "sha-conflict"
            -- Create file
            _ <- ghPutFile cfg path "title: Original\n" Nothing
            -- Create dashboard with wrong SHA
            dashId <- createDash tr "Conflict Test" []
            _ <- runTestBg frozenTime tr $ GitSync.updateDashboardGitInfo dashId path "wrong-sha-12345"
            _ <- toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid dashId
            -- Should not crash, just log error
            runSyncJobs tr
            -- Original file unchanged
            (content, _) <- fromRightShow <$> ghGetFile cfg path
            BS.isInfixOf "Original" content `shouldBe` True




-- ═══════════════════════════════════════════════════════════════════════════
-- Live smoke test — any host, opt-in
-- ═══════════════════════════════════════════════════════════════════════════

-- | Coordinates for a real repository on a real host, from the environment.
--
-- Deliberately host-agnostic where the GitHub E2E block above is not: the thing that needs
-- proving against a live vendor is that "Pkg.Git" speaks its dialect, and that is the same
-- five operations everywhere. Set @GIT_SMOKE_HOST@ (github|gitlab|bitbucket|gitea),
-- @GIT_SMOKE_TOKEN@, @GIT_SMOKE_OWNER@, @GIT_SMOKE_REPO@, optionally @GIT_SMOKE_ORIGIN@ (the
-- server URL, required for Gitea) and @GIT_SMOKE_BRANCH@ (default @main@).
data SmokeConfig = SmokeConfig
  { conn :: Git.GitConn
  , repoRef :: Git.RepoRef
  }


loadSmokeConfig :: IO (Maybe SmokeConfig)
loadSmokeConfig = do
  hostM <- (>>= Git.parseHostSlug . toText) <$> lookupEnv "GIT_SMOKE_HOST"
  tokenM <- fmap toText <$> lookupEnv "GIT_SMOKE_TOKEN"
  ownerM <- fmap toText <$> lookupEnv "GIT_SMOKE_OWNER"
  repoM <- fmap toText <$> lookupEnv "GIT_SMOKE_REPO"
  origin <- fmap toText <$> lookupEnv "GIT_SMOKE_ORIGIN"
  branch <- maybe "main" toText <$> lookupEnv "GIT_SMOKE_BRANCH"
  pure do
    host <- hostM
    token <- tokenM
    owner <- ownerM
    repo' <- repoM
    c <- rightToMaybe $ Git.mkGitConn host origin token
    pure $ SmokeConfig c (Git.RepoRef owner repo' branch)


-- | One pass over every operation the sync path uses, against the real host.
--
-- This is the check the mock contract tests cannot make: that the URLs we build are URLs the
-- vendor recognises, and that its actual response shape decodes. It writes one file into the
-- configured repository under @monoscope-smoke\/@ and reads it back — so point it at a
-- scratch repository, not a real one.
--
-- **Never run in CI as written**: it needs a token with write access to a real repository.
liveSmokeSpec :: Spec
liveSmokeSpec = do
  cfgM <- runIO loadSmokeConfig
  case cfgM of
    Nothing ->
      describe "Live git host smoke test"
        $ it "SKIPPED - set GIT_SMOKE_HOST, GIT_SMOKE_TOKEN, GIT_SMOKE_OWNER, GIT_SMOKE_REPO to enable" pending
    Just cfg -> describe ("Live git host smoke test (" <> toString (Git.hostLabel cfg.conn.host) <> ")") do
      it "lists repositories" do
        r <- runEff $ W.runHTTPWreq $ Git.listRepos cfg.conn
        r `shouldSatisfy` isRight

      it "reports a default branch" do
        b <- runEff $ W.runHTTPWreq $ Git.defaultBranchOf cfg.conn cfg.repoRef
        b `shouldSatisfy` not . T.null

      it "round-trips a write, a listing and a read at the sha it reported" do
        let path = "monoscope-smoke/probe.yaml"
            body = "title: monoscope smoke probe\n"
        (sha, _rev) <- runEff (W.runHTTPWreq $ Git.pushFile cfg.conn cfg.repoRef path body Nothing "monoscope smoke test") >>= either (fail . toString) pure

        -- The sha we recorded must be the one a fresh listing reports, or `buildSyncPlan`
        -- will see a phantom change on the very next pull.
        (_, entries) <- runEff (W.runHTTPWreq $ Git.fetchTree cfg.conn cfg.repoRef "monoscope-smoke/") >>= either (fail . toString) pure
        find ((== path) . (.path)) entries `shouldSatisfy` \case
          Just e -> e.sha == Just sha
          Nothing -> False

        readBack <- runEff (W.runHTTPWreq $ Git.fetchFile cfg.conn cfg.repoRef path) >>= either (fail . toString) pure
        readBack `shouldBe` body
