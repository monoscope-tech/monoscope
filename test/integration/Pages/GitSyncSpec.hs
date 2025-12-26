module Pages.GitSyncSpec (spec) where

import BackgroundJobs qualified
import Control.Lens ((.~), (^.), (^?))
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
import Models.Users.Users qualified as Users
import Network.HTTP.Client (HttpException)
import Network.Wreq qualified as Wreq
import Pages.GitSync (GitHubOwner (..), GitHubRepo (..), GitHubWebhookPayload (..), GitSyncForm (..))
import Pages.GitSync qualified as GitSyncPage
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude hiding (head)
import Relude.Unsafe (head)
import System.Types (atAuthToBase)
import Test.Hspec
import UnliftIO.Exception (try)


testPid :: Projects.ProjectId
testPid = UUIDId UUID.nil


isGitSyncPush, isGitSyncFromRepo, isGitSyncPushAll :: (a, BackgroundJobs.BgJobs) -> Bool
isGitSyncPush (_, BackgroundJobs.GitSyncPushDashboard{}) = True
isGitSyncPush _ = False
isGitSyncFromRepo (_, BackgroundJobs.GitSyncFromRepo{}) = True
isGitSyncFromRepo _ = False
isGitSyncPushAll (_, BackgroundJobs.GitSyncPushAllDashboards{}) = True
isGitSyncPushAll _ = False


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
        { owner = cfg.owner
        , repo = cfg.repo
        , branch = cfg.branch
        , accessToken = cfg.pat
        , pathPrefix = prefixM
        }
  void $ testServant tr $ GitSyncPage.gitSyncSettingsPostH testPid form

clearJobs :: TestResources -> IO ()
clearJobs tr = liftIO $ withResource tr.trPool \conn -> do
  void $ execute conn [sql|DELETE FROM background_jobs WHERE payload->>'tag' LIKE 'GitSync%'|] ()

clearAllTestData :: TestResources -> IO ()
clearAllTestData tr = liftIO $ withResource tr.trPool \conn -> do
  void $ execute conn [sql|DELETE FROM background_jobs WHERE payload->>'tag' LIKE 'GitSync%'|] ()
  void $ execute conn [sql|DELETE FROM projects.dashboards WHERE project_id = ?|] (Only testPid)
  void $ execute conn [sql|DELETE FROM projects.github_sync WHERE project_id = ?|] (Only testPid)

clearTestDashboards :: TestResources -> IO ()
clearTestDashboards tr = liftIO $ withResource tr.trPool \conn -> do
  void $ execute conn [sql|DELETE FROM projects.dashboards WHERE project_id = ?|] (Only testPid)

createDash :: TestResources -> Text -> [Text] -> IO Dashboards.DashboardId
createDash tr title tags = do
  now <- getCurrentTime
  dashId <- UUIDId <$> UUID.nextRandom
  let dash = Dashboards.DashboardVM
        { Dashboards.id = dashId
        , Dashboards.projectId = testPid
        , Dashboards.createdAt = now
        , Dashboards.updatedAt = now
        , Dashboards.createdBy = Users.UserId UUID.nil
        , Dashboards.baseTemplate = Nothing
        , Dashboards.schema = Just (def :: Dashboards.Dashboard){Dashboards.title = Just title, Dashboards.tags = Just tags}
        , Dashboards.starredSince = Nothing
        , Dashboards.homepageSince = Nothing
        , Dashboards.tags = V.fromList tags
        , Dashboards.title = title
        , Dashboards.teams = V.empty
        , Dashboards.filePath = Nothing
        , Dashboards.fileSha = Nothing
        }
  _ <- runTestBg tr $ Dashboards.insert dash
  pure dashId

runSyncJobs :: TestResources -> IO ()
runSyncJobs tr = do
  withResource tr.trPool setBjRunAtInThePast
  void $ runBackgroundJobsWhere tr.trATCtx isGitSyncJob
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
spec = do
  -- Layer 1: Unit/Integration tests (no external deps)
  aroundAll withTestResources do
    describe "GitHub Sync Settings" do
      it "creates sync config with PAT" \tr -> do
        let form = GitSyncForm{owner = "test-owner", repo = "test-repo", branch = "main", accessToken = "ghp_test", pathPrefix = Nothing}
        void $ testServant tr $ GitSyncPage.gitSyncSettingsPostH testPid form
        syncM <- runTestBg tr $ GitSync.getGitHubSync testPid
        syncM `shouldSatisfy` isJust
        let sync = fromJust syncM
        (sync.owner, sync.repo, sync.branch, sync.syncEnabled) `shouldBe` ("test-owner", "test-repo", "main", True)

      it "updates sync config" \tr -> do
        let form = GitSyncForm{owner = "updated", repo = "updated-repo", branch = "dev", accessToken = "ghp_new", pathPrefix = Nothing}
        void $ testServant tr $ GitSyncPage.gitSyncSettingsPostH testPid form
        syncM <- runTestBg tr $ GitSync.getGitHubSync testPid
        (fromJust syncM).owner `shouldBe` "updated"

      it "deletes sync config" \tr -> do
        void $ testServant tr $ GitSyncPage.gitSyncSettingsDeleteH testPid
        syncM <- runTestBg tr $ GitSync.getGitHubSync testPid
        syncM `shouldSatisfy` isNothing

    describe "Sync Plan Building" do
      it "creates for new files" \_ -> do
        let entries = [GitSync.TreeEntry "dashboards/new.yaml" "blob" "sha1" (Just 100), GitSync.TreeEntry "dashboards/other.yaml" "blob" "sha2" (Just 100)]
            plan = GitSync.buildSyncPlan "dashboards/" entries M.empty
        length [() | GitSync.SyncCreate{} <- plan] `shouldBe` 2

      it "updates for changed SHAs" \_ -> do
        let did = UUIDId [uuid|11111111-1111-1111-1111-111111111111|]
            entries = [GitSync.TreeEntry "dashboards/x.yaml" "blob" "newsha" (Just 100)]
            plan = GitSync.buildSyncPlan "dashboards/" entries (one ("dashboards/x.yaml", (did, "oldsha")))
        length [() | GitSync.SyncUpdate{} <- plan] `shouldBe` 1

      it "deletes for removed files" \_ -> do
        let did = UUIDId [uuid|22222222-2222-2222-2222-222222222222|]
            plan = GitSync.buildSyncPlan "dashboards/" [] (one ("dashboards/gone.yaml", (did, "sha")))
        length [() | GitSync.SyncDelete{} <- plan] `shouldBe` 1

      it "ignores non-dashboard files" \_ -> do
        let entries = [GitSync.TreeEntry "src/main.hs" "blob" "s1" Nothing, GitSync.TreeEntry "dashboards/ok.yaml" "blob" "s2" Nothing]
            plan = GitSync.buildSyncPlan "dashboards/" entries M.empty
        length plan `shouldBe` 1

      it "supports .yml extension" \_ -> do
        let entries = [GitSync.TreeEntry "dashboards/test.yml" "blob" "sha" Nothing]
            plan = GitSync.buildSyncPlan "dashboards/" entries M.empty
        length plan `shouldBe` 1

      it "respects path prefix" \_ -> do
        let entries = [GitSync.TreeEntry "app/dashboards/x.yaml" "blob" "sha" Nothing, GitSync.TreeEntry "dashboards/y.yaml" "blob" "sha2" Nothing]
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
        let schema = (def :: Dashboards.Dashboard){Dashboards.title = Just "My Dash", Dashboards.tags = Just ["a", "b"]}
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
        _ <- runTestBg tr $ GitSync.updateDashboardGitInfo dashId "dashboards/test.yaml" "abc123"
        dashM <- runTestBg tr $ Dashboards.getDashboardById dashId
        (fromJust dashM).filePath `shouldBe` Just "dashboards/test.yaml"
        (fromJust dashM).fileSha `shouldBe` Just "abc123"

      it "gets git state for project" \tr -> do
        d1 <- createDash tr "State A" []
        d2 <- createDash tr "State B" []
        _ <- runTestBg tr $ GitSync.updateDashboardGitInfo d1 "dashboards/a.yaml" "sha-a"
        _ <- runTestBg tr $ GitSync.updateDashboardGitInfo d2 "dashboards/b.yaml" "sha-b"
        gitState <- runTestBg tr $ GitSync.getDashboardGitState testPid
        M.lookup "dashboards/a.yaml" gitState `shouldBe` Just (d1, "sha-a")
        M.lookup "dashboards/b.yaml" gitState `shouldBe` Just (d2, "sha-b")

    describe "Job Queuing" do
      it "queues push job when sync enabled" \tr -> do
        setupSync tr GitHubTestConfig{pat = "fake", owner = "o", repo = "r", branch = "main"} Nothing
        clearJobs tr
        dashId <- liftIO UUID.nextRandom
        _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid dashId (Just "dashboards/x.yaml")
        jobs <- getPendingBackgroundJobs tr.trATCtx
        V.length (V.filter isGitSyncPush jobs) `shouldSatisfy` (>= 1)

      it "skips push when sync disabled" \tr -> do
        void $ testServant tr $ GitSyncPage.gitSyncSettingsDeleteH testPid
        clearJobs tr
        dashId <- liftIO UUID.nextRandom
        _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid dashId Nothing
        jobs <- getPendingBackgroundJobs tr.trATCtx
        V.length (V.filter isGitSyncPush jobs) `shouldBe` 0

    describe "Webhook Handler" do
      it "queues sync on push event" \tr -> do
        setupSync tr GitHubTestConfig{pat = "fake", owner = "webhook-owner", repo = "webhook-repo", branch = "main"} Nothing
        clearJobs tr
        let payload = GitHubWebhookPayload
              { ref = Just "refs/heads/main"
              , repository = Just $ GitHubRepo "webhook-owner/webhook-repo" "webhook-repo" (GitHubOwner "webhook-owner" Nothing)
              , pusher = Nothing, commits = Nothing
              }
        _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ GitSyncPage.githubWebhookPostH Nothing (Just "push") (toStrict $ AE.encode payload)
        jobs <- getPendingBackgroundJobs tr.trATCtx
        V.length (V.filter isGitSyncFromRepo jobs) `shouldSatisfy` (>= 1)

      it "ignores non-push events" \tr -> do
        clearJobs tr
        let payload = GitHubWebhookPayload{ref = Nothing, repository = Just $ GitHubRepo "o/r" "r" (GitHubOwner "o" Nothing), pusher = Nothing, commits = Nothing}
        _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ GitSyncPage.githubWebhookPostH Nothing (Just "ping") (toStrict $ AE.encode payload)
        jobs <- getPendingBackgroundJobs tr.trATCtx
        V.length (V.filter isGitSyncFromRepo jobs) `shouldBe` 0

      it "ignores untracked repos" \tr -> do
        clearJobs tr
        let payload = GitHubWebhookPayload{ref = Just "refs/heads/main", repository = Just $ GitHubRepo "unknown/repo" "repo" (GitHubOwner "unknown" Nothing), pusher = Nothing, commits = Nothing}
        _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ GitSyncPage.githubWebhookPostH Nothing (Just "push") (toStrict $ AE.encode payload)
        jobs <- getPendingBackgroundJobs tr.trATCtx
        V.length (V.filter isGitSyncFromRepo jobs) `shouldBe` 0

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
            _ <- runTestBg tr $ GitSync.updateDashboardGitInfo dashId path ""  -- set path, empty sha = new file
            _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid (coerce dashId) (Just path)
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
            _ <- runTestBg tr $ GitSync.updateDashboardGitInfo dashId path sha1
            clearJobs tr
            _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid (coerce dashId) (Just path)
            runSyncJobs tr
            (content, _) <- fromRightShow <$> ghGetFile cfg path
            BS.isInfixOf "Updated Title" content `shouldBe` True

          it "stores filePath and fileSha after push" \tr -> do
            clearAllTestData tr
            setupSync tr cfg Nothing
            dashId <- createDash tr "SHA Test" []
            let expectedPath = "dashboards/sha-test.yaml"
            _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid (coerce dashId) (Just expectedPath)
            runSyncJobs tr
            dashM <- runTestBg tr $ Dashboards.getDashboardById dashId
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
            dashes <- runTestBg tr $ Dashboards.selectDashboardsSortedBy testPid "title"
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
            dashes <- runTestBg tr $ Dashboards.selectDashboardsSortedBy testPid "title"
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
            dashesBefore <- runTestBg tr $ Dashboards.selectDashboardsSortedBy testPid "title"
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
            dashesAfter <- runTestBg tr $ Dashboards.selectDashboardsSortedBy testPid "title"
            let afterCount = length $ filter (\d -> d.filePath == Just path) dashesAfter
            afterCount `shouldBe` 0

        describe "Full Sync Cycles" $ do
          it "round-trips: local → push → modify in GitHub → pull" \tr -> do
            clearAllTestData tr
            setupSync tr cfg Nothing
            path <- uniquePath "roundtrip"
            -- Create and push
            dashId <- createDash tr "Round Trip Original" ["local"]
            _ <- runTestBg tr $ GitSync.updateDashboardGitInfo dashId path ""  -- set path for push
            _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid (coerce dashId) (Just path)
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
            dashes <- runTestBg tr $ Dashboards.selectDashboardsSortedBy testPid "title"
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
            _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid (coerce dashId) (Just path)
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
            _ <- runTestBg tr $ GitSync.updateDashboardGitInfo dashId path "wrong-sha-12345"
            _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid (coerce dashId) (Just path)
            -- Should not crash, just log error
            runSyncJobs tr
            -- Original file unchanged
            (content, _) <- fromRightShow <$> ghGetFile cfg path
            BS.isInfixOf "Original" content `shouldBe` True


