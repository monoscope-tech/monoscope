module Pages.GitSyncSpec (spec) where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Maybe (fromJust)
import Data.Map.Strict qualified as M
import Data.Pool (withResource)
import Data.Time (getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.Quasi (uuid)
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (execute)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import Pages.GitSync (GitHubOwner (..), GitHubRepo (..), GitHubWebhookPayload (..), GitSyncForm (..))
import Pages.GitSync qualified as GitSyncPage
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import System.Types (atAuthToBase)
import Test.Hspec


testPid :: Projects.ProjectId
testPid = UUIDId UUID.nil


isGitSyncPush :: (a, BackgroundJobs.BgJobs) -> Bool
isGitSyncPush (_, job) = case job of
  BackgroundJobs.GitSyncPushDashboard{} -> True
  _ -> False


isGitSyncFromRepo :: (a, BackgroundJobs.BgJobs) -> Bool
isGitSyncFromRepo (_, job) = case job of
  BackgroundJobs.GitSyncFromRepo{} -> True
  _ -> False


spec :: Spec
spec = aroundAll withTestResources do
  describe "GitHub Sync Settings" do
    it "should create GitHub sync configuration" \tr -> do
      let form = GitSyncForm
            { owner = "test-owner"
            , repo = "test-repo"
            , branch = "main"
            , accessToken = "ghp_test_token"
            }
      (_, _) <- testServant tr $ GitSyncPage.gitSyncSettingsPostH testPid form
      syncM <- runTestBg tr $ GitSync.getGitHubSync testPid
      syncM `shouldSatisfy` isJust
      let sync = fromJust syncM
      sync.owner `shouldBe` "test-owner"
      sync.repo `shouldBe` "test-repo"
      sync.branch `shouldBe` "main"
      sync.syncEnabled `shouldBe` True

    it "should update GitHub sync configuration" \tr -> do
      let form = GitSyncForm
            { owner = "updated-owner"
            , repo = "updated-repo"
            , branch = "develop"
            , accessToken = "ghp_updated_token"
            }
      (_, _) <- testServant tr $ GitSyncPage.gitSyncSettingsPostH testPid form
      syncM <- runTestBg tr $ GitSync.getGitHubSync testPid
      syncM `shouldSatisfy` isJust
      let sync = fromJust syncM
      sync.owner `shouldBe` "updated-owner"
      sync.repo `shouldBe` "updated-repo"
      sync.branch `shouldBe` "develop"

    it "should delete GitHub sync configuration" \tr -> do
      (_, _) <- testServant tr $ GitSyncPage.gitSyncSettingsDeleteH testPid
      syncM <- runTestBg tr $ GitSync.getGitHubSync testPid
      syncM `shouldSatisfy` isNothing

  describe "Git Sync Logic" do
    it "should build sync plan with creates for new files" \_ -> do
      let entries =
            [ GitSync.TreeEntry "dashboards/new.yaml" "blob" "sha123" (Just 100)
            , GitSync.TreeEntry "dashboards/another.yaml" "blob" "sha456" (Just 200)
            , GitSync.TreeEntry "README.md" "blob" "sha789" (Just 50)
            ]
          dbState = M.empty
          plan = GitSync.buildSyncPlan entries dbState
      length [() | GitSync.SyncCreate{} <- plan] `shouldBe` 2
      length [() | GitSync.SyncUpdate{} <- plan] `shouldBe` 0
      length [() | GitSync.SyncDelete{} <- plan] `shouldBe` 0

    it "should build sync plan with updates for changed SHAs" \_ -> do
      let dashId = UUIDId [uuid|11111111-1111-1111-1111-111111111111|]
          entries = [GitSync.TreeEntry "dashboards/existing.yaml" "blob" "newsha" (Just 100)]
          dbState = M.singleton "dashboards/existing.yaml" (dashId, "oldsha")
          plan = GitSync.buildSyncPlan entries dbState
      length [() | GitSync.SyncCreate{} <- plan] `shouldBe` 0
      length [() | GitSync.SyncUpdate{} <- plan] `shouldBe` 1
      length [() | GitSync.SyncDelete{} <- plan] `shouldBe` 0

    it "should build sync plan with deletes for removed files" \_ -> do
      let dashId = UUIDId [uuid|22222222-2222-2222-2222-222222222222|]
          entries = []
          dbState = M.singleton "dashboards/deleted.yaml" (dashId, "sha123")
          plan = GitSync.buildSyncPlan entries dbState
      length [() | GitSync.SyncCreate{} <- plan] `shouldBe` 0
      length [() | GitSync.SyncUpdate{} <- plan] `shouldBe` 0
      length [() | GitSync.SyncDelete{} <- plan] `shouldBe` 1

    it "should not include non-dashboard files in sync plan" \_ -> do
      let entries =
            [ GitSync.TreeEntry "src/main.hs" "blob" "sha1" (Just 100)
            , GitSync.TreeEntry "package.json" "blob" "sha2" (Just 50)
            , GitSync.TreeEntry "dashboards/valid.yaml" "blob" "sha3" (Just 200)
            ]
          plan = GitSync.buildSyncPlan entries M.empty
      length plan `shouldBe` 1
      case listToMaybe plan of
        Just (GitSync.SyncCreate path _) -> path `shouldBe` "dashboards/valid.yaml"
        _ -> fail "Expected SyncCreate action"

  describe "YAML Conversion" do
    it "should parse valid dashboard YAML" \_ -> do
      let yamlContent = "title: Test Dashboard\ntags:\n  - production\nwidgets: []\n"
      case GitSync.yamlToDashboard yamlContent of
        Left err -> fail $ toString err
        Right schema -> do
          schema.title `shouldBe` Just "Test Dashboard"
          schema.tags `shouldBe` Just ["production"]

    it "should convert dashboard to YAML" \_ -> do
      let schema = (def :: Dashboards.Dashboard){Dashboards.title = Just "My Dashboard", Dashboards.tags = Just ["test", "example"]}
          yamlBytes = GitSync.dashboardToYaml schema
      case GitSync.yamlToDashboard yamlBytes of
        Left err -> fail $ toString err
        Right parsed -> do
          parsed.title `shouldBe` Just "My Dashboard"
          parsed.tags `shouldBe` Just ["test", "example"]

  describe "Dashboard Git Info" do
    it "should update dashboard git file path and SHA" \tr -> do
      now <- liftIO getCurrentTime
      dashId <- UUIDId <$> liftIO UUID.nextRandom
      let dashboard = Dashboards.DashboardVM
            { Dashboards.id = dashId
            , Dashboards.projectId = testPid
            , Dashboards.createdAt = now
            , Dashboards.updatedAt = now
            , Dashboards.createdBy = Users.UserId UUID.nil
            , Dashboards.baseTemplate = Nothing
            , Dashboards.schema = Nothing
            , Dashboards.starredSince = Nothing
            , Dashboards.homepageSince = Nothing
            , Dashboards.tags = V.empty
            , Dashboards.title = "Git Test Dashboard"
            , Dashboards.teams = V.empty
            , Dashboards.filePath = Nothing
            , Dashboards.fileSha = Nothing
            }
      _ <- runTestBg tr $ Dashboards.insert dashboard
      _ <- runTestBg tr $ GitSync.updateDashboardGitInfo dashId "dashboards/test.yaml" "abc123sha"
      dashM <- runTestBg tr $ Dashboards.getDashboardById dashId
      dashM `shouldSatisfy` isJust
      let dash = fromJust dashM
      dash.filePath `shouldBe` Just "dashboards/test.yaml"
      dash.fileSha `shouldBe` Just "abc123sha"

    it "should get dashboard git state for project" \tr -> do
      now <- liftIO getCurrentTime
      dashId1 <- UUIDId <$> liftIO UUID.nextRandom
      dashId2 <- UUIDId <$> liftIO UUID.nextRandom
      let makeDash did title = Dashboards.DashboardVM
            { Dashboards.id = did
            , Dashboards.projectId = testPid
            , Dashboards.createdAt = now
            , Dashboards.updatedAt = now
            , Dashboards.createdBy = Users.UserId UUID.nil
            , Dashboards.baseTemplate = Nothing
            , Dashboards.schema = Nothing
            , Dashboards.starredSince = Nothing
            , Dashboards.homepageSince = Nothing
            , Dashboards.tags = V.empty
            , Dashboards.title = title
            , Dashboards.teams = V.empty
            , Dashboards.filePath = Nothing
            , Dashboards.fileSha = Nothing
            }
      _ <- runTestBg tr $ Dashboards.insert (makeDash dashId1 "Dashboard A")
      _ <- runTestBg tr $ Dashboards.insert (makeDash dashId2 "Dashboard B")
      _ <- runTestBg tr $ GitSync.updateDashboardGitInfo dashId1 "dashboards/a.yaml" "sha-a"
      _ <- runTestBg tr $ GitSync.updateDashboardGitInfo dashId2 "dashboards/b.yaml" "sha-b"
      gitState <- runTestBg tr $ GitSync.getDashboardGitState testPid
      M.lookup "dashboards/a.yaml" gitState `shouldBe` Just (dashId1, "sha-a")
      M.lookup "dashboards/b.yaml" gitState `shouldBe` Just (dashId2, "sha-b")

  describe "Outbound Sync (Dashboard Update -> Git Push)" do
    it "should queue git sync push when dashboard is updated with git sync enabled" \tr -> do
      let form = GitSyncForm
            { owner = "outbound-test"
            , repo = "sync-repo"
            , branch = "main"
            , accessToken = "ghp_outbound_test"
            }
      (_, _) <- testServant tr $ GitSyncPage.gitSyncSettingsPostH testPid form
      liftIO $ withResource tr.trPool \conn ->
        void $ execute conn [sql|DELETE FROM background_jobs WHERE payload->>'tag' = 'GitSyncPushDashboard'|] ()
      dashId <- liftIO UUID.nextRandom
      _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid dashId (Just "dashboards/custom.yaml")
      pendingJobs <- getPendingBackgroundJobs tr.trATCtx
      let gitPushJobs = V.filter isGitSyncPush pendingJobs
      V.length gitPushJobs `shouldSatisfy` (>= 1)

    it "should not queue git sync push when sync is disabled" \tr -> do
      (_, _) <- testServant tr $ GitSyncPage.gitSyncSettingsDeleteH testPid
      liftIO $ withResource tr.trPool \conn ->
        void $ execute conn [sql|DELETE FROM background_jobs WHERE payload->>'tag' = 'GitSyncPushDashboard'|] ()
      dashId <- liftIO UUID.nextRandom
      _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ atAuthToBase tr.trSessAndHeader $ GitSyncPage.queueGitSyncPush testPid dashId Nothing
      pendingJobs <- getPendingBackgroundJobs tr.trATCtx
      let gitPushJobs = V.filter isGitSyncPush pendingJobs
      V.length gitPushJobs `shouldBe` 0

  describe "Webhook Handler" do
    it "should create GitSyncFromRepo job on push event" \tr -> do
      let form = GitSyncForm
            { owner = "webhook-test"
            , repo = "webhook-repo"
            , branch = "main"
            , accessToken = "ghp_webhook_test"
            }
      (_, _) <- testServant tr $ GitSyncPage.gitSyncSettingsPostH testPid form
      liftIO $ withResource tr.trPool \conn ->
        void $ execute conn [sql|DELETE FROM background_jobs WHERE payload->>'tag' = 'GitSyncFromRepo'|] ()
      let payload = GitHubWebhookPayload
            { ref = Just "refs/heads/main"
            , repository = Just $ GitHubRepo "webhook-test/webhook-repo" "webhook-repo" (GitHubOwner "webhook-test" Nothing)
            , pusher = Nothing
            , commits = Nothing
            }
      _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ GitSyncPage.githubWebhookPostH Nothing (Just "push") (toStrict $ AE.encode payload)
      pendingJobs <- getPendingBackgroundJobs tr.trATCtx
      let gitSyncJobs = V.filter isGitSyncFromRepo pendingJobs
      V.length gitSyncJobs `shouldSatisfy` (>= 1)

    it "should ignore non-push events" \tr -> do
      liftIO $ withResource tr.trPool \conn ->
        void $ execute conn [sql|DELETE FROM background_jobs WHERE payload->>'tag' = 'GitSyncFromRepo'|] ()
      let payload = GitHubWebhookPayload
            { ref = Nothing
            , repository = Just $ GitHubRepo "webhook-test/webhook-repo" "webhook-repo" (GitHubOwner "webhook-test" Nothing)
            , pusher = Nothing
            , commits = Nothing
            }
      _ <- toBaseServantResponse tr.trATCtx tr.trLogger $ GitSyncPage.githubWebhookPostH Nothing (Just "ping") (toStrict $ AE.encode payload)
      pendingJobs <- getPendingBackgroundJobs tr.trATCtx
      let gitSyncJobs = V.filter isGitSyncFromRepo pendingJobs
      V.length gitSyncJobs `shouldBe` 0
