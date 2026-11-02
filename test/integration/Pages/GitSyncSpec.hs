module Pages.GitSyncSpec (spec) where

import BackgroundJobs qualified
import Control.Lens ((.~), (^.), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _String)
import Data.Base64.Types (extractBase64)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Effectful.Wreq qualified as W
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (execute)
import Effectful (runEff)
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Client (HttpException)
import Network.Wreq qualified as Wreq
import OddJobs.Job (createJob)
import Pages.GitSync (GitSyncForm (..))
import Pages.GitSync qualified as GitSyncPage
import Pkg.Git qualified as Git
import Pkg.TestUtils
import Test.Hspec (afterAll_, beforeAll_, pending)
import UnliftIO.Exception (try)


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

-- | Cheap preflight: can this PAT read the target repo at all? Distinguishes
-- "credential broken" from "sync code broken", which a 401 inside a sync
-- assertion cannot.
ghCredentialUsable :: GitHubTestConfig -> IO Bool
ghCredentialUsable cfg = do
  let url = toString $ "https://api.github.com/repos/" <> cfg.owner <> "/" <> cfg.repo
  either (\(_ :: SomeException) -> False) (const True) <$> try (Wreq.getWith (ghOpts cfg.pat) url)


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
  withResource tr.trPool \conn -> do
    void $ execute conn [sql|DELETE FROM background_jobs WHERE payload->>'tag' LIKE 'GitSync%'|] ()
    void $ execute conn [sql|DELETE FROM projects.dashboards WHERE project_id = ?|] (Only testPid)
    void $ execute conn [sql|DELETE FROM projects.git_sync WHERE project_id = ?|] (Only testPid)
  void $ testServant tr $ GitSyncPage.gitSyncSettingsPostH testPid
    GitSyncForm{host = Just Git.GitHub, apiBase = Nothing, owner = cfg.owner, repo = cfg.repo, branch = cfg.branch, accessToken = cfg.pat, webhookSecret = Nothing, pathPrefix = prefixM}


-- | A dashboard assigned to the connected repository, optionally first given a new file's path.
createDash :: TestResources -> Text -> [Text] -> Maybe Text -> IO Dashboards.DashboardId
createDash tr title tags newPath = do
  now <- getCurrentTime
  dashId <- UUIDId <$> UUID.nextRandom
  let dash = (Dashboards.mkDashboardVM dashId testPid now Projects.systemUserId)
        { Dashboards.schema = Just (def :: Dashboards.Dashboard){Dashboards.title = Just title, Dashboards.tags = Just tags}
        , Dashboards.tags = V.fromList tags
        , Dashboards.title = title
        }
  _ <- runTestBg frozenTime tr (Dashboards.insert dash)
  for_ newPath \path -> gitInfo tr dashId path ""
  [sync] <- runQueryEffect tr $ GitSync.getGitSyncs testPid
  runQueryEffect tr (GitSync.assignDashboardRepository testPid dashId sync.id) `shouldReturn` Right ()
  pure dashId


-- | Queue @job@ (a push of a dashboard, or any sync job) and run every sync job.
syncNow :: TestResources -> Either Dashboards.DashboardId BackgroundJobs.BgJobs -> IO ()
syncNow tr job = do
  withResource tr.trPool \conn -> void $ execute conn [sql|DELETE FROM background_jobs WHERE payload->>'tag' LIKE 'GitSync%'|] ()
  either (void . toBaseServantResponse tr . atAuthToBase tr.trSessAndHeader . GitSyncPage.queueGitSyncPush testPid) (\j -> withResource tr.trPool \conn -> void $ createJob conn "background_jobs" j) job
  withResource tr.trPool $ setBjRunAtInThePast frozenTime
  void $ runBackgroundJobsWhere frozenTime tr.trATCtx \case
    BackgroundJobs.GitSyncPushDashboard{} -> True
    BackgroundJobs.GitSyncFromRepo{} -> True
    BackgroundJobs.GitSyncRepository{} -> True
    BackgroundJobs.GitSyncPushAllDashboards{} -> True
    _ -> False


gitInfo :: TestResources -> Dashboards.DashboardId -> Text -> Text -> IO ()
gitInfo tr did path sha = void $ runTestBg frozenTime tr $ GitSync.updateDashboardGitInfo did path sha


dashboardsAt :: TestResources -> Text -> IO [Dashboards.DashboardVM]
dashboardsAt tr path = filter (\d -> d.filePath == T.stripPrefix "dashboards/" path) <$> runTestBg frozenTime tr (Dashboards.selectDashboardsSortedBy testPid Dashboards.Asc Dashboards.DSTitle)


uniquePath :: Text -> IO Text
uniquePath base = do
  uid <- UUID.nextRandom
  pure $ "dashboards/" <> base <> "-" <> T.take 8 (UUID.toText uid) <> ".yaml"


-- ═══════════════════════════════════════════════════════════════════════════
-- SPEC
-- ═══════════════════════════════════════════════════════════════════════════

-- | Opt-in checks against real git hosts; everything that runs without one is in "Pages.CodeContextSpec".
spec :: Spec
-- aroundAll shares one resource across examples, so this spec runs sequentially.
spec = sequential do
  liveSmokeSpec
  -- The credential is probed first: an expired or revoked PAT is an environment problem, and
  -- reporting it as failing examples trains readers to ignore a permanently-red suite.
  configM <- runIO (loadTestConfig >>= traverse (\c -> (c,) <$> ghCredentialUsable c))
  case configM of
    Nothing -> describe "GitHub Sync E2E" $
      it "SKIPPED - Set GH_TEST_PAT, GH_TEST_OWNER, GH_TEST_REPO to enable" pending

    Just (_, False) -> describe "GitHub Sync E2E" $
      it "SKIPPED - GH_TEST_PAT is set but GitHub rejected it (expired, revoked, or lacking repo scope)" pending

    Just (cfg, True) -> aroundAll withTestResources $ beforeAll_ (ghCleanupDashboards cfg) $ afterAll_ (ghCleanupDashboards cfg) $ do
      describe "GitHub Sync E2E (Real API)" $ do
        it "pushes creates, updates, records the sha, honours the prefix and leaves a conflicting file alone" \tr -> do
          setupSync tr cfg Nothing
          created <- uniquePath "push-create"
          syncNow tr . Left =<< createDash tr "Push Create Test" ["e2e"] (Just created)
          (content, _) <- fromRightShow <$> ghGetFile cfg created
          BS.isInfixOf "Push Create Test" content `shouldBe` True

          updated <- uniquePath "push-update"
          _ <- ghPutFile cfg updated "title: Original\n" Nothing
          (_, sha1) <- fromRightShow <$> ghGetFile cfg updated
          updatedDash <- createDash tr "Updated Title" ["updated"] Nothing
          gitInfo tr updatedDash updated sha1
          syncNow tr (Left updatedDash)
          (updatedContent, _) <- fromRightShow <$> ghGetFile cfg updated
          BS.isInfixOf "Updated Title" updatedContent `shouldBe` True

          recorded <- createDash tr "SHA Test" [] Nothing
          syncNow tr (Left recorded)
          Just dash <- runTestBg frozenTime tr $ Dashboards.getDashboardByProjectId testPid recorded
          dash.filePath `shouldBe` Just "sha-test.yaml"
          dash.fileSha `shouldSatisfy` isJust

          -- A stale sha must not overwrite the file.
          conflicting <- uniquePath "sha-conflict"
          _ <- ghPutFile cfg conflicting "title: Original\n" Nothing
          conflictDash <- createDash tr "Conflict Test" [] Nothing
          gitInfo tr conflictDash conflicting "wrong-sha-12345"
          syncNow tr (Left conflictDash)
          (conflictContent, _) <- fromRightShow <$> ghGetFile cfg conflicting
          BS.isInfixOf "Original" conflictContent `shouldBe` True

          setupSync tr cfg (Just "test-prefix")
          syncNow tr . Left =<< createDash tr "Prefixed Push" [] Nothing
          (_, prefixedSha) <- fromRightShow <$> ghGetFile cfg "test-prefix/dashboards/prefixed-push.yaml"
          void $ ghDeleteFile cfg "test-prefix/dashboards/prefixed-push.yaml" prefixedSha

        it "pulls creates, updates and deletes local dashboards from GitHub" \tr -> do
          setupSync tr cfg Nothing
          let pull = syncNow tr $ Right $ BackgroundJobs.GitSyncFromRepo testPid
          path <- uniquePath "pull"
          _ <- ghPutFile cfg path "title: Created In GitHub\ntags:\n  - github\nwidgets: []\n" Nothing
          pull
          map (.title) <$> dashboardsAt tr path `shouldReturn` ["Created In GitHub"]
          (_, sha1) <- fromRightShow <$> ghGetFile cfg path
          _ <- ghPutFile cfg path "title: V2 Updated\ntags: []\nwidgets: []\n" (Just sha1)
          pull
          map (.title) <$> dashboardsAt tr path `shouldReturn` ["V2 Updated"]
          (_, sha2) <- fromRightShow <$> ghGetFile cfg path
          _ <- ghDeleteFile cfg path sha2
          pull
          map (.title) <$> dashboardsAt tr path `shouldReturn` []

        it "round-trips local → push → GitHub edit → pull, and bulk-pushes only assigned dashboards" \tr -> do
          setupSync tr cfg Nothing
          path <- uniquePath "roundtrip"
          syncNow tr . Left =<< createDash tr "Round Trip Original" ["local"] (Just path)
          (content1, sha1) <- fromRightShow <$> ghGetFile cfg path
          BS.isInfixOf "Round Trip Original" content1 `shouldBe` True
          _ <- ghPutFile cfg path "title: Round Trip Modified\ntags:\n  - local\n  - github-added\nwidgets: []\n" (Just sha1)
          syncNow tr $ Right $ BackgroundJobs.GitSyncFromRepo testPid
          [roundTripped] <- dashboardsAt tr path
          roundTripped.title `shouldBe` "Round Trip Modified"
          V.toList roundTripped.tags `shouldContain` ["github-added"]

          for_ (["Bulk A", "Bulk B", "Bulk C"] :: [Text]) \title -> createDash tr title [] Nothing
          now <- getCurrentTime
          localId <- UUIDId <$> UUID.nextRandom
          _ <- runTestBg frozenTime tr $ Dashboards.insert (Dashboards.mkDashboardVM localId testPid now Projects.systemUserId){Dashboards.title = "Stays Local"}
          syncNow tr $ Right $ BackgroundJobs.GitSyncPushAllDashboards testPid
          files <- ghListFiles cfg "dashboards"
          map (\slug -> any (slug `T.isInfixOf`) files) ["bulk-a", "bulk-b", "bulk-c", "stays-local"] `shouldBe` [True, True, True, False]


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
      it "lists repositories, reports a default branch, and round-trips a write at the sha it reported" do
        runEff (W.runHTTPWreq $ Git.listTokenRepos cfg.conn) >>= (`shouldSatisfy` isRight)
        runEff (W.runHTTPWreq $ Git.defaultBranchOf cfg.conn cfg.repoRef) >>= (`shouldSatisfy` not . T.null)
        let path = "monoscope-smoke/probe.yaml"
            body = "title: monoscope smoke probe\n"
        (sha, _rev) <- runEff (W.runHTTPWreq $ Git.pushFile cfg.conn cfg.repoRef path body Nothing "monoscope smoke test") >>= either (fail . toString) pure
        -- The sha we recorded must be the one a fresh listing reports, or `buildSyncPlan`
        -- will see a phantom change on the very next pull.
        (_, entries) <- runEff (W.runHTTPWreq $ Git.fetchTree cfg.conn cfg.repoRef "monoscope-smoke/") >>= either (fail . toString) pure
        ((.sha) <$> find ((== path) . (.path)) entries) `shouldBe` Just (Just sha)
        runEff (W.runHTTPWreq $ Git.fetchFile cfg.conn cfg.repoRef path) >>= either (fail . toString) pure >>= (`shouldBe` body)
