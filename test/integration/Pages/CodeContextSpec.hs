module Pages.CodeContextSpec (spec) where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Base64.Types (extractBase64)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Cache qualified as Cache
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Wreq qualified as W
import Data.Map.Strict qualified as M
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Types (Query (..))
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpose, interpret)
import Hasql.Interpolate qualified as HI
import Lucid qualified
import Models.Projects.CodeContext qualified as CodeContext
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ImpactReviews qualified as ImpactReviews
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Client (HttpException (InvalidUrlException))
import Network.HTTP.Client.Internal (Response (..), ResponseClose (..), createCookieJar, defaultRequest)
import Network.HTTP.Types.Status (Status (..))
import Network.HTTP.Types.Version (http11)
import Pages.BodyWrapper (PageCtx (..))
import Pages.Bots.BotTestHelpers (withHTTPResponses)
import Pages.CodeContext qualified as PageCodeContext
import Pages.Components (stackTrace_)
import Pages.Dashboards qualified as DashboardPage
import Pages.GitSync qualified as GitSyncPage
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Git qualified as Git
import Pkg.TestUtils
import Relude
import Servant (getHeaders)
import System.Config (AuthContext (..), EnvConfig (..))
import Test.Hspec
import UnliftIO.Exception (throwIO)


render :: Lucid.Html () -> Text
render = LT.toStrict . Lucid.renderText


-- | Regression: the revision a span reports lives in its /resource/, not its attributes.
--
-- 'revisionFor' was handed the span-attribute lookup, which structurally cannot hold
-- @service.version@ (ProcessMessage writes it to @resource@, and every other reader —
-- 'Telemetry.spanServiceName', the @resource___service___version@ column, the
-- @resource.service.version@ facet — reads it there). So the snippet URL never carried a
-- revision and every frame rendered at the mapping's branch: today's source, silently, which
-- is wrong exactly when someone is debugging an old release. The doctests could not catch it
-- because they pass 'revisionFor' a hand-built lookup; only the wiring was wrong.
revisionWiringSpec :: Spec
revisionWiringSpec = describe "stack frame source URL" do
  let pyStack = "Traceback (most recent call last):\n  File \"/srv/app/checkout.py\", line 88, in charge\n    raise ValueError(x)"
      lookupOf kvs k = listToMaybe [v | (k', v) <- kvs, k' == k]
      urlFor spanAttrs resourceAttrs = render $ stackTrace_ testPid (Just "checkout") (lookupOf spanAttrs) (lookupOf resourceAttrs) pyStack

  -- The needle omits the leading @&@ on purpose: Lucid escapes it to @&amp;@ inside an
  -- attribute, so matching on "&revision=" never fires — which would have made the two
  -- negative cases below pass whether or not the bug was fixed.
  it "carries the revision the span's resource reports" do
    urlFor [] [("service.version", "1c0ffee")] `shouldSatisfy` T.isInfixOf "revision=1c0ffee"

  it "does not mistake a span attribute for the resource's revision" do
    -- The whole bug: a sha in the wrong map must not be read, or the fix is untested.
    urlFor [("service.version", "1c0ffee")] [] `shouldNotSatisfy` T.isInfixOf "revision="

  it "omits the revision when the resource reports a version that is not a sha" do
    urlFor [] [("service.version", "v2.3.1")] `shouldNotSatisfy` T.isInfixOf "revision="


withUnavailableRepositoryAPI :: (IOE :> es, W.HTTP :> es) => Eff es a -> Eff es a
withUnavailableRepositoryAPI = interpose @W.HTTP \_ _ -> liftIO $ throwIO $ InvalidUrlException "https://git.invalid" "Fixture account is unavailable"


-- | Count outbound requests while serving one canned file body.
--
-- A PAT credential means no installation-token exchange, so every request the interpreter sees
-- is the blob fetch itself and the count is the thing under test with nothing subtracted.
-- Spelled out rather than wildcarded: every constructor returns the same response type, but
-- GADT refinement only happens on an explicit match. Writes fail loudly — reading a snippet
-- is a GET, and a mutation reaching here means the call path changed shape.
runCountingHTTP :: IOE :> es => IORef Int -> LBS.ByteString -> Eff (W.HTTP ': es) a -> Eff es a
runCountingHTTP calls body = interpret \_ -> \case
  W.Get _ -> served
  W.GetWith _ _ -> served
  W.Delete _ -> served
  W.DeleteWith _ _ -> served
  W.Post{} -> wrote "POST"
  W.PostWith{} -> wrote "POST"
  W.Put{} -> wrote "PUT"
  W.PutWith{} -> wrote "PUT"
  W.Patch{} -> wrote "PATCH"
  W.PatchWith{} -> wrote "PATCH"
  where
    wrote :: Text -> a
    wrote verb = error $ "fetchSnippet must not " <> verb <> " — the snippet path is read-only"
    served = do
      liftIO $ modifyIORef' calls (+ 1)
      pure $ httpResponse body


httpResponse :: LBS.ByteString -> Response LBS.ByteString
httpResponse body =
  Response
    { responseStatus = Status 200 "OK"
    , responseVersion = http11
    , responseHeaders = []
    , responseBody = body
    , responseCookieJar = createCookieJar []
    , responseClose' = ResponseClose pass
    , responseOriginalRequest = defaultRequest
    , responseEarlyHints = []
    }


-- | One git-host API call per frame opened is the shape that makes a hot issue a rate-limit
-- stall — and a rate-limited fetch presents as the panel silently not filling in, which reads
-- as "this feature does not work" rather than as a quota. The cache is keyed on
-- @(owner, repo, ref, path)@, so the second view of the same frame must not leave the process.
snippetCacheSpec :: Spec
snippetCacheSpec = around withTestResources do
  describe "Frame source caching" do
    it "fetches a blob once however many frames ask for it" \tr -> do
      let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
      _ <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "acme" "checkout-service" "main" (GitSync.PersonalToken "ghp_test") Nothing ""
      _ <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just "checkout-service") (Just "main") (Just "checkout") Nothing (Just "/srv/app/") (Just "") Nothing)

      calls <- newIORef (0 :: Int)
      -- GitHub's contents API answers base64; four lines so both line numbers below are in range.
      let contents = encodeUtf8 @Text @LBS.ByteString $ "{\"content\":\"" <> extractBase64 (B64.encodeBase64 "one\ntwo\nthree\nfour\n") <> "\"}"
          fetch n = runQueryEffect tr $ runCountingHTTP calls contents $ CodeContext.fetchSnippet tr.trATCtx.codeBlobCache tr.trATCtx.config testPid (Just "checkout") Nothing "/srv/app/checkout.py" n

      first_ <- fetch 2
      readIORef calls >>= \c -> c `shouldBe` 1
      -- A DIFFERENT line in the same file: the cache is per blob, not per snippet, so the
      -- second frame of one stack trace must be free.
      second_ <- fetch 3
      readIORef calls >>= \c -> c `shouldBe` 1
      fmap (.focusLine) first_ `shouldBe` Right 2
      fmap (.focusLine) second_ `shouldBe` Right 3
      fmap (.body) second_ `shouldBe` Right ["one", "two", "three", "four"]


-- | Listing the picker's repositories is a token exchange plus a listing call against the git
-- host, and it sat in front of a settings page that renders in 0.2s otherwise — measured at
-- 1.5s every load. It is cached per credential now.
--
-- Nothing here can reach GitHub, so an uncached render offers the free-text fallback and no
-- options at all. A seeded entry showing up in the markup is therefore proof the cache is what
-- answered, with no request counting needed.
repoListCacheSpec :: Spec
repoListCacheSpec = around withTestResources do
  it "renders the repository picker from the cache instead of calling the git host" \tr -> do
    let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
    credM <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "acme" (Just 42) Nothing
    cred <- maybe (fail "expected a credential") pure credM

    uncached <- render . snd <$> testServant tr (withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsGetH testPid Nothing)
    uncached `shouldNotSatisfy` T.isInfixOf "sentinel-repo"

    Cache.insert tr.trATCtx.repoListCache cred.id [Git.GitRepo "acme/sentinel-repo" "sentinel-repo" False "trunk"]
    cached <- render . snd <$> testServant tr (withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsGetH testPid Nothing)
    cached `shouldSatisfy` T.isInfixOf "sentinel-repo"
    -- The option carries its own default branch, which is what lets the Branch field fill
    -- itself in — a cache that dropped it would silently send every mapping to "main".
    cached `shouldSatisfy` T.isInfixOf "trunk"


spec :: Spec
spec = do
  revisionWiringSpec
  snippetCacheSpec
  repoListCacheSpec
  around withTestResources do
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
        Just credential <- runQueryEffect tr $ GitSync.upsertGitHubCredential (encodeUtf8 cfg.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "acme" Nothing (Just "ghp_deployment_fixture")
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
          case result of
            Right page -> do
              length page.entries `shouldBe` min 5 count
              page.limitReached `shouldBe` (count >= 5)
              case page.entries of
                firstEntry : secondEntry : _ -> do
                  firstEntry.deployment.revision `shouldBe` revision
                  case firstEntry.statuses of
                    Right reported -> do
                      map (.state) reported.entries `shouldBe` take (min 10 statusCount) (cycle [Git.DSSuccess, Git.DSPending])
                      reported.limitReached `shouldBe` (statusCount >= 10)
                      all ((> firstEntry.deployment.createdAt) . (.createdAt)) reported.entries `shouldBe` True
                    Left err -> fail $ show err
                  secondEntry.statuses `shouldSatisfy` isLeft
                _ -> fail "Expected at least two deployments"
            Left err -> fail $ show err

    describe "Source code settings (code mappings)" do
      it "repositoryPicker_connectsCodebasesWithoutEnablingCapabilitiesAndRejectsUnavailableSelections" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
            repositories = [Git.GitRepo "team/checkout" "checkout" True "trunk", Git.GitRepo "team/catalog" "catalog" False "main"]
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "team" (Just 42) Nothing
        Cache.insert tr.trATCtx.repoListCache account.id repositories
        (_, picker) <- testServant tr $ PageCodeContext.repositoryConnectGetH testPid (Just account.id)
        picker.content.repositories `shouldBe` Right repositories
        (_, rejected) <- testServant tr $ PageCodeContext.repositoryConnectPostH testPid (Just account.id) (PageCodeContext.RepositoryConnectForm ["team/checkout", "foreign/private"])
        rejected.content.selected `shouldBe` ["team/checkout", "foreign/private"]
        rejected.content.connectionError `shouldSatisfy` isJust
        runQueryEffect tr (GitSync.getRepositories testPid) >>= (`shouldSatisfy` null)
        replicateM_ 2 do
          (response, _) <- testServant tr $ PageCodeContext.repositoryConnectPostH testPid (Just account.id) (PageCodeContext.RepositoryConnectForm ["team/checkout", "team/catalog", "team/checkout"])
          getHeaders response `shouldSatisfy` elem ("HX-Redirect", encodeUtf8 ("/p/" <> testPid.toText <> "/repositories"))
        connected <- runQueryEffect tr $ GitSync.getRepositories testPid
        map (.repo) connected `shouldBe` ["catalog", "checkout"]
        map (.credentialId) connected `shouldBe` [Just account.id, Just account.id]
        runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= (`shouldSatisfy` null)
        runQueryEffect tr (GitSync.getGitSyncs testPid) >>= (`shouldSatisfy` null)
        otherPid <- createTestProject tr "Other picker account"
        Just foreignAccount <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey otherPid Git.GitHub Nothing "foreign" (Just 43) Nothing
        Cache.insert tr.trATCtx.repoListCache foreignAccount.id repositories
        runQueryEffect tr (GitSync.connectRepository testPid foreignAccount.id (Git.GitRepo "foreign/private" "private" True "main")) >>= (`shouldSatisfy` isNothing)
        (_, denied) <- testServant tr $ PageCodeContext.repositoryConnectPostH testPid (Just foreignAccount.id) (PageCodeContext.RepositoryConnectForm ["team/catalog"])
        denied.content.connectionError `shouldSatisfy` isJust
        runQueryEffect tr (GitSync.getRepositories otherPid) >>= (`shouldSatisfy` null)
        Cache.insert tr.trATCtx.repoListCache account.id []
        (_, emptyPicker) <- testServant tr $ PageCodeContext.repositoryConnectGetH testPid (Just account.id)
        emptyPicker.content.repositories `shouldBe` Right []
        render (Lucid.toHtml emptyPicker.content) `shouldSatisfy` T.isInfixOf "No repositories are available"
        Cache.delete tr.trATCtx.repoListCache account.id
        let unavailable = tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppPrivateKey = ""}}}
        (_, failed) <- testServant unavailable $ PageCodeContext.repositoryConnectGetH testPid (Just account.id)
        failed.content.repositories `shouldSatisfy` isLeft
        render (Lucid.toHtml failed.content) `shouldSatisfy` T.isInfixOf "Could not load repositories"
        render (Lucid.toHtml failed.content) `shouldSatisfy` T.isInfixOf "Retry"
        Cache.lookup tr.trATCtx.repoListCache account.id `shouldReturn` Nothing
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql| DELETE FROM projects.git_credentials WHERE project_id = #{testPid} AND id = #{account.id} |]
        retained <- runQueryEffect tr $ GitSync.getRepositories testPid
        map (.credentialId) retained `shouldBe` [Nothing, Nothing]

      it "repositoryFeatures_areDiscoverableOutsideNotificationSettings" \tr -> do
        (_, html) <- testServant tr $ PageCodeContext.codeMappingsGetH testPid Nothing
        let out = render html
        out `shouldSatisfy` T.isInfixOf ("href=\"/p/" <> testPid.toText <> "/repositories\"")
        out `shouldSatisfy` T.isInfixOf "Pull requests"
        out `shouldSatisfy` T.isInfixOf "Configuration"

      it "sourceRepositoryPicker_preservesTheSelectedNamespaceInsteadOfUsingTheAccountName" \tr -> do
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitLab (Just "https://git.example.com") "team" Nothing (Just "fixture")
        Cache.insert tr.trATCtx.repoListCache account.id [Git.GitRepo "team/platform/checkout" "checkout" True "trunk"]
        (_, picker) <- testServant tr $ PageCodeContext.codeMappingsEditorGetH testPid (Just account.id) Nothing
        render picker `shouldSatisfy` T.isInfixOf "value=\"team/platform/checkout\""
        _ <- testServant tr $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just "team/platform/checkout") (Just "trunk") (Just "checkout") Nothing Nothing Nothing (Just account.id))
        [mapping] <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        (mapping.owner, mapping.repo) `shouldBe` ("team/platform", "checkout")

      it "repositoryOverview_includesServiceAndDashboardRepositoriesWithoutCallingTheProvider" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        Just credential <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "acme" (Just 42) Nothing
        runQueryEffect tr $ CodeContext.insertCodeMapping testPid credential.id (Git.RepoRef "acme" "checkout-service" "main") (Just "checkout") "/srv/app/" ""
        _ <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "acme" "dashboards" "main" (GitSync.AppInstallation 42) Nothing ""
        (_, html) <- testServant tr $ PageCodeContext.repositoriesGetH testPid Nothing Nothing
        let out = render html
        for_ (["acme/checkout-service", "checkout", "acme/dashboards", "Awaiting first sync", "Production impact reviews"] :: [Text]) \label ->
          T.isInfixOf label out `shouldBe` True
        (_, reviews) <- testServant tr $ PageCodeContext.repositoriesGetH testPid (Just PageCodeContext.PullRequests) Nothing
        T.isInfixOf "No pull requests reviewed yet" (render reviews) `shouldBe` True

      it "repositorySync_acceptsIndependentRepositoriesForDifferentTeams" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
            connect owner = runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing owner "dashboards" "main" (GitSync.AppInstallation 42) Nothing ""
        Just syncA <- connect "team-a"
        Just syncB <- connect "team-b"
        syncA.id `shouldNotBe` syncB.id
        (_, html) <- testServant tr $ PageCodeContext.repositoriesGetH testPid Nothing Nothing
        for_ (["team-a/dashboards", "team-b/dashboards"] :: [Text]) \repo ->
          T.isInfixOf repo (render html) `shouldBe` True

      it "repositoryAccounts_requireAnExplicitChoiceInsteadOfUsingTheNewestGrant" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        accounts <- forM ["team-a", "team-b"] \account -> do
          Just credential <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing account (Just 42) Nothing
          Cache.insert tr.trATCtx.repoListCache credential.id []
          pure credential
        (_, html) <- testServant tr $ PageCodeContext.codeMappingsGetH testPid Nothing
        render html `shouldSatisfy` T.isInfixOf "Choose an account"
        _ <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just "checkout") (Just "main") Nothing Nothing Nothing Nothing Nothing)
        runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= (`shouldSatisfy` null)
        for_ accounts \account -> do
          (_, editor) <- testServant tr $ PageCodeContext.codeMappingsEditorGetH testPid (Just account.id) (Just "/srv/app/main.py")
          T.isInfixOf ("name=\"credentialId\" value=\"" <> account.id.toText <> "\"") (render editor) `shouldBe` True
          T.isInfixOf "value=\"/srv/app/main.py\"" (render editor) `shouldBe` True
          _ <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just "checkout") (Just "main") (Just account.account) Nothing Nothing Nothing (Just account.id))
          pure ()
        mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        sort [(m.owner, m.credentialId) | m <- mappings] `shouldBe` sort [(a.account, a.id) | a <- accounts]
        otherPid <- createTestProject tr "Other account project"
        Just foreignAccount <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey otherPid Git.GitHub Nothing "foreign" (Just 42) Nothing
        _ <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just "checkout") (Just "main") (Just "foreign") Nothing Nothing Nothing (Just foreignAccount.id))
        runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= \rows -> length rows `shouldBe` 2

      it "repositoryConnection_survivesCapabilityRemovalAndKeepsItsOwnDetails" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
            configured = tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppWebhookSecret = "fixture", githubAppId = "fixture", githubAppPrivateKey = "fixture"}}}
        Just credential <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "team" (Just 42) Nothing
        runQueryEffect tr $ CodeContext.insertCodeMapping testPid credential.id (Git.RepoRef "team" "checkout" "main") (Just "checkout") "/app/" "src"
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "team" "checkout" "main" (GitSync.AppInstallation 42) Nothing "ops"
        [repository] <- runQueryEffect tr $ GitSync.getRepositories testPid
        (_, details) <- testServant configured $ PageCodeContext.repositoryGetH testPid repository.id
        render (Lucid.toHtml details.content) `shouldSatisfy` T.isInfixOf ("href=\"/p/" <> testPid.toText <> "/repositories/" <> repository.id.toText <> "/source\"")
        map (.service) details.content.mappings `shouldBe` [Just "checkout"]
        fmap (.id) details.content.dashboardSync `shouldBe` Just sync.id
        details.content.reviewReadiness `shouldSatisfy` \case PageCodeContext.ReviewReady _ -> True; _ -> False
        let incomplete = configured{trATCtx = configured.trATCtx{config = configured.trATCtx.config{githubAppPrivateKey = ""}}}
        (_, unavailable) <- testServant incomplete $ PageCodeContext.repositoryGetH testPid repository.id
        unavailable.content.reviewReadiness `shouldSatisfy` \case PageCodeContext.ReviewNeedsServer -> True; _ -> False
        runQueryEffect tr $ ImpactReviews.updateSettings testPid (ImpactReviews.ReviewSettings "team" "checkout" False False)
        (_, disabled) <- testServant configured $ PageCodeContext.repositoryGetH testPid repository.id
        disabled.content.reviewReadiness `shouldSatisfy` \case PageCodeContext.ReviewDisabled -> True; _ -> False
        for_ details.content.mappings \mapping -> runQueryEffect tr $ CodeContext.deleteCodeMapping testPid mapping.id
        void $ runQueryEffect tr $ GitSync.deleteGitHubSync sync.id
        [retained] <- runQueryEffect tr $ GitSync.getRepositories testPid
        retained.id `shouldBe` repository.id
        (_, disconnected) <- testServant configured $ PageCodeContext.repositoryGetH testPid repository.id
        disconnected.content.mappings `shouldSatisfy` null
        disconnected.content.dashboardSync `shouldSatisfy` isNothing
        disconnected.content.reviewReadiness `shouldSatisfy` \case PageCodeContext.ReviewNeedsService -> True; _ -> False
        otherPid <- createTestProject tr "Other repository details"
        runQueryEffect tr (GitSync.getRepository otherPid repository.id) >>= (`shouldSatisfy` isNothing)
        Just reconnected <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "team" "checkout" "main" (GitSync.AppInstallation 42) Nothing ""
        void $ runQueryEffect tr $ GitSync.updateGitHubSync encKey reconnected.id "team" "dashboards" "main" Nothing Nothing
        runQueryEffect tr (GitSync.getRepositories testPid) >>= \rows -> map (.repo) rows `shouldBe` ["checkout", "dashboards"]

      it "repositoryDashboardSetup_reusesItsAccountAndQueuesOnlyItsInitialPull" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
            configured = tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppWebhookSecret = "fixture", githubAppId = "fixture", githubAppPrivateKey = "fixture"}}}
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "team" (Just 42) Nothing
        let repo = Git.GitRepo "team/checkout" "checkout" True "trunk"
        Cache.insert tr.trATCtx.repoListCache account.id [repo]
        Just repository <- runQueryEffect tr $ GitSync.connectRepository testPid account.id repo
        (_, initial) <- testServant configured $ GitSyncPage.repositoryDashboardGetH testPid repository.id
        initial.content.form.branch `shouldBe` "trunk"
        initial.content.form.credentialId `shouldBe` Just account.id
        initial.content.sync `shouldSatisfy` isNothing
        let selection = GitSyncPage.RepositoryDashboardForm (Just account.id) "trunk" (Just "/ops/")
        (_, missingConfig) <- testServant tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppWebhookSecret = ""}}} $ GitSyncPage.repositoryDashboardPostH testPid repository.id selection
        missingConfig.content.setupError `shouldSatisfy` isJust
        missingConfig.content.form.pathPrefix `shouldBe` Just "/ops/"
        runQueryEffect tr (GitSync.getGitSyncs testPid) >>= (`shouldSatisfy` null)
        (_, connected) <- testServant configured $ GitSyncPage.repositoryDashboardPostH testPid repository.id selection
        sync <- maybe (fail "Expected dashboard sync") pure connected.content.sync
        (sync.owner, sync.repo, sync.branch, sync.pathPrefix, sync.installationId, sync.webhookSecret) `shouldBe` ("team", "checkout", "trunk", "ops", Just 42, Just "fixture")
        _ <- testServant configured $ GitSyncPage.repositoryDashboardPostH testPid repository.id selection{GitSyncPage.branch = "different"}
        [unchanged] <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        unchanged.branch `shouldBe` "trunk"
        jobs <- getPendingBackgroundJobs tr.trATCtx
        length [() | (_, BackgroundJobs.GitSyncRepository project sid) <- toList jobs, project == testPid, sid == sync.id] `shouldBe` 1
        otherPid <- createTestProject tr "Foreign dashboard account"
        Just foreignAccount <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey otherPid Git.GitHub Nothing "foreign" (Just 43) Nothing
        runQueryEffect tr (GitSync.enableRepositoryDashboardSync testPid repository.id foreignAccount.id "main" "" Nothing) >>= (`shouldSatisfy` isNothing)
        Just tokenAccount <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitLab (Just "https://git.example.com") "team" Nothing (Just "fixture-token")
        Just tokenRepo <- runQueryEffect tr $ GitSync.connectRepository testPid tokenAccount.id (Git.GitRepo "team/catalog" "catalog" True "main")
        (_, unavailableBranch) <- testServant tr $ withUnavailableRepositoryAPI $ GitSyncPage.repositoryDashboardPostH testPid tokenRepo.id (GitSyncPage.RepositoryDashboardForm (Just tokenAccount.id) "" (Just "ops"))
        unavailableBranch.content.setupError `shouldSatisfy` isJust
        unavailableBranch.content.sync `shouldSatisfy` isNothing
        (_, tokenSetup) <-
          testServant tr
            $ interpose @W.HTTP (\_ -> \case W.GetWith _ _ -> pure $ httpResponse "{\"default_branch\":\"stable\"}"; _ -> liftIO $ throwIO $ InvalidUrlException "https://git.invalid" "Branch detection must only GET repository metadata")
            $ GitSyncPage.repositoryDashboardPostH testPid tokenRepo.id (GitSyncPage.RepositoryDashboardForm (Just tokenAccount.id) "" Nothing)
        tokenSync <- maybe (fail "Expected token dashboard sync") pure tokenSetup.content.sync
        tokenSync.branch `shouldBe` "stable"
        tokenSync.webhookSecret `shouldSatisfy` isJust
        decrypted <- runQueryEffect tr $ GitSync.getGitSyncsDecrypted encKey testPid
        map (.accessToken) (filter ((== tokenSync.id) . (.id)) decrypted) `shouldBe` [Just "fixture-token"]
        render (Lucid.toHtml tokenSetup.content) `shouldSatisfy` T.isInfixOf "Webhook secret"

      it "repositorySourceSetup_preservesIdentityAccountAndBranchAndScopesItsMutations" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
            repos = [Git.GitRepo "team/checkout" "checkout" True "trunk", Git.GitRepo "team/catalog" "catalog" True "main"]
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "team" (Just 42) Nothing
        Cache.insert tr.trATCtx.repoListCache account.id repos
        Just repository <- runQueryEffect tr $ GitSync.connectRepository testPid account.id (Git.GitRepo "team/checkout" "checkout" True "trunk")
        runQueryEffect tr $ CodeContext.insertCodeMapping testPid account.id (Git.RepoRef "team" "catalog" "main") (Just "catalog") "/catalog/" ""
        (_, source) <- testServant tr $ PageCodeContext.repositorySourceGetH testPid repository.id Nothing (Just "/app/main.py")
        let html = render source
        for_ ["value=\"team/checkout\"", "value=\"trunk\"", "value=\"/app/main.py\"", "name=\"credentialId\" value=\"" <> account.id.toText <> "\""] \value -> html `shouldSatisfy` T.isInfixOf value
        html `shouldNotSatisfy` T.isInfixOf "Unlink team/catalog"
        _ <- testServant tr $ PageCodeContext.repositorySourcePostH testPid repository.id (PageCodeContext.CodeMappingForm (Just "foreign/private") (Just "release") (Just "checkout") Nothing (Just "/app/") Nothing Nothing)
        mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        map (\m -> (m.owner, m.repo, m.ref, m.credentialId)) (filter ((== Just "checkout") . (.service)) mappings) `shouldBe` [("team", "checkout", "release", account.id)]
        _ <- testServant tr $ PageCodeContext.repositoryReviewsPostH testPid repository.id (ImpactReviews.ReviewSettings "foreign" "private" False False)
        settings <- runQueryEffect tr $ ImpactReviews.repositorySettings testPid
        map (\s -> (s.owner, s.repo, s.enabled)) settings `shouldMatchList` [("team", "checkout", False), ("team", "catalog", True)]
        for_ (filter ((== "checkout") . (.repo)) mappings) \mapping -> void $ testServant tr $ PageCodeContext.repositorySourceDeleteH testPid repository.id mapping.id
        runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= \remaining -> map (.repo) remaining `shouldBe` ["catalog"]

      it "repositoryCredentials_keepTheSameAccountOnDifferentServersIndependent" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        for_ ([Nothing, Just "https://git.example.com"] :: [Maybe Text]) \origin ->
          runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub origin "team" Nothing (Just "fixture")
        credentials <- runQueryEffect tr $ GitSync.getGitHubCredentials testPid
        sort (map (.apiBase) credentials) `shouldBe` [Nothing, Just "https://git.example.com"]

      it "repositoryConnection_keepsIdenticalNamesOnDifferentServersIndependent" \tr -> do
        let form origin = GitSyncPage.GitSyncForm (Just Git.GitHub) origin "team" "dashboards" "main" "fixture" Nothing Nothing
        for_ ([Nothing, Just "https://git.example.com"] :: [Maybe Text]) $ testServant tr . GitSyncPage.gitSyncSettingsPostH testPid . form
        syncs <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        sort (map (.apiBase) syncs) `shouldBe` [Nothing, Just "https://git.example.com"]
        credentials <- runQueryEffect tr $ GitSync.getGitHubCredentials testPid
        sort (map (.apiBase) credentials) `shouldBe` [Nothing, Just "https://git.example.com"]
        (_, overview) <- testServant tr $ PageCodeContext.repositoriesGetH testPid Nothing Nothing
        T.count "team/dashboards" (render overview) `shouldBe` 2

      it "repositoryConnection_reportsUnreadableTokensInsteadOfSilentlySkipping" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        _ <- runQueryEffect tr $ Hasql.interpExecute [HI.sql| UPDATE projects.git_sync SET access_token = 'invalid-base64' WHERE id = #{sync.id} |]
        decrypted <- runQueryEffect tr $ GitSync.getGitSyncsDecrypted (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid
        length decrypted `shouldBe` 0
        Just failed <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        failed.lastError `shouldSatisfy` isJust

      it "repositoryPicker_doesNotTreatAnEnterpriseRepositoryAsItsPublicGitHubCounterpart" \tr -> do
        _ <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub (Just "https://git.example.com") "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        _ <- testServant tr $ GitSyncPage.githubAppSelectRepoH testPid $ GitSyncPage.RepoSelectForm ["team/dashboards"] "main" Nothing 42
        syncs <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        sort (map (.apiBase) syncs) `shouldBe` [Nothing, Just "https://git.example.com"]

      it "repositoryOwnership_isolatesIdenticalPathsAndRetainsDashboardsOnDisconnect" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
            connect owner = runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing owner "dashboards" "main" (GitSync.AppInstallation 42) Nothing ""
            create = do
              did <- UUIDId <$> UUID.nextRandom
              runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Overview"}
        Just syncA <- connect "team-a"
        Just syncB <- connect "team-b"
        dashA <- create
        dashB <- create
        conflict <- create
        runQueryEffect tr (GitSync.assignDashboardRepository testPid dashA.id syncA.id) `shouldReturn` Right ()
        runQueryEffect tr (GitSync.assignDashboardRepository testPid dashB.id syncB.id) `shouldReturn` Right ()
        runQueryEffect tr (GitSync.assignDashboardRepository testPid dashA.id syncB.id) `shouldReturn` Left (GitSync.OwnedByRepository syncA.id)
        runQueryEffect tr (GitSync.assignDashboardRepository testPid conflict.id syncA.id) `shouldReturn` Left (GitSync.FileOwnedByDashboard dashA.id)
        otherPid <- createTestProject tr "Other repository project"
        runQueryEffect tr (GitSync.assignDashboardRepository otherPid dashA.id syncA.id) `shouldReturn` Left GitSync.RepositoryMissing
        _ <- runQueryEffect tr $ GitSync.updateDashboardGitInfo dashA.id "overview.yaml" "sha-a"
        _ <- runQueryEffect tr $ GitSync.updateDashboardGitInfo dashB.id "overview.yaml" "sha-b"
        stateA <- runQueryEffect tr $ GitSync.getRepositoryDashboardState testPid syncA.id
        stateB <- runQueryEffect tr $ GitSync.getRepositoryDashboardState testPid syncB.id
        M.toList stateA `shouldBe` [("overview.yaml", (dashA.id, "sha-a"))]
        M.toList stateB `shouldBe` [("overview.yaml", (dashB.id, "sha-b"))]
        runAuthHandler tr $ GitSyncPage.queueGitSyncPush testPid dashB.id
        jobs <- getPendingBackgroundJobs tr.trATCtx
        jobs
          `shouldSatisfy` any
            ( \(_, job) -> case job of
                BackgroundJobs.GitSyncPushDashboard project did -> project == testPid && did == dashB.id.unwrap
                _ -> False
            )
        _ <- testServant tr $ GitSyncPage.gitSyncRepositoryDeleteH testPid syncA.id
        Just retained <- runQueryEffect tr $ Dashboards.getDashboardByProjectId testPid dashA.id
        (retained.title, retained.gitSyncId, retained.filePath, retained.fileSha) `shouldBe` ("Overview", Nothing, Nothing, Nothing)
        Just untouched <- runQueryEffect tr $ Dashboards.getDashboardByProjectId testPid dashB.id
        (untouched.gitSyncId, untouched.fileSha) `shouldBe` (Just syncB.id, Just "sha-b")
        (_, overview) <- testServant tr $ PageCodeContext.repositoriesGetH testPid Nothing Nothing
        for_ (["team-a/dashboards", "team-b/dashboards"] :: [Text]) \repo ->
          T.isInfixOf repo (render overview) `shouldBe` True

      it "repositoryMigration_normalizesLegacyPathsAndRetainsCollidingDashboards" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" "dashboards" "main" (GitSync.AppInstallation 42) Nothing "ops"
        dashboards <- forM (zip [1 :: Int ..] ["overview.yaml", "dashboards/overview.yaml", "ops/dashboards/overview.yaml"]) \(n, path) -> do
          did <- UUIDId <$> UUID.nextRandom
          runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Retained overview", Dashboards.updatedAt = addUTCTime (fromIntegral n) frozenTime, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just path, Dashboards.fileSha = Just "blob"}
        migration <- Query <$> readFileBS "static/migrations/0216_repository_relative_paths.sql"
        withResource tr.trPool \conn -> void $ PG.execute_ conn migration
        retained <- forM dashboards \dash -> runQueryEffect tr $ Dashboards.getDashboardByProjectId testPid dash.id
        map (fmap (\d -> (d.title, d.gitSyncId, d.filePath, d.fileSha))) retained
          `shouldBe` [ Just ("Retained overview", Nothing, Nothing, Nothing)
                     , Just ("Retained overview", Nothing, Nothing, Nothing)
                     , Just ("Retained overview", Just sync.id, Just "overview.yaml", Just "blob")
                     ]

      it "dashboardRepositoryAssignment_usesTheChosenRepositoryAndKeepsConflictsLocal" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
            connect project owner = runQueryEffect tr $ GitSync.insertGitHubSync encKey project Git.GitHub Nothing owner "dashboards" "main" (GitSync.AppInstallation 42) Nothing ""
            create = do
              did <- UUIDId <$> UUID.nextRandom
              runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Overview", Dashboards.baseTemplate = Just "redis.yaml", Dashboards.fileSha = Just "local-checksum"}
        Just syncA <- connect testPid "team-a"
        Just syncB <- connect testPid "team-b"
        dash <- create
        (_, initial) <- testServant tr $ GitSyncPage.dashboardRepositoryGetH testPid dash.id
        length initial.content.repositories `shouldBe` 2
        (_, assigned) <- testServant tr $ GitSyncPage.dashboardRepositoryPostH testPid dash.id (GitSyncPage.DashboardRepositoryForm syncB.id)
        let owned = assigned.content.dashboard
        (owned.gitSyncId, owned.filePath, owned.fileSha, isJust owned.schema) `shouldBe` (Just syncB.id, Just "overview.yaml", Nothing, True)
        (owned.schema >>= (.file)) `shouldBe` Just "redis.yaml"
        jobs <- getPendingBackgroundJobs tr.trATCtx
        jobs
          `shouldSatisfy` any
            ( \(_, job) -> case job of
                BackgroundJobs.GitSyncPushDashboard project did -> project == testPid && did == dash.id.unwrap
                _ -> False
            )
        (_, reassigned) <- testServant tr $ GitSyncPage.dashboardRepositoryPostH testPid dash.id (GitSyncPage.DashboardRepositoryForm syncA.id)
        reassigned.content.assignmentError `shouldBe` Just (GitSync.OwnedByRepository syncB.id)
        other <- create
        (_, conflict) <- testServant tr $ GitSyncPage.dashboardRepositoryPostH testPid other.id (GitSyncPage.DashboardRepositoryForm syncB.id)
        conflict.content.assignmentError `shouldBe` Just (GitSync.FileOwnedByDashboard dash.id)
        conflict.content.dashboard.gitSyncId `shouldBe` Nothing
        otherPid <- createTestProject tr "Other repository owner"
        Just foreignRepo <- connect otherPid "foreign"
        (_, foreignResult) <- testServant tr $ GitSyncPage.dashboardRepositoryPostH testPid other.id (GitSyncPage.DashboardRepositoryForm foreignRepo.id)
        foreignResult.content.assignmentError `shouldBe` Just GitSync.RepositoryMissing

      it "repositoryOwnedDashboardEdit_preservesTheProviderVersionForItsNextPush" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub (Just "https://127.0.0.1:1") "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        did <- UUIDId <$> UUID.nextRandom
        _ <- runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Overview", Dashboards.schema = Just def, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "overview.yaml", Dashboards.fileSha = Just "provider-blob"}
        _ <- testServant tr $ DashboardPage.dashboardYamlPutH testPid did (DashboardPage.YamlForm "title: Overview\nwidgets: []\n")
        _ <- testServant tr $ DashboardPage.dashboardRenamePatchH testPid did (DashboardPage.DashboardRenameForm "Team overview" Nothing)
        Just updated <- runQueryEffect tr $ Dashboards.getDashboardByProjectId testPid did
        updated.title `shouldBe` "Team overview"
        (updated.filePath, updated.fileSha) `shouldBe` (Just "overview.yaml", Just "provider-blob")
        let transport = withHTTPResponses \_ endpoint -> pure $ Just $ if "/contents/" `T.isInfixOf` toText endpoint then "{\"content\":{\"sha\":\"next-blob\"}}" else "{\"sha\":\"head\"}"
        (requests, ()) <- runTestBgRecordingHTTP frozenTime tr $ transport $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncPushDashboard testPid did.unwrap)
        let bodies = [body | (endpoint, body) <- requests, "/contents/" `T.isInfixOf` endpoint]
        map (AE.eitherDecode @AE.Value) bodies `shouldSatisfy` \case
          [Right (AE.Object body)] -> KM.lookup "sha" body == Just (AE.String "provider-blob")
          _ -> False
        Just pushed <- runQueryEffect tr $ Dashboards.getDashboardByProjectId testPid did
        pushed.fileSha `shouldBe` Just "next-blob"

      it "repositoryFirstPush_doesNotSendALocalChecksumAsTheProviderVersion" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub (Just "https://127.0.0.1:1") "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        did <- UUIDId <$> UUID.nextRandom
        _ <- runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Overview", Dashboards.schema = Just def, Dashboards.filePath = Just "overview.yaml", Dashboards.fileSha = Just "local-checksum"}
        let transport = withHTTPResponses \_ endpoint -> pure $ Just $ if "/contents/" `T.isInfixOf` toText endpoint then "{\"content\":{\"sha\":\"provider-blob\"}}" else "{\"sha\":\"head\"}"
        (requests, ()) <- runTestBgRecordingHTTP frozenTime tr $ transport $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncPushDashboard testPid did.unwrap)
        let bodies = [body | (endpoint, body) <- requests, "/contents/" `T.isInfixOf` endpoint]
        map (AE.eitherDecode @AE.Value) bodies `shouldSatisfy` \case
          [Right (AE.Object body)] -> not (KM.member "sha" body)
          _ -> False
        Just pushed <- runQueryEffect tr $ Dashboards.getDashboardByProjectId testPid did
        (pushed.gitSyncId, pushed.fileSha) `shouldBe` (Just sync.id, Just "provider-blob")

      it "repositoryPush_doesNotAdvanceThePullCursorPastOtherRemoteDashboards" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub (Just "https://127.0.0.1:1") "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        did <- UUIDId <$> UUID.nextRandom
        _ <- runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Local", Dashboards.schema = Just def, Dashboards.filePath = Just "local.yaml"}
        let transport = withHTTPResponses \_ endpoint ->
              pure
                $ Just
                $ if "remote.yaml" `T.isInfixOf` toText endpoint
                  then AE.encode $ AE.object ["content" AE..= extractBase64 (B64.encodeBase64 "title: Remote\nwidgets: []\n")]
                  else
                    if "/contents/" `T.isInfixOf` toText endpoint
                      then "{\"content\":{\"sha\":\"local-blob\"}}"
                      else
                        if "/git/trees/" `T.isInfixOf` toText endpoint
                          then "{\"tree\":[{\"path\":\"dashboards/local.yaml\",\"sha\":\"local-blob\",\"type\":\"blob\"},{\"path\":\"dashboards/remote.yaml\",\"sha\":\"remote-blob\",\"type\":\"blob\"}]}"
                          else "{\"sha\":\"head\"}"
        _ <- runTestBgRecordingHTTP frozenTime tr $ transport do
          BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncPushDashboard testPid did.unwrap)
          BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncRepository testPid sync.id)
        gitState <- runQueryEffect tr $ GitSync.getRepositoryDashboardState testPid sync.id
        M.keys gitState `shouldBe` ["local.yaml", "remote.yaml"]

      it "repositoryPicker_connectsSeveralRepositoriesAndPreservesExistingConfiguration" \tr -> do
        let selection = GitSyncPage.RepoSelectForm ["team-a/dashboards", "team-b/dashboards", "team-a/dashboards"] "trunk" (Just "observability") 42
        _ <- testServant tr $ GitSyncPage.githubAppSelectRepoH testPid selection
        syncs <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        sort [(s.owner, s.repo, s.branch, s.pathPrefix) | s <- syncs] `shouldBe` [("team-a", "dashboards", "trunk", "observability"), ("team-b", "dashboards", "trunk", "observability")]
        _ <- testServant tr $ GitSyncPage.githubAppSelectRepoH testPid selection{GitSyncPage.branch = "different", GitSyncPage.pathPrefix = Just "other"}
        unchanged <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        sort [(s.id, s.branch, s.pathPrefix) | s <- unchanged] `shouldBe` sort [(s.id, s.branch, s.pathPrefix) | s <- syncs]

      it "repositoryPull_renamesAnOwnedDashboardWithoutLosingItsIdentity" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub (Just "https://127.0.0.1:1") "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        did <- UUIDId <$> UUID.nextRandom
        _ <- runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Overview", Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "old.yaml", Dashboards.fileSha = Just "same-content"}
        let transport = withHTTPResponses \_ endpoint ->
              pure
                $ Just
                $ if "/git/trees/" `T.isInfixOf` toText endpoint
                  then "{\"tree\":[{\"path\":\"dashboards/new.yaml\",\"sha\":\"same-content\",\"type\":\"blob\"}]}"
                  else "{\"sha\":\"new-head\"}"
        (requests, ()) <- runTestBgRecordingHTTP frozenTime tr $ transport $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncRepository testPid sync.id)
        length requests `shouldBe` 2
        Just renamed <- runQueryEffect tr $ Dashboards.getDashboardByProjectId testPid did
        (renamed.filePath, renamed.fileSha) `shouldBe` (Just "new.yaml", Just "same-content")

      it "repositoryPull_reportsInvalidYamlAndRetriesTheSameRevisionAfterCorrection" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub (Just "https://127.0.0.1:1") "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        let pull yaml =
              runTestBgRecordingHTTP frozenTime tr
                $ withHTTPResponses
                  ( \_ endpoint ->
                      pure
                        $ Just
                        $ if "/git/trees/" `T.isInfixOf` toText endpoint
                          then "{\"tree\":[{\"path\":\"dashboards/overview.yaml\",\"sha\":\"blob\",\"type\":\"blob\"}]}"
                          else
                            if "/contents/" `T.isInfixOf` toText endpoint
                              then AE.encode $ AE.object ["content" AE..= extractBase64 (B64.encodeBase64 yaml)]
                              else "{\"sha\":\"head\"}"
                  )
                $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncRepository testPid sync.id)
        _ <- pull "title: [invalid"
        Just failed <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        failed.lastRevision `shouldBe` Nothing
        failed.lastError `shouldSatisfy` maybe False (T.isInfixOf "overview.yaml")
        _ <- pull "title: Recovered\nwidgets: []\n"
        Just recovered <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        (recovered.lastRevision, recovered.lastError) `shouldBe` (Just "head", Nothing)
        dashboards <- runQueryEffect tr $ GitSync.getRepositoryDashboardState testPid sync.id
        M.keys dashboards `shouldBe` ["overview.yaml"]

      -- Without a credential there is nothing to read repos through, so the page must point at
      -- the control that fixes that rather than offer a form whose every submission is discarded.
      it "asks for a GitHub connection before it asks for a mapping" \tr -> do
        (_, html) <- testServant tr $ PageCodeContext.codeMappingsGetH testPid Nothing
        let out = render html
        out `shouldSatisfy` T.isInfixOf "Connect GitHub"
        -- The install has to come back here, not to dashboard sync: they are one grant but
        -- two destinations, and landing on the wrong one is how the old flow made source
        -- context look like it required configuring YAML sync.
        out `shouldSatisfy` T.isInfixOf "git-sync/install?to=code"
        out `shouldNotSatisfy` T.isInfixOf "Link repository"

      it "maps several services onto several repos, and resolves each frame to its own" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        -- The config-sync repo. It is the project's monoscope YAML, NOT where the services live —
        -- the whole point of 0124 is that these are different repositories.
        _ <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "acme" "monoscope-config" "main" (GitSync.PersonalToken "ghp_test") Nothing ""

        let add repo svc prefix root =
              testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just repo) (Just "main") svc Nothing (Just prefix) (Just root) Nothing)
        _ <- add "checkout-service" (Just "checkout") "/srv/app/" "src"
        (_, added) <- add "billing-service" (Just "billing") "/opt/billing/" ""

        -- The account came from the installation already granted for config sync; the repos did
        -- not, and neither is the config repo.
        let out = render added
        out `shouldSatisfy` T.isInfixOf "acme/checkout-service"
        out `shouldSatisfy` T.isInfixOf "acme/billing-service"
        out `shouldNotSatisfy` T.isInfixOf "monoscope-config"
        -- The row has to say what the mapping covers in frame-path terms, not print the four
        -- columns it is stored as.
        out `shouldSatisfy` T.isInfixOf "frames under /srv/app/"

        mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        sort (map (.repo) mappings) `shouldBe` ["billing-service", "checkout-service"]

        -- The rewrite is the whole feature, and it must send each service's frame to its own
        -- repo rather than to whichever mapping sorted first.
        let repoFor svc path = (\(cm, p) -> (cm.repo, p)) <$> CodeContext.resolveRepoPath mappings (Just svc) path
        repoFor "checkout" "/srv/app/services/checkout.py" `shouldBe` Just ("checkout-service", "src/services/checkout.py")
        repoFor "billing" "/opt/billing/invoice.rb" `shouldBe` Just ("billing-service", "invoice.rb")
        -- A service whose frames no mapping claims resolves to nothing rather than to the other
        -- service's repo — a snippet from the wrong repo is worse than no snippet.
        repoFor "search" "/srv/search/index.go" `shouldBe` Nothing

        case mappings of
          (cm : _) -> do
            (_, afterDelete) <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsDeleteH testPid cm.id
            render afterDelete `shouldNotSatisfy` T.isInfixOf (cm.owner <> "/" <> cm.repo)
            runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= \ms -> length ms `shouldBe` 1
          [] -> expectationFailure "expected two mappings, got none"

      it "repositoryAccounts_offerBothGrantsEvenWhenOneAlsoSyncsDashboards" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        _ <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "zzz-ours" "monoscope-config" "main" (GitSync.PersonalToken "ghp_test") Nothing ""
        for_ ["aaa-stray", "zzz-ours"] \account -> do
          Just credential <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing account (Just 222) Nothing
          Cache.insert tr.trATCtx.repoListCache credential.id []
        out <- render . snd <$> testServant tr (withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsGetH testPid Nothing)
        for_ (["zzz-ours", "aaa-stray", "Choose an account"] :: [Text]) \label ->
          T.isInfixOf label out `shouldBe` True

      -- A frame that no mapping covers is exactly when someone needs the mapping form, and
      -- the one thing they cannot be expected to retype is the path they were just looking at.
      it "carries the unmapped frame's path into the form" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        _ <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "acme" "monoscope-config" "main" (GitSync.PersonalToken "ghp_test") Nothing ""

        (_, unmapped) <- testServant tr $ PageCodeContext.codeContextH testPid (Just "/srv/app/checkout.py") (Just 88) Nothing Nothing
        render unmapped `shouldSatisfy` T.isInfixOf "repositories?tab=configuration&amp;sample=/srv/app/checkout.py"

        (_, form) <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsGetH testPid (Just "/srv/app/checkout.py")
        render form `shouldSatisfy` T.isInfixOf "value=\"/srv/app/checkout.py\""

    describe "Frame source endpoint (codeContextH)" do
      -- A project that never configured a mapping opens an error panel like anyone else. It
      -- must see a sentence, not a failure it did not cause — and the sentence has to say
      -- which of the two possible problems it is.
      it "explains an unresolvable frame instead of failing the panel" \tr -> do
        (_, unmapped) <- testServant tr $ PageCodeContext.codeContextH testPid (Just "/srv/app/checkout.py") (Just 88) Nothing Nothing
        render unmapped `shouldSatisfy` T.isInfixOf "No linked repository covers /srv/app/checkout.py"

        (_, noLine) <- testServant tr $ PageCodeContext.codeContextH testPid (Just "/srv/app/checkout.py") Nothing Nothing Nothing
        render noLine `shouldSatisfy` T.isInfixOf "no file and line"
