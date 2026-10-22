module Pages.CodeContextSpec (spec) where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AET
import Data.Base64.Types (extractBase64)
import Data.ByteString qualified as BS
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
import Models.Apis.MonitorDefinitions qualified as MonitorDefinitions
import Models.Projects.CodeContext qualified as CodeContext
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ImpactReviews qualified as ImpactReviews
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.ProjectMemory qualified as Memory
import Models.Projects.Projects qualified as Projects
import Models.Projects.RepositorySync qualified as RepositorySync
import Network.HTTP.Client (HttpException (InvalidUrlException))
import Network.HTTP.Client.Internal (Response (..), ResponseClose (..), createCookieJar, defaultRequest)
import Network.HTTP.Types qualified as H
import Network.HTTP.Types.Status (Status (..))
import Network.HTTP.Types.URI (urlEncode)
import Network.HTTP.Types.Version (http11)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Test qualified as WT
import OddJobs.ConfigBuilder (mkConfig)
import OddJobs.Job qualified as Jobs
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
import Servant (getHeaders, getResponse)
import Servant.Server.Generic (genericServeTWithContext)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Process.Typed (proc, readProcessStdout_, setEnv)
import System.Timeout (timeout)
import System.Types (ATBackgroundCtx)
import Test.Hspec
import UnliftIO.Async (cancel, concurrently, wait, waitCatch, withAsync)
import UnliftIO.Exception (bracket, throwIO, try)
import Web.Routes qualified as Routes


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

    uncached <- render . Lucid.toHtml . snd <$> testServant tr (withUnavailableRepositoryAPI $ PageCodeContext.repositoryConnectGetH testPid (Just cred.id))
    uncached `shouldNotSatisfy` T.isInfixOf "sentinel-repo"

    Cache.insert tr.trATCtx.repoListCache cred.id [Git.GitRepo "acme/sentinel-repo" "sentinel-repo" False "trunk"]
    cached <- snd <$> testServant tr (withUnavailableRepositoryAPI $ PageCodeContext.repositoryConnectGetH testPid (Just cred.id))
    render (Lucid.toHtml cached) `shouldSatisfy` T.isInfixOf "sentinel-repo"
    cached.content.repositories `shouldBe` Right [Git.GitRepo "acme/sentinel-repo" "sentinel-repo" False "trunk"]


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


spec :: Spec
spec = do
  revisionWiringSpec
  snippetCacheSpec
  repoListCacheSpec
  around withTestResources do
    describe "Project wiki" do
      it "shares a project wiki with immutable revisions, conflict-safe edits and project permissions" \tr -> do
        let user = (getResponse tr.trSessAndHeader :: Projects.Session).user.id
            page = UUIDId $ UUID.fromWords 50 0 0 1 :: Memory.PageId
            app =
              genericServeTWithContext
                (effToServantHandlerTest tr.trTestClock tr.trUUIDRef tr.trATCtx tr.trLogger tr.trTracerProvider)
                (Routes.server tr.trLogger tr.trATCtx tr.trTracerProvider (\_ _ -> pure $ Left "unused") (\_ _ -> pure $ Left "unused"))
                (Routes.genAuthServerContext tr.trLogger tr.trATCtx)
            base pid = "/p/" <> pid.toText <> "/memories"
            authorization = ("Authorization", "Bearer " <> UUID.toASCIIBytes (getResponse tr.trSessAndHeader).persistentSession.id.getPersistentSessionId)
            request method path fields =
              WT.runSession
                ( WT.srequest
                    $ WT.SRequest
                      (WT.setPath Wai.defaultRequest{Wai.requestMethod = method, Wai.requestHeaders = [authorization, ("Content-Type", "application/x-www-form-urlencoded")]} $ encodeUtf8 path)
                      (fromStrict $ BS.intercalate "&" [key <> "=" <> urlEncode True (encodeUtf8 value) | (key, value) <- fields])
                )
                app
            save pid expected title body = request H.methodPost (base pid) [("pageId", page.toText), ("version", show @Text expected), ("title", title), ("body", body)]
            html = decodeUtf8 @Text . (.simpleBody)
        created <- save testPid (0 :: Word32) "Retry behavior" "Keep retry tokens. <script>alert('wiki')</script>"
        H.statusCode created.simpleStatus `shouldBe` 200
        html created `shouldSatisfy` T.isInfixOf "Keep retry tokens."
        html created `shouldNotSatisfy` T.isInfixOf "<script>alert('wiki')</script>"
        firstHistory <- runQueryEffect tr $ Memory.pageHistory testPid page
        map (.version) firstHistory `shouldBe` [1]
        map (.author) firstHistory `shouldBe` [Memory.Member user]
        (firstEdit, secondEdit) <- concurrently (save testPid 1 "Retry behavior" "First concurrent edit") (save testPid 1 "Retry behavior" "Second concurrent edit")
        length (filter (T.isInfixOf "This page changed" . html) [firstEdit, secondEdit]) `shouldBe` 1
        history <- runQueryEffect tr $ Memory.pageHistory testPid page
        map (.version) history `shouldBe` [2, 1]
        map (.document) (drop 1 history) `shouldBe` map (.document) firstHistory
        index <- request H.methodGet (base testPid) []
        html index `shouldSatisfy` T.isInfixOf "Retry behavior"
        viewed <- request H.methodGet (base testPid <> "?page=" <> page.toText) []
        html viewed `shouldSatisfy` T.isInfixOf "Revision history"
        html viewed `shouldSatisfy` T.isInfixOf "Keep retry tokens. &lt;script&gt;"
        invalid <- save testPid 2 " " "Invalid title draft"
        html invalid `shouldSatisfy` T.isInfixOf "Invalid title draft"
        length <$> runQueryEffect tr (Memory.pageHistory testPid page) `shouldReturn` 2
        for_ [(T.replicate 121 "x", "Oversized title"), ("Blank body", " "), ("Oversized body", T.replicate 16001 "x"), ("Invalid body", "bad\NULbody")] \(title, body) -> do
          rejected <- save testPid 2 title body
          H.statusCode rejected.simpleStatus `shouldBe` 200
          html rejected `shouldSatisfy` T.isInfixOf "characters without null bytes"
          length <$> runQueryEffect tr (Memory.pageHistory testPid page) `shouldReturn` 2
        other <- createTestProject tr "Isolated memory project"
        hidden <- request H.methodGet (base other <> "?page=" <> page.toText) []
        H.statusCode hidden.simpleStatus `shouldBe` 404
        runQueryEffect tr (Memory.pageHistory other page) `shouldReturn` []
        void $ withResource tr.trPool \conn -> PG.execute conn "UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?" (ProjectMembers.PView, testPid, user)
        readOnly <- request H.methodGet (base testPid <> "?page=" <> page.toText) []
        H.statusCode readOnly.simpleStatus `shouldBe` 200
        html readOnly `shouldNotSatisfy` T.isInfixOf "Save page"
        refused <- save testPid 2 "Retry behavior" "Viewer must not write"
        H.statusCode refused.simpleStatus `shouldBe` 403
        length <$> runQueryEffect tr (Memory.pageHistory testPid page) `shouldReturn` 2
        anonymous <- WT.runSession (WT.request $ WT.setPath Wai.defaultRequest $ encodeUtf8 $ base testPid) app
        H.statusCode anonymous.simpleStatus `shouldSatisfy` (`elem` [302, 401, 403, 404])
        demo <- WT.runSession (WT.request $ WT.setPath Wai.defaultRequest $ encodeUtf8 $ "/p/" <> testPid.toText <> "/repositories") app
        html demo `shouldNotSatisfy` T.isInfixOf "?tab=wiki"

      it "wiki_saveAndNavigate_doesNotNestHtmxTargets" \tr -> do
        available <- doesFileExist "e2e/node_modules/playwright/index.mjs"
        unless available $ pendingWith "Install the repository e2e browser dependencies to run the wiki browser regression"
        environment <- filter (\(key, _) -> key `elem` ["PATH", "HOME", "TMPDIR", "XDG_CACHE_HOME", "PLAYWRIGHT_BROWSERS_PATH"]) <$> getEnvironment
        probe <- readProcessStdout_ $ setEnv environment $ proc "node" ["test/fixtures/wiki-browser.mjs", "--probe"]
        browser <- either fail pure $ AE.eitherDecode @Bool probe
        unless browser $ pendingWith "Install the repository Playwright browser to run the wiki browser regression"
        let app =
              genericServeTWithContext
                (effToServantHandlerTest tr.trTestClock tr.trUUIDRef tr.trATCtx tr.trLogger tr.trTracerProvider)
                (Routes.server tr.trLogger tr.trATCtx tr.trTracerProvider (\_ _ -> pure $ Left "unused") (\_ _ -> pure $ Left "unused"))
                (Routes.genAuthServerContext tr.trLogger tr.trATCtx)
            token = UUID.toString (getResponse tr.trSessAndHeader).persistentSession.id.getPersistentSessionId
        Warp.testWithApplication (pure app) \port -> do
          output <- readProcessStdout_ $ setEnv (("MONOSCOPE_TEST_BEARER", token) : environment) $ proc "node" ["test/fixtures/wiki-browser.mjs", "http://127.0.0.1:" <> show port <> "/p/" <> toString testPid.toText <> "/memories"]
          value <- either fail pure $ AE.eitherDecode @AE.Value output
          AET.parseEither (AE.withObject "Browser result" (AE..: "targets")) value `shouldBe` Right ([1, 1, 1] :: [Int])
          pages <- runQueryEffect tr $ Memory.latestPages testPid
          map (Memory.memoryText . (.document.body)) pages `shouldBe` ["Keep caller retry tokens."]

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
      it "githubCallback_verifiesTheSessionAttemptAndIgnoresForgedOrReplayedInstallationIds" \tr -> do
        let configured = tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppId = "123", githubClientId = "fixture-client", githubClientSecret = "fixture-secret"}}}
            session = (getResponse tr.trSessAndHeader).persistentSession.id
            provider :: (IOE :> es, W.HTTP :> es) => Eff es a -> Eff es a
            provider = interpose @W.HTTP \_ -> \case
              W.PostWith _ "https://github.com/login/oauth/access_token" _ -> pure $ httpResponse "{\"access_token\":\"private-user-token\"}"
              W.GetWith _ "https://api.github.com/user/installations?per_page=100&page=1" -> pure $ httpResponse "{\"installations\":[{\"id\":42,\"app_id\":123,\"account\":{\"login\":\"team\",\"type\":\"User\"}}]}"
              W.GetWith _ "https://api.github.com/user" -> pure $ httpResponse "{\"login\":\"team\"}"
              _ -> liftIO $ expectationFailure "Unexpected authorization request" >> throwIO (InvalidUrlException "https://git.invalid" "Unexpected authorization request")
        (_, forged) <- testServant configured $ withUnavailableRepositoryAPI $ GitSyncPage.githubAppCallbackH (Just 42) Nothing (Just $ testPid.toText <> ":code") Nothing
        render forged `shouldSatisfy` T.isInfixOf "missing, expired, or already used"
        runQueryEffect tr (GitSync.getGitHubCredentials testPid) >>= (`shouldSatisfy` null)
        (installState, authorize) <- runAuthHandler configured do
          initialState <- GitSync.createInstallationAttempt session (GitSync.InstallationAttempt testPid GitSync.InstallCode GitSync.InstallApp)
          response <- withUnavailableRepositoryAPI $ GitSyncPage.githubAppCallbackH (Just 42) Nothing (Just $ UUID.toText initialState) Nothing
          pure (initialState, getResponse response)
        render authorize `shouldSatisfy` T.isInfixOf "https://github.com/login/oauth/authorize?client_id=fixture-client"
        runQueryEffect tr (GitSync.consumeInstallationAttempt session installState) `shouldReturn` Nothing
        Just (HI.OneColumn nonce) <- runQueryEffect tr $ Hasql.interpOne @(HI.OneColumn UUID.UUID) [HI.sql| SELECT id FROM projects.github_installation_attempts WHERE project_id = #{testPid} AND session_id = #{session} AND installation_id = 42 |]
        nonce `shouldNotBe` installState
        (_, verified) <- testServant configured $ provider $ GitSyncPage.githubAppCallbackH (Just 999999) Nothing (Just $ UUID.toText nonce) (Just "private-code")
        let html = render verified
        html `shouldSatisfy` T.isInfixOf ("/p/" <> testPid.toText <> "/repositories/connect")
        for_ ["private-user-token", "private-code", "fixture-secret"] \secret -> html `shouldNotSatisfy` T.isInfixOf secret
        [account] <- runQueryEffect tr $ GitSync.getGitHubCredentials testPid
        account.installationId `shouldBe` Just 42
        (_, replay) <- testServant configured $ withUnavailableRepositoryAPI $ GitSyncPage.githubAppCallbackH (Just 42) Nothing (Just $ UUID.toText nonce) (Just "private-code")
        render replay `shouldSatisfy` T.isInfixOf "missing, expired, or already used"
        runQueryEffect tr (GitSync.getGitSyncs testPid) >>= (`shouldSatisfy` null)

      it "repositoryTokenConnection_checksAccessWithoutEnablingSyncAndRequiresExplicitReplacement" \tr -> do
        let form = PageCodeContext.RepositoryTokenForm Git.GitHub Nothing "team/checkout" "fixture-token" Nothing
            metadata :: (IOE :> es, W.HTTP :> es) => Eff es a -> Eff es a
            metadata = interpose @W.HTTP \_ -> \case
              W.GetWith _ endpoint -> do
                liftIO $ endpoint `shouldBe` "https://api.github.com/repos/team/checkout"
                pure $ httpResponse "{\"full_name\":\"team/checkout\",\"private\":true,\"default_branch\":\"trunk\"}"
              _ -> liftIO $ throwIO $ InvalidUrlException "https://git.invalid" "Token connection must only read repository metadata"
            encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        (_, rejected) <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.repositoryTokenPostH testPid form
        rejected.content.connectionError `shouldSatisfy` isJust
        rejected.content.repoFullName `shouldBe` "team/checkout"
        render (Lucid.toHtml rejected.content) `shouldNotSatisfy` T.isInfixOf "fixture-token"
        (_, incomplete) <- testServant tr $ interpose @W.HTTP (\_ -> \case W.GetWith _ _ -> pure $ httpResponse "{\"full_name\":\"\"}"; _ -> liftIO $ throwIO $ InvalidUrlException "https://git.invalid" "Unexpected request") $ PageCodeContext.repositoryTokenPostH testPid form
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
        let savedToken = runQueryEffect tr (GitSync.getGitHubCredential encKey testPid account.id) <&> (>>= (.accessToken))
        savedToken `shouldReturn` Just "fixture-token"
        runQueryEffect tr (GitSync.saveTokenCredential encKey testPid Git.GitHub Nothing "team" "concurrent-token" Nothing) >>= (`shouldSatisfy` isNothing)
        savedToken `shouldReturn` Just "fixture-token"
        (_, conflict) <- testServant tr $ metadata $ PageCodeContext.repositoryTokenPostH testPid form{PageCodeContext.accessToken = "replacement-token"}
        conflict.content.connectionError `shouldSatisfy` isJust
        savedToken `shouldReturn` Just "fixture-token"
        _ <- testServant tr $ metadata $ PageCodeContext.repositoryTokenPostH testPid form{PageCodeContext.accessToken = "replacement-token", PageCodeContext.replaceToken = Just True}
        savedToken `shouldReturn` Just "replacement-token"
        runQueryEffect tr (GitSync.saveTokenCredential encKey testPid Git.GitHub Nothing "team" "concurrent-token" (Just account)) >>= (`shouldSatisfy` isNothing)
        savedToken `shouldReturn` Just "replacement-token"
        (_, source) <- testServant tr $ metadata $ PageCodeContext.repositorySourceGetH testPid repository.id Nothing Nothing
        render source `shouldSatisfy` T.isInfixOf "value=\"trunk\""
        (_, picker) <-
          testServant tr
            $ interpose @W.HTTP
              ( \_ -> \case
                  W.GetWith _ endpoint -> do
                    liftIO $ endpoint `shouldBe` "https://api.github.com/user/repos?per_page=100&page=1"
                    pure $ httpResponse "[{\"full_name\":\"team/checkout\",\"private\":true,\"default_branch\":\"trunk\"}]"
                  _ -> liftIO $ throwIO $ InvalidUrlException "https://git.invalid" "Token repository picker must only GET"
              )
            $ PageCodeContext.repositoryConnectGetH testPid (Just account.id)
        picker.content.repositories `shouldBe` Right [Git.GitRepo "team/checkout" "checkout" True "trunk"]
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql| UPDATE projects.git_credentials SET installation_id = 42, access_token = NULL WHERE id = #{account.id} |]
        runQueryEffect tr (GitSync.saveTokenCredential encKey testPid Git.GitHub Nothing "team" "concurrent-token" (Just account)) >>= (`shouldSatisfy` isNothing)
        (_, appDenied) <- testServant tr $ metadata $ PageCodeContext.repositoryTokenPostH testPid form{PageCodeContext.replaceToken = Just True}
        appDenied.content.connectionError `shouldSatisfy` isJust
        Just app <- runQueryEffect tr $ GitSync.getGitHubCredential encKey testPid account.id
        app.installationId `shouldBe` Just 42

      it "repositoryTokenConnection_supportsGitLabNamespacesGiteaAndBitbucketMetadata" \tr -> do
        for_
          ([(Git.GitLab, Nothing, "team/platform/checkout", "{\"path_with_namespace\":\"team/platform/checkout\",\"visibility\":\"private\",\"default_branch\":\"stable\"}"), (Git.Gitea, Just "https://git.example.com", "team/checkout", "{\"full_name\":\"team/checkout\",\"private\":true,\"default_branch\":\"stable\"}"), (Git.Bitbucket, Nothing, "team/checkout", "{\"full_name\":\"team/checkout\",\"is_private\":true,\"mainbranch\":{\"name\":\"stable\"}}")] :: [(Git.GitHost, Maybe Text, Text, LBS.ByteString)])
          \(host, origin, name, body) -> do
            let metadata :: (IOE :> es, W.HTTP :> es) => Eff es a -> Eff es a
                metadata = interpose @W.HTTP \_ -> \case W.GetWith _ _ -> pure $ httpResponse body; _ -> liftIO $ throwIO $ InvalidUrlException "https://git.invalid" "Token connection must only read metadata"
            _ <- testServant tr $ metadata $ PageCodeContext.repositoryTokenPostH testPid (PageCodeContext.RepositoryTokenForm host origin name "fixture-token" Nothing)
            repositories <- runQueryEffect tr $ GitSync.getRepositories testPid
            repository <- maybe (fail "Expected repository for this Git host") pure $ find ((== host) . (.host)) repositories
            (repository.owner, repository.repo) `shouldBe` Git.splitFullName name
            (_, source) <- testServant tr $ metadata $ PageCodeContext.repositorySourceGetH testPid repository.id Nothing Nothing
            render source `shouldSatisfy` T.isInfixOf "value=\"stable\""
        runQueryEffect tr (GitSync.getGitSyncs testPid) >>= (`shouldSatisfy` null)

      it "repositorySettings_readOnlyMembersSeeMappingsWithoutEditControlsOrProviderRequests" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
            uid = (getResponse tr.trSessAndHeader).user.id
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "team" (Just 42) Nothing
        runQueryEffect tr $ CodeContext.insertCodeMapping testPid account.id (Git.RepoRef "team" "checkout" "main") (Just "checkout") "/app/" "src"
        [repository] <- runQueryEffect tr $ GitSync.getRepositories testPid
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql| UPDATE projects.project_members SET permission = 'view' WHERE project_id = #{testPid} AND user_id = #{uid} |]
        (_, details) <- testServant tr $ PageCodeContext.repositoryGetH testPid repository.id
        render (Lucid.toHtml details.content) `shouldSatisfy` T.isInfixOf "View source context"
        render (Lucid.toHtml details.content) `shouldNotSatisfy` T.isInfixOf "Configure source context"
        let noProvider = interpose @W.HTTP \_ _ -> liftIO $ expectationFailure "Read-only repository settings must not contact the provider" >> throwIO (InvalidUrlException "https://git.invalid" "Unexpected provider call")
        for_ [PageCodeContext.repositoriesGetH testPid (Just PageCodeContext.Overview) Nothing, PageCodeContext.repositorySourceGetH testPid repository.id Nothing Nothing, PageCodeContext.codeMappingsEditorGetH testPid (Just account.id) Nothing] \handler -> do
          (_, page) <- testServant tr $ noProvider handler
          let html = render page
          html `shouldSatisfy` T.isInfixOf "team/checkout"
          html `shouldNotSatisfy` T.isInfixOf "name=\"credentialId\""
          for_ ["Unlink", "Link repository", "Save review settings", "Install GitHub App", "Connect with token"] \control -> html `shouldNotSatisfy` T.isInfixOf control

      it "repositoryConnections_githubCasingDoesNotDuplicateConnectionsOrSyncs" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "team" (Just 42) Nothing
        Just sameAccount <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "TEAM" (Just 42) Nothing
        sameAccount.id `shouldBe` account.id
        for_ (["Team/Checkout", "team/checkout"] :: [Text]) \name -> do
          Cache.insert tr.trATCtx.repoListCache account.id [Git.GitRepo name "checkout" True "main"]
          void $ testServant tr $ PageCodeContext.repositoryConnectPostH testPid (Just account.id) (PageCodeContext.RepositoryConnectForm [name])
        repositories <- runQueryEffect tr $ GitSync.getRepositories testPid
        map (\r -> (r.owner, r.repo)) repositories `shouldBe` [("team", "checkout")]
        for_ ([("Team", "Checkout"), ("team", "checkout")] :: [(Text, Text)]) \(owner, repo) ->
          void $ testServant tr $ GitSyncPage.gitSyncSettingsPostH testPid GitSyncPage.GitSyncForm{host = Just Git.GitHub, apiBase = Nothing, owner, repo, branch = "main", accessToken = "fixture-token", webhookSecret = Just "fixture-secret", pathPrefix = Nothing}
        syncs <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        map (\s -> (s.owner, s.repo)) syncs `shouldBe` [("team", "checkout")]
        matching <- runQueryEffect tr $ GitSync.getGitSyncsByRepo Git.GitHub "TEAM" "CHECKOUT"
        map (.id) matching `shouldBe` map (.id) syncs
        runQueryEffect tr $ CodeContext.insertCodeMapping testPid account.id (Git.RepoRef "TEAM" "CHECKOUT" "main") (Just "checkout") "/app/" "src"
        mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        map (\m -> (m.owner, m.repo)) mappings `shouldBe` [("team", "checkout")]
        Just gitlab <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitLab Nothing "Team" Nothing (Just "fixture")
        for_ (["Team/Checkout", "team/checkout"] :: [Text]) \name -> void $ runQueryEffect tr $ GitSync.connectRepository testPid gitlab.id (Git.GitRepo name "checkout" True "main")
        allRepositories <- runQueryEffect tr $ GitSync.getRepositories testPid
        sort [(r.owner, r.repo) | r <- allRepositories, r.host == Git.GitLab] `shouldBe` [("Team", "Checkout"), ("team", "checkout")]

      it "githubIdentityMigration_mergesCaseAliasesWithoutDeletingLocalDashboards" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "team" (Just 42) Nothing
        Just alias <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "other" Nothing (Just "fixture")
        for_ ([(account.id, "team"), (alias.id, "other")] :: [(GitSync.GitHubCredentialId, Text)]) \(cid, owner) ->
          runQueryEffect tr $ CodeContext.insertCodeMapping testPid cid (Git.RepoRef owner "checkout" "main") (Just owner) ("/" <> owner <> "/") "src"
        syncs <- forM (["team", "other"] :: [Text]) \owner -> do
          Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing owner "checkout" "main" (GitSync.AppInstallation 42) Nothing ""
          did <- UUIDId <$> UUID.nextRandom
          void $ runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = owner, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "overview.yaml", Dashboards.fileSha = Just "blob"}
          pure sync
        migration <- Query <$> readFileBS "static/migrations/0223_github_repository_identity.sql"
        withResource tr.trPool \conn -> PG.withTransaction conn do
          void $ PG.execute_ conn "ALTER TABLE projects.git_credentials DROP CONSTRAINT git_credentials_canonical_account; ALTER TABLE projects.repositories DROP CONSTRAINT repositories_canonical_name; ALTER TABLE projects.git_sync DROP CONSTRAINT git_sync_canonical_name; DROP FUNCTION projects.git_identity_name(TEXT, TEXT)"
          void $ PG.execute conn "UPDATE projects.git_credentials SET account = 'TEAM' WHERE id = ?" (PG.Only alias.id)
          void $ PG.execute conn "UPDATE projects.repositories SET owner = 'TEAM', repo = 'CHECKOUT' WHERE project_id = ? AND owner = 'other'" (PG.Only testPid)
          void $ PG.execute conn "UPDATE projects.git_sync SET owner = 'TEAM', repo = 'CHECKOUT' WHERE project_id = ? AND owner = 'other'" (PG.Only testPid)
          void $ PG.execute conn "UPDATE projects.code_mappings SET owner = 'TEAM', repo = 'CHECKOUT' WHERE credential_id = ?" (PG.Only alias.id)
          void $ PG.execute_ conn migration
        accounts <- runQueryEffect tr $ GitSync.getGitHubCredentials testPid
        map (.id) accounts `shouldBe` [account.id]
        repositories <- runQueryEffect tr $ GitSync.getRepositories testPid
        map (\r -> (r.owner, r.repo, r.credentialId)) repositories `shouldBe` [("team", "checkout", Just account.id)]
        mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        map (\m -> (m.owner, m.repo, m.credentialId)) mappings `shouldBe` replicate 2 ("team", "checkout", account.id)
        remaining <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        length remaining `shouldBe` 1
        retained <- runQueryEffect tr $ Dashboards.selectDashboardsSortedBy testPid Dashboards.Asc Dashboards.DSTitle
        sort (map (.title) retained) `shouldBe` ["other", "team"]
        length (filter (isJust . (.gitSyncId)) retained) `shouldBe` 1
        mapMaybe (.gitSyncId) retained `shouldSatisfy` all (`elem` map (.id) syncs)

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
        (_, connection) <- testServant tr $ PageCodeContext.repositoryConnectGetH testPid Nothing
        render (Lucid.toHtml connection.content) `shouldSatisfy` T.isInfixOf ("href=\"/p/" <> testPid.toText <> "/repositories/connect/token\"")
        (_, html) <- testServant tr $ PageCodeContext.codeMappingsGetH testPid Nothing
        let out = render html
        out `shouldSatisfy` T.isInfixOf ("href=\"/p/" <> testPid.toText <> "/repositories\"")
        out `shouldSatisfy` T.isInfixOf "Pull requests"
        out `shouldSatisfy` T.isInfixOf "Repositories"

      it "sourceRepositoryPicker_preservesTheSelectedNamespaceInsteadOfUsingTheAccountName" \tr -> do
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitLab (Just "https://git.example.com") "team" Nothing (Just "fixture")
        Cache.insert tr.trATCtx.repoListCache account.id [Git.GitRepo "team/platform/checkout" "checkout" True "trunk"]
        (_, picker) <- testServant tr $ PageCodeContext.codeMappingsEditorGetH testPid (Just account.id) Nothing
        render picker `shouldSatisfy` T.isInfixOf "value=\"team/platform/checkout\""
        _ <- testServant tr $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just "team/platform/checkout") (Just "trunk") (Just "checkout") Nothing Nothing Nothing (Just account.id))
        [mapping] <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        (mapping.owner, mapping.repo) `shouldBe` ("team/platform", "checkout")

      it "repositoryConfiguration_usesTheSingleListInsteadOfDuplicateConnectionForms" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "team" (Just 42) Nothing
        Just repository <- runQueryEffect tr $ GitSync.connectRepository testPid account.id (Git.GitRepo "team/checkout" "checkout" True "main")
        (_, legacy) <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.repositoriesGetH testPid (Just PageCodeContext.Overview) Nothing
        let html = render legacy
        html `shouldSatisfy` T.isInfixOf ("/repositories/" <> repository.id.toText)
        for_ ["Connect a dashboard repository", "Link repository", ">Configuration<"] \label -> html `shouldNotSatisfy` T.isInfixOf label
        html `shouldSatisfy` T.isInfixOf "Source context"

      it "repositoryAnalysis_transientFailureAfterSetupDoesNotRestoreRemovedServiceLinks" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "team" Nothing (Just "fixture")
        Just repository <- runQueryEffect tr $ GitSync.connectRepository testPid account.id (Git.GitRepo "team/checkout" "checkout" True "main")
        withResource tr.trPool \conn -> void $ PG.execute conn "INSERT INTO apis.schema_summary (project_id, doc) VALUES (?, '{\"fields\":{},\"services\":[\"checkout\",\"billing\"],\"top_values_by_field\":{}}')" (PG.Only testPid)
        let analyze = BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.AnalyzeRepository testPid repository.id)
            provider = withHTTPResponses \_ endpoint -> pure $ Just if "/commits/" `T.isInfixOf` toText endpoint then "{\"sha\":\"head\"}" else if "/git/trees/" `T.isInfixOf` toText endpoint then "{\"tree\":[{\"path\":\"config/monitors\",\"type\":\"tree\",\"sha\":\"folder\"}]}" else "{\"full_name\":\"team/checkout\",\"name\":\"checkout\",\"private\":true,\"default_branch\":\"main\"}"
        void $ runTestBgRecordingHTTP frozenTime tr $ withUnavailableRepositoryAPI analyze
        Just failed <- runQueryEffect tr $ GitSync.getRepositoryAnalysis testPid repository.id
        failed.error `shouldSatisfy` isJust
        void $ runTestBgRecordingHTTP frozenTime tr $ provider analyze
        mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        map (.service) mappings `shouldBe` [Just "checkout"]
        [sync] <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        (sync.branch, sync.pathPrefix, sync.syncEnabled) `shouldBe` ("main", "config", True)
        _ <- testServant tr $ PageCodeContext.repositorySourceEnabledPostH testPid repository.id (PageCodeContext.SourceContextForm False)
        void $ runQueryEffect tr $ GitSync.pauseGitSync testPid sync.id
        for_ mappings $ \mapping -> runQueryEffect tr $ CodeContext.deleteCodeMapping testPid mapping.id
        void $ runTestBgRecordingHTTP frozenTime tr $ withUnavailableRepositoryAPI analyze
        void $ runTestBgRecordingHTTP frozenTime tr $ provider analyze
        Just current <- runQueryEffect tr $ GitSync.getRepository testPid repository.id
        current.sourceEnabled `shouldBe` False
        Just paused <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        paused.syncEnabled `shouldBe` False
        runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= \rows -> length rows `shouldBe` 0
        Just ambiguous <- runQueryEffect tr $ GitSync.connectRepository testPid account.id (Git.GitRepo "team/other" "other" True "main")
        let ambiguousProvider = withHTTPResponses \_ endpoint -> pure $ if "/git/trees/" `T.isInfixOf` toText endpoint then Just "{\"tree\":[{\"path\":\"a/dashboards\",\"type\":\"tree\",\"sha\":\"a\"},{\"path\":\"b/monitors\",\"type\":\"tree\",\"sha\":\"b\"}]}" else Nothing
        void $ runTestBgRecordingHTTP frozenTime tr $ provider $ ambiguousProvider $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.AnalyzeRepository testPid ambiguous.id)
        runQueryEffect tr (GitSync.getGitSyncs testPid) >>= \rows -> length rows `shouldBe` 1
        runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= \rows -> length rows `shouldBe` 0

      it "configurationFolder_roundTripsAllMonitorDefinitionsAndPreservesRuntimeState" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "team" "checkout" "main" (GitSync.AppInstallation 42) Nothing ""
        let query = def{MonitorDefinitions.title = "Errors", MonitorDefinitions.query = "status_code >= 500 | summarize count()", MonitorDefinitions.alertThreshold = 5, MonitorDefinitions.timeWindowMins = 5, MonitorDefinitions.checkIntervalMins = 1}
            definitions = [RepositorySync.QueryDefinition query, RepositorySync.UptimeDefinitionConfig $ RepositorySync.UptimeDefinition "Site" "https://example.com" 60 200 True, RepositorySync.CronDefinitionConfig $ RepositorySync.CronDefinition "daily-job" "Daily job" 3600 60]
        for_ definitions \definition -> do
          localId <- UUID.nextRandom
          let kind = RepositorySync.definitionKind definition
              path = "monitors/" <> T.toLower (show kind) <> ".yaml"
          runQueryEffect tr $ RepositorySync.registerResource testPid sync.id kind localId path
          [resource] <- filter ((== localId) . (.localId)) <$> runQueryEffect tr (RepositorySync.resources testPid sync.id)
          runQueryEffect tr (RepositorySync.importDefinition resource Nothing $ Just definition) `shouldReturn` Right ()
          Just actual <- runQueryEffect tr $ RepositorySync.localDefinition resource
          let encoded = RepositorySync.encodeDefinition actual
          fmap AE.toJSON (RepositorySync.decodeDefinition path encoded) `shouldBe` Right (AE.toJSON actual)
          withResource tr.trPool \conn -> case kind of
            RepositorySync.Query -> void $ PG.execute conn "UPDATE monitors.query_monitors SET current_status = 'alerting', current_value = 17, muted_until = now() + interval '1 hour' WHERE id = ?" (PG.Only localId)
            RepositorySync.Uptime -> void $ PG.execute conn "UPDATE apis.prometheus_scrape_configs SET last_status = 'down', consecutive_failures = 7 WHERE id = ?" (PG.Only localId)
            RepositorySync.Cron -> void $ PG.execute conn "UPDATE apis.cron_monitors SET last_status = 'error', last_checkin_at = now() WHERE id = ?" (PG.Only localId)
            RepositorySync.Dashboard -> pure ()
          runtimeBefore <- runQueryEffect tr $ RepositorySync.resourceSnapshot resource
          [freshResource] <- filter ((== localId) . (.localId)) <$> runQueryEffect tr (RepositorySync.resources testPid sync.id)
          runQueryEffect tr (RepositorySync.importDefinition freshResource runtimeBefore $ Just actual) `shouldReturn` Right ()
          runtimeAfter <- runQueryEffect tr $ RepositorySync.resourceSnapshot resource
          let runtimeKeys = ["current_status", "current_value", "muted_until", "last_status", "consecutive_failures", "last_checkin_at"]
              runtime = fmap $ \case AE.Object row -> KM.filterWithKey (\key _ -> key `elem` runtimeKeys) row; _ -> KM.empty
          runtime runtimeAfter `shouldBe` runtime runtimeBefore
          when (kind == RepositorySync.Query) do
            [beforeDelete] <- filter ((== localId) . (.localId)) <$> runQueryEffect tr (RepositorySync.resources testPid sync.id)
            runQueryEffect tr (RepositorySync.importDefinition beforeDelete runtimeAfter Nothing) `shouldReturn` Right ()
            [deleted] <- filter ((== localId) . (.localId)) <$> runQueryEffect tr (RepositorySync.resources testPid sync.id)
            observed <- runQueryEffect tr $ RepositorySync.resourceSnapshot deleted
            runQueryEffect tr (RepositorySync.importDefinition deleted observed $ Just actual) `shouldReturn` Right ()
            fmap (fmap AE.toJSON) (runQueryEffect tr $ RepositorySync.localDefinition deleted) `shouldReturn` Just (AE.toJSON actual)

      it "configurationImport_doesNotOverwriteAnEditMadeAfterItsSnapshot" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "team" "checkout" "main" (GitSync.AppInstallation 42) Nothing ""
        localId <- UUID.nextRandom
        runQueryEffect tr $ RepositorySync.registerResource testPid sync.id RepositorySync.Cron localId "monitors/cron.yaml"
        [resource] <- runQueryEffect tr $ RepositorySync.resources testPid sync.id
        let original = RepositorySync.CronDefinitionConfig $ RepositorySync.CronDefinition "job" "Original" 3600 60
        runQueryEffect tr (RepositorySync.importDefinition resource Nothing $ Just original) `shouldReturn` Right ()
        [freshResource] <- runQueryEffect tr $ RepositorySync.resources testPid sync.id
        observed <- runQueryEffect tr $ RepositorySync.resourceSnapshot freshResource
        withResource tr.trPool \conn -> void $ PG.execute conn "UPDATE apis.cron_monitors SET name = 'Local edit' WHERE id = ?" (PG.Only localId)
        result <- runQueryEffect tr $ RepositorySync.importDefinition freshResource observed (Just original)
        result `shouldSatisfy` isLeft
        Just current <- runQueryEffect tr $ RepositorySync.localDefinition resource
        AE.toJSON current `shouldNotBe` AE.toJSON original
        -- Deletion uses the same compare-and-update guard.
        runQueryEffect tr (RepositorySync.importDefinition freshResource observed Nothing) `shouldReturn` result
        fmap (fmap AE.toJSON) (runQueryEffect tr $ RepositorySync.localDefinition resource) `shouldReturn` Just (AE.toJSON current)

      it "configurationFolderChange_preservesLocalResourcesAndProposesThemAtTheNewPath" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "team" "checkout" "main" (GitSync.AppInstallation 42) Nothing "old"
        localId <- UUID.nextRandom
        runQueryEffect tr $ RepositorySync.registerResource testPid sync.id RepositorySync.Cron localId "old/monitors/job.yaml"
        [resource] <- runQueryEffect tr $ RepositorySync.resources testPid sync.id
        let definition = RepositorySync.CronDefinitionConfig $ RepositorySync.CronDefinition "job" "Job" 60 60
        runQueryEffect tr (RepositorySync.importDefinition resource Nothing $ Just definition) `shouldReturn` Right ()
        void $ runQueryEffect tr $ GitSync.updateGitHubSync encKey sync.id "team" "checkout" "main" Nothing (Just "new")
        [moved] <- runQueryEffect tr $ RepositorySync.resources testPid sync.id
        moved.filePath `shouldBe` "new/monitors/job.yaml"
        moved.localBaseline `shouldBe` Nothing
        fmap (fmap AE.toJSON) (runQueryEffect tr $ RepositorySync.localDefinition moved) `shouldReturn` Just (AE.toJSON definition)

      it "configurationImport_reportsDuplicateCronSlugsWithoutAbortingOtherResources" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" "checkout" "main" (GitSync.AppInstallation 42) Nothing ""
        for_ (["first", "second", "third"] :: [Text]) \name -> do
          localId <- UUID.nextRandom
          runQueryEffect tr $ RepositorySync.registerResource testPid sync.id RepositorySync.Cron localId ("monitors/" <> name <> ".yaml")
          [resource] <- filter ((== localId) . (.localId)) <$> runQueryEffect tr (RepositorySync.resources testPid sync.id)
          result <- runQueryEffect tr $ RepositorySync.importDefinition resource Nothing (Just $ RepositorySync.CronDefinitionConfig $ RepositorySync.CronDefinition (if name == "second" then "first" else name) name 60 60)
          if name == "second" then result `shouldSatisfy` isLeft else result `shouldBe` Right ()

      it "serviceMappings_keepMultipleRepositoriesForTheSameServiceAndPath" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "team" (Just 42) Nothing
        for_ ["checkout", "shared"] \name -> runQueryEffect tr $ CodeContext.insertCodeMapping testPid account.id (Git.RepoRef "team" name "main") (Just "checkout") "" ""
        mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        sort (map (.repo) mappings) `shouldBe` ["checkout", "shared"]

      it "sourceContext_disableRetainsServiceLinksAndDoesNotDisableReviews" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "team" (Just 42) Nothing
        runQueryEffect tr $ CodeContext.insertCodeMapping testPid account.id (Git.RepoRef "team" "checkout" "main") (Just "checkout") "" ""
        [repository] <- runQueryEffect tr $ GitSync.getRepositories testPid
        _ <- testServant tr $ PageCodeContext.repositorySourceEnabledPostH testPid repository.id (PageCodeContext.SourceContextForm False)
        runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= \mappings -> map (.service) mappings `shouldBe` [Just "checkout"]
        result <- runQueryEffect tr $ W.runHTTPWreq $ withUnavailableRepositoryAPI $ CodeContext.fetchSnippet tr.trATCtx.codeBlobCache tr.trATCtx.config testPid (Just "checkout") Nothing "main.py" 1
        result `shouldBe` Left (CodeContext.NoMapping "main.py")
        runQueryEffect tr $ ImpactReviews.receiveEvent (ImpactReviews.PullRequestEvent "team" "checkout" 42 1 (T.replicate 40 "a") frozenTime True)
        runQueryEffect tr (ImpactReviews.latestRuns testPid) >>= \runs -> length runs `shouldBe` 1

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

      it "serviceRepositories_showOnlyApplicableOriginsAndExplainMissingLinksWithoutOfferingViewerEdits" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
            service = "team/api & jobs"
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "acme" (Just 42) Nothing
        Just enterprise <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub (Just "https://git.example.com") "acme" (Just 43) Nothing
        forM_ ([(account.id, "checkout-service", Just service, "/srv/"), (account.id, "shared-source", Nothing, "/shared/"), (account.id, "payments", Just "billing", "/payments/"), (enterprise.id, "checkout-service", Just "billing", "/enterprise/")] :: [(GitSync.GitHubCredentialId, Text, Maybe Text, Text)]) \(cid, repo, svc, prefix) ->
          runQueryEffect tr $ CodeContext.insertCodeMapping testPid cid (Git.RepoRef "acme" repo "main") svc prefix ""
        void $ runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "acme" "dashboards" "main" (GitSync.AppInstallation 42) Nothing ""
        repositories <- runQueryEffect tr $ GitSync.getRepositories testPid
        (_, scoped) <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.serviceRepositoriesGetH testPid service
        let html = render scoped
        forM_ repositories \repository ->
          T.isInfixOf ("/repositories/" <> repository.id.toText <> "\"") html `shouldBe` (isNothing repository.apiBase && repository.repo `elem` ["checkout-service", "shared-source"])
        html `shouldSatisfy` T.isInfixOf "All services"
        html `shouldSatisfy` T.isInfixOf "Repositories for team/api &amp; jobs"
        html `shouldSatisfy` T.isInfixOf "All repositories"
        html `shouldNotSatisfy` T.isInfixOf "id=\"repository-nav\""
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql| DELETE FROM projects.code_mappings WHERE project_id = #{testPid} |]
        (_, unlinkedPage) <- testServant tr $ PageCodeContext.serviceRepositoriesGetH testPid service
        render unlinkedPage `shouldSatisfy` T.isInfixOf "No repositories linked to this service"
        render unlinkedPage `shouldSatisfy` T.isInfixOf "Choose a repository"
        let uid = (getResponse tr.trSessAndHeader).user.id
        runQueryEffect tr do
          Hasql.interpExecute_ [HI.sql| UPDATE projects.project_members SET permission = 'view' WHERE project_id = #{testPid} AND user_id = #{uid} |]
          Hasql.interpExecute_ [HI.sql| DELETE FROM projects.repositories WHERE project_id = #{testPid} |]
        forM_ [PageCodeContext.serviceRepositoriesGetH testPid service, PageCodeContext.repositoriesGetH testPid Nothing Nothing] \handler -> do
          (_, readonly) <- testServant tr handler
          render readonly `shouldNotSatisfy` T.isInfixOf "Add repositories"
          render readonly `shouldNotSatisfy` T.isInfixOf "Choose a repository"
          render readonly `shouldNotSatisfy` T.isInfixOf "Connect repositories"
          render readonly `shouldSatisfy` T.isInfixOf "Ask a project editor"

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
        (_, html) <- testServant tr $ PageCodeContext.repositoryConnectGetH testPid Nothing
        render (Lucid.toHtml html.content) `shouldSatisfy` T.isInfixOf "Choose an account"
        _ <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just "checkout") (Just "main") Nothing Nothing Nothing Nothing Nothing)
        runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= (`shouldSatisfy` null)
        for_ accounts \account -> do
          (_, editor) <- testServant tr $ PageCodeContext.codeMappingsEditorGetH testPid (Just account.id) (Just "/srv/app/main.py")
          T.isInfixOf ("name=\"credentialId\" value=\"" <> account.id.toText <> "\"") (render editor) `shouldBe` True
          T.isInfixOf "value=\"/srv/app/main.py\"" (render editor) `shouldBe` True
          _ <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just "checkout") (Just "main") (Just account.account) Nothing Nothing Nothing (Just account.id))
          pass
        mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        sort [(m.owner, m.credentialId) | m <- mappings] `shouldBe` sort [(a.account, a.id) | a <- accounts]
        otherPid <- createTestProject tr "Other account project"
        Just foreignAccount <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey otherPid Git.GitHub Nothing "foreign" (Just 42) Nothing
        _ <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just "checkout") (Just "main") (Just "foreign") Nothing Nothing Nothing (Just foreignAccount.id))
        runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= \rows -> length rows `shouldBe` 2

      it "repositoryConnection_missingReviewRunnerIsNotReadyAndRetainsDetails" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
            configured = tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppWebhookSecret = "fixture", githubAppId = "fixture", githubAppPrivateKey = "fixture", impactReviewAgent = Just "test/fixtures/impact-review-agent.mjs"}}}
        Just credential <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub Nothing "team" (Just 42) Nothing
        runQueryEffect tr $ CodeContext.insertCodeMapping testPid credential.id (Git.RepoRef "team" "checkout" "main") (Just "checkout") "/app/" "src"
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "team" "checkout" "main" (GitSync.AppInstallation 42) Nothing "ops"
        [repository] <- runQueryEffect tr $ GitSync.getRepositories testPid
        (_, details) <- testServant configured $ PageCodeContext.repositoryGetH testPid repository.id
        render (Lucid.toHtml details.content) `shouldSatisfy` T.isInfixOf ("href=\"/p/" <> testPid.toText <> "/repositories/" <> repository.id.toText <> "/source\"")
        map (.service) details.content.mappings `shouldBe` [Just "checkout"]
        fmap (.id) details.content.dashboardSync `shouldBe` Just sync.id
        details.content.reviewReadiness `shouldSatisfy` \case PageCodeContext.ReviewReady _ -> True; _ -> False
        let withoutAgent = configured{trATCtx = configured.trATCtx{config = configured.trATCtx.config{impactReviewAgent = Nothing}}}
        (_, noRunner) <- testServant withoutAgent $ PageCodeContext.repositoryGetH testPid repository.id
        render (Lucid.toHtml noRunner.content) `shouldNotSatisfy` T.isInfixOf "Ready for pull requests"
        render (Lucid.toHtml noRunner.content) `shouldSatisfy` T.isInfixOf "Automatic reviews are unavailable. Ask your Monoscope administrator to finish review setup."
        let incomplete = configured{trATCtx = configured.trATCtx{config = configured.trATCtx.config{githubAppPrivateKey = ""}}}
        (_, unavailable) <- testServant incomplete $ PageCodeContext.repositoryGetH testPid repository.id
        unavailable.content.reviewReadiness `shouldSatisfy` \case PageCodeContext.ReviewNeedsServer -> True; _ -> False
        runQueryEffect tr $ ImpactReviews.updateSettings testPid (ImpactReviews.ReviewSettings "team" "checkout" False False)
        (_, disabled) <- testServant withoutAgent $ PageCodeContext.repositoryGetH testPid repository.id
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

      it "repositoryRemoval_keepsLocalDashboardsAndHistoryWithoutRemovingOtherOriginsOrSharedAccounts" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        for_ ([Nothing, Just "https://git.example.com"] :: [Maybe Text]) \origin -> do
          Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential encKey testPid Git.GitHub origin "team" (Just 42) Nothing
          runQueryEffect tr $ CodeContext.insertCodeMapping testPid account.id (Git.RepoRef "team" "checkout" "main") (Just $ fromMaybe "public" origin) "/app/" "src"
          Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub origin "team" "checkout" "main" (GitSync.PersonalToken "fixture") Nothing "ops"
          did <- UUIDId <$> UUID.nextRandom
          void $ runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Local overview", Dashboards.schema = Just def, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "overview.yaml", Dashboards.fileSha = Just "provider-version"}
        repositories <- runQueryEffect tr $ GitSync.getRepositories testPid
        repository <- maybe (fail "public repository missing") pure $ find (isNothing . (.apiBase)) repositories
        runQueryEffect tr $ ImpactReviews.receiveEvent (ImpactReviews.PullRequestEvent "team" "checkout" 42 1 (T.replicate 40 "a") frozenTime True)
        history <- runQueryEffect tr $ ImpactReviews.latestRuns testPid
        length history `shouldBe` 1
        otherPid <- createTestProject tr "Other repository removal"
        runQueryEffect tr (GitSync.removeRepository otherPid repository.id) `shouldReturn` Left GitSync.ConnectionMissing
        (removed, _) <- testServant tr $ PageCodeContext.repositoryDeleteH testPid repository.id
        getHeaders removed `shouldContain` [("HX-Redirect", encodeUtf8 $ "/p/" <> testPid.toText <> "/repositories")]
        remaining <- runQueryEffect tr $ GitSync.getRepositories testPid
        map (.apiBase) remaining `shouldBe` [Just "https://git.example.com"]
        syncs <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        map (.apiBase) syncs `shouldBe` [Just "https://git.example.com"]
        mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        map (.service) mappings `shouldBe` [Just "https://git.example.com"]
        accounts <- runQueryEffect tr $ GitSync.getGitHubCredentials testPid
        length accounts `shouldBe` 2
        dashboards <- runQueryEffect tr $ Dashboards.selectDashboardsSortedBy testPid Dashboards.Desc Dashboards.DSUpdatedAt
        length dashboards `shouldBe` 2
        [(d.title, d.filePath, d.fileSha) | d <- dashboards, isNothing d.gitSyncId] `shouldBe` [("Local overview", Nothing, Nothing)]
        length (filter (isJust . (.gitSyncId)) dashboards) `shouldBe` 1
        runQueryEffect tr (ImpactReviews.latestRuns testPid) >>= \runs -> map (.id) runs `shouldBe` map (.id) history
        runQueryEffect tr (Hasql.interpOne @(Bool, Bool) [HI.sql| SELECT enabled, include_evidence FROM projects.pr_review_settings WHERE project_id = #{testPid} AND owner = 'team' AND repo = 'checkout' |]) `shouldReturn` Just (False, False)
        runQueryEffect tr (GitSync.removeRepository testPid repository.id) `shouldReturn` Left GitSync.ConnectionMissing
        runQueryEffect tr $ ImpactReviews.receiveEvent (ImpactReviews.PullRequestEvent "team" "checkout" 42 2 (T.replicate 40 "b") frozenTime True)
        runQueryEffect tr (ImpactReviews.latestRuns testPid) >>= \runs -> map (.id) runs `shouldBe` map (.id) history
        localDashboard <- maybe (fail "local dashboard missing") pure $ find (isNothing . (.gitSyncId)) dashboards
        jobs <- getPendingBackgroundJobs tr.trATCtx
        runAuthHandler tr $ GitSyncPage.queueGitSyncPush testPid localDashboard.id
        length <$> getPendingBackgroundJobs tr.trATCtx `shouldReturn` length jobs
        (requests, _) <- runTestBgRecordingHTTP frozenTime tr $ withHTTPResponses (\_ _ -> pure $ Just "{\"content\":{\"sha\":\"written-blob\"},\"commit\":{\"sha\":\"written-head\"}}") do
          BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncPushDashboard testPid localDashboard.id.unUUIDId)
          BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncPushAllDashboards testPid)
        map fst requests `shouldSatisfy` (not . any (T.isInfixOf "local-overview.yaml"))
        Just stillLocal <- runQueryEffect tr $ Dashboards.getDashboardByProjectId testPid localDashboard.id
        stillLocal.gitSyncId `shouldBe` Nothing

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

      it "repositoryConnection_reportsUnreadableTokensWithoutChangingTheFailedOperation" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        _ <- runQueryEffect tr $ Hasql.interpExecute [HI.sql| UPDATE projects.git_sync SET access_token = 'invalid-base64' WHERE id = #{sync.id} |]
        decrypted <- runQueryEffect tr $ GitSync.getGitSyncsDecrypted (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid
        length decrypted `shouldBe` 0
        Just failed <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        failed.lastError `shouldSatisfy` isJust
        did <- UUIDId <$> UUID.nextRandom
        void $ runQueryEffect tr $ GitSync.recordSyncError sync.id (GitSync.ExportDashboard did) "Export failed"
        _ <- testServant tr $ PageCodeContext.repositoryConnectGetH testPid Nothing
        Just stillFailed <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        (stillFailed.lastError <&> (.operation)) `shouldBe` Just (GitSync.ExportDashboard did)

      it "repositoryPicker_doesNotTreatAnEnterpriseRepositoryAsItsPublicGitHubCounterpart" \tr -> do
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" (Just 42) Nothing
        Cache.insert tr.trATCtx.repoListCache account.id [Git.GitRepo "team/dashboards" "dashboards" True "main"]
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
        (selection, _) <- testServant tr $ GitSyncPage.gitSyncSettingsDeleteH testPid
        getHeaders selection `shouldSatisfy` elem ("HX-Redirect", encodeUtf8 ("/p/" <> testPid.toText <> "/repositories"))
        length <$> runQueryEffect tr (GitSync.getGitSyncs testPid) `shouldReturn` 2
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
        M.toList stateA `shouldBe` [("overview.yaml", (dashA.id, Just "sha-a"))]
        M.toList stateB `shouldBe` [("overview.yaml", (dashB.id, Just "sha-b"))]
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

      it "repositoryEdits_updateOneConfigurationPRWithoutWritingToTheTargetBranch" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        did <- UUIDId <$> UUID.nextRandom
        void $ runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Overview", Dashboards.schema = Just def, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "overview.yaml"}
        let push = BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncPushDashboard testPid did.unwrap)
        (firstRequests, ()) <- runTestBgRecordingHTTP frozenTime tr $ configurationProvider tr "open" push
        Just firstSync <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        firstSync.pullRequestNumber `shouldBe` Just 17
        void $ testServant tr $ DashboardPage.dashboardRenamePatchH testPid did (DashboardPage.DashboardRenameForm "Updated overview" Nothing)
        (secondRequests, ()) <- runTestBgRecordingHTTP frozenTime tr $ configurationProvider tr "open" push
        Just updatedSync <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        updatedSync.pullRequestNumber `shouldBe` Just 17
        length [() | (url, _) <- firstRequests <> secondRequests, "/pulls" `T.isSuffixOf` url] `shouldBe` 1
        let writes = [(url, body) | (url, body) <- firstRequests <> secondRequests, not (LBS.null body)]
        writes `shouldSatisfy` any (T.isInfixOf "/git/trees" . fst)
        writes `shouldSatisfy` any (T.isInfixOf "/git/refs/heads/monoscope-dashboards" . fst)
        writes `shouldNotSatisfy` any (T.isInfixOf "/git/refs/heads/main" . fst)
        writes `shouldNotSatisfy` any (T.isInfixOf "/contents/" . fst)
        Just localDashboard <- runQueryEffect tr $ Dashboards.getDashboardByProjectId testPid did
        localDashboard.title `shouldBe` "Updated overview"
        void $ runTestBgRecordingHTTP frozenTime tr $ configurationProvider tr "closed" push
        Just paused <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        paused.exportPaused `shouldBe` True
        (fmap (.title) <$> runQueryEffect tr (Dashboards.getDashboardByProjectId testPid did)) `shouldReturn` Just "Updated overview"

      it "repositoryExport_doesNotProposeEditsAgainstAnUnreconciledTargetRevision" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        did <- UUIDId <$> UUID.nextRandom
        void $ runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Local", Dashboards.schema = Just def, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "local.yaml"}
        calls <- newIORef (0 :: Int)
        let advancing = withHTTPResponses \_ endpoint ->
              if "/commits/main" `T.isInfixOf` toText endpoint
                then do
                  number <- atomicModifyIORef' calls (\n -> (n + 1, n))
                  pure $ Just $ if number == 0 then "{\"sha\":\"head\",\"commit\":{\"tree\":{\"sha\":\"tree\"}}}" else "{\"sha\":\"new-head\",\"commit\":{\"tree\":{\"sha\":\"new-tree\"}}}"
                else pure Nothing
        (requests, ()) <- runTestBgRecordingHTTP frozenTime tr $ configurationProvider tr "open" $ advancing $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncPushDashboard testPid did.unwrap)
        requests `shouldNotSatisfy` any (\(url, body) -> not (LBS.null body) && "/git/trees" `T.isSuffixOf` url)
        Just failed <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        failed.lastError `shouldSatisfy` isJust
        (fmap (.title) <$> runQueryEffect tr (Dashboards.getDashboardByProjectId testPid did)) `shouldReturn` Just "Local"

      it "repositoryRevertedAddition_removesTheStaleFileFromItsOpenPR" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        did <- UUIDId <$> UUID.nextRandom
        void $ runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Temporary", Dashboards.schema = Just def, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "temporary.yaml"}
        let push = BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncPushDashboard testPid did.unwrap)
            reconcile = BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncRepository testPid sync.id)
        void $ runTestBgRecordingHTTP frozenTime tr $ configurationProvider tr "open" push
        void $ runQueryEffect tr $ Dashboards.deleteDashboardsByIds testPid (fromList [did])
        (requests, ()) <- runTestBgRecordingHTTP frozenTime tr $ configurationProvider tr "open" reconcile
        let trees = [body | (url, body) <- requests, "/git/trees" `T.isSuffixOf` url]
        length trees `shouldBe` 1
        trees `shouldSatisfy` all (T.isInfixOf "\"tree\":[]" . decodeUtf8 . LBS.toStrict)

      it "repositoryPush_doesNotAdvanceThePullCursorPastOtherRemoteDashboards" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub (Just "https://127.0.0.1:1") "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        did <- UUIDId <$> UUID.nextRandom
        _ <- runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Local", Dashboards.schema = Just def, Dashboards.filePath = Just "local.yaml"}
        runQueryEffect tr (GitSync.assignDashboardRepository testPid did sync.id) `shouldReturn` Right ()
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
        Just account <- runQueryEffect tr $ GitSync.upsertGitHubCredential (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team-a" (Just 42) Nothing
        Cache.insert tr.trATCtx.repoListCache account.id [Git.GitRepo "team-a/dashboards" "dashboards" True "main", Git.GitRepo "team-b/dashboards" "dashboards" True "main"]
        let selection = GitSyncPage.RepoSelectForm ["team-a/dashboards", "team-b/dashboards", "team-a/dashboards"] "trunk" (Just "observability") 42
        _ <- testServant tr $ GitSyncPage.githubAppSelectRepoH testPid selection
        syncs <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        sort [(s.owner, s.repo, s.branch, s.pathPrefix) | s <- syncs] `shouldBe` [("team-a", "dashboards", "trunk", "observability"), ("team-b", "dashboards", "trunk", "observability")]
        _ <- testServant tr $ GitSyncPage.githubAppSelectRepoH testPid selection{GitSyncPage.branch = "different", GitSyncPage.pathPrefix = Just "other"}
        unchanged <- runQueryEffect tr $ GitSync.getGitSyncs testPid
        sort [(s.id, s.branch, s.pathPrefix) | s <- unchanged] `shouldBe` sort [(s.id, s.branch, s.pathPrefix) | s <- syncs]

      it "repositoryPull_preservesAnAssignedDashboardBeforeFirstPushAndReportsRemotePathConflicts" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub (Just "https://127.0.0.1:1") "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        did <- UUIDId <$> UUID.nextRandom
        _ <- runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Local overview", Dashboards.schema = Just def, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "overview.yaml"}
        let pull :: Text -> LBS.ByteString -> IO ([(Text, LBS.ByteString)], ())
            pull revision tree = runTestBgRecordingHTTP frozenTime tr $ withHTTPResponses (\_ endpoint -> pure $ Just $ if "/git/trees/" `T.isInfixOf` toText endpoint then tree else if "/contents/" `T.isInfixOf` toText endpoint then AE.encode $ AE.object ["content" AE..= extractBase64 (B64.encodeBase64 "title: Remote overview\nwidgets: []\n")] else AE.encode $ AE.object ["sha" AE..= revision]) $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncRepository testPid sync.id)
        _ <- pull "before-first-push" "{\"tree\":[]}"
        Just awaiting <- runQueryEffect tr $ Dashboards.getDashboardByProjectId testPid did
        (awaiting.title, awaiting.fileSha) `shouldBe` ("Local overview", Nothing)
        outcome <- try @_ @SomeException $ pull "remote-conflict" "{\"tree\":[{\"path\":\"dashboards/overview.yaml\",\"sha\":\"remote-blob\",\"type\":\"blob\"}]}"
        whenLeft_ outcome $ expectationFailure . ("Pull must report a first-push conflict rather than attempt a duplicate insert: " <>) . show
        Just conflicted <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        conflicted.lastRevision `shouldBe` Just "remote-conflict"
        conflicts <- runQueryEffect tr $ RepositorySync.resources testPid sync.id
        conflicts `shouldSatisfy` any (.conflicted)
        [repository] <- runQueryEffect tr $ GitSync.getRepositories testPid
        (_, status) <- testServant tr $ PageCodeContext.repositoryGetH testPid repository.id
        let html = render $ Lucid.toHtml status.content
        html `shouldSatisfy` T.isInfixOf "Both versions changed"
        html `shouldSatisfy` T.isInfixOf "Keep Monoscope"
        html `shouldSatisfy` T.isInfixOf "Compare versions"
        html `shouldSatisfy` T.isInfixOf "Active in Monoscope"
        conflict <- maybe (fail "Expected the dashboard conflict") pure $ find (.conflicted) conflicts
        void $ testServant tr $ PageCodeContext.repositoryConflictPostH testPid repository.id conflict.id (PageCodeContext.ConflictForm False)
        void $ pull "remote-conflict" "{\"tree\":[{\"path\":\"dashboards/overview.yaml\",\"sha\":\"remote-blob\",\"type\":\"blob\"}]}"
        Just retained <- runQueryEffect tr $ Dashboards.getDashboardByProjectId testPid did
        retained.title `shouldBe` "Local overview"
        resolved <- runQueryEffect tr $ RepositorySync.resources testPid sync.id
        resolved `shouldSatisfy` all (not . (.conflicted))

      it "repositorySyncFailureMigration_preservesLegacyErrorsAsImportFailures" \tr -> do
        let connect owner = runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing owner "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        Just failed <- connect "failed"
        Just healthy <- connect "healthy"
        withResource tr.trPool \conn -> do
          void $ PG.execute_ conn "ALTER TABLE projects.git_sync ALTER COLUMN last_error TYPE TEXT USING last_error->>'message'"
          void $ PG.execute conn "UPDATE projects.git_sync SET last_error = 'Legacy provider failure' WHERE id = ?" (PG.Only failed.id)
        migration <- Query <$> readFileBS "static/migrations/0222_repository_sync_failures.sql"
        withResource tr.trPool \conn -> void $ PG.execute_ conn migration
        Just migrated <- runQueryEffect tr $ GitSync.getGitSyncById testPid failed.id
        migrated.lastError `shouldBe` Just (GitSync.SyncFailure GitSync.ImportDashboards "Legacy provider failure")
        Just unchanged <- runQueryEffect tr $ GitSync.getGitSyncById testPid healthy.id
        unchanged.lastError `shouldBe` Nothing

      it "repositoryExportFailure_retriesTheSharedSnapshotAndRetainsActiveLocalChanges" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        did <- UUIDId <$> UUID.nextRandom
        void $ runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Local change", Dashboards.schema = Just def, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "local.yaml"}
        let job = BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncPushDashboard testPid did.unwrap)
            failedExport = withHTTPResponses \_ endpoint -> pure $ Just $ if "/git/trees/" `T.isInfixOf` toText endpoint then "{\"tree\":[]}" else "{\"sha\":\"head\"}"
        void $ runTestBgRecordingHTTP frozenTime tr $ failedExport job
        Just failed <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        failed.lastError `shouldBe` Just (GitSync.SyncFailure GitSync.ExportDashboards "GitHub did not return the target tree.")
        (_, retried) <- testServant tr $ GitSyncPage.gitSyncRepositoryRetryH testPid sync.id
        render retried `shouldSatisfy` T.isInfixOf "Retry export"
        queued <- getPendingBackgroundJobs tr.trATCtx
        [sid | (_, BackgroundJobs.GitSyncPushRepository pid sid) <- toList queued, pid == testPid] `shouldBe` [sync.id]
        void $ runTestBgRecordingHTTP frozenTime tr $ configurationProvider tr "open" job
        Just recovered <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        (recovered.pullRequestNumber, recovered.lastError) `shouldBe` (Just 17, Nothing)
        fmap (.title) <$> runQueryEffect tr (Dashboards.getDashboardByProjectId testPid did) `shouldReturn` Just "Local change"

      it "repositoryPull_keepsTheTreeContentAndCursorAtOneCommitWhenTheBranchMoves" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        let transport = withHTTPResponses \_ endpoint -> do
              let path = toText endpoint
                  pinned = "first-commit" `T.isInfixOf` path
              pure
                $ Just
                $ if "/commits/" `T.isInfixOf` path
                  then "{\"sha\":\"first-commit\"}"
                  else
                    if "/git/trees/" `T.isInfixOf` path
                      then "{\"tree\":[{\"path\":\"dashboards/overview.yaml\",\"sha\":\"first-blob\",\"type\":\"blob\"}]}"
                      else AE.encode $ AE.object ["content" AE..= extractBase64 (B64.encodeBase64 (if pinned then "title: Original overview\nwidgets: []\n" else "title: Later overview\nwidgets: []\n"))]
        (requests, ()) <- runTestBgRecordingHTTP frozenTime tr $ transport $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncRepository testPid sync.id)
        dashboards <- runQueryEffect tr $ Dashboards.selectDashboardsSortedBy testPid Dashboards.Asc Dashboards.DSTitle
        map (\d -> (d.title, d.fileSha)) dashboards `shouldBe` [("Original overview", Just "first-blob")]
        map fst requests `shouldBe` map ("https://api.github.com/repos/team/dashboards" <>) ["/commits/main", "/git/trees/first-commit?recursive=1", "/contents/dashboards/overview.yaml?ref=first-commit"]
        Just completed <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        completed.lastRevision `shouldBe` Just "first-commit"

      it "repositoryWorkers_deferOverlappingImportsWithoutDuplicatingDashboards" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        Just other <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "another-team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        did <- UUIDId <$> UUID.nextRandom
        void $ runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Local overview", Dashboards.schema = Just def, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "local.yaml"}
        fetching <- newEmptyMVar
        release <- newEmptyMVar
        let job = BackgroundJobs.GitSyncRepository testPid sync.id
            provider blocked = withHTTPResponses \_ endpoint -> do
              let path = toText endpoint
              if "/contents/" `T.isInfixOf` path
                then do
                  when blocked $ putMVar fetching () >> takeMVar release
                  pure $ Just $ AE.encode $ AE.object ["content" AE..= extractBase64 (B64.encodeBase64 "title: Imported overview\nwidgets: []\n")]
                else pure $ Just $ if "/git/trees/" `T.isInfixOf` path then "{\"tree\":[{\"path\":\"dashboards/overview.yaml\",\"sha\":\"blob\",\"type\":\"blob\"}]}" else "{\"sha\":\"head\"}"
        withAsync (runTestBgRecordingHTTP frozenTime tr $ provider True $ BackgroundJobs.processBackgroundJob tr.trATCtx job) \worker -> do
          timeout 5000000 (takeMVar fetching) `shouldReturn` Just ()
          for_ [job, BackgroundJobs.GitSyncPushDashboard testPid did.unUUIDId, BackgroundJobs.GitSyncPushAllDashboards testPid] \overlap -> do
            outcome <- try @_ @SomeException $ runTestBgRecordingHTTP frozenTime tr $ provider False $ BackgroundJobs.processBackgroundJob tr.trATCtx overlap
            outcome `shouldSatisfy` isLeft
          jobs <- getPendingBackgroundJobs tr.trATCtx
          jobs `shouldSatisfy` null
          (independent, ()) <- runTestBgRecordingHTTP frozenTime tr $ provider False $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncRepository testPid other.id)
          length independent `shouldBe` 3
          putMVar release ()
          void $ wait worker
        (_, ()) <- runTestBgRecordingHTTP frozenTime tr $ provider False $ BackgroundJobs.processBackgroundJob tr.trATCtx job
        dashboards <- runQueryEffect tr $ Dashboards.selectDashboardsSortedBy testPid Dashboards.Asc Dashboards.DSTitle
        map (\d -> (d.title, d.gitSyncId)) dashboards `shouldMatchList` [("Imported overview", Just sync.id), ("Imported overview", Just other.id), ("Local overview", Just sync.id)]
        Just completed <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        completed.lastRevision `shouldBe` Just "head"
        completed.lastError `shouldBe` Just (GitSync.SyncFailure GitSync.ExportDashboards "GitHub did not return the target tree.")

      it "repositoryRemoval_waitsForActiveImportsAndExportsWithoutDetachingTheirDashboards" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        [repository] <- runQueryEffect tr $ GitSync.getRepositories testPid
        did <- UUIDId <$> UUID.nextRandom
        void $ runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Local overview", Dashboards.schema = Just def, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "local.yaml", Dashboards.fileSha = Just "blob-local"}
        for_ [BackgroundJobs.GitSyncRepository testPid sync.id, BackgroundJobs.GitSyncPushDashboard testPid did.unUUIDId] \job -> do
          fetching <- newEmptyMVar
          release <- newEmptyMVar
          blockedOnce <- newIORef False
          let provider = withHTTPResponses \_ endpoint -> do
                let path = toText endpoint
                if "/contents/" `T.isInfixOf` path
                  then do
                    shouldBlock <- atomicModifyIORef' blockedOnce (\blocked -> (True, not blocked))
                    when shouldBlock $ putMVar fetching () >> takeMVar release
                    pure $ Just $ if "/local.yaml" `T.isInfixOf` path then "{\"content\":{\"sha\":\"written-blob\"}}" else AE.encode $ AE.object ["content" AE..= extractBase64 (B64.encodeBase64 "title: Imported overview\nwidgets: []\n")]
                  else pure $ Just $ if "/git/trees/" `T.isInfixOf` path then "{\"tree\":[{\"path\":\"dashboards/overview.yaml\",\"sha\":\"blob\",\"type\":\"blob\"},{\"path\":\"dashboards/local.yaml\",\"sha\":\"blob-local\",\"type\":\"blob\"}]}" else "{\"sha\":\"head\"}"
          withAsync (runTestBgRecordingHTTP frozenTime tr $ provider $ BackgroundJobs.processBackgroundJob tr.trATCtx job) \worker -> do
            timeout 5000000 (takeMVar fetching) `shouldReturn` Just ()
            (_, refused) <- testServant tr $ PageCodeContext.repositoryDeleteH testPid repository.id
            render refused `shouldSatisfy` T.isInfixOf "A sync is running"
            runQueryEffect tr (GitSync.getRepository testPid repository.id) >>= (`shouldSatisfy` isJust)
            (_, disconnected) <- testServant tr $ GitSyncPage.gitSyncRepositoryDeleteH testPid sync.id
            render disconnected `shouldSatisfy` T.isInfixOf "A dashboard sync is running"
            Just retained <- runQueryEffect tr $ Dashboards.getDashboardByProjectId testPid did
            retained.gitSyncId `shouldBe` Just sync.id
            putMVar release ()
            void $ wait worker
        bracket
          (runTestBg (addUTCTime (-181) frozenTime) tr (GitSync.claimSyncLease sync.id) >>= maybe (fail "Expected a free repository lease") pure)
          (\lease -> runQueryEffect tr $ GitSync.releaseSyncLease lease)
          \_ -> do
            advanceTestTime tr 181
            runQueryEffect tr (GitSync.removeRepository testPid repository.id) `shouldReturn` Right ()
        dashboards <- runQueryEffect tr $ Dashboards.selectDashboardsSortedBy testPid Dashboards.Asc Dashboards.DSTitle
        map (\d -> (d.title, d.gitSyncId, d.filePath, d.fileSha)) dashboards `shouldMatchList` [("Imported overview", Nothing, Nothing, Nothing), ("Local overview", Nothing, Nothing, Nothing)]
        (requests, ()) <- runTestBgRecordingHTTP frozenTime tr $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncRepository testPid sync.id)
        requests `shouldBe` []

      it "repositoryWorkers_releaseCancelledClaimsAndCannotReleaseANewerLease" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        fetching <- newEmptyMVar
        release <- newEmptyMVar
        let blocked = withHTTPResponses \_ _ -> putMVar fetching () >> takeMVar release $> Nothing
            acquire time = runTestBg time tr (GitSync.claimSyncLease sync.id) >>= maybe (fail "Expected a free repository lease") pure
            relinquish lease = runQueryEffect tr $ GitSync.releaseSyncLease lease
        withAsync (runTestBgRecordingHTTP frozenTime tr $ blocked $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncRepository testPid sync.id)) \worker -> do
          timeout 5000000 (takeMVar fetching) `shouldReturn` Just ()
          cancel worker
          waitCatch worker >>= (`shouldSatisfy` isLeft)
        runQueryEffect tr (Hasql.interpOne @(HI.OneColumn Int) [HI.sql| SELECT count(*) FROM projects.repository_sync_leases WHERE sync_id = #{sync.id} |]) `shouldReturn` Just (HI.OneColumn 0)
        bracket (acquire frozenTime) relinquish \old ->
          bracket (acquire $ addUTCTime 181 frozenTime) relinquish \_ -> do
            relinquish old
            runTestBg (addUTCTime 181 frozenTime) tr (GitSync.claimSyncLease sync.id) `shouldReturn` Nothing
        bracket (acquire $ addUTCTime 181 frozenTime) relinquish (const pass)

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
        length requests `shouldBe` 3
        Just renamed <- runQueryEffect tr $ Dashboards.getDashboardByProjectId testPid did
        (renamed.filePath, renamed.fileSha) `shouldBe` (Just "new.yaml", Just "same-content")

      it "repositoryWorkers_leaveContentionToTheQueuesBoundedRetryPolicy" \tr -> do
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        Just lease <- runTestBg frozenTime tr $ GitSync.claimSyncLease sync.id
        bracket (pure lease) (\claimed -> runQueryEffect tr $ GitSync.releaseSyncLease claimed) \_ -> do
          job <- withResource tr.trATCtx.jobsPool \conn -> Jobs.createJob conn "background_jobs" (BackgroundJobs.GitSyncRepository testPid sync.id)
          threads <- newIORef M.empty
          let runner queued = do
                payload <- Jobs.throwParsePayload queued
                void $ runTestBgRecordingHTTP frozenTime tr $ BackgroundJobs.processBackgroundJob tr.trATCtx payload
              config = mkConfig (\_ _ -> pass) "background_jobs" tr.trATCtx.jobsPool (Jobs.MaxConcurrentJobs 1) runner id
              env = Jobs.RunnerEnv config threads
          for_ [1 .. 10] \attempt -> do
            withResource tr.trATCtx.jobsPool \conn -> void $ PG.execute conn "UPDATE background_jobs SET run_at = ? WHERE id = ?" (frozenTime, job.jobId)
            Just worker <- runReaderT (Jobs.pollRunJob "repository-retry-test" Nothing) env
            void $ waitCatch worker
            stored <- withResource tr.trATCtx.jobsPool \conn -> Jobs.findJobByIdIO conn "background_jobs" job.jobId
            stored `shouldSatisfy` isJust
            retried <- maybe (fail "Retry replaced the original job") pure stored
            (retried.jobAttempts, retried.jobStatus) `shouldBe` (attempt, if attempt == 10 then Jobs.Failed else Jobs.Retry)
            retried.jobLastError `shouldSatisfy` maybe False (T.isInfixOf "SyncLeaseUnavailable" . decodeUtf8 . LBS.toStrict . AE.encode)
            withResource tr.trATCtx.jobsPool (\conn -> PG.query_ conn "SELECT count(*) FROM background_jobs" :: IO [PG.Only Int]) `shouldReturn` [PG.Only 1]

      it "repositoryBulkExport_claimsOneLeasePerRepository" \tr -> do
        leases <- newIORef ([] :: [(Text, UUID.UUID)])
        for_ ["one", "two"] \repo -> do
          Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" repo "main" (GitSync.PersonalToken "fixture") Nothing ""
          for_ ["a.yaml", "b.yaml"] \path -> do
            did <- UUIDId <$> UUID.nextRandom
            void $ runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM did testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = path, Dashboards.schema = Just def, Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just path}
        let provider = withHTTPResponses \_ endpoint -> do
              owners <- runQueryEffect tr $ Hasql.interp @[(Text, UUID.UUID)] [HI.sql| SELECT s.repo, l.owner FROM projects.repository_sync_leases l JOIN projects.git_sync s ON s.id = l.sync_id |]
              modifyIORef' leases (<> owners)
              pure $ Just $ if "/contents/" `T.isInfixOf` toText endpoint then "{\"content\":{\"sha\":\"written\"}}" else "{\"sha\":\"head\"}"
        (requests, ()) <- runTestBgRecordingHTTP frozenTime tr $ provider $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncPushAllDashboards testPid)
        length requests `shouldBe` 6
        owners <- readIORef leases
        length (ordNub owners) `shouldBe` 2

      it "repositoryPull_reportsInvalidYamlAndRetriesTheSameRevisionAfterCorrection" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub (Just "https://127.0.0.1:1") "team" "dashboards" "main" (GitSync.PersonalToken "fixture") Nothing ""
        deleted <- UUIDId <$> UUID.nextRandom
        void $ runQueryEffect tr $ Dashboards.insert (Dashboards.mkDashboardVM deleted testPid frozenTime (Projects.UserId UUID.nil)){Dashboards.title = "Removed", Dashboards.gitSyncId = Just sync.id, Dashboards.filePath = Just "removed.yaml", Dashboards.fileSha = Just "old"}
        let pull yaml =
              runTestBgRecordingHTTP frozenTime tr
                $ withHTTPResponses
                  ( \_ endpoint ->
                      pure
                        $ Just
                        $ if "/git/trees/" `T.isInfixOf` toText endpoint
                          then "{\"tree\":[{\"path\":\"dashboards/overview.yaml\",\"sha\":\"blob\",\"type\":\"blob\"},{\"path\":\"dashboards/valid.yaml\",\"sha\":\"valid\",\"type\":\"blob\"}]}"
                          else
                            if "/contents/" `T.isInfixOf` toText endpoint
                              then AE.encode $ AE.object ["content" AE..= extractBase64 (B64.encodeBase64 (if "valid.yaml" `T.isInfixOf` toText endpoint then "title: Valid\nwidgets: []\n" else yaml))]
                              else "{\"sha\":\"head\"}"
                  )
                $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.GitSyncRepository testPid sync.id)
        _ <- pull "title: [invalid"
        Just failed <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        runQueryEffect tr (Dashboards.getDashboardByProjectId testPid deleted) >>= (`shouldSatisfy` isNothing)
        partial <- runQueryEffect tr $ GitSync.getRepositoryDashboardState testPid sync.id
        M.keys partial `shouldBe` ["valid.yaml"]
        failed.lastRevision `shouldBe` Nothing
        failed.lastError `shouldSatisfy` maybe False (T.isInfixOf "overview.yaml" . (.message))
        _ <- pull "title: Recovered\nwidgets: []\n"
        Just recovered <- runQueryEffect tr $ GitSync.getGitSyncById testPid sync.id
        (recovered.lastRevision, recovered.lastError) `shouldBe` (Just "head", Nothing)
        dashboards <- runQueryEffect tr $ GitSync.getRepositoryDashboardState testPid sync.id
        M.keys dashboards `shouldBe` ["overview.yaml", "valid.yaml"]

      -- Without a credential there is nothing to read repos through, so the page must point at
      -- the control that fixes that rather than offer a form whose every submission is discarded.
      it "asks for an account connection before it asks for a mapping" \tr -> do
        (_, html) <- testServant tr $ PageCodeContext.codeMappingsGetH testPid Nothing
        let out = render html
        out `shouldSatisfy` T.isInfixOf "Connect repositories"
        out `shouldSatisfy` T.isInfixOf ("href=\"/p/" <> testPid.toText <> "/repositories/connect\"")
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
        (_, accountPage) <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.repositoryConnectGetH testPid Nothing
        let out = render $ Lucid.toHtml accountPage.content
        for_ (["zzz-ours", "aaa-stray", "Choose an account"] :: [Text]) \label ->
          T.isInfixOf label out `shouldBe` True

      -- A frame that no mapping covers is exactly when someone needs the mapping form, and
      -- the one thing they cannot be expected to retype is the path they were just looking at.
      it "carries the unmapped frame's path into the form" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        _ <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "acme" "monoscope-config" "main" (GitSync.PersonalToken "ghp_test") Nothing ""

        (_, unmapped) <- testServant tr $ PageCodeContext.codeContextH testPid (Just "/srv/app/checkout.py") (Just 88) Nothing Nothing
        render unmapped `shouldSatisfy` T.isInfixOf "repositories?sample=%2Fsrv%2Fapp%2Fcheckout.py"

        (_, form) <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsGetH testPid (Just "/srv/app/checkout.py")
        render form `shouldSatisfy` T.isInfixOf "sample=%2Fsrv%2Fapp%2Fcheckout.py"

      -- Membership is checked per handler, not by the router, and these two never checked it:
      -- any signed-in user could add or delete another project's mappings by its id.
      it "codeMappings_nonMember_cannotMutate" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
            form repo prefix = PageCodeContext.CodeMappingForm (Just repo) (Just "main") Nothing Nothing (Just prefix) (Just "") Nothing
        victim <- createTestProject tr "victim"
        let mappingIds = map (.id) <$> runQueryEffect tr (CodeContext.getCodeMappings victim)
        _ <- runQueryEffect tr $ GitSync.insertGitHubSync encKey victim Git.GitHub Nothing "acme" "monoscope-config" "main" (GitSync.PersonalToken "ghp_test") Nothing ""
        _ <- testServant tr $ withUnavailableRepositoryAPI $ PageCodeContext.codeMappingsPostH victim (form "checkout-service" "/srv/app/")
        seeded <- mappingIds
        length seeded `shouldBe` 1

        assertNonMemberDenied tr victim $ PageCodeContext.codeMappingsPostH victim (form "attacker-repo" "/evil/")
        forM_ seeded $ assertNonMemberDenied tr victim . PageCodeContext.codeMappingsDeleteH victim
        mappingIds >>= (`shouldBe` seeded)

    describe "Frame source endpoint (codeContextH)" do
      -- A project that never configured a mapping opens an error panel like anyone else. It
      -- must see a sentence, not a failure it did not cause — and the sentence has to say
      -- which of the two possible problems it is.
      it "explains an unresolvable frame instead of failing the panel" \tr -> do
        (_, unmapped) <- testServant tr $ PageCodeContext.codeContextH testPid (Just "/srv/app/checkout.py") (Just 88) Nothing Nothing
        render unmapped `shouldSatisfy` T.isInfixOf "No linked repository covers /srv/app/checkout.py"

        (_, noLine) <- testServant tr $ PageCodeContext.codeContextH testPid (Just "/srv/app/checkout.py") Nothing Nothing Nothing
        render noLine `shouldSatisfy` T.isInfixOf "no file and line"
