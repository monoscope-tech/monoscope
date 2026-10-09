module Pages.ImpactReviewsSpec (spec) where

import Data.Aeson qualified as AE
import Data.Base64.Types (extractBase64)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.Effectful.LLM qualified as LLM
import Data.Pool (withResource)
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Only (..), query)
import Effectful.Dispatch.Dynamic (interpose)
import Effectful.Error.Static (catchError)
import Effectful.Reader.Static qualified as Reader
import Models.Projects.CodeContext qualified as CodeContext
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ImpactReviews qualified as Reviews
import Pages.Bots.BotTestHelpers (withHTTPResponses)
import Pages.CodeContext qualified as CodeContextPage
import Pages.GitSync qualified as GitSyncPage
import Pkg.Git qualified as Git
import Pkg.ImpactReview qualified as Impact
import Pkg.TestUtils
import Relude
import Servant (ServerError (..))
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATBackgroundCtx)
import Test.Hspec
import "cryptonite" Crypto.Hash.Algorithms (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC


revision :: Text
revision = T.replicate 40 "a"


setup :: TestResources -> IO ()
setup tr = do
  let cfg = tr.trATCtx.config
  credential <- maybe (fail "credential missing") pure =<< runQueryEffect tr (GitSync.upsertGitHubCredential (encodeUtf8 cfg.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "impact-org" (Just 987654) Nothing)
  runQueryEffect tr $ CodeContext.insertCodeMapping testPid credential.id (Git.RepoRef "impact-org" "checkout" "main") (Just "test-service") "" ""


payload :: Text -> Text -> Text -> AE.Value
payload action sha timestamp =
  AE.object
    [ "action" AE..= action
    , "number" AE..= (7 :: Int)
    , "installation" AE..= AE.object ["id" AE..= (987654 :: Int)]
    , "repository" AE..= AE.object ["full_name" AE..= ("impact-org/checkout" :: Text)]
    , "pull_request"
        AE..= AE.object
          ["state" AE..= (if action == "closed" then "closed" else "open" :: Text), "draft" AE..= (action == "converted_to_draft"), "updated_at" AE..= timestamp, "head" AE..= AE.object ["sha" AE..= sha]]
    ]


deliver :: TestResources -> AE.Value -> IO AE.Value
deliver tr value = do
  let body = toStrict $ AE.encode value
      signature = "sha256=" <> decodeUtf8 (B16.encode $ BA.convert (HMAC.hmac ("impact-secret" :: ByteString) body :: HMAC.HMAC SHA256))
      configured = tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppWebhookSecret = "impact-secret"}}}
  snd <$> runAsBaseRecordingHTTP configured (GitSyncPage.gitWebhookPostH Git.GitHub $ Git.WebhookReq (Just "pull_request") (Just signature) Nothing Nothing Nothing body)


spec :: Spec
spec = around withTestResources $ describe "Production impact reviews" do
  it "queues signed source-repository events once, rejects unverified events, and retains the latest revision" \tr -> do
    setup tr
    runQueryEffect tr (Reviews.repositorySettings testPid) >>= \settings -> map (.includeEvidence) settings `shouldBe` [False]
    let firstEvent = payload "opened" revision "2025-01-01T00:00:00Z"
        next = T.replicate 40 "b"
    deliver tr firstEvent `shouldReturn` AE.object ["status" AE..= ("ok" :: Text)]
    deliver tr firstEvent `shouldReturn` AE.object ["status" AE..= ("ok" :: Text)]
    queued <- withResource tr.trPool \conn -> query conn "SELECT count(*) FROM background_jobs WHERE payload->>'tag' = 'ReviewPullRequest'" () :: IO [Only Int]
    queued `shouldBe` [Only 1]
    [old] <- runQueryEffect tr $ Reviews.latestRuns testPid
    runQueryEffect tr (Reviews.claimRun old) `shouldReturn` True
    runQueryEffect tr (Reviews.currentRun old) `shouldReturn` True
    void $ deliver tr $ payload "synchronize" next "2025-01-01T00:01:00Z"
    void $ deliver tr firstEvent
    runQueryEffect tr (Reviews.currentRun old) `shouldReturn` False
    runs <- runQueryEffect tr $ Reviews.latestRuns testPid
    map (.latestRevision) runs `shouldBe` [next, next]
    new <- maybe (fail "new revision missing") pure $ find ((== next) . (.revision)) runs
    runQueryEffect tr (Reviews.claimRun new) `shouldReturn` False
    runQueryEffect tr $ Reviews.releaseRun old Nothing
    runQueryEffect tr (Reviews.claimRun new) `shouldReturn` True
    runQueryEffect tr (Reviews.currentRun new) `shouldReturn` True
    void $ deliver tr $ payload "closed" next "2025-01-01T00:02:00Z"
    runQueryEffect tr (Reviews.currentRun new) `shouldReturn` False
    void $ deliver tr $ payload "reopened" next "2025-01-01T00:03:00Z"
    reopened <- maybe (fail "run missing") pure =<< runQueryEffect tr (Reviews.getRun new.id)
    reopened.state `shouldBe` Reviews.Queued
    runQueryEffect tr $ Reviews.releaseRun new Nothing
    (_, unsignedStatus) <-
      runAsBaseRecordingHTTP tr
        $ catchError @ServerError
          (GitSyncPage.gitWebhookPostH Git.GitHub (Git.WebhookReq (Just "pull_request") Nothing Nothing Nothing Nothing (toStrict $ AE.encode $ payload "synchronize" (T.replicate 40 "c") "2025-01-01T00:04:00Z")) $> 200)
          (\_ err -> pure err.errHTTPCode)
    unsignedStatus `shouldBe` 503
    final <- runQueryEffect tr $ Reviews.latestRuns testPid
    length final `shouldBe` 2
    otherPid <- createTestProject tr "Other review project"
    runQueryEffect tr (Reviews.retryRun otherPid new.id)
    runQueryEffect tr (Reviews.latestRuns otherPid) >>= (`shouldSatisfy` null)
    credential <- maybe (fail "credential missing") pure =<< runQueryEffect tr (GitSync.upsertGitHubCredential (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) otherPid Git.GitHub Nothing "impact-org" (Just 987654) Nothing)
    runQueryEffect tr $ CodeContext.insertCodeMapping otherPid credential.id (Git.RepoRef "impact-org" "checkout" "main") Nothing "" ""
    void $ deliver tr $ payload "synchronize" (T.replicate 40 "c") "2025-01-01T00:04:00Z"
    [otherRun] <- runQueryEffect tr $ Reviews.latestRuns otherPid
    otherRun.projectId `shouldBe` otherPid
    otherRun.revision `shouldBe` T.replicate 40 "c"
    -- Disabling a repository during work must release the lease and remain rerunnable.
    runQueryEffect tr (Reviews.claimRun otherRun) `shouldReturn` True
    runQueryEffect tr $ Reviews.updateSettings otherPid (Reviews.ReviewSettings "impact-org" "checkout" False True)
    runQueryEffect tr (Reviews.currentRun otherRun) `shouldReturn` False
    runQueryEffect tr $ Reviews.releaseRun otherRun Nothing
    released <- maybe (fail "run missing") pure =<< runQueryEffect tr (Reviews.getRun otherRun.id)
    released.state `shouldBe` Reviews.Incomplete

  it "publishes aggregate evidence, updates its own comment, and honors repository disable and links-only settings" \tr -> do
    setup tr
    runQueryEffect tr $ Reviews.updateSettings testPid (Reviews.ReviewSettings "impact-org" "checkout" True True)
    void $ deliver tr $ payload "opened" revision "2025-01-01T00:00:00Z"
    [run] <- runQueryEffect tr $ Reviews.latestRuns testPid
    apiKey <- createTestAPIKey tr testPid "Impact review telemetry"
    ingestLog tr apiKey "private-log-body-must-not-leave-monoscope" frozenTime
    -- Throwaway RSA fixture generated only for these JWT tests; never registered with a GitHub App.
    pem <- BS.readFile "test/fixtures/github-app-test.pem"
    let cfg = tr.trATCtx.config{githubAppId = "123", githubAppPrivateKey = extractBase64 $ B64.encodeBase64 pem}
        pr = AE.object ["head" AE..= AE.object ["sha" AE..= revision], "base" AE..= AE.object ["sha" AE..= T.replicate 40 "d"], "state" AE..= ("open" :: Text), "draft" AE..= False, "changed_files" AE..= (1 :: Int), "updated_at" AE..= ("2025-01-01T00:00:00Z" :: Text)]
        files :: [Git.PullRequestFile]
        files = [Git.PullRequestFile "checkout.hs" "modified" (Just "@@ -1 +1 @@\n-old\n+new")]
        marker = "<!-- monoscope-impact:" <> testPid.toText <> " -->"
        transport hasComment = withHTTPResponses \_ endpoint ->
          pure
            $ Just
            $ AE.encode
            $ if "/access_tokens" `T.isSuffixOf` toText endpoint
              then AE.object ["token" AE..= ("fixture-token" :: Text), "expires_at" AE..= ("2030-01-01T00:00:00Z" :: Text)]
              else
                if "/files?per_page=100" `T.isSuffixOf` toText endpoint
                  then AE.toJSON files
                  else
                    if "/comments?per_page=100&page=1" `T.isSuffixOf` toText endpoint
                      then
                        AE.toJSON @[AE.Value]
                          $ if not hasComment
                            then []
                            else
                              [AE.object ["id" AE..= (88 :: Int), "body" AE..= marker, "performed_via_github_app" AE..= AE.object ["id" AE..= (456 :: Int)]], AE.object ["id" AE..= (99 :: Int), "body" AE..= marker, "performed_via_github_app" AE..= AE.object ["id" AE..= (123 :: Int)]]]
                      else
                        if "/issues/comments/99" `T.isSuffixOf` toText endpoint || "/issues/7/comments" `T.isSuffixOf` toText endpoint
                          then AE.object ["id" AE..= (99 :: Int)]
                          else pr
        result = Impact.ReviewResult Impact.WorthChecking [Impact.Finding (Impact.ChangedLine "checkout.hs" 1 Impact.Head) "The changed operation may stop emitting the signal used by the monitor. *check* @impact-review-test [outside](https://untrusted.invalid)\r#heading" "Check the monitor query before deploying." ["telemetry-1"]] [] [] frozenTime frozenTime
        fakeModel :: Impact.ReviewResult -> ATBackgroundCtx a -> ATBackgroundCtx a
        fakeModel answer = interpose @LLM.LLM \_ -> \case
          LLM.CallLLM _ prompt _ -> do
            liftIO $ prompt `shouldNotSatisfy` T.isInfixOf "private-log-body-must-not-leave-monoscope"
            pure $ Right $ decodeUtf8 $ AE.encode (answer :: Impact.ReviewResult)
          LLM.CallAgenticChat history params token -> LLM.callAgenticChat history params token
          LLM.EmbedDocuments config docs -> LLM.embedDocuments config docs
    (requests, ()) <- runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = cfg}) $ transport True $ fakeModel result $ Impact.reviewPullRequest run.id
    let comments = filter (T.isInfixOf "/issues/comments/99" . fst) requests
    length comments `shouldBe` 1
    let published = T.intercalate " " $ map (decodeUtf8 . snd) comments
    published `shouldSatisfy` T.isInfixOf "Worth checking"
    published `shouldSatisfy` T.isInfixOf "Observed"
    published `shouldSatisfy` T.isInfixOf "\\\\*check"
    published `shouldNotSatisfy` T.isInfixOf "@impact-review-test"
    published `shouldNotSatisfy` T.isInfixOf "https://untrusted.invalid"
    published `shouldNotSatisfy` T.isInfixOf "private-log-body-must-not-leave-monoscope"
    published `shouldSatisfy` T.isInfixOf revision
    completed <- maybe (fail "run missing") pure =<< runQueryEffect tr (Reviews.getRun run.id)
    completed.state `shouldBe` Reviews.Completed
    completed.commentId `shouldBe` Just 99
    -- Reruns recover the app-owned comment and keep production prose out of links-only output.
    void $ testServant tr $ CodeContextPage.impactReviewSettingsPostH testPid (Reviews.ReviewSettings "impact-org" "checkout" True False)
    void $ testServant tr $ CodeContextPage.impactReviewRetryH testPid run.id
    (linksRequests, ()) <- runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = cfg}) $ transport True $ fakeModel result $ Impact.reviewPullRequest run.id
    let linksPublished = T.intercalate " " $ map (decodeUtf8 . snd) $ filter (T.isInfixOf "/issues/comments/99" . fst) linksRequests
    linksPublished `shouldSatisfy` T.isInfixOf "telemetry evidence"
    linksPublished `shouldNotSatisfy` T.isInfixOf "Observed"
    linksPublished `shouldNotSatisfy` T.isInfixOf "The changed operation"
    -- A fabricated location cannot become a finding, even if the model supplies a verdict.
    runQueryEffect tr $ Reviews.retryRun testPid run.id
    let invalid = result{Impact.findings = [Impact.Finding (Impact.ChangedLine "unrelated.hs" 1 Impact.Head) "Invented mechanism" "Invented next step" ["telemetry-1"]]}
    (newRequests, ()) <- runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = cfg}) $ transport False $ fakeModel invalid $ Impact.reviewPullRequest run.id
    let created = T.intercalate " " $ map (decodeUtf8 . snd) $ filter (T.isSuffixOf "/issues/7/comments" . fst) newRequests
    created `shouldSatisfy` T.isInfixOf "Coverage unknown"
    created `shouldNotSatisfy` T.isInfixOf "Invented mechanism"
    void $ testServant tr $ CodeContextPage.impactReviewSettingsPostH testPid (Reviews.ReviewSettings "impact-org" "checkout" False False)
    void $ deliver tr $ payload "synchronize" (T.replicate 40 "b") "2025-01-01T00:01:00Z"
    runs <- runQueryEffect tr $ Reviews.latestRuns testPid
    length runs `shouldBe` 1
    runQueryEffect tr (Reviews.repositorySettings testPid) >>= \settings -> map (\s -> (s.enabled, s.includeEvidence)) settings `shouldBe` [(False, False)]

  it "reports invalid and unconfigured App webhook deliveries as HTTP failures" \tr -> do
    let body = toStrict $ AE.encode $ payload "opened" revision "2025-01-01T00:00:00Z"
        request = Git.WebhookReq (Just "pull_request") (Just $ "sha256=" <> T.replicate 64 "0") Nothing Nothing Nothing body
    forM_ ([("", 503), ("impact-secret", 401)] :: [(Text, Int)]) \(secret, expected) -> do
      let configured = tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppWebhookSecret = secret}}}
      (_, status) <-
        runAsBaseRecordingHTTP configured
          $ catchError @ServerError
            (GitSyncPage.gitWebhookPostH Git.GitHub request $> 200)
            (\_ err -> pure err.errHTTPCode)
      status `shouldBe` expected
