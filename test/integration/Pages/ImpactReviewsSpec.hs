{-# LANGUAGE OrPatterns #-}

module Pages.ImpactReviewsSpec (spec) where

import BackgroundJobs qualified
import BackgroundJobs.Types qualified as Jobs
import CLI.Harness qualified as CLI
import Control.Monad.Trans.Except (throwE)
import Data.Aeson qualified as AE
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AET
import Data.Base64.Types (extractBase64)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Cache qualified as Cache
import Data.Conduit.Combinators qualified as CC
import Data.Effectful.AgentArtifacts qualified as Artifacts
import Data.Effectful.AgentWorkspace qualified as Workspace
import Data.Effectful.AgentWorkspace.Guest qualified as Guest
import Data.Effectful.CodeReview qualified as Agent
import Data.Effectful.CodeReview.Artifacts qualified as Native
import Data.Effectful.CodeReview.Checkout qualified as Checkout
import Data.Effectful.CodeReview.Explanation qualified as Media
import Data.Effectful.CodeReview.Outbox qualified as Outbox
import Data.Effectful.CodeReview.Policy qualified as Policy
import Data.Effectful.CodeReview.Types qualified as Proof (ProofMedia (..))
import Data.Effectful.GitPublication qualified as Publication
import Data.Effectful.LLM qualified as LLM
import Data.Effectful.ReviewEvaluation qualified as Evaluation
import Data.Effectful.Wreq qualified as HTTP
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (execute)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..), getAeson)
import Database.PostgreSQL.Simple.Types (Query (..))
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Dispatch.Dynamic (impose, interpose, interpret, localSeqUnliftIO, passthrough)
import Effectful.Error.Static (catchError)
import Effectful.Reader.Static qualified as Reader
import Models.Apis.Investigations qualified as Investigations
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogQueries (selectLogTable)
import Models.Projects.AgentUsage qualified as Usage
import Models.Projects.CodeContext qualified as CodeContext
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ImpactReviews qualified as Reviews
import Models.Projects.IssueFixes qualified as Fixes
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.ProjectMemory qualified as Memory
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types qualified as H
import Network.Minio qualified as Minio
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Test qualified as WT
import Pages.AIThreads qualified as AIThreads

import Pages.CodeContext qualified as CodeContextPage
import Pages.GitSync qualified as GitSyncPage
import Pkg.AI qualified as AI
import Pkg.AgentCatalog qualified as Catalog
import Pkg.Components.Widget qualified as Widget
import Pkg.Git qualified as Git
import Pkg.GitCommand qualified as HostGit
import Pkg.ImpactReview qualified as Impact
import Pkg.Minio qualified as Storage
import Pkg.Parser.Stats qualified as KQL
import Pkg.TestUtils hiding (call)
import Pkg.TestUtils qualified as Test
import Servant (ServerError (..), getResponse)
import Servant.Server.Generic (genericServeTWithContext)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing, createFileLink, doesDirectoryExist, doesFileExist, listDirectory, removeFile, removePathForcibly, renameDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose)
import System.IO.Error (userError)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files qualified as Posix
import System.Posix.Signals qualified as Signal
import System.Process.Typed (createPipe, getPid, getStdin, getStdout, nullStream, proc, readProcessStdout_, setEnv, setStderr, setStdin, setStdout, setWorkingDir, stopProcess, waitExitCode, withProcessTerm)
import System.Types (ATBackgroundCtx, ATBaseCtx)
import UnliftIO.Async (cancel, concurrently, waitCatch, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket_, finally, throwIO, tryAny)
import UnliftIO.MVar qualified as MVar
import UnliftIO.Timeout qualified as Timeout
import Web.HttpApiData (parseUrlPiece)
import Web.Links (proofRatingPath)
import Web.Routes qualified as Routes
import "cryptonite" Crypto.Hash (Digest, hash)
import "cryptonite" Crypto.Hash.Algorithms (SHA256, SHA512)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC


-- | A complete merge-risk grading, which every accepted review draft carries.
graded :: [(AE.Key, AE.Value)]
graded = ["claims" AE..= ([] :: [AE.Value]), "summary" AE..= ("Changes retry handling." :: Text), "assessments" AE..= [Impact.Assessment d Impact.Good [] | d <- universe]]


revision :: Text
revision = T.replicate 40 "a"


setup :: TestResources -> IO ()
setup tr = setupProject tr testPid


setupProject :: TestResources -> Projects.ProjectId -> IO ()
setupProject tr pid = do
  credential <- maybe (fail "credential missing") pure =<< runQueryEffect tr (GitSync.upsertGitHubCredential (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) pid Git.GitHub Nothing "impact-org" (Just 987654) Nothing)
  runQueryEffect tr $ CodeContext.insertCodeMapping pid credential.id (Git.RepoRef "impact-org" "checkout" "main") (Just "test-service") "" ""
  -- Handlers render the repository list; a warm cache keeps them off the GitHub token endpoint.
  Cache.insert tr.trATCtx.repoListCache credential.id [Git.GitRepo "impact-org/checkout" "checkout" False "main"]
  void $ Test.call tr $ AIThreads.routineInstallPostH pid (AIThreads.RoutineTemplateForm "production-impact-review")


-- | The fixture project, its mapped repository and one runtime issue to fix.
fixFixture :: TestResources -> Text -> IO (CodeContext.CodeMapping, Issues.IssueId, Fixes.Revision)
fixFixture tr target = do
  setup tr
  [mapping] <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
  issue <- seedIssue tr (issueSeed Issues.RuntimeException target){title = "Retain retry tokens", service = Just "test-service"}
  (mapping,issue,) <$> fixRevision revision


fixRevision :: Text -> IO Fixes.Revision
fixRevision = either (fail . show) pure . Fixes.mkRevision


payload :: Text -> Text -> Text -> AE.Value
payload action sha timestamp =
  AE.object
    [ "action" AE..= action
    , "number" AE..= (7 :: Int)
    , "installation" AE..= AE.object ["id" AE..= (987654 :: Int)]
    , "repository" AE..= AE.object ["full_name" AE..= ("impact-org/checkout" :: Text)]
    , "pull_request"
        AE..= AE.object
          ["state" AE..= (if action == "closed" then "closed" else "open" :: Text), "draft" AE..= (action == "converted_to_draft"), "changed_files" AE..= (1 :: Int), "updated_at" AE..= timestamp, "base" AE..= AE.object ["sha" AE..= T.replicate 40 "d"], "head" AE..= AE.object ["sha" AE..= sha]]
    ]


deliver :: TestResources -> AE.Value -> IO AE.Value
deliver tr = fmap snd . deliverEvent tr "pull_request" (const $ pure Nothing)


-- | A signed delivery of @event@, with @respond@ answering the GitHub API calls it makes.
deliverEvent :: TestResources -> Text -> (Text -> IO (Maybe AE.Value)) -> AE.Value -> IO ([(Text, LByteString)], AE.Value)
deliverEvent tr event respond value = do
  app <- githubApp tr
  let body = toStrict $ AE.encode value
      signature = "sha256=" <> decodeUtf8 (B16.encode $ BA.convert (HMAC.hmac ("impact-secret" :: ByteString) body :: HMAC.HMAC SHA256))
      configured = tr{trATCtx = tr.trATCtx{config = app{githubAppWebhookSecret = "impact-secret"}}}
  runAsBaseRecordingHTTP configured $ githubResponses respond $ GitSyncPage.gitWebhookPostH Git.GitHub $ Git.WebhookReq (Just event) (Just signature) Nothing Nothing Nothing body


commentPayload :: Text -> Text -> AE.Value
commentPayload association body =
  AE.object
    [ "action" AE..= ("created" :: Text)
    , "installation" AE..= AE.object ["id" AE..= (987654 :: Int)]
    , "repository" AE..= AE.object ["full_name" AE..= ("impact-org/checkout" :: Text)]
    , "issue" AE..= AE.object ["number" AE..= (7 :: Int), "pull_request" AE..= AE.object []]
    , "comment" AE..= AE.object ["id" AE..= (55 :: Int), "body" AE..= body, "author_association" AE..= association, "user" AE..= AE.object ["login" AE..= ("ada" :: Text), "type" AE..= ("User" :: Text)]]
    ]


-- | The GitHub App configuration plus a transport answering its token exchange
-- and then @respond@ (by URL; 'Nothing' falls through to the recorder).
githubApp :: TestResources -> IO EnvConfig
githubApp tr = do
  -- Throwaway RSA fixture generated only for these JWT tests; never registered with a GitHub App.
  pem <- readFileBS "test/fixtures/github-app-test.pem"
  pure tr.trATCtx.config{githubAppId = "123", githubAppPrivateKey = extractBase64 $ B64.encodeBase64 pem}


githubResponses :: (HTTP.HTTP :> es, IOE :> es) => (Text -> IO (Maybe AE.Value)) -> Eff es a -> Eff es a
githubResponses respond = withHTTPResponses \_ endpoint ->
  if "/access_tokens" `T.isSuffixOf` toText endpoint
    then pure $ Just $ AE.encode $ AE.object ["token" AE..= ("fixture-token" :: Text), "expires_at" AE..= ("2030-01-01T00:00:00Z" :: Text)]
    else fmap AE.encode <$> respond (toText endpoint)


-- | A pull request whose files, comments and comment writes GitHub answers; anything else is the PR itself.
pullRequestApi :: [Git.PullRequestFile] -> [AE.Value] -> IO () -> AE.Value -> ATBackgroundCtx a -> ATBackgroundCtx a
pullRequestApi files comments published pr = githubResponses \url ->
  Just
    <$> if
      | "/files?per_page=100" `T.isInfixOf` url -> pure $ AE.toJSON $ if "&page=2" `T.isSuffixOf` url then drop 100 files else take 100 files
      | "/comments?per_page=100&page=1" `T.isSuffixOf` url -> pure $ AE.toJSON comments
      | "/compare/" `T.isInfixOf` url -> pure $ AE.object ["files" AE..= ([] :: [AE.Value])]
      | "/issues/comments/99" `T.isSuffixOf` url || "/issues/7/comments" `T.isSuffixOf` url -> published $> AE.object ["id" AE..= (99 :: Int)]
      | otherwise -> pure pr


pullRequestAt :: Text -> Text -> Int -> AE.Value
pullRequestAt base headRevision changed = AE.object ["head" AE..= AE.object ["sha" AE..= headRevision], "base" AE..= AE.object ["sha" AE..= base], "state" AE..= ("open" :: Text), "draft" AE..= False, "changed_files" AE..= changed, "updated_at" AE..= ("2025-01-01T00:00:00Z" :: Text)]


noProofs :: Applicative m => Agent.ProofHooks m
noProofs = Agent.ProofHooks{save = \_ _ -> pure $ Left "Unexpected proof", progress = const $ pure ()}


-- | The fall-through of every test interpreter: operations a spec does not expect are refused.
refuse :: Agent.CodeReview m a -> a
refuse = \case
  Agent.ReadSessionContext{} -> Right Nothing
  Agent.ReviewRepository{} -> Left "Unexpected advisory review"
  Agent.ReviewSource{} -> Left "Unexpected source review"
  Agent.PrepareRepository{} -> Left "Unexpected workspace preparation"
  Agent.ResolveImage -> Left "Unexpected image resolution"
  Agent.ReviewPatch{} -> Left "Unexpected patch comparison"
  Agent.FixRepository{} -> Left "Unexpected issue-fix task"
  Agent.AdmitRegression{} -> Left "Unexpected admission task"
  Agent.VerifyFixArtifacts{} -> Left "Unexpected artifact task"
  Agent.ExplainVerifiedFix{} -> Left "Unexpected explanation task"
  Agent.ReviewVerifiedFix{} -> Left "Unexpected candidate review task"


-- | A review agent that only answers repository reviews.
repositoryReview :: (forall m. Agent.Request "pr_review" -> (Agent.TelemetryQuery -> m (Either Text AE.Value)) -> (Agent.KnowledgeRequest -> m (Either Text AE.Value)) -> (forall r. m r -> IO r) -> IO (Either Text Text)) -> ATBackgroundCtx a -> ATBackgroundCtx a
repositoryReview answer = interpose @Agent.CodeReview \env -> \case
  Agent.ReviewRepository _ request respond knowledge _ _ _ _ _ _ -> localSeqUnliftIO env \unlift -> liftIO $ answer request respond knowledge unlift
  operation -> pure $ refuse operation


-- | A child process speaking the review protocol from a test fixture script.
scripted :: IOE :> es => Maybe Agent.ArtifactStore -> FilePath -> [FilePath] -> Eff (Agent.CodeReview ': es) a -> Eff es a
scripted store script = Agent.runCodeReviewReal store (Right Nothing) (Just script)


field :: AE.FromJSON a => AE.Key -> AE.Value -> AET.Parser a
field key = AE.withObject "fixture" (AE..: key)


parse :: (a -> AET.Parser b) -> a -> IO b
parse parser = either fail pure . AET.parseEither parser


jsonText :: AE.ToJSON a => a -> Text
jsonText = decodeUtf8 . AE.encode


readJSON :: FilePath -> IO AE.Value
readJSON path = readFileBS path >>= either fail pure . AE.eitherDecodeStrict


-- | Appends what it is given; the second action reads it all back in order.
recorder :: IO (a -> IO (), IO [a])
recorder = newIORef [] <&> \ref -> (\value -> modifyIORef' ref (<> [value]), readIORef ref)


-- | A VM allocation of @attempt@ whose cleanup was confirmed.
allocationReleased :: UUIDId "agent_attempt" -> Agent.AllocationReceipt -> IO ()
allocationReleased attempt receipt = do
  Agent.allocationAttempt receipt `shouldBe` attempt
  parse (field "outcome") (AE.toJSON receipt) `shouldReturn` ("released" :: Text)
  parse (field "durationMs") (AE.toJSON receipt) >>= (`shouldSatisfy` (> (0 :: Word64)))


withTemp :: String -> (FilePath -> IO a) -> IO a
withTemp name action = withSystemTempDirectory name $ canonicalizePath >=> action


storeIn :: FilePath -> IO Agent.ArtifactStore
storeIn directory = Agent.ArtifactStore (directory </> "usage") (directory </> "cache") <$ traverse_ (createDirectoryIfMissing True . (directory </>)) ["usage", "cache"]


-- | Git in a fixture checkout, isolated from the host's hooks and signing.
gitIn :: FilePath -> [String] -> IO ByteString
gitIn repository args = toStrict <$> readProcessStdout_ (setWorkingDir repository $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "commit.gpgSign=false"] <> args)


gitText :: FilePath -> [String] -> IO Text
gitText repository = fmap (T.strip . decodeUtf8) . gitIn repository


initRepo :: FilePath -> IO ()
initRepo repository = do
  createDirectoryIfMissing True repository
  traverse_ (gitIn repository) [["init", "--quiet"], ["config", "user.name", "Fixture"], ["config", "user.email", "fixture@example.invalid"], ["config", "core.autocrlf", "false"]]


writeFiles :: FilePath -> [(FilePath, ByteString)] -> IO ()
writeFiles root = traverse_ \(path, bytes) -> createDirectoryIfMissing True (takeDirectory $ root </> path) >> writeFileBS (root </> path) bytes


-- | Writes @files@, commits the whole tree and returns the new HEAD.
commit :: FilePath -> [(FilePath, ByteString)] -> IO Text
commit repository files = do
  writeFiles repository files
  traverse_ (gitIn repository) [["add", "."], ["commit", "--quiet", "--allow-empty", "-m", "fixture"]]
  gitText repository ["rev-parse", "HEAD"]


-- | The staged diff of @files@ over HEAD; the checkout is left at HEAD.
stagedPatch :: FilePath -> [(FilePath, ByteString)] -> IO Text
stagedPatch repository files = do
  writeFiles repository files
  void $ gitIn repository ["add", "."]
  patch <- decodeUtf8 <$> gitIn repository ["diff", "--cached", "--binary", "--no-ext-diff", "--no-textconv"]
  patch <$ gitIn repository ["reset", "--hard", "HEAD"]


-- | A patch whose new-file hunk headers undercount, which appliers must recount.
shortHunks :: Text -> Text
shortHunks = unlines . map (\line -> if "@@ " `T.isPrefixOf` line then "@@ -0,0 +1,1 @@" else line) . lines


sha256 :: ByteString -> Text
sha256 bytes = show (hash bytes :: Digest SHA256)


minioFor :: EnvConfig -> Minio.ConnectInfo
minioFor config = Minio.setCreds (Minio.CredentialValue (fromString $ toString config.s3AccessKey) (fromString $ toString config.s3SecretKey) Nothing) $ Minio.setRegion config.s3Region $ fromString $ toString config.s3Endpoint


-- | Replaces one key of a JSON object.
withKey :: AE.Key -> AE.Value -> AE.Value -> AE.Value
withKey key value = \case AE.Object fields -> AE.Object $ KM.insert key value fields; other -> other


data SandboxSession = SandboxSession {id :: Text, pid :: Word32, alive :: Bool}
  deriving stock (Generic)


instance AE.FromJSON SandboxSession where
  parseJSON value = do
    session <- AE.genericParseJSON AE.defaultOptions value
    unless (session.pid > 1 && session.pid <= fromIntegral (maxBound :: Int32)) $ fail "Invalid SDK host process ID"
    pure session


sandboxSessions :: IO [SandboxSession]
sandboxSessions = do
  output <- readProcessStdout_ $ setWorkingDir "impact-review-worker" $ proc "node" ["--input-type=module", "-e", "import { listSessions } from '@earendil-works/gondolin'; console.log(JSON.stringify(await listSessions()));"]
  either fail pure $ AE.eitherDecode output


-- | The self-hosted guest image; these SDK cases need it and a hypervisor.
guestImage :: IO FilePath
guestImage = do
  available <- doesFileExist "impact-review-worker/.guest/manifest.json"
  unless available $ pendingWith "Build the self-hosted guest before running SDK integration tests"
  canonicalizePath "impact-review-worker/.guest"


-- | @IMPACT_REVIEW_TEST_PROFILE@, else the locally built @fallback@ profile.
profileImage :: FilePath -> IO FilePath
profileImage fallback = do
  configured <- lookupEnv "IMPACT_REVIEW_TEST_PROFILE"
  let selected = fromMaybe fallback configured
  available <- doesFileExist $ selected </> "manifest.json"
  unless (available || isJust configured) $ pendingWith $ "Build " <> fallback <> " or set IMPACT_REVIEW_TEST_PROFILE"
  canonicalizePath selected


nativeTestImage :: IO FilePath
nativeTestImage = profileImage "impact-review-worker/.guest"


data NativeRecovery = ReviewWake Investigations.ReviewDecision | ExpiryWake | ProviderWake | FinalProviderWake | WorkerWake | RuntimeWake
  deriving stock (Eq, Show)


data NativeInterruption = ReviewSuspension | ProviderFailure | FinalProviderFailure
  deriving stock (Eq, Show)


data SdkReply = SdkText Text | SdkTool Text AE.Value


data SdkTokens = SdkTokens {promptTokens :: Word32, cachedTokens :: Word32}


data SdkResponse = SdkResponse SdkReply SdkTokens | SdkUnavailable Text


bash :: Text -> SdkReply
bash command = SdkTool "bash" $ AE.object ["command" AE..= command]


-- | The text of the tool result answering @identifier@ (@tool-N@ is the N-th call).
toolReply :: Text -> [AE.Value] -> IO Text
toolReply identifier results = maybe (fail $ "Missing tool reply " <> toString identifier) (parse $ field "content") $ find (\value -> AET.parseEither (field "tool_call_id") value == Right identifier) results


-- | The function names a model request offers.
toolNames :: AE.Value -> IO [Text]
toolNames = parse (field "tools") >=> traverse (parse $ field "function" >=> field "name")


withSdkModel :: FilePath -> Text -> ([AE.Value] -> IO SdkReply) -> (FilePath -> IO [AE.Value] -> IO a) -> IO a
withSdkModel directory provider next = withSdkProvider directory provider (\_ results -> (`SdkResponse` SdkTokens 7 0) <$> next results)


withSdkProvider :: FilePath -> Text -> (AE.Value -> [AE.Value] -> IO SdkResponse) -> (FilePath -> IO [AE.Value] -> IO a) -> IO a
withSdkProvider directory provider next action = do
  (record, requests) <- recorder
  let app incoming respond = do
        body <- Wai.strictRequestBody incoming >>= either fail pure . AE.eitherDecode @AE.Value
        record body
        messages <- parse (field "messages") body :: IO [AE.Value]
        let results = filter (\value -> AET.parseEither (field "role") value == Right ("tool" :: Text)) messages
        next body results >>= \case
          SdkUnavailable message -> respond $ Wai.responseLBS H.status400 [("Content-Type", "application/json")] $ AE.encode $ AE.object ["error" AE..= AE.object ["message" AE..= message]]
          SdkResponse reply tokens -> do
            let (delta, reason) = case reply of
                  SdkTool name arguments -> (AE.object ["role" AE..= ("assistant" :: Text), "tool_calls" AE..= V.singleton (AE.object ["index" AE..= (0 :: Int), "id" AE..= ("tool-" <> show @Text (length results)), "type" AE..= ("function" :: Text), "function" AE..= AE.object ["name" AE..= name, "arguments" AE..= jsonText arguments]])], "tool_calls")
                  SdkText content -> (AE.object ["role" AE..= ("assistant" :: Text), "content" AE..= content], "stop")
                response = AE.object ["id" AE..= ("sdk-fixture" :: Text), "object" AE..= ("chat.completion.chunk" :: Text), "created" AE..= (0 :: Int), "model" AE..= ("fixture" :: Text), "usage" AE..= AE.object ["prompt_tokens" AE..= tokens.promptTokens, "completion_tokens" AE..= (3 :: Int), "total_tokens" AE..= (tokens.promptTokens + 3), "prompt_tokens_details" AE..= AE.object ["cached_tokens" AE..= tokens.cachedTokens]], "choices" AE..= V.singleton (AE.object ["index" AE..= (0 :: Int), "delta" AE..= delta, "finish_reason" AE..= (reason :: Text)])]
            respond $ Wai.responseLBS H.status200 [("Content-Type", "text/event-stream")] $ "data: " <> AE.encode response <> "\n\ndata: [DONE]\n\n"
  Warp.testWithApplication (pure app) \port -> do
    let models = directory </> "models.json"
    writeFileLBS models $ AE.encode $ AE.object ["providers" AE..= AE.object [fromString (toString provider) AE..= AE.object ["baseUrl" AE..= ("http://127.0.0.1:" <> show @Text port <> "/v1"), "api" AE..= ("openai-completions" :: Text), "apiKey" AE..= ("fixture" :: Text), "models" AE..= V.singleton (AE.object ["id" AE..= ("fixture" :: Text), "contextWindow" AE..= (64000 :: Int), "maxTokens" AE..= (4096 :: Int), "input" AE..= (["text", "image"] :: [Text]), "cost" AE..= AE.object ["input" AE..= (2 :: Int), "output" AE..= (10 :: Int), "cacheRead" AE..= (1 :: Int), "cacheWrite" AE..= (0 :: Int)]])]]]
    action models requests


-- Exercise the host protocol without a VM or model; Close acknowledgement is observable.
withAllocationBridge :: Text -> (Guest.Host -> FilePath -> IO a) -> IO a
withAllocationBridge mode action = withTemp "allocation-regression" \directory -> do
  let bridge = directory </> "bridge.mjs"
      json = toStrict . AE.encode
  writeFileBS bridge
    $ "#!/usr/bin/env node\n"
    <> "import { appendFileSync } from 'node:fs';\nimport { createInterface } from 'node:readline';\n"
    <> "const mode = "
    <> json mode
    <> "; const events = "
    <> json (toText $ directory </> "operations.log")
    <> ";\n"
    <> """
       for await (const line of createInterface({ input: process.stdin })) {
         const { tag } = JSON.parse(line);
         appendFileSync(events, tag + '\\n');
         if (tag === 'Close' && mode === 'timeout') { await new Promise(() => {}); }
         if (tag === 'CopyFile' && mode === 'pending') await new Promise((resolve) => setTimeout(resolve, 250));
         const result = tag === 'Resolve' ? { assetDir: '/fixture', buildId: '00000000-0000-4000-8000-000000000001' }
           : tag === 'GetIdentity' ? mode === 'malformed' ? 'malformed-id' : '00000000-0000-4000-8000-000000000002'
           : tag === 'Execute' ? { exitCode: 0, stdout: '1000\\n' } : [];
         console.log(JSON.stringify(tag === 'Close' && ['reject', 'pending'].includes(mode) ? { Left: 'cleanup refused' } : { Right: result }));
         if (tag === 'Close' && mode === 'drain') { await new Promise(resolve => setTimeout(resolve, 250)); appendFileSync(events, 'Drained\\n'); }
         if (tag === 'Close') break;
       }
       """
  Posix.setFileMode bridge 0o700
  writeFileBS (directory </> "operations.log") ""
  action (Guest.Host "fixture" bridge [] Nothing) directory


-- | The SDK worker against the fixture model: @models : image : diagnostic : options@.
sdkAgent :: IOE :> es => Agent.ArtifactStore -> [FilePath] -> Eff (Agent.CodeReview ': es) a -> Eff es a
sdkAgent store arguments action = do
  let option name = case dropWhile (/= name) arguments of _ : value : _ -> Just value; _ -> Nothing
      positional n = fromMaybe "" $ viaNonEmpty head $ drop n arguments
      (models, image, diagnostic) = (positional 0, positional 1, fromMaybe "/dev/null" $ viaNonEmpty head $ drop 2 arguments)
      host = if null image then Right Nothing else Just . (\configured -> configured{Guest.bridge = fromMaybe "test/fixtures/native-sandbox.sh" $ option "--bridge"}) <$> Agent.workerHost (maybeToList $ option "--skill") (option "--prepare") (option "--dependencies") Nothing (toText image) "impact-review-worker/pi-host.mjs"
  Agent.runCodeReviewWith (Just store) host (Just $ \environment -> setEnv ([("IMPACT_REVIEW_MODELS", models), ("IMPACT_REVIEW_USAGE_DIR", store.usageDir), ("IMPACT_REVIEW_CACHE_DIR", store.cacheDir), ("IMPACT_REVIEW_FIXTURE_SECRET", "host-only-fixture")] <> [("TMPDIR", temporary) | Just temporary <- [option "--temp"]] <> [("IMPACT_REVIEW_NATIVE_CLI", cli) | Just cli <- [option "--cli"]] <> filter (\(key, _) -> key `elem` ["PATH", "HOME", "TMPDIR", "IMPACT_REVIEW_OUTBOX_DIR", "IMPACT_REVIEW_SESSION_FILE"] && not (key == "TMPDIR" && isJust (option "--temp"))) environment) $ proc "sh" ["-c", "exec node impact-review-worker/pi-host.mjs 2>>\"$1\"", "sdk-worker", diagnostic]) action


-- Protocol fixtures supply proof explicitly; native file integrity is tested separately.
artifactProofFixture :: IOE :> es => Either Text () -> Agent.ArtifactRequest -> Eff es (Either Text Agent.VerifiedFix)
artifactProofFixture outcome request =
  Agent.runCodeReviewTest (const $ Left "Unexpected review") (const $ Left "Unexpected fix") (const $ Left "Unexpected admission") (const outcome) (const $ Left "Unexpected explanation") (const $ Left "Unexpected peer review") $ Agent.verifyFixArtifacts (Agent.ArtifactStore "fixture-usage" "fixture-cache") request


explanationProofFixture :: IOE :> es => Agent.ArtifactStore -> Either Text Agent.Explanation -> Agent.ArtifactRequest -> Agent.VerifiedFix -> Eff es (Either Text Agent.ExplainedFix)
explanationProofFixture store outcome request verified =
  Agent.runCodeReviewTest (const $ Left "Unexpected review") (const $ Left "Unexpected fix") (const $ Left "Unexpected admission") (const $ Left "Unexpected artifacts") (const outcome) (const $ Left "Unexpected peer review") $ Agent.explainVerifiedFix store request verified


fixtureExplanation :: Agent.FixResult -> Agent.Explanation
fixtureExplanation result =
  let files = Map.fromList [(name, sha256 $ encodeUtf8 name) | asset <- [minBound .. maxBound], let name = Artifacts.assetFile asset]
   in Agent.Explanation{version = 1, source = T.replicate 64 "c", rendererSha256 = T.replicate 64 "d", projectId = result.projectId, runId = result.runId, issueId = result.issueId, candidateId = result.candidateId, head = result.head, checkId = result.checkId, regressionSha256 = result.regressionSha256, durationMs = 36000, width = 1280, height = 720, renderMs = 1200, mediaBytes = fromIntegral $ sum $ map (BS.length . encodeUtf8) $ Map.keys files, videoBytes = 15, resourceCostStatus = Agent.Unpriced, tools = Map.fromList [("ffmpeg", "ffmpeg fixture"), ("graphviz", "graphviz fixture")], files}


-- | A node with this many agent slots, for admission.
slots :: Int -> Workspace.Slots
slots agents = Workspace.Slots{node = "test-node", agents, verifiers = 2, builds = 1, prepares = False}


spec :: Spec
spec = around withTestResources $ describe "Production impact reviews" do
  it "allocation bridge: a malformed VM identity issues no proof; a cancelled VM records its confirmed cleanup per close outcome" \_ -> do
    base <- either (fail . show) pure $ Workspace.mkRevision revision
    let operations directory = lines . decodeUtf8 @Text <$> readFileBS (directory </> "operations.log")
        open host directory = Guest.withGuest host Nothing directory base (Guest.Fresh Guest.Explore) Nothing (UUIDId UUID.nil)
    withAllocationBridge "malformed" \host directory -> do
      entered <- newIORef False
      (capture, receipts) <- recorder @Agent.AllocationReceipt
      open host directory capture capture (const $ writeIORef entered True) >>= (`shouldSatisfy` isLeft)
      readIORef entered `shouldReturn` False
      receipts >>= (`shouldSatisfy` null)
      operations directory >>= (`shouldContain` ["Close"])
    for_ @[] [("ok", "released"), ("drain", "released"), ("reject", "release_failed"), ("timeout", "release_failed"), ("pending", "release_failed")] \(mode, expected) -> withAllocationBridge mode \host directory -> do
      ready <- MVar.newEmptyMVar
      (capture, receipts) <- recorder @Agent.AllocationReceipt
      writeFileBS (directory </> "source") "fixture"
      let pendingCopy = readFileBS (directory </> "operations.log") >>= \bytes -> unless ("CopyFile" `BS.isInfixOf` bytes) (threadDelay 10000 >> pendingCopy)
          action session = if mode == "pending" then session.copyIn (directory </> "source") "/run/fixture" else MVar.putMVar ready () >> threadDelay 60000000
      withAsync (open host directory capture capture action) \running -> do
        Timeout.timeout 5000000 (if mode == "pending" then pendingCopy else MVar.takeMVar ready) `shouldReturn` Just ()
        cancel running
        waitCatch running >>= either (\failure -> displayException failure `shouldBe` "AsyncCancelled") (const $ expectationFailure "Cancellation was swallowed")
      (mode,) <$> (receipts >>= traverse (parse (field "outcome") . AE.toJSON)) `shouldReturn` (mode, [expected :: Text])
      closed <- operations directory
      closed `shouldContain` ["Close"]
      when (mode == "drain") $ closed `shouldContain` ["Drained"]

  it "hostGit_ignoresInheritedCredentialsAndCommands" \_ -> withSystemTempDirectory "isolated-host-git" \directory -> do
    path <- filter ((== "PATH") . fst) <$> getEnvironment
    let inherited = path <> [("MONOSCOPE_GIT_SECRET", "host-only"), ("GIT_CONFIG_COUNT", "1"), ("GIT_CONFIG_KEY_0", "alias.inherited"), ("GIT_CONFIG_VALUE_0", "!echo unsafe-host-command"), ("GIT_CONFIG_GLOBAL", "/missing-host-config")]
    for_ [(HostGit.Local, "file" :: Text), (HostGit.GitHub Nothing, "https"), (HostGit.GitHub $ Just "fixture-token", "https")] \(access, protocol) -> do
      let git args = decodeUtf8 @Text <$> readProcessStdout_ (HostGit.command inherited access directory args)
      environment <- git ["-c", "alias.environment=!env", "environment"]
      environment `shouldNotSatisfy` T.isInfixOf "MONOSCOPE_GIT_SECRET"
      environment `shouldSatisfy` T.isInfixOf ("GIT_ALLOW_PROTOCOL=" <> protocol)
      configured <- git ["config", "--list"]
      configured `shouldNotSatisfy` T.isInfixOf "alias.inherited"
      configured `shouldContainAll` ["core.hookspath=/dev/null", "core.fsmonitor=false", "credential.helper=", "http.followredirects=false"]

  it "resolves localhost offline and closes the SDK sandbox after parent loss" \_ -> withTemp "native-sdk-pipe-loss" \directory -> do
    image <- guestImage
    guestInit <- canonicalizePath "impact-review-worker/guest-init.sh"
    let sessions = map (.id) <$> sandboxSessions
    withProcessTerm (setStdin createPipe $ setStdout createPipe $ setStderr nullStream $ proc "test/fixtures/native-sandbox.sh" []) \child -> do
      let send tag contents = LBS.hPut (getStdin child) (AE.encode (AE.object ["tag" AE..= (tag :: Text), "contents" AE..= contents]) <> "\n") >> hFlush (getStdin child)
          receive = Timeout.timeout (15 * 1000000) (BSC.hGetLine (getStdout child) >>= either fail pure . AE.eitherDecodeStrict @AE.Value) >>= maybe (fail "SDK sandbox did not reply") pure
          exec argv = send "Execute" (AE.object ["argv" AE..= (argv :: [Text]), "cwd" AE..= AE.Null, "quiet" AE..= False]) >> receive >>= parse (field "Right")
          unit = AE.object ["Right" AE..= ([] :: [AE.Value])]
      send "Create" $ AE.object ["imagePath" AE..= image, "cpus" AE..= (2 :: Int), "memoryMiB" AE..= (1024 :: Int), "isolation" AE..= ("registry_cow" :: Text), "mounts" AE..= AE.object []]
      receive `shouldReturn` unit
      resolved <- exec ["/usr/bin/getent", "-s", "files", "hosts", "localhost"]
      parse (field "exitCode") resolved `shouldReturn` (0 :: Int)
      parse (field "stdout") resolved >>= \output -> words output `shouldContain` ["localhost"]
      send "CopyFile" $ AE.object ["source" AE..= guestInit, "destination" AE..= ("/run/guest-init.sh" :: Text)]
      receive `shouldReturn` unit
      initialized <- exec ["/bin/sh", "-ec", "printf '127.0.0.2 project-service\\n' > /etc/hosts; . /run/guest-init.sh; . /run/guest-init.sh; cat /etc/hosts"]
      parse (field "exitCode") initialized `shouldReturn` (0 :: Int)
      parse (field "stdout") initialized >>= \output -> lines output `shouldBe` (["127.0.0.2 project-service", "127.0.0.1 localhost", "::1 localhost"] :: [Text])
      send "GetIdentity" $ AE.toJSON ([] :: [AE.Value])
      vmId <- receive >>= parse (field "Right")
      sessions >>= (`shouldContain` [vmId])
      let logPath = directory </> "command.log"
          started = readFileBS logPath >>= \bytes -> unless ("sandbox-command-started" `BS.isInfixOf` bytes) (threadDelay 10000 >> started)
      writeFileBS logPath ""
      send "ExecuteLogged" $ AE.object ["command" AE..= AE.object ["argv" AE..= (["/bin/sh", "-c", "echo sandbox-command-started; sleep 60"] :: [Text]), "cwd" AE..= AE.Null, "quiet" AE..= True], "path" AE..= logPath, "limitBytes" AE..= (1024 :: Int)]
      Timeout.timeout (5 * 1000000) started `shouldReturn` Just ()
      hClose $ getStdin child
      Timeout.timeout (5 * 1000000) (waitExitCode child) `shouldReturn` Just ExitSuccess
      sessions >>= (`shouldNotContain` [vmId])

  for_ @[] [False, True] \abrupt ->
    it ("retains interrupted verifier evidence after SDK " <> if abrupt then "caller loss" else "cancellation") \_ -> CLI.withCliBin \cli -> withTemp "native-sdk-cancellation" \directory -> do
      image <- guestImage
      environment <- filter (\(key, _) -> key `elem` ["PATH", "HOME", "TMPDIR"]) <$> getEnvironment
      let repository = directory </> "repository"
          artifacts = directory </> "checks"
          input = directory </> "input.json"
          uid n = UUIDId $ UUID.fromWords 52 0x4000 0x80000000 n :: UUIDId "fixture"
          untilReady action = action >>= maybe (threadDelay 50000 >> untilReady action) pure
      createDirectoryIfMissing True artifacts
      initRepo repository
      base <- commit repository [("runtime.txt", "actual source\n")]
      writeFileLBS input
        $ AE.encode
        $ AE.object
          [ "request" AE..= AE.object ["projectId" AE..= testPid, "runId" AE..= uid 1, "attemptId" AE..= uid 2, "sessionId" AE..= uid 3, "base" AE..= base, "artifact" AE..= AE.object ["script" AE..= ("echo verifier-started; sleep 60" :: Text), "failureMessage" AE..= ("unused" :: Text)], "revisions" AE..= ([["base", base]] :: [[Text]])]
          , "options" AE..= AE.object ["artifactDir" AE..= artifacts, "checkout" AE..= repository]
          ]
      withProcessTerm (setEnv environment $ setStdin createPipe $ setStdout createPipe $ setStderr nullStream $ proc "node" ["test/fixtures/native-regression-caller.mjs", input, cli, image]) \child -> do
        observed <- newIORef Nothing
        let started = do
              directories <- filter (isJust . UUID.fromString) <$> listDirectory artifacts
              matches <- forM directories \name -> do
                let path = artifacts </> name
                logged <- doesFileExist (path </> "base.log") >>= bool (pure False) (BS.isInfixOf "verifier-started" <$> readFileBS (path </> "base.log"))
                if not logged
                  then pure Nothing
                  else do
                    vmIds <- readJSON (path </> "report.json") >>= parse (field "results") >>= traverse (parse $ field "vmId") :: IO [Text]
                    filter (\session -> session.id `elem` vmIds) <$> sandboxSessions >>= \case
                      [session] -> writeIORef observed (Just session) $> Just path
                      _ -> pure Nothing
              pure $ viaNonEmpty head $ catMaybes matches
            vanished session = untilReady $ bool (Just ()) Nothing . any ((== session.id) . (.id)) <$> sandboxSessions
            cleanup = do
              stopProcess child
              readIORef observed >>= traverse_ \session -> do
                closed <- Timeout.timeout (5 * 1000000) $ vanished session
                when (isNothing closed) do
                  sessions <- sandboxSessions
                  when (any (\current -> current.id == session.id && current.pid == session.pid && current.alive) sessions) $ Signal.signalProcess Signal.sigTERM $ fromIntegral session.pid
                  Timeout.timeout (5 * 1000000) (vanished session) `shouldReturn` Just ()
        ( do
            path <- Timeout.timeout (15 * 1000000) (untilReady started) >>= maybe (fail "Verifier did not produce its startup marker and VM identity") pure
            if abrupt
              then do
                pid <- getPid child >>= maybe (fail "SDK caller exited before cancellation") pure
                Signal.signalProcess Signal.sigKILL pid
                Timeout.timeout (5 * 1000000) (waitExitCode child) `shouldReturn` Just (ExitFailure (-9))
              else do
                hClose $ getStdin child
                reply <- Timeout.timeout (5 * 1000000) (BSC.hGetLine $ getStdout child) >>= maybe (fail "SDK cancellation did not return") pure
                AE.eitherDecodeStrict @AE.Value reply `shouldBe` Right (AE.object ["name" AE..= ("AbortError" :: Text)])
                Timeout.timeout (5 * 1000000) (waitExitCode child) `shouldReturn` Just ExitSuccess
            readIORef observed >>= traverse_ \session -> Timeout.timeout (5 * 1000000) (vanished session) `shouldReturn` Just ()
            readJSON (path </> "report.json") >>= parse (field "status") >>= (`shouldNotBe` ("completed" :: Text))
            readFileBS (path </> "base.log") >>= (`shouldSatisfy` BS.isInfixOf "verifier-started")
          )
          `finally` cleanup

  it "nativeSubmission_admissionSurvivesFinalProviderFailure" \_ -> withTemp "native-sdk-admission" \directory -> do
    image <- guestImage
    store <- storeIn directory
    let repository = directory </> "repository"
        diagnostic = directory </> "worker.log"
        uid n = UUIDId $ UUID.fromWords 51 0x4000 0x80000000 n
        regression = "set -eu\n. ./runtime.sh\n[ \"$(retry_token original)\" = original ] || { echo 'retry token lost'; exit 1; }\n"
    initRepo repository
    base <- commit repository [("runtime.sh", "retry_token() { printf 'lost\\n'; }\n"), ("existing-regression.sh", "# regression coverage placeholder\n")]
    testPatch <- stagedPatch repository [("regression.sh", regression)]
    imageId <- readJSON (image </> "manifest.json") >>= parse (field "buildId")
    verdict <- newIORef ("Admitted" :: Text)
    investigated <- newIORef ([] :: [Text])
    let next results = do
          for_ results (parse (field "content") >=> \text -> modifyIORef' investigated (<> [text]))
          decision <- readIORef verdict
          pure $ case length results of
            0 -> bash "set -eu; id -u; test ! -w /run/monoscope-admission.patch; test ! -w /run/monoscope-admission.json; git diff --cached --no-ext-diff --no-textconv; cat runtime.sh /run/monoscope-admission.json; printf forbidden > runtime.sh"
            1 -> SdkTool "submit_admission" (AE.object ["tag" AE..= decision, "contents" AE..= ("The regression calls the actual retry_token runtime and checks the issue's caller token." :: Text)])
            _ -> SdkText "Fixture finished."
        provider _ results =
          if length results == 2
            then pure $ SdkUnavailable "Provider failed after checked admission submission"
            else (`SdkResponse` SdkTokens 7 0) <$> next results
    withSdkProvider directory "admission-fixture" provider \models requests -> do
      let request = Agent.Request{projectId = testPid, runId = uid 1, attemptId = uid 2, repository = toText repository, base, head = base, gitToken = "", prompt = "Issue: retry_token must preserve the original retry token.", provider = "admission-fixture", model = "fixture", thinkingLevel = Agent.Off}
          admission = Agent.AdmissionRequest (Agent.FixRequest request (uid 3) imageId) (Agent.TestArtifact "bash regression.sh" testPatch "retry token lost")
          withArtifact artifact = admission{Agent.artifact = artifact}
          checkDecision expected = either (\failure -> readFileBS diagnostic >>= expectationFailure . (toString failure <>) . decodeUtf8) (parse (field "tag") . AE.toJSON >=> (`shouldBe` expected))
          run n acceptStarts (input :: Agent.AdmissionRequest) = do
            (recordCall, calls) <- recorder
            (recordUsage, usage) <- recorder
            (recordAllocation, allocations) <- recorder
            let configured = input{Agent.fix = input.fix{Agent.request = input.fix.request{Agent.runId = uid n}}} :: Agent.AdmissionRequest
                retained identifier expected = readJSON (store.usageDir </> "admissions" </> toString configured.fix.request.runId.toText </> toString identifier <> ".json") `shouldReturn` expected
            result <-
              runEff
                $ sdkAgent store [models, image, diagnostic]
                $ Agent.admitRegression
                  Agent.Isolated
                  configured
                  (const $ liftIO $ fail "Admission unexpectedly requested knowledge")
                  ( \receipt -> liftIO do
                      retained receipt.id.toText $ AE.object ["tag" AE..= ("Call" :: Text), "contents" AE..= receipt]
                      recordCall receipt
                      unless acceptStarts $ fail "Call start rejected"
                  )
                  (\receipt -> liftIO $ retained receipt.id.toText (AE.toJSON receipt) >> recordUsage receipt)
                  (\receipt -> liftIO $ retained (Agent.allocationId receipt).toText (AE.object ["tag" AE..= ("Allocation" :: Text), "contents" AE..= receipt]) >> recordAllocation receipt)
            (,,,) result <$> calls <*> usage <*> allocations
      for_ ([(1, "Admitted"), (4, "Rejected")] :: [(Word32, Text)]) \(n, decision) -> do
        writeIORef verdict decision
        priorCalls <- length <$> requests
        (result, starts, responses, allocated) <- run n True admission
        checkDecision decision result
        actual <- subtract priorCalls . length <$> requests
        (length starts, length responses, length (filter (not . (.succeeded)) responses)) `shouldBe` (actual, actual, 1)
        responses `shouldSatisfy` all (\receipt -> receipt.attemptId == request.attemptId && (not receipt.succeeded || maybe False (> 0) receipt.costNanousd) && receipt.callId `elem` map (Just . (.id)) starts)
        length allocated `shouldBe` 1
        traverse_ (allocationReleased request.attemptId) allocated
      priorCalls <- length <$> requests
      (denied, starts, responses, _) <- run 5 False admission
      isLeft denied `shouldBe` True
      (length starts, length responses) `shouldBe` (1, 0)
      length <$> requests `shouldReturn` priorCalls
      productionPatch <- stagedPatch repository [("runtime.sh", "retry_token() { printf '%s\\n' \"$1\"; }\n"), ("regression.sh", regression)]
      (production, _, _, _) <- run 6 True $ withArtifact admission.artifact{Agent.testPatch = productionPatch}
      checkDecision "Rejected" production
      writeIORef verdict "Admitted"
      writeIORef investigated []
      (recounted, _, _, _) <- run 13 True $ withArtifact admission.artifact{Agent.testPatch = shortHunks testPatch}
      checkDecision "Admitted" recounted
      readIORef investigated >>= (`shouldSatisfy` T.isInfixOf "\n+[ \"$(retry_token original)\" = original ]") . T.intercalate "\n"
      existingPatch <- stagedPatch repository [("existing-regression.sh", regression)]
      (existing, _, _, _) <- run 7 True $ withArtifact admission.artifact{Agent.script = "bash existing-regression.sh", Agent.testPatch = existingPatch}
      checkDecision "Admitted" existing
      T.intercalate "\n" <$> readIORef investigated >>= (`shouldContainAll` ["1000", "regression.sh", "existing-regression.sh", "Permission denied"])
      observed <- requests
      for_ observed $ toolNames >=> (`shouldSatisfy` all (`notElem` ["edit", "write"]))
      jsonText observed `shouldContainAll` ["Permission denied", "retry_token must preserve"]
      beforeInvalid <- length <$> requests
      for_ (zip [8 ..] [(admission{Agent.fix = admission.fix{Agent.imageBuildId = uid 99}} :: Agent.AdmissionRequest), withArtifact admission.artifact{Agent.testPatch = "not a patch"}, withArtifact admission.artifact{Agent.script = " "}, withArtifact admission.artifact{Agent.failureMessage = T.replicate 201 "x"}, withArtifact admission.artifact{Agent.testPatch = ""}]) \(n, invalid) -> do
        (rejected, _, _, _) <- run n True invalid
        isLeft rejected `shouldBe` True
      length <$> requests `shouldReturn` beforeInvalid

  it "nativeSession_resumesFixAdmissionAndPeerWithoutRepeatingCompletedWork" \_ -> CLI.withCliBin \cli -> withTemp "native-sdk-fix" \directory -> do
    image <- guestImage
    store <- storeIn directory
    bridge <- canonicalizePath "test/fixtures/native-sandbox.sh"
    let repository = directory </> "repository"
        scriptFile = directory </> "prepare.sh"
        requestFile = directory </> "request.json"
        diagnostic = directory </> "worker.log"
        uid n = UUIDId $ UUID.fromWords 55 0x4000 0x80000000 n
        testSource = "set -eu\n. ./runtime.sh\nif [ \"$(retry_token original)\" != original ]; then echo 'retry token was lost' >&2; exit 1; fi\necho regression-passed\n" :: ByteString
        broken = "retry_token() { printf 'dropped\\n'; }" :: Text
        fixed = "retry_token() { printf '%s\\n' \"$1\"; }" :: Text
        rootCause = "retry_token ignores its argument and drops the retry token." :: Text
        scratchPath = "~/notes/quote's $(touch path-expanded).txt" :: Text
        scratchContent = "\xfeffliteral $USER $(touch content-expanded)\r\n" :: Text
        patchPath = "/tmp/admitted regression's $(touch patch-path-expanded).patch" :: Text
        write path content = SdkTool "write" $ AE.object ["path" AE..= (path :: Text), "content" AE..= (content :: Text)]
        edit path old new = SdkTool "edit" $ AE.object ["path" AE..= (path :: Text), "edits" AE..= V.singleton (AE.object ["oldText" AE..= (old :: Text), "newText" AE..= (new :: Text)])]
        propose = SdkTool "propose_fix" $ AE.object ["rootCause" AE..= rootCause]
        verify = SdkTool "verify_fix" $ AE.object []
        submit = SdkTool "submit_fix" $ AE.object ["summary" AE..= ("Preserve the retry token." :: Text)]
        toolEnds = filter (\value -> AET.parseEither (field "event") value == Right ("tool_execution_end" :: Text)) . mapMaybe (AE.decodeStrict @AE.Value) . BSC.lines <$> readFileBS diagnostic
    initRepo repository
    base <- commit repository [("runtime.sh", encodeUtf8 $ broken <> "\n"), ("template.txt", encodeUtf8 $ T.replace "literal" "LITERAL" scratchContent)]
    canonicalPatch <- stagedPatch repository [("regression.sh", testSource)]
    let testPatch = shortHunks canonicalPatch
    writeFileBS scriptFile "test -f runtime.sh"
    writeFileLBS requestFile $ AE.encode $ AE.object ["projectId" AE..= testPid, "repository" AE..= repository, "base" AE..= base]
    prepared <- runEff (Workspace.runWorkspaceReal $ Workspace.runWorkspace (Workspace.Options store.cacheDir (toText image) bridge scriptFile Nothing Nothing) (Workspace.Prepare repository) requestFile) >>= either (fail . toString) (maybe (fail "Fix workspace missing") (pure . AE.toJSON))
    baseRevision <- either (fail . show) pure $ Agent.mkRevision base
    imageId <- parse (field "manifest" >=> field "identity" >=> field "imageBuildId") prepared
    fingerprint <- parse (field "manifest" >=> field "sha256") prepared :: IO Text
    let reproduction source = SdkTool "reproduce_issue" $ AE.object $ ["script" AE..= ("bash regression.sh" :: Text), "failureMessage" AE..= ("retry token was lost" :: Text)] <> source
        frozen = Agent.TestArtifact "bash regression.sh" canonicalPatch "retry token was lost"
        steps =
          [ propose
          , bash "rm -f runtime.sh; printf forbidden > runtime.sh"
          , write "runtime.sh" "forbidden"
          , edit "runtime.sh" "dropped" "forbidden"
          , reproduction ["testPatch" AE..= ("production edit is not a regression test" :: Text)]
          , bash "head -c 262145 /dev/zero > /tmp/oversized.patch; : > /tmp/empty.patch"
          ]
            <> map (\path -> reproduction ["testPatchFile" AE..= (path :: Text)]) ["/tmp/oversized.patch", "/tmp/empty.patch"]
            <> [ reproduction ["testPatch" AE..= testPatch, "testPatchFile" AE..= patchPath]
               , write patchPath testPatch
               , reproduction ["testFiles" AE..= V.singleton (AE.object ["path" AE..= ("regression.sh" :: Text), "content" AE..= decodeUtf8 @Text testSource])]
               , write "runtime.sh\0ignored" "forbidden"
               , write scratchPath scratchContent
               , edit (T.replace "~" "/home/review-agent" scratchPath) "literal" "LITERAL"
               , bash "cmp -- template.txt '/home/review-agent/notes/quote'\"'\"'s $(touch path-expanded).txt' && test ! -e path-expanded && test ! -e patch-path-expanded && test ! -e content-expanded && rm -r -- /home/review-agent/notes"
               , edit "runtime.sh" broken fixed
               , propose
               , verify
               , write "regression.sh" "exit 0\n"
               , submit
               , write "regression.sh" (decodeUtf8 testSource)
               , propose
               , verify
               , submit
               ]
        request = Agent.Request{projectId = testPid, runId = uid 1, attemptId = uid 2, repository = toText repository, base, head = base, gitToken = "", prompt = "Fix dropped retry tokens and retain a regression.", provider = "fix-fixture", model = "fixture", thinkingLevel = Agent.Off}
        fixRequest = Agent.FixRequest request (uid 3) imageId
        continuation = Agent.Retained (uid 200) $ pure $ Agent.FixContext request.attemptId request.prompt $ Agent.ReviewContext (Agent.ReviewInput baseRevision baseRevision request.provider "fixture" [] [] [] frozenTime frozenTime request.prompt) (Agent.ReviewMemory mempty [])
    plan <- newIORef steps
    replies <- newIORef ([] :: [AE.Value])
    phases <- newIORef ([] :: [Agent.PhaseReceipt])
    checks <- newIORef ([] :: [Agent.CheckReceipt])
    calls <- newIORef ([] :: [Agent.ModelCall])
    usage <- newIORef ([] :: [Agent.ModelUsage])
    allocations <- newIORef ([] :: [Agent.AllocationReceipt])
    validationFailure <- newIORef Nothing
    failProvider <- newIORef True
    interruptCandidate <- newIORef True
    interruptAdmission <- newIORef True
    interruptPeer <- newIORef True
    let record ref receipt = modifyIORef' ref (<> [receipt])
        next results = do
          writeIORef replies results
          choices <- readIORef plan
          pure $ fromMaybe (SdkText "Fix attempt finished.") $ viaNonEmpty head $ drop (length results) choices
        provider body results = do
          tools <- toolNames body
          if "submit_admission" `elem` tools
            then do
              stopAdmission <- readIORef interruptAdmission
              if stopAdmission && length results == 1
                then writeIORef interruptAdmission False $> SdkUnavailable "Provider interrupted after admission inspection"
                else pure $ case length results of
                  0 -> SdkResponse (bash "cat /run/monoscope-admission.json; test ! -w runtime.sh") $ SdkTokens 7 0
                  1 -> SdkResponse (SdkTool "submit_admission" $ AE.object ["tag" AE..= ("Admitted" :: Text), "contents" AE..= ("Relevant fixture regression admitted" :: Text)]) $ SdkTokens 7 0
                  _ -> SdkUnavailable "Provider failed after checked admission submission"
            else do
              let peer = "submit_fix_review" `elem` tools
              reply <- next results
              failNow <- readIORef failProvider
              captureNow <- readIORef interruptCandidate
              stopPeer <- readIORef interruptPeer
              if captureNow && not peer && length results == 17
                then writeIORef interruptCandidate False $> SdkUnavailable "Provider interrupted after captured candidate"
                else
                  if stopPeer && peer && length results == 1
                    then writeIORef interruptPeer False $> SdkUnavailable "Provider interrupted after independent inspection"
                    else
                      if (peer && length results == 2) || (failNow && length results == length steps)
                        then do
                          unless peer $ writeIORef failProvider False
                          pure $ SdkUnavailable "Provider failed after checked native submission"
                        else pure $ SdkResponse reply $ SdkTokens 7 0
    withSdkProvider directory "fix-fixture" provider \models requests -> do
      let run action = runEff $ sdkAgent store [models, image, diagnostic, "--prepare", scriptFile, "--cli", cli] action
          knowledge = const $ pure $ Left "Unexpected knowledge request"
          capture ref receipt = liftIO $ record ref receipt
          inspect completedTools result = runExceptT do
            evidence <- liftIO $ readIORef checks
            (baseline, candidate) <- case filter ((== result.checkId) . (.checkId)) evidence of
              [baseline, candidate] -> pure (baseline, candidate)
              _ -> throwE "Verified comparison is missing"
            let artifacts = Agent.ArtifactRequest (toText repository) result baseline candidate
            liftIO do
              let admissions = store.usageDir </> "fixes" </> toString result.runId.toText </> "checks" </> "admissions"
              entries <- listDirectory admissions
              length entries `shouldBe` 1
              for_ entries \entry -> readJSON (admissions </> entry </> "admission.json") `shouldReturn` AE.object ["projectId" AE..= result.projectId, "runId" AE..= result.runId, "issueId" AE..= result.issueId, "attemptId" AE..= result.attemptId, "base" AE..= result.base, "regressionSha256" AE..= result.regressionSha256, "rationale" AE..= ("Relevant fixture regression admitted" :: Text)]
            ended <- liftIO $ filter (\value -> AET.parseEither (field "attemptId") value == Right request.attemptId) <$> toolEnds
            liftIO do
              length ended `shouldBe` completedTools
              failed <- traverse (parse $ field "failed") ended
              when (fromMaybe False $ viaNonEmpty head $ drop 17 failed) do
                results <- readIORef replies
                expectationFailure $ "Restored candidate verification failed: " <> toString (jsonText [value | value <- results, AET.parseEither (field "tool_call_id") value == Right ("tool-17" :: Text)])
              [index | (index, True) <- zip [0 :: Int ..] failed] `shouldBe` [0, 1, 2, 3, 4, 6, 7, 8, 11, 19]
            beforeMedia <- liftIO $ length <$> requests
            verified <- ExceptT $ Agent.verifyFixArtifacts store artifacts
            explained <- ExceptT $ Agent.explainVerifiedFix store artifacts verified
            liftIO do
              Agent.validExplanation result (Agent.fixExplanation explained) `shouldBe` True
              length <$> requests `shouldReturn` beforeMedia
            let peer n decision = do
                  liftIO
                    $ writeIORef
                      plan
                      [ bash "cat runtime.sh regression.sh /run/monoscope-fix-review.json /run/monoscope-fix-review-base.log /run/monoscope-fix-review-head.log; printf forbidden > runtime.sh"
                      , SdkTool "submit_fix_review" $ AE.toJSON decision
                      ]
                  ExceptT $ Agent.reviewVerifiedFix (Agent.Retained (uid $ 200 + n) $ pure $ uid n) (Agent.FixReviewRequest fixRequest{Agent.request = request{Agent.attemptId = uid n}} artifacts) verified knowledge (capture calls) (capture usage) (capture allocations)
            changed <- peer 4 $ Agent.ReviewChanges $ one "runtime.sh:1 needs another caller check"
            liftIO $ case changed of Agent.ChangesNeeded values -> values `shouldBe` one "runtime.sh:1 needs another caller check"; _ -> expectationFailure "Peer changes were not retained"
            blocked <- peer 5 $ Agent.ReviewBlocked $ one "Required checks are unavailable"
            liftIO $ case blocked of Agent.ReviewUnavailable values -> values `shouldBe` one "Required checks are unavailable"; _ -> expectationFailure "Peer missing verification was not retained"
            approved <- peer 6 $ Agent.ReviewApproved "The retained test exercises the runtime and the fix preserves its argument."
            case approved of Agent.Reviewed reviewed -> pure reviewed; _ -> throwE "Peer approval was not established"
          validate completedTools result = do
            checked <- tryAny $ inspect completedTools result
            case checked of
              Left failure
                | isJust (fromException @Agent.NativePause failure) -> throwIO failure
                | isJust (fromException @AI.AgentAwaitingReview failure) -> throwIO failure
                | otherwise -> liftIO (writeIORef validationFailure $ Just failure) $> Left "SDK fixture validation failed"
              Right value -> pure value
          fixWith scope input admitFn phase = run . Agent.fixRepository scope input (const $ pure $ Left "Unexpected query") knowledge admitFn (capture calls) (capture usage) (capture allocations) (capture checks) phase
          resume input = fixWith continuation input admit (\receipt -> capture phases receipt $> Right ())
          admit artifact =
            if artifact /= frozen
              then pure $ Left "The test patch must only contain the actual regression"
              else do
                decision <- Agent.admitRegression (Agent.Retained (uid 200) $ pure $ uid 8) (Agent.AdmissionRequest fixRequest{Agent.request = request{Agent.attemptId = uid 8}} artifact) knowledge (capture calls) (capture usage) (capture allocations)
                pure $ decision >>= \case Agent.Admitted rationale -> Right $ Memory.memoryText rationale; Agent.Rejected rationale -> Left $ Memory.memoryText rationale
      for_ ([("admission", interruptAdmission, "Native child session interrupted; retry its retained request"), ("candidate", interruptCandidate, "Review session interrupted; retry its retained request"), ("peer", interruptPeer, "Native child session interrupted; retry its retained request")] :: [(Text, IORef Bool, Text)]) \(label, boundary, reason) -> do
        interrupted <- resume fixRequest (validate 23)
        isLeft interrupted `shouldBe` True
        observed <- readIORef boundary
        when observed $ readIORef validationFailure >>= traverse_ throwIO
        (label, observed, leftToMaybe interrupted) `shouldBe` (label, False, Just reason)
        doesFileExist (store.usageDir </> "fixes" </> toString request.runId.toText </> "sessions" </> toString (uid 200 :: UUIDId "ai_routine_run").toText </> "checkpoint.json") `shouldReturn` True
      outcome <- resume fixRequest (validate 23)
      readIORef validationFailure >>= traverse_ throwIO
      reviewed <-
        either
          ( \failure -> do
              transcript <- AE.encode <$> readIORef replies
              diagnostics <- readFileBS diagnostic
              fail $ toString failure <> toString (decodeUtf8 @Text diagnostics) <> toString (decodeUtf8 @Text transcript)
          )
          pure
          outcome
      let result = Agent.reviewedFixResult reviewed
      beforeReplay <- length <$> requests
      cached <- resume fixRequest (validate 24)
      readIORef validationFailure >>= traverse_ throwIO
      recovered <- either (fail . toString) pure cached
      Agent.reviewedFixResult recovered `shouldBe` result
      length <$> requests `shouldReturn` beforeReplay
      foreignResult <- resume fixRequest{Agent.request = request{Agent.attemptId = uid 99}} (validate 24)
      isLeft foreignResult `shouldBe` True
      length <$> requests `shouldReturn` beforeReplay
      result.base `shouldSatisfy` ((== base) . Fixes.revisionText)
      result.attemptId `shouldBe` request.attemptId
      result.rootCause `shouldBe` rootCause
      result.summary `shouldBe` "Preserve the retry token."
      fst (Agent.reviewedFixApproval reviewed) `shouldBe` uid 6
      recordedChecks <- readIORef checks
      map (.status) recordedChecks `shouldBe` [Agent.Failed, Agent.Failed, Agent.Passed, Agent.Failed, Agent.Passed]
      map (AE.toJSON . (.workspace)) recordedChecks `shouldBe` replicate 5 (AE.toJSON fingerprint)
      recordedPhases <- readIORef phases
      length recordedPhases `shouldBe` 8
      let proposed = [candidate.revision | Agent.PhaseReceipt{step = Fixes.ProposeCandidate candidate} <- recordedPhases]
      length proposed `shouldBe` 2
      viaNonEmpty last proposed `shouldBe` Just result.head
      map (.step) (take 1 recordedPhases) `shouldSatisfy` all (\case Fixes.RestartReproduction -> True; _ -> False)
      case viaNonEmpty last recordedPhases of
        Just Agent.PhaseReceipt{step = Fixes.Observe receipt} -> AE.toJSON receipt.regression `shouldBe` AE.toJSON (Just $ Agent.RetainedTest result.regressionSha256)
        _ -> expectationFailure "Final verification was not retained"
      let checksDir = store.usageDir </> "fixes" </> toString result.runId.toText </> "checks"
          candidateDir = checksDir </> "candidates" </> toString result.candidateId.toText
          bundle = candidateDir </> "candidate.bundle"
          ref = "refs/heads/monoscope-candidate/" <> toString result.candidateId.toText
      patch <- readFileBS $ candidateDir </> "fix.patch"
      patch `shouldSatisfy` BS.isInfixOf "regression.sh"
      patch `shouldSatisfy` BS.isInfixOf "runtime.sh"
      void $ gitIn repository ["bundle", "verify", bundle]
      gitText repository ["bundle", "list-heads", bundle] `shouldReturn` Fixes.revisionText result.head <> " " <> toText ref
      void $ gitIn repository ["fetch", "--quiet", bundle, ref]
      gitText repository ["rev-parse", "FETCH_HEAD"] `shouldReturn` Fixes.revisionText result.head
      results <- readIORef replies
      length results `shouldBe` 24
      let reply = (`toolReply` results)
      reply "tool-6" >>= (`shouldSatisfy` T.isInfixOf "256 KiB limit")
      reply "tool-13" >>= (`shouldSatisfy` T.isInfixOf "/home/review-agent/notes/")
      reply "tool-19" >>= (`shouldSatisfy` T.isInfixOf "Workspace changed; propose and verify a new candidate")
      proposal <- parse AE.parseJSON $ AE.object ["patch" AE..= decodeUtf8 @Text patch, "rootCause" AE..= rootCause]
      captured <- Native.captureCandidate checksDir fixRequest repository proposal >>= either (fail . toString) pure
      identifier <- parse (field "candidateId") $ AE.toJSON captured
      Native.candidateMatchesPatch captured identifier (decodeUtf8 patch) `shouldBe` True
      Native.candidateMatchesPatch captured (uid 99) (decodeUtf8 patch) `shouldBe` False
      snapshot <- either fail pure $ AE.eitherDecode $ AE.encode $ Native.candidateSnapshot captured
      restored <- Native.restoreCandidate store fixRequest repository snapshot >>= either (fail . toString) pure
      Native.candidateIdentity restored `shouldBe` Native.candidateIdentity captured
      let retainedCandidate = checksDir </> "candidates" </> toString (Native.candidateIdentity captured).toText
      for_ ["candidate.json", "fix.patch", "candidate.bundle"] \file -> do
        original <- readFileBS $ retainedCandidate </> file
        bracket_ (writeFileBS (retainedCandidate </> file) "corrupt retained candidate") (writeFileBS (retainedCandidate </> file) original) do
          isLeft <$> Native.restoreCandidate store fixRequest repository snapshot `shouldReturn` True
      jsonText results `shouldNotSatisfy` T.isInfixOf "/file-tools/"
      contexts <- requests
      toolsets <- traverse toolNames contexts
      let peers = filter (elem "submit_fix_review") toolsets
      length peers `shouldBe` 10
      length (filter (elem "submit_admission") toolsets) `shouldBe` 4
      length (filter (elem "submit_fix") toolsets) `shouldBe` 26
      for_ peers \tools -> do
        tools `shouldSatisfy` all (`notElem` ["edit", "write", "remember_knowledge", "recall_knowledge", "reproduce_issue", "verify_fix", "run_regression"])
        tools `shouldSatisfy` (\availableTools -> all (`elem` availableTools) ["list_project_memory", "read_project_memory", "write_project_memory"])
      recordedCalls <- readIORef calls
      recordedUsage <- readIORef usage
      let callAttempts = Map.fromList [(receipt.id, receipt.attemptId) | receipt <- recordedCalls]
      length recordedCalls `shouldBe` length contexts
      length recordedUsage `shouldBe` length contexts
      length (filter (not . (.succeeded)) recordedUsage) `shouldBe` 8
      recordedUsage `shouldSatisfy` all (\receipt -> (receipt.callId >>= (`Map.lookup` callAttempts)) == Just receipt.attemptId && (not receipt.succeeded || maybe False (> 0) receipt.costNanousd))
      recordedAllocations <- readIORef allocations
      length recordedAllocations `shouldBe` 10
      map Agent.allocationAttempt recordedAllocations `shouldMatchList` [request.attemptId, request.attemptId, request.attemptId, request.attemptId, uid 8, uid 8, uid 4, uid 4, uid 5, uid 6]
      for_ recordedAllocations \receipt -> do
        parse (field "outcome") (AE.toJSON receipt) `shouldReturn` ("released" :: Text)
        parse (field "durationMs") (AE.toJSON receipt) >>= (\duration -> (duration :: Word64) `shouldSatisfy` (> 0))
      ends <- toolEnds
      case viaNonEmpty last ends of
        Just value -> parse (field "failed") value `shouldReturn` False
        Nothing -> expectationFailure "Final SDK tool event missing"
      writeIORef plan [reproduction ["testPatch" AE..= testPatch]]
      beforeDenied <- length <$> requests
      denied <- fixWith Agent.Isolated fixRequest{Agent.request = request{Agent.runId = uid 10, Agent.attemptId = uid 11}} admit (const $ pure $ Left "Phase persistence rejected") (const $ pure $ Left "Unexpected validation")
      isLeft denied `shouldBe` True
      length <$> requests `shouldReturn` beforeDenied
      for_ (zip [30, 32] [("true", Agent.Passed), ("printf 'retry token was lost\\n'; exit 127", Agent.Blocked)]) \(number, (script, expectedStatus)) -> do
        offset <- length <$> toolEnds
        writeIORef plan [SdkTool "reproduce_issue" $ AE.object ["script" AE..= (script :: Text), "testPatch" AE..= testPatch, "failureMessage" AE..= frozen.failureMessage], write "runtime.sh" "forbidden"]
        let configured = fixRequest{Agent.request = request{Agent.runId = uid number, Agent.attemptId = uid (number + 1)}}
        failed <- fixWith Agent.Isolated configured (const $ pure $ Right "Deliberately admit this artifact to exercise the native reproduction guard") (const $ pure $ Right ()) (const $ pure $ Left "Unreproduced source reached validation")
        isLeft failed `shouldBe` True
        ended <- drop offset <$> toolEnds
        traverse (parse $ field "failed") ended `shouldReturn` [True, True]
        map (.status) . filter ((== configured.request.attemptId) . (.attemptId)) <$> readIORef checks `shouldReturn` [expectedStatus]
      ready <- MVar.newEmptyMVar
      blockedAdmission <- MVar.newEmptyMVar :: IO (MVar.MVar ())
      beforeCancel <- length <$> readIORef checks
      let cancelled = fixWith Agent.Isolated fixRequest{Agent.request = request{Agent.runId = uid 20, Agent.attemptId = uid 21}} (\_ -> liftIO (MVar.putMVar ready () >> MVar.takeMVar blockedAdmission) $> Right "Admission finished after cancellation") (const $ pure $ Right ()) (const $ pure $ Left "Unexpected validation")
      withAsync cancelled \worker -> do
        Timeout.timeout (30 * 1000000) (MVar.takeMVar ready) `shouldReturn` Just ()
        cancel worker
        isLeft <$> waitCatch worker `shouldReturn` True
      length <$> readIORef checks `shouldReturn` beforeCancel

  it "calibrates pinned source with protected gitlinks, isolated scratch clones and durable evidence" \_ -> CLI.withCliBin \cli -> withTemp "native-sdk-calibration" \directory -> do
    profile <- nativeTestImage
    store <- storeIn directory
    bridge <- canonicalizePath "test/fixtures/native-sandbox.sh"
    let repository = directory </> "repository"
        outputDir = directory </> "results"
        caseFile = directory </> "case.json"
        sdkTemp = directory </> "tmp"
        requestFile = directory </> "request.json"
        scriptFile = directory </> "prepare.sh"
        finding file start end = ["file" AE..= (file :: Text), "start_line" AE..= (start :: Int), "end_line" AE..= (end :: Int), "message" AE..= ("retry_token drops its argument, so retries lose the caller token." :: Text)]
        draft file = ranged file 1
        ranged file start end = AE.object ["findings" AE..= V.singleton (AE.object $ finding file start end)]
        regression = "set -eu\ntest \"$(cat .prepared-proof)\" = dependencies-ready\n. ./runtime.sh\nif [ \"$(retry_token original)\" != original ]; then echo 'retry token lost' >&2; exit 1; fi\n" :: Text
        check message = SdkTool "run_regression" $ AE.object ["script" AE..= regression, "failureMessage" AE..= (message :: Text)]
    traverse_ (createDirectoryIfMissing True) [outputDir, sdkTemp]
    initRepo repository
    initial <- commit repository [("AGENTS.md", "Read the implementation before reporting.\n"), ("runtime.sh", "retry_token() { printf '%s\\n' \"$1\"; }\n")]
    createDirectoryIfMissing True $ repository </> "vendor"
    void $ gitIn repository ["update-index", "--add", "--cacheinfo", "160000," <> toString initial <> ",vendor"]
    void $ gitIn repository ["commit", "--quiet", "-m", "retain uninitialized submodule"]
    base <- gitText repository ["rev-parse", "HEAD"]
    writeFileBS (repository </> "runtime.sh") "retry_token() { printf 'dropped\\n'; }\n"
    void $ gitIn repository ["commit", "--quiet", "-am", "head"]
    headRevision <- gitText repository ["rev-parse", "HEAD"]
    writeFileBS (repository </> "golden-private.txt") "Private reference must not enter the agent workspace.\n"
    let metadata = AE.object ["repo" AE..= ("https://github.com/fixture/retry" :: Text), "pr_number" AE..= (17 :: Int), "title" AE..= ("Retry token handling" :: Text), "body" AE..= ("Preserve the caller token." :: Text)]
    writeFileLBS caseFile $ AE.encode $ AE.object ["projectId" AE..= testPid, "repository" AE..= repository, "base" AE..= base, "head" AE..= headRevision, "provider" AE..= ("fixture" :: Text), "model" AE..= ("fixture" :: Text), "pullRequest" AE..= metadata, "rubric" AE..= ("Private grading stays outside provider requests." :: Text)]
    plan <- newIORef [SdkText $ jsonText $ draft "runtime.sh" 1]
    replies <- newIORef ([] :: [AE.Value])
    let next results = do
          writeIORef replies results
          choices <- readIORef plan
          pure $ fromMaybe (SdkText "Review submitted.") $ viaNonEmpty head $ drop (length results) choices
    withSdkModel directory "fixture" next \models requests -> do
      let run mode name extra =
            runEff
              $ sdkAgent store ([models, profile, directory </> "worker.log", "--cli", cli, "--temp", sdkTemp] <> extra)
              $ Evaluation.runEvaluationReal
              $ Evaluation.evaluateSource (mode :| []) caseFile (outputDir </> name)
          result mode name extra = do
            outcome <- Timeout.timeout (90 * 1000000) $ run mode name extra
            report <- maybe (fail "Native source evaluation exceeded its test deadline") (either (fail . show) pure) outcome
            report.contract `shouldBe` "native-source-evaluation/v1"
            readJSON (outputDir </> name) `shouldReturn` AE.toJSON report
            listDirectory sdkTemp `shouldReturn` []
            case report.runs of [row] -> pure row; _ -> fail "Expected one source-review run"
          accepted row = case row.outcome of
            Evaluation.Completed elapsed value -> do
              elapsed `shouldSatisfy` (> 0)
              AE.toJSON value `shouldBe` draft "runtime.sh" 1
            _ -> expectationFailure $ toString $ "Source review did not complete: " <> jsonText (AE.toJSON row)
      bounded <- result Agent.BoundedPatch "bounded.json" []
      accepted bounded
      map (.validation) bounded.submissions `shouldBe` [Right ()]
      traverse (parse (field "candidate" >=> field "tag") . AE.toJSON) bounded.submissions `shouldReturn` (["ProviderText"] :: [Text])
      length bounded.calls `shouldBe` 1
      map (.callId) bounded.usage `shouldBe` map (Just . (.id)) bounded.calls
      bounded.allocations `shouldSatisfy` null
      for_ (zip [0 :: Int ..] [(draft "../runtime.sh" 1, "repository-relative"), (ranged "runtime.sh" 2 1, "ordered"), (draft "absent.sh" 1, "cannot be read")]) \(index, (candidate, rejection)) -> do
        writeIORef plan [SdkText $ jsonText candidate]
        rejected <- result Agent.BoundedPatch ("invalid-" <> show index <> ".json") []
        case rejected.outcome of Evaluation.Failed _ _ -> pass; _ -> expectationFailure "Invalid source range was accepted"
        mapMaybe (leftToMaybe . (.validation)) rejected.submissions `shouldSatisfy` any (T.isInfixOf rejection)
      writeFileBS scriptFile "printf dependencies-ready > .prepared-proof"
      writeFileLBS requestFile $ AE.encode $ AE.object ["projectId" AE..= testPid, "repository" AE..= repository, "base" AE..= base]
      checkpoint <- runEff (Workspace.runWorkspaceReal $ Workspace.runWorkspace (Workspace.Options store.cacheDir (toText profile) bridge scriptFile Nothing Nothing) (Workspace.Prepare repository) requestFile) >>= either (fail . toString) (maybe (fail "Calibration preparation missing") (pure . AE.toJSON))
      fingerprint <- parse (field "manifest" >=> field "sha256") checkpoint :: IO Text
      writeIORef
        plan
        [ SdkTool "bash"
            $ AE.object
              [ "command"
                  AE..= unlines
                    [ "set -eu"
                    , "test ! -e golden-private.txt"
                    , "test ! -w runtime.sh"
                    , "test \"$(cat .prepared-proof)\" = dependencies-ready"
                    , "git show " <> base <> ":runtime.sh"
                    , "cat runtime.sh"
                    , "before=$(sha256sum runtime.sh)"
                    , "scratch=$(mktemp -d /tmp/monoscope-source-preflight.XXXXXX)"
                    , "trap 'rm -rf \"$scratch\"' EXIT"
                    , "git clone --quiet --no-local -- /workspace \"$scratch/checkout\""
                    , "cd \"$scratch/checkout\""
                    , "test ! -e .git/objects/info/alternates"
                    , "test -w runtime.sh"
                    , "printf scratch-only > runtime.sh"
                    , "printf scratch-only > check.txt"
                    , "cd /workspace"
                    , "test ! -w runtime.sh"
                    , "test ! -e check.txt"
                    , "test \"$(sha256sum runtime.sh)\" = \"$before\""
                    , "echo scratch-checkout-isolated"
                    ]
              ]
        , SdkTool "submit_review" $ draft "runtime.sh" 200
        , SdkTool "submit_review" $ draft "runtime.sh" 1
        , SdkTool "run_regression" $ AE.object ["script" AE..= regression, "failureMessage" AE..= ("retry token lost" :: Text), "testPatch" AE..= ("not a patch" :: Text)]
        , check "The runtime loses its caller token"
        , SdkTool "run_regression" $ AE.object ["script" AE..= ("bash check-regression.sh" :: Text), "failureMessage" AE..= ("retry token lost" :: Text), "testFiles" AE..= V.singleton (AE.object ["path" AE..= ("check-regression.sh" :: Text), "content" AE..= regression])]
        , SdkTool "submit_review" $ draft "runtime.sh" 1
        ]
      callsBefore <- length <$> requests
      harness <- result Agent.CheckoutHarness "harness.json" ["--prepare", scriptFile]
      accepted harness
      map (isRight . (.validation)) harness.submissions `shouldBe` [False, True, True]
      traverse (parse (field "candidate" >=> field "tag") . AE.toJSON) harness.submissions `shouldReturn` replicate 3 ("Structured" :: Text)
      map (.status) harness.checks `shouldBe` [Agent.Blocked, Agent.Blocked, Agent.Passed, Agent.Blocked, Agent.Passed, Agent.Failed]
      map (AE.toJSON . (.workspace)) harness.checks `shouldBe` replicate 6 (AE.toJSON fingerprint)
      map (.attemptId) harness.checks `shouldBe` replicate 6 harness.attemptId
      length harness.usage `shouldBe` 8
      length <$> requests `shouldReturn` callsBefore + length harness.usage
      harnessRequests <- drop callsBefore <$> requests
      for_ harnessRequests $ toolNames >=> (`shouldSatisfy` all (`notElem` ["edit", "write"]))
      harness.usage `shouldSatisfy` all (\receipt -> receipt.attemptId == harness.attemptId && receipt.succeeded && maybe False (> 0) receipt.costNanousd && receipt.callId `elem` map (Just . (.id)) harness.calls)
      map Agent.allocationAttempt harness.allocations `shouldBe` [harness.attemptId]
      for_ harness.allocations \receipt -> parse (field "outcome") (AE.toJSON receipt) `shouldReturn` ("released" :: Text)
      results <- readIORef replies
      let reply = (`toolReply` results)
      reply "tool-0" >>= (`shouldSatisfy` T.isInfixOf "dropped")
      reply "tool-0" >>= (`shouldSatisfy` T.isInfixOf "scratch-checkout-isolated")
      reply "tool-1" >>= (`shouldSatisfy` T.isInfixOf "outside the file")
      reply "tool-3" >>= (`shouldSatisfy` T.isInfixOf "Git diff")
      mismatched <- reply "tool-4" >>= either fail pure . AE.eitherDecodeStrict @AE.Value . encodeUtf8
      reason <- parse (field "results") mismatched >>= maybe (fail "Missing counterfactual result") pure . find (\value -> AET.parseEither (field "side") value == Right ("head" :: Text)) >>= parse (field "reason")
      reason `shouldContainAll` ["case-sensitive literal", "The runtime loses its caller token", "full log"]
      readFileBS (repository </> "runtime.sh") `shouldReturn` "retry_token() { printf 'dropped\\n'; }\n"
      let reserved = outputDir </> "reserved.json"
      writeFileBS reserved "prior metadata\n"
      count <- length <$> requests
      existing <- run Agent.BoundedPatch "reserved.json" []
      leftToMaybe existing `shouldBe` Just Evaluation.OutputUnavailable
      readFileBS reserved `shouldReturn` "prior metadata\n"
      length <$> requests `shouldReturn` count
      missing <- result Agent.CheckoutHarness "unprepared.json" ["--dependencies", directory </> "dependencies.tar"]
      missing.calls `shouldSatisfy` null
      case missing.outcome of Evaluation.Failed _ _ -> pass; _ -> expectationFailure "Dependency archive without preparation was accepted"
      length <$> requests `shouldReturn` count
      observed <- jsonText <$> requests
      for_ @[] ["Private reference must", "Private grading", "query_telemetry", "recall_knowledge"] \private -> observed `shouldNotSatisfy` T.isInfixOf private

  it "preserves screenshot images through pinned SDK read tools, with skills, memory, bounded evidence and isolated cancellation" \_ -> CLI.withCliBin \cli -> withTemp "native-sdk-exploration" \directory -> do
    image <- guestImage
    store <- storeIn directory
    let repository = directory </> "repository"
        skill = directory </> "review-skill"
        hostSkill = directory </> "host-only-skill"
        sdkTemp = directory </> "tmp"
        diagnostic = directory </> "worker.log"
        uid n = UUIDId $ UUID.fromWords 57 0x4000 0x80000000 n
        document = AE.object ["title" AE..= ("Retry context" :: Text), "body" AE..= ("Recheck callers before changing retry behavior." :: Text)]
        page = "00000000-0000-4000-8000-000000000099" :: Text
        draft line keys = AE.object (graded <> ["findings" AE..= V.singleton (AE.object ["location" AE..= AE.object ["path" AE..= ("src/handler.txt" :: Text), "line" AE..= (line :: Word32), "side" AE..= ("head" :: Text)], "mechanism" AE..= ("Retry metadata is removed before calling the helper." :: Text), "nextStep" AE..= ("Check retry behavior." :: Text), "evidenceKeys" AE..= (keys :: [Text])]), "coverage" AE..= ([] :: [Text])])
        accepted = draft 1 ["telemetry-1"]
        regression = "set -eu\ntest \"$(id -u)\" = 1000\ntest ! -w \"$0\"\n. ./src/runtime.sh\nif [ \"$(retry_token original)\" != original ]; then echo 'retry token was lost' >&2; exit 1; fi\necho 'retry token preserved'\n" :: Text
        tool name args = const $ pure $ SdkTool name args
        readPath path = tool "read" $ AE.object ["path" AE..= (path :: Text)]
        shell = const . pure . bash
        check script = tool "run_regression" $ AE.object ["script" AE..= (script :: Text), "failureMessage" AE..= ("retry token was lost" :: Text)]
        fullOutput results = do
          text <- toolReply "tool-5" results
          let path = T.takeWhile (\c -> c /= ']' && c /= '\n') $ T.drop (T.length "Full output: ") $ snd $ T.breakOn "Full output: " text
          unless ("/run/monoscope-review-output/" `T.isPrefixOf` path && not ("'" `T.isInfixOf` path)) $ fail "SDK did not retain a readable large-output path"
          pure path
    traverse_ (createDirectoryIfMissing True) [skill, hostSkill, sdkTemp]
    writeFileBS (skill </> "SKILL.md") "---\nname: retry-review\ndescription: Inspect retry metadata in a production review.\n---\nFollow callers before proposing a retry change.\n"
    writeFileBS (hostSkill </> "SKILL.md") "---\nname: host-only-skill\ndescription: This host skill must never be discovered.\n---\nprivate-host-skill-body\n"
    initRepo repository
    writeFileBS (repository </> "AGENTS.md") "Repository review rule: never remove retry metadata.\n"
    for_ ([(".agents/skills/repo-review", "repo-review"), (".pi/skills/pi-review", "pi-review")] :: [(FilePath, Text)]) \(path, name) -> do
      createDirectoryIfMissing True $ repository </> path
      writeFileBS (repository </> path </> "SKILL.md") $ encodeUtf8 $ "---\nname: " <> (name :: Text) <> "\ndescription: Check repository-specific review behavior.\n---\nRepository skill content.\n"
    createFileLink hostSkill (repository </> ".agents/skills/host-link")
    createDirectoryIfMissing True $ repository </> ".agents/skills/file-link"
    createFileLink (hostSkill </> "SKILL.md") (repository </> ".agents/skills/file-link/SKILL.md")
    imageBytes <- readFileBS "test/fixtures/review-image.png"
    base <- commit repository [("review image.txt", imageBytes), ("src/helper.txt", "preserve-retry-metadata\n"), ("src/handler.txt", "calls helper with retry metadata\n"), ("src/runtime.sh", "retry_token() { printf '%s\\n' \"$1\"; }\n")]
    headRevision <- commit repository [("src/handler.txt", "calls helper without retry metadata\n"), ("src/runtime.sh", "retry_token() { printf 'dropped\\n'; }\n")]
    void $ commit repository [("src/helper.txt", "unreviewed-later-change\n")]
    plan <- newIORef ([] :: [[AE.Value] -> IO SdkReply])
    replies <- newIORef ([] :: [AE.Value])
    proofs <- newIORef ([] :: [Proof.ProofMedia])
    ready <- MVar.newEmptyMVar
    blocked <- MVar.newEmptyMVar :: IO (MVar.MVar ())
    let prefix = [readPath "/skills/0/SKILL.md", readPath "/skills/1/systematic-debugging/SKILL.md", readPath "/skills/1/systematic-debugging/root-cause-tracing.md", readPath "/workspace/.agents/skills/repo-review/SKILL.md", shell $ "set -eu; test \"$(id -u)\" = 1000; test ! -w /usr/bin/git; echo supervisor-tools-protected; pwd; uname -s; git rev-parse HEAD; git show " <> base <> ":src/handler.txt; rg preserve-retry-metadata src; git diff " <> base <> "..." <> headRevision <> "; test -z \"$(printenv IMPACT_REVIEW_FIXTURE_SECRET || true)\"; echo host-credentials-absent; test \"$(cat src/handler.txt)\" = 'calls helper without retry metadata'; echo repository-check-passed; if (printf changed > /skills/0/SKILL.md) 2>/dev/null; then exit 1; fi; if (printf changed > /checkout/AGENTS.md) 2>/dev/null; then exit 1; fi; echo readonly-mounts-enforced; test -z \"$(git config --get-regexp extraheader || true)\"", shell "printf 'first-output-line\\n'; seq 1 2505", fmap (\path -> SdkTool "read" $ AE.object ["path" AE..= path, "limit" AE..= (2 :: Int)]) . fullOutput, fmap (\path -> SdkTool "bash" $ AE.object ["command" AE..= ("head -n 1 '" <> path <> "'")]) . fullOutput]
        fileTools =
          [ tool "write" $ AE.object ["path" AE..= ("tests/review-probe.sh" :: Text), "content" AE..= ("printf draft-probe\n" :: Text)]
          , tool "edit" $ AE.object ["path" AE..= ("tests/review-probe.sh" :: Text), "oldText" AE..= ("draft-probe" :: Text), "newText" AE..= ("retained-probe" :: Text)]
          , readPath "/workspace/tests/review-probe.sh"
          , shell "set -eu; test \"$(bash tests/review-probe.sh)\" = retained-probe; test ! -e /checkout/tests/review-probe.sh; echo guest-file-tools-isolated"
          , tool "write" $ AE.object ["path" AE..= ("/checkout/AGENTS.md" :: Text), "content" AE..= ("must not replace host instructions" :: Text)]
          ]
        reviewPlan searchTerm limitScript afterSubmit = prefix <> fileTools <> [tool "list_project_memory" $ AE.object ["query" AE..= value | value <- maybeToList searchTerm], tool "read_project_memory" $ AE.object ["pageId" AE..= page], tool "write_project_memory" $ AE.object ["pageId" AE..= page, "version" AE..= (0 :: Int), "document" AE..= document], tool "recall_knowledge" $ AE.object ["service" AE..= ("test-service" :: Text)], tool "remember_knowledge" $ AE.object ["service" AE..= ("test-service" :: Text), "summary" AE..= ("Recheck the retry hypothesis." :: Text), "evidenceKeys" AE..= (["telemetry-1"] :: [Text])], check regression, check "set -eu\ngit update-index --assume-unchanged src/runtime.sh\nprintf changed > src/runtime.sh\n", check limitScript, check "exit 0", tool "submit_review" $ draft 1 [], tool "submit_review" $ draft 2 ["telemetry-1"], tool "submit_review" accepted] <> afterSubmit
        next results = do
          writeIORef replies results
          step <- atomicModifyIORef' plan \case [] -> ([], const $ pure $ SdkText "Review submitted."); action : rest -> (rest, action)
          step results
    withSdkModel directory "exploration-fixture" next \models requests -> do
      let request = Agent.Request{projectId = testPid, runId = uid 1, attemptId = uid 2, repository = toText repository, base, head = headRevision, gitToken = "fixture-git-token", prompt = "Review telemetry-1.", provider = "exploration-fixture", model = "fixture", thinkingLevel = Agent.Off}
          run n steps = do
            writeIORef plan steps
            writeIORef replies []
            checks <- newIORef ([] :: [Agent.CheckReceipt])
            allocations <- newIORef ([] :: [Agent.AllocationReceipt])
            submissions <- newIORef ([] :: [AE.Value])
            memories <- newIORef ([] :: [AE.Value])
            let configured = request{Agent.runId = uid n} :: Agent.Request "pr_review"
            result <-
              runEff
                $ sdkAgent store [models, image, diagnostic, "--cli", cli, "--temp", sdkTemp, "--skill", skill]
                $ Agent.reviewRepository
                  Agent.Isolated
                  configured
                  (const $ liftIO $ MVar.putMVar ready () >> MVar.takeMVar blocked $> Left "Cancelled query")
                  ( \knowledge -> liftIO $ case knowledge of
                      Agent.ProjectMemory value -> modifyIORef' memories (<> [AE.toJSON value]) $> Right (AE.toJSON value)
                      Agent.Recall service -> pure $ if service == "test-service" then Right $ AE.toJSON $ V.singleton $ AE.object ["summary" AE..= ("Historical hypothesis; verify again." :: Text), "revision" AE..= base] else Left "Unexpected service"
                      Agent.Remember note -> pure $ if note.service == "test-service" && note.summary == "Recheck the retry hypothesis." && toList note.evidenceKeys == ["telemetry-1"] then Right $ AE.String "Note staged" else Left "Unexpected knowledge"
                      (Agent.OpenIssues; Agent.ServiceMonitors) -> pure $ Left "Unexpected project signal query"
                  )
                  (const pass)
                  (const pass)
                  (\receipt -> liftIO $ modifyIORef' allocations (<> [receipt]))
                  (\receipt -> liftIO $ modifyIORef' checks (<> [receipt]))
                  Agent.ProofHooks{save = \_ sides -> liftIO $ modifyIORef' proofs (<> toList sides) $> Right "proof-1", progress = const pass}
                  ( \draftValue -> liftIO do
                      modifyIORef' submissions (<> [draftValue])
                      pure $ if draftValue == accepted then Right () else Left "Finding does not reference a changed line"
                  )
            (,,,,,) result <$> readIORef checks <*> readIORef allocations <*> readIORef submissions <*> readIORef memories <*> readIORef replies
      for_ ([(1, "exec monoscope-unavailable-regression-command", []), (3, "yes regression-output", [readPath "/workspace/src/helper.txt"])] :: [(Word32, Text, [[AE.Value] -> IO SdkReply])]) \(n, limitScript, afterSubmit) -> do
        (result, checks, allocations, submissions, memories, results) <- run n $ reviewPlan ("retry" <$ guard (n /= 1) :: Maybe Text) limitScript afterSubmit
        if n == 1 then either (fail . toString) (\value -> either fail pure (AE.eitherDecodeStrict $ encodeUtf8 value) >>= (`shouldBe` accepted)) result else isLeft result `shouldBe` True
        map (.status) checks `shouldBe` [Agent.Passed, Agent.Failed, Agent.Invalidated, Agent.Invalidated] <> if n == 1 then [Agent.Blocked, Agent.Blocked] else [Agent.OutputLimit, Agent.OutputLimit]
        map (.exitCode) (take 2 checks) `shouldBe` [Just 0, Just 1]
        when (n == 1) $ map (.exitCode) (drop 4 checks) `shouldBe` [Just 127, Just 127]
        when (n /= 1) $ map (.logBytes) checks `shouldSatisfy` all (<= Workspace.checkLogLimit)
        submissions `shouldBe` [draft 2 ["telemetry-1"], accepted]
        memories `shouldBe` [if n == 1 then AE.object ["tag" AE..= ("List" :: Text)] else AE.object ["tag" AE..= ("Search" :: Text), "contents" AE..= ("retry" :: Text)], AE.object ["tag" AE..= ("Read" :: Text), "contents" AE..= page], AE.object ["tag" AE..= ("Write" :: Text), "contents" AE..= AE.object ["pageId" AE..= page, "version" AE..= (0 :: Int), "document" AE..= document]]]
        length allocations `shouldBe` 1
        for_ allocations \allocation -> do
          allocationReleased request.attemptId allocation
          sandboxSessions >>= (\sessions -> map (.id) sessions `shouldNotContain` [(Agent.allocationId allocation).toText])
          parse (field "cpus") (AE.toJSON allocation) `shouldReturn` (2 :: Word16)
          parse (field "memoryMiB") (AE.toJSON allocation) `shouldReturn` (1024 :: Word32)
        toolReply "tool-5" results >>= (`shouldNotSatisfy` T.isInfixOf "first-output-line")
        for_ ["tool-6", "tool-7"] \identifier -> toolReply identifier results >>= (`shouldSatisfy` T.isInfixOf "first-output-line")
        jsonText results `shouldContainAll` ["Linux", "supervisor-tools-protected", "host-credentials-absent", "repository-check-passed", "readonly-mounts-enforced", "preserve-retry-metadata", "Follow callers before proposing", "Repository skill content", "NO FIXES WITHOUT ROOT CAUSE INVESTIGATION FIRST", "Root Cause Tracing", "Historical hypothesis", headRevision]
        for_ ["host-only-fixture", "unreviewed-later-change", "fixture-git-token", "private-host-skill-body"] \text -> jsonText results `shouldNotSatisfy` T.isInfixOf text
        toolReply "tool-10" results >>= (`shouldSatisfy` T.isInfixOf "retained-probe")
        toolReply "tool-11" results >>= (`shouldSatisfy` T.isInfixOf "guest-file-tools-isolated")
        toolReply "tool-12" results >>= (`shouldNotSatisfy` T.isInfixOf "Successfully wrote")
        doesFileExist (repository </> "tests/review-probe.sh") `shouldReturn` False
        readFileBS (repository </> "AGENTS.md") `shouldReturn` "Repository review rule: never remove retry metadata.\n"
        toolReply "tool-21" results >>= (`shouldSatisfy` T.isInfixOf "budget")
        toolReply "tool-22" results >>= (`shouldSatisfy` T.isInfixOf "evidenceKeys")
        toolReply "tool-23" results >>= (`shouldSatisfy` T.isInfixOf "changed line")
        let archive = store.usageDir </> toString (uid n :: UUIDId "pr_review").toText </> "checks"
        reports <- listDirectory archive >>= traverse (\name -> (name,) <$> readJSON (archive </> name </> "report.json"))
        length reports `shouldBe` 3
        for_ reports \(name, report) -> do
          parse (field "status") report `shouldReturn` ("completed" :: Text)
          parse (field "projectId") report `shouldReturn` testPid
          parse (field "runId") report `shouldReturn` (uid n :: UUIDId "pr_review")
          rows <- parse (field "results") report :: IO [AE.Value]
          when (any (\row -> AET.parseEither (field "status") row == Right ("passed" :: Text)) rows) do
            jsonText rows `shouldSatisfy` T.isInfixOf "retry token was lost"
            readFileBS (archive </> name </> "script.sh") `shouldReturn` encodeUtf8 regression
            for_ rows \row -> do
              parse (field "sourceBefore") row >>= (\original -> parse (field "sourceAfter") row `shouldReturn` (original :: Text))
              parse (field "resourceCostStatus") row `shouldReturn` ("unpriced" :: Text)
        listDirectory sdkTemp `shouldReturn` []
      (timeoutResult, _, _, _, _, timeoutReplies) <-
        run
          7
          [ tool "bash" $ AE.object ["command" AE..= ("trap '' TERM; printf retained > timeout-retained.txt; sleep 60 & echo $! > timeout-child.pid; wait" :: Text), "timeout" AE..= (1 :: Int)]
          , readPath "/workspace/timeout-retained.txt"
          , shell "set -eu; test \"$(cat timeout-retained.txt)\" = retained; child=$(cat timeout-child.pid); if kill -0 \"$child\" 2>/dev/null; then test \"$(awk '{print $3}' /proc/\"$child\"/stat)\" = Z; fi; echo timeout-kept-workspace-and-killed-child"
          , tool "bash" $ AE.object ["command" AE..= ("exit 137" :: Text), "timeout" AE..= (1 :: Int)]
          , tool "submit_review" accepted
          ]
      isRight timeoutResult `shouldBe` True
      toolReply "tool-1" timeoutReplies `shouldReturn` "retained"
      toolReply "tool-2" timeoutReplies >>= (`shouldSatisfy` T.isInfixOf "timeout-kept-workspace-and-killed-child")
      toolReply "tool-0" timeoutReplies >>= (`shouldSatisfy` T.isInfixOf "timeout: sending signal KILL")
      toolReply "tool-3" timeoutReplies >>= (`shouldSatisfy` T.isInfixOf "Command exited with code 137")
      toolReply "tool-3" timeoutReplies >>= (`shouldNotSatisfy` T.isInfixOf "timeout: sending signal")
      doesFileExist (repository </> "timeout-retained.txt") `shouldReturn` False
      -- A proof replays the claim's command under asciinema on base and head, and the host
      -- renders each recording; the model only sees the recorded output.
      (proofResult, _, _, _, _, proofReplies) <- run 11 [tool "record_proof" $ AE.object ["claim" AE..= ("Keeps the retry token" :: Text), "highlight" AE..= ("== retry token" :: Text), "command" AE..= ("set -eu\n. ./src/runtime.sh\necho \"== retry token: $(retry_token original)\"\ntest \"$(retry_token original)\" = original" :: Text)], tool "submit_review" accepted]
      isRight proofResult `shouldBe` True
      -- The preview is the highlighted clip; the reply tells the agent where it sits in the recording.
      toolReply "tool-0" proofReplies >>= (`shouldContainAll` ["proof-1", "== retry token: original", "== retry token: dropped", "clip", "total"])
      recorded <- readIORef proofs
      map (\proof -> (proof.side, proof.exitCode)) recorded `shouldBe` [(Agent.Base, Just 0), (Agent.Head, Just 1)]
      for_ recorded \proof -> do
        BS.take 6 proof.gif `shouldBe` "GIF89a"
        BS.length proof.mp4 `shouldSatisfy` (> 0)
        decodeUtf8 proof.cast `shouldSatisfy` T.isInfixOf "== retry token"
      -- The unrecorded setup may seed and start services, never change the source being proved.
      (_, _, _, _, _, tamperedReplies) <- run 12 [tool "record_proof" $ AE.object ["claim" AE..= ("Keeps the retry token" :: Text), "setup" AE..= ("echo 'retry_token() { echo original; }' >> src/runtime.sh" :: Text), "command" AE..= ("test \"$(. ./src/runtime.sh; retry_token original)\" = original" :: Text)], tool "submit_review" accepted]
      toolReply "tool-0" tamperedReplies >>= (`shouldSatisfy` T.isInfixOf "setup changed the commit or tracked files")
      beforeImage <- length <$> requests
      (imageResult, _, _, _, _, _) <- run 9 [readPath "/workspace/review image.txt", tool "submit_review" accepted]
      isRight imageResult `shouldBe` True
      imageRequests <- drop beforeImage <$> requests
      imageMessages <- concat <$> traverse (parse $ field "messages") imageRequests :: IO [AE.Value]
      let imageParts = filter (\part -> AET.parseMaybe (field "type") part == Just ("image_url" :: Text)) $ concatMap (fromMaybe [] . AET.parseMaybe (field "content")) imageMessages
          imageUrls = mapMaybe (AET.parseMaybe $ field "image_url" >=> field "url") imageParts
      imageUrls `shouldContain` ["data:image/png;base64," <> extractBase64 (B64.encodeBase64 imageBytes)]
      observed <- requests
      for_ observed \body -> do
        names <- toolNames body
        for_ ["read", "bash", "submit_review", "run_regression", "recall_knowledge", "remember_knowledge", "edit", "write"] \name -> names `shouldContain` [name]
        jsonText body `shouldContainAll` ["never remove retry metadata", "retry-review", "repo-review", "pi-review", "systematic-debugging", "test-driven-development", "verification-before-completion"]
        messages <- parse (field "messages") body :: IO [AE.Value]
        let systemMessages = filter (\message -> AET.parseMaybe (field "role") message == Just ("system" :: Text)) messages
        jsonText systemMessages `shouldContainAll` ["Use read to examine files", "Use edit for targeted changes", "Use write only for new files or complete rewrites."]
        for_ ["host-only-skill", "private-host-skill-body", "fixture-git-token"] \text -> jsonText body `shouldNotSatisfy` T.isInfixOf text
      let cancelPlan = prefix <> [tool "query_telemetry" $ AE.object ["service" AE..= ("test-service" :: Text), "query" AE..= ("| summarize count()" :: Text)]]
      baselineSessions <- map (.id) <$> sandboxSessions
      withAsync (run 5 cancelPlan) \worker -> do
        Timeout.timeout (30 * 1000000) (MVar.takeMVar ready) `shouldReturn` Just ()
        active <- filter (\session -> session.id `notElem` baselineSessions) <$> sandboxSessions
        length active `shouldBe` 1
        listDirectory sdkTemp >>= (`shouldSatisfy` (not . null))
        cancel worker
        cancelled <- waitCatch worker
        isLeft cancelled `shouldBe` True
        sandboxSessions >>= (\sessions -> map (.id) sessions `shouldSatisfy` all (`notElem` map (.id) active))
        listDirectory sdkTemp `shouldReturn` []
        for_ active \session -> do
          retained <- readJSON $ store.usageDir </> toString (uid 5 :: UUIDId "pr_review").toText </> toString session.id <> ".json"
          parse (field "tag") retained `shouldReturn` ("Allocation" :: Text)
          allocation <- parse (field "contents") retained :: IO Agent.AllocationReceipt
          (Agent.allocationId allocation).toText `shouldBe` session.id
          Agent.allocationAttempt allocation `shouldBe` request.attemptId
          parse (field "outcome") (AE.toJSON allocation) `shouldReturn` ("released" :: Text)

  it "evaluates actual SDK reviews with compaction receipts and explicit schema, citation and provider failures" \_ -> CLI.withCliBin \cli -> withTemp "native-sdk-evaluation" \directory -> do
    image <- guestImage
    store <- storeIn directory
    let repository = directory </> "repository"
        sdkTemp = directory </> "tmp"
        caseFile = directory </> "case.json"
        uid n = UUIDId $ UUID.fromWords 56 0x4000 0x80000000 n
        draft line keys = AE.object (graded <> ["findings" AE..= V.singleton (AE.object ["location" AE..= AE.object ["path" AE..= ("runtime.sh" :: Text), "line" AE..= (line :: Word32), "side" AE..= ("head" :: Text)], "mechanism" AE..= ("Retry metadata is removed before calling the helper." :: Text), "nextStep" AE..= ("Check retry behavior." :: Text), "evidenceKeys" AE..= (keys :: [Text])]), "coverage" AE..= ([] :: [Text])])
        accepted = draft 1 ["telemetry-1"]
        summarizing = T.isInfixOf "You are a context summarization assistant" . jsonText
    createDirectoryIfMissing True sdkTemp
    initRepo repository
    base <- commit repository [("runtime.sh", "retry_token() { printf '%s\\n' \"$1\"; }\n")]
    headRevision <- commit repository [("runtime.sh", "retry_token() { printf 'dropped\\n'; }\n")]
    baseline <- newIORef $ jsonText accepted
    unavailable <- newIORef False
    native <- newIORef False
    let next body results = do
          failed <- readIORef unavailable
          supervised <- readIORef native
          tools <- parse (AE.withObject "SDK evaluation request" (\object -> object AE..:? "tools" AE..!= [])) body :: IO [AE.Value]
          let reviewing = any (T.isInfixOf "submit_review" . jsonText) tools
              supervisedReply = case length results of
                0 -> SdkTool "query_telemetry" $ AE.object ["service" AE..= ("test-service" :: Text), "query" AE..= ("| summarize count()" :: Text)]
                1 -> SdkTool "run_regression" $ AE.object ["script" AE..= ("set -eu\n. ./runtime.sh\n[ \"$(retry_token original)\" = original ] || { echo 'retry token lost'; exit 1; }\n" :: Text), "failureMessage" AE..= ("retry token lost" :: Text)]
                2 -> SdkTool "recall_knowledge" $ AE.object ["service" AE..= ("test-service" :: Text)]
                3 -> SdkTool "remember_knowledge" $ AE.object ["service" AE..= ("test-service" :: Text), "summary" AE..= ("Retry behavior needs verification." :: Text), "evidenceKeys" AE..= (["query-1"] :: [Text])]
                4 -> SdkTool "submit_review" accepted
                _ -> SdkText $ "Review submitted. " <> T.replicate 5000 "Retained review context. "
          if failed
            then pure $ SdkUnavailable "fixture-insufficient-credit"
            else
              if summarizing body
                then pure $ SdkResponse (SdkText "The review was accepted; preserve its evidence and missing verification.") (SdkTokens 777 2)
                else
                  if supervised
                    then pure $ SdkResponse supervisedReply (SdkTokens (if length results >= 5 then 56000 else 7) 2)
                    else
                      if reviewing
                        then
                          pure
                            $ if null results
                              then SdkResponse (SdkTool "submit_review" accepted) (SdkTokens 7 2)
                              else SdkResponse (SdkText $ "Review submitted. " <> T.replicate 5000 "Retained review context. ") (SdkTokens 56000 2)
                        else do
                          response <- atomicModifyIORef' baseline (jsonText accepted,)
                          pure $ SdkResponse (SdkText response) (SdkTokens 7 2)
    withSdkProvider directory "evaluation-fixture" next \models requests -> do
      let request = Agent.Request{projectId = testPid, runId = uid 1, attemptId = uid 2, repository = toText repository, base, head = headRevision, gitToken = "", prompt = "Review telemetry-1.", provider = "evaluation-fixture", model = "fixture", thinkingLevel = Agent.Off}
      writeFileLBS caseFile $ AE.encode $ AE.object ["projectId" AE..= testPid, "repository" AE..= repository, "base" AE..= base, "head" AE..= headRevision, "provider" AE..= request.provider, "model" AE..= request.model, "evidenceKind" AE..= ("synthetic" :: Text), "evidence" AE..= V.fromList [AE.object ["key" AE..= ("telemetry-1" :: Text), "kind" AE..= ("telemetry" :: Text), "description" AE..= ("synthetic-evaluation-evidence" :: Text)], AE.object ["key" AE..= ("monitor-1" :: Text), "kind" AE..= ("monitor" :: Text)]], "rubric" AE..= (["private-grading-not-model"] :: [Text])]
      let both = Agent.BoundedPatch :| [Agent.CheckoutHarness]
          evaluateAt modes output =
            runEff
              $ sdkAgent store [models, image, directory </> "evaluation-worker.log", "--cli", cli, "--temp", sdkTemp]
              $ Evaluation.runEvaluationReal
              $ Evaluation.evaluate modes caseFile output
          evaluateCase modes name = do
            let output = directory </> name <> ".json"
            completed <- Timeout.timeout (90 * 1000000) $ evaluateAt modes output
            report <- maybe (fail "Evaluation exceeded its test deadline") (either (fail . show) pure) completed
            readJSON output `shouldReturn` AE.toJSON report
            report.contract `shouldBe` "native-impact-evaluation/v1"
            AE.toJSON report.assessment `shouldBe` AE.String "manual_required"
            nativeSource <- Native.sha256 <$> readFileBS "src/Data/Effectful/ReviewEvaluation.hs"
            Map.lookup "src/Data/Effectful/ReviewEvaluation.hs" report.harness `shouldBe` Just nativeSource
            listDirectory sdkTemp `shouldReturn` []
            pure report.runs
          completedDraft row = case row.outcome of Evaluation.Completed elapsed value -> (elapsed `shouldSatisfy` (> 0)) $> AE.toJSON value; _ -> expectationFailure (toString $ "Evaluation did not complete: " <> jsonText (AE.toJSON row)) $> AE.Null
      rows <- evaluateCase both "accepted"
      map (AE.toJSON . (.mode)) rows `shouldBe` [AE.String "bounded_patch", AE.String "checkout_harness"]
      traverse completedDraft rows `shouldReturn` [accepted, accepted]
      savedOutput <- readFileBS $ directory </> "accepted.json"
      beforeDuplicate <- length <$> requests
      duplicate <- evaluateAt both $ directory </> "accepted.json"
      leftToMaybe duplicate `shouldBe` Just Evaluation.OutputUnavailable
      readFileBS (directory </> "accepted.json") `shouldReturn` savedOutput
      length <$> requests `shouldReturn` beforeDuplicate
      for_ (zip [0 :: Int ..] rows) \(index, row) -> do
        length row.usage `shouldBe` if index == 0 then 1 else 3
        length (ordNub $ map (.id) row.usage) `shouldBe` length row.usage
        row.usage `shouldSatisfy` all (\receipt -> receipt.attemptId == row.attemptId && receipt.provider == request.provider && receipt.succeeded && receipt.callId `elem` map (Just . (.id)) row.calls)
        length (filter (\receipt -> receipt.inputTokens == 775 && receipt.cacheReadTokens == 2 && receipt.costNanousd == Just 1582000) row.usage) `shouldBe` index
        let ordinary = filter (\receipt -> receipt.inputTokens == 5) row.usage
        length ordinary `shouldBe` 1
        for_ ordinary \receipt -> receipt.costNanousd `shouldBe` Just 42000
        length row.allocations `shouldBe` index
        for_ row.allocations \allocation -> do
          Agent.allocationAttempt allocation `shouldBe` row.attemptId
          parse (field "outcome") (AE.toJSON allocation) `shouldReturn` ("released" :: Text)
        map (.validation) row.submissions `shouldBe` [Right ()]
      recorded <- requests
      length (filter summarizing recorded) `shouldBe` 1
      case recorded of
        firstRequest : _ -> do
          messages <- parse (field "messages") firstRequest :: IO [AE.Value]
          system <- maybe (fail "Patch review has no system instructions") pure $ find (\message -> AET.parseMaybe (field "role") message == Just ("system" :: Text)) messages
          parse (field "content") system >>= (`shouldSatisfy` T.isInfixOf (decodeUtf8 @Text $ AE.encode $ Policy.toolSchema Policy.SubmitReview))
        [] -> expectationFailure "Patch review made no provider request"
      for_ recorded \body -> do
        jsonText body `shouldSatisfy` T.isInfixOf "synthetic-evaluation-evidence"
        jsonText body `shouldNotSatisfy` T.isInfixOf "private-grading-not-model"
      for_ [Agent.BoundedPatch, Agent.CheckoutHarness] \mode -> do
        beforeSingle <- length <$> requests
        selected <- evaluateCase (mode :| []) $ "only-" <> show mode
        map (.mode) selected `shouldBe` [mode]
        traverse completedDraft selected `shouldReturn` [accepted]
        for_ selected \row -> length row.allocations `shouldBe` if mode == Agent.BoundedPatch then 0 else 1
        observed <- drop beforeSingle <$> requests
        observed `shouldSatisfy` (not . null)
        for_ (filter (not . summarizing) observed) \body -> do
          tools <- parse (AE.withObject "Selected evaluation tools" (\object -> object AE..:? "tools" AE..!= [])) body :: IO [AE.Value]
          null tools `shouldBe` (mode == Agent.BoundedPatch)
      for_ ([("json", "not-json", "JSON"), ("schema", jsonText $ draft 1 [], "schema"), ("citation", jsonText $ draft 2 ["telemetry-1"], "changed line"), ("evidence", jsonText $ draft 1 ["invented"], "unavailable evidence"), ("context", jsonText $ draft 1 ["monitor-1"], "production context")] :: [(FilePath, Text, Text)]) \(name, rejected, message) -> do
        writeIORef baseline rejected
        rejectedRows <- evaluateCase both name
        case rejectedRows of
          [rejectedRow, recovered] -> do
            case rejectedRow.outcome of Evaluation.Failed _ _ -> pass; _ -> expectationFailure "Invalid baseline was accepted"
            mapMaybe (leftToMaybe . (.validation)) rejectedRow.submissions `shouldSatisfy` any (T.isInfixOf message)
            completedDraft recovered `shouldReturn` accepted
          _ -> expectationFailure "Evaluation discarded the comparison"
      writeIORef unavailable True
      failedRows <- evaluateCase both "provider-failed"
      length failedRows `shouldBe` 1
      for_ failedRows \row -> case row.outcome of
        Evaluation.Failed _ _ -> do
          row.calls `shouldSatisfy` (not . null)
          row.usage `shouldSatisfy` (not . any (.succeeded))
        _ -> expectationFailure "Unavailable provider produced a quality result"
      writeIORef native True
      writeIORef unavailable False
      let run n = do
            calls <- newIORef ([] :: [Agent.ModelCall])
            usage <- newIORef ([] :: [Agent.ModelUsage])
            allocations <- newIORef ([] :: [Agent.AllocationReceipt])
            checks <- newIORef ([] :: [Agent.CheckReceipt])
            queries <- newIORef ([] :: [Agent.TelemetryQuery])
            knowledge <- newIORef ([] :: [AE.Value])
            submissions <- newIORef ([] :: [AE.Value])
            let configured = request{Agent.runId = uid n} :: Agent.Request "pr_review"
                journal = store.usageDir </> toString configured.runId.toText
                retained identifier expected = readJSON (journal </> toString identifier <> ".json") >>= (`shouldBe` expected)
            result <-
              runEff
                $ sdkAgent store [models, image, directory </> "worker.log", "--cli", cli, "--temp", sdkTemp]
                $ Agent.reviewRepository
                  Agent.Isolated
                  configured
                  (\telemetryQuery -> liftIO $ modifyIORef' queries (<> [telemetryQuery]) $> Right (AE.object ["key" AE..= ("query-1" :: Text), "queryResult" AE..= AE.object ["kind" AE..= ("aggregated" :: Text), "headers" AE..= (["count"] :: [Text]), "dataset" AE..= ([[7]] :: [[Int]]), "limitReached" AE..= False, "fieldsTruncated" AE..= False]]))
                  ( \knowledgeQuery -> liftIO $ case knowledgeQuery of
                      Agent.Recall service -> modifyIORef' knowledge (<> [AE.object ["tag" AE..= ("Recall" :: Text), "contents" AE..= service]]) $> Right (AE.toJSON ([] :: [AE.Value]))
                      Agent.Remember note -> modifyIORef' knowledge (<> [AE.object ["tag" AE..= ("Remember" :: Text), "contents" AE..= note]]) $> Right (AE.String "Note staged")
                      Agent.ProjectMemory _ -> fail "Unexpected wiki query"
                      (Agent.OpenIssues; Agent.ServiceMonitors) -> fail "Unexpected project signal query"
                  )
                  (\receipt -> liftIO $ retained receipt.id.toText (AE.object ["tag" AE..= ("Call" :: Text), "contents" AE..= receipt]) >> modifyIORef' calls (<> [receipt]))
                  (\receipt -> liftIO $ retained receipt.id.toText (AE.toJSON receipt) >> modifyIORef' usage (<> [receipt]))
                  (\receipt -> liftIO $ retained (Agent.allocationId receipt).toText (AE.object ["tag" AE..= ("Allocation" :: Text), "contents" AE..= receipt]) >> modifyIORef' allocations (<> [receipt]))
                  (\receipt -> liftIO $ retained receipt.id.toText (AE.object ["tag" AE..= ("Check" :: Text), "contents" AE..= receipt]) >> modifyIORef' checks (<> [receipt]))
                  noProofs
                  (\draftValue -> liftIO $ modifyIORef' submissions (<> [draftValue]) $> Right ())
            listDirectory sdkTemp `shouldReturn` []
            queued <- filter (T.isSuffixOf ".json" . toText) <$> listDirectory journal
            queued `shouldBe` []
            (,,,,,,,) result <$> readIORef calls <*> readIORef usage <*> readIORef allocations <*> readIORef checks <*> readIORef queries <*> readIORef knowledge <*> readIORef submissions
      count <- length <$> requests
      (result, calls, receipts, allocations, checks, queries, knowledge, submissions) <- run 3
      either (fail . toString) (\draftValue -> either fail pure (AE.eitherDecodeStrict $ encodeUtf8 draftValue) >>= (`shouldBe` accepted)) result
      length calls `shouldBe` 7
      length receipts `shouldBe` 7
      length (ordNub $ map (.id) receipts) `shouldBe` 7
      receipts `shouldSatisfy` all (\receipt -> receipt.attemptId == request.attemptId && receipt.callId `elem` map (Just . (.id)) calls)
      length (filter (\receipt -> receipt.inputTokens == 775 && receipt.costNanousd == Just 1582000) receipts) `shouldBe` 1
      length . filter summarizing . drop count <$> requests `shouldReturn` 1
      map (.status) checks `shouldBe` [Agent.Passed, Agent.Failed]
      length (ordNub $ map (.checkId) checks) `shouldBe` 1
      length (ordNub $ map (.scriptSha256) checks) `shouldBe` 1
      map (\telemetryQuery -> (telemetryQuery.service, telemetryQuery.query)) queries `shouldBe` [("test-service", "| summarize count()")]
      knowledge `shouldBe` [AE.object ["tag" AE..= ("Recall" :: Text), "contents" AE..= ("test-service" :: Text)], AE.object ["tag" AE..= ("Remember" :: Text), "contents" AE..= AE.object ["service" AE..= ("test-service" :: Text), "summary" AE..= ("Retry behavior needs verification." :: Text), "evidenceKeys" AE..= (["query-1"] :: [Text])]]]
      submissions `shouldBe` [accepted]
      length allocations `shouldBe` 1
      writeIORef unavailable True
      (failed, failedCalls, failedUsage, failedAllocations, failedChecks, failedQueries, failedKnowledge, failedSubmissions) <- run 4
      isLeft failed `shouldBe` True
      failedCalls `shouldSatisfy` (not . null)
      length failedUsage `shouldBe` length failedCalls
      failedUsage `shouldSatisfy` all (\receipt -> not receipt.succeeded && isNothing receipt.costNanousd)
      length failedAllocations `shouldBe` 1
      length failedChecks `shouldBe` 0
      length failedQueries `shouldBe` 0
      failedKnowledge `shouldBe` []
      failedSubmissions `shouldBe` []
      traverse_ (allocationReleased request.attemptId) (allocations <> failedAllocations)

  it "review interpreters keep validation and transport failures apart and preserve a retained operation's suspension" \_ -> do
    for_ [(False, Nothing), (True, Nothing), (False, Just Agent.BoundedPatch), (True, Just Agent.CheckoutHarness)] \(structured, source) -> do
      (record, checked) <- recorder
      let unused :: a -> Either Text b
          unused _ = Left "Unexpected fixture operation"
          request = Agent.Request testPid (UUIDId UUID.nil) (UUIDId UUID.nil) "." revision (T.replicate 40 "b") "" "Fixture review" "fixture" "fixture" Agent.Off
          rejected = if structured then "{}" else "not-json"
          accepted = if structured then "{\"findings\":[]}" else "accepted"
          run response validation = runEff $ Agent.runCodeReviewTest (const response) unused unused unused unused unused do
            let check raw = liftIO (record raw) $> validation
            case source of
              Just mode -> Agent.reviewSource mode request (const pass) (const pass) (const pass) (const pass) (\case Agent.ProviderText raw -> check raw; Agent.Structured value -> check $ jsonText value)
              Nothing ->
                if structured
                  then Agent.reviewRepository Agent.Isolated request (pure . unused) (pure . unused) (const pass) (const pass) (const pass) (const pass) noProofs (check . jsonText)
                  else Agent.reviewPatch request (const pass) (const pass) check
      run (Right rejected) (Left "Review rejected") `shouldReturn` Left "Review rejected"
      run (Right accepted) (Right ()) `shouldReturn` Right accepted
      run (Left "Provider unavailable") (Right ()) `shouldReturn` Left "Provider unavailable"
      when structured $ run (Right "not-json") (Right ()) >>= (`shouldSatisfy` isLeft)
      checked `shouldReturn` [rejected, accepted]
    withSystemTempDirectory "review-suspension" \directory -> do
      let uid n = UUIDId $ UUID.fromWords 59 0 0 n
          request = Agent.Request{projectId = testPid, runId = uid 1, attemptId = uid 2, repository = "unavailable-source", base = revision, head = revision, gitToken = "", prompt = "Retain pending work", provider = "fixture", model = "fixture", thinkingLevel = Agent.Off}
          script = directory </> "review.sh"
          drained = directory </> "drained"
      writeFileBS script "read request\nprintf '%s\\n' '{\"tag\":\"Knowledge\",\"contents\":{\"tag\":\"Recall\",\"contents\":\"test-service\"}}'\nread reply\nsleep 0.25\nprintf drained > \"$1\"\n"
      outcome <- tryAny $ runEff $ scripted Nothing "sh" [script, drained] $ Agent.reviewRepository Agent.Isolated request (const $ pure $ Left "Unexpected telemetry") (const $ throwIO $ AI.AgentAwaitingReview (uid 3) frozenTime) (const pass) (const pass) (const pass) (const pass) noProofs (const $ pure $ Left "Unexpected result")
      case outcome of
        Left err -> case fromException err of
          Just (AI.AgentAwaitingReview identifier deadline) -> (identifier, deadline) `shouldBe` (uid 3, frozenTime)
          Nothing -> expectationFailure $ "Wrong native operation exception: " <> displayException err
        Right result -> expectationFailure $ "Native operation suspension was lost: " <> show result
      doesFileExist drained `shouldReturn` True

  it "patchComparison_acknowledgesCallsBeforeInference_withoutGuestAssets" \_ -> withTemp "native-patch-comparison" \directory -> do
    store <- storeIn directory
    let repository = directory </> "repository"
        sdkTemp = directory </> "tmp"
        uid n = UUIDId $ UUID.fromWords 57 0x4000 0x80000000 n
        accepted = AE.object (graded <> ["findings" AE..= ([] :: [AE.Value]), "coverage" AE..= ([] :: [Text])])
        decodedAs expected = either (fail . toString) (either fail pure . AE.eitherDecodeStrict . encodeUtf8 >=> (`shouldBe` expected))
    createDirectoryIfMissing True sdkTemp
    initRepo repository
    commitRevision <- commit repository [("src/source.txt", "fixture source\n"), ("src/binary", "\0\n")]
    unavailable <- newIORef False
    replyValue <- newIORef accepted
    let next body _ = do
          parse (AE.withObject "Patch request" (\value -> value AE..:? "tools" AE..!= ([] :: [AE.Value]))) body `shouldReturn` []
          failed <- readIORef unavailable
          response <- readIORef replyValue
          pure $ if failed then SdkUnavailable "fixture-insufficient-credit" else SdkResponse (SdkText $ jsonText response) (SdkTokens 7 2)
    withSdkProvider directory "patch-fixture" next \models requests -> do
      let patch n permitStart = do
            (recordCall, calls) <- recorder
            (recordUsage, usage) <- recorder
            requestCount <- length <$> requests
            let request = Agent.Request{projectId = testPid, runId = uid n, attemptId = uid 10, repository = toText repository, base = commitRevision, head = commitRevision, gitToken = "", prompt = "Review this empty patch.", provider = "patch-fixture", model = "fixture", thinkingLevel = Agent.Off}
                retained identifier expected = readJSON (store.usageDir </> toString request.runId.toText </> toString identifier <> ".json") `shouldReturn` expected
            result <-
              runEff
                $ sdkAgent store [models, "", directory </> "worker.log", "--temp", sdkTemp]
                $ Agent.reviewPatch
                  request
                  ( \receipt -> liftIO do
                      retained receipt.id.toText $ AE.object ["tag" AE..= ("Call" :: Text), "contents" AE..= receipt]
                      length <$> requests `shouldReturn` requestCount
                      unless permitStart $ throwIO $ userError "Fixture rejected model start"
                      recordCall receipt
                  )
                  (\receipt -> liftIO $ retained receipt.id.toText (AE.toJSON receipt) >> recordUsage receipt)
                  (\value -> pure $ if AE.eitherDecodeStrict (encodeUtf8 value) == Right accepted then Right () else Left "Rejected patch comparison")
            listDirectory sdkTemp `shouldReturn` []
            (result,,) <$> calls <*> usage
      (result, calls, usage) <- patch 1 True
      decodedAs accepted result
      length calls `shouldBe` 1
      map (.callId) usage `shouldBe` map (Just . (.id)) calls
      map (.succeeded) usage `shouldBe` [True]
      countBeforeRejection <- length <$> requests
      (blocked, blockedCalls, blockedUsage) <- patch 2 False
      blocked `shouldSatisfy` isLeft
      (blockedCalls, blockedUsage) `shouldSatisfy` \(starts, receipts) -> null starts && null receipts
      length <$> requests `shouldReturn` countBeforeRejection
      writeIORef unavailable True
      (failed, failedCalls, failedUsage) <- patch 3 True
      failed `shouldSatisfy` isLeft
      length failedCalls `shouldBe` 1
      map (\receipt -> (receipt.succeeded, receipt.costNanousd)) failedUsage `shouldBe` [(False, Nothing)]

      writeIORef unavailable False
      let sourceCaseFile = directory </> "source-case.json"
          sourceDraft = AE.object ["findings" AE..= ([] :: [AE.Value])]
          document repositoryURL = AE.object ["projectId" AE..= testPid, "repository" AE..= repository, "base" AE..= commitRevision, "head" AE..= commitRevision, "provider" AE..= ("patch-fixture" :: Text), "model" AE..= ("fixture" :: Text), "pullRequest" AE..= AE.object ["repo" AE..= (repositoryURL :: Text), "pr_number" AE..= (1 :: Int), "title" AE..= ("Source case" :: Text), "body" AE..= ("No telemetry context required." :: Text)]]
          source name =
            runEff
              $ sdkAgent store [models, "", directory </> "source-worker.log", "--temp", sdkTemp]
              $ Evaluation.runEvaluationReal
              $ Evaluation.evaluateSource (Agent.BoundedPatch :| []) sourceCaseFile (directory </> name)
          singleRun name = source name >>= either (fail . show) pure >>= \report -> case report.runs of [row] -> pure row; _ -> fail "Source review did not retain exactly one run"
      writeFileLBS sourceCaseFile $ AE.encode $ document "https://github.com/fixture/source"
      writeIORef replyValue sourceDraft
      row <- singleRun "source.json"
      case row.outcome of Evaluation.Completed _ value -> AE.toJSON value `shouldBe` sourceDraft; _ -> expectationFailure "Source baseline failed without a guest"
      length row.calls `shouldBe` 1
      map (.callId) row.usage `shouldBe` map (Just . (.id)) row.calls
      row.allocations `shouldSatisfy` null
      writeIORef replyValue accepted
      invalid <- source "source-invalid.json" >>= either (fail . show) pure
      concatMap (mapMaybe (leftToMaybe . (.validation)) . (.submissions)) invalid.runs `shouldSatisfy` any (T.isInfixOf "schema")
      for_ (["src", "src/binary", "src/source.txt"] :: [Text]) \path -> do
        writeIORef replyValue $ AE.object ["findings" AE..= ([AE.object ["file" AE..= path, "start_line" AE..= (1 :: Int), "end_line" AE..= (1 :: Int), "message" AE..= ("Fixture finding" :: Text)]] :: [AE.Value])]
        reviewed <- singleRun $ toString (T.replace "/" "-" path) <> ".json"
        case reviewed.outcome of
          Evaluation.Completed{} -> path `shouldBe` "src/source.txt"
          Evaluation.Failed{} -> do
            path `shouldNotBe` "src/source.txt"
            mapMaybe (leftToMaybe . (.validation)) reviewed.submissions `shouldSatisfy` any (T.isInfixOf "Finding file")
          Evaluation.Running -> expectationFailure "Source validation did not finish"
      count <- length <$> requests
      writeFileLBS sourceCaseFile $ AE.encode $ document "https://github.com/fixture/source?unexpected"
      leftToMaybe <$> source "source-malformed.json" `shouldReturn` Just Evaluation.InvalidCase
      length <$> requests `shouldReturn` count
      listDirectory sdkTemp `shouldReturn` []

  it "replays offline npm dependencies through the SDK without install scripts or shared mutations" \_ -> CLI.withCliBin \cli -> withTemp "native-sdk-dependencies" \directory -> do
    profile <- profileImage "impact-review-worker/.guest-monoscope"
    store <- storeIn directory
    bridge <- canonicalizePath "test/fixtures/native-sandbox.sh"
    let fixture = directory </> "package"
        repository = directory </> "repository"
        cache = directory </> "npm-cache"
        archive = directory </> "dependencies.tar"
        script = directory </> "prepare.sh"
        requestFile = directory </> "request.json"
        diagnostic = directory </> "worker.log"
        name = "monoscope-dependency-fixture" :: Text
        version = "1.0.0-fixture" :: Text
        uid n = UUIDId $ UUID.fromWords 53 0x4000 0x80000000 n
        npm cwd args = readProcessStdout_ $ setWorkingDir cwd $ proc "npm" args
        preparation = "set -eu\nmkdir -p /home/review-agent/dependency-cache\ntar -xf /run/monoscope-dependencies.tar -C /home/review-agent/dependency-cache\nnpm ci --offline --ignore-scripts --no-audit --no-fund --cache /home/review-agent/dependency-cache\ntest ! -e /workspace/install-script-ran\ntest ! -e /home/review-agent/dependency-cache/host-only-secret\n" :: ByteString
        replay = "set -eu\ntest ! -e /workspace/install-script-ran\ntest ! -e node_modules/monoscope-dependency-fixture/vm-only\ntouch node_modules/monoscope-dependency-fixture/vm-only\nnode check.cjs\n" :: Text
        options = Workspace.Options store.cacheDir (toText profile) bridge script (Just archive) Nothing
        lookupPrepared configured = runEff $ Workspace.runWorkspaceReal $ Workspace.runWorkspace configured Workspace.Lookup requestFile
    traverse_ (createDirectoryIfMissing True) [fixture, cache]
    initRepo repository
    writeFileLBS (fixture </> "package.json") $ AE.encode $ AE.object ["name" AE..= name, "version" AE..= version, "main" AE..= ("index.cjs" :: Text), "scripts" AE..= AE.object ["install" AE..= ("touch /workspace/install-script-ran" :: Text)]]
    writeFileBS (fixture </> "index.cjs") "module.exports = value => value;\n"
    packed <- npm fixture ["pack", "--offline", "--ignore-scripts", "--json"] >>= either fail pure . AE.eitherDecode @(NonEmpty AE.Value)
    filename <- parse (field "filename") $ head packed
    let tarball = fixture </> filename
    contents <- readFileBS tarball
    void $ npm fixture ["cache", "add", tarball, "--cache", cache, "--offline", "--ignore-scripts", "--loglevel", "error"]
    writeFileBS (cache </> "host-only-secret") "must not be supplied"
    void $ readProcessStdout_ $ proc "tar" ["-cf", archive, "-C", cache, "_cacache/content-v2"]
    bytes <- readFileBS archive
    let dependency = AE.object ["sha256" AE..= sha256 bytes, "bytes" AE..= BS.length bytes]
        dependencies = AE.object [fromString (toString name) AE..= version]
        consumer = AE.object ["name" AE..= ("dependency-consumer" :: Text), "version" AE..= ("1.0.0" :: Text), "dependencies" AE..= dependencies]
        integrity = "sha512-" <> extractBase64 (B64.encodeBase64 $ BA.convert (hash contents :: Digest SHA512))
    writeFileLBS (repository </> "package.json") $ AE.encode consumer
    writeFileLBS (repository </> "package-lock.json") $ AE.encode $ AE.object ["name" AE..= ("dependency-consumer" :: Text), "version" AE..= ("1.0.0" :: Text), "lockfileVersion" AE..= (3 :: Int), "packages" AE..= AE.object ["" AE..= consumer, fromString (toString $ "node_modules/" <> name) AE..= AE.object ["version" AE..= version, "resolved" AE..= ("https://registry.npmjs.org/" <> name <> "/-/" <> name <> "-" <> version <> ".tgz"), "integrity" AE..= integrity, "hasInstallScript" AE..= True]]]
    writeFileBS (repository </> "runtime.cjs") "module.exports = require('monoscope-dependency-fixture');\n"
    writeFileBS (repository </> "check.cjs") "require('node:assert/strict').equal(require('./runtime.cjs')('original'), 'original', 'dependency echo lost');\n"
    base <- commit repository []
    headRevision <- commit repository [("runtime.cjs", "module.exports = () => require('monoscope-dependency-fixture')('lost');\n")]
    writeFileLBS requestFile $ AE.encode $ AE.object ["projectId" AE..= testPid, "repository" AE..= repository, "base" AE..= base]
    writeFileBS script preparation
    prepared <- runEff (Workspace.runWorkspaceReal $ Workspace.runWorkspace options (Workspace.Prepare repository) requestFile) >>= either (fail . toString) (maybe (fail "Dependency preparation missing") (pure . AE.toJSON))
    manifest <- parse (field "manifest") prepared
    parse (field "identity" >=> field "dependency") manifest `shouldReturn` dependency
    imageId <- parse (field "identity" >=> field "imageBuildId") manifest :: IO Text
    let legacy = Encoding.pairs ("projectId" AE..= testPid <> "repository" AE..= repository <> "base" AE..= base <> "imageBuildId" AE..= imageId <> "preparationSha256" AE..= sha256 preparation <> Encoding.pair "dependency" (Encoding.pairs $ "sha256" AE..= sha256 bytes <> "bytes" AE..= BS.length bytes))
        key = sha256 $ toStrict $ Encoding.encodingToLazyByteString legacy
    checkpoint <- parse (field "path") prepared
    takeFileName (takeDirectory checkpoint) `shouldBe` toString key
    Posix.getFileStatus checkpoint >>= (`shouldSatisfy` (> 0)) . Posix.fileSize
    loaded <- lookupPrepared options >>= either (fail . toString) pure
    fmap AE.toJSON loaded `shouldBe` Just prepared
    writeFileBS archive ""
    isLeft <$> lookupPrepared options `shouldReturn` True
    writeFileBS archive bytes
    fmap isNothing <$> lookupPrepared options{Workspace.dependencyArchive = Nothing} `shouldReturn` Right True
    writeFileBS archive $ bytes <> "changed"
    fmap isNothing <$> lookupPrepared options `shouldReturn` Right True
    writeFileBS archive bytes
    let link = directory </> "archive-link"
    createFileLink archive link
    isLeft <$> lookupPrepared options{Workspace.dependencyArchive = Just link} `shouldReturn` True
    let next results = pure $ case length results of
          0 -> SdkTool "run_regression" (AE.object ["script" AE..= replay, "failureMessage" AE..= ("dependency echo lost" :: Text)])
          1 -> SdkTool "submit_review" (AE.object (graded <> ["findings" AE..= ([] :: [AE.Value]), "coverage" AE..= ([] :: [AE.Value])]))
          _ -> SdkText "Fixture finished."
    withSdkModel directory "dependency-fixture" next \models requests -> do
      let request = Agent.Request{projectId = testPid, runId = uid 1, attemptId = uid 2, repository = toText repository, base, head = headRevision, gitToken = "", prompt = "Check actual offline dependency behavior.", provider = "dependency-fixture", model = "fixture", thinkingLevel = Agent.Off}
          run n trustedScript = do
            checks <- newIORef ([] :: [Agent.CheckReceipt])
            result <-
              runEff
                $ sdkAgent store [models, profile, diagnostic, "--prepare", trustedScript, "--dependencies", archive, "--cli", cli]
                $ Agent.reviewRepository Agent.Isolated request{Agent.runId = uid n} (const $ pure $ Left "Unexpected query") (const $ pure $ Left "Unexpected knowledge") (const pass) (const pass) (const pass) (\receipt -> liftIO $ modifyIORef' checks (<> [receipt])) noProofs (const $ pure $ Right ())
            (result,) <$> readIORef checks
      writeFileBS archive $ bytes <> "changed"
      (unseeded, _) <- run 1 script
      isLeft unseeded `shouldBe` True
      length <$> requests `shouldReturn` 0
      writeFileBS archive bytes
      (untrusted, _) <- run 3 ""
      isLeft untrusted `shouldBe` True
      length <$> requests `shouldReturn` 0
      (result, checks) <- run 4 script
      encoded <- either (\failure -> readFileBS diagnostic >>= fail . (toString failure <>) . toString . decodeUtf8 @Text) pure result
      AE.eitherDecodeStrict @AE.Value (encodeUtf8 encoded) `shouldBe` Right (AE.object (graded <> ["findings" AE..= ([] :: [AE.Value]), "coverage" AE..= ([] :: [AE.Value])]))
      fingerprint <- parse (field "sha256") manifest :: IO Text
      map (\receipt -> (receipt.side, receipt.status, AE.toJSON receipt.workspace)) checks `shouldBe` [(Agent.Base, Agent.Passed, AE.toJSON fingerprint), (Agent.Head, Agent.Failed, AE.toJSON fingerprint)]
      events <- mapMaybe (AE.decodeStrict @AE.Value) . BSC.lines <$> readFileBS diagnostic
      events `shouldSatisfy` any (\value -> AET.parseEither (field "type") value == Right ("workspace_cache" :: Text) && AET.parseEither (field "status") value == Right ("hit" :: Text) && AET.parseEither (field "sha256") value == Right fingerprint)

  it "runs frozen regressions natively, fingerprints gitlink contents and rejects weakened candidate tests" \_ -> withTemp "native-regression" \directory -> do
    image <- guestImage
    bridge <- canonicalizePath "test/fixtures/native-sandbox.sh"
    let repository = directory </> "repository"
        artifacts = directory </> "checks"
        uid n = UUIDId $ UUID.fromWords 0 0 0 n :: UUIDId "fixture"
        git = gitIn repository
        revisionAt = gitText repository ["rev-parse", "HEAD"]
    createDirectoryIfMissing True artifacts
    initRepo repository
    writeFileBS (repository </> "runtime.sh") "retry_token() { printf 'dropped\\n'; }\n"
    writeFileBS (repository </> "executable.sh") "#!/bin/bash\nexit 0\n"
    Posix.setFileMode (repository </> "executable.sh") 0o755
    createDirectoryIfMissing True (directory </> "outside")
    createFileLink (directory </> "outside") (repository </> "linked-test")
    void $ git ["add", "."]
    void $ git ["commit", "--quiet", "-m", "broken base"]
    initial <- revisionAt
    createDirectoryIfMissing True $ repository </> "vendor"
    void $ git ["update-index", "--add", "--cacheinfo", "160000," <> toString initial <> ",vendor"]
    void $ git ["commit", "--quiet", "-m", "retain uninitialized submodule"]
    base <- revisionAt
    writeFileBS (repository </> "regression.sh") "set -eu\ntest -d vendor\n. ./runtime.sh\nif [ \"$(retry_token original)\" != original ]; then echo 'retry token was lost' >&2; exit 1; fi\necho regression-passed\n"
    void $ git ["add", "."]
    void $ git ["commit", "--quiet", "-m", "retain regression"]
    testPatch <- git ["show", "--format=", "--binary", "HEAD"]
    writeFileBS (repository </> "runtime.sh") "retry_token() { printf '%s\\n' \"$1\"; }\n"
    void $ git ["commit", "--quiet", "-am", "fix retry token"]
    imageId <- readJSON (image </> "manifest.json") >>= parse (field "buildId") :: IO Text
    let runWith session artifact expectedPatch = do
          headRevision <- revisionAt
          let request = AE.object ["projectId" AE..= testPid, "runId" AE..= uid 1, "attemptId" AE..= uid 2, "sessionId" AE..= uid session, "base" AE..= base, "imageBuildId" AE..= imageId, "workspace" AE..= AE.object ["tag" AE..= ("Pristine" :: Text)], "artifact" AE..= artifact, "revisions" AE..= ([["base", base], ["head", headRevision]] :: [[Text]])]
              options = Workspace.RegressionOptions artifacts repository (toText image) bridge
              requestPath = directory </> "request.json"
          writeFileLBS requestPath $ AE.encode request
          outcome <- runEff $ Workspace.runWorkspaceReal $ Workspace.runRegression options requestPath
          result <- either (fail . toString . Workspace.regressionErrorText) (pure . AE.toJSON) outcome
          report <- parse (field "report") result
          rows <- parse (field "results") report :: IO [AE.Value]
          receipts <- parse (field "receipts") result :: IO [Agent.CheckReceipt]
          for_ receipts \receipt -> do
            readJSON (directory </> toString receipt.id.toText <> ".json") `shouldReturn` AE.object ["tag" AE..= ("Check" :: Text), "contents" AE..= receipt]
            for_ expectedPatch $ \patch -> readFileBS (artifacts </> toString receipt.checkId.toText </> "test.patch") `shouldReturn` patch
          pure (report, rows, receipts)
        run session script patch = runWith session (AE.object ["script" AE..= (script :: Text), "testPatch" AE..= decodeUtf8 @Text patch, "failureMessage" AE..= ("retry token was lost" :: Text)]) (Just patch)
        statuses rows = traverse (parse $ field "status") rows :: IO [Text]
    (report, rows, receipts) <- run 3 "bash regression.sh" testPatch
    statuses rows `shouldReturn` ["failed", "passed"]
    (_, mutatedDirectory, _) <- run 10 "mkdir -p vendor/nested; printf changed > vendor/nested/source; ln -s nested/source vendor/link; exit 0" testPatch
    statuses mutatedDirectory `shouldReturn` ["invalidated", "invalidated"]
    parse (field "testPatchSha256") report `shouldReturn` sha256 testPatch
    fingerprints <- traverse (parse $ field "testFilesSha256") rows :: IO [Text]
    length (ordNub fingerprints) `shouldBe` 1
    content <- decodeUtf8 @Text <$> readFileBS (repository </> "regression.sh")
    (_, generated, _) <- runWith 7 (AE.object ["script" AE..= ("bash regression.sh" :: Text), "failureMessage" AE..= ("retry token was lost" :: Text), "testFiles" AE..= V.singleton (AE.object ["path" AE..= ("regression.sh" :: Text), "content" AE..= content])]) Nothing
    statuses generated `shouldReturn` ["failed", "passed"]
    traverse (parse $ field "testFilesSha256") generated `shouldReturn` fingerprints
    let executable = AE.object ["script" AE..= ("./executable.sh" :: Text), "failureMessage" AE..= ("retry token was lost" :: Text), "testFiles" AE..= V.singleton (AE.object ["path" AE..= ("executable.sh" :: Text), "content" AE..= ("#!/bin/bash\n" <> content)])]
    (_, executableRows, _) <- runWith 8 executable Nothing
    statuses executableRows `shouldReturn` ["failed", "passed"]
    let nested = AE.object ["script" AE..= ("cmp <(printf '\\000\\316\\273') tests/generated/fixture.bin && bash tests/generated/regression.sh" :: Text), "failureMessage" AE..= ("retry token was lost" :: Text), "testFiles" AE..= V.fromList [AE.object ["path" AE..= path, "content" AE..= body] | (path, body) <- [("tests/generated/regression.sh", content), ("tests/generated/fixture.bin", "\NULλ")] :: [(Text, Text)]]]
    (_, nestedRows, _) <- runWith 9 nested Nothing
    statuses nestedRows `shouldReturn` ["failed", "passed"]
    git ["status", "--porcelain=v1"] `shouldReturn` ""
    for_ @[] [("linked-test", "not a test"), ("linked-test/outside", "not a test"), ("runtime.sh", "retry_token() { printf 'dropped\\n'; }\n")] \(path, body) -> do
      source <- readFileBS (repository </> "runtime.sh")
      files <- parse AE.parseJSON $ AE.toJSON $ V.singleton $ AE.object ["path" AE..= (path :: Text), "content" AE..= (body :: Text)]
      rejected <- Workspace.renderTestPatch repository base files
      isLeft rejected `shouldBe` True
      readFileBS (repository </> "runtime.sh") `shouldReturn` source
    git ["status", "--porcelain=v1"] `shouldReturn` ""
    listDirectory (directory </> "outside") `shouldReturn` []
    let shortHeader = encodeUtf8 $ shortHunks $ decodeUtf8 testPatch
    (_, recounted, _) <- run 6 "bash regression.sh" shortHeader
    statuses recounted `shouldReturn` ["failed", "passed"]
    traverse (parse $ field "testFilesSha256") recounted `shouldReturn` fingerprints
    let frozen = "{\"script\":\"bash regression.sh\",\"testPatch\":" <> toStrict (AE.encode $ decodeUtf8 @Text testPatch) <> ",\"failureMessage\":\"retry token was lost\"}"
    AE.toJSON (map (.regression) receipts) `shouldBe` AE.toJSON ([Just $ Agent.InjectedTest $ sha256 frozen, Just $ Agent.RetainedTest $ sha256 frozen] :: [Maybe Agent.Regression])
    (_, mutated, _) <- run 3 "printf weakened > regression.sh" testPatch
    statuses mutated `shouldReturn` ["invalidated", "invalidated"]
    (_, invalid, invalidReceipts) <- run 3 "exit 0" "not a patch"
    statuses invalid `shouldReturn` ["blocked", "blocked"]
    traverse (parse $ field "reason") invalid >>= (`shouldSatisfy` all (T.isInfixOf "Git diff"))
    map (.exitCode) invalidReceipts `shouldBe` [Nothing, Nothing]
    exhausted <- runEff $ Workspace.runWorkspaceReal $ Workspace.runRegression (Workspace.RegressionOptions artifacts repository (toText image) bridge) (directory </> "request.json")
    leftToMaybe (first Workspace.regressionErrorText exhausted) `shouldBe` Just "Local check comparison budget is exhausted"
    void $ git ["rm", "--quiet", "regression.sh"]
    void $ git ["commit", "--quiet", "-m", "fix without retained test"]
    (_, injected, injectedReceipts) <- run 4 "bash regression.sh" testPatch
    statuses injected `shouldReturn` ["failed", "passed"]
    map (.regression) injectedReceipts `shouldSatisfy` all (\case Just Agent.InjectedTest{} -> True; _ -> False)
    writeFileBS (repository </> "regression.sh") "echo regression-passed\n"
    void $ git ["add", "."]
    void $ git ["commit", "--quiet", "-m", "weaken candidate test"]
    (_, weakened, _) <- run 5 "bash regression.sh" testPatch
    statuses weakened `shouldReturn` ["failed", "blocked"]

  it "reports host maintenance budgets without deleting unknown cache data" \_ -> withSystemTempDirectory "maintenance-budget" \directory -> do
    let config = directory </> "maintenance.json"
        cache = directory </> "cache"
        unknown = cache </> "operator-file"
    writeFileLBS config $ AE.encode $ AE.object ["cacheDir" AE..= cache, "remote" AE..= (directory </> "missing-remote"), "maxBytes" AE..= (maxBound :: Word64)]
    (initialized, _) <- CLI.runCLILifecycleLive ["workspace", "maintain", "--config", config]
    initialized `shouldBe` ExitSuccess
    doesDirectoryExist cache `shouldReturn` True
    writeFileBS unknown "Retain this unknown file."
    for_ [0, maxBound] \budget -> do
      writeFileLBS config $ AE.encode $ AE.object ["cacheDir" AE..= cache, "remote" AE..= (directory </> "missing-remote"), "maxBytes" AE..= (budget :: Word64)]
      (status, output) <- CLI.runCLILifecycleLive ["workspace", "maintain", "--config", config]
      report <- either fail pure $ AE.eitherDecodeStrict @Workspace.MaintenanceReport $ encodeUtf8 output
      Workspace.maintenanceBudgetMet report `shouldBe` (budget == maxBound)
      status `shouldBe` if budget == maxBound then ExitSuccess else ExitFailure 1
      readFileBS unknown `shouldReturn` "Retain this unknown file."

  it "nativeGuest_resumePreservesCompletedEdits" \_ -> do
    profile <- nativeTestImage
    withTemp "native-guest-resume" \directory -> do
      bridge <- canonicalizePath "test/fixtures/native-sandbox.sh"
      let repository = directory </> "repository"
          host = Guest.Host (toText profile) bridge [] Nothing
      initRepo repository
      base <- commit repository [("completed.txt", "original\n")] >>= either (fail . show) pure . Workspace.mkRevision
      let open :: Guest.Start -> (Guest.Session -> IO a) -> IO (Either Text a)
          open preparation = Guest.withGuest host Nothing repository base preparation Nothing (UUIDId UUID.nil) (const $ pure ()) (const $ pure ())
      let disk = directory </> "suspended.qcow2"
          digest = readProcessStdout_ $ proc "shasum" ["-a", "256", disk]
      saved <- open (Guest.Fresh Guest.Explore) \session -> do
        session.root ("/bin/sh" :| ["-c", "printf 'completed edit\\n' >> /workspace/completed.txt"])
        session.suspend disk
      suspended <- either (fail . toString) pure saved
      restored <- either fail pure $ AE.eitherDecode @Guest.Suspended $ AE.encode suspended
      originalHash <- digest
      replicateM_ 2 do
        resumed <- open (Guest.ResumeSession restored) \session -> do
          contents <- session.output ("/bin/cat" :| ["/workspace/completed.txt"]) 1024
          session.root ("/bin/sh" :| ["-c", "printf 'discarded edit\\n' >> /workspace/completed.txt"])
          pure contents
        fmap (\(_, bytes, truncated) -> (bytes, truncated)) resumed `shouldBe` Right ("original\ncompleted edit\n", False)
        digest `shouldReturn` originalHash
      entered <- newIORef False
      changed <- either (fail . show) pure $ Workspace.mkRevision $ T.replicate 40 "1"
      mismatched <- Guest.withGuest host Nothing repository changed (Guest.ResumeSession restored) Nothing (UUIDId UUID.nil) (const $ pure ()) (const $ pure ()) $ const $ writeIORef entered True
      mismatched `shouldSatisfy` isLeft
      readIORef entered `shouldReturn` False
      writeFileBS disk "corrupted checkpoint"
      rejected <- open (Guest.ResumeSession restored) $ const $ writeIORef entered True
      rejected `shouldSatisfy` isLeft
      readIORef entered `shouldReturn` False

  it "reuses native Haskell build checkpoints without mutating relocated baselines" \tr -> do
    profile <- profileImage "impact-review-worker/.guest-monoscope"
    withTemp "native-profile" \directory -> do
      bridge <- canonicalizePath "test/fixtures/native-sandbox.sh"
      let repository = directory </> "repository"
          cache = directory </> "cache"
          script = directory </> "prepare.sh"
          request = directory </> "request.json"
          checks = directory </> "checks"
          source = "module Main where\nimport Data.Aeson (eitherDecodeStrict')\nimport qualified Data.ByteString.Char8 as BS\nmain = case eitherDecodeStrict' (BS.pack (show (\"cached\" :: String))) :: Either String String of\n  Right \"cached\" -> putStrLn \"cached-package-check-passed\"\n  _ -> fail \"cached dependency decoding failed\"\n" :: ByteString
          options = Workspace.Options cache (toText profile) bridge script Nothing Nothing
          lookupPrepared encodedRequest workspaceOptions = do
            writeFileLBS request $ AE.encode encodedRequest
            runEff $ Workspace.runWorkspaceReal $ Workspace.runWorkspace workspaceOptions Workspace.Lookup request
      traverse_ (createDirectoryIfMissing True) [cache, checks]
      initRepo repository
      base <- commit repository [("Main.hs", source), ("Makefile", "prepared-app: Main.hs\n\tghc -O0 -package-db=\"$$(echo \"$$HOME\"/.cabal/store/ghc-*/package.db)\" -package=aeson Main.hs -o prepared-app\ncheck: prepared-app\n\ttest \"$$(id -u)\" = 1000\n\ttest ! -e host-only-secret\n\ttest \"$$(ghc --numeric-version)\" = 9.12.2\n\tmonoscope --help >/dev/null\n\tnode --version\n\ttest ! -e \"$$HOME/.cabal/profile-test-marker\"\n\ttouch \"$$HOME/.cabal/profile-test-marker\"\n\t./prepared-app\n")]
      headRevision <- commit repository [("Main.hs", encodeUtf8 $ T.replace "show (\"cached\" :: String)" "show (\"lost\" :: String)" $ decodeUtf8 @Text source)]
      writeFileBS (repository </> "host-only-secret") "not committed"
      writeFileBS script "exec make prepared-app"
      let input pid repo revisionValue = AE.object ["projectId" AE..= (pid :: Projects.ProjectId), "repository" AE..= (repo :: Text), "base" AE..= (revisionValue :: Text)]
          original = input testPid (toText repository) base
      writeFileLBS request $ AE.encode original
      (exit, promoted) <- CLI.runCLILifecycleLive ["workspace", "prepare", "--request", request, "--cache", cache, "--image", profile, "--bridge", bridge, "--script", script, "--checkout", repository]
      exit `shouldBe` ExitSuccess
      promotedPrepared <- either fail pure $ AE.eitherDecodeStrict @AE.Value $ encodeUtf8 promoted
      prepared <- runEff (Workspace.runWorkspaceReal $ Workspace.runWorkspace options (Workspace.Prepare repository) request) >>= either (fail . toString) (maybe (fail "Missing checkpoint") (pure . AE.toJSON))
      loaded <- lookupPrepared original options >>= either (fail . toString) (maybe (fail "Missing cache hit") (pure . AE.toJSON))
      loaded `shouldBe` prepared
      prepared `shouldBe` promotedPrepared
      manifest <- parse (field "manifest") prepared
      checkpoint <- parse (field "path") prepared
      fingerprint <- parse (field "sha256" :: AE.Value -> AET.Parser Text) manifest
      workspaceIdentity <- parse (field "identity") manifest
      imageId <- parse (field "imageBuildId" :: AE.Value -> AET.Parser Text) workspaceIdentity
      for_ [input (UUIDId $ UUID.fromWords 0 0 0 99) (toText repository) base, input testPid (toText repository) headRevision, input testPid "https://github.com/another/repository.git" base] \changed -> do
        outcome <- lookupPrepared changed options
        isNothing <$> outcome `shouldBe` Right True
      let mismatchedImage = withKey "expectedImageBuildId" (AE.toJSON $ UUID.fromWords 0 0 0 99) original
      isLeft <$> lookupPrepared mismatchedImage options `shouldReturn` True
      denied <- lookupPrepared (input testPid "https://credential@github.com/private/repository.git" base) options
      isLeft denied `shouldBe` True
      writeFileBS script "exec make different-target"
      fmap isNothing <$> lookupPrepared original options `shouldReturn` Right True
      writeFileBS script "echo modified > Main.hs"
      writeFileLBS request $ AE.encode original
      isLeft <$> runEff (Workspace.runWorkspaceReal $ Workspace.runWorkspace options (Workspace.Prepare repository) request) `shouldReturn` True
      writeFileBS script "exec make prepared-app"
      let regression snapshotPath snapshotHash = AE.object ["projectId" AE..= testPid, "runId" AE..= testPid, "attemptId" AE..= testPid, "sessionId" AE..= testPid, "base" AE..= base, "imageBuildId" AE..= imageId, "workspace" AE..= AE.object ["tag" AE..= ("PreparedCheckpoint" :: Text), "contents" AE..= AE.object ["path" AE..= (snapshotPath :: FilePath), "sha256" AE..= (snapshotHash :: Text)]], "artifact" AE..= AE.object ["script" AE..= ("exec make check" :: Text), "failureMessage" AE..= ("cached dependency decoding failed" :: Text)], "revisions" AE..= ([["base", base], ["head", headRevision]] :: [[Text]])]
      writeFileLBS request $ AE.encode $ regression checkpoint fingerprint
      result <- runEff (Workspace.runWorkspaceReal $ Workspace.runRegression (Workspace.RegressionOptions checks repository (toText profile) bridge) request) >>= either (fail . show) (pure . AE.toJSON)
      rows <- parse (field "report" >=> field "results") result :: IO [AE.Value]
      traverse (parse $ field "status") rows `shouldReturn` (["passed", "failed"] :: [Text])
      outputTails <- traverse (parse $ field "tail") rows :: IO [Text]
      map (T.isInfixOf "Compiling Main") outputTails `shouldBe` [False, True]
      parse (field "report" >=> field "workspace") result `shouldReturn` fingerprint
      let manifestPath = takeDirectory checkpoint </> "manifest.json"
      void $ readProcessStdout_ $ proc "qemu-img" ["rebase", "-u", "-F", "raw", "-b", "/old-monoscope-host/rootfs.ext4", checkpoint]
      relocatedBytes <- readFileBS checkpoint
      let relocatedHash = sha256 relocatedBytes
          relocatedManifest = withKey "bytes" (AE.toJSON $ BS.length relocatedBytes) $ withKey "sha256" (AE.String relocatedHash) manifest
      writeFileLBS manifestPath $ AE.encode relocatedManifest
      writeFileLBS request $ AE.encode $ regression checkpoint relocatedHash
      relocated <- runEff (Workspace.runWorkspaceReal $ Workspace.runRegression (Workspace.RegressionOptions checks repository (toText profile) bridge) request) >>= either (fail . show) (pure . AE.toJSON)
      sha256 <$> readFileBS checkpoint `shouldReturn` relocatedHash
      isRight <$> lookupPrepared original options `shouldReturn` True
      relocatedRows <- parse (field "report" >=> field "results") relocated :: IO [AE.Value]
      traverse (parse $ field "status") relocatedRows `shouldReturn` (["passed", "failed"] :: [Text])
      writeFileLBS request $ AE.encode original
      held <- runEff $ Workspace.runWorkspaceReal $ Workspace.withWorkspace options request \case
        Nothing -> liftIO $ fail "Prepared workspace lease missing"
        Just value -> do
          let frame = AE.toJSON value
          immutable <- liftIO $ parse (field "prepared" >=> field "path") frame
          mutable <- liftIO $ parse (field "resumePath") frame
          liftIO do
            immutable `shouldNotBe` (mutable :: FilePath)
            sha256 <$> readFileBS immutable `shouldReturn` relocatedHash
            removeFile checkpoint
            writeFileLBS request $ AE.encode $ regression immutable relocatedHash
          replay <- Workspace.runRegression (Workspace.RegressionOptions checks repository (toText profile) bridge) request
          liftIO do
            replayed <- either (fail . show) (pure . AE.toJSON) replay
            statuses <- parse (field "report" >=> field "results") replayed >>= traverse (parse $ field "status")
            statuses `shouldBe` (["passed", "failed"] :: [Text])
            sha256 <$> readFileBS immutable `shouldReturn` relocatedHash
            pure immutable
      released <- either (fail . toString) pure held
      doesDirectoryExist (takeDirectory released) `shouldReturn` False
      writeFileBS checkpoint relocatedBytes
      writeFileLBS request $ AE.encode original
      borrowerFailure <- tryAny $ runEff $ Workspace.runWorkspaceReal $ Workspace.withWorkspace options request $ \_ -> liftIO (throwIO (userError "Fixture borrower failed") :: IO ())
      borrowerFailure `shouldSatisfy` \case Left failure -> "Fixture borrower failed" `T.isInfixOf` toText (displayException failure); Right _ -> False
      requireMinio tr pendingWith do
        remoteObjects <- newIORef []
        let config = tr.trATCtx.config
            remote = directory </> "remote.json"
            transfer command target = CLI.runCLILifecycleLive ["workspace", command, "--request", request, "--cache", target, "--image", profile, "--bridge", bridge, "--script", script, "--remote", remote]
            remotePrefix = T.intercalate "/" ["agent-workspaces", "v1", testPid.toText, toText $ takeFileName $ takeDirectory checkpoint]
            objectKey = remotePrefix <> "/" <> relocatedHash <> ".qcow2"
            connection = minioFor config
            overwrite key bytes = do
              uploaded <- Storage.runMinio tr.trATCtx.s3HttpManager connection $ Minio.putObject config.s3Bucket key (CC.sourceLazy bytes) (Just $ LBS.length bytes) Minio.defaultPutObjectOptions
              uploaded `shouldSatisfy` isRight
            replace = withKey
            remoteConfiguration = AE.object ["endpoint" AE..= config.s3Endpoint, "bucket" AE..= config.s3Bucket, "region" AE..= config.s3Region, "accessKey" AE..= config.s3AccessKey, "secretKey" AE..= config.s3SecretKey, "maxBytes" AE..= (BS.length relocatedBytes * 3 + 1024)]
            removeObjects = readIORef remoteObjects >>= \keys -> void $ Storage.runMinio tr.trATCtx.s3HttpManager connection $ traverse_ (Minio.removeObject config.s3Bucket) keys
        writeIORef remoteObjects [objectKey, remotePrefix <> "/manifest.json", remotePrefix <> "/" <> relocatedHash <> ".json"]
        writeFileLBS remote $ AE.encode remoteConfiguration
        Posix.setFileMode remote 0o600
        ( do
            (archived, _) <- transfer "archive" cache
            archived `shouldBe` ExitSuccess
            removePathForcibly $ takeDirectory checkpoint
            (restored, encoded) <- transfer "restore" cache
            restored `shouldBe` ExitSuccess
            restoredPrepared <- either fail pure $ AE.eitherDecodeStrict @AE.Value $ encodeUtf8 encoded
            parse (field "manifest" >=> field "sha256") restoredPrepared `shouldReturn` relocatedHash
            sha256 <$> readFileBS checkpoint `shouldReturn` relocatedHash
            (cached, _) <- transfer "restore" cache
            cached `shouldBe` ExitSuccess
            writeFileLBS remote $ AE.encode $ replace "maxBytes" (AE.toJSON $ BS.length relocatedBytes - 1) remoteConfiguration
            (overBudget, _) <- transfer "archive" cache
            overBudget `shouldBe` ExitFailure 1
            writeFileLBS remote $ AE.encode remoteConfiguration
            removePathForcibly $ takeDirectory checkpoint
            for_ ["corrupt snapshot", toLazy $ BS.map (`xor` 1) relocatedBytes] \corrupted -> do
              overwrite objectKey corrupted
              (corrupt, _) <- transfer "restore" cache
              corrupt `shouldBe` ExitFailure 1
              doesDirectoryExist (takeDirectory checkpoint) `shouldReturn` False
            stages <- listDirectory $ takeDirectory $ takeDirectory checkpoint
            stages `shouldSatisfy` not . any (isPrefixOf ".restore-")
            overwrite objectKey $ toLazy relocatedBytes
            foreignIdentity <- parse (field "identity") relocatedManifest
            for_ [replace "identity" (replace "projectId" (AE.toJSON $ UUID.fromWords 0 0 0 99) foreignIdentity) relocatedManifest, replace "bytes" (AE.toJSON $ BS.length relocatedBytes + 10000) relocatedManifest] \tampered -> do
              overwrite (remotePrefix <> "/manifest.json") $ AE.encode tampered
              (rejected, _) <- transfer "restore" cache
              rejected `shouldBe` ExitFailure 1
              doesDirectoryExist (takeDirectory checkpoint) `shouldReturn` False
            writeFileLBS request $ AE.encode $ replace "expectedCheckpoint" (AE.String relocatedHash) original
            (pinnedRestore, _) <- transfer "restore" cache
            pinnedRestore `shouldBe` ExitSuccess
            sha256 <$> readFileBS checkpoint `shouldReturn` relocatedHash
            removePathForcibly $ takeDirectory checkpoint
            writeFileLBS request $ AE.encode original
            overwrite (remotePrefix <> "/manifest.json") $ AE.encode $ replace "assetDir" (AE.String "/old-monoscope-host/image") relocatedManifest
            (relocatedRestore, output) <- transfer "restore" cache
            relocatedRestore `shouldBe` ExitSuccess
            normalized <- either fail pure $ AE.eitherDecodeStrict @AE.Value $ encodeUtf8 output
            originalAssetDir <- parse (field "assetDir") manifest :: IO FilePath
            parse (field "manifest" >=> field "assetDir") normalized `shouldReturn` originalAssetDir
            writeFileLBS request $ AE.encode $ replace "sessionId" (AE.toJSON $ UUID.fromWords 0 0 0 97) $ regression checkpoint relocatedHash
            replay <- runEff (Workspace.runWorkspaceReal $ Workspace.runRegression (Workspace.RegressionOptions checks repository (toText profile) bridge) request) >>= either (fail . show) (pure . AE.toJSON)
            statuses <- parse (field "report" >=> field "results") replay >>= traverse (parse $ field "status")
            statuses `shouldBe` (["passed", "failed"] :: [Text])
            sha256 <$> readFileBS checkpoint `shouldReturn` relocatedHash
            writeFileLBS request $ AE.encode original
            let records = cache </> toString testPid.toText </> "transfers"
            receipts <- listDirectory records >>= traverse (\name -> readJSON (records </> name))
            tags <- traverse (parse $ field "outcome" >=> field "tag") receipts
            sort tags `shouldBe` sort (["TransferCompleted", "TransferCompleted", "TransferFailed", "TransferFailed", "TransferFailed", "TransferFailed", "TransferFailed", "TransferCompleted", "TransferCompleted"] :: [Text])
            for_ receipts \receipt -> do
              parse (field "identity" >=> field "projectId") receipt `shouldReturn` testPid
              parse (field "resourceCostStatus") receipt `shouldReturn` ("unpriced" :: Text)
              show receipt `shouldSatisfy` (not . T.isInfixOf config.s3SecretKey)
            let pruneRequest = directory </> "prune.json"
                prune budget = do
                  writeFileLBS pruneRequest $ AE.encode $ AE.object ["projectId" AE..= testPid, "maxBytes" AE..= (budget :: Word64)]
                  (pruned, report) <- CLI.runCLILifecycleLive ["workspace", "prune", "--request", pruneRequest, "--cache", cache, "--remote", remote]
                  decoded <- either fail pure $ AE.eitherDecodeStrict @AE.Value $ encodeUtf8 report
                  remaining <- parse (field "afterBytes") decoded :: IO Word64
                  pruned `shouldBe` if remaining <= budget then ExitSuccess else ExitFailure 1
                  pure decoded
                kept key reason = AE.object ["tag" AE..= ("Kept" :: Text), "contents" AE..= ([key, reason] :: [Text])]
                firstKey = toText $ takeFileName $ takeDirectory checkpoint
            -- A second real preparation has its own identity and no archived copy yet.
            writeFileBS script "exec make prepared-app\n# second cache identity\n"
            other <- runEff (Workspace.runWorkspaceReal $ Workspace.runWorkspace options (Workspace.Prepare repository) request) >>= either (fail . toString) (maybe (fail "Second cache missing") (pure . AE.toJSON))
            otherPath <- parse (field "path") other :: IO FilePath
            otherHash <- parse (field "manifest" >=> field "sha256") other :: IO Text
            let otherKey = toText $ takeFileName $ takeDirectory otherPath
                otherPrefix = T.intercalate "/" ["agent-workspaces", "v1", testPid.toText, otherKey]
            modifyIORef' remoteObjects (<> [otherPrefix <> "/" <> otherHash <> ".qcow2", otherPrefix <> "/" <> otherHash <> ".json", otherPrefix <> "/manifest.json"])
            writeFileBS script "exec make prepared-app"
            protected <- runEff $ Workspace.runWorkspaceReal $ Workspace.withWorkspace options request $ \lease -> liftIO do
              isJust lease `shouldBe` True
              report <- prune 0
              actions <- parse (field "entries") report :: IO [AE.Value]
              length actions `shouldBe` 2
              for_ [kept firstKey "pinned", kept otherKey "archive_unavailable"] \expected -> actions `shouldContain` [expected]
              doesFileExist checkpoint `shouldReturn` True
              parse (field "afterBytes") report >>= (\bytes -> (bytes :: Word64) `shouldSatisfy` (> 0))
              pass
            protected `shouldBe` Right ()
            localMetadata <- readFileBS manifestPath
            writeFileLBS manifestPath $ AE.encode $ replace "version" (AE.toJSON (2 :: Int)) relocatedManifest
            invalidCache <- prune 0
            parse (field "entries") invalidCache >>= (\actions -> actions `shouldContain` [kept firstKey "invalid_entry"])
            doesFileExist checkpoint `shouldReturn` True
            writeFileBS manifestPath localMetadata
            overwrite objectKey $ toLazy $ BS.map (`xor` 1) relocatedBytes
            corruptArchive <- prune 0
            parse (field "entries") corruptArchive >>= (\actions -> actions `shouldContain` [kept firstKey "archive_unavailable"])
            doesFileExist checkpoint `shouldReturn` True
            overwrite objectKey $ toLazy relocatedBytes
            void $ prune 0
            doesDirectoryExist (takeDirectory checkpoint) `shouldReturn` False
            doesFileExist otherPath `shouldReturn` True
            (rehydrated, _) <- transfer "restore" cache
            rehydrated `shouldBe` ExitSuccess
            writeFileBS script "exec make prepared-app\n# second cache identity\n"
            (otherArchived, _) <- transfer "archive" cache
            otherArchived `shouldBe` ExitSuccess
            writeFileBS script "exec make prepared-app"
            let parent = takeDirectory $ takeDirectory checkpoint
                unrelated = parent </> "operator-file"
                foreignProject = cache </> toString (UUID.toText $ UUID.fromWords 0 0 0 99)
                directoryLink = parent </> replicate 64 'd'
            createDirectoryIfMissing True foreignProject
            writeFileBS (foreignProject </> "keep") "foreign project"
            writeFileBS unrelated "operator content"
            createFileLink foreignProject directoryLink
            writeFileLBS (takeDirectory checkpoint </> "last-used.json") $ AE.encode $ addUTCTime (-120) frozenTime
            writeFileLBS (takeDirectory otherPath </> "last-used.json") $ AE.encode frozenTime
            preview <- prune maxBound
            allocated <- parse (field "beforeBytes") preview :: IO Word64
            allocated `shouldSatisfy` (> 0)
            reclaimed <- prune (allocated - 1)
            fakeReport <- either fail pure $ AE.eitherDecode $ AE.encode reclaimed
            faked <- runEff $ Workspace.runWorkspaceTest (\_ _ _ -> Left "Unexpected workspace") (\_ _ -> Left Workspace.RegressionUnavailable) (\_ _ -> Right fakeReport) (const $ Left "No maintenance fixture") $ Workspace.pruneWorkspaces (Workspace.PruneOptions "/missing-cache" "/missing-remote") "/missing-request"
            fmap AE.toJSON faked `shouldBe` Right reclaimed
            actions <- parse (field "entries") reclaimed :: IO [AE.Value]
            actions `shouldContain` [AE.object ["tag" AE..= ("Evicted" :: Text), "contents" AE..= firstKey]]
            parse (field "afterBytes") reclaimed >>= (\remaining -> (remaining :: Word64) `shouldSatisfy` (< allocated))
            doesDirectoryExist (takeDirectory checkpoint) `shouldReturn` False
            doesFileExist otherPath `shouldReturn` True
            readFileBS unrelated `shouldReturn` "operator content"
            readFileBS (foreignProject </> "keep") `shouldReturn` "foreign project"
            Posix.isSymbolicLink <$> Posix.getSymbolicLinkStatus directoryLink `shouldReturn` True
            parse (field "unrecognized") reclaimed >>= (\names -> names `shouldContain` ["operator-file" :: FilePath])
            (finalRestore, _) <- transfer "restore" cache
            finalRestore `shouldBe` ExitSuccess
            journals <- listDirectory $ cache </> toString testPid.toText </> "prunes"
            length journals `shouldBe` 6
            for_ journals \name -> do
              record <- readJSON (cache </> toString testPid.toText </> "prunes" </> name)
              parse (field "outcome" >=> field "tag") record `shouldReturn` ("PruneFinished" :: Text)
              parse (field "projectId") record `shouldReturn` testPid
            let maintenanceConfig = directory </> "maintenance.json"
                maintain budget = do
                  writeFileLBS maintenanceConfig $ AE.encode $ AE.object ["cacheDir" AE..= cache, "remote" AE..= remote, "maxBytes" AE..= (budget :: Word64)]
                  (maintenanceStatus, maintenanceOutput) <- CLI.runCLILifecycleLive ["workspace", "maintain", "--config", maintenanceConfig]
                  report <- either fail pure $ AE.eitherDecodeStrict @Workspace.MaintenanceReport $ encodeUtf8 maintenanceOutput
                  maintenanceStatus `shouldBe` if Workspace.maintenanceBudgetMet report then ExitSuccess else ExitFailure 1
                  pure $ AE.toJSON report
            void $ Storage.runMinio tr.trATCtx.s3HttpManager connection $ traverse_ (Minio.removeObject config.s3Bucket) [otherPrefix <> "/" <> otherHash <> ".qcow2", otherPrefix <> "/" <> otherHash <> ".json", otherPrefix <> "/manifest.json"]
            pinned <- runEff $ Workspace.runWorkspaceReal $ Workspace.withWorkspace options request $ \lease -> liftIO do
              isJust lease `shouldBe` True
              report <- maintain 0
              parse (field "afterBytes") report >>= (\bytes -> (bytes :: Word64) `shouldSatisfy` (> 0))
              doesFileExist checkpoint `shouldReturn` True
              doesFileExist otherPath `shouldReturn` False
            pinned `shouldBe` Right ()
            writeFileBS script "exec make prepared-app\n# second cache identity\n"
            (automaticRestore, _) <- transfer "restore" cache
            automaticRestore `shouldBe` ExitSuccess
            sha256 <$> readFileBS otherPath `shouldReturn` otherHash
            writeFileBS script "exec make prepared-app"
            writeFileLBS remote $ AE.encode $ replace "maxBytes" (AE.toJSON (1 :: Word64)) remoteConfiguration
            void $ maintain 0
            doesFileExist checkpoint `shouldReturn` True
            doesFileExist otherPath `shouldReturn` True
            writeFileLBS remote $ AE.encode remoteConfiguration
            roomy <- maintain maxBound
            parse (field "failures") roomy `shouldReturn` ([] :: [AE.Value])
            CLI.withCliBin \cli -> do
              removePathForcibly $ takeDirectory checkpoint
              controlledBridge <- canonicalizePath "test/fixtures/stage-recovery-bridge.sh"
              environment <- getEnvironment
              let ready = directory </> "stage-ready"
                  release = directory </> "stage-release"
                  waitReady = unlessM (doesFileExist ready) (threadDelay 10000 >> waitReady)
                  childEnv = [("GONDOLIN_STAGE_TEST_BRIDGE", bridge), ("GONDOLIN_STAGE_TEST_READY", ready), ("GONDOLIN_STAGE_TEST_RELEASE", release)] <> environment
                  command = setEnv childEnv $ setStderr nullStream $ setStdout nullStream $ proc cli ["workspace", "restore", "--request", request, "--cache", cache, "--image", profile, "--bridge", controlledBridge, "--script", script, "--remote", remote]
              withProcessTerm command \child ->
                ( do
                    Timeout.timeout (15 * 1000000) waitReady `shouldReturn` Just ()
                    candidates <- filter (isPrefixOf ".restore-") <$> listDirectory parent
                    stagedName <- case candidates of [name] -> pure name; _ -> fail "Expected one interrupted restore stage"
                    let staged = parent </> stagedName
                        journal = cache </> toString testPid.toText </> "stages" </> drop (length (".restore-" :: String)) stagedName <> ".json"
                    void $ maintain maxBound
                    doesDirectoryExist staged `shouldReturn` True
                    childPid <- getPid child >>= maybe (fail "Restore exited before interruption") pure
                    Signal.signalProcess Signal.sigKILL childPid
                    Timeout.timeout (5 * 1000000) (waitExitCode child) `shouldReturn` Just (ExitFailure (-9))
                    originalRecord <- readFileBS journal
                    record <- either fail pure $ AE.eitherDecodeStrict @AE.Value originalRecord
                    recordedIdentity <- parse (field "identity") record
                    writeFileLBS journal $ AE.encode $ replace "identity" (replace "projectId" (AE.toJSON $ UUID.fromWords 0 0 0 99) recordedIdentity) record
                    void $ maintain maxBound
                    doesDirectoryExist staged `shouldReturn` True
                    writeFileBS journal originalRecord
                    writeFileBS (staged </> "unknown") "Retain unknown stage content."
                    void $ maintain maxBound
                    readFileBS (staged </> "unknown") `shouldReturn` "Retain unknown stage content."
                    removeFile $ staged </> "unknown"
                    writeFileLBS remote $ AE.encode $ replace "maxBytes" (AE.toJSON (1 :: Word64)) remoteConfiguration
                    unavailable <- maintain maxBound
                    outcomes <- parse (field "recoveries") unavailable :: IO [AE.Value]
                    mapM (parse $ field "outcome" >=> field "tag") outcomes `shouldReturn` (["StageRetained"] :: [Text])
                    doesDirectoryExist staged `shouldReturn` True
                    writeFileLBS remote $ AE.encode remoteConfiguration
                    recovered <- maintain maxBound
                    recoveredRows <- parse (field "recoveries") recovered :: IO [AE.Value]
                    mapM (parse $ field "outcome" >=> field "tag") recoveredRows `shouldReturn` (["StageRemoved"] :: [Text])
                    doesDirectoryExist staged `shouldReturn` False
                    recoveredRecord <- readJSON journal
                    parse (field "phase" >=> field "tag") recoveredRecord `shouldReturn` ("StageRecovered" :: Text)
                    doesFileExist checkpoint `shouldReturn` False
                )
                  `finally` writeFileBS release "release"
              (afterCrash, _) <- transfer "restore" cache
              afterCrash `shouldBe` ExitSuccess
              sha256 <$> readFileBS checkpoint `shouldReturn` relocatedHash
            let stageRecords = cache </> toString testPid.toText </> "stages"
            storedStages <- listDirectory stageRecords >>= traverse (\name -> readJSON (stageRecords </> name))
            expectedIdentity <- parse (field "identity") relocatedManifest :: IO AE.Value
            let retired = filter (\value -> AET.parseMaybe (field "identity") value == Just expectedIdentity && (AET.parseMaybe (field "phase" >=> field "contents" >=> field "tag") value :: Maybe Text) == Just "ArchivedEviction") storedStages
            retiredRecord <- maybe (fail "No retained eviction intent") pure $ viaNonEmpty head retired
            retiredId <- parse (field "id") retiredRecord :: IO Text
            retiredPhase <- parse (field "phase") retiredRecord
            let retiredStage = parent </> (".evict-" <> toString retiredId)
                retiredJournal = stageRecords </> toString retiredId <> ".json"
            createDirectoryIfMissing True $ retiredStage </> "entry"
            copyFile checkpoint $ retiredStage </> "entry" </> "baseline.qcow2"
            -- Resume deletion after its manifest was already unlinked.
            writeFileLBS retiredJournal $ AE.encode $ replace "phase" (replace "tag" (AE.String "StageReclaiming") retiredPhase) retiredRecord
            void $ maintain maxBound
            doesDirectoryExist retiredStage `shouldReturn` False
            sha256 <$> readFileBS checkpoint `shouldReturn` relocatedHash
            fakeMaintenance <- either fail pure $ AE.eitherDecode $ AE.encode roomy
            simulated <- runEff $ Workspace.runWorkspaceTest (\_ _ _ -> Left "Unexpected workspace") (\_ _ -> Left Workspace.RegressionUnavailable) (\_ _ -> Left "Unexpected prune") (const $ Right fakeMaintenance) $ Workspace.maintainWorkspaces "/missing-maintenance"
            fmap AE.toJSON simulated `shouldBe` Right roomy
            readFileBS unrelated `shouldReturn` "operator content"
            readFileBS (foreignProject </> "keep") `shouldReturn` "foreign project"
          )
          `finally` removeObjects
      retainedManifest <- readFileBS manifestPath
      writeFileLBS manifestPath $ AE.encode $ withKey "sha256" (AE.String "invalid-digest") manifest
      isLeft <$> lookupPrepared original options `shouldReturn` True
      writeFileBS manifestPath retainedManifest
      writeFileBS checkpoint "broken checkpoint"
      isLeft <$> lookupPrepared original options `shouldReturn` True

  it "checkout_cleanupWaitsForChildExit_withPinnedSourceAccess_andRebuildsInterruptedCache" \_ -> withTemp "native-checkout" \directory -> do
    let repository = directory </> "repository"
        store = Agent.ArtifactStore directory (directory </> "cache")
        launched = directory </> "worker.log"
        git = gitText repository
    createDirectoryIfMissing True store.cacheDir
    initRepo repository
    writeFileBS (directory </> "host-instructions.md") "Host-only instructions must not enter the prompt.\n"
    createFileLink (directory </> "host-instructions.md") (repository </> "AGENTS.override.md")
    createDirectoryIfMissing True $ repository </> ".agents/skills/linked"
    createFileLink (repository </> "AGENTS.md") (repository </> ".agents/skills/linked/SKILL.md")
    base <- commit repository [("AGENTS.md", "Read actual callers before reviewing.\n"), (".agents/skills/review/SKILL.md", "Committed skill\n"), (".pi/skills/review/reference.md", "Committed reference\n"), (".agents/skills/review/execute.sh", "must not load\n")]
    headRevision <- commit repository [("runtime.txt", "reviewed head\n")]
    void $ commit repository [("runtime.txt", "unreviewed later change\n")]
    writeFiles repository [(".agents/skills/untracked/SKILL.md", "Uncommitted skill\n")]
    recoveredCalls <- newIORef (0 :: Int)
    let request :: Agent.Request "pr_review"
        request = Agent.Request{projectId = testPid, runId = UUIDId $ UUID.fromWords 42 0 0 1, attemptId = UUIDId $ UUID.fromWords 42 0 0 2, repository = toText repository, base, head = headRevision, gitToken = "fixture-secret-token", prompt = "Review actual callers", provider = "fixture", model = "fixture", thinkingLevel = Agent.Off}
        mirror = store.cacheDir </> toString testPid.toText </> toString (show (hash (encodeUtf8 request.repository :: ByteString) :: Digest SHA256) :: Text)
        decode = either (fail . toString) pure . AE.eitherDecodeStrict @AE.Value . encodeUtf8
        at key = AET.parseEither (field key)
        cleaned value = for_ ["directory", "repositorySkills"] $ \key -> parse (field key) value >>= doesDirectoryExist >>= (`shouldBe` False)
        run input flags validate = runEff $ scripted (Just store) "test/fixtures/native-checkout-agent.mjs" (launched : flags) $ Agent.reviewRepository Agent.Isolated input (const $ pure $ Left "Unexpected query") (const $ pure $ Left "Unexpected knowledge") (const $ liftIO $ if "--replay-call" `elem` flags then modifyIORef' recoveredCalls (+ 1) else fail "Unexpected model call") (const $ liftIO $ fail "Unexpected model usage") (const $ liftIO $ fail "Unexpected allocation") (const $ liftIO $ fail "Unexpected check") noProofs validate
        check input flags = run input flags (const $ pure $ Right ()) >>= either (fail . toString) decode
        withRun n input = input{Agent.runId = UUIDId $ UUID.fromWords 42 0 0 n} :: Agent.Request "pr_review"
    preparedFirst <- check request []
    traverse (`at` preparedFirst) ["cacheStatus", "head", "rootInstructions", "instructions"] `shouldBe` Right ["miss", headRevision, "Read actual callers before reviewing.\n", "Read actual callers before reviewing.\n" :: Text]
    parse (field "policy" >=> field "instructions") preparedFirst >>= (`shouldContainAll` [headRevision, "Read actual callers before reviewing."])
    cleaned preparedFirst
    for_ ["protocol", "callback", "cancel"] \mode -> do
      ready <- MVar.newEmptyMVar
      blocked <- MVar.newEmptyMVar :: IO (MVar.MVar ())
      validated <- newIORef False
      let lifetime = directory </> "child-lifetime-" <> mode <> ".json"
          runLifetime =
            runEff
              $ scripted (Just store) "test/fixtures/native-child-lifetime.mjs" [lifetime, mode]
              $ Agent.reviewRepository
                Agent.Isolated
                request
                (const $ liftIO (MVar.putMVar ready () >> MVar.takeMVar blocked) $> Right AE.Null)
                (const $ pure $ Left "Unexpected knowledge")
                (const $ liftIO $ fail "Unexpected model call")
                (const $ liftIO $ fail "Unexpected usage")
                (const $ liftIO $ fail "Unexpected allocation")
                (const $ liftIO $ fail "Unexpected check")
                noProofs
                (const $ liftIO $ writeIORef validated True >> throwIO (userError "Fixture validation failed"))
      if mode == "cancel"
        then withAsync runLifetime \worker -> do
          Timeout.timeout (10 * 1000000) (MVar.takeMVar ready) `shouldReturn` Just ()
          cancel worker
          waitCatch worker >>= (`shouldSatisfy` isLeft)
        else runLifetime >>= (`shouldSatisfy` isLeft)
      readIORef validated `shouldReturn` (mode == "callback")
      observedLifetime <- readJSON lifetime
      traverse (`at` observedLifetime) ["checkoutPresent", "metadataPresent"] `shouldBe` Right [True, True]
      cleaned observedLifetime
    -- planProofs_reachesTheReviewThroughTheHost: the agent's plan arrives at the progress hook, and the reply says so.
    plans <- newIORef ([] :: [[Text]])
    let planReply = directory </> "plan-reply.json"
        planned = \case Agent.Planned claims -> modifyIORef' plans (<> [toList claims]); (Agent.Started{}; Agent.Recorded{}; Agent.Unproved{}) -> pass
    void $ runEff $ scripted (Just store) "test/fixtures/native-child-lifetime.mjs" [planReply, "plan"] $ Agent.reviewRepository Agent.Isolated request (const $ pure $ Left "Unexpected query") (const $ pure $ Left "Unexpected knowledge") (const pass) (const pass) (const pass) (const pass) Agent.ProofHooks{save = \_ _ -> pure $ Left "Unexpected proof", progress = liftIO . planned} (const $ pure $ Right ())
    readIORef plans `shouldReturn` [["Shows the retry count", "Retries once"]]
    readJSON planReply `shouldReturn` AE.object ["Right" AE..= AE.object ["planned" AE..= (2 :: Int)]]
    (preparedSecond, preparedThird) <- concurrently (check (withRun 2 request) []) (check (withRun 3 request) [])
    for_ [preparedSecond, preparedThird] \value -> do
      at "cacheStatus" value `shouldBe` Right ("hit" :: Text)
      cleaned value
    -- A host crash can retain objects while losing the mirror's Git metadata.
    for_ ([("HEAD", ""), ("config", "not git configuration\n")] :: [(FilePath, ByteString)]) $ \(path, bytes) -> do
      writeFileBS (mirror </> path) bytes
      rebuilt <- check request []
      traverse (`at` rebuilt) ["head", "cacheStatus"] `shouldBe` Right [headRevision, "miss" :: Text]
      cleaned rebuilt
      reused <- check request []
      at "cacheStatus" reused `shouldBe` Right ("hit" :: Text)
      cleaned reused
    let isolated = request{Agent.projectId = UUIDId $ UUID.fromWords 42 0 0 3} :: Agent.Request "pr_review"
    (isolatedFirst, isolatedSecond) <- concurrently (check (withRun 4 isolated) []) (check (withRun 5 isolated) [])
    sort <$> traverse (parse $ field "cacheStatus") [isolatedFirst, isolatedSecond] `shouldReturn` (["hit", "miss"] :: [Text])
    let otherRepository = directory </> "other-repository"
    void $ git ["clone", "--quiet", "--no-local", "--", repository, otherRepository]
    different <- run (request{Agent.repository = toText otherRepository} :: Agent.Request "pr_review") [] (const $ pure $ Right ()) >>= either (fail . toString) decode
    at "cacheStatus" different `shouldBe` Right ("miss" :: Text)
    observed <- newIORef Nothing
    failed <- run request ["--fail"] (\value -> liftIO (writeIORef observed $ Just value) $> Right ())
    failed `shouldSatisfy` isLeft
    captured <- readIORef observed >>= maybe (fail "Checkout validation was not reached") pure
    cleaned captured
    removeFile $ repository </> "AGENTS.override.md"
    writeFileBS (repository </> "AGENTS.override.md") $ encodeUtf8 $ T.replicate 3000 "Override rule.\n"
    void $ git ["add", "AGENTS.override.md"]
    void $ git ["commit", "--quiet", "-m", "override repository instructions"]
    overrideRevision <- git ["rev-parse", "HEAD"]
    overridden <- check (request{Agent.head = overrideRevision} :: Agent.Request "pr_review") ["--override-instructions"]
    at "rootInstructions" overridden `shouldBe` Right (T.take 32768 $ T.replicate 3000 "Override rule.\n")
    cleaned overridden
    -- Later unrelated runs must not make a retained base disappear when a
    -- verifier clones the shared mirror through Git's file transport.
    void $ git ["checkout", "--quiet", "--orphan", "unrelated"]
    void $ git ["commit", "--quiet", "--allow-empty", "-m", "unrelated history"]
    unrelated <- git ["rev-parse", "HEAD"]
    run (request{Agent.base = unrelated, Agent.head = unrelated} :: Agent.Request "pr_review") ["--override-instructions"] (const $ pure $ Right ()) >>= (`shouldSatisfy` isRight)
    let snapshot = directory </> "mirror-snapshot"
    void $ git ["clone", "--quiet", "--no-local", "--no-checkout", "--", mirror, snapshot]
    retainedBase <- tryAny $ git ["-C", snapshot, "cat-file", "-e", toString base <> "^{commit}"]
    isRight retainedBase `shouldBe` True
    writeFileBS (repository </> ".agents/skills/review/oversized.md") $ BS.replicate (1048576 + 1) 97
    void $ git ["add", ".agents/skills/review/oversized.md"]
    void $ git ["commit", "--quiet", "-m", "oversized skill metadata"]
    oversized <- git ["rev-parse", "HEAD"]
    run (request{Agent.base = unrelated, Agent.head = oversized} :: Agent.Request "pr_review") ["--override-instructions"] (const $ pure $ Right ()) >>= (`shouldSatisfy` isLeft)
    launchesBefore <- readFileBS launched
    run request ["--skip-checkout"] (const $ liftIO $ fail "Validation bypassed native checkout") >>= (`shouldSatisfy` isLeft)
    for_ @[] [request{Agent.base = "--not-a-revision"} :: Agent.Request "pr_review", request{Agent.repository = "https://user:secret@github.com/org/repo.git"} :: Agent.Request "pr_review"] \invalid -> do
      run invalid [] (const $ liftIO $ fail "Invalid source reached validation") >>= (`shouldSatisfy` isLeft)
    renameDirectory repository (repository <> "-unavailable")
    denied <- run request ["--replay-call"] (const $ liftIO $ fail "Cached objects bypassed source access") `finally` renameDirectory (repository <> "-unavailable") repository
    denied `shouldSatisfy` isLeft
    readIORef recoveredCalls `shouldReturn` 1
    readFileBS launched `shouldReturn` launchesBefore

  it "journals native fix phases before delivery and replays rejected or interrupted writes" \_ -> withSystemTempDirectory "native-phase-delivery" \directory -> do
    let uid n = UUIDId $ UUID.fromWords 56 0x4000 0x80000000 n
        store = Agent.ArtifactStore directory (directory </> "cache")
        attempt = uid 1
    Outbox.withOutbox store (Outbox.IssueFix $ uid 2) \outbox -> do
      let pendingFiles = filter (T.isSuffixOf ".json" . toText) <$> listDirectory (Outbox.outboxPath outbox)
          inspect receipt = case receipt of
            Outbox.Phase phase -> do
              phase.attemptId `shouldBe` attempt
              AE.toJSON phase.step `shouldBe` AE.toJSON Fixes.RestartReproduction
              let path = Outbox.outboxPath outbox </> toString phase.id.toText <> ".json"
              AE.eitherDecodeStrict <$> readFileBS path `shouldReturn` Right (AE.toJSON receipt)
              permissions <- Posix.fileMode <$> Posix.getFileStatus path
              Posix.intersectFileModes permissions 0o777 `shouldBe` 0o600
            _ -> expectationFailure "Native phase publisher emitted a different receipt kind"
          replay expected = do
            observed <- newIORef ([] :: [AE.Value])
            Outbox.replay outbox (\receipt -> inspect receipt >> modifyIORef' observed (<> [AE.toJSON receipt]) $> Right ()) `shouldReturn` Right ()
            readIORef observed `shouldReturn` [expected]
            pendingFiles `shouldReturn` []
            Outbox.replay outbox (const $ fail "Acknowledged native phase replayed twice") `shouldReturn` Right ()
      for_ [pure $ Left ("Phase store rejected delivery" :: Text), throwIO $ userError "Phase store lost its response"] \reject -> do
        saved <- newIORef Nothing
        outcome <- tryAny $ Outbox.deliverPhase outbox attempt Fixes.RestartReproduction \receipt -> inspect receipt >> writeIORef saved (Just receipt) >> reject
        outcome `shouldSatisfy` either (const True) isLeft
        original <- readIORef saved >>= maybe (fail "Native phase callback did not run") pure
        Outbox.deliver outbox original (const $ pure $ Left "Retry still rejected") `shouldReturn` Left "Retry still rejected"
        case original of
          Outbox.Phase receipt -> do
            called <- newIORef False
            let conflicting = Outbox.Phase $ Agent.PhaseReceipt receipt.id (uid 99) receipt.step
            isLeft <$> tryAny (Outbox.deliver outbox conflicting $ const $ writeIORef called True $> Right ()) `shouldReturn` True
            readIORef called `shouldReturn` False
          _ -> expectationFailure "Native phase publisher returned another receipt kind"
        inspect original
        replay $ AE.toJSON original
      ready <- MVar.newEmptyMVar
      blocked <- MVar.newEmptyMVar :: IO (MVar.MVar ())
      withAsync (Outbox.deliverPhase outbox attempt Fixes.RestartReproduction \receipt -> inspect receipt >> MVar.putMVar ready (AE.toJSON receipt) >> MVar.takeMVar blocked $> Right ()) \worker -> do
        expected <- Timeout.timeout (5 * 1000000) (MVar.takeMVar ready) >>= maybe (fail "Native phase delivery did not reach its callback") pure
        cancel worker
        isLeft <$> waitCatch worker `shouldReturn` True
        replay expected
    Outbox.withOutbox store (Outbox.Admission $ uid 3) \outbox -> do
      called <- newIORef False
      isLeft <$> tryAny (Outbox.deliverPhase outbox attempt Fixes.RestartReproduction $ const $ writeIORef called True $> Right ()) `shouldReturn` True
      readIORef called `shouldReturn` False
      filter (T.isSuffixOf ".json" . toText) <$> listDirectory (Outbox.outboxPath outbox) `shouldReturn` []

  it "recovers durable receipts in Haskell before starting the worker and retains rejected usage" \_ -> withTemp "native-receipts" \directory -> do
    let uid n = UUIDId $ UUID.fromWords 43 0 0 n
        store = Agent.ArtifactStore directory (directory </> "cache")
        request = Agent.Request{projectId = testPid, runId = uid 1, attemptId = uid 2, repository = toText $ directory </> "unavailable-source", base = revision, head = revision, gitToken = "", prompt = "Recover recorded work", provider = "fixture", model = "fixture", thinkingLevel = Agent.Off}
        fixRequest = Agent.FixRequest request (uid 3) (uid 4)
        call = Agent.ModelCall{id = uid 99, attemptId = request.attemptId, provider = "fixture", model = "fixture"}
        usage = Agent.ModelUsage{id = uid 50, attemptId = request.attemptId, callId = Just call.id, provider = "fixture", model = "fixture", responseId = Nothing, inputTokens = 7, outputTokens = 3, cacheReadTokens = 0, cacheWriteTokens = 0, costNanousd = Nothing, pricing = AE.Null, succeeded = False}
        phase = Agent.PhaseReceipt{id = uid 5, attemptId = request.attemptId, step = Fixes.RestartReproduction}
        check = Agent.CheckReceipt{id = uid 60, attemptId = request.attemptId, checkId = uid 61, side = Agent.Base, revision, imageBuildId = "fixture", scriptSha256 = T.replicate 64 "a", regression = Nothing, status = Agent.Failed, exitCode = Just 1, durationMs = 1, cpus = 2, memoryMiB = 1024, logBytes = 0, logSha256 = T.replicate 64 "b", workspace = Nothing}
        allocation = AE.object ["id" AE..= (uid 70 :: UUIDId "agent_allocation"), "attemptId" AE..= request.attemptId, "durationMs" AE..= (10 :: Int), "cpus" AE..= (2 :: Int), "memoryMiB" AE..= (1024 :: Int), "outcome" AE..= ("released" :: Text)]
        outbox = directory </> "fixes" </> toString request.runId.toText
        path identifier = outbox </> toString identifier <> ".json"
        marker = directory </> "worker"
    createDirectoryIfMissing True outbox
    writeFileLBS (path call.id.toText) $ AE.encode $ AE.object ["tag" AE..= ("Call" :: Text), "contents" AE..= call]
    writeFileLBS (path usage.id.toText) $ AE.encode usage
    writeFileLBS (path phase.id.toText) $ AE.encode $ AE.object ["tag" AE..= ("Phase" :: Text), "contents" AE..= phase]
    writeFileLBS (path check.id.toText) $ AE.encode $ AE.object ["tag" AE..= ("Check" :: Text), "contents" AE..= check]
    writeFileLBS (path (uid 70 :: UUIDId "agent_allocation").toText) $ AE.encode $ AE.object ["tag" AE..= ("Allocation" :: Text), "contents" AE..= allocation]
    let otherRun = directory </> "fixes" </> toString (uid 100 :: UUIDId "issue_fix").toText
    createDirectoryIfMissing True otherRun
    writeFileLBS (otherRun </> "unrelated.json") $ AE.encode usage
    denied <- newIORef True
    events <- newIORef ([] :: [Text])
    let record label identifier = liftIO do
          tryAny (readFileBS $ marker <> ".process") >>= (`shouldSatisfy` isLeft)
          readFileBS (path identifier) >>= (`shouldSatisfy` (not . BS.null))
          modifyIORef' events (<> [label])
        run flags =
          runEff
            $ scripted (Just store) "test/fixtures/native-checkout-agent.mjs" (marker : flags)
            $ Agent.fixRepository
              Agent.Isolated
              fixRequest
              (const $ pure $ Left "No query")
              (const $ pure $ Left "No knowledge")
              (const $ pure $ Left "No admission")
              (\receipt -> record "Call" receipt.id.toText)
              ( \receipt -> do
                  record "Usage" receipt.id.toText
                  liftIO $ readIORef denied >>= \reject -> when reject $ throwIO $ userError "Usage store unavailable"
              )
              (\receipt -> record "Allocation" (Agent.allocationId receipt).toText)
              (\receipt -> record "Check" receipt.id.toText)
              (\receipt -> record "Phase" receipt.id.toText $> Right ())
              (const $ pure $ Left "No candidate")
    isLeft <$> run [] `shouldReturn` True
    readIORef events `shouldReturn` ["Call", "Usage"]
    tryAny (readFileBS $ path call.id.toText) >>= (`shouldSatisfy` isLeft)
    readFileBS (path usage.id.toText) >>= (`shouldSatisfy` (not . BS.null))
    writeIORef events []
    writeIORef denied False
    isLeft <$> run [] `shouldReturn` True
    readIORef events `shouldReturn` ["Usage", "Check", "Allocation", "Phase"]
    for_ [usage.id.toText, check.id.toText, (uid 70 :: UUIDId "agent_allocation").toText, phase.id.toText] \identifier -> tryAny (readFileBS $ path identifier) >>= (`shouldSatisfy` isLeft)
    readFileBS (marker <> ".process") `shouldReturn` "started\n"
    readFileBS (otherRun </> "unrelated.json") `shouldReturn` toStrict (AE.encode usage)
    removeFile $ marker <> ".process"
    writeIORef events []
    isLeft <$> run ["--journal-exit"] `shouldReturn` True
    readIORef events `shouldReturn` []
    removeFile $ marker <> ".process"
    isLeft <$> run [] `shouldReturn` True
    readIORef events `shouldReturn` ["Call"]
    removeFile $ marker <> ".process"
    writeIORef events []
    for_ ["{", toStrict $ AE.encode usage] \bytes -> do
      writeFileBS (path call.id.toText) bytes
      isLeft <$> run [] `shouldReturn` True
      readIORef events `shouldReturn` []
      tryAny (readFileBS $ marker <> ".process") >>= (`shouldSatisfy` isLeft)
    removeFile $ path call.id.toText
    createFileLink (otherRun </> "unrelated.json") (path usage.id.toText)
    isLeft <$> run [] `shouldReturn` True
    readIORef events `shouldReturn` []
    removeFile $ path usage.id.toText

    let admissionOutbox = directory </> "admissions" </> toString request.runId.toText
    createDirectoryIfMissing True admissionOutbox
    for_ [(call.id.toText, AE.object ["tag" AE..= ("Call" :: Text), "contents" AE..= call]), (phase.id.toText, AE.object ["tag" AE..= ("Phase" :: Text), "contents" AE..= phase])] \(identifier, receipt) ->
      writeFileLBS (admissionOutbox </> toString identifier <> ".json") $ AE.encode receipt
    refused <-
      runEff
        $ scripted (Just store) "test/fixtures/native-checkout-agent.mjs" [marker]
        $ Agent.admitRegression
          Agent.Isolated
          (Agent.AdmissionRequest fixRequest $ Agent.TestArtifact "false" "test patch" "failure")
          (const $ pure $ Left "Unexpected memory request")
          (const $ liftIO $ modifyIORef' events (<> ["Forbidden admission replay"]))
          (const pass)
          (const pass)
    isLeft refused `shouldBe` True
    readIORef events `shouldReturn` []
    tryAny (readFileBS $ marker <> ".process") >>= (`shouldSatisfy` isLeft)

  it "nativePublication_rechecksProjectEligibilityWhileVerifyingArtifactsAndPublishingWithoutAWorker" \tr -> withTemp "native-artifacts" \directory -> do
    let repository = directory </> "repository"
        store = Agent.ArtifactStore (directory </> "usage") (directory </> "cache")
        remoteRepository = "https://github.com/impact-org/checkout.git" :: Text
        uid n = UUIDId $ UUID.fromWords 0 0 0 n
        git = gitIn repository
        revisionAt = gitText repository ["rev-parse", "HEAD"] >>= fixRevision
        script = "bash regression.sh" :: ByteString
        test = "set -eu\n. ./runtime.sh\n[ \"$(retry_token original)\" = original ] || { echo \"retry token lost\"; exit 1; }\n" :: ByteString
        metadata result = case AE.toJSON result of AE.Object fields -> AE.Object $ foldr KM.delete fields ["checkId", "regressionSha256", "summary"]; value -> value
    initRepo repository
    base <- commit repository [("runtime.sh", "retry_token() { printf 'lost\\n'; }\n")] >>= fixRevision
    let mirror = store.cacheDir </> toString testPid.toText </> toString (sha256 $ encodeUtf8 remoteRepository)
    createDirectoryIfMissing True $ store.cacheDir </> toString testPid.toText
    void $ git ["clone", "--quiet", "--mirror", repository, mirror]
    writeFileBS (repository </> "regression.sh") test
    void $ git ["add", "."]
    testPatch <- git ["diff", "--cached", "--binary", "--no-ext-diff", "--no-textconv"]
    writeFileBS (repository </> "runtime.sh") "retry_token() { printf '%s\\n' \"$1\"; }\n"
    void $ git ["add", "."]
    patch <- git ["diff", "--cached", "--binary", "--no-ext-diff", "--no-textconv"]
    void $ git ["commit", "--quiet", "-m", "retain regression and fix"]
    candidateRevision <- revisionAt
    let candidateId = uid 4 :: UUIDId "fix_candidate"
        ref = "refs/heads/monoscope-candidate/" <> toString candidateId.toText
    void $ git ["update-ref", ref, toString $ Fixes.revisionText candidateRevision]
    void $ git ["bundle", "create", directory </> "candidate.bundle", ref, "^" <> toString (Fixes.revisionText base)]
    bundle <- readFileBS $ directory </> "candidate.bundle"
    -- Literal wire ordering matches JSON.stringify used by the existing worker.
    let frozen = "{\"script\":" <> toStrict (AE.encode $ decodeUtf8 @Text script) <> ",\"testPatch\":" <> toStrict (AE.encode $ decodeUtf8 @Text testPatch) <> ",\"failureMessage\":\"retry token lost\"}"
        result = Agent.FixResult{projectId = testPid, runId = uid 1, issueId = uid 2, attemptId = uid 3, candidateId, base, head = candidateRevision, patchSha256 = sha256 patch, bundleSha256 = sha256 bundle, rootCause = "Runtime drops retry token <code> --> & test", checkId = uid 5, regressionSha256 = sha256 frozen, summary = "Preserve retry tokens"}
        baselineLog = "retry token lost\n\nliteral <code> --> & test\n" <> BS.pack [0, 255] :: ByteString
        candidateLog = "regression-passed\n" :: ByteString
        baseline = Agent.CheckReceipt{id = uid 6, attemptId = result.attemptId, checkId = result.checkId, side = Agent.Base, revision = Fixes.revisionText base, imageBuildId = (uid 8 :: UUIDId "image").toText, scriptSha256 = sha256 script, regression = Just $ Agent.InjectedTest result.regressionSha256, status = Agent.Failed, exitCode = Just 1, durationMs = 10, cpus = 2, memoryMiB = 1024, logBytes = fromIntegral $ BS.length baselineLog, logSha256 = sha256 baselineLog, workspace = Nothing}
        candidate = baseline{Agent.id = uid 7, Agent.side = Agent.Head, Agent.revision = Fixes.revisionText candidateRevision, Agent.regression = Just $ Agent.RetainedTest result.regressionSha256, Agent.status = Agent.Passed, Agent.exitCode = Just 0, Agent.logBytes = fromIntegral $ BS.length candidateLog, Agent.logSha256 = sha256 candidateLog}
        request = Agent.ArtifactRequest remoteRepository result baseline candidate
        row (receipt :: Agent.CheckReceipt) disposition = AE.object ["id" AE..= receipt.id, "side" AE..= receipt.side, "revision" AE..= receipt.revision, "status" AE..= receipt.status, "exitCode" AE..= receipt.exitCode, "configuredResources" AE..= AE.object ["cpus" AE..= receipt.cpus, "memoryMiB" AE..= receipt.memoryMiB], "durationMs" AE..= (9.5 :: Double), "logBytes" AE..= receipt.logBytes, "logSha256" AE..= receipt.logSha256, "sourceBefore" AE..= sha256 "source", "sourceAfter" AE..= sha256 "source", "testFilesSha256" AE..= sha256 test, "testPatchDisposition" AE..= (disposition :: Text)]
        report = AE.object ["id" AE..= result.checkId, "projectId" AE..= testPid, "runId" AE..= result.runId, "attemptId" AE..= result.attemptId, "imageBuildId" AE..= baseline.imageBuildId, "scriptSha256" AE..= sha256 script, "testPatchSha256" AE..= sha256 testPatch, "regressionSha256" AE..= result.regressionSha256, "failureMessage" AE..= ("retry token lost" :: Text), "status" AE..= ("completed" :: Text), "results" AE..= ([row baseline "injected", row candidate "retained"] :: [AE.Value])]
        checks = store.usageDir </> "fixes" </> toString result.runId.toText </> "checks"
        captured = checks </> "candidates" </> toString candidateId.toText
        execution = checks </> toString result.checkId.toText
        files = [(captured </> "candidate.json", toStrict $ AE.encode $ metadata result), (captured </> "fix.patch", patch), (captured </> "candidate.bundle", bundle), (execution </> "report.json", toStrict $ AE.encode report), (execution </> "script.sh", script), (execution </> "test.patch", testPatch), (execution </> "base.log", baselineLog), (execution </> "head.log", candidateLog)]
        restore = traverse_ (uncurry writeFileBS) files
        verify input = runEff $ Agent.runCodeReviewReal Nothing (Right Nothing) Nothing [] $ Agent.verifyFixArtifacts store input
    -- A child's acknowledgement never stands in for retained files that are absent.
    isLeft <$> runEff (scripted Nothing "test/fixtures/issue-fix-agent.mjs" [directory </> "failure.txt", toString baseline.imageBuildId, ""] $ Agent.verifyFixArtifacts store request) `shouldReturn` True
    createDirectoryIfMissing True captured
    createDirectoryIfMissing True execution
    restore
    fmap Agent.verifiedFixResult <$> verify request `shouldReturn` Right result
    snapshot <- Checkout.withCheckout store (Checkout.VerifiedCandidate request) (\prepared evidence -> pure (AE.toJSON prepared, evidence))
    (_, preparedFiles) <- either (fail . toString) pure snapshot
    let reviewInput = AE.object ["result" AE..= result, "regression" AE..= Agent.TestArtifact (decodeUtf8 script) (decodeUtf8 testPatch) "retry token lost", "baseline" AE..= request.baseline, "candidate" AE..= request.candidate]
        peerFiles = Map.fromList [("/run/monoscope-fix-review.json", toStrict $ AE.encode reviewInput), ("/run/monoscope-fix-review-base.log", baselineLog), ("/run/monoscope-fix-review-head.log", candidateLog)] :: Map Text ByteString
    preparedFiles `shouldBe` peerFiles
    for_ files \(path, bytes) -> do
      writeFileBS path (bytes <> "corruption")
      isLeft <$> verify request `shouldReturn` True
      restore
    removeFile $ execution </> "head.log"
    isLeft <$> verify request `shouldReturn` True
    createFileLink (execution </> "base.log") (execution </> "head.log")
    isLeft <$> verify request `shouldReturn` True
    removeFile $ execution </> "head.log"
    restore
    for_ @[] [("status" :: AE.Key, AE.String "running"), ("imageBuildId", AE.String (uid 9 :: UUIDId "image").toText), ("results", AE.toJSON ([] :: [AE.Value]))] \(key, value) -> do
      writeFileLBS (execution </> "report.json") $ AE.encode $ withKey key value report
      isLeft <$> verify request `shouldReturn` True
      restore
    let (prefix, suffix) = BS.splitAt (BS.length bundle - 1) bundle
        corruptBundle = prefix <> BS.map (`xor` 1) suffix
    for_ [result{Agent.patchSha256 = sha256 testPatch}, result{Agent.bundleSha256 = sha256 corruptBundle}] \changed -> do
      writeFileBS (captured </> "candidate.json") $ toStrict $ AE.encode $ metadata changed
      writeFileBS (captured </> "fix.patch") $ if changed.patchSha256 /= result.patchSha256 then testPatch else patch
      writeFileBS (captured </> "candidate.bundle") $ if changed.bundleSha256 /= result.bundleSha256 then corruptBundle else bundle
      isLeft <$> verify request{Agent.result = changed} `shouldReturn` True
      restore
    void $ git ["bundle", "create", directory </> "full.bundle", ref]
    fullBundle <- readFileBS $ directory </> "full.bundle"
    let different = result{Agent.bundleSha256 = sha256 fullBundle}
    writeFileBS (captured </> "candidate.bundle") fullBundle
    writeFileLBS (captured </> "candidate.json") $ AE.encode $ metadata different
    isLeft <$> verify request{Agent.result = different} `shouldReturn` True
    restore
    differentWorkspace <- parse AE.parseJSON $ withKey "workspace" (AE.String $ T.replicate 64 "f") $ AE.toJSON candidate
    for_ [request{Agent.result = result{Agent.runId = uid 99}}, request{Agent.candidate = candidate{Agent.attemptId = uid 99}}, request{Agent.candidate = differentWorkspace}] \changed ->
      isLeft <$> verify changed `shouldReturn` True
    fmap Agent.verifiedFixResult <$> verify request `shouldReturn` Right result
    let render = runEff $ Agent.runCodeReviewReal Nothing (Right Nothing) Nothing [] $ runExceptT do
          verified <- ExceptT $ Agent.verifyFixArtifacts store request
          ExceptT $ Agent.explainVerifiedFix store request verified
    rendered <- render
    explained <- either (fail . toString) pure rendered
    let manifest = Agent.fixExplanation explained
        media = captured </> "explanations" </> toString manifest.source
    Agent.validExplanation result manifest `shouldBe` True
    for_ (Map.toList manifest.files) \(name, digest) -> do
      bytes <- Media.readExplanationMedia store.usageDir manifest name >>= either (fail . toString) pure
      sha256 bytes `shouldBe` digest
    captions <- readFileBS $ media </> "captions.vtt"
    decodeUtf8 @Text captions `shouldSatisfy` T.isInfixOf "&lt;code&gt; --&gt; &amp; test"
    probe <- readProcessStdout_ $ proc "ffprobe" ["-v", "error", "-show_streams", "-show_format", "-of", "json", media </> "explanation.mp4"]
    let stream = AE.withObject "Video probe" \fields -> do
          streams <- fields AE..: "streams"
          forM (streams :: [AE.Value]) $ AE.withObject "Video stream" \entry -> (,,) <$> entry AE..: "codec_name" <*> entry AE..: "width" <*> entry AE..: "height"
    (AET.parseEither stream =<< AE.eitherDecode probe) `shouldBe` Right [("h264" :: Text, 1280 :: Int, 720 :: Int)]
    decoded <- readProcessStdout_ $ proc "ffmpeg" ["-hide_banner", "-loglevel", "error", "-i", media </> "captions.vtt", "-f", "webvtt", "pipe:1"]
    decodeUtf8 @Text decoded `shouldSatisfy` T.isInfixOf "retry token lost"
    isLeft <$> Media.readExplanationMedia store.usageDir manifest{Agent.projectId = uid 99} "explanation.mp4" `shouldReturn` True
    isLeft <$> Media.readExplanationMedia store.usageDir manifest "../candidate.bundle" `shouldReturn` True
    fmap Agent.fixExplanation <$> render `shouldReturn` Right manifest
    writeFileBS (media </> "explanation.mp4") "corrupted video"
    isLeft <$> Media.readExplanationMedia store.usageDir manifest "explanation.mp4" `shouldReturn` True
    (firstRepair, secondRepair) <- concurrently render render
    repaired <- either (fail . toString) (pure . Agent.fixExplanation) firstRepair
    fmap Agent.fixExplanation secondRepair `shouldBe` Right repaired
    repaired.source `shouldBe` manifest.source
    isRight <$> Media.readExplanationMedia store.usageDir repaired "explanation.mp4" `shouldReturn` True
    writeFileLBS (media </> "manifest.json") $ AE.encode repaired{Agent.renderMs = 0}
    cacheRepair <- render >>= either (fail . toString) (pure . Agent.fixExplanation)
    Agent.validExplanation result cacheRepair `shouldBe` True
    cacheRepair.source `shouldBe` manifest.source
    for_ [" ", "bad\NULcause", T.replicate 241 "x"] \cause -> do
      let changed = result{Agent.rootCause = cause}
      writeFileBS (captured </> "candidate.json") $ toStrict $ AE.encode $ metadata changed
      isLeft <$> Media.renderExplanation store request{Agent.result = changed} `shouldReturn` True
      restore

    -- Build the normal DB capability from the same retained native fixture.
    setup tr
    [mapping] <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
    let user = (getResponse tr.trSessAndHeader :: Projects.Session).user.id
    dbExec tr "INSERT INTO apis.issues (id, project_id, issue_type, service, target_hash, title) VALUES (?, ?, 'runtime_exception', 'test-service', 'native-publication', 'Retain retry tokens')" (result.issueId, testPid)
    runQueryEffect tr (Fixes.createRun testPid result.runId result.issueId mapping.id base (uid 8)) `shouldReturn` Just result.runId
    for_ (zip [0 ..] [Fixes.Observe baseline{Agent.id = uid 10, Agent.checkId = uid 11}, Fixes.ProposeCandidate (Fixes.Candidate result.head result.rootCause), Fixes.Observe baseline, Fixes.Observe candidate]) \(version, step) -> do
      progressed <- runQueryEffect tr $ Fixes.recordStep testPid result.runId version (uid $ 20 + fromIntegral version) step
      progressed `shouldSatisfy` isRight
    runQueryEffect tr (Fixes.queueJob testPid user result.runId) `shouldReturn` True
    peer <- maybe (fail "Native publication review attempt missing") pure =<< runQueryEffect tr (Usage.startAttempt testPid (Usage.Fix result.runId) "fixture" "native-publication")
    dbExec tr "UPDATE projects.issue_fix_jobs SET result = ?, verified_result = ?, investigation = '{}'::jsonb, checkpoint_investigation = '{}'::jsonb, explanation = ?, media_retained_at = now(), review_attempt_id = ?, review_rationale = 'Fixture independent approval' WHERE run_id = ?" (Aeson result, Aeson result, Aeson cacheRepair, peer, result.runId)
    completed <- maybe (fail "Native publication capability missing") pure =<< runQueryEffect tr (Fixes.publicationForUser testPid result.runId user)
    -- The publish job tries the project's personal tokens first; one that cannot push falls back to the App
    -- installation, whose pull request links Monoscope-signed media. The issue records the PR once.
    void $ runQueryEffect tr $ GitSync.upsertGitHubCredential (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "ada" Nothing (Just "ghp_fixture")
    app <- githubApp tr
    attempts <- newIORef []
    let opened = Publication.Receipt 7 "https://github.com/impact-org/checkout/pull/7" result.head "source" "media" (Publication.MediaUrls "video" "diagram") True Publication.Open
        publisher = interpret \env -> \case
          Publication.PublishFix _ credential _ accept -> localSeqUnliftIO env \unlift -> do
            modifyIORef' attempts (Publication.linkedMedia credential :)
            maybe (pure $ Left Publication.AccessRevoked) (\_ -> Left Publication.NotConfigured <$ unlift (accept opened)) (Publication.linkedMedia credential)
        publishJob = void $ runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = app}) $ githubResponses (\_ -> pure $ Just $ AE.object ["full_name" AE..= ("impact-org/checkout" :: Text), "visibility" AE..= ("private" :: Text), "permissions" AE..= AE.object ["push" AE..= True]]) $ publisher $ Impact.publishIssueFix testPid result.runId
    publishJob >> publishJob
    tried <- readIORef attempts
    map isJust tried `shouldBe` [True, False, True, False]
    for_ (catMaybes tried) \urls -> (urls.video, urls.diagram) `shouldSatisfy` \(video, diagram) -> all (T.isInfixOf ("/issue-fixes/" <> result.runId.toText <> "/shared/")) [video, diagram] && "/shared/video?sig=" `T.isInfixOf` video && "/shared/cause?sig=" `T.isInfixOf` diagram
    dbOne tr "SELECT count(*)::int FROM apis.issue_activity_log WHERE issue_id = ? AND event = 'linked' AND metadata->>'url' = ?" (result.issueId, opened.url) `shouldReturn` (1 :: Int)
    publication <- either (fail . show) pure $ Publication.requestFor completed "main" Publication.Private "https://monoscope.example"
    credential <- either (fail . show) pure $ Publication.uploadCredential $ GitSync.PersonalToken "ghp_fixture"
    stages <- newIORef []
    accepted <- newIORef []
    httpRequests <- newIORef []
    changedRemoteBody <- newIORef False
    let github = directory </> "gh"
        stateFile = directory </> "github.json"
        branch = "monoscope/fix/" <> testPid.toText <> "/" <> result.runId.toText <> "/" <> result.candidateId.toText
        initial = AE.object ["base" AE..= Fixes.revisionText base, "head" AE..= Fixes.revisionText result.head, "branch" AE..= branch, "writes" AE..= ([] :: [Text])]
        readState = readJSON stateFile
        resetRemote = removePathForcibly (store.usageDir </> "fixes" </> toString result.runId.toText </> "publications-native") >> writeFileLBS stateFile (AE.encode initial)
        fixture _ url = do
          remote <- readState
          changed <- readIORef changedRemoteBody
          let pr = fromRight AE.Null $ AET.parseEither (field "pr") remote
              confirmed = if changed then case pr of AE.Object fields -> AE.Object $ KM.mapWithKey (\key value -> case (key, value) of ("body", AE.String text) -> AE.String $ text <> "\nEdited explanation"; _ -> value) fields; value -> value else pr
          pure
            $ Just
            $ AE.encode
            $ if "/pulls/7" `T.isSuffixOf` toText url
              then confirmed
              else AE.object ["full_name" AE..= ("impact-org/checkout" :: Text), "visibility" AE..= ("private" :: Text), "permissions" AE..= AE.object ["push" AE..= True]]
        authorize stage = do
          modifyIORef' stages (stage :)
          current <- runQueryEffect tr $ Fixes.publicationForUser testPid result.runId user
          pure $ if isJust current then Right () else Left Publication.AccessRevoked
        runPublisher auth accept = runEff $ HTTP.runHTTPRecord httpRequests $ withHTTPResponses fixture $ Publication.runGitPublicationReal (Just store) (Just github) $ Publication.publishFix publication credential (liftIO . auth) (liftIO . accept)
        validate receipt = modifyIORef' accepted (receipt :) $> Right ()
        writes = readState >>= parse (field "writes")
        setMode mode = readState >>= writeFileLBS stateFile . AE.encode . withKey "mode" (AE.String mode)
        failure = fmap leftToMaybe . runPublisher authorize
        allWrites = ["create", "explanation.mp4", "cause.png"] :: [Text]
    copyFile "test/fixtures/native-publication-gh.mjs" github
    Posix.setFileMode github 0o700
    writeFileLBS stateFile $ AE.encode initial
    published <- runPublisher authorize validate
    fmap (\proof -> (Publication.publicationReceipt proof).head) published `shouldBe` Right result.head
    reverse <$> readIORef stages `shouldReturn` [Publication.Prepare, Publication.Discover, Publication.Branch, Publication.Create, Publication.AttachVideo, Publication.AttachDiagram, Publication.Complete]
    writes `shouldReturn` allWrites
    fmap Publication.publicationReceipt <$> runPublisher authorize validate `shouldReturn` fmap Publication.publicationReceipt published
    writes `shouldReturn` allWrites
    failedAcceptance <- tryAny $ runPublisher authorize (\_ -> throwIO $ userError "Fixture acceptance failure")
    isRight failedAcceptance `shouldBe` True
    either (const Nothing) leftToMaybe failedAcceptance `shouldBe` Just Publication.ProtocolFailure
    writeIORef changedRemoteBody True
    failure (\_ -> fail "Edited remote explanation reached application acceptance") `shouldReturn` Just Publication.InvalidReceipt
    writeIORef changedRemoteBody False
    for_ ([("visibility", Publication.VisibilityChanged), ("denied", Publication.AccessRevoked), ("fork", Publication.PullRequestChanged)] :: [(Text, Publication.Failure)]) \(mode, expected) -> do
      setMode mode
      failure validate `shouldReturn` Just expected
    setMode "merged"
    fmap ((.state) . Publication.publicationReceipt) <$> runPublisher authorize validate `shouldReturn` Right Publication.Closed
    setMode ""
    leftToMaybe <$> runPublisher (const $ pure $ Left Publication.AccessRevoked) validate `shouldReturn` Just Publication.AccessRevoked
    originalPlan <- dbOne tr "SELECT payment_plan FROM projects.projects WHERE id = ?" (Only testPid) :: IO Text
    let earlierOwner = uid 77 :: Projects.UserId
    dbExec tr "INSERT INTO users.users (id, email) VALUES (?, 'native-eligibility-owner@example.test')" (Only earlierOwner)
    dbExec tr "INSERT INTO projects.project_members (project_id, user_id, permission, created_at) VALUES (?, ?, 'admin', '2000-01-01')" (testPid, earlierOwner)
    deniedProjects <- forM [(False, originalPlan), (True, "Free")] \(active, plan) -> do
      dbExec tr "UPDATE projects.projects SET active = ?, payment_plan = ? WHERE id = ?" (active, plan, testPid)
      mediaDenied <- isNothing <$> runQueryEffect tr (Fixes.mediaFor testPid result.runId (Fixes.Member user))
      listingDenied <- null <$> runQueryEffect tr (Fixes.latestMediaForUser testPid user)
      publicationDenied <- failure validate
      pageStatus <- runAuthHandler tr $ catchError @ServerError (CodeContextPage.issueFixGetH testPid result.runId $> 200) (\_ err -> pure err.errHTTPCode)
      pure (mediaDenied, listingDenied, publicationDenied, pageStatus)
    isJust <$> runQueryEffect tr (Fixes.mediaFor testPid result.runId (Fixes.Member earlierOwner)) `shouldReturn` True
    dbExec tr "UPDATE projects.projects SET active = true, payment_plan = ? WHERE id = ?" (originalPlan, testPid)
    deniedProjects `shouldBe` replicate 2 (True, True, Just Publication.AccessRevoked, 404)
    isJust <$> runQueryEffect tr (Fixes.publicationForUser testPid result.runId user) `shouldReturn` True
    dbExec tr "UPDATE users.users SET active = false WHERE id = ?" (Only user)
    failure validate `shouldReturn` Just Publication.AccessRevoked
    dbExec tr "UPDATE users.users SET active = true WHERE id = ?" (Only user)
    resetRemote
    setMode "moved"
    failure validate `shouldReturn` Just Publication.SourceChanged
    writes `shouldReturn` ([] :: [Text])
    for_ ["after-create", "after-attachment"] \mode -> do
      resetRemote
      setMode mode
      failure validate `shouldReturn` Just Publication.GitHubFailure
      setMode ""
      fmap Publication.publicationReceipt <$> runPublisher authorize validate `shouldReturn` fmap Publication.publicationReceipt published
      writes `shouldReturn` allWrites
    resetRemote
    let revokeDiagram stage = if stage == Publication.AttachDiagram then pure $ Left Publication.AccessRevoked else authorize stage
    leftToMaybe <$> runPublisher revokeDiagram validate `shouldReturn` Just Publication.AccessRevoked
    writes `shouldReturn` (["create", "explanation.mp4"] :: [Text])
    isRight <$> runPublisher authorize validate `shouldReturn` True
    writes `shouldReturn` allWrites
    -- An interrupted upload is journaled before the CLI command; it is never
    -- blindly retried when the remote body has no matching uploaded asset.
    resetRemote
    setMode "uncertain"
    failure validate `shouldReturn` Just Publication.GitHubFailure
    setMode ""
    failure validate `shouldReturn` Just Publication.UploadUncertain
    writes `shouldReturn` (["create"] :: [Text])
    -- Corrupt media is rejected before creating or updating remote resources.
    writeFileBS (media </> "explanation.mp4") "corrupted video"
    failure validate `shouldReturn` Just Publication.SourceChanged
    -- An App installation token cannot upload attachments, so its pull request is created already linking
    -- signed copies: nothing is uploaded and the (here corrupt) retained media is never read.
    resetRemote
    readState >>= writeFileLBS stateFile . AE.encode . withKey "token" (AE.String "ghs_fixture")
    writeIORef stages []
    let links = Publication.MediaUrls "https://monoscope.example/p/fix/shared/video?sig=a" "https://monoscope.example/p/fix/shared/cause?sig=b"
    linkedReceipt <- runEff $ HTTP.runHTTPRecord httpRequests $ withHTTPResponses fixture $ Publication.runGitPublicationReal (Just store) (Just github) $ Publication.publishFix publication (Publication.installationCredential "ghs_fixture" links) (liftIO . authorize) (liftIO . validate)
    fmap ((.assets) . Publication.publicationReceipt) linkedReceipt `shouldBe` Right links
    writes `shouldReturn` (["create"] :: [Text])
    reverse <$> readIORef stages `shouldReturn` [Publication.Prepare, Publication.Discover, Publication.Branch, Publication.Create, Publication.Complete]

  it "queues signed source-repository events once, rejects unverified and unconfigured deliveries, and retains the latest revision" \tr -> do
    setup tr
    runQueryEffect tr (Reviews.repositorySettings testPid) >>= \settings -> map (.includeEvidence) settings `shouldBe` [False]
    let firstEvent = payload "opened" revision "2025-01-01T00:00:00Z"
        next = T.replicate 40 "b"
    replicateM_ 2 $ deliver tr firstEvent `shouldReturn` AE.object ["status" AE..= ("ok" :: Text)]
    for_ @[] [AE.object [], payload "edited" revision "2025-01-01T00:00:00Z"] \ignored ->
      deliver tr ignored `shouldReturn` AE.object ["status" AE..= ("ignored" :: Text)]
    dbOne tr "SELECT count(*)::int FROM background_jobs_long WHERE payload->>'tag' = 'ReviewPullRequest'" () `shouldReturn` (1 :: Int)
    [old] <- runQueryEffect tr $ Reviews.latestRuns testPid
    runQueryEffect tr (Reviews.claimRun (slots 100) old) `shouldReturn` True
    runQueryEffect tr (Reviews.currentRun old) `shouldReturn` True
    dbOne tr "SELECT lease_until > now() + interval '9 minutes' FROM projects.pr_review_threads WHERE id = ?" (Only old.threadId) `shouldReturn` True
    dbOne tr "SELECT payload FROM background_jobs_long WHERE payload->>'tag' = 'ReviewPullRequest'" () `shouldReturn` Aeson (AE.toJSON $ Jobs.ReviewPullRequest old.id)
    (_, ()) <- runTestBgRecordingHTTP frozenTime tr $ Impact.reviewPullRequest old.id
    dbOne tr "SELECT count(*)::int FROM background_jobs_long WHERE payload = ? AND status = 'queued'" (Only $ Aeson $ AE.toJSON $ Jobs.ReviewPullRequest old.id) `shouldReturn` (2 :: Int)
    void $ deliver tr $ payload "synchronize" next "2025-01-01T00:01:00Z"
    void $ deliver tr firstEvent
    runQueryEffect tr (Reviews.currentRun old) `shouldReturn` False
    runs <- runQueryEffect tr $ Reviews.latestRuns testPid
    map (.latestRevision) runs `shouldBe` [next, next]
    new <- maybe (fail "new revision missing") pure $ find ((== next) . (.revision)) runs
    runQueryEffect tr (Reviews.claimRun (slots 100) new) `shouldReturn` False
    dbOne tr "SELECT payload FROM background_jobs_long WHERE run_at > now() AND payload = ?" (Only $ Aeson $ AE.toJSON $ Jobs.ReviewPullRequest new.id) `shouldReturn` Aeson (AE.toJSON $ Jobs.ReviewPullRequest new.id)
    runQueryEffect tr $ Reviews.releaseRun old
    runQueryEffect tr (Reviews.claimRun (slots 100) new) `shouldReturn` True
    runQueryEffect tr (Reviews.currentRun new) `shouldReturn` True
    dbExec tr "UPDATE projects.pr_review_threads SET lease_until = now() - interval '1 second' WHERE id = ?" (Only new.threadId)
    runQueryEffect tr (Reviews.currentRun new) `shouldReturn` False
    runQueryEffect tr (Reviews.claimRun (slots 100) new) `shouldReturn` True
    void $ deliver tr $ payload "converted_to_draft" next "2025-01-01T00:01:30Z"
    runQueryEffect tr (Reviews.currentRun new) `shouldReturn` False
    void $ deliver tr $ payload "closed" next "2025-01-01T00:02:00Z"
    runQueryEffect tr (Reviews.currentRun new) `shouldReturn` False
    void $ deliver tr $ payload "reopened" next "2025-01-01T00:03:00Z"
    reopened <- maybe (fail "run missing") pure =<< runQueryEffect tr (Reviews.getRun new.id)
    reopened.state `shouldBe` Reviews.Queued
    runQueryEffect tr $ Reviews.releaseRun new
    let status secret signature = fmap snd $ runAsBaseRecordingHTTP tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppWebhookSecret = secret}}} $ catchError @ServerError (GitSyncPage.gitWebhookPostH Git.GitHub (Git.WebhookReq (Just "pull_request") signature Nothing Nothing Nothing (toStrict $ AE.encode $ payload "synchronize" (T.replicate 40 "c") "2025-01-01T00:04:00Z")) $> 200) (\_ err -> pure err.errHTTPCode)
        forged = Just $ "sha256=" <> T.replicate 64 "0"
    traverse (uncurry status) [(tr.trATCtx.config.githubAppWebhookSecret, Nothing), ("", forged), ("impact-secret", forged)] `shouldReturn` [503, 503, 401]
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
    -- Switching to asks only keeps a running review; unlinking the repository revokes it,
    -- and the released run stays rerunnable.
    runQueryEffect tr (Reviews.claimRun (slots 100) otherRun) `shouldReturn` True
    runQueryEffect tr $ Reviews.updateSettings otherPid (Reviews.ReviewSettings "impact-org" "checkout" Reviews.WhenAsked True)
    runQueryEffect tr (Reviews.currentRun otherRun) `shouldReturn` True
    dbExec tr "DELETE FROM projects.code_mappings WHERE project_id = ?" (Only otherPid)
    runQueryEffect tr (Reviews.currentRun otherRun) `shouldReturn` False
    runQueryEffect tr $ Reviews.releaseRun otherRun
    released <- maybe (fail "run missing") pure =<< runQueryEffect tr (Reviews.getRun otherRun.id)
    released.state `shouldBe` Reviews.Incomplete

  it "reviewTriggers_draftsFollowTheModeAndMembersCanAskInEveryMode" \tr -> do
    setup tr
    let queued = dbOne tr "SELECT count(*)::int FROM background_jobs_long WHERE payload->>'tag' = 'ReviewPullRequest' AND status = 'queued'" ()
        mode trigger = runQueryEffect tr $ Reviews.updateSettings testPid (Reviews.ReviewSettings "impact-org" "checkout" trigger False)
        sha = T.replicate 40
        draftHead = AE.object ["head" AE..= AE.object ["sha" AE..= sha "c"], "base" AE..= AE.object ["sha" AE..= sha "0"], "state" AE..= ("open" :: Text), "draft" AE..= True, "changed_files" AE..= (1 :: Int), "updated_at" AE..= ("2025-01-01T00:05:00Z" :: Text)]
        api permission url = pure $ if "/pulls/7" `T.isSuffixOf` url then Just draftHead else if "/reactions" `T.isSuffixOf` url then Just (AE.object []) else if "/collaborators/ada/permission" `T.isSuffixOf` url then Just (AE.object ["permission" AE..= (permission :: Text)]) else Nothing
    void $ deliver tr $ payload "converted_to_draft" (sha "a") "2025-01-01T00:00:00Z"
    queued `shouldReturn` 0
    mode Reviews.AllPullRequests
    void $ deliver tr $ payload "converted_to_draft" (sha "b") "2025-01-01T00:01:00Z"
    queued `shouldReturn` 1
    mode Reviews.WhenAsked
    void $ deliver tr $ payload "synchronize" (sha "c") "2025-01-01T00:02:00Z"
    queued `shouldReturn` 1
    runQueryEffect tr (Reviews.latestRuns testPid) >>= (`shouldSatisfy` all ((== sha "c") . (.latestRevision)))
    -- Contributors and ordinary comments queue nothing and call no GitHub API.
    for_ @[] [("CONTRIBUTOR", "@monoscope prove it"), ("MEMBER", "looks good to me" :: Text)] \(association :: Text, body) ->
      deliverEvent tr "issue_comment" (api "write") (commentPayload association body) `shouldReturn` ([], AE.object ["status" AE..= ("ignored" :: Text)])
    -- Org membership is not write access: a read-only member's ask queues nothing and gets no reaction.
    (readOnly, _) <- deliverEvent tr "issue_comment" (api "read") $ commentPayload "MEMBER" "@monoscope review"
    map fst readOnly `shouldNotSatisfy` any ("/reactions" `T.isSuffixOf`)
    queued `shouldReturn` 1
    -- A member's ask reviews the draft head even though this mode never runs on its own.
    (calls, response) <- deliverEvent tr "issue_comment" (api "write") $ commentPayload "MEMBER" "@monoscope prove it, and check the empty state"
    response `shouldBe` AE.object ["status" AE..= ("ok" :: Text)]
    map fst calls `shouldSatisfy` any ("/issues/comments/55/reactions" `T.isSuffixOf`)
    queued `shouldReturn` 2
    asked <- maybe (fail "asked run missing") pure . find ((== sha "c") . (.revision)) =<< runQueryEffect tr (Reviews.latestRuns testPid)
    asked.request `shouldBe` Just Reviews.Ask{login = "ada", commentId = 55, command = Reviews.Prove, focus = Just "and check the empty state"}
    -- A later ask before any worker starts takes the queued run over instead of queueing twice.
    void $ deliverEvent tr "issue_comment" (api "write") $ commentPayload "MEMBER" "@monoscope review"
    queued `shouldReturn` 2
    runQueryEffect tr (Reviews.getRun asked.id) >>= (`shouldSatisfy` (== Just (Just Reviews.Review)) . fmap (fmap (.command) . (.request)))
    -- An ask on a closed pull request queues nothing and is not acknowledged.
    (closedCalls, _) <- deliverEvent tr "issue_comment" (\url -> if "/pulls/7" `T.isSuffixOf` url then pure $ Just $ AE.object ["head" AE..= AE.object ["sha" AE..= sha "c"], "base" AE..= AE.object ["sha" AE..= sha "0"], "state" AE..= ("closed" :: Text), "draft" AE..= False, "changed_files" AE..= (1 :: Int), "updated_at" AE..= ("2025-01-01T00:04:00Z" :: Text)] else api "write" url) $ commentPayload "MEMBER" "@monoscope review"
    map fst closedCalls `shouldNotSatisfy` any ("/reactions" `T.isSuffixOf`)
    queued `shouldReturn` 2

  it "proofMedia_servesOnlySignedRecordingsOfTheirProject" \tr -> do
    setup tr
    void $ deliver tr $ payload "opened" revision "2025-01-01T00:00:00Z"
    [run] <- runQueryEffect tr $ Reviews.latestRuns testPid
    let attempt = UUIDId UUID.nil
    dbExec tr "INSERT INTO projects.pr_review_proofs (run_id, attempt_id, key, side, revision, exit_code, cast_bytes, gif, mp4) VALUES (?, ?, 'proof-1', 'head', ?, 0, convert_to('cast', 'UTF8'), convert_to('GIF89a fixture', 'UTF8'), convert_to('mp4 fixture', 'UTF8'))" (run.id, attempt, revision)
    [link] <- map (Impact.proofLink tr.trATCtx run) <$> runQueryEffect tr (Reviews.proofSides run attempt)
    let app = genericServeTWithContext (effToServantHandlerTest tr.trTestClock tr.trUUIDRef tr.trATCtx tr.trLogger tr.trTracerProvider) (Routes.server (\_ _ _ -> pure $ Left "unused") (\_ _ _ -> pure $ Left "unused")) (Routes.genAuthServerContext tr.trLogger tr.trATCtx)
        fetchWith headers url = WT.runSession (WT.request $ WT.setPath Wai.defaultRequest{Wai.requestHeaders = headers} $ encodeUtf8 $ fromMaybe url $ T.stripPrefix tr.trATCtx.config.hostUrl url) app
        fetch = fetchWith []
    preview <- fetch link.preview
    (H.statusCode preview.simpleStatus, lookup "Content-Type" preview.simpleHeaders, preview.simpleBody) `shouldBe` (200, Just "image/gif", "GIF89a fixture")
    video <- fetch link.video
    (H.statusCode video.simpleStatus, lookup "Content-Type" video.simpleHeaders) `shouldBe` (200, Just "video/mp4")
    -- Safari plays video only through byte ranges.
    ranged <- fetchWith [("Range", "bytes=0-2")] link.video
    (H.statusCode ranged.simpleStatus, ranged.simpleBody) `shouldBe` (206, "mp4")
    -- A signature binds the project, review and file: none of them can be swapped.
    otherPid <- createTestProject tr "Other proof project"
    for_ [T.replace "sig=" "sig=0" link.preview, T.replace testPid.toText otherPid.toText link.preview, T.replace "proof-1-head.gif" "proof-1-base.gif" link.preview] \forged ->
      (H.statusCode . (.simpleStatus) <$> fetch forged) `shouldReturn` 403
    -- proofRating_recordsTheAuthorsVerdict: the comment's signed Yes and No links rate the review; a forged one is refused.
    let rateUrl r = proofRatingPath testPid run.id r $ Widget.signWidgetUrl tr.trATCtx.env.apiKeyEncryptionSecretKey testPid (Reviews.ratingSignedPath run.id r)
    (H.statusCode . (.simpleStatus) <$> fetch (T.replace "sig=" "sig=0" $ rateUrl Reviews.Down)) `shouldReturn` 403
    (H.statusCode . (.simpleStatus) <$> fetch (T.replace "/up?" "/down?" $ rateUrl Reviews.Up)) `shouldReturn` 403
    -- Opening the link only asks: mail scanners and unfurlers open every link, so the button POSTs.
    (H.statusCode . (.simpleStatus) <$> fetch (rateUrl Reviews.Up)) `shouldReturn` 200
    dbOne tr "SELECT rating IS NULL FROM projects.pr_review_runs WHERE id = ?" (Only run.id) `shouldReturn` True
    posted <- WT.runSession (WT.request $ WT.setPath Wai.defaultRequest{Wai.requestMethod = "POST"} $ encodeUtf8 $ rateUrl Reviews.Up) app
    H.statusCode posted.simpleStatus `shouldBe` 200
    dbOne tr "SELECT rating FROM projects.pr_review_runs WHERE id = ?" (Only run.id) `shouldReturn` ("up" :: Text)

  it "sandbox_installsFromPackageRegistriesButReachesNothingElse" \_ -> withTemp "registry-egress" \directory -> do
    image <- guestImage
    store <- storeIn directory
    bridge <- canonicalizePath "test/fixtures/native-sandbox.sh"
    let repository = directory </> "repository"
        scriptFile = directory </> "prepare.sh"
        requestFile = directory </> "request.json"
    initRepo repository
    base <- commit repository [("app.sh", "v1\n")]
    -- A build or proof may fetch packages, so a missing toolchain is installable; it cannot write
    -- to a registry or reach any other host, GitHub included.
    writeFileBS scriptFile "set -eu\ncurl -sSfI https://registry.npmjs.org/ >/dev/null\n# Node, pip and uv read their CA from the environment, which --reset-env clears; it must still trust the egress proxy.\ncurl -sSfI --cacert \"$NODE_EXTRA_CA_CERTS\" https://registry.npmjs.org/ >/dev/null\n! curl -sf -o /dev/null https://github.com/\ncurl -sSfI https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-0.0.8.yaml >/dev/null\n! curl -sf -o /dev/null https://raw.githubusercontent.com/torvalds/linux/master/README\n! curl -sf -o /dev/null -X POST https://registry.npmjs.org/-/v1/search\n"
    writeFileLBS requestFile $ AE.encode $ AE.object ["projectId" AE..= testPid, "repository" AE..= repository, "base" AE..= (base :: Text)]
    runEff (Workspace.runWorkspaceReal $ Workspace.runWorkspace (Workspace.Options store.cacheDir (toText image) bridge scriptFile Nothing Nothing) (Workspace.Prepare repository) requestFile) >>= either (fail . toString) (const pass)

  it "workspacePrepare_warmStartsFromTheRepositoryLatestBuild" \tr -> withTemp "native-ancestor" \directory -> do
    image <- guestImage
    store <- storeIn directory
    bridge <- canonicalizePath "test/fixtures/native-sandbox.sh"
    let repository = directory </> "repository"
        scriptFile = directory </> "prepare.sh"
        requestFile = directory </> "request.json"
        prepareAt stage base = do
          writeFileLBS requestFile $ AE.encode $ AE.object ["projectId" AE..= testPid, "repository" AE..= repository, "base" AE..= base]
          runEff (Workspace.runWorkspaceReal $ Workspace.runWorkspace (Workspace.Options store.cacheDir (toText image) bridge scriptFile Nothing Nothing) (Workspace.Prepare repository) requestFile) >>= either (fail . toString . ((stage <> ": ") <>)) (maybe (fail "Workspace missing") (parse (\value -> (,) <$> (field "manifest" >=> field "identity" >=> field "base") value <*> field "outcome" value) . AE.toJSON))
        built start = AE.toJSON $ Workspace.Built start Nothing
    initRepo repository
    original <- commit repository [(".gitignore", "build/\n"), ("app.sh", "v1\n")]
    -- Like a compiler cache, the build keeps state in an ignored directory. At the second base it
    -- refuses to run unless that state came along, so a cold start fails this test.
    writeFileBS scriptFile "set -eu\nif [ \"$(cat app.sh)\" = v2 ]; then test -s build/runs; fi\nmkdir -p build\necho run >> build/runs\n"
    prepareAt "cold" original `shouldReturn` (original :: Text, built Workspace.Cold)
    next <- commit repository [("app.sh", "v2\n")]
    prepareAt "warm" next `shouldReturn` (next, built Workspace.Warm)
    -- A damaged ancestor is forgotten and the build retries cold rather than failing.
    void $ readProcessStdout_ $ proc "sh" ["-c", "find \"$0\" -name baseline.qcow2 -exec sh -c 'printf corrupt > \"$1\"' _ {} \\;", store.cacheDir]
    third <- commit repository [("app.sh", "v3\n")]
    (fallbackBase, fallback) <- prepareAt "fallback" third
    fallbackBase `shouldBe` third
    -- The cold retry still reports the failed warm start, so the build job can raise it.
    decodeUtf8 @Text (AE.encode fallback) `shouldSatisfy` T.isInfixOf "ColdAfterWarm"
    -- A build that misses locally restores the archived checkpoint instead of compiling.
    requireMinio tr pendingWith do
      let config = tr.trATCtx.config
          remote = directory </> "remote.json"
          run operation = runEff (Workspace.runWorkspaceReal $ Workspace.runWorkspace (Workspace.Options store.cacheDir (toText image) bridge scriptFile Nothing (Just remote)) operation requestFile) >>= either (fail . toString) (maybe (fail "Workspace missing") (pure . AE.toJSON))
      writeFileLBS remote $ AE.encode $ AE.object ["endpoint" AE..= config.s3Endpoint, "bucket" AE..= config.s3Bucket, "region" AE..= config.s3Region, "accessKey" AE..= config.s3AccessKey, "secretKey" AE..= config.s3SecretKey, "maxBytes" AE..= (1073741824 :: Int)]
      Posix.setFileMode remote 0o600
      -- A build archives itself where storage is configured; nothing calls Archive in production.
      archived <- commit repository [("app.sh", "v3b\n")]
      writeFileLBS requestFile $ AE.encode $ AE.object ["projectId" AE..= testPid, "repository" AE..= repository, "base" AE..= archived]
      void $ run (Workspace.Prepare repository)
      removePathForcibly $ store.cacheDir </> toString testPid.toText </> "workspaces"
      run (Workspace.Prepare repository) >>= parse (field "outcome") >>= (`shouldBe` AE.toJSON Workspace.Restored)
      -- A fresh node has no local ancestor: it restores the lineage's latest archived build and starts warm.
      removePathForcibly $ store.cacheDir </> toString testPid.toText </> "workspaces"
      fourth <- commit repository [("app.sh", "v4\n")]
      writeFileLBS requestFile $ AE.encode $ AE.object ["projectId" AE..= testPid, "repository" AE..= repository, "base" AE..= fourth]
      run (Workspace.Prepare repository) >>= parse (field "outcome" >=> field "contents") >>= (`shouldSatisfy` ((== Just (AE.toJSON Workspace.Warm)) . listToMaybe @AE.Value))

  it "admission_capsReviewsPerProjectAndHostAndSavesProofRecipes" \tr -> do
    setup tr
    let eventAt base n = Reviews.PullRequestEvent "impact-org" "checkout" 987654 n (T.replicate 40 (show n)) base frozenTime Reviews.Open
        event = eventAt (T.replicate 40 "b")
        sideProof side code = Proof.ProofMedia{side, revision = T.replicate 40 "7", exitCode = Just code, cast = "cast", gif = "gif", mp4 = "mp4", browser = False, shots = [], chapters = []}
        deferred rid = dbOne tr "SELECT count(*)::int FROM background_jobs_long WHERE payload = ? AND run_at > now()" (Only $ Aeson $ AE.toJSON $ Jobs.ReviewPullRequest rid)
    for_ [7, 8, 9] \n -> void $ runQueryEffect tr $ Reviews.receiveEvent Nothing $ event n
    -- Reviews of one base share a single workspace build; another base queues its own.
    let builds = dbOne tr "SELECT count(*)::int FROM background_jobs_long WHERE payload->>'tag' = 'PrepareReviewWorkspace'" ()
    builds `shouldReturn` (1 :: Int)
    [early, middle, late] <- sortOn (\(run :: Reviews.ReviewRun) -> run.number) <$> runQueryEffect tr (Reviews.latestRuns testPid)
    -- Proofs run in the base's prepared workspace, so no review of that base starts while its build is pending.
    runQueryEffect tr (Reviews.claimRun (slots 100){Workspace.prepares = True} early) `shouldReturn` False
    dbExec tr "DELETE FROM background_jobs_long WHERE payload->>'tag' = 'PrepareReviewWorkspace'" ()
    -- Two reviews per project run at once; the third waits in the queue rather than for a VM.
    traverse (\run -> runQueryEffect tr $ Reviews.claimRun (slots 100) run) [early, middle, late] `shouldReturn` [True, True, False]
    deferred late.id `shouldReturn` (1 :: Int)
    runQueryEffect tr $ Reviews.releaseRun early
    -- A node admits up to its agent slots across projects; another node's reviews leave its own free.
    runQueryEffect tr (Reviews.claimRun (slots 1){Workspace.node = "test-node"} late) `shouldReturn` False
    runQueryEffect tr (Reviews.claimRun (slots 1){Workspace.node = "second-node"} late) `shouldReturn` True
    -- A setup whose head recording succeeded becomes the repository's recipe; an attempt stores at most three proofs.
    let attempt = UUIDId UUID.nil
        saveProof = runQueryEffect tr $ Reviews.saveProofs middle attempt (Just "make serve &") (sideProof Agent.Base 1 :| [sideProof Agent.Head 0])
    -- The first replays no saved recipe; later ones replay the one it saved.
    traverse (const saveProof) [1 .. 4 :: Int] `shouldReturn` ([Right ("proof-1", Reviews.Changed), Right ("proof-2", Reviews.Reused), Right ("proof-3", Reviews.Reused), Left "Proof budget exhausted; cite the proofs already recorded"] :: [Either Text (Text, Reviews.SetupReuse)])
    fmap (.setup) <$> runQueryEffect tr (Reviews.proofRecipe middle) `shouldReturn` Just "make serve &"
    void $ runQueryEffect tr $ Reviews.receiveEvent Nothing $ eventAt (T.replicate 40 "c") 4
    builds `shouldReturn` 1 -- the first base's build finished above; this base queues its own
    -- askOnReviewedHead_answersInANewComment: an ask on a head that already has a progress comment answers below itself.
    dbExec tr "UPDATE projects.pr_review_runs SET state = 'completed', progress_comment_id = 55 WHERE revision = ?" (Only $ T.replicate 40 "7")
    void $ runQueryEffect tr $ Reviews.receiveEvent (Just Reviews.Ask{login = "ada", commentId = 78, command = Reviews.Prove, focus = Nothing}) (event 7)
    dbQuery tr "SELECT state, progress_comment_id FROM projects.pr_review_runs WHERE revision = ?" (Only $ T.replicate 40 "7") `shouldReturn` [("queued" :: Text, Nothing :: Maybe Int64)]

  it "reviewComment_leadsWithMergeRiskAndShowsProductionDetailOnlyWhereAllowed" \_ -> do
    let pid = UUIDId UUID.nil
        run = Reviews.ReviewRun{id = UUIDId UUID.nil, threadId = UUIDId UUID.nil, projectId = pid, owner = "acme", repo = "checkout", number = 42, revision = T.replicate 40 "a", state = Reviews.Completed, result = Nothing, error = Nothing, commentId = Nothing, latestRevision = T.replicate 40 "a", createdAt = frozenTime, request = Nothing, base = Nothing}
        telemetry = Impact.Evidence "telemetry-1" Impact.Telemetry (Just "checkout-api") (Just "production") "https://app.monoscope.tech/p/x/log_explorer" Nothing (Just $ Impact.Observation 120340 312) (Just "https://app.monoscope.tech/p/x/widget.png?widgetZ=fixture") Nothing
        finding path line mechanism next = Impact.Finding (Impact.ChangedLine path line Impact.Head) mechanism next ["telemetry-1"]
        grades = [(Impact.Alignment, Impact.Good, []), (Impact.Impact, Impact.Poor, ["Changes how payment captures are retried", "checkout-api serves every order"]), (Impact.Verification, Impact.Fair, ["No test covers a capture that times out twice"]), (Impact.Rollback, Impact.Good, []), (Impact.ErrorDetection, Impact.Poor, ["The new retry path logs nothing when it gives up"])]
        result =
          Impact.ReviewResult
            Impact.WorthChecking
            [ finding "src/payments/capture.ts" 88 "A retried capture reuses the same idempotency key only on the first retry; a second timeout sends a fresh key, so a slow provider can charge twice." "Count captures with two or more retries in the last week, then add a test for a double timeout."
            , finding "src/payments/capture.ts" 120 "The give-up branch swallows the provider error, so a failed capture leaves no log or span error." "Log the provider error and mark the span as an error before returning."
            ]
            ["No load test covers the retry backoff.", "Provider-side duplicates are not visible in this project's telemetry."]
            [telemetry]
            frozenTime
            (addUTCTime (7 * 86400) frozenTime)
            Nothing
            (Just "Adds retries to payment capture. A second timeout can charge a customer twice, and the give-up path is silent in production.")
            (Just [Impact.Assessment d g gaps | (d, g, gaps) <- grades])
            ( Just
                [ Impact.Claim "Retries a timed-out capture once" Impact.Matches "The capture command now retries after the first timeout and succeeds." ["proof-1"]
                , Impact.Claim "Reuses the idempotency key on every retry" Impact.Differs "The second retry sent a new key, so the provider saw two charges." ["proof-2"]
                , Impact.Claim "Shows the retry count in the receipt" Impact.Unverified "The receipt page needs a browser, which this review cannot record." []
                ]
            )
            (Just [Impact.ProofLink key side (T.replicate 40 (if side == Agent.Base then "d" else "a")) (Just $ if side == Agent.Base then 1 else 0) (media "gif") (media "mp4") (media "cast") (Just browser) (Just [Impact.Shot label (media (show n <> ".png")) | browser, (n, label) <- zip [1 :: Int ..] (if side == Agent.Base then ["Receipt without retries"] else ["Receipt with retry count", "Retry history"])]) (Just [(t, step) | browser, (t, step) <- [(0, "Open the receipt"), (6.4, "Retry the charge"), (14.2, "Read the retry count")]]) | (key, browser) <- [("proof-1", True), ("proof-2", False)], side <- [Agent.Base, Agent.Head], let media ext = "https://app.monoscope.tech/p/x/reviews/r/proofs/" <> key <> "-" <> T.toLower (show side) <> (if "png" `T.isSuffixOf` ext then "-" else ".") <> ext <> "?sig=s"])
            Nothing
        render audience = Impact.renderReview "https://app.monoscope.tech" run audience result
    goldenText "impact-review/github-comment.md" $ render $ Impact.GitHubComment True "https://app.monoscope.tech/p/x/ai/runs/y" Nothing
    goldenText "impact-review/github-comment-links-only.md" $ render $ Impact.GitHubComment False "https://app.monoscope.tech/p/x/ai/runs/y" Nothing
    goldenText "impact-review/monoscope-view.md" $ render Impact.MonoscopeView
    -- A browser proof leads with its recording, chapters and the merge-base and pull-request stills.
    let uiProof = result{Impact.claims = Just [Impact.Claim "Shows the retry count in the receipt" Impact.Matches "The receipt shows Retried once under the charge; before, it showed only the amount." ["proof-1"], Impact.Claim "Retries a timed-out capture once" Impact.Matches "The capture command retries after the first timeout and succeeds." ["proof-2"]]}
    goldenText "impact-review/github-comment-ui-proof.md" $ Impact.renderReview "https://app.monoscope.tech" run (Impact.GitHubComment True "https://app.monoscope.tech/p/x/ai/runs/y" (Just \case Reviews.Up -> "https://app.monoscope.tech/rate/up"; Reviews.Down -> "https://app.monoscope.tech/rate/down")) uiProof{Impact.issues = Just [Impact.Issue "Receipt total overflows its card at 1280px" "The total runs 9px past the receipt card on the right; it fits at 1440px." ["proof-1"]]}
    -- While proofs run, the comment lists each planned claim and how far it has got.
    goldenText "impact-review/github-comment-progress.md" $ Impact.renderProgress "https://app.monoscope.tech" (T.replicate 40 "d") (T.replicate 40 "a") [Reviews.ClaimProgress "Shows the retry count in the receipt" (Reviews.Proved "proof-1"), Reviews.ClaimProgress "Retries a timed-out capture once" Reviews.Running, Reviews.ClaimProgress "Keeps the receipt link | after a retry" Reviews.Pending] (fold uiProof.proofs)

  it "keeps started calls attributable until matching usage arrives, including after billing changes" \tr -> do
    setup tr
    void $ deliver tr $ payload "opened" revision "2025-01-01T00:00:00Z"
    [run] <- runQueryEffect tr $ Reviews.latestRuns testPid
    dbExec tr "UPDATE projects.projects SET billing_provider = ?, customer_id = 'cus_call_start' WHERE id = ?" (Projects.StripeProvider, testPid)
    attempt <- maybe (fail "Attempt missing") pure =<< runQueryEffect tr (Usage.startAttempt testPid (Usage.Review run.id) "fixture" "fixture-model")
    otherPid <- createTestProject tr "Unrelated model calls"
    let uid n = UUIDId $ UUID.fromWords 0 0 0 n
        call = Agent.ModelCall{id = uid 991, attemptId = attempt, provider = "fixture", model = "fixture-model"}
        received = Agent.ModelUsage{id = uid 992, attemptId = attempt, callId = Just call.id, provider = "fixture", model = "fixture-response-model", responseId = Just "fixture-response", inputTokens = 10, outputTokens = 1, cacheReadTokens = 0, cacheWriteTokens = 0, costNanousd = Just 42, pricing = AE.object [], succeeded = True}
        totals = runQueryEffect tr $ Usage.usageTotals Usage.Reviews testPid
        summary = fmap (.usage) . find ((== Just run.id) . (.runId))
    for_ [call, call] \value -> runQueryEffect tr (Usage.recordCall testPid (Usage.Review run.id) value) `shouldReturn` True
    runQueryEffect tr (Usage.recordCall testPid (Usage.Review run.id) call{Agent.model = "changed"}) `shouldReturn` False
    runQueryEffect tr (Usage.recordCall otherPid (Usage.Review run.id) call) `shouldReturn` False
    awaiting <- maybe (fail "Pending call missing") pure . summary =<< totals
    (awaiting.calls, awaiting.unreportedCalls, awaiting.unpricedCalls, awaiting.costNanousd) `shouldBe` (1, 1, 1, 0)
    dbExec tr "UPDATE projects.projects SET customer_id = 'cus_after_call' WHERE id = ?" (Only testPid)
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Review run.id) received{Agent.callId = Nothing}) `shouldReturn` False
    for_ [received, received] \value -> runQueryEffect tr (Usage.recordUsage testPid (Usage.Review run.id) value) `shouldReturn` True
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Review run.id) received{Agent.id = uid 993}) `shouldReturn` False
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Review run.id) received{Agent.callId = Just $ uid 999}) `shouldReturn` False
    runQueryEffect tr (Usage.recordCall testPid (Usage.Review run.id) call{Agent.id = uid 994}) `shouldReturn` True
    reconciled <- maybe (fail "Reconciled usage missing") pure . summary =<< totals
    (reconciled.calls, reconciled.unreportedCalls, reconciled.unpricedCalls, reconciled.costNanousd) `shouldBe` (2, 1, 1, 42)
    invalid <- tryAny $ dbExec tr "INSERT INTO projects.pr_review_usage (id, run_id, project_id, billing_provider, receipt, model_call_id) VALUES (?, ?, ?, ?, ?, ?)" (uid 998 :: UUIDId "model_usage", run.id, testPid, Projects.StripeProvider, Aeson $ AE.toJSON received, uid 994 :: UUIDId "model_call")
    isLeft invalid `shouldBe` True
    for_ (leftToMaybe invalid) \failure -> show @Text failure `shouldSatisfy` T.isInfixOf "agent_model_usage_requires_attempt"
    rows <- runQueryEffect tr $ Usage.customerUsage (getResponse tr.trSessAndHeader).user.id
    fmap (.customerId) (find ((== testPid) . (.projectId)) rows) `shouldBe` Just (Just "cus_call_start")
    (_, response) <- testServant tr $ CodeContextPage.repositoriesGetH testPid (Just CodeContextPage.Usage) Nothing
    renderT response `shouldSatisfy` T.isInfixOf "calls without usage: 1 (cost unknown)"
    legacyAttempt <- maybe (fail "Legacy attempt missing") pure =<< runQueryEffect tr (Usage.startAttempt testPid (Usage.Review run.id) "fixture" "fixture-model")
    let legacy = received{Agent.id = uid 995, Agent.attemptId = legacyAttempt, Agent.callId = Nothing}
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Review run.id) legacy) `shouldReturn` True
    dbExec tr "UPDATE projects.pr_review_usage SET receipt = receipt - 'callId' WHERE id = ?" (Only legacy.id)
    runQueryEffect tr (Usage.recordCall testPid (Usage.Review run.id) call{Agent.id = uid 996, Agent.attemptId = legacyAttempt}) `shouldReturn` True
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Review run.id) legacy) `shouldReturn` True
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Review run.id) legacy{Agent.id = uid 997}) `shouldReturn` False

  for_ [ReviewWake Investigations.ReviewApproved, ReviewWake Investigations.ReviewRejected, ExpiryWake, ProviderWake, FinalProviderWake, WorkerWake, RuntimeWake] $ \recovery ->
    it ("nativeReview_resumesOriginalEvidenceAndNotesAfterInterruption: " <> show recovery) \tr -> withTemp "native-review-context" \directory -> do
      image <- nativeTestImage
      store <- storeIn directory
      let repository = directory </> "repository"
          bridge = directory </> "sandbox.sh"
          abrupt = recovery `elem` [WorkerWake, RuntimeWake]
          extraCall = recovery `elem` [ProviderWake, WorkerWake, RuntimeWake]
          pageId = UUIDId $ UUID.fromWords 62 0x4000 0x80000000 1
          draft = AE.object (graded <> ["findings" AE..= V.singleton (AE.object ["location" AE..= AE.object ["path" AE..= ("completed.txt" :: Text), "line" AE..= (1 :: Int), "side" AE..= ("head" :: Text)], "mechanism" AE..= ("The changed behavior needs a retry check against the recorded count." :: Text), "nextStep" AE..= ("Check retry behavior." :: Text), "evidenceKeys" AE..= (["query-1"] :: [Text])]), "coverage" AE..= ([] :: [Text])])
          plan =
            [ SdkTool "query_telemetry" $ AE.object ["service" AE..= ("test-service" :: Text), "query" AE..= ("| summarize count()" :: Text)]
            , SdkTool "remember_knowledge" $ AE.object ["service" AE..= ("test-service" :: Text), "summary" AE..= ("Recheck the recorded retry observation." :: Text), "evidenceKeys" AE..= (["query-1"] :: [Text])]
            , SdkTool "write_project_memory" $ AE.object ["pageId" AE..= pageId, "version" AE..= (0 :: Int), "document" AE..= AE.object ["title" AE..= ("Recorded review recovery" :: Text), "body" AE..= ("Check the dated observation before changing retry behavior." :: Text)]]
            , SdkTool "submit_review" draft
            , SdkText "Review submitted."
            ]
      when (recovery == RuntimeWake) do
        writeFileBS bridge "#!/bin/sh\nprintf '%s\\n' \"$$\" > \"$0.pid\"\nexec test/fixtures/native-sandbox.sh\n"
        Posix.setFileMode bridge 0o700
      initRepo repository
      base <- commit repository [("completed.txt", "original\n")]
      headRevision <- commit repository [("completed.txt", "changed\n")]
      setup tr
      dbExec tr "UPDATE projects.projects SET customer_id = 'cus_native_context', billing_provider = ? WHERE id = ?" (Projects.StripeProvider, testPid)
      void $ deliver tr $ payload "opened" headRevision "2025-01-01T00:00:00Z"
      [review] <- runQueryEffect tr $ Reviews.latestRuns testPid
      Just routine <- runQueryEffect tr $ Issues.getEventRoutine testPid Issues.EventPullRequest
      let policy = case recovery of
            (ProviderWake; FinalProviderWake) -> Issues.Autonomous
            (ReviewWake{}; ExpiryWake; WorkerWake; RuntimeWake) -> Issues.ReviewSelected $ Catalog.ReviewSelection (one Catalog.KnowledgeChanges) Catalog.ReviewEditors
      runQueryEffect tr $ Issues.setRoutinePolicy routine.requestedBy testPid routine.conversationId policy
      apiKey <- createTestAPIKey tr testPid "Native context evidence"
      ingestLog tr apiKey "native-context-observation" $ addUTCTime (-60) frozenTime
      appConfig <- githubApp tr
      publications <- newIORef (0 :: Int)
      interrupt <- newIORef (recovery `elem` [ProviderWake, FinalProviderWake, WorkerWake, RuntimeWake])
      runners <- newIORef ([] :: [Int32])
      let runnerGone pid = do
            alive <- isRight <$> tryAny (Signal.signalProcess Signal.nullSignal $ fromIntegral pid)
            when alive $ threadDelay 10000 >> runnerGone pid
          cleanup = readIORef runners >>= traverse_ (void . tryAny . Signal.signalProcess Signal.sigKILL . fromIntegral)
      let cfg = appConfig{impactReviewUsageDir = Just store.usageDir, impactReviewCacheDir = Just store.cacheDir, impactReviewProvider = "context-fixture", openaiModel = "fixture"}
          transport :: ATBackgroundCtx a -> ATBackgroundCtx a
          transport = pullRequestApi [Git.PullRequestFile "completed.txt" "modified" $ Just "@@ -1 +1 @@\n-original\n+changed"] [] (modifyIORef' publications (+ 1)) (pullRequestAt base headRevision 1)
          next results = pure $ fromMaybe (SdkText "Unexpected repeated inference") $ viaNonEmpty head $ drop (length results) plan
      let provider _ replies = do
            interruptNow <- readIORef interrupt
            if interruptNow && abrupt && length replies == 3
              then do
                events <- mapMaybe (AE.decodeStrict @AE.Value) . BSC.lines <$> readFileBS (directory </> "worker.log")
                let pids = flip mapMaybe events $ AET.parseMaybe $ AE.withObject "Completed native mutation" \fields -> do
                      event <- fields AE..: "event"
                      tool <- fields AE..: "tool"
                      guard $ event == ("tool_execution_end" :: Text) && tool == ("write_project_memory" :: Text)
                      fields AE..: "pid"
                pid <- case pids of [owned] -> pure (owned :: Int32); _ -> fail "Expected the owned SDK worker after its committed wiki write"
                pid `shouldSatisfy` (> 1)
                map (.version) <$> runQueryEffect tr (Memory.pageHistory testPid pageId) `shouldReturn` [1]
                readIORef publications `shouldReturn` 0
                writeIORef interrupt False
                when (recovery == RuntimeWake) do
                  owner <- either fail pure . AE.eitherDecodeStrict @Int32 =<< readFileBS (bridge <> ".pid")
                  owner `shouldSatisfy` (> 1)
                  owned <- filter (\session -> session.pid == fromIntegral owner && session.alive) <$> sandboxSessions
                  length owned `shouldBe` 1
                  children <- words . decodeUtf8 @Text <$> readProcessStdout_ (proc "pgrep" ["-P", show owner])
                  runner <- case traverse (readMaybe . toString) children of Just [child] | child > 1 -> pure (child :: Int32); _ -> fail "Expected the owned VM runner"
                  command <- decodeUtf8 @Text <$> readProcessStdout_ (proc "ps" ["-p", show runner, "-o", "comm="])
                  command `shouldSatisfy` (\name -> any (`T.isInfixOf` name) ["gondolin-krun-runner", "qemu-system"])
                  writeIORef runners [runner]
                  Signal.signalProcess Signal.sigKILL $ fromIntegral owner
                Signal.signalProcess Signal.sigKILL $ fromIntegral pid
                pure $ SdkUnavailable "Native runtime interrupted after the wiki write committed"
              else
                if interruptNow && recovery `elem` [ProviderWake, FinalProviderWake] && length replies == (if recovery == FinalProviderWake then 4 else 2)
                  then writeIORef interrupt False $> SdkUnavailable "Provider interrupted after original evidence and notes"
                  else (`SdkResponse` SdkTokens 7 0) <$> next replies
      flip finally cleanup $ withSdkProvider directory "context-fixture" provider \models observed -> do
        let backend :: ATBackgroundCtx a -> ATBackgroundCtx a
            backend = impose (sdkAgent store $ [models, image, directory </> "worker.log"] <> [arg | recovery == RuntimeWake, arg <- ["--bridge", bridge]]) \env -> \case
              Agent.ReviewRepository selected request respond knowledge calls usage allocations checks proofs validate ->
                passthrough env $ Agent.ReviewRepository selected request{Agent.repository = toText repository} respond knowledge calls usage allocations checks proofs validate
              operation -> passthrough env operation
            worker at = runTestBgRecordingHTTP at tr $ Reader.local (\ctx -> ctx{config = cfg}) $ transport $ backend $ BackgroundJobs.processBackgroundJob tr.trATCtx $ Jobs.ReviewPullRequest review.id
        initial <- tryAny $ worker frozenTime
        isLeft initial `shouldBe` (recovery == ProviderWake)
        Just waiting <- runQueryEffect tr $ Issues.getEventRun testPid $ Issues.PullRequestReview review.id
        waiting.status `shouldBe` (case recovery of ProviderWake -> Issues.RunFailed; FinalProviderWake -> Issues.RunSucceeded; (ReviewWake{}; ExpiryWake; WorkerWake; RuntimeWake) -> Issues.RunRunning)
        operations <- runQueryEffect tr $ Investigations.listOperations @AI.OperationPolicy testPid waiting.id
        length operations `shouldBe` (case recovery of ProviderWake -> 0; FinalProviderWake -> 2; (ReviewWake{}; ExpiryWake; WorkerWake; RuntimeWake) -> 1)
        length <$> observed `shouldReturn` (if recovery == FinalProviderWake then 5 else 3)
        readIORef publications `shouldReturn` (if recovery == FinalProviderWake then 1 else 0)
        let vote = case recovery of ReviewWake decision -> Just decision; (WorkerWake; RuntimeWake) -> Just Investigations.ReviewApproved; (ExpiryWake; ProviderWake; FinalProviderWake) -> Nothing
        for_ vote $ \decision -> for_ operations $ \operation ->
          void $ testServant tr $ AIThreads.operationReviewPostH testPid operation.id $ AIThreads.OperationReviewForm decision
        let resumedAt = addUTCTime (if recovery == ExpiryWake then 24 * 60 * 60 + 1 else 60) frozenTime
        continued <- tryAny $ worker resumedAt
        readIORef runners >>= traverse_ (\runner -> Timeout.timeout (2 * 1000000) (runnerGone runner) `shouldReturn` Just ())
        writeIORef runners []
        if abrupt
          then continued `shouldSatisfy` isLeft
          else whenLeft_ continued \err -> do
            diagnostic <- readFileBS $ directory </> "worker.log"
            expectationFailure $ "Original evidence did not resume: " <> displayException err <> "\n" <> decodeUtf8 diagnostic
        void $ worker $ addUTCTime 60 resumedAt
        Just completed <- runQueryEffect tr $ Issues.getEventRun testPid $ Issues.PullRequestReview review.id
        completed.status `shouldBe` Issues.RunSucceeded
        Just initialContext <- runQueryEffect tr $ Issues.runContextFor testPid $ Issues.EventClaim (Issues.PullRequestReview review.id) waiting.id waiting.startedAt
        Just finalContext <- runQueryEffect tr $ Issues.runContextFor testPid $ Issues.EventClaim (Issues.PullRequestReview review.id) completed.id completed.startedAt
        finalContext.rootId `shouldBe` initialContext.rootId
        case recovery of
          (ProviderWake; WorkerWake; RuntimeWake) -> completed.id `shouldNotBe` waiting.id
          (ReviewWake{}; ExpiryWake; FinalProviderWake) -> completed.id `shouldBe` waiting.id
        Just retained <- runQueryEffect tr $ Reviews.getRun review.id
        result <- maybe (fail "Review result missing") (either fail pure . AET.parseEither AE.parseJSON) retained.result :: IO Impact.ReviewResult
        result.windowEnd `shouldBe` frozenTime
        map (.key) (filter (("query-" `T.isPrefixOf`) . (.key)) result.evidence) `shouldBe` ["query-1"]
        fmap (map (.summary)) result.knowledge `shouldBe` Just ["Recheck the recorded retry observation."]
        length <$> observed `shouldReturn` (if extraCall then 6 else 5)
        totals <- runQueryEffect tr $ Usage.usageTotals Usage.Reviews testPid
        fmap (\entry -> (entry.usage.calls, entry.usage.unreportedCalls, entry.usage.failedCalls)) (find ((== Just review.id) . (.runId)) totals) `shouldBe` Just (if extraCall then 6 else 5, if abrupt then 1 else 0, if recovery `elem` [ProviderWake, FinalProviderWake] then 1 else 0)
        fmap (.resources.cleanupFailures) (find ((== Just review.id) . (.runId)) totals) `shouldBe` Just (if recovery == RuntimeWake then 1 else 0)
        readIORef publications `shouldReturn` 1
        pages <- runQueryEffect tr $ Memory.pageHistory testPid pageId
        let writes = case recovery of ReviewWake Investigations.ReviewApproved -> True; (ProviderWake; FinalProviderWake; WorkerWake; RuntimeWake) -> True; (ReviewWake Investigations.ReviewRejected; ExpiryWake) -> False
            writingRun = if abrupt then waiting.id else completed.id
        map (\page -> (page.version, AE.toJSON page.author)) pages `shouldBe` [(1, AE.toJSON $ Memory.Run writingRun) | writes]

  for_ ([(ReviewSuspension, 0, 1), (ProviderFailure, 1, 0), (FinalProviderFailure, 0, 1)] :: [(NativeInterruption, Int, Int)]) $ \(interruption, extraCalls, initialCallbacks) ->
    it ("nativeReview_resumeKeepsTheSdkTranscriptAndCompletedGuestEdit_andRejectsMissingManifest: " <> show interruption) \_ -> withTemp "native-review-resume" \directory -> do
      image <- nativeTestImage
      store <- storeIn directory
      let repository = directory </> "repository"
          uid n = UUIDId $ UUID.fromWords 61 0x4000 0x80000000 n
          sessionDirectory = store.usageDir </> toString (uid 1 :: UUIDId "pr_review").toText </> "sessions" </> toString (uid 4 :: UUIDId "ai_routine_run").toText
          suspendedDisks = filterM doesDirectoryExist . map (sessionDirectory </>) =<< listDirectory sessionDirectory
          draft = AE.object (graded <> ["findings" AE..= ([] :: [AE.Value]), "coverage" AE..= ([] :: [Text])])
          plan =
            [ SdkTool "bash" $ AE.object ["command" AE..= ("printf 'edit\\n' >> completed.txt; head -c 70000 /dev/zero | tr '\\0' x" :: Text)]
            , SdkTool "write_project_memory" $ AE.object ["pageId" AE..= ("00000000-0000-4000-8000-000000000099" :: Text), "version" AE..= (0 :: Int), "document" AE..= AE.object ["title" AE..= ("Resume context" :: Text), "body" AE..= ("Keep the original pending action." :: Text)]]
            , SdkTool "bash" $ AE.object ["command" AE..= ("test \"$(cat completed.txt)\" = \"$(printf 'original\\nedit')\" && echo completed-edit-preserved" :: Text)]
            , SdkTool "submit_review" draft
            , SdkText "Review submitted."
            ]
      initRepo repository
      commitSha <- commit repository [("completed.txt", "original\n")]
      commitRevision <- either (fail . show) pure $ Agent.mkRevision commitSha
      callbacks <- newIORef (0 :: Int)
      failProvider <- newIORef (interruption /= ReviewSuspension)
      let next replies
            | length replies == 2 = do
                firstReply <- maybe (fail "Completed tool result missing") pure $ viaNonEmpty head replies
                text <- either fail pure $ AET.parseEither (AE.withObject "Completed tool output" (AE..: "content")) firstReply
                let path = T.takeWhile (\c -> c /= ']' && c /= '\n') $ T.drop (T.length "Full output: ") $ snd $ T.breakOn "Full output: " text
                unless ("/run/monoscope-review-output/" `T.isPrefixOf` path && not ("'" `T.isInfixOf` path)) $ fail "Completed output did not retain its guest path"
                pure $ SdkTool "bash" $ AE.object ["command" AE..= ("test \"$(cat completed.txt)\" = \"$(printf 'original\\nedit')\" && test -s '" <> path <> "' && echo completed-edit-preserved-with-log")]
            | otherwise = pure $ fromMaybe (SdkText "Unexpected repeated model call") $ viaNonEmpty head $ drop (length replies) plan
      let provider _ replies = do
            failNow <- readIORef failProvider
            if failNow && length replies == (if interruption == FinalProviderFailure then 4 else 1)
              then writeIORef failProvider False $> SdkUnavailable "Interrupted after the completed guest edit"
              else (`SdkResponse` SdkTokens 7 0) <$> next replies
      withSdkProvider directory "resume-fixture" provider \models observed -> do
        let request = Agent.Request{projectId = testPid, runId = uid 1, attemptId = uid 2, repository = toText repository, base = commitSha, head = commitSha, gitToken = "fixture-git-secret-must-not-be-retained", prompt = "Resume the original review.", provider = "resume-fixture", model = "fixture", thinkingLevel = Agent.Off}
            native :: Eff '[Agent.CodeReview, IOE] a -> IO a
            native action = runEff $ sdkAgent store [models, image, directory </> "worker.log"] action
            invoke root configured =
              native
                $ Agent.reviewRepository
                  (Agent.Retained root $ pure $ Agent.ReviewContext Agent.ReviewInput{base = commitRevision, head = commitRevision, provider = configured.provider, model = "fixture", files = [], evidence = [], coverage = [], windowStart = frozenTime, windowEnd = frozenTime, prompt = configured.prompt} (Agent.ReviewMemory mempty []))
                  configured
                  (const $ pure $ Left "Unexpected telemetry")
                  ( \_ -> liftIO do
                      modifyIORef' callbacks (+ 1)
                      count <- readIORef callbacks
                      if interruption == ReviewSuspension && count == 1 then throwIO $ AI.AgentAwaitingReview (uid 3) frozenTime else pure $ Right $ AE.String "Original pending action completed"
                  )
                  (const pass)
                  (const pass)
                  (const pass)
                  (const pass)
                  noProofs
                  (const $ pure $ Right ())
            run = invoke (uid 4) request
        suspended <- tryAny run
        case (interruption, suspended) of
          (ReviewSuspension, Left err) -> case fromException err of
            Just (AI.AgentAwaitingReview identifier _) -> identifier `shouldBe` uid 3
            Nothing -> expectationFailure $ displayException err
          (ProviderFailure, Right (Left _)) -> pass
          (FinalProviderFailure, Right (Right value)) -> AE.eitherDecodeStrict @AE.Value (encodeUtf8 value) `shouldBe` Right draft
          _ -> do
            diagnostic <- readFileBS $ directory </> "worker.log"
            expectationFailure $ "Unexpected native recovery outcome: " <> show suspended <> "\n" <> decodeUtf8 diagnostic
        let initialCalls = if interruption == FinalProviderFailure then length plan else 2
            resumedCallbacks = initialCallbacks + if interruption == FinalProviderFailure then 0 else 1
        length <$> observed `shouldReturn` initialCalls
        doesFileExist (sessionDirectory </> "checkpoint.json") `shouldReturn` True
        if interruption == FinalProviderFailure
          then suspendedDisks `shouldReturn` []
          else do
            generations <- suspendedDisks
            length generations `shouldBe` 1
            generation <- maybe (fail "Native interruption did not retain its SDK/guest pair") pure $ viaNonEmpty head generations
            let transcript = generation </> "session.jsonl"
            frozen <- readFileBS transcript
            writeFileBS transcript $ frozen <> "\n"
            run >>= (`shouldSatisfy` isLeft)
            length <$> observed `shouldReturn` initialCalls
            readIORef callbacks `shouldReturn` initialCallbacks
            writeFileBS transcript frozen
        let manifestPath = sessionDirectory </> "checkpoint.json"
        manifestBytes <- readFileBS manifestPath
        transcriptBytes <- readFileBS $ sessionDirectory </> "active.jsonl"
        bracket_ (removeFile manifestPath) (Native.writePrivateArtifact sessionDirectory "checkpoint.json" $ fromStrict manifestBytes) do
          orphanedContext <- native $ Agent.readSessionContext (Agent.ReviewSubject request.runId) (uid 4)
          isLeft orphanedContext `shouldBe` True
          run >>= (`shouldSatisfy` isLeft)
          length <$> observed `shouldReturn` initialCalls
          readIORef callbacks `shouldReturn` initialCallbacks
          readFileBS (sessionDirectory </> "active.jsonl") `shouldReturn` transcriptBytes
        resumed <- run
        when (isLeft resumed) do
          diagnostic <- readFileBS $ directory </> "worker.log"
          expectationFailure $ show resumed <> "\n" <> decodeUtf8 diagnostic
        fmap (AE.eitherDecodeStrict @AE.Value . encodeUtf8) resumed `shouldBe` Right (Right draft)
        length <$> observed `shouldReturn` (length plan + extraCalls)
        suspendedDisks `shouldReturn` []
        repeated <- invoke (uid 4) (request{Agent.attemptId = uid 6} :: Agent.Request "pr_review")
        repeated `shouldBe` resumed
        changed <- invoke (uid 4) (request{Agent.prompt = "A different request must not reuse this session"} :: Agent.Request "pr_review")
        changed `shouldSatisfy` isLeft
        length <$> observed `shouldReturn` (length plan + extraCalls)
        checkpoint <- readFileBS $ store.usageDir </> toString request.runId.toText </> "sessions" </> toString (uid 4 :: UUIDId "ai_routine_run").toText </> "checkpoint.json"
        decodeUtf8 @Text checkpoint `shouldNotSatisfy` T.isInfixOf request.gitToken
        readIORef callbacks `shouldReturn` resumedCallbacks
        messages <- observed >>= traverse (either fail pure . AET.parseEither (AE.withObject "Resumed model" (AE..: "messages")))
        let results = concat messages :: [AE.Value]
            contents = mapMaybe (AET.parseMaybe $ AE.withObject "Completed guest tool" (AE..: "content")) results :: [Text]
        contents `shouldSatisfy` any (T.isInfixOf "completed-edit-preserved-with-log")
        fresh <- invoke (uid 5) request
        fresh `shouldBe` resumed
        length <$> observed `shouldReturn` (2 * length plan + extraCalls)
        readIORef callbacks `shouldReturn` (resumedCallbacks + 1)

  for_ [Just Investigations.ReviewApproved, Just Investigations.ReviewRejected, Nothing] $ \decision ->
    it ("nativeReview_pullRequestPublicationResumesRetainedAnalysis: " <> show decision) \tr -> do
      setup tr
      void $ deliver tr $ payload "opened" revision "2025-01-01T00:00:00Z"
      [review] <- runQueryEffect tr $ Reviews.latestRuns testPid
      Just routine <- runQueryEffect tr $ Issues.getEventRoutine testPid Issues.EventPullRequest
      runQueryEffect tr $ Issues.setRoutinePolicy routine.requestedBy testPid routine.conversationId $ Issues.ReviewSelected $ Catalog.ReviewSelection (one Catalog.ExternalPosts) Catalog.ReviewEditors
      let result = Impact.ReviewResult Impact.NoFinding [] [] [] frozenTime frozenTime Nothing Nothing Nothing Nothing Nothing Nothing
      runQueryEffect tr $ Reviews.saveResult review result
      dbExec tr "DELETE FROM background_jobs_long WHERE payload = ?" (Only $ Aeson $ AE.toJSON $ Jobs.ReviewPullRequest review.id)
      cfg <- githubApp tr
      let worker at = runTestBgRecordingHTTP at tr $ Reader.local (\ctx -> ctx{config = cfg}) $ pullRequestApi [Git.PullRequestFile "checkout.hs" "modified" (Just "@@ -1 +1 @@\n-old\n+new")] [] pass (pullRequestAt (T.replicate 40 "d") revision 1) $ BackgroundJobs.processBackgroundJob tr.trATCtx $ Jobs.ReviewPullRequest review.id
          publications = length . filter (T.isSuffixOf "/issues/7/comments" . fst)
          queued = dbOne tr "SELECT count(*)::int FROM background_jobs_long WHERE payload = ? AND status IN ('queued', 'retry')" (Only $ Aeson $ AE.toJSON $ Jobs.ReviewPullRequest review.id) :: IO Int
      (initial, ()) <- worker frozenTime
      publications initial `shouldBe` 0
      Just waiting <- runQueryEffect tr $ Issues.getEventRun testPid $ Issues.PullRequestReview review.id
      waiting.status `shouldBe` Issues.RunRunning
      [operation] <- runQueryEffect tr $ Investigations.listOperations @AI.OperationPolicy testPid waiting.id
      (_, page) <- testServant tr $ AIThreads.operationGetH testPid operation.id
      renderT page `shouldSatisfy` T.isInfixOf "Approve action"
      queued `shouldReturn` 1
      let resumedAt = maybe (addUTCTime (24 * 60 * 60 + 1) frozenTime) (const frozenTime) decision
      case decision of
        Just vote -> void $ testServant tr $ AIThreads.operationReviewPostH testPid operation.id $ AIThreads.OperationReviewForm vote
        Nothing -> pass
      Just leased <- runQueryEffect tr $ Reviews.getRun review.id
      bracket_
        (runQueryEffect tr (Reviews.claimRun (slots 100) leased) >>= (`shouldBe` True))
        (runQueryEffect tr $ Reviews.releaseRun leased)
        do
          dbExec tr "DELETE FROM background_jobs_long WHERE payload = ?" (Only $ Aeson $ AE.toJSON $ Jobs.ReviewPullRequest review.id)
          (busy, ()) <- worker resumedAt
          publications busy `shouldBe` 0
          Just stillWaiting <- runQueryEffect tr $ Issues.getEventRun testPid $ Issues.PullRequestReview review.id
          stillWaiting.status `shouldBe` Issues.RunRunning
          queued `shouldReturn` 1
      (resumed, ()) <- worker resumedAt
      (repeated, ()) <- worker resumedAt
      publications (initial <> resumed <> repeated) `shouldBe` if decision == Just Investigations.ReviewApproved then 1 else 0
      Just finished <- runQueryEffect tr $ Issues.getEventRun testPid $ Issues.PullRequestReview review.id
      (finished.id, finished.status) `shouldBe` (waiting.id, if decision == Just Investigations.ReviewApproved then Issues.RunNoFindings else Issues.RunCancelled)
      Just retained <- runQueryEffect tr $ Reviews.getRun review.id
      retained.result `shouldBe` Just (AE.toJSON result)
      dbOne tr "SELECT count(*)::int FROM projects.pr_review_attempts WHERE run_id = ?" (Only review.id) `shouldReturn` (0 :: Int)

  it "runs the checkout harness with atomic wiki receipts, publication recovery and Observe wiki reads without writes" \tr -> do
    reviewPid <- createTestProject tr "Impact review isolation"
    setupProject tr reviewPid
    dbExec tr "UPDATE projects.projects SET customer_id = 'cus_impact_usage', billing_provider = ? WHERE id = ?" (Projects.StripeProvider, reviewPid)
    runQueryEffect tr $ Reviews.updateSettings reviewPid (Reviews.ReviewSettings "impact-org" "checkout" Reviews.ReadyPullRequests True)
    void $ deliver tr $ payload "opened" revision "2025-01-01T00:00:00Z"
    [run] <- runQueryEffect tr $ Reviews.latestRuns reviewPid
    apiKey <- createTestAPIKey tr reviewPid "Impact review telemetry"
    ingestLog tr apiKey ("long-review-log-body" <> T.replicate 9000 "x") (addUTCTime (-60) frozenTime)
    ingestLog tr apiKey "another-in-scope-event" (addUTCTime (-120) frozenTime)
    ingestTrace tr apiKey "checkout-review-span" (addUTCTime (-180) frozenTime)
    ingestMetric tr apiKey [] [] "review.temperature" 42 (addUTCTime (-60) frozenTime)
    ingestLog tr apiKey "outside-review-window" (addUTCTime (-(8 * 86400)) frozenTime)
    outsideProject <- createTestProject tr "Outside impact review"
    outsideKey <- createTestAPIKey tr outsideProject "Outside review telemetry"
    ingestLog tr outsideKey "outside-review-project" (addUTCTime (-60) frozenTime)
    ingestLogWithResource tr apiKey [("service.name", "unmapped-service")] "outside-review-service" (addUTCTime (-60) frozenTime)
    appConfig <- githubApp tr
    let cfg = appConfig{impactReviewUsageDir = Just "fixture-usage", impactReviewCacheDir = Just "fixture-cache", impactReviewProvider = "fixture", openaiModel = "gpt-5.6-luna#high"}
        files = [Git.PullRequestFile ("unrelated-" <> show n <> ".hs") "modified" (Just "@@ -1 +1 @@\n-old\n+new") | n <- [1 :: Int .. 100]] <> [Git.PullRequestFile "checkout.hs" "modified" (Just "@@ -1 +1 @@\n-old\n+new")]
        prAt reviewedHead = pullRequestAt (T.replicate 40 "d") reviewedHead (length files)
        marker = "<!-- monoscope-impact:" <> reviewPid.toText <> " -->"
        appComment (n :: Int) (app :: Int) body = AE.object ["id" AE..= n, "body" AE..= (body :: Text), "performed_via_github_app" AE..= AE.object ["id" AE..= app]]
        transport :: Bool -> ATBackgroundCtx a -> ATBackgroundCtx a
        transport hasComment = pullRequestApi files [appComment n app marker | hasComment, (n, app) <- [(88, 456), (99, 123)]] pass (prAt revision)
        result = Impact.ReviewResult Impact.WorthChecking [Impact.Finding (Impact.ChangedLine "checkout.hs" 1 Impact.Head) "The changed operation may stop emitting the signal used by the monitor. *check* @impact-review-test [outside](https://untrusted.invalid)\r#heading" "Check the monitor query before deploying." ["telemetry-1"]] [] [] frozenTime frozenTime Nothing (Just "Moves retry handling into the checkout operation. @impact-review-test | the monitor may lose its signal.") (Just [Impact.Assessment d (if d == Impact.ErrorDetection then Impact.Poor else Impact.Good) ["New path emits no span | table-breaking gap" | d == Impact.ErrorDetection] | d <- universe]) (Just []) Nothing Nothing
        resultText = pure . Right . jsonText
        fakeAgent :: ATBackgroundCtx a -> ATBackgroundCtx a
        fakeAgent = fakeAgentWith []
        fakeAgentWith :: [FilePath] -> ATBackgroundCtx a -> ATBackgroundCtx a
        fakeAgentWith arguments =
          interpose @LLM.LLM (\_ _ -> liftIO $ fail "Legacy LLM loop called instead of checkout harness")
            . interpose @Agent.CodeReview \env -> \case
              Agent.ReviewRepository _ request respond knowledge recordCall recordUsage recordAllocation recordCheck hooks check -> localSeqUnliftIO env \unlift ->
                liftIO $ withSystemTempDirectory "impact-review-response" \directory -> do
                  request.prompt `shouldNotSatisfy` T.isInfixOf "private-log-body-must-not-leave-monoscope"
                  dbOne tr "SELECT count(*)::int FROM projects.pr_review_attempts WHERE id = ? AND run_id = ? AND project_id = ? AND provider = ?" (request.attemptId, request.runId, request.projectId, request.provider) `shouldReturn` (1 :: Int)
                  for_ [("--expect-recipe", True), ("--expect-no-recipe", False)] \(flag, expected) -> when (flag `elem` arguments) $ ("saved proof setup" `T.isInfixOf` request.prompt) `shouldBe` expected
                  when ("--prove-live" `elem` arguments) $ traverse_ (unlift . hooks.progress) [Agent.Planned ("Shows the retry count" :| ["Retries once"]), Agent.Started ("Shows the retry count" :| []), Agent.Recorded ("Shows the retry count" :| []) "proof-1"]
                  when ("--billing-change-before-usage" `elem` arguments) $ dbExec tr "UPDATE projects.projects SET customer_id = 'cus_after_attempt', billing_provider = ? WHERE id = ?" (Projects.LemonSqueezyProvider, reviewPid)
                  when (null arguments) do
                    title <- either (fail . toString) pure $ Memory.mkMemoryText "Review context"
                    body <- either (fail . toString) pure $ Memory.mkMemoryText "Retry handling needs caller and telemetry evidence; recheck this dated note."
                    let page = case request.attemptId of UUIDId value -> UUIDId value
                        write = unlift $ knowledge $ Agent.ProjectMemory $ Memory.Write $ Memory.Edit page 0 $ Memory.Document title body
                        ddl statement = dbExec tr statement ()
                    existing <- runQueryEffect tr $ Memory.latestPages reviewPid
                    when (null existing)
                      $ bracket_
                        (ddl "ALTER TABLE apis.ai_agent_operations ADD CONSTRAINT reject_wiki_receipt CHECK (proposal #>> '{call,contents,tag}' <> 'WriteWiki' OR outcome->>'tag' <> 'OperationReturned') NOT VALID")
                        (ddl "ALTER TABLE apis.ai_agent_operations DROP CONSTRAINT reject_wiki_receipt")
                        do
                          tryAny write >>= (`shouldSatisfy` isLeft)
                          length <$> runQueryEffect tr (Memory.pageHistory reviewPid page) `shouldReturn` 0
                          Just execution <- runQueryEffect tr $ Issues.getEventRun reviewPid $ Issues.PullRequestReview request.runId
                          length <$> runQueryEffect tr (Investigations.listOperations @AI.OperationPolicy reviewPid execution.id) `shouldReturn` 0
                    saved <- write
                    saved `shouldSatisfy` isRight
                    write `shouldReturn` saved
                    unlift (knowledge $ Agent.ProjectMemory $ Memory.Read page) `shouldReturn` saved
                    -- Open issues and monitors come back as lists, scoped to the review's mapped services.
                    for_ [Agent.OpenIssues, Agent.ServiceMonitors] \signal -> unlift (knowledge signal) >>= (`shouldSatisfy` \case Right (AE.Array _) -> True; _ -> False)
                  let response = directory </> "result.json"
                  writeFileLBS response $ AE.encode result{Impact.coverage = ["Related test toolchain unavailable"]}
                  reply <- runEff $ scripted Nothing "test/fixtures/impact-review-agent.mjs" ([response, toString reviewPid.toText] <> arguments) $ Agent.reviewRepository Agent.Isolated request (liftIO . unlift . respond) (liftIO . unlift . knowledge) (liftIO . unlift . recordCall) (liftIO . unlift . recordUsage) (liftIO . unlift . recordAllocation) (liftIO . unlift . recordCheck) (Agent.hoistHooks (liftIO . unlift) hooks) (liftIO . unlift . check)
                  diagnostic <- tryAny $ decodeUtf8 <$> readFileBS (response <> ".failure")
                  pure $ first (<> either (const "") ("\n" <>) diagnostic) reply
              operation -> pure $ refuse operation
        within :: (ATBackgroundCtx a -> ATBackgroundCtx a) -> ATBackgroundCtx a -> IO ([(Text, LByteString)], a)
        within wrap = runTestBgRecordingHTTP frozenTime tr . Reader.local (\ctx -> ctx{config = cfg}) . wrap
        -- A retried review through the PR transport; @wrap@ installs the agent and any extra responses.
        review :: (ATBackgroundCtx () -> ATBackgroundCtx ()) -> IO [(Text, LByteString)]
        review wrap = retry >> fst <$> within (transport True . wrap) (Impact.reviewPullRequest run.id)
        attempt :: (ATBackgroundCtx (Either SomeException ()) -> ATBackgroundCtx (Either SomeException ())) -> IO ([(Text, LByteString)], Either SomeException ())
        attempt wrap = retry >> within (transport True . wrap) (tryAny $ Impact.reviewPullRequest run.id)
        retry = runQueryEffect tr $ Reviews.retryRun reviewPid run.id
        sentTo path = T.intercalate " " . map (decodeUtf8 . snd) . filter (T.isInfixOf path . fst)
        reaches path = any (T.isInfixOf path . fst)
        reviewState = fmap (.state) <$> runQueryEffect tr (Reviews.getRun run.id)
        queryResult = result{Impact.evidence = [Impact.Evidence "telemetry-1" Impact.Monitor Nothing Nothing "/monitor" (Just "private-customer-query") Nothing Nothing Nothing]}
    for_ @[] [Impact.GitHubComment False "/run" Nothing, Impact.GitHubComment True "/run" Nothing, Impact.MonoscopeView] \audience -> Impact.renderReview cfg.hostUrl run audience queryResult `shouldNotSatisfy` T.isInfixOf "private-customer-query"
    (requests, ()) <- within (transport True . fakeAgent) $ Impact.reviewPullRequest run.id
    requests `shouldNotSatisfy` reaches "/contents/"
    requests `shouldSatisfy` reaches "/files?per_page=100&page=2"
    visibleRuns <- runQueryEffect tr $ Issues.listRoutineRuns reviewPid 12
    visibleRuns `shouldSatisfy` (not . null)
    for_ visibleRuns \visible -> do
      visible.status `shouldBe` Issues.RunSucceeded
      runQueryEffect tr (Memory.latestPages reviewPid) >>= (\pages -> map (.author) pages `shouldBe` [Memory.Run visible.id])
      runQueryEffect tr (Issues.listArtifacts reviewPid visible.conversationId Nothing) >>= (`shouldSatisfy` (not . null))
      (_, detail) <- testServant tr $ AIThreads.routineRunGetH reviewPid visible.id Nothing
      -- The run page draws the review's evidence as live widgets, not only prose.
      renderT detail `shouldContainAll` ["write project memory", "publish pull request comment", "Merge risk: low", "Observed service events"]
    length (filter (T.isInfixOf "/issues/comments/99" . fst) requests) `shouldBe` 1
    let published = sentTo "/issues/comments/99" requests
    published `shouldContainAll` ["Merge risk: low", "Worth checking", "Related test toolchain unavailable", "production query reached its result limit", "test-service](", "/widget.png?widgetZ=", "![Review query results]", "\\\\*check", revision]
    for_ @[] ["@impact-review-test", "https://untrusted.invalid", "long-review-log-body", "another-in-scope-event", "checkout-review-span", "Review temperature gauge"] \hidden -> published `shouldNotSatisfy` T.isInfixOf hidden
    completed <- maybe (fail "run missing") pure =<< runQueryEffect tr (Reviews.getRun run.id)
    (completed.state, completed.commentId) `shouldBe` (Reviews.Completed, Just 99)
    checks <- runQueryEffect tr $ Reviews.latestChecks reviewPid
    length checks `shouldBe` 2
    Reviews.RecordedCheck checkRun (Aeson execution) <- maybe (fail "Base execution missing") pure $ find (\item -> case item.receipt of Aeson receipt -> receipt.revision == T.replicate 40 "d") checks
    checkRun `shouldBe` run.id
    retained <- runQueryEffect tr $ Reviews.priorKnowledge run "test-service"
    map (\note -> (note.revision, note.evidenceKeys, note.windowEnd)) retained `shouldBe` [(revision, V.singleton "query-6", frozenTime)]
    snapshot <- jsonText . getAeson <$> (dbOne tr "SELECT evidence FROM projects.pr_review_knowledge WHERE run_id = ?" (Only run.id) :: IO (Aeson AE.Value))
    snapshot `shouldSatisfy` T.isInfixOf "review.temperature"
    snapshot `shouldNotSatisfy` T.isInfixOf "dataset"
    runQueryEffect tr (Reviews.priorKnowledge run{Reviews.repo = "another-repository"} "test-service") >>= (`shouldSatisfy` null)
    (execution.durationMs, execution.revision, AE.toJSON execution.status) `shouldBe` (1250, T.replicate 40 "d", AE.String "failed")
    dbExec tr "UPDATE projects.pr_review_checks SET receipt = receipt - 'regression' - 'workspace' WHERE id = ?" (Only execution.id)
    traverse (\receipt -> runQueryEffect tr $ Reviews.recordCheck run receipt) [execution{Agent.regression = Nothing}, execution{Agent.regression = Just $ Agent.RetainedTest $ T.replicate 64 "a"}, (execution{Agent.durationMs = 9999} :: Agent.CheckReceipt)] `shouldReturn` [True, False, False]
    totals <- runQueryEffect tr $ Reviews.usageTotals reviewPid
    let projectUsage = find (isNothing . (.runId)) totals
        runUsage = find ((== Just run.id) . (.runId)) totals
    fmap (\u -> (u.usage.calls, u.usage.failedCalls, u.usage.unpricedCalls, u.usage.costNanousd)) projectUsage `shouldBe` Just (2, 1, 1, 1234000)
    fmap (.usage.calls) runUsage `shouldBe` Just 2
    fmap (\total -> (total.resources.checks, total.resources.vcpuMs, total.resources.memoryMiBMs, total.resources.logBytes, total.resources.explanations)) runUsage `shouldBe` Just (2, 9000, 4608000, 46, 0)
    emptyPid <- createTestProject tr "Empty usage project"
    runQueryEffect tr (Reviews.latestChecks emptyPid) >>= (`shouldSatisfy` null)
    runQueryEffect tr (Reviews.priorKnowledge run{Reviews.projectId = emptyPid} "test-service") >>= (`shouldSatisfy` null)
    runQueryEffect tr (Reviews.recordCheck run{Reviews.projectId = emptyPid} execution) `shouldReturn` False
    sum . map (.usage.calls) <$> runQueryEffect tr (Reviews.usageTotals emptyPid) `shouldReturn` 0
    (_, history) <- testServant tr $ CodeContextPage.repositoriesGetH reviewPid (Just CodeContextPage.PullRequests) Nothing
    let historyText = renderT history
    historyText `shouldContainAll` ["$0.001234 estimated", "1 unpriced", "not customer charges", "Base: assertion failed", "Head: passed", "1250 ms", "Compute cost is unpriced", "Service knowledge from this review", "Review temperature gauge", "1 finding to check", "Model and sandbox usage for this project"]
    T.count ("Check " <> T.take 8 execution.checkId.toText) historyText `shouldBe` 1
    dbOne tr "SELECT count(*)::bigint FROM projects.ai_usage WHERE project_id = ?" (Only reviewPid) `shouldReturn` (0 :: Int64)
    dbOne tr "SELECT count(*)::bigint FROM projects.pr_review_usage u JOIN projects.projects p ON p.id = u.project_id WHERE u.project_id = ? AND u.customer_id = 'cus_impact_usage' AND u.customer_id = p.customer_id AND u.billing_provider = p.billing_provider AND (u.receipt->>'inputTokens')::bigint = 12 AND (u.receipt->>'cacheReadTokens')::bigint = 13" (Only reviewPid) `shouldReturn` (2 :: Int64)
    dbOne tr "SELECT count(*)::bigint FROM projects.pr_review_checks WHERE project_id = ? AND customer_id = 'cus_impact_usage'" (Only reviewPid) `shouldReturn` (2 :: Int64)
    let customerProjects = [("Included customer project", Just "cus_impact_usage"), ("Viewer customer project", Just "cus_impact_usage"), ("Inactive customer project", Just "cus_impact_usage"), ("Unlinked one", Nothing), ("Unlinked two", Just "")] :: [(Text, Maybe Text)]
    related <- forM (zip [1 :: Word32 ..] customerProjects) \(n, (title, cid)) -> do
      pid <- createTestProject tr title
      dbExec tr "UPDATE projects.projects SET customer_id = ?, billing_provider = ? WHERE id = ?" (cid, Projects.StripeProvider, pid)
      credential <- maybe (fail "credential missing") pure =<< runQueryEffect tr (GitSync.upsertGitHubCredential (encodeUtf8 cfg.apiKeyEncryptionSecretKey) pid Git.GitHub Nothing "impact-org" (Just 987654) Nothing)
      runQueryEffect tr $ CodeContext.insertCodeMapping pid credential.id (Git.RepoRef "impact-org" "checkout" "main") Nothing "" ""
      pure (pid, n)
    void $ deliver tr $ payload "opened" revision "2025-01-01T00:00:00Z"
    receipt <- dbOne tr "SELECT receipt FROM projects.pr_review_usage WHERE project_id = ? AND cost_nanousd IS NOT NULL LIMIT 1" (Only reviewPid) <&> \(Aeson value) -> value :: Agent.ModelUsage
    for_ related \(pid, n) -> do
      [relatedRun] <- runQueryEffect tr $ Reviews.latestRuns pid
      let observed = receipt{Agent.id = UUIDId $ UUID.fromWords 42 0 0 n, Agent.attemptId = UUIDId $ UUID.fromWords 43 0 0 n, Agent.responseId = Just $ "response-" <> show n}
      runQueryEffect tr (Reviews.recordUsage relatedRun observed) `shouldReturn` True
      when (n == 5) do
        dbExec tr "UPDATE projects.projects SET billing_provider = ? WHERE id = ?" (Projects.LemonSqueezyProvider, pid)
        runQueryEffect tr (Reviews.recordUsage relatedRun observed{Agent.id = UUIDId $ UUID.fromWords 44 0 0 n, Agent.responseId = Just "second-response"}) `shouldReturn` True
      when (n == 2) $ dbExec tr "UPDATE projects.project_members SET permission = ? WHERE project_id = ?" (ProjectMembers.PView, pid)
      when (n == 3) $ dbExec tr "UPDATE projects.project_members SET active = false WHERE project_id = ?" (Only pid)
      when (n == 1) do
        [credential] <- runQueryEffect tr $ GitSync.getGitHubCredentials pid
        runQueryEffect tr $ CodeContext.insertCodeMapping pid credential.id (Git.RepoRef "impact-org" "noisy" "main") Nothing "" ""
        event <- either (fail . show) pure $ Reviews.decodeEvent $ toStrict $ AE.encode $ payload "opened" revision "2025-01-01T00:00:00Z"
        runQueryEffect tr $ for_ [8 .. 28] \number -> Reviews.receiveEvent Nothing (event{Reviews.number = number, Reviews.repo = "noisy"} :: Reviews.PullRequestEvent)
    runQueryEffect tr (Reviews.latestChecks reviewPid) >>= (\executions -> length executions `shouldBe` 2)
    let userId = (getResponse tr.trSessAndHeader :: Projects.Session).user.id
    customerRows <- runQueryEffect tr $ Reviews.customerUsage userId
    (length customerRows, sum (map (.usage.calls) customerRows), sum (map (.resources.vcpuMs) customerRows)) `shouldBe` (5, 6, 9000)
    dbExec tr "UPDATE users.users SET active = false WHERE id = ?" (Only userId)
    length <$> runQueryEffect tr (Reviews.customerUsage userId) `shouldReturn` 0
    dbExec tr "UPDATE users.users SET active = true WHERE id = ?" (Only userId)
    usageTab <- either (fail . toString) pure $ parseUrlPiece @CodeContextPage.RepositoryTab "usage"
    (_, usagePage) <- testServant tr $ CodeContextPage.repositoriesGetH reviewPid (Just usageTab) Nothing
    let usagePageText = renderT usagePage
    usagePageText `shouldContainAll` ["$0.002468 estimated", "active administrator", "attributed on receipt (no attempt snapshot)"]
    T.count "No billing customer linked" usagePageText `shouldBe` 2
    for_ related \(pid, n) -> T.count ("href=\"/p/" <> pid.toText <> "/repositories?tab=reviews\"") usagePageText `shouldBe` if n `elem` [2, 3] then 0 else 1

    saved <- maybe (fail "review result missing") (parse AE.parseJSON) completed.result
    let measured = filter ((== Impact.Telemetry) . (.kind)) (saved :: Impact.ReviewResult).evidence
    for_ measured \item -> whenJust item.chartUrl \chart -> do
      let compressed = T.takeWhile (/= '&') $ T.drop 8 $ snd $ T.breakOn "widgetZ=" chart
      widgetJSON <- maybe (fail "invalid evidence widget") pure =<< Widget.decodeWidgetZ (decodeUtf8 $ H.urlDecode True $ encodeUtf8 compressed)
      widget <- either fail pure $ AE.eitherDecodeStrict @Widget.Widget $ encodeUtf8 widgetJSON
      if item.key == "telemetry-1"
        then widget.query `shouldSatisfy` maybe False (\query -> all (`T.isInfixOf` query) ["timestamp >= \"2024-12-25T00:00:00Z\"", "timestamp <= \"2025-01-01T00:00:00Z\""])
        else do
          (widget.query, isJust widget.dataset, widget.pngProfile) `shouldBe` (Nothing, True, Just Widget.PngSlack)
          item.url `shouldBe` T.dropWhileEnd (== '/') cfg.hostUrl <> "/p/" <> reviewPid.toText <> "/settings/code-mappings"
      when (item.key == "query-1") do
        pngs <- forM [2, 3 :: Int] \count -> do
          let encoded = jsonText widget{Widget.dataset = Just (def :: Widget.WidgetDataset){Widget.source = AE.toJSON ([[AE.String "events"], [AE.toJSON count]] :: [[AE.Value]])}}
              signature = Widget.signWidgetUrl tr.trATCtx.env.apiKeyEncryptionSecretKey reviewPid encoded
          bytes <- getResponse <$> toBaseServantResponse tr (Routes.widgetPngGetH reviewPid (Just encoded) Nothing Nothing Nothing Nothing Nothing Nothing (Just signature) [])
          LBS.take 8 bytes `shouldBe` "\137PNG\r\n\26\n"
          pure bytes
        -- The first aggregate column used to become an invisible time axis.
        length (ordNub pngs) `shouldBe` 2
        fmap (.source) widget.dataset `shouldBe` Just (AE.toJSON ([[AE.String "events", AE.String "percent"], [AE.Number 3, AE.Number 100]] :: [[AE.Value]]))
        widgetQuery <- maybe (fail "widget query missing") pure item.query
        ast <- either (fail . toString) pure $ KQL.parseQueryToAST widgetQuery
        replayed <- runQueryEffect tr $ selectLogTable False reviewPid ast widgetQuery Nothing (Just $ addUTCTime (-(30 * 86400)) frozenTime, Just $ addUTCTime 86400 frozenTime) [] Nothing Nothing Nothing Nothing
        (rows, _, _) <- either (fail . toString) pure replayed
        map V.toList (V.toList rows) `shouldBe` [[AE.Number 3, AE.Number 100]]
    expectedChart <-
      runQueryEffect tr
        $ Widget.widgetPngUrl
          tr.trATCtx.env.apiKeyEncryptionSecretKey
          cfg.hostUrl
          reviewPid
          (def{Widget.wType = Widget.WTTimeseries, Widget.title = Just "Observed service events", Widget.query = Just "resource.service.name == \"test-service\" and timestamp >= \"2024-12-25T00:00:00Z\" AND timestamp <= \"2025-01-01T00:00:00Z\" | summarize events = count() by bin_auto(timestamp)", Widget.timeFrom = Just "2024-12-25T00:00:00Z", Widget.timeTo = Just "2025-01-01T00:00:00Z", Widget.pngProfile = Just Widget.PngSlack})
          Nothing
          (Just "2024-12-25T00:00:00Z")
          (Just "2025-01-01T00:00:00Z")
    map (.chartUrl) (filter (isNothing . (.queryResult)) measured) `shouldBe` [expectedChart]
    length (filter (isJust . (.queryResult)) measured) `shouldBe` 8
    -- A worker can stop after recording the GitHub publication but before
    -- finishing its routine ledger. A replay must repair that ledger locally.
    dbExec tr "UPDATE apis.ai_routine_runs SET status = 'running', finished_at = NULL WHERE project_id = ?" (Only reviewPid)
    dbExec tr "DELETE FROM apis.ai_routine_run_artifacts WHERE run_id IN (SELECT id FROM apis.ai_routine_runs WHERE project_id = ?)" (Only reviewPid)
    fst <$> within (transport True . fakeAgent) (replicateM_ 2 $ Impact.reviewPullRequest run.id) `shouldReturn` []
    recovered <- runQueryEffect tr $ Issues.listRoutineRuns reviewPid 12
    map (.status) recovered `shouldBe` [Issues.RunSucceeded]
    for_ recovered \visible -> runQueryEffect tr (Issues.listRunArtifacts reviewPid visible.id) >>= (`shouldSatisfy` (not . null))
    dbOne tr "SELECT count(*)::int FROM apis.ai_chat_messages WHERE project_id = ? AND role = 'assistant'" (Only reviewPid) `shouldReturn` (1 :: Int)
    dbExec tr "UPDATE apis.ai_routine_runs SET status = 'running', finished_at = NULL WHERE project_id = ?" (Only reviewPid)
    dbExec tr "UPDATE projects.pr_review_runs SET result = ? WHERE id = ?" (Aeson $ AE.object ["invalid" AE..= True], run.id)
    fst <$> runTestBgRecordingHTTP frozenTime tr (Impact.reviewPullRequest run.id) `shouldReturn` []
    map (.status) <$> runQueryEffect tr (Issues.listRoutineRuns reviewPid 12) `shouldReturn` [Issues.RunFailed]
    -- Reruns recover the app-owned comment and keep production prose out of links-only output.
    void $ Test.call tr $ CodeContextPage.impactReviewSettingsPostH reviewPid (Reviews.ReviewSettings "impact-org" "checkout" Reviews.ReadyPullRequests False)
    void $ Test.call tr $ CodeContextPage.impactReviewRetryH reviewPid run.id
    retryPayloads <- dbQuery tr "SELECT payload FROM background_jobs_long WHERE payload->>'tag' = 'ReviewPullRequest' AND payload->>'contents' = ?" (Only run.id.toText) :: IO [Only (Aeson AE.Value)]
    retryPayloads `shouldBe` replicate 2 (Only $ Aeson $ AE.toJSON $ Jobs.ReviewPullRequest run.id)
    (linksRequests, ()) <- within (transport True . fakeAgentWith ["--expect-knowledge"]) $ Impact.reviewPullRequest run.id
    runQueryEffect tr (Reviews.priorKnowledge run "test-service") >>= (\notes -> length notes `shouldBe` 1)
    let linksPublished = sentTo "/issues/comments/99" linksRequests
    linksPublished `shouldSatisfy` T.isInfixOf "production evidence are in the [full review]("
    for_ @[] [": query results", "/widget.png", "The changed operation", "Related test toolchain unavailable"] \hidden -> linksPublished `shouldNotSatisfy` T.isInfixOf hidden
    -- Final publication validation also rejects fabricated locations if the effect is bypassed.
    retry
    let invalid = result{Impact.findings = [Impact.Finding (Impact.ChangedLine "unrelated.hs" 1 Impact.Head) "Invented mechanism" "Invented next step" ["telemetry-1"]]}
    (newRequests, ()) <- within (transport False . repositoryReview \_ _ _ _ -> resultText invalid) $ Impact.reviewPullRequest run.id
    let created = sentTo "/issues/7/comments" newRequests
    created `shouldSatisfy` T.isInfixOf "Coverage unknown"
    created `shouldNotSatisfy` T.isInfixOf "Invented mechanism"
    let incomplete = do
          failedRun <- maybe (fail "run missing") pure =<< runQueryEffect tr (Reviews.getRun run.id)
          (failedRun.state, failedRun.error) `shouldBe` (Reviews.Incomplete, Just "Review failed; retry to collect current evidence")
          pure failedRun
    for_ @[] [True, False] \unexpected -> do
      retry
      tryAny (within (if unexpected then withHTTPResponses (\_ _ -> throwIO $ userError "private-exception-detail") else Reader.local (\ctx -> ctx{config = cfg{githubAppPrivateKey = ""}})) $ Impact.reviewPullRequest run.id) >>= (`shouldSatisfy` isLeft)
      failedRun <- incomplete
      runQueryEffect tr (Reviews.claimRun (slots 100) failedRun) `shouldReturn` True
      runQueryEffect tr $ Reviews.releaseRun failedRun
    (failedRequests, failedAgent) <- attempt $ repositoryReview \_ _ _ _ -> pure $ Left "private-agent-failure"
    failedAgent `shouldSatisfy` isLeft
    failedRequests `shouldNotSatisfy` reaches "/issues/"
    void incomplete
    for_ [CodeContextPage.PullRequests, CodeContextPage.Usage] \tab -> do
      (_, interrupted) <- testServant tr $ CodeContextPage.repositoriesGetH reviewPid (Just tab) Nothing
      renderT interrupted `shouldSatisfy` T.isInfixOf "attempts without model receipts:"
    let missing = sum . map (.usage.unreportedAttempts) . filter ((== Just run.id) . (.runId))
    missingUsage <- runQueryEffect tr $ Reviews.usageTotals reviewPid
    missing missingUsage `shouldSatisfy` (> 0)
    interruptedAttempt <- dbOne tr "SELECT a.id FROM projects.pr_review_attempts a WHERE a.run_id = ? AND NOT EXISTS (SELECT 1 FROM projects.pr_review_usage u WHERE u.attempt_id = a.id) ORDER BY a.started_at DESC, a.id DESC LIMIT 1" (Only run.id)
    runQueryEffect tr (Reviews.recordUsage run receipt{Agent.id = UUIDId $ UUID.fromWords 71 0 0 1, Agent.attemptId = interruptedAttempt, Agent.costNanousd = Nothing, Agent.succeeded = False}) `shouldReturn` True
    missing <$> runQueryEffect tr (Reviews.usageTotals reviewPid) `shouldReturn` missing missingUsage - 1
    for_ @[] ["--fail-after-usage", "--conflicting-usage", "--skip-submission", "--tampered-result", "--check-after-submission", "--wrong-check-revision", "--foreign-allocation", "--zero-allocation"] \failure -> do
      let measuredCost = sum . map (.usage.costNanousd) . filter (isNothing . (.runId))
          ledger = (,,) <$> (measuredCost <$> runQueryEffect tr (Reviews.usageTotals reviewPid)) <*> (length <$> runQueryEffect tr (Reviews.latestChecks reviewPid)) <*> (length <$> runQueryEffect tr (Reviews.priorKnowledge run "test-service"))
      (cost, checked, known) <- ledger
      (failureRequests, processFailure) <- attempt $ fakeAgentWith [failure]
      processFailure `shouldSatisfy` isLeft
      failureRequests `shouldNotSatisfy` reaches "/issues/"
      ledger `shouldReturn` if failure == "--wrong-check-revision" then (cost, checked, known) else (cost + 1234000, checked + 2, known)

    void $ review fakeAgent
    reviewState `shouldReturn` Just Reviews.Completed
    fmap (.error) <$> runQueryEffect tr (Reviews.getRun run.id) `shouldReturn` Just Nothing
    -- reviewRetries_stopAfterThreeInOneChain (#820): a deferring node requeues with fresh odd-jobs attempts, so
    -- without a per-chain budget a failing review retried 54 times.
    retry
    failures <- replicateM 3 $ snd <$> within (transport True . fakeAgentWith ["--fail-after-usage"]) (tryAny $ Impact.reviewPullRequest run.id)
    map isLeft failures `shouldBe` [True, True, False]
    void $ review fakeAgent
    -- reask_afterCompletedReview_startsFresh (#820): the finished chain's model calls are not a lost session to
    -- the next ask, and a lost one ends the chain instead of retrying. The real agent records calls; this fixture does not.
    finishedAttempt <- dbOne tr "SELECT id FROM projects.pr_review_attempts WHERE run_id = ? ORDER BY started_at DESC, id DESC LIMIT 1" (Only run.id)
    runQueryEffect tr (Usage.recordCall reviewPid (Usage.Review run.id) $ Agent.ModelCall (UUIDId $ UUID.fromWords 72 0 0 1) finishedAttempt "fixture" "fixture") `shouldReturn` True
    void $ review fakeAgent
    reviewState `shouldReturn` Just Reviews.Completed
    retry
    lost <- replicateM 2 $ snd <$> within (transport True . fakeAgentWith ["--fail-after-usage"]) (tryAny $ Impact.reviewPullRequest run.id)
    map isLeft lost `shouldBe` [True, False]
    void $ review fakeAgent
    -- askedProof_postsANewComment (#826): an ask's answer lands below it, not in the review comment far above.
    dbExec tr "UPDATE projects.pr_review_runs SET request = ? WHERE id = ?" (Aeson $ AE.toJSON Reviews.Ask{login = "ada", commentId = 77, command = Reviews.Prove, focus = Nothing}, run.id)
    askRequests <- review fakeAgent
    (askRequests `shouldSatisfy` reaches "/issues/7/comments") >> (askRequests `shouldNotSatisfy` reaches "/issues/comments/99")
    -- savedProofSetup_followsItsProofMd: a saved setup is offered until .monoscope/PROOF.md changes after it.
    dbExec tr "INSERT INTO projects.pr_review_proof_recipes (project_id, owner, repo, setup, revision, run_id) VALUES (?, 'impact-org', 'checkout', 'make serve &', ?, ?)" (reviewPid, T.replicate 40 "e", run.id)
    let comparedFiles paths = withHTTPResponses \_ endpoint -> pure if "/compare/" `T.isInfixOf` toText endpoint then Just $ AE.encode $ AE.object ["files" AE..= [AE.object ["filename" AE..= path, "status" AE..= ("modified" :: Text)] | path <- paths :: [Text]]] else Nothing
    void $ review $ comparedFiles ["src/app.hs"] . fakeAgentWith ["--expect-recipe"]
    void $ review $ comparedFiles ["src/app.hs", ".monoscope/PROOF.md"] . fakeAgentWith ["--expect-no-recipe"]
    dbExec tr "DELETE FROM projects.pr_review_proof_recipes WHERE project_id = ?" (Only reviewPid)
    -- askedProof_updatesOneCommentLive: each proof step edits the comment the ask created, and the
    -- finished review replaces it there rather than posting a second one.
    liveRequests <- review $ fakeAgentWith ["--prove-live"]
    let sent suffix = [body | (url, body) <- liveRequests, suffix `T.isSuffixOf` url]
    liveBodies <- traverse (either fail pure . (AE.eitherDecode >=> AET.parseEither (field "body"))) (sent "/issues/7/comments" <> sent "/issues/comments/99")
    length (sent "/issues/7/comments") `shouldBe` 1
    map (\body -> (T.isInfixOf "*Pending*" body, T.isInfixOf "*Running*" body, T.isInfixOf "still running" body)) liveBodies `shouldBe` [(True, False, True), (True, True, True), (True, False, True), (False, False, False)]
    dbExec tr "UPDATE projects.pr_review_runs SET request = NULL, progress_comment_id = NULL, proof_progress = '[]' WHERE id = ?" (Only run.id)
    let rejectedPublication = withHTTPResponses \_ endpoint -> if "/issues/comments/99" `T.isSuffixOf` toText endpoint then throwIO $ userError "Publication transport interrupted" else pure Nothing
        sentBody recorded = do
          [(_, encoded)] <- pure $ filter (T.isInfixOf "/issues/comments/99" . fst) recorded
          either fail pure $ AE.eitherDecode encoded >>= AET.parseEither (field "body")
        lostPublication = retry >> within (rejectedPublication . transport True . fakeAgent) (tryAny $ Impact.reviewPullRequest run.id)
    (rejectedRequests, rejected) <- lostPublication
    rejected `shouldSatisfy` isLeft
    Just rejectedRun <- runQueryEffect tr $ Issues.getEventRun reviewPid (Issues.PullRequestReview run.id)
    rejectedRun.status `shouldBe` Issues.RunFailed
    runQueryEffect tr (Issues.listRunArtifacts reviewPid rejectedRun.id) >>= (`shouldSatisfy` (not . null))
    retainedBody <- sentBody rejectedRequests
    let observedComment app body = withHTTPResponses \_ endpoint -> pure if "/issues/7/comments?" `T.isInfixOf` toText endpoint then Just $ AE.encode @[AE.Value] [appComment 99 app body] else Nothing
        recoverPublication app body = within (transport True . observedComment app body . interpose @Agent.CodeReview (\_ _ -> liftIO $ fail "Publication recovery reran the review agent")) $ tryAny $ Impact.reviewPullRequest run.id
        publicationOutcomes = runQueryEffect tr (Investigations.listOperations @AI.OperationPolicy reviewPid rejectedRun.id) <&> \entries -> [entry.outcome | entry <- entries, Catalog.NativeCall Catalog.PublishPullRequestComment{} <- [entry.proposal.call]]
    map (\case Investigations.OperationUnknown -> True; _ -> False) <$> publicationOutcomes `shouldReturn` [True]
    for_ @[] [(456, retainedBody), (123, retainedBody <> "\nEdited")] \(app, body) -> do
      (uncertainRequests, uncertain) <- recoverPublication app body
      uncertain `shouldSatisfy` isLeft
      uncertainRequests `shouldNotSatisfy` reaches "/issues/comments/99"
    (reconciledRequests, reconciled) <- recoverPublication 123 retainedBody
    reconciled `shouldSatisfy` isRight
    reconciledRequests `shouldNotSatisfy` reaches "/issues/comments/99"
    reviewState `shouldReturn` Just Reviews.Completed
    map (\case Investigations.OperationReturned{} -> True; _ -> False) <$> publicationOutcomes `shouldReturn` [True]
    Just reconciledRun <- runQueryEffect tr $ Issues.getEventRun reviewPid $ Issues.PullRequestReview run.id
    dbExec tr "UPDATE projects.pr_review_runs SET state = 'incomplete' WHERE id = ?" (Only run.id)
    dbExec tr "UPDATE apis.ai_routine_runs SET status = 'failed' WHERE id = ?" (Only reconciledRun.id)
    dbExec tr "DELETE FROM apis.slack_turn_checkpoints WHERE project_id = ? AND run_id = (SELECT root_run_id FROM apis.ai_routine_runs WHERE id = ?)" (reviewPid, reconciledRun.id)
    (cachedRequests, cached) <- recoverPublication 123 retainedBody
    cached `shouldSatisfy` isRight
    cachedRequests `shouldNotSatisfy` reaches "/issues/comments/99"
    (lostRequests, lostResponse) <- lostPublication
    lostResponse `shouldSatisfy` isLeft
    lostBody <- sentBody lostRequests
    for_ [False, True] \revokeMapping -> do
      let changeAccess revoke =
            if revokeMapping
              then dbExec tr "UPDATE projects.code_mappings SET service = ? WHERE project_id = ? AND repo = 'checkout'" (if revoke then Nothing else Just ("test-service" :: Text), reviewPid)
              else dbExec tr "UPDATE projects.project_members SET permission = ? WHERE project_id = ?" (if revoke then ProjectMembers.PView else ProjectMembers.PAdmin, reviewPid)
          revokeDuringLookup = withHTTPResponses \_ endpoint -> Nothing <$ when ("/issues/7/comments?" `T.isInfixOf` toText endpoint) (changeAccess True)
          revisions = dbOne tr "SELECT count(*) FROM projects.memory_revisions WHERE project_id = ?" (Only reviewPid) :: IO Int64
      priorRevisions <- revisions
      (revokedWrites, revoked) <- attempt (observedComment 123 lostBody . revokeDuringLookup . fakeAgent)
      changeAccess False
      revoked `shouldSatisfy` isLeft
      revokedWrites `shouldNotSatisfy` reaches "/issues/comments/99"
      revisions >>= (\stored -> (revokeMapping, stored) `shouldBe` (revokeMapping, priorRevisions))
    (freshRequests, freshResult) <- attempt (observedComment 123 lostBody . fakeAgent)
    freshResult `shouldSatisfy` isRight
    length (filter (T.isInfixOf "/issues/comments/99" . fst) freshRequests) `shouldBe` 1

    -- Billing changes during a worker attempt must not move its later receipts.
    dbExec tr "UPDATE projects.projects SET customer_id = 'cus_before_attempt' WHERE id = ?" (Only reviewPid)
    void $ review $ fakeAgentWith ["--billing-change-before-usage"]
    dbQuery tr "SELECT (SELECT count(*) FROM projects.pr_review_usage WHERE project_id = ? AND customer_id = 'cus_before_attempt' AND billing_provider = ?), (SELECT count(*) FROM projects.pr_review_checks WHERE project_id = ? AND customer_id = 'cus_before_attempt' AND billing_provider = ?)" (reviewPid, Projects.StripeProvider, reviewPid, Projects.StripeProvider) `shouldReturn` [(2 :: Int64, 2 :: Int64)]
    delayed <- dbOne tr "SELECT receipt FROM projects.pr_review_usage WHERE project_id = ? AND customer_id = 'cus_before_attempt' AND cost_nanousd IS NOT NULL" (Only reviewPid) <&> \(Aeson value) -> value :: Agent.ModelUsage
    runQueryEffect tr (Reviews.recordUsage run delayed) `shouldReturn` True
    runQueryEffect tr (Reviews.recordUsage run delayed{Agent.id = UUIDId $ UUID.fromWords 70 0 0 1, Agent.provider = "another-provider"}) `shouldReturn` False
    runQueryEffect tr (Reviews.recordUsage run{Reviews.projectId = emptyPid} delayed) `shouldReturn` False
    runQueryEffect tr (Reviews.recordCheck run execution{Agent.id = UUIDId $ UUID.fromWords 70 0 0 2, Agent.attemptId = delayed.attemptId}) `shouldReturn` True
    dbExec tr "UPDATE projects.projects SET customer_id = NULL WHERE id = ?" (Only reviewPid)
    runQueryEffect tr (Reviews.recordUsage run delayed{Agent.id = UUIDId $ UUID.fromWords 70 0 0 3}) `shouldReturn` True
    dbOne tr "SELECT count(*) FROM projects.pr_review_usage WHERE project_id = ? AND customer_id = 'cus_before_attempt' AND attempt_id IS NOT NULL" (Only reviewPid) `shouldReturn` (3 :: Int64)
    for_ related \(pid, n) -> when (n == 5) do
      [otherRun] <- runQueryEffect tr $ Reviews.latestRuns pid
      runQueryEffect tr (Reviews.recordUsage otherRun delayed{Agent.id = UUIDId $ UUID.fromWords 70 0 0 4}) `shouldReturn` False
      runQueryEffect tr (Reviews.recordCheck otherRun execution{Agent.id = UUIDId $ UUID.fromWords 70 0 0 5, Agent.attemptId = delayed.attemptId}) `shouldReturn` False
    void $ review $ fakeAgentWith ["--billing-change-before-usage"]
    dbOne tr "SELECT count(*) FROM projects.pr_review_usage u JOIN projects.pr_review_attempts a ON a.id = u.attempt_id WHERE u.project_id = ? AND a.customer_id IS NULL AND u.customer_id IS NULL" (Only reviewPid) `shouldReturn` (2 :: Int64)

    -- Unlinking the repository while the agent runs revokes its tools and publication;
    -- switching to asks only does not cancel a review already running.
    revokedRequests <- review $ repositoryReview \_ respond knowledge unlift -> do
      let request = Agent.TelemetryQuery "test-service" "| summarize count()"
      unlift (respond request) >>= (`shouldSatisfy` isRight)
      runQueryEffect tr $ Reviews.updateSettings reviewPid (Reviews.ReviewSettings "impact-org" "checkout" Reviews.WhenAsked False)
      unlift (respond request) >>= (`shouldSatisfy` isRight)
      dbExec tr "UPDATE projects.code_mappings SET repo = 'unlinked' WHERE project_id = ?" (Only reviewPid)
      unlift (respond request) >>= (`shouldSatisfy` isLeft)
      unlift (knowledge $ Agent.Recall "test-service") >>= (`shouldSatisfy` isLeft)
      resultText result
    revokedRequests `shouldNotSatisfy` reaches "/issues/"
    dbExec tr "UPDATE projects.code_mappings SET repo = 'checkout' WHERE project_id = ?" (Only reviewPid)
    -- whenAsked_supersedesAQueuedAutomaticReview: switching to asks only stops an automatic run that has not started.
    runQueryEffect tr $ Reviews.updateSettings reviewPid (Reviews.ReviewSettings "impact-org" "checkout" Reviews.WhenAsked False)
    quiet <- review (interpose @Agent.CodeReview \_ _ -> liftIO $ fail "An automatic review ran after the repository switched to asks only")
    quiet `shouldNotSatisfy` reaches "/issues/comments/99"
    reviewState `shouldReturn` Just Reviews.Superseded
    runQueryEffect tr $ Reviews.updateSettings reviewPid (Reviews.ReviewSettings "impact-org" "checkout" Reviews.ReadyPullRequests False)
    -- Unmapped repositories publish a coverage gap without calling the model.
    dbExec tr "UPDATE projects.code_mappings SET service = NULL WHERE project_id = ?" (Only reviewPid)
    unmapped <- sentTo "/issues/comments/99" <$> review (interpose @Agent.CodeReview \_ _ -> liftIO $ fail "Review agent called without measured evidence")
    unmapped `shouldSatisfy` T.isInfixOf "Coverage unknown"
    unmapped `shouldNotSatisfy` T.isInfixOf "test-service]("
    void $ testServant tr $ CodeContextPage.impactReviewSettingsPostH reviewPid (Reviews.ReviewSettings "impact-org" "checkout" Reviews.WhenAsked False)
    void $ deliver tr $ payload "synchronize" (T.replicate 40 "b") "2025-01-01T00:01:00Z"
    -- Asks-only mode queues nothing, but the push still supersedes the older head.
    runQueryEffect tr (Reviews.latestRuns reviewPid) >>= \runs -> map (\r -> (r.revision, r.latestRevision)) runs `shouldBe` [(revision, T.replicate 40 "b")]
    runQueryEffect tr (Reviews.repositorySettings reviewPid) >>= \settings -> map (\s -> (s.trigger, s.includeEvidence)) settings `shouldBe` [(Reviews.WhenAsked, False)]
    dbExec tr "UPDATE projects.pr_review_threads SET latest_revision = ? WHERE project_id = ?" (revision, reviewPid)
    -- A metrics-only service still needs an investigation when there are no log/span counts.
    [credential] <- runQueryEffect tr $ GitSync.getGitHubCredentials reviewPid
    runQueryEffect tr $ CodeContext.insertCodeMapping reviewPid credential.id (Git.RepoRef "impact-org" "checkout" "main") (Just "test-service") "" ""
    dbExec tr "DELETE FROM otel_logs_and_spans WHERE project_id = ?" (Only reviewPid)
    runQueryEffect tr $ Reviews.updateSettings reviewPid (Reviews.ReviewSettings "impact-org" "checkout" Reviews.ReadyPullRequests True)
    metricsRequests <- review $ repositoryReview \_ respond _ unlift -> do
      let request = Agent.TelemetryQuery "test-service" "metrics | metric_name == \"review.temperature\" | summarize avg(value)"
      observed <- unlift $ respond request
      jsonText observed `shouldSatisfy` T.isInfixOf "42"
      dbExec tr "UPDATE otel_metrics SET value = 84 WHERE project_id = ? AND metric_name = 'review.temperature'" (Only reviewPid.toText)
      unlift (respond request) >>= (`shouldBe` AE.encode observed) . AE.encode
      unlift (respond request{Agent.query = request.query <> " | take 1"}) >>= (`shouldSatisfy` T.isInfixOf "84") . jsonText
      resultText result{Impact.findings = [Impact.Finding (Impact.ChangedLine "checkout.hs" 1 Impact.Head) "Check the changed metric behavior." "Verify the gauge." ["query-1"]]}
    T.intercalate " " (map (decodeUtf8 . snd) metricsRequests) `shouldSatisfy` T.isInfixOf "Review query results"
    T.intercalate " " (map (decodeUtf8 . snd) metricsRequests) `shouldNotSatisfy` T.isInfixOf "No telemetry observed"

    Just routine <- runQueryEffect tr $ Issues.getEventRoutine reviewPid Issues.EventPullRequest
    nativeWrites <- newIORef (0 :: Int)
    void $ review $ repositoryReview \request _ _ _ -> do
      Just nativeExecution <- runQueryEffect tr $ Issues.getEventRun reviewPid $ Issues.PullRequestReview run.id
      runContext <- runTestBg frozenTime tr $ AI.requireRunContext reviewPid $ Issues.EventClaim (Issues.PullRequestReview run.id) nativeExecution.id nativeExecution.startedAt
      title <- either (fail . toString) pure $ Memory.mkMemoryText "Recovered native write"
      body <- either (fail . toString) pure $ Memory.mkMemoryText "The same proposal must not write twice."
      let page = case request.attemptId of UUIDId value -> UUIDId value
          wikiInput = Catalog.WikiWrite request.attemptId $ Memory.Edit page 0 $ Memory.Document title body
          operation = Catalog.WriteWiki wikiInput
          target = Investigations.RunCheckpoint $ Investigations.RunTurn reviewPid runContext.conversationId runContext.requestedBy runContext.rootId
      for_ @[] [Catalog.WriteWiki wikiInput{Catalog.attemptId = UUIDId UUID.nil}, Catalog.PublishPullRequestComment $ Catalog.ReviewComment (UUIDId UUID.nil) "Wrong review"] \invalidRequest -> do
        rejectedActor <- tryAny $ runTestBg frozenTime tr $ AI.executeNativeOperation runContext invalidRequest \_ -> AI.EffectfulOperation do
          liftIO $ modifyIORef' nativeWrites (+ 1)
          pure $ Right AE.Null
        rejectedActor `shouldSatisfy` isLeft
      readIORef nativeWrites `shouldReturn` 0
      runQueryEffect tr (Investigations.loadCheckpointFor target) >>= (`shouldSatisfy` isNothing)
      committed <- MVar.newEmptyMVar
      release <- MVar.newEmptyMVar
      let perform = \case
            Catalog.WriteWiki write -> AI.EffectfulOperation do
              invocation <- liftIO $ atomicModifyIORef' nativeWrites (\count -> (count + 1, count + 1))
              stored <- Memory.writePage reviewPid write.edit.pageId write.edit.version Nothing write.edit.document $ Memory.Agent write.attemptId
              when (invocation == 1) $ liftIO $ MVar.putMVar committed () >> MVar.takeMVar release
              pure $ first show $ AE.toJSON <$> stored
            Catalog.PublishPullRequestComment{} -> AI.EffectfulOperation $ throwIO Investigations.OperationProposalChanged
            Catalog.EnrichIssue{} -> AI.EffectfulOperation $ throwIO Investigations.OperationProposalChanged
          executeWrite = runTestBg frozenTime tr $ AI.executeNativeOperation runContext operation perform
      late <- withAsync executeWrite \worker -> do
        Timeout.timeout (10 * 1000000) (MVar.takeMVar committed) `shouldReturn` Just ()
        tryAny executeWrite >>= (`shouldSatisfy` isLeft)
        entries <- runQueryEffect tr $ Investigations.listOperations @AI.OperationPolicy reviewPid nativeExecution.id
        map (\entry -> case entry.outcome of Investigations.OperationUnknown -> True; _ -> False) entries `shouldBe` [True]
        MVar.putMVar release ()
        either throwIO pure =<< waitCatch worker
      late `shouldSatisfy` isRight
      dbExec tr "DELETE FROM apis.slack_turn_checkpoints WHERE project_id = ? AND run_id = ?" (reviewPid, runContext.rootId)
      replicateM_ 2 $ executeWrite `shouldReturn` late
      readIORef nativeWrites `shouldReturn` 1
      runQueryEffect tr (Memory.pageHistory reviewPid page) >>= (\revisions -> map (.version) revisions `shouldBe` [1])
      resultText result{Impact.findings = [], Impact.coverage = ["Recovered native action"]}
    for_ [False, True] $ \withdraw -> do
      runQueryEffect tr $ Issues.setRoutinePolicy routine.requestedBy reviewPid routine.conversationId (if withdraw then Issues.Autonomous else Issues.Observe)
      beforePages <- runQueryEffect tr $ Memory.latestPages reviewPid
      knowledgeResults <- newIORef Nothing
      wikiRequests <- review $ repositoryReview \request _ knowledge unlift -> do
        when withdraw $ runQueryEffect tr $ Issues.setRoutinePolicy routine.requestedBy reviewPid routine.conversationId Issues.Observe
        title <- either (fail . toString) pure $ Memory.mkMemoryText "Must not be written"
        body <- either (fail . toString) pure $ Memory.mkMemoryText "Observe policy does not authorize a wiki edit."
        let page = case request.attemptId of UUIDId value -> UUIDId value
            memory = unlift . knowledge . Agent.ProjectMemory
        searchTerm <- either (fail . toString) pure $ Memory.mkMemoryText "no-wiki-matches-this-observation"
        outcomes <- (,,) <$> memory Memory.List <*> memory (Memory.Search searchTerm) <*> memory (Memory.Write $ Memory.Edit page 0 $ Memory.Document title body)
        writeIORef knowledgeResults $ Just outcomes
        resultText result{Impact.findings = [], Impact.coverage = ["Observed without project changes"]}
      readIORef knowledgeResults >>= \case
        Just (listed, searched, wikiSaved) -> do
          listed `shouldSatisfy` isRight
          searched `shouldBe` Right (AE.toJSON ([] :: [AE.Value]))
          wikiSaved `shouldSatisfy` isLeft
        Nothing -> fail "Expected the review worker to try wiki reads and writes"
      wikiRequests `shouldNotSatisfy` reaches "/issues/"
      runQueryEffect tr (Memory.latestPages reviewPid) >>= (\pages -> length pages `shouldBe` length beforePages)
    runQueryEffect tr $ Issues.setRoutinePolicy routine.requestedBy reviewPid routine.conversationId Issues.Observe
    dbExec tr "DELETE FROM projects.code_mappings WHERE project_id = ? AND service IS NOT NULL" (Only reviewPid)
    review (interpose @Agent.CodeReview \_ _ -> liftIO $ fail "Observe coverage-gap review must not call the model") >>= (`shouldNotSatisfy` reaches "/issues/")
    Just observedRun <- runQueryEffect tr $ Issues.getEventRun reviewPid $ Issues.PullRequestReview run.id
    observedRun.status `shouldBe` Issues.RunSucceeded
    runQueryEffect tr (Issues.listRunArtifacts reviewPid observedRun.id) >>= (\artifacts -> T.intercalate " " (mapMaybe (.content) artifacts) `shouldSatisfy` T.isInfixOf "Observe policy: the GitHub comment was not updated.")

    for_ (["policy", "revision", "evidence"] :: [Text]) $ \restriction -> do
      runQueryEffect tr $ Issues.setRoutinePolicy routine.requestedBy reviewPid routine.conversationId Issues.Autonomous
      revisionChanged <- newIORef False
      let restrictBeforePublication = withHTTPResponses \_ endpoint -> do
            when ("/issues/7/comments?" `T.isInfixOf` toText endpoint) $ case restriction of
              "policy" -> runQueryEffect tr $ Issues.setRoutinePolicy routine.requestedBy reviewPid routine.conversationId Issues.Observe
              "revision" -> writeIORef revisionChanged True
              _ -> runQueryEffect tr $ Investigations.retainEvidencePermission reviewPid [observedRun.conversationId] ProjectMembers.PAdmin
            changed <- readIORef revisionChanged
            pure $ if changed && "/pulls/7" `T.isSuffixOf` toText endpoint then Just $ AE.encode $ prAt $ T.replicate 40 "e" else Nothing
      (restrictedRequests, outcome) <- attempt restrictBeforePublication
      outcome `shouldSatisfy` isLeft
      restrictedRequests `shouldSatisfy` reaches "/issues/7/comments?"
      restrictedRequests `shouldNotSatisfy` reaches "/issues/comments/99"

  it "persists issue-fix phases only after frozen reproduction and counterfactual verification" \tr -> do
    (mapping, issue, base) <- fixFixture tr "fix-workflow"
    let uid n = UUIDId $ UUID.fromWords 91 0 0 n
        image = uid 2
    dbExec tr "UPDATE projects.projects SET customer_id = 'cus_fix_before', billing_provider = ? WHERE id = ?" (Projects.StripeProvider, testPid)
    candidateRevision <- fixRevision $ T.replicate 40 "b"
    newerRevision <- fixRevision $ T.replicate 40 "c"
    workspace <- either fail pure $ AE.eitherDecode $ AE.encode $ T.replicate 64 "a"
    fid <- maybe (fail "fix run missing") pure =<< runQueryEffect tr (Fixes.createRun testPid (uid 0) issue mapping.id base image)
    otherPid <- createTestProject tr "Other fix project"
    runQueryEffect tr (Fixes.createRun otherPid (uid 0) issue mapping.id base image) `shouldReturn` Nothing
    (fmap (.id) <<$>> runQueryEffect tr (Fixes.getRun otherPid fid)) `shouldReturn` Right Nothing
    let load = runQueryEffect tr (Fixes.getRun testPid fid) >>= either (fail . show) (maybe (fail "fix run missing") pure)
        candidate = Fixes.Candidate candidateRevision "The source drops the retry token."
        check = Agent.CheckReceipt (uid 3) (uid 4) (uid 5) Agent.Base (Fixes.revisionText base) image.toText (T.replicate 64 "d") (Just $ Agent.InjectedTest $ T.replicate 64 "a") Agent.Failed (Just 1) 10 2 1024 20 (T.replicate 64 "e") (Just workspace)
        record run n step = runQueryEffect tr $ Fixes.recordStep testPid fid run.version (uid n) step
        accept run n step = record run n step >>= either (fail . show) pure
        reject run n step err = record run n step >>= (`shouldBe` Left err) . fmap (.version)
    initial <- load
    Fixes.phase initial.progress `shouldBe` Fixes.Reproducing
    initial.customerId `shouldBe` Just "cus_fix_before"
    dbExec tr "UPDATE projects.projects SET customer_id = 'cus_fix_after' WHERE id = ?" (Only testPid)
    runQueryEffect tr (Fixes.createRun testPid fid issue mapping.id base image) `shouldReturn` Just fid
    runQueryEffect tr (Fixes.createRun testPid fid issue mapping.id candidateRevision image) `shouldReturn` Nothing
    reject initial 10 (Fixes.ProposeCandidate candidate) Fixes.ReproductionRequired
    let blockedCheck = check{Agent.id = uid 6, Agent.regression = Nothing, Agent.status = Agent.Blocked, Agent.exitCode = Just 127}
    blocked <- accept initial 11 $ Fixes.Observe blockedCheck
    Fixes.phase blocked.progress `shouldBe` Fixes.Reproducing
    dbExec tr "UPDATE projects.issue_fix_steps SET step = jsonb_set(step, '{contents}', (step->'contents') - 'regression') WHERE id = ?" (Only (uid 11 :: Fixes.StepId))
    legacyReplay <- accept initial 11 $ Fixes.Observe blockedCheck
    legacyReplay.version `shouldBe` blocked.version
    notReproduced <- accept blocked 12 $ Fixes.Observe check{Agent.id = uid 7, Agent.status = Agent.Passed, Agent.exitCode = Just 0}
    Fixes.phase notReproduced.progress `shouldBe` Fixes.Reproducing
    reject notReproduced 13 (Fixes.Observe check{Agent.exitCode = Just 126}) Fixes.InvalidCheck
    reject notReproduced 13 (Fixes.Observe check{Agent.regression = Nothing}) Fixes.TestArtifactRequired
    reject notReproduced 13 (Fixes.Observe check{Agent.regression = Just $ Agent.ScriptCheck $ T.replicate 64 "a"}) Fixes.TestArtifactRequired
    reject notReproduced 13 (Fixes.Observe check{Agent.regression = Just $ Agent.InjectedTest "bad"}) Fixes.InvalidCheck
    reproduced <- accept notReproduced 14 $ Fixes.Observe check
    Fixes.phase reproduced.progress `shouldBe` Fixes.Fixing
    replayed <- accept notReproduced 14 $ Fixes.Observe check
    replayed.version `shouldBe` reproduced.version
    reject reproduced 14 (Fixes.Observe check{Agent.logBytes = 99}) Fixes.StepConflict
    reject notReproduced 15 Fixes.RestartReproduction Fixes.StaleVersion
    verifying <- accept reproduced 16 $ Fixes.ProposeCandidate candidate
    let baseCheck = check{Agent.id = uid 30, Agent.checkId = uid 31}
        headCheck = baseCheck{Agent.id = uid 32, Agent.side = Agent.Head, Agent.revision = Fixes.revisionText candidateRevision, Agent.regression = Just $ Agent.RetainedTest $ T.replicate 64 "a", Agent.status = Agent.Passed, Agent.exitCode = Just 0}
    reject verifying 17 (Fixes.Observe headCheck) Fixes.UnexpectedCheck
    oldReceipt <- accept verifying 18 $ Fixes.Observe check
    oldReceipt.version `shouldBe` verifying.version
    reject verifying 18 (Fixes.Observe check{Agent.id = uid 33}) Fixes.WrongComparison
    for_ [((baseCheck :: Agent.CheckReceipt){Agent.imageBuildId = (uid 99 :: UUIDId "guest_image").toText}, Fixes.WrongImage), (baseCheck{Agent.scriptSha256 = T.replicate 64 "f"}, Fixes.WrongTest), (baseCheck{Agent.regression = Just $ Agent.InjectedTest $ T.replicate 64 "b"}, Fixes.WrongTest), (baseCheck{Agent.regression = Nothing}, Fixes.WrongTest), (baseCheck{Agent.revision = Fixes.revisionText candidateRevision}, Fixes.WrongRevision), (baseCheck{Agent.cpus = 1}, Fixes.WrongResources), (baseCheck{Agent.workspace = Nothing}, Fixes.WrongWorkspace)] \(observed, err) ->
      reject verifying 19 (Fixes.Observe observed) err
    cachedCheck <- either fail pure $ AE.eitherDecode $ AE.encode $ case AE.toJSON baseCheck of
      AE.Object fields -> AE.Object $ KM.insert "workspace" (AE.String $ T.replicate 64 "f") fields
      value -> value
    let invalidWorkspace = case AE.toJSON baseCheck of AE.Object fields -> AE.Object $ KM.insert "workspace" (AE.String "invalid") fields; value -> value
    (AE.eitherDecode (AE.encode invalidWorkspace) :: Either String Agent.CheckReceipt) `shouldSatisfy` isLeft
    changedEnvironment <- record verifying 19 $ Fixes.Observe cachedCheck
    fmap (.version) changedEnvironment `shouldBe` Left Fixes.WrongWorkspace
    rechecked <- accept verifying 20 $ Fixes.Observe baseCheck
    reject rechecked 21 (Fixes.Observe headCheck{Agent.checkId = uid 98}) Fixes.WrongComparison
    stillBroken <- accept rechecked 22 $ Fixes.Observe headCheck{Agent.status = Agent.Failed, Agent.exitCode = Just 1}
    Fixes.phase stillBroken.progress `shouldBe` Fixes.Fixing
    verifyingAgain <- accept stillBroken 23 $ Fixes.ProposeCandidate candidate
    recheckedAgain <- accept verifyingAgain 24 $ Fixes.Observe baseCheck{Agent.id = uid 40, Agent.checkId = uid 41}
    reject recheckedAgain 25 (Fixes.Observe headCheck{Agent.id = uid 42, Agent.checkId = uid 41, Agent.regression = check.regression}) Fixes.TestArtifactRequired
    verified <- accept recheckedAgain 25 $ Fixes.Observe headCheck{Agent.id = uid 42, Agent.checkId = uid 41}
    Fixes.phase verified.progress `shouldBe` Fixes.Verified
    Fixes.verifiedRevision verified.progress `shouldBe` Just candidateRevision
    resumed <- load
    resumed.version `shouldBe` verified.version
    Fixes.verifiedRevision resumed.progress `shouldBe` Just candidateRevision
    resumed.customerId `shouldBe` Just "cus_fix_before"
    invalidated <- accept resumed 26 $ Fixes.ProposeCandidate $ Fixes.Candidate newerRevision "A new candidate needs fresh verification."
    Fixes.phase invalidated.progress `shouldBe` Fixes.Verifying
    Fixes.verifiedRevision invalidated.progress `shouldBe` Nothing
    baseNowPasses <- accept invalidated 27 $ Fixes.Observe baseCheck{Agent.id = uid 50, Agent.checkId = uid 51, Agent.status = Agent.Passed, Agent.exitCode = Just 0}
    Fixes.phase baseNowPasses.progress `shouldBe` Fixes.Reproducing
    let restart n = record baseNowPasses n Fixes.RestartReproduction
    (leftOutcome, rightOutcome) <- concurrently (restart 28) (restart 29)
    length (rights [leftOutcome, rightOutcome]) `shouldBe` 1
    length (lefts [leftOutcome, rightOutcome]) `shouldBe` 1
    concurrentRun <- load
    concurrentRun.version `shouldBe` baseNowPasses.version + 1
    changedTest <- accept concurrentRun 30 $ Fixes.Observe check{Agent.id = uid 60, Agent.checkId = uid 61, Agent.scriptSha256 = T.replicate 64 "f"}
    Fixes.phase changedTest.progress `shouldBe` Fixes.Fixing
    dbExec tr "UPDATE projects.git_credentials SET installation_id = installation_id + 1 WHERE id = ?" (Only mapping.credentialId)
    reject changedTest 31 (Fixes.ProposeCandidate candidate) Fixes.StepConflict
    dbExec tr "UPDATE projects.git_credentials SET installation_id = installation_id - 1 WHERE id = ?" (Only mapping.credentialId)
    dbExec tr "UPDATE projects.code_mappings SET service = 'revoked-service' WHERE id = ?" (Only mapping.id)
    reject changedTest 31 (Fixes.ProposeCandidate candidate) Fixes.StepConflict
    unchanged <- load
    unchanged.version `shouldBe` changedTest.version

  it "accounts for review and issue-fix work through the persisted protocol gates" \tr -> do
    (mapping, issue, base) <- fixFixture tr "fix-protocol"
    let uid n = UUIDId $ UUID.fromWords 93 0 0 n
        image = uid 2
    let load fid = runQueryEffect tr (Fixes.getRun testPid fid) >>= either (fail . show) (maybe (fail "fix missing") pure)
    otherPid <- createTestProject tr "Other fix ledger project"
    void $ deliver tr $ payload "opened" revision "2025-01-01T00:00:00Z"
    [review] <- runQueryEffect tr $ Reviews.latestRuns testPid
    for_ (zip [10 :: Word32 ..] ["", "--tampered-result", "--post-phase", "--deny-phase"]) \(n, mode) -> do
      let fid = uid n
      runQueryEffect tr (Fixes.createRun testPid fid issue mapping.id base image) `shouldReturn` Just fid
      let customer = if n == 10 then Nothing else Just "cus_fix_usage" :: Maybe Text
      dbExec tr "UPDATE projects.projects SET customer_id = ?, billing_provider = ? WHERE id = ?" (customer, Projects.StripeProvider, testPid)
      runQueryEffect tr (Usage.startAttempt otherPid (Usage.Fix fid) "fixture" "gpt-5.6-luna#high") `shouldReturn` Nothing
      attempt <- maybe (fail "Fix attempt missing from shared ledger") pure =<< runQueryEffect tr (Usage.startAttempt testPid (Usage.Fix fid) "fixture" "gpt-5.6-luna#high")
      dbExec tr "UPDATE projects.projects SET customer_id = 'cus_after_fix', billing_provider = ? WHERE id = ?" (Projects.LemonSqueezyProvider, testPid)
      let request = Agent.Request{projectId = testPid, runId = fid, attemptId = attempt, repository = "https://github.com/impact-org/checkout.git", base = Fixes.revisionText base, head = Fixes.revisionText base, gitToken = "fixture-token", prompt = "Fix the issue", provider = "fixture", model = "gpt-5.6-luna", thinkingLevel = Agent.High}
      usage <- newIORef []
      checks <- newIORef []
      let phase delivery = liftIO do
            current <- load fid
            if delivery.attemptId /= attempt || (mode == "--deny-phase" && case delivery.step of Fixes.Observe{} -> True; _ -> False)
              then pure $ Left "Phase rejected by host"
              else first show . void <$> runQueryEffect tr (Fixes.recordStep testPid fid current.version delivery.id delivery.step)
          validate result = do
            valid <- liftIO do
              current <- load fid
              seen <- readIORef checks
              pure $ result.projectId == testPid && result.runId == fid && result.issueId == issue && result.attemptId == attempt && result.base == base && Fixes.validateResult current result && any (\receipt -> receipt.checkId == result.checkId && receipt.side == Agent.Head && receipt.status == Agent.Passed) seen
            if not valid
              then pure $ Left "Fix result does not match this verified run"
              else do
                current <- liftIO $ load fid
                artifacts <- maybe (liftIO $ fail "Verified artifacts missing") pure $ Fixes.artifactRequest request.repository current result
                proof <- artifactProofFixture (Right ()) artifacts
                case proof of
                  Left message -> pure $ Left message
                  Right verified -> do
                    let candidateReview reviewMode =
                          scripted Nothing "test/fixtures/fix-review-agent.mjs" [reviewMode]
                            $ Agent.reviewVerifiedFix Agent.Isolated (Agent.FixReviewRequest (Agent.FixRequest request{Agent.attemptId = uid 998} issue image) artifacts) verified (const $ pure $ Left "Unexpected memory request") (const pass) (const pass) (const pass)
                    when (null mode) $ for_ ["--skip", "--tampered", "--post-query", "--check"] \reviewMode -> do
                      rejected <- candidateReview reviewMode
                      liftIO $ isLeft rejected `shouldBe` True
                    outcome <- candidateReview ""
                    pure
                      $ outcome
                      >>= \case
                        Agent.Reviewed reviewed -> Right reviewed
                        _ -> Left "Independent review did not approve the candidate"
      outcome <- withSystemTempDirectory "fix-protocol" \directory -> do
        let diagnostic = directory </> "failure.txt"
        result <-
          runEff
            $ scripted Nothing "test/fixtures/issue-fix-agent.mjs" [diagnostic, toString image.toText, mode]
            $ Agent.fixRepository
              Agent.Isolated
              (Agent.FixRequest request issue image)
              (const $ pure $ Left "No telemetry fixture")
              (const $ pure $ Left "No knowledge fixture")
              (\artifact -> pure $ if artifact.testPatch == "admitted-test-patch" then Right "Fixture exercises the reported retry failure" else Left "Test rejected")
              (\receipt -> liftIO $ runQueryEffect tr (Usage.recordCall testPid (Usage.Fix fid) receipt) `shouldReturn` True)
              ( \receipt -> liftIO do
                  runQueryEffect tr (Usage.recordUsage testPid (Usage.Fix fid) receipt) `shouldReturn` True
                  runQueryEffect tr (Usage.recordUsage testPid (Usage.Fix fid) receipt) `shouldReturn` True
                  runQueryEffect tr (Usage.recordUsage testPid (Usage.Fix fid) receipt{Agent.costNanousd = Just 999}) `shouldReturn` False
                  runQueryEffect tr (Usage.recordUsage testPid (Usage.Fix fid) receipt{Agent.attemptId = uid 999}) `shouldReturn` False
                  runQueryEffect tr (Usage.recordUsage otherPid (Usage.Fix fid) receipt) `shouldReturn` False
                  runQueryEffect tr (Usage.recordUsage testPid (Usage.Review review.id) receipt) `shouldReturn` False
                  runQueryEffect tr (Usage.recordUsage testPid (Usage.Fix fid) receipt{Agent.provider = "other-provider"}) `shouldReturn` False
                  modifyIORef' usage (receipt :)
              )
              ( \receipt -> liftIO do
                  runQueryEffect tr (Usage.recordAllocation testPid (Usage.Fix fid) receipt) `shouldReturn` True
                  runQueryEffect tr (Usage.recordAllocation testPid (Usage.Fix fid) receipt) `shouldReturn` True
                  runQueryEffect tr (Usage.recordAllocation otherPid (Usage.Fix fid) receipt) `shouldReturn` False
                  runQueryEffect tr (Usage.recordAllocation testPid (Usage.Review review.id) receipt) `shouldReturn` False
                  let withField key value = withKey key value $ AE.toJSON receipt
                  for_ ([("durationMs", AE.toJSON (999 :: Int)), ("attemptId", AE.toJSON (uid 999 :: UUIDId "agent_attempt"))] :: [(AE.Key, AE.Value)]) \(key, value) -> do
                    changed <- either fail pure $ AE.eitherDecode $ AE.encode $ withField key value
                    runQueryEffect tr (Usage.recordAllocation testPid (Usage.Fix fid) changed) `shouldReturn` False
                  for_ (["cpus", "memoryMiB"] :: [AE.Key]) \resource ->
                    (AE.eitherDecode (AE.encode $ withField resource $ AE.toJSON (0 :: Int)) :: Either String Agent.AllocationReceipt) `shouldSatisfy` isLeft
              )
              ( \receipt -> liftIO do
                  runQueryEffect tr (Usage.recordCheck testPid (Usage.Fix fid) receipt) `shouldReturn` True
                  runQueryEffect tr (Usage.recordCheck testPid (Usage.Fix fid) receipt) `shouldReturn` True
                  runQueryEffect tr (Usage.recordCheck testPid (Usage.Fix fid) receipt{Agent.durationMs = 999}) `shouldReturn` False
                  runQueryEffect tr (Usage.recordCheck testPid (Usage.Fix fid) receipt{Agent.attemptId = uid 999}) `shouldReturn` False
                  runQueryEffect tr (Usage.recordCheck otherPid (Usage.Fix fid) receipt) `shouldReturn` False
                  runQueryEffect tr (Usage.recordCheck testPid (Usage.Review review.id) receipt) `shouldReturn` False
                  modifyIORef' checks (receipt :)
              )
              phase
              validate
        detail <- tryAny $ readFileBS diagnostic
        pure $ first (<> either (const "") (("\n" <>) . decodeUtf8) detail) result
      if null mode then fmap (\reviewed -> (Agent.reviewedFixResult reviewed).runId) outcome `shouldBe` Right fid else isLeft outcome `shouldBe` True
      stored <- load fid
      Fixes.phase stored.progress `shouldBe` if mode == "--deny-phase" then Fixes.Reproducing else Fixes.Verified
      stored.version `shouldBe` if mode == "--deny-phase" then 1 else 5
      receipts <- readIORef usage
      length receipts `shouldBe` 1
      dbQuery tr "SELECT customer_id, billing_provider FROM projects.pr_review_usage WHERE fix_run_id = ? AND run_id IS NULL AND attempt_id = ?" (fid, attempt) `shouldReturn` [(customer, Projects.StripeProvider)]
      dbOne tr "SELECT count(*) FROM projects.pr_review_checks WHERE fix_run_id = ? AND run_id IS NULL AND attempt_id = ? AND customer_id IS NOT DISTINCT FROM ? AND billing_provider = ?" (fid, attempt, customer, Projects.StripeProvider) `shouldReturn` (if mode == "--deny-phase" then 1 else 3 :: Int64)
    let fid = uid 10
    lateAttempt <- maybe (fail "Late attempt missing") pure =<< runQueryEffect tr (Usage.startAttempt testPid (Usage.Fix fid) "fixture" "gpt-5.6-luna#high")
    missing <- runQueryEffect tr $ Usage.usageTotals Usage.Fixes testPid
    fmap (.usage.unreportedAttempts) (find (isNothing . (.runId)) missing) `shouldBe` Just 1
    Aeson receipt <- dbOne tr "SELECT receipt FROM projects.pr_review_usage WHERE fix_run_id = ?" (Only fid) :: IO (Aeson Agent.ModelUsage)
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Fix fid) receipt{Agent.id = uid 900, Agent.attemptId = lateAttempt}) `shouldReturn` False
    let lateCall = Agent.ModelCall{id = uid 902, attemptId = lateAttempt, provider = receipt.provider, model = receipt.model}
    runQueryEffect tr (Usage.recordCall testPid (Usage.Fix fid) lateCall) `shouldReturn` True
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Fix fid) receipt{Agent.id = uid 900, Agent.attemptId = lateAttempt, Agent.callId = Just lateCall.id, Agent.costNanousd = Nothing, Agent.succeeded = False}) `shouldReturn` True
    runQueryEffect tr (Reviews.claimRun (slots 100) review) `shouldReturn` True
    reviewAttempt <- maybe (fail "Review attempt missing") pure =<< runQueryEffect tr (Reviews.startAttempt review "fixture" "gpt-5.6-luna#high")
    let reviewCall = Agent.ModelCall{id = uid 903, attemptId = reviewAttempt, provider = receipt.provider, model = receipt.model}
    runQueryEffect tr (Usage.recordCall testPid (Usage.Review review.id) reviewCall) `shouldReturn` True
    runQueryEffect tr (Reviews.recordUsage review receipt{Agent.id = uid 901, Agent.attemptId = reviewAttempt, Agent.callId = Just reviewCall.id}) `shouldReturn` True
    reviewTotals <- runQueryEffect tr $ Reviews.usageTotals testPid
    fixTotals <- runQueryEffect tr $ Usage.usageTotals Usage.Fixes testPid
    let amounts total = (total.usage.calls, total.usage.failedCalls, total.usage.unpricedCalls, total.usage.unreportedAttempts, total.usage.costNanousd)
    map amounts (filter (isNothing . (.runId)) reviewTotals) `shouldBe` [(1, 0, 0, 0, 42000)]
    map amounts (filter (isNothing . (.runId)) fixTotals) `shouldBe` [(5, 1, 1, 0, 168000)]
    map (.runId) (filter (isJust . (.runId)) reviewTotals) `shouldBe` [Just review.id]
    length (filter (isJust . (.runId)) fixTotals) `shouldBe` 4
    fmap (.usage.calls) (find ((== Just fid) . (.runId)) fixTotals) `shouldBe` Just 2
    fmap (\total -> (total.resources.checks, total.resources.vcpuMs, total.resources.memoryMiBMs, total.resources.logBytes)) (find (isNothing . (.runId)) fixTotals) `shouldBe` Just (10, 320, 163840, 200)
    fmap (.resources.checks) (find (isNothing . (.runId)) reviewTotals) `shouldBe` Just 0
    customers <- runQueryEffect tr $ Usage.customerUsage (getResponse tr.trSessAndHeader).user.id
    let original = filter (\row -> row.billingProvider == Projects.StripeProvider && row.customerId == Just "cus_fix_usage") customers
    sum (map (.resources.checks) original) `shouldBe` 7
    sum (map (.resources.vcpuMs) original) `shouldBe` 220
    sum (map (.resources.allocations) original) `shouldBe` 2
    sum (map (.resources.cleanupFailures) original) `shouldBe` 1
    sum (map (.resources.unreportedAllocations) original) `shouldBe` 1
    runQueryEffect tr (Reviews.latestChecks testPid) >>= (`shouldSatisfy` null)
    (_, usagePage) <- testServant tr $ CodeContextPage.repositoriesGetH testPid (Just CodeContextPage.Usage) Nothing
    renderT usagePage `shouldContainAll` ["review and issue-fix", "cus_fix_usage", "No billing customer linked", "$0.000126 estimated", "1 unpriced", "Sandbox allocation", "agent VM allocations", "agent VM cleanup unconfirmed", "attempts without an agent allocation receipt", "resource cost unavailable"]
    dbOne tr "SELECT count(*) FROM projects.ai_usage WHERE project_id = ?" (Only testPid) `shouldReturn` (0 :: Int64)

  it "binds separately metered admission decisions to their exact artifact without fix capabilities" \tr -> do
    (mapping, issue, base) <- fixFixture tr "admission-protocol"
    let uid n = UUIDId $ UUID.fromWords 94 0 0 n
        fid = uid 2
        artifact = Agent.TestArtifact "bash regression.sh" "native test patch" "retry token lost"
    runQueryEffect tr (Fixes.createRun testPid fid issue mapping.id base (uid 3)) `shouldReturn` Just fid
    for_ ["", "--reject", "--tampered", "--skip", "--check", "--post-query"] \mode -> do
      attempt <- maybe (fail "Admission attempt missing") pure =<< runQueryEffect tr (Usage.startAttempt testPid (Usage.Fix fid) "fixture" "gpt-5.6-luna#high")
      let request = Agent.Request{projectId = testPid, runId = fid, attemptId = attempt, repository = "https://github.com/impact-org/checkout.git", base = revision, head = revision, gitToken = "fixture-token", prompt = "Issue: retain retry tokens", provider = "fixture", model = "gpt-5.6-luna", thinkingLevel = Agent.High}
      outcome <-
        runEff
          $ scripted Nothing "test/fixtures/regression-admission-agent.mjs" [mode]
          $ Agent.admitRegression
            Agent.Isolated
            (Agent.AdmissionRequest (Agent.FixRequest request issue (uid 3)) artifact)
            (const $ pure $ Left "Unexpected memory request")
            (\receipt -> liftIO $ runQueryEffect tr (Usage.recordCall testPid (Usage.Fix fid) receipt) `shouldReturn` True)
            (\receipt -> liftIO $ runQueryEffect tr (Usage.recordUsage testPid (Usage.Fix fid) receipt) `shouldReturn` True)
            (\receipt -> liftIO $ runQueryEffect tr (Usage.recordAllocation testPid (Usage.Fix fid) receipt) `shouldReturn` True)
      case (mode, outcome) of
        ("", Right Agent.Admitted{}) -> pass
        ("--reject", Right Agent.Rejected{}) -> pass
        _ -> outcome `shouldSatisfy` isLeft
    totals <- runQueryEffect tr $ Usage.usageTotals Usage.Fixes testPid
    fmap (\total -> (total.usage.calls, total.usage.costNanousd, total.usage.unreportedAttempts)) (find ((== Just fid) . (.runId)) totals) `shouldBe` Just (6, 252000, 0)
    fmap (\total -> (total.resources.allocations, total.resources.unreportedAllocations, total.resources.vcpuMs)) (find ((== Just fid) . (.runId)) totals) `shouldBe` Just (5, 1, 1000)
    loaded <- runQueryEffect tr $ Fixes.getRun testPid fid
    ((.version) <<$>> loaded) `shouldBe` Right (Just 0)
    dbOne tr "SELECT count(*) FROM projects.pr_review_checks WHERE fix_run_id = ?" (Only fid) `shouldReturn` (0 :: Int64)

  it "nativeFix_migrationRetainsOriginalAdminGrantsWithoutRestoringRevokedAccess" \tr -> do
    (mapping, issue, base) <- fixFixture tr "legacy-fix-grants"
    let uid n = UUIDId $ UUID.fromWords 96 0 0 n
        user = (getResponse tr.trSessAndHeader :: Projects.Session).user.id
        jobs = [(uid 2, Nothing), (uid 3, Just "Interrupted before upgrade")] :: [(Fixes.FixId, Maybe Text)]
    for_ jobs $ \(identifier, _) -> runQueryEffect tr (Fixes.createRun testPid identifier issue mapping.id base (uid 4)) `shouldReturn` Just identifier
    -- This database belongs to this example. Reconstruct its pre-upgrade job
    -- shape, then run the actual consolidated linkage migration over populated records.
    withResource tr.trPool $ \conn -> do
      void $ execute conn "DROP TRIGGER fix_agent_link ON projects.issue_fix_jobs; DROP FUNCTION projects.guard_fix_agent_link(); DROP TRIGGER retain_fix_run_grant ON apis.ai_routine_runs; DROP FUNCTION apis.retain_fix_run_grant(); DROP INDEX apis.ai_routine_runs_native_fix; ALTER TABLE apis.ai_routine_runs DROP CONSTRAINT native_fix_run_grant; ALTER TABLE projects.issue_fix_jobs DROP COLUMN agent_run_id" ()
      for_ jobs $ \(identifier, failure) -> do
        inserted <- execute conn "INSERT INTO projects.issue_fix_jobs (run_id, requested_by, created_at, failure, finished_at) SELECT run_id, user_id, ?, ?, ? FROM projects.issue_fix_run_access WHERE run_id = ? AND user_id = ?" (addUTCTime (-60) frozenTime, failure, frozenTime <$ failure, identifier, user)
        inserted `shouldBe` 1
      void $ execute conn "UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?" (ProjectMembers.PEdit, testPid, user)
      migration <- readFileBS "static/migrations/0263_native_agent_runs.sql"
      void $ execute conn (Query migration) ()
    runs <- runQueryEffect tr $ Issues.listRoutineRuns testPid 12
    length runs `shouldBe` 2
    for_ jobs $ \(identifier, failure) -> do
      linked <- maybe (fail "Migrated shared run missing") pure $ find ((== Issues.FixRun identifier) . (.source)) runs
      linked.status `shouldBe` maybe Issues.RunRunning (const Issues.RunFailed) failure
      Just evidence <- runQueryEffect tr $ Issues.getRoutineRunEvidence testPid linked.id
      (evidence.requestedBy, evidence.permissionCeiling) `shouldBe` (Just user, ProjectMembers.PAdmin)
      runQueryEffect tr (isJust <$> Fixes.claimJob testPid identifier) `shouldReturn` False
      dbExec tr "UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?" (ProjectMembers.PAdmin, testPid, user)
      runQueryEffect tr (isJust <$> Fixes.claimJob testPid identifier) `shouldReturn` False
      Just stopped <- runQueryEffect tr $ Issues.getRoutineRun testPid linked.id
      stopped.status `shouldBe` Issues.RunCancelled
      dbExec tr "UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?" (ProjectMembers.PEdit, testPid, user)

  it "nativeFix_applicationResumesSdkSessionsWithoutReplacingItsRunOrAttempts" \tr -> withTemp "native-fix-job" \directory -> do
    image <- nativeTestImage
    store <- storeIn directory
    let repository = directory </> "repository"
        uid n = UUIDId $ UUID.fromWords 96 0x4000 0x80000000 n
        fid = uid 2
        script = "set -eu\n. ./runtime.sh\n[ \"$(retry_token original)\" = original ] || { echo 'retry token lost'; exit 1; }\n" :: Text
        steps =
          [ SdkTool "reproduce_issue" $ AE.object ["script" AE..= ("bash regression.sh" :: Text), "failureMessage" AE..= ("retry token lost" :: Text), "testFiles" AE..= V.singleton (AE.object ["path" AE..= ("regression.sh" :: Text), "content" AE..= script])]
          , SdkTool "write" $ AE.object ["path" AE..= ("runtime.sh" :: Text), "content" AE..= ("retry_token() { printf '%s\\n' \"$1\"; }\n" :: Text)]
          , SdkTool "propose_fix" $ AE.object ["rootCause" AE..= ("retry_token drops its caller's token." :: Text)]
          , SdkTool "verify_fix" $ AE.object []
          , SdkTool "submit_fix" $ AE.object ["summary" AE..= ("Preserve the caller's retry token." :: Text)]
          , SdkText "Fix complete."
          ]
    initRepo repository
    base <- commit repository [("runtime.sh", "retry_token() { printf 'lost\\n'; }\n")]
    imageId <- readJSON (image </> "manifest.json") >>= parse (field "buildId")
    (mapping, issue, _) <- fixFixture tr "sdk-job-recovery"
    baseRevision <- fixRevision base
    runQueryEffect tr (Fixes.createRun testPid fid issue mapping.id baseRevision imageId) `shouldReturn` Just fid
    runQueryEffect tr (Fixes.queueJob testPid (getResponse tr.trSessAndHeader).user.id fid) `shouldReturn` True
    [original] <- runQueryEffect tr $ Issues.listRoutineRuns testPid 12
    original.source `shouldBe` Issues.FixRun fid
    appConfig <- githubApp tr
    nativeFailures <- newIORef ([] :: [Text])
    interruptions <- newIORef (["admission", "candidate", "peer"] :: [Text])
    let provider body results = do
          tools <- toolNames body
          let admission = "submit_admission" `elem` tools
              peer = "submit_fix_review" `elem` tools
              boundary
                | admission && length results == 1 = Just "admission"
                | peer && length results == 1 = Just "peer"
                | not admission && not peer && length results == 3 = Just "candidate"
                | otherwise = Nothing
          remainingInterruptions <- readIORef interruptions
          if maybe False (`elem` remainingInterruptions) boundary
            then do
              modifyIORef' interruptions $ filter (\value -> Just value /= boundary)
              pure $ SdkUnavailable "Fixture interrupts an unfinished native session"
            else
              pure
                $ (`SdkResponse` SdkTokens 7 0)
                $ if admission
                  then case length results of
                    0 -> bash "cat runtime.sh /run/monoscope-admission.json"
                    1 -> SdkTool "submit_admission" $ AE.object ["tag" AE..= ("Admitted" :: Text), "contents" AE..= ("The regression exercises the caller's retry token." :: Text)]
                    _ -> SdkText "Admission complete."
                  else
                    if peer
                      then case length results of
                        0 -> bash "cat runtime.sh regression.sh /run/monoscope-fix-review.json"
                        1 -> SdkTool "submit_fix_review" $ AE.toJSON $ Agent.ReviewApproved "The retained regression and source change preserve the caller's token."
                        _ -> SdkText "Review complete."
                      else fromMaybe (SdkText "Unexpected repeated inference") $ viaNonEmpty head $ drop (length results) steps
        cfg = appConfig{impactReviewUsageDir = Just store.usageDir, impactReviewCacheDir = Just store.cacheDir, impactReviewProvider = "fix-job-fixture", openaiModel = "fixture", impactReviewArtifactBucket = Just "fixture-bucket"}
        transport = githubResponses $ const $ pure Nothing
    withSdkProvider directory "fix-job-fixture" provider \models observed -> do
      let recordNative :: IOE :> es => Eff es (Either Text a) -> Eff es (Either Text a)
          recordNative action = do
            answer <- action
            for_ (leftToMaybe answer) (\message -> liftIO $ modifyIORef' nativeFailures (<> [message]))
            pure answer
          localFix :: Agent.FixRequest -> Agent.FixRequest
          localFix request = request{Agent.request = request.request{Agent.repository = toText repository}}
          localArtifacts :: Agent.ArtifactRequest -> Agent.ArtifactRequest
          localArtifacts artifacts = artifacts{Agent.repository = toText repository}
          backend :: ATBackgroundCtx a -> ATBackgroundCtx a
          backend =
            interpose @Artifacts.AgentArtifacts
              ( \_ -> \case
                  Artifacts.RetainFixMedia explained -> Artifacts.runAgentArtifactsTest (const $ Right ()) (\_ _ -> Left Artifacts.ArtifactUnavailable) $ Artifacts.retainFixMedia explained
                  _ -> liftIO $ fail "Unexpected artifact publication"
              )
              . interpose @LLM.LLM (\_ _ -> liftIO $ fail "Legacy LLM used by native continuation")
              . impose
                (sdkAgent store [models, image, directory </> "worker.log"])
                ( \env -> \case
                    Agent.ReadSessionContext (Agent.PeerSubject fixId artifacts) root -> recordNative $ passthrough env $ Agent.ReadSessionContext (Agent.PeerSubject fixId $ localArtifacts artifacts) root
                    Agent.ReadSessionContext subject root -> recordNative $ passthrough env $ Agent.ReadSessionContext subject root
                    Agent.FixRepository selected request queryTelemetry knowledge admit calls usage allocations checks phase validate ->
                      recordNative $ passthrough env $ Agent.FixRepository selected (localFix request) queryTelemetry knowledge admit calls usage allocations checks phase validate
                    Agent.AdmitRegression selected request knowledge calls usage allocations ->
                      recordNative $ passthrough env $ Agent.AdmitRegression selected request{Agent.fix = localFix request.fix} knowledge calls usage allocations
                    Agent.VerifyFixArtifacts artifacts request -> recordNative $ passthrough env $ Agent.VerifyFixArtifacts artifacts $ localArtifacts request
                    Agent.ReviewVerifiedFix selected request verified knowledge calls usage allocations ->
                      recordNative $ passthrough env $ Agent.ReviewVerifiedFix selected request{Agent.fix = localFix request.fix, Agent.artifacts = localArtifacts request.artifacts} verified knowledge calls usage allocations
                    Agent.ExplainVerifiedFix artifacts request verified -> recordNative $ passthrough env $ Agent.ExplainVerifiedFix artifacts (localArtifacts request) verified
                    _ -> liftIO $ fail "Unexpected native workflow"
                )
          worker at = runTestBgRecordingHTTP at tr $ Reader.local (\ctx -> ctx{config = if at == frozenTime then cfg else cfg{impactReviewProvider = "changed-after-interruption", openaiModel = "changed-after-interruption"}}) $ transport $ backend $ BackgroundJobs.processBackgroundJob tr.trATCtx $ Jobs.FixIssue testPid fid
      for_ [0, 60, 120] \elapsed -> do
        outcome <- tryAny $ worker $ addUTCTime elapsed frozenTime
        isLeft outcome `shouldBe` True
        Just failed <- runQueryEffect tr $ Issues.getRoutineRun testPid original.id
        failed.status `shouldBe` Issues.RunFailed
        failed.source `shouldBe` original.source
      readIORef interruptions `shouldReturn` []
      continued <- tryAny $ worker $ addUTCTime 180 frozenTime
      whenLeft_ continued \failure -> readIORef nativeFailures >>= fail . toString . ((show failure <> "\n") <>) . T.intercalate "\n"
      result <- maybe (fail "Resumed job did not retain its verified fix") pure =<< runQueryEffect tr (Fixes.completedResult testPid fid)
      result.runId `shouldBe` fid
      result.summary `shouldBe` "Preserve the caller's retry token."
      Just completed <- runQueryEffect tr $ Issues.getRoutineRun testPid original.id
      completed.status `shouldBe` Issues.RunSucceeded
      artifacts <- runQueryEffect tr $ Issues.listRunArtifacts testPid original.id
      map (toText . (.key)) artifacts `shouldContain` ["latest-result"]
      runs <- runQueryEffect tr $ Issues.listRoutineRuns testPid 12
      map (.id) runs `shouldBe` [original.id]
      dbOne tr "SELECT count(*)::int FROM projects.pr_review_attempts WHERE project_id = ? AND fix_run_id = ?" (testPid, fid) `shouldReturn` (3 :: Int)
      dbOne tr "SELECT count(*)::int FROM projects.pr_review_checks WHERE project_id = ? AND fix_run_id = ?" (testPid, fid) `shouldReturn` (3 :: Int)
      beforeReplay <- length <$> observed
      (_, ()) <- worker $ addUTCTime 240 frozenTime
      length <$> observed `shouldReturn` beforeReplay
      runQueryEffect tr (Fixes.completedResult testPid fid) >>= (`shouldSatisfy` isJust)
      map (toText . (.key)) <$> runQueryEffect tr (Issues.listRunArtifacts testPid original.id) `shouldReturn` map (toText . (.key)) artifacts

  it "autofix_queuesEachTriagedIssueOnceUntilItsRoutineIsPaused" \tr -> do
    (_, issue, _) <- fixFixture tr "autofix-runtime"
    let sweepAt = addUTCTime 7200 frozenTime
        seed ty target at = seedIssue tr (issueSeed ty target){service = Just "test-service", at}
    void $ seed Issues.ApiChange "autofix-schema" frozenTime
    void $ seed Issues.Performance "autofix-fresh" (addUTCTime (-60) sweepAt)
    Right (routine, _) <- runTestBg frozenTime tr $ Issues.ensureRoutineTemplate (getResponse tr.trSessAndHeader :: Projects.Session).user.id testPid "UTC" Issues.issueAutofixTemplate
    cfg <- githubApp tr
    let sweep = void $ runTestBgRecordingHTTP sweepAt tr $ Reader.local (\ctx -> ctx{config = cfg}) $ githubResponses (\url -> pure $ AE.object ["sha" AE..= revision] <$ guard ("/commits/" `T.isInfixOf` url)) $ BackgroundJobs.processBackgroundJob tr.trATCtx $ Jobs.AutofixIssues testPid
        fixed = dbQuery tr "SELECT run.issue_id FROM projects.issue_fix_runs run JOIN projects.issue_fix_jobs job ON job.run_id = run.id ORDER BY run.created_at" ()
    dbExec tr [sql|UPDATE apis.issues SET title = ? WHERE id = ?|] (T.replicate 20 "Vacuum failed ", issue)
    -- API changes and issues triage has not had an hour with are skipped; a second sweep adds nothing.
    sweep >> sweep
    fixed `shouldReturn` [Only issue]
    -- fixTitle_marksTheCut: a long issue title is cut with an ellipsis, not mid-word without one.
    dbOne tr "SELECT title FROM apis.ai_conversations WHERE title LIKE 'Fix: %'" () >>= (`shouldSatisfy` T.isSuffixOf "…")
    dbOne tr "SELECT count(*)::int FROM background_jobs_long WHERE payload->>'tag' = 'FixIssue'" () `shouldReturn` (1 :: Int)
    -- The routine's page lists the fix run it started, though fix runs carry no routine_id.
    fixId <- dbOne tr "SELECT run_id FROM projects.issue_fix_jobs" ()
    map ((.source) &&& (.routine)) <$> runQueryEffect tr (Issues.listConversationRuns testPid routine 5) `shouldReturn` [(Issues.FixRun fixId, Just "Issue autofix")]
    void $ seed Issues.RuntimeException "autofix-paused" frozenTime
    runQueryEffect tr $ Issues.pauseRoutine testPid routine
    sweep
    fixed `shouldReturn` [Only issue]

  it "nativeFix_applicationJobSharesRunAuthorityReceiptsAndCompletion" \tr -> do
    (mapping, issue, base) <- fixFixture tr "application-fix"
    let uid n = UUIDId $ UUID.fromWords 95 0 0 n
        fid = uid 2
        image = uid 3
        user = (getResponse tr.trSessAndHeader :: Projects.Session).user.id
    runQueryEffect tr (Fixes.createRun testPid fid issue mapping.id base image) `shouldReturn` Just fid
    runQueryEffect tr (Fixes.queueJob testPid user fid) `shouldReturn` True
    runQueryEffect tr (Fixes.queueJob testPid user fid) `shouldReturn` True
    [shared] <- runQueryEffect tr $ Issues.listRoutineRuns testPid 12
    shared.source `shouldBe` Issues.FixRun fid
    runQueryEffect tr (Issues.runContextFor testPid $ Issues.ConversationClaim shared.id) `shouldReturn` Nothing
    dbQuery tr "SELECT payload FROM background_jobs_long WHERE payload->>'tag' = 'FixIssue'" () `shouldReturn` [Only $ Aeson $ AE.toJSON $ Jobs.FixIssue testPid fid]
    unleased <- runQueryEffect tr $ Fixes.recordStep testPid fid 0 (uid 6) Fixes.RestartReproduction
    unleased `shouldSatisfy` isLeft
    lease <- maybe (fail "Fix lease missing") pure =<< runQueryEffect tr (Fixes.claimJob testPid fid)
    runQueryEffect tr (isJust <$> Fixes.claimJob testPid fid) `shouldReturn` False
    runQueryEffect tr (Fixes.currentJob lease) `shouldReturn` True
    Just originalContext <- runQueryEffect tr $ Fixes.agentRunContext lease
    let foreignFix = uid 990
    runQueryEffect tr (Fixes.createRun testPid foreignFix issue mapping.id base image) `shouldReturn` Just foreignFix
    foreignAttempt <- maybe (fail "Foreign attempt missing") pure =<< runQueryEffect tr (Usage.startAttempt testPid (Usage.Fix foreignFix) "fixture" "fixture")
    deniedTitle <- either (fail . toString) pure $ Memory.mkMemoryText "Fenced wiki write"
    deniedBody <- either (fail . toString) pure $ Memory.mkMemoryText "This unowned write must not be retained."
    let deniedEdit = Memory.Edit (uid 991) 0 $ Memory.Document deniedTitle deniedBody
        write authorization = runTestBgNoReset tr $ AI.executeNativeOperation authorization (Catalog.WriteWiki $ Catalog.WikiWrite foreignAttempt deniedEdit) \case
          Catalog.WriteWiki proposal -> AI.AtomicOperation $ bimap show AE.toJSON <$> Memory.writeRunPageTx testPid authorization.id proposal.edit
          (Catalog.PublishPullRequestComment{}; Catalog.EnrichIssue{}) -> AI.EffectfulOperation $ throwIO Investigations.OperationProposalChanged
    write originalContext `shouldThrow` (\case Investigations.OperationScopeMismatch -> True; _ -> False)
    runQueryEffect tr (Memory.pageHistory testPid deniedEdit.pageId) >>= (`shouldSatisfy` null)
    dbExec tr "UPDATE projects.issue_fix_jobs SET agent_run_id = ? WHERE run_id = ?" (uid 992 :: UUIDId "ai_routine_run", fid)
      `shouldThrow` (\err -> "Fix run link is immutable" `T.isInfixOf` toText (displayException (err :: SomeException)))
    dbExec tr "UPDATE projects.issue_fix_jobs SET lease_until = now() - interval '1 second' WHERE run_id = ?" (Only fid)
    replacement <- maybe (fail "Expired lease not reclaimed") pure =<< runQueryEffect tr (Fixes.claimJob testPid fid)
    runQueryEffect tr $ Fixes.releaseJob lease
    runQueryEffect tr (Fixes.currentJob replacement) `shouldReturn` True
    runQueryEffect tr (Issues.runContextAuthority originalContext) `shouldReturn` Nothing
    write originalContext `shouldThrow` (const True :: AI.AgentStopped -> Bool)
    stale <- runQueryEffect tr $ Fixes.recordJobStep lease 0 (uid 4) Fixes.RestartReproduction
    stale `shouldSatisfy` isLeft
    runQueryEffect tr $ Fixes.releaseJob replacement
    apiKey <- createTestAPIKey tr testPid "Fix job telemetry"
    ingestLog tr apiKey "fix-job-event" (addUTCTime (-60) frozenTime)
    savedFixContexts <- newIORef Map.empty
    lostSessionCalls <- newIORef (0 :: Int)
    revokedReplies <- newIORef []
    appConfig <- githubApp tr
    let cfg = appConfig{impactReviewUsageDir = Just "fixture-usage", impactReviewCacheDir = Just "fixture-cache", impactReviewProvider = "fixture", openaiModel = "gpt-5.6-luna#high", impactReviewArtifactBucket = Just "fixture-bucket", s3Endpoint = "http://storage.invalid"}
        memoryRoundTrip attempt respond = do
          let page = case attempt of UUIDId value -> UUIDId value
          title <- either (fail . toString) pure $ Memory.mkMemoryText "Retry context"
          body <- either (fail . toString) pure $ Memory.mkMemoryText "The runtime must preserve caller retry tokens. Recheck this note against current source."
          let edit = Memory.Edit page 0 $ Memory.Document title body
              send = respond . Agent.ProjectMemory
          created <- send $ Memory.Write edit
          created `shouldSatisfy` isRight
          send (Memory.Read page) `shouldReturn` created
          listed <- send Memory.List
          listed `shouldSatisfy` isRight
          send (Memory.Write edit) `shouldReturn` created
          isLeft <$> send (Memory.Read $ uid 997) `shouldReturn` True
        fakeAgent :: Text -> ATBackgroundCtx a -> ATBackgroundCtx a
        fakeAgent mode =
          interpose @Artifacts.AgentArtifacts
            ( \_ -> \case
                Artifacts.RetainFixMedia explained -> Artifacts.runAgentArtifactsTest (const $ if mode == "--app-job-retention-denied" then Left Artifacts.StorageUnavailable else Right ()) (\_ _ -> Left Artifacts.ArtifactUnavailable) $ Artifacts.retainFixMedia explained
                Artifacts.ReadFixMedia{} -> pure $ Left Artifacts.ArtifactUnavailable
                Artifacts.PackageFixMedia{} -> pure $ Left Artifacts.PublicationUnavailable
            )
            . interpose @LLM.LLM (\_ _ -> liftIO $ fail "Legacy LLM used by source-fix job")
            . interpose @Agent.CodeReview
              ( \env -> \case
                  Agent.ReadSessionContext (Agent.FixSubject fixId) _ -> Right . Map.lookup fixId <$> liftIO (readIORef savedFixContexts)
                  Agent.FixRepository scope request queryTelemetry knowledge admit calls usage allocations checks phase validate -> localSeqUnliftIO env \unlift -> liftIO $ withSystemTempDirectory "job-fix-agent" \directory -> do
                    for_ (case scope of Agent.Isolated -> Nothing; Agent.Retained _ capture -> Just capture) $ \capture -> unlift capture >>= \context -> modifyIORef' savedFixContexts (Map.insert request.request.runId context)
                    when (mode == "--app-job-lost-session") $ modifyIORef' lostSessionCalls (+ 1)
                    let diagnostic = directory </> "failure.txt"
                    if mode `elem` ["--app-job-revoke-account", "--app-job-revoke-role"]
                      then do
                        heading <- either (fail . toString) pure $ Memory.mkMemoryText "Revocation context"
                        contents <- either (fail . toString) pure $ Memory.mkMemoryText "Keep the original authorized revision only."
                        let page = case request.request.attemptId of UUIDId value -> UUIDId value
                            edit = Memory.Edit page 0 $ Memory.Document heading contents
                            send = unlift . knowledge . Agent.ProjectMemory
                            access enabled =
                              if mode == "--app-job-revoke-account"
                                then dbExec tr "UPDATE users.users SET active = ? WHERE id = ?" (enabled, user)
                                else dbExec tr "UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?" (if enabled then ProjectMembers.PAdmin else ProjectMembers.PView, testPid, user)
                        created <- send $ Memory.Write edit
                        (readBack, changed, resubmitted) <- bracket_ (access False) (access True) do
                          readBack <- send $ Memory.Read page
                          changed <- send $ Memory.Write edit{Memory.version = 1}
                          resubmitted <- runQueryEffect tr $ Fixes.queueJob testPid user request.request.runId
                          pure (readBack, changed, resubmitted)
                        restored <- send $ Memory.Read page
                        modifyIORef' revokedReplies (<> [(isRight created, isLeft readBack, isLeft changed, not resubmitted, isLeft restored)])
                        pure $ Left "The original fix grant was revoked"
                      else do
                        when (mode == "--app-job") $ memoryRoundTrip request.request.attemptId (unlift . knowledge)
                        result <-
                          runEff
                            $ scripted Nothing "test/fixtures/issue-fix-agent.mjs" [diagnostic, toString image.toText, toString mode]
                            $ Agent.fixRepository
                              Agent.Isolated
                              request
                              (liftIO . unlift . queryTelemetry)
                              (liftIO . unlift . knowledge)
                              (liftIO . unlift . admit)
                              (liftIO . unlift . calls)
                              (liftIO . unlift . usage)
                              (liftIO . unlift . allocations)
                              (liftIO . unlift . checks)
                              (liftIO . unlift . phase)
                              (liftIO . unlift . validate)
                        detail <- tryAny $ readFileBS diagnostic
                        pure $ first (<> either (const "") (("\n" <>) . decodeUtf8) detail) result
                  Agent.AdmitRegression _scope request knowledge calls usage allocations -> localSeqUnliftIO env \unlift -> liftIO do
                    when (mode == "--app-job") $ memoryRoundTrip request.fix.request.attemptId (unlift . knowledge)
                    runEff
                      $ scripted Nothing "test/fixtures/regression-admission-agent.mjs" [if request.artifact.testPatch == "admitted-test-patch" then "" else "--reject"]
                      $ Agent.admitRegression Agent.Isolated request (liftIO . unlift . knowledge) (liftIO . unlift . calls) (liftIO . unlift . usage) (liftIO . unlift . allocations)
                  Agent.VerifyFixArtifacts _ request -> artifactProofFixture (if mode == "--app-job-artifacts-denied" then Left "Fixture rejects artifacts" else Right ()) request
                  Agent.ReviewVerifiedFix _scope request verified knowledge calls usage allocations -> localSeqUnliftIO env \unlift -> liftIO do
                    when (mode == "--app-job") $ memoryRoundTrip request.fix.request.attemptId (unlift . knowledge)
                    runEff
                      $ scripted Nothing "test/fixtures/fix-review-agent.mjs" [if mode == "--app-job-review-changes" then "--reject" else if mode == "--app-job-review-blocked" then "--blocked" else ""]
                      $ Agent.reviewVerifiedFix Agent.Isolated request verified (liftIO . unlift . knowledge) (liftIO . unlift . calls) (liftIO . unlift . usage) (liftIO . unlift . allocations)
                  Agent.ExplainVerifiedFix store request verified -> explanationProofFixture store (if mode == "--app-job-explanation-denied" then Left "Fixture rejects explanation" else Right $ fixtureExplanation request.result) request verified
                  operation -> pure $ refuse operation
              )
        transport = githubResponses $ const $ pure Nothing
        run fid' mode = runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = if mode == "--app-job-no-storage" then cfg{impactReviewArtifactBucket = Nothing} else cfg}) $ transport $ fakeAgent mode $ BackgroundJobs.processBackgroundJob tr.trATCtx (Jobs.FixIssue testPid fid')
        notes = length <$> runQueryEffect tr (Reviews.priorRepositoryKnowledge testPid (Git.RepoRef "impact-org" "checkout" revision) "test-service")
        queue jobId = do
          runQueryEffect tr (Fixes.createRun testPid jobId issue mapping.id base image) `shouldReturn` Just jobId
          runQueryEffect tr (Fixes.queueJob testPid user jobId) `shouldReturn` True
        -- A refused job completes nothing.
        refused jobId mode = do
          tryAny (run jobId mode) >>= (`shouldSatisfy` isLeft)
          runQueryEffect tr (Fixes.completedResult testPid jobId) >>= (`shouldSatisfy` isNothing)
        active enabled = dbExec tr "UPDATE users.users SET active = ? WHERE id = ?" (enabled, user)
    tryAny (run fid "--app-job-no-storage") >>= (`shouldSatisfy` isLeft)
    initialCosts <- runQueryEffect tr $ Usage.usageTotals Usage.Fixes testPid
    maybe 0 (.usage.calls) (find ((== Just fid) . (.runId)) initialCosts) `shouldBe` 0
    runQueryEffect tr (isJust <$> Fixes.publicationForUser testPid fid user) `shouldReturn` False
    -- A lost session store must not turn recorded inference into a fresh attempt.
    let lostId = uid 993
    queue lostId
    lostAttempt <- maybe (fail "Lost-session attempt missing") pure =<< runQueryEffect tr (Usage.startAttempt testPid (Usage.Fix lostId) "fixture" "fixture")
    runQueryEffect tr (Usage.recordCall testPid (Usage.Fix lostId) $ Agent.ModelCall (uid 994) lostAttempt "fixture" "fixture") `shouldReturn` True
    void $ tryAny $ run lostId "--app-job-lost-session"
    readIORef lostSessionCalls `shouldReturn` 0
    dbOne tr [sql|SELECT failure FROM projects.issue_fix_jobs WHERE run_id = ?|] (Only lostId) `shouldReturn` (Just "Saved progress was lost, so this run cannot resume. Start it again for a fresh run." :: Maybe Text)
    lostRuns <- filter ((== Issues.FixRun lostId) . (.source)) <$> runQueryEffect tr (Issues.listRoutineRuns testPid 12)
    map (.status) lostRuns `shouldBe` [Issues.RunFailed]
    map (.error) lostRuns `shouldBe` [Just "Saved progress was lost, so this run cannot resume. Start it again for a fresh run."]
    dbOne tr [sql|SELECT count(*)::int FROM projects.pr_review_attempts WHERE project_id = ? AND fix_run_id = ?|] (testPid, lostId) `shouldReturn` (1 :: Int)
    runQueryEffect tr (Fixes.completedResult testPid lostId) >>= (`shouldSatisfy` isNothing)
    (requests, ()) <- run fid "--app-job"
    requests `shouldNotSatisfy` any (\(url, _) -> "/pulls" `T.isInfixOf` url || "/comments" `T.isInfixOf` url)
    completed <- maybe (fail "Verified application result missing") pure =<< runQueryEffect tr (Fixes.completedResult testPid fid)
    Fixes.revisionText completed.head `shouldBe` T.replicate 40 "b"
    completed.runId `shouldBe` fid
    memories <- runQueryEffect tr $ Memory.latestPages testPid
    length memories `shouldBe` 4
    map (.author) memories `shouldBe` replicate 4 (Memory.Run shared.id)
    Just finishedRun <- runQueryEffect tr $ Issues.getRoutineRun testPid shared.id
    finishedRun.status `shouldBe` Issues.RunSucceeded
    let renderRun = renderT . snd <$> testServant tr (AIThreads.routineRunGetH testPid shared.id Nothing)
        permission role = dbExec tr "UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?" (role, testPid, user)
    adminPage <- renderRun
    adminPage `shouldContainAll` ["Code fix", "Fix workflow", "Verified fix evidence", "Bug reproduction and verified fix"]
    permission ProjectMembers.PView
    viewerPage <- renderRun `finally` permission ProjectMembers.PAdmin
    viewerPage `shouldSatisfy` T.isInfixOf "The verified fix is retained. Administrators can review its artifacts."
    viewerPage `shouldNotSatisfy` T.isInfixOf "Verified fix evidence"
    viewerPage `shouldNotSatisfy` T.isInfixOf ("/issue-fixes/" <> fid.toText)
    operations <- runQueryEffect tr $ Investigations.listOperations @AI.OperationPolicy testPid shared.id
    length operations `shouldBe` 4
    map (.version) memories `shouldBe` [1, 1, 1, 1]
    dbOne tr "SELECT coalesce(to_jsonb(j)->'explanation', 'null'::jsonb) <> 'null'::jsonb AND j.review_attempt_id IS NOT NULL AND j.review_rationale IS NOT NULL FROM projects.issue_fix_jobs j WHERE run_id = ?" (Only fid) `shouldReturn` True
    explanation <- maybe (fail "Completed explanation missing") pure =<< runQueryEffect tr (Fixes.completedExplanation testPid fid)
    explanation.head `shouldBe` completed.head
    explanation.resourceCostStatus `shouldBe` Agent.Unpriced
    explanation.regressionSha256 `shouldBe` completed.regressionSha256
    completedCapability <- maybe (fail "Authorized publication capability missing") pure =<< runQueryEffect tr (Fixes.publicationForUser testPid fid user)

    publicationRequest <- either (fail . show) pure $ Publication.requestFor completedCapability "main" Publication.Private "https://monoscope.example"
    credential <- either (fail . show) pure $ Publication.uploadCredential $ GitSync.PersonalToken "ghp_fixture"
    leftToMaybe (Publication.uploadCredential $ GitSync.AppInstallation 987654) `shouldBe` Just Publication.NativeUnavailable
    let receipt = Publication.Receipt 7 "https://github.com/impact-org/checkout/pull/7" completed.head explanation.source (show (hash (toStrict $ AE.encode $ Map.toAscList explanation.files) :: Digest SHA256)) (Publication.MediaUrls "https://github.com/user-attachments/assets/00000000-0000-4000-8000-000000000001" "https://github.com/user-attachments/assets/00000000-0000-4000-8000-000000000002") True Publication.Open
        authorizePublication _ = pure $ Right ()
        checkPublication _ = pure $ Right ()
        publish response authorize = runEff $ Publication.runGitPublicationTest (const $ Right response) $ Publication.publishFix publicationRequest credential authorize checkPublication
    fmap Publication.publicationReceipt <$> publish receipt authorizePublication `shouldReturn` Right receipt
    rejected <- publish (receipt{Publication.source = T.replicate 64 "f"} :: Publication.Receipt) authorizePublication
    leftToMaybe rejected `shouldBe` Just Publication.InvalidReceipt
    deniedFake <- publish receipt (const $ pure $ Left Publication.AccessRevoked)
    leftToMaybe deniedFake `shouldBe` Just Publication.AccessRevoked
    disabledRequests <- newIORef []
    disabled <- runEff $ HTTP.runHTTPRecord disabledRequests $ Publication.runGitPublicationReal Nothing Nothing $ Publication.publishFix publicationRequest credential authorizePublication checkPublication
    leftToMaybe disabled `shouldBe` Just Publication.NotConfigured
    active True
    dbOne tr "SELECT to_jsonb(j)->>'media_retained_at' IS NOT NULL FROM projects.issue_fix_jobs j WHERE run_id = ?" (Only fid) `shouldReturn` True
    runQueryEffect tr (isJust <$> Fixes.claimJob testPid fid) `shouldReturn` False
    (_, ()) <- run fid "--app-job"
    totals <- runQueryEffect tr $ Usage.usageTotals Usage.Fixes testPid
    fmap (\total -> (total.usage.calls, total.usage.costNanousd)) (find ((== Just fid) . (.runId)) totals) `shouldBe` Just (4, 168000)
    fmap (\total -> (total.resources.checks, total.resources.vcpuMs, total.resources.explanations, total.resources.renderMs, total.resources.mediaBytes)) (find ((== Just fid) . (.runId)) totals) `shouldBe` Just (3, 700, 1, 1200, fromIntegral explanation.mediaBytes)
    knowledge <- runQueryEffect tr $ Reviews.priorRepositoryKnowledge testPid (Git.RepoRef "impact-org" "checkout" revision) "test-service"
    map (\note -> AE.toJSON note.source) knowledge `shouldBe` [AE.toJSON $ Reviews.FixSource fid]
    map (.revision) knowledge `shouldBe` [T.replicate 40 "b"]
    Aeson evidence <- dbOne tr "SELECT evidence FROM projects.pr_review_knowledge WHERE fix_run_id = ?" (Only fid) :: IO (Aeson AE.Value)
    jsonText evidence `shouldNotSatisfy` T.isInfixOf "dataset"
    let failedId = uid 5
    queue failedId
    refused failedId "--app-job-tampered"
    dbQuery tr "SELECT failure, lease_owner FROM projects.issue_fix_jobs WHERE run_id = ?" (Only failedId) `shouldReturn` [(Just "Issue fix failed; retry to collect current evidence" :: Maybe Text, Nothing :: Maybe UUID.UUID)]
    notes `shouldReturn` 1
    refused failedId "--app-job-artifacts-denied"
    notes `shouldReturn` 1
    refused failedId "--app-job-explanation-denied"
    runQueryEffect tr (Fixes.completedExplanation testPid failedId) >>= (`shouldSatisfy` isNothing)
    unfinished <- runQueryEffect tr $ Usage.usageTotals Usage.Fixes testPid
    fmap (.resources.explanations) (find ((== Just failedId) . (.runId)) unfinished) `shouldBe` Just 0
    notes `shouldReturn` 1
    for_ ["native", "run"] $ \boundary -> do
      let ddl statement = dbExec tr statement ()
          add = if boundary == ("native" :: Text) then "ALTER TABLE projects.issue_fix_jobs ADD CONSTRAINT reject_fix_commit CHECK (result IS NULL) NOT VALID" else "ALTER TABLE apis.ai_routine_runs ADD CONSTRAINT reject_fix_commit CHECK (status <> 'succeeded') NOT VALID"
          remove = if boundary == "native" then "ALTER TABLE projects.issue_fix_jobs DROP CONSTRAINT reject_fix_commit" else "ALTER TABLE apis.ai_routine_runs DROP CONSTRAINT reject_fix_commit"
      bracket_ (ddl add) (ddl remove) $ refused failedId "--app-job-completion-denied"
      identifier <- dbOne tr "SELECT agent_run_id FROM projects.issue_fix_jobs WHERE run_id = ?" (Only failedId)
      Just failedRun <- runQueryEffect tr $ Issues.getRoutineRun testPid identifier
      failedRun.status `shouldBe` Issues.RunFailed
      runQueryEffect tr (Issues.listRunArtifacts testPid identifier) >>= (\artifacts -> map (toText . (.key)) artifacts `shouldNotContain` ["latest-result"])
    refused failedId "--app-job-retention-denied"
    notes `shouldReturn` 1
    checkpointLease <- maybe (fail "Checkpoint lease missing") pure =<< runQueryEffect tr (Fixes.claimJob testPid failedId)
    runQueryEffect tr (isJust <$> Fixes.getCheckpoint checkpointLease) `shouldReturn` True
    active False
    runQueryEffect tr (isJust <$> Fixes.getCheckpoint checkpointLease) `shouldReturn` False
    active True
    runQueryEffect tr $ Fixes.releaseJob checkpointLease
    runQueryEffect tr (isJust <$> Fixes.getCheckpoint checkpointLease) `shouldReturn` False
    (_, ()) <- run failedId "--app-job"
    runQueryEffect tr (Fixes.completedResult testPid failedId) >>= (`shouldSatisfy` isJust)
    recoveredCosts <- runQueryEffect tr $ Usage.usageTotals Usage.Fixes testPid
    fmap (.usage.calls) (find ((== Just failedId) . (.runId)) recoveredCosts) `shouldBe` Just 11
    notes `shouldReturn` 2
    for_ ([(uid 20, "--app-job-review-changes"), (uid 21, "--app-job-review-blocked")] :: [(Fixes.FixId, Text)]) \(reviewId, mode) -> do
      queue reviewId
      refused reviewId mode
      runQueryEffect tr (isJust <$> Fixes.publicationForUser testPid reviewId user) `shouldReturn` False
      dbOne tr "SELECT verified_result IS NOT NULL OR review_attempt_id IS NOT NULL FROM projects.issue_fix_jobs WHERE run_id = ?" (Only reviewId) `shouldReturn` False
      notes `shouldReturn` 2
    for_ ([(uid 30, "--app-job-revoke-account"), (uid 31, "--app-job-revoke-role")] :: [(Fixes.FixId, Text)]) $ \(revokedId, mode) -> do
      queue revokedId
      tryAny (run revokedId mode) >>= (`shouldSatisfy` isLeft)
      identifier <- dbOne tr "SELECT agent_run_id FROM projects.issue_fix_jobs WHERE run_id = ?" (Only revokedId)
      Just cancelled <- runQueryEffect tr $ Issues.getRoutineRun testPid identifier
      cancelled.status `shouldBe` Issues.RunCancelled
      Just grantEvidence <- runQueryEffect tr $ Issues.getRoutineRunEvidence testPid identifier
      grantEvidence.requestedBy `shouldBe` Just user
      runQueryEffect tr (isJust <$> Fixes.claimJob testPid revokedId) `shouldReturn` False
    readIORef revokedReplies `shouldReturn` replicate 2 (True, True, True, True, True)
    let interrupted = uid 40
        retainedPage = uid 41
    queue interrupted
    tryAny (run interrupted "--app-job-wiki-exit") >>= (`shouldSatisfy` isLeft)
    sharedId <- dbOne tr "SELECT agent_run_id FROM projects.issue_fix_jobs WHERE run_id = ?" (Only interrupted)
    Just stopped <- runQueryEffect tr $ Issues.getRoutineRun testPid sharedId
    stopped.status `shouldBe` Issues.RunFailed
    [retainedOperation] <- runQueryEffect tr $ Investigations.listOperations @AI.OperationPolicy testPid sharedId
    [revisionBefore] <- runQueryEffect tr $ Memory.pageHistory testPid retainedPage
    (_, ()) <- run interrupted "--app-job-wiki-retry"
    Just recovered <- runQueryEffect tr $ Issues.getRoutineRun testPid sharedId
    recovered.status `shouldBe` Issues.RunSucceeded
    [resumedOperation] <- runQueryEffect tr $ Investigations.listOperations @AI.OperationPolicy testPid sharedId
    (resumedOperation.id, AE.toJSON resumedOperation.proposal, AE.toJSON resumedOperation.outcome) `shouldBe` (retainedOperation.id, AE.toJSON retainedOperation.proposal, AE.toJSON retainedOperation.outcome)
    runQueryEffect tr (Memory.pageHistory testPid retainedPage) `shouldReturn` [revisionBefore]
    revisionBefore.author `shouldBe` Memory.Run sharedId
    Just retriedFix <- runQueryEffect tr $ Fixes.completedResult testPid interrupted
    retainedOperation.proposal.call `shouldSatisfy` \case
      Catalog.NativeCall (Catalog.WriteWiki proposal) -> retriedFix.attemptId == proposal.attemptId
      _ -> False
    requireMinio tr pendingWith $ withSystemTempDirectory "retained-fix-media" \mediaRoot -> do
      current <- runQueryEffect tr (Fixes.getRun testPid fid) >>= either (fail . show) (maybe (fail "Completed run missing") pure)
      artifacts <- maybe (fail "Verified comparison missing") pure $ Fixes.artifactRequest "https://github.com/impact-org/checkout.git" current completed
      let storeCtx = tr.trATCtx{config = tr.trATCtx.config{impactReviewArtifactBucket = Just tr.trATCtx.config.s3Bucket, impactReviewUsageDir = Just mediaRoot, impactReviewAgent = Nothing, impactReviewSkills = []}}
      prepared <- runEff $ runExceptT do
        verified <- ExceptT $ artifactProofFixture (Right ()) artifacts
        ExceptT $ explanationProofFixture (Agent.ArtifactStore mediaRoot mediaRoot) (Right $ fixtureExplanation completed) artifacts verified
      explained <- either (fail . toString) pure prepared
      let manifest = Agent.fixExplanation explained
          readCtx = storeCtx{config = storeCtx.config{impactReviewAgent = Nothing}}
          bytesFor asset = encodeUtf8 $ Artifacts.assetFile asset :: LBS.ByteString
          mediaDirectory = mediaRoot </> "fixes" </> toString manifest.runId.toText </> "checks" </> "candidates" </> toString manifest.candidateId.toText </> "explanations" </> toString manifest.source
      createDirectoryIfMissing True mediaDirectory
      writeFileLBS (mediaDirectory </> "manifest.json") $ AE.encode manifest
      for_ [minBound .. maxBound] \asset -> writeFileLBS (mediaDirectory </> toString (Artifacts.assetFile asset)) $ bytesFor asset
      let config = minioFor readCtx.config
          keyFor name digest = T.intercalate "/" ["agent-explanations", "v1", manifest.projectId.toText, manifest.runId.toText, manifest.candidateId.toText, manifest.source, digest, name]
          cleanup = for_ (Map.toList manifest.files) \(name, digest) ->
            void $ Storage.runMinio readCtx.s3HttpManager config $ Minio.removeObject readCtx.config.s3Bucket (keyFor name digest)
      flip finally cleanup do
        stored <- runEff $ Artifacts.runAgentArtifactsReal storeCtx $ Artifacts.retainFixMedia explained
        isRight stored `shouldBe` True
        for_ [minBound .. maxBound] \asset -> do
          received <- runEff $ Artifacts.runAgentArtifactsReal readCtx $ Artifacts.readFixMedia manifest asset
          fmap Artifacts.verifiedBytes received `shouldBe` Right (bytesFor asset)
        videoDigest <- maybe (fail "Video digest missing") pure $ Map.lookup "explanation.mp4" manifest.files
        let videoKey = keyFor "explanation.mp4" videoDigest
        unsigned <- HC.parseRequest $ toString $ readCtx.config.s3Endpoint <> "/" <> readCtx.config.s3Bucket <> "/" <> videoKey
        privateResponse <- HC.httpLbs unsigned{HC.checkResponse = \_ _ -> pass} readCtx.s3HttpManager
        H.statusCode (HC.responseStatus privateResponse) `shouldBe` 403
        let withMedia :: Text -> ATBaseCtx a -> ATBaseCtx a
            withMedia mode = interpose @Artifacts.AgentArtifacts \_ -> \case
              Artifacts.RetainFixMedia{} -> pure $ Left Artifacts.StorageUnavailable
              Artifacts.PackageFixMedia result saved origin -> do
                received <- if mode == "unavailable" then pure $ Left Artifacts.StorageUnavailable else Artifacts.runAgentArtifactsReal readCtx $ Artifacts.packageFixMedia result saved origin
                when (mode == "revoke") $ liftIO $ active False
                pure received
              Artifacts.ReadFixMedia saved asset -> do
                received <- if mode == "unavailable" then pure $ Left Artifacts.StorageUnavailable else Artifacts.runAgentArtifactsReal readCtx $ Artifacts.readFixMedia saved asset
                when (mode == "revoke") $ liftIO $ active False
                pure received
            app mode =
              genericServeTWithContext
                (effToServantHandlerTest tr.trTestClock tr.trUUIDRef tr.trATCtx tr.trLogger tr.trTracerProvider . withMedia mode)
                (Routes.server (\_ _ _ -> pure $ Left "unused") (\_ _ _ -> pure $ Left "unused"))
                (Routes.genAuthServerContext tr.trLogger tr.trATCtx)
            path = "/p/" <> testPid.toText <> "/issue-fixes/" <> fid.toText
            authorization = ("Authorization", "Bearer " <> UUID.toASCIIBytes (getResponse tr.trSessAndHeader).persistentSession.id.getPersistentSessionId)
            fetch mode method url headers = WT.runSession (WT.srequest $ WT.SRequest (WT.setPath Wai.defaultRequest{Wai.requestMethod = method, Wai.requestHeaders = headers} $ encodeUtf8 url) "") (app mode)
            size = show $ LBS.length $ bytesFor Artifacts.Video :: Text
        player <- fetch "normal" H.methodGet path [authorization]
        H.statusCode player.simpleStatus `shouldBe` 200
        lookup "Cache-Control" player.simpleHeaders `shouldBe` Just "private, no-store"
        decodeUtf8 @Text player.simpleBody `shouldSatisfy` T.isInfixOf "<video controls"
        decodeUtf8 @Text player.simpleBody `shouldSatisfy` T.isInfixOf "<track"
        decodeUtf8 @Text player.simpleBody `shouldNotSatisfy` T.isInfixOf "autoplay"
        publication <- fetch "normal" H.methodGet (path <> "/publication.tar") [authorization]
        H.statusCode publication.simpleStatus `shouldBe` 200
        lookup "Cache-Control" publication.simpleHeaders `shouldBe` Just "private, no-store"
        lookup "Content-Type" publication.simpleHeaders `shouldBe` Just "application/x-tar"
        withSystemTempDirectory "publication-download" \directory -> do
          let archive = directory </> "publication.tar"
          writeFileLBS archive publication.simpleBody
          entries <- decodeUtf8 @Text <$> readProcessStdout_ (proc "tar" ["-tf", archive])
          sort (lines entries) `shouldBe` sort (["description.md", "evidence.json", "PUBLISHING.md"] <> Map.keys manifest.files)
          description <- decodeUtf8 @Text <$> readProcessStdout_ (proc "tar" ["-xOf", archive, "description.md"])
          description `shouldSatisfy` T.isInfixOf "![](explanation.mp4)"
          description `shouldSatisfy` T.isInfixOf "![Reproduction, inferred cause, and verified fix](cause.png)"
          description `shouldSatisfy` T.isInfixOf (Fixes.revisionText completed.head)
          description `shouldSatisfy` T.isInfixOf "Same retained regression fails on base"
          description `shouldNotSatisfy` T.isInfixOf "All checks passed"
          length (words description) `shouldSatisfy` (< 180)
          for_ (Map.keys manifest.files) \name ->
            readProcessStdout_ (proc "tar" ["-xOf", archive, toString name]) `shouldReturn` encodeUtf8 name
        publicationDenied <- fetch "unavailable" H.methodGet (path <> "/publication.tar") [authorization]
        H.statusCode publicationDenied.simpleStatus `shouldBe` 503
        for_ ([(H.methodHead, 200), (H.methodPost, 405)] :: [(H.Method, Int)]) \(method, status) -> do
          response <- fetch "normal" method (path <> "/publication.tar") [authorization]
          H.statusCode response.simpleStatus `shouldBe` status
          response.simpleBody `shouldBe` ""
        invalidOrigin <- runEff $ Artifacts.runAgentArtifactsReal readCtx $ Artifacts.packageFixMedia completed manifest "https://secret@invalid.example/"
        leftToMaybe invalidOrigin `shouldBe` Just Artifacts.PublicationUnavailable
        mismatched <- runEff $ Artifacts.runAgentArtifactsReal readCtx $ Artifacts.packageFixMedia completed manifest{Agent.head = completed.base} "https://monoscope.example"
        leftToMaybe mismatched `shouldBe` Just Artifacts.InvalidArtifact
        listed <- fetch "normal" H.methodGet ("/p/" <> testPid.toText <> "/repositories?tab=fixes") [authorization]
        H.statusCode listed.simpleStatus `shouldBe` 200
        decodeUtf8 @Text listed.simpleBody `shouldSatisfy` T.isInfixOf path
        for_ ([(H.methodGet, [], 200, bytesFor Artifacts.Video, Nothing), (H.methodGet, [("Range", "bytes=2-6")], 206, "plana", Just $ "bytes 2-6/" <> size), (H.methodGet, [("Range", "bytes=-3")], 206, "mp4", Just $ "bytes " <> show (LBS.length (bytesFor Artifacts.Video) - 3) <> "-" <> show (LBS.length (bytesFor Artifacts.Video) - 1) <> "/" <> size), (H.methodGet, [("Range", "bytes=999-")], 416, "", Just $ "bytes */" <> size), (H.methodHead, [], 200, "", Nothing), (H.methodPost, [], 405, "", Nothing)] :: [(H.Method, H.RequestHeaders, Int, LBS.ByteString, Maybe Text)]) \(method, headers, status, body, range) -> do
          response <- fetch "normal" method (path <> "/media/video") (authorization : headers)
          H.statusCode response.simpleStatus `shouldBe` status
          response.simpleBody `shouldBe` body
          lookup "Cache-Control" response.simpleHeaders `shouldBe` Just "private, no-store"
          fmap (decodeUtf8 @Text) (lookup "Content-Range" response.simpleHeaders) `shouldBe` range
          when (status `elem` [200, 206]) do
            lookup "Content-Type" response.simpleHeaders `shouldBe` Just "video/mp4"
            length (filter ((== "Content-Type") . fst) response.simpleHeaders) `shouldBe` 1
        for_ ([("diagram", "image/svg+xml"), ("captions", "text/vtt; charset=utf-8")] :: [(Text, ByteString)]) \(asset, mime) -> do
          response <- fetch "normal" H.methodGet (path <> "/media/" <> asset) [authorization]
          H.statusCode response.simpleStatus `shouldBe` 200
          lookup "Content-Type" response.simpleHeaders `shouldBe` Just mime
          lookup "X-Content-Type-Options" response.simpleHeaders `shouldBe` Just "nosniff"
        missing <- fetch "unavailable" H.methodGet (path <> "/media/video") [authorization]
        H.statusCode missing.simpleStatus `shouldBe` 503
        anonymous <- fetch "normal" H.methodGet (path <> "/media/video") []
        H.statusCode anonymous.simpleStatus `shouldSatisfy` (`elem` [302, 401, 404])
        wrongProject <- createTestProject tr "Wrong artifact project"
        for_ (["/media/video", "/publication.tar"] :: [Text]) \suffix -> do
          crossProject <- fetch "normal" H.methodGet ("/p/" <> wrongProject.toText <> "/issue-fixes/" <> fid.toText <> suffix) [authorization]
          H.statusCode crossProject.simpleStatus `shouldBe` 404
          revoked <- fetch "revoke" H.methodGet (path <> suffix) [authorization]
          H.statusCode revoked.simpleStatus `shouldBe` 404
          revoked.simpleBody `shouldBe` ""
          active True
        permission ProjectMembers.PView
        for_ (["/media/video", "/publication.tar"] :: [Text]) \suffix -> do
          denied <- fetch "normal" H.methodGet (path <> suffix) [authorization]
          H.statusCode denied.simpleStatus `shouldBe` 404
        permission ProjectMembers.PAdmin
        let corrupted = LBS.map (const 0) $ bytesFor Artifacts.Video
        overwritten <- Storage.runMinio readCtx.s3HttpManager config $ Minio.putObject readCtx.config.s3Bucket videoKey (CC.sourceLazy corrupted) (Just $ LBS.length corrupted) Minio.defaultPutObjectOptions
        isRight overwritten `shouldBe` True
        damaged <- fetch "normal" H.methodGet (path <> "/media/video") [authorization]
        H.statusCode damaged.simpleStatus `shouldBe` 503
        damaged.simpleBody `shouldNotBe` corrupted
        damagedPackage <- fetch "normal" H.methodGet (path <> "/publication.tar") [authorization]
        H.statusCode damagedPackage.simpleStatus `shouldBe` 503
        damagedPackage.simpleBody `shouldBe` ""
