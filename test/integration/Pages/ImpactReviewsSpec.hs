module Pages.ImpactReviewsSpec (spec) where

import BackgroundJobs qualified
import BackgroundJobs.Types qualified as Jobs
import CLI.CLIE2ESpec qualified as CLIBinary
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
import Data.Default (def)
import Data.Effectful.AgentArtifacts qualified as Artifacts
import Data.Effectful.AgentWorkspace qualified as Workspace
import Data.Effectful.CodeReview qualified as Agent
import Data.Effectful.CodeReview.Artifacts qualified as Native
import Data.Effectful.CodeReview.Checkout qualified as Checkout
import Data.Effectful.CodeReview.Explanation qualified as Media
import Data.Effectful.CodeReview.Outbox qualified as Outbox
import Data.Effectful.GitPublication qualified as Publication
import Data.Effectful.LLM qualified as LLM
import Data.Effectful.Wreq qualified as HTTP
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpose, localSeqUnlift, localSeqUnliftIO)
import Effectful.Error.Static (catchError)
import Effectful.Reader.Static qualified as Reader
import Lucid qualified
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
import Pages.Bots.BotTestHelpers (withHTTPResponses)
import Pages.CodeContext qualified as CodeContextPage
import Pages.GitSync qualified as GitSyncPage
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Git qualified as Git
import Pkg.GitCommand qualified as HostGit
import Pkg.ImpactReview qualified as Impact
import Pkg.Minio qualified as Storage
import Pkg.Parser.Stats qualified as KQL
import Pkg.TestUtils
import Relude
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
import System.Info qualified as Info
import System.Posix.Files qualified as Posix
import System.Posix.Signals qualified as Signal
import System.Process.Typed (createPipe, getPid, getStdin, getStdout, nullStream, proc, readProcess, readProcessStdout_, setEnv, setStderr, setStdin, setStdout, setWorkingDir, stopProcess, waitExitCode, withProcessTerm)
import System.Types (ATBackgroundCtx, ATBaseCtx)
import Test.Hspec
import UnliftIO.Async (cancel, concurrently, waitCatch, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (finally, throwIO, tryAny)
import UnliftIO.MVar qualified as MVar
import UnliftIO.Timeout qualified as Timeout
import Web.HttpApiData (parseUrlPiece)
import Web.Routes qualified as Routes
import "cryptonite" Crypto.Hash (Digest, hash)
import "cryptonite" Crypto.Hash.Algorithms (SHA256, SHA512)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC


revision :: Text
revision = T.replicate 40 "a"


setup :: TestResources -> IO ()
setup tr = setupProject tr testPid


setupProject :: TestResources -> Projects.ProjectId -> IO ()
setupProject tr pid = do
  let cfg = tr.trATCtx.config
  credential <- maybe (fail "credential missing") pure =<< runQueryEffect tr (GitSync.upsertGitHubCredential (encodeUtf8 cfg.apiKeyEncryptionSecretKey) pid Git.GitHub Nothing "impact-org" (Just 987654) Nothing)
  runQueryEffect tr $ CodeContext.insertCodeMapping pid credential.id (Git.RepoRef "impact-org" "checkout" "main") (Just "test-service") "" ""
  -- Handlers render the repository list; a warm cache keeps them off the GitHub token endpoint.
  Cache.insert tr.trATCtx.repoListCache credential.id [Git.GitRepo "impact-org/checkout" "checkout" False "main"]


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


data SdkReply = SdkText Text | SdkTool Text AE.Value


data SdkTokens = SdkTokens {promptTokens :: Word32, cachedTokens :: Word32}


data SdkResponse = SdkResponse SdkReply SdkTokens | SdkUnavailable Text


withSdkModel :: FilePath -> Text -> ([AE.Value] -> IO SdkReply) -> (FilePath -> IO [AE.Value] -> IO a) -> IO a
withSdkModel directory provider next = withSdkProvider directory provider (\_ results -> (`SdkResponse` SdkTokens 7 0) <$> next results)


withSdkProvider :: FilePath -> Text -> (AE.Value -> [AE.Value] -> IO SdkResponse) -> (FilePath -> IO [AE.Value] -> IO a) -> IO a
withSdkProvider directory provider next action = do
  requests <- newIORef ([] :: [AE.Value])
  let field name = AE.withObject "SDK model request" (AE..: name)
      app incoming respond = do
        body <- Wai.strictRequestBody incoming >>= either fail pure . AE.eitherDecode @AE.Value
        modifyIORef' requests (<> [body])
        messages <- either fail pure $ AET.parseEither (field "messages") body :: IO [AE.Value]
        let results = filter (\value -> AET.parseEither (field "role") value == Right ("tool" :: Text)) messages
        result <- next body results
        case result of
          SdkUnavailable message -> respond $ Wai.responseLBS H.status400 [("Content-Type", "application/json")] $ AE.encode $ AE.object ["error" AE..= AE.object ["message" AE..= message]]
          SdkResponse reply tokens -> do
            let (delta, reason) = case reply of
                  SdkTool name arguments -> (AE.object ["role" AE..= ("assistant" :: Text), "tool_calls" AE..= V.singleton (AE.object ["index" AE..= (0 :: Int), "id" AE..= ("tool-" <> show @Text (length results)), "type" AE..= ("function" :: Text), "function" AE..= AE.object ["name" AE..= name, "arguments" AE..= decodeUtf8 @Text (AE.encode arguments)]])], "tool_calls")
                  SdkText content -> (AE.object ["role" AE..= ("assistant" :: Text), "content" AE..= content], "stop")
                response = AE.object ["id" AE..= ("sdk-fixture" :: Text), "object" AE..= ("chat.completion.chunk" :: Text), "created" AE..= (0 :: Int), "model" AE..= ("fixture" :: Text), "usage" AE..= AE.object ["prompt_tokens" AE..= tokens.promptTokens, "completion_tokens" AE..= (3 :: Int), "total_tokens" AE..= (tokens.promptTokens + 3), "prompt_tokens_details" AE..= AE.object ["cached_tokens" AE..= tokens.cachedTokens]], "choices" AE..= V.singleton (AE.object ["index" AE..= (0 :: Int), "delta" AE..= delta, "finish_reason" AE..= (reason :: Text)])]
            respond $ Wai.responseLBS H.status200 [("Content-Type", "text/event-stream")] $ "data: " <> AE.encode response <> "\n\ndata: [DONE]\n\n"
  Warp.testWithApplication (pure app) \port -> do
    let models = directory </> "models.json"
    writeFileLBS models $ AE.encode $ AE.object ["providers" AE..= AE.object [fromString (toString provider) AE..= AE.object ["baseUrl" AE..= ("http://127.0.0.1:" <> show @Text port <> "/v1"), "api" AE..= ("openai-completions" :: Text), "apiKey" AE..= ("fixture" :: Text), "models" AE..= V.singleton (AE.object ["id" AE..= ("fixture" :: Text), "contextWindow" AE..= (64000 :: Int), "maxTokens" AE..= (4096 :: Int), "cost" AE..= AE.object ["input" AE..= (2 :: Int), "output" AE..= (10 :: Int), "cacheRead" AE..= (1 :: Int), "cacheWrite" AE..= (0 :: Int)]])]]]
    action models $ readIORef requests


deliver :: TestResources -> AE.Value -> IO AE.Value
deliver tr value = do
  let body = toStrict $ AE.encode value
      signature = "sha256=" <> decodeUtf8 (B16.encode $ BA.convert (HMAC.hmac ("impact-secret" :: ByteString) body :: HMAC.HMAC SHA256))
      configured = tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{githubAppWebhookSecret = "impact-secret"}}}
  snd <$> runAsBaseRecordingHTTP configured (GitSyncPage.gitWebhookPostH Git.GitHub $ Git.WebhookReq (Just "pull_request") (Just signature) Nothing Nothing Nothing body)


spec :: Spec
spec = around withTestResources $ describe "Production impact reviews" do
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
      for_ ["core.hookspath=/dev/null", "core.fsmonitor=false", "credential.helper=", "http.followredirects=false"] \setting -> configured `shouldSatisfy` T.isInfixOf setting

  it "closes the actual SDK sandbox when its parent pipe ends during a command" \_tr -> withSystemTempDirectory "native-sdk-pipe-loss" \directory -> do
    available <- doesFileExist "impact-review-worker/.guest/manifest.json"
    unless available $ pendingWith "Build the self-hosted guest before running SDK integration tests"
    image <- canonicalizePath "impact-review-worker/.guest"
    let sessions = map (.id) <$> sandboxSessions
        field key = AE.withObject "SDK sandbox reply" (AE..: key)
        parse parser = either fail pure . AET.parseEither parser
    withProcessTerm (setStdin createPipe $ setStdout createPipe $ setStderr nullStream $ proc "test/fixtures/native-sandbox.sh" []) \child -> do
      let send tag contents = LBS.hPut (getStdin child) (AE.encode (AE.object ["tag" AE..= (tag :: Text), "contents" AE..= contents]) <> "\n") >> hFlush (getStdin child)
          receive = Timeout.timeout (15 * 1000000) (BSC.hGetLine (getStdout child) >>= either fail pure . AE.eitherDecodeStrict @AE.Value) >>= maybe (fail "SDK sandbox did not reply") pure
      send "Create" $ AE.object ["imagePath" AE..= image, "cpus" AE..= (2 :: Int), "memoryMiB" AE..= (1024 :: Int), "isolation" AE..= ("offline_cow" :: Text), "mounts" AE..= AE.object []]
      receive `shouldReturn` AE.object ["Right" AE..= ([] :: [AE.Value])]
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
    it ("retains interrupted verifier evidence after SDK " <> if abrupt then "caller loss" else "cancellation") \_tr -> CLIBinary.withCliBin \cli -> withSystemTempDirectory "native-sdk-cancellation" \temporary -> do
      available <- doesFileExist "impact-review-worker/.guest/manifest.json"
      unless available $ pendingWith "Build the self-hosted guest before running SDK integration tests"
      directory <- canonicalizePath temporary
      image <- canonicalizePath "impact-review-worker/.guest"
      environment <- filter (\(key, _) -> key `elem` ["PATH", "HOME", "TMPDIR"]) <$> getEnvironment
      let repository = directory </> "repository"
          artifacts = directory </> "checks"
          input = directory </> "input.json"
          uid n = UUIDId $ UUID.fromWords 52 0x4000 0x80000000 n :: UUIDId "fixture"
          field name = AE.withObject "Interrupted verifier evidence" (AE..: name)
          parse parser = either fail pure . AET.parseEither parser
          git args = T.strip . decodeUtf8 @Text <$> readProcessStdout_ (setWorkingDir repository $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "commit.gpgSign=false"] <> args)
          readReport path = readFileBS path >>= either fail pure . AE.eitherDecodeStrict @AE.Value
          untilReady action = action >>= maybe (threadDelay 50000 >> untilReady action) pure
      traverse_ (createDirectoryIfMissing True) [repository, artifacts]
      void $ git ["init", "--quiet"]
      void $ git ["config", "user.name", "Verifier fixture"]
      void $ git ["config", "user.email", "fixture@example.invalid"]
      writeFileBS (repository </> "runtime.txt") "actual source\n"
      void $ git ["add", "."]
      void $ git ["commit", "--quiet", "-m", "pinned source"]
      base <- git ["rev-parse", "HEAD"]
      writeFileLBS input
        $ AE.encode
        $ AE.object
          [ "request" AE..= AE.object ["projectId" AE..= testPid, "runId" AE..= uid 1, "attemptId" AE..= uid 2, "sessionId" AE..= uid 3, "base" AE..= base, "artifact" AE..= AE.object ["script" AE..= ("echo verifier-started; sleep 60" :: Text), "failureMessage" AE..= ("unused" :: Text)], "revisions" AE..= ([["base", base]] :: [[Text]])]
          , "options" AE..= AE.object ["artifactDir" AE..= artifacts, "checkout" AE..= repository]
          ]
      withProcessTerm (setEnv environment $ setStdin createPipe $ setStdout createPipe $ setStderr nullStream $ proc "node" ["test/fixtures/native-regression-caller.mjs", input, cli, image]) \child -> do
        observed <- newIORef Nothing
        let started = do
              names <- listDirectory artifacts
              let directories = filter (isJust . UUID.fromString) names
              matches <- forM directories \name -> do
                let path = artifacts </> name
                ready <- doesFileExist $ path </> "base.log"
                if not ready
                  then pure Nothing
                  else do
                    bytes <- readFileBS $ path </> "base.log"
                    if not ("verifier-started" `BS.isInfixOf` bytes)
                      then pure Nothing
                      else do
                        report <- readReport $ path </> "report.json"
                        rows <- parse (field "results") report :: IO [AE.Value]
                        vmIds <- traverse (parse $ field "vmId") rows
                        sessions <- filter (\session -> session.id `elem` vmIds) <$> sandboxSessions
                        case sessions of
                          [session] -> writeIORef observed (Just session) $> Just path
                          _ -> pure Nothing
              pure $ viaNonEmpty head $ catMaybes matches
            vanished session = untilReady do
              sessions <- sandboxSessions
              pure $ if any ((== session.id) . (.id)) sessions then Nothing else Just ()
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
            readReport (path </> "report.json") >>= parse (field "status") >>= (`shouldNotBe` ("completed" :: Text))
            readFileBS (path </> "base.log") >>= (`shouldSatisfy` BS.isInfixOf "verifier-started")
          )
          `finally` cleanup

  it "admits frozen regressions through the real SDK with native checkout and durable metering" \_tr -> withSystemTempDirectory "native-sdk-admission" \temporary -> do
    available <- doesFileExist "impact-review-worker/.guest/manifest.json"
    unless available $ pendingWith "Build the self-hosted guest before running SDK integration tests"
    directory <- canonicalizePath temporary
    image <- canonicalizePath "impact-review-worker/.guest"
    let repository = directory </> "repository"
        store = Agent.ArtifactStore (directory </> "usage") (directory </> "cache")
        diagnostic = directory </> "worker.log"
        uid n = UUIDId $ UUID.fromWords 51 0x4000 0x80000000 n
        field name = AE.withObject "SDK admission fixture" (AE..: name)
        parse parser = either fail pure . AET.parseEither parser
        git args = decodeUtf8 @Text <$> readProcessStdout_ (setWorkingDir repository $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "commit.gpgSign=false"] <> args)
        regression = "set -eu\n. ./runtime.sh\n[ \"$(retry_token original)\" = original ] || { echo 'retry token lost'; exit 1; }\n"
    traverse_ (createDirectoryIfMissing True) [repository, store.usageDir, store.cacheDir]
    void $ git ["init", "--quiet"]
    void $ git ["config", "user.name", "Admission fixture"]
    void $ git ["config", "user.email", "fixture@example.invalid"]
    writeFileBS (repository </> "runtime.sh") "retry_token() { printf 'lost\\n'; }\n"
    writeFileBS (repository </> "existing-regression.sh") "# regression coverage placeholder\n"
    void $ git ["add", "."]
    void $ git ["commit", "--quiet", "-m", "broken runtime"]
    base <- T.strip <$> git ["rev-parse", "HEAD"]
    writeFileBS (repository </> "regression.sh") regression
    void $ git ["add", "regression.sh"]
    testPatch <- git ["diff", "--cached", "--binary", "--no-ext-diff", "--no-textconv"]
    void $ git ["reset", "--hard", toString base]
    imageId <- readFileBS (image </> "manifest.json") >>= either fail pure . AE.eitherDecodeStrict >>= parse (field "buildId")
    verdict <- newIORef ("Admitted" :: Text)
    investigated <- newIORef ([] :: [Text])
    let next results = do
          for_ results (parse (field "content") >=> \text -> modifyIORef' investigated (<> [text]))
          decision <- readIORef verdict
          pure $ case length results of
            0 -> SdkTool "bash" (AE.object ["command" AE..= ("set -eu; id -u; test ! -w /run/monoscope-admission.patch; test ! -w /run/monoscope-admission.json; git diff --cached --name-only; cat runtime.sh /run/monoscope-admission.json; printf forbidden > runtime.sh" :: Text)])
            1 -> SdkTool "submit_admission" (AE.object ["tag" AE..= decision, "contents" AE..= ("The regression calls the actual retry_token runtime and checks the issue's caller token." :: Text)])
            _ -> SdkText "Fixture finished."
    withSdkModel directory "admission-fixture" next \models requests -> do
      let request = Agent.Request{projectId = testPid, runId = uid 1, attemptId = uid 2, repository = toText repository, base, head = base, gitToken = "", prompt = "Issue: retry_token must preserve the original retry token.", provider = "admission-fixture", model = "fixture", thinkingLevel = Agent.Off}
          admission :: Agent.AdmissionRequest
          admission = Agent.AdmissionRequest (Agent.FixRequest request (uid 3) imageId) (Agent.TestArtifact "bash regression.sh" testPatch "retry token lost")
          checkDecision expected = either (\failure -> readFileBS diagnostic >>= expectationFailure . (toString failure <>) . toString . decodeUtf8 @Text) (\outcome -> parse (field "tag") (AE.toJSON outcome) >>= (`shouldBe` expected))
          run n acceptStarts (input :: Agent.AdmissionRequest) = do
            calls <- newIORef ([] :: [Agent.ModelCall])
            usage <- newIORef ([] :: [Agent.ModelUsage])
            allocations <- newIORef ([] :: [Agent.AllocationReceipt])
            let configured = input{Agent.fix = input.fix{Agent.request = input.fix.request{Agent.runId = uid n}}} :: Agent.AdmissionRequest
                journal = store.usageDir </> "admissions" </> toString configured.fix.request.runId.toText
                retained identifier expected = do
                  saved <- readFileBS (journal </> toString identifier <> ".json") >>= either fail pure . AE.eitherDecodeStrict @AE.Value
                  saved `shouldBe` expected
            result <-
              runEff
                $ Agent.runCodeReviewReal (Just store) (Just "test/fixtures/native-sdk-worker.py") [models, store.usageDir, store.cacheDir, image, diagnostic]
                $ Agent.admitRegression
                  configured
                  (const $ liftIO $ fail "Admission unexpectedly requested knowledge")
                  ( \receipt -> liftIO do
                      retained receipt.id.toText $ AE.object ["tag" AE..= ("Call" :: Text), "contents" AE..= receipt]
                      modifyIORef' calls (<> [receipt])
                      unless acceptStarts $ fail "Call start rejected"
                  )
                  (\receipt -> liftIO $ retained receipt.id.toText (AE.toJSON receipt) >> modifyIORef' usage (<> [receipt]))
                  (\receipt -> liftIO $ retained (Agent.allocationId receipt).toText (AE.object ["tag" AE..= ("Allocation" :: Text), "contents" AE..= receipt]) >> modifyIORef' allocations (<> [receipt]))
            (,,,) result <$> readIORef calls <*> readIORef usage <*> readIORef allocations
      for_ ([(1, "Admitted"), (4, "Rejected")] :: [(Word32, Text)]) \(n, decision) -> do
        writeIORef verdict decision
        priorCalls <- length <$> requests
        (result, starts, responses, allocated) <- run n True admission
        checkDecision decision result
        actual <- subtract priorCalls . length <$> requests
        length starts `shouldBe` actual
        length responses `shouldBe` actual
        responses `shouldSatisfy` all (\receipt -> receipt.attemptId == request.attemptId && maybe False (> 0) receipt.costNanousd && receipt.callId `elem` map (Just . (.id)) starts)
        length allocated `shouldBe` 1
        for_ allocated \receipt -> do
          Agent.allocationAttempt receipt `shouldBe` request.attemptId
          parse (field "outcome") (AE.toJSON receipt) `shouldReturn` ("released" :: Text)
          parse (field "durationMs") (AE.toJSON receipt) >>= (\duration -> (duration :: Word64) `shouldSatisfy` (> 0))
      priorCalls <- length <$> requests
      (denied, starts, responses, _) <- run 5 False admission
      isLeft denied `shouldBe` True
      length starts `shouldBe` 1
      length responses `shouldBe` 0
      length <$> requests `shouldReturn` priorCalls
      writeFileBS (repository </> "runtime.sh") "retry_token() { printf '%s\\n' \"$1\"; }\n"
      writeFileBS (repository </> "regression.sh") regression
      void $ git ["add", "."]
      productionPatch <- git ["diff", "--cached", "--binary", "--no-ext-diff", "--no-textconv"]
      void $ git ["reset", "--hard", toString base]
      (production, _, _, _) <- run 6 True admission{Agent.artifact = admission.artifact{Agent.testPatch = productionPatch}}
      checkDecision "Rejected" production
      writeIORef verdict "Admitted"
      writeFileBS (repository </> "existing-regression.sh") regression
      void $ git ["add", "."]
      existingPatch <- git ["diff", "--cached", "--binary", "--no-ext-diff", "--no-textconv"]
      void $ git ["reset", "--hard", toString base]
      (existing, _, _, _) <- run 7 True admission{Agent.artifact = admission.artifact{Agent.script = "bash existing-regression.sh", Agent.testPatch = existingPatch}}
      checkDecision "Admitted" existing
      inspected <- T.intercalate "\n" <$> readIORef investigated
      for_ @[] ["1000", "regression.sh", "existing-regression.sh", "Permission denied"] \fragment -> inspected `shouldSatisfy` T.isInfixOf fragment
      observed <- requests
      let contents = decodeUtf8 @Text $ AE.encode observed
      contents `shouldSatisfy` T.isInfixOf "Permission denied"
      contents `shouldSatisfy` T.isInfixOf "retry_token must preserve"
      beforeInvalid <- length <$> requests
      (wrongImage, _, _, _) <- run 8 True admission{Agent.fix = admission.fix{Agent.imageBuildId = uid 99}}
      isLeft wrongImage `shouldBe` True
      (malformed, _, _, _) <- run 9 True admission{Agent.artifact = admission.artifact{Agent.testPatch = "not a patch"}}
      isLeft malformed `shouldBe` True
      for_ (zip [10 ..] [admission.artifact{Agent.script = " "}, admission.artifact{Agent.failureMessage = T.replicate 201 "x"}, admission.artifact{Agent.testPatch = ""}]) \(n, artifact) -> do
        (invalid, _, _, _) <- run n True admission{Agent.artifact = artifact}
        isLeft invalid `shouldBe` True
      length <$> requests `shouldReturn` beforeInvalid

  it "fixes through the real SDK with frozen reproduction, native verification and independent review" \_tr -> CLIBinary.withCliBin \cli -> withSystemTempDirectory "native-sdk-fix" \temporary -> do
    available <- doesFileExist "impact-review-worker/.guest/manifest.json"
    unless available $ pendingWith "Build the self-hosted guest before running SDK integration tests"
    directory <- canonicalizePath temporary
    image <- canonicalizePath "impact-review-worker/.guest"
    bridge <- canonicalizePath "test/fixtures/native-sandbox.sh"
    let repository = directory </> "repository"
        store = Agent.ArtifactStore (directory </> "usage") (directory </> "cache")
        scriptFile = directory </> "prepare.sh"
        requestFile = directory </> "request.json"
        diagnostic = directory </> "worker.log"
        uid n = UUIDId $ UUID.fromWords 55 0x4000 0x80000000 n
        field key = AE.withObject "SDK fix evidence" (AE..: key)
        parse parser = either fail pure . AET.parseEither parser
        git args = decodeUtf8 @Text <$> readProcessStdout_ (setWorkingDir repository $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "commit.gpgSign=false"] <> args)
        testSource = "set -eu\n. ./runtime.sh\nif [ \"$(retry_token original)\" != original ]; then echo 'retry token was lost' >&2; exit 1; fi\necho regression-passed\n" :: ByteString
        broken = "retry_token() { printf 'dropped\\n'; }" :: Text
        fixed = "retry_token() { printf '%s\\n' \"$1\"; }" :: Text
        rootCause = "retry_token ignores its argument and drops the retry token." :: Text
        scratchPath = "~/notes/quote's $(touch path-expanded).txt" :: Text
        scratchContent = "\xfeffliteral $USER $(touch content-expanded)\r\n" :: Text
        patchPath = "/tmp/admitted regression's $(touch patch-path-expanded).patch" :: Text
        write path content = SdkTool "write" $ AE.object ["path" AE..= (path :: Text), "content" AE..= (content :: Text)]
        edit path old new = SdkTool "edit" $ AE.object ["path" AE..= (path :: Text), "edits" AE..= V.singleton (AE.object ["oldText" AE..= (old :: Text), "newText" AE..= (new :: Text)])]
        bash command = SdkTool "bash" $ AE.object ["command" AE..= (command :: Text)]
        propose = SdkTool "propose_fix" $ AE.object ["rootCause" AE..= rootCause]
        verify = SdkTool "verify_fix" $ AE.object []
        submit = SdkTool "submit_fix" $ AE.object ["summary" AE..= ("Preserve the retry token." :: Text)]
        toolEnds = filter (\value -> AET.parseEither (field "event") value == Right ("tool_execution_end" :: Text)) . mapMaybe (AE.decodeStrict @AE.Value) . BSC.lines <$> readFileBS diagnostic
    traverse_ (createDirectoryIfMissing True) [repository, store.usageDir, store.cacheDir]
    void $ git ["init", "--quiet"]
    void $ git ["config", "user.name", "Fix fixture"]
    void $ git ["config", "user.email", "fixture@example.invalid"]
    void $ git ["config", "core.autocrlf", "false"]
    writeFileBS (repository </> "runtime.sh") $ encodeUtf8 $ broken <> "\n"
    writeFileBS (repository </> "template.txt") $ encodeUtf8 $ T.replace "literal" "LITERAL" scratchContent
    void $ git ["add", "."]
    void $ git ["commit", "--quiet", "-m", "broken base"]
    base <- T.strip <$> git ["rev-parse", "HEAD"]
    writeFileBS (repository </> "regression.sh") testSource
    void $ git ["add", "regression.sh"]
    testPatch <- git ["diff", "--cached", "--binary", "--no-ext-diff", "--no-textconv"]
    void $ git ["reset", "--hard", toString base]
    writeFileBS scriptFile "test -f runtime.sh"
    writeFileLBS requestFile $ AE.encode $ AE.object ["projectId" AE..= testPid, "repository" AE..= repository, "base" AE..= base]
    prepared <- runEff (Workspace.runWorkspaceReal $ Workspace.runWorkspace (Workspace.Options store.cacheDir (toText image) bridge scriptFile Nothing) (Workspace.Prepare repository) requestFile) >>= either (fail . toString) (maybe (fail "Fix workspace missing") (pure . AE.toJSON))
    imageId <- parse (field "manifest" >=> field "identity" >=> field "imageBuildId") prepared
    fingerprint <- parse (field "manifest" >=> field "sha256") prepared :: IO Text
    let reproduction source = SdkTool "reproduce_issue" $ AE.object $ ["script" AE..= ("bash regression.sh" :: Text), "failureMessage" AE..= ("retry token was lost" :: Text)] <> source
        frozen = Agent.TestArtifact "bash regression.sh" testPatch "retry token was lost"
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
               , reproduction ["testPatchFile" AE..= patchPath]
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
    plan <- newIORef steps
    replies <- newIORef ([] :: [AE.Value])
    phases <- newIORef ([] :: [Agent.PhaseReceipt])
    checks <- newIORef ([] :: [Agent.CheckReceipt])
    calls <- newIORef ([] :: [Agent.ModelCall])
    usage <- newIORef ([] :: [Agent.ModelUsage])
    allocations <- newIORef ([] :: [Agent.AllocationReceipt])
    validationFailure <- newIORef Nothing
    let record ref receipt = modifyIORef' ref (<> [receipt])
        next results = do
          writeIORef replies results
          choices <- readIORef plan
          pure $ fromMaybe (SdkText "Fix attempt finished.") $ viaNonEmpty head $ drop (length results) choices
    withSdkModel directory "fix-fixture" next \models requests -> do
      let run action = runEff $ Agent.runCodeReviewReal (Just store) (Just "test/fixtures/native-sdk-worker.py") [models, store.usageDir, store.cacheDir, image, diagnostic, "--prepare", scriptFile, "--cli", cli] action
          knowledge = const $ pure $ Left "Unexpected knowledge request"
          capture ref receipt = liftIO $ record ref receipt
          inspect result = runExceptT do
            evidence <- liftIO $ readIORef checks
            (baseline, candidate) <- case filter ((== result.checkId) . (.checkId)) evidence of
              [baseline, candidate] -> pure (baseline, candidate)
              _ -> throwE "Verified comparison is missing"
            let artifacts = Agent.ArtifactRequest (toText repository) result baseline candidate
            liftIO do
              let admissions = store.usageDir </> "fixes" </> toString result.runId.toText </> "checks" </> "admissions"
              entries <- listDirectory admissions
              length entries `shouldBe` 1
              for_ entries \entry -> do
                retained <- either fail pure . AE.eitherDecodeStrict =<< readFileBS (admissions </> entry </> "admission.json")
                retained `shouldBe` AE.object ["projectId" AE..= result.projectId, "runId" AE..= result.runId, "issueId" AE..= result.issueId, "attemptId" AE..= result.attemptId, "base" AE..= result.base, "regressionSha256" AE..= result.regressionSha256, "rationale" AE..= ("Relevant fixture regression admitted" :: Text)]
            ended <- liftIO toolEnds
            liftIO do
              length ended `shouldBe` 23
              failed <- traverse (parse $ field "failed") ended
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
                  ExceptT $ Agent.reviewVerifiedFix (Agent.FixReviewRequest fixRequest{Agent.request = request{Agent.attemptId = uid n}} artifacts) verified knowledge (capture calls) (capture usage) (capture allocations)
            changed <- peer 4 $ Agent.ReviewChanges $ one "runtime.sh:1 needs another caller check"
            liftIO $ case changed of Agent.ChangesNeeded values -> values `shouldBe` one "runtime.sh:1 needs another caller check"; _ -> expectationFailure "Peer changes were not retained"
            blocked <- peer 5 $ Agent.ReviewBlocked $ one "Required checks are unavailable"
            liftIO $ case blocked of Agent.ReviewUnavailable values -> values `shouldBe` one "Required checks are unavailable"; _ -> expectationFailure "Peer missing verification was not retained"
            approved <- peer 6 $ Agent.ReviewApproved "The retained test exercises the runtime and the fix preserves its argument."
            case approved of Agent.Reviewed reviewed -> pure reviewed; _ -> throwE "Peer approval was not established"
          validate result = do
            checked <- tryAny $ inspect result
            case checked of
              Left failure -> liftIO (writeIORef validationFailure $ Just failure) $> Left "SDK fixture validation failed"
              Right value -> pure value
          admit artifact = pure $ if artifact == frozen then Right "Relevant fixture regression admitted" else Left "The test patch must only contain the actual regression"
      outcome <- run $ Agent.fixRepository fixRequest (const $ pure $ Left "Unexpected query") knowledge admit (capture calls) (capture usage) (capture allocations) (capture checks) (\receipt -> capture phases receipt $> Right ()) validate
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
      void $ git ["bundle", "verify", bundle]
      T.strip <$> git ["bundle", "list-heads", bundle] `shouldReturn` Fixes.revisionText result.head <> " " <> toText ref
      void $ git ["fetch", "--quiet", bundle, ref]
      T.strip <$> git ["rev-parse", "FETCH_HEAD"] `shouldReturn` Fixes.revisionText result.head
      results <- readIORef replies
      length results `shouldBe` 24
      let reply identifier = maybe (fail "Missing fix tool reply") (parse $ field "content") $ find (\value -> AET.parseEither (field "tool_call_id") value == Right (identifier :: Text)) results
      reply "tool-6" >>= (`shouldSatisfy` T.isInfixOf "256 KiB limit")
      reply "tool-13" >>= (`shouldSatisfy` T.isInfixOf "/home/review-agent/notes/")
      reply "tool-19" >>= (`shouldSatisfy` T.isInfixOf "Workspace changed; propose and verify a new candidate")
      proposal <- parse AE.parseJSON $ AE.object ["patch" AE..= decodeUtf8 @Text patch, "rootCause" AE..= rootCause]
      captured <- Native.captureCandidate checksDir fixRequest repository proposal >>= either (fail . toString) pure
      identifier <- parse (field "candidateId") $ AE.toJSON captured
      Native.candidateMatchesPatch captured identifier (decodeUtf8 patch) `shouldBe` True
      Native.candidateMatchesPatch captured (uid 99) (decodeUtf8 patch) `shouldBe` False
      decodeUtf8 @Text (AE.encode results) `shouldNotSatisfy` T.isInfixOf "/file-tools/"
      contexts <- requests
      let names = AE.withObject "Model tools" (AE..: "tools") >=> traverse (AE.withObject "Model tool" (AE..: "function") >=> AE.withObject "Tool function" (AE..: "name"))
      toolsets <- traverse (parse names) contexts :: IO [[Text]]
      let peers = filter (elem "submit_fix_review") toolsets
      length peers `shouldBe` 9
      for_ peers \tools -> do
        tools `shouldSatisfy` all (`notElem` ["edit", "write", "remember_knowledge", "recall_knowledge", "reproduce_issue", "verify_fix", "run_regression"])
        tools `shouldSatisfy` (\availableTools -> all (`elem` availableTools) ["list_project_memory", "read_project_memory", "write_project_memory"])
      recordedCalls <- readIORef calls
      recordedUsage <- readIORef usage
      let callAttempts = Map.fromList [(receipt.id, receipt.attemptId) | receipt <- recordedCalls]
      length recordedCalls `shouldBe` length contexts
      length recordedUsage `shouldBe` length contexts
      recordedUsage `shouldSatisfy` all (\receipt -> (receipt.callId >>= (`Map.lookup` callAttempts)) == Just receipt.attemptId && receipt.succeeded && maybe False (> 0) receipt.costNanousd)
      recordedAllocations <- readIORef allocations
      length recordedAllocations `shouldBe` 4
      map Agent.allocationAttempt recordedAllocations `shouldMatchList` [request.attemptId, uid 4, uid 5, uid 6]
      for_ recordedAllocations \receipt -> do
        parse (field "outcome") (AE.toJSON receipt) `shouldReturn` ("released" :: Text)
        parse (field "durationMs") (AE.toJSON receipt) >>= (\duration -> (duration :: Word64) `shouldSatisfy` (> 0))
      ends <- toolEnds
      case viaNonEmpty last ends of
        Just value -> parse (field "failed") value `shouldReturn` False
        Nothing -> expectationFailure "Final SDK tool event missing"
      writeIORef plan [reproduction ["testPatch" AE..= testPatch]]
      beforeDenied <- length <$> requests
      denied <- run $ Agent.fixRepository fixRequest{Agent.request = request{Agent.runId = uid 10, Agent.attemptId = uid 11}} (const $ pure $ Left "Unexpected query") knowledge admit (capture calls) (capture usage) (capture allocations) (capture checks) (const $ pure $ Left "Phase persistence rejected") (const $ pure $ Left "Unexpected validation")
      isLeft denied `shouldBe` True
      length <$> requests `shouldReturn` beforeDenied
      for_ (zip [30, 32] [("true", Agent.Passed), ("printf 'retry token was lost\\n'; exit 127", Agent.Blocked)]) \(number, (script, expectedStatus)) -> do
        offset <- length <$> toolEnds
        writeIORef plan [SdkTool "reproduce_issue" $ AE.object ["script" AE..= (script :: Text), "testPatch" AE..= testPatch, "failureMessage" AE..= frozen.failureMessage], write "runtime.sh" "forbidden"]
        let configured = fixRequest{Agent.request = request{Agent.runId = uid number, Agent.attemptId = uid (number + 1)}}
        failed <- run $ Agent.fixRepository configured (const $ pure $ Left "Unexpected query") knowledge (const $ pure $ Right "Deliberately admit this artifact to exercise the native reproduction guard") (capture calls) (capture usage) (capture allocations) (capture checks) (const $ pure $ Right ()) (const $ pure $ Left "Unreproduced source reached validation")
        isLeft failed `shouldBe` True
        ended <- drop offset <$> toolEnds
        traverse (parse $ field "failed") ended `shouldReturn` [True, True]
        map (.status) . filter ((== configured.request.attemptId) . (.attemptId)) <$> readIORef checks `shouldReturn` [expectedStatus]
      ready <- MVar.newEmptyMVar
      blockedAdmission <- MVar.newEmptyMVar :: IO (MVar.MVar ())
      beforeCancel <- length <$> readIORef checks
      let cancelled = run $ Agent.fixRepository fixRequest{Agent.request = request{Agent.runId = uid 20, Agent.attemptId = uid 21}} (const $ pure $ Left "Unexpected query") knowledge (\_ -> liftIO (MVar.putMVar ready () >> MVar.takeMVar blockedAdmission) $> Right "Admission finished after cancellation") (capture calls) (capture usage) (capture allocations) (capture checks) (const $ pure $ Right ()) (const $ pure $ Left "Unexpected validation")
      withAsync cancelled \worker -> do
        Timeout.timeout (30 * 1000000) (MVar.takeMVar ready) `shouldReturn` Just ()
        cancel worker
        isLeft <$> waitCatch worker `shouldReturn` True
      length <$> readIORef checks `shouldReturn` beforeCancel

  it "calibrates pinned source through both CLI modes with repaired citations and durable evidence" \_tr -> CLIBinary.withCliBin \cli -> withSystemTempDirectory "native-sdk-calibration" \temporary -> do
    configuredProfile <- lookupEnv "IMPACT_REVIEW_TEST_PROFILE"
    let profile = fromMaybe "impact-review-worker/.guest" configuredProfile
    available <- doesFileExist $ profile </> "manifest.json"
    unless (available || isJust configuredProfile) $ pendingWith "Build the self-hosted guest before running calibration integration tests"
    directory <- canonicalizePath temporary
    bridge <- canonicalizePath "test/fixtures/native-sandbox.sh"
    environment <- filter (\(key, _) -> key `elem` ["PATH", "HOME", "TMPDIR"]) <$> getEnvironment
    let repository = directory </> "repository"
        store = Agent.ArtifactStore (directory </> "usage") (directory </> "cache")
        outputDir = directory </> "results"
        artifacts = directory </> "checks"
        taskFile = directory </> "pr.json"
        checkoutFile = directory </> "checkout.json"
        requestFile = directory </> "request.json"
        scriptFile = directory </> "prepare.sh"
        uid n = UUIDId $ UUID.fromWords 54 0x4000 0x80000000 n
        field key = AE.withObject "Calibration CLI evidence" (AE..: key)
        parse parser = either fail pure . AET.parseEither parser
        readJSON path = readFileBS path >>= either fail pure . AE.eitherDecodeStrict @AE.Value
        git args = T.strip . decodeUtf8 @Text <$> readProcessStdout_ (setWorkingDir repository $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "commit.gpgSign=false"] <> args)
        finding file end = ["file" AE..= (file :: Text), "start_line" AE..= (1 :: Int), "end_line" AE..= (end :: Int), "message" AE..= ("retry_token drops its argument, so retries lose the caller token." :: Text)]
        draft file end = AE.object ["findings" AE..= V.singleton (AE.object $ finding file end)]
        jsonText value = decodeUtf8 @Text $ AE.encode value
        regression = "set -eu\ntest \"$(cat .prepared-proof)\" = dependencies-ready\n. ./runtime.sh\nif [ \"$(retry_token original)\" != original ]; then echo 'retry token lost' >&2; exit 1; fi\n" :: Text
        check message = SdkTool "run_regression" $ AE.object ["script" AE..= regression, "failureMessage" AE..= (message :: Text)]
    traverse_ (createDirectoryIfMissing True) [repository, store.usageDir, store.cacheDir, outputDir, artifacts]
    void $ git ["init", "--quiet"]
    void $ git ["config", "user.name", "Source fixture"]
    void $ git ["config", "user.email", "fixture@example.invalid"]
    writeFileBS (repository </> "AGENTS.md") "Read the implementation before reporting.\n"
    writeFileBS (repository </> "runtime.sh") "retry_token() { printf '%s\\n' \"$1\"; }\n"
    void $ git ["add", "."]
    void $ git ["commit", "--quiet", "-m", "base"]
    base <- git ["rev-parse", "HEAD"]
    writeFileBS (repository </> "runtime.sh") "retry_token() { printf 'dropped\\n'; }\n"
    void $ git ["commit", "--quiet", "-am", "head"]
    headRevision <- git ["rev-parse", "HEAD"]
    writeFileBS (repository </> "golden-private.txt") "Private reference must not enter the agent workspace.\n"
    let expectedPR = AE.object ["repo" AE..= ("https://github.com/fixture/retry" :: Text), "pr_number" AE..= (17 :: Int), "base" AE..= base, "head" AE..= headRevision]
        source :: Agent.Request "pr_review"
        source = Agent.Request{projectId = testPid, runId = uid 1, attemptId = uid 2, repository = toText repository, base, head = headRevision, gitToken = "", prompt = "Source calibration", provider = "fixture", model = "fixture", thinkingLevel = Agent.Off}
    writeFileLBS taskFile $ AE.encode $ AE.object ["repo" AE..= ("https://github.com/fixture/retry" :: Text), "pr_number" AE..= (17 :: Int), "base" AE..= base, "head" AE..= headRevision, "title" AE..= ("Retry token handling" :: Text), "body" AE..= ("Preserve the caller token." :: Text)]
    plan <- newIORef [SdkText $ jsonText $ draft "runtime.sh" 1]
    replies <- newIORef ([] :: [AE.Value])
    let next results = do
          writeIORef replies results
          choices <- readIORef plan
          pure $ fromMaybe (SdkText "Review submitted.") $ viaNonEmpty head $ drop (length results) choices
    withSdkModel directory "fixture" next \models requests -> do
      outcome <- Checkout.withCheckout store (Checkout.Repository source) \prepared -> tryAny do
        writeFileLBS checkoutFile $ AE.encode prepared
        let run mode name extra = do
              let output = outputDir </> name
                  config = [("RB_NWO", "fixture/retry"), ("RB_PR_NUMBER", "17"), ("RB_BASE", toString base), ("RB_HEAD", toString headRevision), ("RB_REPO", repository), ("RB_OUT", output), ("RB_PR_JSON", taskFile), ("RB_CONFIG_PROJECT_ID", toString testPid.toText), ("RB_CONFIG_PROVIDER", "fixture"), ("RB_CONFIG_MODEL", "fixture"), ("RB_CONFIG_EFFORT", "off"), ("RB_CONFIG_MODE", mode), ("IMPACT_REVIEW_GUEST_IMAGE", profile), ("IMPACT_REVIEW_CHECKOUT", checkoutFile), ("IMPACT_REVIEW_MODELS", models), ("IMPACT_REVIEW_CACHE_DIR", store.cacheDir), ("IMPACT_REVIEW_ARTIFACT_DIR", artifacts), ("IMPACT_REVIEW_NATIVE_CLI", cli)]
                  configured = Map.toList $ Map.fromList $ environment <> [("GONDOLIN_VMM", "krun") | Info.os == "darwin"] <> config <> extra
              result <- Timeout.timeout (90 * 1000000) $ readProcess $ setEnv configured $ proc "node" ["impact-review-worker/reviewbench.mjs"]
              maybe (fail "Calibration CLI exceeded its test deadline") pure result
            success mode name extra = do
              (exit, _, diagnostic) <- run mode name extra
              unless (exit == ExitSuccess) $ fail $ toString $ decodeUtf8 @Text diagnostic
              output <- readJSON $ outputDir </> name
              metadata <- readJSON $ outputDir </> name <> ".meta"
              parse (field "pr") output `shouldReturn` expectedPR
              parse (field "findings") output `shouldReturn` ([AE.object $ finding "runtime.sh" 1 <> ["producer" AE..= ("monoscope-" <> toText mode)]] :: [AE.Value])
              parse (field "status") metadata `shouldReturn` ("completed" :: Text)
              parse (field "durationMs") metadata >>= (\duration -> (duration :: Double) `shouldSatisfy` (> 0))
              pure metadata
        void $ success "bounded-patch" "bounded.json" []
        writeIORef plan [SdkText $ jsonText $ draft "../runtime.sh" 1]
        (invalid, _, _) <- run "bounded-patch" "invalid.json" []
        invalid `shouldSatisfy` (/= ExitSuccess)
        doesFileExist (outputDir </> "invalid.json") `shouldReturn` False
        rejected <- readJSON $ outputDir </> "invalid.json.meta"
        parse (field "status") rejected `shouldReturn` ("failed" :: Text)
        submissions <- parse (field "submissions") rejected :: IO [AE.Value]
        traverse (parse $ field "accepted") submissions `shouldReturn` [False]
        writeFileBS scriptFile "printf dependencies-ready > .prepared-proof"
        writeFileLBS requestFile $ AE.encode $ AE.object ["projectId" AE..= testPid, "repository" AE..= repository, "base" AE..= base]
        checkpoint <- runEff (Workspace.runWorkspaceReal $ Workspace.runWorkspace (Workspace.Options store.cacheDir (toText profile) bridge scriptFile Nothing) (Workspace.Prepare repository) requestFile) >>= either (fail . toString) (maybe (fail "Calibration preparation missing") (pure . AE.toJSON))
        fingerprint <- parse (field "manifest" >=> field "sha256") checkpoint :: IO Text
        writeIORef
          plan
          [ SdkTool "bash" $ AE.object ["command" AE..= ("set -eu; test ! -e golden-private.txt; test ! -w runtime.sh; test \"$(cat .prepared-proof)\" = dependencies-ready; git show " <> base <> ":runtime.sh; cat runtime.sh")]
          , SdkTool "submit_review" $ draft "runtime.sh" 200
          , SdkTool "submit_review" $ draft "runtime.sh" 1
          , check "The runtime loses its caller token"
          , check "retry token lost"
          , SdkTool "submit_review" $ draft "runtime.sh" 1
          ]
        callsBefore <- length <$> requests
        metadata <- success "checkout-harness" "harness.json" [("IMPACT_REVIEW_PREPARE_SCRIPT", scriptFile)]
        accepted <- parse (field "submissions") metadata :: IO [AE.Value]
        traverse (parse $ field "accepted") accepted `shouldReturn` [False, True, True]
        caches <- parse (field "caches") metadata :: IO [AE.Value]
        caches `shouldSatisfy` any (\value -> AET.parseEither (field "type") value == Right ("workspace_cache" :: Text) && AET.parseEither (field "status") value == Right ("hit" :: Text) && AET.parseEither (field "sha256") value == Right fingerprint)
        checks <- parse (field "checks") metadata :: IO [Agent.CheckReceipt]
        map (.status) checks `shouldBe` [Agent.Passed, Agent.Blocked, Agent.Passed, Agent.Failed]
        map (AE.toJSON . (.workspace)) checks `shouldBe` replicate 4 (AE.toJSON fingerprint)
        attempt <- parse (field "attemptId") metadata
        map (.attemptId) checks `shouldBe` replicate 4 attempt
        usage <- parse (field "usage") metadata :: IO [Agent.ModelUsage]
        length usage `shouldBe` 7
        length <$> requests `shouldReturn` callsBefore + length usage
        usage `shouldSatisfy` all (\receipt -> receipt.attemptId == attempt && receipt.succeeded && maybe False (> 0) receipt.costNanousd)
        allocations <- parse (field "allocations") metadata :: IO [Agent.AllocationReceipt]
        map Agent.allocationAttempt allocations `shouldBe` [attempt]
        for_ allocations \receipt -> do
          parse (field "outcome") (AE.toJSON receipt) `shouldReturn` ("released" :: Text)
          parse (field "durationMs") (AE.toJSON receipt) >>= (\duration -> (duration :: Word64) `shouldSatisfy` (> 0))
        results <- readIORef replies
        let reply identifier = maybe (fail $ "Missing tool reply " <> toString identifier) (parse $ field "content") $ find (\value -> AET.parseEither (field "tool_call_id") value == Right identifier) results
        reply "tool-0" >>= (`shouldSatisfy` T.isInfixOf "dropped")
        reply "tool-1" >>= (`shouldSatisfy` T.isInfixOf "outside the file")
        mismatched <- reply "tool-3" >>= either fail pure . AE.eitherDecodeStrict @AE.Value . encodeUtf8
        reason <- parse (field "results") mismatched >>= maybe (fail "Missing counterfactual result") pure . find (\value -> AET.parseEither (field "side") value == Right ("head" :: Text)) >>= parse (field "reason")
        for_ @[] ["case-sensitive literal", "The runtime loses its caller token", "full log"] \message -> reason `shouldSatisfy` T.isInfixOf message
        readFileBS (repository </> "runtime.sh") `shouldReturn` "retry_token() { printf 'dropped\\n'; }\n"
        let reserved = outputDir </> "reserved.json.meta"
        writeFileBS reserved "prior metadata\n"
        count <- length <$> requests
        (existing, _, _) <- run "bounded-patch" "reserved.json" []
        existing `shouldSatisfy` (/= ExitSuccess)
        readFileBS reserved `shouldReturn` "prior metadata\n"
        length <$> requests `shouldReturn` count
        (missing, _, _) <- run "checkout-harness" "unprepared.json" [("IMPACT_REVIEW_DEPENDENCY_ARCHIVE", directory </> "dependencies.tar"), ("IMPACT_REVIEW_PREPARE_SCRIPT", "")]
        missing `shouldSatisfy` (/= ExitSuccess)
        doesFileExist (outputDir </> "unprepared.json.meta") `shouldReturn` False
        length <$> requests `shouldReturn` count
        observed <- jsonText <$> requests
        observed `shouldNotSatisfy` T.isInfixOf "Private reference must"
      either (fail . toString) (either throwIO pure) outcome

  it "explores pinned SDK checkouts with skills, project memory, bounded evidence and isolated cancellation" \_tr -> CLIBinary.withCliBin \cli -> withSystemTempDirectory "native-sdk-exploration" \temporary -> do
    available <- doesFileExist "impact-review-worker/.guest/manifest.json"
    unless available $ pendingWith "Build the self-hosted guest before running sandbox exploration tests"
    directory <- canonicalizePath temporary
    image <- canonicalizePath "impact-review-worker/.guest"
    let repository = directory </> "repository"
        skill = directory </> "review-skill"
        hostSkill = directory </> "host-only-skill"
        sdkTemp = directory </> "tmp"
        diagnostic = directory </> "worker.log"
        store = Agent.ArtifactStore (directory </> "usage") (directory </> "cache")
        uid n = UUIDId $ UUID.fromWords 57 0x4000 0x80000000 n
        field key = AE.withObject "SDK exploration evidence" (AE..: key)
        parse parser = either fail pure . AET.parseEither parser
        jsonText value = decodeUtf8 @Text $ AE.encode value
        readJSON path = readFileBS path >>= either fail pure . AE.eitherDecodeStrict @AE.Value
        git args = T.strip . decodeUtf8 @Text <$> readProcessStdout_ (setWorkingDir repository $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "commit.gpgSign=false"] <> args)
        document = AE.object ["title" AE..= ("Retry context" :: Text), "body" AE..= ("Recheck callers before changing retry behavior." :: Text)]
        page = "00000000-0000-4000-8000-000000000099" :: Text
        draft line keys = AE.object ["findings" AE..= V.singleton (AE.object ["location" AE..= AE.object ["path" AE..= ("src/handler.txt" :: Text), "line" AE..= (line :: Word32), "side" AE..= ("head" :: Text)], "mechanism" AE..= ("Retry metadata is removed before calling the helper." :: Text), "nextStep" AE..= ("Check retry behavior." :: Text), "evidenceKeys" AE..= (keys :: [Text])]), "coverage" AE..= ([] :: [Text])]
        accepted = draft 1 ["telemetry-1"]
        regression = "set -eu\ntest \"$(id -u)\" = 1000\ntest ! -w \"$0\"\n. ./src/runtime.sh\nif [ \"$(retry_token original)\" != original ]; then echo 'retry token was lost' >&2; exit 1; fi\necho 'retry token preserved'\n" :: Text
        tool name args = const $ pure $ SdkTool name args
        readPath path = tool "read" $ AE.object ["path" AE..= (path :: Text)]
        bash command = tool "bash" $ AE.object ["command" AE..= (command :: Text)]
        check script = tool "run_regression" $ AE.object ["script" AE..= (script :: Text), "failureMessage" AE..= ("retry token was lost" :: Text)]
        reply :: Text -> [AE.Value] -> IO Text
        reply identifier results = maybe (fail $ "Missing tool reply " <> toString identifier) (parse $ field "content") $ find (\value -> AET.parseEither (field "tool_call_id") value == Right identifier) results
        fullOutput results = do
          text <- reply "tool-5" results
          let path = T.takeWhile (\c -> c /= ']' && c /= '\n') $ T.drop (T.length "Full output: ") $ snd $ T.breakOn "Full output: " text
          unless ("/run/monoscope-review-output/" `T.isPrefixOf` path && not ("'" `T.isInfixOf` path)) $ fail "SDK did not retain a readable large-output path"
          pure path
    traverse_ (createDirectoryIfMissing True) [repository </> "src", skill, hostSkill, sdkTemp, store.usageDir, store.cacheDir]
    writeFileBS (skill </> "SKILL.md") "---\nname: retry-review\ndescription: Inspect retry metadata in a production review.\n---\nFollow callers before proposing a retry change.\n"
    writeFileBS (hostSkill </> "SKILL.md") "---\nname: host-only-skill\ndescription: This host skill must never be discovered.\n---\nprivate-host-skill-body\n"
    void $ git ["init", "--quiet"]
    void $ git ["config", "user.name", "Exploration fixture"]
    void $ git ["config", "user.email", "fixture@example.invalid"]
    writeFileBS (repository </> "AGENTS.md") "Repository review rule: never remove retry metadata.\n"
    for_ ([(".agents/skills/repo-review", "repo-review"), (".pi/skills/pi-review", "pi-review")] :: [(FilePath, Text)]) \(path, name) -> do
      createDirectoryIfMissing True $ repository </> path
      writeFileBS (repository </> path </> "SKILL.md") $ encodeUtf8 $ "---\nname: " <> (name :: Text) <> "\ndescription: Check repository-specific review behavior.\n---\nRepository skill content.\n"
    createFileLink hostSkill (repository </> ".agents/skills/host-link")
    createDirectoryIfMissing True $ repository </> ".agents/skills/file-link"
    createFileLink (hostSkill </> "SKILL.md") (repository </> ".agents/skills/file-link/SKILL.md")
    writeFileBS (repository </> "src/helper.txt") "preserve-retry-metadata\n"
    writeFileBS (repository </> "src/handler.txt") "calls helper with retry metadata\n"
    writeFileBS (repository </> "src/runtime.sh") "retry_token() { printf '%s\\n' \"$1\"; }\n"
    void $ git ["add", "."]
    void $ git ["commit", "--quiet", "-m", "base"]
    base <- git ["rev-parse", "HEAD"]
    writeFileBS (repository </> "src/handler.txt") "calls helper without retry metadata\n"
    writeFileBS (repository </> "src/runtime.sh") "retry_token() { printf 'dropped\\n'; }\n"
    void $ git ["commit", "--quiet", "-am", "head"]
    headRevision <- git ["rev-parse", "HEAD"]
    writeFileBS (repository </> "src/helper.txt") "unreviewed-later-change\n"
    void $ git ["commit", "--quiet", "-am", "later"]
    plan <- newIORef ([] :: [[AE.Value] -> IO SdkReply])
    replies <- newIORef ([] :: [AE.Value])
    ready <- MVar.newEmptyMVar
    blocked <- MVar.newEmptyMVar :: IO (MVar.MVar ())
    let prefix = [readPath "/skills/0/SKILL.md", readPath "/skills/1/systematic-debugging/SKILL.md", readPath "/skills/1/systematic-debugging/root-cause-tracing.md", readPath "/workspace/.agents/skills/repo-review/SKILL.md", bash $ "set -eu; test \"$(id -u)\" = 1000; test ! -w /usr/bin/git; echo supervisor-tools-protected; pwd; uname -s; git rev-parse HEAD; git show " <> base <> ":src/handler.txt; rg preserve-retry-metadata src; git diff " <> base <> "..." <> headRevision <> "; test -z \"$(printenv IMPACT_REVIEW_FIXTURE_SECRET || true)\"; echo host-credentials-absent; test \"$(cat src/handler.txt)\" = 'calls helper without retry metadata'; echo repository-check-passed; if (printf changed > /skills/0/SKILL.md) 2>/dev/null; then exit 1; fi; if (printf changed > /checkout/AGENTS.md) 2>/dev/null; then exit 1; fi; echo readonly-mounts-enforced; test -z \"$(git config --get-regexp extraheader || true)\"", bash "printf 'first-output-line\\n'; seq 1 2505", fmap (\path -> SdkTool "read" $ AE.object ["path" AE..= path, "limit" AE..= (2 :: Int)]) . fullOutput, fmap (\path -> SdkTool "bash" $ AE.object ["command" AE..= ("head -n 1 '" <> path <> "'")]) . fullOutput]
        reviewPlan limitScript afterSubmit = prefix <> [tool "list_project_memory" $ AE.object [], tool "read_project_memory" $ AE.object ["pageId" AE..= page], tool "write_project_memory" $ AE.object ["pageId" AE..= page, "version" AE..= (0 :: Int), "document" AE..= document], tool "recall_knowledge" $ AE.object ["service" AE..= ("test-service" :: Text)], tool "remember_knowledge" $ AE.object ["service" AE..= ("test-service" :: Text), "summary" AE..= ("Recheck the retry hypothesis." :: Text), "evidenceKeys" AE..= (["telemetry-1"] :: [Text])], check regression, check "set -eu\ngit update-index --assume-unchanged src/runtime.sh\nprintf changed > src/runtime.sh\n", check limitScript, check "exit 0", tool "submit_review" $ draft 1 [], tool "submit_review" $ draft 2 ["telemetry-1"], tool "submit_review" accepted] <> afterSubmit
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
                $ Agent.runCodeReviewReal (Just store) (Just "test/fixtures/native-sdk-worker.py") [models, store.usageDir, store.cacheDir, image, diagnostic, "--cli", cli, "--temp", sdkTemp, "--skill", skill]
                $ Agent.reviewRepository
                  configured
                  (const $ liftIO $ MVar.putMVar ready () >> MVar.takeMVar blocked $> Left "Cancelled query")
                  ( \knowledge -> liftIO $ case knowledge of
                      Agent.ProjectMemory value -> modifyIORef' memories (<> [AE.toJSON value]) $> Right (AE.toJSON value)
                      Agent.Recall service -> pure $ if service == "test-service" then Right $ AE.toJSON $ V.singleton $ AE.object ["summary" AE..= ("Historical hypothesis; verify again." :: Text), "revision" AE..= base] else Left "Unexpected service"
                      Agent.Remember note -> pure $ if note.service == "test-service" && note.summary == "Recheck the retry hypothesis." && toList note.evidenceKeys == ["telemetry-1"] then Right $ AE.String "Note staged" else Left "Unexpected knowledge"
                  )
                  (const pass)
                  (const pass)
                  (\receipt -> liftIO $ modifyIORef' allocations (<> [receipt]))
                  (\receipt -> liftIO $ modifyIORef' checks (<> [receipt]))
                  ( \draftValue -> liftIO do
                      modifyIORef' submissions (<> [draftValue])
                      pure $ if draftValue == accepted then Right () else Left "Finding does not reference a changed line"
                  )
            (,,,,,) result <$> readIORef checks <*> readIORef allocations <*> readIORef submissions <*> readIORef memories <*> readIORef replies
      for_ ([(1, "exec monoscope-unavailable-regression-command", []), (3, "yes regression-output", [readPath "/workspace/src/helper.txt"])] :: [(Word32, Text, [[AE.Value] -> IO SdkReply])]) \(n, limitScript, afterSubmit) -> do
        (result, checks, allocations, submissions, memories, results) <- run n $ reviewPlan limitScript afterSubmit
        if n == 1 then either (fail . toString) (\value -> either fail pure (AE.eitherDecodeStrict $ encodeUtf8 value) >>= (`shouldBe` accepted)) result else isLeft result `shouldBe` True
        map (.status) checks `shouldBe` [Agent.Passed, Agent.Failed, Agent.Invalidated, Agent.Invalidated] <> if n == 1 then [Agent.Blocked, Agent.Blocked] else [Agent.OutputLimit, Agent.OutputLimit]
        map (.exitCode) (take 2 checks) `shouldBe` [Just 0, Just 1]
        when (n == 1) $ map (.exitCode) (drop 4 checks) `shouldBe` [Just 127, Just 127]
        when (n /= 1) $ map (.logBytes) checks `shouldSatisfy` all (<= 1024 * 1024)
        submissions `shouldBe` [draft 2 ["telemetry-1"], accepted]
        memories `shouldBe` [AE.object ["tag" AE..= ("List" :: Text)], AE.object ["tag" AE..= ("Read" :: Text), "contents" AE..= page], AE.object ["tag" AE..= ("Write" :: Text), "contents" AE..= AE.object ["pageId" AE..= page, "version" AE..= (0 :: Int), "document" AE..= document]]]
        length allocations `shouldBe` 1
        for_ allocations \allocation -> do
          Agent.allocationAttempt allocation `shouldBe` request.attemptId
          parse (field "outcome") (AE.toJSON allocation) `shouldReturn` ("released" :: Text)
          parse (field "durationMs") (AE.toJSON allocation) >>= (\duration -> (duration :: Word64) `shouldSatisfy` (> 0))
          sandboxSessions >>= (\sessions -> map (.id) sessions `shouldNotContain` [(Agent.allocationId allocation).toText])
          parse (field "cpus") (AE.toJSON allocation) `shouldReturn` (2 :: Word16)
          parse (field "memoryMiB") (AE.toJSON allocation) `shouldReturn` (1024 :: Word32)
        reply "tool-5" results >>= (`shouldNotSatisfy` T.isInfixOf "first-output-line")
        for_ ["tool-6", "tool-7"] \identifier -> reply identifier results >>= (`shouldSatisfy` T.isInfixOf "first-output-line")
        for_ ["Linux", "supervisor-tools-protected", "host-credentials-absent", "repository-check-passed", "readonly-mounts-enforced", "preserve-retry-metadata", "Follow callers before proposing", "Repository skill content", "NO FIXES WITHOUT ROOT CAUSE INVESTIGATION FIRST", "Root Cause Tracing", "Historical hypothesis", headRevision] \text -> jsonText results `shouldSatisfy` T.isInfixOf text
        for_ ["host-only-fixture", "unreviewed-later-change", "fixture-git-token", "private-host-skill-body"] \text -> jsonText results `shouldNotSatisfy` T.isInfixOf text
        reply "tool-16" results >>= (`shouldSatisfy` T.isInfixOf "budget")
        reply "tool-17" results >>= (`shouldSatisfy` T.isInfixOf "evidenceKeys")
        reply "tool-18" results >>= (`shouldSatisfy` T.isInfixOf "changed line")
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
      observed <- requests
      for_ observed \body -> do
        tools <- parse (field "tools") body :: IO [AE.Value]
        names <- traverse (parse $ field "function" >=> field "name") tools :: IO [Text]
        for_ ["read", "bash", "submit_review", "run_regression", "recall_knowledge", "remember_knowledge"] \name -> names `shouldContain` [name]
        names `shouldNotContain` ["edit"]
        names `shouldNotContain` ["write"]
        for_ ["never remove retry metadata", "retry-review", "repo-review", "pi-review", "systematic-debugging", "test-driven-development", "verification-before-completion"] \text -> jsonText body `shouldSatisfy` T.isInfixOf text
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

  it "evaluates actual SDK reviews with compaction receipts and explicit schema, citation and provider failures" \_tr -> CLIBinary.withCliBin \cli -> withSystemTempDirectory "native-sdk-evaluation" \temporary -> do
    available <- doesFileExist "impact-review-worker/.guest/manifest.json"
    unless available $ pendingWith "Build the self-hosted guest before running evaluation integration tests"
    directory <- canonicalizePath temporary
    image <- canonicalizePath "impact-review-worker/.guest"
    environment <- filter (\(key, _) -> key `elem` ["PATH", "HOME"]) <$> getEnvironment
    let repository = directory </> "repository"
        store = Agent.ArtifactStore (directory </> "usage") (directory </> "cache")
        sdkTemp = directory </> "tmp"
        caseFile = directory </> "case.json"
        uid n = UUIDId $ UUID.fromWords 56 0x4000 0x80000000 n
        field key = AE.withObject "SDK evaluation evidence" (AE..: key)
        parse parser = either fail pure . AET.parseEither parser
        jsonText = decodeUtf8 @Text . AE.encode
        readJSON path = readFileBS path >>= either fail pure . AE.eitherDecodeStrict @AE.Value
        git args = T.strip . decodeUtf8 @Text <$> readProcessStdout_ (setWorkingDir repository $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "commit.gpgSign=false"] <> args)
        draft line keys = AE.object ["findings" AE..= V.singleton (AE.object ["location" AE..= AE.object ["path" AE..= ("runtime.sh" :: Text), "line" AE..= (line :: Word32), "side" AE..= ("head" :: Text)], "mechanism" AE..= ("Retry metadata is removed before calling the helper." :: Text), "nextStep" AE..= ("Check retry behavior." :: Text), "evidenceKeys" AE..= (keys :: [Text])]), "coverage" AE..= ([] :: [Text])]
        accepted = draft 1 ["telemetry-1"]
        summarizing = T.isInfixOf "You are a context summarization assistant" . jsonText
    traverse_ (createDirectoryIfMissing True) [repository, store.usageDir, store.cacheDir, sdkTemp]
    void $ git ["init", "--quiet"]
    void $ git ["config", "user.name", "Evaluation fixture"]
    void $ git ["config", "user.email", "fixture@example.invalid"]
    writeFileBS (repository </> "runtime.sh") "retry_token() { printf '%s\\n' \"$1\"; }\n"
    void $ git ["add", "."]
    void $ git ["commit", "--quiet", "-m", "base"]
    base <- git ["rev-parse", "HEAD"]
    writeFileBS (repository </> "runtime.sh") "retry_token() { printf 'dropped\\n'; }\n"
    void $ git ["commit", "--quiet", "-am", "head"]
    headRevision <- git ["rev-parse", "HEAD"]
    baseline <- newIORef accepted
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
                          response <- atomicModifyIORef' baseline (accepted,)
                          pure $ SdkResponse (SdkText $ jsonText response) (SdkTokens 7 2)
    withSdkProvider directory "evaluation-fixture" next \models requests -> do
      let request = Agent.Request{projectId = testPid, runId = uid 1, attemptId = uid 2, repository = toText repository, base, head = headRevision, gitToken = "", prompt = "Review telemetry-1.", provider = "evaluation-fixture", model = "fixture", thinkingLevel = Agent.Off}
      outcome <- Checkout.withCheckout store (Checkout.Repository request) \prepared -> tryAny do
        writeFileLBS caseFile $ AE.encode $ AE.object ["checkout" AE..= prepared, "projectId" AE..= testPid, "repository" AE..= repository, "base" AE..= base, "head" AE..= headRevision, "provider" AE..= request.provider, "model" AE..= request.model, "evidenceKind" AE..= ("synthetic" :: Text), "evidence" AE..= V.fromList [AE.object ["key" AE..= ("telemetry-1" :: Text), "kind" AE..= ("telemetry" :: Text), "description" AE..= ("synthetic-evaluation-evidence" :: Text)], AE.object ["key" AE..= ("monitor-1" :: Text), "kind" AE..= ("monitor" :: Text)]], "rubric" AE..= (["private-grading-not-model"] :: [Text])]
        let configured = Map.toList $ Map.fromList $ environment <> [("TMPDIR", sdkTemp), ("IMPACT_REVIEW_MODELS", models), ("IMPACT_REVIEW_GUEST_IMAGE", image), ("IMPACT_REVIEW_NATIVE_CLI", cli), ("IMPACT_REVIEW_USAGE_DIR", store.usageDir), ("IMPACT_REVIEW_CACHE_DIR", store.cacheDir)] <> [("GONDOLIN_VMM", "krun") | Info.os == "darwin"]
            run name = do
              let output = directory </> name <> ".json"
              completed <- Timeout.timeout (90 * 1000000) $ readProcess $ setEnv configured $ proc "node" ["impact-review-worker/evaluate.mjs", caseFile, output]
              (exit, _, diagnostic) <- maybe (fail "Evaluation CLI exceeded its test deadline") pure completed
              artifact <- readJSON output
              rows <- parse (field "runs") artifact :: IO [AE.Value]
              listDirectory sdkTemp `shouldReturn` []
              pure (exit, diagnostic, artifact, rows)
        (exit, diagnostic, artifact, rows) <- run "accepted"
        unless (exit == ExitSuccess) $ fail $ toString $ decodeUtf8 @Text diagnostic
        traverse (parse $ field "mode") rows `shouldReturn` (["bounded-patch", "checkout-harness"] :: [Text])
        traverse (parse $ field "status") rows `shouldReturn` (["completed", "completed"] :: [Text])
        traverse (parse $ field "draft") rows `shouldReturn` [accepted, accepted]
        parse (field "harness" >=> field "skills" >=> field "revision") artifact `shouldReturn` ("bb92a77741419a4ab5f06e711a283343f1ada0c3" :: Text)
        harnessHash <- parse (field "harness" >=> field "files" >=> field "review.mjs") artifact :: IO Text
        harnessHash `shouldSatisfy` (\value -> T.length value == 64 && T.all (`elem` (['0' .. '9'] <> ['a' .. 'f'])) value)
        for_ (zip [0 :: Int ..] rows) \(index, row) -> do
          attempt <- parse (field "attemptId") row
          receipts <- parse (field "usage") row >>= traverse (parse $ field "receipt") :: IO [Agent.ModelUsage]
          length receipts `shouldBe` if index == 0 then 1 else 3
          length (ordNub $ map (.id) receipts) `shouldBe` length receipts
          receipts `shouldSatisfy` all (\receipt -> receipt.attemptId == attempt && receipt.provider == request.provider && receipt.succeeded)
          let summary = filter (\receipt -> receipt.inputTokens == 775 && receipt.cacheReadTokens == 2 && receipt.costNanousd == Just 1582000) receipts
          length summary `shouldBe` index
          let ordinary = filter (\receipt -> receipt.inputTokens == 5) receipts
          length ordinary `shouldBe` 1
          for_ ordinary \receipt -> receipt.costNanousd `shouldBe` Just 42000
          allocations <- parse (field "allocations") row :: IO [Agent.AllocationReceipt]
          length allocations `shouldBe` index
          when (index == 1) $ parse (field "checkout" >=> field "cacheStatus") row `shouldReturn` AE.toJSON prepared.cacheStatus
          for_ allocations \allocation -> do
            Agent.allocationAttempt allocation `shouldBe` attempt
            parse (field "outcome") (AE.toJSON allocation) `shouldReturn` ("released" :: Text)
          submissions <- parse (field "submissions") row :: IO [AE.Value]
          traverse (parse $ field "accepted") submissions `shouldReturn` [True]
          parse (field "durationMs") row >>= (\duration -> (duration :: Word64) `shouldSatisfy` (> 0))
        recorded <- requests
        length (filter summarizing recorded) `shouldBe` 1
        for_ recorded \body -> do
          jsonText body `shouldSatisfy` T.isInfixOf "synthetic-evaluation-evidence"
          jsonText body `shouldNotSatisfy` T.isInfixOf "private-grading-not-model"
        for_ ([("schema", draft 1 [], "schema", "evidenceKeys"), ("citation", draft 2 ["telemetry-1"], "validation", "changed line"), ("evidence", draft 1 ["invented"], "validation", "unavailable evidence"), ("context", draft 1 ["monitor-1"], "validation", "production context")] :: [(FilePath, AE.Value, Text, Text)]) \(name, rejected, failureType, message) -> do
          writeIORef baseline rejected
          (failed, _, _, rejectedRows) <- run name
          failed `shouldSatisfy` (/= ExitSuccess)
          traverse (parse $ field "status") rejectedRows `shouldReturn` (["failed", "completed"] :: [Text])
          case rejectedRows of
            rejectedRow : _ -> do
              parse (field "failureType") rejectedRow `shouldReturn` (failureType :: Text)
              parse (AE.withObject "Rejected evaluation" (AE..:? "draft")) rejectedRow `shouldReturn` (Nothing :: Maybe AE.Value)
              jsonText rejectedRow `shouldSatisfy` T.isInfixOf message
            [] -> expectationFailure "Evaluation discarded both runs"
        writeIORef unavailable True
        (failed, _, _, failedRows) <- run "provider-failed"
        failed `shouldSatisfy` (/= ExitSuccess)
        length failedRows `shouldBe` 1
        for_ failedRows \row -> do
          parse (field "status") row `shouldReturn` ("failed" :: Text)
          parse (AE.withObject "Failed evaluation" (AE..:? "draft")) row `shouldReturn` (Nothing :: Maybe AE.Value)
      either (fail . toString) (either throwIO pure) outcome
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
                $ Agent.runCodeReviewReal (Just store) (Just "test/fixtures/native-sdk-worker.py") [models, store.usageDir, store.cacheDir, image, directory </> "worker.log", "--cli", cli, "--temp", sdkTemp]
                $ Agent.reviewRepository
                  configured
                  (\telemetryQuery -> liftIO $ modifyIORef' queries (<> [telemetryQuery]) $> Right (AE.object ["key" AE..= ("query-1" :: Text), "queryResult" AE..= AE.object ["kind" AE..= ("aggregated" :: Text), "headers" AE..= (["count"] :: [Text]), "dataset" AE..= ([[7]] :: [[Int]]), "limitReached" AE..= False, "fieldsTruncated" AE..= False]]))
                  ( \knowledgeQuery -> liftIO $ case knowledgeQuery of
                      Agent.Recall service -> modifyIORef' knowledge (<> [AE.object ["tag" AE..= ("Recall" :: Text), "contents" AE..= service]]) $> Right (AE.toJSON ([] :: [AE.Value]))
                      Agent.Remember note -> modifyIORef' knowledge (<> [AE.object ["tag" AE..= ("Remember" :: Text), "contents" AE..= note]]) $> Right (AE.String "Note staged")
                      Agent.ProjectMemory _ -> fail "Unexpected wiki query"
                  )
                  (\receipt -> liftIO $ retained receipt.id.toText (AE.object ["tag" AE..= ("Call" :: Text), "contents" AE..= receipt]) >> modifyIORef' calls (<> [receipt]))
                  (\receipt -> liftIO $ retained receipt.id.toText (AE.toJSON receipt) >> modifyIORef' usage (<> [receipt]))
                  (\receipt -> liftIO $ retained (Agent.allocationId receipt).toText (AE.object ["tag" AE..= ("Allocation" :: Text), "contents" AE..= receipt]) >> modifyIORef' allocations (<> [receipt]))
                  (\receipt -> liftIO $ retained receipt.id.toText (AE.object ["tag" AE..= ("Check" :: Text), "contents" AE..= receipt]) >> modifyIORef' checks (<> [receipt]))
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
      for_ (allocations <> failedAllocations) \allocation -> do
        Agent.allocationAttempt allocation `shouldBe` request.attemptId
        parse (field "outcome") (AE.toJSON allocation) `shouldReturn` ("released" :: Text)
        parse (field "durationMs") (AE.toJSON allocation) >>= (\duration -> (duration :: Word64) `shouldSatisfy` (> 0))

  it "replays offline npm dependencies through the SDK without install scripts or shared mutations" \_tr -> CLIBinary.withCliBin \cli -> withSystemTempDirectory "native-sdk-dependencies" \temporary -> do
    configuredProfile <- lookupEnv "IMPACT_REVIEW_TEST_PROFILE"
    let profile = fromMaybe "impact-review-worker/.guest-monoscope" configuredProfile
    available <- doesFileExist $ profile </> "manifest.json"
    unless (available || isJust configuredProfile) $ pendingWith "Set IMPACT_REVIEW_TEST_PROFILE or build impact-review-worker/.guest-monoscope"
    directory <- canonicalizePath temporary
    bridge <- canonicalizePath "test/fixtures/native-sandbox.sh"
    let fixture = directory </> "package"
        repository = directory </> "repository"
        cache = directory </> "npm-cache"
        archive = directory </> "dependencies.tar"
        script = directory </> "prepare.sh"
        requestFile = directory </> "request.json"
        diagnostic = directory </> "worker.log"
        store = Agent.ArtifactStore (directory </> "usage") (directory </> "cache")
        name = "monoscope-dependency-fixture" :: Text
        version = "1.0.0-fixture" :: Text
        uid n = UUIDId $ UUID.fromWords 53 0x4000 0x80000000 n
        field key = AE.withObject "Dependency replay fixture" (AE..: key)
        parse parser = either fail pure . AET.parseEither parser
        git args = T.strip . decodeUtf8 @Text <$> readProcessStdout_ (setWorkingDir repository $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "commit.gpgSign=false"] <> args)
        npm cwd args = readProcessStdout_ $ setWorkingDir cwd $ proc "npm" args
        sha bytes = show (hash bytes :: Digest SHA256) :: Text
        preparation = "set -eu\nmkdir -p /home/review-agent/dependency-cache\ntar -xf /run/monoscope-dependencies.tar -C /home/review-agent/dependency-cache\nnpm ci --offline --ignore-scripts --no-audit --no-fund --cache /home/review-agent/dependency-cache\ntest ! -e /workspace/install-script-ran\ntest ! -e /home/review-agent/dependency-cache/host-only-secret\n" :: ByteString
        replay = "set -eu\ntest ! -e /workspace/install-script-ran\ntest ! -e node_modules/monoscope-dependency-fixture/vm-only\ntouch node_modules/monoscope-dependency-fixture/vm-only\nnode check.cjs\n" :: Text
        options = Workspace.Options store.cacheDir (toText profile) bridge script (Just archive)
        lookupPrepared configured = runEff $ Workspace.runWorkspaceReal $ Workspace.runWorkspace configured Workspace.Lookup requestFile
    traverse_ (createDirectoryIfMissing True) [fixture, repository, cache, store.usageDir, store.cacheDir]
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
    let dependency = AE.object ["sha256" AE..= sha bytes, "bytes" AE..= BS.length bytes]
        dependencies = AE.object [fromString (toString name) AE..= version]
        consumer = AE.object ["name" AE..= ("dependency-consumer" :: Text), "version" AE..= ("1.0.0" :: Text), "dependencies" AE..= dependencies]
        integrity = "sha512-" <> extractBase64 (B64.encodeBase64 $ BA.convert (hash contents :: Digest SHA512))
    writeFileLBS (repository </> "package.json") $ AE.encode consumer
    writeFileLBS (repository </> "package-lock.json") $ AE.encode $ AE.object ["name" AE..= ("dependency-consumer" :: Text), "version" AE..= ("1.0.0" :: Text), "lockfileVersion" AE..= (3 :: Int), "packages" AE..= AE.object ["" AE..= consumer, fromString (toString $ "node_modules/" <> name) AE..= AE.object ["version" AE..= version, "resolved" AE..= ("https://registry.npmjs.org/" <> name <> "/-/" <> name <> "-" <> version <> ".tgz"), "integrity" AE..= integrity, "hasInstallScript" AE..= True]]]
    writeFileBS (repository </> "runtime.cjs") "module.exports = require('monoscope-dependency-fixture');\n"
    writeFileBS (repository </> "check.cjs") "require('node:assert/strict').equal(require('./runtime.cjs')('original'), 'original', 'dependency echo lost');\n"
    void $ git ["init", "--quiet"]
    void $ git ["config", "user.name", "Dependency fixture"]
    void $ git ["config", "user.email", "fixture@example.invalid"]
    void $ git ["add", "."]
    void $ git ["commit", "--quiet", "-m", "base"]
    base <- git ["rev-parse", "HEAD"]
    writeFileBS (repository </> "runtime.cjs") "module.exports = () => require('monoscope-dependency-fixture')('lost');\n"
    void $ git ["commit", "--quiet", "-am", "head"]
    headRevision <- git ["rev-parse", "HEAD"]
    writeFileLBS requestFile $ AE.encode $ AE.object ["projectId" AE..= testPid, "repository" AE..= repository, "base" AE..= base]
    writeFileBS script preparation
    prepared <- runEff (Workspace.runWorkspaceReal $ Workspace.runWorkspace options (Workspace.Prepare repository) requestFile) >>= either (fail . toString) (maybe (fail "Dependency preparation missing") (pure . AE.toJSON))
    manifest <- parse (field "manifest") prepared
    parse (field "identity" >=> field "dependency") manifest `shouldReturn` dependency
    imageId <- parse (field "identity" >=> field "imageBuildId") manifest :: IO Text
    let legacy = Encoding.pairs ("projectId" AE..= testPid <> "repository" AE..= repository <> "base" AE..= base <> "imageBuildId" AE..= imageId <> "preparationSha256" AE..= sha preparation <> Encoding.pair "dependency" (Encoding.pairs $ "sha256" AE..= sha bytes <> "bytes" AE..= BS.length bytes))
        key = sha $ toStrict $ Encoding.encodingToLazyByteString legacy
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
          1 -> SdkTool "submit_review" (AE.object ["findings" AE..= ([] :: [AE.Value]), "coverage" AE..= ([] :: [AE.Value])])
          _ -> SdkText "Fixture finished."
    withSdkModel directory "dependency-fixture" next \models requests -> do
      let request = Agent.Request{projectId = testPid, runId = uid 1, attemptId = uid 2, repository = toText repository, base, head = headRevision, gitToken = "", prompt = "Check actual offline dependency behavior.", provider = "dependency-fixture", model = "fixture", thinkingLevel = Agent.Off}
          run n trustedScript = do
            checks <- newIORef ([] :: [Agent.CheckReceipt])
            result <-
              runEff
                $ Agent.runCodeReviewReal (Just store) (Just "test/fixtures/native-sdk-worker.py") [models, store.usageDir, store.cacheDir, profile, diagnostic, "--prepare", trustedScript, "--dependencies", archive, "--cli", cli]
                $ Agent.reviewRepository request{Agent.runId = uid n} (const $ pure $ Left "Unexpected query") (const $ pure $ Left "Unexpected knowledge") (const pass) (const pass) (const pass) (\receipt -> liftIO $ modifyIORef' checks (<> [receipt])) (const $ pure $ Right ())
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
      AE.eitherDecodeStrict @AE.Value (encodeUtf8 encoded) `shouldBe` Right (AE.object ["findings" AE..= ([] :: [AE.Value]), "coverage" AE..= ([] :: [AE.Value])])
      fingerprint <- parse (field "sha256") manifest :: IO Text
      map (\receipt -> (receipt.side, receipt.status, AE.toJSON receipt.workspace)) checks `shouldBe` [(Agent.Base, Agent.Passed, AE.toJSON fingerprint), (Agent.Head, Agent.Failed, AE.toJSON fingerprint)]
      events <- mapMaybe (AE.decodeStrict @AE.Value) . BSC.lines <$> readFileBS diagnostic
      events `shouldSatisfy` any (\value -> AET.parseEither (field "type") value == Right ("workspace_cache" :: Text) && AET.parseEither (field "status") value == Right ("hit" :: Text) && AET.parseEither (field "sha256") value == Right fingerprint)

  it "runs frozen regressions natively, preserves receipts and rejects weakened candidate tests" \_tr -> withSystemTempDirectory "native-regression" \temporary -> do
    directory <- canonicalizePath temporary
    available <- doesFileExist "impact-review-worker/.guest/manifest.json"
    unless available $ pendingWith "Build the self-hosted guest with impact-review-worker/build-guest.mjs before running microVM integration tests"
    image <- canonicalizePath "impact-review-worker/.guest"
    bridge <- canonicalizePath "test/fixtures/native-sandbox.sh"
    let repository = directory </> "repository"
        artifacts = directory </> "checks"
        uid n = UUIDId $ UUID.fromWords 0 0 0 n :: UUIDId "fixture"
        field name = AE.withObject "Native regression evidence" (AE..: name)
        parse parser = either fail pure . AET.parseEither parser
        git args = toStrict <$> readProcessStdout_ (setWorkingDir repository $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "commit.gpgSign=false"] <> args)
        revisionAt = T.strip . decodeUtf8 @Text <$> git ["rev-parse", "HEAD"]
        sha bytes = show (hash bytes :: Digest SHA256) :: Text
    createDirectoryIfMissing True repository
    createDirectoryIfMissing True artifacts
    void $ git ["init", "--quiet"]
    void $ git ["config", "user.name", "Regression fixture"]
    void $ git ["config", "user.email", "fixture@example.invalid"]
    writeFileBS (repository </> "runtime.sh") "retry_token() { printf 'dropped\\n'; }\n"
    void $ git ["add", "."]
    void $ git ["commit", "--quiet", "-m", "broken base"]
    base <- revisionAt
    writeFileBS (repository </> "regression.sh") "set -eu\n. ./runtime.sh\nif [ \"$(retry_token original)\" != original ]; then echo 'retry token was lost' >&2; exit 1; fi\necho regression-passed\n"
    void $ git ["add", "."]
    void $ git ["commit", "--quiet", "-m", "retain regression"]
    testPatch <- git ["show", "--format=", "--binary", "HEAD"]
    writeFileBS (repository </> "runtime.sh") "retry_token() { printf '%s\\n' \"$1\"; }\n"
    void $ git ["commit", "--quiet", "-am", "fix retry token"]
    imageManifest <- readFileBS (image </> "manifest.json") >>= either fail pure . AE.eitherDecodeStrict
    imageId <- parse (field "buildId" :: AE.Value -> AET.Parser Text) imageManifest
    let run session script patch = do
          headRevision <- revisionAt
          let artifact = AE.object ["script" AE..= (script :: Text), "testPatch" AE..= decodeUtf8 @Text patch, "failureMessage" AE..= ("retry token was lost" :: Text)]
              request = AE.object ["projectId" AE..= testPid, "runId" AE..= uid 1, "attemptId" AE..= uid 2, "sessionId" AE..= uid session, "base" AE..= base, "imageBuildId" AE..= imageId, "workspace" AE..= AE.object ["tag" AE..= ("Pristine" :: Text)], "artifact" AE..= artifact, "revisions" AE..= ([["base", base], ["head", headRevision]] :: [[Text]])]
              options = Workspace.RegressionOptions artifacts repository (toText image) bridge
              requestPath = directory </> "request.json"
          writeFileLBS requestPath $ AE.encode request
          outcome <- runEff $ Workspace.runWorkspaceReal $ Workspace.runRegression options requestPath
          result <- either (fail . toString . Workspace.regressionErrorText) (pure . AE.toJSON) outcome
          report <- parse (field "report") result
          rows <- parse (field "results") report :: IO [AE.Value]
          receipts <- parse (field "receipts") result :: IO [Agent.CheckReceipt]
          for_ receipts \receipt -> do
            retained <- readFileBS (directory </> toString receipt.id.toText <> ".json") >>= either fail pure . AE.eitherDecodeStrict
            retained `shouldBe` AE.object ["tag" AE..= ("Check" :: Text), "contents" AE..= receipt]
            readFileBS (artifacts </> toString receipt.checkId.toText </> "test.patch") `shouldReturn` patch
          pure (report, rows, receipts)
        statuses rows = traverse (parse $ field "status") rows :: IO [Text]
    (report, rows, receipts) <- run 3 "bash regression.sh" testPatch
    statuses rows `shouldReturn` ["failed", "passed"]
    parse (field "testPatchSha256") report `shouldReturn` sha testPatch
    fingerprints <- traverse (parse $ field "testFilesSha256") rows :: IO [Text]
    length (ordNub fingerprints) `shouldBe` 1
    let frozen = "{\"script\":\"bash regression.sh\",\"testPatch\":" <> toStrict (AE.encode $ decodeUtf8 @Text testPatch) <> ",\"failureMessage\":\"retry token was lost\"}"
    AE.toJSON (map (.regression) receipts) `shouldBe` AE.toJSON ([Just $ Agent.InjectedTest $ sha frozen, Just $ Agent.RetainedTest $ sha frozen] :: [Maybe Agent.Regression])
    (_, mutated, _) <- run 3 "printf weakened > regression.sh" testPatch
    statuses mutated `shouldReturn` ["invalidated", "invalidated"]
    (_, invalid, _) <- run 3 "exit 0" "not a patch"
    statuses invalid `shouldReturn` ["blocked", "blocked"]
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

  it "reuses native Haskell build checkpoints without mutating relocated baselines" \tr -> do
    configuredProfile <- lookupEnv "IMPACT_REVIEW_TEST_PROFILE"
    let profile = fromMaybe "impact-review-worker/.guest-monoscope" configuredProfile
    localProfile <- doesFileExist $ profile </> "manifest.json"
    unless (localProfile || isJust configuredProfile) $ pendingWith "Set IMPACT_REVIEW_TEST_PROFILE or build impact-review-worker/.guest-monoscope"
    withSystemTempDirectory "native-profile" \temporary -> do
      directory <- canonicalizePath temporary
      bridge <- canonicalizePath "test/fixtures/native-sandbox.sh"
      let repository = directory </> "repository"
          cache = directory </> "cache"
          script = directory </> "prepare.sh"
          request = directory </> "request.json"
          checks = directory </> "checks"
          field name = AE.withObject "Prepared profile" (AE..: name)
          parse parser = either fail pure . AET.parseEither parser
          git args = toStrict <$> readProcessStdout_ (setWorkingDir repository $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "commit.gpgSign=false"] <> args)
          revisionAt = T.strip . decodeUtf8 @Text <$> git ["rev-parse", "HEAD"]
          source = "module Main where\nimport Data.Aeson (eitherDecodeStrict')\nimport qualified Data.ByteString.Char8 as BS\nmain = case eitherDecodeStrict' (BS.pack (show (\"cached\" :: String))) :: Either String String of\n  Right \"cached\" -> putStrLn \"cached-package-check-passed\"\n  _ -> fail \"cached dependency decoding failed\"\n" :: ByteString
          options = Workspace.Options cache (toText profile) bridge script Nothing
          lookupPrepared encodedRequest workspaceOptions = do
            writeFileLBS request $ AE.encode encodedRequest
            runEff $ Workspace.runWorkspaceReal $ Workspace.runWorkspace workspaceOptions Workspace.Lookup request
      createDirectoryIfMissing True repository
      createDirectoryIfMissing True cache
      createDirectoryIfMissing True checks
      void $ git ["init", "--quiet"]
      void $ git ["config", "user.name", "Profile fixture"]
      void $ git ["config", "user.email", "fixture@example.invalid"]
      writeFileBS (repository </> "Main.hs") source
      writeFileBS (repository </> "Makefile") "prepared-app: Main.hs\n\tghc -O0 -package-db=\"$$(echo \"$$HOME\"/.cabal/store/ghc-*/package.db)\" -package=aeson Main.hs -o prepared-app\ncheck: prepared-app\n\ttest \"$$(id -u)\" = 1000\n\ttest ! -e host-only-secret\n\ttest \"$$(ghc --numeric-version)\" = 9.12.2\n\tmonoscope --help >/dev/null\n\tnode --version\n\ttest ! -e \"$$HOME/.cabal/profile-test-marker\"\n\ttouch \"$$HOME/.cabal/profile-test-marker\"\n\t./prepared-app\n"
      void $ git ["add", "."]
      void $ git ["commit", "--quiet", "-m", "base"]
      base <- revisionAt
      writeFileBS (repository </> "Main.hs") $ encodeUtf8 $ T.replace "show (\"cached\" :: String)" "show (\"lost\" :: String)" $ decodeUtf8 @Text source
      void $ git ["commit", "--quiet", "-am", "head"]
      headRevision <- revisionAt
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
      let mismatchedImage = case original of AE.Object fields -> AE.Object $ KM.insert "expectedImageBuildId" (AE.toJSON $ UUID.fromWords 0 0 0 99) fields; value -> value
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
          sha bytes = show (hash bytes :: Digest SHA256) :: Text
      void $ readProcessStdout_ $ proc "qemu-img" ["rebase", "-u", "-F", "raw", "-b", "/old-monoscope-host/rootfs.ext4", checkpoint]
      relocatedBytes <- readFileBS checkpoint
      let relocatedHash = sha relocatedBytes
          relocatedManifest = case manifest of AE.Object fields -> AE.Object $ KM.insert "bytes" (AE.toJSON $ BS.length relocatedBytes) $ KM.insert "sha256" (AE.String relocatedHash) fields; value -> value
      writeFileLBS manifestPath $ AE.encode relocatedManifest
      writeFileLBS request $ AE.encode $ regression checkpoint relocatedHash
      relocated <- runEff (Workspace.runWorkspaceReal $ Workspace.runRegression (Workspace.RegressionOptions checks repository (toText profile) bridge) request) >>= either (fail . show) (pure . AE.toJSON)
      sha <$> readFileBS checkpoint `shouldReturn` relocatedHash
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
            sha <$> readFileBS immutable `shouldReturn` relocatedHash
            removeFile checkpoint
            writeFileLBS request $ AE.encode $ regression immutable relocatedHash
          replay <- Workspace.runRegression (Workspace.RegressionOptions checks repository (toText profile) bridge) request
          liftIO do
            replayed <- either (fail . show) (pure . AE.toJSON) replay
            statuses <- parse (field "report" >=> field "results") replayed >>= traverse (parse $ field "status")
            statuses `shouldBe` (["passed", "failed"] :: [Text])
            sha <$> readFileBS immutable `shouldReturn` relocatedHash
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
            connection = Minio.setCreds (Minio.CredentialValue (fromString $ toString config.s3AccessKey) (fromString $ toString config.s3SecretKey) Nothing) $ Minio.setRegion config.s3Region $ fromString $ toString config.s3Endpoint
            overwrite key bytes = do
              uploaded <- Storage.runMinio tr.trATCtx.s3HttpManager connection $ Minio.putObject config.s3Bucket key (CC.sourceLazy bytes) (Just $ LBS.length bytes) Minio.defaultPutObjectOptions
              uploaded `shouldSatisfy` isRight
            replace key value = \case AE.Object fields -> AE.Object $ KM.insert key value fields; other -> other
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
            sha <$> readFileBS checkpoint `shouldReturn` relocatedHash
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
            sha <$> readFileBS checkpoint `shouldReturn` relocatedHash
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
            sha <$> readFileBS checkpoint `shouldReturn` relocatedHash
            writeFileLBS request $ AE.encode original
            let records = cache </> toString testPid.toText </> "transfers"
            receipts <- listDirectory records >>= traverse (\name -> readFileBS (records </> name) >>= either fail pure . AE.eitherDecodeStrict @AE.Value)
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
              record <- readFileBS (cache </> toString testPid.toText </> "prunes" </> name) >>= either fail pure . AE.eitherDecodeStrict @AE.Value
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
            sha <$> readFileBS otherPath `shouldReturn` otherHash
            writeFileBS script "exec make prepared-app"
            writeFileLBS remote $ AE.encode $ replace "maxBytes" (AE.toJSON (1 :: Word64)) remoteConfiguration
            void $ maintain 0
            doesFileExist checkpoint `shouldReturn` True
            doesFileExist otherPath `shouldReturn` True
            writeFileLBS remote $ AE.encode remoteConfiguration
            roomy <- maintain maxBound
            parse (field "failures") roomy `shouldReturn` ([] :: [AE.Value])
            CLIBinary.withCliBin \cli -> do
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
                    recoveredRecord <- readFileBS journal >>= either fail pure . AE.eitherDecodeStrict @AE.Value
                    parse (field "phase" >=> field "tag") recoveredRecord `shouldReturn` ("StageRecovered" :: Text)
                    doesFileExist checkpoint `shouldReturn` False
                )
                  `finally` writeFileBS release "release"
              (afterCrash, _) <- transfer "restore" cache
              afterCrash `shouldBe` ExitSuccess
              sha <$> readFileBS checkpoint `shouldReturn` relocatedHash
            let stageRecords = cache </> toString testPid.toText </> "stages"
            storedStages <- listDirectory stageRecords >>= traverse (\name -> readFileBS (stageRecords </> name) >>= either fail pure . AE.eitherDecodeStrict @AE.Value)
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
            sha <$> readFileBS checkpoint `shouldReturn` relocatedHash
            fakeMaintenance <- either fail pure $ AE.eitherDecode $ AE.encode roomy
            simulated <- runEff $ Workspace.runWorkspaceTest (\_ _ _ -> Left "Unexpected workspace") (\_ _ -> Left Workspace.RegressionUnavailable) (\_ _ -> Left "Unexpected prune") (const $ Right fakeMaintenance) $ Workspace.maintainWorkspaces "/missing-maintenance"
            fmap AE.toJSON simulated `shouldBe` Right roomy
            readFileBS unrelated `shouldReturn` "operator content"
            readFileBS (foreignProject </> "keep") `shouldReturn` "foreign project"
          )
          `finally` removeObjects
      retainedManifest <- readFileBS manifestPath
      writeFileLBS manifestPath $ AE.encode $ case manifest of AE.Object fields -> AE.Object $ KM.insert "sha256" (AE.String "invalid-digest") fields; value -> value
      isLeft <$> lookupPrepared original options `shouldReturn` True
      writeFileBS manifestPath retainedManifest
      writeFileBS checkpoint "broken checkpoint"
      isLeft <$> lookupPrepared original options `shouldReturn` True

  it "rejects a child artifact acknowledgement when retained files are absent" \_tr -> withSystemTempDirectory "missing-native-artifacts" \directory -> do
    base <- either (fail . show) pure $ Fixes.mkRevision $ T.replicate 40 "a"
    candidateRevision <- either (fail . show) pure $ Fixes.mkRevision $ T.replicate 40 "b"
    let uid n = UUIDId $ UUID.fromWords 0 0 0 n
        result = Agent.FixResult{projectId = testPid, runId = uid 1, issueId = uid 2, attemptId = uid 3, candidateId = uid 4, base, head = candidateRevision, patchSha256 = T.replicate 64 "a", bundleSha256 = T.replicate 64 "b", rootCause = "Retry token dropped", checkId = uid 5, regressionSha256 = T.replicate 64 "c", summary = "Preserve retry tokens"}
        baseline = Agent.CheckReceipt{id = uid 6, attemptId = result.attemptId, checkId = result.checkId, side = Agent.Base, revision = Fixes.revisionText base, imageBuildId = (uid 8 :: UUIDId "image").toText, scriptSha256 = T.replicate 64 "d", regression = Just $ Agent.InjectedTest result.regressionSha256, status = Agent.Failed, exitCode = Just 1, durationMs = 1, cpus = 2, memoryMiB = 1024, logBytes = 1, logSha256 = T.replicate 64 "e", workspace = Nothing}
        candidate = baseline{Agent.id = uid 7, Agent.side = Agent.Head, Agent.revision = Fixes.revisionText candidateRevision, Agent.regression = Just $ Agent.RetainedTest result.regressionSha256, Agent.status = Agent.Passed, Agent.exitCode = Just 0}
        request = Agent.ArtifactRequest "https://github.com/fixture/missing.git" result baseline candidate
    proof <- runEff $ Agent.runCodeReviewReal Nothing (Just "test/fixtures/issue-fix-agent.mjs") [directory </> "failure.txt", toString baseline.imageBuildId, ""] $ Agent.verifyFixArtifacts (Agent.ArtifactStore directory directory) request
    isLeft proof `shouldBe` True

  it "checkout_cleanupWaitsForChildExit_withPinnedSourceAccess" \_tr -> withSystemTempDirectory "native-checkout" \temporary -> do
    directory <- canonicalizePath temporary
    let repository = directory </> "repository"
        store = Agent.ArtifactStore directory (directory </> "cache")
        launched = directory </> "worker.log"
        git args = T.strip . decodeUtf8 @Text <$> readProcessStdout_ (setWorkingDir repository $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "commit.gpgSign=false"] <> args)
    createDirectoryIfMissing True repository
    createDirectoryIfMissing True store.cacheDir
    void $ git ["init", "--quiet"]
    void $ git ["config", "user.name", "Checkout fixture"]
    void $ git ["config", "user.email", "fixture@example.invalid"]
    writeFileBS (repository </> "AGENTS.md") "Read actual callers before reviewing.\n"
    writeFileBS (directory </> "host-instructions.md") "Host-only instructions must not enter the prompt.\n"
    createFileLink (directory </> "host-instructions.md") (repository </> "AGENTS.override.md")
    for_ [".agents/skills/review", ".pi/skills/review", ".agents/skills/linked"] $ createDirectoryIfMissing True . (repository </>)
    writeFileBS (repository </> ".agents/skills/review/SKILL.md") "Committed skill\n"
    writeFileBS (repository </> ".pi/skills/review/reference.md") "Committed reference\n"
    writeFileBS (repository </> ".agents/skills/review/execute.sh") "must not load\n"
    createFileLink (repository </> "AGENTS.md") (repository </> ".agents/skills/linked/SKILL.md")
    void $ git ["add", "."]
    void $ git ["commit", "--quiet", "-m", "base"]
    base <- git ["rev-parse", "HEAD"]
    writeFileBS (repository </> "runtime.txt") "reviewed head\n"
    void $ git ["add", "."]
    void $ git ["commit", "--quiet", "-m", "head"]
    headRevision <- git ["rev-parse", "HEAD"]
    writeFileBS (repository </> "runtime.txt") "unreviewed later change\n"
    void $ git ["commit", "--quiet", "-am", "later"]
    createDirectoryIfMissing True $ repository </> ".agents/skills/untracked"
    writeFileBS (repository </> ".agents/skills/untracked/SKILL.md") "Uncommitted skill\n"
    recoveredCalls <- newIORef (0 :: Int)
    let request :: Agent.Request "pr_review"
        request = Agent.Request{projectId = testPid, runId = UUIDId $ UUID.fromWords 42 0 0 1, attemptId = UUIDId $ UUID.fromWords 42 0 0 2, repository = toText repository, base, head = headRevision, gitToken = "fixture-secret-token", prompt = "Review actual callers", provider = "fixture", model = "fixture", thinkingLevel = Agent.Off}
        decode = either (fail . toString) pure . AE.eitherDecodeStrict @AE.Value . encodeUtf8
        field key = AET.parseEither $ AE.withObject "Prepared checkout" (AE..: key)
        cleaned value = for_ ["directory", "repositorySkills"] $ \key -> either fail pure (field key value) >>= doesDirectoryExist >>= (`shouldBe` False)
        run input flags validate = runEff $ Agent.runCodeReviewReal (Just store) (Just "test/fixtures/native-checkout-agent.mjs") (launched : flags) $ Agent.reviewRepository input (const $ pure $ Left "Unexpected query") (const $ pure $ Left "Unexpected knowledge") (const $ liftIO $ if "--replay-call" `elem` flags then modifyIORef' recoveredCalls (+ 1) else fail "Unexpected model call") (const $ liftIO $ fail "Unexpected model usage") (const $ liftIO $ fail "Unexpected allocation") (const $ liftIO $ fail "Unexpected check") validate
        check input flags = run input flags (const $ pure $ Right ()) >>= either (fail . toString) decode
        withRun n input = input{Agent.runId = UUIDId $ UUID.fromWords 42 0 0 n} :: Agent.Request "pr_review"
    preparedFirst <- check request []
    field "cacheStatus" preparedFirst `shouldBe` Right ("miss" :: Text)
    field "head" preparedFirst `shouldBe` Right headRevision
    field "rootInstructions" preparedFirst `shouldBe` Right ("Read actual callers before reviewing.\n" :: Text)
    field "instructions" preparedFirst `shouldBe` Right ("Read actual callers before reviewing.\n" :: Text)
    policy <- either fail pure $ field "policy" preparedFirst
    instructions <- either fail pure $ field "instructions" policy
    (instructions :: Text) `shouldSatisfy` T.isInfixOf headRevision
    instructions `shouldSatisfy` T.isInfixOf "Read actual callers before reviewing."
    cleaned preparedFirst
    for_ ["protocol", "callback", "cancel"] \mode -> do
      ready <- MVar.newEmptyMVar
      blocked <- MVar.newEmptyMVar :: IO (MVar.MVar ())
      validated <- newIORef False
      let lifetime = directory </> "child-lifetime-" <> mode <> ".json"
          runLifetime =
            runEff
              $ Agent.runCodeReviewReal (Just store) (Just "test/fixtures/native-child-lifetime.py") [lifetime, mode]
              $ Agent.reviewRepository
                request
                (const $ liftIO (MVar.putMVar ready () >> MVar.takeMVar blocked) $> Right AE.Null)
                (const $ pure $ Left "Unexpected knowledge")
                (const $ liftIO $ fail "Unexpected model call")
                (const $ liftIO $ fail "Unexpected usage")
                (const $ liftIO $ fail "Unexpected allocation")
                (const $ liftIO $ fail "Unexpected check")
                (const $ liftIO $ writeIORef validated True >> throwIO (userError "Fixture validation failed"))
      if mode == "cancel"
        then withAsync runLifetime \worker -> do
          Timeout.timeout (10 * 1000000) (MVar.takeMVar ready) `shouldReturn` Just ()
          cancel worker
          waitCatch worker >>= (`shouldSatisfy` isLeft)
        else runLifetime >>= (`shouldSatisfy` isLeft)
      readIORef validated `shouldReturn` (mode == "callback")
      observedLifetime <- readFileBS lifetime >>= either fail pure . AE.eitherDecodeStrict @AE.Value
      traverse (`field` observedLifetime) ["checkoutPresent", "metadataPresent"] `shouldBe` Right [True, True]
      cleaned observedLifetime
    (preparedSecond, preparedThird) <- concurrently (check (withRun 2 request) []) (check (withRun 3 request) [])
    for_ [preparedSecond, preparedThird] \value -> do
      field "cacheStatus" value `shouldBe` Right ("hit" :: Text)
      cleaned value
    let isolated = request{Agent.projectId = UUIDId $ UUID.fromWords 42 0 0 3} :: Agent.Request "pr_review"
    (isolatedFirst, isolatedSecond) <- concurrently (check (withRun 4 isolated) []) (check (withRun 5 isolated) [])
    sort <$> traverse (either fail pure . field "cacheStatus") [isolatedFirst, isolatedSecond] `shouldReturn` (["hit", "miss"] :: [Text])
    let otherRepository = directory </> "other-repository"
    void $ git ["clone", "--quiet", "--no-local", "--", repository, otherRepository]
    different <- run (request{Agent.repository = toText otherRepository} :: Agent.Request "pr_review") [] (const $ pure $ Right ()) >>= either (fail . toString) decode
    field "cacheStatus" different `shouldBe` Right ("miss" :: Text)
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
    field "rootInstructions" overridden `shouldBe` Right (T.take 32768 $ T.replicate 3000 "Override rule.\n")
    cleaned overridden
    -- Later unrelated runs must not make a retained base disappear when a
    -- verifier clones the shared mirror through Git's file transport.
    void $ git ["checkout", "--quiet", "--orphan", "unrelated"]
    void $ git ["commit", "--quiet", "--allow-empty", "-m", "unrelated history"]
    unrelated <- git ["rev-parse", "HEAD"]
    run (request{Agent.base = unrelated, Agent.head = unrelated} :: Agent.Request "pr_review") ["--override-instructions"] (const $ pure $ Right ()) >>= (`shouldSatisfy` isRight)
    let mirror = store.cacheDir </> toString testPid.toText </> toString (show (hash (encodeUtf8 request.repository :: ByteString) :: Digest SHA256) :: Text)
        snapshot = directory </> "mirror-snapshot"
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

  it "journals native fix phases before delivery and replays rejected or interrupted writes" \_tr -> withSystemTempDirectory "native-phase-delivery" \directory -> do
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

  it "recovers durable receipts in Haskell before starting the worker and retains rejected usage" \_tr -> withSystemTempDirectory "native-receipts" \temporary -> do
    directory <- canonicalizePath temporary
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
            $ Agent.runCodeReviewReal (Just store) (Just "test/fixtures/native-checkout-agent.mjs") (marker : flags)
            $ Agent.fixRepository
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
        $ Agent.runCodeReviewReal (Just store) (Just "test/fixtures/native-checkout-agent.mjs") [marker]
        $ Agent.admitRegression
          (Agent.AdmissionRequest fixRequest $ Agent.TestArtifact "false" "test patch" "failure")
          (const $ pure $ Left "Unexpected memory request")
          (const $ liftIO $ modifyIORef' events (<> ["Forbidden admission replay"]))
          (const pass)
          (const pass)
    isLeft refused `shouldBe` True
    readIORef events `shouldReturn` []
    tryAny (readFileBS $ marker <> ".process") >>= (`shouldSatisfy` isLeft)

  it "verifies native artifacts, renders media and publishes with native tools without a worker" \tr -> withSystemTempDirectory "native-artifacts" \temporary -> do
    directory <- canonicalizePath temporary
    let repository = directory </> "repository"
        store = Agent.ArtifactStore (directory </> "usage") (directory </> "cache")
        remoteRepository = "https://github.com/impact-org/checkout.git" :: Text
        uid n = UUIDId $ UUID.fromWords 0 0 0 n
        sha :: ByteString -> Text
        sha bytes = show (hash bytes :: Digest SHA256) :: Text
        git args = toStrict <$> readProcessStdout_ (setWorkingDir repository $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "commit.gpgSign=false"] <> args)
        revisionAt = git ["rev-parse", "HEAD"] >>= either (fail . show) pure . Fixes.mkRevision . T.strip . decodeUtf8
        script = "bash regression.sh" :: ByteString
        test = "set -eu\n. ./runtime.sh\n[ \"$(retry_token original)\" = original ] || { echo \"retry token lost\"; exit 1; }\n" :: ByteString
        metadata result = case AE.toJSON result of AE.Object fields -> AE.Object $ foldr KM.delete fields ["checkId", "regressionSha256", "summary"]; value -> value
    createDirectoryIfMissing True repository
    void $ git ["init", "--quiet"]
    void $ git ["config", "user.name", "Artifact fixture"]
    void $ git ["config", "user.email", "fixture@example.invalid"]
    writeFileBS (repository </> "runtime.sh") "retry_token() { printf 'lost\\n'; }\n"
    void $ git ["add", "."]
    void $ git ["commit", "--quiet", "-m", "base"]
    base <- revisionAt
    let mirror = store.cacheDir </> toString testPid.toText </> toString (sha $ encodeUtf8 remoteRepository)
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
        result = Agent.FixResult{projectId = testPid, runId = uid 1, issueId = uid 2, attemptId = uid 3, candidateId, base, head = candidateRevision, patchSha256 = sha patch, bundleSha256 = sha bundle, rootCause = "Runtime drops retry token <code> --> & test", checkId = uid 5, regressionSha256 = sha frozen, summary = "Preserve retry tokens"}
        baselineLog = "retry token lost\n\nliteral <code> --> & test\n" <> BS.pack [0, 255] :: ByteString
        candidateLog = "regression-passed\n" :: ByteString
        baseline = Agent.CheckReceipt{id = uid 6, attemptId = result.attemptId, checkId = result.checkId, side = Agent.Base, revision = Fixes.revisionText base, imageBuildId = (uid 8 :: UUIDId "image").toText, scriptSha256 = sha script, regression = Just $ Agent.InjectedTest result.regressionSha256, status = Agent.Failed, exitCode = Just 1, durationMs = 10, cpus = 2, memoryMiB = 1024, logBytes = fromIntegral $ BS.length baselineLog, logSha256 = sha baselineLog, workspace = Nothing}
        candidate = baseline{Agent.id = uid 7, Agent.side = Agent.Head, Agent.revision = Fixes.revisionText candidateRevision, Agent.regression = Just $ Agent.RetainedTest result.regressionSha256, Agent.status = Agent.Passed, Agent.exitCode = Just 0, Agent.logBytes = fromIntegral $ BS.length candidateLog, Agent.logSha256 = sha candidateLog}
        request = Agent.ArtifactRequest remoteRepository result baseline candidate
        row (receipt :: Agent.CheckReceipt) disposition = AE.object ["id" AE..= receipt.id, "side" AE..= receipt.side, "revision" AE..= receipt.revision, "status" AE..= receipt.status, "exitCode" AE..= receipt.exitCode, "configuredResources" AE..= AE.object ["cpus" AE..= receipt.cpus, "memoryMiB" AE..= receipt.memoryMiB], "durationMs" AE..= (9.5 :: Double), "logBytes" AE..= receipt.logBytes, "logSha256" AE..= receipt.logSha256, "sourceBefore" AE..= sha "source", "sourceAfter" AE..= sha "source", "testFilesSha256" AE..= sha test, "testPatchDisposition" AE..= (disposition :: Text)]
        report = AE.object ["id" AE..= result.checkId, "projectId" AE..= testPid, "runId" AE..= result.runId, "attemptId" AE..= result.attemptId, "imageBuildId" AE..= baseline.imageBuildId, "scriptSha256" AE..= sha script, "testPatchSha256" AE..= sha testPatch, "regressionSha256" AE..= result.regressionSha256, "failureMessage" AE..= ("retry token lost" :: Text), "status" AE..= ("completed" :: Text), "results" AE..= ([row baseline "injected", row candidate "retained"] :: [AE.Value])]
        checks = store.usageDir </> "fixes" </> toString result.runId.toText </> "checks"
        captured = checks </> "candidates" </> toString candidateId.toText
        execution = checks </> toString result.checkId.toText
        files = [(captured </> "candidate.json", toStrict $ AE.encode $ metadata result), (captured </> "fix.patch", patch), (captured </> "candidate.bundle", bundle), (execution </> "report.json", toStrict $ AE.encode report), (execution </> "script.sh", script), (execution </> "test.patch", testPatch), (execution </> "base.log", baselineLog), (execution </> "head.log", candidateLog)]
        restore = traverse_ (uncurry writeFileBS) files
        verify input = runEff $ Agent.runCodeReviewReal Nothing Nothing [] $ Agent.verifyFixArtifacts store input
    createDirectoryIfMissing True captured
    createDirectoryIfMissing True execution
    restore
    fmap Agent.verifiedFixResult <$> verify request `shouldReturn` Right result
    snapshot <- Checkout.withCheckout store (Checkout.VerifiedCandidate request) (pure . AE.toJSON)
    prepared <- either (fail . toString) pure snapshot
    let evidenceField name = AE.withObject "Verified peer-review evidence" (AE..: name)
        reviewInput = AE.object ["result" AE..= result, "regression" AE..= Agent.TestArtifact (decodeUtf8 script) (decodeUtf8 testPatch) "retry token lost", "baseline" AE..= request.baseline, "candidate" AE..= request.candidate]
        peerFiles = Map.fromList [("/run/monoscope-fix-review.json", toStrict $ AE.encode reviewInput), ("/run/monoscope-fix-review-base.log", baselineLog), ("/run/monoscope-fix-review-head.log", candidateLog)] :: Map Text ByteString
    AET.parseMaybe (evidenceField "evidence" >=> evidenceField "contents" >=> evidenceField "result") prepared `shouldBe` Just (AE.toJSON result)
    AET.parseMaybe (evidenceField "evidence" >=> evidenceField "contents" >=> evidenceField "files") prepared `shouldBe` Just (Map.map (extractBase64 . B64.encodeBase64) peerFiles)
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
      writeFileBS (execution </> "report.json") $ toStrict $ AE.encode $ case report of AE.Object fields -> AE.Object $ KM.insert key value fields; other -> other
      isLeft <$> verify request `shouldReturn` True
      restore
    let (prefix, suffix) = BS.splitAt (BS.length bundle - 1) bundle
        corruptBundle = prefix <> BS.map (`xor` 1) suffix
    for_ [result{Agent.patchSha256 = sha testPatch}, result{Agent.bundleSha256 = sha corruptBundle}] \changed -> do
      writeFileBS (captured </> "candidate.json") $ toStrict $ AE.encode $ metadata changed
      writeFileBS (captured </> "fix.patch") $ if changed.patchSha256 /= result.patchSha256 then testPatch else patch
      writeFileBS (captured </> "candidate.bundle") $ if changed.bundleSha256 /= result.bundleSha256 then corruptBundle else bundle
      isLeft <$> verify request{Agent.result = changed} `shouldReturn` True
      restore
    void $ git ["bundle", "create", directory </> "full.bundle", ref]
    fullBundle <- readFileBS $ directory </> "full.bundle"
    let different = result{Agent.bundleSha256 = sha fullBundle}
    writeFileBS (captured </> "candidate.bundle") fullBundle
    writeFileLBS (captured </> "candidate.json") $ AE.encode $ metadata different
    isLeft <$> verify request{Agent.result = different} `shouldReturn` True
    restore
    differentWorkspace <- either fail pure $ AET.parseEither (AE.withObject "Candidate receipt" $ AE.parseJSON . AE.Object . KM.insert "workspace" (AE.String $ T.replicate 64 "f")) $ AE.toJSON candidate
    for_ [request{Agent.result = result{Agent.runId = uid 99}}, request{Agent.candidate = candidate{Agent.attemptId = uid 99}}, request{Agent.candidate = differentWorkspace}] \changed ->
      isLeft <$> verify changed `shouldReturn` True
    fmap Agent.verifiedFixResult <$> verify request `shouldReturn` Right result
    let render = runEff $ Agent.runCodeReviewReal Nothing Nothing [] $ runExceptT do
          verified <- ExceptT $ Agent.verifyFixArtifacts store request
          ExceptT $ Agent.explainVerifiedFix store request verified
    rendered <- render
    explained <- either (fail . toString) pure rendered
    let manifest = Agent.fixExplanation explained
        media = captured </> "explanations" </> toString manifest.source
    Agent.validExplanation result manifest `shouldBe` True
    for_ (Map.toList manifest.files) \(name, digest) -> do
      bytes <- Media.readExplanationMedia store.usageDir manifest name >>= either (fail . toString) pure
      sha bytes `shouldBe` digest
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
    void $ withResource tr.trPool \conn -> execute conn "INSERT INTO apis.issues (id, project_id, issue_type, service, target_hash, title) VALUES (?, ?, 'runtime_exception', 'test-service', 'native-publication', 'Retain retry tokens')" (result.issueId, testPid)
    runQueryEffect tr (Fixes.createRun testPid result.runId result.issueId mapping.id base (uid 8)) `shouldReturn` Just result.runId
    for_ (zip [0 ..] [Fixes.Observe baseline{Agent.id = uid 10, Agent.checkId = uid 11}, Fixes.ProposeCandidate (Fixes.Candidate result.head result.rootCause), Fixes.Observe baseline, Fixes.Observe candidate]) \(version, step) -> do
      progressed <- runQueryEffect tr $ Fixes.recordStep testPid result.runId version (uid $ 20 + fromIntegral version) step
      progressed `shouldSatisfy` isRight
    runQueryEffect tr (Fixes.queueJob testPid user result.runId) `shouldReturn` True
    peer <- maybe (fail "Native publication review attempt missing") pure =<< runQueryEffect tr (Usage.startAttempt testPid (Usage.Fix result.runId) "fixture" "native-publication")
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.issue_fix_jobs SET result = ?, verified_result = ?, investigation = '{}'::jsonb, checkpoint_investigation = '{}'::jsonb, explanation = ?, media_retained_at = now(), review_attempt_id = ?, review_rationale = 'Fixture independent approval' WHERE run_id = ?" (Aeson result, Aeson result, Aeson cacheRepair, peer, result.runId)
    completed <- maybe (fail "Native publication capability missing") pure =<< runQueryEffect tr (Fixes.publicationForUser testPid result.runId user)
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
        readState = readFileBS stateFile >>= either fail pure . AE.eitherDecodeStrict @AE.Value
        field name = AE.withObject "GitHub fixture" (AE..: name)
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
        writes = readState >>= either fail pure . AET.parseEither (field "writes")
        setMode mode = do
          remote <- readState
          writeFileLBS stateFile $ AE.encode $ case remote of AE.Object fields -> AE.Object $ KM.insert "mode" (AE.String mode) fields; value -> value
    copyFile "test/fixtures/native-publication-gh.py" github
    Posix.setFileMode github 0o700
    writeFileLBS stateFile $ AE.encode initial
    published <- runPublisher authorize validate
    fmap (\proof -> (Publication.publicationReceipt proof).head) published `shouldBe` Right result.head
    reverse <$> readIORef stages `shouldReturn` [Publication.Prepare, Publication.Discover, Publication.Branch, Publication.Create, Publication.AttachVideo, Publication.AttachDiagram, Publication.Complete]
    writes `shouldReturn` (["create", "explanation.mp4", "cause.png"] :: [Text])
    again <- runPublisher authorize validate
    fmap Publication.publicationReceipt again `shouldBe` fmap Publication.publicationReceipt published
    writes `shouldReturn` (["create", "explanation.mp4", "cause.png"] :: [Text])
    failedAcceptance <- tryAny $ runPublisher authorize (\_ -> throwIO $ userError "Fixture acceptance failure")
    isRight failedAcceptance `shouldBe` True
    either (const Nothing) leftToMaybe failedAcceptance `shouldBe` Just Publication.ProtocolFailure
    writeIORef changedRemoteBody True
    unconfirmed <- runPublisher authorize (\_ -> fail "Edited remote explanation reached application acceptance")
    leftToMaybe unconfirmed `shouldBe` Just Publication.InvalidReceipt
    writeIORef changedRemoteBody False
    for_ ([("visibility", Publication.VisibilityChanged), ("denied", Publication.AccessRevoked)] :: [(Text, Publication.Failure)]) \(mode, failure) -> do
      setMode mode
      rejected <- runPublisher authorize validate
      leftToMaybe rejected `shouldBe` Just failure
    setMode "fork"
    leftToMaybe <$> runPublisher authorize validate `shouldReturn` Just Publication.PullRequestChanged
    setMode "merged"
    fmap ((.state) . Publication.publicationReceipt) <$> runPublisher authorize validate `shouldReturn` Right Publication.Closed
    setMode ""
    denied <- runPublisher (const $ pure $ Left Publication.AccessRevoked) validate
    leftToMaybe denied `shouldBe` Just Publication.AccessRevoked
    void $ withResource tr.trPool \conn -> execute conn "UPDATE users.users SET active = false WHERE id = ?" (Only user)
    revoked <- runPublisher authorize validate
    leftToMaybe revoked `shouldBe` Just Publication.AccessRevoked
    void $ withResource tr.trPool \conn -> execute conn "UPDATE users.users SET active = true WHERE id = ?" (Only user)
    removePathForcibly $ store.usageDir </> "fixes" </> toString result.runId.toText </> "publications-native"
    writeFileLBS stateFile $ AE.encode initial
    setMode "moved"
    moved <- runPublisher authorize validate
    leftToMaybe moved `shouldBe` Just Publication.SourceChanged
    writes `shouldReturn` ([] :: [Text])
    for_ ["after-create", "after-attachment"] \mode -> do
      removePathForcibly $ store.usageDir </> "fixes" </> toString result.runId.toText </> "publications-native"
      writeFileLBS stateFile $ AE.encode initial
      setMode mode
      leftToMaybe <$> runPublisher authorize validate `shouldReturn` Just Publication.GitHubFailure
      setMode ""
      recovered <- runPublisher authorize validate
      fmap Publication.publicationReceipt recovered `shouldBe` fmap Publication.publicationReceipt published
      writes `shouldReturn` (["create", "explanation.mp4", "cause.png"] :: [Text])
    removePathForcibly $ store.usageDir </> "fixes" </> toString result.runId.toText </> "publications-native"
    writeFileLBS stateFile $ AE.encode initial
    let revokeDiagram stage = if stage == Publication.AttachDiagram then pure $ Left Publication.AccessRevoked else authorize stage
    leftToMaybe <$> runPublisher revokeDiagram validate `shouldReturn` Just Publication.AccessRevoked
    writes `shouldReturn` (["create", "explanation.mp4"] :: [Text])
    isRight <$> runPublisher authorize validate `shouldReturn` True
    writes `shouldReturn` (["create", "explanation.mp4", "cause.png"] :: [Text])
    -- An interrupted upload is journaled before the CLI command; it is never
    -- blindly retried when the remote body has no matching uploaded asset.
    removePathForcibly $ store.usageDir </> "fixes" </> toString result.runId.toText </> "publications-native"
    writeFileLBS stateFile $ AE.encode initial
    setMode "uncertain"
    interrupted <- runPublisher authorize validate
    leftToMaybe interrupted `shouldBe` Just Publication.GitHubFailure
    setMode ""
    uncertain <- runPublisher authorize validate
    leftToMaybe uncertain `shouldBe` Just Publication.UploadUncertain
    writes `shouldReturn` (["create"] :: [Text])
    -- Corrupt media is rejected before creating or updating remote resources.
    writeFileBS (media </> "explanation.mp4") "corrupted video"
    corrupt <- runPublisher authorize validate
    leftToMaybe corrupt `shouldBe` Just Publication.SourceChanged

  it "queues signed source-repository events once, rejects unverified events, and retains the latest revision" \tr -> do
    setup tr
    runQueryEffect tr (Reviews.repositorySettings testPid) >>= \settings -> map (.includeEvidence) settings `shouldBe` [False]
    let firstEvent = payload "opened" revision "2025-01-01T00:00:00Z"
        next = T.replicate 40 "b"
    deliver tr firstEvent `shouldReturn` AE.object ["status" AE..= ("ok" :: Text)]
    deliver tr firstEvent `shouldReturn` AE.object ["status" AE..= ("ok" :: Text)]
    for_ @[] [AE.object [], payload "edited" revision "2025-01-01T00:00:00Z"] \ignored ->
      deliver tr ignored `shouldReturn` AE.object ["status" AE..= ("ignored" :: Text)]
    queued <- withResource tr.trPool \conn -> query conn "SELECT count(*) FROM background_jobs WHERE payload->>'tag' = 'ReviewPullRequest'" () :: IO [Only Int]
    queued `shouldBe` [Only 1]
    [old] <- runQueryEffect tr $ Reviews.latestRuns testPid
    runQueryEffect tr (Reviews.claimRun old) `shouldReturn` True
    runQueryEffect tr (Reviews.currentRun old) `shouldReturn` True
    leaseCoversReview <- withResource tr.trPool \conn -> query conn "SELECT lease_until > now() + interval '9 minutes' FROM projects.pr_review_threads WHERE id = ?" (Only old.threadId) :: IO [Only Bool]
    leaseCoversReview `shouldBe` [Only True]
    queuedPayload <- withResource tr.trPool \conn -> query conn "SELECT payload FROM background_jobs WHERE payload->>'tag' = 'ReviewPullRequest'" () :: IO [Only (Aeson AE.Value)]
    queuedPayload `shouldBe` [Only $ Aeson $ AE.toJSON $ Jobs.ReviewPullRequest old.id]
    (_, ()) <- runTestBgRecordingHTTP frozenTime tr $ Impact.reviewPullRequest old.id
    void $ deliver tr $ payload "synchronize" next "2025-01-01T00:01:00Z"
    void $ deliver tr firstEvent
    runQueryEffect tr (Reviews.currentRun old) `shouldReturn` False
    runs <- runQueryEffect tr $ Reviews.latestRuns testPid
    map (.latestRevision) runs `shouldBe` [next, next]
    new <- maybe (fail "new revision missing") pure $ find ((== next) . (.revision)) runs
    runQueryEffect tr (Reviews.claimRun new) `shouldReturn` False
    deferred <- withResource tr.trPool \conn -> query conn "SELECT payload FROM background_jobs WHERE run_at > now() AND payload->>'tag' = 'ReviewPullRequest'" () :: IO [Only (Aeson AE.Value)]
    deferred `shouldBe` [Only $ Aeson $ AE.toJSON $ Jobs.ReviewPullRequest new.id]
    runQueryEffect tr $ Reviews.releaseRun old
    runQueryEffect tr (Reviews.claimRun new) `shouldReturn` True
    runQueryEffect tr (Reviews.currentRun new) `shouldReturn` True
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.pr_review_threads SET lease_until = now() - interval '1 second' WHERE id = ?" (Only new.threadId)
    runQueryEffect tr (Reviews.currentRun new) `shouldReturn` False
    runQueryEffect tr (Reviews.claimRun new) `shouldReturn` True
    void $ deliver tr $ payload "converted_to_draft" next "2025-01-01T00:01:30Z"
    runQueryEffect tr (Reviews.currentRun new) `shouldReturn` False
    void $ deliver tr $ payload "closed" next "2025-01-01T00:02:00Z"
    runQueryEffect tr (Reviews.currentRun new) `shouldReturn` False
    void $ deliver tr $ payload "reopened" next "2025-01-01T00:03:00Z"
    reopened <- maybe (fail "run missing") pure =<< runQueryEffect tr (Reviews.getRun new.id)
    reopened.state `shouldBe` Reviews.Queued
    runQueryEffect tr $ Reviews.releaseRun new
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
    runQueryEffect tr $ Reviews.releaseRun otherRun
    released <- maybe (fail "run missing") pure =<< runQueryEffect tr (Reviews.getRun otherRun.id)
    released.state `shouldBe` Reviews.Incomplete

  it "keeps started calls attributable until matching usage arrives, including after billing changes" \tr -> do
    setup tr
    void $ deliver tr $ payload "opened" revision "2025-01-01T00:00:00Z"
    [run] <- runQueryEffect tr $ Reviews.latestRuns testPid
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.projects SET billing_provider = ?, customer_id = 'cus_call_start' WHERE id = ?" (Projects.StripeProvider, testPid)
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
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.projects SET customer_id = 'cus_after_call' WHERE id = ?" (Only testPid)
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Review run.id) received{Agent.callId = Nothing}) `shouldReturn` False
    for_ [received, received] \value -> runQueryEffect tr (Usage.recordUsage testPid (Usage.Review run.id) value) `shouldReturn` True
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Review run.id) received{Agent.id = uid 993}) `shouldReturn` False
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Review run.id) received{Agent.callId = Just $ uid 999}) `shouldReturn` False
    runQueryEffect tr (Usage.recordCall testPid (Usage.Review run.id) call{Agent.id = uid 994}) `shouldReturn` True
    reconciled <- maybe (fail "Reconciled usage missing") pure . summary =<< totals
    (reconciled.calls, reconciled.unreportedCalls, reconciled.unpricedCalls, reconciled.costNanousd) `shouldBe` (2, 1, 1, 42)
    invalid <- tryAny $ withResource tr.trPool \conn -> execute conn "INSERT INTO projects.pr_review_usage (id, run_id, project_id, billing_provider, receipt, model_call_id) VALUES (?, ?, ?, ?, ?, ?)" (uid 998 :: UUIDId "model_usage", run.id, testPid, Projects.StripeProvider, Aeson $ AE.toJSON received, uid 994 :: UUIDId "model_call")
    isLeft invalid `shouldBe` True
    for_ (leftToMaybe invalid) \failure -> show @Text failure `shouldSatisfy` T.isInfixOf "agent_model_usage_requires_attempt"
    rows <- runQueryEffect tr $ Usage.customerUsage (getResponse tr.trSessAndHeader).user.id
    fmap (.customerId) (find ((== testPid) . (.projectId)) rows) `shouldBe` Just (Just "cus_call_start")
    (_, response) <- testServant tr $ CodeContextPage.repositoriesGetH testPid (Just CodeContextPage.Usage) Nothing
    let html = decodeUtf8 @Text $ Lucid.renderBS response
    html `shouldSatisfy` T.isInfixOf "calls without usage: 1 (cost unknown)"
    legacyAttempt <- maybe (fail "Legacy attempt missing") pure =<< runQueryEffect tr (Usage.startAttempt testPid (Usage.Review run.id) "fixture" "fixture-model")
    let legacy = received{Agent.id = uid 995, Agent.attemptId = legacyAttempt, Agent.callId = Nothing}
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Review run.id) legacy) `shouldReturn` True
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.pr_review_usage SET receipt = receipt - 'callId' WHERE id = ?" (Only legacy.id)
    runQueryEffect tr (Usage.recordCall testPid (Usage.Review run.id) call{Agent.id = uid 996, Agent.attemptId = legacyAttempt}) `shouldReturn` True
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Review run.id) legacy) `shouldReturn` True
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Review run.id) legacy{Agent.id = uid 997}) `shouldReturn` False

  it "runs the checkout harness, publishes shared PNG evidence, and honors repository disable and links-only settings" \tr -> do
    parseUrlPiece @CodeContextPage.RepositoryTab "usage" `shouldSatisfy` isRight
    reviewPid <- createTestProject tr "Impact review isolation"
    setupProject tr reviewPid
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.projects SET customer_id = 'cus_impact_usage', billing_provider = ? WHERE id = ?" (Projects.StripeProvider, reviewPid)
    runQueryEffect tr $ Reviews.updateSettings reviewPid (Reviews.ReviewSettings "impact-org" "checkout" True True)
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
    -- Throwaway RSA fixture generated only for these JWT tests; never registered with a GitHub App.
    pem <- readFileBS "test/fixtures/github-app-test.pem"
    let cfg = tr.trATCtx.config{githubAppId = "123", githubAppPrivateKey = extractBase64 $ B64.encodeBase64 pem, impactReviewUsageDir = Just "fixture-usage", impactReviewCacheDir = Just "fixture-cache", impactReviewProvider = "fixture", openaiModel = "gpt-5.6-luna#high"}
        pr = AE.object ["head" AE..= AE.object ["sha" AE..= revision], "base" AE..= AE.object ["sha" AE..= T.replicate 40 "d"], "state" AE..= ("open" :: Text), "draft" AE..= False, "changed_files" AE..= length files, "updated_at" AE..= ("2025-01-01T00:00:00Z" :: Text)]
        files :: [Git.PullRequestFile]
        files = [Git.PullRequestFile ("unrelated-" <> show n <> ".hs") "modified" (Just "@@ -1 +1 @@\n-old\n+new") | n <- [1 :: Int .. 100]] <> [Git.PullRequestFile "checkout.hs" "modified" (Just "@@ -1 +1 @@\n-old\n+new")]
        marker = "<!-- monoscope-impact:" <> reviewPid.toText <> " -->"
        transport :: Bool -> ATBackgroundCtx a -> ATBackgroundCtx a
        transport hasComment = withHTTPResponses \_ endpoint ->
          pure
            $ Just
            $ AE.encode
            $ if "/access_tokens" `T.isSuffixOf` toText endpoint
              then AE.object ["token" AE..= ("fixture-token" :: Text), "expires_at" AE..= ("2030-01-01T00:00:00Z" :: Text)]
              else
                if "/files?per_page=100" `T.isInfixOf` toText endpoint
                  then AE.toJSON $ if "&page=2" `T.isSuffixOf` toText endpoint then drop 100 files else take 100 files
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
        result = Impact.ReviewResult Impact.WorthChecking [Impact.Finding (Impact.ChangedLine "checkout.hs" 1 Impact.Head) "The changed operation may stop emitting the signal used by the monitor. *check* @impact-review-test [outside](https://untrusted.invalid)\r#heading" "Check the monitor query before deploying." ["telemetry-1"]] [] [] frozenTime frozenTime Nothing
        fakeAgent :: Impact.ReviewResult -> ATBackgroundCtx a -> ATBackgroundCtx a
        fakeAgent = fakeAgentWith []
        fakeAgentWith :: [FilePath] -> Impact.ReviewResult -> ATBackgroundCtx a -> ATBackgroundCtx a
        fakeAgentWith arguments answer action =
          interpose @LLM.LLM (\_ _ -> liftIO $ fail "Legacy LLM loop called instead of checkout harness")
            $ interpose @Agent.CodeReview
              ( \env ->
                  \case
                    Agent.ReviewRepository request respond knowledge recordCall recordUsage recordAllocation recordCheck check -> localSeqUnliftIO env \unlift ->
                      liftIO $ withSystemTempDirectory "impact-review-response" \directory -> do
                        started <- withResource tr.trPool \conn -> query conn "SELECT count(*) FROM projects.pr_review_attempts WHERE id = ? AND run_id = ? AND project_id = ? AND provider = ?" (request.attemptId, request.runId, request.projectId, request.provider) :: IO [Only Int64]
                        started `shouldBe` [Only 1]
                        when ("--billing-change-before-usage" `elem` arguments) $ void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.projects SET customer_id = 'cus_after_attempt', billing_provider = ? WHERE id = ?" (Projects.LemonSqueezyProvider, reviewPid)
                        when (null arguments) do
                          title <- either (fail . toString) pure $ Memory.mkMemoryText "Review context"
                          body <- either (fail . toString) pure $ Memory.mkMemoryText "Retry handling needs caller and telemetry evidence; recheck this dated note."
                          let page = case request.attemptId of UUIDId value -> UUIDId value
                          saved <- unlift $ knowledge $ Agent.ProjectMemory $ Memory.Write $ Memory.Edit page 0 $ Memory.Document title body
                          saved `shouldSatisfy` isRight
                          unlift (knowledge $ Agent.ProjectMemory $ Memory.Read page) `shouldReturn` saved
                        let response = directory </> "result.json"
                        writeFileLBS response $ AE.encode answer{Impact.coverage = ["Related test toolchain unavailable"]}
                        reply <- runEff $ Agent.runCodeReviewReal Nothing (Just "test/fixtures/impact-review-agent.mjs") ([response, toString reviewPid.toText] <> arguments) $ Agent.reviewRepository request (liftIO . unlift . respond) (liftIO . unlift . knowledge) (liftIO . unlift . recordCall) (liftIO . unlift . recordUsage) (liftIO . unlift . recordAllocation) (liftIO . unlift . recordCheck) (liftIO . unlift . check)
                        diagnostic <- tryAny $ decodeUtf8 <$> readFileBS (response <> ".failure")
                        pure $ first (<> either (const "") ("\n" <>) diagnostic) reply
                    Agent.FixRepository{} -> pure $ Left "Unexpected issue-fix task"
                    Agent.AdmitRegression{} -> pure $ Left "Unexpected admission task"
                    Agent.VerifyFixArtifacts{} -> pure $ Left "Unexpected artifact task"
                    Agent.ExplainVerifiedFix{} -> pure $ Left "Unexpected explanation task"
                    Agent.ReviewVerifiedFix{} -> pure $ Left "Unexpected candidate review task"
              )
              action
    let queryResult = result{Impact.evidence = [Impact.Evidence "telemetry-1" Impact.Monitor Nothing Nothing "/monitor" (Just "private-customer-query") Nothing Nothing Nothing]}
    for_ @[] [False, True] \includeEvidence -> Impact.renderReview cfg.hostUrl run includeEvidence queryResult `shouldNotSatisfy` T.isInfixOf "private-customer-query"
    (requests, ()) <- runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = cfg}) $ transport True $ fakeAgent result $ Impact.reviewPullRequest run.id
    requests `shouldNotSatisfy` any (T.isInfixOf "/contents/" . fst)
    requests `shouldSatisfy` any (T.isInfixOf "/files?per_page=100&page=2" . fst)
    let comments = filter (T.isInfixOf "/issues/comments/99" . fst) requests
    length comments `shouldBe` 1
    let published = T.intercalate " " $ map (decodeUtf8 . snd) comments
    published `shouldSatisfy` T.isInfixOf "Worth checking"
    published `shouldSatisfy` T.isInfixOf "Related test toolchain unavailable"
    published `shouldSatisfy` T.isInfixOf "production query reached its result limit"
    requests `shouldNotSatisfy` any (T.isInfixOf "/contents/.." . fst)
    published `shouldSatisfy` T.isInfixOf "Observed"
    published `shouldSatisfy` T.isInfixOf "/widget.png?widgetZ="
    published `shouldSatisfy` T.isInfixOf "![Review query results]"
    published `shouldSatisfy` T.isInfixOf "\\\\*check"
    published `shouldNotSatisfy` T.isInfixOf "@impact-review-test"
    published `shouldNotSatisfy` T.isInfixOf "https://untrusted.invalid"
    published `shouldNotSatisfy` T.isInfixOf "long-review-log-body"
    published `shouldNotSatisfy` T.isInfixOf "another-in-scope-event"
    published `shouldNotSatisfy` T.isInfixOf "checkout-review-span"
    published `shouldNotSatisfy` T.isInfixOf "Review temperature gauge"
    published `shouldSatisfy` T.isInfixOf revision
    completed <- maybe (fail "run missing") pure =<< runQueryEffect tr (Reviews.getRun run.id)
    completed.state `shouldBe` Reviews.Completed
    completed.commentId `shouldBe` Just 99
    checks <- runQueryEffect tr $ Reviews.latestChecks reviewPid
    length checks `shouldBe` 2
    Reviews.RecordedCheck checkRun (Aeson execution) <- maybe (fail "Base execution missing") pure $ find (\item -> case item.receipt of Aeson receipt -> receipt.revision == T.replicate 40 "d") checks
    checkRun `shouldBe` run.id
    retained <- runQueryEffect tr $ Reviews.priorKnowledge run "test-service"
    length retained `shouldBe` 1
    map (.revision) retained `shouldBe` [revision]
    map (.evidenceKeys) retained `shouldBe` [V.singleton "query-6"]
    map (.windowEnd) retained `shouldBe` [frozenTime]
    [Only (Aeson snapshot)] <- withResource tr.trPool (\conn -> query conn "SELECT evidence FROM projects.pr_review_knowledge WHERE run_id = ?" (Only run.id)) :: IO [Only (Aeson AE.Value)]
    decodeUtf8 @Text (AE.encode snapshot) `shouldSatisfy` T.isInfixOf "review.temperature"
    decodeUtf8 @Text (AE.encode snapshot) `shouldNotSatisfy` T.isInfixOf "dataset"
    runQueryEffect tr (Reviews.priorKnowledge run{Reviews.repo = "another-repository"} "test-service") >>= (`shouldSatisfy` null)
    execution.durationMs `shouldBe` 1250
    execution.revision `shouldBe` T.replicate 40 "d"
    AE.toJSON execution.status `shouldBe` AE.String "failed"
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.pr_review_checks SET receipt = receipt - 'regression' - 'workspace' WHERE id = ?" (Only execution.id)
    runQueryEffect tr (Reviews.recordCheck run execution{Agent.regression = Nothing}) `shouldReturn` True
    runQueryEffect tr (Reviews.recordCheck run execution{Agent.regression = Just $ Agent.RetainedTest $ T.replicate 64 "a"}) `shouldReturn` False
    runQueryEffect tr (Reviews.recordCheck run execution{Agent.durationMs = 9999}) `shouldReturn` False
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
    otherTotals <- runQueryEffect tr $ Reviews.usageTotals emptyPid
    sum (map (.usage.calls) otherTotals) `shouldBe` 0
    (_, history) <- testServant tr $ CodeContextPage.repositoriesGetH reviewPid (Just CodeContextPage.PullRequests) Nothing
    let historyText = decodeUtf8 $ Lucid.renderBS history
    historyText `shouldSatisfy` T.isInfixOf "$0.001234 estimated"
    historyText `shouldSatisfy` T.isInfixOf "1 unpriced"
    historyText `shouldSatisfy` T.isInfixOf "not customer charges"
    historyText `shouldSatisfy` T.isInfixOf "Base: assertion failed"
    historyText `shouldSatisfy` T.isInfixOf "Head: passed"
    T.count ("Check " <> T.take 8 execution.checkId.toText) historyText `shouldBe` 1
    historyText `shouldSatisfy` T.isInfixOf "1250 ms"
    historyText `shouldSatisfy` T.isInfixOf "Compute cost is unpriced"
    historyText `shouldSatisfy` T.isInfixOf "Service knowledge from this review"
    historyText `shouldSatisfy` T.isInfixOf "Review temperature gauge"
    billable <- withResource tr.trPool \conn -> query conn "SELECT count(*)::bigint FROM projects.ai_usage WHERE project_id = ?" (Only reviewPid) :: IO [Only Int64]
    billable `shouldBe` [Only 0]
    attributed <- withResource tr.trPool \conn -> query conn "SELECT count(*)::bigint FROM projects.pr_review_usage u JOIN projects.projects p ON p.id = u.project_id WHERE u.project_id = ? AND u.customer_id = 'cus_impact_usage' AND u.customer_id = p.customer_id AND u.billing_provider = p.billing_provider AND (u.receipt->>'inputTokens')::bigint = 12 AND (u.receipt->>'cacheReadTokens')::bigint = 13" (Only reviewPid) :: IO [Only Int64]
    attributed `shouldBe` [Only 2]
    checkAttribution <- withResource tr.trPool \conn -> query conn "SELECT count(*)::bigint FROM projects.pr_review_checks WHERE project_id = ? AND customer_id = 'cus_impact_usage'" (Only reviewPid) :: IO [Only Int64]
    checkAttribution `shouldBe` [Only 2]
    let customerProjects = [("Included customer project", Just "cus_impact_usage"), ("Viewer customer project", Just "cus_impact_usage"), ("Inactive customer project", Just "cus_impact_usage"), ("Unlinked one", Nothing), ("Unlinked two", Just "")] :: [(Text, Maybe Text)]
    related <- forM (zip [1 :: Word32 ..] customerProjects) \(n, (title, cid)) -> do
      pid <- createTestProject tr title
      void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.projects SET customer_id = ?, billing_provider = ? WHERE id = ?" (cid, Projects.StripeProvider, pid)
      credential <- maybe (fail "credential missing") pure =<< runQueryEffect tr (GitSync.upsertGitHubCredential (encodeUtf8 cfg.apiKeyEncryptionSecretKey) pid Git.GitHub Nothing "impact-org" (Just 987654) Nothing)
      runQueryEffect tr $ CodeContext.insertCodeMapping pid credential.id (Git.RepoRef "impact-org" "checkout" "main") Nothing "" ""
      pure (pid, n)
    void $ deliver tr $ payload "opened" revision "2025-01-01T00:00:00Z"
    [Only (Aeson receipt)] <- withResource tr.trPool (\conn -> query conn "SELECT receipt FROM projects.pr_review_usage WHERE project_id = ? AND cost_nanousd IS NOT NULL LIMIT 1" (Only reviewPid)) :: IO [Only (Aeson Agent.ModelUsage)]
    for_ related \(pid, n) -> do
      [relatedRun] <- runQueryEffect tr $ Reviews.latestRuns pid
      let observed = receipt{Agent.id = UUIDId $ UUID.fromWords 42 0 0 n, Agent.attemptId = UUIDId $ UUID.fromWords 43 0 0 n, Agent.responseId = Just $ "response-" <> show n}
      runQueryEffect tr (Reviews.recordUsage relatedRun observed) `shouldReturn` True
      when (n == 5) do
        void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.projects SET billing_provider = ? WHERE id = ?" (Projects.LemonSqueezyProvider, pid)
        runQueryEffect tr (Reviews.recordUsage relatedRun observed{Agent.id = UUIDId $ UUID.fromWords 44 0 0 n, Agent.responseId = Just "second-response"}) `shouldReturn` True

      when (n == 2) $ void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.project_members SET permission = ? WHERE project_id = ?" (ProjectMembers.PView, pid)
      when (n == 3) $ void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.project_members SET active = false WHERE project_id = ?" (Only pid)
      when (n == 1) do
        [credential] <- runQueryEffect tr $ GitSync.getGitHubCredentials pid
        runQueryEffect tr $ CodeContext.insertCodeMapping pid credential.id (Git.RepoRef "impact-org" "noisy" "main") Nothing "" ""
        event <- either (fail . show) pure $ Reviews.decodeEvent $ toStrict $ AE.encode $ payload "opened" revision "2025-01-01T00:00:00Z"
        runQueryEffect tr $ for_ [8 .. 28] \number -> Reviews.receiveEvent (event{Reviews.number = number, Reviews.repo = "noisy"} :: Reviews.PullRequestEvent)
    runQueryEffect tr (Reviews.latestChecks reviewPid) >>= (\executions -> length executions `shouldBe` 2)
    let userId = (getResponse tr.trSessAndHeader :: Projects.Session).user.id
    customerRows <- runQueryEffect tr $ Reviews.customerUsage userId
    length customerRows `shouldBe` 5
    sum (map (.usage.calls) customerRows) `shouldBe` 6
    sum (map (.resources.vcpuMs) customerRows) `shouldBe` 9000
    void $ withResource tr.trPool \conn -> execute conn "UPDATE users.users SET active = false WHERE id = ?" (Only userId)
    runQueryEffect tr (Reviews.customerUsage userId) >>= (\rows -> length rows `shouldBe` 0)
    void $ withResource tr.trPool \conn -> execute conn "UPDATE users.users SET active = true WHERE id = ?" (Only userId)
    usageTab <- either (fail . toString) pure $ parseUrlPiece @CodeContextPage.RepositoryTab "usage"
    (_, usagePage) <- testServant tr $ CodeContextPage.repositoriesGetH reviewPid (Just usageTab) Nothing
    let usagePageText = decodeUtf8 $ Lucid.renderBS usagePage
    usagePageText `shouldSatisfy` T.isInfixOf "$0.002468 estimated"
    usagePageText `shouldSatisfy` T.isInfixOf "active administrator"
    usagePageText `shouldSatisfy` T.isInfixOf "attributed on receipt (no attempt snapshot)"
    T.count "No billing customer linked" usagePageText `shouldBe` 2
    for_ related \(pid, n) -> T.count ("href=\"/p/" <> pid.toText <> "/repositories?tab=reviews\"") usagePageText `shouldBe` if n `elem` [2, 3] then 0 else 1

    saved <- maybe (fail "review result missing") (either fail pure . AET.parseEither AE.parseJSON) completed.result
    let measured = filter ((== Impact.Telemetry) . (.kind)) (saved :: Impact.ReviewResult).evidence
    for_ measured \item -> whenJust item.chartUrl \chart -> do
      let compressed = T.takeWhile (/= '&') $ T.drop 8 $ snd $ T.breakOn "widgetZ=" chart
      widgetJSON <- maybe (fail "invalid evidence widget") pure =<< Widget.decodeWidgetZ compressed
      widget <- either fail pure $ AE.eitherDecodeStrict @Widget.Widget $ encodeUtf8 widgetJSON
      if item.key == "telemetry-1"
        then do
          widget.query `shouldSatisfy` maybe False (T.isInfixOf "timestamp >= \"2024-12-25T00:00:00Z\"")
          widget.query `shouldSatisfy` maybe False (T.isInfixOf "timestamp <= \"2025-01-01T00:00:00Z\"")
        else do
          widget.query `shouldBe` Nothing
          widget.dataset `shouldSatisfy` isJust
          widget.pngProfile `shouldBe` Just Widget.PngSlack
          item.url `shouldBe` T.dropWhileEnd (== '/') cfg.hostUrl <> "/p/" <> reviewPid.toText <> "/settings/code-mappings"
      when (item.key == "query-1") do
        pngs <- forM [2, 3 :: Int] \count -> do
          let scalar = widget{Widget.dataset = Just (def :: Widget.WidgetDataset){Widget.source = AE.toJSON ([[AE.String "events"], [AE.toJSON count]] :: [[AE.Value]])}}
              encoded = decodeUtf8 @Text $ AE.encode scalar
              signature = Widget.signWidgetUrl tr.trATCtx.env.apiKeyEncryptionSecretKey reviewPid encoded
          response <- toBaseServantResponse tr $ Routes.widgetPngGetH reviewPid (Just encoded) Nothing Nothing Nothing Nothing Nothing Nothing (Just signature) []
          let bytes = getResponse response
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
          (def{Widget.wType = Widget.WTTimeseries, Widget.title = Just "Observed service events", Widget.query = Just "resource.service.name == \"test-service\" and timestamp >= \"2024-12-25T00:00:00Z\" AND timestamp <= \"2025-01-01T00:00:00Z\" | summarize events = count() by bin_auto(timestamp)", Widget.pngProfile = Just Widget.PngSlack})
          Nothing
          (Just "2024-12-25T00:00:00Z")
          (Just "2025-01-01T00:00:00Z")
    map (.chartUrl) (filter (isNothing . (.queryResult)) measured) `shouldBe` [Just expectedChart]
    length (filter (isJust . (.queryResult)) measured) `shouldBe` 7
    -- Reruns recover the app-owned comment and keep production prose out of links-only output.
    void $ testServant tr $ CodeContextPage.impactReviewSettingsPostH reviewPid (Reviews.ReviewSettings "impact-org" "checkout" True False)
    void $ testServant tr $ CodeContextPage.impactReviewRetryH reviewPid run.id
    retryPayloads <- withResource tr.trPool \conn -> query conn "SELECT payload FROM background_jobs WHERE payload->>'tag' = 'ReviewPullRequest' AND payload->>'contents' = ?" (Only run.id.toText) :: IO [Only (Aeson AE.Value)]
    retryPayloads `shouldBe` replicate 2 (Only $ Aeson $ AE.toJSON $ Jobs.ReviewPullRequest run.id)
    (linksRequests, ()) <- runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = cfg}) $ transport True $ fakeAgentWith ["--expect-knowledge"] result $ Impact.reviewPullRequest run.id
    runQueryEffect tr (Reviews.priorKnowledge run "test-service") >>= (\notes -> length notes `shouldBe` 1)
    let linksPublished = T.intercalate " " $ map (decodeUtf8 . snd) $ filter (T.isInfixOf "/issues/comments/99" . fst) linksRequests
    linksPublished `shouldSatisfy` T.isInfixOf "telemetry evidence"
    linksPublished `shouldNotSatisfy` T.isInfixOf "Observed"
    linksPublished `shouldNotSatisfy` T.isInfixOf "/widget.png"
    linksPublished `shouldNotSatisfy` T.isInfixOf "The changed operation"
    -- Final publication validation also rejects fabricated locations if the effect is bypassed.
    runQueryEffect tr $ Reviews.retryRun reviewPid run.id
    let invalid = result{Impact.findings = [Impact.Finding (Impact.ChangedLine "unrelated.hs" 1 Impact.Head) "Invented mechanism" "Invented next step" ["telemetry-1"]]}
    (newRequests, ()) <- runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = cfg}) $ transport False $ interpose @Agent.CodeReview (\_ -> \case Agent.ReviewRepository{} -> pure $ Right $ decodeUtf8 $ AE.encode invalid; Agent.FixRepository{} -> pure $ Left "Unexpected issue-fix task"; Agent.AdmitRegression{} -> pure $ Left "Unexpected admission task"; Agent.VerifyFixArtifacts{} -> pure $ Left "Unexpected artifact task"; Agent.ExplainVerifiedFix{} -> pure $ Left "Unexpected explanation task"; Agent.ReviewVerifiedFix{} -> pure $ Left "Unexpected candidate review task") $ Impact.reviewPullRequest run.id
    let created = T.intercalate " " $ map (decodeUtf8 . snd) $ filter (T.isSuffixOf "/issues/7/comments" . fst) newRequests
    created `shouldSatisfy` T.isInfixOf "Coverage unknown"
    created `shouldNotSatisfy` T.isInfixOf "Invented mechanism"
    for_ @[] [True, False] \unexpected -> do
      runQueryEffect tr $ Reviews.retryRun reviewPid run.id
      let failing = if unexpected then withHTTPResponses (\_ _ -> throwIO $ userError "private-exception-detail") else Reader.local (\ctx -> ctx{config = cfg{githubAppPrivateKey = ""}})
      failed <- tryAny $ runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = cfg}) $ failing $ Impact.reviewPullRequest run.id
      failed `shouldSatisfy` isLeft
      failedRun <- maybe (fail "run missing") pure =<< runQueryEffect tr (Reviews.getRun run.id)
      failedRun.state `shouldBe` Reviews.Incomplete
      failedRun.error `shouldBe` Just "Review failed; retry to collect current evidence"
      runQueryEffect tr (Reviews.claimRun failedRun) `shouldReturn` True
      runQueryEffect tr $ Reviews.releaseRun failedRun
    runQueryEffect tr $ Reviews.retryRun reviewPid run.id
    (failedRequests, failedAgent) <-
      runTestBgRecordingHTTP frozenTime tr
        $ Reader.local (\ctx -> ctx{config = cfg})
        $ transport True
        $ interpose @Agent.CodeReview (\_ -> \case Agent.ReviewRepository{} -> pure $ Left "private-agent-failure"; Agent.FixRepository{} -> pure $ Left "Unexpected issue-fix task"; Agent.AdmitRegression{} -> pure $ Left "Unexpected admission task"; Agent.VerifyFixArtifacts{} -> pure $ Left "Unexpected artifact task"; Agent.ExplainVerifiedFix{} -> pure $ Left "Unexpected explanation task"; Agent.ReviewVerifiedFix{} -> pure $ Left "Unexpected candidate review task")
        $ tryAny (Impact.reviewPullRequest run.id)
    failedAgent `shouldSatisfy` isLeft
    failedRequests `shouldNotSatisfy` any (T.isInfixOf "/issues/" . fst)
    unavailable <- maybe (fail "run missing") pure =<< runQueryEffect tr (Reviews.getRun run.id)
    unavailable.state `shouldBe` Reviews.Incomplete
    unavailable.error `shouldBe` Just "Review failed; retry to collect current evidence"
    (_, interruptedHistory) <- testServant tr $ CodeContextPage.repositoriesGetH reviewPid (Just CodeContextPage.PullRequests) Nothing
    T.isInfixOf "attempts without model receipts:" (decodeUtf8 @Text $ Lucid.renderBS interruptedHistory) `shouldBe` True
    (_, interruptedCustomers) <- testServant tr $ CodeContextPage.repositoriesGetH reviewPid (Just CodeContextPage.Usage) Nothing
    T.isInfixOf "attempts without model receipts:" (decodeUtf8 @Text $ Lucid.renderBS interruptedCustomers) `shouldBe` True
    missingUsage <- runQueryEffect tr $ Reviews.usageTotals reviewPid
    let missing = sum . map (.usage.unreportedAttempts) . filter ((== Just run.id) . (.runId))
    missing missingUsage `shouldSatisfy` (> 0)
    [Only interruptedAttempt] <- withResource tr.trPool (\conn -> query conn "SELECT a.id FROM projects.pr_review_attempts a WHERE a.run_id = ? AND NOT EXISTS (SELECT 1 FROM projects.pr_review_usage u WHERE u.attempt_id = a.id) ORDER BY a.started_at DESC, a.id DESC LIMIT 1" (Only run.id)) :: IO [Only (UUIDId "agent_attempt")]
    runQueryEffect tr (Reviews.recordUsage run receipt{Agent.id = UUIDId $ UUID.fromWords 71 0 0 1, Agent.attemptId = interruptedAttempt, Agent.costNanousd = Nothing, Agent.succeeded = False}) `shouldReturn` True
    recoveredUsage <- runQueryEffect tr $ Reviews.usageTotals reviewPid
    missing recoveredUsage `shouldBe` missing missingUsage - 1
    for_ @[] ["--fail-after-usage", "--conflicting-usage", "--skip-submission", "--tampered-result", "--check-after-submission", "--wrong-check-revision", "--foreign-allocation", "--zero-allocation"] \failure -> do
      beforeFailure <- runQueryEffect tr $ Reviews.usageTotals reviewPid
      beforeChecks <- runQueryEffect tr $ Reviews.latestChecks reviewPid
      beforeKnowledge <- runQueryEffect tr $ Reviews.priorKnowledge run "test-service"
      runQueryEffect tr $ Reviews.retryRun reviewPid run.id
      (failureRequests, processFailure) <- runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = cfg}) $ transport True $ fakeAgentWith [failure] result $ tryAny (Impact.reviewPullRequest run.id)
      processFailure `shouldSatisfy` isLeft
      failureRequests `shouldNotSatisfy` any (T.isInfixOf "/issues/" . fst)
      afterFailure <- runQueryEffect tr $ Reviews.usageTotals reviewPid
      let measuredCost = sum . map (.usage.costNanousd) . filter (isNothing . (.runId))
      measuredCost afterFailure - measuredCost beforeFailure `shouldBe` if failure == "--wrong-check-revision" then 0 else 1234000
      afterChecks <- runQueryEffect tr $ Reviews.latestChecks reviewPid
      length afterChecks - length beforeChecks `shouldBe` if failure == "--wrong-check-revision" then 0 else 2
      afterKnowledge <- runQueryEffect tr $ Reviews.priorKnowledge run "test-service"
      length afterKnowledge `shouldBe` length beforeKnowledge

    runQueryEffect tr $ Reviews.retryRun reviewPid run.id
    (_, ()) <- runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = cfg}) $ transport True $ fakeAgent result $ Impact.reviewPullRequest run.id
    retried <- maybe (fail "run missing") pure =<< runQueryEffect tr (Reviews.getRun run.id)
    retried.state `shouldBe` Reviews.Completed
    retried.error `shouldBe` Nothing
    -- Billing changes during a worker attempt must not move its later receipts.
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.projects SET customer_id = 'cus_before_attempt' WHERE id = ?" (Only reviewPid)
    runQueryEffect tr $ Reviews.retryRun reviewPid run.id
    (_, ()) <- runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = cfg}) $ transport True $ fakeAgentWith ["--billing-change-before-usage"] result $ Impact.reviewPullRequest run.id
    billingSnapshot <- withResource tr.trPool \conn -> query conn "SELECT (SELECT count(*) FROM projects.pr_review_usage WHERE project_id = ? AND customer_id = 'cus_before_attempt' AND billing_provider = ?), (SELECT count(*) FROM projects.pr_review_checks WHERE project_id = ? AND customer_id = 'cus_before_attempt' AND billing_provider = ?)" (reviewPid, Projects.StripeProvider, reviewPid, Projects.StripeProvider) :: IO [(Int64, Int64)]
    billingSnapshot `shouldBe` [(2, 2)]
    [Only (Aeson delayed)] <- withResource tr.trPool (\conn -> query conn "SELECT receipt FROM projects.pr_review_usage WHERE project_id = ? AND customer_id = 'cus_before_attempt' AND cost_nanousd IS NOT NULL" (Only reviewPid)) :: IO [Only (Aeson Agent.ModelUsage)]
    runQueryEffect tr (Reviews.recordUsage run delayed) `shouldReturn` True
    runQueryEffect tr (Reviews.recordUsage run delayed{Agent.id = UUIDId $ UUID.fromWords 70 0 0 1, Agent.provider = "another-provider"}) `shouldReturn` False
    runQueryEffect tr (Reviews.recordUsage run{Reviews.projectId = emptyPid} delayed) `shouldReturn` False
    runQueryEffect tr (Reviews.recordCheck run execution{Agent.id = UUIDId $ UUID.fromWords 70 0 0 2, Agent.attemptId = delayed.attemptId}) `shouldReturn` True
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.projects SET customer_id = NULL WHERE id = ?" (Only reviewPid)
    runQueryEffect tr (Reviews.recordUsage run delayed{Agent.id = UUIDId $ UUID.fromWords 70 0 0 3}) `shouldReturn` True
    pinned <- withResource tr.trPool \conn -> query conn "SELECT count(*) FROM projects.pr_review_usage WHERE project_id = ? AND customer_id = 'cus_before_attempt' AND attempt_id IS NOT NULL" (Only reviewPid) :: IO [Only Int64]
    pinned `shouldBe` [Only 3]
    for_ related \(pid, n) -> when (n == 5) do
      [otherRun] <- runQueryEffect tr $ Reviews.latestRuns pid
      runQueryEffect tr (Reviews.recordUsage otherRun delayed{Agent.id = UUIDId $ UUID.fromWords 70 0 0 4}) `shouldReturn` False
      runQueryEffect tr (Reviews.recordCheck otherRun execution{Agent.id = UUIDId $ UUID.fromWords 70 0 0 5, Agent.attemptId = delayed.attemptId}) `shouldReturn` False
    runQueryEffect tr $ Reviews.retryRun reviewPid run.id
    (_, ()) <- runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = cfg}) $ transport True $ fakeAgentWith ["--billing-change-before-usage"] result $ Impact.reviewPullRequest run.id
    unlinkedSnapshot <- withResource tr.trPool \conn -> query conn "SELECT count(*) FROM projects.pr_review_usage u JOIN projects.pr_review_attempts a ON a.id = u.attempt_id WHERE u.project_id = ? AND a.customer_id IS NULL AND u.customer_id IS NULL" (Only reviewPid) :: IO [Only Int64]
    unlinkedSnapshot `shouldBe` [Only 2]

    -- Revoking review permission while the agent runs also revokes its query tool.
    runQueryEffect tr $ Reviews.retryRun reviewPid run.id
    (revokedRequests, ()) <-
      runTestBgRecordingHTTP frozenTime tr
        $ Reader.local (\ctx -> ctx{config = cfg})
        $ transport True
        $ interpose @Agent.CodeReview
          ( \env -> \case
              Agent.ReviewRepository _ respond knowledge _ _ _ _ _ -> localSeqUnlift env \unlift -> do
                let request = Agent.TelemetryQuery "test-service" "| summarize count()"
                unlift (respond request) >>= liftIO . (`shouldSatisfy` isRight)
                Reviews.updateSettings reviewPid (Reviews.ReviewSettings "impact-org" "checkout" False False)
                reply <- unlift $ respond request
                liftIO $ reply `shouldSatisfy` isLeft
                remembered <- unlift $ knowledge $ Agent.Recall "test-service"
                liftIO $ remembered `shouldSatisfy` isLeft
                pure $ Right $ decodeUtf8 $ AE.encode result
              Agent.FixRepository{} -> pure $ Left "Unexpected issue-fix task"
              Agent.AdmitRegression{} -> pure $ Left "Unexpected admission task"
              Agent.VerifyFixArtifacts{} -> pure $ Left "Unexpected artifact task"
              Agent.ExplainVerifiedFix{} -> pure $ Left "Unexpected explanation task"
              Agent.ReviewVerifiedFix{} -> pure $ Left "Unexpected candidate review task"
          )
        $ Impact.reviewPullRequest run.id
    revokedRequests `shouldNotSatisfy` any (T.isInfixOf "/issues/" . fst)
    runQueryEffect tr $ Reviews.updateSettings reviewPid (Reviews.ReviewSettings "impact-org" "checkout" True False)
    -- Unmapped repositories publish a coverage gap without calling the model.
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.code_mappings SET service = NULL WHERE project_id = ?" (Only reviewPid)
    runQueryEffect tr $ Reviews.retryRun reviewPid run.id
    (unmappedRequests, ()) <- runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = cfg}) $ transport True $ interpose @Agent.CodeReview (\_ _ -> liftIO $ fail "Review agent called without measured evidence") $ Impact.reviewPullRequest run.id
    let unmapped = T.intercalate " " $ map (decodeUtf8 . snd) $ filter (T.isInfixOf "/issues/comments/99" . fst) unmappedRequests
    unmapped `shouldSatisfy` T.isInfixOf "Coverage unknown"
    unmapped `shouldNotSatisfy` T.isInfixOf "telemetry evidence"
    void $ testServant tr $ CodeContextPage.impactReviewSettingsPostH reviewPid (Reviews.ReviewSettings "impact-org" "checkout" False False)
    void $ deliver tr $ payload "synchronize" (T.replicate 40 "b") "2025-01-01T00:01:00Z"
    runs <- runQueryEffect tr $ Reviews.latestRuns reviewPid
    length runs `shouldBe` 1
    runQueryEffect tr (Reviews.repositorySettings reviewPid) >>= \settings -> map (\s -> (s.enabled, s.includeEvidence)) settings `shouldBe` [(False, False)]
    -- A metrics-only service still needs an investigation when there are no log/span counts.
    [credential] <- runQueryEffect tr $ GitSync.getGitHubCredentials reviewPid
    runQueryEffect tr $ CodeContext.insertCodeMapping reviewPid credential.id (Git.RepoRef "impact-org" "checkout" "main") (Just "test-service") "" ""
    void $ withResource tr.trPool \conn -> execute conn "DELETE FROM otel_logs_and_spans WHERE project_id = ?" (Only reviewPid)
    runQueryEffect tr $ Reviews.updateSettings reviewPid (Reviews.ReviewSettings "impact-org" "checkout" True True)
    runQueryEffect tr $ Reviews.retryRun reviewPid run.id
    (metricsRequests, ()) <-
      runTestBgRecordingHTTP frozenTime tr
        $ Reader.local (\ctx -> ctx{config = cfg})
        $ transport True
        $ interpose @Agent.CodeReview
          ( \env -> \case
              Agent.ReviewRepository _ respond _ _ _ _ _ _ -> localSeqUnlift env \unlift -> do
                let request = Agent.TelemetryQuery "test-service" "metrics | metric_name == \"review.temperature\" | summarize avg(value)"
                observed <- unlift $ respond request
                liftIO $ decodeUtf8 @Text (AE.encode observed) `shouldSatisfy` T.isInfixOf "42"
                liftIO $ void $ withResource tr.trPool \conn -> execute conn "UPDATE otel_metrics SET value = 84 WHERE project_id = ? AND metric_name = 'review.temperature'" (Only reviewPid.toText)
                repeated <- unlift $ respond request
                liftIO $ AE.encode repeated `shouldBe` AE.encode observed
                refreshed <- unlift $ respond request{Agent.query = request.query <> " | take 1"}
                liftIO $ decodeUtf8 @Text (AE.encode refreshed) `shouldSatisfy` T.isInfixOf "84"
                pure $ Right $ decodeUtf8 $ AE.encode result{Impact.findings = [Impact.Finding (Impact.ChangedLine "checkout.hs" 1 Impact.Head) "Check the changed metric behavior." "Verify the gauge." ["query-1"]]}
              Agent.FixRepository{} -> pure $ Left "Unexpected issue-fix task"
              Agent.AdmitRegression{} -> pure $ Left "Unexpected admission task"
              Agent.VerifyFixArtifacts{} -> pure $ Left "Unexpected artifact task"
              Agent.ExplainVerifiedFix{} -> pure $ Left "Unexpected explanation task"
              Agent.ReviewVerifiedFix{} -> pure $ Left "Unexpected candidate review task"
          )
        $ Impact.reviewPullRequest run.id
    T.intercalate " " (map (decodeUtf8 . snd) metricsRequests) `shouldSatisfy` T.isInfixOf "Review query results"
    T.intercalate " " (map (decodeUtf8 . snd) metricsRequests) `shouldNotSatisfy` T.isInfixOf "No telemetry observed"

  it "persists issue-fix phases only after frozen reproduction and counterfactual verification" \tr -> do
    setup tr
    [mapping] <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
    let uid n = UUIDId $ UUID.fromWords 91 0 0 n
        issue = uid 1
        image = uid 2
    void $ withResource tr.trPool \conn -> execute conn "INSERT INTO apis.issues (id, project_id, issue_type, service, target_hash, title) VALUES (?, ?, 'runtime_exception', 'test-service', 'fix-workflow', 'Reproduce the issue')" (issue, testPid)
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.projects SET customer_id = 'cus_fix_before', billing_provider = ? WHERE id = ?" (Projects.StripeProvider, testPid)
    base <- either (fail . show) pure $ Fixes.mkRevision $ T.replicate 40 "a"
    candidateRevision <- either (fail . show) pure $ Fixes.mkRevision $ T.replicate 40 "b"
    newerRevision <- either (fail . show) pure $ Fixes.mkRevision $ T.replicate 40 "c"
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
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.projects SET customer_id = 'cus_fix_after' WHERE id = ?" (Only testPid)
    runQueryEffect tr (Fixes.createRun testPid fid issue mapping.id base image) `shouldReturn` Just fid
    runQueryEffect tr (Fixes.createRun testPid fid issue mapping.id candidateRevision image) `shouldReturn` Nothing
    reject initial 10 (Fixes.ProposeCandidate candidate) Fixes.ReproductionRequired
    let blockedCheck = check{Agent.id = uid 6, Agent.regression = Nothing, Agent.status = Agent.Blocked, Agent.exitCode = Just 127}
    blocked <- accept initial 11 $ Fixes.Observe blockedCheck
    Fixes.phase blocked.progress `shouldBe` Fixes.Reproducing
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.issue_fix_steps SET step = jsonb_set(step, '{contents}', (step->'contents') - 'regression') WHERE id = ?" (Only (uid 11 :: Fixes.StepId))
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
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.git_credentials SET installation_id = installation_id + 1 WHERE id = ?" (Only mapping.credentialId)
    reject changedTest 31 (Fixes.ProposeCandidate candidate) Fixes.StepConflict
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.git_credentials SET installation_id = installation_id - 1 WHERE id = ?" (Only mapping.credentialId)
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.code_mappings SET service = 'revoked-service' WHERE id = ?" (Only mapping.id)
    reject changedTest 31 (Fixes.ProposeCandidate candidate) Fixes.StepConflict
    unchanged <- load
    unchanged.version `shouldBe` changedTest.version

  it "accounts for review and issue-fix work through the persisted protocol gates" \tr -> do
    setup tr
    [mapping] <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
    let uid n = UUIDId $ UUID.fromWords 93 0 0 n
        issue = uid 1
        image = uid 2
    void $ withResource tr.trPool \conn -> execute conn "INSERT INTO apis.issues (id, project_id, issue_type, service, target_hash, title) VALUES (?, ?, 'runtime_exception', 'test-service', 'fix-protocol', 'Retain retry tokens')" (issue, testPid)
    base <- either (fail . show) pure $ Fixes.mkRevision $ T.replicate 40 "a"
    let load fid = runQueryEffect tr (Fixes.getRun testPid fid) >>= either (fail . show) (maybe (fail "fix missing") pure)
    otherPid <- createTestProject tr "Other fix ledger project"
    void $ deliver tr $ payload "opened" revision "2025-01-01T00:00:00Z"
    [review] <- runQueryEffect tr $ Reviews.latestRuns testPid
    for_ (zip [10 :: Word32 ..] ["", "--tampered-result", "--post-phase", "--deny-phase"]) \(n, mode) -> do
      let fid = uid n
      runQueryEffect tr (Fixes.createRun testPid fid issue mapping.id base image) `shouldReturn` Just fid
      let customer = if n == 10 then Nothing else Just "cus_fix_usage" :: Maybe Text
      void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.projects SET customer_id = ?, billing_provider = ? WHERE id = ?" (customer, Projects.StripeProvider, testPid)
      runQueryEffect tr (Usage.startAttempt otherPid (Usage.Fix fid) "fixture" "gpt-5.6-luna#high") `shouldReturn` Nothing
      attempt <- maybe (fail "Fix attempt missing from shared ledger") pure =<< runQueryEffect tr (Usage.startAttempt testPid (Usage.Fix fid) "fixture" "gpt-5.6-luna#high")
      void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.projects SET customer_id = 'cus_after_fix', billing_provider = ? WHERE id = ?" (Projects.LemonSqueezyProvider, testPid)
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
                          Agent.runCodeReviewReal Nothing (Just "test/fixtures/fix-review-agent.mjs") [reviewMode]
                            $ Agent.reviewVerifiedFix (Agent.FixReviewRequest (Agent.FixRequest request{Agent.attemptId = uid 998} issue image) artifacts) verified (const $ pure $ Left "Unexpected memory request") (const pass) (const pass) (const pass)
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
            $ Agent.runCodeReviewReal Nothing (Just "test/fixtures/issue-fix-agent.mjs") [diagnostic, toString image.toText, mode]
            $ Agent.fixRepository
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
                  let withField key value = case AE.toJSON receipt of AE.Object fields -> AE.Object $ KM.insert key value fields; original -> original
                  for_ ([("durationMs", AE.toJSON (999 :: Int)), ("attemptId", AE.toJSON (uid 999 :: UUIDId "agent_attempt"))] :: [(AE.Key, AE.Value)]) \(key, value) -> do
                    changed <- either fail pure $ AE.eitherDecode $ AE.encode $ withField key value
                    runQueryEffect tr (Usage.recordAllocation testPid (Usage.Fix fid) changed) `shouldReturn` False
                  for_ (["cpus", "memoryMiB"] :: [AE.Key]) \field ->
                    (AE.eitherDecode (AE.encode $ withField field $ AE.toJSON (0 :: Int)) :: Either String Agent.AllocationReceipt) `shouldSatisfy` isLeft
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
      attribution <- withResource tr.trPool \conn -> query conn "SELECT customer_id, billing_provider FROM projects.pr_review_usage WHERE fix_run_id = ? AND run_id IS NULL AND attempt_id = ?" (fid, attempt) :: IO [(Maybe Text, Projects.BillingProvider)]
      attribution `shouldBe` [(customer, Projects.StripeProvider)]
      storedChecks <- withResource tr.trPool \conn -> query conn "SELECT count(*) FROM projects.pr_review_checks WHERE fix_run_id = ? AND run_id IS NULL AND attempt_id = ? AND customer_id IS NOT DISTINCT FROM ? AND billing_provider = ?" (fid, attempt, customer, Projects.StripeProvider) :: IO [Only Int64]
      storedChecks `shouldBe` [Only $ if mode == "--deny-phase" then 1 else 3]
    let fid = uid 10
    lateAttempt <- maybe (fail "Late attempt missing") pure =<< runQueryEffect tr (Usage.startAttempt testPid (Usage.Fix fid) "fixture" "gpt-5.6-luna#high")
    missing <- runQueryEffect tr $ Usage.usageTotals Usage.Fixes testPid
    fmap (.usage.unreportedAttempts) (find (isNothing . (.runId)) missing) `shouldBe` Just 1
    [Only (Aeson receipt)] <- withResource tr.trPool (\conn -> query conn "SELECT receipt FROM projects.pr_review_usage WHERE fix_run_id = ?" (Only fid)) :: IO [Only (Aeson Agent.ModelUsage)]
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Fix fid) receipt{Agent.id = uid 900, Agent.attemptId = lateAttempt}) `shouldReturn` False
    let lateCall = Agent.ModelCall{id = uid 902, attemptId = lateAttempt, provider = receipt.provider, model = receipt.model}
    runQueryEffect tr (Usage.recordCall testPid (Usage.Fix fid) lateCall) `shouldReturn` True
    runQueryEffect tr (Usage.recordUsage testPid (Usage.Fix fid) receipt{Agent.id = uid 900, Agent.attemptId = lateAttempt, Agent.callId = Just lateCall.id, Agent.costNanousd = Nothing, Agent.succeeded = False}) `shouldReturn` True
    runQueryEffect tr (Reviews.claimRun review) `shouldReturn` True
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
    let usageText = decodeUtf8 $ Lucid.renderBS usagePage
    usageText `shouldSatisfy` T.isInfixOf "review and issue-fix"
    usageText `shouldSatisfy` T.isInfixOf "cus_fix_usage"
    usageText `shouldSatisfy` T.isInfixOf "No billing customer linked"
    usageText `shouldSatisfy` T.isInfixOf "$0.000126 estimated"
    usageText `shouldSatisfy` T.isInfixOf "1 unpriced"
    usageText `shouldSatisfy` T.isInfixOf "Sandbox allocation"
    usageText `shouldSatisfy` T.isInfixOf "agent VM allocations"
    usageText `shouldSatisfy` T.isInfixOf "agent VM cleanup unconfirmed"
    usageText `shouldSatisfy` T.isInfixOf "attempts without an agent allocation receipt"
    usageText `shouldSatisfy` T.isInfixOf "resource cost unavailable"
    billable <- withResource tr.trPool \conn -> query conn "SELECT count(*) FROM projects.ai_usage WHERE project_id = ?" (Only testPid) :: IO [Only Int64]
    billable `shouldBe` [Only 0]

  it "binds separately metered admission decisions to their exact artifact without fix capabilities" \tr -> do
    setup tr
    [mapping] <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
    let uid n = UUIDId $ UUID.fromWords 94 0 0 n
        issue = uid 1
        fid = uid 2
        artifact = Agent.TestArtifact "bash regression.sh" "native test patch" "retry token lost"
    void $ withResource tr.trPool \conn -> execute conn "INSERT INTO apis.issues (id, project_id, issue_type, service, target_hash, title) VALUES (?, ?, 'runtime_exception', 'test-service', 'admission-protocol', 'Retain retry tokens')" (issue, testPid)
    base <- either (fail . show) pure $ Fixes.mkRevision revision
    runQueryEffect tr (Fixes.createRun testPid fid issue mapping.id base (uid 3)) `shouldReturn` Just fid
    for_ ["", "--reject", "--tampered", "--skip", "--check", "--post-query"] \mode -> do
      attempt <- maybe (fail "Admission attempt missing") pure =<< runQueryEffect tr (Usage.startAttempt testPid (Usage.Fix fid) "fixture" "gpt-5.6-luna#high")
      let request = Agent.Request{projectId = testPid, runId = fid, attemptId = attempt, repository = "https://github.com/impact-org/checkout.git", base = revision, head = revision, gitToken = "fixture-token", prompt = "Issue: retain retry tokens", provider = "fixture", model = "gpt-5.6-luna", thinkingLevel = Agent.High}
      outcome <-
        runEff
          $ Agent.runCodeReviewReal Nothing (Just "test/fixtures/regression-admission-agent.mjs") [mode]
          $ Agent.admitRegression
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
    checks <- withResource tr.trPool \conn -> query conn "SELECT count(*) FROM projects.pr_review_checks WHERE fix_run_id = ?" (Only fid) :: IO [Only Int64]
    checks `shouldBe` [Only 0]

  it "runs an authorized issue-fix application job through admission, evidence, phases and accounting" \tr -> do
    setup tr
    [mapping] <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
    let uid n = UUIDId $ UUID.fromWords 95 0 0 n
        issue = uid 1
        fid = uid 2
        image = uid 3
        user = (getResponse tr.trSessAndHeader :: Projects.Session).user.id
    void $ withResource tr.trPool \conn -> execute conn "INSERT INTO apis.issues (id, project_id, issue_type, service, target_hash, title) VALUES (?, ?, 'runtime_exception', 'test-service', 'application-fix', 'Retain retry tokens')" (issue, testPid)
    base <- either (fail . show) pure $ Fixes.mkRevision revision
    runQueryEffect tr (Fixes.createRun testPid fid issue mapping.id base image) `shouldReturn` Just fid
    runQueryEffect tr (Fixes.queueJob testPid user fid) `shouldReturn` True
    runQueryEffect tr (Fixes.queueJob testPid user fid) `shouldReturn` True
    queued <- withResource tr.trPool \conn -> query conn "SELECT payload FROM background_jobs WHERE payload->>'tag' = 'FixIssue'" () :: IO [Only (Aeson AE.Value)]
    queued `shouldBe` [Only $ Aeson $ AE.toJSON $ Jobs.FixIssue testPid fid]
    unleased <- runQueryEffect tr $ Fixes.recordStep testPid fid 0 (uid 6) Fixes.RestartReproduction
    unleased `shouldSatisfy` isLeft
    lease <- maybe (fail "Fix lease missing") pure =<< runQueryEffect tr (Fixes.claimJob testPid fid)
    runQueryEffect tr (isJust <$> Fixes.claimJob testPid fid) `shouldReturn` False
    runQueryEffect tr (Fixes.currentJob lease) `shouldReturn` True
    void $ withResource tr.trPool \conn -> execute conn "UPDATE users.users SET active = false WHERE id = ?" (Only user)
    runQueryEffect tr (Fixes.currentJob lease) `shouldReturn` False
    runQueryEffect tr (Fixes.queueJob testPid user fid) `shouldReturn` False
    void $ withResource tr.trPool \conn -> execute conn "UPDATE users.users SET active = true WHERE id = ?" (Only user)
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?" (ProjectMembers.PView, testPid, user)
    runQueryEffect tr (Fixes.currentJob lease) `shouldReturn` False
    runQueryEffect tr (Fixes.queueJob testPid user fid) `shouldReturn` False
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?" (ProjectMembers.PAdmin, testPid, user)
    void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.issue_fix_jobs SET lease_until = now() - interval '1 second' WHERE run_id = ?" (Only fid)
    replacement <- maybe (fail "Expired lease not reclaimed") pure =<< runQueryEffect tr (Fixes.claimJob testPid fid)
    runQueryEffect tr $ Fixes.releaseJob lease
    runQueryEffect tr (Fixes.currentJob replacement) `shouldReturn` True
    stale <- runQueryEffect tr $ Fixes.recordJobStep lease 0 (uid 4) Fixes.RestartReproduction
    stale `shouldSatisfy` isLeft
    runQueryEffect tr $ Fixes.releaseJob replacement
    apiKey <- createTestAPIKey tr testPid "Fix job telemetry"
    ingestLog tr apiKey "fix-job-event" (addUTCTime (-60) frozenTime)
    pem <- readFileBS "test/fixtures/github-app-test.pem"
    let cfg = tr.trATCtx.config{githubAppId = "123", githubAppPrivateKey = extractBase64 $ B64.encodeBase64 pem, impactReviewUsageDir = Just "fixture-usage", impactReviewCacheDir = Just "fixture-cache", impactReviewProvider = "fixture", openaiModel = "gpt-5.6-luna#high", impactReviewArtifactBucket = Just "fixture-bucket", s3Endpoint = "http://storage.invalid"}
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
          conflicted <- send $ Memory.Write edit
          conflicted `shouldSatisfy` isLeft
          isLeft <$> send (Memory.Read $ uid 997) `shouldReturn` True
          let permission value = void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?" (value, testPid, user)
          permission ProjectMembers.PView
          ( do
              isLeft <$> send (Memory.Read page) `shouldReturn` True
              isLeft <$> send (Memory.Write edit{Memory.version = 1}) `shouldReturn` True
            )
            `finally` permission ProjectMembers.PAdmin
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
                  Agent.FixRepository request queryTelemetry knowledge admit calls usage allocations checks phase validate -> localSeqUnliftIO env \unlift -> liftIO $ withSystemTempDirectory "job-fix-agent" \directory -> do
                    let diagnostic = directory </> "failure.txt"
                    when (mode == "--app-job") $ memoryRoundTrip request.request.attemptId (unlift . knowledge)
                    result <-
                      runEff
                        $ Agent.runCodeReviewReal Nothing (Just "test/fixtures/issue-fix-agent.mjs") [diagnostic, toString image.toText, toString mode]
                        $ Agent.fixRepository
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
                  Agent.AdmitRegression request knowledge calls usage allocations -> localSeqUnliftIO env \unlift -> liftIO do
                    when (mode == "--app-job") $ memoryRoundTrip request.fix.request.attemptId (unlift . knowledge)
                    runEff
                      $ Agent.runCodeReviewReal Nothing (Just "test/fixtures/regression-admission-agent.mjs") [if request.artifact.testPatch == "admitted-test-patch" then "" else "--reject"]
                      $ Agent.admitRegression request (liftIO . unlift . knowledge) (liftIO . unlift . calls) (liftIO . unlift . usage) (liftIO . unlift . allocations)
                  Agent.VerifyFixArtifacts _ request -> artifactProofFixture (if mode == "--app-job-artifacts-denied" then Left "Fixture rejects artifacts" else Right ()) request
                  Agent.ReviewVerifiedFix request verified knowledge calls usage allocations -> localSeqUnliftIO env \unlift -> liftIO do
                    when (mode == "--app-job") $ memoryRoundTrip request.fix.request.attemptId (unlift . knowledge)
                    runEff
                      $ Agent.runCodeReviewReal Nothing (Just "test/fixtures/fix-review-agent.mjs") [if mode == "--app-job-review-changes" then "--reject" else if mode == "--app-job-review-blocked" then "--blocked" else ""]
                      $ Agent.reviewVerifiedFix request verified (liftIO . unlift . knowledge) (liftIO . unlift . calls) (liftIO . unlift . usage) (liftIO . unlift . allocations)
                  Agent.ExplainVerifiedFix store request verified -> explanationProofFixture store (if mode == "--app-job-explanation-denied" then Left "Fixture rejects explanation" else Right $ fixtureExplanation request.result) request verified
                  Agent.ReviewRepository{} -> pure $ Left "Unexpected advisory review"
              )
        transport = withHTTPResponses \_ endpoint -> pure $ if "/access_tokens" `T.isSuffixOf` toText endpoint then Just (AE.encode $ AE.object ["token" AE..= ("fixture-token" :: Text), "expires_at" AE..= ("2030-01-01T00:00:00Z" :: Text)]) else Nothing
        run fid' mode = runTestBgRecordingHTTP frozenTime tr $ Reader.local (\ctx -> ctx{config = if mode == "--app-job-no-storage" then cfg{impactReviewArtifactBucket = Nothing} else cfg}) $ transport $ fakeAgent mode $ BackgroundJobs.processBackgroundJob tr.trATCtx (Jobs.FixIssue testPid fid')
    tryAny (run fid "--app-job-no-storage") >>= (`shouldSatisfy` isLeft)
    initialCosts <- runQueryEffect tr $ Usage.usageTotals Usage.Fixes testPid
    maybe 0 (.usage.calls) (find ((== Just fid) . (.runId)) initialCosts) `shouldBe` 0
    runQueryEffect tr (isJust <$> Fixes.publicationForUser testPid fid user) `shouldReturn` False
    (requests, ()) <- run fid "--app-job"
    requests `shouldNotSatisfy` any (\(url, _) -> "/pulls" `T.isInfixOf` url || "/comments" `T.isInfixOf` url)
    completed <- maybe (fail "Verified application result missing") pure =<< runQueryEffect tr (Fixes.completedResult testPid fid)
    Fixes.revisionText completed.head `shouldBe` T.replicate 40 "b"
    completed.runId `shouldBe` fid
    memories <- runQueryEffect tr $ Memory.latestPages testPid
    length memories `shouldBe` 4
    map (.author) memories `shouldSatisfy` all (\case Memory.Agent _ -> True; Memory.Member _ -> False; Memory.Conversation _ -> False)
    map (.version) memories `shouldBe` [1, 1, 1, 1]
    [Only persisted] <- withResource tr.trPool (\conn -> query conn "SELECT coalesce(to_jsonb(j)->'explanation', 'null'::jsonb) <> 'null'::jsonb FROM projects.issue_fix_jobs j WHERE run_id = ?" (Only fid)) :: IO [Only Bool]
    persisted `shouldBe` True
    [Only reviewed] <- withResource tr.trPool (\conn -> query conn "SELECT j.review_attempt_id IS NOT NULL AND j.review_rationale IS NOT NULL FROM projects.issue_fix_jobs j WHERE run_id = ?" (Only fid)) :: IO [Only Bool]
    reviewed `shouldBe` True
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
    void $ withResource tr.trPool \conn -> execute conn "UPDATE users.users SET active = true WHERE id = ?" (Only user)
    [Only retained] <- withResource tr.trPool (\conn -> query conn "SELECT to_jsonb(j)->>'media_retained_at' IS NOT NULL FROM projects.issue_fix_jobs j WHERE run_id = ?" (Only fid)) :: IO [Only Bool]
    retained `shouldBe` True
    runQueryEffect tr (isJust <$> Fixes.claimJob testPid fid) `shouldReturn` False
    (_, ()) <- run fid "--app-job"
    totals <- runQueryEffect tr $ Usage.usageTotals Usage.Fixes testPid
    fmap (\total -> (total.usage.calls, total.usage.costNanousd)) (find ((== Just fid) . (.runId)) totals) `shouldBe` Just (4, 168000)
    fmap (\total -> (total.resources.checks, total.resources.vcpuMs, total.resources.explanations, total.resources.renderMs, total.resources.mediaBytes)) (find ((== Just fid) . (.runId)) totals) `shouldBe` Just (3, 700, 1, 1200, fromIntegral explanation.mediaBytes)
    knowledge <- runQueryEffect tr $ Reviews.priorRepositoryKnowledge testPid (Git.RepoRef "impact-org" "checkout" revision) "test-service"
    map (\note -> AE.toJSON note.source) knowledge `shouldBe` [AE.toJSON $ Reviews.FixSource fid]
    map (.revision) knowledge `shouldBe` [T.replicate 40 "b"]
    [Only (Aeson evidence)] <- withResource tr.trPool (\conn -> query conn "SELECT evidence FROM projects.pr_review_knowledge WHERE fix_run_id = ?" (Only fid)) :: IO [Only (Aeson AE.Value)]
    decodeUtf8 @Text (AE.encode evidence) `shouldNotSatisfy` T.isInfixOf "dataset"
    let failedId = uid 5
    runQueryEffect tr (Fixes.createRun testPid failedId issue mapping.id base image) `shouldReturn` Just failedId
    runQueryEffect tr (Fixes.queueJob testPid user failedId) `shouldReturn` True
    tryAny (run failedId "--app-job-tampered") >>= (`shouldSatisfy` isLeft)
    runQueryEffect tr (Fixes.completedResult testPid failedId) >>= (`shouldSatisfy` isNothing)
    failedRows <- withResource tr.trPool \conn -> query conn "SELECT failure, lease_owner FROM projects.issue_fix_jobs WHERE run_id = ?" (Only failedId) :: IO [(Maybe Text, Maybe UUID.UUID)]
    failedRows `shouldBe` [(Just "Issue fix failed; retry to collect current evidence", Nothing)]
    runQueryEffect tr (Reviews.priorRepositoryKnowledge testPid (Git.RepoRef "impact-org" "checkout" revision) "test-service") >>= (\notes -> length notes `shouldBe` 1)
    tryAny (run failedId "--app-job-artifacts-denied") >>= (`shouldSatisfy` isLeft)
    runQueryEffect tr (Fixes.completedResult testPid failedId) >>= (`shouldSatisfy` isNothing)
    runQueryEffect tr (Reviews.priorRepositoryKnowledge testPid (Git.RepoRef "impact-org" "checkout" revision) "test-service") >>= (\notes -> length notes `shouldBe` 1)
    tryAny (run failedId "--app-job-explanation-denied") >>= (`shouldSatisfy` isLeft)
    runQueryEffect tr (Fixes.completedResult testPid failedId) >>= (`shouldSatisfy` isNothing)
    runQueryEffect tr (Fixes.completedExplanation testPid failedId) >>= (`shouldSatisfy` isNothing)
    unfinished <- runQueryEffect tr $ Usage.usageTotals Usage.Fixes testPid
    fmap (.resources.explanations) (find ((== Just failedId) . (.runId)) unfinished) `shouldBe` Just 0
    runQueryEffect tr (Reviews.priorRepositoryKnowledge testPid (Git.RepoRef "impact-org" "checkout" revision) "test-service") >>= (\notes -> length notes `shouldBe` 1)
    tryAny (run failedId "--app-job-retention-denied") >>= (`shouldSatisfy` isLeft)
    runQueryEffect tr (Fixes.completedResult testPid failedId) >>= (`shouldSatisfy` isNothing)
    runQueryEffect tr (Reviews.priorRepositoryKnowledge testPid (Git.RepoRef "impact-org" "checkout" revision) "test-service") >>= (\notes -> length notes `shouldBe` 1)
    checkpointLease <- maybe (fail "Checkpoint lease missing") pure =<< runQueryEffect tr (Fixes.claimJob testPid failedId)
    runQueryEffect tr (isJust <$> Fixes.getCheckpoint checkpointLease) `shouldReturn` True
    void $ withResource tr.trPool \conn -> execute conn "UPDATE users.users SET active = false WHERE id = ?" (Only user)
    runQueryEffect tr (isJust <$> Fixes.getCheckpoint checkpointLease) `shouldReturn` False
    void $ withResource tr.trPool \conn -> execute conn "UPDATE users.users SET active = true WHERE id = ?" (Only user)
    runQueryEffect tr $ Fixes.releaseJob checkpointLease
    runQueryEffect tr (isJust <$> Fixes.getCheckpoint checkpointLease) `shouldReturn` False
    (_, ()) <- run failedId "--app-job"
    runQueryEffect tr (Fixes.completedResult testPid failedId) >>= (`shouldSatisfy` isJust)
    recoveredCosts <- runQueryEffect tr $ Usage.usageTotals Usage.Fixes testPid
    fmap (.usage.calls) (find ((== Just failedId) . (.runId)) recoveredCosts) `shouldBe` Just 11
    runQueryEffect tr (Reviews.priorRepositoryKnowledge testPid (Git.RepoRef "impact-org" "checkout" revision) "test-service") >>= (\notes -> length notes `shouldBe` 2)
    for_ ([(uid 20, "--app-job-review-changes"), (uid 21, "--app-job-review-blocked")] :: [(Fixes.FixId, Text)]) \(reviewId, mode) -> do
      runQueryEffect tr (Fixes.createRun testPid reviewId issue mapping.id base image) `shouldReturn` Just reviewId
      runQueryEffect tr (Fixes.queueJob testPid user reviewId) `shouldReturn` True
      tryAny (run reviewId mode) >>= (`shouldSatisfy` isLeft)
      runQueryEffect tr (Fixes.completedResult testPid reviewId) >>= (`shouldSatisfy` isNothing)
      runQueryEffect tr (isJust <$> Fixes.publicationForUser testPid reviewId user) `shouldReturn` False
      [Only checkpointed] <- withResource tr.trPool (\conn -> query conn "SELECT verified_result IS NOT NULL OR review_attempt_id IS NOT NULL FROM projects.issue_fix_jobs WHERE run_id = ?" (Only reviewId)) :: IO [Only Bool]
      checkpointed `shouldBe` False
      runQueryEffect tr (Reviews.priorRepositoryKnowledge testPid (Git.RepoRef "impact-org" "checkout" revision) "test-service") >>= (\notes -> length notes `shouldBe` 2)
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
      let config = Minio.setCreds (Minio.CredentialValue (fromString $ toString readCtx.config.s3AccessKey) (fromString $ toString readCtx.config.s3SecretKey) Nothing) $ Minio.setRegion readCtx.config.s3Region $ fromString $ toString readCtx.config.s3Endpoint
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
                when (mode == "revoke") $ liftIO $ void $ withResource tr.trPool \conn -> execute conn "UPDATE users.users SET active = false WHERE id = ?" (Only user)
                pure received
              Artifacts.ReadFixMedia saved asset -> do
                received <- if mode == "unavailable" then pure $ Left Artifacts.StorageUnavailable else Artifacts.runAgentArtifactsReal readCtx $ Artifacts.readFixMedia saved asset
                when (mode == "revoke") $ liftIO $ void $ withResource tr.trPool \conn -> execute conn "UPDATE users.users SET active = false WHERE id = ?" (Only user)
                pure received
            app mode =
              genericServeTWithContext
                (effToServantHandlerTest tr.trTestClock tr.trUUIDRef tr.trATCtx tr.trLogger tr.trTracerProvider . withMedia mode)
                (Routes.server tr.trLogger tr.trATCtx tr.trTracerProvider (\_ _ -> pure $ Left "unused") (\_ _ -> pure $ Left "unused"))
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
          void $ withResource tr.trPool \conn -> execute conn "UPDATE users.users SET active = true WHERE id = ?" (Only user)
        void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?" (ProjectMembers.PView, testPid, user)
        for_ (["/media/video", "/publication.tar"] :: [Text]) \suffix -> do
          denied <- fetch "normal" H.methodGet (path <> suffix) [authorization]
          H.statusCode denied.simpleStatus `shouldBe` 404
        void $ withResource tr.trPool \conn -> execute conn "UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?" (ProjectMembers.PAdmin, testPid, user)
        let corrupted = LBS.map (const 0) $ bytesFor Artifacts.Video
        overwritten <- Storage.runMinio readCtx.s3HttpManager config $ Minio.putObject readCtx.config.s3Bucket videoKey (CC.sourceLazy corrupted) (Just $ LBS.length corrupted) Minio.defaultPutObjectOptions
        isRight overwritten `shouldBe` True
        damaged <- fetch "normal" H.methodGet (path <> "/media/video") [authorization]
        H.statusCode damaged.simpleStatus `shouldBe` 503
        damaged.simpleBody `shouldNotBe` corrupted
        damagedPackage <- fetch "normal" H.methodGet (path <> "/publication.tar") [authorization]
        H.statusCode damagedPackage.simpleStatus `shouldBe` 503
        damagedPackage.simpleBody `shouldBe` ""

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


-- Protocol fixtures supply proof explicitly; native file integrity is tested separately.
artifactProofFixture :: IOE :> es => Either Text () -> Agent.ArtifactRequest -> Eff es (Either Text Agent.VerifiedFix)
artifactProofFixture outcome request =
  Agent.runCodeReviewTest (const $ Left "Unexpected review") (const $ Left "Unexpected fix") (const $ Left "Unexpected admission") (const outcome) (const $ Left "Unexpected explanation") (const $ Left "Unexpected peer review") $ Agent.verifyFixArtifacts (Agent.ArtifactStore "fixture-usage" "fixture-cache") request


explanationProofFixture :: IOE :> es => Agent.ArtifactStore -> Either Text Agent.Explanation -> Agent.ArtifactRequest -> Agent.VerifiedFix -> Eff es (Either Text Agent.ExplainedFix)
explanationProofFixture store outcome request verified =
  Agent.runCodeReviewTest (const $ Left "Unexpected review") (const $ Left "Unexpected fix") (const $ Left "Unexpected admission") (const $ Left "Unexpected artifacts") (const outcome) (const $ Left "Unexpected peer review") $ Agent.explainVerifiedFix store request verified


fixtureExplanation :: Agent.FixResult -> Agent.Explanation
fixtureExplanation result =
  let files = Map.fromList [(name, show (hash (encodeUtf8 name :: ByteString) :: Digest SHA256)) | asset <- [minBound .. maxBound], let name = Artifacts.assetFile asset]
   in Agent.Explanation{version = 1, source = T.replicate 64 "c", rendererSha256 = T.replicate 64 "d", projectId = result.projectId, runId = result.runId, issueId = result.issueId, candidateId = result.candidateId, head = result.head, checkId = result.checkId, regressionSha256 = result.regressionSha256, durationMs = 36000, width = 1280, height = 720, renderMs = 1200, mediaBytes = fromIntegral $ sum $ map (BS.length . encodeUtf8) $ Map.keys files, videoBytes = 15, resourceCostStatus = Agent.Unpriced, tools = Map.fromList [("ffmpeg", "ffmpeg fixture"), ("graphviz", "graphviz fixture")], files}
