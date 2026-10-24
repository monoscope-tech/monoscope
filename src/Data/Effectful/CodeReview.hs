{-# LANGUAGE NoFieldSelectors #-}

module Data.Effectful.CodeReview (CodeReview (..), Request (..), FixRequest (..), TestArtifact (..), AdmissionRequest (..), AdmissionDecision (..), PhaseReceipt (..), FixResult (..), ArtifactRequest (..), ArtifactStore (..), FixReviewRequest (..), FixReviewDecision (..), ReviewedFix, reviewedFixResult, reviewedFixApproval, FixReviewOutcome (..), VerifiedFix, verifiedFixResult, ExplainedFix, explainedFixResult, fixExplanation, Explanation (..), PricingStatus (..), ThinkingLevel (..), ReviewMode (..), DraftSubmission (..), TelemetryQuery (..), KnowledgeNote (..), KnowledgeRequest (..), ModelUsage (..), ModelCall (..), AllocationReceipt, allocationId, allocationAttempt, Regression (..), CheckReceipt (..), CheckStatus (..), RevisionSide (..), reviewRepository, reviewPatch, reviewSource, fixRepository, admitRegression, verifyFixArtifacts, reviewVerifiedFix, explainVerifiedFix, runCodeReviewReal, runCodeReviewTest, validExplanation) where

import Control.Concurrent.STM qualified as STM
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.State.Strict qualified as State
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Effectful.CodeReview.Artifacts (ArtifactStore (..))
import Data.Effectful.CodeReview.Artifacts qualified as Native
import Data.Effectful.CodeReview.Checkout qualified as Checkout
import Data.Effectful.CodeReview.Explanation qualified as Media
import Data.Effectful.CodeReview.Outbox qualified as Outbox
import Data.Effectful.CodeReview.Policy qualified as Policy
import Data.Effectful.CodeReview.Types
import Data.OpenApi.Schema.Validation qualified as Schema
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUID4
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Models.Projects.ProjectMemory qualified as Memory
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Relude
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import System.Process.Typed (ExitCode (..), createPipe, getStdin, getStdout, nullStream, proc, setEnv, setStderr, setStdin, setStdout, stopProcess, waitExitCode, waitExitCodeSTM, withProcessTerm)
import UnliftIO.Exception (finally, tryAny)
import UnliftIO.Timeout qualified as Timeout


-- JSON-lines protocol: queries, usage, execution receipts, validation, and result.
data Message = Checkout () | Tool Policy.Invocation | Query TelemetryQuery | Knowledge KnowledgeRequest | Admit TestArtifact | InitializeAdmission () | RestartFix FixBaseline | CompleteFix Native.FixSummary | GuestResult (Either Text Text) | CopiedGuest (Either Text ()) | CheckedRegression (Either Text RegressionWire) | Validate AE.Value | ValidateText Text | Result AE.Value
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data GuestCommand = GuestCommand {arguments :: NonEmpty Text, maxBytes :: Word32, identity :: GuestIdentity}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data GuestIdentity = Agent | Supervisor
  deriving stock (Generic, Read, Show)
  deriving (AE.ToJSON) via WrappedEnumSC 'Nothing "" GuestIdentity


data GuestCopy = GuestCopy {source :: FilePath, destination :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data GuestRead = GuestRead {path :: Text, maxBytes :: Word32}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data SandboxCommand = Guest GuestCommand | CopyGuest GuestCopy | ReadGuest GuestRead | RunCheck FilePath
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data PreparedDisk = PreparedDisk {path :: FilePath, sha256 :: WorkspaceFingerprint}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data FixBaseline = Pristine | PreparedCheckpoint PreparedDisk
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- The report is opaque presentation data; decisions use the typed receipts.
data RegressionWire = RegressionWire {report :: AE.Value, receipts :: [CheckReceipt], isError :: Bool}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data RegressionPatch = InlinePatch Text | PatchFile Text


data ReproductionInput = ReproductionInput Text Text RegressionPatch


instance AE.FromJSON ReproductionInput where
  parseJSON = AE.withObject "Issue reproduction" \fields -> do
    patch <-
      (,) <$> fields AE..:? "testPatch" <*> fields AE..:? "testPatchFile" >>= \case
        (Just inline, Nothing) -> pure $ InlinePatch inline
        (Nothing, Just path) -> pure $ PatchFile path
        _ -> fail "Supply exactly one regression patch or patch file"
    ReproductionInput <$> fields AE..: "script" <*> fields AE..: "failureMessage" <*> pure patch


data FixProgress = NeedsReproduction | Reproduced TestArtifact | Captured TestArtifact Native.CapturedCandidate | Checked TestArtifact Native.CapturedCandidate CheckReceipt


data FixSession = FixSession {id :: UUIDId "verifier_session", baseline :: FixBaseline, progress :: FixProgress}


data GuestSession = AdmissionReady | SourceReady | FixReady FixSession


data FlowState = FlowState {progress :: FixProgress, remaining :: Int}


data FlowFailure = ToolRejected Text | Fatal Text


type Flow = ExceptT FlowFailure (State.StateT FlowState IO)


data FixActions = FixActions {invoke :: Policy.Invocation -> Flow AE.Value, finish :: Native.FixSummary -> Flow FixResult}


-- Only the artifact interpreter can construct completion proof.
newtype VerifiedFix = VerifiedFix FixResult


verifiedFixResult :: VerifiedFix -> FixResult
verifiedFixResult (VerifiedFix result) = result


-- Only the independent review interpreter can combine verification and approval.
data ReviewedFix = ReviewedFix VerifiedFix (UUIDId "agent_attempt") Text


reviewedFixResult :: ReviewedFix -> FixResult
reviewedFixResult (ReviewedFix verified _ _) = verifiedFixResult verified


reviewedFixApproval :: ReviewedFix -> (UUIDId "agent_attempt", Text)
reviewedFixApproval (ReviewedFix _ attempt rationale) = (attempt, rationale)


data FixReviewOutcome = Reviewed ReviewedFix | ChangesNeeded (NonEmpty Text) | ReviewUnavailable (NonEmpty Text)


data ExplainedFix = ExplainedFix FixResult Explanation


explainedFixResult :: ExplainedFix -> FixResult
explainedFixResult (ExplainedFix result _) = result


fixExplanation :: ExplainedFix -> Explanation
fixExplanation (ExplainedFix _ explanation) = explanation


data CodeReview :: Effect where
  ExplainVerifiedFix :: ArtifactStore -> ArtifactRequest -> VerifiedFix -> CodeReview m (Either Text ExplainedFix)
  VerifyFixArtifacts :: ArtifactStore -> ArtifactRequest -> CodeReview m (Either Text VerifiedFix)
  ReviewVerifiedFix :: FixReviewRequest -> VerifiedFix -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (AllocationReceipt -> m ()) -> CodeReview m (Either Text FixReviewOutcome)
  ReviewSource :: ReviewMode -> Request "pr_review" -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (AllocationReceipt -> m ()) -> (CheckReceipt -> m ()) -> (DraftSubmission -> m (Either Text ())) -> CodeReview m (Either Text Text)
  ReviewPatch :: Request "pr_review" -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (Text -> m (Either Text ())) -> CodeReview m (Either Text Text)
  ReviewRepository :: Request "pr_review" -> (TelemetryQuery -> m (Either Text AE.Value)) -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (AllocationReceipt -> m ()) -> (CheckReceipt -> m ()) -> (AE.Value -> m (Either Text ())) -> CodeReview m (Either Text Text)
  AdmitRegression :: AdmissionRequest -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (AllocationReceipt -> m ()) -> CodeReview m (Either Text AdmissionDecision)
  FixRepository :: FixRequest -> (TelemetryQuery -> m (Either Text AE.Value)) -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (TestArtifact -> m (Either Text Text)) -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (AllocationReceipt -> m ()) -> (CheckReceipt -> m ()) -> (PhaseReceipt -> m (Either Text ())) -> (FixResult -> m (Either Text ReviewedFix)) -> CodeReview m (Either Text ReviewedFix)


type instance DispatchOf CodeReview = 'Dynamic


makeEffect ''CodeReview


data DraftFormat = JsonDraft | RawText


data Task format where
  PatchTask :: Request "pr_review" -> Task 'RawText
  SourcePatchTask :: Request "pr_review" -> Task 'RawText
  SourceTask :: Request "pr_review" -> Task 'JsonDraft
  AdvisoryTask :: Request "pr_review" -> Task 'JsonDraft
  FixTask :: FixRequest -> Task 'JsonDraft
  AdmissionTask :: AdmissionRequest -> Task 'JsonDraft
  PeerTask :: FixReviewRequest -> Task 'JsonDraft


data DraftValidator format result where
  StructuredDraft :: (AE.Value -> IO (Either Text result)) -> DraftValidator 'JsonDraft result
  DraftText :: (Text -> IO (Either Text result)) -> DraftValidator 'RawText result


data Callbacks format result = Callbacks
  { respond :: TelemetryQuery -> IO (Either Text AE.Value)
  , knowledge :: KnowledgeRequest -> IO (Either Text AE.Value)
  , admit :: TestArtifact -> IO (Either Text Text)
  , calls :: ModelCall -> IO ()
  , usage :: ModelUsage -> IO ()
  , allocations :: Maybe (AllocationReceipt -> IO ())
  , checks :: Maybe (CheckReceipt -> IO ())
  , phase :: PhaseReceipt -> IO (Either Text ())
  , validate :: DraftValidator format result
  }


runCodeReviewReal :: IOE :> es => Maybe ArtifactStore -> Maybe FilePath -> [FilePath] -> Eff (CodeReview ': es) a -> Eff es a
runCodeReviewReal artifactStore executable skills = interpret \env -> \case
  ReviewRepository request respond knowledge recordCall recordUsage recordAllocation recordCheck check -> localSeqUnliftIO env \unlift ->
    liftIO
      $ launch
        (AdvisoryTask request)
        Callbacks
          { respond = unlift . respond
          , knowledge = unlift . knowledge
          , admit = const $ pure $ Left "Test admission requires an issue-fix task"
          , calls = unlift . recordCall
          , usage = unlift . recordUsage
          , allocations = Just $ unlift . recordAllocation
          , checks = Just $ unlift . recordCheck
          , phase = const $ pure $ Left "Fix phases require an issue-fix task"
          , validate = StructuredDraft $ \value -> (decodeUtf8 (AE.encode value) <$) <$> unlift (check value)
          }
  ReviewSource mode request recordCall recordUsage recordAllocation recordCheck check -> localSeqUnliftIO env \unlift -> liftIO do
    let callbacks :: forall format. Maybe (AllocationReceipt -> IO ()) -> Maybe (CheckReceipt -> IO ()) -> DraftValidator format Text -> Callbacks format Text
        callbacks allocations checks validator =
          Callbacks
            { respond = const $ pure $ Left "Source review has no telemetry tools"
            , knowledge = const $ pure $ Left "Source review has no project memory tools"
            , admit = const $ pure $ Left "Source review cannot admit tests"
            , calls = unlift . recordCall
            , usage = unlift . recordUsage
            , allocations
            , checks
            , phase = const $ pure $ Left "Source review cannot advance fix phases"
            , validate = validator
            }
        validateText raw = (raw <$) <$> unlift (check $ ProviderText raw)
        validateValue value = (decodeUtf8 (AE.encode value) <$) <$> unlift (check $ Structured value)
    case mode of
      BoundedPatch -> launch (SourcePatchTask request) $ callbacks Nothing Nothing $ DraftText validateText
      CheckoutHarness -> launch (SourceTask request) $ callbacks (Just $ unlift . recordAllocation) (Just $ unlift . recordCheck) $ StructuredDraft validateValue
  ReviewPatch request recordCall recordUsage check -> localSeqUnliftIO env \unlift ->
    liftIO
      $ launch
        (PatchTask request)
        Callbacks
          { respond = const $ pure $ Left "Patch comparison has no telemetry tools"
          , knowledge = const $ pure $ Left "Patch comparison has no knowledge tools"
          , admit = const $ pure $ Left "Patch comparison cannot admit tests"
          , calls = unlift . recordCall
          , usage = unlift . recordUsage
          , allocations = Nothing
          , checks = Nothing
          , phase = const $ pure $ Left "Patch comparison cannot advance fix phases"
          , validate = DraftText $ \raw -> (raw <$) <$> unlift (check raw)
          }
  FixRepository request respond knowledge admit recordCall recordUsage recordAllocation recordCheck recordPhase check -> localSeqUnliftIO env \unlift ->
    liftIO
      $ launch
        (FixTask request)
        Callbacks
          { respond = unlift . respond
          , knowledge = unlift . knowledge
          , admit = unlift . admit
          , calls = unlift . recordCall
          , usage = unlift . recordUsage
          , allocations = Just $ unlift . recordAllocation
          , checks = Just $ unlift . recordCheck
          , phase = unlift . recordPhase
          , validate = StructuredDraft $ \value -> case AE.fromJSON @FixResult value of
              AE.Error _ -> pure $ Left "Fix result does not match the required schema"
              AE.Success result -> (>>= matchingReview result) <$> unlift (check result)
          }
  AdmitRegression request knowledge recordCall recordUsage recordAllocation -> localSeqUnliftIO env \unlift ->
    liftIO
      $ launch
        (AdmissionTask request)
        Callbacks
          { respond = const $ pure $ Left "Admission has no production query capability"
          , knowledge = unlift . knowledge
          , admit = const $ pure $ Left "Admission cannot recursively admit a test"
          , calls = unlift . recordCall
          , usage = unlift . recordUsage
          , allocations = Just $ unlift . recordAllocation
          , checks = Nothing
          , phase = const $ pure $ Left "Admission cannot advance a fix phase"
          , validate = StructuredDraft $ \value -> pure $ case AE.fromJSON @AdmissionResult value of
              AE.Success result
                | result.artifact == request.artifact
                , let rationale = case result.decision of Admitted text -> text; Rejected text -> text
                , not (T.null $ T.strip rationale) && T.length rationale <= 1200 ->
                    Right result.decision
              _ -> Left "Admission result must bind the exact test artifact and a nonempty rationale"
          }
  ReviewVerifiedFix request verified knowledge recordCall recordUsage recordAllocation -> localSeqUnliftIO env \unlift ->
    if not (validReviewContext request verified)
      then pure $ Left "Peer review requires the same verified candidate and a distinct attempt"
      else
        liftIO
          $ launch
            (PeerTask request)
            Callbacks
              { respond = const $ pure $ Left "Peer review has no production query capability"
              , knowledge = unlift . knowledge
              , admit = const $ pure $ Left "Peer review cannot admit tests"
              , calls = unlift . recordCall
              , usage = unlift . recordUsage
              , allocations = Just $ unlift . recordAllocation
              , checks = Nothing
              , phase = const $ pure $ Left "Peer review cannot advance fix phases"
              , validate = StructuredDraft $ \value -> pure $ case AE.fromJSON @FixReviewResult value of
                  AE.Success response | response.result == request.artifacts.result -> reviewOutcome request verified response.decision
                  _ -> Left "Peer review must bind the exact verified candidate"
              }
  VerifyFixArtifacts store request -> liftIO $ (VerifiedFix request.result <$) <$> Native.verifyArtifacts store request
  ExplainVerifiedFix store request (VerifiedFix result)
    | request.result /= result -> pure $ Left "Explanation requires the same verified candidate"
    | otherwise -> liftIO do
        rendered <- Media.renderExplanation store request
        pure $ rendered >>= \explanation -> if validExplanation result explanation then Right $ ExplainedFix result explanation else Left "Explanation must bind the verified candidate and complete media manifest"
  where
    launch :: forall format result. Task format -> Callbacks format result -> IO (Either Text result)
    launch task callbacks = case executable of
      Nothing -> pure $ Left "Checkout-backed review agent is not configured"
      Just command -> do
        let tagged :: AE.ToJSON value => Text -> value -> AE.Value
            tagged tag contents = AE.object ["tag" AE..= tag, "contents" AE..= contents]
            (scope, source, payload, role) = case task of
              PatchTask request -> (Outbox.Advisory request.runId, Checkout.Repository request, tagged "PatchTask" request{gitToken = ""}, Policy.Patch)
              SourcePatchTask request -> (Outbox.Advisory request.runId, Checkout.Repository request, tagged "PatchTask" request{gitToken = ""}, Policy.SourcePatch)
              SourceTask request -> (Outbox.Advisory request.runId, Checkout.Repository request, AE.toJSON request{gitToken = ""}, Policy.Source)
              AdvisoryTask request -> (Outbox.Advisory request.runId, Checkout.Repository request, AE.toJSON request{gitToken = ""}, Policy.Advisory)
              FixTask request -> (Outbox.IssueFix request.request.runId, Checkout.Repository request.request{head = request.request.base}, tagged "FixTask" request{request = request.request{gitToken = ""}}, Policy.Fix)
              AdmissionTask request -> (Outbox.Admission request.fix.request.runId, Checkout.Repository request.fix.request{head = request.fix.request.base}, tagged "AdmitTask" (request{fix = request.fix{request = request.fix.request{gitToken = ""}}} :: AdmissionRequest), Policy.Admission)
              PeerTask request -> (Outbox.PeerReview request.fix.request.runId, Checkout.VerifiedCandidate request.artifacts, tagged "ReviewFixTask" (request{fix = request.fix{request = request.fix.request{gitToken = ""}}} :: FixReviewRequest), Policy.Peer)
            record = \case
              Outbox.Call receipt -> callbacks.calls receipt $> Right ()
              Outbox.Usage receipt -> callbacks.usage receipt $> Right ()
              Outbox.Allocation receipt -> maybe (pure $ Left "This task cannot allocate a sandbox") (\save -> save receipt $> Right ()) callbacks.allocations
              Outbox.Check receipt -> maybe (pure $ Left "This task cannot record regression executions") (\save -> save receipt $> Right ()) callbacks.checks
              Outbox.Phase receipt -> callbacks.phase receipt
            withJournal action = maybe (action Nothing) (\configured -> Outbox.withOutbox configured scope $ action . Just) artifactStore
        -- Replay persisted work before starting a child, fetching source or calling a model.
        outcome <- tryAny $ Timeout.timeout (480 * 1000000) $ withJournal \journal -> do
          recovered <- maybe (pure $ Right ()) (`Outbox.replay` record) journal
          case recovered of
            Left message -> pure $ Left message
            Right () -> do
              environment <- getEnvironment
              let childEnvironment = maybe environment (\outbox -> ("IMPACT_REVIEW_OUTBOX_DIR", Outbox.outboxPath outbox) : filter ((/= "IMPACT_REVIEW_OUTBOX_DIR") . fst) environment) journal
              withProcessTerm (setEnv childEnvironment $ setStdin createPipe $ setStdout createPipe $ setStderr nullStream $ proc command skills) \process -> do
                let stop =
                      ( do
                          hClose $ getStdin process
                          -- Cleanup is masked; an exception-based timeout cannot end this wait.
                          elapsed <- STM.registerDelay 5000000
                          STM.atomically $ void (waitExitCodeSTM process) `STM.orElse` (STM.readTVar elapsed >>= STM.check)
                      )
                        `finally` stopProcess process
                    writeMessage :: AE.ToJSON value => value -> IO ()
                    writeMessage value = LBS.hPut (getStdin process) (AE.encode value <> "\n") >> hFlush (getStdin process)
                    parseMessage :: AE.Value -> AET.Parser (Either Outbox.Receipt Message)
                    parseMessage value = (Left <$> AE.parseJSON value) <|> (Right <$> AE.parseJSON value)
                    rpc :: SandboxCommand -> (Message -> Maybe (Either Text value)) -> IO (Either Text value)
                    rpc instruction extract = do
                      writeMessage instruction
                      response <- AE.eitherDecodeStrict <$> BS.hGetLine (getStdout process)
                      pure $ case response of
                        Right message | Just output <- extract message -> output
                        _ -> Left "Guest returned an invalid workspace response"
                    execute privilege argv = rpc (Guest $ GuestCommand argv 1048576 privilege) \case GuestResult output -> Just output; _ -> Nothing
                    copy hostPath guestPath = rpc (CopyGuest $ GuestCopy hostPath guestPath) \case CopiedGuest output -> Just output; _ -> Nothing
                    protectWorkspace = for_ (["/usr/bin/git" :| ["config", "--system", "--add", "safe.directory", "/workspace"], "/bin/chown" :| ["-hR", "0:0", "/workspace"], "/bin/chmod" :| ["-R", "u+rwX,go+rX,go-w", "/workspace"], "/usr/bin/git" :| ["update-index", "--refresh"]] :: [NonEmpty Text]) $ void . ExceptT . execute Supervisor
                    prepareAdmission :: ArtifactStore -> TestArtifact -> IO (Either Text ())
                    prepareAdmission configured artifact = runExceptT do
                      let bounded limit text = not (T.null $ T.strip text) && T.length text <= limit
                      unless (bounded 65536 artifact.script && bounded 262144 artifact.testPatch && bounded 200 artifact.failureMessage) $ throwE "Regression artifact must contain bounded script, patch and failure message"
                      protectWorkspace
                      for_ ([("/run/monoscope-admission.patch", encodeUtf8 artifact.testPatch), ("/run/monoscope-admission.json", toStrict $ AE.encode artifact)] :: [(Text, ByteString)]) \(destination, bytes) -> ExceptT $ withTempFile configured.cacheDir ".admission-" \path handle -> do
                        BS.hPut handle bytes
                        hFlush handle
                        copy path destination
                      stats <- ExceptT $ execute Supervisor $ "/usr/bin/git" :| ["apply", "--recount", "--numstat", "-z", "/run/monoscope-admission.patch"]
                      let paths = filter (not . T.null) $ T.splitOn "\0" stats
                      unless (not (null paths) && length paths <= 100) $ throwE "Regression patch requires between one and 100 paths"
                      void $ ExceptT $ execute Supervisor $ "/usr/bin/git" :| ["-c", "core.hooksPath=/dev/null", "apply", "--recount", "--index", "/run/monoscope-admission.patch"]
                      void $ ExceptT $ execute Supervisor $ "/bin/chmod" :| ["0444", "/run/monoscope-admission.patch", "/run/monoscope-admission.json"]
                      protectWorkspace
                    capturePatch base = runExceptT do
                      void $ ExceptT $ execute Agent $ "/usr/bin/git" :| ["add", "-A"]
                      ExceptT $ execute Agent $ "/usr/bin/git" :| ["diff", "--cached", "--binary", "--no-ext-diff", "--no-textconv", "--no-color", "--no-relative", "--src-prefix=a/", "--dst-prefix=b/", base, "--"]
                    validTool invocation = invocation.name `elem` Policy.toolsForRole role && null (Schema.validateJSON mempty (Policy.toolDefinition role invocation.name).parameters invocation.arguments)
                    workflow :: FixRequest -> Checkout.Prepared -> Outbox.Outbox -> FixSession -> FixActions
                    workflow request checkout outbox session =
                      let reject :: Text -> Flow value
                          reject = throwE . ToolRejected
                          fatal :: Text -> Flow value
                          fatal = throwE . Fatal
                          io :: IO (Either Text value) -> Flow value
                          io action = liftIO action >>= either reject pure
                          requireBudget count = do
                            current <- lift State.get
                            when (current.remaining < count) $ fatal "Review agent exceeded its query budget"
                          spend count = requireBudget count >> lift (State.modify' \current -> current{remaining = current.remaining - count})
                          progress :: Flow FixProgress
                          progress = (.progress) <$> lift State.get
                          advance value = lift $ State.modify' \current -> (current{progress = value} :: FlowState)
                          phase step = do
                            spend 1
                            liftIO (Outbox.deliverPhase outbox request.request.attemptId step record) >>= either fatal pure
                          root arguments = void $ io $ execute Supervisor arguments
                          checks = Outbox.outboxPath outbox </> "checks"
                          assertWorkspace captured = do
                            patch <- io $ capturePatch request.request.base
                            unless (Native.candidateMatchesPatch captured (Native.candidateIdentity captured) patch) $ reject "Workspace changed; propose and verify a new candidate"
                          comparison artifact revisions = do
                            requireBudget $ length revisions
                            let input = AE.object ["projectId" AE..= request.request.projectId, "runId" AE..= request.request.runId, "attemptId" AE..= request.request.attemptId, "sessionId" AE..= session.id, "base" AE..= request.request.base, "imageBuildId" AE..= request.imageBuildId, "workspace" AE..= session.baseline, "artifact" AE..= artifact, "revisions" AE..= revisions]
                            result <- io $ withTempFile checks ".regression-" \path handle -> do
                              LBS.hPut handle $ AE.encode input
                              hFlush handle
                              rpc (RunCheck path) \case CheckedRegression output -> Just output; _ -> Nothing
                            for_ result.receipts \receipt -> do
                              liftIO (Outbox.deliver outbox (Outbox.Check receipt) record) >>= either fatal pure
                              phase $ Observe receipt
                            for_ result.receipts \receipt -> do
                              let side = case receipt.side of Base -> "base"; Head -> "head"
                              io $ copy (checks </> toString receipt.checkId.toText </> toString side <> ".log") ("/run/monoscope-review-output/check-" <> receipt.checkId.toText <> "-" <> side <> ".log")
                            when result.isError $ reject $ "Native regression did not produce usable evidence.\n" <> decodeUtf8 (AE.encode result.report)
                            pure result
                          describe message result = AE.object ["message" AE..= (message :: Text), "report" AE..= result.report]
                          fingerprint = \case Just (InjectedTest value) -> Just value; Just (RetainedTest value) -> Just value; _ -> Nothing
                          invoke invocation = do
                            let parsed :: (AE.Value -> AET.Parser value) -> Flow value
                                parsed parser = either (reject . toText) pure $ AET.parseEither parser invocation.arguments
                            case invocation.name of
                              Policy.ReproduceIssue -> do
                                progress >>= \case NeedsReproduction -> pass; _ -> reject "A regression is already frozen for this run"
                                ReproductionInput script failureMessage patch <- parsed AE.parseJSON
                                testPatch <- case patch of
                                  InlinePatch text -> pure text
                                  PatchFile path -> do
                                    text <- io $ rpc (ReadGuest $ GuestRead path 262145) \case GuestResult output -> Just output; _ -> Nothing
                                    when (BS.length (encodeUtf8 text) > 262144) $ reject "Regression patch file exceeds its 256 KiB limit"
                                    pure text
                                when (T.null testPatch || T.length testPatch > 262144) $ reject "Regression patch must be nonempty and within its limit"
                                let artifact = TestArtifact script testPatch failureMessage
                                spend 1
                                rationale <- io $ callbacks.admit artifact
                                when (T.null $ T.strip rationale) $ reject "Host admission requires a recorded rationale"
                                liftIO $ Native.retainAdmission checks request artifact rationale
                                result <- comparison artifact $ (Base, checkout.base) :| []
                                receipt <- case result.receipts of
                                  [receipt] | receipt.side == Base && receipt.status == Failed && isJust (fingerprint receipt.regression) -> pure receipt
                                  _ -> reject "The admitted regression did not fail on base; source edits remain locked"
                                io $ copy (checks </> toString receipt.checkId.toText </> "test.patch") "/run/monoscope-admitted-test.patch"
                                case receipt.regression of
                                  Just RetainedTest{} -> pass
                                  _ -> root $ "/usr/bin/git" :| ["-c", "core.hooksPath=/dev/null", "apply", "--recount", "--index", "/run/monoscope-admitted-test.patch"]
                                root $ "/bin/chown" :| ["-hR", "1000:1000", "/workspace"]
                                advance $ Reproduced artifact
                                pure $ describe "Regression reproduced and frozen. Source editing is now available." result
                              Policy.ProposeFix -> do
                                artifact <-
                                  progress >>= \case
                                    NeedsReproduction -> reject "Reproduce the issue before proposing a fix"
                                    Reproduced artifact -> pure artifact
                                    Captured artifact _ -> pure artifact
                                    Checked artifact captured _ -> advance (Captured artifact captured) $> artifact
                                cause <- parsed $ AE.withObject "Fix proposal" (AE..: "rootCause")
                                requireBudget 2
                                spend 1
                                snapshot <- io $ capturePatch request.request.base
                                patch <- either reject pure $ Native.mkCandidatePatch snapshot cause
                                captured <- io $ Native.captureCandidate checks request checkout.directory patch
                                phase $ Native.candidateStep captured
                                advance $ Captured artifact captured
                                pure $ AE.toJSON captured
                              Policy.VerifyFix -> do
                                (artifact, captured) <-
                                  progress >>= \case
                                    Captured artifact captured -> pure (artifact, captured)
                                    Checked artifact captured _ -> pure (artifact, captured)
                                    _ -> reject "Capture a candidate after reproduction first"
                                advance $ Captured artifact captured
                                assertWorkspace captured
                                result <- comparison artifact $ (Base, checkout.base) :| [(Head, Native.candidateRevision captured)]
                                receipt <- case result.receipts of
                                  [baseline, candidate]
                                    | baseline.side == Base && baseline.status == Failed && candidate.side == Head && candidate.status == Passed
                                    , Just RetainedTest{} <- candidate.regression
                                    , baseline.checkId == candidate.checkId && fingerprint baseline.regression == fingerprint candidate.regression ->
                                        pure candidate
                                  _ -> reject $ "Candidate verification did not establish a failing base and passing retained test.\n" <> decodeUtf8 (AE.encode result.report)
                                advance $ Checked artifact captured receipt
                                pure $ describe "Frozen regression fails on base and passes on the candidate with its test retained." result
                              _ -> reject "This tool is not a fix workflow operation"
                          finish summary = do
                            (captured, receipt) <-
                              progress >>= \case
                                Checked _ captured receipt -> pure (captured, receipt)
                                _ -> reject "Capture and verify a source fix before submission"
                            assertWorkspace captured
                            either reject pure $ Native.candidateResult captured receipt summary
                       in FixActions invoke finish
                    receive :: Maybe Checkout.Prepared -> Maybe GuestSession -> Int -> Int -> Maybe (AE.Value, result) -> IO (Either Text result)
                    receive supplied guestSession remaining submissions accepted = do
                      message <- (AE.eitherDecodeStrict >=> AET.parseEither parseMessage) <$> BS.hGetLine (getStdout process)
                      case message of
                        Left _ -> pure $ Left "Review agent returned an invalid protocol message"
                        Right (Left receipt) -> do
                          let phase = case receipt of Outbox.Phase _ -> True; _ -> False
                              invalidates = case receipt of Outbox.Check _ -> True; Outbox.Phase _ -> True; _ -> False
                          if phase && remaining <= 0
                            then pure $ Left "Review agent exceeded its query budget"
                            else do
                              prepared <- case (task, receipt, guestSession) of
                                (SourceTask{}, Outbox.Call{}, Nothing) -> fmap (const $ Just SourceReady) <$> runExceptT protectWorkspace
                                _ -> pure $ Right guestSession
                              case prepared of
                                Left failure -> writeMessage (Left failure :: Either Text ()) $> Left failure
                                Right ready -> do
                                  saved <- maybe (record receipt) (\outbox -> Outbox.acknowledge outbox receipt record) journal
                                  writeMessage saved
                                  case saved of
                                    Left failure | not phase -> pure $ Left failure
                                    _ -> receive supplied ready (remaining - bool 0 1 phase) submissions (if invalidates then Nothing else accepted)
                        Right (Right (Checkout ())) -> case (supplied, artifactStore) of
                          (Nothing, Just configured) -> do
                            prepared <- Checkout.withCheckout configured source \checkout ->
                              (writeMessage (Right checkout{Checkout.policy = Policy.policy role (revisionText checkout.base) (revisionText checkout.head) checkout.rootInstructions} :: Either Text Checkout.Prepared) >> receive (Just checkout) guestSession remaining submissions accepted) `finally` stop
                            pure $ join prepared
                          _ -> pure $ Left "Repository checkout was unavailable or already supplied"
                        Right (Right (Tool invocation))
                          | isNothing supplied -> pure $ Left "Tool invocation requires its repository checkout"
                          | invocation.name `elem` [Policy.ReproduceIssue, Policy.ProposeFix, Policy.VerifyFix] && validTool invocation -> case (task, supplied, journal, guestSession) of
                              (FixTask request, Just checkout, Just outbox, Just (FixReady session)) -> do
                                (outcome, current) <- State.runStateT (runExceptT $ (workflow request checkout outbox session).invoke invocation) (FlowState session.progress remaining)
                                case outcome of
                                  Left (Fatal failure) -> writeMessage (Left failure :: Either Text AE.Value) $> Left failure
                                  _ -> do
                                    writeMessage $ first (\case ToolRejected failure -> failure; Fatal failure -> failure) outcome
                                    receive supplied (Just $ FixReady session{progress = current.progress}) current.remaining submissions Nothing
                              _ -> pure $ Left "Fix tools require an initialized issue-fix session"
                          | remaining > 0 -> do
                              let parsed :: (AE.Value -> AET.Parser value) -> (value -> IO (Either Text AE.Value)) -> IO (Either Text AE.Value)
                                  parsed parser = whenRight (Left "Tool arguments do not match the host contract") (AET.parseEither parser invocation.arguments)
                                  property :: AE.FromJSON value => AE.Key -> (value -> IO (Either Text AE.Value)) -> IO (Either Text AE.Value)
                                  property key = parsed $ AE.withObject "Tool arguments" (AE..: key)
                                  request = case invocation.name of
                                    Policy.QueryTelemetry -> parsed AE.parseJSON callbacks.respond
                                    Policy.RecallKnowledge -> property "service" $ callbacks.knowledge . Recall
                                    Policy.RememberKnowledge -> parsed AE.parseJSON $ callbacks.knowledge . Remember
                                    Policy.ListProjectMemory -> callbacks.knowledge $ ProjectMemory Memory.List
                                    Policy.ReadProjectMemory -> property "pageId" $ callbacks.knowledge . ProjectMemory . Memory.Read
                                    Policy.WriteProjectMemory -> parsed AE.parseJSON $ callbacks.knowledge . ProjectMemory . Memory.Write
                                    _ -> pure $ Left "This tool requires a native workflow handler"
                              response <- if validTool invocation then request else pure $ Left "Tool is unavailable or its arguments violate the host contract"
                              writeMessage response
                              receive supplied guestSession (remaining - 1) submissions Nothing
                          | otherwise -> pure $ Left "Review agent exceeded its query budget"
                        Right (Right (Query query))
                          | remaining > 0 -> callbacks.respond query >>= writeMessage >> receive supplied guestSession (remaining - 1) submissions Nothing
                          | otherwise -> pure $ Left "Review agent exceeded its query budget"
                        Right (Right (Knowledge requestKnowledge))
                          | remaining > 0 -> callbacks.knowledge requestKnowledge >>= writeMessage >> receive supplied guestSession (remaining - 1) submissions Nothing
                          | otherwise -> pure $ Left "Review agent exceeded its query budget"
                        Right (Right (Admit artifact))
                          | isNothing supplied && isJust artifactStore -> pure $ Left "Review agent has not acquired its repository checkout"
                          | remaining > 0 -> do
                              admitted <- callbacks.admit artifact
                              saved <- case (task, admitted) of
                                (FixTask request, Right rationale)
                                  | not (T.null $ T.strip rationale) -> for_ journal (\outbox -> Native.retainAdmission (Outbox.outboxPath outbox <> "/checks") request artifact rationale) $> admitted
                                  | otherwise -> pure $ Left "Host admission requires a recorded rationale"
                                _ -> pure admitted
                              writeMessage saved
                              receive supplied guestSession (remaining - 1) submissions Nothing
                          | otherwise -> pure $ Left "Review agent exceeded its query budget"
                        Right (Right (InitializeAdmission ())) -> case (task, supplied, artifactStore, guestSession) of
                          (AdmissionTask request, Just _, Just configured, Nothing) | remaining > 0 -> do
                            prepared <- prepareAdmission configured request.artifact
                            writeMessage prepared
                            case prepared of
                              Left failure -> pure $ Left failure
                              Right () -> receive supplied (Just AdmissionReady) (remaining - 1) submissions Nothing
                          _ -> pure $ Left "Admission initialization requires a prepared admission task and remaining budget"
                        Right (Right (RestartFix baseline)) -> case (task, supplied, journal, guestSession) of
                          (FixTask request, Just _, Just outbox, Nothing) | remaining > 0 -> do
                            protected <- runExceptT protectWorkspace
                            case protected of
                              Left failure -> writeMessage protected $> Left failure
                              Right () -> do
                                saved <- Outbox.deliverPhase outbox request.request.attemptId RestartReproduction record
                                writeMessage saved
                                case saved of
                                  Left failure -> pure $ Left failure
                                  Right () -> do
                                    identifier <- UUIDId <$> UUID4.nextRandom
                                    receive supplied (Just $ FixReady $ FixSession identifier baseline NeedsReproduction) (remaining - 1) submissions Nothing
                          _ -> pure $ Left "Fix initialization requires a prepared issue-fix task and remaining budget"
                        Right (Right (CompleteFix summary)) -> case (task, supplied, journal, guestSession) of
                          (FixTask request, Just checkout, Just outbox, Just (FixReady session)) | submissions > 0 -> do
                            (outcome, current) <- State.runStateT (runExceptT $ (workflow request checkout outbox session).finish summary) (FlowState session.progress remaining)
                            case outcome of
                              Left (Fatal failure) -> writeMessage (Left failure :: Either Text FixResult) $> Left failure
                              Left (ToolRejected failure) -> writeMessage (Left failure :: Either Text FixResult) >> receive supplied (Just $ FixReady session{progress = current.progress}) current.remaining submissions Nothing
                              Right draft -> do
                                checked <- case callbacks.validate of
                                  StructuredDraft validate -> validate $ AE.toJSON draft
                                writeMessage $ draft <$ checked
                                receive supplied (Just $ FixReady session{progress = current.progress}) current.remaining (submissions - 1) $ either (const Nothing) (Just . (AE.toJSON draft,)) checked
                          _ -> pure $ Left "Fix submission requires an initialized session and remaining budget"
                        Right (Right GuestResult{}) -> pure $ Left "Guest response has no pending workspace command"
                        Right (Right CopiedGuest{}) -> pure $ Left "Copy response has no pending workspace command"
                        Right (Right CheckedRegression{}) -> pure $ Left "Check response has no pending workspace command"
                        Right (Right (Validate draft))
                          | isNothing supplied && isJust artifactStore -> pure $ Left "Review agent has not acquired its repository checkout"
                          | SourceTask{} <- task, isJust artifactStore, Just SourceReady <- guestSession, submissions > 0 -> accept
                          | SourceTask{} <- task, isJust artifactStore -> pure $ Left "Source review requires a protected workspace and remaining submission budget"
                          | AdmissionTask{} <- task, isJust artifactStore, Just AdmissionReady <- guestSession, submissions > 0 -> accept
                          | AdmissionTask{} <- task, isJust artifactStore -> pure $ Left "Admission requires an initialized workspace and remaining submission budget"
                          | submissions > 0 -> accept
                          | otherwise -> pure $ Left "Review agent exceeded its submission budget"
                          where
                            accept = do
                              checked <- case callbacks.validate of
                                StructuredDraft validate -> validate draft
                                DraftText{} -> pure $ Left "Review submission requires structured validation"
                              writeMessage $ void checked
                              receive supplied guestSession remaining (submissions - 1) $ either (const Nothing) (Just . (draft,)) checked
                        Right (Right (ValidateText raw)) -> case (callbacks.validate, supplied) of
                          (DraftText validate, Just _) | submissions > 0 -> do
                            checked <- validate raw
                            writeMessage $ void checked
                            receive supplied guestSession remaining (submissions - 1) $ either (const Nothing) (Just . (AE.String raw,)) checked
                          _ -> pure $ Left "Text submission requires a prepared patch comparison and remaining budget"
                        Right (Right (Result draft)) -> case accepted of
                          Just (acceptedDraft, result) | acceptedDraft == draft -> do
                            status <- waitExitCode process
                            pure $ if status == ExitSuccess then Right result else Left "Review agent did not produce a validated result"
                          _ -> pure $ Left "Review agent returned an unvalidated draft"
                writeMessage payload
                receive Nothing Nothing (20 :: Int) (5 :: Int) Nothing
        pure case outcome of
          Left _ -> Left "Review agent could not be executed"
          Right Nothing -> Left "Review agent exceeded its eight-minute budget"
          Right (Just result) -> result


runCodeReviewTest :: IOE :> es => (Request "pr_review" -> Either Text Text) -> (FixRequest -> Either Text FixResult) -> (AdmissionRequest -> Either Text AdmissionDecision) -> (ArtifactRequest -> Either Text ()) -> (ArtifactRequest -> Either Text Explanation) -> (FixReviewRequest -> Either Text FixReviewDecision) -> Eff (CodeReview ': es) a -> Eff es a
runCodeReviewTest respond respondFix respondAdmission respondArtifacts respondExplanation respondReview = interpret \env -> \case
  ReviewRepository request _ _ _ _ _ _ _ -> pure $ respond request
  ReviewSource mode request _ _ _ _ check -> case respond request of
    Left message -> pure $ Left message
    Right raw -> case (case mode of BoundedPatch -> Right $ ProviderText raw; CheckoutHarness -> Structured <$> first (const "Source review returned invalid JSON") (AE.eitherDecodeStrict $ encodeUtf8 raw)) of
      Left failure -> pure $ Left failure
      Right candidate -> localSeqUnliftIO env $ \unlift -> (raw <$) <$> unlift (check candidate)
  ReviewPatch request _ _ check -> case respond request of
    Left message -> pure $ Left message
    Right raw -> localSeqUnliftIO env $ \unlift -> (raw <$) <$> unlift (check raw)
  FixRepository request _ _ _ _ _ _ _ _ check -> case respondFix request of
    Left message -> pure $ Left message
    Right result -> localSeqUnliftIO env $ \unlift -> (>>= matchingReview result) <$> unlift (check result)
  AdmitRegression request _ _ _ _ -> pure $ respondAdmission request
  ReviewVerifiedFix request verified _ _ _ _ -> pure $ respondReview request >>= reviewOutcome request verified
  VerifyFixArtifacts _ request -> pure $ VerifiedFix request.result <$ respondArtifacts request
  ExplainVerifiedFix _ request (VerifiedFix result) -> pure do
    unless (request.result == result) $ Left "Explanation requires the same verified candidate"
    explanation <- respondExplanation request
    unless (validExplanation result explanation) $ Left "Invalid explanation fixture"
    pure $ ExplainedFix result explanation


matchingReview :: FixResult -> ReviewedFix -> Either Text ReviewedFix
matchingReview result reviewed = if reviewedFixResult reviewed == result then Right reviewed else Left "Review approval belongs to a different candidate"


validReviewContext :: FixReviewRequest -> VerifiedFix -> Bool
validReviewContext request verified =
  let result = request.artifacts.result
      context = request.fix.request
   in result
        == verifiedFixResult verified
        && (context.projectId, context.runId, request.fix.issueId, context.base, context.repository, request.fix.imageBuildId.toText)
        == (result.projectId, result.runId, result.issueId, revisionText result.base, request.artifacts.repository, request.artifacts.baseline.imageBuildId)
        && context.attemptId
        /= result.attemptId


reviewOutcome :: FixReviewRequest -> VerifiedFix -> FixReviewDecision -> Either Text FixReviewOutcome
reviewOutcome request verified decision = do
  unless (validReviewContext request verified) $ Left "Peer review requires the same verified candidate and a distinct attempt"
  let bounded limit text = not (T.null $ T.strip text) && T.length text <= limit
      concerns constructor values = if length values <= 5 && all (bounded 600) values then Right $ constructor values else Left "Peer review concerns require one to five bounded explanations"
  case decision of
    ReviewApproved reason | bounded 1200 reason -> Right $ Reviewed $ ReviewedFix verified request.fix.request.attemptId reason
    ReviewApproved _ -> Left "Peer review approval requires a bounded rationale"
    ReviewChanges values -> concerns ChangesNeeded values
    ReviewBlocked values -> concerns ReviewUnavailable values
