{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.Effectful.CodeReview (CodeReview (..), SessionIdentity (..), SessionSubject (..), NativePause, FixContext (..), ReviewInput (..), ReviewMemory (..), ReviewContext (..), readSessionContext, prepareRepository, Request (..), FixRequest (..), TestArtifact (..), AdmissionRequest (..), AdmissionDecision (..), PhaseReceipt (..), FixResult (..), ArtifactRequest (..), ArtifactStore (..), FixReviewRequest (..), FixReviewDecision (..), ReviewedFix, reviewedFixResult, reviewedFixApproval, FixReviewOutcome (..), VerifiedFix, verifiedFixResult, ExplainedFix, explainedFixResult, fixExplanation, Explanation (..), PricingStatus (..), ThinkingLevel (..), ReviewMode (..), proofBudget, ProofHooks (..), ProofProgress (..), hoistHooks, DraftSubmission (..), TelemetryQuery (..), KnowledgeNote (..), KnowledgeRequest (..), ModelUsage (..), ModelCall (..), AllocationReceipt, allocationId, allocationAttempt, Regression (..), CheckReceipt (..), CheckStatus (..), RevisionSide (..), Revision, mkRevision, revisionText, reviewRepository, reviewPatch, reviewSource, fixRepository, admitRegression, verifyFixArtifacts, reviewVerifiedFix, explainVerifiedFix, runCodeReviewReal, runCodeReviewWith, workerHost, runCodeReviewTest, validExplanation) where

import Control.Concurrent.STM qualified as STM
import Control.Monad.Trans.Except (except, throwE)
import Control.Monad.Trans.State.Strict qualified as State
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AET
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Effectful.AgentWorkspace qualified as Workspace
import Data.Effectful.AgentWorkspace.Guest qualified as Guest
import Data.Effectful.CodeReview.Artifacts (ArtifactStore (..))
import Data.Effectful.CodeReview.Artifacts qualified as Native
import Data.Effectful.CodeReview.Checkout qualified as Checkout
import Data.Effectful.CodeReview.Explanation qualified as Media
import Data.Effectful.CodeReview.Outbox qualified as Outbox
import Data.Effectful.CodeReview.Policy qualified as Policy
import Data.Effectful.CodeReview.Types
import Data.OpenApi.Schema.Validation qualified as Schema
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID4
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Models.Projects.ProjectMemory qualified as Memory
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import System.Directory (listDirectory, removeFile, removePathForcibly)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose)
import System.IO.Error (isDoesNotExistError, tryIOError)
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (ExitCode (..), ProcessConfig, createPipe, getStdin, getStdout, nullStream, proc, setEnv, setStderr, setStdin, setStdout, stopProcess, waitExitCode, waitExitCodeSTM, withProcessTerm)
import UnliftIO.Async (forConcurrently)
import UnliftIO.Exception (catch, finally, throwIO, tryAny)
import UnliftIO.Timeout qualified as Timeout


-- JSON-lines protocol: queries, usage, execution receipts, validation, and result.
data GuestIdentity = Agent | Supervisor


data Message = Checkout () | Tool Policy.Invocation | Query TelemetryQuery | Knowledge KnowledgeRequest | Admit TestArtifact | Guest Guest.Operation | Validate AE.Value | ValidateText Text | Result AE.Value | Suspend ()
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


-- | @record_proof@ arguments; the tool schema calls the shell text @command@.
data ProofArgs = ProofArgs {claim :: Text, shellCommand :: Text, setup :: Maybe Text}
  deriving stock (Generic)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.Rename "shellCommand" "command"]] ProofArgs


-- | Proof attempts a review may make. Failed attempts count too: each one costs two VM runs.
proofBudget :: Int
proofBudget = 3


-- The report is opaque presentation data; decisions use the typed receipts.
data RegressionWire = RegressionWire {report :: AE.Value, receipts :: [CheckReceipt], isError :: Bool}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data RegressionPatch = InlinePatch Text | PatchFile Text | FileContents Workspace.TestFiles


data ReproductionInput = ReproductionInput Text Text RegressionPatch


instance AE.FromJSON ReproductionInput where
  parseJSON = AE.withObject "Issue reproduction" \fields -> do
    patch <-
      (,,) <$> fields AE..:? "testPatch" <*> fields AE..:? "testPatchFile" <*> fields AE..:? "testFiles" >>= \case
        (Just inline, Nothing, Nothing) -> pure $ InlinePatch inline
        (Nothing, Just path, Nothing) -> pure $ PatchFile path
        (Nothing, Nothing, Just files) -> pure $ FileContents files
        _ -> fail "Supply exactly one of testFiles, testPatch or testPatchFile"
    ReproductionInput <$> fields AE..: "script" <*> fields AE..: "failureMessage" <*> pure patch


data Reproduction = Reproduction {artifact :: TestArtifact, baseline :: CheckReceipt}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data FixProgress candidate = NeedsReproduction | Reproduced Reproduction | Captured Reproduction candidate | Checked Reproduction candidate CheckReceipt CheckReceipt
  deriving stock (Foldable, Functor, Generic, Traversable)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data FixSession = FixSession {id :: UUIDId "verifier_session", baseline :: Workspace.Baseline, progress :: FixProgress Native.CapturedCandidate}


data GuestSession = AdmissionReady | SourceReady | FixReady FixSession


data FlowState = FlowState {progress :: FixProgress Native.CapturedCandidate, remaining :: Int}


data NativePause = DecisionWait AgentAwaitingReview | ProviderWait
  deriving stock (Show)
  deriving anyclass (Exception)


data FlowFailure = ToolRejected Text | Fatal Text | Paused NativePause


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


data SessionIdentity m (context :: Type) = Isolated | Retained (UUIDId "ai_routine_run") (m context)


mapSession :: (m context -> n context) -> SessionIdentity m context -> SessionIdentity n context
mapSession transform = \case Isolated -> Isolated; Retained root capture -> Retained root $ transform capture


data SessionSubject context where
  ReviewSubject :: UUIDId "pr_review" -> SessionSubject ReviewContext
  FixSubject :: UUIDId "issue_fix" -> SessionSubject FixContext
  AdmissionSubject :: UUIDId "issue_fix" -> TestArtifact -> SessionSubject (UUIDId "agent_attempt")
  PeerSubject :: UUIDId "issue_fix" -> ArtifactRequest -> SessionSubject (UUIDId "agent_attempt")


queryLimit, submissionLimit :: Int
(queryLimit, submissionLimit) = (20, 5)


-- | @proofs@ is absent in checkpoints written before proof recording existed.
data SavedGuest = SavedGuest {id :: UUIDId "native_checkpoint", guest :: Guest.Suspended, sha256 :: Text, remaining :: Int, submissions :: Int, proofs :: Maybe Int}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


instance AE.FromJSON SavedGuest where
  parseJSON value = do
    saved <- AE.genericParseJSON AE.defaultOptions value
    unless (saved.remaining >= 0 && saved.remaining <= queryLimit && saved.submissions >= 0 && saved.submissions <= submissionLimit && all (\n -> n >= 0 && n <= proofBudget) saved.proofs) $ fail "Invalid native continuation budget"
    pure saved


data SavedFix = SavedFix {id :: UUIDId "verifier_session", progress :: FixProgress Native.CandidateSnapshot}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Suspension = AnalysisSuspended SavedGuest | FixSuspended SavedGuest SavedFix


data RetainedSession = RetainedSession {directory :: FilePath, persist :: NativeProgress -> IO (), saved :: Maybe Suspension}


data Accepted result = Accepted {submission :: AE.Value, validated :: AE.Value, result :: result}


data NativeProgress = Waiting SavedGuest | FixWaiting SavedGuest SavedFix | Finished AE.Value
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data NativeCheckpoint context = NativeCheckpoint {input :: AE.Value, progress :: NativeProgress, context :: context}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data CodeReview :: Effect where
  ReadSessionContext :: SessionSubject context -> UUIDId "ai_routine_run" -> CodeReview m (Either Text (Maybe context))
  ExplainVerifiedFix :: ArtifactStore -> ArtifactRequest -> VerifiedFix -> CodeReview m (Either Text ExplainedFix)
  VerifyFixArtifacts :: ArtifactStore -> ArtifactRequest -> CodeReview m (Either Text VerifiedFix)
  ReviewVerifiedFix :: SessionIdentity m (UUIDId "agent_attempt") -> FixReviewRequest -> VerifiedFix -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (AllocationReceipt -> m ()) -> CodeReview m (Either Text FixReviewOutcome)
  ReviewSource :: ReviewMode -> Request "pr_review" -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (AllocationReceipt -> m ()) -> (CheckReceipt -> m ()) -> (DraftSubmission -> m (Either Text ())) -> CodeReview m (Either Text Text)
  ReviewPatch :: Request "pr_review" -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (Text -> m (Either Text ())) -> CodeReview m (Either Text Text)
  ReviewRepository :: SessionIdentity m ReviewContext -> Request "pr_review" -> (TelemetryQuery -> m (Either Text AE.Value)) -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (AllocationReceipt -> m ()) -> (CheckReceipt -> m ()) -> ProofHooks m -> (AE.Value -> m (Either Text ())) -> CodeReview m (Either Text Text)
  -- | Builds the workspace checkpoint at the request's base, warm from the repository's latest
  -- build when one exists. Runs outside any review's budget; a no-op when the checkpoint exists.
  PrepareRepository :: Checkout.Fetch -> CodeReview m (Either Text ())
  AdmitRegression :: SessionIdentity m (UUIDId "agent_attempt") -> AdmissionRequest -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (AllocationReceipt -> m ()) -> CodeReview m (Either Text AdmissionDecision)
  FixRepository :: SessionIdentity m FixContext -> FixRequest -> (TelemetryQuery -> m (Either Text AE.Value)) -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (TestArtifact -> m (Either Text Text)) -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (AllocationReceipt -> m ()) -> (CheckReceipt -> m ()) -> (PhaseReceipt -> m (Either Text ())) -> (FixResult -> m (Either Text ReviewedFix)) -> CodeReview m (Either Text ReviewedFix)


type instance DispatchOf CodeReview = 'Dynamic


makeEffect ''CodeReview


data DraftFormat = JsonDraft | RawText


data Task format where
  PatchTask :: Request "pr_review" -> Task 'RawText
  SourcePatchTask :: Request "pr_review" -> Task 'RawText
  SourceTask :: Request "pr_review" -> Task 'JsonDraft
  AdvisoryTask :: SessionIdentity IO ReviewContext -> Request "pr_review" -> Task 'JsonDraft
  FixTask :: SessionIdentity IO FixContext -> FixRequest -> Task 'JsonDraft
  AdmissionTask :: SessionIdentity IO (UUIDId "agent_attempt") -> AdmissionRequest -> Task 'JsonDraft
  PeerTask :: SessionIdentity IO (UUIDId "agent_attempt") -> FixReviewRequest -> Task 'JsonDraft


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
  , proofs :: Maybe (ProofHooks IO)
  , phase :: PhaseReceipt -> IO (Either Text ())
  , validate :: DraftValidator format result
  }


runCodeReviewReal :: IOE :> es => Maybe ArtifactStore -> Either Text (Maybe Guest.Host) -> Maybe FilePath -> [FilePath] -> Eff (CodeReview ': es) a -> Eff es a
runCodeReviewReal artifactStore host executable skills = runCodeReviewWith artifactStore host ((\command environment -> setEnv environment $ setStderr nullStream $ proc command skills) <$> executable)


runCodeReviewWith :: IOE :> es => Maybe ArtifactStore -> Either Text (Maybe Guest.Host) -> Maybe ([(String, String)] -> ProcessConfig () () ()) -> Eff (CodeReview ': es) a -> Eff es a
runCodeReviewWith artifactStore hostConfig executable = interpret \env -> \case
  ReadSessionContext subject root -> liftIO do
    let load :: AE.FromJSON context => Outbox.Scope -> [FilePath] -> IO (Either Text (Maybe context))
        load scope key = case artifactStore of
          Nothing -> pure $ Right Nothing
          Just store -> do
            loaded <- tryAny $ Outbox.withOutbox store scope \outbox -> do
              directory <- Outbox.privateDirectory (Outbox.outboxPath outbox) $ ["sessions", toString root.toText] <> key
              fmap (.context) <$> readNativeCheckpoint directory
            pure $ first (const "Native context could not be restored") loaded
    case subject of
      ReviewSubject review -> load (Outbox.Advisory review) []
      FixSubject fid -> load (Outbox.IssueFix fid) []
      AdmissionSubject fid artifact -> load (Outbox.Admission fid) [nativeKey artifact]
      PeerSubject fid artifacts -> load (Outbox.PeerReview fid) [nativeKey artifacts]
  ReviewRepository sessionScope request respond knowledge recordCall recordUsage recordAllocation recordCheck hooks check -> localSeqUnliftIO env \unlift ->
    liftIO
      $ launch
        (AdvisoryTask (mapSession unlift sessionScope) request)
        Callbacks
          { respond = unlift . respond
          , knowledge = unlift . knowledge
          , admit = const $ pure $ Left "Test admission requires an issue-fix task"
          , calls = unlift . recordCall
          , usage = unlift . recordUsage
          , allocations = Just $ unlift . recordAllocation
          , checks = Just $ unlift . recordCheck
          , proofs = Just $ hoistHooks unlift hooks
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
            , proofs = Nothing
            , phase = const $ pure $ Left "Source review cannot advance fix phases"
            , validate = validator
            }
        validateText raw = (raw <$) <$> unlift (check $ ProviderText raw)
        validateValue value = (decodeUtf8 (AE.encode value) <$) <$> unlift (check $ Structured value)
    case mode of
      BoundedPatch -> launch (SourcePatchTask request) $ callbacks Nothing Nothing $ DraftText validateText
      CheckoutHarness -> launch (SourceTask request) $ callbacks (Just $ unlift . recordAllocation) (Just $ unlift . recordCheck) $ StructuredDraft validateValue
  PrepareRepository request -> liftIO case (artifactStore, hostConfig) of
    (Just store, Right (Just host))
      | Just configured <- host.workspace ->
          join
            . first show
            <$> tryAny
              ( join <$> Checkout.withCheckout store (Checkout.Repository request) \fetched _ ->
                  withSystemTempDirectory "monoscope-prepare-request" \directory -> runExceptT do
                    let requestFile = directory </> "request.json"
                    liftIO . writeFileLBS requestFile . AE.encode =<< except (Workspace.mkRequest request.projectId request.repository request.base Nothing)
                    void $ ExceptT $ runEff $ Workspace.runWorkspaceReal $ Workspace.runWorkspace (Workspace.Options store.cacheDir host.image host.bridge configured.script configured.dependencyArchive) (Workspace.Prepare fetched.directory) requestFile
              )
    _ -> pure $ Left "Workspace preparation is not configured on this host"
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
          , proofs = Nothing
          , phase = const $ pure $ Left "Patch comparison cannot advance fix phases"
          , validate = DraftText $ \raw -> (raw <$) <$> unlift (check raw)
          }
  FixRepository sessionScope request respond knowledge admit recordCall recordUsage recordAllocation recordCheck recordPhase check -> localSeqUnliftIO env \unlift ->
    liftIO
      $ launch
        (FixTask (mapSession unlift sessionScope) request)
        Callbacks
          { respond = unlift . respond
          , knowledge = unlift . knowledge
          , admit = unlift . admit
          , calls = unlift . recordCall
          , usage = unlift . recordUsage
          , allocations = Just $ unlift . recordAllocation
          , checks = Just $ unlift . recordCheck
          , proofs = Nothing
          , phase = unlift . recordPhase
          , validate = StructuredDraft $ \value -> case AE.fromJSON @FixResult value of
              AE.Error _ -> pure $ Left "Fix result does not match the required schema"
              AE.Success result -> (>>= matchingReview result) <$> unlift (check result)
          }
  AdmitRegression sessionScope request knowledge recordCall recordUsage recordAllocation -> localSeqUnliftIO env \unlift ->
    liftIO
      $ launch
        (AdmissionTask (mapSession unlift sessionScope) request)
        Callbacks
          { respond = const $ pure $ Left "Admission has no production query capability"
          , knowledge = unlift . knowledge
          , admit = const $ pure $ Left "Admission cannot recursively admit a test"
          , calls = unlift . recordCall
          , usage = unlift . recordUsage
          , allocations = Just $ unlift . recordAllocation
          , checks = Nothing
          , proofs = Nothing
          , phase = const $ pure $ Left "Admission cannot advance a fix phase"
          , validate = StructuredDraft $ \value -> pure $ case AE.fromJSON @AdmissionResult value of
              AE.Success result | result.artifact == request.artifact -> Right result.decision
              _ -> Left "Admission result must bind the exact test artifact and a nonempty rationale"
          }
  ReviewVerifiedFix sessionScope request verified knowledge recordCall recordUsage recordAllocation -> localSeqUnliftIO env \unlift ->
    if not (validReviewContext request verified)
      then pure $ Left "Peer review requires the same verified candidate and a distinct attempt"
      else
        liftIO
          $ launch
            (PeerTask (mapSession unlift sessionScope) request)
            Callbacks
              { respond = const $ pure $ Left "Peer review has no production query capability"
              , knowledge = unlift . knowledge
              , admit = const $ pure $ Left "Peer review cannot admit tests"
              , calls = unlift . recordCall
              , usage = unlift . recordUsage
              , allocations = Just $ unlift . recordAllocation
              , checks = Nothing
              , proofs = Nothing
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
    launch task callbacks = case (hostConfig, executable) of
      (Left failure, _) -> pure $ Left failure
      (_, Nothing) -> pure $ Left "Checkout-backed review agent is not configured"
      (Right host, Just command) -> do
        verifierSession <- UUID4.nextRandom
        proofCount <- newIORef (0 :: Int)
        let tagged :: AE.ToJSON value => Text -> value -> AE.Value
            tagged tag contents = AE.object ["tag" AE..= tag, "contents" AE..= contents]
            (scope, source, payload, role) = case task of
              PatchTask request -> (Outbox.Advisory request.runId, Checkout.Repository (Checkout.fetchOf (.head) request), tagged "PatchTask" request{gitToken = ""}, Policy.Patch)
              SourcePatchTask request -> (Outbox.Advisory request.runId, Checkout.Repository (Checkout.fetchOf (.head) request), tagged "PatchTask" request{gitToken = ""}, Policy.SourcePatch)
              SourceTask request -> (Outbox.Advisory request.runId, Checkout.Repository (Checkout.fetchOf (.head) request), AE.toJSON request{gitToken = ""}, Policy.Source)
              AdvisoryTask _ request -> (Outbox.Advisory request.runId, Checkout.Repository (Checkout.fetchOf (.head) request), AE.toJSON request{gitToken = ""}, Policy.Advisory)
              FixTask _ request -> (Outbox.IssueFix request.request.runId, Checkout.Repository (Checkout.fetchOf (.base) request.request), tagged "FixTask" request{request = request.request{gitToken = ""}}, Policy.Fix)
              AdmissionTask _ request -> (Outbox.Admission request.fix.request.runId, Checkout.Repository (Checkout.fetchOf (.base) request.fix.request), tagged "AdmitTask" (request{fix = request.fix{request = request.fix.request{gitToken = ""}}} :: AdmissionRequest), Policy.Admission)
              PeerTask _ request -> (Outbox.PeerReview request.fix.request.runId, Checkout.VerifiedCandidate request.artifacts, tagged "ReviewFixTask" (request{fix = request.fix{request = request.fix.request{gitToken = ""}}} :: FixReviewRequest), Policy.Peer)
            record = \case
              Outbox.Call receipt -> callbacks.calls receipt $> Right ()
              Outbox.Usage receipt -> callbacks.usage receipt $> Right ()
              Outbox.Allocation receipt -> maybe (pure $ Left "This task cannot allocate a sandbox") (\save -> save receipt $> Right ()) callbacks.allocations
              Outbox.Check receipt -> maybe (pure $ Left "This task cannot record regression executions") (\save -> save receipt $> Right ()) callbacks.checks
              Outbox.Phase receipt -> callbacks.phase receipt
            input = case role of
              (Policy.Fix; Policy.Admission; Policy.Peer) -> payload
              (Policy.Patch; Policy.SourcePatch; Policy.Source; Policy.Advisory) -> case payload of AE.Object fields -> AE.Object $ KM.delete "attemptId" fields; value -> value
            asPause :: SomeException -> Maybe NativePause
            asPause failure = (DecisionWait <$> fromException failure) <|> fromException failure
            throwPause :: NativePause -> IO value
            throwPause = \case DecisionWait waiting -> throwIO waiting; ProviderWait -> throwIO ProviderWait
            withJournal action = maybe (action Nothing) (\configured -> Outbox.withOutbox configured scope $ action . Just) artifactStore
            runChild :: Maybe Outbox.Outbox -> Maybe RetainedSession -> IO (Either Text result)
            runChild journal retention = do
              let sessionDirectory = (.directory) <$> retention
                  resumed = retention >>= (.saved)
                  paused = (\case AnalysisSuspended saved -> saved; FixSuspended saved _ -> saved) <$> resumed
                  savedFix = case resumed of Just (FixSuspended _ saved) -> Just saved; (Nothing; Just AnalysisSuspended{}) -> Nothing
                  persist progress = for_ retention \retained -> do
                    retained.persist progress
                    pruneNativeSnapshots retained.directory $ case progress of Waiting saved -> Just saved.id; FixWaiting saved _ -> Just saved.id; Finished{} -> Nothing
              -- A resumed session keeps its proof attempts, so the budget is checked before VMs run.
              writeIORef proofCount $ fromMaybe 0 $ (.proofs) =<< paused
              for_ sessionDirectory \directory -> case paused of
                Nothing -> do
                  removed <- tryIOError $ removeFile $ directory </> "active.jsonl"
                  case removed of Left failed | not (isDoesNotExistError failed) -> throwIO failed; _ -> pass
                Just saved -> do
                  transcript <- Native.readPrivateArtifact (directory </> toString saved.id.toText) "session.jsonl" (64 * 1048576)
                  unless (Native.sha256 transcript == saved.sha256) $ fail "Native continuation transcript has changed"
                  Native.writePrivateArtifact directory "active.jsonl" $ fromStrict transcript
              environment <- getEnvironment
              let selected = maybe [] (\outbox -> [("IMPACT_REVIEW_OUTBOX_DIR", Outbox.outboxPath outbox)]) journal <> maybe [] (\directory -> [("IMPACT_REVIEW_SESSION_FILE", directory </> "active.jsonl")]) sessionDirectory
                  childEnvironment = selected <> filter (\(name, _) -> name `notElem` ["IMPACT_REVIEW_OUTBOX_DIR", "IMPACT_REVIEW_SESSION_FILE"]) environment
              withProcessTerm (setStdin createPipe $ setStdout createPipe $ command childEnvironment) \process -> do
                guest <- newIORef Nothing
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
                    suspend :: Int -> Int -> Maybe AE.Value -> Maybe GuestSession -> IO ()
                    suspend remaining submissions accepted workflowState = for_ sessionDirectory \directory -> case accepted of
                      Just draft -> persist $ Finished draft
                      Nothing -> do
                        transcript <- Native.readPrivateArtifact directory "active.jsonl" (64 * 1048576)
                        identifier <- UUIDId <$> UUID4.nextRandom
                        generation <- Outbox.privateDirectory directory [toString identifier.toText]
                        Native.writePrivateArtifact generation "session.jsonl" $ fromStrict transcript
                        session <- readIORef guest >>= maybe (fail "Native continuation requires a live guest") pure
                        disk <- session.suspend $ generation </> "guest.qcow2"
                        attempted <- readIORef proofCount
                        let saved = SavedGuest identifier disk (Native.sha256 transcript) remaining submissions (Just attempted)
                        persist $ case workflowState of
                          Just (FixReady current) -> FixWaiting saved $ SavedFix current.id $ Native.candidateSnapshot <$> current.progress
                          (Nothing; Just AdmissionReady; Just SourceReady) -> Waiting saved
                    pause :: Int -> Int -> Maybe GuestSession -> IO value -> IO value
                    pause remaining submissions workflowState action =
                      action `catch` \(failure :: SomeException) -> case asPause failure of
                        Nothing -> throwIO failure
                        Just waiting -> suspend remaining submissions Nothing workflowState >> throwPause waiting
                    active :: (Guest.Session -> IO (Either Text value)) -> IO (Either Text value)
                    active action = readIORef guest >>= maybe (pure $ Left "Guest operations require a host-owned sandbox") action
                    execute privilege argv = active $ \session -> do
                      outcome <- tryAny $ case privilege of
                        Supervisor -> session.root argv $> ""
                        Agent ->
                          session.output (Workspace.agentCommand $ toList argv) 1048576 >>= \case
                            (Workspace.Exited 0, bytes, False) -> pure $ decodeUtf8 bytes
                            ((Workspace.Exited{}, _, _); (Workspace.LogLimit, _, _)) -> fail "Guest workspace command failed or exceeded its output limit"
                      pure $ first (const "Guest workspace command failed") outcome
                    copy hostPath destination = active $ \session -> first (const "Guest copy failed") <$> tryAny (session.copyIn hostPath destination)
                    readGuest path = active $ \session ->
                      session.output (Workspace.agentCommand ["/usr/bin/head", "-c", "262145", "--", path]) 262145 <&> \case
                        (Workspace.Exited 0, bytes, False) -> Right $ decodeUtf8 bytes
                        (Workspace.Exited 0, _, True) -> Left "Regression patch file exceeds its 256 KiB limit"
                        ((Workspace.Exited{}, _, _); (Workspace.LogLimit, _, _)) -> Left "Regression patch file could not be read"
                    protectWorkspace = for_ (["/usr/bin/git" :| ["config", "--system", "--add", "safe.directory", path] | path <- ["/workspace", "/workspace/.git"]] <> ["/bin/chown" :| ["-hR", "0:0", "/workspace"], "/bin/chmod" :| ["-R", "u+rwX,go+rX,go-w", "/workspace"], "/usr/bin/git" :| ["update-index", "--refresh"]] :: [NonEmpty Text]) $ void . ExceptT . execute Supervisor
                    regression :: Request run -> Workspace.Baseline -> UUID.UUID -> AE.Value -> [(Text, Text)] -> Outbox.Outbox -> IO (Either Text (RegressionWire, [(Workspace.Side, FilePath, Text)]))
                    regression pinned baseline sessionId artifact revisions outbox = active $ \session -> do
                      let options = session.verifier (Outbox.checksPath outbox)
                      case Workspace.mkRegressionRequest pinned.projectId pinned.runId pinned.attemptId sessionId pinned.base session.imageBuildId baseline artifact revisions of
                        Left _ -> pure $ Left "Regression arguments do not match the host contract"
                        Right request -> do
                          checked <- tryAny $ Workspace.verifyRegression options request
                          case checked of
                            Left failure -> pure $ Left $ maybe "Local check could not run" Workspace.regressionErrorText $ fromException failure
                            Right result -> do
                              let logs = Workspace.regressionLogs options result
                              for_ logs \(_, hostPath, guestPath) -> session.copyIn hostPath guestPath
                              pure $ bimap toText (,logs) $ AET.parseEither AE.parseJSON $ AE.toJSON result
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
                          spend count = requireBudget count >> lift (State.modify' \current -> FlowState current.progress (current.remaining - count))
                          progress :: Flow (FixProgress Native.CapturedCandidate)
                          progress = (.progress) <$> lift State.get
                          advance value = lift $ State.modify' \current -> (current{progress = value} :: FlowState)
                          phase step = do
                            spend 1
                            liftIO (Outbox.deliverPhase outbox request.request.attemptId step record) >>= either fatal pure
                          root arguments = void $ io $ execute Supervisor arguments
                          checks = Outbox.checksPath outbox
                          assertWorkspace captured = do
                            patch <- io $ capturePatch request.request.base
                            unless (Native.candidateMatchesPatch captured (Native.candidateIdentity captured) patch) $ reject "Workspace changed; propose and verify a new candidate"
                          comparison artifact revisions = do
                            requireBudget $ length revisions
                            (result, _) <- io $ regression request.request session.baseline session.id.unUUIDId (AE.toJSON artifact) [(case side of { Base -> "base"; Head -> "head" }, revisionText revision) | (side, revision) <- toList revisions] outbox
                            for_ result.receipts \receipt -> do
                              liftIO (Outbox.deliver outbox (Outbox.Check receipt) record) >>= either fatal pure
                              phase $ Observe receipt
                            when result.isError $ reject $ "Native regression did not produce usable evidence.\n" <> decodeUtf8 (AE.encode result.report)
                            pure result
                          describe message result = AE.object ["message" AE..= (message :: Text), "report" AE..= result.report]
                          fingerprint = \case Just (InjectedTest value) -> Just value; Just (RetainedTest value) -> Just value; (Nothing; Just ScriptCheck{}) -> Nothing
                          invoke invocation = do
                            let parsed :: (AE.Value -> AET.Parser value) -> Flow value
                                parsed parser = either (reject . toText) pure $ AET.parseEither parser invocation.arguments
                            case invocation.name of
                              Policy.ReproduceIssue -> do
                                progress >>= \case NeedsReproduction -> pass; (Reproduced{}; Captured{}; Checked{}) -> reject "A regression is already frozen for this run"
                                ReproductionInput script failureMessage patch <- parsed AE.parseJSON
                                testPatch <- case patch of
                                  InlinePatch text -> pure text
                                  FileContents files -> io $ first Workspace.testPatchErrorText <$> Workspace.renderTestPatch checkout.directory (revisionText checkout.base) files
                                  PatchFile path -> do
                                    text <- io $ readGuest path
                                    when (BS.length (encodeUtf8 text) > 262144) $ reject "Regression patch file exceeds its 256 KiB limit"
                                    pure text
                                when (T.null testPatch || T.length testPatch > 262144) $ reject "Regression patch must be nonempty and within its limit"
                                let artifact = TestArtifact script testPatch failureMessage
                                requireBudget 1
                                admitted <- liftIO $ tryAny $ callbacks.admit artifact
                                rationale <- case admitted of
                                  Left failure -> maybe (liftIO $ throwIO failure) (throwE . Paused) $ asPause failure
                                  Right response -> spend 1 >> either reject pure response
                                when (T.null $ T.strip rationale) $ reject "Host admission requires a recorded rationale"
                                liftIO $ Native.retainAdmission checks request artifact rationale
                                result <- comparison artifact $ (Base, checkout.base) :| []
                                receipt <- case result.receipts of
                                  [receipt] | receipt.side == Base && receipt.status == Failed && isJust (fingerprint receipt.regression) -> pure receipt
                                  _ -> reject "The admitted regression did not fail on base; source edits remain locked"
                                io $ copy (checks </> toString receipt.checkId.toText </> "test.patch") "/run/monoscope-admitted-test.patch"
                                case receipt.regression of
                                  Just RetainedTest{} -> pass
                                  (Nothing; Just ScriptCheck{}; Just InjectedTest{}) -> root $ "/usr/bin/git" :| ["-c", "core.hooksPath=/dev/null", "apply", "--recount", "--index", "/run/monoscope-admitted-test.patch"]
                                root $ "/bin/chown" :| ["-hR", "1000:1000", "/workspace"]
                                advance $ Reproduced $ Reproduction artifact receipt
                                pure $ describe "Regression reproduced and frozen. Source editing is now available." result
                              Policy.ProposeFix -> do
                                artifact <-
                                  progress >>= \case
                                    NeedsReproduction -> reject "Reproduce the issue before proposing a fix"
                                    Reproduced artifact -> pure artifact
                                    Captured artifact _ -> pure artifact
                                    Checked artifact captured _ _ -> advance (Captured artifact captured) $> artifact
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
                                    Checked artifact captured _ _ -> pure (artifact, captured)
                                    (NeedsReproduction; Reproduced{}) -> reject "Capture a candidate after reproduction first"
                                advance $ Captured artifact captured
                                assertWorkspace captured
                                result <- comparison artifact.artifact $ (Base, checkout.base) :| [(Head, Native.candidateRevision captured)]
                                (baseline, receipt) <- case result.receipts of
                                  [baseline, candidate]
                                    | baseline.side == Base && baseline.status == Failed && candidate.side == Head && candidate.status == Passed
                                    , Just RetainedTest{} <- candidate.regression
                                    , baseline.checkId == candidate.checkId && fingerprint baseline.regression == fingerprint candidate.regression ->
                                        pure (baseline, candidate)
                                  _ -> reject $ "Candidate verification did not establish a failing base and passing retained test.\n" <> decodeUtf8 (AE.encode result.report)
                                advance $ Checked artifact captured baseline receipt
                                pure $ describe "Frozen regression fails on base and passes on the candidate with its test retained." result
                              (Policy.RunRegression; Policy.RecordProof; Policy.PlanProofs; Policy.QueryTelemetry; Policy.SubmitFix; Policy.SubmitAdmission; Policy.SubmitFixReview; Policy.RecallKnowledge; Policy.RememberKnowledge; Policy.ListProjectMemory; Policy.ReadProjectMemory; Policy.WriteProjectMemory; Policy.SubmitReview) -> reject "This tool is not a fix workflow operation"
                          finish summary = do
                            (captured, receipt) <-
                              progress >>= \case
                                Checked _ captured _ receipt -> pure (captured, receipt)
                                (NeedsReproduction; Reproduced{}; Captured{}) -> reject "Capture and verify a source fix before submission"
                            assertWorkspace captured
                            either reject pure $ Native.candidateResult captured receipt summary
                       in FixActions invoke finish
                    runComparison request outbox identifier invocation = active $ \session -> do
                      verified <- regression request session.baseline identifier invocation.arguments [("base", request.base), ("head", request.head)] outbox
                      case verified of
                        Left failure -> pure $ Left failure
                        Right (result, _) -> do
                          saved <- runExceptT $ for_ result.receipts $ \receipt -> ExceptT $ Outbox.deliver outbox (Outbox.Check receipt) record
                          pure $ saved >> if result.isError then Left (decodeUtf8 $ AE.encode result.report) else Right result.report

                    -- The model's command is never trusted to report its own result: the host
                    -- replays it on both revisions and keeps only what asciinema recorded.
                    recordProof request outbox identifier invocation hooks = case AET.parseEither (AE.parseJSON @ProofArgs) invocation.arguments of
                      Left _ -> pure $ Left "Tool arguments do not match the host contract"
                      Right proof -> do
                        taken <- atomicModifyIORef' proofCount \n -> (n + 1, n)
                        if taken >= proofBudget
                          then pure $ Left "Proof budget exhausted; cite the proofs already recorded"
                          else do
                            hooks.progress $ Started proof.claim
                            recorded <- recordSides request outbox identifier proof hooks
                            recorded <$ whenLeft_ recorded (const $ hooks.progress $ Unproved proof.claim)
                    recordSides request outbox identifier proof hooks =
                      active \session -> runExceptT do
                        (result, logs) <- ExceptT $ regression request session.baseline identifier (AE.object ["script" AE..= Media.proofScript proof.setup proof.shellCommand, "failureMessage" AE..= Media.proofFailure]) [("base", request.base), ("head", request.head)] outbox
                        for_ result.receipts \receipt -> ExceptT $ Outbox.deliver outbox (Outbox.Check receipt) record
                        -- Both sides render at once; each is an agg and ffmpeg pass under the review lease.
                        recorded <-
                          ExceptT $ sequence <$> forConcurrently result.receipts \receipt -> runExceptT do
                            let side = case receipt.side of Base -> Workspace.Base; Head -> Workspace.Head
                            logFile <- maybe (throwE "The proof log was not retained") pure $ listToMaybe [path | (logged, path, _) <- logs, logged == side]
                            logBytes <- readFileBS logFile
                            (cast, output) <- except $ Media.proofCast logBytes
                            let browser = Media.browserMedia logBytes
                            (gif, mp4) <- ExceptT $ maybe (Media.renderProof cast) (Media.renderVideo browser.page) browser.video
                            let summary = AE.object ["side" AE..= receipt.side, "revision" AE..= receipt.revision, "status" AE..= receipt.status, "exitCode" AE..= receipt.exitCode, "output" AE..= T.takeEnd 3000 output, "screenshots" AE..= map fst browser.shots, "video" AE..= isJust browser.video]
                            pure (ProofMedia{side = receipt.side, revision = receipt.revision, exitCode = receipt.exitCode, cast, gif, mp4, browser = isJust browser.video, shots = browser.shots, chapters = browser.chapters}, summary)
                        sides <- maybe (throwE "The proof recorded no revision") pure $ nonEmpty $ map fst recorded
                        key <- ExceptT $ hooks.save proof.setup sides
                        lift $ hooks.progress $ Recorded proof.claim key
                        pure $ AE.object ["key" AE..= key, "claim" AE..= proof.claim, "sides" AE..= map snd recorded, "note" AE..= ("Cite this key in a claim. The PR author sees both recordings." :: Text)]

                    receive :: Maybe Checkout.Prepared -> Maybe GuestSession -> Int -> Int -> Maybe (Accepted result) -> IO (Either Text result)
                    receive supplied guestSession remaining submissions accepted = do
                      message <- (AE.eitherDecodeStrict >=> AET.parseEither parseMessage) <$> BS.hGetLine (getStdout process)
                      case message of
                        Left _ -> pure $ Left "Review agent returned an invalid protocol message"
                        Right (Left receipt) -> do
                          let phase = case receipt of Outbox.Phase _ -> True; (Outbox.Call{}; Outbox.Usage{}; Outbox.Allocation{}; Outbox.Check{}) -> False
                              invalidates = case receipt of Outbox.Check _ -> True; Outbox.Phase _ -> True; (Outbox.Call{}; Outbox.Usage{}; Outbox.Allocation{}) -> False
                          if phase && remaining <= 0
                            then pure $ Left "Review agent exceeded its query budget"
                            else do
                              prepared <- case (task, receipt, guestSession) of
                                (SourceTask{}, Outbox.Call{}, Nothing) -> fmap (const $ Just SourceReady) <$> runExceptT protectWorkspace
                                (SourceTask{}, Outbox.Call{}, Just _) -> pure $ Right guestSession
                                ((SourceTask{}, Outbox.Usage{}, _); (SourceTask{}, Outbox.Allocation{}, _); (SourceTask{}, Outbox.Check{}, _); (SourceTask{}, Outbox.Phase{}, _)) -> pure $ Right guestSession
                                ((PatchTask{}, _, _); (SourcePatchTask{}, _, _)) -> pure $ Right guestSession
                                ((AdvisoryTask{}, _, _); (FixTask{}, _, _); (AdmissionTask{}, _, _); (PeerTask{}, _, _)) -> pure $ Right guestSession
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
                            prepared <- Checkout.withCheckout configured source \fetched evidence -> do
                              let checkout = fetched{Checkout.policy = Policy.policy role (revisionText fetched.base) (revisionText fetched.head) fetched.rootInstructions}
                                  deliver ready readySession = (writeMessage (Right ready :: Either Text Checkout.Prepared) >> receive (Just ready) readySession remaining submissions accepted) `finally` stop
                                  own :: Request run -> Maybe (UUIDId "guest_image") -> Guest.Setup -> Maybe Checkout.Envelope -> Guest.Host -> IO (Either Text result)
                                  own request expected setup envelope worker = case Workspace.mkRevision (revisionText checkout.head) of
                                    Left failure -> pure $ Left failure
                                    Right revision -> do
                                      lease <- either (fail . toString) (pure . Guest.Lease configured.cacheDir) $ Workspace.mkRequest request.projectId request.repository request.base expected
                                      let allocate receipt = do
                                            saved <- maybe (record $ Outbox.Allocation receipt) (\outbox -> Outbox.deliver outbox (Outbox.Allocation receipt) record) journal
                                            either (fail . toString) pure saved
                                          retain receipt = for_ journal $ \outbox -> Outbox.retain outbox $ Outbox.Allocation receipt
                                      join <$> Guest.withGuest worker expected checkout.directory revision (maybe (Guest.Fresh setup) (Guest.ResumeSession . (.guest)) paused) (Just lease) request.attemptId allocate retain \session -> do
                                        writeIORef guest $ Just session
                                        initialized <- case task of
                                          FixTask _ job -> case (journal, savedFix) of
                                            (Nothing, _) -> pure $ Left "Fix execution requires a durable outbox"
                                            (Just _, Just saved) -> runExceptT do
                                              progress <- traverse (ExceptT . Native.restoreCandidate configured job checkout.directory) saved.progress
                                              let reproduction = case progress of NeedsReproduction -> Nothing; Reproduced value -> Just value; Captured value _ -> Just value; Checked value _ _ _ -> Just value
                                              for_ reproduction \value -> ExceptT $ Native.verifyReproduction configured job value.artifact value.baseline
                                              case progress of
                                                Checked _ captured baseline candidate -> ExceptT $ Native.verifyComparison configured job.request.repository captured baseline candidate
                                                (NeedsReproduction; Reproduced{}; Captured{}) -> pass
                                              pure $ Just $ FixReady $ FixSession saved.id session.baseline progress
                                            (Just outbox, Nothing) -> do
                                              saved <- Outbox.deliverPhase outbox job.request.attemptId RestartReproduction record
                                              identifier <- UUIDId <$> UUID4.nextRandom
                                              pure $ saved $> Just (FixReady $ FixSession identifier session.baseline NeedsReproduction)
                                          SourceTask{} -> pure $ Right $ Just SourceReady
                                          AdmissionTask{} -> pure $ Right $ Just AdmissionReady
                                          (PatchTask{}; SourcePatchTask{}) -> pure $ Right Nothing
                                          (AdvisoryTask{}; PeerTask{}) -> pure $ Right Nothing
                                        either (pure . Left) (deliver checkout{Checkout.cache = Just session.cache, Checkout.guest = Just session.skills, Checkout.envelope = envelope}) initialized
                              case host of
                                Nothing -> deliver checkout guestSession
                                Just worker -> case task of
                                  PatchTask{} -> deliver checkout guestSession
                                  SourcePatchTask{} -> deliver checkout guestSession
                                  SourceTask request -> own request Nothing Guest.LockForReproduction Nothing worker
                                  AdvisoryTask _ request -> own request Nothing Guest.Explore Nothing worker
                                  FixTask _ job -> own job.request (Just job.imageBuildId) Guest.LockForReproduction Nothing worker
                                  AdmissionTask _ job -> either (pure . Left) (\setup -> own job.fix.request (Just job.fix.imageBuildId) setup (Just $ Checkout.Artifact job.artifact) worker) $ Guest.admissionSetup job.artifact
                                  PeerTask _ job -> own job.fix.request (Just job.fix.imageBuildId) (Guest.ReviewEvidence evidence) (Just $ Checkout.Result job.artifacts.result) worker
                            pure $ join prepared
                          _ -> pure $ Left "Repository checkout was unavailable or already supplied"
                        Right (Right (Tool invocation))
                          | isNothing supplied -> pure $ Left "Tool invocation requires its repository checkout"
                          | invocation.name == Policy.RunRegression && validTool invocation -> do
                              response <- case (task, journal) of
                                (AdvisoryTask _ request, Just outbox) -> runComparison request outbox verifierSession invocation
                                (SourceTask request, Just outbox) -> runComparison request outbox verifierSession invocation
                                ((AdvisoryTask{}, Nothing); (SourceTask{}, Nothing)) -> pure $ Left "Regression requires an authorized source review"
                                ((PatchTask{}, _); (SourcePatchTask{}, _)) -> pure $ Left "Regression requires an authorized source review"
                                ((FixTask{}, _); (AdmissionTask{}, _); (PeerTask{}, _)) -> pure $ Left "Regression requires an authorized source review"
                              writeMessage response
                              receive supplied guestSession remaining submissions Nothing
                          | invocation.name == Policy.PlanProofs && validTool invocation && remaining > 0 -> do
                              response <- case (task, callbacks.proofs, AET.parseEither (AE.withObject "Plan" (AE..: "claims")) invocation.arguments) of
                                (AdvisoryTask{}, Just hooks, Right claims) -> Right (AE.object ["planned" AE..= length claims]) <$ hooks.progress (Planned claims)
                                (_, _, Left _) -> pure $ Left "Tool arguments do not match the host contract"
                                ((AdvisoryTask{}, Nothing, Right _); (SourceTask{}, _, Right _); (FixTask{}, _, Right _); (AdmissionTask{}, _, Right _); (PeerTask{}, _, Right _)) -> pure $ Left "Proof requires an authorized pull request review"
                                ((PatchTask{}, _, Right _); (SourcePatchTask{}, _, Right _)) -> pure $ Left "Proof requires an authorized pull request review"
                              -- Each plan edits the PR comment, so it spends the query budget like any other call.
                              writeMessage response
                              receive supplied guestSession (remaining - 1) submissions Nothing
                          | invocation.name == Policy.RecordProof && validTool invocation && remaining > 0 -> do
                              -- Each attempt also spends the persisted query budget, which a resumed session keeps.
                              response <- case (task, journal, callbacks.proofs) of
                                (AdvisoryTask _ request, Just outbox, Just hooks) -> recordProof request outbox verifierSession invocation hooks
                                ((AdvisoryTask{}, _, _); (SourceTask{}, _, _); (FixTask{}, _, _); (AdmissionTask{}, _, _); (PeerTask{}, _, _)) -> pure $ Left "Proof requires an authorized pull request review"
                                ((PatchTask{}, _, _); (SourcePatchTask{}, _, _)) -> pure $ Left "Proof requires an authorized pull request review"
                              writeMessage response
                              receive supplied guestSession (remaining - 1) submissions Nothing
                          | invocation.name `elem` [Policy.ReproduceIssue, Policy.ProposeFix, Policy.VerifyFix] && validTool invocation -> case (task, supplied, journal, guestSession) of
                              (FixTask _ request, Just checkout, Just outbox, Just (FixReady session)) -> do
                                (outcome, current) <- State.runStateT (runExceptT $ (workflow request checkout outbox session).invoke invocation) (FlowState session.progress remaining)
                                let workflowState = Just $ FixReady session{progress = current.progress}
                                    continue response = writeMessage response >> receive supplied workflowState current.remaining submissions Nothing
                                case outcome of
                                  Left (Paused waiting) -> suspend current.remaining submissions Nothing workflowState >> throwPause waiting
                                  Left (Fatal failure) -> writeMessage (Left failure :: Either Text AE.Value) $> Left failure
                                  Left (ToolRejected failure) -> continue (Left failure)
                                  Right response -> continue (Right response)
                              ((FixTask{}, _, _, Nothing); (FixTask{}, _, _, Just AdmissionReady); (FixTask{}, _, _, Just SourceReady)) -> pure $ Left "Fix tools require an initialized issue-fix session"
                              (FixTask{}, Nothing, _, Just FixReady{}) -> pure $ Left "Fix tools require an initialized issue-fix session"
                              (FixTask{}, Just _, Nothing, Just FixReady{}) -> pure $ Left "Fix tools require an initialized issue-fix session"
                              ((PatchTask{}, _, _, _); (SourcePatchTask{}, _, _, _)) -> pure $ Left "Fix tools require an initialized issue-fix session"
                              ((SourceTask{}, _, _, _); (AdvisoryTask{}, _, _, _); (AdmissionTask{}, _, _, _); (PeerTask{}, _, _, _)) -> pure $ Left "Fix tools require an initialized issue-fix session"
                          | remaining > 0 -> do
                              let parsed :: (AE.Value -> AET.Parser value) -> (value -> IO (Either Text AE.Value)) -> IO (Either Text AE.Value)
                                  parsed parser = whenRight (Left "Tool arguments do not match the host contract") (AET.parseEither parser invocation.arguments)
                                  property :: AE.FromJSON value => AE.Key -> (value -> IO (Either Text AE.Value)) -> IO (Either Text AE.Value)
                                  property key = parsed $ AE.withObject "Tool arguments" (AE..: key)
                                  request = case invocation.name of
                                    Policy.QueryTelemetry -> parsed AE.parseJSON callbacks.respond
                                    Policy.RecallKnowledge -> property "service" $ callbacks.knowledge . Recall
                                    Policy.RememberKnowledge -> parsed AE.parseJSON $ callbacks.knowledge . Remember
                                    Policy.ListProjectMemory -> parsed Memory.parsePageQuery $ callbacks.knowledge . ProjectMemory . maybe Memory.List Memory.Search
                                    Policy.ReadProjectMemory -> property "pageId" $ callbacks.knowledge . ProjectMemory . Memory.Read
                                    Policy.WriteProjectMemory -> parsed AE.parseJSON $ callbacks.knowledge . ProjectMemory . Memory.Write
                                    (Policy.RunRegression; Policy.RecordProof; Policy.PlanProofs; Policy.SubmitFix; Policy.ReproduceIssue; Policy.ProposeFix; Policy.VerifyFix; Policy.SubmitAdmission; Policy.SubmitFixReview; Policy.SubmitReview) -> pure $ Left "This tool requires a native workflow handler"
                              response <- pause remaining submissions guestSession $ if validTool invocation then request else pure $ Left "Tool is unavailable or its arguments violate the host contract"
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
                                (FixTask _ request, Right rationale)
                                  | not (T.null $ T.strip rationale) -> for_ journal (\outbox -> Native.retainAdmission (Outbox.checksPath outbox) request artifact rationale) $> admitted
                                  | otherwise -> pure $ Left "Host admission requires a recorded rationale"
                                (FixTask{}, Left _) -> pure admitted
                                ((PatchTask{}, _); (SourcePatchTask{}, _)) -> pure admitted
                                ((SourceTask{}, _); (AdvisoryTask{}, _); (AdmissionTask{}, _); (PeerTask{}, _)) -> pure admitted
                              writeMessage saved
                              receive supplied guestSession (remaining - 1) submissions Nothing
                          | otherwise -> pure $ Left "Review agent exceeded its query budget"
                        Right (Right (Guest operation)) -> do
                          response <- active $ \session -> session.operate operation
                          writeMessage response
                          receive supplied guestSession remaining submissions Nothing
                        Right (Right (Validate draft))
                          | FixTask _ request <- task
                          , Just checkout <- supplied
                          , Just outbox <- journal
                          , Just (FixReady session) <- guestSession
                          , submissions > 0 ->
                              case AET.parseEither (AE.withObject "Fix summary" (AE..: "summary")) draft of
                                Left _ -> writeMessage (Left "Fix summary does not match the host contract" :: Either Text AE.Value) >> receive supplied guestSession remaining (submissions - 1) Nothing
                                Right summary -> do
                                  (outcome, current) <- State.runStateT (runExceptT $ (workflow request checkout outbox session).finish summary) (FlowState session.progress remaining)
                                  let workflowState = Just $ FixReady session{progress = current.progress}
                                  checked <- case outcome of
                                    Left (ToolRejected failure) -> pure $ Left failure
                                    Left (Fatal failure) -> pure $ Left failure
                                    Left (Paused waiting) -> suspend current.remaining submissions Nothing workflowState >> throwPause waiting
                                    Right result -> case callbacks.validate of
                                      StructuredDraft validate -> fmap (Accepted draft $ AE.toJSON result) <$> pause current.remaining submissions workflowState (validate $ AE.toJSON result)
                                  writeMessage $ void checked
                                  receive supplied workflowState current.remaining (submissions - 1) $ rightToMaybe checked
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
                              receive supplied guestSession remaining (submissions - 1) $ either (const Nothing) (Just . Accepted draft draft) checked
                        Right (Right (ValidateText raw)) -> case (callbacks.validate, supplied) of
                          (DraftText validate, Just _) | submissions > 0 -> do
                            checked <- validate raw
                            writeMessage $ void checked
                            receive supplied guestSession remaining (submissions - 1) $ either (const Nothing) (Just . Accepted (AE.String raw) (AE.String raw)) checked
                          (DraftText{}, _) -> pure $ Left "Text submission requires a prepared patch comparison and remaining budget"
                          (StructuredDraft{}, _) -> pure $ Left "Text submission requires a prepared patch comparison and remaining budget"
                        Right (Right (Suspend ())) -> case (sessionDirectory, accepted) of
                          (Nothing, Nothing) -> writeMessage (Left "Session retention is not configured" :: Either Text ()) $> Left "Session retention is not configured"
                          _ -> do
                            suspend remaining submissions ((.validated) <$> accepted) guestSession
                            writeMessage (Right () :: Either Text ())
                            case accepted of
                              Just checked -> pure $ Right checked.result
                              Nothing -> case role of
                                (Policy.Admission; Policy.Peer) -> throwIO ProviderWait
                                (Policy.Patch; Policy.SourcePatch; Policy.Source; Policy.Advisory; Policy.Fix) -> pure $ Left "Review session interrupted; retry its retained request"
                        Right (Right (Result draft)) -> case accepted of
                          Just checked | checked.submission == draft -> do
                            status <- waitExitCode process
                            if status == ExitSuccess then persist (Finished checked.validated) $> Right checked.result else pure $ Left "Review agent did not produce a validated result"
                          _ -> pure $ Left "Review agent returned an unvalidated draft"
                ( do
                    writeMessage payload
                    receive Nothing Nothing (maybe queryLimit (.remaining) paused) (maybe submissionLimit (.submissions) paused) Nothing
                  )
                  `finally` stop
        -- Replay persisted work before starting a child, fetching source or calling a model.
        -- A review's agent stops a minute before its job; fixes keep the main queue's short budget.
        let budget = case task of AdvisoryTask{} -> reviewSeconds - 60; _ -> 480
        outcome <- tryAny $ Timeout.timeout (budget * 1000000) $ withJournal \journal -> do
          recovered <- maybe (pure $ Right ()) (`Outbox.replay` record) journal
          case recovered of
            Left message -> pure $ Left message
            Right () -> do
              let retain :: forall context. (AE.FromJSON context, AE.ToJSON context) => SessionIdentity IO context -> [FilePath] -> DraftValidator 'JsonDraft result -> IO (Either Text result)
                  retain Isolated _ _ = runChild journal Nothing
                  retain (Retained root capture) key (StructuredDraft validate) = case journal of
                    Nothing -> fail "Retained sessions require a durable outbox"
                    Just outbox -> do
                      directory <- Outbox.privateDirectory (Outbox.outboxPath outbox) $ ["sessions", toString root.toText] <> key
                      stored <- readNativeCheckpoint @context directory
                      for_ stored \checkpoint -> unless (checkpoint.input == input) $ fail "Native continuation does not match the original request"
                      let persist progress = capture >>= Native.writePrivateArtifact directory "checkpoint.json" . AE.encode . NativeCheckpoint input progress
                          begin saved = runChild journal $ Just $ RetainedSession directory persist saved
                      case stored of
                        Nothing -> begin Nothing
                        Just NativeCheckpoint{progress = Waiting saved} -> case role of
                          Policy.Fix -> fail "Fix continuation has no verified workflow state"
                          (Policy.Patch; Policy.SourcePatch; Policy.Source; Policy.Advisory; Policy.Admission; Policy.Peer) -> begin $ Just $ AnalysisSuspended saved
                        Just NativeCheckpoint{progress = FixWaiting saved workflow} -> case role of
                          Policy.Fix -> begin $ Just $ FixSuspended saved workflow
                          (Policy.Patch; Policy.SourcePatch; Policy.Source; Policy.Advisory; Policy.Admission; Policy.Peer) -> fail "Analysis continuation contains a fix workflow"
                        Just NativeCheckpoint{progress = Finished draft} -> validate draft <* pruneNativeSnapshots directory Nothing
              case task of
                AdvisoryTask selected _ -> retain selected [] callbacks.validate
                FixTask selected _ -> retain selected [] callbacks.validate
                AdmissionTask selected request -> retain selected [nativeKey request.artifact] callbacks.validate
                PeerTask selected request -> retain selected [nativeKey request.artifacts] callbacks.validate
                PatchTask{} -> runChild journal Nothing
                SourcePatchTask{} -> runChild journal Nothing
                SourceTask{} -> runChild journal Nothing

        case outcome of
          Left err
            | Just waiting <- fromException @AgentAwaitingReview err -> throwIO waiting
            | Just ProviderWait <- fromException @NativePause err -> case role of
                (Policy.Admission; Policy.Peer) -> throwIO ProviderWait
                (Policy.Patch; Policy.SourcePatch; Policy.Source; Policy.Advisory; Policy.Fix) -> pure $ Left "Native child session interrupted; retry its retained request"
            | otherwise -> pure $ Left "Review agent could not be executed"
          Right Nothing -> pure $ Left "Review agent exceeded its eight-minute budget"
          Right (Just result) -> pure result


-- The durable manifest owns its live generation; cleanup cannot undo a saved result.
pruneNativeSnapshots :: FilePath -> Maybe (UUIDId "native_checkpoint") -> IO ()
pruneNativeSnapshots directory retained = void $ tryIOError do
  files <- listDirectory directory
  for_ files \filename -> for_ (UUID.fromString filename) \identifier ->
    unless (Just (UUIDId identifier) == retained) $ removePathForcibly $ directory </> filename


readNativeCheckpoint :: AE.FromJSON context => FilePath -> IO (Maybe (NativeCheckpoint context))
readNativeCheckpoint directory = do
  stored <- tryIOError $ Native.readPrivateArtifact directory "checkpoint.json" (16 * 1048576)
  case stored of
    Left failed | isDoesNotExistError failed -> do
      unlessM (null <$> listDirectory directory) $ fail "Native continuation manifest is missing"
      pure Nothing
    Left failed -> throwIO failed
    Right bytes -> either (const $ fail "Invalid native continuation") (pure . Just) $ AE.eitherDecodeStrict bytes


runCodeReviewTest :: IOE :> es => (Request "pr_review" -> Either Text Text) -> (FixRequest -> Either Text FixResult) -> (AdmissionRequest -> Either Text AdmissionDecision) -> (ArtifactRequest -> Either Text ()) -> (ArtifactRequest -> Either Text Explanation) -> (FixReviewRequest -> Either Text FixReviewDecision) -> Eff (CodeReview ': es) a -> Eff es a
runCodeReviewTest respond respondFix respondAdmission respondArtifacts respondExplanation respondReview = interpret \env -> \case
  ReadSessionContext _ _ -> pure $ Right Nothing
  ReviewRepository _ request _ _ _ _ _ _ _ check -> localSeqUnliftIO env $ \unlift -> checked (respond request) (first (const "Review agent returned invalid review JSON") . AE.eitherDecodeStrict . encodeUtf8) (unlift . check)
  ReviewSource mode request _ _ _ _ check -> localSeqUnliftIO env $ \unlift ->
    checked (respond request) (case mode of BoundedPatch -> Right . ProviderText; CheckoutHarness -> fmap Structured . first (const "Source review returned invalid JSON") . AE.eitherDecodeStrict . encodeUtf8) (unlift . check)
  ReviewPatch request _ _ check -> localSeqUnliftIO env $ \unlift -> checked (respond request) Right (unlift . check)
  FixRepository _ request _ _ _ _ _ _ _ _ check -> case respondFix request of
    Left message -> pure $ Left message
    Right result -> localSeqUnliftIO env $ \unlift -> (>>= matchingReview result) <$> unlift (check result)
  PrepareRepository _ -> pure $ Right ()
  AdmitRegression _ request _ _ _ _ -> pure $ respondAdmission request
  ReviewVerifiedFix _ request verified _ _ _ _ -> pure $ respondReview request >>= reviewOutcome request verified
  VerifyFixArtifacts _ request -> pure $ VerifiedFix request.result <$ respondArtifacts request
  ExplainVerifiedFix _ request (VerifiedFix result) -> pure do
    unless (request.result == result) $ Left "Explanation requires the same verified candidate"
    explanation <- respondExplanation request
    unless (validExplanation result explanation) $ Left "Invalid explanation fixture"
    pure $ ExplainedFix result explanation
  where
    checked :: Either Text Text -> (Text -> Either Text value) -> (value -> IO (Either Text ())) -> IO (Either Text Text)
    checked response parse check = runExceptT do
      raw <- hoistEither response
      value <- hoistEither $ parse raw
      ExceptT $ check value
      pure raw


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


workerHost :: [FilePath] -> Maybe FilePath -> Maybe FilePath -> Text -> FilePath -> Either Text Guest.Host
workerHost skills script archive image agent = do
  when (isNothing script && isJust archive) $ Left "A dependency archive requires a trusted preparation script"
  pure $ Guest.Host image (takeDirectory agent </> "sandbox.mjs") (skills <> [takeDirectory agent </> "skills" </> "superpowers"]) ((`Guest.Workspace` archive) <$> script)


nativeKey :: AE.ToJSON value => value -> FilePath
nativeKey = toString . Native.sha256 . toStrict . AE.encode
