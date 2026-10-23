module Data.Effectful.CodeReview (CodeReview (..), Receipt (..), meteringReceipt, Request (..), FixRequest (..), TestArtifact (..), AdmissionRequest (..), AdmissionDecision (..), PhaseReceipt (..), FixResult (..), ArtifactRequest (..), ArtifactStore (..), FixReviewRequest (..), FixReviewDecision (..), ReviewedFix, reviewedFixResult, reviewedFixApproval, FixReviewOutcome (..), VerifiedFix, verifiedFixResult, ExplainedFix, explainedFixResult, fixExplanation, Explanation (..), PricingStatus (..), ThinkingLevel (..), TelemetryQuery (..), KnowledgeNote (..), KnowledgeRequest (..), ModelUsage (..), ModelCall (..), AllocationReceipt, allocationId, allocationAttempt, Regression (..), CheckReceipt (..), CheckStatus (..), RevisionSide (..), reviewRepository, fixRepository, admitRegression, verifyFixArtifacts, reviewVerifiedFix, explainVerifiedFix, runCodeReviewReal, runCodeReviewWith, runCodeReviewTest, validExplanation, workerHost) where

import Control.Concurrent.STM qualified as STM
import Control.Monad (foldM)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AET
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Effectful.AgentWorkspace (publish, sync)
import Data.Effectful.AgentWorkspace qualified as Workspace
import Data.Effectful.AgentWorkspace.Guest qualified as Guest
import Data.Effectful.CodeReview.Artifacts (ArtifactStore (..), readPrivateArtifact)
import Data.Effectful.CodeReview.Artifacts qualified as Media
import Data.Effectful.CodeReview.Artifacts qualified as Native
import Data.Effectful.CodeReview.Checkout qualified as Checkout
import Data.Effectful.CodeReview.Policy qualified as Policy
import Data.Effectful.CodeReview.Repair qualified as Repair
import Data.Effectful.CodeReview.Types
import Data.OpenApi.Schema.Validation qualified as Schema
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUID4
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Models.Projects.ProjectMemory qualified as Memory
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import System.Directory (canonicalizePath, createDirectoryIfMissing, listDirectory, removeFile)
import System.Environment (getEnvironment)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hClose)
import System.IO.Error (isEOFError, userError)
import System.Posix.Files qualified as Posix
import System.Process.Typed (ExitCode (..), ProcessConfig, createPipe, getStdin, getStdout, nullStream, proc, setEnv, setStderr, setStdin, setStdout, stopProcess, waitExitCode, waitExitCodeSTM, withProcessTerm)
import UnliftIO.Exception (finally, throwIO, tryAny, tryJust)
import UnliftIO.Timeout qualified as Timeout


-- JSON-lines protocol: queries, usage, execution receipts, validation, and result.
data Message = Checkout () | Tool Policy.Invocation | Guest Guest.Operation | Validate AE.Value | Result AE.Value
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


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


data Receipt check phase = Call ModelCall | Usage ModelUsage | Allocation AllocationReceipt | Check check | Phase phase
  deriving stock (Foldable, Functor, Generic, Traversable)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


deriveBifunctor ''Receipt
deriveBifoldable ''Receipt
deriveBitraversable ''Receipt


meteringReceipt :: Receipt Void Void -> Receipt check phase
meteringReceipt = bimap absurd absurd


type WireReceipt = Receipt CheckReceipt PhaseReceipt


data CodeReview :: Effect where
  ExplainVerifiedFix :: ArtifactStore -> ArtifactRequest -> VerifiedFix -> CodeReview m (Either Text ExplainedFix)
  VerifyFixArtifacts :: ArtifactStore -> ArtifactRequest -> CodeReview m (Either Text VerifiedFix)
  ReviewVerifiedFix :: FixReviewRequest -> VerifiedFix -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (Receipt Void Void -> m (Either Text ())) -> CodeReview m (Either Text FixReviewOutcome)
  ReviewRepository :: Request "pr_review" -> (TelemetryQuery -> m (Either Text AE.Value)) -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (Receipt CheckReceipt Void -> m (Either Text ())) -> (AE.Value -> m (Either Text ())) -> CodeReview m (Either Text Text)
  AdmitRegression :: AdmissionRequest -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (Receipt Void Void -> m (Either Text ())) -> CodeReview m (Either Text AdmissionDecision)
  FixRepository :: FixRequest -> (TelemetryQuery -> m (Either Text AE.Value)) -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (TestArtifact -> m (Either Text Text)) -> (WireReceipt -> m (Either Text ())) -> (FixResult -> m (Either Text ReviewedFix)) -> CodeReview m (Either Text ReviewedFix)


type instance DispatchOf CodeReview = 'Dynamic


makeEffect ''CodeReview


-- | A role whose VM the host owns: who is billed for it, the image it must use, how its guest is prepared from
-- recorded evidence, and the fields the agent wraps around its submission.
data Ownership = Ownership {host :: Guest.Host, attempt :: UUIDId "agent_attempt", image :: Maybe (UUIDId "guest_image"), setup :: Map Text ByteString -> Guest.Setup, envelope :: Maybe Checkout.Envelope, lease :: Maybe Guest.Lease}


-- | The host refused a receipt the run cannot continue without; its reason is the run's failure.
newtype ReceiptRefused = ReceiptRefused Text
  deriving stock (Show)
  deriving anyclass (Exception)


data Task = AdvisoryTask (Request "pr_review") | FixTask FixRequest | AdmissionTask AdmissionRequest | PeerTask FixReviewRequest


data Callbacks result = Callbacks
  { respond :: TelemetryQuery -> IO (Either Text AE.Value)
  , knowledge :: KnowledgeRequest -> IO (Either Text AE.Value)
  , admit :: TestArtifact -> IO (Either Text Text)
  , record :: WireReceipt -> IO (Either Text ())
  , validate :: AE.Value -> IO (Either Text result)
  }


runCodeReviewReal :: IOE :> es => Maybe ArtifactStore -> Maybe Guest.Host -> Maybe FilePath -> [FilePath] -> Eff (CodeReview ': es) a -> Eff es a
runCodeReviewReal artifactStore host executable skills = runCodeReviewWith artifactStore host ((\command environment -> setEnv environment $ setStderr nullStream $ proc command skills) <$> executable)


runCodeReviewWith :: IOE :> es => Maybe ArtifactStore -> Maybe Guest.Host -> Maybe ([(String, String)] -> ProcessConfig () () ()) -> Eff (CodeReview ': es) a -> Eff es a
runCodeReviewWith artifactStore host executable = interpret \env -> \case
  ReviewRepository request respond knowledge record check -> localSeqUnliftIO env \unlift ->
    liftIO
      $ launch
        (AdvisoryTask request)
        Callbacks
          { respond = unlift . respond
          , knowledge = unlift . knowledge
          , admit = const $ pure $ Left "Test admission requires an issue-fix task"
          , record = either (pure . Left) (unlift . record) . bitraverse Right (const $ Left "Fix phases require an issue-fix task")
          , validate = \value -> (decodeUtf8 (AE.encode value) <$) <$> unlift (check value)
          }
  FixRepository request respond knowledge admit record check -> localSeqUnliftIO env \unlift ->
    liftIO
      $ launch
        (FixTask request)
        Callbacks
          { respond = unlift . respond
          , knowledge = unlift . knowledge
          , admit = unlift . admit
          , record = unlift . record
          , validate = \value -> case AE.fromJSON @FixResult value of
              AE.Error _ -> pure $ Left "Fix result does not match the required schema"
              AE.Success result -> (>>= matchingReview result) <$> unlift (check result)
          }
  AdmitRegression request knowledge record -> localSeqUnliftIO env \unlift ->
    liftIO
      $ launch
        (AdmissionTask request)
        Callbacks
          { respond = const $ pure $ Left "Admission has no production query capability"
          , knowledge = unlift . knowledge
          , admit = const $ pure $ Left "Admission cannot recursively admit a test"
          , record = either (pure . Left) (unlift . record) . bitraverse (const $ Left "This task cannot record regression executions") (const $ Left "Admission cannot advance a fix phase")
          , validate = \value -> pure $ case AE.fromJSON @AdmissionResult value of
              AE.Success result
                | result.artifact == request.artifact
                , let rationale = case result.decision of Admitted text -> text; Rejected text -> text
                , not (T.null $ T.strip rationale) && T.length rationale <= 1200 ->
                    Right result.decision
              _ -> Left "Admission result must bind the exact test artifact and a nonempty rationale"
          }
  ReviewVerifiedFix request verified knowledge record -> localSeqUnliftIO env \unlift ->
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
              , record = either (pure . Left) (unlift . record) . bitraverse (const $ Left "This task cannot record regression executions") (const $ Left "Peer review cannot advance fix phases")
              , validate = \value -> pure $ case AE.fromJSON @FixReviewResult value of
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
    launch :: Task -> Callbacks result -> IO (Either Text result)
    launch task callbacks = case executable of
      Nothing -> pure $ Left "Checkout-backed review agent is not configured"
      Just command -> do
        verifierSession <- UUID4.nextRandom
        let (scope, source, payload) = case task of
              AdvisoryTask request -> (Advisory request.runId, Checkout.Repository request, AE.toJSON request{gitToken = ""})
              FixTask request -> (IssueFix request.request.runId, Checkout.Repository request.request{head = request.request.base}, AE.object ["tag" AE..= ("FixTask" :: Text), "contents" AE..= request{request = request.request{gitToken = ""}}])
              AdmissionTask request -> (Admission request.fix.request.runId, Checkout.Repository request.fix.request{head = request.fix.request.base}, AE.object ["tag" AE..= ("AdmitTask" :: Text), "contents" AE..= (request{fix = request.fix{request = request.fix.request{gitToken = ""}}} :: AdmissionRequest)])
              PeerTask request -> (PeerReview request.fix.request.runId, Checkout.VerifiedCandidate request.artifacts, AE.object ["tag" AE..= ("ReviewFixTask" :: Text), "contents" AE..= (request{fix = request.fix{request = request.fix.request{gitToken = ""}}} :: FixReviewRequest)])
            role = case scope of Advisory _ -> Policy.Advisory; IssueFix _ -> Policy.Fix; Admission _ -> Policy.Admission; PeerReview _ -> Policy.Peer
            owned =
              host <&> \configured ->
                let
                  -- Prepared checkpoints are keyed by the pinned project, repository and base, never by the reviewed head.
                  own :: Request run -> Maybe (UUIDId "guest_image") -> (Map Text ByteString -> Guest.Setup) -> Maybe Checkout.Envelope -> Ownership
                  own request pinned setup envelope = Ownership configured request.attemptId pinned setup envelope do
                    store <- artifactStore
                    Guest.Lease store.cacheDir <$> rightToMaybe (Workspace.mkRequest request.projectId request.repository request.base pinned)
                 in
                  case task of
                    AdvisoryTask request -> own request Nothing (const Guest.Explore) Nothing
                    AdmissionTask request -> own request.fix.request (Just request.fix.imageBuildId) (const $ Guest.InspectAdmission request.artifact) (Just $ Checkout.Artifact request.artifact)
                    PeerTask request -> own request.fix.request (Just request.fix.imageBuildId) Guest.ReviewEvidence (Just $ Checkout.Result request.artifacts.result)
                    FixTask request -> own request.request (Just request.imageBuildId) (const Guest.LockForReproduction) Nothing
            honours invocation = invocation.name `elem` Policy.toolsForRole role && null (Schema.validateJSON mempty (Policy.toolSchema invocation.name) invocation.arguments)
            -- Fresh verifier VMs replay a frozen artifact. Their receipts are already journaled beside the outbox;
            -- each is acknowledged, then @observe@d (a fix also records it as a phase). Errors carry the report.
            replayFrozen :: Guest.Session -> Outbox -> Request run -> AE.Value -> [(Text, Text)] -> (CheckReceipt -> IO ()) -> IO (Either Text (AE.Value, [CheckReceipt]))
            replayFrozen session outbox pinned artifact revisions observe = do
              let options = session.verifier ((.path) outbox </> "checks")
              case Workspace.mkRegressionRequest pinned.projectId pinned.runId pinned.attemptId verifierSession pinned.base session.imageBuildId session.baseline artifact revisions of
                Left _ -> pure $ Left "Regression arguments do not match the host contract"
                Right regression ->
                  tryAny (Workspace.verifyRegression options regression) >>= \case
                    Left failure -> pure $ Left $ maybe "Local check could not run" Workspace.regressionErrorText $ fromException failure
                    Right result -> do
                      let checks = result.receipts
                      for_ checks $ \check -> (acknowledgeReceipt outbox (Check check) record >>= orThrow) >> observe check
                      traverse_ (uncurry session.copyIn) $ Workspace.regressionLogs options result
                      pure $ if result.isError then Left (decodeUtf8 $ AE.encode result.report) else Right (AE.toJSON result.report, checks)
            -- A rejected phase ends the run, as an unacknowledged receipt did in the child.
            persistPhase outbox attempt step = UUID4.nextRandom >>= \identifier -> settle outbox (Phase $ PhaseReceipt (UUIDId identifier) attempt step) >>= orThrow
            orThrow = either (throwIO . ReceiptRefused) pure
            settle outbox receipt = journalReceipt outbox receipt >> acknowledgeReceipt outbox receipt record
            record = callbacks.record
            withJournal action = maybe (action Nothing) (\configured -> withOutbox configured scope $ action . Just) artifactStore
            allocate journal receipt = maybe record settle journal $ Allocation receipt
        -- Replay persisted work before starting a child, fetching source or calling a model.
        outcome <- tryAny $ Timeout.timeout (480 * 1000000) $ withJournal \journal -> do
          recovered <- maybe (pure $ Right ()) (`replayReceipts` record) journal
          case recovered of
            Left message -> pure $ Left message
            Right () -> do
              environment <- getEnvironment
              let childEnvironment = maybe environment (\outbox -> ("IMPACT_REVIEW_OUTBOX_DIR", (.path) outbox) : filter ((/= "IMPACT_REVIEW_OUTBOX_DIR") . fst) environment) journal
              withProcessTerm (setStdin createPipe $ setStdout createPipe $ command childEnvironment) \process -> do
                guest <- newIORef Nothing
                repairing <- newIORef Nothing
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
                    parseMessage :: AE.Value -> AET.Parser (Either WireReceipt Message)
                    parseMessage value = (Left <$> AE.parseJSON value) <|> (Right <$> AE.parseJSON value)
                    receive supplied remaining submissions accepted = do
                      line <- tryJust (guard . isEOFError) $ BS.hGetLine (getStdout process)
                      case either (const $ Left "Review agent exited before completing the protocol") (first (const "Review agent returned an invalid protocol message") . (AE.eitherDecodeStrict >=> AET.parseEither parseMessage)) line of
                        Left failure -> pure $ Left failure
                        Right (Left receipt) -> do
                          let phase = case receipt of Phase _ -> True; _ -> False
                              invalidates = case receipt of Check _ -> True; Phase _ -> True; _ -> False
                          if phase && remaining <= 0
                            then pure $ Left "Review agent exceeded its query budget"
                            else do
                              saved <- maybe (record receipt) (\outbox -> acknowledgeReceipt outbox receipt record) journal
                              writeMessage saved
                              case saved of
                                Left failure | not phase -> pure $ Left failure
                                _ -> receive supplied (remaining - bool 0 1 phase) submissions (if invalidates then Nothing else accepted)
                        Right (Right (Checkout ())) -> case (supplied, artifactStore) of
                          (Nothing, Just configured) -> do
                            let deliver checkout = (writeMessage (Right checkout :: Either Text Checkout.Prepared) >> receive (Just checkout) remaining submissions accepted) `finally` stop
                            prepared <- Checkout.withCheckout configured source \fetched evidence -> do
                              let checkout = fetched{Checkout.policy = Policy.policy role (revisionText fetched.base) (revisionText fetched.head) fetched.rootInstructions}
                              case owned of
                                Nothing -> deliver checkout
                                Just ownership ->
                                  join <$> Guest.withGuest ownership.host ownership.image checkout.directory checkout.head (ownership.setup evidence) ownership.lease ownership.attempt (allocate journal >=> orThrow) (\receipt -> for_ journal (`journalReceipt` Allocation receipt)) \session ->
                                    do
                                      writeIORef guest (Just session)
                                      case (task, journal) of
                                        (FixTask job, Just outbox) -> do
                                          stage <- newIORef Nothing
                                          persistPhase outbox job.request.attemptId RestartReproduction
                                          writeIORef repairing $ Just $ Repair.Repair session job checkout.directory ((.path) outbox </> "checks") callbacks.admit (persistPhase outbox job.request.attemptId) (\artifact revisions -> replayFrozen session outbox job.request (AE.toJSON artifact) revisions (persistPhase outbox job.request.attemptId . Observe)) stage
                                        _ -> pass
                                      deliver checkout{Checkout.guest = Just session.skills, Checkout.envelope = ownership.envelope}
                            pure $ join prepared
                          _ -> pure $ Left "Repository checkout was unavailable or already supplied"
                        Right (Right (Tool invocation))
                          | isNothing supplied && isJust artifactStore -> pure $ Left "Tool invocation requires its repository checkout"
                          | invocation.name == Policy.RunRegression -> do
                              session <- readIORef guest
                              response <- case (session, journal, task) of
                                (Just active, Just outbox, AdvisoryTask advisory) | honours invocation -> fmap fst <$> replayFrozen active outbox advisory invocation.arguments [("base", advisory.base), ("head", advisory.head)] (const pass)
                                _ -> pure $ Left "Tool is unavailable or its arguments violate the host contract"
                              writeMessage response
                              -- Comparisons spend the verifier's own budget of three per session, not the request budget.
                              receive supplied remaining submissions Nothing
                          -- Fix steps act on the host-owned workspace; only reproduction, which asks the host to admit a test, spends the request budget.
                          | invocation.name `elem` [Policy.ReproduceIssue, Policy.ProposeFix, Policy.VerifyFix] && (remaining > 0 || invocation.name /= Policy.ReproduceIssue) -> do
                              repair <- readIORef repairing
                              response <- case repair of
                                Just active | honours invocation -> case invocation.name of
                                  Policy.ReproduceIssue -> Repair.reproduce active invocation.arguments
                                  Policy.ProposeFix -> maybe (pure $ Left "A root cause is required") (Repair.propose active) $ AET.parseMaybe (AE.withObject "Candidate" (AE..: "rootCause")) invocation.arguments
                                  _ -> Repair.verify active
                                _ -> pure $ Left "Tool is unavailable or its arguments violate the host contract"
                              writeMessage response
                              receive supplied (remaining - bool 0 1 (invocation.name == Policy.ReproduceIssue)) submissions Nothing
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
                              response <- if honours invocation then request else pure $ Left "Tool is unavailable or its arguments violate the host contract"
                              writeMessage response
                              receive supplied (remaining - 1) submissions Nothing
                          | otherwise -> pure $ Left "Review agent exceeded its query budget"
                        Right (Right (Guest operation)) ->
                          readIORef guest >>= \case
                            -- Pi's own file and shell calls neither spend the request budget nor keep an accepted draft.
                            Just session -> session.operate operation >>= writeMessage >> receive supplied remaining submissions Nothing
                            Nothing -> pure $ Left "Guest operations require a host-owned sandbox"
                        Right (Right (Validate draft))
                          | isNothing supplied && isJust artifactStore -> pure $ Left "Review agent has not acquired its repository checkout"
                          | submissions > 0 -> do
                              composed <- readIORef repairing >>= maybe (pure $ Right draft) (`Repair.submission` draft)
                              checked <- either (pure . Left) callbacks.validate composed
                              writeMessage $ void checked
                              receive supplied remaining (submissions - 1) $ either (const Nothing) (Just . (draft,)) checked
                          | otherwise -> pure $ Left "Review agent exceeded its submission budget"
                        Right (Right (Result draft)) -> case accepted of
                          Just (acceptedDraft, result) | acceptedDraft == draft -> do
                            status <- waitExitCode process
                            pure $ if status == ExitSuccess then Right result else Left "Review agent did not produce a validated result"
                          _ -> pure $ Left "Review agent returned an unvalidated draft"
                writeMessage payload
                receive Nothing (20 :: Int) (5 :: Int) Nothing
        pure case outcome of
          Left failure | Just (ReceiptRefused reason) <- fromException failure -> Left reason
          Left _ -> Left "Review agent could not be executed"
          Right Nothing -> Left "Review agent exceeded its eight-minute budget"
          Right (Just result) -> result


-- | The worker directory supplies the SDK bridge and the bundled skill pack, after trusted skills.
-- A dependency archive is only meaningful with the preparation script that installs it.
workerHost :: [FilePath] -> Maybe FilePath -> Maybe FilePath -> Text -> FilePath -> Guest.Host
workerHost skills script archive image agent = Guest.Host image (takeDirectory agent </> "sandbox.mjs") (skills <> [takeDirectory agent </> "skills" </> "superpowers"]) ((`Guest.Workspace` archive) <$> script)


runCodeReviewTest :: IOE :> es => (Request "pr_review" -> Either Text Text) -> (FixRequest -> Either Text FixResult) -> (AdmissionRequest -> Either Text AdmissionDecision) -> (ArtifactRequest -> Either Text ()) -> (ArtifactRequest -> Either Text Explanation) -> (FixReviewRequest -> Either Text FixReviewDecision) -> Eff (CodeReview ': es) a -> Eff es a
runCodeReviewTest respond respondFix respondAdmission respondArtifacts respondExplanation respondReview = interpret \env -> \case
  ReviewRepository request _ _ _ _ -> pure $ respond request
  FixRepository request _ _ _ _ check -> case respondFix request of
    Left message -> pure $ Left message
    Right result -> localSeqUnliftIO env $ \unlift -> (>>= matchingReview result) <$> unlift (check result)
  AdmitRegression request _ _ -> pure $ respondAdmission request
  ReviewVerifiedFix request verified _ _ -> pure $ respondReview request >>= reviewOutcome request verified
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


data Scope = Advisory (UUIDId "pr_review") | IssueFix (UUIDId "issue_fix") | Admission (UUIDId "issue_fix") | PeerReview (UUIDId "issue_fix")


data Outbox = Outbox {path :: FilePath, scope :: Scope}


withOutbox :: ArtifactStore -> Scope -> (Outbox -> IO a) -> IO a
withOutbox store scope action = do
  root <- canonicalizePath store.usageDir
  let parts = case scope of
        Advisory run -> [toString run.toText]
        IssueFix run -> ["fixes", toString run.toText]
        Admission run -> ["admissions", toString run.toText]
        PeerReview run -> ["fix-reviews", toString run.toText]
  directory <-
    foldM
      ( \parent part -> do
          let child = parent </> part
          createDirectoryIfMissing False child
          canonicalizePath child >>= receiptInvariant . (== child)
          Posix.setFileMode child 0o700
          sync parent
          pure child
      )
      root
      parts
  withFileLock (directory </> ".native.lock") Exclusive $ const $ action $ Outbox directory scope


-- Decode the whole run before any callback; malformed entries stay available for repair.
replayReceipts :: Outbox -> (WireReceipt -> IO (Either Text ())) -> IO (Either Text ())
replayReceipts outbox record = do
  files <- filter ((== ".json") . takeExtension) <$> listDirectory outbox.path
  receipts <- traverse (readReceipt outbox) files
  runExceptT $ traverse_ (\receipt -> ExceptT $ acknowledgeReceipt outbox receipt record) $ sortOn (\receipt -> (rank receipt, receiptId receipt)) receipts
  where
    rank = \case Call _ -> 0 :: Int; Phase _ -> 2; _ -> 1


-- Host-measured receipts take the same durable path as SDK ones.
journalReceipt :: Outbox -> WireReceipt -> IO ()
journalReceipt outbox receipt = publish (outbox.path </> toString (receiptId receipt) <> ".json") receipt


-- The SDK journals before transmission; the database must accept before unlink/ACK.
acknowledgeReceipt :: Outbox -> WireReceipt -> (WireReceipt -> IO (Either Text ())) -> IO (Either Text ())
acknowledgeReceipt outbox receipt record = do
  let filename = toString (receiptId receipt) <> ".json"
  stored <- readReceipt outbox filename
  receiptInvariant $ AE.toJSON stored == AE.toJSON receipt
  result <- record receipt
  for_ result $ \() -> removeFile (outbox.path </> filename) >> sync outbox.path
  pure result


readReceipt :: Outbox -> FilePath -> IO WireReceipt
readReceipt outbox filename = do
  receipt <- either (const invalidReceipt) pure . (AE.eitherDecodeStrict >=> AET.parseEither storedReceipt) =<< readPrivateArtifact outbox.path filename (2 * 1048576)
  receiptInvariant $ filename == toString (receiptId receipt) <> ".json"
  receiptInvariant $ case (outbox.scope, receipt) of
    (IssueFix _, _) -> True
    (_, Phase _) -> False
    (Advisory _, _) -> True
    (_, Check _) -> False
    _ -> True
  pure receipt
  where
    -- Only files accept legacy untagged usage; the live wire remains a tagged sum.
    storedReceipt value = AE.withObject "Stored receipt" (\fields -> if KM.member "tag" fields then AE.parseJSON value else Usage <$> AE.parseJSON value) value


receiptId :: WireReceipt -> Text
receiptId = \case
  Call receipt -> receipt.id.toText
  Usage receipt -> receipt.id.toText
  Allocation receipt -> (allocationId receipt).toText
  Check receipt -> receipt.id.toText
  Phase receipt -> receipt.id.toText


receiptInvariant :: Bool -> IO ()
receiptInvariant condition = unless condition invalidReceipt


invalidReceipt :: IO a
invalidReceipt = throwIO $ userError "Invalid durable agent receipt"
