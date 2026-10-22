module Data.Effectful.CodeReview (CodeReview (..), Request (..), FixRequest (..), TestArtifact (..), AdmissionRequest (..), AdmissionDecision (..), PhaseReceipt (..), FixResult (..), ArtifactRequest (..), ArtifactStore (..), FixReviewRequest (..), FixReviewDecision (..), ReviewedFix, reviewedFixResult, reviewedFixApproval, FixReviewOutcome (..), VerifiedFix, verifiedFixResult, ExplainedFix, explainedFixResult, fixExplanation, Explanation (..), PricingStatus (..), ThinkingLevel (..), TelemetryQuery (..), KnowledgeNote (..), KnowledgeRequest (..), ModelUsage (..), ModelCall (..), AllocationReceipt, allocationId, allocationAttempt, Regression (..), CheckReceipt (..), CheckStatus (..), RevisionSide (..), reviewRepository, fixRepository, admitRegression, verifyFixArtifacts, reviewVerifiedFix, explainVerifiedFix, runCodeReviewReal, runCodeReviewTest, validExplanation, workerHost) where

import Control.Concurrent.STM qualified as STM
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Effectful.CodeReview.Artifacts (ArtifactStore (..))
import Data.Effectful.CodeReview.Artifacts qualified as Native
import Data.Effectful.CodeReview.Checkout qualified as Checkout
import Data.Effectful.CodeReview.Explanation qualified as Media
import Data.Effectful.CodeReview.Guest qualified as Guest
import Data.Effectful.CodeReview.Outbox qualified as Outbox
import Data.Effectful.CodeReview.Policy qualified as Policy
import Data.Effectful.CodeReview.Types
import Data.OpenApi.Schema.Validation qualified as Schema
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Models.Projects.ProjectMemory qualified as Memory
import Pkg.DeriveUtils (UUIDId)
import Relude
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose)
import System.IO.Error (isEOFError)
import System.Process.Typed (ExitCode (..), createPipe, getStdin, getStdout, nullStream, proc, setEnv, setStderr, setStdin, setStdout, stopProcess, waitExitCode, waitExitCodeSTM, withProcessTerm)
import UnliftIO.Exception (finally, tryAny, tryJust)
import UnliftIO.Timeout qualified as Timeout


-- JSON-lines protocol: queries, usage, execution receipts, validation, and result.
data Message = Checkout () | Tool Policy.Invocation | Query TelemetryQuery | Knowledge KnowledgeRequest | Admit TestArtifact | Propose Native.CandidatePatch | Guest Guest.Operation | Validate AE.Value | Result AE.Value
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


data CodeReview :: Effect where
  ExplainVerifiedFix :: ArtifactStore -> ArtifactRequest -> VerifiedFix -> CodeReview m (Either Text ExplainedFix)
  VerifyFixArtifacts :: ArtifactStore -> ArtifactRequest -> CodeReview m (Either Text VerifiedFix)
  ReviewVerifiedFix :: FixReviewRequest -> VerifiedFix -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (AllocationReceipt -> m ()) -> CodeReview m (Either Text FixReviewOutcome)
  ReviewRepository :: Request "pr_review" -> (TelemetryQuery -> m (Either Text AE.Value)) -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (AllocationReceipt -> m ()) -> (CheckReceipt -> m ()) -> (AE.Value -> m (Either Text ())) -> CodeReview m (Either Text Text)
  AdmitRegression :: AdmissionRequest -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (AllocationReceipt -> m ()) -> CodeReview m (Either Text AdmissionDecision)
  FixRepository :: FixRequest -> (TelemetryQuery -> m (Either Text AE.Value)) -> (KnowledgeRequest -> m (Either Text AE.Value)) -> (TestArtifact -> m (Either Text Text)) -> (ModelCall -> m ()) -> (ModelUsage -> m ()) -> (AllocationReceipt -> m ()) -> (CheckReceipt -> m ()) -> (PhaseReceipt -> m (Either Text ())) -> (FixResult -> m (Either Text ReviewedFix)) -> CodeReview m (Either Text ReviewedFix)


type instance DispatchOf CodeReview = 'Dynamic


makeEffect ''CodeReview


data Task = AdvisoryTask (Request "pr_review") | FixTask FixRequest | AdmissionTask AdmissionRequest | PeerTask FixReviewRequest


data Callbacks result = Callbacks
  { respond :: TelemetryQuery -> IO (Either Text AE.Value)
  , knowledge :: KnowledgeRequest -> IO (Either Text AE.Value)
  , admit :: TestArtifact -> IO (Either Text Text)
  , calls :: ModelCall -> IO ()
  , usage :: ModelUsage -> IO ()
  , allocations :: AllocationReceipt -> IO ()
  , checks :: Maybe (CheckReceipt -> IO ())
  , phase :: PhaseReceipt -> IO (Either Text ())
  , validate :: AE.Value -> IO (Either Text result)
  }


runCodeReviewReal :: IOE :> es => Maybe ArtifactStore -> Maybe Guest.Host -> Maybe FilePath -> [FilePath] -> Eff (CodeReview ': es) a -> Eff es a
runCodeReviewReal artifactStore host executable skills = interpret \env -> \case
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
          , allocations = unlift . recordAllocation
          , checks = Just $ unlift . recordCheck
          , phase = const $ pure $ Left "Fix phases require an issue-fix task"
          , validate = \value -> (decodeUtf8 (AE.encode value) <$) <$> unlift (check value)
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
          , allocations = unlift . recordAllocation
          , checks = Just $ unlift . recordCheck
          , phase = unlift . recordPhase
          , validate = \value -> case AE.fromJSON @FixResult value of
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
          , allocations = unlift . recordAllocation
          , checks = Nothing
          , phase = const $ pure $ Left "Admission cannot advance a fix phase"
          , validate = \value -> pure $ case AE.fromJSON @AdmissionResult value of
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
              , allocations = unlift . recordAllocation
              , checks = Nothing
              , phase = const $ pure $ Left "Peer review cannot advance fix phases"
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
        let (scope, source, payload) = case task of
              AdvisoryTask request -> (Outbox.Advisory request.runId, Checkout.Repository request, AE.toJSON request{gitToken = ""})
              FixTask request -> (Outbox.IssueFix request.request.runId, Checkout.Repository request.request{head = request.request.base}, AE.object ["tag" AE..= ("FixTask" :: Text), "contents" AE..= request{request = request.request{gitToken = ""}}])
              AdmissionTask request -> (Outbox.Admission request.fix.request.runId, Checkout.Repository request.fix.request{head = request.fix.request.base}, AE.object ["tag" AE..= ("AdmitTask" :: Text), "contents" AE..= (request{fix = request.fix{request = request.fix.request{gitToken = ""}}} :: AdmissionRequest)])
              PeerTask request -> (Outbox.PeerReview request.fix.request.runId, Checkout.VerifiedCandidate request.artifacts, AE.object ["tag" AE..= ("ReviewFixTask" :: Text), "contents" AE..= (request{fix = request.fix{request = request.fix.request{gitToken = ""}}} :: FixReviewRequest)])
            role = case scope of Outbox.Advisory _ -> Policy.Advisory; Outbox.IssueFix _ -> Policy.Fix; Outbox.Admission _ -> Policy.Admission; Outbox.PeerReview _ -> Policy.Peer
            -- Roles migrate to a host-owned VM one at a time; the rest still create theirs in the child.
            owned = case task of
              AdmissionTask request -> (,request.fix.request.attemptId,Just request.fix.imageBuildId,Guest.InspectAdmission request.artifact,Just $ AE.object ["artifact" AE..= request.artifact]) <$> host
              _ -> Nothing
            record = \case
              Outbox.Call receipt -> callbacks.calls receipt $> Right ()
              Outbox.Usage receipt -> callbacks.usage receipt $> Right ()
              Outbox.Allocation receipt -> callbacks.allocations receipt $> Right ()
              Outbox.Check receipt -> maybe (pure $ Left "This task cannot record regression executions") (\save -> save receipt $> Right ()) callbacks.checks
              Outbox.Phase receipt -> callbacks.phase receipt
            withJournal action = maybe (action Nothing) (\configured -> Outbox.withOutbox configured scope $ action . Just) artifactStore
            allocate journal receipt = maybe (record $ Outbox.Allocation receipt) (\outbox -> Outbox.journal outbox (Outbox.Allocation receipt) >> Outbox.acknowledge outbox (Outbox.Allocation receipt) record) journal
        -- Replay persisted work before starting a child, fetching source or calling a model.
        outcome <- tryAny $ Timeout.timeout (480 * 1000000) $ withJournal \journal -> do
          recovered <- maybe (pure $ Right ()) (`Outbox.replay` record) journal
          case recovered of
            Left message -> pure $ Left message
            Right () -> do
              environment <- getEnvironment
              let childEnvironment = maybe environment (\outbox -> ("IMPACT_REVIEW_OUTBOX_DIR", Outbox.outboxPath outbox) : filter ((/= "IMPACT_REVIEW_OUTBOX_DIR") . fst) environment) journal
              withProcessTerm (setEnv childEnvironment $ setStdin createPipe $ setStdout createPipe $ setStderr nullStream $ proc command skills) \process -> do
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
                    receive supplied remaining submissions accepted = do
                      line <- tryJust (guard . isEOFError) $ BS.hGetLine (getStdout process)
                      case either (const $ Left "Review agent exited before completing the protocol") (first (const "Review agent returned an invalid protocol message") . (AE.eitherDecodeStrict >=> AET.parseEither parseMessage)) line of
                        Left failure -> pure $ Left failure
                        Right (Left receipt) -> do
                          let phase = case receipt of Outbox.Phase _ -> True; _ -> False
                              invalidates = case receipt of Outbox.Check _ -> True; Outbox.Phase _ -> True; _ -> False
                          if phase && remaining <= 0
                            then pure $ Left "Review agent exceeded its query budget"
                            else do
                              saved <- maybe (record receipt) (\outbox -> Outbox.acknowledge outbox receipt record) journal
                              writeMessage saved
                              case saved of
                                Left failure | not phase -> pure $ Left failure
                                _ -> receive supplied (remaining - bool 0 1 phase) submissions (if invalidates then Nothing else accepted)
                        Right (Right (Checkout ())) -> case (supplied, artifactStore) of
                          (Nothing, Just configured) -> do
                            let deliver checkout = (writeMessage (Right checkout :: Either Text Checkout.Prepared) >> receive (Just checkout) remaining submissions accepted) `finally` stop
                            prepared <- Checkout.withCheckout configured source \fetched -> do
                              let checkout = fetched{Checkout.policy = Policy.policy role (revisionText fetched.base) (revisionText fetched.head) fetched.rootInstructions}
                              case owned of
                                Nothing -> deliver checkout
                                Just (configuredHost, attempt, expected, setup, envelope) ->
                                  join <$> Guest.withGuest configuredHost expected checkout.directory checkout.head setup attempt (void . allocate journal) \skillsDir operate ->
                                    writeIORef guest (Just operate) >> deliver checkout{Checkout.guest = Just skillsDir, Checkout.envelope = envelope}
                            pure $ join prepared
                          _ -> pure $ Left "Repository checkout was unavailable or already supplied"
                        Right (Right (Tool invocation))
                          | isNothing supplied -> pure $ Left "Tool invocation requires its repository checkout"
                          | remaining > 0 -> do
                              let allowed = Policy.toolsForRole role
                                  parsed :: (AE.Value -> AET.Parser value) -> (value -> IO (Either Text AE.Value)) -> IO (Either Text AE.Value)
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
                              response <- if invocation.name `elem` allowed && null (Schema.validateJSON mempty (Policy.toolSchema invocation.name) invocation.arguments) then request else pure $ Left "Tool is unavailable or its arguments violate the host contract"
                              writeMessage response
                              receive supplied (remaining - 1) submissions Nothing
                          | otherwise -> pure $ Left "Review agent exceeded its query budget"
                        Right (Right (Query query))
                          | remaining > 0 -> callbacks.respond query >>= writeMessage >> receive supplied (remaining - 1) submissions Nothing
                          | otherwise -> pure $ Left "Review agent exceeded its query budget"
                        Right (Right (Knowledge requestKnowledge))
                          | remaining > 0 -> callbacks.knowledge requestKnowledge >>= writeMessage >> receive supplied (remaining - 1) submissions Nothing
                          | otherwise -> pure $ Left "Review agent exceeded its query budget"
                        Right (Right (Admit artifact))
                          | remaining > 0 -> callbacks.admit artifact >>= writeMessage >> receive supplied (remaining - 1) submissions Nothing
                          | otherwise -> pure $ Left "Review agent exceeded its query budget"
                        Right (Right (Propose patch)) -> case (task, supplied, journal) of
                          (FixTask request, Just checkout, Just outbox) | remaining > 0 -> do
                            captured <- Native.captureCandidate (Outbox.outboxPath outbox <> "/checks") request checkout.directory patch
                            writeMessage captured
                            receive supplied (remaining - 1) submissions Nothing
                          _ -> pure $ Left "Candidate capture requires a prepared issue-fix task and remaining budget"
                        Right (Right (Guest operation)) ->
                          readIORef guest >>= \case
                            -- Pi's own file and shell calls neither spend the request budget nor keep an accepted draft.
                            Just operate -> operate operation >>= writeMessage >> receive supplied remaining submissions Nothing
                            Nothing -> pure $ Left "Guest operations require a host-owned sandbox"
                        Right (Right (Validate draft))
                          | isNothing supplied && isJust artifactStore -> pure $ Left "Review agent has not acquired its repository checkout"
                          | submissions > 0 -> do
                              checked <- callbacks.validate draft
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
          Left _ -> Left "Review agent could not be executed"
          Right Nothing -> Left "Review agent exceeded its eight-minute budget"
          Right (Just result) -> result


-- | The worker directory supplies the SDK bridge and the bundled skill pack, after trusted skills.
workerHost :: [FilePath] -> Text -> FilePath -> Guest.Host
workerHost skills image agent = Guest.Host image (takeDirectory agent </> "sandbox.mjs") (skills <> [takeDirectory agent </> "skills" </> "superpowers"]) Nothing


runCodeReviewTest :: IOE :> es => (Request "pr_review" -> Either Text Text) -> (FixRequest -> Either Text FixResult) -> (AdmissionRequest -> Either Text AdmissionDecision) -> (ArtifactRequest -> Either Text ()) -> (ArtifactRequest -> Either Text Explanation) -> (FixReviewRequest -> Either Text FixReviewDecision) -> Eff (CodeReview ': es) a -> Eff es a
runCodeReviewTest respond respondFix respondAdmission respondArtifacts respondExplanation respondReview = interpret \env -> \case
  ReviewRepository request _ _ _ _ _ _ _ -> pure $ respond request
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
