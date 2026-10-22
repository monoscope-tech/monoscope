module Data.Effectful.CodeReview.Types (Evidence (..), EvidenceKind (..), Observation (..), QueryKind (..), QueryObservation (..), KnowledgeSnapshot (..), ThinkingLevel (..), Request (..), FixRequest (..), TestArtifact (..), AdmissionRequest (..), AdmissionResult (..), AdmissionDecision (..), PhaseReceipt (..), FixResult (..), ArtifactRequest (..), FixReviewRequest (..), FixReviewDecision (..), FixReviewResult (..), Explanation (..), validExplanation, PricingStatus (..), TelemetryQuery (..), KnowledgeNote (..), KnowledgeRequest (..), ModelUsage (..), ModelCall (..), AllocationReceipt, AllocationOutcome (..), allocationId, allocationAttempt, agentResources, hostAllocation, Regression (..), CheckReceipt (..), CheckStatus (..), RevisionSide (..), WorkspaceFingerprint, mkWorkspaceFingerprint, Revision, mkRevision, revisionText, Candidate (..), Step (..), Failure (..)) where

import Data.Aeson qualified as AE
import Data.Char (isHexDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Hasql.Interpolate qualified as HI
import Models.Projects.ProjectMemory qualified as Memory
import OpenAI.V1.Models (Model)
import Pkg.DeriveUtils (UUIDId, WrappedEnumSC (..))
import Relude


data ThinkingLevel = Off | Minimal | Low | Medium | High
  deriving stock (Generic, Read, Show)
  deriving (AE.ToJSON) via WrappedEnumSC 'Nothing "" ThinkingLevel


-- No Show: this subprocess wire request contains the installation token.
data Request run = Request
  { projectId :: UUIDId "project"
  , runId :: UUIDId run
  , attemptId :: UUIDId "agent_attempt"
  , repository :: Text
  , base :: Text
  , head :: Text
  , gitToken :: Text
  , prompt :: Text
  , provider :: Text
  , model :: Model
  , thinkingLevel :: ThinkingLevel
  }
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data TelemetryQuery = TelemetryQuery {service :: Text, query :: Text}
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data KnowledgeNote = KnowledgeNote {service :: Text, summary :: Text, evidenceKeys :: NonEmpty Text}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data KnowledgeRequest = Recall Text | Remember KnowledgeNote | ProjectMemory Memory.Request
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


-- A call is recorded before provider work; absence of usage remains unknown.
data ModelCall = ModelCall {id :: UUIDId "model_call", attemptId :: UUIDId "agent_attempt", provider :: Text, model :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Provider usage is emitted by the trusted host, never by a repository tool.
data ModelUsage = ModelUsage
  { id :: UUIDId "model_usage"
  , attemptId :: UUIDId "agent_attempt"
  , callId :: Maybe (UUIDId "model_call") -- Legacy receipts predate call starts.
  , provider :: Text
  , model :: Text
  , responseId :: Maybe Text
  , inputTokens :: Word32
  , outputTokens :: Word32
  , cacheReadTokens :: Word32
  , cacheWriteTokens :: Word32
  , costNanousd :: Maybe Word64
  , pricing :: AE.Value
  , succeeded :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data AllocationOutcome = Released | ReleaseFailed
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" AllocationOutcome


data AllocationData = AllocationData {id :: UUIDId "agent_allocation", attemptId :: UUIDId "agent_attempt", durationMs :: Word64, cpus :: Word16, memoryMiB :: Word32, outcome :: AllocationOutcome}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | The private wire product preserves positive resources through JSON round-trips.
-- >>> let receipt cpus = AE.object ["id" AE..= ("00000000-0000-4000-8000-000000000001" :: Text), "attemptId" AE..= ("00000000-0000-4000-8000-000000000002" :: Text), "durationMs" AE..= (1 :: Int), "cpus" AE..= (cpus :: Int), "memoryMiB" AE..= (1024 :: Int), "outcome" AE..= ("released" :: Text)]
-- >>> let decoded = AE.fromJSON (receipt 2) :: AE.Result AllocationReceipt
-- >>> case decoded of AE.Success _ -> True; _ -> False
-- True
-- >>> (decoded >>= AE.fromJSON . AE.toJSON) == decoded
-- True
-- >>> case (AE.fromJSON (receipt 0) :: AE.Result AllocationReceipt) of AE.Error _ -> True; _ -> False
-- True
newtype AllocationReceipt = AllocationReceipt AllocationData
  deriving stock (Eq, Show)
  deriving newtype (AE.ToJSON)


instance AE.FromJSON AllocationReceipt where
  parseJSON value = do
    receipt <- AE.parseJSON value
    unless (receipt.cpus > 0 && receipt.memoryMiB > 0) $ fail "Allocation resources must be positive"
    pure $ AllocationReceipt receipt


allocationId :: AllocationReceipt -> UUIDId "agent_allocation"
allocationId (AllocationReceipt receipt) = receipt.id


allocationAttempt :: AllocationReceipt -> UUIDId "agent_attempt"
allocationAttempt (AllocationReceipt receipt) = receipt.attemptId


-- | Every agent VM uses this shape, so host-measured receipts cannot report other resources.
agentResources :: (Word16, Word32)
agentResources = (2, 1024)


hostAllocation :: UUIDId "agent_allocation" -> UUIDId "agent_attempt" -> Word64 -> AllocationOutcome -> AllocationReceipt
hostAllocation identifier attempt durationMs = AllocationReceipt . uncurry (AllocationData identifier attempt durationMs) agentResources


data RevisionSide = Base | Head
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" RevisionSide


data CheckStatus = Passed | Failed | Blocked | Invalidated | TimedOut | OutputLimit | Cancelled | CleanupFailed
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" CheckStatus


-- The digest binds command, test patch, and expected assertion; constructors record placement.
data Regression = ScriptCheck {sha256 :: Text} | InjectedTest {sha256 :: Text} | RetainedTest {sha256 :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Execution metadata comes from the host verifier, without scripts or log bodies.
data CheckReceipt = CheckReceipt
  { id :: UUIDId "check_execution"
  , attemptId :: UUIDId "agent_attempt"
  , checkId :: UUIDId "regression_check"
  , side :: RevisionSide
  , revision :: Text
  , imageBuildId :: Text
  , scriptSha256 :: Text
  , regression :: Maybe Regression
  , status :: CheckStatus
  , exitCode :: Maybe Int
  , durationMs :: Word64
  , cpus :: Word16
  , memoryMiB :: Word32
  , logBytes :: Word32
  , logSha256 :: Text
  , workspace :: Maybe WorkspaceFingerprint
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Identifies the prepared disk; absence in a receipt means a pristine toolchain image.
-- >>> let fingerprint = mkWorkspaceFingerprint (T.replicate 64 "a")
-- >>> fmap (const True) fingerprint
-- Right True
-- >>> (fingerprint >>= first toText . AE.eitherDecode . AE.encode) == fingerprint
-- True
-- >>> mkWorkspaceFingerprint "invalid"
-- Left "Invalid workspace fingerprint"
newtype WorkspaceFingerprint = WorkspaceFingerprint Text
  deriving stock (Eq, Show)
  deriving newtype (AE.ToJSON)


mkWorkspaceFingerprint :: Text -> Either Text WorkspaceFingerprint
mkWorkspaceFingerprint value
  | T.length value == 64 && T.all (\c -> c `elem` (['0' .. '9'] <> ['a' .. 'f'])) value = Right $ WorkspaceFingerprint value
  | otherwise = Left "Invalid workspace fingerprint"


instance AE.FromJSON WorkspaceFingerprint where
  parseJSON = AE.withText "Workspace fingerprint" $ either (fail . toString) pure . mkWorkspaceFingerprint


-- $setup
-- >>> import Data.Text qualified as T
-- >>> import Data.Aeson qualified as AE
-- >>> import Relude


newtype Revision = Revision Text
  deriving stock (Eq, Show)
  deriving newtype (AE.ToJSON, HI.EncodeValue)


-- | Both the valid round-trip and rejection belong to this boundary.
--
-- >>> fmap revisionText (mkRevision (T.replicate 40 "A")) == Right (T.replicate 40 "a")
-- True
-- >>> mkRevision "branch-name"
-- Left InvalidRevision
mkRevision :: Text -> Either Failure Revision
mkRevision text = if T.length text `elem` [40, 64] && T.all isHexDigit text then Right $ Revision $ T.toLower text else Left InvalidRevision


revisionText :: Revision -> Text
revisionText (Revision text) = text


instance AE.FromJSON Revision where
  parseJSON = AE.withText "Revision" $ either (fail . show) pure . mkRevision


data Candidate = Candidate {revision :: Revision, rootCause :: Text}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Step = Observe CheckReceipt | ProposeCandidate Candidate | RestartReproduction
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue) via Aeson Step


data Failure = InvalidRevision | InvalidCandidate | Unavailable | StaleVersion | StepConflict | ReproductionRequired | TestArtifactRequired | UnexpectedCheck | InvalidCheck | WrongRevision | WrongImage | WrongTest | WrongResources | WrongWorkspace | WrongComparison | InvalidHistory
  deriving stock (Eq, Show)


data FixRequest = FixRequest {request :: Request "issue_fix", issueId :: UUIDId "issue", imageBuildId :: UUIDId "guest_image"}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data TestArtifact = TestArtifact {script :: Text, testPatch :: Text, failureMessage :: Text}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data PhaseReceipt = PhaseReceipt {id :: UUIDId "issue_fix_step", attemptId :: UUIDId "agent_attempt", step :: Step}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data FixResult = FixResult
  { projectId :: UUIDId "project"
  , runId :: UUIDId "issue_fix"
  , issueId :: UUIDId "issue"
  , attemptId :: UUIDId "agent_attempt"
  , candidateId :: UUIDId "fix_candidate"
  , base :: Revision
  , head :: Revision
  , patchSha256 :: Text
  , bundleSha256 :: Text
  , rootCause :: Text
  , checkId :: UUIDId "regression_check"
  , regressionSha256 :: Text
  , summary :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data ArtifactRequest = ArtifactRequest {repository :: Text, result :: FixResult, baseline :: CheckReceipt, candidate :: CheckReceipt}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data FixReviewRequest = FixReviewRequest {fix :: FixRequest, artifacts :: ArtifactRequest}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data FixReviewDecision = ReviewApproved Text | ReviewChanges (NonEmpty Text) | ReviewBlocked (NonEmpty Text)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data FixReviewResult = FixReviewResult {result :: FixResult, decision :: FixReviewDecision}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data PricingStatus = Unpriced
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" PricingStatus


data Explanation = Explanation
  { version :: Word8
  , source :: Text
  , rendererSha256 :: Text
  , projectId :: UUIDId "project"
  , runId :: UUIDId "issue_fix"
  , issueId :: UUIDId "issue"
  , candidateId :: UUIDId "fix_candidate"
  , head :: Revision
  , checkId :: UUIDId "regression_check"
  , regressionSha256 :: Text
  , durationMs :: Word64
  , width :: Word16
  , height :: Word16
  , renderMs :: Word64
  , mediaBytes :: Word64
  , videoBytes :: Word64
  , resourceCostStatus :: PricingStatus
  , tools :: Map Text Text
  , files :: Map Text Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


validExplanation :: FixResult -> Explanation -> Bool
validExplanation result explanation =
  and
    [ (explanation.projectId, explanation.runId, explanation.issueId, explanation.candidateId, explanation.head, explanation.checkId) == (result.projectId, result.runId, result.issueId, result.candidateId, result.head, result.checkId)
    , explanation.regressionSha256 == result.regressionSha256
    , (explanation.version, explanation.durationMs, explanation.width, explanation.height) == (1, 36000, 1280, 720)
    , Map.keys explanation.files == sort ["explanation.mp4", "diagram.svg", "captions.vtt", "before.png", "cause.png", "change.png", "after.png"]
    , all (\hash -> T.length hash == 64 && T.all isHexDigit hash) (explanation.source : explanation.rendererSha256 : Map.elems explanation.files)
    , explanation.renderMs > 0 && explanation.videoBytes > 0 && explanation.videoBytes <= 32 * 1024 * 1024
    , explanation.mediaBytes >= explanation.videoBytes && explanation.mediaBytes <= 7 * 32 * 1024 * 1024
    , Map.keys explanation.tools == ["ffmpeg", "graphviz"] && all (\tool -> not (T.null $ T.strip tool) && T.length tool <= 256) explanation.tools
    ]


-- Admission requests contain installation credentials through the fix request.
data AdmissionRequest = AdmissionRequest {fix :: FixRequest, artifact :: TestArtifact}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data AdmissionDecision = Admitted Text | Rejected Text
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data AdmissionResult = AdmissionResult {artifact :: TestArtifact, decision :: AdmissionDecision}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data EvidenceKind = Telemetry | Monitor | Dashboard
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" EvidenceKind


data Observation = Observation {events :: Int64, errors :: Int64}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Evidence = Evidence
  { key :: Text
  , kind :: EvidenceKind
  , service :: Maybe Text
  , environment :: Maybe Text
  , url :: Text
  , query :: Maybe Text
  , observed :: Maybe Observation
  , chartUrl :: Maybe Text
  , queryResult :: Maybe QueryObservation
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data QueryKind = Aggregated | Sampled
  deriving stock (Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" QueryKind


data QueryObservation = QueryObservation {kind :: QueryKind, headers :: V.Vector Text, dataset :: V.Vector (V.Vector AE.Value), limitReached :: Bool, fieldsTruncated :: Bool}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data KnowledgeSnapshot = KnowledgeSnapshot {evidence :: [Evidence], knowledge :: [KnowledgeNote], windowStart :: UTCTime, windowEnd :: UTCTime}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
