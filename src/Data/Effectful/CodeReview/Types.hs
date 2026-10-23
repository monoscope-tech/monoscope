module Data.Effectful.CodeReview.Types (Evidence (..), EvidenceKind (..), Observation (..), QueryKind (..), QueryObservation (..), KnowledgeSnapshot (..), ThinkingLevel (..), Request (..), FixRequest (..), TestArtifact (..), AdmissionRequest (..), AdmissionResult (..), AdmissionDecision (..), PhaseReceipt (..), FixResult (..), ArtifactRequest (..), FixReviewRequest (..), FixReviewDecision (..), FixReviewResult (..), Explanation (..), Asset (..), assetFile, mediaFiles, validExplanation, PricingStatus (..), TelemetryQuery (..), KnowledgeNote (..), KnowledgeRequest (..), ModelUsage (..), ModelCall (..), AllocationReceipt, AllocationOutcome (..), allocationId, allocationAttempt, agentResources, hostAllocation, Regression (..), CheckReceipt (..), CheckStatus (..), RevisionSide (..), WorkspaceFingerprint, mkWorkspaceFingerprint, Revision, mkRevision, revisionText, Candidate (..), Step (..), Failure (..)) where

import Data.Aeson qualified as AE
import Data.Char (isHexDigit)
import Data.Effectful.AgentWorkspace (AllocationOutcome (..), AllocationReceipt, CheckReceipt (..), CheckStatus (..), Regression (..), Revision, RevisionSide (..), TestArtifact (..), WorkspaceFingerprint, agentResources, allocationAttempt, allocationId, hostAllocation, mkWorkspaceFingerprint, revisionText)
import Data.Effectful.AgentWorkspace qualified as Workspace
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
import Web.HttpApiData (FromHttpApiData)


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


-- | Normalize a pinned Git revision at the server boundary.
-- >>> fmap revisionText (mkRevision (T.replicate 40 "A")) == Right (T.replicate 40 "a")
-- True
-- >>> mkRevision "branch-name"
-- Left InvalidRevision
mkRevision :: Text -> Either Failure Revision
mkRevision = first (const InvalidRevision) . Workspace.mkRevision


instance HI.EncodeValue Revision where
  encodeValue = contramap revisionText (HI.encodeValue @Text)


-- $setup
-- >>> import Data.Text qualified as T
-- >>> import Relude


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


data Asset = Video | Diagram | Captions | Before | Cause | Change | After
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (AE.ToJSON, FromHttpApiData) via WrappedEnumSC 'Nothing "" Asset


assetFile :: Asset -> Text
assetFile = \case Video -> "explanation.mp4"; Diagram -> "diagram.svg"; Captions -> "captions.vtt"; Before -> "before.png"; Cause -> "cause.png"; Change -> "change.png"; After -> "after.png"


mediaFiles :: [Text]
mediaFiles = map assetFile [minBound .. maxBound]


validExplanation :: FixResult -> Explanation -> Bool
validExplanation result explanation =
  and
    [ (explanation.projectId, explanation.runId, explanation.issueId, explanation.candidateId, explanation.head, explanation.checkId) == (result.projectId, result.runId, result.issueId, result.candidateId, result.head, result.checkId)
    , explanation.regressionSha256 == result.regressionSha256
    , (explanation.version, explanation.durationMs, explanation.width, explanation.height) == (1, 36000, 1280, 720)
    , Map.keys explanation.files == sort mediaFiles
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
