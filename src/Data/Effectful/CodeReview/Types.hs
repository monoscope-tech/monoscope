module Data.Effectful.CodeReview.Types (AgentAwaitingReview (..), ReviewInput (..), ReviewMemory (..), ReviewContext (..), FixContext (..), Evidence (..), EvidenceKind (..), Observation (..), QueryKind (..), QueryObservation (..), KnowledgeSnapshot (..), ThinkingLevel (..), ReviewMode (..), DraftSubmission (..), Request (..), FixRequest (..), TestArtifact (..), AdmissionRequest (..), AdmissionResult (..), AdmissionDecision (..), PhaseReceipt (..), FixResult (..), ArtifactRequest (..), FixReviewRequest (..), FixReviewDecision (..), FixReviewResult (..), Explanation (..), validExplanation, PricingStatus (..), TelemetryQuery (..), KnowledgeNote (..), KnowledgeRequest (..), ModelUsage (..), ModelCall (..), AllocationReceipt, allocationId, allocationAttempt, Regression (..), CheckReceipt (..), CheckStatus (..), RevisionSide (..), ProofMedia (..), WorkspaceFingerprint, mkWorkspaceFingerprint, Revision, mkRevision, revisionText, Candidate (..), Step (..), Failure (..)) where

import Data.Aeson qualified as AE
import Data.Char (isHexDigit)
import Data.Effectful.AgentWorkspace (AllocationReceipt, TestArtifact (..), allocationAttempt, allocationId)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Hasql.Interpolate qualified as HI
import Models.Projects.ProjectMemory qualified as Memory
import OpenAI.V1.Models (Model)
import Pkg.DeriveUtils (UUIDId, WrappedEnumSC (..))
import Pkg.Git qualified as Git
import Relude
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)


data ThinkingLevel = Off | Minimal | Low | Medium | High
  deriving stock (Generic, Read, Show)
  deriving (AE.ToJSON) via WrappedEnumSC 'Nothing "" ThinkingLevel


data ReviewMode = BoundedPatch | CheckoutHarness
  deriving stock (Eq, Generic, Show)
  deriving (AE.ToJSON) via WrappedEnumSC 'Nothing "" ReviewMode


data DraftSubmission = Structured AE.Value | ProviderText Text
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


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
  deriving anyclass (AE.FromJSON, AE.FromJSONKey, AE.ToJSON, AE.ToJSONKey)


data KnowledgeNote = KnowledgeNote {service :: Text, summary :: Text, evidenceKeys :: NonEmpty Text}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data ReviewInput = ReviewInput {base :: Revision, head :: Revision, provider :: Text, model :: Text, files :: [Git.PullRequestFile], evidence :: [Evidence], coverage :: [Text], windowStart :: UTCTime, windowEnd :: UTCTime, prompt :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data ReviewMemory = ReviewMemory {evidence :: Map TelemetryQuery Evidence, notes :: [KnowledgeNote]}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data ReviewContext = ReviewContext {input :: ReviewInput, memory :: ReviewMemory}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data FixContext = FixContext {attempt :: UUIDId "agent_attempt", issue :: Text, context :: ReviewContext}
  deriving stock (Generic)
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


data RevisionSide = Base | Head
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, ToHttpApiData) via WrappedEnumSC 'Nothing "" RevisionSide


-- | One side of a recorded proof: the claim's command replayed under asciinema in a fresh VM,
-- rendered on the host as an animated preview and a video.
data ProofMedia = ProofMedia {claim :: Text, side :: RevisionSide, revision :: Text, exitCode :: Maybe Int, cast :: ByteString, gif :: ByteString, mp4 :: ByteString}


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


-- | Admission decisions preserve their wire format and carry a bounded rationale.
--
-- >>> let wire = "{\"tag\":\"Admitted\",\"contents\":\"Relevant regression\"}"
-- >>> fmap AE.toJSON (AE.eitherDecode @AdmissionDecision wire) == AE.eitherDecode @AE.Value wire
-- True
-- >>> let decode :: Text -> Either String AdmissionDecision; decode value = AE.eitherDecode (AE.encode $ AE.object ["tag" AE..= ("Rejected" :: Text), "contents" AE..= value])
-- >>> map (isLeft . decode) ["", " ", T.replicate 1201 "x", "bad\NULrationale"]
-- [True,True,True,True]
-- >>> isRight $ decode $ T.replicate 1200 "x"
-- True
data AdmissionDecision = Admitted (Memory.MemoryText 1200) | Rejected (Memory.MemoryText 1200)
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


data AgentAwaitingReview = AgentAwaitingReview (UUIDId "agent_operation") UTCTime
  deriving stock (Show)
  deriving anyclass (Exception)
