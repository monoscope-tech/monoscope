-- | Durable state for autonomous onboarding instrumentation runs.
-- Each run tracks the Setup agent lifecycle from repository inspection through
-- ephemeral-key live ingestion verification and secret-free PR creation.
module Models.Projects.OnboardingInstrument (
  RunId,
  RunStatus (..),
  isTerminal,
  PhaseState (..),
  PhaseItem (..),
  SuggestedUrl (..),
  InstrumentRun (..),
  StartRefusal (..),
  startRefusal,
  initialPhases,
  createRun,
  getRun,
  latestRunForProject,
  updateRunProgress,
  completeRun,
  failRun,
) where

import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Models.Ids qualified as Projects
import Models.Projects.GitSync (RepositoryId)
import Pkg.DeriveUtils (UUIDId, WrappedEnumSC (..))
import Relude
import System.Types (DB)


type RunId = UUIDId "onboarding_instrument_run"


data RunStatus
  = RunQueued
  | RunCloning
  | RunDetecting
  | RunPatching
  | RunTesting
  | RunVerifying
  | RunOpeningPr
  | RunCompleted
  | RunFailed
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC 'Nothing "Run" RunStatus


isTerminal :: RunStatus -> Bool
isTerminal = \case
  RunCompleted -> True
  RunFailed -> True
  _ -> False


data PhaseState = PhasePending | PhaseRunning | PhaseDone | PhaseFailed
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "Phase" PhaseState


data PhaseItem = PhaseItem
  { key :: Text
  , label :: Text
  , state :: PhaseState
  , detail :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data SuggestedUrl = SuggestedUrl
  { url :: Text
  , name :: Text
  , reason :: Text
  , expectedStatus :: Int
  , statusCode :: Maybe Int
  , latencyMs :: Maybe Int
  , recommended :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data InstrumentRun = InstrumentRun
  { id :: RunId
  , projectId :: Projects.ProjectId
  , repositoryId :: Maybe RepositoryId
  , repoFullName :: Text
  , branch :: Text
  , targetKind :: Text
  , status :: RunStatus
  , detectedStack :: Text
  , phases :: Aeson [PhaseItem]
  , diffPreview :: Text
  , suggestedUrls :: Aeson [SuggestedUrl]
  , prUrl :: Maybe Text
  , prNumber :: Maybe Int32
  , verifiedEventAt :: Maybe UTCTime
  , errorSummary :: Maybe Text
  , errorLog :: Maybe Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


initialPhases :: Text -> [PhaseItem]
initialPhases repoName =
  [ PhaseItem "clone" ("Inspecting " <> repoName) PhaseRunning Nothing
  , PhaseItem "detect" "Detecting stack and telemetry targets" PhasePending Nothing
  , PhaseItem "patch" "Adding OpenTelemetry instrumentation" PhasePending Nothing
  , PhaseItem "verify" "Sending and verifying live test span in Monoscope" PhasePending Nothing
  , PhaseItem "pr" "Opening secret-free Pull Request" PhasePending Nothing
  ]


-- | Why a Setup agent run may not start. Unpaid projects (Free or still onboarding)
-- get one PR and 'unpaidDailyRuns' attempts a day; every plan runs one at a time.
data StartRefusal = RunInProgress | UnpaidPrUsed | UnpaidDailyLimit
  deriving stock (Bounded, Enum, Eq, Show)


-- | Decide from (paid, active run, PR'd runs, runs in the last day).
--
-- >>> map (startRefusal True) [(True, 0, 0), (False, 5, 9)]
-- [Just RunInProgress,Nothing]
-- >>> map (startRefusal False) [(False, 0, 0), (False, 0, 2), (False, 1, 0), (False, 0, 3), (True, 1, 3)]
-- [Nothing,Nothing,Just UnpaidPrUsed,Just UnpaidDailyLimit,Just RunInProgress]
startRefusal :: Bool -> (Bool, Int64, Int64) -> Maybe StartRefusal
startRefusal paid (active, prs, today)
  | active = Just RunInProgress
  | paid = Nothing
  | prs > 0 = Just UnpaidPrUsed
  | today >= unpaidDailyRuns = Just UnpaidDailyLimit
  | otherwise = Nothing
  where
    unpaidDailyRuns = 3


-- | Starts a run unless 'startRefusal' says otherwise. A run idle for 30 minutes counts as
-- dead so a crashed worker can't block the project; the guarded insert closes double-submits.
createRun :: DB es => Bool -> Projects.ProjectId -> Maybe RepositoryId -> Text -> Text -> Text -> Eff es (Either StartRefusal InstrumentRun)
createRun paid pid repoId repoFullName branch targetKind = do
  counts <-
    HI.getOneRow
      <$> Hasql.interp
        [HI.sql| SELECT COALESCE(bool_or(status NOT IN (#{RunCompleted}, #{RunFailed}) AND updated_at > NOW() - INTERVAL '30 minutes'), FALSE),
                        COUNT(*) FILTER (WHERE pr_url IS NOT NULL),
                        COUNT(*) FILTER (WHERE created_at > NOW() - INTERVAL '1 day')
                 FROM projects.onboarding_instrument_runs WHERE project_id = #{pid} |]
  case startRefusal paid counts of
    Just refusal -> pure $ Left refusal
    Nothing ->
      maybeToRight RunInProgress
        <$> Hasql.interp
          [HI.sql| INSERT INTO projects.onboarding_instrument_runs
                     (project_id, repository_id, repo_full_name, branch, target_kind, status, phases)
                   SELECT #{pid}, #{repoId}, #{repoFullName}, #{branch}, #{targetKind}, #{RunQueued}, #{HI.AsJsonb (initialPhases repoFullName)}
                   WHERE NOT EXISTS (SELECT 1 FROM projects.onboarding_instrument_runs
                                     WHERE project_id = #{pid} AND status NOT IN (#{RunCompleted}, #{RunFailed}) AND updated_at > NOW() - INTERVAL '30 minutes')
                   RETURNING id, project_id, repository_id, repo_full_name, branch, target_kind, status,
                             detected_stack, phases, diff_preview, suggested_urls, pr_url, pr_number,
                             verified_event_at, error_summary, error_log, created_at, updated_at |]


getRun :: DB es => Projects.ProjectId -> RunId -> Eff es (Maybe InstrumentRun)
getRun pid rid =
  Hasql.interp
    [HI.sql| SELECT id, project_id, repository_id, repo_full_name, branch, target_kind, status,
                    detected_stack, phases, diff_preview, suggested_urls, pr_url, pr_number,
                    verified_event_at, error_summary, error_log, created_at, updated_at
             FROM projects.onboarding_instrument_runs
             WHERE project_id = #{pid} AND id = #{rid} |]


latestRunForProject :: DB es => Projects.ProjectId -> Eff es (Maybe InstrumentRun)
latestRunForProject pid =
  Hasql.interp
    [HI.sql| SELECT id, project_id, repository_id, repo_full_name, branch, target_kind, status,
                    detected_stack, phases, diff_preview, suggested_urls, pr_url, pr_number,
                    verified_event_at, error_summary, error_log, created_at, updated_at
             FROM projects.onboarding_instrument_runs
             WHERE project_id = #{pid}
             ORDER BY created_at DESC
             LIMIT 1 |]


updateRunProgress :: DB es => RunId -> RunStatus -> Text -> [PhaseItem] -> Text -> [SuggestedUrl] -> Eff es ()
updateRunProgress rid status detectedStack phases diffPreview suggestedUrls =
  Hasql.interpExecute_
    [HI.sql| UPDATE projects.onboarding_instrument_runs
             SET status = #{status},
                 detected_stack = #{detectedStack},
                 phases = #{HI.AsJsonb phases},
                 diff_preview = #{diffPreview},
                 suggested_urls = #{HI.AsJsonb suggestedUrls},
                 updated_at = NOW()
             WHERE id = #{rid} |]


completeRun :: DB es => RunId -> [PhaseItem] -> Text -> [SuggestedUrl] -> Maybe Text -> Maybe Int32 -> Maybe UTCTime -> Eff es ()
completeRun rid phases diffPreview suggestedUrls prUrl prNumber verifiedAt =
  Hasql.interpExecute_
    [HI.sql| UPDATE projects.onboarding_instrument_runs
             SET status = #{RunCompleted},
                 phases = #{HI.AsJsonb phases},
                 diff_preview = #{diffPreview},
                 suggested_urls = #{HI.AsJsonb suggestedUrls},
                 pr_url = #{prUrl},
                 pr_number = #{prNumber},
                 verified_event_at = #{verifiedAt},
                 updated_at = NOW()
             WHERE id = #{rid} |]


failRun :: DB es => RunId -> [PhaseItem] -> Text -> Text -> Eff es ()
failRun rid phases summary logText =
  Hasql.interpExecute_
    [HI.sql| UPDATE projects.onboarding_instrument_runs
             SET status = #{RunFailed},
                 phases = #{HI.AsJsonb phases},
                 error_summary = #{summary},
                 error_log = #{logText},
                 updated_at = NOW()
             WHERE id = #{rid} |]
