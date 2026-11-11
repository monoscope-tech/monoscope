{-# LANGUAGE OrPatterns #-}

module Models.Projects.IssueFixes (FixId, StepId, Revision, mkRevision, revisionText, Setup (..), Candidate (..), Step (..), Phase (..), Progress, phase, verifiedRevision, Run (..), Failure (..), createRun, autofixCandidates, jobOwner, getRun, recordStep, queueJob, Lease, Checkpoint, checkpointContents, getCheckpoint, saveReviewedJob, claimJob, agentRunContext, currentJob, releaseJob, failJob, finishJob, completedResult, completedExplanation, CompletedFix, completedFixContents, publicationForUser, MediaViewer (..), mediaFor, fixMediaSignedPath, latestMediaForUser, validateResult, artifactRequest, recordJobStep) where

import BackgroundJobs.Types qualified as Jobs
import Data.Aeson qualified as AE
import Data.Char (isHexDigit)
import Data.Effectful.AgentArtifacts (RetainedFix, retainedFix)
import Data.Effectful.AgentArtifacts qualified as Artifacts
import Data.Effectful.CodeReview (ReviewedFix, explainedFixResult, fixExplanation, reviewedFixApproval, reviewedFixResult, validExplanation)
import Data.Effectful.CodeReview.Types (Candidate (..), Failure (..), Revision, Step (..), mkRevision, revisionText)
import Data.Effectful.CodeReview.Types qualified as Agent
import Data.Effectful.Hasql qualified as Hasql
import Data.Text qualified as T
import Data.Time (addUTCTime, nominalDay)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Effectful (Eff, (:>))
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import Models.Apis.Issues (IssueId)
import Models.Apis.Issues qualified as Issues
import Models.Projects.CodeContext (CodeMappingId)
import Models.Projects.GitSync (GitHubCredentialId)
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects (ProjectId)
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId)
import Relude
import System.Types (DB)
import Web.HttpApiData (toUrlPiece)


type FixId = UUIDId "issue_fix"
type StepId = UUIDId "issue_fix_step"


-- Only the leased persistence boundary returns a retained checkpoint.
data Checkpoint = Checkpoint Agent.FixResult Agent.KnowledgeSnapshot


checkpointContents :: Checkpoint -> (Agent.FixResult, Agent.KnowledgeSnapshot)
checkpointContents (Checkpoint result investigation) = (result, investigation)


data Setup = Setup {mappingId :: CodeMappingId, credentialId :: GitHubCredentialId, installationId :: Int64, owner :: Text, repo :: Text, service :: Text, base :: Revision, imageBuildId :: UUIDId "guest_image"}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue) via Aeson Setup


data Phase = Reproducing | Fixing | Verifying | Verified
  deriving stock (Eq, Show)


-- Constructors stay private: only replaying admitted observations creates proof.
newtype Reproduction = Reproduction Agent.CheckReceipt
  deriving stock (Show)


data Verification = AwaitingBase | AwaitingCandidate Reproduction
  deriving stock (Show)


data Progress
  = Collecting
  | Reproduced Reproduction
  | Checking Reproduction Candidate Verification
  | Checked Reproduction Candidate Reproduction Agent.CheckReceipt
  deriving stock (Show)


phase :: Progress -> Phase
phase = \case Collecting -> Reproducing; Reproduced{} -> Fixing; Checking{} -> Verifying; Checked{} -> Verified


verifiedRevision :: Progress -> Maybe Revision
verifiedRevision = \case Checked _ candidate _ _ -> Just candidate.revision; (Collecting; Reproduced{}; Checking{}) -> Nothing


-- The SQL boundary carries a history; traversal replaces it with admitted progress.
data Run progress = Run
  { id :: FixId
  , projectId :: ProjectId
  , issueId :: IssueId
  , setup :: Setup
  , billingProvider :: Projects.BillingProvider
  , customerId :: Maybe Text
  , version :: Int64
  , progress :: progress
  }
  deriving stock (Foldable, Functor, Generic, Show, Traversable)
  deriving anyclass (HI.DecodeRow)


advance :: Setup -> Progress -> Step -> Either Failure Progress
advance _ _ RestartReproduction = Right Collecting
advance setup progress (ProposeCandidate candidate) = do
  unless (candidate.revision /= setup.base && not (T.null $ T.strip candidate.rootCause) && T.length candidate.rootCause <= 1200) $ Left InvalidCandidate
  reproduction <- case progress of
    Collecting -> Left ReproductionRequired
    Reproduced repro -> Right repro
    Checking repro _ _ -> Right repro
    Checked repro _ _ _ -> Right repro
  pure $ Checking reproduction candidate AwaitingBase
advance setup progress (Observe check) = do
  unless (check.imageBuildId == setup.imageBuildId.toText) $ Left WrongImage
  unless (check.cpus > 0 && check.memoryMiB > 0 && digest check.scriptSha256 && digest check.logSha256) $ Left InvalidCheck
  unless (maybe True (digest . (.sha256)) check.regression) $ Left InvalidCheck
  case check.status of
    Agent.Passed -> unless (check.exitCode == Just 0) $ Left InvalidCheck
    Agent.Failed -> unless (maybe False (\code -> code > 0 && code `notElem` [126, 127]) check.exitCode && check.logBytes > 0) $ Left InvalidCheck
    (Agent.Blocked; Agent.Invalidated; Agent.TimedOut; Agent.OutputLimit; Agent.Cancelled; Agent.CleanupFailed) -> pass
  case progress of
    Collecting -> do
      expect Agent.Base setup.base
      when (check.status == Agent.Failed) $ case check.regression of
        Just Agent.InjectedTest{} -> pass
        Just Agent.RetainedTest{} -> pass
        (Nothing; Just Agent.ScriptCheck{}) -> Left TestArtifactRequired
      pure $ if check.status == Agent.Failed then Reproduced (Reproduction check) else Collecting
    Checking repro@(Reproduction frozen) candidate verification -> do
      unless (check.scriptSha256 == frozen.scriptSha256 && fmap (.sha256) check.regression == fmap (.sha256) frozen.regression) $ Left WrongTest
      unless (check.workspace == frozen.workspace) $ Left WrongWorkspace
      unless (check.cpus == frozen.cpus && check.memoryMiB == frozen.memoryMiB) $ Left WrongResources
      case check.side of
        Agent.Base -> do
          expect Agent.Base setup.base
          unless (check.checkId /= frozen.checkId && check.id /= frozen.id) $ Left WrongComparison
          pure case check.status of
            Agent.Failed -> Checking repro candidate $ AwaitingCandidate $ Reproduction check
            Agent.Passed -> Collecting
            (Agent.Blocked; Agent.Invalidated; Agent.TimedOut; Agent.OutputLimit; Agent.Cancelled; Agent.CleanupFailed) -> Checking repro candidate AwaitingBase
        Agent.Head -> do
          expect Agent.Head candidate.revision
          case verification of
            AwaitingBase -> Left UnexpectedCheck
            AwaitingCandidate baseline@(Reproduction baseCheck) -> do
              unless (check.checkId == baseCheck.checkId && check.attemptId == baseCheck.attemptId) $ Left WrongComparison
              when (check.status == Agent.Passed) $ case check.regression of
                Just Agent.RetainedTest{} -> pass
                (Nothing; Just Agent.ScriptCheck{}; Just Agent.InjectedTest{}) -> Left TestArtifactRequired
              pure case check.status of
                Agent.Passed -> Checked repro candidate baseline check
                Agent.Failed -> Reproduced repro
                (Agent.Blocked; Agent.Invalidated; Agent.TimedOut; Agent.OutputLimit; Agent.Cancelled; Agent.CleanupFailed) -> Checking repro candidate AwaitingBase
    (Reproduced{}; Checked{}) -> Left UnexpectedCheck
  where
    digest text = T.length text == 64 && T.all isHexDigit text
    expect side revision = do
      unless (check.side == side) $ Left UnexpectedCheck
      unless (T.toLower check.revision == revisionText revision) $ Left WrongRevision


-- | The credentials fix runs accept, as @c@: a github.com App installation.
appCredential :: HI.Sql
appCredential = [HI.sql|c.host = 'github' AND c.api_base IS NULL AND c.installation_id IS NOT NULL|]


createRun :: DB es => ProjectId -> FixId -> IssueId -> CodeMappingId -> Revision -> UUIDId "guest_image" -> Eff es (Maybe FixId)
createRun pid fid issue mapping base image =
  Hasql.interpOne
    [HI.sql|INSERT INTO projects.issue_fix_runs (id, project_id, issue_id, setup, billing_provider, customer_id)
    SELECT #{fid}, p.id, i.id, jsonb_build_object('mappingId', m.id, 'credentialId', c.id, 'installationId', c.installation_id, 'owner', m.owner, 'repo', m.repo,
      'service', m.service, 'base', #{base}, 'imageBuildId', #{image}), p.billing_provider, p.customer_id
    FROM projects.projects p JOIN apis.issues i ON i.project_id = p.id
    JOIN projects.code_mappings m ON m.project_id = p.id AND m.service = i.service
    JOIN projects.git_credentials c ON c.id = m.credential_id AND c.project_id = p.id
    WHERE p.id = #{pid} AND p.deleted_at IS NULL AND i.id = #{issue} AND i.archived_at IS NULL
      AND m.id = #{mapping} AND ^{appCredential}
    ON CONFLICT (id) DO UPDATE SET id = EXCLUDED.id
      WHERE issue_fix_runs.project_id = EXCLUDED.project_id AND issue_fix_runs.issue_id = EXCLUDED.issue_id
        AND issue_fix_runs.setup = EXCLUDED.setup
    RETURNING id|]


-- | Up to @n@ open issues from the last week that triage has had an hour with, each with a mapping
-- 'createRun' accepts and no fix run in the last day (a failed one is retried a day later); newest first.
autofixCandidates :: (DB es, Time :> es) => ProjectId -> Int -> Eff es [(IssueId, CodeMappingId)]
autofixCandidates pid n =
  Time.currentTime >>= \now ->
    Hasql.interp
      [HI.sql|SELECT DISTINCT ON (i.created_at, i.id) i.id, m.id FROM apis.issues i
    JOIN projects.code_mappings m ON m.project_id = i.project_id AND m.service = i.service
    JOIN projects.git_credentials c ON c.id = m.credential_id AND c.project_id = i.project_id
    WHERE i.project_id = #{pid} AND i.archived_at IS NULL AND i.spam_at IS NULL AND i.issue_type <> 'api_change'
      AND i.created_at BETWEEN #{addUTCTime (-7 * nominalDay) now} AND #{addUTCTime (-3600) now}
      AND ^{appCredential}
      AND NOT EXISTS (SELECT 1 FROM projects.issue_fix_runs f WHERE f.issue_id = i.id AND f.created_at > #{addUTCTime (-nominalDay) now})
    ORDER BY i.created_at DESC, i.id, length(m.path_prefix) DESC LIMIT #{n}|]


getRun :: DB es => ProjectId -> FixId -> Eff es (Either Failure (Maybe (Run Progress)))
getRun pid fid = do
  row <-
    Hasql.interpOne @(Run (V.Vector Step))
      [HI.sql|SELECT id, project_id, issue_id, setup, billing_provider, customer_id, version,
    ARRAY(SELECT step FROM projects.issue_fix_steps WHERE run_id = r.id ORDER BY version)
    FROM projects.issue_fix_runs r WHERE project_id = #{pid} AND id = #{fid}|]
  pure $ forM row \run -> do
    unless (run.version == fromIntegral (V.length run.progress)) $ Left InvalidHistory
    traverse (V.foldM (advance run.setup) Collecting) run


recordStep :: DB es => ProjectId -> FixId -> Int64 -> StepId -> Step -> Eff es (Either Failure (Run Progress))
recordStep = recordStepWith ProjectScope


recordJobStep :: DB es => Lease -> Int64 -> StepId -> Step -> Eff es (Either Failure (Run Progress))
recordJobStep (Lease pid fid owner) = recordStepWith (JobScope owner) pid fid


data StepScope = ProjectScope | JobScope (UUIDId "fix_lease")


recordStepWith :: DB es => StepScope -> ProjectId -> FixId -> Int64 -> StepId -> Step -> Eff es (Either Failure (Run Progress))
recordStepWith scope pid fid expected sid step = do
  let owner = case scope of ProjectScope -> Nothing; JobScope token -> Just token
      execution = case step of Observe check -> Just check.id.toText; (RestartReproduction; ProposeCandidate{}) -> Nothing
  previous <-
    Hasql.interp @[(FixId, Step)]
      [HI.sql|SELECT s.run_id, s.step FROM projects.issue_fix_steps s
    JOIN projects.issue_fix_runs r ON r.id = s.run_id WHERE r.project_id = #{pid}
      AND (s.id = #{sid} OR (s.step->>'tag' = 'Observe' AND s.step->'contents'->>'id' = #{execution}))|]
  loaded <- getRun pid fid
  case loaded >>= maybeToRight Unavailable of
    Left err -> pure $ Left err
    Right run -> case previous of
      stored@(_ : _) -> pure $ if all (\(storedRun, storedStep) -> storedRun == fid && AE.toJSON storedStep == AE.toJSON step) stored then Right run else Left StepConflict
      [] | run.version /= expected -> pure $ Left StaleVersion
      [] -> case advance run.setup run.progress step of
        Left err -> pure $ Left err
        Right _ -> do
          saved <-
            Hasql.interpOne @Bool
              [HI.sql|WITH leased AS (
              SELECT j.run_id FROM projects.issue_fix_jobs j JOIN projects.issue_fix_run_access a ON a.run_id = j.run_id AND a.user_id = j.requested_by
              WHERE j.run_id = #{fid} AND a.project_id = #{pid} AND j.lease_owner = #{owner} AND j.lease_until > now() AND j.result IS NULL
              FOR UPDATE OF j
            ), eligible AS (
              SELECT r.id FROM projects.issue_fix_runs r JOIN projects.projects p ON p.id = r.project_id
              JOIN apis.issues i ON i.id = r.issue_id AND i.project_id = p.id
              JOIN projects.code_mappings m ON m.id = (r.setup->>'mappingId')::uuid AND m.project_id = p.id
              JOIN projects.git_credentials c ON c.id = m.credential_id AND c.project_id = p.id
              WHERE r.id = #{fid} AND r.project_id = #{pid} AND r.version = #{expected} AND p.deleted_at IS NULL
                AND ((#{owner} IS NULL AND NOT EXISTS (SELECT 1 FROM projects.issue_fix_jobs j WHERE j.run_id = r.id)) OR r.id IN (SELECT run_id FROM leased))
                AND i.archived_at IS NULL AND i.service = r.setup->>'service' AND m.service = i.service
                AND m.owner = r.setup->>'owner' AND m.repo = r.setup->>'repo'
                AND c.id = (r.setup->>'credentialId')::uuid AND c.installation_id = (r.setup->>'installationId')::bigint
                AND ^{appCredential}
              FOR UPDATE OF r
            ), inserted AS (
              INSERT INTO projects.issue_fix_steps (id, run_id, version, step)
              SELECT #{sid}, id, #{expected} + 1, #{Aeson step} FROM eligible
              ON CONFLICT DO NOTHING RETURNING run_id
            ) UPDATE projects.issue_fix_runs SET version = version + 1
              WHERE id IN (SELECT run_id FROM inserted) RETURNING TRUE|]
          if saved == Just True then fmap (>>= maybeToRight Unavailable) (getRun pid fid) else pure $ Left StepConflict


-- | The member who queued a fix; publication acts with their access.
jobOwner :: DB es => ProjectId -> FixId -> Eff es (Maybe Projects.UserId)
jobOwner pid fid = Hasql.interpOne [HI.sql|SELECT j.requested_by FROM projects.issue_fix_jobs j JOIN projects.issue_fix_runs r ON r.id = j.run_id WHERE r.project_id = #{pid} AND j.run_id = #{fid}|]


queueJob :: DB es => ProjectId -> Projects.UserId -> FixId -> Eff es Bool
queueJob pid uid fid = Hasql.transaction TxS.ReadCommitted TxS.Write do
  HI.OneColumn subjectM <-
    Hasql.oneTx @(HI.OneColumn (Maybe Text))
      [HI.sql|SELECT (SELECT issue.title FROM projects.issue_fix_runs fix
      JOIN projects.issue_fix_run_access allowed ON allowed.run_id = fix.id
      JOIN apis.issues issue ON issue.id = fix.issue_id AND issue.project_id = fix.project_id
      WHERE fix.id = #{fid} AND allowed.project_id = #{pid} AND allowed.user_id = #{uid} FOR UPDATE OF fix)|]
  case subjectM of
    Nothing -> pure False
    Just subject -> do
      HI.OneColumn previous <-
        Hasql.oneTx @(HI.OneColumn (Maybe Projects.UserId))
          [HI.sql|SELECT (SELECT requested_by FROM projects.issue_fix_jobs WHERE run_id = #{fid})|]
      case previous of
        Just owner -> pure $ owner == uid
        Nothing -> do
          (identifier, conversation) <- Hasql.oneTx @(UUIDId "ai_routine_run", UUIDId "conversation") [HI.sql|SELECT gen_random_uuid(), gen_random_uuid()|]
          let title = T.take 100 $ "Fix: " <> subject
              input = "Fix reported issue: " <> subject
          Hasql.executeTx
            [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type, title, context)
            VALUES (#{pid}, #{conversation}, #{Issues.CTWeb}, #{title}, '{}'::jsonb)|]
          Hasql.executeTx
            [HI.sql|INSERT INTO apis.ai_routine_runs
            (id, root_run_id, project_id, conversation_id, requested_by, input, policy, source, scheduled_at, started_at, status, permission_ceiling)
            VALUES (#{identifier}, #{identifier}, #{pid}, #{conversation}, #{uid}, #{input}, #{Issues.Autonomous}, #{Issues.FixRun fid}, now(), now(), 'running', #{ProjectMembers.PAdmin})|]
          Hasql.executeTx [HI.sql|INSERT INTO projects.issue_fix_jobs (run_id, requested_by, agent_run_id) VALUES (#{fid}, #{uid}, #{identifier})|]
          Hasql.executeTx [HI.sql|INSERT INTO background_jobs_long (run_at, status, payload) VALUES (now(), 'queued', #{Aeson $ AE.toJSON $ Jobs.FixIssue pid fid})|]
          pure True


-- $setup
-- >>> import Data.Aeson.Types qualified as AET


-- | Only a successful claim constructs authority to mutate this job.
--
-- >>> AET.parseEither (AE.genericParseJSON AE.defaultOptions :: AE.Value -> AET.Parser Lease) AE.Null
-- ...
-- ...No instance for...Generic Lease...
-- ...
-- >>> HI.decodeRow @Lease `seq` ()
-- ...
-- ...No instance for...DecodeRow Lease...
-- ...
data Lease = Lease ProjectId FixId (UUIDId "fix_lease")
  deriving stock (Show)


claimJob :: DB es => ProjectId -> FixId -> Eff es (Maybe Lease)
claimJob pid fid = Hasql.transaction TxS.ReadCommitted TxS.Write do
  Issues.retireFixRunTx pid fid
  leased <-
    map (\(project, identifier, owner) -> Lease project identifier owner)
      <$> Hasql.queryTx @[(ProjectId, FixId, UUIDId "fix_lease")]
        [HI.sql|UPDATE projects.issue_fix_jobs job SET lease_owner = gen_random_uuid(), lease_until = now() + interval '10 minutes', failure = NULL, finished_at = NULL
      FROM projects.issue_fix_run_access allowed, apis.ai_routine_runs run
      WHERE job.run_id = #{fid} AND allowed.run_id = job.run_id AND allowed.project_id = #{pid} AND allowed.user_id = job.requested_by
        AND run.id = job.agent_run_id AND run.status IN ('running', 'failed', 'timed_out') AND run.policy_limit = #{Issues.Autonomous}
        AND EXISTS (SELECT 1 FROM apis.ai_conversations conversation WHERE conversation.project_id = run.project_id AND conversation.conversation_id = run.conversation_id)
        AND job.result IS NULL AND (job.lease_until IS NULL OR job.lease_until < now())
      RETURNING allowed.project_id, job.run_id, job.lease_owner|]
  for_ leased $ \(Lease _ _ owner) ->
    Hasql.executeTx
      [HI.sql|UPDATE apis.ai_routine_runs run SET status = 'running', started_at = clock_timestamp(), finished_at = NULL, error = NULL
      FROM projects.issue_fix_jobs job WHERE job.agent_run_id = run.id AND job.run_id = #{fid} AND job.lease_owner = #{owner}|]
  pure $ listToMaybe leased


agentRunContext :: DB es => Lease -> Eff es (Maybe Issues.RunContext)
agentRunContext (Lease pid fid owner) = do
  identifier <- Hasql.interpOne @(UUIDId "ai_routine_run") [HI.sql|SELECT agent_run_id FROM projects.issue_fix_jobs WHERE run_id = #{fid} AND lease_owner = #{owner}|]
  maybe (pure Nothing) (\run -> Issues.runContextFor pid $ Issues.FixClaim fid run owner) identifier


currentJob :: (DB es, Time :> es) => Lease -> Eff es Bool
currentJob lease@(Lease _ fid owner) = do
  active <-
    fromMaybe False
      <$> Hasql.interpOne @Bool
        [HI.sql|SELECT EXISTS (SELECT 1 FROM projects.issue_fix_jobs
    WHERE run_id = #{fid} AND lease_owner = #{owner} AND lease_until > now() AND result IS NULL)|]
  if not active
    then pure False
    else
      agentRunContext lease >>= \case
        Nothing -> pure False
        Just context ->
          Issues.runContextAuthority context >>= \case
            Just _ -> pure True
            Nothing -> Issues.completeRun context (Issues.CancelledResult Nothing) $> False


releaseJob :: DB es => Lease -> Eff es ()
releaseJob (Lease _ fid owner) =
  Hasql.interpExecute_
    [HI.sql| UPDATE projects.issue_fix_jobs SET lease_owner = NULL, lease_until = NULL WHERE run_id = #{fid} AND lease_owner = #{owner} |]


failJob :: (DB es, Time :> es) => Lease -> Text -> Eff es ()
failJob lease@(Lease _ fid owner) message = do
  now <- Time.currentTime
  context <- agentRunContext lease
  Hasql.transaction TxS.ReadCommitted TxS.Write do
    Hasql.executeTx
      [HI.sql|UPDATE projects.issue_fix_jobs SET failure = #{message}, finished_at = now()
      WHERE run_id = #{fid} AND lease_owner = #{owner} AND result IS NULL|]
    for_ context $ \run -> void $ Issues.completeRunTx now Nothing run $ Issues.FailedResult $ Issues.RunFailure message Issues.RunOnly


completedResult :: DB es => ProjectId -> FixId -> Eff es (Maybe Agent.FixResult)
completedResult pid fid =
  Hasql.interpOneJson
    [HI.sql| SELECT j.result FROM projects.issue_fix_jobs j JOIN projects.issue_fix_runs r ON r.id = j.run_id
    WHERE r.project_id = #{pid} AND r.id = #{fid} AND j.result IS NOT NULL |]


completedExplanation :: DB es => ProjectId -> FixId -> Eff es (Maybe Agent.Explanation)
completedExplanation pid fid =
  Hasql.interpOneJson
    [HI.sql| SELECT j.explanation FROM projects.issue_fix_jobs j JOIN projects.issue_fix_runs r ON r.id = j.run_id
    WHERE r.project_id = #{pid} AND r.id = #{fid} AND j.explanation IS NOT NULL AND j.media_retained_at IS NOT NULL |]


-- Only current authorized reads of a completed, retained result issue this proof.
data CompletedFix = CompletedFix Agent.ArtifactRequest Agent.Explanation


completedFixContents :: CompletedFix -> (Agent.ArtifactRequest, Agent.Explanation)
completedFixContents (CompletedFix artifacts explanation) = (artifacts, explanation)


publicationForUser :: DB es => ProjectId -> FixId -> Projects.UserId -> Eff es (Maybe CompletedFix)
publicationForUser pid fid user = do
  saved <- mediaFor pid fid (Member user)
  case saved of
    Nothing -> pure Nothing
    Just (result, explanation) -> do
      approved <-
        Hasql.interpOne @Bool
          [HI.sql| SELECT EXISTS (SELECT 1 FROM projects.issue_fix_jobs j JOIN projects.pr_review_attempts t ON t.id = j.review_attempt_id
          WHERE j.run_id = #{fid} AND t.fix_run_id = j.run_id AND t.project_id = #{pid}
            AND j.review_rationale IS NOT NULL AND j.verified_result = #{Aeson result} AND j.result = #{Aeson result}) |]
      current <- getRun pid fid
      pure $ case current of
        Right (Just run) | approved == Just True && validExplanation result explanation -> do
          artifacts <- artifactRequest ("https://github.com/" <> run.setup.owner <> "/" <> run.setup.repo <> ".git") run result
          pure $ CompletedFix artifacts explanation
        _ -> Nothing


-- | Who reads retained fix media: an eligible member, or GitHub through a link 'fixMediaSignedPath' signed.
data MediaViewer = Member Projects.UserId | SignedLink


mediaFor :: DB es => ProjectId -> FixId -> MediaViewer -> Eff es (Maybe (Agent.FixResult, Agent.Explanation))
mediaFor pid fid viewer = do
  row <-
    Hasql.interpOne @(Aeson Agent.FixResult, Aeson Agent.Explanation)
      $ [HI.sql| SELECT j.result, j.explanation FROM projects.issue_fix_jobs j JOIN projects.issue_fix_runs r ON r.id = j.run_id
    WHERE r.project_id = #{pid} AND j.run_id = #{fid}
      AND j.result IS NOT NULL AND j.explanation IS NOT NULL AND j.media_retained_at IS NOT NULL |]
      <> case viewer of
        Member user -> [HI.sql| AND EXISTS (SELECT 1 FROM projects.issue_fix_run_access a WHERE a.run_id = j.run_id AND a.project_id = #{pid} AND a.user_id = #{user}) |]
        SignedLink -> mempty
  pure $ (\(Aeson result, Aeson explanation) -> (result, explanation)) <$> row


fixMediaSignedPath :: FixId -> Artifacts.Asset -> Text
fixMediaSignedPath fid asset = "fix:" <> fid.toText <> "/" <> toUrlPiece asset


latestMediaForUser :: DB es => ProjectId -> Projects.UserId -> Eff es [Agent.FixResult]
latestMediaForUser pid user = do
  rows <-
    Hasql.interp @[HI.OneColumn (Aeson Agent.FixResult)]
      [HI.sql| SELECT j.result FROM projects.issue_fix_jobs j
    JOIN projects.issue_fix_run_access a ON a.run_id = j.run_id
    WHERE a.project_id = #{pid} AND a.user_id = #{user} AND j.result IS NOT NULL AND j.media_retained_at IS NOT NULL
    ORDER BY j.finished_at DESC, j.run_id LIMIT 20 |]
  pure [result | HI.OneColumn (Aeson result) <- rows]


validateResult :: Run Progress -> Agent.FixResult -> Bool
validateResult run result = case run.progress of
  Checked _ candidate _ check ->
    (result.projectId, result.runId, result.issueId)
      == (run.projectId, run.id, run.issueId)
      && result.base
      == run.setup.base
      && Candidate result.head result.rootCause
      == candidate
      && (result.attemptId, result.checkId)
      == (check.attemptId, check.checkId)
      && fmap (.sha256) check.regression
      == Just result.regressionSha256
      && all (\hash -> T.length hash == 64 && T.all isHexDigit hash) [result.patchSha256, result.bundleSha256]
      && T.length result.rootCause
      <= 240
      && not (T.null $ T.strip result.summary)
      && T.length result.summary
      <= 180
  (Collecting; Reproduced{}; Checking{}) -> False


artifactRequest :: Text -> Run Progress -> Agent.FixResult -> Maybe Agent.ArtifactRequest
artifactRequest repository run result = case run.progress of
  Checked _ _ (Reproduction baseline) candidate | validateResult run result -> Just $ Agent.ArtifactRequest repository result baseline candidate
  (Collecting; Reproduced{}; Checking{}; Checked{}) -> Nothing


getCheckpoint :: DB es => Lease -> Eff es (Maybe Checkpoint)
getCheckpoint (Lease pid fid owner) = do
  row <-
    Hasql.interpOne @(Aeson Agent.FixResult, Aeson Agent.KnowledgeSnapshot)
      [HI.sql| SELECT j.verified_result, j.checkpoint_investigation
    FROM projects.issue_fix_jobs j JOIN projects.issue_fix_run_access a ON a.run_id = j.run_id AND a.user_id = j.requested_by
    WHERE j.run_id = #{fid} AND a.project_id = #{pid} AND j.lease_owner = #{owner} AND j.lease_until > now()
      AND j.result IS NULL AND j.verified_result IS NOT NULL
      AND j.review_rationale IS NOT NULL AND EXISTS (SELECT 1 FROM projects.pr_review_attempts t WHERE t.id = j.review_attempt_id AND t.fix_run_id = j.run_id AND t.project_id = a.project_id) |]
  pure $ (\(Aeson result, Aeson investigation) -> Checkpoint result investigation) <$> row


saveReviewedJob :: DB es => Lease -> ReviewedFix -> Agent.KnowledgeSnapshot -> Eff es (Maybe Checkpoint)
saveReviewedJob lease@(Lease pid fid owner) verified snapshot = do
  let result = reviewedFixResult verified
      (reviewAttempt, rationale) = reviewedFixApproval verified
  loaded <- getRun pid fid
  case loaded of
    Right (Just run) | validateResult run result -> do
      saved <-
        Hasql.interpOne @Bool
          [HI.sql| WITH pinned AS (
          SELECT j.run_id FROM projects.issue_fix_jobs j JOIN projects.issue_fix_runs r ON r.id = j.run_id
          JOIN projects.issue_fix_run_access a ON a.run_id = j.run_id AND a.user_id = j.requested_by
          WHERE j.run_id = #{fid} AND a.project_id = #{pid} AND r.version = #{run.version}
            AND j.lease_owner = #{owner} AND j.lease_until > now() AND j.result IS NULL
            AND EXISTS (SELECT 1 FROM projects.pr_review_attempts t WHERE t.id = #{reviewAttempt} AND t.fix_run_id = j.run_id AND t.project_id = a.project_id)
            AND (j.review_attempt_id IS NULL OR (j.verified_result = #{Aeson result} AND j.checkpoint_investigation = #{Aeson snapshot} AND j.review_attempt_id = #{reviewAttempt} AND j.review_rationale = #{rationale}))
          FOR UPDATE OF j, r
        ), saved AS (
          UPDATE projects.issue_fix_jobs j SET verified_result = #{Aeson result}, checkpoint_investigation = #{Aeson snapshot}, review_attempt_id = #{reviewAttempt}, review_rationale = #{rationale}
          WHERE j.run_id IN (SELECT run_id FROM pinned) RETURNING j.run_id
        ) SELECT EXISTS (SELECT 1 FROM saved) |]
      if saved == Just True then getCheckpoint lease else pure Nothing
    _ -> pure Nothing


finishJob :: (DB es, Time :> es) => Lease -> RetainedFix -> Checkpoint -> Eff es Bool
finishJob lease@(Lease pid fid owner) verified (Checkpoint expected snapshot) = do
  let result = explainedFixResult $ retainedFix verified
      explanation = fixExplanation $ retainedFix verified
  loaded <- getRun pid fid
  context <- agentRunContext lease
  now <- Time.currentTime
  case (loaded, context) of
    (Right (Just run), Just authority)
      | validateResult run result && result == expected -> do
          let persist =
                Hasql.oneTx @Bool
                  [HI.sql| WITH pinned AS (
        SELECT j.run_id FROM projects.issue_fix_jobs j JOIN projects.issue_fix_runs r ON r.id = j.run_id
        JOIN projects.issue_fix_run_access a ON a.run_id = j.run_id AND a.user_id = j.requested_by
        WHERE j.run_id = #{fid} AND a.project_id = #{pid} AND r.version = #{run.version}
          AND j.lease_owner = #{owner} AND j.lease_until > now() AND j.result IS NULL
          AND j.verified_result = #{Aeson result} AND j.checkpoint_investigation = #{Aeson snapshot}
          AND j.review_rationale IS NOT NULL AND EXISTS (SELECT 1 FROM projects.pr_review_attempts t WHERE t.id = j.review_attempt_id AND t.fix_run_id = j.run_id AND t.project_id = a.project_id) FOR UPDATE OF j, r
      ), finished AS (
        UPDATE projects.issue_fix_jobs j SET result = #{Aeson result}, explanation = #{Aeson explanation}, media_retained_at = now(), investigation = #{Aeson snapshot}, lease_owner = NULL, lease_until = NULL, failure = NULL, finished_at = now()
        WHERE j.run_id IN (SELECT run_id FROM pinned) RETURNING j.run_id
      ), remembered AS (
        INSERT INTO projects.pr_review_knowledge (fix_run_id, project_id, owner, repo, service, revision, summary, evidence_keys, window_start, window_end, evidence)
        SELECT f.run_id, r.project_id, r.setup->>'owner', r.setup->>'repo', note->>'service', #{revisionText result.head}, note->>'summary',
          ARRAY(SELECT jsonb_array_elements_text(note->'evidenceKeys')), #{snapshot.windowStart}, #{snapshot.windowEnd},
          coalesce((SELECT jsonb_agg(e #- '{queryResult,dataset}') FROM jsonb_array_elements(#{Aeson snapshot.evidence}::jsonb) e
            WHERE e->>'key' IN (SELECT jsonb_array_elements_text(note->'evidenceKeys'))), '[]'::jsonb)
        FROM finished f JOIN projects.issue_fix_runs r ON r.id = f.run_id,
          LATERAL jsonb_array_elements(#{Aeson snapshot.knowledge}::jsonb) note
      ) SELECT EXISTS (SELECT 1 FROM finished) |]
          Hasql.transaction TxS.ReadCommitted TxS.Write do
            completed <-
              Issues.completeRunTx now Nothing authority
                $ Issues.CompletedResult Issues.ResultSucceeded Issues.RetainAnswer
                $ Issues.reportAnswer "The verified fix is retained. Administrators can review its artifacts." Nothing
            case completed of
              Issues.RunCompleted Issues.CompletedSucceeded _ -> do
                saved <- persist
                unless saved Tx.condemn
                pure saved
              (Issues.CompletionRejected; Issues.RunCompleted{}) -> pure False
    _ -> pure False
