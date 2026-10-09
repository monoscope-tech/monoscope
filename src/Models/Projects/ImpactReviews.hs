module Models.Projects.ImpactReviews (
  ReviewState (..),
  ReviewId,
  ReviewRun (..),
  ReviewSettings (..),
  PullRequestEvent (..),
  receiveEvent,
  getRun,
  claimRun,
  currentRun,
  saveResult,
  finishRun,
  releaseRun,
  repositorySettings,
  updateSettings,
  latestRuns,
  retryRun,
  supersedeRun,
) where

import BackgroundJobs.Types qualified as Jobs
import Data.Aeson qualified as AE
import Data.Char (isHexDigit)
import Data.Effectful.Hasql qualified as Hasql
import Data.Text qualified as T
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects (ProjectId)
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Relude
import System.Types (DB)
import Web.FormUrlEncoded (FromForm)


data ReviewState = Queued | Reviewing | Completed | Incomplete | Superseded
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" ReviewState


type ReviewId = UUIDId "pr_review"


data ReviewRun = ReviewRun
  { id :: ReviewId
  , threadId :: UUIDId "pr_review_thread"
  , projectId :: ProjectId
  , owner :: Text
  , repo :: Text
  , number :: Int
  , revision :: Text
  , state :: ReviewState
  , result :: Maybe AE.Value
  , error :: Maybe Text
  , commentId :: Maybe Int64
  , latestRevision :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


data ReviewSettings = ReviewSettings {owner :: Text, repo :: Text, enabled :: Bool, includeEvidence :: Bool}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm, HI.DecodeRow)


-- Only ready, open PR revisions are reviewable; closure/draft deliveries still
-- advance the thread so they invalidate an in-flight publication.
data PullRequestEvent = PullRequestEvent
  { owner :: Text
  , repo :: Text
  , installationId :: Int64
  , number :: Int
  , revision :: Text
  , updatedAt :: UTCTime
  , reviewable :: Bool
  }
  deriving stock (Generic, Show)


instance AE.FromJSON PullRequestEvent where
  parseJSON = AE.withObject "PullRequestEvent" \o -> do
    action <- o AE..: "action"
    unless (action `elem` (["opened", "synchronize", "reopened", "ready_for_review", "closed", "converted_to_draft"] :: [Text])) $ fail "Unsupported PR action"
    fullName <- o AE..: "repository" >>= (AE..: "full_name")
    (owner, repo) <- case T.splitOn "/" fullName of
      [owner, repo] | not (T.null owner || T.null repo) -> pure (T.toLower owner, T.toLower repo)
      _ -> fail "Invalid repository"
    installationId <- o AE..: "installation" >>= (AE..: "id")
    number <- o AE..: "number"
    unless (number > 0) $ fail "Invalid PR number"
    pr <- o AE..: "pull_request"
    revision <- pr AE..: "head" >>= (AE..: "sha")
    unless (T.length revision == 40 && T.all isHexDigit revision) $ fail "Invalid head revision"
    updatedAt <- pr AE..: "updated_at"
    prState <- pr AE..: "state"
    draft <- pr AE..: "draft"
    pure PullRequestEvent{owner, repo, installationId, number, revision = T.toLower revision, updatedAt, reviewable = prState == ("open" :: Text) && not draft}


-- | Grant resolution, receipt, and odd-job insertion are one statement. An
-- out-of-order delivery cannot replace a newer head, and duplicates queue once.
receiveEvent :: DB es => PullRequestEvent -> Eff es ()
receiveEvent event =
  Hasql.interpExecute_
    [HI.sql|WITH eligible AS (
    SELECT DISTINCT m.project_id FROM projects.code_mappings m
    JOIN projects.git_credentials c ON c.id = m.credential_id AND c.project_id = m.project_id
    LEFT JOIN projects.pr_review_settings s ON s.project_id = m.project_id AND s.owner = lower(m.owner) AND s.repo = lower(m.repo)
    WHERE c.host = 'github' AND c.api_base IS NULL AND c.installation_id = #{event.installationId}
      AND lower(m.owner) = #{event.owner} AND lower(m.repo) = #{event.repo} AND coalesce(s.enabled, true)
  ), threads AS (
    INSERT INTO projects.pr_review_threads (project_id, owner, repo, number, latest_revision, event_at)
    SELECT project_id, #{event.owner}, #{event.repo}, #{event.number}, #{event.revision}, #{event.updatedAt} FROM eligible
    ON CONFLICT (project_id, owner, repo, number) DO UPDATE
      SET latest_revision = EXCLUDED.latest_revision, event_at = EXCLUDED.event_at
      WHERE projects.pr_review_threads.event_at <= EXCLUDED.event_at
    RETURNING id
  ), runs AS (
    INSERT INTO projects.pr_review_runs (thread_id, revision, state)
    SELECT id, #{event.revision}, CASE WHEN #{event.reviewable} THEN 'queued' ELSE 'superseded' END FROM threads
    ON CONFLICT (thread_id, revision) DO UPDATE SET state = CASE
      WHEN NOT #{event.reviewable} THEN 'superseded'
      WHEN projects.pr_review_runs.state = 'superseded' THEN 'queued'
      ELSE projects.pr_review_runs.state END
    WHERE NOT #{event.reviewable} OR projects.pr_review_runs.state = 'superseded'
    RETURNING id, state
  ) INSERT INTO background_jobs (run_at, status, payload)
    SELECT now(), 'queued', jsonb_build_object('tag', 'ReviewPullRequest', 'contents', id)
    FROM runs WHERE state = 'queued'|]


getRun :: DB es => ReviewId -> Eff es (Maybe ReviewRun)
getRun rid =
  Hasql.interpOneJson
    [HI.sql|SELECT jsonb_build_object('id', r.id, 'threadId', t.id, 'projectId', t.project_id,
    'owner', t.owner, 'repo', t.repo, 'number', t.number, 'revision', r.revision,
    'state', r.state, 'result', r.result, 'error', r.error, 'commentId', t.comment_id, 'latestRevision', t.latest_revision)
    FROM projects.pr_review_runs r JOIN projects.pr_review_threads t ON t.id = r.thread_id WHERE r.id = #{rid}|]


claimRun :: DB es => ReviewRun -> Eff es Bool
claimRun run =
  (> 0)
    <$> Hasql.interpExecute
      [HI.sql|WITH leased AS (
    UPDATE projects.pr_review_threads t SET lease_owner = #{run.id}, lease_until = now() + interval '10 minutes'
    WHERE t.id = #{run.threadId} AND t.latest_revision = #{run.revision}
      AND (t.lease_until IS NULL OR t.lease_until < now())
      AND EXISTS (SELECT 1 FROM projects.pr_review_runs r WHERE r.id = #{run.id} AND r.state IN ('queued', 'reviewing', 'incomplete'))
    RETURNING t.id
  ) UPDATE projects.pr_review_runs SET state = 'reviewing', error = NULL
    WHERE id = #{run.id} AND thread_id IN (SELECT id FROM leased)|]


currentRun :: DB es => ReviewRun -> Eff es Bool
currentRun run =
  fromMaybe False
    <$> Hasql.interpOne
      [HI.sql|SELECT t.latest_revision = r.revision AND t.lease_owner = r.id AND t.lease_until > now()
    AND r.state = 'reviewing'
    AND coalesce(s.enabled, true)
    AND EXISTS (SELECT 1 FROM projects.code_mappings m JOIN projects.git_credentials c ON c.id = m.credential_id AND c.project_id = m.project_id
      WHERE m.project_id = t.project_id AND lower(m.owner) = t.owner AND lower(m.repo) = t.repo AND c.host = 'github' AND c.api_base IS NULL AND c.installation_id IS NOT NULL)
    FROM projects.pr_review_runs r JOIN projects.pr_review_threads t ON t.id = r.thread_id
    LEFT JOIN projects.pr_review_settings s ON s.project_id = t.project_id AND s.owner = t.owner AND s.repo = t.repo
    WHERE r.id = #{run.id}|]


saveResult :: (AE.ToJSON a, DB es) => ReviewRun -> a -> Eff es ()
saveResult run result = Hasql.interpExecute_ [HI.sql|UPDATE projects.pr_review_runs SET result = #{Aeson $ AE.toJSON result} WHERE id = #{run.id}|]


finishRun :: DB es => ReviewRun -> Maybe Int64 -> Eff es ()
finishRun run commentId =
  Hasql.interpExecute_
    [HI.sql|WITH finished AS (UPDATE projects.pr_review_runs SET state = 'completed', finished_at = now()
    WHERE id = #{run.id} AND state = 'reviewing' RETURNING thread_id)
    UPDATE projects.pr_review_threads SET comment_id = coalesce(#{commentId}, comment_id)
    WHERE id IN (SELECT thread_id FROM finished) AND lease_owner = #{run.id}|]


releaseRun :: DB es => ReviewRun -> Maybe Text -> Eff es ()
releaseRun run err =
  Hasql.interpExecute_
    [HI.sql|WITH released AS (UPDATE projects.pr_review_threads SET lease_owner = NULL, lease_until = NULL
    WHERE id = #{run.threadId} AND lease_owner = #{run.id} RETURNING latest_revision)
    UPDATE projects.pr_review_runs SET state = CASE
      WHEN revision <> (SELECT latest_revision FROM released) THEN 'superseded'
      WHEN #{err}::text IS NOT NULL OR state = 'reviewing' THEN 'incomplete' ELSE state END,
      error = coalesce(#{err}, CASE WHEN state = 'reviewing' THEN 'Review interrupted before publication; rerun when ready' ELSE error END)
    WHERE id = #{run.id} AND EXISTS (SELECT 1 FROM released)|]


repositorySettings :: DB es => ProjectId -> Eff es [ReviewSettings]
repositorySettings pid =
  Hasql.interp
    [HI.sql|SELECT DISTINCT lower(m.owner), lower(m.repo), coalesce(s.enabled, true), coalesce(s.include_evidence, true)
    FROM projects.code_mappings m JOIN projects.git_credentials c ON c.id = m.credential_id AND c.project_id = m.project_id
    LEFT JOIN projects.pr_review_settings s ON s.project_id = m.project_id AND s.owner = lower(m.owner) AND s.repo = lower(m.repo)
    WHERE m.project_id = #{pid} AND c.host = 'github' AND c.api_base IS NULL AND c.installation_id IS NOT NULL
    ORDER BY lower(m.owner), lower(m.repo)|]


updateSettings :: DB es => ProjectId -> ReviewSettings -> Eff es ()
updateSettings pid settings =
  Hasql.interpExecute_
    [HI.sql|INSERT INTO projects.pr_review_settings (project_id, owner, repo, enabled, include_evidence)
    SELECT #{pid}, #{settings.owner}, #{settings.repo}, #{settings.enabled}, #{settings.includeEvidence}
    WHERE EXISTS (SELECT 1 FROM projects.code_mappings WHERE project_id = #{pid} AND lower(owner) = #{settings.owner} AND lower(repo) = #{settings.repo})
    ON CONFLICT (project_id, owner, repo) DO UPDATE SET enabled = EXCLUDED.enabled, include_evidence = EXCLUDED.include_evidence|]


latestRuns :: DB es => ProjectId -> Eff es [ReviewRun]
latestRuns pid =
  Hasql.interp
    [HI.sql|SELECT r.id FROM projects.pr_review_runs r JOIN projects.pr_review_threads t ON t.id = r.thread_id
    WHERE t.project_id = #{pid} ORDER BY r.created_at DESC LIMIT 20|]
    >>= fmap catMaybes
    . traverse getRun


retryRun :: DB es => ProjectId -> ReviewId -> Eff es ()
retryRun pid rid = do
  let payload = Aeson $ AE.toJSON $ Jobs.ReviewPullRequest rid
  Hasql.interpExecute_
    [HI.sql|WITH retried AS (UPDATE projects.pr_review_runs r SET state = 'queued', result = NULL, error = NULL
      FROM projects.pr_review_threads t WHERE r.id = #{rid} AND r.thread_id = t.id AND t.project_id = #{pid}
        AND r.revision = t.latest_revision AND (t.lease_until IS NULL OR t.lease_until < now())
      RETURNING r.id)
      INSERT INTO background_jobs (run_at, status, payload) SELECT now(), 'queued', #{payload} FROM retried|]


supersedeRun :: DB es => ReviewRun -> Eff es ()
supersedeRun run = Hasql.interpExecute_ [HI.sql|UPDATE projects.pr_review_runs SET state = 'superseded', finished_at = now() WHERE id = #{run.id}|]
