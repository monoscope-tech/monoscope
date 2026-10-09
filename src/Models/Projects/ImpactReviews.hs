module Models.Projects.ImpactReviews (
  ReviewState (..),
  ReviewId,
  ReviewRun (..),
  ReviewSettings (..),
  PullRequestEvent (..),
  receiveEvent,
  decodeEvent,
  EventDecodeError (..),
  recordFailure,
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
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects (ProjectId)
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Pkg.Git qualified as Git
import Relude
import System.Types (DB)
import Web.FormUrlEncoded (FromForm)


-- $setup
-- >>> import Relude
-- >>> import Data.Aeson qualified as AE
-- >>> import Data.Text qualified as T


data ReviewState = Queued | Reviewing | Completed | Incomplete | Superseded
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "" ReviewState


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
  deriving anyclass (AE.FromJSON, HI.DecodeRow)


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


newtype Repository = Repository {fullName :: Text}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.Snake Repository


data PullRequestStatus = PullRequestStatus {head :: Git.CommitRef, state :: Text, draft :: Bool, updatedAt :: UTCTime}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.Snake PullRequestStatus


data GitHubEvent = GitHubEvent {action :: Text, repository :: Repository, installation :: Git.GitHubObjectId, number :: Int, pullRequest :: PullRequestStatus}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.Snake GitHubEvent


data EventDecodeError = UnsupportedAction | InvalidPayload
  deriving stock (Generic, Show)
  deriving (AE.ToJSON) via WrappedEnumSC 'Nothing "" EventDecodeError


-- | Decode the supported wire shape and refine its coordinates at the boundary.
--
-- >>> map AE.toJSON [UnsupportedAction, InvalidPayload]
-- [String "unsupported_action",String "invalid_payload"]
--
-- >>> let body = "{\"action\":\"opened\",\"number\":1,\"installation\":{\"id\":1},\"repository\":{\"full_name\":\"acme/service\"},\"pull_request\":{\"state\":\"open\",\"draft\":false,\"updated_at\":\"2025-01-01T00:00:00Z\",\"head\":{\"sha\":\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"}}}" :: Text
-- >>> fmap (.number) $ decodeEvent $ encodeUtf8 body
-- Right 1
-- >>> let reject before after = either (show @Text) (const "accepted") $ decodeEvent $ encodeUtf8 $ T.replace before after body
-- >>> map (uncurry reject) [("\"number\":1", "\"number\":0"), ("\"number\":1", "\"number\":-1"), ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "a"), ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"), ("acme/service", "acme")]
-- ["InvalidPayload","InvalidPayload","InvalidPayload","InvalidPayload","InvalidPayload"]
-- >>> reject "opened" "edited"
-- "UnsupportedAction"
decodeEvent :: ByteString -> Either EventDecodeError PullRequestEvent
decodeEvent body = do
  event <- first (const InvalidPayload) $ AE.eitherDecodeStrict @GitHubEvent body
  unless (event.action `elem` ["opened", "synchronize", "reopened", "ready_for_review", "closed", "converted_to_draft"]) $ Left UnsupportedAction
  (owner, repo) <- case T.splitOn "/" event.repository.fullName of
    [owner, repo] | not (T.null owner || T.null repo) -> pure (T.toLower owner, T.toLower repo)
    _ -> Left InvalidPayload
  unless (event.number > 0) $ Left InvalidPayload
  let pr = event.pullRequest
      revision = pr.head.revision
  unless (T.length revision == 40 && T.all isHexDigit revision) $ Left InvalidPayload
  pure PullRequestEvent{owner, repo, installationId = event.installation.id, number = event.number, revision = T.toLower revision, updatedAt = pr.updatedAt, reviewable = pr.state == "open" && not pr.draft}


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
    SELECT id, #{event.revision}, CASE WHEN #{event.reviewable} THEN #{Queued} ELSE #{Superseded} END FROM threads
    ON CONFLICT (thread_id, revision) DO UPDATE SET state = CASE
      WHEN NOT #{event.reviewable} THEN #{Superseded}
      WHEN projects.pr_review_runs.state = #{Superseded} THEN #{Queued}
      ELSE projects.pr_review_runs.state END
    WHERE NOT #{event.reviewable} OR projects.pr_review_runs.state = #{Superseded}
    RETURNING id, state
  ) INSERT INTO background_jobs (run_at, status, payload)
    SELECT now(), 'queued', jsonb_build_object('tag', 'ReviewPullRequest', 'contents', id)
    FROM runs WHERE state = #{Queued}|]


getRun :: DB es => ReviewId -> Eff es (Maybe ReviewRun)
getRun rid = listToMaybe <$> selectRuns Nothing (Just rid)


selectRuns :: DB es => Maybe ProjectId -> Maybe ReviewId -> Eff es [ReviewRun]
selectRuns pid rid =
  Hasql.interp
    [HI.sql|SELECT r.id, t.id, t.project_id, t.owner, t.repo, t.number::bigint, r.revision,
    r.state, r.result, r.error, t.comment_id, t.latest_revision
    FROM projects.pr_review_runs r JOIN projects.pr_review_threads t ON t.id = r.thread_id
    WHERE (#{pid}::uuid IS NULL OR t.project_id = #{pid}) AND (#{rid}::uuid IS NULL OR r.id = #{rid})
    ORDER BY r.created_at DESC LIMIT 20|]


claimRun :: DB es => ReviewRun -> Eff es Bool
claimRun run = do
  let payload = Aeson $ AE.toJSON $ Jobs.ReviewPullRequest run.id
  fromMaybe False
    <$> Hasql.interpOne
      [HI.sql|WITH leased AS (
    UPDATE projects.pr_review_threads t SET lease_owner = #{run.id}, lease_until = now() + interval '3 minutes'
    WHERE t.id = #{run.threadId} AND t.latest_revision = #{run.revision}
      AND (t.lease_until IS NULL OR t.lease_until < now())
      AND EXISTS (SELECT 1 FROM projects.pr_review_runs r WHERE r.id = #{run.id} AND r.state IN (#{Queued}, #{Reviewing}, #{Incomplete}))
    RETURNING t.id
  ), claimed AS (
    UPDATE projects.pr_review_runs SET state = #{Reviewing}, error = NULL
    WHERE id = #{run.id} AND thread_id IN (SELECT id FROM leased) RETURNING id
  ), deferred AS (
    INSERT INTO background_jobs (run_at, status, payload)
    SELECT t.lease_until + interval '1 second', 'queued', #{payload}
    FROM projects.pr_review_threads t JOIN projects.pr_review_runs r ON r.thread_id = t.id
    WHERE r.id = #{run.id} AND t.latest_revision = r.revision AND t.lease_owner <> r.id AND t.lease_until > now()
      AND r.state IN (#{Queued}, #{Incomplete}) AND NOT EXISTS (SELECT 1 FROM claimed)
    RETURNING id
  ) SELECT EXISTS (SELECT 1 FROM claimed)|]


currentRun :: DB es => ReviewRun -> Eff es Bool
currentRun run =
  fromMaybe False
    <$> Hasql.interpOne
      [HI.sql|SELECT t.latest_revision = r.revision AND t.lease_owner = r.id AND t.lease_until > now()
    AND r.state = #{Reviewing}
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
    [HI.sql|WITH finished AS (UPDATE projects.pr_review_runs SET state = #{Completed}, finished_at = now()
    WHERE id = #{run.id} AND state = #{Reviewing} RETURNING thread_id)
    UPDATE projects.pr_review_threads SET comment_id = coalesce(#{commentId}, comment_id)
    WHERE id IN (SELECT thread_id FROM finished) AND lease_owner = #{run.id}|]


recordFailure :: DB es => ReviewRun -> Text -> Eff es ()
recordFailure run err = Hasql.interpExecute_ [HI.sql|UPDATE projects.pr_review_runs SET error = #{err} WHERE id = #{run.id}|]


releaseRun :: DB es => ReviewRun -> Eff es ()
releaseRun run =
  Hasql.interpExecute_
    [HI.sql|WITH released AS (UPDATE projects.pr_review_threads SET lease_owner = NULL, lease_until = NULL
    WHERE id = #{run.threadId} AND lease_owner = #{run.id} RETURNING latest_revision)
    UPDATE projects.pr_review_runs SET state = CASE
      WHEN revision <> (SELECT latest_revision FROM released) THEN #{Superseded}
      WHEN state = #{Reviewing} THEN #{Incomplete} ELSE state END,
      error = coalesce(error, CASE WHEN state = #{Reviewing} THEN 'Review interrupted before publication; rerun when ready' ELSE NULL END)
    WHERE id = #{run.id} AND EXISTS (SELECT 1 FROM released)|]


repositorySettings :: DB es => ProjectId -> Eff es [ReviewSettings]
repositorySettings pid =
  Hasql.interp
    [HI.sql|SELECT DISTINCT lower(m.owner), lower(m.repo), coalesce(s.enabled, true), coalesce(s.include_evidence, false)
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
latestRuns pid = selectRuns (Just pid) Nothing


retryRun :: DB es => ProjectId -> ReviewId -> Eff es ()
retryRun pid rid = do
  let payload = Aeson $ AE.toJSON $ Jobs.ReviewPullRequest rid
  Hasql.interpExecute_
    [HI.sql|WITH retried AS (UPDATE projects.pr_review_runs r SET state = #{Queued}, result = NULL, error = NULL
      FROM projects.pr_review_threads t WHERE r.id = #{rid} AND r.thread_id = t.id AND t.project_id = #{pid}
        AND r.revision = t.latest_revision AND (t.lease_until IS NULL OR t.lease_until < now())
      RETURNING r.id)
      INSERT INTO background_jobs (run_at, status, payload) SELECT now(), 'queued', #{payload} FROM retried|]


supersedeRun :: DB es => ReviewRun -> Eff es ()
supersedeRun run = Hasql.interpExecute_ [HI.sql|UPDATE projects.pr_review_runs SET state = #{Superseded}, finished_at = now() WHERE id = #{run.id}|]
