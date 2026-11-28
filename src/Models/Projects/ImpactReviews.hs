module Models.Projects.ImpactReviews (
  ReviewState (..),
  ReviewId,
  ReviewRun (..),
  ReviewSettings (..),
  ReviewTrigger (..),
  PullRequestState (..),
  pullRequestEvent,
  Reception (..),
  PullRequestEvent (..),
  Ask (..),
  Command (..),
  CommentRequest (..),
  receiveEvent,
  decodeEvent,
  decodeComment,
  parseAsk,
  EventDecodeError (..),
  recordFailure,
  getRun,
  claimRun,
  routineMissingError,
  retryRoutineBlockedRuns,
  announceReviewWorker,
  reviewWorkerRunning,
  currentRun,
  saveResult,
  finishRun,
  releaseRun,
  repositorySettings,
  updateSettings,
  latestRuns,
  retryRun,
  queueSetupRun,
  markSetupTried,
  setupOnly,
  supersedeRun,
  startAttempt,
  recordUsage,
  UsageTotals (..),
  usageTotals,
  UsageCost (..),
  ProjectUsage (..),
  customerUsage,
  recordCheck,
  RecordedCheck (..),
  latestChecks,
  KnowledgeEntry (..),
  priorKnowledge,
  priorRepositoryKnowledge,
  KnowledgeSource (..),
  ProofSide (..),
  ProofFormat (..),
  ProofFile (..),
  ProofAsset (..),
  proofSignedPath,
  Rating (..),
  ratingSignedPath,
  rateRun,
  setDismissal,
  saveProofs,
  ProofRecipe (..),
  SetupReuse (..),
  proofRecipe,
  recordSetupFailure,
  PriorReview (..),
  priorReview,
  proofSides,
  proofFile,
  ClaimState (..),
  ClaimProgress (..),
  advance,
  recordProgress,
  progressComment,
  setProgressComment,
) where

import BackgroundJobs.Types qualified as Jobs
import Data.Aeson qualified as AE
import Data.Char (isDigit, isHexDigit)
import Data.Effectful.AgentWorkspace qualified as Workspace (Slots (..))
import Data.Effectful.CodeReview qualified as Agent
import Data.Effectful.CodeReview.Types qualified as Agent (ProofMedia (..), reviewSeconds)
import Data.Effectful.Hasql qualified as Hasql
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import Models.Ids (ProjectId)
import Models.Projects.AgentUsage (ProjectUsage (..), UsageCost (..), UsageTotals (..), customerUsage)
import Models.Projects.AgentUsage qualified as Usage
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Pkg.Git qualified as Git
import Relude hiding (Down)
import System.Types (DB)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))


-- $setup
-- >>> import Relude
-- >>> import Data.Aeson qualified as AE
-- >>> import Data.Text qualified as T
-- >>> import Data.Effectful.CodeReview qualified as Agent


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
  , createdAt :: UTCTime
  , request :: Maybe Ask
  , base :: Maybe Text
  , rating :: Maybe Rating
  , dismissedBy :: Maybe Text
  , reviewUrl :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, HI.DecodeRow)


-- | Which pull requests are reviewed without being asked. Every mode accepts an ask.
data ReviewTrigger = ReadyPullRequests | AllPullRequests | WhenAsked
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, ToHttpApiData) via WrappedEnumSC 'Nothing "" ReviewTrigger


data ReviewSettings = ReviewSettings {owner :: Text, repo :: Text, trigger :: ReviewTrigger, includeEvidence :: Bool}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm, HI.DecodeRow)


-- | What a delivery did: queued a review, joined one already queued or running, or neither
-- (closed, not linked to any project, or a mode that waits to be asked).
data Reception = RunQueued | JoinedRun | NotReviewable
  deriving stock (Eq, Generic, Read, Show)
  deriving (HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC 'Nothing "" Reception


data PullRequestState = Open | Draft | Closed
  deriving stock (Eq, Generic, Read, Show)
  deriving (HI.EncodeValue) via WrappedEnumSC 'Nothing "" PullRequestState


-- Every delivery advances the thread, so a newer head or a closure invalidates an
-- in-flight publication even when that delivery queues nothing.
data PullRequestEvent = PullRequestEvent
  { owner :: Text
  , repo :: Text
  , installationId :: Int64
  , number :: Int
  , revision :: Text
  , base :: Text
  , updatedAt :: UTCTime
  , state :: PullRequestState
  }
  deriving stock (Generic, Show)


-- | Setup: a setup-only run that builds the repository's proof setup and leaves the pull request's review alone.
-- Dismiss and Restore set the latest finished review's proof aside, or take it back, without a new run.
data Command = Review | Prove | Setup | Dismiss | Restore
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" Command


-- | A member's @\@monoscope@ comment. Its focus is untrusted guidance for the agent.
data Ask = Ask {login :: Text, commentId :: Int64, command :: Command, focus :: Maybe Text}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue, HI.EncodeValue) via Aeson Ask


-- | A verified ask whose head revision is still unknown: comment deliveries carry no PR head.
data CommentRequest = CommentRequest {owner :: Text, repo :: Text, installationId :: Int64, number :: Int, ask :: Ask}


newtype Repository = Repository {fullName :: Text}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.Snake Repository


data GitHubEvent = GitHubEvent {action :: Text, repository :: Repository, installation :: Git.GitHubObjectId, number :: Int, pullRequest :: Git.PullRequest}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.Snake GitHubEvent


data EventDecodeError = UnsupportedAction | InvalidPayload | NotAnAsk | UntrustedAuthor
  deriving stock (Generic, Show)
  deriving (AE.ToJSON) via WrappedEnumSC 'Nothing "" EventDecodeError


-- | Decode the supported wire shape and refine its coordinates at the boundary.
--
-- >>> map AE.toJSON [UnsupportedAction, InvalidPayload, NotAnAsk, UntrustedAuthor]
-- [String "unsupported_action",String "invalid_payload",String "not_an_ask",String "untrusted_author"]
--
-- >>> let body = "{\"action\":\"opened\",\"number\":1,\"installation\":{\"id\":1},\"repository\":{\"full_name\":\"acme/service\"},\"pull_request\":{\"state\":\"open\",\"draft\":false,\"changed_files\":1,\"updated_at\":\"2025-01-01T00:00:00Z\",\"base\":{\"sha\":\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"},\"head\":{\"sha\":\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"}}}" :: Text
-- >>> fmap (\e -> (e.number, e.state)) $ decodeEvent $ encodeUtf8 body
-- Right (1,Open)
-- >>> fmap (.state) $ decodeEvent $ encodeUtf8 $ T.replace "\"draft\":false" "\"draft\":true" body
-- Right Draft
-- >>> let reject before after = either (show @Text) (const "accepted") $ decodeEvent $ encodeUtf8 $ T.replace before after body
-- >>> map (uncurry reject) [("\"number\":1", "\"number\":0"), ("\"number\":1", "\"number\":-1"), ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "a"), ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"), ("acme/service", "acme")]
-- ["InvalidPayload","InvalidPayload","InvalidPayload","InvalidPayload","InvalidPayload"]
-- >>> reject "opened" "edited"
-- "UnsupportedAction"
decodeEvent :: ByteString -> Either EventDecodeError PullRequestEvent
decodeEvent body = do
  event <- first (const InvalidPayload) $ AE.eitherDecodeStrict @GitHubEvent body
  unless (event.action `elem` ["opened", "synchronize", "reopened", "ready_for_review", "closed", "converted_to_draft"]) $ Left UnsupportedAction
  (owner, repo) <- repositoryName event.repository
  unless (event.number > 0) $ Left InvalidPayload
  let revision = event.pullRequest.head.revision
  unless (T.length revision == 40 && T.all isHexDigit revision) $ Left InvalidPayload
  pure $ pullRequestEvent owner repo event.installation.id event.number event.pullRequest


-- | The event a fetched pull request implies, as if GitHub had just delivered it.
pullRequestEvent :: Text -> Text -> Int64 -> Int -> Git.PullRequest -> PullRequestEvent
pullRequestEvent owner repo installationId number pr = PullRequestEvent{owner, repo, installationId, number, revision = T.toLower pr.head.revision, base = T.toLower pr.base.revision, updatedAt = pr.updatedAt, state = if pr.state /= "open" then Closed else if pr.draft then Draft else Open}


repositoryName :: Repository -> Either EventDecodeError (Text, Text)
repositoryName repository = case T.splitOn "/" repository.fullName of
  [owner, repo] | not (T.null owner || T.null repo) -> pure (T.toLower owner, T.toLower repo)
  _ -> Left InvalidPayload


data CommentUser = CommentUser {login :: Text, type_ :: Text}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.Rename "type_" "type"]] CommentUser


data Comment = Comment {id :: Int64, body :: Text, authorAssociation :: Text, user :: CommentUser}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.Snake Comment


data CommentIssue = CommentIssue {number :: Int, pullRequest :: Maybe AE.Value}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.Snake CommentIssue


data CommentEvent = CommentEvent {action :: Text, repository :: Repository, installation :: Git.GitHubObjectId, issue :: CommentIssue, comment :: Comment}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.Snake CommentEvent


-- | Owners, members and collaborators pass here; `requestReview` then requires write access.
-- Bots are ignored, so a review that quotes the command cannot trigger itself.
--
-- >>> let body = "{\"action\":\"created\",\"installation\":{\"id\":7},\"repository\":{\"full_name\":\"Acme/Service\"},\"issue\":{\"number\":4,\"pull_request\":{}},\"comment\":{\"id\":9,\"body\":\"@monoscope prove it\",\"author_association\":\"MEMBER\",\"user\":{\"login\":\"ada\",\"type\":\"User\"}}}" :: Text
-- >>> fmap (\r -> (r.owner, r.number, r.ask.command)) $ decodeComment $ encodeUtf8 body
-- Right ("acme",4,Prove)
-- >>> let reject before after = either (show @Text) (const "accepted") $ decodeComment $ encodeUtf8 $ T.replace before after body
-- >>> map (uncurry reject) [("MEMBER", "CONTRIBUTOR"), ("\"User\"", "\"Bot\""), ("prove it", "hello"), ("\"pull_request\":{}", "\"pull_request\":null"), ("created", "edited")]
-- ["UntrustedAuthor","UntrustedAuthor","NotAnAsk","NotAnAsk","UnsupportedAction"]
decodeComment :: ByteString -> Either EventDecodeError CommentRequest
decodeComment body = do
  event <- first (const InvalidPayload) $ AE.eitherDecodeStrict @CommentEvent body
  (owner, repo) <- repositoryName event.repository
  let comment = event.comment
  unless (event.issue.number > 0) $ Left InvalidPayload
  unless (event.action == "created") $ Left UnsupportedAction
  unless (isJust event.issue.pullRequest) $ Left NotAnAsk
  unless (comment.user.type_ /= "Bot" && comment.authorAssociation `elem` ["OWNER", "MEMBER", "COLLABORATOR"]) $ Left UntrustedAuthor
  (command, focus) <- maybeToRight NotAnAsk $ parseAsk comment.body
  pure CommentRequest{owner, repo, installationId = event.installation.id, number = event.issue.number, ask = Ask{login = comment.user.login, commentId = comment.id, command, focus}}


-- | The first @\@monoscope@ command in a comment, and any focus written after it.
--
-- >>> map parseAsk ["@monoscope review", "@Monoscope TEST", "Thanks! @monoscope prove it, and check the empty state on mobile", "@monoscope prove", "email me@monoscope.dev", "@monoscope reviewer", "@monoscope setup"]
-- [Just (Review,Nothing),Just (Prove,Nothing),Just (Prove,Just "and check the empty state on mobile"),Nothing,Nothing,Nothing,Just (Setup,Nothing)]
-- >>> map parseAsk ["@monoscope dismiss: the base was broken", "@monoscope restore", "@monoscope restore this proof"]
-- [Just (Dismiss,Just "the base was broken"),Just (Restore,Nothing),Just (Restore,Just "this proof")]
parseAsk :: Text -> Maybe (Command, Maybe Text)
parseAsk body = listToMaybe $ mapMaybe command $ T.breakOnAll "@" body
  where
    prefixCI word text = T.toLower (T.take (T.length word) text) == word
    command (before, after) = do
      guard $ T.null before || T.last before `elem` [' ', '\n', '\t', '(']
      guard $ prefixCI "@monoscope " after
      let rest = T.drop (T.length "@monoscope ") after
      asum
        [ (c, focus) <$ guard (prefixCI word rest && maybe True ((`elem` [' ', ',', '.', ':', '!', '\n']) . fst) (T.uncons remainder))
        | (word, c) <- [("review", Review), ("test", Prove), ("prove it", Prove), ("setup", Setup), ("dismiss", Dismiss), ("restore", Restore)]
        , let remainder = T.drop (T.length word) rest
              focus = guarded (not . T.null) $ T.take 500 $ T.strip $ T.dropWhile (`elem` [',', ':', '-', '.', ' ']) $ T.takeWhile (/= '\n') remainder
        ]


-- | Grant resolution, receipt, and odd-job insertion are one statement. An
-- out-of-order delivery cannot replace a newer head, and duplicates queue once.
-- A closure, or a draft in a mode that skips drafts, supersedes the revision's run;
-- an ask queues in every mode, replaces a finished run of the same revision and joins a queued one.
-- A queued review queues its workspace build unless one for the same project, repository and base waits.
receiveEvent :: DB es => Maybe Ask -> PullRequestEvent -> Eff es Reception
receiveEvent request event =
  maybe NotReviewable HI.getOneColumn
    <$> Hasql.interpOne
      [HI.sql|WITH eligible AS (
    SELECT DISTINCT m.project_id, CASE WHEN s.enabled IS FALSE THEN #{WhenAsked} ELSE coalesce(s.trigger, #{ReadyPullRequests}) END AS trigger FROM projects.code_mappings m
    JOIN projects.git_credentials c ON c.id = m.credential_id AND c.project_id = m.project_id
    LEFT JOIN projects.pr_review_settings s ON s.project_id = m.project_id AND s.owner = lower(m.owner) AND s.repo = lower(m.repo)
    WHERE c.host = 'github' AND c.api_base IS NULL AND c.installation_id = #{event.installationId}
      AND lower(m.owner) = #{event.owner} AND lower(m.repo) = #{event.repo}
  ), threads AS (
    INSERT INTO projects.pr_review_threads (project_id, owner, repo, number, latest_revision, event_at)
    SELECT project_id, #{event.owner}, #{event.repo}, #{event.number}, #{event.revision}, #{event.updatedAt} FROM eligible
    ON CONFLICT (project_id, owner, repo, number) DO UPDATE
      SET latest_revision = EXCLUDED.latest_revision, event_at = EXCLUDED.event_at
      WHERE projects.pr_review_threads.event_at <= EXCLUDED.event_at
    RETURNING id, project_id
  ), decided AS (
    SELECT t.id,
      #{event.state} = #{Closed} OR (#{event.state} = #{Draft} AND e.trigger = #{ReadyPullRequests} AND #{request}::jsonb IS NULL) AS stop,
      #{event.state} <> #{Closed} AND (#{request}::jsonb IS NOT NULL OR (#{event.state} = #{Open} AND e.trigger <> #{WhenAsked}) OR e.trigger = #{AllPullRequests}) AS queue
    FROM threads t JOIN eligible e ON e.project_id = t.project_id
  ), runs AS (
    INSERT INTO projects.pr_review_runs (thread_id, revision, state, request, base)
    SELECT id, #{event.revision}, CASE WHEN queue THEN #{Queued} ELSE #{Superseded} END, #{request}, #{event.base} FROM decided WHERE stop OR queue
    ON CONFLICT (thread_id, revision) DO UPDATE SET state = EXCLUDED.state, request = coalesce(EXCLUDED.request, CASE WHEN projects.pr_review_runs.request->>'command' <> 'setup' THEN projects.pr_review_runs.request END), base = EXCLUDED.base,
      -- A setup-only run leaves the pull request's review and its comment as they were.
      result = CASE WHEN EXCLUDED.request IS NULL OR EXCLUDED.request->>'command' = 'setup' THEN projects.pr_review_runs.result END, error = NULL,
      -- An ask answers below itself, so it does not reuse the comment an earlier review of this head updated.
      progress_comment_id = CASE WHEN EXCLUDED.request IS NULL OR EXCLUDED.request->>'command' = 'setup' THEN projects.pr_review_runs.progress_comment_id END
    WHERE EXCLUDED.state = #{Superseded} OR projects.pr_review_runs.state = #{Superseded}
      OR (EXCLUDED.request IS NOT NULL AND projects.pr_review_runs.state IN (#{Queued}, #{Completed}, #{Incomplete}))
    RETURNING id, state, thread_id
  ), prior AS (
    SELECT r.id FROM projects.pr_review_runs r JOIN decided d ON d.id = r.thread_id
    WHERE r.revision = #{event.revision} AND r.state = #{Queued}
  ), queued AS (
    SELECT r.id, t.project_id FROM runs r JOIN threads t ON t.id = r.thread_id WHERE r.state = #{Queued} AND r.id NOT IN (SELECT id FROM prior)
  ), builds AS (
    INSERT INTO background_jobs_long (run_at, status, payload)
    SELECT now(), 'queued', jsonb_build_object('tag', 'PrepareReviewWorkspace', 'contents', q.id) FROM queued q
    WHERE NOT EXISTS (
      SELECT 1 FROM background_jobs_long j
      JOIN projects.pr_review_runs r ON r.id::text = j.payload->>'contents'
      JOIN projects.pr_review_threads t ON t.id = r.thread_id
      WHERE j.payload->>'tag' = 'PrepareReviewWorkspace' AND j.status IN ('queued', 'retry', 'locked')
        AND t.project_id = q.project_id AND t.owner = #{event.owner} AND t.repo = #{event.repo} AND r.base = #{event.base})
  ), jobs AS (
    INSERT INTO background_jobs_long (run_at, status, payload)
    SELECT now(), 'queued', jsonb_build_object('tag', 'ReviewPullRequest', 'contents', id) FROM queued RETURNING 1
  ) SELECT CASE WHEN EXISTS (SELECT 1 FROM jobs) THEN #{RunQueued} WHEN EXISTS (SELECT 1 FROM decided WHERE queue) THEN #{JoinedRun} ELSE #{NotReviewable} END|]


getRun :: DB es => ReviewId -> Eff es (Maybe ReviewRun)
getRun rid = listToMaybe <$> selectRuns Nothing (Just rid)


selectRuns :: DB es => Maybe ProjectId -> Maybe ReviewId -> Eff es [ReviewRun]
selectRuns pid rid =
  Hasql.interp
    [HI.sql|SELECT r.id, t.id, t.project_id, t.owner, t.repo, t.number::bigint, r.revision,
    r.state, r.result, r.error, t.comment_id, t.latest_revision, r.created_at, r.request, r.base, r.rating, r.dismissed_by, r.review_url
    FROM projects.pr_review_runs r JOIN projects.pr_review_threads t ON t.id = r.thread_id
    WHERE (#{pid}::uuid IS NULL OR t.project_id = #{pid}) AND (#{rid}::uuid IS NULL OR r.id = #{rid})
    ORDER BY r.created_at DESC LIMIT 20|]


-- | Called every minute by a node with agent slots, so readiness can tell a queued review will run.
announceReviewWorker :: DB es => UTCTime -> Workspace.Slots -> Eff es ()
announceReviewWorker now slots =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO projects.review_workers (node, agents, seen_at) VALUES (#{slots.node}, #{fromIntegral @Int @Int32 slots.agents}, #{now})
      ON CONFLICT (node) DO UPDATE SET agents = EXCLUDED.agents, seen_at = EXCLUDED.seen_at |]


-- | A worker with agent slots checked in within three minutes.
reviewWorkerRunning :: DB es => UTCTime -> Eff es Bool
reviewWorkerRunning now = Hasql.interpScalar [HI.sql| SELECT EXISTS (SELECT 1 FROM projects.review_workers WHERE agents > 0 AND seen_at > #{now} - interval '3 minutes') |]


-- | Admission happens here, before a review's time budget starts: past this node's agent slots, or two
-- reviews of this project across nodes (so one busy tenant cannot take every slot), the run waits in
-- the job queue instead of for a VM.
claimRun :: DB es => Workspace.Slots -> ReviewRun -> Eff es Bool
claimRun slots run = Hasql.transaction TxS.ReadCommitted TxS.Write do
  let payload = Aeson $ AE.toJSON $ Jobs.ReviewPullRequest run.id
  -- Concurrent claims would each count the others' leases as free; the lock makes them take turns,
  -- and read committed gives the counting statement a snapshot taken after the lock.
  void $ Hasql.queryTx @[Text] [HI.sql|SELECT pg_advisory_xact_lock(hashtextextended('pr_review_admission', 0))::text|]
  Hasql.oneTx @Bool
    [HI.sql|WITH busy AS (
    SELECT count(*) FILTER (WHERE o.project_id = #{run.projectId}) >= 2 OR count(*) FILTER (WHERE o.lease_node = #{slots.node}) >= #{fromIntegral @Int @Int64 slots.agents} AS saturated
    FROM projects.pr_review_threads o WHERE o.id <> #{run.threadId} AND o.lease_until > now()
  ), building AS (
    -- Proofs run in the base's prepared workspace, so a review waits while that build is pending.
    SELECT #{slots.prepares} AND EXISTS (
      SELECT 1 FROM background_jobs_long j
      JOIN projects.pr_review_runs b ON b.id::text = j.payload->>'contents'
      JOIN projects.pr_review_threads bt ON bt.id = b.thread_id
      JOIN projects.pr_review_threads t ON t.id = #{run.threadId}
      WHERE j.payload->>'tag' = 'PrepareReviewWorkspace' AND j.status IN ('queued', 'retry', 'locked')
        AND bt.project_id = t.project_id AND bt.owner = t.owner AND bt.repo = t.repo AND b.base = #{run.base}) AS pending
  ), leased AS (
    UPDATE projects.pr_review_threads t SET lease_owner = #{run.id}, lease_node = #{slots.node}, lease_until = now() + make_interval(secs => #{fromIntegral @Int @Int32 (Agent.reviewSeconds + 300)})
    WHERE t.id = #{run.threadId} AND t.latest_revision = #{run.revision}
      AND (t.lease_until IS NULL OR t.lease_until < now()) AND NOT (SELECT saturated FROM busy) AND NOT (SELECT pending FROM building)
      AND EXISTS (SELECT 1 FROM projects.pr_review_runs r WHERE r.id = #{run.id} AND r.state IN (#{Queued}, #{Reviewing}, #{Incomplete}))
    RETURNING t.id
  ), claimed AS (
    UPDATE projects.pr_review_runs SET state = #{Reviewing}, error = NULL
    WHERE id = #{run.id} AND thread_id IN (SELECT id FROM leased) RETURNING id
  ), deferred AS (
    INSERT INTO background_jobs_long (run_at, status, payload)
    SELECT CASE WHEN t.lease_until > now() AND t.lease_owner <> r.id THEN t.lease_until + interval '1 second'
      WHEN t.lease_until > now() THEN now() + interval '1 second' WHEN (SELECT pending FROM building) THEN now() + interval '30 seconds'
      ELSE now() + interval '15 seconds' END, 'queued', #{payload}
    FROM projects.pr_review_threads t JOIN projects.pr_review_runs r ON r.thread_id = t.id
    WHERE r.id = #{run.id} AND t.latest_revision = r.revision AND (t.lease_until > now() OR (SELECT saturated FROM busy) OR (SELECT pending FROM building))
      AND r.state IN (#{Queued}, #{Reviewing}, #{Incomplete}) AND NOT EXISTS (SELECT 1 FROM claimed)
    RETURNING id
  ) SELECT EXISTS (SELECT 1 FROM claimed)|]


currentRun :: DB es => ReviewRun -> Eff es Bool
currentRun run =
  fromMaybe False
    <$> Hasql.interpOne
      [HI.sql|SELECT t.latest_revision = r.revision AND t.lease_owner = r.id AND t.lease_until > now()
    AND r.state = #{Reviewing}
    AND EXISTS (SELECT 1 FROM projects.code_mappings m JOIN projects.git_credentials c ON c.id = m.credential_id AND c.project_id = m.project_id
      WHERE m.project_id = t.project_id AND lower(m.owner) = t.owner AND lower(m.repo) = t.repo AND c.host = 'github' AND c.api_base IS NULL AND c.installation_id IS NOT NULL)
    FROM projects.pr_review_runs r JOIN projects.pr_review_threads t ON t.id = r.thread_id
    WHERE r.id = #{run.id}|]


saveResult :: (AE.ToJSON a, DB es) => ReviewRun -> a -> Eff es ()
saveResult run result = Hasql.interpExecute_ [HI.sql|UPDATE projects.pr_review_runs SET result = #{Aeson $ AE.toJSON result} WHERE id = #{run.id}|]


-- | Completes a run with the comment it published and the full review's address, which a later
-- dismissal needs to redraw the same comment.
finishRun :: DB es => ReviewRun -> Maybe (Int64, Text) -> Eff es ()
finishRun run published =
  Hasql.interpExecute_
    [HI.sql|WITH finished AS (UPDATE projects.pr_review_runs SET state = #{Completed}, finished_at = now(), review_url = #{snd <$> published}
    WHERE id = #{run.id} AND state = #{Reviewing} RETURNING id, thread_id, revision, result),
    remembered AS (
      INSERT INTO projects.pr_review_knowledge
        (run_id, project_id, owner, repo, service, revision, summary, evidence_keys, window_start, window_end, evidence)
      SELECT f.id, t.project_id, t.owner, t.repo, note->>'service', f.revision, note->>'summary',
        ARRAY(SELECT jsonb_array_elements_text(note->'evidenceKeys')),
        (f.result->>'windowStart')::timestamptz, (f.result->>'windowEnd')::timestamptz,
        coalesce((SELECT jsonb_agg(e #- '{queryResult,dataset}') FROM jsonb_array_elements(f.result->'evidence') e
          WHERE e->>'key' IN (SELECT jsonb_array_elements_text(note->'evidenceKeys'))), '[]'::jsonb)
      FROM finished f JOIN projects.pr_review_threads t ON t.id = f.thread_id,
        LATERAL jsonb_array_elements(CASE WHEN jsonb_typeof(f.result->'knowledge') = 'array'
          THEN f.result->'knowledge' ELSE '[]'::jsonb END) note
      WHERE NOT EXISTS (SELECT 1 FROM projects.pr_review_knowledge k
        WHERE k.run_id = f.id AND k.service = note->>'service' AND k.summary = note->>'summary'
          AND k.window_end = (f.result->>'windowEnd')::timestamptz)
    )
    UPDATE projects.pr_review_threads SET comment_id = coalesce(#{fst <$> published}, comment_id)
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
    -- Unsaved evidence follows visibility: a private repository's readers already have access,
    -- so its comments carry production aggregates. A public one gets links only, whatever was
    -- saved while it was private: its comments are readable by anyone.
    [HI.sql|SELECT DISTINCT lower(m.owner), lower(m.repo), CASE WHEN s.enabled IS FALSE THEN #{WhenAsked} ELSE coalesce(s.trigger, #{ReadyPullRequests}) END,
      coalesce(s.include_evidence, bool_or(r.private) OVER w, false) AND bool_or(r.private) OVER w IS NOT FALSE
    FROM projects.code_mappings m JOIN projects.git_credentials c ON c.id = m.credential_id AND c.project_id = m.project_id
    LEFT JOIN projects.pr_review_settings s ON s.project_id = m.project_id AND s.owner = lower(m.owner) AND s.repo = lower(m.repo)
    LEFT JOIN projects.repositories r ON r.project_id = m.project_id AND r.host = c.host AND r.api_base IS NOT DISTINCT FROM c.api_base AND lower(r.owner) = lower(m.owner) AND lower(r.repo) = lower(m.repo)
    WHERE m.project_id = #{pid} AND c.host = 'github' AND c.api_base IS NULL AND c.installation_id IS NOT NULL
    WINDOW w AS (PARTITION BY lower(m.owner), lower(m.repo))
    ORDER BY lower(m.owner), lower(m.repo)|]


updateSettings :: DB es => ProjectId -> ReviewSettings -> Eff es ()
updateSettings pid settings =
  Hasql.interpExecute_
    [HI.sql|INSERT INTO projects.pr_review_settings (project_id, owner, repo, trigger, enabled, include_evidence)
    SELECT #{pid}, #{settings.owner}, #{settings.repo}, #{settings.trigger}, #{settings.trigger /= WhenAsked}, #{settings.includeEvidence}
    WHERE EXISTS (SELECT 1 FROM projects.code_mappings WHERE project_id = #{pid} AND lower(owner) = #{settings.owner} AND lower(repo) = #{settings.repo})
    ON CONFLICT (project_id, owner, repo) DO UPDATE SET trigger = EXCLUDED.trigger, enabled = EXCLUDED.enabled, include_evidence = EXCLUDED.include_evidence|]


latestRuns :: DB es => ProjectId -> Eff es [ReviewRun]
latestRuns pid = selectRuns (Just pid) Nothing


retryRun :: DB es => ProjectId -> ReviewId -> Eff es ()
retryRun pid rid = do
  let payload = Aeson $ AE.toJSON $ Jobs.ReviewPullRequest rid
  Hasql.interpExecute_
    [HI.sql|WITH retried AS (UPDATE projects.pr_review_runs r SET state = #{Queued}, result = NULL, error = NULL, rating = NULL, rated_at = NULL,
      request = CASE WHEN r.request->>'command' <> 'setup' THEN r.request END
      FROM projects.pr_review_threads t WHERE r.id = #{rid} AND r.thread_id = t.id AND t.project_id = #{pid}
        AND r.revision = t.latest_revision AND (t.lease_until IS NULL OR t.lease_until < now())
      RETURNING r.id)
      INSERT INTO background_jobs_long (run_at, status, payload) SELECT now(), 'queued', #{payload} FROM retried|]


-- | After a review, a repository that uses proofs but has no setup, or whose setup last failed, gets one setup-only run
-- of the same pull request, at most daily; a setup that works and has not failed since is left alone.
queueSetupRun :: DB es => ReviewRun -> Eff es ()
queueSetupRun run =
  Hasql.interpExecute_
    [HI.sql|WITH needed AS (
      SELECT 1 WHERE (EXISTS (SELECT 1 FROM projects.pr_review_proof_recipes
          WHERE project_id = #{run.projectId} AND owner = #{run.owner} AND repo = #{run.repo}
            AND (failure IS NOT NULL OR stage < 3 AND (setup_tried_at IS NULL OR setup_tried_at < updated_at)))
        OR (EXISTS (SELECT 1 FROM projects.pr_review_proofs WHERE run_id = #{run.id})
          AND NOT EXISTS (SELECT 1 FROM projects.pr_review_proof_recipes WHERE project_id = #{run.projectId} AND owner = #{run.owner} AND repo = #{run.repo})))
        AND NOT EXISTS (SELECT 1 FROM projects.pr_review_runs r JOIN projects.pr_review_threads t ON t.id = r.thread_id
          WHERE t.project_id = #{run.projectId} AND t.owner = #{run.owner} AND t.repo = #{run.repo} AND r.request->>'command' = 'setup'
            AND (r.state IN (#{Queued}, #{Reviewing}) OR r.finished_at > now() - interval '24 hours'))
    ), queued AS (
      UPDATE projects.pr_review_runs SET state = #{Queued}, request = #{Ask "monoscope" 0 Setup Nothing}, error = NULL
      WHERE id = #{run.id} AND state = #{Completed} AND EXISTS (SELECT 1 FROM needed) RETURNING id
    ) INSERT INTO background_jobs_long (run_at, status, payload) SELECT now(), 'queued', #{Aeson $ AE.toJSON $ Jobs.ReviewPullRequest run.id} FROM queued|]


markSetupTried :: DB es => ReviewRun -> Eff es ()
markSetupTried run = Hasql.interpExecute_ [HI.sql|UPDATE projects.pr_review_proof_recipes SET setup_tried_at = now() WHERE project_id = #{run.projectId} AND owner = #{run.owner} AND repo = #{run.repo}|]


-- | A setup-only run: it builds the repository's proof setup and publishes nothing to the pull request.
setupOnly :: ReviewRun -> Bool
setupOnly run = ((.command) <$> run.request) == Just Setup


-- | Why a review stopped before starting: its project had no active review routine.
routineMissingError :: Text
routineMissingError = "Activate or resume Production impact review in AI routines."


-- | Reviews that stopped only for want of the routine, on their pull request's latest commit, run
-- once it exists, so a pull request opened before the routine was installed still gets reviewed.
retryRoutineBlockedRuns :: DB es => ProjectId -> Eff es ()
retryRoutineBlockedRuns pid =
  Hasql.interp @[HI.OneColumn ReviewId]
    [HI.sql| SELECT r.id FROM projects.pr_review_runs r JOIN projects.pr_review_threads t ON t.id = r.thread_id
      WHERE t.project_id = #{pid} AND r.revision = t.latest_revision AND r.state = #{Incomplete} AND r.error = #{routineMissingError} |]
    >>= traverse_ (retryRun pid . HI.getOneColumn)


supersedeRun :: DB es => ReviewRun -> Eff es ()
supersedeRun run = Hasql.interpExecute_ [HI.sql|UPDATE projects.pr_review_runs SET state = #{Superseded}, finished_at = now() WHERE id = #{run.id}|]


startAttempt :: DB es => ReviewRun -> Text -> Text -> Eff es (Maybe (UUIDId "agent_attempt"))
startAttempt run provider modelSpec = do
  current <- currentRun run
  -- An attempt shows only its own proofs; the comment it updates stays the run's.
  when current $ Hasql.interpExecute_ [HI.sql|UPDATE projects.pr_review_runs SET proof_progress = '[]' WHERE id = #{run.id}|]
  if current then Usage.startAttempt run.projectId (Usage.Review run.id) provider modelSpec else pure Nothing


recordUsage :: DB es => ReviewRun -> Agent.ModelUsage -> Eff es Bool
recordUsage run = Usage.recordUsage run.projectId (Usage.Review run.id)


recordCheck :: DB es => ReviewRun -> Agent.CheckReceipt -> Eff es Bool
recordCheck run = Usage.recordCheck run.projectId (Usage.Review run.id)


usageTotals :: DB es => ProjectId -> Eff es [UsageTotals "pr_review"]
usageTotals = Usage.usageTotals Usage.Reviews


data RecordedCheck = RecordedCheck {runId :: ReviewId, receipt :: Aeson Agent.CheckReceipt}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


latestChecks :: DB es => ProjectId -> Eff es [RecordedCheck]
latestChecks pid =
  Hasql.interp
    [HI.sql|
  SELECT run_id, receipt FROM projects.pr_review_checks
  WHERE project_id = #{pid} AND run_id IN (
    SELECT r.id FROM projects.pr_review_runs r JOIN projects.pr_review_threads t ON t.id = r.thread_id
    WHERE t.project_id = #{pid} ORDER BY r.created_at DESC LIMIT 20)
  ORDER BY created_at, id
|]


data KnowledgeSource = ReviewSource ReviewId | FixSource (UUIDId "issue_fix")
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue) via Aeson KnowledgeSource


data KnowledgeEntry = KnowledgeEntry
  { source :: KnowledgeSource
  , revision :: Text
  , service :: Text
  , summary :: Text
  , evidenceKeys :: V.Vector Text
  , windowStart :: UTCTime
  , windowEnd :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON, HI.DecodeRow)


-- Evidence snapshots survive reruns replacing the original review result.
priorKnowledge :: DB es => ReviewRun -> Text -> Eff es [KnowledgeEntry]
priorKnowledge run = priorRepositoryKnowledge run.projectId (Git.RepoRef run.owner run.repo run.revision)


priorRepositoryKnowledge :: DB es => ProjectId -> Git.RepoRef -> Text -> Eff es [KnowledgeEntry]
priorRepositoryKnowledge pid ref service =
  Hasql.interp
    [HI.sql|
  SELECT jsonb_build_object('tag', CASE WHEN run_id IS NULL THEN 'FixSource' ELSE 'ReviewSource' END, 'contents', coalesce(run_id, fix_run_id)), revision, service, summary, evidence_keys, window_start, window_end
  FROM projects.pr_review_knowledge
  WHERE project_id = #{pid} AND owner = #{ref.owner} AND repo = #{ref.repo} AND service = #{service}
  ORDER BY created_at DESC, id LIMIT 20
|]


-- | Metadata of one recorded side; the media stays in the database until a viewer asks. Each
-- screenshot carries its label and pixel width, read from its PNG header.
data ProofSide = ProofSide {key :: Text, side :: Agent.RevisionSide, revision :: Text, exitCode :: Maybe Int32, browser :: Bool, chapters :: Aeson [(Double, Text)], shots :: Aeson [(Text, Maybe Int)]}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data ProofFormat = Gif | Mp4 | Cast
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (FromHttpApiData, HI.EncodeValue, ToHttpApiData) via WrappedEnumSC 'Nothing "" ProofFormat


-- | A side's recording in one format, or one of its numbered screenshots.
data ProofAsset = Recording ProofFormat | Screenshot Int16
  deriving stock (Eq, Show)


-- | One recorded side of a proof, named like @proof-2-head.mp4@, or a screenshot numbered from one
-- like @proof-2-head-1.png@.
--
-- >>> parseUrlPiece @ProofFile "proof-2-head.mp4" == Right (ProofFile "proof-2" Agent.Head (Recording Mp4))
-- True
-- >>> parseUrlPiece @ProofFile "proof-2-base-3.png" == Right (ProofFile "proof-2" Agent.Base (Screenshot 3))
-- True
-- >>> map toUrlPiece [ProofFile "proof-1" Agent.Base (Recording Gif), ProofFile "proof-1" Agent.Head (Screenshot 2)]
-- ["proof-1-base.gif","proof-1-head-2.png"]
-- >>> map (isLeft . parseUrlPiece @ProofFile) ["proof-1-side.gif", "proof-1-head.png", "proof-1-head-2.gif", "proof-1-head-5.png", "proof-x-head.gif", "proof-1"]
-- [True,True,True,True,True,True]
data ProofFile = ProofFile {key :: Text, side :: Agent.RevisionSide, asset :: ProofAsset}
  deriving stock (Eq, Show)


instance ToHttpApiData ProofFile where
  toUrlPiece file = file.key <> "-" <> toUrlPiece file.side <> case file.asset of Recording format -> "." <> toUrlPiece format; Screenshot n -> "-" <> show n <> ".png"


instance FromHttpApiData ProofFile where
  parseUrlPiece piece = do
    let (stem, extension) = first (T.dropEnd 1) $ T.breakOnEnd "." piece
    (n, side, asset) <- case (T.splitOn "-" stem, extension) of
      (["proof", n, side], _) -> (n,side,) . Recording <$> parseUrlPiece extension
      (["proof", n, side, shot], "png") -> (n,side,) . Screenshot <$> parseUrlPiece shot
      _ -> Left "Not a proof file"
    unless (not (T.null n) && T.all isDigit n && case asset of Screenshot i -> i `elem` [1 .. 4]; Recording _ -> True) $ Left "Not a proof file"
    ProofFile ("proof-" <> n) <$> parseUrlPiece side <*> pure asset


-- | Whether a proof's setup replayed the repository's saved recipe byte for byte, the measure of
-- how deterministic proofs are.
data SetupReuse = NoSetup | Reused | Changed
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.ToJSON, HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC 'Nothing "" SetupReuse


-- | Stores one proof's recordings, all sides or none, under the run's next unused key. Only the run
-- holding the review lease can record, so a superseded attempt cannot add media. The setup behind a
-- successful head recording becomes the repository's saved recipe.
saveProofs :: DB es => ReviewRun -> UUIDId "agent_attempt" -> Agent.ProofScript -> NonEmpty Agent.ProofMedia -> Eff es (Either Text (Text, SetupReuse))
saveProofs run attempt script sides = Hasql.transaction TxS.ReadCommitted TxS.Write do
  HI.OneColumn leased <-
    Hasql.oneTx
      [HI.sql|SELECT EXISTS (SELECT 1 FROM projects.pr_review_runs r JOIN projects.pr_review_threads t ON t.id = r.thread_id
    WHERE r.id = #{run.id} AND r.state = #{Reviewing} AND t.lease_owner = r.id AND t.lease_until > now())|]
  (used :: Int64, taken :: Int64) <- Hasql.oneTx [HI.sql|SELECT count(DISTINCT key), count(DISTINCT key) FILTER (WHERE attempt_id = #{attempt}) FROM projects.pr_review_proofs WHERE run_id = #{run.id}|]
  let key = "proof-" <> show (used + 1)
      full = taken >= fromIntegral Agent.proofBudget
  stored <-
    if not leased || full
      then pure []
      else forM (toList sides) \proof ->
        Hasql.queryTx @(Maybe (HI.OneColumn Bool))
          [HI.sql|WITH saved AS (INSERT INTO projects.pr_review_proofs (run_id, attempt_id, key, side, revision, exit_code, cast_bytes, gif, mp4, browser, chapters, script)
        VALUES (#{run.id}, #{attempt}, #{key}, #{proof.side}, #{proof.revision}, #{fromIntegral @Int @Int32 <$> proof.exitCode}, #{proof.cast}, #{proof.gif}, #{proof.mp4}, #{proof.browser}, #{Aeson proof.chapters}, #{Aeson script})
        ON CONFLICT (run_id, key, side) DO NOTHING RETURNING run_id, key, side
      ), shots AS (
        INSERT INTO projects.pr_review_proof_shots (run_id, key, side, n, label, png)
        SELECT saved.run_id, saved.key, saved.side, shot.n, shot.label, shot.png
        FROM saved, unnest(#{V.fromList (map fst proof.shots)}::text[], #{V.fromList (map snd proof.shots)}::bytea[]) WITH ORDINALITY AS shot(label, png, n)
      ) SELECT true FROM saved|]
  let setup = script.setup
      reachOf wanted = find ((== wanted) . (.side)) (toList sides) >>= (.setup)
      -- Older guests print no stage; a head that passed still proves the setup ran.
      headStage = maybe (bool Agent.SetupFailed Agent.SetupRan $ any (\p -> p.side == Agent.Head && p.exitCode == Just 0) sides) (.stage) (reachOf Agent.Head)
      -- The base runs on the prepared build, so its time is the setup's own cost rather than the PR's rebuild.
      seconds = (\h -> maybe h.seconds (.seconds) (mfilter ((== h.stage) . (.stage)) (reachOf Agent.Base))) <$> reachOf Agent.Head
      stage = fromIntegral @Int @Int16 $ fromEnum headStage
  if
    | not leased -> Left "Review authorization is no longer available" <$ Tx.condemn
    | full -> Left "Proof budget exhausted; cite the proofs already recorded" <$ Tx.condemn
    | all isJust stored ->
        Right
          . (key,)
          . HI.getOneColumn
          <$> Hasql.oneTx
            [HI.sql|WITH prior AS (
          SELECT setup FROM projects.pr_review_proof_recipes WHERE project_id = #{run.projectId} AND owner = #{run.owner} AND repo = #{run.repo}
        ), saved AS (
          INSERT INTO projects.pr_review_proof_recipes (project_id, owner, repo, setup, revision, run_id, stage, setup_seconds)
          SELECT #{run.projectId}, #{run.owner}, #{run.repo}, #{setup}, #{run.revision}, #{run.id}, #{stage}, #{fromIntegral @Int @Int32 <$> seconds}
          WHERE #{setup}::text IS NOT NULL AND #{headStage > Agent.SetupFailed}
          ON CONFLICT (project_id, owner, repo) DO UPDATE SET setup = EXCLUDED.setup, revision = EXCLUDED.revision, run_id = EXCLUDED.run_id, stage = EXCLUDED.stage, setup_seconds = EXCLUDED.setup_seconds, failure = NULL, updated_at = now()
          -- A setup that got further wins; once both start the app, the faster one does. Below that, a trivial
          -- terminal setup must not displace a full app recipe, and a browser proof's setup carries the app.
          WHERE EXCLUDED.stage > projects.pr_review_proof_recipes.stage
            OR (EXCLUDED.stage = projects.pr_review_proof_recipes.stage AND CASE WHEN EXCLUDED.stage >= 2
              THEN EXCLUDED.setup_seconds < coalesce(projects.pr_review_proof_recipes.setup_seconds, 2147483647)
              ELSE #{any (.browser) sides} OR length(EXCLUDED.setup) >= length(projects.pr_review_proof_recipes.setup) END)
          RETURNING 1
        ), healed AS (
          -- The saved setup working again clears the error kept for its repair.
          UPDATE projects.pr_review_proof_recipes SET failure = NULL
          WHERE project_id = #{run.projectId} AND owner = #{run.owner} AND repo = #{run.repo} AND setup = #{setup}
            AND #{headStage > Agent.SetupFailed} AND NOT EXISTS (SELECT 1 FROM saved)
        ) SELECT CASE WHEN #{setup}::text IS NULL THEN #{NoSetup} WHEN (SELECT setup FROM prior) = #{setup} THEN #{Reused} ELSE #{Changed} END|]
    | otherwise -> Left ("Proof " <> key <> " was already recorded; record the proof again") <$ Tx.condemn


-- | A repository's best setup so far: how far it got, how long it took on the prepared build, and its last error.
data ProofRecipe = ProofRecipe {setup :: Text, revision :: Text, stage :: Int16, setupSeconds :: Maybe Int32, failure :: Maybe Text}
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


-- | The latest earlier review of this pull request: its claims (without their proof keys) and the
-- scripts whose head recording passed, so a review of a new push re-proves them the same way.
data PriorReview = PriorReview {revision :: Text, claims :: AE.Value, scripts :: AE.Value}
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


priorReview :: DB es => ReviewRun -> Eff es (Maybe PriorReview)
priorReview run =
  Hasql.interpOne
    [HI.sql|SELECT p.revision,
      coalesce((SELECT jsonb_agg(c - 'proofKeys') FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p.result->'claims') = 'array' THEN p.result->'claims' ELSE '[]' END) c), '[]'),
      coalesce((SELECT jsonb_agg(DISTINCT s.script) FROM projects.pr_review_proofs s WHERE s.run_id = p.id AND s.side = 'head' AND s.exit_code = 0 AND s.script IS NOT NULL), '[]')
    FROM projects.pr_review_runs p JOIN projects.pr_review_runs r ON r.id = #{run.id}
    WHERE p.thread_id = r.thread_id AND p.revision <> r.revision AND p.created_at < r.created_at
      AND (jsonb_typeof(p.result->'claims') = 'array' OR EXISTS (SELECT 1 FROM projects.pr_review_proofs s WHERE s.run_id = p.id AND s.script IS NOT NULL))
    ORDER BY p.created_at DESC LIMIT 1|]


proofRecipe :: DB es => ReviewRun -> Eff es (Maybe ProofRecipe)
proofRecipe run = Hasql.interpOne [HI.sql|SELECT setup, revision, stage, setup_seconds, failure FROM projects.pr_review_proof_recipes WHERE project_id = #{run.projectId} AND owner = #{run.owner} AND repo = #{run.repo}|]


-- | Keeps a failed setup's error for the next attempt to repair: on the saved setup itself, or, before any
-- setup has worked, as the starting point the next review fixes rather than rewrites.
recordSetupFailure :: DB es => ReviewRun -> Text -> Text -> Eff es ()
recordSetupFailure run setup failure =
  Hasql.interpExecute_
    [HI.sql|INSERT INTO projects.pr_review_proof_recipes (project_id, owner, repo, setup, revision, run_id, stage, failure)
    VALUES (#{run.projectId}, #{run.owner}, #{run.repo}, #{setup}, #{run.revision}, #{run.id}, 0, #{T.take 2000 failure})
    ON CONFLICT (project_id, owner, repo) DO UPDATE SET failure = EXCLUDED.failure, setup = EXCLUDED.setup, revision = EXCLUDED.revision, run_id = EXCLUDED.run_id, updated_at = now()
    WHERE projects.pr_review_proof_recipes.setup = EXCLUDED.setup OR projects.pr_review_proof_recipes.stage = 0|]


proofSides :: DB es => ReviewRun -> UUIDId "agent_attempt" -> Eff es [ProofSide]
proofSides run attempt =
  Hasql.interp
    [HI.sql|SELECT key, side, revision, exit_code, browser, chapters,
      coalesce((SELECT jsonb_agg(jsonb_build_array(s.label, CASE WHEN length(s.png) >= 24 THEN (get_byte(s.png, 16) << 24) + (get_byte(s.png, 17) << 16) + (get_byte(s.png, 18) << 8) + get_byte(s.png, 19) END) ORDER BY s.n) FROM projects.pr_review_proof_shots s WHERE s.run_id = p.run_id AND s.key = p.key AND s.side = p.side), '[]')
    FROM projects.pr_review_proofs p WHERE run_id = #{run.id} AND attempt_id = #{attempt} ORDER BY key, side|]


-- | How far one planned claim has got: waiting, being recorded, recorded under a proof key, or failed.
data ClaimState = Pending | Running | Proved Text | Unrecorded
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data ClaimProgress = ClaimProgress {claim :: Text, state :: ClaimState}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | A plan keeps the state of the claims it repeats; a claim recorded outside the plan joins the end.
--
-- >>> let plan = advance (Agent.Planned ("A" :| ["B"])) []
-- >>> plan
-- [ClaimProgress {claim = "A", state = Pending},ClaimProgress {claim = "B", state = Pending}]
-- >>> advance (Agent.Recorded ("C" :| []) "proof-1") (advance (Agent.Started ("A" :| [])) plan)
-- [ClaimProgress {claim = "A", state = Running},ClaimProgress {claim = "B", state = Pending},ClaimProgress {claim = "C", state = Proved "proof-1"}]
-- >>> map (.state) $ advance (Agent.Recorded ("A" :| ["B"]) "proof-2") plan
-- [Proved "proof-2",Proved "proof-2"]
-- >>> advance (Agent.Planned ("B" :| ["A"])) (advance (Agent.Started ("A" :| [])) plan)
-- [ClaimProgress {claim = "B", state = Pending},ClaimProgress {claim = "A", state = Running}]
advance :: Agent.ProofProgress -> [ClaimProgress] -> [ClaimProgress]
advance event claims = case event of
  Agent.Planned planned -> [fromMaybe (ClaimProgress c Pending) (find ((== c) . (.claim)) claims) | c <- toList planned]
  Agent.Started covered -> mark covered Running
  Agent.Recorded covered key -> mark covered (Proved key)
  Agent.Unproved covered -> mark covered Unrecorded
  where
    mark covered next = [ClaimProgress p.claim (if p.claim `elem` covered then next else p.state) | p <- claims] <> [ClaimProgress c next | c <- toList covered, c `notElem` map (.claim) claims]


-- | Applies a proof event to the run's plan; one agent drives a run, so the read and write cannot race.
recordProgress :: DB es => ReviewRun -> Agent.ProofProgress -> Eff es [ClaimProgress]
recordProgress run event = do
  current <- fromMaybe [] <$> Hasql.interpOneJson [HI.sql|SELECT proof_progress FROM projects.pr_review_runs WHERE id = #{run.id}|]
  let next = advance event current
  next <$ Hasql.interpExecute_ [HI.sql|UPDATE projects.pr_review_runs SET proof_progress = #{Aeson next} WHERE id = #{run.id}|]


-- | The comment a run still owns: a newer run of the pull request takes it over by updating it or finishing.
progressComment :: DB es => ReviewRun -> Eff es (Maybe Int64)
progressComment run =
  (>>= HI.getOneColumn)
    <$> Hasql.interpOne
      [HI.sql|SELECT r.progress_comment_id FROM projects.pr_review_runs r WHERE r.id = #{run.id}
    AND NOT EXISTS (SELECT 1 FROM projects.pr_review_runs n WHERE n.thread_id = r.thread_id AND n.created_at > r.created_at
      AND (n.progress_comment_id = r.progress_comment_id OR n.state = #{Completed}))|]


setProgressComment :: DB es => ReviewRun -> Int64 -> Eff es ()
setProgressComment run cid = Hasql.interpExecute_ [HI.sql|UPDATE projects.pr_review_runs SET progress_comment_id = #{cid} WHERE id = #{run.id}|]


-- | The PR author's answer to "Did the proof get it right?".
data Rating = Up | Down
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (AE.FromJSON, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, ToHttpApiData) via WrappedEnumSC 'Nothing "" Rating


ratingSignedPath :: ReviewId -> Rating -> Text
ratingSignedPath rid rating = "rating:" <> rid.toText <> "/" <> toUrlPiece rating


rateRun :: DB es => ProjectId -> ReviewId -> Rating -> Eff es Bool
rateRun pid rid rating =
  (> 0)
    <$> Hasql.interpExecute
      [HI.sql|UPDATE projects.pr_review_runs r SET rating = #{rating}, rated_at = now()
      FROM projects.pr_review_threads t WHERE r.id = #{rid} AND t.id = r.thread_id AND t.project_id = #{pid}|]


-- | Sets the pull request's latest finished review aside for a member (@Just login@) or takes it back
-- (@Nothing@), returning that run so its comment can be redrawn. A dismissal also rates the proof down,
-- as feedback that it got the change wrong; taking it back clears that rating.
setDismissal :: DB es => Text -> Text -> Int -> Maybe Text -> Eff es (Maybe ReviewRun)
setDismissal owner repo number who = do
  updated <-
    Hasql.interpOne
      [HI.sql|UPDATE projects.pr_review_runs r SET dismissed_by = #{who}, rating = #{Down <$ who}, rated_at = now()
      WHERE r.id = (SELECT f.id FROM projects.pr_review_runs f JOIN projects.pr_review_threads t ON t.id = f.thread_id
        WHERE t.owner = #{owner} AND t.repo = #{repo} AND t.number = #{number} AND f.state = #{Completed} AND f.result IS NOT NULL
        ORDER BY f.finished_at DESC NULLS LAST LIMIT 1)
      RETURNING r.id|]
  join <$> traverse (getRun . HI.getOneColumn) updated


-- | What a proof media link signs; the prefix keeps it apart from widget signatures under the same key.
proofSignedPath :: ReviewId -> ProofFile -> Text
proofSignedPath rid file = "proof:" <> rid.toText <> "/" <> toUrlPiece file


proofFile :: DB es => ProjectId -> ReviewId -> ProofFile -> Eff es (Maybe ByteString)
proofFile pid rid file =
  fmap HI.getOneColumn
    <$> Hasql.interpOne
      [HI.sql|SELECT CASE #{format} WHEN 'gif' THEN p.gif WHEN 'mp4' THEN p.mp4 WHEN 'png' THEN s.png ELSE p.cast_bytes END
    FROM projects.pr_review_proofs p JOIN projects.pr_review_runs r ON r.id = p.run_id JOIN projects.pr_review_threads t ON t.id = r.thread_id
      LEFT JOIN projects.pr_review_proof_shots s ON s.run_id = p.run_id AND s.key = p.key AND s.side = p.side AND s.n = #{shot}
    WHERE t.project_id = #{pid} AND p.run_id = #{rid} AND p.key = #{file.key} AND p.side = #{file.side} AND (#{format} <> 'png' OR s.png IS NOT NULL)|]
  where
    (format, shot) = case file.asset of Recording f -> (toUrlPiece f, Nothing); Screenshot n -> ("png" :: Text, Just n)
