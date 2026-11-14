{-# LANGUAGE OrPatterns #-}

module BackgroundJobs.Types (BgJobs (..), Conversion (..), insertJob, wakeJob, longJob, reviewOperation, deferOperationReview, wakeStoppedReviews, startConversation, startCommand) where

import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Wreq qualified as HTTP
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Investigations qualified as Investigations
import Models.Apis.Issues qualified as Issues
import Models.Apis.PrometheusScrapeConfigs qualified as PromCfg
import Models.Ids qualified as Projects
import Pkg.DeriveUtils (DB, UUIDId)
import Relude


data BgJobs
  = InviteUserToProject Projects.UserId Projects.ProjectId Text Text
  | CreatedProjectSuccessfully Projects.UserId Projects.ProjectId Text Text
  | SendDiscordData Projects.UserId Projects.ProjectId Text [Text] Text
  | SyncProjectContacts Projects.ProjectId
  | NewAnomaly
      { projectId :: Projects.ProjectId
      , createdAt :: ZonedTime
      , anomalyType :: Text
      , anomalyAction :: Text
      , targetHashes :: [Text]
      }
  | DailyJob
  | HourlyJob UTCTime Int
  | ReportUsage Projects.ProjectId
  | TrialEndingReminder Projects.ProjectId Int
  | GenerateOtelFacetsBatch (V.Vector Projects.ProjectId) UTCTime
  | QueryMonitorsCheck
  | DeletedProject Projects.ProjectId
  | CleanupDemoProject
  | SlackNotification Projects.ProjectId Text
  | ProcessSlackEvent (UUIDId "slack_event")
  | RefreshSlackProgress (UUIDId "slack_progress")
  | RunConversation Projects.ProjectId (UUIDId "ai_routine_run")
  | AIRoutineRun (UUIDId "ai_routine") UTCTime
  | InvestigateIncident Projects.ProjectId (UUIDId "incident_event") (UUIDId "slack_incident_root")
  | -- | 'Issues.activitySql' writes this payload by hand; an integration test pins the encoding.
    DeliverSlackIncidents
  | ResetSlackSession Projects.ProjectId Projects.UserId (UUIDId "slack_event")
  | EnhanceIssuesWithLLM Projects.ProjectId (V.Vector Issues.IssueId)
  | ProcessIssuesEnhancement UTCTime
  | ReviewPullRequest (UUIDId "pr_review")
  | PrepareReviewWorkspace (UUIDId "pr_review")
  | FixIssue Projects.ProjectId (UUIDId "issue_fix")
  | AutofixIssues Projects.ProjectId
  | PublishFix Projects.ProjectId (UUIDId "issue_fix")
  | AnalyzeRepository Projects.ProjectId (UUIDId "repository")
  | RepositorySyncTick UTCTime
  | GitSyncFromRepo Projects.ProjectId
  | GitSyncRepository Projects.ProjectId (UUIDId "github_sync")
  | GitSyncPushDashboard Projects.ProjectId UUID.UUID -- projectId, dashboardId
  | GitSyncPushRepository Projects.ProjectId (UUIDId "github_sync")
  | GitSyncPushAllDashboards Projects.ProjectId -- Push all existing dashboards to repo
  | CompressReplaySessions
  | MergeReplaySession Projects.ProjectId UUID.UUID
  | ExpireReplayData
  | ExpireShareEvents
  | LogPatternPeriodicProcessing UTCTime Projects.ProjectId
  | LogPatternHourlyProcessing UTCTime Projects.ProjectId
  | ErrorBaselineCalculation Projects.ProjectId -- Calculate baselines for all errors in a project
  | ErrorSpikeDetection Projects.ProjectId -- Detect error spikes and create issues
  | PerformanceIssueDetection Projects.ProjectId -- N+1 and slow database queries over the last hour's spans
  | FrontendIssueDetection Projects.ProjectId -- Rage clicks over the last hour's browser interaction spans
  | FeedbackDetection Projects.ProjectId -- user.feedback spans/logs into feedback issues
  | ErrorAssigned Projects.ProjectId ErrorPatterns.ErrorPatternId Projects.UserId -- projectId, errorId, assigneeId
  | PatternEmbeddingAndMerge UTCTime Projects.ProjectId
  | ErrorGroupReview UTCTime Projects.ProjectId
  | EndpointTemplateDiscovery UTCTime Projects.ProjectId
  | EndpointMergeCleanup UTCTime Projects.ProjectId
  | -- | Per-minute dispatcher: atomically lease due Prometheus targets (SKIP LOCKED,
    -- multi-node safe) and fan out one PrometheusScrapeOne per target.
    PrometheusScrapeTick UTCTime
  | -- | Scrape a single Prometheus target; drained by workers across all pods.
    PrometheusScrapeOne PromCfg.PrometheusScrapeConfigId
  | -- | Five-minute dispatcher: fan out one 'ServiceMapRollup' per active project for the
    -- last closed bucket, so the expensive span self-join runs once per project per slice
    -- instead of once per service-map page view.
    ServiceMapRollupTick UTCTime
  | -- | Roll one closed 5-minute bucket of one project's spans into
    -- @service_dependency_edges_env@. Idempotent (upsert replaces), so re-running a bucket to
    -- absorb late-arriving spans is safe.
    ServiceMapRollup Projects.ProjectId UTCTime
  | MonoscopeAdminDaily
  | UsageAuditReport
  | -- | Hourly catch-up for rows the extraction worker missed. Re-drives rows
    -- where processed_at IS NULL through the same submitBatch path as live
    -- ingestion.
    SafetyNetReprocess Projects.ProjectId
  | -- | Daily host retention pass: bump last_seen_at from fresh traffic,
    -- system-archive hosts idle 30 days, wake system-archived hosts whose
    -- traffic returned and email the project a digest about them.
    HostRetentionSweep Projects.ProjectId
  | -- | Per-batch odd-job fired from the extraction worker's eager track, carrying
    -- the error vector it decoded in-memory. Separated from the worker so
    -- createJob failure aborts UPDATE-1 and safety-net retries the whole batch,
    -- and so per-error odd-jobs retry semantics are preserved.
    ProcessProjectErrorsJob Projects.ProjectId (V.Vector ErrorPatterns.ATError) UTCTime
  | -- | Periodic (every 10 min) safety net for notify eligibility. Picks up
    -- patterns whose inline notify on `ProcessProjectErrorsJob` was skipped
    -- (worker down, rate limit overflow, channel configured after the event)
    -- and re-enqueues itself. Bounded to patterns within the last 24h.
    NotificationSweepJob UTCTime
  | -- | Hourly flush of `apis.notification_digest_queue`. Re-enqueues itself.
    -- Delivers one digest message per project per channel summarising
    -- rate-limited + log-pattern issues from the past hour.
    NotificationDigestJob UTCTime
  | -- | Hourly external watchdog. Compares per-project TimescaleDB↔TimeFusion
    -- row counts for the last settled hour (alerts on TF drift/stall), checks
    -- the mainline + DLQ-replay consumer groups have live members, and reports
    -- fresh inflow into the DLQ / parking topics. Added after the 2026-07-06
    -- silent dual-write loss ran ~9h undetected. Alerts go to the admin
    -- Discord webhook; the whole job is best-effort (never blocks ingestion).
    InfraHealthCheck UTCTime
  | -- | Hourly: create integration dashboards (postgres, redis, k8s, …) for
    -- projects whose ingested metrics match a template's @discovery_metrics@
    -- prefixes. Once-ever per (project, template) via a marker table, so a
    -- user deleting an auto-created dashboard is a respected opt-out.
    DashboardsAutoProvision UTCTime
  | -- | Operator-triggered re-drive of the terminal parking topic
    -- (@<dlq>-parking@) back into the base DLQ so the (now-fixed) tiered
    -- replay re-attempts them. Reads up to @cap@ messages per run under a
    -- dedicated committing group and self-reschedules until drained. This is
    -- the sanctioned Haskell replacement for the ad-hoc parking_replay script;
    -- enqueue it manually (see docs/runbooks/dlq-recovery.md) — it is never
    -- auto-scheduled because parking is a forensic archive by design.
    ReplayParkedMessages Int
  | AdConversion Conversion
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | An ad conversion, sent to the ad platforms by 'Pkg.AdConversions.sendConversion'.
data Conversion = Signup Projects.UserId | Activated Projects.ProjectId
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Queue a job on the request's own DB connection: the odd-jobs row 'BackgroundJobs.enqueueJobs'
-- writes, for modules that cannot import 'BackgroundJobs' or hold no 'jobsPool'.
insertJob :: DB es => BgJobs -> Eff es ()
insertJob job = Hasql.interpExecute_ [HI.sql| INSERT INTO background_jobs (run_at, status, payload) VALUES (now(), 'queued', #{HI.AsJsonb job}) |]


wakeJob :: DB es => BgJobs -> Eff es ()
wakeJob = Hasql.transaction TxS.ReadCommitted TxS.Write . queueJobTx WakeNow


-- Retain the human decision and its continuation in one transaction.
reviewOperation :: (DB es, HTTP.HTTP :> es, Time.Time :> es) => Projects.ProjectId -> Projects.UserId -> UUIDId "agent_operation" -> Investigations.ReviewDecision -> Eff es Investigations.ReviewResponse
reviewOperation project = Investigations.decideOperationReviewWith (queueReviewTx project ResumeReview) project


deferOperationReview :: DB es => Projects.ProjectId -> UUIDId "agent_operation" -> UTCTime -> Eff es ()
deferOperationReview project identifier deadline = Hasql.transaction TxS.ReadCommitted TxS.Write $ queueReviewTx project (DeferReview deadline) identifier


wakeStoppedReviews :: DB es => Issues.RunStop -> Eff es ()
wakeStoppedReviews scope = Hasql.transaction TxS.ReadCommitted TxS.Write do
  let stopped = Issues.runStopScopeSql scope
  operations <-
    Hasql.queryTx @[(Projects.ProjectId, UUIDId "agent_operation")]
      [HI.sql|SELECT operation.project_id, operation.id FROM apis.ai_agent_operations operation
      JOIN apis.ai_routine_runs run ON run.id = operation.run_id AND run.project_id = operation.project_id
      JOIN apis.slack_events receipt ON receipt.id = CASE WHEN run.source #>> '{contents,tag}' = 'SlackMessage'
        THEN (run.source #>> '{contents,contents}')::uuid END
      WHERE run.source->>'tag' = 'TriggeredRun' AND receipt.processed_at IS NULL
        AND operation.review->>'tag' <> 'NoReview' AND ^{stopped}|]
  for_ operations $ \(project, identifier) -> queueReviewTx project FinishCancelledReview identifier


startConversation :: (DB es, Time.Time :> es) => Maybe Issues.PageContext -> Projects.ProjectId -> UUIDId "conversation" -> Projects.UserId -> UUIDId "ai_routine_run" -> Text -> Issues.ExecutionPolicy -> Issues.InteractiveOrigin -> Eff es Issues.InteractiveStart
startConversation pageContext project conversation user request =
  Issues.startMemberConversationRunWith
    ( \scheduled -> do
        queueJobTx WakeNow $ RunConversation project request
        for_ scheduled $ \routine -> queueJobTx (DeferUntil routine.scheduledAt) $ AIRoutineRun routine.id routine.scheduledAt
    )
    pageContext
    project
    conversation
    user
    request


startCommand :: (DB es, Time.Time :> es) => Issues.EventRoutine -> Issues.CommandOrigin -> UUIDId "conversation" -> Projects.UserId -> Text -> Eff es (Maybe Issues.EventRun)
startCommand routine =
  Issues.startCommandRunWith
    (queueJobTx WakeNow . RunConversation routine.projectId . (.id))
    routine


data ReviewWake = ResumeReview | DeferReview UTCTime | FinishCancelledReview


queueReviewTx :: Projects.ProjectId -> ReviewWake -> UUIDId "agent_operation" -> Tx.Transaction ()
queueReviewTx project wake identifier = do
  let (timing, waiting) = case wake of
        (ResumeReview; FinishCancelledReview) -> (WakeNow, mempty)
        DeferReview deadline -> (DeferUntil deadline, [HI.sql| AND operation.review->>'tag' = 'ReviewPending'|])
      (status, active) = case wake of
        FinishCancelledReview -> (Issues.RunCancelled, mempty)
        (ResumeReview; DeferReview{}) ->
          ( Issues.RunRunning
          , [HI.sql| AND routine.active
          AND (run.source <> #{Issues.ScheduledRun} OR routine.next_run_at = run.scheduled_at)|]
          )
  slots <-
    Hasql.queryTx @[(Issues.RoutineRunSource, UUIDId "ai_routine", UTCTime)]
      $ [HI.sql|SELECT run.source, run.routine_id, run.scheduled_at |]
      <> Investigations.operationRunSql project identifier
      <> [HI.sql| AND run.status = #{status}
      AND run.requested_by IS NOT DISTINCT FROM operation.requested_by
      AND EXISTS (SELECT 1 FROM apis.ai_routines routine WHERE routine.id = run.routine_id
        AND routine.project_id = run.project_id ^{active})|]
      <> waiting
      <> [HI.sql| FOR UPDATE OF operation|]
  requests <-
    Hasql.queryTx @[UUIDId "ai_routine_run"]
      $ [HI.sql|SELECT run.id |]
      <> Investigations.operationRunSql project identifier
      <> [HI.sql| AND run.status = #{status} AND (run.source->>'tag' = 'ConversationRun' OR (run.source->>'tag' = 'TriggeredRun' AND run.source #>> '{contents,tag}' = 'SlackCommand'))
      AND run.requested_by IS NOT DISTINCT FROM operation.requested_by|]
      <> waiting
      <> [HI.sql| FOR UPDATE OF operation|]
  for_ requests $ queueJobTx timing . RunConversation project
  for_ slots $ \(source, routine, scheduled) -> do
    let continuation = case source of
          Issues.ScheduledRun -> Just $ AIRoutineRun routine scheduled
          Issues.TriggeredRun (Issues.SlackMessage receipt) -> Just $ ProcessSlackEvent receipt
          Issues.TriggeredRun (Issues.PullRequestReview review) -> Just $ ReviewPullRequest review
          Issues.TriggeredRun (Issues.IssueEnrichment issue) -> Just $ EnhanceIssuesWithLLM project (V.singleton issue)
          Issues.TriggeredRun (Issues.IncidentEvent event root) -> Just $ InvestigateIncident project event root
          (Issues.FixRun{}; Issues.TriggeredRun{}; Issues.InteractiveRun{}; Issues.ConversationRun{}) -> Nothing
    for_ continuation $ queueJobTx timing


data JobTiming = WakeNow | DeferUntil UTCTime


-- | Jobs for the agent hosts' queue: reviews, fixes, their publication and workspace builds outlast the
-- main queue's timeout (which odd-jobs also treats as a stale lock), and the autofix sweep needs the agent
-- sandbox. Every other job is short, so a new constructor defaults to the main queue.
longJob :: BgJobs -> Bool
longJob = \case
  ReviewPullRequest{} -> True
  PrepareReviewWorkspace{} -> True
  FixIssue{} -> True
  PublishFix{} -> True
  AutofixIssues{} -> True
  _ -> False


queueJobTx :: JobTiming -> BgJobs -> Tx.Transaction ()
queueJobTx timing payload = do
  let job = HI.AsJsonb payload
      (at, bound) = case timing of
        WakeNow -> ([HI.sql|app_now()|], [HI.sql|LEAST(run_at, app_now())|])
        DeferUntil deadline -> ([HI.sql|#{deadline}|], [HI.sql|GREATEST(run_at, #{deadline})|])
      table = if longJob payload then [HI.sql|background_jobs_long|] else [HI.sql|background_jobs|]
  Hasql.executeTx
    [HI.sql|WITH ready AS (
      UPDATE ^{table} SET run_at = ^{bound}
      WHERE payload = #{job} AND status IN ('queued', 'retry') RETURNING id
    ) INSERT INTO ^{table} (run_at, status, payload)
      SELECT ^{at}, 'queued', #{job} WHERE NOT EXISTS (SELECT 1 FROM ready)|]
