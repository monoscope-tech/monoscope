module BackgroundJobs.Types (BgJobs (..)) where

import Data.Aeson qualified as AE
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Issues qualified as Issues
import Models.Apis.PrometheusScrapeConfigs qualified as PromCfg
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId)
import Relude


data BgJobs
  = InviteUserToProject Projects.UserId Projects.ProjectId Text Text
  | CreatedProjectSuccessfully Projects.UserId Projects.ProjectId Text Text
  | SendDiscordData Projects.UserId Projects.ProjectId Text [Text] Text
  | NewAnomaly
      { projectId :: Projects.ProjectId
      , createdAt :: ZonedTime
      , anomalyType :: Text
      , anomalyAction :: Text
      , targetHashes :: [Text]
      }
  | DailyReports Projects.ProjectId
  | WeeklyReports Projects.ProjectId
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
  | ResetSlackSession Projects.ProjectId Projects.UserId (UUIDId "slack_event")
  | EnhanceIssuesWithLLM Projects.ProjectId (V.Vector Issues.IssueId)
  | ProcessIssuesEnhancement UTCTime
  | GitSyncFromRepo Projects.ProjectId
  | GitSyncPushDashboard Projects.ProjectId UUID.UUID -- projectId, dashboardId
  | GitSyncPushAllDashboards Projects.ProjectId -- Push all existing dashboards to repo
  | CompressReplaySessions
  | MergeReplaySession Projects.ProjectId UUID.UUID
  | ExpireReplayData
  | ExpireShareEvents
  | LogPatternPeriodicProcessing UTCTime Projects.ProjectId
  | LogPatternHourlyProcessing UTCTime Projects.ProjectId
  | ErrorBaselineCalculation Projects.ProjectId -- Calculate baselines for all errors in a project
  | ErrorSpikeDetection Projects.ProjectId -- Detect error spikes and create issues
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
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
