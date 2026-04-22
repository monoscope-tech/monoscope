{-# LANGUAGE StrictData #-}

module BackgroundJobs (jobsWorkerInit, jobsRunner, processBackgroundJob, BgJobs (..), jobTypeName, runHourlyJob, generateOtelFacetsBatch, throwParsePayload, checkTriggeredQueryMonitors, monitorStatus, detectSpikeOrDrop, spikeZScoreThreshold, spikeMinAbsoluteDelta, calculateLogPatternBaselines, detectLogPatternSpikes, processNewLogPatterns, calculateErrorBaselines, detectErrorSpikes, notifyErrorSubscriptions, sweepErrorSubscriptions, consumeNotificationToken, endpointTemplateDiscovery, patternEmbeddingAndMerge, processEagerBatch, flushDrainTask, runErrorDecayFiber, runDrainFlusher, runDrainAgeFlushTimer, runSessionBackfillTimer, getStripeSubDetails, scheduleTrialReminders, StripeSubDetails (..)) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM.TBQueue (isFullTBQueue, readTBQueue, writeTBQueue)
import Control.Lens (view, (.~), (^.), (^?), _1, _3)
import Data.Aeson qualified as AE
import Data.Aeson.Lens qualified as AL
import Data.Aeson.QQ (aesonQQ)
import Data.Cache qualified as Cache
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.Hasql (Hasql, withHasqlTimefusion)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.LLM qualified as ELLM
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq qualified as W
import Data.Either qualified as Unsafe
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HashSet
import Data.List as L (partition)
import Data.List.Extra (chunksOf, groupBy)
import Data.Map.Strict qualified as Map
import Data.Ord (clamp)
import Data.Pool (withResource)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (DayOfWeek (Monday), UTCTime (utctDay, utctDayTime), ZonedTime, addUTCTime, dayOfWeek, formatTime, getZonedTime)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime), getCurrentTimeZone, utcToZonedTime, zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VA
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple qualified as SimplePG
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent.Async (concurrently, forConcurrently)
import Effectful.Ki qualified as Ki
import Effectful.Labeled (Labeled)
import Effectful.Log (Log)
import Effectful.Reader.Static (ask)
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Hasql.TH (resultlessStatement, singletonStatement)
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import Langchain.DocumentLoader.Core qualified
import Langchain.Embeddings.OpenAI qualified
import Log (LogLevel (..), Logger, runLogT)
import Log qualified as LogLegacy
import Lucid (Html)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Fields qualified as Fields
import Models.Apis.Integrations (PagerdutyData (..), getPagerdutyByProjectId)
import Models.Apis.IssueEnhancement qualified as Enhancement
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Apis.LogQueries qualified as LogQueries
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.PatternMerge qualified as PatternMergeDB
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitHub
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (SeverityLevel (..), generateSummary, insertSystemLog, mkSystemLog)
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.Wreq (defaults, getWith, header, postWith, responseBody)
import Network.Wreq qualified as Wreq
import OddJobs.ConfigBuilder (mkConfig)
import OddJobs.Job (ConcurrencyControl (..), Job (..), LogEvent, LogLevel, createJob, scheduleJob, startJobRunner, throwParsePayload)
import OpenTelemetry.Attributes qualified as OA
import OpenTelemetry.Trace (TracerProvider)
import Pages.Bots.Utils (ReportType (..))
import Pages.Charts.Charts qualified as Charts
import Pages.Replay qualified as Replay
import Pages.Reports qualified as RP
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (BaselineState (..), UUIDId (..), rawSql)
import Pkg.Drain qualified as Drain
import Pkg.EmailTemplates qualified as ET
import Pkg.ExtractionWorker qualified as ExtractionWorker
import Pkg.Mail (NotificationAlerts (..), RuntimeAlertType (..), sendDiscordAlert, sendDiscordAlertWith, sendPagerdutyAlertToService, sendRenderedEmail, sendSlackAlert, sendSlackAlertWith, sendSlackMessage, sendWhatsAppAlert)
import Pkg.Parser
import Pkg.PatternMerge qualified as PatternMerge
import Pkg.QueryCache qualified as QueryCache
import Pkg.TraceSessionCache qualified as TSC
import ProcessMessage (parseCanonicalPaths, processSpanToEntities, tokenizeUrlPath)
import PyF (fmtTrim)
import Relude hiding (ask)
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Config qualified as Config
import System.Logging qualified as Log
import System.Tracing (SpanStatus (..), Tracing, addEvent, setStatus, withSpan)
import System.Types (ATBackgroundCtx, DB, runBackground)
import UnliftIO.Exception (bracket, catch, throwIO, try, tryAny)
import Utils (formatUTC, freeTierDailyMaxEvents, toXXHash)


data BgJobs
  = InviteUserToProject Projects.UserId Projects.ProjectId Text Text
  | CreatedProjectSuccessfully Projects.UserId Projects.ProjectId Text Text
  | SendDiscordData Projects.UserId Projects.ProjectId Text [Text] Text
  | -- NewAnomaly Projects.ProjectId Anomalies.AnomalyTypes Anomalies.AnomalyActions TargetHash
    NewAnomaly
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
  | EndpointTemplateDiscovery UTCTime Projects.ProjectId
  | MonoscopeAdminDaily
  | UsageAuditReport
  | -- | Hourly catch-up for rows the extraction worker missed. Re-drives rows
    -- where processed_at IS NULL through the same submitBatch path as live
    -- ingestion.
    SafetyNetReprocess Projects.ProjectId
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
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


sendMessageToDiscord :: Text -> Text -> ATBackgroundCtx ()
sendMessageToDiscord msg webhookUrl = do
  let message = AE.object ["content" AE..= msg]
  let opts = defaults & header "Content-Type" .~ ["application/json"]
  response <- liftIO $ postWith opts (toString webhookUrl) message
  pass


-- | Get the constructor name of a BgJobs value
jobTypeName :: BgJobs -> Text
jobTypeName bgJob = T.takeWhile (/= ' ') $ toText $ show bgJob


jobsRunner :: Logger -> Config.AuthContext -> TracerProvider -> Job -> IO ()
jobsRunner logger authCtx tp job = Relude.when authCtx.config.enableBackgroundJobs $ do
  bgJob <- throwParsePayload job
  void $ runBackground logger authCtx tp $ do
    -- Create a span for the entire job execution
    let jobType = jobTypeName bgJob
    withSpan
      ("background_job." <> jobType)
      [ ("job.id", OA.toAttribute job.jobId)
      , ("job.type", OA.toAttribute jobType)
      , ("job.attempts", OA.toAttribute job.jobAttempts)
      ]
      $ \sp -> do
        -- Add start event
        addEvent sp "job.started" []

        -- Execute the job
        result <- try $ processBackgroundJob authCtx bgJob

        -- Set span status based on result, then re-throw so odd-jobs sees the failure
        case result of
          Left (e :: SomeException) -> do
            Log.logAttention "Background job failed" (show e)
            addEvent sp "job.failed" [("error", OA.toAttribute $ toText $ show e)]
            setStatus sp (Error $ toText $ show e)
            throwIO e
          Right _ -> do
            addEvent sp "job.completed" []
            setStatus sp Ok


unlessStale :: Text -> UTCTime -> NominalDiffTime -> ATBackgroundCtx () -> ATBackgroundCtx ()
unlessStale jobName scheduledTime buffer action = do
  now <- Time.currentTime
  bool action (Log.logTrace ("Skipping stale " <> jobName) scheduledTime) $ diffUTCTime now scheduledTime > buffer


-- | Self-chained ticker: enqueue the next tick so a mid-run failure still
-- produces a successor.
rescheduleSelf :: Config.AuthContext -> (UTCTime -> BgJobs) -> UTCTime -> ATBackgroundCtx ()
rescheduleSelf authCtx mkJob at =
  liftIO $ withResource authCtx.jobsPool \conn ->
    void $ scheduleJob conn "background_jobs" (mkJob at) at


-- | Process a background job - extracted so it can be run with different effect interpreters
processBackgroundJob :: Config.AuthContext -> BgJobs -> ATBackgroundCtx ()
processBackgroundJob authCtx bgJob =
  case bgJob of
    GenerateOtelFacetsBatch pids timestamp -> generateOtelFacetsBatch pids timestamp
    NewAnomaly{projectId, createdAt, anomalyType, anomalyAction, targetHashes} -> newAnomalyJob projectId createdAt anomalyType anomalyAction (V.fromList targetHashes)
    InviteUserToProject userId projectId reciever projectTitle' -> do
      userM <- Projects.userById userId
      whenJust userM \user -> do
        let projectUrl = authCtx.env.hostUrl <> "p/" <> projectId.toText
            (subj, html) = ET.projectInviteEmail user.firstName projectTitle' projectUrl
        sendRenderedEmail reciever subj (ET.renderEmail subj html)
    SendDiscordData userId projectId fullName stack foundUsFrom -> whenJustM (Projects.projectById projectId) \project -> do
      users <- Projects.usersByProjectId projectId
      let stackString = intercalate ", " $ map toString stack
      forM_ users \user -> do
        let userEmail = CI.original user.email
        let project_url = authCtx.env.hostUrl <> "p/" <> projectId.toText
        let project_title = project.title
        let msg =
              [fmtTrim|
  🎉 New project created on monoscope.tech! 🎉
  - **User Full Name**: {fullName}
  - **User Email**: {userEmail}
  - **Project Title**: [{project_title}]({project_url})
  - **User ID**: {userId.toText}
  - **Payment Plan**: {project.paymentPlan}
  - **Stack**: {stackString}
  - **Found us from**: {foundUsFrom}
  |]
        sendMessageToDiscord msg authCtx.config.discordWebhookUrl
    CreatedProjectSuccessfully userId projectId reciever projectTitle -> do
      userM <- Projects.userById userId
      whenJust userM \user -> do
        let projectUrl = authCtx.env.hostUrl <> "p/" <> projectId.toText
            (subj, html) = ET.projectCreatedEmail user.firstName projectTitle projectUrl
        sendRenderedEmail reciever subj (ET.renderEmail subj html)
    DeletedProject pid -> do
      users <- Projects.usersByProjectId pid
      projectM <- Projects.projectById pid
      forM_ projectM \pr -> forM_ users \user -> do
        let (subj, html) = ET.projectDeletedEmail user.firstName pr.title
        sendRenderedEmail (CI.original user.email) subj (ET.renderEmail subj html)
    ErrorAssigned pid errId assigneeId -> do
      errM <- ErrorPatterns.getErrorPatternById errId
      userM <- Projects.userById assigneeId
      projectM <- Projects.projectById pid
      case (projectM, errM, userM) of
        (Just project, Just err, Just user) | err.projectId == pid -> do
          let userEmail = CI.original user.email
              userName = if T.null user.firstName then userEmail else user.firstName
          issueM <- Issues.selectIssueByHash pid err.hash
          let (issueTitle, issuePath) = case issueM of
                Just issue -> (issue.title, issue.id.toText)
                Nothing -> (err.errorType <> ": " <> err.message, "by_hash/" <> err.hash)
              issueUrl = authCtx.env.hostUrl <> "p/" <> pid.toText <> "/issues/" <> issuePath
              projectTitle = project.title
              errorType = err.errorType
              errorMessage = err.message
              (subj, html) = ET.issueAssignedEmail userName projectTitle issueTitle issueUrl errorType errorMessage
          sendRenderedEmail userEmail subj (ET.renderEmail subj html)
        _ -> pass
    DailyJob ->
      unless authCtx.config.enableDailyJobScheduling (Log.logInfo "Daily job scheduling is disabled, skipping" ())
        >> Relude.when authCtx.config.enableDailyJobScheduling (withAdvisoryLock "daily_job_scheduling" runDailyJobScheduling)
      where
        -- Advisory lock for distributed coordination. Uses session-level locks which auto-release
        -- on connection close, providing a safety net if explicit unlock fails.
        withAdvisoryLock :: Text -> ATBackgroundCtx () -> ATBackgroundCtx ()
        withAdvisoryLock lockName action = do
          let acquireLock = Hasql.statement lockName [singletonStatement|SELECT pg_try_advisory_lock(hashtext($1 :: text)) :: bool|]
              releaseLock =
                ( Hasql.statement lockName [singletonStatement|SELECT pg_advisory_unlock(hashtext($1 :: text)) :: bool|] >>= \ok ->
                    Relude.unless ok
                      $ Log.logAttention
                        "Advisory unlock returned FALSE (lock was not held by this session)"
                        (AE.object ["lock_name" AE..= lockName, "released" AE..= ok])
                )
                  `catch` \(e :: SomeException) -> Log.logAttention "Failed to release advisory lock (will auto-release on disconnect)" (AE.object ["lock_name" AE..= lockName, "error" AE..= show e])
          bracket acquireLock (Relude.when ?? releaseLock) \acquired ->
            if acquired then action else Log.logInfo "Daily job already running in another pod, skipping" ()

        runDailyJobScheduling = do
          Log.logInfo "Running daily job" ()
          currentDay <- utctDay <$> Time.currentTime
          currentTime <- Time.currentTime
          -- Check if app-wide jobs already scheduled for today (idempotent check)
          hourlyJobsExist <-
            (>= (24 :: Int64))
              . HI.getOneColumn
              . HI.getOneRow
              <$> Hasql.interp
                [HI.sql|SELECT COUNT(*)::int8 FROM background_jobs
               WHERE payload->>'tag' = 'HourlyJob'
                 AND run_at >= date_trunc('day', #{currentTime}::timestamptz)
                 AND run_at < date_trunc('day', #{currentTime}::timestamptz) + interval '1 day'
                 AND status IN ('queued', 'locked')|]

          unless hourlyJobsExist $ do
            Log.logInfo "Scheduling hourly jobs for today" ()
            liftIO $ withResource authCtx.jobsPool \conn -> do
              void $ createJob conn "background_jobs" BackgroundJobs.MonoscopeAdminDaily
              void $ createJob conn "background_jobs" BackgroundJobs.UsageAuditReport
              -- 30-day replay retention sweep. The handler re-enqueues itself if
              -- it hit the batch cap, so backlog drains across multiple runs.
              void $ createJob conn "background_jobs" BackgroundJobs.ExpireReplayData
              void $ createJob conn "background_jobs" BackgroundJobs.ExpireShareEvents
              -- background job to cleanup demo project
              Relude.when (dayOfWeek currentDay == Monday) do
                void $ createJob conn "background_jobs" BackgroundJobs.CleanupDemoProject
              -- (count, stepSeconds, mkJob-from-index+time). Each handler re-enqueues
              -- itself; the daily loop seeds a full day's ticks so a single restart
              -- can't leave a gap when the self-chain was broken.
              let seed :: Int -> Int -> (UTCTime -> BgJobs) -> IO ()
                  seed count step mkJob = forM_ [0 .. count - 1] \i -> do
                    let at = addUTCTime (fromIntegral @Int $ i * step) currentTime
                    void $ scheduleJob conn "background_jobs" (mkJob at) at
              forM_ [0 .. 23 :: Int] \i -> do
                let at = addUTCTime (fromIntegral @Int $ i * 3600) currentTime
                void $ scheduleJob conn "background_jobs" (BackgroundJobs.HourlyJob at i) at
              seed 24 3600 BackgroundJobs.ProcessIssuesEnhancement
              seed 1440 60 (const BackgroundJobs.QueryMonitorsCheck)
              seed 144 600 BackgroundJobs.NotificationSweepJob
              seed 24 3600 BackgroundJobs.NotificationDigestJob

          Relude.when hourlyJobsExist
            $ Log.logInfo "Hourly jobs already scheduled for today, skipping" ()

          projects <- Projects.recentlyActiveProjectIds currentTime
          Log.logInfo "Scheduling jobs for projects" ("project_count", length projects)
          -- Only `SafetyNetReprocess` drives span derivation (hourly catch-up
          -- for the extraction worker's live path).
          let guardTag = "SafetyNetReprocess" :: Text
              guardThreshold = 24 :: Int64
          forM_ projects \p -> do
            let pTxt = p.toText
            projectJobsExistCount <-
              HI.getOneColumn
                . HI.getOneRow
                <$> Hasql.interp
                  [HI.sql|SELECT COUNT(*)::int8 FROM background_jobs
                 WHERE payload->>'tag' = #{guardTag}
                   AND payload->>'projectId' = #{pTxt}
                   AND run_at >= date_trunc('day', #{currentTime}::timestamptz)
                   AND run_at < date_trunc('day', #{currentTime}::timestamptz) + interval '1 day'
                   AND status IN ('queued', 'locked')|]
            let projectJobsExist = (projectJobsExistCount :: Int64) >= guardThreshold

            unless projectJobsExist
              $ liftIO
              $ withResource authCtx.jobsPool \conn -> do
                void $ createJob conn "background_jobs" $ BackgroundJobs.ReportUsage p
                let sched count secs mkJob = forM_ [0 .. count - 1] \i -> do
                      let t = addUTCTime (fromIntegral @Int $ i * secs) currentTime
                      void $ scheduleJob conn "background_jobs" (mkJob t) t
                -- Derived-table maintenance jobs read from `apis.*` tables, not
                -- the hypertable, and are unaffected by the derivation path.
                sched 96 900 (`PatternEmbeddingAndMerge` p)
                sched 96 900 (`LogPatternPeriodicProcessing` p)
                sched 4 21600 (`EndpointTemplateDiscovery` p)
                sched 24 3600 (`LogPatternHourlyProcessing` p)
                -- The extraction worker owns live span/log/error derivation.
                -- SafetyNetReprocess is the hourly catch-up for
                -- `processed_at IS NULL` rows (near-empty steady state).
                sched 24 3600 (\_ -> SafetyNetReprocess p)

            Relude.when projectJobsExist
              $ Log.logInfo "Jobs already scheduled for project today, skipping" ("project_id", p.toText)

            -- Weekly reports scheduled independently of per-project idempotency guard,
            -- so they aren't skipped when periodic jobs were already scheduled earlier.
            Relude.when (dayOfWeek currentDay == Monday) $ liftIO $ withResource authCtx.jobsPool \conn -> do
              existing <-
                SimplePG.query
                  conn
                  [sql|SELECT COUNT(*)::int FROM background_jobs
                       WHERE payload->>'tag' = 'WeeklyReports'
                         AND payload->>'projectId' = ?
                         AND run_at >= date_trunc('day', ?::timestamptz)
                         AND status IN ('queued', 'locked', 'retry')|]
                  (p, currentTime)
              let alreadyQueued = case existing of
                    [Only (c :: Int)] -> c > 0
                    _ -> False
              unless alreadyQueued $ void $ createJob conn "background_jobs" $ BackgroundJobs.WeeklyReports p
    HourlyJob scheduledTime hour -> unlessStale "HourlyJob" scheduledTime (2 * 3600) $ runHourlyJob scheduledTime hour
    DailyReports pid -> sendReportForProject pid DailyReport
    WeeklyReports pid -> sendReportForProject pid WeeklyReport
    ReportUsage pid -> whenJustM (Projects.projectById pid) \project -> do
      let provider = Projects.billingProvider project.subId
      Log.logInfo "Reporting usage" ("project_id", pid.toText, "plan", project.paymentPlan, "provider", show provider)
      Relude.when (project.paymentPlan /= "Free" && project.paymentPlan /= "ONBOARDING") $ whenJust (mfilter (not . T.null) project.firstSubItemId) \fSubId -> do
        currentTime <- liftIO getZonedTime
        totalToReport <- Telemetry.getTotalEventsToReport pid project.usageLastReported
        totalMetricsCount <- Telemetry.getTotalMetricsCount pid project.usageLastReported
        let totalUsage = totalToReport + totalMetricsCount
        Log.logInfo "Usage to report" ("project_id", pid.toText, "events", totalToReport, "metrics", totalMetricsCount, "total", totalUsage)
        Relude.when (totalUsage > 0) do
          -- Capture failures for structured logging; rethrow so odd-jobs retries.
          reportResult <- tryAny $ case provider of
            Projects.StripeProvider -> whenJust (mfilter (not . T.null) (project.customerId <|> project.orderId)) \custId ->
              liftIO $ reportUsageToStripe authCtx.config.stripeSecretKey custId "events_usage" totalUsage
            Projects.LemonSqueezyProvider -> liftIO $ reportUsageToLemonsqueezy fSubId totalUsage authCtx.config.lemonSqueezyApiKey
            Projects.NoBillingProvider -> Log.logAttention "ReportUsage: NoBillingProvider for paid project" ("project_id", pid.toText, "sub_id", project.subId, "plan", project.paymentPlan)
          case reportResult of
            Left e -> do
              Log.logAttention "Usage report FAILED — billing data may be missing" ("project_id", pid.toText, "provider", show provider :: Text, "total", totalUsage, "error", displayException e)
              throwIO e
            Right () ->
              Relude.when (provider /= Projects.NoBillingProvider) do
                void $ Projects.addDailyUsageReport pid totalUsage
                void $ Projects.updateUsageLastReported pid currentTime
                Log.logInfo "Usage reported successfully" ("project_id", pid.toText, "total", totalUsage)
    TrialEndingReminder pid scheduledDaysLeft -> do
      projectM <- Projects.projectById pid
      case projectM of
        Nothing -> Log.logAttention "TrialEndingReminder: project not found" (pid.toText, scheduledDaysLeft :: Int)
        Just project -> case project.subId of
          Nothing -> Log.logAttention "TrialEndingReminder: project has no subId" (pid.toText, scheduledDaysLeft :: Int)
          Just subId
            | Projects.billingProvider (Just subId) /= Projects.StripeProvider ->
                Log.logInfo "TrialEndingReminder: project migrated off Stripe, skipping" (pid.toText, scheduledDaysLeft :: Int, subId)
            | otherwise -> do
                details <- liftIO $ getStripeSubDetails authCtx.config.stripeSecretKey subId
                case details of
                  Nothing -> Log.logAttention "TrialEndingReminder: Stripe sub fetch failed" (pid.toText, scheduledDaysLeft :: Int, subId)
                  Just StripeSubDetails{status, trialEnd} -> case status of
                    "past_due" -> Log.logAttention "TrialEndingReminder: sub past_due at reminder time" (pid.toText, scheduledDaysLeft :: Int, subId)
                    "trialing" -> do
                      -- Re-derive days left from Stripe's current trial_end so email copy
                      -- reflects any mid-trial extension rather than the scheduled value.
                      now <- liftIO getCurrentTime
                      let actualDaysLeft = case trialEnd of
                            Just epoch ->
                              let secs = fromIntegral epoch - utcTimeToPOSIXSeconds now :: POSIXTime
                               in max 0 (ceiling (secs / 86400))
                            Nothing -> scheduledDaysLeft
                      users <- Projects.usersByProjectId pid
                      let billingUrl = authCtx.env.hostUrl <> "p/" <> pid.toText <> "/manage_billing"
                          (subj, html) = ET.trialEndingEmail project.title actualDaysLeft billingUrl
                      forM_ users \u -> do
                        sendRes <- tryAny $ sendRenderedEmail (CI.original u.email) subj (ET.renderEmail subj html)
                        whenLeft_ sendRes \e ->
                          Log.logAttention "TrialEndingReminder: email send failed" (pid.toText, CI.original u.email, displayException e)
                    _ -> Log.logInfo "TrialEndingReminder: sub not trialing, skipping" (pid.toText, scheduledDaysLeft :: Int, subId, status)
    CleanupDemoProject ->
      -- ReadCommitted is sufficient: we need atomicity across the three deletes,
      -- not Serializable's anti-conflict guarantees (which would surface as opaque
      -- retryable errors that the current Hasql retry path does not handle).
      Hasql.transaction TxS.ReadCommitted TxS.Write $ do
        let pid = UUID.nil
        Tx.statement pid [resultlessStatement|DELETE FROM projects.project_members WHERE project_id = $1 :: uuid|]
        Tx.statement pid [resultlessStatement|DELETE FROM tests.collections WHERE project_id = $1 :: uuid AND title != 'Default Health check'|]
        Tx.statement pid [resultlessStatement|DELETE FROM projects.project_api_keys WHERE project_id = $1 :: uuid AND title != 'Default API Key'|]
    SlackNotification pid message -> sendSlackMessage pid message
    EnhanceIssuesWithLLM pid issueIds -> enhanceIssuesWithLLM pid issueIds
    ProcessIssuesEnhancement scheduledTime -> unlessStale "ProcessIssuesEnhancement" scheduledTime (2 * 3600) $ processIssuesEnhancement scheduledTime
    GitSyncFromRepo pid -> gitSyncFromRepo pid
    GitSyncPushDashboard pid dashboardId -> gitSyncPushDashboard pid (UUIDId dashboardId)
    GitSyncPushAllDashboards pid -> gitSyncPushAllDashboards pid
    QueryMonitorsCheck -> checkTriggeredQueryMonitors
    CompressReplaySessions -> Replay.compressAndMergeReplaySessions
    MergeReplaySession pid sid -> Replay.mergeReplaySession pid sid
    ExpireReplayData -> Replay.expireOldReplayData
    -- 48h expiry + 30d grace so "Link expired" still renders before deletion.
    ExpireShareEvents ->
      Hasql.statement () [resultlessStatement|DELETE FROM apis.share_events WHERE created_at < now() - interval '30 days' - interval '48 hours'|]
    ErrorBaselineCalculation pid -> calculateErrorBaselines pid
    ErrorSpikeDetection pid -> detectErrorSpikes pid
    PatternEmbeddingAndMerge scheduledTime pid -> unlessStale "PatternEmbeddingAndMerge" scheduledTime (15 * 60) $ patternEmbeddingAndMerge pid
    EndpointTemplateDiscovery scheduledTime pid -> unlessStale "EndpointTemplateDiscovery" scheduledTime 3600 $ endpointTemplateDiscovery pid
    LogPatternPeriodicProcessing scheduledTime pid ->
      unlessStale "LogPatternPeriodicProcessing" scheduledTime (15 * 60)
        $ tryLog "detectLogPatternSpikes"
        $ detectLogPatternSpikes pid scheduledTime authCtx
    LogPatternHourlyProcessing _scheduledTime pid -> do
      tryLog "calculateLogPatternBaselines" $ calculateLogPatternBaselines pid
      tryLog "processNewLogPatterns" $ processNewLogPatterns pid authCtx
      tryLog "pruneStaleLogPatterns" $ pruneStaleLogPatterns pid
    SafetyNetReprocess pid -> safetyNetReprocess pid
    ProcessProjectErrorsJob pid errors now -> do
      -- Preserves today's legacy issue-creation + alert-dispatch semantics, now
      -- driven per-batch from the extraction worker instead of the 1-minute job.
      -- Odd-jobs retries apply if either sub-call throws.
      processProjectErrors pid errors now
      notifyErrorSubscriptions pid (V.uniq $ V.modify VA.sort $ V.map (.hash) errors)
    NotificationSweepJob scheduledTime -> do
      -- Re-enqueue first so a mid-tick failure still produces a next tick.
      rescheduleSelf authCtx BackgroundJobs.NotificationSweepJob (addUTCTime 600 scheduledTime)
      unlessStale "NotificationSweepJob" scheduledTime 1800 $ runNotificationSweep scheduledTime
    NotificationDigestJob scheduledTime -> do
      rescheduleSelf authCtx BackgroundJobs.NotificationDigestJob (addUTCTime 3600 scheduledTime)
      unlessStale "NotificationDigestJob" scheduledTime (2 * 3600) $ runNotificationDigest scheduledTime
    MonoscopeAdminDaily -> do
      now <- Time.currentTime
      let since = addUTCTime (-86400) now
          dateStr = toText $ formatTime defaultTimeLocale "%Y-%m-%d" now
          send msg = sendMessageToDiscord msg authCtx.config.discordWebhookUrl
          buildMessages [] msgs = msgs
          buildMessages items msgs =
            let (chunk, rest) = splitAccum 1800 items
             in buildMessages rest (msgs <> [unlines chunk])
          splitAccum _ [] = ([], [])
          splitAccum remaining (x : xs)
            | T.length x > remaining = ([], x : xs)
            | otherwise = let (taken, left) = splitAccum (remaining - T.length x - 1) xs in (x : taken, left)
          fmtNum n
            | n >= 1000000 = show (n `div` 1000) <> "K"
            | n >= 10000 = show (n `div` 1000) <> "K"
            | otherwise = show n

      -- Gather all projects and usage data
      allProjects <- Projects.activeProjects
      let projectMap = Map.fromList $ map (\(p :: Projects.Project) -> (p.id, p)) allProjects
          lookupTitle pid = maybe pid.toText (.title) $ Map.lookup pid projectMap

      allRows <- forM allProjects \(project :: Projects.Project) -> do
        events <- Telemetry.getTotalEventsToReport project.id since
        metrics <- Telemetry.getTotalMetricsCount project.id since
        pure (project, events, metrics, events + metrics)
      let (activeRows, inactiveRows) = partition (\(_, _, _, t) -> t > 0) allRows
          sorted = sortOn (\(_, _, _, t) -> negate t) activeRows

      -- Section 1: Usage summary
      let totalEvents = sum $ map (\(_, e, _, _) -> e) sorted
          totalMetrics = sum $ map (\(_, _, m, _) -> m) sorted
          summaryHeader = "**Daily Usage Summary** (" <> dateStr <> ")\n" <> show (length sorted) <> " active projects | " <> show totalEvents <> " events | " <> show totalMetrics <> " metrics"
          fmtRow (p, e, m, t) = T.justifyLeft 25 ' ' p.title <> T.justifyLeft 12 ' ' p.paymentPlan <> T.justifyRight 10 ' ' (show e) <> T.justifyRight 10 ' ' (show m) <> T.justifyRight 10 ' ' (show t)
          tableHeader = T.justifyLeft 25 ' ' "Project" <> T.justifyLeft 12 ' ' "Plan" <> T.justifyRight 10 ' ' "Events" <> T.justifyRight 10 ' ' "Metrics" <> T.justifyRight 10 ' ' "Total"
          tableBody = tableHeader : map fmtRow sorted
      send summaryHeader
      send $ "```\n" <> unlines tableBody <> "```"

      -- Section 2: New projects (created in last 24h)
      tryLog "newProjects" do
        newProjects <- Projects.newProjectsSince since
        let fmtNew (p :: Projects.Project) =
              let hoursAgo = show (round (diffUTCTime now p.createdAt / 3600) :: Int)
               in "- " <> p.title <> " (" <> p.paymentPlan <> ") -- created " <> hoursAgo <> "h ago"
        send
          $ bool
            ("**New Projects** (" <> show (length newProjects) <> ")\n" <> unlines (map fmtNew newProjects))
            "**New Projects**: None"
            (null newProjects)

      -- Section 3: Inactive projects (non-ONBOARDING with 0 events)
      tryLog "churnSignals" do
        let churn = filter (\(p, _, _, _) -> p.paymentPlan /= "ONBOARDING") inactiveRows
        unless (null churn)
          $ send
          $ "**Inactive Projects** ("
          <> show (length churn)
          <> " with 0 events in 24h)\n"
          <> unlines (map (\(p, _, _, _) -> "- " <> p.title <> " (" <> p.paymentPlan <> ")") churn)

      -- Section 4: New issues
      tryLog "newIssues" do
        issueCounts :: [(Projects.ProjectId, Text, Int)] <-
          Hasql.interp
            [HI.sql|SELECT project_id::uuid, issue_type, COUNT(*)::int FROM apis.issues WHERE created_at > #{since}::timestamptz GROUP BY project_id, issue_type ORDER BY COUNT(*) DESC|]
        unless (null issueCounts) do
          let total = sum $ map (view _3) issueCounts
              byProject = Map.toDescList $ Map.fromListWith (<>) $ map (\(pid, itype, cnt) -> (pid, [(itype, cnt)])) issueCounts
              projectCount = length byProject
              fmtProject (pid, types) =
                let pTotal = sum $ map snd types
                    details = T.intercalate ", " $ map (\(t, c) -> t <> ": " <> show c) types
                 in "- " <> lookupTitle pid <> ": " <> show pTotal <> " (" <> details <> ")"
          send
            $ "**New Issues** ("
            <> show total
            <> " across "
            <> show projectCount
            <> " projects)\n"
            <> unlines (map fmtProject $ take 10 byProject)

      -- Section 5: Monitor alerts
      tryLog "monitorAlerts" do
        alertCounts :: [(Projects.ProjectId, Text, Int)] <-
          Hasql.interp
            [HI.sql|SELECT m.project_id::uuid, m.current_status, COUNT(*)::int FROM monitors.query_monitors m
               WHERE m.deactivated_at IS NULL AND m.deleted_at IS NULL AND m.current_status != 'normal'
               GROUP BY m.project_id, m.current_status|]
        unless (null alertCounts) do
          let totalAlerting = sum [c | (_, s, c) <- alertCounts, s == "alerting"]
              totalWarning = sum [c | (_, s, c) <- alertCounts, s == "warning"]
              byProject = Map.toDescList $ Map.fromListWith (<>) $ map (\(pid, st, cnt) -> (pid, [(st, cnt)])) alertCounts
              fmtProject (pid, statuses) =
                "- " <> lookupTitle pid <> ": " <> T.intercalate ", " (map (\(s, c) -> show c <> " " <> s) statuses)
          send
            $ "**Monitor Alerts**\n"
            <> show totalAlerting
            <> " alerting | "
            <> show totalWarning
            <> " warning across "
            <> show (length byProject)
            <> " projects\n"
            <> unlines (map fmtProject $ take 10 byProject)

      -- Section 6: Background job health
      tryLog "jobHealth" do
        jobStats :: [(Text, Int)] <- Hasql.interp [HI.sql|SELECT status, COUNT(*)::int FROM background_jobs WHERE created_at >= #{since}::timestamptz GROUP BY status|]
        let stuckThreshold = addUTCTime (-1800) now
        stuckJobs :: [Int] <- Hasql.interp [HI.sql|SELECT COUNT(*)::int FROM background_jobs WHERE status = 'locked' AND locked_at < #{stuckThreshold}::timestamptz|]
        let statsLine = T.intercalate " | " $ map (\(s, c) -> s <> ": " <> show c) jobStats
            stuck = fromMaybe 0 $ listToMaybe stuckJobs
        send
          $ "**Job Health** (last 24h)\n"
          <> statsLine
          <> bool ("\n!! " <> show stuck <> " stuck jobs (locked > 30min)") "" (stuck == 0)

      -- Section 7: Top growing projects (day-over-day)
      tryLog "topGrowing" do
        growthData :: [(Projects.ProjectId, Int, Int)] <-
          Hasql.interp
            [HI.sql|SELECT du.project_id::uuid,
                 COALESCE(SUM(CASE WHEN du.created_at >= (#{now}::date - interval '1 day') THEN du.total_requests ELSE 0 END), 0)::int as yesterday,
                 COALESCE(SUM(CASE WHEN du.created_at < (#{now}::date - interval '1 day') AND du.created_at >= (#{now}::date - interval '2 days') THEN du.total_requests ELSE 0 END), 0)::int as day_before
               FROM apis.daily_usage du WHERE du.created_at >= (#{now}::date - interval '2 days')
               GROUP BY du.project_id|]
        let withGrowth = sortOn (\(_, _, _, g) -> negate g) [(pid, y, db, y - db) | (pid, y, db) <- growthData, y > db, db > 0]
            fmtGrowth (pid, y, db, _) =
              let pct = show (round @Double @Int $ fromIntegral (y - db) / fromIntegral db * 100)
               in "- " <> lookupTitle pid <> ": " <> fmtNum db <> " -> " <> fmtNum y <> " (+" <> pct <> "%)"
        unless (null withGrowth)
          $ send
          $ "**Top Growing** (vs previous day)\n"
          <> unlines (map fmtGrowth $ take 5 withGrowth)

      -- Section 8: Active users (last 24h)
      tryLog "activeUsers" do
        activeUsers :: [(Text, Text, Text, V.Vector Projects.ProjectId)] <-
          Hasql.interp
            [HI.sql|SELECT u.email, u.first_name, u.last_name, array_agg(DISTINCT pm.project_id)::uuid[] as project_ids
               FROM users.persistent_sessions ps
               JOIN users.users u ON u.id = ps.user_id
               LEFT JOIN projects.project_members pm ON pm.user_id = ps.user_id AND pm.active = TRUE
               WHERE ps.updated_at >= #{since}::timestamptz
               GROUP BY u.id, u.email, u.first_name, u.last_name ORDER BY u.email|]
        let fmtUser (email, _, _, pidsVec) =
              let pids = V.toList pidsVec
                  projs = T.intercalate ", " $ map lookupTitle $ filter (`Map.member` projectMap) pids
               in "- " <> email <> bool (" -- " <> projs) "" (T.null projs)
        send
          $ "**Active Users** ("
          <> show (length activeUsers)
          <> " in last 24h)\n"
          <> unlines (map fmtUser activeUsers)

      -- Section 9: Project links
      let linkRow (p, _, _, _) = "- [" <> p.title <> "](" <> authCtx.env.hostUrl <> "p/" <> p.id.toText <> ")"
          links = map linkRow sorted
      forM_ (buildMessages links ([] :: [Text])) send
      Log.logInfo "Sent daily admin summary to Discord" ("project_count", length sorted)
    UsageAuditReport -> do
      now <- Time.currentTime
      let since = addUTCTime (-86400) now
          send msg = sendMessageToDiscord msg authCtx.config.discordWebhookUrl
      -- Paying projects only; cross-check reported (apis.daily_usage) vs ingested (otel + metrics) over last 24h.
      rows :: [(Projects.ProjectId, Text, Text, Int, Int)] <-
        Hasql.interp
          [HI.sql|
            WITH reported AS (
              SELECT project_id::uuid AS pid, COALESCE(SUM(total_requests),0)::int AS r
                FROM apis.daily_usage
               WHERE created_at >= #{since}::timestamptz
               GROUP BY project_id
            ),
            ingested_events AS (
              SELECT project_id::uuid AS pid, COUNT(*)::int AS c
                FROM public.otel_logs_and_spans
               WHERE timestamp >= #{since}::timestamptz
               GROUP BY project_id
            ),
            ingested_metrics AS (
              SELECT project_id::uuid AS pid, COUNT(*)::int AS c
                FROM telemetry.metrics
               WHERE time >= #{since}::timestamptz
               GROUP BY project_id
            )
            SELECT p.id::uuid, p.title, p.payment_plan,
                   COALESCE(r.r, 0)::int AS reported,
                   (COALESCE(ie.c, 0) + COALESCE(im.c, 0))::int AS ingested
              FROM projects.projects p
              LEFT JOIN reported r ON r.pid = p.id
              LEFT JOIN ingested_events ie ON ie.pid = p.id
              LEFT JOIN ingested_metrics im ON im.pid = p.id
             WHERE p.payment_plan NOT IN ('Free','ONBOARDING','Bring nothing')
               AND p.deleted_at IS NULL
          |]
      let mismatches =
            [ (pid, title, plan, reported, ingested, reported - ingested, pct)
            | (pid, title, plan, reported, ingested) <- rows
            , let denom = max 1 (max reported ingested)
            , let pct = round @Double @Int (fromIntegral (abs (reported - ingested)) / fromIntegral denom * 100)
            , ingested > 100 || reported > 100
            , pct > 5
            ]
          sorted = sortOn (\(_, _, _, _, _, _, pct) -> negate pct) mismatches
      forM_ sorted \(pid, title, plan, reported, ingested, delta, pct) ->
        Log.logAttention "usage_audit_mismatch" (pid.toText, title, plan, reported, ingested, delta, pct)
      unless (null sorted) do
        let fmtRow (_, title, plan, reported, ingested, delta, pct) =
              T.justifyLeft 25 ' ' title
                <> T.justifyLeft 12 ' ' plan
                <> T.justifyRight 10 ' ' (show reported)
                <> T.justifyRight 10 ' ' (show ingested)
                <> T.justifyRight 10 ' ' (show delta)
                <> T.justifyRight 6 ' ' (show pct <> "%")
            hdr =
              T.justifyLeft 25 ' ' "Project"
                <> T.justifyLeft 12 ' ' "Plan"
                <> T.justifyRight 10 ' ' "Reported"
                <> T.justifyRight 10 ' ' "Ingested"
                <> T.justifyRight 10 ' ' "Delta"
                <> T.justifyRight 6 ' ' "Pct"
            top = take 20 sorted
            heading =
              "**Usage Audit** (last 24h) — "
                <> show (length sorted)
                <> " projects with >5% drift"
                <> if length sorted > length top then " (showing top " <> show (length top) <> ")" else ""
        send heading
        send $ "```\n" <> hdr <> "\n" <> unlines (map fmtRow top) <> "```"
      Log.logInfo "Usage audit complete" ("mismatch_count", length sorted)


tryLog :: Text -> ATBackgroundCtx () -> ATBackgroundCtx ()
tryLog label = (`catch` \(e :: SomeException) -> Log.logAttention ("LogPattern pipeline step failed: " <> label) (show e))


-- | Run hourly scheduled tasks for all projects
runHourlyJob :: UTCTime -> Int -> ATBackgroundCtx ()
runHourlyJob scheduledTime hour = do
  ctx <- ask @Config.AuthContext
  let oneHourAgo = addUTCTime (-3600) scheduledTime
  activeProjects <-
    Hasql.interp
      [HI.sql| SELECT DISTINCT project_id::uuid
              FROM otel_logs_and_spans ols
              WHERE ols.timestamp >= #{oneHourAgo}::timestamptz
                AND ols.timestamp <= #{scheduledTime}::timestamptz |]

  Log.logTrace "Projects with new data in the last hour window" ("count", AE.toJSON $ length activeProjects)
  let projectBatches = chunksOf 10 activeProjects

  liftIO $ withResource ctx.jobsPool \conn -> do
    forM_ projectBatches \batch ->
      createJob conn "background_jobs" $ BackgroundJobs.GenerateOtelFacetsBatch (V.fromList batch) scheduledTime
    forM_ activeProjects \pid -> do
      void $ createJob conn "background_jobs" $ ErrorBaselineCalculation pid
      void $ createJob conn "background_jobs" $ ErrorSpikeDetection pid

  -- Cleanup expired query cache entries
  deletedCount <- QueryCache.cleanupExpiredCache
  Relude.when (deletedCount > 0) $ Log.logInfo "Cleaned up expired query cache entries" ("deleted_count", AE.toJSON deletedCount)

  deviceCodesDeleted <- Hasql.interpExecute [HI.sql| DELETE FROM users.device_auth_codes WHERE expires_at < now() - interval '1 hour' |]
  Relude.when (deviceCodesDeleted > 0) $ Log.logInfo "Cleaned up expired device auth codes" ("deleted_count", AE.toJSON deviceCodesDeleted)

  staleMetricsDeleted <- Hasql.interpExecute [HI.sql| DELETE FROM telemetry.metrics_meta WHERE updated_at < now() - interval '3 months' |]
  Relude.when (staleMetricsDeleted > 0) $ Log.logInfo "Cleaned up stale metrics metadata" ("deleted_count", AE.toJSON staleMetricsDeleted)

  liftIO $ withResource ctx.jobsPool \conn ->
    void $ createJob conn "background_jobs" BackgroundJobs.CompressReplaySessions

  checkFreeTierUsageNotifications activeProjects scheduledTime

  Log.logTrace "Completed hourly job scheduling for hour" ("hour", AE.toJSON hour)


-- | For each active free-tier project, check usage and send system log + email at 80% and 100% thresholds.
-- Deduplication: only notifies if no system log with the same event name exists for this project in the last 24h.
checkFreeTierUsageNotifications :: [Projects.ProjectId] -> UTCTime -> ATBackgroundCtx ()
checkFreeTierUsageNotifications pids now = forM_ pids \pid -> tryLog "free-tier-check" do
  projectM <- Projects.projectById pid
  forM_ projectM \project -> Relude.when (project.paymentPlan == "Free") do
    -- Use the TTL-cached daily event count to avoid a full 24-hour count(*) scan.
    cacheM <- Projects.projectCacheById pid
    let count = maybe 0 (.dailyEventCount) cacheM :: Int
    let limit = fromInteger freeTierDailyMaxEvents
        exceeded = count >= limit
        warning = count >= (limit * 80) `div` 100
        eventName :: Text = if exceeded then "system.free_tier.exceeded" else "system.free_tier.warning"
    Relude.when (warning || exceeded) do
      -- Dedup: skip if we already logged this event for this project today
      alreadyNotified <-
        let pidText = pid.toText
         in fromMaybe False
              <$> Hasql.interpOne
                [HI.sql| SELECT EXISTS(SELECT 1 FROM otel_logs_and_spans WHERE project_id=#{pidText} AND name=#{eventName} AND timestamp > #{now}::timestamptz - interval '1 day') |]
      Relude.unless alreadyNotified do
        ctx <- ask @Config.AuthContext
        let sev = if exceeded then SLError else SLWarn
            bodyMsg = if exceeded then "Daily event limit reached — new events are being dropped." else "Approaching daily event limit (" <> show count <> " of " <> show limit <> " events used)."
            attrs = Map.fromList [("used", AE.toJSON count), ("limit", AE.toJSON limit), ("payment_plan", AE.String "Free")]
        insertSystemLog ctx.env.enableTimefusionWrites $ mkSystemLog pid eventName sev bodyMsg attrs Nothing now
        -- Send email to all project members
        users <- Projects.usersByProjectId pid
        let billingUrl = ctx.env.hostUrl <> "p/" <> pid.toText <> "/manage_billing"
            (subj, html) = ET.freeTierUsageEmail project.title billingUrl count limit exceeded
        forM_ users \user -> sendRenderedEmail (CI.original user.email) subj (ET.renderEmail subj html)


generateOtelFacetsBatch :: (DB es, Effectful.Reader.Static.Reader Config.AuthContext :> es, Ki.StructuredConcurrency :> es, Labeled "timefusion" Hasql :> es, Log :> es, Tracing :> es, UUID.UUIDEff :> es) => V.Vector Projects.ProjectId -> UTCTime -> Eff es ()
generateOtelFacetsBatch projectIds timestamp = do
  ctx <- ask @Config.AuthContext
  let enableTfReads = ctx.env.enableTimefusionReads
  Log.logTrace "Starting batch OTLP facets generation" ("project_count", AE.toJSON $ V.length projectIds)

  -- Process projects concurrently with individual error handling
  results <- Ki.scoped \scope -> do
    threads <- forM projectIds \pid -> Ki.fork scope $ do
      -- Wrap each project's facet generation in a span
      withSpan
        "facet_generation.project"
        [ ("project_id", OA.toAttribute pid.toText)
        , ("batch_size", OA.toAttribute $ V.length projectIds)
        ]
        $ \sp -> do
          addEvent sp "facet_generation.started" []
          result <- try $ Fields.generateAndSaveFacets enableTfReads pid "otel_logs_and_spans" 50 timestamp
          case result of
            Left (e :: SomeException) -> do
              addEvent sp "facet_generation.failed" [("error", OA.toAttribute $ toText $ show e)]
              setStatus sp (Error $ toText $ show e)
              pure $ Left (pid, show e)
            Right _ -> do
              addEvent sp "facet_generation.completed" []
              setStatus sp Ok
              pure $ Right pid
    traverse (Ki.atomically . Ki.await) threads

  let successes = V.length $ V.filter isRight results
      failures = V.length $ V.filter isLeft results

  Log.logTrace "Completed batch OTLP facets generation"
    $ AE.object
      [ "total_projects" AE..= V.length projectIds
      , "successes" AE..= successes
      , "failures" AE..= failures
      ]


-- | Input for Drain tree processing.
-- SeedPattern: re-inserts existing templates with no sample (Nothing). When a real log
-- later matches, updateLogGroupWithTemplate replaces the sample with actual content.
data DrainInput = SeedPattern Text | NewEvent Text Text


processBatch :: Bool -> V.Vector DrainInput -> UTCTime -> Drain.DrainTree -> Drain.DrainTree
processBatch isSummary batch now initial = fst $ processBatchWithMapping isSummary batch now initial


processBatchWithMapping :: Bool -> V.Vector DrainInput -> UTCTime -> Drain.DrainTree -> (Drain.DrainTree, V.Vector (Text, Text))
processBatchWithMapping isSummary batch now initial = Drain.buildDrainTreeWithMapping tokenize logId sampleContent initial batch now
  where
    tok = if isSummary then Drain.generateSummaryDrainTokens else Drain.generateDrainTokens
    tokenize = tok . \case SeedPattern c -> c; NewEvent _ c -> c
    logId = \case SeedPattern _ -> ""; NewEvent lid _ -> lid
    sampleContent = \case SeedPattern _ -> Nothing; NewEvent _ c -> Just c


-- | Process errors from OpenTelemetry spans to detect runtime exceptions
-- This job runs every minute to extract errors from span data and create
-- runtime exception issues for tracking and notification.
--
-- Error Detection Strategy:
-- 1. Query spans with error indicators (status, events, attributes)
-- 2. Extract error details from span events using OpenTelemetry conventions
data ErrorSubscriptionDue = ErrorSubscriptionDue
  { errorId :: ErrorPatterns.ErrorPatternId
  , errorData :: ErrorPatterns.ATError
  , errorState :: ErrorPatterns.ErrorState
  , issueId :: Issues.IssueId
  , issueTitle :: Text
  , slackThreadTs :: Maybe Text
  , discordMessageId :: Maybe Text
  , occurrences1h :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData)


-- | Send alerts to all configured notification channels for a project, threading Slack/Discord messages.
sendAlertToChannels
  :: NotificationAlerts
  -> Projects.ProjectId
  -> Projects.Project
  -> [Projects.User]
  -> Text
  -> Text
  -> Html ()
  -> (Maybe Text, Maybe Text)
  -> ATBackgroundCtx (Maybe Text, Maybe Text)
sendAlertToChannels alert pid project users alertUrl subj html (initSlackTs, initDiscordMsgId) =
  foldlM
    ( \(slackTs, discordMsgId) -> \case
        Projects.NSlack -> do
          tsM <- sendSlackAlertWith slackTs alert pid project.title Nothing
          pure (slackTs <|> tsM, discordMsgId)
        Projects.NDiscord -> do
          msgIdM <- sendDiscordAlertWith discordMsgId alert pid project.title Nothing
          pure (slackTs, discordMsgId <|> msgIdM)
        Projects.NPhone -> (slackTs, discordMsgId) <$ sendWhatsAppAlert alert pid project.title project.whatsappNumbers
        Projects.NEmail -> let rendered = ET.renderEmail subj html in (slackTs, discordMsgId) <$ forM_ users \u -> sendRenderedEmail (CI.original u.email) subj rendered
        Projects.NPagerduty -> (slackTs, discordMsgId) <$ (getPagerdutyByProjectId pid >>= traverse_ \pd -> sendPagerdutyAlertToService pd.integrationKey alert project.title alertUrl)
    )
    (initSlackTs, initDiscordMsgId)
    project.notificationsChannel


trendChartUrl :: Log :> es => Config.AuthContext -> Projects.ProjectId -> Widget.Widget -> Text -> Text -> Eff es (Maybe Text)
trendChartUrl ctx pid widget fromTxt toTxt =
  mfilter (not . T.null)
    . Just
    <$> Widget.widgetPngUrl ctx.env.apiKeyEncryptionSecretKey ctx.env.hostUrl pid widget Nothing (Just fromTxt) (Just toTxt)


errorTrendChartUrl :: Log :> es => Config.AuthContext -> Projects.ProjectId -> Text -> Text -> Text -> Eff es (Maybe Text)
errorTrendChartUrl ctx pid errHash =
  trendChartUrl ctx pid def{Widget.wType = Widget.WTTimeseries, Widget.query = Just $ "hashes has_any [\"err:" <> errHash <> "\"] | summarize count(*) by bin_auto(timestamp)", Widget.theme = Just "roma"}


-- | Compact relative time ("3m ago", "2h ago", "5d ago") for alert timestamps.
-- Used in Slack/Discord notifications where readers need to judge recency at a glance.
relTimeAgo :: UTCTime -> UTCTime -> Text
relTimeAgo now t =
  let s = max 0 $ round (diffUTCTime now t) :: Int
   in if s < 60
        then "just now"
        else
          if s < 3600
            then show (s `div` 60) <> "m ago"
            else
              if s < 86400
                then show (s `div` 3600) <> "h ago"
                else show (s `div` 86400) <> "d ago"


monitorTrendChartUrl :: Log :> es => Config.AuthContext -> Projects.ProjectId -> Monitors.QueryMonitor -> Text -> Text -> Eff es (Maybe Text)
monitorTrendChartUrl ctx pid monitor =
  trendChartUrl ctx pid def{Widget.wType = Widget.WTTimeseries, Widget.query = Just monitor.logQuery, Widget.alertThreshold = Just monitor.alertThreshold, Widget.warningThreshold = monitor.warningThreshold}


-- | Fast inline path after ingestion: dispatch due notifications only for the
-- given error-pattern hashes. Empty vector is a no-op.
notifyErrorSubscriptions :: Projects.ProjectId -> V.Vector Text -> ATBackgroundCtx ()
notifyErrorSubscriptions pid hashes = unless (V.null hashes) $ runDueNotifications pid (Just hashes)


-- | Sweep path: picks up patterns whose inline notify was skipped (job lag,
-- rate-limit overflow, channel added after the fact).
sweepErrorSubscriptions :: Projects.ProjectId -> ATBackgroundCtx ()
sweepErrorSubscriptions pid = runDueNotifications pid Nothing


runDueNotifications :: Projects.ProjectId -> Maybe (V.Vector Text) -> ATBackgroundCtx ()
runDueNotifications pid hashesM = do
  ctx <- ask @Config.AuthContext
  Relude.unless ctx.config.pauseNotifications do
    now <- Time.currentTime
    dueErrors :: [ErrorSubscriptionDue] <- queryDueErrorNotifications pid hashesM now
    dispatchDueErrorNotifications ctx pid now dueErrors


-- | Every 10 min: sweep projects that have any eligible error-pattern
-- notifications (still within the 24h ceiling) and dispatch them. Primary
-- safety net against the old 60-min orphan hole.
runNotificationSweep :: UTCTime -> ATBackgroundCtx ()
runNotificationSweep _scheduledTime = do
  projects :: [Projects.ProjectId] <-
    HI.getOneColumn
      <<$>> Hasql.interp
        [HI.sql|
          SELECT DISTINCT e.project_id
          FROM apis.error_patterns e
          JOIN projects.projects p ON p.id = e.project_id
          WHERE p.error_alerts
            AND e.state <> 'resolved'
            AND e.created_at >= now() - interval '24 hours'
            AND (
              e.last_notified_at IS NULL
              OR e.subscribed = TRUE
              OR (e.state = 'regressed' AND (e.last_notified_at IS NULL OR e.last_notified_at < e.regressed_at))
              OR (e.state IN ('new','escalating','ongoing') AND e.last_notified_at IS NOT NULL)
            )
        |]
  Log.logInfo "NotificationSweepJob: candidate projects" (AE.object ["count" AE..= length projects])
  forM_ projects sweepErrorSubscriptions


-- | Flush the digest queue once per hour. Groups pending rows by project and
-- marks them `sent_at = now()`. Delivery today is log-only + email fallback;
-- Slack/Discord digest formatters can be layered on later without changing
-- the queue contract.
runNotificationDigest :: UTCTime -> ATBackgroundCtx ()
runNotificationDigest _scheduledTime = do
  ctx <- ask @Config.AuthContext
  Relude.unless ctx.config.pauseNotifications do
    projectIds :: [Projects.ProjectId] <-
      HI.getOneColumn
        <<$>> Hasql.interp
          [HI.sql|
            SELECT DISTINCT project_id FROM apis.notification_digest_queue
            WHERE sent_at IS NULL
              AND created_at >= now() - interval '24 hours'
          |]
    forM_ projectIds \pid -> do
      projectM <- Projects.projectById pid
      whenJust projectM \project -> Relude.when project.errorAlerts do
        rows :: [(UUID.UUID, Text, Text)] <-
          Hasql.interp
            [HI.sql|
              SELECT id, reason, title
              FROM apis.notification_digest_queue
              WHERE project_id = #{pid} AND sent_at IS NULL
              ORDER BY created_at ASC
              LIMIT 500
            |]
        unless (null rows) do
          let idsVec = V.fromList [i | (i, _, _) <- rows]
              summary = T.unlines ["• [" <> reason <> "] " <> title | (_, reason, title) <- take 10 rows]
              subj = "[" <> project.title <> "] " <> show (length rows) <> " batched notifications"
              url = ctx.env.hostUrl <> "p/" <> pid.toText <> "/issues?filter=Inbox"
          Log.logInfo "notification_digest_flushing"
            $ AE.object ["project_id" AE..= pid.toText, "count" AE..= length rows, "url" AE..= url]
          -- Email-only delivery today; Slack/Discord templates TBD.
          Relude.when (V.elem Projects.NEmail project.notificationsChannel) do
            users <- Projects.usersByProjectId pid
            let html = ET.digestEmail project.title url summary (length rows)
                rendered = ET.renderEmail subj html
            forM_ users \u -> void $ tryAny $ sendRenderedEmail (CI.original u.email) subj rendered
          -- Mark sent even if a channel silently no-ops; keep retrying patterns
          -- on the fast path via last_notified_at, not here.
          void
            $ Hasql.interpExecute
              [HI.sql|
              UPDATE apis.notification_digest_queue SET sent_at = now()
              WHERE id = ANY(#{idsVec}::uuid[])
            |]
          Log.logInfo "notification_digest_sent" (AE.object ["project_id" AE..= pid.toText, "count" AE..= length rows])


-- | Eligibility clauses:
--   (a) never-notified patterns created within the last 24h — was `60 minutes`
--       pre-0073, which left anything older permanently orphaned. Bounded to 24h
--       so the periodic sweep has a ceiling and does not alert on ancient rows.
--   (b) explicitly-subscribed re-notification respecting `notify_every_minutes`.
--   (c) regression re-notification.
--   (d) ongoing-issue reminder cadence (1h → 6h → 24h → daily) gated on the
--       issue still firing (at least one `event_count>0` hourly stat within
--       the last hour). Uses `apis.error_hourly_stats` because the decayed
--       `occurrences_1h` on `error_patterns` is unreliable.
queryDueErrorNotifications
  :: Projects.ProjectId
  -> Maybe (V.Vector Text)
  -> UTCTime
  -> ATBackgroundCtx [ErrorSubscriptionDue]
queryDueErrorNotifications pid mHashes now =
  let hashes = fromMaybe V.empty mHashes
      applyHashFilter = isJust mHashes
   in Hasql.interp
        [HI.sql|
          SELECT DISTINCT ON (e.id)
                 e.id, e.error_data, e.state, i.id, i.title,
                 e.slack_thread_ts, e.discord_message_id, e.occurrences_1h
          FROM apis.error_patterns e
          JOIN apis.issues i ON i.project_id = e.project_id AND i.target_hash = e.hash
          WHERE e.project_id = #{pid}
            AND (NOT #{applyHashFilter} OR e.hash = ANY(#{hashes}::text[]))
            AND e.state != 'resolved'
            AND i.issue_type = #{Issues.RuntimeException}::apis.issue_type
            AND (
              (e.last_notified_at IS NULL AND e.created_at >= #{now}::timestamptz - INTERVAL '24 hours')
              OR (e.subscribed = TRUE
                  AND #{now}::timestamptz - e.last_notified_at >= (e.notify_every_minutes * INTERVAL '1 minute'))
              OR (e.state = 'regressed'
                  AND (e.last_notified_at IS NULL OR e.last_notified_at < e.regressed_at))
              OR (e.state IN ('new','escalating','ongoing')
                  AND e.last_notified_at IS NOT NULL
                  AND EXISTS (
                    SELECT 1 FROM apis.error_hourly_stats s
                    WHERE s.error_id = e.id
                      AND s.hour_bucket >= date_trunc('hour', #{now}::timestamptz) - INTERVAL '1 hour'
                      AND s.event_count > 0
                  )
                  AND #{now}::timestamptz - e.last_notified_at >= CASE
                        WHEN #{now}::timestamptz - e.created_at < INTERVAL '1 hour'  THEN INTERVAL '1 hour'
                        WHEN #{now}::timestamptz - e.created_at < INTERVAL '6 hours' THEN INTERVAL '6 hours'
                        WHEN #{now}::timestamptz - e.created_at < INTERVAL '24 hours' THEN INTERVAL '24 hours'
                        ELSE INTERVAL '24 hours'
                      END)
            )
          ORDER BY e.id, i.created_at DESC
        |]


dispatchDueErrorNotifications
  :: Config.AuthContext
  -> Projects.ProjectId
  -> UTCTime
  -> [ErrorSubscriptionDue]
  -> ATBackgroundCtx ()
dispatchDueErrorNotifications ctx pid now dueErrors =
  unless (null dueErrors) do
    Log.logInfo "Notifying error subscriptions" ("project_id", AE.toJSON pid.toText, "due_count", AE.toJSON (length dueErrors))
    let ctxObj = AE.object ["project_id" AE..= pid.toText, "due_count" AE..= length dueErrors]
    Projects.projectById pid >>= flip whenJust \project ->
      if
        | not project.errorAlerts -> Log.logInfo "Error alerts disabled for project — skipping" ctxObj
        | V.null project.notificationsChannel -> Log.logAttention "Notifications skipped: no channels configured" ctxObj
        | otherwise -> do
            users <- Projects.usersByProjectId pid
            let alertTypeForState = \case
                  ErrorPatterns.ESEscalating -> EscalatingErrors
                  ErrorPatterns.ESRegressed -> RegressedErrors
                  _ -> NewRuntimeError
            results <- forConcurrently dueErrors \sub -> do
              let alertType = alertTypeForState sub.errorState
                  errorsUrl = ctx.env.hostUrl <> "p/" <> pid.toText <> "/issues/" <> sub.issueId.toText
                  occTextM = (show sub.occurrences1h <> "/hr") <$ guard (sub.occurrences1h > 0)
                  fromTime = addUTCTime (-(15 * 60)) now
                  firstSeenTextM = Just $ relTimeAgo now sub.errorData.when <> " · " <> toText (formatTime defaultTimeLocale "%b %-e %-l:%M %p" sub.errorData.when)
              chartUrlM <- errorTrendChartUrl ctx pid sub.errorData.hash (formatUTC fromTime) (formatUTC now)
              let alert = RuntimeErrorAlert{issueId = Issues.issueIdText sub.issueId, issueTitle = sub.issueTitle, errorData = sub.errorData, runtimeAlertType = alertType, chartUrl = chartUrlM, occurrenceText = occTextM, firstSeenText = firstSeenTextM}
                  ~(subj, html) = case alertType of
                    EscalatingErrors -> ET.escalatingErrorsEmail project.title errorsUrl [sub.errorData] chartUrlM occTextM
                    RegressedErrors -> ET.regressedErrorsEmail project.title errorsUrl [sub.errorData] chartUrlM occTextM
                    _ -> ET.runtimeErrorsEmail project.title errorsUrl [sub.errorData] chartUrlM occTextM
              -- Rate-limit gate. Overflow lands in apis.notification_digest_queue
              -- and is flushed by NotificationDigestJob.
              allowed <- consumeNotificationToken pid now
              if allowed
                then do
                  (finalSlackTs, finalDiscordMsgId) <-
                    sendAlertToChannels alert pid project users errorsUrl subj html (sub.slackThreadTs, sub.discordMessageId)
                  Log.logInfo "notification_sent" (AE.object ["project_id" AE..= pid.toText, "error_id" AE..= sub.errorId, "alert_type" AE..= show @Text alertType])
                  pure (sub.errorId, finalSlackTs, finalDiscordMsgId, True)
                else do
                  enqueueDigest pid (Just sub.errorId) (Just sub.issueId) "rate_limit" sub.issueTitle
                  Log.logInfo "notification_skipped" (AE.object ["project_id" AE..= pid.toText, "error_id" AE..= sub.errorId, "reason" AE..= ("rate_limit" :: Text)])
                  pure (sub.errorId, sub.slackThreadTs, sub.discordMessageId, False)
            -- Only stamp last_notified_at for deliveries that actually went out.
            -- Rate-limited rows stay eligible so the next tick retries.
            forM_ results \(errorId, slackTs, discordMsgId, delivered) ->
              Relude.when delivered
                $ void
                $ ErrorPatterns.updateErrorPatternThreadIdsAndNotifiedAt errorId slackTs discordMsgId now


-- | Sliding-window rate limiter. One bucket per project per hour. Returns True
-- and increments the counter when under the cap; False otherwise. Atomic via
-- `INSERT … ON CONFLICT DO UPDATE … RETURNING`.
notificationsPerProjectPerHour :: Int
notificationsPerProjectPerHour = 20


consumeNotificationToken :: Projects.ProjectId -> UTCTime -> ATBackgroundCtx Bool
consumeNotificationToken pid now = do
  -- UPSERT…RETURNING always produces one row; Nothing is treated as pass-through.
  countM :: Maybe Int <-
    Hasql.interpOne
      [HI.sql|
          INSERT INTO apis.notification_rate_limit (project_id, window_start, count)
          VALUES (#{pid}, date_trunc('hour', #{now}::timestamptz), 1)
          ON CONFLICT (project_id, window_start)
          DO UPDATE SET count = apis.notification_rate_limit.count + 1
          RETURNING count :: int
        |]
  pure $ maybe True (<= notificationsPerProjectPerHour) countM


-- | Push a rate-limited or low-signal notification into the digest queue. The
-- hourly NotificationDigestJob flushes these into a single message per project.
enqueueDigest
  :: Projects.ProjectId
  -> Maybe ErrorPatterns.ErrorPatternId
  -> Maybe Issues.IssueId
  -> Text -- reason: 'rate_limit' | 'log_pattern_rate_change' | 'log_pattern'
  -> Text -- title (used for the digest body)
  -> ATBackgroundCtx ()
enqueueDigest pid errorPatternId issueId reason title =
  Hasql.interpExecute
    [HI.sql|
      INSERT INTO apis.notification_digest_queue (project_id, error_pattern_id, issue_id, reason, title)
      VALUES (#{pid}, #{errorPatternId}, #{issueId}, #{reason}, #{title})
    |]
    >> pass


-- | Process and insert errors for a specific project (single batched round-trip via unnest).
-- Creates issues synchronously for new/regressed errors. Reopens existing issues on regression.
processProjectErrors :: Projects.ProjectId -> V.Vector ErrorPatterns.ATError -> UTCTime -> ATBackgroundCtx ()
processProjectErrors pid errors now = do
  tryAny (ErrorPatterns.batchUpsertErrorPatterns pid errors now) >>= \case
    Left e ->
      Log.logAttention "ERR_BATCH_UPSERT_FAILED"
        $ AE.object
          [ "error_id" AE..= ("ERR_BATCH_UPSERT_FAILED" :: Text)
          , "project_id" AE..= pid.toText
          , "kind" AE..= (if isJust (fromException @Hasql.HasqlException e) then "hasql" else "other" :: Text)
          , "error" AE..= show @Text e
          ]
    Right newOrRegressed -> do
      unless (V.null errors) do
        Log.logTrace "Successfully inserted errors for project"
          $ AE.object [("project_id", AE.toJSON pid.toText), ("error_count", AE.toJSON $ V.length errors)]
        -- Upsert hourly rollup stats (must follow batchUpsertErrorPatterns which creates the FK rows)
        let addErrCounts (c1, u1) (c2, u2) = (c1 + c2, u1 + u2)
            errorRollupGroups = HM.toList $ V.foldl' (\acc e -> HM.insertWith addErrCounts e.hash (1 :: Int, bool 0 1 (isJust e.userId)) acc) HM.empty errors
            errorRollupStats = V.fromList [(h, cnt, users) | (h, (cnt, users)) <- errorRollupGroups]
        void $ ErrorPatterns.upsertErrorPatternHourlyStats pid now errorRollupStats
      forM_ newOrRegressed \(errorHash, errState) -> do
        errM <- ErrorPatterns.getErrorPatternByHash pid errorHash
        whenJust errM \err ->
          if errState == "regressed"
            then do
              existingM <- Issues.selectLatestIssueByHash pid errorHash
              maybe
                ( do
                    issue <- createIssueForError pid err
                    Log.logInfo "Created new issue for regressed error (no prior issue)" (pid, err.id, issue.id)
                )
                (handleRegression pid err)
                existingM
            else do
              -- Fast pre-merge: skip issue creation if an identical canonical pattern already exists
              canonicalM <- ErrorPatterns.findCanonicalMatch pid err.service err.errorType err.message
              case canonicalM of
                Just canonicalId | canonicalId /= err.id -> do
                  void $ PatternMergeDB.setCanonicalId err.id canonicalId
                  Log.logTrace "Pre-merged new error into canonical" (pid, err.id, canonicalId)
                _ -> do
                  issue <- createIssueForError pid err
                  authCtx <- Effectful.Reader.Static.ask @Config.AuthContext
                  liftIO $ withResource authCtx.jobsPool \conn ->
                    void $ createJob conn "background_jobs" $ EnhanceIssuesWithLLM pid (V.singleton issue.id)
                  Log.logInfo "Created issue for new error" (pid, err.id, issue.id)
  where
    createIssueForError pid' err' = do
      issue <- Issues.createNewErrorIssue pid' err'
      Issues.insertIssue issue
      Issues.logIssueActivity issue.id Issues.IECreated Nothing Nothing
      pure issue
    handleRegression pid' err' existing = do
      let wasClosed = isJust existing.acknowledgedAt || isJust existing.archivedAt
      if wasClosed
        then do
          Issues.reopenIssue existing.id
          Issues.logIssueActivity existing.id Issues.IERegressed Nothing Nothing
          Log.logInfo "Reopened issue for regressed error" (pid', err'.id, existing.id)
        else do
          Issues.bumpIssueUpdatedAt existing.id
          Log.logTrace "Bumped already-open issue for regressed error" (pid', err'.id, existing.id)


deduplicateByHash :: (a -> Text) -> V.Vector a -> V.Vector a
deduplicateByHash getHash = V.fromList . HM.elems . V.foldl' (\acc item -> HM.insert (getHash item) item acc) HM.empty
{-# INLINE deduplicateByHash #-}


-- | Hourly catch-up. Sweeps `processed_at IS NULL` rows via the time-scoped
-- partial index `idx_otel_unprocessed` and re-drives them through the
-- extraction worker via `Telemetry.handOffBatches` (same code path as live
-- ingestion).
safetyNetReprocess :: Projects.ProjectId -> ATBackgroundCtx ()
safetyNetReprocess pid = do
  ctx <- ask @Config.AuthContext
  let cutoff = ctx.config.processedAtCutoff
  projectCacheVal <- liftIO $ Cache.fetchWithCache ctx.projectCache pid \pid' -> do
    mpjCache <- Projects.projectCacheByIdIO ctx.hasqlJobsPool pid'
    pure $ fromMaybe Projects.defaultProjectCache mpjCache
  let caches = one (pid, projectCacheVal)
  -- Single page per tick (1000 rows); next hourly tick picks up any remainder.
  let pidText = pid.toText
  rows <-
    V.fromList
      <$> Hasql.interp
        [HI.sql| SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity,
                     body, attributes, resource, COALESCE(hashes, '{}'::text[]) AS hashes,
                     kind, status_code, status_message, start_time, end_time, events,
                     links, duration, name, parent_id, COALESCE(summary, '{}'::text[]) AS summary, date
              FROM otel_logs_and_spans
              WHERE project_id = #{pidText}
                AND processed_at IS NULL
                AND timestamp >= #{cutoff}
                AND timestamp <  now() - interval '10 minutes'
                AND timestamp >  now() - interval '6 hours'
              ORDER BY timestamp
              LIMIT 1000 |]
  Relude.unless (V.null rows) $ do
    Log.logTrace "SafetyNetReprocess re-driving unprocessed rows" (AE.object ["project_id" AE..= pid.toText, "row_count" AE..= V.length rows])
    liftIO $ Telemetry.handOffBatches ctx.extractionWorker caches rows


-- | Dual-fork an UPDATE to Postgres (blocking, deadlock-retried) + TimeFusion (best-effort, circuit-broken).
dualExecPgTf :: (DB es, Ki.StructuredConcurrency :> es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es) => Config.AuthContext -> HI.Sql -> Eff es Int64
dualExecPgTf ctx sql' = Ki.scoped \scope -> do
  mainThread <- Ki.fork scope $ retryOnDeadlock 2 $ Hasql.interpExecute sql'
  _ <- Ki.fork scope $ Relude.when ctx.config.enableTimefusionWrites $ do
    now <- Time.currentTime
    shouldAttempt <- liftIO $ ExtractionWorker.shouldAttemptCircuit ctx.tfCircuit now
    Relude.when shouldAttempt
      $ tryAny (withHasqlTimefusion True $ Hasql.interpExecute sql')
      >>= \case
        Right _ -> liftIO $ ExtractionWorker.recordCircuitSuccess ctx.tfCircuit
        Left e -> do
          opened <- liftIO $ ExtractionWorker.recordCircuitFailure ctx.tfCircuit now
          Log.logAttention (if opened then "TimeFusion circuit opened" else "TimeFusion write failed") (show @Text e)
  Ki.atomically $ Ki.await mainThread
  where
    retryOnDeadlock :: (DB es, Log :> es) => Int -> Eff es Int64 -> Eff es Int64
    retryOnDeadlock 0 action = action
    retryOnDeadlock n action =
      tryAny action >>= \case
        Right r -> pure r
        Left e
          | Just he <- fromException @Hasql.HasqlException e
          , Hasql.isDeadlockError he -> do
              Log.logAttention "UPDATE-1 deadlock, retrying" (show @Text e)
              liftIO $ threadDelay (50000 * (3 - n)) -- 50ms, 100ms backoff
              retryOnDeadlock (n - 1) action
        Left e -> throwIO e


processEagerBatch
  :: ExtractionWorker.ExtractionBatch Telemetry.OtelLogsAndSpans
  -> ExtractionWorker.ShardState Telemetry.OtelLogsAndSpans
  -> ATBackgroundCtx ()
processEagerBatch batch shard
  | V.null batch.spans = pass
  | otherwise = do
      ctx <- ask @Config.AuthContext
      Relude.when ctx.config.enableEventsTableUpdates do
        now <- Time.currentTime
        let pid = batch.projectId
            spans = batch.spans
            projectCache = batch.projectCache
            spanIdsV = batch.spanIds
            traceIdsV = batch.traceIds
            batchMinTs = batch.batchMinTs
            -- Pad +1s so the exclusive `<` in UPDATE-1 covers the inclusive batchMaxTs.
            batchMaxTsPad = addUTCTime 1 batch.batchMaxTs

        -- Pure entity + hash derivation.
        !entityIds <- V.replicateM (V.length spans) UUID.genUUID
        let !canonicalTemplates = parseCanonicalPaths projectCache.canonicalPaths
            !results = V.zipWith (processSpanToEntities canonicalTemplates projectCache) spans entityIds
            !(endpoints, shapes, fields, formats, spanHashes, normalizedPaths) = V.unzip6 results
            !endpointsFinal = deduplicateByHash (.hash) $ V.mapMaybe id endpoints
            !shapesFinal = deduplicateByHash (.hash) $ V.mapMaybe id shapes
            !fieldsFinal = deduplicateByHash (.hash) $ V.concatMap id fields
            !formatsFinal = deduplicateByHash (.hash) $ V.concatMap id formats

        -- Error extraction (pure).
        let !allErrors = Telemetry.getAllATErrors spans
            sortedErrors = V.modify (VA.sortBy (comparing \e -> (e.traceId, e.spanId))) allErrors
            errorsByTrace = V.groupBy (\a b -> a.traceId == b.traceId && a.spanId == b.spanId) sortedErrors
            mkErrorEntry grouped = do
              (firstE, _) <- V.uncons grouped
              sId <- firstE.spanId
              tId <- firstE.traceId
              let mapped = V.map (\x -> AE.object ["type" AE..= x.errorType, "message" AE..= x.message, "stack_trace" AE..= x.stackTrace]) grouped
              pure ((sId, tId), AE.toJSON mapped)
            errorsByKey :: HM.HashMap (Text, Text) AE.Value
            errorsByKey = HM.fromList $ mapMaybe mkErrorEntry errorsByTrace
            errHashesByKey :: HM.HashMap (Text, Text) [Text]
            errHashesByKey =
              V.foldl'
                ( \acc e -> case (e.spanId, e.traceId) of
                    (Just sid, Just tid) -> HM.insertWith (<>) (sid, tid) ["err:" <> e.hash] acc
                    _ -> acc
                )
                HM.empty
                allErrors

        -- Per-row packed vectors for UPDATE-1.
        let perRowHashesJson :: V.Vector AE.Value
            perRowHashesJson =
              V.zipWith3
                ( \sid tid base ->
                    let extra = fromMaybe [] (HM.lookup (sid, tid) errHashesByKey)
                     in AE.toJSON (V.toList base <> extra)
                )
                spanIdsV
                traceIdsV
                spanHashes
            perRowErrorsJson :: V.Vector AE.Value
            perRowErrorsJson =
              V.zipWith (\sid tid -> fromMaybe AE.Null (HM.lookup (sid, tid) errorsByKey)) spanIdsV traceIdsV

        Relude.when (V.length endpointsFinal > 0 || V.length shapesFinal > 0 || V.length fieldsFinal > 0 || V.length formatsFinal > 0 || V.length allErrors > 0)
          $ Log.logTrace
            "Eager-track derivations"
            ( AE.object
                [ "project_id" AE..= pid.toText
                , "spans" AE..= V.length spans
                , "endpoints" AE..= V.length endpointsFinal
                , "shapes" AE..= V.length shapesFinal
                , "fields" AE..= V.length fieldsFinal
                , "formats" AE..= V.length formatsFinal
                , "errors" AE..= V.length allErrors
                ]
            )

        -- Sibling inserts (any throw prevents UPDATE-1 from running — safety-net retries).
        -- Note: error pattern upsert + hourly stats are handled by ProcessProjectErrorsJob
        -- to avoid double-upsert (which would return 'unchanged' and skip issue creation).
        Ki.scoped \scope -> do
          let forkNonEmpty :: V.Vector a -> (V.Vector a -> ATBackgroundCtx ()) -> ATBackgroundCtx ()
              forkNonEmpty v action = Relude.unless (V.null v) $ void $ Ki.fork scope $ action v
          forkNonEmpty endpointsFinal Endpoints.bulkInsertEndpoints
          forkNonEmpty shapesFinal Fields.bulkInsertShapes
          forkNonEmpty fieldsFinal Fields.bulkInsertFields
          forkNonEmpty formatsFinal Fields.bulkInsertFormat
          forkNonEmpty allErrors \_ -> liftIO $ withResource ctx.jobsPool \conn ->
            void $ createJob conn "background_jobs" $ ProcessProjectErrorsJob pid allErrors now
          Ki.atomically $ Ki.awaitAll scope

        -- UPDATE-1: non-destructive hash merge, dual-forked to PG + TimeFusion.
        let dbSpanIds = V.toList spanIdsV
            dbTraceIds = V.toList traceIdsV
            dbHashesJson = V.toList perRowHashesJson
            dbErrorsJson = V.toList perRowErrorsJson
            dbNormPaths = V.toList normalizedPaths
            pidText = pid.toText
            update1Sql =
              [HI.sql| UPDATE otel_logs_and_spans o
                    SET hashes = ARRAY(
                          SELECT DISTINCT h
                          FROM unnest(COALESCE(o.hashes, '{}'::text[]) || u.new_hashes) AS h
                        ),
                        errors = COALESCE(NULLIF(u.errors, 'null'::jsonb), o.errors),
                        attributes___url___path = COALESCE(u.norm_path, o.attributes___url___path),
                        attributes = CASE WHEN u.norm_path IS NOT NULL THEN
                          COALESCE(o.attributes, '{}'::jsonb)
                            || jsonb_build_object(
                                 'url',  COALESCE(o.attributes->'url',  '{}'::jsonb) || jsonb_build_object('path',  u.norm_path),
                                 'http', COALESCE(o.attributes->'http', '{}'::jsonb) || jsonb_build_object('route', u.norm_path))
                          ELSE o.attributes END,
                        processed_at = now()
                    FROM (
                      SELECT span_id, trace_id, errors, norm_path,
                             ARRAY(SELECT jsonb_array_elements_text(hashes_json)) AS new_hashes
                      FROM (
                        SELECT unnest(#{dbSpanIds}::text[])  AS span_id,
                               unnest(#{dbTraceIds}::text[])  AS trace_id,
                               unnest(#{dbHashesJson}::jsonb[]) AS hashes_json,
                               unnest(#{dbErrorsJson}::jsonb[]) AS errors,
                               unnest(#{dbNormPaths}::text[])  AS norm_path
                      ) raw
                      ORDER BY span_id, trace_id
                    ) u
                    WHERE o.project_id = #{pidText}
                      AND o.timestamp >= #{batchMinTs}
                      AND o.timestamp <  #{batchMaxTsPad}
                      AND o.context___span_id = u.span_id
                      AND o.context___trace_id = u.trace_id |]
        rowsUpdated <- dualExecPgTf ctx update1Sql
        Log.logTrace "Eager-track UPDATE-1 complete" (AE.object ["project_id" AE..= pid.toText, "span_count" AE..= V.length spans, "rows_updated" AE..= rowsUpdated])

        -- TODO(otel-metrics): emit counters for batches_processed, spans_processed here.
        -- Drain-track hand-off: buffer spans for pattern tagging (T.copy to unpin).
        let bufferedSpans :: [(Text, ExtractionWorker.BufferedSpan)]
            bufferedSpans = V.toList $ V.zipWith3 buildBuffered spans spanIdsV traceIdsV
            buildBuffered s spanCtxId' traceId' =
              let svcName = fromMaybe "unknown" (Telemetry.spanServiceName s)
                  summaryText = T.copy $ T.intercalate " " (V.toList s.summary)
               in ( T.copy svcName
                  , ExtractionWorker.BufferedSpan
                      { ExtractionWorker.spanCtxId = T.copy spanCtxId'
                      , ExtractionWorker.traceId = T.copy traceId'
                      , ExtractionWorker.timestamp = s.timestamp
                      , ExtractionWorker.summary = summaryText
                      }
                  )
        liftIO $ ExtractionWorker.appendBufferedSpans shard pid ctx.config.drainFlushBatchSize now ctx.extractionWorker.droppedFlushTasks bufferedSpans


-- | 1-minute error-state decay tick. Owns `propagateMergedCounts` +
-- `updateOccurrenceCounts` so errors auto-resolve once quiet long enough.
-- Runs every minute per active project.
runErrorDecayFiber :: Logger -> Config.AuthContext -> TracerProvider -> IO ()
runErrorDecayFiber logger ctx tp = forever $ do
  threadDelay 60_000_000
  runBackground logger ctx tp $ do
    now <- Time.currentTime
    projects <- Projects.activeNonOnboardingProjectIds
    tryLog "propagateMergedCountsBatch" $ void $ ErrorPatterns.propagateMergedCountsBatch projects
    tryLog "updateOccurrenceCountsBatch" $ void $ ErrorPatterns.updateOccurrenceCountsBatch projects now


-- | Drain-flusher shard fiber: pulls `DrainFlushTask`s from the shard's
-- drainFlushQ, runs Drain clustering (rehydrating the tree from
-- apis.log_patterns when stale), persists log patterns, and issues the
-- additive UPDATE-2 that appends `pat:*` tags to the owning spans' `hashes`
-- column. Unlike UPDATE-1, UPDATE-2 never touches `processed_at` — only the
-- eager track claims a row (invariant #1 from the plan).
runDrainFlusher
  :: Logger
  -> Config.AuthContext
  -> TracerProvider
  -> ExtractionWorker.ShardState Telemetry.OtelLogsAndSpans
  -> IO ()
runDrainFlusher logger ctx tp shard = forever $ do
  task <- atomically $ readTBQueue shard.drainFlushQ
  tryAny (maybeSpawnRehydration logger ctx tp shard (task.projectId, task.serviceName)) >>= \case
    Right () -> pass
    Left e -> logExc "drain-flusher:rehydration" e
  tryAny (runBackground logger ctx tp $ flushDrainTask shard task) >>= \case
    Right () -> pass
    Left e -> logExc "drain-flusher:task" e
  where
    logExc label e =
      runLogT (show ctx.config.environment) logger ctx.config.logLevel
        $ LogLegacy.logAttention label (show @Text e)


-- | Spawn async rehydration for a (project, service) tree if stale and not
-- already pending. Uses change detection via MAX(last_seen_at). The flusher
-- continues with the existing (stale) tree — rehydration swaps in the fresh
-- tree on completion.
maybeSpawnRehydration
  :: Logger
  -> Config.AuthContext
  -> TracerProvider
  -> ExtractionWorker.ShardState Telemetry.OtelLogsAndSpans
  -> (Projects.ProjectId, Text)
  -> IO ()
maybeSpawnRehydration logger ctx tp shard key@(pid, svcName) = do
  now <- getCurrentTime
  let rehydrateInterval = fromIntegral ctx.config.drainRehydrateIntervalSecs :: NominalDiffTime
  existing <- HM.lookup key <$> readIORef shard.drainTrees
  let isStale = case existing of
        Nothing -> True
        Just sdt -> diffUTCTime now sdt.lastSeededAt >= rehydrateInterval
  when isStale $ do
    pending <- readIORef shard.pendingRehydrations
    unless (HashSet.member key pending) $ do
      let job = do
            tryAny (runBackground logger ctx tp $ rehydrateTree shard key now existing) >>= \case
              Right () -> pass
              Left e ->
                runLogT (show ctx.config.environment) logger ctx.config.logLevel
                  $ LogLegacy.logAttention "drain-rehydrate failed" (show @Text e)
            atomicModifyIORef' shard.pendingRehydrations \s -> (HashSet.delete key s, ())
      atomicModifyIORef' shard.pendingRehydrations \s -> (HashSet.insert key s, ())
      enqueued <- atomically $ do
        full <- isFullTBQueue shard.rehydrationQ
        if full
          then pure False
          else writeTBQueue shard.rehydrationQ job $> True
      unless enqueued
        $ atomicModifyIORef' shard.pendingRehydrations \s -> (HashSet.delete key s, ())


rehydrateTree
  :: ExtractionWorker.ShardState Telemetry.OtelLogsAndSpans
  -> (Projects.ProjectId, Text)
  -> UTCTime
  -> Maybe ExtractionWorker.ServiceDrainTree
  -> ATBackgroundCtx ()
rehydrateTree shard key@(pid, svcName) now existing = do
  (texts, maxSeen) <- LogPatterns.getLogPatternTextsByService pid "summary" svcName
  -- Change detection: skip rebuild if maxPatternSeenAt hasn't advanced.
  let unchanged = case (existing, maxSeen) of
        (Just sdt, Just ms) | sdt.maxPatternSeenAt == Just ms -> True
        _ -> False
  if unchanged
    then liftIO $ atomicModifyIORef' shard.drainTrees \m ->
      case HM.lookup key m of
        Just sdt -> (HM.insert key sdt{ExtractionWorker.lastSeededAt = now} m, ())
        Nothing -> (m, ())
    else void $ seedFromPatterns shard key now texts maxSeen


-- | Seed a drain tree from DB patterns and insert it into the shard's IORef.
seedDrainTreeFromDB
  :: DB es
  => ExtractionWorker.ShardState Telemetry.OtelLogsAndSpans
  -> (Projects.ProjectId, Text)
  -> UTCTime
  -> Eff es Drain.DrainTree
seedDrainTreeFromDB shard key@(pid, svcName) now = do
  (texts, maxSeen) <- LogPatterns.getLogPatternTextsByService pid "summary" svcName
  seedFromPatterns shard key now texts maxSeen


-- | Build a drain tree from pre-fetched patterns and insert into shard state.
seedFromPatterns
  :: IOE :> es
  => ExtractionWorker.ShardState Telemetry.OtelLogsAndSpans
  -> (Projects.ProjectId, Text)
  -> UTCTime
  -> [Text]
  -> Maybe UTCTime
  -> Eff es Drain.DrainTree
seedFromPatterns shard key now texts maxSeen = do
  let freshTree = processBatch True (V.fromList $ map SeedPattern texts) now Drain.emptyDrainTree
      newEntry = ExtractionWorker.ServiceDrainTree{tree = freshTree, lastSeededAt = now, maxPatternSeenAt = maxSeen}
  liftIO $ atomicModifyIORef' shard.drainTrees \m -> (HM.insert key newEntry m, ())
  pure freshTree


-- | Process a single drain-flush task: merge the batch through the current
-- (possibly stale) tree via `processBatchWithMapping`, persist patterns to
-- `apis.log_patterns` + hourly stats, then issue UPDATE-2 to append each
-- span's pattern hash tag to `otel_logs_and_spans.hashes`.
flushDrainTask
  :: ExtractionWorker.ShardState Telemetry.OtelLogsAndSpans
  -> ExtractionWorker.DrainFlushTask
  -> ATBackgroundCtx ()
flushDrainTask shard task
  | null task.spans = pass
  | otherwise = do
      ctx <- ask @Config.AuthContext
      now <- Time.currentTime
      let pid = task.projectId
          svcName = task.serviceName
          key = (pid, svcName)
      -- Use whatever tree is currently available (rehydration runs async).
      -- If no tree exists yet, seed synchronously on first encounter.
      existingTree <- liftIO $ HM.lookup key <$> readIORef shard.drainTrees
      seededTree <- maybe (seedDrainTreeFromDB shard key now) (pure . (.tree)) existingTree

      -- Build NewEvent inputs from the buffered spans.
      let events =
            V.fromList
              [ NewEvent bs.spanCtxId bs.summary
              | bs <- task.spans
              ]
          (!mergedTree, mapping) = processBatchWithMapping True events now seededTree

      -- Swap the merged tree back. Preserve lastSeededAt/maxPatternSeenAt from
      -- the existing entry (async rehydration owns those fields).
      liftIO $ atomicModifyIORef' shard.drainTrees \m ->
        let prev = HM.lookup key m
            newEntry =
              ExtractionWorker.ServiceDrainTree
                { tree = mergedTree
                , lastSeededAt = maybe now (.lastSeededAt) prev
                , maxPatternSeenAt = prev >>= (.maxPatternSeenAt)
                }
         in (HM.insert key newEntry m, ())

      -- Build a spanCtxId → patternHash tag map so each row can have its own
      -- tag appended in UPDATE-2.
      let tagFor lid = "pat:" <> toXXHash lid
          tagByCtx :: HM.HashMap Text Text
          tagByCtx = HM.fromList [(lid, tagFor tpl) | (lid, tpl) <- V.toList mapping]

      -- Persist log patterns + hourly stats.
      let allPatternsRaw = Drain.getAllLogGroups mergedTree
          allPatterns = PatternMerge.mergeByJaccard PatternMerge.jaccardMergeThreshold allPatternsRaw
          prepared = flip mapMaybe (V.toList allPatterns) \dp ->
            let eventCount = fromIntegral dp.frequency :: Int64
                patternHash = toXXHash dp.templateStr
             in if eventCount <= 0 || T.null dp.templateStr
                  then Nothing
                  else
                    Just
                      ( LogPatterns.UpsertPattern
                          { projectId = pid
                          , logPattern = dp.templateStr
                          , hash = patternHash
                          , sourceField = "summary"
                          , serviceName = Just svcName
                          , logLevel = Nothing
                          , traceId = Nothing
                          , sampleMessage = Just dp.exampleLog
                          , eventCount
                          }
                      , (pid, "summary" :: Text, patternHash, task.flushedAt, eventCount)
                      )
      let (ups, hss) = unzip prepared
      Relude.unless (null ups) $ void $ LogPatterns.upsertLogPatternBatch ups
      Relude.unless (null hss) $ void $ LogPatterns.upsertHourlyStatBatch hss

      -- UPDATE-2: additive per-row tag append. Dual-forked to Postgres + TimeFusion.
      let taggedSpans =
            [ (bs.spanCtxId, bs.traceId, tag, bs.timestamp)
            | bs <- task.spans
            , Just tag <- [HM.lookup bs.spanCtxId tagByCtx]
            ]
      whenJust (nonEmpty taggedSpans) \taggedNE -> do
        let spanIds' = [sid | (sid, _, _, _) <- taggedSpans]
            traceIds' = [tid | (_, tid, _, _) <- taggedSpans]
            tagArr = [t | (_, _, t, _) <- taggedSpans]
            tsList = fmap (\(_, _, _, ts) -> ts) taggedNE
            (minTs, maxTs) = foldl' (\(!lo, !hi) t -> (min lo t, max hi t)) (head tsList, head tsList) tsList
            maxTsPad = addUTCTime 1 maxTs
            pidText = pid.toText
            update2Sql =
              [HI.sql| UPDATE otel_logs_and_spans o
                    SET hashes = COALESCE(o.hashes, '{}'::text[]) || ARRAY[u.tag]
                    FROM (SELECT * FROM (SELECT unnest(#{spanIds'}::text[]) AS span_id,
                                 unnest(#{traceIds'}::text[]) AS trace_id,
                                 unnest(#{tagArr}::text[]) AS tag) raw
                                 ORDER BY span_id, trace_id) u
                    WHERE o.project_id = #{pidText}
                      AND o.timestamp >= #{minTs}
                      AND o.timestamp <  #{maxTsPad}
                      AND o.context___span_id = u.span_id
                      AND o.context___trace_id = u.trace_id
                      AND NOT (COALESCE(o.hashes, '{}'::text[]) @> ARRAY[u.tag]) |]
        void $ dualExecPgTf ctx update2Sql
      -- TODO(otel-metrics): emit counters for drain_flushes_completed, spans_flushed, patterns_persisted.
      Log.logTrace "Drain-flush complete" (AE.object ["project_id" AE..= pid.toText, "service" AE..= svcName, "span_count" AE..= length task.spans, "pattern_count" AE..= length ups])


-- | Age-flush timer fiber. Every 10 s: (1) evict age-stale buffers,
-- (2) enforce global maxBufferedSpans by force-flushing the largest buffer,
-- (3) LRU-evict drainTrees entries older than 1 hour or exceeding maxDrainTrees.
-- TODO(otel-metrics): periodically export batches_processed, spans_processed,
-- dropped_batches, dropped_flush_tasks, drain_flushes_completed as OTel counters.
runDrainAgeFlushTimer :: Logger -> Config.AuthContext -> IO ()
runDrainAgeFlushTimer logger ctx = forever $ do
  threadDelay 10_000_000 -- 10 s
  now <- getCurrentTime
  let worker = ctx.extractionWorker
      tryOp name action =
        tryAny action >>= \case
          Right () -> pass
          Left e ->
            runLogT (show ctx.config.environment) logger ctx.config.logLevel
              $ LogLegacy.logAttention ("drain-age-flush:" <> name) (show @Text e)
  tryOp "collectAgedFlushes" $ ExtractionWorker.collectAgedFlushes worker ctx.config.drainFlushMaxAgeSecs now
  tryOp "enforceBufferBound" $ ExtractionWorker.enforceBufferBound worker ctx.config.maxBufferedSpans now
  tryOp "evictStaleTrees" $ ExtractionWorker.evictStaleTrees worker ctx.config.maxDrainTrees now
  tryAny (TSC.evictStaleEntries ctx.traceSessionCache 300 50_000 now) >>= \case
    Right n ->
      when (n > 0)
        $ runLogT (show ctx.config.environment) logger ctx.config.logLevel
        $ LogLegacy.logTrace "drain-age-flush:evictTraceSessions" (show @Text n)
    Left e ->
      runLogT (show ctx.config.environment) logger ctx.config.logLevel
        $ LogLegacy.logAttention "drain-age-flush:evictTraceSessions" (show @Text e)


runSessionBackfillTimer :: Logger -> Config.AuthContext -> TracerProvider -> IO ()
runSessionBackfillTimer logger ctx tp = forever $ do
  threadDelay 90_000_000 -- 90 s
  tryAny (runBackground logger ctx tp go) >>= \case
    Right () -> pass
    Left e ->
      runLogT (show ctx.config.environment) logger ctx.config.logLevel
        $ LogLegacy.logAttention "session-backfill" (show @Text e)
  where
    go = do
      n <- TSC.backfillSessionAttributes
      when (n > 0) $ Log.logTrace "session-backfill" ("rows_updated" :: Text, n)


stripeAuth :: Text -> Wreq.Options
stripeAuth apiKey = defaults & (header "Authorization" .~ ["Bearer " <> encodeUtf8 apiKey])


reportUsageToStripe :: Text -> Text -> Text -> Int -> IO ()
reportUsageToStripe apiKey customerId eventName quantity = do
  let params :: [(ByteString, ByteString)]
      params =
        [ ("event_name", encodeUtf8 eventName)
        , ("payload[value]", encodeUtf8 $ show quantity)
        , ("payload[stripe_customer_id]", encodeUtf8 customerId)
        ]
  void $ postWith (stripeAuth apiKey) "https://api.stripe.com/v1/billing/meter_events" params


-- | `trialEnd` is Nothing when the sub has no trial or the trial is over.
data StripeSubDetails = StripeSubDetails
  { subItemId :: Text
  , priceId :: Text
  , status :: Text
  , trialEnd :: Maybe Int
  }


-- | Returns Nothing on HTTP/network/TLS error or JSON decode miss. Callers
-- treat Nothing as an actionable failure.
getStripeSubDetails :: Text -> Text -> IO (Maybe StripeSubDetails)
getStripeSubDetails apiKey subId = do
  respE <- tryAny $ getWith (stripeAuth apiKey) ("https://api.stripe.com/v1/subscriptions/" <> toString subId)
  pure $ case respE of
    Left _ -> Nothing
    Right resp -> do
      let body = resp ^. responseBody
          item0 = AL.key "items" . AL.key "data" . AL.nth 0
      v <- AE.decode @AE.Value body
      subItemId <- v ^? item0 . AL.key "id" . AL._String
      priceId <- v ^? item0 . AL.key "price" . AL.key "id" . AL._String
      let status = fromMaybe "" (v ^? AL.key "status" . AL._String)
          trialEnd = v ^? AL.key "trial_end" . AL._Integer <&> fromInteger
      pure StripeSubDetails{..}


-- | Enqueues TrialEndingReminder jobs at T-7d and T-3d. Enqueue failures are
-- logged but never raised — checkout billing state is already committed.
scheduleTrialReminders :: (DB es, Log :> es, Time.Time :> es) => Projects.ProjectId -> Int -> Eff es ()
scheduleTrialReminders pid trialEndEpoch = do
  now <- Time.currentTime
  let trialEnd = posixSecondsToUTCTime (fromIntegral trialEndEpoch)
      (past, due) =
        L.partition
          (\(runAt, _) -> runAt <= now)
          [ (addUTCTime (negate (fromIntegral daysLeft * 86400)) trialEnd, daysLeft)
          | daysLeft <- [7, 3 :: Int]
          ]
  forM_ past \(runAt, daysLeft) ->
    Log.logAttention "Trial reminder runAt already past; skipping" (pid.toText, daysLeft, runAt)
  forM_ due \(runAt, daysLeft) -> do
    let payload = AE.toJSON (TrialEndingReminder pid daysLeft)
    res <-
      tryAny
        $ void
        $ Hasql.interpExecute
          [HI.sql|INSERT INTO background_jobs (run_at, status, payload)
             VALUES (#{runAt}, 'queued', #{HI.AsJsonb payload})|]
    whenLeft_ res \e ->
      Log.logAttention "Trial reminder enqueue failed" (pid.toText, daysLeft, displayException e)


reportUsageToLemonsqueezy :: Text -> Int -> Text -> IO ()
reportUsageToLemonsqueezy subItemId quantity apiKey = do
  let formData =
        [aesonQQ|{
          "data": {"type": "usage-records","attributes": {"quantity": #{quantity},"action": "increment"},
                     "relationships": {"subscription-item": {"data": {"type": "subscription-items","id": #{subItemId}}}}
                  }}|]
      hds =
        defaults
          & (header "Authorization" .~ ["Bearer " <> encodeUtf8 apiKey])
          & (header "Content-Type" .~ ["application/vnd.api+json"])
          & (header "Accept" .~ ["application/vnd.api+json"])
  void $ postWith hds "https://api.lemonsqueezy.com/v1/usage-records" formData


-- | Send notifications for a query monitor status change (inline, no separate job)
notifyQueryMonitorStatusChange :: Monitors.QueryMonitor -> Double -> Bool -> ATBackgroundCtx ()
notifyQueryMonitorStatusChange monitor value isRecovery = do
  appCtx <- ask @Config.AuthContext
  whenJustM (Projects.projectById monitor.projectId) \p -> do
    teams <- ProjectMembers.getTeamsById monitor.projectId monitor.teams
    Relude.when (not (V.null monitor.teams) && null teams)
      $ Log.logAttention "Monitor configured with teams but none found (possibly deleted)" (monitor.id, monitor.projectId, V.length monitor.teams)
    let hostUrl = appCtx.env.hostUrl
        monitorListUrl = hostUrl <> "/p/" <> monitor.projectId.toText <> "/monitors"
        thresholdDir = if monitor.triggerLessThan then "below" else "above" :: Text
    now <- Time.currentTime
    let chartMins = clamp (15, 240) (4 * monitor.checkIntervalMins)
        chartFrom = addUTCTime (negate $ fromIntegral (chartMins * 60)) now
    chartUrlM <- if isRecovery then pure Nothing else monitorTrendChartUrl appCtx monitor.projectId monitor (formatUTC chartFrom) (formatUTC now)
    (alert, alertUrl) <-
      if isRecovery
        then pure (MonitorsRecoveryAlert{monitorTitle = monitor.alertConfig.title, monitorUrl = monitorListUrl}, monitorListUrl)
        else do
          issue <- Issues.createQueryAlertIssue monitor.projectId (show monitor.id) monitor.alertConfig.title monitor.logQuery monitor.alertThreshold value thresholdDir
          Issues.insertIssue issue
          let issueUrl = hostUrl <> "/p/" <> monitor.projectId.toText <> "/issues/" <> issue.id.toText
          pure (MonitorsAlert{monitorTitle = monitor.alertConfig.title, monitorUrl = issueUrl, chartUrl = chartUrlM}, issueUrl)
    targetTeams <-
      if null teams
        then maybeToList <$> ProjectMembers.getEveryoneTeam monitor.projectId
        else pure teams
    let (subj, html) =
          if isRecovery
            then ET.monitorRecoveryEmail p.title monitor.alertConfig.title alertUrl
            else ET.monitorAlertEmail p.title monitor.alertConfig.title alertUrl value monitor.alertThreshold thresholdDir chartUrlM
        renderedBody = ET.renderEmail subj html
    for_ targetTeams \team -> dispatchTeamNotifications
      team
      alert
      monitor.projectId
      p.title
      alertUrl
      \email _userM -> sendRenderedEmail (CI.original email) subj renderedBody


dispatchTeamNotifications :: ProjectMembers.Team -> Pkg.Mail.NotificationAlerts -> Projects.ProjectId -> Text -> Text -> (CI.CI Text -> Maybe Projects.User -> ATBackgroundCtx ()) -> ATBackgroundCtx ()
dispatchTeamNotifications team alert projectId projectTitle monitorUrl emailAction = do
  emails <- ProjectMembers.resolveTeamEmails projectId team
  for_ emails (`emailAction` Nothing)
  for_ team.slack_channels (void . sendSlackAlert alert projectId projectTitle . Just)
  for_ team.discord_channels (void . sendDiscordAlert alert projectId projectTitle . Just)
  for_ team.pagerduty_services \integrationKey -> sendPagerdutyAlertToService integrationKey alert projectTitle monitorUrl


jobsWorkerInit :: Logger -> Config.AuthContext -> TracerProvider -> IO ()
jobsWorkerInit logger appCtx tp = do
  Relude.when appCtx.config.enableDailyJobScheduling do
    ensureDailyJobScheduled appCtx
    void $ async $ forever do
      threadDelay (30 * 60 * 1_000_000) -- 30 minutes
      ensureDailyJobScheduled appCtx
  startJobRunner
    $ mkConfig jobLogger "background_jobs" appCtx.jobsPool (MaxConcurrentJobs appCtx.config.maxConcurrentJobs) (jobsRunner logger appCtx tp) id
  where
    jobLogger :: OddJobs.Job.LogLevel -> LogEvent -> IO ()
    jobLogger logLevel logEvent = runLogT "OddJobs" logger LogAttention $ LogLegacy.logInfo "Background jobs ping." (show @Text logLevel, show @Text logEvent)


-- | Ensure a DailyJob is queued for today. Safe to call from multiple pods —
-- uses INSERT ... WHERE NOT EXISTS to atomically skip if one already exists.
ensureDailyJobScheduled :: Config.AuthContext -> IO ()
ensureDailyJobScheduled appCtx = withResource appCtx.jobsPool \conn -> do
  [Only (inserted :: Int)] <-
    SimplePG.query_
      conn
      [sql|WITH ins AS (
             INSERT INTO background_jobs (run_at, status, payload)
             SELECT now(), 'queued', jsonb_build_object('tag', 'DailyJob')
             WHERE NOT EXISTS (
               SELECT 1 FROM background_jobs
               WHERE payload->>'tag' IN ('DailyJob', 'HourlyJob')
                 AND status IN ('queued', 'locked')
                 AND run_at >= date_trunc('day', now())
             )
             RETURNING 1
           ) SELECT COUNT(*)::int FROM ins|]
  Relude.when (inserted > 0) $ putTextLn "Scheduled DailyJob for today"


sendReportForProject :: Projects.ProjectId -> ReportType -> ATBackgroundCtx ()
sendReportForProject pid rType = do
  Log.logInfo "Generating report for project" pid
  ctx <- ask @Config.AuthContext
  users <- Projects.usersByProjectId pid
  currentTime <- Time.currentTime
  let (prv, typTxt) = case rType of
        WeeklyReport -> (6 * 86400, "weekly")
        _ -> (86400, "daily")

  let startTime = addUTCTime (negate prv) currentTime
      prevStart = addUTCTime (negate (prv * 2)) currentTime
      prevEnd = addUTCTime (negate prv) currentTime
      parseQ q =
        let qAST = Unsafe.fromRight [] (parseQueryToAST q)
            sqlQueryComponents = (defSqlQueryCfg pid currentTime Nothing Nothing){dateRange = (Just startTime, Just currentTime)}
            (_, qc) = queryASTToComponents sqlQueryComponents qAST
         in maybeToMonoid qc.finalSummarizeQuery
  projectM <- Projects.projectById pid
  forM_ projectM \pr -> do
    ( (stats, statsPrev)
      , ( (statsBySpanType, statsBySpanTypePrev)
          , ((slowDbQueriesL, (endpointStats, endpointStatsPrev)), ((chartDataEvents, chartDataErrors), (anomalies, _)))
          )
      ) <-
      concurrently
        ( concurrently
            (Telemetry.getProjectStatsForReport pid startTime currentTime)
            (Telemetry.getProjectStatsForReport pid prevStart prevEnd)
        )
        ( concurrently
            ( concurrently
                (V.fromList <$> Telemetry.getProjectStatsBySpanType pid startTime currentTime)
                (V.fromList <$> Telemetry.getProjectStatsBySpanType pid prevStart prevEnd)
            )
            ( concurrently
                ( concurrently
                    (Telemetry.getDBQueryStats pid startTime currentTime)
                    ( concurrently
                        (V.fromList <$> Telemetry.getEndpointStats pid startTime currentTime)
                        (V.fromList <$> Telemetry.getEndpointStats pid prevStart prevEnd)
                    )
                )
                ( concurrently
                    ( concurrently
                        (liftIO $ Charts.fetchMetricsData Charts.DTMetric (parseQ "| summarize count(*) by bin_auto(timestamp), resource___service___name") currentTime (Just startTime) (Just currentTime) ctx Nothing)
                        (liftIO $ Charts.fetchMetricsData Charts.DTMetric (parseQ "status_code == \"ERROR\" | summarize count(*) by bin_auto(timestamp), resource___service___name") currentTime (Just startTime) (Just currentTime) ctx Nothing)
                    )
                    (Issues.selectIssues pid Nothing (Just False) Nothing 100 0 (Just (startTime, currentTime)) Nothing "7d" [] [])
                )
            )
        )
    let slowDbQueries = V.fromList slowDbQueriesL
        anomalies' = V.fromList $ Issues.toIssueSummary <$> anomalies
        totalErrors = sum $ map (\(_, x, _) -> x) stats
        totalEvents = sum $ map (\(_, _, x) -> x) stats
        totalErrorsPrev = sum $ map (\(_, x, _) -> x) statsPrev
        totalEventsPrev = sum $ map (\(_, _, x) -> x) statsPrev
        errorsChange' = if totalErrorsPrev == 0 then 0.00 else fromIntegral (totalErrors - totalErrorsPrev) / fromIntegral totalErrorsPrev * 100
        eventsChange' = if totalEventsPrev == 0 then 0.00 else fromIntegral (totalEvents - totalEventsPrev) / fromIntegral totalEventsPrev * 100
        errorsChange = fromIntegral (round (errorsChange' * 100)) / 100
        eventsChange = fromIntegral (round (eventsChange' * 100)) / 100
        spanStatsDiff = RP.getSpanTypeStats statsBySpanType statsBySpanTypePrev
        endpointPerformance = RP.computeDurationChanges endpointStats endpointStatsPrev
    let rp_json = RP.buildReportJson' totalEvents totalErrors eventsChange errorsChange spanStatsDiff endpointPerformance slowDbQueries chartDataEvents chartDataErrors anomalies'
    timeZone <- liftIO getCurrentTimeZone
    reportId <- UUIDId <$> liftIO UUIDV4.nextRandom
    let report =
          Issues.Report
            { id = reportId
            , reportJson = rp_json
            , createdAt = utcToZonedTime timeZone currentTime
            , updatedAt = utcToZonedTime timeZone currentTime
            , projectId = pid
            , startTime = startTime
            , endTime = currentTime
            , reportType = typTxt
            }
    res <- Issues.addReport report
    Log.logInfo "Completed report generation for" pid
    unless ((typTxt == "daily" && not pr.dailyNotif) || (typTxt == "weekly" && not pr.weeklyNotif)) $ do
      Log.logInfo "Sending report notifications for" pid
      let stmTxt = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" startTime
          currentTimeTxt = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" currentTime
          reportUrl = ctx.env.hostUrl <> "p/" <> pid.toText <> "/reports/" <> report.id.toText
          eventsWidget = RP.eventsWidget
          errorsWidget = RP.errorsWidget
      allQ <- Widget.widgetPngUrl ctx.env.apiKeyEncryptionSecretKey ctx.env.hostUrl pid eventsWidget Nothing (Just stmTxt) (Just currentTimeTxt)
      errQ <- Widget.widgetPngUrl ctx.env.apiKeyEncryptionSecretKey ctx.env.hostUrl pid errorsWidget Nothing (Just stmTxt) (Just currentTimeTxt)
      let alert = ReportAlert typTxt stmTxt currentTimeTxt totalErrors totalEvents (V.fromList stats) reportUrl allQ errQ

      Relude.when pr.weeklyNotif $ forM_ pr.notificationsChannel \case
        Projects.NDiscord -> void $ sendDiscordAlert alert pid pr.title Nothing
        Projects.NSlack -> void $ sendSlackAlert alert pid pr.title Nothing
        Projects.NPhone -> do
          sendWhatsAppAlert alert pid pr.title pr.whatsappNumbers
        _ -> do
          totalRequest <- LogQueries.getLastSevenDaysTotalRequest pid
          Relude.when (totalRequest > 0) do
            patterns <- LogPatterns.getLogPatterns pid 10 0
            let dayEnd = show $ localDay (zonedTimeToLocalTime (utcToZonedTime timeZone currentTime))
                sevenDaysAgoUTCTime = addUTCTime (negate $ 6 * 86400) currentTime
                dayStart = show $ localDay (zonedTimeToLocalTime (utcToZonedTime timeZone sevenDaysAgoUTCTime))
                (errTotal, apiTotal, qTotal, lpTotal, rcTotal) = RP.anomalyTypeCounts (.issueType) anomalies'
                topPatterns = V.fromList $ patterns <&> \p -> (p.logPattern, p.occurrenceCount, LogPatterns.sourceFieldLabel p.sourceField)
            forM_ users \user -> do
              let reportData =
                    ET.WeeklyReportData
                      { userName = user.firstName
                      , projectName = pr.title
                      , reportUrl = ctx.env.hostUrl <> "p/" <> pid.toText <> "/reports/" <> report.id.toText
                      , projectUrl = ctx.env.hostUrl <> "p/" <> pid.toText
                      , startDate = dayStart
                      , endDate = dayEnd
                      , eventsChartUrl = allQ
                      , errorsChartUrl = errQ
                      , totalEvents
                      , totalErrors
                      , eventsChangePct = eventsChange
                      , errorsChangePct = errorsChange
                      , runtimeErrorsCount = errTotal
                      , apiChangesCount = apiTotal
                      , alertsCount = qTotal
                      , logPatternCount = lpTotal
                      , rateChangeCount = rcTotal
                      , anomalies = anomalies'
                      , performance = endpointPerformance
                      , slowQueries = slowDbQueries
                      , topPatterns
                      , freeTierExceeded = pr.paymentPlan == "FREE" && totalRequest > 5000
                      }
                  (subj, html) = ET.weeklyReportEmail reportData
              sendRenderedEmail (CI.original user.email) subj (ET.renderEmail subj html)
      Log.logInfo "Completed sending report notifications for" pid


-- | Process new anomalies detected by database triggers
-- This job is created by the apis.new_anomaly_proc() stored procedure
-- when new entities (endpoints, shapes, fields, formats, errors) are inserted.
--
-- Anomaly Processing Strategy:
-- 1. API Changes (endpoint/shape/format) -> Group by endpoint into single issue
-- 2. Runtime Exceptions -> Create individual issues for each error
-- 3. All issues are queued for LLM enhancement if configured
-- 4. Notifications are sent based on project settings
newAnomalyJob :: Projects.ProjectId -> ZonedTime -> Text -> Text -> V.Vector Text -> ATBackgroundCtx ()
newAnomalyJob pid createdAt anomalyTypesT anomalyActionsT targetHashes = do
  authCtx <- ask @Config.AuthContext
  let anomalyType = fromMaybe (error "parseAnomalyTypes returned Nothing") $ Anomalies.parseAnomalyTypes anomalyTypesT
  Log.logTrace "Processing new anomalies" ()
  case anomalyType of
    -- API Change anomalies (endpoint, shape, format) - group into single issue per endpoint
    -- This prevents notification spam when multiple related changes occur
    Anomalies.ATEndpoint -> processAPIChangeAnomalies pid targetHashes
    Anomalies.ATShape -> processAPIChangeAnomalies pid targetHashes
    Anomalies.ATFormat -> processAPIChangeAnomalies pid targetHashes
    -- Runtime exceptions get individual issues
    -- Each unique error pattern gets its own issue for tracking
    _ -> pass


-- | Process API change anomalies (endpoint, shape, format) into unified APIChange issues
-- This function groups related anomalies by endpoint to prevent notification spam.
-- For example, if a new endpoint is added with 5 fields and 2 formats, instead of
-- creating 8 separate issues, we create 1 issue that encompasses all changes.
--
-- Grouping Strategy:
-- 1. All anomalies are grouped by their endpoint hash
-- 2. If an open issue exists for that endpoint, update it with new anomalies
-- 3. Otherwise, create a new issue containing all anomalies for that endpoint
processAPIChangeAnomalies :: Projects.ProjectId -> V.Vector Text -> ATBackgroundCtx ()
processAPIChangeAnomalies pid targetHashes = do
  authCtx <- ask @Config.AuthContext

  -- Get all anomalies
  anomaliesList <- Anomalies.getAnomaliesVM pid targetHashes
  let anomaliesVM = V.fromList anomaliesList

  -- Group by endpoint hash to consolidate related changes
  let anomaliesByEndpoint = groupAnomaliesByEndpointHash anomaliesVM

  -- Process each endpoint group, collecting info for newly created issues only
  newEndpointInfos <-
    catMaybes <$> forM anomaliesByEndpoint \(endpointHash, anomalies) -> do
      existingIssueM <- Issues.findOpenIssueForEndpoint pid endpointHash
      case existingIssueM of
        Just existingIssue -> do
          let allNewFields = V.concatMap (.shapeNewUniqueFields) anomalies
              allDeletedFields = V.concatMap (.shapeDeletedFields) anomalies
              allModifiedFields = V.concatMap (.shapeUpdatedFieldFormats) anomalies
              hasNewEndpoint = V.any ((== Anomalies.ATEndpoint) . (.anomalyType)) anomalies
              hasChanges = hasNewEndpoint || not (V.null allNewFields && V.null allDeletedFields && V.null allModifiedFields)
          Relude.when hasChanges do
            -- All endpoint_* fields on an anomaly come from the same joined apis.endpoints
            -- row, so they always co-vary. Matching the create path (Issues.createAPIChangeIssue),
            -- we take anomaly[0]'s fields rather than searching for the first non-null per column.
            let firstAnom = V.head anomalies
                apiChangeData =
                  Issues.APIChangeData
                    { endpointMethod = fromMaybe "UNKNOWN" firstAnom.endpointMethod
                    , endpointPath = fromMaybe "/" firstAnom.endpointUrlPath
                    , endpointHost = fromMaybe "Unknown" firstAnom.endpointHost
                    , anomalyHashes = V.map (.targetHash) anomalies
                    , shapeChanges = V.empty
                    , formatChanges = V.empty
                    , newFields = allNewFields
                    , deletedFields = allDeletedFields
                    , modifiedFields = allModifiedFields
                    }
            Issues.updateIssueWithNewAnomaly existingIssue.id apiChangeData
          pure Nothing
        Nothing -> do
          let allNewFields = V.concatMap (.shapeNewUniqueFields) anomalies
              allDeletedFields = V.concatMap (.shapeDeletedFields) anomalies
              allModifiedFields = V.concatMap (.shapeUpdatedFieldFormats) anomalies
              hasNewEndpoint = V.any ((== Anomalies.ATEndpoint) . (.anomalyType)) anomalies
              hasChanges = hasNewEndpoint || not (V.null allNewFields && V.null allDeletedFields && V.null allModifiedFields)
          if not hasChanges
            then pure Nothing
            else do
              issue <- Issues.createAPIChangeIssue pid endpointHash anomalies
              Issues.insertIssue issue
              _ <- liftIO $ withResource authCtx.jobsPool \conn ->
                createJob conn "background_jobs" $ BackgroundJobs.EnhanceIssuesWithLLM pid (V.singleton issue.id)
              let firstAnom = V.head anomalies
                  label = fromMaybe "UNKNOWN" firstAnom.endpointMethod <> " " <> fromMaybe "/" firstAnom.endpointUrlPath
              pure $ Just ET.EndpointAlertRow{label, host = firstAnom.endpointHost, service = firstAnom.endpointServiceName, environment = firstAnom.endpointEnvironment}

  -- Only send notifications for newly created issues, with 30-minute cooldown
  Relude.when (not (null newEndpointInfos) && not authCtx.config.pauseNotifications) do
    now <- Time.currentTime
    recentIssueCount :: [Int] <-
      Hasql.interp
        [HI.sql| SELECT COUNT(*)::int FROM apis.issues
            WHERE project_id = #{pid} AND issue_type = 'api_change'
              AND created_at >= #{now}::timestamptz - INTERVAL '30 minutes'
              AND created_at < #{now}::timestamptz - INTERVAL '1 minute' |]
    Relude.unless (any (> 0) recentIssueCount) do
      projectM <- Projects.projectById pid
      whenJust projectM \project -> do
        users <- Projects.usersByProjectId pid
        Relude.when project.endpointAlerts do
          let alert = EndpointAlert{project = project.title, endpoints = V.fromList newEndpointInfos, endpointHash = fromMaybe "" $ viaNonEmpty head $ V.toList targetHashes}
          forM_ project.notificationsChannel \case
            Projects.NSlack -> void $ sendSlackAlert alert pid project.title Nothing
            Projects.NDiscord -> void $ sendDiscordAlert alert pid project.title Nothing
            Projects.NPhone -> sendWhatsAppAlert alert pid project.title project.whatsappNumbers
            Projects.NPagerduty -> pass
            Projects.NEmail -> do
              forM_ users \u -> do
                let anomalyUrl = authCtx.env.hostUrl <> "p/" <> pid.toText <> "/issues"
                    (subj, html) = ET.anomalyEndpointEmail u.firstName project.title anomalyUrl newEndpointInfos
                sendRenderedEmail (CI.original u.email) subj (ET.renderEmail subj html)


-- | Group anomalies by endpoint hash
groupAnomaliesByEndpointHash :: V.Vector Anomalies.AnomalyVM -> [(Text, V.Vector Anomalies.AnomalyVM)]
groupAnomaliesByEndpointHash anomalies =
  let getEndpointHash a = case a.anomalyType of
        Anomalies.ATEndpoint -> a.targetHash
        _ -> T.take 8 a.targetHash
      sorted = sortOn getEndpointHash $ V.toList anomalies
      grouped = groupBy (\a b -> getEndpointHash a == getEndpointHash b) sorted
   in mapMaybe
        ( \grp -> case viaNonEmpty head grp of
            Just h -> Just (getEndpointHash h, V.fromList grp)
            Nothing -> Nothing
        )
        grouped


-- | Process issues enhancement job - finds issues that need LLM enhancement
processIssuesEnhancement :: UTCTime -> ATBackgroundCtx ()
processIssuesEnhancement scheduledTime = do
  ctx <- ask @Config.AuthContext
  let oneHourAgo = addUTCTime (-3600) scheduledTime

  -- Find issues created in the last hour that haven't been enhanced yet
  issuesToEnhance <-
    V.fromList
      <$> Hasql.interp
        [HI.sql| SELECT id, project_id
              FROM apis.issues
              WHERE created_at >= #{oneHourAgo} AND created_at < #{scheduledTime}
                AND llm_enhanced_at IS NULL
                AND acknowledged_at IS NULL
                AND archived_at IS NULL
              ORDER BY project_id
              LIMIT 100 |]

  Log.logTrace "Found issues to enhance with LLM" (V.length issuesToEnhance)

  -- Group issues by project (SQL already sorts by project_id, but sort defensively for V.groupBy)
  let issuesByProject = V.groupBy (\a b -> snd a == snd b) $ V.modify (VA.sortBy (comparing snd)) issuesToEnhance

  -- Create enhancement jobs for each project
  liftIO $ withResource ctx.jobsPool \conn ->
    forM_ issuesByProject \projectIssues -> case V.uncons projectIssues of
      Nothing -> pass
      Just ((_, pid), _) -> do
        let issueIds = V.map (UUIDId . fst) projectIssues
        void $ createJob conn "background_jobs" $ BackgroundJobs.EnhanceIssuesWithLLM pid issueIds


-- | Enhance issues with LLM-generated titles and descriptions
enhanceIssuesWithLLM :: Projects.ProjectId -> V.Vector Issues.IssueId -> ATBackgroundCtx ()
enhanceIssuesWithLLM pid issueIds = do
  ctx <- ask @Config.AuthContext

  -- Check if OpenAI API key is configured
  if T.null ctx.config.openaiApiKey
    then Log.logAttention "OpenAI API key not configured, skipping issue enhancement" pid
    else do
      Log.logInfo "Enhancing issues with LLM" (pid.toText, V.length issueIds)

      -- Process each issue
      forM_ issueIds \issueId -> do
        issueM <- Issues.selectIssueById issueId
        case issueM of
          Nothing -> Log.logTrace "Issue not found for enhancement (likely cleaned up by endpoint canonicalization)" issueId
          Just issue -> do
            -- Call LLM to enhance the issue based on type
            enhancementResult <- Enhancement.enhanceIssueWithLLM ctx issue
            case enhancementResult of
              Left err -> Log.logAttention "Failed to enhance issue with LLM" (issueId, err)
              Right enhancement -> do
                -- Update the issue with enhanced data
                _ <-
                  Issues.updateIssueEnhancement
                    enhancement.issueId
                    enhancement.enhancedTitle
                    enhancement.recommendedAction
                    enhancement.migrationComplexity

                -- Also classify and update criticality
                criticalityResult <- Enhancement.classifyIssueCriticality ctx issue
                case criticalityResult of
                  Left err -> Log.logAttention "Failed to classify issue criticality" (issueId, err)
                  Right (isCritical, breakingCount, incrementalCount) -> do
                    _ <- Enhancement.updateIssueClassification issue.id isCritical breakingCount incrementalCount
                    Log.logInfo "Successfully enhanced and classified issue" (issueId, isCritical, breakingCount)

                -- Analyze error patterns for root cause and category
                analysisResult <- Enhancement.analyzeErrorPattern ctx issue
                case analysisResult of
                  Left _ -> pass -- not a runtime exception or LLM failure
                  Right (rootCause, category) -> do
                    epM <- ErrorPatterns.getErrorPatternByHash pid issue.targetHash
                    for_ epM \ep -> void $ ErrorPatterns.updateErrorPatternAnalysis ep.id rootCause category

                Log.logInfo "Successfully enhanced issue" issueId


patternEmbeddingAndMerge :: Projects.ProjectId -> ATBackgroundCtx ()
patternEmbeddingAndMerge pid = do
  ctx <- ask @Config.AuthContext
  if T.null ctx.config.openaiApiKey
    then Log.logAttention "OpenAI API key not configured, skipping pattern embedding" pid
    else do
      tryLog "errorPatternEmbedding" $ embedAndMergeErrors pid ctx
      tryLog "logPatternEmbedding" $ embedAndMergeLogPatterns pid ctx


embedAndMergeErrors :: Projects.ProjectId -> Config.AuthContext -> ATBackgroundCtx ()
embedAndMergeErrors pid ctx = do
  unembedded <- PatternMergeDB.getUnembeddedErrorPatterns pid
  embedAndMerge
    pid
    ctx
    MergeConfig
      { label = "error"
      , items = unembedded
      , itemText = \(_, et, msg) -> PatternMerge.embeddingTextForError et msg
      , normalizeEmb = PatternMerge.normalizeErrorForEmbedding
      , toId = view _1
      , updateEmbs = PatternMergeDB.updateErrorEmbeddings
      , getCentroids = PatternMergeDB.getCanonicalErrorPatterns pid
      , assignCanonical = PatternMergeDB.assignErrorsToCanonical
      , fetchTexts = PatternMergeDB.fetchErrorTexts
      , canMerge = PatternMerge.errorCanMerge
      , judgeFn = mkJudge PatternMerge.buildErrorJudgePrompt
      , onCanonicalPath = \_ _ -> pass
      , verifyMerge = Nothing
      }


embedAndMergeLogPatterns :: Projects.ProjectId -> Config.AuthContext -> ATBackgroundCtx ()
embedAndMergeLogPatterns pid ctx = do
  unembedded <- PatternMergeDB.getUnembeddedLogPatterns pid
  embedAndMerge
    pid
    ctx
    MergeConfig
      { label = "log"
      , items = unembedded
      , itemText = snd
      , normalizeEmb = PatternMerge.normalizeForEmbedding
      , toId = fst
      , updateEmbs = PatternMergeDB.updateLogEmbeddings
      , getCentroids = PatternMergeDB.getCanonicalLogPatterns pid
      , assignCanonical = PatternMergeDB.assignLogsToCanonical
      , fetchTexts = PatternMergeDB.fetchLogTexts
      , canMerge = PatternMerge.logCanMerge
      , judgeFn = mkJudge PatternMerge.buildLogClusterJudgePrompt
      , onCanonicalPath = \_ _ -> pass
      , verifyMerge = Just PatternMergeDB.fetchLogSamples
      }


-- | Build a judge from a prompt builder. Calls LLM and parses the response.
mkJudge :: ([(Text, Text)] -> Text) -> [(Text, Text)] -> ATBackgroundCtx (Either Text [(Int, Bool, Maybe Text)])
mkJudge buildPrompt pairs = do
  ctx <- ask @Config.AuthContext
  ELLM.callLLM ctx.config.openaiSmallModel (buildPrompt pairs) ctx.config.openaiApiKey <&> fmap PatternMerge.parseJudgeResponse


data MergeConfig k a = MergeConfig
  { label :: Text
  , items :: [a]
  , itemText :: a -> Text
  , normalizeEmb :: Text -> Text -- normalize text before embedding (e.g. unify placeholders)
  , toId :: a -> k
  , updateEmbs :: [(k, [Float])] -> ATBackgroundCtx Int64
  , getCentroids :: ATBackgroundCtx [(k, [Float])]
  , assignCanonical :: [(k, k)] -> ATBackgroundCtx Int64
  , fetchTexts :: [k] -> ATBackgroundCtx (Map k Text)
  , canMerge :: Text -> Text -> Bool
  , judgeFn :: [(Text, Text)] -> ATBackgroundCtx (Either Text [(Int, Bool, Maybe Text)])
  , onCanonicalPath :: k -> Text -> ATBackgroundCtx ()
  , verifyMerge :: Maybe ([k] -> ATBackgroundCtx (Map k Text))
  }


embedAndMerge :: Ord k => Projects.ProjectId -> Config.AuthContext -> MergeConfig k a -> ATBackgroundCtx ()
embedAndMerge pid ctx cfg = unless (null cfg.items) do
  let docs = map (\item -> Langchain.DocumentLoader.Core.Document (toLazy $ cfg.normalizeEmb $ cfg.itemText item) mempty) cfg.items
  embedResult <- ELLM.embedDocuments (embeddingConfig ctx) docs
  case embedResult of
    Left err -> Log.logAttention ("Failed to embed " <> cfg.label <> " patterns") (pid, err)
    Right embeddingsList -> do
      void $ cfg.updateEmbs $ zipWith (\item emb -> (cfg.toId item, emb)) cfg.items embeddingsList
      allCentroids <- cfg.getCentroids
      let newIds = S.fromList $ map cfg.toId cfg.items
          centroids = filter (\(cid, _) -> not $ S.member cid newIds) allCentroids
          newWithEmbs = zip (map cfg.toId cfg.items) embeddingsList
          !(autoMerges, ambiguous) = PatternMerge.assignToCentroids centroids newWithEmbs
      let newTextMap = Map.fromList $ map (\item -> (cfg.toId item, cfg.itemText item)) cfg.items
          neededIds = ordNub $ map snd autoMerges <> map snd ambiguous
      textMap <- if null neededIds then pure mempty else cfg.fetchTexts neededIds
      let allTexts = newTextMap <> textMap
          validMerge (newId, canId) = fromMaybe False $ cfg.canMerge <$> Map.lookup newId allTexts <*> Map.lookup canId allTexts
          !validAutoMerges = filter validMerge autoMerges
      -- Verify auto-merges against sample logs when verification is available
      verifiedAutoMerges <- case cfg.verifyMerge of
        Nothing -> pure validAutoMerges
        Just fetchSamples -> do
          let mergeNewIds = map fst validAutoMerges
          samples <- fetchSamples mergeNewIds
          pure $ filter (\(newId, canId) -> fromMaybe True $ PatternMerge.verifyMergeDecision <$> Map.lookup canId allTexts <*> Map.lookup newId samples) validAutoMerges
      void $ cfg.assignCanonical verifiedAutoMerges
      let !validAmbiguous = filter validMerge ambiguous
      unless (null validAmbiguous) do
        let pairs = mapMaybe (\(newId, canId) -> (,) <$> Map.lookup newId allTexts <*> Map.lookup canId allTexts) validAmbiguous
        unless (null pairs) do
          judgeResult <- cfg.judgeFn pairs
          case judgeResult of
            Left err -> Log.logAttention ("LLM judge failed for " <> cfg.label <> " patterns") (pid, err)
            Right decisions -> do
              let ambiguousV = V.fromList validAmbiguous
                  merges = mapMaybe (\(idx, shouldMerge, _) -> bool Nothing (ambiguousV V.!? idx) shouldMerge) decisions
              -- Verify LLM judge merges against sample logs
              verifiedMerges <- case cfg.verifyMerge of
                Nothing -> pure merges
                Just fetchSamples -> do
                  let mergeNewIds = map fst merges
                  samples <- fetchSamples mergeNewIds
                  pure $ filter (\(newId, canId) -> fromMaybe True $ PatternMerge.verifyMergeDecision <$> Map.lookup canId allTexts <*> Map.lookup newId samples) merges
              void $ cfg.assignCanonical verifiedMerges
              forM_ decisions \(idx, shouldMerge, mPath) ->
                when shouldMerge $ for_ ((,) . snd <$> (ambiguousV V.!? idx) <*> mPath) (uncurry cfg.onCanonicalPath)


embeddingConfig :: Config.AuthContext -> Langchain.Embeddings.OpenAI.OpenAIEmbeddings
embeddingConfig ctx =
  Langchain.Embeddings.OpenAI.defaultOpenAIEmbeddings
    { Langchain.Embeddings.OpenAI.apiKey = ctx.config.openaiApiKey
    , Langchain.Embeddings.OpenAI.baseUrl = if T.null ctx.config.openaiBaseUrl then Just "https://api.openai.com/v1" else Just (toString ctx.config.openaiBaseUrl)
    }


-- | Only merge endpoints with the same number of path segments.
-- Prevents e.g. /api/v2/users/auth0|abc from merging into /api/v2/users.
--
-- >>> sameSegmentCount "/api/v1/users/{param}" "/api/v1/users/123"
-- True
--
-- >>> sameSegmentCount "/api/users" "/api/users/{param}"
-- False
sameSegmentCount :: Text -> Text -> Bool
sameSegmentCount a b = T.count "/" a == T.count "/" b


-- Endpoint template discovery ---------------------------------------------------

endpointTemplateDiscovery :: Projects.ProjectId -> ATBackgroundCtx ()
endpointTemplateDiscovery pid = do
  -- Step 1: Deterministic template discovery via tokenization grouping
  endpoints <- Endpoints.getUnmergedEndpoints pid
  unless (null endpoints) do
    let grouped =
          HM.toList
            $ HM.fromListWith (<>)
            $ map
              ( \(h, m, host, path) ->
                  let tp = T.intercalate "/" $ map (\t -> bool "{param}" t (t /= "<*>")) $ V.toList $ tokenizeUrlPath path
                   in ((m, host, tp), [h])
              )
              endpoints
        (!allUpdates, !allInserts) =
          foldMap
            ( \((method, host, tp), hs) ->
                let ch = toXXHash $ pid.toText <> host <> method <> tp
                 in (map (,ch,tp) hs, [(pid, tp, method, host, ch)])
            )
            $ filter (\((_, _, tp), hs) -> length hs >= 2 && T.isInfixOf "{param}" tp) grouped
    void $ Endpoints.setEndpointCanonical allUpdates
    Endpoints.insertCanonicalEndpoints allInserts
    Log.logInfo "Endpoint template discovery complete" ("project_id", pid.toText, "endpoint_count", length endpoints)
  -- Step 2: Embedding + LLM merge for remaining ambiguous endpoints
  ctx <- ask @Config.AuthContext
  unless (T.null ctx.config.openaiApiKey) $ tryLog "endpointEmbedding" do
    unembedded <- Endpoints.getUnembeddedEndpoints pid
    embedAndMerge
      pid
      ctx
      MergeConfig
        { label = "endpoint"
        , items = unembedded
        , itemText = \(_, _, path) -> path
        , normalizeEmb = id
        , toId = view _1
        , updateEmbs = Endpoints.updateEndpointEmbeddings
        , getCentroids = Endpoints.getCanonicalEndpoints pid
        , assignCanonical = Endpoints.assignEndpointsToCanonical
        , fetchTexts = Endpoints.fetchEndpointTexts
        , canMerge = sameSegmentCount
        , judgeFn = mkJudge PatternMerge.buildEndpointJudgePrompt
        , onCanonicalPath = \eid path -> void $ Endpoints.setEndpointCanonicalTemplate eid path
        , verifyMerge = Nothing
        }
  -- Step 3: Migrate data and delete all merged endpoints
  mergedPairs <- Endpoints.getMergedEndpointPairs pid
  Log.logInfo "Endpoint merge cleanup starting" ("project_id", pid.toText, "merged_count", length mergedPairs)
  Endpoints.migrateAndDeleteMergedEndpoints mergedPairs
  unless (null mergedPairs)
    $ Log.logInfo "Cleaned up merged endpoints" ("project_id", pid.toText, "merged_count", length mergedPairs)


-- | Get access token for GitHub sync (either PAT or GitHub App installation token)
getGitSyncToken :: (IOE :> es, W.HTTP :> es) => Config.EnvConfig -> GitSync.GitHubSync -> Eff es (Either Text Text)
getGitSyncToken config sync = case (sync.installationId, sync.accessToken) of
  (Just instId, _) ->
    GitHub.getInstallationToken config.githubAppId config.githubAppPrivateKey instId <&> \case
      Left err -> Left $ "Failed to get installation token: " <> err
      Right tok -> Right tok.token
  (_, Just token) -> pure $ Right token
  (Nothing, Nothing) -> pure $ Left "No authentication method configured"


-- | Sync dashboards from GitHub repo to Monoscope
gitSyncFromRepo :: Projects.ProjectId -> ATBackgroundCtx ()
gitSyncFromRepo pid = do
  Log.logInfo "Starting GitHub sync for project" pid
  ctx <- ask @Config.AuthContext
  let encKey = encodeUtf8 ctx.config.apiKeyEncryptionSecretKey
  syncM <- GitSync.getGitHubSyncDecrypted encKey pid
  case syncM of
    Nothing -> Log.logAttention "No GitHub sync configured for project" pid
    Just sync | not sync.syncEnabled -> Log.logInfo "GitHub sync disabled for project" pid
    Just sync -> W.runHTTPWreq do
      tokenResult <- getGitSyncToken ctx.config sync
      case tokenResult of
        Left err -> Log.logAttention "Failed to get GitHub token" (pid, err)
        Right token -> do
          treeResult <- GitSync.fetchGitTree token sync
          case treeResult of
            Left err -> Log.logAttention "Failed to fetch git tree" (pid, err)
            Right (treeSha, entries)
              | sync.lastTreeSha == Just treeSha -> Log.logInfo "Tree unchanged, skipping sync" (pid, treeSha)
              | otherwise -> do
                  dbState <- GitSync.getDashboardGitState pid
                  allTeams <- ProjectMembers.getTeamsVM pid
                  let teamMap = Map.fromList [(t.handle, t.id) | t <- allTeams]
                      prefix = GitSync.getDashboardsPath sync
                      actions = GitSync.buildSyncPlan prefix entries dbState
                      creates = [a | a@GitSync.SyncCreate{} <- actions]
                      updates = [a | a@GitSync.SyncUpdate{} <- actions]
                      deletes = [(path, dashId) | GitSync.SyncDelete path dashId <- actions]
                  Log.logInfo "Git sync plan" ("creates" :: Text, length creates, "updates" :: Text, length updates, "deletes" :: Text, length deletes)
                  -- Fetch file contents in parallel for creates and updates
                  let fetchActions = creates <> updates
                  Ki.scoped \scope -> do
                    forM_ fetchActions $ Ki.fork scope . processGitSyncAction pid token sync teamMap
                    Ki.atomically $ Ki.awaitAll scope
                  -- Process deletes (no HTTP needed)
                  forM_ deletes \(path, dashId) -> do
                    _ <- Dashboards.deleteDashboard dashId
                    Log.logInfo "Deleted dashboard (removed from git)" (path, dashId)
                  _ <- GitSync.updateLastTreeSha sync.id treeSha
                  Log.logInfo "Completed GitHub sync for project" pid


processGitSyncAction :: (DB es, Log :> es, Time.Time :> es, W.HTTP :> es) => Projects.ProjectId -> Text -> GitSync.GitHubSync -> Map.Map Text UUID.UUID -> GitSync.SyncAction -> Eff es ()
processGitSyncAction pid token sync teamMap = \case
  GitSync.SyncCreate path sha ->
    fetchAndParseDashboard token sync path >>= either (Log.logAttention "Failed to sync dashboard from git" . (path,)) \schema -> do
      now <- Time.currentTime
      dashId <- UUIDId <$> liftIO UUIDV4.nextRandom
      let prefix = GitSync.getDashboardsPath sync
          relativePath = fromMaybe path $ T.stripPrefix prefix path
          teamIds = mapMaybe (`Map.lookup` teamMap) (fold schema.teams)
          dashboard =
            Dashboards.DashboardVM
              { Dashboards.id = dashId
              , Dashboards.projectId = pid
              , Dashboards.createdAt = now
              , Dashboards.updatedAt = now
              , Dashboards.createdBy = Projects.UserId UUID.nil
              , Dashboards.baseTemplate = Just path
              , Dashboards.schema = Just schema
              , Dashboards.starredSince = Nothing
              , Dashboards.homepageSince = Nothing
              , Dashboards.tags = V.fromList $ fold schema.tags
              , Dashboards.title = fromMaybe "Untitled" schema.title
              , Dashboards.teams = V.fromList teamIds
              , Dashboards.filePath = Just relativePath
              , Dashboards.fileSha = Just sha
              }
      _ <- Dashboards.insert dashboard
      Log.logInfo "Created dashboard from git" (relativePath, dashId)
  GitSync.SyncUpdate path sha dashId ->
    fetchAndParseDashboard token sync path >>= either (Log.logAttention "Failed to sync dashboard from git" . (path,)) \schema -> do
      now <- Time.currentTime
      let prefix = GitSync.getDashboardsPath sync
          relativePath = fromMaybe path $ T.stripPrefix prefix path
      _ <- Dashboards.updateSchemaAndUpdatedAt dashId schema now
      whenJust schema.title $ void . Dashboards.updateTitle dashId
      _ <- Dashboards.updateTags dashId (V.fromList $ fold schema.tags)
      _ <- GitSync.updateDashboardGitInfo dashId relativePath sha
      Log.logInfo "Updated dashboard from git" (relativePath, dashId)
  GitSync.SyncRename path sha dashId -> do
    let prefix = GitSync.getDashboardsPath sync
        relativePath = fromMaybe path $ T.stripPrefix prefix path
    _ <- GitSync.updateDashboardGitInfo dashId relativePath sha
    Log.logInfo "Renamed dashboard path from git" (relativePath, dashId)
  GitSync.SyncDelete{} -> pass -- Handled separately


fetchAndParseDashboard :: (IOE :> es, W.HTTP :> es) => Text -> GitSync.GitHubSync -> Text -> Eff es (Either Text Dashboards.Dashboard)
fetchAndParseDashboard token sync path = GitSync.fetchFileContent token sync path <&> (>>= GitSync.yamlToDashboard)


-- | Push a dashboard change to GitHub
-- Skips template-based dashboards (schema = Nothing) since they have no custom content to sync.
gitSyncPushDashboard :: Projects.ProjectId -> Dashboards.DashboardId -> ATBackgroundCtx ()
gitSyncPushDashboard pid dashId = do
  Log.logInfo "Pushing dashboard to GitHub" (pid, dashId)
  ctx <- ask @Config.AuthContext
  let encKey = encodeUtf8 ctx.config.apiKeyEncryptionSecretKey
  syncM <- GitSync.getGitHubSyncDecrypted encKey pid
  dashM <- Dashboards.getDashboardById dashId
  case (syncM, dashM) of
    (Nothing, _) -> Log.logAttention "No GitHub sync configured for project" pid
    (Just sync, _) | not sync.syncEnabled -> Log.logInfo "GitHub sync disabled, skipping push" pid
    (_, Nothing) -> Log.logAttention "Dashboard not found for git push" dashId
    (_, Just dash) | isNothing dash.schema -> Log.logInfo "Skipping git push for template-based dashboard" dashId
    (Just sync, Just dash) -> W.runHTTPWreq do
      tokenResult <- getGitSyncToken ctx.config sync
      case tokenResult of
        Left err -> Log.logAttention "Failed to get GitHub token" (pid, err)
        Right token -> do
          teams <- ProjectMembers.getTeamsById pid dash.teams
          let schema = GitSync.buildSchemaWithMeta dash.schema dash.title (V.toList dash.tags) (map (.handle) teams)
              yamlContent = GitSync.dashboardToYaml schema
              prefix = GitSync.getDashboardsPath sync
              -- Strip any accidental prefix from DB path to ensure we don't get dashboards/dashboards/...
              rawPath = fromMaybe (GitSync.titleToFilePath dash.title) dash.filePath
              relativePath = fromMaybe rawPath $ T.stripPrefix "dashboards/" rawPath <|> T.stripPrefix prefix rawPath
              fullPath = prefix <> relativePath
              existingSha = dash.fileSha
              message = "Update dashboard: " <> dash.title
          pushResult <- GitSync.pushFileToGit token sync fullPath yamlContent existingSha message
          case pushResult of
            Left err -> Log.logAttention "Failed to push dashboard to git" (dashId, err)
            Right (fileSha, treeSha) -> do
              _ <- GitSync.updateDashboardGitInfo dashId relativePath fileSha
              _ <- GitSync.updateLastTreeSha sync.id treeSha
              Log.logInfo "Successfully pushed dashboard to git" (dashId, fileSha)


-- | Push all dashboards from a project to GitHub (used after initial repo connection)
-- Skips template-based dashboards (schema = Nothing) since they have no custom content to sync.
gitSyncPushAllDashboards :: Projects.ProjectId -> ATBackgroundCtx ()
gitSyncPushAllDashboards pid = do
  Log.logInfo "Pushing all dashboards to GitHub" pid
  ctx <- ask @Config.AuthContext
  let encKey = encodeUtf8 ctx.config.apiKeyEncryptionSecretKey
  syncM <- GitSync.getGitHubSyncDecrypted encKey pid
  case syncM of
    Nothing -> Log.logAttention "No GitHub sync configured for project" pid
    Just sync | not sync.syncEnabled -> Log.logInfo "GitHub sync disabled, skipping push" pid
    Just sync -> W.runHTTPWreq do
      tokenResult <- getGitSyncToken ctx.config sync
      case tokenResult of
        Left err -> Log.logAttention "Failed to get GitHub token" (pid, err)
        Right token -> do
          dashboards <- Dashboards.selectDashboardsSortedBy pid "updated_at"
          let syncableDashboards = filter (isJust . (.schema)) dashboards
          Log.logInfo "Found dashboards to push" (pid, length syncableDashboards)
          let prefix = GitSync.getDashboardsPath sync
          forM_ syncableDashboards \dash -> do
            teams <- ProjectMembers.getTeamsById pid dash.teams
            let schema = GitSync.buildSchemaWithMeta dash.schema dash.title (V.toList dash.tags) (map (.handle) teams)
                yamlContent = GitSync.dashboardToYaml schema
                rawPath = fromMaybe (GitSync.titleToFilePath dash.title) dash.filePath
                relativePath = fromMaybe rawPath $ T.stripPrefix "dashboards/" rawPath <|> T.stripPrefix prefix rawPath
                fullPath = prefix <> relativePath
                existingSha = dash.fileSha
                message = "Sync dashboard: " <> dash.title
            pushResult <- GitSync.pushFileToGit token sync fullPath yamlContent existingSha message
            case pushResult of
              Left err -> Log.logAttention "Failed to push dashboard" (dash.id, err)
              Right (fileSha, treeSha) -> do
                _ <- GitSync.updateDashboardGitInfo dash.id relativePath fileSha
                _ <- GitSync.updateLastTreeSha sync.id treeSha
                Log.logInfo "Pushed dashboard" (dash.id, dash.title)
          Log.logInfo "Finished pushing all dashboards" pid


checkTriggeredQueryMonitors :: ATBackgroundCtx ()
checkTriggeredQueryMonitors = do
  Log.logTrace "Checking query monitors" ()
  monitors <- Monitors.getActiveQueryMonitors
  forM_ monitors \monitor -> do
    startWall <- Time.currentTime
    let evalInterval = startWall `diffUTCTime` monitor.lastEvaluated
    Relude.when (evalInterval >= fromIntegral monitor.checkIntervalMins * 60) $ do
      Log.logInfo "Evaluating query monitor" (monitor.id, monitor.alertConfig.title)
      -- Catch ANY synchronous exception: updateLastEvaluatedAt MUST run on failure
      -- to prevent an infinite re-evaluation loop hammering a failing backend.
      tryAny (evaluateQueryMonitor monitor startWall) >>= \case
        Right _ -> pass
        Left err -> do
          Log.logWarn "Query monitor evaluation failed" (monitor.id, monitor.alertConfig.title, show err)
          void $ Monitors.updateLastEvaluatedAt monitor.id startWall


evaluateQueryMonitor :: Monitors.QueryMonitor -> UTCTime -> ATBackgroundCtx ()
evaluateQueryMonitor monitor startWall = do
  ctx <- ask @Config.AuthContext
  let tfEnabled = ctx.config.enableTimefusionReads
      title = monitor.alertConfig.title
      -- Use the monitor's configured "Include rows from" time window.
      -- Fall back to 60 when unset/zero (e.g. def-constructed monitors).
      lookbackMins = if monitor.timeWindowMins > 0 then monitor.timeWindowMins else 60
      -- Re-parse on every evaluation so parser fixes apply to existing monitors
      -- without needing a DB migration of cached SQL.
      parseCfg = (defSqlQueryCfg monitor.projectId fixedUTCTime Nothing Nothing){presetRollup = Just "5m", alertLookbackMins = lookbackMins}
  -- Empty logQuery means no source KQL is tracked; trust the cached SQL.
  freshSql <-
    if T.null (T.strip monitor.logQuery)
      then pure monitor.logQueryAsSql
      else case parseQueryToComponents parseCfg monitor.logQuery of
        Right (_, qc) -> case qc.finalAlertQuery of
          Just q -> pure q
          Nothing -> do
            Log.logAttention "Monitor parsed but produced no alert query; using stale cached SQL" (monitor.id, title, monitor.logQuery)
            pure monitor.logQueryAsSql
        Left err -> do
          -- Escalate to logError: the user's saved KQL no longer parses, meaning
          -- the monitor is silently running stale SQL. On-call should see this.
          Log.logError "Monitor logQuery re-parse failed; using stale cached SQL" (monitor.id, title, monitor.logQuery, err)
          pure monitor.logQueryAsSql
  let isOtelQuery = "otel_logs_and_spans" `T.isInfixOf` freshSql
  start <- liftIO $ getTime Monotonic
  results <- Hasql.withHasqlTimefusion (tfEnabled && isOtelQuery) $ Hasql.interp (rawSql freshSql)
  end <- liftIO $ getTime Monotonic

  -- No rows in the lookback window:
  --   * greater-than monitor currently alerting/warning -> treat as 0 so it can recover
  --   * otherwise skip, to avoid spuriously firing (triggerLessThan=True) or
  --     spuriously recovering when data is simply missing.
  let durationNs = toNanoSecs (diffTimeSpec end start)
  case results :: [Double] of
    []
      | not monitor.triggerLessThan && monitor.currentStatus /= Monitors.MSNormal ->
          evaluateWithResults monitor startWall title 0 durationNs
    [] -> do
      Log.logInfo "Monitor query returned no rows in lookback window; skipping evaluation" (monitor.id, title, lookbackMins)
      void $ Monitors.updateLastEvaluatedAt monitor.id startWall
    (v : vs) -> evaluateWithResults monitor startWall title (foldr max v vs) durationNs


evaluateWithResults :: Monitors.QueryMonitor -> UTCTime -> Text -> Double -> Integer -> ATBackgroundCtx ()
evaluateWithResults monitor startWall title total durationNs = do
  ctx <- ask @Config.AuthContext
  let prevStatus = monitor.currentStatus
      status =
        monitorStatus
          monitor.triggerLessThan
          monitor.warningThreshold
          monitor.alertThreshold
          monitor.alertRecoveryThreshold
          monitor.warningRecoveryThreshold
          (prevStatus == Monitors.MSAlerting)
          (prevStatus == Monitors.MSWarning)
          total
      severity = case status of Monitors.MSAlerting -> SLError; Monitors.MSWarning -> SLWarn; _ -> SLInfo
      attrs =
        Map.fromList
          [ ("monitor.id", AE.toJSON monitor.id)
          , ("monitor.title", AE.toJSON title)
          , ("monitor.value", AE.toJSON total)
          , ("monitor.threshold", AE.toJSON monitor.alertThreshold)
          , ("monitor.warning_threshold", AE.toJSON monitor.warningThreshold)
          , ("monitor.status", AE.toJSON status)
          , ("monitor.query", AE.toJSON monitor.logQueryAsSql)
          , ("monitor.condition", AE.toJSON $ if monitor.triggerLessThan then "less_than" :: Text else "greater_than")
          ]
      otelLog =
        (mkSystemLog monitor.projectId "monitor.alert.triggered" severity (title <> ": " <> display status) attrs (Just $ fromIntegral durationNs) startWall)
          { Telemetry.kind = Just "alert"
          , Telemetry.parent_id = Just monitor.id.toText
          }
  insertSystemLog ctx.env.enableTimefusionWrites otelLog{Telemetry.summary = generateSummary otelLog}

  -- Determine notification intent before UPDATE so we can set timestamps correctly
  let isRecovery = status == Monitors.MSNormal && prevStatus /= Monitors.MSNormal
      statusChanged = status /= prevStatus
      hasRenotify = isJust monitor.renotifyIntervalMins
      reminderIntervalSecs = maybe 0 (fromIntegral . (* 60)) monitor.renotifyIntervalMins :: NominalDiffTime
      lastTriggered = monitor.alertLastTriggered <|> monitor.warningLastTriggered
      pastReminderWindow = maybe True (\lt -> diffUTCTime startWall lt >= reminderIntervalSecs) lastTriggered
      stoppedByCount = maybe False (monitor.notificationCount >=) monitor.stopAfterCount
      -- If notificationCount is 0 while in a non-normal state, the initial
      -- notification was suppressed (e.g. muted during the transition). Fire
      -- it now that we're no longer blocked.
      missedInitial = status /= Monitors.MSNormal && monitor.notificationCount == 0
      shouldNotify
        | isRecovery = True
        | stoppedByCount = False
        | statusChanged && status /= Monitors.MSNormal = True
        | missedInitial = True
        | not statusChanged && status /= Monitors.MSNormal && hasRenotify && pastReminderWindow = True
        | otherwise = False
      -- Update timestamps on transitions OR reminders (prevents spam after reminder window)
      warningAt = if status == Monitors.MSWarning && (statusChanged || shouldNotify) then Just startWall else monitor.warningLastTriggered
      alertAt = if status == Monitors.MSAlerting && (statusChanged || shouldNotify) then Just startWall else monitor.alertLastTriggered
      (finalWarningAt, finalAlertAt) = if isRecovery then (Nothing, Nothing) else (warningAt, alertAt)

  let isMuted = ctx.config.pauseNotifications || maybe False (> startWall) monitor.mutedUntil
      newNotifCount
        | isRecovery = 0
        | shouldNotify && not isMuted = monitor.notificationCount + 1
        | otherwise = monitor.notificationCount

  let monId = monitor.id
  void
    $ Hasql.interpExecute
      [HI.sql| UPDATE monitors.query_monitors
          SET current_status = #{status}, current_value = #{total}, last_evaluated = #{startWall},
              warning_last_triggered = #{finalWarningAt}, alert_last_triggered = #{finalAlertAt},
              notification_count = #{newNotifCount}
          WHERE id = #{monId} |]

  let reason :: Text
      reason
        | isRecovery = "recovery"
        | statusChanged = "status_changed"
        | missedInitial = "missed_initial"
        | otherwise = "reminder"
  Log.logInfo "Monitor notify decision" (monitor.id, title, status, total, shouldNotify, isMuted, reason)
  Relude.when (shouldNotify && not isMuted) do
    notifyQueryMonitorStatusChange monitor total isRecovery


-- | Determine monitor status with hysteresis support.
-- Args: triggerLessThan warnThreshold alertThreshold alertRecovery warnRecovery wasAlerting wasWarning value
-- For "above" (triggerLessThan=False): alert when value >= threshold, recover when value < recoveryThreshold
-- For "below" (triggerLessThan=True): alert when value <= threshold, recover when value > recoveryThreshold
--
-- Trigger tests (above direction):
-- >>> monitorStatus False Nothing 100 Nothing Nothing False False 100
-- MSAlerting
-- >>> monitorStatus False Nothing 100 Nothing Nothing False False 150
-- MSAlerting
-- >>> monitorStatus False Nothing 100 Nothing Nothing False False 99
-- MSNormal
--
-- Trigger tests (below direction):
-- >>> monitorStatus True Nothing 100 Nothing Nothing False False 100
-- MSAlerting
-- >>> monitorStatus True Nothing 100 Nothing Nothing False False 50
-- MSAlerting
-- >>> monitorStatus True Nothing 100 Nothing Nothing False False 101
-- MSNormal
--
-- Hysteresis: stays alerting until recovery threshold crossed (above)
-- >>> monitorStatus False Nothing 100 (Just 80) Nothing True False 95
-- MSAlerting
-- >>> monitorStatus False Nothing 100 (Just 80) Nothing True False 79
-- MSNormal
--
-- Hysteresis: stays alerting until recovery threshold crossed (below)
-- >>> monitorStatus True Nothing 100 (Just 120) Nothing True False 105
-- MSAlerting
-- >>> monitorStatus True Nothing 100 (Just 120) Nothing True False 121
-- MSNormal
--
-- Warning threshold with hysteresis
-- >>> monitorStatus False (Just 80) 100 Nothing (Just 60) False True 75
-- MSWarning
-- >>> monitorStatus False (Just 80) 100 Nothing (Just 60) False True 59
-- MSNormal
--
-- Edge case: recovery > threshold (above) - no hysteresis band
-- >>> monitorStatus False Nothing 100 (Just 120) Nothing True False 110
-- MSAlerting
-- >>> monitorStatus False Nothing 100 (Just 120) Nothing True False 99
-- MSNormal
--
-- Edge case: recovery < threshold (below) - no hysteresis band
-- >>> monitorStatus True Nothing 100 (Just 80) Nothing True False 95
-- MSAlerting
-- >>> monitorStatus True Nothing 100 (Just 80) Nothing True False 101
-- MSNormal
monitorStatus :: Bool -> Maybe Double -> Double -> Maybe Double -> Maybe Double -> Bool -> Bool -> Double -> Monitors.MonitorStatus
monitorStatus triggerLessThan warnThreshold alertThreshold alertRecovery warnRecovery wasAlerting wasWarning value
  | breached alertThreshold = Monitors.MSAlerting
  | maybe False breached warnThreshold = Monitors.MSWarning
  | wasAlerting && not (recovered alertRecovery alertThreshold) = Monitors.MSAlerting
  | wasWarning && not (recovered warnRecovery (fromMaybe alertThreshold warnThreshold)) = Monitors.MSWarning
  | otherwise = Monitors.MSNormal
  where
    breached t = if triggerLessThan then t >= value else t <= value
    recovered r t = if triggerLessThan then value > fromMaybe t r else value < fromMaybe t r


-- ============================================================================
-- Log Pattern Processing Jobs
-- ============================================================================

-- Tuning knobs for log pattern baseline and spike detection
baselineWindowHours, baselinePageSize, maxNewPatternsPerRun, stalePatternDays, staleNewPatternDays :: Int
baselineWindowHours = 48 -- 2 days
baselinePageSize = 1000
maxNewPatternsPerRun = 500
stalePatternDays = 30 -- prune acknowledged patterns older than this
staleNewPatternDays = 7 -- auto-acknowledge 'new' patterns older than this


minEventsForNewPatternAlerts :: Int64
minEventsForNewPatternAlerts = 2000 -- skip onboarding projects with <2k events/48h


minMedianForEstablished, minAgeDaysForEstablished, spikeZScoreThreshold, spikeMinAbsoluteDelta :: Double
minMedianForEstablished = 500 -- events/hour required for immediate "established"
minAgeDaysForEstablished = 3 -- days old enough for reliable baselines
spikeZScoreThreshold = 3.0 -- 99.7% confidence interval
spikeMinAbsoluteDelta = 50 -- avoids alerting on tiny volumes (e.g. 20→35/hr)


-- | Calculate baselines for log patterns
-- Uses hourly counts over the last 48 hours
calculateLogPatternBaselines :: Projects.ProjectId -> ATBackgroundCtx ()
calculateLogPatternBaselines pid = do
  Log.logTrace "Calculating log pattern baselines" pid
  now <- Time.currentTime
  allStats <- LogPatterns.getBatchPatternStats pid now baselineWindowHours
  let statsMap = HM.fromList $ map (\s -> ((s.sourceField, s.patternHash), s)) allStats
      go offset totalProcessed totalEstablished = do
        patterns <- LogPatterns.getLogPatterns pid baselinePageSize offset
        let ageDays lp = realToFrac (diffUTCTime now (zonedTimeToUTC lp.firstSeenAt)) / 86400 :: Double
            updates = V.fromList $ flip mapMaybe patterns \lp ->
              HM.lookup (lp.sourceField, lp.patternHash) statsMap <&> \stats ->
                let est = stats.hourlyMedian > minMedianForEstablished || ageDays lp >= minAgeDaysForEstablished
                 in (lp.sourceField, lp.patternHash, bool BSLearning BSEstablished est, stats.hourlyMedian, stats.hourlyMADScaled, stats.totalHours)
            established = V.length $ V.filter (\(_, _, s, _, _, _) -> s == BSEstablished) updates
        void $ LogPatterns.updateBaselineBatch pid updates
        let newTotal = totalProcessed + length patterns
            newEstablished = totalEstablished + established
        if length patterns == baselinePageSize
          then go (offset + baselinePageSize) newTotal newEstablished
          else pure (newTotal, newEstablished)
  (total, established) <- go 0 0 0
  Log.logTrace "Finished calculating log pattern baselines" ("patterns" :: Text, total, "established" :: Text, established)


-- | Pure spike/drop detection for a single pattern.
-- Uses a MAD floor of 1.0 so perfectly stable patterns (MAD=0) can still trigger.
detectSpikeOrDrop :: Double -> Double -> Double -> Double -> Int64 -> Maybe Issues.SpikeResult
detectSpikeOrDrop zThreshold minDelta mean mad currentCount
  | z > zThreshold && rate > mean + minDelta = Just $ Issues.SpikeResult rate mean effectiveMad z Issues.Spike
  | z < -zThreshold && rate < mean - minDelta = Just $ Issues.SpikeResult rate mean effectiveMad z Issues.Drop
  | otherwise = Nothing
  where
    rate = fromIntegral currentCount
    effectiveMad = max mad 1.0
    z = (rate - mean) / effectiveMad


-- | Detect log pattern volume spikes and create issues.
-- Projects partial-hour counts to hourly rate for sub-hourly detection.
detectLogPatternSpikes :: Projects.ProjectId -> UTCTime -> Config.AuthContext -> ATBackgroundCtx ()
detectLogPatternSpikes pid scheduledTime authCtx = do
  Log.logTrace "Detecting log pattern spikes" pid
  let totalSecs = truncate (utctDayTime scheduledTime) :: Int
      minutesIntoHour = max 15 $ (totalSecs `mod` 3600) `div` 60
      scaleFactor = 60.0 / fromIntegral minutesIntoHour :: Double
  -- Re-alerting: ON CONFLICT dedup only matches open (unacknowledged) issues,
  -- so a fresh issue is created if the prior one was acknowledged — intentional.
  patternsWithRates <- LogPatterns.getPatternsWithCurrentRates pid scheduledTime
  let anomalies = flip mapMaybe patternsWithRates \lpRate ->
        guard (lpRate.logLevel `notElem` [Just "INFO", Just "TRACE" :: Maybe Text])
          *> case (lpRate.baselineState, lpRate.baselineMean, lpRate.baselineMad) of
            (BSEstablished, Just mean, Just mad) ->
              let projectedCount = round (fromIntegral lpRate.currentHourCount * scaleFactor)
               in detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta mean mad projectedCount <&> (lpRate,)
            _ -> Nothing

  issueIds <- forM anomalies \(lpRate, sr) -> do
    let dir = display sr.direction
    Log.logInfo ("Log pattern " <> dir <> " detected") (lpRate.patternId, lpRate.logPattern, sr.currentRate, sr.mean)
    issue <- Issues.createLogPatternRateChangeIssue pid lpRate sr
    Issues.insertIssue issue
    Log.logInfo ("Created issue for log pattern " <> dir) (pid, lpRate.patternId, issue.id)
    pure issue.id
  unless (null issueIds) $ liftIO $ withResource authCtx.jobsPool \conn ->
    void $ createJob conn "background_jobs" $ EnhanceIssuesWithLLM pid (V.fromList issueIds)
  Log.logTrace "Finished log pattern spike detection" ("checked" :: Text, length patternsWithRates, "anomalies" :: Text, length anomalies)


-- | Batch-process all state='new' log patterns for a project.
-- Runs as part of the hourly pipeline instead of per-insert trigger.
processNewLogPatterns :: Projects.ProjectId -> Config.AuthContext -> ATBackgroundCtx ()
processNewLogPatterns pid authCtx = do
  void $ LogPatterns.acknowledgeMergedPatterns pid
  newPatterns <- LogPatterns.getNewLogPatterns pid maxNewPatternsPerRun
  Relude.when (length newPatterns >= maxNewPatternsPerRun) $ Log.logWarn "getNewLogPatterns hit limit, some patterns deferred" (pid, length newPatterns)
  unless (null newPatterns) do
    -- Acknowledge ALL patterns first to prevent pile-up (even when volume too low for issues)
    void $ LogPatterns.acknowledgeLogPatterns pid Nothing (V.fromList $ map (\lp -> (lp.sourceField, lp.patternHash)) newPatterns)
    now <- Time.currentTime
    totalEvents <- LogPatterns.getTotalEventCount pid now baselineWindowHours
    if totalEvents < minEventsForNewPatternAlerts
      then Log.logInfo "Skipping new log pattern issue creation due to low event volume" (pid, length newPatterns, totalEvents)
      else do
        -- Filter out infrastructure noise; errors/exceptions/app logs still create issues
        let issueWorthy = V.fromList $ filter isIssueWorthy newPatterns
        issueIds <- V.forM issueWorthy \lp -> do
          issue <- Issues.createLogPatternIssue pid lp
          Issues.insertIssue issue
          Log.logInfo "Created issue for new log pattern" (pid, lp.id, issue.id)
          pure issue.id
        unless (V.null issueIds) $ liftIO $ withResource authCtx.jobsPool \conn ->
          void $ createJob conn "background_jobs" $ EnhanceIssuesWithLLM pid issueIds


-- | Should a new log pattern create an issue? Whitelist-based: only error/warn logs
-- and patterns with error status codes. Everything else is acknowledged but no issue created.
isIssueWorthy :: LogPatterns.LogPattern -> Bool
isIssueWorthy lp
  | lp.logLevel `elem` [Just "ERROR", Just "WARN" :: Maybe Text] = True
  | hasErrorStatus lp.logPattern = True
  | otherwise = False
  where
    hasErrorStatus p = "status;badge-error⇒ERROR" `T.isInfixOf` p || "status_code;badge-4xx" `T.isInfixOf` p || "status_code;badge-5xx" `T.isInfixOf` p


-- | Prune acknowledged patterns not seen in 30 days, auto-acknowledge stale 'new' patterns,
-- and delete old hourly stats beyond the baseline window.
pruneStaleLogPatterns :: Projects.ProjectId -> ATBackgroundCtx ()
pruneStaleLogPatterns pid = do
  now <- Time.currentTime
  autoAcked <- LogPatterns.autoAcknowledgeStaleNewPatterns pid now staleNewPatternDays
  pruned <- LogPatterns.pruneStalePatterns pid now stalePatternDays
  statsPruned <- LogPatterns.pruneOldHourlyStats pid now (baselineWindowHours + 24)
  Relude.when (autoAcked > 0 || pruned > 0 || statsPruned > 0)
    $ Log.logTrace "Pruned stale log patterns and old stats" (pid, "autoAcked" :: Text, autoAcked, "pruned" :: Text, pruned, "statsPruned" :: Text, statsPruned)


calculateErrorBaselines :: Projects.ProjectId -> ATBackgroundCtx ()
calculateErrorBaselines pid = do
  now <- Time.currentTime
  Log.logTrace "Calculating error baselines" pid
  updated <- ErrorPatterns.bulkCalculateAndUpdateBaselines pid now
  Log.logTrace "Finished calculating error baselines" (pid, updated)


-- | Detect error spikes and create issues
-- Uses error_hourly_stats for current rate calculation
detectErrorSpikes :: Projects.ProjectId -> ATBackgroundCtx ()
detectErrorSpikes pid = do
  now <- Time.currentTime
  Log.logTrace "Detecting error spikes" pid

  -- Get all errors with their current hour counts in one query
  errorsWithRates <- ErrorPatterns.getErrorPatternsWithCurrentRates pid now

  void $ forConcurrently errorsWithRates \errRate -> do
    -- Only check errors with established baselines
    case (errRate.baselineState, errRate.baselineMean, errRate.baselineStddev) of
      (BSEstablished, Just mean, Just stddev) ->
        case detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta mean stddev (fromIntegral errRate.currentHourCount) of
          Just spike | spike.direction == Issues.Spike -> do
            let alreadyEscalating = errRate.state == ErrorPatterns.ESEscalating
            Log.logInfo "Error spike detected" (errRate.errorId, errRate.errorType, spike.currentRate, mean, spike.zScore)
            unless alreadyEscalating do
              void $ ErrorPatterns.updateErrorPatternState errRate.errorId ErrorPatterns.ESEscalating now
              issue <- Issues.createErrorSpikeIssue pid errRate spike.currentRate mean spike.zScore
              createAndNotifyErrorIssue pid issue ErrorSpike errRate.errorData ET.errorSpikesEmail errRate.errorId (errRate.slackThreadTs, errRate.discordMessageId)
              Log.logInfo "Created issue for error spike" (pid, errRate.errorId, issue.id)
          _ ->
            -- No spike (or drop): de-escalate if currently escalating
            Relude.when (errRate.state == ErrorPatterns.ESEscalating)
              $ void
              $ ErrorPatterns.updateErrorPatternState errRate.errorId ErrorPatterns.ESOngoing now
      _ -> pass -- Skip errors without established baseline
  Log.logTrace "Finished error spike detection" pid


-- | Insert an issue, queue LLM enhancement, and send alerts to all channels.
-- Shared by detectErrorSpikes and other error notification paths.
createAndNotifyErrorIssue
  :: Projects.ProjectId
  -> Issues.Issue
  -> RuntimeAlertType
  -> ErrorPatterns.ATError
  -> (Text -> Text -> [ErrorPatterns.ATError] -> Maybe Text -> Maybe Text -> (Text, Html ()))
  -> ErrorPatterns.ErrorPatternId
  -> (Maybe Text, Maybe Text)
  -> ATBackgroundCtx ()
createAndNotifyErrorIssue pid issue runtimeAlertType errorData emailFn errorPatternId (existSlackTs, existDiscordId) = do
  authCtx <- Effectful.Reader.Static.ask @Config.AuthContext
  now <- Time.currentTime
  Issues.insertIssue issue
  liftIO $ withResource authCtx.jobsPool \conn ->
    void $ createJob conn "background_jobs" $ EnhanceIssuesWithLLM pid (V.singleton issue.id)
  projectM <- Projects.projectById pid
  whenJust projectM \project -> Relude.when project.errorAlerts do
    users <- Projects.usersByProjectId pid
    let fromTime = addUTCTime (-(15 * 60)) now
    chartUrlM <- errorTrendChartUrl authCtx pid errorData.hash (formatUTC fromTime) (formatUTC now)
    let firstSeenTextM = Just $ relTimeAgo now errorData.when <> " · " <> toText (formatTime defaultTimeLocale "%b %-e %-l:%M %p" errorData.when)
        alert = RuntimeErrorAlert{issueId = issue.id.toText, issueTitle = issue.title, errorData, runtimeAlertType, chartUrl = chartUrlM, occurrenceText = Nothing, firstSeenText = firstSeenTextM}
        errorsUrl = authCtx.env.hostUrl <> "p/" <> pid.toText <> "/issues/" <> issue.id.toText
        (subj, html) = emailFn project.title errorsUrl [errorData] chartUrlM Nothing
    (finalSlackTs, finalDiscordMsgId) <-
      sendAlertToChannels alert pid project users errorsUrl subj html (existSlackTs, existDiscordId)
    void $ ErrorPatterns.updateErrorPatternThreadIdsAndNotifiedAt errorPatternId finalSlackTs finalDiscordMsgId now
