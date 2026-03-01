module BackgroundJobs (jobsWorkerInit, jobsRunner, processBackgroundJob, BgJobs (..), jobTypeName, runHourlyJob, generateOtelFacetsBatch, processFiveMinuteSpans, processOneMinuteErrors, throwParsePayload, checkTriggeredQueryMonitors, monitorStatus, detectSpikeOrDrop, spikeZScoreThreshold, spikeMinAbsoluteDelta, logsPatternExtraction, calculateLogPatternBaselines, detectLogPatternSpikes, processNewLogPatterns, calculateErrorBaselines, detectErrorSpikes, processNewError, notifyErrorSubscriptions) where

import Control.Lens (view, (.~), _3)
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Cache qualified as Cache
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq qualified as W
import Data.Either qualified as Unsafe
import Data.HashMap.Strict qualified as HM
import Data.List as L (foldl, partition)
import Data.List.Extra (chunksOf, groupBy)
import Data.Map.Lazy qualified as Map
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (DayOfWeek (Monday), UTCTime (utctDay, utctDayTime), ZonedTime, addUTCTime, dayOfWeek, formatTime, getZonedTime)
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime), getCurrentTimeZone, utcToZonedTime, zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VA
import Database.PostgreSQL.Simple (FromRow, SomePostgreSqlException)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent.Async (forConcurrently)
import Effectful.Ki qualified as Ki
import Effectful.Labeled (Labeled)
import Effectful.Log (Log, object)
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as PG
import Effectful.Reader.Static (ask)
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time
import Log (LogLevel (..), Logger, runLogT)
import Log qualified as LogLegacy
import Lucid (Html)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Fields qualified as Fields
import Models.Apis.Integrations (PagerdutyData (..), getPagerdutyByProjectId)
import Models.Apis.Issues qualified as Issues
import Models.Apis.Issues.Enhancement qualified as Enhancement
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.SummaryGenerator (generateSummary)
import Models.Telemetry.Telemetry (SeverityLevel (..), insertSystemLog, mkSystemLog)
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Users
import Network.Wreq (defaults, header, postWith)
import OddJobs.ConfigBuilder (mkConfig)
import OddJobs.Job (ConcurrencyControl (..), Job (..), LogEvent, LogLevel, createJob, scheduleJob, startJobRunner, throwParsePayload)
import OpenTelemetry.Attributes qualified as OA
import OpenTelemetry.Trace (TracerProvider)
import Pages.Bots.Utils qualified as BotUtils
import Pages.Charts.Charts qualified as Charts
import Pages.Replay qualified as Replay
import Pages.Reports qualified as RP
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (BaselineState (..), UUIDId (..))
import Pkg.Drain qualified as Drain
import Pkg.EmailTemplates qualified as ET
import Pkg.GitHub qualified as GitHub
import Pkg.Mail (NotificationAlerts (..), RuntimeAlertType (..), sendDiscordAlert, sendDiscordAlertWith, sendPagerdutyAlertToService, sendRenderedEmail, sendSlackAlert, sendSlackAlertWith, sendSlackMessage, sendWhatsAppAlert)
import Pkg.Parser
import Pkg.QueryCache qualified as QueryCache
import ProcessMessage (processSpanToEntities)
import PyF (fmtTrim)
import Relude hiding (ask)
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Config qualified as Config
import System.Logging qualified as Log
import System.Tracing (SpanStatus (..), Tracing, addEvent, setStatus, withSpan)
import System.Types (ATBackgroundCtx, DB, runBackground, withTimefusion)
import UnliftIO.Exception (bracket, catch, try)
import Utils (replaceAllFormats, toXXHash)


data BgJobs
  = InviteUserToProject Users.UserId Projects.ProjectId Text Text
  | CreatedProjectSuccessfully Users.UserId Projects.ProjectId Text Text
  | SendDiscordData Users.UserId Projects.ProjectId Text [Text] Text
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
  | GenerateOtelFacetsBatch (V.Vector Projects.ProjectId) UTCTime
  | QueryMonitorsCheck
  | DeletedProject Projects.ProjectId
  | CleanupDemoProject
  | FiveMinuteSpanProcessing UTCTime Projects.ProjectId
  | OneMinuteErrorProcessing UTCTime Projects.ProjectId
  | SlackNotification Projects.ProjectId Text
  | EnhanceIssuesWithLLM Projects.ProjectId (V.Vector Issues.IssueId)
  | ProcessIssuesEnhancement UTCTime
  | FiveMinuteLogPatternExtraction UTCTime Projects.ProjectId
  | GitSyncFromRepo Projects.ProjectId
  | GitSyncPushDashboard Projects.ProjectId UUID.UUID -- projectId, dashboardId
  | GitSyncPushAllDashboards Projects.ProjectId -- Push all existing dashboards to repo
  | CompressReplaySessions
  | MergeReplaySession Projects.ProjectId UUID.UUID
  | LogPatternPeriodicProcessing UTCTime Projects.ProjectId
  | LogPatternHourlyProcessing UTCTime Projects.ProjectId
  | ErrorBaselineCalculation Projects.ProjectId -- Calculate baselines for all errors in a project
  | ErrorSpikeDetection Projects.ProjectId -- Detect error spikes and create issues
  | NewErrorDetected Projects.ProjectId Text -- projectId, error hash - creates issue for new error
  | ErrorAssigned Projects.ProjectId ErrorPatterns.ErrorPatternId Users.UserId -- projectId, errorId, assigneeId
  | MonoscopeAdminDaily
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

        -- Set span status based on result
        case result of
          Left (e :: SomeException) -> do
            Log.logAttention "Background job failed" (show e)
            addEvent sp "job.failed" [("error", OA.toAttribute $ toText $ show e)]
            setStatus sp (Error $ toText $ show e)
          Right _ -> do
            addEvent sp "job.completed" []
            setStatus sp Ok


-- | Process a background job - extracted so it can be run with different effect interpreters
processBackgroundJob :: Config.AuthContext -> BgJobs -> ATBackgroundCtx ()
processBackgroundJob authCtx bgJob =
  case bgJob of
    GenerateOtelFacetsBatch pids timestamp -> generateOtelFacetsBatch pids timestamp
    NewAnomaly{projectId, createdAt, anomalyType, anomalyAction, targetHashes} -> newAnomalyJob projectId createdAt anomalyType anomalyAction (V.fromList targetHashes)
    InviteUserToProject userId projectId reciever projectTitle' -> do
      userM <- Users.userById userId
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
      userM <- Users.userById userId
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
      userM <- Users.userById assigneeId
      projectM <- Projects.projectById pid
      case (projectM, errM, userM) of
        (Just project, Just err, Just user) | err.projectId == pid -> do
          let userEmail = CI.original user.email
              userName = if T.null user.firstName then userEmail else user.firstName
          issueM <- Issues.selectIssueByHash pid err.hash
          let (issueTitle, issuePath) = case issueM of
                Just issue -> (issue.title, issue.id.toText)
                Nothing -> (err.errorType <> ": " <> err.message, "by_hash/" <> err.hash)
              issueUrl = authCtx.env.hostUrl <> "p/" <> pid.toText <> "/anomalies/" <> issuePath
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
          let acquireLock = PG.query [sql|SELECT pg_try_advisory_lock(hashtext(?))|] (Only lockName) <&> \case [Only True] -> True; _ -> False
              releaseLock =
                ( PG.query [sql|SELECT pg_advisory_unlock(hashtext(?))|] (Only lockName) >>= \case
                    [Only True] -> pass
                    other -> Log.logAttention "Advisory unlock returned unexpected result" (AE.object ["lock_name" AE..= lockName, "result" AE..= show (other :: [Only Bool])])
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
            maybe False ((>= 24) . fromOnly)
              . listToMaybe
              <$> PG.query
                [sql|SELECT COUNT(*) FROM background_jobs
               WHERE payload->>'tag' = 'HourlyJob'
                 AND run_at >= date_trunc('day', ?::timestamptz)
                 AND run_at < date_trunc('day', ?::timestamptz) + interval '1 day'
                 AND status IN ('queued', 'locked')|]
                (currentTime, currentTime)

          unless hourlyJobsExist $ do
            Log.logInfo "Scheduling hourly jobs for today" ()
            liftIO $ withResource authCtx.jobsPool \conn -> do
              void $ createJob conn "background_jobs" BackgroundJobs.MonoscopeAdminDaily
              -- background job to cleanup demo project
              Relude.when (dayOfWeek currentDay == Monday) do
                void $ createJob conn "background_jobs" BackgroundJobs.CleanupDemoProject
              forM_ [0 .. 23] \hour -> do
                let scheduledTime = addUTCTime (fromIntegral $ hour * 3600) currentTime
                void $ scheduleJob conn "background_jobs" (BackgroundJobs.HourlyJob scheduledTime hour) scheduledTime

              -- Schedule issue enhancement processing every hour
              forM_ [0 .. 23] \hr -> do
                let scheduledTime4 = addUTCTime (fromIntegral $ hr * 3600) currentTime
                scheduleJob conn "background_jobs" (BackgroundJobs.ProcessIssuesEnhancement scheduledTime4) scheduledTime4

          Relude.when hourlyJobsExist
            $ Log.logInfo "Hourly jobs already scheduled for today, skipping" ()

          projects <- PG.query [sql|SELECT DISTINCT p.id FROM projects.projects p JOIN otel_logs_and_spans o ON o.project_id = p.id::text WHERE p.active = TRUE AND p.deleted_at IS NULL AND p.payment_plan != 'ONBOARDING' AND o.timestamp > ?::timestamptz - interval '24 hours'|] (Only currentTime)
          Log.logInfo "Scheduling jobs for projects" ("project_count", length projects)
          forM_ projects \p -> do
            -- Check if this project's jobs already scheduled for today (per-project idempotent check)
            existingProjectJobs <-
              PG.query
                [sql|SELECT COUNT(*) FROM background_jobs
                 WHERE payload->>'tag' = 'FiveMinuteSpanProcessing'
                   AND payload->>'projectId' = ?
                   AND run_at >= date_trunc('day', ?::timestamptz)
                   AND run_at < date_trunc('day', ?::timestamptz) + interval '1 day'
                   AND status IN ('queued', 'locked')|]
                (p, currentTime, currentTime)

            let projectJobsExist = case existingProjectJobs of
                  [Only (count :: Int)] -> count >= 288
                  _ -> False

            unless projectJobsExist $ do
              liftIO $ withResource authCtx.jobsPool \conn -> do
                void $ createJob conn "background_jobs" $ BackgroundJobs.ReportUsage p
                -- Schedule 5-minute log pattern extraction (288 per day)
                forM_ [0 .. 287] \interval -> do
                  let scheduledTime = addUTCTime (fromIntegral $ interval * 300) currentTime
                  void $ scheduleJob conn "background_jobs" (BackgroundJobs.FiveMinuteLogPatternExtraction scheduledTime p) scheduledTime
                -- Schedule 15-minute spike detection (96 per day)
                forM_ [0 .. 95] \interval -> do
                  let scheduledTime = addUTCTime (fromIntegral $ interval * 900) currentTime
                  void $ scheduleJob conn "background_jobs" (BackgroundJobs.LogPatternPeriodicProcessing scheduledTime p) scheduledTime
                -- Schedule hourly baseline + new-pattern + prune processing (24 per day)
                forM_ [0 .. 23] \h -> do
                  let scheduledTime = addUTCTime (fromIntegral @Int $ h * 3600) currentTime
                  void $ scheduleJob conn "background_jobs" (BackgroundJobs.LogPatternHourlyProcessing scheduledTime p) scheduledTime
                -- Schedule 5-minute span processing jobs (288 jobs per day = 24 hours * 12 per hour)
                forM_ [0 .. 287] \interval -> do
                  let scheduledTime2 = addUTCTime (fromIntegral $ interval * 300) currentTime
                  scheduleJob conn "background_jobs" (BackgroundJobs.FiveMinuteSpanProcessing scheduledTime2 p) scheduledTime2
                -- Schedule 1-minute error processing jobs (1440 jobs per hour = 24 hours * 60 per hour)
                forM_ [0 .. 1439] \interval -> do
                  let scheduledTime3 = addUTCTime (fromIntegral $ interval * 60) currentTime
                  scheduleJob conn "background_jobs" (BackgroundJobs.OneMinuteErrorProcessing scheduledTime3 p) scheduledTime3
                Relude.when (dayOfWeek currentDay == Monday)
                  $ void
                  $ createJob conn "background_jobs"
                  $ BackgroundJobs.WeeklyReports p

            Relude.when projectJobsExist
              $ Log.logInfo "Jobs already scheduled for project today, skipping" ("project_id", p.toText)
    HourlyJob scheduledTime hour -> runHourlyJob scheduledTime hour
    DailyReports pid -> sendReportForProject pid DailyReport
    WeeklyReports pid -> sendReportForProject pid WeeklyReport
    ReportUsage pid -> whenJustM (Projects.projectById pid) \project -> do
      Log.logInfo "Reporting usage for project" ("project_id", pid.toText)
      Relude.when (project.paymentPlan /= "Free" && project.paymentPlan /= "ONBOARDING") $ whenJust project.firstSubItemId \fSubId -> do
        currentTime <- liftIO getZonedTime
        totalToReport <- Telemetry.getTotalEventsToReport pid project.usageLastReported
        totalMetricsCount <- Telemetry.getTotalMetricsCount pid project.usageLastReported
        Log.logInfo "Total events to report" ("events_count", totalToReport + totalMetricsCount)
        let totalUsage = totalToReport + totalMetricsCount
        Relude.when (totalUsage > 0) do
          liftIO $ reportUsageToLemonsqueezy fSubId totalUsage authCtx.config.lemonSqueezyApiKey
          void $ Projects.addDailyUsageReport pid totalUsage
          void $ Projects.updateUsageLastReported pid currentTime
      Log.logInfo "Completed usage report for project" ("project_id", pid.toText)
    CleanupDemoProject -> do
      let pid = UUIDId UUID.nil
      void $ PG.execute [sql| DELETE FROM projects.project_members WHERE project_id = ? |] (Only pid)
      void $ PG.execute [sql|DELETE FROM tests.collections  WHERE project_id = ? and title != 'Default Health check' |] (Only pid)
      void $ PG.execute [sql| DELETE FROM projects.project_api_keys WHERE project_id = ? AND title != 'Default API Key' |] (Only pid)
    FiveMinuteSpanProcessing scheduledTime pid -> processFiveMinuteSpans scheduledTime pid
    OneMinuteErrorProcessing scheduledTime pid -> processOneMinuteErrors scheduledTime pid
    SlackNotification pid message -> sendSlackMessage pid message
    EnhanceIssuesWithLLM pid issueIds -> enhanceIssuesWithLLM pid issueIds
    ProcessIssuesEnhancement scheduledTime -> processIssuesEnhancement scheduledTime
    FiveMinuteLogPatternExtraction scheduledTime pid -> logsPatternExtraction scheduledTime pid
    GitSyncFromRepo pid -> gitSyncFromRepo pid
    GitSyncPushDashboard pid dashboardId -> gitSyncPushDashboard pid (UUIDId dashboardId)
    GitSyncPushAllDashboards pid -> gitSyncPushAllDashboards pid
    QueryMonitorsCheck -> checkTriggeredQueryMonitors
    CompressReplaySessions -> Replay.compressAndMergeReplaySessions
    MergeReplaySession pid sid -> Replay.mergeReplaySession pid sid
    ErrorBaselineCalculation pid -> calculateErrorBaselines pid
    ErrorSpikeDetection pid -> detectErrorSpikes pid
    NewErrorDetected pid errorHash -> processNewError pid errorHash
    LogPatternPeriodicProcessing scheduledTime pid ->
      tryLog "detectLogPatternSpikes" $ detectLogPatternSpikes pid scheduledTime authCtx
    LogPatternHourlyProcessing _scheduledTime pid -> do
      tryLog "calculateLogPatternBaselines" $ calculateLogPatternBaselines pid
      tryLog "processNewLogPatterns" $ processNewLogPatterns pid authCtx
      tryLog "pruneStaleLogPatterns" $ pruneStaleLogPatterns pid
    MonoscopeAdminDaily -> do
      now <- Time.currentTime
      let since = addUTCTime (-86400) now
          dateStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" now
          send msg = sendMessageToDiscord msg authCtx.config.discordWebhookUrl
          buildMessages [] msgs = msgs
          buildMessages items msgs =
            let (chunk, rest) = splitAccum 1800 items
             in buildMessages rest (msgs <> [T.unlines chunk])
          splitAccum _ [] = ([], [])
          splitAccum remaining (x : xs)
            | T.length x > remaining = ([], x : xs)
            | otherwise = let (taken, left) = splitAccum (remaining - T.length x - 1) xs in (x : taken, left)
          fmtNum n
            | n >= 1000000 = show (n `div` 1000) <> "K"
            | n >= 10000 = show (n `div` 1000) <> "K"
            | otherwise = show n

      -- Gather all projects and usage data
      allProjects <- PG.query [sql|SELECT p.* FROM projects.projects p WHERE p.active = TRUE AND p.deleted_at IS NULL|] ()
      let projectMap = Map.fromList $ map (\(p :: Projects.Project) -> (p.id, p)) allProjects
          lookupTitle pid = maybe (pid.toText) (.title) $ Map.lookup pid projectMap

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
      send $ "```\n" <> T.unlines tableBody <> "```"

      -- Section 2: New projects (created in last 24h)
      tryLog "newProjects" do
        newProjects :: [Projects.Project] <- PG.query [sql|SELECT p.* FROM projects.projects p WHERE p.created_at >= ?::timestamptz AND p.deleted_at IS NULL ORDER BY p.created_at DESC|] (Only since)
        let fmtNew (p :: Projects.Project) =
              let hoursAgo = show (round (diffUTCTime now p.createdAt / 3600) :: Int)
               in "- " <> p.title <> " (" <> p.paymentPlan <> ") -- created " <> hoursAgo <> "h ago"
        send
          $ bool
            ("**New Projects** (" <> show (length newProjects) <> ")\n" <> T.unlines (map fmtNew newProjects))
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
          <> T.unlines (map (\(p, _, _, _) -> "- " <> p.title <> " (" <> p.paymentPlan <> ")") churn)

      -- Section 4: New issues
      tryLog "newIssues" do
        issueCounts :: [(Projects.ProjectId, Text, Int)] <-
          PG.query
            [sql|SELECT project_id::uuid, issue_type, COUNT(*)::int FROM apis.issues WHERE created_at > ?::timestamptz GROUP BY project_id, issue_type ORDER BY COUNT(*) DESC|]
            (Only since)
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
            <> T.unlines (map fmtProject $ take 10 byProject)

      -- Section 5: Monitor alerts
      tryLog "monitorAlerts" do
        alertCounts :: [(Projects.ProjectId, Text, Int)] <-
          PG.query
            [sql|SELECT m.project_id::uuid, m.current_status, COUNT(*)::int FROM monitors.query_monitors m
               WHERE m.deactivated_at IS NULL AND m.deleted_at IS NULL AND m.current_status != 'normal'
               GROUP BY m.project_id, m.current_status|]
            ()
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
            <> T.unlines (map fmtProject $ take 10 byProject)

      -- Section 6: Background job health
      tryLog "jobHealth" do
        jobStats :: [(Text, Int)] <- PG.query [sql|SELECT status, COUNT(*)::int FROM background_jobs WHERE created_at >= ?::timestamptz GROUP BY status|] (Only since)
        stuckJobs :: [Only Int] <- PG.query [sql|SELECT COUNT(*)::int FROM background_jobs WHERE status = 'locked' AND locked_at < ?::timestamptz|] (Only (addUTCTime (-1800) now))
        let statsLine = T.intercalate " | " $ map (\(s, c) -> s <> ": " <> show c) jobStats
            stuck = maybe 0 (.fromOnly) $ listToMaybe stuckJobs
        send
          $ "**Job Health** (last 24h)\n"
          <> statsLine
          <> bool ("\n!! " <> show stuck <> " stuck jobs (locked > 30min)") "" (stuck == 0)

      -- Section 7: Top growing projects (day-over-day)
      tryLog "topGrowing" do
        growthData :: [(Projects.ProjectId, Int, Int)] <-
          PG.query
            [sql|SELECT du.project_id::uuid,
                 COALESCE(SUM(CASE WHEN du.created_at >= (?::date - interval '1 day') THEN du.total_requests ELSE 0 END), 0)::int as yesterday,
                 COALESCE(SUM(CASE WHEN du.created_at < (?::date - interval '1 day') AND du.created_at >= (?::date - interval '2 days') THEN du.total_requests ELSE 0 END), 0)::int as day_before
               FROM apis.daily_usage du WHERE du.created_at >= (?::date - interval '2 days')
               GROUP BY du.project_id|]
            (now, now, now, now)
        let withGrowth = sortOn (\(_, _, _, g) -> negate g) [(pid, y, db, y - db) | (pid, y, db) <- growthData, y > db, db > 0]
            fmtGrowth (pid, y, db, _) =
              let pct = show (round @Double @Int $ fromIntegral (y - db) / fromIntegral db * 100)
               in "- " <> lookupTitle pid <> ": " <> fmtNum db <> " -> " <> fmtNum y <> " (+" <> pct <> "%)"
        unless (null withGrowth)
          $ send
          $ "**Top Growing** (vs previous day)\n"
          <> T.unlines (map fmtGrowth $ take 5 withGrowth)

      -- Section 8: Active users (last 24h)
      tryLog "activeUsers" do
        activeUsers :: [(Text, Text, Text, PGArray Projects.ProjectId)] <-
          PG.query
            [sql|SELECT u.email, u.first_name, u.last_name, array_agg(DISTINCT pm.project_id)::uuid[] as project_ids
               FROM users.persistent_sessions ps
               JOIN users.users u ON u.id = ps.user_id
               LEFT JOIN projects.project_members pm ON pm.user_id = ps.user_id AND pm.active = TRUE
               WHERE ps.updated_at >= ?::timestamptz
               GROUP BY u.id, u.email, u.first_name, u.last_name ORDER BY u.email|]
            (Only since)
        let fmtUser (email, _, _, PGArray pids) =
              let projs = T.intercalate ", " $ map lookupTitle $ filter (`Map.member` projectMap) pids
               in "- " <> email <> bool (" -- " <> projs) "" (T.null projs)
        send
          $ "**Active Users** ("
          <> show (length activeUsers)
          <> " in last 24h)\n"
          <> T.unlines (map fmtUser activeUsers)

      -- Section 9: Project links
      let linkRow (p, _, _, _) = "- [" <> p.title <> "](" <> authCtx.env.hostUrl <> "p/" <> p.id.toText <> ")"
          links = map linkRow sorted
      forM_ (buildMessages links []) send
      Log.logInfo "Sent daily admin summary to Discord" ("project_count", length sorted)


tryLog :: Text -> ATBackgroundCtx () -> ATBackgroundCtx ()
tryLog label = (`catch` \(e :: SomeException) -> Log.logAttention ("LogPattern pipeline step failed: " <> label) (show e))


-- | Run hourly scheduled tasks for all projects
runHourlyJob :: UTCTime -> Int -> ATBackgroundCtx ()
runHourlyJob scheduledTime hour = do
  ctx <- ask @Config.AuthContext
  let oneHourAgo = addUTCTime (-3600) scheduledTime
  activeProjects <-
    coerce @[Only Projects.ProjectId] @[Projects.ProjectId]
      <$> PG.query
        [sql| SELECT DISTINCT project_id::uuid
              FROM otel_logs_and_spans ols
              WHERE ols.timestamp >= ?
                AND ols.timestamp <= ? |]
        (oneHourAgo, scheduledTime)

  -- Log count of projects to process
  Log.logInfo "Projects with new data in the last hour window" ("count", AE.toJSON $ length activeProjects)

  -- Batch projects in groups of 10 (reduced from 100 to prevent timeouts)
  let batchSize = 10
      projectBatches = chunksOf batchSize activeProjects

  -- For each batch, create a single job with multiple project IDs
  liftIO $ withResource ctx.jobsPool \conn ->
    forM_ projectBatches \batch -> do
      let batchJob = BackgroundJobs.GenerateOtelFacetsBatch (V.fromList batch) scheduledTime
      createJob conn "background_jobs" batchJob

  -- Schedule baseline calculation and spike detection for active projects
  liftIO $ withResource ctx.jobsPool \conn ->
    forM_ activeProjects \pid -> do
      void $ createJob conn "background_jobs" $ ErrorBaselineCalculation pid
      void $ createJob conn "background_jobs" $ ErrorSpikeDetection pid

  -- Cleanup expired query cache entries
  deletedCount <- QueryCache.cleanupExpiredCache
  Relude.when (deletedCount > 0) $ Log.logInfo "Cleaned up expired query cache entries" ("deleted_count", AE.toJSON deletedCount)

  -- Compress & merge inactive replay sessions
  liftIO $ withResource ctx.jobsPool \conn ->
    void $ createJob conn "background_jobs" BackgroundJobs.CompressReplaySessions

  Log.logInfo "Completed hourly job scheduling for hour" ("hour", AE.toJSON hour)


-- | Batch process facets generation for multiple projects using 24-hour window
-- Processes projects concurrently with individual error handling to prevent batch failures
generateOtelFacetsBatch :: (DB es, Effectful.Reader.Static.Reader Config.AuthContext :> es, Ki.StructuredConcurrency :> es, Labeled "timefusion" WithConnection :> es, Log :> es, Tracing :> es, UUID.UUIDEff :> es) => V.Vector Projects.ProjectId -> UTCTime -> Eff es ()
generateOtelFacetsBatch projectIds timestamp = do
  Log.logInfo "Starting batch OTLP facets generation" ("project_count", AE.toJSON $ V.length projectIds)

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
          result <- try $ Fields.generateAndSaveFacets pid "otel_logs_and_spans" Fields.facetColumns 50 timestamp
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

  Log.logInfo "Completed batch OTLP facets generation"
    $ AE.object
      [ "total_projects" AE..= V.length projectIds
      , "successes" AE..= successes
      , "failures" AE..= failures
      ]


-- | Process HTTP spans to extract API entities and detect changes
-- This job runs every 5 minutes to analyze HTTP traffic and identify:
-- - New endpoints (API routes)
-- - New shapes (request/response structures)
-- - New fields and their formats
--
-- Processing Flow:
-- 1. Query HTTP spans from the last 5 minutes
-- 2. Group spans by project for batch processing
-- 3. Extract entities using processSpanToEntities
-- 4. Bulk insert new entities (triggers anomaly detection)
-- 5. Update spans with computed hashes for tracking
processFiveMinuteSpans :: UTCTime -> Projects.ProjectId -> ATBackgroundCtx ()
processFiveMinuteSpans scheduledTime pid = do
  ctx <- ask @Config.AuthContext
  let fiveMinutesAgo = addUTCTime (-300) scheduledTime
  Relude.when ctx.config.enableEventsTableUpdates $ do
    processSpansWithPagination fiveMinutesAgo 0
  Log.logInfo "Completed 5-minute span processing" ()
  where
    perPage = 250
    processSpansWithPagination :: UTCTime -> Int -> ATBackgroundCtx ()
    processSpansWithPagination fiveMinutesAgo skip = do
      -- Get APIToolkit-specific HTTP spans (excludes generic telemetry)
      httpSpans <-
        V.fromList
          <$> PG.query
            [sql| SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity, body, attributes, resource,
                         hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, summary, date
                  FROM otel_logs_and_spans
              WHERE project_id = ? AND timestamp >= ? AND timestamp < ? AND name = 'monoscope.http' OFFSET ? LIMIT ? |]
            (pid, fiveMinutesAgo, scheduledTime, skip, perPage)
      -- Only log if there are actually spans to process (reduces noise in tests)
      Relude.when (V.length httpSpans > 0) $ do
        Log.logTrace "Processing HTTP spans from 5-minute window" ("span_count", AE.toJSON $ V.length httpSpans)
        processProjectSpans pid httpSpans fiveMinutesAgo scheduledTime
        Log.logTrace "Processing complete for page" ("skip", skip)
      Relude.when (V.length httpSpans == perPage) $ do
        processSpansWithPagination fiveMinutesAgo (skip + perPage)


logsPatternExtraction :: UTCTime -> Projects.ProjectId -> ATBackgroundCtx ()
logsPatternExtraction scheduledTime pid = do
  ctx <- ask @Config.AuthContext
  let tfEnabled = ctx.config.enableTimefusionReads
  Relude.when ctx.config.enableEventsTableUpdates $ do
    let startTime = addUTCTime (-300) scheduledTime
    extractSummaryPatterns tfEnabled startTime
    extractFieldPatterns tfEnabled startTime
  Log.logInfo "Completed logs pattern extraction for project" ("project_id", AE.toJSON pid.toText)
  where
    limitVal = 250
    sourceField = "summary" :: Text
    extractSummaryPatterns tfEnabled startTime = do
      existingPatterns <- LogPatterns.getLogPatternTexts pid sourceField
      Relude.when (length existingPatterns > 5000)
        $ Log.logWarn "High pattern count for source field, consider pruning stale patterns" (pid, sourceField, length existingPatterns)
      let seedTree = processBatch True (V.fromList $ map SeedPattern existingPatterns) scheduledTime Drain.emptyDrainTree
      (finalTree, eventMeta) <- paginateTree tfEnabled seedTree HM.empty 0 startTime
      persistSummaryPatterns tfEnabled finalTree eventMeta startTime

    paginateTree :: Bool -> Drain.DrainTree -> HM.HashMap Text (Maybe Text, Maybe Text, Maybe Text) -> Int -> UTCTime -> ATBackgroundCtx (Drain.DrainTree, HM.HashMap Text (Maybe Text, Maybe Text, Maybe Text))
    paginateTree tfEnabled tree meta offset startTime = do
      otelEvents :: [(Text, Text, Maybe Text, Maybe Text, Maybe Text)] <- withTimefusion tfEnabled $ PG.query [sql| SELECT id::text, coalesce(array_to_string(summary, ' '),''), context___trace_id, resource___service___name, level FROM otel_logs_and_spans WHERE project_id = ? AND timestamp >= ? AND timestamp < ? OFFSET ? LIMIT ?|] (pid, startTime, scheduledTime, offset, limitVal)
      if null otelEvents
        then pure (tree, meta)
        else do
          Log.logInfo "Fetching events for pattern extraction" ("offset", AE.toJSON offset, "count", AE.toJSON (length otelEvents))
          let newMeta = HM.fromList [(i, (trId, sName, lvl)) | (i, _, trId, sName, lvl) <- otelEvents, i /= ""]
              batch = V.fromList [NewEvent logId content | (logId, content, _, _, _) <- otelEvents]
              tree' = processBatch True batch scheduledTime tree
          if length otelEvents == limitVal
            then paginateTree tfEnabled tree' (meta <> newMeta) (offset + limitVal) startTime
            else pure (tree', meta <> newMeta)

    persistSummaryPatterns tfEnabled tree eventMeta since = do
      let allPatterns = Drain.getAllLogGroups tree
          prepared = flip mapMaybe (V.toList allPatterns) \dp ->
            let filteredIds = V.filter (/= "") dp.logIds
                eventCount = fromIntegral $ V.length filteredIds :: Int64
             in if V.null filteredIds || T.null dp.templateStr
                  then Nothing
                  else
                    let (logTraceId, serviceName, logLevel) = fromMaybe (Nothing, Nothing, Nothing) (filteredIds V.!? 0 >>= (`HM.lookup` eventMeta))
                        patternHash = toXXHash dp.templateStr
                     in Just (filteredIds, patternHash, LogPatterns.UpsertPattern{projectId = pid, logPattern = dp.templateStr, hash = patternHash, sourceField, serviceName, logLevel, traceId = logTraceId, sampleMessage = Just dp.exampleLog, eventCount}, (pid, sourceField, patternHash, scheduledTime, eventCount))
      let (idHashPairs, ups, hss) = unzip3 [((filteredIds, patternHash), up, hs) | (filteredIds, patternHash, up, hs) <- prepared]
      -- Tag otel events with pattern hashes
      forM_ idHashPairs \(filteredIds, patternHash) -> do
        let hashTag = "pat:" <> patternHash
        void $ withTimefusion tfEnabled $ PG.execute [sql|UPDATE otel_logs_and_spans SET hashes = array_append(hashes, ?) WHERE project_id = ? AND timestamp >= ? AND id = ANY(?::uuid[]) AND NOT (hashes @> ARRAY[?])|] (hashTag, pid, since, filteredIds, hashTag)
      void $ LogPatterns.upsertLogPatternBatch ups
      void $ LogPatterns.upsertHourlyStatBatch hss

    extractFieldPatterns tfEnabled startTime = do
      urlPaths :: [(Maybe Text, Maybe Text, Int64)] <- withTimefusion tfEnabled $ PG.query [sql| SELECT attributes___url___path, resource___service___name, COUNT(*)::BIGINT FROM otel_logs_and_spans WHERE project_id = ? AND timestamp >= ? AND timestamp < ? AND attributes___url___path IS NOT NULL GROUP BY attributes___url___path, resource___service___name LIMIT 1000|] (pid, startTime, scheduledTime)
      Relude.when (length urlPaths == 1000) $ Log.logWarn "url_path pattern limit reached" pid
      let urlUpserts = [(up, hs) | (Just path, serviceName, cnt) <- urlPaths, let (up, hs) = mkNormalized "url_path" path serviceName Nothing cnt]
      exceptions :: [(Maybe Text, Maybe Text, Maybe Text, Int64)] <- withTimefusion tfEnabled $ PG.query [sql| SELECT attributes___exception___message, resource___service___name, level, COUNT(*)::BIGINT FROM otel_logs_and_spans WHERE project_id = ? AND timestamp >= ? AND timestamp < ? AND attributes___exception___message IS NOT NULL GROUP BY attributes___exception___message, resource___service___name, level LIMIT 1000|] (pid, startTime, scheduledTime)
      Relude.when (length exceptions == 1000) $ Log.logWarn "exception pattern limit reached" pid
      let excUpserts = [(up, hs) | (Just msg, serviceName, level, cnt) <- exceptions, let (up, hs) = mkNormalized "exception" msg serviceName level cnt]
          (ups, hss) = unzip (urlUpserts ++ excUpserts)
      void $ LogPatterns.upsertLogPatternBatch ups
      void $ LogPatterns.upsertHourlyStatBatch hss

    mkNormalized sf raw serviceName logLevel cnt =
      let normalized = replaceAllFormats raw
          patternHash = toXXHash normalized
       in ( LogPatterns.UpsertPattern{projectId = pid, logPattern = normalized, hash = patternHash, sourceField = sf, serviceName, logLevel, traceId = Nothing, sampleMessage = Just raw, eventCount = cnt}
          , (pid, sf, patternHash, scheduledTime, cnt)
          )


-- | Input for Drain tree processing.
-- SeedPattern: re-inserts existing templates with no sample (Nothing). When a real log
-- later matches, updateLogGroupWithTemplate replaces the sample with actual content.
data DrainInput = SeedPattern Text | NewEvent Text Text


processBatch :: Bool -> V.Vector DrainInput -> UTCTime -> Drain.DrainTree -> Drain.DrainTree
processBatch isSummary batch now initial = Drain.buildDrainTree tokenize logId sampleContent initial batch now
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
-- 3. Group errors by hash (project + service + error type + message)
-- 4. Create runtime exception issues (triggers anomaly detection)
--
-- Note: Uses 2-minute window instead of 1-minute to account for Kafka/PubSub
-- processing delays and ensure no errors are missed.
processOneMinuteErrors :: UTCTime -> Projects.ProjectId -> ATBackgroundCtx ()
processOneMinuteErrors scheduledTime pid = do
  Log.logInfo "Starting 1-minute error processing for project" ("project_id", AE.toJSON pid.toText)
  ctx <- ask @Config.AuthContext
  -- This processing might happen before the spans within the timestamp are stored in db
  -- hence will be missed and never get processed
  -- since we use hashes of errors and don't insert same error twice
  -- we can increase the window to account for time spent on kafka
  -- use two minutes for now before use a better solution
  Relude.when ctx.config.enableEventsTableUpdates $ do
    let oneMinuteAgo = addUTCTime (-(60 * 2)) scheduledTime
    processErrorsPaginated oneMinuteAgo 0
  void $ ErrorPatterns.updateOccurrenceCounts pid scheduledTime
  where
    processErrorsPaginated :: UTCTime -> Int -> ATBackgroundCtx ()
    processErrorsPaginated oneMinuteAgo skip = do
      -- Get all spans with errors from time window
      -- Check for:
      -- 1. Spans with error status codes
      -- 2. Spans with exception events (OpenTelemetry standard)
      -- 3. Spans with error attributes
      spansWithErrors <-
        V.fromList
          <$> PG.query
            [sql| SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity, body, attributes, resource,
                             hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, summary, date
                      FROM otel_logs_and_spans
                  WHERE project_id = ? AND timestamp >= ? AND timestamp < ?
                  AND (
                    -- Check for error status
                    status_code = 'error' OR status_code = 'ERROR' OR status_code = '2'
                    -- Check for exception events
                    OR (
                      events IS NOT NULL
                      AND EXISTS (
                        SELECT 1 FROM jsonb_array_elements(events) AS event
                        WHERE event->>'event_name' = 'exception'
                           OR event->>'event_name' ILIKE '%exception%'
                           OR event->>'event_name' ILIKE '%error%'
                      )
                    )
                    -- Check for error attributes
                    OR attributes->>'error' = 'true'
                    OR attributes->>'error.type' IS NOT NULL
                    OR attributes->>'exception.type' IS NOT NULL
                    OR attributes->>'exception.message' IS NOT NULL
                  )
                  OFFSET ? LIMIT 2000 |]
            (pid, oneMinuteAgo, scheduledTime, skip)
      -- Only log if there are actually errors to process (reduces noise in tests)
      Relude.when (V.length spansWithErrors > 0)
        $ Log.logInfo "Processing spans with errors from 1-minute window" ("span_count", AE.toJSON $ V.length spansWithErrors)
      let allErrors = Telemetry.getAllATErrors spansWithErrors
      -- Group errors by (traceId, spanId) — must sort first since V.groupBy only groups consecutive elements
      let sortedErrors = V.modify (VA.sortBy (comparing \e -> (e.traceId, e.spanId))) allErrors
          errorsByTrace = V.groupBy (\a b -> a.traceId == b.traceId && a.spanId == b.spanId) sortedErrors
      processProjectErrors pid allErrors scheduledTime
      notifyErrorSubscriptions pid (V.uniq $ V.modify VA.sort $ V.map (.hash) allErrors)
      -- Upsert hourly rollup stats (aggregated by hash)
      let hashGroups = HM.toList $ V.foldl' addError HM.empty allErrors
          addError acc e = HM.insertWith addCounts e.hash (1 :: Int, bool 0 1 (isJust e.userId)) acc
          addCounts (c1, u1) (c2, u2) = (c1 + c2, u1 + u2)
          rollupStats = V.fromList [(h, cnt, users) | (h, (cnt, users)) <- hashGroups]
      void $ ErrorPatterns.upsertErrorPatternHourlyStats pid scheduledTime rollupStats
      -- Batch otel updates: write extracted errors JSON into spans
      let mkErrorUpdate groupedErrors = do
            (firstError, _) <- V.uncons groupedErrors
            sId <- firstError.spanId
            tId <- firstError.traceId
            let mappedErrors = V.map (\x -> AE.object ["type" AE..= x.errorType, "message" AE..= x.message, "stack_trace" AE..= x.stackTrace]) groupedErrors
            pure (sId, tId, AE.toJSON mappedErrors)
          updates = V.mapMaybe mkErrorUpdate (V.fromList errorsByTrace)
      unless (V.null updates) $ do
        let (spanIds, traceIds, errorsJson) = V.unzip3 updates
        rowsUpdated <-
          PG.execute
            [sql| UPDATE otel_logs_and_spans o
                  SET errors = u.errors
                  FROM (SELECT unnest(?::text[]) AS span_id, unnest(?::text[]) AS trace_id,
                               unnest(?::jsonb[]) AS errors) u
                  WHERE o.project_id = ? AND o.context___span_id = u.span_id AND o.context___trace_id = u.trace_id |]
            (spanIds, traceIds, errorsJson, pid)
        Relude.when (fromIntegral rowsUpdated /= V.length updates)
          $ Log.logAttention "Some error updates had no effect" (AE.object ["project_id" AE..= pid.toText, "expected" AE..= V.length updates, "actual" AE..= rowsUpdated])
      -- Append "err:<hash>" to otel hashes using array_append + guard (same pattern as pat: hashes)
      let hashToSpans = HM.toList $ V.foldl' (\acc e -> maybe acc (\sId -> HM.insertWith (<>) ("err:" <> e.hash) [sId] acc) e.spanId) HM.empty allErrors
      forM_ hashToSpans \(hashTag, sIds) ->
        void $ PG.execute [sql|UPDATE otel_logs_and_spans SET hashes = array_append(hashes, ?) WHERE project_id = ? AND context___span_id = ANY(?::text[]) AND NOT (hashes @> ARRAY[?])|] (hashTag, pid, PGArray sIds, hashTag)
      Relude.when (V.length spansWithErrors == 2000)
        $ processErrorsPaginated oneMinuteAgo (skip + 2000)


data ErrorSubscriptionDue = ErrorSubscriptionDue
  { errorId :: ErrorPatterns.ErrorPatternId
  , errorData :: ErrorPatterns.ATError
  , errorState :: ErrorPatterns.ErrorState
  , issueId :: Issues.IssueId
  , issueTitle :: Text
  , slackThreadTs :: Maybe Text
  , discordMessageId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData)


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
        Projects.NEmail -> (slackTs, discordMsgId) <$ forM_ users \u -> sendRenderedEmail (CI.original u.email) subj (ET.renderEmail subj html)
        Projects.NPagerduty -> (slackTs, discordMsgId) <$ (getPagerdutyByProjectId pid >>= traverse_ \pd -> sendPagerdutyAlertToService pd.integrationKey alert project.title alertUrl)
    )
    (initSlackTs, initDiscordMsgId)
    project.notificationsChannel


notifyErrorSubscriptions :: Projects.ProjectId -> V.Vector Text -> ATBackgroundCtx ()
notifyErrorSubscriptions pid errorHashes = unless (V.null errorHashes) do
  ctx <- ask @Config.AuthContext
  now <- Time.currentTime
  dueErrors :: [ErrorSubscriptionDue] <-
    PG.query
      [sql|
        SELECT DISTINCT ON (e.id)
               e.id, e.error_data, e.state, i.id, i.title,
               e.slack_thread_ts, e.discord_message_id
        FROM apis.error_patterns e
        JOIN apis.issues i ON i.project_id = e.project_id AND i.target_hash = e.hash
        WHERE e.project_id = ?
          AND e.hash = ANY(?::text[])
          AND e.subscribed = TRUE
          AND e.state != 'resolved'
          AND i.issue_type = ? -- error patterns always create RuntimeException issues; alert type derives from error state
          AND (
            e.last_notified_at IS NULL
            OR ?::timestamptz - e.last_notified_at >= (e.notify_every_minutes * INTERVAL '1 minute')
          )
        ORDER BY e.id, i.created_at DESC
      |]
      (pid, errorHashes, Issues.RuntimeException, now)
  unless (null dueErrors) do
    Log.logInfo "Notifying error subscriptions" ("project_id", AE.toJSON pid.toText, "due_count", AE.toJSON (length dueErrors))
    projectM <- Projects.projectById pid
    whenJust projectM \project -> Relude.when project.errorAlerts do
      users <- Projects.usersByProjectId pid
      let alertTypeForState = \case
            ErrorPatterns.ESEscalating -> EscalatingErrors
            ErrorPatterns.ESRegressed -> RegressedErrors
            _ -> NewRuntimeError
      results <- forConcurrently dueErrors \sub -> do
        let alertType = alertTypeForState sub.errorState
            alert = RuntimeErrorAlert{issueId = Issues.issueIdText sub.issueId, issueTitle = sub.issueTitle, errorData = sub.errorData, runtimeAlertType = alertType}
            errorsUrl = ctx.env.hostUrl <> "p/" <> pid.toText <> "/issues/" <> sub.issueId.toText
            ~(subj, html) = case alertType of
              EscalatingErrors -> ET.escalatingErrorsEmail project.title errorsUrl [sub.errorData]
              RegressedErrors -> ET.regressedErrorsEmail project.title errorsUrl [sub.errorData]
              _ -> ET.runtimeErrorsEmail project.title errorsUrl [sub.errorData]
        (finalSlackTs, finalDiscordMsgId) <-
          sendAlertToChannels alert pid project users errorsUrl subj html (sub.slackThreadTs, sub.discordMessageId)
        pure (sub.errorId, finalSlackTs, finalDiscordMsgId)
      forM_ results \(errorId, slackTs, discordMsgId) ->
        void $ ErrorPatterns.updateErrorPatternThreadIdsAndNotifiedAt errorId slackTs discordMsgId now


-- | Process and insert errors for a specific project (single batched round-trip via unnest)
processProjectErrors :: Projects.ProjectId -> V.Vector ErrorPatterns.ATError -> UTCTime -> ATBackgroundCtx ()
processProjectErrors pid errors now = do
  result <- try $ ErrorPatterns.batchUpsertErrorPatterns pid errors now
  case result of
    Left (e :: SomePostgreSqlException) ->
      Log.logAttention "Failed to insert errors" ("error", AE.toJSON $ show e)
    Right _ ->
      unless (V.null errors)
        $ Log.logInfo "Successfully inserted errors for project"
        $ AE.object [("project_id", AE.toJSON pid.toText), ("error_count", AE.toJSON $ V.length errors)]


-- | Deduplicate a vector of items by their hash field using HashMap for O(n) performance
-- This is much faster than nubBy which is O(n²)
deduplicateByHash :: (a -> Text) -> V.Vector a -> V.Vector a
deduplicateByHash getHash = V.fromList . HM.elems . V.foldl' (\acc item -> HM.insert (getHash item) item acc) HM.empty
{-# INLINE deduplicateByHash #-}


-- | Process spans for a specific project to extract API entities
-- This is where the core anomaly detection logic happens:
-- 1. Load project cache (contains known entities to skip)
-- 2. Extract entities from each span
-- 3. Deduplicate entities by hash
-- 4. Bulk insert new entities (triggers DB anomaly detection)
-- 5. Update spans with hashes for tracking
--
-- The bulk inserts use "ON CONFLICT DO NOTHING" which prevents duplicates
-- but still triggers the database anomaly detection triggers for new entities.
processProjectSpans :: Projects.ProjectId -> V.Vector Telemetry.OtelLogsAndSpans -> UTCTime -> UTCTime -> ATBackgroundCtx ()
processProjectSpans pid spans fiveMinutesAgo scheduledTime = do
  ctx <- ask @Config.AuthContext

  -- Get project cache to filter out known entities
  projectCacheVal <- liftIO $ Cache.fetchWithCache ctx.projectCache pid \pid' -> do
    mpjCache <- Projects.projectCacheByIdIO ctx.jobsPool pid'
    pure $ fromMaybe projectCacheDefault mpjCache

  -- Batch generate all UUIDs upfront to avoid repeated IO
  !spanIds <- V.replicateM (V.length spans) UUID.genUUID

  -- Process each span to extract entities (pure computation)
  let !results = V.zipWith (processSpanToEntities projectCacheVal) spans spanIds

  -- Unzip and deduplicate extracted entities using HashMap for O(n) performance
  let !(endpoints, shapes, fields, formats, spanUpdates) = V.unzip5 results

  -- Deduplicate using HashMap instead of O(n²) nubBy
  let !endpointsFinal = deduplicateByHash (.hash) $ V.mapMaybe id endpoints
  let !shapesFinal = deduplicateByHash (.hash) $ V.mapMaybe id shapes
  let !fieldsFinal = deduplicateByHash (.hash) $ V.concat $ V.toList fields
  let !formatsFinal = deduplicateByHash (.hash) $ V.concat $ V.toList formats

  -- Only log if there are actually entities to process (reduces noise in tests)
  Relude.when (V.length endpointsFinal > 0 || V.length shapesFinal > 0 || V.length fieldsFinal > 0 || V.length formatsFinal > 0)
    $ Log.logTrace
      "Entities extracted"
      ( object
          [ "project_id" AE..= pid.toText
          , "endpoints_count" AE..= V.length endpointsFinal
          , "shapes_extracted" AE..= V.length shapes
          , "shapes_final" AE..= V.length shapesFinal
          , "fields_final" AE..= V.length fieldsFinal
          , "formats_final" AE..= V.length formatsFinal
          ]
      )

  -- Insert extracted entities
  result <- try $ Ki.scoped \scope -> do
    unless (null endpointsFinal) $ void $ Ki.fork scope $ Endpoints.bulkInsertEndpoints endpointsFinal
    unless (null shapesFinal) $ void $ Ki.fork scope $ Shapes.bulkInsertShapes shapesFinal
    unless (null fieldsFinal) $ void $ Ki.fork scope $ Fields.bulkInsertFields fieldsFinal
    unless (null formatsFinal) $ void $ Ki.fork scope $ Fields.bulkInsertFormat formatsFinal
    Ki.atomically $ Ki.awaitAll scope

  case result of
    Left (e :: SomePostgreSqlException) -> Log.logAttention "Postgres Exception during span processing" (show e)
    Right _ -> do
      -- Batch update spans with computed hashes using a single query
      let spansWithHashes = V.filter (\(_, hashes) -> not $ V.null hashes) $ V.zip spans spanUpdates
      Relude.when (V.length spansWithHashes > 0) $ do
        let expectedCount = V.length spansWithHashes
        Log.logTrace "Updating spans with computed hashes" ("span_count", AE.toJSON expectedCount)
        let dbSpanIds = PGArray $ V.toList $ V.map ((.id) . fst) spansWithHashes
            hashValues = PGArray $ V.toList $ V.map (AE.toJSON . snd) spansWithHashes
        rowsUpdated <-
          PG.execute
            [sql| UPDATE otel_logs_and_spans
                  SET hashes = converted.arr
                  FROM (SELECT unnest(?::uuid[]) as id, unnest(?::jsonb[]) as hashes_json) updates
                  CROSS JOIN LATERAL (
                    SELECT ARRAY(SELECT jsonb_array_elements_text(updates.hashes_json)) as arr
                  ) converted
                  WHERE otel_logs_and_spans.id = updates.id
                    AND otel_logs_and_spans.project_id = ?
                    AND otel_logs_and_spans.timestamp >= ?
                    AND otel_logs_and_spans.timestamp < ? |]
            (dbSpanIds, hashValues, pid, fiveMinutesAgo, scheduledTime)
        Relude.when (fromIntegral rowsUpdated /= expectedCount)
          $ Log.logAttention "Span hash update count mismatch" (AE.object ["project_id" AE..= pid.toText, "expected" AE..= expectedCount, "actual" AE..= rowsUpdated])
        Log.logTrace "Completed span processing for project" ("project_id", AE.toJSON pid.toText)
  where
    projectCacheDefault :: Projects.ProjectCache
    projectCacheDefault =
      Projects.ProjectCache
        { hosts = []
        , endpointHashes = []
        , shapeHashes = []
        , redactFieldslist = []
        , dailyEventCount = 0
        , dailyMetricCount = 0
        , paymentPlan = ""
        }


reportUsageToLemonsqueezy :: Text -> Int -> Text -> IO ()
reportUsageToLemonsqueezy subItemId quantity apiKey = do
  let formData =
        [aesonQQ|{
          "data": {"type": "usage-records","attributes": {"quantity": #{quantity},"action": "increment"},
                     "relationships": {"subscription-item": {"data": {"type": "subscription-items","id": #{subItemId}}}}
                  }}|]

  let hds = defaults & (header "Authorization" .~ ["Bearer " <> encodeUtf8 @Text @ByteString apiKey]) & (header "Content-Type" .~ ["application/vnd.api+json"])
  _ <- postWith hds "https://api.lemonsqueezy.com/v1/usage-records" formData
  pass


-- | Send notifications for a query monitor status change (inline, no separate job)
notifyQueryMonitorStatusChange :: Monitors.QueryMonitor -> Double -> Monitors.MonitorStatus -> Bool -> ATBackgroundCtx ()
notifyQueryMonitorStatusChange monitor value newStatus isRecovery = do
  appCtx <- ask @Config.AuthContext
  whenJustM (Projects.projectById monitor.projectId) \p -> do
    teams <- ProjectMembers.getTeamsById monitor.projectId monitor.teams
    Relude.when (not (V.null monitor.teams) && null teams)
      $ Log.logAttention "Monitor configured with teams but none found (possibly deleted)" (monitor.id, monitor.projectId, V.length monitor.teams)
    let hostUrl = appCtx.env.hostUrl
        monitorUrl = hostUrl <> "/p/" <> monitor.projectId.toText <> "/monitors"
    alert <-
      if isRecovery
        then pure $ MonitorsRecoveryAlert{monitorTitle = monitor.alertConfig.title, monitorUrl}
        else do
          let thresholdType = if monitor.triggerLessThan then "below" else "above"
          issue <- Issues.createQueryAlertIssue monitor.projectId (show monitor.id) monitor.alertConfig.title monitor.logQuery monitor.alertThreshold value thresholdType
          Issues.insertIssue issue
          pure $ MonitorsAlert{monitorTitle = monitor.alertConfig.title, monitorUrl = hostUrl <> "/p/" <> monitor.projectId.toText <> "/anomalies/" <> issue.id.toText}
    targetTeams <-
      if null teams
        then maybeToList <$> ProjectMembers.getEveryoneTeam monitor.projectId
        else pure teams
    for_ targetTeams \team -> dispatchTeamNotifications team alert monitor.projectId p.title monitorUrl (const . const pass)


dispatchTeamNotifications :: ProjectMembers.Team -> Pkg.Mail.NotificationAlerts -> Projects.ProjectId -> Text -> Text -> (CI.CI Text -> Maybe Users.User -> ATBackgroundCtx ()) -> ATBackgroundCtx ()
dispatchTeamNotifications team alert projectId projectTitle monitorUrl emailAction = do
  emails <- ProjectMembers.resolveTeamEmails projectId team
  for_ emails (`emailAction` Nothing)
  for_ team.slack_channels (void . sendSlackAlert alert projectId projectTitle . Just)
  for_ team.discord_channels (void . sendDiscordAlert alert projectId projectTitle . Just)
  for_ team.pagerduty_services \integrationKey -> sendPagerdutyAlertToService integrationKey alert projectTitle monitorUrl


-- way to get emails for company. for email all
-- TODO: based on monitor send emails or slack

jobsWorkerInit :: Logger -> Config.AuthContext -> TracerProvider -> IO ()
jobsWorkerInit logger appCtx tp =
  startJobRunner
    $ mkConfig jobLogger "background_jobs" appCtx.jobsPool (MaxConcurrentJobs appCtx.config.maxConcurrentJobs) (jobsRunner logger appCtx tp) id
  where
    jobLogger :: OddJobs.Job.LogLevel -> LogEvent -> IO ()
    jobLogger logLevel logEvent = runLogT "OddJobs" logger LogAttention $ LogLegacy.logInfo "Background jobs ping." (show @Text logLevel, show @Text logEvent) -- logger show (logLevel, logEvent)
    -- jobLogger logLevel logEvent = print show (logLevel, logEvent) -- logger show (logLevel, logEvent)


data ReportType = DailyReport | WeeklyReport
  deriving (Show)




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
  projectM <- Projects.projectById pid
  forM_ projectM \pr -> do
    stats <- Telemetry.getProjectStatsForReport pid startTime currentTime
    statsPrev <- Telemetry.getProjectStatsForReport pid (addUTCTime (negate (prv * 2)) currentTime) (addUTCTime (negate prv) currentTime)
    statsBySpanType <- V.fromList <$> Telemetry.getProjectStatsBySpanType pid startTime currentTime
    statsBySpanTypePrev <- V.fromList <$> Telemetry.getProjectStatsBySpanType pid (addUTCTime (negate (prv * 2)) currentTime) (addUTCTime (negate prv) currentTime)
    let totalErrors = sum $ map (\(_, x, _) -> x) stats
        totalEvents = sum $ map (\(_, _, x) -> x) stats
        totalErrorsPrev = sum $ map (\(_, x, _) -> x) statsPrev
        totalEventsPrev = sum $ map (\(_, _, x) -> x) statsPrev
        -- roundto two decimal places
        errorsChange' = if totalErrorsPrev == 0 then 0.00 else fromIntegral (totalErrors - totalErrorsPrev) / fromIntegral totalErrorsPrev * 100
        eventsChange' = if totalEventsPrev == 0 then 0.00 else fromIntegral (totalEvents - totalEventsPrev) / fromIntegral totalEventsPrev * 100
        errorsChange = fromIntegral (round (errorsChange' * 100)) / 100
        eventsChange = fromIntegral (round (eventsChange' * 100)) / 100
        spanStatsDiff = RP.getSpanTypeStats statsBySpanType statsBySpanTypePrev

    slowDbQueries <- V.fromList <$> Telemetry.getDBQueryStats pid startTime currentTime

    let parseQ q =
          let qAST = Unsafe.fromRight [] (parseQueryToAST q)
              sqlQueryComponents =
                (defSqlQueryCfg pid currentTime Nothing Nothing)
                  { dateRange = (Just startTime, Just currentTime)
                  }
              (_, qc) = queryASTToComponents sqlQueryComponents qAST
           in maybeToMonoid qc.finalSummarizeQuery

    chartDataEvents <- liftIO $ Charts.fetchMetricsData Charts.DTMetric (parseQ "| summarize count(*) by bin_auto(timestamp), resource___service___name") currentTime (Just startTime) (Just currentTime) ctx
    chartDataErrors <- liftIO $ Charts.fetchMetricsData Charts.DTMetric (parseQ "status_code == \"ERROR\" | summarize count(*) by bin_auto(timestamp), resource___service___name") currentTime (Just startTime) (Just currentTime) ctx

    (anomalies, _) <- Issues.selectIssues pid Nothing (Just False) Nothing 100 0 (Just (startTime, currentTime)) Nothing

    let anomalies' = V.fromList $ (\x -> (x.id, x.title, x.critical, x.severity, x.issueType)) <$> anomalies

    endpointStats <- V.fromList <$> Telemetry.getEndpointStats pid startTime currentTime
    endpointStatsPrev <- V.fromList <$> Telemetry.getEndpointStats pid (addUTCTime (negate (prv * 2)) currentTime) (addUTCTime (negate prv) currentTime)
    endpoint_rp <- RequestDumps.getRequestDumpForReports pid typTxt
    let endpointPerformance = RP.computeDurationChanges endpointStats endpointStatsPrev
    total_anomalies <- Anomalies.countAnomalies pid typTxt
    previous_week <- RequestDumps.getRequestDumpsForPreviousReportPeriod pid typTxt
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
          eventsWidget = def{Widget.wType = Widget.WTTimeseries, Widget.query = Just "summarize count(*) by bin_auto(timestamp), status_code"}
          errorsWidget = eventsWidget{Widget.query = Just "status_code == \"ERROR\" | summarize count(*) by bin_auto(timestamp), status_code", Widget.theme = Just "roma"}
      allQ <- BotUtils.widgetPngUrl ctx.env.apiKeyEncryptionSecretKey ctx.env.hostUrl pid eventsWidget Nothing (Just stmTxt) (Just currentTimeTxt)
      errQ <- BotUtils.widgetPngUrl ctx.env.apiKeyEncryptionSecretKey ctx.env.hostUrl pid errorsWidget Nothing (Just stmTxt) (Just currentTimeTxt)
      let alert = ReportAlert typTxt stmTxt currentTimeTxt totalErrors totalEvents (V.fromList stats) reportUrl allQ errQ

      Relude.when pr.weeklyNotif $ forM_ pr.notificationsChannel \case
        Projects.NDiscord -> void $ sendDiscordAlert alert pid pr.title Nothing
        Projects.NSlack -> void $ sendSlackAlert alert pid pr.title Nothing
        Projects.NPhone -> do
          sendWhatsAppAlert alert pid pr.title pr.whatsappNumbers
        _ -> do
          totalRequest <- RequestDumps.getLastSevenDaysTotalRequest pid
          Relude.when (totalRequest > 0) do
            let dayEnd = show $ localDay (zonedTimeToLocalTime (utcToZonedTime timeZone currentTime))
                sevenDaysAgoUTCTime = addUTCTime (negate $ 6 * 86400) currentTime
                dayStart = show $ localDay (zonedTimeToLocalTime (utcToZonedTime timeZone sevenDaysAgoUTCTime))
                totalAnomalies = length anomalies'
                (errTotal, apiTotal, qTotal) = L.foldl (\(e, a, m) (_, _, _, _, t) -> (e + if t == Issues.RuntimeException then 1 else 0, a + if t == Issues.ApiChange then 1 else 0, m + if t == Issues.QueryAlert then 1 else 0)) (0, 0, 0) anomalies'
                pctOf n = if totalAnomalies == 0 then 0 else (fromIntegral n / fromIntegral totalAnomalies) * 99
            forM_ users \user -> do
              let reportData =
                    ET.WeeklyReportData
                      { userName = user.firstName
                      , projectName = pr.title
                      , reportUrl = ctx.env.hostUrl <> "p/" <> pid.toText <> "/reports/" <> report.id.toText
                      , startDate = dayStart
                      , endDate = dayEnd
                      , eventsChartUrl = allQ
                      , errorsChartUrl = errQ
                      , totalEvents
                      , totalErrors
                      , anomaliesCount = total_anomalies
                      , runtimeErrorsPct = pctOf errTotal
                      , apiChangesPct = pctOf apiTotal
                      , alertsPct = pctOf qTotal
                      , anomalies = anomalies'
                      , performance = endpointPerformance
                      , slowQueries = slowDbQueries
                      , freeTierExceeded = pr.paymentPlan == "FREE" && totalRequest > 5000
                      }
                  (subj, html) = ET.weeklyReportEmail reportData
              sendRenderedEmail (CI.original user.email) subj (ET.renderEmail subj html)
      Log.logInfo "Completed sending report notifications for" pid


-- TODO: implement query alert email using postmark
--   (CI.original email)
--   [fmt| 🤖 APITOOLKIT: log monitor triggered `{alertConfig.title}` |]
--   [fmtTrim|
--     Hi {user.firstName},<br/>
--
--     The monitor: `{alertConfig.title}` was triggered and got above it's defined threshold.
--
--     <br/><br/>
--     Regards,
--     Apitoolkit team
--               |]

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

  -- Process each endpoint group
  forM_ anomaliesByEndpoint \(endpointHash, anomalies) -> do
    -- Check for existing open issue to avoid duplicates
    existingIssueM <- Issues.findOpenIssueForEndpoint pid endpointHash

    case existingIssueM of
      Just existingIssue -> do
        -- Update existing issue with new anomaly data
        let apiChangeData =
              Issues.APIChangeData
                { endpointMethod = fromMaybe "UNKNOWN" $ viaNonEmpty head $ V.toList $ V.mapMaybe (.endpointMethod) anomalies
                , endpointPath = fromMaybe "/" $ viaNonEmpty head $ V.toList $ V.mapMaybe (.endpointUrlPath) anomalies
                , endpointHost = "Unknown"
                , anomalyHashes = V.map (.targetHash) anomalies
                , shapeChanges = V.empty -- Simplified for now
                , formatChanges = V.empty -- Simplified for now
                , newFields = V.concatMap (.shapeNewUniqueFields) anomalies
                , deletedFields = V.concatMap (.shapeDeletedFields) anomalies
                , modifiedFields = V.concatMap (.shapeUpdatedFieldFormats) anomalies
                }
        Issues.updateIssueWithNewAnomaly existingIssue.id apiChangeData
      Nothing -> do
        -- Create new issue
        issue <- Issues.createAPIChangeIssue pid endpointHash anomalies
        Issues.insertIssue issue

        -- Queue enhancement job
        _ <- liftIO $ withResource authCtx.jobsPool \conn ->
          createJob conn "background_jobs" $ BackgroundJobs.EnhanceIssuesWithLLM pid (V.singleton issue.id)
        pass

  -- Send notifications
  projectM <- Projects.projectById pid
  whenJust projectM \project -> do
    users <- Projects.usersByProjectId pid
    let endpointInfo =
          map
            ( \(_, anoms) ->
                let firstAnom = V.head anoms
                 in fromMaybe "UNKNOWN" firstAnom.endpointMethod <> " " <> fromMaybe "/" firstAnom.endpointUrlPath
            )
            anomaliesByEndpoint
    -- Only send notifications if we have valid endpoint info
    Relude.when (project.endpointAlerts && not (null endpointInfo)) do
      let alert = EndpointAlert{project = project.title, endpoints = V.fromList endpointInfo, endpointHash = fromMaybe "" $ viaNonEmpty head $ V.toList targetHashes}

      forM_ project.notificationsChannel \case
        Projects.NSlack -> void $ sendSlackAlert alert pid project.title Nothing
        Projects.NDiscord -> void $ sendDiscordAlert alert pid project.title Nothing
        Projects.NPhone -> sendWhatsAppAlert alert pid project.title project.whatsappNumbers
        Projects.NPagerduty -> pass -- PagerDuty is only for monitor alerts
        Projects.NEmail -> do
          forM_ users \u -> do
            let anomalyUrl = authCtx.env.hostUrl <> "p/" <> pid.toText <> "/issues"
                (subj, html) = ET.anomalyEndpointEmail u.firstName project.title anomalyUrl endpointInfo
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
      <$> PG.query
        [sql| SELECT id, project_id
              FROM apis.issues
              WHERE created_at >= ? AND created_at < ?
                AND llm_enhanced_at IS NULL
                AND acknowledged_at IS NULL
                AND archived_at IS NULL
              ORDER BY project_id
              LIMIT 100 |]
        (oneHourAgo, scheduledTime)

  Log.logInfo "Found issues to enhance with LLM" (V.length issuesToEnhance)

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
          Nothing -> Log.logAttention "Issue not found for enhancement" issueId
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

                Log.logInfo "Successfully enhanced issue" issueId


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
              , Dashboards.createdBy = Users.UserId UUID.nil
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
      catch (evaluateQueryMonitor monitor startWall) \(err :: SomePostgreSqlException) -> do
        Log.logWarn "Query monitor evaluation failed" (monitor.id, monitor.alertConfig.title, show err)
        void $ Monitors.updateLastEvaluatedAt monitor.id startWall -- Still update to prevent infinite retry loop


evaluateQueryMonitor :: Monitors.QueryMonitor -> UTCTime -> ATBackgroundCtx ()
evaluateQueryMonitor monitor startWall = do
  start <- liftIO $ getTime Monotonic
  results <- PG.query (Query $ encodeUtf8 monitor.logQueryAsSql) () :: ATBackgroundCtx [Only Double]
  end <- liftIO $ getTime Monotonic

  let total = sum [v | Only v <- results]
      durationNs = toNanoSecs (diffTimeSpec end start)
      title = monitor.alertConfig.title
      prevStatus = monitor.currentStatus
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
      otelLog = mkSystemLog monitor.projectId "monitor.alert.triggered" severity (title <> ": " <> display status) attrs (Just $ fromIntegral durationNs) startWall
  insertSystemLog otelLog{Telemetry.summary = generateSummary otelLog}

  -- Determine notification intent before UPDATE so we can set timestamps correctly
  let isRecovery = status == Monitors.MSNormal && prevStatus /= Monitors.MSNormal
      statusChanged = status /= prevStatus
      reminderIntervalSecs = 3600 :: NominalDiffTime -- 1 hour
      lastTriggered = monitor.alertLastTriggered <|> monitor.warningLastTriggered
      pastReminderWindow = maybe True (\lt -> diffUTCTime startWall lt >= reminderIntervalSecs) lastTriggered
      shouldNotify
        | isRecovery = True
        | statusChanged && status /= Monitors.MSNormal = True
        | not statusChanged && status /= Monitors.MSNormal && pastReminderWindow = True
        | otherwise = False
      -- Update timestamps on transitions OR reminders (prevents spam after 1h window)
      warningAt = if status == Monitors.MSWarning && (statusChanged || shouldNotify) then Just startWall else monitor.warningLastTriggered
      alertAt = if status == Monitors.MSAlerting && (statusChanged || shouldNotify) then Just startWall else monitor.alertLastTriggered
      (finalWarningAt, finalAlertAt) = if isRecovery then (Nothing, Nothing) else (warningAt, alertAt)

  void
    $ PG.execute
      [sql| UPDATE monitors.query_monitors
          SET current_status = ?, current_value = ?, last_evaluated = ?,
              warning_last_triggered = ?, alert_last_triggered = ?
          WHERE id = ? |]
      (status, total, startWall, finalWarningAt, finalAlertAt, monitor.id)

  Relude.when shouldNotify do
    Log.logInfo "Query monitor notification" (monitor.id, title, status, total, "recovery" :: Text, isRecovery)
    notifyQueryMonitorStatusChange monitor total status isRecovery


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
  Log.logInfo "Calculating log pattern baselines" pid
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
  Log.logInfo "Finished calculating log pattern baselines" ("patterns" :: Text, total, "established" :: Text, established)


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
  Log.logInfo "Detecting log pattern spikes" pid
  let totalSecs = truncate (utctDayTime scheduledTime) :: Int
      minutesIntoHour = max 15 $ (totalSecs `mod` 3600) `div` 60
      scaleFactor = 60.0 / fromIntegral minutesIntoHour :: Double
  -- Re-alerting: ON CONFLICT dedup only matches open (unacknowledged) issues,
  -- so a fresh issue is created if the prior one was acknowledged — intentional.
  patternsWithRates <- LogPatterns.getPatternsWithCurrentRates pid scheduledTime
  let anomalies = flip mapMaybe patternsWithRates \lpRate ->
        case (lpRate.baselineState, lpRate.baselineMean, lpRate.baselineMad) of
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
  Log.logInfo "Finished log pattern spike detection" ("checked" :: Text, length patternsWithRates, "anomalies" :: Text, length anomalies)


-- | Batch-process all state='new' log patterns for a project.
-- Runs as part of the hourly pipeline instead of per-insert trigger.
processNewLogPatterns :: Projects.ProjectId -> Config.AuthContext -> ATBackgroundCtx ()
processNewLogPatterns pid authCtx = do
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
        -- url_path patterns skip new-pattern issues (noise) but still get rate-change alerts
        let issueWorthy = V.fromList $ filter (\lp -> lp.sourceField /= "url_path") newPatterns
        issueIds <- V.forM issueWorthy \lp -> do
          issue <- Issues.createLogPatternIssue pid lp
          Issues.insertIssue issue
          Log.logInfo "Created issue for new log pattern" (pid, lp.id, issue.id)
          pure issue.id
        unless (V.null issueIds) $ liftIO $ withResource authCtx.jobsPool \conn ->
          void $ createJob conn "background_jobs" $ EnhanceIssuesWithLLM pid issueIds


-- | Prune acknowledged patterns not seen in 30 days, auto-acknowledge stale 'new' patterns,
-- and delete old hourly stats beyond the baseline window.
pruneStaleLogPatterns :: Projects.ProjectId -> ATBackgroundCtx ()
pruneStaleLogPatterns pid = do
  now <- Time.currentTime
  autoAcked <- LogPatterns.autoAcknowledgeStaleNewPatterns pid now staleNewPatternDays
  pruned <- LogPatterns.pruneStalePatterns pid now stalePatternDays
  statsPruned <- LogPatterns.pruneOldHourlyStats pid now (baselineWindowHours + 24)
  Relude.when (autoAcked > 0 || pruned > 0 || statsPruned > 0)
    $ Log.logInfo "Pruned stale log patterns and old stats" (pid, "autoAcked" :: Text, autoAcked, "pruned" :: Text, pruned, "statsPruned" :: Text, statsPruned)


calculateErrorBaselines :: Projects.ProjectId -> ATBackgroundCtx ()
calculateErrorBaselines pid = do
  now <- Time.currentTime
  Log.logInfo "Calculating error baselines" pid
  updated <- ErrorPatterns.bulkCalculateAndUpdateBaselines pid now
  Log.logInfo "Finished calculating error baselines" (pid, updated)


-- | Detect error spikes and create issues
-- Uses error_hourly_stats for current rate calculation
detectErrorSpikes :: Projects.ProjectId -> ATBackgroundCtx ()
detectErrorSpikes pid = do
  now <- Time.currentTime
  Log.logInfo "Detecting error spikes" pid

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
  Log.logInfo "Finished error spike detection" pid


-- | Insert an issue, queue LLM enhancement, and send alerts to all channels.
-- Shared by detectErrorSpikes and processNewError.
createAndNotifyErrorIssue
  :: Projects.ProjectId
  -> Issues.Issue
  -> RuntimeAlertType
  -> ErrorPatterns.ATError
  -> (Text -> Text -> [ErrorPatterns.ATError] -> (Text, Html ()))
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
    let alert = RuntimeErrorAlert{issueId = issue.id.toText, issueTitle = issue.title, errorData, runtimeAlertType}
        errorsUrl = authCtx.env.hostUrl <> "p/" <> pid.toText <> "/issues/" <> issue.id.toText
        (subj, html) = emailFn project.title errorsUrl [errorData]
    (finalSlackTs, finalDiscordMsgId) <-
      sendAlertToChannels alert pid project users errorsUrl subj html (existSlackTs, existDiscordId)
    Relude.when (finalSlackTs /= existSlackTs || finalDiscordMsgId /= existDiscordId)
      $ void
      $ ErrorPatterns.updateErrorPatternThreadIdsAndNotifiedAt errorPatternId finalSlackTs finalDiscordMsgId now


-- | Process a new error and create an issue
processNewError :: Projects.ProjectId -> Text -> ATBackgroundCtx ()
processNewError pid errorHash = do
  Log.logInfo "Processing new error" (pid, errorHash)
  errorM <- ErrorPatterns.getErrorPatternByHash pid errorHash
  case errorM of
    Nothing -> Log.logAttention "Error not found for new error processing" (pid, errorHash)
    Just err -> do
      let alertType = case err.state of
            ErrorPatterns.ESNew -> Just NewRuntimeError
            ErrorPatterns.ESRegressed -> Just RegressedErrors
            _ -> Nothing
      whenJust alertType \runtimeAlertType -> do
        issue <- Issues.createNewErrorIssue pid err
        let emailFn = case runtimeAlertType of
              RegressedErrors -> ET.regressedErrorsEmail
              _ -> ET.runtimeErrorsEmail
        createAndNotifyErrorIssue pid issue runtimeAlertType err.errorData emailFn err.id (err.slackThreadTs, err.discordMessageId)
        Log.logInfo "Created issue for error" (pid, err.id, issue.id, show runtimeAlertType)
