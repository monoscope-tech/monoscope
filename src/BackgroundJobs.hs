module BackgroundJobs (jobsWorkerInit, jobsRunner, processBackgroundJob, BgJobs (..), jobTypeName, runHourlyJob, generateOtelFacetsBatch, processFiveMinuteSpans, processOneMinuteErrors, throwParsePayload) where

import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Cache qualified as Cache
import Data.CaseInsensitive qualified as CI
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq qualified as W
import Data.Either qualified as Unsafe
import Data.HashMap.Strict qualified as HM
import Data.List as L (foldl, partition)
import Data.List.Extra (chunksOf, groupBy)
import Data.Map.Lazy qualified as Map
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (DayOfWeek (Monday), UTCTime (utctDay), ZonedTime, addUTCTime, dayOfWeek, formatTime, getZonedTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime), getCurrentTimeZone, utcToZonedTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (SomePostgreSqlException)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types
import Effectful (Eff, IOE, (:>))
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
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Facets qualified as Facets
import Models.Apis.Fields.Types qualified as Fields
import Models.Apis.Issues qualified as Issues
import Models.Apis.Issues.Enhancement qualified as Enhancement
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.Reports qualified as Reports
import Models.Apis.RequestDumps (ATError (..))
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Pkg.GitHub qualified as GitHub
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Network.HTTP.Types (urlEncode)
import Network.Wreq (defaults, header, postWith)
import OddJobs.ConfigBuilder (mkConfig)
import OddJobs.Job (ConcurrencyControl (..), Job (..), LogEvent, LogLevel, createJob, scheduleJob, startJobRunner, throwParsePayload)
import OpenTelemetry.Attributes qualified as OA
import OpenTelemetry.Trace (TracerProvider)
import Pages.Charts.Charts qualified as Charts
import Pages.Reports qualified as RP
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Drain qualified as Drain
import Pkg.Mail (NotificationAlerts (..), sendDiscordAlert, sendPostmarkEmail, sendSlackAlert, sendSlackMessage, sendWhatsAppAlert)
import Pkg.Parser
import ProcessMessage (processSpanToEntities)
import PyF (fmtTrim)
import Relude hiding (ask)
import RequestMessages qualified
import System.Config qualified as Config
import System.Logging qualified as Log
import System.Tracing (SpanStatus (..), Tracing, addEvent, setStatus, withSpan)
import System.Types (ATBackgroundCtx, DB, runBackground)
import UnliftIO.Exception (bracket, catch, try)
import Utils (DBField)


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
  | QueryMonitorsTriggered (V.Vector Monitors.QueryMonitorId)
  | DeletedProject Projects.ProjectId
  | CleanupDemoProject
  | FiveMinuteSpanProcessing UTCTime Projects.ProjectId
  | OneMinuteErrorProcessing UTCTime Projects.ProjectId
  | SlackNotification Projects.ProjectId Text
  | EnhanceIssuesWithLLM Projects.ProjectId (V.Vector Issues.IssueId)
  | ProcessIssuesEnhancement UTCTime
  | FifteenMinutesLogsPatternProcessing UTCTime Projects.ProjectId
  | GitSyncFromRepo Projects.ProjectId
  | GitSyncPushDashboard Projects.ProjectId UUID.UUID Text -- projectId, dashboardId, filePath
  | GitSyncPushAllDashboards Projects.ProjectId -- Push all existing dashboards to repo
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
    QueryMonitorsTriggered queryMonitorIds -> queryMonitorsTriggered queryMonitorIds authCtx
    NewAnomaly{projectId, createdAt, anomalyType, anomalyAction, targetHashes} -> newAnomalyJob projectId createdAt anomalyType anomalyAction (V.fromList targetHashes)
    InviteUserToProject userId projectId reciever projectTitle' -> do
      userM <- Users.userById userId
      whenJust userM \user -> do
        let firstName = user.firstName
        let project_url = authCtx.env.hostUrl <> "p/" <> projectId.toText
        let templateVars =
              [aesonQQ|{
             "user_name": #{firstName},
             "project_name": #{projectTitle'},
             "project_url": #{project_url}
          }|]
        sendPostmarkEmail reciever (Just ("project-invite", templateVars)) Nothing
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
        let firstName = user.firstName
        let project_url = authCtx.env.hostUrl <> "p/" <> projectId.toText
        let templateVars =
              [aesonQQ|{
            "user_name": #{firstName},
            "project_name": #{projectTitle},
            "project_url": #{project_url}
          }|]
        sendPostmarkEmail reciever (Just ("project-created", templateVars)) Nothing
    DeletedProject pid -> do
      users <- Projects.usersByProjectId pid
      projectM <- Projects.projectById pid
      forM_ projectM \pr -> do
        forM_ users \user -> do
          let firstName = user.firstName
          let projectTitle = pr.title
          let userEmail = CI.original user.email
          let templateVars =
                [aesonQQ|{
             "user_name": #{firstName},
             "project_name": #{projectTitle}
           }|]
          sendPostmarkEmail userEmail (Just ("project-deleted", templateVars)) Nothing
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
                 AND run_at >= date_trunc('day', now())
                 AND run_at < date_trunc('day', now()) + interval '1 day'
                 AND status IN ('queued', 'locked')|]
                ()

          unless hourlyJobsExist $ do
            Log.logInfo "Scheduling hourly jobs for today" ()
            liftIO $ withResource authCtx.jobsPool \conn -> do
              -- background job to cleanup demo project
              Relude.when (dayOfWeek currentDay == Monday) do
                void $ createJob conn "background_jobs" BackgroundJobs.CleanupDemoProject
              forM_ [0 .. 23] \hour -> do
                -- Schedule each hourly job to run at the appropriate hour
                let scheduledTime = addUTCTime (fromIntegral $ hour * 3600) currentTime
                _ <- scheduleJob conn "background_jobs" (BackgroundJobs.HourlyJob scheduledTime hour) scheduledTime
                pass

              -- Schedule issue enhancement processing every hour
              forM_ [0 .. 23] \hr -> do
                let scheduledTime4 = addUTCTime (fromIntegral $ hr * 3600) currentTime
                scheduleJob conn "background_jobs" (BackgroundJobs.ProcessIssuesEnhancement scheduledTime4) scheduledTime4

          Relude.when hourlyJobsExist
            $ Log.logInfo "Hourly jobs already scheduled for today, skipping" ()

          projects <- PG.query [sql|SELECT DISTINCT p.id FROM projects.projects p JOIN otel_logs_and_spans o ON o.project_id = p.id::text WHERE p.active = TRUE AND p.deleted_at IS NULL AND p.payment_plan != 'ONBOARDING' AND o.timestamp > now() - interval '24 hours'|] ()
          Log.logInfo "Scheduling jobs for projects" ("project_count", length projects)
          forM_ projects \p -> do
            -- Check if this project's jobs already scheduled for today (per-project idempotent check)
            existingProjectJobs <-
              PG.query
                [sql|SELECT COUNT(*) FROM background_jobs
                 WHERE payload->>'tag' = 'FiveMinuteSpanProcessing'
                   AND payload->>'projectId' = ?
                   AND run_at >= date_trunc('day', now())
                   AND run_at < date_trunc('day', now()) + interval '1 day'
                   AND status IN ('queued', 'locked')|]
                (Only p)

            let projectJobsExist = case existingProjectJobs of
                  [Only (count :: Int)] -> count >= 288
                  _ -> False

            unless projectJobsExist $ do
              liftIO $ withResource authCtx.jobsPool \conn -> do
                -- Report usage to lemon squeezy
                _ <- createJob conn "background_jobs" $ BackgroundJobs.ReportUsage p
                -- Schedule 5-minute log pattern extraction
                forM_ [0 .. 287] \interval -> do
                  let scheduledTime = addUTCTime (fromIntegral $ interval * 300) currentTime
                  _ <- scheduleJob conn "background_jobs" (BackgroundJobs.FifteenMinutesLogsPatternProcessing scheduledTime p) scheduledTime
                  pass
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
        Relude.when (totalToReport > 0) do
          liftIO $ reportUsageToLemonsqueezy fSubId (totalToReport + totalMetricsCount) authCtx.config.lemonSqueezyApiKey
          _ <- Projects.addDailyUsageReport pid totalToReport
          _ <- Projects.updateUsageLastReported pid currentTime
          pass
      Log.logInfo "Completed usage report for project" ("project_id", pid.toText)
    CleanupDemoProject -> do
      let pid = UUIDId UUID.nil
      _ <- PG.execute [sql| DELETE FROM projects.project_members WHERE project_id = ? |] (Only pid)
      _ <- PG.execute [sql|DELETE FROM tests.collections  WHERE project_id = ? and title != 'Default Health check' |] (Only pid)
      _ <- PG.execute [sql| DELETE FROM projects.project_api_keys WHERE project_id = ? AND title != 'Default API Key' |] (Only pid)
      pass
    FiveMinuteSpanProcessing scheduledTime pid -> processFiveMinuteSpans scheduledTime pid
    OneMinuteErrorProcessing scheduledTime pid -> processOneMinuteErrors scheduledTime pid
    SlackNotification pid message -> sendSlackMessage pid message
    EnhanceIssuesWithLLM pid issueIds -> enhanceIssuesWithLLM pid issueIds
    ProcessIssuesEnhancement scheduledTime -> processIssuesEnhancement scheduledTime
    FifteenMinutesLogsPatternProcessing scheduledTime pid -> logsPatternExtraction scheduledTime pid
    GitSyncFromRepo pid -> gitSyncFromRepo pid
    GitSyncPushDashboard pid dashboardId filePath -> gitSyncPushDashboard pid (UUIDId dashboardId) filePath
    GitSyncPushAllDashboards pid -> gitSyncPushAllDashboards pid


-- | Run hourly scheduled tasks for all projects
runHourlyJob :: UTCTime -> Int -> ATBackgroundCtx ()
runHourlyJob scheduledTime hour = do
  ctx <- ask @Config.AuthContext
  let oneHourAgo = addUTCTime (-3600) scheduledTime
  activeProjects <-
    coerce @[Only Projects.ProjectId] @[Projects.ProjectId]
      <$> PG.query
        [sql| SELECT DISTINCT project_id
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
          result <- try $ Facets.generateAndSaveFacets pid "otel_logs_and_spans" Facets.facetColumns 50 timestamp
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
        Log.logInfo "Processing HTTP spans from 5-minute window" ("span_count", AE.toJSON $ V.length httpSpans)
        processProjectSpans pid httpSpans fiveMinutesAgo scheduledTime
        Log.logInfo "Processing complete for page" ("skip", skip)
      Relude.when (V.length httpSpans == perPage) $ do
        processSpansWithPagination fiveMinutesAgo (skip + perPage)


logsPatternExtraction :: UTCTime -> Projects.ProjectId -> ATBackgroundCtx ()
logsPatternExtraction scheduledTime pid = do
  ctx <- ask @Config.AuthContext
  Relude.when ctx.config.enableEventsTableUpdates $ do
    tenMinutesAgo <- liftIO $ addUTCTime (-300) <$> Time.currentTime
    paginate 0 tenMinutesAgo
  Log.logInfo "Completed logs pattern extraction for project" ("project_id", AE.toJSON pid.toText)
  where
    limitVal = 250
    paginate :: Int -> UTCTime -> ATBackgroundCtx ()
    paginate offset startTime = do
      otelEvents <- PG.query [sql| SELECT kind, id::text, coalesce(body::text,''), coalesce(summary::text,'') FROM otel_logs_and_spans WHERE project_id = ?   AND timestamp >= ?   AND timestamp < ?   AND (summary_pattern IS NULL  OR log_pattern IS NULL) OFFSET ? LIMIT ?|] (pid, startTime, scheduledTime, offset, limitVal)
      unless (null otelEvents) do
        Log.logInfo "Fetching events for pattern extraction" ("offset", AE.toJSON offset, "count", AE.toJSON (length otelEvents))
        let (logs, summaries) = L.partition (\(k, _, _, _) -> k == "log") otelEvents
        processPatterns "log" "log_pattern" (V.fromList [(i, body) | (_, i, body, _) <- logs]) pid scheduledTime startTime
        processPatterns "summary" "summary_pattern" (V.fromList [(i, s) | (_, i, _, s) <- summaries]) pid scheduledTime startTime
        Log.logInfo "Completed events pattern extraction for page" ("offset", AE.toJSON offset)
      Relude.when (length otelEvents == limitVal) $ paginate (offset + limitVal) startTime


-- | Generic pattern extraction for logs or summaries
processPatterns :: Text -> Text -> V.Vector (Text, Text) -> Projects.ProjectId -> UTCTime -> UTCTime -> ATBackgroundCtx ()
processPatterns kind fieldName events pid scheduledTime since = do
  Relude.when (not $ V.null events) $ do
    let qq = [text| select $fieldName from otel_logs_and_spans where project_id= ? AND timestamp >= now() - interval '1 hour' and $fieldName is not null GROUP BY $fieldName ORDER BY count(*) desc limit 20|]
    existingPatterns <- coerce @[Only Text] @[Text] <$> PG.query (Query $ encodeUtf8 qq) pid
    let known = V.fromList $ map ("",) existingPatterns
        combined = known <> events
        drainTree = processBatch (kind == "summary") combined scheduledTime Drain.emptyDrainTree
        newPatterns = Drain.getAllLogGroups drainTree
    -- Only log if patterns were extracted
    Relude.when (V.length newPatterns > 0)
      $ Log.logInfo ("Extracted " <> kind <> " patterns") ("count", AE.toJSON $ V.length newPatterns)

    forM_ newPatterns \(patternTxt, ids) -> do
      let q = [text|UPDATE otel_logs_and_spans SET $fieldName = ?  WHERE project_id = ? AND timestamp > ? AND id::text = ANY(?)|]
      unless (V.null ids)
        $ void
        $ PG.execute (Query $ encodeUtf8 q) (patternTxt, pid, since, V.filter (/= "") ids)


-- | Process a batch of (id, content) pairs through Drain
processBatch :: Bool -> V.Vector (Text, Text) -> UTCTime -> Drain.DrainTree -> Drain.DrainTree
processBatch isSummary batch now inTree =
  V.foldl' (\tree (logId, content) -> processNewLog isSummary logId content now tree) inTree batch


processNewLog :: Bool -> Text -> Text -> UTCTime -> Drain.DrainTree -> Drain.DrainTree
processNewLog isSummary logId content now tree =
  let tokens = Drain.generateDrainTokens content
   in if V.null tokens
        then tree
        else
          let tokenCount = V.length tokens
              firstToken = V.head tokens
           in Drain.updateTreeWithLog tree tokenCount firstToken tokens logId content now


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
  ctx <- ask @Config.AuthContext
  -- This processing might happen before the spans within the timestamp are stored in db
  -- hence will be missed and never get processed
  -- since we use hashes of errors and don't insert same error twice
  -- we can increase the window to account for time spent on kafka
  -- use two minutes for now before use a better solution
  Relude.when ctx.config.enableEventsTableUpdates $ do
    let oneMinuteAgo = addUTCTime (-(60 * 2)) scheduledTime
    processErrorsPaginated oneMinuteAgo 0
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
                  OFFSET ? LIMIT 30 |]
            (pid, oneMinuteAgo, scheduledTime, skip)
      -- Only log if there are actually errors to process (reduces noise in tests)
      Relude.when (V.length spansWithErrors > 0)
        $ Log.logInfo "Processing spans with errors from 1-minute window" ("span_count", AE.toJSON $ V.length spansWithErrors)
      let allErrors = Telemetry.getAllATErrors spansWithErrors
      -- Group errors by traceId within each project to avoid duplicate errors from same trace
      -- (otel log and span, [errors])
      let errorsByTrace = V.groupBy (\a b -> a.traceId == b.traceId && a.spanId == b.spanId) allErrors
      processProjectErrors pid allErrors
      -- Batch all error updates into a single query using unnest (avoids N+1 pattern)
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
                  FROM (SELECT unnest(?::text[]) AS span_id, unnest(?::text[]) AS trace_id, unnest(?::jsonb[]) AS errors) u
                  WHERE o.project_id = ? AND o.context___span_id = u.span_id AND o.context___trace_id = u.trace_id |]
            (spanIds, traceIds, errorsJson, pid)
        Relude.when (fromIntegral rowsUpdated /= V.length updates)
          $ Log.logAttention "Some error updates had no effect" (AE.object ["project_id" AE..= pid.toText, "expected" AE..= V.length updates, "actual" AE..= rowsUpdated])
      Relude.when (V.length spansWithErrors == 30) $ do
        processErrorsPaginated oneMinuteAgo (skip + 30)
        pass


-- Log.logInfo "Completed 1-minute error processing" ()

-- | Process and insert errors for a specific project
processProjectErrors :: Projects.ProjectId -> V.Vector RequestDumps.ATError -> ATBackgroundCtx ()
processProjectErrors pid errors = do
  -- Process each error, extracting HTTP fields if available
  let processedErrors = V.map processError errors

  -- Extract queries and params
  let (_, queries, paramsList) = V.unzip3 processedErrors

  -- Bulk insert errors
  result <- try $ V.zipWithM_ PG.execute queries paramsList

  case result of
    Left (e :: SomePostgreSqlException) ->
      Log.logAttention "Failed to insert errors" ("error", AE.toJSON $ show e)
    Right _ ->
      -- Only log if errors were actually inserted (reduces noise in tests)
      Relude.when (V.length errors > 0)
        $ Log.logInfo "Successfully inserted errors for project"
        $ AE.object [("project_id", AE.toJSON pid.toText), ("error_count", AE.toJSON $ V.length errors)]
  where
    -- Process a single error - the error already has requestMethod and requestPath
    -- set by getAllATErrors if it was extracted from span context
    processError :: RequestDumps.ATError -> (RequestDumps.ATError, Query, [DBField])
    processError = RequestMessages.processErrors pid Nothing Nothing Nothing


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
    $ Log.logInfo
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
        Log.logInfo "Updating spans with computed hashes" ("span_count", AE.toJSON expectedCount)
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
        Log.logInfo "Completed span processing for project" ("project_id", AE.toJSON pid.toText)
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


queryMonitorsTriggered :: V.Vector Monitors.QueryMonitorId -> Config.AuthContext -> ATBackgroundCtx ()
queryMonitorsTriggered queryMonitorIds authCtx = do
  monitorsEvaled <- Monitors.queryMonitorsById queryMonitorIds
  forM_ monitorsEvaled \monitorE ->
    if (monitorE.triggerLessThan && monitorE.evalResult >= monitorE.alertThreshold)
      || (not monitorE.triggerLessThan && monitorE.evalResult <= monitorE.alertThreshold)
      then handleQueryMonitorThreshold monitorE True authCtx.config.hostUrl
      else do
        if Just True
          == ( monitorE.warningThreshold <&> \warningThreshold ->
                 (monitorE.triggerLessThan && monitorE.evalResult >= warningThreshold)
                   || (not monitorE.triggerLessThan && monitorE.evalResult <= warningThreshold)
             )
          then handleQueryMonitorThreshold monitorE False authCtx.config.hostUrl
          else pass


handleQueryMonitorThreshold :: Monitors.QueryMonitorEvaled -> Bool -> Text -> ATBackgroundCtx ()
handleQueryMonitorThreshold monitorE isAlert hostUrl = do
  Log.logTrace "Query Monitors Triggered " monitorE
  _ <- Monitors.updateQMonitorTriggeredState monitorE.id isAlert
  whenJustM (Projects.projectById monitorE.projectId) \p -> do
    teams <- ProjectMembers.getTeamsById monitorE.projectId monitorE.teams
    logMissingTeams monitorE teams
    issue <- createAndInsertIssue monitorE hostUrl
    let alert = buildMonitorAlert monitorE issue hostUrl
    sendNotifications monitorE teams p alert


logMissingTeams :: Monitors.QueryMonitorEvaled -> [ProjectMembers.Team] -> ATBackgroundCtx ()
logMissingTeams monitorE teams = do
  Relude.when (not (V.null monitorE.teams) && null teams)
    $ Log.logAttention
      "Monitor configured with teams but none found (possibly deleted)"
      (monitorE.id, monitorE.projectId, V.length monitorE.teams)
  Relude.when (not (V.null monitorE.teams) && length teams < V.length monitorE.teams)
    $ Log.logAttention
      "Some monitor teams not found (possibly deleted)"
      (monitorE.id, monitorE.projectId, "expected" :: Text, V.length monitorE.teams, "found" :: Text, length teams)


createAndInsertIssue :: Monitors.QueryMonitorEvaled -> Text -> ATBackgroundCtx Issues.Issue
createAndInsertIssue monitorE hostUrl = do
  let (thresholdType, threshold) = calculateThreshold monitorE
  issue <-
    liftIO
      $ Issues.createQueryAlertIssue
        monitorE.projectId
        (show monitorE.id)
        monitorE.alertConfig.title
        monitorE.logQuery
        threshold
        (fromIntegral monitorE.evalResult :: Double)
        thresholdType
  Issues.insertIssue issue
  pure issue


calculateThreshold :: Monitors.QueryMonitorEvaled -> (Text, Double)
calculateThreshold monitorE =
  ( if monitorE.triggerLessThan then "below" else "above"
  , fromIntegral monitorE.alertThreshold
  )


buildMonitorAlert :: Monitors.QueryMonitorEvaled -> Issues.Issue -> Text -> Pkg.Mail.NotificationAlerts
buildMonitorAlert monitorE issue hostUrl =
  MonitorsAlert
    { monitorTitle = monitorE.alertConfig.title
    , monitorUrl = hostUrl <> "/p/" <> monitorE.projectId.toText <> "/anomalies/" <> issue.id.toText
    }


sendNotifications :: Monitors.QueryMonitorEvaled -> [ProjectMembers.Team] -> Projects.Project -> Pkg.Mail.NotificationAlerts -> ATBackgroundCtx ()
sendNotifications monitorE teams p alert = do
  for_ teams \team -> dispatchTeamNotifications team alert monitorE.projectId p.title (emailQueryMonitorAlert monitorE)
  Relude.when (null teams) $ sendFallbackNotifications monitorE


sendFallbackNotifications :: Monitors.QueryMonitorEvaled -> ATBackgroundCtx ()
sendFallbackNotifications monitorE = do
  Relude.when monitorE.alertConfig.emailAll do
    users <- Projects.usersByProjectId monitorE.projectId
    forM_ users \u -> emailQueryMonitorAlert monitorE u.email (Just u)
  forM_ monitorE.alertConfig.emails \email -> emailQueryMonitorAlert monitorE email Nothing
  unless (null monitorE.alertConfig.slackChannels) $ sendSlackMessage monitorE.projectId [fmtTrim| 🤖 *Log Alert triggered for `{monitorE.alertConfig.title}`*|]


dispatchTeamNotifications :: ProjectMembers.Team -> Pkg.Mail.NotificationAlerts -> Projects.ProjectId -> Text -> (CI.CI Text -> Maybe Users.User -> ATBackgroundCtx ()) -> ATBackgroundCtx ()
dispatchTeamNotifications team alert projectId projectTitle emailAction = do
  for_ team.notify_emails \email -> emailAction (CI.mk email) Nothing
  for_ team.slack_channels (sendSlackAlert alert projectId projectTitle . Just)
  for_ team.discord_channels (sendDiscordAlert alert projectId projectTitle . Just)


-- Send notifications

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


getSpanTypeStats :: V.Vector (Text, Int, Int) -> V.Vector (Text, Int, Int) -> V.Vector (Text, Int, Double, Int, Double)
getSpanTypeStats current prev =
  let
    spanTypes =
      ordNub $ V.toList (V.map (\(t, _, _) -> t) current <> V.map (\(t, _, _) -> t) prev)
    getStats t =
      let
        evtCount = maybe 0 (\(_, c, _) -> c) (V.find (\(st, _, _) -> st == t) current)
        prevEvtCount = maybe 0 (\(_, c, _) -> c) (V.find (\(st, _, _) -> st == t) prev)
        evtChange' = if prevEvtCount == 0 then 0.00 :: Double else fromIntegral (evtCount - prevEvtCount) / fromIntegral prevEvtCount * 100
        evtChange = fromIntegral (round (evtChange' * 100)) / 100
        -- average duration
        avgDuration = maybe 0 (\(_, _, d) -> d) (V.find (\(st, _, _) -> st == t) current)
        prevAvgDuration = maybe 0 (\(_, _, d) -> d) (V.find (\(st, _, _) -> st == t) prev)
        durationChange' = if prevAvgDuration == 0 then 0.00 :: Double else fromIntegral (avgDuration - prevAvgDuration) / fromIntegral prevAvgDuration * 100
        durationChange = fromIntegral (round (durationChange' * 100)) / 100
       in
        (t, evtCount, evtChange, avgDuration, durationChange)
   in
    V.fromList (map getStats spanTypes)


type EndpointStatsTuple = (Text, Text, Text, Int, Int)


computeDurationChanges :: V.Vector EndpointStatsTuple -> V.Vector EndpointStatsTuple -> V.Vector (Text, Text, Text, Int, Double, Int, Double)
computeDurationChanges current prev =
  let
    prevMap :: Map.Map (Text, Text, Text) Int
    prevMap =
      Map.fromList
        [ ((h, m, u), dur)
        | (h, m, u, dur, req) <- V.toList prev
        ]
    prevMapReq :: Map.Map (Text, Text, Text) Int
    prevMapReq =
      Map.fromList
        [ ((h, m, u), req)
        | (h, m, u, dur, req) <- V.toList prev
        ]
    compute (h, m, u, dur, req) =
      let change = case Map.lookup (h, m, u) prevMap of
            Just prevDur
              | prevDur > 0 ->
                  Just $ (fromIntegral (dur - prevDur) / fromIntegral prevDur) * 100
            _ -> Nothing
          reqChange = case Map.lookup (h, m, u) prevMapReq of
            Just prevDur
              | prevDur > 0 ->
                  Just $ (fromIntegral (req - prevDur) / fromIntegral prevDur) * 100
            _ -> Nothing
       in (h, m, u, dur, maybe 100.00 (\x -> fromIntegral (round (x * 100)) / 100) change, req, maybe 100.00 (\x -> fromIntegral (round (x * 100)) / 100) reqChange)
   in
    V.map compute current


sendReportForProject :: Projects.ProjectId -> ReportType -> ATBackgroundCtx ()
sendReportForProject pid rType = do
  Log.logInfo "Generating report for project" pid
  ctx <- ask @Config.AuthContext
  users <- Projects.usersByProjectId pid
  currentTime <- Time.currentTime
  let (prv, typTxt, intv) = case rType of
        WeeklyReport -> (6 * 86400, "weekly", "1d")
        _ -> (86400, "daily", "1h")

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
        spanStatsDiff = getSpanTypeStats statsBySpanType statsBySpanTypePrev

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
    let endpointPerformance = computeDurationChanges endpointStats endpointStatsPrev
    total_anomalies <- Anomalies.countAnomalies pid typTxt
    previous_week <- RequestDumps.getRequestDumpsForPreviousReportPeriod pid typTxt
    let rp_json = RP.buildReportJson' totalEvents totalErrors eventsChange errorsChange spanStatsDiff endpointPerformance slowDbQueries chartDataEvents chartDataErrors anomalies'
    timeZone <- liftIO getCurrentTimeZone
    reportId <- UUIDId <$> liftIO UUIDV4.nextRandom
    let report =
          Reports.Report
            { id = reportId
            , reportJson = rp_json
            , createdAt = utcToZonedTime timeZone currentTime
            , updatedAt = utcToZonedTime timeZone currentTime
            , projectId = pid
            , startTime = startTime
            , endTime = currentTime
            , reportType = typTxt
            }
    res <- Reports.addReport report
    Log.logInfo "Completed report generation for" pid
    unless ((typTxt == "daily" && not pr.dailyNotif) || (typTxt == "weekly" && not pr.weeklyNotif)) $ do
      Log.logInfo "Sending report notifications for" pid
      let stmTxt = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" startTime
          currentTimeTxt = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" currentTime
          reportUrl = ctx.env.hostUrl <> "p/" <> pid.toText <> "/reports/" <> report.id.toText
          chartShotUrl = ctx.env.chartShotUrl <> "?t=bar&p=" <> pid.toText <> "&from=" <> stmTxt <> "&to=" <> currentTimeTxt
          allQ = chartShotUrl <> "&q=" <> decodeUtf8 (urlEncode True (encodeUtf8 "summarize count(*) by bin(timestamp," <> intv <> "), resource___service___name"))
          errQ = chartShotUrl <> "&theme=roma" <> "&q=" <> decodeUtf8 (urlEncode True (encodeUtf8 "status_code == \"ERROR\" | summarize count(*) by bin(timestamp," <> intv <> "), resource___service___name"))
          alert = ReportAlert typTxt stmTxt currentTimeTxt totalErrors totalEvents (V.fromList stats) reportUrl allQ errQ

      Relude.when pr.weeklyNotif $ forM_ pr.notificationsChannel \case
        Projects.NDiscord -> do
          sendDiscordAlert alert pid pr.title Nothing
        Projects.NSlack -> do
          sendSlackAlert alert pid pr.title Nothing
        Projects.NPhone -> do
          sendWhatsAppAlert alert pid pr.title pr.whatsappNumbers
        _ -> do
          totalRequest <- RequestDumps.getLastSevenDaysTotalRequest pid
          Relude.when (totalRequest > 0) do
            forM_ users \user -> do
              let firstName = user.firstName
                  projectTitle = pr.title
                  userEmail = CI.original user.email
                  anmls = if total_anomalies == 0 then [AE.object ["message" AE..= "No anomalies detected yet."]] else RP.getAnomaliesEmailTemplate anomalies'
                  perf_count = V.length endpointPerformance
                  perf_shrt = if perf_count == 0 then [AE.object ["message" AE..= "No performance data yet."]] else (\(u, m, p, d, dc, req, cc) -> AE.object ["host" AE..= u, "urlPath" AE..= p, "method" AE..= m, "averageLatency" AE..= d, "latencyChange" AE..= dc]) <$> V.take 10 endpointPerformance
                  rp_url = ctx.env.hostUrl <> "p/" <> pid.toText <> "/reports/" <> report.id.toText
                  dayEnd = show $ localDay (zonedTimeToLocalTime (utcToZonedTime timeZone currentTime))
                  sevenDaysAgoUTCTime = addUTCTime (negate $ 6 * 86400) currentTime
                  sevenDaysAgoZonedTime = utcToZonedTime timeZone sevenDaysAgoUTCTime
                  dayStart = show $ localDay (zonedTimeToLocalTime sevenDaysAgoZonedTime)
                  freeTierLimitExceeded = pr.paymentPlan == "FREE" && totalRequest > 5000
                  slowQueriesCount = V.length slowDbQueries
                  slowQueriesList = if slowQueriesCount == 0 then [AE.object ["message" AE..= "No slow queries detected."]] else (\(x, y, z) -> AE.object ["statement" AE..= x, "latency" AE..= z, "total" AE..= y]) <$> slowDbQueries
                  totalAnomalies = length anomalies'
                  (errTotal, apiTotal, qTotal) = L.foldl (\(e, a, m) (_, _, _, _, t) -> (e + if t == Issues.RuntimeException then 1 else 0, a + if t == Issues.APIChange then 1 else 0, m + if t == Issues.QueryAlert then 1 else 0)) (0, 0, 0) anomalies'
                  runtimeErrorsBarPercentage = if totalAnomalies == 0 then 0 else (fromIntegral errTotal / fromIntegral totalAnomalies) * 99
                  apiChangesBarPercentage = if totalAnomalies == 0 then 0 else (fromIntegral apiTotal / fromIntegral totalAnomalies) * 99
                  alertIssuesBarPercentage = if totalAnomalies == 0 then 0 else (fromIntegral qTotal / fromIntegral totalAnomalies) * 99
                  templateVars =
                    [aesonQQ|{
                     "user_name": #{firstName},
                     "total_events": #{totalEvents},
                     "total_errors": #{totalErrors},
                     "events_chart_url": #{allQ},
                     "errors_chart_url": #{errQ},
                     "project_name": #{projectTitle},
                     "anomalies_count": #{total_anomalies},
                     "errors_bar_per": #{runtimeErrorsBarPercentage},
                     "api_bar_per": #{apiChangesBarPercentage},
                     "alerts_bar_per": #{alertIssuesBarPercentage},
                     "anomalies":  #{anmls},
                     "report_url": #{rp_url},
                     "performance_count": #{perf_count},
                     "performance": #{perf_shrt},
                     "queries": #{slowQueriesList},
                     "slow_queries_count": #{slowQueriesCount},
                     "start_date": #{dayStart},
                     "end_date": #{dayEnd},
                     "free_exceeded": #{freeTierLimitExceeded}
              }|]
              sendPostmarkEmail userEmail (Just ("weekly-report", templateVars)) Nothing
      Log.logInfo "Completed sending report notifications for" pid


emailQueryMonitorAlert :: Monitors.QueryMonitorEvaled -> CI.CI Text -> Maybe Users.User -> ATBackgroundCtx ()
emailQueryMonitorAlert monitorE@Monitors.QueryMonitorEvaled{alertConfig} email userM = whenJust userM (const pass)


-- FIXME: implement query alert email using postmark
-- sendEmail
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
  Log.logInfo "Processing new anomalies" ()
  case anomalyType of
    -- API Change anomalies (endpoint, shape, format) - group into single issue per endpoint
    -- This prevents notification spam when multiple related changes occur
    Anomalies.ATEndpoint -> processAPIChangeAnomalies pid targetHashes
    Anomalies.ATShape -> processAPIChangeAnomalies pid targetHashes
    Anomalies.ATFormat -> processAPIChangeAnomalies pid targetHashes
    -- Runtime exceptions get individual issues
    -- Each unique error pattern gets its own issue for tracking
    Anomalies.ATRuntimeException -> do
      errors <- Anomalies.errorsByHashes pid targetHashes

      -- Create one issue per error
      forM_ errors \err -> do
        issue <- liftIO $ Issues.createRuntimeExceptionIssue pid err.errorData
        Issues.insertIssue issue
        -- Queue enhancement job
        _ <- liftIO $ withResource authCtx.jobsPool \conn ->
          createJob conn "background_jobs" $ BackgroundJobs.EnhanceIssuesWithLLM pid (V.singleton issue.id)
        -- Send notifications only if project exists and has alerts enabled
        projectM <- Projects.projectById pid
        whenJust projectM \project -> Relude.when project.errorAlerts do
          users <- Projects.usersByProjectId pid
          let issueId = UUID.toText issue.id.unUUIDId
          forM_ project.notificationsChannel \case
            Projects.NSlack ->
              forM_ errors \err' -> sendSlackAlert (RuntimeErrorAlert{issueId = issueId, errorData = err'.errorData}) pid project.title Nothing
            Projects.NDiscord ->
              forM_ errors \err' -> sendDiscordAlert (RuntimeErrorAlert{issueId = issueId, errorData = err'.errorData}) pid project.title Nothing
            Projects.NPhone ->
              forM_ errors \err' ->
                sendWhatsAppAlert (RuntimeErrorAlert{issueId = issueId, errorData = err'.errorData}) pid project.title project.whatsappNumbers
            Projects.NEmail ->
              forM_ users \u -> do
                let errosJ =
                      ( \ee ->
                          let e = ee.errorData
                           in AE.object
                                [ "root_error_message" AE..= e.rootErrorMessage
                                , "error_type" AE..= e.errorType
                                , "error_message" AE..= e.message
                                , "stack_trace" AE..= e.stackTrace
                                , "when" AE..= formatTime defaultTimeLocale "%b %-e, %Y, %-l:%M:%S %p" e.when
                                , "hash" AE..= e.hash
                                , "tech" AE..= e.technology
                                , "request_info" AE..= (fromMaybe "" e.requestMethod <> " " <> fromMaybe "" e.requestPath)
                                , "root_error_type" AE..= e.rootErrorType
                                ]
                      )
                        <$> errors
                    title = project.title
                    errors_url = authCtx.env.hostUrl <> "p/" <> pid.toText <> "/issues/"
                    templateVars =
                      [aesonQQ|{
                          "project_name": #{title},
                          "errors_url": #{errors_url},
                          "errors": #{errosJ}
                     }|]
                sendPostmarkEmail (CI.original u.email) (Just ("runtime-errors", templateVars)) Nothing
    -- Ignore other anomaly types
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
        issue <- liftIO $ Issues.createAPIChangeIssue pid endpointHash anomalies
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
        Projects.NSlack -> sendSlackAlert alert pid project.title Nothing
        Projects.NDiscord -> sendDiscordAlert alert pid project.title Nothing
        Projects.NPhone -> sendWhatsAppAlert alert pid project.title project.whatsappNumbers
        Projects.NEmail -> do
          forM_ users \u -> do
            let templateVars =
                  AE.object
                    [ "user_name" AE..= u.firstName
                    , "project_name" AE..= project.title
                    , "anomaly_url" AE..= (authCtx.env.hostUrl <> "p/" <> pid.toText <> "/issues")
                    , "endpoint_name" AE..= endpointInfo
                    ]
            sendPostmarkEmail (CI.original u.email) (Just ("anomaly-endpoint-2", templateVars)) Nothing


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

  -- Group issues by project
  let issuesByProject = V.groupBy (\a b -> snd a == snd b) issuesToEnhance

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
  (Just instId, _) -> GitHub.getInstallationToken config.githubAppId config.githubAppPrivateKey instId <&> \case
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
            Right (treeSha, entries) -> do
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
      let teamIds = mapMaybe (`Map.lookup` teamMap) (fold schema.teams)
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
              , Dashboards.filePath = Just path
              , Dashboards.fileSha = Just sha
              }
      _ <- Dashboards.insert dashboard
      Log.logInfo "Created dashboard from git" (path, dashId)
  GitSync.SyncUpdate path sha dashId ->
    fetchAndParseDashboard token sync path >>= either (Log.logAttention "Failed to sync dashboard from git" . (path,)) \schema -> do
      now <- Time.currentTime
      _ <- Dashboards.updateSchemaAndUpdatedAt dashId schema now
      whenJust schema.title $ void . Dashboards.updateTitle dashId
      _ <- Dashboards.updateTags dashId (V.fromList $ fold schema.tags)
      _ <- GitSync.updateDashboardGitInfo dashId path sha
      Log.logInfo "Updated dashboard from git" (path, dashId)
  GitSync.SyncDelete{} -> pass -- Handled separately


fetchAndParseDashboard :: (IOE :> es, W.HTTP :> es) => Text -> GitSync.GitHubSync -> Text -> Eff es (Either Text Dashboards.Dashboard)
fetchAndParseDashboard token sync path = GitSync.fetchFileContent token sync path <&> (>>= GitSync.yamlToDashboard)


-- | Push a dashboard change to GitHub
gitSyncPushDashboard :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> ATBackgroundCtx ()
gitSyncPushDashboard pid dashId filePath = do
  Log.logInfo "Pushing dashboard to GitHub" (pid, dashId, filePath)
  ctx <- ask @Config.AuthContext
  let encKey = encodeUtf8 ctx.config.apiKeyEncryptionSecretKey
  syncM <- GitSync.getGitHubSyncDecrypted encKey pid
  dashM <- Dashboards.getDashboardById dashId
  case (syncM, dashM) of
    (Nothing, _) -> Log.logAttention "No GitHub sync configured for project" pid
    (Just sync, _) | not sync.syncEnabled -> Log.logInfo "GitHub sync disabled, skipping push" pid
    (_, Nothing) -> Log.logAttention "Dashboard not found for git push" dashId
    (Just sync, Just dash) -> W.runHTTPWreq do
      tokenResult <- getGitSyncToken ctx.config sync
      case tokenResult of
        Left err -> Log.logAttention "Failed to get GitHub token" (pid, err)
        Right token -> do
          teams <- ProjectMembers.getTeamsById pid dash.teams
          let schema = GitSync.buildSchemaWithMeta dash.schema dash.title (V.toList dash.tags) (map (.handle) teams)
              yamlContent = GitSync.dashboardToYaml schema
              existingSha = dash.fileSha
              message = "Update dashboard: " <> dash.title
          pushResult <- GitSync.pushFileToGit token sync filePath yamlContent existingSha message
          case pushResult of
            Left err -> Log.logAttention "Failed to push dashboard to git" (dashId, err)
            Right newSha -> do
              _ <- GitSync.updateDashboardGitInfo dashId filePath newSha
              Log.logInfo "Successfully pushed dashboard to git" (dashId, newSha)


-- | Push all dashboards from a project to GitHub (used after initial repo connection)
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
          Log.logInfo "Found dashboards to push" (pid, length dashboards)
          forM_ dashboards \dash -> do
            teams <- ProjectMembers.getTeamsById pid dash.teams
            let schema = GitSync.buildSchemaWithMeta dash.schema dash.title (V.toList dash.tags) (map (.handle) teams)
                yamlContent = GitSync.dashboardToYaml schema
                -- Use existing filePath if set, otherwise generate from title
                filePath = fromMaybe (GitSync.getDashboardsPath sync <> GitSync.titleToFilePath dash.title) dash.filePath
                existingSha = dash.fileSha
                message = "Sync dashboard: " <> dash.title
            pushResult <- GitSync.pushFileToGit token sync filePath yamlContent existingSha message
            case pushResult of
              Left err -> Log.logAttention "Failed to push dashboard" (dash.id, err)
              Right newSha -> do
                _ <- GitSync.updateDashboardGitInfo dash.id filePath newSha
                Log.logInfo "Pushed dashboard" (dash.id, dash.title)
          Log.logInfo "Finished pushing all dashboards" pid
