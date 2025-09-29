module BackgroundJobs (jobsWorkerInit, jobsRunner, processBackgroundJob, BgJobs (..), runHourlyJob, generateOtelFacetsBatch, processFiveMinuteSpans, processOneMinuteErrors, throwParsePayload) where

import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Cache qualified as Cache
import Data.CaseInsensitive qualified as CI
import Data.Effectful.UUID qualified as UUID
import Data.List.Extra (chunksOf, groupBy)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (DayOfWeek (Monday), UTCTime (utctDay), ZonedTime, addUTCTime, dayOfWeek, formatTime, getZonedTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime), getCurrentTimeZone, utcToZonedTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Data.Vector.Algorithms qualified as VAA
import Database.PostgreSQL.Entity.DBT (execute, query, withPool)
import Database.PostgreSQL.Simple (Only (Only), Query, SomePostgreSqlException)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact qualified as PTR
import Effectful (Eff, IOE, (:>))
import Effectful.Ki qualified as Ki
import Effectful.Labeled (Labeled)
import Effectful.Log (Log, object)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Reader.Static (ask)
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time
import Log (LogLevel (..), Logger, runLogT)
import Log qualified as LogLegacy
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Facets qualified as Facets
import Models.Apis.Fields.Query qualified as FieldsQ
import Models.Apis.Fields.Types qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Issues qualified as Issues
import Models.Apis.Issues.Enhancement qualified as Enhancement
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.Reports qualified as Reports
import Models.Apis.RequestDumps (ATError (..))
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.LemonSqueezy qualified as LemonSqueezy
import Models.Projects.Projects (ProjectId (unProjectId))
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Users qualified as Users
import Network.HTTP.Types (urlEncode)
import Network.Wreq (defaults, header, postWith)
import OddJobs.ConfigBuilder (mkConfig)
import OddJobs.Job (ConcurrencyControl (..), Job (..), LogEvent, LogLevel, createJob, scheduleJob, startJobRunner, throwParsePayload)
import OpenTelemetry.Attributes qualified as OA
import OpenTelemetry.Trace (TracerProvider)
import Pages.Reports qualified as RP
import Pkg.Drain qualified as Drain
import Pkg.Mail (NotificationAlerts (EndpointAlert, ReportAlert, RuntimeErrorAlert), sendDiscordAlert, sendPostmarkEmail, sendSlackAlert, sendSlackMessage, sendWhatsAppAlert)
import ProcessMessage (processSpanToEntities)
import PyF (fmtTrim)
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import RequestMessages (replaceAllFormats)
import RequestMessages qualified
import System.Config qualified as Config
import System.Logging qualified as Log
import System.Tracing (SpanStatus (..), addEvent, setStatus, withSpan)
import System.Types (ATBackgroundCtx, runBackground)
import UnliftIO.Exception (try)
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
  | GenerateOtelFacetsBatch (V.Vector Text) UTCTime
  | QueryMonitorsTriggered (V.Vector Monitors.QueryMonitorId)
  | DeletedProject Projects.ProjectId
  | CleanupDemoProject
  | FiveMinuteSpanProcessing UTCTime
  | OneMinuteErrorProcessing UTCTime
  | SlackNotification Projects.ProjectId Text
  | EnhanceIssuesWithLLM Projects.ProjectId (V.Vector Issues.IssueId)
  | ProcessIssuesEnhancement UTCTime
  | FifteenMinutesLogsPatternProcessing UTCTime
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
jobTypeName bgJob = T.takeWhile (/= ' ') $ T.pack $ show bgJob


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
        result <- try $ processBackgroundJob authCtx job bgJob

        -- Set span status based on result
        case result of
          Left (e :: SomeException) -> do
            Log.logAttention "Background job failed" (show e)
            addEvent sp "job.failed" [("error", OA.toAttribute $ T.pack (show e))]
            setStatus sp (Error $ T.pack $ show e)
          Right _ -> do
            addEvent sp "job.completed" []
            setStatus sp Ok


-- | Process a background job - extracted so it can be run with different effect interpreters
processBackgroundJob :: Config.AuthContext -> Job -> BgJobs -> ATBackgroundCtx ()
processBackgroundJob authCtx job bgJob =
  case bgJob of
    GenerateOtelFacetsBatch pids timestamp -> generateOtelFacetsBatch pids timestamp
    QueryMonitorsTriggered queryMonitorIds -> queryMonitorsTriggered queryMonitorIds
    NewAnomaly{projectId, createdAt, anomalyType, anomalyAction, targetHashes} -> newAnomalyJob projectId createdAt anomalyType anomalyAction (V.fromList targetHashes)
    InviteUserToProject userId projectId reciever projectTitle' -> do
      userM <- Users.userById userId
      whenJust userM \user -> do
        let firstName = user.firstName
        let project_url = "https://app.monoscope.tech/p/" <> projectId.toText
        let templateVars =
              [aesonQQ|{
             "user_name": #{firstName},
             "project_name": #{projectTitle'},
             "project_url": #{project_url}
          }|]
        sendPostmarkEmail reciever (Just ("project-invite", templateVars)) Nothing
    SendDiscordData userId projectId fullName stack foundUsFrom -> whenJustM (dbtToEff $ Projects.projectById projectId) \project -> do
      users <- dbtToEff $ Projects.usersByProjectId projectId
      let stackString = intercalate ", " $ map toString stack
      forM_ users \user -> do
        let userEmail = CI.original user.email
        let project_url = "https://app.monoscope.tech/p/" <> projectId.toText
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
        let project_url = "https://app.monoscope.tech/p/" <> projectId.toText
        let templateVars =
              [aesonQQ|{
            "user_name": #{firstName},
            "project_name": #{projectTitle},
            "project_url": #{project_url}
          }|]
        sendPostmarkEmail reciever (Just ("project-created", templateVars)) Nothing
    DeletedProject pid -> do
      users <- dbtToEff $ Projects.usersByProjectId pid
      projectM <- dbtToEff $ Projects.projectById pid
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
    DailyJob -> do
      currentDay <- utctDay <$> Time.currentTime
      currentTime <- Time.currentTime

      -- Schedule all 24 hourly jobs in one batch
      liftIO $ withResource authCtx.jobsPool \conn -> do
        forM_ [0 .. 287] \interval -> do
          let scheduledTime = addUTCTime (fromIntegral $ interval * 600) currentTime
          _ <- scheduleJob conn "background_jobs" (BackgroundJobs.FifteenMinutesLogsPatternProcessing scheduledTime) scheduledTime
          pass

        -- background job to cleanup demo project
        Relude.when (dayOfWeek currentDay == Monday) do
          void $ createJob conn "background_jobs" $ BackgroundJobs.CleanupDemoProject
        forM_ [0 .. 23] \hour -> do
          -- Schedule each hourly job to run at the appropriate hour
          let scheduledTime = addUTCTime (fromIntegral $ hour * 3600) currentTime
          _ <- scheduleJob conn "background_jobs" (BackgroundJobs.HourlyJob scheduledTime hour) scheduledTime
          pass

        -- Schedule 5-minute span processing jobs (288 jobs per day = 24 hours * 12 per hour)
        forM_ [0 .. 287] \interval -> do
          let scheduledTime2 = addUTCTime (fromIntegral $ interval * 300) currentTime
          scheduleJob conn "background_jobs" (BackgroundJobs.FiveMinuteSpanProcessing scheduledTime2) scheduledTime2

        -- Schedule 1-minute error processing jobs (1440 jobs per hour = 24 hours * 60 per hour)
        forM_ [0 .. 1439] \interval -> do
          let scheduledTime3 = addUTCTime (fromIntegral $ interval * 60) currentTime
          scheduleJob conn "background_jobs" (BackgroundJobs.OneMinuteErrorProcessing scheduledTime3) scheduledTime3

        -- Schedule issue enhancement processing every hour
        forM_ [0 .. 23] \hr -> do
          let scheduledTime4 = addUTCTime (fromIntegral $ hr * 3600) currentTime
          scheduleJob conn "background_jobs" (BackgroundJobs.ProcessIssuesEnhancement scheduledTime4) scheduledTime4

      -- Handle regular daily jobs for each project
      projects <- dbtToEff $ query [sql|SELECT DISTINCT p.id FROM projects.projects p JOIN otel_logs_and_spans o ON o.project_id = p.id::text WHERE p.active = TRUE AND p.deleted_at IS NULL AND p.payment_plan != 'ONBOARDING' AND o.timestamp > now() - interval '24 hours'|] ()
      forM_ projects \p -> do
        liftIO $ withResource authCtx.jobsPool \conn -> do
          Relude.when (dayOfWeek currentDay == Monday)
            $ void
            $ createJob conn "background_jobs"
            $ BackgroundJobs.WeeklyReports p
          createJob conn "background_jobs" $ BackgroundJobs.ReportUsage p
    HourlyJob scheduledTime hour -> runHourlyJob scheduledTime hour
    DailyReports pid -> sendReportForProject pid DailyReport
    WeeklyReports pid -> sendReportForProject pid WeeklyReport
    ReportUsage pid -> whenJustM (dbtToEff $ Projects.projectById pid) \project -> do
      Relude.when (project.paymentPlan /= "Free" && project.paymentPlan /= "ONBOARDING") $ whenJust project.firstSubItemId \fSubId -> do
        currentTime <- liftIO getZonedTime
        totalToReport <- Telemetry.getTotalEventsToReport pid project.usageLastReported
        Relude.when (totalToReport > 0) do
          liftIO $ reportUsageToLemonsqueezy fSubId totalToReport authCtx.config.lemonSqueezyApiKey
          _ <- dbtToEff $ LemonSqueezy.addDailyUsageReport pid totalToReport
          _ <- dbtToEff $ Projects.updateUsageLastReported pid currentTime
          pass
    CleanupDemoProject -> do
      let pid = Projects.ProjectId{unProjectId = UUID.nil}
      -- DELETE PROJECT members
      _ <- withPool authCtx.pool $ PTR.execute [sql| DELETE FROM projects.project_members WHERE project_id = ? |] (Only pid)
      -- SOFT DELETE test collections
      _ <- withPool authCtx.pool $ PTR.execute [sql|DELETE FROM tests.collections  WHERE project_id = ? and title != 'Default Health check' |] (Only pid)
      -- DELETE API KEYS
      _ <- withPool authCtx.pool $ PTR.execute [sql| DELETE FROM projects.project_api_keys WHERE project_id = ? AND title != 'Default API Key' |] (Only pid)
      pass
    FiveMinuteSpanProcessing scheduledTime -> processFiveMinuteSpans scheduledTime
    OneMinuteErrorProcessing scheduledTime -> processOneMinuteErrors scheduledTime
    SlackNotification pid message -> sendSlackMessage pid message
    EnhanceIssuesWithLLM pid issueIds -> enhanceIssuesWithLLM pid issueIds
    ProcessIssuesEnhancement scheduledTime -> processIssuesEnhancement scheduledTime
    FifteenMinutesLogsPatternProcessing scheduledTime -> logsPatternExtraction scheduledTime


-- | Run hourly scheduled tasks for all projects
runHourlyJob :: UTCTime -> Int -> ATBackgroundCtx ()
runHourlyJob scheduledTime hour = do
  ctx <- ask @Config.AuthContext
  let oneHourAgo = addUTCTime (-3600) scheduledTime
  activeProjects <-
    dbtToEff
      $ query
        [sql| SELECT DISTINCT project_id 
              FROM otel_logs_and_spans ols
              WHERE ols.timestamp >= ?
                AND ols.timestamp <= ? |]
        (oneHourAgo, scheduledTime)

  -- Log count of projects to process
  Log.logInfo "Projects with new data in the last hour window" ("count", AE.toJSON $ length activeProjects)

  -- Batch projects in groups of 100
  let batchSize = 100
      projectBatches = chunksOf batchSize $ V.toList activeProjects

  -- For each batch, create a single job with multiple project IDs
  liftIO $ withResource ctx.jobsPool \conn ->
    forM_ projectBatches \batch -> do
      let batchJob = BackgroundJobs.GenerateOtelFacetsBatch (V.fromList batch) scheduledTime
      createJob conn "background_jobs" batchJob

  Log.logInfo "Completed hourly job scheduling for hour" ("hour", AE.toJSON hour)


-- | Batch process facets generation for multiple projects using 24-hour window
generateOtelFacetsBatch :: (DB :> es, Effectful.Reader.Static.Reader Config.AuthContext :> es, IOE :> es, Labeled "timefusion" DB :> es, Log :> es, UUID.UUIDEff :> es) => V.Vector Text -> UTCTime -> Eff es ()
generateOtelFacetsBatch projectIds timestamp = do
  forM_ projectIds \pid -> void $ Facets.generateAndSaveFacets (Projects.ProjectId $ Unsafe.fromJust $ UUID.fromText pid) "otel_logs_and_spans" Facets.facetColumns 50 timestamp
  Log.logInfo "Completed batch OTLP facets generation for projects" ("project_count", AE.toJSON $ V.length projectIds)


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
processFiveMinuteSpans :: UTCTime -> ATBackgroundCtx ()
processFiveMinuteSpans scheduledTime = do
  ctx <- ask @Config.AuthContext
  let fiveMinutesAgo = addUTCTime (-300) scheduledTime

  Log.logInfo "Getting HTTP spans from 5-minute window" ()
  -- Get APIToolkit-specific HTTP spans (excludes generic telemetry)
  httpSpans <-
    dbtToEff
      $ query
        [sql| SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity, body, attributes, resource, 
                     hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, summary, date
              FROM otel_logs_and_spans 
          WHERE project_id != '98fdd4f3-3544-4087-ad91-1e7ca95aba29' AND timestamp >= ? AND timestamp < ? AND name = 'monoscope.http'
          ORDER BY project_id |]
        (fiveMinutesAgo, scheduledTime)

  Log.logInfo "Processing HTTP spans from 5-minute window" ("span_count", AE.toJSON $ V.length httpSpans)

  -- Group spans by project
  let spansByProject = V.groupBy (\a b -> a.project_id == b.project_id) httpSpans

  Log.logInfo "Grouped spans by project" ("project_count", AE.toJSON $ length spansByProject)

  forM_ spansByProject \projectSpans -> case V.uncons projectSpans of
    Nothing -> pass
    Just (firstSpan, _) -> do
      let pid = Projects.ProjectId $ Unsafe.fromJust $ UUID.fromText firstSpan.project_id
      Log.logInfo "Processing project spans" $ AE.object [("project_id", AE.toJSON pid.toText), ("span_count", AE.toJSON $ V.length projectSpans)]
      processProjectSpans pid projectSpans fiveMinutesAgo scheduledTime

  Log.logInfo "Completed 5-minute span processing" ()


logsPatternExtraction :: UTCTime -> ATBackgroundCtx ()
logsPatternExtraction scheduledTime = do
  ctx <- ask @Config.AuthContext
  fiveMinutesAgo <- liftIO $ addUTCTime (-300) <$> Time.currentTime

  Log.logInfo "Fetching logs for pattern extraction" ()
  otelLogs <- dbtToEff $ query [sql| SELECT project_id, id::text, body::text FROM otel_logs_and_spans WHERE timestamp >= ? and timestamp < ?  and kind = 'log' and log_pattern is null and body is not null and body != 'null' |] (fiveMinutesAgo, scheduledTime)
  Log.logInfo "Fetched OTEL logs for pattern extraction" ("log_count", AE.toJSON $ V.length otelLogs)

  logsByProject <- pure $ V.groupBy (\(p, _, _) (bp, _, _) -> p == bp) otelLogs
  Log.logInfo "Grouped logs by project for pattern extraction" ("project_count", AE.toJSON $ length logsByProject)

  forM_ logsByProject \projectLogs -> case V.uncons projectLogs of
    Nothing -> pass
    Just ((pidTxt, _, _), _) -> do
      let pid = Projects.ProjectId $ Unsafe.fromJust $ UUID.fromText pidTxt
      Log.logInfo "Processing project logs" $ AE.object [("project_id", AE.toJSON pid.toText), ("log_count", AE.toJSON $ V.length projectLogs)]
      Relude.when (null projectLogs) $ do
        Log.logInfo "No logs found for pattern extraction" ("project_id", AE.toJSON pid.toText)
      unless (null projectLogs) $ do
        Log.logInfo "Fetching existing log patterns for project" ("project_id", AE.toJSON pid.toText)
        logsWithPatterns' <- dbtToEff $ query [sql| SELECT log_pattern FROM otel_logs_and_spans WHERE project_id = ? and timestamp >= now() - interval '2 hours' AND kind = 'log' AND log_pattern IS NOT NULL limit 3000|] (pid)
        let logsWithPatterns = (\(p) -> ("", p)) <$> logsWithPatterns'
        let fLogs = V.map (\(_, logId, logContent) -> (logId, logContent)) projectLogs
        let finalVals = logsWithPatterns <> fLogs
        let drainTree = processBatch finalVals scheduledTime Drain.emptyDrainTree
            patterns = Drain.getAllLogGroups drainTree
        Log.logInfo "Extracted log patterns" ("pattern_count", AE.toJSON $ V.length patterns)
        forM_ patterns \p -> do
          _ <- dbtToEff $ execute [sql| UPDATE otel_logs_and_spans SET log_pattern = ? WHERE project_id=? and timestamp > ? and id::text=Any(?) |] (fst p, pid, fiveMinutesAgo, V.filter (\p' -> p' /= "") $ snd p)
          pass
        Log.logInfo "Completed logs pattern extraction for project" ("project_id", AE.toJSON pid.toText)
        pass
  where
    processNewLog :: Text -> Text -> UTCTime -> Drain.DrainTree -> Drain.DrainTree
    processNewLog logId logContent now tree = do
      let tokensVec = V.fromList $ T.words $ replaceAllFormats $ logContent
          tokenCount = V.length tokensVec
          firstToken = if V.null tokensVec then "" else V.head tokensVec
       in if tokenCount == 0
            then tree -- Skip empty logs
            else Drain.updateTreeWithLog tree tokenCount firstToken tokensVec logId logContent now
    processBatch :: V.Vector (Text, Text) -> UTCTime -> Drain.DrainTree -> Drain.DrainTree
    processBatch logBatch now initialTree = do
      V.foldl (\tree (logId, logContent) -> processNewLog logId logContent now tree) initialTree logBatch


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
processOneMinuteErrors :: UTCTime -> ATBackgroundCtx ()
processOneMinuteErrors scheduledTime = do
  ctx <- ask @Config.AuthContext
  -- This processing might happen before the spans within the timestamp are stored in db
  -- hence will be missed and never get processed
  -- since we use hashes of errors and don't insert same error twice
  -- we can increase the window to account for time spent on kafka
  -- use two minutes for now before use a better solution
  let oneMinuteAgo = addUTCTime (-60 * 2) scheduledTime
  -- Get all spans with errors from time window
  -- Check for:
  -- 1. Spans with error status codes
  -- 2. Spans with exception events (OpenTelemetry standard)
  -- 3. Spans with error attributes
  spansWithErrors <-
    dbtToEff
      $ query
        [sql| SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity, body, attributes, resource, 
                     hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, summary, date
              FROM otel_logs_and_spans 
          WHERE timestamp >= ? AND timestamp < ?
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
          ORDER BY project_id |]
        (oneMinuteAgo, scheduledTime)
  Log.logInfo "Processing spans with errors from 1-minute window" ("span_count", AE.toJSON $ V.length spansWithErrors)

  -- Extract errors from all spans using the existing getAllATErrors function
  let allErrors = Telemetry.getAllATErrors spansWithErrors

  -- Log.logInfo "Found errors to process" ("error_count", AE.toJSON $ V.length allErrors)

  -- Group errors by project
  let errorsByProject = V.groupBy (\a b -> a.projectId == b.projectId) allErrors

  -- Group errors by traceId within each project to avoid duplicate errors from same trace
  -- (otel log and span, [errors])
  let errorsByProjectAndTrace = V.groupBy (\a b -> a.traceId == b.traceId && a.projectId == b.projectId && a.spanId == b.spanId) allErrors

  forM_ errorsByProject \projectErrors -> case V.uncons projectErrors of
    Nothing -> pass
    Just (firstError, _) -> do
      whenJust firstError.projectId \pid -> do
        -- Process errors for this project
        processProjectErrors pid projectErrors

  forM_ errorsByProjectAndTrace \groupedErrors -> case V.uncons groupedErrors of
    Nothing -> pass
    Just (firstError, _) -> do
      whenJust firstError.projectId \pid -> do
        let mappedErrors = V.map (\x -> AE.object ["type" AE..= x.errorType, "message" AE..= x.message, "stack_trace" AE..= x.stackTrace]) groupedErrors
        Relude.when (not $ V.null groupedErrors) $ do
          _ <-
            dbtToEff
              $ execute
                [sql| UPDATE otel_logs_and_spans 
                  SET errors = ? 
                  WHERE project_id = ? AND context___trace_id = ? AND context___span_id = ? |]
                (AE.toJSON mappedErrors, pid, firstError.traceId, firstError.spanId)
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
  result <- try $ forM_ (V.zip queries paramsList) \(q, params) -> do
    dbtToEff $ execute q params

  case result of
    Left (e :: SomePostgreSqlException) ->
      Log.logAttention "Failed to insert errors" ("error", AE.toJSON $ show e)
    Right _ ->
      Log.logInfo "Successfully inserted errors for project" $ AE.object [("project_id", AE.toJSON pid.toText), ("error_count", AE.toJSON $ V.length errors)]
  where
    -- Process a single error - the error already has requestMethod and requestPath
    -- set by getAllATErrors if it was extracted from span context
    processError :: RequestDumps.ATError -> (RequestDumps.ATError, Query, [DBField])
    processError err = RequestMessages.processErrors pid Nothing Nothing Nothing err


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
    mpjCache <- withPool ctx.jobsPool $ Projects.projectCacheById pid'
    pure $ fromMaybe projectCacheDefault mpjCache

  -- Process each span to extract entities
  results <- forM spans \spn -> do
    spanId <- liftIO UUIDV4.nextRandom
    let result = processSpanToEntities projectCacheVal spn spanId
    pure result

  -- Unzip and deduplicate extracted entities
  let (endpoints, shapes, fields, formats, spanUpdates) = V.unzip5 results
  let endpointsFinal = VAA.nubBy (comparing (.hash)) $ V.fromList $ catMaybes $ V.toList endpoints
  let shapesFinal = VAA.nubBy (comparing (.hash)) $ V.fromList $ catMaybes $ V.toList shapes
  let fieldsFinal = VAA.nubBy (comparing (.hash)) $ V.concat $ V.toList fields
  let formatsFinal = VAA.nubBy (comparing (.hash)) $ V.concat $ V.toList formats

  Log.logInfo
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
    unless (null fieldsFinal) $ void $ Ki.fork scope $ FieldsQ.bulkInsertFields fieldsFinal
    unless (null formatsFinal) $ void $ Ki.fork scope $ Formats.bulkInsertFormat formatsFinal
    Ki.atomically $ Ki.awaitAll scope

  case result of
    Left (e :: SomePostgreSqlException) -> Log.logAttention "Postgres Exception during span processing" (show e)
    Right _ -> do
      -- Update span records with computed hashes
      Log.logInfo "Updating spans with computed hashes" ("span_count", AE.toJSON $ V.length spans)
      forM_ (V.zip spans spanUpdates) \(spn, hashes) -> do
        Relude.when (not $ V.null hashes) $ do
          _ <-
            dbtToEff
              $ execute
                [sql| UPDATE otel_logs_and_spans 
                  SET hashes = ? 
                  WHERE project_id = ? and timestamp >= ? and timestamp < ? AND id = ? |]
                (hashes, pid, fiveMinutesAgo, scheduledTime, spn.id)
          pass
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


-- FIXME: implement inteligent allerting logic, where we pause to ensure users are not alerted too often, or spammed.
-- sendTestFailedAlert :: Projects.ProjectId -> Testing.CollectionId -> Testing.Collection -> V.Vector Testing.StepResult -> ATBackgroundCtx ()
-- sendTestFailedAlert pid col_id collection stepResult = do
--   let
--     -- sv = if collection.alertSeverity == "" then "INFO" else collection.alertSeverity
--     sbjt = collection.title <> ": " <> if collection.alertSubject == "" then "API Test Failed" else collection.alertSubject
--     (Testing.CollectionSteps colStepsV) = collection.collectionSteps
--     failedSteps = Testing.getCollectionFailedSteps colStepsV stepResult
--     msg' = "Failing steps: \n" <> unwords (V.toList $ V.catMaybes $ V.map (\(s, rs) -> s.title) failedSteps)
--     msg = if collection.alertMessage == "" then msg' else collection.alertMessage
--   users <- dbtToEff $ Projects.usersByProjectId pid
--   forM_ users \user -> do
--     let email = CI.original user.email
--     sendPostmarkEmail email Nothing (Just (sbjt, msg))
--   pass

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


queryMonitorsTriggered :: V.Vector Monitors.QueryMonitorId -> ATBackgroundCtx ()
queryMonitorsTriggered queryMonitorIds = do
  monitorsEvaled <- dbtToEff $ Monitors.queryMonitorsById queryMonitorIds
  forM_ monitorsEvaled \monitorE ->
    if (monitorE.triggerLessThan && monitorE.evalResult >= monitorE.alertThreshold)
      || (not monitorE.triggerLessThan && monitorE.evalResult <= monitorE.alertThreshold)
      then handleQueryMonitorThreshold monitorE True
      else do
        if Just True
          == ( monitorE.warningThreshold <&> \warningThreshold ->
                 (monitorE.triggerLessThan && monitorE.evalResult >= warningThreshold)
                   || (not monitorE.triggerLessThan && monitorE.evalResult <= warningThreshold)
             )
          then handleQueryMonitorThreshold monitorE False
          else pass


handleQueryMonitorThreshold :: Monitors.QueryMonitorEvaled -> Bool -> ATBackgroundCtx ()
handleQueryMonitorThreshold monitorE isAlert = do
  Log.logAttention "Query Monitors Triggered " monitorE
  _ <- dbtToEff $ Monitors.updateQMonitorTriggeredState monitorE.id isAlert

  -- Create query alert issue
  let thresholdType = if monitorE.triggerLessThan then "below" else "above"
      threshold = fromIntegral monitorE.alertThreshold

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

  dbtToEff $ Issues.insertIssue issue

  -- Send notifications
  Relude.when monitorE.alertConfig.emailAll do
    users <- dbtToEff $ Projects.usersByProjectId monitorE.projectId
    forM_ users \u -> emailQueryMonitorAlert monitorE u.email (Just u)
  forM_ monitorE.alertConfig.emails \email -> emailQueryMonitorAlert monitorE email Nothing
  unless (null monitorE.alertConfig.slackChannels) $ sendSlackMessage monitorE.projectId [fmtTrim| 🤖 *Log Alert triggered for `{monitorE.alertConfig.title}`*|]


-- way to get emails for company. for email all
-- TODO: based on monitor send emails or slack

jobsWorkerInit :: Logger -> Config.AuthContext -> TracerProvider -> IO ()
jobsWorkerInit logger appCtx tp =
  startJobRunner
    $ mkConfig jobLogger "background_jobs" appCtx.jobsPool (MaxConcurrentJobs 1) (jobsRunner logger appCtx tp) id
  where
    jobLogger :: OddJobs.Job.LogLevel -> LogEvent -> IO ()
    jobLogger logLevel logEvent = runLogT "OddJobs" logger LogAttention $ LogLegacy.logInfo "Background jobs ping." (show @Text logLevel, show @Text logEvent) -- logger show (logLevel, logEvent)
    -- jobLogger logLevel logEvent = print show (logLevel, logEvent) -- logger show (logLevel, logEvent)


data ReportType = DailyReport | WeeklyReport
  deriving (Show)


sendReportForProject :: Projects.ProjectId -> ReportType -> ATBackgroundCtx ()
sendReportForProject pid rType = do
  ctx <- ask @Config.AuthContext
  users <- dbtToEff $ Projects.usersByProjectId pid
  currentTime <- Time.currentTime
  let (prv, typTxt, intv) = case rType of
        WeeklyReport -> (6 * 86400, "weekly", "1d")
        _ -> (86400, "daily", "1h")

  let startTime = addUTCTime (negate prv) currentTime
  projectM <- dbtToEff $ Projects.projectById pid
  forM_ projectM \pr -> do
    stats <- Telemetry.getProjectStatsForReport pid startTime currentTime
    -- TODO: Replace with Issues.selectIssues when fully migrated
    anomalies <- dbtToEff $ Issues.selectIssues pid Nothing Nothing Nothing 100 0
    endpoint_rp <- dbtToEff $ RequestDumps.getRequestDumpForReports pid typTxt
    total_anomalies <- dbtToEff $ Anomalies.countAnomalies pid typTxt
    previous_week <- dbtToEff $ RequestDumps.getRequestDumpsForPreviousReportPeriod pid typTxt
    let rep_json = RP.buildReportJSON anomalies endpoint_rp previous_week
    timeZone <- liftIO getCurrentTimeZone

    reportId <- Reports.ReportId <$> liftIO UUIDV4.nextRandom
    let report =
          Reports.Report
            { id = reportId
            , reportJson = rep_json
            , createdAt = utcToZonedTime timeZone currentTime
            , updatedAt = utcToZonedTime timeZone currentTime
            , projectId = pid
            , reportType = typTxt
            }
    _ <- dbtToEff $ Reports.addReport report

    let totalErrors = sum $ map (\(_, x, _) -> x) (V.toList stats)
        totalEvents = sum $ map (\(_, _, x) -> x) (V.toList stats)
        stmTxt = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" startTime
        currentTimeTxt = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" currentTime
        reportUrl = ctx.env.hostUrl <> "p/" <> pid.toText <> "/reports/" <> show report.id.reportId
        chartShotUrl = ctx.env.chartShotUrl <> "?t=bar&p=" <> pid.toText <> "&from=" <> stmTxt <> "&to=" <> currentTimeTxt
        allQ = chartShotUrl <> "&q=" <> (decodeUtf8 $ urlEncode True (encodeUtf8 "summarize count(*) by bin(timestamp," <> intv <> "), resource___service___name"))
        errQ = chartShotUrl <> "&theme=roma" <> "&q=" <> (decodeUtf8 $ urlEncode True (encodeUtf8 "status_code == \"ERROR\" | summarize count(*) by bin(timestamp," <> intv <> "), resource___service___name"))
        alert = ReportAlert typTxt stmTxt currentTimeTxt totalErrors totalEvents stats reportUrl allQ errQ

    Relude.when pr.weeklyNotif $ forM_ pr.notificationsChannel \case
      Projects.NDiscord -> do
        sendDiscordAlert alert pid pr.title
      Projects.NSlack -> do
        sendSlackAlert alert pid pr.title
      Projects.NPhone -> do
        sendWhatsAppAlert alert pid pr.title pr.whatsappNumbers
      _ -> do
        totalRequest <- dbtToEff $ RequestDumps.getLastSevenDaysTotalRequest pid
        Relude.when (totalRequest > 0) do
          forM_ users \user -> do
            let firstName = user.firstName
                projectTitle = pr.title
                userEmail = CI.original user.email
                anmls = if total_anomalies == 0 then [AE.object ["message" AE..= "No anomalies detected yet."]] else RP.getAnomaliesEmailTemplate anomalies
                perf = RP.getPerformanceEmailTemplate endpoint_rp previous_week
                perf_count = V.length perf
                perf_shrt = if perf_count == 0 then [AE.object ["message" AE..= "No performance data yet."]] else V.take 10 perf
                rp_url = "https://app.monoscope.tech/p/" <> pid.toText <> "/reports/" <> show report.id.reportId
                dayEnd = show $ localDay (zonedTimeToLocalTime (utcToZonedTime timeZone currentTime))
                sevenDaysAgoUTCTime = addUTCTime (negate $ 6 * 86400) currentTime
                sevenDaysAgoZonedTime = utcToZonedTime timeZone sevenDaysAgoUTCTime
                dayStart = show $ localDay (zonedTimeToLocalTime sevenDaysAgoZonedTime)
                freeTierLimitExceeded = pr.paymentPlan == "FREE" && totalRequest > 5000
                templateVars =
                  [aesonQQ|{
                   "user_name": #{firstName},
                   "project_name": #{projectTitle},
                   "anomalies_count": #{total_anomalies},
                   "anomalies":  #{anmls},
                   "report_url": #{rp_url},
                   "performance_count": #{perf_count},
                   "performance": #{perf_shrt},
                   "start_date": #{dayStart},
                   "end_date": #{dayEnd},
                   "free_exceeded": #{freeTierLimitExceeded}
            }|]
            sendPostmarkEmail userEmail (Just ("weekly-report", templateVars)) Nothing


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

  case anomalyType of
    -- API Change anomalies (endpoint, shape, format) - group into single issue per endpoint
    -- This prevents notification spam when multiple related changes occur
    Anomalies.ATEndpoint -> processAPIChangeAnomalies pid targetHashes
    Anomalies.ATShape -> processAPIChangeAnomalies pid targetHashes
    Anomalies.ATFormat -> processAPIChangeAnomalies pid targetHashes
    -- Runtime exceptions get individual issues
    -- Each unique error pattern gets its own issue for tracking
    Anomalies.ATRuntimeException -> do
      errors <- dbtToEff $ Anomalies.errorsByHashes pid targetHashes
      project <- Unsafe.fromJust <<$>> dbtToEff $ Projects.projectById pid
      users <- dbtToEff $ Projects.usersByProjectId pid

      -- Create one issue per error
      forM_ errors \err -> do
        issue <- liftIO $ Issues.createRuntimeExceptionIssue pid err.errorData
        dbtToEff $ Issues.insertIssue issue

        -- Queue enhancement job
        _ <- liftIO $ withResource authCtx.jobsPool \conn ->
          createJob conn "background_jobs" $ BackgroundJobs.EnhanceIssuesWithLLM pid (V.singleton issue.id)
        pass

      -- Send notifications
      forM_ project.notificationsChannel \case
        Projects.NSlack ->
          forM_ errors \err -> do sendSlackAlert (RuntimeErrorAlert err.errorData) pid project.title
        Projects.NDiscord ->
          forM_ errors \err -> do sendDiscordAlert (RuntimeErrorAlert err.errorData) pid project.title
        Projects.NPhone ->
          forM_ errors \err -> do
            sendWhatsAppAlert (RuntimeErrorAlert err.errorData) pid project.title project.whatsappNumbers
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
                errors_url = "https://app.monoscope.tech/p/" <> pid.toText <> "/issues/"
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
  anomaliesVM <- dbtToEff $ Anomalies.getAnomaliesVM pid targetHashes

  -- Group by endpoint hash to consolidate related changes
  let anomaliesByEndpoint = groupAnomaliesByEndpointHash anomaliesVM

  -- Process each endpoint group
  forM_ anomaliesByEndpoint \(endpointHash, anomalies) -> do
    -- Check for existing open issue to avoid duplicates
    existingIssueM <- dbtToEff $ Issues.findOpenIssueForEndpoint pid endpointHash

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
        dbtToEff $ Issues.updateIssueWithNewAnomaly existingIssue.id apiChangeData
      Nothing -> do
        -- Create new issue
        issue <- liftIO $ Issues.createAPIChangeIssue pid endpointHash anomalies
        dbtToEff $ Issues.insertIssue issue

        -- Queue enhancement job
        _ <- liftIO $ withResource authCtx.jobsPool \conn ->
          createJob conn "background_jobs" $ BackgroundJobs.EnhanceIssuesWithLLM pid (V.singleton issue.id)
        pass

  -- Send notifications
  projectM <- dbtToEff $ Projects.projectById pid
  whenJust projectM \project -> do
    users <- dbtToEff $ Projects.usersByProjectId pid
    let endpointInfo =
          map
            ( \(_, anoms) ->
                let firstAnom = V.head anoms
                 in fromMaybe "UNKNOWN" firstAnom.endpointMethod <> " " <> fromMaybe "/" firstAnom.endpointUrlPath
            )
            anomaliesByEndpoint
    let alert = EndpointAlert project.title (V.fromList endpointInfo) (fromMaybe "" $ viaNonEmpty head $ V.toList targetHashes)

    forM_ project.notificationsChannel \case
      Projects.NSlack -> sendSlackAlert alert pid project.title
      Projects.NDiscord -> sendDiscordAlert alert pid project.title
      Projects.NPhone -> sendWhatsAppAlert alert pid project.title project.whatsappNumbers
      Projects.NEmail -> do
        forM_ users \u -> do
          let templateVars =
                AE.object
                  [ "user_name" AE..= u.firstName
                  , "project_name" AE..= project.title
                  , "anomaly_url" AE..= ("https://app.monoscope.tech/p/" <> pid.toText <> "/issues")
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
    dbtToEff
      $ query
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
        let issueIds = V.map (Issues.IssueId . fst) projectIssues
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
        issueM <- dbtToEff $ Issues.selectIssueById issueId
        case issueM of
          Nothing -> Log.logAttention "Issue not found for enhancement" issueId
          Just issue -> do
            -- Call LLM to enhance the issue based on type
            enhancementResult <- liftIO $ Enhancement.enhanceIssueWithLLM ctx issue
            case enhancementResult of
              Left err -> Log.logAttention "Failed to enhance issue with LLM" (issueId, err)
              Right enhancement -> do
                -- Update the issue with enhanced data
                _ <-
                  dbtToEff
                    $ Issues.updateIssueEnhancement
                      enhancement.issueId
                      enhancement.enhancedTitle
                      enhancement.recommendedAction
                      enhancement.migrationComplexity

                -- Also classify and update criticality
                criticalityResult <- liftIO $ Enhancement.classifyIssueCriticality ctx issue
                case criticalityResult of
                  Left err -> Log.logAttention "Failed to classify issue criticality" (issueId, err)
                  Right (isCritical, breakingCount, incrementalCount) -> do
                    _ <- dbtToEff $ Enhancement.updateIssueClassification issue.id isCritical breakingCount incrementalCount
                    Log.logInfo "Successfully enhanced and classified issue" (issueId, isCritical, breakingCount)

                Log.logInfo "Successfully enhanced issue" issueId
