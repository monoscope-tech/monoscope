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
import Effectful.Log (Log, object)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Reader.Static (ask)
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
import Models.Projects.Swaggers qualified as Swaggers
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Tests.TestToDump qualified as TestToDump
import Models.Tests.Testing qualified as Testing
import Models.Users.Users qualified as Users
import Network.HTTP.Types (urlEncode)
import Network.Wreq (defaults, header, postWith)
import OddJobs.ConfigBuilder (mkConfig)
import OddJobs.Job (ConcurrencyControl (..), Job (..), LogEvent, LogLevel, createJob, scheduleJob, startJobRunner, throwParsePayload)
import OpenTelemetry.Attributes qualified as OA
import OpenTelemetry.Trace (TracerProvider)
import Pages.Reports qualified as RP
import Pages.Specification.GenerateSwagger (generateSwagger)
import Pkg.Mail (NotificationAlerts (EndpointAlert, ReportAlert, RuntimeErrorAlert), sendDiscordAlert, sendPostmarkEmail, sendSlackAlert, sendSlackMessage, sendWhatsAppAlert)
import ProcessMessage (processSpanToEntities)
import PyF (fmtTrim)
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
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
  | GenSwagger Projects.ProjectId Users.UserId Text
  | ReportUsage Projects.ProjectId
  | GenerateOtelFacetsBatch (V.Vector Text) UTCTime
  | QueryMonitorsTriggered (V.Vector Monitors.QueryMonitorId)
  | RunCollectionTests Testing.CollectionId
  | DeletedProject Projects.ProjectId
  | APITestFailed Projects.ProjectId Testing.CollectionId (V.Vector Testing.StepResult)
  | CleanupDemoProject
  | FiveMinuteSpanProcessing UTCTime
  | OneMinuteErrorProcessing UTCTime
  | SlackNotification Projects.ProjectId Text
  | EnhanceIssuesWithLLM Projects.ProjectId (V.Vector Issues.IssueId)
  | ProcessIssuesEnhancement UTCTime
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


webhookUrl :: String
webhookUrl = "https://discord.com/api/webhooks/1230980245423788045/JQOJ7w3gmEduaOvPTnxEz4L8teDpX5PJoFkyQmqZHR8HtRqAkWIjv2Xk1aKadTyXuFy_"


sendMessageToDiscord :: Text -> ATBackgroundCtx ()
sendMessageToDiscord msg = do
  let message = AE.object ["content" AE..= msg]
  let opts = defaults & header "Content-Type" .~ ["application/json"]
  response <- liftIO $ postWith opts webhookUrl message
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
        let project_url = "https://app.apitoolkit.io/p/" <> projectId.toText
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
        let project_url = "https://app.apitoolkit.io/p/" <> projectId.toText
        let project_title = project.title
        let msg =
              [fmtTrim|
  🎉 New project created on apitoolkit.io! 🎉
  - **User Full Name**: {fullName}
  - **User Email**: {userEmail}
  - **Project Title**: [{project_title}]({project_url})
  - **User ID**: {userId.toText}
  - **Payment Plan**: {project.paymentPlan}
  - **Stack**: {stackString}
  - **Found us from**: {foundUsFrom}
  |]
        sendMessageToDiscord msg
    CreatedProjectSuccessfully userId projectId reciever projectTitle -> do
      userM <- Users.userById userId
      whenJust userM \user -> do
        let firstName = user.firstName
        let project_url = "https://app.apitoolkit.io/p/" <> projectId.toText
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
        -- background job to cleanup demo project
        Relude.when (dayOfWeek currentDay == Monday) do
          void $ createJob conn "background_jobs" $ BackgroundJobs.CleanupDemoProject
        forM_ [0 .. 23] \hour -> do
          -- Schedule each hourly job to run at the appropriate hour
          let scheduledTime = addUTCTime (fromIntegral $ hour * 3600) currentTime
          _ <- scheduleJob conn "background_jobs" (BackgroundJobs.HourlyJob scheduledTime hour) scheduledTime

          -- Schedule 5-minute span processing jobs (288 jobs per day = 24 hours * 12 per hour)
          forM_ [0 .. 287] \interval -> do
            let scheduledTime2 = addUTCTime (fromIntegral $ interval * 300) currentTime
            scheduleJob conn "background_jobs" (BackgroundJobs.FiveMinuteSpanProcessing scheduledTime2) scheduledTime2

          -- Schedule 1-minute error processing jobs (1440 jobs per day = 24 hours * 60 per hour)
          forM_ [0 .. 1439] \interval -> do
            let scheduledTime3 = addUTCTime (fromIntegral $ interval * 60) currentTime
            scheduleJob conn "background_jobs" (BackgroundJobs.OneMinuteErrorProcessing scheduledTime3) scheduledTime3

          -- Schedule issue enhancement processing every hour
          forM_ [0 .. 23] \hr -> do
            let scheduledTime4 = addUTCTime (fromIntegral $ hr * 3600) currentTime
            scheduleJob conn "background_jobs" (BackgroundJobs.ProcessIssuesEnhancement scheduledTime4) scheduledTime4

      -- Handle regular daily jobs for each project
      projects <- dbtToEff $ query [sql|SELECT id FROM projects.projects WHERE active=? AND deleted_at IS NULL and payment_plan != 'ONBOARDING'|] (Only True)
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
    GenSwagger pid uid host -> generateSwaggerForProject pid uid host
    ReportUsage pid -> whenJustM (dbtToEff $ Projects.projectById pid) \project -> do
      Relude.when (project.paymentPlan /= "Free" && project.paymentPlan /= "ONBOARDING") $ whenJust project.firstSubItemId \fSubId -> do
        currentTime <- liftIO getZonedTime
        totalToReport <- Telemetry.getTotalEventsToReport pid project.usageLastReported
        Relude.when (totalToReport > 0) do
          liftIO $ reportUsageToLemonsqueezy fSubId totalToReport authCtx.config.lemonSqueezyApiKey
          _ <- dbtToEff $ LemonSqueezy.addDailyUsageReport pid totalToReport
          _ <- dbtToEff $ Projects.updateUsageLastReported pid currentTime
          pass
    RunCollectionTests col_id -> do
      now <- Time.currentTime
      collectionM <- dbtToEff $ Testing.getCollectionById col_id
      if job.jobRunAt > addUTCTime (-900) now -- Run time is less than 15 mins ago
        then whenJust collectionM \collection -> Relude.when collection.isScheduled do
          let (Testing.CollectionSteps colStepsV) = collection.collectionSteps
              Testing.CollectionVariables colV = collection.collectionVariables
          stepResultsE <- TestToDump.runCollectionTest colStepsV colV col_id
          case stepResultsE of
            Left e -> do
              _ <- TestToDump.logTest collection.projectId col_id colStepsV (Left e)
              pass
            Right stepResults -> do
              let (_, failed) = Testing.getCollectionRunStatus stepResults
              Relude.when (failed > 0) do
                _ <- liftIO $ withResource authCtx.jobsPool \conn -> do
                  createJob conn "background_jobs" $ BackgroundJobs.APITestFailed collection.projectId col_id stepResults
                pass
              _ <- TestToDump.logTest collection.projectId col_id colStepsV (Right stepResults)
              pass
        else Log.logAttention "RunCollectionTests failed.  Job was sheduled to run over 30 mins ago" $ maybe AE.Null (\c -> AE.object [("title", AE.toJSON c.title), ("id", AE.toJSON c.id)]) collectionM
    APITestFailed pid col_id stepResult -> do
      -- collectionM <- dbtToEff $ Testing.getCollectionById col_id
      -- whenJust collectionM \collection -> do
      --   _ <- sendTestFailedAlert pid col_id collection stepResult
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
generateOtelFacetsBatch :: (DB :> es, IOE :> es, Log :> es, UUID.UUIDEff :> es) => V.Vector Text -> UTCTime -> Eff es ()
generateOtelFacetsBatch projectIds timestamp = do
  forM_ projectIds \pid -> void $ Facets.generateAndSaveFacets (Projects.ProjectId $ Unsafe.fromJust $ UUID.fromText pid) "otel_logs_and_spans" Facets.facetColumns 50 timestamp
  Log.logInfo "Completed batch OTLP facets generation for projects" ("project_count", AE.toJSON $ V.length projectIds)


-- | Process HTTP spans from the last 5 minutes
processFiveMinuteSpans :: UTCTime -> ATBackgroundCtx ()
processFiveMinuteSpans scheduledTime = do
  ctx <- ask @Config.AuthContext
  let fiveMinutesAgo = addUTCTime (-300) scheduledTime

  -- Get HTTP spans from last 5 minutes
  httpSpans <-
    dbtToEff
      $ query
        [sql| SELECT project_id, id, timestamp, observed_timestamp, context, level, severity, body, attributes, resource, 
                     hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, summary, date
              FROM otel_logs_and_spans 
          WHERE timestamp >= ? AND timestamp < ?
          AND kind IN ('server', 'client')
          AND (name LIKE '%http%' 
               OR attributes___http___request___method IS NOT NULL
               OR attributes___http___response___status_code IS NOT NULL
               OR attributes___url___full IS NOT NULL)   
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
      processProjectSpans pid projectSpans

  Log.logInfo "Completed 5-minute span processing" ()


-- | Process errors from spans in the last minute
processOneMinuteErrors :: UTCTime -> ATBackgroundCtx ()
processOneMinuteErrors scheduledTime = do
  ctx <- ask @Config.AuthContext
  let oneMinuteAgo = addUTCTime (-60) scheduledTime

  -- Get all spans with errors from last minute
  -- Check for:
  -- 1. Spans with error status
  -- 2. Spans with exception events
  -- 3. Spans with error attributes
  spansWithErrors <-
    dbtToEff
      $ query
        [sql| SELECT project_id, id, timestamp, observed_timestamp, context, level, severity, body, attributes, resource, 
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

  Log.logInfo "Found errors to process" ("error_count", AE.toJSON $ V.length allErrors)

  -- Group errors by project
  let errorsByProject = V.groupBy (\a b -> a.projectId == b.projectId) allErrors

  forM_ errorsByProject \projectErrors -> case V.uncons projectErrors of
    Nothing -> pass
    Just (firstError, _) -> do
      whenJust firstError.projectId \pid -> do
        -- Process errors for this project
        processProjectErrors pid projectErrors

  Log.logInfo "Completed 1-minute error processing" ()


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


-- | Process spans for a specific project
processProjectSpans :: Projects.ProjectId -> V.Vector Telemetry.OtelLogsAndSpans -> ATBackgroundCtx ()
processProjectSpans pid spans = do
  ctx <- ask @Config.AuthContext

  -- Get project cache
  projectCacheVal <- liftIO $ Cache.fetchWithCache ctx.projectCache pid \pid' -> do
    mpjCache <- withPool ctx.jobsPool $ Projects.projectCacheById pid'
    pure $ fromMaybe projectCacheDefault mpjCache

  -- Process each span to extract endpoints, shapes, fields, and formats
  results <- forM spans \spn -> do
    spanId <- liftIO UUIDV4.nextRandom
    let result = processSpanToEntities projectCacheVal spn spanId
    pure result

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
      forM_ (V.zip spans spanUpdates) \(spn, hashes) -> do
        Relude.when (not $ V.null hashes) $ do
          _ <-
            dbtToEff
              $ execute
                [sql| UPDATE otel_logs_and_spans 
                  SET hashes = ? 
                  WHERE id = ? |]
                (hashes, spn.id)
          pass
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


generateSwaggerForProject :: Projects.ProjectId -> Users.UserId -> Text -> ATBackgroundCtx ()
generateSwaggerForProject pid uid host = whenJustM (dbtToEff $ Projects.projectById pid) \project -> do
  endpoints <- dbtToEff $ Endpoints.endpointsByProjectId pid host
  let endpoint_hashes = V.map (.hash) endpoints
  shapes <- dbtToEff $ Shapes.shapesByEndpointHashes pid endpoint_hashes
  fields <- dbtToEff $ FieldsQ.fieldsByEndpointHashes pid endpoint_hashes
  let field_hashes = V.map (.fHash) fields
  formats <- dbtToEff $ Formats.formatsByFieldsHashes pid field_hashes
  let (projectTitle, projectDescription) = (toText project.title, toText project.description)
  let swagger = generateSwagger projectTitle projectDescription endpoints shapes fields formats
  swaggerId <- Swaggers.SwaggerId <$> liftIO UUIDV4.nextRandom
  currentTime <- liftIO getZonedTime
  let swaggerToAdd =
        Swaggers.Swagger
          { id = swaggerId
          , projectId = pid
          , createdBy = uid
          , createdAt = currentTime
          , updatedAt = currentTime
          , swaggerJson = swagger
          , host = host
          }
  dbtToEff $ Swaggers.addSwagger swaggerToAdd


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
                rp_url = "https://app.apitoolkit.io/p/" <> pid.toText <> "/reports/" <> show report.id.reportId
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

newAnomalyJob :: Projects.ProjectId -> ZonedTime -> Text -> Text -> V.Vector Text -> ATBackgroundCtx ()
newAnomalyJob pid createdAt anomalyTypesT anomalyActionsT targetHashes = do
  authCtx <- ask @Config.AuthContext
  let anomalyType = fromMaybe (error "parseAnomalyTypes returned Nothing") $ Anomalies.parseAnomalyTypes anomalyTypesT

  case anomalyType of
    -- API Change anomalies (endpoint, shape, format) - group into single issue per endpoint
    Anomalies.ATEndpoint -> processAPIChangeAnomalies pid targetHashes
    Anomalies.ATShape -> processAPIChangeAnomalies pid targetHashes
    Anomalies.ATFormat -> processAPIChangeAnomalies pid targetHashes
    -- Runtime exceptions get individual issues
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
                errors_url = "https://app.apitoolkit.io/p/" <> pid.toText <> "/issues/"
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
processAPIChangeAnomalies :: Projects.ProjectId -> V.Vector Text -> ATBackgroundCtx ()
processAPIChangeAnomalies pid targetHashes = do
  authCtx <- ask @Config.AuthContext

  -- Get all anomalies
  anomaliesVM <- dbtToEff $ Anomalies.getAnomaliesVM pid targetHashes

  -- Group by endpoint hash
  let anomaliesByEndpoint = groupAnomaliesByEndpointHash anomaliesVM

  -- Process each endpoint group
  forM_ anomaliesByEndpoint \(endpointHash, anomalies) -> do
    -- Check for existing open issue
    existingIssueM <- dbtToEff $ Issues.findOpenIssueForEndpoint pid endpointHash

    case existingIssueM of
      Just existingIssue -> do
        -- Update existing issue
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
                  , "anomaly_url" AE..= ("https://app.apitoolkit.io/p/" <> pid.toText <> "/issues")
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
