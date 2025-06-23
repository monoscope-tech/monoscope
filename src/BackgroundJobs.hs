module BackgroundJobs (jobsWorkerInit, jobsRunner, BgJobs (..), runHourlyJob, generateOtelFacetsBatch) where

import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Cache qualified as Cache
import Data.CaseInsensitive qualified as CI
import Data.Effectful.UUID qualified as UUID
import Data.List qualified as L (intersect, union)
import Data.List.Extra (chunksOf)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (DayOfWeek (Monday), UTCTime (utctDay), ZonedTime, addUTCTime, dayOfWeek, formatTime, getZonedTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime), getCurrentTimeZone, utcToZonedTime, zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Data.Vector.Algorithms qualified as VAA
import Database.PostgreSQL.Entity.DBT (execute, query, withPool)
import Database.PostgreSQL.Simple (Only (Only), Query, SomePostgreSqlException)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact qualified as PTR
import Effectful (Eff, (:>))
import Effectful.Ki qualified as Ki
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Log qualified
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Facets qualified as Facets
import Models.Apis.Fields.Query qualified as FieldsQ
import Models.Apis.Fields.Types qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.Reports qualified as Reports
import Models.Apis.RequestDumps (ATError (..))
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Apis.Slack (DiscordData (..), getDiscordDataByProjectId)
import Models.Projects.LemonSqueezy qualified as LemonSqueezy
import Models.Projects.Projects (ProjectId (unProjectId))
import Models.Projects.Projects qualified as Projects
import Models.Projects.Swaggers qualified as Swaggers
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Tests.TestToDump qualified as TestToDump
import Models.Tests.Testing qualified as Testing
import Models.Users.Users qualified as Users
import NeatInterpolation (trimming)
import Network.Wreq (defaults, header, postWith)
import OddJobs.ConfigBuilder (mkConfig)
import OddJobs.Job (ConcurrencyControl (..), Job (..), LogEvent, LogLevel, createJob, scheduleJob, startJobRunner, throwParsePayload)
import Pages.Reports qualified as RP
import Pages.Specification.GenerateSwagger (generateSwagger)
import Pkg.Mail (NotificationAlerts (EndpointAlert, RuntimeErrorAlert), sendDiscordAlert, sendDiscordNotif, sendPostmarkEmail, sendSlackAlert, sendSlackMessage)
import ProcessMessage (processSpanToEntities)
import PyF (fmtTrim)
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import RequestMessages qualified
import System.Config qualified as Config
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


jobsRunner :: Log.Logger -> Config.AuthContext -> Job -> IO ()
jobsRunner logger authCtx job = Relude.when authCtx.config.enableBackgroundJobs $ do
  bgJob <- throwParsePayload job
  void $ runBackground logger authCtx do
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
            scheduleJob conn "background_jobs" (BackgroundJobs.HourlyJob scheduledTime hour) scheduledTime

          -- Schedule 5-minute span processing jobs (288 jobs per day = 24 hours * 12 per hour)
          forM_ [0 .. 287] \interval -> do
            let scheduledTime = addUTCTime (fromIntegral $ interval * 300) currentTime
            scheduleJob conn "background_jobs" (BackgroundJobs.FiveMinuteSpanProcessing scheduledTime) scheduledTime

          -- Schedule 1-minute error processing jobs (1440 jobs per day = 24 hours * 60 per hour)
          forM_ [0 .. 1439] \interval -> do
            let scheduledTime = addUTCTime (fromIntegral $ interval * 60) currentTime
            scheduleJob conn "background_jobs" (BackgroundJobs.OneMinuteErrorProcessing scheduledTime) scheduledTime

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
      DailyReports pid -> dailyReportForProject pid
      WeeklyReports pid -> weeklyReportForProject pid
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
          else Log.logAttention "RunCollectionTests failed.  Job was sheduled to run over 30 mins ago" $ collectionM <&> \c -> (c.title, c.id)
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
  Log.logInfo "Projects with new data in the last hour window" (length activeProjects)

  -- Batch projects in groups of 100
  let batchSize = 100
      projectBatches = chunksOf batchSize $ V.toList activeProjects

  -- For each batch, create a single job with multiple project IDs
  liftIO $ withResource ctx.jobsPool \conn ->
    forM_ projectBatches \batch -> do
      let batchJob = BackgroundJobs.GenerateOtelFacetsBatch (V.fromList batch) scheduledTime
      createJob conn "background_jobs" batchJob

  Log.logInfo "Completed hourly job scheduling for hour" hour


-- | Batch process facets generation for multiple projects using 24-hour window
generateOtelFacetsBatch :: (DB :> es, Log :> es, UUID.UUIDEff :> es) => V.Vector Text -> UTCTime -> Eff es ()
generateOtelFacetsBatch projectIds timestamp = do
  forM_ projectIds \pid -> void $ Facets.generateAndSaveFacets (Projects.ProjectId $ Unsafe.fromJust $ UUID.fromText pid) "otel_logs_and_spans" Facets.facetColumns 50 timestamp
  Log.logInfo "Completed batch OTLP facets generation for projects" (V.length projectIds)


-- | Process HTTP spans from the last 5 minutes
processFiveMinuteSpans :: UTCTime -> ATBackgroundCtx ()
processFiveMinuteSpans scheduledTime = do
  ctx <- ask @Config.AuthContext
  let fiveMinutesAgo = addUTCTime (-300) scheduledTime

  -- Get HTTP spans from last 5 minutes
  httpSpans <-
    dbtToEff
      $ query
        [sql| SELECT * FROM otel_logs_and_spans 
          WHERE timestamp >= ? AND timestamp < ?
          AND kind IN ('server', 'client')
          AND (name LIKE '%http%' 
               OR attributes___http___request___method IS NOT NULL
               OR attributes___http___response___status_code IS NOT NULL
               OR attributes___url___full IS NOT NULL)   
          ORDER BY project_id |]
        (fiveMinutesAgo, scheduledTime)

  Log.logInfo "Processing HTTP spans from 5-minute window" (V.length httpSpans)

  -- Group spans by project
  let spansByProject = V.groupBy (\a b -> a.project_id == b.project_id) httpSpans
  forM_ spansByProject \projectSpans -> case V.uncons projectSpans of
    Nothing -> pass
    Just (firstSpan, _) -> do
      let pid = Projects.ProjectId $ Unsafe.fromJust $ UUID.fromText firstSpan.project_id
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
        [sql| SELECT * FROM otel_logs_and_spans 
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

  Log.logInfo "Processing spans with errors from 1-minute window" (V.length spansWithErrors)

  -- Extract errors from all spans using the existing getAllATErrors function
  let allErrors = Telemetry.getAllATErrors spansWithErrors

  Log.logInfo "Found errors to process" (V.length allErrors)

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
      Log.logAttention "Failed to insert errors" (show e)
    Right _ ->
      Log.logInfo "Successfully inserted errors for project" (pid.toText, V.length errors)
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
    pure $ processSpanToEntities projectCacheVal spn spanId

  let (endpoints, shapes, fields, formats, spanUpdates) = V.unzip5 results
  let endpointsFinal = VAA.nubBy (comparing (.hash)) $ V.fromList $ catMaybes $ V.toList endpoints
  let shapesFinal = VAA.nubBy (comparing (.hash)) $ V.fromList $ catMaybes $ V.toList shapes
  let fieldsFinal = VAA.nubBy (comparing (.hash)) $ V.concat $ V.toList fields
  let formatsFinal = VAA.nubBy (comparing (.hash)) $ V.concat $ V.toList formats

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
  Relude.when monitorE.alertConfig.emailAll do
    users <- dbtToEff $ Projects.usersByProjectId monitorE.projectId
    forM_ users \u -> emailQueryMonitorAlert monitorE u.email (Just u)
  forM_ monitorE.alertConfig.emails \email -> emailQueryMonitorAlert monitorE email Nothing
  unless (null monitorE.alertConfig.slackChannels) $ sendSlackMessage monitorE.projectId [fmtTrim| 🤖 *Log Alert triggered for `{monitorE.alertConfig.title}`*|]


-- way to get emails for company. for email all
-- TODO: based on monitor send emails or slack

jobsWorkerInit :: Log.Logger -> Config.AuthContext -> IO ()
jobsWorkerInit logger appCtx =
  startJobRunner
    $ mkConfig jobLogger "background_jobs" appCtx.jobsPool (MaxConcurrentJobs 1) (jobsRunner logger appCtx) id
  where
    jobLogger :: LogLevel -> LogEvent -> IO ()
    jobLogger logLevel logEvent = Log.runLogT "OddJobs" logger Log.LogAttention $ Log.logInfo "Background jobs ping." (show @Text logLevel, show @Text logEvent) -- logger show (logLevel, logEvent)
    -- jobLogger logLevel logEvent = print show (logLevel, logEvent) -- logger show (logLevel, logEvent)


dailyReportForProject :: Projects.ProjectId -> ATBackgroundCtx ()
dailyReportForProject pid = do
  users <- dbtToEff $ Projects.usersByProjectId pid
  projectM <- dbtToEff $ Projects.projectById pid
  forM_ projectM \pr -> do
    anomalies <- dbtToEff $ Anomalies.getReportAnomalies pid "daily"
    total_anomalies <- dbtToEff $ Anomalies.countAnomalies pid "daily"
    endpoint_rp <- dbtToEff $ RequestDumps.getRequestDumpForReports pid "daily"
    previous_day <- dbtToEff $ RequestDumps.getRequestDumpsForPreviousReportPeriod pid "daily"
    let rep_json = RP.buildReportJSON anomalies endpoint_rp previous_day
    currentTime <- liftIO getZonedTime
    reportId <- Reports.ReportId <$> liftIO UUIDV4.nextRandom
    let report =
          Reports.Report
            { id = reportId
            , reportJson = rep_json
            , createdAt = currentTime
            , updatedAt = currentTime
            , projectId = pid
            , reportType = "daily"
            }
    _ <- dbtToEff $ Reports.addReport report
    Relude.when pr.dailyNotif $ forM_ pr.notificationsChannel \case
      Projects.NSlack ->
        sendSlackMessage
          pid
          [fmtTrim| 🤖 *Daily Report for `{pr.title}`**

<https://app.apitoolkit.io/p/{pid.toText}/reports/{show report.id.reportId}|View today's report>
|]
      Projects.NDiscord -> do
        discordData <- getDiscordDataByProjectId pid
        let projectUrl = "https://app.apitoolkit.io/p/" <> pid.toText <> "/reports/" <> show report.id.reportId
        whenJust discordData \d -> do
          case d.notifsChannelId of
            Just channelId -> sendDiscordNotif channelId [fmtTrim|**Daily REPORT**: [{pr.title}]({projectUrl})|]
            Nothing -> pass
      _ -> do
        users & mapM_ \user -> do
          let firstName = user.firstName
              projectTitle = pr.title
              userEmail = CI.original user.email
              anmls = if total_anomalies == 0 then [AE.object ["message" AE..= "No anomalies detected yet."]] else RP.getAnomaliesEmailTemplate anomalies
              perf = RP.getPerformanceEmailTemplate endpoint_rp previous_day
              perf_count = V.length perf
              perf_shrt = if perf_count == 0 then [AE.object ["message" AE..= "No performance data yet."]] else V.take 10 perf
              rp_url = "https://app.apitoolkit.io/p/" <> pid.toText <> "/reports/" <> show report.id.reportId
              day = show $ localDay (zonedTimeToLocalTime currentTime)
              templateVars =
                [aesonQQ|{
                 "user_name": #{firstName},
                 "project_name": #{projectTitle},
                 "anomalies_count": #{total_anomalies},
                 "anomalies":  #{anmls},
                 "report_url": #{rp_url},
                 "performance_count": #{perf_count},
                 "performance": #{perf_shrt},
                 "start_date": #{day},
                 "end_date": #{day}
          }|]
          sendPostmarkEmail userEmail (Just ("daily-report", templateVars)) Nothing


weeklyReportForProject :: Projects.ProjectId -> ATBackgroundCtx ()
weeklyReportForProject pid = do
  ctx <- ask @Config.AuthContext
  users <- dbtToEff $ Projects.usersByProjectId pid
  projectM <- dbtToEff $ Projects.projectById pid
  forM_ projectM \pr -> do
    anomalies <- dbtToEff $ Anomalies.getReportAnomalies pid "weekly"
    endpoint_rp <- dbtToEff $ RequestDumps.getRequestDumpForReports pid "weekly"
    total_anomalies <- dbtToEff $ Anomalies.countAnomalies pid "weekly"
    previous_week <- dbtToEff $ RequestDumps.getRequestDumpsForPreviousReportPeriod pid "weekly"
    let rep_json = RP.buildReportJSON anomalies endpoint_rp previous_week
    currentTime <- liftIO getZonedTime
    timeZone <- liftIO getCurrentTimeZone

    reportId <- Reports.ReportId <$> liftIO UUIDV4.nextRandom
    let report =
          Reports.Report
            { id = reportId
            , reportJson = rep_json
            , createdAt = currentTime
            , updatedAt = currentTime
            , projectId = pid
            , reportType = "weekly"
            }
    _ <- dbtToEff $ Reports.addReport report
    Relude.when pr.weeklyNotif $ forM_ pr.notificationsChannel \case
      Projects.NSlack ->
        sendSlackMessage
          pid
          [trimming| 🤖 *Weekly Report for `{pr.title}`*

<https://app.apitoolkit.io/p/{pid.toText}/reports/{show report.id.reportId}|View this week's report>
                     |]
      Projects.NDiscord -> do
        let projectUrl = "https://app.apitoolkit.io/p/" <> pid.toText <> "/reports/" <> show report.id.reportId
        discordData <- getDiscordDataByProjectId pid
        whenJust discordData \d -> do
          case d.notifsChannelId of
            Just channelId -> sendDiscordNotif channelId [fmtTrim|**WEEKLY REPORT**: [{pr.title}]({projectUrl})|]
            Nothing -> pass
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
                dayEnd = show $ localDay (zonedTimeToLocalTime currentTime)
                currentUTCTime = zonedTimeToUTC currentTime
                sevenDaysAgoUTCTime = addUTCTime (negate $ 6 * 86400) currentUTCTime
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

convertAnomaliesToIssues :: V.Vector Anomalies.AnomalyVM -> V.Vector Endpoints.Endpoint -> V.Vector Anomalies.Issue
convertAnomaliesToIssues ans ens = V.catMaybes $ (\e -> V.find (\a -> e.hash `T.isPrefixOf` a.targetHash) ans >>= Anomalies.convertAnomalyToIssue (Just e.host)) <$> ens


newAnomalyJob :: Projects.ProjectId -> ZonedTime -> Text -> Text -> V.Vector Text -> ATBackgroundCtx ()
newAnomalyJob pid createdAt anomalyTypesT anomalyActionsT targetHashes = do
  let anomalyType = fromMaybe (error "parseAnomalyTypes returned Nothing") $ Anomalies.parseAnomalyTypes anomalyTypesT
  case anomalyType of
    Anomalies.ATEndpoint -> do
      anomaliesVM <- dbtToEff $ Anomalies.getAnomaliesVM pid targetHashes
      endpoints <- dbtToEff $ Endpoints.endpointsByHashes pid targetHashes
      users <- dbtToEff $ Projects.usersByProjectId pid
      project <- Unsafe.fromJust <<$>> dbtToEff $ Projects.projectById pid
      let endpointsPaths = (\e -> e.method <> " " <> e.urlPath) <$> endpoints
          anIssues = convertAnomaliesToIssues anomaliesVM endpoints
          targetHash = fromMaybe "" $ viaNonEmpty head (V.toList targetHashes)
      _ <- dbtToEff $ Anomalies.insertIssues anIssues
      let alert = EndpointAlert project.title endpointsPaths targetHash
      forM_ project.notificationsChannel \case
        Projects.NSlack -> sendSlackAlert alert pid project.title
        Projects.NDiscord -> sendDiscordAlert alert pid project.title
        _ -> do
          forM_ users \u -> do
            let templateVars =
                  AE.object
                    [ "user_name" AE..= u.firstName
                    , "project_name" AE..= project.title
                    , "anomaly_url" AE..= ("https://app.apitoolkit.io/p/" <> pid.toText <> "/anomalies/by_hash/" <> targetHash)
                    , "endpoint_name" AE..= endpointsPaths
                    ]
            sendPostmarkEmail (CI.original u.email) (Just ("anomaly-endpoint-2", templateVars)) Nothing

      pass
    Anomalies.ATShape -> do
      anomaliesVM <- dbtToEff $ Anomalies.getAnomaliesVM pid targetHashes
      endpoints <- dbtToEff $ Endpoints.endpointsByHashes pid $ T.take 8 <$> targetHashes
      anomalies <- forM anomaliesVM \anomaly -> do
        let targetHash = anomaly.targetHash
            getShapesQuery = [sql| select hash, field_hashes from apis.shapes where project_id=? and endpoint_hash=? |]
        shapes <- dbtToEff $ query getShapesQuery (pid, T.take 8 targetHash)
        let targetFields = maybe [] (V.toList . snd) $ V.find (\a -> fst a == targetHash) shapes
        let otherFields = toList <$> toList (snd $ V.unzip $ V.filter (\a -> fst a /= targetHash) shapes)
        updatedFieldFormats <- dbtToEff $ getUpdatedFieldFormats pid (V.fromList targetFields)
        let newFields = filter (`notElem` foldl' L.union [] otherFields) targetFields
        let deletedFields = filter (`notElem` targetFields) $ foldl' L.intersect (head $ [] :| otherFields) (tail $ [] :| otherFields)
        _ <- dbtToEff $ updateShapeCounts pid targetHash (V.fromList newFields) (V.fromList deletedFields) updatedFieldFormats
        -- Send an email about the new shape anomaly but only if there was no endpoint anomaly logged
        -- users <- dbtToEff $ Projects.usersByProjectId pid
        project <- Unsafe.fromJust <<$>> dbtToEff $ Projects.projectById pid
        pure anomaly{Anomalies.anomalyType = anomalyType, Anomalies.shapeDeletedFields = V.fromList deletedFields, Anomalies.shapeUpdatedFieldFormats = updatedFieldFormats, Anomalies.shapeNewUniqueFields = V.fromList newFields}
      _ <- dbtToEff $ Anomalies.insertIssues $ convertAnomaliesToIssues anomalies endpoints
      pass
    Anomalies.ATFormat -> do
      -- pass
      -- -- Send an email about the new shape anomaly but only if there was no endpoint anomaly logged
      -- hasEndpointAnomaly <- dbtToEff $ Anomalies.getFormatParentAnomaliesVM pid targetHashes
      endpoints <- dbtToEff $ Endpoints.endpointsByHashes pid $ T.take 8 <$> targetHashes
      anomaliesVM <- dbtToEff $ Anomalies.getAnomaliesVM pid targetHashes
      project <- Unsafe.fromJust <<$>> dbtToEff $ Projects.projectById pid
      _ <- dbtToEff $ Anomalies.insertIssues $ convertAnomaliesToIssues anomaliesVM endpoints
      pass
    Anomalies.ATRuntimeException -> do
      users <- dbtToEff $ Projects.usersByProjectId pid
      project <- Unsafe.fromJust <<$>> dbtToEff $ Projects.projectById pid
      errs <- dbtToEff $ Anomalies.errorsByHashes pid targetHashes
      issueId <- liftIO $ Anomalies.AnomalyId <$> UUIDV4.nextRandom
      _ <-
        dbtToEff
          $ Anomalies.insertIssues
          $ ( \err ->
                Anomalies.Issue
                  { id = issueId
                  , createdAt = err.createdAt
                  , updatedAt = err.updatedAt
                  , projectId = pid
                  , anomalyType = Anomalies.ATRuntimeException
                  , targetHash = err.hash
                  , issueData = Anomalies.IDNewRuntimeExceptionIssue err.errorData
                  , acknowlegedAt = Nothing
                  , acknowlegedBy = Nothing
                  , endpointId = Nothing
                  , archivedAt = Nothing
                  }
            )
            <$> errs

      forM_ project.notificationsChannel \case
        Projects.NSlack ->
          forM_ errs \err -> do sendSlackAlert (RuntimeErrorAlert err.errorData) pid project.title
        Projects.NDiscord ->
          forM_ errs \err -> do sendDiscordAlert (RuntimeErrorAlert err.errorData) pid project.title
        _ ->
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
                    <$> errs
                title = project.title
                errors_url = "https://app.apitoolkit.io/p/" <> pid.toText <> "/anomalies/"
                templateVars =
                  [aesonQQ|{
                        "project_name": #{title},
                        "errors_url": #{errors_url},
                        "errors": #{errosJ}
                   }|]
            sendPostmarkEmail (CI.original u.email) (Just ("runtime-errors", templateVars)) Nothing
    Anomalies.ATField -> pass
    Anomalies.ATUnknown -> pass


getUpdatedFieldFormats :: Projects.ProjectId -> V.Vector Text -> PTR.DBT IO (V.Vector Text)
getUpdatedFieldFormats pid fieldHashes = query q (pid, fieldHashes)
  where
    q =
      [sql| select fm.hash from apis.formats fm JOIN apis.fields fd ON (fm.project_id=fd.project_id AND fd.hash=fm.field_hash) 
                where fm.project_id=? AND fm.created_at>(fd.created_at+interval '2 minutes') AND fm.field_hash=ANY(?) |]


updateShapeCounts :: Projects.ProjectId -> Text -> V.Vector Text -> V.Vector Text -> V.Vector Text -> PTR.DBT IO Int64
updateShapeCounts pid shapeHash newFields deletedFields updatedFields = execute q (newFields, deletedFields, updatedFields, pid, shapeHash)
  where
    q = [sql| update apis.shapes SET new_unique_fields=?, deleted_fields=?, updated_field_formats=? where project_id=? and hash=?|]
