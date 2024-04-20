{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module BackgroundJobs (jobsWorkerInit, jobsRunner, BgJobs (..)) where

import Control.Lens ((.~))
import Data.Aeson as Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.CaseInsensitive qualified as CI
import Data.List.Extra (intersect, union)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (DayOfWeek (Monday), UTCTime (utctDay), ZonedTime, dayOfWeek, getCurrentTime, getZonedTime)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (Select, Update), execute, query)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Log qualified
import Lucid (renderText)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Query qualified as FieldsQ
import Models.Apis.Fields.Types qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.Reports qualified as Reports
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Projects.Swaggers qualified as Swaggers
import Models.Tests.Testing qualified as Testing
import Models.Users.Users qualified as Users
import NeatInterpolation (text, trimming)
import Network.Wreq (defaults, header, postWith)
import OddJobs.ConfigBuilder (mkConfig)
import OddJobs.Job (ConcurrencyControl (..), Job (..), LogEvent, LogLevel, createJob, startJobRunner, throwParsePayload)
import Pages.GenerateSwagger (generateSwagger)
import Pages.Reports qualified as RP
import Pkg.Mail (sendEmail, sendSlackMessage)
import PyF (fmt, fmtTrim)
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Config qualified as Config
import System.Types (ATBackgroundCtx, runBackground)
import Utils (scheduleIntervals)


data BgJobs
  = InviteUserToProject Users.UserId Projects.ProjectId Text Text
  | CreatedProjectSuccessfully Users.UserId Projects.ProjectId Text Text
  | -- NewAnomaly Projects.ProjectId Anomalies.AnomalyTypes Anomalies.AnomalyActions TargetHash
    NewAnomaly Projects.ProjectId ZonedTime Text Text Text
  | DailyReports Projects.ProjectId
  | WeeklyReports Projects.ProjectId
  | DailyJob
  | GenSwagger Projects.ProjectId Users.UserId
  | ReportUsage Projects.ProjectId
  | QueryMonitorsTriggered (Vector Monitors.QueryMonitorId)
  | ScheduleForCollection Testing.CollectionId
  | RunCollectionTests Testing.CollectionId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


getUpdatedFieldFormats :: Projects.ProjectId -> Vector Text -> DBT IO (Vector Text)
getUpdatedFieldFormats pid fieldHashes = query Select q (pid, fieldHashes)
  where
    q =
      [sql| select fm.hash from apis.formats fm JOIN apis.fields fd ON (fm.project_id=fd.project_id AND fd.hash=fm.field_hash) 
                where fm.project_id=? AND fm.created_at>(fd.created_at+interval '2 minutes') AND fm.field_hash=ANY(?) |]


updateShapeCounts :: Projects.ProjectId -> Text -> Vector Text -> Vector Text -> Vector Text -> DBT IO Int64
updateShapeCounts pid shapeHash newFields deletedFields updatedFields = execute Update q (newFields, deletedFields, updatedFields, pid, shapeHash)
  where
    q = [sql| update apis.shapes SET new_unique_fields=?, deleted_fields=?, updated_field_formats=? where project_id=? and hash=?|]


-- TODO:
-- Analyze shapes for
-- 1: [x] how many fields in the current shape are completely new?
-- 2: [ ] how many were updated fields?
-- 3. [x] how many were deleted params
-- Send a notification email about the new anomaly (shape and endpoint etc)
--

jobsRunner :: Log.Logger -> Config.AuthContext -> Job -> IO ()
jobsRunner logger authCtx job = when authCtx.config.enableBackgroundJobs $ do
  bgJob <- throwParsePayload job
  runBackground logger authCtx $ case bgJob of
    QueryMonitorsTriggered queryMonitorIds -> queryMonitorsTriggered queryMonitorIds
    NewAnomaly pid createdAt anomalyTypesT anomalyActionsT targetHash -> newAnomalyJob pid createdAt anomalyTypesT anomalyActionsT targetHash
    InviteUserToProject userId projectId reciever projectTitle' ->
      sendEmail
        reciever
        [fmt| 🤖 APITOOLKIT: You've been invited to a project '{projectTitle'}' on apitoolkit.io |]
        [fmtTrim|
  Hi,<br/>

  <p>You have been invited to the $projectTitle project on apitoolkit. 
  Please use the following link to activate your account and access the $projectTitle project.</p>
  <a href="https://app.apitoolkit.io/p/{projectId.toText}">Click Here</a>
  <br/><br/>
  Regards,
  Apitoolkit team
            |]
    CreatedProjectSuccessfully userId projectId reciever projectTitle ->
      sendEmail
        reciever
        [fmt| 🤖 APITOOLKIT: Project created successfully '{projectTitle}' on apitoolkit.io |]
        [fmtTrim|
  Hi,<br/>

  <p>You have been invited to the $projectTitle project on apitoolkit. 
  Please use the following link to activate your account and access the {projectTitle} project.</p>
  <a href="app.apitoolkit.io/p/{projectId.toText}">Click Here to access the project</a><br/><br/>.

  By the way, we know it can be difficult or confusing to integrate SDKs sometimes. So we're willing to assist. You can schedule a time here, and we can help with integrating: 
  <a href="https://calendar.google.com/calendar/u/0/appointments/schedules/AcZssZ21Q1uDPjHN4YPpM2lNBS0_Nwc16IQj-25e5WIoPOKEVsBBIWJgy3usCUS4d7MtQz7kiuzyBJLb">Click Here to Schedule</a>
  <br/><br/>
  Regards,<br/>
  Apitoolkit team
            |]
    DailyJob -> do
      currentDay <- utctDay <$> Time.currentTime
      projects <- dbtToEff $ query Select [sql|SELECT id FROM projects.projects WHERE active=? AND deleted_at IS NULL|] (Only True)
      forM_ projects \p -> do
        liftIO $ withResource authCtx.jobsPool \conn -> do
          _ <- createJob conn "background_jobs" $ BackgroundJobs.ReportUsage p
          if dayOfWeek currentDay == Monday
            then createJob conn "background_jobs" $ BackgroundJobs.WeeklyReports p
            else createJob conn "background_jobs" $ BackgroundJobs.DailyReports p
      collections <- dbtToEff Testing.getCollectionsId
      forM_ collections \col -> liftIO $ withResource authCtx.jobsPool \conn -> createJob conn "background_jobs" $ BackgroundJobs.ScheduleForCollection col
    DailyReports pid -> dailyReportForProject pid
    WeeklyReports pid -> weeklyReportForProject pid
    GenSwagger pid uid -> generateSwaggerForProject pid uid
    ReportUsage pid -> whenJustM (dbtToEff $ Projects.projectById pid) \project -> do
      when (project.paymentPlan == "UsageBased") $ whenJust project.firstSubItemId \fSubId -> do
        totalRequestForThisMonth <- dbtToEff $ RequestDumps.getTotalRequestForCurrentMonth pid
        liftIO $ reportUsageToLemonsqueezy fSubId totalRequestForThisMonth authCtx.config.lemonSqueezyApiKey
    ScheduleForCollection col_id -> do
      whenJustM (dbtToEff $ Testing.getCollectionById col_id) \collection -> whenJust (collection.schedule) \schedule -> do
        currentTime <- liftIO getCurrentTime
        let intervals = scheduleIntervals currentTime schedule
        let dbParams = (\x -> (x, "queued" :: Text, Aeson.object ["tag" .= Aeson.String "RunCollectionTests", "contents" .= show col_id.collectionId])) <$> intervals
        void $ dbtToEff $ Testing.scheduleInsertScheduleInBackgroundJobs dbParams
    RunCollectionTests col_id -> do
      (collectionM, steps) <- dbtToEff $ do
        colM <- Testing.getCollectionById col_id
        steps <- Testing.getCollectionSteps col_id
        pure (colM, steps)
      whenJust collectionM \collection -> do
        -- let steps_data = (\x -> x.stepData) <$> steps
        -- let col_json = (decodeUtf8 $ Aeson.encode steps_data :: String)
        pass


generateSwaggerForProject :: Projects.ProjectId -> Users.UserId -> ATBackgroundCtx ()
generateSwaggerForProject pid uid = whenJustM (dbtToEff $ Projects.projectById pid) \project -> do
  endpoints <- dbtToEff $ Endpoints.endpointsByProjectId pid
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
          }
  dbtToEff $ Swaggers.addSwagger swaggerToAdd


reportUsageToLemonsqueezy :: Text -> Int -> Text -> IO ()
reportUsageToLemonsqueezy subItemId quantity apiKey = do
  let formData =
        [aesonQQ|{
          "data": {
            "type": "usage-records",
            "attributes": {
                "quantity": #{quantity},
                "action": "set"
            },
            "relationships": {
               "subscription-item": {
                  "data": {
                    "type": "subscription-items",
                    "id": #{subItemId}
                  }
                }
             }}} |]
  let hds =
        defaults
          & (header "Authorization" .~ ["Bearer " <> encodeUtf8 @Text @ByteString apiKey])
          & (header "Content-Type" .~ ["application/vnd.api+json"])
  _ <- postWith hds "https://api.lemonsqueezy.com/v1/usage-records" formData
  pass


queryMonitorsTriggered :: Vector Monitors.QueryMonitorId -> ATBackgroundCtx ()
queryMonitorsTriggered queryMonitorIds = do
  monitorsEvaled <- dbtToEff $ Monitors.queryMonitorsById queryMonitorIds
  forM_ monitorsEvaled \monitorE ->
    if (monitorE.triggerLessThan && monitorE.evalResult >= monitorE.alertThreshold)
      || (not monitorE.triggerLessThan && monitorE.evalResult <= monitorE.alertThreshold)
      then handleQueryMonitorThreshold monitorE True
      else do
        if ( Just True
              == ( monitorE.warningThreshold <&> \warningThreshold ->
                    (monitorE.triggerLessThan && monitorE.evalResult >= warningThreshold)
                      || (not monitorE.triggerLessThan && monitorE.evalResult <= warningThreshold)
                 )
           )
          then handleQueryMonitorThreshold monitorE False
          else pass


handleQueryMonitorThreshold :: Monitors.QueryMonitorEvaled -> Bool -> ATBackgroundCtx ()
handleQueryMonitorThreshold monitorE isAlert = do
  Log.logAttention "Query Monitors Triggered " monitorE
  _ <- dbtToEff $ Monitors.updateQMonitorTriggeredState monitorE.id isAlert
  when monitorE.alertConfig.emailAll do
    users <- dbtToEff $ Projects.usersByProjectId monitorE.projectId
    forM_ users \u -> emailQueryMonitorAlert monitorE u.email (Just u)
  forM_ monitorE.alertConfig.emails \email -> emailQueryMonitorAlert monitorE email Nothing
  unless (null monitorE.alertConfig.slackChannels) $ sendSlackMessage monitorE.projectId [fmtTrim| 🤖 *Log Alert triggered for `{monitorE.alertConfig.title}`*|]


-- way to get emails for company. for email all
-- TODO: based on monitor send emails or slack

jobsWorkerInit :: Log.Logger -> Config.AuthContext -> IO ()
jobsWorkerInit logger appCtx =
  startJobRunner
    $ mkConfig jobLogger "background_jobs" (appCtx.jobsPool) (MaxConcurrentJobs 1) (jobsRunner logger appCtx) id
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
    when pr.dailyNotif $ forM_ pr.notificationsChannel \case
      Projects.NSlack ->
        sendSlackMessage
          pid
          ( [fmtTrim| 🤖 *Daily Report for `{pr.title}`*
          
                        <https://app.apitoolkit.io/p/{pid.toText}/reports/{show report.id.reportId}|View today's report>
                           |]
              :: Text
          )
      _ -> users & mapM_ \user -> sendEmail (CI.original user.email) [fmt| APITOOLKIT: Daily Report for {pr.title} |] (renderText $ RP.reportEmail pid report)


weeklyReportForProject :: Projects.ProjectId -> ATBackgroundCtx ()
weeklyReportForProject pid = do
  ctx <- ask @Config.AuthContext
  users <- dbtToEff $ Projects.usersByProjectId pid
  projectM <- dbtToEff $ Projects.projectById pid
  forM_ projectM \pr -> do
    anomalies <- dbtToEff $ Anomalies.getReportAnomalies pid "weekly"
    endpoint_rp <- dbtToEff $ RequestDumps.getRequestDumpForReports pid "weekly"
    previous_week <- dbtToEff $ RequestDumps.getRequestDumpsForPreviousReportPeriod pid "weekly"
    let rep_json = RP.buildReportJSON anomalies endpoint_rp previous_week
    currentTime <- liftIO getZonedTime
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
    when pr.weeklyNotif $ forM_ pr.notificationsChannel \case
      Projects.NSlack ->
        sendSlackMessage
          pid
          [trimming| 🤖 *Weekly Report for `{pr.title}`***
    
                              <https://app.apitoolkit.io/p/{pid.toText}/reports/{show report.id.reportId}|View this week's report>
                     |]
      _ -> forM_ users \user -> sendEmail (CI.original user.email) [text| APITOOLKIT: Weekly Report for `{pr.title}` |] $ renderText $ RP.reportEmail pid report


emailQueryMonitorAlert :: Monitors.QueryMonitorEvaled -> CI.CI Text -> Maybe Users.User -> ATBackgroundCtx ()
emailQueryMonitorAlert monitorE@Monitors.QueryMonitorEvaled{alertConfig} email userM = whenJust userM \user ->
  sendEmail
    (CI.original email)
    [fmt| 🤖 APITOOLKIT: log monitor triggered `{alertConfig.title}` |]
    [fmtTrim|
      Hi {user.firstName},<br/>
      
      The monitor: `{alertConfig.title}` was triggered and got above it's defined threshold.
      
      <br/><br/>
      Regards,
      Apitoolkit team
                |]


newAnomalyJob :: Projects.ProjectId -> ZonedTime -> Text -> Text -> Text -> ATBackgroundCtx ()
newAnomalyJob pid createdAt anomalyTypesT anomalyActionsT targetHash = do
  let anomalyType = Unsafe.fromJust $ Anomalies.parseAnomalyTypes anomalyTypesT
  -- let anomalyAction = Unsafe.fromJust $ Anomalies.parseAnomalyActions anomalyActionsT
  case anomalyType of
    Anomalies.ATEndpoint -> do
      endp <- dbtToEff $ Endpoints.endpointByHash pid targetHash
      users <- dbtToEff $ Projects.usersByProjectId pid
      project <- Unsafe.fromJust <<$>> dbtToEff $ Projects.projectById pid
      let enp = Unsafe.fromJust endp
      let endpointPath = enp.method <> " " <> enp.urlPath
      forM_ project.notificationsChannel \case
        Projects.NSlack ->
          sendSlackMessage
            pid
            [fmtTrim| 🤖 *New Endpoint Detected for `{project.title}`*

                           We have detected a new endpoint on *{project.title}*

                           Endpoint: `{endpointPath}`

                           <https://app.apitoolkit.io/p/{pid.toText}/anomalies/by_hash/{targetHash}|More details on the apitoolkit>
                            |]
        _ -> do
          forM_ users \u ->
            sendEmail
              (CI.original u.email)
              [fmt| 🤖 APITOOLKIT: New Endpoint detected for `{project.title}` |]
              [fmtTrim|
                     Hi {u.firstName},<br/>
         
                     <p>We detected a new endpoint on `{project.title}`:</p>
                     <p><strong>{endpointPath}</strong></p>
                     <a href="https://app.apitoolkit.io/p/{pid.toText}/anomalies/by_hash/{targetHash}">More details on the apitoolkit</a>
                     <br/><br/>
                     Regards,
                     Apitoolkit team
                               |]
    Anomalies.ATShape -> do
      hasEndpointAnomaly <- dbtToEff $ Anomalies.getShapeParentAnomalyVM pid targetHash
      when (hasEndpointAnomaly == 0) do
        let getShapesQuery = [sql| select hash, field_hashes from apis.shapes where project_id=? and endpoint_hash=? |]
        shapes <- (dbtToEff $ query Select getShapesQuery (pid, T.take 8 targetHash))
        let targetFields = maybe [] (V.toList . snd) (V.find (\a -> fst a == targetHash) shapes)
        updatedFieldFormats <- dbtToEff $ getUpdatedFieldFormats pid (V.fromList targetFields)
        let otherFields = toList <$> toList (snd $ V.unzip $ V.filter (\a -> fst a /= targetHash) shapes)
        let newFields = filter (`notElem` foldl' union [] otherFields) targetFields
        let deletedFields = filter (`notElem` targetFields) $ foldl' intersect (head $ [] :| otherFields) (tail $ [] :| otherFields)
        -- Update the shape values in the database
        _ <- dbtToEff $ updateShapeCounts pid targetHash (V.fromList newFields) (V.fromList deletedFields) updatedFieldFormats
        -- Send an email about the new shape anomaly but only if there was no endpoint anomaly logged
        whenJustM (dbtToEff $ Anomalies.getAnomalyVM pid $ T.take 8 targetHash) \anomaly -> do
          users <- dbtToEff $ Projects.usersByProjectId pid
          project <- Unsafe.fromJust <<$>> dbtToEff $ Projects.projectById pid
          forM_ project.notificationsChannel \case
            Projects.NSlack ->
              sendSlackMessage
                pid
                [fmtTrim| 🤖 *New Shape anomaly found for `{project.title}`******
    
                                          We detected a different API request shape to your endpoints than what you usually have
    
                                          <https://app.apitoolkit.io/p/{pid.toText}/anomalies/by_hash/{targetHash}|More details on the apitoolkit>
                                 |]
            _ -> do
              forM_ users \u ->
                sendEmail
                  (CI.original u.email)
                  [fmt| 🤖 APITOOLKIT: New Shape anomaly found for `{project.title}` |]
                  [fmtTrim|
         Hi {u.firstName},<br/>
       
         <p>We detected a different API request shape to your endpoints than what you usually have..</p>
         <a href="https://app.apitoolkit.io/p/{pid.toText}/anomalies/by_hash/{targetHash}">More details on the apitoolkit</a>
         <br/><br/>
         Regards,<br/>
         Apitoolkit team
                                 |]
    Anomalies.ATFormat -> do
      -- Send an email about the new shape anomaly but only if there was no endpoint anomaly logged
      hasEndpointAnomaly <- dbtToEff $ Anomalies.getFormatParentAnomalyVM pid targetHash
      when (hasEndpointAnomaly == 0) $ whenJustM (dbtToEff $ Anomalies.getAnomalyVM pid targetHash) \anomaly -> do
        users <- dbtToEff $ Projects.usersByProjectId pid
        project <- Unsafe.fromJust <<$>> dbtToEff $ Projects.projectById pid
        forM_ project.notificationsChannel \case
          Projects.NSlack ->
            sendSlackMessage
              pid
              [fmtTrim| 🤖 *New Field Format Anomaly Found for `{project.title}`****
  
                                       We detected that a particular field on your API is returning a different format/type than what it usually gets.
  
                                       <https://app.apitoolkit.io/p/{pid.toText}/anomalies/by_hash/{targetHash}|More details on the apitoolkit>
                               |]
          _ -> forM_ users \u ->
            sendEmail
              (CI.original u.email)
              [fmt| 🤖 APITOOLKIT: New field format anomaly found for `{project.title}` |]
              [fmtTrim|
     Hi {u.firstName},<br/>
   
     <p>We detected that a particular field on your API is returning a different format/type than what it usually gets.</p>
     <a href="https://app.apitoolkit.io/p/{pid.toText}/anomalies/by_hash/{targetHash}">More details on the apitoolkit</a>
     <br/><br/>
     Regards,<br/>
     Apitoolkit team
                           |]
    Anomalies.ATField -> pass
    Anomalies.ATUnknown -> pass
