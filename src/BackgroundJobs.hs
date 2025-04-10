module BackgroundJobs (jobsWorkerInit, jobsRunner, BgJobs (..)) where

import Control.Lens ((.~))
import Data.Aeson ((.=))
import Data.Aeson qualified as AE
import Data.Aeson.Encoding qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.CaseInsensitive qualified as CI
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (DayOfWeek (Monday), UTCTime (utctDay), ZonedTime, addUTCTime, dayOfWeek, formatTime, getZonedTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime), getCurrentTimeZone, utcToZonedTime, zonedTimeToUTC)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Log qualified
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Query qualified as FieldsQ
import Models.Apis.Fields.Types qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.Reports qualified as Reports
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.LemonSqueezy qualified as LemonSqueezy
import Models.Projects.Projects qualified as Projects
import Models.Projects.Swaggers qualified as Swaggers
import Models.Tests.TestToDump qualified as TestToDump
import Models.Tests.Testing qualified as Testing
import Models.Users.Users qualified as Users
import NeatInterpolation (trimming)
import Network.Wreq (defaults, header, postWith)
import OddJobs.ConfigBuilder (mkConfig)
import OddJobs.Job (ConcurrencyControl (..), Job (..), LogEvent, LogLevel, createJob, startJobRunner, throwParsePayload)
import Pages.Reports qualified as RP
import Pages.Specification.GenerateSwagger (generateSwagger)
import Pkg.Mail (sendDiscordNotif, sendPostmarkEmail, sendSlackMessage)
import PyF (fmtTrim)
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Config qualified as Config
import System.Types (ATBackgroundCtx, runBackground)


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
  | GenSwagger Projects.ProjectId Users.UserId Text
  | ReportUsage Projects.ProjectId
  | QueryMonitorsTriggered (V.Vector Monitors.QueryMonitorId)
  | RunCollectionTests Testing.CollectionId
  | DeletedProject Projects.ProjectId
  | APITestFailed Projects.ProjectId Testing.CollectionId (V.Vector Testing.StepResult)
  deriving stock (Show, Generic)
  deriving anyclass (AE.ToJSON, AE.FromJSON)


webhookUrl :: String
webhookUrl = "https://discord.com/api/webhooks/1230980245423788045/JQOJ7w3gmEduaOvPTnxEz4L8teDpX5PJoFkyQmqZHR8HtRqAkWIjv2Xk1aKadTyXuFy_"


sendMessageToDiscord :: Text -> ATBackgroundCtx ()
sendMessageToDiscord msg = do
  let message = AE.object ["content" AE..= msg]
  let opts = defaults & header "Content-Type" .~ ["application/json"]
  response <- liftIO $ postWith opts webhookUrl message
  pass


jobsRunner :: Log.Logger -> Config.AuthContext -> Job -> IO ()
jobsRunner logger authCtx job = when authCtx.config.enableBackgroundJobs $ do
  bgJob <- throwParsePayload job
  void $ runBackground logger authCtx do
    case bgJob of
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
        projects <- dbtToEff $ query Select [sql|SELECT id FROM projects.projects WHERE active=? AND deleted_at IS NULL and payment_plan != 'ONBOARDING'|] (Only True)
        forM_ projects \p -> do
          liftIO $ withResource authCtx.jobsPool \conn -> do
            _ <-
              if dayOfWeek currentDay == Monday
                then do
                  _ <- createJob conn "background_jobs" $ BackgroundJobs.WeeklyReports p
                  pass
                else pass
            _ <- createJob conn "background_jobs" $ BackgroundJobs.ReportUsage p
            pass
      DailyReports pid -> dailyReportForProject pid
      WeeklyReports pid -> weeklyReportForProject pid
      GenSwagger pid uid host -> generateSwaggerForProject pid uid host
      ReportUsage pid -> whenJustM (dbtToEff $ Projects.projectById pid) \project -> do
        when (project.paymentPlan /= "Free" && project.paymentPlan /= "ONBOARDING") $ whenJust project.firstSubItemId \fSubId -> do
          currentTime <- liftIO getZonedTime
          totalToReport <- dbtToEff $ RequestDumps.getTotalRequestToReport pid project.usageLastReported
          liftIO $ reportUsageToLemonsqueezy fSubId totalToReport authCtx.config.lemonSqueezyApiKey
          _ <- dbtToEff $ LemonSqueezy.addDailyUsageReport pid totalToReport
          _ <- dbtToEff $ Projects.updateUsageLastReported pid currentTime
          pass
      RunCollectionTests col_id -> do
        now <- Time.currentTime
        collectionM <- dbtToEff $ Testing.getCollectionById col_id
        if job.jobRunAt > addUTCTime (-900) now -- Run time is less than 15 mins ago
          then whenJust collectionM \collection -> when collection.isScheduled do
            let (Testing.CollectionSteps colStepsV) = collection.collectionSteps
                Testing.CollectionVariables colV = collection.collectionVariables
            stepResultsE <- TestToDump.runCollectionTest colStepsV colV col_id
            case stepResultsE of
              Left e -> do
                _ <- TestToDump.logTest collection.projectId col_id colStepsV (Left e)
                pass
              Right stepResults -> do
                let (_, failed) = Testing.getCollectionRunStatus stepResults
                when (failed > 0) do
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
          "data": {
            "type": "usage-records",
            "attributes": {
                "quantity": #{quantity},
                "action": "increment"
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
  when monitorE.alertConfig.emailAll do
    users <- dbtToEff $ Projects.usersByProjectId monitorE.projectId
    forM_ users \u -> emailQueryMonitorAlert monitorE u.email (Just u)
  forM_ monitorE.alertConfig.emails \email -> emailQueryMonitorAlert monitorE email Nothing
  unless (null monitorE.alertConfig.slackChannels) $ sendSlackMessage monitorE.projectId [fmtTrim| 🤖 *Log Alert triggered for `{monitorE.alertConfig.title}`*|]


-- way to get emails for company. for email all
-- TODO: based on monitor send emails or slack

jobsWorkerInit :: Log.Logger -> Config.AuthContext -> IO ()
jobsWorkerInit logger appCtx =
  startJobRunner $
    mkConfig jobLogger "background_jobs" appCtx.jobsPool (MaxConcurrentJobs 1) (jobsRunner logger appCtx) id
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
    when pr.dailyNotif $ forM_ pr.notificationsChannel \case
      Projects.NSlack ->
        sendSlackMessage
          pid
          [fmtTrim| 🤖 *Daily Report for `{pr.title}`**

<https://app.apitoolkit.io/p/{pid.toText}/reports/{show report.id.reportId}|View today's report>
|]
      Projects.NDiscord -> do
        let projectUrl = "https://app.apitoolkit.io/p/" <> pid.toText <> "/reports/" <> show report.id.reportId
        whenJust pr.discordUrl \url -> sendDiscordNotif url [fmtTrim|**Daily REPORT**: [{pr.title}]({projectUrl})|]
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
    when pr.weeklyNotif $ forM_ pr.notificationsChannel \case
      Projects.NSlack ->
        sendSlackMessage
          pid
          [trimming| 🤖 *Weekly Report for `{pr.title}`*

<https://app.apitoolkit.io/p/{pid.toText}/reports/{show report.id.reportId}|View this week's report>
                     |]
      Projects.NDiscord -> do
        let projectUrl = "https://app.apitoolkit.io/p/" <> pid.toText <> "/reports/" <> show report.id.reportId
        whenJust pr.discordUrl \url -> sendDiscordNotif url [fmtTrim|**WEEKLY REPORT**: [{pr.title}]({projectUrl})|]
      _ -> do
        totalRequest <- dbtToEff $ RequestDumps.getLastSevenDaysTotalRequest pid
        when (totalRequest > 0) do
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
convertAnomaliesToIssues ans ens = V.catMaybes $ (\e -> V.find (\a -> a.targetHash == e.hash) ans >>= (\a -> Anomalies.convertAnomalyToIssue (Just e.host) a)) <$> ens


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
          endpointLines = T.intercalate "\n" (V.toList endpointsPaths)
          anIssues = convertAnomaliesToIssues anomaliesVM endpoints
          targetHash = fromMaybe "" $ viaNonEmpty head (V.toList targetHashes)
      _ <- dbtToEff $ Anomalies.insertIssues anIssues
      forM_ project.notificationsChannel \case
        Projects.NSlack -> do
          sendSlackMessage
            pid
            [fmtTrim| {{...}} *New Endpoint(s) Detected for `{project.title}`*

We have detected a new endpoint on *{project.title}*

**Endpoints:**
`{endpointLines}`

<https://app.apitoolkit.io/p/{pid.toText}/anomalies/by_hash/{targetHash}|More details on APItoolkit>
                              |]
        Projects.NDiscord -> do
          let msg =
                [fmtTrim|
{{···}} **New Endpoint(s) Detected For {project.title}**

**Endpoints**: 
`{endpointLines}`
[View more](https://app.apitoolkit.io/p/{pid.toText}/anomalies/by_hash/{targetHash})|]
          whenJust project.discordUrl (`sendDiscordNotif` msg)
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
      pass
    -- hasEndpointAnomaly <- dbtToEff $ Anomalies.getShapeParentAnomalyVM pid targetHash
    -- when (hasEndpointAnomaly == 0) $ whenJustM (dbtToEff $ Anomalies.getAnomalyVM pid targetHash) \anomaly -> do
    --   endp <- dbtToEff $ Endpoints.endpointByHash pid $ T.take 8 targetHash
    --   let getShapesQuery = [sql| select hash, field_hashes from apis.shapes where project_id=? and endpoint_hash=? |]
    --   shapes <- (dbtToEff $ query Select getShapesQuery (pid, T.take 8 targetHash))
    --   let targetFields = maybe [] (V.toList . snd) $ V.find (\a -> fst a == targetHash) shapes
    --   let otherFields = toList <$> toList (snd $ V.unzip $ V.filter (\a -> fst a /= targetHash) shapes)
    --   updatedFieldFormats <- dbtToEff $ getUpdatedFieldFormats pid (V.fromList targetFields)

    --   let newFields = filter (`notElem` foldl' union [] otherFields) targetFields
    --   let deletedFields = filter (`notElem` targetFields) $ foldl' intersect (head $ [] :| otherFields) (tail $ [] :| otherFields)
    --   _ <- dbtToEff $ updateShapeCounts pid targetHash (V.fromList newFields) (V.fromList deletedFields) updatedFieldFormats
    --   -- Send an email about the new shape anomaly but only if there was no endpoint anomaly logged
    --   users <- dbtToEff $ Projects.usersByProjectId pid
    --   project <- Unsafe.fromJust <<$>> dbtToEff $ Projects.projectById pid
    --   let anomaly' = anomaly{Anomalies.anomalyType = anomalyType, Anomalies.shapeDeletedFields = V.fromList deletedFields, Anomalies.shapeUpdatedFieldFormats = updatedFieldFormats, Anomalies.shapeNewUniqueFields = V.fromList newFields}
    --   _ <- dbtToEff $ Anomalies.insertIssue $ Unsafe.fromJust $ Anomalies.convertAnomalyToIssue (endp <&> (.host)) anomaly'
    --   pass
    -- forM_ project.notificationsChannel \case
    --   Projects.NSlack ->
    --     sendSlackMessage
    --       pid
    --       [fmtTrim| 🤖 *New Shape anomaly found for `{project.title}`*
    --                  We detected a different API request shape to your endpoints than what you usually have

    --                  <https://app.apitoolkit.io/p/{pid.toText}/anomalies/by_hash/{targetHash}|More details on the apitoolkit>
    --                       |]
    --   _ -> do
    --     forM_ users \u -> do
    --       let templateVars =
    --             object
    --               [ "user_name" .= u.firstName
    --               , "project_name" .= project.title
    --               , "anomaly_url" .= ("https://app.apitoolkit.io/p/" <> pid.toText <> "/anomalies/by_hash/" <> targetHash)
    --               ]
    --       sendPostmarkEmail (CI.original u.email) "anomaly-shape" templateVars
    Anomalies.ATFormat -> do
      pass
    -- -- Send an email about the new shape anomaly but only if there was no endpoint anomaly logged
    -- hasEndpointAnomaly <- dbtToEff $ Anomalies.getFormatParentAnomalyVM pid targetHash
    -- when (hasEndpointAnomaly == 0) $ whenJustM (dbtToEff $ Anomalies.getAnomalyVM pid targetHash) \anomaly -> do
    --   endp <- dbtToEff $ Endpoints.endpointByHash pid $ T.take 8 targetHash
    --   users <- dbtToEff $ Projects.usersByProjectId pid
    --   project <- Unsafe.fromJust <<$>> dbtToEff $ Projects.projectById pid
    --   _ <- dbtToEff $ Anomalies.insertIssue $ Unsafe.fromJust $ Anomalies.convertAnomalyToIssue (endp <&> (.host)) anomaly
    --   pass
    -- forM_ project.notificationsChannel \case
    --   Projects.NSlack ->
    --     sendSlackMessage
    --       pid
    --       [fmtTrim| 🤖 *New Field Format Anomaly Found for `{project.title}`*

    --                      We detected that a particular field on your API is returning a different format/type than what it usually gets.

    --                      <https://app.apitoolkit.io/p/{pid.toText}/anomalies/by_hash/{targetHash}|More details on the apitoolkit>
    --                        |]
    --   _ -> forM_ users \u -> do
    --     let firstName = u.firstName
    --     let title = project.title
    --     let anomaly_url = "https://app.apitoolkit.io/p/" <> pid.toText <> "/anomalies/by_hash/" <> targetHash
    --     let templateVars =
    --           [aesonQQ|{
    --               "user_name": #{firstName},
    --               "project_name": #{title},
    --               "anomaly_url": #{anomaly_url}
    --          }|]
    --     sendPostmarkEmail (CI.original u.email) "anomaly-field" templateVars
    Anomalies.ATRuntimeException -> do
      let targetHash = fromMaybe "" $ viaNonEmpty head (V.toList targetHashes)
      users <- dbtToEff $ Projects.usersByProjectId pid
      project <- Unsafe.fromJust <<$>> dbtToEff $ Projects.projectById pid
      errs <- dbtToEff $ Anomalies.errorsByHashes pid targetHashes
      issueId <- liftIO $ Anomalies.AnomalyId <$> UUIDV4.nextRandom
      _ <-
        dbtToEff $
          Anomalies.insertIssues $
            ( \err ->
                Anomalies.Issue
                  { id = issueId
                  , createdAt = err.createdAt
                  , updatedAt = err.updatedAt
                  , projectId = pid
                  , anomalyType = Anomalies.ATRuntimeException
                  , targetHash = targetHash
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
          sendSlackMessage
            pid
            [fmtTrim| 🤖 *New Runtime Exception(s) Found for `{project.title}`*

A new runtime exception has been detected. click the link below to see more details.

<https://app.apitoolkit.io/p/{pid.toText}/anomalies/by_hash/{targetHash}|More details on the apitoolkit>
                               |]
        Projects.NDiscord -> do
          let msg =
                [fmtTrim|
{{···}} **New Runtime Exception Found for {project.title}**
A new runtime exception has been detected. click the link below to see more details.

[View more](https://app.apitoolkit.io/p/{pid.toText}/anomalies/by_hash/{targetHash})|]

          whenJust project.discordUrl (`sendDiscordNotif` msg)
        _ ->
          forM_ users \u -> do
            let errosJ =
                  ( \ee ->
                      let e = ee.errorData
                       in AE.object
                            [ "root_error_message" .= e.rootErrorMessage
                            , "error_type" .= e.errorType
                            , "error_message" .= e.message
                            , "stack_trace" .= e.stackTrace
                            , "when" .= formatTime defaultTimeLocale "%b %-e, %Y, %-l:%M:%S %p" e.when
                            , "hash" .= e.hash
                            , "tech" .= e.technology
                            , "request_info" .= ((fromMaybe "" e.requestMethod) <> " " <> (fromMaybe "" e.requestPath))
                            , "root_error_type" .= e.rootErrorType
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
