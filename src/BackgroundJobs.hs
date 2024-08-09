{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module BackgroundJobs (jobsWorkerInit, jobsRunner, BgJobs (..)) where

import Control.Lens ((.~))
import Data.Aeson as Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.CaseInsensitive qualified as CI
import Data.List.Extra (intersect, union)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (DayOfWeek (Monday), UTCTime (utctDay), ZonedTime, addUTCTime, dayOfWeek, getZonedTime)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime), getCurrentTimeZone, utcToZonedTime, zonedTimeToUTC)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (Select, Update), execute, query)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Debug.Pretty.Simple
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
    NewAnomaly Projects.ProjectId ZonedTime Text Text Text
  | DailyReports Projects.ProjectId
  | WeeklyReports Projects.ProjectId
  | DailyJob
  | GenSwagger Projects.ProjectId Users.UserId
  | ReportUsage Projects.ProjectId
  | QueryMonitorsTriggered (Vector Monitors.QueryMonitorId)
  | RunCollectionTests Testing.CollectionId
  | DeletedProject Projects.ProjectId
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


webhookUrl :: String
webhookUrl = "https://discord.com/api/webhooks/1230980245423788045/JQOJ7w3gmEduaOvPTnxEz4L8teDpX5PJoFkyQmqZHR8HtRqAkWIjv2Xk1aKadTyXuFy_"


sendMessageToDiscord :: Text -> ATBackgroundCtx ()
sendMessageToDiscord msg = do
  let message = object ["content" .= msg]
  let opts = defaults & header "Content-Type" .~ ["application/json"]
  response <- liftIO $ postWith opts webhookUrl message
  pass


jobsRunner :: Log.Logger -> Config.AuthContext -> Job -> IO ()
jobsRunner logger authCtx job = when authCtx.config.enableBackgroundJobs $ do
  bgJob <- throwParsePayload job
  void $ runBackground logger authCtx do
    case bgJob of
      QueryMonitorsTriggered queryMonitorIds -> queryMonitorsTriggered queryMonitorIds
      NewAnomaly pid createdAt anomalyTypesT anomalyActionsT targetHash -> newAnomalyJob pid createdAt anomalyTypesT anomalyActionsT targetHash
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
          sendPostmarkEmail reciever "project-invite" templateVars
      SendDiscordData userId projectId fullName stack foundUsFrom -> whenJustM (dbtToEff $ Projects.projectById projectId) \project -> do
        users <- dbtToEff $ Projects.usersByProjectId projectId
        let stackString = intercalate ", " $ map T.unpack stack
        forM_ users \user -> do
          let userEmail = CI.original (user.email)
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
          sendPostmarkEmail reciever "project-created" templateVars
      DeletedProject pid -> do
        users <- dbtToEff $ Projects.usersByProjectId pid
        projectM <- dbtToEff $ Projects.projectById pid
        forM_ projectM \pr -> do
          forM_ users \user -> do
            let firstName = user.firstName
            let projectTitle = pr.title
            let userEmail = CI.original (user.email)
            let templateVars =
                  [aesonQQ|{
             "user_name": #{firstName},
             "project_name": #{projectTitle}
           }|]
            sendPostmarkEmail userEmail "project-deleted" templateVars
      DailyJob -> do
        currentDay <- utctDay <$> Time.currentTime
        projects <- dbtToEff $ query Select [sql|SELECT id FROM projects.projects WHERE active=? AND deleted_at IS NULL|] (Only True)
        forM_ projects \p -> do
          liftIO $ withResource authCtx.jobsPool \conn -> do
            _ <-
              if dayOfWeek currentDay == Monday
                then createJob conn "background_jobs" $ BackgroundJobs.WeeklyReports p
                else createJob conn "background_jobs" $ BackgroundJobs.DailyReports p
            _ <- createJob conn "background_jobs" $ BackgroundJobs.ReportUsage p
            pass
      DailyReports pid -> dailyReportForProject pid
      WeeklyReports pid -> weeklyReportForProject pid
      GenSwagger pid uid -> generateSwaggerForProject pid uid
      ReportUsage pid -> whenJustM (dbtToEff $ Projects.projectById pid) \project -> do
        when (project.paymentPlan == "UsageBased" || project.paymentPlan == "GraduatedPricing") $ whenJust project.firstSubItemId \fSubId -> do
          currentTime <- liftIO getZonedTime
          totalToReport <- dbtToEff $ RequestDumps.getTotalRequestToReport pid project.usageLastReported
          liftIO $ reportUsageToLemonsqueezy fSubId totalToReport authCtx.config.lemonSqueezyApiKey
          _ <- dbtToEff $ Projects.updateUsageLastReported pid currentTime
          pass
      RunCollectionTests col_id -> do
        now <- Time.currentTime
        collectionM <- dbtToEff $ Testing.getCollectionById col_id
        if job.jobRunAt > addUTCTime (-900) now -- Run time is less than 15 mins ago
          then whenJust collectionM \collection -> when (collection.isScheduled) do
            let (Testing.CollectionSteps colStepsV) = collection.collectionSteps
            _ <- TestToDump.runTestAndLog collection.projectId col_id colStepsV
            pass
          else Log.logAttention "RunCollectionTests failed.  Job was sheduled to run over 30 mins ago" $ collectionM <&> \c -> (c.title, c.id)


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
  startJobRunner $
    mkConfig jobLogger "background_jobs" (appCtx.jobsPool) (MaxConcurrentJobs 1) (jobsRunner logger appCtx) id
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
          let projectTitle = pr.title
          let userEmail = CI.original (user.email)
          let anmls = if total_anomalies == 0 then [Aeson.object ["message" .= "No anomalies detected yet."]] else RP.getAnomaliesEmailTemplate anomalies
          let perf = RP.getPerformanceEmailTemplate endpoint_rp previous_day
          let perf_count = V.length perf
          let perf_shrt = if perf_count == 0 then [Aeson.object ["message" .= "No performance data yet."]] else V.take 10 perf

          let rp_url = "https://app.apitoolkit.io/p/" <> pid.toText <> "/reports/" <> show report.id.reportId
          let day = show $ localDay (zonedTimeToLocalTime currentTime)
          let templateVars =
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
          sendPostmarkEmail userEmail "daily-report" templateVars


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
        forM_ users \user -> do
          let firstName = user.firstName
          let projectTitle = pr.title
          let userEmail = CI.original (user.email)
          let anmls = if total_anomalies == 0 then [Aeson.object ["message" .= "No anomalies detected yet."]] else RP.getAnomaliesEmailTemplate anomalies
          let perf = RP.getPerformanceEmailTemplate endpoint_rp previous_week
          let perf_count = V.length perf
          let perf_shrt = if perf_count == 0 then [Aeson.object ["message" .= "No performance data yet."]] else V.take 10 perf
          let rp_url = "https://app.apitoolkit.io/p/" <> pid.toText <> "/reports/" <> show report.id.reportId
          let dayEnd = show $ localDay (zonedTimeToLocalTime currentTime)
          let currentUTCTime = zonedTimeToUTC currentTime
          let sevenDaysAgoUTCTime = addUTCTime (negate $ 6 * 86400) currentUTCTime
          let sevenDaysAgoZonedTime = utcToZonedTime timeZone sevenDaysAgoUTCTime
          let dayStart = show $ localDay (zonedTimeToLocalTime sevenDaysAgoZonedTime)

          let templateVars =
                [aesonQQ|{
                 "user_name": #{firstName},
                 "project_name": #{projectTitle},
                 "anomalies_count": #{total_anomalies},
                 "anomalies":  #{anmls},
                 "report_url": #{rp_url},
                 "performance_count": #{perf_count},
                 "performance": #{perf_shrt},
                 "start_date": #{dayStart},
                 "end_date": #{dayEnd}
          }|]
          sendPostmarkEmail userEmail "weekly-report" templateVars


emailQueryMonitorAlert :: Monitors.QueryMonitorEvaled -> CI.CI Text -> Maybe Users.User -> ATBackgroundCtx ()
emailQueryMonitorAlert monitorE@Monitors.QueryMonitorEvaled{alertConfig} email userM = whenJust userM \user ->
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
  pass


newAnomalyJob :: Projects.ProjectId -> ZonedTime -> Text -> Text -> Text -> ATBackgroundCtx ()
newAnomalyJob pid createdAt anomalyTypesT anomalyActionsT targetHash = do
  let anomalyType = fromMaybe (error "parseAnomalyTypes returned Nothing") $ Anomalies.parseAnomalyTypes anomalyTypesT
  case anomalyType of
    Anomalies.ATEndpoint -> do
      totalRequestsCount <- dbtToEff $ RequestDumps.countRequestDumpByProject pid
      whenJustM (dbtToEff $ Anomalies.getAnomalyVM pid targetHash) \anomaly -> do
        endp <- dbtToEff $ Endpoints.endpointByHash pid targetHash
        users <- dbtToEff $ Projects.usersByProjectId pid
        project <- Unsafe.fromJust <<$>> dbtToEff $ Projects.projectById pid
        let enp = Unsafe.fromJust endp
        let endpointPath = enp.method <> " " <> enp.urlPath
        _ <- dbtToEff $ Anomalies.insertIssue $ Unsafe.fromJust $ Anomalies.convertAnomalyToIssue (Just enp.host) anomaly
        forM_ project.notificationsChannel \case
          Projects.NSlack ->
            sendSlackMessage
              pid
              [fmtTrim| 🤖 *New Endpoint Detected for `{project.title}`*

We have detected a new endpoint on *{project.title}*

Endpoint: `{endpointPath}`

<https://app.apitoolkit.io/p/{pid.toText}/anomalies/by_hash/{targetHash}|More details on APItoolkit>
                              |]
          Projects.NDiscord -> do
            let msg =
                  [fmtTrim|
{{···}} **New Endpoint Detected For {project.title}**

**Endpoint**: `{endpointPath}`
[View more](https://app.apitoolkit.io/p/{pid.toText}/anomalies/by_hash/{targetHash})|]
            whenJust project.discordUrl (`sendDiscordNotif` msg)
          _ -> do
            when (totalRequestsCount > 50) $
              forM_ users \u -> do
                let templateVars =
                      object
                        [ "user_name" .= u.firstName
                        , "project_name" .= project.title
                        , "anomaly_url" .= ("https://app.apitoolkit.io/p/" <> pid.toText <> "/anomalies/by_hash/" <> targetHash)
                        , "endpoint_name" .= endpointPath
                        ]
                sendPostmarkEmail (CI.original u.email) "anomaly-endpoint" templateVars
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
      users <- dbtToEff $ Projects.usersByProjectId pid
      project <- Unsafe.fromJust <<$>> dbtToEff $ Projects.projectById pid
      err <- Unsafe.fromJust <<$>> dbtToEff $ Anomalies.errorByHash targetHash
      issueId <- liftIO $ Anomalies.AnomalyId <$> UUIDV4.nextRandom
      _ <-
        dbtToEff $
          Anomalies.insertIssue $
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
      forM_ project.notificationsChannel \case
        Projects.NSlack ->
          sendSlackMessage
            pid
            [fmtTrim| 🤖 *New Runtime Exception Found for `{project.title}`*

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
        _ -> forM_ users \u -> do
          let firstName = u.firstName
          let title = project.title
          let anomaly_url = "https://app.apitoolkit.io/p/" <> pid.toText <> "/anomalies/by_hash/" <> targetHash
          let templateVars =
                [aesonQQ|{
                      "user_name": #{firstName},
                      "project_name": #{title},
                      "anomaly_url": #{anomaly_url}
                 }|]
          sendPostmarkEmail (CI.original u.email) "anomaly-field" templateVars
    Anomalies.ATField -> pass
    Anomalies.ATUnknown -> pass
