module BackgroundJobs (jobsWorkerInit, BgJobs (..)) where

-- This example is using these functions to introduce an artificial delay of a
-- few seconds in one of the jobs. Otherwise it is not really needed.

-- This example is using these functions to introduce an artificial delay of a
-- few seconds in one of the jobs. Otherwise it is not really needed.
import Colog (LogAction, (<&))
import Config qualified
import Data.Aeson as Aeson
import Data.CaseInsensitive qualified as CI
import Data.List.Extra (intersect, union)
import Data.Pool (Pool, withResource)
import Data.Text qualified as T
import Data.Time (UTCTime (utctDay), ZonedTime, getCurrentTime, getZonedTime)
import Data.Time.Calendar
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (Select, Update), execute, query, withPool)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Transact (DBT)
import GHC.Generics
import Lucid (Html, renderText, style_, table_, tbody_, td_, th_, thead_, toHtml, tr_)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Reports qualified as Reports
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import NeatInterpolation (text, trimming)
import OddJobs.ConfigBuilder (mkConfig)
import OddJobs.Job (ConcurrencyControl (..), Job (..), createJob, startJobRunner, throwParsePayload)
import Pages.Reports qualified as RP
import Pkg.Mail
import Relude
import Relude.Unsafe qualified as Unsafe


data BgJobs
  = InviteUserToProject Users.UserId Projects.ProjectId Text Text
  | CreatedProjectSuccessfully Users.UserId Projects.ProjectId Text Text
  | -- NewAnomaly Projects.ProjectId Anomalies.AnomalyTypes Anomalies.AnomalyActions TargetHash
    NewAnomaly Projects.ProjectId ZonedTime Text Text Text
  | DailyReports Projects.ProjectId
  | WeeklyReports Projects.ProjectId
  | DailyJob
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


getShapes :: Projects.ProjectId -> Text -> DBT IO (Vector (Text, Vector Text))
getShapes pid enpHash = query Select q (pid, enpHash)
  where
    q = [sql| select hash, field_hashes from apis.shapes where project_id=? and endpoint_hash=? |]


instance FromRow Text where
  fromRow = field


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


getAllProjects :: DBT IO (Vector Projects.ProjectId)
getAllProjects = query Select q (Only True)
  where
    q = [sql|SELECT id FROM projects.projects WHERE active=? AND deleted_at IS NULL|]


-- TODO:
-- Analyze shapes for
-- 1: [x] how many fields in the current shape are completely new?
-- 2: [ ] how many were updated fields?
-- 3. [x] how many were deleted params
-- Send a notification email about the new anomaly (shape and endpoint etc)
--

jobsRunner :: Pool Connection -> LogAction IO String -> Config.EnvConfig -> Job -> IO ()
jobsRunner dbPool logger cfg job = do
  when cfg.enableBackgroundJobs do
    throwParsePayload job >>= \case
      NewAnomaly pid createdAt anomalyTypesT anomalyActionsT targetHash -> do
        let anomalyType = Unsafe.fromJust $ Anomalies.parseAnomalyTypes anomalyTypesT
        -- let anomalyAction = Unsafe.fromJust $ Anomalies.parseAnomalyActions anomalyActionsT

        case anomalyType of
          Anomalies.ATEndpoint -> do
            endp <- withPool dbPool $ Endpoints.endpointByHash pid targetHash
            users <- withPool dbPool $ Projects.usersByProjectId pid
            project <- Unsafe.fromJust <<$>> withPool dbPool $ Projects.projectById pid
            let enp = Unsafe.fromJust endp
            let endpointPath = enp.method <> " " <> enp.urlPath
            forM_ users \u ->
              let projectTitle = project.title
                  projectIdTxt = pid.toText
                  name = u.firstName
                  subject = [text| 🤖 APITOOLKIT: New Endpoint detected for `$projectTitle` |]
                  body =
                    toLText
                      [trimming|
            Hi $name,<br/>

            <p>We detected a new endpoint on ``$projectTitle`:</p>
            <p><strong>$endpointPath</strong></p>
            <a href="https://app.apitoolkit.io/p/$projectIdTxt/anomalies">More details on the apitoolkit</a>
            <br/><br/>
            Regards,
            Apitoolkit team
                      |]
                  reciever = CI.original u.email
               in sendEmail cfg reciever subject body
          Anomalies.ATShape -> do
            shapes <- withPool dbPool $ getShapes pid $ T.take 8 targetHash
            let targetFields = maybe [] (toList . snd) (V.find (\a -> fst a == targetHash) shapes)
            updatedFieldFormats <- withPool dbPool $ getUpdatedFieldFormats pid (V.fromList targetFields)
            let otherFields = toList <$> toList (snd $ V.unzip $ V.filter (\a -> fst a /= targetHash) shapes)
            let newFields = filter (`notElem` foldl' union [] otherFields) targetFields
            let deletedFields = filter (`notElem` targetFields) $ foldl' intersect (head $ [] :| otherFields) (tail $ [] :| otherFields)
            -- Update the shape values in the database
            _ <- withPool dbPool $ updateShapeCounts pid targetHash (V.fromList newFields) (V.fromList deletedFields) updatedFieldFormats
            -- Send an email about the new shape anomaly but only if there was no endpoint anomaly logged
            anomalyM <- withPool dbPool $ Anomalies.getAnomalyVM pid $ T.take 8 targetHash
            case anomalyM of
              Nothing -> pass
              Just anomaly -> do
                -- TODO: DOn't send any anomaly emails other than for email
                error "retry later"
                users <- withPool dbPool $ Projects.usersByProjectId pid
                project <- Unsafe.fromJust <<$>> withPool dbPool $ Projects.projectById pid
                forM_ users \u ->
                  let projectTitle = project.title
                      projectIdTxt = pid.toText
                      name = u.firstName
                      subject = [text| 🤖 APITOOLKIT: New Shape anomaly found for `$projectTitle` |]
                      body =
                        toLText
                          [trimming|
  Hi $name,<br/>

  <p>We detected a different API request shape to your endpoints than what you usually have..</p>
  <a href="https://app.apitoolkit.io/p/$projectIdTxt/anomalies">More details on the apitoolkit</a>
  <br/><br/>
  Regards,<br/>
  Apitoolkit team
                          |]
                      reciever = CI.original u.email
                   in sendEmail cfg reciever subject body
          Anomalies.ATFormat -> do
            -- Send an email about the new shape anomaly but only if there was no endpoint anomaly logged
            anomalyM <- withPool dbPool $ Anomalies.getAnomalyVM pid targetHash
            case anomalyM of
              Nothing -> pass
              Just anomaly -> do
                -- TODO: DOn't send any anomaly emails other than for email
                error "retry later"
                users <- withPool dbPool $ Projects.usersByProjectId pid
                project <- Unsafe.fromJust <<$>> withPool dbPool $ Projects.projectById pid
                forM_ users \u ->
                  let projectTitle = project.title
                      projectIdTxt = pid.toText
                      name = u.firstName
                      subject = [text| 🤖 APITOOLKIT: New field format anomaly found for `$projectTitle` |]
                      body =
                        toLText
                          [trimming|
  Hi $name,<br/>

  <p>We detected that a particular field on your API is returning a different format/type than what it usually gets.</p>
  <a href="https://app.apitoolkit.io/p/$projectIdTxt/anomalies">More details on the apitoolkit</a>
  <br/><br/>
  Regards,<br/>
  Apitoolkit team
                        |]
                      reciever = CI.original u.email
                   in sendEmail cfg reciever subject body
          Anomalies.ATField -> pass
          Anomalies.ATUnknown -> pass
      InviteUserToProject userId projectId reciever projectTitle' ->
        let projectTitle = projectTitle'
            projectIdTxt = projectId.toText
            subject = [text| 🤖 APITOOLKIT: You've been invited to a project '$projectTitle' on apitoolkit.io |]
            body =
              toLText
                [trimming|
  Hi,<br/>

  <p>You have been invited to the $projectTitle project on apitoolkit. 
  Please use the following link to activate your account and access the $projectTitle project.</p>
  <a href="https://app.apitoolkit.io/p/$projectIdTxt">Click Here</a>
  <br/><br/>
  Regards,
  Apitoolkit team
            |]
         in sendEmail cfg reciever subject body
      CreatedProjectSuccessfully userId projectId reciever projectTitle' ->
        let projectTitle = projectTitle'
            projectIdTxt = projectId.toText
            subject = [text| 🤖 APITOOLKIT: Project created successfully '$projectTitle ' on apitoolkit.io |]
            body =
              toLText
                [trimming|
  Hi,<br/>

  <p>You have been invited to the $projectTitle project on apitoolkit. 
  Please use the following link to activate your account and access the $projectTitle project.</p>
  <a href="app.apitoolkit.io/p/$projectIdTxt">Click Here to access the project</a><br/><br/>.

  By the way, we know it can be difficult or confusing to integrate SDKs sometimes. So we're willing to assist. You can schedule a time here, and we can help with integrating: 
  <a href="https://calendar.google.com/calendar/u/0/appointments/schedules/AcZssZ21Q1uDPjHN4YPpM2lNBS0_Nwc16IQj-25e5WIoPOKEVsBBIWJgy3usCUS4d7MtQz7kiuzyBJLb">Click Here to Schedule</a>
  <br/><br/>
  Regards,<br/>
  Apitoolkit team
            |]
         in sendEmail cfg reciever subject body
      DailyJob -> do
        projects <- withPool dbPool getAllProjects
        forM_ projects \p -> do
          liftIO $ withResource dbPool \conn -> do
            currentDay <- utctDay <$> getCurrentTime
            if dayOfWeek currentDay == Monday
              then createJob conn "background_jobs" $ BackgroundJobs.WeeklyReports p
              else createJob conn "background_jobs" $ BackgroundJobs.DailyReports p
        pass
      DailyReports pid -> do
        dailyReportForProject dbPool cfg pid
        pass
      WeeklyReports pid -> do
        weeklyReportForProject dbPool cfg pid
        pass


jobsWorkerInit :: Pool Connection -> LogAction IO String -> Config.EnvConfig -> IO ()
jobsWorkerInit dbPool logger envConfig = startJobRunner $ mkConfig jobLogger "background_jobs" dbPool (MaxConcurrentJobs 1) (jobsRunner dbPool logger envConfig) id
  where
    jobLogger logLevel logEvent = logger <& show (logLevel, logEvent)


dailyReportForProject :: Pool Connection -> Config.EnvConfig -> Projects.ProjectId -> IO ()
dailyReportForProject dbPool cfg pid = do
  users <- withPool dbPool $ Projects.usersByProjectId pid
  projectM <- withPool dbPool $ Projects.projectById pid
  forM_ projectM \pr -> do
    users & mapM_ \user -> do
      anomalies <- withPool dbPool $ Anomalies.getReportAnomalies pid "daily"
      endpoint_rp <- withPool dbPool $ RequestDumps.getRequestDumpForReports pid "daily"
      previous_day <- withPool dbPool $ RequestDumps.getRequestDumpsForPreviousReportPeriod pid "daily"
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
      _ <- withPool dbPool $ Reports.addReport report
      when pr.dailyNotif do
        let body = renderText $ RP.reportEmail pid report
        let projectTitle = pr.title
        let subject = [text| APITOOLKIT: Daily Report for $projectTitle |]
        sendEmail cfg (CI.original user.email) subject body


weeklyReportForProject :: Pool Connection -> Config.EnvConfig -> Projects.ProjectId -> IO ()
weeklyReportForProject dbPool cfg pid = do
  users <- withPool dbPool $ Projects.usersByProjectId pid
  projectM <- withPool dbPool $ Projects.projectById pid
  forM_ projectM \pr -> do
    users & mapM_ \user -> do
      anomalies <- withPool dbPool $ Anomalies.getReportAnomalies pid "weekly"
      endpoint_rp <- withPool dbPool $ RequestDumps.getRequestDumpForReports pid "weekly"
      previous_week <- withPool dbPool $ RequestDumps.getRequestDumpsForPreviousReportPeriod pid "weekly"
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
      _ <- withPool dbPool $ Reports.addReport report
      when pr.weeklyNotif do
        let body = renderText $ RP.reportEmail pid report
        let projectTitle = pr.title
        let subject = [text| APITOOLKIT: Weekly Report for `$projectTitle` |]
        sendEmail cfg (CI.original user.email) subject body
