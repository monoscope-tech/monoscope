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
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.Text qualified as T
import Data.Time (ZonedTime)
import Data.Vector (Vector)
import Data.Vector qualified as V

import Database.PostgreSQL.Entity.DBT (QueryNature (Select, Update), execute, query, withPool)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Transact (DBT)
import GHC.Generics
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Field
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users

import Models.Apis.RequestDumps (EndpointPerf, RequestForReport)
import Models.Apis.RequestDumps qualified as RequestDumps
import NeatInterpolation (text, trimming)
import OddJobs.ConfigBuilder (mkConfig)
import OddJobs.Job (ConcurrencyControl (..), Job (..), startJobRunner, throwParsePayload)
import Pages.Reports qualified as RP
import Pkg.Mail
import Pkg.Ortto qualified as Ortto
import Relude
import Relude.Unsafe qualified as Unsafe

data BgJobs
  = InviteUserToProject Users.UserId Projects.ProjectId Text Text
  | CreatedProjectSuccessfully Users.UserId Projects.ProjectId Text Text
  | -- NewAnomaly Projects.ProjectId Anomalies.AnomalyTypes Anomalies.AnomalyActions TargetHash
    NewAnomaly Projects.ProjectId ZonedTime Text Text Text
  | DailyOrttoSync
  | DailyReports
  | WeeklyReports
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PerformanceReport = PerformanceReport
  { urlPath :: Text
  , method :: Text
  , averageDuration :: Integer
  , durationDiff :: Integer
  , durationDiffType :: Text
  , durationDiffPct :: Integer
  }
  deriving stock (Show, Generic)
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

getUsersByProjectId :: Projects.ProjectId -> DBT IO (Vector Users.User)
getUsersByProjectId pid = query Select q (Only pid)
 where
  q =
    [sql| select u.id, u.created_at, u.updated_at, u.deleted_at, u.active, u.first_name, u.last_name, u.display_image_url, u.email
                    from users.users u join projects.project_members pm on (pm.user_id=u.id) where project_id=? |]

getAllProjects :: DBT IO (Vector Projects.ProjectId)
getAllProjects = query Select q (Only True)
 where
  q = [sql|SELECT id FROM projects.projects WHERE active=?|]

-- TODO:
-- Analyze shapes for
-- 1: [x] how many fields in the current shape are completely new?
-- 2: [ ] how many were updated fields?
-- 3. [x] how many were deleted params
-- Send a notification email about the new anomaly (shape and endpoint etc)
--

jobsRunner :: Pool Connection -> LogAction IO String -> Config.EnvConfig -> Job -> IO ()
jobsRunner dbPool logger cfg job =
  throwParsePayload job >>= \case
    NewAnomaly pid createdAt anomalyTypesT anomalyActionsT targetHash -> do
      let anomalyType = Unsafe.fromJust $ Anomalies.parseAnomalyTypes anomalyTypesT
      -- let anomalyAction = Unsafe.fromJust $ Anomalies.parseAnomalyActions anomalyActionsT

      case anomalyType of
        Anomalies.ATEndpoint -> do
          endp <- withPool dbPool $ Endpoints.endpointByHash pid targetHash
          users <- withPool dbPool $ getUsersByProjectId pid
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
              users <- withPool dbPool $ getUsersByProjectId pid
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
              Regards,
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
              users <- withPool dbPool $ getUsersByProjectId pid
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
            Regards,
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
<a href="app.apitoolkit.io/p/$projectIdTxt">Click Here to access the project</a>
<br/><br/>
Regards,<br/>
Apitoolkit team
          |]
       in sendEmail cfg reciever subject body
    DailyOrttoSync -> do
      projReqs <- withPool dbPool getProjectsReqsCount
      logger <& "📊  pushed ortto updates for " <> show (length projReqs) <> " companies"
      Ortto.pushedTrafficViaSdk cfg.orttoApiKey $ toList projReqs
     where
      getProjectsReqsCount :: DBT IO (Vector (Projects.ProjectId, Text, Int64, Users.UserId))
      getProjectsReqsCount = query Select q ()
       where
        q =
          [sql|SELECT pp.id, pp.title, CAST(SUM(total_count) AS integer), pm.user_id --, us.email
                  FROM apis.project_requests_by_endpoint_per_min apm
                  JOIN projects.projects pp ON (id=project_id)
                  JOIN projects.project_members pm ON (pp.id = pm.project_id)
                  --JOIN users.users us on (pm.user_id = us.id)
                  where apm.timeB > NOW() - INTERVAL '1 days'
                  GROUP BY pp.id, pp.title, pm.user_id --, us.email
          |]
    DailyReports -> do
      projects <- withPool dbPool getAllProjects
      forM_ projects \p -> do
        dailyReportForProject dbPool cfg p
        pass
    WeeklyReports -> do
      projects <- withPool dbPool getAllProjects
      forM_ projects \p -> do
        weeklyReportForProject dbPool cfg p
      pass

jobsWorkerInit :: Pool Connection -> LogAction IO String -> Config.EnvConfig -> IO ()
jobsWorkerInit dbPool logger envConfig = startJobRunner $ mkConfig jobLogger "background_jobs" dbPool (MaxConcurrentJobs 1) (jobsRunner dbPool logger envConfig) id
 where
  jobLogger logLevel logEvent = logger <& show (logLevel, logEvent)

dailyReportForProject :: Pool Connection -> Config.EnvConfig -> Projects.ProjectId -> IO ()
dailyReportForProject dbPool cfg pid = do
  users <- withPool dbPool $ getUsersByProjectId pid
  if not (null users)
    then do
      let _first_user = V.head users
      anomalies <- withPool dbPool $ Anomalies.getReportAnomalies pid "daily"
      count <- withPool dbPool $ Anomalies.countAnomalies pid "daily"
      endpoint_rp <- withPool dbPool $ RequestDumps.getRequestDumpForReports pid "daily"
      previous_day <- withPool dbPool $ RequestDumps.getRequestDumpsForPreviousReportPeriod pid "daily"
      let perf_ins = RP.getPerformanceInsight endpoint_rp previous_day
      let anomaly_json = RP.buildAnomalyJSON anomalies count
      let rep_json = RP.buildReportJSON anomalies count endpoint_rp previous_day
      print rep_json
      sendEmail cfg "yousiph77@gmail.com" "Hello" "World"
    else pass

weeklyReportForProject :: Pool Connection -> Config.EnvConfig -> Projects.ProjectId -> IO ()
weeklyReportForProject dbPool cfg pid = do
  users <- withPool dbPool $ getUsersByProjectId pid
  if not (null users)
    then do
      let _first_user = V.head users
      anomalies <- withPool dbPool $ Anomalies.getReportAnomalies pid "weekly"
      count <- withPool dbPool $ Anomalies.countAnomalies pid "weekly"
      endpoint_rp <- withPool dbPool $ RequestDumps.getRequestDumpForReports pid "weekly"
      previous_day <- withPool dbPool $ RequestDumps.getRequestDumpsForPreviousReportPeriod pid "weekly"
      let perf_ins = RP.getPerformanceInsight endpoint_rp previous_day
      let anomaly_json = RP.buildAnomalyJSON anomalies count
      let rep_json = RP.buildReportJSON anomalies count endpoint_rp previous_day
      print rep_json
      pass
    else pass
