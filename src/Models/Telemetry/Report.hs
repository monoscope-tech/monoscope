-- | Persisted evidence for system reports. Counts are collected before the email's
-- presentation limits; optional sources retain failure separately from an empty result.
module Models.Telemetry.Report (
  ReportSnapshot (..),
  PreviewStatus (..),
  claimPreview,
  getPreview,
  finishPreview,
  EndpointStats (..),
  EndpointComparison (..),
  DatabaseStats (..),
  WorkloadStats (..),
  compareEndpoints,
  ReportSection (..),
  ServiceStats (..),
  ServiceComparison (..),
  InfrastructureStats (..),
  InfrastructureResource (..),
  MonitorStats (..),
  MonitorObservation (..),
  IssueStats (..),
  IssueObservation (..),
  serviceStats,
  compareServices,
  issueStats,
  monitorStats,
  infrastructureStats,
  endpointStats,
  databaseStats,
  workloadStats,
) where

import Data.Aeson qualified as AE
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Data.Map.Strict qualified as M
import Data.Time (UTCTime, addUTCTime)
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Labeled (Labeled)
import Hasql.Interpolate qualified as HI
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Containers qualified as Containers
import Pkg.DeriveUtils (AesonText (..), DB)
import Relude


data ReportSection a = Available a | Unavailable
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data ReportSnapshot = ReportSnapshot
  { services :: [ServiceComparison]
  , infrastructure :: ReportSection InfrastructureStats
  , monitors :: ReportSection MonitorStats
  , issues :: ReportSection IssueStats
  , generatedAt :: UTCTime
  , topPatterns :: ReportSection [(Text, Int64, Text)]
  , performance :: ReportSection [EndpointComparison]
  , databases :: ReportSection [DatabaseStats]
  , workloads :: ReportSection [WorkloadStats]
  , ingestionCapped :: Maybe Bool
  , startTime :: UTCTime
  , endTime :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | All events and server spans have deliberately separate denominators. Latency
-- is in milliseconds, converted once by the database projection.
data ServiceStats = ServiceStats
  { service :: Maybe Text
  , environment :: Maybe Text
  , events :: Int64
  , errorEvents :: Int64
  , logs :: Int64
  , serverRequests :: Int64
  , serverErrors :: Int64
  , recentDayEvents :: Int64
  , serverLatencyMs :: Maybe Double
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


data ServiceComparison = ServiceComparison
  { service :: Maybe Text
  , environment :: Maybe Text
  , current :: Maybe ServiceStats
  , previous :: Maybe ServiceStats
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Missing services in either period remain visible; no baseline is not a 0% change.
compareServices :: [ServiceStats] -> [ServiceStats] -> [ServiceComparison]
compareServices current previous =
  sortOn
    (\s -> (Down $ maybe 0 (.errorEvents) s.current, Down $ maybe 0 (.events) s.current, s.service, s.environment))
    [ ServiceComparison service environment (M.lookup key currentMap) (M.lookup key previousMap)
    | key@(service, environment) <- M.keys $ M.union currentMap previousMap
    ]
  where
    index = M.fromList . map (\s -> ((s.service, s.environment), s))
    currentMap = index current
    previousMap = index previous


serviceStats :: (DB es, Labeled "timefusion" Hasql :> es) => Bool -> Projects.ProjectId -> UTCTime -> UTCTime -> Eff es [ServiceStats]
serviceStats useTf pid start end =
  Hasql.withHasqlTimefusion useTf
    $ Hasql.interp
      [HI.sql|
    SELECT NULLIF(resource___service___name, ''), NULLIF(resource___deployment___environment___name, ''),
      COUNT(*)::bigint,
      COUNT(*) FILTER (WHERE status_code = 'ERROR' OR attributes___exception___type IS NOT NULL
        OR lower(level) IN ('error', 'fatal') OR severity___severity_number >= 17)::bigint,
      COUNT(*) FILTER (WHERE kind = 'log')::bigint,
      COUNT(*) FILTER (WHERE kind = 'server')::bigint,
      COUNT(*) FILTER (WHERE kind = 'server' AND (status_code = 'ERROR'
        OR attributes___exception___type IS NOT NULL))::bigint,
      COUNT(*) FILTER (WHERE timestamp >= #{dayStart})::bigint,
      (AVG(duration) FILTER (WHERE kind = 'server') / 1000000.0)::float8
    FROM otel_logs_and_spans
    WHERE project_id = #{pid.toText} AND timestamp >= #{start} AND timestamp < #{end}
    GROUP BY NULLIF(resource___service___name, ''), NULLIF(resource___deployment___environment___name, '')
  |]
  where
    dayStart = addUTCTime (-86400) end


data EndpointStats = EndpointStats
  { service :: Maybe Text
  , environment :: Maybe Text
  , host :: Text
  , method :: Text
  , path :: Text
  , averageMs :: Maybe Double
  , requests :: Int64
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


data EndpointComparison = EndpointComparison
  { current :: EndpointStats
  , previous :: Maybe EndpointStats
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


compareEndpoints :: [EndpointStats] -> [EndpointStats] -> [EndpointComparison]
compareEndpoints current previous = map (\e -> EndpointComparison e $ M.lookup (key e) indexed) current
  where
    key e = (e.service, e.environment, e.host, e.method, e.path)
    indexed = M.fromList [(key e, e) | e <- previous]


data DatabaseStats = DatabaseStats
  { service :: Maybe Text
  , statement :: Text
  , averageMs :: Double
  , operations :: Int64
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


data WorkloadStats = WorkloadStats
  { kind :: Text
  , events :: Int64
  , averageMs :: Maybe Double
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


endpointStats :: (DB es, Labeled "timefusion" Hasql :> es) => Bool -> Projects.ProjectId -> UTCTime -> UTCTime -> Eff es [EndpointStats]
endpointStats useTf pid start end =
  Hasql.withHasqlTimefusion useTf
    $ Hasql.interp
      [HI.sql|
    SELECT NULLIF(resource___service___name, ''), NULLIF(resource___deployment___environment___name, ''),
      COALESCE(attributes___server___address, resource___service___name, ''),
      attributes___http___request___method, COALESCE(attributes___url___path, ''),
      (AVG(duration) / 1000000.0)::float8, COUNT(*)::bigint
    FROM otel_logs_and_spans
    WHERE project_id = #{pid.toText} AND timestamp >= #{start} AND timestamp < #{end}
      AND kind = 'server' AND attributes___http___request___method IS NOT NULL
    GROUP BY NULLIF(resource___service___name, ''), NULLIF(resource___deployment___environment___name, ''), COALESCE(attributes___server___address, resource___service___name, ''),
      attributes___http___request___method, COALESCE(attributes___url___path, '')
    ORDER BY COUNT(*) DESC
  |]


databaseStats :: (DB es, Labeled "timefusion" Hasql :> es) => Bool -> Projects.ProjectId -> UTCTime -> UTCTime -> Eff es [DatabaseStats]
databaseStats useTf pid start end =
  Hasql.withHasqlTimefusion useTf
    $ Hasql.interp
      [HI.sql|
    SELECT resource___service___name, attributes___db___query___text, (AVG(duration) / 1000000.0)::float8, COUNT(*)::bigint
    FROM otel_logs_and_spans
    WHERE project_id = #{pid.toText} AND timestamp >= #{start} AND timestamp < #{end}
      AND kind != 'log' AND attributes___db___query___text IS NOT NULL
    GROUP BY resource___service___name, attributes___db___query___text HAVING AVG(duration) > 500000000
    ORDER BY AVG(duration) DESC LIMIT 10
  |]


workloadStats :: (DB es, Labeled "timefusion" Hasql :> es) => Bool -> Projects.ProjectId -> UTCTime -> UTCTime -> Eff es [WorkloadStats]
workloadStats useTf pid start end =
  Hasql.withHasqlTimefusion useTf
    $ Hasql.interp
      [HI.sql|
    SELECT COALESCE(kind, 'unspecified'), COUNT(*)::bigint, (AVG(duration) FILTER (WHERE kind != 'log') / 1000000.0)::float8
    FROM otel_logs_and_spans
    WHERE project_id = #{pid.toText} AND timestamp >= #{start} AND timestamp < #{end}
    GROUP BY COALESCE(kind, 'unspecified') ORDER BY COUNT(*) DESC
  |]


data InfrastructureResource = InfrastructureResource
  { name :: Text
  , scope :: Text
  , host :: Maybe Text
  , cluster :: Maybe Text
  , namespace :: Maybe Text
  , cpuRatio :: Maybe Double
  , memoryRatio :: Maybe Double
  , storageRatio :: Maybe Double
  , ready :: Maybe Bool
  , restartCounter :: Maybe Double
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data InfrastructureStats = InfrastructureStats
  { hosts :: Int
  , containers :: Int
  , pods :: Int
  , resources :: [InfrastructureResource]
  , unready :: Int
  , observedFrom :: UTCTime
  , observedUntil :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


infrastructureStats :: UTCTime -> UTCTime -> V.Vector Containers.ContainerRow -> InfrastructureStats
infrastructureStats start end rows = InfrastructureStats (count Containers.ScopeHost) (count Containers.ScopeContainer) (count Containers.ScopePod) resources (V.length $ V.filter ((== Just 0) . (.ready)) rows) start end
  where
    count scope = V.length $ V.filter ((== scope) . (.scope)) rows
    resources =
      take 10 $ sortOn (\r -> (r.ready /= Just False, Down $ foldl' max 0 (catMaybes [r.cpuRatio, r.memoryRatio, r.storageRatio]), r.name)) $ V.toList rows <&> \r ->
        InfrastructureResource
          { name = r.containerName
          , scope = case r.scope of
              Containers.ScopeHost -> "Host"
              Containers.ScopeContainer -> "Container"
              Containers.ScopePod -> "Pod"
          , host = r.nodeName
          , cluster = r.cluster
          , namespace = r.namespace
          , cpuRatio = Containers.cpuPctOfLimit r
          , memoryRatio = Containers.memPctOfLimit r
          , storageRatio = r.storagePct
          , ready = (> 0) <$> r.ready
          , restartCounter = r.restarts
          }


data MonitorObservation = MonitorObservation
  { id :: Text
  , title :: Text
  , status :: Text
  , value :: Maybe Double
  , lastEvaluated :: Maybe UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data MonitorStats = MonitorStats
  { normal :: Int
  , warning :: Int
  , alerting :: Int
  , paused :: Int
  , unevaluated :: Int
  , observations :: [MonitorObservation]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


monitorStats :: [Monitors.QueryMonitor] -> MonitorStats
monitorStats rows = MonitorStats (count "Normal") (count "Warning") (count "Alerting") (count "Paused") (count "Not evaluated") observations
  where
    count label = length $ filter ((== label) . (.status)) observations
    observations =
      sortOn (\m -> (rank m.status, m.title)) $ rows <&> \m ->
        MonitorObservation
          { id = m.id.toText
          , title = m.alertConfig.title
          , status =
              if isJust m.deactivatedAt
                then "Paused"
                else
                  if isNothing m.lastEvaluated
                    then "Not evaluated"
                    else case m.currentStatus of
                      Monitors.MSNormal -> "Normal"
                      Monitors.MSWarning -> "Warning"
                      Monitors.MSAlerting -> "Alerting"
          , value = m.lastEvaluated $> m.currentValue
          , lastEvaluated = m.lastEvaluated
          }
    rank :: Text -> Int
    rank = \case
      "Alerting" -> 0
      "Warning" -> 1
      "Not evaluated" -> 2
      "Paused" -> 3
      _ -> 4


data IssueObservation = IssueObservation
  { id :: Text
  , title :: Text
  , service :: Maybe Text
  , severity :: Text
  , issueType :: Text
  , affectedRequests :: Int64
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


data IssueStats = IssueStats
  { newIssues :: Int64
  , openIssues :: Int64
  , criticalOpen :: Int64
  , acknowledged :: Int64
  , archivedInPeriod :: Int64
  , priorities :: [IssueObservation]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Lifecycle totals are database aggregates, not the length of the priority sample.
-- State is observed at generation; archived is not called resolved, since archival
-- does not establish that the underlying failure recovered.
issueStats :: DB es => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es IssueStats
issueStats pid start end = do
  totals <-
    Hasql.interpOne
      [HI.sql|
      SELECT COUNT(*) FILTER (WHERE created_at >= #{start} AND created_at < #{end})::bigint,
        COUNT(*) FILTER (WHERE archived_at IS NULL AND acknowledged_at IS NULL)::bigint,
        COUNT(*) FILTER (WHERE archived_at IS NULL AND acknowledged_at IS NULL AND (critical OR severity = 'critical'))::bigint,
        COUNT(*) FILTER (WHERE archived_at IS NULL AND acknowledged_at IS NOT NULL)::bigint,
        COUNT(*) FILTER (WHERE archived_at >= #{start} AND archived_at < #{end})::bigint
      FROM apis.issues WHERE project_id = #{pid}
    |]
  priorities <-
    Hasql.interp
      [HI.sql|
      SELECT id::text, title, service, CASE WHEN critical THEN 'critical' ELSE COALESCE(severity, 'info') END,
        issue_type::text, affected_requests::bigint
      FROM apis.issues WHERE project_id = #{pid} AND archived_at IS NULL AND acknowledged_at IS NULL
      ORDER BY (critical OR severity = 'critical') DESC NULLS LAST, affected_requests DESC, created_at DESC LIMIT 10
    |]
  let (newIssues, openIssues, criticalOpen, acknowledged, archivedInPeriod) = fromMaybe (0, 0, 0, 0, 0) totals
  pure IssueStats{newIssues, openIssues, criticalOpen, acknowledged, archivedInPeriod, priorities}


-- | A shared preview survives polling across replicas. Only the lease owner may
-- publish a result; a restart leaves an expiring lease that a later request renews.
data PreviewStatus = PreviewBuilding | PreviewReady ReportSnapshot | PreviewFailed
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


claimPreview :: DB es => Projects.ProjectId -> Eff es (Maybe UTCTime)
claimPreview pid = do
  let payload = AesonText PreviewBuilding
  fmap (\(HI.OneColumn token) -> token)
    . listToMaybe
    <$> Hasql.interp
      [HI.sql|
        INSERT INTO apis.report_previews (project_id, requested_at, expires_at, payload)
        VALUES (#{pid}, clock_timestamp(), now() + interval '6 minutes', #{payload})
        ON CONFLICT (project_id) DO UPDATE
          SET requested_at = EXCLUDED.requested_at, expires_at = EXCLUDED.expires_at, payload = EXCLUDED.payload
          WHERE report_previews.expires_at <= now()
        RETURNING requested_at
      |]


getPreview :: DB es => Projects.ProjectId -> Eff es (Maybe PreviewStatus)
getPreview pid =
  fmap (\(HI.OneColumn (AesonText payload)) -> payload)
    . listToMaybe
    <$> Hasql.interp [HI.sql|SELECT payload FROM apis.report_previews WHERE project_id = #{pid}|]


finishPreview :: DB es => Projects.ProjectId -> UTCTime -> PreviewStatus -> Eff es ()
finishPreview pid token status = do
  let payload = AesonText status
      ttlSeconds = case status of
        PreviewReady{} -> 300 :: Int64
        PreviewFailed -> 60
        PreviewBuilding -> 360
  Hasql.interpExecute_
    [HI.sql|
      UPDATE apis.report_previews SET payload = #{payload}, expires_at = now() + make_interval(secs => #{ttlSeconds}::double precision)
      WHERE project_id = #{pid} AND requested_at = #{token}
    |]
