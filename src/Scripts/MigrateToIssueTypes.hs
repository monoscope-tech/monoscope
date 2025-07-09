module Scripts.MigrateToIssueTypes (migrateToNewIssueTypes) where

import Control.Lens ((.~))
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (execute, query)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact qualified as PTR
import Effectful (Eff, (:>))
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Log qualified
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Issues qualified as Issues
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import OddJobs.Job (createJob)
import Relude hiding (ask)
import System.Config qualified as Config
import System.Types (ATBackgroundCtx)
import BackgroundJobs qualified

-- | Migrate all existing anomalies to the new issue types system
migrateToNewIssueTypes :: ATBackgroundCtx ()
migrateToNewIssueTypes = do
  ctx <- ask @Config.AuthContext
  Log.logInfo "Starting migration to new issue types..." ()
  
  -- Step 1: Get all projects with anomalies
  projectIds <- dbtToEff $ query
    [sql| SELECT DISTINCT project_id 
          FROM apis.anomalies 
          WHERE acknowleged_at IS NULL
          ORDER BY project_id |] ()
  
  Log.logInfo "Found projects to migrate" (length projectIds)
  
  -- Step 2: Process each project
  forM_ projectIds $ \(Only pid) -> do
    migrateProjectAnomalies ctx pid
  
  Log.logInfo "Migration completed successfully!" ()

-- | Migrate anomalies for a single project
migrateProjectAnomalies :: Config.AuthContext -> Projects.ProjectId -> ATBackgroundCtx ()
migrateProjectAnomalies ctx pid = do
  Log.logInfo "Migrating project" pid.toText
  
  -- Get all unacknowledged anomalies
  anomalies <- dbtToEff $ query
    [sql| SELECT DISTINCT target_hash, anomaly_type
          FROM apis.anomalies 
          WHERE project_id = ? 
            AND acknowleged_at IS NULL
          ORDER BY anomaly_type, target_hash |] 
    (Only pid) :: ATBackgroundCtx (V.Vector (Text, Anomalies.AnomalyTypes))
  
  -- Group by type
  let endpointAnomalies = V.filter (\(_, t) -> t == Anomalies.ATEndpoint) anomalies
      shapeAnomalies = V.filter (\(_, t) -> t == Anomalies.ATShape) anomalies
      formatAnomalies = V.filter (\(_, t) -> t == Anomalies.ATFormat) anomalies
      runtimeAnomalies = V.filter (\(_, t) -> t == Anomalies.ATRuntimeException) anomalies
  
  -- Process API changes (endpoint + shape + format)
  let apiChangeHashes = V.map fst $ endpointAnomalies <> shapeAnomalies <> formatAnomalies
  unless (V.null apiChangeHashes) $ do
    processAPIChanges ctx pid apiChangeHashes
  
  -- Process runtime exceptions
  let runtimeHashes = V.map fst runtimeAnomalies
  unless (V.null runtimeHashes) $ do
    processRuntimeExceptions ctx pid runtimeHashes
  
  Log.logInfo "Completed migration for project" pid.toText

-- | Process API change anomalies
processAPIChanges :: Config.AuthContext -> Projects.ProjectId -> V.Vector Text -> ATBackgroundCtx ()
processAPIChanges ctx pid targetHashes = do
  -- Get all anomaly details
  anomaliesVM <- dbtToEff $ Anomalies.getAnomaliesVM pid targetHashes
  
  -- Group by endpoint hash
  let anomaliesByEndpoint = groupByEndpointHash anomaliesVM
  
  Log.logInfo "Creating API change issues" (length anomaliesByEndpoint, pid.toText)
  
  -- Create one issue per endpoint
  forM_ anomaliesByEndpoint $ \(endpointHash, anomalies) -> do
    -- Create API change issue
    issue <- liftIO $ Issues.createAPIChangeIssue pid endpointHash anomalies
    dbtToEff $ Issues.insertIssue issue
    
    -- Queue enhancement job
    _ <- liftIO $ withResource ctx.jobsPool \conn -> 
      createJob conn "background_jobs" $ BackgroundJobs.EnhanceIssuesWithLLM pid (V.singleton issue.id)
    pass

-- | Process runtime exception anomalies
processRuntimeExceptions :: Config.AuthContext -> Projects.ProjectId -> V.Vector Text -> ATBackgroundCtx ()
processRuntimeExceptions ctx pid targetHashes = do
  -- Get error details
  errors <- dbtToEff $ Anomalies.errorsByHashes pid targetHashes
  
  Log.logInfo "Creating runtime exception issues" (V.length errors, pid.toText)
  
  -- Create one issue per error
  forM_ errors $ \err -> do
    issue <- liftIO $ Issues.createRuntimeExceptionIssue pid err.errorData
    dbtToEff $ Issues.insertIssue issue
    
    -- Queue enhancement job
    _ <- liftIO $ withResource ctx.jobsPool \conn -> 
      createJob conn "background_jobs" $ BackgroundJobs.EnhanceIssuesWithLLM pid (V.singleton issue.id)
    pass

-- | Group anomalies by endpoint hash
groupByEndpointHash :: V.Vector Anomalies.AnomalyVM -> [(Text, V.Vector Anomalies.AnomalyVM)]
groupByEndpointHash anomalies =
  let getEndpointHash a = case a.anomalyType of
        Anomalies.ATEndpoint -> a.targetHash
        _ -> T.take 8 a.targetHash
      sorted = sortBy (comparing getEndpointHash) $ V.toList anomalies
      grouped = groupBy (\a b -> getEndpointHash a == getEndpointHash b) sorted
  in map (\grp -> (getEndpointHash $ head grp, V.fromList grp)) grouped