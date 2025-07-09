module Scripts.RecreateIssues (recreateAllIssues) where

import Control.Lens ((.~))
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (execute, query)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
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
import Models.Projects.Projects qualified as Projects
import OddJobs.Job (createJob)
import Relude hiding (ask)
import System.Config qualified as Config
import System.Types (ATBackgroundCtx)

-- | Recreate all issues by processing existing anomalies
-- This groups anomalies by endpoint and creates one issue per endpoint
recreateAllIssues :: ATBackgroundCtx ()
recreateAllIssues = do
  ctx <- ask @Config.AuthContext
  Log.logInfo "Starting issue recreation process..." ()
  
  -- Step 1: Delete all existing issues (except runtime exceptions)
  deleteCount <- dbtToEff $ execute 
    [sql| DELETE FROM apis.issues WHERE anomaly_type != 'runtime_exception' |] ()
  Log.logInfo "Deleted existing issues" deleteCount
  
  -- Step 2: Get all projects with unacknowledged anomalies
  projectIds <- dbtToEff $ query
    [sql| SELECT DISTINCT project_id 
          FROM apis.anomalies 
          WHERE acknowleged_at IS NULL 
            AND anomaly_type IN ('endpoint', 'shape', 'format')
          ORDER BY project_id |] ()
  
  Log.logInfo "Found projects with anomalies" (length projectIds)
  
  -- Step 3: Process each project
  forM_ projectIds $ \(Only pid) -> do
    processProjectAnomalies ctx pid
  
  -- Step 4: Queue all issues for LLM enhancement
  Log.logInfo "Queuing issues for LLM enhancement..." ()
  issuesToEnhance <- dbtToEff $ query
    [sql| SELECT id, project_id
          FROM apis.issues
          WHERE llm_enhanced_at IS NULL
            AND acknowleged_at IS NULL
            AND archived_at IS NULL
          ORDER BY project_id
          LIMIT 1000 |] ()
  
  -- Group by project and create enhancement jobs
  let issuesByProject = V.groupBy (\a b -> snd a == snd b) issuesToEnhance
  
  liftIO $ withResource ctx.jobsPool \conn ->
    forM_ issuesByProject \projectIssues -> case V.uncons projectIssues of
      Nothing -> pass
      Just ((_, projectId), _) -> do
        let issueIds = V.map (Anomalies.AnomalyId . fst) projectIssues
        void $ createJob conn "background_jobs" $ 
          BackgroundJobs.EnhanceIssuesWithLLM projectId issueIds
  
  Log.logInfo "Issue recreation completed successfully!" ()

-- | Process anomalies for a single project and create grouped issues
processProjectAnomalies :: Config.AuthContext -> Projects.ProjectId -> ATBackgroundCtx ()
processProjectAnomalies ctx pid = do
  Log.logInfo "Processing project" pid.toText
  
  -- Get all unacknowledged anomalies for this project
  anomalies <- dbtToEff $ Anomalies.getAnomaliesVM pid =<< getAllUnacknowledgedTargetHashes pid
  
  -- Get endpoints for the anomalies
  let endpointHashes = V.map (T.take 8) $ V.filter (\a -> T.length a >= 8) $ V.map (.targetHash) anomalies
  endpoints <- dbtToEff $ Endpoints.endpointsByHashes pid endpointHashes
  
  -- Group anomalies by endpoint
  let anomaliesByEndpoint = groupAnomaliesByEndpoint anomalies endpoints
  
  Log.logInfo "Creating issues for endpoints" (V.length anomaliesByEndpoint)
  
  -- Create one issue per endpoint group
  currentTime <- Time.currentTime
  forM_ anomaliesByEndpoint $ \groupedAnomalies -> do
    -- Get the first anomaly as representative
    case V.uncons groupedAnomalies of
      Nothing -> pass
      Just (firstAnomaly, restAnomalies) -> do
        -- Find the endpoint for this group
        let endpointHash = case firstAnomaly.anomalyType of
              Anomalies.ATEndpoint -> firstAnomaly.targetHash
              _ -> T.take 8 firstAnomaly.targetHash
        
        let endpoint = V.find (\e -> e.hash == endpointHash) endpoints
        
        -- Convert anomalies to issue
        case Anomalies.convertAnomalyToIssue (endpoint >>= \e -> Just e.host) firstAnomaly of
          Nothing -> Log.logAttention "Failed to convert anomaly to issue" firstAnomaly.id
          Just baseIssue -> do
            -- Merge all anomalies in the group
            let mergedIssue = V.foldl' mergeAnomalyIntoIssue baseIssue restAnomalies
            
            -- Insert the grouped issue
            _ <- dbtToEff $ Anomalies.insertIssues (V.singleton mergedIssue)
            Log.logInfo "Created issue for endpoint" endpointHash
  
  where
    -- Get all unacknowledged target hashes for a project
    getAllUnacknowledgedTargetHashes :: Projects.ProjectId -> PTR.DBT IO (V.Vector Text)
    getAllUnacknowledgedTargetHashes projectId = query
      [sql| SELECT DISTINCT target_hash 
            FROM apis.anomalies 
            WHERE project_id = ? 
              AND acknowleged_at IS NULL 
              AND anomaly_type IN ('endpoint', 'shape', 'format') |] 
      (Only projectId)
    
    -- Group anomalies by their endpoint
    groupAnomaliesByEndpoint :: V.Vector Anomalies.AnomalyVM -> V.Vector Endpoints.Endpoint -> V.Vector (V.Vector Anomalies.AnomalyVM)
    groupAnomaliesByEndpoint anomalies endpoints =
      let getEndpointHash anomaly = case anomaly.anomalyType of
            Anomalies.ATEndpoint -> anomaly.targetHash
            _ -> T.take 8 anomaly.targetHash
          grouped = V.groupBy (\a b -> getEndpointHash a == getEndpointHash b) anomalies
      in V.fromList grouped
    
    -- Merge an anomaly into an existing issue
    mergeAnomalyIntoIssue :: Anomalies.Issue -> Anomalies.AnomalyVM -> Anomalies.Issue
    mergeAnomalyIntoIssue issue anomaly = 
      let newHash = anomaly.targetHash
          additionalBreaking = Anomalies.countBreakingChanges anomaly
          additionalIncremental = Anomalies.countIncrementalChanges anomaly
      in issue
        { Anomalies.anomalyHashes = issue.anomalyHashes <> V.singleton newHash
        , Anomalies.breakingChanges = issue.breakingChanges + additionalBreaking
        , Anomalies.incrementalChanges = issue.incrementalChanges + additionalIncremental
        , Anomalies.updatedAt = max issue.updatedAt anomaly.updatedAt
        }

-- Import BackgroundJobs to access the job types
import qualified BackgroundJobs