#!/usr/bin/env stack
-- stack script --resolver lts-22.24 --package postgresql-simple --package time --package uuid --package text --package vector --package aeson

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (forM_, void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import qualified Data.Vector as V
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import qualified Data.Aeson as AE
import System.Environment (getEnv)

-- | Script to recreate all issues by processing existing anomalies
-- This groups anomalies by endpoint and creates one issue per endpoint
main :: IO ()
main = do
  putStrLn "Starting issue recreation script..."
  
  -- Get database connection string from environment
  connStr <- getEnv "DATABASE_URL"
  conn <- connectPostgreSQL (fromString connStr)
  
  -- Start transaction
  begin conn
  
  -- Step 1: Delete all existing issues
  putStrLn "Deleting existing issues..."
  deleteCount <- execute_ conn [sql| 
    DELETE FROM apis.issues WHERE anomaly_type != 'runtime_exception'
  |]
  putStrLn $ "Deleted " ++ show deleteCount ++ " existing issues"
  
  -- Step 2: Get all unacknowledged anomalies grouped by project
  putStrLn "Fetching anomalies to process..."
  anomalies <- query_ conn [sql|
    SELECT DISTINCT ON (project_id) project_id 
    FROM apis.anomalies 
    WHERE acknowleged_at IS NULL 
      AND anomaly_type IN ('endpoint', 'shape', 'format')
    ORDER BY project_id
  |] :: IO [(Text,)]
  
  putStrLn $ "Found " ++ show (length anomalies) ++ " projects with anomalies"
  
  -- Step 3: Process each project
  forM_ anomalies $ \(projectIdText,) -> do
    let projectId = UUID.fromText projectIdText
    case projectId of
      Nothing -> putStrLn $ "Invalid project ID: " ++ T.unpack projectIdText
      Just pid -> processProjectAnomalies conn pid
  
  -- Step 4: Queue all issues for LLM enhancement
  putStrLn "Queuing issues for LLM enhancement..."
  issueIds <- query_ conn [sql|
    SELECT id, project_id
    FROM apis.issues
    WHERE llm_enhanced_at IS NULL
      AND acknowleged_at IS NULL
      AND archived_at IS NULL
    ORDER BY project_id
  |] :: IO [(UUID, UUID)]
  
  -- Create background jobs for enhancement (batched by project)
  let groupedByProject = groupBy (\(_, p1) (_, p2) -> p1 == p2) issueIds
  forM_ groupedByProject $ \projectIssues -> do
    case projectIssues of
      [] -> pure ()
      ((_, pid):_) -> do
        let issueIdList = map fst projectIssues
        let jobData = AE.object 
              [ "projectId" AE..= pid
              , "issueIds" AE..= issueIdList
              ]
        void $ execute conn [sql|
          INSERT INTO background_jobs (type, payload, created_at, updated_at)
          VALUES ('EnhanceIssuesWithLLM', ?::jsonb, NOW(), NOW())
        |] (Only jobData)
  
  -- Commit transaction
  commit conn
  close conn
  
  putStrLn "Issue recreation completed successfully!"

-- | Process anomalies for a single project
processProjectAnomalies :: Connection -> UUID -> IO ()
processProjectAnomalies conn projectId = do
  putStrLn $ "Processing project: " ++ show projectId
  
  -- Get all anomalies for this project grouped by endpoint
  anomaliesByEndpoint <- query conn [sql|
    SELECT 
      CASE 
        WHEN anomaly_type = 'endpoint' THEN target_hash
        ELSE SUBSTRING(target_hash, 1, 8)
      END as endpoint_hash,
      array_agg(DISTINCT target_hash) as anomaly_hashes,
      anomaly_type,
      MIN(created_at) as first_created,
      MAX(updated_at) as last_updated
    FROM apis.anomalies
    WHERE project_id = ?
      AND acknowleged_at IS NULL
      AND anomaly_type IN ('endpoint', 'shape', 'format')
    GROUP BY 
      CASE 
        WHEN anomaly_type = 'endpoint' THEN target_hash
        ELSE SUBSTRING(target_hash, 1, 8)
      END,
      anomaly_type
  |] (Only projectId) :: IO [(Text, V.Vector Text, Text, ZonedTime, ZonedTime)]
  
  putStrLn $ "  Found " ++ show (length anomaliesByEndpoint) ++ " unique endpoints with anomalies"
  
  -- Create one issue per endpoint
  forM_ anomaliesByEndpoint $ \(endpointHash, anomalyHashes, anomalyType, firstCreated, lastUpdated) -> do
    issueId <- UUIDV4.nextRandom
    
    -- Get sample payloads (up to 10)
    payloads <- query conn [sql|
      SELECT DISTINCT ON (rq.request_body_hash) 
        rq.request_body,
        rq.response_body
      FROM apis.request_dumps rq
      WHERE rq.project_id = ?
        AND (
          (? = 'endpoint' AND rq.endpoint_hash = ?)
          OR (? != 'endpoint' AND rq.endpoint_hash = ? AND (rq.shape_hash = ANY(?) OR ANY(rq.format_hashes) = ANY(?)))
        )
      LIMIT 10
    |] (projectId, anomalyType, endpointHash, anomalyType, endpointHash, anomalyHashes, anomalyHashes)
      :: IO [(Maybe AE.Value, Maybe AE.Value)]
    
    let requestPayloads = map fst payloads
        responsePayloads = map snd payloads
    
    -- Determine basic values
    let breakingChanges = if anomalyType == "shape" then V.length anomalyHashes else 0
        incrementalChanges = if anomalyType == "format" then V.length anomalyHashes else 0
        title = case anomalyType of
          "endpoint" -> "New API endpoint discovered"
          "shape" -> "API structure has changed"
          "format" -> "Field format has changed"
          _ -> "API change detected"
    
    -- Insert the grouped issue
    void $ execute conn [sql|
      INSERT INTO apis.issues (
        id, created_at, updated_at, project_id, anomaly_type, target_hash,
        issue_data, endpoint_id, title, service, critical, breaking_changes,
        incremental_changes, affected_payloads, affected_clients, estimated_requests,
        migration_complexity, recommended_action, request_payloads, response_payloads,
        anomaly_hashes, endpoint_hash
      ) VALUES (
        ?, ?, ?, ?, ?::apis.anomaly_type, ?,
        '{}', NULL, ?, 'Unknown', false, ?,
        ?, 0, 0, 'N/A',
        'medium', 'Review the changes and update your integration accordingly.', ?::jsonb, ?::jsonb,
        ?, ?
      )
    |] ( issueId, firstCreated, lastUpdated, projectId, anomalyType, endpointHash
       , title, breakingChanges
       , incrementalChanges, AE.toJSON requestPayloads, AE.toJSON responsePayloads
       , anomalyHashes, endpointHash
       )
    
    putStrLn $ "    Created issue for endpoint: " ++ T.unpack endpointHash

-- Helper to group by a key
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy eq (x:xs) = (x:ys) : groupBy eq zs
  where (ys,zs) = span (eq x) xs