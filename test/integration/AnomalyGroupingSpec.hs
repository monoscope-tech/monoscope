module AnomalyGroupingSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.KeyMap qualified as AEKM
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock (getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..), query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import OddJobs.Job (Job (..))
import BackgroundJobs (processFiveMinuteSpans, processBackgroundJob, processOneMinuteErrors)
import BackgroundJobs qualified
import Pkg.TestUtils
import ProcessMessage (processMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import Control.Monad.IO.Class (liftIO)
import RequestMessages (RequestMessage (..), replaceNullChars, valueToFields)
import Test.Hspec (Spec, aroundAll, describe, it, pendingWith, shouldBe, shouldSatisfy)
import Utils (toXXHash)


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Anomaly Grouping and Enhancement" do
    it "should group multiple anomalies by endpoint into a single issue" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      
      -- Send multiple shape changes for the same endpoint
      let baseMsg = createTestMessage nowTxt "/api/users" "GET" 
            [aesonQQ|{"id": 1, "name": "John"}|]
      let shapeChange1 = createTestMessage nowTxt "/api/users" "GET" 
            [aesonQQ|{"id": 1, "name": "John", "email": "john@example.com"}|]
      let shapeChange2 = createTestMessage nowTxt "/api/users" "GET" 
            [aesonQQ|{"id": 1, "name": "John", "email": "john@example.com", "role": "admin"}|]
      
      let msgs = 
            [ ("base", BL.toStrict $ AE.encode $ Unsafe.fromJust $ convert baseMsg)
            , ("change1", BL.toStrict $ AE.encode $ Unsafe.fromJust $ convert shapeChange1)
            , ("change2", BL.toStrict $ AE.encode $ Unsafe.fromJust $ convert shapeChange2)
            ]
      
      -- Process messages
      processMessagesAndBackgroundJobs TestResources{..} msgs
      processAllBackgroundJobsMultipleTimes TestResources{..}
      
      -- Process anomaly jobs
      processEndpointAnomalyJobs TestResources{..}
      _ <- runAllBackgroundJobs trATCtx
      processShapeAndFieldAnomalyJobs TestResources{..}
      _ <- runAllBackgroundJobs trATCtx
      
      -- Check that only one issue was created for multiple shape changes
      issues <- withPool trPool $ DBT.query [sql|
        SELECT id, anomaly_hashes, breaking_changes, incremental_changes, endpoint_hash
        FROM apis.issues 
        WHERE project_id = ? AND anomaly_type = 'shape'
      |] (Only testPid) :: IO (V.Vector (Anomalies.AnomalyId, V.Vector Text, Int, Int, Text))
      
      -- Should have only one issue that groups all shape anomalies
      V.length issues `shouldBe` 1
      
      let (issueId, anomalyHashes, breakingChanges, incrementalChanges, endpointHash) = V.head issues
      -- Should have multiple anomaly hashes in the single issue
      V.length anomalyHashes `shouldSatisfy` (>= 2)
      -- Should have counted changes
      (breakingChanges + incrementalChanges) `shouldSatisfy` (> 0)

    it "should create separate issues for different endpoints" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      
      -- Send shape changes for different endpoints
      let endpoint1Base = createTestMessage nowTxt "/api/users" "GET" 
            [aesonQQ|{"id": 1}|]
      let endpoint1Change = createTestMessage nowTxt "/api/users" "GET" 
            [aesonQQ|{"id": 1, "name": "John"}|]
      let endpoint2Base = createTestMessage nowTxt "/api/products" "GET" 
            [aesonQQ|{"id": 1, "price": 100}|]
      let endpoint2Change = createTestMessage nowTxt "/api/products" "GET" 
            [aesonQQ|{"id": 1, "price": 100, "description": "Product"}|]
      
      let msgs = 
            [ ("e1base", BL.toStrict $ AE.encode $ Unsafe.fromJust $ convert endpoint1Base)
            , ("e1change", BL.toStrict $ AE.encode $ Unsafe.fromJust $ convert endpoint1Change)
            , ("e2base", BL.toStrict $ AE.encode $ Unsafe.fromJust $ convert endpoint2Base)
            , ("e2change", BL.toStrict $ AE.encode $ Unsafe.fromJust $ convert endpoint2Change)
            ]
      
      -- Process messages
      processMessagesAndBackgroundJobs TestResources{..} msgs
      processAllBackgroundJobsMultipleTimes TestResources{..}
      
      -- Process anomaly jobs
      processEndpointAnomalyJobs TestResources{..}
      _ <- runAllBackgroundJobs trATCtx
      processShapeAndFieldAnomalyJobs TestResources{..}
      _ <- runAllBackgroundJobs trATCtx
      
      -- Check that separate issues were created for different endpoints
      issues <- withPool trPool $ DBT.query [sql|
        SELECT endpoint_hash, COUNT(*) as issue_count
        FROM apis.issues 
        WHERE project_id = ? AND anomaly_type = 'shape'
        GROUP BY endpoint_hash
      |] (Only testPid) :: IO (V.Vector (Text, Int))
      
      -- Should have exactly 2 different endpoint hashes
      V.length issues `shouldBe` 2
      -- Each endpoint should have exactly one issue
      V.all (\(_, count) -> count == 1) issues `shouldBe` True

    it "should allow new issue creation after acknowledging previous issue" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      
      -- Get existing shape issue
      existingIssues <- withPool trPool $ DBT.query [sql|
        SELECT id, endpoint_hash FROM apis.issues 
        WHERE project_id = ? AND anomaly_type = 'shape' AND acknowleged_at IS NULL
        LIMIT 1
      |] (Only testPid) :: IO (V.Vector (Anomalies.AnomalyId, Text))
      
      case V.headM existingIssues of
        Nothing -> pendingWith "No existing shape issue found"
        Just (issueId, endpointHash) -> do
          -- Acknowledge the issue
          _ <- withPool trPool $ DBT.execute [sql|
            UPDATE apis.issues SET acknowleged_at = NOW(), acknowleged_by = ?
            WHERE id = ?
          |] (Users.UserId UUID.nil, issueId)
          
          -- Send new shape change for same endpoint
          let newChange = createTestMessage nowTxt "/api/users" "GET" 
                [aesonQQ|{"id": 1, "name": "John", "age": 30, "address": "123 Main St"}|]
          let msgs = [("newchange", BL.toStrict $ AE.encode $ Unsafe.fromJust $ convert newChange)]
          
          -- Process messages
          processMessagesAndBackgroundJobs TestResources{..} msgs
          processAllBackgroundJobsMultipleTimes TestResources{..}
          processShapeAndFieldAnomalyJobs TestResources{..}
          _ <- runAllBackgroundJobs trATCtx
          
          -- Check that a new issue was created for the same endpoint
          newIssues <- withPool trPool $ DBT.query [sql|
            SELECT id FROM apis.issues 
            WHERE project_id = ? 
              AND anomaly_type = 'shape' 
              AND endpoint_hash = ?
              AND acknowleged_at IS NULL
          |] (Only testPid, endpointHash) :: IO (V.Vector (Only Anomalies.AnomalyId))
          
          -- Should have created a new issue after acknowledgment
          V.length newIssues `shouldBe` 1

    it "should enhance issues with LLM when API key is configured" \TestResources{..} -> do
      -- Check if OpenAI API key is configured
      let hasApiKey = not $ T.null trATCtx.config.openaiApiKey
      
      if not hasApiKey
        then pendingWith "OpenAI API key not configured"
        else do
          -- Get an unenhanced issue
          unenhancedIssues <- withPool trPool $ DBT.query [sql|
            SELECT id FROM apis.issues 
            WHERE project_id = ? 
              AND llm_enhanced_at IS NULL
              AND anomaly_type != 'runtime_exception'
            LIMIT 1
          |] (Only testPid) :: IO (V.Vector (Only Anomalies.AnomalyId))
          
          case V.headM unenhancedIssues of
            Nothing -> pendingWith "No unenhanced issues found"
            Just (Only issueId) -> do
              -- Create and run enhancement job
              currentTime <- liftIO getCurrentTime
              let enhanceJob = BackgroundJobs.EnhanceIssuesWithLLM testPid (V.singleton issueId)
              processBackgroundJob trATCtx (Job 0 enhanceJob "" "" 0 0 currentTime currentTime) enhanceJob
              
              -- Check that the issue was enhanced
              enhancedIssue <- withPool trPool $ DBT.queryOne [sql|
                SELECT title, recommended_action, migration_complexity, llm_enhanced_at
                FROM apis.issues 
                WHERE id = ?
              |] (Only issueId) :: IO (Maybe (Text, Text, Text, Maybe UTCTime))
              
              case enhancedIssue of
                Nothing -> error "Issue not found after enhancement"
                Just (title, recommendedAction, complexity, enhancedAt) -> do
                  -- Check that LLM enhancement was applied
                  isJust enhancedAt `shouldBe` True
                  -- Title should be more descriptive than default
                  T.length title `shouldSatisfy` (> 20)
                  -- Recommended action should be meaningful
                  T.length recommendedAction `shouldSatisfy` (> 30)
                  -- Complexity should be one of the valid values
                  complexity `shouldSatisfy` (`elem` ["low", "medium", "high"])

    it "should classify runtime exceptions as critical" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      
      -- Create a message with runtime error
      let errorMsg = createTestMessageWithError nowTxt "/api/users" "GET" 
            [aesonQQ|{"error": "Internal Server Error"}|]
            [aesonQQ|{
              "hash": "error123",
              "error_type": "NullPointerException",
              "message": "Cannot read property 'id' of null",
              "stack_trace": "at UserService.getUser (user.js:42)",
              "when": #{nowTxt}
            }|]
      
      let msgs = [("error", BL.toStrict $ AE.encode $ Unsafe.fromJust $ convert errorMsg)]
      
      -- Process messages and errors
      processMessagesAndBackgroundJobs TestResources{..} msgs
      processAllBackgroundJobsMultipleTimes TestResources{..}
      
      -- Process error detection using background job
      let errorProcessingJob = BackgroundJobs.OneMinuteErrorProcessing currentTime
      processBackgroundJob trATCtx (Job 0 errorProcessingJob "" "" 0 0 currentTime currentTime) errorProcessingJob
      _ <- runAllBackgroundJobs trATCtx
      
      -- Check that runtime exception issue was created as critical
      runtimeIssues <- withPool trPool $ DBT.query [sql|
        SELECT id, critical, title
        FROM apis.issues 
        WHERE project_id = ? AND anomaly_type = 'runtime_exception'
      |] (Only testPid) :: IO (V.Vector (Anomalies.AnomalyId, Bool, Text))
      
      V.length runtimeIssues `shouldSatisfy` (>= 1)
      -- All runtime exceptions should be marked as critical
      V.all (\(_, critical, _) -> critical) runtimeIssues `shouldBe` True


-- Helper function to create test messages
createTestMessage :: Text -> Text -> Text -> AE.Value -> AE.Value
createTestMessage timestamp path method body =
  [aesonQQ|{
    "duration": 476434,
    "host": "api.example.com",
    "method": #{method},
    "path_params": {},
    "project_id": "00000000-0000-0000-0000-000000000000",
    "proto_minor": 1,
    "proto_major": 1,
    "query_params": {},
    "raw_url": #{path},
    "referer": "",
    "request_body": #{AE.encode body},
    "request_headers": {
      "connection": ["keep-alive"],
      "host": ["api.example.com"],
      "content-type": ["application/json"]
    },
    "response_body": #{AE.encode body},
    "response_headers": {
      "content-type": ["application/json"],
      "content-length": ["100"]
    },
    "sdk_type": "NodeJS",
    "status_code": 200,
    "timestamp": #{timestamp},
    "url_path": #{path},
    "errors": [],
    "tags": []
  }|]

createTestMessageWithError :: Text -> Text -> Text -> AE.Value -> AE.Value -> AE.Value
createTestMessageWithError timestamp path method body errorData =
  let baseMsg = createTestMessage timestamp path method body
  in case baseMsg of
    AE.Object obj -> AE.Object $ obj <> AEKM.fromList [("errors", AE.Array $ V.singleton errorData)]
    _ -> baseMsg