module Pages.Endpoints.ApiCatalogSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.List (find)
import Data.Int (Int64)
import Data.Time (UTCTime, ZonedTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..), query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import OddJobs.Job (Job (..))
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper (PageCtx (..))
import Pages.Endpoints.ApiCatalog qualified as ApiCatalog
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.TestUtils
import ProcessMessage (processMessages)
import BackgroundJobs (processFiveMinuteSpans, processBackgroundJob, processOneMinuteErrors)
import BackgroundJobs qualified
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec (Spec, aroundAll, describe, it, pendingWith, shouldBe, shouldSatisfy)
import Utils (toXXHash)

testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil

-- These helper functions are now in Pkg.TestUtils

-- Helper function to get endpoint stats
getEndpointStats :: TestResources -> Maybe Text -> Maybe Text -> IO (V.Vector ApiCatalog.EnpReqStatsVM)
getEndpointStats TestResources{..} filterParam hostM = do
  resp <- toServantResponse trATCtx trSessAndHeader trLogger $ 
    ApiCatalog.endpointListGetH testPid Nothing Nothing filterParam hostM Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  case resp of
    ApiCatalog.EndpointsListPage (PageCtx _ (ItemsList.ItemsPage _ enpList)) -> pure enpList
    _ -> error "Unexpected response from endpointListGetH"

-- Helper function to verify endpoint creation
verifyEndpointsCreated :: TestResources -> IO ()
verifyEndpointsCreated TestResources{..} = do
  endpoints <- withPool trPool $ DBT.query [sql|
    SELECT url_path, method, host, hash
    FROM apis.endpoints
    WHERE project_id = ?
  |] (Only testPid) :: IO (V.Vector (Text, Text, Text, Text))
  
  V.length endpoints `shouldBe` 2
  
  let endpoint1 = Unsafe.fromJust $ V.find (\(path, _, _, _) -> path == "/") endpoints
  let endpoint2 = Unsafe.fromJust $ V.find (\(path, _, _, _) -> path == "/api/v1/user/login") endpoints
  
  let (_, method1, host1, hash1) = endpoint1
  let (_, method2, host2, hash2) = endpoint2
  
  method1 `shouldBe` "GET"
  host1 `shouldBe` "172.31.29.11"
  hash1 `shouldBe` toXXHash (testPid.toText <> "172.31.29.11" <> "GET" <> "/")
  
  method2 `shouldBe` "POST"
  host2 `shouldBe` "api.test.com"
  hash2 `shouldBe` toXXHash (testPid.toText <> "api.test.com" <> "POST" <> "/api/v1/user/login")

-- Helper function to prepare test messages
prepareTestMessages :: IO [(Text, ByteString)]
prepareTestMessages = do
  currentTime <- getCurrentTime
  let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
  let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
  let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
  pure $ concat $ replicate 100
    [ ("m1", BL.toStrict $ AE.encode reqMsg1)
    , ("m2", BL.toStrict $ AE.encode reqMsg2)
    ]

spec :: Spec
spec = aroundAll withTestResources do
  describe "API Catalog and Endpoints" do
    it "returns empty list when no data exists" \tr@TestResources{..} -> do
      PageCtx _ (ItemsList.ItemsPage _ hostsAndEvents) <- 
        toServantResponse trATCtx trSessAndHeader trLogger $ 
          ApiCatalog.apiCatalogH testPid Nothing Nothing Nothing
      length hostsAndEvents `shouldBe` 0

    it "creates endpoints from processed spans" \tr -> do
      msgs <- prepareTestMessages
      processMessagesAndBackgroundJobs tr msgs
      verifyEndpointsCreated tr

    it "returns hosts list after processing messages" \tr@TestResources{..} -> do
      PageCtx _ (ItemsList.ItemsPage _ hostsAndEvents) <- 
        toServantResponse trATCtx trSessAndHeader trLogger $ 
          ApiCatalog.apiCatalogH testPid Nothing Nothing (Just "Incoming")
      length hostsAndEvents `shouldBe` 2

    it "creates anomalies automatically via database triggers" \tr@TestResources{..} -> do
      -- Verify anomalies were created by triggers
      anomaliesCount <- withPool trPool $ DBT.query [sql|
        SELECT COUNT(*)
        FROM apis.anomalies
        WHERE project_id = ?
      |] (Only testPid) :: IO (V.Vector (Only Int))
      
      case anomaliesCount of
        [(Only count)] -> count `shouldSatisfy` (> 0)
        _ -> error "Unexpected anomalies count result"

    it "processes anomaly background jobs to create issues" \tr@TestResources{..} -> do
      -- Process anomaly jobs
      processEndpointAnomalyJobs tr
      _ <- runAllBackgroundJobs trATCtx
      
      -- Verify issues were created
      issuesCount <- withPool trPool $ DBT.query [sql|
        SELECT COUNT(*)
        FROM apis.issues
        WHERE project_id = ? AND anomaly_type = 'endpoint'
      |] (Only testPid) :: IO (V.Vector (Only Int))
      
      case issuesCount of
        [(Only count)] -> count `shouldBe` 2
        _ -> error "Unexpected issues count result"

    it "returns endpoints in inbox filter after creating issues" \tr -> do
      -- Create request dumps to populate materialized view
      createRequestDumps tr testPid 10
      _ <- withPool tr.trPool $ refreshMaterializedView "apis.endpoint_request_stats"
      
      -- Test inbox filter without host
      inboxEndpoints <- getEndpointStats tr (Just "Inbox") Nothing
      V.length inboxEndpoints `shouldBe` 2
      
      -- Test inbox filter with specific host
      host1Endpoints <- getEndpointStats tr (Just "Inbox") (Just "172.31.29.11")
      V.length host1Endpoints `shouldBe` 1
      
      host2Endpoints <- getEndpointStats tr (Just "Inbox") (Just "api.test.com")
      V.length host2Endpoints `shouldBe` 1

    it "handles empty host filter correctly" \tr -> do
      -- Test that empty host string is handled same as no host filter
      emptyHostEndpoints <- getEndpointStats tr (Just "Inbox") (Just "")
      noHostEndpoints <- getEndpointStats tr (Just "Inbox") Nothing
      V.length emptyHostEndpoints `shouldBe` V.length noHostEndpoints

    it "returns active endpoints after acknowledging issues" \tr@TestResources{..} -> do
      -- Acknowledge all endpoint issues
      _ <- withPool trPool $ DBT.execute [sql|
        UPDATE apis.issues 
        SET acknowleged_at = NOW(), acknowleged_by = ?
        WHERE project_id = ? AND anomaly_type = 'endpoint'
      |] (Users.UserId UUID.nil, testPid)
      
      -- Test active filter
      activeEndpoints <- getEndpointStats tr (Just "Active") Nothing
      V.length activeEndpoints `shouldBe` 2
      
      -- Verify endpoint details
      let enp1 = (\(ApiCatalog.EnpReqStatsVM _ _ c) -> c) <$> 
            Unsafe.fromJust $ find (\(ApiCatalog.EnpReqStatsVM _ _ c) -> c.urlPath == "/") activeEndpoints
      let enp2 = (\(ApiCatalog.EnpReqStatsVM _ _ c) -> c) <$> 
            Unsafe.fromJust $ find (\(ApiCatalog.EnpReqStatsVM _ _ c) -> c.urlPath == "/api/v1/user/login") activeEndpoints
      
      enp1.endpointHash `shouldBe` toXXHash (testPid.toText <> "172.31.29.11" <> "GET" <> "/")
      enp2.endpointHash `shouldBe` toXXHash (testPid.toText <> "api.test.com" <> "POST" <> "/api/v1/user/login")

    -- it "handles anomaly bulk actions correctly" \tr@TestResources{..} -> do
    --   -- First ensure endpoints are created and all background jobs are processed
    --   msgs <- prepareTestMessages
    --   processMessagesAndBackgroundJobs tr msgs
    --   createRequestDumps tr testPid 10
    --
    --   -- Process all background jobs multiple times to ensure anomalies are created
    --   _ <- runAllBackgroundJobs trATCtx
    --   _ <- runAllBackgroundJobs trATCtx
    --   _ <- runAllBackgroundJobs trATCtx
    --
    --   -- Check what anomalies were created as issues (not endpoint type)
    --   nonEndpointIssues <- withPool trPool $ DBT.query [sql|
    --     SELECT id, anomaly_type, target_hash
    --     FROM apis.issues
    --     WHERE project_id = ? AND anomaly_type != 'endpoint'
    --   |] (Only testPid) :: IO (V.Vector (AnomalyId, Text, Text))
    --
    --   -- If we have non-endpoint issues, test the anomaly list API
    --   if V.length nonEndpointIssues > 0 then do
    --     -- Get anomalies through the API
    --     pg <- toServantResponse trATCtx trSessAndHeader trLogger $ 
    --       AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    --
    --     case pg of
    --       AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> do
    --         -- We should have anomalies (shape, field, or format)
    --         V.length anomalies `shouldSatisfy` (> 0)
    --
    --         -- Test bulk acknowledge with first anomaly
    --         case V.headM anomalies of
    --           Just (AnomalyList.IssueVM _ _ _ firstAnomaly) -> do
    --             let bulkFrm = AnomalyList.AnomalyBulk{anomalyId = [anomalyIdText firstAnomaly.id]}
    --             _ <- toServantResponse trATCtx trSessAndHeader trLogger $ 
    --               AnomalyList.anomalyBulkActionsPostH testPid "acknowlege" bulkFrm
    --             pass
    --           Nothing -> error "Expected at least one anomaly"
    --       _ -> error "Unexpected response from anomaly list"
    --   else 
    --     -- Skip test if no non-endpoint issues were created
    --     pendingWith "No non-endpoint issues were created in this test run"

    it "creates shape and field anomalies alongside endpoint anomalies" \tr@TestResources{..} -> do
      -- First ensure endpoints are created and anomalies are generated
      msgs <- prepareTestMessages
      processMessagesAndBackgroundJobs tr msgs
      
      -- Check that we have anomalies for shapes and fields too
      anomalyTypes <- withPool trPool $ DBT.query [sql|
        SELECT DISTINCT anomaly_type::text, COUNT(*)
        FROM apis.anomalies
        WHERE project_id = ?
        GROUP BY anomaly_type
        ORDER BY anomaly_type
      |] (Only testPid) :: IO (V.Vector (Text, Int))
      
      -- We should have at least endpoint, shape, field, and format anomalies
      V.length anomalyTypes `shouldSatisfy` (>= 4)
      
      -- Verify we have anomalies of each expected type
      let hasType t = V.any (\(atype, _) -> atype == t) anomalyTypes
      hasType "endpoint" `shouldBe` True
      hasType "shape" `shouldBe` True
      hasType "field" `shouldBe` True
      hasType "format" `shouldBe` True

    it "filters endpoints by request type (incoming/outgoing)" \tr@TestResources{..} -> do
      -- All our test endpoints are incoming (outgoing = false)
      endpoints <- withPool trPool $ DBT.query [sql|
        SELECT outgoing, COUNT(*)
        FROM apis.endpoints
        WHERE project_id = ?
        GROUP BY outgoing
      |] (Only testPid) :: IO (V.Vector (Bool, Int))
      
      -- Verify all are incoming
      case endpoints of
        [(False, count)] -> count `shouldBe` 2
        _ -> error "Expected all endpoints to be incoming"
