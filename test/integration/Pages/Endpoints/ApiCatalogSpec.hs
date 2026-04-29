module Pages.Endpoints.ApiCatalogSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value)
import Data.Default (def)
import Data.List (sort)
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
import OddJobs.Job (Job (..))
import Pages.Anomalies qualified as AnomalyList
import Pages.BodyWrapper (PageCtx (..))
import Pages.Endpoints qualified as ApiCatalog
import Pkg.Components.Table qualified as Table
import Pkg.TestUtils
import ProcessMessage (processMessages)
import BackgroundJobs qualified
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec (Spec, aroundAll, describe, expectationFailure, it, pendingWith, shouldBe, shouldSatisfy)
import Utils (toXXHash)


-- These helper functions are now in Pkg.TestUtils

-- Helper function to get endpoint stats
getEndpointStats :: TestResources -> Maybe Text -> Maybe Text -> IO (V.Vector ApiCatalog.EnpReqStatsVM)
getEndpointStats tr filterParam hostM = do
  (_, resp) <- testServant tr $
    ApiCatalog.endpointListGetH testPid Nothing Nothing Nothing filterParam hostM Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  case resp of
    ApiCatalog.EndpointsListPage (PageCtx _ tbl) -> pure tbl.rows
    _ -> error "Unexpected response from endpointListGetH"


-- | Fetch a specific page from the endpoints handler and return rows + total.
getEndpointsPage :: TestResources -> Text -> Int -> Int -> IO (V.Vector ApiCatalog.EnpReqStatsVM, Int)
getEndpointsPage tr host page perPage = do
  (_, resp) <- testServant tr $
    ApiCatalog.endpointListGetH testPid (Just (show page)) (Just (show perPage)) Nothing Nothing (Just host) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  case resp of
    ApiCatalog.EndpointsListPage (PageCtx _ tbl) ->
      pure (tbl.rows, maybe 0 (.totalCount) tbl.features.pagination)
    _ -> error "Unexpected response from endpointListGetH"

-- Helper function to verify endpoint creation
verifyEndpointsCreated :: TestResources -> IO ()
verifyEndpointsCreated tr = do
  endpoints <- withPool tr.trPool $ DBT.query [sql|
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
    [ ("m1", toStrict $ AE.encode reqMsg1)
    , ("m2", toStrict $ AE.encode reqMsg2)
    ]

spec :: Spec
spec = aroundAll withTestResources do
  describe "API Catalog and Endpoints" do
    it "returns empty list when no data exists" \tr -> do
      (_, catalogList) <- testServant tr $
          ApiCatalog.apiCatalogH testPid Nothing Nothing Nothing Nothing Nothing Nothing
      case catalogList of
        ApiCatalog.CatalogListPage (PageCtx _ tbl) ->
          length tbl.rows `shouldBe` 0
        _ -> expectationFailure "Expected CatalogListPage"

    it "creates endpoints from processed spans" \tr -> do
      -- First verify the demo project exists
      projectExists <- withPool tr.trPool $ DBT.query [sql|
        SELECT COUNT(*)
        FROM projects.projects
        WHERE id = ?
      |] (Only testPid) :: IO (V.Vector (Only Int))
      
      case projectExists of
        [Only 0] -> error $ "Demo project with ID " <> show testPid <> " does not exist in database"
        _ -> pass
      
      msgs <- prepareTestMessages
      processMessagesAndBackgroundJobs tr msgs
      
      -- Debug: Check if any spans were inserted at all
      allSpansCount <- withPool tr.trPool $ DBT.query [sql|
        SELECT COUNT(*)
        FROM otel_logs_and_spans
      |] () :: IO (V.Vector (Only Int))
      
      case allSpansCount of
        [Only totalCount] -> if totalCount == 0
          then error "No spans at all in otel_logs_and_spans table"
          else pass
        _ -> pass
      
      -- Debug: Check if spans were actually inserted for our project
      spanCount <- withPool tr.trPool $ DBT.query [sql|
        SELECT COUNT(*)
        FROM otel_logs_and_spans
        WHERE project_id = ? AND (name = 'monoscope.http' OR name = 'apitoolkit-http-span')
      |] (Only testPid) :: IO (V.Vector (Only Int))
      
      case spanCount of
        [Only count] -> if count == 0
          then error $ "No spans found in otel_logs_and_spans table after processing messages for project " <> show testPid
          else pass
        _ -> error "Unexpected span count result"
      
      verifyEndpointsCreated tr

    it "returns hosts list after processing messages" \tr -> do
      (_, catalogList) <- testServant tr $
          ApiCatalog.apiCatalogH testPid Nothing Nothing (Just "Incoming") Nothing Nothing Nothing
      case catalogList of
        ApiCatalog.CatalogListPage (PageCtx _ tbl) ->
          length tbl.rows `shouldBe` 2
        _ -> expectationFailure "Expected CatalogListPage"

    it "attributes events to the correct host (no cross-host inflation)" \tr -> do
      -- Each host got 100 spans in prepareTestMessages. A regression where the
      -- query joined spans to endpoints by (url_path, method) only — the bug
      -- that produced "every host shows the same total" — is caught here.
      (_, catalogList) <- testServant tr
        $ ApiCatalog.apiCatalogH testPid Nothing Nothing (Just "Incoming") Nothing Nothing Nothing
      case catalogList of
        ApiCatalog.CatalogListPage (PageCtx _ tbl) -> do
          let countOf host = sum
                [ he.eventCount
                | ApiCatalog.HostEventsVM _ he _ _ _ _ <- V.toList tbl.rows
                , he.host == host
                ]
          countOf "172.31.29.11" `shouldBe` 100
          countOf "api.test.com" `shouldBe` 100
        _ -> expectationFailure "Expected CatalogListPage"

    it "creates anomalies automatically via database triggers" \tr -> do
      -- Verify anomalies were created by triggers
      anomaliesCount <- withPool tr.trPool $ DBT.query [sql|
        SELECT COUNT(*)
        FROM apis.anomalies
        WHERE project_id = ?
      |] (Only testPid) :: IO (V.Vector (Only Int))
      
      case anomaliesCount of
        [Only count] -> count `shouldSatisfy` (> 0)
        _ -> error "Unexpected anomalies count result"

    it "processes anomaly background jobs to create issues" \tr -> do
      -- Get pending jobs and log them
      pendingJobs <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs

      -- Run only NewAnomaly jobs to create issues from anomalies
      _ <- runBackgroundJobsWhere frozenTime tr.trATCtx $ \case
        BackgroundJobs.NewAnomaly{} -> True
        _ -> False

      -- Verify issues were created
      issuesCount <- withPool tr.trPool $ DBT.query [sql|
        SELECT COUNT(*)
        FROM apis.issues
        WHERE project_id = ? AND issue_type = 'api_change'
      |] (Only testPid) :: IO (V.Vector (Only Int))

      case issuesCount of
        [Only count] -> count `shouldBe` 2
        _ -> error "Unexpected issues count result"

    it "returns endpoints in inbox filter after creating issues" \tr -> do
      -- Create test spans to populate materialized view
      createTestSpans tr testPid 10
      -- _ <- withPool tr.trPool $ refreshMaterializedView "apis.endpoint_request_stats"
      
      -- Test inbox filter without host
      inboxEndpoints <- getEndpointStats tr (Just "Endpoints") Nothing
      V.length inboxEndpoints `shouldBe` 2
      
      -- Test inbox filter with specific host
      host1Endpoints <- getEndpointStats tr (Just "Endpoints") (Just "172.31.29.11")
      V.length host1Endpoints `shouldBe` 1
      
      host2Endpoints <- getEndpointStats tr (Just "Endpoints") (Just "api.test.com")
      V.length host2Endpoints `shouldBe` 1

    it "handles empty host filter correctly" \tr -> do
      -- Test that empty host string is handled same as no host filter
      emptyHostEndpoints <- getEndpointStats tr (Just "Endpoints") (Just "")
      noHostEndpoints <- getEndpointStats tr (Just "Endpoints") Nothing
      V.length emptyHostEndpoints `shouldBe` V.length noHostEndpoints

    it "returns active endpoints after acknowledging issues" \tr -> do
      -- Acknowledge all endpoint issues
      _ <- withPool tr.trPool $ DBT.execute [sql|
        UPDATE apis.issues
        SET acknowledged_at = ?, acknowledged_by = ?
        WHERE project_id = ? AND issue_type = 'api_change'
      |] (frozenTime, Projects.UserId UUID.nil, testPid)
      
      -- Test active filter
      activeEndpoints <- getEndpointStats tr (Just "Endpoints") Nothing
      V.length activeEndpoints `shouldBe` 2
      
      -- Verify endpoint details
      let enp1 = (\(ApiCatalog.EnpReqStatsVM _ _ _ c) -> c) <$>
            Unsafe.fromJust $ find (\(ApiCatalog.EnpReqStatsVM _ _ _ c) -> c.urlPath == "/") activeEndpoints
      let enp2 = (\(ApiCatalog.EnpReqStatsVM _ _ _ c) -> c) <$>
            Unsafe.fromJust $ find (\(ApiCatalog.EnpReqStatsVM _ _ _ c) -> c.urlPath == "/api/v1/user/login") activeEndpoints

      enp1.endpointHash `shouldBe` toXXHash (testPid.toText <> "172.31.29.11" <> "GET" <> "/")
      enp2.endpointHash `shouldBe` toXXHash (testPid.toText <> "api.test.com" <> "POST" <> "/api/v1/user/login")

    -- it "handles anomaly bulk actions correctly" \tr -> do
    --   -- First ensure endpoints are created and all background jobs are processed
    --   msgs <- prepareTestMessages
    --   processMessagesAndBackgroundJobs tr msgs
    --   createTestSpans tr testPid 10
    --
    --   -- Process all background jobs multiple times to ensure anomalies are created
    --   _ <- runAllBackgroundJobs tr.trATCtx
    --   _ <- runAllBackgroundJobs tr.trATCtx
    --   _ <- runAllBackgroundJobs tr.trATCtx
    --
    --   -- Check what anomalies were created as issues (not endpoint type)
    --   nonEndpointIssues <- withPool tr.trPool $ DBT.query [sql|
    --     SELECT id, anomaly_type, target_hash
    --     FROM apis.issues
    --     WHERE project_id = ? AND anomaly_type != 'endpoint'
    --   |] (Only testPid) :: IO (V.Vector (AnomalyId, Text, Text))
    --
    --   -- If we have non-endpoint issues, test the anomaly list API
    --   if V.length nonEndpointIssues > 0 then do
    --     -- Get anomalies through the API
    --     pg <- testServant tr $ 
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
    --             _ <- testServant tr $ 
    --               AnomalyList.anomalyBulkActionsPostH testPid "acknowlege" bulkFrm
    --             pass
    --           Nothing -> error "Expected at least one anomaly"
    --       _ -> error "Unexpected response from anomaly list"
    --   else 
    --     -- Skip test if no non-endpoint issues were created
    --     pendingWith "No non-endpoint issues were created in this test run"

    it "creates shape and field anomalies alongside endpoint anomalies" \tr -> do
      -- First ensure endpoints are created and anomalies are generated
      msgs <- prepareTestMessages
      processMessagesAndBackgroundJobs tr msgs
      
      -- Check that we have anomalies for shapes and fields too
      anomalyTypes <- withPool tr.trPool $ DBT.query [sql|
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

    describe "host archiving" do
      let listHosts tr reqType filt = do
            (_, cat) <- testServant tr $ ApiCatalog.apiCatalogH testPid Nothing Nothing (Just reqType) Nothing Nothing filt
            case cat of
              ApiCatalog.CatalogListPage (PageCtx _ tbl) ->
                pure $ V.toList $ V.map (\(ApiCatalog.HostEventsVM _ he _ _ _ _) -> he.host) tbl.rows
              _ -> expectationFailure "Expected CatalogListPage" >> pure []
          bulk tr action reqType hosts = testServant tr $
            ApiCatalog.apiCatalogBulkActionH testPid action (Just reqType) (ApiCatalog.HostBulk{itemId = hosts})
          bulkAny tr action hosts = testServant tr $
            ApiCatalog.apiCatalogBulkActionH testPid action Nothing (ApiCatalog.HostBulk{itemId = hosts})

      it "archives an incoming host and surfaces it under the Archived tab" \tr -> do
        _ <- bulk tr "archive" "Incoming" ["172.31.29.11"]
        active <- listHosts tr "Incoming" Nothing
        archived <- listHosts tr "Incoming" (Just "Archived")
        active `shouldBe` ["api.test.com"]
        archived `shouldBe` ["172.31.29.11"]

      it "unarchive restores the host to the Active list" \tr -> do
        _ <- bulk tr "unarchive" "Incoming" ["172.31.29.11"]
        active <- listHosts tr "Incoming" Nothing
        archived <- listHosts tr "Incoming" (Just "Archived")
        sort active `shouldBe` ["172.31.29.11", "api.test.com"]
        archived `shouldBe` []

      it "archives an outgoing host independently of incoming" \tr -> do
        -- Seed an outgoing host directly (no endpoint, so we don't perturb
        -- endpoint-count assertions in surrounding tests).
        let outEp = (def :: Endpoints.Endpoint)
              { Endpoints.projectId = testPid
              , Endpoints.host = "api.upstream.example"
              , Endpoints.outgoing = True
              }
        runQueryEffect tr $ Endpoints.bulkInsertHosts (V.singleton outEp)
        _ <- bulk tr "archive" "Outgoing" ["api.upstream.example"]
        outActive <- listHosts tr "Outgoing" Nothing
        outArchived <- listHosts tr "Outgoing" (Just "Archived")
        outActive `shouldBe` []
        outArchived `shouldBe` ["api.upstream.example"]
        -- Direction isolation: incoming list still has both hosts.
        inActive <- listHosts tr "Incoming" Nothing
        sort inActive `shouldBe` ["172.31.29.11", "api.test.com"]

      it "unarchive from the Archived tab works without a request_type filter" \tr -> do
        -- Production path: user clicks Unarchive while on the Archived tab,
        -- where the form posts no request_type. The handler must apply across
        -- both directions (mirror of archiveHosts pid Nothing in the model).
        _ <- bulk tr "archive" "Incoming" ["172.31.29.11"]
        _ <- bulk tr "archive" "Outgoing" ["api.upstream.example"]
        archivedAny <- listHosts tr "Incoming" (Just "Archived")
        archivedAny `shouldSatisfy` ((/= 0) . length)
        _ <- bulkAny tr "unarchive" ["172.31.29.11", "api.upstream.example"]
        inActive <- listHosts tr "Incoming" Nothing
        outActive <- listHosts tr "Outgoing" Nothing
        sort inActive `shouldBe` ["172.31.29.11", "api.test.com"]
        outActive `shouldBe` ["api.upstream.example"]

      it "ignores empty host selections without error" \tr -> do
        (_, resp) <- bulk tr "archive" "Incoming" []
        case resp of
          ApiCatalog.CatalogBulkDone -> pass
        active <- listHosts tr "Incoming" Nothing
        sort active `shouldBe` ["172.31.29.11", "api.test.com"]

    it "filters endpoints by request type (incoming/outgoing)" \tr -> do
      -- All our test endpoints are incoming (outgoing = false)
      endpoints <- withPool tr.trPool $ DBT.query [sql|
        SELECT outgoing, COUNT(*)
        FROM apis.endpoints
        WHERE project_id = ?
        GROUP BY outgoing
      |] (Only testPid) :: IO (V.Vector (Bool, Int))

      -- Verify all are incoming
      case endpoints of
        [(False, count)] -> count `shouldBe` 2
        _ -> error "Expected all endpoints to be incoming"

    it "paginates the endpoints list with disjoint pages and accurate totalCount" \tr -> do
      -- Seeds extra endpoints under a synthetic host. Runs last because it
      -- inflates the project's endpoint count and would break the prior
      -- "filters endpoints by request type" assertion.
      let hostName = "paginate.example"
          mk i =
            (def :: Endpoints.Endpoint)
              { Endpoints.projectId = testPid
              , Endpoints.urlPath = "/p" <> show (i :: Int)
              , Endpoints.urlParams = AE.object []
              , Endpoints.method = "GET"
              , Endpoints.host = hostName
              , Endpoints.hash = toXXHash (testPid.toText <> hostName <> "GET" <> "/p" <> show i)
              , Endpoints.outgoing = False
              }
          eps = V.fromList $ map mk [1 .. 12]
      runQueryEffect tr $ Endpoints.bulkInsertEndpoints eps
      (page0Rows, total0) <- getEndpointsPage tr hostName 0 5
      (page1Rows, total1) <- getEndpointsPage tr hostName 1 5
      (page2Rows, total2) <- getEndpointsPage tr hostName 2 5
      total0 `shouldBe` 12
      total1 `shouldBe` 12
      total2 `shouldBe` 12
      V.length page0Rows `shouldBe` 5
      V.length page1Rows `shouldBe` 5
      V.length page2Rows `shouldBe` 2 -- 12 mod 5
      let hashes vm = [enp.endpointHash | ApiCatalog.EnpReqStatsVM _ _ _ enp <- V.toList vm]
          all3 = hashes page0Rows <> hashes page1Rows <> hashes page2Rows
      length all3 `shouldBe` 12
      length (ordNub all3) `shouldBe` 12 -- pages are disjoint
