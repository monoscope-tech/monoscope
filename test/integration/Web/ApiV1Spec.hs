module Web.ApiV1Spec (spec) where

import Control.Lens ((^.), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _Array, _Number, _Object)
import Data.Default (def)
import Data.Map qualified as Map
import Data.OpenApi (OpenApi, info, title, version)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Models.Apis.Monitors qualified as Monitors
import Models.Telemetry.Schema qualified as Schema
import Pages.Charts.Charts qualified as Charts
import Pages.Charts.Types (MetricsData (..))
import Pages.LogExplorer.Log qualified as Log
import Pkg.TestUtils
import Relude
import Servant (NoContent (..))
import System.Types (ATBaseCtx)
import Test.Hspec
import Web.ApiHandlers qualified as ApiH
import Web.ApiTypes qualified as ApiT
import Web.Auth (resolveApiKeyProject)
import Web.Routes (apiV1OpenApiSpec)


specJson :: AE.Value
specJson = AE.toJSON apiV1OpenApiSpec


spec :: Spec
spec = aroundAll withTestResources do
  describe "API v1" do
    describe "OpenAPI spec" do
      it "round-trips through JSON and has correct metadata" $ \_tr -> do
        AE.eitherDecode @OpenApi (AE.encode apiV1OpenApiSpec) `shouldSatisfy` isRight
        (apiV1OpenApiSpec ^. info . title) `shouldBe` "Monoscope API"
        (apiV1OpenApiSpec ^. info . version) `shouldBe` "1.0"

      it "has server base path and security definitions" $ \_tr -> do
        (specJson ^? key "servers" . _Array) `shouldSatisfy` \case
          Just arr -> not (null arr)
          Nothing -> False
        (specJson ^? key "components" . key "securitySchemes" . key "BearerAuth") `shouldSatisfy` isJust
        (specJson ^? key "security" . _Array) `shouldSatisfy` \case
          Just arr -> not (null arr)
          Nothing -> False

      it "declares all expected endpoint paths" $ \_tr -> do
        (specJson ^? key "paths" . key "/events") `shouldSatisfy` isJust
        (specJson ^? key "paths" . key "/metrics") `shouldSatisfy` isJust
        (specJson ^? key "paths" . key "/schema") `shouldSatisfy` isJust
        (specJson ^? key "paths" . key "/monitors") `shouldSatisfy` isJust

      it "includes schema definitions for response types" $ \_tr -> do
        let schemas = specJson ^? key "components" . key "schemas" . _Object
        schemas `shouldSatisfy` isJust
        (specJson ^? key "components" . key "schemas" . key "LogResult") `shouldSatisfy` isJust
        (specJson ^? key "components" . key "schemas" . key "LogResult" . key "properties" . _Object) `shouldSatisfy` \case
          Just obj -> not (null obj)
          Nothing -> False
        (specJson ^? key "components" . key "schemas" . key "TraceTreeEntry") `shouldSatisfy` isJust
        (specJson ^? key "components" . key "schemas" . key "MetricsData") `shouldSatisfy` isJust
        (specJson ^? key "components" . key "schemas" . key "Schema") `shouldSatisfy` isJust
        (specJson ^? key "components" . key "schemas" . key "QueryMonitor") `shouldSatisfy` isJust

    describe "API key auth" do
      it "resolves valid keys and rejects invalid ones" $ \tr -> do
        apiKey <- createTestAPIKey tr testPid "test-v1-key"
        result <- runQueryEffect tr $ resolveApiKeyProject apiKey
        result `shouldBe` Just testPid

        invalidResult <- runQueryEffect tr $ resolveApiKeyProject "invalid-key"
        invalidResult `shouldBe` Nothing

        emptyResult <- runQueryEffect tr $ resolveApiKeyProject ""
        emptyResult `shouldBe` Nothing

    describe "Schema endpoint" do
      it "returns schema with expected fields and valid JSON round-trip" $ \_tr -> do
        let schema = Schema.telemetrySchema
        schema.fields `shouldSatisfy` (not . null)
        Map.lookup "timestamp" schema.fields `shouldSatisfy` isJust
        Map.lookup "status_code" schema.fields `shouldSatisfy` isJust
        Map.lookup "body" schema.fields `shouldSatisfy` isJust
        AE.eitherDecode @Schema.Schema (AE.encode schema) `shouldBe` Right schema

    describe "Monitors endpoint" do
      it "returns JSON-serializable monitors list for project" $ \tr -> do
        result <- runQueryEffect tr $ Monitors.queryMonitorsAll testPid
        AE.eitherDecode @[Monitors.QueryMonitor] (AE.encode result) `shouldSatisfy` isRight

    describe "Events endpoint" do
      it "returns valid LogResult with expected JSON structure" $ \tr -> do
        result <-
          toBaseServantResponse tr.trATCtx tr.trLogger
            $ Log.queryEvents testPid (Just "") (Just "1h") Nothing Nothing Nothing Nothing
        let json = AE.toJSON result
        (json ^? key "logsData" . _Array) `shouldSatisfy` isJust
        (json ^? key "cols" . _Array) `shouldSatisfy` isJust
        (json ^? key "colIdxMap" . _Object) `shouldSatisfy` isJust
        (json ^? key "count" . _Number) `shouldSatisfy` isJust
        (json ^? key "traces" . _Array) `shouldSatisfy` isJust
        (json ^? key "hasMore") `shouldSatisfy` isJust

      it "returns 400 for malformed query" $ \tr -> do
        (toBaseServantResponse tr.trATCtx tr.trLogger
          (Log.queryEvents testPid (Just "|| invalid {{") (Just "1h") Nothing Nothing Nothing Nothing)
          >>= evaluateWHNF_) `shouldThrow` anyException

    describe "Metrics endpoint" do
      it "returns valid MetricsData for count query with JSON round-trip" $ \tr -> do
        result <-
          toBaseServantResponse tr.trATCtx tr.trLogger
            $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just testPid) (Just "summarize count(*) by bin_auto(timestamp)") Nothing (Just "1h") Nothing Nothing (Just "spans") []
        result.rowsCount `shouldSatisfy` (>= 0)
        AE.eitherDecode @MetricsData (AE.encode result) `shouldSatisfy` isRight

    describe "Monitor CRUD + lifecycle" do
      it "create → get → patch → mute → unmute → delete round-trip" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
            input = (def :: ApiT.MonitorInput){ApiT.title = "t1", ApiT.query = "count(*)", ApiT.alertThreshold = 5, ApiT.checkIntervalMins = 5, ApiT.timeWindowMins = 15}
        created <- runB $ ApiH.apiMonitorCreate testPid input
        created.alertConfig.title `shouldBe` "t1"
        got <- runB $ ApiH.apiMonitorGet testPid created.id
        got.id `shouldBe` created.id
        patched <- runB $ ApiH.apiMonitorPatch testPid created.id ((def :: ApiT.MonitorPatch){ApiT.title = Just "t2"})
        patched.alertConfig.title `shouldBe` "t2"
        muted <- runB $ ApiH.apiMonitorMute testPid created.id (Just 60)
        muted.mutedUntil `shouldSatisfy` isJust
        unmuted <- runB $ ApiH.apiMonitorUnmute testPid created.id
        unmuted.mutedUntil `shouldBe` Nothing
        NoContent <- runB $ ApiH.apiMonitorDelete testPid created.id
        pass

      it "bulk delete marks monitors as deleted" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
            mkInput n = (def :: ApiT.MonitorInput){ApiT.title = "bulk-" <> n, ApiT.query = "count(*)", ApiT.alertThreshold = 1, ApiT.checkIntervalMins = 5, ApiT.timeWindowMins = 15}
        m1 <- runB $ ApiH.apiMonitorCreate testPid (mkInput "a")
        m2 <- runB $ ApiH.apiMonitorCreate testPid (mkInput "b")
        res <- runB $ ApiH.apiMonitorBulk testPid ApiT.BulkAction{ApiT.action = "delete", ApiT.ids = [m1.id.unQueryMonitorId, m2.id.unQueryMonitorId], ApiT.durationMinutes = Nothing}
        length res.succeeded `shouldBe` 2

    describe "Dashboard CRUD" do
      it "create → star → unstar → delete round-trip" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
            input = ApiT.DashboardInput{ApiT.title = "d1", ApiT.tags = Just ["a"], ApiT.teams = Nothing, ApiT.filePath = Nothing, ApiT.schema = Nothing}
        created <- runB $ ApiH.apiDashboardCreate testPid input
        created.title `shouldBe` "d1"
        starred <- runB $ ApiH.apiDashboardStar testPid created.id
        starred.starred `shouldBe` True
        NoContent <- runB $ ApiH.apiDashboardUnstar testPid created.id
        NoContent <- runB $ ApiH.apiDashboardDelete testPid created.id
        pass

      it "bulk delete removes multiple dashboards" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
            mkInput n = ApiT.DashboardInput{ApiT.title = "bulk-d-" <> n, ApiT.tags = Nothing, ApiT.teams = Nothing, ApiT.filePath = Nothing, ApiT.schema = Nothing}
        d1 <- runB $ ApiH.apiDashboardCreate testPid (mkInput "a")
        d2 <- runB $ ApiH.apiDashboardCreate testPid (mkInput "b")
        res <- runB $ ApiH.apiDashboardBulk testPid ApiT.BulkAction{ApiT.action = "delete", ApiT.ids = [d1.id.unwrap, d2.id.unwrap], ApiT.durationMinutes = Nothing}
        length res.succeeded `shouldBe` 2

      it "apply upserts by file_path (idempotent)" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
            doc = ApiT.DashboardYAMLDoc{ApiT.filePath = "dashes/foo.yaml", ApiT.title = Just "foo", ApiT.tags = Nothing, ApiT.teams = Nothing, ApiT.schema = def}
        d1 <- runB $ ApiH.apiDashboardApply testPid doc
        d2 <- runB $ ApiH.apiDashboardApply testPid doc
        d1.id `shouldBe` d2.id -- same row updated, not inserted again

    describe "API keys CRUD" do
      it "create returns plaintext once; list includes it; delete deactivates" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
        created <- runB $ ApiH.apiKeyCreate testPid ApiT.ApiKeyCreate{ApiT.title = "k1"}
        created.summary.title `shouldBe` "k1"
        created.key `shouldSatisfy` not . T.null
        keys <- runB $ ApiH.apiKeysList testPid
        (created.summary.id `elem` fmap (.id) keys) `shouldBe` True
        deact <- runB $ ApiH.apiKeyDeactivate testPid created.summary.id
        deact.active `shouldBe` False

    describe "Events body query" do
      it "accepts EventsQuery payload and returns LogResult" $ \tr -> do
        result <-
          toBaseServantResponse tr.trATCtx tr.trLogger
            $ ApiH.apiEventsQuery testPid (def :: ApiT.EventsQuery){ApiT.query = Just "", ApiT.since = Just "1h"}
        let json = AE.toJSON result
        (json ^? key "logsData" . _Array) `shouldSatisfy` isJust
        (json ^? key "count" . _Number) `shouldSatisfy` isJust

    describe "Share link create" do
      it "returns id and url containing /share/r/<id>" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
        eventId <- UUIDV4.nextRandom
        now <- getCurrentTime
        res <- runB $ ApiH.apiShareLinkCreate testPid ApiH.ShareLinkCreate{ApiH.eventId = eventId, ApiH.eventCreatedAt = now, ApiH.eventType = Just "log"}
        UUID.toText res.id `shouldSatisfy` (not . T.null)
        ("/share/r/" `T.isInfixOf` res.url) `shouldBe` True
