module Web.ApiV1Spec (spec) where

import Control.Lens ((^.), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _Array, _Number, _Object)
import Data.Map qualified as Map
import Data.OpenApi (OpenApi, info, title, version)
import Models.Apis.Monitors qualified as Monitors
import Models.Telemetry.Schema qualified as Schema
import Pages.Charts.Charts qualified as Charts
import Pages.Charts.Types (MetricsData (..))
import Pages.LogExplorer.Log qualified as Log
import Pkg.TestUtils
import Relude
import Test.Hspec
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

      it "declares all expected endpoint paths" $ \_tr -> do
        (specJson ^? key "paths" . key "/events") `shouldSatisfy` isJust
        (specJson ^? key "paths" . key "/metrics") `shouldSatisfy` isJust
        (specJson ^? key "paths" . key "/schema") `shouldSatisfy` isJust
        (specJson ^? key "paths" . key "/monitors") `shouldSatisfy` isJust

      it "includes schema definitions for response types" $ \_tr -> do
        let schemas = specJson ^? key "components" . key "schemas" . _Object
        schemas `shouldSatisfy` isJust
        (specJson ^? key "components" . key "schemas" . key "LogResult") `shouldSatisfy` isJust
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
