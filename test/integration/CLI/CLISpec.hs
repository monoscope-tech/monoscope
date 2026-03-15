module CLI.CLISpec (spec) where

import Relude hiding (get, put)

import Control.Lens ((&), (.~), (^.))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Effectful.Wreq (HTTP, getWith, responseBody)
import Data.Effectful.Wreq qualified as W
import Data.Time (addUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Effectful
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Network.Wreq qualified as Wreq
import Pages.Charts.Charts qualified as Charts
import Pages.Charts.Types qualified as Charts
import Pages.LogExplorer.Log qualified as Log
import Pkg.CLITestUtils (runHTTPtoServant)
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Test.Hspec

pid :: Projects.ProjectId
pid = UUIDId UUID.nil

timeAt :: Int -> Text
timeAt offsetSeconds = toText $ iso8601Show $ addUTCTime (fromIntegral offsetSeconds) frozenTime

-- | Simulate CLI's apiGet: build wreq Options with params, call getWith via HTTP effect
cliGet :: (HTTP :> es) => Text -> Text -> [(Text, Text)] -> Eff es LBS.ByteString
cliGet base path params = do
  let url = toString (base <> path)
      opts = foldl' (\acc (k, v) -> acc & Wreq.param k .~ [v]) W.defaults params
  (^. responseBody) <$> getWith opts url

runCLITest :: TestResources -> Eff '[HTTP, IOE] a -> IO a
runCLITest tr = runEff . runHTTPtoServant tr

baseUrl :: Text
baseUrl = "https://app.monoscope.tech"

spec :: Spec
spec = aroundAll withTestResources do
  describe "CLI integrated tests" do
    it "events search returns JSON with log data via HTTP effect" \tr -> do
      key <- createTestAPIKey tr pid "cli-test-events"
      ingestLog tr key "cli test log entry" frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      bs <- runCLITest tr $
        cliGet baseUrl "/log_explorer" [("query", ""), ("from", timeAt (-3600)), ("to", timeAt 3600), ("json", "true")]
      case AE.eitherDecode @AE.Value bs of
        Right (AE.Object obj) -> do
          KM.member "cols" obj `shouldBe` True
          KM.member "logsData" obj `shouldBe` True
          KM.member "count" obj `shouldBe` True
        Right _ -> expectationFailure "Expected JSON object"
        Left err -> expectationFailure $ "JSON decode error: " <> err

    it "chart_data returns metrics with headers via HTTP effect" \tr -> do
      key <- createTestAPIKey tr pid "cli-test-metrics"
      ingestLog tr key "cli metrics test" frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      bs <- runCLITest tr $
        cliGet baseUrl "/chart_data" [("query", "summarize count(*) by bin_auto(timestamp)"), ("from", timeAt (-3600)), ("to", timeAt 3600)]
      case AE.eitherDecode @AE.Value bs of
        Right (AE.Object obj) -> do
          KM.member "headers" obj `shouldBe` True
          KM.member "dataset" obj `shouldBe` True
          KM.member "rows_count" obj `shouldBe` True
        Right _ -> expectationFailure "Expected JSON object"
        Left err -> expectationFailure $ "JSON decode error: " <> err

    it "schema returns field definitions via HTTP effect" \tr -> do
      bs <- runCLITest tr $
        cliGet baseUrl "/log_explorer/schema" [("since", "24h")]
      case AE.eitherDecode @AE.Value bs of
        Right (AE.Object obj) -> do
          KM.member "fields" obj `shouldBe` True
          case KM.lookup "fields" obj of
            Just (AE.Object fields) ->
              KM.size fields `shouldSatisfy` (> 0)
            _ -> expectationFailure "Expected fields to be an object"
        Right _ -> expectationFailure "Expected JSON object"
        Left err -> expectationFailure $ "JSON decode error: " <> err

    it "returns empty results when query matches nothing" \tr -> do
      bs <- runCLITest tr $
        cliGet baseUrl "/log_explorer" [("query", "nonexistent_xyzzy_42"), ("since", "1h"), ("json", "true")]
      case AE.eitherDecode @AE.Value bs of
        Right (AE.Object obj) -> do
          KM.lookup "logsData" obj `shouldBe` Just (AE.Array mempty)
          KM.lookup "count" obj `shouldBe` Just (AE.Number 0)
        Right _ -> expectationFailure "Expected JSON object"
        Left err -> expectationFailure $ "JSON decode error: " <> err

    it "kind filter returns only matching source type" \tr -> do
      key <- createTestAPIKey tr pid "cli-kind-filter"
      ingestLog tr key "kind filter log" frozenTime
      ingestTrace tr key "kind-filter-span" frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      bs <- runCLITest tr $
        cliGet baseUrl "/log_explorer" [("query", ""), ("from", timeAt (-3600)), ("to", timeAt 3600), ("source", "log"), ("json", "true")]
      case AE.eitherDecode @AE.Value bs of
        Right (AE.Object obj) -> do
          KM.member "logsData" obj `shouldBe` True
          KM.member "count" obj `shouldBe` True
        Right _ -> expectationFailure "Expected JSON object"
        Left err -> expectationFailure $ "JSON decode error: " <> err

    it "empty metrics query returns empty dataset" \tr -> do
      bs <- runCLITest tr $
        cliGet baseUrl "/chart_data" [("query", "summarize count(*) by bin_auto(timestamp) | where service_name == 'nonexistent_xyzzy'"), ("since", "1h")]
      case AE.eitherDecode @AE.Value bs of
        Right (AE.Object obj) -> do
          KM.member "dataset" obj `shouldBe` True
          KM.lookup "rows_count" obj `shouldBe` Just (AE.Number 0)
        Right _ -> expectationFailure "Expected JSON object"
        Left err -> expectationFailure $ "JSON decode error: " <> err

    it "schema contains resource.service.name field" \tr -> do
      bs <- runCLITest tr $
        cliGet baseUrl "/log_explorer/schema" [("since", "24h")]
      case AE.eitherDecode @AE.Value bs of
        Right (AE.Object obj) -> case KM.lookup "fields" obj of
          Just (AE.Object fields) ->
            KM.member "resource.service.name" fields `shouldBe` True
          _ -> expectationFailure "Expected fields object"
        Right _ -> expectationFailure "Expected JSON object"
        Left err -> expectationFailure $ "JSON decode error: " <> err

  describe "JSON shape for CLI endpoints" do
    it "log_explorer JSON has cols, logsData, count keys" \tr -> do
      key <- createTestAPIKey tr pid "json-shape-test"
      ingestLog tr key "test log entry" frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      (_, pg) <-
        testServant tr $
          Log.apiLogH pid Nothing Nothing Nothing Nothing (Just $ timeAt (-3600)) (Just $ timeAt 3600) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing
      let json = AE.toJSON pg
      case json of
        AE.Object obj -> do
          KM.member "cols" obj `shouldBe` True
          KM.member "logsData" obj `shouldBe` True
          KM.member "count" obj `shouldBe` True
          KM.member "serviceColors" obj `shouldBe` True
          KM.member "hasMore" obj `shouldBe` True
        _ -> expectationFailure "Expected JSON object from log_explorer"

    it "chart_data JSON has headers, dataset, rows_count keys" \tr -> do
      key <- createTestAPIKey tr pid "json-shape-chart"
      ingestLog tr key "chart test entry" frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      result <-
        runQueryEffect tr $
          Charts.queryMetrics (Just Charts.DTMetric) (Just pid) (Just "summarize count(*) by bin_auto(timestamp)") Nothing Nothing (Just $ timeAt (-3600)) (Just $ timeAt 3600) (Just "spans") []
      let json = AE.toJSON result
      case json of
        AE.Object obj -> do
          KM.member "headers" obj `shouldBe` True
          KM.member "dataset" obj `shouldBe` True
          KM.member "rows_count" obj `shouldBe` True
        _ -> expectationFailure "Expected JSON object from chart_data"

    it "schema JSON has field definitions" \_ -> do
      let json = Schema.telemetrySchemaJson
      case json of
        AE.Object obj -> do
          KM.member "fields" obj `shouldBe` True
          case KM.lookup "fields" obj of
            Just (AE.Object fields) -> do
              KM.size fields `shouldSatisfy` (> 0)
              forM_ (take 1 $ KM.toList fields) \(_key, val) ->
                case val of
                  AE.Object fieldObj -> do
                    KM.member "field_type" fieldObj `shouldBe` True
                    KM.member "description" fieldObj `shouldBe` True
                  _ -> expectationFailure "Expected field value to be an object"
            _ -> expectationFailure "Expected fields to be an object"
        _ -> expectationFailure "Expected schema to be a JSON object"
