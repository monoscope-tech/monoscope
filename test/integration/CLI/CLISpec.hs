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
import Effectful
import Models.Telemetry.Schema qualified as Schema
import Network.Wreq qualified as Wreq
import Pages.Charts.Charts qualified as Charts
import Pages.LogExplorer.Log qualified as Log
import Pkg.TestUtils
import Test.Hspec

timeAt :: Int -> Text
timeAt offsetSeconds = toText $ iso8601Show $ addUTCTime (fromIntegral offsetSeconds) frozenTime

cliGet :: (HTTP :> es) => Text -> Text -> [(Text, Text)] -> Eff es LBS.ByteString
cliGet base path params = do
  let url = toString (base <> path)
      opts = foldl' (\acc (k, v) -> acc & Wreq.param k .~ [v]) W.defaults params
  (^. responseBody) <$> getWith opts url

runCLITest :: TestResources -> Eff '[HTTP, IOE] a -> IO a
runCLITest tr = runEff . runHTTPtoServant tr

baseUrl :: Text
baseUrl = "https://app.monoscope.tech"

-- | Decode JSON bytes and run assertions on the resulting object
decodeObject :: LBS.ByteString -> (KM.KeyMap AE.Value -> IO ()) -> IO ()
decodeObject bs check = case AE.eitherDecode @AE.Value bs of
  Right (AE.Object obj) -> check obj
  Right _ -> expectationFailure "Expected JSON object"
  Left err -> expectationFailure $ "JSON decode error: " <> err

-- | Assert that all given keys exist in the object
shouldHaveKeys :: KM.KeyMap AE.Value -> [AE.Key] -> IO ()
shouldHaveKeys obj = mapM_ (\k -> KM.member k obj `shouldBe` True)

checkJsonValue :: AE.Value -> (KM.KeyMap AE.Value -> IO ()) -> IO ()
checkJsonValue (AE.Object obj) check = check obj
checkJsonValue _ _ = expectationFailure "Expected JSON object"

spec :: Spec
spec = aroundAll withTestResources do
  describe "CLI integrated tests" do
    it "events search returns JSON with log data via HTTP effect" \tr -> do
      key <- createTestAPIKey tr testPid "cli-test-events"
      ingestLog tr key "cli test log entry" frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      bs <- runCLITest tr $
        cliGet baseUrl "/log_explorer" [("query", ""), ("from", timeAt (-3600)), ("to", timeAt 3600), ("json", "true")]
      decodeObject bs $ \obj -> shouldHaveKeys obj ["cols", "logsData", "count"]

    it "chart_data returns metrics with headers via HTTP effect" \tr -> do
      key <- createTestAPIKey tr testPid "cli-test-metrics"
      ingestLog tr key "cli metrics test" frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      bs <- runCLITest tr $
        cliGet baseUrl "/chart_data" [("query", "summarize count(*) by bin_auto(timestamp)"), ("from", timeAt (-3600)), ("to", timeAt 3600)]
      decodeObject bs $ \obj -> shouldHaveKeys obj ["headers", "dataset", "rows_count"]

    it "schema returns field definitions with service.name via HTTP effect" \tr -> do
      bs <- runCLITest tr $
        cliGet baseUrl "/log_explorer/schema" [("since", "24h")]
      decodeObject bs $ \obj -> do
        shouldHaveKeys obj ["fields"]
        case KM.lookup "fields" obj of
          Just (AE.Object fields) -> do
            KM.size fields `shouldSatisfy` (> 0)
            KM.member "resource.service.name" fields `shouldBe` True
          _ -> expectationFailure "Expected fields to be an object"

    it "returns empty results when query matches nothing" \tr -> do
      bs <- runCLITest tr $
        cliGet baseUrl "/log_explorer" [("query", "nonexistent_xyzzy_42"), ("since", "1h"), ("json", "true")]
      decodeObject bs $ \obj -> do
        KM.lookup "logsData" obj `shouldBe` Just (AE.Array mempty)
        KM.lookup "count" obj `shouldBe` Just (AE.Number 0)

    it "kind filter returns only matching source type" \tr -> do
      key <- createTestAPIKey tr testPid "cli-kind-filter"
      ingestLog tr key "kind filter log" frozenTime
      ingestTrace tr key "kind-filter-span" frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      bs <- runCLITest tr $
        cliGet baseUrl "/log_explorer" [("query", ""), ("from", timeAt (-3600)), ("to", timeAt 3600), ("source", "log"), ("json", "true")]
      decodeObject bs $ \obj -> shouldHaveKeys obj ["logsData", "count"]

    it "empty metrics query returns empty dataset" \tr -> do
      bs <- runCLITest tr $
        cliGet baseUrl "/chart_data" [("query", "summarize count(*) by bin_auto(timestamp) | where service_name == 'nonexistent_xyzzy'"), ("since", "1h")]
      decodeObject bs $ \obj -> do
        shouldHaveKeys obj ["dataset"]
        KM.lookup "rows_count" obj `shouldBe` Just (AE.Number 0)

  describe "JSON shape for CLI endpoints" do
    it "log_explorer JSON has cols, logsData, count, serviceColors, hasMore keys" \tr -> do
      key <- createTestAPIKey tr testPid "json-shape-test"
      ingestLog tr key "test log entry" frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      (_, pg) <-
        testServant tr $
          Log.apiLogH testPid Nothing Nothing Nothing Nothing (Just $ timeAt (-3600)) (Just $ timeAt 3600) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing
      checkJsonValue (AE.toJSON pg) $ \obj ->
        shouldHaveKeys obj ["cols", "logsData", "count", "serviceColors", "hasMore"]

    it "chart_data JSON has headers, dataset, rows_count keys" \tr -> do
      key <- createTestAPIKey tr testPid "json-shape-chart"
      ingestLog tr key "chart test entry" frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      result <-
        runQueryEffect tr $
          Charts.queryMetrics (Just Charts.DTMetric) (Just testPid) (Just "summarize count(*) by bin_auto(timestamp)") Nothing Nothing (Just $ timeAt (-3600)) (Just $ timeAt 3600) (Just "spans") []
      checkJsonValue (AE.toJSON result) $ \obj ->
        shouldHaveKeys obj ["headers", "dataset", "rows_count"]

    it "schema JSON has field definitions" \_ -> do
      checkJsonValue Schema.telemetrySchemaJson $ \obj -> do
        shouldHaveKeys obj ["fields"]
        case KM.lookup "fields" obj of
          Just (AE.Object fields) -> do
            KM.size fields `shouldSatisfy` (> 0)
            forM_ (take 1 $ KM.toList fields) \(_key, val) ->
              checkJsonValue val $ \fieldObj ->
                shouldHaveKeys fieldObj ["field_type", "description"]
          _ -> expectationFailure "Expected fields to be an object"
