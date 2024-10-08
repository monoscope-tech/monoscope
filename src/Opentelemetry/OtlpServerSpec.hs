module Opentelemetry.OtlpServerSpec where

import Test.Hspec
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Opentelemetry.Proto.Collector.Trace.V1.TraceService (ExportTraceServiceRequest(..))
import Opentelemetry.Proto.Collector.Logs.V1.LogsService (ExportLogsServiceRequest(..))
import Opentelemetry.Proto.Collector.Metrics.V1.MetricsService (ExportMetricsServiceRequest(..))
import qualified Opentelemetry.Proto.Trace.V1.Trace as Trace
import qualified Opentelemetry.Proto.Logs.V1.Logs as Logs
import qualified Opentelemetry.Proto.Metrics.V1.Metrics as Metrics
import Relude
import Opentelemetry.OtlpMockValues 
import Data.Base64.Types qualified as B64
import Data.ByteString.Base64 qualified as B64
import Opentelemetry.OtlpServer qualified as OtlpServer
import ProcessMessageSpec (testAuthContext)
import Pkg.TestUtils qualified as TestUtils

-- Mock data generators
spec :: Spec
spec = aroundAll TestUtils.withSetup do
  describe "processList" do
    it "should process a request" \pool -> do
      let otlpTraceB64A' = B64.decodeBase64  $ B64.assertBase64 @'B64.StdPadded $ encodeUtf8 otlpTraceB64A
      authCtx <- testAuthContext pool
      resp <- TestUtils.runTestBackground authCtx
        $ OtlpServer.processList  [("A", otlpTraceB64A')] (HashMap.fromList [("ce-type", "org.opentelemetry.otlp.traces.v1")])
      traceShowM resp 
      pass

--   it "processes trace messages correctly" $ do
--     let input = [("ack1", protoToByteString mockTraceRequest)]
--         attrs = HashMap.fromList [("ce-type", "org.opentelemetry.otlp.traces.v1")]
--     result <- runM . evalState mockDB . runReader mockAuthContext . runReader mockLog . runReader mockIOE . runReader mockTime $ processList input attrs
--     result `shouldBe` ["ack1"]

  -- it "processes log messages correctly" $ do
  --   let input = [("ack2", protoToByteString mockLogRequest)]
  --       attrs = HashMap.fromList [("ce-type", "org.opentelemetry.otlp.logs.v1")]
  --   result <- runM . evalState mockDB . runReader mockAuthContext . runReader mockLog . runReader mockIOE . runReader mockTime $ processList input attrs
  --   result `shouldBe` ["ack2"]

  -- it "processes metric messages correctly" $ do
  --   let input = [("ack3", protoToByteString mockMetricRequest)]
  --       attrs = HashMap.fromList [("ce-type", "org.opentelemetry.otlp.metrics.v1")]
  --   result <- runM . evalState mockDB . runReader mockAuthContext . runReader mockLog . runReader mockIOE . runReader mockTime $ processList input attrs
  --   result `shouldBe` ["ack3"]

  -- it "handles empty input correctly" $ do
  --   let input = []
  --       attrs = HashMap.empty
  --   result <- runM . evalState mockDB . runReader mockAuthContext . runReader mockLog . runReader mockIOE . runReader mockTime $ processList input attrs
  --   result `shouldBe` []

  -- it "throws an error for unsupported data types" $ do
  --   let input = [("ack4", "invalid data")]
  --       attrs = HashMap.fromList [("ce-type", "unsupported.type")]
  --   runM . evalState mockDB . runReader mockAuthContext . runReader mockLog . runReader mockIOE . runReader mockTime (processList input attrs)
  --     `shouldThrow` anyErrorCall
