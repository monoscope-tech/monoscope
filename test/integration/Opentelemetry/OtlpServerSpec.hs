module Opentelemetry.OtlpServerSpec where

import Pkg.TestUtils
import Relude
import Test.Hspec


-- Mock data generators
spec :: Spec
spec = aroundAll withTestResources do
  describe "processList" do
    it "should process a request" $ const pending

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
