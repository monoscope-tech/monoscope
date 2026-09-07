module Pkg.KafkaMetricsSpec (spec) where

import Pkg.Queue (parseKafkaFetchQueues)
import Relude
import Test.Hspec


spec :: Spec
spec = describe "native Kafka fetch queue statistics" do
  it "sums every partition across topics and preserves the sample time" do
    parseKafkaFetchQueues "{\"time\":42,\"topics\":{\"first\":{\"partitions\":{\"0\":{\"fetchq_size\":1048576,\"fetchq_cnt\":2},\"1\":{\"fetchq_size\":7,\"fetchq_cnt\":1}}},\"second\":{\"partitions\":{\"3\":{\"fetchq_size\":9,\"fetchq_cnt\":4}}}},\"brokers\":{\"private-host\":{}}}"
      `shouldBe` Right (42, 1048592, 7, 3)
    parseKafkaFetchQueues "{\"time\":43,\"topics\":{}}" `shouldBe` Right (43, 0, 0, 0)

  it "rejects malformed or incomplete samples instead of reporting empty queues" do
    forM_
      [ "not json"
      , "{\"time\":42}"
      , "{\"time\":42,\"topics\":{\"t\":{\"partitions\":{\"0\":{\"fetchq_size\":7}}}}}"
      , "{\"time\":42,\"topics\":{\"t\":{\"partitions\":{\"0\":{\"fetchq_size\":-1,\"fetchq_cnt\":2}}}}}"
      ]
      \sample -> parseKafkaFetchQueues sample `shouldSatisfy` isLeft
