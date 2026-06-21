module Pkg.QueueSpec (spec) where

import Data.IntSet qualified as IntSet
import Pkg.Queue (PartProgress (..), RetryRoute (..), chunksByBytes, completeOffsets, parkingTopicFor, retryDestination, retryTiers)
import Relude
import Test.Hspec

-- Pure logic behind the unified Kafka consumer (Pkg.Queue.decoupledLoop). The
-- IO orchestration (Ki threads, poll/pause/commit) is a thin shell over these;
-- the data-loss-critical invariant lives in the offset watermark below.
spec :: Spec
spec = do
  describe "chunksByBytes (byte-sized sub-chunks)" do
    it "packs greedily up to the byte target" $
      chunksByBytes 10 identity [6, 5, 3, 20, 1] `shouldBe` [[6], [5, 3], [20], [1]]
    it "preserves order and drops nothing" $
      concat (chunksByBytes 7 identity [3, 3, 3, 3, 3 :: Int]) `shouldBe` [3, 3, 3, 3, 3]
    it "gives an over-target record its own chunk (no drop, no infinite loop)" $
      chunksByBytes 4 identity [10, 1, 1] `shouldBe` [[10], [1, 1]]
    it "returns no chunks for empty input" $
      chunksByBytes 10 identity ([] :: [Int]) `shouldBe` []

  describe "offset watermark (completeOffsets) — never commit past an unwritten offset" do
    let base0 = PartProgress 100 mempty
    it "advances across a fully contiguous run" $
      (completeOffsets [100, 101, 102] base0).base `shouldBe` 103
    it "holds at a gap until it is filled (the core safety property)" $ do
      let p = completeOffsets [101, 102] base0 -- 100 still missing
      p.base `shouldBe` 100 -- cannot commit past the unwritten 100
      IntSet.toList p.ahead `shouldBe` [101, 102]
      (completeOffsets [100] p).base `shouldBe` 103 -- gap filled → leaps forward
    it "is order-independent: out-of-order completion = in-order result" $
      (foldr completeOffsets base0 [[102], [100], [101]]).base `shouldBe` 103
    it "ignores already-committed (redelivered) offsets" $
      (completeOffsets [98, 99, 100] base0).base `shouldBe` 101
    it "never advances past a permanently-failed offset (105 never completes)" $
      (foldr completeOffsets base0 [[100], [101], [102], [103], [104], [106], [107]]).base `shouldBe` 105
    it "self-prunes: drained offsets leave the pending set (no unbounded growth)" $
      IntSet.null (completeOffsets [100, 101, 102] base0).ahead `shouldBe` True

  describe "DLQ retry tiers (retryDestination)" do
    let tiers = retryTiers "dlq"
        park = parkingTopicFor "dlq"
    it "routes the first failure to tier 0 (the base topic, 5s backoff)" $
      retryDestination tiers park 1 `shouldBe` RetryTier "dlq" 5
    it "escalates with the attempt count" $ do
      retryDestination tiers park 2 `shouldBe` RetryTier "dlq-retry-60s" 60
      retryDestination tiers park 3 `shouldBe` RetryTier "dlq-retry-600s" 600
    it "sends exhausted attempts to the terminal parking topic" $ do
      retryDestination tiers park 4 `shouldBe` Parking "dlq-parking"
      retryDestination tiers park 99 `shouldBe` Parking "dlq-parking"
