module Pkg.QueryCacheSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently, replicateConcurrently)
import Control.Exception qualified as E
import Data.Effectful.Hasql (SqlSource (..))
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Pages.Charts.Charts (DataType (..), MetricsData (..), convertTimestampsToMs)
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Parser (defPid)
import Pkg.QueryCache (RawCacheKey (..), coalesceQuery, mergeTimeseriesData, newQueryFlights, trimToRange)
import Relude
import System.IO.Error (userError)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)


mkTime :: Int -> UTCTime
mkTime = posixSecondsToUTCTime . fromIntegral


mkMetrics :: [(Double, Double)] -> MetricsData
mkMetrics rows =
  MetricsData
    { dataset = V.fromList $ map (\(ts, val) -> V.fromList [Just ts, Just val]) rows
    , dataFloat = Nothing
    , dataJSON = V.empty
    , dataText = V.empty
    , headers = V.fromList ["timestamp", "value"]
    , rowsCount = fromIntegral $ length rows
    , rowsPerMin = Nothing
    , from = floor . fst <$> viaNonEmpty head rows
    , to = floor . fst <$> viaNonEmpty last rows
    , stats = Nothing
    , error = Nothing
    }


emptyMetrics :: MetricsData
emptyMetrics = MetricsData V.empty Nothing V.empty V.empty V.empty 0 Nothing Nothing Nothing Nothing Nothing


spec :: Spec
spec = do
  describe "raw SQL request coalescing" do
    it "runs one leader for concurrent requests with the complete endpoint key" do
      flights <- newQueryFlights
      calls <- newIORef (0 :: Int)
      let key = rawKey "endpoint-a" (Just "production") "5 minutes"
          fetch = do
            atomicModifyIORef' calls $ \n -> (n + 1, ())
            threadDelay 100_000
            pure emptyMetrics
      results <- replicateConcurrently 12 $ coalesceQuery flights key fetch
      readIORef calls `shouldReturn` 1
      length (filter fst results) `shouldBe` 1
      map ((.rowsCount) . snd) results `shouldBe` replicate 12 0

    it "wakes every follower on failure and permits a later retry" do
      flights <- newQueryFlights
      let key = rawKey "failure" Nothing "5 minutes"
          failed = threadDelay 100000 >> E.throwIO (userError "backend failed")
      results <- replicateConcurrently 12 $ E.try @E.IOException $ coalesceQuery flights key failed
      results `shouldSatisfy` all isLeft
      retried <- coalesceQuery flights key (pure emptyMetrics)
      fst retried `shouldBe` True
      (snd retried).rowsCount `shouldBe` 0

    it "does not coalesce requests when an explicit key dimension changes" do
      flights <- newQueryFlights
      calls <- newIORef (0 :: Int)
      let key = rawKey "endpoint-a" Nothing "5 minutes"
          keys :: [RawCacheKey]
          keys =
            [ key
            , key{projectId = UUIDId $ UUID.fromWords 0 0 0 1} :: RawCacheKey
            , key{endpointHash = "endpoint-b"} :: RawCacheKey
            , key{environment = Just "production"} :: RawCacheKey
            , key{service = Just "checkout"} :: RawCacheKey
            , key{from = mkTime 11} :: RawCacheKey
            , key{to = mkTime 21} :: RawCacheKey
            , key{sqlQuery = "SELECT 2"} :: RawCacheKey
            , key{kqlQuery = "name == foo"} :: RawCacheKey
            , key{rollupInterval = "1 hour"} :: RawCacheKey
            , key{backend = SqlPostgres} :: RawCacheKey
            , key{decoder = DTFloat} :: RawCacheKey
            ]
          fetch = do
            atomicModifyIORef' calls (\n -> (n + 1, ()))
            threadDelay 100000
            pure emptyMetrics
      void $ mapConcurrently (\k -> coalesceQuery flights k fetch) keys
      readIORef calls `shouldReturn` length keys

  describe "chart response event rate" do
    it "uses event totals and the selected interval for fresh, merged and trimmed data" do
      let fresh = (mkMetrics [(86400, 103680000)]){from = Just 0, to = Just 2592000, rowsCount = 103680000, rowsPerMin = Just 0.01}
          merged = mergeTimeseriesData (mkMetrics [(86400, 51840000)]) (mkMetrics [(172800, 51840000)])
          trimmed = trimToRange merged (mkTime 0) (mkTime 2592000)
      forM_ [fresh, trimmed] \metrics -> do
        let response = convertTimestampsToMs metrics
        response.rowsPerMin `shouldBe` Just 2400
        response.to `shouldBe` Just 2592000000
      forM_ [0, -1] \end ->
        (convertTimestampsToMs fresh{to = Just end}).rowsPerMin `shouldBe` Just 0
      (convertTimestampsToMs fresh{rowsCount = 0, dataset = V.empty}).rowsPerMin `shouldBe` Just 0
      (convertTimestampsToMs fresh{rowsPerMin = Nothing}).rowsPerMin `shouldBe` Nothing


rawKey :: Text -> Maybe Text -> Text -> RawCacheKey
rawKey endpointHash environment rollupInterval =
  RawCacheKey
    { projectId = defPid
    , endpointHash
    , from = mkTime 10
    , to = mkTime 20
    , environment
    , service = Nothing
    , sqlQuery = "SELECT 1"
    , kqlQuery = ""
    , rollupInterval
    , backend = SqlTimefusion
    , decoder = DTText
    }
