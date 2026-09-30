module Main (main) where

import Data.List (lookup)
import Relude
import Spec qualified
import Test.Hspec.Runner (Config (..), defaultConfig, hspecWith)


-- Process-level sharding for the integration suite.
--
-- In-process hspec `parallel` deadlocks here: `withTestResources` builds a per-example
-- connection pool (resource-pool) whose background collector MVar gets orphaned when
-- hspec tears down `around` resources concurrently. So instead of one process with
-- parallel examples, `make test-shards` runs N OS processes — each its own RTS (own
-- pools, logger, globals; nothing shared) — and each runs a disjoint shard SEQUENTIALLY.
--
-- Sharding is keyed on the OUTERMOST describe group, so every example of a spec lands
-- in the same shard. That is required for the state-sharing specs (aroundAll / ordered
-- flows): splitting their examples across processes would break them.
--
-- SHARD_INDEX / SHARD_TOTAL drive it; absent (or TOTAL<=1) ⇒ run the whole suite, so
-- `cabal test`, `make test`, and the live-test-dev watcher are unchanged.
main :: IO ()
main = do
  idx <- (readMaybe =<<) <$> lookupEnv "SHARD_INDEX"
  tot <- (readMaybe =<<) <$> lookupEnv "SHARD_TOTAL"
  let cfg = case (idx, tot) of
        (Just i, Just n) | n > 1 -> defaultConfig{configFilterPredicate = Just (inShard i n)}
        _ -> defaultConfig
  hspecWith cfg Spec.spec
  where
    -- Deterministic hash of the outermost group; keeps a spec's examples (and any
    -- shared state) together in one shard.
    inShard :: Int -> Int -> ([String], String) -> Bool
    inShard i n (groups, _) = (if n == 12 then fromMaybe hashed (lookup groupName balanced) else hashed) == i
      where
        groupName = concat (take 1 groups)
        hashed = foldl' (\a c -> a * 31 + ord c) 7 groupName `mod` n
        balanced =
          [ ("Pages.LogExplorer.Log", 4)
          , ("Pages.Bots.Workflows", 5)
          , ("BackgroundJobs.ReportUsage", 2)
          , ("EndpointDiscovery", 7)
          , ("Pages.Endpoints.ApiCatalog", 0)
          ]
