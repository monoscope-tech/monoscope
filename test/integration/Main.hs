module Main (main) where

import Data.List (lookup)
import Pkg.TestUtils (clearFixedProjects)
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
  clearFixedProjects
  hspecWith cfg Spec.spec
  where
    -- Deterministic hash of the outermost group; keeps a spec's examples (and any
    -- shared state) together in one shard. Per-example isolated heavy specs
    -- (Pages.Bots, Pages.ImpactReviews — both use `around`, never `aroundAll`)
    -- split across two shards so no single shard dominates wall clock.
    inShard :: Int -> Int -> ([String], String) -> Bool
    inShard i n (groups, desc) = target == i
      where
        groupName = concat (take 1 groups)
        hashStr s = foldl' (\a c -> a * 31 + ord c) 7 s
        hashed = hashStr groupName `mod` n
        target
          | n == 12, Just shards <- lookup groupName splitGroups = fromMaybe hashed (shards !!? (hashStr desc `mod` length shards))
          | n == 12 = fromMaybe hashed (lookup groupName balanced)
          | otherwise = hashed
        splitGroups =
          [ ("Pages.Bots", [5, 3, 0])
          , ("Pages.ImpactReviews", [10, 4])
          ]
        -- Measured overrides for the local CI default of 12 shards; other totals
        -- use the stable hash so changing CI_SHARDS never drops a spec.
        balanced =
          [ ("Pages.LogExplorer.Log", 9)
          , ("Pages.CodeContext", 1)
          , ("Pages.Projects.Projects", 11)
          , ("Billing", 4)
          , ("Endpoints", 7)
          , ("Opentelemetry.Ingestion", 6)
          ]
