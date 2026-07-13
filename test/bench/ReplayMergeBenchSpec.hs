-- | CPU + memory benchmark for the replay-session merge hot path
-- (`Pages.Replay.mergeOneSession`).
--
-- Motivated by a prod incident (2026-07-13): a single 240-file, 1.4 GiB session
-- whose `merged.json.gz` was 497 MiB *compressed* melted every app replica.
-- Each merge pass decompresses the whole merged blob (multi-GB live), concatenates
-- up to 25 new files, and re-compresses — and because the compressed result is
-- both length-measured and streamed to S3, the full compressed blob is retained
-- too. Draining 240 files in 25-file passes is O(n²) in bytes.
--
-- This bench reproduces the *pure core* of the merge against the real fixtures so
-- we can measure the blowup and compare optimizations. Point it at a downloaded
-- session directory (raw `<ts>.json` files + `merged.json.gz`):
--
--   BENCH_REPLAY_DIR=/path/to/<session-uuid> \
--     cabal run monoscope:bench:utils-bench -- +RTS -T -s
--
-- For cost-centre + heap attribution, build the target with profiling and run:
--
--   cabal run --enable-profiling monoscope:bench:utils-bench -- +RTS -T -p -hc -l-au
--   hp2ps -c utils-bench.hp && open utils-bench.ps   # heap-by-cost-centre
--
-- Set BENCH_SCENARIO to run a single scenario in isolation (clean per-scenario
-- peak-residency numbers, since GHC.Stats max_live_bytes is a process high-water):
-- one of `current-pass`, `current-drain`, `sharded-drain`.
module ReplayMergeBenchSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Control.Exception (evaluate)
import Data.ByteString.Lazy qualified as BL
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Stats
import Pages.Replay (concatRawJsonArrays, firstEventTimestampRaw)
import Relude
import System.Directory (doesFileExist, listDirectory)
import System.Mem (performMajorGC)
import Test.Hspec

-- Mirror of Pages.Replay's constant; the merge caps each pass at this many files.
maxFilesPerMerge :: Int
maxFilesPerMerge = 25

spec :: Spec
spec = describe "replay merge hot path" $
  beforeAll loadFixtures $ do
    it "reports CPU + memory for the merge scenarios" $ \fx -> case fx of
      Nothing -> pendingWith "set BENCH_REPLAY_DIR to a downloaded session dir to run"
      Just Fixtures{mergedGz, files} -> do
        scenario <- lookupEnv "BENCH_SCENARIO"
        putStrLn $ "\n=== replay merge bench: " <> show (length files) <> " event files, "
          <> "merged.json.gz " <> humanBytes (BL.length mergedGz) <> " compressed ===\n"
        let run name = maybe True (== name) scenario
        when (run "current-pass") $
          measure "current: single worst-case pass (decompress merged + concat 25 + recompress)" $
            currentPass mergedGz (take maxFilesPerMerge files)
        when (run "current-drain") $
          measure "current: full drain from empty, 25-file passes (O(n^2))" $
            currentDrain files
        when (run "sharded-drain") $
          measure "candidate: bounded 32 MiB shards, single pass (O(n), flat heap)" $
            shardedDrain files

-- | One production merge pass: existing merged.json.gz (gzipped) + N raw new files
-- → sort by first-event ts → concat raw arrays → recompress. Returns the new
-- compressed blob's length, forcing the whole pipeline (as prod does when it takes
-- `BL.length compressed` for the S3 put).
currentPass :: BL.ByteString -> [BL.ByteString] -> IO Int64
currentPass mergedGz newRaws = do
  let mergedRaw = GZip.decompress mergedGz
      tagged = (tsOf mergedRaw, mergedRaw) : [(tsOf r, r) | r <- newRaws]
      merged = concatRawJsonArrays (map snd (sortOn fst tagged))
      compressed = GZip.compress merged
  evaluate (BL.length compressed)

-- | Simulate production draining a fresh session: fold the files in 25-file
-- passes, each pass decompressing the *growing* merged blob, concatenating the
-- next 25, and recompressing. This is what actually happens over a session's
-- life and is quadratic in total bytes.
currentDrain :: [BL.ByteString] -> IO Int64
currentDrain allFiles = go (GZip.compress "[]") allFiles
  where
    go mergedGz [] = evaluate (BL.length mergedGz)
    go mergedGz rest = do
      let (chunk, more) = splitAt maxFilesPerMerge rest
      newGz <- passOnce mergedGz chunk
      _ <- evaluate (BL.length newGz) -- force this pass fully, like prod
      go newGz more
    passOnce mergedGz chunk = do
      let mergedRaw = GZip.decompress mergedGz
          tagged = (tsOf mergedRaw, mergedRaw) : [(tsOf r, r) | r <- chunk]
      pure $ GZip.compress (concatRawJsonArrays (map snd (sortOn fst tagged)))

-- | Candidate optimization: never hold one monolithic merged blob. Roll the
-- output into bounded shards — start a new shard whenever the current one would
-- exceed the cap. Peak heap is bounded by one shard + its compression buffer
-- regardless of session size, and total work is O(n) (each file compressed once).
-- The read path already concatenates raw arrays, so it can stitch shards.
shardedDrain :: [BL.ByteString] -> IO Int
shardedDrain allFiles = go 0 0 [] allFiles
  where
    shardCapBytes = 32 * 1024 * 1024 :: Int64
    -- (shardCount, curBytes, curAcc) fold; emit a gzipped shard when cap exceeded.
    go shards _ acc [] = do
      _ <- flush acc
      pure (shards + if null acc then 0 else 1)
    go shards curBytes acc (f : fs)
      | curBytes + BL.length f > shardCapBytes && not (null acc) = do
          _ <- flush acc
          go (shards + 1) (BL.length f) [f] fs
      | otherwise = go shards (curBytes + BL.length f) (f : acc) fs
    flush [] = pure (0 :: Int64)
    flush acc = do
      let shardGz = GZip.compress (concatRawJsonArrays (sortOn tsOf acc))
      evaluate (BL.length shardGz)

tsOf :: BL.ByteString -> Double
tsOf = fromRight 0 . firstEventTimestampRaw

-- Fixtures ------------------------------------------------------------------

data Fixtures = Fixtures {mergedGz :: BL.ByteString, files :: [BL.ByteString]}

loadFixtures :: IO (Maybe Fixtures)
loadFixtures =
  lookupEnv "BENCH_REPLAY_DIR" >>= \case
    Nothing -> pure Nothing
    Just dir -> do
      names <- listDirectory dir
      let isJson n = n /= "merged.json.gz" && drop (length n - 5) n == ".json"
          jsonNames = sort (filter isJson names)
          path n = dir <> "/" <> n
      files <- mapM (BL.readFile . path) jsonNames
      let mergedPath = path "merged.json.gz"
      hasMerged <- doesFileExist mergedPath
      mergedGz <- if hasMerged then BL.readFile mergedPath else pure (GZip.compress "[]")
      -- Force fixture bytes into memory once so bench numbers exclude disk I/O.
      _ <- evaluate (sum (map BL.length files) + BL.length mergedGz)
      pure $ Just Fixtures{mergedGz, files}

-- Measurement ---------------------------------------------------------------

-- | Run one scenario, forcing its result, and print wall time plus GHC.Stats
-- deltas: bytes allocated (total churn) and the process peak-live high-water
-- (`max_live_bytes`) — the number that OOM-kills replicas. Needs `+RTS -T`.
measure :: (Show a) => String -> IO a -> IO ()
measure name act = do
  enabled <- getRTSStatsEnabled
  performMajorGC
  s0 <- if enabled then Just <$> getRTSStats else pure Nothing
  t0 <- getCurrentTime
  r <- act
  t1 <- getCurrentTime
  performMajorGC
  s1 <- if enabled then Just <$> getRTSStats else pure Nothing
  putStrLn $ "• " <> name
  putStrLn $ "    result (forced): " <> show r
  putStrLn $ "    wall:            " <> showMs (realToFrac (diffUTCTime t1 t0))
  case (s0, s1) of
    (Just a, Just b) -> do
      putStrLn $ "    allocated:       " <> humanBytes (fromIntegral (allocated_bytes b - allocated_bytes a))
      putStrLn $ "    peak live (hwm): " <> humanBytes (fromIntegral (max_live_bytes b))
      putStrLn $ "    max mem in use:  " <> humanBytes (fromIntegral (max_mem_in_use_bytes b))
      putStrLn $ "    GC cpu:          " <> showMs (fromIntegral (gc_cpu_ns b - gc_cpu_ns a) / 1e9)
    _ -> putStrLn "    (run with +RTS -T for allocation/peak-memory stats)"
  putStrLn ""

showMs :: Double -> String
showMs s = show (round (s * 1000) :: Int) <> "ms"

humanBytes :: Int64 -> String
humanBytes n
  | n < 1024 = show n <> " B"
  | n < 1024 * 1024 = show (n `div` 1024) <> " KiB"
  | n < 1024 * 1024 * 1024 = show (n `div` (1024 * 1024)) <> " MiB"
  | otherwise = show (fromIntegral n / (1024 * 1024 * 1024) :: Double) <> " GiB"
