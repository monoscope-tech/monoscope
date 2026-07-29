module ExtractionWorkerSpec (spec) where

import BackgroundJobs qualified
import Control.Concurrent.STM.TBQueue (flushTBQueue)
import Data.Effectful.Hasql qualified as Hasql
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.ExtractionWorker qualified as ExtractionWorker
import Pkg.TestUtils
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import Test.Hspec (Spec, around, describe, it, shouldSatisfy)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


spec :: Spec
spec = around withTestResources do
  describe "Extraction Worker" do
    it "parity: ingest spans → processEagerBatch produces endpoints + hashes" \tr -> do
      apiKey <- createTestAPIKey tr pid "ew-parity-key"
      -- Ingest several HTTP spans
      replicateM_ 3 $ ingestTrace tr apiKey "GET /api/parity/check" frozenTime
      replicateM_ 3 $ ingestTrace tr apiKey "POST /api/parity/submit" frozenTime

      let ctx = tr.trATCtx
          trTf = tr{trATCtx = ctx{env = ctx.env{enableTimefusionReads = True}, config = ctx.config{enableTimefusionWrites = True}}}
      drainExtractionWorker trTf

      -- Verify endpoints were created
      endpoints <-
        withPool tr.trPool
          $ DBT.query
            [sql| SELECT url_path FROM apis.endpoints
              WHERE project_id = ? AND url_path LIKE '/api/parity/%' |]
            (Only pid)
          :: IO (V.Vector (Only Text))
      V.length endpoints `shouldSatisfy` (>= 2)

      -- Verify spans got processed_at stamped
      processed <-
        withPool tr.trPool
          $ DBT.query
            [sql| SELECT COUNT(*)::INT FROM otel_logs_and_spans
              WHERE project_id = ? AND processed_at IS NOT NULL
                AND timestamp >= ? AND timestamp <= ? |]
            (pid, addUTCTime (-10) frozenTime, addUTCTime 10 frozenTime)
          :: IO (V.Vector (Only Int))
      case V.toList processed of
        [Only n] -> n `shouldSatisfy` (>= 6)
        _ -> pass

      tfHashes :: V.Vector Text <-
        runTestBg frozenTime trTf
          $ Hasql.withHasqlTimefusion True
          $ Hasql.interp
            [HI.sql| SELECT array_to_string(hashes, chr(31))
                  FROM otel_logs_and_spans
                  WHERE project_id = #{pid.toText}
                    AND name = 'GET /api/parity/check' |]
      tfHashes `shouldSatisfy` any (not . T.null)

    it "safety-net: unprocessed rows get re-driven through the worker" \tr -> do
      apiKey <- createTestAPIKey tr pid "ew-safetynet-key"
      -- Query DB clock (may be real or frozen) and use a timestamp 1h before it
      -- Must satisfy: now() - 24h < ts < now() - 10m
      [Only dbNow] <- withPool tr.trPool $ DBT.query_ [sql| SELECT now() |]
      let safetyNetTime = addUTCTime (-3600) dbNow
      -- Temporarily disable accepting so submitBatch drops the batch
      let worker = (.extractionWorker) tr.trATCtx
      atomically $ writeTVar worker.acceptingBatches False
      ingestTrace tr apiKey "GET /api/safetynet/test" safetyNetTime
      atomically $ writeTVar worker.acceptingBatches True

      -- Verify the span has processed_at IS NULL
      unprocessed <-
        withPool tr.trPool
          $ DBT.query
            [sql| SELECT COUNT(*)::INT FROM otel_logs_and_spans
              WHERE project_id = ? AND processed_at IS NULL
                AND name = 'GET /api/safetynet/test' |]
            (Only pid)
          :: IO (V.Vector (Only Int))
      case V.toList unprocessed of
        [Only n] -> n `shouldSatisfy` (>= 1)
        _ -> pass

      -- Run safety-net reprocess
      runTestBg frozenTime tr
        $ BackgroundJobs.processBackgroundJob
          tr.trATCtx
          (BackgroundJobs.SafetyNetReprocess pid)
      drainExtractionWorker tr

      -- Verify the span now has processed_at IS NOT NULL
      processed <-
        withPool tr.trPool
          $ DBT.query
            [sql| SELECT COUNT(*)::INT FROM otel_logs_and_spans
              WHERE project_id = ? AND processed_at IS NOT NULL
                AND name = 'GET /api/safetynet/test' |]
            (Only pid)
          :: IO (V.Vector (Only Int))
      case V.toList processed of
        [Only n] -> n `shouldSatisfy` (>= 1)
        _ -> pass

    -- Regression guard for the drain hash-update re-send loop (2026-07-29/30):
    -- `processed_at` was stamped ONLY by UPDATE-1, which is gated by
    -- `enableHashUpdates` + `hashUpdateMaxAgeSecs`. Whenever that gate closed,
    -- fully-derived rows stayed `processed_at IS NULL`, so SafetyNetReprocess
    -- re-selected and re-drove the same rows on every tick forever — re-sending
    -- hash-append UPDATEs to TimeFusion at a volume that never declined.
    it "safety-net: a fully drained batch is never re-selected on the next tick" \tr -> do
      apiKey <- createTestAPIKey tr pid "ew-loop-key"
      [Only dbNow] <- withPool tr.trPool $ DBT.query_ [sql| SELECT now() |]
      let ctx = tr.trATCtx
          -- Hash updates disabled: derivation still runs, so bookkeeping must
          -- still be recorded (by attempt, not by rows-affected).
          trNoHash = tr{trATCtx = ctx{config = ctx.config{enableHashUpdates = False}}}
          worker = ctx.extractionWorker
          shards = V.toList worker.shards
          -- Drive the worker at `dbNow` so the age cutoff is evaluated against
          -- the rows' real age (drainExtractionWorker pins frozenTime, which
          -- would place every row inside the cutoff).
          drainAt t res = do
            for_ shards \shard -> do
              batches <- atomically do
                bs <- flushTBQueue shard.ingressQ
                modifyTVar' shard.queueDepth (\d -> d - length bs) $> bs
              for_ batches \b -> runTestBg t res $ BackgroundJobs.processEagerBatch b shard
            ExtractionWorker.forceFlushAllBuffers worker t
            for_ shards \shard -> do
              tasks <- atomically $ flushTBQueue shard.drainFlushQ
              for_ tasks $ runTestBg t res . BackgroundJobs.flushDrainTask shard
          handedOff = atomically $ sum <$> mapM (fmap length . flushTBQueue . (.ingressQ)) shards

      -- 1h old: inside the safety-net window (10m–6h) and inside the age cutoff.
      atomically $ writeTVar worker.acceptingBatches False
      ingestTrace tr apiKey "GET /api/loop/test" (addUTCTime (-3600) dbNow)
      -- 3h old: inside the safety-net window but past the 2h age cutoff, so the
      -- pipeline can never complete it — it must not be re-driven at all.
      ingestTrace tr apiKey "GET /api/loop/stale" (addUTCTime (-(3 * 3600)) dbNow)
      atomically $ writeTVar worker.acceptingBatches True

      -- Tick 1: re-drive + derive.
      runTestBg dbNow trNoHash $ BackgroundJobs.processBackgroundJob trNoHash.trATCtx (BackgroundJobs.SafetyNetReprocess pid)
      drainAt dbNow trNoHash

      endpoints <-
        withPool tr.trPool
          $ DBT.query
            [sql| SELECT url_path FROM apis.endpoints WHERE project_id = ? AND url_path LIKE '/api/loop/%' |]
            (Only pid)
          :: IO (V.Vector (Only Text))
      V.length endpoints `shouldSatisfy` (>= 1)

      -- The completable row must be marked done despite the hash gate being closed.
      stillUnprocessed <-
        withPool tr.trPool
          $ DBT.query
            [sql| SELECT COUNT(*)::INT FROM otel_logs_and_spans
              WHERE project_id = ? AND processed_at IS NULL AND name = 'GET /api/loop/test' |]
            (Only pid)
          :: IO (V.Vector (Only Int))
      V.toList stillUnprocessed `shouldSatisfy` all (\(Only n) -> n == 0)

      -- Tick 2: no row may be handed off to the worker a second time.
      runTestBg dbNow trNoHash $ BackgroundJobs.processBackgroundJob trNoHash.trATCtx (BackgroundJobs.SafetyNetReprocess pid)
      reDriven <- handedOff
      reDriven `shouldSatisfy` (== 0)
