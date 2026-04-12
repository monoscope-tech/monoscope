module ExtractionWorkerSpec (spec) where

import BackgroundJobs qualified
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.ExtractionWorker qualified as ExtractionWorker
import Pkg.TestUtils
import Relude
import System.Config (AuthContext (..))
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Extraction Worker" do

    it "parity: ingest spans → processEagerBatch produces endpoints + hashes" \tr -> do
      apiKey <- createTestAPIKey tr pid "ew-parity-key"
      -- Ingest several HTTP spans
      replicateM_ 3 $ ingestTrace tr apiKey "GET /api/parity/check" frozenTime
      replicateM_ 3 $ ingestTrace tr apiKey "POST /api/parity/submit" frozenTime

      drainExtractionWorker tr

      -- Verify endpoints were created
      endpoints <- withPool tr.trPool $ DBT.query
        [sql| SELECT url_path FROM apis.endpoints
              WHERE project_id = ? AND url_path LIKE '/api/parity/%' |]
        (Only pid) :: IO (V.Vector (Only Text))
      V.length endpoints `shouldSatisfy` (>= 2)

      -- Verify spans got processed_at stamped
      processed <- withPool tr.trPool $ DBT.query
        [sql| SELECT COUNT(*)::INT FROM otel_logs_and_spans
              WHERE project_id = ? AND processed_at IS NOT NULL
                AND timestamp >= ? AND timestamp <= ? |]
        (pid, addUTCTime (-10) frozenTime, addUTCTime 10 frozenTime) :: IO (V.Vector (Only Int))
      case V.toList processed of
        [Only n] -> n `shouldSatisfy` (>= 6)
        _ -> pass

    it "safety-net: unprocessed rows get re-driven through the worker" \tr -> do
      apiKey <- createTestAPIKey tr pid "ew-safetynet-key"
      -- Temporarily disable accepting so submitBatch drops the batch
      let worker = (.extractionWorker) tr.trATCtx
      atomically $ writeTVar worker.acceptingBatches False
      ingestTrace tr apiKey "GET /api/safetynet/test" frozenTime
      atomically $ writeTVar worker.acceptingBatches True

      -- Verify the span has processed_at IS NULL
      unprocessed <- withPool tr.trPool $ DBT.query
        [sql| SELECT COUNT(*)::INT FROM otel_logs_and_spans
              WHERE project_id = ? AND processed_at IS NULL
                AND name = 'GET /api/safetynet/test' |]
        (Only pid) :: IO (V.Vector (Only Int))
      case V.toList unprocessed of
        [Only n] -> n `shouldSatisfy` (>= 1)
        _ -> pass

      -- Run safety-net reprocess
      runTestBg frozenTime tr $ BackgroundJobs.processBackgroundJob tr.trATCtx
        (BackgroundJobs.SafetyNetReprocess pid)
      drainExtractionWorker tr

      -- Verify the span now has processed_at IS NOT NULL
      processed <- withPool tr.trPool $ DBT.query
        [sql| SELECT COUNT(*)::INT FROM otel_logs_and_spans
              WHERE project_id = ? AND processed_at IS NOT NULL
                AND name = 'GET /api/safetynet/test' |]
        (Only pid) :: IO (V.Vector (Only Int))
      case V.toList processed of
        [Only n] -> n `shouldSatisfy` (>= 1)
        _ -> pass
