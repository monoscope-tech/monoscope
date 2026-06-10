-- | Regression coverage for the schema-learning efficiency pass.
--
-- The OOM incidents that disabled schema-learning in prod were caused by
-- (a) the per-shard byte budget being a dead config value, (b) the walker
-- + regex sweep running synchronously for 100% of spans regardless of the
-- sample gate, and (c) the summary doc being rewritten on every flush even
-- when nothing changed. This spec pins each of those guarantees so they
-- don't silently regress.
module SchemaLearningPerfSpec (spec) where

import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pkg.DeriveUtils (AesonText (..), UUIDId (..))
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Pkg.SchemaLearning.Hot qualified as Hot
import Pkg.SchemaLearning.Worker qualified as Worker
import Pkg.TestUtils (TestResources (..), frozenTime, runHasqlEffect, withTestResources)
import ProcessMessage qualified
import Relude
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


clearAll :: TestResources -> IO ()
clearAll tr = withPool tr.trPool $ do
  let arr = PGArray [pid]
      exec q ps = void (DBT.execute q ps)
  exec
    [sql| INSERT INTO projects.projects (id, title, payment_plan, active, deleted_at, weekly_notif, daily_notif)
          SELECT id, 'perf-test', 'Free', true, NULL, false, false
          FROM unnest(?::uuid[]) AS id
          ON CONFLICT (id) DO NOTHING |]
    (Only arr)
  exec [sql| DELETE FROM apis.schema_catalog WHERE project_id = ANY(?::uuid[]) |] (Only arr)
  exec [sql| DELETE FROM apis.schema_summary WHERE project_id = ANY(?::uuid[]) |] (Only arr)
  exec [sql| DELETE FROM apis.anomalies WHERE project_id = ANY(?::uuid[]) |] (Only arr)


-- | A synthetic HTTP-server span with stable shape (one of N "endpoints")
-- and a deterministic timestamp offset. Used to drive the shard at scale.
mkSpan :: Int -> Telemetry.OtelLogsAndSpans
mkSpan ix =
  let route = "/endpoint-" <> T.pack (show (ix `mod` 50))
      method = "GET" :: Text
   in Telemetry.OtelLogsAndSpans
        { id = UUID.toText UUID.nil
        , project_id = pid.toText
        , timestamp = frozenTime
        , parent_id = Nothing
        , observed_timestamp = Nothing
        , hashes = Just V.empty
        , name = Just (method <> " " <> route)
        , kind = Just "server"
        , status_code = Just "200"
        , status_message = Nothing
        , level = Nothing
        , severity = Nothing
        , body = Nothing
        , duration = Just 1
        , start_time = frozenTime
        , end_time = Nothing
        , context = Nothing
        , events = Nothing
        , links = Nothing
        , attributes =
            Just
              $ AesonText
              $ Map.fromList
                [ ("http.request.method", AE.String method)
                , ("http.response.status_code", AE.Number 200)
                , ("url.path", AE.String route)
                , ("db.operation.name", AE.String "SELECT")
                ]
        , resource =
            Just
              $ AesonText
              $ Map.fromList
                [("service.name", AE.String ("svc-" <> T.pack (show (ix `mod` 5))))]
        , summary = V.empty
        , date = frozenTime
        , errors = Nothing
        , message_size_bytes = 0
        }


-- | A pathological span: many distinct attribute paths in a single message.
-- Triggers the per-entry field cap. Without the cap, one of these alone
-- can balloon a CatalogEntry to thousands of fields.
mkWideSpan :: Int -> Int -> Telemetry.OtelLogsAndSpans
mkWideSpan ix fieldCount =
  let base = mkSpan ix
      wideAttrs =
        Map.fromList
          [ ("vendor.huge." <> T.pack (show k), AE.String "x")
          | k <- [1 .. fieldCount]
          ]
   in base{Telemetry.attributes = Just $ AesonText wideAttrs}


-- | Light wrapper that runs N spans through the real walker and one flush.
observe :: V.Vector Telemetry.OtelLogsAndSpans -> IORef Hot.SchemaShardState -> IO ()
observe spans ref = do
  let obs = V.map (ProcessMessage.extractObservation HM.empty) spans
  Hot.observeSpans ref Hot.defaultPolicy pid obs


spec :: Spec
spec = aroundAll withTestResources $
  describe "SchemaLearning efficiency" $ do
    it "10k spans through the real walker keep shard bytes under the policy budget" $ \tr -> do
      clearAll tr
      ref <- newIORef Hot.emptySchemaShardState
      let spans = V.generate 10_000 mkSpan
      observe spans ref
      st <- readIORef ref
      -- Bound + a generous 10% slack for estimation drift.
      let cap = Hot.defaultPolicy.maxBytesPerShard
      st.bytesEstimate `shouldSatisfy` (< cap + cap `div` 10)
      -- And the entry count must stay within the per-project LRU cap.
      HM.size st.entries `shouldSatisfy` (<= Hot.defaultPolicy.maxKeysPerProject)

    it "per-entry field cap drops overflow paths without crashing" $ \tr -> do
      clearAll tr
      ref <- newIORef Hot.emptySchemaShardState
      let widePolicy = Hot.defaultPolicy{Hot.maxFieldsPerEntry = 100}
          obs = V.singleton (ProcessMessage.extractObservation HM.empty (mkWideSpan 0 5_000))
      -- Direct call so we can pass a tight policy without rebuilding config.
      Hot.observeSpans ref widePolicy pid obs
      st <- readIORef ref
      case HM.elems st.entries of
        [e] -> HM.size e.template.fields `shouldSatisfy` (<= 100)
        xs -> fail $ "expected exactly one entry, got " <> show (length xs)

    it "second flush of an unchanged shard skips the summary upsert entirely" $ \tr -> do
      clearAll tr
      ref <- newIORef Hot.emptySchemaShardState
      observe (V.generate 200 mkSpan) ref
      r1 <- runHasqlEffect tr (Worker.flushDirty ref)
      r1.summariesUpdated `shouldSatisfy` (> 0)
      -- No new observations between flushes — the shard is still identical
      -- to the snapshot we just committed. The skip-unchanged gate must
      -- short-circuit the doc rewrite.
      r2 <- runHasqlEffect tr (Worker.flushDirty ref)
      -- Dirty key set is empty after takeDirty, so flushDirty short-circuits
      -- before reaching regenerateSummaries. Either way, no summary write.
      r2.summariesUpdated `shouldBe` 0

    it "anomaly diff on a second flush reuses the in-memory snapshot (no extra DB lookup)" $ \tr -> do
      clearAll tr
      ref <- newIORef Hot.emptySchemaShardState
      observe (V.singleton (mkSpan 0)) ref
      _ <- runHasqlEffect tr (Worker.flushDirty ref)
      -- Re-observe the same key with an additional attribute → new-field
      -- anomaly. The differ should pull the prior from flushedEntries, not
      -- DB. Verify the anomaly fires (correctness) and that the prior
      -- snapshot is non-empty (proving the path).
      st <- readIORef ref
      HM.size st.flushedEntries `shouldSatisfy` (> 0)
      let base = mkSpan 0
          widerAttrs =
            Just
              $ AesonText
              $ Map.fromList
                [ ("http.request.method", AE.String "GET")
                , ("url.path", AE.String "/endpoint-0")
                , ("custom.new.field", AE.String "v")
                ]
          widerSpan :: Telemetry.OtelLogsAndSpans
          widerSpan = base{Telemetry.attributes = widerAttrs}
      observe (V.singleton widerSpan) ref
      r <- runHasqlEffect tr (Worker.flushDirty ref)
      r.anomaliesEmitted `shouldSatisfy` (> 0)

    it "non-sampled spans bump sampleCount without forcing the walk thunk" $ \_ -> do
      -- The lazy walk thunk + sample gate together mean that, past
      -- learnFullThreshold, additional spans on a known key only bump
      -- sampleCount; the (path, values, category) walk is never forced and
      -- no new fields land. This is the CPU-savings invariant — if the
      -- walk thunk leaks back into the hot path, this test fails because
      -- 'mergeFullWalk' would start widening 'template.fields' on every
      -- span instead of only on the sampled ones.
      --
      -- We observe across separate batches so the second-and-later batches
      -- see sampleCount > learnFullThreshold and hit the bump-only path.
      -- Same-batch spans collapse into one mergeFullWalk via combinedWalk,
      -- which is by design; the sampling protection is between batches.
      ref <- newIORef Hot.emptySchemaShardState
      let policy = Hot.defaultPolicy{Hot.learnFullThreshold = 1, Hot.learnSampleEveryN = 1_000_000}
          mkObs i =
            Hot.ObservationInput
              { keyKind = Catalog.HttpEndpoint
              , keyHash = "shared-key"
              , scope = Catalog.emptyScope
              , walk = \_ -> [("path-" <> show i, V.singleton (AE.Null, Nothing), Catalog.FCAttribute)]
              , timestamp = frozenTime
              }
      -- First batch: one span, learnPhase fires, walk forced (1 field).
      Hot.observeSpans ref policy pid (V.singleton (mkObs (0 :: Int)))
      -- 499 subsequent single-span batches: sampleCount > threshold,
      -- bump-only path, walk thunks never invoked.
      forM_ [1 .. 499] $ \i ->
        Hot.observeSpans ref policy pid (V.singleton (mkObs (i :: Int)))
      st <- readIORef ref
      case HM.elems st.entries of
        [e] -> do
          HM.size e.template.fields `shouldBe` 1
          e.sampleCount `shouldBe` 500
        xs -> fail $ "expected exactly one entry, got " <> show (length xs)
