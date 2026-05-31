-- | End-to-end coverage for the in-process schema-learning pipeline:
--   * 'observeSpans' populates a shard
--   * 'flushDirty' writes templates / catalog rows / summary
--   * The anomaly producer emits per-(project, key_hash) anomalies +
--     dedupes across flushes + enqueues a single coalesced @NewAnomaly@ job
--
-- Mirrors the production fiber wiring from 'BackgroundJobs.runSchemaFlusherFiber'
-- but calls 'flushDirty' directly so we can assert on each pass.
module SchemaLearningSpec (spec) where

import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToRow qualified
import Database.PostgreSQL.Simple.Types qualified
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Pkg.SchemaLearning.Hot qualified as Hot
import Pkg.SchemaLearning.Worker qualified as Worker
import Pkg.TestUtils (TestResources (..), frozenTime, runHasqlEffect, withTestResources)
import Relude
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import Utils (toXXHash)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


-- | Per-test reset: schema-learning + anomaly + queued-job state for this project.
-- Order matters: catalog → summary → template (FK), then anomalies + queued jobs.
clearAll :: TestResources -> IO ()
clearAll tr = do
  let exec :: forall ps. (Database.PostgreSQL.Simple.ToRow.ToRow ps) => Database.PostgreSQL.Simple.Types.Query -> ps -> IO ()
      exec q ps = void $ withPool tr.trPool $ DBT.execute q ps
  exec [sql| DELETE FROM apis.schema_catalog WHERE project_id = ? |] (Only pid)
  exec [sql| DELETE FROM apis.schema_summary WHERE project_id = ? |] (Only pid)
  exec
    [sql| DELETE FROM apis.schema_template
          WHERE NOT EXISTS (SELECT 1 FROM apis.schema_catalog
                            WHERE template_hash = apis.schema_template.template_hash) |]
    ()
  exec [sql| DELETE FROM apis.anomalies WHERE project_id = ? |] (Only pid)
  exec
    [sql| DELETE FROM background_jobs
          WHERE payload->>'tag' = 'NewAnomaly' AND payload->>'projectId' = ?::text |]
    (Only pid)


-- | Stable HTTP key. Mirrors the keying in 'ProcessMessage.extractObservation'.
keyHashFor :: Text -> Text -> Text -> Text
keyHashFor host method path = toXXHash (pid.toText <> host <> method <> path)


httpScope :: Text -> Text -> Text -> Catalog.Scope
httpScope host method path =
  Catalog.Scope
    { Catalog.service = Just "test-svc"
    , Catalog.spanName = Just "GET /"
    , Catalog.kind = Just "server"
    , Catalog.host = Just host
    , Catalog.method = Just method
    , Catalog.urlPath = Just path
    , Catalog.statusCodes = V.singleton 200
    }


-- | Build one ObservationInput for a synthetic HTTP span. 'walk' is the
-- (path, values, category) triples the leaf walker would produce.
mkObs
  :: UTCTime
  -> Text
  -> Text
  -> Text
  -> [(Text, AE.Value, Catalog.FieldCategoryEnum)]
  -> Hot.ObservationInput
mkObs ts host method path leaves =
  Hot.ObservationInput
    { keyKind = Catalog.HttpEndpoint
    , keyHash = keyHashFor host method path
    , scope = httpScope host method path
    , walk = \_ -> [(p, V.singleton (v, Nothing), c) | (p, v, c) <- leaves]
    , timestamp = ts
    }


-- | Convenience: observe + flush + return the result.
observeAndFlush :: TestResources -> IORef Hot.SchemaShardState -> [Hot.ObservationInput] -> IO Worker.FlushResult
observeAndFlush tr ref obs = do
  Hot.observeSpans ref Hot.defaultPolicy pid (V.fromList obs)
  runHasqlEffect tr (Worker.flushDirty ref)


countAnomalies :: TestResources -> Text -> IO Int
countAnomalies tr atype = do
  rows :: V.Vector (Only Int) <-
    withPool tr.trPool $ DBT.query
      [sql| SELECT COUNT(*)::int FROM apis.anomalies
            WHERE project_id = ? AND anomaly_type = ?::apis.anomaly_type |]
      (pid, atype)
  pure $ maybe 0 (\(Only n) -> n) (rows V.!? 0)


countAllAnomalies :: TestResources -> IO Int
countAllAnomalies tr = do
  rows :: V.Vector (Only Int) <-
    withPool tr.trPool $ DBT.query
      [sql| SELECT COUNT(*)::int FROM apis.anomalies WHERE project_id = ? |]
      (Only pid)
  pure $ maybe 0 (\(Only n) -> n) (rows V.!? 0)


-- | Number of queued NewAnomaly jobs for this project.
countAnomalyJobs :: TestResources -> IO Int
countAnomalyJobs tr = do
  rows :: V.Vector (Only Int) <-
    withPool tr.trPool $ DBT.query
      [sql| SELECT COUNT(*)::int FROM background_jobs
            WHERE payload->>'tag' = 'NewAnomaly'
              AND payload->>'projectId' = ?::text |]
      (Only pid)
  pure $ maybe 0 (\(Only n) -> n) (rows V.!? 0)


spec :: Spec
spec = aroundAll withTestResources $
  describe "Schema Learning – flush + anomaly producer" $ do
    it "first flush emits endpoint+shape+field+format anomalies and enqueues one job per type" $ \tr -> do
      clearAll tr
      ref <- newIORef Hot.emptySchemaShardState
      let obs = mkObs frozenTime "api.example.com" "GET" "/users"
            [ ("request.body.user.id", AE.String "abc", Catalog.FCRequestBody)
            , ("request.body.user.email", AE.String "x@y", Catalog.FCRequestBody)
            ]
      r <- observeAndFlush tr ref [obs]
      r.dirtyKeys `shouldBe` 1
      r.catalogRowsWritten `shouldBe` 1
      -- 1 endpoint + 1 shape + 2 field (user.id, user.email) + 2 format = 6
      r.anomaliesEmitted `shouldBe` 6
      countAllAnomalies tr `shouldReturn` 6
      countAnomalies tr "endpoint" `shouldReturn` 1
      countAnomalies tr "shape" `shouldReturn` 1
      countAnomalies tr "field" `shouldReturn` 2
      countAnomalies tr "format" `shouldReturn` 2
      -- One NewAnomaly job per (project, anomaly_type) — 4 jobs total.
      countAnomalyJobs tr `shouldReturn` 4

    it "second flush of identical observations dedupes — no new anomalies, no extra jobs" $ \tr -> do
      clearAll tr
      ref <- newIORef Hot.emptySchemaShardState
      let obs = mkObs frozenTime "api.example.com" "GET" "/users"
            [("request.body.id", AE.String "abc", Catalog.FCRequestBody)]
      _ <- observeAndFlush tr ref [obs]
      before <- countAllAnomalies tr
      jobsBefore <- countAnomalyJobs tr
      _ <- observeAndFlush tr ref [obs]
      after <- countAllAnomalies tr
      jobsAfter <- countAnomalyJobs tr
      after `shouldBe` before -- ON CONFLICT DO NOTHING on (project_id, target_hash)
      jobsAfter `shouldBe` jobsBefore -- queued job coalesces in place

    it "adding a new field path emits one shape + one field + one format anomaly (no new endpoint)" $ \tr -> do
      clearAll tr
      ref <- newIORef Hot.emptySchemaShardState
      let host = "api.example.com"; method = "POST"; path = "/orders"
      _ <- observeAndFlush tr ref
        [mkObs frozenTime host method path
          [("request.body.id", AE.String "x", Catalog.FCRequestBody)]]
      before <- countAllAnomalies tr
      _ <- observeAndFlush tr ref
        [mkObs frozenTime host method path
          [ ("request.body.id", AE.String "x", Catalog.FCRequestBody)
          , ("request.body.email", AE.String "a@b", Catalog.FCRequestBody)
          ]]
      countAnomalies tr "endpoint" `shouldReturn` 1 -- unchanged
      shapeN <- countAnomalies tr "shape"
      fieldN <- countAnomalies tr "field"
      formatN <- countAnomalies tr "format"
      shapeN `shouldBe` 2 -- one from initial flush + one from added field
      fieldN `shouldBe` 2 -- id + email
      formatN `shouldBe` 2 -- id + email
      after <- countAllAnomalies tr
      after `shouldSatisfy` (> before)

    it "widening a field type emits a shape + format anomaly only" $ \tr -> do
      clearAll tr
      ref <- newIORef Hot.emptySchemaShardState
      let host = "api.example.com"; method = "GET"; path = "/widen"
      -- First: id is string-only.
      _ <- observeAndFlush tr ref
        [mkObs frozenTime host method path
          [("response.body.id", AE.String "abc", Catalog.FCResponseBody)]]
      before <- countAllAnomalies tr
      -- Now widen with a number occurrence — flushDirty re-walks past learnFullThreshold
      -- but for the very first batch the walk runs unconditionally, so a single
      -- widened observation suffices.
      _ <- observeAndFlush tr ref
        [mkObs frozenTime host method path
          [("response.body.id", AE.Number 42, Catalog.FCResponseBody)]]
      after <- countAllAnomalies tr
      -- Endpoint already exists, no new field path → only shape + format add up.
      (after - before) `shouldSatisfy` (`elem` [1, 2 :: Int])
      countAnomalies tr "endpoint" `shouldReturn` 1
      countAnomalies tr "field" `shouldReturn` 1 -- only the original

    it "buildAnomalyRows produces no rows when prior == current" $ \_tr -> do
      let kh = "khTest"
          fs = Catalog.FieldStruct (HS.fromList [Catalog.FTString]) (HS.fromList ["text"]) Catalog.FCRequestBody False
          tmpl = Catalog.Template Catalog.HttpEndpoint (HM.fromList [("user.id", fs)])
          entry = Catalog.CatalogEntry (httpScope "h" "GET" "/p") tmpl HM.empty HM.empty 1 frozenTime frozenTime True
          k = Hot.SchemaKey pid kh
          priors = HM.fromList [((pid, kh), entry)]
          rows = Worker.buildAnomalyRows priors (V.singleton (k, entry))
      V.length rows `shouldBe` 0

    -- 2026-05-31 incident: a span attribute value containing a NUL byte
    -- (observed from kubernetes events with base64-flagged fields)
    -- propagated into the SummaryDoc, where AE.encode serialised it as the
    -- valid JSON escape ' '. PG's jsonb parser rejects this with
    -- '22P05 unsupported Unicode escape sequence', poisoning every
    -- regenerateSummaries batch the rogue project participated in. With
    -- summary writes permanently failing, monoscope OOM'd at 14 GB in
    -- ~28 min, looping for 11 h overnight.
    it "regression: NUL byte in observed value must not crash schema_summary upsert" $ \tr -> do
      clearAll tr
      ref <- newIORef Hot.emptySchemaShardState
      let obs = mkObs frozenTime "api.example.com" "GET" "/k8s"
            [ ("resource.container.image.name", AE.String "ghcr.io/foo/bar\NULextra", Catalog.FCResource)
            , ("resource.container.restart_count", AE.String "0\NUL", Catalog.FCResource)
            ]
      -- Pre-fix, the upsert throws 22P05 inside flushDirty.
      -- Post-fix, flushDirty returns cleanly with one summary written.
      r <- observeAndFlush tr ref [obs]
      r.summariesUpdated `shouldBe` 1

    it "regression: one project's NUL-tainted doc must not block other projects' summary writes" $ \tr -> do
      clearAll tr
      let pidGood = UUIDId (fromMaybe UUID.nil (UUID.fromString "11111111-1111-1111-1111-111111111111"))
      void $ withPool tr.trPool $ DBT.execute
        [sql| INSERT INTO projects.projects (id, title, payment_plan, active, deleted_at, weekly_notif, daily_notif)
              VALUES (?, 'NUL Resilience Project', 'Startup', true, NULL, false, false)
              ON CONFLICT (id) DO UPDATE SET active = true, deleted_at = NULL |]
          (Only pidGood)
      void $ withPool tr.trPool $ DBT.execute
        [sql| DELETE FROM apis.schema_summary WHERE project_id = ? |] (Only pidGood)
      void $ withPool tr.trPool $ DBT.execute
        [sql| DELETE FROM apis.anomalies WHERE project_id = ? |] (Only pidGood)
      ref <- newIORef Hot.emptySchemaShardState
      -- Tainted observation on the default pid.
      let tainted = mkObs frozenTime "api.example.com" "GET" "/bad"
            [("k", AE.String "x\NULy", Catalog.FCRequestBody)]
      -- Clean observation on a different project — both flushed in the same pass.
      let clean = mkObs frozenTime "api.example.com" "GET" "/ok"
            [("k", AE.String "hello", Catalog.FCRequestBody)]
      Hot.observeSpans ref Hot.defaultPolicy pid (V.singleton tainted)
      Hot.observeSpans ref Hot.defaultPolicy pidGood (V.singleton clean)
      r <- runHasqlEffect tr (Worker.flushDirty ref)
      -- Both projects' summaries must land. Pre-fix, the whole `unnest` batch
      -- failed once one project carried a NUL; with scrubNulValue + per-row
      -- fallback the resilient path returns both.
      r.summariesUpdated `shouldBe` 2
