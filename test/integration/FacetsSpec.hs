-- | Coverage for the facet pipeline end to end.
--
-- Three layers:
--   * Layer B (handler-shape): seed a 'SummaryDoc' directly, then invoke
--     'SchemaCatalog.getFacetSummary' through the production effect runner
--     and assert the dotted keys + ordering the @GET /api/v1/facets@
--     handler relies on.
--   * Layer C (round trip): observe spans whose walk hits attribute and
--     resource paths, run 'Worker.flushDirty', then read the summary back
--     and assert each expected dotted key is populated.
--   * Layer C (bulk refresh): K projects, each with one dirty key. Confirm
--     'regenerateSummaries' folds in-memory + writes one batched upsert
--     (every project's summary materialises), and that the skip-if-fresh
--     guard suppresses the write on a project whose summary was already
--     refreshed within 30 s.
module FacetsSpec (spec) where

import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Models.Apis.SchemaCatalog qualified as SC
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Pkg.SchemaLearning.Hot qualified as Hot
import Pkg.SchemaLearning.Worker qualified as Worker
import Pkg.TestUtils (TestResources (..), frozenTime, runHasqlEffect, withTestResources)
import Relude
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldContain, shouldSatisfy)
import Utils (toXXHash)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


-- | Project IDs derived from a small int, used for the bulk-refresh test.
mkPid :: Int -> Projects.ProjectId
mkPid n = UUIDId (UUID.fromWords 0 0 0 (fromIntegral n))


clearAll :: TestResources -> [Projects.ProjectId] -> IO ()
clearAll tr pids = withPool tr.trPool $ do
  let arr = PGArray pids
      exec q ps = void (DBT.execute q ps)
  -- FK targets: every test pid needs a row in projects.projects, otherwise
  -- anomaly + summary inserts fail with FK violations. One round-trip each.
  exec
    [sql| INSERT INTO projects.projects (id, title, payment_plan, active, deleted_at, weekly_notif, daily_notif)
          SELECT id, 'facets-test', 'Free', true, NULL, false, false
          FROM unnest(?::uuid[]) AS id
          ON CONFLICT (id) DO NOTHING |]
    (Only arr)
  exec [sql| DELETE FROM apis.schema_catalog WHERE project_id = ANY(?::uuid[]) |] (Only arr)
  exec [sql| DELETE FROM apis.schema_summary WHERE project_id = ANY(?::uuid[]) |] (Only arr)
  exec [sql| DELETE FROM apis.anomalies WHERE project_id = ANY(?::uuid[]) |] (Only arr)
  -- Template cleanup is intentionally omitted here: deleting orphan
  -- templates would race with other specs whose catalog rows still
  -- reference them. The dead-template GC is a daily job in production
  -- and isn't load-bearing for these tests.


-- | Mirrors the HTTP endpoint hash construction in
-- 'ProcessMessage.extractObservation' (httpKeyOf branch). If that
-- production function changes shape, this needs to follow.
keyHashFor :: Projects.ProjectId -> Text -> Text -> Text -> Text
keyHashFor p host method path = toXXHash (T.intercalate "\0" [p.toText, host, method, path])


httpScope :: Text -> Text -> Text -> Catalog.Scope
httpScope host method path =
  Catalog.Scope
    { Catalog.service = Just "checkout-svc"
    , Catalog.spanName = Just (method <> " " <> path)
    , Catalog.kind = Just "server"
    , Catalog.host = Just host
    , Catalog.method = Just method
    , Catalog.urlPath = Just path
    , Catalog.statusCodes = V.singleton 200
    }


-- | One ObservationInput with rich attribute + resource leaves so the
-- catalog walks paths the renderer cares about (http.request.method,
-- service.name, db.operation.name, ...).
richObs :: Projects.ProjectId -> Text -> Hot.ObservationInput
richObs p method =
  Hot.ObservationInput
    { keyKind = Catalog.HttpEndpoint
    , keyHash = keyHashFor p "api.test" method "/orders"
    , scope = httpScope "api.test" method "/orders"
    , walk =
        [ ("http.request.method", V.singleton (AE.String method, Nothing), Catalog.FCAttribute)
        , ("db.operation.name", V.singleton (AE.String "SELECT", Nothing), Catalog.FCAttribute)
        , ("service.name", V.singleton (AE.String "checkout-svc", Nothing), Catalog.FCResource)
        ]
    , timestamp = frozenTime
    }


-- | Seed an apis.schema_summary row whose doc carries the canonical bare
-- catalog paths (no section prefix) so we can assert toFacetSummary prefixes
-- them by category on read.
seedSummary :: TestResources -> Projects.ProjectId -> IO ()
seedSummary tr p = do
  let fld cat = Catalog.FieldStruct mempty mempty cat False
      doc =
        Catalog.SummaryDoc
          { fields =
              HM.fromList
                [ ("http.request.method", fld Catalog.FCAttribute)
                , ("db.operation.name", fld Catalog.FCAttribute)
                , ("service.name", fld Catalog.FCResource)
                ]
          , services = V.singleton "checkout-svc"
          , topValuesByField =
              HM.fromList
                [ ("http.request.method", Catalog.TopK 2 (HM.fromList [("GET", 5), ("POST", 3)]))
                , ("db.operation.name", Catalog.TopK 1 (HM.fromList [("SELECT", 7)]))
                , ("service.name", Catalog.TopK 1 (HM.fromList [("checkout-svc", 8)]))
                ]
          }
  runHasqlEffect tr $ SC.upsertSummary (V.singleton (p, doc))


spec :: Spec
spec = aroundAll withTestResources $
  describe "Facets" $ do
    describe "Layer B — handler-shape contract" $ do
      it "getFacetSummary surfaces dotted keys with section prefixes" $ \tr -> do
        clearAll tr [pid]
        seedSummary tr pid
        sumM <- runHasqlEffect tr $ SC.getFacetSummary pid "otel_logs_and_spans" frozenTime frozenTime
        case sumM of
          Nothing -> fail "expected a summary, got Nothing"
          Just s -> do
            let Catalog.FacetData m = s.facetJson
                keys = HM.keys m
            keys `shouldContain` ["attributes.http.request.method"]
            keys `shouldContain` ["attributes.db.operation.name"]
            keys `shouldContain` ["resource.service.name"]
            -- Values come back in descending count order.
            case HM.lookup "attributes.http.request.method" m of
              Just vs -> map (.value) vs `shouldBe` ["GET", "POST"]
              Nothing -> fail "missing attributes.http.request.method"

      it "returns Nothing for an unknown project" $ \tr -> do
        let unknown = mkPid 9999
        clearAll tr [unknown]
        sumM <- runHasqlEffect tr (SC.getFacetSummary unknown "otel_logs_and_spans" frozenTime frozenTime)
        sumM `shouldSatisfy` isNothing

    describe "Layer C — ingestion → flush → summary" $ do
      it "rich attribute + resource walk produces all expected dotted facet keys" $ \tr -> do
        clearAll tr [pid]
        ref <- newIORef Hot.emptySchemaShardState
        Hot.observeSpans ref Hot.defaultPolicy pid (V.fromList [richObs pid "GET", richObs pid "POST"])
        _ <- runHasqlEffect tr (Worker.flushDirty ref)
        sumM <- runHasqlEffect tr $ SC.getFacetSummary pid "otel_logs_and_spans" frozenTime frozenTime
        case sumM of
          Nothing -> fail "expected a summary after flushDirty"
          Just s -> do
            let Catalog.FacetData m = s.facetJson
                keys = HM.keys m
            keys `shouldContain` ["attributes.http.request.method"]
            keys `shouldContain` ["attributes.db.operation.name"]
            keys `shouldContain` ["resource.service.name"]
            case HM.lookup "attributes.http.request.method" m of
              Just vs -> map (.value) vs `shouldSatisfy` \xs -> "GET" `elem` xs && "POST" `elem` xs
              Nothing -> fail "missing attributes.http.request.method after flush"

    describe "Layer C — bulk refresh" $ do
      it "K projects, K dirty keys: one flushDirty pass refreshes all summaries" $ \tr -> do
        let projs = [mkPid i | i <- [1 .. 5]]
        clearAll tr projs
        ref <- newIORef Hot.emptySchemaShardState
        forM_ projs \p -> Hot.observeSpans ref Hot.defaultPolicy p (V.singleton (richObs p "GET"))
        r <- runHasqlEffect tr (Worker.flushDirty ref)
        r.summariesUpdated `shouldBe` length projs
        -- Every project has a non-empty doc post-flush.
        forM_ projs \p -> do
          sumM <- runHasqlEffect tr $ SC.getSummary p
          sumM `shouldSatisfy` isJust

      -- Relies on 'upsertSummary' stamping @now()@ at seed time and the
      -- whole test completing well inside the 30 s freshness window —
      -- both safe in practice.
      it "skip-if-fresh: projects refreshed within 30s are excluded from the next pass" $ \tr -> do
        let freshP = mkPid 11
            otherPs = [mkPid 12, mkPid 13]
            projs = freshP : otherPs
        clearAll tr projs
        -- Seed freshP with a recognisable doc + recent generated_at (upsertSummary stamps now()).
        seedSummary tr freshP
        ref <- newIORef Hot.emptySchemaShardState
        -- All three get a dirty key; freshP must be skipped on this flush.
        forM_ projs \p -> Hot.observeSpans ref Hot.defaultPolicy p (V.singleton (richObs p "GET"))
        r <- runHasqlEffect tr (Worker.flushDirty ref)
        r.summariesUpdated `shouldBe` length otherPs -- freshP skipped
        -- The two non-fresh projects were summarised this pass.
        forM_ otherPs \p -> do
          s <- runHasqlEffect tr $ SC.getSummary p
          s `shouldSatisfy` isJust
