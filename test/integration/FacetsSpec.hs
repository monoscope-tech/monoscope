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
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Lucid (renderText)
import Models.Apis.SchemaCatalog qualified as SC
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.LogExplorer.Log qualified as LogPage
import Pkg.DeriveUtils (AesonText (..), UUIDId (..))
import Pkg.Parser.Expr qualified as PE
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Pkg.SchemaLearning.Hot qualified as Hot
import Pkg.SchemaLearning.Worker qualified as Worker
import Pkg.TestUtils (TestResources (..), frozenTime, runAsBase, runHasqlEffect, withTestResources)
import ProcessMessage qualified
import Relude
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldContain, shouldNotContain, shouldSatisfy)
import Web.ApiHandlers qualified as ApiH


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


-- | Build a realistic 'OtelLogsAndSpans' row. Callers override attributes,
-- resource, and top-level columns; everything else is a sensible default.
-- The whole point of going through this builder (and 'extractObservation'
-- below) is that the tests then exercise the same code path production does.
mkSpan
  :: Projects.ProjectId
  -> Map Text AE.Value
  -- ^ attributes (e.g. http.request.method → "GET")
  -> Map Text AE.Value
  -- ^ resource (e.g. service.name → "checkout-svc")
  -> (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
  -- ^ (name, kind, status_code, level)
  -> Maybe Telemetry.Severity
  -> Telemetry.OtelLogsAndSpans
mkSpan p attrs resMap (name, kind, statusCode, level) sev =
  Telemetry.OtelLogsAndSpans
    { id = UUID.toText UUID.nil
    , project_id = p.toText
    , timestamp = frozenTime
    , parent_id = Nothing
    , observed_timestamp = Nothing
    , hashes = Just V.empty
    , name = name
    , kind = kind
    , status_code = statusCode
    , status_message = Nothing
    , level = level
    , severity = sev
    , body = Nothing
    , duration = Just 1
    , start_time = frozenTime
    , end_time = Nothing
    , context = Nothing
    , events = Nothing
    , links = Nothing
    , attributes = if Map.null attrs then Nothing else Just (AesonText attrs)
    , resource = if Map.null resMap then Nothing else Just (AesonText resMap)
    , summary = V.empty
    , date = frozenTime
    , errors = Nothing
    , message_size_bytes = 0
    }


-- | A canonical HTTP span — the kind otel-demo produces non-stop. Runs through
-- the real 'extractObservation' walker.
richObs :: Projects.ProjectId -> Text -> Hot.ObservationInput
richObs p method =
  ProcessMessage.extractObservation HM.empty
    $ mkSpan
      p
      ( Map.fromList
          [ ("http.request.method", AE.String method)
          , ("http.response.status_code", AE.Number 200)
          , ("db.operation.name", AE.String "SELECT")
          , ("url.path", AE.String "/orders")
          ]
      )
      (Map.singleton "service.name" (AE.String "checkout-svc"))
      (Just (method <> " /orders"), Just "server", Just "200", Nothing)
      Nothing


-- | Variant that also populates log-level columns so we can assert the
-- top-level walk (name/kind/level/status_code/severity.*) reaches the
-- summary pipeline.
richObsWithTopLevel :: Projects.ProjectId -> Text -> Hot.ObservationInput
richObsWithTopLevel p method =
  ProcessMessage.extractObservation HM.empty
    $ mkSpan
      p
      ( Map.fromList
          [ ("http.request.method", AE.String method)
          , ("db.operation.name", AE.String "SELECT")
          ]
      )
      (Map.singleton "service.name" (AE.String "checkout-svc"))
      (Just (method <> " /orders"), Just "server", Just "200", Just "INFO")
      ( Just
          Telemetry.Severity
            { severity_text = Just Telemetry.SLInfo
            , severity_number = 9
            }
      )


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
spec = around withTestResources $
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
      -- This test goes through the production walker ('extractObservation'),
      -- not a hand-built ObservationInput. That's the only way to catch
      -- under-walking bugs like #401's HTTP branch dropping the generic
      -- attribute + resource walks.
      it "every Facet.path the sidebar advertises is populated after a real HTTP span flush" $ \tr -> do
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
                -- The full set of paths a real HTTP span must populate.
                -- Asserting one-by-one keeps the failure message specific.
                mustHave =
                  [ "attributes.http.request.method"
                  , "attributes.http.response.status_code"
                  , "attributes.db.operation.name"
                  , "attributes.url.path"
                  , "resource.service.name"
                  , "name"
                  , "kind"
                  , "status_code"
                  ]
            forM_ mustHave $ \k -> keys `shouldContain` [k]
            case HM.lookup "attributes.http.request.method" m of
              Just vs -> map (.value) vs `shouldSatisfy` \xs -> "GET" `elem` xs && "POST" `elem` xs
              Nothing -> fail "missing attributes.http.request.method after flush"
            case HM.lookup "resource.service.name" m of
              Just vs -> map (.value) vs `shouldContain` ["checkout-svc"]
              Nothing -> fail "missing resource.service.name after flush"

      it "log-style spans expose severity + level facets through the top-level walk" $ \tr -> do
        clearAll tr [pid]
        ref <- newIORef Hot.emptySchemaShardState
        Hot.observeSpans ref Hot.defaultPolicy pid (V.singleton (richObsWithTopLevel pid "POST"))
        _ <- runHasqlEffect tr (Worker.flushDirty ref)
        sumM <- runHasqlEffect tr $ SC.getFacetSummary pid "otel_logs_and_spans" frozenTime frozenTime
        case sumM of
          Nothing -> fail "expected a summary"
          Just s -> do
            let Catalog.FacetData m = s.facetJson
                keys = HM.keys m
            forM_ ["name", "kind", "level", "status_code", "severity.severity_text", "severity.severity_number"] $ \k ->
              keys `shouldContain` [k]
            case HM.lookup "level" m of
              Just vs -> map (.value) vs `shouldContain` ["INFO"]
              Nothing -> fail "missing level values"

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

    describe "Layer A — handler wire shape" $ do
      it "GET /api/v1/facets returns dotted keys only (no triple-underscore)" $ \tr -> do
        clearAll tr [pid]
        seedSummary tr pid
        body <- runAsBase tr $ ApiH.apiFacets pid Nothing Nothing Nothing Nothing
        case body of
          AE.Object o -> do
            let keys = AEK.toText <$> AEKM.keys o
            keys `shouldContain` ["attributes.http.request.method"]
            keys `shouldNotContain` ["attributes___http___request___method"]
            not (any (T.isInfixOf "___") keys) `shouldBe` True
          _ -> fail "apiFacets did not return a JSON object"

    describe "Layer A — sidebar HTML render" $ do
      it "renderFacets emits one row per Facet.path, keyed by the canonical dotted form" $ \_ -> do
        let m =
              HM.fromList
                [ ("attributes.http.request.method", [Catalog.FacetValue "GET" 3])
                , ("resource.service.name", [Catalog.FacetValue "checkout-svc" 5])
                , ("level", [Catalog.FacetValue "INFO" 7])
                ]
            summary =
              Catalog.FacetSummary
                { id = UUID.nil
                , projectId = pid.toText
                , tableName = "otel_logs_and_spans"
                , facetJson = Catalog.FacetData m
                }
            html = toText $ renderText (LogPage.renderFacets summary)
        T.isInfixOf "data-field=\"attributes.http.request.method\"" html `shouldBe` True
        T.isInfixOf "data-field=\"resource.service.name\"" html `shouldBe` True
        T.isInfixOf "data-field=\"level\"" html `shouldBe` True
        -- The facet path + value are carried on data-field/data-value; the
        -- hyperscript pseudo-command reads them off the element at click time
        -- (no inline interpolation to escape).
        T.isInfixOf "data-value=\"GET\"" html `shouldBe` True
        T.isInfixOf "toggleSubQuery(@data-field" html `shouldBe` True
        T.isInfixOf "on #filterElement" html `shouldBe` True
        -- Section headers for groups that have populated facets show up.
        T.isInfixOf "Common Filters" html `shouldBe` True
        -- Facets without values still render with the empty-state row.
        T.isInfixOf "no values in window" html `shouldBe` True

      it "every Facet.path is fast (in flattenedOtelAttributes or topLevelOtelColumns)" $ \_ ->
        all
          (\f -> S.member f.path PE.flattenedOtelAttributes || S.member f.path PE.topLevelOtelColumns)
          LogPage.facetDefs
          `shouldBe` True
