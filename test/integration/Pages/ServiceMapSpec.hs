module Pages.ServiceMapSpec (spec) where

import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as V
import Lucid qualified
import Models.Telemetry.ServiceGraph (MapStats (..), NodeKind (..), ServiceEdge (..), ServiceGraph (..), ServiceNode (..), projectsWithSpansInRange, rollupServiceEdges, serviceGraphForRange, upsertServiceDependencyEdges)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.ServiceMap qualified as ServiceMap
import Pages.Telemetry qualified as Trace
import Pkg.TestClock (setTestTime)
import Pkg.TestUtils
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as PT
import Relude
import Test.Hspec


-- A production-shaped trace across two instrumented services that also talks to an
-- uninstrumented database and an uninstrumented queue:
--
--   (entry) -> gateway --client--> checkout --client--> postgres "orders"   [inferred db]
--                                           --producer-> "orders.v1"        [inferred queue]
ingestFixture :: TestResources -> Text -> UTCTime -> IO ()
ingestFixture tr apiKey ts = do
  let hexId = T.replace "-" "" . UUID.toText <$> nextRandom
  (trId, gwSid, gwClientSid, coSid, dbSid, qSid) <- (,,,,,) <$> hexId <*> hexId <*> hexId <*> hexId <*> hexId <*> hexId
  let svc s = mkResource apiKey [mkAttr "service.name" s]
      span' sid parent name kind resource attrs =
        ingestSpanReq tr $ withSpanKind kind $ mkSpanRequest trId sid parent name [] Nothing attrs resource ts
  -- gateway receives the request, then calls checkout
  span' gwSid Nothing "GET /checkout" PT.Span'SPAN_KIND_SERVER (svc "gateway") []
  span' gwClientSid (Just gwSid) "POST checkout" PT.Span'SPAN_KIND_CLIENT (svc "gateway") []
  span' coSid (Just gwClientSid) "POST /checkout" PT.Span'SPAN_KIND_SERVER (svc "checkout") []
  -- checkout's own uninstrumented dependencies: nothing answers these spans
  span' dbSid (Just coSid) "pg.query" PT.Span'SPAN_KIND_CLIENT (svc "checkout") [mkAttr "db.system.name" "postgresql", mkAttr "db.namespace" "orders"]
  span' qSid (Just coSid) "publish" PT.Span'SPAN_KIND_PRODUCER (svc "checkout") [mkAttr "server.address" "orders.v1"]


-- | Roll the fixture's bucket and read the graph back, exactly as the page does.
rollAndRead :: TestResources -> UTCTime -> IO ServiceGraph
rollAndRead tr ts = runTestBg ts tr do
  edges <- rollupServiceEdges False testPid (addUTCTime (-300) ts) (addUTCTime 300 ts)
  upsertServiceDependencyEdges testPid ts edges
  serviceGraphForRange testPid (addUTCTime (-600) ts) (addUTCTime 600 ts)


-- | Assert every needle is present, naming the ones that are not. A bare
-- @shouldSatisfy (T.isInfixOf s)@ dumps the whole 100KB page and never says which string it
-- was looking for.
shouldContainAll :: Text -> [Text] -> Expectation
shouldContainAll haystack needles =
  case filter (not . (`T.isInfixOf` haystack)) needles of
    [] -> pass
    missing -> expectationFailure $ "missing from rendered page: " <> show missing


edgePairs :: ServiceGraph -> [(Text, Text)]
edgePairs g = L.sort [(e.source, e.target) | e <- V.toList g.edges]


spec :: Spec
spec = around withTestResources do
  describe "service map" do
    it "serviceMapLegend_iconsExistInRegularSprite" \_ -> do
      sprite <- readFileText "static/public/assets/svgs/fa-sprites/regular.svg"
      sprite `shouldContainAll` ["id=\"diagram-project\"", "id=\"arrow-right-to-bracket\""]

    -- An empty project must produce an empty graph with no error. The `error` field exists
    -- precisely so a failed query can't masquerade as "you have no services"; this pins the
    -- other side of that contract.
    it "emptyProject_producesEmptyGraphWithoutError" \tr -> do
      g <- runTestBg frozenTime tr $ serviceGraphForRange testPid (addUTCTime (-600) frozenTime) frozenTime
      V.toList g.nodes `shouldBe` []
      V.toList g.edges `shouldBe` []
      g.error `shouldBe` Nothing

    -- The whole point of the map: it shows dependencies nothing is instrumented to report.
    -- No span carries service.name=orders — the database node is inferred from a client span
    -- that no server span answered.
    it "rollup_derivesEntryServiceAndInferredDependencyEdges" \tr -> do
      apiKey <- createTestAPIKey tr testPid "service-map-key"
      ingestFixture tr apiKey frozenTime
      g <- rollAndRead tr frozenTime

      edgePairs g
        `shouldBe` L.sort
          [ ("", "gateway")
          , ("gateway", "checkout")
          , ("checkout", "db:orders")
          , ("checkout", "queue:orders.v1")
          ]
      L.sort [(n.key, n.kind, n.inferred) | n <- V.toList g.nodes]
        `shouldBe` L.sort
          [ ("", NKEntry, False)
          , ("gateway", NKService, False)
          , ("checkout", NKService, False)
          , ("db:orders", NKDatabase, True)
          , ("queue:orders.v1", NKQueue, True)
          ]
      -- The prefix keeps two systems from colliding on a bare name; the label drops it.
      (L.sort [n.label | n <- V.toList g.nodes, n.inferred]) `shouldBe` ["orders", "orders.v1"]
      g.truncated `shouldBe` False

    -- The hourly catch-up pass and DLQ replays re-roll buckets that already exist, so the
    -- upsert must replace rather than accumulate.
    it "rollup_isIdempotentAcrossReRuns" \tr -> do
      apiKey <- createTestAPIKey tr testPid "service-map-idem-key"
      ingestFixture tr apiKey frozenTime
      first' <- rollAndRead tr frozenTime
      second' <- rollAndRead tr frozenTime
      [e.stats.requests | e <- V.toList second'.edges] `shouldBe` [e.stats.requests | e <- V.toList first'.edges]
      V.length second'.edges `shouldBe` V.length first'.edges

    -- The dispatcher fans out over this list, so anything it over-reports is a self-join
    -- run against a project with nothing to join. An active project that sent no span this
    -- bucket must not appear — that is the whole point of discovering rather than fanning
    -- out over projects.activeProjects, which returned all 1726 of them in production.
    it "projectDiscovery_listsOnlyProjectsThatSentSpansInTheBucket" \tr -> do
      apiKey <- createTestAPIKey tr testPid "service-map-discovery-key"
      ingestFixture tr apiKey frozenTime
      (inBucket, idleBucket) <- runTestBg frozenTime tr do
        (,)
          <$> projectsWithSpansInRange False (addUTCTime (-300) frozenTime) (addUTCTime 300 frozenTime)
          <*> projectsWithSpansInRange False (addUTCTime (-86400) frozenTime) (addUTCTime (-86100) frozenTime)
      inBucket `shouldBe` [testPid]
      idleBucket `shouldBe` [] -- testPid is active, but silent in this window
    it "rollup_excludesSpansOutsideTheRange" \tr -> do
      apiKey <- createTestAPIKey tr testPid "service-map-range-key"
      ingestFixture tr apiKey (addUTCTime (-86400) frozenTime)
      g <- rollAndRead tr frozenTime
      V.toList g.edges `shouldBe` []

    it "errorSpans_surfaceOnTheEdgeTheyOccurredOn" \tr -> do
      apiKey <- createTestAPIKey tr testPid "service-map-error-key"
      let hexId = T.replace "-" "" . UUID.toText <$> nextRandom
      (trId, gwSid, gwClientSid, coSid) <- (,,,) <$> hexId <*> hexId <*> hexId <*> hexId
      let svc s = mkResource apiKey [mkAttr "service.name" s]
      ingestSpanReq tr $ withSpanKind PT.Span'SPAN_KIND_SERVER $ mkSpanRequest trId gwSid Nothing "GET /checkout" [] Nothing [] (svc "gateway") frozenTime
      ingestSpanReq tr $ withSpanKind PT.Span'SPAN_KIND_CLIENT $ mkSpanRequest trId gwClientSid (Just gwSid) "POST checkout" [] Nothing [] (svc "gateway") frozenTime
      ingestSpanReq tr
        $ withSpanStatus PT.Status'STATUS_CODE_ERROR
        $ withSpanKind PT.Span'SPAN_KIND_SERVER
        $ mkSpanRequest trId coSid (Just gwClientSid) "POST /checkout" [] Nothing [] (svc "checkout") frozenTime
      g <- rollAndRead tr frozenTime
      [(e.source, e.target, e.stats.errors) | e <- V.toList g.edges]
        `shouldMatchList` [("", "gateway", 0), ("gateway", "checkout", 1)]

    -- The end-to-end contract, asserted the way a user meets it: ingest spans, load the
    -- page, and read the dependency table. Everything above this pins query mechanics; this
    -- pins that loading the URL actually shows you your architecture.
    it "page_showsTheWholeDependencyGraphIncludingUninstrumentedPeers" \tr -> do
      apiKey <- createTestAPIKey tr testPid "service-map-page-key"
      ingestFixture tr apiKey frozenTime
      _ <- rollAndRead tr frozenTime
      -- Rolled buckets are always in the past (the dispatcher lags 10 minutes), and the read
      -- range is half-open, so advance the clock rather than querying a bucket at exactly now.
      setTestTime tr.trTestClock (addUTCTime 600 frozenTime)
      (_, page) <- testServant tr $ ServiceMap.serviceMapGetH testPid Nothing Nothing (Just "1H")
      let ServiceMap.ServiceMapPage (PageCtx conf pd) = page
          html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
      conf.pageTitle `shouldBe` "Service Map"

      -- Every hop the fixture produced is reachable from the handler's own payload...
      edgePairs pd.graph
        `shouldBe` L.sort
          [ ("", "gateway")
          , ("gateway", "checkout")
          , ("checkout", "db:orders")
          , ("checkout", "queue:orders.v1")
          ]
      -- ...and is legible in the rendered page without running any JavaScript: the
      -- dependency table is what a shared link opened on a phone actually shows.
      html `shouldContainAll` ["Entry point", "gateway", "checkout", "orders", "orders.v1", "Dependencies (4)"]
      -- The canvas hydrates from the embedded payload rather than an extra round trip.
      html `shouldContainAll` ["data-service-map", "global-service-map-data", "global-service-map-colors"]
      html `shouldContainAll` ["aria-label=\"Service colors\"", "data-service-color=\"gateway\"", "data-service-color=\"checkout\""]
      html `shouldContainAll` ["Events", "Metrics", "Service Map"]

    it "serviceMapFilter_quotesHyphenatedEventNameForHyperscript" \tr -> do
      (_, page) <- testServant tr $ ServiceMap.serviceMapGetH testPid Nothing Nothing (Just "1H")
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
      html `shouldContainAll` ["send &quot;service-map-filter&quot;(q: my value)"]

    -- The trace-level map: same grammar, scoped to one request. It is derived from spans
    -- already on the page (no extra query), so the whole contract is "load the trace and the
    -- Map tab is there, wired up, carrying this trace's flow".
    it "traceView_rendersAMapTabCarryingThisTracesFlow" \tr -> do
      apiKey <- createTestAPIKey tr testPid "trace-map-key"
      let hexId = T.replace "-" "" . UUID.toText <$> nextRandom
      (trId, gwSid, gwClientSid, coSid, dbSid) <- (,,,,) <$> hexId <*> hexId <*> hexId <*> hexId <*> hexId
      let svc s = mkResource apiKey [mkAttr "service.name" s]
          span' sid parent name kind resource attrs =
            ingestSpanReq tr $ withSpanKind kind $ mkSpanRequest trId sid parent name [] Nothing attrs resource frozenTime
      span' gwSid Nothing "GET /checkout" PT.Span'SPAN_KIND_SERVER (svc "gateway") []
      span' gwClientSid (Just gwSid) "POST checkout" PT.Span'SPAN_KIND_CLIENT (svc "gateway") []
      span' coSid (Just gwClientSid) "POST /checkout" PT.Span'SPAN_KIND_SERVER (svc "checkout") []
      span' dbSid (Just coSid) "pg.query" PT.Span'SPAN_KIND_CLIENT (svc "checkout") [mkAttr "db.system.name" "postgresql", mkAttr "db.namespace" "orders"]

      (_, page) <- testServant tr $ Trace.traceH testPid trId (Just frozenTime) Nothing Nothing
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page

      -- The tab sits beside the existing three and drives the same navigatable() mechanism
      -- the timeline depends on — converting this strip to CSS radios has broken it before.
      -- Lucid escapes attribute values, so assert the form the browser actually receives.
      html `shouldContainAll` ["navigatable(this, &#39;#service_map&#39;", "Waterfall", "Timeline", "Services", ">Map<"]

      -- The graph is serialised into the page, so a shared trace link needs no fetch. It
      -- must carry both instrumented services, the inferred database nobody instruments,
      -- and the per-service share of execution time that sizes the nodes.
      html `shouldContainAll` ["trace-service-map-", "\"gateway\"", "\"checkout\"", "\"orders\"", "duration_share"]
