module Pages.ServiceMapSpec (spec) where

import Control.Exception (evaluate)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as V
import Lucid qualified
import Models.Telemetry.ServiceGraph (Collapse (..), EdgeSample (..), MapStats (..), NodeKind (..), ServiceEdge (..), ServiceGraph (..), ServiceNode (..), buildServiceGraph, drawnEdges, drawnNodes, projectsWithSpansInRange, rollupServiceEdges, serviceGraphForRange, serviceMapFanout, serviceMapNodeCap, singletonLatency, upsertServiceDependencyEdges)
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


-- | The same shape as 'ingestFixture' but split across two deployment environments, so the
-- Env facet has something real to group, offer and filter on.
ingestEnvFixture :: TestResources -> Text -> UTCTime -> IO ()
ingestEnvFixture tr apiKey ts = do
  let hexId = T.replace "-" "" . UUID.toText <$> nextRandom
  let svc s env = mkResource apiKey [mkAttr "service.name" s, mkAttr "deployment.environment.name" env]
      span' trId sid parent name kind resource attrs =
        ingestSpanReq tr $ withSpanKind kind $ mkSpanRequest trId sid parent name [] Nothing attrs resource ts
  -- prod: gateway -> checkout -> postgres
  (t1, gw, gwc, co, db) <- (,,,,) <$> hexId <*> hexId <*> hexId <*> hexId <*> hexId
  span' t1 gw Nothing "GET /checkout" PT.Span'SPAN_KIND_SERVER (svc "gateway" "prod") []
  span' t1 gwc (Just gw) "POST checkout" PT.Span'SPAN_KIND_CLIENT (svc "gateway" "prod") []
  span' t1 co (Just gwc) "POST /checkout" PT.Span'SPAN_KIND_SERVER (svc "checkout" "prod") []
  span' t1 db (Just co) "pg.query" PT.Span'SPAN_KIND_CLIENT (svc "checkout" "prod") [mkAttr "db.system.name" "postgresql"]
  -- staging: the same two services, one hop, nothing downstream
  (t2, gw2, gwc2, co2) <- (,,,) <$> hexId <*> hexId <*> hexId <*> hexId
  span' t2 gw2 Nothing "GET /checkout" PT.Span'SPAN_KIND_SERVER (svc "gateway" "staging") []
  span' t2 gwc2 (Just gw2) "POST checkout" PT.Span'SPAN_KIND_CLIENT (svc "gateway" "staging") []
  span' t2 co2 (Just gwc2) "POST /checkout" PT.Span'SPAN_KIND_SERVER (svc "checkout" "staging") []


-- | Roll the fixture's bucket and read the graph back, exactly as the page does.
rollAndRead :: TestResources -> UTCTime -> IO ServiceGraph
rollAndRead tr ts = runTestBg ts tr do
  edges <- rollupServiceEdges False testPid (addUTCTime (-300) ts) (addUTCTime 300 ts)
  upsertServiceDependencyEdges testPid ts edges
  serviceGraphForRange testPid Nothing (addUTCTime (-600) ts) (addUTCTime 600 ts)


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


-- | One hop from @api@ into an uninstrumented peer, for exercising the grouping fold
-- without paying for OTLP ingestion of a few hundred tenant subdomains.
hop :: Text -> NodeKind -> Int64 -> EdgeSample
hop target kind reqs = EdgeSample ("api", NKService) (target, kind) reqs 0 (singletonLatency 1_000_000) 0


nodeBy :: ServiceGraph -> Text -> Maybe ServiceNode
nodeBy g k = V.find (\n -> n.key == k) g.nodes


spec :: Spec
spec = around withTestResources do
  describe "service map" do
    it "serviceMapLegend_iconsExistInRegularSprite" \_ -> do
      sprite <- readFileText "static/public/assets/svgs/fa-sprites/regular.svg"
      sprite `shouldContainAll` ["id=\"diagram-project\"", "id=\"arrow-right-to-bracket\""]

    -- The bug this guards: a project whose one instrumented service fans out to hundreds of
    -- per-tenant hostnames rendered every hostname as its own node, which the layered layout
    -- then crushed into an unreadable column. Peers on a shared registrable domain collapse
    -- into one head, and both levels travel so the client can expand without a refetch.
    it "buildServiceGraph_collapsesPerTenantFanOutButNotLoneOrNamedPeers" \_ -> do
      let tenants = ["http:t" <> show n <> ".myshopify.com" | n <- [1 :: Int .. 3]]
          graph =
            buildServiceGraph 60 serviceMapNodeCap serviceMapFanout Nothing
              $ [hop t NKExternal 100 | t <- tenants]
              <> [hop "http:api.paystack.co" NKExternal 50, hop "db:redis" NKDatabase 900]

      -- The head stands in for its members, is labelled by the domain, and counts them.
      (nodeBy graph "grp:http:myshopify.com" <&> \n -> (n.label, n.memberCount, n.stats.requests))
        `shouldBe` Just ("myshopify.com", Just 3, 300)
      -- Members ride along, tagged, so expanding is a re-layout rather than a second query.
      map (.groupKey) (mapMaybe (nodeBy graph) tenants) `shouldBe` replicate 3 (Just "grp:http:myshopify.com")
      -- A group of one is not a group: a lone peer stays itself rather than becoming "×1",
      -- and a database is named by its system already, so it never groups.
      map (\k -> nodeBy graph k <&> \n -> (n.memberCount, n.groupKey)) ["http:api.paystack.co", "db:redis"]
        `shouldBe` [Just (Nothing, Nothing), Just (Nothing, Nothing)]
      -- Both levels of edge are present; the renderer picks by expansion state.
      edgePairs graph
        `shouldBe` L.sort ([("api", t) | t <- tenants] <> [("api", "grp:http:myshopify.com"), ("api", "http:api.paystack.co"), ("api", "db:redis")])

    -- The cap counts what the map draws, so a collapsed group costs one slot however many
    -- peers it holds. Before grouping, 150 tenants exhausted the whole budget.
    it "buildServiceGraph_capsDrawnNodesNotGroupMembers" \_ -> do
      let graph = buildServiceGraph 60 3 serviceMapFanout Nothing [hop ("http:t" <> show n <> ".myshopify.com") NKExternal 1 | n <- [1 :: Int .. 40]]
      (nodeBy graph "grp:http:myshopify.com" <&> (.memberCount)) `shouldBe` Just (Just 40)
      graph.truncated `shouldBe` False
      length (V.filter (isNothing . (.groupKey)) graph.nodes) `shouldBe` 2 -- api + the one head

    -- Domain grouping cannot help here: every peer is a different merchant's own domain,
    -- which is what a real shipping-API project actually looks like. Only a volume-ranked
    -- fold bounds that, and bounding it is what lets the map carry per-node detail at all.
    it "buildServiceGraph_foldsTheUnrelatedLongTailUnderItsBusiestCaller" \_ -> do
      let peers = [(n, "http:merchant" <> show n <> ".com") | n <- [1 :: Int .. 20]]
          graph = buildServiceGraph 60 serviceMapNodeCap (CollapseTo 5) Nothing [hop p NKExternal (fromIntegral (100 - n)) | (n, p) <- peers]
          drawn = drawnNodes graph

      -- The five busiest stay named; the remaining fifteen become one node that says so.
      map (.label) (V.toList drawn) `shouldContain` ["15 more dependencies"]
      (nodeBy graph "rest:api" <&> \n -> (n.memberCount, n.stats.requests))
        `shouldBe` Just (Just 15, sum [fromIntegral (100 - n) | (n, _) <- drop 5 peers])
      -- api + 5 named + 1 fold. The other 15 ride along for expansion but are not drawn.
      V.length drawn `shouldBe` 7
      V.length graph.nodes `shouldBe` 22

      -- The counts the page prints must describe the picture, not the payload. Counting the
      -- folded members here is what made the header claim "234 services" for a map of 150.
      V.length (drawnEdges graph) `shouldBe` 6
      map (.label) (V.toList (V.filter (\n -> n.memberCount == Just 15) drawn)) `shouldBe` ["15 more dependencies"]

    -- A trace map is one request's path; folding two of its five hops into "3 more" would
    -- hide exactly what the reader opened it to see.
    it "buildServiceGraph_neverCollapsesATraceMap" \_ -> do
      let peers = ["http:merchant" <> show n <> ".com" | n <- [1 :: Int .. 20]]
          graph = buildServiceGraph 0 serviceMapNodeCap CollapseOff (Just 1_000_000) [hop p NKExternal 1 | p <- peers]
      V.length (drawnNodes graph) `shouldBe` 21 -- api + all 20, nothing folded
      V.toList (V.filter (isJust . (.memberCount)) graph.nodes) `shouldBe` []

    -- The state that a stale haddock claimed could not exist: a domain head that itself
    -- falls into the long tail is a head *and* a member. It is what makes expansion nest —
    -- "N more dependencies" opens into "myshopify.com x3", which opens into its own members
    -- — and nothing pinned it, which is how the wrong invariant survived in the type.
    it "buildServiceGraph_aDomainHeadInTheTailIsBothHeadAndMember" \_ -> do
      let tenants = ["http:t" <> show n <> ".myshopify.com" | n <- [1 :: Int .. 3]]
          loud = ["http:loud" <> show n <> ".example" <> show n <> ".com" | n <- [1 :: Int .. 4]]
          graph =
            buildServiceGraph 60 serviceMapNodeCap (CollapseTo 2) Nothing
              $ [hop t NKExternal 1 | t <- tenants]
              <> [hop l NKExternal 5000 | l <- loud]

      -- The quiet domain group is outranked by the loud singletons, so it folds into the
      -- tail while still standing in for its own three members.
      (nodeBy graph "grp:http:myshopify.com" <&> \n -> (n.memberCount, isJust n.groupKey))
        `shouldBe` Just (Just 3, True)
      -- And its members still point at it, not at the tail head above it.
      map (.groupKey) (mapMaybe (nodeBy graph) tenants) `shouldBe` replicate 3 (Just "grp:http:myshopify.com")

    -- A regression guard with a stopwatch, because the bug it pins had no wrong output —
    -- only a cost. `domainOf` was written as a function of the key, so the collapse map was
    -- rebuilt on every lookup: quadratic in node count, and a 221-node project spent seconds
    -- in the fold. 400 nodes finishes in milliseconds when it is bound as a value, and takes
    -- long enough to be unmissable when it is not.
    it "buildServiceGraph_foldsALargeGraphWithoutQuadraticBlowup" \_ -> do
      let peers = [(n, "http:merchant" <> show n <> ".example" <> show (n `mod` 40) <> ".com") | n <- [1 :: Int .. 400]]
          samples = [hop p NKExternal (fromIntegral n) | (n, p) <- peers]
      start <- getCurrentTime
      let graph = buildServiceGraph 60 serviceMapNodeCap serviceMapFanout Nothing samples
      _ <- evaluate (V.length graph.nodes)
      elapsed <- flip diffUTCTime start <$> getCurrentTime
      V.length (drawnNodes graph) `shouldSatisfy` (> 0)
      elapsed `shouldSatisfy` (< 2.0)

    -- The Env facet is a rollup dimension, so it has to survive the whole round trip: read
    -- off the span's resource, grouped into the rollup, offered as the facet's options,
    -- honoured as a filter, and finally rendered as a control on the page.
    it "serviceMap_environmentFacetRoundTripsFromSpanToRenderedControl" \tr -> do
      apiKey <- createTestAPIKey tr testPid "service-map-env-key"
      ingestEnvFixture tr apiKey frozenTime
      g <- runTestBg frozenTime tr do
        edges <- rollupServiceEdges False testPid (addUTCTime (-300) frozenTime) (addUTCTime 300 frozenTime)
        upsertServiceDependencyEdges testPid frozenTime edges
        serviceGraphForRange testPid Nothing (addUTCTime (-600) frozenTime) (addUTCTime 600 frozenTime)

      -- Both environments are offered, sorted, however many hops each contributed.
      V.toList g.environments `shouldBe` ["prod", "staging"]

      -- Selecting one keeps only its hops. staging has the single gateway->checkout call.
      prod <- runTestBg frozenTime tr $ serviceGraphForRange testPid (Just "prod") (addUTCTime (-600) frozenTime) (addUTCTime 600 frozenTime)
      staging <- runTestBg frozenTime tr $ serviceGraphForRange testPid (Just "staging") (addUTCTime (-600) frozenTime) (addUTCTime 600 frozenTime)
      map fst (edgePairs staging) `shouldNotContain` ["checkout"] -- no db/queue hops in staging
      length (edgePairs prod) `shouldSatisfy` (> length (edgePairs staging))

      -- An environment nothing reported yields an empty map rather than silently falling
      -- back to "all", which would show prod traffic under someone else's label.
      none <- runTestBg frozenTime tr $ serviceGraphForRange testPid (Just "qa") (addUTCTime (-600) frozenTime) (addUTCTime 600 frozenTime)
      V.toList none.edges `shouldBe` []

      -- And the control itself renders, with an option per environment plus "All". The clock
      -- advances first for the same reason the page test does it: the handler's 1H window is
      -- relative to now, and a bucket sitting exactly at now falls outside a half-open range.
      setTestTime tr.trTestClock (addUTCTime 600 frozenTime)
      (_, page) <- testServant tr $ ServiceMap.serviceMapGetH testPid Nothing Nothing (Just "1H") (Just "prod")
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
      html `shouldContainAll` ["?env=prod", "?env=staging", ">All<"]

    -- An empty project must produce an empty graph with no error. The `error` field exists
    -- precisely so a failed query can't masquerade as "you have no services"; this pins the
    -- other side of that contract.
    it "emptyProject_producesEmptyGraphWithoutError" \tr -> do
      g <- runTestBg frozenTime tr $ serviceGraphForRange testPid Nothing (addUTCTime (-600) frozenTime) frozenTime
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
      (_, page) <- testServant tr $ ServiceMap.serviceMapGetH testPid Nothing Nothing (Just "1H") Nothing
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

    -- Regression (4615b0c): the filter used to `send "service-map-filter"(q: my value)`,
    -- but hyperscript parses an event name as an identifier path, so the dashed name was a
    -- parse error and the whole attribute was dropped — the filter silently did nothing in
    -- prod. This asserted the dashed `send`, so after the fix it was guarding the bug.
    it "serviceMapFilter_wiresTheInputWithoutADashedSend" \tr -> do
      (_, page) <- testServant tr $ ServiceMap.serviceMapGetH testPid Nothing Nothing (Just "1H") Nothing
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
      html `shouldContainAll` ["call window.serviceMapFilter(me.value)"]
      T.isInfixOf "service-map-filter&quot;(" html `shouldBe` False

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

      (_, page) <- testServant tr $ Trace.traceH testPid trId (Just frozenTime) Nothing Nothing Nothing
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page

      -- The tab sits beside the existing three and drives the same navigatable() mechanism
      -- the timeline depends on — converting this strip to CSS radios has broken it before.
      -- Lucid escapes attribute values, so assert the form the browser actually receives.
      html `shouldContainAll` ["navigatable(this, &#39;#service_map&#39;", "Waterfall", "Timeline", "Services", ">Map<"]

      -- The graph is serialised into the page, so a shared trace link needs no fetch. It
      -- must carry both instrumented services, the inferred database nobody instruments,
      -- and the per-service share of execution time that sizes the nodes.
      html `shouldContainAll` ["trace-service-map-", "\"gateway\"", "\"checkout\"", "\"orders\"", "duration_share"]
