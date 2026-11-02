-- | Endpoints end to end: concrete paths → template discovery and merge gates → issues
-- folded, announced and auto-acknowledged by served traffic → schema learning and its
-- anomalies → the API catalog (hosts, per-host stats, filters, sorts, archiving,
-- retention) → the learned OpenAPI document and docs page.
module EndpointsSpec (spec) where

import BackgroundJobs qualified
import Control.Lens ((^?))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens (key, _Array, _Object, _String)
import Data.Effectful.Notify (EmailData (..), Notification (..))
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Map.Strict qualified as Map
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml
import Database.PostgreSQL.Simple qualified as SimplePG
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Endpoints qualified as ApiCatalog
import Pkg.Components.Table qualified as Table
import Pkg.DeriveUtils (AesonText (..), assetUrl)
import Pkg.OpenApi qualified as OpenApi
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Pkg.SchemaLearning.Hot qualified as Hot
import Pkg.SchemaLearning.Worker qualified as Worker
import Pkg.TestUtils
import ProcessMessage qualified
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext (..), EnvConfig (..))
import Text.Printf (printf)
import Utils (toXXHash)


spec :: Spec
spec = around withTestResources do
  it "discovery collapses id families onto templates, keeps route words, and folds then announces their issues" \tr -> do
    let seeded routes = do
          pid <- createTestProject tr "discovery"
          insertEndpoints tr [endpoint pid m host p | (m, p) <- routes]
          pure pid
        discover pid = for_ @[] [BackgroundJobs.endpointTemplateDiscovery, BackgroundJobs.endpointMergeCleanup] \job -> runTestBg frozenTime tr (job pid)
        routesOf pid = sort . map (\(m, p) -> m <> " " <> p) <$> dbQuery @_ @(Text, Text) tr [sql| SELECT method, url_path FROM apis.endpoints WHERE project_id = ? |] (Only pid)
        -- Each case runs twice: a second discovery + cleanup pass must change nothing.
        collapses routes expected = do
          pid <- seeded routes
          replicateM_ 2 $ discover pid >> routesOf pid >>= (`shouldBe` sort expected)
          pure pid
        viaGet = map ("GET",)
        sb = map ("SB-" <>) ["91673E0634FB", "5A3469F964B1", "F76B055211BA"]
        verbs = ["verify_phone", "verify_email", "deactivate_user", "reset-password", "send_otp", "biometric_login", "update_email", "random_username", "transactions", "notifications"]
    traverse_ @[]
      (uncurry collapses)
      [ (viaGet ["/api/v1/users/auth0|abc123def456", "/api/v1/users/auth0|xyz789ghi012", "/api/v1/users/google-oauth2|111222333", "/api/v1/users/list-all", "/api/v1/health", "/api/v1/users"], ["GET /api/v1/health", "GET /api/v1/users", "GET /api/v1/users/list-all", "GET /api/v1/users/{param}"])
      , -- one path is not a population
        (viaGet ["/api/v1/orders/provider|order001"], ["GET /api/v1/orders/provider|order001"])
      , ([(m, "/api/v1/items/provider|item00" <> i) | m <- ["GET", "POST"], i <- ["1", "2"]], ["GET /api/v1/items/{param}", "POST /api/v1/items/{param}"])
      , -- A customer keyed on "SB-<hex>" once got 18,213 endpoint rows for ~1,900 routes:
        -- prefixed, encoded and mid-path ids must collapse while route words survive.
        ( viaGet
            $ map ("/v1/shipments/tracking/" <>) sb
            <> map ("/v1/wallet/complete-fund/WALLET_" <>) ["3D16EFF38182", "A1B2C3D4E5F6"]
            <> map ("/api/v1/customers/auth0%7C" <>) ["6a5ebc849d77a9a4717b0980", "66a12f77bc56dad584b79c8b"]
            <> [("/v1/shipbubble/products/" <> h <> "/stock_status") | h <- ["27ad70cdce4a", "40e1beed4b9e"]]
        , ["GET /api/v1/customers/{param}", "GET /v1/shipbubble/products/{param}/stock_status", "GET /v1/shipments/tracking/{param}", "GET /v1/wallet/complete-fund/{param}"]
        )
      , ([("POST", "/v1/account/" <> v) | v <- verbs], ["POST /v1/account/" <> v | v <- verbs])
      , (viaGet $ ["/v1/orders/search", "/v1/orders/summary"] <> map ("/v1/orders/" <>) sb, ["GET /v1/orders/search", "GET /v1/orders/summary", "GET /v1/orders/{param}"])
      ]
    -- A lone later straggler is too little evidence to mint a template, but the existing one vouches for it.
    straggler <- collapses (viaGet $ map ("/v1/track/" <>) (take 2 sb)) ["GET /v1/track/{param}"]
    insertEndpoints tr [endpoint straggler "GET" host "/v1/track/SB-000000000001"]
    discover straggler
    routesOf straggler `shouldReturn` ["GET /v1/track/{param}"]

    -- Every concrete path arrives with an un-notified api_change issue, as ingest leaves them.
    -- Four issues wanting one (project, target, type) once merged used to abort the cleanup on
    -- idx_issues_project_target_type_open; and discovery, not ingest, announces the survivors.
    let ship = "status" : sb <> ["SB-D5AC39C066E5"]
    pid <- seeded (viaGet $ map ("/v1/ship/" <>) ship)
    let shipHash p = Endpoints.hashEndpoint pid.toText host "GET" ("/v1/ship/" <> p)
        openIssues = sort <$> dbQuery @_ @(Text, Bool) tr [sql| SELECT target_hash, last_notified_at IS NOT NULL FROM apis.issues WHERE project_id = ? AND acknowledged_at IS NULL AND archived_at IS NULL |] (Only pid)
    for_ ship \p -> dbExec tr [sql| INSERT INTO apis.issues (project_id, issue_type, endpoint_hash, target_hash, title) VALUES (?, 'api_change', ?, ?, 'test issue') |] (pid, shipHash p, shipHash p)
    openIssues `shouldReturn` sort [(shipHash p, False) | p <- ship]
    discover pid
    routesOf pid `shouldReturn` ["GET /v1/ship/status", "GET /v1/ship/{param}"]
    openIssues `shouldReturn` sort [(shipHash "status", True), (shipHash "{param}", False)]

  -- A "param" verdict deletes issues if wrong and nothing downstream checks it.
  it "merge evidence needs agreement across passes, a real population, and no route-word majority" \_ -> do
    let metrics = ["app.cart.get_cart.latency", "aspnetcore.routing.match_attempts", "k8s.container.cpu_limit", "app_recommendations_counter"]
    map
      (\(confirmations, size, prior, members) -> BackgroundJobs.mergeEvidenceMet confirmations size prior members)
      [ (1, 8, 500, ["a1b2c3d4"]) -- one pass is not agreement, however large
      , (9, 20, 19, ["a1b2c3d4"]) -- the group shrank since the verdict
      , (3, 8, 40, ["deactivate_user", "verify_phone", "a1b2c3d4"]) -- members are route words
      , (3, 2, 5, ["a1b2c3d4"]) -- too small to be a population
      , (2, 8, 40, ["a1b2c3d4", "SHO3KOOWWN", "00Zj"])
      , -- A settled group used to be refused forever: 55 of 60 correct verdicts on the
        -- fleet, since a group is only re-asked when it changes.
        (2, 20, 20, ["a1b2c3d4"])
      , -- Nine near-identical alerts differing in a metric name: a word-like minority
        -- must not veto a family of identifiers.
        (3, 17, 24, metrics)
      ]
      `shouldBe` [False, False, False, False, True, True, True]
    map BackgroundJobs.routeWordFraction [metrics, ["login", "settings", "activity-logs", "a1b2c3d4"]] `shouldBe` [0.25, 0.75]
    map BackgroundJobs.looksLikeRouteWord ["cus_QpeOrF3HMRjazD", "SHO3KOOWWN", "a-df0u-05mwux", "00Zj"] `shouldBe` [False, False, False, False]
    -- One shape agrees with itself: a pure ratio rejected every group under ten members.
    map (uncurry BackgroundJobs.shapeAgreementOk) [(9, 1), (532, 5), (311, 177), (0, 0)] `shouldBe` [True, True, False, False]

  -- Sustained served traffic answers "is this a route, or noise?". A real route and a scanner
  -- burst differ in shape (spread over hours), not volume.
  it "auto-ack acks endpoints proven by traffic spread over hours, never a burst, an un-notified alert or a held lease" \tr -> do
    pid <- createTestProject tr "auto-ack"
    apiKey <- createTestAPIKey tr pid "auto-ack-key"
    let scanNow = addUTCTime 1800 frozenTime
        boundary = addUTCTime (-24 * 3600) scanNow -- the scanNow sweep's daily window edge
        hours origin n spread = [addUTCTime (fromIntegral ((i `mod` spread) * 3600 + 60)) origin | i <- [1 .. n :: Int]]
        paths :: [(Text, [UTCTime], UTCTime, Bool)] -- (path, request times, issue created_at, notified)
        paths =
          [ ("/v1/orders", hours frozenTime 30 3, frozenTime, True)
          , ("/v1/burst", hours frozenTime 40 1, frozenTime, True) -- more requests, one hour
          , ("/v1/cross-day", hours (addUTCTime (-25 * 3600) frozenTime) 20 2, addUTCTime (-26 * 3600) frozenTime, True)
          , ("/v1/same-hour-boundary", replicate 10 (addUTCTime (-600) boundary) <> replicate 10 (addUTCTime 600 boundary), addUTCTime (-26 * 3600) scanNow, True)
          , ("/v1/invoices", hours frozenTime 30 3, frozenTime, False) -- acking would swallow its alert
          ]
    -- One export: a request per span overflows the extraction queue and drops spans.
    ingestSpanReq tr $ mergeSpanRequests
      [ mkSpanRequest (UUID.toText UUID.nil) (T.take 8 (toXXHash path) <> toText (printf "%08d" i :: String)) Nothing ("GET " <> path) [] Nothing [mkAttr k v | (k, v) <- [("http.method", "GET"), ("http.route", path), ("http.response.status_code", "200"), ("server.address", host)]] (mkResource apiKey []) ts
      | (path, times, _, _) <- paths
      , (i, ts) <- zip [1 :: Int ..] times
      ]
    drainExtractionWorker tr{trATCtx = tr.trATCtx{env = tr.trATCtx.env{enableTimefusionReads = True}, config = tr.trATCtx.config{enableTimefusionWrites = True}}}
    void $ runAllBackgroundJobs frozenTime tr.trATCtx
    -- Evidence only counts from the issue's own hour, and the sweep only takes notified (or
    -- day-old) issues, so pin discovery's real issue rather than a synthetic one.
    for_ paths \(path, _, createdAt, notified) ->
      dbOne @_ @Int
        tr
        [sql| WITH u AS (UPDATE apis.issues SET created_at = ?, last_notified_at = ?
                         WHERE project_id = ? AND issue_type = 'api_change'
                           AND endpoint_hash = (SELECT hash FROM apis.endpoints WHERE project_id = ? AND url_path = ?) RETURNING 1)
              SELECT count(*)::int FROM u |]
        (createdAt, bool Nothing (Just createdAt) notified, pid, pid, path)
        `shouldReturn` 1
    let sweep at = runTestBg at tr $ BackgroundJobs.autoAckProvenEndpoints pid
        acked = map fromOnly <$> dbQuery tr [sql| SELECT e.url_path FROM apis.issues i JOIN apis.endpoints e ON e.hash = i.endpoint_hash WHERE i.project_id = ? AND i.acknowledged_until IS NOT NULL ORDER BY 1 |] (Only pid)
    withResource tr.trPool \conn -> SimplePG.withTransaction conn do
      void (SimplePG.query conn [sql|SELECT pg_advisory_xact_lock(hashtext(?))|] (Only BackgroundJobs.endpointAutoAckLockName) :: IO [Only ()])
      sweep frozenTime
    acked `shouldReturn` []
    sweep frozenTime
    -- cross-day: neither daily window proves it alone, their combined buckets do
    acked `shouldReturn` ["/v1/cross-day", "/v1/orders"]
    dbQuery tr [sql| SELECT a.metadata->>'reason' FROM apis.issue_activity_log a JOIN apis.issues i ON i.id = a.issue_id WHERE i.project_id = ? AND a.event = 'acknowledged' |] (Only pid)
      `shouldReturn` replicate 2 (Only ("endpoint_proven_by_traffic" :: Text))
    -- One UTC hour straddling the scanNow sweep's window edge still counts once.
    sweep scanNow
    acked `shouldReturn` ["/v1/cross-day", "/v1/orders"]

  it "schema learning announces an endpoint, its shape, fields and formats once, then only diffs" \tr -> do
    let other = UUIDId (UUID.fromWords 1 1 1 1)
        tainted = UUIDId (UUID.fromWords 2 2 2 2)
        clean = UUIDId (UUID.fromWords 3 3 3 3)
        flush shard = runHasqlEffect tr (Worker.flushDirty shard)
        jobs = dbOne @_ @Int tr [sql| SELECT count(*)::int FROM background_jobs WHERE payload->>'tag' = 'NewAnomaly' AND payload->>'projectId' = ?::text |] (Only testPid)
        users = observation testPid "GET" "/users" 200 . map (\(p, v) -> ("request.body.user." <> p, v, Nothing, Catalog.FCRequestBody))
    resetSchemaLearning tr [testPid, other, tainted, clean]
    ref <- newIORef Hot.emptySchemaShardState
    let observeFlush pid obs = Hot.observeSpans ref Hot.defaultPolicy pid (V.fromList obs) >> flush ref
        idEmail = users [("id", AE.String "abc"), ("email", AE.String "x@y")]
    r1 <- observeFlush testPid [idEmail]
    (r1.dirtyKeys, r1.catalogRowsWritten, r1.anomaliesEmitted, r1.summariesUpdated) `shouldBe` (1, 1, 6, 1)
    anomalyCounts tr testPid `shouldReturn` (1, 1, 2, 2)
    jobs `shouldReturn` 4 -- one NewAnomaly job per anomaly type
    -- Priors come from the flushed in-memory snapshot, and an entry diffed against itself emits nothing.
    st <- readIORef ref
    [(k, e)] <- pure $ HM.toList st.flushedEntries
    V.length (Worker.buildAnomalyRows (one ((k.projectId, k.keyHash), e)) (V.singleton (k, e))) `shouldBe` 0
    -- identical re-observation: no new anomalies, queued jobs coalesce; an empty flush skips the summary
    (.anomaliesEmitted) <$> observeFlush testPid [idEmail] `shouldReturn` 0
    jobs `shouldReturn` 4
    (.summariesUpdated) <$> flush ref `shouldReturn` 0
    -- a new field path: shape + field + format, no new endpoint
    (.anomaliesEmitted) <$> observeFlush testPid [users [("id", AE.String "abc"), ("email", AE.String "x@y"), ("name", AE.String "Ada")]] `shouldReturn` 3
    anomalyCounts tr testPid `shouldReturn` (1, 2, 3, 3)
    -- widening a type: a shape anomaly only
    (.anomaliesEmitted) <$> observeFlush testPid [users [("id", AE.Number 42)]] `shouldReturn` 1
    anomalyCounts tr testPid `shouldReturn` (1, 3, 3, 3)

    -- A 404 keeps its catalog row but announces no endpoint; a later matched response does,
    -- once. 201 observations pass the full-learning threshold so field sampling can't decide it.
    let matched = observation other "GET" "/users" 200 [("response.body.message", AE.String "missing", Nothing, Catalog.FCResponseBody)]
        missing = matched{Hot.scope = matched.scope{Catalog.statusCodes = V.singleton 404}}
    (.catalogRowsWritten) <$> observeFlush other (replicate 201 missing) `shouldReturn` 1
    anomalyCounts tr other `shouldReturn` (0, 1, 1, 1)
    void $ observeFlush other [matched]
    void $ observeFlush other [missing, matched]
    anomalyCounts tr other `shouldReturn` (1, 1, 1, 1)

    -- 2026-05-31: a NUL byte in a field path or value 22P05-poisoned the template and summary
    -- batches for every project in them, and the retry loop OOM'd monoscope at 14 GB.
    -- Fresh projects on a fresh shard, so both summaries are first writes.
    nulShard <- newIORef Hot.emptySchemaShardState
    Hot.observeSpans nulShard Hot.defaultPolicy tainted $ V.singleton $ observation tainted "GET" "/k8s" 200 [("resource.container.image\NULname", AE.String "ghcr.io/foo/bar\NULextra", Nothing, Catalog.FCResource)]
    Hot.observeSpans nulShard Hot.defaultPolicy clean $ V.singleton $ observation clean "GET" "/ok" 200 [("k", AE.String "hello", Nothing, Catalog.FCRequestBody)]
    nul <- flush nulShard
    (nul.catalogRowsWritten, nul.summariesUpdated) `shouldBe` (2, 2)
    dbOne @_ @Int tr [sql| SELECT count(*)::int FROM apis.schema_template t JOIN apis.schema_catalog c USING (template_hash) WHERE c.project_id = ? |] (Only tainted)
      `shouldReturn` 1

  -- The OOMs that disabled schema learning in prod: a dead byte budget, a walk run for every
  -- span regardless of the sample gate, and an uncapped field set per entry.
  it "a schema shard stays inside its byte, key and field budgets and stops walking past the learning threshold" \_ -> do
    let observed policy obs = newIORef Hot.emptySchemaShardState >>= \ref -> Hot.observeSpans ref policy testPid (V.fromList obs) >> readIORef ref
        cap = Hot.defaultPolicy.maxBytesPerShard
    st <- observed Hot.defaultPolicy $ map (ProcessMessage.extractObservation ProcessMessage.emptyPathClassifier . serverSpan) [0 .. 9_999]
    st.bytesEstimate `shouldSatisfy` (< cap + cap `div` 10)
    HM.size st.entries `shouldBe` 50
    wide <- observed Hot.defaultPolicy{Hot.maxFieldsPerEntry = 100} [observation testPid "GET" "/wide" 200 [("vendor.huge." <> show k, AE.String "x", Nothing, Catalog.FCAttribute) | k <- [1 .. 5_000 :: Int]]]
    map (HM.size . (.template.fields)) (HM.elems wide.entries) `shouldBe` [100]
    -- Past the threshold, separate batches on a known key only bump sampleCount: the walk
    -- thunk is never forced, so no new field lands.
    ref <- newIORef Hot.emptySchemaShardState
    for_ @[] [0 .. 499 :: Int] \i ->
      Hot.observeSpans ref Hot.defaultPolicy{Hot.learnFullThreshold = 1, Hot.learnSampleEveryN = 1_000_000} testPid
        $ V.singleton
        $ observation testPid "GET" "/sampled" 200 [("path-" <> show i, AE.Null, Nothing, Catalog.FCAttribute)]
    map (\e -> (HM.size e.template.fields, e.sampleCount)) . HM.elems . (.entries) <$> readIORef ref `shouldReturn` [(1, 500)]

  it "the catalog lists ingested endpoints per host, files their issues, filters them, and archives hosts" \tr -> do
    pid <- createTestProject tr "api-catalog"
    let hostsTable reqType filt stats = (\(PageCtx _ t) -> t) <$> (tablePage =<< call tr (ApiCatalog.apiCatalogH pid Nothing Nothing reqType Nothing Nothing filt stats))
        hostNames reqType filt = sort . map (.events.host) . V.toList . (.rows) <$> hostsTable (Just reqType) filt Nothing
        listed hostM = endpointRows <$> endpointsPage tr pid Nothing hostM
        hashOf h m p = Endpoints.hashEndpoint pid.toText h m p
        both = sort [("/", hashOf "172.31.29.11" "GET" "/"), ("/api/v1/user/login", hashOf "api.test.com" "POST" "/api/v1/user/login")]
    V.length . (.rows) <$> hostsTable Nothing Nothing Nothing `shouldReturn` 0
    -- 100 deliveries per host, each with its own timestamp: a span id is a UUIDv5 over the
    -- request, so byte-identical replicas would collapse in TimeFusion and fake the counts.
    now <- getCurrentTime
    let stamp i = toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (negate $ fromIntegral i) now
        withPid = \case AE.Object o -> AE.Object $ KM.insert "project_id" (AE.String pid.toText) o; v -> v
        encoded mk i = toStrict $ AE.encode $ Unsafe.fromJust $ convert $ withPid $ mk (stamp i)
    processMessagesAndBackgroundJobs tr $ concat [[("m1", encoded testRequestMsgs.reqMsg1 i), ("m2", encoded testRequestMsgs.reqMsg2 i)] | i <- [1 .. 100 :: Int]]
    dbQuery tr [sql| SELECT method, host, url_path, hash, outgoing FROM apis.endpoints WHERE project_id = ? ORDER BY url_path |] (Only pid)
      `shouldReturn` [("GET" :: Text, "172.31.29.11" :: Text, "/" :: Text, hashOf "172.31.29.11" "GET" "/", False), ("POST", "api.test.com", "/api/v1/user/login", hashOf "api.test.com" "POST" "/api/v1/user/login", False)]
    anomalyCounts tr pid `shouldReturn` (2, 2, 96, 96)
    dbOne @_ @Int tr [sql| SELECT count(*)::int FROM apis.issues WHERE project_id = ? AND issue_type = 'api_change' |] (Only pid) `shouldReturn` 2
    -- Perf guard: the first load renders hosts from Postgres with empty stats (the telemetry
    -- aggregate took 20-107s on busy projects) and defers stats=true to HTMX. Each host then
    -- counts only its own spans: joining on (url_path, method) gave every host the same total.
    shell <- hostsTable (Just "Incoming") Nothing Nothing
    stats <- hostsTable (Just "Incoming") Nothing (Just "true")
    let perHost t = sort [(vm.events.host, vm.events.eventCount) | vm <- V.toList t.rows]
    (perHost shell, shell.config.deferredUrl) `shouldBe` ([("172.31.29.11", 0), ("api.test.com", 0)], Just ("/p/" <> pid.toText <> "/api_catalog?filter=Incoming&sort=-events&period=24h&stats=true"))
    (perHost stats, stats.config.deferredUrl) `shouldBe` ([("172.31.29.11", 100), ("api.test.com", 100)], Nothing)
    listed Nothing `shouldReturn` both
    listed (Just "") `shouldReturn` both
    listed (Just "172.31.29.11") `shouldReturn` take 1 both
    listed (Just "api.test.com") `shouldReturn` drop 1 both
    -- "View logs" filters on server.address, where ingest normalises every host; the deprecated
    -- net.host.name landed OTLP-sourced endpoints on an empty explorer.
    table <- endpointsPage tr pid Nothing Nothing
    sort [q | ps <- linkParams ("/p/" <> pid.toText <> "/log_explorer") (renderT table), ("query", q) <- ps]
      `shouldBe` [ "attributes.server.address==\"172.31.29.11\" AND kind==\"server\" AND attributes.http.route==\"/\" AND attributes.http.request.method==\"GET\""
                 , "attributes.server.address==\"api.test.com\" AND kind==\"server\" AND attributes.http.route==\"/api/v1/user/login\" AND attributes.http.request.method==\"POST\""
                 , "resource.service.name==\"172.31.29.11\" AND kind==\"server\" AND attributes.http.route==\"/\""
                 , "resource.service.name==\"api.test.com\" AND kind==\"server\" AND attributes.http.route==\"/api/v1/user/login\""
                 ]
    dbExec tr [sql| UPDATE apis.issues SET acknowledged_at = ?, acknowledged_by = ? WHERE project_id = ? AND issue_type = 'api_change' |] (frozenTime, Projects.UserId UUID.nil, pid)
    listed Nothing `shouldReturn` both

    let bulk action reqType hosts = snd <$> testServant tr (ApiCatalog.apiCatalogBulkActionH pid action reqType ApiCatalog.HostBulk{itemId = hosts})
        directions = (,) <$> hostNames "Incoming" Nothing <*> hostNames "Outgoing" Nothing
    void $ bulk ApiCatalog.BAArchive (Just "Incoming") ["172.31.29.11"]
    (,) <$> hostNames "Incoming" Nothing <*> hostNames "Incoming" (Just "Archived") `shouldReturn` (["api.test.com"], ["172.31.29.11"])
    void $ bulk ApiCatalog.BAUnarchive (Just "Incoming") ["172.31.29.11"]
    (,) <$> hostNames "Incoming" Nothing <*> hostNames "Incoming" (Just "Archived") `shouldReturn` (["172.31.29.11", "api.test.com"], [])
    -- An outgoing host archives independently of incoming ones.
    runQueryEffect tr $ Endpoints.bulkInsertHosts $ V.singleton (def :: Endpoints.Endpoint){Endpoints.projectId = pid, Endpoints.host = "api.upstream.example", Endpoints.outgoing = True}
    void $ bulk ApiCatalog.BAArchive (Just "Outgoing") ["api.upstream.example"]
    directions `shouldReturn` (["172.31.29.11", "api.test.com"], [])
    hostNames "Outgoing" (Just "Archived") `shouldReturn` ["api.upstream.example"]
    -- The Archived tab posts no request_type: unarchive applies to both directions.
    void $ bulk ApiCatalog.BAArchive (Just "Incoming") ["172.31.29.11"]
    hostNames "Incoming" (Just "Archived") `shouldReturn` ["172.31.29.11", "api.upstream.example"]
    void $ bulk ApiCatalog.BAUnarchive Nothing ["172.31.29.11", "api.upstream.example"]
    directions `shouldReturn` (["172.31.29.11", "api.test.com"], ["api.upstream.example"])
    bulk ApiCatalog.BAArchive (Just "Incoming") [] >>= \case ApiCatalog.CatalogBulkDone -> pass
    directions `shouldReturn` (["172.31.29.11", "api.test.com"], ["api.upstream.example"])

  it "endpoint queries paginate disjointly, scope to a service before paginating, and sort by name or recency" \tr -> do
    pid <- createTestProject tr "catalog-queries"
    let traffic (e :: Endpoints.Endpoint) service minsAgo =
          dbExec
            tr
            [sql| INSERT INTO otel_logs_and_spans (id, project_id, timestamp, start_time, attributes___http___request___method, attributes___url___path,
                    attributes___server___address, resource___service___name, hashes, context, kind, status_code, summary)
                  VALUES (gen_random_uuid(), ?, ?, ?, 'GET', ?, ?, ?, ARRAY[?], '{}'::jsonb, 'server', '200', '{}') |]
            (pid, at, at, e.urlPath, e.host, service :: Maybe Text, e.hash)
          where
            at = addUTCTime (fromIntegral (-60 * minsAgo :: Int)) frozenTime
        page q = map (.urlPath) . V.toList . fst <$> runTestBg frozenTime tr (Endpoints.endpointRequestStatsPageByProject Endpoints.WithStats False pid q)
        query srt hostM = Endpoints.EndpointQuery{direction = Endpoints.Incoming, archived = False, host = hostM, search = Nothing, sort = srt, page = 0, perPage = 10, period = Endpoints.Window7d, service = Nothing}
    insertEndpoints tr [endpoint pid "GET" "paginate.example" ("/p" <> show i) | i <- [1 .. 12 :: Int]]
    pages <- forM @[] ["0", "1", "2"] \p -> endpointsPage tr pid (Just (p, "5")) (Just "paginate.example")
    map (\t -> (length (endpointRows t), maybe 0 (.totalCount) t.features.pagination)) pages `shouldBe` [(5, 12), (5, 12), (2, 12)]
    length (ordNub $ concatMap endpointRows pages) `shouldBe` 12

    -- Zero-traffic hosts and endpoints drop out before pagination, so a page of one is exact.
    let alpha = (endpoint pid "GET" "scope-alpha.example" "/alpha" :: Endpoints.Endpoint){Endpoints.serviceName = Just "catalog-scope-alpha"}
        beta = (endpoint pid "GET" "scope-beta.example" "/beta" :: Endpoints.Endpoint){Endpoints.serviceName = Just "catalog-scope-beta"}
    insertEndpoints tr [alpha, beta]
    traffic alpha (Just "catalog-scope-alpha") 0
    traffic beta (Just "catalog-scope-beta") 0
    (endpointRowsQ, endpointTotal) <- runTestBg frozenTime tr $ Endpoints.endpointRequestStatsPageByProject Endpoints.WithStats False pid Endpoints.EndpointQuery{direction = Endpoints.Incoming, archived = False, host = Nothing, search = Nothing, sort = Endpoints.SortName, page = 0, perPage = 1, period = Endpoints.Window24h, service = Just "catalog-scope-alpha"}
    (map (.urlPath) (V.toList endpointRowsQ), endpointTotal) `shouldBe` (["/alpha"], 1)
    hostRows <- runTestBg frozenTime tr $ Endpoints.dependenciesAndEventsCount Endpoints.WithStats False pid Endpoints.HostQuery{direction = Just Endpoints.Incoming, archived = False, sort = Endpoints.SortName, skip = 0, since = Endpoints.Since24h, period = Endpoints.Window24h, service = Just "catalog-scope-alpha"}
    map (.host) hostRows `shouldBe` ["scope-alpha.example"]

    -- "Alphabetical" was a silent no-op falling through to traffic order; /zebra has 9x the traffic.
    let zebra = endpoint pid "GET" "alphasort.example" "/zebra"
        alphaEp = endpoint pid "GET" "alphasort.example" "/alpha"
    insertEndpoints tr [zebra, alphaEp]
    replicateM_ 9 (traffic zebra Nothing 0) >> traffic alphaEp Nothing 0
    page (query Endpoints.SortEvents (Just "alphasort.example")) `shouldReturn` ["/zebra", "/alpha"]
    page (query Endpoints.SortName (Just "alphasort.example")) `shouldReturn` ["/alpha", "/zebra"]
    -- last_seen ordered by endpoint created_at; "/old" was created first but served most recently.
    let old = endpoint pid "GET" "lastseen.example" "/old"
        new = endpoint pid "GET" "lastseen.example" "/new"
    insertEndpoints tr [old, new]
    for_ @[] [(old, "2 days"), (new, "1 hour" :: Text)] \(e, ago) -> dbExec tr [sql| UPDATE apis.endpoints SET created_at = ?::timestamptz - ?::interval WHERE hash = ? |] (frozenTime, ago, e.hash)
    traffic old Nothing 5 >> traffic new Nothing 120
    page (query Endpoints.SortLastSeen (Just "lastseen.example")) `shouldReturn` ["/old", "/new"]

  it "host retention archives idle hosts as the system, wakes them on traffic with a digest, and never overrides a user's archive" \tr -> do
    pid <- createTestProject tr "retention"
    now <- getCurrentTime
    apiKey <- createTestAPIKey tr pid "retention-key"
    let idle = "idle.retention.example"
        manual = "manual.retention.example"
        -- (archived, archived by the system)
        hostState h = dbQuery @_ @(Bool, Bool) tr [sql|SELECT archived_at IS NOT NULL, archived_by IS NULL FROM apis.hosts WHERE project_id = ? AND host = ? AND NOT outgoing|] (pid, h)
        sweep = fst <$> runTestBackgroundWithNotifications now tr.trLogger tr.trATCtx (BackgroundJobs.runHostRetentionSweep tr.trATCtx pid)
        emails ns = [d.htmlBody | EmailNotification d <- ns]
        ingestFor h = do
          spanId <- T.take 16 . T.filter (/= '-') . UUID.toText <$> nextRandom
          ingestSpanReq tr $ mkSpanRequest (T.replicate 32 "a") spanId Nothing "GET /ping" [] Nothing [mkAttr "server.address" h, mkAttr "http.request.method" "GET", mkAttr "url.path" "/ping"] (mkResource apiKey []) (addUTCTime 120 now)
    runQueryEffect tr $ Endpoints.bulkInsertHosts $ V.fromList [(def :: Endpoints.Endpoint){Endpoints.projectId = pid, Endpoints.host = h, Endpoints.outgoing = False} | h <- [idle, manual]]
    dbExec tr [sql|UPDATE apis.hosts SET last_seen_at = ? WHERE project_id = ? AND host LIKE '%.retention.example'|] (addUTCTime (-40 * 86400) now, pid)
    users <- runTestBg now tr $ Projects.usersByProjectId pid
    runQueryEffect tr $ void $ Endpoints.setHostsArchived pid Nothing (Endpoints.ArchiveBy (maybe (error "project has no users") ((.id) . head) (nonEmpty users))) [manual]
    emails <$> sweep `shouldReturn` []
    hostState idle `shouldReturn` [(True, True)]
    -- Traffic returns after the archive moment for both; only the system archive wakes.
    ingestFor idle >> ingestFor manual
    woken <- emails <$> sweep
    (map (T.isInfixOf idle) woken, map (T.isInfixOf manual) woken) `shouldBe` ([True], [False])
    (,) <$> hostState idle <*> hostState manual `shouldReturn` ([(False, True)], [(True, False)])
    -- The wake bumped recency, so an immediate re-sweep must not re-archive.
    emails <$> sweep `shouldReturn` []
    hostState idle `shouldReturn` [(False, True)]

  -- Seeded through the real learner, so drift between what it writes and what the generator
  -- reads breaks this.
  it "the learned OpenAPI document covers every learned location, scopes to an endpoint or project, and backs the docs page" \tr -> do
    -- A primitive that evolved into an object is a union, not a discarded primitive.
    let stringField formats = Catalog.FieldStruct (HS.singleton Catalog.FTString) formats Catalog.FCRequestBody False
        evidence fs values = (fs, Just (Catalog.Examples $ V.fromList values), Nothing)
        metadata =
          fromMaybe AE.Null
            $ OpenApi.jsonSchema
              [ ("metadata", evidence (stringField $ HS.singleton "{uuid}") [AE.String "legacy-id"])
              , ("metadata.state", evidence (stringField $ HS.fromList ["text", "{uuid}"]) [AE.String "ready", AE.String "550e8400-e29b-41d4-a716-446655440000"])
              ]
              ^? key "properties"
              . key "metadata"
        alternatives = maybe [] V.toList $ metadata ^? key "anyOf" . _Array
        stateSchema = fromMaybe AE.Null $ find (isJust . (^? key "properties")) alternatives ^? traverse . key "properties" . key "state"
    map (^? key "type" . _String) alternatives `shouldBe` [Just "object", Just "string"]
    stateSchema ^? key "format" `shouldBe` Nothing
    maybe [] V.toList (stateSchema ^? key "x-monoscope-observed-formats" . _Array) `shouldMatchList` [AE.String "{uuid}", AE.String "text"]
    stateSchema ^? key "examples" . _Array `shouldBe` Just (V.fromList [AE.String "ready", AE.String "550e8400-e29b-41d4-a716-446655440000"])

    let path = "/v1/stores/:store_id/orders"
        opHash = Endpoints.hashEndpoint testPid.toText host "POST" path
        leaves =
          [ ("store_id", AE.String "st_123", Just "{uuid}", Catalog.FCPathParam)
          , ("page[*]", AE.String "2", Just "{integer}", Catalog.FCQueryParam)
          , ("x-api-key", AE.String "k", Nothing, Catalog.FCRequestHeader)
          , ("content-type", AE.String "application/json", Nothing, Catalog.FCResponseHeader)
          , ("customer.email", AE.String "a@b.co", Just "{email}", Catalog.FCRequestBody)
          , ("items[*].sku", AE.String "TK-1", Nothing, Catalog.FCRequestBody)
          , ("items[*].qty", AE.Number 2, Nothing, Catalog.FCRequestBody)
          , ("[*].order_id", AE.String "SB-1", Nothing, Catalog.FCResponseBody)
          ]
        fetch hostM endpointM = snd <$> testServant tr (ApiCatalog.apiSpecJsonH testPid hostM (Just "Incoming") endpointM)
    resetSchemaLearning tr [testPid]
    ref <- newIORef Hot.emptySchemaShardState
    Hot.observeSpans ref Hot.defaultPolicy testPid (V.replicate 201 (observation testPid "POST" path 201 leaves))
    void $ runHasqlEffect tr (Worker.flushDirty ref)
    doc <- fetch (Just host) Nothing
    doc ^? key "openapi" . _String `shouldBe` Just "3.1.0"
    -- ':store_id' is a framework spelling; OpenAPI needs '{store_id}'.
    let op = fromMaybe AE.Null $ doc ^? key "paths" . key "/v1/stores/{store_id}/orders" . key "post"
        params l = [(p ^? key "name" . _String, p ^? key "required") | p <- maybe [] V.toList (op ^? key "parameters" . _Array), p ^? key "in" . _String == Just l]
        reqSchema = fromMaybe AE.Null $ op ^? key "requestBody" . key "content" . key "application/json" . key "schema" . key "properties"
        email = fromMaybe AE.Null $ reqSchema ^? key "customer" . key "properties" . key "email"
        resp = fromMaybe AE.Null $ op ^? key "responses" . key "201"
    (op ^? key "operationId" . _String, op ^? key "tags" . _Array) `shouldBe` (Just opHash, Just (V.singleton "orders-api"))
    -- Only the path variable is required: traffic can't prove the rest are. 'page[*]' is a repeated query param.
    map params ["path", "query", "header"] `shouldBe` [[(Just "store_id", Just (AE.Bool True))], [(Just "page", Just (AE.Bool False))], [(Just "x-api-key", Just (AE.Bool False))]]
    -- Flattened body paths rebuild into nested JSON Schema, arrays included.
    (email ^? key "format" . _String, email ^? key "x-monoscope-observed-formats" . _Array, email ^? key "examples" . _Array) `shouldBe` (Just "email", Just (V.singleton "{email}"), Just (V.singleton "a@b.co"))
    (reqSchema ^? key "items" . key "type" . _String, reqSchema ^? key "items" . key "items" . key "properties" . key "qty" . key "type" . _String) `shouldBe` (Just "array", Just "number")
    -- The observed status carries the response; its schema lives in components so statuses can share it.
    resp ^? key "headers" . key "content-type" . key "schema"
      `shouldBe` Just (AE.object ["type" AE..= ("string" :: Text), "example" AE..= ("application/json" :: Text), "examples" AE..= V.singleton ("application/json" :: Text), "x-monoscope-observed-formats" AE..= V.singleton ("text" :: Text)])
    resp ^? key "content" . key "application/json" . key "schema" . key "$ref" . _String `shouldBe` Just ("#/components/schemas/" <> opHash <> "Response")
    doc ^? key "components" . key "schemas" . key (fromString $ toString $ opHash <> "Response") . key "type" . _String `shouldBe` Just "array"

    scoped <- fetch (Just host) (Just opHash)
    unknown <- fetch (Just host) (Just "deadbeef") -- documents nothing rather than falling back to the host
    map (fmap length . (^? key "paths" . _Object)) [scoped, unknown] `shouldBe` [Just 1, Just 0]
    yaml <- call tr (ApiCatalog.apiSpecYamlH testPid (Just host) (Just "Incoming") (Just opHash))
    either (const AE.Null) id (Yaml.decodeEither' (encodeUtf8 yaml)) `shouldBe` scoped
    -- No host is the whole project (the API Docs nav entry), served from the observed hosts.
    project <- fetch Nothing Nothing
    (isJust (project ^? key "paths" . key "/v1/stores/{store_id}/orders"), project ^? key "servers" . _Array) `shouldBe` (True, Just (V.singleton (AE.object ["url" AE..= ("https://" <> host)])))

    ApiCatalog.ApiDocsPage (PageCtx bw vm) <- call tr (ApiCatalog.apiDocsH testPid (Just host) (Just "Incoming") (Just opHash))
    let specUrl ext = "/p/" <> testPid.toText <> "/api_catalog/openapi." <> ext <> "?request_type=Incoming&host=api.example.com&endpoint=" <> opHash
    (vm.specJsonUrl, vm.specYamlUrl, vm.operations, vm.truncated, vm.inlineYaml) `shouldBe` (specUrl "json", specUrl "yaml", 1, False, Just yaml)
    tagsWith ["swagger-ui-bundle"] (foldMap renderT bw.headContent) `shouldBe` ["<script src=\"" <> assetUrl "/public/assets/deps/swagger-ui/swagger-ui-bundle.js" <> "\" defer=\"true\""]
    goldenParts "api-docs-swagger-mount" $ elementsWith ["id=\"swagger-ui\""] (renderT vm)


host :: Text
host = "api.example.com"


endpoint :: Projects.ProjectId -> Text -> Text -> Text -> Endpoints.Endpoint
endpoint pid method h path = (def :: Endpoints.Endpoint){Endpoints.projectId = pid, Endpoints.urlPath = path, Endpoints.urlParams = AE.object [], Endpoints.method = method, Endpoints.host = h, Endpoints.hash = Endpoints.hashEndpoint pid.toText h method path, Endpoints.outgoing = False}


insertEndpoints :: TestResources -> [Endpoints.Endpoint] -> IO ()
insertEndpoints tr eps = runQueryEffect tr $ Endpoints.bulkInsertEndpoints $ V.fromList eps


-- | (endpoint, shape, field, format) anomalies of a project.
anomalyCounts :: TestResources -> Projects.ProjectId -> IO (Int, Int, Int, Int)
anomalyCounts tr pid =
  dbQuery
    tr
    [sql| SELECT count(*) FILTER (WHERE anomaly_type = 'endpoint')::int, count(*) FILTER (WHERE anomaly_type = 'shape')::int,
                 count(*) FILTER (WHERE anomaly_type = 'field')::int, count(*) FILTER (WHERE anomaly_type = 'format')::int
          FROM apis.anomalies WHERE project_id = ? |]
    (Only pid)
    >>= \case
      [r] -> pure r
      rs -> fail $ "anomalyCounts: " <> show (length rs) <> " rows"


-- | The endpoints table with stats, optionally at a (page, perPage).
endpointsPage :: TestResources -> Projects.ProjectId -> Maybe (Text, Text) -> Maybe Text -> IO (Table.Table ApiCatalog.EnpReqStatsVM)
endpointsPage tr pid pageM hostM = do
  PageCtx _ t <- tablePage =<< call tr (ApiCatalog.endpointListGetH pid (fst <$> pageM) (snd <$> pageM) Nothing Nothing hostM Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true"))
  pure t


endpointRows :: Table.Table ApiCatalog.EnpReqStatsVM -> [(Text, Text)]
endpointRows t = sort [(e.urlPath, e.endpointHash) | ApiCatalog.EnpReqStatsVM _ _ e <- V.toList t.rows]


-- | One observed HTTP span on api.example.com: @leaves@ are the (path, value, format,
-- category) the leaf walker would emit.
observation :: Projects.ProjectId -> Text -> Text -> Int -> [(Text, AE.Value, Maybe Text, Catalog.FieldCategoryEnum)] -> Hot.ObservationInput
observation pid method path status leaves =
  Hot.ObservationInput
    { keyKind = Catalog.HttpEndpoint
    , keyHash = Endpoints.hashEndpoint pid.toText host method path
    , scope = Catalog.Scope{Catalog.service = Just "orders-api", Catalog.spanName = Just (method <> " " <> path), Catalog.kind = Just "server", Catalog.host = Just host, Catalog.method = Just method, Catalog.urlPath = Just path, Catalog.statusCodes = V.singleton status}
    , walk = \_ -> [(p, V.singleton (v, f), c) | (p, v, f, c) <- leaves]
    , timestamp = frozenTime
    }


-- | One of 50 routes across 5 services, for driving the real walker at scale.
serverSpan :: Int -> Telemetry.OtelLogsAndSpans
serverSpan ix =
  Telemetry.OtelLogsAndSpans
    { id = UUID.toText UUID.nil, project_id = testPid.toText, timestamp = frozenTime, parent_id = Nothing, observed_timestamp = Nothing, hashes = Just V.empty
    , name = Just ("GET " <> route), kind = Just "server", status_code = Just "200", status_message = Nothing, level = Nothing, severity = Nothing, body = Nothing
    , duration = Just 1, start_time = frozenTime, end_time = Nothing, context = Nothing, events = Nothing, links = Nothing, summary = V.empty, date = frozenTime
    , errors = Nothing, message_size_bytes = 0
    , attributes = Just $ AesonText $ Map.fromList [("http.request.method", AE.String "GET"), ("http.response.status_code", AE.Number 200), ("url.path", AE.String route), ("db.operation.name", AE.String "SELECT")]
    , resource = Just $ AesonText $ Map.singleton "service.name" (AE.String $ "svc-" <> show (ix `mod` 5))
    }
  where
    route = "/endpoint-" <> show (ix `mod` 50)
