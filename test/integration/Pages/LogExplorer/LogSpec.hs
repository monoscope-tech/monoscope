-- | The log explorer end to end: ingest → rows (list, filters, time windows, pagination,
-- trace trees) → detail panel → page shell → facets → chart cache → live tail → share links.
module Pages.LogExplorer.LogSpec (spec) where

import Control.Concurrent.STM (isEmptyTBQueue)
import Control.Exception qualified as E
import Control.Exception.Safe qualified as Safe
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.Set qualified as S
import Data.Pool (defaultPoolConfig, destroyAllResources, newPool, setNumStripes, withResource)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, defaultTimeLocale, getCurrentTime, parseTimeM)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Types (PGArray (..))
import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Interpolate qualified as HI
import Ki qualified
import Models.Apis.LogQueries qualified as LogQueries
import Models.Apis.SchemaCatalog qualified as SC
import Models.Apis.ShareEvents qualified as ShareEvents
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.HTTP.Types qualified as H
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WT
import Pages.Charts.Charts qualified as Charts
import Pages.LogExplorer.LiveTail qualified as LiveTailPage
import Pages.LogExplorer.Log qualified as Log
import Pages.LogExplorer.LogItem qualified as LogItem
import Pages.LogExplorer.QueryLibrary qualified as QueryLibrary
import Pages.Share qualified as Share
import Pages.Telemetry qualified as TelemetryPage
import Pkg.Components.LogQueryBox qualified as LogQueryBox
import Pkg.DeriveUtils (AesonText (..))
import Pkg.LiveTail qualified as LT
import Pkg.Parser (SqlQueryCfg (..), defSqlQueryCfg, parseQueryToAST)
import Pkg.Parser qualified as Parser
import Pkg.Parser.Expr (ToQueryText (..))
import Pkg.Parser.Expr qualified as PE
import Pkg.Parser.Stats (Sources (..))
import Pkg.QueryCache qualified as QC
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Pkg.SchemaLearning.Hot qualified as Hot
import Pkg.SchemaLearning.Worker qualified as Worker
import Pkg.TestUtils
import ProcessMessage (processMessages)
import ProcessMessage qualified
import Relude.Unsafe qualified as Unsafe
import Servant qualified
import Servant.QueryParam.Record (RecordParam)
import Servant.Types.SourceT (SourceT (..), StepT (..))
import System.Config (AuthContext (..), EnvConfig (..))
import System.Server qualified as Server
import Text.Read (read)
import Utils (toXXHash)
import Utils qualified
import Web.ApiHandlers qualified as ApiH
import Web.Routes qualified as Routes
import "base64" Data.Base64.Types qualified as B64T
import "base64" Data.ByteString.Base64 qualified as B64


spec :: Spec
spec = do
  it "rows endpoint boundary: route decoding, aggregate-shaped rows, orphan headers" \_ -> do
    let decodeApp =
          Servant.serve
            (Proxy @("decode" Servant.:> RecordParam Routes.LogDataFieldExp Log.LogDataQuery Servant.:> Servant.Get '[Servant.JSON] Text))
            (pure . show)
        request path = WT.runSession (WT.srequest $ WT.SRequest (WT.setPath Wai.defaultRequest path) "") decodeApp
    decoded <- request "/decode?query=name%3D%3D%22checkout%22&cols=name%2Cduration&cursor=2026-09-18T10%3A20%3A30Z&direction=newer&since=24H&from=2026-09-17T10%3A20%3A30Z&to=2026-09-18T10%3A20%3A30Z&source=spans&target-spans=span-a%2Cspan-b&sort=-duration"
    AE.eitherDecode @Text decoded.simpleBody `shouldBe` Right (show def{Log.query = Just "name==\"checkout\"", Log.cols = Just "name,duration", Log.cursor = Just (read "2026-09-18 10:20:30 UTC"), Log.direction = Just Parser.PageNewer, Log.since = Just "24H", Log.from = Just "2026-09-17T10:20:30Z", Log.to = Just "2026-09-18T10:20:30Z", Log.source = Just "spans", Log.targetSpans = Just "span-a,span-b", Log.sort = Just "-duration"})
    forM_ ["/decode?cursor=not-a-time", "/decode?direction=sideways"] \path ->
      (H.statusCode . (.simpleStatus) <$> request path) `shouldReturn` 400
    -- `index out of bounds (7,1)` in production: log-table column indices over a one-column aggregate row.
    let colIdxMap = Utils.listToIndexHashMap ["id", "timestamp", "trace_id", "span_name", "duration", "service", "parent_id", "start_time_ns", "errors", "summary", "latency_breakdown", "kind"]
        narrow = V.singleton (V.singleton (AE.Number 42))
    Log.colsFitRows colIdxMap narrow `shouldBe` False
    Log.buildTraceTree colIdxMap 1 narrow `shouldBe` (narrow, [])
    Log.synthesizeOrphanHeaders colIdxMap narrow `shouldBe` V.empty
    -- startNs was folded with a 0 seed, so every orphan header spanned from the epoch.
    let orphanIdx = HM.fromList $ zip ["trace_id", "parent_id", "latency_breakdown", "start_time_ns", "duration"] [0 ..]
        row sid start dur = V.fromList [AE.String "t1", AE.String "missing-parent", AE.String sid, AE.Number start, AE.Number dur]
        cell k r = HM.lookup k orphanIdx >>= (r V.!?)
    map (\h -> (cell "start_time_ns" h, cell "duration" h)) (V.toList $ Log.synthesizeOrphanHeaders orphanIdx $ V.fromList [row "s1" 1000 10, row "s2" 2000 20])
      `shouldBe` [(Just (AE.Number 1000), Just (AE.Number 1020))]

  it "renders the alert badge, query library items and facet sidebar" \_ -> do
    -- The alert header read status_message, which mkSystemLog never sets: an empty success badge.
    let alert = (Telemetry.mkSystemLog testPid "monitor.alert.triggered" Telemetry.SLError "CPU: alerting" (Map.fromList [("monitor.status", AE.String "alerting")]) Nothing frozenTime :: Telemetry.OtelLogsAndSpans){Telemetry.kind = Just "alert"}
        qli = Projects.QueryLibItem{id = UUIDId UUID.nil, projectId = testPid, createdAt = frozenTime, updatedAt = frozenTime, userId = Projects.UserId UUID.nil, queryType = Projects.QLTSaved, queryText = "status_code == \"500\"", queryAst = AE.Null, title = Just "errors", byMe = True}
        facets m = renderT $ Log.renderFacets Catalog.FacetSummary{id = UUID.nil, projectId = testPid.toText, tableName = "otel_logs_and_spans", facetJson = Catalog.FacetData m}
        sidebar = facets $ HM.fromList [("attributes.http.request.method", [Catalog.FacetValue "GET" 3]), ("resource.service.name", [Catalog.FacetValue "checkout-svc" 5]), ("level", [Catalog.FacetValue "INFO" 7])]
        overflow = facets $ HM.singleton "resource.service.name" [Catalog.FacetValue ("service-" <> show n) 1 | n <- [1 .. 6 :: Int]]
    -- Library items hand raw KQL to handleAddQuery (a JSON.parse threw on every click). Facet
    -- clicks go through window.queryEditorCall: Monaco loads lazily, so calling the element
    -- directly threw "not a function". Overflow values arrive as a fragment once More is opened.
    goldenParts "log-explorer-fragments"
      . ordNub
      $ elementsWith [">Alerting</span>"] (renderT $ LogItem.expandedItemView testPid alert Nothing Nothing)
      <> tagsWith ["dataset.query", "aria-label=\"Edit query title\"", "aria-label=\"Delete query\"", "aria-label=\"Show all team queries\""] (renderT $ LogQueryBox.queryLibraryContent_ (V.singleton qli) V.empty)
      <> tagsWith ["data-field=\"", "facets?group=", "queryEditorCall(", "data-component=\"facet-option\""] sidebar
      <> elementsWith ["no values in window", "Common Filters"] sidebar
      <> tagsWith ["data-value=\"service-"] overflow
      <> elementsWith ["+ More ("] overflow
    filter (\p -> not (S.member p PE.flattenedOtelAttributes || S.member p PE.topLevelOtelColumns)) (map (.path) Log.facetDefs) `shouldBe` []

  it "live tail matching: service, environment and kind gates, fan-out, free text" \_ -> do
    let rec' = logRecord "checkout" "prod"
        cases :: [([LT.Subscription], [Telemetry.OtelLogsAndSpans], [[Text]])]
        cases =
          [ ([mkSub 1 "checkout" Nothing "level == \"error\""], [rec' "error" "boom", rec' "info" "fine", logRecord "billing" "prod" "error" "not yours", spanRecord "checkout" "prod" "error"], [["boom"]])
          , ([mkSub 2 "checkout" Nothing ""], [rec' "info" "a", logRecord "billing" "prod" "info" "b"], [["a"]])
          , -- The page opens on no service, logs; the kind is a mode, not an implicit kind = 'log'.
            ([mkSubScoped n (LT.LogTail Nothing k) [] Nothing "" | (n, k) <- [(25, LT.SKLogs), (26, LT.SKSpans), (27, LT.SKAny)]], [rec' "info" "a", logRecord "billing" "prod" "info" "b", spanRecord "checkout" "prod" "error"], [["a", "b"], [""], ["a", "b", ""]])
          , -- Selectors are resolved values, never spliced into the query: an always-true filter stays scoped.
            ([mkSub 3 "checkout" (Just "prod") "level == \"error\" or level != \"error\""], [rec' "error" "prod row", logRecord "checkout" "staging" "error" "staging row", logRecord "billing" "prod" "error" "other service"], [["prod row"]])
          , ([mkSub 4 "checkout" Nothing "", mkSub 5 "checkout" Nothing "level == \"error\""], [rec' "error" "boom"], [["boom"], ["boom"]])
          , -- Events sees spans a logs-only tail must never see; its service gate is optional.
            ([mkSubScoped 20 (LT.AllSignals Nothing) ["kind", "name"] Nothing "", mkSub 21 "checkout" Nothing ""], [spanRecord "checkout" "prod" "error"], [["<table>"], []])
          , ([mkSubScoped 22 (LT.AllSignals Nothing) ["service"] Nothing "", mkSubScoped 23 (LT.AllSignals (Just "checkout")) ["service"] Nothing ""], [rec' "info" "a", logRecord "billing" "prod" "info" "b"], [["<table>", "<table>"], ["<table>"]])
          , -- Free text matches like the explorer: span status messages, sequences tolerate punctuation, terms AND.
            ([mkSubScoped (30 + n) (LT.AllSignals Nothing) ["id"] Nothing q | (n, q) <- zip [0 ..] ["\"deadlock\"", "\"lock wait\"", "\"order\" \"saved\"", "\"deadlock\" \"saved\""]], [rec' "error" "ER_LOCK_DEADLOCK", (spanRecord "checkout" "prod" "error" :: Telemetry.OtelLogsAndSpans){Telemetry.status_message = Just "Deadlock found when trying to get lock"}, rec' "error" "Lock-wait timeout", rec' "info" "order saved"], [["<table>", "<table>"], ["<table>"], ["<table>"], []])
          ]
    forM_ cases \(subs, recs, expected) -> (map (map bodyOf . fst) <$> delivered 100 subs recs) `shouldReturn` expected
    -- Nobody tailing: the batch is never walked, which makes the hook safe on the ingest hot path.
    hub <- LT.newHub
    rt <- runtimeWith hub []
    (getSum . (.evaluated) <$> LT.publishMatches rt testPid (V.fromList [rec' "error" "boom"])) `shouldReturn` 0

  it "live tail rows: projection, truncation, timestamps, wire and batch bounds, backpressure, framing" \_ -> do
    let rec' = logRecord "checkout" "prod"
        fatSummary = (rec' "info" "a" :: Telemetry.OtelLogsAndSpans){Telemetry.summary = V.fromList ["kind;badge⇒log", "body;text-textStrong⇒" <> T.replicate (LT.maxRowFieldChars * 3) "x"]}
        richSummary = V.fromList ["request_type;badge-neutral⇒incoming", "span_name;text-textStrong⇒handler"]
        logTab = LT.LogTail (Just "checkout")
    [[LT.LogRow long], [LT.LogRow http], [LT.LogRow spanSummary], [LT.TableRow table], [LT.TableRow aggregate], [LT.TableRow fatEvents], [LT.LogRow fatTab]] <-
      forM
        [ (mkSub 6 "checkout" Nothing "", rec' "info" (T.replicate (LT.maxRowFieldChars + 500) "x"))
        , (mkSubScoped 7 (logTab LT.SKLogs) ["attributes___http___request___method"] Nothing "", httpLogRecord "GET")
        , (mkSubScoped 8 (logTab LT.SKSpans) ["summary"] Nothing "", (spanRecord "checkout" "prod" "error" :: Telemetry.OtelLogsAndSpans){Telemetry.summary = richSummary})
        , -- The column list arrives in the query layer's ___ spelling; it must resolve as matching does.
          (mkSubScoped 23 (LT.AllSignals Nothing) ["timestamp", "kind", "resource___service___name", "attributes___http___request___method"] Nothing "", httpLogRecord "GET")
        , -- Aggregates have no in-memory value: the key is left out for the durable read to fill.
          (mkSubScoped 24 (LT.AllSignals Nothing) ["kind", "count_"] Nothing "", rec' "info" "a")
        , -- Shipbubble 2026-10-19: multi-MB bodies rode inside `summary`, past the string cap.
          (mkSubScoped 26 (LT.AllSignals Nothing) ["summary"] Nothing "", fatSummary)
        , (mkSubScoped 27 (logTab LT.SKLogs) ["summary"] Nothing "", fatSummary)
        ]
        \(sub, r) -> concatMap fst <$> delivered 100 [sub] [r]
    (long.truncated, T.length long.body) `shouldBe` (True, LT.maxRowFieldChars)
    Map.lookup "attributes___http___request___method" http.fields `shouldBe` Just (AE.String "GET")
    Map.lookup "summary" spanSummary.fields `shouldBe` Just (AE.toJSON richSummary)
    (Map.lookup "resource___service___name" table, Map.lookup "attributes___http___request___method" table, Map.lookup "kind" table) `shouldBe` (Just "checkout", Just "GET", Just "log")
    Map.member "count_" aggregate `shouldBe` False
    let capped = \case AE.Array xs -> all (\case AE.String t -> T.length t <= LT.maxRowFieldChars; _ -> False) xs; _ -> False
    (capped <$> Map.lookup "summary" fatEvents, capped <$> Map.lookup "summary" fatTab.fields) `shouldBe` (Just True, Just True)
    -- The durable record lookup round-trips microseconds, so the live row is stamped with them.
    [incoming, stored, midnightEdge, nextDay] <- traverse (parseTimeM True defaultTimeLocale "%F %T%Q UTC") ["2026-08-28 11:19:22.10099157 UTC", "2026-08-28 11:19:22.100992 UTC", "2026-08-28 23:59:59.9999997 UTC", "2026-08-29 00:00:00 UTC"] :: IO [UTCTime]
    (LT.storageTimestamp incoming, LT.storageTimestamp midnightEdge) `shouldBe` (stored, nextDay)
    case LT.toLiveRow (logTab LT.SKLogs) [] (rec' "info" "precision" :: Telemetry.OtelLogsAndSpans){Telemetry.timestamp = incoming} of
      LT.LogRow f -> f.timestamp `shouldBe` stored
      _ -> fail "expected a log row"
    -- The per-field cap does not bound the sum the broker measures: dropped before the transport.
    (wideConn, wideRt) <- fixture [mkSubScoped 61 (LT.AllSignals Nothing) ["attributes___k" <> show n | n <- [0 :: Int .. 127]] Nothing ""]
    wide <- LT.publishMatches wideRt testPid (V.fromList [wideRecord])
    (getSum wide.matched, getSum wide.oversized, getSum wide.publishFailed) `shouldBe` (1, 1, 0)
    atomically (isEmptyTBQueue wideConn.queue) `shouldReturn` True
    -- Fifty subscriptions make the batch budget bite at a thousand records.
    let subs = [mkSub (70 + fromIntegral n) ("svc" <> show n) Nothing "" | n <- [0 :: Int .. 49]]
        affordable = LT.affordableRecords (length subs)
    budgetRt <- LT.newHub >>= (`runtimeWith` subs)
    budget <- LT.publishMatches budgetRt testPid (V.fromList (replicate (affordable + 5) (logRecord "untailed" "prod" "info" "x")))
    (getSum budget.evaluated, getSum budget.skipped) `shouldBe` (affordable, 5)
    -- A full queue keeps the newest rows and counts the oldest it lost.
    [(burst, lost)] <- delivered 2 [mkSub 7 "checkout" Nothing ""] [rec' "info" (show n) | n <- [1 :: Int .. 5]]
    (map bodyOf burst, lost) `shouldBe` (["4", "5"], 3)
    -- Shipbubble 2026-10-19: a 500-row drain went out as one frame, parsed on the main thread.
    let sub = mkSub 28 "checkout" Nothing ""
    hub <- LT.newHub
    conn <- LT.newConn LT.queueCapacity
    _ <- LT.attachConn hub sub.id conn
    rt <- runtimeWith hub [sub]
    _ <- LT.publishMatches rt testPid (V.fromList [rec' "info" (show i) | i <- [1 .. 250 :: Int]])
    frames <- takeFrames 4 (LiveTailPage.streamFrom sub conn pass)
    map (length . T.breakOnAll "\"shape\"") (filter ("event: log" `T.isPrefixOf`) frames) `shouldBe` [100, 100, 50]

  it "live tail transport: envelopes, routing, reconnects, broken filters, failing brokers" \_ -> do
    -- Kafka is optional infrastructure; without it the Postgres relay carries the tail.
    map LT.transportFor [[], [""], ["broker:9092"]] `shouldBe` [LT.PostgresRelay, LT.PostgresRelay, LT.KafkaTopic LT.kafkaTopicName]
    let boom = logRecord "checkout" "prod" "error" "boom"
        a = mkSub 32 "checkout" Nothing ""
        envFor s scope cols r = LT.LiveEnvelope LT.envelopeVersion s.id (LT.toLiveRow scope cols r)
        logEnv = envFor a (LT.LogTail (Just "checkout") LT.SKLogs) [] boom
        roundTrip env = either (fail . ("envelope did not decode: " <>)) pure (AE.eitherDecode @LT.LiveEnvelope (AE.encode env))
    forM_ [logEnv, envFor a (LT.AllSignals Nothing) ["kind"] boom] \env -> roundTrip env >>= \d -> (d.v, d.subscriptionId) `shouldBe` (env.v, env.subscriptionId)
    -- Every pod consumes every message: delivering to the wrong local queue leaks rows across tails.
    hub <- LT.newHub
    [mine, theirs] <- replicateM 2 (LT.newConn 10)
    zipWithM_ (\s c -> LT.attachConn hub s.id c) [a, mkSub 33 "billing" Nothing ""] [mine, theirs]
    roundTrip logEnv >>= LT.deliver hub
    (map bodyOf . fst <$> LT.takeBatch mine) `shouldReturn` ["boom"]
    atomically (isEmptyTBQueue theirs.queue) `shouldReturn` True
    -- Producers from before projected fields still decode during a rolling deploy; a version
    -- skew is read first so it is reported as a skew, not as corruption.
    let legacyRow = AE.object ["shape" AE..= ("log" :: Text), "log" AE..= AE.object ["id" AE..= UUID.toText UUID.nil, "timestamp" AE..= frozenTime, "level" AE..= Just @Text "info", "service" AE..= Just @Text "checkout", "trace_id" AE..= Nothing @Text, "span_id" AE..= Nothing @Text, "name" AE..= Just @Text "legacy", "body" AE..= ("still visible" :: Text), "truncated" AE..= False]]
        envelope v row = AE.object ["v" AE..= v, "subscription_id" AE..= a.id.toText, "row" AE..= row]
        outcome = \case LT.Delivered e -> (case e.row of LT.LogRow f -> "log fields=" <> show (Map.size f.fields); _ -> "other row"); LT.VersionMismatch _ -> "skew"; LT.Undecodable _ -> "undecodable" :: Text
    map (outcome . LT.envelopeFromValue) [envelope LT.envelopeVersion legacyRow, envelope (LT.envelopeVersion - 1) (AE.object []), AE.object ["nope" AE..= True]] `shouldBe` ["log fields=0", "skew", "undecodable"]
    -- At-most-once: the lease survives a reconnect, the queue does not.
    let sub = mkSub 90 "checkout" Nothing ""
    reHub <- LT.newHub
    first' <- LT.newConn 100
    detach <- LT.attachConn reHub sub.id first'
    rt <- runtimeWith reHub [sub]
    let publish body = void $ LT.publishMatches rt testPid (V.fromList [logRecord "checkout" "prod" "info" body])
    publish "while connected" >> detach >> publish "during the gap"
    second' <- LT.newConn 100
    _ <- LT.attachConn reHub sub.id second'
    publish "after reconnect"
    (map bodyOf . fst <$> LT.takeBatch second') `shouldReturn` ["after reconnect"]
    -- A filter a deploy stopped compiling drops only that tail, and tells its browser rather than going quiet.
    let good = mkSub 8 "checkout" Nothing "level == \"error\""
        bad = mkSub 80 "checkout" Nothing "level == \"error\" | summarize count()"
        (grouped, rejected) = LT.compileSubs [good, bad]
    (map fst rejected, map (.sub.id) (fold (HM.elems grouped))) `shouldBe` ([bad.id], [good.id])
    badConn <- LT.newConn 10
    noticeHub <- LT.newHub
    _ <- LT.attachConn noticeHub bad.id badConn
    noticeRt <- runtimeWith noticeHub []
    LT.noticeBrokenFilters noticeRt rejected
    (fst <$> LT.takeBatch badConn) >>= (`shouldSatisfy` \case [LT.Notice msg] -> "no longer valid" `T.isInfixOf` msg; _ -> False)
    -- A broker that is down costs the tail its rows and ingestion nothing, and is counted.
    cache <- LT.newSubCache
    _ <- LT.refreshSubCache cache frozenTime [sub]
    buf <- LT.newRelayBuffer
    failed <- LT.publishMatches LT.Runtime{transport = LT.PostgresRelay, cache, hub = reHub, relayBuffer = buf, emit = \_ -> Safe.throwString "broker is down"} testPid (V.fromList [boom, boom])
    (getSum failed.matched, getSum failed.publishFailed) `shouldBe` (2, 2)
    -- Relay buffer overflow is expected under load and counted, never silent.
    overflow <- LT.newRelayBuffer
    traverse_ (LT.bufferForRelay overflow . (\n -> envFor (mkSub n "checkout" Nothing "") (LT.LogTail (Just "checkout") LT.SKLogs) [] boom)) [1 .. fromIntegral LT.relayBufferDepth + 5]
    (first length <$> LT.takeRelayBuffer overflow) `shouldReturn` (fromIntegral LT.relayBufferDepth, 5)

  around withTestResources do
    it "rows: queue-path ingest → list, columns, time windows, trace trees, aggregates; history recorded by the rows request only" \tr -> do
      pid <- createTestProject tr "explorer-rows"
      let fetch = explorerRows tr pid
          ingest vs = do
            msgs <- forM vs \v -> ("m",) <$> freshMsg pid v
            bimap (show @Text) (length . fst) <$> runTestBackground frozenTime tr.trATCtx (processMessages msgs HM.empty)
          msgAt f s = f testRequestMsgs (stampAt s)
          recorded = (,) <$> dbOne @_ @Int tr [sql| SELECT count(*)::int FROM projects.query_library WHERE project_id = ? AND query_type = 'history' |] (Only pid) <*> dbOne @_ @Bool tr [sql| SELECT 'explored_logs' = ANY(onboarding_steps_completed) FROM projects.projects WHERE id = ? |] (Only pid)
      -- The body preloads links on hover, so the page shell GET runs for pages nobody opened:
      -- only the rows request records history and onboarding, once per query, not per page.
      let q = Just "status_code == \"500\""
      _ <- explorerPage tr pid q Nothing Nothing
      recorded `shouldReturn` (0, False)
      replicateM_ 2 $ fetch def{Log.query = q}
      _ <- fetch def{Log.query = q, Log.cursor = Just frozenTime}
      recorded `shouldReturn` (1, True)
      blank <- fetch def
      (V.length blank.logsData, blank.count, blank.cols) `shouldBe` (0, 0, defaultCols)

      -- Distinct msg ids: without one, byte-identical copies are ONE event on TimeFusion.
      ingest (concat (replicate 100 [msgAt (.reqMsg1) 0, msgAt (.reqMsg2) 0]) <> [msgAt (.reqMsg1) (-86400), msgAt (.reqMsg2) (-172800)]) `shouldReturn` Right 202
      -- Several rows in one batch, read through a window above the earliest: a TimeFusion mem
      -- bucket advertised its routing timestamp and pruned unflushed rows (fixed in bf3abc8).
      ingest [msgAt (.reqMsg1) s | s <- [-3600, -7200, -10800]] `shouldReturn` Right 3
      everything <- eventually (fetch (span' (-259200) 86400)) ((>= 205) . V.length . (.logsData))
      (V.length everything.logsData, everything.cols) `shouldBe` (205, defaultCols)
      everything.count `shouldSatisfy` (>= 205)
      -- Detail and trace routes capture this cell as UTCTime; TF once echoed the to_char format.
      filter (isNothing . (iso8601ParseM @Maybe @UTCTime . toString <=< str "timestamp")) (cells everything) `shouldBe` []
      -- The stamped URLs target the data endpoint the web component consumes, not the page shell.
      (everything.nextUrl, everything.resetLogsUrl, everything.recentUrl) `shouldSatisfy` \(n, r, c) -> "/log_explorer/data" `T.isInfixOf` n && r /= "" && c /= ""
      (V.length . (.logsData) <$> eventually (fetch (span' (-9000) (-5400))) ((>= 1) . V.length . (.logsData))) `shouldReturn` 1
      [hour, day] <- forM ["1H", "24H"] \s -> (.count) <$> fetch def{Log.since = Just s}
      (hour >= 200, day > hour, day < 205) `shouldBe` (True, True, True)
      filtered <- fetch (span' (-60) 60){Log.query = Just "status_code == \"200\""}
      (filtered.count, V.length filtered.logsData) `shouldSatisfy` \(c, n) -> c > 0 && n > 0
      ((.cols) <$> fetch (span' (-60) 60){Log.cols = Just "id,timestamp,name,duration"}) `shouldReturn` ["id", "timestamp", "name", "duration", "service", "summary", "latency_breakdown"]

      -- Trace-tree entries reference only this page's traces and spans.
      page <- fetch (span' (-60) 60)
      let pageTraces = mapMaybe (str "trace_id") (cells page)
          pageSpans = mapMaybe (\c -> str "latency_breakdown" c <|> str "id" c) (cells page)
      page.traces `shouldSatisfy` (not . null)
      forM_ page.traces \e -> do
        (e.traceId /= "", e.root /= "", e.startTime >= 0, e.duration >= 0) `shouldBe` (True, True, True, True)
        pageTraces `shouldContain` [e.traceId]
        forM_ (e.root : concat (Map.elems e.children)) \sid -> pageSpans `shouldContain` [sid]

      -- A summarize sent to the rows endpoint returns rather than indexing past the aggregate
      -- row; a `| where` after it filters in HAVING, on the alias, while unknown names stay rejected.
      forM_ ([("kind == \"server\" | summarize count(*) by kind", span' (-60) 60), ("summarize count(*) by bin_auto(timestamp)", def)] :: [(Text, Log.LogDataQuery)]) \(agg, w) ->
        ((.queryResultCount) <$> fetch w{Log.query = Just agg}) >>= (`shouldSatisfy` (>= 0))
      ((.error) <$> fetch (span' (-60) 60){Log.query = Just "summarize count() by kind | where count_ > 1"}) `shouldReturn` Nothing
      ((.error) <$> fetch def{Log.query = Just "summarize count() by kind | where nosuchalias > 1"}) >>= (`shouldSatisfy` maybe False (T.isInfixOf "Unknown field"))

      apiKey <- createTestAPIKey tr pid "metric-summary-key"
      now <- getCurrentTime
      ingestMetric tr apiKey [] [] "summary.pressure" 28 (addUTCTime (-2) now)
      ingestMetric tr apiKey [] [] "summary.pressure" 80 (addUTCTime (-1) now)
      let metricTable suffix = toBaseServantResponse tr $ Log.queryEvents pid (Just $ "metrics | summarize peak=max(value)" <> suffix) Nothing (Just $ toText $ iso8601Show $ addUTCTime (-60) now) (Just $ toText $ iso8601Show $ addUTCTime 60 now) Nothing Nothing Nothing Nothing Nothing Nothing
      ((\r -> (r.cols, r.logsData)) <$> metricTable "") `shouldReturn` (["peak"], V.singleton (V.singleton $ AE.Number 80))
      ((\r -> (r.cols, r.logsData)) <$> metricTable " by metric_name") `shouldReturn` (["metric_name", "peak"], V.singleton (V.fromList [AE.String "summary.pressure", AE.Number 80]))

    it "search: scoping, deep links, matched subtrees, free text, body caps, query errors, replay gate, error flags" \tr -> do
      pid <- createTestProject tr "explorer-search"
      apiKey <- createTestAPIKey tr pid "search-key"
      let fetch = explorerRows tr pid
          hexId = T.replace "-" "" . UUID.toText <$> nextRandom
          spanWith name attrs res = do
            (trId, sid) <- (,) <$> hexId <*> hexId
            ingestSpanReq tr $ mkSpanRequest trId sid Nothing name [] Nothing attrs (mkResource apiKey res) frozenTime
            pure trId
      -- Shipbubble 2026-10-19: multi-MB bodies reached the browser whole inside `summary`.
      ingestLog tr apiKey (T.replicate 2_000_000 "x") frozenTime
      fat <- eventually (fetch (span' (-60) 60)) (not . V.null . (.logsData))
      [T.length t | c <- cells fat, Just (AE.Array els) <- [HM.lookup "summary" c], AE.String t <- V.toList els] `shouldSatisfy` \ls -> not (null ls) && all (<= 8000) ls

      -- `deployment.environment.name` is a promoted column so the app-wide selector scopes every
      -- query; an environment nothing reports is empty, not unfiltered.
      _ <- spanWith "GET /env/prod" [] [mkAttr "deployment.environment.name" "prod", mkAttr "service.name" "checkout"]
      _ <- spanWith "GET /env/staging" [] [mkAttr "deployment.environment.name" "staging", mkAttr "service.name" "catalog"]
      let envNames rs = sort [n | r <- rs, Just n <- [str "span_name" r], "GET /env/" `T.isPrefixOf` n]
          tableNames envM svcM = envNames <$> logTable tr pid envM svcM
          eventNames envM svcM = envNames . cells <$> toBaseServantResponse tr (Log.queryEvents pid Nothing Nothing (stamp (-60)) (stamp 60) Nothing Nothing Nothing Nothing envM svcM)
      eventually (tableNames Nothing Nothing) ((== 2) . length) `shouldReturn` ["GET /env/prod", "GET /env/staging"]
      forM_ ([(tableNames, Nothing, Nothing, ["GET /env/prod", "GET /env/staging"]), (tableNames, Just "prod", Nothing, ["GET /env/prod"]), (tableNames, Nothing, Just "catalog", ["GET /env/staging"]), (tableNames, Just "staging", Just "catalog", ["GET /env/staging"]), (tableNames, Just "does-not-exist", Nothing, []), (eventNames, Nothing, Nothing, ["GET /env/prod", "GET /env/staging"]), (eventNames, Just "prod", Nothing, ["GET /env/prod"]), (eventNames, Nothing, Just "checkout", ["GET /env/prod"]), (eventNames, Just "staging", Just "catalog", ["GET /env/staging"])] :: [(Maybe Text -> Maybe Text -> IO [Text], Maybe Text, Maybe Text, [Text])]) \(names, envM, svcM, expected) ->
        names envM svcM `shouldReturn` expected

      linked <- spanWith "GET /linked-checkout" [] []
      _ <- spanWith "GET /unrelated" [] []
      deepLink <- eventually (fetch (span' (-60) 60){Log.query = Just ("trace_id == \"" <> linked <> "\""), Log.cols = Just "id,timestamp,name,context___trace_id"}) ((>= 1) . V.length . (.logsData))
      (deepLink.count, V.length deepLink.logsData) `shouldBe` (1, 1)

      -- A match brings its descendants, never siblings sharing the trace: A (matched) + B + C, not E, D.
      treeId <- hexId
      [a, b, c, e, d] <- replicateM 5 hexId
      forM_ ([(a, Nothing, "ui.tree.root", [("ui.marker", "tree")]), (b, Just a, "ui.tree.child", []), (c, Just b, "ui.tree.grandchild", []), (e, Nothing, "ui.tree.unrelated.root", []), (d, Just e, "ui.tree.unrelated.child", [])] :: [(Text, Maybe Text, Text, [(Text, Text)])]) \(sid, parent, name, extras) ->
        ingestSpanLinked tr apiKey treeId sid parent name extras (addUTCTime (-30) frozenTime)
      subtree <- eventually (fetch (span' (-90) 30){Log.query = Just "attributes.ui.marker == \"tree\""}) ((>= 3) . V.length . (.logsData))
      (V.length subtree.logsData, subtree.count) `shouldBe` (3, 1)

      -- Shipbubble 2026-10-19: `deadlock` hid in span status messages and needed quoting to search.
      ingestSpanReq tr =<< createOtelTraceWithExceptionAtTime apiKey ([], [], []) "update orders" "QueryError" "Deadlock found when trying to get lock; try restarting transaction" "" frozenTime
      forM_ ["ER_LOCK_DEADLOCK while saving order", "order saved", "hello beautiful world", "Hello, world", "hello-world", "goodbye world"] \body -> ingestLog tr apiKey body frozenTime
      let hits q = (\r -> (r.error, V.length r.logsData)) <$> fetch (span' (-60) 60){Log.query = Just q, Log.cols = Just "id,body"}
      _ <- eventually ((,) <$> hits "\"hello\"" <*> hits "\"deadlock\"") (== ((Nothing, 3), (Nothing, 2)))
      forM_ ([("\"deadlock\"", 2), ("\"hello\"", 3), ("\"Hello\" \"world\"", 3), ("\"hello\" AND \"world\"", 3), ("\"hello world\"", 2), ("\"goodbye\"", 1)] :: [(Text, Int)]) \(q, n) -> hits q `shouldReturn` (Nothing, n)
      -- Query errors: an unknown field is named before any SQL is built (2026-08-06 shipped
      -- `attribute` to the database); bad syntax answers empty rather than crashing.
      ((.error) <$> fetch def{Log.query = Just "deadlock"}) `shouldReturn` Just "Unknown field \"deadlock\". To search text, quote it: \"deadlock\""
      ((\r -> (r.error, V.length r.logsData)) <$> fetch def{Log.query = Just "attribute contains \"Red Flashlight\""}) `shouldReturn` (Just "Unknown field \"attribute\". Did you mean \"attributes\"?", 0)
      forM_ ["status_code = 200", "status_code === \"200\""] \bad -> ((\r -> (V.length r.logsData, r.count)) <$> fetch def{Log.query = Just bad}) `shouldReturn` (0, 0)

      -- 2026-09-01: every browser span carried Replay because the `session;` tag was stamped at
      -- ingest; it now follows projects.replay_sessions, knowable only at read time.
      sessionId <- nextRandom
      _ <- spanWith "GET /replay-gate" [mkAttr "session.id" (UUID.toText sessionId)] []
      let sessionTags = do
            rs <- logTable tr pid Nothing Nothing
            pure (any ((== Just "GET /replay-gate") . str "span_name") rs, [t | r <- rs, Just (AE.Array els) <- [HM.lookup "summary" r], AE.String t <- V.toList els, "session;" `T.isPrefixOf` t])
      eventually sessionTags fst `shouldReturn` (True, [])
      dbExec tr [sql| INSERT INTO projects.replay_sessions (session_id, project_id) VALUES (?, ?) |] (sessionId, pid)
      (snd <$> sessionTags) >>= (`shouldSatisfy` any (T.isSuffixOf (UUID.toText sessionId)))

      flagPid <- createTestProject tr "explorer-error-flag"
      flagKey <- createTestAPIKey tr flagPid "error-flag-key"
      ingestErrorLog tr flagKey "boom: db connection failed" [] frozenTime
      ingestLog tr flagKey "ordinary info line" frozenTime
      let logFlags = map (\r -> HM.lookup "errors" r == Just (AE.Bool True)) . filter ((== Just "log") . str "kind") <$> logTable tr flagPid Nothing Nothing
      (sort <$> eventually logFlags ((== 2) . length)) `shouldReturn` [False, True]

    it "pagination: older and newer cursors, edges, orphan headers, events API children, the 500-row cap" \tr -> do
      pid <- createTestProject tr "explorer-pages"
      apiKey <- createTestAPIKey tr pid "pages-key"
      let ago i = addUTCTime (negate (fromIntegral (i :: Int))) frozenTime
          ingestSet set i ts = nextRandom >>= \t -> nextRandom >>= \s -> ingestSpanLinked tr apiKey (UUID.toText t) (UUID.toText s) Nothing (set <> ".row." <> show i) [("page.set", set)] ts
          fetchSet set lim cur dir = explorerRows tr pid def{Log.query = Just ("attributes.page.set == \"" <> set <> "\"" <> lim), Log.cursor = cur, Log.direction = dir, Log.from = stamp (-60), Log.to = stamp 0}
          -- Response order, never re-sorted: the display contract is what is under test.
          idx r = mapMaybe (readMaybe . toString . T.takeWhileEnd (/= '.') <=< str "span_name") (cells r) :: [Int]
      forM_ [1 .. 5] \i -> ingestSet "older" i (ago i)
      forM_ [1 .. 6] \i -> ingestSet "newer" i (ago (10 - i))
      p1 <- eventually (fetchSet "older" " | limit 2" Nothing Nothing) ((== [1, 2]) . idx)
      p2 <- fetchSet "older" " | limit 2" (p1.cursor >>= nextCursor) Nothing
      p3 <- fetchSet "older" " | limit 2" (p2.cursor >>= nextCursor) Nothing
      map (\r -> (idx r, r.hasMore)) [p1, p2, p3] `shouldBe` [([1, 2], True), ([3, 4], True), ([5], False)]
      p2.cursor `shouldNotBe` p1.cursor
      -- Live tail and "Load newer" page from the client's newest row: the server must answer with
      -- the rows adjacent to that cursor, newest-first, or a burst strands a hole nothing recovers.
      (idx <$> fetchSet "newer" " | limit 2" (Just (ago 7)) (Just Parser.PageNewer)) `shouldReturn` [4, 3]
      (idx <$> fetchSet "newer" " | limit 2" (Just (addUTCTime 0.001 (ago 4))) (Just Parser.PageNewer)) `shouldReturn` []

      -- Synthesized "Upstream span missing" headers are display rows: they neither cut hasMore
      -- short nor make the drain revisit or skip a real row.
      orphTid <- UUID.toText <$> nextRandom
      ghost <- UUID.toText <$> nextRandom
      forM_ [1, 2] \i -> nextRandom >>= \s -> ingestSpanLinked tr apiKey orphTid (UUID.toText s) (Just ghost) ("orph.child." <> show i) [("page.set", "orphan")] (ago i)
      forM_ [3 .. 5] \i -> ingestSet "orphan" i (ago i)
      let synth c = maybe False ("Upstream span missing" `T.isInfixOf`) (str "span_name" c)
          realIds r = [sid | c <- cells r, not (synth c), Just sid <- [str "latency_breakdown" c]]
          drain cur acc n = do
            r <- fetchSet "orphan" " | limit 2" cur Nothing
            let acc' = acc <> realIds r
            if r.hasMore then drain (r.cursor >>= nextCursor) acc' (n + 1) else pure (acc', n + 1 :: Int)
      o1 <- eventually (fetchSet "orphan" " | limit 2" Nothing Nothing) (any synth . cells)
      o1.hasMore `shouldBe` True
      (seen, pages) <- drain Nothing [] 0
      (pages, length seen, length (ordNub seen)) `shouldBe` (3, 5, 5)

      -- v1/CLI events API: --with-children must page through every matched root.
      forM_ [1 .. 3] \i -> do
        rootTid <- UUID.toText <$> nextRandom
        rootSid <- UUID.toText <$> nextRandom
        ingestSpanLinked tr apiKey rootTid rootSid Nothing ("evroot." <> show i) [("page.set", "events")] (ago (i * 10))
        replicateM_ 2 $ nextRandom >>= \c -> ingestSpanLinked tr apiKey rootTid (UUID.toText c) (Just rootSid) "evchild" [] (addUTCTime 0.1 (ago (i * 10)))
      let eventsPage toM = toBaseServantResponse tr (Log.queryEvents pid (Just "attributes.page.set == \"events\"") Nothing (stamp (-120)) toM Nothing (Just 2) (Just True) Nothing Nothing Nothing)
          drainEvents toM acc n = do
            r <- eventsPage toM
            let acc' = acc <> [t | c <- cells r, Just t <- [str "span_name" c], "evroot." `T.isPrefixOf` t]
            if r.hasMore then drainEvents (toText . iso8601Show <$> (r.cursor >>= nextCursor)) acc' (n + 1) else pure (acc', n + 1 :: Int)
      (roots, eventPages) <- drainEvents Nothing [] 0
      ordNub roots `shouldMatchList` ["evroot.1", "evroot.2", "evroot.3"]
      eventPages `shouldSatisfy` (> 1)

      -- One merged export keeps 520 rows a single dual-write.
      reqs <- forM [1 .. 520 :: Int] \i -> nextRandom >>= \t -> nextRandom >>= \s -> pure (mkSpanRequest (UUID.toText t) (UUID.toText s) Nothing "seed.row" [] Nothing [mkAttr "page.set" "cap"] (mkResource apiKey []) (addUTCTime (fromIntegral i * (-0.05)) frozenTime))
      ingestSpanReq tr $ mergeSpanRequests reqs
      capped <- eventually (fetchSet "cap" "" Nothing Nothing) ((>= 500) . V.length . (.logsData))
      (V.length capped.logsData, capped.hasMore, isJust . str "timestamp" <$> viaNonEmpty last (cells capped)) `shouldBe` (500, True, Just True)

    -- The query editor underlines exactly what this returns, so the positions are the contract.
    -- Saved queries use the raw ___ column names (92 in production); metrics queries are judged
    -- against the metrics table; summarize aliases are real names downstream.
    it "query validation locates unknown fields and syntax errors under the query's own source" \tr -> do
      let metricsQ = "metric_name == \"redis.commands\" | summarize avg(value) by bin_auto(timestamp)"
      results <- forM ([(Just "kind == \"log\"", Nothing), (Just "kind == \"a\" and attribut contains \"x\"", Nothing), (Just "attributes contains ddd", Nothing), (Just metricsQ, Just "metrics"), (Just metricsQ, Nothing), (Just "context___trace_id != null and resource___service___name == \"api\"", Nothing), (Just "context___trace_ix != null", Nothing), (Nothing, Nothing), (Just "level == \"ERROR\" | summarize count() by kind | where count_ > 1", Nothing)] :: [(Maybe Text, Maybe Text)]) \(q, src) ->
        call tr (Log.logExplorerValidateH testPid q src) <&> \v -> show (q, src) <> "\n  " <> show v
      goldenText "log-explorer-validation.txt" (unlines results)

    it "page shell: preload placement, detail-request sync, viz switching, trace overlay, editor, facets, alert form" \tr -> do
      seedSummary tr testPid
      [plain, chart, line, traced] <- forM ([(Nothing, Nothing), (Nothing, Just "timeseries"), (Nothing, Just "timeseries_line"), (Just "774115aaa715abf80d93fc629c2269a4/?timestamp=2026-07-15T18:59:16.952128Z", Nothing)] :: [(Maybe Text, Maybe Text)]) \(ref, viz) -> explorerPage tr testPid Nothing ref viz
      -- HTMX swaps only #main-content, so a preload in <head> was discarded on navigation; chart
      -- modes and trace links hide the list, so preloading its rows ran a query nobody saw.
      map (any (T.isInfixOf "window.logDataPromise") . elementsWith ["id=\"main-content\""]) [plain, chart, line, traced] `shouldBe` [True, False, False, False]
      alertForm <- renderT <$> call tr (Log.alertFormH testPid Nothing)
      -- The detail container replaces an in-flight request (htmx's default queue dropped the new
      -- click and froze the loader); the viz container reads its type from the mutable
      -- widgetJSON; the trace overlay is no scroll container; the editor's example is a placeholder.
      goldenParts "log-explorer-shell"
        . ordNub
        $ tagsWith ["id=\"log_details_container\"", "id=\"trace_expanded_view\"", "data-next-window", "h-4 w-px shrink-0 bg-strokeWeak opacity-60", "aria-label=\"Save query\"", "id=\"ai-search-submit\"", "api-log-explorer/\"", "bg-bgRaised rounded-lg border border-strokeWeak focus-within:ring-2", "data-field=\"resource.service.name\"", "/orders", "/log_explorer/facets\"", "facets?group=http"] plain
        <> elementsWith ["data-query-input"] plain
        <> tagsWith ["div id=\"visualization-widget-container\""] line
        <> filter (T.isInfixOf "\"type\":\"timeseries_line\"") (elementsWith ["var widgetJSON = {"] line)
        <> tagsWith ["template id=\"trace-loading-skeleton\"", "init send loadTrace"] traced
        <> elementsWith ["Loading trace…", "Retry loading trace"] traced
        <> tagsWith ["name=\"environment\"", "name=\"service\""] alertForm
        <> elementsWith [">Create monitor</h3>"] alertForm

    it "detail panel: TimeFusion and Postgres reads, lazy tabs, identity pills, SDK bodies anchored on the request" \tr -> do
      apiKey <- createTestAPIKey tr testPid "detail-key"
      let hexId = T.replace "-" "" . UUID.toText <$> nextRandom
          b64 = B64T.extractBase64 . B64.encodeBase64 . encodeUtf8 @Text
      (trId, rootSid, sdkSid) <- (,,) <$> hexId <*> hexId <*> hexId
      ingestSpanLinked tr apiKey trId rootSid Nothing "GET /api/orders" [("http.request.method", "GET"), ("http.route", "/api/orders"), ("session.id", "sess-9f2"), ("user.id", "usr-4471"), ("user.email", "ada@example.com"), ("tenant.id", "acme-eu"), ("workspace.id", "ws-12")] frozenTime
      ingestSpanLinked tr apiKey trId sdkSid (Just rootSid) "apitoolkit-http-span" [("http.request.body", b64 "{\"item\":\"apple\"}"), ("http.response.body", b64 "{\"ok\":true}"), ("http.request.method", "GET")] (addUTCTime 0.01 frozenTime)
      [(rootId, rootTs, "GET /api/orders", _), (sdkId, sdkTs, "monoscope.http", sdkStoredSid)] <-
        dbQuery @_ @(UUID.UUID, UTCTime, Text, Text) tr [sql| SELECT id, timestamp, name, context___span_id FROM otel_logs_and_spans WHERE project_id = ? AND context___trace_id = ? ORDER BY start_time |] (testPid, trId)
      let ctx = tr.trATCtx
          withTf tf = tr{trATCtx = ctx{env = ctx.env{enableTimefusionReads = tf}}}
          item tf rid ts tab subtab partial = call (withTf tf) $ LogItem.expandAPIlogItemH testPid rid ts Nothing tab subtab partial
          anchored = \case
            LogItem.SpanItemExpanded _ anchor sdkM _ -> (anchor.name, (.name) <$> sdkM)
            LogItem.ItemDetailedNotFound msg -> (Just ("not found: " <> msg), Nothing)
            _ -> (Just "other item", Nothing)
          request = (Just "GET /api/orders", Just (Just "monoscope.http"))
      -- The SDK payload span confused users: the request's panel showed empty body tabs and a
      -- click on the SDK span opened a synthetic span. Both clicks land on the request now.
      forM_ [True, False] \tf -> do
        (anchored <$> item tf rootId rootTs Nothing Nothing False) `shouldReturn` request
        -- An exact timestamp, no window: one second off finds nothing on either store.
        (anchored <$> item tf rootId (addUTCTime 1 rootTs) Nothing Nothing False) >>= (`shouldSatisfy` maybe False ("not found" `T.isPrefixOf`) . fst)
      (anchored <$> item True sdkId sdkTs (Just "tab-req") Nothing False) `shouldReturn` request
      -- Legacy rows carry NULL hashes, which the Postgres read must still decode.
      dbExec tr [sql| UPDATE otel_logs_and_spans SET hashes = NULL WHERE id = ? |] (Only rootId)
      (anchored <$> item False rootId rootTs Nothing Nothing False) `shouldReturn` request
      (call (withTf True) (TelemetryPage.traceH testPid trId (Just rootTs) Nothing Nothing Nothing Nothing)) >>= \case
        TelemetryPage.TraceDetails{} -> pass
        _ -> expectationFailure "expected TimeFusion trace details"
      -- Waterfall keyboard-nav onto the SDK span anchors on the request too.
      call tr (TelemetryPage.traceH testPid trId (Just rootTs) (Just sdkStoredSid) (Just "next") Nothing Nothing) >>= \case
        TelemetryPage.SpanDetails _ target sdkM -> (target.name, (.name) <$> sdkM) `shouldBe` request
        _ -> expectationFailure "expected SpanDetails anchored on the request span"
      waterfall <- renderT <$> call tr (TelemetryPage.traceH testPid trId (Just rootTs) Nothing Nothing Nothing Nothing)
      (T.isInfixOf "monoscope.http" waterfall, T.isInfixOf "timeline.addEventListener('tab-visible'" waterfall) `shouldBe` (False, True)

      initial <- renderT <$> item True rootId rootTs Nothing Nothing False
      rawTab <- renderT <$> item True rootId rootTs (Just "tab-raw") Nothing True
      reqTab <- renderT <$> item True rootId rootTs (Just "tab-req") Nothing False
      reqBody <- renderT <$> item True rootId rootTs (Just "tab-req") (Just "htab-req") True
      bogusTab <- renderT <$> item True rootId rootTs (Just "tab-nonexistent") Nothing False
      -- Hidden tabs fetch their panel on first reveal, so the first response stays small, and the
      -- request body is inlined once (in copy-as-curl), never twice.
      T.length initial `shouldSatisfy` (< 50_000)
      (T.count "apple" reqTab, "apple" `T.isInfixOf` reqBody) `shouldBe` (1, True)
      -- Every session, user and tenant field is a filter pill (only user.email used to show);
      -- waterfall rows are keyboard-reachable buttons.
      goldenParts "log-explorer-detail"
        $ tagsWith ["id=\"m-raw-content\""] initial
        <> filter (T.isPrefixOf "<span") (elementsWith ["ID: ", "Email: "] initial)
        <> tagsWith ["data-field-value=\"&quot;"] initial
        <> tagsWith ["id=\"m-raw-content\""] rawTab
        <> tagsWith ["subtab=htab-req"] reqTab
        <> elementsWith ["curl -X GET"] reqTab
        <> tagsWith ["id=\"req_content\""] reqBody
        <> tagsWith ["\" checked>"] bogusTab
        <> tagsWith ["aria-label=\"Show span details: "] waterfall

    it "query library and aggregate views: save → rename → delete, patterns, sessions, pattern expand" \tr -> do
      pid <- createTestProject tr "explorer-aggregates"
      -- Mutations used to ride the log-fetch GET via ?layout=; they are POST/DELETE now.
      let titled t (QueryLibrary.QueryLibraryView (items, _)) = V.find ((== Just t) . (.title)) items
          save qid t = call tr $ QueryLibrary.saveQueryH pid (QueryLibrary.SaveQueryForm (Just "status_code == \"200\"") qid (Just t))
      saved <- save Nothing "saved"
      qid <- maybe (fail "saved item missing") (pure . (.id.toText)) (titled "saved" saved)
      renamed <- save (Just qid) "renamed"
      deleted <- call tr $ QueryLibrary.deleteQueryH pid qid
      map (isJust . titled "renamed") [renamed, deleted] <> [isJust (titled "saved" renamed)] `shouldBe` [True, False, False]

      apiKey <- createTestAPIKey tr pid "aggregates-key"
      ingestTrace tr apiKey "GET /api/pattern-expand" frozenTime
      pv <- call tr $ Log.logPatternsH pid Nothing (stamp (-60)) Nothing (stamp 60) (Just "spans") Nothing Nothing
      case pv of Log.PatternsView total _ _ _ -> total `shouldSatisfy` (>= 0)
      (case AE.toJSON pv of AE.Object o -> filter (`AEKM.member` o) ["totalPatterns", "colIdxMap"]; _ -> []) `shouldBe` ["totalPatterns", "colIdxMap"]
      -- A live poll (cursor set) merges into a list that already has its summary header.
      sessions <- forM [Nothing, stamp (-60)] \cur -> call tr (Log.logSessionsH pid Nothing (stamp (-60)) Nothing (stamp 60) Nothing Nothing cur) <&> \(Log.SessionsView total _ summ) -> (total >= 0, isJust summ)
      sessions `shouldBe` [(True, True), (True, False)]
      -- Recording-only sessions (spans never arrived) carry no attributes: only the unfiltered or
      -- browser view holds them, with Replay.
      recording <- nextRandom
      dbExec tr [sql| INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name) VALUES (?, ?, ?, ?, 1, 'Ada') |] (recording, pid, frozenTime, addUTCTime 30 frozenTime)
      forM_ ([(Just Parser.browserKql, [(UUID.toText recording, True, Just "Ada")]), (Nothing, [(UUID.toText recording, True, Just "Ada")]), (Just "status_code == \"ERROR\"", [])] :: [(Maybe Text, [(Text, Bool, Maybe Text)])]) \(q, expected) ->
        (call tr (Log.logSessionsH pid q (stamp (-60)) Nothing (stamp 60) Nothing Nothing Nothing) <&> \(Log.SessionsView _ rs _) -> [(r.sessionId, r.hasReplay, r.userName) | r <- V.toList rs]) `shouldReturn` expected

      -- Expand matched the summary template by ILIKE and never found the real row; it matches the
      -- `pat:<hash>` tag the drain-flush stamps, on whichever store the expand reads.
      let patTags = ["pat:abcd1234"] :: [Text]
          spanName = "GET /api/pattern-expand" :: Text
      dbExec tr [sql| UPDATE otel_logs_and_spans SET hashes = ? WHERE project_id = ? AND name = ? |] (PGArray patTags, pid, spanName)
      when tr.trATCtx.env.enableTimefusionReads
        $ runQueryEffect tr
        $ Hasql.withHasqlTimefusion True
        $ Hasql.interpExecute_ [HI.sql| UPDATE otel_logs_and_spans SET hashes = #{patTags} WHERE project_id = #{pid.toText} AND name = #{spanName} |]
      let expand key = call tr (Log.apiLogExpandH pid (Just "pattern") (Just key) Nothing Nothing (stamp (-60)) Nothing (stamp 60)) <&> \case
            AE.Object o | Just (AE.Number n) <- AEKM.lookup "queryResultCount" o -> round n
            _ -> -1 :: Int
      eventually (expand "abcd1234") (== 1) `shouldReturn` 1
      mapM expand ["00000000", spanName] `shouldReturn` [0, 0]

    it "facets: seeded summaries, real-span flushes, narrowed reads, bulk refresh, API wire shape" \tr -> do
      let unknown = mkPid 9999
          facetMap = maybe HM.empty (\s -> let Catalog.FacetData m = s.facetJson in m)
          summaryOf paths p = facetMap <$> runHasqlEffect tr (SC.getFacetSummary paths p "otel_logs_and_spans" frozenTime frozenTime)
          flush p obs = newIORef Hot.emptySchemaShardState >>= \ref -> Hot.observeSpans ref Hot.defaultPolicy p (V.fromList obs) >> void (runHasqlEffect tr (Worker.flushDirty ref))
          values k = map (.value) . fold . HM.lookup k
      resetSchemaLearning tr [testPid, unknown]
      (isNothing <$> runHasqlEffect tr (SC.getFacetSummary Nothing unknown "otel_logs_and_spans" frozenTime frozenTime)) `shouldReturn` True
      toBaseServantResponse tr (ApiH.apiFacets testPid Nothing Nothing Nothing (Just "resource.service.name")) `shouldReturn` AE.object ["resource.service.name" AE..= ([] :: [AE.Value]), "source" AE..= ("fallback" :: Text)]
      -- Catalog paths are stored bare and prefixed by category on read, values by descending count.
      seedSummary tr testPid
      seeded <- summaryOf Nothing testPid
      filter (`notElem` HM.keys seeded) ["attributes.http.request.method", "attributes.db.operation.name", "resource.service.name"] `shouldBe` []
      values "attributes.http.request.method" seeded `shouldBe` ["GET", "POST"]
      wire <- toBaseServantResponse tr $ ApiH.apiFacets testPid Nothing Nothing Nothing Nothing
      (case wire of AE.Object o -> (AEK.toText <$> AEKM.keys o) & \ks -> ("attributes.http.request.method" `elem` ks, any (T.isInfixOf "___") ks); _ -> (False, True)) `shouldBe` (True, False)

      -- Through the production walker: #401's HTTP branch once dropped the generic attribute and
      -- resource walks, and log columns reach the summary through the top-level walk.
      resetSchemaLearning tr [testPid]
      flush testPid [richObs testPid "GET" False, richObs testPid "POST" False, richObs testPid "POST" True]
      full <- summaryOf Nothing testPid
      filter (`notElem` HM.keys full) ["attributes.http.request.method", "attributes.http.response.status_code", "attributes.db.operation.name", "attributes.url.path", "resource.service.name", "name", "kind", "status_code", "level", "severity.severity_text", "severity.severity_number"] `shouldBe` []
      (sort (values "attributes.http.request.method" full), values "resource.service.name" full, values "level" full) `shouldBe` (["GET", "POST"], ["checkout-svc"], ["INFO"])
      -- Page shells read only the facets they render; the multi-MB full doc made every page slow.
      let paths = ["attributes.http.request.method", "resource.service.name", "level", "severity.severity_text", "attributes.not.seen"]
      narrowed <- summaryOf (Just paths) testPid
      (HM.size full > HM.size narrowed, map (`HM.lookup` narrowed) paths, HM.member "attributes.db.operation.name" narrowed) `shouldBe` (True, map (`HM.lookup` full) paths, False)

      -- One flush folds every dirty project into one batched upsert, skipping summaries refreshed
      -- within 30s (upsertSummary stamps now() at seed time).
      let projs = map mkPid [1 .. 5]
          freshP = mkPid 11
          staleP = [mkPid 12, mkPid 13]
      resetSchemaLearning tr (projs <> (freshP : staleP))
      seedSummary tr freshP
      updated <- forM [projs, freshP : staleP] \ps -> do
        ref <- newIORef Hot.emptySchemaShardState
        forM_ ps \p -> Hot.observeSpans ref Hot.defaultPolicy p (V.singleton (richObs p "GET" False))
        (.summariesUpdated) <$> runHasqlEffect tr (Worker.flushDirty ref)
      updated `shouldBe` [5, 2]
      (all isJust <$> mapM (\p -> runHasqlEffect tr (SC.getSummary Nothing p)) (projs <> staleP)) `shouldReturn` True

    it "chart cache: hits and partial merges equal fresh reads; streams populate it and replay it without telemetry" \tr -> do
      key <- createTestAPIKey tr testPid "chart-cache-key"
      let countQ = "summarize count(*) by bin_auto(timestamp)"
          ingestAt prefix offsets = forM_ offsets (\(i :: Int) -> ingestLog tr key (prefix <> show i) (addUTCTime (fromIntegral i) baseTime)) >> void (runAllBackgroundJobs frozenTime tr.trATCtx)
          fresh q from to = clearCache tr >> queryMetrics tr q from to
      clearAllTestData tr
      ingestAt "log " [0, 60 .. 1140]
      miss <- queryMetrics tr countQ (timeAt (-1800)) (timeAt 1800)
      hit <- queryMetrics tr countQ (timeAt (-1800)) (timeAt 1800)
      refetched <- fresh countQ (timeAt (-1800)) (timeAt 1800)
      (V.length miss.dataset > 0, sameResult miss hit, sameResult miss refetched) `shouldBe` (True, True, True)
      -- A partial hit fetches only the delta and merges in timestamp order.
      clearAllTestData tr
      ingestAt "first " [-600, -540 .. -60]
      _ <- queryMetrics tr countQ (timeAt (-900)) (timeAt 0)
      ingestAt "second " [0, 60 .. 540]
      merged <- queryMetrics tr countQ (timeAt (-900)) (timeAt 900)
      (sameResult merged <$> fresh countQ (timeAt (-900)) (timeAt 900)) `shouldReturn` True
      let stamps = V.mapMaybe (join . V.headM) merged.dataset
      V.and (V.zipWith (<=) stamps (V.drop 1 stamps)) `shouldBe` True
      -- Multi-column groups: stats were computed from the first column only after a merge.
      clearAllTestData tr
      let byKind = "summarize count(*) by bin_auto(timestamp), kind"
          mixed reps offsets = forM_ offsets \(i :: Int) -> do
            let ts = addUTCTime (fromIntegral i) baseTime
            replicateM_ reps $ ingestLog tr key ("log " <> show i) ts
            replicateM_ (reps - 1) $ ingestTrace tr key ("GET /api/" <> show i) ts
      mixed 3 [-300, -240 .. -60] >> void (runAllBackgroundJobs frozenTime tr.trATCtx)
      _ <- queryMetrics tr byKind (timeAt (-600)) (timeAt 0)
      mixed 4 [0, 60 .. 240] >> void (runAllBackgroundJobs frozenTime tr.trATCtx)
      mergedKinds <- queryMetrics tr byKind (timeAt (-600)) (timeAt 600)
      V.length mergedKinds.headers `shouldSatisfy` (>= 3)
      (sameResult mergedKinds <$> fresh byKind (timeAt (-600)) (timeAt 600)) `shouldReturn` True

      -- The stream matches the whole query, fills the cache, then serves one frame with telemetry gone.
      clearAllTestData tr
      forM_ [17, 3599, 3600, 86400, 172800, 259200] \offset -> ingestLog tr key ("boundary " <> show offset) (addUTCTime (fromIntegral offset) baseTime)
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      let hourly = "summarize count(*) by bin(timestamp, 1h)"
          stream q from to = runQueryEffect tr (Charts.queryMetricsStream (Just Hasql.SqlPostgres) (Just Charts.DTMetric) (Just testPid) (Just q) Nothing Nothing (Just from) (Just to) (Just "spans") Nothing []) >>= streamFrames >>= either fail pure
          final :: [AE.Value] -> IO Charts.MetricsData
          final frames = case viaNonEmpty last frames of
            Just (AE.Object o) | AEKM.lookup "type" o == Just "complete", Just (AE.Success md) <- AE.fromJSON <$> AEKM.lookup "data" o -> pure md
            _ -> fail "missing completion frame"
          shape (md :: Charts.MetricsData) = (md.dataset, md.headers, md.rowsCount)
      expected <- fresh hourly (timeAt 17) (timeAt 259200)
      clearCache tr
      frames <- stream hourly (timeAt 17) (timeAt 259200)
      length frames `shouldSatisfy` (> 2)
      (shape <$> final frames) `shouldReturn` shape expected
      dbExec tr [sql|DELETE FROM otel_logs_and_spans WHERE project_id = ?|] (Only testPid)
      cached <- stream hourly (timeAt 17) (timeAt 259200)
      cachedMd <- final cached
      (length cached, cachedMd.dataset, cachedMd.rowsCount) `shouldBe` (1, expected.dataset, expected.rowsCount)
      -- One query's rows arrive in one burst: framing it as partials redrew every chart three times.
      clearAllTestData tr
      forM_ [60, 1200, 2400, 3500] \offset -> ingestLog tr key ("event " <> show offset) (addUTCTime (fromIntegral offset) baseTime)
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      clearCache tr
      single <- stream "summarize count(*) by bin(timestamp, 10m)" (timeAt 0) (timeAt 3600)
      [k | AE.Object o <- single, Just k <- [AEKM.lookup "type" o], Just (AE.Object d) <- [AEKM.lookup "data" o], Just (AE.Array ds) <- [AEKM.lookup "dataset" d], not (null ds)] `shouldBe` [AE.String "complete"]

    -- Chart SQL failures were swallowed into empty datasets, the same "no data" overlay as an
    -- empty range; and a failed partial-hit delta still advanced `cached_to`, leaving a permanent
    -- hole between the old and new watermark.
    it "chart cache failures: errors surface, the watermark holds, nothing is cached; empty successes are cached and advance" \tr -> do
      let countQ = "summarize count(*) by bin_auto(timestamp)"
          times@(_, t1, t2) = (addUTCTime (-1800) baseTime, baseTime, addUTCTime 1800 baseTime)
          unscoped = runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just testPid) (Just countQ) Nothing Nothing (Just (timeAt (-1800))) (Just (timeAt 1800)) Nothing Nothing []
      clearAllTestData tr
      ((\r -> (r.error, V.length r.dataset)) <$> queryMetrics tr "totally_made_up_column == \"x\" | summarize count(*) by bin_auto(timestamp)" (timeAt (-1800)) (timeAt 1800)) `shouldReturn` (Just "Unknown field \"totally_made_up_column\"", 0)
      ((.error) <$> withoutTelemetryTable tr (queryMetrics tr countQ (timeAt (-1800)) (timeAt 1800))) >>= (`shouldSatisfy` isJust)
      dbOne @_ @Int tr [sql|SELECT count(*)::int FROM query_cache WHERE project_id = ?|] (Only testPid) `shouldReturn` 0
      key <- seedCache tr countQ times
      ((.error) <$> withoutTelemetryTable tr unscoped) >>= (`shouldSatisfy` isJust)
      cachedToFor tr key `shouldReturn` Just t1
      -- A delta that succeeds with no rows is not a failure: the watermark advances.
      _ <- unscoped
      cachedToFor tr key `shouldReturn` Just t2
      -- Empty history is a result too, reused without telemetry; a stored error is never served.
      clearAllTestData tr
      let hourly = queryMetrics tr "summarize count(*) by bin(timestamp, 1h)" (timeAt 0) (timeAt 3600)
          outcome r = (r.error, r.dataset)
      (outcome <$> hourly) `shouldReturn` (Nothing, V.empty)
      (outcome <$> withoutTelemetryTable tr hourly) `shouldReturn` (Nothing, V.empty)
      dbExec tr [sql|UPDATE query_cache SET cached_data = jsonb_set(cached_data, '{error}', '"old failure"') WHERE project_id = ?|] (Only testPid)
      (outcome <$> hourly) `shouldReturn` (Nothing, V.empty)

    it "chart cache scope and streaming: compiler changes and service scope change keys, shared cache expires, streams survive closed connections and slow queries" \tr -> do
      -- Results cached under the old jsonpath predicate must not answer the new compiler's query.
      forM_ ["attributes.url.full matches regex \"^/cache-invalidated$\"", "spans | where attributes.url.full matches regex \"^/cache-invalidated$\"", "attributes.events[*].timestamp > ago(1h)"] \predicate -> do
        clearAllTestData tr
        let q = predicate <> " | summarize count(*) by bin(timestamp, 1h)"
            end = addUTCTime 3600 baseTime
        sections <- either (fail . toString) pure $ parseQueryToAST q
        let cfg = (defSqlQueryCfg testPid baseTime Nothing Nothing){dateRange = (Just baseTime, Just end)}
            key = QC.generateCacheKey testPid Nothing sections cfg
            legacyKey = key{QC.queryHash = toXXHash $ toQText sections <> "\NULenvironment=\NULservice="} :: QC.CacheKey
        QC.generateCacheKey testPid Nothing sections cfg{currentTime = end} `shouldBe` key
        runQueryEffect tr $ QC.updateCache legacyKey (baseTime, end) (seedMetrics baseTime) q
        ((.dataset) <$> queryMetrics tr q (timeAt 0) (timeAt 3600)) `shouldReturn` V.empty
        runQueryEffect tr (QC.lookupCache key (baseTime, end)) >>= (`shouldSatisfy` \case QC.CacheHit _ -> True; _ -> False)
      sections <- either (fail . toString) pure $ parseQueryToAST "summarize count(*) by bin(timestamp, 1h)"
      let scoped svc = (defSqlQueryCfg testPid baseTime Nothing Nothing){dateRange = (Just baseTime, Just $ addUTCTime 3600 baseTime), environment = Just "production", service = Just svc}
      QC.generateCacheKey testPid Nothing sections (scoped "checkout") `shouldNotBe` QC.generateCacheKey testPid Nothing sections (scoped "catalog")
      calls <- newIORef (0 :: Int)
      sharedKey <- ("shared-cache-spec-" <>) . show <$> getMonotonicTimeNSec
      let read' = runQueryEffect tr $ QC.withSharedCache sharedKey 300 (liftIO $ atomicModifyIORef' calls \n -> (n + 1, n + 1))
      (,) <$> read' <*> read' `shouldReturn` (1, 1)
      dbExec tr "UPDATE rum_panel_cache SET expires_at = now() - interval '1 minute' WHERE cache_key = ?" (Only $ toXXHash sharedKey)
      read' `shouldReturn` 2

      clearAllTestData tr
      key <- createTestAPIKey tr testPid "closed-chart-connection"
      ingestLog tr key "one event" (addUTCTime 3600 baseTime)
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      let createPool = newPool $ setNumStripes (Just 1) $ defaultPoolConfig (PG.connectPostgreSQL tr.trConnStr) PG.close 1800 3
      E.bracket createPool destroyAllResources \pool -> withResource pool \active -> do
        withResource pool \idleA -> withResource pool \idleB -> forM_ [idleA, idleB] \conn -> do
          [Only backend] <- PG.query_ conn "SELECT pg_backend_pid()" :: IO [Only Int]
          dbQuery @_ @(Only Bool) tr "SELECT pg_terminate_backend(?, 5000)" (Only backend)
        frames <- runQueryEffect tr{trATCtx = tr.trATCtx{timefusionPgPool = pool}} (Charts.queryMetricsStream (Just Hasql.SqlTimefusion) (Just Charts.DTMetric) (Just testPid) (Just "summarize count(*) by bin(timestamp, 1h)") Nothing Nothing (Just $ timeAt 0) (Just $ timeAt 259200) (Just "spans") Nothing []) >>= streamFrames >>= either fail pure
        (case viaNonEmpty last frames of Just (AE.Object o) -> (AEKM.lookup "type" o, AEKM.lookup "data" o >>= \case AE.Object f -> AEKM.lookup "rows_count" f; _ -> Nothing); _ -> (Nothing, Nothing)) `shouldBe` (Just "complete", Just (AE.Number 1))
        PG.query_ active "SELECT 1" `shouldReturn` [Only (1 :: Int)]
      slow <- runQueryEffect tr (Charts.queryMetricsStream (Just Hasql.SqlPostgres) (Just Charts.DTMetric) (Just testPid) Nothing (Just "SELECT 1::integer AS bucket, 'wait'::text AS series, count(*)::float AS value FROM pg_sleep(6)") Nothing (Just $ timeAt 0) (Just $ timeAt 3600) (Just "spans") Nothing []) >>= streamFrames >>= either fail pure
      let kind = \case AE.Object o -> AEKM.lookup "type" o; _ -> Nothing
      (kind <$> listToMaybe slow, kind <$> viaNonEmpty last slow) `shouldBe` (Just (Just "partial"), Just (Just "complete"))

    -- Wall-clock, not frozenTime: Postgres evaluates `expires_at > now()`, and the test clock is in 2025.
    it "live tail storage: owner scoping, in-statement caps, teardown, expiry, relay, direct OTLP delivery, non-member denial" \tr -> do
      future <- addUTCTime 300 <$> getCurrentTime
      let register user cap pid exp' = runHasqlEffect tr $ LT.insertSubscription pid user (newSub "checkout") (LT.LogTail (Just "checkout") LT.SKLogs) cap 99 exp'
          lookup' pid user sid = fmap (.id) <$> runHasqlEffect tr (LT.activeSubscriptionFor pid user sid)
      sid <- register ownerId 9 testPid future >>= maybe (fail "insert produced no id") pure
      -- Stream, renew and delete all resolve through this lookup: another user or project cannot find it.
      mapM (\(p, u) -> lookup' p u sid) [(testPid, ownerId), (testPid, intruderId), (otherPid, ownerId)] `shouldReturn` [Just sid, Nothing, Nothing]
      -- The cap is a predicate on the INSERT, so a burst against a met cap all loses (an empty cap
      -- under READ COMMITTED is only "about N" — see 'insertSubscription').
      (map isJust <$> replicateM 2 (register ownerId 2 testPid future)) `shouldReturn` [True, False]
      -- int4 counts against the int8 decoder threw, turning "too many live tails" into a 500.
      atCap <- runHasqlEffect tr $ LT.countActiveForUser testPid ownerId
      runHasqlEffect tr (LT.countActiveForProject testPid) >>= (`shouldSatisfy` (>= atCap))
      atCap `shouldBe` 2
      (Ki.scoped \scope -> replicateM 8 (Ki.fork scope (register ownerId atCap testPid future)) >>= atomically . traverse Ki.await) >>= (`shouldBe` []) . filter isJust
      -- Stream teardown drops the lease (~2s, not the ~45s lease); racing the browser's DELETE is fine.
      LT.deleteSubscriptionIO tr.trATCtx.hasqlPool testPid ownerId sid
      lookup' testPid ownerId sid `shouldReturn` Nothing
      LT.deleteSubscriptionIO tr.trATCtx.hasqlPool testPid ownerId sid
      -- Expiry, not DELETE, deactivates: crashed tabs and killed pods never send one.
      expired <- register ownerId 9 testPid (addUTCTime (-360) future) >>= maybe (fail "expired insert refused") pure
      (map (.id) <$> runHasqlEffect tr (LT.activeSubscriptions 100)) >>= (`shouldNotContain` [expired])

      -- The relay is the transport for every deployment without Kafka.
      let relayed = LT.LiveEnvelope LT.envelopeVersion (mkSub 40 "checkout" Nothing "").id (LT.toLiveRow (LT.LogTail (Just "checkout") LT.SKLogs) [] (logRecord "checkout" "prod" "error" "relayed"))
      before <- runHasqlEffect tr LT.relayWatermark
      runHasqlEffect tr (LT.relayPublish [relayed])
      (results, next) <- runHasqlEffect tr (LT.relayDrain before)
      next `shouldSatisfy` (> before)
      map bodyOf [e.row | LT.Delivered e <- results] `shouldContain` ["relayed"]

      -- The hook lived only in the queue path: OTLP straight to the store registered a tail that
      -- said live and matched nothing. A bounded wait, so a missing hook fails instead of hanging.
      let sub = mkSubScoped 100 (LT.LogTail Nothing LT.SKLogs) [] Nothing ""
      conn <- LT.newConn 10
      _ <- LT.attachConn tr.trATCtx.liveTail.hub sub.id conn
      _ <- LT.refreshSubCache tr.trATCtx.liveTail.cache frozenTime [sub]
      apiKey <- createTestAPIKey tr testPid "live-tail-direct-ingest"
      ingestLog tr apiKey "a log that arrived without a queue" frozenTime
      LT.takeBatchWithin 2_000_000 conn >>= \case
        Just ([LT.LogRow row], _) -> do
          recordId <- maybe (fail "live row id was not a UUID") pure (UUID.fromText row.id)
          stored <- call tr $ LiveTailPage.liveTailRecordH testPid recordId row.timestamp
          (stored.id, case AE.toJSON stored of AE.Object o -> length o > 10; _ -> False) `shouldBe` (row.id, True)
        other -> fail ("expected one streamed log row, got " <> show (length . fst <$> other))

      -- Registration never checked membership: anyone could stream another project's logs by id.
      victim <- createTestProject tr "victim"
      assertNonMemberDenied tr victim $ LiveTailPage.liveTailRegisterH victim (newSub "checkout")
      runHasqlEffect tr (LT.countActiveForProject victim) `shouldReturn` 0

    it "share links: created from an ingested event, scoped to real events, expired after 48 hours" \tr -> do
      apiKey <- createTestAPIKey tr testPid "shared-event"
      ingestTrace tr apiKey "shared-checkout" frozenTime
      (eventId, eventTime) <- dbQuery @_ @(UUID.UUID, UTCTime) tr [sql|SELECT id, timestamp FROM otel_logs_and_spans WHERE project_id = ? AND name = 'shared-checkout' ORDER BY timestamp DESC LIMIT 1|] (Only testPid) >>= maybe (fail "the shared event was not ingested") pure . listToMaybe
      missing <- nextRandom
      let shares = dbOne @_ @Int tr [sql|SELECT count(*)::int FROM apis.share_events WHERE project_id = ?|] (Only testPid)
          apiCreate eid = toBaseServantResponse tr $ ApiH.apiShareLinkCreate testPid ApiH.ShareLinkCreate{ApiH.eventId = eid, ApiH.eventCreatedAt = eventTime, ApiH.eventType = Just ShareEvents.ShareLog}
          view = fmap renderT . toBaseServantResponse tr . Share.shareLinkGetH
      initial <- shares
      void $ E.try @SomeException $ testServant tr $ Share.shareLinkPostH testPid missing eventTime (Just ShareEvents.ShareRequest)
      apiCreate missing `shouldThrow` anyException
      shares `shouldReturn` initial
      Share.ShareLinkPost shareText <- call tr $ Share.shareLinkPostH testPid eventId eventTime (Just ShareEvents.ShareRequest)
      shareId <- maybe (fail "the share ID was invalid") pure $ UUID.fromText shareText
      view shareId >>= (`shouldContainAll` ["shared-checkout", "Expires in"])
      (apiCreate eventId >>= view . (.id)) >>= (`shouldContainAll` ["shared-checkout"])
      dbExec tr [sql|UPDATE apis.share_events SET created_at = ? WHERE id = ?|] (frozenTime, shareId)
      advanceDays tr 3
      expired <- view shareId
      (T.isInfixOf "Link expired" expired, T.isInfixOf "shared-checkout" expired) `shouldBe` (True, False)
      -- A 404 thrown as an IO exception skipped the handler's Error ServerError channel.
      path <- (\a b -> "/share/r/" <> UUID.toASCIIBytes a <> "/replay_session/" <> UUID.toASCIIBytes b) <$> nextRandom <*> nextRandom
      resp <- WT.runSession (WT.srequest $ WT.SRequest (WT.setPath Wai.defaultRequest path) "") (Server.mkServer tr.trLogger tr.trATCtx tr.trTracerProvider)
      H.statusCode resp.simpleStatus `shouldBe` 404


defaultCols :: [Text]
defaultCols = ["id", "timestamp", "service", "summary", "latency_breakdown"]


stampAt :: NominalDiffTime -> Text
stampAt s = toText $ iso8601Show $ addUTCTime s frozenTime


stamp :: NominalDiffTime -> Maybe Text
stamp = Just . stampAt


-- | A from/to window relative to 'frozenTime'.
span' :: NominalDiffTime -> NominalDiffTime -> Log.LogDataQuery
span' a b = def{Log.from = stamp a, Log.to = stamp b}


-- | The rows endpoint the log-list web component fetches.
explorerRows :: TestResources -> Projects.ProjectId -> Log.LogDataQuery -> IO Log.LogResult
explorerRows tr pid = call tr . Log.logExplorerDataH pid


explorerPage :: TestResources -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> IO Text
explorerPage tr pid q traceRef viz = renderT <$> call tr (Log.apiLogH pid q Nothing Nothing Nothing Nothing Nothing Nothing Nothing traceRef viz Nothing Nothing Nothing)


-- | The upper bound the next older page expects, as nextUrl builds it (-1ms skips the boundary row).
nextCursor :: Text -> Maybe UTCTime
nextCursor t = addUTCTime (-0.001) <$> iso8601ParseM (toString t)


cells :: Log.LogResult -> [HM.HashMap Text AE.Value]
cells r = [HM.mapMaybe (row V.!?) r.colIdxMap | row <- V.toList r.logsData]


str :: Text -> HM.HashMap Text AE.Value -> Maybe Text
str k m = HM.lookup k m >>= \case AE.String t -> Just t; _ -> Nothing


-- | Spans of the last minute straight from the log table, as column → value maps.
logTable :: TestResources -> Projects.ProjectId -> Maybe Text -> Maybe Text -> IO [HM.HashMap Text AE.Value]
logTable tr pid envM svcM = do
  res <- runQueryEffect tr $ LogQueries.selectLogTable tr.trATCtx.env.enableTimefusionReads pid [] "" Nothing (Just (addUTCTime (-60) frozenTime), Just (addUTCTime 60 frozenTime)) [] (Just SSpans) Nothing envM svcM
  (rs, cols, _) <- either (fail . toString) pure res
  pure [HM.fromList (zip cols (V.toList r)) | r <- V.toList rs]


-- | A legacy SDK message re-pointed at @pid@ with its own @msg_id@, so copies stay distinct events.
freshMsg :: Projects.ProjectId -> AE.Value -> IO ByteString
freshMsg (UUIDId pidU) v = do
  mid <- nextRandom
  pure $ toStrict $ AE.encode (Unsafe.fromJust (convert v)){ProcessMessage.projectId = pidU, ProcessMessage.msgId = Just mid}


mkPid :: Int -> Projects.ProjectId
mkPid n = UUIDId (UUID.fromWords 0 0 0 (fromIntegral n))


-- | A summary doc carrying bare catalog paths, as the worker writes them.
seedSummary :: TestResources -> Projects.ProjectId -> IO ()
seedSummary tr p = runHasqlEffect tr $ SC.upsertSummary (V.singleton (p, doc))
  where
    fld cat = Catalog.FieldStruct mempty mempty cat False
    doc =
      Catalog.SummaryDoc
        { fields = HM.fromList [("http.request.method", fld Catalog.FCAttribute), ("db.operation.name", fld Catalog.FCAttribute), ("url.path", fld Catalog.FCAttribute), ("service.name", fld Catalog.FCResource)]
        , services = V.singleton "checkout-svc"
        , topValuesByField = HM.fromList [("http.request.method", Catalog.TopK 2 (HM.fromList [("GET", 5), ("POST", 3)])), ("db.operation.name", Catalog.TopK 1 (HM.singleton "SELECT" 7)), ("url.path", Catalog.TopK 1 (HM.singleton "/orders" 5)), ("service.name", Catalog.TopK 1 (HM.singleton "checkout-svc" 8))]
        }


-- | An in-memory record; callers set what their path reads.
otelRow :: Projects.ProjectId -> Text -> Map Text AE.Value -> Map Text AE.Value -> Telemetry.OtelLogsAndSpans
otelRow pid name attrs res =
  (Telemetry.mkSystemLog pid name Telemetry.SLInfo "" attrs (Just 1) frozenTime :: Telemetry.OtelLogsAndSpans)
    { Telemetry.id = UUID.toText UUID.nil
    , Telemetry.observed_timestamp = Nothing
    , Telemetry.level = Nothing
    , Telemetry.severity = Nothing
    , Telemetry.body = Nothing
    , Telemetry.context = Nothing
    , Telemetry.resource = if Map.null res then Nothing else Just (AesonText res)
    }


-- | A canonical HTTP span through the real 'extractObservation' walker; @logCols@ also fills
-- the log-level columns the top-level walk reads.
richObs :: Projects.ProjectId -> Text -> Bool -> Hot.ObservationInput
richObs p method logCols =
  ProcessMessage.extractObservation ProcessMessage.emptyPathClassifier
    (otelRow p (method <> " /orders") (Map.fromList attrs) (Map.singleton "service.name" (AE.String "checkout-svc")) :: Telemetry.OtelLogsAndSpans)
      { Telemetry.kind = Just "server"
      , Telemetry.status_code = Just "200"
      , Telemetry.level = "INFO" <$ guard logCols
      , Telemetry.severity = Telemetry.Severity{severity_text = Just Telemetry.SLInfo, severity_number = 9} <$ guard logCols
      }
  where
    attrs = [("http.request.method", AE.String method), ("db.operation.name", AE.String "SELECT")] <> if logCols then [] else [("http.response.status_code", AE.Number 200), ("url.path", AE.String "/orders")]


-- Chart cache fixtures, aligned to 'baseTime' rather than the test clock.
baseTime :: UTCTime
baseTime = read "2025-01-01 00:00:00 UTC"


timeAt :: Int -> Text
timeAt s = toText $ iso8601Show $ addUTCTime (fromIntegral s) baseTime


queryMetrics :: TestResources -> Text -> Text -> Text -> IO Charts.MetricsData
queryMetrics tr query from to = runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just testPid) (Just query) Nothing Nothing (Just from) (Just to) (Just "spans") Nothing []


clearCache :: TestResources -> IO ()
clearCache tr = dbExec tr [sql|DELETE FROM query_cache WHERE project_id = ?|] (Only testPid)


clearAllTestData :: TestResources -> IO ()
clearAllTestData tr = clearCache tr >> dbExec tr [sql|DELETE FROM otel_logs_and_spans WHERE project_id = ?|] (Only testPid)


-- | Datasets, headers and stats all agree.
sameResult :: Charts.MetricsData -> Charts.MetricsData -> Bool
sameResult a b = (a.rowsCount, a.headers, a.dataset) == (b.rowsCount, b.headers, b.dataset) && (stats <$> a.stats) == (stats <$> b.stats)
  where
    stats s = (s.min, s.max, s.sum, s.count, s.maxGroupSum)


seedMetrics :: UTCTime -> Charts.MetricsData
seedMetrics t = def{Charts.dataset = V.singleton (V.fromList [Just (realToFrac (utcTimeToPOSIXSeconds t)), Just 5]), Charts.headers = V.fromList ["timestamp", "count"], Charts.rowsCount = 1}


-- | A non-empty entry for @q@ over [t0, t1], keyed as a request over [t0, t2] builds it.
seedCache :: TestResources -> Text -> (UTCTime, UTCTime, UTCTime) -> IO QC.CacheKey
seedCache tr q (t0, t1, t2) = do
  sections <- either (fail . toString) pure (parseQueryToAST q)
  let key = QC.generateCacheKey testPid Nothing sections (defSqlQueryCfg testPid baseTime Nothing Nothing){dateRange = (Just t0, Just t2)}
  runQueryEffect tr $ QC.updateCache key (t0, t1) (seedMetrics t0) q
  pure key


cachedToFor :: TestResources -> QC.CacheKey -> IO (Maybe UTCTime)
cachedToFor tr key = fmap fromOnly . listToMaybe <$> dbQuery tr [sql|SELECT cached_to FROM query_cache WHERE project_id = ? AND query_hash = ? AND bin_interval = ? AND source = ?|] (key.projectId, key.queryHash, key.binInterval, key.source)


-- | The parser accepts the query while the database cannot run it; the table is restored even on failure.
withoutTelemetryTable :: TestResources -> IO a -> IO a
withoutTelemetryTable tr = E.bracket_ (rename "otel_logs_and_spans" "chart_test_unavailable") (rename "chart_test_unavailable" "otel_logs_and_spans")
  where
    rename a b = dbExec tr (fromString $ "ALTER TABLE " <> a <> " RENAME TO " <> b) ()


-- Live tail fixtures.
otherPid :: Projects.ProjectId
otherPid = UUIDId (UUID.fromWords 0 0 0 99)


ownerId, intruderId :: Projects.UserId
ownerId = Projects.UserId UUID.nil
intruderId = Projects.UserId (UUID.fromWords 0 0 0 98)


newSub :: Text -> LT.NewSubscription
newSub svc = LT.NewSubscription (Just svc) Nothing Nothing Nothing Nothing Nothing


bodyOf :: LT.LiveRow -> Text
bodyOf = \case
  LT.LogRow f -> f.body
  LT.TableRow _ -> "<table>"
  LT.Notice msg -> "notice: " <> msg


-- | Each subscription on its own connection of @depth@, one batch published, what each received.
delivered :: Natural -> [LT.Subscription] -> [Telemetry.OtelLogsAndSpans] -> IO [([LT.LiveRow], Int)]
delivered depth subs recs = do
  hub <- LT.newHub
  conns <- forM subs \s -> LT.newConn depth >>= \c -> c <$ LT.attachConn hub s.id c
  rt <- runtimeWith hub subs
  _ <- LT.publishMatches rt testPid (V.fromList recs)
  forM conns \c -> atomically (isEmptyTBQueue c.queue) >>= bool (LT.takeBatch c) (pure ([], 0))


fixture :: [LT.Subscription] -> IO (LT.Conn, LT.Runtime)
fixture subs = do
  hub <- LT.newHub
  conn <- LT.newConn 100
  forM_ subs \s -> LT.attachConn hub s.id conn
  (conn,) <$> runtimeWith hub subs


runtimeWith :: LT.Hub -> [LT.Subscription] -> IO LT.Runtime
runtimeWith hub subs = do
  cache <- LT.newSubCache
  _ <- LT.refreshSubCache cache frozenTime subs
  buf <- LT.newRelayBuffer
  pure LT.Runtime{transport = LT.PostgresRelay, cache, hub, relayBuffer = buf, emit = LT.deliver hub}


-- | The first @n@ SSE frames a stream body yields.
takeFrames :: Int -> SourceT IO Builder -> IO [Text]
takeFrames n (SourceT run) = run (go n)
  where
    go :: Int -> StepT IO Builder -> IO [Text]
    go k step = case step of
      _ | k <= 0 -> pure []
      Yield b next -> (decodeUtf8 (toLazyByteString b) :) <$> go (k - 1) next
      Effect m -> m >>= go k
      Skip next -> go k next
      _ -> pure []


mkSub :: Word32 -> Text -> Maybe Text -> Text -> LT.Subscription
mkSub n svc = mkSubScoped n (LT.LogTail (Just svc) LT.SKLogs) []


mkSubScoped :: Word32 -> LT.Scope -> [Text] -> Maybe Text -> Text -> LT.Subscription
mkSubScoped n scope columns environment query = LT.Subscription{id = UUIDId (UUID.fromWords 0 0 0 n), projectId = testPid, userId = ownerId, scope, environment, query, columns, expiresAt = addUTCTime 300 frozenTime}


-- | Resource keys nested the way the ingest path writes them, which the subject resolver walks.
logRecord :: Text -> Text -> Text -> Text -> Telemetry.OtelLogsAndSpans
logRecord svc env level body =
  (otelRow testPid "handler" mempty (Map.fromList [("service", AE.object ["name" AE..= svc]), ("deployment", AE.object ["environment" AE..= AE.object ["name" AE..= env]])]) :: Telemetry.OtelLogsAndSpans)
    { Telemetry.level = Just level
    , Telemetry.body = Just (AesonText (AE.String body))
    }


spanRecord :: Text -> Text -> Text -> Telemetry.OtelLogsAndSpans
spanRecord svc env level = (logRecord svc env level "" :: Telemetry.OtelLogsAndSpans){Telemetry.kind = Just "server", Telemetry.body = Nothing}


httpLogRecord :: Text -> Telemetry.OtelLogsAndSpans
httpLogRecord method = (logRecord "checkout" "prod" "info" "req" :: Telemetry.OtelLogsAndSpans){Telemetry.attributes = Just (AesonText (Map.singleton "http" (AE.object ["request" AE..= AE.object ["method" AE..= method]])))}


-- | 128 attributes at the per-field cap: each legal, their sum past what a broker accepts.
wideRecord :: Telemetry.OtelLogsAndSpans
wideRecord = (logRecord "checkout" "prod" "info" "wide" :: Telemetry.OtelLogsAndSpans){Telemetry.attributes = Just (AesonText (Map.fromList [("k" <> show n, AE.String (T.replicate LT.maxRowFieldChars "x")) | n <- [0 :: Int .. 127]]))}
