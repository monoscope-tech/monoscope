module Pages.LogExplorer.LogSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Clock (addUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Hasql.Interpolate qualified as HI
import Lucid qualified
import Models.Apis.LogQueries qualified as LogQueries
import Models.Apis.SchemaCatalog qualified as SchemaCatalog
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.GRPC.Common.Protobuf (Proto (..))
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.LogExplorer.Log qualified as Log
import Pages.LogExplorer.LogItem qualified as LogItem
import Pages.LogExplorer.QueryLibrary qualified as QueryLibrary
import Pages.Telemetry qualified as TelemetryPage
import Pkg.Components.LogQueryBox qualified as LogQueryBox
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Parser qualified as Parser
import Pkg.Parser.Stats (Sources (..))
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Pkg.TestUtils
import ProcessMessage (processMessages)
import ProcessMessage qualified
import Relude
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext (..), EnvConfig (..))
import Test.Hspec
import Utils qualified
import "base64" Data.Base64.Types qualified as B64T
import "base64" Data.ByteString.Base64 qualified as B64


-- Convert the data handler's cursor (last-row timestamp text) into the UTCTime
-- upper-bound the next page expects, mirroring nextUrl (-0.001s skips the
-- boundary row that would otherwise reappear). Shared by the pagination tests.
nextCursor :: Text -> Maybe UTCTime
nextCursor t = addUTCTime (-0.001) <$> iso8601ParseM (toString t)


-- The log-row payload now has its own endpoint (logExplorerDataH); the page
-- shell (apiLogH) renders only chrome. These helpers hit the data endpoint the
-- log-list web component actually fetches, and return the raw LogResult so the
-- assertions inspect the payload structurally, as before.
fetchData :: TestResources -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> IO Log.LogResult
fetchData tr = fetchDataIn tr testPid


-- As above, with the pagination direction the live-tail path uses. Defaulting to
-- PageOlder is what every existing caller relied on.
fetchDataDir :: TestResources -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Parser.PageDirection -> Maybe Text -> Maybe Text -> Maybe Text -> IO Log.LogResult
fetchDataDir tr = fetchDataDirIn tr testPid


-- Examples that assert absolute counts read a project of their own (see
-- 'createTestProject'): the shared 'testPid' accumulates rows from every other
-- example, shard and prior CI run in the real-TimeFusion topology.
fetchDataIn :: TestResources -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> IO Log.LogResult
fetchDataIn tr pid q cols cur since from to = fetchDataDirIn tr pid q cols cur Nothing since from to


fetchDataDirIn :: TestResources -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Parser.PageDirection -> Maybe Text -> Maybe Text -> Maybe Text -> IO Log.LogResult
fetchDataDirIn tr pid q cols cur dir since from to = snd <$> testServant tr (Log.logExplorerDataH pid q cols cur dir since from to Nothing Nothing)


-- | Re-run @act@ until @ok@ holds, then return the last result (which the caller
-- asserts on, so a persistent failure still reports the real value). TimeFusion is
-- an asynchronous store: a row is durable when the write returns but not
-- necessarily readable in the same instant, so a single read is a race.
eventually :: IO a -> (a -> Bool) -> IO a
eventually act ok = go (40 :: Int)
  where
    go n = do
      a <- act
      if ok a || n <= 0 then pure a else threadDelay 250_000 >> go (n - 1)


-- | A legacy SDK message re-pointed at @pid@ and given its own @msg_id@.
--
-- Both matter under TimeFusion. The project id keeps the rows out of the shared
-- 'testPid'; the msg id makes the event distinct — 'ProcessMessage.requestEventIds'
-- derives the span/trace id from the whole message when @msg_id@ is absent, so N
-- byte-identical copies are ONE event. Postgres stores each copy as a row anyway
-- and TimeFusion deduplicates by id, which is why "ingest 100 copies, expect 100
-- rows" only ever held on Postgres.
freshMsg :: Projects.ProjectId -> AE.Value -> IO ByteString
freshMsg pid v = do
  mid <- nextRandom
  let UUIDId pidU = pid
  pure $ toStrict $ AE.encode (Unsafe.fromJust (convert v)){ProcessMessage.projectId = pidU, ProcessMessage.msgId = Just mid}


seedFacetSummary :: TestResources -> IO ()
seedFacetSummary tr = do
  let field category = Catalog.FieldStruct mempty mempty category False
      doc =
        Catalog.SummaryDoc
          { fields =
              HashMap.fromList
                [ ("service.name", field Catalog.FCResource)
                , ("url.path", field Catalog.FCAttribute)
                ]
          , services = V.singleton "checkout-svc"
          , topValuesByField =
              HashMap.fromList
                [ ("service.name", Catalog.TopK 1 (HashMap.singleton "checkout-svc" 8))
                , ("url.path", Catalog.TopK 1 (HashMap.singleton "/orders" 5))
                ]
          }
  runHasqlEffect tr $ SchemaCatalog.upsertSummary (V.singleton (testPid, doc))


spec :: Spec
spec = around withTestResources do
  describe "Log data endpoint (logExplorerDataH)" do
    it "should return an empty list" \tr -> do
      pid <- createTestProject tr "log-explorer-empty"
      r <- fetchDataIn tr pid Nothing Nothing Nothing Nothing Nothing Nothing
      V.length r.logsData `shouldBe` 0
      r.count `shouldBe` 0
      r.cols `shouldBe` ["id", "timestamp", "service", "summary", "latency_breakdown"]

    it "should return log items" \tr -> do
      pid <- createTestProject tr "log-explorer-items"
      let yesterdayTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-86400) frozenTime
      let twoDaysAgoTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-172800) frozenTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      msgs <-
        forM (concat (replicate 100 [testRequestMsgs.reqMsg1 nowTxt, testRequestMsgs.reqMsg2 nowTxt]) ++ [testRequestMsgs.reqMsg1 yesterdayTxt, testRequestMsgs.reqMsg2 twoDaysAgoTxt])
          $ \v -> ("m",) <$> freshMsg pid v
      res <- runTestBackground frozenTime tr.trATCtx $ processMessages msgs HashMap.empty
      bimap (show @Text) (length . fst) res `shouldBe` Right 202

      let threeDaysAgo = addUTCTime (-259200) frozenTime
      let oneDayFuture = addUTCTime 86400 frozenTime
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" threeDaysAgo
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" oneDayFuture

      r <- eventually (fetchDataIn tr pid Nothing Nothing Nothing Nothing fromTime toTime) ((>= 202) . V.length . (.logsData))
      V.length r.logsData `shouldBe` 202
      r.count `shouldSatisfy` (>= 202)
      r.cols `shouldBe` ["id", "timestamp", "service", "summary", "latency_breakdown"]
      -- URLs are stamped by the data endpoint and consumed by the web component.
      r.nextUrl `shouldNotBe` ""
      r.resetLogsUrl `shouldNotBe` ""
      r.recentUrl `shouldNotBe` ""
      -- The stamped URLs must target the data endpoint, not the page shell.
      r.nextUrl `shouldSatisfy` T.isInfixOf "/log_explorer/data"

    -- `deployment.environment.name` is promoted to a column (migration 0122) precisely so
    -- the app-wide selector can scope every query by it. The selection rides on the session
    -- (read from the `env` cookie at auth time), so this drives it the way the picker does.
    it "scopes results to the selected deployment environment, and to all of them by default" \tr -> do
      pid <- createTestProject tr "log-explorer-env-scope"
      apiKey <- createTestAPIKey tr pid "env-scope-key"
      let hexId = T.replace "-" "" . UUID.toText <$> nextRandom
          ingestIn env name = do
            (trId, sid) <- (,) <$> hexId <*> hexId
            ingestSpanReq tr $ mkSpanRequest trId sid Nothing name [] Nothing [] (mkResource apiKey [mkAttr "deployment.environment.name" env]) frozenTime
      ingestIn "prod" "GET /env/prod"
      ingestIn "staging" "GET /env/staging"

      let range = (Just (addUTCTime (-60) frozenTime), Just (addUTCTime 60 frozenTime))
          namesFor envM = do
            res <- runQueryEffect tr $ LogQueries.selectLogTable tr.trATCtx.env.enableTimefusionReads pid [] "" Nothing range [] (Just SSpans) Nothing envM
            (rows, cols, _) <- either (\e -> error ("selectLogTable failed: " <> e)) pure res
            let idx = fromMaybe (error "span_name not projected") $ V.elemIndex "span_name" (V.fromList cols)
            pure $ sort [n | r <- V.toList rows, Just (AE.String n) <- [r V.!? idx], "GET /env/" `T.isPrefixOf` n]

      eventually (namesFor Nothing) ((== 2) . length) `shouldReturn` ["GET /env/prod", "GET /env/staging"]
      namesFor (Just "prod") `shouldReturn` ["GET /env/prod"]
      namesFor (Just "staging") `shouldReturn` ["GET /env/staging"]
      -- An environment nothing reports is empty, not unfiltered — the difference between
      -- "no data here" and "here is everything" is the whole point of the control.
      namesFor (Just "does-not-exist") `shouldReturn` []

    it "should handle query filters correctly" \tr -> do
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt

      let msgs = [("m1", toStrict $ AE.encode reqMsg1), ("m2", toStrict $ AE.encode reqMsg2)]
      res <- runTestBackground frozenTime tr.trATCtx $ processMessages msgs HashMap.empty
      bimap (show @Text) (length . fst) res `shouldBe` Right 2

      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime

      let query = "status_code == \"200\""
      r <- fetchData tr (Just query) Nothing Nothing Nothing fromTime toTime
      r.count `shouldSatisfy` (> 0)
      V.length r.logsData `shouldSatisfy` (> 0)

    it "opens a trace-id deep link on the matching explorer events" \tr -> do
      apiKey <- createTestAPIKey tr testPid "trace-link-key"
      let hexId = T.replace "-" "" . UUID.toText <$> nextRandom
      (linkedTraceId, linkedSpanId, otherTraceId, otherSpanId) <- (,,,) <$> hexId <*> hexId <*> hexId <*> hexId
      ingestSpanReq tr $ mkSpanRequest linkedTraceId linkedSpanId Nothing "GET /linked-checkout" [] Nothing [] (mkResource apiKey []) frozenTime
      ingestSpanReq tr $ mkSpanRequest otherTraceId otherSpanId Nothing "GET /unrelated" [] Nothing [] (mkResource apiKey []) frozenTime

      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
          query = "trace_id == \"" <> linkedTraceId <> "\""
      r <- fetchData tr (Just query) (Just "id,timestamp,name,context___trace_id") Nothing Nothing fromTime toTime

      r.count `shouldBe` 1
      V.length r.logsData `shouldBe` 1

    it "should return the requested extra columns" \tr -> do
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      let reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      _ <- runTestBackground frozenTime tr.trATCtx $ processMessages [("m1", toStrict $ AE.encode reqMsg)] HashMap.empty
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      r <- fetchData tr Nothing (Just "id,timestamp,name,duration") Nothing Nothing fromTime toTime
      r.cols `shouldBe` ["id", "timestamp", "name", "duration", "service", "summary", "latency_breakdown"]

    -- 2026-09-01: every browser span carried a Replay button, because the
    -- `session;` summary tag is stamped at ingest — before a recording can exist,
    -- and for every SDK that sets session.id whether it records or not. Clicking it
    -- opened an empty player. The affordance has to follow projects.replay_sessions,
    -- which is only knowable at read time.
    it "offers Replay on a session row only once that session has a recording" \tr -> do
      pid <- createTestProject tr "log-explorer-replay-gate"
      apiKey <- createTestAPIKey tr pid "replay-gate-key"
      sessionId <- nextRandom
      let hexId = T.replace "-" "" . UUID.toText <$> nextRandom
          spanName = "GET /replay-gate"
      (trId, spanId) <- (,) <$> hexId <*> hexId
      ingestSpanReq tr
        $ mkSpanRequest trId spanId Nothing spanName [] Nothing [mkAttr "session.id" (UUID.toText sessionId)] (mkResource apiKey []) frozenTime

      let range = (Just (addUTCTime (-60) frozenTime), Just (addUTCTime 60 frozenTime))
          -- (rows returned, session tags among their summaries)
          summaries = do
            res <- runQueryEffect tr $ LogQueries.selectLogTable tr.trATCtx.env.enableTimefusionReads pid [] "" Nothing range [] (Just SSpans) Nothing Nothing
            (rows, cols, _) <- either (\e -> error ("selectLogTable failed: " <> e)) pure res
            -- Fails loudly rather than silently passing if the projection ever stops
            -- carrying a bare `summary` column — the gate keys off exactly that name.
            let idx = fromMaybe (error "summary not projected") $ V.elemIndex "summary" (V.fromList cols)
            pure (length rows, [t | r <- V.toList rows, Just (AE.Array els) <- [r V.!? idx], AE.String t <- V.toList els, "session;" `T.isPrefixOf` t])

      -- The span is there and carries session.id; the tag is not, because nothing
      -- was recorded. Waiting on the row count first keeps "no tag yet" from
      -- passing simply because the write had not landed.
      eventually summaries ((> 0) . fst) `shouldReturn` (1, [])

      void $ withPool tr.trPool $ DBT.execute [sql| INSERT INTO projects.replay_sessions (session_id, project_id) VALUES (?, ?) |] (sessionId, pid)
      (_, tags) <- summaries
      tags `shouldSatisfy` any (T.isSuffixOf (UUID.toText sessionId))

    it "returns only matched spans plus their descendants, not unrelated siblings" \tr -> do
      apiKey <- createTestAPIKey tr testPid "log-spec-tree-key"
      let ts = addUTCTime (-30) frozenTime
      trId <- show <$> nextRandom
      spanA <- show <$> nextRandom
      spanB <- show <$> nextRandom
      spanC <- show <$> nextRandom
      spanE <- show <$> nextRandom
      spanD <- show <$> nextRandom
      marker <- ("ui-" <>) . UUID.toText <$> nextRandom
      let resource = mkResource apiKey []
          ingest sid pidM name extras =
            void
              $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider
              $ Proto (mkSpanRequest trId sid pidM name [] Nothing extras resource ts)
      ingest spanA Nothing "ui.tree.root" [mkAttr "ui.marker" marker]
      ingest spanB (Just spanA) "ui.tree.child" []
      ingest spanC (Just spanB) "ui.tree.grandchild" []
      ingest spanE Nothing "ui.tree.unrelated.root" []
      ingest spanD (Just spanE) "ui.tree.unrelated.child" []

      ingestedRows <-
        withPool tr.trPool
          $ DBT.query
            [sql| SELECT name FROM otel_logs_and_spans WHERE project_id = ? AND name LIKE 'ui.tree.%' ORDER BY name |]
            (Only testPid)
          :: IO (V.Vector (Only Text))
      V.toList (fmap (\(Only n) -> n) ingestedRows)
        `shouldBe` ["ui.tree.child", "ui.tree.grandchild", "ui.tree.root", "ui.tree.unrelated.child", "ui.tree.unrelated.root"]

      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) ts
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 ts
          q = "attributes.ui.marker == \"" <> marker <> "\""

      r <- fetchData tr (Just q) Nothing Nothing Nothing fromTime toTime
      -- A (matched) + B + C (descendants). E and D share trace_id but are not
      -- in A's subtree — must not bleed in.
      V.length r.logsData `shouldBe` 3
      r.count `shouldBe` 1

  -- The query editor underlines exactly what this returns, so the positions are
  -- part of the contract, not a detail.
  describe "Query validation endpoint" do
    it "accepts a valid query" \tr -> do
      v <- snd <$> testServant tr (Log.logExplorerValidateH testPid (Just "kind == \"log\"") Nothing)
      (v.valid, v.message) `shouldBe` (True, Nothing)

    it "locates an unknown field so the editor can underline it" \tr -> do
      v <- snd <$> testServant tr (Log.logExplorerValidateH testPid (Just "kind == \"a\" and attribut contains \"x\"") Nothing)
      v.valid `shouldBe` False
      v.message `shouldBe` Just "Unknown field \"attribut\". Did you mean \"attributes\"?"
      (v.column, v.width) `shouldBe` (Just 17, Just 8)

    it "locates a syntax error" \tr -> do
      v <- snd <$> testServant tr (Log.logExplorerValidateH testPid (Just "attributes contains ddd") Nothing)
      v.valid `shouldBe` False
      v.column `shouldBe` Just 12

    -- The editor underlines whatever this says, so a verdict reached under the wrong table
    -- is a squiggle on a query the server runs happily. `metric_name` is a real column on
    -- the metrics table and no column at all on otel_logs_and_spans.
    it "judges a metrics query against the metrics table, not the spans one" \tr -> do
      let q = Just "metric_name == \"redis.commands\" | summarize avg(value) by bin_auto(timestamp)"
      metrics <- snd <$> testServant tr (Log.logExplorerValidateH testPid q (Just "metrics"))
      (metrics.valid, metrics.message) `shouldBe` (True, Nothing)
      spans <- snd <$> testServant tr (Log.logExplorerValidateH testPid q Nothing)
      spans.valid `shouldBe` False

    -- 92 saved queries in production filter on the raw `___` column names. They
    -- are what the table calls those columns and they reach SQL unchanged, so
    -- rejecting them would have broken working saved queries.
    it "accepts the raw ___ column names saved queries use" \tr -> do
      v <- snd <$> testServant tr (Log.logExplorerValidateH testPid (Just "context___trace_id != null and resource___service___name == \"api\"") Nothing)
      (v.valid, v.message) `shouldBe` (True, Nothing)

    it "still rejects a mistyped ___ column, and suggests in the same notation" \tr -> do
      v <- snd <$> testServant tr (Log.logExplorerValidateH testPid (Just "context___trace_ix != null") Nothing)
      v.valid `shouldBe` False
      v.message `shouldSatisfy` maybe False (T.isInfixOf "context___trace_id")

    it "treats an empty query as valid" \tr -> do
      v <- snd <$> testServant tr (Log.logExplorerValidateH testPid Nothing Nothing)
      v.valid `shouldBe` True

    -- Aliases a query introduces are real names downstream; flagging them would
    -- squiggle a working query.
    it "accepts a filter on a summarize alias" \tr -> do
      v <- snd <$> testServant tr (Log.logExplorerValidateH testPid (Just "level == \"ERROR\" | summarize count() by kind | where count_ > 1") Nothing)
      v.valid `shouldBe` True

  describe "Data endpoint query-error handling" do
    it "returns an empty result for invalid query syntax (not a crash)" \tr -> do
      r <- fetchData tr (Just "status_code = 200") Nothing Nothing Nothing Nothing Nothing
      V.length r.logsData `shouldBe` 0
      r.count `shouldBe` 0

    it "returns an empty result for malformed operators" \tr -> do
      r <- fetchData tr (Just "status_code === \"200\"") Nothing Nothing Nothing Nothing Nothing
      V.length r.logsData `shouldBe` 0
      r.count `shouldBe` 0

    -- 2026-08-06: `attribute contains "…"` (singular) reached the database as a
    -- column and came back as a raw "Schema error: No field named attribute"
    -- logged at ATTENTION. It must be rejected before any SQL is built.
    it "names the unknown field instead of shipping it to the database" \tr -> do
      r <- fetchData tr (Just "attribute contains \"Red Flashlight\"") Nothing Nothing Nothing Nothing Nothing
      r.error `shouldBe` Just "Unknown field \"attribute\". Did you mean \"attributes\"?"
      V.length r.logsData `shouldBe` 0

  -- A summarize query aimed at the rows endpoint used to reach
  -- synthesizeOrphanHeaders, which writes log-table column indices into a row
  -- the width of the aggregate result: `index out of bounds (7,1)`, unhandled,
  -- connection dropped. The UI sends these to the chart endpoint, but the API,
  -- the CLI and a hand-typed URL all land here.
  describe "Aggregate query on the rows endpoint" do
    it "does not crash on a summarize query" \tr -> do
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      let reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      _ <- runTestBackground frozenTime tr.trATCtx $ processMessages [("m1", toStrict $ AE.encode reqMsg)] HashMap.empty
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      r <- fetchData tr (Just "kind == \"server\" | summarize count(*) by kind") Nothing Nothing Nothing fromTime toTime
      -- Whatever it returns, it must return: no exception escaping the handler.
      r.queryResultCount `shouldSatisfy` (>= 0)

    it "does not crash on a summarize binned by time" \tr -> do
      r <- fetchData tr (Just "summarize count(*) by bin_auto(timestamp)") Nothing Nothing Nothing Nothing Nothing
      r.queryResultCount `shouldSatisfy` (>= 0)

    -- The exact shape that threw in production: log-table column indices over a
    -- one-column aggregate row. `V.//` at index 7 of a 1-wide vector is what
    -- `index out of bounds (7,1)` was.
    it "leaves aggregate-shaped rows alone instead of indexing past them" \_ -> do
      let colIdxMap = Utils.listToIndexHashMap ["id", "timestamp", "trace_id", "span_name", "duration", "service", "parent_id", "start_time_ns", "errors", "summary", "latency_breakdown", "kind"]
          narrow = V.singleton (V.singleton (AE.Number 42))
      Log.colsFitRows colIdxMap narrow `shouldBe` False
      Log.buildTraceTree colIdxMap 1 narrow `shouldBe` (narrow, [])
      Log.synthesizeOrphanHeaders colIdxMap narrow `shouldBe` V.empty

  -- `| where` after a `| summarize` used to be hoisted above the aggregation, so
  -- the SQL filtered on an alias the database had not produced yet and the query
  -- failed outright. It belongs in HAVING.
  describe "Filtering after a summarize" do
    it "runs instead of failing on the alias" \tr -> do
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
          reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      _ <- runTestBackground frozenTime tr.trATCtx $ processMessages (replicate 3 ("m1", toStrict $ AE.encode reqMsg)) HashMap.empty
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      r <- fetchData tr (Just "summarize count() by kind | where count_ > 1") Nothing Nothing Nothing fromTime toTime
      r.error `shouldBe` Nothing

    it "still rejects a filter on a name nothing defines" \tr -> do
      r <- fetchData tr (Just "summarize count() by kind | where nosuchalias > 1") Nothing Nothing Nothing Nothing Nothing
      r.error `shouldSatisfy` maybe False (T.isInfixOf "Unknown field")

  describe "Pagination" do
    it "cursor paginates correctly across multiple pages without overlap" \tr -> do
      apiKey <- createTestAPIKey tr testPid "pagination-key"
      pageMarker <- ("pg-" <>) . UUID.toText <$> nextRandom
      let resource = mkResource apiKey []
          rowAt :: Int -> IO ()
          rowAt i = do
            sid <- show <$> nextRandom
            tid <- show <$> nextRandom
            let ts = addUTCTime (fromIntegral (negate i)) frozenTime
                attrs = [mkAttr "page.marker" pageMarker, mkAttr "page.idx" (show i)]
            void
              $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider
              $ Proto (mkSpanRequest tid sid Nothing ("pg.row." <> show i) [] Nothing attrs resource ts)
      mapM_ rowAt [1 .. 5 :: Int]

      ingested <-
        withPool tr.trPool
          $ DBT.query
            [sql| SELECT count(*)::int FROM otel_logs_and_spans WHERE project_id = ? AND attributes->'page'->>'marker' = ? |]
            (testPid, pageMarker)
          :: IO (V.Vector (Only Int))
      V.toList (fmap (\(Only n) -> n) ingested) `shouldBe` [5]

      let q = Just $ "attributes.page.marker == \"" <> pageMarker <> "\" | limit 2"
          fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
          pageIdsFor :: V.Vector (V.Vector AE.Value) -> HashMap.HashMap Text Int -> IO [Int]
          pageIdsFor rows colIdxMap = do
            let spanIdAt r =
                  HashMap.lookup "latency_breakdown" colIdxMap >>= (r V.!?) >>= \case
                    AE.String t -> Just t
                    _ -> Nothing
                spanIds = V.toList $ V.mapMaybe spanIdAt rows
            length spanIds `shouldBe` V.length rows
            attrs <-
              withPool tr.trPool
                $ DBT.query
                  [sql| SELECT attributes->'page'->>'idx' FROM otel_logs_and_spans
                    WHERE project_id = ? AND context___span_id = ANY(?) ORDER BY timestamp DESC |]
                  (testPid, PGArray spanIds)
                :: IO (V.Vector (Only (Maybe Text)))
            pure $ V.toList $ V.mapMaybe (\(Only m) -> m >>= readMaybe . toString) attrs

          fetchPage cursor = fetchData tr q Nothing cursor Nothing fromTime toTime

      r1 <- fetchPage Nothing
      page1Ids <- pageIdsFor r1.logsData r1.colIdxMap
      page1Ids `shouldBe` [1, 2]
      r1.hasMore `shouldBe` True

      r2 <- fetchPage (r1.cursor >>= nextCursor)
      page2Ids <- pageIdsFor r2.logsData r2.colIdxMap
      page2Ids `shouldBe` [3, 4]
      r2.hasMore `shouldBe` True
      r2.cursor `shouldSatisfy` (/= r1.cursor)

      r3 <- fetchPage (r2.cursor >>= nextCursor)
      page3Ids <- pageIdsFor r3.logsData r3.colIdxMap
      page3Ids `shouldBe` [5]
      r3.hasMore `shouldBe` False

    -- The other half of pagination: live tail and "Load newer events" page with
    -- direction=newer, and nothing exercised it server-side. The client builds that
    -- cursor from its newest retained row, so if the server answered with the newest N
    -- rows instead of the N adjacent to the cursor, a burst of traffic would leave a
    -- permanent hole in the middle of the list that no amount of scrolling recovers.
    it "paging newer returns the rows adjacent to the cursor, newest-first, without a gap" \tr -> do
      apiKey <- createTestAPIKey tr testPid "newer-pagination-key"
      marker <- ("nw-" <>) . UUID.toText <$> nextRandom
      let resource = mkResource apiKey []
          -- idx 1 is the oldest, idx 6 the newest.
          rowAt :: Int -> IO ()
          rowAt i = do
            sid <- show <$> nextRandom
            tid <- show <$> nextRandom
            let ts = addUTCTime (fromIntegral (negate (10 - i))) frozenTime
                attrs = [mkAttr "nw.marker" marker, mkAttr "nw.idx" (show i)]
            void
              $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider
              $ Proto (mkSpanRequest tid sid Nothing ("nw.row." <> show i) [] Nothing attrs resource ts)
      mapM_ rowAt [1 .. 6 :: Int]

      let idxOf :: V.Vector (V.Vector AE.Value) -> HashMap.HashMap Text Int -> IO [Int]
          idxOf rows colIdxMap = do
            let spanIdAt r =
                  HashMap.lookup "latency_breakdown" colIdxMap >>= (r V.!?) >>= \case
                    AE.String t -> Just t
                    _ -> Nothing
                spanIds = V.toList $ V.mapMaybe spanIdAt rows
            -- Preserve the response's own row order: the display contract is what is
            -- under test, so re-sorting here would hide exactly the bug we are after.
            attrs <- forM spanIds \sid ->
              withPool tr.trPool
                $ DBT.query
                  [sql| SELECT attributes->'nw'->>'idx' FROM otel_logs_and_spans WHERE project_id = ? AND context___span_id = ? |]
                  (testPid, sid)
                :: IO (V.Vector (Only (Maybe Text)))
            pure $ mapMaybe (\v -> V.headM v >>= \(Only m) -> m >>= readMaybe . toString) attrs

          q = Just $ "attributes.nw.marker == \"" <> marker <> "\" | limit 2"
          fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
          -- The client's cursor: the newest row it already holds.
          cursorAt i = Just $ addUTCTime (fromIntegral (negate (10 - i))) frozenTime

      -- Cursor at row 3 (inclusive: the filter is timestamp >= cursor), page size 2.
      -- The answer must be the two rows adjacent to the cursor — 3 and 4 — NOT the newest
      -- two (5 and 6), which would strand 3 and 4 in a hole no scrolling can reach.
      newer <- fetchDataDir tr q Nothing (cursorAt 3) (Just Parser.PageNewer) Nothing fromTime toTime
      adjacentIdx <- idxOf newer.logsData newer.colIdxMap
      adjacentIdx `shouldBe` [4, 3]

      -- Newest-first is the canonical display order regardless of the scan direction.
      adjacentIdx `shouldBe` sortOn Down adjacentIdx

      -- Every row it returns really is at or newer than the cursor.
      all (>= 3) adjacentIdx `shouldBe` True

    it "paging newer from the newest row returns nothing, so the client can close that edge" \tr -> do
      apiKey <- createTestAPIKey tr testPid "newer-edge-key"
      marker <- ("nwe-" <>) . UUID.toText <$> nextRandom
      let resource = mkResource apiKey []
      for_ ([1 .. 3] :: [Int]) \i -> do
        sid <- show <$> nextRandom
        tid <- show <$> nextRandom
        let ts = addUTCTime (fromIntegral (negate (10 - i))) frozenTime
            attrs = [mkAttr "nwe.marker" marker, mkAttr "nwe.idx" (show i)]
        void
          $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider
          $ Proto (mkSpanRequest tid sid Nothing ("nwe.row." <> show i) [] Nothing attrs resource ts)

      let q = Just $ "attributes.nwe.marker == \"" <> marker <> "\""
          fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
          -- One millisecond past the newest row, which is what buildRecentFetchUrl sends.
          pastNewest = Just $ addUTCTime 0.001 $ addUTCTime (fromIntegral (negate (10 - 3 :: Int))) frozenTime

      atEdge <- fetchDataDir tr q Nothing pastNewest (Just Parser.PageNewer) Nothing fromTime toTime
      V.length atEdge.logsData `shouldBe` 0

    -- Regression: synthesized "Upstream span missing" orphan-header rows are a
    -- DISPLAY augmentation and must not count toward the page-fill check.
    it "load-more is robust to orphan headers: full pages keep hasMore, drain visits every row once" \tr -> do
      apiKey <- createTestAPIKey tr testPid "orphan-pagination-key"
      marker <- ("orph-" <>) . UUID.toText <$> nextRandom
      orphTid <- UUID.toText <$> nextRandom
      ghostParent <- UUID.toText <$> nextRandom
      let extras = [("page.marker", marker)]
          tsAt i = addUTCTime (fromIntegral (negate (i :: Int))) frozenTime
      orphanA <- UUID.toText <$> nextRandom
      orphanB <- UUID.toText <$> nextRandom
      ingestSpanLinked tr apiKey orphTid orphanA (Just ghostParent) "orph.child.a" extras (tsAt 1)
      ingestSpanLinked tr apiKey orphTid orphanB (Just ghostParent) "orph.child.b" extras (tsAt 2)
      forM_ [3 .. 5] \i -> do
        rootTid <- show <$> nextRandom
        rootSid <- UUID.toText <$> nextRandom
        ingestSpanLinked tr apiKey rootTid rootSid Nothing ("root." <> show i) extras (tsAt i)

      let q = Just $ "attributes.page.marker == \"" <> marker <> "\" | limit 2"
          fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
          colText col r row = HashMap.lookup col r.colIdxMap >>= (row V.!?) >>= \case AE.String t -> Just t; _ -> Nothing
          fetchPage cur = fetchData tr q Nothing cur Nothing fromTime toTime
          isSynth r row = maybe False ("Upstream span missing" `T.isInfixOf`) (colText "span_name" r row)
          realIds r = V.toList $ V.mapMaybe (\row -> if isSynth r row then Nothing else colText "latency_breakdown" r row) r.logsData
          hasSynthHeader r = V.any (isSynth r) r.logsData

      page1 <- fetchPage Nothing
      hasSynthHeader page1 `shouldBe` True
      page1.hasMore `shouldBe` True

      let drain cur acc pages = do
            r <- fetchPage cur
            let acc' = acc <> realIds r
            if r.hasMore then drain (r.cursor >>= nextCursor) acc' (pages + 1) else pure (acc', pages + 1, r.hasMore)
      (seen, pageCount, lastHasMore) <- drain (page1.cursor >>= nextCursor) (realIds page1) 1
      lastHasMore `shouldBe` False
      pageCount `shouldBe` 3
      length seen `shouldBe` 5
      length (ordNub seen) `shouldBe` 5

    -- v1/CLI events API (queryEvents): --with-children must not silently truncate.
    it "events API with-children pagination visits every matched root (no silent truncation)" \tr -> do
      apiKey <- createTestAPIKey tr testPid "events-children-key"
      marker <- ("evc-" <>) . UUID.toText <$> nextRandom
      let tsAt i = addUTCTime (fromIntegral (negate (i :: Int))) frozenTime
      forM_ [1 .. 3 :: Int] \i -> do
        rootTid <- UUID.toText <$> nextRandom
        rootSid <- UUID.toText <$> nextRandom
        ingestSpanLinked tr apiKey rootTid rootSid Nothing ("evroot." <> show i) [("page.marker", marker)] (tsAt (i * 10))
        forM_ [1 .. 2 :: Int] \_ -> do
          childSid <- UUID.toText <$> nextRandom
          ingestSpanLinked tr apiKey rootTid childSid (Just rootSid) "evchild" [] (addUTCTime 0.1 (tsAt (i * 10)))

      let q = Just $ "attributes.page.marker == \"" <> marker <> "\""
          fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-120) frozenTime
          spanNameOf r row = HashMap.lookup "span_name" r.colIdxMap >>= (row V.!?) >>= \case AE.String t -> Just t; _ -> Nothing
          rootsOn r = [t | row <- V.toList r.logsData, Just t <- [spanNameOf r row], "evroot." `T.isPrefixOf` t]
          runPage toM = runAsBase tr (Log.queryEvents testPid q Nothing fromTime toM Nothing (Just 2) (Just True) Nothing)
          nextTo r = toText . iso8601Show <$> (r.cursor >>= nextCursor)
          drain toM acc pages = do
            r <- runPage toM
            let acc' = acc <> rootsOn r
            if r.hasMore then drain (nextTo r) acc' (pages + 1) else pure (acc', pages + 1)
      (seenRoots, pageCount) <- drain Nothing [] 0
      ordNub seenRoots `shouldMatchList` ["evroot.1", "evroot.2", "evroot.3"]
      pageCount `shouldSatisfy` (> 1)

    it "caps a page at 500 rows and reports hasMore" \tr -> do
      -- Seeded through the ingest path, not a raw INSERT on tr.trPool: that writes
      -- Postgres only, so against a TimeFusion-backed read the page had nothing of
      -- this test's to cap. One merged export keeps it a single dual-write.
      pid <- createTestProject tr "log-explorer-page-cap"
      apiKey <- createTestAPIKey tr pid "page-cap-key"
      let resource = mkResource apiKey []
      let hexId = T.replace "-" "" . UUID.toText <$> nextRandom
      reqs <- forM [1 .. 520 :: Int] \i -> do
        (tid, sid) <- (,) <$> hexId <*> hexId
        pure $ mkSpanRequest tid sid Nothing "seed.row" [] Nothing [] resource (addUTCTime (fromIntegral i * (-0.05)) frozenTime)
      ingestSpanReq tr $ mergeSpanRequests reqs

      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime

      r <- eventually (fetchDataIn tr pid Nothing Nothing Nothing Nothing fromTime toTime) ((>= 500) . V.length . (.logsData))
      V.length r.logsData `shouldBe` 500
      r.hasMore `shouldBe` True
      let lastItemM = r.logsData V.!? (V.length r.logsData - 1)
      case lastItemM of
        Just lastItem -> case HashMap.lookup "timestamp" r.colIdxMap of
          Just idx -> ((lastItem V.!? idx) >>= \case AE.String t -> Just t; _ -> Nothing) `shouldSatisfy` isJust
          _ -> error "timestamp column not found"
        Nothing -> error "No items in first page"

  describe "Time Range Selection" do
    it "should respect exact time boundaries" \tr -> do
      pid <- createTestProject tr "log-explorer-boundaries"
      let at t = toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime t frozenTime
      msgs <- forM [-3600, -7200, -10800] \t -> ("m",) <$> freshMsg pid (testRequestMsgs.reqMsg1 (at t))
      res <- runTestBackground frozenTime tr.trATCtx $ processMessages msgs HashMap.empty
      bimap (show @Text) (length . fst) res `shouldBe` Right 3

      let fromTime = Just $ at (-9000)
      let toTime = Just $ at (-5400)

      -- Regression guard for a real TimeFusion bug, fixed in timefusion bf3abc8: a mem
      -- bucket advertised its ROUTING timestamp instead of the range of rows it held, so
      -- a batch spanning 21:00-23:00 reported max=21:00 and any `timestamp >= 21:30`
      -- pruned the whole bucket. The rows were in memory, matched the predicate, and were
      -- invisible until flush. This example is the only one that catches it, because it
      -- ingests several rows in one batch and then reads a window ABOVE the earliest.
      --
      -- It was briefly quarantined as "not a product bug" on the strength of prod windows
      -- 2-3h and 25-26h back returning rows — but those read FLUSHED parquet, which the
      -- bug never touched. Only unflushed rows were hidden, i.e. the newest data.

      r <- eventually (fetchDataIn tr pid Nothing Nothing Nothing Nothing fromTime toTime) ((>= 1) . (.count))
      r.count `shouldSatisfy` (>= 1)
      V.length r.logsData `shouldSatisfy` (>= 1)

    it "should handle 'since' parameter correctly" \tr -> do
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      let oneHourBefore = toText $ formatTime defaultTimeLocale "%FT%T%QZ" (addUTCTime (-3600) frozenTime)
      let twoDaysBefore = toText $ formatTime defaultTimeLocale "%FT%T%QZ" (addUTCTime (-172800) frozenTime)
      let msgNow = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let msgHourBeforeMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 oneHourBefore
      let msgTwoDaysBeforeMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 twoDaysBefore
      let msgs = [("m1", toStrict $ AE.encode msgNow), ("m2", toStrict $ AE.encode msgHourBeforeMsg), ("m3", toStrict $ AE.encode msgTwoDaysBeforeMsg)]
      _ <- runTestBackground frozenTime tr.trATCtx $ processMessages msgs HashMap.empty

      r1 <- fetchData tr Nothing Nothing Nothing (Just "1H") Nothing Nothing
      r1.count `shouldSatisfy` (>= 2)
      V.length r1.logsData `shouldSatisfy` (>= 2)

      r2 <- fetchData tr Nothing Nothing Nothing (Just "24H") Nothing Nothing
      r2.count `shouldSatisfy` (>= 2)
      V.length r2.logsData `shouldSatisfy` (>= 2)

    it "should handle missing time range (default behavior)" \tr -> do
      r <- fetchData tr Nothing Nothing Nothing Nothing Nothing Nothing
      r.cols `shouldBe` ["id", "timestamp", "service", "summary", "latency_breakdown"]

  -- The client half of last-click-wins lives in web-components (log-detail-panel.test.ts);
  -- this is the server half. htmx defaults to `hx-sync: queue first`, which DROPS a detail
  -- request issued while another is in flight: the newly clicked row never loads and the
  -- overlay indicator, added on click, is never cleared — a frozen three-dot loader until the
  -- user reloads. Deleting the attribute reintroduces that with every JS test still green.
  describe "Detail panel request sync" do
    it "logExplorerH_detailsContainer_replacesTheInFlightRequest" \tr -> do
      (_, page) <- testServant tr $ Log.apiLogH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      let html = toText $ Lucid.renderText $ Lucid.toHtml page
      -- Scoped to the container's own tag, not the page: `lazyLoad_` and the widget loader
      -- already carry the same attribute, so a page-wide search passes with this one deleted.
      -- Splitting on '<' makes it independent of attribute order within the tag.
      find (T.isInfixOf "id=\"log_details_container\"") (T.splitOn "<" html)
        `shouldSatisfy` maybe False (T.isInfixOf "hx-sync=\"this:replace\"")

  describe "Trace fullscreen scrolling" do
    it "apiLogH_traceOverlayDoesNotCreateAScrollContainer" \tr -> do
      (_, page) <- testServant tr $ Log.apiLogH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      let html = toText $ Lucid.renderText $ Lucid.toHtml page
      find (T.isInfixOf "id=\"trace_expanded_view\"") (T.splitOn "<" html)
        `shouldSatisfy` maybe False (T.isInfixOf "overflow-hidden")

  describe "Query editor skeleton" do
    it "apiLogH_rendersAnEmptyQueryAsAMutedPlaceholder" \tr -> do
      (_, page) <- testServant tr $ Log.apiLogH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      let html = toText $ Lucid.renderText $ Lucid.toHtml page
      find (T.isInfixOf "level ==") (T.splitOn "<" html)
        `shouldSatisfy` maybe False (T.isInfixOf "opacity-60")
  describe "Trace Tree" do
    -- Regression: startNs was folded with a 0 seed, so every synthetic orphan
    -- header started at 0 and its duration spanned from the epoch to the last span.
    it "synthesizeOrphanHeaders anchors start_time_ns at the earliest child span" \_ -> do
      let colIdxMap = HashMap.fromList $ zip ["trace_id", "parent_id", "latency_breakdown", "start_time_ns", "duration"] [0 ..]
          row sid start dur = V.fromList [AE.String "t1", AE.String "missing-parent", AE.String sid, AE.Number start, AE.Number dur]
          synth = Log.synthesizeOrphanHeaders colIdxMap $ V.fromList [row "s1" 1000 10, row "s2" 2000 20]
          cell k r = HashMap.lookup k colIdxMap >>= (r V.!?)
      V.length synth `shouldBe` 1
      let hdr = Unsafe.fromJust $ synth V.!? 0
      cell "start_time_ns" hdr `shouldBe` Just (AE.Number 1000)
      cell "duration" hdr `shouldBe` Just (AE.Number 1020)

    it "should include traces field with tree structure" \tr -> do
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
          reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
          msgs = map (\i -> ("tm" <> show i, toStrict $ AE.encode reqMsg)) ([1 .. 5] :: [Int])
      void $ runTestBackground frozenTime tr.trATCtx $ processMessages msgs HashMap.empty
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      r <- fetchData tr Nothing Nothing Nothing Nothing fromTime toTime
      V.length r.logsData `shouldSatisfy` (>= 5)
      length r.traces `shouldSatisfy` (> 0)
      forM_ r.traces \entry -> do
        entry.traceId `shouldNotBe` ""
        entry.root `shouldNotBe` ""
        entry.startTime `shouldSatisfy` (>= 0)
        entry.duration `shouldSatisfy` (>= 0)

    it "traces should match request vecs trace IDs" \tr -> do
      -- Own project: the trace-tree entries are built from this page of rows, but
      -- their child spans are looked up per trace id, so any foreign trace sharing
      -- the window (the shared testPid collects them from every other example)
      -- shows up as an entry with no row of its own.
      pid <- createTestProject tr "log-explorer-trace-ids"
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      msgs <- forM [testRequestMsgs.reqMsg1 nowTxt, testRequestMsgs.reqMsg2 nowTxt] $ \v -> ("tt",) <$> freshMsg pid v
      void $ runTestBackground frozenTime tr.trATCtx $ processMessages msgs HashMap.empty
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      r <- eventually (fetchDataIn tr pid Nothing Nothing Nothing Nothing fromTime toTime) ((>= 2) . V.length . (.logsData))
      case HashMap.lookup "trace_id" r.colIdxMap of
        Just idx -> do
          let vecTraceIds = V.toList $ V.mapMaybe (\v -> case v V.!? idx of Just (AE.String t) -> Just t; _ -> Nothing) r.logsData
          forM_ r.traces \entry -> vecTraceIds `shouldContain` [entry.traceId]
        Nothing -> pass

    it "children map values reference valid span IDs" \tr -> do
      -- Own project, and 10 distinct messages rather than 10 encodings of one:
      -- an absent msg_id makes the span id a UUIDv5 of the whole payload, so
      -- identical copies collapse to a single row on TimeFusion. On the shared
      -- project the trace entries also came from other examples' rows, whose
      -- span ids are not on this page.
      pid <- createTestProject tr "log-explorer-children"
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      msgs <- forM ([1 .. 10] :: [Int]) \i -> ("cv" <> show i,) <$> freshMsg pid (testRequestMsgs.reqMsg1 nowTxt)
      void $ runTestBackground frozenTime tr.trATCtx $ processMessages msgs HashMap.empty
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      r <- eventually (fetchDataIn tr pid Nothing Nothing Nothing Nothing fromTime toTime) ((>= 1) . V.length . (.logsData))
      case (HashMap.lookup "latency_breakdown" r.colIdxMap, HashMap.lookup "id" r.colIdxMap) of
        (Just lbi, Just idi) -> do
          let allSpanIds = V.toList $ V.mapMaybe (\v -> case v V.!? lbi of Just (AE.String t) -> Just t; _ -> case v V.!? idi of Just (AE.String t) -> Just t; _ -> Nothing) r.logsData
          forM_ r.traces \(entry :: Log.TraceTreeEntry) -> do
            allSpanIds `shouldContain` [entry.root]
            forM_ (Map.elems entry.children) \childIds ->
              forM_ childIds \cid -> allSpanIds `shouldContain` [cid]
        _ -> pass

  describe "Query library endpoints (saveQueryH/deleteQueryH)" do
    -- Regression: the item-body onclick wrapped dataset.query in JSON.parse, but
    -- data-query holds raw KQL — clicking any saved query threw a SyntaxError
    -- while the sibling "Run this query" button passed it raw.
    it "query library items pass dataset.query to handleAddQuery unparsed" \_ -> do
      now' <- getCurrentTime
      qid <- nextRandom
      let qli =
            Projects.QueryLibItem
              { id = UUIDId qid
              , projectId = testPid
              , createdAt = now'
              , updatedAt = now'
              , userId = Projects.UserId UUID.nil
              , queryType = Projects.QLTSaved
              , queryText = "status_code == \"500\""
              , queryAst = AE.Null
              , title = Just "errors"
              , byMe = True
              }
          html = LT.toStrict $ Lucid.renderText $ LogQueryBox.queryLibraryContent_ (V.singleton qli) V.empty
      -- Lucid escapes quotes in attributes, so match quote-free fragments:
      -- "dataset.query)" is the raw pass-through; the Run button renders "dataset.query, true".
      html `shouldSatisfy` T.isInfixOf "dataset.query)"
      html `shouldNotSatisfy` T.isInfixOf "JSON.parse"
      for_ ["Run this query", "Copy query to clipboard", "Edit query title", "Delete query", "Show all team queries"] \action ->
        html `shouldSatisfy` T.isInfixOf ("aria-label=\"" <> action <> "\"")
    -- Mutations used to be smuggled through the log-fetch GET via ?layout=.
    -- They are now their own POST/DELETE endpoints; this exercises the full
    -- create → rename → delete round-trip through the DB.
    it "saves, renames, then deletes a query library item" \tr -> do
      title <- ("saved-" <>) . UUID.toText <$> nextRandom
      (_, saved1) <- testServant tr $ QueryLibrary.saveQueryH testPid (QueryLibrary.SaveQueryForm (Just "status_code == \"200\"") Nothing (Just title))
      let itemsOf (QueryLibrary.QueryLibraryView (s, _)) = s
          found :: Text -> QueryLibrary.QueryLibraryView -> Maybe Projects.QueryLibItem
          found t v = V.find (\q -> q.title == Just t) (itemsOf v)
      found title saved1 `shouldSatisfy` isJust

      let qid = maybe (error "saved item missing") (\q -> q.id.toText) (found title saved1)
      newTitle <- ("renamed-" <>) . UUID.toText <$> nextRandom
      (_, saved2) <- testServant tr $ QueryLibrary.saveQueryH testPid (QueryLibrary.SaveQueryForm (Just "status_code == \"200\"") (Just qid) (Just newTitle))
      found newTitle saved2 `shouldSatisfy` isJust
      found title saved2 `shouldSatisfy` isNothing

      (_, saved3) <- testServant tr $ QueryLibrary.deleteQueryH testPid qid
      found newTitle saved3 `shouldSatisfy` isNothing

  describe "Aggregate viz endpoints (logPatternsH/logSessionsH)" do
    it "patterns endpoint returns a well-formed aggregate envelope" \tr -> do
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
          reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      void $ runTestBackground frozenTime tr.trATCtx $ processMessages [("pm1", toStrict $ AE.encode reqMsg)] HashMap.empty
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      (_, pv) <- testServant tr $ Log.logPatternsH testPid Nothing fromTime Nothing toTime (Just "spans") Nothing Nothing
      case pv of { Log.PatternsView total _ _ _ -> total `shouldSatisfy` (>= 0) }
      -- The JSON envelope carries the shared aggregate columns + the pattern total.
      let j = decodeUtf8 (toStrict (AE.encode pv)) :: Text
      j `shouldSatisfy` T.isInfixOf "totalPatterns"
      j `shouldSatisfy` T.isInfixOf "colIdxMap"

    it "sessions endpoint returns a well-formed aggregate envelope" \tr -> do
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      (_, sv) <- testServant tr $ Log.logSessionsH testPid Nothing fromTime Nothing toTime Nothing Nothing
      case sv of { Log.SessionsView total _ _ -> total `shouldSatisfy` (>= 0) }

  describe "Log Explorer page shell" do
    it "renders Common facets with the page and leaves other groups lazy" \tr -> do
      seedFacetSummary tr
      (_, page) <- testServant tr $ Log.apiLogH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
          eagerUrl = "hx-get=\"/p/" <> testPid.toText <> "/log_explorer/facets\""
      html `shouldSatisfy` T.isInfixOf "data-field=\"resource.service.name\""
      html `shouldSatisfy` T.isInfixOf "data-value=\"checkout-svc\""
      html `shouldNotSatisfy` T.isInfixOf "data-value=\"/orders\""
      html `shouldSatisfy` T.isInfixOf "facets?group=http"
      html `shouldNotSatisfy` T.isInfixOf eagerUrl

    it "queryEditor_keepsRestingBoundariesQuietUntilFocus" \tr -> do
      (_, page) <- testServant tr $ Log.apiLogH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
      html `shouldSatisfy` T.isInfixOf "bg-bgRaised rounded-lg border border-strokeWeak focus-within:ring-2"
      html `shouldSatisfy` T.isInfixOf "focus-within:ring-2 focus-within:ring-strokeBrand-weak"
      html `shouldSatisfy` T.isInfixOf "aria-disabled:border-strokeWeak"
      html `shouldSatisfy` T.isInfixOf "border border-strokeWeak hover:border-strokeStrong h-8"
      html `shouldSatisfy` T.isInfixOf "id=\"ai-search-submit\""
      html `shouldSatisfy` T.isInfixOf "aria-disabled=\"true\""
      html `shouldSatisfy` T.isInfixOf "set #ai-search-submit&#39;s @aria-disabled"

    it "timeTransport_disabledNextButton_hasSingleDivider" \tr -> do
      (_, page) <- testServant tr $ Log.apiLogH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      let html = toText $ Lucid.renderText $ Lucid.toHtml page
      find (T.isInfixOf "aria-label=\"Next time window\"") (T.splitOn "<" html)
        `shouldSatisfy` maybe False (T.isInfixOf "disabled:-ms-px")

    -- Regression: HTMX swaps only #main-content, so a preload script in <head>
    -- was discarded when arriving from another page. The table then raced its
    -- fallback worker fetch against chart requests and could render an empty list.
    it "keeps the initial log-data preload inside the HTMX swap target" \tr -> do
      (_, page) <- testServant tr $ Log.apiLogH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
          indexOf needle = T.length $ fst $ T.breakOn needle html
      html `shouldSatisfy` T.isInfixOf "id=\"main-content\""
      html `shouldSatisfy` T.isInfixOf "window.logDataPromise"
      -- htmx 4 (6de99be) renamed the attribute `preload` -> `hx-preload`; this
      -- assertion kept the old spelling and so stopped checking anything real.
      html `shouldSatisfy` T.isInfixOf "href=\"https://monoscope.tech/docs/dashboard/dashboard-pages/api-log-explorer/\" hx-preload=\"false\""
      indexOf "id=\"main-content\"" `shouldSatisfy` (< indexOf "window.logDataPromise")

  describe "Trace fullscreen loading" do
    -- Regression: opening a trace used to clear the fullscreen overlay while the
    -- request was in flight; shared trace links showed only a disconnected dots
    -- spinner. Both paths must now use the same trace-shaped loading state.
    it "renders a reusable trace skeleton for shared and in-app trace loads" \tr -> do
      let traceRef = "774115aaa715abf80d93fc629c2269a4/?timestamp=2026-07-15T18:59:16.952128Z"
      (_, page) <- testServant tr $ Log.apiLogH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just traceRef) Nothing Nothing Nothing
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
      html `shouldSatisfy` T.isInfixOf "trace-loading-skeleton"
      html `shouldSatisfy` T.isInfixOf "Loading trace"
      html `shouldSatisfy` T.isInfixOf "Retry loading trace"
      html `shouldSatisfy` T.isInfixOf "send loadTrace"
      html `shouldNotSatisfy` T.isInfixOf "window.logDataPromise"

  describe "Pattern expand (apiLogExpandH)" do
    -- Regression: clicking a pattern used to send the summary *template* as the
    -- key and match it via `array_to_string(summary, chr(30)) ILIKE …`, which
    -- never matched the real multi-element / metadata-stripped summary array —
    -- the row always came back "No events". The drain-flush stamps a
    -- `pat:<hash>` tag on each row's `hashes` column, so expand now matches by
    -- tag. This pins that: a hash key finds its tagged row, an untagged hash
    -- doesn't, and the summary-template fallback path can't spoof a hash match.
    it "matches example events by pat:<hash> tag, not by summary ILIKE" \tr -> do
      let spanName = "GET /api/pattern-expand" :: Text
          patHash = "abcd1234" :: Text
      pid <- createTestProject tr "log-explorer-pattern-expand"
      apiKey <- createTestAPIKey tr pid "pattern-expand-key"
      ingestTrace tr apiKey spanName frozenTime
      -- The tag has to be stamped on whichever store the expand reads. tr.trPool is
      -- Postgres only; the TimeFusion copy is reached through the labelled pool, the
      -- same route the drain-flush uses in production.
      -- Bound outside the quoter: hasql-interpolate's TH cannot parse a type
      -- annotation inside #{}, and the list needs one to resolve OverloadedLists.
      let patTags = ["pat:" <> patHash] :: [Text]
      void $ withPool tr.trPool $ DBT.execute [sql| UPDATE otel_logs_and_spans SET hashes = ? WHERE project_id = ? AND name = ? |] (PGArray patTags, pid, spanName)
      when tr.trATCtx.env.enableTimefusionReads
        $ runQueryEffect tr
        $ Hasql.withHasqlTimefusion True
        $ Hasql.interpExecute_ [HI.sql| UPDATE otel_logs_and_spans SET hashes = #{patTags} WHERE project_id = #{pid.toText} AND name = #{spanName} |]

      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
          rowCount v = case v of AE.Object o | Just (AE.Number n) <- AEKM.lookup "queryResultCount" o -> round n; _ -> -1
          expand key = rowCount . snd <$> testServant tr (Log.apiLogExpandH pid (Just "pattern") (Just key) Nothing Nothing fromTime Nothing toTime)

      -- The real hash tag finds the tagged row.
      eventually (expand patHash) (== 1) >>= (`shouldBe` (1 :: Int))
      -- A hash-shaped key with no matching tag finds nothing (no accidental match).
      expand "00000000" >>= (`shouldBe` 0)
      -- The summary-template fallback (non-hash key) must not tag-match this row.
      expand spanName >>= (`shouldBe` 0)

  describe "Alert form endpoint (alertFormH)" do
    it "renders the create-monitor form" \tr -> do
      (_, html) <- testServant tr $ Log.alertFormH testPid Nothing
      LT.toStrict (Lucid.renderText html) `shouldSatisfy` T.isInfixOf "Create monitor"

  describe "Log item detail (expandAPIlogItemH)" do
    it "trace-timefusion-read loads a span and decodes legacy NULL hashes on PG" \tr -> do
      let spanName = "GET /api/log-item/tf" :: Text
      apiKey <- createTestAPIKey tr testPid "log-item-tf-key"
      ingestTrace tr apiKey spanName frozenTime
      rows <-
        withPool tr.trPool
          $ DBT.query
            [sql| SELECT id, timestamp, context___trace_id FROM otel_logs_and_spans WHERE project_id = ? AND name = ? |]
            (testPid, spanName)
          :: IO (V.Vector (UUID.UUID, UTCTime, Text))
      (rid, ts, traceIdTxt) <- maybe (error "ingested span missing from otel_logs_and_spans") pure (rows V.!? 0)

      let expectFound item = case item of
            LogItem.ItemDetailedNotFound msg -> expectationFailure $ "expected record, got not-found: " <> toString msg
            LogItem.SpanItemExpanded _ (rec :: Telemetry.OtelLogsAndSpans) _ _ -> rec.name `shouldBe` Just spanName
            LogItem.LogItemExpanded _ rec _ -> rec.name `shouldBe` Just spanName
            LogItem.DetailTabExpanded _ rec _ _ _ -> rec.name `shouldBe` Just spanName

      let ctx = tr.trATCtx
          withTfReads b = tr{trATCtx = ctx{env = ctx.env{enableTimefusionReads = b}}}
      (_, item) <- testServant (withTfReads True) $ LogItem.expandAPIlogItemH testPid rid ts Nothing Nothing Nothing False
      expectFound item
      (_, traceDetails) <- testServant (withTfReads True) $ TelemetryPage.traceH testPid traceIdTxt (Just ts) Nothing Nothing Nothing Nothing
      case traceDetails of
        TelemetryPage.TraceDetails{} -> pass
        TelemetryPage.SpanDetails _ _ _ -> expectationFailure "expected trace details, got span details"
        TelemetryPage.TraceDetailsNotFound _ _ _ -> expectationFailure "expected TimeFusion trace details"
      let initialHtml = LT.toStrict $ Lucid.renderText $ Lucid.toHtml item
      -- Only the selected panel renders; hidden tabs fetch their panel on first reveal.
      -- The placeholder div still carries the panel id — `hx-swap: outerHTML` has to have
      -- something to target, so asserting the id was *absent* contradicted the lazy-panel
      -- design and only passed before those panels became lazy. What proves laziness is the
      -- loader in its place, plus the deferred fetch asserted below.
      initialHtml `shouldSatisfy` T.isInfixOf "id=\"m-raw-content\""
      initialHtml `shouldSatisfy` T.isInfixOf "aria-busy=\"true\""
      initialHtml `shouldSatisfy` T.isInfixOf "intersect once"
      initialHtml `shouldSatisfy` T.isInfixOf "/detailed?tab=tab-raw&amp;partial=true"
      T.length initialHtml `shouldSatisfy` (< 50_000)
      (_, rawTab) <- testServant (withTfReads True) $ LogItem.expandAPIlogItemH testPid rid ts Nothing (Just "tab-raw") Nothing True
      let rawHtml = LT.toStrict $ Lucid.renderText $ Lucid.toHtml rawTab
      rawHtml `shouldSatisfy` T.isInfixOf "m-raw-content"
      rawHtml `shouldNotSatisfy` T.isInfixOf "intersect once"

      let expectNotFound store item' = case item' of
            LogItem.ItemDetailedNotFound _ -> pass
            _ -> expectationFailure $ store <> ": off-by-1s timestamp unexpectedly matched (window not removed)"
      (_, miss) <- testServant (withTfReads True) $ LogItem.expandAPIlogItemH testPid rid (addUTCTime 1 ts) Nothing Nothing Nothing False
      expectNotFound "TF" miss

      void $ withPool tr.trPool $ DBT.execute [sql| UPDATE otel_logs_and_spans SET hashes = NULL WHERE id = ? |] (Only rid)
      (_, item2) <- testServant (withTfReads False) $ LogItem.expandAPIlogItemH testPid rid ts Nothing Nothing Nothing False
      expectFound item2
      (_, miss2) <- testServant (withTfReads False) $ LogItem.expandAPIlogItemH testPid rid (addUTCTime 1 ts) Nothing Nothing Nothing False
      expectNotFound "PG" miss2

    -- Regression: the panel used to surface one identifier — `user.email`, or `user.id`
    -- mislabelled "user name" when email was absent — so tenant id, user id and the rest
    -- were reachable only by opening the Attributes JSON tree. Readers concluded we only
    -- knew their email.
    it "shows every session, user and tenant field the span carries, each as a filter pill" \tr -> do
      apiKey <- createTestAPIKey tr testPid "identity-key"
      let hexId = T.replace "-" "" . UUID.toText <$> nextRandom
      (trId, sid) <- (,) <$> hexId <*> hexId
      ingestSpanLinked
        tr
        apiKey
        trId
        sid
        Nothing
        "GET /api/invoices"
        [ ("http.request.method", "GET")
        , ("session.id", "sess-9f2")
        , ("user.id", "usr-4471")
        , ("user.email", "ada@example.com")
        , ("tenant.id", "acme-eu")
        , ("workspace.id", "ws-12")
        ]
        frozenTime
      rows <-
        withPool tr.trPool
          $ DBT.query
            [sql| SELECT id, timestamp FROM otel_logs_and_spans WHERE project_id = ? AND context___trace_id = ? |]
            (testPid, trId)
          :: IO (V.Vector (UUID.UUID, UTCTime))
      (rid, ts) <- maybe (error "ingested span missing") pure (rows V.!? 0)
      (_, item) <- testServant tr $ LogItem.expandAPIlogItemH testPid rid ts Nothing Nothing Nothing False
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml item
      for_ ([("Session ID", "sess-9f2"), ("User ID", "usr-4471"), ("User Email", "ada@example.com"), ("Tenant ID", "acme-eu"), ("Workspace ID", "ws-12")] :: [(Text, Text)]) \(label, value) -> do
        html `shouldSatisfy` T.isInfixOf (label <> ": " <> value)
        -- Each is a filter pill, so "everything from this tenant" is one click away.
        html `shouldSatisfy` T.isInfixOf ("data-field-value=\"&quot;" <> value <> "&quot;\"")
      -- A key the span does not carry contributes no row.
      html `shouldNotSatisfy` T.isInfixOf "Org ID"

    -- Regression: the SDK payload span ("monoscope.http") confused users — the
    -- parent request's panel showed empty body tabs (the merge only fired when
    -- http.request.method was absent, which auto-instrumented spans always have),
    -- and clicking the SDK span showed a synthetic span instead of the request.
    it "merges SDK payload bodies into the parent request and anchors SDK-span clicks on the parent" \tr -> do
      apiKey <- createTestAPIKey tr testPid "sdk-merge-key"
      -- Ids are hex-decoded on ingest, so stored ids are dash-free — generate them that way.
      let hexId = T.replace "-" "" . UUID.toText <$> nextRandom
      (trId, rootSid, sdkSid) <- (,,) <$> hexId <*> hexId <*> hexId
      let b64 = B64T.extractBase64 . B64.encodeBase64 . encodeUtf8 @Text
      ingestSpanLinked tr apiKey trId rootSid Nothing "GET /api/orders" [("http.request.method", "GET"), ("http.route", "/api/orders")] frozenTime
      ingestSpanLinked tr apiKey trId sdkSid (Just rootSid) "apitoolkit-http-span" [("http.request.body", b64 "{\"item\":\"apple\"}"), ("http.response.body", b64 "{\"ok\":true}"), ("http.request.method", "GET")] (addUTCTime 0.01 frozenTime)
      rows <-
        withPool tr.trPool
          $ DBT.query
            [sql| SELECT id, timestamp, name, context___span_id FROM otel_logs_and_spans WHERE project_id = ? AND context___trace_id = ? ORDER BY start_time |]
            (testPid, trId)
          :: IO (V.Vector (UUID.UUID, UTCTime, Text, Text))
      [(rootId, rootTs, rootName, _), (sdkId, sdkTs, sdkName, sdkStoredSid)] <- pure $ V.toList rows
      rootName `shouldBe` "GET /api/orders"
      sdkName `shouldBe` "monoscope.http"

      -- Both clicks land on the same panel: anchored on the request, SDK bodies attached.
      let assertAnchored = \case
            LogItem.SpanItemExpanded _ anchor sdkM _ -> do
              anchor.name `shouldBe` Just "GET /api/orders"
              (.name) <$> sdkM `shouldBe` Just (Just "monoscope.http")
            _ -> expectationFailure "expected SpanItemExpanded anchored on the request span"

      -- Clicking the request span: bodies borrowed from the nested SDK span.
      (_, rootItem) <- testServant tr $ LogItem.expandAPIlogItemH testPid rootId rootTs Nothing (Just "tab-req") Nothing False
      assertAnchored rootItem
      let rootHtml = LT.toStrict $ Lucid.renderText $ Lucid.toHtml rootItem
      rootHtml `shouldSatisfy` T.isInfixOf "subtab=htab-req"
      -- Copy-as-curl is assembled server-side from the HTTP attributes.
      rootHtml `shouldSatisfy` T.isInfixOf "curl -X GET"
      -- The body panel is fetched on reveal, so curl holds the only inlined copy of the
      -- body. This used to assert the body text appeared nowhere on the page, which stopped
      -- being true the moment curl started carrying it — one occurrence still proves the
      -- panel didn't inline a second.
      T.count "apple" rootHtml `shouldBe` 1
      (_, reqBodyTab) <- testServant tr $ LogItem.expandAPIlogItemH testPid rootId rootTs Nothing (Just "tab-req") (Just "htab-req") True
      let reqBodyHtml = LT.toStrict $ Lucid.renderText $ Lucid.toHtml reqBodyTab
      reqBodyHtml `shouldSatisfy` T.isInfixOf "req_content"
      reqBodyHtml `shouldSatisfy` T.isInfixOf "apple"
      reqBodyHtml `shouldNotSatisfy` T.isInfixOf "intersect once"

      -- An unknown ?tab= deep link clamps to a real tab (a checked radio must exist).
      (_, bogusTab) <- testServant tr $ LogItem.expandAPIlogItemH testPid rootId rootTs Nothing (Just "tab-nonexistent") Nothing False
      LT.toStrict (Lucid.renderText (Lucid.toHtml bogusTab)) `shouldSatisfy` T.isInfixOf "checked"

      -- Clicking the SDK span: panel anchors on the parent request, bodies intact.
      (_, sdkItem) <- testServant tr $ LogItem.expandAPIlogItemH testPid sdkId sdkTs Nothing (Just "tab-req") Nothing False
      assertAnchored sdkItem

      -- Waterfall keyboard-nav onto the SDK span also anchors on the parent request.
      (_, navDetails) <- testServant tr $ TelemetryPage.traceH testPid trId (Just rootTs) (Just sdkStoredSid) (Just "next") Nothing Nothing
      case navDetails of
        TelemetryPage.SpanDetails _ target sdkM -> do
          target.name `shouldBe` Just "GET /api/orders"
          (.name) <$> sdkM `shouldBe` Just (Just "monoscope.http")
        _ -> expectationFailure "expected SpanDetails anchored on the request span"

      -- The waterfall collapses the redundant SDK row into its parent.
      (_, traceDetails) <- testServant tr $ TelemetryPage.traceH testPid trId (Just rootTs) Nothing Nothing Nothing Nothing
      case traceDetails of
        TelemetryPage.TraceDetails{} -> do
          let traceHtml = LT.toStrict $ Lucid.renderText $ Lucid.toHtml traceDetails
          traceHtml `shouldSatisfy` T.isInfixOf "GET /api/orders"
          traceHtml `shouldNotSatisfy` T.isInfixOf "monoscope.http"
          traceHtml `shouldSatisfy` T.isInfixOf "timeline.addEventListener('tab-visible'"
        _ -> expectationFailure "expected trace details"

    it "flags ERROR-severity logs in the trace-view 'errors' column" \tr -> do
      pid <- createTestProject tr "log-explorer-error-flag"
      apiKey <- createTestAPIKey tr pid "err-log-badge-key"
      ingestErrorLog tr apiKey "boom: db connection failed" [] frozenTime
      ingestLog tr apiKey "ordinary info line" frozenTime
      let range = (Just (addUTCTime (-60) frozenTime), Just (addUTCTime 60 frozenTime))
          logRowsNow = do
            res <- runQueryEffect tr $ LogQueries.selectLogTable tr.trATCtx.env.enableTimefusionReads pid [] "" Nothing range [] (Just SSpans) Nothing Nothing
            (rows, cols, _) <- either (\e -> error ("selectLogTable failed: " <> e)) pure res
            let colIx name = Unsafe.fromJust $ V.elemIndex name (V.fromList cols)
            pure [(r V.!? colIx "errors") == Just (AE.Bool True) | r <- V.toList rows, (r V.!? colIx "kind") == Just (AE.String "log")]
      logRows <- eventually logRowsNow ((== 2) . length)
      length logRows `shouldBe` 2
      length (filter id logRows) `shouldBe` 1
