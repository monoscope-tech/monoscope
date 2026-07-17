module Pages.LogExplorer.LogSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
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
import Lucid qualified
import Models.Apis.LogQueries qualified as LogQueries
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.GRPC.Common.Protobuf (Proto (..))
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.LogExplorer.Log qualified as Log
import Pages.LogExplorer.LogItem qualified as LogItem
import Pkg.Parser.Stats (Sources (..))
import Pkg.TestUtils
import ProcessMessage (processMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext (..), EnvConfig (..))
import Test.Hspec


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
fetchData tr q cols cur since from to = snd <$> testServant tr (Log.logExplorerDataH testPid q cols cur since from to Nothing Nothing)


spec :: Spec
spec = around withTestResources do
  describe "Log data endpoint (logExplorerDataH)" do
    it "should return an empty list" \tr -> do
      r <- fetchData tr Nothing Nothing Nothing Nothing Nothing Nothing
      V.length r.logsData `shouldBe` 0
      r.count `shouldBe` 0
      r.cols `shouldBe` ["id", "timestamp", "service", "summary", "latency_breakdown"]

    it "should return log items" \tr -> do
      let yesterdayTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-86400) frozenTime
      let twoDaysAgoTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-172800) frozenTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      -- new requests otherwise cursor for load more will be the same
      let reqMsg3 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 yesterdayTxt
      let reqMsg4 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 twoDaysAgoTxt

      let msgs = concat (replicate 100 [("m1", toStrict $ AE.encode reqMsg1), ("m2", toStrict $ AE.encode reqMsg2)]) ++ [("m3", toStrict $ AE.encode reqMsg3), ("m4", toStrict $ AE.encode reqMsg4)]
      res <- runTestBackground frozenTime tr.trATCtx $ processMessages msgs HashMap.empty
      bimap (show @Text) (length . fst) res `shouldBe` Right 202

      let threeDaysAgo = addUTCTime (-259200) frozenTime
      let oneDayFuture = addUTCTime 86400 frozenTime
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" threeDaysAgo
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" oneDayFuture

      r <- fetchData tr Nothing Nothing Nothing Nothing fromTime toTime
      V.length r.logsData `shouldBe` 202
      r.count `shouldSatisfy` (>= 202)
      r.cols `shouldBe` ["id", "timestamp", "service", "summary", "latency_breakdown"]
      -- URLs are stamped by the data endpoint and consumed by the web component.
      r.nextUrl `shouldNotBe` ""
      r.resetLogsUrl `shouldNotBe` ""
      r.recentUrl `shouldNotBe` ""
      -- The stamped URLs must target the data endpoint, not the page shell.
      r.nextUrl `shouldSatisfy` T.isInfixOf "/log_explorer/data"

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

    it "should return the requested extra columns" \tr -> do
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      let reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      _ <- runTestBackground frozenTime tr.trATCtx $ processMessages [("m1", toStrict $ AE.encode reqMsg)] HashMap.empty
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      r <- fetchData tr Nothing (Just "id,timestamp,name,duration") Nothing Nothing fromTime toTime
      r.cols `shouldBe` ["id", "timestamp", "name", "duration", "service", "summary", "latency_breakdown"]

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

  describe "Data endpoint query-error handling" do
    it "returns an empty result for invalid query syntax (not a crash)" \tr -> do
      r <- fetchData tr (Just "status_code = 200") Nothing Nothing Nothing Nothing Nothing
      V.length r.logsData `shouldBe` 0
      r.count `shouldBe` 0

    it "returns an empty result for malformed operators" \tr -> do
      r <- fetchData tr (Just "status_code === \"200\"") Nothing Nothing Nothing Nothing Nothing
      V.length r.logsData `shouldBe` 0
      r.count `shouldBe` 0

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
      void
        $ withPool tr.trPool
        $ DBT.execute
          [sql| INSERT INTO otel_logs_and_spans (project_id, timestamp, name, summary)
              SELECT ?, ?::timestamptz - g * interval '50 milliseconds', 'seed.row', '{}'
              FROM generate_series(1, 520) AS g |]
          (testPid, frozenTime)

      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime

      r <- fetchData tr Nothing Nothing Nothing Nothing fromTime toTime
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
      let oneHourAgo = addUTCTime (-3600) frozenTime
      let twoHoursAgo = addUTCTime (-7200) frozenTime
      let threeHoursAgo = addUTCTime (-10800) frozenTime
      let msg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 (toText $ formatTime defaultTimeLocale "%FT%T%QZ" oneHourAgo)
      let msg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 (toText $ formatTime defaultTimeLocale "%FT%T%QZ" twoHoursAgo)
      let msg3 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 (toText $ formatTime defaultTimeLocale "%FT%T%QZ" threeHoursAgo)
      let msgs = [("m1", toStrict $ AE.encode msg1), ("m2", toStrict $ AE.encode msg2), ("m3", toStrict $ AE.encode msg3)]
      _ <- runTestBackground frozenTime tr.trATCtx $ processMessages msgs HashMap.empty

      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-9000) frozenTime
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-5400) frozenTime
      r <- fetchData tr Nothing Nothing Nothing Nothing fromTime toTime
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

  describe "Trace Tree" do
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
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
          reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
          reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
          msgs = [("tt1", toStrict $ AE.encode reqMsg1), ("tt2", toStrict $ AE.encode reqMsg2)]
      void $ runTestBackground frozenTime tr.trATCtx $ processMessages msgs HashMap.empty
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      r <- fetchData tr Nothing Nothing Nothing Nothing fromTime toTime
      case HashMap.lookup "trace_id" r.colIdxMap of
        Just idx -> do
          let vecTraceIds = V.toList $ V.mapMaybe (\v -> case v V.!? idx of Just (AE.String t) -> Just t; _ -> Nothing) r.logsData
          forM_ r.traces \entry -> vecTraceIds `shouldContain` [entry.traceId]
        Nothing -> pass

    it "children map values reference valid span IDs" \tr -> do
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
          reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
          msgs = map (\i -> ("cv" <> show i, toStrict $ AE.encode reqMsg)) ([1 .. 10] :: [Int])
      void $ runTestBackground frozenTime tr.trATCtx $ processMessages msgs HashMap.empty
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      r <- fetchData tr Nothing Nothing Nothing Nothing fromTime toTime
      case (HashMap.lookup "latency_breakdown" r.colIdxMap, HashMap.lookup "id" r.colIdxMap) of
        (Just lbi, Just idi) -> do
          let allSpanIds = V.toList $ V.mapMaybe (\v -> case v V.!? lbi of Just (AE.String t) -> Just t; _ -> case v V.!? idi of Just (AE.String t) -> Just t; _ -> Nothing) r.logsData
          forM_ r.traces \(entry :: Log.TraceTreeEntry) -> do
            allSpanIds `shouldContain` [entry.root]
            forM_ (Map.elems entry.children) \childIds ->
              forM_ childIds \cid -> allSpanIds `shouldContain` [cid]
        _ -> pass

  describe "Query library endpoints (saveQueryH/deleteQueryH)" do
    -- Mutations used to be smuggled through the log-fetch GET via ?layout=.
    -- They are now their own POST/DELETE endpoints; this exercises the full
    -- create → rename → delete round-trip through the DB.
    it "saves, renames, then deletes a query library item" \tr -> do
      title <- ("saved-" <>) . UUID.toText <$> nextRandom
      (_, saved1) <- testServant tr $ Log.saveQueryH testPid (Log.SaveQueryForm (Just "status_code == \"200\"") Nothing (Just title))
      let itemsOf (Log.QueryLibraryView _ s _) = s
          found :: Text -> Log.QueryLibraryView -> Maybe Projects.QueryLibItem
          found t v = V.find (\q -> q.title == Just t) (itemsOf v)
      found title saved1 `shouldSatisfy` isJust

      let qid = maybe (error "saved item missing") (\q -> q.id.toText) (found title saved1)
      newTitle <- ("renamed-" <>) . UUID.toText <$> nextRandom
      (_, saved2) <- testServant tr $ Log.saveQueryH testPid (Log.SaveQueryForm (Just "status_code == \"200\"") (Just qid) (Just newTitle))
      found newTitle saved2 `shouldSatisfy` isJust
      found title saved2 `shouldSatisfy` isNothing

      (_, saved3) <- testServant tr $ Log.deleteQueryH testPid qid
      found newTitle saved3 `shouldSatisfy` isNothing

  describe "Aggregate viz endpoints (logPatternsH/logSessionsH)" do
    it "patterns endpoint returns a well-formed aggregate envelope" \tr -> do
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
          reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      void $ runTestBackground frozenTime tr.trATCtx $ processMessages [("pm1", toStrict $ AE.encode reqMsg)] HashMap.empty
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      (_, pv) <- testServant tr $ Log.logPatternsH testPid Nothing fromTime Nothing toTime (Just "spans") Nothing Nothing
      case pv of Log.PatternsView total _ _ _ -> total `shouldSatisfy` (>= 0)
      -- The JSON envelope carries the shared aggregate columns + the pattern total.
      let j = decodeUtf8 (toStrict (AE.encode pv)) :: Text
      j `shouldSatisfy` T.isInfixOf "totalPatterns"
      j `shouldSatisfy` T.isInfixOf "colIdxMap"

    it "sessions endpoint returns a well-formed aggregate envelope" \tr -> do
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      (_, sv) <- testServant tr $ Log.logSessionsH testPid Nothing fromTime Nothing toTime Nothing Nothing
      case sv of Log.SessionsView total _ _ -> total `shouldSatisfy` (>= 0)

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
      apiKey <- createTestAPIKey tr testPid "pattern-expand-key"
      ingestTrace tr apiKey spanName frozenTime
      void $ withPool tr.trPool $ DBT.execute [sql| UPDATE otel_logs_and_spans SET hashes = ? WHERE project_id = ? AND name = ? |] (PGArray ["pat:" <> patHash], testPid, spanName)

      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
          rowCount v = case v of AE.Object o | Just (AE.Number n) <- AEKM.lookup "queryResultCount" o -> round n; _ -> -1
          expand key = rowCount . snd <$> testServant tr (Log.apiLogExpandH testPid (Just "pattern") (Just key) Nothing Nothing fromTime Nothing toTime)

      -- The real hash tag finds the tagged row.
      expand patHash >>= (`shouldBe` 1)
      -- A hash-shaped key with no matching tag finds nothing (no accidental match).
      expand "00000000" >>= (`shouldBe` 0)
      -- The summary-template fallback (non-hash key) must not tag-match this row.
      expand spanName >>= (`shouldBe` 0)

  describe "Alert form endpoint (alertFormH)" do
    it "renders the create-monitor form" \tr -> do
      (_, html) <- testServant tr $ Log.alertFormH testPid Nothing
      LT.toStrict (Lucid.renderText html) `shouldSatisfy` T.isInfixOf "Create monitor"

  describe "Log item detail (expandAPIlogItemH)" do
    it "loads a span via the TimeFusion read path, and decodes legacy NULL hashes on PG" \tr -> do
      let spanName = "GET /api/log-item/tf" :: Text
      apiKey <- createTestAPIKey tr testPid "log-item-tf-key"
      ingestTrace tr apiKey spanName frozenTime
      rows <-
        withPool tr.trPool
          $ DBT.query
            [sql| SELECT id, timestamp FROM otel_logs_and_spans WHERE project_id = ? AND name = ? |]
            (testPid, spanName)
          :: IO (V.Vector (UUID.UUID, UTCTime))
      (rid, ts) <- maybe (error "ingested span missing from otel_logs_and_spans") pure (rows V.!? 0)

      let expectFound item = case item of
            LogItem.ItemDetailedNotFound msg -> expectationFailure $ "expected record, got not-found: " <> toString msg
            LogItem.SpanItemExpanded _ (rec :: Telemetry.OtelLogsAndSpans) _ _ -> rec.name `shouldBe` Just spanName
            LogItem.LogItemExpanded _ rec -> rec.name `shouldBe` Just spanName

      let ctx = tr.trATCtx
          withTfReads b = tr{trATCtx = ctx{env = ctx.env{enableTimefusionReads = b}}}
      (_, item) <- testServant (withTfReads True) $ LogItem.expandAPIlogItemH testPid rid ts Nothing Nothing
      expectFound item
      let initialHtml = LT.toStrict $ Lucid.renderText $ Lucid.toHtml item
      initialHtml `shouldSatisfy` T.isInfixOf "trace-details-content"
      initialHtml `shouldNotSatisfy` T.isInfixOf "m-raw-content"

      let expectNotFound store item' = case item' of
            LogItem.ItemDetailedNotFound _ -> pass
            _ -> expectationFailure $ store <> ": off-by-1s timestamp unexpectedly matched (window not removed)"
      (_, miss) <- testServant (withTfReads True) $ LogItem.expandAPIlogItemH testPid rid (addUTCTime 1 ts) Nothing Nothing
      expectNotFound "TF" miss

      void $ withPool tr.trPool $ DBT.execute [sql| UPDATE otel_logs_and_spans SET hashes = NULL WHERE id = ? |] (Only rid)
      (_, item2) <- testServant (withTfReads False) $ LogItem.expandAPIlogItemH testPid rid ts Nothing Nothing
      expectFound item2
      (_, miss2) <- testServant (withTfReads False) $ LogItem.expandAPIlogItemH testPid rid (addUTCTime 1 ts) Nothing Nothing
      expectNotFound "PG" miss2

    it "flags ERROR-severity logs in the trace-view 'errors' column" \tr -> do
      apiKey <- createTestAPIKey tr testPid "err-log-badge-key"
      ingestErrorLog tr apiKey "boom: db connection failed" [] frozenTime
      ingestLog tr apiKey "ordinary info line" frozenTime
      let range = (Just (addUTCTime (-60) frozenTime), Just (addUTCTime 60 frozenTime))
      res <- runQueryEffect tr $ LogQueries.selectLogTable testPid [] "" Nothing range [] (Just SSpans) Nothing
      (rows, cols, _) <- either (\e -> error ("selectLogTable failed: " <> e)) pure res
      let colIx name = Unsafe.fromJust $ V.elemIndex name (V.fromList cols)
          logRows = [r | r <- V.toList rows, (r V.!? colIx "kind") == Just (AE.String "log")]
          isErr r = (r V.!? colIx "errors") == Just (AE.Bool True)
      length logRows `shouldBe` 2
      length (filter isErr logRows) `shouldBe` 1
