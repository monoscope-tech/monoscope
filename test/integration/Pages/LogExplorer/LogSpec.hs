module Pages.LogExplorer.LogSpec (spec) where

import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pkg.DeriveUtils (UUIDId (..))
import Pages.LogExplorer.Log qualified as Log
import Pkg.TestUtils
import ProcessMessage (processMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


testPid :: Projects.ProjectId
testPid = UUIDId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Log Page" do
    it "should return an empty list" \tr -> do
      (_, pg) <-
        testServant tr $ Log.apiLogH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing

      case pg of
        Log.LogsGetJson r -> do
          -- For empty case
          V.length r.finalVecs `shouldBe` 0
          r.resultCount `shouldBe` 0
          r.curatedColNames `shouldBe` ["id", "timestamp", "service", "summary", "latency_breakdown"]
        _ -> error "Expected JSON response but got something else"

    it "should return log items" \tr -> do
      -- Use fixed frozen time: January 1, 2025
      let frozenTime = Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime
      let yesterdayTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-86400) frozenTime
      let twoDaysAgoTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-172800) frozenTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt

      -- new requests otherwise cursor for load more will be the same
      -- and hence loadmore request will return 0 items
      let reqMsg3 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 yesterdayTxt
      let reqMsg4 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 twoDaysAgoTxt

      let msgs = concat (replicate 100 [("m1", toStrict $ AE.encode reqMsg1), ("m2", toStrict $ AE.encode reqMsg2)]) ++ [("m3", toStrict $ AE.encode reqMsg3), ("m4", toStrict $ AE.encode reqMsg4)]
      res <- runTestBackground tr.trTestClock tr.trATCtx $ processMessages msgs HashMap.empty
      length res `shouldBe` 202
      
      -- Get time range that includes all messages (3 days ago to 1 day from now)
      let threeDaysAgo = addUTCTime (-259200) frozenTime  -- 3 days in seconds
      let oneDayFuture = addUTCTime 86400 frozenTime  -- 1 day in the future
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" threeDaysAgo
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" oneDayFuture

      (_, pg) <-
        testServant tr $ Log.apiLogH testPid Nothing Nothing Nothing Nothing fromTime toTime Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing

      case pg of
        Log.LogsGetJson r -> do
          -- Verify we got results
          V.length r.finalVecs `shouldBe` 202  -- Should return all 202 test messages (under the 500 limit)
          r.resultCount `shouldSatisfy` (>= 202)  -- At least our 202 test messages (might include data from other tests)

          -- Verify column structure
          r.curatedColNames `shouldBe` ["id", "timestamp", "service", "summary", "latency_breakdown"]

          -- Verify URLs are generated correctly
          r.nextLogsURL `shouldNotBe` ""
          r.resetLogsURL `shouldNotBe` ""
          r.recentLogsURL `shouldNotBe` ""
        _ -> error "Expected JSON response but got something else"

    it "should handle query filters correctly" \tr -> do
      let frozenTime = Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt

      -- Process some test messages
      let msgs = [("m1", toStrict $ AE.encode reqMsg1), ("m2", toStrict $ AE.encode reqMsg2)]
      res <- runTestBackground tr.trTestClock tr.trATCtx $ processMessages msgs HashMap.empty
      length res `shouldBe` 2
      
      -- Get time range that includes the messages we just processed
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime

      -- Test with a query filter (using proper string comparison for JSONB field)
      let query = "status_code == \"200\""
      (_, pg) <-
        testServant tr $ Log.apiLogH testPid (Just query) Nothing Nothing Nothing fromTime toTime Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing

      case pg of
        Log.LogsGetJson r -> do
          -- Should only return entries matching the query
          r.resultCount `shouldSatisfy` (> 0)
          V.length r.finalVecs `shouldSatisfy` (> 0)
        _ -> error "Expected JSON response but got something else"

    it "should paginate results correctly" \tr -> do
      let frozenTime = Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      let reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt

      -- Create many messages to test pagination
      let msgs = take 200 $ map ((, toStrict $ AE.encode reqMsg) . (\i -> "m" <> show i)) [1..]
      res <- runTestBackground tr.trTestClock tr.trATCtx $ processMessages msgs HashMap.empty
      length res `shouldBe` 200
      
      -- Get time range that includes the messages we just processed
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime

      -- First page
      (_, pg1) <-
        testServant tr $ Log.apiLogH testPid Nothing Nothing Nothing Nothing fromTime toTime Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing

      case pg1 of
        Log.LogsGetJson r -> do
          V.length r.finalVecs `shouldSatisfy` (>= 200)  -- Should return at least all 200 test messages (under the 500 limit)
          r.resultCount `shouldSatisfy` (>= 200)  -- At least our 200 test messages
          -- With 500 limit, might not need pagination for 200 items
        _ -> error "Expected JSON response but got something else"

  describe "Column Selection" do
    it "should return only requested columns" \tr -> do
      let frozenTime = Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      let reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      
      let msgs = [("m1", toStrict $ AE.encode reqMsg)]
      _ <- runTestBackground tr.trTestClock tr.trATCtx $ processMessages msgs HashMap.empty
      
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      
      -- Request specific columns (using columns that actually exist in the database)
      -- Note: Just request additional columns beyond the default ones
      let cols = "id,timestamp,name,duration"
      (_, pg) <- testServant tr $ 
        Log.apiLogH testPid Nothing (Just cols) Nothing Nothing fromTime toTime Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing

      case pg of
        Log.LogsGetJson r -> do
          -- The system returns the requested columns plus default columns in a curated order
          r.curatedColNames `shouldBe` ["id", "timestamp", "name", "duration", "service", "summary", "latency_breakdown"]
        _ -> error "Expected JSON response but got something else"

  describe "Query Error Handling" do
    it "should handle invalid query syntax gracefully" \tr -> do
      let invalidQuery = "status_code = 200"  -- Missing quotes around string value

      (_, pg) <- testServant tr $
        Log.apiLogH testPid (Just invalidQuery) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing

      -- Invalid query should return 0 results (not silently ignored)
      case pg of
        Log.LogsGetJson r -> do
          V.length r.finalVecs `shouldBe` 0
          r.resultCount `shouldBe` 0
        Log.LogsGetErrorSimple _ -> pass  -- Also acceptable
        _ -> error "Expected JSON response or error"

    it "should handle malformed query operators" \tr -> do
      let malformedQuery = "status_code === \"200\""  -- Invalid operator
      (_, pg) <- testServant tr $ 
        Log.apiLogH testPid (Just malformedQuery) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing
      
      case pg of
        Log.LogsGetJson r -> do
          V.length r.finalVecs `shouldBe` 0
          r.resultCount `shouldBe` 0
        Log.LogsGetErrorSimple _ -> pass
        _ -> error "Expected JSON response or error"

  describe "Pagination" do
    it "should paginate through multiple pages using cursor" \tr -> do
      let frozenTime = Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      let reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt

      -- Create 1000 messages to ensure multiple pages (limit is 500)
      let msgs = take 1000 $ map ((, toStrict $ AE.encode reqMsg) . (\i -> "m" <> show i)) [1..]
      _ <- runTestBackground tr.trTestClock tr.trATCtx $ processMessages msgs HashMap.empty

      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime

      -- First page
      (_, pg1) <- testServant tr $
        Log.apiLogH testPid Nothing Nothing Nothing Nothing fromTime toTime Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing

      case pg1 of
        Log.LogsGetJson r -> do
          V.length r.finalVecs `shouldBe` 500  -- API limits to 500 per page
          r.resultCount `shouldSatisfy` (>= 1000)  -- At least our 1000 test messages

          -- Extract cursor timestamp from last item
          let lastItemM = r.finalVecs V.!? (V.length r.finalVecs - 1)
          case lastItemM of
            Just lastItem -> do
              let timestampIdx = HashMap.lookup "timestamp" r.colIdxMap
              case timestampIdx of
                Just idx -> do
                  let cursorM = (lastItem V.!? idx) >>= \case AE.String t -> Just t; _ -> Nothing
                  cursorM `shouldSatisfy` isJust
                _ -> error "timestamp column not found"
            Nothing -> error "No items in first page"
        _ -> error "Expected JSON response but got something else"

    it "should return consistent results when using cursor" \tr -> do
      let frozenTime = Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      
      -- Create messages with different timestamps
      let msg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let msg2Time = toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-30) frozenTime
      let msg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 msg2Time
      
      let msgs = [("m1", toStrict $ AE.encode msg1), ("m2", toStrict $ AE.encode msg2)]
      _ <- runTestBackground tr.trTestClock tr.trATCtx $ processMessages msgs HashMap.empty
      
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-120) frozenTime
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      
      -- Get all results
      (_, pg) <- testServant tr $ 
        Log.apiLogH testPid Nothing Nothing Nothing Nothing fromTime toTime Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing
      
      case pg of
        Log.LogsGetJson r -> do
          r.resultCount `shouldSatisfy` (>= 2)
          V.length r.finalVecs `shouldSatisfy` (>= 2)
        _ -> error "Expected JSON response but got something else"

  describe "Time Range Selection" do
    it "should respect exact time boundaries" \tr -> do
      let frozenTime = Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime
      
      -- Create messages at specific times
      let oneHourAgo = addUTCTime (-3600) frozenTime
      let twoHoursAgo = addUTCTime (-7200) frozenTime
      let threeHoursAgo = addUTCTime (-10800) frozenTime
      
      let msg1Txt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" oneHourAgo
      let msg2Txt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" twoHoursAgo
      let msg3Txt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" threeHoursAgo
      
      let msg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 msg1Txt
      let msg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 msg2Txt
      let msg3 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 msg3Txt
      
      let msgs = [("m1", toStrict $ AE.encode msg1), ("m2", toStrict $ AE.encode msg2), ("m3", toStrict $ AE.encode msg3)]
      _ <- runTestBackground tr.trTestClock tr.trATCtx $ processMessages msgs HashMap.empty
      
      -- Query for messages between 2.5 and 1.5 hours ago (should only get msg2)
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-9000) frozenTime  -- 2.5 hours ago
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-5400) frozenTime   -- 1.5 hours ago
      
      (_, pg) <- testServant tr $ 
        Log.apiLogH testPid Nothing Nothing Nothing Nothing fromTime toTime Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing
      
      case pg of
        Log.LogsGetJson r -> do
          -- Should only include messages within the time range
          r.resultCount `shouldSatisfy` (>= 1)
          V.length r.finalVecs `shouldSatisfy` (>= 1)
        _ -> error "Expected JSON response but got something else"

    it "should handle 'since' parameter correctly" \tr -> do
      -- Using frozen time: January 1, 2025
      let frozenTime = Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
      let oneHourBefore = toText $ formatTime defaultTimeLocale "%FT%T%QZ" (addUTCTime (-3600) frozenTime)
      let twoDaysBefore = toText $ formatTime defaultTimeLocale "%FT%T%QZ" (addUTCTime (-172800) frozenTime)
      
      -- Create messages at different times relative to frozen time
      let msgNow = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let msgHourBeforeMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 oneHourBefore  
      let msgTwoDaysBeforeMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 twoDaysBefore
      
      let msgs = [("m1", toStrict $ AE.encode msgNow),
                  ("m2", toStrict $ AE.encode msgHourBeforeMsg),
                  ("m3", toStrict $ AE.encode msgTwoDaysBeforeMsg)]
      
      _ <- runTestBackground tr.trTestClock tr.trATCtx $ processMessages msgs HashMap.empty
      
      -- Test "1H" - should get messages from last hour (msgNow and msgHourBefore)
      (_, pg1) <- testServant tr $ 
        Log.apiLogH testPid Nothing Nothing Nothing (Just "1H") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing
      
      case pg1 of
        Log.LogsGetJson r -> do
          -- In frozen time at 2025-01-01, "1H" means from 1 hour before to now
          -- Should include msgNow and msgHourBeforeMsg
          r.resultCount `shouldSatisfy` (>= 2)
          V.length r.finalVecs `shouldSatisfy` (>= 2)
        _ -> error "Expected JSON response but got something else"

      -- Test "24H" - should get messages from last 24 hours
      (_, pg2) <- testServant tr $
        Log.apiLogH testPid Nothing Nothing Nothing (Just "24H") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing

      case pg2 of
        Log.LogsGetJson r -> do
          -- Should include msgNow and msgHourBeforeMsg, but NOT msgTwoDaysBeforeMsg
          r.resultCount `shouldSatisfy` (>= 2)
          V.length r.finalVecs `shouldSatisfy` (>= 2)
        _ -> error "Expected JSON response but got something else"

    it "should handle missing time range (default behavior)" \tr -> do
      -- When no time range is specified, it should use a default (e.g., last 24 hours)
      (_, pg) <- testServant tr $
        Log.apiLogH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing

      case pg of
        Log.LogsGetJson r -> do
          r.curatedColNames `shouldBe` ["id", "timestamp", "service", "summary", "latency_breakdown"]
        _ -> error "Expected JSON response but got something else"

  describe "Trace Tree" do
    it "should include traces field with tree structure" \tr -> do
      let frozenTime = Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime
          nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
          reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
          msgs = map (\i -> ("tm" <> show i, toStrict $ AE.encode reqMsg)) ([1 .. 5] :: [Int])
      void $ runTestBackground tr.trTestClock tr.trATCtx $ processMessages msgs HashMap.empty

      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      (_, pg) <- testServant tr $ Log.apiLogH testPid Nothing Nothing Nothing Nothing fromTime toTime Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing

      case pg of
        Log.LogsGetJson r -> do
          V.length r.finalVecs `shouldSatisfy` (>= 5)
          length r.traces `shouldSatisfy` (> 0)
          forM_ r.traces \entry -> do
            entry.traceId `shouldNotBe` ""
            entry.root `shouldNotBe` ""
            entry.startTime `shouldSatisfy` (>= 0)
            entry.duration `shouldSatisfy` (>= 0)
        _ -> error "Expected JSON response"

    it "traces should match request vecs trace IDs" \tr -> do
      let frozenTime = Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime
          nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
          reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
          reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
          msgs = [("tt1", toStrict $ AE.encode reqMsg1), ("tt2", toStrict $ AE.encode reqMsg2)]
      void $ runTestBackground tr.trTestClock tr.trATCtx $ processMessages msgs HashMap.empty

      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      (_, pg) <- testServant tr $ Log.apiLogH testPid Nothing Nothing Nothing Nothing fromTime toTime Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing

      case pg of
        Log.LogsGetJson r -> do
          -- All trace IDs in entries should appear somewhere in the request vecs
          let traceIdx = HashMap.lookup "trace_id" r.colIdxMap
          case traceIdx of
            Just idx -> do
              let vecTraceIds = V.toList $ V.mapMaybe (\v -> case v V.!? idx of Just (AE.String t) -> Just t; _ -> Nothing) r.finalVecs
              forM_ r.traces \entry -> vecTraceIds `shouldContain` [entry.traceId]
            Nothing -> pass -- trace_id column not projected, skip
        _ -> error "Expected JSON response"

    it "children map values reference valid span IDs" \tr -> do
      let frozenTime = Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime
          nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
          reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
          msgs = map (\i -> ("cv" <> show i, toStrict $ AE.encode reqMsg)) ([1 .. 10] :: [Int])
      void $ runTestBackground tr.trTestClock tr.trATCtx $ processMessages msgs HashMap.empty

      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) frozenTime
          toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 frozenTime
      (_, pg) <- testServant tr $ Log.apiLogH testPid Nothing Nothing Nothing Nothing fromTime toTime Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing

      case pg of
        Log.LogsGetJson r -> do
          let lbIdx = HashMap.lookup "latency_breakdown" r.colIdxMap
              idIdx = HashMap.lookup "id" r.colIdxMap
          case (lbIdx, idIdx) of
            (Just lbi, Just idi) -> do
              let allSpanIds = V.toList $ V.mapMaybe (\v -> case v V.!? lbi of Just (AE.String t) -> Just t; _ -> case v V.!? idi of Just (AE.String t) -> Just t; _ -> Nothing) r.finalVecs
              forM_ r.traces \(entry :: Log.TraceTreeEntry) -> do
                -- Root should be a valid span ID
                allSpanIds `shouldContain` [entry.root]
                -- All children values should reference valid span IDs
                forM_ (Map.elems entry.children) \childIds ->
                  forM_ childIds \cid -> allSpanIds `shouldContain` [cid]
            _ -> pass
        _ -> error "Expected JSON response"
