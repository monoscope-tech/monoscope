module ReplaySpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.ByteString.Lazy qualified as BL
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.Time (UTCTime (..), fromGregorian)
import Pages.Replay (ReplayPayload (..), concatRawJsonArrays, fetchReplaySession, mergeEventArrays, processReplayEvents, sessionFileKeys, splitReplayPayload, stripJsonNullEscapes)
import Pkg.ErrorMetrics (wireTypeErrorsRef)
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, aroundAll, describe, expectationFailure, it, pendingWith, shouldBe, shouldSatisfy)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


clearReplaySessions :: TestResources -> IO ()
clearReplaySessions tr =
  void $ withPool tr.trPool $ DBT.execute
    [sql| DELETE FROM projects.replay_sessions WHERE project_id = ? |]
    (Only pid)


spec :: Spec
spec = aroundAll withTestResources do
  describe "concatRawJsonArrays" do
    it "returns empty array for no inputs" $ \_ ->
      concatRawJsonArrays [] `shouldBe` "[]"

    it "concatenates two arrays" $ \_ ->
      concatRawJsonArrays ["[1,2]", "[3,4]"] `shouldBe` "[1,2,3,4]"

    it "skips empty arrays" $ \_ ->
      concatRawJsonArrays ["[]", "[1]", "[]"] `shouldBe` "[1]"

    it "handles single non-empty array" $ \_ ->
      concatRawJsonArrays ["[{\"a\":1}]"] `shouldBe` "[{\"a\":1}]"

    it "round-trips valid JSON" $ \_ -> do
      let input = ["[1,2,3]", "[4,5,6]"]
          result = concatRawJsonArrays input
      AE.decode result `shouldBe` Just ([1, 2, 3, 4, 5, 6] :: [Int])

  describe "mergeEventArrays" do
    let ev ts = AE.object ["timestamp" AE..= (ts :: Double)]

    it "sorts arrays by first event timestamp" $ \_ ->
      mergeEventArrays [V.fromList [ev 5], V.fromList [ev 1], V.fromList [ev 3]]
        `shouldBe` V.fromList [ev 1, ev 3, ev 5]

    it "excludes empty arrays" $ \_ ->
      mergeEventArrays [V.empty, V.fromList [ev 2]] `shouldBe` V.fromList [ev 2]

    it "returns empty for all-empty input" $ \_ ->
      mergeEventArrays [V.empty, V.empty] `shouldBe` V.empty

  describe "sessionFileKeys" do
    it "returns empty list for unknown session" $ \tr -> do
      keys <- runQueryEffect tr $ sessionFileKeys pid UUID.nil
      keys `shouldBe` []

    it "returns tracked file keys after insert" $ \tr -> do
      clearReplaySessions tr
      let sid = UUID.nil
          key1 = UUID.toText sid <> "/20260101T000000.json"
      void $ withPool tr.trPool $ DBT.execute
        [sql|
          INSERT INTO projects.replay_sessions
            (session_id, project_id, last_event_at, event_file_count, file_keys)
          VALUES (?, ?, now(), 1, ARRAY[?]::text[])
          ON CONFLICT (session_id) DO NOTHING
        |]
        (sid, pid, key1)
      keys <- runQueryEffect tr $ sessionFileKeys pid sid
      keys `shouldBe` [key1]
      clearReplaySessions tr

  describe "compressAndMergeReplaySessions batch selection" do
    it "picks up sessions older than 30 min with merged=FALSE" $ \tr -> do
      clearReplaySessions tr
      let sid = UUID.fromWords 0 0 0 1
      void $ withPool tr.trPool $ DBT.execute
        [sql|
          INSERT INTO projects.replay_sessions
            (session_id, project_id, last_event_at, event_file_count, merged)
          VALUES (?, ?, now() - interval '2 hours', 0, FALSE)
          ON CONFLICT (session_id) DO NOTHING
        |]
        (sid, pid)
      rows :: V.Vector (UUID.UUID, Int) <- withPool tr.trPool $ DBT.query
        [sql|
          SELECT session_id, event_file_count
          FROM projects.replay_sessions
          WHERE merged = FALSE
            AND last_event_at < now() - interval '30 minutes'
            AND project_id = ?
        |]
        (Only pid)
      rows `shouldSatisfy` (not . V.null)
      clearReplaySessions tr

    it "does not pick up recently active sessions" $ \tr -> do
      clearReplaySessions tr
      let sid = UUID.fromWords 0 0 0 2
      void $ withPool tr.trPool $ DBT.execute
        [sql|
          INSERT INTO projects.replay_sessions
            (session_id, project_id, last_event_at, event_file_count, merged)
          VALUES (?, ?, now(), 0, FALSE)
          ON CONFLICT (session_id) DO NOTHING
        |]
        (sid, pid)
      rows :: V.Vector (Only UUID.UUID) <- withPool tr.trPool $ DBT.query
        [sql|
          SELECT session_id FROM projects.replay_sessions
          WHERE merged = FALSE
            AND last_event_at < now() - interval '30 minutes'
            AND project_id = ?
        |]
        (Only pid)
      rows `shouldBe` V.empty
      clearReplaySessions tr

  describe "splitReplayPayload" do
    -- Reconstructs a kafka-published replay message (events + metadata).
    -- We build payloads with `AE.encode` so we exercise whatever key order
    -- aeson emits, which is exactly what kafka consumers see in prod.
    let mkPayload events =
          BL.toStrict $ AE.encode $ AE.object
            [ "events" AE..= (events :: AE.Value)
            , "sessionId" AE..= ("00000000-0000-0000-0000-000000000001" :: Text)
            , "projectId" AE..= ("00000000-0000-0000-0000-000000000002" :: Text)
            , "timestamp" AE..= ("2026-01-01T00:00:00Z" :: Text)
            ]
        roundTrip events = case splitReplayPayload (mkPayload events) of
          Left e -> error $ "splitReplayPayload failed: " <> toText e
          Right p -> AE.eitherDecodeStrict p.eventsBytes :: Either String AE.Value

    it "round-trips a single rrweb event" $ \_ -> do
      let event = AE.object ["type" AE..= (2 :: Int), "data" AE..= AE.object ["node" AE..= AE.object []], "timestamp" AE..= (1000 :: Int)]
          events = AE.toJSON ([event] :: [AE.Value]) :: AE.Value
      roundTrip events `shouldBe` Right events

    -- Regression for the black-player bug: rrweb mutation events frequently
    -- carry empty-string fields (e.g. `data.text`, removed `tagName`). The
    -- previous skipJsonString consumed the closing `"` of `""` as if it
    -- were content, the parser bailed, the message was silently dropped,
    -- and the session ended up with zero events.
    it "round-trips events containing empty strings" $ \_ -> do
      let event = AE.object
            [ "type" AE..= (3 :: Int)
            , "data" AE..= AE.object ["text" AE..= ("" :: Text), "tag" AE..= ("div" :: Text)]
            , "timestamp" AE..= (1000 :: Int)
            ]
          events = AE.toJSON ([event, event] :: [AE.Value]) :: AE.Value
      roundTrip events `shouldBe` Right events

    it "round-trips events with strings containing JSON-meaningful chars" $ \_ -> do
      let event = AE.object
            [ "type" AE..= (3 :: Int)
            , "data" AE..= AE.object ["text" AE..= ("]],}\"hi\\there" :: Text)]
            , "timestamp" AE..= (2000 :: Int)
            ]
          events = AE.toJSON ([event] :: [AE.Value]) :: AE.Value
      roundTrip events `shouldBe` Right events

    it "round-trips deeply nested rrweb DOM mutation events" $ \_ -> do
      let leaf = AE.object ["tagName" AE..= ("" :: Text), "attributes" AE..= AE.object [], "childNodes" AE..= ([] :: [AE.Value])]
          tree = AE.object ["tagName" AE..= ("div" :: Text), "attributes" AE..= AE.object ["class" AE..= ("" :: Text)], "childNodes" AE..= ([leaf, leaf] :: [AE.Value])]
          event = AE.object ["type" AE..= (2 :: Int), "data" AE..= AE.object ["node" AE..= tree], "timestamp" AE..= (3000 :: Int)]
          events = AE.toJSON ([event] :: [AE.Value]) :: AE.Value
      roundTrip events `shouldBe` Right events

    it "marks empty events arrays as empty" $ \_ -> do
      case splitReplayPayload (mkPayload (AE.toJSON ([] :: [AE.Value]))) of
        Left e -> error $ "splitReplayPayload failed: " <> toText e
        Right p -> p.eventsEmpty `shouldBe` True

    it "marks non-empty events arrays as non-empty" $ \_ -> do
      let events = AE.toJSON ([AE.object ["timestamp" AE..= (1 :: Int)]] :: [AE.Value]) :: AE.Value
      case splitReplayPayload (mkPayload events) of
        Left e -> error $ "splitReplayPayload failed: " <> toText e
        Right p -> p.eventsEmpty `shouldBe` False

    it "fails cleanly on missing events field" $ \_ -> do
      let payload = BL.toStrict $ AE.encode $ AE.object
            [ "sessionId" AE..= ("00000000-0000-0000-0000-000000000001" :: Text)
            , "projectId" AE..= ("00000000-0000-0000-0000-000000000002" :: Text)
            , "timestamp" AE..= ("2026-01-01T00:00:00Z" :: Text)
            ]
      case splitReplayPayload payload of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected splitReplayPayload to fail on missing events"

  describe "stripJsonNullEscapes" do
    it "is identity on input without null escapes" $ \_ ->
      stripJsonNullEscapes "{\"a\":\"b\"}" `shouldBe` "{\"a\":\"b\"}"

    it "removes embedded null escapes without breaking surrounding string" $ \_ ->
      stripJsonNullEscapes "{\"x\":\"a\\u0000b\"}" `shouldBe` "{\"x\":\"ab\"}"

    it "preserves byte boundaries (no off-by-one)" $ \_ ->
      BS.length (stripJsonNullEscapes "abc\\u0000def") `shouldBe` 6

  describe "processReplayEvents (e2e)" do
    -- End-to-end through the kafka batch entry point. Uses a nonexistent
    -- project_id so `saveReplayMinio` short-circuits in the project-lookup
    -- path (returns ackId) — we don't need a live MinIO to verify that
    -- valid messages aren't dropped at the parser. The regression we're
    -- guarding against: empty-string fields inside rrweb events used to
    -- bump `replay:decode_error` and ack the message as "garbage". With
    -- the fix, only genuinely-corrupt JSON should hit that counter.
    let nonexistentPid = "ffffffff-ffff-ffff-ffff-ffffffffffff" :: Text
        mkBody events =
          BL.toStrict $ AE.encode $ AE.object
            [ "events" AE..= (events :: AE.Value)
            , "sessionId" AE..= ("00000000-0000-0000-0000-000000000099" :: Text)
            , "projectId" AE..= nonexistentPid
            , "timestamp" AE..= ("2026-05-08T00:00:00Z" :: Text)
            ]
        readCounter k = do
          (m, _) <- readIORef wireTypeErrorsRef
          pure $ maybe 0 fst $ HM.lookup k m
        oversize = BS.replicate (11 * 1024 * 1024) 0x20  -- 11 MB of spaces
        evNonEmpty = AE.toJSON ([AE.object ["type" AE..= (3 :: Int), "data" AE..= AE.object ["text" AE..= ("hi" :: Text)], "timestamp" AE..= (1 :: Int)]] :: [AE.Value]) :: AE.Value
        evEmptyStr = AE.toJSON ([AE.object ["type" AE..= (3 :: Int), "data" AE..= AE.object ["text" AE..= ("" :: Text), "tag" AE..= ("" :: Text)], "timestamp" AE..= (2 :: Int)]] :: [AE.Value]) :: AE.Value

    it "acks valid + corrupt; counts only the genuinely-corrupt as decode errors" $ \tr -> do
      decodeBefore <- readCounter "replay:decode_error"
      oversizeBefore <- readCounter "replay:oversize_message"
      let batch =
            [ ("ack-good", mkBody evNonEmpty)
            , ("ack-empty-strings", mkBody evEmptyStr) -- regression case
            , ("ack-corrupt", "{not valid json")
            , ("ack-oversized", oversize)
            ]
          frozen = UTCTime (fromGregorian 2026 5 8) 0
      acked <- runTestBg frozen tr $ processReplayEvents batch HM.empty
      decodeAfter <- readCounter "replay:decode_error"
      oversizeAfter <- readCounter "replay:oversize_message"
      -- Parser-valid messages (good + empty-strings) AND the corrupt one are
      -- all acknowledged. Oversized is dropped pre-parse and not in the
      -- returned ack list (pre-existing behavior; redelivered by kafka).
      sort acked `shouldBe` sort ["ack-good", "ack-empty-strings", "ack-corrupt"]
      -- The load-bearing assertion: only the truly-corrupt JSON bumped the
      -- decode counter. Before the fix this would be 2.
      decodeAfter - decodeBefore `shouldBe` 1
      oversizeAfter - oversizeBefore `shouldBe` 1

    it "returns empty list and bumps no counters on empty input" $ \tr -> do
      decodeBefore <- readCounter "replay:decode_error"
      oversizeBefore <- readCounter "replay:oversize_message"
      let frozen = UTCTime (fromGregorian 2026 5 8) 0
      acked <- runTestBg frozen tr $ processReplayEvents [] HM.empty
      decodeAfter <- readCounter "replay:decode_error"
      oversizeAfter <- readCounter "replay:oversize_message"
      acked `shouldBe` []
      decodeAfter `shouldBe` decodeBefore
      oversizeAfter `shouldBe` oversizeBefore

  -- True end-to-end coverage against a real MinIO. The previous specs
  -- short-circuit at the project lookup so they never hit S3; these run
  -- the actual ingestion path (parser → MinIO put → DB row) and the
  -- player's read path (`replaySessionGetH` / `fetchReplaySession`) so
  -- a regression in object-keying, gzip framing, or DB column shape
  -- surfaces before deploy. Each spec calls `requireMinio` so a missing
  -- MinIO turns into a clear `pending` rather than a confusing failure.
  describe "replay round-trip (MinIO required)" do
    let frozen = UTCTime (fromGregorian 2026 5 8) 0
        nonEmptyEvents =
          AE.toJSON
            ( [ AE.object ["type" AE..= (3 :: Int), "data" AE..= AE.object ["text" AE..= ("hello" :: Text)], "timestamp" AE..= (1000 :: Int)]
              , AE.object ["type" AE..= (3 :: Int), "data" AE..= AE.object ["text" AE..= ("world" :: Text)], "timestamp" AE..= (2000 :: Int)]
              ] ::
                [AE.Value]
            ) :: AE.Value
        -- Regression payload: the bug we just fixed silently dropped these.
        emptyStringEvents =
          AE.toJSON
            ( [ AE.object ["type" AE..= (3 :: Int), "data" AE..= AE.object ["text" AE..= ("" :: Text), "tag" AE..= ("" :: Text)], "timestamp" AE..= (3000 :: Int)]
              , AE.object ["type" AE..= (2 :: Int), "data" AE..= AE.object ["node" AE..= AE.object ["tagName" AE..= ("" :: Text)]], "timestamp" AE..= (4000 :: Int)]
              ] ::
                [AE.Value]
            ) :: AE.Value
        mkBody sessionId events =
          BL.toStrict $ AE.encode $ AE.object
            [ "events" AE..= (events :: AE.Value)
            , "sessionId" AE..= UUID.toText sessionId
            , "projectId" AE..= UUID.toText UUID.nil
            , "timestamp" AE..= ("2026-05-08T00:00:00Z" :: Text)
            ]
        clearSessionRow tr sid =
          void $ withPool tr.trPool $ DBT.execute
            [sql| DELETE FROM projects.replay_sessions WHERE session_id = ? AND project_id = ? |]
            (sid, pid)
        -- Pull events from the player's-API surface: same code-path as
        -- `replaySessionGetH`, sans the cookie-session machinery (which
        -- would force every test to set up a logged-in user just to read
        -- a row it knows it just wrote).
        fetchEventsFor tr sid = do
          projM <- runQueryEffect tr $ Projects.projectById pid
          case projM of
            Nothing -> expectationFailure "test project not found" >> pure (V.empty)
            Just p -> do
              resp <- runTestBg frozen tr $ fetchReplaySession p sid
              case resp of
                AE.Object obj -> case AEKM.lookup "events" obj of
                  Just (AE.Array a) -> pure a
                  _ -> expectationFailure "response missing events array" >> pure V.empty
                _ -> expectationFailure "expected JSON object response" >> pure V.empty

    it "round-trips events posted via the kafka entry point through the player API" $ \tr ->
      requireMinio tr pendingWith $ do
        sid <- UUID.fromWords 0 0 0 100 & pure
        clearSessionRow tr sid
        acked <- runTestBg frozen tr $ processReplayEvents [("ack-1", mkBody sid nonEmptyEvents)] HM.empty
        acked `shouldBe` ["ack-1"]
        events <- fetchEventsFor tr sid
        -- Ingested array must come back byte-equivalent (rrweb relies on
        -- exact event objects + timestamp ordering).
        AE.Array events `shouldBe` nonEmptyEvents
        clearSessionRow tr sid

    it "preserves rrweb events containing empty strings end-to-end (regression)" $ \tr ->
      -- This is the test that would have caught the black-player bug at
      -- the data-flow boundary. With the buggy parser, `processReplayEvents`
      -- would drop the message via `replay:decode_error`, no S3 object
      -- would be written, and `fetchReplaySession` would return [].
      requireMinio tr pendingWith $ do
        sid <- UUID.fromWords 0 0 0 101 & pure
        clearSessionRow tr sid
        acked <- runTestBg frozen tr $ processReplayEvents [("ack-empty", mkBody sid emptyStringEvents)] HM.empty
        acked `shouldBe` ["ack-empty"]
        events <- fetchEventsFor tr sid
        AE.Array events `shouldBe` emptyStringEvents
        clearSessionRow tr sid

    it "merges multiple kafka batches into one replayable session" $ \tr ->
      requireMinio tr pendingWith $ do
        sid <- UUID.fromWords 0 0 0 102 & pure
        clearSessionRow tr sid
        let batchA = AE.toJSON ([AE.object ["type" AE..= (3 :: Int), "timestamp" AE..= (10 :: Int), "data" AE..= AE.object []]] :: [AE.Value]) :: AE.Value
            batchB = AE.toJSON ([AE.object ["type" AE..= (3 :: Int), "timestamp" AE..= (20 :: Int), "data" AE..= AE.object []]] :: [AE.Value]) :: AE.Value
            -- Distinct frozen times so the per-message S3 object keys
            -- (`<sid>/<HHMMSS%q>.json`) don't collide and the second
            -- write doesn't clobber the first.
            tA = UTCTime (fromGregorian 2026 5 8) 1
            tB = UTCTime (fromGregorian 2026 5 8) 2
        _ <- runTestBg tA tr $ processReplayEvents [("a", mkBody sid batchA)] HM.empty
        _ <- runTestBg tB tr $ processReplayEvents [("b", mkBody sid batchB)] HM.empty
        events <- fetchEventsFor tr sid
        -- Order matters for replay — getSessionEvents folds individual
        -- file_keys via mergeEventArrays which sorts by first-event ts.
        V.length events `shouldBe` 2
        let timestamps = V.toList $ V.map (\v -> case v of AE.Object o -> AEKM.lookup "timestamp" o; _ -> Nothing) events
        timestamps `shouldBe` [Just (AE.Number 10), Just (AE.Number 20)]
        clearSessionRow tr sid

    it "returns a user-facing error (not crash) for a session with no data" $ \tr ->
      requireMinio tr pendingWith $ do
        sid <- UUID.fromWords 0 0 0 999 & pure
        clearSessionRow tr sid
        events <- fetchEventsFor tr sid
        -- Empty events array + an `error` field is the player's "nothing
        -- to play" signal — the worst-case response should still be
        -- well-formed, not an exception or 500.
        events `shouldBe` V.empty

  describe "concatRawJsonArrays large input" do
    it "handles many arrays efficiently without OOM" $ \_ -> do
      let validChunk = "[1,2,3,4,5]" :: BL.ByteString
          manyChunks = replicate 1000 validChunk
          result = concatRawJsonArrays manyChunks
      -- Should produce valid JSON array of 5000 numbers
      let decoded = AE.decode result :: Maybe [Int]
      fmap length decoded `shouldBe` Just 5000
