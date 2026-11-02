-- | Session replay end to end: the SDK's upload contract → kafka entry point (parser,
-- counters, poison) → per-session merge lease → MinIO objects, merge cron and sealed shards
-- → the player's read path.
module ReplaySpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as V
import GHC.Conc (getAllocationCounter, setAllocationCounter)
import Pages.Replay (RawJson (..), ReplayManifest (..), ReplayPayload (..), ReplayPost (..), ReplaySegment (..), ReplaySessionResp (..), buildReplayManifest, claimMergeLease, compressAndMergeReplaySessions, concatRawJsonArrays, fetchReplaySession, fetchReplayShard, firstEventTimestampRaw, mergeReplaySession, migratedReplayKey, processReplayEvents, releaseMergeLease, replayObjectPrefix, replayPostH, replaySessionGetH, sessionFileKeys, splitReplayPayload)
import Pkg.ErrorMetrics (wireTypeErrorsRef)
import Pkg.TestUtils
import System.Mem (performGC)


spec :: Spec
spec = around withTestResources do
  it "wire contracts: SDK user shapes, object layout, raw-JSON splicing and the kafka payload parser" \_ -> do
    -- The browser SDK sends user/tenant as nested objects while this endpoint expected flat
    -- fields: every recording silently showed no user. Older SDKs still send the flat shape.
    let decodeUser extra = AE.eitherDecodeStrict @ReplayPost ("{\"events\":[],\"sessionId\":\"00000000-0000-0000-0000-00000000002a\",\"timestamp\":\"2026-05-08T00:00:00Z\"" <> extra <> "}") <&> \p -> (p.userId, p.userEmail, p.userName)
    map
      decodeUser
      [ ",\"user\":{\"id\":\"u-1\",\"email\":\"a@b.c\",\"name\":\"Ada\"},\"tenant\":{\"id\":\"t-1\"}"
      , ",\"user\":{\"id\":\"u-2\",\"full_name\":\"Ada Lovelace\"}"
      , ",\"userId\":\"u-3\",\"userEmail\":\"c@d.e\",\"userName\":\"Grace\""
      , ""
      ]
      `shouldBe` map Right [(Just "u-1", Just "a@b.c", Just "Ada"), (Just "u-2", Nothing, Just "Ada Lovelace"), (Just "u-3", Just "c@d.e", Just "Grace"), (Nothing, Nothing, Nothing)]

    -- One rrweb root, then project, ISO date, session; both legacy shapes migrate onto it, idempotently.
    let at = UTCTime (fromGregorian 2026 5 8) 0
        sid = UUID.fromWords 0 0 0 42
        prefix = "rrweb/" <> UUID.toText UUID.nil <> "/2026-05-08/" <> UUID.toText sid <> "/"
    replayObjectPrefix testPid at sid `shouldBe` prefix
    map (migratedReplayKey testPid at sid) [UUID.toText sid <> "/shard-000001-10.json.gz", UUID.toText UUID.nil <> "/rrweb/2026-05-07/" <> UUID.toText sid <> "/shard-000001-10.json.gz", prefix <> "20260508T000000.json"]
      `shouldBe` [prefix <> "shard-000001-10.json.gz", prefix <> "shard-000001-10.json.gz", prefix <> "20260508T000000.json"]

    concatRawJsonArrays ["[{\"a\":1}]"] `shouldBe` "[{\"a\":1}]"
    AE.decode (concatRawJsonArrays ["[1,2,3]", "[4,5,6]"]) `shouldBe` Just ([1 .. 6] :: [Int])
    length <$> AE.decode @[Int] (concatRawJsonArrays (replicate 1000 "[1,2,3,4,5]")) `shouldBe` Just 5000

    -- The viewer splices the events bytes verbatim (never re-encoded): non-canonical spacing survives.
    let rawBytes = "[{\"type\":3,   \"timestamp\":10}]"
        envelope r = AE.decode @AE.Object (AE.encode r)
    BS.isInfixOf (BL.toStrict rawBytes) (BL.toStrict $ AE.encode ReplaySessionResp{events = RawJson rawBytes, partial = Just False, errorMsg = Nothing, meta = Nothing}) `shouldBe` True
    (envelope ReplaySessionResp{events = RawJson rawBytes, partial = Just False, errorMsg = Nothing, meta = Nothing} <&> \o -> (AEKM.lookup "partial" o, AEKM.lookup "events" o <&> \case AE.Array a -> V.length a; _ -> -1))
      `shouldBe` Just (Just (AE.Bool False), Just 1)
    (envelope ReplaySessionResp{events = RawJson "[]", partial = Nothing, errorMsg = Just "boom", meta = Nothing} <&> \o -> (AEKM.member "partial" o, AEKM.lookup "error" o))
      `shouldBe` Just (False, Just (AE.String "boom"))

    -- rrweb mutations carry empty strings; the old skipper ate the closing quote of `""`, the
    -- message was dropped and the player went black. Payloads go through AE.encode, as kafka sees them.
    let ev ty ts d = AE.object ["type" AE..= (ty :: Int), "data" AE..= AE.object d, "timestamp" AE..= (ts :: Int)]
        leaf = AE.object ["tagName" AE..= ("" :: Text), "attributes" AE..= AE.object [], "childNodes" AE..= ([] :: [AE.Value])]
        long = T.replicate 1000000 "x"
        roundTrip events = splitReplayPayload (BL.toStrict $ AE.encode $ AE.object ["events" AE..= events, "sessionId" AE..= UUID.toText sid, "projectId" AE..= UUID.toText UUID.nil, "timestamp" AE..= at]) >>= AE.eitherDecodeStrict @AE.Value . (.eventsBytes)
        payloads :: [AE.Value]
        payloads =
          [ AE.Array [ev 2 1000 ["node" AE..= AE.object []]]
          , AE.toJSON $ replicate 2 $ ev 3 1000 ["text" AE..= ("" :: Text), "tag" AE..= ("div" :: Text)]
          , AE.Array [ev 3 2000 ["text" AE..= ("]],}\"hi\\there" :: Text)]]
          , AE.Array [ev 2 3000 ["node" AE..= AE.object ["tagName" AE..= ("div" :: Text), "attributes" AE..= AE.object ["class" AE..= ("" :: Text)], "childNodes" AE..= AE.Array [leaf, leaf]]]]
          , AE.Array [AE.String (long <> "\"\\é🙂" <> long), ""]
          ]
    map roundTrip payloads `shouldBe` map Right payloads

    -- P0: the merge path needs only the first event's timestamp; a full decode realised the whole
    -- array (the largest term in the prod heap census). Revert to a full decode and the ratio
    -- collapses. Both paths are warmed first so one-time CAF init doesn't pollute the measurement.
    let big = "[" <> BS.intercalate "," (replicate 50000 ("{\"type\":3,\"timestamp\":1000,\"data\":{\"text\":\"" <> BS.replicate 64 0x78 <> "\"}}")) <> "]"
        firstTsOf bs = either (const (-1)) id $ firstEventTimestampRaw (BL.fromStrict bs)
        fullDecodeOf bs = case AE.eitherDecodeStrict bs of Right (AE.Array a) -> V.length a; _ -> 0 :: Int
        allocatedBy act = performGC >> setAllocationCounter maxBound >> act >>= evaluateWHNF >> (maxBound -) <$> getAllocationCounter
    _ <- evaluateWHNF big >> evaluateWHNF (firstTsOf big) >> evaluateWHNF (fullDecodeOf big)
    rawAlloc <- allocatedBy $ pure $ firstTsOf big
    decAlloc <- allocatedBy $ pure $ fullDecodeOf big
    firstEventTimestampRaw (BL.fromStrict big) `shouldBe` Right 1000
    (rawAlloc * 20) `shouldSatisfy` (< decAlloc)

  it "upload, kafka parsing and the merge lease: valid messages are never counted as corrupt, one merge per session" \tr -> do
    -- "warning" (with a reason) where no rrweb topic is configured or publishing fails.
    sid <- nextRandom
    for_ @[] [V.singleton (AE.object ["type" AE..= ("click" :: Text), "timestamp" AE..= (1000 :: Int)]), V.empty] \events -> do
      res <- toBaseServantResponse tr $ replayPostH testPid ReplayPost{events = AE.Array events, sessionId = sid, timestamp = frozenTime, userId = Nothing, userEmail = Nothing, userName = Nothing}
      let field k = case res of AE.Object o -> AEKM.lookup k o; _ -> Nothing
      (field "sessionId", field "status") `shouldSatisfy` \(s, st) -> s == Just (AE.toJSON sid) && st `elem` map (Just . AE.String) ["ok", "warning"]
    unknown <- call tr . replaySessionGetH testPid =<< nextRandom
    (AE.decode @AE.Object (AE.encode unknown) >>= AEKM.lookup "events") `shouldBe` Just (AE.Array V.empty)

    -- Through the kafka entry point with a project that does not exist, so the write short-
    -- circuits at the lookup: parser-valid messages (incl. empty strings) ack; corrupt and
    -- oversized go to poison for the DLQ, and only they bump their counters.
    let counters = readIORef wireTypeErrorsRef <&> \(m, _) -> [maybe 0 fst (HM.lookup k m) | k <- ["replay:decode_error", "replay:oversize_message"]]
        body events = BL.toStrict $ AE.encode $ AE.object ["events" AE..= (events :: [AE.Value]), "sessionId" AE..= sid, "projectId" AE..= ("ffffffff-ffff-ffff-ffff-ffffffffffff" :: Text), "timestamp" AE..= frozenTime]
        ev t = AE.object ["type" AE..= (3 :: Int), "data" AE..= AE.object ["text" AE..= (t :: Text), "tag" AE..= t], "timestamp" AE..= (1 :: Int)]
        process batch = do
          before <- counters
          res <- runTestBg frozenTime tr $ processReplayEvents batch HM.empty
          after <- counters
          pure (bimap sort (sort . map (\(a, _, _) -> a)) <$> rightToMaybe res, zipWith (-) after before)
    process [("ack-good", body [ev "hi"]), ("ack-empty-strings", body [ev ""]), ("ack-corrupt", "{not valid json"), ("ack-oversized", BS.replicate (101 * 1024 * 1024) 0x20)]
      `shouldReturn` (Just (["ack-empty-strings", "ack-good"], ["ack-corrupt", "ack-oversized"]), [1, 1])
    process [] `shouldReturn` (Just ([], []), [0, 0])

    -- File keys are tenant-scoped.
    let key = UUID.toText sid <> "/20260101T000000.json"
    dbExec tr [sql| INSERT INTO projects.replay_sessions (session_id, project_id, last_event_at, event_file_count, file_keys) VALUES (?, ?, now(), 1, ARRAY[?]) |] (sid, testPid, key)
    (otherPid, unknownSid) <- (,) <$> (UUIDId <$> nextRandom) <*> nextRandom
    runQueryEffect tr (traverse (uncurry sessionFileKeys) [(testPid, sid), (otherPid, sid), (testPid, unknownSid)]) `shouldReturn` [[key], [], []]

    -- 2026-07-13: ~24 concurrent merges of one 1.4 GiB session OOM'd every replica. The lease
    -- admits one merge per session; duplicates claim-and-skip; a dead worker's lease goes stale.
    let claim = runQueryEffect tr $ claimMergeLease testPid sid
    sequence [claim, claim] `shouldReturn` [True, False]
    runQueryEffect tr (releaseMergeLease testPid sid)
    claim `shouldReturn` True
    dbExec tr [sql| UPDATE projects.replay_sessions SET merge_started_at = now() - interval '1 hour' WHERE session_id = ? |] (Only sid)
    claim `shouldReturn` True

  -- The real path against MinIO: parser → object put → DB row → merge cron → shards → player read.
  it "MinIO round trip: kafka batches survive merge cron passes and shard sealing, read back in event order" \tr -> requireMinio tr pendingWith do
    project <- projectOf tr testPid
    let frozen = UTCTime (fromGregorian 2026 5 8) 0
        atSec i = UTCTime (fromGregorian 2026 5 8) (fromIntegral i)
        ev t d = AE.object ["type" AE..= (3 :: Int), "data" AE..= AE.object d, "timestamp" AE..= (t :: Int)]
        -- Distinct frozen times keep each message's object key from clobbering the previous one.
        ingest sid i events = runTestBg (atSec i) tr $ processReplayEvents [("m" <> show i, BL.toStrict $ AE.encode $ AE.object ["events" AE..= (events :: [AE.Value]), "sessionId" AE..= sid, "projectId" AE..= UUID.nil, "timestamp" AE..= frozen])] HM.empty
        -- Decoded from the serialized envelope, so the RawJson byte-splice is exercised.
        readBack sid = do
          resp <- runTestBg frozen tr $ fetchReplaySession project sid
          pure $ AE.decode @AE.Object (AE.encode resp) >>= AEKM.lookup "events"
        timestamps sid = readBack sid <&> \case Just (AE.Array a) -> [t | AE.Object o <- V.toList a, Just (AE.Number t) <- [AEKM.lookup "timestamp" o]]; _ -> []
        fileCount sid = length <$> runQueryEffect tr (sessionFileKeys testPid sid)
        merged sid = dbOne @_ @Bool tr [sql| SELECT merged FROM projects.replay_sessions WHERE session_id = ? |] (Only sid)
        cron t = void $ runTestBg t tr compressAndMergeReplaySessions
    [plain, emptyStrings, twoBatches, stranded, sealed, absent] <- replicateM 6 nextRandom

    -- Byte-equivalent round trip, including the empty-string events the black-player bug dropped.
    for_ @[] [(plain, [ev 1000 ["text" AE..= ("hello" :: Text)], ev 2000 ["text" AE..= ("world" :: Text)]]), (emptyStrings, [ev 3000 ["text" AE..= ("" :: Text), "tag" AE..= ("" :: Text)], AE.object ["type" AE..= (2 :: Int), "data" AE..= AE.object ["node" AE..= AE.object ["tagName" AE..= ("" :: Text)]], "timestamp" AE..= (4000 :: Int)]])] \(sid, events) -> do
      rightToMaybe <$> ingest sid (0 :: Int) events `shouldReturn` Just (["m0"], [])
      keys <- runQueryEffect tr $ sessionFileKeys testPid sid
      (length keys, all (replayObjectPrefix testPid (atSec (0 :: Int)) sid `T.isPrefixOf`) keys) `shouldBe` (1, True)
      readBack sid `shouldReturn` Just (AE.toJSON events)
    -- Separate kafka batches merge in event-time order.
    for_ @[] [(1, 10), (2, 20)] \(i, t) -> ingest twoBatches (i :: Int) [ev t []]
    timestamps twoBatches `shouldReturn` [10, 20]

    -- 2026-08-26: a session holding more files than one pass consumes was marked merged after
    -- its FIRST pass, stranding the rest (183 sessions, 9,999 files in prod). The cron only
    -- takes sessions idle for 30 min against its own clock, so at write time it does nothing.
    for_ [1 .. 26 :: Int] \i -> ingest stranded i [ev (i * 10) []]
    cron frozen
    (,) <$> fileCount stranded <*> merged stranded `shouldReturn` (26, False)
    let afterIdle = addUTCTime 3600 frozen
    cron afterIdle
    (,) <$> fileCount stranded <*> merged stranded `shouldReturn` (1, False)
    cron afterIdle
    (,) <$> fileCount stranded <*> merged stranded `shouldReturn` (0, True)

    -- Layer B: the merge seals byte-bounded, append-only shards (three 7 MiB batches against a
    -- 12 MiB budget seal two); the manifest lists them in order and the read stitches them.
    let pad = decodeUtf8 (BS.replicate (7 * 1024 * 1024) 0x78) :: Text
    for_ @[] [(1, 10), (2, 20), (3, 30)] \(i, t) -> ingest sealed (i :: Int) [ev t ["text" AE..= pad]]
    void $ runTestBg frozen tr $ mergeReplaySession testPid sealed
    fileCount sealed `shouldReturn` 0
    manifest <- runTestBg frozen tr $ buildReplayManifest project sealed
    -- The .gz suffix is how the read path itself decides to gunzip.
    map (\s -> (".gz" `T.isSuffixOf` s.key && "/shard-" `T.isInfixOf` s.key, s.firstTs)) manifest.segments `shouldBe` [(True, 10), (True, 30)]
    shard0 <- maybe (fail "no shard segment") (pure . (.key)) (viaNonEmpty head manifest.segments)
    RawJson raw <- runTestBg frozen tr $ fetchReplayShard project sealed (Just shard0)
    length <$> AE.decode @[AE.Value] raw `shouldBe` Just 2
    timestamps sealed `shouldReturn` [10, 20, 30]

    -- Nothing recorded: a well-formed empty answer, not an exception.
    readBack absent `shouldReturn` Just (AE.Array V.empty)
