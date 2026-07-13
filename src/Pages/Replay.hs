module Pages.Replay (replayPostH, ReplayPost (..), processReplayEvents, replaySessionGetH, fetchReplaySession, ReplaySessionResp (..), RawJson (..), compressAndMergeReplaySessions, mergeReplaySession, expireOldReplayData, concatRawJsonArrays, sessionFileKeys, splitReplayPayload, ReplayPayload (..), stripJsonNullEscapes, firstEventTimestampRaw, claimMergeLease, releaseMergeLease, ReplayManifest (..), ReplaySegment (..), replaySessionManifestGetH, replaySessionShardGetH, buildReplayManifest, fetchReplayShard) where

import Codec.Compression.GZip qualified as GZip
import Conduit (runConduit)
import Control.Exception (throwIO, try)
import Data.Aeson qualified as AE
import Data.Aeson.Encoding qualified as AEE
import Data.Aeson.Key qualified as AEKey
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Parser qualified as AEP
import Data.Aeson.Types qualified as AET
import Data.Attoparsec.ByteString qualified as AB
import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.Attoparsec.ByteString.Lazy qualified as ABL
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Conduit ((.|))
import Data.Conduit.Combinators qualified as CC
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.List (partition)
import Data.OpenApi (ToSchema)
import Data.Pool (Pool, withResource)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Connection)
import Effectful (Eff, IOE, type (:>))
import Effectful.Log (Log)
import Effectful.Reader.Static qualified
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.Minio (MinioErr (..), ServiceErr (..))
import Network.Minio qualified as Minio
import OddJobs.Job (createJob)
import Pkg.ErrorMetrics qualified as Metrics
import Pkg.Queue (chunksByBytes, publishJSONToKafka, runSharedProducer)
import Relude
import Relude.Extra.Tuple (traverseToFst)
import System.Config (AuthContext (config, jobsPool), EnvConfig (..))
import System.Logging qualified as Log
import System.Types (ATAuthCtx, ATBackgroundCtx, ATBaseCtx, DB, RespHeaders, addRespHeaders)
import UnliftIO.Exception (finally, tryAny)


data ReplayPost = ReplayPost
  { events :: AE.Value
  , sessionId :: UUID.UUID
  , timestamp :: UTCTime
  , userId :: Maybe Text
  , userEmail :: Maybe Text
  , userName :: Maybe Text
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON, ToSchema)


-- Helper function to publish to Pub/Sub using the queue service
publishReplayEvent :: ReplayPost -> Projects.ProjectId -> ATBaseCtx (Either Text ())
publishReplayEvent replayData pid = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let messagePayload = AE.object ["events" AE..= replayData.events, "sessionId" AE..= replayData.sessionId, "projectId" AE..= pid, "timestamp" AE..= replayData.timestamp, "userId" AE..= replayData.userId, "userEmail" AE..= replayData.userEmail, "userName" AE..= replayData.userName]
      attributes = HM.fromList [("eventType", "replay")]
  case ctx.config.rrwebTopics of
    [] -> pure $ Left "No rrweb pubsub topics configured"
    (topicName : _) -> first ("Failed to publish replay event: " <>) <$> runSharedProducer ctx (publishJSONToKafka topicName messagePayload attributes)


replayPostH :: Projects.ProjectId -> ReplayPost -> ATBaseCtx AE.Value
replayPostH pid body = do
  pubResult <- publishReplayEvent body pid
  pure $ AE.object $ ["sessionId" AE..= body.sessionId] <> case pubResult of
    Left errMsg -> ["status" AE..= ("warning" :: Text), "message" AE..= errMsg]
    Right () -> ["status" AE..= ("ok" :: Text)]


-- | Replay events are multi-MB rrweb session payloads; the kafka batch sees
-- 100s at a time. Without bounds, a single batch can pin 10+ GB after JSON
-- decode. These caps target average-case heap behaviour:
--
-- * `maxReplayMessageBytes` — hard ceiling per kafka message, before any
--   decode. Anything larger is from a buggy / non-chunking SDK and is dropped
--   with a metric instead of allocating a multi-MB Value.
-- * `replayBatchByteBudget` — soft ceiling for total bytes processed in one
--   `processReplayEvents` call. We chunk the message list and run each chunk
--   strictly, letting GC reclaim between chunks.
-- 100 MiB so full session-replay payloads land (2026-07-10: the old 10 MiB cap
-- parked a steady drip of 10–41 MB rrweb blobs as replay:oversize_message
-- poison). Kept below the 120 MiB broker/transport cap so a >100 MiB payload is
-- dropped here (cheap metric) rather than wedging a partition.
maxReplayMessageBytes :: Int
maxReplayMessageBytes = 100 * 1024 * 1024


replayBatchByteBudget :: Int
replayBatchByteBudget = 64 * 1024 * 1024


-- | Strip the JSON-encoded null escape sequence (`\u0000`) directly on a
-- ByteString. Avoids the three-pass decodeUtf8/replace/encodeUtf8 round-trip
-- that allocated 3× the message size per event. Single linear scan via
-- `BS.breakSubstring` followed by a strict left fold over the resulting
-- chunks; for the common case (no nulls) it returns the input unchanged.
--
-- >>> stripJsonNullEscapes "ab"
-- "ab"
-- >>> stripJsonNullEscapes "ab\\u0000cd"
-- "abcd"
-- >>> stripJsonNullEscapes "\\u0000\\u0000end"
-- "end"
-- >>> stripJsonNullEscapes "no\\u0000tabs\\u0000here"
-- "notabshere"
stripJsonNullEscapes :: BS.ByteString -> BS.ByteString
stripJsonNullEscapes bs0 = case BS.breakSubstring needle bs0 of
  (_, rest) | BS.null rest -> bs0
  (pre, rest) -> BS.concat (pre : go (BS.drop 6 rest))
  where
    needle = "\\u0000"
    go bs = case BS.breakSubstring needle bs of
      (pre, rest) | BS.null rest -> [pre]
      (pre, rest) -> pre : go (BS.drop 6 rest)


-- | Lightweight metadata extracted from a published replay message, plus the
-- raw JSON bytes for the events array. We deliberately do NOT parse `events`
-- into an `AE.Value` — for typical sessions that's 90%+ of the heap cost.
data ReplayPayload = ReplayPayload
  { sessionId :: !UUID.UUID
  , projectId :: !Projects.ProjectId
  , timestamp :: !UTCTime
  , userId :: !(Maybe Text)
  , userEmail :: !(Maybe Text)
  , userName :: !(Maybe Text)
  , eventsBytes :: !BS.ByteString
  , eventsEmpty :: !Bool
  }


data ReplayMeta = ReplayMeta
  { sessionId :: UUID.UUID
  , projectId :: Projects.ProjectId
  , timestamp :: UTCTime
  , userId :: Maybe Text
  , userEmail :: Maybe Text
  , userName :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


-- | Walk a published replay JSON object byte-by-byte, capturing the raw byte
-- range of `events` and aeson-decoding the small remaining metadata fields.
-- This is the load-bearing optimisation: an SDK session with thousands of
-- DOM-mutation events parses in O(bytes) here vs. O(bytes × ADT-overhead) for
-- a full `AE.eitherDecode`, since we never realize `events` as `Value`.
--
-- >>> let go = fmap (\p -> (p.eventsBytes, p.eventsEmpty)) . splitReplayPayload
-- >>> go "{\"sessionId\":\"00000000-0000-0000-0000-000000000001\",\"projectId\":\"00000000-0000-0000-0000-000000000002\",\"timestamp\":\"2026-01-01T00:00:00Z\",\"events\":[1,2]}"
-- Right ("[1,2]",False)
-- >>> go "{\"events\":[ ],\"sessionId\":\"00000000-0000-0000-0000-000000000001\",\"projectId\":\"00000000-0000-0000-0000-000000000002\",\"timestamp\":\"2026-01-01T00:00:00Z\"}"
-- Right ("[ ]",True)
-- >>> isLeft (splitReplayPayload "{\"sessionId\":\"x\"}")
-- True
splitReplayPayload :: BS.ByteString -> Either String ReplayPayload
splitReplayPayload = AB.parseOnly $ do
  AC.skipSpace
  _ <- AC.char '{'
  AC.skipSpace
  -- Empty object is invalid: must contain `events` plus metadata.
  AC.peekChar' >>= \case
    '}' -> fail "empty object"
    _ -> go KM.empty Nothing
  where
    go !meta !mEv = do
      AC.skipSpace
      k <- AEP.jstring
      AC.skipSpace
      _ <- AC.char ':'
      AC.skipSpace
      (meta', mEv') <-
        if k == "events"
          then do
            (raw, _) <- AC.match skipJsonValue
            pure (meta, Just raw)
          else do
            v <- AEP.json'
            pure (KM.insert (AEKey.fromText k) v meta, mEv)
      AC.skipSpace
      AC.anyChar >>= \case
        ',' -> go meta' mEv'
        '}' -> finalize meta' mEv'
        c -> fail $ "expected , or } in object, got " <> [c]
    finalize meta mEv = case mEv of
      Nothing -> fail "missing events field"
      Just raw -> case AE.fromJSON (AE.Object meta) of
        AE.Error e -> fail e
        AE.Success ReplayMeta{..} ->
          pure ReplayPayload{eventsBytes = raw, eventsEmpty = isEmptyJsonArray (fromStrict raw), ..}


-- | True if a JSON array byte-slice contains zero elements, regardless of
-- whitespace padding. Cheap two-sided whitespace-trim; we don't need to parse.
--
-- >>> isEmptyJsonArray "[]"
-- True
-- >>> isEmptyJsonArray "[ \t\n]"
-- True
-- >>> isEmptyJsonArray "[1]"
-- False
-- >>> isEmptyJsonArray ""
-- True
isEmptyJsonArray :: BL.ByteString -> Bool
isEmptyJsonArray raw = case BL.uncons (BL.dropWhile isWs raw) of
  Nothing -> True
  Just (0x5b, t) -> case BL.uncons (BL.dropWhile isWs t) of
    Just (0x5d, rest) -> BL.all isWs rest
    _ -> False
  _ -> False
  where
    isWs c = c == 0x20 || c == 0x09 || c == 0x0a || c == 0x0d


-- | Skip one JSON value without allocating it; bracket/quote-aware so we
-- don't get confused by `]` or `,` inside strings or nested arrays. Used with
-- `AC.match` to capture the raw bytes of the events array. The primitive
-- branch (numbers/bool/null) consumes until a structural char so the caller
-- can match a following `,`/`}`/`]` without backtracking.
--
-- >>> AB.parseOnly (fst <$> AC.match skipJsonValue) "[1,2,3] tail"
-- Right "[1,2,3]"
-- >>> AB.parseOnly (fst <$> AC.match skipJsonValue) "{\"a\":[1,{\"b\":2}],\"c\":\"]]}\"}"
-- Right "{\"a\":[1,{\"b\":2}],\"c\":\"]]}\"}"
-- >>> AB.parseOnly (fst <$> AC.match skipJsonValue) "\"escaped \\\"quote\\\" inside\""
-- Right "\"escaped \\\"quote\\\" inside\""
-- >>> AB.parseOnly (fst <$> AC.match skipJsonValue) "[\"\",{\"x\":\"\"},\"a\"] tail"
-- Right "[\"\",{\"x\":\"\"},\"a\"]"
skipJsonValue :: AC.Parser ()
skipJsonValue = do
  AC.skipSpace
  c <- AC.peekChar'
  case c of
    '{' -> skipBracketed '{' '}'
    '[' -> skipBracketed '[' ']'
    '"' -> AC.anyChar *> skipStringBody
    _ -> void $ AC.takeWhile1 (\ch -> ch `notElem` [',' :: Char, '}', ']', ' ', '\t', '\n', '\r'])


skipBracketed :: Char -> Char -> AC.Parser ()
skipBracketed open close = AC.anyChar *> loop (1 :: Int)
  where
    loop 0 = pass
    loop n = do
      _ <- AB.takeWhile (\b -> b /= o && b /= c && b /= q)
      AC.anyChar >>= \case
        ch | ch == open -> loop (n + 1)
        ch | ch == close -> loop (n - 1)
        -- Opening `"` already consumed; jump straight into the body so
        -- empty strings (`""`) terminate immediately instead of swallowing
        -- the closing `"` as if it were content.
        '"' -> skipStringBody *> loop n
        _ -> loop n
    o = fromIntegral (fromEnum open)
    c = fromIntegral (fromEnum close)
    q = fromIntegral (fromEnum '"')


-- | Skip a JSON string's body, assuming the opening `"` has already been
-- consumed. Returns once the closing `"` is read.
--
-- >>> AB.parseOnly (skipStringBody *> AC.takeByteString) "\",rest"
-- Right ",rest"
-- >>> AB.parseOnly (skipStringBody *> AC.takeByteString) "abc\",rest"
-- Right ",rest"
-- >>> AB.parseOnly (skipStringBody *> AC.takeByteString) "a\\\"b\",rest"
-- Right ",rest"
skipStringBody :: AC.Parser ()
skipStringBody =
  AC.anyChar >>= \case
    '"' -> pass
    '\\' -> AC.anyChar *> skipStringBody
    _ -> skipStringBody


-- | Process a kafka batch of published replay messages. Sub-chunks the batch
-- by total byte size so a single call cannot pin more than
-- `replayBatchByteBudget` bytes; each chunk is processed with strict
-- per-message handoff so finished messages are GC-eligible immediately.
-- Drop counts feed `OtlpServer.bumpErrorCounter` so they roll up in the same
-- minute-cadence summary as wire/UTF-8 parse errors — no per-event log spam.
-- | Replay doesn't dual-write so it can't surface a 'Telemetry.WriteFailure'.
-- Oversize + decode-failed messages are returned as 'PoisonMsg' so Pkg.Queue
-- can DLQ their raw bytes before committing offsets (no silent drop). S3/Minio
-- transport errors still propagate as exceptions through the outer tryAny.
processReplayEvents :: [(Text, ByteString)] -> HashMap Text Text -> ATBackgroundCtx (Either Telemetry.WriteFailure ([Text], [Telemetry.PoisonMsg]))
processReplayEvents [] _ = pure (Right ([], []))
processReplayEvents msgs _attrs = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = ctx.config
      (oversized, valid) = partition (\(_, b) -> BS.length b > maxReplayMessageBytes) msgs
      oversizePoison = [(ackId, body, "replay:oversize_message") | (ackId, body) <- oversized]
  traverse_ (\_ -> Metrics.bumpErrorCounter "replay:oversize_message") oversized
  (acks, decodePoison) <- mconcat <$> mapM (handleChunk envCfg ctx.jobsPool) (chunksByBytes replayBatchByteBudget (BS.length . snd) valid)
  pure $ Right (acks, oversizePoison <> decodePoison)
  where
    -- Returns @(acks, poison)@ for one chunk so the caller can DLQ poison and
    -- ack only the successfully-saved + DLQ'd ackIds.
    handleChunk envCfg jobsPool chunk =
      mconcat <$> forM chunk \(!ackId, !body) ->
        case splitReplayPayload (stripJsonNullEscapes body) of
          Left err -> do
            Metrics.bumpErrorCounter "replay:decode_error"
            pure ([], [(ackId, body, "replay:decode_error: " <> toText err)])
          Right payload ->
            saveReplayMinio envCfg jobsPool ackId payload <&> \case
              Just acked -> ([acked], [])
              Nothing -> ([], [])


-- | True if the Minio error represents a missing object (safe to treat as empty).
--
-- >>> isNoSuchKey (MErrService NoSuchKey)
-- True
-- >>> isNoSuchKey (MErrService (ServiceErr "Other" "x"))
-- False
isNoSuchKey :: MinioErr -> Bool
isNoSuchKey (MErrService NoSuchKey) = True
isNoSuchKey _ = False


toObjKey :: Text -> Minio.Object
toObjKey = fromString . toString


mergedKeyFor :: UUID.UUID -> Minio.Object
mergedKeyFor sid = toObjKey (UUID.toText sid <> "/merged.json.gz")


sessionLogCtx :: Projects.ProjectId -> UUID.UUID -> HashMap Text Text
sessionLogCtx pid sid = HM.fromList [("session_id", UUID.toText sid), ("project_id", pid.toText)]


-- | Attach a rendered exception to a log context under the conventional
-- "error" key. Used so call sites stop repeating the displayException dance.
withError :: Exception e => e -> HashMap Text Text -> HashMap Text Text
withError e = HM.insert "error" (toText $ displayException e)


markMerged :: DB es => Projects.ProjectId -> UUID.UUID -> UTCTime -> Eff es ()
markMerged pid sid now =
  Hasql.interpExecute_ [HI.sql| UPDATE projects.replay_sessions SET merged = TRUE, updated_at = #{now} WHERE session_id = #{sid} AND project_id = #{pid} |]


-- | Atomically claim the per-session merge lease. Returns True iff this worker
-- now owns it (lease was unheld or older than the 15-minute stale window — the
-- latter reclaims a lease left behind by a worker that died mid-merge). A merge
-- pass on a large session holds multi-GB heap for ~a minute; the lease caps
-- concurrency to one per session so duplicate/queued jobs claim-and-skip instead
-- of piling on and melting the replica (2026-07-13 incident).
claimMergeLease :: DB es => Projects.ProjectId -> UUID.UUID -> Eff es Bool
claimMergeLease pid sid = do
  rows :: [UUID.UUID] <-
    Hasql.interp
      [HI.sql|
        UPDATE projects.replay_sessions SET merge_started_at = now()
        WHERE session_id = #{sid} AND project_id = #{pid}
          AND (merge_started_at IS NULL OR merge_started_at < now() - interval '15 minutes')
        RETURNING session_id
      |]
  pure $ not (null rows)


-- | Release the merge lease so the next queued job for this session can proceed.
releaseMergeLease :: DB es => Projects.ProjectId -> UUID.UUID -> Eff es ()
releaseMergeLease pid sid =
  Hasql.interpExecute_ [HI.sql| UPDATE projects.replay_sessions SET merge_started_at = NULL WHERE session_id = #{sid} AND project_id = #{pid} |]


data MergeError
  = MergeDecodeFailed [(Text, String)]
  | MergeFetchFailed MinioErr
  | MergePutFailed MinioErr
  deriving stock (Show)
  deriving anyclass (Exception)


-- | Stream one object to raw (optionally gzip-decompressed) bytes. Shared
-- transport primitive for both the viewer decode path and the byte-level merge
-- path so fetch/decompress behaviour can't drift between them.
fetchRawObject :: Minio.Bucket -> Minio.Object -> Bool -> Minio.Minio BL.ByteString
fetchRawObject bucket objKey gzipped = do
  src <- Minio.getObject bucket objKey Minio.defaultGetObjectOptions
  bs <- runConduit $ Minio.gorObjectStream src .| CC.sinkLazy
  pure $ if gzipped then GZip.decompress bs else bs


-- | Fetch one S3 object as raw decompressed bytes plus the first event's
-- timestamp (used to sort files before merging at the byte level). Unlike the
-- viewer path we do NOT decode the whole array — the decoded `AE.Array` was the
-- single largest heap term during a merge (~515MB in prod census). We only need
-- the first event's timestamp for sorting; `firstEventTimestampRaw` parses that
-- prefix and leaves the rest as raw bytes bound for the output.
fetchRawForMerge :: Minio.Bucket -> Minio.Object -> Bool -> Minio.Minio (Either String (Double, BL.ByteString))
fetchRawForMerge bucket objKey gzipped =
  traverseToFst firstEventTimestampRaw <$> fetchRawObject bucket objKey gzipped


-- | Read the first event's `timestamp` from a raw JSON-array blob WITHOUT
-- decoding the whole array. Validates the array envelope cheaply (opening `[`,
-- closing `]` — the exact shape `concatRawJsonArrays` relies on when it strips
-- brackets) then parses only the first element via lazy attoparsec, reusing
-- `eventTimestamp` so timestamp semantics match the viewer path (default 0
-- on absent field / empty array). A corrupt tail after a valid first event is
-- tolerated — the merge produces these files itself; envelope failures still
-- surface as `Left` and drive `MergeDecodeFailed`.
--
-- >>> firstEventTimestampRaw "[]"
-- Right 0.0
-- >>> firstEventTimestampRaw "[{\"timestamp\":1500},{\"timestamp\":2}]"
-- Right 1500.0
-- >>> firstEventTimestampRaw "[{\"type\":3}]"
-- Right 0.0
-- >>> firstEventTimestampRaw "[{\"timestamp\":5},!!!garbage!!!]"
-- Right 5.0
-- >>> isLeft (firstEventTimestampRaw "{\"x\":1}")
-- True
-- >>> isLeft (firstEventTimestampRaw "[{\"timestamp\":1}")
-- True
firstEventTimestampRaw :: BL.ByteString -> Either String Double
firstEventTimestampRaw raw
  | BL.null raw = Left "empty replay blob"
  | BL.last raw /= 0x5d = Left "replay array envelope: missing closing ]"
  | otherwise = maybe 0 eventTimestamp <$> ABL.eitherResult (ABL.parse prefix raw)
  where
    prefix = do
      AC.skipSpace
      _ <- AC.char '['
      AC.skipSpace
      AC.peekChar' >>= \case
        ']' -> pure Nothing
        _ -> Just <$> AEP.json'


-- | Concatenate pre-sorted raw JSON array byte strings into one flat JSON array
-- by stripping outer brackets and joining with commas. No re-encoding of events.
--
-- >>> concatRawJsonArrays []
-- "[]"
-- >>> concatRawJsonArrays ["[1,2]","[3,4]"]
-- "[1,2,3,4]"
-- >>> concatRawJsonArrays ["[]","[1]","[]"]
-- "[1]"
concatRawJsonArrays :: [BL.ByteString] -> BL.ByteString
concatRawJsonArrays bss = case mapMaybe trim bss of
  [] -> "[]"
  inner -> "[" <> BL.intercalate "," inner <> "]"
  where
    trim b
      | BL.length b > 2 = Just $ BL.take (BL.length b - 2) (BL.drop 1 b)
      | otherwise = Nothing


-- | Resolve per-project or default S3 credentials into a connection + bucket.
projectMinioConn :: EnvConfig -> Projects.Project -> (Minio.ConnectInfo, Minio.Bucket)
projectMinioConn envCfg p =
  let (acc, sec, region, bucket, endpoint) =
        maybe
          (envCfg.s3AccessKey, envCfg.s3SecretKey, envCfg.s3Region, envCfg.s3Bucket, envCfg.s3Endpoint)
          (\x -> (x.accessKey, x.secretKey, x.region, x.bucket, x.endpointUrl))
          p.s3Bucket
      creds = Minio.CredentialValue (fromString $ toString acc) (fromString $ toString sec) Nothing
      info = if T.null endpoint then Minio.awsCI else fromString $ toString endpoint
      conn = Minio.setCreds creds (Minio.setRegion (fromString $ toString region) info)
   in (conn, bucket)


-- | Fetch every tracked key individually, tolerating per-key failures (NoSuchKey is
-- silently skipped as a concurrent-merge artifact; transport and decode errors are
-- logged and the key is dropped). Returns merged arrays plus a partial-failure flag
-- that is `True` if any key was lost to a non-NoSuchKey error.
fetchIndividualsRaw
  :: (IOE :> es, Log :> es)
  => Minio.ConnectInfo
  -> Minio.Bucket
  -> [Text]
  -> HashMap Text Text
  -> Eff es ([(Double, BL.ByteString)], Bool, Int)
fetchIndividualsRaw conn bucket fileKeys logCtx = do
  perKey <- liftIO $ forM fileKeys $ \k ->
    Minio.runMinio conn (fetchRawForMerge bucket (toObjKey k) False) <&> (k,)
  let (raws, decodeErrs, transportErrs, missing) = foldr classify ([], [], [], []) perKey
      classify (_, Right (Right r)) (rs, ds, ts, ms) = (r : rs, ds, ts, ms)
      classify (k, Right (Left de)) (rs, ds, ts, ms) = (rs, (k, de) : ds, ts, ms)
      classify (k, Left me) (rs, ds, ts, ms)
        | isNoSuchKey me = (rs, ds, ts, k : ms)
        | otherwise = (rs, ds, (k, displayException me) : ts, ms)
  unless (null decodeErrs)
    $ Log.logAttention "Decode errors while reading replay event files" (HM.insert "errors" (toText $ show decodeErrs) logCtx)
  unless (null transportErrs)
    $ Log.logAttention "Transport errors while reading replay event files" (HM.insert "errors" (toText $ show transportErrs) logCtx)
  -- Tracked keys pointing at missing objects is silent data loss: the DB said
  -- the file exists, S3 disagrees. Always surface, even if some siblings survived.
  unless (null missing)
    $ Log.logAttention
      "Tracked replay file_keys missing from S3"
      (HM.insert "missing_keys" (toText $ show missing) $ HM.insert "missing_count" (show $ length missing) logCtx)
  pure (raws, not (null decodeErrs && null transportErrs), length missing)


-- | Fetch events for a session. Returns `Left` with a user-facing message on
-- unrecoverable failure, `Right` on success (including empty or partial results).
getSessionEvents :: (DB es, Log :> es) => Minio.ConnectInfo -> Projects.ProjectId -> Minio.Bucket -> UUID.UUID -> Eff es (Either Text (BL.ByteString, Bool))
getSessionEvents conn pid bucket sessionId = do
  let sessionStr = UUID.toText sessionId
      mergedKey = mergedKeyFor sessionId
      logCtx = HM.fromList [("session", sessionStr), ("projectId", pid.toText)]
      transientMsg = "Storage is temporarily unreachable. Retry in a moment — your recording is safe." :: Text
      notFoundErr = "No recorded events found for this session. The SDK may not have uploaded any events, or storage retention has expired. Session ID is below — share with support if this is unexpected." :: Text
      corruptedErr = Left "This session’s history is corrupted and can’t be replayed. Contact support with the session ID so we can investigate."
      noEvents reason = do
        Log.logAttention "No replay events found anywhere for session" (HM.insert "reason" reason logCtx)
        pure $ Left notFoundErr
      -- Legacy single-object sessions (pre file_keys migration): fetch raw, same
      -- byte-level handling as the merged path.
      legacy reason = do
        Log.logInfo "Falling back to legacy replay blob" (HM.insert "reason" reason logCtx)
        legacyRes <- liftIO $ Minio.runMinio conn $ fetchRawForMerge bucket (toObjKey (sessionStr <> ".json")) False
        case legacyRes of
          Left err | isNoSuchKey err -> noEvents "legacy_blob_missing"
          Left err -> do
            Log.logError "Failed to load legacy replay blob" (withError err logCtx)
            pure $ Left transientMsg
          Right (Left e) -> do
            Log.logError "Failed to load legacy replay blob" (HM.insert "error" (toText e) logCtx)
            pure $ Left transientMsg
          Right (Right (_, raw))
            | isEmptyJsonArray raw -> noEvents "legacy_blob_missing"
            | otherwise -> pure $ Right (raw, False)
  fileKeys <- sessionFileKeys pid sessionId
  mergedRes <- liftIO $ Minio.runMinio conn $ fetchRawForMerge bucket mergedKey True
  case mergedRes of
    -- Corrupt merged.json.gz is a data-integrity event — do NOT silently fall through
    -- to individuals/legacy, that would hide the pre-merge history from the user.
    Right (Left decodeErr) -> do
      Log.logError "Corrupt merged replay blob" (HM.insert "error" (toText decodeErr) logCtx)
      pure corruptedErr
    Left err | not (isNoSuchKey err) -> do
      Log.logError "Failed to fetch merged replay events" (withError err logCtx)
      pure $ Left transientMsg
    _ -> do
      let mergedRaw = case mergedRes of
            Right (Right r) -> [r] -- (firstTs, raw bytes)
            _ -> [] -- NoSuchKey path
            -- Sealed shards (the current storage format) sit between the legacy monolith
            -- and the unmerged individual tail. Tolerate per-shard NoSuchKey (a shard
            -- removed by a concurrent seal); each shard carries its first-event ts for ordering.
      shardKeys <- liftIO $ listShardKeys conn bucket sessionId
      shardRes <- liftIO $ forM shardKeys $ \sk -> Minio.runMinio conn (fetchRawForMerge bucket (toObjKey sk) True)
      let shardRaws = [r | Right (Right r) <- shardRes]
      (individuals, partial, missingCount) <-
        if null fileKeys then pure ([], False, 0) else fetchIndividualsRaw conn bucket fileKeys logCtx
      -- Mirror the old two-level merge: individuals collapse (sorted by first event)
      -- into one blob, which is then ordered against merged.json.gz + shards by first
      -- event. concatRawJsonArrays drops empty arrays, so `== "[]"` is the empty test.
      let indivBlob = concatRawJsonArrays $ map snd $ sortWith fst individuals
          indivEntry = [(fromRight 0 (firstEventTimestampRaw indivBlob), indivBlob) | not (null individuals)]
          combined = concatRawJsonArrays $ map snd $ sortWith fst (mergedRaw <> shardRaws <> indivEntry)
      if combined /= "[]"
        then pure $ Right (combined, partial)
        else
          if null fileKeys
            then legacy "new_or_pre_migration_session"
            else
              if partial
                then pure $ Left transientMsg
                else do
                  Log.logAttention
                    "Replay session resolved empty despite tracked file_keys"
                    ( HM.insert "file_keys_count" (show $ length fileKeys)
                        $ HM.insert "missing_count" (show missingCount) logCtx
                    )
                  pure $ Left notFoundErr


-- | Lookup the tracked object keys for an unmerged replay session.
-- Empty list if the row doesn't exist (new session) or has a NULL/empty column.
sessionFileKeys :: DB es => Projects.ProjectId -> UUID.UUID -> Eff es [Text]
sessionFileKeys pid sessionId = do
  rows :: [V.Vector Text] <- Hasql.interp [HI.sql| SELECT COALESCE(file_keys, '{}'::text[]) FROM projects.replay_sessions WHERE session_id = #{sessionId} AND project_id = #{pid} LIMIT 1 |]
  pure $ maybe [] V.toList (listToMaybe rows)


-- | Identity + timing metadata for a session, surfaced in the player header.
data SessionMeta = SessionMeta
  { userId :: Maybe Text
  , userEmail :: Maybe Text
  , userName :: Maybe Text
  , lastEventAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON, HI.DecodeRow)


sessionMetadata :: DB es => Projects.ProjectId -> UUID.UUID -> Eff es (Maybe SessionMeta)
sessionMetadata pid sessionId = do
  rows :: [SessionMeta] <-
    Hasql.interp [HI.sql| SELECT user_id, user_email, user_name, last_event_at FROM projects.replay_sessions WHERE session_id = #{sessionId} AND project_id = #{pid} LIMIT 1 |]
  pure $ listToMaybe rows


-- | One event's `timestamp` field (0 if absent / not an object). Sorts files by
-- their first event before concatenation, keeping "default 0" in one place.
eventTimestamp :: AE.Value -> Double
eventTimestamp = fromMaybe 0 . AET.parseMaybe (AE.withObject "event" (AE..: "timestamp"))


-- | Raw JSON bytes spliced verbatim into an enclosing object's encoding. Lets the
-- read path return a (potentially 100MB) events array without ever re-parsing it
-- into an `AE.Array` or re-encoding it. `toEncoding` is the hot path aeson `encode`
-- and servant use; `toJSON` is a decode fallback that should never be hit serving.
newtype RawJson = RawJson BL.ByteString
  deriving stock (Show)


instance AE.ToJSON RawJson where
  toEncoding (RawJson bs) = AEE.unsafeToEncoding (BB.lazyByteString bs)
  toJSON (RawJson bs) = fromMaybe AE.Null (AE.decode bs)


-- | Player-API payload: the events array as raw bytes plus the envelope fields the
-- viewer expects. Typed (not opaque `AE.Value`) so handlers/tests inspect it
-- structurally; the `ToJSON` splices `events` raw and matches the legacy shape.
data ReplaySessionResp = ReplaySessionResp
  { events :: RawJson
  , partial :: Maybe Bool
  , errorMsg :: Maybe Text
  , meta :: Maybe SessionMeta
  }
  deriving stock (Generic, Show)


instance AE.ToJSON ReplaySessionResp where
  toEncoding r =
    AE.pairs
      $ AEE.pair "events" (AE.toEncoding r.events)
      <> maybe mempty ("partial" AE..=) r.partial
      <> maybe mempty ("error" AE..=) r.errorMsg
      <> foldMap metaSeries r.meta
    where
      metaSeries m = "userId" AE..= m.userId <> "userEmail" AE..= m.userEmail <> "userName" AE..= m.userName <> "lastEventAt" AE..= m.lastEventAt
  toJSON r =
    AE.object
      $ ["events" AE..= r.events]
      <> maybe [] (\p -> ["partial" AE..= p]) r.partial
      <> maybe [] (\e -> ["error" AE..= e]) r.errorMsg
      <> maybe [] (\m -> ["userId" AE..= m.userId, "userEmail" AE..= m.userEmail, "userName" AE..= m.userName, "lastEventAt" AE..= m.lastEventAt]) r.meta


mergeFileCountThreshold :: Int
mergeFileCountThreshold = 14


-- | Cap on file_keys processed per merge invocation. Sessions with backlog
-- merge in multiple passes; subsequent passes are picked up by the threshold
-- trigger or the periodic cron. Bounds heap per merge to ~25 raw blobs +
-- existing merged.json.gz + concat + gzip output.
maxFilesPerMerge :: Int
maxFilesPerMerge = 25


-- | Upload the events for one replay message. Takes the events sub-tree as
-- raw JSON bytes (already validated as a JSON value by the splitter) and
-- streams them straight into MinIO with no AE.encode round-trip.
{-# ANN saveReplayMinio ("HLint: ignore Use alternative" :: String) #-}
saveReplayMinio :: (DB es, Log :> es, Time :> es) => EnvConfig -> Pool Connection -> Text -> ReplayPayload -> Eff es (Maybe Text)
saveReplayMinio envCfg jobsPool ackId payload = do
  project <- Projects.projectById payload.projectId
  case project of
    Nothing -> pure $ Just ackId
    Just p
      | payload.eventsEmpty -> pure $ Just ackId
      | otherwise -> do
          now <- Time.currentTime
          let session = UUID.toText payload.sessionId
              (conn, bucket) = projectMinioConn envCfg p
              timeStr = toText $ formatTime defaultTimeLocale "%Y%m%dT%H%M%S%q" now
              objKeyText = session <> "/" <> timeStr <> ".json"
              objKey = toObjKey objKeyText
              body = payload.eventsBytes
              bodySize = fromIntegral (BS.length body)
          res <- liftIO $ Minio.runMinio conn $ do
            Minio.putObject bucket objKey (CC.sourceLazy (BL.fromStrict body)) (Just bodySize) Minio.defaultPutObjectOptions
          case res of
            Right _ -> do
              let ReplayPayload{sessionId, projectId, userId, userEmail, userName} = payload
              countRows :: [Int] <-
                Hasql.interp
                  [HI.sql|
                INSERT INTO projects.replay_sessions (session_id, project_id, last_event_at, event_file_count, file_keys, user_id, user_email, user_name)
                VALUES (#{sessionId}, #{projectId}, #{now}, 1, ARRAY[#{objKeyText}]::text[], #{userId}, #{userEmail}, #{userName})
                ON CONFLICT (session_id) DO UPDATE SET
                  last_event_at = #{now}, merged = FALSE,
                  event_file_count = projects.replay_sessions.event_file_count + 1,
                  file_keys = array_append(projects.replay_sessions.file_keys, #{objKeyText}),
                  user_id = COALESCE(EXCLUDED.user_id, projects.replay_sessions.user_id),
                  user_email = COALESCE(EXCLUDED.user_email, projects.replay_sessions.user_email),
                  user_name = COALESCE(EXCLUDED.user_name, projects.replay_sessions.user_name),
                  updated_at = #{now}
                RETURNING event_file_count
              |]
              let fileCount = fromMaybe 0 (listToMaybe countRows)
              -- Enqueue at exact multiples of the threshold so a busy session
              -- with N events past 14/28/42/... fires at most one merge per
              -- threshold-window, rather than one per save (which under high
              -- event rates piled up dozens of duplicate merges per session).
              when (fileCount >= mergeFileCountThreshold && fileCount `mod` mergeFileCountThreshold == 0) $ do
                -- Hand-rolled payload mirrors the derived Generic ToJSON shape for BgJobs;
                -- cannot import BackgroundJobs here (BackgroundJobs imports Pages.Replay).
                let jobPayload = AE.object ["tag" AE..= ("MergeReplaySession" :: Text), "contents" AE..= ([AE.toJSON payload.projectId, AE.toJSON payload.sessionId] :: [AE.Value])]
                liftIO $ withResource jobsPool \conn' ->
                  void $ createJob conn' "background_jobs" jobPayload
              pure $ Just ackId
            Left err -> do
              Log.logAttention "Failed to save replay events to MinIO" (withError err $ HM.fromList [("session", session), ("projectId", payload.projectId.toText)])
              pure Nothing


replaySessionGetH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders ReplaySessionResp)
replaySessionGetH pid sessionId = do
  (_, p) <- Projects.sessionAndProject pid
  addRespHeaders =<< fetchReplaySession p sessionId


-- | Auth-free replay payload fetch: pulls events from S3 + metadata from DB,
-- returning the same JSON envelope the auth handler returns. Callers handle
-- their own authorization (project membership, share-link validity, etc.)
-- before invoking this. Events stay as raw bytes end-to-end — never decoded into
-- an `AE.Array` — and are spliced verbatim into the response by `RawJson`.
fetchReplaySession
  :: (DB es, Effectful.Reader.Static.Reader AuthContext :> es, Log :> es)
  => Projects.Project -> UUID.UUID -> Eff es ReplaySessionResp
fetchReplaySession p sessionId = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let pid = p.id
      (conn, bucket) = projectMinioConn ctx.config p
      sessionStr = UUID.toText sessionId
      summaryCtx = HM.fromList [("session", sessionStr), ("projectId", pid.toText)]
  meta <-
    tryAny (sessionMetadata pid sessionId)
      >>= either
        (\err -> Nothing <$ Log.logAttention "sessionMetadata lookup failed; continuing without identity" (withError err summaryCtx))
        pure
  let emptyResp userMsg = ReplaySessionResp{events = RawJson "[]", partial = Nothing, errorMsg = Just userMsg, meta}
  result <- tryAny $ getSessionEvents conn pid bucket sessionId
  case result of
    Right (Right (replayEvents, partial)) -> do
      Log.logInfo
        "Replay session served"
        (summaryCtx <> HM.fromList [("event_bytes", show $ BL.length replayEvents), ("partial", show partial), ("outcome", "ok")])
      pure ReplaySessionResp{events = RawJson replayEvents, partial = Just partial, errorMsg = Nothing, meta}
    Right (Left userMsg) -> do
      Log.logInfo "Replay session served (user-facing error)" (summaryCtx <> HM.fromList [("outcome", "user_error"), ("user_msg", userMsg)])
      pure $ emptyResp userMsg
    Left err -> do
      Log.logAttention "Unexpected exception fetching replay session" (withError err summaryCtx)
      pure $ emptyResp "We couldn’t load this session right now. Check your connection and retry — if it keeps failing, copy the session ID and share it with support."


-- | One fetchable segment of a session's recording, oldest→newest. `key` is the
-- storage object key (always under "<sid>/"); the shard endpoint validates that
-- prefix before streaming. `firstTs` (first event's ms-epoch timestamp) lets the
-- player pick which segment covers a seek target without downloading it.
data ReplaySegment = ReplaySegment
  { key :: Text
  , firstTs :: Double
  , gzipped :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON)


-- | Progressive-player manifest: the ordered segment list plus session identity.
-- The player fetches this once, plays segment 0 immediately, and lazy-loads the
-- rest via the shard endpoint as playback advances or on seek — bounding both
-- server and browser memory regardless of session length.
data ReplayManifest = ReplayManifest
  { segments :: [ReplaySegment]
  , meta :: Maybe SessionMeta
  , errorMsg :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON)


-- | Does an object exist? Prefix listing on the exact key — no blob read.
objectExists :: Minio.ConnectInfo -> Minio.Bucket -> Text -> IO Bool
objectExists conn bucket key = do
  res <- Minio.runMinio conn $ runConduit $ Minio.listObjects bucket (Just key) True .| CC.sinkList
  pure $ either (const False) (\items -> not (null [() | Minio.ListItemObject _ <- items])) res


-- | Build the progressive manifest, oldest→newest: the legacy monolith (if any),
-- then sealed shards by index, then the unmerged individual tail. Listing + DB
-- only — never opens a blob, so it's cheap regardless of session size.
buildReplayManifest
  :: (DB es, Effectful.Reader.Static.Reader AuthContext :> es, Log :> es)
  => Projects.Project -> UUID.UUID -> Eff es ReplayManifest
buildReplayManifest p sessionId = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let pid = p.id
      (conn, bucket) = projectMinioConn ctx.config p
      summaryCtx = HM.fromList [("session", UUID.toText sessionId), ("projectId", pid.toText)]
  meta <-
    tryAny (sessionMetadata pid sessionId)
      >>= either (\err -> Nothing <$ Log.logAttention "sessionMetadata lookup failed; continuing without identity" (withError err summaryCtx)) pure
  shardKeys <- liftIO $ listShardKeys conn bucket sessionId
  let shardSegs =
        map snd
          $ sortOn
            fst
            [(idx, ReplaySegment{key = k, firstTs = fromIntegral tsMs, gzipped = True}) | k <- shardKeys, Just (idx, tsMs) <- [parseShardKey k]]
      tailTs = maybe 0 (.firstTs) (viaNonEmpty last shardSegs) -- keep the unmerged tail sorting after all shards
  legacyExists <- liftIO $ objectExists conn bucket (UUID.toText sessionId <> "/merged.json.gz")
  tailKeys <- sessionFileKeys pid sessionId
  let legacySeg = [ReplaySegment{key = UUID.toText sessionId <> "/merged.json.gz", firstTs = 0, gzipped = True} | legacyExists]
      tailSegs = [ReplaySegment{key = k, firstTs = tailTs, gzipped = False} | k <- tailKeys]
      segs = legacySeg <> shardSegs <> tailSegs
  pure ReplayManifest{segments = segs, meta, errorMsg = if null segs then Just "No recorded events found for this session." else Nothing}


replaySessionManifestGetH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders ReplayManifest)
replaySessionManifestGetH pid sessionId = do
  (_, p) <- Projects.sessionAndProject pid
  addRespHeaders =<< buildReplayManifest p sessionId


-- | Stream one manifest segment's events as a raw JSON array. Validates the key
-- is scoped to this session's prefix so a crafted key can't read another
-- session's (or another bucket's) objects.
fetchReplayShard
  :: (Effectful.Reader.Static.Reader AuthContext :> es, IOE :> es, Log :> es)
  => Projects.Project -> UUID.UUID -> Maybe Text -> Eff es RawJson
fetchReplayShard p sessionId mkey = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let (conn, bucket) = projectMinioConn ctx.config p
      prefix = UUID.toText sessionId <> "/"
  case mkey of
    Just k | prefix `T.isPrefixOf` k -> do
      res <- liftIO $ Minio.runMinio conn $ fetchRawObject bucket (toObjKey k) (".gz" `T.isSuffixOf` k)
      case res of
        Right raw -> pure $ RawJson raw
        Left err -> RawJson "[]" <$ Log.logAttention "Replay shard fetch failed" (withError err (HM.fromList [("key", k)]))
    _ -> pure $ RawJson "[]"


replaySessionShardGetH :: Projects.ProjectId -> UUID.UUID -> Maybe Text -> ATAuthCtx (RespHeaders RawJson)
replaySessionShardGetH pid sessionId mkey = do
  (_, p) <- Projects.sessionAndProject pid
  addRespHeaders =<< fetchReplayShard p sessionId mkey


-- | Merge a specific replay session's S3 event files (triggered when file count exceeds threshold)
mergeReplaySession :: Projects.ProjectId -> UUID.UUID -> ATBackgroundCtx ()
mergeReplaySession pid sessionId = mergeOneSessionByKeys pid sessionId (Just "(file count threshold)") $ const pass


-- | Find inactive sessions and compress & merge their S3 event files
compressAndMergeReplaySessions :: ATBackgroundCtx ()
compressAndMergeReplaySessions = do
  now <- Time.currentTime
  sessions :: [(UUID.UUID, Projects.ProjectId)] <-
    Hasql.interp
      [HI.sql|
    SELECT session_id, project_id FROM projects.replay_sessions
    WHERE merged = FALSE AND last_event_at < #{now}::timestamptz - interval '30 minutes'
    LIMIT 100
  |]

  Log.logInfo "Starting replay session merge job" ("sessions_count", length sessions)

  forM_ sessions $ \(sessionId, projectId) -> do
    r <- tryAny $ mergeOneSessionByKeys projectId sessionId Nothing $ \_ -> markMerged projectId sessionId now
    whenLeft_ r $ \err ->
      Log.logAttention "Replay session merge threw; continuing batch" (withError err (sessionLogCtx projectId sessionId))


-- | Look up a project + its S3 settings + tracked file keys and merge them into merged.json.gz.
-- Runs the extra DB finalization step after the S3 merge succeeds.
mergeOneSessionByKeys
  :: Projects.ProjectId
  -> UUID.UUID
  -> Maybe Text
  -> (UTCTime -> ATBackgroundCtx ())
  -> ATBackgroundCtx ()
mergeOneSessionByKeys pid sessionId logSuffix afterMerge = do
  projectM <- Projects.projectById pid
  case projectM of
    Nothing -> do
      -- Orphaned row (project gone). Flip merged=TRUE so we stop re-picking it every run.
      Log.logAttention "Project not found for replay session merge; marking merged" logCtx
      now' <- Time.currentTime
      markMerged pid sessionId now'
    Just p -> do
      claimed <- claimMergeLease pid sessionId
      if not claimed
        then Log.logInfo "Replay merge already in progress for session; skipping duplicate" logCtx
        else mergeUnderLease p `finally` releaseMergeLease pid sessionId
  where
    logCtx = sessionLogCtx pid sessionId
    mergeUnderLease p = do
      ctx <- Effectful.Reader.Static.ask @AuthContext
      let (s3Conn, bucket) = projectMinioConn ctx.config p
      allFileKeys <- sessionFileKeys pid sessionId
      let fileKeys = take maxFilesPerMerge allFileKeys
      if null fileKeys
        then do
          -- No tracked files: reset counter so the threshold path doesn't re-trigger
          -- on a stale value, then finalize.
          now' <- Time.currentTime
          Hasql.interpExecute_ [HI.sql| UPDATE projects.replay_sessions SET event_file_count = 0, updated_at = #{now'} WHERE session_id = #{sessionId} AND project_id = #{pid} |]
          afterMerge now'
          Log.logInfo "Replay session merge skipped (no tracked file keys)" ("session_id", UUID.toText sessionId)
        else do
          result <- liftIO $ try @MergeError $ mergeOneSession s3Conn bucket sessionId fileKeys
          case result of
            Left merr -> do
              let ctxWithErr = withError merr logCtx
              case merr of
                -- Fatal: corrupt data will never succeed. Mark merged so we stop
                -- retrying forever and paging on-call.
                MergeDecodeFailed _ -> do
                  Log.logError "Replay session merge aborted due to corrupt data; marking merged to stop retries" ctxWithErr
                  Time.currentTime >>= markMerged pid sessionId
                -- Transient: leave row untouched so next batch retries.
                MergeFetchFailed _ -> Log.logAttention "Replay session merge transient fetch failure; will retry" ctxWithErr
                MergePutFailed _ -> Log.logAttention "Replay session merge transient put failure; will retry" ctxWithErr
            Right _ -> do
              now' <- Time.currentTime
              -- Only remove keys we actually merged; concurrent `array_append` writers
              -- may have added more keys between snapshot and update.
              let vFileKeys = V.fromList fileKeys
              rowsAffected <-
                Hasql.interpExecute
                  [HI.sql|
                    UPDATE projects.replay_sessions SET
                      file_keys = COALESCE(
                        (SELECT array_agg(k) FROM unnest(file_keys) AS k WHERE k <> ALL(#{vFileKeys}::text[])),
                        '{}'::text[]
                      ),
                      event_file_count = COALESCE(
                        (SELECT count(*) FROM unnest(file_keys) AS k WHERE k <> ALL(#{vFileKeys}::text[]))::bigint,
                        0
                      ),
                      updated_at = #{now'}
                    WHERE session_id = #{sessionId} AND project_id = #{pid}
                  |]
              if rowsAffected == 0
                then Log.logAttention "Post-merge UPDATE affected 0 rows; row vanished, skipping afterMerge" logCtx
                else do
                  afterMerge now'
                  Log.logInfo (maybe "Merged replay session" ("Merged replay session " <>) logSuffix) ("session_id", UUID.toText sessionId)
                  -- We capped the merge at `maxFilesPerMerge`; re-enqueue while
                  -- there's backlog so a session that fell behind drains in
                  -- bounded-heap chunks instead of waiting for the 30-min cron.
                  when (length allFileKeys > maxFilesPerMerge) $ do
                    let jobPayload = AE.object ["tag" AE..= ("MergeReplaySession" :: Text), "contents" AE..= ([AE.toJSON pid, AE.toJSON sessionId] :: [AE.Value])]
                    liftIO $ withResource ctx.jobsPool \conn' ->
                      void $ createJob conn' "background_jobs" jobPayload


-- | Uncompressed-bytes budget per sealed shard. A merge accumulates raw event
-- files up to this size, seals them into one gzip shard, releases the buffer, and
-- continues — so peak heap per merge is ~one shard, not the whole session.
-- This is the O(n²)→O(n) fix: shards are append-only and immutable, so a growing
-- session never re-reads/rewrites its accumulated history (the 2026-07-13 incident
-- had a 528 MB merged blob re-decompressed on every 25-file pass).
shardSealBytes :: Int64
shardSealBytes = 12 * 1024 * 1024


-- | Object key for a sealed shard. Encodes the shard's ordinal index (zero-padded
-- for lexical = chronological sort) and its first event's timestamp in ms, so the
-- read/manifest path can order shards and answer "which shard covers time T"
-- from a bucket listing alone, without opening any blob.
shardKeyFor :: UUID.UUID -> Int -> Integer -> Minio.Object
shardKeyFor sid idx firstTsMs =
  toObjKey $ UUID.toText sid <> "/shard-" <> T.justifyRight 6 '0' (show idx) <> "-" <> show firstTsMs <> ".json.gz"


-- | List a session's sealed shard object keys (unordered). Prefix scan only — no blob reads.
listShardKeys :: Minio.ConnectInfo -> Minio.Bucket -> UUID.UUID -> IO [Text]
listShardKeys conn bucket sid = do
  res <- Minio.runMinio conn $ runConduit $ Minio.listObjects bucket (Just (UUID.toText sid <> "/shard-")) True .| CC.sinkList
  pure $ either (const []) (\items -> [Minio.oiObject info | Minio.ListItemObject info <- items]) res


-- | Parse a shard object key into (index, firstTsMs). `<sid>/shard-<idx>-<ts>.json.gz`.
parseShardKey :: Text -> Maybe (Int, Integer)
parseShardKey key = do
  name <- T.stripSuffix ".json.gz" =<< listToMaybe (reverse (T.splitOn "/" key))
  rest <- T.stripPrefix "shard-" name
  case T.splitOn "-" rest of
    [idxT, tsT] -> (,) <$> readMaybe (toString idxT) <*> readMaybe (toString tsT)
    _ -> Nothing


-- | Seal the tracked event files into append-only, byte-bounded gzip shards.
-- Fetches files in upload (≈event) order one at a time, buffering until the size
-- budget is hit, then seals a shard and releases the buffer. Aborts (throwIO
-- MergeError) on any decode/transport failure so we never drop history; NoSuchKey
-- is tolerated (concurrent-merge / crashed-prior-run artifact). Source files are
-- removed only after their shard is durably written, so a crash mid-merge is
-- resumable (unremoved files are re-sealed next run).
mergeOneSession :: Minio.ConnectInfo -> Minio.Bucket -> UUID.UUID -> [Text] -> IO ()
mergeOneSession conn bucket sessionId fileKeys = do
  existing <- listShardKeys conn bucket sessionId
  let startIdx = 1 + foldl' max 0 (mapMaybe (fmap fst . parseShardKey) existing)
  -- Upload order ≈ event order (keys are `<sid>/<uploadTs>.json`), so sealing in
  -- key order keeps shards chronological without holding every file to sort.
  go startIdx (sort fileKeys) [] 0
  where
    -- buf :: [(firstTs, raw, sourceKey)] for the current shard.
    seal _ [] = pass
    seal idx buf = do
      let sorted = sortWith (\(ts, _, _) -> ts) buf
          firstTs = maybe 0 (\(ts, _, _) -> ts) (viaNonEmpty head sorted)
          !merged = concatRawJsonArrays [raw | (_, raw, _) <- sorted]
          !compressed = GZip.compress merged
          shardKey = shardKeyFor sessionId idx (round (firstTs :: Double))
      putRes <-
        Minio.runMinio conn
          $ Minio.putObject bucket shardKey (CC.sourceLazy compressed) (Just (BL.length compressed)) Minio.defaultPutObjectOptions
      whenLeft_ putRes (throwIO . MergePutFailed)
      -- Drop the source files this shard subsumed only after it's durably written
      -- (crash-resumable); NoSuchKey on removal is fine.
      void $ Minio.runMinio conn $ forM_ [toObjKey k | (_, _, k) <- buf] (Minio.removeObject bucket)

    go _ [] [] _ = pass
    go idx [] buf _ = seal idx buf
    go idx (k : ks) buf !bufBytes = do
      res <- Minio.runMinio conn (fetchRawForMerge bucket (toObjKey k) False)
      case res of
        Left me | isNoSuchKey me -> go idx ks buf bufBytes
        Left me -> throwIO $ MergeFetchFailed me
        Right (Left de) -> throwIO $ MergeDecodeFailed [(k, de)]
        Right (Right (ts, raw)) ->
          let buf' = (ts, raw, k) : buf
              b' = bufBytes + BL.length raw
           in if b' >= shardSealBytes
                then seal idx buf' >> go (idx + 1) ks [] 0
                else go idx ks buf' b'


-- | Advertised retention window for default-storage replay data.
replayRetentionDays :: Int
replayRetentionDays = 30


-- | Max sessions handled per `ExpireReplayData` job run. Bounds job runtime;
-- when the cap is hit, the job re-enqueues itself to drain the backlog.
expireReplayBatchSize :: Int
expireReplayBatchSize = 200


-- | Delete R2 objects + tracking rows for replay sessions older than
-- `replayRetentionDays`, scoped to projects using our default S3 bucket.
-- BYO-bucket projects (projects.s3_bucket IS NOT NULL) are excluded entirely —
-- their data lives in the customer's bucket and retention is their concern.
expireOldReplayData :: ATBackgroundCtx ()
expireOldReplayData = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  now <- Time.currentTime
  let cutoff = addUTCTime (negate $ fromIntegral replayRetentionDays * 86400) now
      batchLimit = fromIntegral expireReplayBatchSize :: Int64
  rows :: [(UUID.UUID, Projects.ProjectId, V.Vector Text)] <-
    Hasql.interp
      [HI.sql|
        SELECT rs.session_id, rs.project_id, COALESCE(rs.file_keys, '{}'::text[])
        FROM projects.replay_sessions rs
        JOIN projects.projects p ON p.id = rs.project_id
        WHERE rs.last_event_at < #{cutoff}
          AND p.s3_bucket IS NULL
        ORDER BY rs.last_event_at
        LIMIT #{batchLimit}
      |]
  Log.logInfo "Expiring old replay sessions" ("count", length rows)
  forM_ rows $ \(sid, pid, fileKeys) -> do
    projectM <- Projects.projectById pid
    whenJust projectM $ \p -> do
      let (conn, bucket) = projectMinioConn ctx.config p
          keyObjs = mergedKeyFor sid : map toObjKey (V.toList fileKeys)
          logCtx = sessionLogCtx pid sid
      -- Separate `runMinio` per key so one transport error doesn't abort siblings;
      -- mirrors the `fetchIndividuals` pattern elsewhere in this module.
      perKey <- liftIO $ forM keyObjs $ \o ->
        (o,) <$> Minio.runMinio conn (Minio.removeObject bucket o)
      let fatal = [(o, e) | (o, Left e) <- perKey, not (isNoSuchKey e)]
      case fatal of
        _ : _ ->
          Log.logAttention
            "Replay expire: S3 delete failed, leaving row for retry"
            (HM.insert "errors" (toText $ show $ map (bimap (show :: Minio.Object -> Text) (toText . displayException)) fatal) logCtx)
        [] -> do
          Hasql.interpExecute_
            [HI.sql| DELETE FROM projects.replay_sessions WHERE session_id = #{sid} AND project_id = #{pid} |]
          Log.logInfo "Expired replay session" logCtx
  when (length rows >= expireReplayBatchSize) $ do
    -- See MergeReplaySession note: cannot import BackgroundJobs (circular).
    let payload = AE.object ["tag" AE..= ("ExpireReplayData" :: Text), "contents" AE..= ([] :: [AE.Value])]
    liftIO $ withResource ctx.jobsPool \c ->
      void $ createJob c "background_jobs" payload
