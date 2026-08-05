module Pages.Replay (replayPostH, ReplayPost (..), processReplayEvents, replaySessionGetH, fetchReplaySession, ReplaySessionResp (..), RawJson (..), compressAndMergeReplaySessions, mergeReplaySession, expireOldReplayData, migrateReplayStorage, migrateReplayStorageShard, replayObjectPrefix, migratedReplayKey, concatRawJsonArrays, sessionFileKeys, splitReplayPayload, ReplayPayload (..), stripJsonNullEscapes, firstEventTimestampRaw, claimMergeLease, releaseMergeLease, ReplayManifest (..), ReplaySegment (..), replaySessionManifestGetH, replaySessionShardGetH, buildReplayManifest, fetchReplayShard) where

import Codec.Compression.GZip qualified as GZip
import Conduit (runConduit)
import Control.Exception (ErrorCall (..), throwIO, try)
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


replayPostH :: Projects.ProjectId -> ReplayPost -> ATBaseCtx AE.Value
replayPostH pid body = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let messagePayload = AE.object ["events" AE..= body.events, "sessionId" AE..= body.sessionId, "projectId" AE..= pid, "timestamp" AE..= body.timestamp, "userId" AE..= body.userId, "userEmail" AE..= body.userEmail, "userName" AE..= body.userName]
  pubResult <- case ctx.config.rrwebTopics of
    [] -> pure $ Left "No rrweb pubsub topics configured"
    (topicName : _) -> first ("Failed to publish replay event: " <>) <$> runSharedProducer ctx (publishJSONToKafka topicName messagePayload (HM.fromList [("eventType", "replay")]))
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
    finalize meta = maybe (fail "missing events field") \raw ->
      case AE.fromJSON (AE.Object meta) of
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
      AB.skipWhile (\b -> b /= o && b /= c && b /= q)
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
-- Replay doesn't dual-write so it can't surface a 'Telemetry.WriteFailure'.
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
  replicateM_ (length oversized) $ Metrics.bumpErrorCounter "replay:oversize_message"
  (acks, decodePoison) <- foldMapM (handleChunk envCfg ctx.jobsPool) (chunksByBytes replayBatchByteBudget (BS.length . snd) valid)
  pure $ Right (acks, oversizePoison <> decodePoison)
  where
    -- Returns @(acks, poison)@ for one chunk so the caller can DLQ poison and
    -- ack only the successfully-saved + DLQ'd ackIds.
    handleChunk envCfg jobsPool = foldMapM \(!ackId, !body) ->
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


sessionLogCtx :: Projects.ProjectId -> UUID.UUID -> HashMap Text Text
sessionLogCtx pid sid = HM.fromList [("session_id", UUID.toText sid), ("project_id", pid.toText)]


-- | Attach a rendered exception to a log context under the conventional
-- "error" key. Used so call sites stop repeating the displayException dance.
withError :: Exception e => e -> HashMap Text Text -> HashMap Text Text
withError e = HM.insert "error" (toText $ displayException e)


-- | Enqueue a BgJobs job. The payload is hand-rolled to mirror the derived
-- Generic ToJSON shape because BackgroundJobs imports this module, so we can't
-- import its job sum type here.
enqueueBgJob :: IOE :> es => Pool Connection -> Text -> [AE.Value] -> Eff es ()
enqueueBgJob pool tag contents =
  liftIO $ withResource pool \c -> void $ createJob c "background_jobs" (AE.object ["tag" AE..= tag, "contents" AE..= contents])


markMerged :: DB es => Projects.ProjectId -> UUID.UUID -> UTCTime -> Eff es ()
markMerged pid sid now =
  Hasql.interpExecute_ [HI.sql| UPDATE projects.replay_sessions SET merged = TRUE, updated_at = #{now} WHERE session_id = #{sid} AND project_id = #{pid} |]


-- | Atomically claim the per-session merge lease. Returns True iff this worker
-- now owns it (lease was unheld or older than the 15-minute stale window — the
-- latter reclaims a lease left behind by a worker that died mid-merge). A merge
-- pass on a large session holds multi-GB heap for ~a minute; the lease caps
-- concurrency to one per session so duplicate/queued jobs claim-and-skip instead
-- of piling on and melting the replica (2026-07-13 incident).
-- Invariant: the 15-minute stale window must exceed the worst-case single merge
-- pass, else a slow merge gets double-claimed. maxFilesPerMerge bounds each pass
-- to keep it well under that.
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
      conn = Minio.setCreds creds (Minio.setRegion region info)
   in (conn, bucket)


-- | Stable, UTC, lexicographically sortable replay namespace. ISO dates are
-- deliberately used instead of DD-MM-YYYY so an object-store listing is also
-- chronological.
replayObjectPrefix :: Projects.ProjectId -> UTCTime -> UUID.UUID -> Text
replayObjectPrefix pid at sid =
  pid.toText <> "/rrweb/" <> toText (formatTime defaultTimeLocale "%Y-%m-%d" at) <> "/" <> UUID.toText sid <> "/"


-- | Move any old replay key into the structured namespace while retaining its
-- filename. Already-migrated keys are returned unchanged, making migration
-- retries idempotent.
migratedReplayKey :: Projects.ProjectId -> UTCTime -> UUID.UUID -> Text -> Text
migratedReplayKey pid at sid oldKey
  | (pid.toText <> "/rrweb/") `T.isPrefixOf` oldKey = oldKey
  | otherwise = replayObjectPrefix pid at sid <> T.takeWhileEnd (/= '/') oldKey


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
    Minio.runMinio conn (fetchRawForMerge bucket k False) <&> (k,)
  let raws = [r | (_, Right (Right r)) <- perKey]
      decodeErrs = [(k, de) | (k, Right (Left de)) <- perKey]
      transportErrs = [(k, displayException me) | (k, Left me) <- perKey, not (isNoSuchKey me)]
      missing = [k | (k, Left me) <- perKey, isNoSuchKey me]
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
      logCtx = HM.fromList [("session", sessionStr), ("projectId", pid.toText)]
      transientMsg = "Storage is temporarily unreachable. Retry in a moment — your recording is safe." :: Text
      notFoundErr = "No recorded events found for this session. The SDK may not have uploaded any events, or storage retention has expired. Session ID is below — share with support if this is unexpected." :: Text
      corruptedErr = Left "This session’s history is corrupted and can’t be replayed. Contact support with the session ID so we can investigate."
      noEvents reason = do
        Log.logAttention "No replay events found anywhere for session" (HM.insert "reason" reason logCtx)
        pure $ Left notFoundErr
      legacyFailed e = Left transientMsg <$ Log.logError "Failed to load legacy replay blob" (HM.insert "error" e logCtx)
      -- Ancient single-object sessions (pre file_keys migration): fetch raw.
      legacy reason = do
        Log.logInfo "Falling back to legacy replay blob" (HM.insert "reason" reason logCtx)
        liftIO (Minio.runMinio conn $ fetchRawForMerge bucket (sessionStr <> ".json") False) >>= \case
          Left err
            | isNoSuchKey err -> noEvents "legacy_blob_missing"
            | otherwise -> legacyFailed (toText $ displayException err)
          Right (Left e) -> legacyFailed (toText e)
          Right (Right (_, raw))
            | isEmptyJsonArray raw -> noEvents "legacy_blob_missing"
            | otherwise -> pure $ Right (raw, False)
  -- Shards (incl. the legacy monolith, registered as a shard) and the unmerged
  -- individual tail both come from the DB — never from listing S3 (minio-hs can't
  -- list R2). Fetch each key by getObject, gzip-decoding by suffix.
  (shardKeys, fileKeys) <- sessionKeys pid sessionId
  shardRes <- liftIO $ forM shardKeys $ \sk -> Minio.runMinio conn (fetchRawForMerge bucket sk (".gz" `T.isSuffixOf` sk))
  let shardRaws = [r | Right (Right r) <- shardRes]
      shardDecodeErrs = [e | Right (Left e) <- shardRes] -- corrupt shard bytes
      shardTransportErrs = [e | Left e <- shardRes, not (isNoSuchKey e)] -- transient (NoSuchKey = concurrent seal, tolerate)
      shardCorrupt = not (null shardDecodeErrs) -- data-integrity event
      -- A corrupt shard is surfaced (not silently dropped); a transient fetch failure
      -- flags the recording partial, same contract as a missing individual file.
  unless (null shardDecodeErrs) $ Log.logError "Corrupt replay shard" (HM.insert "errors" (toText $ show shardDecodeErrs) logCtx)
  unless (null shardTransportErrs) $ Log.logAttention "Transient shard fetch failure; serving partial recording" (HM.insert "errors" (toText $ show shardTransportErrs) logCtx)
  (individuals, indivPartial, missingCount) <-
    if null fileKeys then pure ([], False, 0) else fetchIndividualsRaw conn bucket fileKeys logCtx
  let partial = indivPartial || not (null shardTransportErrs)
      -- Individuals collapse (sorted by first event) into one blob, ordered against
      -- the shards by first event. concatRawJsonArrays drops empty arrays.
      indivBlob = concatRawJsonArrays $ map snd $ sortWith fst individuals
      indivEntry = [(fromRight 0 (firstEventTimestampRaw indivBlob), indivBlob) | not (null individuals)]
      combined = concatRawJsonArrays $ map snd $ sortWith fst (shardRaws <> indivEntry)
  if
    | shardCorrupt -> pure corruptedErr
    | combined /= "[]" -> pure $ Right (combined, partial)
    | null fileKeys && null shardKeys -> legacy "new_or_pre_migration_session"
    | partial -> pure $ Left transientMsg
    | otherwise -> do
        Log.logAttention
          "Replay session resolved empty despite tracked keys"
          (HM.insert "file_keys_count" (show $ length fileKeys) $ HM.insert "shard_keys_count" (show $ length shardKeys) $ HM.insert "missing_count" (show missingCount) logCtx)
        pure $ Left notFoundErr


-- | Tracked object keys for a session as @(sealed shards, unmerged tail)@, both
-- empty if the row doesn't exist (new session) or the columns are NULL. Always
-- read from Postgres, never listed from S3 (minio-hs can't list R2); each shard
-- key embeds its first-event ts so readers order shards without opening a blob.
sessionKeys :: DB es => Projects.ProjectId -> UUID.UUID -> Eff es ([Text], [Text])
sessionKeys pid sessionId = do
  rows :: [(V.Vector Text, V.Vector Text)] <-
    Hasql.interp [HI.sql| SELECT COALESCE(shard_keys, '{}'::text[]), COALESCE(file_keys, '{}'::text[]) FROM projects.replay_sessions WHERE session_id = #{sessionId} AND project_id = #{pid} LIMIT 1 |]
  pure $ maybe ([], []) (bimap V.toList V.toList) (listToMaybe rows)


sessionFileKeys :: DB es => Projects.ProjectId -> UUID.UUID -> Eff es [Text]
sessionFileKeys pid = fmap snd . sessionKeys pid


-- | Identity + timing metadata for a session, surfaced in the player header.
data SessionMeta = SessionMeta
  { userId :: Maybe Text
  , userEmail :: Maybe Text
  , userName :: Maybe Text
  , lastEventAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON, HI.DecodeRow)


-- | Never fatal: a lookup failure logs and yields Nothing so the player still
-- renders without identity.
sessionMetadata :: (DB es, Log :> es) => Projects.ProjectId -> UUID.UUID -> HashMap Text Text -> Eff es (Maybe SessionMeta)
sessionMetadata pid sessionId logCtx = do
  r <- tryAny do
    rows :: [SessionMeta] <-
      Hasql.interp [HI.sql| SELECT user_id, user_email, user_name, last_event_at FROM projects.replay_sessions WHERE session_id = #{sessionId} AND project_id = #{pid} LIMIT 1 |]
    pure $ listToMaybe rows
  either (\err -> Nothing <$ Log.logAttention "sessionMetadata lookup failed; continuing without identity" (withError err logCtx)) pure r


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
      <> foldMap ("partial" AE..=) r.partial
      <> foldMap ("error" AE..=) r.errorMsg
      <> foldMap metaSeries r.meta
    where
      metaSeries m = "userId" AE..= m.userId <> "userEmail" AE..= m.userEmail <> "userName" AE..= m.userName <> "lastEventAt" AE..= m.lastEventAt
  toJSON r =
    AE.object
      $ ["events" AE..= r.events]
      <> foldMap (\p -> ["partial" AE..= p]) r.partial
      <> foldMap (\e -> ["error" AE..= e]) r.errorMsg
      <> foldMap (\m -> ["userId" AE..= m.userId, "userEmail" AE..= m.userEmail, "userName" AE..= m.userName, "lastEventAt" AE..= m.lastEventAt]) r.meta


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
saveReplayMinio envCfg jobsPool ackId payload =
  Projects.projectById payload.projectId >>= \case
    Nothing -> pure $ Just ackId
    Just p
      | payload.eventsEmpty -> pure $ Just ackId
      | otherwise -> do
          now <- Time.currentTime
          let session = UUID.toText payload.sessionId
              (conn, bucket) = projectMinioConn envCfg p
              timeStr = toText $ formatTime defaultTimeLocale "%Y%m%dT%H%M%S%q" now
              objKeyText = replayObjectPrefix payload.projectId now payload.sessionId <> timeStr <> ".json"
              body = payload.eventsBytes
          liftIO (Minio.runMinio conn $ Minio.putObject bucket objKeyText (CC.sourceLazy (BL.fromStrict body)) (Just $ fromIntegral $ BS.length body) Minio.defaultPutObjectOptions) >>= \case
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
              when (fileCount >= mergeFileCountThreshold && fileCount `mod` mergeFileCountThreshold == 0)
                $ enqueueBgJob jobsPool "MergeReplaySession" [AE.toJSON payload.projectId, AE.toJSON payload.sessionId]
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
  meta <- sessionMetadata pid sessionId summaryCtx
  let emptyResp userMsg = ReplaySessionResp{events = RawJson "[]", partial = Nothing, errorMsg = Just userMsg, meta}
  tryAny (getSessionEvents conn pid bucket sessionId) >>= \case
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


-- | Build the progressive manifest, oldest→newest: shards (incl. the legacy
-- monolith, registered as a shard) then the unmerged individual tail. Pure DB
-- reads — never lists or opens S3 (minio-hs can't list R2), so it's cheap
-- regardless of session size and correct on R2.
buildReplayManifest :: (DB es, Log :> es) => Projects.Project -> UUID.UUID -> Eff es ReplayManifest
buildReplayManifest p sessionId = do
  let pid = p.id
      summaryCtx = HM.fromList [("session", UUID.toText sessionId), ("projectId", pid.toText)]
  meta <- sessionMetadata pid sessionId summaryCtx
  (shardKeys, tailKeys) <- sessionKeys pid sessionId
  -- Order shards by embedded first-event ts (the legacy monolith key doesn't
  -- parse → 0 → sorts first, which is correct: it's the oldest). gzip by suffix.
  let shardSegs = sortOn (.firstTs) [ReplaySegment{key = k, firstTs = fromIntegral (maybe 0 snd (parseShardKey k)), gzipped = ".gz" `T.isSuffixOf` k} | k <- shardKeys]
      tailTs = maybe 0 (.firstTs) (viaNonEmpty last shardSegs) -- keep the unmerged tail sorting after all shards
      tailSegs = [ReplaySegment{key = k, firstTs = tailTs, gzipped = False} | k <- tailKeys]
      segs = shardSegs <> tailSegs
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
      sidText = UUID.toText sessionId
      -- Authorize against both the current structured namespace and legacy
      -- session-scoped keys while their online migration is in progress.
      structuredMarker = "/rrweb/"
      scoped k =
        (p.id.toText <> structuredMarker)
          `T.isPrefixOf` k
          && ("/" <> sidText <> "/")
          `T.isInfixOf` k
          || (sidText <> "/")
          `T.isPrefixOf` k
          || k
          == sidText
          <> ".json"
  case mkey of
    Just k | scoped k -> do
      liftIO (Minio.runMinio conn $ fetchRawObject bucket k (".gz" `T.isSuffixOf` k)) >>= \case
        Right raw -> pure $ RawJson raw
        -- NoSuchKey = a concurrent seal/merge already removed this source; tolerate
        -- as empty (the sealed shard carries its events). Any other error
        -- (transport/auth) is transient — re-throw so the shard endpoint 500s and
        -- the player shows a retryable error, instead of masking it as "[]" which
        -- the client mislabels as "too few events / recording never started".
        Left err | isNoSuchKey err -> RawJson "[]" <$ Log.logInfo "Replay shard already sealed/removed; serving empty" (HM.fromList [("key", k)])
        Left err -> Log.logAttention "Replay shard fetch failed" (withError err (HM.fromList [("key", k)])) >> liftIO (throwIO err)
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


-- | Look up a project + its S3 settings + tracked file keys and seal them into
-- gzip shards. Runs the extra DB finalization step after the S3 merge succeeds.
mergeOneSessionByKeys
  :: Projects.ProjectId
  -> UUID.UUID
  -> Maybe Text
  -> (UTCTime -> ATBackgroundCtx ())
  -> ATBackgroundCtx ()
mergeOneSessionByKeys pid sessionId logSuffix afterMerge =
  Projects.projectById pid >>= \case
    Nothing -> do
      -- Orphaned row (project gone). Flip merged=TRUE so we stop re-picking it every run.
      Log.logAttention "Project not found for replay session merge; marking merged" logCtx
      now' <- Time.currentTime
      markMerged pid sessionId now'
    Just p ->
      claimMergeLease pid sessionId >>= \case
        False -> Log.logInfo "Replay merge already in progress for session; skipping duplicate" logCtx
        True -> mergeUnderLease p `finally` releaseMergeLease pid sessionId
  where
    logCtx = sessionLogCtx pid sessionId
    mergeUnderLease p = do
      ctx <- Effectful.Reader.Static.ask @AuthContext
      let (s3Conn, bucket) = projectMinioConn ctx.config p
      (existingShards, allFileKeys) <- sessionKeys pid sessionId
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
          -- Next append index from the DB shard count (no S3 listing).
          let startIdx = 1 + foldl' max 0 (mapMaybe (fmap fst . parseShardKey) existingShards)
          nowForKey <- Time.currentTime
          let shardPrefix = replayObjectPrefix pid nowForKey sessionId
          liftIO (try @MergeError $ mergeOneSession s3Conn bucket shardPrefix startIdx fileKeys) >>= \case
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
            Right sealedKeys -> do
              now' <- Time.currentTime
              -- Atomically: append the sealed shard keys and drop only the file_keys
              -- we merged (concurrent `array_append` writers may have added more since
              -- the snapshot). Recording shard keys here — not by listing S3 — is what
              -- makes the read path find them on R2.
              let vFileKeys = V.fromList fileKeys
                  vShardKeys = V.fromList sealedKeys
              rowsAffected <-
                Hasql.interpExecute
                  [HI.sql|
                    UPDATE projects.replay_sessions SET
                      shard_keys = array_cat(COALESCE(shard_keys, '{}'::text[]), #{vShardKeys}::text[]),
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
                  when (length allFileKeys > maxFilesPerMerge)
                    $ enqueueBgJob ctx.jobsPool "MergeReplaySession" [AE.toJSON pid, AE.toJSON sessionId]


-- | Uncompressed-bytes budget per sealed shard. A merge accumulates raw event
-- files up to this size, seals them into one gzip shard, releases the buffer, and
-- continues — so peak heap per merge is ~one shard, not the whole session.
-- This is the O(n²)→O(n) fix: shards are append-only and immutable, so a growing
-- session never re-reads/rewrites its accumulated history (the 2026-07-13 incident
-- had a 528 MB merged blob re-decompressed on every 25-file pass).
shardSealBytes :: Int64
shardSealBytes = 12 * 1024 * 1024


-- | Object key for a sealed shard: `<prefix>/shard-<idx>-<firstTsMs>.json.gz`. The
-- index is a collision-free append id (next = max existing + 1), NOT an ordering
-- key — read/manifest order shards by the embedded first-event timestamp, parsed
-- from the key without opening any blob.
--
-- >>> shardKeyFor "project/rrweb/2026-05-08/session/" 7 1700000000000
-- "project/rrweb/2026-05-08/session/shard-000007-1700000000000.json.gz"
-- >>> parseShardKey (shardKeyFor "project/rrweb/2026-05-08/session/" 7 1700000000000)
-- Just (7,1700000000000)
shardKeyFor :: Text -> Int -> Integer -> Minio.Object
shardKeyFor prefix idx firstTsMs =
  prefix <> "shard-" <> T.justifyRight 6 '0' (show idx) <> "-" <> show firstTsMs <> ".json.gz"


-- | Parse a shard object key into (index, firstTsMs). `<sid>/shard-<idx>-<ts>.json.gz`;
-- splits only the filename so the `-`/`/` in the session UUID prefix never reaches it.
--
-- >>> parseShardKey "abc/shard-000007-1700000000000.json.gz"
-- Just (7,1700000000000)
-- >>> parseShardKey "abc/shard-7.json.gz"
-- Nothing
-- >>> parseShardKey "abc/shard-00-00-1.json.gz"
-- Nothing
-- >>> parseShardKey "abc/notashard.json.gz"
-- Nothing
parseShardKey :: Text -> Maybe (Int, Integer)
parseShardKey key = do
  name <- T.stripSuffix ".json.gz" (T.takeWhileEnd (/= '/') key)
  rest <- T.stripPrefix "shard-" name
  case T.splitOn "-" rest of
    [idxT, tsT] -> (,) <$> readMaybe (toString idxT) <*> readMaybe (toString tsT)
    _ -> Nothing


-- | Seal the tracked event files into append-only, byte-bounded gzip shards and
-- return the sealed shard keys (which the caller records in the DB — we never
-- list S3, since minio-hs can't list R2). Streams the files one at a time,
-- buffering raw bytes up to the budget then sealing, so peak heap per merge is
-- ~one shard — the O(n²)→O(n) fix vs. the old monolith. Each shard's put and its
-- sources' deletion happen in one `runMinio` round. Aborts (throwIO MergeError)
-- on any decode/transport failure so history is never dropped; NoSuchKey is
-- tolerated (a source already removed by a crashed prior run). `startIdx` is the
-- next append index (from the DB shard count) so shard keys stay unique.
mergeOneSession :: Minio.ConnectInfo -> Minio.Bucket -> Text -> Int -> [Text] -> IO [Text]
mergeOneSession conn bucket keyPrefix startIdx fileKeys = reverse <$> go startIdx (sort fileKeys) [] 0 []
  where
    -- buf :: [(firstTs, raw, sourceKey)] for the current shard; sealed :: sealed keys (newest first)
    go idx [] buf _ sealed = seal idx buf sealed
    go idx (k : ks) buf !bufBytes sealed =
      Minio.runMinio conn (fetchRawForMerge bucket k False) >>= \case
        Left me | isNoSuchKey me -> go idx ks buf bufBytes sealed
        Left me -> throwIO (MergeFetchFailed me)
        Right (Left de) -> throwIO (MergeDecodeFailed [(k, de)])
        Right (Right (ts, raw)) ->
          let buf' = (ts, raw, k) : buf
              b' = bufBytes + BL.length raw
           in if b' >= shardSealBytes
                then seal idx buf' sealed >>= \s -> go (idx + 1) ks [] 0 s
                else go idx ks buf' b' sealed
    seal _ [] sealed = pure sealed
    seal idx buf sealed = do
      let sorted = sortWith (\(ts, _, _) -> ts) buf
          firstTs = maybe 0 (\(ts, _, _) -> ts) (viaNonEmpty head sorted)
          !merged = concatRawJsonArrays [raw | (_, raw, _) <- sorted]
          !compressed = GZip.compress merged
          shardKey = shardKeyFor keyPrefix idx (round (firstTs :: Double))
      putRes <- Minio.runMinio conn $ do
        Minio.putObject bucket shardKey (CC.sourceLazy compressed) (Just (BL.length compressed)) Minio.defaultPutObjectOptions
        forM_ [k | (_, _, k) <- sorted] (Minio.removeObject bucket)
      whenLeft_ putRes (throwIO . MergePutFailed)
      pure (shardKey : sealed)


-- | Copy one object and verify the destination byte-for-byte before returning.
-- The source remains untouched; callers switch the DB reference first and only
-- then remove it. This ordering makes every interruption recoverable.
--
-- Size-capped: the copy holds source AND verification bytes fully in memory, so
-- a multi-GB merged session blows the RTS -M heap cap and kills the whole
-- process — every replica in the swarm took turns dying on the same object
-- (2026-08-05 crash-loop). Oversized objects are reported as a failure (the
-- session stays unmigrated, visible in logs) until a streaming/server-side
-- copy handles them.
copyReplayObjectVerified :: Minio.ConnectInfo -> Minio.Bucket -> Text -> Text -> IO (Either Text ())
copyReplayObjectVerified conn bucket oldKey newKey
  | oldKey == newKey = pure $ Right ()
  | otherwise = do
      sizeCheck <- Minio.runMinio conn $ Minio.statObject bucket oldKey Minio.defaultGetObjectOptions
      case sizeCheck of
        Left err -> pure $ Left $ "source stat failed: " <> toText (displayException err)
        Right info
          | Minio.oiSize info > maxMigrateObjectBytes ->
              pure $ Left $ "object exceeds migration size cap (" <> show (Minio.oiSize info) <> " bytes): " <> oldKey
        _ -> copyNow
  where
    maxMigrateObjectBytes = 512 * 1024 * 1024
    copyNow = do
      source <- Minio.runMinio conn $ fetchRawObject bucket oldKey False
      case source of
        Left err -> pure $ Left $ "source read failed: " <> toText (displayException err)
        Right bytes -> do
          putResult <- Minio.runMinio conn $ Minio.putObject bucket newKey (CC.sourceLazy bytes) (Just $ BL.length bytes) Minio.defaultPutObjectOptions
          case putResult of
            Left err -> pure $ Left $ "destination write failed: " <> toText (displayException err)
            Right _ -> do
              verified <- Minio.runMinio conn $ fetchRawObject bucket newKey False
              -- The comparison MUST happen here, not in a returned thunk: callers
              -- collect these Eithers across a whole session before scrutinising
              -- them, and a lazy (copied == bytes) retains BOTH full bytestrings
              -- of every object until then — the second half of the 2026-08-05
              -- heap-overflow crash-loop (the size cap alone didn't bound a
              -- many-object session).
              case verified of
                Left err -> pure $ Left $ "destination verification read failed: " <> toText (displayException err)
                Right copied -> let !ok = copied == bytes in pure $ if ok then Right () else Left "destination bytes differ from source"


-- | Online migration from legacy @<session>/...@ keys to
-- @<project>/rrweb/YYYY-MM-DD/<session>/...@. Processes at most @batchSize@
-- sessions and returns the number whose DB references were switched. Safe to
-- run repeatedly and concurrently with ingestion: the conditional UPDATE loses
-- the race (and retries later) if either key array changed after selection.
-- TEMPORARY: delete this function, 'migratedReplayKey', and the background-job
-- wiring after the query selects zero rows in every environment and old-object
-- cleanup has been verified. Keep 'replayObjectPrefix'; it is the permanent
-- write-path layout.
migrateReplayStorage :: Int -> ATBackgroundCtx Int
migrateReplayStorage = migrateReplayStorageShard 1 0


-- | Migrate one deterministic subset of legacy sessions. Fixed shards avoid
-- relying on a worker to enqueue its successor and keep production jobs below
-- the stale-job threshold. Safe to retry because migrated rows stop matching.
-- TEMPORARY: delete this function after all replay migrations are complete.
migrateReplayStorageShard :: Int -> Int -> Int -> ATBackgroundCtx Int
migrateReplayStorageShard requestedShardCount requestedShardIndex batchSize = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let lim = fromIntegral (max 1 batchSize) :: Int64
      shardCount = fromIntegral (max 1 requestedShardCount) :: Int64
      shardIndex = fromIntegral (requestedShardIndex `mod` max 1 requestedShardCount) :: Int64
  rows :: [(UUID.UUID, Projects.ProjectId, UTCTime, V.Vector Text, V.Vector Text)] <-
    Hasql.interp
      [HI.sql|
        SELECT session_id, project_id, last_event_at,
               COALESCE(file_keys, '{}'::text[]), COALESCE(shard_keys, '{}'::text[])
        FROM projects.replay_sessions
        WHERE EXISTS (
          SELECT 1 FROM unnest(COALESCE(file_keys, '{}'::text[]) || COALESCE(shard_keys, '{}'::text[])) k
          WHERE k NOT LIKE project_id::text || '/rrweb/%'
        )
          AND ((hashtextextended(session_id::text, 0) % #{shardCount}) + #{shardCount}) % #{shardCount} = #{shardIndex}
        ORDER BY last_event_at, session_id
        LIMIT #{lim}
      |]
  migrated <- forM rows $ \(sid, pid, sessionAt, oldFiles, oldShards) ->
    Projects.projectById pid >>= \case
      Nothing -> False <$ Log.logAttention "Replay storage migration: project missing" (sessionLogCtx pid sid)
      Just p -> do
        let (conn, bucket) = projectMinioConn ctx.config p
            remap = migratedReplayKey pid sessionAt sid
            newFiles = V.map remap oldFiles
            newShards = V.map remap oldShards
            pairs = zip (V.toList oldFiles <> V.toList oldShards) (V.toList newFiles <> V.toList newShards)
        -- Force each copy's outcome before moving on so the finished object's
        -- bytes are collectable; deferring to the post-loop scrutiny retained
        -- every object of the session at once.
        copyResults <- liftIO $ forM pairs $ \(oldKey, newKey) -> do
          !res <- copyReplayObjectVerified conn bucket oldKey newKey
          pure (oldKey, newKey, res)
        case [(o, n, e) | (o, n, Left e) <- copyResults] of
          failures@(_ : _) -> do
            Log.logAttention "Replay storage migration: verified copy failed; DB unchanged" (HM.insert "failures" (toText $ show failures) $ sessionLogCtx pid sid)
            pure False
          [] -> do
            changed <-
              Hasql.interpExecute
                [HI.sql|
                  UPDATE projects.replay_sessions
                  SET file_keys = #{newFiles}::text[], shard_keys = #{newShards}::text[], updated_at = now()
                  WHERE session_id = #{sid} AND project_id = #{pid}
                    AND file_keys = #{oldFiles}::text[] AND shard_keys = #{oldShards}::text[]
                |]
            if changed == 0
              then False <$ Log.logInfo "Replay storage migration: concurrent change detected; will retry" (sessionLogCtx pid sid)
              else do
                -- DB already points at verified copies. Deletion failure merely
                -- leaves a duplicate and is safe to retry/clean up separately.
                deleteResults <- liftIO $ forM [(o, n) | (o, n) <- pairs, o /= n] $ \(o, _) -> (o,) <$> Minio.runMinio conn (Minio.removeObject bucket o)
                let deleteFailures = [(o, displayException e) | (o, Left e) <- deleteResults, not (isNoSuchKey e)]
                unless (null deleteFailures) $ Log.logAttention "Replay storage migration: old-key cleanup incomplete" (HM.insert "failures" (toText $ show deleteFailures) $ sessionLogCtx pid sid)
                pure True
  let count = length $ filter id migrated
  Log.logInfo "Replay storage migration batch complete" ("selected", length rows, "migrated", count)
  -- OddJobs retries the batch if a verified copy failed or a concurrent writer
  -- won the conditional UPDATE. Successful rows are already idempotently done.
  when (count < length rows) $ liftIO $ throwIO $ ErrorCall "replay storage migration batch incomplete"
  pure count


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
  rows :: [(UUID.UUID, Projects.ProjectId, V.Vector Text, V.Vector Text)] <-
    Hasql.interp
      [HI.sql|
        SELECT rs.session_id, rs.project_id, COALESCE(rs.file_keys, '{}'::text[]), COALESCE(rs.shard_keys, '{}'::text[])
        FROM projects.replay_sessions rs
        JOIN projects.projects p ON p.id = rs.project_id
        WHERE rs.last_event_at < #{cutoff}
          AND p.s3_bucket IS NULL
        ORDER BY rs.last_event_at
        LIMIT #{batchLimit}
      |]
  Log.logInfo "Expiring old replay sessions" ("count", length rows)
  forM_ rows $ \(sid, pid, fileKeys, shardKeys) -> do
    projectM <- Projects.projectById pid
    whenJust projectM $ \p -> do
      let (conn, bucket) = projectMinioConn ctx.config p
          -- shard_keys covers sealed shards + the legacy monolith; the merged key is kept
          -- as a belt-and-suspenders for any session not yet reflected in shard_keys.
          keyObjs = (UUID.toText sid <> "/merged.json.gz") : V.toList shardKeys <> V.toList fileKeys
          logCtx = sessionLogCtx pid sid
      -- Separate `runMinio` per key so one transport error doesn't abort siblings;
      -- mirrors the `fetchIndividuals` pattern elsewhere in this module.
      perKey <- liftIO $ forM keyObjs $ \o ->
        (o,) <$> Minio.runMinio conn (Minio.removeObject bucket o)
      let fatal = [(o, e) | (o, Left e) <- perKey, not (isNoSuchKey e)]
      if null fatal
        then do
          Hasql.interpExecute_
            [HI.sql| DELETE FROM projects.replay_sessions WHERE session_id = #{sid} AND project_id = #{pid} |]
          Log.logInfo "Expired replay session" logCtx
        else
          Log.logAttention
            "Replay expire: S3 delete failed, leaving row for retry"
            (HM.insert "errors" (toText $ show $ map (bimap (show :: Minio.Object -> Text) (toText . displayException)) fatal) logCtx)
  when (length rows >= expireReplayBatchSize) $ enqueueBgJob ctx.jobsPool "ExpireReplayData" []
