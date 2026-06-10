module Pages.Replay (replayPostH, ReplayPost (..), processReplayEvents, replaySessionGetH, fetchReplaySession, compressAndMergeReplaySessions, mergeReplaySession, expireOldReplayData, concatRawJsonArrays, mergeEventArrays, sessionFileKeys, splitReplayPayload, ReplayPayload (..), stripJsonNullEscapes) where

import Codec.Compression.GZip qualified as GZip
import Conduit (runConduit)
import Control.Exception (throwIO, try)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Parser qualified as AEP
import Data.Aeson.Types qualified as AET
import Data.Attoparsec.ByteString qualified as AB
import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.ByteString qualified as BS
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
import Pkg.Queue (publishJSONToKafka)
import Relude
import System.Config (AuthContext (config, jobsPool), EnvConfig (..))
import System.Logging qualified as Log
import System.Types (ATAuthCtx, ATBackgroundCtx, ATBaseCtx, DB, RespHeaders, addRespHeaders)
import UnliftIO.Exception (tryAny)


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
publishReplayEvent :: ReplayPost -> Projects.ProjectId -> ATBaseCtx (Either Text Text)
publishReplayEvent replayData pid = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let messagePayload = AE.object ["events" AE..= replayData.events, "sessionId" AE..= replayData.sessionId, "projectId" AE..= pid, "timestamp" AE..= replayData.timestamp, "userId" AE..= replayData.userId, "userEmail" AE..= replayData.userEmail, "userName" AE..= replayData.userName]
      attributes = HM.fromList [("eventType", "replay")]
  case ctx.config.rrwebTopics of
    [] -> pure $ Left "No rrweb pubsub topics configured"
    (topicName : _) ->
      liftIO $ first ("Failed to publish replay event: " <>) <$> publishJSONToKafka ctx topicName messagePayload attributes


replayPostH :: Projects.ProjectId -> ReplayPost -> ATBaseCtx AE.Value
replayPostH pid body = do
  pubResult <- publishReplayEvent body pid
  pure $ AE.object $ ["sessionId" AE..= body.sessionId] <> case pubResult of
    Left errMsg -> ["status" AE..= ("warning" :: Text), "message" AE..= errMsg]
    Right messageId -> ["status" AE..= ("ok" :: Text), "messageId" AE..= messageId]


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
maxReplayMessageBytes :: Int
maxReplayMessageBytes = 10 * 1024 * 1024


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
          pure ReplayPayload{eventsBytes = raw, eventsEmpty = isEmptyJsonArray raw, ..}


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
isEmptyJsonArray :: BS.ByteString -> Bool
isEmptyJsonArray raw = case BS.uncons (BS.dropWhile isWs raw) of
  Nothing -> True
  Just (0x5b, t) -> case BS.uncons (BS.dropWhile isWs t) of
    Just (0x5d, rest) -> BS.all isWs rest
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
  (acks, decodePoison) <- mconcat <$> mapM (handleChunk envCfg ctx.jobsPool) (chunkByBytes valid)
  pure $ Right (acks, oversizePoison <> decodePoison)
  where
    chunkByBytes = go 0 [] []
      where
        go _ chunk chunks [] = reverse (reverse chunk : chunks)
        go sz chunk chunks (m : rest)
          | sz + BS.length (snd m) > replayBatchByteBudget && not (null chunk) =
              go (BS.length (snd m)) [m] (reverse chunk : chunks) rest
          | otherwise = go (sz + BS.length (snd m)) (m : chunk) chunks rest

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


-- | Retrieve a legacy single-object replay blob. Returns `Left` on transport
-- failure or decode error (caller decides how to surface); `Right V.empty` only
-- when the underlying JSON is genuinely empty or not an array.
getMinioFile :: IOE :> es => Minio.ConnectInfo -> Minio.Bucket -> Minio.Object -> Eff es (Either Text AE.Array)
getMinioFile conn bucket objKey = do
  res <- liftIO $ Minio.runMinio conn $ fetchEventArray bucket objKey False
  pure $ case res of
    Right (Right a) -> Right a
    Right (Left decodeErr) -> Left $ "decode: " <> toText decodeErr
    Left err
      | isNoSuchKey err -> Right V.empty
      | otherwise -> Left $ "transport: " <> toText (displayException err)


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


data MergeError
  = MergeDecodeFailed [(Text, String)]
  | MergeFetchFailed MinioErr
  | MergePutFailed MinioErr
  deriving stock (Show)
  deriving anyclass (Exception)


-- | Describe the top-level JSON constructor (for error messages).
describeValue :: AE.Value -> String
describeValue = \case
  AE.Object _ -> "object"
  AE.Array _ -> "array"
  AE.String _ -> "string"
  AE.Number _ -> "number"
  AE.Bool _ -> "bool"
  AE.Null -> "null"


-- | Download one object and decode as a JSON array, returning the decoded
-- array alongside the raw decompressed bytes. Transport errors propagate
-- through the `Minio` monad as `Left MinioErr`; decode failures are returned
-- as `Left String`.
loadJsonArray :: Minio.Bucket -> Minio.Object -> Bool -> Minio.Minio (Either String (AE.Array, BL.ByteString))
loadJsonArray bucket objKey gzipped = do
  src <- Minio.getObject bucket objKey Minio.defaultGetObjectOptions
  bs <- runConduit $ Minio.gorObjectStream src .| CC.foldMap fromStrict
  let raw = if gzipped then GZip.decompress bs else bs
  pure $ case AE.eitherDecode raw of
    Right (AE.Array arr) -> Right (arr, raw)
    Right v -> Left $ "expected JSON array, got " <> describeValue v
    Left e -> Left e


fetchEventArray :: Minio.Bucket -> Minio.Object -> Bool -> Minio.Minio (Either String AE.Array)
fetchEventArray bucket objKey gzipped = fst <<$>> loadJsonArray bucket objKey gzipped


-- | Fetch one S3 object as raw decompressed bytes plus the first event's
-- timestamp (used to sort files before merging at the byte level).
-- Avoids re-encoding events during merge — raw bytes go directly to the output.
fetchRawForMerge :: Minio.Bucket -> Minio.Object -> Bool -> Minio.Minio (Either String (Double, BL.ByteString))
fetchRawForMerge bucket objKey gzipped =
  (\(arr, raw) -> (firstEventTimestamp arr, raw)) <<$>> loadJsonArray bucket objKey gzipped


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
fetchIndividuals
  :: (IOE :> es, Log :> es)
  => Minio.ConnectInfo
  -> Minio.Bucket
  -> [Text]
  -> HashMap Text Text
  -> Eff es (AE.Array, Bool, Int)
fetchIndividuals conn bucket fileKeys logCtx = do
  perKey <- liftIO $ forM fileKeys $ \k ->
    Minio.runMinio conn (fetchEventArray bucket (toObjKey k) False) <&> (k,)
  let (arrs, decodeErrs, transportErrs, missing) = foldr classify ([], [], [], []) perKey
      classify (_, Right (Right a)) (as, ds, ts, ms) = (a : as, ds, ts, ms)
      classify (k, Right (Left de)) (as, ds, ts, ms) = (as, (k, de) : ds, ts, ms)
      classify (k, Left me) (as, ds, ts, ms)
        | isNoSuchKey me = (as, ds, ts, k : ms)
        | otherwise = (as, ds, (k, displayException me) : ts, ms)
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
  pure (mergeEventArrays arrs, not (null decodeErrs && null transportErrs), length missing)


-- | Fetch events for a session. Returns `Left` with a user-facing message on
-- unrecoverable failure, `Right` on success (including empty or partial results).
getSessionEvents :: (DB es, Log :> es) => Minio.ConnectInfo -> Projects.ProjectId -> Minio.Bucket -> UUID.UUID -> Eff es (Either Text (AE.Array, Bool))
getSessionEvents conn pid bucket sessionId = do
  let sessionStr = UUID.toText sessionId
      mergedKey = mergedKeyFor sessionId
      logCtx = HM.fromList [("session", sessionStr), ("projectId", pid.toText)]
      legacy reason = do
        Log.logInfo "Falling back to legacy replay blob" (HM.insert "reason" reason logCtx)
        legacyRes <- getMinioFile conn bucket (toObjKey (sessionStr <> ".json"))
        case legacyRes of
          Right a
            | V.null a -> do
                Log.logAttention "No replay events found anywhere for session" (HM.insert "reason" "legacy_blob_missing" logCtx)
                pure $ Left notFoundErr
            | otherwise -> pure $ Right (a, False)
          Left e -> do
            Log.logError "Failed to load legacy replay blob" (HM.insert "error" e logCtx)
            pure $ Left "Storage is temporarily unreachable. Retry in a moment — your recording is safe."
      transientMsg = "Storage is temporarily unreachable. Retry in a moment — your recording is safe." :: Text
      transientErr = Left transientMsg
      corruptedErr = Left "This session’s history is corrupted and can’t be replayed. Contact support with the session ID so we can investigate."
      notFoundErr = "No recorded events found for this session. The SDK may not have uploaded any events, or storage retention has expired. Session ID is below — share with support if this is unexpected." :: Text
  fileKeys <- sessionFileKeys pid sessionId
  mergedRes <- liftIO $ Minio.runMinio conn $ fetchEventArray bucket mergedKey True
  case mergedRes of
    -- Corrupt merged.json.gz is a data-integrity event — do NOT silently fall through
    -- to individuals/legacy, that would hide the pre-merge history from the user.
    Right (Left decodeErr) -> do
      Log.logError "Corrupt merged replay blob" (HM.insert "error" (toText decodeErr) logCtx)
      pure corruptedErr
    Left err | not (isNoSuchKey err) -> do
      Log.logError "Failed to fetch merged replay events" (withError err logCtx)
      pure transientErr
    _ -> do
      let mergedEvents = case mergedRes of
            Right (Right a) -> a
            _ -> V.empty -- NoSuchKey path
      if null fileKeys
        then
          if V.null mergedEvents
            then legacy "new_or_pre_migration_session"
            else pure $ Right (mergedEvents, False)
        else do
          (arr, partial, missingCount) <- fetchIndividuals conn bucket fileKeys logCtx
          let combined = mergeEventArrays [mergedEvents, arr]
          if partial && V.null combined
            then pure $ Left transientMsg
            else
              if V.null combined
                then do
                  Log.logAttention
                    "Replay session resolved empty despite tracked file_keys"
                    ( HM.insert "file_keys_count" (show $ length fileKeys)
                        $ HM.insert "missing_count" (show missingCount) logCtx
                    )
                  pure $ Left notFoundErr
                else pure $ Right (combined, partial)


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


-- | Merge multiple already-sorted event arrays by sorting arrays based on their first event's timestamp, then concatenating.
--
-- >>> let ev ts = AE.object ["timestamp" AE..= (ts :: Double)]
-- >>> mergeEventArrays [V.fromList [ev 5], V.fromList [ev 1], V.fromList [ev 3]] == V.fromList [ev 1, ev 3, ev 5]
-- True
-- >>> mergeEventArrays [V.empty, V.fromList [ev 2]] == V.fromList [ev 2]
-- True
mergeEventArrays :: [AE.Array] -> AE.Array
mergeEventArrays arrays = V.concat $ sortWith firstEventTimestamp $ filter (not . V.null) arrays


-- | First event's `timestamp` field (0 if absent / array empty). Used to sort
-- per-file event arrays before concatenating during merge.
firstEventTimestamp :: AE.Array -> Double
firstEventTimestamp arr = fromMaybe 0 $ (arr V.!? 0) >>= AET.parseMaybe (AE.withObject "event" (AE..: "timestamp"))


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


replaySessionGetH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders AE.Value)
replaySessionGetH pid sessionId = do
  (_, p) <- Projects.sessionAndProject pid
  addRespHeaders =<< fetchReplaySession p sessionId


-- | Auth-free replay payload fetch: pulls events from S3 + metadata from DB,
-- returning the same JSON envelope the auth handler returns. Callers handle
-- their own authorization (project membership, share-link validity, etc.)
-- before invoking this.
fetchReplaySession
  :: (DB es, Effectful.Reader.Static.Reader AuthContext :> es, Log :> es)
  => Projects.Project -> UUID.UUID -> Eff es AE.Value
fetchReplaySession p sessionId = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let pid = p.id
      (conn, bucket) = projectMinioConn ctx.config p
      sessionStr = UUID.toText sessionId
      metaPairs = maybe [] (\m -> ["userId" AE..= m.userId, "userEmail" AE..= m.userEmail, "userName" AE..= m.userName, "lastEventAt" AE..= m.lastEventAt])
      summaryCtx = HM.fromList [("session", sessionStr), ("projectId", pid.toText)]
  meta <-
    tryAny (sessionMetadata pid sessionId)
      >>= either
        (\err -> Nothing <$ Log.logAttention "sessionMetadata lookup failed; continuing without identity" (withError err summaryCtx))
        pure
  let respond events extras = AE.object $ ["events" AE..= AE.Array events] <> extras <> metaPairs meta
  result <- tryAny $ getSessionEvents conn pid bucket sessionId
  case result of
    Right (Right (replayEvents, partial)) -> do
      Log.logInfo
        "Replay session served"
        (summaryCtx <> HM.fromList [("event_count", show $ V.length replayEvents), ("partial", show partial), ("outcome", "ok")])
      pure $ respond replayEvents ["partial" AE..= partial]
    Right (Left userMsg) -> do
      Log.logInfo "Replay session served (user-facing error)" (summaryCtx <> HM.fromList [("outcome", "user_error"), ("user_msg", userMsg)])
      pure $ respond V.empty ["error" AE..= userMsg]
    Left err -> do
      Log.logAttention "Unexpected exception fetching replay session" (withError err summaryCtx)
      pure $ respond V.empty ["error" AE..= ("We couldn’t load this session right now. Check your connection and retry — if it keeps failing, copy the session ID and share it with support." :: Text)]


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
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let logCtx = sessionLogCtx pid sessionId
  projectM <- Projects.projectById pid
  case projectM of
    Nothing -> do
      -- Orphaned row (project gone). Flip merged=TRUE so we stop re-picking it every run.
      Log.logAttention "Project not found for replay session merge; marking merged" logCtx
      now' <- Time.currentTime
      markMerged pid sessionId now'
    Just p -> do
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


-- | Merge the tracked S3 event files for a single session into one gzip-compressed file.
-- Uses raw byte concatenation to avoid the V.concat + AE.encode copies: events are never
-- re-parsed into AE.Array; sorting uses only the first event's timestamp from each file.
-- Aborts (via `throwIO MergeError`) on any decode/fetch failure to avoid silently
-- clobbering historical data.
mergeOneSession :: Minio.ConnectInfo -> Minio.Bucket -> UUID.UUID -> [Text] -> IO ()
mergeOneSession conn bucket sessionId fileKeys = do
  let mergedKey = mergedKeyFor sessionId
      fileObjs = map toObjKey fileKeys

  existingRes <- Minio.runMinio conn $ fetchRawForMerge bucket mergedKey True
  (mergedTs, mergedRaw) <- case existingRes of
    Right (Right r) -> pure r
    Right (Left decodeErr) -> throwIO $ MergeDecodeFailed [("merged.json.gz", decodeErr)]
    Left err
      | isNoSuchKey err -> pure (0, "[]")
      | otherwise -> throwIO $ MergeFetchFailed err

  -- Fetch each key in its own runMinio so one NoSuchKey (e.g. left over from a crashed
  -- prior merge) doesn't abort the whole batch. Decode failures and genuine transport
  -- errors still abort — we will not silently clobber historical data.
  perKey <- forM (zip fileKeys fileObjs) $ \(k, obj) ->
    (k,) <$> Minio.runMinio conn (fetchRawForMerge bucket obj False)
  -- Decode errors are permanent (corrupt data); transport errors are transient.
  let (decodeErrs, transportErrs, newRaws) = foldr classify ([], [], []) perKey
      classify (_, Right (Right r)) (de, te, rs) = (de, te, r : rs)
      classify (k, Right (Left e)) (de, te, rs) = ((k, e) : de, te, rs)
      classify (k, Left me) (de, te, rs)
        | isNoSuchKey me = (de, te, rs)
        | otherwise = (de, me : te, rs)
  case transportErrs of
    (firstErr : _) -> throwIO $ MergeFetchFailed firstErr
    [] -> unless (null decodeErrs) $ throwIO $ MergeDecodeFailed decodeErrs

  -- Force `merged` then `compressed` in order so each is fully computed and released
  -- before the next; otherwise `compressed` would retain the entire raw merged bytes.
  let sorted = sortWith fst $ (mergedTs, mergedRaw) : newRaws
      !merged = concatRawJsonArrays (map snd sorted)
      !compressed = GZip.compress merged
  putRes <- Minio.runMinio conn $ do
    Minio.putObject bucket mergedKey (CC.sourceLazy compressed) (Just (BL.length compressed)) Minio.defaultPutObjectOptions
    forM_ fileObjs $ Minio.removeObject bucket
  case putRes of
    Right _ -> pass
    Left err -> throwIO $ MergePutFailed err


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
