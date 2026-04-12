module Pages.Replay (replayPostH, ReplayPost (..), processReplayEvents, replaySessionGetH, compressAndMergeReplaySessions, mergeReplaySession) where

import Codec.Compression.GZip qualified as GZip
import Conduit (runConduit)
import Control.Exception (throwIO, try)
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.ByteString.Lazy qualified as BL
import Data.Conduit ((.|))
import Data.Conduit.Combinators qualified as CC
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.OpenApi (ToSchema)
import Data.Pool (Pool, withResource)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
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
import Network.Minio (MinioErr (..), ServiceErr (..))
import Network.Minio qualified as Minio
import OddJobs.Job (createJob)
import Pages.Settings (getMinioConnectInfo)
import Pkg.Queue (publishJSONToKafka)
import ProcessMessage (replaceNullChars)
import Relude
import System.Config (AuthContext (config, jobsPool), EnvConfig (..))
import System.Logging qualified as Log
import System.Types (ATAuthCtx, ATBackgroundCtx, ATBaseCtx, DB, RespHeaders, addRespHeaders)
import UnliftIO.Exception (tryAny)
import Utils (eitherStrToText)


data ReplayPost = ReplayPost
  { events :: AE.Value
  , sessionId :: UUID.UUID
  , timestamp :: UTCTime
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON, ToSchema)


data ReplayPost' = ReplayPost'
  { events :: AE.Value
  , sessionId :: UUID.UUID
  , timestamp :: UTCTime
  , projectId :: Projects.ProjectId
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


-- Helper function to publish to Pub/Sub using the queue service
publishReplayEvent :: ReplayPost -> Projects.ProjectId -> ATBaseCtx (Either Text Text)
publishReplayEvent replayData pid = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = ctx.config
  currentTime <- liftIO getCurrentTime

  let messagePayload = AE.object ["events" AE..= replayData.events, "sessionId" AE..= replayData.sessionId, "projectId" AE..= pid, "timestamp" AE..= replayData.timestamp]
  let attributes = HM.fromList [("eventType", "replay")]
  case envCfg.rrwebTopics of
    [] -> pure $ Left "No rrweb pubsub topics configured"
    (topicName : _) -> do
      liftIO
        $ publishJSONToKafka ctx topicName messagePayload attributes
        >>= \case
          Left err -> pure $ Left $ "Failed to publish replay event: " <> err
          Right messageId -> pure $ Right messageId


replayPostH :: Projects.ProjectId -> ReplayPost -> ATBaseCtx AE.Value
replayPostH pid body@ReplayPost{..} = do
  pubResult <- publishReplayEvent body pid
  case pubResult of
    Left errMsg -> pure $ AE.object ["status" AE..= ("warning" :: Text), "message" AE..= errMsg, "sessionId" AE..= sessionId]
    Right messageId -> do pure $ AE.object ["status" AE..= ("ok" :: Text), "messageId" AE..= messageId, "sessionId" AE..= sessionId]


processReplayEvents :: [(Text, ByteString)] -> HashMap Text Text -> ATBackgroundCtx [Text]
processReplayEvents [] _ = pure []
processReplayEvents msgs attrs = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = ctx.config
  let msgs' =
        msgs <&> \(ackId, msg) -> do
          let sanitizedJsonStr = replaceNullChars $ decodeUtf8 msg
          rrMsg <- eitherStrToText $ AE.eitherDecode $ encodeUtf8 sanitizedJsonStr
          Right (ackId, rrMsg)
  vs <- mapM (saveReplayMinio envCfg ctx.jobsPool) (rights msgs')
  pure $ catMaybes vs


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


-- | Download one object as a JSON array. Transport errors propagate through the
-- `Minio` monad as `Left MinioErr`; decode failures are returned as `Left String`.
fetchEventArray :: Minio.Bucket -> Minio.Object -> Bool -> Minio.Minio (Either String AE.Array)
fetchEventArray bucket objKey gzipped = do
  src <- Minio.getObject bucket objKey Minio.defaultGetObjectOptions
  bs <- runConduit $ Minio.gorObjectStream src .| CC.foldMap fromStrict
  let decoded = if gzipped then GZip.decompress bs else bs
  pure $ case AE.eitherDecode decoded of
    Right (AE.Array arr) -> Right arr
    Right v -> Left $ "expected JSON array, got " <> describeValue v
    Left e -> Left e


-- | Resolve per-project or default S3 credentials into a connection + bucket.
projectMinioConn :: EnvConfig -> Projects.Project -> (Minio.ConnectInfo, Minio.Bucket)
projectMinioConn envCfg p =
  let (acc, sec, region, bucket, endpoint) =
        maybe
          (envCfg.s3AccessKey, envCfg.s3SecretKey, envCfg.s3Region, envCfg.s3Bucket, envCfg.s3Endpoint)
          (\x -> (x.accessKey, x.secretKey, x.region, x.bucket, x.endpointUrl))
          p.s3Bucket
   in (getMinioConnectInfo acc sec region bucket endpoint, bucket)


-- | Fetch every tracked key individually, tolerating per-key failures (NoSuchKey is
-- silently skipped as a concurrent-merge artifact; transport and decode errors are
-- logged and the key is dropped). Returns merged arrays plus a partial-failure flag
-- that is `True` if any key was lost to a non-NoSuchKey error.
fetchIndividuals
  :: Minio.ConnectInfo
  -> Minio.Bucket
  -> [Text]
  -> HashMap Text Text
  -> ATAuthCtx (AE.Array, Bool)
fetchIndividuals conn bucket fileKeys logCtx = do
  perKey <- liftIO $ forM fileKeys $ \k ->
    Minio.runMinio conn (fetchEventArray bucket (fromString $ toString k) False) <&> (k,)
  let (arrs, decodeErrs, transportErrs) = foldr classify ([], [], []) perKey
      classify (_, Right (Right a)) (as, ds, ts) = (a : as, ds, ts)
      classify (k, Right (Left de)) (as, ds, ts) = (as, (k, de) : ds, ts)
      classify (k, Left me) (as, ds, ts)
        | isNoSuchKey me = (as, ds, ts)
        | otherwise = (as, ds, (k, displayException me) : ts)
  unless (null decodeErrs)
    $ Log.logAttention "Decode errors while reading replay event files" (HM.insert "errors" (toText $ show decodeErrs) logCtx)
  unless (null transportErrs)
    $ Log.logAttention "Transport errors while reading replay event files" (HM.insert "errors" (toText $ show transportErrs) logCtx)
  pure (mergeEventArrays arrs, not (null decodeErrs && null transportErrs))


-- | Fetch events for a session. Returns `Left` with a user-facing message on
-- unrecoverable failure, `Right` on success (including empty or partial results).
getSessionEvents :: Minio.ConnectInfo -> Projects.ProjectId -> Minio.Bucket -> UUID.UUID -> ATAuthCtx (Either Text (AE.Array, Bool))
getSessionEvents conn pid bucket sessionId = do
  let sessionStr = UUID.toText sessionId
      mergedKey = fromString $ toString $ sessionStr <> "/merged.json.gz"
      logCtx = HM.fromList [("session", sessionStr), ("projectId", pid.toText)]
      legacy reason = do
        Log.logInfo "Falling back to legacy replay blob" (HM.insert "reason" reason logCtx)
        legacyRes <- getMinioFile conn bucket (fromString $ toString $ sessionStr <> ".json")
        case legacyRes of
          Right a -> pure $ Right (a, False)
          Left e -> do
            Log.logError "Failed to load legacy replay blob" (HM.insert "error" e logCtx)
            pure $ Left "Temporarily unavailable; please retry."
      transientErr = Left "Temporarily unavailable; please retry."
      corruptedErr = Left "Session history is corrupted; please contact support."
  fileKeys <- sessionFileKeys pid sessionId
  mergedRes <- liftIO $ Minio.runMinio conn $ fetchEventArray bucket mergedKey True
  case mergedRes of
    -- Corrupt merged.json.gz is a data-integrity event — do NOT silently fall through
    -- to individuals/legacy, that would hide the pre-merge history from the user.
    Right (Left decodeErr) -> do
      Log.logError "Corrupt merged replay blob" (HM.insert "error" (toText decodeErr) logCtx)
      pure corruptedErr
    Left err | not (isNoSuchKey err) -> do
      Log.logError "Failed to fetch merged replay events" (HM.insert "error" (toText $ displayException err) logCtx)
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
          (arr, partial) <- fetchIndividuals conn bucket fileKeys logCtx
          let combined = mergeEventArrays [mergedEvents, arr]
          pure $ if partial && V.null combined then transientErr else Right (combined, partial)


-- | Lookup the tracked object keys for an unmerged replay session.
-- Empty list if the row doesn't exist (new session) or has a NULL/empty column.
sessionFileKeys :: DB es => Projects.ProjectId -> UUID.UUID -> Eff es [Text]
sessionFileKeys pid sessionId = do
  rows :: [V.Vector Text] <- Hasql.interp [HI.sql| SELECT COALESCE(file_keys, '{}'::text[]) FROM projects.replay_sessions WHERE session_id = #{sessionId} AND project_id = #{pid} LIMIT 1 |]
  pure $ maybe [] V.toList (listToMaybe rows)


-- | Merge multiple already-sorted event arrays by sorting arrays based on their first event's timestamp, then concatenating
mergeEventArrays :: [AE.Array] -> AE.Array
mergeEventArrays arrays = V.concat $ sortWith firstTimestamp $ filter (not . V.null) arrays
  where
    firstTimestamp :: AE.Array -> Double
    firstTimestamp arr = fromMaybe 0 $ (arr V.!? 0) >>= AET.parseMaybe (AE.withObject "event" (AE..: "timestamp"))


mergeFileCountThreshold :: Int
mergeFileCountThreshold = 20


saveReplayMinio :: (DB es, Log :> es, Time :> es) => EnvConfig -> Pool Connection -> (Text, ReplayPost') -> Eff es (Maybe Text)
saveReplayMinio envCfg jobsPool (ackId, replayData) = do
  project <- Projects.projectById replayData.projectId
  case project of
    Just p -> do
      let session = UUID.toText replayData.sessionId
          (conn, bucket) = projectMinioConn envCfg p
      case replayData.events of
        AE.Array newEvents
          | V.null newEvents -> pure $ Just ackId
          | otherwise -> do
              now <- Time.currentTime
              let timeStr = toText $ formatTime defaultTimeLocale "%Y%m%dT%H%M%S%q" now
                  objKeyText = session <> "/" <> timeStr <> ".json"
                  objKey = fromString $ toString objKeyText
                  body = AE.encode (AE.Array newEvents)
                  bodySize = BL.length body
              res <- liftIO $ Minio.runMinio conn do
                Minio.putObject bucket objKey (CC.sourceLazy body) (Just bodySize) Minio.defaultPutObjectOptions
              case res of
                Right _ -> do
                  let (rSid, rPid) = (replayData.sessionId, replayData.projectId)
                  countRows :: [Int] <-
                    Hasql.interp
                      [HI.sql|
                    INSERT INTO projects.replay_sessions (session_id, project_id, last_event_at, event_file_count, file_keys)
                    VALUES (#{rSid}, #{rPid}, #{now}, 1, ARRAY[#{objKeyText}]::text[])
                    ON CONFLICT (session_id) DO UPDATE SET
                      last_event_at = #{now}, merged = FALSE,
                      event_file_count = projects.replay_sessions.event_file_count + 1,
                      file_keys = array_append(projects.replay_sessions.file_keys, #{objKeyText}),
                      updated_at = #{now}
                    RETURNING event_file_count
                  |]
                  let fileCount = fromMaybe 0 (listToMaybe countRows)
                  when (fileCount >= mergeFileCountThreshold) $ do
                    let jobPayload = AE.object ["tag" AE..= ("MergeReplaySession" :: Text), "contents" AE..= ([AE.toJSON replayData.projectId, AE.toJSON replayData.sessionId] :: [AE.Value])]
                    liftIO $ withResource jobsPool \conn' ->
                      void $ createJob conn' "background_jobs" jobPayload
                  pure $ Just ackId
                Left err -> do
                  Log.logAttention "Failed to save replay events to MinIO" (HM.fromList [("session", session), ("projectId", replayData.projectId.toText), ("error", toText $ displayException err)])
                  pure Nothing
        _ -> do
          Log.logAttention "Invalid events format - expected JSON array" (HM.fromList [("session", session), ("projectId", replayData.projectId.toText)])
          pure $ Just ackId
    Nothing -> pure $ Just ackId


replaySessionGetH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders AE.Value)
replaySessionGetH pid sessionId = do
  (_, p) <- Projects.sessionAndProject pid
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let (conn, bucket) = projectMinioConn ctx.config p
      sessionStr = UUID.toText sessionId
  result <- tryAny $ getSessionEvents conn pid bucket sessionId
  case result of
    Right (Right (replayEvents, partial)) ->
      addRespHeaders
        $ AE.object
          [ "events" AE..= AE.Array replayEvents
          , "partial" AE..= partial
          ]
    Right (Left userMsg) ->
      addRespHeaders $ AE.object ["events" AE..= AE.Array V.empty, "error" AE..= userMsg]
    Left err -> do
      Log.logAttention "Unexpected exception fetching replay session" (HM.fromList [("session", sessionStr), ("projectId", pid.toText), ("error", toText $ displayException err)])
      addRespHeaders $ AE.object ["events" AE..= AE.Array V.empty, "error" AE..= ("Temporarily unable to load session; please retry." :: Text)]


-- | Merge a specific replay session's S3 event files (triggered when file count exceeds threshold)
mergeReplaySession :: Projects.ProjectId -> UUID.UUID -> ATBackgroundCtx ()
mergeReplaySession pid sessionId = mergeOneSessionByKeys pid sessionId "(file count threshold)" $ const pass


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

  -- Process each session independently; a single failure must not stall the batch.
  forM_ sessions $ \(sessionId, projectId) -> do
    r <- tryAny $ mergeOneSessionByKeys projectId sessionId "" $ \_ ->
      Hasql.interpExecute_ [HI.sql| UPDATE projects.replay_sessions SET merged = TRUE, updated_at = #{now} WHERE session_id = #{sessionId} |]
    whenLeft_ r $ \err ->
      Log.logAttention "Replay session merge threw; continuing batch" (HM.fromList [("session_id", UUID.toText sessionId), ("project_id", projectId.toText), ("error", toText $ displayException err)])


-- | Look up a project + its S3 settings + tracked file keys and merge them into merged.json.gz.
-- Runs the extra DB finalization step after the S3 merge succeeds.
mergeOneSessionByKeys
  :: Projects.ProjectId
  -> UUID.UUID
  -> Text
  -> (UTCTime -> ATBackgroundCtx ())
  -> ATBackgroundCtx ()
mergeOneSessionByKeys pid sessionId logSuffix afterMerge = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let logCtx = HM.fromList [("session_id", UUID.toText sessionId), ("project_id", pid.toText)]
  projectM <- Projects.projectById pid
  case projectM of
    Nothing -> do
      -- Orphaned row (project gone). Flip merged=TRUE so we stop re-picking it every run.
      Log.logAttention "Project not found for replay session merge; marking merged" logCtx
      now' <- Time.currentTime
      Hasql.interpExecute_ [HI.sql| UPDATE projects.replay_sessions SET merged = TRUE, updated_at = #{now'} WHERE session_id = #{sessionId} AND project_id = #{pid} |]
    Just p -> do
      let (s3Conn, bucket) = projectMinioConn ctx.config p
      fileKeys <- sessionFileKeys pid sessionId
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
              let ctxWithErr = HM.insert "error" (toText $ displayException merr) logCtx
              case merr of
                -- Fatal: corrupt data will never succeed. Mark merged so we stop
                -- retrying forever and paging on-call.
                MergeDecodeFailed _ -> do
                  Log.logError "Replay session merge aborted due to corrupt data; marking merged to stop retries" ctxWithErr
                  now' <- Time.currentTime
                  Hasql.interpExecute_ [HI.sql| UPDATE projects.replay_sessions SET merged = TRUE, updated_at = #{now'} WHERE session_id = #{sessionId} AND project_id = #{pid} |]
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
                        (SELECT count(*) FROM unnest(file_keys) AS k WHERE k <> ALL(#{vFileKeys}::text[]))::int,
                        0
                      ),
                      updated_at = #{now'}
                    WHERE session_id = #{sessionId} AND project_id = #{pid}
                  |]
              if rowsAffected == 0
                then Log.logAttention "Post-merge UPDATE affected 0 rows; row vanished, skipping afterMerge" logCtx
                else do
                  afterMerge now'
                  let msg = if logSuffix == "" then "Merged replay session" else "Merged replay session " <> logSuffix
                  Log.logInfo msg ("session_id", UUID.toText sessionId)


-- | Merge the tracked S3 event files for a single session into one gzip-compressed file.
-- The merged.json.gz object is fetched-and-appended if it already exists; individual files
-- are deleted after. Aborts (via `throwIO MergeError`) on any decode/fetch failure to avoid
-- silently clobbering historical data.
mergeOneSession :: Minio.ConnectInfo -> Minio.Bucket -> UUID.UUID -> [Text] -> IO ()
mergeOneSession conn bucket sessionId fileKeys = do
  let mergedKey = fromString $ toString $ UUID.toText sessionId <> "/merged.json.gz"
      fileObjs = map (fromString . toString) fileKeys

  existingRes <- Minio.runMinio conn $ fetchEventArray bucket mergedKey True
  mergedExisting <- case existingRes of
    Right (Right arr) -> pure arr
    Right (Left decodeErr) -> throwIO $ MergeDecodeFailed [("merged.json.gz", decodeErr)]
    Left err
      | isNoSuchKey err -> pure V.empty
      | otherwise -> throwIO $ MergeFetchFailed err

  -- Fetch each key in its own runMinio so one NoSuchKey (e.g. left over from a crashed
  -- prior merge) doesn't abort the whole batch. Decode failures and genuine transport
  -- errors still abort — we will not silently clobber historical data.
  perKey <- forM (zip fileKeys fileObjs) $ \(k, obj) ->
    (k,) <$> Minio.runMinio conn (fetchEventArray bucket obj False)
  let (errs, newArrays) = foldr classify ([], []) perKey
      classify (_, Right (Right a)) (es, as) = (es, a : as)
      classify (k, Right (Left de)) (es, as) = ((k, de) : es, as)
      classify (k, Left me) (es, as)
        | isNoSuchKey me = (es, as)
        | otherwise = ((k, displayException me) : es, as)
  unless (null errs) $ throwIO $ MergeDecodeFailed errs

  let allEvents = mergeEventArrays (mergedExisting : newArrays)
      compressed = GZip.compress $ AE.encode (AE.Array allEvents)
  putRes <- Minio.runMinio conn $ do
    Minio.putObject bucket mergedKey (CC.sourceLazy compressed) (Just (BL.length compressed)) Minio.defaultPutObjectOptions
    forM_ fileObjs $ Minio.removeObject bucket
  case putRes of
    Right _ -> pass
    Left err -> throwIO $ MergePutFailed err
