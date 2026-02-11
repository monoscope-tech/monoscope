{-# LANGUAGE OverloadedStrings #-}

module Pages.Replay (replayPostH, ReplayPost (..), processReplayEvents, replaySessionGetH, compressAndMergeReplaySessions, saveMergedReplayEvents) where

import Codec.Compression.GZip qualified as GZip
import Conduit (runConduit)
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.ByteString.Lazy qualified as BL
import Data.Conduit ((.|))
import Data.Conduit.Combinators qualified as CC
import Data.List (partition)
import Data.HashMap.Strict qualified as HM
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Only (..))
import Effectful (Eff, IOE, runEff, type (:>))
import Effectful.Log (Log)
import Effectful.PostgreSQL qualified as PG
import Effectful.Reader.Static qualified
import Models.Projects.Projects qualified as Projects
import Network.Minio qualified as Minio
import Pages.S3 (getMinioConnectInfo)
import Pkg.Queue (publishJSONToKafka)
import Relude
import RequestMessages (replaceNullChars)
import System.Config (AuthContext (config, jobsPool), EnvConfig (..))
import System.Logging qualified as Log
import System.Types (ATAuthCtx, ATBackgroundCtx, ATBaseCtx, DB, RespHeaders, addRespHeaders)
import Data.Pool (withResource)
import OddJobs.Job (createJob)
import UnliftIO.Exception (tryAny)
import Utils (eitherStrToText)


data ReplayPost = ReplayPost
  { events :: AE.Value
  , sessionId :: UUID.UUID
  , timestamp :: UTCTime
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


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
  vs <- mapM (saveReplayMinio envCfg) (rights msgs')
  pure $ catMaybes vs


-- | Retrieve replay events from a single MinIO/S3 object (legacy format)
getMinioFile :: IOE :> es => Minio.ConnectInfo -> Minio.Bucket -> Minio.Object -> Eff es AE.Array
getMinioFile conn bucket objKey = do
  res <- liftIO $ Minio.runMinio conn do
    src <- Minio.getObject bucket objKey Minio.defaultGetObjectOptions
    let sld = Minio.gorObjectStream src
    bs <- runConduit $ sld .| CC.foldMap fromStrict
    case AE.eitherDecode bs of
      Right v' -> case v' of
        AE.Array a -> pure a
        _ -> pure V.empty
      Left _ -> pure V.empty
  case res of
    Right arr -> pure arr
    Left _ -> pure V.empty


getSessionEvents :: Minio.ConnectInfo -> Projects.ProjectId -> Minio.Bucket -> Text -> ATAuthCtx AE.Array
getSessionEvents conn pid bucket sessionId = do
  let prefix = sessionId <> "/"
      mergedKey = fromString $ toString $ sessionId <> "/merged.json.gz"
  ctx <- Effectful.Reader.Static.ask @AuthContext
  res <- liftIO $ Minio.runMinio conn $ do
    items <- runConduit $ Minio.listObjects bucket (Just prefix) True .| CC.sinkList
    let objectNames = [Minio.oiObject info | Minio.ListItemObject info <- items]
        (mergedFiles, individualFiles) = partition (\n -> n == mergedKey) objectNames
        hasMerged = not $ null mergedFiles
        hasIndividual = not $ null individualFiles
    mergedEvents <- if hasMerged
      then do
        src <- Minio.getObject bucket mergedKey Minio.defaultGetObjectOptions
        let stream = Minio.gorObjectStream src
        compressed <- runConduit $ stream .| CC.foldMap fromStrict
        let decompressed = GZip.decompress compressed
        case AE.eitherDecode decompressed of
          Right (AE.Array arr) -> pure arr
          _ -> pure V.empty
      else pure V.empty
    individualEvents <- forM individualFiles $ \objName -> do
      src <- Minio.getObject bucket objName Minio.defaultGetObjectOptions
      let stream = Minio.gorObjectStream src
      bs <- runConduit $ stream .| CC.foldMap fromStrict
      case AE.eitherDecode bs of
        Right (AE.Array arr) -> pure arr
        _ -> pure V.empty
    let allEvents = sortEvents $ V.concat (mergedEvents : individualEvents)
    when hasIndividual $ do
      let jobPayload = AE.object [ "tag" AE..= ("SaveMergedReplayEvents" :: Text), "contents" AE..= ([AE.toJSON pid, AE.toJSON sessionId, AE.Array allEvents] :: [AE.Value])]
      liftIO $ withResource ctx.jobsPool \conn' ->
              void $ createJob conn' "background_jobs" jobPayload
    pure allEvents
  case res of
    Right events
      | V.null events -> do
          -- Fallback: try legacy single-file format ({sessionId}.json)
          legacyEvents <- getMinioFile conn bucket (fromString $ toString $ sessionId <> ".json")
          pure $ sortEvents legacyEvents
      | otherwise -> pure events
    Left _ -> do
      legacyEvents <- getMinioFile conn bucket (fromString $ toString $ sessionId <> ".json")
      pure $ sortEvents legacyEvents


sortEvents :: AE.Array -> AE.Array
sortEvents events = V.fromList $ sortBy (comparing getEventTimestamp) $ V.toList events
  where
    getEventTimestamp :: AE.Value -> Double
    getEventTimestamp val = fromMaybe 0 $ AET.parseMaybe (AE.withObject "event" (AE..: "timestamp")) val


saveReplayMinio :: (DB es, Log :> es) => EnvConfig -> (Text, ReplayPost') -> Eff es (Maybe Text)
saveReplayMinio envCfg (ackId, replayData) = do
  project <- Projects.projectById replayData.projectId
  case project of
    Just p -> do
      let session = UUID.toText replayData.sessionId
          (acc, sec, region, bucket, endpoint) = maybe (envCfg.s3AccessKey, envCfg.s3SecretKey, envCfg.s3Region, envCfg.s3Bucket, envCfg.s3Endpoint) (\x -> (x.accessKey, x.secretKey, x.region, x.bucket, x.endpointUrl)) p.s3Bucket
          conn = getMinioConnectInfo acc sec region bucket endpoint

      case replayData.events of
        AE.Array newEvents
          | V.null newEvents -> pure $ Just ackId
          | otherwise -> do
              now <- liftIO getCurrentTime
              let timeStr = toText $ formatTime defaultTimeLocale "%Y%m%dT%H%M%S%q" now
                  objKey = fromString $ toString $ session <> "/" <> timeStr <> ".json"
                  body = AE.encode (AE.Array newEvents)
                  bodySize = BL.length body
              res <- liftIO $ Minio.runMinio conn do
                Minio.putObject bucket objKey (CC.sourceLazy body) (Just bodySize) Minio.defaultPutObjectOptions
              case res of
                Right _ -> do
                  _ <- PG.execute [sql|
                    INSERT INTO projects.replay_sessions (session_id, project_id, last_event_at)
                    VALUES (?, ?, now())
                    ON CONFLICT (session_id) DO UPDATE SET last_event_at = now(), merged = FALSE, updated_at = now()
                  |] (replayData.sessionId, replayData.projectId)
                  pure $ Just ackId
                Left err -> do
                  Log.logAttention "Failed to save replay events to MinIO" (HM.fromList [("session", session), ("projectId", replayData.projectId.toText), ("error", toText $ show err)])
                  pure Nothing
        _ -> do
          Log.logAttention "Invalid events format - expected JSON array" (HM.fromList [("session", session), ("projectId", replayData.projectId.toText)])
          pure $ Just ackId
    Nothing -> pure $ Just ackId


replaySessionGetH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders AE.Value)
replaySessionGetH pid sessionId = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = ctx.config
  project <- Projects.projectById pid
  case project of
    Just p -> do
      let (acc, sec, region, bucket, endpoint) = maybe (envCfg.s3AccessKey, envCfg.s3SecretKey, envCfg.s3Region, envCfg.s3Bucket, envCfg.s3Endpoint) (\x -> (x.accessKey, x.secretKey, x.region, x.bucket, x.endpointUrl)) p.s3Bucket
          conn = getMinioConnectInfo acc sec region bucket endpoint
          sessionStr = UUID.toText sessionId
      result <- tryAny $ getSessionEvents conn pid bucket sessionStr
      case result of
        Right replayEvents -> addRespHeaders $ AE.object ["events" AE..= AE.Array replayEvents]
        Left err -> do
          Log.logAttention "Error fetching replay session from MinIO" (HM.fromList [("session", sessionStr), ("projectId", pid.toText), ("error", toText $ show err)])
          addRespHeaders $ AE.object ["events" AE..= AE.Array V.empty, "error" AE..= ("Failed to load session" :: Text)]
    Nothing -> do
      addRespHeaders $ AE.object ["events" AE..= AE.Array V.empty, "error" AE..= ("Project not found" :: Text)]


-- | Save pre-merged replay events to the bucket, replacing individual files
saveMergedReplayEvents :: Projects.ProjectId -> UUID.UUID -> AE.Value -> ATBackgroundCtx ()
saveMergedReplayEvents pid sessionId events = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = ctx.config
  projectM <- Projects.projectById pid
  case projectM of
    Nothing -> Log.logAttention "Project not found for saving merged replay events" (HM.fromList [("session_id", UUID.toText sessionId), ("project_id", pid.toText)])
    Just p -> do
      let (acc, sec, region, bucket, endpoint) = maybe (envCfg.s3AccessKey, envCfg.s3SecretKey, envCfg.s3Region, envCfg.s3Bucket, envCfg.s3Endpoint) (\x -> (x.accessKey, x.secretKey, x.region, x.bucket, x.endpointUrl)) p.s3Bucket
          s3Conn = getMinioConnectInfo acc sec region bucket endpoint
          session = UUID.toText sessionId
          prefix = session <> "/"
          mergedKey = fromString $ toString $ session <> "/merged.json.gz"
          compressed = GZip.compress $ AE.encode events
          compressedSize = BL.length compressed
      result <- liftIO $ tryAny $ Minio.runMinio s3Conn $ do
        Minio.putObject bucket mergedKey (CC.sourceLazy compressed) (Just compressedSize) Minio.defaultPutObjectOptions
        items <- runConduit $ Minio.listObjects bucket (Just prefix) True .| CC.sinkList
        let objectNames = [Minio.oiObject info | Minio.ListItemObject info <- items]
        forM_ objectNames $ \objName ->
          when (objName /= mergedKey) $
            Minio.removeObject bucket objName
      case result of
        Right (Right _) -> do
          _ <- PG.execute [sql|
            UPDATE projects.replay_sessions SET merged = TRUE, updated_at = now() WHERE session_id = ? AND project_id = ?
          |] (sessionId, pid)
          Log.logInfo "Saved merged replay events" ("session_id", UUID.toText sessionId)
        Right (Left err) ->
          Log.logAttention "Minio error saving merged replay events" (HM.fromList [("session_id", UUID.toText sessionId), ("project_id", pid.toText), ("error", toText $ show err)])
        Left err ->
          Log.logAttention "Failed to save merged replay events" (HM.fromList [("session_id", UUID.toText sessionId), ("project_id", pid.toText), ("error", toText $ show err)])


-- | Find inactive sessions and compress & merge their S3 event files
compressAndMergeReplaySessions :: ATBackgroundCtx ()
compressAndMergeReplaySessions = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = ctx.config

  sessions <- PG.query [sql|
    SELECT session_id, project_id FROM projects.replay_sessions
    WHERE merged = FALSE AND last_event_at < now() - interval '30 minutes'
    LIMIT 100
  |] ()

  Log.logInfo "Starting replay session merge job" ("sessions_count", length (sessions :: [(UUID.UUID, Projects.ProjectId)]))

  forM_ sessions $ \(sessionId, projectId) -> do
    projectM <- Projects.projectById projectId
    case projectM of
      Nothing -> Log.logAttention "Project not found for replay session merge" (HM.fromList [("session_id", UUID.toText sessionId), ("project_id", projectId.toText)])
      Just p -> do
        let (acc, sec, region, bucket, endpoint) = maybe (envCfg.s3AccessKey, envCfg.s3SecretKey, envCfg.s3Region, envCfg.s3Bucket, envCfg.s3Endpoint) (\x -> (x.accessKey, x.secretKey, x.region, x.bucket, x.endpointUrl)) p.s3Bucket
            s3Conn = getMinioConnectInfo acc sec region bucket endpoint
        result <- liftIO $ tryAny $ mergeOneSession s3Conn bucket sessionId
        case result of
          Right _ -> do
            _ <- PG.execute [sql|
              UPDATE projects.replay_sessions SET merged = TRUE, updated_at = now() WHERE session_id = ?
            |] (Only sessionId)
            Log.logInfo "Merged replay session" ("session_id", UUID.toText sessionId)
          Left err ->
            Log.logAttention "Failed to merge replay session" (HM.fromList [("session_id", UUID.toText sessionId), ("project_id", projectId.toText), ("error", toText $ show err)])


-- | Merge all S3 event files for a single session into one gzip-compressed file
mergeOneSession :: Minio.ConnectInfo -> Minio.Bucket -> UUID.UUID -> IO ()
mergeOneSession conn bucket sessionId = do
  let session = UUID.toText sessionId
      prefix = session <> "/"
      mergedKey = fromString $ toString $ session <> "/merged.json.gz"

  res <- Minio.runMinio conn $ do
    -- List all objects in the session prefix
    items <- runConduit $ Minio.listObjects bucket (Just prefix) True .| CC.sinkList
    let objectNames = [Minio.oiObject info | Minio.ListItemObject info <- items]

    unless (null objectNames) $ do
      -- Download all objects (decompress merged.json.gz if present)
      eventArrays <- forM objectNames $ \objName -> do
        src <- Minio.getObject bucket objName Minio.defaultGetObjectOptions
        let stream = Minio.gorObjectStream src
        bs <- runConduit $ stream .| CC.foldMap fromStrict
        if objName == mergedKey
          then case AE.eitherDecode (GZip.decompress bs) of
            Right (AE.Array arr) -> pure arr
            _ -> pure V.empty
          else case AE.eitherDecode bs of
            Right (AE.Array arr) -> pure arr
            _ -> pure V.empty

      let allEvents = sortEvents $ V.concat eventArrays
          compressed = GZip.compress $ AE.encode (AE.Array allEvents)
          compressedSize = BL.length compressed

      Minio.putObject bucket mergedKey (CC.sourceLazy compressed) (Just compressedSize) Minio.defaultPutObjectOptions

      forM_ objectNames $ \objName ->
        when (objName /= mergedKey) $
          Minio.removeObject bucket objName

  case res of
    Right _ -> pure ()
    Left err -> error $ "Failed to merge session " <> toText session <> ": " <> toText (show err)
