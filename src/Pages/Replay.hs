{-# LANGUAGE OverloadedStrings #-}

module Pages.Replay (replayPostH, ReplayPost (..), processReplayEvents, replaySessionGetH) where

import Conduit (runConduit)
import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as BL
import Data.Aeson.Types qualified as AET
import Data.Conduit ((.|))
import Data.Conduit.Combinators qualified as CC
import Data.HashMap.Strict qualified as HM
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful (Eff, IOE, runEff, type (:>))
import Effectful.Reader.Static qualified
import Models.Projects.Projects qualified as Projects
import Network.Minio qualified as Minio
import Pages.S3 (getMinioConnectInfo)
import Pkg.Queue (publishJSONToKafka)
import Relude
import RequestMessages (replaceNullChars)
import System.Config (AuthContext (config), EnvConfig (..))
import System.Types (ATAuthCtx, ATBackgroundCtx, ATBaseCtx, DB, RespHeaders, addRespHeaders)
import Utils (eitherStrToText)
import UnliftIO.Exception (tryAny)
import System.Logging qualified as Log
import Effectful.Log (Log, object)

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
  traceShowM pubResult
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
getMinioFile conn bucket object = do
  res <- liftIO $ Minio.runMinio conn do
    src <- Minio.getObject bucket object Minio.defaultGetObjectOptions
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


getSessionEvents :: IOE :> es => Minio.ConnectInfo -> Minio.Bucket -> Text -> Eff es AE.Array
getSessionEvents conn bucket sessionId = do
  let prefix = sessionId <> "/"
  res <- liftIO $ Minio.runMinio conn $ do
    items <- runConduit $ Minio.listObjects bucket (Just prefix) True .| CC.sinkList
    let objectNames = [Minio.oiObject info | Minio.ListItemObject info <- items]
    eventArrays <- forM objectNames $ \objName -> do
      src <- Minio.getObject bucket objName Minio.defaultGetObjectOptions
      let stream = Minio.gorObjectStream src
      bs <- runConduit $ stream .| CC.foldMap fromStrict
      case AE.eitherDecode bs of
        Right (AE.Array arr) -> pure arr
        _ -> pure V.empty
    pure $ V.concat eventArrays
  case res of
    Right events
      | V.null events -> do
          -- Fallback: try legacy single-file format ({sessionId}.json)
          legacyEvents <- getMinioFile conn bucket (fromString $ toString $ sessionId <> ".json")
          pure $ sortEvents legacyEvents
      | otherwise -> pure $ sortEvents events
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
                  object = fromString $ toString $ session <> "/" <> timeStr <> ".json"
                  body = AE.encode (AE.Array newEvents)
                  bodySize = BL.length body
              res <- liftIO $ Minio.runMinio conn do
                Minio.putObject bucket object (CC.sourceLazy body) (Just bodySize) Minio.defaultPutObjectOptions
              case res of
                Right _ -> pure $ Just ackId
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

      result <- liftIO $ tryAny $ runEff $ getSessionEvents conn bucket sessionStr
      traceShowM result
      case result of
        Right replayEvents -> addRespHeaders $ AE.object ["events" AE..= AE.Array replayEvents]
        Left err -> do
          Log.logAttention "Error fetching replay session from MinIO" (HM.fromList [("session", sessionStr), ("projectId", pid.toText), ("error", toText $ show err)])
          addRespHeaders $ AE.object ["events" AE..= AE.Array V.empty, "error" AE..= ("Failed to load session" :: Text)]
    Nothing -> do
      addRespHeaders $ AE.object ["events" AE..= AE.Array V.empty, "error" AE..= ("Project not found" :: Text)]
