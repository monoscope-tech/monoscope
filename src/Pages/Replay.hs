{-# LANGUAGE OverloadedStrings #-}

module Pages.Replay (replayPostH, ReplayPost, processReplayEvents, replaySessionGetH) where

import Conduit (runConduit)
import Control.Lens
import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as BL
import Data.Conduit ((.|))
import Data.Conduit.Combinators qualified as CC
import Data.HashMap.Strict qualified as HM
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful (Eff, IOE, (:>))
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Reader.Static qualified
import Models.Projects.Projects qualified as Projects
import Network.Minio qualified as Minio
import Pages.S3 (getMinioConnectInfo)
import Pkg.Queue (publishJSONToKafka)
import Relude
import RequestMessages (replaceNullChars)
import System.Config (AuthContext (config), EnvConfig (..))
import System.Types (ATAuthCtx, ATBackgroundCtx, ATBaseCtx, RespHeaders, addRespHeaders)
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


replayPostH :: Projects.ProjectId -> Text -> ATBaseCtx AE.Value
replayPostH pid body = do
  case AE.eitherDecode' (encodeUtf8 body) of
    Left err -> pure $ AE.object ["status" AE..= ("error" :: Text), "message" AE..= ("Invalid JSON: " <> toString err)]
    Right replayData@ReplayPost{..} -> do
      pubResult <- publishReplayEvent replayData pid
      case pubResult of
        Left errMsg -> pure $ AE.object ["status" AE..= ("warning" :: Text), "message" AE..= errMsg]
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


getMinioFile :: IOE :> es => Minio.ConnectInfo -> Minio.Bucket -> Minio.Object -> Eff es AE.Array
getMinioFile conn bucket object = do
  res <- liftIO $ Minio.runMinio conn do
    src <- Minio.getObject bucket object Minio.defaultGetObjectOptions
    let sld = Minio.gorObjectStream src
    bs <- runConduit $ sld .| CC.foldMap fromStrict
    let v = case AE.eitherDecode bs of
          Right v' -> case v' of
            AE.Array a -> a
            _ -> V.empty
          Left _ -> V.empty
    pure v 
  whenRight V.empty res pure


saveReplayMinio :: (DB :> es, IOE :> es) => EnvConfig -> (Text, ReplayPost') -> Eff es (Maybe Text)
saveReplayMinio envCfg (ackId, replayData) = do
  project <- dbtToEff $ Projects.projectById replayData.projectId
  case project of
    Just p -> do
      let session = UUID.toText replayData.sessionId
          object =  session <> ".json"
          (acc, sec, region, bucket, endpoint) = maybe (envCfg.s3AccessKey, envCfg.s3SecretKey, envCfg.s3Region, envCfg.s3Bucket, envCfg.s3Endpoint) (\x -> (x.accessKey, x.secretKey, x.region, x.bucket, x.endpointUrl)) p.s3Bucket
      let conn = getMinioConnectInfo acc sec region bucket endpoint
      ds <- getMinioFile conn bucket object
      res <- liftIO $ Minio.runMinio conn do
        bExist <- Minio.bucketExists bucket
        unless bExist $ void $ Minio.makeBucket bucket Nothing
        let finalBody = case replayData.events of
              AE.Array a -> ds <> a
              _ -> ds
        let body = AE.encode (AE.Array finalBody)
            bodySize = BL.length body
        _ <- Minio.putObject bucket object (CC.sourceLazy body) (Just bodySize) Minio.defaultPutObjectOptions
        pass
      case res of
        Right _ -> pure $ Just ackId
        Left e -> pure Nothing
    Nothing -> pure $ Just ackId


replaySessionGetH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders AE.Value)
replaySessionGetH pid sessionId = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = ctx.config
  project <- dbtToEff $ Projects.projectById pid
  case project of
    Just p -> do
      let (acc, sec, region, bucket, endpoint) = maybe (envCfg.s3AccessKey, envCfg.s3SecretKey, envCfg.s3Region, envCfg.s3Bucket, envCfg.s3Endpoint) (\x -> (x.accessKey, x.secretKey, x.region, x.bucket, x.endpointUrl)) p.s3Bucket
          conn = getMinioConnectInfo acc sec region bucket endpoint
      replayEvents <- getMinioFile conn bucket (fromString $ toString $ UUID.toText sessionId <> ".json")
      addRespHeaders $ AE.object ["events" AE..= AE.Array replayEvents]
    Nothing -> do
      addRespHeaders $ AE.object ["events" AE..= AE.Array V.empty]
