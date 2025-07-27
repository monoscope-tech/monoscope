{-# LANGUAGE OverloadedStrings #-}

module Pages.Replay (replayPostH, ReplayPost, processReplayEvents) where

import Control.Lens
import Data.Aeson qualified as AE
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.UUID qualified as UUID
import Relude

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (UTCTime, formatTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale)
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Reader.Static qualified
import Models.Projects.Projects qualified as Projects
import Pkg.Queue (publishJSONToPubsub)
import RequestMessages (replaceNullChars)
import System.Config (AuthContext (config), EnvConfig (rrwebPubsubTopics))
import System.Directory (createDirectoryIfMissing)
import System.Types (ATBaseCtx)
import Utils (eitherStrToText)


data ReplayPost = ReplayPost
  { events :: AE.Value
  , sessionId :: UUID.UUID
  , timestamp :: UTCTime
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
  case envCfg.rrwebPubsubTopics of
    [] -> pure $ Left "No rrweb pubsub topics configured"
    (topicName : _) -> do
      liftIO
        $ publishJSONToPubsub ctx topicName messagePayload attributes >>= \case
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


processReplayEvents :: IOE :> es => [(Text, ByteString)] -> HashMap Text Text -> Eff es [Text]
processReplayEvents [] _ = pure []
processReplayEvents msgs attrs = do
  let msgs' =
        msgs <&> \(ackId, msg) -> do
          let sanitizedJsonStr = replaceNullChars $ decodeUtf8 msg
          rrMsg <- eitherStrToText $ AE.eitherDecode $ encodeUtf8 sanitizedJsonStr
          Right (ackId, rrMsg)

  if null $ rights msgs'
    then pure []
    else mapM saveReplayEvent (rights msgs')


saveReplayEvent :: IOE :> es => (Text, ReplayPost) -> Eff es Text
saveReplayEvent (ackId, replayData) = do
  let timestamp = formatTime defaultTimeLocale "%Y%m%d%H%M%S" replayData.timestamp
  let sessionDir = "static/public/sessions/" <> toString (UUID.toText replayData.sessionId)
  let filePath = sessionDir <> "/" <> timestamp <> ".json"
  liftIO $ createDirectoryIfMissing True sessionDir
  liftIO $ BL8.writeFile filePath (AE.encode replayData.events)
  pure ackId
