{-# LANGUAGE OverloadedStrings #-}

module Pages.Replay (replayPostH, ReplayPost, processReplayEvents, replaySessionGetH) where

import Control.Lens
import Data.Aeson qualified as AE
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.UUID qualified as UUID
import Relude

import Control.Error (hush)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Network.Minio qualified as Minio

import Conduit (runConduit)
import Data.Base64.Types qualified as B64
import Data.ByteString.Base64 qualified as B64
import Data.Conduit ((.|))
import Data.Conduit.Combinators qualified as CC
import Data.Time (UTCTime, formatTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Vector qualified as V
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Reader.Static qualified
import Lucid
import Lucid.Base (TermRaw (termRaw))
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import NeatInterpolation (text)
import OpenTelemetry.Resource.Telemetry (Telemetry (Telemetry))
import Pages.LogExplorer.Log (curateCols)
import Pkg.Parser (parseQueryToAST, toQText)
import Pkg.Queue (publishJSONToPubsub)
import RequestMessages (replaceNullChars)
import System.Config (AuthContext (config), EnvConfig (..))
import System.Directory (createDirectoryIfMissing)
import System.Types (ATAuthCtx, ATBackgroundCtx, ATBaseCtx, RespHeaders, addErrorToast, addRespHeaders)
import Text.Megaparsec (parseMaybe)
import Utils (checkFreeTierExceeded, eitherStrToText, faSprite_, getServiceColors, listToIndexHashMap, lookupVecTextByKey, onpointerdown_, prettyPrintCount)


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
        $ publishJSONToPubsub ctx topicName messagePayload attributes
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
        Left errMsg -> do
          pure $ AE.object ["status" AE..= ("warning" :: Text), "message" AE..= errMsg]
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


getMinioConnectInfo :: EnvConfig -> Minio.ConnectInfo
getMinioConnectInfo envCfg = Minio.setCreds (Minio.CredentialValue accessKey secretKey Nothing) withRegion
  where
    withRegion = Minio.setRegion (fromString $ toString envCfg.s3Region) info
    info = fromString $ toString envCfg.s3Endpoint
    accessKey = fromString $ toString envCfg.s3AccessKey
    secretKey = fromString $ toString envCfg.s3SecretKey


saveReplayMinio :: IOE :> es => EnvConfig -> (Text, ReplayPost) -> Eff es (Maybe Text)
saveReplayMinio envCfg (ackId, replayData) = do
  let session = UUID.toText (sessionId replayData)
      object = session <> ".json"
  let conn = getMinioConnectInfo envCfg
      bucket = fromString $ toString envCfg.s3Bucket
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


replaySessionGetH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders (Html ()))
replaySessionGetH pid sessionId = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = ctx.config
  events <- Telemetry.getEventsBySessionId pid (UUID.toText sessionId)
  let parseQuery q = either (\err -> addErrorToast "Error Parsing Query" (Just err) >> pure []) pure (parseQueryToAST q)
  queryAST <- parseQuery $ "attributes___session___id ==\"" <> UUID.toText sessionId <> "\""
  let queryText = toQText queryAST

  -- child spans might not have session id but are part of the session

  tableAsVecE <- RequestDumps.selectLogTable pid queryAST queryText Nothing (Nothing, Nothing) [] Nothing Nothing
  let tableAsVecM = hush tableAsVecE

  case tableAsVecM of
    Just tableAsVec -> do
      let (requestVecs, colNames, resultCount) = tableAsVec
          curatedColNames = nubOrd $ curateCols [] colNames
          colIdxMap = listToIndexHashMap colNames
          traceIds = V.map (\v -> lookupVecTextByKey v colIdxMap "trace_id") requestVecs
      extraEvents <- Telemetry.getLogsByTraceIds pid (V.catMaybes traceIds)

      let conn = getMinioConnectInfo ctx.config
      replayEvents <- getMinioFile conn (fromString $ toString envCfg.s3Bucket) (fromString $ toString $ UUID.toText sessionId <> ".json")
      let eventsJson = decodeUtf8 $ AE.encode (AE.Array replayEvents)
      let finalVecs = requestVecs <> extraEvents
          serviceNames = V.map (\v -> lookupVecTextByKey v colIdxMap "span_name") finalVecs
          colors = getServiceColors (V.catMaybes serviceNames)

          page =
            ReplayPage
              { requestVecs = finalVecs
              , cols = curatedColNames
              , colIdxMap = colIdxMap
              , serviceColors = colors
              , nextLogsURL = Nothing
              , count = resultCount
              , recentLogsURL = Nothing
              , resetLogsURL = Nothing
              , pid = pid
              }
      addRespHeaders $ replaySessionPage pid sessionId (Just page) eventsJson
    Nothing -> addRespHeaders $ replaySessionPage pid sessionId Nothing "[]"


data ReplayPage = ReplayPage
  { requestVecs :: V.Vector (V.Vector AE.Value)
  , cols :: [Text]
  , colIdxMap :: HashMap Text Int
  , serviceColors :: HashMap Text Text
  , nextLogsURL :: Maybe Text
  , count :: Int
  , recentLogsURL :: Maybe Text
  , resetLogsURL :: Maybe Text
  , pid :: Projects.ProjectId
  }


replaySessionPage :: Projects.ProjectId -> UUID.UUID -> Maybe ReplayPage -> Text -> Html ()
replaySessionPage pid sessionId page eventsJson = do
  div_ [class_ "w-full flex flex-col"] $ do
    div_ [class_ "p-4 border-b bg-gradient-to-r from-blue-50 to-purple-50"] $ do
      div_ [class_ "flex items-center justify-between"] $ do
        div_ $ do
          h2_ [id_ "sessionTitle", class_ "text-xl font-bold"] "Session replay"
        div_ [class_ "flex gap-2"] $ do
          button_ [class_ "px-3 py-1.5 text-sm border border-gray-300 rounded-md hover:bg-gray-50 flex items-center gap-2"] $ do
            "Export"
    div_ [class_ "rounded-lg overflow-hidden ml-4 border p-2 border-strokeWeak bg-gradient-to-br from-slate-900 via-blue-900 to-purple-900 cursor-pointer"] do
      div_ [id_ "videoContainer", class_ "overflow-hidden w-max mx-auto"] pass
    div_ [class_ "p-4 border-b flex items-center justify-between"] $ do
      div_ [class_ "flex items-center gap-2"] $ do
        h3_ [class_ "font-semibold"] "Session Events"
        span_ [id_ "logCount", class_ "bg-gray-100 text-gray-800 px-2 py-1 rounded text-sm"] $ show (maybe 0 (.count) page)
        span_ [id_ "errorCount", class_ "bg-red-100 text-red-800 px-2 py-1 rounded text-sm hidden"] "2 errors"
      button_ [class_ "px-3 py-1.5 text-sm border border-gray-300 rounded-md hover:bg-gray-50 flex items-center gap-2"] $ do
        "Filter"
    whenJust page $ \p -> do
      div_ [class_ "flex flex-col h-[400px]"] do
        termRaw "log-list" [id_ "resultTable", class_ "w-full divide-y shrink-1 flex flex-col h-full min-w-0", term "windowTarget" "sessionList"] ("" :: Text)

      let logs = decodeUtf8 $ AE.encode p.requestVecs
          cols = decodeUtf8 $ AE.encode p.cols
          colIdxMap = decodeUtf8 $ AE.encode p.colIdxMap
          serviceColors = decodeUtf8 $ AE.encode p.serviceColors
          projectid = p.pid.toText

      script_
        [text|
         window.sessionListData = {
          requestVecs: $logs,
          cols: $cols,
          colIdxMap: $colIdxMap,
          serviceColors: $serviceColors,
          projectId: "$projectid",
         }
      |]
  script_
    [text|
      var videoContainer = document.getElementById('videoContainer');
      new rrwebPlayer({target: videoContainer, props: {autoPlay:true, width:852, height:300, events: $eventsJson }});
  |]
