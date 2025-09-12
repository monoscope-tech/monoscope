{-# LANGUAGE StrictData #-}

module Opentelemetry.OtlpServer (processList, runServer) where

import Control.Exception.Annotated (checkpoint)
import Control.Parallel.Strategies (parList, rpar, using)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as KEM
import Data.Base64.Types qualified as B64
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.Cache qualified as Cache
import Data.Effectful.UUID (UUIDEff)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as L
import Data.List.Extra (chunksOf)
import Data.Map qualified as Map
import Data.ProtoLens.Encoding (decodeMessage)
import Data.Scientific (fromFloatDigits)
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Exception (onException)
import Effectful.Labeled (Labeled)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (ask)
import Effectful.Reader.Static qualified as Eff
import Log qualified
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.SummaryGenerator (generateSummary)
import Models.Telemetry.Telemetry (Context (..), OtelLogsAndSpans (..), Severity (..))
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server (SomeRpcHandler)
import Network.GRPC.Server.Run hiding (runServer)
import Network.GRPC.Server.StreamType
import OpenTelemetry.Trace (TracerProvider)
import Pkg.DeriveUtils (AesonText (..))
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService qualified as LS
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as MS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as TS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as TSF
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as PC
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as PCF
import Proto.Opentelemetry.Proto.Logs.V1.Logs qualified as PL
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as PLF
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as PM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as PMF
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as PR
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields qualified as PRF
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as PT
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as PTF
import Relude hiding (ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (runBackground)
import Utils (b64ToJson, freeTierDailyMaxEvents, nestedJsonFromDotNotation)


-- | Minimum valid timestamp in nanoseconds (Year 2000)
-- We consider any timestamp before this as invalid/unset
minValidTimestampNanos :: Word64
minValidTimestampNanos = 946684800000000000


-- | Default project cache for when none is found
defaultProjectCache :: Projects.ProjectCache
defaultProjectCache =
  Projects.ProjectCache
    { hosts = []
    , endpointHashes = []
    , shapeHashes = []
    , redactFieldslist = []
    , dailyEventCount = 0
    , dailyMetricCount = 0
    , paymentPlan = "Free"
    }


-- import Network.GRPC.Server.Service (Service, service, method, fromServices)

-- | Generic lens-based attribute extraction from spans
getSpanAttributeValue :: Text -> V.Vector PT.ResourceSpans -> V.Vector Text
getSpanAttributeValue !attribute = V.mapMaybe (\rs -> getResourceAttr rs <|> getSpanAttr rs)
  where
    getResourceAttr rs = getAttr (resourceAttributes (rs ^. PTF.resource))
    getSpanAttr rs =
      let spans = V.fromList (rs ^. PTF.scopeSpans) >>= (\s -> V.fromList (s ^. PTF.spans))
       in listToMaybe $ V.toList $ V.mapMaybe (getAttr . spanAttributes) spans

    getAttr :: V.Vector PC.KeyValue -> Maybe Text
    getAttr kvs = V.find ((== attribute) . (^. PCF.key)) kvs >>= \kv -> Just (kv ^. PCF.value . PCF.stringValue)

    resourceAttributes resource = V.fromList $ resource ^. PRF.attributes
    spanAttributes spn = V.fromList $ spn ^. PTF.attributes


-- | Extract attribute values from resource logs
getLogAttributeValue :: Text -> V.Vector PL.ResourceLogs -> V.Vector Text
getLogAttributeValue !attribute = V.mapMaybe (\rl -> getResourceAttr rl <|> getLogAttr rl)
  where
    getResourceAttr rl = getAttr (resourceAttributes (rl ^. PLF.resource))
    getLogAttr rl =
      let logs = V.fromList (rl ^. PLF.scopeLogs) >>= (\s -> V.fromList (s ^. PLF.logRecords))
       in listToMaybe $ V.toList $ V.mapMaybe (getAttr . logAttributes) logs

    getAttr :: V.Vector PC.KeyValue -> Maybe Text
    getAttr kvs = V.find ((== attribute) . (^. PCF.key)) kvs >>= \kv -> Just (kv ^. PCF.value . PCF.stringValue)

    resourceAttributes resource = V.fromList $ resource ^. PRF.attributes
    logAttributes log = V.fromList $ log ^. PLF.attributes


-- | Extract metric attribute values
getMetricAttributeValue :: Text -> V.Vector PM.ResourceMetrics -> Maybe Text
getMetricAttributeValue !attribute !rms = listToMaybe $ V.toList $ V.mapMaybe getResourceAttr rms
  where
    getResourceAttr rm = getAttr (resourceAttributes (rm ^. PMF.resource))

    getAttr :: V.Vector PC.KeyValue -> Maybe Text
    getAttr kvs = V.find ((== attribute) . (^. PCF.key)) kvs >>= \kv -> Just (kv ^. PCF.value . PCF.stringValue)

    resourceAttributes resource = V.fromList $ resource ^. PRF.attributes


-- | Process a list of messages
processList :: (Concurrent :> es, DB :> es, Eff.Reader AuthContext :> es, IOE :> es, Labeled "timefusion" DB :> es, Log :> es, UUIDEff :> es) => [(Text, ByteString)] -> HashMap Text Text -> Eff es [Text]
processList [] _ = pure []
processList msgs !attrs = checkpoint "processList" $ do
  startTime <- liftIO getCurrentTime

  (result, processingTime, dbInsertTime) <- process startTime `onException` handleException

  endTime <- liftIO getCurrentTime
  let duration = diffUTCTime endTime startTime
      eventCount = length msgs
  Log.logInfo
    "processList: batch processing completed"
    ( AE.object
        [ "ce-type" AE..= HashMap.lookup "ce-type" attrs
        , "event_count" AE..= eventCount
        , "duration_seconds" AE..= realToFrac @_ @Double duration
        , "duration_ms" AE..= (round (duration * 1000) :: Int)
        , "processing_ms" AE..= processingTime
        , "db_insert_ms" AE..= dbInsertTime
        ]
    )
  pure result
  where
    handleException = checkpoint "processList:exception" $ do
      Log.logAttention "processList: caught exception" (AE.object ["ce-type" AE..= HashMap.lookup "ce-type" attrs, "msg_count" AE..= length msgs, "attrs" AE..= attrs])
      pure (map fst msgs, 0, 0)

    process fallbackTime = do
      appCtx <- ask @AuthContext

      case HashMap.lookup "ce-type" attrs of
        Just "org.opentelemetry.otlp.logs.v1" -> checkpoint "processList:logs" $ do
          processingStartTime <- liftIO getCurrentTime
          let !msgsCount = length msgs

          -- Decode messages in parallel
          let !decodedMsgs =
                [(ackId, decodeMessage msg :: Either String LS.ExportLogsServiceRequest) | (ackId, msg) <- msgs]
                  `using` parList rpar

          -- Extract all unique project keys efficiently
          let !allProjectKeys =
                V.force
                  $ V.concat
                    [ getLogAttributeValue "at-project-key" (V.fromList $ logReq ^. PLF.resourceLogs)
                    | (_, Right logReq) <- decodedMsgs
                    ]
              !uniqueProjectKeys = V.force $ V.fromList $ L.nub $ V.toList allProjectKeys
              atIds' =
                V.concat
                  [ getLogAttributeValue "at-project-id" (V.fromList $ logReq ^. PLF.resourceLogs)
                  | (_, Right logReq) <- decodedMsgs
                  ]
              atIds = V.catMaybes $ Projects.projectIdFromText <$> atIds'

          -- Batch fetch all project data in one go
          (!allProjectIdsAndKeys, !projectCachesMap) <-
            if V.null uniqueProjectKeys
              then pure (V.empty, HashMap.empty)
              else do
                -- Get project IDs
                projectIdsAndKeys <-
                  checkpoint "processList:logs:getProjectIds"
                    $ ProjectApiKeys.projectIdsByProjectApiKeys uniqueProjectKeys

                -- Get unique project IDs
                let !projectIds = L.nub $ V.toList atIds <> [pid | (_, pid) <- V.toList projectIdsAndKeys]

                -- Batch fetch all project caches in parallel
                projectCaches <- checkpoint "processList:logs:getProjectCaches" $ do
                  caches <- liftIO $ do
                    -- Use parallel evaluation for cache fetching
                    cachePairs <- forM projectIds $ \pid ->
                      Cache.fetchWithCache appCtx.projectCache pid $ \pid' -> do
                        mpjCache <- withPool appCtx.jobsPool $ Projects.projectCacheById pid'
                        pure $! fromMaybe defaultProjectCache mpjCache
                    -- Force evaluation of cache pairs
                    pure (zip projectIds cachePairs `using` parList rpar)
                  pure $ HashMap.fromList caches

                pure (projectIdsAndKeys, projectCaches)

          -- Process messages in parallel chunks
          let !chunkSize = max 10 (msgsCount `div` 4) -- Process in chunks for better parallelism
              !chunks = chunksOf chunkSize (zip [0 ..] decodedMsgs)
              processChunk chunk =
                [ case decodeResult of
                    Left err -> (ackId, V.empty)
                    Right logReq ->
                      let !resourceLogs = V.force $ V.fromList $ logReq ^. PLF.resourceLogs
                          !projectKeys = getLogAttributeValue "at-project-key" resourceLogs
                          !relevantProjectIdsAndKeys = V.filter (\(k, _) -> k `V.elem` projectKeys) allProjectIdsAndKeys
                          !logs = V.force $ V.concatMap (V.fromList . convertResourceLogsToOtelLogs fallbackTime projectCachesMap relevantProjectIdsAndKeys) resourceLogs
                       in (ackId, logs)
                | (_, (ackId, decodeResult)) <- chunk
                ]

          !chunkedResults <- liftIO $ pure (map processChunk chunks `using` parList rpar)

          -- Log errors for failed decodings
          sequence_
            [ Log.logAttention
                "processList:logs: unable to parse logs service request"
                (createProtoErrorInfo err (snd $ msgs L.!! idx))
            | (idx, (_, Left err)) <- zip [0 ..] decodedMsgs
            ]

          let !results = V.fromList $ concat chunkedResults
              (!ackIds, !logsVectors) = V.unzip results
              !allLogs = V.force $ V.concat $ V.toList logsVectors

          processingEndTime <- liftIO getCurrentTime
          let processingDuration = diffUTCTime processingEndTime processingStartTime

          dbInsertDuration <-
            if V.null allLogs
              then pure 0
              else do
                dbInsertStartTime <- liftIO getCurrentTime
                checkpoint "processList:logs:bulkInsert"
                  $ Telemetry.bulkInsertOtelLogsAndSpansTF allLogs
                dbInsertEndTime <- liftIO getCurrentTime
                pure $ diffUTCTime dbInsertEndTime dbInsertStartTime

          pure (V.toList ackIds, round (processingDuration * 1000), round (dbInsertDuration * 1000))
        Just "org.opentelemetry.otlp.traces.v1" -> checkpoint "processList:traces" $ do
          processingStartTime <- liftIO getCurrentTime
          let !msgsCount = length msgs

          -- Decode messages in parallel
          let !decodedMsgs =
                [(ackId, decodeMessage msg :: Either String TS.ExportTraceServiceRequest) | (ackId, msg) <- msgs]
                  `using` parList rpar

          -- Extract all unique project keys efficiently
          let !allProjectKeys =
                V.force
                  $ V.concat
                    [ getSpanAttributeValue "at-project-key" (V.fromList $ traceReq ^. PTF.resourceSpans)
                    | (_, Right traceReq) <- decodedMsgs
                    ]
              !uniqueProjectKeys = V.force $ V.fromList $ L.nub $ V.toList allProjectKeys
              atIds' =
                V.concat
                  [ getSpanAttributeValue "at-project-id" (V.fromList $ traceReq ^. PTF.resourceSpans)
                  | (_, Right traceReq) <- decodedMsgs
                  ]
              atIds = V.catMaybes $ Projects.projectIdFromText <$> atIds'

          -- Batch fetch all project data in one go
          (!allProjectIdsAndKeys, !projectCachesMap) <-
            if V.null uniqueProjectKeys
              then pure (V.empty, HashMap.empty)
              else do
                -- Get project IDs
                projectIdsAndKeys <-
                  checkpoint "processList:traces:getProjectIds"
                    $ ProjectApiKeys.projectIdsByProjectApiKeys uniqueProjectKeys

                -- Get unique project IDs
                let !projectIds = L.nub $ V.toList atIds <> [pid | (_, pid) <- V.toList projectIdsAndKeys]

                -- Batch fetch all project caches in parallel
                projectCaches <- checkpoint "processList:traces:getProjectCaches" $ do
                  caches <- liftIO $ do
                    -- Use parallel evaluation for cache fetching
                    cachePairs <- forM projectIds $ \pid ->
                      Cache.fetchWithCache appCtx.projectCache pid $ \pid' -> do
                        mpjCache <- withPool appCtx.jobsPool $ Projects.projectCacheById pid'
                        pure $! fromMaybe defaultProjectCache mpjCache
                    -- Force evaluation of cache pairs
                    pure (zip projectIds cachePairs `using` parList rpar)
                  pure $ HashMap.fromList caches
                pure (projectIdsAndKeys, projectCaches)
          -- Process messages in parallel chunks
          let !chunkSize = max 10 (msgsCount `div` 4) -- Process in chunks for better parallelism
              !chunks = chunksOf chunkSize (zip [0 ..] decodedMsgs)
              processChunk chunk =
                [ case decodeResult of
                    Left err -> (ackId, V.empty)
                    Right traceReq ->
                      let !resourceSpans = V.force $ V.fromList $ traceReq ^. PTF.resourceSpans
                          !projectKeys = getSpanAttributeValue "at-project-key" resourceSpans
                          !relevantProjectIdsAndKeys = V.filter (\(k, _) -> k `V.elem` projectKeys) allProjectIdsAndKeys
                          !spans = V.force $ V.fromList $ convertResourceSpansToOtelLogs fallbackTime projectCachesMap relevantProjectIdsAndKeys resourceSpans
                       in (ackId, spans)
                | (_, (ackId, decodeResult)) <- chunk
                ]

          !chunkedResults <- liftIO $ pure (map processChunk chunks `using` parList rpar)

          -- Log errors for failed decodings
          sequence_
            [ Log.logAttention
                "processList:traces: unable to parse traces service request"
                (createProtoErrorInfo err (snd $ msgs L.!! idx))
            | (idx, (_, Left err)) <- zip [0 ..] decodedMsgs
            ]

          let !results = V.fromList $ concat chunkedResults
              (!ackIds, !spansVectors) = V.unzip results
              !spans' = V.force $ V.concat $ V.toList spansVectors

          processingEndTime <- liftIO getCurrentTime
          let processingDuration = diffUTCTime processingEndTime processingStartTime

          dbInsertDuration <-
            if V.null spans'
              then pure 0
              else do
                dbInsertStartTime <- liftIO getCurrentTime
                checkpoint "processList:traces:bulkInsertOtelLogsAndSpansTF" $ Telemetry.bulkInsertOtelLogsAndSpansTF spans'
                dbInsertEndTime <- liftIO getCurrentTime
                pure $ diffUTCTime dbInsertEndTime dbInsertStartTime

          pure (V.toList ackIds, round (processingDuration * 1000), round (dbInsertDuration * 1000))
        Just "org.opentelemetry.otlp.metrics.v1" -> checkpoint "processList:metrics" do
          processingStartTime <- liftIO getCurrentTime
          let !msgsCount = length msgs
          results <- V.generateM msgsCount $ \idx ->
            let (ackId, msg) = msgs L.!! idx
             in case (decodeMessage msg :: Either String MS.ExportMetricsServiceRequest) of
                  Left err -> do
                    Log.logAttention "processList:metrics: unable to parse metrics service request" (createProtoErrorInfo err msg)
                    pure (ackId, V.empty)
                  Right metricReq -> checkpoint "processList:metrics:getProjectId" do
                    let !resourceMetrics = V.fromList $ metricReq ^. PMF.resourceMetrics
                        !projectKey = getMetricAttributeValue "at-project-key" resourceMetrics
                        !projectIdText = getMetricAttributeValue "at-project-id" resourceMetrics

                    pidM <- join <$> forM projectKey ProjectApiKeys.getProjectIdByApiKey
                    let pid2M = Projects.projectIdFromText =<< projectIdText
                    case pidM <|> pid2M of
                      Just pid -> do
                        -- Fetch project cache using cache pattern
                        projectCache <- liftIO $ Cache.fetchWithCache appCtx.projectCache pid $ \pid' -> do
                          mpjCache <- withPool appCtx.jobsPool $ Projects.projectCacheById pid'
                          pure $ fromMaybe defaultProjectCache mpjCache
                        let !projectCaches = HashMap.singleton pid projectCache
                            !metrics = convertResourceMetricsToMetricRecords fallbackTime projectCaches pid resourceMetrics
                        pure (ackId, V.fromList metrics)
                      Nothing -> checkpoint "processList:metrics:missing-project-info" $ do
                        Log.logAttention_ "processList:metrics: project API Key and project ID not available in metrics"
                        pure (ackId, V.empty)

          let (!ackIds, !metricVectors) = V.unzip results
              !metricRecords = V.concat $ V.toList metricVectors

          processingEndTime <- liftIO getCurrentTime
          let processingDuration = diffUTCTime processingEndTime processingStartTime

          dbInsertDuration <-
            if V.null metricRecords
              then pure 0
              else do
                dbInsertStartTime <- liftIO getCurrentTime
                checkpoint "processList:metrics:bulkInsert"
                  $ Telemetry.bulkInsertMetrics metricRecords
                dbInsertEndTime <- liftIO getCurrentTime
                pure $ diffUTCTime dbInsertEndTime dbInsertStartTime

          pure (V.toList ackIds, round (processingDuration * 1000), round (dbInsertDuration * 1000))
        _ -> do
          Log.logAttention "processList: unsupported opentelemetry data type" (AE.object ["ce-type" AE..= HashMap.lookup "ce-type" attrs])
          pure ([], 0, 0)


-- Helper functions

-- | Migrate HTTP semantic convention fields to their new paths/formats
migrateHttpSemanticConventions :: [(Text, AE.Value)] -> [(Text, AE.Value)]
migrateHttpSemanticConventions !keyVals =
  let
    -- Build a HashMap for O(1) lookups
    fieldMappingsMap :: HashMap Text Text
    fieldMappingsMap =
      HashMap.fromList
        [ ("http.method", "http.request.method")
        , ("http.status_code", "http.response.status_code")
        , ("http.request_content_length", "http.request.body.size")
        , ("http.response_content_length", "http.response.body.size")
        , ("net.protocol.name", "network.protocol.name")
        , ("net.protocol.version", "network.protocol.version")
        , ("net.sock.peer.addr", "network.peer.address")
        , ("net.sock.peer.port", "network.peer.port")
        , ("http.url", "url.full")
        , ("http.resend_count", "http.request.resend_count")
        , ("net.peer.name", "server.address")
        , ("net.peer.port", "server.port")
        , ("http.scheme", "url.scheme")
        , ("http.client_ip", "client.address")
        ]

    -- Pre-build lookups for efficiency
    kvMap = HashMap.fromList keyVals
    hasUrlPath = HashMap.member "url.path" kvMap
    httpMethodM = HashMap.lookup "http.method" kvMap
    httpTargetM = HashMap.lookup "http.target" kvMap
    methodOriginalM = HashMap.lookup "http.request.method_original" kvMap

    -- Main migration
    mgVals = map (\(k, v) -> (HashMap.lookupDefault k k fieldMappingsMap, v)) keyVals

    -- Handle the special case of http.target -> url.path and url.query
    migrateHttpTarget = case httpTargetM of
      Just (AE.String target)
        | not hasUrlPath ->
            let
              (path, query) = T.breakOn "?" target
              queryWithoutQ = if T.null query then "" else T.drop 1 query
              pathPair = [("url.path", AE.String path)]
              queryPair = [("url.query", AE.String queryWithoutQ) | not (T.null query)]
             in
              pathPair ++ queryPair
      _ -> []

    -- Migrate method "_OTHER" case
    migrateMethodOther = case (httpMethodM, methodOriginalM) of
      (Just (AE.String "_OTHER"), Just originalMethod)
        | not (HashMap.member "http.request.method" kvMap) ->
            [("http.request.method", originalMethod)]
      _ -> []
   in
    mgVals ++ migrateHttpTarget ++ migrateMethodOther


-- Extract structured information from protobuf decoding errors
createProtoErrorInfo :: String -> ByteString -> AE.Value
createProtoErrorInfo err msg =
  case T.splitOn ":" (toText err) of
    (firstPart : details) ->
      -- Extract the field path that caused the error
      let fieldPath = T.takeWhile (/= ':') (T.strip (unwords details))
          errorType = T.takeWhileEnd (/= ':') (T.strip (unwords details))
       in AE.object
            [ "err_type" AE..= errorType
            , "field_path" AE..= fieldPath
            , "full_err" AE..= err
            , "msg_size" AE..= BS.length msg
            ]
    _ -> AE.object ["full_err" AE..= err, "msg_size" AE..= BS.length msg]


-- Convert nanoseconds to UTCTime
nanosecondsToUTC :: Word64 -> UTCTime
nanosecondsToUTC !ns = posixSecondsToUTCTime (fromIntegral ns / 1e9)


-- | Get a valid timestamp with fallback logic
-- Priority: timestamp -> observed_timestamp -> fallback time
getValidTimestamp :: UTCTime -> Word64 -> Word64 -> UTCTime
getValidTimestamp !fallbackTime !timeNano !observedTimeNano =
  let isValid ns = ns >= minValidTimestampNanos && ns /= 0
   in if isValid timeNano
        then nanosecondsToUTC timeNano
        else
          if isValid observedTimeNano
            then nanosecondsToUTC observedTimeNano
            else fallbackTime


-- Convert ByteString to hex Text
byteStringToHexText :: BS.ByteString -> Text
byteStringToHexText !bs = decodeUtf8 (B16.encode bs)


keyValueToJSON :: V.Vector PC.KeyValue -> AE.Value
keyValueToJSON !kvs =
  let
    -- Extract all key-value pairs
    !allPairs' = [(kv ^. PCF.key, anyValueToJSON (Just (kv ^. PCF.value))) | kv <- V.toList kvs]
    !allPairs = migrateHttpSemanticConventions allPairs'

    specialKeys = ["at-project-key", "at-project-id"]
    (flatPairs, nestedPairs) = L.partition (\(k, _) -> k `elem` specialKeys) allPairs
    nestedObj = nestedJsonFromDotNotation nestedPairs
    flatObj = AE.object [AEK.fromText k AE..= v | (k, v) <- flatPairs]

    mergedObj =
      AE.Object
        $ KEM.union
          (case flatObj of AE.Object km -> km; _ -> KEM.empty)
          (case nestedObj of AE.Object km -> km; _ -> KEM.empty)
   in
    mergedObj


anyValueToJSON :: Maybe PC.AnyValue -> AE.Value
anyValueToJSON Nothing = AE.Null
anyValueToJSON (Just av) = do
  case av ^. PCF.maybe'value of
    Nothing -> AE.Null
    Just (PC.AnyValue'StringValue txt) -> do
      let e = AE.eitherDecodeStrict' (encodeUtf8 (T.strip txt))
      case e of
        Right v -> AE.Object v
        Left _ -> AE.String txt
    Just (PC.AnyValue'BoolValue b) -> AE.Bool b
    Just (PC.AnyValue'IntValue i) -> AE.Number (fromInteger (toInteger i))
    Just (PC.AnyValue'DoubleValue d) -> AE.Number (fromFloatDigits d)
    Just (PC.AnyValue'ArrayValue arr) ->
      AE.Array . V.fromList $ map (anyValueToJSON . Just) (arr ^. PCF.values)
    Just (PC.AnyValue'KvlistValue kvl) ->
      let pairs =
            [ (AEK.fromText (pv ^. PCF.key), anyValueToJSON (Just (pv ^. PCF.value)))
            | pv <- kvl ^. PCF.values
            ]
       in AE.Object (KEM.fromList pairs)
    Just (PC.AnyValue'BytesValue bs) ->
      -- if the bytes is a json string, decode it
      -- otherwise just return AE.string like it it nowl
      AE.String $ B64.extractBase64 $ B64.encodeBase64 bs


-- Convert Resource to JSON
resourceToJSON :: Maybe PR.Resource -> AE.Value
resourceToJSON Nothing = AE.Null
resourceToJSON (Just resource) =
  let
    attrs = V.fromList $ resource ^. PRF.attributes
    attrPairs = [(kv ^. PCF.key, anyValueToJSON (Just (kv ^. PCF.value))) | kv <- V.toList attrs]
    specialKeys = ["at-project-key", "at-project-id"]
    (flatPairs, nestedPairs) = L.partition (\(k, _) -> k `elem` specialKeys) attrPairs

    nestedObj = nestedJsonFromDotNotation nestedPairs
    flatObj = AE.object [AEK.fromText k AE..= v | (k, v) <- flatPairs]
   in
    AE.Object
      $ KEM.union
        (case flatObj of AE.Object km -> km; _ -> KEM.empty)
        (case nestedObj of AE.Object km -> km; _ -> KEM.empty)


-- Convert JSON to Map
jsonToMap :: AE.Value -> Maybe (Map Text AE.Value)
jsonToMap (AE.Object o) = Just $ KEM.toMapText o
jsonToMap _ = Nothing


-- Remove project metadata from JSON
removeProjectId :: AE.Value -> AE.Value
removeProjectId (AE.Object o) = AE.Object $ foldr KEM.delete o ["at-project-key", "at-project-id"]
removeProjectId (AE.Array arr) = AE.Array $ V.map removeProjectId arr
removeProjectId v = v


-- Parse severity level
parseSeverityLevel :: Text -> Maybe Telemetry.SeverityLevel
parseSeverityLevel input = case T.toUpper input of
  "DEBUG" -> Just Telemetry.SLDebug
  "INFO" -> Just Telemetry.SLInfo
  "WARN" -> Just Telemetry.SLWarn
  "ERROR" -> Just Telemetry.SLError
  "FATAL" -> Just Telemetry.SLFatal
  _ -> Nothing


-- | Convert ResourceLogs to OtelLogsAndSpans
convertResourceLogsToOtelLogs :: UTCTime -> HashMap Projects.ProjectId Projects.ProjectCache -> V.Vector (Text, Projects.ProjectId) -> PL.ResourceLogs -> [OtelLogsAndSpans]
convertResourceLogsToOtelLogs !fallbackTime !projectCaches !pids resourceLogs =
  let projectKey = fromMaybe "" $ listToMaybe $ V.toList $ getLogAttributeValue "at-project-key" (V.singleton resourceLogs)
      projectId = case find (\(k, _) -> k == projectKey) pids of
        Just (_, v) -> Just v
        Nothing ->
          let pidText = fromMaybe "" $ listToMaybe $ V.toList $ getLogAttributeValue "at-project-id" (V.singleton resourceLogs)
              uId = UUID.fromText pidText
           in ((Just . Projects.ProjectId) =<< uId)
   in case projectId of
        Just pid ->
          case HashMap.lookup pid projectCaches of
            Just cache ->
              -- Check if project has exceeded daily limit for free tier
              let !totalDailyEvents = fromIntegral cache.dailyEventCount + fromIntegral cache.dailyMetricCount
                  !isFreeTier = cache.paymentPlan == "Free"
                  !hasExceededLimit = isFreeTier && totalDailyEvents >= freeTierDailyMaxEvents
               in if hasExceededLimit
                    then [] -- Discard events for projects that exceeded limits
                    else convertScopeLogsToOtelLogs fallbackTime pid (Just $ resourceLogs ^. PLF.resource) resourceLogs
            Nothing -> [] -- No cache found, discard
        _ -> []


filterEmptyEvents :: OtelLogsAndSpans -> Bool
filterEmptyEvents x =
  not (T.null (fromMaybe "" x.name) && isAesonTextEmpty x.body && isAesonTextEmpty x.attributes && isAesonTextEmpty x.resource && T.null (fromMaybe "" (x.parent_id)))


-- | Convert ScopeLogs to OtelLogsAndSpans
convertScopeLogsToOtelLogs :: UTCTime -> Projects.ProjectId -> Maybe PR.Resource -> PL.ResourceLogs -> [OtelLogsAndSpans]
convertScopeLogsToOtelLogs fallbackTime pid resourceM resourceLogs =
  filter filterEmptyEvents
    $ join
    $ V.toList
    $ V.map
      ( \scopeLog ->
          let scope = Just $ scopeLog ^. PLF.scope
              logRecords = V.fromList $ scopeLog ^. PLF.logRecords
           in V.toList $ V.map (convertLogRecordToOtelLog fallbackTime pid resourceM scope) logRecords
      )
      (V.fromList $ resourceLogs ^. PLF.scopeLogs)


-- | Convert LogRecord to OtelLogsAndSpans
convertLogRecordToOtelLog :: UTCTime -> Projects.ProjectId -> Maybe PR.Resource -> Maybe PC.InstrumentationScope -> PL.LogRecord -> OtelLogsAndSpans
convertLogRecordToOtelLog !fallbackTime !pid resourceM scopeM logRecord =
  let !timeNano = logRecord ^. PLF.timeUnixNano
      !observedTimeNano = logRecord ^. PLF.observedTimeUnixNano
      !severityText = logRecord ^. PLF.severityText
      !severityNumber = fromEnum (logRecord ^. PLF.severityNumber)
      !validTimestamp = getValidTimestamp fallbackTime timeNano observedTimeNano
      !validObservedTimestamp =
        if observedTimeNano >= minValidTimestampNanos && observedTimeNano /= 0
          then nanosecondsToUTC observedTimeNano
          else fallbackTime

      otelLog =
        OtelLogsAndSpans
          { project_id = pid.toText
          , id = UUID.toText UUID.nil -- Will be replaced in bulkInsertOtelLogsAndSpansTF
          , timestamp = validTimestamp
          , observed_timestamp = Just validObservedTimestamp
          , context =
              Just
                $ Context
                  { trace_id = Just $ byteStringToHexText $ logRecord ^. PLF.traceId
                  , span_id = Just $ byteStringToHexText $ logRecord ^. PLF.spanId
                  , trace_state = Nothing
                  , trace_flags = Nothing
                  , is_remote = Nothing
                  }
          , level = Just severityText
          , severity =
              Just
                $ Severity
                  { severity_text = parseSeverityLevel severityText
                  , severity_number = severityNumber
                  }
          , body = fmap AesonText $ Just $ anyValueToJSON $ Just $ logRecord ^. PLF.body
          , attributes = fmap AesonText $ jsonToMap $ removeProjectId $ keyValueToJSON $ V.fromList $ logRecord ^. PLF.attributes
          , resource = fmap AesonText $ jsonToMap $ removeProjectId $ resourceToJSON resourceM
          , hashes = V.empty
          , kind = Just "log"
          , status_code = Nothing
          , status_message = Nothing
          , duration = Nothing
          , start_time = validObservedTimestamp
          , end_time = Nothing
          , events = Nothing
          , links = Nothing
          , name = Nothing
          , parent_id = Nothing
          , summary = V.empty -- Will be populated after creation
          , date = validTimestamp
          , errors = Nothing
          }
   in otelLog{summary = generateSummary otelLog}


-- | Convert ResourceSpans to OtelLogsAndSpans
convertResourceSpansToOtelLogs :: UTCTime -> HashMap Projects.ProjectId Projects.ProjectCache -> V.Vector (Text, Projects.ProjectId) -> V.Vector PT.ResourceSpans -> [OtelLogsAndSpans]
convertResourceSpansToOtelLogs !fallbackTime !projectCaches !pids !resourceSpans =
  join
    $ V.toList
    $ V.map
      ( \rs ->
          let projectKey = fromMaybe "" $ listToMaybe $ V.toList $ getSpanAttributeValue "at-project-key" (V.singleton rs)
              projectId = case find (\(k, _) -> k == projectKey) pids of
                Just (_, v) -> Just v
                Nothing ->
                  let pidText = fromMaybe "" $ listToMaybe $ V.toList $ getSpanAttributeValue "at-project-id" (V.singleton rs)
                      uId = UUID.fromText pidText
                   in ((Just . Projects.ProjectId) =<< uId)
           in case projectId of
                Just pid ->
                  case HashMap.lookup pid projectCaches of
                    Just cache ->
                      -- Check if project has exceeded daily limit for free tier
                      let !totalDailyEvents = toInteger cache.dailyEventCount + toInteger cache.dailyMetricCount
                          !isFreeTier = cache.paymentPlan == "Free"
                          !hasExceededLimit = isFreeTier && totalDailyEvents >= freeTierDailyMaxEvents
                       in if hasExceededLimit
                            then [] -- Discard events for projects that exceeded limits
                            else convertScopeSpansToOtelLogs fallbackTime pid (Just $ rs ^. PTF.resource) rs
                    Nothing -> [] -- No cache found, discard
                _ -> []
      )
      resourceSpans


-- | Convert ScopeSpans to OtelLogsAndSpansF
convertScopeSpansToOtelLogs :: UTCTime -> Projects.ProjectId -> Maybe PR.Resource -> PT.ResourceSpans -> [OtelLogsAndSpans]
convertScopeSpansToOtelLogs fallbackTime pid resourceM resourceSpans =
  filter filterEmptyEvents
    $ join
    $ V.toList
    $ V.map
      ( \scopeSpan ->
          let scope = Just $ scopeSpan ^. PTF.scope
              spans = V.fromList $ scopeSpan ^. PTF.spans
           in V.toList $ V.map (convertSpanToOtelLog fallbackTime pid resourceM scope) spans
      )
      (V.fromList $ resourceSpans ^. PTF.scopeSpans)


-- | Convert Span to OtelLogsAndSpans
convertSpanToOtelLog :: UTCTime -> Projects.ProjectId -> Maybe PR.Resource -> Maybe PC.InstrumentationScope -> PT.Span -> OtelLogsAndSpans
convertSpanToOtelLog !fallbackTime !pid resourceM scopeM pSpan =
  let !startTimeNano = pSpan ^. PTF.startTimeUnixNano
      !endTimeNano = pSpan ^. PTF.endTimeUnixNano
      !validStartTime =
        if startTimeNano >= minValidTimestampNanos && startTimeNano /= 0
          then nanosecondsToUTC startTimeNano
          else fallbackTime
      !validEndTime =
        if endTimeNano >= minValidTimestampNanos && endTimeNano /= 0
          then nanosecondsToUTC endTimeNano
          else validStartTime -- If end time is invalid, use start time
      !durationNanos =
        if startTimeNano > 0 && endTimeNano > startTimeNano
          then endTimeNano - startTimeNano
          else 0
      spanKind = pSpan ^. PTF.kind
      spanKindText = case spanKind of
        PT.Span'SPAN_KIND_INTERNAL -> Just "internal"
        PT.Span'SPAN_KIND_SERVER -> Just "server"
        PT.Span'SPAN_KIND_CLIENT -> Just "client"
        PT.Span'SPAN_KIND_PRODUCER -> Just "producer"
        PT.Span'SPAN_KIND_CONSUMER -> Just "consumer"
        _ -> Just "unspecified"
      statusM = Just $ pSpan ^. PTF.status
      statusCodeText = case statusM of
        Just status -> case status ^. PTF.code of
          PT.Status'STATUS_CODE_OK -> Just "OK"
          PT.Status'STATUS_CODE_ERROR -> Just "ERROR"
          _ -> Just "UNSET"
        Nothing -> Nothing
      statusMsgText = case statusM of
        Just status -> Just $ status ^. PTF.message
        Nothing -> Nothing
      parentSpanId = pSpan ^. PTF.parentSpanId
      parentId =
        if BS.null parentSpanId
          then Nothing
          else Just $ byteStringToHexText parentSpanId

      -- Convert events only if non-empty
      eventsJson =
        let !events = pSpan ^. PTF.events
         in if null events
              then Nothing
              else
                Just
                  $! AE.toJSON
                  $! map
                    ( \ev ->
                        AE.object
                          [ "event_name" AE..= (ev ^. PTF.name)
                          , "event_time"
                              AE..= let evTimeNano = ev ^. PTF.timeUnixNano
                                     in if evTimeNano >= minValidTimestampNanos && evTimeNano /= 0
                                          then nanosecondsToUTC evTimeNano
                                          else fallbackTime
                          , "event_attributes" AE..= keyValueToJSON (V.fromList $ ev ^. PTF.attributes)
                          , "event_dropped_attributes_count" AE..= (ev ^. PTF.droppedAttributesCount)
                          ]
                    )
                    events

      -- Convert links only if non-empty
      linksJson =
        let !links = pSpan ^. PTF.links
         in if null links
              then Nothing
              else
                Just
                  $! show
                  $! AE.toJSON
                  $! map
                    ( \link ->
                        AE.object
                          [ "link_span_id" AE..= byteStringToHexText (link ^. PTF.spanId)
                          , "link_trace_id" AE..= byteStringToHexText (link ^. PTF.traceId)
                          , "link_attributes" AE..= keyValueToJSON (V.fromList $ link ^. PTF.attributes)
                          , "link_dropped_attributes_count" AE..= (link ^. PTF.droppedAttributesCount)
                          , "link_flags" AE..= (link ^. PTF.traceState)
                          ]
                    )
                    links
      !attributes = jsonToMap $ removeProjectId $ keyValueToJSON $ V.fromList $ pSpan ^. PTF.attributes
      (req, res) = case Map.lookup "http" (fromMaybe Map.empty attributes) of
        Just (AE.Object http) -> (KEM.lookup "request" http, KEM.lookup "response" http)
        _ -> (Nothing, Nothing)
      body =
        if pSpan ^. PTF.name == "apitoolkit-http-span" || pSpan ^. PTF.name == "monoscope.http"
          then
            Just
              $ AE.object
                [ "request_body" AE..= extractBody req
                , "response_body" AE..= extractBody res
                ]
          else Nothing
        where
          extractBody :: Maybe AE.Value -> AE.Value
          extractBody (Just (AE.Object obj)) =
            case KEM.lookup "body" obj of
              Just (AE.String b) -> b64ToJson b
              _ -> AE.Null
          extractBody _ = AE.Null
      newAttributes =
        if pSpan ^. PTF.name == "apitoolkit-http-span" || pSpan ^. PTF.name == "monoscope.http"
          then
            let htt = Map.lookup "http" (fromMaybe Map.empty attributes)
             in case htt of
                  Just (AE.Object http) ->
                    let newHttp = case (req, res) of
                          (Just (AE.Object re), Just (AE.Object rs)) ->
                            let newReq = KEM.insert "request" (AE.Object $ KEM.delete "body" re) http
                                newRes = KEM.insert "response" (AE.Object $ KEM.delete "body" rs) newReq
                             in Just $ Map.insert "http" (AE.Object newRes) (fromMaybe Map.empty attributes)
                          _ -> attributes
                     in newHttp
                  _ -> attributes
          else attributes
      otelSpan =
        OtelLogsAndSpans
          { project_id = pid.toText
          , id = UUID.toText UUID.nil -- Will be replaced in bulkInsertOtelLogsAndSpansTF
          , timestamp = validStartTime
          , observed_timestamp = Just validStartTime
          , context =
              Just
                $ Context
                  { trace_id = Just $ byteStringToHexText $ pSpan ^. PTF.traceId
                  , span_id = Just $ byteStringToHexText $ pSpan ^. PTF.spanId
                  , trace_state = Just $ pSpan ^. PTF.traceState
                  , trace_flags = Nothing
                  , is_remote = Nothing
                  }
          , level = Nothing
          , severity = Nothing
          , body = fmap AesonText body
          , attributes = fmap AesonText newAttributes
          , resource = fmap AesonText $ jsonToMap $ removeProjectId $ resourceToJSON resourceM
          , hashes = V.empty
          , kind = spanKindText
          , status_code = statusCodeText
          , status_message = statusMsgText
          , duration = Just $ fromIntegral durationNanos
          , start_time = validStartTime
          , end_time = Just validEndTime
          , events = fmap AesonText eventsJson
          , links = linksJson
          , name = Just $ pSpan ^. PTF.name
          , parent_id = parentId
          , summary = V.empty -- Will be populated after creation
          , date = validStartTime
          , errors = Nothing
          }
   in otelSpan{summary = generateSummary otelSpan}


-- | Convert ResourceMetrics to MetricRecords
convertResourceMetricsToMetricRecords :: UTCTime -> HashMap Projects.ProjectId Projects.ProjectCache -> Projects.ProjectId -> V.Vector PM.ResourceMetrics -> [Telemetry.MetricRecord]
convertResourceMetricsToMetricRecords !fallbackTime !projectCaches !pid !resourceMetrics =
  case HashMap.lookup pid projectCaches of
    Just cache ->
      -- Check if project has exceeded daily limit for free tier
      let !totalDailyEvents = fromIntegral cache.dailyEventCount + fromIntegral cache.dailyMetricCount
          !isFreeTier = cache.paymentPlan == "Free"
          !hasExceededLimit = isFreeTier && totalDailyEvents >= freeTierDailyMaxEvents
       in if hasExceededLimit
            then [] -- Discard metrics for projects that exceeded limits
            else join $ V.toList $ V.map (convertResourceMetricToMetricRecords fallbackTime pid) resourceMetrics
    Nothing -> [] -- No cache found, discard


-- | Convert a single ResourceMetrics to MetricRecords
convertResourceMetricToMetricRecords :: UTCTime -> Projects.ProjectId -> PM.ResourceMetrics -> [Telemetry.MetricRecord]
convertResourceMetricToMetricRecords fallbackTime pid resourceMetric =
  let resourceM = Just $ resourceMetric ^. PMF.resource
      scopeMetrics = V.fromList $ resourceMetric ^. PMF.scopeMetrics
   in join
        $ V.toList
        $ V.map
          ( \sm ->
              let scope = Just $ sm ^. PMF.scope
                  metrics = V.fromList $ sm ^. PMF.metrics
               in join $ V.toList $ V.map (convertMetricToMetricRecords fallbackTime pid resourceM scope) metrics
          )
          scopeMetrics


-- | Convert a single Metric to MetricRecords
convertMetricToMetricRecords :: UTCTime -> Projects.ProjectId -> Maybe PR.Resource -> Maybe PC.InstrumentationScope -> PM.Metric -> [Telemetry.MetricRecord]
convertMetricToMetricRecords fallbackTime pid resourceM scopeM metric =
  -- For brevity, implementing only gauge metrics as an example
  -- In a complete implementation, you would handle all metric types
  case metric ^. PMF.maybe'data' of
    Just metricData ->
      case metricData of
        PM.Metric'Gauge gauge ->
          let gaugePoints = V.fromList $ gauge ^. PMF.dataPoints
           in V.toList
                $ V.map
                  ( \point ->
                      let !timeNano = point ^. PMF.timeUnixNano
                          !validTime =
                            if timeNano >= minValidTimestampNanos && timeNano /= 0
                              then nanosecondsToUTC timeNano
                              else fallbackTime
                          !value = case point ^. PMF.maybe'value of
                            Just (PM.NumberDataPoint'AsDouble d) -> d
                            Just (PM.NumberDataPoint'AsInt i) -> fromIntegral i
                            _ -> 0
                          !attributes = keyValueToJSON $ V.fromList $ point ^. PMF.attributes
                       in Telemetry.MetricRecord
                            { projectId = Projects.unProjectId pid
                            , id = Nothing
                            , metricName = metric ^. PMF.name
                            , metricDescription = metric ^. PMF.description
                            , metricUnit = metric ^. PMF.unit
                            , timestamp = validTime
                            , metricTime = validTime
                            , resource = removeProjectId $ resourceToJSON resourceM
                            , instrumentationScope = case scopeM of
                                Just scope ->
                                  AE.object
                                    [ "name" AE..= (scope ^. PCF.name)
                                    , "version" AE..= (scope ^. PCF.version)
                                    , "attributes" AE..= keyValueToJSON (V.fromList $ scope ^. PCF.attributes)
                                    ]
                                Nothing -> AE.Null
                            , metricValue = Telemetry.GaugeValue $ Telemetry.GaugeSum{value = value}
                            , exemplars = AE.object [] -- Empty object since we can't properly convert the exemplars;  AE.object ["exemplars" AE..= (point ^. PMF.exemplars)]
                            , metricType = Telemetry.MTGauge
                            , flags = fromIntegral $ point ^. PMF.flags
                            , attributes = attributes
                            , metricMetadata = case scopeM of
                                Just scope -> AE.object ["scope" AE..= (scope ^. PCF.name)]
                                Nothing -> AE.Null
                            }
                  )
                  gaugePoints
        -- Add other metric types (Sum, Histogram, etc.) following a similar pattern
        _ -> [] -- Not implemented for brevity
    Nothing -> []


---------------------------------------------------------------------------------------
-- Server
---------------------------------------------------------------------------------------

runServer :: Log.Logger -> AuthContext -> TracerProvider -> IO ()
runServer appLogger appCtx tp = runServerWithHandlers def config (services appLogger appCtx tp)
  where
    serverHost = "0.0.0.0"
    serverPort = appCtx.config.grpcPort
    config :: ServerConfig
    config =
      ServerConfig
        { serverInsecure = Just (InsecureConfig (Just $ toString serverHost) (fromIntegral serverPort))
        , serverSecure = Nothing
        }


-- | Trace service handler (Export)
traceServiceExport :: Log.Logger -> AuthContext -> TracerProvider -> Proto TS.ExportTraceServiceRequest -> IO (Proto TS.ExportTraceServiceResponse)
traceServiceExport appLogger appCtx tp (Proto req) = do
  _ <- runBackground appLogger appCtx tp do
    Log.logInfo "Received trace export request" AE.Null

    currentTime <- liftIO getCurrentTime

    let !resourceSpans = V.fromList $ req ^. TSF.resourceSpans
        !projectKeys = getSpanAttributeValue "at-project-key" resourceSpans
        atIds' = getSpanAttributeValue "at-project-id" resourceSpans
        atIds = V.catMaybes $ Projects.projectIdFromText <$> atIds'

    projectIdsAndKeys <-
      checkpoint "processList:traces:getProjectIds"
        $ ProjectApiKeys.projectIdsByProjectApiKeys projectKeys
    -- Fetch project caches for limit checking
    projectCaches <- checkpoint "processList:traces:getProjectCaches" $ do
      let projectIds = L.nub $ V.toList atIds <> [pid | (_, pid) <- V.toList projectIdsAndKeys]

      caches <- forM projectIds $ \pid -> do
        cache <- liftIO $ Cache.fetchWithCache appCtx.projectCache pid $ \pid' -> do
          mpjCache <- withPool appCtx.jobsPool $ Projects.projectCacheById pid'
          pure $ fromMaybe defaultProjectCache mpjCache
        pure (pid, cache)
      pure $ HashMap.fromList caches
    let !spans = convertResourceSpansToOtelLogs currentTime projectCaches projectIdsAndKeys resourceSpans
        !spans' = V.fromList spans

    unless (V.null spans') $ Telemetry.bulkInsertOtelLogsAndSpansTF spans'

  -- Return an empty response
  pure defMessage


-- | Logs service handler (Export)
logsServiceExport :: Log.Logger -> AuthContext -> TracerProvider -> Proto LS.ExportLogsServiceRequest -> IO (Proto LS.ExportLogsServiceResponse)
logsServiceExport appLogger appCtx tp (Proto req) = do
  _ <- runBackground appLogger appCtx tp $ do
    Log.logInfo "Received logs export request" AE.Null
    currentTime <- liftIO getCurrentTime

    let !resourceLogs = V.fromList $ req ^. PLF.resourceLogs
        !projectKeys = getLogAttributeValue "at-project-key" resourceLogs
        atIds' = getLogAttributeValue "at-project-id" resourceLogs
        atIds = V.catMaybes $ Projects.projectIdFromText <$> atIds'

    projectIdsAndKeys <- ProjectApiKeys.projectIdsByProjectApiKeys projectKeys

    -- Fetch project caches using cache pattern
    projectCaches <- checkpoint "processList:logs:getProjectCaches" $ do
      let projectIds = L.nub $ V.toList atIds <> [pid | (_, pid) <- V.toList projectIdsAndKeys]
      caches <- forM projectIds $ \pid -> do
        cache <- liftIO $ Cache.fetchWithCache appCtx.projectCache pid $ \pid' -> do
          mpjCache <- withPool appCtx.jobsPool $ Projects.projectCacheById pid'
          pure $ fromMaybe defaultProjectCache mpjCache
        pure (pid, cache)
      pure $ HashMap.fromList caches

    let !logs = join $ V.toList $ V.map (convertResourceLogsToOtelLogs currentTime projectCaches projectIdsAndKeys) resourceLogs

    unless (null logs) do
      Telemetry.bulkInsertOtelLogsAndSpansTF (V.fromList logs)

  -- Return an empty response
  pure defMessage


-- | Metrics service handler (Export)
metricsServiceExport :: Log.Logger -> AuthContext -> TracerProvider -> Proto MS.ExportMetricsServiceRequest -> IO (Proto MS.ExportMetricsServiceResponse)
metricsServiceExport appLogger appCtx tp (Proto req) = do
  _ <- runBackground appLogger appCtx tp $ do
    Log.logInfo "Received metrics export request" AE.Null

    currentTime <- liftIO getCurrentTime

    let !resourceMetrics = V.fromList $ req ^. PMF.resourceMetrics
        !projectKey = getMetricAttributeValue "at-project-key" resourceMetrics

    pidM <- do
      p1 <- join <$> forM projectKey ProjectApiKeys.getProjectIdByApiKey
      let p2 = Projects.projectIdFromText =<< getMetricAttributeValue "at-project-id" resourceMetrics
      pure (p1 <|> p2)

    case pidM of
      Just pid -> do
        -- Fetch project cache using cache pattern
        projectCache <- liftIO $ Cache.fetchWithCache appCtx.projectCache pid $ \pid' -> do
          mpjCache <- withPool appCtx.jobsPool $ Projects.projectCacheById pid'
          pure $ fromMaybe defaultProjectCache mpjCache
        let !projectCaches = HashMap.singleton pid projectCache
            !metricRecords = convertResourceMetricsToMetricRecords currentTime projectCaches pid resourceMetrics

        unless (null metricRecords)
          $ Telemetry.bulkInsertMetrics (V.fromList metricRecords)
      Nothing -> Log.logAttention_ "Project API Key and project ID not available in metrics"

  -- Return an empty response
  pure defMessage


isAesonTextEmpty :: AE.ToJSON a => Maybe (AesonText a) -> Bool
isAesonTextEmpty Nothing = True
isAesonTextEmpty (Just (AesonText v)) =
  case AE.toJSON v of
    AE.Object o -> KEM.null o
    AE.Array arr -> V.null arr
    AE.String t -> T.null t
    AE.Null -> True
    _ -> False


services :: Log.Logger -> AuthContext -> TracerProvider -> [SomeRpcHandler IO]
services appLogger appCtx tp =
  fromMethods
    ( simpleMethods
        (mkNonStreaming $ traceServiceExport appLogger appCtx tp :: ServerHandler IO (Protobuf TS.TraceService "export"))
    )
    <> fromMethods
      ( simpleMethods
          (mkNonStreaming $ logsServiceExport appLogger appCtx tp :: ServerHandler IO (Protobuf LS.LogsService "export"))
      )
    <> fromMethods
      ( simpleMethods
          (mkNonStreaming $ metricsServiceExport appLogger appCtx tp :: ServerHandler IO (Protobuf MS.MetricsService "export"))
      )


type instance RequestMetadata (Protobuf TS.TraceService "export") = NoMetadata
type instance ResponseInitialMetadata (Protobuf TS.TraceService "export") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf TS.TraceService "export") = NoMetadata


type instance RequestMetadata (Protobuf LS.LogsService "export") = NoMetadata
type instance ResponseInitialMetadata (Protobuf LS.LogsService "export") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf LS.LogsService "export") = NoMetadata


type instance RequestMetadata (Protobuf MS.MetricsService "export") = NoMetadata
type instance ResponseInitialMetadata (Protobuf MS.MetricsService "export") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf MS.MetricsService "export") = NoMetadata
