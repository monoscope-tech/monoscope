module Opentelemetry.OtlpServer (processList, runServer) where

import Control.Exception.Annotated (checkpoint)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as KEM
import Data.Base64.Types qualified as B64
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Effectful.UUID (UUIDEff)
import Data.HashMap.Strict qualified as HashMap
import Data.List (partition)
import Data.Map qualified as Map
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Encoding (decodeMessage, encodeMessage)
import Data.Scientific (fromFloatDigits)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful
import Effectful.Exception (onException)
import Effectful.Ki qualified as Ki
import Effectful.Labeled (Labeled)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Reader.Static (ask)
import Effectful.Reader.Static qualified as Eff
import Effectful.Time (Time)
import Log qualified
import Models.Apis.Anomalies qualified as Anomalies
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (Context (..), OtelLogsAndSpans (..), Severity (..), SpanKind (..), SpanStatus (..), convertSpanToRequestMessage)
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server (SomeRpcHandler)
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run hiding (runServer)
import Network.GRPC.Server.StreamType
import ProcessMessage qualified
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
import RequestMessages (RequestMessage (..))
import System.Config (AuthContext)
import System.Types (runBackground)
import Utils (b64ToJson, nestedJsonFromDotNotation)


-- import Network.GRPC.Server.Service (Service, service, method, fromServices)

-- | Generic lens-based attribute extraction from spans
getSpanAttributeValue :: Text -> V.Vector PT.ResourceSpans -> V.Vector Text
getSpanAttributeValue attribute rss = V.mapMaybe (\rs -> getResourceAttr rs <|> getSpanAttr rs) rss
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
getLogAttributeValue attribute rls = V.mapMaybe (\rl -> getResourceAttr rl <|> getLogAttr rl) rls
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
getMetricAttributeValue attribute rms = listToMaybe $ V.toList $ V.mapMaybe getResourceAttr rms
  where
    getResourceAttr rm = getAttr (resourceAttributes (rm ^. PMF.resource))

    getAttr :: V.Vector PC.KeyValue -> Maybe Text
    getAttr kvs = V.find ((== attribute) . (^. PCF.key)) kvs >>= \kv -> Just (kv ^. PCF.value . PCF.stringValue)

    resourceAttributes resource = V.fromList $ resource ^. PRF.attributes


-- | Process a list of messages
processList :: (Eff.Reader AuthContext :> es, DB :> es, Labeled "timefusion" DB :> es, Ki.StructuredConcurrency :> es, Log :> es, IOE :> es, Time :> es, UUIDEff :> es) => [(Text, ByteString)] -> HashMap Text Text -> Eff es [Text]
processList [] _ = pure []
processList msgs attrs = checkpoint "processList" $ process `onException` handleException
  where
    handleException = do
      Log.logAttention "processList: caught exception" (AE.object ["ce-type" AE..= (HashMap.lookup "ce-type" attrs)])
      pure $ map fst msgs

    process = do
      let msgs' = V.fromList msgs
      appCtx <- ask @AuthContext
      case HashMap.lookup "ce-type" attrs of
        Just "org.opentelemetry.otlp.logs.v1" -> checkpoint "processList:logs" $ do
          results <- V.forM msgs' $ \(ackId, msg) ->
            case (decodeMessage msg :: Either String LS.ExportLogsServiceRequest) of
              Left err -> do
                Log.logAttention "processList:logs: unable to parse logs service request" (createProtoErrorInfo err msg)
                pure (ackId, [])
              Right logReq -> do
                let resourceLogs = V.fromList $ logReq ^. PLF.resourceLogs
                    projectKeys = getLogAttributeValue "at-project-key" resourceLogs
                projectIdsAndKeys <-
                  checkpoint "processList:logs:getProjectIds" $
                    dbtToEff $
                      ProjectApiKeys.projectIdsByProjectApiKeys projectKeys
                pure (ackId, join $ V.toList $ V.map (convertResourceLogsToOtelLogs projectIdsAndKeys) resourceLogs)

          let (ackIds, logs) = V.unzip results
              allLogs = concat logs -- Flattens the list of lists
          unless (null allLogs) $
            checkpoint "processList:logs:bulkInsert" $
              Telemetry.bulkInsertOtelLogsAndSpansTF (V.fromList allLogs)
          pure $ V.toList ackIds
        Just "org.opentelemetry.otlp.traces.v1" -> checkpoint "processList:traces" $ do
          results <- V.forM msgs' $ \(ackId, msg) ->
            case (decodeMessage msg :: Either String TS.ExportTraceServiceRequest) of
              Left err -> do
                Log.logAttention "processList:traces: unable to parse traces service request" (createProtoErrorInfo err msg)
                pure (ackId, [])
              Right traceReq -> do
                let resourceSpans = V.fromList $ traceReq ^. PTF.resourceSpans
                    projectKeys = getSpanAttributeValue "at-project-key" resourceSpans
                projectIdsAndKeys <-
                  checkpoint "processList:traces:getProjectIds" $
                    dbtToEff $
                      ProjectApiKeys.projectIdsByProjectApiKeys projectKeys
                let spans = convertResourceSpansToOtelLogs projectIdsAndKeys resourceSpans
                pure (ackId, spans)

          let (ackIds, spans) = V.unzip results
              allSpans = concat spans -- Flattens the list of lists
              spans' = V.fromList allSpans
              apitoolkitSpans =
                V.mapMaybe
                  ( \s ->
                      if s.name == Just "apitoolkit-http-span"
                        then convertSpanToRequestMessage s "apitoolkit-http-span"
                        else Nothing
                  )
                  spans'

          unless (V.null apitoolkitSpans) $
            checkpoint "processList:traces:processRequestMessages" $
              (void $ ProcessMessage.processRequestMessages $ V.toList apitoolkitSpans <&> ("",))

          unless (null allSpans) do
            checkpoint "processList:traces:bulkInsertSpans" $ Telemetry.bulkInsertOtelLogsAndSpansTF spans'
            checkpoint "processList:traces:bulkInsertErrors" $ Anomalies.bulkInsertErrors $ Telemetry.getAllATErrors spans'

          pure $ V.toList ackIds
        Just "org.opentelemetry.otlp.metrics.v1" -> checkpoint "processList:metrics" do
          results <- V.forM msgs' $ \(ackId, msg) ->
            -- Handle each message individually
            case (decodeMessage msg :: Either String MS.ExportMetricsServiceRequest) of
              Left err -> do
                Log.logAttention "processList:metrics: unable to parse metrics service request" (createProtoErrorInfo err msg)
                pure (ackId, [])
              Right metricReq -> checkpoint "processList:metrics:getProjectId" do
                let resourceMetrics = V.fromList $ metricReq ^. PMF.resourceMetrics
                    projectKey = getMetricAttributeValue "at-project-key" resourceMetrics
                pidM <- join <$> (forM projectKey ProjectApiKeys.getProjectIdByApiKey)
                let pid2M = Projects.projectIdFromText =<< getMetricAttributeValue "at-project-id" resourceMetrics
                case (pidM <|> pid2M) of
                  Just pid -> pure (ackId, convertResourceMetricsToMetricRecords pid resourceMetrics)
                  Nothing -> do
                    Log.logAttention_ "processList:metrics: project API Key and project ID not available in metrics"
                    pure (ackId, [])

          let (ackIds, metricLists) = V.unzip results
              metricRecords = concat metricLists

          unless (null metricRecords) $
            checkpoint "processList:metrics:bulkInsert" $
              Telemetry.bulkInsertMetrics (V.fromList metricRecords)

          pure $ V.toList ackIds
        _ -> do
          Log.logAttention "processList: unsupported opentelemetry data type" (AE.object ["ce-type" AE..= (HashMap.lookup "ce-type" attrs)])
          pure []


-- Helper functions

-- | Migrate HTTP semantic convention fields to their new paths/formats
migrateHttpSemanticConventions :: Maybe (Map Text AE.Value) -> Maybe (Map Text AE.Value)
migrateHttpSemanticConventions Nothing = Nothing
migrateHttpSemanticConventions (Just attributes) =
  let
    -- Field mapping from old -> new
    fieldMappings = [
      ("http.method", "http.request.method"),
      ("http.status_code", "http.response.status_code"),
      ("http.request_content_length", "http.request.body.size"),
      ("http.response_content_length", "http.response.body.size"),
      ("net.protocol.name", "network.protocol.name"),
      ("net.protocol.version", "network.protocol.version"),
      ("net.sock.peer.addr", "network.peer.address"),
      ("net.sock.peer.port", "network.peer.port"),
      ("http.url", "url.full"),
      ("http.resend_count", "http.request.resend_count"),
      ("net.peer.name", "server.address"),
      ("net.peer.port", "server.port"),
      ("http.scheme", "url.scheme"),
      ("http.client_ip", "client.address"),
      ("net.host.name", "server.address"),
      ("net.host.port", "server.port")
    ]
    
    -- Handle the special case of http.target -> url.path and url.query
    migrateHttpTarget attributes' = 
      case Map.lookup "http.target" attributes' of
        Just (AE.String target) ->
          let
            -- Simple split at first '?' to separate path and query
            (path, query) = T.breakOn "?" target
            queryWithoutQ = if T.null query then "" else T.drop 1 query
            
            -- Add path and query fields if they don't exist yet
            withPath = if Map.member "url.path" attributes'
              then attributes' 
              else Map.insert "url.path" (AE.String path) attributes'
            
            withPathAndQuery = if T.null query || Map.member "url.query" withPath
              then withPath
              else Map.insert "url.query" (AE.String queryWithoutQ) withPath
          in
            withPathAndQuery
        _ -> attributes'
    
    -- Migrate method "_OTHER" case
    migrateMethodOther attributes' =
      case (Map.lookup "http.method" attributes', Map.lookup "http.request.method_original" attributes') of
        (Just (AE.String method), Just originalMethod) ->
          if method == "_OTHER" && not (Map.member "http.request.method" attributes')
            then Map.insert "http.request.method" originalMethod attributes'
            else attributes'
        _ -> attributes'
    
    -- Migrate fields based on mapping
    migrateFields attributes' =
      foldl' 
        (\acc (oldKey, newKey) -> 
          case Map.lookup oldKey acc of
            Just value ->
              if Map.member newKey acc
                then acc  -- Don't override existing values
                else Map.insert newKey value acc
            Nothing -> acc
        ) 
        attributes' 
        fieldMappings
  in
    Just $ migrateMethodOther $ migrateHttpTarget $ migrateFields attributes

-- Decode ByteString to Text with lenient UTF-8 handling
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TE.decodeUtf8With lenientDecode


-- Extract structured information from protobuf decoding errors
createProtoErrorInfo :: String -> ByteString -> AE.Value
createProtoErrorInfo err msg =
  case T.splitOn ":" (T.pack err) of
    (firstPart : details) ->
      -- Extract the field path that caused the error
      let fieldPath = T.takeWhile (/= ':') (T.strip (T.unwords details))
          errorType = T.takeWhileEnd (/= ':') (T.strip (T.unwords details))
       in AE.object
            [ "err_type" AE..= errorType
            , "field_path" AE..= fieldPath
            , "full_err" AE..= err
            , "msg_size" AE..= BS.length msg
            ]
    _ -> AE.object ["full_err" AE..= err, "msg_size" AE..= BS.length msg]


-- Convert nanoseconds to UTCTime
nanosecondsToUTC :: Word64 -> UTCTime
nanosecondsToUTC ns = posixSecondsToUTCTime (fromIntegral ns / 1e9)


-- Convert ByteString to hex Text
byteStringToHexText :: BS.ByteString -> Text
byteStringToHexText bs = TE.decodeUtf8 (B16.encode bs)


keyValueToJSON :: V.Vector PC.KeyValue -> AE.Value
keyValueToJSON kvs =
  let
    -- Extract all key-value pairs
    allPairs = [(kv ^. PCF.key, anyValueToJSON (Just (kv ^. PCF.value))) | kv <- V.toList kvs]

    -- Special case keys that should remain flat for OTel compatibility
    specialKeys = ["at-project-key", "at-project-id"]

    -- Split into flat and nested categories
    (flatPairs, nestedPairs) = partition (\(k, _) -> k `elem` specialKeys) allPairs

    -- Create the nested structure for regular keys
    nestedObj = nestedJsonFromDotNotation nestedPairs

    -- Create a separate object for special keys that should remain flat
    flatObj = AE.object [AEK.fromText k AE..= v | (k, v) <- flatPairs]

    -- Merge both objects (keeping special keys at the top level)
    mergedObj =
      AE.Object $
        KEM.union
          (case flatObj of AE.Object km -> km; _ -> KEM.empty)
          (case nestedObj of AE.Object km -> km; _ -> KEM.empty)
   in
    mergedObj


anyValueToJSON :: Maybe PC.AnyValue -> AE.Value
anyValueToJSON Nothing = AE.Null
anyValueToJSON (Just av) =
  case av ^. PCF.maybe'value of
    Nothing -> AE.Null
    Just (PC.AnyValue'StringValue txt) -> AE.String txt
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
      AE.String $ B64.extractBase64 $ B64.encodeBase64 bs


-- Convert Resource to JSON
resourceToJSON :: Maybe PR.Resource -> AE.Value
resourceToJSON Nothing = AE.Null
resourceToJSON (Just resource) =
  let
    attrs = V.fromList $ resource ^. PRF.attributes
    -- Process resource attributes with proper nesting
    attrPairs = [(kv ^. PCF.key, anyValueToJSON (Just (kv ^. PCF.value))) | kv <- V.toList attrs]
    -- Special case keys that should remain flat
    specialKeys = ["at-project-key", "at-project-id"]
    -- Split into flat and nested categories
    (flatPairs, nestedPairs) = partition (\(k, _) -> k `elem` specialKeys) attrPairs

    -- Create properly nested structure
    nestedObj = nestedJsonFromDotNotation nestedPairs

    -- Add flat keys at top level
    flatObj = AE.object [AEK.fromText k AE..= v | (k, v) <- flatPairs]
   in
    -- Merge both objects
    AE.Object $
      KEM.union
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
convertResourceLogsToOtelLogs :: V.Vector (Text, Projects.ProjectId, Integer) -> PL.ResourceLogs -> [OtelLogsAndSpans]
convertResourceLogsToOtelLogs pids resourceLogs =
  let projectKey = fromMaybe "" $ listToMaybe $ V.toList $ getLogAttributeValue "at-project-key" (V.singleton resourceLogs)
      projectId = case find (\(k, _, count) -> k == projectKey) pids of
        Just (_, v, count) -> if count >= 10000 then Nothing else Just v
        Nothing ->
          let pidText = fromMaybe "" $ listToMaybe $ V.toList $ getLogAttributeValue "at-project-id" (V.singleton resourceLogs)
              uId = UUID.fromText pidText
           in ((Just . Projects.ProjectId) =<< uId)
   in case projectId of
        Just pid -> convertScopeLogsToOtelLogs pid (Just $ resourceLogs ^. PLF.resource) resourceLogs
        _ -> []


-- | Convert ScopeLogs to OtelLogsAndSpans
convertScopeLogsToOtelLogs :: Projects.ProjectId -> Maybe PR.Resource -> PL.ResourceLogs -> [OtelLogsAndSpans]
convertScopeLogsToOtelLogs pid resourceM resourceLogs =
  join $
    V.toList $
      V.map
        ( \scopeLog ->
            let scope = Just $ scopeLog ^. PLF.scope
                logRecords = V.fromList $ scopeLog ^. PLF.logRecords
             in V.toList $ V.map (convertLogRecordToOtelLog pid resourceM scope) logRecords
        )
        (V.fromList $ resourceLogs ^. PLF.scopeLogs)


-- | Convert LogRecord to OtelLogsAndSpans
convertLogRecordToOtelLog :: Projects.ProjectId -> Maybe PR.Resource -> Maybe PC.InstrumentationScope -> PL.LogRecord -> OtelLogsAndSpans
convertLogRecordToOtelLog pid resourceM scopeM logRecord =
  let timeNano = logRecord ^. PLF.timeUnixNano
      observedTimeNano = logRecord ^. PLF.observedTimeUnixNano
      severityText = logRecord ^. PLF.severityText
      severityNumber = fromEnum (logRecord ^. PLF.severityNumber)
   in OtelLogsAndSpans
        { project_id = pid.toText
        , id = UUID.nil -- Will be replaced in bulkInsertOtelLogsAndSpansTF
        , timestamp = if timeNano == 0 then nanosecondsToUTC observedTimeNano else nanosecondsToUTC timeNano
        , observed_timestamp = Just $ nanosecondsToUTC observedTimeNano
        , context =
            Just $
              Context
                { trace_id = Just $ byteStringToHexText $ logRecord ^. PLF.traceId
                , span_id = Just $ byteStringToHexText $ logRecord ^. PLF.spanId
                , trace_state = Nothing
                , trace_flags = Nothing
                , is_remote = Nothing
                }
        , level = Just severityText
        , severity =
            Just $
              Severity
                { severity_text = parseSeverityLevel severityText
                , severity_number = severityNumber
                }
        , body = Just $ anyValueToJSON $ Just $ logRecord ^. PLF.body
        , attributes = migrateHttpSemanticConventions $ jsonToMap $ removeProjectId $ keyValueToJSON $ V.fromList $ logRecord ^. PLF.attributes
        , resource = jsonToMap $ removeProjectId $ resourceToJSON resourceM
        , hashes = V.empty
        , kind = Just "log"
        , status_code = Nothing
        , status_message = Nothing
        , duration = Nothing
        , start_time = nanosecondsToUTC observedTimeNano
        , end_time = Nothing
        , events = Nothing
        , links = Nothing
        , name = Nothing
        , parent_id = Nothing
        , date = if timeNano == 0 then nanosecondsToUTC observedTimeNano else nanosecondsToUTC timeNano
        }


-- | Convert ResourceSpans to OtelLogsAndSpans
convertResourceSpansToOtelLogs :: V.Vector (Text, Projects.ProjectId, Integer) -> V.Vector PT.ResourceSpans -> [OtelLogsAndSpans]
convertResourceSpansToOtelLogs pids resourceSpans =
  join $
    V.toList $
      V.map
        ( \rs ->
            let projectKey = fromMaybe "" $ listToMaybe $ V.toList $ getSpanAttributeValue "at-project-key" (V.singleton rs)
                projectId = case find (\(k, _, count) -> k == projectKey) pids of
                  Just (_, v, count) -> if count >= 10000 then Nothing else Just v
                  Nothing ->
                    let pidText = fromMaybe "" $ listToMaybe $ V.toList $ getSpanAttributeValue "at-project-id" (V.singleton rs)
                        uId = UUID.fromText pidText
                     in ((Just . Projects.ProjectId) =<< uId)
             in case projectId of
                  Just pid -> convertScopeSpansToOtelLogs pid (Just $ rs ^. PTF.resource) rs
                  _ -> []
        )
        resourceSpans


-- | Convert ScopeSpans to OtelLogsAndSpans
convertScopeSpansToOtelLogs :: Projects.ProjectId -> Maybe PR.Resource -> PT.ResourceSpans -> [OtelLogsAndSpans]
convertScopeSpansToOtelLogs pid resourceM resourceSpans =
  join $
    V.toList $
      V.map
        ( \scopeSpan ->
            let scope = Just $ scopeSpan ^. PTF.scope
                spans = V.fromList $ scopeSpan ^. PTF.spans
             in V.toList $ V.map (convertSpanToOtelLog pid resourceM scope) spans
        )
        (V.fromList $ resourceSpans ^. PTF.scopeSpans)


-- | Convert Span to OtelLogsAndSpans
convertSpanToOtelLog :: Projects.ProjectId -> Maybe PR.Resource -> Maybe PC.InstrumentationScope -> PT.Span -> OtelLogsAndSpans
convertSpanToOtelLog pid resourceM scopeM pSpan =
  let startTimeNano = pSpan ^. PTF.startTimeUnixNano
      endTimeNano = pSpan ^. PTF.endTimeUnixNano
      durationNanos = endTimeNano - startTimeNano
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

      -- Convert events
      events = V.fromList $ pSpan ^. PTF.events
      eventsJson =
        if V.null events
          then Nothing
          else
            Just $
              AE.toJSON $
                V.map
                  ( \ev ->
                      AE.object
                        [ "event_name" AE..= (ev ^. PTF.name)
                        , "event_time" AE..= nanosecondsToUTC (ev ^. PTF.timeUnixNano)
                        , "event_attributes" AE..= keyValueToJSON (V.fromList $ ev ^. PTF.attributes)
                        , "event_dropped_attributes_count" AE..= (ev ^. PTF.droppedAttributesCount)
                        ]
                  )
                  events

      -- Convert links
      links = V.fromList $ pSpan ^. PTF.links
      linksJson =
        if V.null links
          then Nothing
          else
            Just $
              T.pack $
                show $
                  AE.toJSON $
                    V.map
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
      attributes = migrateHttpSemanticConventions $ jsonToMap $ keyValueToJSON $ V.fromList $ pSpan ^. PTF.attributes
      (req, res) = case Map.lookup "http" (fromMaybe Map.empty attributes) of
        Just (AE.Object http) -> (KEM.lookup "request" http, KEM.lookup "response" http)
        _ -> (Nothing, Nothing)
      body =
        if pSpan ^. PTF.name == "apitoolkit-http-span"
          then
            Just $
              AE.object
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
   in OtelLogsAndSpans
        { project_id = pid.toText
        , id = UUID.nil -- Will be replaced in bulkInsertOtelLogsAndSpansTF
        , timestamp = nanosecondsToUTC startTimeNano
        , observed_timestamp = Just $ nanosecondsToUTC startTimeNano
        , context =
            Just $
              Context
                { trace_id = Just $ byteStringToHexText $ pSpan ^. PTF.traceId
                , span_id = Just $ byteStringToHexText $ pSpan ^. PTF.spanId
                , trace_state = Just $ pSpan ^. PTF.traceState
                , trace_flags = Nothing
                , is_remote = Nothing
                }
        , level = Nothing
        , severity = Nothing
        , body
        , attributes
        , resource = jsonToMap $ resourceToJSON resourceM
        , hashes = V.empty
        , kind = spanKindText
        , status_code = statusCodeText
        , status_message = statusMsgText
        , duration = Just $ fromIntegral durationNanos
        , start_time = nanosecondsToUTC startTimeNano
        , end_time = Just $ nanosecondsToUTC endTimeNano
        , events = eventsJson
        , links = linksJson
        , name = Just $ pSpan ^. PTF.name
        , parent_id = parentId
        , date = nanosecondsToUTC startTimeNano
        }


-- | Convert ResourceMetrics to MetricRecords
convertResourceMetricsToMetricRecords :: Projects.ProjectId -> V.Vector PM.ResourceMetrics -> [Telemetry.MetricRecord]
convertResourceMetricsToMetricRecords pid resourceMetrics =
  join $ V.toList $ V.map (convertResourceMetricToMetricRecords pid) resourceMetrics


-- | Convert a single ResourceMetrics to MetricRecords
convertResourceMetricToMetricRecords :: Projects.ProjectId -> PM.ResourceMetrics -> [Telemetry.MetricRecord]
convertResourceMetricToMetricRecords pid resourceMetric =
  let resourceM = Just $ resourceMetric ^. PMF.resource
      scopeMetrics = V.fromList $ resourceMetric ^. PMF.scopeMetrics
   in join $
        V.toList $
          V.map
            ( \sm ->
                let scope = Just $ sm ^. PMF.scope
                    metrics = V.fromList $ sm ^. PMF.metrics
                 in join $ V.toList $ V.map (convertMetricToMetricRecords pid resourceM scope) metrics
            )
            scopeMetrics


-- | Convert a single Metric to MetricRecords
convertMetricToMetricRecords :: Projects.ProjectId -> Maybe PR.Resource -> Maybe PC.InstrumentationScope -> PM.Metric -> [Telemetry.MetricRecord]
convertMetricToMetricRecords pid resourceM scopeM metric =
  -- For brevity, implementing only gauge metrics as an example
  -- In a complete implementation, you would handle all metric types
  case metric ^. PMF.maybe'data' of
    Just metricData ->
      case metricData of
        PM.Metric'Gauge gauge ->
          let gaugePoints = V.fromList $ gauge ^. PMF.dataPoints
           in V.toList $
                V.map
                  ( \point ->
                      let startTimeNano = point ^. PMF.startTimeUnixNano
                          timeNano = point ^. PMF.timeUnixNano
                          value = case point ^. PMF.maybe'value of
                            Just (PM.NumberDataPoint'AsDouble d) -> d
                            Just (PM.NumberDataPoint'AsInt i) -> fromIntegral i
                            _ -> 0
                          attributes = keyValueToJSON $ V.fromList $ point ^. PMF.attributes
                       in Telemetry.MetricRecord
                            { projectId = Projects.unProjectId pid
                            , id = Nothing
                            , metricName = metric ^. PMF.name
                            , metricDescription = metric ^. PMF.description
                            , metricUnit = metric ^. PMF.unit
                            , timestamp = nanosecondsToUTC startTimeNano
                            , metricTime = nanosecondsToUTC timeNano
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

runServer :: Log.Logger -> AuthContext -> IO ()
runServer appLogger appCtx = runServerWithHandlers def config $ (services appLogger appCtx)
  where
    serverHost = "localhost"
    serverPort = 4317
    config :: ServerConfig
    config =
      ServerConfig
        { serverInsecure = Just (InsecureConfig (Just $ toString serverHost) (fromIntegral serverPort))
        , serverSecure = Nothing
        }


-- | Trace service handler (Export)
traceServiceExport :: Log.Logger -> AuthContext -> Proto TS.ExportTraceServiceRequest -> IO (Proto TS.ExportTraceServiceResponse)
traceServiceExport appLogger appCtx (Proto req) = do
  _ <- runBackground appLogger appCtx do
    Log.logInfo "Received trace export request" AE.Null

    let resourceSpans = V.fromList $ req ^. TSF.resourceSpans
        projectKeys = getSpanAttributeValue "at-project-key" resourceSpans

    projectIdsAndKeys <- dbtToEff $ ProjectApiKeys.projectIdsByProjectApiKeys projectKeys

    let spans = convertResourceSpansToOtelLogs projectIdsAndKeys resourceSpans
        spans' = V.fromList spans
        apitoolkitSpans =
          V.mapMaybe
            ( \s ->
                if s.name == Just "apitoolkit-http-span"
                  then convertSpanToRequestMessage s "apitoolkit-http-span"
                  else Nothing
            )
            spans'

    unless (V.null apitoolkitSpans) $ do
      void $ ProcessMessage.processRequestMessages $ V.toList apitoolkitSpans <&> ("",)

    unless (null spans) do
      Telemetry.bulkInsertOtelLogsAndSpansTF spans'
      Anomalies.bulkInsertErrors $ Telemetry.getAllATErrors spans'

  -- Return an empty response
  pure $ defMessage


-- | Logs service handler (Export)
logsServiceExport :: Log.Logger -> AuthContext -> Proto LS.ExportLogsServiceRequest -> IO (Proto LS.ExportLogsServiceResponse)
logsServiceExport appLogger appCtx (Proto req) = do
  _ <- runBackground appLogger appCtx do
    Log.logInfo "Received logs export request" AE.Null

    let resourceLogs = V.fromList $ req ^. PLF.resourceLogs
        projectKeys = getLogAttributeValue "at-project-key" resourceLogs

    projectIdsAndKeys <- dbtToEff $ ProjectApiKeys.projectIdsByProjectApiKeys projectKeys

    let logs = join $ V.toList $ V.map (convertResourceLogsToOtelLogs projectIdsAndKeys) resourceLogs

    unless (null logs) do
      Telemetry.bulkInsertOtelLogsAndSpansTF (V.fromList logs)

  -- Return an empty response
  pure $ defMessage


-- | Metrics service handler (Export)
metricsServiceExport :: Log.Logger -> AuthContext -> Proto MS.ExportMetricsServiceRequest -> IO (Proto MS.ExportMetricsServiceResponse)
metricsServiceExport appLogger appCtx (Proto req) = do
  _ <- runBackground appLogger appCtx do
    Log.logInfo "Received metrics export request" AE.Null

    let resourceMetrics = V.fromList $ req ^. PMF.resourceMetrics
        projectKey = getMetricAttributeValue "at-project-key" resourceMetrics

    pidM <- do
      p1 <- join <$> (forM projectKey $ \key -> ProjectApiKeys.getProjectIdByApiKey key)
      let p2 = Projects.projectIdFromText =<< getMetricAttributeValue "at-project-id" resourceMetrics
      pure (p1 <|> p2)

    case pidM of
      Just pid -> do
        let metricRecords = convertResourceMetricsToMetricRecords pid resourceMetrics

        unless (null metricRecords) $
          Telemetry.bulkInsertMetrics (V.fromList metricRecords)
      Nothing -> Log.logAttention_ "Project API Key and project ID not available in metrics"

  -- Return an empty response
  pure $ defMessage


services :: Log.Logger -> AuthContext -> [SomeRpcHandler IO]
services appLogger appCtx =
  ( fromMethods $
      simpleMethods
        (mkNonStreaming $ traceServiceExport appLogger appCtx :: ServerHandler IO (Protobuf TS.TraceService "export"))
  )
    <> ( fromMethods $
          simpleMethods
            (mkNonStreaming $ logsServiceExport appLogger appCtx :: ServerHandler IO (Protobuf LS.LogsService "export"))
       )
    <> ( fromMethods $
          simpleMethods
            (mkNonStreaming $ metricsServiceExport appLogger appCtx :: ServerHandler IO (Protobuf MS.MetricsService "export"))
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
