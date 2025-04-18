module Opentelemetry.OtlpServer (processList, runServer, otlpServer) where

import Control.Exception.Annotated (checkpoint)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as KEM
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as LBS
import Data.Effectful.UUID (UUIDEff)
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Encoding (decodeMessage, encodeMessage)
import Data.Scientific (fromFloatDigits)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
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
import Lens.Micro ((^?))
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
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as C
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as CF
import Proto.Opentelemetry.Proto.Logs.V1.Logs qualified as PL
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as L
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as LF
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as PM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as M
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as MF
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as PR
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields qualified as R
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields qualified as RF
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as PT
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as T
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as TF
import Relude hiding (ask)
import RequestMessages (RequestMessage (..))
import System.Config (AuthContext)
import System.Types (runBackground)


-- import Network.GRPC.Server.Service (Service, service, method, fromServices)

-- | Generic lens-based attribute extraction from spans
getSpanAttributeValue :: Text -> V.Vector PT.ResourceSpans -> V.Vector Text
getSpanAttributeValue attribute rss = V.mapMaybe (\rs -> getResourceAttr rs <|> getSpanAttr rs) rss
  where
    getResourceAttr rs =
      let resourceVal = rs ^. T.resource
       in getAttr (V.fromList $ resourceVal ^. R.attributes)

    getSpanAttr rs =
      let scopeSpans = V.fromList $ rs ^. T.scopeSpans
          allSpans = join $ V.map (\ss -> V.fromList $ ss ^. T.spans) scopeSpans
       in listToMaybe $ V.toList $ V.mapMaybe (\s -> getAttr (V.fromList $ s ^. T.attributes)) allSpans

    getAttr :: V.Vector PC.KeyValue -> Maybe Text
    getAttr kvs =
      let matchingKvs = V.filter (\kv -> kv ^. C.key == attribute) kvs
       in if V.null matchingKvs
            then Nothing
            else
              let kv = V.head matchingKvs
               in case kv ^? C.value . C.stringValue of
                    Just val -> Just val
                    Nothing -> Nothing


-- | Extract attribute values from resource logs
getLogAttributeValue :: Text -> V.Vector PL.ResourceLogs -> V.Vector Text
getLogAttributeValue attribute rls = V.mapMaybe (\rl -> getResourceAttr rl <|> getLogAttr rl) rls
  where
    getResourceAttr rl =
      let resource = rl ^. L.resource
       in getAttr (V.fromList $ resource ^. R.attributes)

    getLogAttr rl =
      let scopeLogs = V.fromList $ rl ^. L.scopeLogs
          allLogs = join $ V.map (\sl -> V.fromList $ sl ^. L.logRecords) scopeLogs
       in listToMaybe $ V.toList $ V.mapMaybe (\lr -> getAttr (V.fromList $ lr ^. L.attributes)) allLogs

    getAttr :: V.Vector PC.KeyValue -> Maybe Text
    getAttr kvs =
      let matchingKvs = V.filter (\kv -> kv ^. C.key == attribute) kvs
       in if V.null matchingKvs
            then Nothing
            else
              let kv = V.head matchingKvs
               in case kv ^? C.value . C.stringValue of
                    Just val -> Just val
                    Nothing -> Nothing


-- | Extract metric attribute values
getMetricAttributeValue :: Text -> V.Vector PM.ResourceMetrics -> Maybe Text
getMetricAttributeValue attribute rms = join $ listToMaybe $ V.toList $ V.map getResourceAttr $ V.filter (\rm -> isJust $ getResourceAttr rm) rms
  where
    getResourceAttr rm =
      let resource = rm ^. M.resource
       in getAttr (V.fromList $ resource ^. R.attributes)

    getAttr :: V.Vector PC.KeyValue -> Maybe Text
    getAttr kvs =
      let matchingKvs = V.filter (\kv -> kv ^. C.key == attribute) kvs
       in if V.null matchingKvs
            then Nothing
            else
              let kv = V.head matchingKvs
               in case kv ^? C.value . C.stringValue of
                    Just val -> Just val
                    Nothing -> Nothing


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
                Log.logAttention "processList:logs: unable to parse logs service request with err " (AE.object ["err" AE..= err, "decoded_msg" AE..= (decodeUtf8 @Text msg)])
                pure (ackId, [])
              Right logReq -> do
                let resourceLogs = V.fromList $ logReq ^. L.resourceLogs
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
                Log.logAttention "processList:traces: unable to parse traces service request with err " (AE.object ["err" AE..= err, "decoded_msg" AE..= (decodeUtf8 @Text msg)])
                pure (ackId, [])
              Right traceReq -> do
                let resourceSpans = V.fromList $ traceReq ^. T.resourceSpans
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
            checkpoint "processList:traces:bulkInsertSpans" $
              Telemetry.bulkInsertOtelLogsAndSpansTF spans'
            checkpoint "processList:traces:bulkInsertErrors" $
              Anomalies.bulkInsertErrors $
                Telemetry.getAllATErrors spans'

          pure $ V.toList ackIds
        Just "org.opentelemetry.otlp.metrics.v1" -> checkpoint "processList:metrics" do
          results <- V.forM msgs' $ \(ackId, msg) ->
            case (decodeMessage msg :: Either String MS.ExportMetricsServiceRequest) of
              Left err -> do
                Log.logAttention "processList:metrics: unable to parse metrics service request with err " (AE.object ["err" AE..= err, "decoded_msg" AE..= (decodeUtf8 @Text msg)])
                pure (ackId, [])
              Right metricReq -> checkpoint "processList:metrics:getProjectId" do
                let resourceMetrics = V.fromList $ metricReq ^. M.resourceMetrics
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

-- Convert nanoseconds to UTCTime
nanosecondsToUTC :: Word64 -> UTCTime
nanosecondsToUTC ns = posixSecondsToUTCTime (fromIntegral ns / 1e9)


-- Convert ByteString to hex Text
byteStringToHexText :: BS.ByteString -> Text
byteStringToHexText bs = TE.decodeUtf8 (B16.encode bs)


-- Convert a list of KeyValue to a JSON object
keyValueToJSON :: V.Vector PC.KeyValue -> AE.Value
keyValueToJSON kvs =
  AE.object $
    V.foldr (\kv acc -> (AEK.fromText (kv ^. C.key), anyValueToJSON (kv ^? C.value)) : acc) [] kvs


-- Convert AnyValue to JSON
anyValueToJSON :: Maybe PC.AnyValue -> AE.Value
anyValueToJSON Nothing = AE.Null
anyValueToJSON (Just av) =
  case av ^? C.stringValue of
    Just val -> AE.String val
    Nothing -> case av ^? C.boolValue of
      Just val -> AE.Bool val
      Nothing -> case av ^? C.intValue of
        Just val -> AE.Number (fromIntegral val)
        Nothing -> case av ^? C.doubleValue of
          Just val -> AE.Number (fromFloatDigits val)
          Nothing -> case av ^? C.arrayValue of
            Just arr -> AE.Array $ V.map (anyValueToJSON . Just) (V.fromList $ arr ^. C.values)
            Nothing -> AE.Null


-- Convert Resource to JSON
resourceToJSON :: Maybe PR.Resource -> AE.Value
resourceToJSON Nothing = AE.Null
resourceToJSON (Just resource) = keyValueToJSON (V.fromList $ resource ^. R.attributes)


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
        Just pid -> convertScopeLogsToOtelLogs pid (Just $ resourceLogs ^. L.resource) resourceLogs
        _ -> []


-- | Convert ScopeLogs to OtelLogsAndSpans
convertScopeLogsToOtelLogs :: Projects.ProjectId -> Maybe PR.Resource -> PL.ResourceLogs -> [OtelLogsAndSpans]
convertScopeLogsToOtelLogs pid resourceM resourceLogs =
  join $
    V.toList $
      V.map
        ( \scopeLog ->
            let scope = Just $ scopeLog ^. L.scope
                logRecords = V.fromList $ scopeLog ^. L.logRecords
             in V.toList $ V.map (convertLogRecordToOtelLog pid resourceM scope) logRecords
        )
        (V.fromList $ resourceLogs ^. L.scopeLogs)


-- | Convert LogRecord to OtelLogsAndSpans
convertLogRecordToOtelLog :: Projects.ProjectId -> Maybe PR.Resource -> Maybe PC.InstrumentationScope -> PL.LogRecord -> OtelLogsAndSpans
convertLogRecordToOtelLog pid resourceM scopeM logRecord =
  let timeNano = logRecord ^. L.timeUnixNano
      observedTimeNano = logRecord ^. L.observedTimeUnixNano
      severityText = logRecord ^. L.severityText
      severityNumber = fromEnum (logRecord ^. L.severityNumber)
   in OtelLogsAndSpans
        { project_id = pid.toText
        , id = UUID.nil -- Will be replaced in bulkInsertOtelLogsAndSpansTF
        , timestamp = if timeNano == 0 then nanosecondsToUTC observedTimeNano else nanosecondsToUTC timeNano
        , observed_timestamp = Just $ nanosecondsToUTC observedTimeNano
        , context =
            Just $
              Context
                { trace_id = Just $ byteStringToHexText $ logRecord ^. L.traceId
                , span_id = Just $ byteStringToHexText $ logRecord ^. L.spanId
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
        , body = Just $ anyValueToJSON $ logRecord ^? L.body
        , attributes = jsonToMap $ removeProjectId $ keyValueToJSON $ V.fromList $ logRecord ^. L.attributes
        , resource = jsonToMap $ removeProjectId $ resourceToJSON resourceM
        , hashes = V.empty
        , kind = Nothing
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
                  Just pid -> convertScopeSpansToOtelLogs pid (Just $ rs ^. T.resource) rs
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
            let scope = Just $ scopeSpan ^. T.scope
                spans = V.fromList $ scopeSpan ^. T.spans
             in V.toList $ V.map (convertSpanToOtelLog pid resourceM scope) spans
        )
        (V.fromList $ resourceSpans ^. T.scopeSpans)


-- | Convert Span to OtelLogsAndSpans
convertSpanToOtelLog :: Projects.ProjectId -> Maybe PR.Resource -> Maybe PC.InstrumentationScope -> PT.Span -> OtelLogsAndSpans
convertSpanToOtelLog pid resourceM scopeM pSpan =
  let startTimeNano = pSpan ^. T.startTimeUnixNano
      endTimeNano = pSpan ^. T.endTimeUnixNano
      durationNanos = endTimeNano - startTimeNano
      spanKind = pSpan ^. T.kind
      spanKindText = case spanKind of
        PT.Span'SPAN_KIND_INTERNAL -> Just "internal"
        PT.Span'SPAN_KIND_SERVER -> Just "server"
        PT.Span'SPAN_KIND_CLIENT -> Just "client"
        PT.Span'SPAN_KIND_PRODUCER -> Just "producer"
        PT.Span'SPAN_KIND_CONSUMER -> Just "consumer"
        _ -> Just "unspecified"
      statusM = Just $ pSpan ^. T.status
      statusCodeText = case statusM of
        Just status -> case status ^. T.code of
          PT.Status'STATUS_CODE_OK -> Just "OK"
          PT.Status'STATUS_CODE_ERROR -> Just "ERROR"
          _ -> Just "UNSET"
        Nothing -> Nothing
      statusMsgText = case statusM of
        Just status -> Just $ status ^. T.message
        Nothing -> Nothing
      parentSpanId = pSpan ^. T.parentSpanId
      parentId =
        if BS.null parentSpanId
          then Nothing
          else Just $ byteStringToHexText parentSpanId

      -- Convert events
      events = V.fromList $ pSpan ^. T.events
      eventsJson =
        if V.null events
          then Nothing
          else
            Just $
              AE.toJSON $
                V.map
                  ( \ev ->
                      AE.object
                        [ "event_name" AE..= (ev ^. T.name)
                        , "event_time" AE..= nanosecondsToUTC (ev ^. T.timeUnixNano)
                        , "event_attributes" AE..= keyValueToJSON (V.fromList $ ev ^. T.attributes)
                        , "event_dropped_attributes_count" AE..= (ev ^. T.droppedAttributesCount)
                        ]
                  )
                  events

      -- Convert links
      links = V.fromList $ pSpan ^. T.links
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
                            [ "link_span_id" AE..= byteStringToHexText (link ^. T.spanId)
                            , "link_trace_id" AE..= byteStringToHexText (link ^. T.traceId)
                            , "link_attributes" AE..= keyValueToJSON (V.fromList $ link ^. T.attributes)
                            , "link_dropped_attributes_count" AE..= (link ^. T.droppedAttributesCount)
                            , "link_flags" AE..= (link ^. T.traceState)
                            ]
                      )
                      links
   in OtelLogsAndSpans
        { project_id = pid.toText
        , id = UUID.nil -- Will be replaced in bulkInsertOtelLogsAndSpansTF
        , timestamp = nanosecondsToUTC startTimeNano
        , observed_timestamp = Just $ nanosecondsToUTC startTimeNano
        , context =
            Just $
              Context
                { trace_id = Just $ byteStringToHexText $ pSpan ^. T.traceId
                , span_id = Just $ byteStringToHexText $ pSpan ^. T.spanId
                , trace_state = Just $ pSpan ^. T.traceState
                , trace_flags = Nothing
                , is_remote = Nothing
                }
        , level = Nothing
        , severity = Nothing
        , body = Nothing
        , attributes = jsonToMap $ keyValueToJSON $ V.fromList $ pSpan ^. T.attributes
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
        , name = Just $ pSpan ^. T.name
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
  let resourceM = Just $ resourceMetric ^. M.resource
      scopeMetrics = V.fromList $ resourceMetric ^. M.scopeMetrics
   in join $
        V.toList $
          V.map
            ( \sm ->
                let scope = Just $ sm ^. M.scope
                    metrics = V.fromList $ sm ^. M.metrics
                 in join $ V.toList $ V.map (convertMetricToMetricRecords pid resourceM scope) metrics
            )
            scopeMetrics


-- | Convert a single Metric to MetricRecords
convertMetricToMetricRecords :: Projects.ProjectId -> Maybe PR.Resource -> Maybe PC.InstrumentationScope -> PM.Metric -> [Telemetry.MetricRecord]
convertMetricToMetricRecords pid resourceM scopeM metric =
  -- For brevity, implementing only gauge metrics as an example
  -- In a complete implementation, you would handle all metric types
  case metric ^. M.maybe'data' of
    Just metricData ->
      case metricData of
        PM.Metric'Gauge gauge ->
          let gaugePoints = V.fromList $ gauge ^. M.dataPoints
           in V.toList $
                V.map
                  ( \point ->
                      let startTimeNano = point ^. M.startTimeUnixNano
                          timeNano = point ^. M.timeUnixNano
                          value = case point ^? M.asDouble of
                            Just v -> v
                            Nothing -> case point ^? M.asInt of
                              Just v -> fromIntegral v
                              Nothing -> 0
                          attributes = keyValueToJSON $ V.fromList $ point ^. M.attributes
                       in Telemetry.MetricRecord
                            { projectId = Projects.unProjectId pid
                            , id = Nothing
                            , metricName = metric ^. M.name
                            , metricDescription = metric ^. M.description
                            , metricUnit = metric ^. M.unit
                            , timestamp = nanosecondsToUTC startTimeNano
                            , metricTime = nanosecondsToUTC timeNano
                            , resource = removeProjectId $ resourceToJSON resourceM
                            , instrumentationScope = case scopeM of
                                Just scope ->
                                  AE.object
                                    [ "name" AE..= (scope ^. C.name)
                                    , "version" AE..= (scope ^. C.version)
                                    , "attributes" AE..= keyValueToJSON (V.fromList $ scope ^. C.attributes)
                                    ]
                                Nothing -> AE.Null
                            , metricValue = Telemetry.GaugeValue $ Telemetry.GaugeSum{value = value}
                            , exemplars = AE.object [] -- Empty object since we can't properly convert the exemplars;  AE.object ["exemplars" AE..= (point ^. M.exemplars)]
                            , metricType = Telemetry.MTGauge
                            , flags = fromIntegral $ point ^. M.flags
                            , attributes = attributes
                            , metricMetadata = case scopeM of
                                Just scope -> AE.object ["scope" AE..= (scope ^. C.name)]
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

-- | Data type to hold server options
data ServiceOptions = ServiceOptions
  { serverHost :: Text
  , serverPort :: Int
  , useCompression :: Bool
  , userAgentPrefix :: Maybe Text
  , userAgentSuffix :: Maybe Text
  , initialMetadata :: [(Text, Text)]
  , sslConfig :: Maybe Text -- Simplified SSL config for now
  , logger :: Maybe (Text -> IO ())
  , serverMaxReceiveMessageLength :: Maybe Int
  , serverMaxMetadataSize :: Maybe Int
  }


-- | Default service options
defaultServiceOptions :: ServiceOptions
defaultServiceOptions =
  ServiceOptions
    { serverHost = "localhost"
    , serverPort = 4317
    , useCompression = False
    , userAgentPrefix = Nothing
    , userAgentSuffix = Nothing
    , initialMetadata = []
    , sslConfig = Nothing
    , logger = Nothing
    , serverMaxReceiveMessageLength = Nothing
    , serverMaxMetadataSize = Nothing
    }


-- | Run the OpenTelemetry GRPC server
runServer :: Log.Logger -> AuthContext -> IO ()
runServer appLogger appCtx = do
  let opts =
        defaultServiceOptions
          { serverHost = "localhost"
          , serverPort = 4317
          }
  otlpServer opts appLogger appCtx


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

    let resourceLogs = V.fromList $ req ^. L.resourceLogs
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

    let resourceMetrics = V.fromList $ req ^. M.resourceMetrics
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


-- | Create the OpenTelemetry GRPC server
otlpServer :: ServiceOptions -> Log.Logger -> AuthContext -> IO ()
otlpServer opts appLogger appCtx = do
  runServerWithHandlers def config $ (services appLogger appCtx)
  where
    config :: ServerConfig
    config =
      ServerConfig
        { serverInsecure = Just (InsecureConfig (Just $ toString opts.serverHost) (fromIntegral opts.serverPort))
        , serverSecure = Nothing
        }


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
