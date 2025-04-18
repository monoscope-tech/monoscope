{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
import Data.ProtoLens.Encoding (decodeMessage)
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

import Lens.Micro ((^.), (^?))
import Log qualified
import Models.Apis.Anomalies qualified as Anomalies
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (Context (..), OtelLogsAndSpans (..), Severity (..), SpanKind (..), SpanStatus (..), convertSpanToRequestMessage)
import Models.Telemetry.Telemetry qualified as Telemetry
import ProcessMessage qualified
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService (ExportLogsServiceRequest)
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService (ExportMetricsServiceRequest)
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService (ExportTraceServiceRequest)
import Proto.Opentelemetry.Proto.Common.V1.Common (AnyValue, InstrumentationScope, KeyValue)
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as C
import Proto.Opentelemetry.Proto.Logs.V1.Logs (LogRecord, ResourceLogs, SeverityNumber (..))
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as L
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as M
import Proto.Opentelemetry.Proto.Resource.V1.Resource (Resource)
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields qualified as R
import Proto.Opentelemetry.Proto.Trace.V1.Trace (ResourceSpans, Span, Span'SpanKind (..), Status, Status'StatusCode (..))
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as T
import Relude hiding (ask)
import RequestMessages (RequestMessage (..))
import System.Config (AuthContext)

-- GRPC imports
import Grapesy qualified
import Grapesy.Server qualified
import Network.Socket (ServiceName)
import Network.Socket qualified as Socket
import Control.Monad.IO.Class (liftIO)


-- | Generic lens-based attribute extraction from spans
getSpanAttributeValue :: Text -> V.Vector ResourceSpans -> V.Vector Text
getSpanAttributeValue attribute rss = V.mapMaybe (\rs -> getResourceAttr rs <|> getSpanAttr rs) rss
  where
    getResourceAttr rs =
      let resource = rs ^. T.resource
       in getAttr (V.fromList $ resource ^. R.attributes)

    getSpanAttr rs =
      let scopeSpans = V.fromList $ rs ^. T.scopeSpans
          allSpans = join $ V.map (\ss -> V.fromList $ ss ^. T.spans) scopeSpans
       in listToMaybe $ V.toList $ V.mapMaybe (\s -> getAttr (V.fromList $ s ^. T.attributes)) allSpans

    getAttr :: V.Vector KeyValue -> Maybe Text
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
getLogAttributeValue :: Text -> V.Vector ResourceLogs -> V.Vector Text
getLogAttributeValue attribute rls = V.mapMaybe (\rl -> getResourceAttr rl <|> getLogAttr rl) rls
  where
    getResourceAttr rl =
      let resource = rl ^. L.resource
       in getAttr (V.fromList $ resource ^. R.attributes)

    getLogAttr rl =
      let scopeLogs = V.fromList $ rl ^. L.scopeLogs
          allLogs = join $ V.map (\sl -> V.fromList $ sl ^. L.logRecords) scopeLogs
       in listToMaybe $ V.toList $ V.mapMaybe (\lr -> getAttr (V.fromList $ lr ^. L.attributes)) allLogs

    getAttr :: V.Vector KeyValue -> Maybe Text
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
getMetricAttributeValue :: Text -> V.Vector ResourceMetrics -> Maybe Text
getMetricAttributeValue attribute rms = join $ listToMaybe $ V.toList $ V.map getResourceAttr $ V.filter (\rm -> isJust $ getResourceAttr rm) rms
  where
    getResourceAttr rm =
      let resource = rm ^. M.resource
       in getAttr (V.fromList $ resource ^. R.attributes)

    getAttr :: V.Vector KeyValue -> Maybe Text
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
            case (decodeMessage msg :: Either String ExportLogsServiceRequest) of
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
            case (decodeMessage msg :: Either String ExportTraceServiceRequest) of
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
            case (decodeMessage msg :: Either String ExportMetricsServiceRequest) of
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
keyValueToJSON :: V.Vector KeyValue -> AE.Value
keyValueToJSON kvs =
  AE.object $
    V.foldr (\kv acc -> (AEK.fromText (kv ^. C.key), anyValueToJSON (kv ^? C.value)) : acc) [] kvs


-- Convert AnyValue to JSON
anyValueToJSON :: Maybe AnyValue -> AE.Value
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
resourceToJSON :: Maybe Resource -> AE.Value
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
convertResourceLogsToOtelLogs :: V.Vector (Text, Projects.ProjectId, Integer) -> ResourceLogs -> [OtelLogsAndSpans]
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
convertScopeLogsToOtelLogs :: Projects.ProjectId -> Maybe Resource -> ResourceLogs -> [OtelLogsAndSpans]
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
convertLogRecordToOtelLog :: Projects.ProjectId -> Maybe Resource -> Maybe InstrumentationScope -> LogRecord -> OtelLogsAndSpans
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
convertResourceSpansToOtelLogs :: V.Vector (Text, Projects.ProjectId, Integer) -> V.Vector ResourceSpans -> [OtelLogsAndSpans]
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
convertScopeSpansToOtelLogs :: Projects.ProjectId -> Maybe Resource -> ResourceSpans -> [OtelLogsAndSpans]
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
convertSpanToOtelLog :: Projects.ProjectId -> Maybe Resource -> Maybe InstrumentationScope -> Span -> OtelLogsAndSpans
convertSpanToOtelLog pid resourceM scopeM span =
  let startTimeNano = span ^. T.startTimeUnixNano
      endTimeNano = span ^. T.endTimeUnixNano
      durationNanos = endTimeNano - startTimeNano
      spanKind = span ^. T.kind
      spanKindText = case spanKind of
        Span'SPAN_KIND_INTERNAL -> Just "internal"
        Span'SPAN_KIND_SERVER -> Just "server"
        Span'SPAN_KIND_CLIENT -> Just "client"
        Span'SPAN_KIND_PRODUCER -> Just "producer"
        Span'SPAN_KIND_CONSUMER -> Just "consumer"
        _ -> Just "unspecified"
      statusM = Just $ span ^. T.status
      statusCodeText = case statusM of
        Just status -> case status ^. T.code of
          Status'STATUS_CODE_OK -> Just "OK"
          Status'STATUS_CODE_ERROR -> Just "ERROR"
          _ -> Just "UNSET"
        Nothing -> Nothing
      statusMsgText = case statusM of
        Just status -> Just $ status ^. T.message
        Nothing -> Nothing
      parentSpanId = span ^. T.parentSpanId
      parentId =
        if BS.null parentSpanId
          then Nothing
          else Just $ byteStringToHexText parentSpanId

      -- Convert events
      events = V.fromList $ span ^. T.events
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
      links = V.fromList $ span ^. T.links
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
                { trace_id = Just $ byteStringToHexText $ span ^. T.traceId
                , span_id = Just $ byteStringToHexText $ span ^. T.spanId
                , trace_state = Just $ span ^. T.traceState
                , trace_flags = Nothing
                , is_remote = Nothing
                }
        , level = Nothing
        , severity = Nothing
        , body = Nothing
        , attributes = jsonToMap $ keyValueToJSON $ V.fromList $ span ^. T.attributes
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
        , name = Just $ span ^. T.name
        , parent_id = parentId
        , date = nanosecondsToUTC startTimeNano
        }


-- | Convert ResourceMetrics to MetricRecords
convertResourceMetricsToMetricRecords :: Projects.ProjectId -> V.Vector ResourceMetrics -> [Telemetry.MetricRecord]
convertResourceMetricsToMetricRecords pid resourceMetrics =
  join $ V.toList $ V.map (convertResourceMetricToMetricRecords pid) resourceMetrics


-- | Convert a single ResourceMetrics to MetricRecords
convertResourceMetricToMetricRecords :: Projects.ProjectId -> ResourceMetrics -> [Telemetry.MetricRecord]
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
convertMetricToMetricRecords :: Projects.ProjectId -> Maybe Resource -> Maybe InstrumentationScope -> Metric -> [Telemetry.MetricRecord]
convertMetricToMetricRecords pid resourceM scopeM metric =
  -- For brevity, implementing only gauge metrics as an example
  -- In a complete implementation, you would handle all metric types
  case metric ^. M.maybe'data' of
    Just metricData ->
      case metricData of
        Metric'Gauge gauge ->
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
