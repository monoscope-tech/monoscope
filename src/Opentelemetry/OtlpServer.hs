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

module Opentelemetry.OtlpServer (runServer, processList) where

import Control.Lens hiding ((.=))
import Control.Lens.At
import Control.Lens.Extras (is)
import Control.Lens.Getter
import Control.Lens.Prism
import Control.Lens.Setter
import Control.Lens.TH
import Control.Lens.Wrapped
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as KEM
import Data.Aeson.Lens
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Effectful.UUID (UUIDEff)
import Data.Generics.Labels
import Data.Generics.Product.Fields
import Data.HashMap.Strict qualified as HashMap
import Data.Scientific
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Effectful
import Effectful.Ki qualified as Ki
import Effectful.Labeled (Labeled, labeled)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Reader.Static (ask)
import Effectful.Reader.Static qualified as Eff
import Effectful.Time (Time)
import Log qualified
import Models.Apis.Anomalies qualified as Anomalies
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (SpanKind (..), SpanStatus (..), convertSpanToRequestMessage)
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.GRPC.HighLevel.Client as HsGRPC
import Network.GRPC.HighLevel.Generated as HsGRPC
import Network.GRPC.HighLevel.Server as HsGRPC hiding (serverLoop)
import Network.GRPC.HighLevel.Server.Unregistered as HsGRPC (serverLoop)
import Opentelemetry.Proto.Collector.Logs.V1.LogsService
import Opentelemetry.Proto.Collector.Metrics.V1.MetricsService (ExportMetricsServiceRequest (ExportMetricsServiceRequest), ExportMetricsServiceResponse (ExportMetricsServiceResponse))
import Opentelemetry.Proto.Collector.Trace.V1.TraceService
import Opentelemetry.Proto.Common.V1.Common
import Opentelemetry.Proto.Logs.V1.Logs
import Opentelemetry.Proto.Metrics.V1.Metrics (
  ExponentialHistogram (..),
  ExponentialHistogramDataPoint (..),
  ExponentialHistogramDataPoint_Buckets (..),
  Gauge (..),
  Histogram (..),
  HistogramDataPoint (..),
  Metric (..),
  MetricData (..),
  NumberDataPoint (..),
  NumberDataPointValue (..),
  ResourceMetrics (..),
  ScopeMetrics (..),
  Sum (..),
  Summary (..),
  SummaryDataPoint (..),
  SummaryDataPoint_ValueAtQuantile (..),
 )
import Opentelemetry.Proto.Resource.V1.Resource
import Opentelemetry.Proto.Trace.V1.Trace (
  ResourceSpans (resourceSpansResource, resourceSpansScopeSpans),
  ScopeSpans (scopeSpansScope),
  Span (..),
  Span_Event (..),
  Span_Link (..),
  Span_SpanKind (..),
  Status (..),
  Status_StatusCode (..),
  scopeSpansSpans,
 )
import ProcessMessage qualified
import Proto3.Suite.Class qualified as HsProtobuf
import Proto3.Suite.Types qualified as HsProtobuf
import Proto3.Wire qualified as HsProtobuf
import Proto3.Wire.Decode qualified as HsProtobuf
import Relude hiding (ask)
import RequestMessages (RequestMessage (..))
import System.Config
import System.Types (runBackground)


-- | Generic lens-based attribute extraction from spans
getSpanAttributeValue :: Text -> V.Vector ResourceSpans -> V.Vector Text
getSpanAttributeValue attribute rss = V.mapMaybe (\rs -> getResourceAttr rs <|> getSpanAttr rs) rss
  where
    getResourceAttr rs = rs.resourceSpansResource >>= getAttr . resourceAttributes
    getSpanAttr rs = rs.resourceSpansScopeSpans & listToMaybe . V.toList . V.mapMaybe (getAttr . spanAttributes) . V.concatMap scopeSpansSpans
    getAttr :: V.Vector KeyValue -> Maybe Text
    getAttr = V.find ((== attribute) . toText . keyValueKey) >=> keyValueValue >=> anyValueValue >=> anyValueToString >=> (Just . toText)


getLogAttributeValue :: Text -> V.Vector ResourceLogs -> V.Vector Text
getLogAttributeValue attribute rls = V.mapMaybe (\rl -> getResourceAttr rl <|> getLogAttr rl) rls
  where
    getResourceAttr rl = rl.resourceLogsResource >>= getAttr . resourceAttributes
    getLogAttr rl = rl.resourceLogsScopeLogs & listToMaybe . V.toList . V.mapMaybe (getAttr . logRecordAttributes) . V.concatMap scopeLogsLogRecords
    getAttr :: V.Vector KeyValue -> Maybe Text
    getAttr = V.find ((== attribute) . toText . keyValueKey) >=> keyValueValue >=> anyValueValue >=> anyValueToString >=> (Just . toText)


getMetricAttributeValue :: Text -> V.Vector ResourceMetrics -> Maybe Text
getMetricAttributeValue attribute rms = listToMaybe $ V.toList $ V.mapMaybe getResourceAttr rms
  where
    getResourceAttr rm = rm.resourceMetricsResource >>= getAttr . resourceAttributes
    getAttr :: V.Vector KeyValue -> Maybe Text
    getAttr = V.find ((== attribute) . toText . keyValueKey) >=> keyValueValue >=> anyValueValue >=> anyValueToString >=> (Just . toText)


processList :: (Eff.Reader AuthContext :> es, DB :> es, Ki.StructuredConcurrency :> es, Log :> es, IOE :> es, Time :> es, UUIDEff :> es) => [(Text, ByteString)] -> HashMap Text Text -> Eff es [Text]
processList [] _ = pure []
processList msgs attrs = do
  let msgs' = V.fromList msgs
  appCtx <- ask @AuthContext
  case HashMap.lookup "ce-type" attrs of
    Just "org.opentelemetry.otlp.logs.v1" -> do
      results <-
        V.forM msgs' \(ackId, msg) -> case (HsProtobuf.fromByteString msg :: Either HsProtobuf.ParseError ExportLogsServiceRequest) of
          Left err -> do
            Log.logAttention_ $ "processList: unable to parse logs service request with err " <> show err <> (decodeUtf8 msg)
            pure (ackId, [])
          Right (ExportLogsServiceRequest logReq) -> do
            projectIdsAndKeys <- dbtToEff $ ProjectApiKeys.projectIdsByProjectApiKeys $ getLogAttributeValue "at-project-key" logReq
            pure (ackId, join $ V.map (convertToLog projectIdsAndKeys) logReq)
      let (ackIds, logs) = V.unzip results
      unless (null $ join logs) do
        _ <- Telemetry.bulkInsertOtelLogsAndSpansTF $ join logs
        pure ()
      pure $ V.toList ackIds
    Just "org.opentelemetry.otlp.traces.v1" -> do
      reqsAndProjects <- V.forM msgs' \(ackId, msg) -> do
        case (HsProtobuf.fromByteString msg :: Either HsProtobuf.ParseError ExportTraceServiceRequest) of
          Left err -> do
            Log.logAttention_ $ "processList: unable to parse traces service request with err " <> show err <> (decodeUtf8 msg)
            pure (ackId, ([], []))
          Right (ExportTraceServiceRequest traceReq) -> do
            projectIdsAndKeys <- dbtToEff $ ProjectApiKeys.projectIdsByProjectApiKeys $ getSpanAttributeValue "at-project-key" traceReq
            pure (ackId, (traceReq, projectIdsAndKeys))

      let (ackIds, reqsAndPids) = V.unzip reqsAndProjects
          results =
            V.map (\(req, pids) -> (join $ V.map (convertToSpan pids) req, req, pids)) reqsAndPids
          spans = join $ V.map (\(s, _, _) -> s) results
          apitoolkitSpans = V.map mapHTTPSpan spans
      unless (V.null apitoolkitSpans) do
        _ <- ProcessMessage.processRequestMessages $ V.toList $ V.catMaybes apitoolkitSpans <&> ("",)
        pass
      unless (V.null spans) do
        _ <- Telemetry.bulkInsertOtelLogsAndSpansTF spans
        _ <- Anomalies.bulkInsertErrors $ Telemetry.getAllATErrors spans
        pure ()

      pure $ V.toList ackIds
    Just "org.opentelemetry.otlp.metrics.v1" -> do
      results <-
        V.forM msgs' \(ackId, msg) -> do
          case (HsProtobuf.fromByteString msg :: Either HsProtobuf.ParseError ExportMetricsServiceRequest) of
            Left err -> do
              Log.logAttention_ $ "processList: unable to parse metrics service request with err " <> show err <> (decodeUtf8 msg)
              pure (ackId, [])
            Right (ExportMetricsServiceRequest metricReq) -> do
              pidM <- join <$> forM (getMetricAttributeValue "at-project-key" metricReq) ProjectApiKeys.getProjectIdByApiKey
              let pid2M = Projects.projectIdFromText =<< getMetricAttributeValue "at-project-id" metricReq
              case (pidM <|> pid2M) of
                Just pid -> pure (ackId, join $ V.map (convertToMetric pid) metricReq)
                Nothing -> do
                  Log.logAttention_ "processList: project API Key and project ID not available in trace"
                  pure (ackId, [])
      let (ackIds, metricVec) = V.unzip results
          metricRecords = join metricVec
      unless (null metricRecords) $ Telemetry.bulkInsertMetrics metricRecords
      pure $ V.toList ackIds
    _ -> do
      Log.logAttention "processList: unsupported opentelemetry data type" (AE.object ["ce-type" AE..= (HashMap.lookup "ce-type" attrs)])
      pure []


logsServiceExportH
  :: Log.Logger
  -> AuthContext
  -> ServerRequest 'Normal ExportLogsServiceRequest ExportLogsServiceResponse
  -> IO (ServerResponse 'Normal ExportLogsServiceResponse)
logsServiceExportH appLogger appCtx (ServerNormalRequest meta (ExportLogsServiceRequest req)) = do
  -- let projectKey = T.replace "Bearer " "" $ decodeUtf8 $ Unsafe.fromJust $ Safe.headMay =<< M.lookup "authorization" meta.metadata.unMap
  --     apiKeyUUID = projectApiKeyFromB64 appCtx.config.apiKeyEncryptionSecretKey projectKey
  _ <- runBackground appLogger appCtx do
    let projectKey = fromMaybe (error "Missing project key") $ listToMaybe $ V.toList $ getLogAttributeValue "at-project-key" req
    projectIdM <- ProjectApiKeys.getProjectIdByApiKey projectKey
    let pid = fromMaybe (error "project API Key is invalid pid") projectIdM

    let logRecords = join $ V.map (convertToLog [(projectKey, pid, 0)]) req
    Telemetry.bulkInsertOtelLogsAndSpansTF logRecords
  return (ServerNormalResponse (ExportLogsServiceResponse Nothing) mempty StatusOk "")


-- Convert nanoseconds to UTCTime
nanosecondsToUTC :: Word64 -> UTCTime
nanosecondsToUTC ns = posixSecondsToUTCTime (fromIntegral ns / 1e9)


byteStringToHexText :: BS.ByteString -> T.Text
byteStringToHexText bs = decodeUtf8 (B16.encode bs)


-- Convert a list of KeyValue to a JSONB object
keyValueToJSONB :: V.Vector KeyValue -> AE.Value
keyValueToJSONB kvs =
  AE.object $
    V.foldr (\kv acc -> (AEK.fromText $ toText kv.keyValueKey, convertAnyValue kv.keyValueValue) : acc) [] kvs


convertAnyValue :: Maybe AnyValue -> AE.Value
convertAnyValue (Just (AnyValue (Just (AnyValueValueStringValue val)))) = AE.String $ toText val
convertAnyValue (Just (AnyValue (Just (AnyValueValueBoolValue val)))) = AE.Bool val
convertAnyValue (Just (AnyValue (Just (AnyValueValueIntValue val)))) = AE.Number (fromIntegral val)
convertAnyValue (Just (AnyValue (Just (AnyValueValueDoubleValue val)))) = AE.Number (fromFloatDigits val)
convertAnyValue (Just (AnyValue (Just (AnyValueValueArrayValue vals)))) = AE.Array (V.map (convertAnyValue . Just) vals.arrayValueValues)
convertAnyValue _ = AE.Null


-- Convert Protobuf severity text to SeverityLevel
parseSeverityLevel :: Text -> Maybe Telemetry.SeverityLevel
parseSeverityLevel input = case T.toUpper input of
  "DEBUG" -> Just Telemetry.SLDebug
  "INFO" -> Just Telemetry.SLInfo
  "WARN" -> Just Telemetry.SLWarn
  "ERROR" -> Just Telemetry.SLError
  "FATAL" -> Just Telemetry.SLFatal
  _ -> Nothing


-- Convert a resource to JSONB
resourceToJSONB :: Maybe Resource -> AE.Value
resourceToJSONB (Just resource) = keyValueToJSONB resource.resourceAttributes
resourceToJSONB Nothing = AE.Null


-- Convert an instrumentation scope to JSONB
instrumentationScopeToJSONB :: Maybe InstrumentationScope -> AE.Value
instrumentationScopeToJSONB (Just scope) =
  AE.object
    [ "name" AE..= scope.instrumentationScopeName
    , "version" AE..= scope.instrumentationScopeVersion
    , "attributes" AE..= keyValueToJSONB scope.instrumentationScopeAttributes
    ]
instrumentationScopeToJSONB Nothing = AE.Null


-- Convert ExportLogsServiceRequest to [Log]
convertToLog :: V.Vector (Text, Projects.ProjectId, Integer) -> ResourceLogs -> V.Vector Telemetry.OtelLogsAndSpans
convertToLog pids resourceLogs =
  let keyy = fromMaybe "" $ listToMaybe $ V.toList $ getLogAttributeValue "at-project-key" [resourceLogs]
      pid = case find (\(k, _, count) -> k == keyy) pids of
        Just (_, v, count) -> if count >= 10000 then Nothing else Just v
        Nothing ->
          let pidText = fromMaybe "" $ listToMaybe $ V.toList $ getLogAttributeValue "at-project-id" [resourceLogs]
              uId = UUID.fromText pidText
           in ((Just . Projects.ProjectId) =<< uId)
   in case pid of
        Just p -> join $ V.map (convertScopeLog p resourceLogs.resourceLogsResource) resourceLogs.resourceLogsScopeLogs
        _ -> V.empty


-- | Convert a log record with scope information
convertLogToTimeFusionLog :: Projects.ProjectId -> Maybe Resource -> Maybe InstrumentationScope -> LogRecord -> Telemetry.OtelLogsAndSpans
convertLogToTimeFusionLog pid resource scope lr = do
  Telemetry.OtelLogsAndSpans
    { project_id = pid.toText
    , status_code = Nothing
    , status_message = Nothing
    , kind = Nothing
    , name = Nothing
    , duration = Nothing
    , context = Just $ makeLogContext lr
    , hashes = []
    , end_time = Nothing
    , start_time = nanosecondsToUTC lr.logRecordObservedTimeUnixNano
    , parent_id = Nothing
    , observed_timestamp = Just $ nanosecondsToUTC lr.logRecordObservedTimeUnixNano
    , id = UUID.nil
    , events = Nothing
    , links = Nothing
    , timestamp = if lr.logRecordTimeUnixNano == 0 then nanosecondsToUTC lr.logRecordObservedTimeUnixNano else nanosecondsToUTC lr.logRecordTimeUnixNano
    , level = Just $ toText lr.logRecordSeverityText
    , severity = Just $ Telemetry.Severity{severity_text = parseSeverityLevel $ toText lr.logRecordSeverityText, severity_number = fromIntegral $ (either id HsProtobuf.fromProtoEnum . HsProtobuf.enumerated) lr.logRecordSeverityNumber}
    , body = Just $ convertAnyValue lr.logRecordBody
    , attributes = jsonToMap $ removeProjectId $ keyValueToJSONB lr.logRecordAttributes
    , resource = jsonToMap $ removeProjectId $ resourceToJSONB resource
    , date = if lr.logRecordTimeUnixNano == 0 then nanosecondsToUTC lr.logRecordObservedTimeUnixNano else nanosecondsToUTC lr.logRecordTimeUnixNano
    }
  where
    makeLogContext :: LogRecord -> Telemetry.Context
    makeLogContext l =
      Telemetry.Context
        { trace_id = Just $ byteStringToHexText l.logRecordTraceId
        , span_id = Just $ byteStringToHexText l.logRecordSpanId
        , trace_state = Nothing
        , trace_flags = Nothing
        , is_remote = Nothing
        }


-- | Convert a scope log to internal representation
convertScopeLog :: Projects.ProjectId -> Maybe Resource -> ScopeLogs -> V.Vector Telemetry.OtelLogsAndSpans
convertScopeLog pid resource sl = V.map (convertLogToTimeFusionLog pid resource sl.scopeLogsScope) sl.scopeLogsLogRecords


-- | Clean up project metadata fields from JSON objects using lens combinators
removeProjectId :: AE.Value -> AE.Value
removeProjectId = \case
  AE.Object o -> AE.Object $ foldr KEM.delete o ["at-project-key", "at-project-id"]
  AE.Array arr -> AE.Array $ arr & mapped %~ removeProjectId
  v -> v


anyValueToString :: AnyValueValue -> Maybe Text
anyValueToString (AnyValueValueStringValue val) = Just $ toStrict val
anyValueToString _ = Nothing


-- | Process scope spans for span conversion
convertScopeSpan :: Projects.ProjectId -> Maybe Resource -> ScopeSpans -> V.Vector Telemetry.OtelLogsAndSpans
convertScopeSpan pid resource sl =
  V.map (otelSpansToTimeFusionSpans pid resource sl.scopeSpansScope) sl.scopeSpansSpans


-- | Convert resource spans to span records
convertToSpan :: V.Vector (Text, Projects.ProjectId, Integer) -> ResourceSpans -> V.Vector Telemetry.OtelLogsAndSpans
convertToSpan pids resourceSpans =
  let
    keyy = fromMaybe "" $ listToMaybe $ V.toList $ getSpanAttributeValue "at-project-key" [resourceSpans]
    pid = case find (\(k, _, count) -> k == keyy) pids of
      Just (_, v, count) -> if count >= 10000 then Nothing else Just v
      Nothing ->
        let pidText = fromMaybe "" $ listToMaybe $ V.toList $ getSpanAttributeValue "at-project-id" [resourceSpans]
            uId = UUID.fromText pidText
         in ((Just . Projects.ProjectId) =<< uId)
   in
    case pid of
      Just p -> join $ V.map (convertScopeSpan p resourceSpans.resourceSpansResource) resourceSpans.resourceSpansScopeSpans
      _ -> V.empty


-- | Convert span events to JSON
eventsToJSONB :: [Span_Event] -> AE.Value
eventsToJSONB spans =
  AE.toJSON $
    spans
      <&> \sp ->
        AE.object
          [ "event_name" AE..= toText sp.span_EventName
          , "event_time" AE..= nanosecondsToUTC sp.span_EventTimeUnixNano
          , "event_attributes" AE..= keyValueToJSONB sp.span_EventAttributes
          , "event_dropped_attributes_count" AE..= fromIntegral sp.span_EventDroppedAttributesCount
          ]


-- | Convert span links to JSON
linksToJSONB :: [Span_Link] -> AE.Value
linksToJSONB lnks =
  AE.toJSON $
    lnks
      <&> \lnk ->
        AE.object
          [ "link_span_id" AE..= (decodeUtf8 lnk.span_LinkSpanId :: Text)
          , "link_trace_id" AE..= (decodeUtf8 lnk.span_LinkTraceId :: Text)
          , "link_attributes" AE..= keyValueToJSONB lnk.span_LinkAttributes
          , "link_dropped_attributes_count" AE..= fromIntegral lnk.span_LinkDroppedAttributesCount
          , "link_flags" AE..= fromIntegral lnk.span_LinkFlags
          ]


-- | Convert span kind from protobuf to internal representation
parseSpanKind :: Either Int32 Span_SpanKind -> Maybe SpanKind
parseSpanKind = \case
  Left _ -> Nothing
  Right Span_SpanKindSPAN_KIND_INTERNAL -> Just SKInternal
  Right Span_SpanKindSPAN_KIND_SERVER -> Just SKServer
  Right Span_SpanKindSPAN_KIND_CLIENT -> Just SKClient
  Right Span_SpanKindSPAN_KIND_PRODUCER -> Just SKProducer
  Right Span_SpanKindSPAN_KIND_CONSUMER -> Just SKConsumer
  Right _ -> Just SKUnspecified


-- | Convert span status from protobuf to internal representation
parseSpanStatus :: Maybe Status -> Maybe SpanStatus
parseSpanStatus = \case
  Just est -> case HsProtobuf.enumerated est.statusCode of
    Left _ -> Nothing
    Right Status_StatusCodeSTATUS_CODE_OK -> Just SSOk
    Right Status_StatusCodeSTATUS_CODE_ERROR -> Just SSError
    Right Status_StatusCodeSTATUS_CODE_UNSET -> Just SSUnset
  Nothing -> Nothing


convertToMetric :: Projects.ProjectId -> ResourceMetrics -> V.Vector Telemetry.MetricRecord
convertToMetric pid resourceMetrics = join $ V.map (convertScopeMetric pid resourceMetrics.resourceMetricsResource) resourceMetrics.resourceMetricsScopeMetrics


convertScopeMetric :: Projects.ProjectId -> Maybe Resource -> ScopeMetrics -> V.Vector Telemetry.MetricRecord
convertScopeMetric pid resource sm = join $ V.map (convertMetricRecord pid resource sm.scopeMetricsScope) sm.scopeMetricsMetrics


convertMetricRecord :: Projects.ProjectId -> Maybe Resource -> Maybe InstrumentationScope -> Metric -> V.Vector Telemetry.MetricRecord
convertMetricRecord pid resource iscp metric =
  ( \(flags, exemplars, attributes, startTime, metricTime, metricValue, metricType) ->
      Telemetry.MetricRecord
        { projectId = pid.unProjectId
        , id = Nothing
        , metricName = toText metric.metricName
        , metricDescription = toText metric.metricDescription
        , metricUnit = toText metric.metricUnit
        , timestamp = nanosecondsToUTC startTime
        , metricTime = nanosecondsToUTC metricTime
        , resource = removeProjectId $ resourceToJSONB resource
        , instrumentationScope = instrumentationScopeToJSONB iscp
        , metricValue
        , exemplars
        , metricType
        , flags
        , attributes
        , metricMetadata = keyValueToJSONB metric.metricMetadata
        }
  )
    <$> valuess
  where
    valuess = case metric.metricData of
      Just metricData -> case metricData of
        MetricDataGauge (Gauge gauges) ->
          ( \gauge ->
              let attr = keyValueToJSONB gauge.numberDataPointAttributes
                  stTime = gauge.numberDataPointStartTimeUnixNano
                  mtTime = gauge.numberDataPointTimeUnixNano
                  mtValue = case gauge.numberDataPointValue of
                    Just (NumberDataPointValueAsDouble x) -> Telemetry.GaugeValue Telemetry.GaugeSum{value = x}
                    Just (NumberDataPointValueAsInt x) -> Telemetry.GaugeValue Telemetry.GaugeSum{value = fromIntegral x}
                    _ -> Telemetry.GaugeValue Telemetry.GaugeSum{value = 0}
                  mtType = Telemetry.MTGauge
                  exems = AE.toJSON gauge.numberDataPointExemplars
               in (fromIntegral gauge.numberDataPointFlags, exems, attr, stTime, mtTime, mtValue, mtType)
          )
            <$> gauges
        MetricDataSum s ->
          ( \sm ->
              let attr = keyValueToJSONB sm.numberDataPointAttributes
                  stTime = sm.numberDataPointStartTimeUnixNano
                  mtTime = sm.numberDataPointTimeUnixNano
                  mtValue = case sm.numberDataPointValue of
                    Just (NumberDataPointValueAsDouble x) -> Telemetry.SumValue Telemetry.GaugeSum{value = x}
                    Just (NumberDataPointValueAsInt x) -> Telemetry.SumValue Telemetry.GaugeSum{value = fromIntegral x}
                    _ -> Telemetry.SumValue Telemetry.GaugeSum{value = 0}
                  mtType = Telemetry.MTSum
                  exems = AE.toJSON sm.numberDataPointExemplars
               in (fromIntegral sm.numberDataPointFlags, exems, attr, stTime, mtTime, mtValue, mtType)
          )
            <$> s.sumDataPoints
        MetricDataHistogram histograms ->
          ( \histogram ->
              let attr = keyValueToJSONB histogram.histogramDataPointAttributes
                  stTime = histogram.histogramDataPointStartTimeUnixNano
                  mtTime = histogram.histogramDataPointTimeUnixNano
                  mtValue =
                    Telemetry.HistogramValue
                      Telemetry.Histogram
                        { count = fromIntegral histogram.histogramDataPointCount
                        , sum = histogram.histogramDataPointSum
                        , bucketCounts = fromIntegral <$> histogram.histogramDataPointBucketCounts
                        , explicitBounds = histogram.histogramDataPointExplicitBounds
                        , pointMin = histogram.histogramDataPointMin
                        , pointMax = histogram.histogramDataPointMax
                        }
                  mtType = Telemetry.MTHistogram
                  exems = AE.toJSON histogram.histogramDataPointExemplars
               in (fromIntegral histogram.histogramDataPointFlags, exems, attr, stTime, mtTime, mtValue, mtType)
          )
            <$> histograms.histogramDataPoints
        MetricDataExponentialHistogram histograms ->
          ( \histogram ->
              let attr = keyValueToJSONB histogram.exponentialHistogramDataPointAttributes
                  stTime = histogram.exponentialHistogramDataPointStartTimeUnixNano
                  mtTime = histogram.exponentialHistogramDataPointTimeUnixNano
                  pointNegative =
                    ( \b ->
                        Just $
                          Telemetry.EHBucket
                            { bucketOffset = fromIntegral $ b.exponentialHistogramDataPoint_BucketsOffset
                            , bucketCounts = fromIntegral <$> b.exponentialHistogramDataPoint_BucketsBucketCounts
                            }
                    )
                      =<< histogram.exponentialHistogramDataPointNegative
                  pointPositive =
                    ( \b ->
                        Just $
                          Telemetry.EHBucket
                            { bucketOffset = fromIntegral $ b.exponentialHistogramDataPoint_BucketsOffset
                            , bucketCounts = fromIntegral <$> b.exponentialHistogramDataPoint_BucketsBucketCounts
                            }
                    )
                      =<< histogram.exponentialHistogramDataPointPositive
                  mtValue =
                    Telemetry.ExponentialHistogramValue
                      Telemetry.ExponentialHistogram
                        { count = fromIntegral histogram.exponentialHistogramDataPointCount
                        , sum = histogram.exponentialHistogramDataPointSum
                        , pointMin = histogram.exponentialHistogramDataPointMin
                        , pointMax = histogram.exponentialHistogramDataPointMax
                        , zeroCount = fromIntegral histogram.exponentialHistogramDataPointZeroCount
                        , scale = fromIntegral histogram.exponentialHistogramDataPointScale
                        , pointNegative
                        , pointPositive
                        , zeroThreshold = histogram.exponentialHistogramDataPointZeroThreshold
                        }
                  mtType = Telemetry.MTHistogram
                  exems = AE.toJSON histogram.exponentialHistogramDataPointExemplars
               in (fromIntegral histogram.exponentialHistogramDataPointFlags, exems, attr, stTime, mtTime, mtValue, mtType)
          )
            <$> histograms.exponentialHistogramDataPoints
        MetricDataSummary summaries ->
          ( \summary ->
              let att = keyValueToJSONB summary.summaryDataPointAttributes
                  stTime = summary.summaryDataPointStartTimeUnixNano
                  mtTime = summary.summaryDataPointTimeUnixNano
                  mtValue =
                    Telemetry.SummaryValue
                      Telemetry.Summary
                        { sum = summary.summaryDataPointSum
                        , count = fromIntegral summary.summaryDataPointCount
                        , quantiles =
                            ( \x ->
                                Telemetry.Quantile
                                  { quantile = x.summaryDataPoint_ValueAtQuantileQuantile
                                  , value = x.summaryDataPoint_ValueAtQuantileValue
                                  }
                            )
                              <$> summary.summaryDataPointQuantileValues
                        }
                  mtType = Telemetry.MTSummary
                  exems = AE.Array []
               in (fromIntegral summary.summaryDataPointFlags, exems, att, stTime, mtTime, mtValue, mtType)
          )
            <$> summaries.summaryDataPoints
      Nothing -> []


-- | Convert a Span to OtelLogsAndSpans with appropriate metadata
otelSpansToTimeFusionSpans :: Projects.ProjectId -> Maybe Resource -> Maybe InstrumentationScope -> Span -> Telemetry.OtelLogsAndSpans
otelSpansToTimeFusionSpans pid res scope sp =
  Telemetry.OtelLogsAndSpans
    { observed_timestamp = utcTime
    , id = UUID.nil
    , parent_id =
        if BS.null sp.spanParentSpanId
          then Nothing
          else Just $ byteStringToHexText sp.spanParentSpanId
    , hashes = V.empty
    , name = Just $ toText sp.spanName
    , kind = getSpanKindText sp.spanKind
    , status_code = getStatusCodeText =<< sp.spanStatus
    , status_message = getStatusMessage =<< sp.spanStatus
    , level = Nothing
    , severity = Nothing
    , body = Nothing
    , duration = Just $ fromIntegral $ sp.spanEndTimeUnixNano - sp.spanStartTimeUnixNano
    , start_time = nanosecondsToUTC sp.spanStartTimeUnixNano
    , end_time = Just $ nanosecondsToUTC sp.spanEndTimeUnixNano
    , context = Just $ makeSpanContext sp
    , events = Just $ eventsToJSONB $ V.toList sp.spanEvents
    , links = Just $ T.pack $ show $ linksToJSONB $ V.toList sp.spanLinks
    , attributes = jsonToMap $ keyValueToJSONB sp.spanAttributes
    , resource = jsonToMap $ resourceToJSONB res
    , project_id = T.pack $ UUID.toString pid.unProjectId
    , timestamp = nanosecondsToUTC sp.spanStartTimeUnixNano
    , date = nanosecondsToUTC sp.spanStartTimeUnixNano
    }
  where
    utcTime = Just $ nanosecondsToUTC sp.spanStartTimeUnixNano

    -- Convert span to context
    makeSpanContext s =
      Telemetry.Context
        { trace_id = Just $ byteStringToHexText s.spanTraceId
        , span_id = Just $ byteStringToHexText s.spanSpanId
        , trace_state = Just $ toText s.spanTraceState
        , trace_flags = Nothing
        , is_remote = Nothing
        }

    getStatusMessage st = Just $ toText st.statusMessage

    getSpanKindText kind = case HsProtobuf.enumerated kind of
      Right Span_SpanKindSPAN_KIND_INTERNAL -> Just "internal"
      Right Span_SpanKindSPAN_KIND_SERVER -> Just "server"
      Right Span_SpanKindSPAN_KIND_CLIENT -> Just "client"
      Right Span_SpanKindSPAN_KIND_PRODUCER -> Just "producer"
      Right Span_SpanKindSPAN_KIND_CONSUMER -> Just "consumer"
      _ -> Just "unspecified"

    -- Convert status code to text
    getStatusCodeText st = case HsProtobuf.enumerated st.statusCode of
      Right Status_StatusCodeSTATUS_CODE_OK -> Just "OK"
      Right Status_StatusCodeSTATUS_CODE_ERROR -> Just "ERROR"
      _ -> Just "UNSET"


-- Convert JSON to map
jsonToMap :: AE.Value -> Maybe (Map Text AE.Value)
jsonToMap (AE.Object o) = Just $ KEM.toMapText o
jsonToMap _ = Nothing


traceServiceExportH
  :: Log.Logger
  -> AuthContext
  -> ServerRequest 'Normal ExportTraceServiceRequest ExportTraceServiceResponse
  -> IO (ServerResponse 'Normal ExportTraceServiceResponse)
traceServiceExportH appLogger appCtx (ServerNormalRequest _meta (ExportTraceServiceRequest req)) = do
  _ <- runBackground appLogger appCtx do
    pids <- dbtToEff $ ProjectApiKeys.projectIdsByProjectApiKeys $ getSpanAttributeValue "at-project-key" req
    let spanRecords = join $ V.map (convertToSpan pids) req
        apitoolkitSpans = V.map mapHTTPSpan spanRecords
    _ <- ProcessMessage.processRequestMessages $ V.toList $ V.catMaybes apitoolkitSpans <&> ("",)
    -- Telemetry.bulkInsertSpans $ V.filter (\s -> s.spanName /= "apitoolkit-http-span") spanRecords
    Telemetry.bulkInsertOtelLogsAndSpansTF spanRecords
  return (ServerNormalResponse (ExportTraceServiceResponse Nothing) mempty StatusOk "")


metricsServiceExportH
  :: Log.Logger
  -> AuthContext
  -> ServerRequest 'Normal ExportMetricsServiceRequest ExportMetricsServiceResponse
  -> IO (ServerResponse 'Normal ExportMetricsServiceResponse)
metricsServiceExportH appLogger appCtx (ServerNormalRequest _meta (ExportMetricsServiceRequest req)) = do
  _ <- runBackground appLogger appCtx do
    let projectKey = fromMaybe (error "Missing project key") $ getMetricAttributeValue "at-project-key" req
    projectIdM <- ProjectApiKeys.getProjectIdByApiKey projectKey
    let pid = fromMaybe (error $ "project API Key is invalid pid") projectIdM
    let metricRecords = join $ V.map (convertToMetric pid) req
    Telemetry.bulkInsertMetrics metricRecords
  return (ServerNormalResponse (ExportMetricsServiceResponse Nothing) mempty StatusOk "")


mapHTTPSpan :: Telemetry.OtelLogsAndSpans -> Maybe RequestMessage
mapHTTPSpan s =
  if s.name == Just "apitoolkit-http-span"
    then convertSpanToRequestMessage s "apitoolkit-http-span"
    else Nothing


---------------------------------------------------------------------------------------
-- Server
---------------------------------------------------------------------------------------

runServer :: Log.Logger -> AuthContext -> IO ()
runServer appLogger appCtx = do
  let opts =
        defaultServiceOptions
          { serverHost = Host "localhost"
          , serverPort = Port 4317
          }
  otlpServer opts appLogger appCtx


otlpServer :: HsGRPC.ServiceOptions -> Log.Logger -> AuthContext -> IO ()
otlpServer opts appLogger appCtx =
  HsGRPC.serverLoop
    HsGRPC.defaultOptions
      { HsGRPC.optNormalHandlers =
          [ HsGRPC.UnaryHandler
              (HsGRPC.MethodName "/opentelemetry.proto.collector.logs.v1.LogsService/Export")
              (HsGRPC.convertGeneratedServerHandler $ logsServiceExportH appLogger appCtx)
          , HsGRPC.UnaryHandler
              (HsGRPC.MethodName "/opentelemetry.proto.collector.trace.v1.TraceService/Export")
              (HsGRPC.convertGeneratedServerHandler $ traceServiceExportH appLogger appCtx)
          , HsGRPC.UnaryHandler
              (HsGRPC.MethodName "/opentelemetry.proto.collector.metrics.v1.MetricsService/Export")
              (HsGRPC.convertGeneratedServerHandler $ metricsServiceExportH appLogger appCtx)
          ]
      , HsGRPC.optClientStreamHandlers = []
      , HsGRPC.optServerStreamHandlers = []
      , HsGRPC.optBiDiStreamHandlers = []
      , optServerHost = opts.serverHost
      , optServerPort = opts.serverPort
      , optUseCompression = opts.useCompression
      , optUserAgentPrefix = opts.userAgentPrefix
      , optUserAgentSuffix = opts.userAgentSuffix
      , optInitialMetadata = opts.initialMetadata
      , optSSLConfig = opts.sslConfig
      , optLogger = opts.logger
      , optMaxReceiveMessageLength = opts.serverMaxReceiveMessageLength
      , optMaxMetadataSize = opts.serverMaxMetadataSize
      }
