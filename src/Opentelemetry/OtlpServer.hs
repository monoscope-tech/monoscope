{-# LANGUAGE OverloadedRecordDot #-}

module Opentelemetry.OtlpServer (runServer, processList) where

import Control.Lens hiding ((.=))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as KEM
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.HashMap.Strict qualified as HashMap
import Data.Scientific
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Reader.Static (ask)
import Effectful.Reader.Static qualified as Eff
import Effectful.Time (Time)
import Log qualified
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


getSpanAttributeValue :: Text -> V.Vector ResourceSpans -> V.Vector Text
getSpanAttributeValue attribute rss = V.mapMaybe (\rs -> getResourceAttr rs <|> getSpanAttr rs) rss
  where
    getResourceAttr rs = rs.resourceSpansResource >>= getAttr . resourceAttributes
    getSpanAttr rs = rs.resourceSpansScopeSpans & listToMaybe . V.toList . V.mapMaybe (getAttr . spanAttributes) . V.concatMap scopeSpansSpans
    getAttr :: V.Vector KeyValue -> Maybe Text
    getAttr = V.find ((== attribute) . toText . keyValueKey) >=> keyValueValue >=> anyValueValue >=> anyValueToString >=> (Just . toText)


getLogAttributeValue :: Text -> V.Vector ResourceLogs -> Maybe Text
getLogAttributeValue attribute rls = listToMaybe $ V.toList $ V.mapMaybe (\rl -> getResourceAttr rl <|> getLogAttr rl) rls
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


processList :: (Eff.Reader AuthContext :> es, DB :> es, Log :> es, IOE :> es, Time :> es) => [(Text, ByteString)] -> HashMap Text Text -> Eff es [Text]
processList [] _ = pure []
processList msgs attrs = do
  let msgs' = V.fromList msgs
  appCtx <- ask @AuthContext
  case HashMap.lookup "ce-type" attrs of
    Just "org.opentelemetry.otlp.logs.v1" -> do
      results <-
        V.forM msgs' \(ackId, msg) -> case (HsProtobuf.fromByteString msg :: Either HsProtobuf.ParseError ExportLogsServiceRequest) of
          Left err -> error $ "unable to parse logs service request with err " <> show err
          Right (ExportLogsServiceRequest logReq) -> do
            pidM <- join <$> forM (getLogAttributeValue "at-project-key" logReq) ProjectApiKeys.getProjectIdByApiKey
            let pid2M = Projects.projectIdFromText =<< getLogAttributeValue "at-project-id" logReq
            let pid = fromMaybe (error "project API Key and project ID not available in trace") $ pidM <|> pid2M
            pure (ackId, join $ V.map (convertToLog pid) logReq)
      let (ackIds, logs) = V.unzip results
      Telemetry.bulkInsertLogs $ join logs
      pure $ V.toList ackIds
    Just "org.opentelemetry.otlp.traces.v1" -> do
      results <-
        V.forM msgs' \(ackId, msg) -> do
          case (HsProtobuf.fromByteString msg :: Either HsProtobuf.ParseError ExportTraceServiceRequest) of
            Left err -> do
              Log.logInfo_ $ "unable to parse traces service request with err " <> show err <> (decodeUtf8 msg)
              pure (ackId, [])
            Right (ExportTraceServiceRequest traceReq) -> do
              projectIdsAndKeys <- dbtToEff $ ProjectApiKeys.projectIdsByProjectApiKeys $ getSpanAttributeValue "at-project-key" traceReq
              pure (ackId, join $ V.map (convertToSpan projectIdsAndKeys) traceReq)
      let (ackIds, spansVec) = V.unzip results
          spans = join spansVec
          apitoolkitSpans = V.map mapHTTPSpan spans
      _ <- ProcessMessage.processRequestMessages $ V.toList $ V.catMaybes apitoolkitSpans <&> ("",)
      Telemetry.bulkInsertSpans spans
      pure $ V.toList ackIds
    Just "org.opentelemetry.otlp.metrics.v1" -> do
      results <-
        V.forM msgs' \(ackId, msg) -> do
          case (HsProtobuf.fromByteString msg :: Either HsProtobuf.ParseError ExportMetricsServiceRequest) of
            Left err -> error $ "unable to parse traces service request with err " <> show err
            Right (ExportMetricsServiceRequest metricReq) -> do
              pidM <- join <$> forM (getMetricAttributeValue "at-project-key" metricReq) ProjectApiKeys.getProjectIdByApiKey
              let pid2M = Projects.projectIdFromText =<< getMetricAttributeValue "at-project-id" metricReq
              let pid = fromMaybe (error $ "project API Key and project ID not available in trace") $ pidM <|> pid2M
              pure (ackId, join $ V.map (convertToMetric pid) metricReq)
      let (ackIds, metricVec) = V.unzip results
          metricRecords = join metricVec
      Telemetry.bulkInsertMetrics metricRecords
      pure $ V.toList ackIds
    _ -> error "unsupported opentelemetry data type"


logsServiceExportH
  :: Log.Logger
  -> AuthContext
  -> ServerRequest 'Normal ExportLogsServiceRequest ExportLogsServiceResponse
  -> IO (ServerResponse 'Normal ExportLogsServiceResponse)
logsServiceExportH appLogger appCtx (ServerNormalRequest meta (ExportLogsServiceRequest req)) = do
  -- let projectKey = T.replace "Bearer " "" $ decodeUtf8 $ Unsafe.fromJust $ Safe.headMay =<< M.lookup "authorization" meta.metadata.unMap
  --     apiKeyUUID = projectApiKeyFromB64 appCtx.config.apiKeyEncryptionSecretKey projectKey
  _ <- runBackground appLogger appCtx do
    let projectKey = fromMaybe (error "Missing project key") $ getLogAttributeValue "at-project-key" req
    projectIdM <- ProjectApiKeys.getProjectIdByApiKey projectKey
    let pid = fromMaybe (error "project API Key is invalid pid") projectIdM

    let logRecords = join $ V.map (convertToLog pid) req
    Telemetry.bulkInsertLogs logRecords
  return (ServerNormalResponse (ExportLogsServiceResponse Nothing) mempty StatusOk "")


-- Convert nanoseconds to UTCTime
nanosecondsToUTC :: Word64 -> UTCTime
nanosecondsToUTC ns = posixSecondsToUTCTime (fromIntegral ns / 1e9)


byteStringToHexText :: BS.ByteString -> T.Text
byteStringToHexText bs = decodeUtf8 (B16.encode bs)


-- Convert a list of KeyValue to a JSONB object
keyValueToJSONB :: V.Vector KeyValue -> AE.Value
keyValueToJSONB kvs =
  AE.object
    $ V.foldr (\kv acc -> (AEK.fromText $ toText kv.keyValueKey, convertAnyValue kv.keyValueValue) : acc) [] kvs


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
convertToLog :: Projects.ProjectId -> ResourceLogs -> V.Vector Telemetry.LogRecord
convertToLog pid resourceLogs = join $ V.map (convertScopeLog pid resourceLogs.resourceLogsResource) resourceLogs.resourceLogsScopeLogs


convertScopeLog :: Projects.ProjectId -> Maybe Resource -> ScopeLogs -> V.Vector Telemetry.LogRecord
convertScopeLog pid resource sl = V.map (convertLogRecord pid resource sl.scopeLogsScope) sl.scopeLogsLogRecords


removeProjectId :: AE.Value -> AE.Value
removeProjectId (AE.Object v) = AE.Object $ KEM.delete "at-project-key" $ KEM.delete "at-project-id" v
removeProjectId (AE.Array v) = AE.Array $ V.map removeProjectId v
removeProjectId v = v


anyValueToString :: AnyValueValue -> Maybe Text
anyValueToString (AnyValueValueStringValue val) = Just $ toStrict val
anyValueToString _ = Nothing


convertScopeSpan :: Projects.ProjectId -> Maybe Resource -> ScopeSpans -> V.Vector Telemetry.SpanRecord
convertScopeSpan pid resource sl =
  V.map (convertSpanRecord pid resource sl.scopeSpansScope) sl.scopeSpansSpans


convertToSpan :: V.Vector (Text, Projects.ProjectId) -> ResourceSpans -> V.Vector Telemetry.SpanRecord
convertToSpan pids resourceSpans =
  let
    key = fromMaybe "" $ listToMaybe $ V.toList $ getSpanAttributeValue "at-project-key" [resourceSpans]
    pid = case find (\(k, _) -> k == key) pids of
      Just (_, v) -> Just v
      Nothing ->
        let pidText = fromMaybe "" $ listToMaybe $ V.toList $ getSpanAttributeValue "at-project-id" [resourceSpans]
            uId = UUID.fromText pidText
         in ((Just . Projects.ProjectId) =<< uId)
   in
    case pid of
      Just p -> join $ V.map (convertScopeSpan p resourceSpans.resourceSpansResource) resourceSpans.resourceSpansScopeSpans
      _ -> V.empty


convertLogRecord :: Projects.ProjectId -> Maybe Resource -> Maybe InstrumentationScope -> LogRecord -> Telemetry.LogRecord
convertLogRecord pid resource scope lr =
  Telemetry.LogRecord
    { projectId = pid.unProjectId
    , id = UUID.nil
    , timestamp = if lr.logRecordTimeUnixNano == 0 then nanosecondsToUTC lr.logRecordObservedTimeUnixNano else nanosecondsToUTC lr.logRecordTimeUnixNano
    , observedTimestamp = nanosecondsToUTC lr.logRecordObservedTimeUnixNano
    , traceId = byteStringToHexText lr.logRecordTraceId
    , spanId = if BS.null lr.logRecordSpanId then Nothing else Just (byteStringToHexText lr.logRecordSpanId)
    , severityText = parseSeverityLevel $ toText lr.logRecordSeverityText
    , severityNumber = fromIntegral $ (either id HsProtobuf.fromProtoEnum . HsProtobuf.enumerated) lr.logRecordSeverityNumber
    , body = convertAnyValue lr.logRecordBody
    , attributes = removeProjectId $ keyValueToJSONB lr.logRecordAttributes
    , resource = removeProjectId $ resourceToJSONB resource
    , instrumentationScope = instrumentationScopeToJSONB scope
    }


convertSpanRecord :: Projects.ProjectId -> Maybe Resource -> Maybe InstrumentationScope -> Span -> Telemetry.SpanRecord
convertSpanRecord pid resource scope sp =
  Telemetry.SpanRecord
    { uSpanId = UUID.nil
    , projectId = pid.unProjectId
    , timestamp = nanosecondsToUTC sp.spanStartTimeUnixNano
    , traceId = byteStringToHexText sp.spanTraceId
    , spanId = byteStringToHexText sp.spanSpanId
    , parentSpanId = if BS.null sp.spanParentSpanId then Nothing else Just $ byteStringToHexText sp.spanParentSpanId
    , traceState = Just (toText sp.spanTraceState)
    , spanName = toText sp.spanName
    , startTime = nanosecondsToUTC sp.spanStartTimeUnixNano
    , endTime = Just $ nanosecondsToUTC sp.spanEndTimeUnixNano
    , kind = parseSpanKind (HsProtobuf.enumerated sp.spanKind)
    , status = parseSpanStatus sp.spanStatus
    , statusMessage = (\s -> Just $ toText s.statusMessage) =<< sp.spanStatus
    , attributes = removeProjectId $ keyValueToJSONB sp.spanAttributes
    , events = eventsToJSONB $ V.toList sp.spanEvents
    , links = linksToJSONB $ V.toList sp.spanLinks
    , resource = removeProjectId $ resourceToJSONB resource
    , instrumentationScope = instrumentationScopeToJSONB scope
    , spanDurationNs = fromIntegral $ sp.spanEndTimeUnixNano - sp.spanStartTimeUnixNano
    }


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
    <$> values
  where
    values = case metric.metricData of
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
                        Just
                          $ Telemetry.EHBucket
                            { bucketOffset = fromIntegral $ b.exponentialHistogramDataPoint_BucketsOffset
                            , bucketCounts = fromIntegral <$> b.exponentialHistogramDataPoint_BucketsBucketCounts
                            }
                    )
                      =<< histogram.exponentialHistogramDataPointNegative
                  pointPositive =
                    ( \b ->
                        Just
                          $ Telemetry.EHBucket
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


traceServiceExportH
  :: Log.Logger
  -> AuthContext
  -> ServerRequest 'Normal ExportTraceServiceRequest ExportTraceServiceResponse
  -> IO (ServerResponse 'Normal ExportTraceServiceResponse)
traceServiceExportH appLogger appCtx (ServerNormalRequest _meta (ExportTraceServiceRequest req)) = do
  _ <- runBackground appLogger appCtx do
    let projectKey = fromMaybe (error "Missing project key") $ listToMaybe $ V.toList $ getSpanAttributeValue "at-project-key" req
    projectIdM <- ProjectApiKeys.getProjectIdByApiKey projectKey
    let pid = fromMaybe (error "project API Key is invalid pid") projectIdM
    let spanRecords = join $ V.map (convertToSpan [(projectKey, pid)]) req
        apitoolkitSpans = V.map mapHTTPSpan spanRecords
    _ <- ProcessMessage.processRequestMessages $ V.toList $ V.catMaybes apitoolkitSpans <&> ("",)
    Telemetry.bulkInsertSpans $ V.filter (\s -> s.spanName /= "apitoolkit-http-span") spanRecords
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


mapHTTPSpan :: Telemetry.SpanRecord -> Maybe RequestMessage
mapHTTPSpan s = do
  case s.instrumentationScope of
    AE.Object v ->
      if apitoolkitSpan
        then Just $ convertSpanToRequestMessage s "apitoolkit-http-span"
        else
          if httpScope
            then Just $ convertSpanToRequestMessage s "@opentelemetry/instrumentation-undici"
            else Nothing
      where
        y = KEM.lookup "name" v
        httpScope = y == Just "@opentelemetry/instrumentation-undici"
        apitoolkitSpan = s.spanName == "apitoolkit-http-span"
    _ -> Nothing


-- Convert Protobuf kind to SpanKind
parseSpanKind :: Either Int32 Span_SpanKind -> Maybe SpanKind
parseSpanKind spKind = case spKind of
  Left _ -> Nothing
  Right sp -> case sp of
    Span_SpanKindSPAN_KIND_INTERNAL -> Just SKInternal
    Span_SpanKindSPAN_KIND_SERVER -> Just SKServer
    Span_SpanKindSPAN_KIND_CLIENT -> Just SKClient
    Span_SpanKindSPAN_KIND_PRODUCER -> Just SKProducer
    Span_SpanKindSPAN_KIND_CONSUMER -> Just SKConsumer
    _ -> Just SKUnspecified


parseSpanStatus :: Maybe Status -> Maybe SpanStatus
parseSpanStatus st = case st of
  Just est -> case HsProtobuf.enumerated est.statusCode of
    Left _ -> Nothing
    Right Status_StatusCodeSTATUS_CODE_OK -> Just SSOk
    Right Status_StatusCodeSTATUS_CODE_ERROR -> Just SSError
    Right Status_StatusCodeSTATUS_CODE_UNSET -> Just SSUnset
  _ -> Nothing


eventsToJSONB :: [Span_Event] -> AE.Value
eventsToJSONB spans =
  AE.toJSON
    $ ( \sp ->
          AE.object
            [ "event_name" AE..= toText sp.span_EventName
            , "event_time" AE..= nanosecondsToUTC sp.span_EventTimeUnixNano
            , "event_attributes" AE..= keyValueToJSONB sp.span_EventAttributes
            , "event_dropped_attributes_count" AE..= fromIntegral sp.span_EventDroppedAttributesCount
            ]
      )
    <$> spans


linksToJSONB :: [Span_Link] -> AE.Value
linksToJSONB lnks =
  AE.toJSON
    $ lnks
    <&> \lnk ->
      AE.object
        [ "link_span_id" AE..= (decodeUtf8 lnk.span_LinkSpanId :: Text)
        , "link_trace_id" AE..= (decodeUtf8 lnk.span_LinkTraceId :: Text)
        , "link_attributes" AE..= keyValueToJSONB lnk.span_LinkAttributes
        , "link_dropped_attributes_count" AE..= fromIntegral lnk.span_LinkDroppedAttributesCount
        , "link_flags" AE..= fromIntegral lnk.span_LinkFlags
        ]


---------------------------------------------------------------------------------------
-- Server
---------------------------------------------------------------------------------------

runServer :: Log.Logger -> AuthContext -> IO ()
runServer appLogger appCtx = do
  let opts =
        defaultServiceOptions
          { serverHost = Host "localhost"
          , serverPort = Port 14317
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
