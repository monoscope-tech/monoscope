{-# LANGUAGE PackageImports #-}

module Opentelemetry.OtlpMockValues (
  createOtelLogAtTime,
  createOtelSpanAtTime,
  createOtelTraceAtTime,
  createGaugeMetricAtTime,
) where

import Control.Lens ((.~))
import Data.ByteString.Base16 qualified as B16
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Encoding (encodeMessage)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService qualified as LS
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields qualified as LSF
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as MS
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as MSF
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as TS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as TSF
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as PCF
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as PLF
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as PMF
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as PR
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields qualified as PRF
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as PT
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as PTF
import Relude


-- | Create resource with API key and service name
mkResource :: Text -> PR.Resource
mkResource apiKey =
  defMessage
    & PRF.attributes
    .~ [ defMessage & PCF.key .~ "service.name" & PCF.value .~ (defMessage & PCF.stringValue .~ "test-service")
       , defMessage & PCF.key .~ "at-project-key" & PCF.value .~ (defMessage & PCF.stringValue .~ apiKey)
       ]


-- | Create OTLP log message at a specific time
createOtelLogAtTime :: Text -> Text -> UTCTime -> IO ByteString
createOtelLogAtTime apiKey bodyText timestamp = do
  let timestampNanos = round (utcTimeToPOSIXSeconds timestamp * 1e9) :: Word64
      logRecord = defMessage & PLF.timeUnixNano .~ timestampNanos & PLF.body .~ (defMessage & PCF.stringValue .~ bodyText) & PLF.severityText .~ "INFO"
      scopeLog = defMessage & PLF.logRecords .~ [logRecord]
      request = defMessage @LS.ExportLogsServiceRequest & LSF.resourceLogs .~ [defMessage & PLF.resource .~ mkResource apiKey & PLF.scopeLogs .~ [scopeLog]]
  pure $ encodeMessage request


-- | Create OTLP trace/span message at a specific time
createOtelSpanAtTime :: Text -> Text -> Text -> Maybe Text -> Text -> UTCTime -> IO ByteString
createOtelSpanAtTime apiKey trId spanId parentSpanIdM spanName timestamp = do
  let startTime = round (utcTimeToPOSIXSeconds timestamp * 1e9) :: Word64
      endTime = startTime + 100000000 -- 100ms duration
      -- Convert hex IDs to ByteString
      traceIdBS = fromRight "" $ B16.decode $ encodeUtf8 $ T.replicate (32 - T.length trId) "0" <> trId
      spanIdBS = fromRight "" $ B16.decode $ encodeUtf8 $ T.replicate (16 - T.length spanId) "0" <> spanId
      parentSpanIdBS = maybe "" (\pid -> fromRight "" $ B16.decode $ encodeUtf8 $ T.replicate (16 - T.length pid) "0" <> pid) parentSpanIdM
      otelSpan =
        defMessage
          & PTF.traceId
          .~ traceIdBS
            & PTF.spanId
          .~ spanIdBS
            & PTF.parentSpanId
          .~ parentSpanIdBS
            & PTF.name
          .~ spanName
            & PTF.kind
          .~ PT.Span'SPAN_KIND_SERVER
            & PTF.startTimeUnixNano
          .~ startTime
            & PTF.endTimeUnixNano
          .~ endTime
            & PTF.attributes
          .~ [defMessage & PCF.key .~ "http.method" & PCF.value .~ (defMessage & PCF.stringValue .~ "GET")]
      scopeSpan = defMessage & PTF.spans .~ [otelSpan]
      request = defMessage @TS.ExportTraceServiceRequest & TSF.resourceSpans .~ [defMessage & PTF.resource .~ mkResource apiKey & PTF.scopeSpans .~ [scopeSpan]]
  pure $ encodeMessage request


-- | Create simple trace (single span) at a specific time
createOtelTraceAtTime :: Text -> Text -> UTCTime -> IO ByteString
createOtelTraceAtTime apiKey spanName timestamp = do
  trIdText <- UUID.toText <$> nextRandom
  spanIdText <- UUID.toText <$> nextRandom
  createOtelSpanAtTime apiKey trIdText spanIdText Nothing spanName timestamp


-- | Create gauge metric at a specific time
createGaugeMetricAtTime :: Text -> Text -> Double -> UTCTime -> IO ByteString
createGaugeMetricAtTime apiKey metricName value timestamp = do
  let timestampNanos = round (utcTimeToPOSIXSeconds timestamp * 1e9) :: Word64
      dataPoint = defMessage & PMF.timeUnixNano .~ timestampNanos & PMF.asDouble .~ value
      gauge = defMessage & PMF.dataPoints .~ [dataPoint]
      metric = defMessage & PMF.name .~ metricName & PMF.description .~ ("Test gauge metric: " <> metricName) & PMF.unit .~ "1" & PMF.gauge .~ gauge
      scopeMetric = defMessage & PMF.metrics .~ [metric]
      request = defMessage @MS.ExportMetricsServiceRequest & MSF.resourceMetrics .~ [defMessage & PMF.resource .~ mkResource apiKey & PMF.scopeMetrics .~ [scopeMetric]]
  pure $ encodeMessage request
