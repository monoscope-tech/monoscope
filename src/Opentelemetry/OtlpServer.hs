{-# LANGUAGE OverloadedRecordDot #-}

module Opentelemetry.OtlpServer (runServer) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.ByteString qualified as BS
import Data.Scientific
import Data.Text.Lazy qualified as LT
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Debug.Pretty.Simple
import Log qualified
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.GRPC.HighLevel.Client as HsGRPC
import Network.GRPC.HighLevel.Generated as HsGRPC
import Network.GRPC.HighLevel.Server as HsGRPC hiding (serverLoop)
import Network.GRPC.HighLevel.Server.Unregistered as HsGRPC (serverLoop)
import Opentelemetry.Proto.Collector.Logs.V1.LogsService
import Opentelemetry.Proto.Collector.Trace.V1.TraceService
import Opentelemetry.Proto.Common.V1.Common
import Opentelemetry.Proto.Logs.V1.Logs
import Opentelemetry.Proto.Resource.V1.Resource
import Proto3.Suite.Class qualified as HsProtobuf
import Proto3.Suite.Types qualified as HsProtobuf
import Proto3.Wire qualified as HsProtobuf
import Relude
import System.Config (AuthContext)
import System.Types (runBackground)


logsServiceExportH
  :: Log.Logger
  -> AuthContext
  -> ServerRequest 'Normal ExportLogsServiceRequest ExportLogsServiceResponse
  -> IO (ServerResponse 'Normal ExportLogsServiceResponse)
logsServiceExportH appLogger appCtx (ServerNormalRequest _meta (ExportLogsServiceRequest req)) = do
  pTraceShowM "Hello called"
  pTraceShowM req
  pTraceShowM _meta
  let pid = UUID.nil
  let logRecords = join $ V.map (convertToLog pid) req
  _ <- runBackground appLogger appCtx $ Telemetry.bulkInsertLogs logRecords
  return (ServerNormalResponse (ExportLogsServiceResponse Nothing) mempty StatusOk "")


-- Convert nanoseconds to UTCTime
nanosecondsToUTC :: Word64 -> UTCTime
nanosecondsToUTC ns = posixSecondsToUTCTime (fromIntegral ns / 1e9)


-- Convert a list of KeyValue to a JSONB object
keyValueToJSONB :: V.Vector KeyValue -> AE.Value
keyValueToJSONB kvs =
  AE.object
    $ V.foldr (\kv acc -> (AEK.fromText $ LT.toStrict kv.keyValueKey, convertAnyValue (kv.keyValueValue)) : acc) [] kvs


convertAnyValue :: Maybe AnyValue -> AE.Value
convertAnyValue (Just (AnyValue (Just (AnyValueValueStringValue val)))) = AE.String $ toText val
convertAnyValue (Just (AnyValue (Just (AnyValueValueBoolValue val)))) = AE.Bool val
convertAnyValue (Just (AnyValue (Just (AnyValueValueIntValue val)))) = AE.Number (fromIntegral val)
convertAnyValue (Just (AnyValue (Just (AnyValueValueDoubleValue val)))) = AE.Number (fromFloatDigits val)
convertAnyValue (Just (AnyValue (Just (AnyValueValueArrayValue vals)))) = AE.Array ((V.map (convertAnyValue . Just) vals.arrayValueValues))
convertAnyValue _ = AE.Null


-- Convert Protobuf severity text to SeverityLevel
parseSeverityLevel :: Text -> Maybe Telemetry.SeverityLevel
parseSeverityLevel = \case
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
  object
    [ "name" .= scope.instrumentationScopeName
    , "version" .= scope.instrumentationScopeVersion
    , "attributes" .= keyValueToJSONB scope.instrumentationScopeAttributes
    ]
instrumentationScopeToJSONB Nothing = AE.Null


-- Convert ExportLogsServiceRequest to [Log]
convertToLog :: UUID.UUID -> ResourceLogs -> V.Vector Telemetry.LogRecord
convertToLog projectId resourceLogs = join $ V.map (convertScopeLog projectId resourceLogs.resourceLogsResource) resourceLogs.resourceLogsScopeLogs


convertScopeLog :: UUID.UUID -> Maybe Resource -> ScopeLogs -> V.Vector Telemetry.LogRecord
convertScopeLog projectId resource sl =
  V.map (convertLogRecord projectId resource sl.scopeLogsScope) sl.scopeLogsLogRecords


convertLogRecord :: UUID.UUID -> Maybe Resource -> Maybe InstrumentationScope -> LogRecord -> Telemetry.LogRecord
convertLogRecord projectId resource scope lr =
  Telemetry.LogRecord
    { projectId = projectId
    , id = Nothing
    , timestamp = nanosecondsToUTC lr.logRecordTimeUnixNano
    , observedTimestamp = nanosecondsToUTC lr.logRecordObservedTimeUnixNano
    , traceId = lr.logRecordTraceId
    , spanId = if BS.null lr.logRecordSpanId then Nothing else Just lr.logRecordSpanId
    , severityText = parseSeverityLevel $ toText lr.logRecordSeverityText
    , severityNumber = fromIntegral $ (either id HsProtobuf.fromProtoEnum . HsProtobuf.enumerated) lr.logRecordSeverityNumber
    , body = convertAnyValue (lr.logRecordBody)
    , attributes = keyValueToJSONB lr.logRecordAttributes
    , resource = resourceToJSONB resource
    , instrumentationScope = instrumentationScopeToJSONB scope
    }


traceServiceExportH
  :: Log.Logger
  -> AuthContext
  -> ServerRequest 'Normal ExportTraceServiceRequest ExportTraceServiceResponse
  -> IO (ServerResponse 'Normal ExportTraceServiceResponse)
traceServiceExportH appLogger appCtx (ServerNormalRequest _meta (ExportTraceServiceRequest req)) = do
  pTraceShowM "Hello trace called"
  pTraceShowM req
  pTraceShowM _meta
  return (ServerNormalResponse (ExportTraceServiceResponse Nothing) mempty StatusOk "")


-- -- Convert nanoseconds to UTCTime
-- nanosecondsToUTC :: Int64 -> UTCTime
-- nanosecondsToUTC ns = posixSecondsToUTCTime (fromIntegral ns / 1e9)

-- -- Convert a list of KeyValue to a JSONB object
-- keyValueToJSONB :: [Logs.KeyValue] -> Value
-- keyValueToJSONB kvs = object [(unpack kv.key, kv.value) | kv <- kvs]

-- -- Convert Protobuf kind to SpanKind
-- parseSpanKind :: Maybe Text -> Maybe SpanKind
-- parseSpanKind = fmap $ \case
--   "INTERNAL" -> INTERNAL
--   "SERVER"   -> SERVER
--   "CLIENT"   -> CLIENT
--   "PRODUCER" -> PRODUCER
--   "CONSUMER" -> CONSUMER
--   _          -> INTERNAL

-- -- Convert Protobuf status to SpanStatus
-- parseSpanStatus :: Maybe Text -> Maybe SpanStatus
-- parseSpanStatus = fmap $ \case
--   "OK"    -> OK
--   "ERROR" -> ERROR
--   "UNSET" -> UNSET
--   _       -> UNSET

-- -- Convert ExportLogsServiceRequest to [Log]
-- convertToLog :: UUID.UUID -> Logs.ExportLogsServiceRequest -> [UUID] -> [Log]
-- convertToLog projectId req logIds = concat $ zipWith (convertResourceLog projectId) req.resourceLogs logIds

-- convertResourceLog :: UUID.UUID -> Logs.ResourceLog -> UUID -> [Log]
-- convertResourceLog projectId rl logId =
--   concatMap (convertScopeLog projectId rl.resource logId) rl.scopeLogs

-- convertScopeLog :: UUID.UUID -> Logs.Resource -> UUID -> Logs.ScopeLog -> [Log]
-- convertScopeLog projectId resource logId sl =
--   map (convertLogRecord projectId resource sl.scope logId) sl.logRecords

-- convertLogRecord :: UUID.UUID -> Logs.Resource -> Logs.InstrumentationScope -> UUID -> Logs.LogRecord -> Log
-- convertLogRecord projectId resource scope logId lr =
--   Log
--     { projectId = projectId
--     , logId = logId
--     , timestamp = nanosecondsToUTC lr.timeUnixNano
--     , observedTimestamp = nanosecondsToUTC lr.observedTimeUnixNano
--     , traceId = lr.traceId
--     , spanId = if null lr.spanId then Nothing else Just lr.spanId
--     , severityText = parseSeverityLevel lr.severityText
--     , severityNumber = lr.severityNumber
--     , body = lr.body
--     , attributes = keyValueToJSONB lr.attributes
--     , resource = resourceToJSONB resource
--     , instrumentationScope = instrumentationScopeToJSONB scope
--     }

-- -- Convert ExportTraceServiceRequest to [Span]
-- convertToSpan :: UUID.UUID -> Traces.ExportTraceServiceRequest -> [Span]
-- convertToSpan projectId req = concatMap (convertResourceSpan projectId) req.resourceSpans

-- convertResourceSpan :: UUID.UUID -> Traces.ResourceSpan -> [Span]
-- convertResourceSpan projectId rs = concatMap (convertScopeSpan projectId rs.resource) rs.scopeSpans

-- convertScopeSpan :: UUID.UUID -> Traces.Resource -> Traces.ScopeSpan -> [Span]
-- convertScopeSpan projectId resource ss = map (convertSpanData projectId resource ss.scope) ss.spans

-- convertSpanData :: UUID.UUID -> Traces.Resource -> Traces.InstrumentationScope -> Traces.Span -> Span
-- convertSpanData projectId resource scope sd =
--   let startTime = nanosecondsToUTC sd.startTimeUnixNano
--       endTime = if sd.endTimeUnixNano == 0 then Nothing else Just $ nanosecondsToUTC sd.endTimeUnixNano
--   in Span
--     { projectId = projectId
--     , timestamp = startTime
--     , traceId = sd.traceId
--     , spanId = sd.spanId
--     , parentSpanId = if null sd.parentSpanId then Nothing else Just sd.parentSpanId
--     , traceState = Nothing
--     , spanName = sd.name
--     , startTime = startTime
--     , endTime = endTime
--     , kind = parseSpanKind sd.kind
--     , status = parseSpanStatus sd.status
--     , statusMessage = sd.statusMessage
--     , attributes = keyValueToJSONB sd.attributes
--     , events = eventsToJSONB sd.events
--     , links = linksToJSONB sd.links
--     , resource = resourceToJSONB resource
--     , instrumentationScope = instrumentationScopeToJSONB scope
--     }

-- -- Helper functions for JSONB conversion
-- resourceToJSONB :: Traces.Resource -> Value
-- resourceToJSONB _ = object [] -- Replace with actual implementation if necessary

-- instrumentationScopeToJSONB :: Traces.InstrumentationScope -> Value
-- instrumentationScopeToJSONB _ = object [] -- Replace with actual implementation if necessary

-- eventsToJSONB :: [Traces.Event] -> Value
-- eventsToJSONB _ = object [] -- Replace with actual implementation if necessary

-- linksToJSONB :: [Traces.Link] -> Value
-- linksToJSONB _ = object [] -- Replace with actual implementation if necessary

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
