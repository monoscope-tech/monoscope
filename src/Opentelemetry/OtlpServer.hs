{-# LANGUAGE OverloadedRecordDot #-}

module Opentelemetry.OtlpServer (runServer, processList) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as C
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as M
import Data.Scientific
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Debug.Pretty.Simple
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Reader.Static (ask, asks)
import Effectful.Reader.Static qualified as Eff
import Log qualified
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
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
import Proto3.Wire.Decode qualified as HsProtobuf
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import Safe qualified
import System.Config
import System.Types (runBackground)


processList :: (Eff.Reader AuthContext :> es, DB :> es) => [(Text, ByteString)] -> HashMap Text Text -> Eff es [Text]
processList [] _ = pure []
processList msgs attrs = do
  let msgs' = V.fromList msgs
  appCtx <- ask @AuthContext
  case HashMap.lookup "ce-type" attrs of
    Just "org.opentelemetry.otlp.logs.v1" -> do
      let results =
            msgs' & V.map \(ackId, msg) -> do
              let resp = HsProtobuf.fromByteString msg :: Either HsProtobuf.ParseError ExportLogsServiceRequest
              case resp of
                Left err -> error $ "unable to parse logs service request with err " <> show err
                Right (ExportLogsServiceRequest a) -> (ackId, join $ V.map convertToLog a)
      let (ackIds, logs) = V.unzip results
      Telemetry.bulkInsertLogs $ join logs
      pure $ V.toList ackIds
    _ -> error "unsupported opentelemetry data type"


projectApiKeyFromB64 :: Text -> Text -> ProjectApiKeys.ProjectApiKeyId
projectApiKeyFromB64 apiKeyEncryptionSecretKey projectKey = do
  let authTextE = B64.decodeBase64Untyped $ encodeUtf8 $ projectKey
  case authTextE of
    Left err -> error err
    Right authText -> do
      let decryptedKey = ProjectApiKeys.decryptAPIKey (encodeUtf8 apiKeyEncryptionSecretKey) authText
      Unsafe.fromJust $ ProjectApiKeys.ProjectApiKeyId <$> UUID.fromASCIIBytes decryptedKey


logsServiceExportH
  :: Log.Logger
  -> AuthContext
  -> ServerRequest 'Normal ExportLogsServiceRequest ExportLogsServiceResponse
  -> IO (ServerResponse 'Normal ExportLogsServiceResponse)
logsServiceExportH appLogger appCtx (ServerNormalRequest meta (ExportLogsServiceRequest req)) = do
  let projectKey = T.replace "Bearer " "" $ decodeUtf8 $ Unsafe.fromJust $ Safe.headMay =<< M.lookup "authorization" meta.metadata.unMap
      apiKeyUUID = projectApiKeyFromB64 appCtx.config.apiKeyEncryptionSecretKey projectKey
  _ <- runBackground appLogger appCtx do
    pApiKey <- dbtToEff $ ProjectApiKeys.getProjectApiKey apiKeyUUID
    -- No longer inserting the log using project id from api key. Maybe atleast assert the project id at this point?
    let logRecords = join $ V.map convertToLog req
    Telemetry.bulkInsertLogs logRecords
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
  object
    [ "name" .= scope.instrumentationScopeName
    , "version" .= scope.instrumentationScopeVersion
    , "attributes" .= keyValueToJSONB scope.instrumentationScopeAttributes
    ]
instrumentationScopeToJSONB Nothing = AE.Null


-- Convert ExportLogsServiceRequest to [Log]
convertToLog :: ResourceLogs -> V.Vector Telemetry.LogRecord
convertToLog resourceLogs = join $ V.map (convertScopeLog resourceLogs.resourceLogsResource) resourceLogs.resourceLogsScopeLogs


convertScopeLog :: Maybe Resource -> ScopeLogs -> V.Vector Telemetry.LogRecord
convertScopeLog resource sl =
  V.map (convertLogRecord resource sl.scopeLogsScope) sl.scopeLogsLogRecords


anyValueToString :: AnyValueValue -> Maybe Text
anyValueToString (AnyValueValueStringValue val) = Just $ toStrict val
anyValueToString _ = Nothing


convertLogRecord :: Maybe Resource -> Maybe InstrumentationScope -> LogRecord -> Telemetry.LogRecord
convertLogRecord resource scope lr =
  Telemetry.LogRecord
    { projectId = fromMaybe (error "invalid at-project-id in logs") do
        let v = Unsafe.fromJust $ resource >>= \r -> find (\kv -> kv.keyValueKey == "at-project-id") r.resourceAttributes >>= (.keyValueValue) >>= (.anyValueValue)
        UUID.fromText =<< anyValueToString v
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
  -- pTraceShowM "Hello trace called"
  -- pTraceShowM req
  -- pTraceShowM _meta
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
