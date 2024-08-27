{-# LANGUAGE OverloadedRecordDot #-}

module Opentelemetry.OtlpServer (runServer, processList) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as KEM
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as M
import Data.Scientific
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Reader.Static (ask)
import Effectful.Reader.Static qualified as Eff
import Log qualified
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Telemetry.Telemetry (SpanKind (..), SpanStatus (..))
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
    Just "org.opentelemetry.otlp.traces.v1" -> do
      let results =
            msgs' & V.map \(ackId, msg) -> do
              let res = HsProtobuf.fromByteString msg :: Either HsProtobuf.ParseError ExportTraceServiceRequest
              case res of
                Left err -> error $ "unable to parse traces service request with err " <> show err
                Right (ExportTraceServiceRequest a) -> (ackId, join $ V.map convertToSpan a)
      let (ackIds, spans) = V.unzip results
      Telemetry.bulkInsertSpans $ join spans
      pure $ V.toList ackIds
    Just "org.opentelemetry.otlp.metrics.v1" -> do
      let (ackIds, _) = V.unzip $ V.fromList msgs
      pure $ V.toList ackIds
    _ -> error "unsupported opentelemetry data type"


projectApiKeyFromB64 :: Text -> Text -> ProjectApiKeys.ProjectApiKeyId
projectApiKeyFromB64 apiKeyEncryptionSecretKey projectKey = case (B64.decodeBase64Untyped $ encodeUtf8 projectKey) of
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
  -- let projectKey = T.replace "Bearer " "" $ decodeUtf8 $ Unsafe.fromJust $ Safe.headMay =<< M.lookup "authorization" meta.metadata.unMap
  --     apiKeyUUID = projectApiKeyFromB64 appCtx.config.apiKeyEncryptionSecretKey projectKey
  _ <- runBackground appLogger appCtx do
    -- pApiKey <- dbtToEff $ ProjectApiKeys.getProjectApiKey apiKeyUUID
    -- No longer inserting the log using project id from api key. Maybe atleast assert the project id at this point?
    let logRecords = join $ V.map convertToLog req
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
  AE.object $
    V.foldr (\kv acc -> (AEK.fromText $ LT.toStrict kv.keyValueKey, convertAnyValue kv.keyValueValue) : acc) [] kvs


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
convertScopeLog resource sl = V.map (convertLogRecord resource sl.scopeLogsScope) sl.scopeLogsLogRecords


removeProjectId :: AE.Value -> AE.Value
removeProjectId (AE.Object v) = AE.Object $ KEM.delete "at-project-key" $ KEM.delete "at-project-id" v
removeProjectId (AE.Array v) = AE.Array $ V.map removeProjectId v
removeProjectId v = v


anyValueToString :: AnyValueValue -> Maybe Text
anyValueToString (AnyValueValueStringValue val) = Just $ toStrict val
anyValueToString _ = Nothing


convertScopeSpan :: Maybe Resource -> ScopeSpans -> V.Vector Telemetry.SpanRecord
convertScopeSpan resource sl =
  V.map (convertSpanRecord resource sl.scopeSpansScope) sl.scopeSpansSpans


-- Convert ExportTraceServiceRequest to [Trace]
convertToSpan :: ResourceSpans -> V.Vector Telemetry.SpanRecord
convertToSpan resourceSpans = join $ V.map (convertScopeSpan resourceSpans.resourceSpansResource) resourceSpans.resourceSpansScopeSpans


-- fromMaybe (error "invalid at-project-id in logs") do
--         let v = Unsafe.fromJust $ resource >>= \r -> find (\kv -> kv.keyValueKey == "at-project-id") r.resourceAttributes >>= (.keyValueValue) >>= (.anyValueValue)
--         UUID.fromText =<< anyValueToString v

convertLogRecord :: Maybe Resource -> Maybe InstrumentationScope -> LogRecord -> Telemetry.LogRecord
convertLogRecord resource scope lr =
  Telemetry.LogRecord
    { projectId = fromMaybe (error "invalid at-project-id in logs") do
        UUID.fromText =<< anyValueToString =<< pid'
    , id = Nothing
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
  where
    pid = resource >>= \r -> find (\kv -> kv.keyValueKey == "at-project-id") r.resourceAttributes >>= (.keyValueValue) >>= (.anyValueValue)
    pid' = if isJust pid then pid else find (\kv -> kv.keyValueKey == "at-project-id") lr.logRecordAttributes >>= (.keyValueValue) >>= (.anyValueValue)


convertSpanRecord :: Maybe Resource -> Maybe InstrumentationScope -> Span -> Telemetry.SpanRecord
convertSpanRecord resource scope sp =
  Telemetry.SpanRecord
    { projectId = fromMaybe (error "invalid at-project-id in span") do
        UUID.fromText =<< anyValueToString =<< pid'
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
    , spanDurationNs = 0
    }
  where
    pid = resource >>= \r -> find (\kv -> kv.keyValueKey == "at-project-id") r.resourceAttributes >>= (.keyValueValue) >>= (.anyValueValue)
    pid' = if isJust pid then pid else find (\kv -> kv.keyValueKey == "at-project-id") sp.spanAttributes >>= (.keyValueValue) >>= (.anyValueValue)


traceServiceExportH
  :: Log.Logger
  -> AuthContext
  -> ServerRequest 'Normal ExportTraceServiceRequest ExportTraceServiceResponse
  -> IO (ServerResponse 'Normal ExportTraceServiceResponse)
traceServiceExportH appLogger appCtx (ServerNormalRequest _meta (ExportTraceServiceRequest req)) = do
  _ <- runBackground appLogger appCtx do
    let spanRecords = join $ V.map convertToSpan req
    Telemetry.bulkInsertSpans spanRecords
  return (ServerNormalResponse (ExportTraceServiceResponse Nothing) mempty StatusOk "")


-- -- Convert nanoseconds to UTCTime
-- nanosecondsToUTC :: Int64 -> UTCTime
-- nanosecondsToUTC ns = posixSecondsToUTCTime (fromIntegral ns / 1e9)

-- -- Convert a list of KeyValue to a JSONB object
-- keyValueToJSONB :: [Logs.KeyValue] -> Value
-- keyValueToJSONB kvs = object [(unpack kv.key, kv.value) | kv <- kvs]

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


-- data Span_Event = Span_Event
--   { span_EventTimeUnixNano :: Hs.Word64
--   , span_EventName :: Hs.Text
--   , span_EventAttributes :: (Hs.Vector Opentelemetry.Proto.Common.V1.Common.KeyValue)
--   , span_EventDroppedAttributesCount :: Hs.Word32
--   }

eventsToJSONB :: [Span_Event] -> AE.Value
eventsToJSONB spans =
  AE.toJSON $
    ( \sp ->
        object
          [ "event_name" .= toText sp.span_EventName
          , "event_time" .= nanosecondsToUTC sp.span_EventTimeUnixNano
          , "event_attributes" .= keyValueToJSONB sp.span_EventAttributes
          , "event_dropped_attributes_count" .= fromIntegral sp.span_EventDroppedAttributesCount
          ]
    )
      <$> spans


linksToJSONB :: [Span_Link] -> AE.Value
linksToJSONB lnks =
  AE.toJSON $
    ( \lnk ->
        object
          [ "link_span_id" .= (decodeUtf8 lnk.span_LinkSpanId :: Text)
          , "link_trace_id" .= (decodeUtf8 lnk.span_LinkTraceId :: Text)
          , "link_attributes" .= keyValueToJSONB lnk.span_LinkAttributes
          , "link_dropped_attributes_count" .= fromIntegral lnk.span_LinkDroppedAttributesCount
          , "link_flags" .= fromIntegral lnk.span_LinkFlags
          ]
    )
      <$> lnks


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
