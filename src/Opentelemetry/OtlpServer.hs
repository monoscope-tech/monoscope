{-# LANGUAGE OverloadedRecordDot #-}

module Opentelemetry.OtlpServer (runServer, processList) where

import Control.Lens hiding ((.=))
import Control.Monad.Extra (fromMaybeM)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as KEM
import Data.Base64.Types qualified as B64
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.HashMap.Strict qualified as HashMap
import Data.Scientific
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (TimeZone (..), UTCTime, utcToZonedTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful
import Effectful.Error.Static (throwError)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (ask)
import Effectful.Reader.Static qualified as Eff
import Effectful.Time (Time)
import Log qualified
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
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
import ProcessMessage qualified
import Proto3.Suite.Class qualified as HsProtobuf
import Proto3.Suite.Types qualified as HsProtobuf
import Proto3.Wire qualified as HsProtobuf
import Proto3.Wire.Decode qualified as HsProtobuf
import Relude hiding (ask)
import RequestMessages (RequestMessage (..))
import System.Config
import System.Types (runBackground)


getSpanAttributeValue :: Text -> V.Vector ResourceSpans -> Maybe Text
getSpanAttributeValue attribute rss = listToMaybe $ V.toList $ V.mapMaybe (\rs -> getResourceAttr rs <|> getSpanAttr rs) rss
  where
    getResourceAttr rs = rs.resourceSpansResource >>= getAttr . resourceAttributes
    getSpanAttr rs = rs.resourceSpansScopeSpans & listToMaybe . V.toList . V.mapMaybe (getAttr . spanAttributes) . V.concatMap (scopeSpansSpans)

    getAttr :: V.Vector KeyValue -> Maybe Text
    getAttr = V.find ((== attribute) . toText . keyValueKey) >=> keyValueValue >=> anyValueValue >=> anyValueToString >=> (Just . toText)


getLogAttributeValue :: Text -> V.Vector ResourceLogs -> Maybe Text
getLogAttributeValue attribute rls = listToMaybe $ V.toList $ V.mapMaybe (\rl -> getResourceAttr rl <|> getLogAttr rl) rls
  where
    -- Extract attribute from resource-level logs
    getResourceAttr rl = rl.resourceLogsResource >>= getAttr . resourceAttributes

    -- Extract attribute from log records
    getLogAttr rl = rl.resourceLogsScopeLogs & listToMaybe . V.toList . V.mapMaybe (getAttr . logRecordAttributes) . V.concatMap (scopeLogsLogRecords)

    -- Helper function to extract attribute values from KeyValue pairs
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
            let pid = fromMaybe (error $ "project API Key and project ID not available in trace") $ pidM <|> pid2M
            pure (ackId, join $ V.map (convertToLog pid) logReq)
      let (ackIds, logs) = V.unzip results
      Telemetry.bulkInsertLogs $ join logs
      pure $ V.toList ackIds
    Just "org.opentelemetry.otlp.traces.v1" -> do
      results <-
        V.forM msgs' \(ackId, msg) -> do
          case (HsProtobuf.fromByteString msg :: Either HsProtobuf.ParseError ExportTraceServiceRequest) of
            Left err -> error $ "unable to parse traces service request with err " <> show err
            Right (ExportTraceServiceRequest traceReq) -> do
              pidM <- join <$> forM (getSpanAttributeValue "at-project-key" traceReq) ProjectApiKeys.getProjectIdByApiKey
              let pid2M = Projects.projectIdFromText =<< getSpanAttributeValue "at-project-id" traceReq
              let pid = fromMaybe (error $ "project API Key and project ID not available in trace") $ pidM <|> pid2M
              pure (ackId, join $ V.map (convertToSpan pid) traceReq)
      let (ackIds, spansVec) = V.unzip results
          spans = join spansVec
          apitoolkitSpans = V.map mapHTTPSpan spans
      _ <- ProcessMessage.processRequestMessages $ V.toList $ V.catMaybes apitoolkitSpans <&> ("",)
      Telemetry.bulkInsertSpans $ V.filter (\s -> s.spanName /= "apitoolkit-http-span") spans
      pure $ V.toList ackIds
    Just "org.opentelemetry.otlp.metrics.v1" -> do
      let (ackIds, _) = V.unzip $ V.fromList msgs
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
    let pid = fromMaybe (error $ "project API Key is invalid pid") projectIdM

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


convertToSpan :: Projects.ProjectId -> ResourceSpans -> V.Vector Telemetry.SpanRecord
convertToSpan pid resourceSpans = join $ V.map (convertScopeSpan pid resourceSpans.resourceSpansResource) resourceSpans.resourceSpansScopeSpans


convertLogRecord :: Projects.ProjectId -> Maybe Resource -> Maybe InstrumentationScope -> LogRecord -> Telemetry.LogRecord
convertLogRecord pid resource scope lr =
  Telemetry.LogRecord
    { projectId = pid.unProjectId
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


convertSpanRecord :: Projects.ProjectId -> Maybe Resource -> Maybe InstrumentationScope -> Span -> Telemetry.SpanRecord
convertSpanRecord pid resource scope sp =
  Telemetry.SpanRecord
    { uSpandId = Nothing
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


traceServiceExportH
  :: Log.Logger
  -> AuthContext
  -> ServerRequest 'Normal ExportTraceServiceRequest ExportTraceServiceResponse
  -> IO (ServerResponse 'Normal ExportTraceServiceResponse)
traceServiceExportH appLogger appCtx (ServerNormalRequest _meta (ExportTraceServiceRequest req)) = do
  _ <- runBackground appLogger appCtx do
    let projectKey = fromMaybe (error "Missing project key") $ getSpanAttributeValue "at-project-key" req
    projectIdM <- ProjectApiKeys.getProjectIdByApiKey projectKey
    let pid = fromMaybe (error $ "project API Key is invalid pid") projectIdM
    let spanRecords = join $ V.map (convertToSpan pid) req
        apitoolkitSpans = V.map mapHTTPSpan spanRecords
    _ <- ProcessMessage.processRequestMessages $ V.toList $ V.catMaybes apitoolkitSpans <&> ("",)
    Telemetry.bulkInsertSpans $ V.filter (\s -> s.spanName /= "apitoolkit-http-span") spanRecords
  return (ServerNormalResponse (ExportTraceServiceResponse Nothing) mempty StatusOk "")


mapHTTPSpan :: Telemetry.SpanRecord -> Maybe RequestMessage
mapHTTPSpan s = do
  case s.instrumentationScope of
    AE.Object v ->
      if apitoolkitSpan
        then Just $ convertSpanToRequestMessage s "apitoolkit-http-span"
        else
          if httpScope
            then Just $ convertSpanToRequestMessage s scopeName
            else Nothing
      where
        y = KEM.lookup "name" v
        scopeName =
          if y == Just "@opentelemetry/instrumentation-undici"
            then "@opentelemetry/instrumentation-undici"
            else "@opentelemetry/instrumentation-http"
        httpScope = y == Just "@opentelemetry/instrumentation-undici" || y == Just "@opentelemetry/instrumentation-http"
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


convertSpanToRequestMessage :: Telemetry.SpanRecord -> Text -> RequestMessage
convertSpanToRequestMessage sp instrumentationScope =
  RequestMessage
    { duration = fromInteger sp.spanDurationNs
    , host = host
    , method = method
    , pathParams = pathParams
    , projectId = sp.projectId
    , protoMajor = 1
    , protoMinor = 1
    , queryParams = queryParams
    , rawUrl = rawUrl
    , referer = referer
    , requestBody = requestBody
    , requestHeaders = requestHeaders
    , responseBody = responseBody
    , responseHeaders = responseHeaders
    , statusCode = status
    , sdkType = sdkType
    , msgId = messageId
    , parentId = parentId
    , errors
    , tags = Nothing
    , urlPath = urlPath
    , timestamp = utcToZonedTime (TimeZone 0 False "UTC+0") sp.timestamp
    , serviceVersion = Nothing
    }
  where
    host = getSpanAttribute "net.host.name" sp.attributes
    method = fromMaybe (fromMaybe "GET" $ getSpanAttribute "http.request.method" sp.attributes) $ getSpanAttribute "http.method" sp.attributes
    pathParams = fromMaybe (AE.object []) (AE.decode $ encodeUtf8 $ fromMaybe "" $ getSpanAttribute "http.request.path_params" sp.attributes)
    queryParams = fromMaybe (AE.object []) (AE.decode $ encodeUtf8 $ fromMaybe "" $ getSpanAttribute "http.request.query_params" sp.attributes)
    errors = AE.decode $ encodeUtf8 $ fromMaybe "" $ getSpanAttribute "apitoolkit.errors" sp.attributes
    messageId = UUID.fromText $ fromMaybe "" $ getSpanAttribute "apitoolkit.msg_id" sp.attributes
    parentId = UUID.fromText $ fromMaybe "" $ getSpanAttribute "apitoolkit.parent_id" sp.attributes
    referer = Just $ Left (fromMaybe "" $ getSpanAttribute "http.request.headers.referer" sp.attributes) :: Maybe (Either Text [Text])
    requestBody = fromMaybe "" $ getSpanAttribute "http.request.body" sp.attributes
    (requestHeaders, responseHeaders) = case sp.attributes of
      AE.Object v -> (getValsWithPrefix "http.request.header." v, getValsWithPrefix "http.response.header." v)
      _ -> (AE.object [], AE.object [])
    responseBody = fromMaybe "" $ getSpanAttribute "http.response.body" sp.attributes
    responseStatus = (readMaybe . toString =<< getSpanAttribute "http.response.status_code" sp.attributes) :: Maybe Double
    responseStatus' = (readMaybe . toString =<< getSpanAttribute "http.status_code" sp.attributes) :: Maybe Double
    status = round $ fromMaybe (fromMaybe 0.0 responseStatus') responseStatus
    sdkType = RequestDumps.parseSDKType $ fromMaybe "" $ getSpanAttribute "apitoolkit.sdk_type" sp.attributes
    urlPath' = getSpanAttribute "http.route" sp.attributes
    undUrlPath = getSpanAttribute "url.path" sp.attributes
    urlPath = if instrumentationScope == "@opentelemetry/instrumentation-undici" then undUrlPath else urlPath'
    rawUrl' = fromMaybe "" $ getSpanAttribute "http.target" sp.attributes
    rawUrl =
      if instrumentationScope == "@opentelemetry/instrumentation-undici"
        then fromMaybe "" undUrlPath <> fromMaybe "" (getSpanAttribute "url.query" sp.attributes)
        else rawUrl'


getValsWithPrefix :: Text -> AE.Object -> AE.Value
getValsWithPrefix prefix obj = AE.object $ map (\k -> (AEK.fromText (T.replace prefix "" $ AEK.toText k), fromMaybe (AE.object []) $ KEM.lookup k obj)) keys
  where
    keys = filter (\k -> prefix `T.isPrefixOf` AEK.toText k) (KEM.keys obj)


getSpanAttribute :: Text -> AE.Value -> Maybe Text
getSpanAttribute key attr = case attr of
  AE.Object o -> case KEM.lookup (AEK.fromText key) o of
    Just (AE.String v) -> Just v
    Just (AE.Number v) -> Just $ show v
    _ -> Nothing
  _ -> Nothing


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
    lnks
      <&> \lnk ->
        object
          [ "link_span_id" .= (decodeUtf8 lnk.span_LinkSpanId :: Text)
          , "link_trace_id" .= (decodeUtf8 lnk.span_LinkTraceId :: Text)
          , "link_attributes" .= keyValueToJSONB lnk.span_LinkAttributes
          , "link_dropped_attributes_count" .= fromIntegral lnk.span_LinkDroppedAttributesCount
          , "link_flags" .= fromIntegral lnk.span_LinkFlags
          ]


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
