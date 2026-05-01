{-# LANGUAGE StrictData #-}

module Opentelemetry.OtlpServer (
  processList,
  runServer,
  -- Exported for testing
  logsServiceExport,
  traceServiceExport,
  metricsServiceExport,
  processLogsRequest,
  processTraceRequest,
  processMetricsRequest,
  migrateHttpSemanticConventions,
  parseConnectionString,
  migrateElasticsearchPathParts,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (throwIO)
import Control.Exception.Annotated (checkpoint)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as KEM
import Data.Base64.Types qualified as B64
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Cache qualified as Cache
import Data.CaseInsensitive qualified as CI
import Data.Char (isDigit)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.UUID (UUIDEff)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.Map qualified as Map
import Data.ProtoLens.Encoding (decodeMessage)
import Data.Scientific (fromFloatDigits)
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Exception (onException, try)
import Effectful.Ki qualified as Ki
import Effectful.Labeled (Labeled)
import Effectful.Log (Log)
import Effectful.Reader.Static (ask)
import Effectful.Reader.Static qualified as Eff
import Effectful.Time qualified as Time
import Log (LogLevel (..), Logger, runLogT)
import Log qualified as LogBase
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (Context (..), OtelLogsAndSpans (..), Severity (..), generateSummary)
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server (RpcHandler, SomeRpcHandler, getRequestMetadata, mkRpcHandler, recvFinalInput, sendFinalOutput)
import Network.GRPC.Server.Run hiding (runServer)
import Network.GRPC.Server.StreamType (Methods (..), fromMethods)
import OpenTelemetry.Trace (TracerProvider)
import Pkg.DeriveUtils (AesonText (..), UUIDId (..), unUUIDId)
import Pkg.TraceSessionCache qualified as TSC
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService qualified as LS
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields qualified as LSF
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as MS
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as MSF
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
import Relude.Extra.Enum (safeToEnum)
import System.Config (AuthContext (..), EnvConfig (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Logging qualified as Log
import System.Types (ATBackgroundCtx, DB, runBackground)
import UnliftIO.Exception (tryAny)
import Utils (b64ToJson, freeTierDailyMaxEvents, jsonToMap, nestedJsonFromDotNotation)
import "base64" Data.ByteString.Base64 qualified as B64


-- | Custom request metadata for OTLP services containing optional API key from Authorization header
newtype OtlpRequestMetadata = OtlpRequestMetadata
  { otlpApiKey :: Maybe Text
  }
  deriving (Eq, Show)


-- | Parse API key from gRPC metadata (Authorization header, x-api-key header)
instance ParseMetadata OtlpRequestMetadata where
  parseMetadata metadata = do
    let findHeader name = find (\case CustomMetadata (AsciiHeader n) _ -> CI.mk n == CI.mk name; _ -> False) metadata
        extractValue (CustomMetadata _ value) = T.strip $ decodeUtf8 value
        stripBearer v = fromMaybe v $ T.stripPrefix "Bearer " v
        apiKey = (stripBearer . extractValue <$> findHeader "authorization") <|> (extractValue <$> findHeader "x-api-key")
    pure $ OtlpRequestMetadata{otlpApiKey = apiKey}


-- | Global error counters for common parsing errors (wire type, UTF-8, etc)
-- Second element tracks dropped categories when map is at capacity
wireTypeErrorsRef :: IORef (HM.HashMap Text (Int, AE.Value), Int)
{-# NOINLINE wireTypeErrorsRef #-}
wireTypeErrorsRef = unsafePerformIO $ newIORef (HM.empty, 0)


-- | Initialize periodic error logging
initPeriodicErrorLogging :: Logger -> IO ()
initPeriodicErrorLogging logger = void $ forkIO $ forever $ do
  threadDelay (60 * 1000000) -- 60 seconds
  (errors, dropped) <- atomicModifyIORef' wireTypeErrorsRef ((HM.empty, 0),)
  unless (HM.null errors) $ do
    let totalErrors = sum $ map fst $ HM.elems errors
        errorDetails =
          AE.object
            $ [ "period_seconds" AE..= (60 :: Int)
              , "total_errors" AE..= totalErrors
              , "error_types"
                  AE..= AE.object
                    [ AEK.fromText k
                        AE..= AE.object
                          [ "count" AE..= count
                          , "example" AE..= example
                          ]
                    | (k, (count, example)) <- HM.toList errors
                    ]
              ]
            ++ ["dropped_categories" AE..= dropped | dropped > 0]
    runLogT "monoscope" logger LogAttention
      $ LogBase.logAttention "Wire type errors summary" errorDetails


-- | Minimum valid timestamp in nanoseconds (Year 2000)
-- We consider any timestamp before this as invalid/unset
minValidTimestampNanos :: Word64
minValidTimestampNanos = 946684800000000000


-- import Network.GRPC.Server.Service (Service, service, method, fromServices)

-- | Generic lens-based attribute extraction from spans
-- | Keys that carry project identity and must not be stored in telemetry payloads
projectSecretKeys :: [Text]
projectSecretKeys = ["at-project-key", "at-project-id", "x-api-key"]


-- | Extract a string attribute by key from a KeyValue list
getAttr :: Text -> [PC.KeyValue] -> Maybe Text
getAttr !key kvs = (^. PCF.value . PCF.stringValue) <$> find ((== key) . (^. PCF.key)) kvs


-- | Extract a string from inside a kvlist attribute (e.g. x-api-key inside grpc.metadata)
getNestedAttr :: Text -> Text -> [PC.KeyValue] -> Maybe Text
getNestedAttr !outerKey !innerKey kvs = do
  kv <- find ((== outerKey) . (^. PCF.key)) kvs
  PC.AnyValue'KvlistValue kvl <- kv ^. PCF.value . PCF.maybe'value
  (^. PCF.value . PCF.stringValue) <$> find ((== innerKey) . (^. PCF.key)) (kvl ^. PCF.values)


-- | Extract API key from attributes, checking at-project-key, x-api-key, then x-api-key inside grpc.metadata/http.headers
getApiKeyAttr :: [PC.KeyValue] -> Maybe Text
getApiKeyAttr kvs =
  getAttr "at-project-key" kvs
    <|> getAttr "x-api-key" kvs
    <|> getNestedAttr "grpc.metadata" "x-api-key" kvs
    <|> getNestedAttr "http.headers" "x-api-key" kvs


getSpanAttributeValue :: Text -> V.Vector PT.ResourceSpans -> V.Vector Text
getSpanAttributeValue !attribute = V.mapMaybe (\rs -> getAttr attribute (rs ^. PTF.resource . PRF.attributes) <|> getSpanAttr rs)
  where
    getSpanAttr rs =
      let spans = V.fromList (rs ^. PTF.scopeSpans) >>= (\s -> V.fromList (s ^. PTF.spans))
       in V.mapMaybe (getAttr attribute . (^. PTF.attributes)) spans V.!? 0


getSpanApiKey :: V.Vector PT.ResourceSpans -> V.Vector Text
getSpanApiKey = V.mapMaybe (\rs -> getApiKeyAttr (rs ^. PTF.resource . PRF.attributes))


getLogAttributeValue :: Text -> V.Vector PL.ResourceLogs -> V.Vector Text
getLogAttributeValue !attribute = V.mapMaybe (\rl -> getAttr attribute (rl ^. PLF.resource . PRF.attributes) <|> getLogAttr rl)
  where
    getLogAttr rl =
      let logs = V.fromList (rl ^. PLF.scopeLogs) >>= (\s -> V.fromList (s ^. PLF.logRecords))
       in V.mapMaybe (getAttr attribute . (^. PLF.attributes)) logs V.!? 0


getLogApiKey :: V.Vector PL.ResourceLogs -> V.Vector Text
getLogApiKey = V.mapMaybe (\rl -> getApiKeyAttr (rl ^. PLF.resource . PRF.attributes))


getMetricAttributeValue :: Text -> V.Vector PM.ResourceMetrics -> Maybe Text
getMetricAttributeValue !attribute !rms = V.mapMaybe getResourceAttr rms V.!? 0
  where
    getResourceAttr rm = getAttr attribute (rm ^. PMF.resource . PRF.attributes)


getMetricApiKey :: V.Vector PM.ResourceMetrics -> Maybe Text
getMetricApiKey !rms = V.mapMaybe (\rm -> getApiKeyAttr (rm ^. PMF.resource . PRF.attributes)) rms V.!? 0


-- | Best-effort session stamping; falls back to unstamped spans on failure (backfill timer catches them).
stampOrPassthrough :: (IOE :> es, Log :> es, Time.Time :> es) => AuthContext -> V.Vector Telemetry.OtelLogsAndSpans -> Eff es (V.Vector Telemetry.OtelLogsAndSpans)
stampOrPassthrough appCtx v =
  try (TSC.lookupAndStamp appCtx.traceSessionCache v) >>= \case
    Right stamped -> pure stamped
    Left (e :: SomeException) -> do
      Log.logAttention "trace-session-cache:stamp-failed" (AE.object ["error" AE..= show @Text e, "span_count" AE..= V.length v])
      pure v


-- | Process a list of messages
processList :: (Concurrent :> es, DB es, Eff.Reader AuthContext :> es, Ki.StructuredConcurrency :> es, Labeled "timefusion" Hasql.Hasql :> es, Log :> es, Time.Time :> es, UUIDEff :> es) => [(Text, ByteString)] -> HM.HashMap Text Text -> Eff es [Text]
processList [] _ = pure []
processList msgs !attrs = checkpoint "processList" $ do
  startTime <- liftIO getCurrentTime

  (result, processingTime, dbInsertTime) <- process startTime `onException` handleException

  endTime <- liftIO getCurrentTime
  let duration = diffUTCTime endTime startTime
      eventCount = length msgs
  Log.logTrace
    "processList: batch processing completed"
    ( AE.object
        [ "ce-type" AE..= HM.lookup "ce-type" attrs
        , "event_count" AE..= eventCount
        , "duration_seconds" AE..= realToFrac @_ @Double duration
        , "duration_ms" AE..= (round (duration * 1000) :: Int)
        , "processing_ms" AE..= processingTime
        , "db_insert_ms" AE..= dbInsertTime
        ]
    )
  pure result
  where
    handleException = checkpoint "processList:exception" $ do
      Log.logAttention "processList: caught exception" (AE.object ["ce-type" AE..= HM.lookup "ce-type" attrs, "msg_count" AE..= length msgs, "attrs" AE..= attrs])
      pure (map fst msgs, 0, 0)

    process fallbackTime = do
      appCtx <- ask @AuthContext

      case HM.lookup "ce-type" attrs of
        Just "org.opentelemetry.otlp.logs.v1" ->
          processBatchPipeline @LS.ExportLogsServiceRequest
            "logs"
            msgs
            appCtx
            fallbackTime
            (\req -> getLogApiKey (V.fromList $ req ^. PLF.resourceLogs))
            (\req -> V.toList $ V.catMaybes $ Projects.projectIdFromText <$> getLogAttributeValue "at-project-id" (V.fromList $ req ^. PLF.resourceLogs))
            ( \ft caches keyToId req ->
                let !rls = V.fromList $ req ^. PLF.resourceLogs
                    !pkeys = getLogApiKey rls
                    !pids = V.fromList [(k, pid) | (k, pid) <- HM.toList keyToId, k `V.elem` pkeys]
                 in V.concatMap (V.fromList . convertResourceLogsToOtelLogs ft caches pids) rls
            )
            ( \caches v -> do
                stamped <- stampOrPassthrough appCtx v
                Telemetry.mintOtelLogIds stamped
                  >>= checkpoint "processList:logs:bulkInsert"
                  . Telemetry.insertAndHandOff appCtx.env.enableTimefusionWrites appCtx.extractionWorker caches
            )
        Just "org.opentelemetry.otlp.traces.v1" ->
          processBatchPipeline @TS.ExportTraceServiceRequest
            "traces"
            msgs
            appCtx
            fallbackTime
            (\req -> getSpanApiKey (V.fromList $ req ^. PTF.resourceSpans))
            (\req -> V.toList $ V.catMaybes $ Projects.projectIdFromText <$> getSpanAttributeValue "at-project-id" (V.fromList $ req ^. PTF.resourceSpans))
            ( \ft caches keyToId req ->
                let !rss = V.fromList $ req ^. PTF.resourceSpans
                    !pkeys = getSpanApiKey rss
                    !pids = V.fromList [(k, pid) | (k, pid) <- HM.toList keyToId, k `V.elem` pkeys]
                 in V.fromList $ convertResourceSpansToOtelLogs ft caches pids rss
            )
            ( \caches v -> do
                stamped <- stampOrPassthrough appCtx v
                Telemetry.mintOtelLogIds stamped
                  >>= checkpoint "processList:traces:bulkInsert"
                  . Telemetry.insertAndHandOff appCtx.env.enableTimefusionWrites appCtx.extractionWorker caches
            )
        Just "org.opentelemetry.otlp.metrics.v1" ->
          processBatchPipeline @MS.ExportMetricsServiceRequest
            "metrics"
            msgs
            appCtx
            fallbackTime
            (\req -> maybe V.empty V.singleton $ getMetricApiKey (V.fromList $ req ^. PMF.resourceMetrics))
            (\req -> maybeToList $ Projects.projectIdFromText =<< getMetricAttributeValue "at-project-id" (V.fromList $ req ^. PMF.resourceMetrics))
            ( \ft caches keyToId req ->
                let !rms = V.fromList $ req ^. PMF.resourceMetrics
                    !pidM = getMetricApiKey rms >>= (`HM.lookup` keyToId)
                    !pid2M = Projects.projectIdFromText =<< getMetricAttributeValue "at-project-id" rms
                 in maybe V.empty (\pid -> V.fromList $ convertResourceMetricsToMetricRecords ft caches pid rms) (pidM <|> pid2M)
            )
            (\_caches -> checkpoint "processList:metrics:bulkInsert" . Telemetry.bulkInsertMetrics)
        _ -> do
          Log.logAttention "processList: unsupported opentelemetry data type" (AE.object ["ce-type" AE..= HM.lookup "ce-type" attrs])
          pure ([], 0, 0)


processBatchPipeline
  :: forall req res es
   . (DB es, Eff.Reader AuthContext :> es, Log :> es, Message req)
  => Text
  -> [(Text, ByteString)]
  -> AuthContext
  -> UTCTime
  -> (req -> V.Vector Text) -- extract project keys
  -> (req -> [Projects.ProjectId]) -- extract project IDs
  -> (UTCTime -> HM.HashMap Projects.ProjectId Projects.ProjectCache -> HM.HashMap Text Projects.ProjectId -> req -> V.Vector res) -- per-message converter
  -> (HM.HashMap Projects.ProjectId Projects.ProjectCache -> V.Vector res -> Eff es ()) -- DB insert (caches threaded for worker hand-off)
  -> Eff es ([Text], Int, Int)
processBatchPipeline !label msgs appCtx fallbackTime extractKeys extractIds convert dbInsert =
  checkpoint (cp "") $ do
    processingStartTime <- liftIO getCurrentTime
    let !decodedMsgs = [(ackId, decodeMessage msg :: Either String req) | (ackId, msg) <- msgs]
        !allProjectKeys = V.concat [extractKeys req | (_, Right req) <- decodedMsgs]
        !uniqueProjectKeys = V.fromList $ toList $ HS.fromList $ V.toList allProjectKeys
        !atIds = concatMap extractIds [req | (_, Right req) <- decodedMsgs]

    (!keyToIdMap, !projectCachesMap) <-
      if V.null uniqueProjectKeys
        then pure (HM.empty, HM.empty)
        else do
          projectIdsAndKeys <- checkpoint (cp ":getProjectIds") $ ProjectApiKeys.projectIdsByProjectApiKeys uniqueProjectKeys
          let !keyToId = HM.fromList $ V.toList projectIdsAndKeys
              !projectIds = toList $ HS.fromList $ atIds <> HM.elems keyToId
          projectCaches <- checkpoint (cp ":getProjectCaches") $ do
            caches <- liftIO $ do
              cachePairs <- forM projectIds \pid ->
                Cache.fetchWithCache appCtx.projectCache pid \pid' -> do
                  mpjCache <- Projects.projectCacheByIdIO appCtx.hasqlJobsPool pid'
                  pure $! fromMaybe Projects.defaultProjectCache mpjCache
              pure $! zip projectIds cachePairs
            pure $ HM.fromList caches
          pure (keyToId, projectCaches)

    let !processedMsgs =
          [ case decodeResult of
              Left _ -> (ackId, V.empty)
              Right req -> let !out = convert fallbackTime projectCachesMap keyToIdMap req in (ackId, out)
          | (ackId, decodeResult) <- decodedMsgs
          ]

    forM_ [(idx, err) | (idx, (_, Left err)) <- zip [0 ..] decodedMsgs] \(idx, err) ->
      recordProtoError label err (snd $ msgs L.!! idx) Log.logAttention

    let !results = V.fromList processedMsgs
        (!ackIds, !outputVectors) = V.unzip results
        !allOutput = V.concatMap Relude.id outputVectors

    processingEndTime <- liftIO getCurrentTime
    let processingDuration = diffUTCTime processingEndTime processingStartTime

    dbInsertDuration <-
      if V.null allOutput
        then pure 0
        else do
          dbInsertStartTime <- liftIO getCurrentTime
          dbInsert projectCachesMap allOutput
          dbInsertEndTime <- liftIO getCurrentTime
          pure $ diffUTCTime dbInsertEndTime dbInsertStartTime

    pure (V.toList ackIds, round (processingDuration * 1000), round (dbInsertDuration * 1000))
  where
    cp suffix = fromString $ toString $ "processList:" <> label <> suffix


-- Helper functions

-- | Parse a database connection string to extract server address and port
--
-- >>> import qualified Data.Aeson as AE
-- >>> import "monoscope" Opentelemetry.OtlpServer qualified as OS
--
-- >>> OS.parseConnectionString "Server=localhost:5432"
-- [("server.address",String "localhost"),("server.port",String "5432")]
--
-- >>> OS.parseConnectionString "Server=localhost,1433"
-- [("server.address",String "localhost"),("server.port",String "1433")]
--
-- >>> OS.parseConnectionString "Server=localhost,1433;Database=mydb;User Id=myuser;Password=mypass;"
-- [("server.address",String "localhost"),("server.port",String "1433")]
--
-- >>> OS.parseConnectionString "Server=(localdb)\\v11.0;Integrated Security=true;"
-- [("server.address",String "(localdb)\\v11.0")]
--
-- >>> OS.parseConnectionString "Server=db.example.com"
-- [("server.address",String "db.example.com")]
--
-- >>> OS.parseConnectionString "Host=localhost"
-- [("server.address",String "localhost")]
--
-- >>> OS.parseConnectionString ""
-- []
parseConnectionString :: Text -> [(Text, AE.Value)]
parseConnectionString connStr =
  case T.breakOn "=" connStr of
    (prefix, serverPart)
      | not (T.null serverPart) && (T.toLower prefix == "server" || T.toLower prefix == "host") ->
          let serverVal = T.drop 1 serverPart
              -- Remove any trailing semicolons
              cleanVal = T.takeWhile (/= ';') serverVal
              -- Handle both colon and comma separators
              (host, portPart) = case T.breakOn ":" cleanVal of
                (h, p) | not (T.null p) -> (h, T.drop 1 p)
                _ -> case T.breakOn "," cleanVal of
                  (h, p) | not (T.null p) -> (h, T.drop 1 p) -- For comma, extract port
                  _ -> (cleanVal, "")
           in if T.null host
                then []
                else
                  ("server.address", AE.String host)
                    : [("server.port", AE.String portNum) | let portNum = T.takeWhile isDigit portPart, not (T.null portNum)]
    _ -> []


-- | Migrate Elasticsearch path parts to db.operation.parameter
--
-- >>> import qualified Data.Aeson as AE
-- >>> import "monoscope" Opentelemetry.OtlpServer qualified as OS
-- >>> OS.migrateElasticsearchPathParts [("db.elasticsearch.path_parts.index", AE.String "users"), ("db.elasticsearch.path_parts.id", AE.String "123")]
-- [("db.operation.parameter.index",String "users"),("db.operation.parameter.id",String "123")]
--
-- >>> OS.migrateElasticsearchPathParts [("other.field", AE.String "value"), ("db.elasticsearch.path_parts.doc_type", AE.String "customer")]
-- [("db.operation.parameter.doc_type",String "customer")]
--
-- >>> OS.migrateElasticsearchPathParts []
-- []
--
-- >>> OS.migrateElasticsearchPathParts [("db.system", AE.String "elasticsearch")]
-- []
migrateElasticsearchPathParts :: [(Text, AE.Value)] -> [(Text, AE.Value)]
migrateElasticsearchPathParts keyVals =
  [ ("db.operation.parameter." <> T.drop (T.length "db.elasticsearch.path_parts.") k, v)
  | (k, v) <- keyVals
  , "db.elasticsearch.path_parts." `T.isPrefixOf` k
  ]


-- | Migrate HTTP semantic convention fields to their new paths/formats
-- This function handles migration of deprecated OpenTelemetry semantic convention fields
-- to their new standardized names according to the latest OTEL spec.
--
-- >>> import qualified Data.Aeson as AE
-- >>> import "monoscope" Opentelemetry.OtlpServer qualified as OS
--
-- == HTTP field migrations
-- >>> OS.migrateHttpSemanticConventions [("http.method", AE.String "GET")]
-- [("http.request.method",String "GET")]
--
-- >>> OS.migrateHttpSemanticConventions [("http.status_code", AE.Number 200)]
-- [("http.response.status_code",Number 200.0)]
--
-- >>> OS.migrateHttpSemanticConventions [("http.url", AE.String "https://example.com"), ("http.client_ip", AE.String "192.168.1.1")]
-- [("url.full",String "https://example.com"),("client.address",String "192.168.1.1")]
--
-- == Database field migrations
-- >>> OS.migrateHttpSemanticConventions [("db.system", AE.String "postgresql"), ("db.name", AE.String "users")]
-- [("db.system.name",String "postgresql"),("db.namespace",String "users")]
--
-- >>> OS.migrateHttpSemanticConventions [("db.statement", AE.String "SELECT * FROM users")]
-- [("db.query.text",String "SELECT * FROM users")]
--
-- >>> OS.migrateHttpSemanticConventions [("db.cassandra.table", AE.String "products"), ("db.cassandra.consistency_level", AE.String "quorum")]
-- [("db.collection.name",String "products"),("cassandra.consistency.level",String "quorum")]
--
-- == Special migrations
-- >>> OS.migrateHttpSemanticConventions [("db.connection_string", AE.String "Server=db.example.com:5432")]
-- [("server.address",String "db.example.com"),("server.port",String "5432")]
--
-- >>> OS.migrateHttpSemanticConventions [("db.elasticsearch.path_parts.index", AE.String "logs-2024"), ("db.elasticsearch.path_parts.id", AE.String "abc123")]
-- [("db.operation.parameter.index",String "logs-2024"),("db.operation.parameter.id",String "abc123")]
--
-- >>> OS.migrateHttpSemanticConventions [("db.redis.database_index", AE.Number 3)]
-- [("db.namespace",String "3")]
--
-- == Deprecated fields removal
-- >>> OS.migrateHttpSemanticConventions [("db.user", AE.String "admin"), ("db.jdbc.driver_classname", AE.String "org.postgresql.Driver")]
-- []
--
-- == HTTP target migration
-- >>> OS.migrateHttpSemanticConventions [("http.target", AE.String "/api/users?page=1&limit=10")]
-- [("url.path",String "/api/users"),("url.query",String "page=1&limit=10")]
--
-- >>> OS.migrateHttpSemanticConventions [("http.target", AE.String "/api/users"), ("url.path", AE.String "/api/users")]
-- [("url.path",String "/api/users")]
--
-- == HTTP method _OTHER migration
-- >>> OS.migrateHttpSemanticConventions [("http.method", AE.String "_OTHER"), ("http.request.method_original", AE.String "PROPFIND")]
-- [("http.request.method",String "PROPFIND")]
--
-- == Browser error.* → OTel exception.* normalization
-- Browser SDKs commonly use @error.{name,type,message,stack}@ rather than the
-- OTel @exception.*@ semconv. We copy into @exception.*@ at ingest time so the
-- extraction and Issues pipelines only have to understand one namespace.
-- error.name (JS class, e.g. "TypeError") is preferred over error.type
-- (category, e.g. "uncaught_exception") for exception.type.
-- >>> OS.migrateHttpSemanticConventions [("error.name", AE.String "TypeError"), ("error.message", AE.String "x is null"), ("error.stack", AE.String "at f()")]
-- [("error.name",String "TypeError"),("error.message",String "x is null"),("error.stack",String "at f()"),("exception.type",String "TypeError"),("exception.message",String "x is null"),("exception.stacktrace",String "at f()")]
--
-- Existing @exception.*@ fields win when both are present:
-- >>> OS.migrateHttpSemanticConventions [("error.name", AE.String "TypeError"), ("exception.type", AE.String "DomainError")]
-- [("error.name",String "TypeError"),("exception.type",String "DomainError")]
fieldMappingsMap :: HM.HashMap Text Text
fieldMappingsMap =
  HM.fromList
    [ ("http.method", "http.request.method")
    , ("http.status_code", "http.response.status_code")
    , ("http.request_content_length", "http.request.body.size")
    , ("http.response_content_length", "http.response.body.size")
    , ("net.protocol.name", "network.protocol.name")
    , ("net.protocol.version", "network.protocol.version")
    , ("net.sock.peer.addr", "network.peer.address")
    , ("net.sock.peer.port", "network.peer.port")
    , ("http.url", "url.full")
    , ("http.resend_count", "http.request.resend_count")
    , ("net.peer.name", "server.address")
    , ("net.peer.port", "server.port")
    , ("http.scheme", "url.scheme")
    , ("http.client_ip", "client.address")
    , ("db.cassandra.consistency_level", "cassandra.consistency.level")
    , ("db.cassandra.coordinator.dc", "cassandra.coordinator.dc")
    , ("db.cassandra.coordinator.id", "cassandra.coordinator.id")
    , ("db.cassandra.idempotence", "cassandra.query.idempotent")
    , ("db.cassandra.page_size", "cassandra.page.size")
    , ("db.cassandra.speculative_execution_count", "cassandra.speculative_execution.count")
    , ("db.cassandra.table", "db.collection.name")
    , ("db.cosmosdb.client_id", "azure.client.id")
    , ("db.cosmosdb.connection_mode", "azure.cosmosdb.connection.mode")
    , ("db.cosmosdb.consistency_level", "azure.cosmosdb.consistency.level")
    , ("db.cosmosdb.container", "db.collection.name")
    , ("db.cosmosdb.regions_contacted", "azure.cosmosdb.operation.contacted_regions")
    , ("db.cosmosdb.request_charge", "azure.cosmosdb.operation.request_charge")
    , ("db.cosmosdb.request_content_length", "azure.cosmosdb.request.body.size")
    , ("db.cosmosdb.status_code", "db.response.status_code")
    , ("db.cosmosdb.sub_status_code", "azure.cosmosdb.response.sub_status_code")
    , ("db.elasticsearch.cluster.name", "db.namespace")
    , ("db.elasticsearch.node.name", "elasticsearch.node.name")
    , ("db.mongodb.collection", "db.collection.name")
    , ("db.name", "db.namespace")
    , ("db.operation", "db.operation.name")
    , ("db.redis.database_index", "db.namespace")
    , ("db.sql.table", "db.collection.name")
    , ("db.statement", "db.query.text")
    , ("db.system", "db.system.name")
    ]


migrateHttpSemanticConventions :: [(Text, AE.Value)] -> [(Text, AE.Value)]
migrateHttpSemanticConventions !keyVals =
  let
    -- Pre-build lookups for efficiency
    kvMap = HM.fromList keyVals
    hasUrlPath = HM.member "url.path" kvMap
    httpMethodM = HM.lookup "http.method" kvMap
    httpTargetM = HM.lookup "http.target" kvMap
    methodOriginalM = HM.lookup "http.request.method_original" kvMap

    -- Fields to remove (deprecated with no replacement)
    fieldsToRemove = ["db.user", "db.instance.id", "db.jdbc.driver_classname", "db.mssql.instance_name", "db.cosmosdb.operation_type"]

    -- Fields that require special handling and should be excluded from regular mapping
    -- These fields need custom migration logic rather than simple field renames:
    -- - db.connection_string: Gets parsed into server.address and server.port via parseConnectionString
    -- - http.target: Gets split into url.path and url.query components
    -- - db.redis.database_index: Converted from number to string for db.namespace
    -- - db.elasticsearch.path_parts.*: Transformed to db.operation.parameter.* preserving key suffix
    -- - http.method + http.request.method_original (when "_OTHER"): Special handling to use original method value
    specialFields =
      ["db.connection_string" | HM.member "db.connection_string" kvMap]
        ++ ["http.target" | isJust httpTargetM] -- Always exclude http.target when it exists
        ++ ["db.redis.database_index" | HM.member "db.redis.database_index" kvMap]
        ++ filter ("db.elasticsearch.path_parts." `T.isPrefixOf`) (map fst keyVals)
        ++ (if httpMethodM == Just (AE.String "_OTHER") && isJust methodOriginalM then ["http.method", "http.request.method_original"] else [])

    -- Filter out deprecated fields and special fields, then apply migrations
    filteredVals = filter (\(k, _) -> k `notElem` fieldsToRemove && k `notElem` specialFields) keyVals
    mgVals = map (\(k, v) -> (HM.lookupDefault k k fieldMappingsMap, v)) filteredVals

    -- Handle special migrations

    -- Handle db.connection_string -> server.address, server.port
    migrateConnectionString = case HM.lookup "db.connection_string" kvMap of
      Just (AE.String connStr) -> parseConnectionString connStr
      _ -> []

    -- Handle db.elasticsearch.path_parts.<key> -> db.operation.parameter.<key>
    migrateElasticsearchPaths = migrateElasticsearchPathParts keyVals

    -- Handle db.redis.database_index special case (convert to string for db.namespace)
    migrateRedisIndex = case HM.lookup "db.redis.database_index" kvMap of
      Just (AE.Number n) -> [("db.namespace", AE.String (show (round n :: Int)))]
      _ -> []

    -- Handle the special case of http.target -> url.path and url.query
    migrateHttpTarget = case httpTargetM of
      Just (AE.String target)
        | not hasUrlPath ->
            let
              (path, query) = T.breakOn "?" target
              queryWithoutQ = if T.null query then "" else T.drop 1 query
              pathPair = [("url.path", AE.String path)]
              queryPair = [("url.query", AE.String queryWithoutQ) | not (T.null query)]
             in
              pathPair ++ queryPair
      _ -> []

    -- Migrate method "_OTHER" case
    migrateMethodOther = case (httpMethodM, methodOriginalM) of
      (Just (AE.String "_OTHER"), Just originalMethod)
        | not (HM.member "http.request.method" kvMap) ->
            [("http.request.method", originalMethod)]
      _ -> []

    -- Browser SDKs use attributes.error.*; copy into OTel attributes.exception.*
    -- so the Issues pipeline has a single canonical namespace. Keeps the
    -- originals so bespoke queries / detail views that know about error.* still work.
    migrateBrowserError =
      let pick k = HM.lookup k kvMap
          emit target src = [(target, v) | not (HM.member target kvMap), Just v <- [src]]
       in emit "exception.type" (pick "error.name" <|> pick "error.type")
            ++ emit "exception.message" (pick "error.message")
            ++ emit "exception.stacktrace" (pick "error.stacktrace" <|> pick "error.stack")
   in
    mgVals ++ migrateHttpTarget ++ migrateMethodOther ++ migrateConnectionString ++ migrateElasticsearchPaths ++ migrateRedisIndex ++ migrateBrowserError


-- Extract structured information from protobuf decoding errors
createProtoErrorInfo :: String -> ByteString -> AE.Value
createProtoErrorInfo err msg =
  case T.splitOn ":" (toText err) of
    (firstPart : details) ->
      let fieldPath = T.takeWhile (/= ':') (T.strip (unwords details))
          errorType = T.takeWhileEnd (/= ':') (T.strip (unwords details))
       in AE.object ["err_type" AE..= errorType, "field_path" AE..= fieldPath, "full_err" AE..= err, "msg_size" AE..= BS.length msg]
    _ -> AE.object ["full_err" AE..= err, "msg_size" AE..= BS.length msg]


-- | Record a protobuf decode error, categorizing wire type, UTF-8, and EOF errors
recordProtoError :: MonadIO m => Text -> String -> ByteString -> (Text -> AE.Value -> m ()) -> m ()
recordProtoError prefix err msg logFn = do
  let errorInfo = createProtoErrorInfo err msg
      categorize
        | "Unknown wire type" `L.isInfixOf` err = Just (prefix <> ":wire_type_error")
        | "Cannot decode byte" `L.isInfixOf` err && "Invalid UTF-8 stream" `L.isInfixOf` err = Just (prefix <> ":utf8_decode_error")
        | "Unexpected end of input" `L.isInfixOf` err = Just (prefix <> ":unexpected_eof_error")
        | otherwise = Nothing
  case categorize of
    Just errorKey -> liftIO $ atomicModifyIORef' wireTypeErrorsRef $ \(m, dropped) ->
      if HM.size m >= 100
        then case HM.lookup errorKey m of
          Just (count, example) -> ((HM.insert errorKey (count + 1, example) m, dropped), ())
          Nothing -> ((m, dropped + 1), ())
        else
          let updateFn Nothing = Just (1, errorInfo)
              updateFn (Just (count, example)) = Just (count + 1, example)
           in ((HM.alter updateFn errorKey m, dropped), ())
    Nothing -> logFn ("processList:" <> prefix <> ": unable to parse service request") errorInfo


-- Convert nanoseconds to UTCTime
nanosecondsToUTC :: Word64 -> UTCTime
nanosecondsToUTC !ns = posixSecondsToUTCTime (fromIntegral ns / 1e9)


-- | Get a valid timestamp with fallback logic
-- Priority: timestamp -> observed_timestamp -> fallback time
getValidTimestamp :: UTCTime -> Word64 -> Word64 -> UTCTime
getValidTimestamp !fallbackTime !timeNano !observedTimeNano =
  let isValid ns = ns >= minValidTimestampNanos && ns /= 0
   in if isValid timeNano
        then nanosecondsToUTC timeNano
        else
          if isValid observedTimeNano
            then nanosecondsToUTC observedTimeNano
            else fallbackTime


-- Convert ByteString to hex Text
byteStringToHexText :: BS.ByteString -> Text
byteStringToHexText !bs = decodeUtf8 (B16.encode bs)


-- | Hex-encode a span/parent id, collapsing empty and all-zero bytes to
-- Nothing. Some producers send 8/16 zero bytes to mean "no parent" instead
-- of an empty field — downstream queries must not treat "0000…" as a real id.
hexIdMaybe :: BS.ByteString -> Maybe Text
hexIdMaybe bs
  | BS.null bs || BS.all (== 0) bs = Nothing
  | otherwise = Just (byteStringToHexText bs)


keyValueToJSON :: [PC.KeyValue] -> AE.Value
keyValueToJSON !kvs =
  let
    !allPairs' = [(kv ^. PCF.key, anyValueToJSON (Just (kv ^. PCF.value))) | kv <- kvs]
    !allPairs = migrateHttpSemanticConventions allPairs'

    specialKeys = projectSecretKeys
    (flatPairs, nestedPairs) = L.partition (\(k, _) -> k `elem` specialKeys) allPairs
    nestedObj = nestedJsonFromDotNotation nestedPairs
    flatObj = AE.object [AEK.fromText k AE..= v | (k, v) <- flatPairs]

    mergedObj =
      AE.Object
        $ KEM.union
          (case flatObj of AE.Object km -> km; _ -> KEM.empty)
          (case nestedObj of AE.Object km -> km; _ -> KEM.empty)
   in
    mergedObj


anyValueToJSON :: Maybe PC.AnyValue -> AE.Value
anyValueToJSON Nothing = AE.Null
anyValueToJSON (Just av) = do
  case av ^. PCF.maybe'value of
    Nothing -> AE.Null
    Just (PC.AnyValue'StringValue txt) ->
      let !stripped = T.strip txt
       in if T.isPrefixOf "{" stripped
            then case AE.eitherDecodeStrict' (encodeUtf8 stripped) of
              Right v -> AE.Object v
              Left _ -> AE.String txt
            else AE.String txt
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
      -- if the bytes is a json string, decode it
      -- otherwise just return AE.string like it it nowl
      AE.String $ B64.extractBase64 $ B64.encodeBase64 bs


-- Convert Resource to JSON
resourceToJSON :: Maybe PR.Resource -> AE.Value
resourceToJSON Nothing = AE.Null
resourceToJSON (Just resource) =
  let
    attrPairs = [(kv ^. PCF.key, anyValueToJSON (Just (kv ^. PCF.value))) | kv <- resource ^. PRF.attributes]
    specialKeys = projectSecretKeys
    (flatPairs, nestedPairs) = L.partition (\(k, _) -> k `elem` specialKeys) attrPairs

    nestedObj = nestedJsonFromDotNotation nestedPairs
    flatObj = AE.object [AEK.fromText k AE..= v | (k, v) <- flatPairs]
   in
    AE.Object
      $ KEM.union
        (case flatObj of AE.Object km -> km; _ -> KEM.empty)
        (case nestedObj of AE.Object km -> km; _ -> KEM.empty)


-- Remove project metadata from JSON
removeProjectId :: AE.Value -> AE.Value
removeProjectId (AE.Object o) = AE.Object $ foldr (KEM.delete . AEK.fromText) o projectSecretKeys
removeProjectId (AE.Array arr) = AE.Array $ V.map removeProjectId arr
removeProjectId v = v


canonicalLevels :: [(Text, Telemetry.SeverityLevel)]
canonicalLevels = [("TRACE", Telemetry.SLTrace), ("DEBUG", Telemetry.SLDebug), ("INFO", Telemetry.SLInfo), ("WARN", Telemetry.SLWarn), ("ERROR", Telemetry.SLError), ("FATAL", Telemetry.SLFatal)]


severityMap :: Map.Map Text (Text, Telemetry.SeverityLevel)
severityMap = Map.fromList $ [(t, (t, sl)) | (t, sl) <- canonicalLevels] <> [(alias, (canonical, sl)) | (alias, canonical) <- [("WARNING", "WARN"), ("INFORMATION", "INFO"), ("CRITICAL", "FATAL")], Just sl <- [L.lookup canonical canonicalLevels]]


{-# INLINE parseSeverityLevel #-}
parseSeverityLevel :: Text -> Maybe Telemetry.SeverityLevel
parseSeverityLevel txt = snd <$> Map.lookup (T.toUpper txt) severityMap


{-# INLINE normalizeSeverityLevel #-}
normalizeSeverityLevel :: Text -> Maybe Text
normalizeSeverityLevel txt = fst <$> Map.lookup (T.toUpper txt) severityMap


-- | Convert ResourceLogs to OtelLogsAndSpans
convertResourceLogsToOtelLogs :: UTCTime -> HM.HashMap Projects.ProjectId Projects.ProjectCache -> V.Vector (Text, Projects.ProjectId) -> PL.ResourceLogs -> [OtelLogsAndSpans]
convertResourceLogsToOtelLogs !fallbackTime !projectCaches !pids resourceLogs =
  let projectKey = fromMaybe "" $ (V.!? 0) $ getLogApiKey (V.singleton resourceLogs)
      projectId = case find (\(k, _) -> k == projectKey) pids of
        Just (_, v) -> Just v
        Nothing ->
          let pidText = fromMaybe "" $ (V.!? 0) $ getLogAttributeValue "at-project-id" (V.singleton resourceLogs)
              uId = UUID.fromText pidText
           in ((Just . UUIDId) =<< uId)
   in case projectId of
        Just pid ->
          case HM.lookup pid projectCaches of
            Just cache ->
              -- Check if project has exceeded daily limit for free tier
              let !totalDailyEvents = fromIntegral cache.dailyEventCount + fromIntegral cache.dailyMetricCount
                  !isFreeTier = cache.paymentPlan == "Free"
                  !hasExceededLimit = isFreeTier && totalDailyEvents >= freeTierDailyMaxEvents
               in if hasExceededLimit
                    then [] -- Discard events for projects that exceeded limits
                    else convertScopeLogsToOtelLogs fallbackTime pid (Just $ resourceLogs ^. PLF.resource) resourceLogs
            Nothing -> [] -- No cache found, discard
        _ -> []


filterEmptyEvents :: OtelLogsAndSpans -> Bool
filterEmptyEvents x =
  not (T.null (fromMaybe "" x.name) && isAesonTextEmpty x.body && isAesonTextEmpty x.attributes && isAesonTextEmpty x.resource && T.null (fromMaybe "" x.parent_id))


-- | Convert ScopeLogs to OtelLogsAndSpans
convertScopeLogsToOtelLogs :: UTCTime -> Projects.ProjectId -> Maybe PR.Resource -> PL.ResourceLogs -> [OtelLogsAndSpans]
convertScopeLogsToOtelLogs fallbackTime pid resourceM resourceLogs =
  filter
    filterEmptyEvents
    [ otelLog
    | scopeLog <- resourceLogs ^. PLF.scopeLogs
    , let scope = Just $ scopeLog ^. PLF.scope
    , logRecord <- scopeLog ^. PLF.logRecords
    , let otelLog = convertLogRecordToOtelLog fallbackTime pid resourceM scope logRecord
    ]


-- | Convert LogRecord to OtelLogsAndSpans
convertLogRecordToOtelLog :: UTCTime -> Projects.ProjectId -> Maybe PR.Resource -> Maybe PC.InstrumentationScope -> PL.LogRecord -> OtelLogsAndSpans
convertLogRecordToOtelLog !fallbackTime !pid resourceM scopeM logRecord =
  let !timeNano = logRecord ^. PLF.timeUnixNano
      !observedTimeNano = logRecord ^. PLF.observedTimeUnixNano
      !severityText = logRecord ^. PLF.severityText
      !severityNumber = fromEnum (logRecord ^. PLF.severityNumber)
      !validTimestamp = getValidTimestamp fallbackTime timeNano observedTimeNano
      !validObservedTimestamp =
        if observedTimeNano >= minValidTimestampNanos && observedTimeNano /= 0
          then nanosecondsToUTC observedTimeNano
          else fallbackTime

      !parentId = hexIdMaybe (logRecord ^. PLF.spanId)

      otelLog =
        OtelLogsAndSpans
          { project_id = pid.toText
          , id = UUID.toText UUID.nil
          , timestamp = validTimestamp
          , observed_timestamp = Just validObservedTimestamp
          , context =
              Just
                $ Context
                  { trace_id = Just $ byteStringToHexText $ logRecord ^. PLF.traceId
                  , span_id = Nothing
                  , trace_state = Nothing
                  , trace_flags = Nothing
                  , is_remote = Nothing
                  }
          , level = normalizeSeverityLevel severityText
          , severity =
              Just
                $ Severity
                  { severity_text = parseSeverityLevel severityText
                  , severity_number = severityNumber
                  }
          , body = fmap AesonText $ Just $ anyValueToJSON $ Just $ logRecord ^. PLF.body
          , attributes = fmap AesonText $ jsonToMap $ removeProjectId $ keyValueToJSON $ logRecord ^. PLF.attributes
          , resource = fmap AesonText $ jsonToMap $ removeProjectId $ resourceToJSON resourceM
          , hashes = V.empty
          , kind = Just "log"
          , status_code = Nothing
          , status_message = Nothing
          , duration = Nothing
          , start_time = validObservedTimestamp
          , end_time = Nothing
          , events = Nothing
          , links = Nothing
          , name = Nothing
          , parent_id = parentId
          , summary = V.empty -- Will be populated after creation
          , date = validTimestamp
          , errors = Nothing
          }
   in otelLog{summary = generateSummary otelLog}


-- | Convert ResourceSpans to OtelLogsAndSpans
convertResourceSpansToOtelLogs :: UTCTime -> HM.HashMap Projects.ProjectId Projects.ProjectCache -> V.Vector (Text, Projects.ProjectId) -> V.Vector PT.ResourceSpans -> [OtelLogsAndSpans]
convertResourceSpansToOtelLogs !fallbackTime !projectCaches !pids !resourceSpans =
  concatMap convertOne (V.toList resourceSpans)
  where
    convertOne rs =
      let projectKey = fromMaybe "" $ (V.!? 0) $ getSpanApiKey (V.singleton rs)
          projectId =
            case find (\(k, _) -> k == projectKey && k /= "") pids of
              Just (_, v) -> Just v
              Nothing ->
                case (V.!? 0) $ getSpanAttributeValue "at-project-id" (V.singleton rs) of
                  Just pidText | Just uid <- UUID.fromText pidText -> Just (UUIDId uid)
                  _ | not (V.null pids) -> Just (snd $ V.head pids)
                  _ -> Nothing
       in case projectId of
            Just pid ->
              case HM.lookup pid projectCaches of
                Just cache ->
                  let !totalDailyEvents = toInteger cache.dailyEventCount + toInteger cache.dailyMetricCount
                      !isFreeTier = cache.paymentPlan == "Free"
                      !hasExceededLimit = isFreeTier && totalDailyEvents >= freeTierDailyMaxEvents
                   in if hasExceededLimit then [] else convertScopeSpansToOtelLogs fallbackTime pid (Just $ rs ^. PTF.resource) rs
                Nothing -> []
            _ -> []


-- | Convert ScopeSpans to OtelLogsAndSpansF
convertScopeSpansToOtelLogs :: UTCTime -> Projects.ProjectId -> Maybe PR.Resource -> PT.ResourceSpans -> [OtelLogsAndSpans]
convertScopeSpansToOtelLogs fallbackTime pid resourceM resourceSpans =
  filter
    filterEmptyEvents
    [ otelLog
    | scopeSpan <- resourceSpans ^. PTF.scopeSpans
    , let scope = Just $ scopeSpan ^. PTF.scope
    , pSpan <- scopeSpan ^. PTF.spans
    , let otelLog = convertSpanToOtelLog fallbackTime pid resourceM scope pSpan
    ]


-- | Convert Span to OtelLogsAndSpans
convertSpanToOtelLog :: UTCTime -> Projects.ProjectId -> Maybe PR.Resource -> Maybe PC.InstrumentationScope -> PT.Span -> OtelLogsAndSpans
convertSpanToOtelLog !fallbackTime !pid resourceM scopeM pSpan =
  let !startTimeNano = pSpan ^. PTF.startTimeUnixNano
      !endTimeNano = pSpan ^. PTF.endTimeUnixNano
      !validStartTime =
        if startTimeNano >= minValidTimestampNanos && startTimeNano /= 0
          then nanosecondsToUTC startTimeNano
          else fallbackTime
      !validEndTime =
        if endTimeNano >= minValidTimestampNanos && endTimeNano /= 0
          then nanosecondsToUTC endTimeNano
          else validStartTime -- If end time is invalid, use start time
      !durationNanos =
        if startTimeNano > 0 && endTimeNano > startTimeNano
          then endTimeNano - startTimeNano
          else 0
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
      parentId = hexIdMaybe (pSpan ^. PTF.parentSpanId)

      -- Convert events only if non-empty
      eventsJson =
        let !events = pSpan ^. PTF.events
         in if null events
              then Nothing
              else
                Just
                  $! AE.toJSON
                  $! map
                    ( \ev ->
                        AE.object
                          [ "event_name" AE..= (ev ^. PTF.name)
                          , "event_time"
                              AE..= let evTimeNano = ev ^. PTF.timeUnixNano
                                     in if evTimeNano >= minValidTimestampNanos && evTimeNano /= 0
                                          then nanosecondsToUTC evTimeNano
                                          else fallbackTime
                          , "event_attributes" AE..= keyValueToJSON (ev ^. PTF.attributes)
                          , "event_dropped_attributes_count" AE..= (ev ^. PTF.droppedAttributesCount)
                          ]
                    )
                    events

      -- Convert links only if non-empty
      linksJson =
        let !links = pSpan ^. PTF.links
         in if null links
              then Nothing
              else
                Just
                  $! show
                  $! AE.toJSON
                  $! map
                    ( \link ->
                        AE.object
                          [ "link_span_id" AE..= byteStringToHexText (link ^. PTF.spanId)
                          , "link_trace_id" AE..= byteStringToHexText (link ^. PTF.traceId)
                          , "link_attributes" AE..= keyValueToJSON (link ^. PTF.attributes)
                          , "link_dropped_attributes_count" AE..= (link ^. PTF.droppedAttributesCount)
                          , "link_flags" AE..= (link ^. PTF.traceState)
                          ]
                    )
                    links
      !attributes = jsonToMap $ removeProjectId $ keyValueToJSON $ pSpan ^. PTF.attributes
      spanName' = pSpan ^. PTF.name
      -- "monoscope.http" included so re-ingested spans get consistent body/attribute processing
      isOurSdkSpan = spanName' `elem` ["apitoolkit-http-span", "monoscope.http"]
      (req, res) = case Map.lookup "http" (fromMaybe Map.empty attributes) of
        Just (AE.Object http) -> (KEM.lookup "request" http, KEM.lookup "response" http)
        _ -> (Nothing, Nothing)
      body =
        if isOurSdkSpan
          then
            Just
              $ AE.object
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
      newAttributes' =
        if isOurSdkSpan
          then
            let htt = Map.lookup "http" (fromMaybe Map.empty attributes)
             in case htt of
                  Just (AE.Object http) ->
                    let newHttp = case (req, res) of
                          (Just (AE.Object re), Just (AE.Object rs)) ->
                            let newReq = KEM.insert "request" (AE.Object $ KEM.delete "body" re) http
                                newRes = KEM.insert "response" (AE.Object $ KEM.delete "body" rs) newReq
                             in Just $ Map.insert "http" (AE.Object newRes) (fromMaybe Map.empty attributes)
                          _ -> attributes
                     in newHttp
                  _ -> attributes
          else attributes
      -- Fall back to resource.service.name for server.address when no host attribute is present on HTTP spans
      !resourceAttrs = jsonToMap $ removeProjectId $ resourceToJSON resourceM
      newAttributes = case newAttributes' of
        Just attrs | hasNoHost attrs ->
          case hostFromUrlFull attrs <|> serviceName of
            Just sn | not (T.null sn) -> Just $ Map.insertWith mergeObjects "server" (AE.Object $ KEM.singleton "address" (AE.String sn)) attrs
            _ -> newAttributes'
        _ -> newAttributes'
      hasNoHost attrs =
        isNothing (lookupNested "net" "host" attrs)
          && isNothing (lookupNested "server" "address" attrs)
          && isNothing (lookupNested "http" "host" attrs)
      lookupNested outer inner attrs =
        Map.lookup outer attrs >>= \case
          AE.Object o -> KEM.lookup (AEK.fromText inner) o
          _ -> Nothing
      hostFromUrlFull attrs =
        lookupNested "url" "full" attrs >>= \case
          AE.String u ->
            let stripped = fromMaybe u $ T.stripPrefix "https://" u <|> T.stripPrefix "http://" u
                h = T.takeWhile (\c -> c /= '/' && c /= '?' && c /= ':') stripped
             in if T.null h then Nothing else Just h
          _ -> Nothing
      serviceName =
        resourceAttrs >>= Map.lookup "service" >>= \case
          AE.Object s -> KEM.lookup "name" s >>= \case AE.String n -> Just n; _ -> Nothing
          _ -> Nothing
      mergeObjects (AE.Object new) (AE.Object old) = AE.Object (KEM.union new old)
      mergeObjects new _ = new
      otelSpan =
        OtelLogsAndSpans
          { project_id = pid.toText
          , id = UUID.toText UUID.nil
          , timestamp = validStartTime
          , observed_timestamp = Just validStartTime
          , context =
              Just
                $ Context
                  { trace_id = Just $ byteStringToHexText $ pSpan ^. PTF.traceId
                  , span_id = Just $ byteStringToHexText $ pSpan ^. PTF.spanId
                  , trace_state = Just $ pSpan ^. PTF.traceState
                  , trace_flags = Nothing
                  , is_remote = Nothing
                  }
          , level = Nothing
          , severity = Nothing
          , body = fmap AesonText body
          , attributes = fmap AesonText newAttributes
          , resource = fmap AesonText resourceAttrs
          , hashes = V.empty
          , kind = if spanName' == "apitoolkit-http-span" then Just "server" else spanKindText
          , status_code = statusCodeText
          , status_message = statusMsgText
          , duration = Just $ fromIntegral durationNanos
          , start_time = validStartTime
          , end_time = Just validEndTime
          , events = fmap AesonText eventsJson
          , links = linksJson
          , name = Just $ if isOurSdkSpan then "monoscope.http" else spanName'
          , parent_id = parentId
          , summary = V.empty -- Will be populated after creation
          , date = validStartTime
          , errors = Nothing
          }
   in otelSpan{summary = generateSummary otelSpan}


-- | Convert ResourceMetrics to MetricRecords
convertResourceMetricsToMetricRecords :: UTCTime -> HM.HashMap Projects.ProjectId Projects.ProjectCache -> Projects.ProjectId -> V.Vector PM.ResourceMetrics -> [Telemetry.MetricRecord]
convertResourceMetricsToMetricRecords !fallbackTime !projectCaches !pid !resourceMetrics =
  case HM.lookup pid projectCaches of
    Just cache ->
      let !totalDailyEvents = fromIntegral cache.dailyEventCount + fromIntegral cache.dailyMetricCount
          !isFreeTier = cache.paymentPlan == "Free"
          !hasExceededLimit = isFreeTier && totalDailyEvents >= freeTierDailyMaxEvents
       in if hasExceededLimit
            then []
            else concatMap (convertResourceMetricToMetricRecords fallbackTime pid) (V.toList resourceMetrics)
    Nothing -> []


-- | Convert a single ResourceMetrics to MetricRecords
convertResourceMetricToMetricRecords :: UTCTime -> Projects.ProjectId -> PM.ResourceMetrics -> [Telemetry.MetricRecord]
convertResourceMetricToMetricRecords fallbackTime pid resourceMetric =
  let resourceM = Just $ resourceMetric ^. PMF.resource
   in [ record
      | sm <- resourceMetric ^. PMF.scopeMetrics
      , let scope = Just $ sm ^. PMF.scope
      , metric <- sm ^. PMF.metrics
      , record <- convertMetricToMetricRecords fallbackTime pid resourceM scope metric
      ]


-- | Convert a single Metric to MetricRecords
convertMetricToMetricRecords :: UTCTime -> Projects.ProjectId -> Maybe PR.Resource -> Maybe PC.InstrumentationScope -> PM.Metric -> [Telemetry.MetricRecord]
convertMetricToMetricRecords fallbackTime pid resourceM scopeM metric =
  case metric ^. PMF.maybe'data' of
    Just metricData ->
      case metricData of
        PM.Metric'Gauge gauge -> convertNumberDataPoints (gauge ^. PMF.dataPoints) Telemetry.MTGauge Telemetry.GaugeValue Nothing Nothing
        PM.Metric'Sum s ->
          let !n = fromEnum (s ^. PMF.aggregationTemporality)
              !temporality = safeToEnum @Telemetry.AggregationTemporality n
              !monotonic = Just $ s ^. PMF.isMonotonic
           in convertNumberDataPoints (s ^. PMF.dataPoints) Telemetry.MTSum Telemetry.SumValue temporality monotonic
        PM.Metric'Histogram hist -> mapMaybe convertHistogramPoint $ hist ^. PMF.dataPoints
        PM.Metric'ExponentialHistogram ehist -> map convertExpHistogramPoint $ ehist ^. PMF.dataPoints
        PM.Metric'Summary summary -> map convertSummaryPoint $ summary ^. PMF.dataPoints
    Nothing -> []
  where
    !res = removeProjectId $ resourceToJSON resourceM
    !mName = metric ^. PMF.name
    !mDesc = metric ^. PMF.description
    !mUnit = metric ^. PMF.unit
    !scopeJSON = case scopeM of
      Just scope ->
        AE.object
          [ "name" AE..= (scope ^. PCF.name)
          , "version" AE..= (scope ^. PCF.version)
          , "attributes" AE..= keyValueToJSON (scope ^. PCF.attributes)
          ]
      Nothing -> AE.Null
    !metaJSON = case scopeM of
      Just scope -> AE.object ["scope" AE..= (scope ^. PCF.name)]
      Nothing -> AE.Null

    pointTime timeNano = if timeNano >= minValidTimestampNanos && timeNano /= 0 then nanosecondsToUTC timeNano else fallbackTime
    pointAttrs point = keyValueToJSON (V.toList $ point ^. PMF.vec'attributes)

    mkRecord validTime attrs mType mValue fls temporality_ monotonic_ =
      Telemetry.MetricRecord
        { projectId = unUUIDId pid
        , id = Nothing
        , metricName = mName
        , metricDescription = mDesc
        , metricUnit = mUnit
        , timestamp = validTime
        , metricTime = validTime
        , resource = res
        , instrumentationScope = scopeJSON
        , metricValue = mValue
        , exemplars = AE.object []
        , metricType = mType
        , flags = fls
        , attributes = attrs
        , metricMetadata = metaJSON
        , aggregationTemporality = temporality_
        , isMonotonic = monotonic_
        }

    convertNumberDataPoints points mType wrap temporality_ monotonic_ = map go points
      where
        go point =
          let !value = case point ^. PMF.maybe'value of
                Just (PM.NumberDataPoint'AsDouble d) -> d
                Just (PM.NumberDataPoint'AsInt i) -> fromIntegral i
                _ -> 0
           in mkRecord (pointTime $ point ^. PMF.timeUnixNano) (pointAttrs point) mType (wrap $ Telemetry.GaugeSum{value}) (fromIntegral $ point ^. PMF.flags) temporality_ monotonic_

    convertHistogramPoint point
      | VU.length vBCounts /= VU.length vEBounds + 1 = Nothing -- OTLP spec: len(bucket_counts) == len(explicit_bounds) + 1
      | otherwise = Just $ mkRecord (pointTime $ point ^. PMF.timeUnixNano) (pointAttrs point) Telemetry.MTHistogram (Telemetry.HistogramValue hval) (fromIntegral $ point ^. PMF.flags) Nothing Nothing
      where
        vBCounts = point ^. PMF.vec'bucketCounts
        vEBounds = point ^. PMF.vec'explicitBounds
        hval =
          Telemetry.Histogram
            { sum = fromMaybe 0 $ point ^. PMF.maybe'sum
            , count = fromIntegral $ point ^. PMF.count
            , bucketCounts = V.fromList $ map fromIntegral $ point ^. PMF.bucketCounts
            , explicitBounds = V.fromList $ point ^. PMF.explicitBounds
            , pointMin = fromMaybe 0 $ point ^. PMF.maybe'min
            , pointMax = fromMaybe 0 $ point ^. PMF.maybe'max
            }

    convertExpHistogramPoint point =
      mkRecord (pointTime $ point ^. PMF.timeUnixNano) (pointAttrs point) Telemetry.MTExponentialHistogram (Telemetry.ExponentialHistogramValue ehval) (fromIntegral $ point ^. PMF.flags) Nothing Nothing
      where
        toBucket b = Telemetry.EHBucket{bucketOffset = fromIntegral $ b ^. PMF.offset, bucketCounts = V.fromList $ map fromIntegral $ b ^. PMF.bucketCounts}
        !ehval =
          Telemetry.ExponentialHistogram
            { sum = fromMaybe 0 $ point ^. PMF.maybe'sum
            , count = fromIntegral $ point ^. PMF.count
            , pointMin = fromMaybe 0 $ point ^. PMF.maybe'min
            , pointMax = fromMaybe 0 $ point ^. PMF.maybe'max
            , zeroCount = fromIntegral $ point ^. PMF.zeroCount
            , scale = fromIntegral $ point ^. PMF.scale
            , pointPositive = toBucket <$> point ^. PMF.maybe'positive
            , pointNegative = toBucket <$> point ^. PMF.maybe'negative
            , zeroThreshold = point ^. PMF.zeroThreshold
            }

    convertSummaryPoint point =
      mkRecord (pointTime $ point ^. PMF.timeUnixNano) (pointAttrs point) Telemetry.MTSummary (Telemetry.SummaryValue sval) (fromIntegral $ point ^. PMF.flags) Nothing Nothing
      where
        !sval =
          Telemetry.Summary
            { sum = point ^. PMF.sum
            , count = fromIntegral $ point ^. PMF.count
            , quantiles = V.fromList $ map (\q -> Telemetry.Quantile{quantile = q ^. PMF.quantile, value = q ^. PMF.value}) $ point ^. PMF.quantileValues
            }


---------------------------------------------------------------------------------------
-- Server
---------------------------------------------------------------------------------------

runServer :: Logger -> AuthContext -> TracerProvider -> IO ()
runServer appLogger appCtx tp = do
  initPeriodicErrorLogging appLogger
  runServerWithHandlers def config (services appLogger appCtx tp)
  where
    serverHost = "0.0.0.0"
    serverPort = appCtx.config.grpcPort
    config :: ServerConfig
    config =
      ServerConfig
        { serverInsecure = Just (InsecureConfig (Just $ toString serverHost) (fromIntegral serverPort))
        , serverSecure = Nothing
        }


-- | Process trace request with optional API key from gRPC metadata (extracted for testing)
processTraceRequest :: (Concurrent :> es, DB es, Eff.Reader AuthContext :> es, Ki.StructuredConcurrency :> es, Labeled "timefusion" Hasql.Hasql :> es, Log :> es, Time.Time :> es, UUIDEff :> es) => Maybe Text -> TS.ExportTraceServiceRequest -> Eff es ()
processTraceRequest metadataApiKey req = do
  Log.logTrace "Received trace export request" AE.Null

  currentTime <- liftIO getCurrentTime
  appCtx <- ask @AuthContext

  let !resourceSpans = V.fromList $ req ^. TSF.resourceSpans
      !projectKeys = getSpanApiKey resourceSpans
      atIds' = getSpanAttributeValue "at-project-id" resourceSpans
      atIds = V.catMaybes $ Projects.projectIdFromText <$> atIds'

      -- Combine API key from metadata with keys from resource attributes
      !allApiKeys = case metadataApiKey of
        Just key -> V.cons key projectKeys
        Nothing -> projectKeys

  -- Verify authentication: if project keys or IDs are present, they must resolve to valid projects
  when (not (V.null allApiKeys) || not (V.null atIds)) $ do
    projectIdsAndKeys <- ProjectApiKeys.projectIdsByProjectApiKeys allApiKeys
    let allProjectIds = toList . HS.fromList $ V.toList atIds <> [pid | (_, pid) <- V.toList projectIdsAndKeys]
    when (null allProjectIds)
      $ liftIO
      $ throwIO
      $ GrpcException
        { grpcError = GrpcUnauthenticated
        , grpcErrorMessage = Just "Invalid or missing project API key"
        , grpcErrorMetadata = []
        , grpcErrorDetails = Nothing
        }
    Log.logTrace
      "Traces: Authentication successful"
      (AE.object ["project_ids" AE..= map unUUIDId allProjectIds, "project_keys" AE..= V.toList allApiKeys, "metadata_auth" AE..= isJust metadataApiKey])

  projectIdsAndKeys <-
    checkpoint "processList:traces:getProjectIds"
      $ ProjectApiKeys.projectIdsByProjectApiKeys allApiKeys
  -- Fetch project caches for limit checking
  projectCaches <- checkpoint "processList:traces:getProjectCaches" $ do
    let projectIds = toList . HS.fromList $ V.toList atIds <> [pid | (_, pid) <- V.toList projectIdsAndKeys]

    caches <- forM projectIds $ \pid -> do
      cache <- liftIO $ Cache.fetchWithCache appCtx.projectCache pid $ \pid' -> do
        mpjCache <- Projects.projectCacheByIdIO appCtx.hasqlJobsPool pid'
        pure $ fromMaybe Projects.defaultProjectCache mpjCache
      pure (pid, cache)
    pure $ HM.fromList caches
  let !spans' = V.fromList $ convertResourceSpansToOtelLogs currentTime projectCaches projectIdsAndKeys resourceSpans

  Log.logTrace
    "Traces: Converted resource spans to OtelLogs"
    (AE.object ["span_count" AE..= V.length spans'])

  unless (V.null spans') do
    stampedSpans <- stampOrPassthrough appCtx spans'
    mintedSpans <- Telemetry.mintOtelLogIds stampedSpans
    Telemetry.insertAndHandOff appCtx.env.enableTimefusionWrites appCtx.extractionWorker projectCaches mintedSpans
    Log.logTrace
      "Traces: Successfully inserted spans into database"
      (AE.object ["inserted_count" AE..= V.length mintedSpans])


-- | Trace service handler (Export)
traceServiceExport :: Logger -> AuthContext -> TracerProvider -> Proto TS.ExportTraceServiceRequest -> IO (Proto TS.ExportTraceServiceResponse)
traceServiceExport appLogger appCtx tp (Proto req) = do
  -- Note: This version is for backwards compatibility when called directly in tests
  -- The RpcHandler version below has access to metadata
  _ <- runBackground appLogger appCtx tp $ processTraceRequest Nothing req
  -- Return an empty response
  pure defMessage


-- | Process logs request with optional API key from gRPC metadata (extracted for testing)
processLogsRequest :: (Concurrent :> es, DB es, Eff.Reader AuthContext :> es, Ki.StructuredConcurrency :> es, Labeled "timefusion" Hasql.Hasql :> es, Log :> es, Time.Time :> es, UUIDEff :> es) => Maybe Text -> LS.ExportLogsServiceRequest -> Eff es ()
processLogsRequest metadataApiKey req = do
  Log.logTrace "Received logs export request" AE.Null
  currentTime <- liftIO getCurrentTime
  appCtx <- ask @AuthContext

  let !resourceLogs = V.fromList $ req ^. PLF.resourceLogs
      !projectKeys = getLogApiKey resourceLogs
      atIds' = getLogAttributeValue "at-project-id" resourceLogs
      atIds = V.catMaybes $ Projects.projectIdFromText <$> atIds'

      -- Combine API key from metadata with keys from resource attributes
      !allApiKeys = case metadataApiKey of
        Just key -> V.cons key projectKeys
        Nothing -> projectKeys

  -- Verify authentication: if project keys or IDs are present, they must resolve to valid projects
  when (not (V.null allApiKeys) || not (V.null atIds)) $ do
    projectIdsAndKeys <- ProjectApiKeys.projectIdsByProjectApiKeys allApiKeys
    let allProjectIds = toList . HS.fromList $ V.toList atIds <> [pid | (_, pid) <- V.toList projectIdsAndKeys]
    when (null allProjectIds)
      $ liftIO
      $ throwIO
      $ GrpcException
        { grpcError = GrpcUnauthenticated
        , grpcErrorMessage = Just "Invalid or missing project API key"
        , grpcErrorMetadata = []
        , grpcErrorDetails = Nothing
        }
    Log.logTrace
      "Logs: Authentication successful"
      (AE.object ["project_ids" AE..= map unUUIDId allProjectIds, "project_keys" AE..= V.toList allApiKeys, "metadata_auth" AE..= isJust metadataApiKey])

  projectIdsAndKeys <- ProjectApiKeys.projectIdsByProjectApiKeys allApiKeys

  -- Fetch project caches using cache pattern
  projectCaches <- checkpoint "processList:logs:getProjectCaches" $ do
    let projectIds = toList . HS.fromList $ V.toList atIds <> [pid | (_, pid) <- V.toList projectIdsAndKeys]
    caches <- forM projectIds $ \pid -> do
      cache <- liftIO $ Cache.fetchWithCache appCtx.projectCache pid $ \pid' -> do
        mpjCache <- Projects.projectCacheByIdIO appCtx.hasqlJobsPool pid'
        pure $ fromMaybe Projects.defaultProjectCache mpjCache
      pure (pid, cache)
    pure $ HM.fromList caches

  let !logs = V.fromList $ concatMap (convertResourceLogsToOtelLogs currentTime projectCaches projectIdsAndKeys) (V.toList resourceLogs)

  Log.logTrace
    "Logs: Converted resource logs to OtelLogs"
    (AE.object ["log_count" AE..= V.length logs])

  unless (V.null logs) do
    stampedLogs <- stampOrPassthrough appCtx logs
    mintedLogs <- Telemetry.mintOtelLogIds stampedLogs
    Telemetry.insertAndHandOff appCtx.env.enableTimefusionWrites appCtx.extractionWorker projectCaches mintedLogs
    Log.logTrace
      "Logs: Successfully inserted logs into database"
      (AE.object ["inserted_count" AE..= V.length mintedLogs])


-- | Logs service handler (Export)
logsServiceExport :: Logger -> AuthContext -> TracerProvider -> Proto LS.ExportLogsServiceRequest -> IO (Proto LS.ExportLogsServiceResponse)
logsServiceExport appLogger appCtx tp (Proto req) = do
  -- Note: This version is for backwards compatibility when called directly in tests
  -- The RpcHandler version below has access to metadata
  _ <- runBackground appLogger appCtx tp $ processLogsRequest Nothing req
  -- Return an empty response
  pure defMessage


-- | Process metrics request with optional API key from gRPC metadata (extracted for testing)
processMetricsRequest :: (DB es, Eff.Reader AuthContext :> es, Log :> es) => Maybe Text -> MS.ExportMetricsServiceRequest -> Eff es ()
processMetricsRequest metadataApiKey req = do
  Log.logTrace "Received metrics export request" AE.Null

  currentTime <- liftIO getCurrentTime
  appCtx <- ask @AuthContext

  let !resourceMetrics = V.fromList $ req ^. PMF.resourceMetrics
      !projectKey = getMetricApiKey resourceMetrics

  pidM <- do
    -- Try metadata API key first, then resource attribute key, then project ID
    p1 <- case metadataApiKey of
      Just key -> ProjectApiKeys.getProjectIdByApiKey key
      Nothing -> pure Nothing
    p2 <- join <$> forM projectKey ProjectApiKeys.getProjectIdByApiKey
    let p3 = Projects.projectIdFromText =<< getMetricAttributeValue "at-project-id" resourceMetrics
    pure (p1 <|> p2 <|> p3)

  case pidM of
    Just pid -> do
      Log.logTrace
        "Metrics: Authentication successful"
        (AE.object ["project_id" AE..= unUUIDId pid, "project_key" AE..= projectKey, "metadata_auth" AE..= isJust metadataApiKey])

      -- Fetch project cache using cache pattern
      projectCache <- liftIO $ Cache.fetchWithCache appCtx.projectCache pid $ \pid' -> do
        mpjCache <- Projects.projectCacheByIdIO appCtx.hasqlJobsPool pid'
        pure $ fromMaybe Projects.defaultProjectCache mpjCache
      let !projectCaches = one (pid, projectCache)
          !metricRecords = convertResourceMetricsToMetricRecords currentTime projectCaches pid resourceMetrics

      Log.logTrace
        "Metrics: Converted resource metrics to MetricRecords"
        (AE.object ["metric_count" AE..= length metricRecords])

      unless (null metricRecords) do
        Telemetry.bulkInsertMetrics (V.fromList metricRecords)
        Log.logTrace
          "Metrics: Successfully inserted metrics into database"
          (AE.object ["inserted_count" AE..= length metricRecords])
    Nothing ->
      -- Return authentication error for gRPC requests with invalid or missing keys
      liftIO
        $ throwIO
        $ GrpcException
          { grpcError = GrpcUnauthenticated
          , grpcErrorMessage = Just "Invalid or missing project API key"
          , grpcErrorMetadata = []
          , grpcErrorDetails = Nothing
          }


-- | Metrics service handler (Export)
metricsServiceExport :: Logger -> AuthContext -> TracerProvider -> Proto MS.ExportMetricsServiceRequest -> IO (Proto MS.ExportMetricsServiceResponse)
metricsServiceExport appLogger appCtx tp (Proto req) = do
  -- Note: This version is for backwards compatibility when called directly in tests
  -- The RpcHandler version below has access to metadata
  _ <- runBackground appLogger appCtx tp $ processMetricsRequest Nothing req
  -- Return an empty response
  pure defMessage


isAesonTextEmpty :: AE.ToJSON a => Maybe (AesonText a) -> Bool
isAesonTextEmpty Nothing = True
isAesonTextEmpty (Just (AesonText v)) =
  case AE.toJSON v of
    AE.Object o -> KEM.null o
    AE.Array arr -> V.null arr
    AE.String t -> T.null t
    AE.Null -> True
    _ -> False


-- | Check if a project's free tier is exceeded using cached data (non-blocking).
-- Returns True if the API key resolves to a free-tier project that has exceeded its daily limit.
isFreeTierExceededCached :: AuthContext -> Maybe Text -> IO Bool
isFreeTierExceededCached appCtx = \case
  Nothing -> pure False
  Just key -> do
    pidM <- join <$> Cache.lookup appCtx.projectKeyCache key
    case pidM of
      Nothing -> pure False
      Just pid -> do
        cacheM <- Cache.lookup appCtx.projectCache pid
        pure $ case cacheM of
          Just pc -> pc.paymentPlan == "Free" && pc.dailyEventCount >= fromInteger freeTierDailyMaxEvents
          Nothing -> False


-- | RpcHandler for trace service with metadata access
traceServiceRpcHandler :: Logger -> AuthContext -> TracerProvider -> RpcHandler IO (Protobuf TS.TraceService "export")
traceServiceRpcHandler appLogger appCtx tp = mkRpcHandler $ \call -> do
  metadata <- getRequestMetadata call
  let apiKey = otlpApiKey metadata
  Proto req <- recvFinalInput call
  exceeded <- isFreeTierExceededCached appCtx apiKey
  if exceeded
    then do
      let count = fromIntegral $ sum [length (ss ^. PTF.spans) | rs <- req ^. TSF.resourceSpans, ss <- rs ^. PTF.scopeSpans]
          resp = defMessage & TSF.partialSuccess .~ (defMessage & TSF.rejectedSpans .~ count & TSF.errorMessage .~ "Free tier daily event limit exceeded")
      sendFinalOutput call (resp, NoMetadata)
    else do
      grpcRunBackground appLogger appCtx tp "traces" $ processTraceRequest apiKey req
      sendFinalOutput call (defMessage, NoMetadata)


-- | RpcHandler for logs service with metadata access
logsServiceRpcHandler :: Logger -> AuthContext -> TracerProvider -> RpcHandler IO (Protobuf LS.LogsService "export")
logsServiceRpcHandler appLogger appCtx tp = mkRpcHandler $ \call -> do
  metadata <- getRequestMetadata call
  let apiKey = otlpApiKey metadata
  Proto req <- recvFinalInput call
  exceeded <- isFreeTierExceededCached appCtx apiKey
  if exceeded
    then do
      let count = fromIntegral $ sum [length (sl ^. PLF.logRecords) | rl <- req ^. LSF.resourceLogs, sl <- rl ^. PLF.scopeLogs]
          resp = defMessage & LSF.partialSuccess .~ (defMessage & LSF.rejectedLogRecords .~ count & LSF.errorMessage .~ "Free tier daily event limit exceeded")
      sendFinalOutput call (resp, NoMetadata)
    else do
      grpcRunBackground appLogger appCtx tp "logs" $ processLogsRequest apiKey req
      sendFinalOutput call (defMessage, NoMetadata)


-- | RpcHandler for metrics service with metadata access
metricsServiceRpcHandler :: Logger -> AuthContext -> TracerProvider -> RpcHandler IO (Protobuf MS.MetricsService "export")
metricsServiceRpcHandler appLogger appCtx tp = mkRpcHandler $ \call -> do
  metadata <- getRequestMetadata call
  let apiKey = otlpApiKey metadata
  Proto req <- recvFinalInput call
  exceeded <- isFreeTierExceededCached appCtx apiKey
  if exceeded
    then do
      let count = fromIntegral $ sum [length (sm ^. PMF.metrics) | rm <- req ^. MSF.resourceMetrics, sm <- rm ^. PMF.scopeMetrics]
          resp = defMessage & MSF.partialSuccess .~ (defMessage & MSF.rejectedDataPoints .~ count & MSF.errorMessage .~ "Free tier daily event limit exceeded")
      sendFinalOutput call (resp, NoMetadata)
    else do
      grpcRunBackground appLogger appCtx tp "metrics" $ processMetricsRequest apiKey req
      sendFinalOutput call (defMessage, NoMetadata)


-- | Run a background task for gRPC handlers, catching exceptions to prevent server crash.
-- Transient DB errors were propagating up and crashing the entire process via waitAnyCancel.
-- Deliberately swallow & log: the OTLP collector treats no-response as retryable, so the
-- client will resend rather than seeing an INTERNAL status.
grpcRunBackground :: Logger -> AuthContext -> TracerProvider -> Text -> ATBackgroundCtx () -> IO ()
grpcRunBackground appLogger appCtx tp label task =
  tryAny (runBackground appLogger appCtx tp task) >>= \case
    Right _ -> pass
    Left e ->
      let kind = if isJust (fromException @Hasql.HasqlException e) then "hasql" else "other" :: Text
       in runLogT "monoscope" appLogger LogAttention
            $ LogBase.logAttention "GRPC_HANDLER_CAUGHT"
            $ AE.object
              [ "error_id" AE..= ("GRPC_HANDLER_CAUGHT" :: Text)
              , "handler" AE..= label
              , "kind" AE..= kind
              , "error" AE..= show @Text e
              ]


services :: Logger -> AuthContext -> TracerProvider -> [SomeRpcHandler IO]
services appLogger appCtx tp =
  fromMethods
    $ RawMethod (traceServiceRpcHandler appLogger appCtx tp :: RpcHandler IO (Protobuf TS.TraceService "export"))
    $ RawMethod (logsServiceRpcHandler appLogger appCtx tp :: RpcHandler IO (Protobuf LS.LogsService "export"))
    $ RawMethod
      (metricsServiceRpcHandler appLogger appCtx tp :: RpcHandler IO (Protobuf MS.MetricsService "export"))
      NoMoreMethods


type instance RequestMetadata (Protobuf TS.TraceService "export") = OtlpRequestMetadata
type instance ResponseInitialMetadata (Protobuf TS.TraceService "export") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf TS.TraceService "export") = NoMetadata


type instance RequestMetadata (Protobuf LS.LogsService "export") = OtlpRequestMetadata
type instance ResponseInitialMetadata (Protobuf LS.LogsService "export") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf LS.LogsService "export") = NoMetadata


type instance RequestMetadata (Protobuf MS.MetricsService "export") = OtlpRequestMetadata
type instance ResponseInitialMetadata (Protobuf MS.MetricsService "export") = NoMetadata
type instance ResponseTrailingMetadata (Protobuf MS.MetricsService "export") = NoMetadata
