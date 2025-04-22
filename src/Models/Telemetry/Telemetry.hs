module Models.Telemetry.Telemetry (
  LogRecord (..),
  logRecordByProjectAndId,
  spanRecordByProjectAndId,
  getSpandRecordsByTraceId,
  convertOtelLogsAndSpansToSpanRecord,
  SpanRecord (..),
  getAllATErrors,
  Trace (..),
  SeverityLevel (..),
  SpanStatus (..),
  SpanKind (..),
  MetricType (..),
  MetricRecord (..),
  ExponentialHistogram (..),
  GaugeSum (..),
  Histogram (..),
  MetricValue (..),
  MetricDataPoint (..),
  MetricChartListData (..),
  Summary (..),
  EHBucket (..),
  Quantile (..),
  OtelLogsAndSpans (..),
  Severity (..),
  Context (..),
  getDataPointsData,
  spanRecordById,
  getTraceDetails,
  getMetricData,
  bulkInsertMetrics,
  bulkInsertOtelLogsAndSpansTF,
  getMetricChartListData,
  getLogsByTraceIds,
  getMetricLabelValues,
  getValsWithPrefix,
  getSpanAttribute,
  getMetricServiceNames,
  getChildSpans,
  SpanEvent (..),
  SpanLink (..),
  convertSpanToRequestMessage,
)
where

import Control.Exception.Annotated (checkpoint)
import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as KEM
import Data.ByteString.Base16 qualified as B16
import Data.Effectful.UUID (UUIDEff, genUUID)
import Data.Generics.Labels ()
import Data.List (nubBy)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (TimeZone (..), UTCTime, formatTime, utcToZonedTime)
import Data.Time.Format (defaultTimeLocale)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (..), executeMany, query, queryOne, withPool)
import Database.PostgreSQL.Simple (Only (..), ResultError (ConversionFailed))
import Database.PostgreSQL.Simple.FromField (Conversion (..), FromField (..), returnError)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query (..))
import Database.PostgreSQL.Transact qualified as DBT
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful
import Effectful.Labeled (Labeled, labeled)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Models.Apis.RequestDumps (RequestDump)
import Models.Projects.Projects (ProjectId (unProjectId))
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pkg.DBUtils (WrappedEnum (..))
import Relude hiding (ask)
import RequestMessages (RequestMessage (..))
import Utils (lookupValueText, toXXHash)


-- Helper function to get nested value from a map using dot notation
getNestedValue :: [Text] -> Map Text AE.Value -> Maybe AE.Value
getNestedValue [] _ = Nothing
getNestedValue [k] m = Map.lookup k m
getNestedValue (k : ks) m = do
  v <- Map.lookup k m
  case v of
    AE.Object obj -> getNestedValue ks (KEM.toMapText obj)
    _ -> Nothing


-- Lens-like access helpers for Map Text AE.Value fields
atMapText :: Text -> Maybe (Map Text AE.Value) -> Maybe Text
atMapText key maybeMap = do
  m <- maybeMap
  val <- getNestedValue (T.split (== '.') key) m
  case val of
    AE.String t -> Just t
    AE.Number n -> Just $ T.pack $ show n
    _ -> Nothing


atMapInt :: Text -> Maybe (Map Text AE.Value) -> Maybe Int
atMapInt key maybeMap = do
  m <- maybeMap
  val <- getNestedValue (T.split (== '.') key) m
  case val of
    AE.Number n -> Just $ round n
    AE.String t -> readMaybe $ T.unpack t
    _ -> Nothing


data SeverityLevel = SLDebug | SLInfo | SLWarn | SLError | SLFatal
  deriving (Show, Generic, Read)
  deriving anyclass (NFData, AE.FromJSON, AE.ToJSON)
  deriving (ToField, FromField) via WrappedEnum "SL" SeverityLevel


data SpanStatus = SSOk | SSError | SSUnset
  deriving (Show, Generic, Read, Eq)
  deriving anyclass (NFData, AE.FromJSON, AE.ToJSON)
  deriving (ToField, FromField) via WrappedEnum "SS" SpanStatus


data SpanKind = SKInternal | SKServer | SKClient | SKProducer | SKConsumer | SKUnspecified
  deriving (Show, Generic, Read)
  deriving anyclass (NFData, AE.FromJSON, AE.ToJSON)
  deriving (ToField, FromField) via WrappedEnum "SK" SpanKind


data Trace = Trace
  { traceId :: Text
  , traceStartTime :: UTCTime
  , traceEndTime :: UTCTime
  , traceDurationNs :: Integer
  , totalSpans :: Int
  , serviceNames :: Maybe (V.Vector Text)
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Trace
  deriving anyclass (NFData, FromRow)


data LogRecord = LogRecord
  { projectId :: UUID.UUID
  , id :: UUID.UUID
  , timestamp :: UTCTime
  , observedTimestamp :: UTCTime
  , traceId :: Text
  , spanId :: Maybe Text
  , severityText :: Maybe SeverityLevel
  , severityNumber :: Int
  , body :: AE.Value
  , attributes :: AE.Value
  , resource :: AE.Value
  , instrumentationScope :: AE.Value
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake LogRecord
  deriving anyclass (NFData, FromRow)


instance AE.FromJSON ByteString where
  parseJSON = AE.withText "ByteString" $ \t ->
    case B16.decode (Relude.encodeUtf8 t) of
      Right bs -> return bs
      Left err -> fail $ "Invalid hex-encoded ByteString: " ++ err


instance AE.ToJSON ByteString where
  toJSON = AE.String . decodeUtf8 . B16.encode


-- Custom FromField instance for Map Text AE.Value
instance FromField (Map Text AE.Value) where
  fromField f mdata = do
    v <- fromField f mdata :: Conversion AE.Value
    case v of
      AE.Object o -> pure $ KEM.toMapText o
      _ -> returnError ConversionFailed f "Expected a JSON object"


-- Custom ToField instance for Map Text AE.Value
instance ToField (Map Text AE.Value) where
  toField = toField . AE.Object . KEM.fromMapText


data SpanRecord = SpanRecord
  { uSpanId :: UUID.UUID
  , projectId :: UUID.UUID
  , timestamp :: UTCTime
  , traceId :: Text
  , spanId :: Text
  , parentSpanId :: Maybe Text
  , traceState :: Maybe Text
  , spanName :: Text
  , startTime :: UTCTime
  , endTime :: Maybe UTCTime
  , kind :: Maybe SpanKind
  , status :: Maybe SpanStatus
  , statusMessage :: Maybe Text
  , attributes :: Maybe (Map Text AE.Value)
  , events :: AE.Value
  , links :: Maybe Text
  , resource :: Maybe (Map Text AE.Value)
  , instrumentationScope :: AE.Value
  , spanDurationNs :: Integer
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake SpanRecord
  deriving anyclass (NFData, FromRow, ToRow)


convertOtelLogsAndSpansToSpanRecord :: OtelLogsAndSpans -> Maybe SpanRecord
convertOtelLogsAndSpansToSpanRecord lgSp = case (trId, spanId, projectId, spanName) of
  (Just tId, Just sId, Just pId, Just sName) ->
    Just
      SpanRecord
        { uSpanId = lgSp.id
        , projectId = pId
        , timestamp = lgSp.timestamp
        , traceId = tId
        , spanId = sId
        , parentSpanId = lgSp.parent_id
        , traceState = lgSp.context >>= (.trace_state)
        , spanName = sName
        , startTime = lgSp.start_time
        , endTime = lgSp.end_time
        , kind = Nothing -- USE actual span kind
        , status = Nothing -- TODO use actual span status
        , statusMessage = lgSp.status_message
        , attributes = lgSp.attributes
        , events = fromMaybe AE.Null lgSp.events
        , links = lgSp.links
        , resource = lgSp.resource
        , instrumentationScope = AE.Null
        , spanDurationNs = maybe 0 fromIntegral lgSp.duration
        }
  _ -> Nothing
  where
    trId = lgSp.context >>= (.trace_id)
    spanId = lgSp.context >>= (.span_id)
    projectId = UUID.fromText lgSp.project_id
    spanName = lgSp.name


data SpanEvent = SpanEvent
  { eventName :: Text
  , eventTime :: UTCTime
  , eventAttributes :: AE.Value
  , eventDroppedAttributesCount :: Int
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake SpanEvent
  deriving anyclass (NFData, FromRow, ToRow)
  deriving (ToField, FromField) via Aeson SpanEvent


data SpanLink = SpanLink
  { linkTraceId :: Text
  , linkSpanId :: Text
  , linkAttributes :: AE.Value
  , linkDroppedAttributesCount :: Int
  , linkFlags :: Int
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake SpanLink
  deriving anyclass (NFData, FromRow, ToRow)
  deriving (ToField, FromField) via Aeson SpanLink


data MetricRecord = MetricRecord
  { id :: Maybe UUID.UUID
  , projectId :: UUID.UUID
  , metricName :: Text
  , metricType :: MetricType
  , metricUnit :: Text
  , metricDescription :: Text
  , metricTime :: UTCTime
  , timestamp :: UTCTime
  , attributes :: AE.Value
  , resource :: AE.Value
  , instrumentationScope :: AE.Value
  , metricValue :: MetricValue
  , metricMetadata :: AE.Value
  , exemplars :: AE.Value
  , flags :: Int
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MetricRecord
  deriving anyclass (FromRow, ToRow, NFData)


data MetricMeta = MetricMeta
  { projectId :: UUID.UUID
  , metricName :: Text
  , metricType :: MetricType
  , metricUnit :: Text
  , metricDescription :: Text
  , serviceName :: Text
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MetricMeta
  deriving anyclass (FromRow, ToRow, NFData)


data MetricValue
  = GaugeValue GaugeSum
  | SumValue GaugeSum
  | HistogramValue Histogram
  | SummaryValue Summary
  | ExponentialHistogramValue ExponentialHistogram
  deriving (Show, Generic)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MetricValue
  deriving (FromField, ToField) via Aeson MetricValue


newtype GaugeSum = GaugeSum
  { value :: Double
  }
  deriving (Show, Generic)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake GaugeSum


data Histogram = Histogram
  { sum :: Double
  , count :: Int
  , bucketCounts :: V.Vector Int
  , explicitBounds :: V.Vector Double
  , pointMin :: Double
  , pointMax :: Double
  }
  deriving (Show, Generic)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON)


data ExponentialHistogram = ExponentialHistogram
  { sum :: Double
  , count :: Int
  , pointMin :: Double
  , pointMax :: Double
  , zeroCount :: Int
  , scale :: Int
  , pointNegative :: Maybe EHBucket
  , pointPositive :: Maybe EHBucket
  , zeroThreshold :: Double
  }
  deriving (Show, Generic)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON)


data EHBucket = EHBucket
  { bucketOffset :: Int
  , bucketCounts :: V.Vector Int
  }
  deriving (Show, Generic)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON)


data Summary = Summary
  { sum :: Double
  , count :: Int
  , quantiles :: V.Vector Quantile
  }
  deriving (Show, Generic)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON)


data Quantile = Quantile
  { quantile :: Double
  , value :: Double
  }
  deriving (Show, Generic)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON)


data Exemplar = Exemplar
  { value :: Double
  , timestamp :: UTCTime
  , attributes :: AE.Value
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON)


data MetricType = MTGauge | MTSum | MTHistogram | MTExponentialHistogram | MTSummary
  deriving (Show, Generic, Read)
  deriving (AE.FromJSON, AE.ToJSON, NFData)
  deriving (ToField, FromField) via WrappedEnum "MT" MetricType


data MetricDataPoint = MetricDataPoint
  { metricName :: Text
  , metricType :: Text
  , metricUnit :: Text
  , metricDescription :: Text
  , dataPointsCount :: Int
  , serviceNames :: V.Vector Text
  , metricLabels :: V.Vector Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)


data MetricChartListData = MetricChartListData
  { metricName :: Text
  , metricType :: Text
  , metricUnit :: Text
  , metricDescription :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)


getTraceDetails :: DB :> es => Projects.ProjectId -> Text -> Eff es (Maybe Trace)
getTraceDetails pid trId = dbtToEff $ queryOne Select q (pid.toText, trId)
  where
    q =
      [sql| SELECT
              context___trace_id,
              MIN(start_time) AS trace_start_time,
              MAX(COALESCE(end_time, start_time)) AS trace_end_time,
              CAST(EXTRACT(EPOCH FROM (MAX(COALESCE(end_time, start_time)) - MIN(start_time))) * 1000000000 AS BIGINT) AS trace_duration_ns,
              COUNT(context->>'span_id') AS total_spans,
              ARRAY_REMOVE(ARRAY_AGG(DISTINCT jsonb_extract_path_text(resource, 'service.name')), NULL) AS service_names
            FROM otel_logs_and_spans
            WHERE  project_id = ? AND context___trace_id = ?
            GROUP BY context___trace_id;
        |]


logRecordByProjectAndId :: DB :> es => Projects.ProjectId -> UTCTime -> UUID.UUID -> Eff es (Maybe OtelLogsAndSpans)
logRecordByProjectAndId pid createdAt rdId = dbtToEff $ queryOne Select q (createdAt, pid.toText, rdId)
  where
    q =
      [sql|SELECT project_id, id, timestamp, observed_timestamp, context, level, severity, body, attributes, resource, 
                  hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, date
             FROM otel_logs_and_spans where (timestamp=?)  and project_id=? and id=? LIMIT 1|]


getSpandRecordsByTraceId :: DB :> es => Projects.ProjectId -> Text -> Eff es (V.Vector OtelLogsAndSpans)
getSpandRecordsByTraceId pid trId = dbtToEff $ query Select q (pid.toText, trId)
  where
    q =
      [sql|
      SELECT project_id, id, timestamp, observed_timestamp, context, level, severity, body, attributes, resource, 
                  hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, date
              FROM otel_logs_and_spans where project_id=? and context___trace_id=? ORDER BY start_time ASC;
    |]


spanRecordByProjectAndId :: DB :> es => Projects.ProjectId -> UTCTime -> UUID.UUID -> Eff es (Maybe OtelLogsAndSpans)
spanRecordByProjectAndId pid createdAt rdId = dbtToEff $ queryOne Select q (createdAt, pid.toText, rdId)
  where
    q =
      [sql| SELECT project_id, id, timestamp, observed_timestamp, context, level, severity, body, attributes, resource, 
                  hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, date
              FROM otel_logs_and_spans where (timestamp=?)  and project_id=? and id=? LIMIT 1|]


spanRecordById :: DB :> es => Projects.ProjectId -> Text -> Text -> Eff es (Maybe OtelLogsAndSpans)
spanRecordById pid trId spanId = dbtToEff $ queryOne Select q (pid.toText, trId, spanId)
  where
    q =
      [sql| SELECT project_id, id, timestamp, observed_timestamp, context, level, severity, body, attributes, resource, 
                  hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, date
              FROM otel_logs_and_spans where project_id=? and context___trace_id = ? and context___span_id=? LIMIT 1|]


getChildSpans :: DB :> es => Projects.ProjectId -> V.Vector Text -> Eff es (V.Vector OtelLogsAndSpans)
getChildSpans pid spanIds = dbtToEff $ query Select q (pid.toText, spanIds)
  where
    q =
      [sql| SELECT project_id, id, timestamp, observed_timestamp, context, level, severity, body, attributes, resource, 
                  hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, date
              FROM otel_logs_and_spans where project_id =? AND parent_id=Any(?)|]


getDataPointsData :: DB :> es => Projects.ProjectId -> (Maybe UTCTime, Maybe UTCTime) -> Eff es (V.Vector MetricDataPoint)
getDataPointsData pid dateRange = dbtToEff $ query Select (Query $ Relude.encodeUtf8 q) (pid, pid)
  where
    dateRangeStr = toText $ case dateRange of
      (Nothing, Just b) -> "AND timestamp BETWEEN NOW() AND '" <> formatTime defaultTimeLocale "%F %R" b <> "'"
      (Just a, Just b) -> "AND timestamp BETWEEN '" <> formatTime defaultTimeLocale "%F %R" a <> "' AND '" <> formatTime defaultTimeLocale "%F %R" b <> "'"
      _ -> ""

    q =
      [text|
WITH metrics_aggregated AS (
    SELECT 
        project_id,
        metric_name,
        COUNT(*) AS data_points
    FROM telemetry.metrics
    WHERE project_id = ? $dateRangeStr
    GROUP BY project_id, metric_name
)
SELECT 
    mm.metric_name,
    mm.metric_type,
    mm.metric_unit,
    mm.metric_description,
    COALESCE(ma.data_points, 0) AS data_points,
    ARRAY_AGG(mm.service_name) AS service_names,
    '{}'::text[] AS labels
FROM telemetry.metrics_meta mm
LEFT JOIN metrics_aggregated ma
    ON mm.project_id = ma.project_id
    AND mm.metric_name = ma.metric_name
WHERE mm.project_id = ?
GROUP BY mm.metric_name, mm.metric_type, mm.metric_unit, mm.metric_description, ma.data_points;
|]


getLogsByTraceIds :: DB :> es => Projects.ProjectId -> V.Vector Text -> Eff es (V.Vector (V.Vector AE.Value))
getLogsByTraceIds pid traceIds = do
  logitems <- queryToValues pid traceIds
  pure $ V.mapMaybe valueToVector logitems


valueToVector :: Only AE.Value -> Maybe (V.Vector AE.Value)
valueToVector (Only val) = case val of
  AE.Array arr -> Just arr
  _ -> Nothing


queryToValues :: DB :> es => Projects.ProjectId -> V.Vector Text -> Eff es (V.Vector (Only AE.Value))
queryToValues pid traceIds = dbtToEff $ V.fromList <$> DBT.query q (pid.toText, traceIds)
  where
    q =
      [sql|
      SELECT json_build_array(id, timestamp, context___trace_id, context___span_id, CAST(EXTRACT(EPOCH FROM (timestamp)) * 1_000_000_000 AS BIGINT), severity___severity_text, body, resource->>'service.name')
      FROM otel_logs_and_spans WHERE project_id = ? AND context___trace_id = ANY(?);
    |]


getMetricData :: DB :> es => Projects.ProjectId -> Text -> Eff es (Maybe MetricDataPoint)
getMetricData pid metricName = dbtToEff $ queryOne Select q (pid, metricName, pid, metricName)
  where
    q =
      [sql|
      SELECT
            metric_name,
            metric_type,
            metric_unit,
            metric_description,
            COUNT(*) AS data_points,
            ARRAY_AGG(DISTINCT COALESCE(resource->>'service.name', 'unknown'))::text[] AS service_names,
    COALESCE(
        (SELECT ARRAY_AGG(DISTINCT key) 
         FROM (
             SELECT DISTINCT jsonb_object_keys(attributes) AS key 
             FROM telemetry.metrics 
             WHERE project_id = ? AND metric_name = ? AND attributes IS NOT NULL
         ) AS unique_keys
        ),
        ARRAY[]::text[]
    ) AS metric_labels      FROM telemetry.metrics
      WHERE project_id = ? AND metric_name = ?
      GROUP BY metric_name, metric_type, metric_unit, metric_description;
        |]


getMetricChartListData :: DB :> es => Projects.ProjectId -> Maybe Text -> Maybe Text -> (Maybe UTCTime, Maybe UTCTime) -> Int -> Eff es (V.Vector MetricChartListData)
getMetricChartListData pid sourceM prefixM dateRange cursor = dbtToEff $ query Select (Query $ Relude.encodeUtf8 q) pid
  where
    dateRangeStr = toText $ case dateRange of
      (Nothing, Just b) -> "AND created_at BETWEEN NOW() AND '" <> formatTime defaultTimeLocale "%F %R" b <> "'"
      (Just a, Just b) -> "AND created_at BETWEEN '" <> formatTime defaultTimeLocale "%F %R" a <> "' AND '" <> formatTime defaultTimeLocale "%F %R" b <> "'"
      _ -> ""
    sourceFilter = case sourceM of
      Nothing -> ""
      Just source -> if source == "" || source == "all" then "" else "AND service_name = '" <> source <> "'"
    prefixFilter = case prefixM of
      Nothing -> ""
      Just prefix -> if prefix == "" || prefix == "all" then "" else "AND metric_name LIKE '" <> prefix <> "%'"
    cursorTxt = show cursor
    q =
      [text|
        SELECT distinct metric_name, metric_type, metric_unit, metric_description
        FROM telemetry.metrics_meta WHERE project_id = ? $sourceFilter $prefixFilter $dateRangeStr OFFSET $cursorTxt LIMIT 20;
     |]


getMetricLabelValues :: DB :> es => Projects.ProjectId -> Text -> Text -> Eff es (V.Vector Text)
getMetricLabelValues pid metricName labelName = dbtToEff $ query Select q (labelName, pid, metricName)
  where
    q = [sql| SELECT DISTINCT attributes->>? FROM telemetry.metrics WHERE project_id = ? AND metric_name = ?|]


getMetricServiceNames :: DB :> es => Projects.ProjectId -> Eff es (V.Vector Text)
getMetricServiceNames pid = dbtToEff $ query Select q pid
  where
    q =
      [sql| SELECT DISTINCT service_name FROM telemetry.metrics_meta WHERE project_id = ?|]


bulkInsertMetrics :: DB :> es => V.Vector MetricRecord -> Eff es ()
bulkInsertMetrics metrics = checkpoint "bulkInsertMetrics" $ do
  void $ dbtToEff $ executeMany Insert q (V.toList rowsToInsert)
  void $ dbtToEff $ executeMany Insert q2 (removeDuplic $ V.toList rows2)
  where
    q =
      [sql|
        INSERT INTO telemetry.metrics
        (project_id, metric_name, metric_type, metric_unit, metric_description, metric_time, timestamp,
         attributes, resource, instrumentation_scope, metric_value, exemplars, flags)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
     |]
    q2 =
      [sql|
       INSERT INTO telemetry.metrics_meta (project_id, metric_name, metric_type, metric_unit, metric_description, service_name) VALUES (?, ?, ?, ?, ?, ?)
       ON CONFLICT (project_id, metric_name, service_name) DO UPDATE SET metric_type = EXCLUDED.metric_type, metric_unit = EXCLUDED.metric_unit, metric_description = EXCLUDED.metric_description
    |]

    rowsToInsert = V.map metricToTuple metrics
    rows2 = V.map (\(o, t, tr, f, fi, _, _, _, res, _, _, _, _) -> (o, t, tr, f, fi, fromMaybe "unknown" $ lookupValueText res "service.name")) rowsToInsert
    metricToTuple entry =
      ( entry.projectId
      , entry.metricName
      , entry.metricType
      , entry.metricUnit
      , entry.metricDescription
      , entry.metricTime
      , entry.timestamp
      , entry.attributes
      , entry.resource
      , entry.instrumentationScope
      , entry.metricValue
      , entry.exemplars
      , entry.flags
      )


-- OtelLogsAndSpans ToRow instance
instance ToRow OtelLogsAndSpans where
  toRow entry =
    [ toField entry.timestamp -- timestamp
    , toField entry.observed_timestamp -- observed_timestamp
    , toField entry.id -- id
    , toField entry.parent_id -- parent_id
    , toField entry.hashes
    , toField entry.name -- name
    , toField entry.kind -- kind
    , toField entry.status_code -- status_code
    , toField entry.status_message -- status_message
    , toField entry.level -- level
    , toField $ fmap AE.toJSON entry.severity -- severity as JSON
    , toField $ parseSeverityText entry.severity -- severity___severity_text
    , toField $ parseSeverityNumber entry.severity -- severity___severity_number
    , toField entry.body -- body as JSON
    , toField entry.duration -- duration
    , toField entry.start_time -- start_time
    , toField entry.end_time -- end_time
    , toField $ fmap AE.toJSON entry.context -- context as JSON
    , toField $ entry.context >>= (.trace_id) -- context___trace_id
    , toField $ entry.context >>= (.span_id) -- context___span_id
    , toField $ entry.context >>= (.trace_state) -- context___trace_state
    , toField $ entry.context >>= (.trace_flags) -- context___trace_flags
    , toField $ entry.context >>= (.is_remote) -- context___is_remote
    , toField entry.events -- events as JSON
    , toField entry.links -- links
    , toField $ fmap AE.Object $ fmap KEM.fromMapText entry.attributes -- attributes as JSON
    , toField $ atMapText "client.address" entry.attributes -- attributes___client___address
    , toField $ atMapInt "client.port" entry.attributes -- attributes___client___port
    , toField $ atMapText "server.address" entry.attributes -- attributes___server___address
    , toField $ atMapInt "server.port" entry.attributes -- attributes___server___port
    , toField $ atMapText "network.local.address" entry.attributes -- attributes___network___local__address
    , toField $ atMapInt "network.local.port" entry.attributes -- attributes___network___local__port
    , toField $ atMapText "network.peer.address" entry.attributes -- attributes___network___peer___address
    , toField $ atMapInt "network.peer.port" entry.attributes -- attributes___network___peer__port
    , toField $ atMapText "network.protocol.name" entry.attributes -- attributes___network___protocol___name
    , toField $ atMapText "network.protocol.version" entry.attributes -- attributes___network___protocol___version
    , toField $ atMapText "network.transport" entry.attributes -- attributes___network___transport
    , toField $ atMapText "network.type" entry.attributes -- attributes___network___type
    , toField $ atMapInt "code.number" entry.attributes -- attributes___code___number
    , toField $ atMapText "code.file.path" entry.attributes -- attributes___code___file___path
    , toField $ atMapText "code.function.name" entry.attributes -- attributes___code___function___name
    , toField $ atMapInt "code.line.number" entry.attributes -- attributes___code___line___number
    , toField $ atMapText "code.stacktrace" entry.attributes -- attributes___code___stacktrace
    , toField $ atMapText "log.record.original" entry.attributes -- attributes___log__record___original
    , toField $ atMapText "log.record.uid" entry.attributes -- attributes___log__record___uid
    , toField $ atMapText "error.type" entry.attributes -- attributes___error___type
    , toField $ atMapText "exception.type" entry.attributes -- attributes___exception___type
    , toField $ atMapText "exception.message" entry.attributes -- attributes___exception___message
    , toField $ atMapText "exception.stacktrace" entry.attributes -- attributes___exception___stacktrace
    , toField $ atMapText "url.fragment" entry.attributes -- attributes___url___fragment
    , toField $ atMapText "url.full" entry.attributes -- attributes___url___full
    , toField $ atMapText "url.path" entry.attributes -- attributes___url___path
    , toField $ atMapText "url.query" entry.attributes -- attributes___url___query
    , toField $ atMapText "url.scheme" entry.attributes -- attributes___url___scheme
    , toField $ atMapText "user_agent.original" entry.attributes -- attributes___user_agent___original
    , toField $ atMapText "http.request.method" entry.attributes -- attributes___http___request___method
    , toField $ atMapText "http.request.method_original" entry.attributes -- attributes___http___request___method_original
    , toField $ atMapInt "http.response.status_code" entry.attributes -- attributes___http___response___status_code
    , toField $ atMapInt "http.request.resend_count" entry.attributes -- attributes___http___request___resend_count
    , toField $ atMapInt "http.request.body.size" entry.attributes -- attributes___http___request___body___size
    , toField $ atMapText "session.id" entry.attributes -- attributes___session___id
    , toField $ atMapText "session.previous.id" entry.attributes -- attributes___session___previous___id
    , toField $ atMapText "db.system.name" entry.attributes -- attributes___db___system___name
    , toField $ atMapText "db.collection.name" entry.attributes -- attributes___db___collection___name
    , toField $ atMapText "db.namespace" entry.attributes -- attributes___db___namespace
    , toField $ atMapText "db.operation.name" entry.attributes -- attributes___db___operation___name
    , toField $ atMapText "db.response.status_code" entry.attributes -- attributes___db___response___status_code
    , toField $ atMapInt "db.operation.batch.size" entry.attributes -- attributes___db___operation___batch___size
    , toField $ atMapText "db.query.summary" entry.attributes -- attributes___db___query___summary
    , toField $ atMapText "db.query.text" entry.attributes -- attributes___db___query___text
    , toField $ atMapText "user.id" entry.attributes -- attributes___user___id
    , toField $ atMapText "user.email" entry.attributes -- attributes___user___email
    , toField $ atMapText "user.full_name" entry.attributes -- attributes___user___full_name
    , toField $ atMapText "user.name" entry.attributes -- attributes___user___name
    , toField $ atMapText "user.hash" entry.attributes -- attributes___user___hash
    , toField $ fmap AE.Object $ fmap KEM.fromMapText entry.resource -- resource as JSON
    , toField $ atMapText "service.name" entry.resource -- resource___service___name
    , toField $ atMapText "service.version" entry.resource -- resource___service___version
    , toField $ atMapText "service.instance.id" entry.resource -- resource___service___instance___id
    , toField $ atMapText "service.namespace" entry.resource -- resource___service___namespace
    , toField $ atMapText "telemetry.sdk.language" entry.resource -- resource___telemetry___sdk___language
    , toField $ atMapText "telemetry.sdk.name" entry.resource -- resource___telemetry___sdk___name
    , toField $ atMapText "telemetry.sdk.version" entry.resource -- resource___telemetry___sdk___version
    , toField $ atMapText "user_agent.original" entry.resource -- resource___user_agent___original
    , toField entry.project_id -- project_id
    , toField entry.date
    ]
    where
      -- Helper functions for severity fields
      parseSeverityText sev = do
        s <- sev
        lvl <- s.severity_text
        pure $ T.pack $ show lvl

      parseSeverityNumber sev = fmap (T.pack . show . severity_number) sev


bulkInsertOtelLogsAndSpansTF :: (DB :> es, Labeled "timefusion" DB :> es, UUIDEff :> es) => V.Vector OtelLogsAndSpans -> Eff es ()
bulkInsertOtelLogsAndSpansTF records = do
  updatedRecords <- updateIds records
  _ <- bulkInsertSpansTS updatedRecords
  _ <- bulkInsertOtelLogsAndSpans updatedRecords
  pure ()
  where
    updateIds :: UUIDEff :> es => V.Vector OtelLogsAndSpans -> Eff es (V.Vector OtelLogsAndSpans)
    updateIds recs = V.mapM updateId recs
      where
        updateId :: UUIDEff :> es => OtelLogsAndSpans -> Eff es OtelLogsAndSpans
        updateId record = do
          newId <- genUUID
          pure $ record & #id .~ newId


bulkInsertSpansTS :: DB :> es => V.Vector OtelLogsAndSpans -> Eff es Int64
bulkInsertSpansTS records = dbtToEff $ executeMany Insert bulkInserSpansAndLogsQuery (V.toList records)


-- Function to insert OtelLogsAndSpans records with all fields in flattened structure
-- Using direct connection without transaction
bulkInsertOtelLogsAndSpans :: Labeled "timefusion" DB :> es => V.Vector OtelLogsAndSpans -> Eff es Int64
-- bulkInsertOtelLogsAndSpansTF :: (IOE :> es, Effectful.Reader.Static.Reader AuthContext :> es) => V.Vector OtelLogsAndSpans -> Eff es Int64
bulkInsertOtelLogsAndSpans records = labeled @"timefusion" @DB $ dbtToEff $ executeMany Insert bulkInserSpansAndLogsQuery (V.toList records)


-- envCfg <- ask @AuthContext
-- -- liftIO $ withResource envCfg.timefusionPgPool \conn -> do
-- --   PG.executeMany conn q (V.toList records)
--
-- liftIO $ do
--   -- Create a direct connection instead of using the pool
--   -- postgresql://postgres:postgres@localhost:12345/postgres
--   conn <- DBUtils.connectPostgreSQL "postgresql://postgres:postgres@localhost:12345/postgres"
--   result <- PG.executeMany conn q (V.toList records)
--   PG.close conn
--   return result

bulkInserSpansAndLogsQuery :: Query
bulkInserSpansAndLogsQuery =
  [sql| INSERT INTO otel_logs_and_spans
      (timestamp, observed_timestamp, id, parent_id, hashes, name, kind, status_code, status_message, 
       level, severity, severity___severity_text, severity___severity_number, body, duration, 
       start_time, end_time, context, context___trace_id, context___span_id, context___trace_state, 
       context___trace_flags, context___is_remote, events, links, attributes, 
       attributes___client___address, attributes___client___port, attributes___server___address,
       attributes___server___port, attributes___network___local__address, attributes___network___local__port,
       attributes___network___peer___address, attributes___network___peer__port, 
       attributes___network___protocol___name, attributes___network___protocol___version,
       attributes___network___transport, attributes___network___type, attributes___code___number,
       attributes___code___file___path, attributes___code___function___name, attributes___code___line___number,
       attributes___code___stacktrace, attributes___log__record___original, attributes___log__record___uid,
       attributes___error___type, attributes___exception___type, attributes___exception___message,
       attributes___exception___stacktrace, attributes___url___fragment, attributes___url___full,
       attributes___url___path, attributes___url___query, attributes___url___scheme,
       attributes___user_agent___original, attributes___http___request___method,
       attributes___http___request___method_original, attributes___http___response___status_code,
       attributes___http___request___resend_count, attributes___http___request___body___size,
       attributes___session___id, attributes___session___previous___id, attributes___db___system___name,
       attributes___db___collection___name, attributes___db___namespace, attributes___db___operation___name,
       attributes___db___response___status_code, attributes___db___operation___batch___size,
       attributes___db___query___summary, attributes___db___query___text, attributes___user___id,
       attributes___user___email, attributes___user___full_name, attributes___user___name,
       attributes___user___hash, resource, resource___service___name, resource___service___version,
       resource___service___instance___id, resource___service___namespace, 
       resource___telemetry___sdk___language, resource___telemetry___sdk___name,
       resource___telemetry___sdk___version, resource___user_agent___original,
       project_id, date)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
              ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
              ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    |]


removeDuplic :: Eq a => Eq e => [(a, e, b, c, d, q)] -> [(a, e, b, c, d, q)]
removeDuplic = nubBy (\(a1, a2, _, _, _, _) (b1, b2, _, _, _, _) -> a1 == b1 && a2 == b2)


convertSpanToRequestMessage :: OtelLogsAndSpans -> Text -> Maybe RequestMessage
convertSpanToRequestMessage sp instrumentationScope =
  pidM >>= \pid ->
    Just
      RequestMessage
        { duration = fromIntegral $ fromMaybe 0 sp.duration
        , host = host
        , method = method
        , pathParams = pathParams
        , projectId = pid
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
    pidM = UUID.fromText sp.project_id
    attrJson = fromMaybe AE.Null $ fmap AE.Object $ fmap KEM.fromMapText sp.attributes
    host = getSpanAttribute "net.host.name" attrJson
    method = fromMaybe (fromMaybe "GET" $ getSpanAttribute "http.request.method" attrJson) $ getSpanAttribute "http.method" attrJson
    pathParams = fromMaybe (AE.object []) (AE.decode $ Relude.encodeUtf8 $ fromMaybe "" $ getSpanAttribute "http.request.path_params" attrJson)
    queryParams = fromMaybe (AE.object []) (AE.decode $ Relude.encodeUtf8 $ fromMaybe "" $ getSpanAttribute "http.request.query_params" attrJson)
    errors = AE.decode $ Relude.encodeUtf8 $ fromMaybe "" $ getSpanAttribute "apitoolkit.errors" attrJson
    messageId = UUID.fromText $ fromMaybe "" $ getSpanAttribute "apitoolkit.msg_id" attrJson
    parentId = UUID.fromText $ fromMaybe "" $ getSpanAttribute "apitoolkit.parent_id" attrJson
    referer = Just $ Left (fromMaybe "" $ getSpanAttribute "http.request.headers.referer" attrJson) :: Maybe (Either Text [Text])
    requestBody = fromMaybe "{}" $ getSpanAttribute "http.request.body" attrJson
    responseBody = fromMaybe "{}" $ getSpanAttribute "http.response.body" attrJson
    (requestHeaders, responseHeaders) = case attrJson of
      AE.Object v -> (getValsWithPrefix "http.request.header." v, getValsWithPrefix "http.response.header." v)
      _ -> (AE.object [], AE.object [])
    responseStatus = (readMaybe . toString =<< getSpanAttribute "http.response.status_code" attrJson) :: Maybe Double
    responseStatus' = (readMaybe . toString =<< getSpanAttribute "http.status_code" attrJson) :: Maybe Double
    status = round $ fromMaybe (fromMaybe 0.0 responseStatus') responseStatus
    sdkType = RequestDumps.parseSDKType $ fromMaybe "" $ getSpanAttribute "apitoolkit.sdk_type" attrJson
    urlPath' = getSpanAttribute "http.route" attrJson
    undUrlPath = getSpanAttribute "url.path" attrJson
    urlPath = if instrumentationScope == "@opentelemetry/instrumentation-undici" then undUrlPath else urlPath'
    rawUrl' = fromMaybe "" $ getSpanAttribute "http.target" attrJson
    rawUrl =
      if instrumentationScope == "@opentelemetry/instrumentation-undici"
        then fromMaybe "" undUrlPath <> fromMaybe "" (getSpanAttribute "url.query" attrJson)
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


data Severity = Severity
  { severity_text :: Maybe SeverityLevel
  , severity_number :: Int
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Severity
  deriving anyclass (NFData, FromRow, ToRow)
  deriving (ToField, FromField) via Aeson Severity


data Context = Context
  { trace_id :: Maybe Text
  , span_id :: Maybe Text
  , trace_state :: Maybe Text
  , trace_flags :: Maybe Text
  , is_remote :: Maybe Text
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Context
  deriving anyclass (NFData, FromRow, ToRow)
  deriving (ToField, FromField) via Aeson Context


data OtelLogsAndSpans = OtelLogsAndSpans
  { project_id :: Text
  , id :: UUID.UUID
  , timestamp :: UTCTime
  , observed_timestamp :: Maybe UTCTime
  , context :: Maybe Context
  , level :: Maybe Text
  , severity :: Maybe Severity
  , body :: Maybe AE.Value
  , attributes :: Maybe (Map Text AE.Value)
  , resource :: Maybe (Map Text AE.Value)
  , hashes :: V.Vector Text
  , kind :: Maybe Text
  , status_code :: Maybe Text
  , status_message :: Maybe Text
  , start_time :: UTCTime
  , end_time :: Maybe UTCTime
  , events :: Maybe AE.Value
  , links :: Maybe Text
  , duration :: Maybe Int64
  , name :: Maybe Text
  , parent_id :: Maybe Text
  , date :: UTCTime
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake OtelLogsAndSpans
  deriving anyclass (NFData, FromRow)


getErrorEvents :: OtelLogsAndSpans -> V.Vector AE.Value
getErrorEvents OtelLogsAndSpans{events = Just (AE.Array arr)} =
  V.filter isErrorEvent arr
  where
    isErrorEvent (AE.Object o) =
      case KEM.lookup "event_name" o of
        Just (AE.String name) -> "exception" `T.isInfixOf` name || "error" `T.isInfixOf` name
        _ -> False
    isErrorEvent _ = False
getErrorEvents _ = []


getAllATErrors :: V.Vector OtelLogsAndSpans -> V.Vector RequestDumps.ATError
getAllATErrors = V.concatMap extractErrorsFromSpan
  where
    extractErrorsFromSpan spanObj =
      let events = getErrorEvents spanObj
       in V.mapMaybe (extractATError spanObj) events


extractATError :: OtelLogsAndSpans -> AE.Value -> Maybe RequestDumps.ATError
extractATError spanObj (AE.Object o) = do
  AE.Object attrs <- KEM.lookup "event_attributes" o
  let lookupText k = case KEM.lookup k attrs of
        Just (AE.String s) -> Just s
        _ -> Nothing
      getTextOrEmpty k = fromMaybe "" (lookupText k)

      typ = getTextOrEmpty "exception.type"
      msg = getTextOrEmpty "exception.message"
      stack = getTextOrEmpty "exception.stacktrace"

      -- TODO: parse telemetry.sdk.name to SDKTypes
      tech = case spanObj.resource >>= Map.lookup "telemetry.sdk.name" of
        Just (AE.String sdk) -> Nothing
        _ -> Nothing
      serviceName = spanObj.resource >>= Map.lookup "service.name" >>= asText

      spanId = spanObj.context >>= (.span_id)

      asText (AE.String t) = Just t
      asText _ = Nothing

  return $
    RequestDumps.ATError
      { projectId = UUID.fromText spanObj.project_id >>= (\uid -> Just Projects.ProjectId{unProjectId = uid})
      , when = spanObj.timestamp
      , errorType = typ
      , rootErrorType = typ
      , message = msg
      , rootErrorMessage = msg
      , stackTrace = stack
      , hash = Just $ (toXXHash (spanObj.project_id <> fromMaybe "" serviceName <> fromMaybe "" spanObj.name <> typ <> msg))
      , technology = tech
      , requestMethod = Just "Span Id"
      , requestPath = spanId
      }
extractATError _ _ = Nothing
