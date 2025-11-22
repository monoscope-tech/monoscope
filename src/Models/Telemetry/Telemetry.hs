{-# LANGUAGE StrictData #-}

module Models.Telemetry.Telemetry (
  LogRecord (..),
  logRecordByProjectAndId,
  spanRecordByProjectAndId,
  getSpanRecordsByTraceId,
  getSpanRecordsByTraceIds,
  convertOtelLogsAndSpansToSpanRecord,
  getTotalEventsToReport,
  SpanRecord (..),
  getAllATErrors,
  getProjectStatsForReport,
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
  spanRecordByName,
  getTraceDetails,
  getTotalMetricsCount,
  getMetricData,
  bulkInsertMetrics,
  bulkInsertOtelLogsAndSpansTF,
  getMetricChartListData,
  getLogsByTraceIds,
  getMetricLabelValues,
  getTraceShapes,
  getValsWithPrefix,
  getSpanAttribute,
  getMetricServiceNames,
  SpanEvent (..),
  SpanLink (..),
  atMapText,
  atMapInt,
  getProjectStatsBySpanType,
  getEndpointStats,
  getDBQueryStats,
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
import Data.List qualified as L (nubBy)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Display (Display)
import Data.Time (UTCTime, formatTime)
import Data.Time.Clock (addUTCTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (executeMany, query, queryOne)
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
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Labeled (Labeled, labeled)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Reader.Static qualified as Eff
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects (ProjectId (unProjectId))
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pkg.DBUtils (WrappedEnum (..), WrappedEnumSC (..))
import Pkg.DeriveUtils (AesonText (..), unAesonTextMaybe)
import Relude hiding (ask)
import RequestMessages (replaceAllFormats)
import System.Config (AuthContext)
import System.Config qualified as SysConfig
import System.Logging qualified as Log
import Text.Regex.TDFA.Text ()
import UnliftIO (throwIO, tryAny)
import Utils (formatUTC, lookupValueText, toXXHash)


-- Helper function to get nested value from a map using dot notation
getNestedValue :: [Text] -> Map Text AE.Value -> Maybe AE.Value
getNestedValue [] _ = Nothing
getNestedValue [k] m = Map.lookup k m
getNestedValue (k : ks) m = do
  v <- Map.lookup k m
  case v of
    AE.Object obj -> getNestedValue ks (KEM.toMapText obj)
    _ -> Nothing


-- Helper function to clean null bytes from text
cleanNullBytes :: Text -> Text
cleanNullBytes !t =
  if T.any (== '\NUL') t
    then T.filter (/= '\NUL') t
    else t


-- Helper function to clean null bytes from JSON values
cleanNullBytesFromJSON :: AE.Value -> AE.Value
cleanNullBytesFromJSON (AE.String t) = AE.String $ cleanNullBytes t
cleanNullBytesFromJSON (AE.Object o) = AE.Object $ KEM.map cleanNullBytesFromJSON o
cleanNullBytesFromJSON (AE.Array a) = AE.Array $ V.map cleanNullBytesFromJSON a
cleanNullBytesFromJSON v = v


-- Lens-like access helpers for Map Text AE.Value fields
atMapText :: Text -> Maybe (Map Text AE.Value) -> Maybe Text
atMapText key maybeMap = do
  m <- maybeMap
  val <- getNestedValue (T.split (== '.') key) m
  case val of
    AE.String t -> Just $ cleanNullBytes t
    AE.Number n -> Just $ show n
    _ -> Nothing


atMapInt :: Text -> Maybe (Map Text AE.Value) -> Maybe Int
atMapInt key maybeMap = do
  m <- maybeMap
  val <- getNestedValue (T.split (== '.') key) m
  case val of
    AE.Number n -> Just $ round n
    AE.String t -> readMaybe $ toString t
    _ -> Nothing


data SeverityLevel = SLDebug | SLInfo | SLWarn | SLError | SLFatal
  deriving (Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "SL", DAE.CamelToSnake]] SeverityLevel
  deriving (Display, FromField, ToField) via WrappedEnumSC "SL" SeverityLevel


data SpanStatus = SSOk | SSError | SSUnset
  deriving (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "SS", DAE.CamelToSnake]] SpanStatus
  deriving (Display, FromField, ToField) via WrappedEnumSC "SS" SpanStatus


data SpanKind = SKInternal | SKServer | SKClient | SKProducer | SKConsumer | SKUnspecified
  deriving (Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "SK", DAE.CamelToSnake]] SpanKind
  deriving (Display, FromField, ToField) via WrappedEnumSC "SK" SpanKind


data Trace = Trace
  { traceId :: Text
  , traceStartTime :: UTCTime
  , traceEndTime :: UTCTime
  , traceDurationNs :: Integer
  , totalSpans :: Int
  , serviceNames :: Maybe (V.Vector Text)
  }
  deriving (Generic, Show)
  deriving anyclass (FromRow, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Trace


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
  deriving (Generic, Show)
  deriving anyclass (FromRow, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake LogRecord


instance AE.FromJSON ByteString where
  parseJSON = AE.withText "ByteString" $ \t ->
    case B16.decode (Relude.encodeUtf8 t) of
      Right bs -> return bs
      Left err -> fail $ "Invalid hex-encoded ByteString: " ++ err


instance AE.ToJSON ByteString where
  toJSON = AE.String . decodeUtf8 . B16.encode


-- Custom FromField instance for Map Text AE.Value that handles both JSONB and varchar/text
instance FromField (Map Text AE.Value) where
  fromField f mdata = do
    -- Try to parse as JSONB first
    let tryJsonb = do
          v <- fromField f mdata :: Conversion AE.Value
          case v of
            AE.Object o -> pure $ KEM.toMapText o
            _ -> returnError ConversionFailed f "Expected a JSON object"
    -- If that fails, try parsing as text/varchar
    let tryText = do
          txt <- fromField f mdata :: Conversion Text
          case AE.eitherDecodeStrict (encodeUtf8 txt) of
            Right (AE.Object o) -> return $ KEM.toMapText o
            Right _ -> returnError ConversionFailed f "Expected a JSON object"
            Left err -> returnError ConversionFailed f ("Failed to parse JSON from text: " ++ err)
    tryJsonb <|> tryText


-- Custom ToField instance for Map Text AE.Value
instance ToField (Map Text AE.Value) where
  toField = toField . AE.Object . KEM.fromMapText


data SpanRecord = SpanRecord
  { uSpanId :: Text
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
  deriving (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake SpanRecord


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
        , attributes = unAesonTextMaybe lgSp.attributes
        , events = fromMaybe AE.Null (unAesonTextMaybe lgSp.events)
        , links = lgSp.links
        , resource = unAesonTextMaybe lgSp.resource
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
  deriving (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (FromField, ToField) via Aeson SpanEvent
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake SpanEvent


data SpanLink = SpanLink
  { linkTraceId :: Text
  , linkSpanId :: Text
  , linkAttributes :: AE.Value
  , linkDroppedAttributesCount :: Int
  , linkFlags :: Int
  }
  deriving (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (FromField, ToField) via Aeson SpanLink
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake SpanLink


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
  deriving (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MetricRecord


data MetricMeta = MetricMeta
  { projectId :: UUID.UUID
  , metricName :: Text
  , metricType :: MetricType
  , metricUnit :: Text
  , metricDescription :: Text
  , serviceName :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MetricMeta


data MetricValue
  = GaugeValue GaugeSum
  | SumValue GaugeSum
  | HistogramValue Histogram
  | SummaryValue Summary
  | ExponentialHistogramValue ExponentialHistogram
  deriving (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson MetricValue
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MetricValue


newtype GaugeSum = GaugeSum
  { value :: Double
  }
  deriving (Generic, Show)
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
  deriving (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON)
  deriving anyclass (NFData)


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
  deriving (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON)
  deriving anyclass (NFData)


data EHBucket = EHBucket
  { bucketOffset :: Int
  , bucketCounts :: V.Vector Int
  }
  deriving (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON)
  deriving anyclass (NFData)


data Summary = Summary
  { sum :: Double
  , count :: Int
  , quantiles :: V.Vector Quantile
  }
  deriving (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON)
  deriving anyclass (NFData)


data Quantile = Quantile
  { quantile :: Double
  , value :: Double
  }
  deriving (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON)
  deriving anyclass (NFData)


data Exemplar = Exemplar
  { value :: Double
  , timestamp :: UTCTime
  , attributes :: AE.Value
  }
  deriving (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON)


data MetricType = MTGauge | MTSum | MTHistogram | MTExponentialHistogram | MTSummary
  deriving (Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, NFData)
  deriving (FromField, ToField) via WrappedEnum "MT" MetricType


data MetricDataPoint = MetricDataPoint
  { metricName :: Text
  , metricType :: Text
  , metricUnit :: Text
  , metricDescription :: Text
  , dataPointsCount :: Int
  , serviceNames :: V.Vector Text
  , metricLabels :: V.Vector Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)


data MetricChartListData = MetricChartListData
  { metricName :: Text
  , metricType :: Text
  , metricUnit :: Text
  , metricDescription :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)


getTraceDetails :: DB :> es => Projects.ProjectId -> Text -> Maybe UTCTime -> Eff es (Maybe Trace)
getTraceDetails pid trId tme = dbtToEff $ queryOne (Query $ encodeUtf8 q) (pid.toText, trId)
  where
    (startTime, endTime) = case tme of
      Nothing -> ("now() - interval '1 day'", "now()")
      Just ts -> ("'" <> (formatUTC $ addUTCTime (-60 * 5) ts) <> "'", "'" <> (formatUTC $ addUTCTime (60 * 5) ts) <> "'")
    q = do
      [text| SELECT
              context___trace_id,
              MIN(start_time) AS trace_start_time,
              MAX(COALESCE(end_time, start_time)) AS trace_end_time,
              CAST(EXTRACT(EPOCH FROM (MAX(COALESCE(end_time, start_time)) - MIN(start_time))) * 1000000000 AS BIGINT) AS trace_duration_ns,
              COUNT(context->>'span_id') AS total_spans,
              ARRAY_REMOVE(ARRAY_AGG(DISTINCT jsonb_extract_path_text(resource, 'service.name')), NULL) AS service_names
            FROM otel_logs_and_spans
            WHERE  project_id = ? AND timestamp BETWEEN $startTime AND $endTime AND context___trace_id = ?
            GROUP BY context___trace_id;
        |]


logRecordByProjectAndId :: DB :> es => Projects.ProjectId -> UTCTime -> UUID.UUID -> Eff es (Maybe OtelLogsAndSpans)
logRecordByProjectAndId pid createdAt rdId = dbtToEff $ queryOne q (createdAt, pid.toText, rdId)
  where
    q =
      [sql|SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity, body, attributes, resource, 
                  hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, summary, date::timestamptz
             FROM otel_logs_and_spans where (timestamp=?)  and project_id=? and id=? LIMIT 1|]


getSpanRecordsByTraceId :: DB :> es => Projects.ProjectId -> Text -> Maybe UTCTime -> Eff es (V.Vector OtelLogsAndSpans)
getSpanRecordsByTraceId pid trId tme = do
  dbtToEff $ query (Query $ encodeUtf8 q) (pid.toText, trId)
  where
    (start, end) = case tme of
      Nothing -> ("now() - interval '1 day'", "now()")
      Just ts -> ("'" <> (formatUTC $ addUTCTime (-60 * 5) ts) <> "'", "'" <> (formatUTC $ addUTCTime (60 * 5) ts) <> "'")
    q =
      [text|
      SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity, body, attributes, resource, 
                  hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, summary, date::timestamptz
              FROM otel_logs_and_spans where project_id=? and timestamp >= $start AND timestamp <= $end and context___trace_id=? ORDER BY start_time ASC;
    |]


getSpanRecordsByTraceIds :: DB :> es => Projects.ProjectId -> V.Vector Text -> Maybe UTCTime -> Eff es (V.Vector OtelLogsAndSpans)
getSpanRecordsByTraceIds pid traceIds tme = do
  dbtToEff $ query (Query $ encodeUtf8 q) (pid.toText, traceIds)
  where
    (start, end) =
      case tme of
        Nothing -> ("now() - interval '1 day'", "now()")
        Just ts -> ("'" <> toText (iso8601Show $ addUTCTime (-300) ts) <> "'", "'" <> toText (iso8601Show $ addUTCTime 300 ts) <> "'")
    q =
      [text|
        SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity,
               body, attributes, resource, hashes, kind, status_code, status_message,
               start_time, end_time, events, links, duration, name,
               parent_id, summary, date::timestamptz
        FROM otel_logs_and_spans 
        WHERE project_id = ?
          AND timestamp >= $start
          AND timestamp <= $end
          AND context___trace_id = ANY(?)
        ORDER BY context___trace_id ASC, start_time ASC;
      |]


spanRecordByProjectAndId :: DB :> es => Projects.ProjectId -> UTCTime -> UUID.UUID -> Eff es (Maybe OtelLogsAndSpans)
spanRecordByProjectAndId pid createdAt rdId = dbtToEff $ queryOne q (createdAt, pid.toText, rdId)
  where
    q =
      [sql| SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity, body, attributes, resource, 
                  hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, summary, date::timestamptz
              FROM otel_logs_and_spans where (timestamp=?)  and project_id=? and id=? LIMIT 1|]


spanRecordByName :: DB :> es => Projects.ProjectId -> Text -> Text -> Eff es (Maybe OtelLogsAndSpans)
spanRecordByName pid trId spanName = dbtToEff $ queryOne q (pid.toText, trId, spanName)
  where
    q =
      [sql| SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity, body, attributes, resource, 
                  hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, summary, date::timestamptz
              FROM otel_logs_and_spans where project_id=? and context___trace_id = ? and name=? LIMIT 1|]


getDataPointsData :: DB :> es => Projects.ProjectId -> (Maybe UTCTime, Maybe UTCTime) -> Eff es (V.Vector MetricDataPoint)
getDataPointsData pid dateRange = dbtToEff $ query (Query $ Relude.encodeUtf8 q) (pid, pid)
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
queryToValues pid traceIds
  | V.null traceIds = pure V.empty
  | otherwise = dbtToEff $ V.fromList <$> DBT.query q (pid.toText, traceIds)
  where
    q =
      [sql|
      SELECT json_build_array(id, timestamp, context___trace_id, context___span_id, CAST(EXTRACT(EPOCH FROM (timestamp)) * 1_000_000_000 AS BIGINT), severity___severity_text, body, resource->>'service.name')
      FROM otel_logs_and_spans WHERE project_id = ? AND context___trace_id = ANY(?) and attributes___session___id is null;
    |]


getMetricData :: DB :> es => Projects.ProjectId -> Text -> Eff es (Maybe MetricDataPoint)
getMetricData pid metricName = dbtToEff $ queryOne q (pid, metricName, pid, metricName)
  where
    q =
      [sql|
      SELECT
            mm.metric_name,
            mm.metric_type,
            mm.metric_unit,
            mm.metric_description,
            COALESCE(m.data_points, 0) AS data_points,
            COALESCE(m.service_names, ARRAY[mm.service_name]::text[]) AS service_names,
            COALESCE(m.metric_labels, ARRAY[]::text[]) AS metric_labels
      FROM telemetry.metrics_meta mm
      LEFT JOIN LATERAL (
          SELECT
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
              ) AS metric_labels
          FROM telemetry.metrics
          WHERE project_id = mm.project_id AND metric_name = mm.metric_name
      ) m ON true
      WHERE mm.project_id = ? AND mm.metric_name = ?
      LIMIT 1;
        |]


getTotalEventsToReport :: DB :> es => Projects.ProjectId -> UTCTime -> Eff es Int
getTotalEventsToReport pid lastReported = do
  result <- dbtToEff $ query q (pid, lastReported)
  case result of
    [Only c] -> return c
    v -> return $ length v
  where
    q =
      [sql| SELECT count(*) FROM otel_logs_and_spans WHERE project_id=? AND timestamp > ?|]


getTotalMetricsCount :: DB :> es => Projects.ProjectId -> UTCTime -> Eff es Int
getTotalMetricsCount pid lastReported = do
  result <- dbtToEff $ query q (pid, lastReported)
  case result of
    [Only c] -> return c
    v -> return $ length v
  where
    q =
      [sql| SELECT count(*) FROM telemetry.metrics WHERE project_id=? AND timestamp > ?|]


getMetricChartListData :: DB :> es => Projects.ProjectId -> Maybe Text -> Maybe Text -> (Maybe UTCTime, Maybe UTCTime) -> Int -> Eff es (V.Vector MetricChartListData)
getMetricChartListData pid sourceM prefixM dateRange cursor = dbtToEff $ query (Query $ Relude.encodeUtf8 q) pid
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
getMetricLabelValues pid metricName labelName = dbtToEff $ query q (labelName, pid, metricName)
  where
    q = [sql| SELECT DISTINCT attributes->>? FROM telemetry.metrics WHERE project_id = ? AND metric_name = ?|]


getMetricServiceNames :: DB :> es => Projects.ProjectId -> Eff es (V.Vector Text)
getMetricServiceNames pid = dbtToEff $ query q pid
  where
    q =
      [sql| SELECT DISTINCT service_name FROM telemetry.metrics_meta WHERE project_id = ?|]


bulkInsertMetrics :: DB :> es => V.Vector MetricRecord -> Eff es ()
bulkInsertMetrics metrics = checkpoint "bulkInsertMetrics" $ do
  void $ dbtToEff $ executeMany q (V.toList rowsToInsert)
  void $ dbtToEff $ executeMany q2 (removeDuplic $ V.toList rows2)
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
    , toField $ fmap cleanNullBytes entry.parent_id -- parent_id
    , toField $ V.map cleanNullBytes entry.hashes
    , toField $ fmap cleanNullBytes entry.name -- name
    , toField $ fmap cleanNullBytes entry.kind -- kind
    , toField $ fmap cleanNullBytes entry.status_code -- status_code
    , toField $ fmap cleanNullBytes entry.status_message -- status_message
    , toField $ fmap cleanNullBytes entry.level -- level
    , toField $ fmap AE.toJSON entry.severity -- severity as JSON
    , toField $ parseSeverityText entry.severity -- severity___severity_text
    , toField $ parseSeverityNumber entry.severity -- severity___severity_number
    , toField $ fmap cleanNullBytesFromJSON (unAesonTextMaybe entry.body) -- body as JSON
    , toField entry.duration -- duration
    , toField entry.start_time -- start_time
    , toField entry.end_time -- end_time
    , toField $ fmap (cleanNullBytesFromJSON . AE.toJSON) entry.context -- context as JSON
    , toField $ fmap cleanNullBytes $ entry.context >>= (.trace_id) -- context___trace_id
    , toField $ fmap cleanNullBytes $ entry.context >>= (.span_id) -- context___span_id
    , toField $ fmap cleanNullBytes $ entry.context >>= (.trace_state) -- context___trace_state
    , toField $ fmap cleanNullBytes $ entry.context >>= (.trace_flags) -- context___trace_flags
    , toField $ fmap cleanNullBytes $ entry.context >>= (.is_remote) -- context___is_remote
    , toField $ fmap cleanNullBytesFromJSON (unAesonTextMaybe entry.events) -- events as JSON
    , toField $ fmap cleanNullBytes entry.links -- links
    , toField $ fmap (cleanNullBytesFromJSON . AE.Object . KEM.fromMapText) (unAesonTextMaybe entry.attributes) -- attributes as JSON
    , toField $ atMapText "client.address" (unAesonTextMaybe entry.attributes) -- attributes___client___address
    , toField $ atMapInt "client.port" (unAesonTextMaybe entry.attributes) -- attributes___client___port
    , toField $ atMapText "server.address" (unAesonTextMaybe entry.attributes) -- attributes___server___address
    , toField $ atMapInt "server.port" (unAesonTextMaybe entry.attributes) -- attributes___server___port
    , toField $ atMapText "network.local.address" (unAesonTextMaybe entry.attributes) -- attributes___network___local__address
    , toField $ atMapInt "network.local.port" (unAesonTextMaybe entry.attributes) -- attributes___network___local__port
    , toField $ atMapText "network.peer.address" (unAesonTextMaybe entry.attributes) -- attributes___network___peer___address
    , toField $ atMapInt "network.peer.port" (unAesonTextMaybe entry.attributes) -- attributes___network___peer__port
    , toField $ atMapText "network.protocol.name" (unAesonTextMaybe entry.attributes) -- attributes___network___protocol___name
    , toField $ atMapText "network.protocol.version" (unAesonTextMaybe entry.attributes) -- attributes___network___protocol___version
    , toField $ atMapText "network.transport" (unAesonTextMaybe entry.attributes) -- attributes___network___transport
    , toField $ atMapText "network.type" (unAesonTextMaybe entry.attributes) -- attributes___network___type
    , toField $ atMapInt "code.number" (unAesonTextMaybe entry.attributes) -- attributes___code___number
    , toField $ atMapText "code.file.path" (unAesonTextMaybe entry.attributes) -- attributes___code___file___path
    , toField $ atMapText "code.function.name" (unAesonTextMaybe entry.attributes) -- attributes___code___function___name
    , toField $ atMapInt "code.line.number" (unAesonTextMaybe entry.attributes) -- attributes___code___line___number
    , toField $ atMapText "code.stacktrace" (unAesonTextMaybe entry.attributes) -- attributes___code___stacktrace
    , toField $ atMapText "log.record.original" (unAesonTextMaybe entry.attributes) -- attributes___log__record___original
    , toField $ atMapText "log.record.uid" (unAesonTextMaybe entry.attributes) -- attributes___log__record___uid
    , toField $ atMapText "error.type" (unAesonTextMaybe entry.attributes) -- attributes___error___type
    , toField $ atMapText "exception.type" (unAesonTextMaybe entry.attributes) -- attributes___exception___type
    , toField $ atMapText "exception.message" (unAesonTextMaybe entry.attributes) -- attributes___exception___message
    , toField $ atMapText "exception.stacktrace" (unAesonTextMaybe entry.attributes) -- attributes___exception___stacktrace
    , toField $ atMapText "url.fragment" (unAesonTextMaybe entry.attributes) -- attributes___url___fragment
    , toField $ atMapText "url.full" (unAesonTextMaybe entry.attributes) -- attributes___url___full
    , toField $ atMapText "url.path" (unAesonTextMaybe entry.attributes) -- attributes___url___path
    , toField $ atMapText "url.query" (unAesonTextMaybe entry.attributes) -- attributes___url___query
    , toField $ atMapText "url.scheme" (unAesonTextMaybe entry.attributes) -- attributes___url___scheme
    , toField $ atMapText "user_agent.original" (unAesonTextMaybe entry.attributes) -- attributes___user_agent___original
    , toField $ atMapText "http.request.method" (unAesonTextMaybe entry.attributes) -- attributes___http___request___method
    , toField $ atMapText "http.request.method_original" (unAesonTextMaybe entry.attributes) -- attributes___http___request___method_original
    , toField $ atMapInt "http.response.status_code" (unAesonTextMaybe entry.attributes) -- attributes___http___response___status_code
    , toField $ atMapInt "http.request.resend_count" (unAesonTextMaybe entry.attributes) -- attributes___http___request___resend_count
    , toField $ atMapInt "http.request.body.size" (unAesonTextMaybe entry.attributes) -- attributes___http___request___body___size
    , toField $ atMapText "session.id" (unAesonTextMaybe entry.attributes) -- attributes___session___id
    , toField $ atMapText "session.previous.id" (unAesonTextMaybe entry.attributes) -- attributes___session___previous___id
    , toField $ atMapText "db.system.name" (unAesonTextMaybe entry.attributes) -- attributes___db___system___name
    , toField $ atMapText "db.collection.name" (unAesonTextMaybe entry.attributes) -- attributes___db___collection___name
    , toField $ atMapText "db.namespace" (unAesonTextMaybe entry.attributes) -- attributes___db___namespace
    , toField $ atMapText "db.operation.name" (unAesonTextMaybe entry.attributes) -- attributes___db___operation___name
    , toField $ atMapText "db.response.status_code" (unAesonTextMaybe entry.attributes) -- attributes___db___response___status_code
    , toField $ atMapInt "db.operation.batch.size" (unAesonTextMaybe entry.attributes) -- attributes___db___operation___batch___size
    , toField $ atMapText "db.query.summary" (unAesonTextMaybe entry.attributes) -- attributes___db___query___summary
    , toField $ atMapText "db.query.text" (unAesonTextMaybe entry.attributes) -- attributes___db___query___text
    , toField $ atMapText "user.id" (unAesonTextMaybe entry.attributes) -- attributes___user___id
    , toField $ atMapText "user.email" (unAesonTextMaybe entry.attributes) -- attributes___user___email
    , toField $ atMapText "user.full_name" (unAesonTextMaybe entry.attributes) -- attributes___user___full_name
    , toField $ atMapText "user.name" (unAesonTextMaybe entry.attributes) -- attributes___user___name
    , toField $ atMapText "user.hash" (unAesonTextMaybe entry.attributes) -- attributes___user___hash
    , toField $ fmap (cleanNullBytesFromJSON . AE.Object . KEM.fromMapText) (unAesonTextMaybe entry.resource) -- resource as JSON
    , toField $ atMapText "service.name" (unAesonTextMaybe entry.resource) -- resource___service___name
    , toField $ atMapText "service.version" (unAesonTextMaybe entry.resource) -- resource___service___version
    , toField $ atMapText "service.instance.id" (unAesonTextMaybe entry.resource) -- resource___service___instance___id
    , toField $ atMapText "service.namespace" (unAesonTextMaybe entry.resource) -- resource___service___namespace
    , toField $ atMapText "telemetry.sdk.language" (unAesonTextMaybe entry.resource) -- resource___telemetry___sdk___language
    , toField $ atMapText "telemetry.sdk.name" (unAesonTextMaybe entry.resource) -- resource___telemetry___sdk___name
    , toField $ atMapText "telemetry.sdk.version" (unAesonTextMaybe entry.resource) -- resource___telemetry___sdk___version
    , toField $ atMapText "user_agent.original" (unAesonTextMaybe entry.resource) -- resource___user_agent___original
    , toField $ cleanNullBytes entry.project_id -- project_id
    , toField $ V.map cleanNullBytes entry.summary -- summary
    , toField entry.date
    ]
    where
      -- Helper functions for severity fields
      parseSeverityText sev = do
        s <- sev
        cleanNullBytes . show <$> s.severity_text

      parseSeverityNumber = fmap (show . severity_number)


bulkInsertOtelLogsAndSpansTF :: (Concurrent :> es, DB :> es, Eff.Reader AuthContext :> es, IOE :> es, Labeled "timefusion" DB :> es, Log :> es, UUIDEff :> es) => V.Vector OtelLogsAndSpans -> Eff es ()
bulkInsertOtelLogsAndSpansTF records = do
  appCtx <- Eff.ask @AuthContext
  updatedRecords <- V.mapM (\r -> genUUID >>= \uid -> pure (r & #id .~ UUID.toText uid)) records
  _ <- bulkInsertOtelLogsAndSpans updatedRecords
  when appCtx.config.enableTimefusionWrites $ void $ retryTimefusion 10 updatedRecords
  where
    retryTimefusion 0 recs = labeled @"timefusion" @DB $ bulkInsertOtelLogsAndSpans recs
    retryTimefusion n recs = do
      tryAny (labeled @"timefusion" @DB $ bulkInsertOtelLogsAndSpans recs) >>= \case
        Left e | "Connection refused" `T.isInfixOf` T.pack (show e) -> do
          Log.logAttention "Retrying bulkInsertOtelLogsAndSpans" $ AE.object [("remaining_retries", show n), ("error", show e)]
          threadDelay (1000000 * (4 - n))
          retryTimefusion (n - 1) recs
        Left e -> throwIO e
        Right count -> pure count


-- Function to insert OtelLogsAndSpans records with all fields in flattened structure
-- Using direct connection without transaction
bulkInsertOtelLogsAndSpans :: DB :> es => V.Vector OtelLogsAndSpans -> Eff es Int64
bulkInsertOtelLogsAndSpans records = dbtToEff $ executeMany bulkInserSpansAndLogsQuery (V.toList records)


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
       project_id, summary, date)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
              ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
              ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
 
    |]


removeDuplic :: Eq a => Eq e => [(a, e, b, c, d, q)] -> [(a, e, b, c, d, q)]
removeDuplic = L.nubBy (\(a1, a2, _, _, _, _) (b1, b2, _, _, _, _) -> a1 == b1 && a2 == b2)


getValsWithPrefix :: Text -> AE.Object -> AE.Value
getValsWithPrefix prefix obj = AE.object $ map (\k -> (AEK.fromText (T.replace prefix "" $ AEK.toText k), fromMaybe (AE.object []) $ KEM.lookup k obj)) keys
  where
    keys = filter (\k -> prefix `T.isPrefixOf` AEK.toText k) (KEM.keys obj)


getSpanAttribute :: Text -> AE.Object -> Maybe Text
getSpanAttribute key attr = case KEM.lookup (AEK.fromText key) attr of
  Just (AE.String v) -> Just v
  Just (AE.Number v) -> Just $ show v
  _ -> Nothing


data Severity = Severity
  { severity_text :: Maybe SeverityLevel
  , severity_number :: Int
  }
  deriving (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (FromField, ToField) via AesonText Severity
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Severity


data Context = Context
  { trace_id :: Maybe Text
  , span_id :: Maybe Text
  , trace_state :: Maybe Text
  , trace_flags :: Maybe Text
  , is_remote :: Maybe Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (FromField, ToField) via AesonText Context
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Context


data OtelLogsAndSpans = OtelLogsAndSpans
  { id :: Text -- UUID
  , project_id :: Text
  , timestamp :: UTCTime
  , parent_id :: Maybe Text
  , observed_timestamp :: Maybe UTCTime
  , hashes :: V.Vector Text
  , name :: Maybe Text
  , kind :: Maybe Text
  , status_code :: Maybe Text
  , status_message :: Maybe Text
  , level :: Maybe Text
  , severity :: Maybe Severity
  , body :: Maybe (AesonText AE.Value)
  , duration :: Maybe Int64
  , start_time :: UTCTime
  , end_time :: Maybe UTCTime
  , context :: Maybe Context
  , events :: Maybe (AesonText AE.Value)
  , links :: Maybe Text
  , attributes :: Maybe (AesonText (Map Text AE.Value))
  , resource :: Maybe (AesonText (Map Text AE.Value))
  , summary :: V.Vector Text
  , date :: UTCTime
  , errors :: Maybe AE.Value
  }
  deriving (Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake OtelLogsAndSpans


-- Custom FromRow instance that matches the column order in specific SELECT queries
instance FromRow OtelLogsAndSpans where
  fromRow = do
    -- Order from SELECT queries in this file:
    -- project_id, id, timestamp, observed_timestamp, context, level, severity, body, attributes, resource,
    -- hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, summary, date
    project_id' <- field
    id' <- field
    timestamp' <- field
    observed_timestamp' <- field
    context' <- field
    level' <- field
    severity' <- field
    body' <- field
    attributes' <- field
    resource' <- field
    hashes' <- field
    kind' <- field
    status_code' <- field
    status_message' <- field
    start_time' <- field
    end_time' <- field
    events' <- field
    links' <- field
    duration' <- field
    name' <- field
    parent_id' <- field
    summary' <- field
    date' <- field
    return
      $ OtelLogsAndSpans
        { id = id'
        , project_id = project_id'
        , timestamp = timestamp'
        , parent_id = parent_id'
        , observed_timestamp = observed_timestamp'
        , hashes = hashes'
        , name = name'
        , kind = kind'
        , status_code = status_code'
        , status_message = status_message'
        , level = level'
        , severity = severity'
        , body = body'
        , duration = duration'
        , start_time = start_time'
        , end_time = end_time'
        , context = context'
        , events = events'
        , links = links'
        , attributes = attributes'
        , resource = resource'
        , summary = summary'
        , date = date'
        , errors = Nothing
        }


-- | Extract error events from OpenTelemetry span events
-- Following OpenTelemetry semantic conventions for exceptions:
-- https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/exceptions/
getErrorEvents :: OtelLogsAndSpans -> V.Vector AE.Value
getErrorEvents OtelLogsAndSpans{events = Just (AesonText (AE.Array arr))} =
  V.filter isErrorEvent arr
  where
    isErrorEvent (AE.Object o) =
      case KEM.lookup "event_name" o of
        Just (AE.String name) -> "exception" `T.isInfixOf` name || "error" `T.isInfixOf` name
        _ -> False
    isErrorEvent _ = False
getErrorEvents _ = []


-- | Extract all runtime errors from a collection of spans
-- This is the main entry point for error anomaly detection
getAllATErrors :: V.Vector OtelLogsAndSpans -> V.Vector RequestDumps.ATError
getAllATErrors = V.concatMap extractErrorsFromSpan
  where
    extractErrorsFromSpan spanObj =
      let events = getErrorEvents spanObj
       in V.mapMaybe (extractATError spanObj) events


-- | Extract error details from an OpenTelemetry event
-- Follows the OpenTelemetry semantic conventions:
-- - exception.type: The type of the exception
-- - exception.message: The exception message
-- - exception.stacktrace: The stacktrace
-- Also extracts HTTP context (method, path) for better error tracking
extractATError :: OtelLogsAndSpans -> AE.Value -> Maybe RequestDumps.ATError
extractATError spanObj (AE.Object o) = do
  AE.Object attrs' <- KEM.lookup "event_attributes" o
  AE.Object attrs <- KEM.lookup "exception" attrs'
  let method = case unAesonTextMaybe spanObj.attributes >>= Map.lookup "http.request.method" of
        Just (AE.String s) -> Just s
        _ -> Nothing
      urlPath = case unAesonTextMaybe spanObj.attributes >>= Map.lookup "http.route" of
        Just (AE.String s) -> Just s
        _ -> case unAesonTextMaybe spanObj.attributes >>= Map.lookup "http.target" of
          Just (AE.String s) -> Just s
          _ -> Nothing
  let lookupText k = case KEM.lookup k attrs of
        Just (AE.String s) -> Just s
        _ -> Nothing
      getTextOrEmpty k = fromMaybe "" (lookupText k)

      typ = getTextOrEmpty "type"
      msg = getTextOrEmpty "message"
      stack = getTextOrEmpty "stacktrace"

      -- TODO: parse telemetry.sdk.name to SDKTypes
      tech = case unAesonTextMaybe spanObj.resource >>= Map.lookup "telemetry" of
        Just (AE.Object tel) ->
          KEM.lookup "sdk" tel
            >>= ( \case
                    AE.Object sdkObj -> KEM.lookup "name" sdkObj >>= asText
                    _ -> Nothing
                )
        _ -> Nothing
      serviceName = case unAesonTextMaybe spanObj.resource >>= Map.lookup "service" of
        Just (AE.Object serviceObj) ->
          KEM.lookup "name" serviceObj >>= asText
        _ -> Nothing

      spanId = spanObj.context >>= (.span_id)
      trId = spanObj.context >>= (.trace_id)

      asText (AE.String t) = Just t
      asText _ = Nothing

  -- Build ATError structure for anomaly detection
  -- The hash is critical for grouping similar errors together
  -- Hash components: projectId + service + span name + error type + sanitized message/stack
  -- This ensures similar errors are grouped while allowing variations in the actual message
  return
    $ RequestDumps.ATError
      { projectId = UUID.fromText spanObj.project_id >>= (\uid -> Just Projects.ProjectId{unProjectId = uid})
      , when = spanObj.timestamp
      , errorType = typ
      , rootErrorType = typ
      , message = msg
      , rootErrorMessage = msg
      , stackTrace = stack
      , hash = Just (toXXHash (spanObj.project_id <> fromMaybe "" serviceName <> fromMaybe "" spanObj.name <> typ <> (replaceAllFormats $ msg <> stack)))
      , technology = Nothing
      , serviceName = serviceName
      , requestMethod = method
      , requestPath = urlPath
      , spanId = spanId
      , traceId = trId
      , stack = tech
      }
extractATError _ _ = Nothing


getProjectStatsForReport :: DB :> es => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es (V.Vector (Text, Int, Int))
getProjectStatsForReport projectId start end = dbtToEff $ query q (projectId, start, end)
  where
    q =
      [sql| SELECT resource___service___name AS service_name,  COUNT(*) FILTER ( WHERE status_code = 'ERROR' OR attributes___exception___type IS NOT NULL) AS total_error_events, COUNT(*) AS total_events
           FROM otel_logs_and_spans
           WHERE project_id = ? AND timestamp >= ? AND timestamp <= ? AND resource___service___name is not null
           GROUP BY resource___service___name 
           ORDER BY total_events DESC;
        |]


getProjectStatsBySpanType :: DB :> es => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es (V.Vector (Text, Int, Int))
getProjectStatsBySpanType projectId start end = dbtToEff $ query q (projectId, start, end)
  where
    q =
      [sql|
        SELECT
          CASE
            WHEN attributes___http___request___method IS NOT NULL THEN 'http'
            WHEN attributes___db___system___name IS NOT NULL THEN 'db'
            WHEN attributes ->>'rpc' IS NOT NULL THEN 'rpc'
            WHEN attributes ->>'messaging' IS NOT NULL THEN 'messaging'
            WHEN kind = 'internal' THEN 'internal'
            ELSE 'other'
          END AS span_type,
          COUNT(*) AS total_events,
          AVG(duration)::BIGINT AS avg_duration
        FROM otel_logs_and_spans
        WHERE project_id = ?
          AND timestamp >= ?
          AND timestamp <= ?
        GROUP BY span_type
        ORDER BY total_events DESC;
      |]


getEndpointStats :: DB :> es => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es (V.Vector (Text, Text, Text, Int, Int))
getEndpointStats projectId start end = dbtToEff $ query q (projectId, start, end)
  where
    q =
      [sql|
SELECT
    COALESCE(attributes___server___address, attributes___network___peer___address, '') AS host,
    COALESCE(attributes___http___request___method, 'GET') AS method,
    COALESCE(attributes___url___path, '') AS url_path,
    CAST(ROUND(AVG(COALESCE(duration, 0))) AS BIGINT) AS average_duration,
    COUNT(*) AS request_count
FROM otel_logs_and_spans
WHERE 
    project_id = ?::text
    AND timestamp > ? AND timestamp < ?
    AND name = 'monoscope.http'
GROUP BY
    host, method, url_path
ORDER BY
    request_count DESC,
    average_duration DESC;
    |]


getDBQueryStats :: DB :> es => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es (V.Vector (Text, Int, Int))
getDBQueryStats projectId start end = dbtToEff $ query q (projectId, start, end)
  where
    q =
      [sql| SELECT
  attributes___db___query___text AS query,
  ROUND(AVG(duration))::BIGINT AS avg_duration,
  COUNT(*) AS count
FROM otel_logs_and_spans
WHERE
  project_id = ?
  AND timestamp >= ?
  AND timestamp <= ?
  AND attributes___db___query___text IS NOT NULL
GROUP BY attributes___db___query___text
HAVING ROUND(AVG(duration)/1000000) > 500
ORDER BY avg_duration DESC LIMIT 10;
      |]


getTraceShapes :: DB :> es => Projects.ProjectId -> V.Vector Text -> Eff es (V.Vector (Text, Text, Int, Int))
getTraceShapes pid trIds =
  dbtToEff $ query q (pid, trIds, pid, pid)
  where
    q =
      [sql|
      WITH target_trace_spans AS (
        SELECT DISTINCT context___trace_id, name
        FROM otel_logs_and_spans
        WHERE project_id = ?
          AND timestamp > now() - interval '1 hour'
          AND context___trace_id = ANY(?)
      ),
      target_shapes AS (
        SELECT
          context___trace_id AS target_trace_id,
          ARRAY_AGG(DISTINCT name ORDER BY name) AS span_names
        FROM target_trace_spans
        GROUP BY context___trace_id
      ),
      trace_shapes AS (
        SELECT
          context___trace_id AS trace_id,
          ARRAY_AGG(DISTINCT name ORDER BY name) AS span_names
        FROM otel_logs_and_spans
        WHERE project_id = ?
          AND timestamp > now() - interval '1 hour'
          AND context___trace_id IS NOT NULL
        GROUP BY context___trace_id
      ),
      matching_traces AS (
        SELECT
          ts.target_trace_id,
          t.trace_id
        FROM trace_shapes t
        JOIN target_shapes ts
          ON t.span_names = ts.span_names
      )
      SELECT
        m.target_trace_id,
        s.name,
        AVG(s.duration)::BIGINT AS avg_duration,
        COUNT(*) AS span_count
      FROM otel_logs_and_spans s
      JOIN matching_traces m
        ON s.context___trace_id = m.trace_id
      WHERE s.project_id = ?
        AND s.timestamp > now() - interval '1 hour' and s.name IS NOT NULL
      GROUP BY m.target_trace_id, s.name
      ORDER BY m.target_trace_id, s.name;
      |]
