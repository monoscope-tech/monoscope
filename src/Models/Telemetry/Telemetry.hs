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
  AggregationTemporality (..),
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
  insertAndHandOff,
  handOffBatches,
  mintOtelLogIds,
  getMetricChartListData,
  getMetricLabelValues,
  getTraceShapes,
  getMetricServiceNames,
  SpanEvent (..),
  SpanLink (..),
  atMapText,
  atMapInt,
  spanServiceName,
  getProjectStatsBySpanType,
  getEndpointStats,
  getDBQueryStats,
  mkSystemLog,
  insertSystemLog,
  generateSummary,
)
where

import Control.Exception.Annotated (checkpoint)
import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as KEM
import Data.ByteString.Base16 qualified as B16
import Data.Default (Default (..))
import Data.Effectful.UUID (UUIDEff, genUUID)
import Data.Generics.Labels ()
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L (groupBy)
import Data.List.Extra (chunksOf)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Display (Display)
import Data.Time (UTCTime)
import Data.Time.Clock (addUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (ResultError (ConversionFailed))

import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Database.PostgreSQL.Simple.FromField (Conversion (..), FromField (..), returnError)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Database.PostgreSQL.Simple.ToRow
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Ki qualified as Ki
import Effectful.Labeled (Labeled, labeled)
import Effectful.Log (Log)
import Effectful.Time qualified as Time
import Hasql.Decoders qualified as D
import Hasql.DynamicStatements.Snippet (Snippet, encoderAndParam, param)
import Hasql.DynamicStatements.Statement (dynamicallyParameterized)
import Hasql.Encoders qualified as E
import Hasql.Interpolate qualified as HI
import Hasql.Session qualified as HSession
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (AesonText (..), DB, UUIDId (..), WrappedEnum (..), WrappedEnumSC (..), encodeEnumSC, idFromText, textArrayEnc, unAesonTextMaybe)
import Pkg.ExtractionWorker qualified as EW
import Relude hiding (ask)
import System.IO (hPutStrLn)
import System.Logging qualified as Log
import Text.Regex.TDFA.Text ()
import UnliftIO (throwIO, tryAny)
import Utils (extractMessageFromLog, getDurationNSMS, lookupValueText)


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


spanServiceName :: OtelLogsAndSpans -> Maybe Text
spanServiceName s = atMapText "service.name" (unAesonTextMaybe s.resource)


data SeverityLevel = SLTrace | SLDebug | SLInfo | SLWarn | SLError | SLFatal
  deriving (Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC "SL" SeverityLevel


data SpanStatus = SSOk | SSError | SSUnset
  deriving (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC "SS" SpanStatus


data SpanKind = SKInternal | SKServer | SKClient | SKProducer | SKConsumer | SKUnspecified
  deriving (Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC "SK" SpanKind


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
  , aggregationTemporality :: Maybe AggregationTemporality
  , isMonotonic :: Maybe Bool
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
  deriving (HI.EncodeValue, HI.DecodeValue) via Aeson MetricValue


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
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Histogram


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
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ExponentialHistogram


data EHBucket = EHBucket
  { bucketOffset :: Int
  , bucketCounts :: V.Vector Int
  }
  deriving (Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake EHBucket


data Summary = Summary
  { sum :: Double
  , count :: Int
  , quantiles :: V.Vector Quantile
  }
  deriving (Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Summary


data Quantile = Quantile
  { quantile :: Double
  , value :: Double
  }
  deriving (Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Quantile


data Exemplar = Exemplar
  { value :: Double
  , timestamp :: UTCTime
  , attributes :: AE.Value
  }
  deriving (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON)


data NativeMetricColumns = NativeMetricColumns
  { nValue :: Maybe Double
  , nSum :: Maybe Double
  , nCount :: Maybe Int
  , nBucketCounts :: Maybe AE.Value
  , nExplicitBounds :: Maybe AE.Value
  , nPointMin :: Maybe Double
  , nPointMax :: Maybe Double
  , nQuantiles :: Maybe AE.Value
  }
  deriving (Generic)
  deriving anyclass (Default, ToRow)


metricValueToNative :: MetricValue -> NativeMetricColumns
metricValueToNative = \case
  GaugeValue g -> def{nValue = Just g.value}
  SumValue g -> def{nValue = Just g.value}
  HistogramValue h -> def{nSum = Just h.sum, nCount = Just h.count, nBucketCounts = Just $ AE.toJSON h.bucketCounts, nExplicitBounds = Just $ AE.toJSON h.explicitBounds, nPointMin = Just h.pointMin, nPointMax = Just h.pointMax}
  SummaryValue s -> def{nSum = Just s.sum, nCount = Just s.count, nQuantiles = Just $ AE.toJSON s.quantiles}
  ExponentialHistogramValue e -> def{nSum = Just e.sum, nCount = Just e.count, nPointMin = Just e.pointMin, nPointMax = Just e.pointMax}


data MetricType = MTGauge | MTSum | MTHistogram | MTExponentialHistogram | MTSummary
  deriving (Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, NFData)
  deriving (FromField, ToField) via WrappedEnum "MT" MetricType
  deriving (HI.EncodeValue, HI.DecodeValue) via WrappedEnum "MT" MetricType


data AggregationTemporality = ATUnspecified | ATDelta | ATCumulative
  deriving (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake AggregationTemporality


instance ToField AggregationTemporality where toField = toField . fromEnum
instance HI.EncodeValue AggregationTemporality where encodeValue = contramap fromEnum HI.encodeValue


instance FromField AggregationTemporality where
  fromField f bs =
    fromField @Int f bs >>= \n ->
      if n >= fromEnum (minBound @AggregationTemporality) && n <= fromEnum (maxBound @AggregationTemporality)
        then pure (toEnum n)
        else returnError ConversionFailed f ("Invalid aggregation_temporality: " <> show n)
instance HI.DecodeValue AggregationTemporality where decodeValue = toEnum <$> HI.decodeValue


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
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)


data MetricChartListData = MetricChartListData
  { metricName :: Text
  , metricType :: Text
  , metricUnit :: Text
  , metricDescription :: Text
  , lastSeen :: UTCTime
  }
  deriving (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)


getTraceDetails :: (Hasql :> es, IOE :> es) => Projects.ProjectId -> Text -> Maybe UTCTime -> UTCTime -> Eff es (Maybe Trace)
getTraceDetails pid trId tme now = do
  let pidTxt = pid.toText
  rows :: V.Vector (Text, UTCTime, UTCTime, Int64, Int64, Maybe (V.Vector Text)) <-
    Hasql.interp
      [HI.sql| SELECT
              context___trace_id,
              MIN(start_time) AS trace_start_time,
              MAX(COALESCE(end_time, start_time)) AS trace_end_time,
              CAST(EXTRACT(EPOCH FROM (MAX(COALESCE(end_time, start_time)) - MIN(start_time))) * 1000000000 AS int8) AS trace_duration_ns,
              COUNT(context->>'span_id')::int8 AS total_spans,
              ARRAY_REMOVE(ARRAY_AGG(DISTINCT jsonb_extract_path_text(resource, 'service.name')), NULL) AS service_names
            FROM otel_logs_and_spans
            WHERE project_id = #{pidTxt} AND timestamp BETWEEN #{start} AND #{end} AND context___trace_id = #{trId}
            GROUP BY context___trace_id |]
  pure $ (\(tid, ts, te, dur, ns, sn) -> Trace tid ts te (fromIntegral dur) (fromIntegral ns) sn) <$> (rows V.!? 0)
  where
    (start, end) = case tme of
      Nothing -> (addUTCTime (-(14 * 24 * 3600)) now, now)
      Just ts -> (addUTCTime (-(60 * 5)) ts, addUTCTime (60 * 5) ts)


logRecordByProjectAndId :: DB es => Projects.ProjectId -> UTCTime -> UUID.UUID -> Eff es (Maybe OtelLogsAndSpans)
logRecordByProjectAndId pid createdAt rdId = do
  let pidTxt = pid.toText
  Hasql.interpOne
    [HI.sql|SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity, body, attributes, resource,
                COALESCE(hashes, '{}'), kind, status_code, status_message, COALESCE(start_time, timestamp), end_time, events, links, duration, name, parent_id, summary, date::timestamptz
           FROM otel_logs_and_spans where (timestamp=#{createdAt})  and project_id=#{pidTxt} and id=#{rdId} LIMIT 1|]


getSpanRecordsByTraceId :: DB es => Projects.ProjectId -> Text -> Maybe UTCTime -> UTCTime -> Eff es [OtelLogsAndSpans]
getSpanRecordsByTraceId pid trId tme now = do
  let pidTxt = pid.toText
      (start, end) = case tme of
        Nothing -> (addUTCTime (-(14 * 24 * 3600)) now, now)
        Just ts -> (addUTCTime (-(60 * 5)) ts, addUTCTime (60 * 5) ts)
  Hasql.interp
    [HI.sql|
      SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity, body, attributes, resource,
                  COALESCE(hashes, '{}'), kind, status_code, status_message, COALESCE(start_time, timestamp), end_time, events, links, duration, name, parent_id, summary, date::timestamptz
              FROM otel_logs_and_spans where project_id=#{pidTxt} and timestamp BETWEEN #{start} AND #{end} and context___trace_id=#{trId} ORDER BY start_time ASC
    |]


getSpanRecordsByTraceIds :: (DB es, Time.Time :> es) => Projects.ProjectId -> V.Vector Text -> Maybe UTCTime -> Eff es [OtelLogsAndSpans]
getSpanRecordsByTraceIds pid traceIds tme = do
  now <- Time.currentTime
  let (start, end') = case tme of
        Nothing -> (addUTCTime (-86400) now, now)
        Just ts -> (addUTCTime (-300) ts, addUTCTime 300 ts)
      pidText = pid.toText
      traceIdsList = V.toList traceIds
  Hasql.interp
    [HI.sql| SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity,
                 body, attributes, resource, COALESCE(hashes, '{}'), kind, status_code, status_message,
                 COALESCE(start_time, timestamp), end_time, events, links, duration, name,
                 parent_id, summary, date::timestamptz
          FROM otel_logs_and_spans
          WHERE project_id = #{pidText}
            AND timestamp >= #{start}
            AND timestamp <= #{end'}
            AND context___trace_id = ANY(#{traceIdsList})
          ORDER BY context___trace_id ASC, start_time ASC |]


spanRecordByProjectAndId :: DB es => Projects.ProjectId -> UTCTime -> UUID.UUID -> Eff es (Maybe OtelLogsAndSpans)
spanRecordByProjectAndId pid createdAt rdId = do
  let pidTxt = pid.toText
  Hasql.interpOne
    [HI.sql| SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity, body, attributes, resource,
                COALESCE(hashes, '{}'), kind, status_code, status_message, COALESCE(start_time, timestamp), end_time, events, links, duration, name, parent_id, summary, date::timestamptz
            FROM otel_logs_and_spans where (timestamp=#{createdAt})  and project_id=#{pidTxt} and id=#{rdId} LIMIT 1|]


spanRecordByName :: DB es => Projects.ProjectId -> Text -> Text -> Eff es (Maybe OtelLogsAndSpans)
spanRecordByName pid trId spanName = do
  let pidTxt = pid.toText
  Hasql.interpOne
    [HI.sql| SELECT project_id, id::text, timestamp, observed_timestamp, context, level, severity, body, attributes, resource,
                COALESCE(hashes, '{}'), kind, status_code, status_message, COALESCE(start_time, timestamp), end_time, events, links, duration, name, parent_id, summary, date::timestamptz
            FROM otel_logs_and_spans where project_id=#{pidTxt} and context___trace_id = #{trId} and name=#{spanName} LIMIT 1|]


getDataPointsData :: (DB es, Time.Time :> es) => Projects.ProjectId -> (Maybe UTCTime, Maybe UTCTime) -> Eff es [MetricDataPoint]
getDataPointsData pid dateRange = do
  now <- Time.currentTime
  let dateFilter = case dateRange of
        (Nothing, Just b) -> [HI.sql| AND timestamp BETWEEN #{now} AND #{b} |]
        (Just a, Just b) -> [HI.sql| AND timestamp BETWEEN #{a} AND #{b} |]
        _ -> mempty
  Hasql.interp $
    [HI.sql| WITH metrics_aggregated AS (
        SELECT project_id, metric_name, COUNT(*)::int AS data_points
        FROM telemetry.metrics
        WHERE project_id = #{pid} |] <> dateFilter <> [HI.sql|
        GROUP BY project_id, metric_name
    )
    SELECT mm.metric_name, mm.metric_type, mm.metric_unit, mm.metric_description,
           COALESCE(ma.data_points, 0) AS data_points,
           ARRAY_AGG(mm.service_name) AS service_names, '{}'::text[] AS labels
    FROM telemetry.metrics_meta mm
    LEFT JOIN metrics_aggregated ma ON mm.project_id = ma.project_id AND mm.metric_name = ma.metric_name
    WHERE mm.project_id = #{pid}
    GROUP BY mm.metric_name, mm.metric_type, mm.metric_unit, mm.metric_description, ma.data_points |]


getMetricData :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe MetricDataPoint)
getMetricData pid metricName = Hasql.interpOne
  [HI.sql| SELECT mm.metric_name, mm.metric_type, mm.metric_unit, mm.metric_description,
           COALESCE(m.data_points, 0) AS data_points,
           COALESCE(m.service_names, ARRAY[mm.service_name]::text[]) AS service_names,
           COALESCE(m.metric_labels, ARRAY[]::text[]) AS metric_labels
      FROM telemetry.metrics_meta mm
      LEFT JOIN LATERAL (
          SELECT COUNT(*)::int AS data_points,
              ARRAY_AGG(DISTINCT COALESCE(resource->>'service.name', 'unknown'))::text[] AS service_names,
              COALESCE(
                  (SELECT ARRAY_AGG(DISTINCT key)
                   FROM (SELECT DISTINCT jsonb_object_keys(attributes) AS key
                         FROM telemetry.metrics
                         WHERE project_id = #{pid} AND metric_name = #{metricName} AND attributes IS NOT NULL
                   ) AS unique_keys),
                  ARRAY[]::text[]
              ) AS metric_labels
          FROM telemetry.metrics
          WHERE project_id = mm.project_id AND metric_name = mm.metric_name
      ) m ON true
      WHERE mm.project_id = #{pid} AND mm.metric_name = #{metricName}
      LIMIT 1 |]


getTotalEventsToReport :: DB es => Projects.ProjectId -> UTCTime -> Eff es Int
getTotalEventsToReport pid lastReported = do
  let pidText = pid.toText
  fromMaybe 0 <$> Hasql.interpOne [HI.sql| SELECT count(*)::int FROM otel_logs_and_spans WHERE project_id=#{pidText} AND timestamp > #{lastReported}|]


getTotalMetricsCount :: DB es => Projects.ProjectId -> UTCTime -> Eff es Int
getTotalMetricsCount pid lastReported =
  fromMaybe 0 <$> Hasql.interpOne [HI.sql| SELECT count(*)::int FROM telemetry.metrics WHERE project_id=#{pid} AND timestamp > #{lastReported}|]


getMetricChartListData :: DB es => Projects.ProjectId -> Maybe Text -> Maybe Text -> Eff es [MetricChartListData]
getMetricChartListData pid sourceM prefixM = do
  let sourceFilter = case sourceM of
        Just source | source /= "" && source /= "all" -> [HI.sql| AND service_name = #{source}|]
        _ -> mempty
      prefixFilter = case prefixM of
        Just prefix | prefix /= "" && prefix /= "all" -> let pat = prefix <> "%" in [HI.sql| AND metric_name LIKE #{pat}|]
        _ -> mempty
  Hasql.interp $
    [HI.sql| SELECT metric_name, MAX(metric_type) as metric_type, MAX(metric_unit) as metric_unit,
             MAX(metric_description) as metric_description, MAX(updated_at) as last_seen
      FROM telemetry.metrics_meta WHERE project_id = #{pid} |] <> sourceFilter <> prefixFilter
      <> [HI.sql| GROUP BY metric_name ORDER BY MAX(updated_at) DESC, metric_name |]


getMetricLabelValues :: DB es => Projects.ProjectId -> Text -> Text -> Eff es [Text]
getMetricLabelValues pid metricName labelName =
  Hasql.interp [HI.sql| SELECT DISTINCT attributes->>#{labelName} FROM telemetry.metrics WHERE project_id = #{pid} AND metric_name = #{metricName}|]


getMetricServiceNames :: DB es => Projects.ProjectId -> Eff es [Text]
getMetricServiceNames pid =
  Hasql.interp [HI.sql| SELECT DISTINCT service_name FROM telemetry.metrics_meta WHERE project_id = #{pid}|]


bulkInsertMetrics :: DB es => V.Vector MetricRecord -> Eff es ()
bulkInsertMetrics metrics = checkpoint "bulkInsertMetrics" $ unless (V.null metrics) do
  -- 23 params per row; PostgreSQL limit is 65535 params → max ~2849 rows per INSERT
  forM_ (chunksOf 2800 $ V.toList metrics) \chunk -> do
    let metricRows = map metricRowSql chunk
    Hasql.interpExecute_ $ "INSERT INTO telemetry.metrics (project_id, metric_name, metric_type, metric_unit, metric_description, metric_time, timestamp, attributes, resource, instrumentation_scope, metric_value, exemplars, flags, value, metric_sum, metric_count, bucket_counts, explicit_bounds, point_min, point_max, quantiles, aggregation_temporality, is_monotonic) VALUES " <> mconcat (intersperse ", " metricRows)
  let metas = map metaRowSql $ removeDuplic $ V.toList $ V.map metaTuple metrics
  unless (null metas) $ Hasql.interpExecute_ $ "INSERT INTO telemetry.metrics_meta (project_id, metric_name, metric_type, metric_unit, metric_description, service_name) VALUES " <> mconcat (intersperse ", " metas) <> " ON CONFLICT (project_id, metric_name, service_name) DO UPDATE SET metric_type = EXCLUDED.metric_type, metric_unit = EXCLUDED.metric_unit, metric_description = EXCLUDED.metric_description, updated_at = current_timestamp"
  where
    metricRowSql (m :: MetricRecord) =
      let NativeMetricColumns{nValue, nSum, nCount, nBucketCounts, nExplicitBounds, nPointMin, nPointMax, nQuantiles} = metricValueToNative m.metricValue
          MetricRecord{projectId, metricName, metricType, metricUnit, metricDescription, metricTime, timestamp, attributes, resource, instrumentationScope, metricValue, exemplars, flags, aggregationTemporality, isMonotonic} = m
       in [HI.sql|(#{projectId}, #{metricName}, #{metricType}, #{metricUnit}, #{metricDescription}, #{metricTime}, #{timestamp}, #{attributes}, #{resource}, #{instrumentationScope}, #{metricValue}, #{exemplars}, #{flags}, #{nValue}, #{nSum}, #{nCount}, #{nBucketCounts}, #{nExplicitBounds}, #{nPointMin}, #{nPointMax}, #{nQuantiles}, #{aggregationTemporality}, #{isMonotonic})|]
    metaTuple (m :: MetricRecord) =
      let svc = fromMaybe "unknown" $ lookupValueText m.resource "service.name"
          MetricRecord{projectId, metricName, metricType, metricUnit, metricDescription} = m
       in (projectId, metricName, metricType, metricUnit, metricDescription, svc)
    metaRowSql (pid, mName, mType, mUnit, mDesc, svc) =
      [HI.sql|(#{pid}, #{mName}, #{mType}, #{mUnit}, #{mDesc}, #{svc})|]


-- | Mint a fresh UUID for every row's `id` field. Call this once at the ingestion
-- call site BEFORE handing the vector to `bulkInsertOtelLogsAndSpansTF`, so the
-- caller holds a vector whose ids match what lands in the DB (needed by the
-- in-process extraction worker hand-off — see plan notes).
mintOtelLogIds :: UUIDEff :> es => V.Vector OtelLogsAndSpans -> Eff es (V.Vector OtelLogsAndSpans)
mintOtelLogIds = V.mapM \r -> genUUID <&> \uid -> r & #id .~ UUID.toText uid


bulkInsertOtelLogsAndSpansTF
  :: ( Concurrent :> es
     , Hasql :> es
     , IOE :> es
     , Ki.StructuredConcurrency :> es
     , Labeled "timefusion" Hasql :> es
     , Log :> es
     )
  => Bool
  -- ^ enableTimefusionWrites (passed by caller to avoid a `Reader AuthContext`
  -- constraint here — keeps this module a leaf of `System.Config` so the
  -- extraction worker in `AuthContext` can be typed against `OtelLogsAndSpans`).
  -> V.Vector OtelLogsAndSpans
  -> Eff es ()
bulkInsertOtelLogsAndSpansTF enableTf records = do
  Log.logTrace "bulkInsertOtelLogsAndSpansTF called" $ AE.object [("record_count", AE.toJSON $ V.length records), ("enableTimefusionWrites", AE.toJSON enableTf)]
  if enableTf
    then Ki.scoped \scope -> do
      mainThread <- Ki.fork scope $ void $ bulkInsertOtelLogsAndSpans records
      _ <- Ki.fork scope do
        Log.logTrace "TimeFusion write enabled, attempting write" $ AE.object [("record_count", AE.toJSON $ V.length records)]
        tryAny (retryTimefusion 10 records) >>= \case
          Left e -> do
            -- Distinguish infra failures (HasqlException) from programmer bugs so on-call
            -- can triage: infra → page storage; bug → file an issue.
            let isHasql = isJust (fromException @Hasql.HasqlException e)
                errId = if isHasql then "TIMEFUSION_WRITE_FAILED" else "TIMEFUSION_WRITE_BUG" :: Text
                kind = if isHasql then "infra" else "programmer_error" :: Text
            Log.logAttention errId
              $ AE.object
                [ "error_id" AE..= errId
                , "kind" AE..= kind
                , "record_count" AE..= V.length records
                , "error" AE..= show @Text e
                ]
          Right _ -> pass
      Ki.atomically $ Ki.await mainThread
    else void $ bulkInsertOtelLogsAndSpans records
  where
    -- Retry only transient HasqlException; non-Hasql exceptions (e.g. InvalidOtelRowIdException
    -- from bad UUIDs, encode panics) are programmer bugs and must propagate immediately.
    -- Do NOT widen to tryAny-style retry — that would re-raise programmer bugs in a loop.
    retryTimefusion n recs =
      tryAny (labeled @"timefusion" @Hasql $ bulkInsertOtelLogsAndSpans recs) >>= \case
        Right count -> Log.logTrace "TimeFusion write succeeded" $ AE.object [("rows_inserted", AE.toJSON count)]
        Left e
          | n > 0
          , maybe False Hasql.isTransientHasqlError (fromException e) -> do
              let attempt = 11 - n -- attempts: 1..10
                  delayMicros = min 5000000 (100000 * (2 ^ (attempt - 1) :: Int)) -- 100ms,200ms,...,cap 5s
              Log.logAttention "Retrying bulkInsertOtelLogsAndSpans"
                $ AE.object [("attempt", AE.toJSON attempt), ("max_attempts", AE.toJSON (10 :: Int)), ("backoff_us", AE.toJSON delayMicros), ("error", AE.toJSON $ show @Text e)]
              threadDelay delayMicros
              retryTimefusion (n - 1) recs
        Left e -> throwIO e


-- | Persist `records` to Postgres (+TimeFusion when enabled) and hand each
-- per-project sub-batch to the in-process extraction worker. Every ingestion
-- call site (Pub/Sub, Kafka, OTLP gRPC) routes through this helper so the
-- eager-track derivation pipeline kicks off as soon as the rows land in
-- storage. A drop from `submitBatch` is non-fatal: the row is durable in both
-- stores, `processed_at` stays NULL, and the hourly `SafetyNetReprocess` job
-- re-submits it on the next tick.
insertAndHandOff
  :: ( Concurrent :> es
     , Hasql :> es
     , IOE :> es
     , Ki.StructuredConcurrency :> es
     , Labeled "timefusion" Hasql :> es
     , Log :> es
     )
  => Bool
  -> EW.WorkerState OtelLogsAndSpans
  -> HM.HashMap Projects.ProjectId Projects.ProjectCache
  -> V.Vector OtelLogsAndSpans
  -> Eff es ()
insertAndHandOff enableTf worker caches records
  | V.null records = pass
  | otherwise = do
      bulkInsertOtelLogsAndSpansTF enableTf records
      liftIO $ handOffBatches worker caches records


-- | Group `records` by `project_id`, build one `ExtractionBatch` per group, and
-- submit via STM. Rows whose `project_id` is unparseable or whose project cache
-- is absent are silently skipped — the safety-net picks them up on the next
-- tick via `processed_at IS NULL`. Runs in IO (pure STM + one metric bump).
handOffBatches
  :: EW.WorkerState OtelLogsAndSpans
  -> HM.HashMap Projects.ProjectId Projects.ProjectCache
  -> V.Vector OtelLogsAndSpans
  -> IO ()
handOffBatches worker caches records = do
  let groups :: HM.HashMap Text [OtelLogsAndSpans]
      groups = V.foldr' (\r -> HM.insertWith (<>) r.project_id [r]) HM.empty records
  forM_ (HM.toList groups) \(pidText, rowList) ->
    case idFromText pidText of
      Nothing -> hPutStrLn stderr $ "handOffBatches: unparseable project_id: " <> toString pidText
      Just pid -> case HM.lookup pid caches of
        Nothing -> pass -- safety-net picks these up via processed_at IS NULL
        Just cache -> do
          let rows = V.fromList rowList
              extractSpanId r = fromMaybe "" (r.context >>= (.span_id))
              extractTraceId r = fromMaybe "" (r.context >>= (.trace_id))
              spanIds = V.map extractSpanId rows
              traceIds = V.map extractTraceId rows
              firstTs = (V.unsafeHead rows).timestamp
              (minTs, maxTs) =
                V.foldl'
                  (\(lo, hi) r -> (min lo r.timestamp, max hi r.timestamp))
                  (firstTs, firstTs)
                  rows
              batch =
                EW.ExtractionBatch
                  { projectId = pid
                  , projectCache = cache
                  , spans = rows
                  , spanIds
                  , traceIds
                  , batchMinTs = minTs
                  , batchMaxTs = maxTs
                  }
          ok <- atomically (EW.submitBatch worker batch)
          -- TODO(otel-metrics): emit a counter for dropped batches when submitBatch fails.
          unless ok $ atomicModifyIORef' worker.droppedBatches \n -> (n + 1, ())


-- | Hasql-backed bulk insert for OtelLogsAndSpans.
--
-- Sends all 87 columns of every row as binary parameters via a multi-row VALUES
-- INSERT built with hasql-dynamic-statements. Postgres caps the bind protocol at
-- 65535 parameters per statement, so we chunk to keep `chunkSize * 87 < 65535`.
-- Each row's `id` MUST be a parseable UUID — callers (`bulkInsertOtelLogsAndSpansTF`)
-- guarantee this by minting fresh UUIDs upfront.
bulkInsertOtelLogsAndSpans :: (Hasql :> es, IOE :> es, Log :> es) => V.Vector OtelLogsAndSpans -> Eff es Int64
bulkInsertOtelLogsAndSpans records
  | V.null records = pure 0
  | otherwise = do
      rowSnippets <- V.mapM otelRowSnippet records
      let chunkSize = 700 -- 700 * 87 = 60900 < 65535 PG bind-message limit
          chunks = unfoldr (\v -> if V.null v then Nothing else Just (V.splitAt chunkSize v)) rowSnippets
          chunkStmt c = dynamicallyParameterized (otelInsertHeader <> mconcat (intersperse ", " (V.toList c))) D.rowsAffected True
      case chunks of
        [single] -> Hasql.session (HSession.statement () (chunkStmt single))
        _ ->
          -- Multi-chunk insert: wrap in a single transaction so a mid-stream failure
          -- cannot leave a partial write.
          Hasql.transaction TxS.ReadCommitted TxS.Write
            $ Relude.sum
            <$> traverse (Tx.statement () . chunkStmt) chunks


-- | Thrown when an OtelLogsAndSpans row reaches the bulk insert path with an
-- unparseable id. Callers must mint UUIDs upfront — failing loud here prevents
-- silently writing rows under a sentinel UUID.
newtype InvalidOtelRowIdException = InvalidOtelRowIdException Text
  deriving stock (Show)
  deriving anyclass (Exception)


-- | Per-row context: pre-decoded attribute/resource maps (bound once instead
-- of re-parsing JSON ~50× per row) plus the params that need IO/logging to
-- compute (id UUID parse, is_remote textual bool parse).
data OtelRowCtx = OtelRowCtx
  { row :: OtelLogsAndSpans
  , am :: Maybe (Map.Map Text AE.Value)
  , rm :: Maybe (Map.Map Text AE.Value)
  , uuidP :: Snippet
  , isRemoteP :: Snippet
  }


-- | Single source of truth for the otel_logs_and_spans bulk insert: SQL column
-- name + per-row encoder. `otelInsertHeader` and `otelRowSnippet` are both
-- derived from this list, so the two can never drift apart.
otelColumns :: [(Text, OtelRowCtx -> Snippet)]
otelColumns =
  let aT k c = param (atMapText k c.am)
      aI k c = param (fmap (fromIntegral :: Int -> Int32) (atMapInt k c.am))
      aI8 k c = param (fmap (fromIntegral :: Int -> Int64) (atMapInt k c.am))
      rT k c = param (atMapText k c.rm)
      top f c = f c.row
      txt g = top (param . fmap cleanNullBytes . g)
      jp :: AE.ToJSON a => (OtelLogsAndSpans -> Maybe a) -> OtelRowCtx -> Snippet
      jp g = top (param . fmap (cleanNullBytesFromJSON . AE.toJSON) . g)
      aeson g = top (param . fmap cleanNullBytesFromJSON . unAesonTextMaybe . g)
      arr g = top (encoderAndParam (E.nonNullable textArrayEnc) . V.map cleanNullBytes . g)
      ctxT f = top (\e -> param (fmap cleanNullBytes (e.context >>= f)))
      sevText e = e.severity >>= \s -> cleanNullBytes . toText . encodeEnumSC @"SL" <$> s.severity_text
      sevNum :: OtelLogsAndSpans -> Maybe Int32
      sevNum e = fromIntegral . severity_number <$> e.severity
   in [ ("timestamp", top (param . (.timestamp)))
      , ("observed_timestamp", top (param . (.observed_timestamp)))
      , ("id", (.uuidP))
      , ("parent_id", txt (.parent_id))
      , ("hashes", arr (.hashes))
      , ("name", txt (.name))
      , ("kind", txt (.kind))
      , ("status_code", txt (.status_code))
      , ("status_message", txt (.status_message))
      , ("level", txt (.level))
      , ("severity", jp (.severity))
      , ("severity___severity_text", top (param . sevText))
      , ("severity___severity_number", top (param . sevNum))
      , ("body", aeson (.body))
      , ("duration", top (param . (.duration)))
      , ("start_time", top (param . (.start_time)))
      , ("end_time", top (param . (.end_time)))
      , ("context", jp (.context))
      , ("context___trace_id", ctxT (.trace_id))
      , ("context___span_id", ctxT (.span_id))
      , ("context___trace_state", ctxT (.trace_state))
      , ("context___trace_flags", ctxT (.trace_flags))
      , ("context___is_remote", (.isRemoteP))
      , ("events", aeson (.events))
      , ("links", txt (.links))
      , ("attributes", \c -> param (fmap (cleanNullBytesFromJSON . AE.Object . KEM.fromMapText) c.am))
      , ("attributes___client___address", aT "client.address")
      , ("attributes___client___port", aI "client.port")
      , ("attributes___server___address", aT "server.address")
      , ("attributes___server___port", aI "server.port")
      , ("attributes___network___local__address", aT "network.local.address")
      , ("attributes___network___local__port", aI "network.local.port")
      , ("attributes___network___peer___address", aT "network.peer.address")
      , ("attributes___network___peer__port", aI "network.peer.port")
      , ("attributes___network___protocol___name", aT "network.protocol.name")
      , ("attributes___network___protocol___version", aT "network.protocol.version")
      , ("attributes___network___transport", aT "network.transport")
      , ("attributes___network___type", aT "network.type")
      , ("attributes___code___number", aI "code.number")
      , ("attributes___code___file___path", aT "code.file.path")
      , ("attributes___code___function___name", aT "code.function.name")
      , ("attributes___code___line___number", aI "code.line.number")
      , ("attributes___code___stacktrace", aT "code.stacktrace")
      , ("attributes___log__record___original", aT "log.record.original")
      , ("attributes___log__record___uid", aT "log.record.uid")
      , ("attributes___error___type", aT "error.type")
      , ("attributes___exception___type", aT "exception.type")
      , ("attributes___exception___message", aT "exception.message")
      , ("attributes___exception___stacktrace", aT "exception.stacktrace")
      , ("attributes___url___fragment", aT "url.fragment")
      , ("attributes___url___full", aT "url.full")
      , ("attributes___url___path", aT "url.path")
      , ("attributes___url___query", aT "url.query")
      , ("attributes___url___scheme", aT "url.scheme")
      , ("attributes___user_agent___original", aT "user_agent.original")
      , ("attributes___http___request___method", aT "http.request.method")
      , ("attributes___http___request___method_original", aT "http.request.method_original")
      , ("attributes___http___response___status_code", aI "http.response.status_code")
      , ("attributes___http___request___resend_count", aI "http.request.resend_count")
      , ("attributes___http___request___body___size", aI8 "http.request.body.size")
      , ("attributes___session___id", aT "session.id")
      , ("attributes___session___previous___id", aT "session.previous.id")
      , ("attributes___db___system___name", aT "db.system.name")
      , ("attributes___db___collection___name", aT "db.collection.name")
      , ("attributes___db___namespace", aT "db.namespace")
      , ("attributes___db___operation___name", aT "db.operation.name")
      , ("attributes___db___response___status_code", aI "db.response.status_code")
      , ("attributes___db___operation___batch___size", aI "db.operation.batch.size")
      , ("attributes___db___query___summary", aT "db.query.summary")
      , ("attributes___db___query___text", aT "db.query.text")
      , ("attributes___user___id", aT "user.id")
      , ("attributes___user___email", aT "user.email")
      , ("attributes___user___full_name", aT "user.full_name")
      , ("attributes___user___name", aT "user.name")
      , ("attributes___user___hash", aT "user.hash")
      , ("resource", \c -> param (fmap (cleanNullBytesFromJSON . AE.Object . KEM.fromMapText) c.rm))
      , ("resource___service___name", rT "service.name")
      , ("resource___service___version", rT "service.version")
      , ("resource___service___instance___id", rT "service.instance.id")
      , ("resource___service___namespace", rT "service.namespace")
      , ("resource___telemetry___sdk___language", rT "telemetry.sdk.language")
      , ("resource___telemetry___sdk___name", rT "telemetry.sdk.name")
      , ("resource___telemetry___sdk___version", rT "telemetry.sdk.version")
      , ("resource___user_agent___original", rT "user_agent.original")
      , ("project_id", top (param . cleanNullBytes . (.project_id)))
      , ("summary", arr (.summary))
      , ("date", top (param . (.date)))
      ]


otelInsertHeader :: Snippet
otelInsertHeader =
  fromString $ "INSERT INTO otel_logs_and_spans (" <> toString (T.intercalate ", " (fst <$> otelColumns)) <> ") VALUES "


-- | Build the (?, ?, ..., ?) values group for one OtelLogsAndSpans row by
-- walking `otelColumns`. Throws `InvalidOtelRowIdException` on bad id and logs
-- on unparseable `is_remote` rather than silently corrupting the row.
otelRowSnippet :: (IOE :> es, Log :> es) => OtelLogsAndSpans -> Eff es Snippet
otelRowSnippet e = do
  uuidP <- maybe (liftIO $ throwIO (InvalidOtelRowIdException e.id)) (pure . param) (UUID.fromText e.id)
  isRemoteP <- case e.context >>= (.is_remote) of
    Nothing -> pure (param (Nothing :: Maybe Bool))
    Just t -> case T.toLower t of
      l
        | l `elem` ["true", "t", "1"] -> pure (param (Just True))
        | l `elem` ["false", "f", "0"] -> pure (param (Just False))
        | otherwise -> do
            Log.logAttention "OTEL_UNPARSEABLE_IS_REMOTE"
              $ AE.object ["error_id" AE..= ("OTEL_UNPARSEABLE_IS_REMOTE" :: Text), "value" AE..= t, "trace_id" AE..= (e.context >>= (.trace_id))]
            pure (param (Nothing :: Maybe Bool))
  let ctx = OtelRowCtx{row = e, am = unAesonTextMaybe e.attributes, rm = unAesonTextMaybe e.resource, uuidP, isRemoteP}
  pure $ "(" <> mconcat (intersperse ", " [enc ctx | (_, enc) <- otelColumns]) <> ")"


removeDuplic :: (Ord a, Ord e) => [(a, e, b, c, d, q)] -> [(a, e, b, c, d, q)]
removeDuplic = mapMaybe (viaNonEmpty Relude.head) . L.groupBy (\(a1, a2, _, _, _, _) (b1, b2, _, _, _, _) -> a1 == b1 && a2 == b2) . sortOn (\(a, b, _, _, _, _) -> (a, b))


data Severity = Severity
  { severity_text :: Maybe SeverityLevel
  , severity_number :: Int
  }
  deriving (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (FromField, ToField) via AesonText Severity
  deriving (HI.DecodeValue, HI.EncodeValue) via AesonText Severity
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
  deriving (HI.DecodeValue, HI.EncodeValue) via AesonText Context
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


-- Custom FromRow/DecodeRow instances match the column order in SELECT queries:
-- project_id, id, timestamp, observed_timestamp, context, level, severity, body, attributes, resource,
-- hashes, kind, status_code, status_message, start_time, end_time, events, links, duration, name, parent_id, summary, date
instance HI.DecodeRow OtelLogsAndSpans where
  decodeRow = do
    project_id' <- HI.decodeRow
    id' <- HI.decodeRow
    timestamp' <- HI.decodeRow
    observed_timestamp' <- HI.decodeRow
    context' <- HI.decodeRow
    level' <- HI.decodeRow
    severity' <- HI.decodeRow
    body' <- HI.decodeRow
    attributes' <- HI.decodeRow
    resource' <- HI.decodeRow
    hashes' <- HI.decodeRow
    kind' <- HI.decodeRow
    status_code' <- HI.decodeRow
    status_message' <- HI.decodeRow
    start_time' <- HI.decodeRow
    end_time' <- HI.decodeRow
    events' <- HI.decodeRow
    links' <- HI.decodeRow
    duration' <- HI.decodeRow
    name' <- HI.decodeRow
    parent_id' <- HI.decodeRow
    summary' <- HI.decodeRow
    date' <- HI.decodeRow
    pure OtelLogsAndSpans
      { id = id', project_id = project_id', timestamp = timestamp', parent_id = parent_id'
      , observed_timestamp = observed_timestamp', hashes = hashes', name = name', kind = kind'
      , status_code = status_code', status_message = status_message', level = level', severity = severity'
      , body = body', duration = duration', start_time = start_time', end_time = end_time'
      , context = context', events = events', links = links', attributes = attributes'
      , resource = resource', summary = summary', date = date', errors = Nothing
      }

instance FromRow OtelLogsAndSpans where
  fromRow = do
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
getAllATErrors :: V.Vector OtelLogsAndSpans -> V.Vector ErrorPatterns.ATError
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
extractATError :: OtelLogsAndSpans -> AE.Value -> Maybe ErrorPatterns.ATError
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
      getSpanAttr k = unAesonTextMaybe spanObj.attributes >>= Map.lookup k >>= \case AE.String s -> Just s; _ -> Nothing
      getUserAttrM k v = case unAesonTextMaybe spanObj.resource >>= Map.lookup v of
        Just (AE.Object userAttrs) -> KEM.lookup k userAttrs >>= asText
        _ -> Nothing

      typ = getTextOrEmpty "type"
      msg = getTextOrEmpty "message"
      stack = getTextOrEmpty "stacktrace"

      userId = getUserAttrM "id" "user"
      userEmail = getUserAttrM "email" "user"
      userIp = getSpanAttr "client.address"
      sessionId = getUserAttrM "id" "session"

      -- TODO: parse telemetry.sdk.name to SDKTypes
      tech = case unAesonTextMaybe spanObj.resource >>= Map.lookup "telemetry" of
        Just (AE.Object tel) ->
          KEM.lookup "sdk" tel
            >>= ( \case
                    AE.Object sdkObj -> KEM.lookup "language" sdkObj >>= asText
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
      pid = UUID.fromText spanObj.project_id >>= (Just . UUIDId)

  -- Build ATError structure for anomaly detection
  -- The hash is critical for grouping similar errors together
  -- Hash components: projectId + service + span name + error type + sanitized message/stack
  -- This ensures similar errors are grouped while allowing variations in the actual message
  -- projectId mService mEndpoint runtime exceptionType message stackTrace

  -- Use "" (empty) rather than a magic "unknown" sentinel so the hash inputs stay stable
  -- across events regardless of whether runtime detection succeeded.
  let rt = fromMaybe "" tech
      hashes = ErrorPatterns.computeErrorHashes spanObj.project_id serviceName spanObj.name rt typ msg stack
      isFwk = ErrorPatterns.isFrameworkTransportError typ (ErrorPatterns.normalizeMessage msg)
  return
    $ ErrorPatterns.ATError
      { projectId = pid
      , when = spanObj.timestamp
      , errorType = typ
      , rootErrorType = typ
      , message = msg
      , rootErrorMessage = msg
      , stackTrace = stack
      , hash = hashes.narrow
      , parentHash = Just hashes.broad
      , isFramework = isFwk
      , technology = Nothing
      , serviceName = serviceName
      , requestMethod = method
      , requestPath = urlPath
      , spanId = spanId
      , traceId = trId
      , runtime = tech
      , parentSpanId = spanObj.parent_id
      , endpointHash = Nothing
      , environment = Nothing
      , userId = userId
      , userEmail = userEmail
      , userIp = userIp
      , sessionId = sessionId
      }
extractATError _ _ = Nothing


getProjectStatsForReport :: DB es => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es [(Text, Int, Int)]
getProjectStatsForReport projectId start end = do
  let pidTxt = projectId.toText
  Hasql.interp
    [HI.sql| SELECT resource___service___name AS service_name,  (COUNT(*) FILTER ( WHERE status_code = 'ERROR' OR attributes___exception___type IS NOT NULL))::int AS total_error_events, COUNT(*)::int AS total_events
         FROM otel_logs_and_spans
         WHERE project_id = #{pidTxt} AND timestamp >= #{start} AND timestamp <= #{end} AND resource___service___name is not null
         GROUP BY resource___service___name
         ORDER BY total_events DESC
      |]


getProjectStatsBySpanType :: DB es => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es [(Text, Int, Int)]
getProjectStatsBySpanType projectId start end = do
  let pidTxt = projectId.toText
  Hasql.interp
    [HI.sql|
        SELECT
          CASE
            WHEN attributes___http___request___method IS NOT NULL THEN 'http'
            WHEN attributes___db___system___name IS NOT NULL THEN 'db'
            WHEN attributes ->>'rpc' IS NOT NULL THEN 'rpc'
            WHEN attributes ->>'messaging' IS NOT NULL THEN 'messaging'
            WHEN kind = 'internal' THEN 'internal'
            ELSE 'other'
          END AS span_type,
          COUNT(*)::int AS total_events,
          AVG(duration)::int AS avg_duration
        FROM otel_logs_and_spans
        WHERE project_id = #{pidTxt}
          AND timestamp >= #{start}
          AND timestamp <= #{end}
          AND kind != 'log'
        GROUP BY span_type
        ORDER BY total_events DESC
      |]


getEndpointStats :: DB es => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es [(Text, Text, Text, Int, Int)]
getEndpointStats projectId start end = do
  Hasql.interp
    [HI.sql|
SELECT
    COALESCE(attributes___server___address, attributes->'net'->'host'->>'name', attributes->'http'->>'host', NULLIF(split_part(split_part(split_part(attributes___url___full, '://', 2), '/', 1), ':', 1), ''), resource___service___name, '') AS host,
    COALESCE(attributes___http___request___method, 'GET') AS method,
    COALESCE(attributes___url___path, '') AS url_path,
    CAST(ROUND(AVG(COALESCE(duration, 0))) AS INT) AS average_duration,
    COUNT(*)::int AS request_count
FROM otel_logs_and_spans
WHERE
    project_id = #{projectId}::text
    AND timestamp > #{start} AND timestamp < #{end}
    AND attributes___http___request___method IS NOT NULL
GROUP BY
    host, method, url_path
ORDER BY
    request_count DESC,
    average_duration DESC
    |]


getDBQueryStats :: (Hasql :> es, IOE :> es) => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es [(Text, Int, Int)]
getDBQueryStats projectId start end = do
  let pidTxt = projectId.toText
  rows :: V.Vector (Text, Int64, Int64) <-
    Hasql.interp
      [HI.sql| SELECT
        attributes___db___query___text AS query,
        ROUND(AVG(duration))::int8 AS avg_duration,
        COUNT(*)::int8 AS count
      FROM otel_logs_and_spans
      WHERE project_id = #{pidTxt}
        AND timestamp >= #{start}
        AND timestamp <= #{end}
        AND kind != 'log'
        AND attributes___db___query___text IS NOT NULL
      GROUP BY attributes___db___query___text
      HAVING ROUND(AVG(duration)/1000000) > 500
      ORDER BY avg_duration DESC LIMIT 10 |]
  pure [(q, fromIntegral d, fromIntegral c) | (q, d, c) <- V.toList rows]


getTraceShapes :: (DB es, Time.Time :> es) => Projects.ProjectId -> V.Vector Text -> Eff es [(Text, Text, Int, Int)]
getTraceShapes pid trIds = do
  now <- Time.currentTime
  let pidTxt = pid.toText
  Hasql.interp
    [HI.sql|
      WITH target_trace_spans AS (
        SELECT DISTINCT context___trace_id, name
        FROM otel_logs_and_spans
        WHERE project_id = #{pidTxt}
          AND timestamp > #{now}::timestamptz - interval '1 hour'
          AND context___trace_id = ANY(#{trIds})
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
        WHERE project_id = #{pidTxt}
          AND timestamp > #{now}::timestamptz - interval '1 hour'
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
        AVG(s.duration)::int AS avg_duration,
        COUNT(*)::int AS span_count
      FROM otel_logs_and_spans s
      JOIN matching_traces m
        ON s.context___trace_id = m.trace_id
      WHERE s.project_id = #{pidTxt}
        AND s.timestamp > #{now}::timestamptz - interval '1 hour' and s.name IS NOT NULL
      GROUP BY m.target_trace_id, s.name
      ORDER BY m.target_trace_id, s.name
    |]


mkSystemLog
  :: Projects.ProjectId
  -> Text -- event name (e.g., "monitor.alert.triggered")
  -> SeverityLevel
  -> Text -- body message (human-readable)
  -> Map Text AE.Value -- attributes
  -> Maybe Int64 -- optional duration in nanoseconds
  -> UTCTime
  -> OtelLogsAndSpans
mkSystemLog (UUIDId pid) eventName sev bodyMsg attrs duration ts =
  let
    (levelText, sevNum) = case sev of
      SLTrace -> ("TRACE", 1)
      SLDebug -> ("DEBUG", 5)
      SLInfo -> ("INFO", 9)
      SLWarn -> ("WARN", 13)
      SLError -> ("ERROR", 17)
      SLFatal -> ("FATAL", 21)
    resource = Map.fromList [("service.name", AE.String "SYSTEM")]
   in
    OtelLogsAndSpans
      { id = ""
      , project_id = UUID.toText pid
      , timestamp = ts
      , parent_id = Nothing
      , observed_timestamp = Just ts
      , hashes = V.empty
      , name = Just eventName
      , kind = Just "log"
      , status_code = Nothing
      , status_message = Nothing
      , level = Just levelText
      , severity = Just Severity{severity_text = Just sev, severity_number = sevNum}
      , body = Just $ AesonText $ AE.String bodyMsg
      , duration = duration
      , start_time = ts
      , end_time = Nothing
      , context = Just Context{trace_id = Nothing, span_id = Nothing, trace_state = Nothing, trace_flags = Nothing, is_remote = Nothing}
      , events = Nothing
      , links = Nothing
      , attributes = if Map.null attrs then Nothing else Just (AesonText attrs)
      , resource = Just (AesonText resource)
      , summary = V.empty -- Generated by caller via generateSummary
      , date = ts
      , errors = Nothing
      }


insertSystemLog
  :: (Concurrent :> es, Hasql :> es, IOE :> es, Ki.StructuredConcurrency :> es, Labeled "timefusion" Hasql :> es, Log :> es, UUIDEff :> es)
  => Bool
  -- ^ enableTimefusionWrites
  -> OtelLogsAndSpans
  -> Eff es ()
insertSystemLog enableTf otelLog = do
  minted <- mintOtelLogIds (V.singleton otelLog)
  bulkInsertOtelLogsAndSpansTF enableTf minted


-- | Generate summary array for an OtelLogsAndSpans record
-- Format: "field;style⇒value" where:
--   - field is optional (can be empty for plain text)
--   - style can be: info-strong, info-weak, error-strong, error-weak,
--                   warning-strong, warning-weak, success-strong, success-weak, neutral
--   - special styles: "right" for right-aligned items
--   - plain text has no semicolon separator
generateSummary :: OtelLogsAndSpans -> V.Vector T.Text
generateSummary otel =
  case otel.kind of
    Just "log" -> generateLogSummary otel
    _ -> generateSpanSummary otel


generateLogSummary :: OtelLogsAndSpans -> V.Vector T.Text
generateLogSummary otel =
  let
    isRawDataLog =
      isNothing otel.body
        && isNothing otel.severity
        && maybe True Map.null (unAesonTextMaybe otel.attributes)

    elements = if isRawDataLog then rawDataLogElements else normalLogElements
    resourceFallback = case unAesonTextMaybe otel.resource of
      Just res
        | not (Map.null res) ->
            let resText = decodeUtf8 (AE.encode res)
                truncated =
                  if T.length resText > 500
                    then T.take 497 resText <> "..."
                    else resText
             in "resource;text-textWeak⇒" <> truncated
      _ -> "resource;text-textWeak⇒{}"
    rawDataLogElements =
      catMaybes
        [ case otel.context of
            Just ctx -> case ctx.trace_state of
              Just ts | ts /= "" -> Just $ "trace_state;neutral⇒" <> ts
              _ -> Nothing
            _ -> Nothing
        , otel.context >>= \ctx ->
            ctx.trace_id >>= \tid ->
              if tid /= ""
                then Just $ "trace_id;right-badge-neutral⇒" <> T.take 16 tid
                else Nothing
        , Just resourceFallback
        ]

    normalLogElements =
      catMaybes
        [ case otel.severity of
            Just Severity{severity_text = Just sev} ->
              Just $ "severity_text;" <> severityStyle sev <> "⇒" <> severityText sev
            _ -> Nothing
        , case unAesonTextMaybe otel.body of
            Just (AE.String txt) -> Just txt
            Just (AE.Object obj) -> case extractMessageFromLog (AE.Object obj) of
              Just v -> Just v
              _ -> Just $ decodeUtf8 (AE.encode obj)
            Just val -> case val of
              AE.Null -> Nothing
              _ -> Just $ T.take 200 $ toText $ show val
            Nothing -> Nothing
        , case unAesonTextMaybe otel.attributes of
            Just attrs
              | not (Map.null attrs) ->
                  let attrText = decodeUtf8 (AE.encode attrs)
                      truncated =
                        if T.length attrText > 500
                          then T.take 497 attrText <> "..."
                          else attrText
                   in Just $ "attributes;text-textWeak⇒" <> truncated
            _ -> Nothing
        ]
   in
    V.fromList $ if null elements then rawDataLogElements else elements
  where
    severityStyle sev = case sev of
      SLTrace -> "badge-neutral"
      SLDebug -> "badge-neutral"
      SLInfo -> "badge-info"
      SLWarn -> "badge-warning"
      SLError -> "badge-error"
      SLFatal -> "badge-fatal"

    severityText sev = case sev of
      SLTrace -> "TRACE"
      SLDebug -> "DEBUG"
      SLInfo -> "INFO"
      SLWarn -> "WARN"
      SLError -> "ERROR"
      SLFatal -> "FATAL"


generateSpanSummary :: OtelLogsAndSpans -> V.Vector T.Text
generateSpanSummary otel =
  let
    hasHttp = case unAesonTextMaybe otel.attributes of
      Just attrs ->
        isJust (atMapText "http.request.method" (Just attrs))
          || isJust (atMapInt "http.response.status_code" (Just attrs))
      _ -> False

    isEmptySpan =
      (isNothing otel.name || otel.name == Just "")
        && maybe True Map.null (unAesonTextMaybe otel.attributes)

    elements = if isEmptySpan then resourceFallbackElements else normalElements

    resourceFallbackElements =
      catMaybes
        [ case unAesonTextMaybe otel.resource of
            Just res ->
              case Map.lookup "process" res of
                Just (AE.Object procObj) ->
                  let procMap = Map.fromList [(AEK.toText k, v) | (k, v) <- KEM.toList procObj]
                   in case Map.lookup "executable" procMap of
                        Just (AE.Object execObj) ->
                          let execMap = Map.fromList [(AEK.toText k, v) | (k, v) <- KEM.toList execObj]
                           in case Map.lookup "name" execMap of
                                Just (AE.String name)
                                  | name /= "" ->
                                      Just $ "process;neutral⇒" <> name
                                _ ->
                                  case Map.lookup "pid" procMap of
                                    Just (AE.Number n) ->
                                      Just $ "process;neutral⇒PID " <> toText (show (round n :: Int))
                                    _ -> Nothing
                        _ ->
                          case Map.lookup "pid" procMap of
                            Just (AE.Number n) ->
                              Just $ "process;neutral⇒PID " <> toText (show (round n :: Int))
                            _ -> Nothing
                _ -> Nothing
            _ -> Nothing
        , unAesonTextMaybe otel.resource
            >>= atMapText "service.name"
            . Just
            <&> ("service;neutral⇒" <>)
        , case unAesonTextMaybe otel.resource of
            Just attrs
              | not (Map.null attrs) ->
                  let filtered = Map.filterWithKey (\k _ -> k /= "process") attrs
                      attrText = decodeUtf8 (AE.encode filtered)
                      truncated =
                        if T.length attrText > 300
                          then T.take 297 attrText <> "..."
                          else attrText
                   in if Map.null filtered
                        then Nothing
                        else Just $ "resource;text-textWeak⇒" <> truncated
            _ -> Nothing
        , otel.context >>= \ctx ->
            ctx.trace_id >>= \tid ->
              if tid /= ""
                then Just $ "trace_id;right-badge-neutral⇒" <> T.take 16 tid
                else Nothing
        , otel.duration <&> \dur ->
            "duration;right-badge-neutral⇒" <> toText (getDurationNSMS (fromIntegral dur))
        ]

    normalElements =
      catMaybes
        $ [ case (otel.kind, hasHttp, atMapText "component" (unAesonTextMaybe otel.attributes)) of
              (Just "server", True, _) -> Just "request_type;neutral⇒incoming"
              (Just "client", True, _) -> Just "request_type;neutral⇒outgoing"
              (_, True, Just comp) | "proxy" `T.isInfixOf` comp -> Just "request_type;neutral⇒incoming"
              (_, True, Just "frontend") -> Just "request_type;neutral⇒outgoing"
              (_, True, _) -> Just "request_type;neutral⇒outgoing"
              (Just "server", _, _) | isJust (atMapText "rpc.method" (unAesonTextMaybe otel.attributes)) -> Just "request_type;neutral⇒incoming"
              (Just "client", _, _) | isJust (atMapText "rpc.method" (unAesonTextMaybe otel.attributes)) -> Just "request_type;neutral⇒outgoing"
              (_, _, _) | isJust (atMapText "db.system.name" (unAesonTextMaybe otel.attributes) <|> atMapText "db.system" (unAesonTextMaybe otel.attributes)) -> Just "kind;neutral⇒database"
              (Just "internal", _, _) -> Just "kind;neutral⇒internal"
              _ -> Nothing
          ]
        ++ [ case atMapInt "http.response.status_code" (unAesonTextMaybe otel.attributes) of
               Just code -> Just $ "status_code;" <> statusCodeStyle code <> "⇒" <> toText (show code)
               _ -> Nothing
           ]
        ++ [ case atMapText "http.request.method" (unAesonTextMaybe otel.attributes) of
               Just method -> Just $ "method;" <> methodStyle method <> "⇒" <> method
               _ -> Nothing
           ]
        ++ [ case (atMapText "http.route" (unAesonTextMaybe otel.attributes), atMapText "url.path" (unAesonTextMaybe otel.attributes)) of
               (Just route, _) -> Just $ "route;neutral⇒" <> route
               (_, Just url) -> Just $ "url;neutral⇒" <> url
               _ -> Nothing
           ]
        ++ [ case (atMapText "db.system.name" (unAesonTextMaybe otel.attributes), atMapText "db.system" (unAesonTextMaybe otel.attributes)) of
               (Just system, _) -> Just $ "db.system;neutral⇒" <> system
               (_, Just system) -> Just $ "db.system;neutral⇒" <> system
               _ -> Nothing
           , case ( atMapText "db.system.name" (unAesonTextMaybe otel.attributes) <|> atMapText "db.system" (unAesonTextMaybe otel.attributes)
                  , atMapText "db.query.text" (unAesonTextMaybe otel.attributes)
                  ) of
               (Just _, Just queryText) -> Just $ "db.query.text;text-textStrong⇒" <> T.take 200 queryText
               _ -> Nothing
           , case atMapText "db.statement" (unAesonTextMaybe otel.attributes) of
               Just stmt -> Just $ "db.statement;neutral⇒" <> T.take 200 stmt
               _ -> Nothing
           ]
        ++ [ case atMapText "rpc.method" (unAesonTextMaybe otel.attributes) of
               Just method -> Just $ "rpc.method;neutral⇒" <> method
               _ -> Nothing
           , case atMapText "rpc.service" (unAesonTextMaybe otel.attributes) of
               Just service -> Just $ "rpc.service;neutral⇒" <> service
               _ -> Nothing
           ]
        ++ [ case otel.name of
               Just n ->
                 case (atMapText "http.route" (unAesonTextMaybe otel.attributes), atMapText "url.path" (unAesonTextMaybe otel.attributes)) of
                   (Nothing, Nothing) -> Just $ "span_name;neutral⇒" <> n
                   _ -> Nothing
               _ -> Nothing
           ]
        ++ [ case (otel.status_code, atMapInt "http.response.status_code" (unAesonTextMaybe otel.attributes)) of
               (Just "ERROR", Just httpStatus) | httpStatus >= 400 -> Nothing
               (Just "ERROR", _) -> Just "status;badge-error⇒ERROR"
               _ -> Nothing
           ]
        ++ [ case unAesonTextMaybe otel.attributes of
               Just attrs
                 | not (Map.null attrs) ->
                     let attrText = decodeUtf8 (AE.encode attrs)
                         truncated =
                           if T.length attrText > 500
                             then T.take 497 attrText <> "..."
                             else attrText
                      in Just $ "attributes;text-textWeak⇒" <> truncated
               _ -> Nothing
           ]
        ++ [ case atMapText "session.id" (unAesonTextMaybe otel.attributes) of
               Just v -> Just $ "session;right-badge-neutral⇒" <> v
               _ -> Nothing
           , case atMapText "user.email" (unAesonTextMaybe otel.attributes) of
               Just eml -> Just $ "user email;right-badge-neutral⇒" <> eml
               _ -> case atMapText "user.id" (unAesonTextMaybe otel.attributes) of
                 Just s -> Just $ "user name;right-badge-neutral⇒" <> s
                 _ -> Nothing
           , case atMapText "user.full_name" (unAesonTextMaybe otel.attributes) of
               Just s -> Just $ "user name;right-badge-neutral⇒" <> s
               _ -> case atMapText "user.name" (unAesonTextMaybe otel.attributes) of
                 Just s -> Just $ "user name;right-badge-neutral⇒" <> s
                 _ -> Nothing
           , case (otel.status_code, atMapInt "http.response.status_code" (unAesonTextMaybe otel.attributes)) of
               (Just "ERROR", Just httpStatus) | httpStatus >= 400 -> Nothing
               (Just "ERROR", _) -> Just "status;right-badge-error⇒ERROR"
               _ -> Nothing
           , case (atMapText "db.system.name" (unAesonTextMaybe otel.attributes), atMapText "db.system" (unAesonTextMaybe otel.attributes)) of
               (Just "postgresql", _) -> Just "db.system;right-badge-postgres⇒postgres"
               (_, Just "postgresql") -> Just "db.system;right-badge-postgres⇒postgres"
               (Just "mysql", _) -> Just "db.system;right-badge-mysql⇒mysql"
               (_, Just "mysql") -> Just "db.system;right-badge-mysql⇒mysql"
               (Just "redis", _) -> Just "db.system;right-badge-redis⇒redis"
               (_, Just "redis") -> Just "db.system;right-badge-redis⇒redis"
               (Just "mongodb", _) -> Just "db.system;right-badge-mongo⇒mongodb"
               (_, Just "mongodb") -> Just "db.system;right-badge-mongo⇒mongodb"
               (Just "elasticsearch", _) -> Just "db.system;right-badge-elastic⇒elastic"
               (_, Just "elasticsearch") -> Just "db.system;right-badge-elastic⇒elastic"
               (Just system, _) -> Just $ "db.system;right-badge-neutral⇒" <> system
               (_, Just system) -> Just $ "db.system;right-badge-neutral⇒" <> system
               _ -> Nothing
           , if hasHttp then Just "protocol;right-badge-neutral⇒http" else Nothing
           , case atMapText "rpc.method" (unAesonTextMaybe otel.attributes) of
               Just _ -> Just "protocol;right-badge-neutral⇒rpc"
               _ -> Nothing
           , case otel.duration of
               Just dur -> Just $ "duration;right-badge-neutral⇒" <> toText (getDurationNSMS (fromIntegral dur))
               _ ->
                 Nothing
           ]
   in
    V.fromList elements


statusCodeStyle :: Int -> T.Text
statusCodeStyle code
  | code < 200 = "badge-neutral"
  | code < 300 = "badge-2xx"
  | code < 400 = "badge-3xx"
  | code < 500 = "badge-4xx"
  | otherwise = "badge-5xx"


methodStyle :: T.Text -> T.Text
methodStyle method = case method of
  "GET" -> "badge-GET"
  "POST" -> "badge-POST"
  "PUT" -> "badge-PUT"
  "DELETE" -> "badge-DELETE"
  "PATCH" -> "badge-PATCH"
  _ -> "badge-neutral"
