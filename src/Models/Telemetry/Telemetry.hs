{-# LANGUAGE StrictData #-}

module Models.Telemetry.Telemetry (
  LogRecord (..),
  logRecordByProjectAndId,
  logRecordById,
  spanRecordByProjectAndId,
  getSpanRecordsByTraceId,
  getSpanRecordsByTraceIds,
  convertOtelLogsAndSpansToSpanRecord,
  getTotalEventsToReport,
  getUsageTotals,
  SpanRecord (..),
  getAllATErrors,
  isErrorRecord,
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
  getEndpointTraceId,
  getTotalMetricsCount,
  getMetricData,
  bulkInsertMetrics,
  bulkInsertOtelLogsAndSpansTF,
  insertAndHandOff,
  WriteTarget (..),
  writeTargetFor,
  WriteFailure,
  writeFailureSummary,
  writeFailureDlqHeaders,
  PoisonMsg,
  BulkInsertResult (..),
  RowPoisonInfo,
  SilentUnderPersistError (..),
  unaccountedRows,
  retryHasqlWrite,
  retryTransientEff,
  maxWriteAttempts,
  maxReadAttempts,
  poisonReason,
  handOffBatches,
  mintOtelLogIds,
  getMetricChartListData,
  getMetricLabelValues,
  getTraceShapes,
  getMetricServiceNames,
  resourceServiceName,
  metricServiceNameFromResource,
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
  otelSpanColsSql,
  bisectCap,
)
where

import Control.Exception.Annotated (checkpoint)
import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as KEM
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Default (Default (..))
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Data.Generics.Labels ()
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as L (groupBy)
import Data.List.Extra (chunksOf)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Display (Display)
import Data.These (These (..))
import Data.These qualified as These
import Data.Time (UTCTime)
import Data.Time.Clock (addUTCTime, utctDay)
import Data.UUID qualified as UUID
import Data.UUID.Quasi (uuid)
import Data.UUID.V5 qualified as UUIDv5
import Data.Vector qualified as V
import Data.Vector.Split qualified as VS
import Database.PostgreSQL.Simple.FromField (Conversion, FromField (..))
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
import Hasql.DynamicStatements.Snippet (Snippet, encoderAndParam, param, toPreparableStatement)
import Hasql.Encoders qualified as E
import Hasql.Interpolate qualified as HI
import Hasql.Session qualified as HSession
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (AesonText (..), DB, UUIDId (..), WrappedEnum (..), WrappedEnumInt (..), WrappedEnumSC (..), encodeEnumSC, idFromText, textArrayEnc, unAesonTextMaybe)
import Pkg.ExtractionWorker qualified as EW
import Relude hiding (ask)
import Relude.Extra.Tuple (traverseToSnd)
import System.IO (hPutStrLn)
import System.Logging qualified as Log
import System.Tracing (forkWithCtx)
import Text.Regex.TDFA.Text ()
import UnliftIO (throwIO, tryAny)
import Utils (extractMessageFromLog, getDurationNSMS, lookupValueText, scrubNulText, scrubNulValue)


-- Helper function to get nested value from a map using dot notation
getNestedValue :: [Text] -> Map Text AE.Value -> Maybe AE.Value
getNestedValue [] _ = Nothing
getNestedValue [k] m = Map.lookup k m
getNestedValue ks@(k : rest) m =
  Map.lookup (T.intercalate "." ks) m <|> do
    v <- Map.lookup k m
    case v of
      AE.Object obj -> getNestedValue rest (KEM.toMapText obj)
      _ -> Nothing


-- Lens-like access helpers for Map Text AE.Value fields
atMapText :: Text -> Maybe (Map Text AE.Value) -> Maybe Text
atMapText key maybeMap = do
  m <- maybeMap
  val <- getNestedValue (T.split (== '.') key) m
  case val of
    AE.String t -> Just $ scrubNulText t
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
spanServiceName s = resourceServiceName (unAesonTextMaybe s.resource)


resourceServiceName :: Maybe (Map Text AE.Value) -> Maybe Text
resourceServiceName resource =
  atMapText "service.name" resource
    <|> flatAttrText "service.name" resource


flatAttrText :: Text -> Maybe (Map Text AE.Value) -> Maybe Text
flatAttrText key maybeMap = do
  m <- maybeMap
  val <- Map.lookup key m
  case val of
    AE.String t -> Just $ scrubNulText t
    AE.Number n -> Just $ show n
    _ -> Nothing


data SeverityLevel = SLTrace | SLDebug | SLInfo | SLWarn | SLError | SLFatal
  deriving (Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "SL" SeverityLevel


data SpanStatus = SSOk | SSError | SSUnset
  deriving (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "SS" SpanStatus


data SpanKind = SKInternal | SKServer | SKClient | SKProducer | SKConsumer | SKUnspecified
  deriving (Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "SK" SpanKind


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


-- JSONB/text decode for Map Text Value reuses the generic AesonText decoder in DeriveUtils.
instance FromField (Map Text AE.Value) where
  fromField f mdata = coerce (fromField f mdata :: Conversion (AesonText (Map Text AE.Value)))


instance ToField (Map Text AE.Value) where
  toField = toField . AesonText


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
  , messageSizeBytes :: Int64
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
  deriving (HI.DecodeValue, HI.EncodeValue) via Aeson MetricValue
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
  deriving (HI.DecodeValue, HI.EncodeValue) via WrappedEnum "MT" MetricType


data AggregationTemporality = ATUnspecified | ATDelta | ATCumulative
  deriving (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake AggregationTemporality
  deriving (FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumInt AggregationTemporality


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
            WHERE project_id = #{pid.toText} AND timestamp BETWEEN #{start} AND #{end} AND context___trace_id = #{trId}
            GROUP BY context___trace_id |]
  pure $ (\(tid, ts, te, dur, ns, sn) -> Trace tid ts te (fromIntegral dur) (fromIntegral ns) sn) <$> (rows V.!? 0)
  where
    (start, end) = case tme of
      Nothing -> (addUTCTime (-(14 * 24 * 3600)) now, now)
      Just ts -> (addUTCTime (-(60 * 5)) ts, addUTCTime (60 * 5) ts)


logRecordByProjectAndId :: DB es => Projects.ProjectId -> UTCTime -> UUID.UUID -> Eff es (Maybe OtelLogsAndSpans)
logRecordByProjectAndId pid = lookupOtelRecord (Just pid.toText)


-- | Lookup by id + timestamp only (no project_id filter). Used as fallback for demo project where
-- session data may span multiple projects.
logRecordById :: DB es => UTCTime -> UUID.UUID -> Eff es (Maybe OtelLogsAndSpans)
logRecordById = lookupOtelRecord Nothing


-- | Full column list for SELECT against otel_logs_and_spans, in the field
-- order 'OtelLogsAndSpans' expects. Centralised so the four trace/log lookups
-- below can't drift out of sync.
otelSpanColsSql :: HI.Sql
otelSpanColsSql =
  -- NB: no COALESCE(hashes, '{}') — DataFusion can't coerce a Utf8 literal to
  -- List(Utf8View), which 500s every TF-routed lookup. NULL (legacy PG rows)
  -- is absorbed by the Maybe on the 'hashes' field instead.
  --
  -- 'date' fills the partition column position. On PG it's TIMESTAMPTZ, but on
  -- TF it's the Date32 Hive partition key, and DataFusion's date::timestamptz
  -- drops the tz → wire OID 1114 (timestamp) ≠ the UTCTime decoder's 1184
  -- (timestamptz), crashing every TF-routed lookup at this column. Since 'date'
  -- equals 'timestamp' on write and the decoded value is never read, project
  -- the already-tz-aware 'timestamp' here so both stores agree on 1184.
  [HI.sql|project_id, id::text, timestamp, observed_timestamp, context, level, severity, body, attributes, resource,
          hashes, kind, status_code, status_message, COALESCE(start_time, timestamp), end_time, events, links, duration, name, parent_id, summary, timestamp AS date, errors, COALESCE(message_size_bytes, 0)|]


-- | Shared SELECT prefix for span fetchers: full column list, project + timestamp window.
-- The caller appends its own trailing clause, including the leading @AND@ for any extra
-- predicate (so an empty @predSql@ still yields valid SQL).
selectOtelSpans :: Text -> UTCTime -> UTCTime -> HI.Sql -> HI.Sql
selectOtelSpans pidTxt lo hi predSql =
  [HI.sql|SELECT |]
    <> otelSpanColsSql
    <> [HI.sql| FROM otel_logs_and_spans WHERE project_id=#{pidTxt} AND timestamp BETWEEN #{lo} AND #{hi} |]
    <> predSql


-- | Random-access lookup of a single row by exact (timestamp, id). The caller always
-- has the precise stored timestamp (it originates from a prior query result), so we match
-- @timestamp = ts@ rather than a window — both PG and TF store microsecond precision, which
-- round-trips losslessly through the UTCTime↔Hasql encoder. Exact equality also lets the
-- planner hit the (timestamp, id) ordering instead of scanning a ±window range.
lookupOtelRecord :: DB es => Maybe Text -> UTCTime -> UUID.UUID -> Eff es (Maybe OtelLogsAndSpans)
lookupOtelRecord mPid createdAt rdId = do
  let pidFilter = maybe mempty (\p -> [HI.sql| and project_id=#{p}|]) mPid
  Hasql.interpOne
    $ [HI.sql|SELECT |]
    <> otelSpanColsSql
    <> [HI.sql| FROM otel_logs_and_spans where timestamp = #{createdAt}|]
    <> pidFilter
    <> [HI.sql| and id=#{rdId} LIMIT 1|]


-- | First/recent trace_id for a (project, method, url_path). Window kept tight (7d) since
-- endpoint-anomaly issues are minutes-to-hours old; tie-broken by id for stable nav between
-- "First" and "Recent" page loads. We UNION ALL two index-friendly subqueries (one per
-- url-path source) instead of OR'ing them in a single WHERE, so each side can hit its own
-- expression index and avoid a 7d seq scan on otel_logs_and_spans.
getEndpointTraceId :: (Hasql :> es, IOE :> es) => Projects.ProjectId -> Text -> Text -> Bool -> UTCTime -> Eff es (Maybe Text)
getEndpointTraceId pid method urlPath isFirst now =
  Hasql.interpOne
    $ [HI.sql| SELECT context___trace_id FROM ( |]
    <> branch [HI.sql| attributes___url___path = #{urlPath} |]
    <> [HI.sql| UNION ALL |]
    <> branch [HI.sql| attributes->'http'->>'route' = #{urlPath} |]
    <> [HI.sql| ) c |]
    <> orderSql
    <> [HI.sql| LIMIT 1 |]
  where
    windowStart = addUTCTime (-(7 * 24 * 3600)) now
    orderSql =
      if isFirst
        then [HI.sql| ORDER BY start_time ASC, id ASC |]
        else [HI.sql| ORDER BY start_time DESC, id DESC |]
    branch pathPred =
      [HI.sql| (SELECT context___trace_id, start_time, id FROM otel_logs_and_spans
                WHERE project_id = #{pid.toText}
                  AND timestamp BETWEEN #{windowStart} AND #{now}
                  AND attributes___http___request___method = #{method}
                  AND |]
        <> pathPred
        <> [HI.sql| AND context___trace_id IS NOT NULL |]
        <> orderSql
        <> [HI.sql| LIMIT 1) |]


-- | Up to this many extra round-trips to chase chains of late-ingested parents.
maxOrphanResolverHops :: Int
maxOrphanResolverHops = 3


-- | Parent_ids referenced by some span in the batch but not present (and not
-- empty). Drives both the iterative resolver and the unresolved-orphan log.
orphanParentIds :: [OtelLogsAndSpans] -> [Text]
orphanParentIds rs =
  let present = S.fromList $ mapMaybe (\r -> r.context >>= (.span_id)) rs
   in ordNub [p | r <- rs, Just p <- [r.parent_id], not (T.null p), not (S.member p present)]


getSpanRecordsByTraceId :: (DB es, Log :> es) => Projects.ProjectId -> Text -> Maybe UTCTime -> UTCTime -> Eff es [OtelLogsAndSpans]
getSpanRecordsByTraceId pid trId tme now = do
  let baseT = fromMaybe now tme
      (start, end) = case tme of
        Nothing -> (addUTCTime (-(14 * 24 * 3600)) now, now)
        Just ts -> (addUTCTime (-300) ts, addUTCTime 300 ts)
      (wideStart, wideEnd) = (addUTCTime (-86400) baseT, addUTCTime 86400 baseT)
      resolveHop ids =
        Hasql.interp
          $ selectOtelSpans
            pid.toText
            wideStart
            wideEnd
            [HI.sql| AND context___trace_id=#{trId} AND context___span_id = ANY(#{ids})|]
  initial :: [OtelLogsAndSpans] <-
    Hasql.interp
      $ selectOtelSpans
        pid.toText
        start
        end
        [HI.sql| AND context___trace_id=#{trId} ORDER BY start_time ASC|]
  -- Resolve missing parents iteratively: a parent ingested outside ±5min
  -- (clock skew, late ingestion, async batch) is looked up via
  -- context___span_id within ±24h. Recovered spans may themselves have a
  -- missing parent — loop up to 'maxOrphanResolverHops'. Anything still
  -- unresolved becomes a synthetic placeholder in 'buildSpanTree' and is
  -- logged so SREs can correlate with ingestion incidents.
  let go !acc !hop !ids
        | hop >= maxOrphanResolverHops || null ids = pure acc
        | otherwise = do
            extras <- resolveHop ids
            if null extras
              then pure acc -- no progress: remaining ids are truly absent
              else let acc' = acc <> extras in go acc' (hop + 1) (orphanParentIds acc')
  resolved <- go initial 0 (orphanParentIds initial)
  whenNotNull (orphanParentIds resolved) \stillMissing ->
    Log.logAttention "TRACE_ORPHAN_PARENTS_UNRESOLVED"
      $ AE.object
        [ "project_id" AE..= pid.toText
        , "trace_id" AE..= trId
        , "missing_count" AE..= length stillMissing
        , "missing_sample" AE..= NE.take 5 stillMissing
        ]
  pure resolved


getSpanRecordsByTraceIds :: (DB es, Time.Time :> es) => Projects.ProjectId -> V.Vector Text -> Maybe UTCTime -> Eff es [OtelLogsAndSpans]
getSpanRecordsByTraceIds pid traceIds tme = do
  now <- Time.currentTime
  let (start, end') = case tme of
        Nothing -> (addUTCTime (-86400) now, now)
        Just ts -> (addUTCTime (-300) ts, addUTCTime 300 ts)
      traceIdsList = V.toList traceIds
  Hasql.interp
    $ selectOtelSpans
      pid.toText
      start
      end'
      [HI.sql| AND context___trace_id = ANY(#{traceIdsList}) ORDER BY context___trace_id ASC, start_time ASC|]


spanRecordByProjectAndId :: DB es => Projects.ProjectId -> UTCTime -> UUID.UUID -> Eff es (Maybe OtelLogsAndSpans)
spanRecordByProjectAndId pid = lookupOtelRecord (Just pid.toText)


spanRecordByName :: DB es => Projects.ProjectId -> Text -> Text -> Eff es (Maybe OtelLogsAndSpans)
spanRecordByName pid trId spanName = do
  Hasql.interpOne
    $ [HI.sql| SELECT |]
    <> otelSpanColsSql
    <> [HI.sql| FROM otel_logs_and_spans where project_id=#{pid.toText} and context___trace_id = #{trId} and name=#{spanName} LIMIT 1|]


getDataPointsData :: (DB es, Time.Time :> es) => Projects.ProjectId -> (Maybe UTCTime, Maybe UTCTime) -> Eff es [MetricDataPoint]
getDataPointsData pid dateRange = do
  now <- Time.currentTime
  let dateFilter = case dateRange of
        (Nothing, Just b) -> [HI.sql| AND timestamp BETWEEN #{now} AND #{b} |]
        (Just a, Just b) -> [HI.sql| AND timestamp BETWEEN #{a} AND #{b} |]
        _ -> mempty
  Hasql.interp
    $ [HI.sql| WITH metrics_aggregated AS (
        SELECT project_id, metric_name, COUNT(*)::bigint AS data_points
        FROM telemetry.metrics
        WHERE project_id = #{pid} |]
    <> dateFilter
    <> [HI.sql|
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
getMetricData pid metricName =
  Hasql.interpOne
    [HI.sql| SELECT mm.metric_name, mm.metric_type, mm.metric_unit, mm.metric_description,
           COALESCE(m.data_points, 0) AS data_points,
           COALESCE(m.service_names, ARRAY[mm.service_name]::text[]) AS service_names,
           COALESCE(m.metric_labels, ARRAY[]::text[]) AS metric_labels
      FROM telemetry.metrics_meta mm
      LEFT JOIN LATERAL (
          SELECT COUNT(*)::bigint AS data_points,
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
  fromMaybe 0 <$> Hasql.interpOne [HI.sql| SELECT count(*)::bigint FROM otel_logs_and_spans WHERE project_id=#{pid.toText} AND timestamp > #{lastReported}|]


getTotalMetricsCount :: DB es => Projects.ProjectId -> UTCTime -> Eff es Int
getTotalMetricsCount pid lastReported =
  fromMaybe 0 <$> Hasql.interpOne [HI.sql| SELECT count(*)::bigint FROM telemetry.metrics WHERE project_id=#{pid} AND timestamp > #{lastReported}|]


-- | (eventCount, eventBytes, metricCount, metricBytes) for a project since
-- `lastReported`. Single helper so ReportUsage stays a 1-call site instead of
-- juggling four separate queries.
getUsageTotals :: DB es => Projects.ProjectId -> UTCTime -> Eff es (Int, Int64, Int, Int64)
getUsageTotals pid lastReported = do
  (eC, eB) <- fromMaybe (0, 0) <$> Hasql.interpOne [HI.sql| SELECT count(*)::bigint, COALESCE(SUM(message_size_bytes),0)::bigint FROM otel_logs_and_spans WHERE project_id=#{pid.toText} AND timestamp > #{lastReported}|]
  (mC, mB) <- fromMaybe (0, 0) <$> Hasql.interpOne [HI.sql| SELECT count(*)::bigint, COALESCE(SUM(message_size_bytes),0)::bigint FROM telemetry.metrics WHERE project_id=#{pid} AND timestamp > #{lastReported}|]
  pure (eC, eB, mC, mB)


getMetricChartListData :: DB es => Projects.ProjectId -> Maybe Text -> Maybe Text -> Eff es [MetricChartListData]
getMetricChartListData pid sourceM prefixM = do
  let sourceFilter = case sourceM of
        Just source | source /= "" && source /= "all" -> [HI.sql| AND service_name = #{source}|]
        _ -> mempty
      prefixFilter = case prefixM of
        Just prefix | prefix /= "" && prefix /= "all" -> let pat = prefix <> "%" in [HI.sql| AND metric_name LIKE #{pat}|]
        _ -> mempty
  Hasql.interp
    $ [HI.sql| SELECT metric_name, MAX(metric_type) as metric_type, MAX(metric_unit) as metric_unit,
             MAX(metric_description) as metric_description, MAX(updated_at) as last_seen
      FROM telemetry.metrics_meta WHERE project_id = #{pid} |]
    <> sourceFilter
    <> prefixFilter
    <> [HI.sql| GROUP BY metric_name ORDER BY MAX(updated_at) DESC, metric_name |]


getMetricLabelValues :: DB es => Projects.ProjectId -> Text -> Text -> Eff es [Text]
getMetricLabelValues pid metricName labelName =
  Hasql.interp [HI.sql| SELECT DISTINCT attributes->>#{labelName} FROM telemetry.metrics WHERE project_id = #{pid} AND metric_name = #{metricName}|]


getMetricServiceNames :: DB es => Projects.ProjectId -> Eff es [Text]
getMetricServiceNames pid =
  Hasql.interp [HI.sql| SELECT DISTINCT service_name FROM telemetry.metrics_meta WHERE project_id = #{pid}|]


bulkInsertMetrics :: DB es => V.Vector MetricRecord -> Eff es ()
bulkInsertMetrics metrics = checkpoint "bulkInsertMetrics" $ unless (V.null metrics) do
  -- 24 params per row; PostgreSQL limit is 65535 params → max ~2730 rows per INSERT
  forM_ (chunksOf 2700 $ V.toList metrics) \chunk -> do
    let metricRows = map metricRowSql chunk
    Hasql.interpExecute_ $ "INSERT INTO telemetry.metrics (project_id, metric_name, metric_type, metric_unit, metric_description, metric_time, timestamp, attributes, resource, instrumentation_scope, metric_value, exemplars, flags, value, metric_sum, metric_count, bucket_counts, explicit_bounds, point_min, point_max, quantiles, aggregation_temporality, is_monotonic, message_size_bytes) VALUES " <> mconcat (intersperse ", " metricRows)
  let metas = map metaRowSql $ removeDuplic $ V.toList $ V.map metaTuple metrics
  unless (null metas) $ Hasql.interpExecute_ $ "INSERT INTO telemetry.metrics_meta (project_id, metric_name, metric_type, metric_unit, metric_description, service_name) VALUES " <> mconcat (intersperse ", " metas) <> " ON CONFLICT (project_id, metric_name, service_name) DO UPDATE SET metric_type = EXCLUDED.metric_type, metric_unit = EXCLUDED.metric_unit, metric_description = EXCLUDED.metric_description, updated_at = current_timestamp"
  where
    metricRowSql (m :: MetricRecord) =
      let NativeMetricColumns{nValue, nSum, nCount, nBucketCounts, nExplicitBounds, nPointMin, nPointMax, nQuantiles} = metricValueToNative m.metricValue
          MetricRecord{projectId, metricName, metricType, metricUnit, metricDescription, metricTime, timestamp, attributes, resource, instrumentationScope, metricValue, exemplars, flags, aggregationTemporality, isMonotonic, messageSizeBytes} = m
       in [HI.sql|(#{projectId}, #{metricName}, #{metricType}, #{metricUnit}, #{metricDescription}, #{metricTime}, #{timestamp}, #{attributes}, #{resource}, #{instrumentationScope}, #{metricValue}, #{exemplars}, #{flags}, #{nValue}, #{nSum}, #{nCount}, #{nBucketCounts}, #{nExplicitBounds}, #{nPointMin}, #{nPointMax}, #{nQuantiles}, #{aggregationTemporality}, #{isMonotonic}, #{messageSizeBytes})|]
    metaTuple (m :: MetricRecord) =
      let svc = metricServiceNameFromResource m.metricName m.resource
          MetricRecord{projectId, metricName, metricType, metricUnit, metricDescription} = m
       in (projectId, metricName, metricType, metricUnit, metricDescription, svc)
    metaRowSql (pid, mName, mType, mUnit, mDesc, svc) =
      [HI.sql|(#{pid}, #{mName}, #{mType}, #{mUnit}, #{mDesc}, #{svc})|]


metricServiceNameFromResource :: Text -> AE.Value -> Text
metricServiceNameFromResource metricName resource =
  fromMaybe "unknown" $
    resourceServiceName (aesonObjectMap resource)
      <|> nestedText ["container", "name"] resource
      <|> lookupValueText resource "compose_service"
      <|> if "system." `T.isPrefixOf` metricName then Just "SYSTEM" else Nothing
  where
    aesonObjectMap :: AE.Value -> Maybe (Map Text AE.Value)
    aesonObjectMap (AE.Object obj) = Just $ KEM.toMapText obj
    aesonObjectMap _ = Nothing

    nestedText :: [Text] -> AE.Value -> Maybe Text
    nestedText path (AE.Object obj) = do
      val <- getNestedValue path (KEM.toMapText obj)
      case val of
        AE.String t -> Just t
        AE.Number n -> Just $ show n
        _ -> Nothing
    nestedText _ _ = Nothing


-- | Fixed namespace for the content-derived (v5) row ids below. Arbitrary but
-- stable — changing it re-ids every row, so don't.
otelIdNamespace :: UUID.UUID
otelIdNamespace = [uuid|6f1a7c30-9b2d-5e84-8a3f-0c1d2e3f4a5b|]


-- | Content-derived UUID (v5) for a row's `id`. Reprocessing a dead-letter
-- message re-parses the original payload and re-derives the SAME id it would
-- have had on first ingest, so TF's @(id, timestamp)@ dedup (flush + read-side
-- row_number) collapses the replay instead of appending a fresh random row —
-- the source of the 06-19 duplicate pile-up. Random v4 (the old behaviour)
-- defeated that dedup because every replay produced a new id.
--
-- Identity: spans key on the OTel-unique @(project_id, trace_id, span_id)@,
-- matching TF's "same span id at the same timestamp collapses retries" intent
-- regardless of attribute drift; logs (no span_id) key on stable content
-- @(project_id, body, name, severity, attributes, resource)@ so distinct logs
-- that merely share a timestamp stay separate while true repeats collapse.
-- Deliberately excludes @timestamp@ (it is the *other* dedup key, so it
-- distinguishes same-content/different-time events) and the parse-time fallback
-- fields (@observed_timestamp@/@start_time@ → currentTime) which aren't stable
-- across reprocessing.
deterministicOtelId :: OtelLogsAndSpans -> Text
deterministicOtelId r = UUID.toText $ UUIDv5.generateNamed otelIdNamespace $ intercalate [0x1f] (map BS.unpack keyParts)
  where
    -- JSON-encode every part (incl. project_id/name) so the 0x1f delimiter can
    -- never appear inside a part (Aeson escapes it). Raw encodeUtf8 would let two
    -- distinct records collide on the same key via separator injection.
    enc :: AE.ToJSON a => Maybe a -> BS.ByteString
    enc = maybe "" (toStrict . AE.encode)
    keyParts = case r.context >>= (.span_id) of
      Just sid
        | not (T.null sid) ->
            [enc (Just r.project_id), enc (r.context >>= (.trace_id)), enc (Just sid)]
      _ ->
        [ enc (Just r.project_id)
        , enc (unAesonTextMaybe r.body)
        , enc r.name
        , enc r.severity
        , enc (unAesonTextMaybe r.attributes)
        , enc (unAesonTextMaybe r.resource)
        ]


-- | Assign every row a deterministic, content-derived `id` (see
-- 'deterministicOtelId'). Pure and idempotent: same input record ⇒ same id, so
-- the dead-letter replay path lands ids that dedup against the original ingest.
-- Call once at the ingestion call site BEFORE 'bulkInsertOtelLogsAndSpansTF' so
-- the caller's vector ids match what lands in the DB (the extraction-worker
-- hand-off relies on this).
mintOtelLogIds :: V.Vector OtelLogsAndSpans -> V.Vector OtelLogsAndSpans
mintOtelLogIds = V.map \r -> r & #id .~ deterministicOtelId r


-- | This = PG failed; That = TF failed; These = both. Both stores are mandatory
-- because dashboards still read from PG; any failure must reach the DLQ via
-- 'Pkg.Queue' — never silent-ack.
type WriteFailure = These SomeException SomeException


-- | Which stores a (re)write should attempt. Live ingest writes both; DLQ
-- replay narrows to the leg that originally failed so the still-durable leg
-- isn't duplicated (PG inserts aren't idempotent — a 'tf-failed' message is
-- already in PG, dual-write replay would double the row).
data WriteTarget = WriteBoth | WritePgOnly | WriteTfOnly
  deriving stock (Eq, Ord, Show)


-- | Derive the write target from the global TF flag and an optional
-- @monoscope-write-failure@ header (present on DLQ replay). A 'tf-failed' replay
-- presumes TF was enabled when it failed, so it always targets TF; everything
-- else honours @enableTf@ (TF off ⇒ PG only).
--
-- >>> map (writeTargetFor True) [Just "tf-failed", Just "pg-failed", Just "both-failed", Nothing]
-- [WriteTfOnly,WritePgOnly,WriteBoth,WriteBoth]
-- >>> writeTargetFor False Nothing
-- WritePgOnly
writeTargetFor :: Bool -> Maybe Text -> WriteTarget
writeTargetFor enableTf = \case
  Just "tf-failed" -> WriteTfOnly
  Just "pg-failed" -> WritePgOnly
  _ -> if enableTf then WriteBoth else WritePgOnly


-- | A message that couldn't be parsed/converted. Callers MUST publish these to
-- the DLQ before committing source-topic offsets so the bytes are preserved
-- for manual investigation (never silent-drop).
type PoisonMsg =
  (Text, ByteString, Text)
  -- ^ @(ackId, rawBytes, errorReason)@


-- | Per-row outcome from a dual-write. @This pgErr@ = PG-only failure;
-- @That tfErr@ = TF-only failure; @These pgErr tfErr@ = both failed. A row
-- accepted by both stores never appears here, so the no-error case is
-- structurally absent (no need for a defensive Nothing-Nothing branch in callers).
type RowPoisonInfo = These SomeException SomeException


-- | Human-readable per-row error reason for DLQ payload. Capped at 2 KiB per
-- side so a multi-MB nested exception can't blow past broker message limits;
-- the full stack stays in the logAttention emitted at the write site.
poisonReason :: RowPoisonInfo -> Text
poisonReason =
  These.these
    (\pg -> "row insert failed (pg-only): " <> truncErr pg)
    (\tf -> "row insert failed (tf-only): " <> truncErr tf)
    (\pg tf -> "row insert failed (both): pg=" <> truncErr pg <> "; tf=" <> truncErr tf)
  where
    truncErr = T.take 2048 . toText . displayException


-- | Result of a single-side ('bulkInsertOtelLogsAndSpans') bulk write. Rows
-- that bisect-isolated to a non-transient failure are surfaced here instead of
-- silently dropped — caller routes them to the DLQ.
data BulkInsertResult = BulkInsertResult
  { rowsInserted :: !Int64
  , poisonRows :: !(V.Vector (OtelLogsAndSpans, SomeException))
  }


instance Semigroup BulkInsertResult where
  BulkInsertResult n1 p1 <> BulkInsertResult n2 p2 = BulkInsertResult (n1 + n2) (p1 <> p2)


instance Monoid BulkInsertResult where
  mempty = BulkInsertResult 0 V.empty


-- | A store accepted a bulk write without raising an error, yet persisted
-- fewer rows than were submitted and returned no per-row poison to isolate the
-- gap — the "accept-but-drop" class: TimeFusion's PGWire @TuplesOk@ wire-tag
-- swallow, or any backend that silently discards. A reported success is NOT
-- proof the rows landed; surfacing this as a 'WriteFailure' forces the batch to
-- the DLQ instead of a silent ack. @lostRows@ vanished out of @submittedRows@.
data SilentUnderPersistError = SilentUnderPersistError
  { store :: !Text
  , lostRows :: !Int
  , submittedRows :: !Int
  }
  deriving stock (Show)
  deriving anyclass (Exception)


-- | Rows a single-side bulk write failed to account for: every submitted record
-- must either land ('rowsInserted') or be isolated as poison ('poisonRows'). A
-- positive result means the store reported success yet silently dropped that
-- many rows — the gap that bypasses every DLQ safety net.
--
-- >>> unaccountedRows 3 (BulkInsertResult 3 V.empty)
-- 0
-- >>> unaccountedRows 3 (BulkInsertResult 0 V.empty)
-- 3
-- >>> unaccountedRows 5 (BulkInsertResult 2 V.empty)
-- 3
unaccountedRows :: Int -> BulkInsertResult -> Int
unaccountedRows submitted bir =
  max 0 (submitted - fromIntegral bir.rowsInserted - V.length bir.poisonRows)


writeFailureSummary :: WriteFailure -> Text
writeFailureSummary = These.these (const "pg-failed") (const "tf-failed") (\_ _ -> "both-failed")


-- | DLQ headers a replay tool reads to rewrite only the missing side(s) and
-- avoid duplicating rows on the successful side.
--
-- >>> let e = toException (SilentUnderPersistError "x" 1 1)
-- >>> let look h = [HM.lookup k (writeFailureDlqHeaders h) | k <- ["monoscope-write-failure", "monoscope-pg-succeeded", "monoscope-tf-succeeded"]]
-- >>> (look (This e), look (That e), look (These e e))
-- ([Just "pg-failed",Just "false",Just "true"],[Just "tf-failed",Just "true",Just "false"],[Just "both-failed",Just "false",Just "false"])
writeFailureDlqHeaders :: WriteFailure -> HM.HashMap Text Text
writeFailureDlqHeaders wf =
  let (pgOk, tfOk) = These.these (const ("false", "true")) (const ("true", "false")) (\_ _ -> ("false", "false")) wf
   in HM.fromList
        [ ("monoscope-write-failure", writeFailureSummary wf)
        , ("monoscope-pg-succeeded", pgOk)
        , ("monoscope-tf-succeeded", tfOk)
        ]


-- | Cap on retry attempts per store inside 'retryHasqlWrite'. ~25s of wall
-- time at the exponential backoff used here. Both PG and TF call sites pass
-- this so a future tuning change moves both in lock-step.
maxWriteAttempts :: Int
maxWriteAttempts = 10


-- | Exponential backoff schedule shared by the write ('retryHasqlWrite') and
-- read ('retryTransientEff') retry loops: 100ms, 200ms, 400ms … capped at 5s.
transientBackoffMicros :: Int -> Int
transientBackoffMicros attempt = min 5000000 (100000 * (2 ^ (attempt - 1)))


-- | Retry a Hasql write up to @n@ times on transient errors with exponential
-- backoff (100ms → 5s cap; ~25s total at n=10). Non-transient errors return
-- 'Left' immediately. TimeFusion's PGWire "TuplesOk" wire-mismatch is treated
-- as success (rows landed; only the wire tag is wrong) — retrying would
-- duplicate rows. PG never produces "TuplesOk" so the swallow is a no-op there.
--
-- Generic over PG vs TF: both call sites are symmetric. Programmer-bug
-- exceptions (non-Hasql) propagate as Left without retry.
retryHasqlWrite
  :: (Concurrent :> es, IOE :> es, Log :> es, Monoid a)
  => Int
  -- ^ max attempts (must be >= 1)
  -> Text
  -- ^ store label for log lines ("pg" / "tf")
  -> Eff es a
  -- ^ write action; throws on transient failure (will retry)
  -> Eff es (Either SomeException a)
retryHasqlWrite maxAttempts store act = go 1
  where
    go attempt =
      tryAny act >>= \case
        Right a -> pure (Right a)
        Left e
          | "TuplesOk" `T.isInfixOf` show e -> do
              -- PGWire status tag is wrong; the rows MAY have landed. We can't
              -- tell from here, so we return a 0-row success rather than retry
              -- (which would duplicate any rows that did land). This deliberately
              -- loses the row count — the dual-write cross-check ('unaccountedRows'
              -- in 'bulkInsertOtelLogsAndSpansTF') is what verifies persistence and
              -- DLQs the batch if these rows are actually missing. Logged at
              -- attention (not trace) because a silent swallow here was the root of
              -- the 2026-06-12 TF gap where PG had the data and TF did not.
              Log.logAttention "retryHasqlWrite: TuplesOk wire-mismatch — 0-row success, persistence verified by cross-check" $ AE.object ["store" AE..= store]
              pure (Right mempty)
          | attempt < maxAttempts
          , Hasql.isTransientException e -> do
              let delayMicros = transientBackoffMicros attempt
              Log.logAttention "retryHasqlWrite: transient error, retrying"
                $ AE.object
                  [ "store" AE..= store
                  , "attempt" AE..= attempt
                  , "max_attempts" AE..= maxAttempts
                  , "backoff_us" AE..= delayMicros
                  , "error" AE..= show @Text e
                  ]
              threadDelay delayMicros
              go (attempt + 1)
          | otherwise -> pure (Left e)


-- | Read-side transient-retry budget. Smaller than 'maxWriteAttempts' (10):
-- a read blip blocking the consumer for the full ~25s write budget would stall
-- the partition, and the DLQ is a fine backstop for a read that won't recover.
maxReadAttempts :: Int
maxReadAttempts = 5


-- | Retry a read action on transient Hasql errors (dropped connection, empty
-- SQLSTATE from a pgdog/pgwire reset — see 'Data.Effectful.Hasql.isTransientUsageError')
-- with the same 100ms→5s backoff as 'retryHasqlWrite'. Rethrows the final
-- exception when the budget is exhausted or the error is non-transient, so the
-- caller's existing catch still routes the batch to the DLQ as the last resort.
-- Guards the per-batch project-id / cache lookups that previously dead-lettered
-- a whole batch on a single connection blip (the 2026-06-21 DLQ flood).
retryTransientEff :: (Concurrent :> es, IOE :> es, Log :> es) => Int -> Text -> Eff es a -> Eff es a
retryTransientEff maxAttempts op act = go 1
  where
    go attempt =
      tryAny act >>= \case
        Right a -> pure a
        Left e
          | attempt < maxAttempts
          , Hasql.isTransientException e -> do
              let delayMicros = transientBackoffMicros attempt
              Log.logAttention "retryTransientEff: transient read error, retrying"
                $ AE.object
                  [ "op" AE..= op
                  , "attempt" AE..= attempt
                  , "max_attempts" AE..= maxAttempts
                  , "backoff_us" AE..= delayMicros
                  , "error" AE..= show @Text e
                  ]
              threadDelay delayMicros
              go (attempt + 1)
          | otherwise -> throwIO e


-- | Dual-write to PG + TimeFusion concurrently. Both stores are mandatory.
--
--   * 'Left WriteFailure' — bounded transient-retry budget exhausted on at
--     least one side; the whole batch needs to be DLQ'd as a unit.
--   * 'Right' poisonRows — per-side bulk-insert returned poison rows
--     (constraint violations, schema mismatch, etc.). Each row carries
--     'RowPoisonInfo' saying which side(s) rejected it so the DLQ replay tool
--     can rewrite only the missing side. Empty vector = full success.
bulkInsertOtelLogsAndSpansTF
  :: ( Concurrent :> es
     , Hasql :> es
     , IOE :> es
     , Ki.StructuredConcurrency :> es
     , Labeled "timefusion" Hasql :> es
     , Log :> es
     )
  => WriteTarget
  -> V.Vector OtelLogsAndSpans
  -> Eff es (Either WriteFailure (V.Vector (OtelLogsAndSpans, RowPoisonInfo)))
bulkInsertOtelLogsAndSpansTF target records = do
  Log.logTrace "bulkInsertOtelLogsAndSpansTF called"
    $ AE.object [("record_count", AE.toJSON $ V.length records), ("write_target", AE.toJSON $ show @Text target)]
  case target of
    WriteBoth -> Ki.scoped \scope -> do
      pgThread <- forkWithCtx scope $ retryHasqlWrite maxWriteAttempts "pg" (bulkInsertOtelLogsAndSpans records)
      tfThread <- forkWithCtx scope $ retryHasqlWrite maxWriteAttempts "tf" (labeled @"timefusion" @Hasql $ bulkInsertOtelLogsAndSpans records)
      (pgRes, tfRes) <- Ki.atomically $ (,) <$> Ki.await pgThread <*> Ki.await tfThread
      classify pgRes tfRes
    -- Single-leg replay: only the store that originally failed is rewritten, so
    -- the durable leg keeps its single copy.
    WritePgOnly -> singleLeg "pg" This (,0) =<< retryHasqlWrite maxWriteAttempts "pg" (bulkInsertOtelLogsAndSpans records)
    WriteTfOnly -> singleLeg "tf" That (0,) =<< retryHasqlWrite maxWriteAttempts "tf" (labeled @"timefusion" @Hasql $ bulkInsertOtelLogsAndSpans records)
  where
    -- Classify one store's result; @side@ is 'This'/'That' for the written leg,
    -- @losts@ places the under-persist count on that leg ((l,0) for pg, (0,l) for tf).
    singleLeg store side losts = \case
      Right bir
        | lost <- unaccountedRows (V.length records) bir, lost > 0 -> let (pgL, tfL) = losts lost in underPersist (pgL, "pg") (tfL, "tf")
        | otherwise -> pure (Right (V.map (second side) bir.poisonRows))
      Left e -> do
        Log.logAttention (T.toUpper store <> "_WRITE_FAILED") (AE.object ["record_count" AE..= V.length records, "error" AE..= show @Text e])
        pure (Left (side e))
    -- Cross-check persistence on a Right/Right dual-write: a store that reported
    -- success but didn't account for every submitted row (inserted + poison <
    -- submitted) silently dropped data. Convert to a WriteFailure so Pkg.Queue
    -- DLQs the batch instead of acking it. Without this a TuplesOk-swallow /
    -- accept-but-drop reads as a clean success and the rows are lost with no
    -- trace (the 2026-06-12 TF-only gap).
    classify (Right pgBir) (Right tfBir) =
      let n = V.length records
       in case (unaccountedRows n pgBir, unaccountedRows n tfBir) of
            (0, 0) -> pure (Right (combinePoison pgBir tfBir))
            (pgLost, tfLost) -> underPersist (pgLost, "pg") (tfLost, "tf")
    classify (Left ePg) (Right _) = do
      Log.logAttention "PG_WRITE_FAILED"
        $ AE.object ["record_count" AE..= V.length records, "error" AE..= show @Text ePg]
      pure (Left (This ePg))
    classify (Right _) (Left eTf) = do
      Log.logAttention "TF_WRITE_FAILED"
        $ AE.object ["record_count" AE..= V.length records, "error" AE..= show @Text eTf]
      pure (Left (That eTf))
    classify (Left ePg) (Left eTf) = do
      Log.logAttention "BOTH_WRITES_FAILED"
        $ AE.object
          [ "record_count" AE..= V.length records
          , "pg_error" AE..= show @Text ePg
          , "tf_error" AE..= show @Text eTf
          ]
      pure (Left (These ePg eTf))

    -- Build the Left WriteFailure for a detected silent under-persist. A side
    -- with lost == 0 is healthy and dropped from the These.
    underPersist (pgLost, pgStore) (tfLost, tfStore) = do
      let n = V.length records
          mkErr store lost = toException (SilentUnderPersistError store lost n)
      Log.logAttention "WRITE_UNDERPERSIST_DETECTED"
        $ AE.object ["record_count" AE..= n, "pg_lost" AE..= pgLost, "tf_lost" AE..= tfLost]
      pure . Left $ case (pgLost > 0, tfLost > 0) of
        (True, True) -> These (mkErr pgStore pgLost) (mkErr tfStore tfLost)
        (False, True) -> That (mkErr tfStore tfLost)
        _ -> This (mkErr pgStore pgLost)

    -- Per-row attribution: a row appears in the result iff at least one side
    -- rejected it. Rows accepted by both stores are silently absent (full
    -- success). Result order is HashMap-iteration order — DLQ consumers
    -- process entries independently, so order is irrelevant downstream.
    combinePoison :: BulkInsertResult -> BulkInsertResult -> V.Vector (OtelLogsAndSpans, RowPoisonInfo)
    combinePoison pg tf
      | V.null pg.poisonRows && V.null tf.poisonRows = V.empty
      | V.null tf.poisonRows = V.map (second This) pg.poisonRows
      | V.null pg.poisonRows = V.map (second That) tf.poisonRows
      | otherwise =
          let mkMap bir = HM.fromList [(r.id, (r, e)) | (r, e) <- V.toList bir.poisonRows]
              pgMap = mkMap pg
              tfMap = mkMap tf
              both = HM.intersectionWith (\(r, pe) (_, te) -> (r, These pe te)) pgMap tfMap
              pgOnly = HM.map (second This) (pgMap `HM.difference` tfMap)
              tfOnly = HM.map (second That) (tfMap `HM.difference` pgMap)
           in V.fromList $ HM.elems (HM.unions [both, pgOnly, tfOnly])


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
  => WriteTarget
  -> EW.WorkerState OtelLogsAndSpans
  -- ^ NB: returns 'Left WriteFailure' if either store failed. Hand-off to the
  -- extraction worker only happens on full success; a failed batch goes to DLQ.
  -> HM.HashMap Projects.ProjectId Projects.ProjectCache
  -> V.Vector OtelLogsAndSpans
  -> Eff es (Either WriteFailure (V.Vector (OtelLogsAndSpans, RowPoisonInfo)))
insertAndHandOff target worker caches records
  | V.null records = pure (Right V.empty)
  | otherwise =
      bulkInsertOtelLogsAndSpansTF target records >>= \case
        Left wf -> pure (Left wf)
        Right rowPoison -> do
          -- Hand off only the fully-successful rows — don't let downstream
          -- extraction observe records that landed in only one store.
          let poisonIds = HS.fromList [r.id | (r, _) <- V.toList rowPoison]
              fullyOk = V.filter (\r -> not (HS.member r.id poisonIds)) records
          liftIO $ handOffBatches worker caches fullyOk
          pure (Right rowPoison)


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


maxBisectDepth, bisectCap :: Int
maxBisectDepth = 9
bisectCap = 2 ^ maxBisectDepth


-- | Bulk-insert with row-level poison isolation. Slices oversized inputs so
-- every Bind fits libpq's 65 535-param ceiling; the bisector then has enough
-- depth (log2 bisectCap) to drill any failing slice to the offending row.
--
-- Slices commit independently — no enclosing transaction, since TF doesn't
-- support real transactions and PG matches. Retry duplicates are absorbed by
-- TF dedup on (id, timestamp) at flush time; PG doesn't retry. The PG-only
-- path (enableTf=False) accepts the same per-slice contract — partial commit
-- on failure, no rollback across slices.
--
-- >>> bisectCap * length otelColumns <= 65535  -- libpq Bind param ceiling
-- True
bulkInsertOtelLogsAndSpans :: (Hasql :> es, IOE :> es, Log :> es) => V.Vector OtelLogsAndSpans -> Eff es BulkInsertResult
bulkInsertOtelLogsAndSpans = fmap mconcat . traverse insertSlice . VS.chunksOf bisectCap
  where
    insertSlice rs = V.mapM (traverseToSnd otelRowSnippet) rs >>= go maxBisectDepth

    go d pairs =
      tryAny (Hasql.use $ HSession.statement () (stmt pairs)) >>= \case
        Right n -> pure (BulkInsertResult n V.empty)
        Left e
          | Hasql.isTransientException e -> throwIO e
          | V.length pairs == 1 -> do
              -- Single row failed non-transiently: surface as poison so caller
              -- can DLQ. Previous behaviour was to drop + log (silent loss).
              Log.logAttention
                "ROW_INSERT_FAILED"
                (AE.object ["id" AE..= (fst (V.head pairs)).id, "error" AE..= show @Text e])
              pure (BulkInsertResult 0 (V.map (\(r, _) -> (r, e)) pairs))
          | d <= 0 -> do
              -- Couldn't isolate within bisect budget. Treat the whole
              -- remaining slice as poison so the rows still reach the DLQ.
              Log.logAttention
                "BISECT_DEPTH_EXHAUSTED"
                (AE.object ["record_count" AE..= V.length pairs, "error" AE..= show @Text e])
              pure (BulkInsertResult 0 (V.map (\(r, _) -> (r, e)) pairs))
          | otherwise ->
              -- <*> is sequential in Eff: a throw from the left short-circuits the right. Do not parallelize.
              let (l, r) = V.splitAt (V.length pairs `div` 2) pairs
               in (<>) <$> go (d - 1) l <*> go (d - 1) r

    stmt pairs =
      toPreparableStatement
        (otelInsertHeader <> mconcat (intersperse ", " (map snd (V.toList pairs))))
        D.rowsAffected


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
      txt g = top (param . fmap scrubNulText . g)
      jp :: AE.ToJSON a => (OtelLogsAndSpans -> Maybe a) -> OtelRowCtx -> Snippet
      jp g = top (param . fmap (scrubNulValue . AE.toJSON) . g)
      aeson g = top (param . fmap scrubNulValue . unAesonTextMaybe . g)
      arr g = top (encoderAndParam (E.nonNullable textArrayEnc) . V.map scrubNulText . g)
      ctxT f = top (\e -> param (fmap scrubNulText (e.context >>= f)))
      sevText e = e.severity >>= \s -> scrubNulText . toText . encodeEnumSC @"SL" <$> s.severity_text
      sevNum :: OtelLogsAndSpans -> Maybe Int32
      sevNum e = fromIntegral . severity_number <$> e.severity
   in [ ("timestamp", top (param . (.timestamp)))
      , ("observed_timestamp", top (param . (.observed_timestamp)))
      , ("id", (.uuidP))
      , ("parent_id", txt (.parent_id))
      , ("hashes", arr (fromMaybe V.empty . (.hashes)))
      , ("name", txt (fmap (T.take 500) . (.name)))
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
      , ("attributes", \c -> param (fmap (scrubNulValue . AE.Object . KEM.fromMapText) c.am))
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
      , ("attributes___db___response___status_code", aT "db.response.status_code")
      , ("attributes___db___operation___batch___size", aI "db.operation.batch.size")
      , ("attributes___db___query___summary", aT "db.query.summary")
      , ("attributes___db___query___text", aT "db.query.text")
      , ("attributes___user___id", aT "user.id")
      , ("attributes___user___email", aT "user.email")
      , ("attributes___user___full_name", aT "user.full_name")
      , ("attributes___user___name", aT "user.name")
      , ("attributes___user___hash", aT "user.hash")
      , ("resource", \c -> param (fmap (scrubNulValue . AE.Object . KEM.fromMapText) c.rm))
      , ("resource___service___name", rT "service.name")
      , ("resource___service___version", rT "service.version")
      , ("resource___service___instance___id", rT "service.instance.id")
      , ("resource___service___namespace", rT "service.namespace")
      , ("resource___telemetry___sdk___language", rT "telemetry.sdk.language")
      , ("resource___telemetry___sdk___name", rT "telemetry.sdk.name")
      , ("resource___telemetry___sdk___version", rT "telemetry.sdk.version")
      , ("resource___user_agent___original", rT "user_agent.original")
      , ("project_id", top (param . scrubNulText . (.project_id)))
      , ("summary", arr (.summary))
      , -- Send the partition column as a Day (PG `date` OID 1082): TF's `date` is a
        -- Date32 Hive key and its pgwire decoder can't coerce TIMESTAMPTZ→Date32, so a
        -- raw UTCTime drops the whole row on the TF leg. PG's `date` is TIMESTAMPTZ and
        -- coerces the Day to midnight, but that value is never read (see otelSpanColsSql).
        ("date", top (param . utctDay . (.date)))
      , ("message_size_bytes", top (param . (.message_size_bytes)))
      , ("errors", aeson (.errors))
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


-- | Field order matches 'otelSpanColsSql' (and the INSERT path in 'otelColumns')
-- so FromRow / DecodeRow can be derived generically. Reorder both in lockstep.
--
-- Field count is pinned via doctest so any add/remove fails CI unless
-- 'otelSpanColsSql' + 'otelColumns' are touched in the same change:
--
-- >>> length otelColumns
-- 89
data OtelLogsAndSpans = OtelLogsAndSpans
  { project_id :: Text
  , id :: Text -- UUID
  , timestamp :: UTCTime
  , observed_timestamp :: Maybe UTCTime
  , context :: Maybe Context
  , level :: Maybe Text
  , severity :: Maybe Severity
  , body :: Maybe (AesonText AE.Value)
  , attributes :: Maybe (AesonText (Map Text AE.Value))
  , resource :: Maybe (AesonText (Map Text AE.Value))
  , hashes :: Maybe (V.Vector Text)
  , kind :: Maybe Text
  , status_code :: Maybe Text
  , status_message :: Maybe Text
  , start_time :: UTCTime
  , end_time :: Maybe UTCTime
  , events :: Maybe (AesonText AE.Value)
  , links :: Maybe Text
  , duration :: Maybe Int64
  , name :: Maybe Text
  , parent_id :: Maybe Text
  , summary :: V.Vector Text
  , date :: UTCTime
  , errors :: Maybe (AesonText AE.Value)
  , message_size_bytes :: Int64
  }
  deriving (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake OtelLogsAndSpans


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


-- | Predicate mirroring the Sessions UI @is_error@ SQL
-- (@lower(level) = 'error' OR severity___severity_number >= 17 OR status_code = 'ERROR'@).
-- Any record the UI flags as an error should also feed the Issues pipeline so teams
-- get notified — covers frontend @console.error@ logs and server spans marked ERROR
-- without a recorded exception event.
isErrorRecord :: OtelLogsAndSpans -> Bool
isErrorRecord s =
  maybe False ((== "error") . T.toLower) s.level
    || maybe False ((>= 17) . (.severity_number)) s.severity
    || s.status_code
    == Just "ERROR"


-- | Extract all runtime errors from a collection of spans/logs.
-- Prefers OTel @exception.*@ events; falls back to record-level extraction for
-- error-severity records that lack an exception event (e.g. OTLP log records,
-- which always have @events = Nothing@).
getAllATErrors :: V.Vector OtelLogsAndSpans -> V.Vector ErrorPatterns.ATError
getAllATErrors = V.concatMap extractErrorsFromSpan
  where
    extractErrorsFromSpan spanObj =
      let fromEvents = V.mapMaybe (extractATError spanObj) (getErrorEvents spanObj)
       in if V.null fromEvents && isErrorRecord spanObj
            then maybe V.empty V.singleton (extractATErrorFromRecord spanObj)
            else fromEvents


-- | Extract error details from an OpenTelemetry exception event.
-- Follows OTel semantic conventions: @event_attributes.exception.{type,message,stacktrace}@.
extractATError :: OtelLogsAndSpans -> AE.Value -> Maybe ErrorPatterns.ATError
extractATError spanObj (AE.Object o) = do
  AE.Object attrs' <- KEM.lookup "event_attributes" o
  AE.Object attrs <- KEM.lookup "exception" attrs'
  let getTextOrEmpty k = fromMaybe "" $ case KEM.lookup k attrs of
        Just (AE.String s) -> Just s
        _ -> Nothing
  pure $ atErrorFrom spanObj (getTextOrEmpty "type") (getTextOrEmpty "message") (getTextOrEmpty "stacktrace")
extractATError _ _ = Nothing


-- | Fallback extractor for error-severity records with no exception event.
-- Reads OTel @attributes.exception.{type,message,stacktrace}@ (nested by
-- 'nestedJsonFromDotNotation'). Browser SDK variants (@attributes.error.*@) are
-- copied into @exception.*@ at ingest by 'migrateHttpSemanticConventions', so
-- only the canonical namespace needs to be checked here. Message falls back to
-- body (log records) / @status_message@ (spans). Returns Nothing when neither
-- message nor stack exists — prevents empty, unhelpful issues.
extractATErrorFromRecord :: OtelLogsAndSpans -> Maybe ErrorPatterns.ATError
extractATErrorFromRecord spanObj =
  let attrs = unAesonTextMaybe spanObj.attributes
      excAttr k = case attrs >>= Map.lookup "exception" of
        Just (AE.Object o) -> case KEM.lookup (AEK.fromText k) o of
          Just (AE.String s) -> Just s
          _ -> Nothing
        _ -> Nothing
      bodyTxt = case unAesonTextMaybe spanObj.body of
        Just (AE.String s) -> Just s
        _ -> Nothing
      typ = fromMaybe "Error" (excAttr "type")
      msg = fromMaybe "" $ excAttr "message" <|> bodyTxt <|> spanObj.status_message
      stack = fromMaybe "" (excAttr "stacktrace")
   in if T.null msg && T.null stack
        then Nothing
        else Just (atErrorFrom spanObj typ msg stack)


-- | Build an 'ErrorPatterns.ATError' from a span/log plus the extracted
-- @(errorType, message, stackTrace)@ triple. Hash-driven dedup groups similar errors.
atErrorFrom :: OtelLogsAndSpans -> Text -> Text -> Text -> ErrorPatterns.ATError
atErrorFrom spanObj typ msg stack =
  let attrs = unAesonTextMaybe spanObj.attributes
      resc = unAesonTextMaybe spanObj.resource
      asText (AE.String t) = Just t
      asText _ = Nothing
      getSpanAttr k = attrs >>= Map.lookup k >>= asText
      getUserAttrM k v = case resc >>= Map.lookup v of
        Just (AE.Object userAttrs) -> KEM.lookup k userAttrs >>= asText
        _ -> Nothing
      method = getSpanAttr "http.request.method"
      urlPath = getSpanAttr "http.route" <|> getSpanAttr "http.target"
      -- TODO: parse telemetry.sdk.name to SDKTypes
      tech = case resc >>= Map.lookup "telemetry" of
        Just (AE.Object tel) ->
          KEM.lookup "sdk" tel >>= \case
            AE.Object sdkObj -> KEM.lookup "language" sdkObj >>= asText
            _ -> Nothing
        _ -> Nothing
      serviceName = resourceServiceName resc
      -- Empty (not "unknown") keeps hash inputs stable when runtime detection fails.
      rt = fromMaybe "" tech
      hashes = ErrorPatterns.computeErrorHashes spanObj.project_id serviceName spanObj.name rt typ msg stack
   in ErrorPatterns.ATError
        { projectId = UUID.fromText spanObj.project_id >>= (Just . UUIDId)
        , when = spanObj.timestamp
        , errorType = typ
        , rootErrorType = typ
        , message = msg
        , rootErrorMessage = msg
        , stackTrace = stack
        , hash = hashes.narrow
        , parentHash = Just hashes.broad
        , isFramework = ErrorPatterns.isFrameworkTransportError typ (ErrorPatterns.normalizeMessage msg)
        , technology = Nothing
        , serviceName = serviceName
        , requestMethod = method
        , requestPath = urlPath
        , spanId = spanObj.context >>= (.span_id)
        , traceId = spanObj.context >>= (.trace_id)
        , runtime = tech
        , parentSpanId = spanObj.parent_id
        , endpointHash = Nothing
        , environment = Nothing
        , userId = getUserAttrM "id" "user"
        , userEmail = getUserAttrM "email" "user"
        , userIp = getSpanAttr "client.address"
        , sessionId = getUserAttrM "id" "session"
        }


getProjectStatsForReport :: DB es => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es [(Text, Int, Int)]
getProjectStatsForReport projectId start end = do
  Hasql.interp
    [HI.sql| SELECT resource___service___name AS service_name,  (COUNT(*) FILTER ( WHERE status_code = 'ERROR' OR attributes___exception___type IS NOT NULL))::bigint AS total_error_events, COUNT(*)::bigint AS total_events
         FROM otel_logs_and_spans
         WHERE project_id = #{projectId.toText} AND timestamp >= #{start} AND timestamp <= #{end} AND resource___service___name is not null
         GROUP BY resource___service___name
         ORDER BY total_events DESC
      |]


getProjectStatsBySpanType :: DB es => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es [(Text, Int, Int)]
getProjectStatsBySpanType projectId start end = do
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
          COUNT(*)::bigint AS total_events,
          AVG(duration)::bigint AS avg_duration
        FROM otel_logs_and_spans
        WHERE project_id = #{projectId.toText}
          AND timestamp >= #{start}
          AND timestamp <= #{end}
          AND kind != 'log'
        GROUP BY span_type
        ORDER BY total_events DESC
      |]


getEndpointStats :: DB es => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es [(Text, Text, Text, Int64, Int64)]
getEndpointStats projectId start end = do
  Hasql.interp
    [HI.sql|
SELECT
    COALESCE(attributes___server___address, attributes->'net'->'host'->>'name', attributes->'http'->>'host', NULLIF(split_part(split_part(split_part(attributes___url___full, '://', 2), '/', 1), ':', 1), ''), resource___service___name, '') AS host,
    COALESCE(attributes___http___request___method, 'GET') AS method,
    COALESCE(attributes___url___path, '') AS url_path,
    CAST(ROUND(AVG(COALESCE(duration, 0))) AS BIGINT) AS average_duration,
    COUNT(*)::bigint AS request_count
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
  rows :: V.Vector (Text, Int64, Int64) <-
    Hasql.interp
      [HI.sql| SELECT
        attributes___db___query___text AS query,
        ROUND(AVG(duration))::int8 AS avg_duration,
        COUNT(*)::int8 AS count
      FROM otel_logs_and_spans
      WHERE project_id = #{projectId.toText}
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
  Hasql.interp
    [HI.sql|
      WITH target_trace_spans AS (
        SELECT DISTINCT context___trace_id, name
        FROM otel_logs_and_spans
        WHERE project_id = #{pid.toText}
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
        WHERE project_id = #{pid.toText}
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
        AVG(s.duration)::bigint AS avg_duration,
        COUNT(*)::bigint AS span_count
      FROM otel_logs_and_spans s
      JOIN matching_traces m
        ON s.context___trace_id = m.trace_id
      WHERE s.project_id = #{pid.toText}
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
      , hashes = Just V.empty
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
      , message_size_bytes = 0
      }


insertSystemLog
  :: (Concurrent :> es, Hasql :> es, IOE :> es, Ki.StructuredConcurrency :> es, Labeled "timefusion" Hasql :> es, Log :> es)
  => Bool
  -- ^ enableTimefusionWrites
  -> OtelLogsAndSpans
  -> Eff es ()
insertSystemLog enableTf otelLog = do
  let minted = mintOtelLogIds (V.singleton otelLog)
  -- System logs are best-effort. The inner write logs its own failure via
  -- logAttention; surface a higher-level "system log dropped" so an incident
  -- triager investigating the original event can see that its trail was lost.
  bulkInsertOtelLogsAndSpansTF (writeTargetFor enableTf Nothing) minted >>= \case
    Right _ -> pass
    Left wf ->
      Log.logAttention "SYSTEM_LOG_DROPPED" (AE.object ["reason" AE..= writeFailureSummary wf])


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


-- Shared summary-element helpers (used by both log and span summarizers).
tag :: T.Text -> T.Text -> T.Text -> T.Text
tag n s v = n <> ";" <> s <> "⇒" <> v


truncT :: Int -> T.Text -> T.Text
truncT n t = if T.length t > n then T.take (n - 3) t <> "..." else t


encTrunc :: AE.ToJSON a => Int -> a -> T.Text
encTrunc n = truncT n . decodeUtf8 . AE.encode


nonEmptyT :: Maybe T.Text -> Maybe T.Text
nonEmptyT = mfilter (not . T.null)


generateLogSummary :: OtelLogsAndSpans -> V.Vector T.Text
generateLogSummary otel =
  let
    attrsM = unAesonTextMaybe otel.attributes
    bodyV = unAesonTextMaybe otel.body
    resM = unAesonTextMaybe otel.resource
    isRawDataLog = isNothing otel.body && isNothing otel.severity && maybe True Map.null attrsM

    severityBadge = \case
      SLTrace -> ("badge-neutral", "TRACE")
      SLDebug -> ("badge-neutral", "DEBUG")
      SLInfo -> ("badge-info", "INFO")
      SLWarn -> ("badge-warning", "WARN")
      SLError -> ("badge-error", "ERROR")
      SLFatal -> ("badge-fatal", "FATAL")

    -- Always emits a single element; falls back to "{}" when resource is missing/empty.
    resourceFallback =
      tag "resource" "text-textWeak"
        $ maybe "{}" (encTrunc 500) (mfilter (not . Map.null) resM)

    rawDataLogElements =
      catMaybes
        [ tag "trace_state" "neutral" <$> (otel.context >>= nonEmptyT . (.trace_state))
        , tag "trace_id" "right-badge-neutral" . T.take 16 <$> (otel.context >>= nonEmptyT . (.trace_id))
        , Just resourceFallback
        ]

    -- Body: raw string passes through verbatim; objects prefer extractMessageFromLog
    -- then fall back to encoded JSON; primitives use `show` truncated to 200; Null drops.
    bodyElt =
      bodyV >>= \case
        AE.String t -> Just t
        AE.Object obj -> Just $ fromMaybe (decodeUtf8 (AE.encode obj)) (extractMessageFromLog (AE.Object obj))
        AE.Null -> Nothing
        v -> Just $ T.take 200 (toText (show v))

    normalLogElements =
      catMaybes
        [ uncurry (tag "severity_text") . severityBadge <$> (otel.severity >>= (.severity_text))
        , bodyElt
        , tag "attributes" "text-textWeak" . encTrunc 500 <$> mfilter (not . Map.null) attrsM
        ]
   in
    V.fromList $ if isRawDataLog || null normalLogElements then rawDataLogElements else normalLogElements


-- | Classify a browser/frontend span by name into a product-level display category.
-- Returns (human label, badge style class) or Nothing if not a frontend span.
-- Covers page loads, documents, resources, navigations, user actions, long tasks,
-- web vitals. Errors flow through the existing ERROR status path. HTTP fetch/xhr
-- spans ("HTTP GET" etc.) fall through to the generic HTTP path since they have
-- http.request.method / http.response.status_code attributes the existing code handles.
--
-- >>> classifyFrontendSpan "documentLoad"
-- Just ("page load","badge-info")
-- >>> classifyFrontendSpan "resourceFetch"
-- Just ("resource","badge-neutral")
-- >>> classifyFrontendSpan "web-vital.lcp"
-- Just ("vital lcp","badge-vital")
-- >>> classifyFrontendSpan "HTTP GET"
-- Nothing
-- >>> classifyFrontendSpan "random-span"
-- Nothing
classifyFrontendSpan :: T.Text -> Maybe (T.Text, T.Text)
classifyFrontendSpan = \case
  "documentLoad" -> Just ("page load", "badge-info")
  "documentFetch" -> Just ("document", "badge-info")
  "resourceFetch" -> Just ("resource", "badge-neutral")
  "resource" -> Just ("resource", "badge-neutral")
  "navigation" -> Just ("navigation", "badge-info")
  "route.change" -> Just ("navigation", "badge-info")
  "click" -> Just ("click", "badge-action")
  "submit" -> Just ("submit", "badge-action")
  "keydown" -> Just ("keydown", "badge-action")
  "keyup" -> Just ("keyup", "badge-action")
  "longtask" -> Just ("long task", "badge-warning")
  n | Just v <- T.stripPrefix "web-vital." n -> Just ("vital " <> v, "badge-vital")
  _ -> Nothing


-- | Drop scheme+host from a full URL for compact display. Fragments are stripped.
-- Returns the original input when the URL is malformed (has no path).
--
-- >>> shortenUrl "https://example.com/api/orders?id=1#x"
-- "/api/orders?id=1"
-- >>> shortenUrl "http://host/path"
-- "/path"
-- >>> shortenUrl "/already/path"
-- "/already/path"
-- >>> shortenUrl "https://example.com"
-- "example.com"
shortenUrl :: T.Text -> T.Text
shortenUrl full =
  let stripped = fromMaybe full $ T.stripPrefix "https://" full <|> T.stripPrefix "http://" full
      path = T.dropWhile (/= '/') stripped
      chosen = if T.null path then stripped else path
   in T.takeWhile (/= '#') chosen


-- | Extract basename (last path segment, no query/fragment) from a URL.
-- Falls back to the host/path prefix when the URL ends with '/' (no filename).
--
-- >>> urlBasename "https://cdn.example.com/assets/app.js?v=1"
-- "app.js"
-- >>> urlBasename "/img/logo.png"
-- "logo.png"
-- >>> urlBasename "https://example.com/"
-- "example.com"
-- >>> urlBasename ""
-- ""
urlBasename :: T.Text -> T.Text
urlBasename url =
  let stripped = fromMaybe url $ T.stripPrefix "https://" url <|> T.stripPrefix "http://" url
      path = T.takeWhile (\c -> c /= '?' && c /= '#') stripped
      trimmed = T.dropWhileEnd (== '/') path
      seg = snd (T.breakOnEnd "/" trimmed)
   in if T.null seg then trimmed else seg


-- | Best-effort label for a user-interaction span (click, submit, etc.).
-- Prefers accessible labels over raw selectors.
clickTargetLabel :: Maybe (Map Text AE.Value) -> Maybe T.Text
clickTargetLabel attrs =
  atMapText "target.aria_label" attrs
    <|> atMapText "aria.label" attrs
    <|> atMapText "target.text_content" attrs
    <|> atMapText "text_content" attrs
    <|> atMapText "target.element" attrs
    <|> atMapText "target_element" attrs
    <|> atMapText "target.tag_name" attrs
    <|> atMapText "target.xpath" attrs
    <|> atMapText "event_type" attrs


-- | Format a byte count compactly (e.g. 340000 → "340KB").
humanBytes :: Int -> T.Text
humanBytes n
  | n < 1024 = toText (show n) <> "B"
  | n < 1024 * 1024 = toText (show (n `div` 1024)) <> "KB"
  | otherwise = toText (show (n `div` (1024 * 1024))) <> "MB"


generateSpanSummary :: OtelLogsAndSpans -> V.Vector T.Text
generateSpanSummary otel =
  let
    attrsM = unAesonTextMaybe otel.attributes
    resM = unAesonTextMaybe otel.resource
    spanNameT = fromMaybe "" otel.name
    frontendCat = classifyFrontendSpan spanNameT
    urlFull = atMapText "url.full" attrsM
    urlPathOrFull = atMapText "url.path" attrsM <|> shortenUrl <$> urlFull
    httpRoute = atMapText "http.route" attrsM
    dbSys = atMapText "db.system.name" attrsM <|> atMapText "db.system" attrsM
    rpcMethod = atMapText "rpc.method" attrsM
    httpStatus = atMapInt "http.response.status_code" attrsM
    httpMethod = atMapText "http.request.method" attrsM
    hasHttp = isJust httpMethod || isJust httpStatus

    durMs d = toText (getDurationNSMS (fromIntegral d))
    -- ERROR badge is dropped when http status >=400 already conveys it.
    errorStatus style = case (otel.status_code, httpStatus) of
      (Just "ERROR", Just s) | s >= 400 -> Nothing
      (Just "ERROR", _) -> Just $ tag "status" style "ERROR"
      _ -> Nothing
    dbBadge = \case
      "postgresql" -> tag "db.system" "right-badge-postgres" "postgres"
      "mysql" -> tag "db.system" "right-badge-mysql" "mysql"
      "redis" -> tag "db.system" "right-badge-redis" "redis"
      "mongodb" -> tag "db.system" "right-badge-mongo" "mongodb"
      "elasticsearch" -> tag "db.system" "right-badge-elastic" "elastic"
      s -> tag "db.system" "right-badge-neutral" s

    isEmptySpan = (isNothing otel.name || otel.name == Just "") && maybe True Map.null attrsM

    resourceFallbackElements =
      catMaybes
        [ tag "process" "neutral" <$> (nonEmptyT (atMapText "process.executable.name" resM) <|> ("PID " <>) . show <$> atMapInt "process.pid" resM)
        , tag "service" "neutral" <$> atMapText "service.name" resM
        , tag "resource" "text-textWeak" . encTrunc 300 <$> mfilter (not . Map.null) (Map.filterWithKey (\k _ -> k /= "process") <$> resM)
        , tag "trace_id" "right-badge-neutral" . T.take 16 <$> (otel.context >>= nonEmptyT . (.trace_id))
        , tag "duration" "right-badge-neutral" . durMs <$> otel.duration
        ]

    -- Frontend-derived label (suppressed via `frontendCat *>` when span isn't classified frontend).
    frontendLabel =
      frontendCat *> case spanNameT of
        n | n `elem` ["documentLoad", "documentFetch", "navigation", "route.change"] -> tag "url" "text-textStrong" <$> urlPathOrFull
        n | n == "resourceFetch" || n == "resource" -> tag "resource" "text-textStrong" . urlBasename <$> (urlFull <|> urlPathOrFull)
        n | n `elem` ["click", "submit", "keydown", "keyup"] -> tag "target" "text-textStrong" <$> clickTargetLabel attrsM
        "longtask" -> tag "blocked" "text-textStrong" . ("main thread " <>) . durMs <$> otel.duration
        n | "web-vital." `T.isPrefixOf` n -> (\v -> tag "value" "text-textStrong" (v <> "ms")) <$> atMapText "value" attrsM
        _ -> Nothing

    -- Original arm order preserved: request_type wins before database/internal.
    requestType = case (otel.kind, hasHttp, atMapText "component" attrsM) of
      (Just "server", True, _) -> Just "incoming"
      (Just "client", True, _) -> Just "outgoing"
      (_, True, Just comp) | "proxy" `T.isInfixOf` comp -> Just "incoming"
      (_, True, _) -> Just "outgoing"
      (Just "server", _, _) | isJust rpcMethod -> Just "incoming"
      (Just "client", _, _) | isJust rpcMethod -> Just "outgoing"
      _ -> Nothing
    kindElt = case frontendCat of
      Just (cat, style) -> Just $ tag "kind" style cat
      Nothing ->
        tag "request_type" "neutral"
          <$> requestType
          <|> tag "kind" "neutral" "database"
          <$ guard (isJust dbSys)
          <|> tag "kind" "neutral" "internal"
          <$ guard (otel.kind == Just "internal")

    routeOrUrl = case (httpRoute, urlPathOrFull, frontendLabel) of
      (_, _, Just _) -> Nothing
      (Just route, _, _) -> Just $ tag "route" "neutral" route
      (_, Just url, _) -> Just $ tag "url" "neutral" url
      _ -> Nothing

    spanNameFallback = do
      n <- otel.name
      guard (all isNothing [httpRoute, urlPathOrFull, frontendLabel])
      pure $ tag "span_name" "neutral" n

    normalElements =
      catMaybes
        [ kindElt
        , frontendLabel
        , (\c -> tag "status_code" (statusCodeStyle c) (toText (show c))) <$> httpStatus
        , (\m -> tag "method" (methodStyle m) m) <$> httpMethod
        , routeOrUrl
        , tag "db.system" "neutral" <$> dbSys
        , tag "db.query.text" "text-textStrong" . T.take 200 <$> (dbSys *> atMapText "db.query.text" attrsM)
        , tag "db.statement" "neutral" . T.take 200 <$> atMapText "db.statement" attrsM
        , tag "rpc.method" "neutral" <$> rpcMethod
        , tag "rpc.service" "neutral" <$> atMapText "rpc.service" attrsM
        , spanNameFallback
        , errorStatus "badge-error"
        , tag "attributes" "text-textWeak" . encTrunc 500 <$> mfilter (not . Map.null) attrsM
        , tag "session" "right-badge-neutral" <$> atMapText "session.id" attrsM
        , tag "user email" "right-badge-neutral" <$> atMapText "user.email" attrsM <|> tag "user name" "right-badge-neutral" <$> atMapText "user.id" attrsM
        , tag "user name" "right-badge-neutral" <$> (atMapText "user.full_name" attrsM <|> atMapText "user.name" attrsM)
        , errorStatus "right-badge-error"
        , dbBadge <$> dbSys
        , (atMapInt "http.response.body.size" attrsM <|> atMapInt "http.response_content_length" attrsM) >>= \n -> guard (n > 0) $> tag "size" "right-badge-neutral" (humanBytes n)
        , tag "protocol" "right-badge-neutral" "http" <$ guard hasHttp
        , tag "protocol" "right-badge-neutral" "rpc" <$ rpcMethod
        , tag "duration" "right-badge-neutral" . durMs <$> otel.duration
        ]
   in
    V.fromList (if isEmptySpan then resourceFallbackElements else normalElements)


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
