module Models.Telemetry.Telemetry (
  LogRecord (..),
  logRecordByProjectAndId,
  spanRecordByProjectAndId,
  getSpandRecordsByTraceId,
  SpanRecord (..),
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
  getDataPointsData,
  bulkInsertLogs,
  spanRecordById,
  getTraceDetails,
  getMetricData,
  bulkInsertMetrics,
  bulkInsertSpans,
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

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as KEM

import Data.ByteString.Base16 qualified as B16
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (..), executeMany, query, queryOne)
import Database.PostgreSQL.Transact qualified as DBT

import Data.Aeson (Value)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (FromRow, ResultError (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

import Data.Default (Default)
import Data.Text qualified as T
import Data.Time (TimeZone (..), UTCTime, formatTime, utcToZonedTime)
import Data.Time.Format (defaultTimeLocale)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (..), executeMany, query, queryOne)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query (..))
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful

import Database.PostgreSQL.Simple (Only (..))
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pkg.DBUtils (WrappedEnum (..))
import Relude
import RequestMessages (RequestMessage (..))


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
  { projectId :: UUID
  , id :: UUID
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


data SpanRecord = SpanRecord
  { uSpanId :: UUID
  , projectId :: UUID
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
  , attributes :: AE.Value
  , events :: AE.Value
  , links :: AE.Value
  , resource :: AE.Value
  , instrumentationScope :: AE.Value
  , spanDurationNs :: Integer
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake SpanRecord
  deriving anyclass (NFData, FromRow, ToRow)


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
  { id :: Maybe UUID
  , projectId :: UUID
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
getTraceDetails pid trId = dbtToEff $ queryOne Select q (pid, trId)
  where
    q =
      [sql| SELECT
              trace_id,
              MIN(start_time) AS trace_start_time,
              MAX(COALESCE(end_time, start_time)) AS trace_end_time,
              CAST(EXTRACT(EPOCH FROM (MAX(COALESCE(end_time, start_time)) - MIN(start_time))) * 1000000000 AS BIGINT) AS trace_duration_ns,
              COUNT(span_id) AS total_spans,
              ARRAY_REMOVE(ARRAY_AGG(DISTINCT jsonb_extract_path_text(resource, 'service.name')), NULL) AS service_names
            FROM telemetry.spans
            WHERE  project_id = ? AND trace_id = ?
            GROUP BY trace_id;
        |]


logRecordByProjectAndId :: DB :> es => Projects.ProjectId -> UTCTime -> UUID.UUID -> Eff es (Maybe LogRecord)
logRecordByProjectAndId pid createdAt rdId = dbtToEff $ queryOne Select q (createdAt, pid, rdId)
  where
    q =
      [sql|SELECT project_id, id, timestamp, observed_timestamp, trace_id, span_id, severity_text,
                  severity_number, body, attributes, resource, instrumentation_scope
             FROM telemetry.logs where (timestamp=?)  and project_id=? and id=? LIMIT 1|]


getSpandRecordsByTraceId :: DB :> es => Projects.ProjectId -> Text -> Eff es (V.Vector SpanRecord)
getSpandRecordsByTraceId pid trId = dbtToEff $ query Select q (pid, trId)
  where
    q =
      [sql|
      SELECT id, project_id, timestamp, trace_id::text, span_id::text, parent_span_id::text, trace_state,
                     span_name, start_time, end_time, kind, status, status_message, attributes,
                     events, links, resource, instrumentation_scope, CAST(EXTRACT(EPOCH FROM (end_time - start_time)) * 1000000000 AS BIGINT) as span_duration
              FROM telemetry.spans where project_id=? and trace_id=? ORDER BY start_time ASC;
    |]


spanRecordByProjectAndId :: DB :> es => Projects.ProjectId -> UTCTime -> UUID.UUID -> Eff es (Maybe SpanRecord)
spanRecordByProjectAndId pid createdAt rdId = dbtToEff $ queryOne Select q (createdAt, pid, rdId)
  where
    q =
      [sql| SELECT id, project_id, timestamp, trace_id::text, span_id::text, parent_span_id::text, trace_state,
                     span_name, start_time, end_time, kind, status, status_message, attributes,
                     events, links, resource, instrumentation_scope, CAST(EXTRACT(EPOCH FROM (end_time - start_time)) * 1000000000 AS BIGINT) as span_duration
              FROM telemetry.spans where (timestamp=?)  and project_id=? and id=? LIMIT 1|]


spanRecordById :: DB :> es => Projects.ProjectId -> Text -> Text -> Eff es (Maybe SpanRecord)
spanRecordById pid trId spanId = dbtToEff $ queryOne Select q (pid, trId, spanId)
  where
    q =
      [sql| SELECT id, project_id, timestamp, trace_id::text, span_id::text, parent_span_id::text, trace_state,
                     span_name, start_time, end_time, kind, status, status_message, attributes,
                     events, links, resource, instrumentation_scope, CAST(EXTRACT(EPOCH FROM (end_time - start_time)) * 1000000000 AS BIGINT) as span_duration
              FROM telemetry.spans where project_id=? and trace_id = ? and span_id=? LIMIT 1|]


getChildSpans :: DB :> es => Projects.ProjectId -> V.Vector Text -> Eff es (V.Vector SpanRecord)
getChildSpans pid spanIds = dbtToEff $ query Select q (pid, spanIds)
  where
    q =
      [sql| SELECT id, project_id, timestamp, trace_id::text, span_id::text, parent_span_id::text, trace_state,
                     span_name, start_time, end_time, kind, status, status_message, attributes,
                     events, links, resource, instrumentation_scope, CAST(EXTRACT(EPOCH FROM (end_time - start_time)) * 1000000000 AS BIGINT) as span_duration
              FROM telemetry.spans where project_id =? AND parent_span_id=Any(?)|]


getDataPointsData :: DB :> es => Projects.ProjectId -> (Maybe UTCTime, Maybe UTCTime) -> Eff es (V.Vector MetricDataPoint)
getDataPointsData pid dateRange = dbtToEff $ query Select (Query $ Relude.encodeUtf8 q) pid
  where
    dateRangeStr = toText $ case dateRange of
      (Nothing, Just b) -> "AND timestamp BETWEEN NOW() AND '" <> formatTime defaultTimeLocale "%F %R" b <> "'"
      (Just a, Just b) -> "AND timestamp BETWEEN '" <> formatTime defaultTimeLocale "%F %R" a <> "' AND '" <> formatTime defaultTimeLocale "%F %R" b <> "'"
      _ -> ""
    q =
      [text| SELECT metric_name, metric_type, metric_unit, metric_description, COUNT(*) AS data_points,
             ARRAY_AGG(DISTINCT COALESCE(resource->>'service.name', 'unknown'))::text[] AS service_names, '{}'::text[] AS labels
             FROM telemetry.metrics WHERE project_id = ? $dateRangeStr
             GROUP BY metric_name, metric_type, metric_unit, metric_description
             ORDER BY metric_name;
getLogsByTraceIds pid traceIds = dbtToEff $ V.fromList <$> DBT.query q (pid, traceIds)
  where
    q =
      [sql|
      SELECT project_id, id, timestamp, observed_timestamp, trace_id, span_id, severity_text, severity_number, body
      FROM telemetry.logs WHERE project_id = ? AND trace_id = ANY(?);
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
queryToValues pid traceIds = dbtToEff $ V.fromList <$> DBT.query q (pid, traceIds)
  where
    q =
      [sql|
      SELECT json_build_array(id, timestamp, trace_id, span_id, CAST(EXTRACT(EPOCH FROM (timestamp)) * 1_000_000_000 AS BIGINT), severity_text, body)
      FROM telemetry.logs WHERE project_id = ? AND trace_id = ANY(?);
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
            (
                SELECT ARRAY_AGG(DISTINCT key)
                FROM (
                    SELECT DISTINCT jsonb_object_keys(attributes) AS key
                    FROM telemetry.metrics
                    WHERE project_id = ? AND metric_name = ?
                ) AS unique_keys
            ) AS metric_labels
      FROM telemetry.metrics
      WHERE project_id = ? AND metric_name = ?
      GROUP BY metric_name, metric_type, metric_unit, metric_description;
        |]


getMetricChartListData :: DB :> es => Projects.ProjectId -> Maybe Text -> Maybe Text -> (Maybe UTCTime, Maybe UTCTime) -> Int -> Eff es (V.Vector MetricChartListData)
getMetricChartListData pid sourceM prefixM dateRange cursor = dbtToEff $ query Select (Query $ Relude.encodeUtf8 q) pid
  where
    dateRangeStr = toText $ case dateRange of
      (Nothing, Just b) -> "AND timestamp BETWEEN NOW() AND '" <> formatTime defaultTimeLocale "%F %R" b <> "'"
      (Just a, Just b) -> "AND timestamp BETWEEN '" <> formatTime defaultTimeLocale "%F %R" a <> "' AND '" <> formatTime defaultTimeLocale "%F %R" b <> "'"
      _ -> ""
    sourceFilter = case sourceM of
      Nothing -> ""
      Just source -> if source == "" || source == "all" then "" else "AND resource->>'service.name' = '" <> source <> "'"
    prefixFilter = case prefixM of
      Nothing -> ""
      Just prefix -> if prefix == "" || prefix == "all" then "" else "AND metric_name LIKE '" <> prefix <> "%'"
    cursorTxt = show cursor
    q =
      [text|
        SELECT DISTINCT ON (metric_name) metric_name, metric_type, metric_unit, metric_description
        FROM telemetry.metrics WHERE project_id = ? $sourceFilter $prefixFilter $dateRangeStr ORDER BY metric_name OFFSET $cursorTxt LIMIT 20;
     |]


getMetricLabelValues :: DB :> es => Projects.ProjectId -> Text -> Text -> Eff es (V.Vector Text)
getMetricLabelValues pid metricName labelName = dbtToEff $ query Select q (labelName, pid, metricName)
  where
    q = [sql| SELECT DISTINCT attributes->>? FROM telemetry.metrics WHERE project_id = ? AND metric_name = ?|]


getMetricServiceNames :: DB :> es => Projects.ProjectId -> Eff es (V.Vector Text)
getMetricServiceNames pid = dbtToEff $ query Select q pid
  where
    q =
      [sql| SELECT DISTINCT resource->>'service.name' FROM telemetry.metrics WHERE project_id = ?  AND resource->>'service.name' IS NOT NULL|]


-- Function to insert multiple log entries
bulkInsertLogs :: DB :> es => V.Vector LogRecord -> Eff es ()
bulkInsertLogs logs = void $ dbtToEff $ executeMany Insert q (V.toList rowsToInsert)
  where
    q =
      [sql|
      INSERT INTO telemetry.logs
      (project_id, timestamp, observed_timestamp, trace_id, span_id,
       severity_text, severity_number, body, attributes, resource, instrumentation_scope)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    |]
    rowsToInsert = V.map logToTuple logs
    logToTuple entry =
      ( entry.projectId
      , entry.timestamp
      , entry.observedTimestamp
      , entry.traceId
      , entry.spanId
      , entry.severityText
      , entry.severityNumber
      , entry.body
      , entry.attributes
      , entry.resource
      , entry.instrumentationScope
      )


-- Function to insert multiple span entries
bulkInsertSpans :: DB :> es => V.Vector SpanRecord -> Eff es ()
bulkInsertSpans spans = void $ dbtToEff $ executeMany Insert q (V.toList rowsToInsert)
  where
    q =
      [sql|
      INSERT INTO telemetry.spans
      (project_id, timestamp, trace_id, span_id, parent_span_id, trace_state, span_name,
       start_time, end_time, kind, status, status_message, attributes, events, links, resource, instrumentation_scope)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    |]
    rowsToInsert = V.map spanToTuple spans
    spanToTuple entry =
      ( entry.projectId
      , entry.timestamp
      , entry.traceId
      , entry.spanId
      , entry.parentSpanId
      , entry.traceState
      , entry.spanName
      , entry.startTime
      , entry.endTime
      , entry.kind
      , entry.status
      , entry.statusMessage
      , entry.attributes
      , entry.events
      , entry.links
      , entry.resource
      , entry.instrumentationScope
      )


bulkInsertMetrics :: DB :> es => V.Vector MetricRecord -> Eff es ()
bulkInsertMetrics metrics = void $ dbtToEff $ executeMany Insert q (V.toList rowsToInsert)
  where
    q =
      [sql|
        INSERT INTO telemetry.metrics
        (project_id, metric_name, metric_type, metric_unit, metric_description, metric_time, timestamp,
         attributes, resource, instrumentation_scope, metric_value, exemplars, flags)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
     |]
    rowsToInsert = V.map metricToTuple metrics
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


convertSpanToRequestMessage :: SpanRecord -> Text -> RequestMessage
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
    pathParams = fromMaybe (AE.object []) (AE.decode $ Relude.encodeUtf8 $ fromMaybe "" $ getSpanAttribute "http.request.path_params" sp.attributes)
    queryParams = fromMaybe (AE.object []) (AE.decode $ Relude.encodeUtf8 $ fromMaybe "" $ getSpanAttribute "http.request.query_params" sp.attributes)
    errors = AE.decode $ Relude.encodeUtf8 $ fromMaybe "" $ getSpanAttribute "apitoolkit.errors" sp.attributes
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
