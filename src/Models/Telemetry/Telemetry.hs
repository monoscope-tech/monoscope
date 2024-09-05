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
  bulkInsertLogs,
  getTraceDetails,
  bulkInsertSpans,
  SpanEvent (..),
  SpanLink (..),
)
where

import Data.Aeson qualified as AE
import Data.ByteString.Base16 qualified as B16
import Data.Text (toTitle, toUpper)
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (..), executeMany, query, queryOne)
import Database.PostgreSQL.Simple (FromRow, ResultError (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (..), fromField, returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import GHC.TypeLits
import Models.Projects.Projects qualified as Projects
import Relude
import Relude.Unsafe qualified as Unsafe


newtype WrappedEnum (prefix :: Symbol) a = WrappedEnum a
  deriving (Generic)


instance Show a => ToField (WrappedEnum prefix a) where
  toField (WrappedEnum a) = toField . toUpper . fromString . drop 2 . show $ a


instance (KnownSymbol prefix, Typeable a, Read a) => FromField (WrappedEnum prefix a) where
  fromField f = \case
    Nothing -> returnError UnexpectedNull f ""
    Just bss -> pure $ WrappedEnum (Unsafe.read $ (symbolVal (Proxy @prefix)) <> (toString $ toTitle (decodeUtf8 bss)))


data SeverityLevel = SLDebug | SLInfo | SLWarn | SLError | SLFatal
  deriving (Show, Generic, Read)
  deriving anyclass (NFData, AE.FromJSON, AE.ToJSON)
  deriving (ToField, FromField) via WrappedEnum "SL" SeverityLevel


data SpanStatus = SSOk | SSError | SSUnset
  deriving (Show, Generic, Read)
  deriving anyclass (NFData, AE.FromJSON, AE.ToJSON)
  deriving (ToField, FromField) via WrappedEnum "SS" SpanStatus


data SpanKind = SKInternal | SKServer | SKClient | SKProducer | SKConsumer | SKUnspecified
  deriving (Show, Generic, Read)
  deriving anyclass (NFData, AE.FromJSON, AE.ToJSON)
  deriving (ToField, FromField) via WrappedEnum "SK" SpanKind


data Trace = Trace
  { traceId :: Text
  , traceStartTime :: UTCTime
  , traceDurationNs :: Integer
  , totalSpans :: Int
  , serviceNames :: Maybe (V.Vector Text)
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Trace
  deriving anyclass (NFData, FromRow)


data LogRecord = LogRecord
  { projectId :: UUID
  , id :: Maybe UUID
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
    case B16.decode (TE.encodeUtf8 t) of
      Right bs -> return bs
      Left err -> fail $ "Invalid hex-encoded ByteString: " ++ err


instance AE.ToJSON ByteString where
  toJSON = AE.String . TE.decodeUtf8 . B16.encode


data SpanRecord = SpanRecord
  { uSpandId :: Maybe UUID
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


getTraceDetails :: DB :> es => Projects.ProjectId -> Text -> Eff es (Maybe Trace)
getTraceDetails pid trId = dbtToEff $ queryOne Select q (pid, trId)
  where
    q =
      [sql| SELECT
              trace_id,
              MIN(start_time) AS trace_start_time,
              CAST(EXTRACT(EPOCH FROM (MAX(COALESCE(end_time, start_time)) - MIN(start_time))) * 1000000000 AS BIGINT) AS trace_duration_ns,
              COUNT(span_id) AS total_spans,
              ARRAY_AGG(DISTINCT jsonb_extract_path_text(resource, 'service.name')) AS service_names
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
