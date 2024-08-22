{-# LANGUAGE OverloadedRecordDot #-}

module Models.Telemetry.Telemetry (
  LogRecord (..),
  logRecordByProjectAndId,
  spanRecordByProjectAndId,
  SpanRecord (..),
  SeverityLevel (..),
  SpanStatus (..),
  SpanKind (..),
  bulkInsertLogs,
  bulkInsertSpans,
) where

import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as B16
import Data.Text (Text, toTitle, toUpper)
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (..), queryOne)
import Database.PostgreSQL.Simple (FromRow, ResultError (..), ToRow)
import Database.PostgreSQL.Simple.FromField (Conversion, FromField (..), fromField, returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import Database.PostgreSQL.Transact (DBT, execute, executeMany)
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import GHC.Generics (Generic)
import GHC.TypeLits
import Models.Projects.Projects qualified as Projects
import Relude
import Relude.Unsafe qualified as Unsafe


newtype WrappedEnum (prefix :: Symbol) a = WrappedEnum a
  deriving (Generic)


instance Show a => ToField (WrappedEnum prefix a) where
  toField (WrappedEnum a) = toField . toUpper . fromString . drop 2 . show $ a


instance (KnownSymbol prefix, Typeable a, Read a) => FromField (WrappedEnum prefix a) where
  fromField f bs = do
    let pfx = symbolVal (Proxy @prefix)
    case bs of
      Nothing -> returnError UnexpectedNull f ""
      Just bss -> do
        pure $ WrappedEnum (Unsafe.read $ pfx <> (toString $ toTitle (decodeUtf8 bss)))


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


data LogRecord = LogRecord
  { projectId :: UUID
  , id :: Maybe UUID
  , timestamp :: UTCTime
  , observedTimestamp :: UTCTime
  , traceId :: ByteString
  , spanId :: Maybe ByteString -- Hex representation
  , severityText :: Maybe SeverityLevel
  , severityNumber :: Int
  , body :: Value
  , attributes :: Value
  , resource :: Value
  , instrumentationScope :: Value
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
  { projectId :: UUID
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
  , attributes :: Value
  , events :: Value
  , links :: Value
  , resource :: Value
  , instrumentationScope :: Value
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake SpanRecord
  deriving anyclass (NFData, FromRow, ToRow)


logRecordByProjectAndId :: DB :> es => Projects.ProjectId -> UTCTime -> UUID.UUID -> Eff es (Maybe LogRecord)
logRecordByProjectAndId pid createdAt rdId = dbtToEff $ queryOne Select q (createdAt, pid, rdId)
  where
    q =
      [sql|SELECT project_id, id, timestamp, observed_timestamp, trace_id, span_id, severity_text,
                  severity_number, body, attributes, resource, instrumentation_scope
             FROM telemetry.logs where (timestamp=?)  and project_id=? and id=? LIMIT 1|]


spanRecordByProjectAndId :: DB :> es => Projects.ProjectId -> UTCTime -> UUID.UUID -> Eff es (Maybe SpanRecord)
spanRecordByProjectAndId pid createdAt rdId = dbtToEff $ queryOne Select q (createdAt, pid, rdId)
  where
    q =
      [sql| SELECT project_id, timestamp, trace_id::text, span_id::text, parent_span_id::text, trace_state,
                     span_name, start_time, end_time, kind, status, status_message, attributes,
                     events, links, resource, instrumentation_scope
              FROM telemetry.spans where (timestamp=?)  and project_id=? and id=? LIMIT 1|]


-- Function to insert multiple log entries
bulkInsertLogs :: DB :> es => V.Vector LogRecord -> Eff es ()
bulkInsertLogs logs = void $ dbtToEff $ executeMany q (V.toList rowsToInsert)
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
bulkInsertSpans spans = void $ dbtToEff $ executeMany q (V.toList rowsToInsert)
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
