{-# LANGUAGE OverloadedRecordDot #-}

module Models.Telemetry.Telemetry (LogRecord(..), SpanRecord(..), SeverityLevel(..), SpanStatus(..), SpanKind(..), bulkInsertLogs, bulkInsertSpans) where

import Relude
import Effectful
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Data.Vector qualified as V
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Text (Text, toUpper)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Transact (DBT, executeMany, execute)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.ToField (ToField, toField)


data SeverityLevel = SLDebug | SLInfo | SLWarn | SLError | SLFatal 
  deriving (Show, Generic, Read)


data SpanStatus = SSOk | SSError | SSUnset 
  deriving (Show, Generic, Read)


data SpanKind = SKInterval | SKServer | SKClient | SKProducer | SKConsumer 
  deriving (Show, Generic, Read)

instance ToField SeverityLevel where
  toField = toField  . toUpper . fromString . drop 2 . show

instance ToField SpanStatus where
  toField = toField . toUpper . fromString . drop 2 . show

instance ToField SpanKind where
  toField = toField . toUpper . fromString . fromString . drop 2 . show

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


data SpanRecord = SpanRecord
  { projectId :: UUID
  , timestamp :: UTCTime
  , traceId :: ByteString
  , spanId :: ByteString
  , parentSpanId :: Maybe ByteString
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
