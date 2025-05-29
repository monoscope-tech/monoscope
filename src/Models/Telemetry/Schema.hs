module Models.Telemetry.Schema (
  FieldInfo (..),
  Schema (..),
  PopularOtelQuery (..),
  telemetrySchema,
  telemetrySchemaJson,
  generateSchemaForAI,
  popularOtelQueries,
  popularOtelQueriesJson,
) where

import Data.Aeson qualified as AE
import Data.Map qualified as Map
import Data.Text qualified as T
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Relude


-- | Field information for a single field
data FieldInfo = FieldInfo
  { fieldType :: Text
  , description :: Text
  , examples :: Maybe [Text]
  }
  deriving (Eq, Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake FieldInfo


-- | A schema containing fields
data Schema = Schema
  { fields :: Map Text FieldInfo
  }
  deriving (Eq, Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Schema


-- | The complete telemetry schema definition
telemetrySchema :: Schema
telemetrySchema =
  Schema
    { fields =
        Map.fromList
          [ -- Top-level fields
            ("timestamp", FieldInfo "string" "Timestamp when the event occurred" Nothing)
          , ("observed_timestamp", FieldInfo "string" "Timestamp when the event was observed" Nothing)
          , ("id", FieldInfo "string" "Unique identifier for the log/span" Nothing)
          , ("parent_id", FieldInfo "string" "Parent span ID" Nothing)
          , ("hashes", FieldInfo "array" "All relevant hashes for item identification" Nothing)
          , ("name", FieldInfo "string" "Name of the span or log" Nothing)
          , ("kind", FieldInfo "string" "Type of telemetry data (logs, span, request)" (Just ["logs", "span", "request"]))
          , ("status_code", FieldInfo "string" "Status code of the span" (Just ["OK", "ERROR", "UNSET"]))
          , ("status_message", FieldInfo "string" "Status message" Nothing)
          , ("level", FieldInfo "string" "Log level (same as severity text)" (Just ["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"]))
          , ("body", FieldInfo "object" "Body content of the log/span" Nothing)
          , ("duration", FieldInfo "duration" "Duration of the span in nanoseconds" Nothing)
          , ("start_time", FieldInfo "string" "Start time of the span" Nothing)
          , ("end_time", FieldInfo "string" "End time of the span" Nothing)
          , ("events", FieldInfo "array" "Events associated with the span" Nothing)
          , ("links", FieldInfo "string" "Links to other spans" Nothing)
          , ("project_id", FieldInfo "string" "Project ID" Nothing)
          , ("date", FieldInfo "string" "Date" Nothing)
          , -- Severity fields (flattened)
            ("severity", FieldInfo "object" "Severity information" Nothing)
          , ("severity.text", FieldInfo "string" "Text representation of severity" (Just ["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"]))
          , ("severity.number", FieldInfo "string" "Numeric representation of severity" Nothing)
          , -- Context fields (flattened)
            ("context", FieldInfo "object" "Context information" Nothing)
          , ("context.trace_id", FieldInfo "string" "Unique identifier for the trace" Nothing)
          , ("context.span_id", FieldInfo "string" "Unique identifier for the span" Nothing)
          , ("context.trace_state", FieldInfo "string" "Trace state" Nothing)
          , ("context.trace_flags", FieldInfo "string" "Trace flags" Nothing)
          , ("context.is_remote", FieldInfo "string" "Whether this span is remote" Nothing)
          , -- Attributes fields (flattened)
            ("attributes", FieldInfo "object" "Attributes of the log/span" Nothing)
          , ("attributes.client", FieldInfo "object" "Client information" Nothing)
          , ("attributes.client.address", FieldInfo "string" "Client address" Nothing)
          , ("attributes.client.port", FieldInfo "number" "Client port" Nothing)
          , ("attributes.server", FieldInfo "object" "Server information" Nothing)
          , ("attributes.server.address", FieldInfo "string" "Server address" Nothing)
          , ("attributes.server.port", FieldInfo "number" "Server port" Nothing)
          , ("attributes.network", FieldInfo "object" "Network information" Nothing)
          , ("attributes.network.local", FieldInfo "object" "Local network information" Nothing)
          , ("attributes.network.local.address", FieldInfo "string" "Local address" Nothing)
          , ("attributes.network.local.port", FieldInfo "number" "Local port" Nothing)
          , ("attributes.network.peer", FieldInfo "object" "Peer network information" Nothing)
          , ("attributes.network.peer.address", FieldInfo "string" "Peer address" Nothing)
          , ("attributes.network.peer.port", FieldInfo "number" "Peer port" Nothing)
          , ("attributes.network.protocol", FieldInfo "object" "Protocol information" Nothing)
          , ("attributes.network.protocol.name", FieldInfo "string" "Protocol name" Nothing)
          , ("attributes.network.protocol.version", FieldInfo "string" "Protocol version" Nothing)
          , ("attributes.network.transport", FieldInfo "string" "Transport type" Nothing)
          , ("attributes.network.type", FieldInfo "string" "Network type" Nothing)
          , ("attributes.code", FieldInfo "object" "Source code attributes" Nothing)
          , ("attributes.code.number", FieldInfo "number" "Code number" Nothing)
          , ("attributes.code.file", FieldInfo "object" "File information" Nothing)
          , ("attributes.code.file.path", FieldInfo "string" "File path" Nothing)
          , ("attributes.code.function", FieldInfo "object" "Function information" Nothing)
          , ("attributes.code.function.name", FieldInfo "string" "Function name" Nothing)
          , ("attributes.code.line", FieldInfo "object" "Line information" Nothing)
          , ("attributes.code.line.number", FieldInfo "number" "Line number" Nothing)
          , ("attributes.code.stacktrace", FieldInfo "string" "Stack trace" Nothing)
          , ("attributes.log_record", FieldInfo "object" "Log record attributes" Nothing)
          , ("attributes.log_record.original", FieldInfo "string" "Original log record" Nothing)
          , ("attributes.log_record.uid", FieldInfo "string" "Log record UID" Nothing)
          , ("attributes.error", FieldInfo "object" "Error information" Nothing)
          , ("attributes.error.type", FieldInfo "string" "Error type" Nothing)
          , ("attributes.exception", FieldInfo "object" "Exception information" Nothing)
          , ("attributes.exception.type", FieldInfo "string" "Exception type" Nothing)
          , ("attributes.exception.message", FieldInfo "string" "Exception message" Nothing)
          , ("attributes.exception.stacktrace", FieldInfo "string" "Exception stack trace" Nothing)
          , ("attributes.url", FieldInfo "object" "URL information" Nothing)
          , ("attributes.url.fragment", FieldInfo "string" "URL fragment" Nothing)
          , ("attributes.url.full", FieldInfo "string" "Full URL" Nothing)
          , ("attributes.url.path", FieldInfo "string" "URL path" Nothing)
          , ("attributes.url.query", FieldInfo "string" "URL query" Nothing)
          , ("attributes.url.scheme", FieldInfo "string" "URL scheme" Nothing)
          , ("attributes.user_agent", FieldInfo "object" "User agent information" Nothing)
          , ("attributes.user_agent.original", FieldInfo "string" "Original user agent string" Nothing)
          , ("attributes.http", FieldInfo "object" "HTTP information" Nothing)
          , ("attributes.http.request", FieldInfo "object" "HTTP request attributes" Nothing)
          , ("attributes.http.request.method", FieldInfo "string" "HTTP method" (Just ["GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS"]))
          , ("attributes.http.request.method_original", FieldInfo "string" "Original HTTP method" Nothing)
          , ("attributes.http.request.resend_count", FieldInfo "number" "Number of times request was resent" Nothing)
          , ("attributes.http.request.body", FieldInfo "object" "Request body information" Nothing)
          , ("attributes.http.request.body.size", FieldInfo "number" "Request body size" Nothing)
          , ("attributes.http.response", FieldInfo "object" "HTTP response attributes" Nothing)
          , ("attributes.http.response.status_code", FieldInfo "number" "HTTP status code" Nothing)
          , ("attributes.session", FieldInfo "object" "Session information" Nothing)
          , ("attributes.session.id", FieldInfo "string" "Session ID" Nothing)
          , ("attributes.session.previous", FieldInfo "object" "Previous session information" Nothing)
          , ("attributes.session.previous.id", FieldInfo "string" "Previous session ID" Nothing)
          , ("attributes.db", FieldInfo "object" "Database information" Nothing)
          , ("attributes.db.system", FieldInfo "object" "Database system" Nothing)
          , ("attributes.db.system.name", FieldInfo "string" "Database system name" Nothing)
          , ("attributes.db.collection", FieldInfo "object" "Collection information" Nothing)
          , ("attributes.db.collection.name", FieldInfo "string" "Collection name" Nothing)
          , ("attributes.db.namespace", FieldInfo "string" "Database namespace" Nothing)
          , ("attributes.db.operation", FieldInfo "object" "Operation information" Nothing)
          , ("attributes.db.operation.name", FieldInfo "string" "Operation name" Nothing)
          , ("attributes.db.operation.batch", FieldInfo "object" "Batch information" Nothing)
          , ("attributes.db.operation.batch.size", FieldInfo "number" "Batch size" Nothing)
          , ("attributes.db.response", FieldInfo "object" "Database response" Nothing)
          , ("attributes.db.response.status_code", FieldInfo "string" "Response status code" Nothing)
          , ("attributes.db.query", FieldInfo "object" "Query information" Nothing)
          , ("attributes.db.query.summary", FieldInfo "string" "Query summary" Nothing)
          , ("attributes.db.query.text", FieldInfo "string" "Query text" Nothing)
          , ("attributes.user", FieldInfo "object" "User information" Nothing)
          , ("attributes.user.id", FieldInfo "string" "User ID" Nothing)
          , ("attributes.user.email", FieldInfo "string" "User email" Nothing)
          , ("attributes.user.full_name", FieldInfo "string" "User full name" Nothing)
          , ("attributes.user.name", FieldInfo "string" "User name" Nothing)
          , ("attributes.user.hash", FieldInfo "string" "User hash" Nothing)
          , -- Resource fields (flattened)
            ("resource", FieldInfo "object" "Resource attributes" Nothing)
          , ("resource.service", FieldInfo "object" "Service information" Nothing)
          , ("resource.service.name", FieldInfo "string" "Service name" Nothing)
          , ("resource.service.version", FieldInfo "string" "Service version" Nothing)
          , ("resource.service.instance", FieldInfo "object" "Service instance" Nothing)
          , ("resource.service.instance.id", FieldInfo "string" "Service instance ID" Nothing)
          , ("resource.service.namespace", FieldInfo "string" "Service namespace" Nothing)
          , ("resource.telemetry", FieldInfo "object" "Telemetry information" Nothing)
          , ("resource.telemetry.sdk", FieldInfo "object" "SDK information" Nothing)
          , ("resource.telemetry.sdk.language", FieldInfo "string" "SDK language" Nothing)
          , ("resource.telemetry.sdk.name", FieldInfo "string" "SDK name" Nothing)
          , ("resource.telemetry.sdk.version", FieldInfo "string" "SDK version" Nothing)
          , ("resource.user_agent", FieldInfo "object" "User agent information" Nothing)
          , ("resource.user_agent.original", FieldInfo "string" "Original user agent string" Nothing)
          ]
    }


-- | Convert the schema to JSON for frontend consumption
telemetrySchemaJson :: AE.Value
telemetrySchemaJson = AE.toJSON telemetrySchema


-- | Generate a concise schema description for AI queries by categorizing fields
generateSchemaForAI :: Schema -> Text
generateSchemaForAI schema =
  T.unlines
    $ "Available telemetry fields for querying logs and spans:"
    : ""
    : concatMap renderCategory fieldCategories
  where
    fields = Map.toList schema.fields

    fieldCategories :: [(Text, [(Text, FieldInfo)])]
    fieldCategories =
      [ ("Core fields:", filterByPrefixes ["level", "timestamp", "duration", "name", "kind", "status_code", "body", "severity."])
      , ("HTTP attributes:", filterByPrefixes ["attributes.http"])
      , ("Service & Resource:", filterByPrefixes ["resource.service", "resource.telemetry"])
      , ("Context & Tracing:", filterByPrefixes ["context.", "parent_id"])
      , ("Error & Exception:", filterByPrefixes ["attributes.error", "attributes.exception"])
      , ("Database:", filterByPrefixes ["attributes.db"])
      , ("User & Session:", filterByPrefixes ["attributes.user", "attributes.session"])
      , ("Network:", filterByPrefixes ["attributes.client", "attributes.server", "attributes.network"])
      ]

    filterByPrefixes prefixes = filter (matchesAnyPrefix prefixes . fst) fields
    matchesAnyPrefix prefixes name = any (`T.isPrefixOf` name) prefixes || name `elem` prefixes

    renderCategory (title, categoryFields) =
      if null categoryFields
        then []
        else title : map renderField (take 10 categoryFields) ++ [""]

    renderField (name, info) = "- " <> name <> maybe "" (\vals -> " (" <> T.intercalate ", " vals <> ")") info.examples


-- | Popular OTEL query data structure
data PopularOtelQuery = PopularOtelQuery
  { query :: Text
  , description :: Text
  }
  deriving (Eq, Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake PopularOtelQuery


-- | Popular OpenTelemetry queries with correct AQL syntax
popularOtelQueries :: [PopularOtelQuery]
popularOtelQueries =
  [ -- Error and exception queries
    PopularOtelQuery "level == \"ERROR\"" "Show all error-level logs"
  , PopularOtelQuery "attributes.exception.type != null" "Find logs with exceptions"
  , PopularOtelQuery "level == \"ERROR\" AND attributes.http.response.status_code >= 500" "Server errors with HTTP 5xx status codes"
  , PopularOtelQuery "attributes.error.type != null" "Find logs with error information"
  , -- HTTP-related queries
    PopularOtelQuery "attributes.http.request.method == \"POST\"" "All POST requests"
  , PopularOtelQuery "attributes.http.response.status_code >= 400" "HTTP errors (4xx and 5xx)"
  , PopularOtelQuery "attributes.http.response.status_code == 404" "Not found errors"
  , PopularOtelQuery "attributes.http.request.method in (\"GET\", \"POST\", \"PUT\")" "Common HTTP methods"
  , PopularOtelQuery "attributes.http.request.method == \"GET\" AND attributes.http.response.status_code == 200" "Successful GET requests"
  , -- Performance queries using proper duration syntax
    PopularOtelQuery "duration > 5s" "Slow requests (>5 seconds)"
  , PopularOtelQuery "duration > 1s" "Requests taking more than 1 second"
  , PopularOtelQuery "duration > 500ms" "Requests slower than 500ms"
  , PopularOtelQuery "kind == \"span\" AND duration > 100ms" "Slow spans (>100ms)"
  , -- Service and trace queries
    PopularOtelQuery "resource.service.name == \"api\"" "Logs from API service"
  , PopularOtelQuery "kind == \"span\"" "All span data"
  , PopularOtelQuery "kind == \"logs\"" "All log entries"
  , PopularOtelQuery "parent_id != null" "Child spans with parent relationships"
  , PopularOtelQuery "context.trace_id != null" "Logs with trace correlation"
  , -- Text search operations using new operators
    PopularOtelQuery "body contains \"error\"" "Logs containing \"error\" in body"
  , PopularOtelQuery "name startswith \"database\"" "Operations starting with \"database\""
  , PopularOtelQuery "attributes.url.path startswith \"/api/\"" "API endpoint requests"
  , PopularOtelQuery "body has \"timeout\"" "Logs mentioning timeout"
  , -- Database queries
    PopularOtelQuery "attributes.db.operation.name != null" "Database operations"
  , PopularOtelQuery "attributes.db.system.name == \"postgresql\"" "PostgreSQL database operations"
  , PopularOtelQuery "attributes.db.query.text contains \"SELECT\"" "Database SELECT queries"
  , PopularOtelQuery "attributes.db.operation.name in (\"INSERT\", \"UPDATE\", \"DELETE\")" "Database write operations"
  , -- User and session queries
    PopularOtelQuery "attributes.user.id != null" "Logs with user information"
  , PopularOtelQuery "attributes.session.id != null" "Logs with session tracking"
  , PopularOtelQuery "attributes.user.email endswith \"@company.com\"" "Company user activities"
  , -- Status and severity combinations
    PopularOtelQuery "level in (\"ERROR\", \"FATAL\")" "Critical log levels"
  , PopularOtelQuery "status_code == \"ERROR\"" "Failed operations"
  , PopularOtelQuery "level == \"WARN\" AND duration > 2s" "Slow operations with warnings"
  , -- Advanced filtering with new operators
    PopularOtelQuery "attributes.http.request.method == \"POST\" AND attributes.http.response.status_code < 400" "Successful POST requests"
  , PopularOtelQuery "resource.service.name startswith \"auth\" AND level !in (\"DEBUG\", \"TRACE\")" "Important auth service logs"
  , PopularOtelQuery "attributes.url.path contains \"/api/\" AND duration > 2s" "Slow API endpoint calls"
  , PopularOtelQuery "attributes.exception.message has_any [\"timeout\", \"connection\", \"network\"]" "Network-related exceptions"
  , -- Resource and telemetry queries
    PopularOtelQuery "resource.telemetry.sdk.language == \"javascript\"" "JavaScript telemetry data"
  , PopularOtelQuery "resource.service.namespace != null" "Services with namespace information"
  , -- Complex multi-condition queries
    PopularOtelQuery "(level == \"ERROR\" OR duration > 5s) AND resource.service.name != null" "Errors or slow requests from known services"
  , PopularOtelQuery "attributes.http.response.status_code >= 500 AND attributes.user.id != null" "Server errors affecting users"
  , PopularOtelQuery "kind == \"span\" AND (name contains \"database\" OR attributes.db.operation.name != null)" "Database-related spans"
  ]


-- | Convert popular OTEL queries to JSON for frontend consumption
popularOtelQueriesJson :: AE.Value
popularOtelQueriesJson = AE.toJSON popularOtelQueries
