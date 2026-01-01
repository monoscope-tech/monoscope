module Models.Apis.RequestDumps (
  RequestDump (..),
  SDKTypes (..),
  RequestDumpLogItem (..),
  EndpointPerf (..),
  RequestTypes (..),
  RequestForReport (..),
  ATError (..),
  normalizeUrlPath,
  selectLogTable,
  executeArbitraryQuery,
  executeSecuredQuery,
  requestDumpLogUrlPath,
  getRequestDumpForReports,
  getRequestDumpsForPreviousReportPeriod,
  getLast24hTotalRequest,
  getLastSevenDaysTotalRequest,
  fetchLogPatterns,
  selectChildSpansAndLogs,
)
where

import Control.Exception.Annotated (checkpoint)
import Data.Aeson qualified as AE
import Data.Annotation (toAnnotation)
import Data.Default
import Data.Default.Instances ()
import Data.Text qualified as T
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Database.PostgreSQL.Simple.Types (Query (Query))
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Log (Log)
import Effectful.PostgreSQL qualified as PG
import Effectful.Time qualified as Time
import Models.Apis.Fields.Types ()
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pkg.Parser
import Pkg.Parser.Stats (Section, Sources (SSpans))
import Relude hiding (many, some)
import System.Logging qualified as Log
import System.Types (DB)
import Web.HttpApiData (ToHttpApiData (..))


data SDKTypes
  = GoGin
  | GoBuiltIn
  | GoGorillaMux
  | GoFiber
  | GoDefault
  | GoOutgoing
  | PhpLaravel
  | PhpSymfony
  | JsExpress
  | JsNest
  | JsFastify
  | JsAdonis
  | JsNext
  | JavaSpringBoot
  | JsAxiosOutgoing
  | JsOutgoing
  | DotNet
  | PythonFastApi
  | PythonFlask
  | PythonDjango
  | PythonOutgoing
  | PhpSlim
  | GuzzleOutgoing
  | ElixirPhoenix
  | PythonPyramid
  | DotNetOutgoing
  | TestkitOutgoing
  | JavaSpring
  | JavaApacheOutgoing
  | JavaVertx
  | SDKUnknown
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] SDKTypes


instance ToField SDKTypes where
  toField sdkType = toField @String (show sdkType)


instance FromField SDKTypes where
  fromField f mdata = do
    str <- fromField f mdata
    case readMaybe str of
      Just sdkType -> return sdkType
      Nothing -> return GoGin


data RequestTypes
  = Incoming
  | Outgoing
  | Background
  | System
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] RequestTypes


instance ToField RequestTypes where
  toField requestType = toField @String (show requestType)


instance FromField RequestTypes where
  fromField f mdata = do
    str <- fromField f mdata
    case readMaybe str of
      Just requestType -> return requestType
      Nothing -> return Incoming


-- Nothing -> returnError ConversionFailed f ("Could not read SDKTypes: " ++ str)

-- normalize URLPatg based off the SDKTypes. Should allow us have custom logic to parse and transform url paths into a form we are happy with, per library
-- >>> normalizeUrlPath GoGin 200 "GET" "https://apitoolkit.io/abc/:bla?q=abc"
-- "https://apitoolkit.io/abc/:bla"
-- >>> normalizeUrlPath GoGin 200 "GET" "/abc/:bla?q=abc"
-- "/abc/:bla"
--
-- >>> normalizeUrlPath GoGin 404 "GET" "https://apitoolkit.io/abc/:bla?q=abc"
-- ""
--
-- >>> normalizeUrlPath JsExpress 200 "OPTIONS" "https://apitoolkit.io/abc/:bla?q=abc"
-- ""
-- >>> normalizeUrlPath JsExpress 200 "PATCH" "https://apitoolkit.io/abc/:bla?q=abc"
-- "https://apitoolkit.io/abc/:bla"
--
normalizeUrlPath :: SDKTypes -> Int -> Text -> Text -> Text
normalizeUrlPath GoOutgoing statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath GoGin statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath GoFiber statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath GoBuiltIn statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath GoDefault statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath GoGorillaMux statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath PhpLaravel statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath PhpSymfony statusCode _method urlPath = removeQueryParams statusCode urlPath
-- NOTE: Temporary workaround due to storing complex paths in the urlPath, which should be unaccepted, and messes with our logic
normalizeUrlPath JsExpress statusCode "OPTIONS" urlPath = ""
normalizeUrlPath JsExpress statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath JavaSpringBoot statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath JsNest statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath JsAxiosOutgoing statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath DotNet statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath PythonFastApi statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath JsFastify statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath PythonFlask statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath PythonDjango statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath PythonOutgoing statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath JsAdonis statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath PhpSlim statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath GuzzleOutgoing statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath ElixirPhoenix statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath PythonPyramid statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath DotNetOutgoing statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath TestkitOutgoing statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath JavaSpring statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath JavaApacheOutgoing statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath JavaVertx statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath JsOutgoing statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath JsNext statusCode _method urlPath = removeQueryParams statusCode urlPath
normalizeUrlPath SDKUnknown statusCode _method urlPath = removeQueryParams statusCode urlPath


-- getRequestType ...
-- >>> getRequestType GoGin
-- Incoming
-- >>> getRequestType GoOutgoing
-- Outgoing
-- >>> getRequestType JsAxiosOutgoing
-- Outgoing

-- removeQueryParams ...
-- >>> removeQueryParams 200 "https://apitoolkit.io/abc/:bla?q=abc"
--
-- Function to remove the query parameter section from a URL
removeQueryParams :: Int -> Text -> Text
removeQueryParams 404 urlPath = ""
removeQueryParams statusCode urlPath =
  case T.break (== '?') urlPath of
    (before, "") -> before -- No query parameters found
    (before, after) -> before -- Query parameters found, stripping them


data ATError = ATError
  { projectId :: Maybe Projects.ProjectId
  , when :: UTCTime
  , errorType :: Text
  , rootErrorType :: Text
  , message :: Text
  , rootErrorMessage :: Text
  , stackTrace :: Text
  , hash :: Maybe Text
  , technology :: Maybe SDKTypes
  , requestMethod :: Maybe Text
  , requestPath :: Maybe Text
  , spanId :: Maybe Text
  , traceId :: Maybe Text
  , serviceName :: Maybe Text
  , stack :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, NFData)
  deriving (FromField, ToField) via Aeson ATError
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ATError


--   via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ATErrors
-- deriving (ToField, FromField) via Aeson ATErrors

-- request dumps are time series dumps representing each requests which we consume from our users.
-- We use this field via the log explorer for exploring and searching traffic. And at the moment also use it for most time series analytics.
-- It's likely a good idea to stop relying on it for some of the time series analysis, to allow us easily support request sampling, but still support
-- relatively accurate analytic counts.
-- NOTE: This record closely mirrors the order of fields in the table. Changing the order of fields here would break inserting and querying request dumps
data RequestDump = RequestDump
  { id :: UUID.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , projectId :: UUID.UUID
  , host :: Text
  , urlPath :: Text
  , rawUrl :: Text
  , pathParams :: AE.Value
  , method :: Text
  , referer :: Text
  , protoMajor :: Int64
  , protoMinor :: Int64
  , duration :: CalendarDiffTime
  , statusCode :: Int64
  , --
    queryParams :: AE.Value
  , requestHeaders :: AE.Value
  , responseHeaders :: AE.Value
  , requestBody :: AE.Value
  , responseBody :: AE.Value
  , --
    endpointHash :: Text
  , shapeHash :: Text
  , formatHashes :: V.Vector Text
  , fieldHashes :: V.Vector Text
  , durationNs :: Int64
  , sdkType :: SDKTypes
  , parentId :: Maybe UUID.UUID
  , serviceVersion :: Maybe Text
  , errors :: AE.Value -- Vector ATError
  , tags :: V.Vector Text
  , requestType :: RequestTypes
  }
  deriving stock (Generic)
  deriving anyclass (NFData, ToRow)


-- Fields to from request dump neccessary for generating performance reports
data RequestForReport = RequestForReport
  { id :: UUID.UUID
  , createdAt :: ZonedTime
  , projectId :: UUID.UUID
  , host :: Text
  , urlPath :: Text
  , rawUrl :: Text
  , method :: Text
  , endpointHash :: Text
  , averageDuration :: Integer
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "request_dumps", PrimaryKey "id", FieldModifiers '[CamelToSnake]] RequestForReport)


data EndpointPerf = EndpointPerf
  { endpointHash :: Text
  , averageDuration :: Integer
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "request_dumps", PrimaryKey "id", FieldModifiers '[CamelToSnake]] EndpointPerf)


-- RequestDumpLogItem is used in the to query log items for the log query explorer on the dashboard. Each item here can be queried
-- via the query language on said dashboard page.
data RequestDumpLogItem = RequestDumpLogItem
  { id :: UUID.UUID
  , createdAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , host :: Text
  , urlPath :: Text
  , method :: Text
  , rawUrl :: Text
  , referer :: Text
  , --
    pathParams :: AE.Value
  , -- , duration :: CalendarDiffTime
    statusCode :: Int
  , --
    queryParams :: AE.Value
  , requestBody :: AE.Value
  , responseBody :: AE.Value
  , requestHeaders :: AE.Value
  , responseHeaders :: AE.Value
  , durationNs :: Integer
  , sdkType :: SDKTypes
  , parentId :: Maybe UUID.UUID
  , serviceVersion :: Maybe Text -- allow users track deployments and versions (tags, commits, etc)
  , errorsCount :: Integer
  , errors :: AE.Value
  , tags :: Maybe (V.Vector Text)
  , requestType :: RequestTypes
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] RequestDumpLogItem


incrementByOneMillisecond :: String -> String
incrementByOneMillisecond dateStr =
  case maybeTime of
    Nothing -> ""
    Just utcTime ->
      let newTime = posixSecondsToUTCTime $ utcTimeToPOSIXSeconds utcTime + 0.000001
       in iso8601Show newTime
  where
    maybeTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" dateStr :: Maybe UTCTime


requestDumpLogUrlPath :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Bool -> Text
requestDumpLogUrlPath pid q cols cursor since fromV toV layout source recent = "/p/" <> pid.toText <> "/log_explorer?" <> T.intercalate "&" params
  where
    recentTo = cursor >>= (\x -> Just (toText . incrementByOneMillisecond . toString $ x))
    params =
      catMaybes
        [ Just "json=true"
        , fmap ("query=" <>) (toQueryParam <$> q)
        , fmap ("cols=" <>) (toQueryParam <$> cols)
        , if recent then Nothing else fmap ("cursor=" <>) (toQueryParam <$> cursor)
        , if recent then Nothing else fmap ("since=" <>) (toQueryParam <$> since)
        , fmap ("from=" <>) (toQueryParam <$> fromV)
        , if recent then fmap ("to=" <>) (toQueryParam <$> recentTo) else fmap ("to=" <>) (toQueryParam <$> toV)
        , fmap ("layout=" <>) (toQueryParam <$> layout)
        , fmap ("source=" <>) (toQueryParam <$> source)
        ]


getRequestDumpForReports :: DB es => Projects.ProjectId -> Text -> Eff es [RequestForReport]
getRequestDumpForReports pid report_type = PG.query (Query $ encodeUtf8 q) (Only pid)
  where
    report_interval = if report_type == "daily" then ("'24 hours'" :: Text) else "'7 days'"
    q =
      [text|
      SELECT DISTINCT ON (hashes[1]) id, timestamp AS created_at, project_id,
        COALESCE(attributes___server___address, attributes___network___peer___address,'') AS host,
        COALESCE(attributes___url___path,'') AS url_path,
        COALESCE(attributes___url___full,'') AS raw_url,
        COALESCE(attributes___http___request___method, 'GET') AS method,
        hashes[1] AS endpoint_hash,
        CAST (ROUND( AVG(COALESCE(duration, 0)) OVER (PARTITION BY hashes[1]) ) AS BIGINT) AS average_duration
      FROM otel_logs_and_spans
      WHERE
          project_id = ?::text
          AND timestamp > NOW() - INTERVAL $report_interval
          AND name = 'monoscope.http'
          AND kind = 'SERVER'
          AND status_code IS NOT NULL
          AND cardinality(hashes) > 0
      ORDER BY hashes[1], timestamp DESC;
    |]


getRequestDumpsForPreviousReportPeriod :: DB es => Projects.ProjectId -> Text -> Eff es [EndpointPerf]
getRequestDumpsForPreviousReportPeriod pid report_type = PG.query (Query $ encodeUtf8 q) (Only pid)
  where
    (start, end) = if report_type == "daily" then ("'48 hours'" :: Text, "'24 hours'") else ("'14 days'", "'7 days'")
    q =
      [text|
     SELECT  hashes[1] as endpoint_hash,
        CAST (ROUND (AVG (COALESCE(duration, 0))) AS BIGINT) AS average_duration
     FROM otel_logs_and_spans
     WHERE project_id = ?::text AND timestamp > NOW() - interval $start AND timestamp < NOW() - interval $end
       AND kind = 'SERVER' AND status_code IS NOT NULL
       AND cardinality(hashes) > 0
     GROUP BY hashes[1];
    |]


-- | Custom field parser that tries multiple types
data FieldValue
  = FText Text
  | FInt Int64
  | FDouble Double
  | FBool Bool
  | FTime UTCTime
  | FUUID UUID.UUID
  | FJson AE.Value
  | FNull
  deriving (Generic, Show)
  deriving anyclass (NFData)


-- | Convert FieldValue to JSON
fieldValueToJson :: FieldValue -> AE.Value
fieldValueToJson (FText t) = AE.String t
fieldValueToJson (FInt i) = AE.Number (fromIntegral i)
fieldValueToJson (FDouble d) = AE.Number (realToFrac d)
fieldValueToJson (FBool b) = AE.Bool b
fieldValueToJson (FTime t) = AE.String (toText $ iso8601Show t)
fieldValueToJson (FUUID u) = AE.String (UUID.toText u)
fieldValueToJson (FJson v) = v
fieldValueToJson FNull = AE.Null


-- | FromField instance that tries multiple types
instance FromField FieldValue where
  fromField f mdata = do
    -- First check if it's NULL
    case mdata of
      Nothing -> pure FNull
      Just _ ->
        -- Try parsing as different types
        (FText <$> fromField f mdata)
          <|> (FInt <$> fromField f mdata)
          <|> (FDouble <$> fromField f mdata)
          <|> (FBool <$> fromField f mdata)
          <|> (FTime <$> fromField f mdata)
          <|> (FUUID <$> fromField f mdata)
          <|> (FJson <$> fromField f mdata)
          <|> pure FNull


-- | Execute arbitrary SQL query and return results as vector of vectors
-- Each inner vector represents a row with all columns as JSON values
-- This is a pure Haskell solution that doesn't modify the SQL query
executeArbitraryQuery :: DB es => Text -> Eff es (V.Vector (V.Vector AE.Value))
executeArbitraryQuery queryText = do
  -- Execute the query and parse each field using our FieldValue type
  results :: [[FieldValue]] <- PG.query_ (Query $ encodeUtf8 queryText)
  -- Convert each row of FieldValues to a vector of JSON values
  pure $ V.fromList $ map (V.fromList . map fieldValueToJson) results


-- | Execute a user-provided SQL query with mandatory project_id filtering
-- SECURITY: Uses parameterized queries to prevent SQL injection
-- Creates a CTE that pre-filters otel_logs_and_spans by project_id,
-- then replaces references to the table in the user query
executeSecuredQuery :: DB es => Projects.ProjectId -> Text -> Int -> Eff es (Either Text (V.Vector (V.Vector AE.Value)))
executeSecuredQuery pid userQuery limit = do
  -- Create a CTE that pre-filters the table by project_id using parameterized query
  -- The user's query references otel_logs_and_spans which is replaced by the filtered CTE
  let wrappedQuery =
        "WITH otel_logs_and_spans AS ( \
        \  SELECT * FROM otel_logs_and_spans WHERE project_id = ? \
        \) "
          <> userQuery
          <> " LIMIT ?"
  resultE <- try @SomeException $ do
    results :: [[FieldValue]] <- PG.query (Query $ encodeUtf8 wrappedQuery) (pid, limit)
    pure $ V.fromList $ map (V.fromList . map fieldValueToJson) results
  pure $ case resultE of
    Left err -> Left $ "Query execution failed: " <> show err
    Right results -> Right results


selectLogTable :: (DB es, Log :> es, Time.Time :> es) => Projects.ProjectId -> [Section] -> Text -> Maybe UTCTime -> (Maybe UTCTime, Maybe UTCTime) -> [Text] -> Maybe Sources -> Maybe Text -> Eff es (Either Text (V.Vector (V.Vector AE.Value), [Text], Int))
selectLogTable pid queryAST queryText cursorM dateRange projectedColsByUser source targetSpansM = do
  now <- Time.currentTime
  let (q, queryComponents) = queryASTToComponents ((defSqlQueryCfg pid now source targetSpansM){cursorM, dateRange, projectedColsByUser, source, targetSpansM}) queryAST

  -- Debug logging for parsed query AST and generated SQL
  Log.logTrace
    "Query Debug Info"
    ( AE.object
        [ "original_query_input" AE..= queryText
        , "parsed_query_ast" AE..= show queryAST
        , "generated_sql_query" AE..= q
        , "count_query" AE..= queryComponents.countQuery
        , "project_id" AE..= show pid
        , "source" AE..= source
        , "target_spans" AE..= fromMaybe "" targetSpansM
        , "time_range_from" AE..= fmap (toText . iso8601Show) (fst dateRange)
        , "time_range_to" AE..= fmap (toText . iso8601Show) (snd dateRange)
        , "current_time_used_for_defaults" AE..= toText (iso8601Show now)
        ]
    )

  -- Use the new flexible query execution that doesn't modify SQL
  logItemsV <- checkpoint (toAnnotation ("selectLogTable", q)) $ executeArbitraryQuery q
  Only c <- fromMaybe (Only 0) <$> queryCount queryComponents.countQuery
  pure $ Right (logItemsV, queryComponents.toColNames, c)


selectChildSpansAndLogs :: (DB es, Time.Time :> es) => Projects.ProjectId -> [Text] -> V.Vector Text -> (Maybe UTCTime, Maybe UTCTime) -> V.Vector Text -> Eff es [V.Vector AE.Value]
selectChildSpansAndLogs pid projectedColsByUser traceIds dateRange excludedSpanIds = do
  now <- Time.currentTime
  let fmtTime = toText . iso8601Show
  let qConfig = defSqlQueryCfg pid now (Just SSpans) Nothing
      (r, _) = getProcessedColumns projectedColsByUser qConfig.defaultSelect
  let dateRangeStr = case dateRange of
        (Nothing, Just b) -> "AND timestamp BETWEEN '" <> fmtTime b <> "' AND NOW() "
        -- Include a 30 second buffer on either side to (assuming a first and last trace took 30s to catpure all spans/logs)
        (Just a, Just b) -> "AND timestamp BETWEEN '" <> fmtTime (addUTCTime (-30) a) <> "' AND '" <> fmtTime (addUTCTime 30 b) <> "'"
        _ -> ""
      q =
        [text|SELECT json_build_array($r) FROM otel_logs_and_spans
             WHERE project_id= ?  $dateRangeStr and  context___trace_id=Any(?) and parent_id IS NOT NULL AND id::text != ALL(?)
             ORDER BY timestamp DESC;
           |]
  results <- PG.query (Query $ encodeUtf8 q) (pid, traceIds, excludedSpanIds)
  pure $ mapMaybe valueToVector results


valueToVector :: Only AE.Value -> Maybe (V.Vector AE.Value)
valueToVector (Only val) = case val of
  AE.Array arr -> Just arr
  _ -> Nothing


fetchLogPatterns :: (DB es, Time.Time :> es) => Projects.ProjectId -> [Section] -> (Maybe UTCTime, Maybe UTCTime) -> Maybe Sources -> Maybe Text -> Int -> Eff es [(Text, Int)]
fetchLogPatterns pid queryAST dateRange sourceM targetM skip = do
  now <- Time.currentTime
  let (_, queryComponents) = queryASTToComponents ((defSqlQueryCfg pid now sourceM Nothing){dateRange}) queryAST
      pidTxt = pid.toText
      whereCondition = fromMaybe [text|project_id=${pidTxt}|] queryComponents.whereClause
      target = fromMaybe "log_pattern" targetM
      q = [text|select $target, count(*) as p_count from otel_logs_and_spans where project_id='${pidTxt}' and ${whereCondition} and $target is not null GROUP BY $target ORDER BY p_count desc offset ? limit 15;|]
  PG.query (Query $ encodeUtf8 q) (Only skip)


queryCount :: DB es => Text -> Eff es (Maybe (Only Int))
queryCount q = listToMaybe <$> PG.query_ (Query $ encodeUtf8 q)


getLast24hTotalRequest :: DB es => Projects.ProjectId -> Eff es Int
getLast24hTotalRequest = getRequestCountForInterval "1 day"


getLastSevenDaysTotalRequest :: DB es => Projects.ProjectId -> Eff es Int
getLastSevenDaysTotalRequest = getRequestCountForInterval "7 days"


getRequestCountForInterval :: DB es => Text -> Projects.ProjectId -> Eff es Int
getRequestCountForInterval interval pid = fromMaybe 0 . coerce @(Maybe (Only Int)) @(Maybe Int) . listToMaybe <$> PG.query q (pid, interval)
  where
    q = [sql| SELECT count(*) FROM otel_logs_and_spans WHERE project_id=? AND timestamp > NOW() - interval ?;|]
