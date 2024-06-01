{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Models.Apis.RequestDumps (
  RequestDump (..),
  SDKTypes (..),
  RequestDumpLogItem (..),
  EndpointPerf (..),
  RequestForReport (..),
  ATError (..),
  normalizeUrlPath,
  selectLogTable,
  requestDumpLogItemUrlPath,
  requestDumpLogUrlPath,
  selectReqLatenciesRolledBySteps,
  selectReqLatenciesRolledByStepsForProject,
  selectRequestDumpByProject,
  selectRequestDumpByProjectAndId,
  selectRequestDumpByProjectAndParentId,
  bulkInsertRequestDumps,
  getRequestDumpForReports,
  getRequestDumpsForPreviousReportPeriod,
  countRequestDumpByProject,
  getRequestType,
  autoCompleteFromRequestDumps,
  getTotalRequestForCurrentMonth,
  getLastSevenDaysTotalRequest,
  hasRequest,
  getTotalRequestToReport,
)
where

import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.Default
import Data.Default.Instances ()
import Data.Text qualified as T
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime, getCurrentTime)
import Data.Time.Format
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatShow)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Transact (DBT, executeMany)
import Database.PostgreSQL.Transact qualified as DBT
import Deriving.Aeson qualified as DAE
import Models.Apis.Fields.Query ()
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pkg.Parser
import Relude hiding (many, some)
import Witch (from)


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
  | JavaSpringBoot
  | JsAxiosOutgoing
  | DotNet
  | PythonFastApi
  | PythonFlask
  | PythonDjango
  | PythonOutgoing
  | JsAdonis
  | PhpSlim
  | GuzzleOutgoing
  | ElixirPhoenix
  | PythonPyramid
  | DotNetOutgoing
  | TestkitOutgoing
  | JavaSpring
  deriving stock (Show, Generic, Read, Eq)
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
  deriving stock (Show, Generic, Read, Eq)
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
-- "/abc/:bla"
-- >>> normalizeUrlPath GoGin 200 "GET" "/abc/:bla?q=abc"
-- ""
--
-- >>> normalizeUrlPath GoGin 404 "GET" "https://apitoolkit.io/abc/:bla?q=abc"
-- ""
--
-- >>> normalizeUrlPath JsExpress 200 "OPTIONS" "https://apitoolkit.io/abc/:bla?q=abc"
-- ""
-- >>> normalizeUrlPath JsExpress 200 "PATCH" "https://apitoolkit.io/abc/:bla?q=abc"
-- "/abc/:bla"
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


-- getRequestType ...
-- >>> getRequestType GoGin
-- Incoming
-- >>> getRequestType GoOutgoing
-- Outgoing
-- >>> getRequestType JsAxiosOutgoing
-- Outgoing

getRequestType :: SDKTypes -> RequestTypes
getRequestType sdkType
  | T.isSuffixOf "Outgoing" (show sdkType) = Outgoing
  | T.isSuffixOf "Background" (show sdkType) = Background
  | otherwise = Incoming


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
  { when :: ZonedTime
  , errorType :: Text
  , rootErrorType :: Text
  , message :: Text
  , rootErrorMessage :: Text
  , stackTrace :: Text
  , hash :: Maybe Text
  , technology :: Maybe SDKTypes
  , requestMethod :: Maybe Text
  , requestPath :: Maybe Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (NFData, Default)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ATError
  deriving (ToField, FromField) via Aeson ATError


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
  , protoMajor :: Int
  , protoMinor :: Int
  , duration :: CalendarDiffTime
  , statusCode :: Int
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
  , durationNs :: Integer
  , sdkType :: SDKTypes
  , parentId :: Maybe UUID.UUID
  , serviceVersion :: Maybe Text
  , errors :: AE.Value -- Vector ATError
  , tags :: V.Vector Text
  , requestType :: RequestTypes
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToRow, FromRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "request_dumps", PrimaryKey "id", FieldModifiers '[CamelToSnake]] RequestDump)


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
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToRow, FromRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "request_dumps", PrimaryKey "id", FieldModifiers '[CamelToSnake]] RequestForReport)


data EndpointPerf = EndpointPerf
  { endpointHash :: Text
  , averageDuration :: Integer
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToRow, FromRow, NFData)
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
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToRow, FromRow, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] RequestDumpLogItem


requestDumpLogItemUrlPath :: Projects.ProjectId -> RequestDumpLogItem -> Text
requestDumpLogItemUrlPath pid rd = "/p/" <> pid.toText <> "/log_explorer/" <> UUID.toText rd.id <> "/" <> from @String (formatShow iso8601Format rd.createdAt)


requestDumpLogUrlPath
  :: Projects.ProjectId
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Text
requestDumpLogUrlPath pid q cols cursorM sinceM fromM toM layoutM =
  [text|/p/$pidT/log_explorer?query=$queryT&cols=$colsT&cursor=$cursorT&since=$sinceT&from=$fromT&to=$toT&layout=$layoutT|]
  where
    pidT = pid.toText
    queryT = fromMaybe "" q
    colsT = fromMaybe "" cols
    cursorT = fromMaybe "" cursorM
    sinceT = fromMaybe "" sinceM
    fromT = fromMaybe "" fromM
    toT = fromMaybe "" toM
    layoutT = fromMaybe "" layoutM


getRequestDumpForReports :: Projects.ProjectId -> Text -> DBT IO (V.Vector RequestForReport)
getRequestDumpForReports pid report_type = query Select (Query $ encodeUtf8 q) pid
  where
    report_interval = if report_type == "daily" then ("'24 hours'" :: Text) else "'7 days'"
    q =
      [text| 
     SELECT DISTINCT ON (endpoint_hash)
        id, created_at, project_id, host, url_path, raw_url, method, endpoint_hash,
        CAST (ROUND (AVG (duration_ns) OVER (PARTITION BY endpoint_hash)) AS BIGINT) AS average_duration
     FROM
        apis.request_dumps
     WHERE
        project_id = ? AND created_at > NOW() - interval $report_interval;
    |]


getRequestDumpsForPreviousReportPeriod :: Projects.ProjectId -> Text -> DBT IO (V.Vector EndpointPerf)
getRequestDumpsForPreviousReportPeriod pid report_type = query Select (Query $ encodeUtf8 q) pid
  where
    (start, end) = if report_type == "daily" then ("'48 hours'" :: Text, "'24 hours'") else ("'14 days'", "'7 days'")
    q =
      [text| 
     SELECT  endpoint_hash,
        CAST (ROUND (AVG (duration_ns)) AS BIGINT) AS average_duration
     FROM
        apis.request_dumps
     WHERE
        project_id = ? AND created_at > NOW() - interval $start AND created_at < NOW() - interval $end
     GROUP BY endpoint_hash;
    |]


selectLogTable :: Projects.ProjectId -> Text -> Maybe UTCTime -> (Maybe UTCTime, Maybe UTCTime) -> [Text] -> DBT IO (Either Text (V.Vector (V.Vector Value), [Text], Int))
selectLogTable pid extraQuery cursorM dateRange projectedColsByUser = do
  now <- liftIO getCurrentTime
  let resp = parseQueryToComponents ((defSqlQueryCfg pid now){cursorM, dateRange, projectedColsByUser}) extraQuery
  case resp of
    Left x -> pure $ Left x
    Right (q, queryComponents) -> do
      logItems <- queryToValues q
      Only count <- fromMaybe (Only 0) <$> queryCount queryComponents.countQuery
      let logItemsV = V.mapMaybe valueToVector logItems
      pure $ Right (logItemsV, queryComponents.toColNames, count)


valueToVector :: Only Value -> Maybe (V.Vector Value)
valueToVector (Only val) = case val of
  AE.Array arr -> Just arr
  _ -> Nothing


queryToValues :: Text -> DBT IO (V.Vector (Only Value))
queryToValues q = V.fromList <$> DBT.query_ (Query $ encodeUtf8 q)


queryCount :: Text -> DBT IO (Maybe (Only Int))
queryCount q = DBT.queryOne_ (Query $ encodeUtf8 q)


selectRequestDumpByProject :: Projects.ProjectId -> Text -> Maybe Text -> Maybe ZonedTime -> Maybe ZonedTime -> DBT IO (V.Vector RequestDumpLogItem, Int)
selectRequestDumpByProject pid extraQuery cursorM fromM toM = do
  logItems <- query Select (Query $ encodeUtf8 q) (pid, cursorT)
  Only count <- fromMaybe (Only 0) <$> queryOne Select (Query $ encodeUtf8 qCount) (pid, cursorT)
  pure (logItems, count)
  where
    cursorT = fromMaybe "infinity" cursorM
    dateRangeStr = from @String $ case (fromM, toM) of
      (Nothing, Just b) -> "AND created_at BETWEEN NOW() AND '" <> formatTime defaultTimeLocale "%F %R" b <> "'"
      (Just a, Just b) -> "AND created_at BETWEEN '" <> formatTime defaultTimeLocale "%F %R" a <> "' AND '" <> formatTime defaultTimeLocale "%F %R" b <> "'"
      _ -> ""
    extraQueryParsed = either error (\v -> if v == "" then "" else " AND " <> v) $ parseQueryStringToWhereClause extraQuery
    -- We only let people search within the 14 days time period
    qCount =
      [text| SELECT count(*) 
             FROM apis.request_dumps where project_id=? and created_at > NOW() - interval '14 days' and created_at<? $dateRangeStr |]
        <> extraQueryParsed
        <> " limit 1;"
    q =
      [text| SELECT id,created_at,project_id,host,url_path,method,raw_url,referer,
                    path_params, status_code,query_params,
                    request_body,response_body,'{}'::jsonb,'{}'::jsonb,
                    duration_ns, sdk_type,
                    parent_id, service_version, JSONB_ARRAY_LENGTH(errors) as errors_count, '{}'::jsonb, tags, request_type
             FROM apis.request_dumps where project_id=? and created_at > NOW() - interval '14 days' and created_at<? $dateRangeStr |]
        <> extraQueryParsed
        <> " order by created_at desc limit 200;"


countRequestDumpByProject :: Projects.ProjectId -> DBT IO Int
countRequestDumpByProject pid = do
  result <- query Select q pid
  case result of
    [Only count] -> return count
    v -> return $ length v
  where
    q = [sql| SELECT count(*) FROM apis.request_dumps WHERE project_id=? |]


hasRequest :: Projects.ProjectId -> DBT IO Bool
hasRequest pid = do
  result <- query Select q pid
  case result of
    [Only count] -> return count
    v -> return $ length v > 0
  where
    q =
      [sql|SELECT EXISTS(
  SELECT 1
  FROM apis.request_dumps where project_id = ?
) |]


-- bulkInsertRequestDumps is a very import function because it's what inserts the request dumps into the database.
-- But it's tied to the literal database structure for performance purposes, so we need ot update this
-- if we add or remove new columns to the database table.
bulkInsertRequestDumps :: [RequestDump] -> DBT IO Int64
bulkInsertRequestDumps = executeMany q
  where
    q = [sql| INSERT INTO apis.request_dumps VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) ON CONFLICT DO NOTHING; |]


selectRequestDumpByProjectAndId :: Projects.ProjectId -> UTCTime -> UUID.UUID -> DBT IO (Maybe RequestDumpLogItem)
selectRequestDumpByProjectAndId pid createdAt rdId = queryOne Select q (createdAt, pid, rdId)
  where
    q =
      [sql|SELECT   id,created_at,project_id, host,url_path,method,raw_url,referer,
                    path_params,status_code,query_params,
                    request_body,response_body,request_headers,response_headers, 
                    duration_ns, sdk_type,
                    parent_id, service_version, JSONB_ARRAY_LENGTH(errors) as errors_count, errors, tags, request_type
             FROM apis.request_dumps where (created_at=?)  and project_id=? and id=? LIMIT 1|]


selectReqLatenciesRolledBySteps :: Int -> Int -> Projects.ProjectId -> Text -> Text -> DBT IO (V.Vector (Int, Int))
selectReqLatenciesRolledBySteps maxv steps pid urlPath method = query Select q (maxv, steps, steps, steps, pid, urlPath, method)
  where
    q =
      [sql| 
SELECT duration_steps, count(id)
	FROM generate_series(0, ?, ?) AS duration_steps
	LEFT OUTER JOIN apis.request_dumps on (duration_steps = round((EXTRACT(epoch FROM duration)/1000000)/?)*? 
    AND created_at > NOW() - interval '14' day
    AND project_id=? and url_path=? and method=?)
	GROUP BY duration_steps 
	ORDER BY duration_steps;
      |]


-- TODO: expand this into a view
selectReqLatenciesRolledByStepsForProject :: Int -> Int -> Projects.ProjectId -> (Maybe ZonedTime, Maybe ZonedTime) -> DBT IO (V.Vector (Int, Int))
selectReqLatenciesRolledByStepsForProject maxv steps pid dateRange = query Select (Query $ encodeUtf8 q) (maxv, steps, steps, steps, pid)
  where
    dateRangeStr = from @String $ case dateRange of
      (Nothing, Just b) -> "AND created_at BETWEEN NOW() AND '" <> formatTime defaultTimeLocale "%F %R" b <> "'"
      (Just a, Just b) -> "AND created_at BETWEEN '" <> formatTime defaultTimeLocale "%F %R" a <> "' AND '" <> formatTime defaultTimeLocale "%F %R" b <> "'"
      _ -> ""
    q =
      [text| 
select duration_steps, count(id)
	FROM generate_series(0, ?, ?) AS duration_steps
	LEFT OUTER JOIN apis.request_dumps on (duration_steps = round((EXTRACT(epoch FROM duration)/1000000)/?)*?
    AND project_id=? $dateRangeStr )
	GROUP BY duration_steps 
	ORDER BY duration_steps;
      |]


selectRequestDumpByProjectAndParentId :: Projects.ProjectId -> UUID.UUID -> DBT IO (V.Vector RequestDumpLogItem)
selectRequestDumpByProjectAndParentId pid parentId = query Select q (pid, parentId)
  where
    q =
      [sql|
     SELECT id,created_at,project_id,host,url_path,method,raw_url,referer,
                    path_params, status_code,query_params,
                    request_body,response_body,'{}'::jsonb,'{}'::jsonb,
                    duration_ns, sdk_type,
                    parent_id, service_version, JSONB_ARRAY_LENGTH(errors) as errors_count, '{}'::jsonb, tags, request_type
             FROM apis.request_dumps where project_id=? AND created_at > NOW() - interval '14' day AND parent_id= ? LIMIT 199; 
     |]


autoCompleteFromRequestDumps :: Projects.ProjectId -> Text -> Text -> DBT IO (V.Vector Text)
autoCompleteFromRequestDumps pid key prefix = query Select (Query $ encodeUtf8 q) (pid, prefix <> "%")
  where
    q = [text|SELECT DISTINCT $key from apis.request_dumps WHERE project_id = ? AND created_at > NOW() - interval '14' day AND $key <> ''  AND $key LIKE ?|]


getTotalRequestForCurrentMonth :: Projects.ProjectId -> DBT IO Int
getTotalRequestForCurrentMonth pid = do
  result <- queryOne Select q pid
  case fromMaybe (Only 0) result of
    (Only count) -> return count
  where
    q =
      [sql| SELECT count(*) FROM apis.request_dumps WHERE project_id=? AND EXTRACT(MONTH FROM created_at) = EXTRACT(MONTH FROM CURRENT_DATE)
              AND EXTRACT(YEAR FROM created_at) = EXTRACT(YEAR FROM CURRENT_DATE);|]


getLastSevenDaysTotalRequest :: Projects.ProjectId -> DBT IO Int
getLastSevenDaysTotalRequest pid = do
  result <- queryOne Select q pid
  case fromMaybe (Only 0) result of
    (Only count) -> return count
  where
    q =
      [sql| SELECT count(*) FROM apis.request_dumps WHERE project_id=? AND created_at > NOW() - interval '7' day;|]


getTotalRequestToReport :: Projects.ProjectId -> ZonedTime -> DBT IO Int
getTotalRequestToReport pid lastReported = do
  result <- query Select q (pid, lastReported)
  case result of
    [Only count] -> return count
    v -> return $ length v
  where
    q =
      [sql| SELECT count(*) FROM apis.request_dumps WHERE project_id=? AND created_at > ?|]
