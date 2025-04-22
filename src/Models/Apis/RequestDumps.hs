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
  requestDumpLogUrlPath,
  selectRequestDumpByProject,
  selectRequestDumpByProjectAndId,
  bulkInsertRequestDumps,
  getRequestDumpForReports,
  getRequestDumpsForPreviousReportPeriod,
  selectChildSpansAndLogs,
  countRequestDumpByProject,
  getRequestType,
  autoCompleteFromRequestDumps,
  getLastSevenDaysTotalRequest,
  getTotalRequestToReport,
  parseSDKType,
)
where

import Control.Error.Util (hush)
import Data.Aeson qualified as AE
import Data.Default
import Data.Default.Instances ()
import Data.Text qualified as T
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime)
import Data.Time.Format
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (Insert, Select), executeMany, query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Transact (DBT)
import Database.PostgreSQL.Transact qualified as DBT
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Time qualified as Time
import Fmt (fmt)
import Models.Apis.Fields.Query ()
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pkg.Parser
import Pkg.Parser.Expr
import Pkg.Parser.Stats (Section, Sources (SSpans))
import Relude hiding (many, some)
import Text.Megaparsec
import Web.HttpApiData (ToHttpApiData (..))
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
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] SDKTypes


instance ToField SDKTypes where
  toField sdkType = toField @String (show sdkType)


parseSDKType :: Text -> SDKTypes
parseSDKType "GoGin" = GoGin
parseSDKType "GoBuiltIn" = GoBuiltIn
parseSDKType "GoGorillaMux" = GoGorillaMux
parseSDKType "GoFiber" = GoFiber
parseSDKType "GoDefault" = GoDefault
parseSDKType "GoOutgoing" = GoOutgoing
parseSDKType "PhpLaravel" = PhpLaravel
parseSDKType "PhpSymfony" = PhpSymfony
parseSDKType "JsNest" = JsNest
parseSDKType "JsFastify" = JsFastify
parseSDKType "JavaSpringBoot" = JavaSpringBoot
parseSDKType "JsAxiosOutgoing" = JsAxiosOutgoing
parseSDKType "DotNet" = DotNet
parseSDKType "PythonFastApi" = PythonFastApi
parseSDKType "PythonFlask" = PythonFlask
parseSDKType "PythonDjango" = PythonDjango
parseSDKType "PythonOutgoing" = PythonOutgoing
parseSDKType "JsAdonis" = JsAdonis
parseSDKType "PhpSlim" = PhpSlim
parseSDKType "GuzzleOutgoing" = GuzzleOutgoing
parseSDKType "ElixirPhoenix" = ElixirPhoenix
parseSDKType "PythonPyramid" = PythonPyramid
parseSDKType "DotNetOutgoing" = DotNetOutgoing
parseSDKType "TestkitOutgoing" = TestkitOutgoing
parseSDKType "JavaSpring" = JavaSpring
parseSDKType "JavaApacheOutgoing" = JavaApacheOutgoing
parseSDKType "JavaVertx" = JavaVertx
parseSDKType "JsOutgoing" = JsOutgoing
parseSDKType "JsNext" = JsNext
parseSDKType _ = JsExpress


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
  }
  deriving stock (Show, Generic)
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
  deriving anyclass (ToRow, NFData)


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
  deriving stock (Show, Generic)
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
  deriving stock (Show, Generic)
  deriving anyclass (ToRow, FromRow, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] RequestDumpLogItem


requestDumpLogUrlPath :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Maybe Text -> Text
requestDumpLogUrlPath pid q cols cursor since fromV toV layout source queryASTM =
  "/p/" <> pid.toText <> "/log_explorer/json?" <> T.intercalate "&" params
  where
    params =
      catMaybes
        [ fmap ("query=" <>) (toQueryParam <$> q)
        , fmap ("cols=" <>) (toQueryParam <$> cols)
        , fmap ("cursor=" <>) (toQueryParam <$> cursor)
        , fmap ("since=" <>) (toQueryParam <$> since)
        , fmap ("from=" <>) (toQueryParam <$> fromV)
        , fmap ("to=" <>) (toQueryParam <$> toV)
        , fmap ("layout=" <>) (toQueryParam <$> layout)
        , fmap ("queryAST=" <>) (toQueryParam <$> queryASTM)
        , Just ("source=" <> toQueryParam source)
        ]


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
     FROM apis.request_dumps
     WHERE project_id = ? AND created_at > NOW() - interval $start AND created_at < NOW() - interval $end
     GROUP BY endpoint_hash;
    |]


selectLogTable :: (DB :> es, Time.Time :> es) => Projects.ProjectId -> [Section] -> Maybe UTCTime -> (Maybe UTCTime, Maybe UTCTime) -> [Text] -> Maybe Sources -> Maybe Text -> Eff es (Either Text (V.Vector (V.Vector AE.Value), [Text], Int))
selectLogTable pid queryAST cursorM dateRange projectedColsByUser source targetSpansM = do
  now <- Time.currentTime
  let (q, queryComponents) = queryASTToComponents ((defSqlQueryCfg pid now source targetSpansM){cursorM, dateRange, projectedColsByUser, source, targetSpansM}) queryAST
  logItems <- queryToValues $ traceShowId q
  Only c <- fromMaybe (Only 0) <$> queryCount queryComponents.countQuery
  let logItemsV = V.mapMaybe valueToVector logItems
  pure $ Right (logItemsV, queryComponents.toColNames, c)


selectChildSpansAndLogs :: (DB :> es, Time.Time :> es) => Projects.ProjectId -> [Text] -> V.Vector Text -> Eff es (V.Vector (V.Vector AE.Value))
selectChildSpansAndLogs pid projectedColsByUser traceIds = do
  now <- Time.currentTime
  let qConfig = defSqlQueryCfg pid now (Just SSpans) Nothing
      (r, _) = getProcessedColumns projectedColsByUser qConfig.defaultSelect
      q =
        [text|SELECT json_build_array($r) FROM otel_logs_and_spans
             WHERE project_id=? and context___trace_id=Any(?) and parent_id IS NOT NULL ORDER BY timestamp DESC
           |]
  v <- dbtToEff $ query Select (Query $ encodeUtf8 q) (pid, traceIds)
  pure $ V.mapMaybe valueToVector v


valueToVector :: Only AE.Value -> Maybe (V.Vector AE.Value)
valueToVector (Only val) = case val of
  AE.Array arr -> Just arr
  _ -> Nothing


queryToValues :: DB :> es => Text -> Eff es (V.Vector (Only AE.Value))
queryToValues q = dbtToEff $ V.fromList <$> DBT.query_ (Query $ encodeUtf8 q)


queryCount :: DB :> es => Text -> Eff es (Maybe (Only Int))
queryCount q = dbtToEff $ DBT.queryOne_ (Query $ encodeUtf8 q)


selectRequestDumpByProject :: Projects.ProjectId -> Text -> Maybe Text -> Maybe ZonedTime -> Maybe ZonedTime -> DBT IO (V.Vector RequestDumpLogItem, Int)
selectRequestDumpByProject pid extraQuery cursorM fromM toM = do
  logItems <- query Select (Query $ encodeUtf8 q) (pid, cursorT)
  Only c <- fromMaybe (Only 0) <$> queryOne Select (Query $ encodeUtf8 qCount) (pid, cursorT)
  pure (logItems, c)
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
    [Only c] -> return c
    v -> return $ length v
  where
    q = [sql| SELECT count(*) FROM apis.request_dumps WHERE project_id=?  AND created_at > NOW() - interval '14 days'|]


bulkInsertRequestDumps :: DB :> es => [RequestDump] -> Eff es ()
bulkInsertRequestDumps params = void $ dbtToEff $ executeMany Insert q params
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


autoCompleteFromRequestDumps :: Projects.ProjectId -> Text -> Text -> DBT IO (V.Vector Text)
autoCompleteFromRequestDumps pid key prefix = query Select (Query $ encodeUtf8 q) (pid, prefix <> "%")
  where
    q = [text|SELECT DISTINCT $key from apis.request_dumps WHERE project_id = ? AND created_at > NOW() - interval '14' day AND $key <> ''  AND $key LIKE ?|]


getLastSevenDaysTotalRequest :: Projects.ProjectId -> DBT IO Int
getLastSevenDaysTotalRequest pid = do
  result <- queryOne Select q pid
  case fromMaybe (Only 0) result of
    (Only c) -> return c
  where
    q =
      [sql| SELECT count(*) FROM apis.request_dumps WHERE project_id=? AND created_at > NOW() - interval '7' day;|]


getTotalRequestToReport :: Projects.ProjectId -> UTCTime -> DBT IO Int
getTotalRequestToReport pid lastReported = do
  result <- query Select q (pid, lastReported)
  case result of
    [Only c] -> return c
    v -> return $ length v
  where
    q =
      [sql| SELECT count(*) FROM apis.request_dumps WHERE project_id=? AND created_at > ?|]
