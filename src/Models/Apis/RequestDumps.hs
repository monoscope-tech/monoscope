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
  getRequestDumpForReports,
  getRequestDumpsForPreviousReportPeriod,
  getRequestType,
  getLastSevenDaysTotalRequest,
  parseSDKType,
)
where

import Control.Exception.Annotated (checkpoint)
import Data.Aeson qualified as AE
import Data.Annotation (toAnnotation)
import Data.Default
import Data.Default.Instances ()
import Data.Text qualified as T
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (query, queryOne)
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
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Time qualified as Time
import Log qualified
import Models.Apis.Fields.Query ()
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pkg.Parser
import Pkg.Parser.Stats (Section, Sources)
import Relude hiding (many, some)
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
parseSDKType "SDKUnknown" = SDKUnknown
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


requestDumpLogUrlPath :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Bool -> Text
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
        , Just ("source=" <> toQueryParam source)
        ]


getRequestDumpForReports :: Projects.ProjectId -> Text -> DBT IO (V.Vector RequestForReport)
getRequestDumpForReports pid report_type = query (Query $ encodeUtf8 q) pid
  where
    report_interval = if report_type == "daily" then ("'24 hours'" :: Text) else "'7 days'"
    q =
      [text|
     SELECT DISTINCT ON (endpoint_hash)
        id, created_at, project_id, host, url_path, raw_url, method, endpoint_hash,
        CAST (ROUND (AVG (duration_ns) OVER (PARTITION BY endpoint_hash)) AS BIGINT) AS average_duration
     FROM apis.request_dumps
     WHERE project_id = ? AND created_at > NOW() - interval $report_interval;
    |]


getRequestDumpsForPreviousReportPeriod :: Projects.ProjectId -> Text -> DBT IO (V.Vector EndpointPerf)
getRequestDumpsForPreviousReportPeriod pid report_type = query (Query $ encodeUtf8 q) pid
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


selectLogTable :: (DB :> es, Log :> es, Time.Time :> es) => Projects.ProjectId -> [Section] -> Text -> Maybe UTCTime -> (Maybe UTCTime, Maybe UTCTime) -> [Text] -> Maybe Sources -> Maybe Text -> Eff es (Either Text (V.Vector (V.Vector AE.Value), [Text], Int))
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
        ]
    )

  logItems <- checkpoint (toAnnotation ("selectLogTable", q)) $ queryToValues q
  Only c <- fromMaybe (Only 0) <$> queryCount queryComponents.countQuery
  let logItemsV = V.mapMaybe valueToVector logItems
  pure $ Right (logItemsV, queryComponents.toColNames, c)


valueToVector :: Only AE.Value -> Maybe (V.Vector AE.Value)
valueToVector (Only val) = case val of
  AE.Array arr -> Just arr
  _ -> Nothing


queryToValues :: DB :> es => Text -> Eff es (V.Vector (Only AE.Value))
queryToValues q = dbtToEff $ V.fromList <$> DBT.query_ (Query $ encodeUtf8 q)


queryCount :: DB :> es => Text -> Eff es (Maybe (Only Int))
queryCount q = dbtToEff $ DBT.queryOne_ (Query $ encodeUtf8 q)


getLastSevenDaysTotalRequest :: Projects.ProjectId -> DBT IO Int
getLastSevenDaysTotalRequest pid = do
  result <- queryOne q pid
  case fromMaybe (Only 0) result of
    (Only c) -> return c
  where
    q =
      [sql| SELECT count(*) FROM otel_logs_and_spans WHERE project_id=? AND timestamp > NOW() - interval '1' day;|]
