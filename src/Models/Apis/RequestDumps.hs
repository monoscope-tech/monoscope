{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.RequestDumps (
  RequestDump (..),
  SDKTypes (..),
  RequestDumpLogItem,
  EndpointPerf (..),
  RequestForReport (..),
  normalizeUrlPath,
  throughputBy,
  throughputBy',
  latencyBy,
  requestDumpLogItemUrlPath,
  requestDumpLogUrlPath,
  selectReqLatenciesRolledBySteps,
  selectReqLatenciesRolledByStepsForProject,
  selectRequestDumpByProject,
  selectRequestDumpByProjectAndId,
  selectRequestDumpsByProjectForChart,
  bulkInsertRequestDumps,
  getRequestDumpForReports,
  getRequestDumpsForPreviousReportPeriod,
) where

import Control.Error (hush)
import Data.Aeson qualified as AE
import Data.Default.Instances ()
import Data.Scientific (Scientific)
import Data.Time (CalendarDiffTime, ZonedTime, defaultTimeLocale, diffUTCTime, formatTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatShow)
import Data.Tuple.Extra (both)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ResultError (ConversionFailed), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (fromField), returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Transact (DBT, executeMany)
import Deriving.Aeson qualified as DAE
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Optics.TH
import Pkg.Parser
import Relude hiding (many, some)
import Utils (DBField (MkDBField), quoteTxt)
import Witch (from)

data SDKTypes = GoGin | GoBuiltIn | PhpLaravel | PhpSymfony | JsExpress | JsNest | JavaSpringBoot | DotNet
  deriving stock (Show, Generic, Read, Eq)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] SDKTypes

instance ToField SDKTypes where
  toField sdkType = toField @String (show sdkType)

instance FromField SDKTypes where
  fromField f mdata = do
    str <- fromField f mdata
    case readMaybe str of
      Just sdkType -> return sdkType
      Nothing -> return GoGin

-- Nothing -> returnError ConversionFailed f ("Could not read SDKTypes: " ++ str)

-- normalize URLPatg based off the SDKTypes. Should allow us have custom logic to parse and transform url paths into a form we are happy with, per library
normalizeUrlPath :: SDKTypes -> Text -> Text
normalizeUrlPath GoGin urlPath = urlPath
normalizeUrlPath GoBuiltIn urlPath = urlPath
normalizeUrlPath PhpLaravel urlPath = urlPath
normalizeUrlPath PhpSymfony urlPath = urlPath
normalizeUrlPath JsExpress urlPath = urlPath
normalizeUrlPath JavaSpringBoot urlPath = urlPath
normalizeUrlPath JsNest urlPath = urlPath
normalizeUrlPath DotNet urlPath = urlPath

-- request dumps are time series dumps representing each requests which we consume from our users.
-- We use this field via the log explorer for exploring and searching traffic. And at the moment also use it for most time series analytics.
-- It's likely a good idea to stop relying on it for some of the time series analysis, to allow us easily support request sampling, but still support
-- relatively accurate analytic counts.
-- NOTE: This record closely mirrors the order of fields in the table. Changing the orfer of fields here would break inserting and querying request dumps
data RequestDump = RequestDump
  { id :: UUID.UUID
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
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
  , formatHashes :: Vector Text
  , fieldHashes :: Vector Text
  , durationNs :: Integer
  , sdkType :: SDKTypes
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToRow, FromRow)
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
  , averageDuration :: Scientific
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToRow, FromRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "request_dumps", PrimaryKey "id", FieldModifiers '[CamelToSnake]] RequestForReport)

data EndpointPerf = EndpointPerf
  { endpointHash :: Text
  , averageDuration :: Scientific
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToRow, FromRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "request_dumps", PrimaryKey "id", FieldModifiers '[CamelToSnake]] EndpointPerf)

makeFieldLabelsNoPrefix ''RequestForReport

-- RequestDumpLogItem is used in the to query log items for the log query explorer on the dashboard. Each item here can be queried
-- via the query language on said dashboard page.
data RequestDumpLogItem = RequestDumpLogItem
  { id :: UUID.UUID
  , createdAt :: ZonedTime
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
  , fullCount :: Int
  , durationNs :: Integer
  , sdkType :: SDKTypes
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToRow, FromRow)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] RequestDumpLogItem

makeFieldLabelsNoPrefix ''RequestDumpLogItem

requestDumpLogItemUrlPath :: Projects.ProjectId -> RequestDumpLogItem -> Text
requestDumpLogItemUrlPath pid rd = "/p/" <> pid.toText <> "/log_explorer/" <> UUID.toText rd.id <> "/" <> from @String (formatShow iso8601Format rd.createdAt)

requestDumpLogUrlPath :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Text
requestDumpLogUrlPath pid q cols fromM = [text|/p/$pidT/log_explorer?query=$queryT&cols=$colsT&from=$fromT|]
 where
  pidT = pid.toText
  queryT = fromMaybe "" q
  colsT = fromMaybe "" cols
  fromT = fromMaybe "" fromM

getRequestDumpForReports :: Projects.ProjectId -> Text -> DBT IO (Vector RequestForReport)
getRequestDumpForReports pid report_type = query Select (Query $ encodeUtf8 q) pid
 where
  report_interval = if report_type == "daily" then ("'24 hours'" :: Text) else "'7 days'"
  q =
    [text| 
     SELECT DISTINCT ON (endpoint_hash)
        id, created_at, project_id, host, url_path, raw_url, method, endpoint_hash,
        AVG(duration_ns) OVER (PARTITION BY endpoint_hash) AS average_duration
     FROM
        apis.request_dumps
     WHERE
        project_id = ? AND created_at > NOW() - interval $report_interval;
    |]

getRequestDumpsForPreviousReportPeriod :: Projects.ProjectId -> Text -> DBT IO (Vector EndpointPerf)
getRequestDumpsForPreviousReportPeriod pid report_type = query Select (Query $ encodeUtf8 q) pid
 where
  (start, end) = if report_type == "daily" then ("'48 hours'" :: Text, "'24 hours'") else ("'14 days'", "'7 days'")
  q =
    [text| 
     SELECT  endpoint_hash,
        AVG(duration_ns) AS average_duration
     FROM
        apis.request_dumps
     WHERE
        project_id = ? AND created_at > NOW() - interval $start AND created_at < NOW() - interval $end
     GROUP BY endpoint_hash;
    |]

selectRequestDumpByProject :: Projects.ProjectId -> Text -> Maybe Text -> DBT IO (Vector RequestDumpLogItem)
selectRequestDumpByProject pid extraQuery fromM = query Select (Query $ encodeUtf8 q) (pid, fromT)
 where
  fromT = fromMaybe "infinity" fromM
  extraQueryParsed = either error (\v -> if v == "" then "" else " AND " <> v) $ parseQueryStringToWhereClause extraQuery
  q =
    [text| SELECT id,created_at,host,url_path,method,raw_url,referer,
                    path_params, status_code,query_params,
                    request_body,response_body,request_headers,response_headers,
                    count(*) OVER() AS full_count, duration_ns, sdk_type
             FROM apis.request_dumps where project_id=? and created_at<? |]
      <> extraQueryParsed
      <> " order by created_at desc limit 200;"

selectRequestDumpsByProjectForChart :: Projects.ProjectId -> Text -> DBT IO Text
selectRequestDumpsByProjectForChart pid extraQuery = do
  (Only val) <- fromMaybe (Only "[]") <$> queryOne Select (Query $ encodeUtf8 q) (Only pid)
  pure val
 where
  extraQueryParsed = either error (\v -> if v == "" then "" else " AND " <> v) $ parseQueryStringToWhereClause extraQuery
  q =
    [text| SELECT COALESCE(NULLIF(json_agg(json_build_array(timeB, count))::text, '[null]'), '[]')::text 
              from (
                SELECT time_bucket('1 minute', created_at) as timeB,count(*)
               FROM apis.request_dumps where project_id=? $extraQueryParsed  GROUP BY timeB) ts|]

-- bulkInsertRequestDumps :: [RequestDump] -> DBT IO Int64
-- bulkInsertRequestDumps _ = pure 0
--
bulkInsertRequestDumps :: [RequestDump] -> DBT IO Int64
bulkInsertRequestDumps = executeMany q
 where
  q = [sql| INSERT INTO apis.request_dumps VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?); |]

selectRequestDumpByProjectAndId :: Projects.ProjectId -> ZonedTime -> UUID.UUID -> DBT IO (Maybe RequestDumpLogItem)
selectRequestDumpByProjectAndId pid createdAt rdId = queryOne Select q (createdAt, pid, rdId)
 where
  q =
    [sql|SELECT   id,created_at,host,url_path,method,raw_url,referer,
                    path_params,status_code,query_params,
                    request_body,response_body,request_headers,response_headers,
                    0 AS full_count, duration_ns, sdk_type
             FROM apis.request_dumps where created_at=? and project_id=? and id=? LIMIT 1|]

selectReqLatenciesRolledBySteps :: Int -> Int -> Projects.ProjectId -> Text -> Text -> DBT IO (Vector (Int, Int))
selectReqLatenciesRolledBySteps maxv steps pid urlPath method = query Select q (maxv, steps, steps, steps, pid, urlPath, method)
 where
  q =
    [sql| 
select duration_steps, count(id)
	FROM generate_series(0, ?, ?) AS duration_steps
	LEFT OUTER JOIN apis.request_dumps on (duration_steps = round((EXTRACT(epoch FROM duration)/1000000)/?)*? 
    AND created_at > NOW() - interval '14' day
    AND project_id=? and url_path=? and method=?)
	GROUP BY duration_steps 
	ORDER BY duration_steps;
      |]

-- TODO: expand this into a view
selectReqLatenciesRolledByStepsForProject :: Int -> Int -> Projects.ProjectId -> (Maybe ZonedTime, Maybe ZonedTime) -> DBT IO (Vector (Int, Int))
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

latencyBy :: Projects.ProjectId -> Maybe Text -> Int -> (Maybe ZonedTime, Maybe ZonedTime) -> DBT IO Text
latencyBy pid endpointHash numSlots dateRange@(fromT, toT) = do
  let interval = case dateRange of
        (Just a, Just b) -> diffUTCTime (zonedTimeToUTC b) (zonedTimeToUTC a)
        _ -> 60 * 60 * 24 * 14

  let intervalT = from @String @Text $ show $ floor interval `div` (if numSlots == 0 then 1 else numSlots)
  let dateRange' = both (quoteTxt . from @String . formatTime defaultTimeLocale "%F %R" <$>) dateRange
  let dateRangeStr = case dateRange' of
        (Nothing, Just b) -> "AND timeb BETWEEN NOW() AND " <> b
        (Just a, Just b) -> "AND timeb BETWEEN " <> a <> " AND " <> b
        _ -> ""
  let (fromD, toD) = bimap (maybe "now() - INTERVAL '14 days'" ("TIMESTAMP " <>)) (maybe "now()" ("TIMESTAMP " <>)) dateRange'
  let q =
        [text| 
  with f as (  
    SELECT
            time_bucket_gapfill('$intervalT seconds', timeb, $fromD, $toD) time,
            approx_percentile(0.5, rollup(agg))/1000000 p50,
            approx_percentile(0.75, rollup(agg))/1000000 p75,
            approx_percentile(0.9, rollup(agg))/1000000 p90
    from
        apis.project_requests_by_endpoint_per_min
    where
        project_id = ?
        $dateRangeStr
    group by
        time
  ) 
  SELECT COALESCE(json_agg(json_build_array(to_char(f.time, 'YYYY-DD-MM HH24:MI:SS'), f.p50, f.p75, f.p90)), '[]')::text  from f; 
  |]
  (Only val) <- fromMaybe (Only "[]") <$> queryOne Select (Query $ encodeUtf8 q) pid
  pure val

-- A throughput chart query for the request_dump table.
-- daterange :: (Maybe Int, Maybe Int)?
-- We have a requirement that the date range could either be an interval like now to 7 days ago, or be specific dates like day x to day y.
-- Now thinking about it, it might be easier to calculate 7days ago into a specific date than dealing with integer day ranges.
throughputBy :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Int -> Maybe Int -> Maybe Text -> (Maybe ZonedTime, Maybe ZonedTime) -> DBT IO Text
throughputBy pid groupByM endpointHash shapeHash formatHash statusCodeGT numSlots limitM extraQuery dateRange@(fromT, toT) = do
  let extraQueryParsed = hush . parseQueryStringToWhereClause =<< extraQuery
  let condlist =
        catMaybes
          [ " endpoint_hash=? " <$ endpointHash
          , " shape_hash=? " <$ shapeHash
          , " ?=ANY(format_hashes) " <$ formatHash
          , " status_code>? " <$ statusCodeGT
          , extraQueryParsed
          ]
  let groupBy' = fromMaybe @Text "" $ mappend " ," <$> groupByM
  let (groupBy, groupByFields) = case groupByM of
        Just "endpoint" -> (",method, url_path", ",method||' '||url_path as g")
        Nothing -> ("", "")
        _ -> (groupBy', groupBy' <> " as g")
  let groupByFinal = maybe "" (const ",g") groupByM
  let paramList = mapMaybe (MkDBField <$>) [endpointHash, shapeHash, formatHash, statusCodeGT]

  let condlist' = filter (/= "") condlist
  let cond
        | null condlist' = mempty
        | otherwise = "AND " <> mconcat (intersperse " AND " condlist')

  let limit = maybe "" (("limit " <>) . show) limitM
  let interval = case dateRange of
        (Just a, Just b) -> diffUTCTime (zonedTimeToUTC b) (zonedTimeToUTC a)
        _ -> 60 * 60 * 24 * 14

  let intervalT = from @String @Text $ show $ floor interval `div` (if numSlots == 0 then 1 else numSlots)
  let dateRange' = both (quoteTxt . from @String . formatTime defaultTimeLocale "%F %R" <$>) dateRange
  let dateRangeStr = case dateRange' of
        (Nothing, Just b) -> "AND created_at BETWEEN NOW() AND " <> b
        (Just a, Just b) -> "AND created_at BETWEEN " <> a <> " AND " <> b
        _ -> "AND created_at BETWEEN NOW() - INTERVAL '14 days' AND NOW()"
  let (fromD, toD) = bimap (maybe "now() - INTERVAL '14 days'" ("TIMESTAMP " <>)) (maybe "now()" ("TIMESTAMP " <>)) dateRange'
  let q =
        [text| WITH q as (SELECT time_bucket_gapfill('$intervalT seconds', created_at, $fromD,$toD) as timeB $groupByFields , COALESCE(COUNT(*), 0) total_count 
                  FROM apis.request_dumps 
                  WHERE project_id=? $cond $dateRangeStr GROUP BY timeB $groupBy $limit)
              SELECT COALESCE(json_agg(json_build_array(timeB $groupByFinal, total_count)), '[]')::text from q; |]
  (Only val) <- fromMaybe (Only "[]") <$> queryOne Select (Query $ encodeUtf8 q) (MkDBField pid : paramList)
  pure val

throughputBy' :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Int -> Maybe Int -> Maybe Text -> (Maybe ZonedTime, Maybe ZonedTime) -> DBT IO (Vector (Int, Int, String))
throughputBy' pid groupByM endpointHash shapeHash formatHash statusCodeGT numSlots limitM extraQuery dateRange@(fromT, toT) = do
  let extraQueryParsed = hush . parseQueryStringToWhereClause =<< extraQuery
  let condlist =
        catMaybes
          [ " endpoint_hash=? " <$ endpointHash
          , " shape_hash=? " <$ shapeHash
          , " ?=ANY(format_hashes) " <$ formatHash
          , " status_code>? " <$ statusCodeGT
          , extraQueryParsed
          ]
  let groupBy' = fromMaybe @Text "" $ mappend " ," <$> groupByM
  let (groupBy, groupByFields) = case groupByM of
        Just "endpoint" -> (",method, url_path", ",method||' '||url_path as g")
        Nothing -> ("", "")
        _ -> (groupBy', groupBy' <> "::text as g")
  let groupByFinal = maybe "" (const ",g") groupByM
  let paramList = mapMaybe (MkDBField <$>) [endpointHash, shapeHash, formatHash, statusCodeGT]
  let condlist' = filter (/= "") condlist
  let cond
        | null condlist' = mempty
        | otherwise = "AND " <> mconcat (intersperse " AND " condlist')
  let limit = maybe "" (("limit " <>) . show) limitM
  let interval = case dateRange of
        (Just a, Just b) -> diffUTCTime (zonedTimeToUTC b) (zonedTimeToUTC a)
        _ -> 60 * 60 * 24 * 14

  let intervalT = from @String @Text $ show $ floor interval `div` (if numSlots == 0 then 1 else numSlots)
  let dateRange' = both (quoteTxt . from @String . formatTime defaultTimeLocale "%F %R" <$>) dateRange
  let dateRangeStr = case dateRange' of
        (Nothing, Just b) -> "AND created_at BETWEEN NOW() AND " <> b
        (Just a, Just b) -> "AND created_at BETWEEN " <> a <> " AND " <> b
        _ -> "AND created_at BETWEEN NOW() - INTERVAL '14 days' AND NOW()"
  -- let (fromD, toD) = bimap (maybe "now() - INTERVAL '14 days'" ("TIMESTAMP " <> ) )  (maybe "now()" ("TIMESTAMP " <>)) dateRange'
  let q =
        [text| SELECT extract(epoch from time_bucket('$intervalT seconds', created_at))::integer as timeB, COALESCE(COUNT(*), 0) total_count $groupByFields  
                  FROM apis.request_dumps 
                  WHERE project_id=? $cond $dateRangeStr GROUP BY timeB $groupBy $limit; |]
  query Select (Query $ encodeUtf8 q) (MkDBField pid : paramList)
