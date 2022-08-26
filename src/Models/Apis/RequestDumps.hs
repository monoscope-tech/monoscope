{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.RequestDumps
  ( RequestDump (..),
    LabelValue,
    RequestDumpLogItem,
    labelRequestLatency,
    requestDumpLogItemUrlPath,
    requestDumpLogUrlPath,
    selectReqLatenciesRolledBySteps,
    selectReqLatenciesRolledByStepsForProject,
    selectRequestDumpByProject,
    selectRequestDumpByProjectAndId,
    selectRequestDumpsByProjectForChart,
    selectRequestsByEndpointsStatByMin,
    selectRequestsByStatusCodesStatByMin,
    bulkInsertRequestDumps,
  )
where

import Data.Aeson (KeyValue ((.=)), ToJSON, object)
import Data.Aeson qualified as AE
import Data.Default.Instances ()
import Data.Time (CalendarDiffTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Transact (DBT, executeMany)
import Deriving.Aeson qualified as DAE
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Optics.TH
import Pkg.Parser
import Relude hiding (many, some)
import Utils ()

-- request dumps are time series dumps representing each requests which we consume from our users.
-- We use this field via the log explorer for exploring and searching traffic. And at the moment also use it for most time series analytics.
-- It's likely a good idea to stop relying on it for some of the time series analysis, to allow us easily support request sampling, but still support
-- relatively accurate analytic counts.
-- NOTE: This record closely mirrors the order of fields in the table. Changing the orfer of fields here would break inserting and querying request dumps
data RequestDump = RequestDump
  { id :: UUID.UUID,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    projectId :: UUID.UUID,
    host :: Text,
    urlPath :: Text,
    rawUrl :: Text,
    pathParams :: AE.Value,
    method :: Text,
    referer :: Text,
    protoMajor :: Int,
    protoMinor :: Int,
    duration :: CalendarDiffTime,
    statusCode :: Int,
    --
    queryParams :: AE.Value,
    requestHeaders :: AE.Value,
    responseHeaders :: AE.Value,
    requestBody :: AE.Value,
    responseBody :: AE.Value,
    --
    endpointHash :: Text,
    shapeHash :: Text,
    formatHashes :: Vector Text,
    fieldHashes :: Vector Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToRow, FromRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "request_dumps", PrimaryKey "id", FieldModifiers '[CamelToSnake]] RequestDump)

makeFieldLabelsNoPrefix ''RequestDump

-- RequestDumpLogItem is used in the to query log items for the log query explorer on the dashboard. Each item here can be queried
-- via the query language on said dashboard page.
data RequestDumpLogItem = RequestDumpLogItem
  { id :: UUID.UUID,
    createdAt :: ZonedTime,
    host :: Text,
    urlPath :: Text,
    method :: Text,
    rawUrl :: Text,
    referer :: Text,
    --
    pathParams :: AE.Value,
    -- duration :: CalendarDiffTime,
    statusCode :: Int,
    --
    queryParams :: AE.Value,
    requestBody :: AE.Value,
    responseBody :: AE.Value,
    requestHeaders :: AE.Value,
    responseHeaders :: AE.Value,
    fullCount :: Int
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToRow, FromRow)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] RequestDumpLogItem

makeFieldLabelsNoPrefix ''RequestDumpLogItem

requestDumpLogItemUrlPath :: Projects.ProjectId -> UUID.UUID -> Text
requestDumpLogItemUrlPath pid rdId = "/p/" <> Projects.projectIdText pid <> "/log_explorer/" <> UUID.toText rdId

requestDumpLogUrlPath :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Text
requestDumpLogUrlPath pid q cols fromM = [text|/p/$pidT/log_explorer?query=$queryT&cols=$colsT&from=$fromT|]
  where
    pidT = Projects.projectIdText pid
    queryT = fromMaybe "" q
    colsT = fromMaybe "" cols
    fromT = fromMaybe "" fromM

selectRequestDumpByProject :: Projects.ProjectId -> Text -> Maybe Text -> DBT IO (Vector RequestDumpLogItem)
selectRequestDumpByProject pid extraQuery fromM = query Select (Query $ encodeUtf8 q) (pid, fromT)
  where
    fromT = fromMaybe "infinity" fromM
    extraQueryParsed = either error (\v -> if v == "" then "" else " AND " <> v) $ parseQueryStringToWhereClause extraQuery
    q =
      [text| SELECT id,created_at,host,url_path,method,raw_url,referer,
                    path_params,status_code,query_params,
                    request_body,response_body,request_headers,response_headers,
                    count(*) OVER() AS full_count
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
      [text| SELECT COALESCE(NULLIF(json_agg(json_build_array(timeB, count))::text, '[null]'), '[]')::text from (SELECT time_bucket('1 minute', created_at) as timeB,count(*) 
               FROM apis.request_dumps where project_id=? $extraQueryParsed  GROUP BY timeB) ts|]

bulkInsertRequestDumps :: [RequestDump] -> DBT IO Int64
bulkInsertRequestDumps = executeMany q
  where
    q = [sql| INSERT INTO apis.request_dumps VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?); |]

selectRequestDumpByProjectAndId :: Projects.ProjectId -> UUID.UUID -> DBT IO (Maybe RequestDumpLogItem)
selectRequestDumpByProjectAndId pid rdId = queryOne Select q (pid, rdId)
  where
    q =
      [sql|SELECT   id,created_at,host,url_path,method,raw_url,referer,
                    path_params,status_code,query_params,
                    request_body,response_body,request_headers,response_headers,
                    count(*) OVER() AS full_count
             FROM apis.request_dumps where project_id=? and id=?|]

selectRequestsByStatusCodesStatByMin :: Projects.ProjectId -> Text -> Text -> DBT IO (Vector (ZonedTime, Text, Int))
selectRequestsByStatusCodesStatByMin pid urlPath method = query Select q (pid, urlPath, method)
  where
    q =
      [sql|
       SELECT time_bucket('1 minute', created_at) as timeB,
             status_code::text,
             count(id)
        FROM apis.request_dumps
        where created_at > NOW() - interval '14' day
        and project_id=? and url_path=? and method=?
        GROUP BY timeB, status_code;
    |]

selectRequestsByEndpointsStatByMin :: Projects.ProjectId -> DBT IO Text
selectRequestsByEndpointsStatByMin pid = do
  let q = [sql| SELECT ts_text FROM apis.project_requests_by_endpoint_per_min WHERE project_id=?; |]
  (Only val) <- fromMaybe (Only "[]") <$> queryOne Select q (Only pid)
  pure val

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
selectReqLatenciesRolledByStepsForProject :: Int -> Int -> Projects.ProjectId -> DBT IO (Vector (Int, Int))
selectReqLatenciesRolledByStepsForProject maxv steps pid = query Select q (maxv, steps, steps, steps, pid)
  where
    q =
      [sql| 
select duration_steps, count(id)
	FROM generate_series(0, ?, ?) AS duration_steps
	LEFT OUTER JOIN apis.request_dumps on (duration_steps = round((EXTRACT(epoch FROM duration)/1000000)/?)*? AND created_at > NOW() - interval '14' day
    AND project_id=?)
	GROUP BY duration_steps 
	ORDER BY duration_steps;
      |]

-- Useful for charting latency histogram on the dashbaord and endpoint details pages
newtype LabelValue = LabelValue (Int, Int, Maybe Text)
  deriving stock (Show)

instance ToJSON LabelValue where
  toJSON (LabelValue (x, y, Nothing)) = object ["label" .= (show x :: Text), "value" .= (y :: Int)]
  toJSON (LabelValue (x, y, Just z)) =
    object
      [ "label" .= (z :: Text),
        "lineposition" .= (show x :: Text),
        "labelposition" .= (show x :: Text),
        "vline" .= ("true" :: Text),
        "labelhalign" .= ("center" :: Text),
        "dashed" .= ("1" :: Text)
      ]

labelRequestLatency :: (Int, Int, Int, Int) -> (Int, Int) -> [LabelValue]
labelRequestLatency (pMax, p90, p75, p50) (x, y)
  | x == pMax = [LabelValue (x, y, Just "max"), LabelValue (x, y, Nothing)]
  | x == p90 = [LabelValue (x, y, Just "p90"), LabelValue (x, y, Nothing)]
  | x == p75 = [LabelValue (x, y, Just "p75"), LabelValue (x, y, Nothing)]
  | x == p50 = [LabelValue (x, y, Just "p50"), LabelValue (x, y, Nothing)]
  | otherwise = [LabelValue (x, y, Nothing)]
