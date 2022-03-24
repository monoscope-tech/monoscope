{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.RequestDumps
  ( RequestDump (..),
    insertRequestDump,
    selectRequestsByStatusCodesStatByMin,
    selectReqLatencyPercentiles,
    labelRequestLatency,
    LabelValue,
    Percentiles (..),
    selectReqLatenciesRolledBySteps,
    selectReqLatencyPercentilesForProject,
    selectRequestsByEndpointsStatByMin,
    selectReqLatenciesRolledByStepsForProject,
    logsByProject
  )
where

import Data.Aeson (KeyValue ((.=)), ToJSON, object)
import Data.Aeson qualified as AE
import Data.Time (CalendarDiffTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (insert, selectManyByField)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Models.Projects.Projects qualified as Projects
import Optics.TH
import Relude
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.Transact as PgT

instance Eq ZonedTime where
  (==) _ _ = True

data RequestDump = RequestDump
  { id :: UUID.UUID,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    projectId :: UUID.UUID,
    host :: Text,
    urlPath :: Text,
    method :: Text,
    referer :: Text,
    protoMajor :: Int,
    protoMinor :: Int,
    duration :: CalendarDiffTime,
    requestHeaders :: AE.Value,
    responseHeaders :: AE.Value,
    requestBody :: AE.Value,
    responseBody :: AE.Value,
    statusCode :: Int
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToRow, FromRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "request_dumps", PrimaryKey "id", FieldModifiers '[StripPrefix "rd", CamelToSnake]] RequestDump)

makeFieldLabelsNoPrefix ''RequestDump

insertRequestDump :: RequestDump -> DBT IO ()
insertRequestDump = insert @RequestDump

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

selectRequestsByEndpointsStatByMin :: Projects.ProjectId -> DBT IO (Vector (ZonedTime, Text, Int))
selectRequestsByEndpointsStatByMin pid = query Select q (Only pid)
  where
    q =
      [sql|
       SELECT time_bucket('1 minute', created_at) as timeB,
             concat_ws(' ', method, url_path)::text,
             count(id)
        FROM apis.request_dumps
        where created_at > NOW() - interval '14' day
        and project_id=?
        GROUP BY timeB, method, url_path;
    |]

data Percentiles = Percentiles
  { min :: Double,
    p50 :: Double,
    p75 :: Double,
    p90 :: Double,
    p95 :: Double,
    p99 :: Double,
    max :: Double
  }
  deriving stock (Generic)
  deriving anyclass (FromRow)

makeFieldLabelsNoPrefix ''Percentiles

selectReqLatencyPercentiles :: Projects.ProjectId -> Text -> Text -> DBT IO (Maybe Percentiles)
selectReqLatencyPercentiles pid urlPath method = queryOne Select q (pid, urlPath, method)
  where
    q =
      [sql|
      WITH latency_percentiles as (
        SELECT
          percentile_agg(EXTRACT(epoch FROM duration)) as agg
        FROM apis.request_dumps
        where created_at > NOW() - interval '14' day
        and project_id=? and url_path=? and method=?

        )
      SELECT 		
         coalesce(approx_percentile(0, agg)/10000000,0) min,
         coalesce(approx_percentile(0.50, agg)/1000000, 0) p50,
         coalesce(approx_percentile(0.75, agg)/1000000, 0) p75,
         coalesce(approx_percentile(0.90, agg)/1000000, 0) p90,
         coalesce(approx_percentile(0.95, agg)/1000000, 0) p95,
         coalesce(approx_percentile(0.99, agg)/100000, 0) p99,
         coalesce(approx_percentile(1, agg)/10000000,0) max 
        FROM latency_percentiles;                  
      |]

selectReqLatencyPercentilesForProject :: Projects.ProjectId -> DBT IO (Maybe Percentiles)
selectReqLatencyPercentilesForProject pid = queryOne Select q (Only pid)
  where
    q =
      [sql|
      WITH latency_percentiles as (
        SELECT
          percentile_agg(EXTRACT(epoch FROM duration)) as agg
        FROM apis.request_dumps
        where created_at > NOW() - interval '14' day
        and project_id=?
        )
      SELECT 		
          coalesce(approx_percentile(0, agg)/1000000, 0) min,
          coalesce(approx_percentile(0.50, agg)/1000000, 0) p50,
          coalesce(approx_percentile(0.75, agg)/1000000, 0) p75,
          coalesce(approx_percentile(0.90, agg)/1000000, 0) p90,
          coalesce(approx_percentile(0.95, agg)/1000000, 0) p95,
          coalesce(approx_percentile(0.99, agg)/1000000, 0) p99,
          coalesce(approx_percentile(1, agg)/1000000, 0) max 
        FROM latency_percentiles;
      |]

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

selectReqLatenciesRolledByStepsForProject :: Int -> Int -> Projects.ProjectId -> DBT IO (Vector (Int, Int))
selectReqLatenciesRolledByStepsForProject maxv steps pid = query Select q (maxv, steps, steps, steps, pid)
  where
    q =
      [sql| 
select duration_steps, count(id)
	FROM generate_series(0, ?, ?) AS duration_steps
	LEFT OUTER JOIN apis.request_dumps on (duration_steps = round((EXTRACT(epoch FROM duration)/1000000)/?)*? 
    AND created_at > NOW() - interval '14' day
    AND project_id=?)
	GROUP BY duration_steps 
	ORDER BY duration_steps;
      |]

-- Useful for charting latency histogram on the dashbaord and endpoint details pages
data LabelValue = LabelValue (Int, Int, Maybe Text)
  deriving stock (Show)

instance ToJSON LabelValue where
  toJSON (LabelValue (x, y, Nothing)) = object ["label" .= ((show x) :: Text), "value" .= (y :: Int)]
  toJSON (LabelValue (x, y, Just z)) =
    object
      [ "label" .= (z :: Text),
        "lineposition" .= ((show x) :: Text),
        "labelposition" .= ((show x) :: Text),
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

logsByProject :: Projects.ProjectId -> PgT.DBT IO (Vector.Vector RequestDump)
logsByProject pid = selectManyByField @RequestDump [field| project_id |] pid
