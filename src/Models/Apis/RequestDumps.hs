{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.RequestDumps
  ( RequestDump (..),
    insertRequestDump,
  )
where

import Data.Aeson qualified as AE
import Data.Time (CalendarDiffTime, ZonedTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity (insert)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Optics.TH
import Relude

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
  deriving (Show, Generic, Eq)
  deriving (ToRow, FromRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "request_dumps", PrimaryKey "id", FieldModifiers '[StripPrefix "rd", CamelToSnake]] RequestDump)

makeFieldLabelsNoPrefix ''RequestDump

insertRequestDump :: RequestDump -> DBT IO ()
insertRequestDump = insert @RequestDump

-- selectRequestsByStatusCodesStatByDay :: DBT IO (ZonedTime, Int, Int)
-- selectRequestsByStatusCodesStatByDay = query Select q
--   where q = [sql|
--        with abc as (SELECT time_bucket('1 day', created_at) as day,
--              status_code,
--              count(id),
--            project_id, url_path, method
--         FROM apis.request_dumps
--         where created_at > NOW() - interval '14' day
--         --where project_id='', url_path='' and method='';
--         GROUP BY project_id, url_path, method, day, status_code)
--       select  status_code  from abc
--     |]
