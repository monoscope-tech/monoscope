{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.RequestDumps
  ( RequestDump (..),
    insertRequestDump,
    selectRequestsByStatusCodesStatByMin,
  )
where

import Data.Aeson qualified as AE
import Data.Time (CalendarDiffTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (insert)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Models.Projects.Projects qualified as Projects
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

selectRequestsByStatusCodesStatByMin :: Projects.ProjectId -> Text -> Text -> DBT IO (Vector (ZonedTime, Text, Int))
selectRequestsByStatusCodesStatByMin pid urlPath method = query Select q (pid, urlPath, method)
  where
    q =
      [sql|
       SELECT time_bucket('1 minute', created_at) as day,
             status_code::text,
             count(id)
        FROM apis.request_dumps
        where created_at > NOW() - interval '14' day
        and project_id=? and url_path=? and method=?
        GROUP BY day, status_code;
    |]
