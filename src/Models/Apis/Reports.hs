{-# LANGUAGE DuplicateRecordFields #-}

module Models.Apis.Reports (
  Report (..),
  ReportId (..),
  ReportListItem (..),
  addReport,
  reportHistoryByProject,
  getReportById,
) where

import Data.Default (Default)
import Data.Default.Instances ()
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Relude
import Web.HttpApiData (FromHttpApiData)

import Data.Aeson as Aeson
import Database.PostgreSQL.Entity (insert, selectById)

import Database.PostgreSQL.Simple hiding (execute, query)

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact (DBT)

newtype ReportId = ReportId {reportId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, ToJSON, FromJSON, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID

instance HasField "toText" ReportId Text where
  getField = UUID.toText . reportId

data Report = Report
  { id :: ReportId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , reportType :: Text
  , reportJson :: Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "reports", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Report)

data ReportListItem = ReportListItem
  { id :: ReportId
  , createdAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , reportType :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "reports", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ReportListItem)

addReport :: Report -> DBT IO ()
addReport = insert @Report

getReportById :: ReportId -> DBT IO (Maybe Report)
getReportById id' = selectById (Only id')

reportHistoryByProject :: Projects.ProjectId -> Int -> DBT IO (Vector ReportListItem)
reportHistoryByProject pid page = query Select q (pid, offset)
  where
    offset = page * 20
    q =
      [sql| SELECT id, created_at, project_id, report_type FROM apis.reports
    WHERE project_id = ? 
    ORDER BY created_at DESC
    LIMIT 20 OFFSET ?;
  |]

--   selectManyByField [field| project_id |] pid
