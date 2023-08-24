{-# LANGUAGE DuplicateRecordFields #-}

module Models.Apis.Reports (
  Report (..),
  ReportId (..),
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

addReport :: Report -> DBT IO ()
addReport = insert @Report

getReportById :: Text -> DBT IO (Maybe Report)
getReportById id' = selectById (Only id')

reportHistoryByProject :: Projects.ProjectId -> DBT IO (Vector Report)
reportHistoryByProject pid = query Select q (Only pid)
 where
  q =
    [sql| SELECT * FROM apis.reports
    WHERE project_id = ? 
    ORDER BY created_at DESC
  |]

--   selectManyByField [field| project_id |] pid
