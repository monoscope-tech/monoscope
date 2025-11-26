module Models.Apis.Reports (
  Report (..),
  ReportId,
  ReportListItem (..),
  addReport,
  reportHistoryByProject,
  getReportById,
) where

import Data.Aeson qualified as AE
import Data.Default.Instances ()
import Data.Time (UTCTime, ZonedTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (insert, selectById)
import Database.PostgreSQL.Entity.DBT (query)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple hiding (execute, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Relude


type ReportId = UUIDId "report"


data Report = Report
  { id :: ReportId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , reportType :: Text
  , reportJson :: AE.Value
  , startTime :: UTCTime
  , endTime :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "reports", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Report)


data ReportListItem = ReportListItem
  { id :: ReportId
  , createdAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , reportType :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "reports", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ReportListItem)


addReport :: Report -> DBT IO ()
addReport = insert @Report


getReportById :: ReportId -> DBT IO (Maybe Report)
getReportById id' = selectById (Only id')


reportHistoryByProject :: Projects.ProjectId -> Int -> DBT IO (V.Vector ReportListItem)
reportHistoryByProject pid page = query q (pid, offset)
  where
    offset = page * 20
    q =
      [sql| SELECT id, created_at, project_id, report_type FROM apis.reports
    WHERE project_id = ? 
    ORDER BY created_at DESC
    LIMIT 20 OFFSET ?;
  |]

--   selectManyByField [field| project_id |] pid
