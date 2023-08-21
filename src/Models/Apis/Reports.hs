{-# LANGUAGE DuplicateRecordFields #-}

module Models.Projects.Swaggers (
  Report (..),
  ReportId (..),
  addReport,
  reportHistoryByProject,
  getReportById,
) where

import Data.Aeson as Aeson
import Data.Default (Default)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (Entity, insert, selectById, selectManyByField)
import Database.PostgreSQL.Entity.Internal.QQ (field)
import Database.PostgreSQL.Entity.Types (CamelToSnake, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple hiding (execute, query)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact (DBT)
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Relude
import Servant (FromHttpApiData)

newtype ReportId = ReportId {reportId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, ToJSON, FromJSON, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID

instance HasField "toText" ReportId Text where
  getField = UUID.toText . reportId

data Report = Report
  { id :: ReportId
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
reportHistoryByProject pid = selectManyByField [field| project_id |] pid
