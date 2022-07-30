{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Projects.Projects
  ( Project (..),
    ProjectId (..),
    CreateProject (..),
    ProjectRequestStats (..),
    insertProject,
    projectIdText,
    selectProjectsForUser,
    projectRequestStatsByProject,
    selectProjectForUser,
    updateProject,
    deleteProject,
    projectById,
    ProjectCache (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import Models.Users.Users qualified as Users
import Optics.TH
import Relude
import Web.HttpApiData

newtype ProjectId = ProjectId {unProjectId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, ToJSON, FromJSON, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID
  deriving anyclass (FromRow, ToRow)

projectIdText :: ProjectId -> Text
projectIdText = UUID.toText . unProjectId

data Project = Project
  { id :: ProjectId,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    deletedAt :: Maybe ZonedTime,
    active :: Bool,
    title :: Text,
    description :: Text
    -- NOTE: We used to have hsots under project, but now hosts should be gotten from the endpoints.
    -- NOTE: If there's heavy need and usage, we caould create a view. Otherwise, the project cache is best, if it meets our needs.
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (FromJSON, ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Project
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "projects", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Project)

makeFieldLabelsNoPrefix ''Project

data ProjectCache = ProjectCache
  { -- We need this hosts to mirrow all the hosts in the endpoints table, and could use this for validation purposes to skip inserting endpoints just because of hosts
    -- if endpoint exists but host is not in this list, then we have a query specifically for inserting hosts.
    hosts :: Vector.Vector Text,
    -- maybe we don't need this? See the next point.
    endpointHashes :: Vector.Vector Text,
    -- Since shapes always have the endpoints hash prepended to them, maybe we don't need to store the hash of endpoints,
    -- since we can derive that from the shapes.
    shapeHashes :: Vector.Vector Text,
    -- We check if every request is part of the redact list, so it's better if we don't need to  hit the db for them with each request.
    -- Since we have a need to redact fields by endpoint, we can simply have the fields paths be prepended by the endpoint hash.
    -- [endpointHash]<>[field_category eg requestBody]<>[field_key_path]
    -- Those redace fields that don't have endpoint or field_category attached, would be aplied to every endpoint and field category.
    redactFieldslist :: Vector.Vector Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow)

makeFieldLabelsNoPrefix ''ProjectCache

data CreateProject = CreateProject
  { id :: ProjectId,
    title :: Text,
    description :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "projects", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CreateProject)

makeFieldLabelsNoPrefix ''CreateProject

insertProject :: CreateProject -> PgT.DBT IO ()
insertProject = insert @CreateProject

projectById :: ProjectId -> PgT.DBT IO (Maybe Project)
projectById = selectById @Project

selectProjectsForUser :: Users.UserId -> PgT.DBT IO (Vector.Vector Project)
selectProjectsForUser = query Select q
  where
    q = [sql| select pp.* from projects.projects as pp join projects.project_members as ppm on (pp.id=ppm.project_id) where ppm.user_id=? order by updated_at desc|]

selectProjectForUser :: (Users.UserId, ProjectId) -> PgT.DBT IO (Maybe Project)
selectProjectForUser = queryOne Select q
  where
    q = [sql| select pp.* from projects.projects as pp join projects.project_members as ppm on (pp.id=ppm.project_id) where ppm.user_id=? and ppm.project_id=? order by updated_at desc|]

editProjectGetH :: ProjectId -> PgT.DBT IO (Vector.Vector Project)
editProjectGetH pid = query Select q (Only pid)
  where
    q =
      [sql|
        SELECT pp*, ppm* FROM projects.projects AS pp 
            INNER JOIN projects.project_members AS ppm
            ON pp.id = pid 
        WHERE ppm.project_id = pp.id;|]

updateProject :: CreateProject -> PgT.DBT IO Int64
updateProject = PgT.execute q
  where
    q =
      [sql|
      UPDATE projects.projects(title, description) VALUES (?, ?);|]

deleteProject :: ProjectId -> PgT.DBT IO ()
deleteProject pid = delete @Project (Only pid)

data ProjectRequestStats = ProjectRequestStats
  { projectId :: ProjectId,
    min :: Double,
    p50 :: Double,
    p75 :: Double,
    p90 :: Double,
    p95 :: Double,
    p99 :: Double,
    max :: Double,
    totalTime :: Double,
    totalRequests :: Int,
    totalEndpoints :: Int,
    totalEndpointsLastWeek :: Int,
    totalShapes :: Int,
    totalShapesLastWeek :: Int,
    totalAnomalies :: Int,
    totalAnomaliesLastWeek :: Int,
    totalFields :: Int,
    totalFieldsLastWeek :: Int
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow, Default)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "project_request_stats", PrimaryKey "project_id", FieldModifiers '[CamelToSnake]] ProjectRequestStats)

projectRequestStatsByProject :: ProjectId -> PgT.DBT IO (Maybe ProjectRequestStats)
projectRequestStatsByProject = selectById @ProjectRequestStats
