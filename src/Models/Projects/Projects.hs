{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Projects.Projects (
  Project (..),
  Project' (..),
  ProjectId (..),
  CreateProject (..),
  ProjectRequestStats (..),
  insertProject,
  projectIdFromText,
  selectProjectsForUser,
  projectRequestStatsByProject,
  selectProjectForUser,
  updateProject,
  deleteProject,
  projectById,
  projectCacheById,
  ProjectCache (..),
) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Default
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact (DBT)
import Deriving.Aeson qualified as DAE
import GHC.Records (HasField (getField))
import Models.Users.Users qualified as Users
import Optics.TH
import Relude
import Web.HttpApiData

newtype ProjectId = ProjectId {unProjectId :: UUID.UUID}
  deriving stock (Generic, Show, Read)
  deriving
    (Eq, Ord, ToJSON, FromJSON, FromField, ToField, FromHttpApiData, Default, Hashable)
    via UUID.UUID
  deriving anyclass (FromRow, ToRow)

instance HasField "unwrap" ProjectId UUID.UUID where
  getField = coerce

instance HasField "toText" ProjectId Text where
  getField = UUID.toText . unProjectId

projectIdFromText :: Text -> Maybe ProjectId
projectIdFromText pid = ProjectId <$> UUID.fromText pid

data Project = Project
  { id :: ProjectId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , deletedAt :: Maybe ZonedTime
  , active :: Bool
  , title :: Text
  , description :: Text
  , -- NOTE: We used to have hosts under project, but now hosts should be gotten from the endpoints.
    -- NOTE: If there's heavy need and usage, we caould create a view. Otherwise, the project cache is best, if it meets our needs.
    paymentPlan :: Text
  , questions :: Maybe Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow)
  deriving
    (FromJSON, ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Project
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "projects", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Project)

makeFieldLabelsNoPrefix ''Project

data Project' = Project'
  { id :: ProjectId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , deletedAt :: Maybe ZonedTime
  , active :: Bool
  , title :: Text
  , description :: Text
  , -- NOTE: We used to have hosts under project, but now hosts should be gotten from the endpoints.
    -- NOTE: If there's heavy need and usage, we caould create a view. Otherwise, the project cache is best, if it meets our needs.
    paymentPlan :: Text
  , questions :: Maybe Value
  , usersDisplayImages :: Vector Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow)

data ProjectCache = ProjectCache
  { -- We need this hosts to mirror all the hosts in the endpoints table, and could use this for validation purposes to skip inserting endpoints just because of hosts
    -- if endpoint exists but host is not in this list, then we have a query specifically for inserting hosts.
    hosts :: V.Vector Text
  , -- maybe we don't need this? See the next point.
    endpointHashes :: V.Vector Text
  , -- Since shapes always have the endpoints hash prepended to them, maybe we don't need to store the hash of endpoints,
    -- since we can derive that from the shapes.
    shapeHashes :: V.Vector Text
  , -- We check if every request is part of the redact list, so it's better if we don't need to  hit the db for them with each request.
    -- Since we have a need to redact fields by endpoint, we can simply have the fields paths be prepended by the endpoint hash.
    -- [endpointHash]<>[field_category eg requestBody]<>[field_key_path]
    -- Those redact fields that don't have endpoint or field_category attached, would be aplied to every endpoint and field category.
    redactFieldslist :: V.Vector Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow)

makeFieldLabelsNoPrefix ''ProjectCache

data CreateProject = CreateProject
  { id :: ProjectId
  , title :: Text
  , description :: Text
  , paymentPlan :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "projects", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CreateProject)

makeFieldLabelsNoPrefix ''CreateProject

-- FIXME: We currently return an object with empty vectors when nothing was found.
projectCacheById :: ProjectId -> DBT IO (Maybe ProjectCache)
projectCacheById = queryOne Select q
 where
  q =
    [sql| select  coalesce(ARRAY_AGG(DISTINCT hosts ORDER BY hosts ASC),'{}') hosts, 
                    coalesce(ARRAY_AGG(DISTINCT endpoint_hashes ORDER BY endpoint_hashes ASC),'{}') endpoint_hashes, 
                    coalesce(ARRAY_AGG(DISTINCT shape_hashes ORDER BY shape_hashes ASC),'{}') shape_hashes, 
                    coalesce(ARRAY_AGG(DISTINCT paths ORDER BY paths ASC),'{}') redacted_fields 
            from
              (select unnest(akeys(hosts)) hosts, e.hash endpoint_hashes, sh.hash shape_hashes, concat(rf.endpoint_hash,'<>', rf.field_category,'<>', rf.path) paths
                from apis.endpoints e
                left join apis.shapes sh on sh.endpoint_hash = e.hash
                left join projects.redacted_fields rf on rf.project_id = e.project_id
                where e.project_id = ?
               ) enp; |]

insertProject :: CreateProject -> DBT IO ()
insertProject = insert @CreateProject

projectById :: ProjectId -> DBT IO (Maybe Project)
projectById = queryOne Select q
 where
  q = [sql| select p.* from projects.projects p where id=?|]

selectProjectsForUser :: Users.UserId -> DBT IO (V.Vector Project')
selectProjectsForUser = query Select q
 where
  q =
    [sql| select pp.*,  ARRAY_AGG(us.display_image_url) OVER (PARTITION BY pp.id) from projects.projects as pp 
                join projects.project_members as ppm on (pp.id=ppm.project_id) 
                join users.users as us on (us.id=ppm.user_id)
                where ppm.user_id=? and pp.deleted_at IS NULL order by updated_at desc|]

selectProjectForUser :: (Users.UserId, ProjectId) -> DBT IO (Maybe Project)
selectProjectForUser = queryOne Select q
 where
  q =
    [sql| 
        select pp.* from projects.projects as pp 
          join projects.project_members as ppm on (pp.id=ppm.project_id)
          join users.users uu on (uu.id=ppm.user_id OR uu.is_sudo is True)
          where (ppm.user_id=? or uu.is_sudo is True) and ppm.project_id=? and pp.deleted_at IS NULL order by updated_at desc
          limit 1
      |]

editProjectGetH :: ProjectId -> DBT IO (V.Vector Project)
editProjectGetH pid = query Select q (Only pid)
 where
  q =
    [sql|
        SELECT pp*, ppm* FROM projects.projects AS pp 
            INNER JOIN projects.project_members AS ppm
            ON pp.id = pid 
        WHERE ppm.project_id = pp.id;|]

updateProject :: CreateProject -> DBT IO Int64
updateProject cp = do
  execute Update q (cp.title, cp.description, cp.paymentPlan, cp.id)
 where
  q =
    [sql| UPDATE projects.projects SET title=?, description=?, payment_plan=? where id=?;|]

deleteProject :: ProjectId -> DBT IO Int64
deleteProject pid = do
  execute Update q pid
 where
  q =
    [sql| UPDATE projects.projects SET deleted_at=NOW() where id=?;|]

data ProjectRequestStats = ProjectRequestStats
  { projectId :: ProjectId
  , min :: Double
  , p50 :: Double
  , p75 :: Double
  , p90 :: Double
  , p95 :: Double
  , p99 :: Double
  , max :: Double
  , totalTime :: Double
  , totalRequests :: Int
  , totalEndpoints :: Int
  , totalEndpointsLastWeek :: Int
  , totalShapes :: Int
  , totalShapesLastWeek :: Int
  , totalAnomalies :: Int
  , totalAnomaliesLastWeek :: Int
  , totalFields :: Int
  , totalFieldsLastWeek :: Int
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow, Default)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "project_request_stats", PrimaryKey "project_id", FieldModifiers '[CamelToSnake]] ProjectRequestStats)

projectRequestStatsByProject :: ProjectId -> DBT IO (Maybe ProjectRequestStats)
projectRequestStatsByProject = selectById @ProjectRequestStats
