{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Endpoints
  ( Endpoint (..),
    EndpointId (..),
    upsertEndpoints,
    endpointsByProject,
    endpointById,
    endpointIdText,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Default.Instances ()
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity (selectById)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import Models.Projects.Projects qualified as Projects
import Optics.Operators ((^.))
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude
import Web.HttpApiData (FromHttpApiData)

-- Added only for satisfying the tests
instance Eq ZonedTime where
  (==) _ _ = True

newtype EndpointId = EndpointId {unEndpointId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (ToJSON, FromJSON, Eq, Ord, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID
  deriving anyclass (FromRow, ToRow)

endpointIdText :: EndpointId -> Text
endpointIdText = UUID.toText . unEndpointId

-- TODO: Introduce request header hashes and response header hashes
data Endpoint = Endpoint
  { id :: EndpointId,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    projectId :: Projects.ProjectId,
    urlPath :: Text,
    urlParams :: AE.Value,
    method :: Text,
    hosts :: Vector.Vector Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow, Default)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Endpoint
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "endpoints", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Endpoint)
  deriving (FromField) via Aeson Endpoint

makeFieldLabelsNoPrefix ''Endpoint

upsertEndpoints :: Endpoint -> PgT.DBT IO (Maybe EndpointId)
upsertEndpoints endpoint = queryOne Insert q options
  where
    q =
      [sql|  
        with e as (
          INSERT INTO apis.endpoints (project_id, url_path, url_params, method, hosts)
          VALUES(?, ?, ?, ?, ?) 
          ON CONFLICT (project_id, url_path, method) 
          DO 
             UPDATE SET 
              hosts = ARRAY(SELECT DISTINCT e from unnest(apis.endpoints.hosts, excluded.hosts) as e order by e)
          RETURNING id 
        )
        SELECT * from e 
        UNION 
          SELECT id FROM apis.endpoints WHERE project_id=? AND url_path=? AND method=?;
      |]
    options =
      ( endpoint ^. #projectId,
        endpoint ^. #urlPath,
        endpoint ^. #urlParams,
        endpoint ^. #method,
        endpoint ^. #hosts,
        endpoint ^. #projectId,
        endpoint ^. #urlPath,
        endpoint ^. #method
      )

endpointsByProject :: Projects.ProjectId -> PgT.DBT IO (Vector.Vector Endpoint)
endpointsByProject = query Select q
  where
    q = [sql|select * from apis.endpoints where project_id=?|]

-- It appears the selectManyByField leaks memory
-- endpointsByProject pid = selectManyByField @Endpoint [field| project_id |] pid

endpointById :: EndpointId -> PgT.DBT IO (Maybe Endpoint)
endpointById = selectById @Endpoint
