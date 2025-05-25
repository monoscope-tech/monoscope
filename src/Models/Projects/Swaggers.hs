module Models.Projects.Swaggers (Swagger (..), SwaggerId (..), addSwagger, swaggersByProject, updateSwagger, getSwaggerById) where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (Entity, insert, selectById)
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Entity.Types (CamelToSnake, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple hiding (execute, query)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact (DBT)
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import Relude
import Servant (FromHttpApiData)


newtype SwaggerId = SwaggerId {swaggerId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, AE.ToJSON, AE.FromJSON, FromField, ToField, FromHttpApiData, Default, NFData)


instance HasField "toText" SwaggerId Text where
  getField = UUID.toText . swaggerId


data Swagger = Swagger
  { id :: SwaggerId
  , projectId :: Projects.ProjectId
  , createdBy :: Users.UserId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , swaggerJson :: AE.Value
  , host :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "swagger_jsons", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Swagger)


addSwagger :: Swagger -> DBT IO ()
addSwagger = insert @Swagger


getSwaggerById :: Text -> DBT IO (Maybe Swagger)
getSwaggerById id' = selectById (Only id')


swaggersByProject :: Projects.ProjectId -> Text -> DBT IO (V.Vector Swagger)
swaggersByProject pid host = query q (pid, host)
  where
    q = [sql| select id, project_id, created_by, created_at, updated_at, swagger_json, host from apis.swagger_jsons where project_id=? AND host=? order by created_at desc|]


updateSwagger :: Text -> AE.Value -> DBT IO Int64
updateSwagger swaggerId swaggerJson = do
  execute q (swaggerJson, swaggerId)
  where
    q = [sql| UPDATE apis.swagger_jsons SET swagger_json=? WHERE id=? |]
