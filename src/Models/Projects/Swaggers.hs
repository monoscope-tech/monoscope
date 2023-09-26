{-# LANGUAGE DuplicateRecordFields #-}

module Models.Projects.Swaggers (Swagger (..), SwaggerId (..), addSwagger, swaggersByProject, updateSwagger, getSwaggerById) where

import Data.Aeson as Aeson
import Data.Default (Default)
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (Entity, insert, selectById, selectManyByField)
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Entity.Internal.QQ (field)
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
  deriving
    (Eq, Ord, ToJSON, FromJSON, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID

instance HasField "toText" SwaggerId Text where
  getField = UUID.toText . swaggerId

data Swagger = Swagger
  { id :: SwaggerId
  , projectId :: Projects.ProjectId
  , createdBy :: Users.UserId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , swaggerJson :: Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "swagger_jsons", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Swagger)

addSwagger :: Swagger -> DBT IO ()
addSwagger = insert @Swagger

getSwaggerById :: Text -> DBT IO (Maybe Swagger)
getSwaggerById id' = selectById (Only id')

swaggersByProject :: Projects.ProjectId -> DBT IO (Vector Swagger)
swaggersByProject pid = selectManyByField [field| project_id |] pid

updateSwagger :: Text -> Value -> DBT IO Int64
updateSwagger swaggerId swaggerJson = do
  execute Update q (swaggerJson, swaggerId)
  where
    q = [sql| UPDATE apis.swagger_jsons SET swagger_json=? WHERE id=? |]
