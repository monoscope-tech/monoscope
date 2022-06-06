{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Projects.RedactedFields (RedactedField (..), RedactedFieldId (..), ConfiguredVia (..), redactField, redactedFieldById, redactedFieldsByProject) where

import Data.Aeson
import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (Entity, insert, selectById, selectManyByField)
import Database.PostgreSQL.Entity.Internal.QQ (field)
import Database.PostgreSQL.Entity.Types (CamelToSnake, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes (Aeson)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact (DBT)
import Deriving.Aeson qualified as DAE
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Relude
import Relude.Unsafe (read)
import RequestMessages qualified
import Servant (FromHttpApiData)

newtype RedactedFieldId = RedactedFieldId {unRedactedFieldId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, ToJSON, FromJSON, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID

redactedFieldIdText :: RedactedFieldId -> Text
redactedFieldIdText = UUID.toText . unRedactedFieldId

data ConfiguredVia
  = Dashboard
  | SDK RequestMessages.SDKTypes
  deriving stock (Generic, Show, Read)
  deriving (FromJSON, ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] ConfiguredVia

instance FromField ConfiguredVia where
  fromField f dat = read <$> fromField f dat

instance ToField ConfiguredVia where
  toField = Escape . show

data RedactedField = RedactedField
  { id :: RedactedFieldId,
    projectId :: Projects.ProjectId,
    path :: Text,
    configuredVia :: ConfiguredVia,
    description :: Text,
    endpoint :: Maybe Endpoints.EndpointId
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "redacted_fields", PrimaryKey "id", FieldModifiers '[CamelToSnake]] RedactedField)

redactField :: RedactedField -> DBT IO ()
redactField = insert @RedactedField

redactedFieldById :: RedactedFieldId -> DBT IO (Maybe RedactedField)
redactedFieldById id' = selectById (Only id')

redactedFieldsByProject :: Projects.ProjectId -> DBT IO (Vector RedactedField)
redactedFieldsByProject pid = selectManyByField [field| project_id |] pid
