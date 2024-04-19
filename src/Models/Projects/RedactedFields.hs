{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Models.Projects.RedactedFields (RedactedField (..), RedactedFieldId (..), ConfiguredVia (..), redactField, redactedFieldById, redactedFieldsByProject) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (Entity, insert, selectById, selectManyByField)
import Database.PostgreSQL.Entity.Internal.QQ (field)
import Database.PostgreSQL.Entity.Types (CamelToSnake, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (..))
import Database.PostgreSQL.Transact (DBT)
import Deriving.Aeson qualified as DAE
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Relude
import Relude.Unsafe (read)
import Servant (FromHttpApiData)


newtype RedactedFieldId = RedactedFieldId {unRedactedFieldId :: UUID.UUID}
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON, FromField, ToField, FromHttpApiData, Default, NFData)


data ConfiguredVia
  = Dashboard
  | SDK RequestDumps.SDKTypes
  deriving stock (Generic, Show, Read)
  deriving anyclass (NFData)
  deriving (FromJSON, ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] ConfiguredVia


instance FromField ConfiguredVia where
  fromField f dat = read <$> fromField f dat


instance ToField ConfiguredVia where
  toField = Escape . show


-- FIXME: implement insert redacted fields to also store the hash of the endpoints.
data RedactedField = RedactedField
  { id :: RedactedFieldId
  , projectId :: Projects.ProjectId
  , path :: Text
  , configuredVia :: ConfiguredVia
  , description :: Text
  , endpointHash :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "redacted_fields", PrimaryKey "id", FieldModifiers '[CamelToSnake]] RedactedField)


redactField :: RedactedField -> DBT IO ()
redactField = insert @RedactedField


redactedFieldById :: RedactedFieldId -> DBT IO (Maybe RedactedField)
redactedFieldById id' = selectById (Only id')


redactedFieldsByProject :: Projects.ProjectId -> DBT IO (V.Vector RedactedField)
redactedFieldsByProject pid = selectManyByField [field| project_id |] pid
