module Models.Apis.Formats (
  Format (..),
  FormatId (..),
  SwFormat (..),
  formatsByFieldHash,
  formatsByFieldsHashes,
  bulkInsertFormat,
  formatsByHash,
) where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V 
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact (DBT, executeMany)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Models.Apis.Fields.Types qualified as Fields
import Models.Projects.Projects qualified as Projects
import Relude
import Servant (FromHttpApiData)


newtype FormatId = FormatId {unFormatId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, FromField, ToField, FromHttpApiData, Default, AE.FromJSON, NFData, AE.ToJSON)


data Format = Format
  { id :: FormatId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , fieldHash :: Text
  , fieldType :: Fields.FieldTypes
  , fieldFormat :: Text
  , examples :: V.Vector AE.Value
  , hash :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Format
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "formats", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Format)
  deriving (FromField) via Aeson Format


formatsByFieldHash :: Text -> DBT IO (V.Vector Format)
formatsByFieldHash fhash = query Select q (Only fhash)
  where
    q = [sql| SELECT id,created_at,updated_at,project_id, field_hash,field_type,field_format,examples::json[], hash from apis.formats where field_hash=? |]


formatsByHash :: Text -> DBT IO (V.Vector Format)
formatsByHash fhash = query Select q (Only fhash)
  where
    q = [sql| SELECT id,created_at,updated_at,project_id, field_hash,field_type,field_format,examples::json[], hash from apis.formats where hash=? |]


bulkInsertFormat :: DB :> es => V.Vector Format -> Eff es ()
bulkInsertFormat formats = void $ dbtToEff $ executeMany q $ V.toList rowsToInsert
  where
    q =
      [sql| 
      insert into apis.formats (project_id, field_hash, field_type, field_format, examples, hash) 
        VALUES (?, ?, ?, ?, ?, ?) 
        ON CONFLICT (hash)
        DO
          UPDATE SET field_type= EXCLUDED.field_type, examples = ARRAY(SELECT DISTINCT e from unnest(apis.formats.examples || excluded.examples) as e order by e limit 20);
      |]
    rowsToInsert =
      formats <&> \format ->
        ( format.projectId
        , format.fieldHash
        , format.fieldType
        , format.fieldFormat
        , format.examples
        , format.hash
        )


data SwFormat = SwFormat
  { swFieldHash :: Text
  , swFieldType :: Fields.FieldTypes
  , swFieldFormat :: Text
  , swExamples :: V.Vector AE.Value
  , swHash :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SwFormat
  deriving (FromField) via Aeson SwFormat
  deriving anyclass (AE.ToJSON)


formatsByFieldsHashes :: Projects.ProjectId -> V.Vector Text -> PgT.DBT IO (V.Vector SwFormat)
formatsByFieldsHashes pid fieldHashes = query Select q (pid, fieldHashes)
  where
    q =
      [sql|
          SELECT field_hash sw_field_hash,field_type sw_field_type, field_format sw_field_format, examples::json[] sw_examples, hash sw_hash
          FROM apis.formats
          WHERE project_id = ? AND  field_hash = ANY(?)
        |]
