module Models.Apis.Formats (
  Format (..),
  FormatId,
  SwFormat (..),
  bulkInsertFormat,
) where

import Data.Aeson qualified as AE
import Data.Time (ZonedTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (executeMany)
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Models.Apis.Fields.Types qualified as Fields
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Relude


type FormatId = UUIDId "format"


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
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "formats", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Format)
  deriving (FromField) via Aeson Format
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Format


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
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving anyclass (AE.ToJSON)
  deriving (FromField) via Aeson SwFormat
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SwFormat
