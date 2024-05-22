{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Models.Apis.Formats (
  Format (..),
  FormatId (..),
  SwFormat (..),
  formatsByFieldHash,
  formatsByFieldsHashes,
  insertFormatQueryAndParams,
  insertFormats,
) where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), Query, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact (DBT, executeMany)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import Models.Apis.Fields.Types qualified as Fields
import Models.Projects.Projects qualified as Projects
import Relude
import Servant (FromHttpApiData)
import Utils (DBField (MkDBField))


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
  , examples :: Vector.Vector AE.Value
  , hash :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Format
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "formats", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Format)
  deriving (FromField) via Aeson Format


formatsByFieldHash :: Text -> DBT IO (Vector.Vector Format)
formatsByFieldHash fhash = query Select q (Only fhash)
  where
    q = [sql| SELECT id,created_at,updated_at,project_id, field_hash,field_type,field_format,examples::json[], hash from apis.formats where field_hash=? |]


-- TODO: explore using postgres values to handle bulking loading multiple fields and formats into the same insert query.
insertFormatQueryAndParams :: Format -> (Query, [DBField])
insertFormatQueryAndParams format = (q, params)
  where
    q =
      [sql| 
      insert into apis.formats (project_id, field_hash, field_type, field_format, examples, hash) VALUES (?,?,?,?,?,?)
        ON CONFLICT (project_id, field_hash, field_format)
        DO
          UPDATE SET 
            examples = ARRAY(SELECT DISTINCT e from unnest(apis.formats.examples || excluded.examples) as e order by e limit ?); 
      |]
    params =
      [ MkDBField format.projectId
      , MkDBField format.fieldHash
      , MkDBField format.fieldType
      , MkDBField format.fieldFormat
      , MkDBField format.examples
      , MkDBField format.hash
      , MkDBField (20 :: Int64) -- NOTE: max number of examples
      ]


insertFormats :: [Format] -> DBT IO Int64
insertFormats formats = do
  let q =
        [sql| 
      insert into apis.formats (project_id, field_hash, field_type, field_format, examples, hash) VALUES (?,?,?,?,?,?)
        ON CONFLICT (hash)
        DO
          UPDATE SET 
            field_type= EXCLUDED.field_type, examples = ARRAY(SELECT DISTINCT e from unnest(apis.formats.examples || excluded.examples) as e order by e limit 20); 
      |]
  let params = map getFormatParams formats
  executeMany q params


getFormatParams :: Format -> (Projects.ProjectId, Text, Fields.FieldTypes, Text, Vector AE.Value, Text)
getFormatParams format =
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
  , swExamples :: Vector.Vector AE.Value
  , swHash :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SwFormat
  deriving (FromField) via Aeson SwFormat
  deriving anyclass (AE.ToJSON)


formatsByFieldsHashes :: Projects.ProjectId -> Vector Text -> PgT.DBT IO (Vector SwFormat)
formatsByFieldsHashes pid fieldHashes = query Select q (pid, fieldHashes)
  where
    q =
      [sql|
          SELECT field_hash sw_field_hash,field_type sw_field_type, field_format sw_field_format, examples::json[] sw_examples, hash sw_hash
          FROM apis.formats
          WHERE project_id = ? AND  field_hash = ANY(?)
        |]
