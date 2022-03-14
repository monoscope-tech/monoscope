{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Formats
  ( Format (..),
    formatsByFieldId,
  )
where

import Data.Aeson qualified as AE
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity (selectManyByField)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Transact (DBT)
import Deriving.Aeson qualified as DAE
import Models.Apis.Fields qualified as Fields
import Optics.TH
import Relude

data Format = Format
  { id :: UUID.UUID,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    fieldId :: UUID.UUID,
    fieldType :: Fields.FieldTypes,
    fieldFormat :: Text,
    examples :: Vector.Vector Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Format
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "formats", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Format)
  deriving (FromField) via Aeson Format

makeFieldLabelsNoPrefix ''Format

formatsByFieldId :: Fields.FieldId -> DBT IO (Vector.Vector Format)
formatsByFieldId fid = selectManyByField @Format [field| field_id |] (Only fid)
