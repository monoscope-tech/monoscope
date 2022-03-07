{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Formats
  ( Format (..),
    formatsByFieldId,
  )
where

import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity (selectManyByField)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Transact (DBT)
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
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "formats", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Format)

makeFieldLabelsNoPrefix ''Format

formatsByFieldId :: Fields.FieldId -> DBT IO (Vector.Vector Format)
formatsByFieldId = selectManyByField @Format [field| field_id |]
