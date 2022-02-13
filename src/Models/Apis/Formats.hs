{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Formats
  ( Format (..),
    formatsByFieldId,
  )
where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AET
import Data.Default
import Data.Default.Instances
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime)
import Data.Time.Clock (DiffTime, NominalDiffTime)
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity (Entity (fields, tableName), insert, selectManyByField)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, queryOne, query_, withPool)
import Database.PostgreSQL.Entity.Internal.QQ (field)
import qualified Database.PostgreSQL.Entity.Types as PET
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow, query_)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import qualified Database.PostgreSQL.Transact as PgT
import qualified Deriving.Aeson as DAE
import GHC.Generics (Generic)
import qualified Models.Apis.Fields as Fields
import Optics.Operators
import Optics.TH
import Relude
import qualified Relude.Unsafe as Unsafe

data Format = Format
  { createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    id :: UUID.UUID,
    fieldId :: UUID.UUID,
    fieldType :: Fields.FieldTypes,
    fieldFormat :: Text,
    examples :: Vector.Vector Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "apis", PET.TableName "formats", PET.PrimaryKey "id", PET.FieldModifiers '[PET.CamelToSnake]] Format)

makeFieldLabelsNoPrefix ''Format

formatsByFieldId :: Fields.FieldId -> DBT IO (Vector.Vector Format)
formatsByFieldId = selectManyByField @Format [field| field_id |]
