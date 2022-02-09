{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Fields
  ( Field (..),
    FieldTypes (..),
    upsertFields,
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
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, queryOne, query_, withPool)
import qualified Database.PostgreSQL.Entity.Types as PET
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ResultError (..), ToRow, query_)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import qualified Database.PostgreSQL.Transact as PgT
import qualified Deriving.Aeson as DAE
import GHC.Generics (Generic)
import Optics.Operators
import Optics.TH
import Relude
import qualified Relude.Unsafe as Unsafe

data FieldTypes
  = FTUnknown
  | FTString
  | FTNumber
  | FTBool
  | FTObject
  | FTList
  | FTNull
  deriving (Eq, Generic, Show)

instance ToField FieldTypes where
  toField FTString = Escape "string"
  toField FTNumber = Escape "number"
  toField FTBool = Escape "bool"
  toField FTObject = Escape "object"
  toField FTList = Escape "list"
  toField FTNull = Escape "null"
  toField FTUnknown = Escape "unknown"

parseFieldTypes :: (Eq s, IsString s) => s -> Maybe FieldTypes
parseFieldTypes "string" = Just FTString
parseFieldTypes "number" = Just FTNumber
parseFieldTypes "bool" = Just FTBool
parseFieldTypes "object" = Just FTObject
parseFieldTypes "list" = Just FTList
parseFieldTypes "null" = Just FTNull
parseFieldTypes _ = Nothing

instance FromField FieldTypes where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs ->
        case parseFieldTypes bs of
          Just a -> pure a
          Nothing -> returnError ConversionFailed f $ "Conversion error: Expected 'field_type' enum, got " <> decodeUtf8 bs <> " instead."

data FieldCategoryEnum
  = FCQueryParams
  | FCRequestHeaders
  | FCResponseHeaders
  | FCRequestBody
  | FCResponseBody
  deriving (Eq, Generic, Show)

data Field = Field
  { createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    id :: UUID.UUID,
    projectId :: UUID.UUID,
    endpoint :: UUID.UUID,
    key :: Text,
    fieldType :: FieldTypes,
    fieldTypeOverride :: Maybe Text,
    format :: Text,
    formatOverride :: Maybe Text,
    description :: Text,
    keyPath :: Vector.Vector Text,
    keyPathStr :: Text,
    fieldCategory :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "apis", PET.TableName "fields", PET.PrimaryKey "id", PET.FieldModifiers '[PET.CamelToSnake]] Field)

makeFieldLabelsNoPrefix ''Field

-- | upsertFields
upsertFields :: UUID.UUID -> [(Field, [Text])] -> PgT.DBT IO [Maybe (Only Text)]
upsertFields endpointID fields = options & mapM (PgT.queryOne q)
  where
    q =
      [sql|
        select apis.create_field_and_formats(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)::text;
   |]
    options =
      fields
        & map
          ( \(field, examples) ->
              ( field ^. #projectId,
                endpointID,
                field ^. #key,
                field ^. #fieldType,
                field ^. #fieldTypeOverride,
                field ^. #format,
                field ^. #formatOverride,
                field ^. #description,
                field ^. #keyPath,
                field ^. #keyPathStr,
                field ^. #fieldCategory,
                Vector.fromList examples,
                5 :: Int64 -- max examples size
              )
          )
