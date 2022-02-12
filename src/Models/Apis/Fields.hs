{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Fields
  ( Field (..),
    FieldTypes (..),
    FieldCategoryEnum (..),
    selectFields,
    upsertFields,
    parseFieldCategoryEnum,
    groupFieldsByCategory,
  )
where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AET
import Data.Default
import Data.Default.Instances
import Data.HashMap.Strict as HM (fromList)
import Data.List (groupBy)
import qualified Data.Map as Map
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime)
import Data.Time.Clock (DiffTime, NominalDiffTime)
import qualified Data.UUID as UUID
import Data.Vector as Vector (Vector, fromList, toList)
import Database.PostgreSQL.Entity (selectManyByField)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, query, queryOne, query_, withPool)
import Database.PostgreSQL.Entity.Internal.QQ (field)
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ResultError (..), ToRow, query_)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Transact as PgT (DBT, queryOne)
import qualified Deriving.Aeson as DAE
import GHC.Generics (Generic)
import qualified Models.Apis.Endpoints as Endpoints
import qualified Models.Projects.Projects as Projects
import Optics.Operators
import Optics.TH
import Relude
import Relude.Unsafe ((!!))
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

instance Default FieldTypes where
  def = FTUnknown

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
  = FCQueryParam
  | FCRequestHeader
  | FCResponseHeader
  | FCRequestBody
  | FCResponseBody
  deriving (Eq, Generic, Show, Ord)

instance Default FieldCategoryEnum where
  def = FCQueryParam

instance ToField FieldCategoryEnum where
  toField FCQueryParam = Escape "queryparam"
  toField FCRequestHeader = Escape "request_header"
  toField FCResponseHeader = Escape "response_header"
  toField FCRequestBody = Escape "request_body"
  toField FCResponseBody = Escape "response_body"

parseFieldCategoryEnum :: (Eq s, IsString s) => s -> Maybe FieldCategoryEnum
parseFieldCategoryEnum "queryparam" = Just FCQueryParam
parseFieldCategoryEnum "request_header" = Just FCRequestHeader
parseFieldCategoryEnum "response_header" = Just FCResponseHeader
parseFieldCategoryEnum "request_body" = Just FCRequestBody
parseFieldCategoryEnum "response_body" = Just FCResponseBody
parseFieldCategoryEnum _ = Nothing

instance FromField FieldCategoryEnum where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs ->
        case parseFieldCategoryEnum bs of
          Just a -> pure a
          Nothing -> returnError ConversionFailed f $ "Conversion error: Expected 'field_type' enum, got " <> decodeUtf8 bs <> " instead."

data Field = Field
  { createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    id :: UUID.UUID,
    projectId :: Projects.ProjectId,
    endpoint :: Endpoints.EndpointId,
    key :: Text,
    fieldType :: FieldTypes,
    fieldTypeOverride :: Maybe Text,
    format :: Text,
    formatOverride :: Maybe Text,
    description :: Text,
    keyPath :: Vector Text,
    keyPathStr :: Text,
    fieldCategory :: FieldCategoryEnum
  }
  deriving (Show, Generic, Default)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "fields", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Field)

instance Ord Field where
  (<=) f1 f2 =
    (projectId f1 <= projectId f2)
      && (endpoint f1 <= endpoint f2)
      && keyPathStr f1 <= keyPathStr f2

instance Eq Field where
  (==) f1 f2 =
    (projectId f1 == projectId f2)
      && (endpoint f1 == endpoint f2)
      && (keyPathStr f1 == keyPathStr f2)

makeFieldLabelsNoPrefix ''Field

-- | upsertFields
upsertFields :: UUID.UUID -> [(Field, [Text])] -> DBT IO [Maybe (Only Text)]
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

selectFields :: Endpoints.EndpointId -> DBT IO (Vector Field)
selectFields = query Select q
  where
    q = [sql| select * from apis.fields where endpoint=? order by field_category, key |]

-- | NB: The GroupBy function has been merged into the vectors package.
-- We should use that instead of converting to list, once that is available on hackage.
-- We use group by, only because the query returns the result sorted by the field_category.
-- >>> let qparam = (def::Field){fieldCategory=FCQueryParam}
-- >>> let respB = (def::Field){fieldCategory=FCResponseBody}
-- >>> let respB2 = (def::Field){fieldCategory=FCResponseBody, key="respBody2"}
-- >>> groupFieldsByCategory $ Vector.fromList [qparam, respB, respB2]
-- fromList [(FCQueryParam,[Field {createdAt = 2019-08-31 05:14:37.537084021 UTC, updatedAt = 2019-08-31 05:14:37.537084021 UTC, id = 00000000-0000-0000-0000-000000000000, projectId = 00000000-0000-0000-0000-000000000000, endpoint = 00000000-0000-0000-0000-000000000000, key = "", fieldType = FTUnknown, fieldTypeOverride = Nothing, format = "", formatOverride = Nothing, description = "", keyPath = [], keyPathStr = "", fieldCategory = FCQueryParam}]),(FCRequestBody,[Field {createdAt = 2019-08-31 05:14:37.537084021 UTC, updatedAt = 2019-08-31 05:14:37.537084021 UTC, id = 00000000-0000-0000-0000-000000000000, projectId = 00000000-0000-0000-0000-000000000000, endpoint = 00000000-0000-0000-0000-000000000000, key = "", fieldType = FTUnknown, fieldTypeOverride = Nothing, format = "", formatOverride = Nothing, description = "", keyPath = [], keyPathStr = "", fieldCategory = FCRequestBody}]),(FCResponseBody,[Field {createdAt = 2019-08-31 05:14:37.537084021 UTC, updatedAt = 2019-08-31 05:14:37.537084021 UTC, id = 00000000-0000-0000-0000-000000000000, projectId = 00000000-0000-0000-0000-000000000000, endpoint = 00000000-0000-0000-0000-000000000000, key = "", fieldType = FTUnknown, fieldTypeOverride = Nothing, format = "", formatOverride = Nothing, description = "", keyPath = [], keyPathStr = "", fieldCategory = FCResponseBody},Field {createdAt = 2019-08-31 05:14:37.537084021 UTC, updatedAt = 2019-08-31 05:14:37.537084021 UTC, id = 00000000-0000-0000-0000-000000000000, projectId = 00000000-0000-0000-0000-000000000000, endpoint = 00000000-0000-0000-0000-000000000000, key = "respBody2", fieldType = FTUnknown, fieldTypeOverride = Nothing, format = "", formatOverride = Nothing, description = "", keyPath = [], keyPathStr = "", fieldCategory = FCResponseBody}])]
groupFieldsByCategory :: Vector Field -> Map FieldCategoryEnum [Field]
groupFieldsByCategory fields = Relude.fromList fieldGroupTupple
  where
    fields' = Vector.toList fields
    fieldGroup = groupBy (\f1 f2 -> f1 ^. #fieldCategory == f2 ^. #fieldCategory) fields'
    fieldGroupTupple = map (\f -> ((f !! 0) ^. #fieldCategory, f)) fieldGroup
