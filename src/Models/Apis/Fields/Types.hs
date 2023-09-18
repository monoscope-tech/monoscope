{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Fields.Types (
  Field (..),
  FieldTypes (..),
  FieldCategoryEnum (..),
  FieldId (..),
  SwField (..),
  fieldIdText,
  parseFieldCategoryEnum,
  parseFieldTypes,
  groupFieldsByCategory,
  fieldTypeToText,
  fieldCategoryEnumToText,
  textFieldTypeToText,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as AE
import Data.Default
import Data.List (groupBy)
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector as Vector (Vector, toList)
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ResultError (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Deriving.Aeson qualified as DAE
import Models.Projects.Projects qualified as Projects
import Optics.TH
import Relude
import Relude.Unsafe ((!!))
import Web.HttpApiData (FromHttpApiData)

-- $setup
-- >>> import Relude
-- >>> import Data.Default
-- >>> import Data.Vector hiding (fromList)
-- >>> import Data.Vector qualified as Vector

newtype FieldId = FieldId {unFieldId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, ToJSON, FromJSON, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID

fieldIdText :: FieldId -> Text
fieldIdText = UUID.toText . unFieldId

data FieldTypes
  = FTUnknown
  | FTString
  | FTNumber
  | FTBool
  | FTObject
  | FTList
  | FTNull
  deriving stock (Eq, Generic, Show)
  deriving
    (AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "FT", DAE.CamelToSnake]] FieldTypes

instance FromJSON FieldTypes where
  parseJSON (AE.String v) = maybe empty pure (parseFieldTypes v)
  parseJSON _ = empty

instance Default FieldTypes where
  def = FTUnknown

instance ToField FieldTypes where
  toField = Escape . encodeUtf8 <$> fieldTypeToText

fieldTypeToText :: FieldTypes -> Text
fieldTypeToText FTString = "string"
fieldTypeToText FTNumber = "number"
fieldTypeToText FTBool = "bool"
fieldTypeToText FTObject = "object"
fieldTypeToText FTList = "list"
fieldTypeToText FTNull = "null"
fieldTypeToText FTUnknown = "unknown"

textFieldTypeToText :: Text -> Text
textFieldTypeToText "FTString" = "string"
textFieldTypeToText "FTNumber" = "number"
textFieldTypeToText "FTBool" = "bool"
textFieldTypeToText "FTObject" = "object"
textFieldTypeToText "FTList" = "list"
textFieldTypeToText "FTNull" = "null"
textFieldTypeToText _ = "unknown"

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
  | FCPathParam
  | FCRequestHeader
  | FCResponseHeader
  | FCRequestBody
  | FCResponseBody
  deriving stock (Eq, Generic, Show, Ord)
  deriving
    (AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "FC", DAE.CamelToSnake]] FieldCategoryEnum

instance FromJSON FieldCategoryEnum where
  parseJSON (AE.String v) = maybe empty pure (parseFieldCategoryEnum v)
  parseJSON _ = empty

instance Default FieldCategoryEnum where
  def = FCQueryParam

instance ToField FieldCategoryEnum where
  toField = Escape . encodeUtf8 <$> fieldCategoryEnumToText

fieldCategoryEnumToText :: FieldCategoryEnum -> Text
fieldCategoryEnumToText FCQueryParam = "query_param"
fieldCategoryEnumToText FCPathParam = "path_param"
fieldCategoryEnumToText FCRequestHeader = "request_header"
fieldCategoryEnumToText FCResponseHeader = "response_header"
fieldCategoryEnumToText FCRequestBody = "request_body"
fieldCategoryEnumToText FCResponseBody = "response_body"

parseFieldCategoryEnum :: (Eq s, IsString s) => s -> Maybe FieldCategoryEnum
parseFieldCategoryEnum "query_param" = Just FCQueryParam
parseFieldCategoryEnum "path_param" = Just FCPathParam
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
  { id :: FieldId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , endpointHash :: Text
  , key :: Text
  , fieldType :: FieldTypes
  , fieldTypeOverride :: Maybe Text
  , format :: Text -- SHould fields be linked to the format table via the fieldFormat text or format Id?
  , formatOverride :: Maybe Text
  , description :: Text
  , keyPath :: Text
  , fieldCategory :: FieldCategoryEnum
  , hash :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, Default)
  deriving
    (AE.ToJSON, AE.FromJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Field
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "fields", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Field)
  deriving
    (FromField)
    via Aeson Field

data SwField = SwField
  { fEndpointHash :: Text
  , fKey :: Text
  , fFieldType :: FieldTypes
  , fFormat :: Text
  , fDescription :: Text
  , fKeyPath :: Text
  , fFieldCategory :: FieldCategoryEnum
  , fHash :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, Default)
  deriving (AE.ToJSON, AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SwField
  deriving (FromField) via Aeson SwField

instance Ord Field where
  (<=) f1 f2 =
    (projectId f1 <= projectId f2)
      && (endpointHash f1 <= endpointHash f2)
      && keyPath f1 <= keyPath f2

instance Eq Field where
  (==) f1 f2 =
    (projectId f1 == projectId f2)
      && (endpointHash f1 == endpointHash f2)
      && (keyPath f1 == keyPath f2)

makeFieldLabelsNoPrefix ''Field

-- | NB: The GroupBy function has been merged into the vectors package.
-- We should use that instead of converting to list, once that is available on hackage.
-- We use group by, only because the query returns the result sorted by the field_category.
-- >>> let qparam = (def::Field){fieldCategory=FCQueryParam}
-- >>> let respB = (def::Field){fieldCategory=FCResponseBody}
-- >>> let respB2 = (def::Field){fieldCategory=FCResponseBody, key="respBody2"}
-- >>> groupFieldsByCategory $ Vector.fromList [qparam, respB, respB2]
-- fromList [(FCQueryParam,[Field {id = FieldId {unFieldId = 00000000-0000-0000-0000-000000000000}, createdAt = 2019-08-31 05:14:37.537084021 UTC, updatedAt = 2019-08-31 05:14:37.537084021 UTC, projectId = ProjectId {unProjectId = 00000000-0000-0000-0000-000000000000}, endpointHash = "", key = "", fieldType = FTUnknown, fieldTypeOverride = Nothing, format = "", formatOverride = Nothing, description = "", keyPath = "", fieldCategory = FCQueryParam, hash = ""}]),(FCResponseBody,[Field {id = FieldId {unFieldId = 00000000-0000-0000-0000-000000000000}, createdAt = 2019-08-31 05:14:37.537084021 UTC, updatedAt = 2019-08-31 05:14:37.537084021 UTC, projectId = ProjectId {unProjectId = 00000000-0000-0000-0000-000000000000}, endpointHash = "", key = "", fieldType = FTUnknown, fieldTypeOverride = Nothing, format = "", formatOverride = Nothing, description = "", keyPath = "", fieldCategory = FCResponseBody, hash = ""},Field {id = FieldId {unFieldId = 00000000-0000-0000-0000-000000000000}, createdAt = 2019-08-31 05:14:37.537084021 UTC, updatedAt = 2019-08-31 05:14:37.537084021 UTC, projectId = ProjectId {unProjectId = 00000000-0000-0000-0000-000000000000}, endpointHash = "", key = "respBody2", fieldType = FTUnknown, fieldTypeOverride = Nothing, format = "", formatOverride = Nothing, description = "", keyPath = "", fieldCategory = FCResponseBody, hash = ""}])]
groupFieldsByCategory :: Vector Field -> Map FieldCategoryEnum [Field]
groupFieldsByCategory fields = Relude.fromList fieldGroupTupple
 where
  fields' = Vector.toList fields
  fieldGroup = groupBy (\f1 f2 -> f1.fieldCategory == f2.fieldCategory) fields'
  fieldGroupTupple = map (\f -> ((f !! 0).fieldCategory, f)) fieldGroup
