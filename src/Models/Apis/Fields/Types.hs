module Models.Apis.Fields.Types (
  Field (..),
  FieldTypes (..),
  FieldCategoryEnum (..),
  FieldId (..),
  SwField (..),
  FacetSummary (..),
  FacetValue (..),
  FacetData (..),
  fieldIdText,
  parseFieldCategoryEnum,
  parseFieldTypes,
  groupFieldsByCategory,
  fieldTypeToText,
  fieldCategoryEnumToText,
  fieldsToNormalized,
  textFieldTypeToText,
)
where

import Data.Aeson qualified as AE
import Data.Default
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ResultError (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Deriving.Aeson qualified as DAE
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Pkg.DBUtils (WrappedEnumSC (..))
import Relude
import Relude.Unsafe ((!!))
import Web.HttpApiData (FromHttpApiData)


-- $setup
-- >>> import Relude
-- >>> import Data.Default
-- >>> import Data.Vector hiding (fromList)
-- >>> import Data.Vector qualified as V


newtype FieldId = FieldId {unFieldId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (NFData)
  deriving
    (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, Ord, ToField)
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
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via WrappedEnumSC "FT" FieldTypes


instance AE.FromJSON FieldTypes where
  parseJSON (AE.String v) = maybe empty pure (parseFieldTypes v)
  parseJSON _ = empty


instance AE.ToJSON FieldTypes where
  toJSON = AE.String . fieldTypeToText


instance Default FieldTypes where
  def = FTUnknown


instance HasField "toText" FieldTypes Text where
  getField = fieldTypeToText


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


data FieldCategoryEnum
  = FCQueryParam
  | FCPathParam
  | FCRequestHeader
  | FCResponseHeader
  | FCRequestBody
  | FCResponseBody
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)


instance AE.FromJSON FieldCategoryEnum where
  parseJSON (AE.String v) = maybe empty pure (parseFieldCategoryEnum v)
  parseJSON _ = empty


instance AE.ToJSON FieldCategoryEnum where
  toJSON = AE.String . fieldCategoryEnumToText


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


fieldsToNormalized :: [Field] -> [(Text, Maybe Field)]
fieldsToNormalized =
  sortNub . concatMap \field ->
    map
      ((,Nothing) . fst)
      ( field.keyPath
          & T.dropWhile (== '.')
          & T.breakOnAll "."
      )
      & (++ [(T.dropWhile (== '.') field.keyPath, Just field)])


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
  , isEnum :: Bool
  , isRequired :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "fields", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Field)
  deriving
    (FromField)
    via Aeson Field
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Field


data SwField = SwField
  { fEndpointHash :: Text
  , fKey :: Text
  , fFieldType :: FieldTypes
  , fFormat :: Text
  , fDescription :: Text
  , fKeyPath :: Text
  , fFieldCategory :: FieldCategoryEnum
  , fHash :: Text
  , fIsEnum :: Bool
  , fIsRequired :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (FromField) via Aeson SwField
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SwField


data FacetValue = FacetValue
  { value :: Text
  , count :: Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] FacetValue


-- | Map of field names to arrays of facet values
newtype FacetData = FacetData (HM.HashMap Text [FacetValue])
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData)
  deriving (FromField, ToField) via Aeson FacetData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] FacetData


data FacetSummary = FacetSummary
  { id :: UUID.UUID
  , projectId :: Text
  , tableName :: Text
  , facetJson :: FacetData
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "facet_summaries", PrimaryKey "id", FieldModifiers '[CamelToSnake]] FacetSummary)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] FacetSummary


instance Ord Field where
  (<=) f1 f2 =
    (f1.projectId <= f2.projectId)
      && (f1.endpointHash <= f2.endpointHash)
      && (f1.keyPath <= f2.keyPath)


instance Eq Field where
  (==) f1 f2 =
    (f1.projectId == f2.projectId)
      && (f1.endpointHash == f2.endpointHash)
      && (f1.keyPath == f2.keyPath)


-- | NB: The GroupBy function has been merged into the vectors package.
-- We should use that instead of converting to list, once that is available on hackage.
-- We use group by, only because the query returns the result sorted by the field_category.
-- >>> let qparam = (def::Field){fieldCategory=FCQueryParam}
-- >>> let respB = (def::Field){fieldCategory=FCResponseBody}
-- >>> let respB2 = (def::Field){fieldCategory=FCResponseBody, key="respBody2"}
-- >>> groupFieldsByCategory $ V.fromList [qparam, respB, respB2]
-- fromList [(FCQueryParam,[Field {id = FieldId {unFieldId = 00000000-0000-0000-0000-000000000000}, createdAt = 2019-08-31 05:14:37.537084021 UTC, updatedAt = 2019-08-31 05:14:37.537084021 UTC, projectId = ProjectId {unProjectId = 00000000-0000-0000-0000-000000000000}, endpointHash = "", key = "", fieldType = FTUnknown, fieldTypeOverride = Nothing, format = "", formatOverride = Nothing, description = "", keyPath = "", fieldCategory = FCQueryParam, hash = "", isEnum = False, isRequired = False}]),(FCResponseBody,[Field {id = FieldId {unFieldId = 00000000-0000-0000-0000-000000000000}, createdAt = 2019-08-31 05:14:37.537084021 UTC, updatedAt = 2019-08-31 05:14:37.537084021 UTC, projectId = ProjectId {unProjectId = 00000000-0000-0000-0000-000000000000}, endpointHash = "", key = "", fieldType = FTUnknown, fieldTypeOverride = Nothing, format = "", formatOverride = Nothing, description = "", keyPath = "", fieldCategory = FCResponseBody, hash = "", isEnum = False, isRequired = False},Field {id = FieldId {unFieldId = 00000000-0000-0000-0000-000000000000}, createdAt = 2019-08-31 05:14:37.537084021 UTC, updatedAt = 2019-08-31 05:14:37.537084021 UTC, projectId = ProjectId {unProjectId = 00000000-0000-0000-0000-000000000000}, endpointHash = "", key = "respBody2", fieldType = FTUnknown, fieldTypeOverride = Nothing, format = "", formatOverride = Nothing, description = "", keyPath = "", fieldCategory = FCResponseBody, hash = "", isEnum = False, isRequired = False}])]
groupFieldsByCategory :: V.Vector Field -> Map FieldCategoryEnum [Field]
groupFieldsByCategory fields = fromList fieldGroupTupple
  where
    fields' = V.toList fields
    fieldGroup = L.groupBy (\f1 f2 -> f1.fieldCategory == f2.fieldCategory) fields'
    fieldGroupTupple = map (\f -> ((f !! 0).fieldCategory, f)) fieldGroup
