{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Models.Apis.Fields (
  -- Types
  Field (..),
  FieldTypes (..),
  FieldCategoryEnum (..),
  FieldId (..),
  SwField (..),
  FacetSummary (..),
  FacetValue (..),
  FacetData (..),
  parseFieldCategoryEnum,
  parseFieldTypes,
  fieldTypeToText,
  bulkInsertFields,
  Format (..),
  FormatId,
  SwFormat (..),
  bulkInsertFormat,
  -- Facets
  generateAndSaveFacets,
  getFacetSummary,
  -- Shapes
  Shape (..),
  ShapeWithFields (..),
  SwShape (..),
  ShapeId,
  bulkInsertShapes,
)
where

import Control.Exception.Annotated (checkpoint)
import Data.Aeson qualified as AE
import Data.Annotation (toAnnotation)
import Data.Default
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.UUID qualified as UUID
import Data.HashMap.Strict qualified as HM
import Data.Text.Display (Display, display)
import Data.Time (UTCTime, ZonedTime, addUTCTime, diffUTCTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Labeled (Labeled)
import GHC.Records (HasField (getField))
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (DB, UUIDId (..), WrappedEnumSC (..), rawSql)
import Relude
import Web.HttpApiData (FromHttpApiData)


-- $setup
-- >>> import Relude
-- >>> import Data.Default
-- >>> import Data.Vector hiding (fromList)
-- >>> import Data.Vector qualified as V


newtype FieldId = FieldId {unFieldId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, Ord, ToField) via UUID.UUID


data FieldTypes
  = FTUnknown
  | FTString
  | FTNumber
  | FTBool
  | FTObject
  | FTList
  | FTNull
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Default, NFData)
  deriving (FromField, ToField) via WrappedEnumSC "FT" FieldTypes
  deriving (HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC "FT" FieldTypes


instance AE.FromJSON FieldTypes where
  parseJSON (AE.String v) = maybe empty pure (parseFieldTypes v)
  parseJSON _ = empty


instance AE.ToJSON FieldTypes where
  toJSON = AE.String . fieldTypeToText


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
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Default, NFData)
  deriving (Display, FromField, ToField) via WrappedEnumSC "FC" FieldCategoryEnum
  deriving (HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC "FC" FieldCategoryEnum


instance AE.FromJSON FieldCategoryEnum where
  parseJSON (AE.String v) = maybe empty pure (parseFieldCategoryEnum v)
  parseJSON _ = empty


instance AE.ToJSON FieldCategoryEnum where
  toJSON = AE.String . display


parseFieldCategoryEnum :: (Eq s, IsString s) => s -> Maybe FieldCategoryEnum
parseFieldCategoryEnum "query_param" = Just FCQueryParam
parseFieldCategoryEnum "path_param" = Just FCPathParam
parseFieldCategoryEnum "request_header" = Just FCRequestHeader
parseFieldCategoryEnum "response_header" = Just FCResponseHeader
parseFieldCategoryEnum "request_body" = Just FCRequestBody
parseFieldCategoryEnum "response_body" = Just FCResponseBody
parseFieldCategoryEnum _ = Nothing


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
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "fields", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Field)
  deriving (FromField) via Aeson Field
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Field


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


newtype FacetData = FacetData (HM.HashMap Text [FacetValue])
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData)
  deriving (FromField, ToField) via Aeson FacetData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] FacetData
  deriving (HI.DecodeValue, HI.EncodeValue) via HI.AsJsonb FacetData


data FacetSummary = FacetSummary
  { id :: UUID.UUID
  , projectId :: Text
  , tableName :: Text
  , facetJson :: FacetData
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "facet_summaries", PrimaryKey "id", FieldModifiers '[CamelToSnake]] FacetSummary)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] FacetSummary


instance Ord Field where
  (<=) f1 f2 = (f1.projectId <= f2.projectId) && (f1.endpointHash <= f2.endpointHash) && (f1.keyPath <= f2.keyPath)


instance Eq Field where
  (==) f1 f2 = (f1.projectId == f2.projectId) && (f1.endpointHash == f2.endpointHash) && (f1.keyPath == f2.keyPath)


bulkInsertFields :: DB es => V.Vector Field -> Eff es ()
bulkInsertFields flds | V.null flds = pass
bulkInsertFields flds =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.fields (project_id, endpoint_hash, key, field_type, format, description, key_path, field_category, hash)
           SELECT * FROM unnest(#{pids}::uuid[], #{ehs}::text[], #{keys}::text[], #{fts}::apis.field_type[], #{fmts}::text[], #{descs}::text[], #{kps}::text[], #{fcs}::apis.field_category[], #{hs}::text[])
           ON CONFLICT DO NOTHING |]
  where
    pids = V.map (.projectId) flds
    ehs = V.map (.endpointHash) flds
    keys = V.map (.key) flds
    fts = V.map (.fieldType) flds
    fmts = V.map (.format) flds
    descs = V.map (.description) flds
    kps = V.map (.keyPath) flds
    fcs = V.map (.fieldCategory) flds
    hs = V.map (.hash) flds


---------------------------------
-- Formats
type FormatId = UUIDId "format"


data Format = Format
  { id :: FormatId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , fieldHash :: Text
  , fieldType :: FieldTypes
  , fieldFormat :: Text
  , examples :: V.Vector AE.Value
  , hash :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "formats", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Format)
  deriving (FromField) via Aeson Format
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Format


bulkInsertFormat :: DB es => V.Vector Format -> Eff es ()
bulkInsertFormat = V.mapM_ \Format{projectId, fieldHash, fieldType, fieldFormat, examples, hash} ->
  Hasql.interpExecute
    [HI.sql| INSERT INTO apis.formats (project_id, field_hash, field_type, field_format, examples, hash)
             VALUES (#{projectId}, #{fieldHash}, #{fieldType}::apis.field_type, #{fieldFormat}, #{examples}, #{hash})
             ON CONFLICT (project_id, field_hash, field_format)
             DO UPDATE SET field_type = EXCLUDED.field_type, hash = EXCLUDED.hash,
                examples = CASE WHEN COALESCE(array_length(apis.formats.examples, 1), 0) >= 20
                  THEN apis.formats.examples
                  ELSE ARRAY(SELECT DISTINCT e FROM unnest(apis.formats.examples || excluded.examples) AS e ORDER BY e LIMIT 20) END |]


data SwFormat = SwFormat
  { swFieldHash :: Text
  , swFieldType :: FieldTypes
  , swFieldFormat :: Text
  , swExamples :: V.Vector AE.Value
  , swHash :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving anyclass (AE.ToJSON)
  deriving (FromField) via Aeson SwFormat
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SwFormat


---------------------------------
-- Facets

facetColumns :: [Text]
facetColumns =
  [ "name"
  , "resource___service___name"
  , "resource___service___version"
  , "kind"
  , "status_code"
  , "level"
  , "attributes___http___request___method"
  , "attributes___http___response___status_code"
  , "attributes___error___type"
  , "resource___service___instance___id"
  , "resource___service___namespace"
  , "resource___telemetry___sdk___language"
  , "resource___telemetry___sdk___name"
  , "resource___telemetry___sdk___version"
  , "attributes___http___request___method_original"
  , "attributes___http___request___resend_count"
  , "attributes___http___request___body___size"
  , "attributes___url___path"
  , "attributes___url___scheme"
  , "attributes___url___full"
  , "attributes___url___fragment"
  , "attributes___url___query"
  , "attributes___user_agent___original"
  , "attributes___network___protocol___name"
  , "attributes___network___protocol___version"
  , "attributes___network___transport"
  , "attributes___network___type"
  , "attributes___client___address"
  , "attributes___server___address"
  , "attributes___user___id"
  , "attributes___user___email"
  , "attributes___user___name"
  , "attributes___user___full_name"
  , "attributes___session___id"
  , "attributes___session___previous___id"
  , "attributes___db___system___name"
  , "attributes___db___collection___name"
  , "attributes___db___namespace"
  , "attributes___db___operation___name"
  , "attributes___db___response___status_code"
  , "attributes___db___operation___batch___size"
  , "attributes___exception___type"
  , "attributes___exception___message"
  , "severity___severity_text"
  , "severity___severity_number"
  , "status_message"
  ]


generateAndSaveFacets
  :: (DB es, Labeled "timefusion" Hasql :> es, UUID.UUIDEff :> es)
  => Bool
  -- ^ enableTimefusionReads (caller passes it to keep this module a leaf of
  -- `System.Config` — breaks the `Config ↔ Telemetry` cycle).
  -> Projects.ProjectId
  -> Text
  -> Int
  -> UTCTime
  -> Eff es FacetSummary
generateAndSaveFacets enableTfReads pid tableName maxValues timestamp = do
  let dayEnd = timestamp
      dayStart = addUTCTime (-86400) dayEnd

  let pidText = pid.toText
      facetSql = mconcat $ intersperse (fromString "\nUNION ALL\n") $ map (buildFacetColumnSql tableName pidText dayStart dayEnd maxValues) facetColumns
  facetMap <- do
    values <- checkpoint (toAnnotation ("facet-query" :: Text)) $ Hasql.withHasqlTimefusion enableTfReads $ Hasql.interp facetSql
    pure $ processQueryResults $ V.fromList values
  existingIdM <-
    Hasql.interpOne
      [HI.sql| SELECT id FROM apis.facet_summaries WHERE project_id = #{pidText} AND table_name = #{tableName} LIMIT 1 |]

  facetId <- maybe UUID.genUUID pure existingIdM

  let facetData = FacetData facetMap
      summary = FacetSummary{id = facetId, projectId = pidText, tableName = tableName, facetJson = facetData}

  _ <-
    Hasql.interpExecute
      [HI.sql| INSERT INTO apis.facet_summaries (id, project_id, table_name, facet_json)
             VALUES (#{facetId}, #{pidText}, #{tableName}, #{facetData})
             ON CONFLICT (project_id, table_name)
             DO UPDATE SET facet_json = EXCLUDED.facet_json |]

  pure summary


processQueryResults :: V.Vector (Text, Text, Int) -> HM.HashMap Text [FacetValue]
processQueryResults =
  V.foldr' addResult HM.empty
  where
    addResult (colName, valText, count) acc =
      let currentVals = HM.lookupDefault [] colName acc
          newVal = FacetValue valText count
          updatedVals = insertSorted newVal currentVals
       in HM.insert colName updatedVals acc
    insertSorted newVal [] = [newVal]
    insertSorted newVal@(FacetValue _ count) (x@(FacetValue _ xcount) : xs)
      | count > xcount = newVal : x : xs
      | otherwise = x : insertSorted newVal xs


buildFacetColumnSql :: Text -> Text -> UTCTime -> UTCTime -> Int -> Text -> HI.Sql
buildFacetColumnSql tableName pidText dayStart dayEnd maxValues colName =
  rawSql ("(SELECT '" <> colName <> "' as column_name, " <> colName <> "::text as value, COUNT(*)::INT as count FROM " <> tableName <> " WHERE project_id = ")
    <> [HI.sql|#{pidText}::text|]
    <> [HI.sql| AND timestamp >= #{dayStart} AND timestamp < #{dayEnd} AND |]
    <> rawSql (colName <> " IS NOT NULL GROUP BY value ORDER BY count DESC LIMIT ")
    <> [HI.sql|#{maxValues})|]


getFacetSummary :: DB es => Projects.ProjectId -> Text -> UTCTime -> UTCTime -> Eff es (Maybe FacetSummary)
getFacetSummary projectId tableName fromTime toTime = checkpoint "getFacetSummary" $ do
  let projectIdText = projectId.toText
      timeSpanSeconds = max 1 $ floor $ diffUTCTime toTime fromTime
      timeSpanMinutes = (timeSpanSeconds + 59) `div` 60

  summaryM <-
    checkpoint "Fetching facet summary"
      $ Hasql.interpOne
        [HI.sql| SELECT id, project_id, table_name, facet_json FROM apis.facet_summaries WHERE project_id = #{projectIdText} AND table_name = #{tableName} LIMIT 1 |]

  pure $ scaleFacetSummary <$> summaryM <*> pure timeSpanMinutes
  where
    scaleFacetSummary :: FacetSummary -> Int -> FacetSummary
    scaleFacetSummary summary timeSpanMinutes =
      let scaleFactor = fromIntegral timeSpanMinutes / 1440.0
          (FacetData facetMap) = summary.facetJson
          scaledMap =
            if abs (scaleFactor - 1.0) < 0.01
              then facetMap
              else HM.map (scaleFacetValues scaleFactor) facetMap
       in summary{facetJson = FacetData scaledMap}
    scaleFacetValues :: Double -> [FacetValue] -> [FacetValue]
    scaleFacetValues factor =
      map (\(FacetValue v c) -> FacetValue v (max 1 $ ceiling $ fromIntegral c * factor))


---------------------------------
-- Shapes

type ShapeId = UUIDId "shape"


data ShapeWithFields = ShapeWidthFields
  { status :: Int
  , sHash :: Text
  , fieldsMap :: Map FieldCategoryEnum [Field]
  , reqDescription :: Text
  , resDescription :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)


data Shape = Shape
  { id :: ShapeId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , approvedOn :: Maybe UTCTime
  , projectId :: Projects.ProjectId
  , endpointHash :: Text
  , queryParamsKeypaths :: V.Vector Text
  , requestBodyKeypaths :: V.Vector Text
  , responseBodyKeypaths :: V.Vector Text
  , requestHeadersKeypaths :: V.Vector Text
  , responseHeadersKeypaths :: V.Vector Text
  , fieldHashes :: V.Vector Text
  , hash :: Text
  , statusCode :: Int
  , responseDescription :: Text
  , requestDescription :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "shapes", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Shape)
  deriving (FromField) via Aeson Shape
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Shape


bulkInsertShapes :: DB es => V.Vector Shape -> Eff es ()
bulkInsertShapes = V.mapM_ \Shape{projectId, endpointHash, queryParamsKeypaths, requestBodyKeypaths, responseBodyKeypaths, requestHeadersKeypaths, responseHeadersKeypaths, fieldHashes, hash, statusCode} ->
  Hasql.interpExecute
    [HI.sql| INSERT INTO apis.shapes
             (project_id, endpoint_hash, query_params_keypaths, request_body_keypaths, response_body_keypaths, request_headers_keypaths, response_headers_keypaths, field_hashes, hash, status_code, request_description, response_description)
             VALUES (#{projectId}, #{endpointHash}, #{queryParamsKeypaths}, #{requestBodyKeypaths}, #{responseBodyKeypaths}, #{requestHeadersKeypaths}, #{responseHeadersKeypaths}, #{fieldHashes}, #{hash}, #{statusCode}, '', '')
             ON CONFLICT DO NOTHING |]


data SwShape = SwShape
  { swEndpointHash :: Text
  , swQueryParamsKeypaths :: V.Vector Text
  , swRequestBodyKeypaths :: V.Vector Text
  , swResponseBodyKeypaths :: V.Vector Text
  , swRequestHeadersKeypaths :: V.Vector Text
  , swResponseHeadersKeypaths :: V.Vector Text
  , swHash :: Text
  , swStatusCode :: Int
  , swFieldHashes :: V.Vector Text
  , swRequestDescription :: Text
  , swResponseDescription :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving anyclass (AE.ToJSON)
  deriving (FromField) via Aeson SwShape
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SwShape
