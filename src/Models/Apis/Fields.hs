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
  facetColumns,
)
where

import Control.Exception.Annotated (checkpoint)
import Data.Aeson qualified as AE
import Data.Annotation (toAnnotation)
import Data.Default
import Data.Effectful.UUID qualified as UUID
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.Display (Display, display)
import Data.Time (UTCTime, ZonedTime, addUTCTime, diffUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.Types (Only (..), Query)
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Labeled (Labeled, labeled)
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as PG
import Effectful.Reader.Static qualified
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (DB)
import Web.HttpApiData (FromHttpApiData)


-- $setup
-- >>> import Relude
-- >>> import Data.Default
-- >>> import Data.Vector hiding (fromList)
-- >>> import Data.Vector qualified as V


newtype FieldId = FieldId {unFieldId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, Ord, ToField) via UUID.UUID


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
  deriving anyclass (NFData)
  deriving (Display, FromField, ToField) via WrappedEnumSC "FC" FieldCategoryEnum


instance AE.FromJSON FieldCategoryEnum where
  parseJSON (AE.String v) = maybe empty pure (parseFieldCategoryEnum v)
  parseJSON _ = empty


instance AE.ToJSON FieldCategoryEnum where
  toJSON = AE.String . display


instance Default FieldCategoryEnum where
  def = FCQueryParam


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
  (<=) f1 f2 = (f1.projectId <= f2.projectId) && (f1.endpointHash <= f2.endpointHash) && (f1.keyPath <= f2.keyPath)


instance Eq Field where
  (==) f1 f2 = (f1.projectId == f2.projectId) && (f1.endpointHash == f2.endpointHash) && (f1.keyPath == f2.keyPath)


bulkInsertFields :: DB es => V.Vector Field -> Eff es ()
bulkInsertFields fields = void $ PG.executeMany q (V.toList rowsToInsert)
  where
    q =
      [sql| INSERT into apis.fields (project_id, endpoint_hash, key, field_type, format, description, key_path, field_category, hash)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING; |]
    rowsToInsert :: V.Vector (Projects.ProjectId, Text, Text, FieldTypes, Text, Text, Text, FieldCategoryEnum, Text)
    rowsToInsert =
      V.map
        ( \fld ->
            ( fld.projectId
            , fld.endpointHash
            , fld.key
            , fld.fieldType
            , fld.format
            , fld.description
            , fld.keyPath
            , fld.fieldCategory
            , fld.hash
            )
        )
        fields


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
bulkInsertFormat formats = void $ PG.executeMany q $ V.toList rowsToInsert
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
      formats <&> \fmt ->
        (fmt.projectId, fmt.fieldHash, fmt.fieldType, fmt.fieldFormat, fmt.examples, fmt.hash)


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
  , "attributes___error___type"
  , "attributes___exception___type"
  , "attributes___exception___message"
  , "severity___severity_text"
  , "severity___severity_number"
  , "status_message"
  ]


generateAndSaveFacets
  :: (DB es, Effectful.Reader.Static.Reader AuthContext :> es, Labeled "timefusion" WithConnection :> es, UUID.UUIDEff :> es)
  => Projects.ProjectId
  -> Text
  -> [Text]
  -> Int
  -> UTCTime
  -> Eff es FacetSummary
generateAndSaveFacets pid tableName columns maxValues timestamp = do
  let dayEnd = timestamp
      dayStart = addUTCTime (-86400) dayEnd

  authCtx <- Effectful.Reader.Static.ask @AuthContext
  facetMap <- do
    values <-
      if authCtx.env.enableTimefusionReads
        then
          checkpoint (toAnnotation (buildOptimizedFacetQuery tableName (length columns)))
            $ labeled @"timefusion" @WithConnection
            $ PG.query (buildOptimizedFacetQuery tableName (length columns)) (V.fromList columns, pid.toText, dayStart, dayEnd, maxValues)
        else
          checkpoint (toAnnotation (buildOptimizedFacetQuery tableName (length columns)))
            $ PG.query (buildOptimizedFacetQuery tableName (length columns)) (V.fromList columns, pid.toText, dayStart, dayEnd, maxValues)
    pure $ processQueryResults $ V.fromList values

  existingIdM <-
    PG.query
      [sql| SELECT id FROM apis.facet_summaries WHERE project_id = ? AND table_name = ? LIMIT 1 |]
      (pid.toText, tableName)
      <&> \case
        [] -> Nothing
        (v : _) -> Just v

  facetId <- case existingIdM of
    Just (Only uuid) -> pure uuid
    Nothing -> UUID.genUUID

  let summary = FacetSummary{id = facetId, projectId = pid.toText, tableName = tableName, facetJson = FacetData facetMap}

  _ <-
    PG.execute
      [sql| INSERT INTO apis.facet_summaries (id, project_id, table_name, facet_json)
            VALUES (?, ?, ?, ?)
            ON CONFLICT (project_id, table_name)
            DO UPDATE SET facet_json = EXCLUDED.facet_json |]
      (summary.id, pid.toText, summary.tableName, summary.facetJson)

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


buildFacetColumnQuery :: Text -> Text
buildFacetColumnQuery colName =
  "SELECT '" <> colName <> "' as column_name, " <> colName <> "::text as value, COUNT(*) as count FROM filtered_data WHERE " <> colName <> " IS NOT NULL GROUP BY value"


buildOptimizedFacetQuery :: Text -> Int -> Query
buildOptimizedFacetQuery tableName _ =
  let unionAllQueries = T.intercalate "\n      UNION ALL\n      " $ map buildFacetColumnQuery facetColumns
   in [sql|
      WITH columns_list AS (SELECT unnest(?::text[]) as column_name),
      filtered_data AS (
        SELECT * FROM |]
        <> (" " <> fromString (toString tableName) <> " ")
        <> [sql|
        WHERE project_id = ?::text AND timestamp >= ? AND timestamp < ?
      ),
      combined_facets AS (
        |]
        <> fromString (toString unionAllQueries)
        <> [sql|
      ),
      filtered_facets AS (
        SELECT cf.* FROM combined_facets cf JOIN columns_list cl ON cf.column_name = cl.column_name
      ),
      ranked_facets AS (
        SELECT column_name, value, count, ROW_NUMBER() OVER (PARTITION BY column_name ORDER BY count DESC) as rn
        FROM filtered_facets
      )
      SELECT column_name, value, count
      FROM ranked_facets
      WHERE rn <= ?
      ORDER BY column_name, count DESC
    |]


getFacetSummary :: DB es => Projects.ProjectId -> Text -> UTCTime -> UTCTime -> Eff es (Maybe FacetSummary)
getFacetSummary projectId tableName fromTime toTime = checkpoint "getFacetSummary" $ do
  let projectIdText = projectId.toText
      timeSpanSeconds = max 1 $ floor $ diffUTCTime toTime fromTime
      timeSpanMinutes = (timeSpanSeconds + 59) `div` 60

  summaryVec <-
    checkpoint "Fetching facet summary"
      $ PG.query
        [sql| SELECT id, project_id, table_name, facet_json FROM apis.facet_summaries WHERE project_id = ? AND table_name = ? LIMIT 1 |]
        (projectIdText, tableName)

  pure $ case summaryVec of
    [] -> Nothing
    [summary] -> Just $ scaleFacetSummary summary timeSpanMinutes
    _ -> error "Multiple facet summaries found for same project/table (should be impossible)"
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
