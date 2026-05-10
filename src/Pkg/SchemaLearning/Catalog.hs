{-# LANGUAGE OverloadedRecordDot #-}

-- | Pure data + merge for the in-memory schema-learning catalog.
--
-- Two-tier model:
--
--   * 'Template' — structural skeleton (field paths, types, formats, category,
--     enum-flag). Content-addressable via 'templateHash', dedup'd across the
--     whole instance: identical autoinstrumentation spans (e.g. @redis.get@)
--     collapse to one row no matter how many tenants emit them. Holds no
--     examples, no values — safe to share.
--   * 'CatalogEntry' — per-(project, key) row carrying tenant-private bits:
--     'Scope', 'Template', sampled examples, top-K counts, timestamps.
module Pkg.SchemaLearning.Catalog (
  KeyKind (..),
  Scope (..),
  emptyScope,
  FieldStruct (..),
  Template (..),
  Examples (..),
  TopK (..),
  CatalogEntry (..),
  SummaryDoc (..),
  emptySummaryDoc,
  templateHash,
  fieldKindOfValue,
  emptyExamples,
  emptyTopK,
  newEntry,
  mergeFullWalk,
  bumpSeen,
  classifyFormat,
  -- Anomaly diffing.
  AnomalyKind (..),
  ProducedAnomaly (..),
  diffAnomalies,
  fieldHashSuffix,
  examplesCap,
  topKCap,
  exampleStringCap,
  -- Re-homed from the deleted "Models.Apis.Fields"; Anomalies / SchemaCatalog
  -- still need these symbols for VM types.
  FieldTypes (..),
  FieldCategoryEnum (..),
  FieldId,
  FormatId,
  ShapeId,
  FacetValue (..),
  FacetData (..),
  FacetSummary (..),
)
where

import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.Char (toLower)
import Data.Default (Default)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Data.Text.Display (Display)
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as DAE
import GHC.Records (HasField (getField))
import Hasql.Interpolate qualified as HI
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Relude
import Utils (toXXHash)
import Web.HttpApiData (FromHttpApiData)


-- $setup
-- >>> :set -XOverloadedStrings -XQuasiQuotes -XOverloadedRecordDot
-- >>> import Data.Aeson.QQ.Simple (aesonQQ)
-- >>> import Data.HashMap.Strict qualified as HM
-- >>> import Data.HashSet qualified as HS
-- >>> import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
-- >>> let t0 = UTCTime (fromGregorian 2026 5 10) (secondsToDiffTime 0)


-- | The two top-level routing keys for a span. HTTP spans group by
-- @(project, host, method, normalized_path)@; everything else groups by
-- @(project, service.name, span.name, kind)@.
data KeyKind = HttpEndpoint | SpanIdentity
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromField, ToField) via WrappedEnumSC "" KeyKind
  deriving (HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC "" KeyKind


-- | The per-tenant identity of a key. Stored as JSON in @apis.schema_catalog.scope@.
data Scope = Scope
  { service :: !(Maybe Text)
  , spanName :: !(Maybe Text)
  , kind :: !(Maybe Text)
  , host :: !(Maybe Text)
  , method :: !(Maybe Text)
  , urlPath :: !(Maybe Text)
  , statusCodes :: !(V.Vector Int)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Scope


-- | Default 'Scope' — useful for callers that build scopes incrementally.
emptyScope :: Scope
emptyScope = Scope Nothing Nothing Nothing Nothing Nothing Nothing V.empty


-- | Structural facts about one field path. Sets are ordered for deterministic
-- hashing.
data FieldStruct = FieldStruct
  { types :: !(HS.HashSet FieldTypes)
  , formats :: !(HS.HashSet Text)
  , category :: !FieldCategoryEnum
  , isEnum :: !Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] FieldStruct


-- | The structure of a span family. Stored once per unique 'templateHash'
-- across the whole instance.
data Template = Template
  { keyKind :: !KeyKind
  , fields :: !(HM.HashMap Text FieldStruct)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Template


-- | Per-field example reservoir. Capped at 'examplesCap' to bound memory; once
-- full, merging is a no-op.
newtype Examples = Examples {values :: V.Vector AE.Value}
  deriving stock (Eq, Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON)
  deriving anyclass (NFData)


-- | Per-field top-K cardinality / value counts. Summary input replacement for
-- @apis.facet_summaries@.
data TopK = TopK
  { distinct :: !Word64
  , top :: !(HM.HashMap Text Word64)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] TopK


-- | Per-(project, key_hash) row. Mirrors @apis.schema_catalog@ plus a transient
-- 'dirty' flag the flush writer reads.
data CatalogEntry = CatalogEntry
  { scope :: !Scope
  , template :: !Template
  , valuesDelta :: !(HM.HashMap Text Examples)
  , counts :: !(HM.HashMap Text TopK)
  , sampleCount :: !Word64
  , firstSeen :: !UTCTime
  , lastSeen :: !UTCTime
  , dirty :: !Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | Convenience: 'CatalogEntry' carries a single 'templateHash' field via
-- pattern.
instance HasField "templateHash" CatalogEntry Text where
  getField e = templateHash e.template


-- ---------------------------------------------------------------------------
-- Caps. Mirror legacy fieldsToFieldDTO defaults.

examplesCap :: Int
examplesCap = 20


topKCap :: Int
topKCap = 50


exampleStringCap :: Int
exampleStringCap = 256


emptyExamples :: Examples
emptyExamples = Examples V.empty


emptyTopK :: TopK
emptyTopK = TopK 0 HM.empty


-- ---------------------------------------------------------------------------
-- Template hash.

-- | Content-addressable hash of a 'Template'. Two templates with identical
-- field skeletons hash the same, regardless of insertion order.
--
-- >>> let mk xs = templateHash (Template HttpEndpoint (HM.fromList xs))
-- >>> let f1 = ("user.id",    FieldStruct (HS.fromList [FTString]) (HS.fromList ["{uuid}"])  FCRequestBody False)
-- >>> let f2 = ("user.email", FieldStruct (HS.fromList [FTString]) (HS.fromList ["{email}"]) FCRequestBody False)
-- >>> mk [f1, f2] == mk [f2, f1]
-- True
-- >>> mk [f1] == mk [f2]
-- False
templateHash :: Template -> Text
templateHash t =
  toXXHash $ T.intercalate "\n" $ render t.keyKind : sort (renderField <$> HM.toList t.fields)
  where
    render kk = T.pack (show kk)
    renderField (path, fs) =
      path
        <> "|"
        <> T.intercalate "," (sort (T.pack . show <$> HS.toList fs.types))
        <> "|"
        <> T.intercalate "," (sort (HS.toList fs.formats))
        <> "|"
        <> T.pack (show fs.category)
        <> "|"
        <> bool "0" "1" fs.isEnum


-- ---------------------------------------------------------------------------
-- Walk helpers.

-- | Bucket a JSON value into a 'FieldTypes'. Same buckets as the legacy
-- @aeValueToFieldType@ in 'ProcessMessage.fieldsToFieldDTO'.
fieldKindOfValue :: AE.Value -> FieldTypes
fieldKindOfValue = \case
  AET.String _ -> FTString
  AET.Number _ -> FTNumber
  AET.Bool _ -> FTBool
  AET.Null -> FTNull
  AET.Object _ -> FTObject
  AET.Array _ -> FTList


-- | Map a value to its format vocabulary entry. Numeric formats use the same
-- @"integer"@/@"float"@ vocabulary as 'ProcessMessage.valueToFormat'; string
-- formats are determined by the caller via 'valueToFormatStr' so we don't
-- import the regex set here.
classifyFormat :: AE.Value -> Maybe Text -> Text
classifyFormat v fromCaller = case v of
  AET.String _ -> fromMaybe "text" fromCaller
  AET.Number n
    | Scientific.isFloating n -> "float"
    | Scientific.isInteger n -> "integer"
    | otherwise -> "unknown"
  AET.Bool _ -> "bool"
  AET.Null -> "null"
  AET.Object _ -> "object"
  AET.Array _ -> "array"


-- ---------------------------------------------------------------------------
-- Entry construction & merge.

-- | Empty entry used the first time a key is observed.
newEntry :: KeyKind -> Scope -> UTCTime -> CatalogEntry
newEntry kk sc now =
  CatalogEntry
    { scope = sc
    , template = Template kk HM.empty
    , valuesDelta = HM.empty
    , counts = HM.empty
    , sampleCount = 0
    , firstSeen = now
    , lastSeen = now
    , dirty = True
    }


-- | Strict cap on a string sample value: oversize strings are truncated with
-- an ellipsis.
boundExampleValue :: AE.Value -> AE.Value
boundExampleValue = \case
  AET.String s | T.length s > exampleStringCap -> AET.String (T.take exampleStringCap s <> "…")
  v -> v


mergeExamples :: V.Vector AE.Value -> Examples -> Examples
mergeExamples incoming (Examples existing)
  | V.length existing >= examplesCap = Examples existing
  | otherwise =
      let bounded = V.map boundExampleValue incoming
          seen = HS.fromList (V.toList existing)
          fresh = V.filter (\v -> not (HS.member v seen)) bounded
          merged = existing V.++ V.take (examplesCap - V.length existing) fresh
       in Examples merged


bumpTopK :: V.Vector AE.Value -> TopK -> TopK
bumpTopK incoming (TopK d t) =
  let texts = V.toList $ V.mapMaybe valueAsText incoming
      bumped = foldl' (\acc k -> HM.insertWith (+) k 1 acc) t texts
      capped =
        if HM.size bumped <= topKCap
          then bumped
          else HM.fromList $ take topKCap $ sortOn (negate . snd) (HM.toList bumped)
      newDistinct = d + fromIntegral (length texts)
   in TopK newDistinct capped
  where
    valueAsText (AET.String s) = Just s
    valueAsText (AET.Number n) = Just (T.pack (show n))
    valueAsText (AET.Bool b) = Just (if b then "true" else "false")
    valueAsText _ = Nothing


-- | Merge a single span's leaf-walked fields into an entry. Caller supplies
-- @(keyPath, values, category)@ tuples; each value carries its own optional
-- format hint (the result of @ProcessMessage.valueToFormatStr@ applied to
-- that specific value, or 'Nothing'). Per-value because a single key path can
-- carry mixed formats (e.g. a field that's sometimes @{uuid}@, sometimes
-- @{integer}@).
--
-- Commutative: the result of merging spans @a@ then @b@ equals @b@ then @a@
-- modulo example reservoir ordering (capped + dedup'd).
mergeFullWalk
  :: Scope
  -> [(Text, V.Vector (AE.Value, Maybe Text), FieldCategoryEnum)]
  -> UTCTime
  -> CatalogEntry
  -> CatalogEntry
mergeFullWalk newScope walk now e =
  let (newFields, newValues, newCounts) = foldl' step (e.template.fields, e.valuesDelta, e.counts) walk
      newTemplate = Template e.template.keyKind newFields
   in e
        { scope = mergeScope e.scope newScope
        , template = newTemplate
        , valuesDelta = newValues
        , counts = newCounts
        , sampleCount = e.sampleCount + 1
        , lastSeen = now
        , dirty = True
        }
  where
    step (flds, vals, cnts) (path, vhs, cat) =
      let bareValues = V.map fst vhs
          kinds = HS.fromList $ V.toList $ V.map fieldKindOfValue bareValues
          fmts = HS.fromList $ V.toList $ V.map (uncurry classifyFormat) vhs
          fs0 = HM.lookupDefault (FieldStruct HS.empty HS.empty cat False) path flds
          fs1 = fs0{types = fs0.types <> kinds, formats = fs0.formats <> fmts, category = cat}
          flds' = HM.insert path fs1 flds
          vals' = HM.insertWith (\new old -> mergeExamples new.values old) path (Examples bareValues) vals
          cnts' = HM.alter (Just . bumpTopK bareValues . fromMaybe emptyTopK) path cnts
       in (flds', vals', cnts')


-- | Fast path: known key, past learning threshold. Just touches counters.
bumpSeen :: UTCTime -> CatalogEntry -> CatalogEntry
bumpSeen now e = e{sampleCount = e.sampleCount + 1, lastSeen = now, dirty = True}


-- | Union scopes, preferring populated fields and union'ing observed status
-- codes (capped to 32 distinct values).
mergeScope :: Scope -> Scope -> Scope
mergeScope a b =
  Scope
    { service = a.service <|> b.service
    , spanName = a.spanName <|> b.spanName
    , kind = a.kind <|> b.kind
    , host = a.host <|> b.host
    , method = a.method <|> b.method
    , urlPath = a.urlPath <|> b.urlPath
    , statusCodes =
        let merged = HS.fromList (V.toList a.statusCodes ++ V.toList b.statusCodes)
         in V.fromList $ take 32 $ sort $ HS.toList merged
    }


-- ---------------------------------------------------------------------------
-- Per-project summary doc (materialised AI/query-editor read).

-- | Stored verbatim in @apis.schema_summary.doc@ (jsonb). The shape is the
-- 'getSummary' query's response: enough info to feed the AI prompt and the
-- query-editor's autocomplete without further joins.
data SummaryDoc = SummaryDoc
  { fields :: !(HM.HashMap Text FieldStruct)
  , services :: !(V.Vector Text)
  , topValuesByField :: !(HM.HashMap Text TopK)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SummaryDoc


emptySummaryDoc :: SummaryDoc
emptySummaryDoc = SummaryDoc HM.empty V.empty HM.empty


-- ---------------------------------------------------------------------------
-- Anomaly diffing.
--
-- Replaces the legacy DB-trigger fan-out (@new_anomaly_proc@). The flush
-- worker calls 'diffAnomalies' for each dirty entry against its prior
-- catalog row; emitted 'ProducedAnomaly's are inserted into
-- @apis.anomalies@ and a @NewAnomaly@ background job is enqueued so the
-- existing notification pipeline keeps firing.

-- | Anomaly buckets matching @apis.anomaly_type@ minus runtime-exception
-- (which the error-pattern path produces directly).
data AnomalyKind = AKEndpoint | AKShape | AKField | AKFormat
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)


-- | One emitted anomaly. 'targetHash' is shaped so 'getAnomaliesVM' can
-- recover the endpoint via @starts_with(target_hash, endpoints.hash)@.
data ProducedAnomaly = ProducedAnomaly
  { kind :: !AnomalyKind
  , targetHash :: !Text
  , keyHash :: !Text
  -- ^ owning catalog key — for joining back to apis.schema_catalog
  , fieldPath :: !(Maybe Text)
  -- ^ populated for AKField / AKFormat
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)


-- | Stable 8-char suffix for a field path. Keeps target_hash bounded
-- and gives the read path a deterministic way back to the field.
fieldHashSuffix :: Text -> Text
fieldHashSuffix path = T.take 8 (toXXHash path)


-- | Diff a (possibly absent) prior entry against the current one.
-- Order: endpoint > shape > field > format. Endpoint anomalies fire
-- only on the @HttpEndpoint@ key kind — non-HTTP keys still get shape /
-- field / format diffs, just not the "new endpoint" headline.
--
-- >>> let path = "request.body.user.id"
-- >>> let fs1 = FieldStruct (HS.fromList [FTString]) (HS.fromList ["{uuid}"]) FCRequestBody False
-- >>> let fs2 = FieldStruct (HS.fromList [FTString, FTNumber]) (HS.fromList ["{uuid}"]) FCRequestBody False
-- >>> let mk fs = CatalogEntry emptyScope (Template HttpEndpoint (HM.fromList fs)) HM.empty HM.empty 1 t0 t0 True
-- >>> let new = mk [(path, fs1)]
-- >>> map (.kind) (diffAnomalies "kh" Nothing new)
-- [AKEndpoint,AKShape,AKField,AKFormat]
-- >>> map (.kind) (diffAnomalies "kh" (Just new) new)
-- []
-- >>> -- type widened on existing field → AKShape (template hash changed) + AKFormat
-- >>> map (.kind) (diffAnomalies "kh" (Just new) (mk [(path, fs2)]))
-- [AKShape,AKFormat]
-- >>> -- new field added → AKShape + AKField + AKFormat
-- >>> let new2 = mk [(path, fs1), ("request.body.email", fs1)]
-- >>> sort (map (.kind) (diffAnomalies "kh" (Just new) new2))
-- [AKShape,AKField,AKFormat]
diffAnomalies :: Text -> Maybe CatalogEntry -> CatalogEntry -> [ProducedAnomaly]
diffAnomalies kh priorM cur =
  let priorFields = maybe HM.empty (.template.fields) priorM
      curFields = cur.template.fields
      isNew = isNothing priorM
      isHttp = cur.template.keyKind == HttpEndpoint
      headline =
        [ ProducedAnomaly AKEndpoint kh kh Nothing
        | isNew, isHttp
        ]
      shapeChanged = case priorM of
        Nothing -> True
        Just p -> templateHash p.template /= templateHash cur.template
      shape =
        [ ProducedAnomaly AKShape (kh <> ":s:" <> T.take 8 (templateHash cur.template)) kh Nothing
        | shapeChanged
        ]
      newPaths = HS.toList $ HS.difference (HS.fromList (HM.keys curFields)) (HS.fromList (HM.keys priorFields))
      fields =
        [ ProducedAnomaly AKField (kh <> ":f:" <> fieldHashSuffix p) kh (Just p)
        | p <- sort newPaths
        ]
      formatChanged path =
        case (HM.lookup path priorFields, HM.lookup path curFields) of
          (Just p, Just c) -> p.types /= c.types || p.formats /= c.formats
          (Nothing, Just _) -> True
          _ -> False
      changedFormatPaths = filter formatChanged (HM.keys curFields)
      formats =
        [ ProducedAnomaly AKFormat (kh <> ":fmt:" <> fieldHashSuffix p) kh (Just p)
        | p <- sort changedFormatPaths
        ]
   in headline <> shape <> fields <> formats


-- ---------------------------------------------------------------------------
-- Re-homed from the deleted "Models.Apis.Fields". Kept name-compatible so
-- existing readers (Anomalies VM, SchemaCatalog adapter) work unchanged.

-- | Primitive JSON-leaf bucket. Mirrors the legacy @apis.field_type@ enum so
-- migration / readers don't have to translate.
data FieldTypes = FTUnknown | FTString | FTNumber | FTBool | FTObject | FTList | FTNull
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Default, Hashable, NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromField, ToField) via WrappedEnumSC "FT" FieldTypes
  deriving (HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC "FT" FieldTypes


instance HasField "toText" FieldTypes Text where
  getField = toText . map toLower . drop 2 . show


-- | Where on the span a field came from. HTTP-specific buckets keep parity
-- with the legacy @apis.field_category@; @FCAttribute@/@FCResource@/@FCEvent@
-- cover non-HTTP spans. The new variants only flow through
-- @apis.schema_catalog.fields@ (jsonb) — never written to the legacy
-- @apis.field_category@ PG enum column, so no @ALTER TYPE@ is needed.
data FieldCategoryEnum
  = FCQueryParam
  | FCPathParam
  | FCRequestHeader
  | FCResponseHeader
  | FCRequestBody
  | FCResponseBody
  | FCAttribute
  | FCResource
  | FCEvent
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, ToField) via WrappedEnumSC "FC" FieldCategoryEnum
  deriving (HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC "FC" FieldCategoryEnum


-- Type aliases for the legacy ID newtypes — VM types in
-- "Models.Apis.Anomalies" still reference these.
type FieldId = UUIDId "field"


type FormatId = UUIDId "format"


type ShapeId = UUIDId "shape"


-- ---------------------------------------------------------------------------
-- Facet types (re-homed from "Models.Apis.Fields"). Kept here so callers
-- of the AI / query-editor / log-explorer stack don't need to chase the
-- migration.

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


-- Suppress warnings if FromHttpApiData / FromRow stay unused by direct
-- references — the deriving-via lines need them in scope.
_keepFromHttpApiData :: Maybe (Proxy FromHttpApiData)
_keepFromHttpApiData = Nothing
