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
module Pkg.SchemaLearning (
  KeyKind (..),
  Scope (..),
  FieldStruct (..),
  Template (..),
  Examples (..),
  TopK (..),
  CatalogEntry (..),
  SummaryDoc (..),
  templateHash,
  fieldKindOfValue,
  emptyTopK,
  emptyScope,
  newEntry,
  mergeFullWalk,
  mergeScope,
  classifyFormat,
  -- Anomaly diffing.
  AnomalyKind (..),
  ShapeDelta (..),
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

  -- * Streaming hot path
  SchemaShardState (..),
  emptySchemaShardState,
  SchemaKey (..),
  DecisionPolicy (..),
  defaultPolicy,
  ObservationInput (..),
  observeSpans,
  takeDirty,
  pruneEvicted,
  evictLRU,
  snapshotForFlush,
  commitFlushedSnapshot,
)
where

import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.Char (toLower)
import Data.Default (Default)
import Data.Generics.Product.Fields (field)
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
import Deriving.Aeson.Stock qualified as DAE
import GHC.Records (HasField (getField))
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Relude
import Utils (toXXHash)
import Web.Wire (FacetValue (..))


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
  deriving (AE.FromJSON, AE.ToJSON, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC ('Just "apis.schema_key_kind") "" KeyKind


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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Scope


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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake FieldStruct


-- | The structure of a span family. Stored once per unique 'templateHash'
-- across the whole instance.
data Template = Template
  { keyKind :: !KeyKind
  , fields :: !(HM.HashMap Text FieldStruct)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Template


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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake TopK


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


emptyTopK :: TopK
emptyTopK = TopK 0 HM.empty


-- | All-Nothing 'Scope'. Used in tests and as a baseline before merging
-- real scopes in.
emptyScope :: Scope
emptyScope = Scope Nothing Nothing Nothing Nothing Nothing Nothing V.empty


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
  toXXHash $ T.intercalate "\n" $ show t.keyKind : sort (renderField <$> HM.toList t.fields)
  where
    renderField (path, fs) =
      T.intercalate
        "|"
        [ path
        , T.intercalate "," (sort (show <$> HS.toList fs.types))
        , T.intercalate "," (sort (HS.toList fs.formats))
        , show fs.category
        , bool "0" "1" fs.isEnum
        ]


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
          fresh = V.fromList $ ordNub [v | v <- V.toList bounded, not (HS.member v seen)]
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
    valueAsText (AET.Number n) = Just (show n)
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
  :: Int
  -- ^ Per-entry field cap (see 'Hot.DecisionPolicy.maxFieldsPerEntry').
  -- New paths beyond this size are silently dropped. Existing paths
  -- always update — we keep refining what we already know.
  -> Scope
  -> [(Text, V.Vector (AE.Value, Maybe Text), FieldCategoryEnum)]
  -> UTCTime
  -> CatalogEntry
  -> CatalogEntry
mergeFullWalk fieldsCap newScope walk now e =
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
    step acc@(flds, vals, cnts) (path, vhs, cat)
      -- Path already present → always refine (cheap; no growth).
      | HM.member path flds = update flds vals cnts path vhs cat
      -- New path but at cap → drop.
      | HM.size flds >= fieldsCap = acc
      | otherwise = update flds vals cnts path vhs cat
    update flds vals cnts path vhs cat =
      let bareValues = V.map fst vhs
          kinds = HS.fromList $ V.toList $ V.map fieldKindOfValue bareValues
          fmts = HS.fromList $ V.toList $ V.map (uncurry classifyFormat) vhs
          fs0 = HM.lookupDefault (FieldStruct HS.empty HS.empty cat False) path flds
          fs1 = fs0{types = fs0.types <> kinds, formats = fs0.formats <> fmts, category = cat}
          flds' = HM.insert path fs1 flds
          vals' = HM.insertWith (\new old -> mergeExamples new.values old) path (mergeExamples bareValues (Examples V.empty)) vals
          cnts' = HM.alter (Just . bumpTopK bareValues . fromMaybe emptyTopK) path cnts
       in (flds', vals', cnts')


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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake SummaryDoc


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
data AnomalyKind = AKEndpoint | AKShape ShapeDelta | AKField | AKFormat
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)


-- | The field paths a shape change added or re-typed\/re-formatted against the
-- prior shape. Empty for a key's first shape: a new endpoint's fields are not
-- changes to it.
data ShapeDelta = ShapeDelta {newFields :: ![Text], modifiedFields :: ![Text]}
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
fieldHashSuffix = toXXHash -- toXXHash is already exactly 8 chars


-- | Diff a (possibly absent) prior entry against the current one.
-- Order: endpoint > shape > field > format. Endpoint anomalies fire
-- only on the @HttpEndpoint@ key kind — non-HTTP keys still get shape /
-- field / format diffs, just not the "new endpoint" headline.
--
-- >>> let path = "request.body.user.id"
-- >>> let fs1 = FieldStruct (HS.fromList [FTString]) (HS.fromList ["{uuid}"]) FCRequestBody False
-- >>> let fs2 = FieldStruct (HS.fromList [FTString, FTNumber]) (HS.fromList ["{uuid}"]) FCRequestBody False
-- >>> let emptyScope = Scope Nothing Nothing Nothing Nothing Nothing Nothing V.empty
-- >>> let mk fs = CatalogEntry emptyScope (Template HttpEndpoint (HM.fromList fs)) HM.empty HM.empty 1 t0 t0 True
-- >>> let new = mk [(path, fs1)]
-- >>> map (.kind) (diffAnomalies "kh" Nothing new)
-- [AKEndpoint,AKShape (ShapeDelta {newFields = [], modifiedFields = []}),AKField,AKFormat]
-- >>> map (.kind) (diffAnomalies "kh" (Just new) new)
-- []
-- >>> -- type widened on existing field → AKShape (template hash changed) + AKFormat
-- >>> map (.kind) (diffAnomalies "kh" (Just new) (mk [(path, fs2)]))
-- [AKShape (ShapeDelta {newFields = [], modifiedFields = ["request.body.user.id"]}),AKFormat]
-- >>> -- new field added → AKShape + AKField + AKFormat
-- >>> let new2 = mk [(path, fs1), ("request.body.email", fs1)]
-- >>> map (.kind) (diffAnomalies "kh" (Just new) new2)
-- [AKShape (ShapeDelta {newFields = ["request.body.email"], modifiedFields = []}),AKField,AKFormat]
diffAnomalies :: Text -> Maybe CatalogEntry -> CatalogEntry -> [ProducedAnomaly]
diffAnomalies kh priorM cur =
  let priorFields = maybe HM.empty (.template.fields) priorM
      curFields = cur.template.fields
      -- 404 observations remain useful catalog data, but do not establish an
      -- endpoint (processSpanToEntities excludes them too). Announce the first
      -- non-404 response even if an earlier flush persisted only 404s.
      -- Older catalog entries can lack status evidence; retain their eligibility.
      endpointObserved entry =
        entry.template.keyKind
          == HttpEndpoint
          && (V.null entry.scope.statusCodes || V.any (/= 404) entry.scope.statusCodes)
      headline =
        [ ProducedAnomaly AKEndpoint kh kh Nothing
        | endpointObserved cur
        , not $ maybe False endpointObserved priorM
        ]
      shapeChanged = case priorM of
        Nothing -> True
        Just p -> templateHash p.template /= templateHash cur.template
      newPaths = HS.toList $ HS.difference (HS.fromList (HM.keys curFields)) (HS.fromList (HM.keys priorFields))
      delta = case priorM of
        Nothing -> ShapeDelta [] []
        Just _ -> ShapeDelta (sort newPaths) (sort $ filter (`HM.member` priorFields) changedFormatPaths)
      shape =
        [ ProducedAnomaly (AKShape delta) (kh <> ":s:" <> T.take 8 (templateHash cur.template)) kh Nothing
        | shapeChanged
        ]
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
  deriving (AE.FromJSON, AE.ToJSON, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "FT" FieldTypes


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
  | -- | Top-level columns on @otel_logs_and_spans@ (level, name, kind,
    -- status_code, severity_*). Emitted bare in the facet output — no
    -- @attributes.@/@resource.@ prefix.
    FCTopLevel
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "FC" FieldCategoryEnum


-- Type aliases for the legacy ID newtypes — VM types in
-- "Models.Apis.ApiChanges" still reference these.
type FieldId = UUIDId "field"


type FormatId = UUIDId "format"


type ShapeId = UUIDId "shape"


-- ---------------------------------------------------------------------------
-- Facet types (re-homed from "Models.Apis.Fields"). Kept here so callers
-- of the AI / query-editor / log-explorer stack don't need to chase the
-- migration.

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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake FacetSummary


-- Streaming hot path for schema-learning. Single-writer per shard; the
-- shard fiber calls 'observeSpans' once per ingestion batch, the flush
-- fiber (in "Models.Apis.SchemaCatalog") swaps the dirty subset.
--
-- Designed to keep per-span amortised cost flat:
--
--   * Pre-keyed: caller hands us 'ObservationInput' with @keyHash@ already
--     computed from cheap top-level fields. No JSON walk on the hot path
--     until we decide to learn.
--   * Group-by-key: a batch with N spans collapses to K distinct keys via
--     @HashMap.fromListWith (<>)@; we merge once per key, not per span.
--   * Sample-after-threshold: past 'learnFullThreshold' samples on a key,
--     subsequent observations only bump counters. Every
--     'learnSampleEveryN' spans we re-walk to detect drift.

-- | Compound key for the shard-local map. Project-qualified so a single
-- shard can own state for multiple projects (the shard router keys on
-- ProjectId, but we also defend against future re-routing).
data SchemaKey = SchemaKey
  { projectId :: !Projects.ProjectId
  , keyHash :: !Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, NFData)


-- | Single-writer shard state. The shard fiber owns it; the flush fiber
-- only reads via 'takeDirty' (an 'atomicModifyIORef'') which clears
-- 'dirtyKeys' atomically.
data SchemaShardState = SchemaShardState
  { entries :: !(HashMap SchemaKey CatalogEntry)
  , knownTemplates :: !(HS.HashSet Text)
  , dirtyKeys :: !(HS.HashSet SchemaKey)
  , bytesEstimate :: !Int
  -- ^ Running footprint estimate; drives between-tick eviction.
  , flushedSummaryHashes :: !(HashMap Projects.ProjectId Text)
  -- ^ Last-persisted SummaryDoc hash per project — skip unchanged rewrites.
  , flushedEntries :: !(HashMap SchemaKey CatalogEntry)
  -- ^ Last-persisted entries; the anomaly differ reads this instead of DB.
  }
  deriving stock (Generic)
  deriving anyclass (NFData)


emptySchemaShardState :: SchemaShardState
emptySchemaShardState = SchemaShardState HM.empty HS.empty HS.empty 0 HM.empty HM.empty


-- | Sampling policy. Defaults are conservative; tune via 'System.Config'.
data DecisionPolicy = DecisionPolicy
  { learnFullThreshold :: !Word64
  -- ^ During the first N samples for a key, every span gets a full walk.
  , learnSampleEveryN :: !Word64
  -- ^ Past 'learnFullThreshold', re-walk every N spans to refresh examples
  -- and detect drift.
  , maxKeysPerProject :: !Int
  -- ^ Eviction threshold per project (LRU by 'lastSeen').
  , maxFieldsPerEntry :: !Int
  -- ^ Hard cap on field paths per CatalogEntry. New paths beyond this are
  -- dropped silently — keeps a single noisy span from blowing memory.
  , maxBytesPerShard :: !Int
  -- ^ Approximate byte budget for the whole shard. When 'observeSpans'
  -- finishes a batch and the estimate exceeds this, 'evictLRU' fires
  -- between flush ticks instead of waiting up to 60 s.
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


defaultPolicy :: DecisionPolicy
defaultPolicy =
  DecisionPolicy
    { learnFullThreshold = 200
    , learnSampleEveryN = 200
    , maxKeysPerProject = 500
    , maxFieldsPerEntry = 2000
    , maxBytesPerShard = 67_108_864
    }


-- | Per-span keying tuple plus the leaf walk. The caller (in
-- @BackgroundJobs.processEagerBatch@) computes this from the
-- @OtelLogsAndSpans@ so this module stays free of telemetry / regex deps.
data ObservationInput = ObservationInput
  { keyKind :: !KeyKind
  , keyHash :: !Text
  , scope :: !Scope
  , walk :: () -> [(Text, V.Vector (AE.Value, Maybe Text), FieldCategoryEnum)]
  -- ^ Lazy walk thunk — only forced when 'mergeGroup' decides to learn.
  -- Bump-only spans (the 99% past 'learnFullThreshold') never pay it.
  , timestamp :: !UTCTime
  }
  deriving stock (Generic)


-- | Per-batch observation. Single 'atomicModifyIORef'' for the whole batch:
--
--   1. Group inputs by @keyHash@ within the batch (same key → one merge).
--   2. For each unique key, decide full-walk vs bump based on the existing
--      entry's 'sampleCount' and the policy.
--   3. Update entries; mark touched keys dirty.
observeSpans
  :: IORef SchemaShardState
  -> DecisionPolicy
  -> Projects.ProjectId
  -> V.Vector ObservationInput
  -> IO ()
observeSpans ref policy pid inputs
  | V.null inputs = pass
  | otherwise = do
      let groups :: HashMap Text (V.Vector ObservationInput)
          groups =
            HM.fromListWith
              (<>)
              [ (i.keyHash, V.singleton i)
              | i <- V.toList inputs
              ]
      atomicModifyIORef' ref \st ->
        let merged = HM.foldlWithKey' (\acc kh grp -> mergeGroup policy pid kh grp acc) st groups
            -- Between-tick eviction fires only when we're meaningfully over
            -- budget (20% slack), so a shard hovering near the cap doesn't
            -- pay for an O(N log N) sort on every batch.
            evictTrigger = policy.maxBytesPerShard + policy.maxBytesPerShard `div` 5
            st'
              | merged.bytesEstimate > evictTrigger = evictLRU policy merged
              | otherwise = merged
         in (st', ())


-- | Merge one (project, keyHash) group into the shard state. Decides
-- full-walk vs bump-only based on the existing entry's 'sampleCount'.
mergeGroup
  :: DecisionPolicy
  -> Projects.ProjectId
  -> Text
  -> V.Vector ObservationInput
  -> SchemaShardState
  -> SchemaShardState
mergeGroup policy pid keyHash grp st = fromMaybe st do
  rep <- grp V.!? 0
  let key = SchemaKey pid keyHash
      now = rep.timestamp
      curEntry = HM.lookup key st.entries
  -- Hard cap on total shard size: new keys are dropped once the shard
  -- holds maxEntriesPerShard entries. Existing keys always update.
  -- evictLRU enforces the finer per-project cap every flush cycle; this
  -- prevents unbounded growth in the 60-second windows between evictions.
  guard (isJust curEntry || HM.size st.entries < maxEntriesPerShard)
  let learnPhase = maybe True (\e -> e.sampleCount < policy.learnFullThreshold) curEntry
      sampleNow = case curEntry of
        Just e -> e.sampleCount `mod` policy.learnSampleEveryN == 0
        Nothing -> True
      baseEntry = fromMaybe (newEntry rep.keyKind rep.scope now) curEntry
      -- Scope is observation evidence, not sampled field data. In particular,
      -- retain a non-404 response anywhere in the batch after learning slows.
      base = baseEntry & field @"scope" .~ V.foldl' (\acc input -> mergeScope acc input.scope) baseEntry.scope grp
      n = fromIntegral (V.length grp) :: Word64
      walked =
        if learnPhase || sampleNow
          then mergeFullWalk policy.maxFieldsPerEntry rep.scope (combinedWalk grp) now base
          else base
      -- mergeFullWalk only +1's sampleCount; we represent N spans, so add
      -- the remaining (N - walks-applied). bump-only path: add N.
      addExtra = if learnPhase || sampleNow then n - 1 else n
      newEntry' =
        walked
          { sampleCount = walked.sampleCount + addExtra
          , lastSeen = now
          , dirty = True
          }
      entries' = HM.insert key newEntry' st.entries
      dirty' = HS.insert key st.dirtyKeys
      -- Bump-only spans don't change entry size — skip the recompute.
      bytes'
        | learnPhase || sampleNow =
            let oldBytes = maybe 0 entryBytes curEntry
                newBytes = entryBytes newEntry'
             in st.bytesEstimate - oldBytes + newBytes
        | otherwise = st.bytesEstimate
  pure st{entries = entries', dirtyKeys = dirty', bytesEstimate = bytes'}


-- | Combine the leaf-walks across all spans in a group: same path → values
-- concatenated. Lets us merge the whole group with a single
-- 'mergeFullWalk' call.
combinedWalk
  :: V.Vector ObservationInput
  -> [(Text, V.Vector (AE.Value, Maybe Text), FieldCategoryEnum)]
combinedWalk grp =
  let allWalks = concatMap (\i -> i.walk ()) (V.toList grp)
      step acc (path, vs, cat) = HM.insertWith concatVals path (vs, cat) acc
      concatVals (vsNew, _) (vsOld, catOld) = (vsOld <> vsNew, catOld)
      grouped = foldl' step HM.empty allWalks
   in [(p, vs, cat) | (p, (vs, cat)) <- HM.toList grouped]


-- | Atomic dirty-set swap. Returns the dirty entries cloned for the flush
-- writer; clears 'dirtyKeys' so subsequent observations re-mark fresh.
takeDirty :: IORef SchemaShardState -> IO (V.Vector (SchemaKey, CatalogEntry))
takeDirty ref =
  atomicModifyIORef' ref \st ->
    let dirty =
          V.fromList
            [ (k, e{dirty = False})
            | k <- HS.toList st.dirtyKeys
            , Just e <- [HM.lookup k st.entries]
            ]
        st' = st{dirtyKeys = HS.empty}
     in (st', dirty)


-- | Drop entries the flush writer evicted (e.g. after vacuum), and merge in
-- any newly-acknowledged template hashes so subsequent flushes can short-
-- circuit the template upsert.
pruneEvicted
  :: IORef SchemaShardState
  -> HS.HashSet SchemaKey
  -- ^ keys safe to drop from in-memory state
  -> HS.HashSet Text
  -- ^ template hashes now persisted
  -> IO ()
pruneEvicted ref droppedKeys newTemplates =
  atomicModifyIORef' ref \st ->
    let entries' = HS.foldr HM.delete st.entries droppedKeys
        flushed' = HS.foldr HM.delete st.flushedEntries droppedKeys
        known' = st.knownTemplates <> newTemplates
     in (st{entries = entries', flushedEntries = flushed', knownTemplates = known'}, ())


-- | Bound the per-project key set by evicting LRU-by-lastSeen, and cap
-- 'knownTemplates' so the dedup short-circuit set can't grow without bound.
-- 'knownTemplates' is a pure performance optimisation (skipping the upsert
-- when a template has already been persisted); dropping it just means the
-- next flush will re-upsert, which is idempotent. Pure on the shard state;
-- called from the flush worker.
evictLRU :: DecisionPolicy -> SchemaShardState -> SchemaShardState
evictLRU policy st =
  let byProject :: HashMap Projects.ProjectId [(SchemaKey, CatalogEntry)]
      byProject =
        HM.fromListWith
          (<>)
          [(k.projectId, [(k, e)]) | (k, e) <- HM.toList st.entries]
      perProjectVictims = concatMap pickVictims (HM.elems byProject)
      pickVictims xs
        | length xs <= policy.maxKeysPerProject = []
        | otherwise =
            let sorted = sortOn (\(_, e) -> e.lastSeen) xs
                excess = length xs - policy.maxKeysPerProject
             in take excess sorted
      perProjBytes = sum [entryBytes e | (_, e) <- perProjectVictims]
      afterPerProject = HS.foldr HM.delete st.entries (HS.fromList (fst <$> perProjectVictims))
      bytesAfterPerProj = st.bytesEstimate - perProjBytes
      -- Byte-budget pass: if a single high-cardinality project is still
      -- over budget after the per-project LRU, drop globally-oldest
      -- entries until we fit. This is what stops it from exhausting the
      -- shard between flush ticks.
      (entries', bytesFinal) =
        if bytesAfterPerProj <= policy.maxBytesPerShard
          then (afterPerProject, bytesAfterPerProj)
          else evictUntilUnderBudget policy.maxBytesPerShard afterPerProject bytesAfterPerProj
      -- Evicted keys also drop from the flushed-state snapshot. If a key
      -- comes back later it'll be treated as new — same correctness as
      -- after a process restart, and bounded memory.
      droppedKeys =
        HS.difference
          (HS.fromList (HM.keys st.entries))
          (HS.fromList (HM.keys entries'))
      flushed' = HS.foldr HM.delete st.flushedEntries droppedKeys
      known' = if HS.size st.knownTemplates > knownTemplatesCap then HS.empty else st.knownTemplates
   in st
        { entries = entries'
        , flushedEntries = flushed'
        , knownTemplates = known'
        , bytesEstimate = bytesFinal
        }


-- | Greedily drop globally-oldest entries (by 'lastSeen') until the
-- aggregate byte estimate is under @target@. Linear in entries dropped.
evictUntilUnderBudget
  :: Int
  -> HashMap SchemaKey CatalogEntry
  -> Int
  -> (HashMap SchemaKey CatalogEntry, Int)
evictUntilUnderBudget target entries bytes0 =
  let sorted = sortOn (\(_, e) -> e.lastSeen) (HM.toList entries)
      go acc bytes ((k, e) : rest)
        | bytes <= target = (acc, bytes)
        | otherwise = go (HM.delete k acc) (bytes - entryBytes e) rest
      go acc bytes [] = (acc, bytes)
   in go entries bytes0 sorted


-- | Cheap byte-cost estimator for one entry. Constants are heuristic — we
-- only need an order-of-magnitude approximation to drive eviction.
entryBytes :: CatalogEntry -> Int
entryBytes e =
  let bytesPerField = 80 -- FieldStruct: ~3 HashSets + enum + bool, no big strings
      bytesPerExample = 64 -- AE.Value pointer + small string sample
      bytesPerTop = 48 -- (Text, Word64) entry
      fldSize = HM.size e.template.fields * bytesPerField
      vdSize = HM.foldl' (\acc (Examples vs) -> acc + V.length vs * bytesPerExample) 0 e.valuesDelta
      ctSize = HM.foldl' (\acc tk -> acc + HM.size tk.top * bytesPerTop) 0 e.counts
   in 256 + fldSize + vdSize + ctSize


-- | Read-only snapshot of the last-persisted state. Lets the flush worker
-- compute anomaly diffs and skip-unchanged summary writes without touching
-- Postgres for state we already hold in memory.
snapshotForFlush
  :: IORef SchemaShardState
  -> IO (HashMap SchemaKey CatalogEntry, HashMap Projects.ProjectId Text)
snapshotForFlush ref = do
  st <- readIORef ref
  pure (st.flushedEntries, st.flushedSummaryHashes)


-- | After a successful flush write, fold the newly-persisted entries +
-- summary-doc hashes into the shard's snapshot. Subsequent flushes reuse
-- these so the anomaly differ doesn't requery and the summary upsert can
-- skip projects whose doc hash is unchanged.
commitFlushedSnapshot
  :: IORef SchemaShardState
  -> V.Vector (SchemaKey, CatalogEntry)
  -> HashMap Projects.ProjectId Text
  -> IO ()
commitFlushedSnapshot ref persistedEntries newSummaryHashes =
  atomicModifyIORef' ref \st ->
    let entries' = V.foldl' (\m (k, e) -> HM.insert k e m) st.flushedEntries persistedEntries
        hashes' = HM.union newSummaryHashes st.flushedSummaryHashes
     in (st{flushedEntries = entries', flushedSummaryHashes = hashes'}, ())


-- | Hard cap on the per-shard 'knownTemplates' short-circuit set. When
-- exceeded the set is dropped wholesale on the next eviction tick; the next
-- flush re-upserts templates idempotently.
knownTemplatesCap :: Int
knownTemplatesCap = 50_000


-- | Hard cap on total entries in a single shard. New keys are dropped once
-- this limit is reached; existing keys always update. evictLRU applies the
-- finer per-project cap on each flush cycle, but this O(1) guard prevents
-- unbounded growth between 60-second eviction ticks.
maxEntriesPerShard :: Int
maxEntriesPerShard = 10_000
