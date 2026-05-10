{-# LANGUAGE OverloadedRecordDot #-}

-- | Hasql persistence layer for the schema-learning catalog.
--
-- Three tables (see migration @0089_schema_catalog@):
--
--   * @apis.schema_template@ — instance-wide, structure-only, dedup'd by hash.
--   * @apis.schema_catalog@  — per-project, references a template + carries
--     tenant-private bits.
--   * @apis.schema_summary@  — materialised per-project AI/query-editor doc.
--
-- @FacetData@ / @FacetValue@ are re-exported from "Models.Apis.Fields" so
-- existing callers of @Fields.getFacetSummary@ can be redirected here without
-- changing their imports.
module Models.Apis.SchemaCatalog (
  TemplateRow (..),
  CatalogRow (..),
  upsertTemplates,
  upsertCatalogRows,
  getByProject,
  getByHost,
  getByKey,
  getSummary,
  upsertSummary,
  vacuumUnreferencedTemplates,
  toFacetSummary,
  getFacetSummary,
  -- Re-exports for reader migration.
  Catalog.FacetData (..),
  Catalog.FacetValue (..),
  Catalog.FacetSummary (..),
)
where

import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (DB)
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Relude


-- ---------------------------------------------------------------------------
-- Row shapes for inserts/lookups.

-- | One row in @apis.schema_template@. Templates are immutable once written:
-- any structural change mints a new 'Catalog.templateHash'.
data TemplateRow = TemplateRow
  { templateHash :: !Text
  , keyKind :: !Catalog.KeyKind
  , fields :: !(HM.HashMap Text Catalog.FieldStruct)
  , lastSeenAt :: !UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | One row in @apis.schema_catalog@. Holds the tenant-private bits + a
-- pointer to the shared template row.
data CatalogRow = CatalogRow
  { projectId :: !Projects.ProjectId
  , keyKind :: !Catalog.KeyKind
  , keyHash :: !Text
  , templateHash :: !Text
  , scope :: !Catalog.Scope
  , valuesDelta :: !(HM.HashMap Text Catalog.Examples)
  , counts :: !(HM.HashMap Text Catalog.TopK)
  , sampleCount :: !Word64
  , firstSeen :: !UTCTime
  , lastSeen :: !UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- ---------------------------------------------------------------------------
-- Upserts. Both are multi-row INSERTs over @unnest@ so a single round-trip
-- handles the dirty subset of an entire shard.

-- | Insert templates, bumping @last_seen_at@ on hits. Idempotent.
upsertTemplates :: DB es => V.Vector TemplateRow -> Eff es ()
upsertTemplates rows | V.null rows = pass
upsertTemplates rows =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.schema_template (template_hash, key_kind, fields, last_seen_at)
             SELECT * FROM unnest(
               #{hashes}::text[],
               #{kinds}::apis.schema_key_kind[],
               #{fieldsJson}::jsonb[],
               #{seens}::timestamptz[])
             ON CONFLICT (template_hash) DO UPDATE
             SET last_seen_at = GREATEST(apis.schema_template.last_seen_at, EXCLUDED.last_seen_at) |]
  where
    hashes = V.map (.templateHash) rows
    kinds = V.map (.keyKind) rows
    fieldsJson = V.map (HI.AsJsonb . (.fields)) rows
    seens = V.map (.lastSeenAt) rows


-- | Upsert catalog rows. Replaces values_delta/counts/scope wholesale —
-- callers (the flush worker) merge in-memory before writing, so the SQL is a
-- plain assignment.
upsertCatalogRows :: DB es => V.Vector CatalogRow -> Eff es ()
upsertCatalogRows rows | V.null rows = pass
upsertCatalogRows rows =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.schema_catalog
               (project_id, key_kind, key_hash, template_hash, scope,
                values_delta, counts, sample_count, first_seen, last_seen, updated_at)
             SELECT *, now() FROM unnest(
               #{pids}::uuid[],
               #{kinds}::apis.schema_key_kind[],
               #{khs}::text[],
               #{ths}::text[],
               #{scopes}::jsonb[],
               #{vds}::jsonb[],
               #{cnts}::jsonb[],
               #{ss}::bigint[],
               #{firsts}::timestamptz[],
               #{lasts}::timestamptz[])
             ON CONFLICT (project_id, key_hash) DO UPDATE
             SET template_hash = EXCLUDED.template_hash,
                 scope         = EXCLUDED.scope,
                 values_delta  = EXCLUDED.values_delta,
                 counts        = EXCLUDED.counts,
                 sample_count  = EXCLUDED.sample_count,
                 last_seen     = GREATEST(apis.schema_catalog.last_seen, EXCLUDED.last_seen),
                 updated_at    = now() |]
  where
    pids = V.map (.projectId) rows
    kinds = V.map (.keyKind) rows
    khs = V.map (.keyHash) rows
    ths = V.map (.templateHash) rows
    scopes = V.map (HI.AsJsonb . (.scope)) rows
    vds = V.map (HI.AsJsonb . (.valuesDelta)) rows
    cnts = V.map (HI.AsJsonb . (.counts)) rows
    ss = V.map (fromIntegral @Word64 @Int64 . (.sampleCount)) rows
    firsts = V.map (.firstSeen) rows
    lasts = V.map (.lastSeen) rows


-- ---------------------------------------------------------------------------
-- Lookups.

-- | Decoded result of a join across @apis.schema_catalog@ ⨝ @apis.schema_template@.
data CatalogReadRow = CatalogReadRow
  { projectId :: UUID.UUID
  , keyKind :: Catalog.KeyKind
  , keyHash :: Text
  , templateHash :: Text
  , scope :: HI.AsJsonb Catalog.Scope
  , templateFields :: HI.AsJsonb (HM.HashMap Text Catalog.FieldStruct)
  , valuesDelta :: HI.AsJsonb (HM.HashMap Text Catalog.Examples)
  , counts :: HI.AsJsonb (HM.HashMap Text Catalog.TopK)
  , sampleCount :: Int64
  , firstSeen :: UTCTime
  , lastSeen :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


readRowToEntry :: CatalogReadRow -> Catalog.CatalogEntry
readRowToEntry r =
  let HI.AsJsonb sc = r.scope
      HI.AsJsonb tf = r.templateFields
      HI.AsJsonb vd = r.valuesDelta
      HI.AsJsonb ct = r.counts
   in Catalog.CatalogEntry
        { scope = sc
        , template = Catalog.Template r.keyKind tf
        , valuesDelta = vd
        , counts = ct
        , sampleCount = fromIntegral r.sampleCount
        , firstSeen = r.firstSeen
        , lastSeen = r.lastSeen
        , dirty = False
        }


-- | All catalog rows for a project, ordered by most-recently-seen.
getByProject :: DB es => Projects.ProjectId -> Eff es (V.Vector Catalog.CatalogEntry)
getByProject pid = do
  rows :: [CatalogReadRow] <-
    Hasql.interp
      [HI.sql| SELECT c.project_id, c.key_kind, c.key_hash, c.template_hash,
                      c.scope, t.fields, c.values_delta, c.counts,
                      c.sample_count, c.first_seen, c.last_seen
               FROM apis.schema_catalog c
               JOIN apis.schema_template t ON c.template_hash = t.template_hash
               WHERE c.project_id = #{pid}
               ORDER BY c.last_seen DESC |]
  pure $ V.fromList $ readRowToEntry <$> rows


-- | Catalog rows for a project filtered to one host (HTTP keys only).
getByHost :: DB es => Projects.ProjectId -> Text -> Eff es (V.Vector Catalog.CatalogEntry)
getByHost pid host = do
  rows :: [CatalogReadRow] <-
    Hasql.interp
      [HI.sql| SELECT c.project_id, c.key_kind, c.key_hash, c.template_hash,
                      c.scope, t.fields, c.values_delta, c.counts,
                      c.sample_count, c.first_seen, c.last_seen
               FROM apis.schema_catalog c
               JOIN apis.schema_template t ON c.template_hash = t.template_hash
               WHERE c.project_id = #{pid}
                 AND c.key_kind = 'http_endpoint'::apis.schema_key_kind
                 AND c.scope->>'host' = #{host}
               ORDER BY c.last_seen DESC |]
  pure $ V.fromList $ readRowToEntry <$> rows


-- | One catalog row by primary key.
getByKey :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Catalog.CatalogEntry)
getByKey pid keyHash =
  fmap readRowToEntry
    <$> Hasql.interpOne
      [HI.sql| SELECT c.project_id, c.key_kind, c.key_hash, c.template_hash,
                      c.scope, t.fields, c.values_delta, c.counts,
                      c.sample_count, c.first_seen, c.last_seen
               FROM apis.schema_catalog c
               JOIN apis.schema_template t ON c.template_hash = t.template_hash
               WHERE c.project_id = #{pid} AND c.key_hash = #{keyHash} |]


-- ---------------------------------------------------------------------------
-- Summary doc.

newtype SummaryRow = SummaryRow {doc :: HI.AsJsonb Catalog.SummaryDoc}
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


-- | Read the materialised AI/query-editor doc for a project.
getSummary :: DB es => Projects.ProjectId -> Eff es (Maybe Catalog.SummaryDoc)
getSummary pid =
  fmap unwrap
    <$> Hasql.interpOne
      [HI.sql| SELECT doc FROM apis.schema_summary WHERE project_id = #{pid} |]
  where
    unwrap (SummaryRow (HI.AsJsonb d)) = d


upsertSummary :: DB es => Projects.ProjectId -> Catalog.SummaryDoc -> Eff es ()
upsertSummary pid doc = do
  let docJson = HI.AsJsonb doc
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.schema_summary (project_id, doc, generated_at)
             VALUES (#{pid}, #{docJson}, now())
             ON CONFLICT (project_id) DO UPDATE
             SET doc = EXCLUDED.doc, generated_at = now() |]


-- | Drop-in replacement for the legacy @Fields.getFacetSummary@: same
-- type signature; @tableName@ and the time range are accepted for source-
-- compat but ignored (schema is now unified per project, and counts come
-- from in-memory state rather than scaled-by-time-range warehouse scans).
getFacetSummary
  :: DB es
  => Projects.ProjectId
  -> Text
  -> UTCTime
  -> UTCTime
  -> Eff es (Maybe Catalog.FacetSummary)
getFacetSummary pid tableName _from _to =
  fmap (toFacetSummary pid tableName) <$> getSummary pid


-- | Adapter: convert a 'Catalog.SummaryDoc' into the legacy
-- 'Catalog.FacetSummary' shape so existing callers (AI prompt, query editor)
-- don't need to change. The @tableName@ argument is ignored — schema is now
-- unified per project.
toFacetSummary :: Projects.ProjectId -> Text -> Catalog.SummaryDoc -> Catalog.FacetSummary
toFacetSummary pid tableName doc =
  Catalog.FacetSummary
    { id = UUID.nil -- summary is not row-identified; legacy callers don't depend on this
    , projectId = pid.toText
    , tableName = tableName
    , facetJson = Catalog.FacetData $ HM.map topKToFacetValues doc.topValuesByField
    }
  where
    topKToFacetValues :: Catalog.TopK -> [Catalog.FacetValue]
    topKToFacetValues tk =
      sortOn
        (negate . (.count))
        [ Catalog.FacetValue v (fromIntegral n) | (v, n) <- HM.toList tk.top
        ]


-- ---------------------------------------------------------------------------
-- GC.

-- | Drop @apis.schema_template@ rows no catalog row references and that
-- haven't been seen in 7 days (grace window for shards that just evicted but
-- haven't flushed yet). Returns rows deleted.
vacuumUnreferencedTemplates :: DB es => Eff es Int64
vacuumUnreferencedTemplates =
  Hasql.interpExecute
    [HI.sql| DELETE FROM apis.schema_template t
             WHERE t.last_seen_at < now() - interval '7 days'
               AND NOT EXISTS (
                 SELECT 1 FROM apis.schema_catalog c
                 WHERE c.template_hash = t.template_hash) |]
