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
  getByKeysBatch,
  endpointCatalog,
  getSummary,
  upsertSummary,
  freshSummaryProjects,
  toFacetSummary,
  getFacetSummary,
  vacuumUnreferencedTemplates,
  -- Anomaly producer support.
  AnomalyInsertRow (..),
  insertAnomalies,
  enqueueAnomalyJobs,
  existingEndpointHashes,
  -- Re-exports for reader migration.
  SchemaLearning.FacetData (..),
  SchemaLearning.FacetValue (..),
  SchemaLearning.FacetSummary (..),

  -- * Flush worker
  FlushResult (..),
  flushDirty,
  runSchemaFlusher,
  buildAnomalyRows,
)
where

import Control.Concurrent (threadDelay)
import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful
import Effectful.Exception (trySync)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Apis.ApiChanges (AnomalyTypes, FieldEvidence)
import Models.Apis.ApiChanges qualified as ApiChanges
import Models.Ids qualified as Projects
import Pkg.DeriveUtils (DB, UUIDId (..))
import Pkg.SchemaLearning (CatalogEntry, SchemaKey, SchemaShardState, SummaryDoc (..), TopK)
import Pkg.SchemaLearning qualified as SchemaLearning
import Relude
import Utils (scrubNulValue, toXXHash)


-- ---------------------------------------------------------------------------
-- Row shapes for inserts/lookups.

-- | One row in @apis.schema_template@. Templates are immutable once written:
-- any structural change mints a new 'SchemaLearning.templateHash'.
data TemplateRow = TemplateRow
  { templateHash :: !Text
  , keyKind :: !SchemaLearning.KeyKind
  , fields :: !(HM.HashMap Text SchemaLearning.FieldStruct)
  , lastSeenAt :: !UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | One row in @apis.schema_catalog@. Holds the tenant-private bits + a
-- pointer to the shared template row.
data CatalogRow = CatalogRow
  { projectId :: !Projects.ProjectId
  , keyKind :: !SchemaLearning.KeyKind
  , keyHash :: !Text
  , templateHash :: !Text
  , scope :: !SchemaLearning.Scope
  , valuesDelta :: !(HM.HashMap Text SchemaLearning.Examples)
  , counts :: !(HM.HashMap Text SchemaLearning.TopK)
  , sampleCount :: !Word64
  , firstSeen :: !UTCTime
  , lastSeen :: !UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | JSON-encode then strip NULs (PG @jsonb@ rejects @\\u0000@) before wrapping
-- for a jsonb column. Shared by every jsonb-bearing upsert below.
asScrubbedJsonb :: AE.ToJSON a => a -> HI.AsJsonb AE.Value
asScrubbedJsonb = HI.AsJsonb . scrubNulValue . AE.toJSON


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
    -- Scrubbed like every sibling jsonb write: one NUL in a field path would
    -- otherwise 22P05-poison the whole batch. The template_hash is computed over
    -- the unscrubbed structure, which is fine — unscrubbed rows never stored.
    fieldsJson = V.map (asScrubbedJsonb . (.fields)) rows
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
    scopes = V.map (asScrubbedJsonb . (.scope)) rows
    vds = V.map (asScrubbedJsonb . (.valuesDelta)) rows
    cnts = V.map (asScrubbedJsonb . (.counts)) rows
    ss = V.map (fromIntegral @Word64 @Int64 . (.sampleCount)) rows
    firsts = V.map (.firstSeen) rows
    lasts = V.map (.lastSeen) rows


-- ---------------------------------------------------------------------------
-- Lookups.

-- | Decoded result of a join across @apis.schema_catalog@ ⨝ @apis.schema_template@.
data CatalogReadRow = CatalogReadRow
  { projectId :: UUID
  , keyKind :: SchemaLearning.KeyKind
  , keyHash :: Text
  , templateHash :: Text
  , scope :: HI.AsJsonb SchemaLearning.Scope
  , templateFields :: HI.AsJsonb (HM.HashMap Text SchemaLearning.FieldStruct)
  , valuesDelta :: HI.AsJsonb (HM.HashMap Text SchemaLearning.Examples)
  , counts :: HI.AsJsonb (HM.HashMap Text SchemaLearning.TopK)
  , sampleCount :: Int64
  , firstSeen :: UTCTime
  , lastSeen :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


readRowToEntry :: CatalogReadRow -> SchemaLearning.CatalogEntry
readRowToEntry r =
  SchemaLearning.CatalogEntry
    { scope = coerce r.scope
    , template = SchemaLearning.Template r.keyKind (coerce r.templateFields)
    , valuesDelta = coerce r.valuesDelta
    , counts = coerce r.counts
    , sampleCount = fromIntegral r.sampleCount
    , firstSeen = r.firstSeen
    , lastSeen = r.lastSeen
    , dirty = False
    }


-- | Bulk variant: fetches catalog rows for a heterogeneous (project, key_hash)
-- set in one round-trip. Used by the anomaly producer to load priors for an
-- entire dirty batch before diffing.
getByKeysBatch
  :: DB es
  => V.Vector (Projects.ProjectId, Text)
  -> Eff es (HM.HashMap (Projects.ProjectId, Text) SchemaLearning.CatalogEntry)
getByKeysBatch pairs
  | V.null pairs = pure HM.empty
  | otherwise = do
      let (pids, khs) = V.unzip pairs
      rows :: [CatalogReadRow] <-
        Hasql.interp
          [HI.sql| SELECT c.project_id, c.key_kind, c.key_hash, c.template_hash,
                          c.scope, t.fields, c.values_delta, c.counts,
                          c.sample_count, c.first_seen, c.last_seen
                   FROM apis.schema_catalog c
                   JOIN apis.schema_template t ON c.template_hash = t.template_hash
                   JOIN unnest(#{pids}::uuid[], #{khs}::text[]) m(pid, kh)
                     ON c.project_id = m.pid AND c.key_hash = m.kh |]
      pure $ HM.fromList [((UUIDId r.projectId, r.keyHash), readRowToEntry r) | r <- rows]


-- | The learned HTTP endpoints of one host — or one endpoint, when
-- @keyHashM@ is given — most-recently-seen first. Feeds "Pkg.OpenApi" and the
-- api_catalog schema views. @kindM@ separates the two directions a host can
-- carry (@server@ = incoming, @client@ = outgoing).
--
-- The cap keeps a pathological host (a WordPress site with thousands of
-- learned routes) from building a spec no browser can render; callers surface
-- the truncation.
endpointCatalog
  :: DB es
  => Projects.ProjectId
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Int
  -> Eff es (V.Vector (Text, SchemaLearning.CatalogEntry))
endpointCatalog pid hostM kindM keyHashM limit = do
  rows :: [CatalogReadRow] <-
    Hasql.interp
      [HI.sql| SELECT c.project_id, c.key_kind, c.key_hash, c.template_hash,
                      c.scope, t.fields, c.values_delta, c.counts,
                      c.sample_count, c.first_seen, c.last_seen
               FROM apis.schema_catalog c
               JOIN apis.schema_template t ON c.template_hash = t.template_hash
               WHERE c.project_id = #{pid}
                 AND c.key_kind = 'http_endpoint'
                 AND (#{hostM}::text IS NULL OR c.scope->>'host' = #{hostM}::text)
                 AND (#{kindM}::text IS NULL OR c.scope->>'kind' = #{kindM}::text)
                 AND (#{keyHashM}::text IS NULL OR c.key_hash = #{keyHashM}::text)
               ORDER BY c.last_seen DESC
               LIMIT #{fromIntegral limit :: Int64} |]
  pure $ V.fromList [(r.keyHash, readRowToEntry r) | r <- rows]


-- ---------------------------------------------------------------------------
-- Summary doc.

newtype SummaryRow = SummaryRow {doc :: HI.AsJsonb SchemaLearning.SummaryDoc}
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


-- | Read the materialised AI/query-editor doc for a project. @Just keys@ narrows
-- @fields@/@top_values_by_field@ to those bare keys in SQL: the full doc runs to
-- megabytes (7k+ fields), and page shells only need a handful of them. The lateral
-- detoasts each sub-object once instead of once per key lookup.
getSummary :: DB es => Maybe (V.Vector Text) -> Projects.ProjectId -> Eff es (Maybe SchemaLearning.SummaryDoc)
getSummary keysM pid =
  fmap (\(SummaryRow (HI.AsJsonb d)) -> d) <$> Hasql.interpOne case keysM of
    Nothing -> [HI.sql| SELECT doc FROM apis.schema_summary WHERE project_id = #{pid} |]
    Just keys ->
      [HI.sql| SELECT jsonb_build_object('services', x.s,
                 'fields', (SELECT coalesce(jsonb_object_agg(k, x.f->k), '{}') FROM unnest(#{keys}::text[]) k WHERE x.f ? k),
                 'top_values_by_field', (SELECT coalesce(jsonb_object_agg(k, x.t->k), '{}') FROM unnest(#{keys}::text[]) k WHERE x.t ? k))
               FROM apis.schema_summary,
                 LATERAL (SELECT doc->'services' s, doc->'fields' f, doc->'top_values_by_field' t OFFSET 0) x
               WHERE project_id = #{pid} |]


-- | Batched per-project summary upsert. One round trip regardless of how many
-- projects were touched. An empty vector is a no-op (no SQL emitted).
--
-- Two failure modes this guards against, both seen in the 2026-05-30 outage:
-- (a) NUL bytes in observed values render as the valid JSON escape @\\u0000@,
-- which PG @jsonb@ rejects with @22P05@, poisoning the entire @unnest@ batch;
-- (b) one rogue project then blocks every other project's summary write until
-- the bad value ages out (it never does — the in-memory shard keeps it
-- forever). 'scrubNulValue' strips NULs throughout the doc; if the sanitised
-- batch still fails, we fall back to per-row inserts so a single hostile
-- project can't block the rest.
upsertSummary :: DB es => V.Vector (Projects.ProjectId, SchemaLearning.SummaryDoc) -> Eff es ()
upsertSummary rows0 = unless (V.null rows0) $ do
  let rows = V.map (second asScrubbedJsonb) rows0
  whenLeftM_ (trySync (batch rows)) \_ -> V.forM_ rows (void . trySync . batch . V.singleton)
  where
    batch xs =
      let (pids, docs) = V.unzip xs
       in Hasql.interpExecute_
            [HI.sql| INSERT INTO apis.schema_summary (project_id, doc, generated_at)
                     SELECT *, now() FROM unnest(
                       #{pids}::uuid[],
                       #{docs}::jsonb[])
                     ON CONFLICT (project_id) DO UPDATE
                     SET doc = EXCLUDED.doc, generated_at = EXCLUDED.generated_at |]


-- | Of the supplied projects, returns those whose @apis.schema_summary@ was
-- updated within the last 30 s. The window coalesces many flush passes on
-- noisy projects into one write; shorter than the flush interval and well
-- within facet-UI staleness tolerance.
freshSummaryProjects
  :: DB es
  => V.Vector Projects.ProjectId
  -> Eff es (HS.HashSet Projects.ProjectId)
freshSummaryProjects pids
  | V.null pids = pure HS.empty
  | otherwise = do
      rows :: [UUID] <-
        Hasql.interp
          [HI.sql| SELECT project_id FROM apis.schema_summary
                   WHERE project_id = ANY(#{pids}::uuid[])
                     AND generated_at > now() - interval '30 seconds' |]
      pure $ HS.fromList (UUIDId <$> rows)


-- | Drop-in replacement for the legacy @Fields.getFacetSummary@: same
-- type signature; @tableName@ and the time range are accepted for source-
-- compat but ignored (schema is now unified per project, and counts come
-- from in-memory state rather than scaled-by-time-range warehouse scans).
--
-- @Just paths@ fetches only the entries that can surface as those facet paths.
getFacetSummary
  :: DB es
  => Maybe [Text]
  -> Projects.ProjectId
  -> Text
  -> UTCTime
  -> UTCTime
  -> Eff es (Maybe SchemaLearning.FacetSummary)
getFacetSummary pathsM pid tableName _from _to =
  fmap (toFacetSummary pid tableName) <$> getSummary (facetBareKeys <$> pathsM) pid


-- | Every bare doc key that some category prefix maps onto one of these facet paths.
--
-- >>> sort $ V.toList $ facetBareKeys ["resource.service.name", "attributes.http.request.header.host"]
-- ["attributes.http.request.header.host","host","http.request.header.host","resource.service.name","service.name"]
facetBareKeys :: [Text] -> V.Vector Text
facetBareKeys paths = V.fromList $ ordNub [k | p <- paths, cat <- universe, Just k <- [T.stripPrefix (categoryPrefix cat) p]]


-- | Adapter: convert a 'SchemaLearning.SummaryDoc' into the legacy
-- 'SchemaLearning.FacetSummary' shape so existing callers (AI prompt, query editor)
-- don't need to change. The @tableName@ argument is ignored — schema is now
-- unified per project.
--
-- Catalog 'topValuesByField' paths are bare (e.g. @http.request.method@)
-- because each walk strips its section. To match the KQL parser's
-- 'flattenedOtelAttributes' whitelist (and therefore compile to a flat-column
-- scan), we prefix each path with its field category: attributes → @attributes.@,
-- resource → @resource.@, event → @events.@, body/header/param walks get their
-- standard OTel prefixes. Result keys line up with what users type in KQL.
toFacetSummary :: Projects.ProjectId -> Text -> SchemaLearning.SummaryDoc -> SchemaLearning.FacetSummary
toFacetSummary pid tableName doc =
  SchemaLearning.FacetSummary
    { id = UUID.nil -- summary is not row-identified; legacy callers don't depend on this
    , projectId = pid.toText
    , tableName = tableName
    , facetJson =
        SchemaLearning.FacetData
          $ HM.fromList
            [ ( categoryPrefix (maybe SchemaLearning.FCAttribute (.category) (HM.lookup path doc.fields)) <> path
              , sortOn (Down . (.count)) [SchemaLearning.FacetValue v (fromIntegral n :: Int) | (v, n) <- HM.toList tk.top]
              )
            | (path, tk) <- HM.toList doc.topValuesByField
            ]
    }


-- 'FacetData' has two consumers: the Log Explorer sidebar (only renders
-- entries that match 'facetDefs', all of which are in
-- 'flattenedOtelAttributes') and the AI prompt context, which wants the
-- full shape including request/response body keys. So
-- @body.request.*@ / @body.response.*@ keys are intentionally emitted
-- here even though they're unreachable from the sidebar.
categoryPrefix :: SchemaLearning.FieldCategoryEnum -> Text
categoryPrefix = \case
  SchemaLearning.FCAttribute -> "attributes."
  SchemaLearning.FCResource -> "resource."
  SchemaLearning.FCEvent -> "events."
  SchemaLearning.FCRequestHeader -> "attributes.http.request.header."
  SchemaLearning.FCResponseHeader -> "attributes.http.response.header."
  SchemaLearning.FCQueryParam -> "attributes.url.query."
  SchemaLearning.FCPathParam -> "attributes.url.path_param."
  SchemaLearning.FCRequestBody -> "body.request."
  SchemaLearning.FCResponseBody -> "body.response."
  SchemaLearning.FCTopLevel -> "" -- bare flat column (e.g. "level", "name", "severity.severity_text")


-- ---------------------------------------------------------------------------
-- GC.

-- | Drop @apis.schema_template@ rows no catalog row references and that
-- haven't been seen in 40 days. Long grace window covers projects that
-- paused ingestion (vacation, staging idle) so we don't churn templates
-- they'd re-create on return. Returns rows deleted. Run from MonoscopeAdminDaily.
vacuumUnreferencedTemplates :: DB es => Eff es Int64
vacuumUnreferencedTemplates =
  Hasql.interpExecute
    [HI.sql| DELETE FROM apis.schema_template t
             WHERE t.last_seen_at < now() - interval '40 days'
               AND NOT EXISTS (
                 SELECT 1 FROM apis.schema_catalog c
                 WHERE c.template_hash = t.template_hash) |]


-- ---------------------------------------------------------------------------
-- Anomaly producer.

-- | Row shape for bulk inserts into @apis.anomalies@. 'method', 'host', and
-- 'urlPath' are only populated for endpoint anomalies; they let the read
-- side render notifications without depending on a successful join back to
-- @apis.endpoints@ (see migration 0092).
data AnomalyInsertRow = AnomalyInsertRow
  { projectId :: !Projects.ProjectId
  , anomalyType :: !AnomalyTypes
  , targetHash :: !Text
  , method :: !(Maybe Text)
  , host :: !(Maybe Text)
  , urlPath :: !(Maybe Text)
  , fieldEvidence :: !(V.Vector FieldEvidence)
  , delta :: !SchemaLearning.ShapeDelta
  -- ^ only non-empty on shape anomalies
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | Of the given @(project_id, endpoint_hash)@ pairs, return those already
-- present in @apis.endpoints@ /and at least 5 minutes old/. Used by
-- @BackgroundJobs.processAPIChangeAnomalies@ to gate the "new endpoint"
-- notification — the issue is always created, only the send is suppressed.
--
-- Endpoint newness is owned by @apis.endpoints@ (the canonical registry),
-- not the in-memory schema catalog (which is empty for fresh projects and
-- gets pruned by evictLRU). The grace window matters: every batch goes
-- through 'processSpanToEntities' (writes @apis.endpoints@) and
-- 'extractObservation' (feeds the catalog) in parallel; the schema flush
-- runs ~'schemaFlushIntervalSecs' later, by which time a genuinely new
-- endpoint is seconds-to-a-minute old. Without the cutoff we'd suppress
-- those as "already known" and notifications for real new endpoints would
-- never fire. With the cutoff, only endpoints discovered well before this
-- flush cycle are treated as not-new.
existingEndpointHashes
  :: DB es
  => V.Vector (Projects.ProjectId, Text)
  -> Eff es (HS.HashSet (Projects.ProjectId, Text))
existingEndpointHashes pairs
  | V.null pairs = pure HS.empty
  | otherwise = do
      let (pids, hashes) = V.unzip pairs
      rows :: [(UUID, Text)] <-
        Hasql.interp
          [HI.sql| SELECT e.project_id, e.hash
                   FROM apis.endpoints e
                   JOIN unnest(#{pids}::uuid[], #{hashes}::text[]) m(pid, h)
                     ON e.project_id = m.pid AND e.hash = m.h
                   WHERE e.created_at < now() - interval '5 minutes' |]
      pure $ HS.fromList (first UUIDId <$> rows)


-- | Bulk-insert anomalies. The unique @(project_id, target_hash)@ index
-- de-duplicates across flush passes — we rely on it to avoid maintaining a
-- separate "already-emitted" set on the hot path. Returns rows actually
-- inserted (excludes ON CONFLICT collisions).
insertAnomalies :: DB es => V.Vector AnomalyInsertRow -> Eff es Int64
insertAnomalies rows | V.null rows = pure 0
insertAnomalies rows =
  Hasql.interpExecute
    [HI.sql| INSERT INTO apis.anomalies (project_id, anomaly_type, action, target_hash, method, host, url_path, new_fields, modified_fields, field_evidence)
             SELECT pid, atype::apis.anomaly_type, 'created'::apis.anomaly_action, th, mth, hst, up,
                    ARRAY(SELECT jsonb_array_elements_text(nf)), ARRAY(SELECT jsonb_array_elements_text(mf)), evidence
             FROM unnest(#{pids}::uuid[], #{atypes}::text[], #{ths}::text[], #{methods}::text[], #{hosts}::text[], #{paths}::text[], #{nfs}::jsonb[], #{mfs}::jsonb[], #{evidences}::jsonb[])
                    AS m(pid, atype, th, mth, hst, up, nf, mf, evidence)
             ON CONFLICT (project_id, target_hash) DO NOTHING |]
  where
    pids = V.map (.projectId) rows
    atypes = V.map (display . (.anomalyType)) rows
    ths = V.map (.targetHash) rows
    methods = V.map (.method) rows
    hosts = V.map (.host) rows
    paths = V.map (.urlPath) rows
    nfs = V.map (asScrubbedJsonb . (.delta.newFields)) rows
    mfs = V.map (asScrubbedJsonb . (.delta.modifiedFields)) rows
    evidences = V.map (asScrubbedJsonb . (.fieldEvidence)) rows


-- | Enqueue one @NewAnomaly@ background job per (project, anomalyType) group
-- so the existing notification fan-out (legacy @new_anomaly_proc@'s job
-- emitter) keeps firing. Coalesces with an already-queued job for the same
-- group, mirroring the legacy behaviour.
enqueueAnomalyJobs :: DB es => V.Vector AnomalyInsertRow -> Eff es ()
enqueueAnomalyJobs rows | V.null rows = pass
enqueueAnomalyJobs rows = do
  let groups :: HM.HashMap (Projects.ProjectId, Text) [Text]
      groups = HM.fromListWith (<>) [((r.projectId, display r.anomalyType), [r.targetHash]) | r <- V.toList rows]
  forM_ (HM.toList groups) \((pid, atype), ths) -> do
    let payload = V.fromList ths
    Hasql.interpExecute_
      [HI.sql|
        WITH existing AS (
          SELECT id, payload->'targetHashes' AS ths
          FROM background_jobs
          WHERE payload->>'tag' = 'NewAnomaly'
            AND payload->>'projectId' = #{pid}::text
            AND payload->>'anomalyType' = #{atype}
            AND status = 'queued'
          ORDER BY run_at ASC LIMIT 1
        ),
        upd AS (
          UPDATE background_jobs SET payload = jsonb_build_object(
            'tag', 'NewAnomaly',
            'projectId', #{pid}::text,
            'createdAt', to_jsonb(now()),
            'anomalyType', #{atype}::text,
            'anomalyAction', 'created'::text,
            'targetHashes', COALESCE((SELECT ths FROM existing), '[]'::jsonb) || to_jsonb(#{payload}::text[])
          )
          WHERE id = (SELECT id FROM existing)
          RETURNING id
        )
        INSERT INTO background_jobs (run_at, status, payload)
        SELECT now(), 'queued', jsonb_build_object(
          'tag', 'NewAnomaly',
          'projectId', #{pid}::text,
          'createdAt', to_jsonb(now()),
          'anomalyType', #{atype}::text,
          'anomalyAction', 'created'::text,
          'targetHashes', to_jsonb(#{payload}::text[])
        )
        WHERE NOT EXISTS (SELECT 1 FROM upd)
      |]


-- Periodic flush worker for the schema-learning catalog.
--
-- Once per shard, every 'flushIntervalSecs', the worker:
--
--   1. Atomically takes the dirty subset (a single 'atomicModifyIORef'').
--   2. Splits dirty entries into 'TemplateRow's (instance-wide, dedup'd by
--      'SchemaLearning.templateHash') and 'CatalogRow's (per-project pointers).
--   3. Upserts templates first, then catalog rows, then re-derives the
--      per-project summary doc.
--   4. Diffs each dirty entry against its prior catalog row (one batched
--      lookup) and inserts endpoint/shape/field/format anomalies into
--      @apis.anomalies@ + a deduped @NewAnomaly@ background job. Replaces
--      the legacy @new_anomaly_proc@ trigger fan-out.
--   5. Hands the newly-acknowledged template hashes back to the shard so
--      subsequent flushes can short-circuit unchanged-template upserts.

-- | Summary stats from one flush pass — useful for telemetry / log lines.
data FlushResult = FlushResult
  { templatesWritten :: !Int
  , catalogRowsWritten :: !Int
  , summariesUpdated :: !Int
  , dirtyKeys :: !Int
  , anomaliesEmitted :: !Int
  -- ^ rows actually inserted into apis.anomalies (post-dedup)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | One flush pass over a single shard. Pure-Eff except for the
-- 'atomicModifyIORef'' inside 'SchemaLearning.takeDirty'.
--
-- Order matters: we diff against the prior catalog row /before/ upserting
-- so we don't see our own write back as the "prior". Anomaly inserts race-
-- safely on the @(project_id, target_hash)@ unique index.
flushDirty
  :: (DB es, IOE :> es, Time.Time :> es)
  => IORef SchemaShardState
  -> Eff es FlushResult
flushDirty ref = do
  dirty <- liftIO $ SchemaLearning.takeDirty ref
  if V.null dirty
    then pure FlushResult{templatesWritten = 0, catalogRowsWritten = 0, summariesUpdated = 0, dirtyKeys = 0, anomaliesEmitted = 0}
    else do
      now <- Time.currentTime
      -- 1. Anomaly diff (must precede upserts so we don't see our own write
      -- back as the prior). Priors come from the in-process snapshot;
      -- cold-start keys fall through to a 'getByKeysBatch' round trip.
      (priorEntries, priorSummaryHashes) <- liftIO $ SchemaLearning.snapshotForFlush ref
      let inMemoryPriors =
            HM.fromList
              [ ((k.projectId, k.keyHash), e)
              | (k, _) <- V.toList dirty
              , Just e <- [HM.lookup k priorEntries]
              ]
          unknownKeys = V.filter (\(k, _) -> not (HM.member k priorEntries)) dirty
      dbPriors <-
        if V.null unknownKeys
          then pure HM.empty
          else getByKeysBatch (V.map (\(k, _) -> (k.projectId, k.keyHash)) unknownKeys)
      let priors = HM.union inMemoryPriors dbPriors
          anomalyRows = buildAnomalyRows priors dirty
      anomaliesN <- insertAnomalies anomalyRows
      enqueueAnomalyJobs anomalyRows
      -- 2. Upserts.
      let templateRows = dedupTemplates $ V.map (templateRowOf now . snd) dirty
          catalogRows = V.map (uncurry catalogRowOf) dirty
          touchedProjects = HS.fromList [k.projectId | (k, _) <- V.toList dirty]
      upsertTemplates templateRows
      upsertCatalogRows catalogRows
      -- Snapshot the shard's entries after our upserts so the summary fold
      -- sees the freshly-merged catalog state without reading it back from
      -- Postgres. Slightly newer than the dirty set (concurrent writers may
      -- have added more keys), which is fine — the map only grows.
      entries <- liftIO $ (.entries) <$> readIORef ref
      (summariesN, newSummaryHashes) <- regenerateSummaries touchedProjects entries priorSummaryHashes
      let newTemplateHashes = HS.fromList $ V.toList $ V.map (.templateHash) templateRows
      liftIO $ SchemaLearning.pruneEvicted ref HS.empty newTemplateHashes
      liftIO $ SchemaLearning.commitFlushedSnapshot ref dirty newSummaryHashes
      pure
        FlushResult
          { templatesWritten = V.length templateRows
          , catalogRowsWritten = V.length catalogRows
          , summariesUpdated = summariesN
          , dirtyKeys = V.length dirty
          , anomaliesEmitted = fromIntegral anomaliesN
          }


-- | Build the anomaly insert rows for a dirty batch. Pure so it can be
-- doctested independently of the DB.
buildAnomalyRows
  :: HM.HashMap (Projects.ProjectId, Text) CatalogEntry
  -> V.Vector (SchemaKey, CatalogEntry)
  -> V.Vector AnomalyInsertRow
buildAnomalyRows priors dirty =
  V.fromList
    [ AnomalyInsertRow
        { projectId = k.projectId
        , anomalyType
        , targetHash = pa.targetHash
        , method = e.scope.method
        , host = e.scope.host
        , urlPath = e.scope.urlPath
        , delta
        , fieldEvidence =
            V.fromList
              [ ApiChanges.FieldEvidence path (prior >>= ApiChanges.fieldSnapshot path) after
              | path <- delta.newFields <> delta.modifiedFields
              , Just after <- [ApiChanges.fieldSnapshot path e]
              ]
        }
    | (k, e) <- V.toList dirty
    , let prior = HM.lookup (k.projectId, k.keyHash) priors
    , pa <- SchemaLearning.diffAnomalies k.keyHash prior e
    , let (anomalyType, delta) = kindLabel pa.kind
    ]
  where
    noDelta = SchemaLearning.ShapeDelta [] []
    kindLabel = \case
      SchemaLearning.AKEndpoint -> (ApiChanges.ATEndpoint, noDelta)
      SchemaLearning.AKShape d -> (ApiChanges.ATShape, d)
      SchemaLearning.AKField -> (ApiChanges.ATField, noDelta)
      SchemaLearning.AKFormat -> (ApiChanges.ATFormat, noDelta)


templateRowOf :: UTCTime -> CatalogEntry -> TemplateRow
templateRowOf now e =
  TemplateRow
    { templateHash = SchemaLearning.templateHash e.template
    , keyKind = e.template.keyKind
    , fields = e.template.fields
    , lastSeenAt = now
    }


catalogRowOf :: SchemaKey -> CatalogEntry -> CatalogRow
catalogRowOf k e =
  CatalogRow
    { projectId = k.projectId
    , keyKind = e.template.keyKind
    , keyHash = k.keyHash
    , templateHash = SchemaLearning.templateHash e.template
    , scope = e.scope
    , valuesDelta = e.valuesDelta
    , counts = e.counts
    , sampleCount = e.sampleCount
    , firstSeen = e.firstSeen
    , lastSeen = e.lastSeen
    }


dedupTemplates :: V.Vector TemplateRow -> V.Vector TemplateRow
dedupTemplates = V.fromList . HM.elems . V.foldl' step HM.empty
  where
    step acc r = HM.insert r.templateHash r acc


-- | Re-derive @apis.schema_summary.doc@ for each project that had at least
-- one dirty key this pass.
--
-- The fold uses the live in-memory shard slice (caller passes
-- 'SchemaShardState.entries'), so the bulk refresh costs O(dirty projects ×
-- avg entries-per-project) of pure work plus exactly two round-trips: one
-- @SELECT@ to find which touched projects were summarised within the
-- staleness window, one batched @UPSERT@. No per-project catalog read; no
-- per-project upsert.
regenerateSummaries
  :: DB es
  => HS.HashSet Projects.ProjectId
  -> HM.HashMap SchemaKey CatalogEntry
  -> HM.HashMap Projects.ProjectId Text
  -- ^ Per-replica content-hash gate; skips unchanged-doc rewrites.
  -> Eff es (Int, HM.HashMap Projects.ProjectId Text)
regenerateSummaries projects entriesMap priorHashes = do
  -- 'freshSummaryProjects' gates cross-replica races (30 s window in PG);
  -- 'priorHashes' gates per-replica unchanged docs. Both are load-bearing.
  let touched = V.fromList (HS.toList projects)
  fresh <- freshSummaryProjects touched
  let staleSet = HS.difference projects fresh
      byProject :: HM.HashMap Projects.ProjectId (V.Vector CatalogEntry)
      byProject =
        V.fromList
          <$> HM.fromListWith
            (<>)
            [(k.projectId, [e]) | (k, e) <- HM.toList entriesMap, HS.member k.projectId staleSet]
      candidates =
        [ (pid, doc, summaryDocHash doc)
        | pid <- HS.toList staleSet
        , Just es <- [HM.lookup pid byProject]
        , let doc = summariseEntries es
        ]
      changed =
        [ (pid, doc, h)
        | (pid, doc, h) <- candidates
        , HM.lookup pid priorHashes /= Just h
        ]
      writeRows = V.fromList [(pid, doc) | (pid, doc, _) <- changed]
      newHashes = HM.fromList [(pid, h) | (pid, _, h) <- changed]
  upsertSummary writeRows
  pure (V.length writeRows, newHashes)


-- | Stable content hash of a 'SummaryDoc' for the skip-unchanged gate.
-- Uses the canonical-ish aeson encoding — the same encoding the upsert
-- would serialise to JSONB, so equal hash ⇔ equal persisted bytes.
summaryDocHash :: SummaryDoc -> Text
summaryDocHash = toXXHash . decodeUtf8 . AE.encode


-- | Project-scoped roll-up of catalog entries into a 'SummaryDoc'.
-- Aggregates field structures across keys (a field appearing in multiple
-- keys with different types unions them) and reduces top-K counts.
summariseEntries :: V.Vector CatalogEntry -> SummaryDoc
summariseEntries entries =
  let fieldsAcc =
        V.foldl' mergeFields HM.empty
          $ V.map ((.template.fields)) entries
      svcs = V.fromList . ordNub . mapMaybe (.scope.service) $ V.toList entries
      topVals :: HashMap Text TopK
      topVals = V.foldl' mergeCounts HM.empty (V.map (.counts) entries)
   in SummaryDoc{fields = fieldsAcc, services = svcs, topValuesByField = topVals}
  where
    mergeFields = HM.unionWith mergeStruct
    mergeStruct a b =
      a
        { SchemaLearning.types = a.types <> b.types
        , SchemaLearning.formats = a.formats <> b.formats
        , SchemaLearning.isEnum = a.isEnum || b.isEnum
        }
    mergeCounts :: HashMap Text TopK -> HashMap Text TopK -> HashMap Text TopK
    mergeCounts = HM.unionWith addTopK
    addTopK a b =
      SchemaLearning.TopK
        { SchemaLearning.distinct = a.distinct + b.distinct
        , SchemaLearning.top = HM.unionWith (+) a.top b.top
        }


-- ---------------------------------------------------------------------------
-- Long-running fiber.

-- | Sleep + flush. Iterates each shard sequentially per tick — flushes are
-- cheap (one round-trip per shard) so parallel scoping isn't worth the
-- complexity. Caller injects the per-tick effect runner so this module
-- stays free of @AuthContext@ / logging plumbing.
runSchemaFlusher
  :: Int
  -- ^ flushIntervalSecs
  -> [IORef SchemaShardState]
  -- ^ one ref per shard
  -> (IORef SchemaShardState -> IO FlushResult)
  -- ^ caller-supplied per-shard runner — typically wraps 'flushDirty' in an
  -- effect runner (Hasql + logging + tracing).
  -> IO Void
runSchemaFlusher intervalSecs refs flushOne = forever do
  threadDelay (intervalSecs * 1_000_000)
  forM_ refs \ref -> do
    -- `flushOne` MUST catch its own exceptions: if it ever lets one escape,
    -- this fiber dies, the dirty set stops draining, and `entries` grows
    -- unboundedly → heap OOM.
    void $ flushOne ref
    -- Bound the in-memory shard state: LRU-evict per-project keys past
    -- 'maxKeysPerProject' and drop the 'knownTemplates' short-circuit if it
    -- grew past its cap. Without this the entries map grows unboundedly with
    -- every distinct (project, keyHash) the shard has ever seen and eventually
    -- exhausts the heap.
    atomicModifyIORef'_ ref (SchemaLearning.evictLRU SchemaLearning.defaultPolicy)
