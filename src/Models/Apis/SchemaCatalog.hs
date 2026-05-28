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
  Catalog.FacetData (..),
  Catalog.FacetValue (..),
  Catalog.FacetSummary (..),
)
where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (DB, UUIDId (..))
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
    asScrubbed :: AE.ToJSON a => a -> HI.AsJsonb AE.Value
    asScrubbed = HI.AsJsonb . scrubNulValue . AE.toJSON
    scopes = V.map (asScrubbed . (.scope)) rows
    vds = V.map (asScrubbed . (.valuesDelta)) rows
    cnts = V.map (asScrubbed . (.counts)) rows
    ss = V.map (fromIntegral @Word64 @Int64 . (.sampleCount)) rows
    firsts = V.map (.firstSeen) rows
    lasts = V.map (.lastSeen) rows


-- | Recursively replace NUL characters in JSON strings (and object keys) with
-- the Unicode replacement char. Postgres' jsonb rejects NUL in text contexts
-- (SQLSTATE 22P05); user-supplied request data occasionally carries one.
scrubNulValue :: AE.Value -> AE.Value
scrubNulValue = \case
  AE.String t -> AE.String (scrubNulText t)
  AE.Array xs -> AE.Array (fmap scrubNulValue xs)
  AE.Object o
    -- Hot path: untouched objects keep their KeyMap; only rebuild when a key
    -- actually contains a NUL byte.
    | any (hasNul . K.toText) (KM.keys o) ->
        AE.Object
          $ KM.fromList
            [(K.fromText (scrubNulText (K.toText k)), scrubNulValue v) | (k, v) <- KM.toList o]
    | otherwise -> AE.Object (fmap scrubNulValue o)
  v -> v
  where
    hasNul = T.any (== '\NUL')
    scrubNulText t = if hasNul t then T.replace "\NUL" "\xFFFD" t else t


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


-- | Bulk variant: fetches catalog rows for a heterogeneous (project, key_hash)
-- set in one round-trip. Used by the anomaly producer to load priors for an
-- entire dirty batch before diffing.
getByKeysBatch
  :: DB es
  => V.Vector (Projects.ProjectId, Text)
  -> Eff es (HM.HashMap (Projects.ProjectId, Text) Catalog.CatalogEntry)
getByKeysBatch pairs
  | V.null pairs = pure HM.empty
  | otherwise = do
      let pids = V.map fst pairs
          khs = V.map snd pairs
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


-- | Batched per-project summary upsert. One round trip regardless of how many
-- projects were touched in the flush. Callers feed a vector of (project, doc)
-- pairs; **an empty vector is a no-op** (no SQL emitted) — important for
-- callers that build the batch eagerly and might end up with zero rows after
-- filtering (e.g. all touched projects were skip-if-fresh).
upsertSummary :: DB es => V.Vector (Projects.ProjectId, Catalog.SummaryDoc) -> Eff es ()
upsertSummary rows = unless (V.null rows) $ do
  let (pids, rawDocs) = V.unzip rows
      docs = HI.AsJsonb <$> rawDocs
  Hasql.interpExecute_
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
      rows :: [UUID.UUID] <-
        Hasql.interp
          [HI.sql| SELECT project_id FROM apis.schema_summary
                   WHERE project_id = ANY(#{pids}::uuid[])
                     AND generated_at > now() - interval '30 seconds' |]
      pure $ HS.fromList (coerce rows)


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
--
-- Catalog 'topValuesByField' paths are bare (e.g. @http.request.method@)
-- because each walk strips its section. To match the KQL parser's
-- 'flattenedOtelAttributes' whitelist (and therefore compile to a flat-column
-- scan), we prefix each path with its field category: attributes → @attributes.@,
-- resource → @resource.@, event → @events.@, body/header/param walks get their
-- standard OTel prefixes. Result keys line up with what users type in KQL.
toFacetSummary :: Projects.ProjectId -> Text -> Catalog.SummaryDoc -> Catalog.FacetSummary
toFacetSummary pid tableName doc =
  Catalog.FacetSummary
    { id = UUID.nil -- summary is not row-identified; legacy callers don't depend on this
    , projectId = pid.toText
    , tableName = tableName
    , facetJson =
        Catalog.FacetData
          $ mergeAndSort
          <$> HM.fromListWith
            (<>)
            [ (prefixed cat path, [(v, fromIntegral n :: Int)])
            | -- Word64→Int safe: counts are bag sizes well below maxBound.
            (path, tk) <- HM.toList doc.topValuesByField
            , -- Missing fields entry → assume FCAttribute (walker always
            -- co-records both; worst case is wrong prefix, not data loss).
            let cat = maybe Catalog.FCAttribute (.category) (HM.lookup path doc.fields)
            , (v, n) <- HM.toList tk.top
            ]
    }
  where
    -- Outer fromListWith already groups by prefixed key, and each (path, tk)
    -- contributes a unique source so values within a key are already unique
    -- by @v@. Just sort descending.
    mergeAndSort :: [(Text, Int)] -> [Catalog.FacetValue]
    mergeAndSort = sortOn (Down . (.count)) . fmap (uncurry Catalog.FacetValue)

    -- 'FacetData' has two consumers: the Log Explorer sidebar (only renders
    -- entries that match 'facetDefs', all of which are in
    -- 'flattenedOtelAttributes') and the AI prompt context, which wants the
    -- full shape including request/response body keys. So
    -- @body.request.*@ / @body.response.*@ keys are intentionally emitted
    -- here even though they're unreachable from the sidebar.
    prefixed :: Catalog.FieldCategoryEnum -> Text -> Text
    prefixed cat path = case cat of
      Catalog.FCAttribute -> "attributes." <> path
      Catalog.FCResource -> "resource." <> path
      Catalog.FCEvent -> "events." <> path
      Catalog.FCRequestHeader -> "attributes.http.request.header." <> path
      Catalog.FCResponseHeader -> "attributes.http.response.header." <> path
      Catalog.FCQueryParam -> "attributes.url.query." <> path
      Catalog.FCPathParam -> "attributes.url.path_param." <> path
      Catalog.FCRequestBody -> "body.request." <> path
      Catalog.FCResponseBody -> "body.response." <> path


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
  , anomalyType :: !Text
  -- ^ matches @apis.anomaly_type@: "endpoint" | "shape" | "field" | "format"
  , targetHash :: !Text
  , method :: !(Maybe Text)
  , host :: !(Maybe Text)
  , urlPath :: !(Maybe Text)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | Of the given @(project_id, endpoint_hash)@ pairs, return those already
-- present in @apis.endpoints@ /and at least 5 minutes old/. Used by the
-- schema-learning flush to skip emitting "new endpoint" anomalies for
-- endpoints the project already knows about.
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
      let pids = V.map fst pairs
          hashes = V.map snd pairs
      rows :: [(UUID, Text)] <-
        Hasql.interp
          [HI.sql| SELECT e.project_id, e.hash
                   FROM apis.endpoints e
                   JOIN unnest(#{pids}::uuid[], #{hashes}::text[]) m(pid, h)
                     ON e.project_id = m.pid AND e.hash = m.h
                   WHERE e.created_at < now() - interval '5 minutes' |]
      pure $ HS.fromList [(UUIDId pid, h) | (pid, h) <- rows]


-- | Bulk-insert anomalies. The unique @(project_id, target_hash)@ index
-- de-duplicates across flush passes — we rely on it to avoid maintaining a
-- separate "already-emitted" set on the hot path. Returns rows actually
-- inserted (excludes ON CONFLICT collisions).
insertAnomalies :: DB es => V.Vector AnomalyInsertRow -> Eff es Int64
insertAnomalies rows | V.null rows = pure 0
insertAnomalies rows =
  Hasql.interpExecute
    [HI.sql| INSERT INTO apis.anomalies (project_id, anomaly_type, action, target_hash, method, host, url_path)
             SELECT pid, atype::apis.anomaly_type, 'created'::apis.anomaly_action, th, mth, hst, up
             FROM unnest(#{pids}::uuid[], #{atypes}::text[], #{ths}::text[], #{methods}::text[], #{hosts}::text[], #{paths}::text[])
                    AS m(pid, atype, th, mth, hst, up)
             ON CONFLICT (project_id, target_hash) DO NOTHING |]
  where
    pids = V.map (.projectId) rows
    atypes = V.map (.anomalyType) rows
    ths = V.map (.targetHash) rows
    methods = V.map (.method) rows
    hosts = V.map (.host) rows
    paths = V.map (.urlPath) rows


-- | Enqueue one @NewAnomaly@ background job per (project, anomalyType) group
-- so the existing notification fan-out (legacy @new_anomaly_proc@'s job
-- emitter) keeps firing. Coalesces with an already-queued job for the same
-- group, mirroring the legacy behaviour.
enqueueAnomalyJobs :: DB es => V.Vector AnomalyInsertRow -> Eff es ()
enqueueAnomalyJobs rows | V.null rows = pass
enqueueAnomalyJobs rows = do
  let groups :: HM.HashMap (Projects.ProjectId, Text) [Text]
      groups = HM.fromListWith (<>) [((r.projectId, r.anomalyType), [r.targetHash]) | r <- V.toList rows]
  forM_ (HM.toList groups) \((pid, atype), ths) -> do
    let payload :: V.Vector Text
        payload = V.fromList ths
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
