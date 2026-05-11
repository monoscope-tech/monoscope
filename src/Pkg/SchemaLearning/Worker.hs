{-# LANGUAGE OverloadedRecordDot #-}

-- | Periodic flush worker for the schema-learning catalog.
--
-- Once per shard, every 'flushIntervalSecs', the worker:
--
--   1. Atomically takes the dirty subset (a single 'atomicModifyIORef'').
--   2. Splits dirty entries into 'TemplateRow's (instance-wide, dedup'd by
--      'Catalog.templateHash') and 'CatalogRow's (per-project pointers).
--   3. Upserts templates first, then catalog rows, then re-derives the
--      per-project summary doc.
--   4. Diffs each dirty entry against its prior catalog row (one batched
--      lookup) and inserts endpoint/shape/field/format anomalies into
--      @apis.anomalies@ + a deduped @NewAnomaly@ background job. Replaces
--      the legacy @new_anomaly_proc@ trigger fan-out.
--   5. Hands the newly-acknowledged template hashes back to the shard so
--      subsequent flushes can short-circuit unchanged-template upserts.
module Pkg.SchemaLearning.Worker (
  FlushResult (..),
  flushDirty,
  runSchemaFlusher,
  buildAnomalyRows,
)
where

import Control.Concurrent (threadDelay)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Vector qualified as V
import Effectful (Eff)
import Models.Apis.SchemaCatalog qualified as SC
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (DB)
import Pkg.SchemaLearning.Catalog (CatalogEntry, SummaryDoc (..), TopK)
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Pkg.SchemaLearning.Hot (SchemaKey, SchemaShardState)
import Pkg.SchemaLearning.Hot qualified as Hot
import Relude


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
-- 'atomicModifyIORef'' inside 'Hot.takeDirty'.
--
-- Order matters: we diff against the prior catalog row /before/ upserting
-- so we don't see our own write back as the "prior". Anomaly inserts race-
-- safely on the @(project_id, target_hash)@ unique index.
flushDirty
  :: DB es
  => IORef SchemaShardState
  -> Eff es FlushResult
flushDirty ref = do
  dirty <- liftIO $ Hot.takeDirty ref
  if V.null dirty
    then pure FlushResult{templatesWritten = 0, catalogRowsWritten = 0, summariesUpdated = 0, dirtyKeys = 0, anomaliesEmitted = 0}
    else do
      now <- liftIO getCurrentTime
      -- 1. Anomaly diff (must precede upserts).
      priors <- SC.getByKeysBatch (V.map (\(k, _) -> (k.projectId, k.keyHash)) dirty)
      let rawRows = buildAnomalyRows priors dirty
      -- Suppress endpoint anomalies for endpoints already in apis.endpoints
      -- (the legacy discovery path beat us to them) and for rows where we
      -- couldn't resolve a url_path. Both classes would otherwise turn into
      -- "new endpoint" Slack/Discord notifications for endpoints customers
      -- already know about — the schema_catalog being empty at rollout
      -- doesn't make them new.
      let endpointPairs = V.mapMaybe (\r -> if r.anomalyType == "endpoint" then Just (r.projectId, r.targetHash) else Nothing) rawRows
      known <- SC.existingEndpointHashes endpointPairs
      let isSuppressed r =
            r.anomalyType == "endpoint"
              && ( HS.member (r.projectId, r.targetHash) known
                    || maybe True T.null r.urlPath
                 )
          anomalyRows = V.filter (not . isSuppressed) rawRows
      anomaliesN <- SC.insertAnomalies anomalyRows
      SC.enqueueAnomalyJobs anomalyRows
      -- 2. Upserts.
      let templateRows = dedupTemplates $ V.map (templateRowOf now . snd) dirty
          catalogRows = V.map (uncurry catalogRowOf) dirty
          touchedProjects = HS.fromList [k.projectId | (k, _) <- V.toList dirty]
      SC.upsertTemplates templateRows
      SC.upsertCatalogRows catalogRows
      summariesN <- regenerateSummaries touchedProjects
      let newHashes = HS.fromList $ V.toList $ V.map (.templateHash) templateRows
      liftIO $ Hot.pruneEvicted ref HS.empty newHashes
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
  -> V.Vector SC.AnomalyInsertRow
buildAnomalyRows priors dirty =
  V.fromList
    [ SC.AnomalyInsertRow
        { projectId = k.projectId
        , anomalyType = kindLabel pa.kind
        , targetHash = pa.targetHash
        , method = e.scope.method
        , host = e.scope.host
        , urlPath = e.scope.urlPath
        }
    | (k, e) <- V.toList dirty
    , pa <- Catalog.diffAnomalies k.keyHash (HM.lookup (k.projectId, k.keyHash) priors) e
    ]
  where
    kindLabel = \case
      Catalog.AKEndpoint -> "endpoint"
      Catalog.AKShape -> "shape"
      Catalog.AKField -> "field"
      Catalog.AKFormat -> "format"


templateRowOf :: UTCTime -> CatalogEntry -> SC.TemplateRow
templateRowOf now e =
  SC.TemplateRow
    { templateHash = Catalog.templateHash e.template
    , keyKind = e.template.keyKind
    , fields = e.template.fields
    , lastSeenAt = now
    }


catalogRowOf :: SchemaKey -> CatalogEntry -> SC.CatalogRow
catalogRowOf k e =
  SC.CatalogRow
    { projectId = k.projectId
    , keyKind = e.template.keyKind
    , keyHash = k.keyHash
    , templateHash = Catalog.templateHash e.template
    , scope = e.scope
    , valuesDelta = e.valuesDelta
    , counts = e.counts
    , sampleCount = e.sampleCount
    , firstSeen = e.firstSeen
    , lastSeen = e.lastSeen
    }


dedupTemplates :: V.Vector SC.TemplateRow -> V.Vector SC.TemplateRow
dedupTemplates = V.fromList . HM.elems . V.foldl' step HM.empty
  where
    step acc r = HM.insert r.templateHash r acc


-- | Re-derive @apis.schema_summary.doc@ for each project that had at least
-- one dirty key this pass. Reads back the full per-project catalog so the
-- summary reflects all known structure, not just what changed in the batch.
regenerateSummaries
  :: DB es
  => HS.HashSet Projects.ProjectId
  -> Eff es Int
regenerateSummaries projects = do
  let pids = HS.toList projects
  forM_ pids \pid -> do
    entries <- SC.getByProject pid
    let doc = summariseEntries entries
    SC.upsertSummary pid doc
  pure (length pids)


-- | Project-scoped roll-up of catalog entries into a 'SummaryDoc'.
-- Aggregates field structures across keys (a field appearing in multiple
-- keys with different types unions them) and reduces top-K counts.
summariseEntries :: V.Vector CatalogEntry -> SummaryDoc
summariseEntries entries =
  let fieldsAcc =
        V.foldl' mergeFields HM.empty
          $ V.map ((.template.fields)) entries
      svcs =
        V.fromList
          $ HS.toList
          $ HS.fromList
          $ catMaybes [e.scope.service | e <- V.toList entries]
      topVals :: HashMap Text TopK
      topVals = V.foldl' mergeCounts HM.empty (V.map (.counts) entries)
   in SummaryDoc{fields = fieldsAcc, services = svcs, topValuesByField = topVals}
  where
    mergeFields acc fs = HM.unionWith mergeStruct acc fs
    mergeStruct a b =
      a
        { Catalog.types = a.types <> b.types
        , Catalog.formats = a.formats <> b.formats
        , Catalog.isEnum = a.isEnum || b.isEnum
        }
    mergeCounts :: HashMap Text TopK -> HashMap Text TopK -> HashMap Text TopK
    mergeCounts = HM.unionWith addTopK
    addTopK a b =
      Catalog.TopK
        { Catalog.distinct = a.distinct + b.distinct
        , Catalog.top = HM.unionWith (+) a.top b.top
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
    void $ flushOne ref
    -- Bound the in-memory shard state: LRU-evict per-project keys past
    -- 'maxKeysPerProject' and drop the 'knownTemplates' short-circuit if it
    -- grew past its cap. Without this the entries map grows unboundedly with
    -- every distinct (project, keyHash) the shard has ever seen and eventually
    -- exhausts the heap.
    atomicModifyIORef' ref \st -> (Hot.evictLRU Hot.defaultPolicy st, ())
