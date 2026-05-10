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
--   4. Hands the newly-acknowledged template hashes back to the shard so
--      subsequent flushes can short-circuit unchanged-template upserts.
--
-- Anomaly diff/produce is not yet wired here — the legacy
-- @new_anomaly_proc@ trigger is being deprecated and the replacement
-- belongs in a follow-up (see TODO in 'flushDirty').
module Pkg.SchemaLearning.Worker (
  FlushResult (..),
  flushDirty,
  runSchemaFlusher,
)
where

import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Time (UTCTime, getCurrentTime)
import Data.Vector qualified as V
import Control.Concurrent (threadDelay)
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
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | One flush pass over a single shard. Pure-Eff except for the
-- 'atomicModifyIORef'' inside 'Hot.takeDirty'.
flushDirty
  :: DB es
  => IORef SchemaShardState
  -> Eff es FlushResult
flushDirty ref = do
  dirty <- liftIO $ Hot.takeDirty ref
  if V.null dirty
    then pure FlushResult{templatesWritten = 0, catalogRowsWritten = 0, summariesUpdated = 0, dirtyKeys = 0}
    else do
      now <- liftIO getCurrentTime
      let templateRows = dedupTemplates $ V.map (templateRowOf now . snd) dirty
          catalogRows = V.map (uncurry catalogRowOf) dirty
          touchedProjects = HS.fromList [k.projectId | (k, _) <- V.toList dirty]
      SC.upsertTemplates templateRows
      SC.upsertCatalogRows catalogRows
      summariesN <- regenerateSummaries touchedProjects
      let newHashes = HS.fromList $ V.toList $ V.map (.templateHash) templateRows
      liftIO $ Hot.pruneEvicted ref HS.empty newHashes
      -- TODO(schema-anomalies): diff dirty entries vs prior catalog rows
      -- (stale @apis.shapes@/@apis.fields@ triggers no longer fire). Emit
      -- per-(project, key_hash) endpoint/shape/field/format anomalies into
      -- @apis.anomalies@ + @background_jobs@ so the legacy notification
      -- pipeline keeps working.
      pure
        FlushResult
          { templatesWritten = V.length templateRows
          , catalogRowsWritten = V.length catalogRows
          , summariesUpdated = summariesN
          , dirtyKeys = V.length dirty
          }


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
  forM_ refs \ref -> void $ flushOne ref
