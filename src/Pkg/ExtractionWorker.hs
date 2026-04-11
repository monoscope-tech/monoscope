-- | In-process hand-off worker for span/log derivation (endpoints, shapes,
-- fields, log patterns, error patterns). See plan squishy-imagining-diffie.md.
--
-- Slice 1 scope: types + init + a `submitBatch` stub that is a no-op until the
-- worker fibers are spawned in slice 2. The stub returns `False` when
-- `enableExtractionWorker` is off so callers can bump a drop metric and the
-- safety-net picks up the rows via `processed_at IS NULL`.
module Pkg.ExtractionWorker (
  WorkerState (..),
  ShardState (..),
  ExtractionBatch (..),
  ServiceBuffer (..),
  BufferedSpan (..),
  ServiceDrainTree (..),
  DrainFlushTask (..),
  initWorkerState,
  submitBatch,
) where

import Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue, newTBQueueIO, writeTBQueue)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HashSet
import Data.Time.Clock (UTCTime)
import Data.Vector qualified as V
import Models.Projects.Projects qualified as Projects
import Pkg.Drain qualified as Drain
import Relude


-- | Top-level worker handle owned by `AuthContext`. Slice 1 builds this but
-- does not spawn shard fibers — shards exist only so `submitBatch` routing
-- code can compile today. Parameterized on the span row type; see
-- `ExtractionBatch` for the cycle-avoidance rationale.
data WorkerState s = WorkerState
  { shards :: !(V.Vector (ShardState s))
  , acceptingBatches :: !(TVar Bool)
  -- ^ Phase-A shutdown flip. `submitBatch` short-circuits when `False`.
  , droppedBatches :: !(IORef Int)
  -- ^ Prometheus counter backing `extraction_worker_batches_dropped_total`.
  }


data ShardState s = ShardState
  { ingressQ :: !(TBQueue (ExtractionBatch s))
  , drainFlushQ :: !(TBQueue DrainFlushTask)
  , queueDepth :: !(TVar Int)
  , drainBuffers :: !(IORef (HashMap (Projects.ProjectId, Text) ServiceBuffer))
  , drainTrees :: !(IORef (HashMap (Projects.ProjectId, Text) ServiceDrainTree))
  , pendingRehydrations :: !(IORef (HashSet (Projects.ProjectId, Text)))
  }


-- | In-memory hand-off payload. Parallel `spanIds` / `traceIds` vectors are
-- pre-materialized here so the eager-track UPDATE-1 unnest-and-join has its
-- keys ready without re-walking `spans`.
--
-- Parameterized on the span row type (`s`) so this module does not depend on
-- `Models.Telemetry.Telemetry` — that module imports `System.Config`, and
-- `System.Config` imports this one to hold `WorkerState` in `AuthContext`.
-- Callers instantiate `s ~ Telemetry.OtelLogsAndSpans` at the ingestion site
-- in slice 2.
data ExtractionBatch s = ExtractionBatch
  { projectId :: !Projects.ProjectId
  , projectCache :: !Projects.ProjectCache
  , spans :: !(V.Vector s)
  , spanIds :: !(V.Vector Text)
  , traceIds :: !(V.Vector Text)
  , batchMinTs :: !UTCTime
  , batchMaxTs :: !UTCTime
  }


data ServiceBuffer = ServiceBuffer
  { pendingSpans :: ![BufferedSpan]
  -- ^ Prepended on insert; reversed on flush (O(1) append).
  , oldestAt :: !UTCTime
  , spanCount :: !Int
  }


data BufferedSpan = BufferedSpan
  { spanCtxId :: !Text
  , traceId :: !Text
  , timestamp :: !UTCTime
  , summary :: !Text
  -- ^ `T.copy`'d on insert so the buffer does not pin the parent vector.
  }


data ServiceDrainTree = ServiceDrainTree
  { tree :: !Drain.DrainTree
  , lastSeededAt :: !UTCTime
  , maxPatternSeenAt :: !(Maybe UTCTime)
  }


data DrainFlushTask = DrainFlushTask
  { projectId :: !Projects.ProjectId
  , serviceName :: !Text
  , spans :: ![BufferedSpan]
  , flushedAt :: !UTCTime
  }


-- | Build an idle `WorkerState` with `numShards` empty shards. Called from
-- `getAppContext` before any fibers exist. Queue capacity is shared across
-- ingress and drain-flush queues for now — revisit in slice 2 when the fibers
-- actually consume from them.
initWorkerState :: Int -> Int -> IO (WorkerState s)
initWorkerState numShards queueCapacity = do
  shards <-
    V.replicateM (max 1 numShards) do
      ingressQ <- newTBQueueIO (fromIntegral (max 1 queueCapacity))
      drainFlushQ <- newTBQueueIO 32
      queueDepth <- newTVarIO 0
      drainBuffers <- newIORef HM.empty
      drainTrees <- newIORef HM.empty
      pendingRehydrations <- newIORef HashSet.empty
      pure ShardState{ingressQ, drainFlushQ, queueDepth, drainBuffers, drainTrees, pendingRehydrations}
  acceptingBatches <- newTVarIO False
  droppedBatches <- newIORef 0
  pure WorkerState{shards, acceptingBatches, droppedBatches}


-- | Non-blocking hand-off from the ingestion hot path. Returns `True` if the
-- batch was queued, `False` if the worker is disabled, shutting down, or the
-- shard queue is full. Callers that get `False` must bump the drop metric; the
-- batch is still durable in Postgres/TimeFusion and will be re-driven by the
-- hourly `SafetyNetReprocess` job (`processed_at IS NULL`).
--
-- TODO(extraction-worker slice 2): once the shard workers are spawned, route
-- by `hash(projectId) mod N` so all of a project's spans land in one shard
-- (single-writer invariant for the drain buffers / trees IORefs).
submitBatch :: WorkerState s -> ExtractionBatch s -> STM Bool
submitBatch st batch = do
  accepting <- readTVar st.acceptingBatches
  if not accepting || V.null st.shards
    then pure False
    else do
      let shard = V.unsafeHead st.shards -- slice 2: hash routing
      full <- isFullTBQueue shard.ingressQ
      if full
        then pure False
        else do
          writeTBQueue shard.ingressQ batch
          modifyTVar' shard.queueDepth succ
          pure True
