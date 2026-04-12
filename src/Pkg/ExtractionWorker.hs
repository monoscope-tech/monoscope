-- | Sharded extraction worker: hash-routes project batches to single-writer
-- shard fibers for eager derivation + drain-track pattern tagging.
module Pkg.ExtractionWorker (
  WorkerState (..),
  ShardState (..),
  ExtractionBatch (..),
  ServiceBuffer (..),
  BufferedSpan (..),
  ServiceDrainTree (..),
  DrainFlushTask (..),
  CircuitState (..),
  CircuitBreaker,
  newCircuitBreaker,
  shouldAttemptCircuit,
  recordCircuitSuccess,
  recordCircuitFailure,
  initWorkerState,
  runRehydrationWorker,
  submitBatch,
  hashProjectId,
  runShardWorker,
  appendBufferedSpans,
  collectAgedFlushes,
  enforceBufferBound,
  evictStaleTrees,
  forceFlushAllBuffers,
  allQueuesDrained,
) where

import Control.Concurrent.STM (stateTVar)
import Control.Concurrent.STM.TBQueue (TBQueue, isEmptyTBQueue, isFullTBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HashSet
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Models.Projects.Projects qualified as Projects
import Pkg.Drain qualified as Drain
import Relude
import UnliftIO (tryAny)


-- | Simple STM circuit breaker for best-effort writes (e.g. TimeFusion).
-- Opens after 5 consecutive failures; stays open 60s then half-opens.
data CircuitState = CircuitClosed !Int | CircuitOpen !UTCTime


type CircuitBreaker = TVar CircuitState


newCircuitBreaker :: IO CircuitBreaker
newCircuitBreaker = newTVarIO (CircuitClosed 0)


shouldAttemptCircuit :: CircuitBreaker -> UTCTime -> IO Bool
shouldAttemptCircuit cb now =
  atomically $ readTVar cb >>= \case
    CircuitClosed _ -> pure True
    CircuitOpen openedAt -> pure (diffUTCTime now openedAt >= 60)


recordCircuitSuccess :: CircuitBreaker -> IO ()
recordCircuitSuccess cb = atomically $ writeTVar cb (CircuitClosed 0)


-- | Record a failure; returns True if the circuit just opened (caller should log).
recordCircuitFailure :: CircuitBreaker -> UTCTime -> IO Bool
recordCircuitFailure cb now = atomically $ stateTVar cb \case
  CircuitClosed n | n >= 4 -> (True, CircuitOpen now)
  CircuitClosed n -> (False, CircuitClosed (n + 1))
  st -> (False, st)


-- | Top-level worker handle owned by `AuthContext`. Parameterized on the span
-- row type; see `ExtractionBatch` for the cycle-avoidance rationale.
data WorkerState s = WorkerState
  { shards :: !(V.Vector (ShardState s))
  , acceptingBatches :: !(TVar Bool)
  , droppedBatches :: !(IORef Int)
  , droppedFlushTasks :: !(IORef Int)
  }


data ShardState s = ShardState
  { ingressQ :: !(TBQueue (ExtractionBatch s))
  , drainFlushQ :: !(TBQueue DrainFlushTask)
  , rehydrationQ :: !(TBQueue (IO ()))
  , queueDepth :: !(TVar Int)
  , drainBuffers :: !(IORef (HashMap (Projects.ProjectId, Text) ServiceBuffer))
  , drainTrees :: !(IORef (HashMap (Projects.ProjectId, Text) ServiceDrainTree))
  , pendingRehydrations :: !(IORef (HashSet (Projects.ProjectId, Text)))
  }


-- | In-memory hand-off payload. Parameterized on span type `s` to avoid
-- circular imports with Models.Telemetry.
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
  , oldestAt :: !UTCTime
  , spanCount :: !Int
  }


data BufferedSpan = BufferedSpan
  { spanCtxId :: !Text
  , traceId :: !Text
  , timestamp :: !UTCTime
  , summary :: !Text
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


-- | Build an idle `WorkerState` with `numShards` empty shards.
initWorkerState :: Int -> Int -> IO (WorkerState s)
initWorkerState numShards queueCapacity = do
  shards <-
    V.replicateM (max 1 numShards) do
      ingressQ <- newTBQueueIO (fromIntegral (max 1 queueCapacity))
      drainFlushQ <- newTBQueueIO 32
      rehydrationQ <- newTBQueueIO 8
      queueDepth <- newTVarIO 0
      drainBuffers <- newIORef HM.empty
      drainTrees <- newIORef HM.empty
      pendingRehydrations <- newIORef HashSet.empty
      pure ShardState{ingressQ, drainFlushQ, rehydrationQ, queueDepth, drainBuffers, drainTrees, pendingRehydrations}
  acceptingBatches <- newTVarIO False
  droppedBatches <- newIORef 0
  droppedFlushTasks <- newIORef 0
  pure WorkerState{shards, acceptingBatches, droppedBatches, droppedFlushTasks}


-- | Non-blocking hand-off. Returns False if disabled/full (safety-net retries).
submitBatch :: WorkerState s -> ExtractionBatch s -> STM Bool
submitBatch st batch = do
  accepting <- readTVar st.acceptingBatches
  if not accepting || V.null st.shards
    then pure False
    else do
      let shardIx = hashProjectId batch.projectId `mod` V.length st.shards
          shard = V.unsafeIndex st.shards shardIx
      full <- isFullTBQueue shard.ingressQ
      if full
        then pure False
        else do
          writeTBQueue shard.ingressQ batch
          modifyTVar' shard.queueDepth succ
          pure True


-- | Stable XOR hash of ProjectId UUID bytes for shard routing. Must stay
-- stable across deploys (single-writer invariant).
hashProjectId :: Projects.ProjectId -> Int
hashProjectId pid =
  let (w1, w2, w3, w4) = UUID.toWords pid.unwrap
   in fromIntegral (w1 `xor` w2 `xor` w3 `xor` w4)


-- | Shard fiber body: consume batches, trap per-batch exceptions (safety-net
-- retries failed rows). Processor callback injected to avoid import cycle.
runShardWorker
  :: (Text -> IO ())
  -- ^ logger for per-batch failures (caller injects its effect runner here)
  -> (ExtractionBatch s -> ShardState s -> IO ())
  -- ^ eager-track processor — receives the batch and its owning shard
  -> ShardState s
  -> IO ()
runShardWorker logWarn processor shard = forever do
  batch <- atomically do
    b <- readTBQueue shard.ingressQ
    modifyTVar' shard.queueDepth pred
    pure b
  tryAny (processor batch shard) >>= \case
    Right () -> pass
    Left e -> logWarn $ "extraction-worker batch failed: " <> show e


-- | Dedicated rehydration worker per shard: drains rehydrationQ sequentially.
runRehydrationWorker :: (Text -> IO ()) -> ShardState s -> IO ()
runRehydrationWorker logWarn shard = forever do
  job <- atomically $ readTBQueue shard.rehydrationQ
  tryAny job >>= \case
    Right () -> pass
    Left e -> logWarn $ "rehydration job failed: " <> show e


-- | Append spans into per-(project, service) drain buffers; evict + enqueue
-- any buffer crossing `flushBatchSize`.
appendBufferedSpans
  :: ShardState s
  -> Projects.ProjectId
  -> Int
  -- ^ flushBatchSize — evict + enqueue buffers crossing this span count.
  -> UTCTime
  -> IORef Int
  -- ^ droppedFlushTasks counter to bump on queue-full
  -> [(Text, BufferedSpan)]
  -- ^ per-span (serviceName, BufferedSpan) — summary must already be `T.copy`'d.
  -> IO ()
appendBufferedSpans shard pid flushBatchSize now droppedCounter incoming = do
  toFlush <- atomicModifyIORef' shard.drainBuffers \buffers0 ->
    let (buffers1, flushAcc) = foldl' stepInsert (buffers0, []) incoming
        stepInsert (!bufs, !acc) (svcName, bs) =
          let key = (pid, svcName)
              merged = case HM.lookup key bufs of
                Nothing -> ServiceBuffer{pendingSpans = [bs], oldestAt = bs.timestamp, spanCount = 1}
                Just sb ->
                  sb
                    { pendingSpans = bs : sb.pendingSpans
                    , oldestAt = min sb.oldestAt bs.timestamp
                    , spanCount = sb.spanCount + 1
                    }
           in if merged.spanCount >= flushBatchSize
                then (HM.delete key bufs, (svcName, merged) : acc)
                else (HM.insert key merged bufs, acc)
     in (buffers1, flushAcc)
  for_ toFlush \(svcName, sb) -> do
    ok <- enqueueFlush shard pid svcName sb now
    unless ok $ atomicModifyIORef' droppedCounter \n -> (n + 1, ())


-- | Evict buffers older than `maxAgeSecs` into drainFlushQ.
collectAgedFlushes :: WorkerState s -> Int -> UTCTime -> IO ()
collectAgedFlushes st maxAgeSecs now =
  V.forM_ st.shards \shard -> do
    let threshold = fromIntegral maxAgeSecs :: NominalDiffTime
    aged <- atomicModifyIORef' shard.drainBuffers \bufs ->
      let stale = HM.filter (\sb -> diffUTCTime now sb.oldestAt >= threshold) bufs
       in (HM.difference bufs stale, stale)
    for_ (HM.toList aged) \((pid, svcName), sb) -> do
      ok <- enqueueFlush shard pid svcName sb now
      unless ok $ atomicModifyIORef' st.droppedFlushTasks \n -> (n + 1, ())


-- | Force-flush largest buffers until total spans are under `maxSpans`.
enforceBufferBound :: WorkerState s -> Int -> UTCTime -> IO ()
enforceBufferBound st maxSpans now = go
  where
    go = do
      candidates <-
        concat <$> V.forM st.shards \shard -> do
          bufs <- readIORef shard.drainBuffers
          pure [(shard, k, sb) | (k, sb) <- HM.toList bufs]
      let totalSpans = sum [sb.spanCount | (_, _, sb) <- candidates]
      when (totalSpans > maxSpans)
        $ for_ (viaNonEmpty Relude.last $ sortOn (\(_, _, sb) -> sb.spanCount) candidates) \(shard, (pid, svcName), _) -> do
          evicted <- atomicModifyIORef' shard.drainBuffers \bufs ->
            let key = (pid, svcName)
             in case HM.lookup key bufs of
                  Nothing -> (bufs, Nothing)
                  Just sb -> (HM.delete key bufs, Just sb)
          for_ evicted \sb -> do
            ok <- enqueueFlush shard pid svcName sb now
            if ok
              then go
              else do
                -- Put buffer back since the flush queue is full.
                atomicModifyIORef' shard.drainBuffers \bufs -> (HM.insert (pid, svcName) sb bufs, ())
                atomicModifyIORef' st.droppedFlushTasks \n -> (n + 1, ())


-- | LRU eviction: remove drainTrees entries stale > 1 hour or exceeding cap.
evictStaleTrees :: WorkerState s -> Int -> UTCTime -> IO ()
evictStaleTrees st maxTrees now = do
  V.forM_ st.shards \shard ->
    atomicModifyIORef' shard.drainTrees \m ->
      let oneHour = 3600 :: NominalDiffTime
          fresh = HM.filter (\sdt -> diffUTCTime now sdt.lastSeededAt < oneHour) m
       in if HM.size fresh <= maxTrees
            then (fresh, ())
            else
              -- Still over cap after time-based eviction: drop oldest entries.
              let sorted = sortOn ((.lastSeededAt) . snd) (HM.toList fresh)
                  kept = HM.fromList $ drop (HM.size fresh - maxTrees) sorted
               in (kept, ())


-- | Force-flush every drain buffer across all shards into their drainFlushQ.
-- Used during graceful shutdown so buffered spans get pattern-tagged before exit.
forceFlushAllBuffers :: WorkerState s -> UTCTime -> IO ()
forceFlushAllBuffers st now =
  V.forM_ st.shards \shard -> do
    all' <- atomicModifyIORef' shard.drainBuffers \bufs -> (HM.empty, HM.toList bufs)
    for_ all' \((pid, svcName), sb) -> do
      ok <- enqueueFlush shard pid svcName sb now
      unless ok $ atomicModifyIORef' st.droppedFlushTasks \n -> (n + 1, ())


-- | Non-blocking enqueue of a buffer into its shard's drainFlushQ.
-- Returns False if the queue is full (caller may bump drop metric).
enqueueFlush :: ShardState s -> Projects.ProjectId -> Text -> ServiceBuffer -> UTCTime -> IO Bool
enqueueFlush shard pid svcName sb now = atomically do
  full <- isFullTBQueue shard.drainFlushQ
  if full
    then pure False
    else do
      writeTBQueue
        shard.drainFlushQ
        DrainFlushTask
          { projectId = pid
          , serviceName = svcName
          , spans = reverse sb.pendingSpans
          , flushedAt = now
          }
      pure True


-- | Check if all ingress + drain-flush queues are empty across shards.
allQueuesDrained :: WorkerState s -> IO Bool
allQueuesDrained st =
  atomically
    $ V.and
    <$> V.forM st.shards \shard -> do
      depth <- readTVar shard.queueDepth
      drainEmpty <- isEmptyTBQueue shard.drainFlushQ
      pure (depth == 0 && drainEmpty)
