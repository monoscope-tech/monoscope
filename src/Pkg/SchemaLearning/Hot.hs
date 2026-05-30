{-# LANGUAGE OverloadedRecordDot #-}

-- | Streaming hot path for schema-learning. Single-writer per shard; the
-- shard fiber calls 'observeSpans' once per ingestion batch, the flush
-- fiber (in "Pkg.SchemaLearning.Worker") swaps the dirty subset.
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
module Pkg.SchemaLearning.Hot (
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

import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Models.Projects.Projects qualified as Projects
import Pkg.SchemaLearning.Catalog (
  CatalogEntry,
  FieldCategoryEnum,
  KeyKind,
  Scope,
  mergeFullWalk,
  newEntry,
 )
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Relude


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
  -- ^ Cheap running approximation of the in-memory footprint of 'entries'.
  -- Updated incrementally on each merge; used by 'observeSpans' to fire
  -- 'evictLRU' between flush ticks when 'maxBytesPerShard' is exceeded.
  , flushedSummaryHashes :: !(HashMap Projects.ProjectId Text)
  -- ^ Last-persisted SummaryDoc hash per project. Lets the flush worker
  -- skip the @apis.schema_summary@ rewrite when nothing changed since the
  -- previous tick — typical for stable projects, biggest TOAST win.
  , flushedEntries :: !(HashMap SchemaKey CatalogEntry)
  -- ^ Last-persisted CatalogEntry per key. The anomaly differ uses this
  -- snapshot instead of issuing a per-flush @SELECT@ over all dirty keys.
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
  -- ^ Lazy walk thunk. See 'Catalog.mergeFullWalk' — leaf-walked fields
  -- with per-value format hints. Held as a thunk so 'mergeGroup' only
  -- forces the (expensive) JSON traversal + regex sweep when the sample
  -- gate fires; the 99% of spans that bump-only never pay for the walk.
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
            -- Between-tick eviction: if this batch pushed us over budget,
            -- run evictLRU now instead of waiting up to 60 s for the flush
            -- fiber. Bounded amortised cost — eviction only fires when the
            -- shard is actually large.
            st' =
              if merged.bytesEstimate > policy.maxBytesPerShard
                then evictLRU policy merged
                else merged
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
      base = fromMaybe (newEntry rep.keyKind rep.scope now) curEntry
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
          { Catalog.sampleCount = walked.sampleCount + addExtra
          , Catalog.lastSeen = now
          , Catalog.dirty = True
          }
      entries' = HM.insert key newEntry' st.entries
      dirty' = HS.insert key st.dirtyKeys
      -- Incremental byte tracking. Sample-bumped spans don't grow the
      -- entry, so skip the recompute on that hot path.
      bytes' =
        if learnPhase || sampleNow
          then st.bytesEstimate - maybe 0 entryBytes curEntry + entryBytes newEntry'
          else st.bytesEstimate
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
            [ (k, e{Catalog.dirty = False})
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
             in fst <$> take excess sorted
      afterPerProject = HS.foldr HM.delete st.entries (HS.fromList perProjectVictims)
      -- Byte-budget pass: if we're still over budget after the per-project
      -- LRU, keep dropping the globally-oldest entries until we fit. This is
      -- what stops a single high-cardinality project from exhausting the
      -- shard between flush ticks.
      bytesPerProjPass = HM.foldl' (\acc e -> acc + entryBytes e) 0 afterPerProject
      (entries', bytesFinal) =
        if bytesPerProjPass <= policy.maxBytesPerShard
          then (afterPerProject, bytesPerProjPass)
          else evictUntilUnderBudget policy.maxBytesPerShard afterPerProject bytesPerProjPass
      -- Evicted keys also drop from the flushed-state snapshot. If a key
      -- comes back later it'll be treated as new — same correctness as
      -- after a process restart, and bounded memory.
      droppedKeys = HS.difference (HS.fromList (HM.keys st.entries)) (HS.fromList (HM.keys entries'))
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
      vdSize = HM.foldl' (\acc (Catalog.Examples vs) -> acc + V.length vs * bytesPerExample) 0 e.valuesDelta
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
