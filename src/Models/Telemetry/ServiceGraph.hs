-- | Service dependency graphs: the shared payload and the single builder behind both
-- the global service map (aggregated over a time range) and the trace-level map (one
-- request's path through the system).
--
-- Edges follow the OpenTelemetry service-graph contract: a @server@/@consumer@ span whose
-- nearest cross-service ancestor is a @client@/@producer@ span is one hop between two
-- instrumented services. A @client@/@producer@ span that nothing answered is a hop into an
-- /uninstrumented/ dependency — a database, a queue, or a third-party host — which is
-- synthesised as an inferred node so the map shows the whole system, not just the parts
-- that happen to run our SDK.
module Models.Telemetry.ServiceGraph (
  NodeKind (..),
  MapStats (..),
  ServiceNode (..),
  ServiceEdge (..),
  ServiceGraph (..),
  EdgeSample (..),
  LatencyHist (..),
  singletonLatency,
  latencyPercentile,
  buildServiceGraph,
  traceEdgeSamples,
  emptyServiceGraph,
  serviceMapNodeCap,
  RollupEdge (..),
  projectsWithSpansInRange,
  rollupServiceEdges,
  upsertServiceDependencyEdges,
  serviceGraphForRange,
) where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Char (isDigit)
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime)
import Data.Vector qualified as V
import Deriving.Aeson.Stock qualified as DAE
import Effectful
import Effectful.Labeled (Labeled)
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (SpanKind (..), SpanRecord (..), SpanStatus (..), atMapText)
import Pkg.DeriveUtils (AesonText (..), DB, UUIDId (..), WrappedEnumSC (..), idFromText)
import Relude


-- | What a node in the graph /is/. A sum type rather than @inferred :: Bool@ plus
-- @isDatabase :: Bool@ so the renderer's shape/style dispatch is exhaustive and an
-- "inferred entry-point database" cannot be constructed.
data NodeKind = NKEntry | NKService | NKDatabase | NKQueue | NKExternal | NKUnknown
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC 'Nothing "NK" NodeKind


-- | Everything downstream of a service node except an entry point is something that
-- service calls; only 'NKService' nodes run our instrumentation.
inferredKind :: NodeKind -> Bool
inferredKind = \case
  NKEntry -> False
  NKService -> False
  NKDatabase -> True
  NKQueue -> True
  NKExternal -> True
  NKUnknown -> True


-- | Sparse log-scale latency histogram, 4 buckets per octave over microseconds. Exactly
-- mergeable, so a range's percentiles are read off the merged histogram rather than
-- averaging per-bucket percentiles (which is wrong). ~19% relative error — good enough to
-- size a node, never presented as an SLO number.
newtype LatencyHist = LatencyHist (IntMap Int64)
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData)


instance Semigroup LatencyHist where
  LatencyHist a <> LatencyHist b = LatencyHist $ IntMap.unionWith (+) a b


instance Monoid LatencyHist where
  mempty = LatencyHist IntMap.empty


latencyBucket :: Int64 -> Int
latencyBucket durationNs = floor $ 4 * logBase 2 (max 1 (fromIntegral durationNs / 1000) :: Double)


singletonLatency :: Int64 -> LatencyHist
singletonLatency d = LatencyHist $ one (latencyBucket d, 1)


-- | Nanoseconds at quantile @q@. Reads the bucket's upper edge, so a p95 never reports
-- lower than a sample it contains.
--
-- >>> latencyPercentile 0.5 (foldMap singletonLatency [1000, 2000, 4000]) > 0
-- True
latencyPercentile :: Double -> LatencyHist -> Int64
latencyPercentile q (LatencyHist h)
  | total <= 0 = 0
  | otherwise = go 0 (IntMap.toAscList h)
  where
    total = sum (IntMap.elems h)
    target = ceiling $ q * fromIntegral total :: Int64
    go _ [] = 0
    go acc ((idx, c) : rest)
      | acc + c >= target = round $ 1000 * (2 ** ((fromIntegral idx + 1) / 4) :: Double)
      | otherwise = go (acc + c) rest


data MapStats = MapStats
  { requests :: !Int64
  , errors :: !Int64
  , errorRate :: !Double
  , p50Ns :: !Int64
  , p95Ns :: !Int64
  , p99Ns :: !Int64
  , throughputPerSec :: !Double
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MapStats


mkStats :: Double -> Int64 -> Int64 -> LatencyHist -> MapStats
mkStats rangeSecs reqs errs hist =
  MapStats
    { requests = reqs
    , errors = errs
    , errorRate = if reqs > 0 then fromIntegral errs / fromIntegral reqs else 0
    , p50Ns = latencyPercentile 0.5 hist
    , p95Ns = latencyPercentile 0.95 hist
    , p99Ns = latencyPercentile 0.99 hist
    , throughputPerSec = if rangeSecs > 0 then fromIntegral reqs / rangeSecs else 0
    }


data ServiceNode = ServiceNode
  { key :: !Text
  , label :: !Text
  , kind :: !NodeKind
  , inferred :: !Bool
  , durationShare :: !(Maybe Double)
  -- ^ Trace map only: this service's share of the trace's total span time, which is what
  -- sizes its node. Nothing on the global map, where volume sizes nodes instead.
  , stats :: !MapStats
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ServiceNode


data ServiceEdge = ServiceEdge
  { source :: !Text
  , target :: !Text
  , stats :: !MapStats
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ServiceEdge


data ServiceGraph = ServiceGraph
  { nodes :: !(V.Vector ServiceNode)
  , edges :: !(V.Vector ServiceEdge)
  , rangeSeconds :: !Double
  , truncated :: !Bool
  -- ^ Set when the node cap dropped part of the graph, so the UI can say so. A silently
  -- truncated dependency map reads as "these are all your services", which is a lie.
  , error :: !(Maybe Text)
  -- ^ Sanitized backend-failure message. A failed query must render an error state, never
  -- an empty graph that reads as "you have no services".
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ServiceGraph


emptyServiceGraph :: Double -> ServiceGraph
emptyServiceGraph rangeSecs = ServiceGraph V.empty V.empty rangeSecs False Nothing


-- | Beyond this a layered graph stops being readable and starts being a hairball, so the
-- busiest nodes are kept and 'truncated' tells the UI to say so.
serviceMapNodeCap :: Int
serviceMapNodeCap = 150


-- | One observed hop. The rollup emits these pre-aggregated per bucket; the trace fold
-- emits one per matched span pair. Both go through 'buildServiceGraph', so the two maps
-- can't drift in how they aggregate.
data EdgeSample = EdgeSample
  { source :: !(Text, NodeKind)
  , target :: !(Text, NodeKind)
  , requests :: !Int64
  , errors :: !Int64
  , latency :: !LatencyHist
  , targetDurationNs :: !Int64
  -- ^ Callee time attributed to this hop; only used to compute a trace map's duration
  -- share, and ignored when 'buildServiceGraph' is given no total.
  }
  deriving stock (Generic, Show)


-- | Fold hops into a graph: merge duplicate edges, derive each node's stats from its
-- inbound hops, and cap the node count by request volume.
--
-- Node stats come from inbound edges because an edge's latency is the /callee's/ duration
-- — what that service took to serve the call — so a node's latency is just the merge of
-- what its callers observed, with no second measurement to keep in sync.
buildServiceGraph :: Double -> Int -> Maybe Int64 -> [EdgeSample] -> ServiceGraph
buildServiceGraph rangeSecs nodeCap totalDurationM samples =
  ServiceGraph
    { nodes = V.fromList keptNodes
    , edges = V.fromList [e | e <- allEdges, e.source `S.member` keptKeys, e.target `S.member` keptKeys]
    , rangeSeconds = rangeSecs
    , truncated = length nodeStats > nodeCap
    , error = Nothing
    }
  where
    aggOf s = HopAgg s.requests s.errors s.latency s.targetDurationNs
    merged = Map.fromListWith (<>) [((s.source, s.target), aggOf s) | s <- samples]
    allEdges = [ServiceEdge (fst src) (fst tgt) (statsOf agg) | ((src, tgt), agg) <- Map.toList merged]

    -- A node's kind is whatever the hops say it is; a service seen both as a caller and
    -- as an inferred peer is a service (NKService sorts before the inferred kinds).
    nodeKinds = Map.fromListWith min $ concat [[s.source, s.target] | s <- samples]
    inbound = Map.fromListWith (<>) [(fst s.target, aggOf s) | s <- samples]
    -- Entry-only and leaf nodes still need a row, with zeroed stats.
    nodeStats = [(key, Map.findWithDefault mempty key inbound) | key <- Map.keys nodeKinds]

    keptNodes =
      [ ServiceNode
          { key
          , label = nodeLabel key
          , kind
          , inferred = inferredKind kind
          , durationShare = (\total -> if total > 0 then Just (fromIntegral agg.durNs / fromIntegral total) else Nothing) =<< totalDurationM
          , stats = statsOf agg
          }
      | (key, agg) <- take nodeCap $ sortOn (\(_, a) -> Down a.reqs) nodeStats
      , let kind = Map.findWithDefault NKUnknown key nodeKinds
      ]
    keptKeys = S.fromList [n.key | n <- keptNodes]
    statsOf agg = mkStats rangeSecs agg.reqs agg.errs agg.hist


-- | Accumulator for merging hops that share a (source, target) or a target.
data HopAgg = HopAgg {reqs :: !Int64, errs :: !Int64, hist :: !LatencyHist, durNs :: !Int64}


instance Semigroup HopAgg where
  a <> b = HopAgg (a.reqs + b.reqs) (a.errs + b.errs) (a.hist <> b.hist) (a.durNs + b.durNs)


instance Monoid HopAgg where
  mempty = HopAgg 0 0 mempty 0


-- | Inferred dependency keys carry their type as a prefix so two different systems can't
-- collide on a bare host name; the label drops it again for display.
nodeLabel :: Text -> Text
nodeLabel k = fromMaybe k $ listToMaybe [v | p <- ["db:", "queue:", "http:"], Just v <- [T.stripPrefix p k]]


-- | Derive one trace's hops from its spans. Pure, so the trace map costs no extra query:
-- the trace view already holds every span it needs.
--
-- The parent walk climbs to the nearest ancestor in a /different/ service rather than
-- matching only the immediate parent, so internal spans between a client call and the
-- server that answers it don't sever the hop.
traceEdgeSamples :: V.Vector SpanRecord -> ([EdgeSample], Int64)
traceEdgeSamples spans = (entryEdges <> serviceEdges <> inferredEdges, totalDuration)
  where
    totalDuration = sum [fromIntegral s.spanDurationNs | s <- V.toList spans]
    bySpanId = Map.fromList [(s.spanId, s) | s <- V.toList spans]
    serviceOf s = fromMaybe "unknown" $ atMapText "service.name" s.resource
    isServerLike s = s.kind == Just SKServer || s.kind == Just SKConsumer
    isClientLike s = s.kind == Just SKClient || s.kind == Just SKProducer
    errCount s = if s.status == Just SSError then 1 else 0
    sampleOf src tgt s =
      EdgeSample src tgt 1 (errCount s) (singletonLatency (fromIntegral s.spanDurationNs)) (fromIntegral s.spanDurationNs)

    -- Nearest ancestor in another service, if any.
    crossServiceParent s = go (s.parentSpanId >>= (`Map.lookup` bySpanId)) (0 :: Int)
      where
        go Nothing _ = Nothing
        go (Just p) depth
          | depth > 64 = Nothing -- self-referential parent ids exist in malformed traces
          | serviceOf p /= serviceOf s = Just p
          | otherwise = go (p.parentSpanId >>= (`Map.lookup` bySpanId)) (depth + 1)

    servers = [s | s <- V.toList spans, isServerLike s]
    serviceEdges =
      [ sampleOf (serviceOf p, NKService) (serviceOf s, NKService) s
      | s <- servers
      , Just p <- [crossServiceParent s]
      , isClientLike p
      ]
    entryEdges =
      [ sampleOf ("", NKEntry) (serviceOf s, NKService) s
      | s <- servers
      , isNothing (crossServiceParent s)
      ]

    -- A client/producer span nobody answered is a call out of the instrumented world.
    answered = S.fromList [p.spanId | s <- servers, Just p <- [crossServiceParent s]]
    inferredEdges =
      [ sampleOf (serviceOf s, NKService) (inferredPeer s) s
      | s <- V.toList spans
      , isClientLike s
      , not (s.spanId `S.member` answered)
      ]


-- | One rolled-up edge for a single bucket. Kinds travel with the keys so the read path
-- rebuilds node shapes without re-deriving them.
data RollupEdge = RollupEdge
  { sourceKey :: !Text
  , sourceKind :: !NodeKind
  , targetKey :: !Text
  , targetKind :: !NodeKind
  , reqCount :: !Int64
  , errorCount :: !Int64
  , sumDurationNs :: !Int64
  , latHist :: !LatencyHist
  }
  deriving stock (Generic, Show)


-- | One @(edge, latency bucket)@ group as the slice query returns it. Assembling the
-- histogram in Haskell instead of in SQL keeps the statement to plain aggregates that
-- Postgres and DataFusion both plan the same way — no JSON construction, no array_agg of
-- ragged arrays.
data SliceRow = SliceRow
  { src :: !Text
  , srcKind :: !Text
  , tgt :: !Text
  , tgtKind :: !Text
  , latBucket :: !Int64
  , n :: !Int64
  , errs :: !Int64
  , durNs :: !Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


parseNodeKind :: Text -> NodeKind
parseNodeKind = \case
  "entry" -> NKEntry
  "service" -> NKService
  "database" -> NKDatabase
  "queue" -> NKQueue
  "external" -> NKExternal
  _ -> NKUnknown


sliceRowsToEdges :: [SliceRow] -> [RollupEdge]
sliceRowsToEdges rows =
  [ RollupEdge sk (parseNodeKind skk) tk (parseNodeKind tkk) agg.reqs agg.errs agg.durNs agg.hist
  | ((sk, skk, tk, tkk), agg) <- Map.toList grouped
  ]
  where
    grouped =
      Map.fromListWith
        (<>)
        [ ((r.src, r.srcKind, r.tgt, r.tgtKind), HopAgg r.n r.errs (LatencyHist $ one (fromIntegral r.latBucket, r.n)) r.durNs)
        | r <- rows
        ]


-- | Which projects sent a span this slice that could form an edge. The dispatcher fans the
-- rollup out over this rather than over every active project: the self-join costs the same
-- scan whether or not the project has traffic, and the overwhelming majority of projects
-- have none, so fanning out blindly spends ~all of the budget proving there is nothing to
-- do. One scan answers for every project at once.
--
-- Deliberately the same store, window and @kind@ filter as 'rollupServiceEdges' — this is
-- that query's @FROM@ clause with the per-project predicate lifted out, so a project is on
-- this list exactly when the rollup would have found rows for it. A project that drops off
-- mid-window still gets its bucket; one that has never sent a span never gets a job.
--
-- Rows with an unparseable @project_id@ are dropped rather than failing the tick: the span
-- store's column is text, and one malformed value must not cost every other project its
-- bucket.
projectsWithSpansInRange :: (DB es, Labeled "timefusion" Hasql :> es) => Bool -> UTCTime -> UTCTime -> Eff es [Projects.ProjectId]
projectsWithSpansInRange useTf lo hi =
  fmap (mapMaybe (idFromText @"project"))
    $ Hasql.withHasqlTimefusion useTf
    $ Hasql.interp
      [HI.sql|
      SELECT DISTINCT project_id
      FROM otel_logs_and_spans
      WHERE timestamp >= #{lo} AND timestamp < #{hi}
        AND kind IN ('server','client','producer','consumer')|]


-- | Derive one closed time slice's edges straight from the span table. This is the only
-- place the expensive self-join runs, and it runs once per project per slice rather than
-- once per page view.
--
-- @kind IN (…)@ rather than an OR-of-equalities: TimeFusion's Utf8View OR predicate returns
-- wrong rows, and @kind@ is raw-indexed so @IN@ routes through the index.
--
-- Every column @sp@ projects is aliased, including the ones whose base name would do. @sp@
-- is self-joined, and DataFusion cannot resolve @c.kind@\/@p.kind@ when both sides carry the
-- unqualified base-table name — the whole query fails to plan with \"Ambiguous reference to
-- unqualified field kind\". Renaming to @knd@\/@stc@\/@dur@\/@nm@ is what makes the qualifiers
-- bind. Do not \"tidy\" an alias away.
--
-- @ORDER BY COUNT(*)@ rather than the ordinal @ORDER BY 6@ for the same reason: DataFusion
-- rejects an ordinal that points at an aggregate.
rollupServiceEdges :: (DB es, Labeled "timefusion" Hasql :> es) => Bool -> Projects.ProjectId -> UTCTime -> UTCTime -> Eff es [RollupEdge]
rollupServiceEdges useTf pid lo hi =
  fmap sliceRowsToEdges
    $ Hasql.withHasqlTimefusion useTf
    $ Hasql.interp
      [HI.sql|
      WITH sp AS (
        SELECT context___trace_id tid, context___span_id sid, parent_id par,
               COALESCE(resource___service___name, 'unknown') svc, kind knd, status_code stc, duration dur, name nm,
               attributes___db___system___name db_sys, attributes___db___namespace db_ns,
               attributes___server___address srv, attributes___network___peer___address peer
        FROM otel_logs_and_spans
        WHERE project_id = #{pid.toText} AND timestamp >= #{lo} AND timestamp < #{hi}
          AND kind IN ('server','client','producer','consumer')
      ),
      hops AS (
        SELECT p.svc src, 'service' src_kind, c.svc tgt, 'service' tgt_kind, c.stc st, c.dur dur
        FROM sp c JOIN sp p ON c.tid = p.tid AND c.par = p.sid
        WHERE c.knd IN ('server','consumer') AND p.knd IN ('client','producer')
        UNION ALL
        SELECT p.svc, 'service',
               -- A purely numeric db.namespace is a Redis database index, not a name.
               CASE WHEN p.db_ns IS NOT NULL AND p.db_ns <> '' AND p.db_ns !~ '^[0-9]+$' THEN 'db:' || p.db_ns
                    WHEN p.db_sys IS NOT NULL AND p.db_sys <> '' THEN 'db:' || p.db_sys
                    WHEN p.db_ns IS NOT NULL AND p.db_ns <> '' THEN 'db:' || p.db_ns
                    WHEN p.knd = 'producer' THEN 'queue:' || COALESCE(NULLIF(LOWER(p.srv), ''), p.nm)
                    ELSE 'http:' || COALESCE(NULLIF(LOWER(p.srv), ''), NULLIF(LOWER(p.peer), ''), p.nm) END,
               CASE WHEN (p.db_ns IS NOT NULL AND p.db_ns <> '') OR (p.db_sys IS NOT NULL AND p.db_sys <> '') THEN 'database'
                    WHEN p.knd = 'producer' THEN 'queue' ELSE 'external' END,
               p.stc, p.dur
        FROM sp p
        WHERE p.knd IN ('client','producer')
          AND NOT EXISTS (SELECT 1 FROM sp c WHERE c.tid = p.tid AND c.par = p.sid AND c.knd IN ('server','consumer'))
        UNION ALL
        SELECT '', 'entry', c.svc, 'service', c.stc, c.dur
        FROM sp c
        WHERE c.knd IN ('server','consumer')
          AND (c.par IS NULL OR c.par = '' OR NOT EXISTS (SELECT 1 FROM sp p WHERE p.tid = c.tid AND p.sid = c.par))
      )
      SELECT src, src_kind, tgt, tgt_kind, bkt,
             COUNT(*)::int8,
             COUNT(*) FILTER (WHERE st = 'ERROR')::int8,
             COALESCE(SUM(dur), 0)::int8
      FROM (
        SELECT src, src_kind, tgt, tgt_kind, st, dur,
               CAST(FLOOR(4 * LOG(2, GREATEST(COALESCE(dur, 0) / 1000, 1))) AS int8) bkt
        FROM hops
      ) b
      GROUP BY src, src_kind, tgt, tgt_kind, bkt
      ORDER BY COUNT(*) DESC
      LIMIT 20000|]


-- | Replace-on-conflict so re-rolling a bucket (the hourly catch-up pass, or a DLQ replay
-- landing late) is idempotent rather than double-counting.
upsertServiceDependencyEdges :: DB es => Projects.ProjectId -> UTCTime -> [RollupEdge] -> Eff es ()
upsertServiceDependencyEdges _ _ [] = pass
upsertServiceDependencyEdges pid bucket es =
  Hasql.interpExecute_
    [HI.sql|
    INSERT INTO apis.service_dependency_edges
      (project_id, bucket, source_key, source_kind, target_key, target_kind,
       req_count, error_count, sum_duration_ns, lat_hist)
    SELECT #{pid.unUUIDId}, #{bucket}, s, sk, t, tk, r, e, d, h::jsonb
    FROM unnest(#{srcs}::text[], #{srcKinds}::text[], #{tgts}::text[], #{tgtKinds}::text[],
                #{reqs}::int8[], #{errsV}::int8[], #{durs}::int8[], #{hists}::text[])
         AS u(s, sk, t, tk, r, e, d, h)
    ON CONFLICT (project_id, bucket, source_key, source_kind, target_key, target_kind)
    DO UPDATE SET req_count = EXCLUDED.req_count, error_count = EXCLUDED.error_count,
                  sum_duration_ns = EXCLUDED.sum_duration_ns,
                  lat_hist = EXCLUDED.lat_hist,
                  updated_at = now()|]
  where
    srcs = V.fromList $ (.sourceKey) <$> es
    srcKinds = V.fromList $ kindText . (.sourceKind) <$> es
    tgts = V.fromList $ (.targetKey) <$> es
    tgtKinds = V.fromList $ kindText . (.targetKind) <$> es
    reqs = V.fromList $ (.reqCount) <$> es
    errsV = V.fromList $ (.errorCount) <$> es
    durs = V.fromList $ (.sumDurationNs) <$> es
    hists = V.fromList $ decodeUtf8 @Text . AE.encode . histObject . (.latHist) <$> es


histObject :: LatencyHist -> AE.Value
histObject (LatencyHist h) = AE.object [AEKey.fromString (show idx) AE..= c | (idx, c) <- IntMap.toList h]


histFromObject :: Map Text Int64 -> LatencyHist
histFromObject m = LatencyHist $ IntMap.fromListWith (+) [(idx, c) | (k, c) <- Map.toList m, Just idx <- [readMaybe (toString k)]]


kindText :: NodeKind -> Text
kindText = \case
  NKEntry -> "entry"
  NKService -> "service"
  NKDatabase -> "database"
  NKQueue -> "queue"
  NKExternal -> "external"
  NKUnknown -> "unknown"


-- | Read the rollup for a range and fold it into a graph. Cheap: a day is a few thousand
-- narrow Postgres rows, merged in memory.
serviceGraphForRange :: DB es => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es ServiceGraph
serviceGraphForRange pid lo hi = do
  rows <-
    Hasql.interp
      [HI.sql|
      SELECT source_key, source_kind, target_key, target_kind,
             req_count, error_count, sum_duration_ns, lat_hist
      FROM apis.service_dependency_edges
      WHERE project_id = #{pid.unUUIDId} AND bucket >= #{lo} AND bucket < #{hi}|]
  let rangeSecs = realToFrac (diffUTCTime hi lo)
      samples =
        [ EdgeSample (sk, parseNodeKind skk) (tk, parseNodeKind tkk) r e (histFromObject h) d
        | (sk, skk, tk, tkk, r, e, d, AesonText h) <- rows
        ]
  pure $ buildServiceGraph rangeSecs serviceMapNodeCap Nothing samples


-- | Name an uninstrumented dependency the way its own ecosystem would: the database or
-- namespace it serves, the destination a message went to, else the host that was dialled.
inferredPeer :: SpanRecord -> (Text, NodeKind)
inferredPeer s = case (dbName, attr "messaging.destination.name") of
  (Just db, _) -> ("db:" <> db, NKDatabase)
  (Nothing, Just dest) -> ("queue:" <> dest, NKQueue)
  (Nothing, Nothing)
    | s.kind == Just SKProducer -> ("queue:" <> peerHost, NKQueue)
    | otherwise -> ("http:" <> peerHost, NKExternal)
  where
    attr k = T.strip <$> (atMapText k s.attributes >>= guarded (not . T.null) . T.strip)
    -- Redis and friends report db.namespace as the numeric database index, so a purely
    -- numeric namespace names the node "0" — true, and useless on a map. Fall back to the
    -- system ("redis", "postgresql") and only qualify with the namespace when it is a name.
    dbSystem = attr "db.system.name" <|> attr "db.system"
    dbName = case (guarded (not . T.all isDigit) =<< attr "db.namespace", dbSystem) of
      (Just ns, _) -> Just ns
      (Nothing, Just sys) -> Just sys
      (Nothing, Nothing) -> attr "db.namespace"
    -- Lower-cased and port-stripped: an inferred node keyed on a raw host is a
    -- cardinality bomb, and "API:443" and "api" are the same dependency.
    peerHost =
      maybe s.spanName (T.toLower . T.takeWhile (/= ':'))
        $ attr "server.address"
        <|> attr "network.peer.address"
        <|> attr "peer.service"
