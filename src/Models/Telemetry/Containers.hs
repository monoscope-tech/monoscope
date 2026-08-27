-- | The running-container inventory behind the Containers page.
--
-- Datadog's Containers Explorer is one table for every runtime — Kubernetes and Docker
-- containers sit side by side and the runtime is a facet, not a separate page. We copy that,
-- which means normalising four receiver families onto one row:
--
--   * @kubeletstats@ (container group) + @k8s_cluster@ emit @container.cpu.usage@ in cores,
--     memory in bytes, and requests/limits/restarts/ready under @k8s.container.*@.
--   * @kubeletstats@ (pod group only) emits @k8s.pod.*@ and no container metrics at all. A
--     collector configured that way used to render an empty page; those pods now stand in
--     for their own containers.
--   * @docker_stats@ — plain Docker and Swarm — emits @container.cpu.utilization@ and
--     @container.memory.usage.*@, and has no notion of a request or a restart count.
--   * @hostmetrics@ on a bare node or VM emits @system.*@ and knows nothing of containers.
--     A host is its own workload: usage is what it burns, capacity is its limit.
--
-- __Scope comes from the metric family, never from which resource attributes are present.__
-- A collector daemonset scraping @hostmetrics@ inside Kubernetes has @k8s.pod.name@ stamped
-- on its @system.*@ rows by @k8sattributes@; keying those off the resource would file the
-- node under the collector's own pod and then dedup it away. The metric says what a row is
-- about, the resource only says which one.
--
-- Everything is read from @otel_metrics@, which already carries flattened resource columns
-- for the identity fields. Only node, image and workload live in the resource blob, and that
-- blob is a Variant on TimeFusion but JSONB on Postgres — the one place a dialect matters.
module Models.Telemetry.Containers (
  ContainerRow (..),
  Scope (..),
  Runtime (..),
  runtimeOf,
  cpuPctOfLimit,
  memPctOfLimit,
  containerMetricNames,
  imageAndTag,
  swarmService,
  dropShadowed,
  containersInWindow,
  containerListWindow,
) where

import Data.Char (isAlpha, isAsciiLower, isDigit)
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Labeled (Labeled)
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (WrappedEnumSC (..))
import Relude
import System.Types (DB)


-- | One container as the list renders it. Kubernetes-only signals are 'Nothing' on a Docker
-- row and render as an em dash — never as a zero, because Datadog is explicit that a missing
-- denominator means "cannot infer", not "0%", and a fabricated 0% during an incident is worse
-- than a blank.
data ContainerRow = ContainerRow
  { containerName :: Text
  , scope :: Scope
  , podName :: Maybe Text
  , namespace :: Maybe Text
  , nodeName :: Maybe Text
  , cluster :: Maybe Text
  , image :: Maybe Text
  , imageTag :: Maybe Text
  , workload :: Maybe Text
  , cpuCores :: Maybe Double
  , cpuLimit :: Maybe Double
  , cpuRequest :: Maybe Double
  , memBytes :: Maybe Double
  , memLimit :: Maybe Double
  , memRequest :: Maybe Double
  , restarts :: Maybe Double
  , ready :: Maybe Double
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (HI.DecodeRow, NFData)


-- | What a row actually measures, decided by the metric family it was built from. Carried on
-- the row rather than inferred, because only the query knows it — and because 'dropShadowed'
-- has to tell a pod that is standing in for its containers from one that is not.
data Scope = ScopeContainer | ScopePod | ScopeHost
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass (NFData)
  deriving (HI.DecodeValue) via WrappedEnumSC 'Nothing "Scope" Scope


data Runtime = Kubernetes | Docker | Host
  deriving stock (Bounded, Enum, Eq, Ord, Show)


-- | Scope decides the runtime, except that a container may come from either kubeletstats or
-- docker_stats — and only there is the pod name the honest discriminator.
--
-- >>> runtimeOf (emptyRow "c") {podName = Just "web-0"}
-- Kubernetes
-- >>> runtimeOf (emptyRow "c")
-- Docker
-- >>> runtimeOf (emptyRow "node-1") {scope = ScopeHost}
-- Host
-- >>> runtimeOf (emptyRow "web-0") {scope = ScopePod}
-- Kubernetes
runtimeOf :: ContainerRow -> Runtime
runtimeOf r = case r.scope of
  ScopeHost -> Host
  ScopePod -> Kubernetes
  ScopeContainer -> maybe Docker (const Kubernetes) r.podName


-- | Usage over limit as a 0..1 fraction, or 'Nothing' when the limit is absent or zero.
-- Datadog stores these as floats where 1.0 is 100% and lets them exceed 1.0; so do we.
--
-- >>> ratio (Just 1.5) (Just 2)
-- Just 0.75
-- >>> ratio (Just 1.5) (Just 0)
-- Nothing
-- >>> ratio Nothing (Just 2)
-- Nothing
ratio :: Maybe Double -> Maybe Double -> Maybe Double
ratio usage limit = do
  u <- usage
  l <- limit
  guard (l > 0)
  pure (u / l)


cpuPctOfLimit, memPctOfLimit :: ContainerRow -> Maybe Double
cpuPctOfLimit r = ratio r.cpuCores r.cpuLimit
memPctOfLimit r = ratio r.memBytes r.memLimit


-- | Exactly the metrics the list needs, named in full.
--
-- Never a @LIKE 'container.%'@: kubeletstats' @container.memory.usage@ is a strict prefix of
-- docker_stats' @container.memory.usage.total@ and they mean different things. Naming both in
-- full is what makes it safe to read both. Never an
-- OR-chain either — TimeFusion miscomputes chained equality on its @Utf8View@ string columns
-- and silently returns a near-empty result, so this must lower to a single @IN@.
-- These are compile-time constants, so rendering them into the statement rather than binding
-- them as a parameter carries no injection risk and keeps the predicate a plain @IN@ that both
-- stores can prune on.
--
-- >>> metricNameInList
-- "('container.cpu.usage','container.cpu.utilization','container.memory.working_set','container.memory.usage.total','container.memory.usage','container.memory.usage.limit','k8s.container.cpu_limit','k8s.container.cpu_request','k8s.container.memory_limit','k8s.container.memory_request','k8s.container.restarts','k8s.container.ready','k8s.pod.cpu.usage','k8s.pod.memory.working_set','k8s.pod.memory.usage','system.cpu.utilization','system.memory.usage')"
containerMetricNames :: [Text]
containerMetricNames =
  [ "container.cpu.usage"
  , "container.cpu.utilization"
  , "container.memory.working_set"
  , "container.memory.usage.total"
  , "container.memory.usage"
  , "container.memory.usage.limit"
  , "k8s.container.cpu_limit"
  , "k8s.container.cpu_request"
  , "k8s.container.memory_limit"
  , "k8s.container.memory_request"
  , "k8s.container.restarts"
  , "k8s.container.ready"
  , -- kubeletstats with metric_groups omitting @container@: the pod is all there is.
    "k8s.pod.cpu.usage"
  , "k8s.pod.memory.working_set"
  , "k8s.pod.memory.usage"
  , -- hostmetrics on a bare node. Both are dimensioned — CPU per logical core and mode,
    -- memory per state — which is why the pivot below aggregates them differently.
    "system.cpu.utilization"
  , "system.memory.usage"
  ]


metricNameInList :: Text
metricNameInList = "(" <> T.intercalate "," (map (\n -> "'" <> n <> "'") containerMetricNames) <> ")"


-- | How far back the list looks. A container list answers "what is running right now", so it
-- deliberately does not accept a user-supplied range: a wide window over @otel_metrics@ is the
-- query shape that has repeatedly OOM-killed TimeFusion in production.
containerListWindow :: NominalDiffTime
containerListWindow = 900


-- | Cap on rows returned. Filtering and faceting happen in Haskell over this set, so one
-- bounded query serves the table, its facet menus and its counts.
containerListLimit :: Int
containerListLimit = 500


-- | Nested lookup into the resource blob. TimeFusion stores it as a Variant and Postgres as
-- JSONB; neither understands the other's accessor, and on TimeFusion the alternatives all
-- fail outright (@get_field@ and a @::jsonb@ cast both error, and @variant_get@ rejects the
-- type name @'string'@ — it wants an Arrow type).
resourcePath :: Bool -> [Text] -> Text
resourcePath = blobPath "resource"


-- | The same accessor for the datapoint @attributes@ blob. hostmetrics dimensions its series
-- there — CPU by logical core and mode, memory by state — so the pivot has to read it.
blobPath :: Text -> Bool -> [Text] -> Text
blobPath col useTimefusion path
  | useTimefusion = "variant_get(" <> col <> ",'" <> T.intercalate "." path <> "','Utf8')"
  | otherwise = col <> " #>> '{" <> T.intercalate "," path <> "}'"


-- | Split a tag off an image reference. docker_stats sends the whole @repo:tag@ string as
-- @container.image.name@ and no @container.image.tag@ at all, so without this the Image
-- column reads @repo:tag@ for Docker and a bare repo for Kubernetes, the Tag column is
-- blank for 39% of production containers, and the Image facet lists one entry per tag.
--
-- The colon only separates a tag when it comes after the last @\/@ — a registry may carry a
-- port. A @\@sha256:@ digest is not a tag and is left whole.
--
-- >>> imageAndTag "docker.redpanda.com/redpandadata/redpanda:v25.1.4"
-- ("docker.redpanda.com/redpandadata/redpanda",Just "v25.1.4")
-- >>> imageAndTag "redis:7"
-- ("redis",Just "7")
-- >>> imageAndTag "redis"
-- ("redis",Nothing)
-- >>> imageAndTag "registry:5000/app"
-- ("registry:5000/app",Nothing)
-- >>> imageAndTag "registry:5000/app:1.2"
-- ("registry:5000/app",Just "1.2")
-- >>> imageAndTag "ghcr.io/acme/api@sha256:abc123"
-- ("ghcr.io/acme/api@sha256:abc123",Nothing)
imageAndTag :: Text -> (Text, Maybe Text)
imageAndTag img
  | T.isInfixOf "@" img = (img, Nothing)
  | T.null tag = (img, Nothing)
  | otherwise = (registry <> name, Just $ T.drop 1 tag)
  where
    (registry, lastSegment) = T.breakOnEnd "/" img
    (name, tag) = T.breakOn ":" lastSegment


-- | Derive the Swarm service from a task name. Swarm names a task
-- @\<service\>.\<slot\>.\<taskid\>@ for a replicated service and @\<service\>.\<nodeid\>.\<taskid\>@
-- for a global one, and docker_stats reports that task name with no service attribute at all
-- — so every replica looked like an unrelated container with no workload.
--
-- Only a name that really has the task shape is split: the id is 25 lowercase alphanumerics,
-- and the slot is digits or another such id. A container legitimately named with dots keeps
-- its name.
--
-- >>> swarmService "srv-captain--otelcol.3.wcxx3mlmh1q1jpwk9ohyjjv7c"
-- Just "srv-captain--otelcol"
-- >>> swarmService "my.app.web"
-- Nothing
-- >>> swarmService "redis"
-- Nothing
-- >>> swarmService "web.1.short"
-- Nothing
swarmService :: Text -> Maybe Text
swarmService name = case reverse (T.splitOn "." name) of
  taskId : slot : rest@(_ : _)
    | isTaskId taskId
    , T.all isDigit slot || isTaskId slot ->
        Just $ T.intercalate "." (reverse rest)
  _ -> Nothing
  where
    isTaskId t = T.length t == 25 && T.all (\c -> isDigit c || (isAsciiLower c && isAlpha c)) t


-- | Drop pod rollups that their own containers already cover. A collector reporting both
-- @container.*@ and @k8s.pod.*@ produces a row for each container /and/ one for the pod; the
-- pod is only ever a stand-in for containers we cannot see, so it yields to them.
--
-- Shadowing is by pod name, not by workload: two pods of one Deployment are two rows.
--
-- >>> let ctr n p = (emptyRow n) {scope = ScopeContainer, podName = Just p}
-- >>> let pod p = (emptyRow p) {scope = ScopePod, podName = Just p}
-- >>> map (.containerName) $ dropShadowed [ctr "app" "web-0", pod "web-0", pod "db-0"]
-- ["app","db-0"]
-- >>> map (.containerName) $ dropShadowed [pod "web-0"]
-- ["web-0"]
dropShadowed :: [ContainerRow] -> [ContainerRow]
dropShadowed rows = filter keep rows
  where
    covered = S.fromList [p | r <- rows, r.scope == ScopeContainer, Just p <- [r.podName]]
    keep r = r.scope /= ScopePod || maybe True (`S.notMember` covered) r.podName


-- | Fill in what a receiver left implicit, so a row means the same thing whichever collector
-- produced it. Applied once, at the query, so the table, the facet menus, the filters and the
-- detail drawer all agree on the values.
normalizeRow :: ContainerRow -> ContainerRow
normalizeRow r = withWorkload $ case r.imageTag of
  Just _ -> r
  Nothing -> maybe r (\img -> let (i, t) = imageAndTag img in r{image = Just i, imageTag = t}) r.image
  where
    withWorkload row = case row.workload of
      Just _ -> row
      Nothing -> row{workload = swarmService row.containerName}


-- | Every container, stand-in pod and bare host seen in the trailing window, newest datapoint
-- per series.
--
-- One pass: a window function picks the latest datapoint per series, then conditional
-- aggregates pivot the metrics into columns. No self-join — that is the shape that kills
-- TimeFusion.
--
-- The grouping key carries the node or host, not just the container name, because Docker
-- container names collide across the nodes of a Swarm and pod names collide across clusters.
-- Descriptive fields are aggregated rather than grouped on, so a container whose
-- @k8s.container.*@ rows carry no image does not split into two rows.
--
-- The window partition includes the datapoint dimensions, which container metrics do not have
-- and host metrics do: @system.cpu.utilization@ is one series per logical core per mode and
-- @system.memory.usage@ one per state. Without them the row-number would keep a single
-- arbitrary core and the pivot would read a core's idle time as the node's CPU usage.
containersInWindow
  :: (DB es, Labeled "timefusion" Hasql :> es)
  => Bool -> Projects.ProjectId -> UTCTime -> Eff es (V.Vector ContainerRow)
containersInWindow useTimefusion pid now =
  Hasql.withHasqlTimefusion useTimefusion
    $ V.fromList
    . dropShadowed
    . map normalizeRow
    <$> Hasql.interp
      ( [HI.sql|
      WITH latest AS (
        SELECT |]
          <> raw scopeExpr
          <> [HI.sql| AS scope,
          |]
          <> raw identityExpr
          <> [HI.sql| AS container_name,
          -- Everything below describes a container or a pod. On a host row the resource may
          -- still carry the collector's own pod, namespace and workload; blanking them keeps
          -- a node from claiming to be whatever pod happened to scrape it.
          CASE WHEN |]
          <> raw isHost
          <> [HI.sql| THEN NULL ELSE resource___k8s___pod___name END AS pod_name,
          CASE WHEN |]
          <> raw isHost
          <> [HI.sql| THEN NULL ELSE resource___k8s___namespace___name END AS namespace,
          CASE WHEN |]
          <> raw isHost
          <> [HI.sql| THEN resource___host___name ELSE COALESCE(|]
          <> raw (resourcePath useTimefusion ["k8s", "node", "name"])
          <> [HI.sql|, resource___host___name) END AS node_name,
          -- No receiver emits k8s.cluster.name: a cluster has no name in the API, so it
          -- only exists if the collector's resource processor sets it. Fall back to the
          -- uid, which k8sattributes does supply, so the facet is populated either way.
          CASE WHEN |]
          <> raw isHost
          <> [HI.sql| THEN NULL ELSE COALESCE(|]
          <> raw (resourcePath useTimefusion ["k8s", "cluster", "name"])
          <> [HI.sql|, |]
          <> raw (resourcePath useTimefusion ["k8s", "cluster", "uid"])
          <> [HI.sql|) END AS cluster,
          -- Only a container has an image. A pod rollup covers every container in the pod and
          -- a host none at all, so both must read blank rather than borrow one.
          CASE WHEN |]
          <> raw isContainer
          <> [HI.sql| THEN |]
          <> raw (resourcePath useTimefusion ["container", "image", "name"])
          <> [HI.sql| END AS image,
          CASE WHEN |]
          <> raw isContainer
          <> [HI.sql| THEN |]
          <> raw (resourcePath useTimefusion ["container", "image", "tag"])
          <> [HI.sql| END AS image_tag,
          -- k8sattributes names the controller after its kind, so a Deployment is the only
          -- kind our own shipped config could ever produce. Ladder every kind a pod can
          -- have, most specific first: without this a DaemonSet or StatefulSet container
          -- shows no workload at all, which is 20% of the Kubernetes rows in production.
          -- ReplicaSet is last because k8sattributes sets it alongside the Deployment that
          -- owns it, and the Deployment name is the one a human recognises.
          CASE WHEN |]
          <> raw isHost
          <> [HI.sql| THEN NULL ELSE COALESCE(|]
          <> raw (T.intercalate ", " [resourcePath useTimefusion ["k8s", k, "name"] | k <- ["deployment", "statefulset", "daemonset", "cronjob", "job", "replicaset"]])
          <> [HI.sql|) END AS workload,
          metric_name, value,
          |]
          <> raw cpuMode
          <> [HI.sql| AS cpu_mode,
          |]
          <> raw cpuNum
          <> [HI.sql| AS cpu_num,
          |]
          <> raw memState
          <> [HI.sql| AS mem_state,
          row_number() OVER (
            PARTITION BY |]
          <> raw groupKeyExpr
          <> [HI.sql|, |]
          <> raw identityExpr
          <> [HI.sql|, metric_name, |]
          <> raw cpuMode
          <> [HI.sql|, |]
          <> raw cpuNum
          <> [HI.sql|, |]
          <> raw memState
          <> [HI.sql|
            ORDER BY timestamp DESC) AS rn,
          |]
          <> raw groupKeyExpr
          <> [HI.sql| AS group_key
        FROM otel_metrics
        WHERE project_id = #{pid.toText}
          AND timestamp > #{since}
          AND metric_name IN |]
          <> raw metricNameInList
          <> [HI.sql|
          AND |]
          <> raw identityExpr
          <> [HI.sql| IS NOT NULL
      )
      SELECT
        container_name,
        MAX(scope), MAX(pod_name), MAX(namespace), MAX(node_name), MAX(cluster),
        MAX(image), MAX(image_tag), MAX(workload),
        -- Docker reports CPU with Docker's own formula, percent-of-a-single-core, so 200 means
        -- two full cores. Dividing by 100 puts it on the same cores axis as Kubernetes.
        -- A host's usage is the sum of every non-idle mode across every logical core, which is
        -- already in cores for the same reason.
        COALESCE(
          MAX(CASE WHEN metric_name = 'container.cpu.usage' THEN value END),
          MAX(CASE WHEN metric_name = 'container.cpu.utilization' THEN value END) / 100,
          MAX(CASE WHEN metric_name = 'k8s.pod.cpu.usage' THEN value END),
          SUM(CASE WHEN metric_name = 'system.cpu.utilization' AND cpu_mode <> 'idle' THEN value END)),
        -- A node has no quota, so its capacity is the limit: how many cores it has to spend.
        COALESCE(
          MAX(CASE WHEN metric_name = 'k8s.container.cpu_limit' THEN value END),
          CAST(NULLIF(COUNT(DISTINCT CASE WHEN metric_name = 'system.cpu.utilization' AND cpu_mode = 'idle' THEN cpu_num END), 0) AS DOUBLE PRECISION)),
        MAX(CASE WHEN metric_name = 'k8s.container.cpu_request' THEN value END),
        -- Working set, not usage: it is what the kubelet's eviction and OOM logic acts on.
        -- Older hostmetrics builds send system.memory.usage undimensioned; that value is the
        -- last resort, never an addend in the total below, or it would double-count.
        COALESCE(
          MAX(CASE WHEN metric_name = 'container.memory.working_set' THEN value END),
          MAX(CASE WHEN metric_name = 'container.memory.usage.total' THEN value END),
          MAX(CASE WHEN metric_name = 'container.memory.usage' THEN value END),
          MAX(CASE WHEN metric_name = 'k8s.pod.memory.working_set' THEN value END),
          MAX(CASE WHEN metric_name = 'k8s.pod.memory.usage' THEN value END),
          MAX(CASE WHEN metric_name = 'system.memory.usage' AND mem_state = 'used' THEN value END),
          MAX(CASE WHEN metric_name = 'system.memory.usage' AND mem_state IS NULL THEN value END)),
        COALESCE(
          MAX(CASE WHEN metric_name = 'k8s.container.memory_limit' THEN value END),
          MAX(CASE WHEN metric_name = 'container.memory.usage.limit' THEN value END),
          SUM(CASE WHEN metric_name = 'system.memory.usage' AND mem_state IS NOT NULL THEN value END)),
        MAX(CASE WHEN metric_name = 'k8s.container.memory_request' THEN value END),
        MAX(CASE WHEN metric_name = 'k8s.container.restarts' THEN value END),
        MAX(CASE WHEN metric_name = 'k8s.container.ready' THEN value END)
      FROM latest
      WHERE rn = 1
      GROUP BY container_name, group_key
      ORDER BY container_name
      LIMIT #{containerListLimit}|]
      )
  where
    since = addUTCTime (negate containerListWindow) now
    raw = fromString . toString
    isHost = "metric_name LIKE 'system.%'"
    isPod = "metric_name LIKE 'k8s.pod.%'"
    isContainer = "NOT (" <> isHost <> " OR " <> isPod <> ")"
    scopeExpr = "CASE WHEN " <> isHost <> " THEN 'host' WHEN " <> isPod <> " THEN 'pod' ELSE 'container' END"
    identityExpr =
      "CASE WHEN "
        <> isHost
        <> " THEN resource___host___name WHEN "
        <> isPod
        <> " THEN resource___k8s___pod___name ELSE COALESCE(resource___k8s___container___name, resource___container___name) END"
    -- A pod's identity is already unique, so only a container needs the pod or host appended
    -- to separate same-named containers across pods and Swarm nodes.
    groupKeyExpr = "CASE WHEN " <> isHost <> " THEN resource___host___name ELSE COALESCE(resource___k8s___pod___name, resource___host___name) END"
    cpuMode = blobPath "attributes" useTimefusion ["cpu", "mode"]
    cpuNum = blobPath "attributes" useTimefusion ["cpu", "logical_number"]
    memState = blobPath "attributes" useTimefusion ["system", "memory", "state"]


-- $setup
-- >>> :set -XOverloadedStrings -XOverloadedRecordDot
-- >>> let emptyRow n = ContainerRow n ScopeContainer Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
