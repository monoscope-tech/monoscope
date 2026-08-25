-- | The running-container inventory behind the Containers page.
--
-- Datadog's Containers Explorer is one table for every runtime — Kubernetes and Docker
-- containers sit side by side and the runtime is a facet, not a separate page. We copy that,
-- which means normalising two OpenTelemetry receiver families onto one row:
--
--   * @kubeletstats@ + @k8s_cluster@ emit @container.cpu.usage@ in cores, memory in bytes,
--     and the requests/limits/restarts/ready signals under @k8s.container.*@.
--   * @docker_stats@ emits @container.cpu.utilization@ and @container.memory.usage.*@, and
--     has no notion of a request or a restart count.
--
-- Everything is read from @otel_metrics@, which already carries flattened resource columns
-- for the identity fields. Only node, image and workload live in the resource blob, and that
-- blob is a Variant on TimeFusion but JSONB on Postgres — the one place a dialect matters.
module Models.Telemetry.Containers (
  ContainerRow (..),
  Runtime (..),
  runtimeOf,
  cpuPctOfLimit,
  memPctOfLimit,
  containerMetricNames,
  containersInWindow,
  containerListWindow,
) where

import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Data.String (fromString)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Labeled (Labeled)
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Relude
import System.Types (DB)


-- | One container as the list renders it. Kubernetes-only signals are 'Nothing' on a Docker
-- row and render as an em dash — never as a zero, because Datadog is explicit that a missing
-- denominator means "cannot infer", not "0%", and a fabricated 0% during an incident is worse
-- than a blank.
data ContainerRow = ContainerRow
  { containerName :: Text
  , podName :: Maybe Text
  , namespace :: Maybe Text
  , nodeName :: Maybe Text
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


data Runtime = Kubernetes | Docker
  deriving stock (Bounded, Enum, Eq, Ord, Show)


-- | A pod name is the discriminator: kubeletstats and k8s_cluster always set it, docker_stats
-- never can. Deriving the runtime rather than storing it keeps the two from disagreeing.
--
-- >>> runtimeOf (emptyRow "c") {podName = Just "web-0"}
-- Kubernetes
-- >>> runtimeOf (emptyRow "c")
-- Docker
runtimeOf :: ContainerRow -> Runtime
runtimeOf r = maybe Docker (const Kubernetes) r.podName


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
-- docker_stats' @container.memory.usage.total@ and they mean different things. Never an
-- OR-chain either — TimeFusion miscomputes chained equality on its @Utf8View@ string columns
-- and silently returns a near-empty result, so this must lower to a single @IN@.
-- These are compile-time constants, so rendering them into the statement rather than binding
-- them as a parameter carries no injection risk and keeps the predicate a plain @IN@ that both
-- stores can prune on.
--
-- >>> metricNameInList
-- "('container.cpu.usage','container.cpu.utilization','container.memory.working_set','container.memory.usage.total','container.memory.usage.limit','k8s.container.cpu_limit','k8s.container.cpu_request','k8s.container.memory_limit','k8s.container.memory_request','k8s.container.restarts','k8s.container.ready')"
containerMetricNames :: [Text]
containerMetricNames =
  [ "container.cpu.usage"
  , "container.cpu.utilization"
  , "container.memory.working_set"
  , "container.memory.usage.total"
  , "container.memory.usage.limit"
  , "k8s.container.cpu_limit"
  , "k8s.container.cpu_request"
  , "k8s.container.memory_limit"
  , "k8s.container.memory_request"
  , "k8s.container.restarts"
  , "k8s.container.ready"
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
resourcePath useTimefusion path
  | useTimefusion = "variant_get(resource,'" <> T.intercalate "." path <> "','Utf8')"
  | otherwise = "resource #>> '{" <> T.intercalate "," path <> "}'"


-- | Every container seen in the trailing window, newest datapoint per series.
--
-- One pass: a window function picks the latest datapoint per (container, metric), then
-- conditional aggregates pivot the metrics into columns. No self-join — that is the shape
-- that kills TimeFusion.
--
-- The grouping key carries the node or host, not just the container name, because Docker
-- container names collide across the nodes of a Swarm and pod names collide across clusters.
-- Descriptive fields are aggregated rather than grouped on, so a container whose
-- @k8s.container.*@ rows carry no image does not split into two rows.
containersInWindow
  :: (DB es, Labeled "timefusion" Hasql :> es)
  => Bool -> Projects.ProjectId -> UTCTime -> Eff es (V.Vector ContainerRow)
containersInWindow useTimefusion pid now =
  Hasql.withHasqlTimefusion useTimefusion
    $ V.fromList
    <$> Hasql.interp
      ( [HI.sql|
      WITH latest AS (
        SELECT
          COALESCE(resource___k8s___container___name, resource___container___name) AS container_name,
          resource___k8s___pod___name AS pod_name,
          resource___k8s___namespace___name AS namespace,
          COALESCE(|]
          <> raw (resourcePath useTimefusion ["k8s", "node", "name"])
          <> [HI.sql|, resource___host___name) AS node_name,
          |]
          <> raw (resourcePath useTimefusion ["container", "image", "name"])
          <> [HI.sql| AS image,
          |]
          <> raw (resourcePath useTimefusion ["container", "image", "tag"])
          <> [HI.sql| AS image_tag,
          |]
          <> raw (resourcePath useTimefusion ["k8s", "deployment", "name"])
          <> [HI.sql| AS workload,
          metric_name, value,
          row_number() OVER (
            PARTITION BY
              COALESCE(resource___k8s___pod___name, resource___host___name),
              COALESCE(resource___k8s___container___name, resource___container___name),
              metric_name
            ORDER BY timestamp DESC) AS rn
        FROM otel_metrics
        WHERE project_id = #{pid.toText}
          AND timestamp > #{since}
          AND metric_name IN |]
          <> raw metricNameInList
          <> [HI.sql|
          AND COALESCE(resource___k8s___container___name, resource___container___name) IS NOT NULL
      )
      SELECT
        container_name,
        MAX(pod_name), MAX(namespace), MAX(node_name),
        MAX(image), MAX(image_tag), MAX(workload),
        -- Docker reports CPU with Docker's own formula, percent-of-a-single-core, so 200 means
        -- two full cores. Dividing by 100 puts it on the same cores axis as Kubernetes.
        COALESCE(
          MAX(CASE WHEN metric_name = 'container.cpu.usage' THEN value END),
          MAX(CASE WHEN metric_name = 'container.cpu.utilization' THEN value END) / 100),
        MAX(CASE WHEN metric_name = 'k8s.container.cpu_limit' THEN value END),
        MAX(CASE WHEN metric_name = 'k8s.container.cpu_request' THEN value END),
        -- Working set, not usage: it is what the kubelet's eviction and OOM logic acts on.
        COALESCE(
          MAX(CASE WHEN metric_name = 'container.memory.working_set' THEN value END),
          MAX(CASE WHEN metric_name = 'container.memory.usage.total' THEN value END)),
        COALESCE(
          MAX(CASE WHEN metric_name = 'k8s.container.memory_limit' THEN value END),
          MAX(CASE WHEN metric_name = 'container.memory.usage.limit' THEN value END)),
        MAX(CASE WHEN metric_name = 'k8s.container.memory_request' THEN value END),
        MAX(CASE WHEN metric_name = 'k8s.container.restarts' THEN value END),
        MAX(CASE WHEN metric_name = 'k8s.container.ready' THEN value END)
      FROM latest
      WHERE rn = 1
      GROUP BY container_name, COALESCE(pod_name, node_name)
      ORDER BY container_name
      LIMIT #{containerListLimit}|]
      )
  where
    since = addUTCTime (negate containerListWindow) now
    raw = fromString . toString


-- $setup
-- >>> :set -XOverloadedStrings -XOverloadedRecordDot
-- >>> let emptyRow n = ContainerRow n Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
