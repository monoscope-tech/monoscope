module Models.Apis.PrometheusScrapeConfigs (
  PrometheusScrapeConfig (..),
  PrometheusScrapeConfigId,
  insertConfig,
  updateConfig,
  configsByProjectId,
  getConfig,
  getConfigByProject,
  deleteConfig,
  setEnabled,
  toggleEnabled,
  claimDueConfigs,
  markScraped,
  ingestScrapedBody,
  sampleToMetricRecord,
  prometheusScrapeOpts,
) where

import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.Effectful.Hasql qualified as Hasql
import Data.List (partition)
import Data.Text qualified as T
import Data.These qualified as These
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as V
import Effectful (Eff, type (:>))
import Effectful.Ki qualified as Ki
import Effectful.Labeled (Labeled)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Reader.Static qualified as Eff
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.HTTP.Client (managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq (defaults, header)
import Network.Wreq qualified as Wreq
import Pkg.DeriveUtils (UUIDId, unUUIDId)
import Pkg.Prometheus qualified as Prom
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (DB)
import UnliftIO (throwIO)


type PrometheusScrapeConfigId = UUIDId "prometheus_scrape_config"


data PrometheusScrapeConfig = PrometheusScrapeConfig
  { id :: PrometheusScrapeConfigId
  , projectId :: Projects.ProjectId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , name :: Text
  , url :: Text
  , scrapeIntervalSeconds :: Int
  , authHeader :: Maybe Text
  , extraLabels :: AE.Value
  , enabled :: Bool
  , lastScrapedAt :: Maybe UTCTime
  , lastStatus :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow, NFData)


selectCols :: HI.Sql
selectCols = [HI.sql|id, project_id, created_at, updated_at, name, url, scrape_interval_seconds, auth_header, extra_labels, enabled, last_scraped_at, last_status|]


selectFrom :: HI.Sql
selectFrom = [HI.sql|SELECT |] <> selectCols <> [HI.sql| FROM apis.prometheus_scrape_configs |]


insertConfig :: DB es => Projects.ProjectId -> Text -> Text -> Int -> Maybe Text -> AE.Value -> Eff es Int64
insertConfig pid name url interval authHeader extraLabels =
  Hasql.interpExecute
    [HI.sql|INSERT INTO apis.prometheus_scrape_configs (project_id, name, url, scrape_interval_seconds, auth_header, extra_labels)
            VALUES (#{pid}, #{name}, #{url}, #{interval}, #{authHeader}, #{extraLabels})|]


-- | Edit a target in place (preserves id + scrape history). Scoped by project_id.
updateConfig :: DB es => Projects.ProjectId -> PrometheusScrapeConfigId -> Text -> Text -> Int -> Maybe Text -> AE.Value -> Eff es Int64
updateConfig pid cid name url interval authHeader extraLabels =
  Hasql.interpExecute
    [HI.sql|UPDATE apis.prometheus_scrape_configs
            SET name = #{name}, url = #{url}, scrape_interval_seconds = #{interval}, auth_header = #{authHeader}, extra_labels = #{extraLabels}
            WHERE id = #{cid} AND project_id = #{pid}|]


configsByProjectId :: DB es => Projects.ProjectId -> Eff es (V.Vector PrometheusScrapeConfig)
configsByProjectId pid = V.fromList <$> Hasql.interp (selectFrom <> [HI.sql|WHERE project_id = #{pid} ORDER BY created_at DESC|])


-- | Unscoped lookup — only for trusted internal callers that already hold a claimed id
-- (the background scrape worker). Request handlers must use 'getConfigByProject'.
getConfig :: DB es => PrometheusScrapeConfigId -> Eff es (Maybe PrometheusScrapeConfig)
getConfig cid = Hasql.interpOne (selectFrom <> [HI.sql|WHERE id = #{cid}|])


-- | Project-scoped lookup for request handlers, so a foreign id can never load another
-- project's row (and its auth token) into the request context.
getConfigByProject :: DB es => Projects.ProjectId -> PrometheusScrapeConfigId -> Eff es (Maybe PrometheusScrapeConfig)
getConfigByProject pid cid = Hasql.interpOne (selectFrom <> [HI.sql|WHERE id = #{cid} AND project_id = #{pid}|])


deleteConfig :: DB es => Projects.ProjectId -> PrometheusScrapeConfigId -> Eff es Int64
deleteConfig pid cid = Hasql.interpExecute [HI.sql|DELETE FROM apis.prometheus_scrape_configs WHERE id = #{cid} AND project_id = #{pid}|]


setEnabled :: DB es => Projects.ProjectId -> PrometheusScrapeConfigId -> Bool -> Eff es Int64
setEnabled pid cid en = Hasql.interpExecute [HI.sql|UPDATE apis.prometheus_scrape_configs SET enabled = #{en} WHERE id = #{cid} AND project_id = #{pid}|]


-- | Flip enabled atomically in one statement — two concurrent toggles can't read the
-- same value and cancel out (the read-modify-write race a getConfig+setEnabled would have).
toggleEnabled :: DB es => Projects.ProjectId -> PrometheusScrapeConfigId -> Eff es Int64
toggleEnabled pid cid = Hasql.interpExecute [HI.sql|UPDATE apis.prometheus_scrape_configs SET enabled = NOT enabled WHERE id = #{cid} AND project_id = #{pid}|]


-- | Atomically claim up to @limit@ enabled targets whose interval has elapsed (or that
-- were never scraped), advancing @last_scraped_at@ to now() in the SAME statement. The
-- @FOR UPDATE SKIP LOCKED@ subquery makes this safe to run from any number of monoscope
-- nodes/pods concurrently: each due row is handed to exactly one caller, and the lease
-- (the @last_scraped_at@ bump) keeps the next dispatcher tick from re-picking it before
-- its interval elapses — so a crashed worker just retries on the next interval rather than
-- being hammered. Oldest-scraped first so a backlog drains fairly.
claimDueConfigs :: DB es => Int -> Eff es [PrometheusScrapeConfig]
claimDueConfigs limit =
  Hasql.interp
    ( [HI.sql|UPDATE apis.prometheus_scrape_configs SET last_scraped_at = now()
              WHERE id IN (
                SELECT id FROM apis.prometheus_scrape_configs
                WHERE enabled AND (last_scraped_at IS NULL
                      OR last_scraped_at < now() - make_interval(secs => scrape_interval_seconds))
                ORDER BY last_scraped_at ASC NULLS FIRST
                FOR UPDATE SKIP LOCKED
                LIMIT #{limit})
              RETURNING |]
        <> selectCols
    )


-- | Record a scrape outcome. Uses the DB clock for @last_scraped_at@ (not a host
-- @UTCTime@) so a host/DB clock skew can't rewind the lease set by 'claimDueConfigs'
-- and cause the target to be re-claimed before its interval elapses.
markScraped :: DB es => PrometheusScrapeConfigId -> Text -> Eff es Int64
markScraped cid status =
  Hasql.interpExecute [HI.sql|UPDATE apis.prometheus_scrape_configs SET last_scraped_at = now(), last_status = #{status} WHERE id = #{cid}|]


-- | Parse an exposition-format body and ingest its (finite) samples as metrics.
-- Non-finite values (NaN/±Inf) are dropped — Aeson can't encode them — but never
-- silently: the dropped count is logged. Returns the number of samples ingested.
ingestScrapedBody :: (DB es, Eff.Reader AuthContext :> es, Ki.StructuredConcurrency :> es, Labeled "timefusion" Hasql.Hasql :> es, Log :> es) => PrometheusScrapeConfig -> UTCTime -> LByteString -> Eff es Int
ingestScrapedBody cfg now body = do
  appCtx :: AuthContext <- Eff.ask
  let (finite, nonFinite) = partition Prom.isFiniteSample (Prom.parsePrometheus (decodeUtf8 body))
      records = V.fromList (sampleToMetricRecord cfg now <$> finite)
      target = Telemetry.writeTargetFor appCtx.env.enablePostgresTelemetryWrites appCtx.env.enableTimefusionWrites Nothing
  unless (null nonFinite) $ Log.logInfo "Prometheus scrape dropped non-finite samples" (AE.object ["config_id" AE..= cfg.id.toText, "dropped" AE..= length nonFinite])
  Telemetry.bulkInsertOtelMetrics appCtx.metricCatalogBuffer appCtx.hasqlTimefusionUsesPgTypes target records >>= \case
    Left failure -> liftIO $ throwIO $ These.mergeThese const failure
    Right () -> pure (V.length records)


-- | Map one Prometheus sample to a metric row. Counters become monotonic sums;
-- everything else (gauges, histogram/summary component series) becomes a gauge.
-- The config name is the @service.name@ so scraped series group per target.
sampleToMetricRecord :: PrometheusScrapeConfig -> UTCTime -> Prom.Sample -> Telemetry.MetricRecord
sampleToMetricRecord cfg now s =
  Telemetry.MetricRecord
    { id = Nothing
    , projectId = unUUIDId cfg.projectId
    , metricName = s.name
    , metricType = mtype
    , metricUnit = ""
    , metricDescription = s.help
    , metricTime = maybe now (posixSecondsToUTCTime . (/ 1000) . fromIntegral) s.timestampMs
    , timestamp = now
    , startTimestamp = Nothing
    , attributes = attrs
    , resource = AE.object ["service.name" AE..= svc]
    , resourceSchemaUrl = ""
    , instrumentationScope = AE.object []
    , scopeSchemaUrl = ""
    , droppedAttributesCount = 0
    , metricValue = mval
    , metricMetadata = AE.object []
    , exemplars = AE.Null
    , flags = 0
    , aggregationTemporality = Telemetry.ATCumulative <$ guard isCounter
    , isMonotonic = True <$ guard isCounter
    , messageSizeBytes = 0
    }
  where
    svc = if T.null cfg.name then "prometheus" else cfg.name
    -- Only TYPE counter maps to MTSum. Histogram/summary _count/_sum sub-series are
    -- cumulative-monotonic too but stay gauges (their family TYPE isn't counter), so a
    -- downstream rate() keying off aggregationTemporality treats them as instantaneous.
    isCounter = s.sampleType == Prom.Counter
    gs = Telemetry.GaugeSum (Just s.value) Nothing
    (mtype, mval) = bool (Telemetry.MTGauge, Telemetry.GaugeValue gs) (Telemetry.MTSum, Telemetry.SumValue gs) isCounter
    labelMap = AEKM.fromList [(AEK.fromText k, AE.String v) | (k, v) <- s.labels]
    -- KeyMap (<>) is left-biased, so the scraped per-sample labels (specific) must come
    -- first to win over the config's static extraLabels (general context) on key collisions.
    attrs =
      AE.Object $ labelMap <> case cfg.extraLabels of
        AE.Object o -> o
        _ -> mempty


-- | wreq Options for a scrape: hard 10s response timeout (a dead/slow endpoint must never wedge
-- a worker), no redirect following, and an optional Authorization header. Shared by the background
-- worker and the settings Test button so the two never drift on timeout or header wiring.
prometheusScrapeOpts :: Maybe Text -> Wreq.Options
prometheusScrapeOpts authHeader =
  defaults
    & Wreq.manager
    .~ Left tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro 10_000_000}
      -- Don't follow redirects: http-client (unlike browsers) re-sends Authorization across a
      -- cross-host 3xx, so a target answering 302 -> attacker could exfiltrate the bearer token.
      & Wreq.redirects
    .~ 0
      & maybe Relude.id (\h -> header "Authorization" .~ [encodeUtf8 h]) authHeader
