-- | Check history and status pages. Every uptime probe and cron outcome is one
-- @apis.check_results@ row stamped with the region it ran from; the day bars, dashboards
-- and status pages are aggregates over it.
module Models.Apis.Uptime (
  ResultKind (..),
  Region (..),
  regions,
  regionLabel,
  servedRegions,
  downtimeAfter,
  CheckResult (..),
  recordResult,
  DayBucket (..),
  DayHealth (..),
  dayHealth,
  dailyBuckets,
  uptimePercent,
  WindowUptime (..),
  windowUptime,
  LatencyStats (..),
  latencyStats,
  RegionStats (..),
  regionStats,
  HourBucket (..),
  hourlyBuckets,
  Incident (..),
  incidents,
  recentResults,
  monthlyUptime,
  Visibility (..),
  StatusPage (..),
  StatusPageId,
  statusPagesByProject,
  statusPageById,
  statusPageBySlug,
  insertStatusPage,
  updateStatusPage,
  deleteStatusPage,
) where

import Data.Effectful.Hasql qualified as Hasql
import Data.List (lookup)
import Data.Time (Day, UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Models.Ids qualified as Projects
import Pkg.DeriveUtils (UUIDId, WrappedEnumSC (..))
import Relude
import System.Types (DB)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)


data ResultKind = RKUptime | RKCron
  deriving stock (Eq, Read, Show)
  deriving (HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC 'Nothing "RK" ResultKind


-- | A probe location, named as cloud providers name theirs. A node probes from the one its
-- @UPTIME_PROBE_REGION@ names; deploying a worker per region is all multi-region checks need.
data Region = Region {code :: Text, city :: Text, continent :: Text}


regions :: [Region]
regions =
  [ Region "us-east" "Virginia" "North America"
  , Region "us-west" "Oregon" "North America"
  , Region "sa-east" "São Paulo" "South America"
  , Region "eu-west" "France" "Europe"
  , Region "eu-central" "Frankfurt" "Europe"
  , Region "eu-north" "Helsinki" "Europe"
  , Region "af-south" "Johannesburg" "Africa"
  , Region "ap-south" "Mumbai" "Asia"
  , Region "ap-southeast" "Singapore" "Asia"
  , Region "ap-northeast" "Tokyo" "Asia"
  , Region "au-east" "Sydney" "Oceania"
  ]


-- | >>> regionLabel "eu-central"
-- "Frankfurt, Europe"
-- >>> regionLabel "global"
-- "global"
regionLabel :: Text -> Text
regionLabel c = maybe c (\r -> r.city <> ", " <> r.continent) $ lookup c [(r.code, r) | r <- regions]


-- | Consecutive failed probes that make an uptime check down: one alone is a blip.
downtimeAfter :: Int
downtimeAfter = 2


-- | Regions a worker probed uptime checks from in the last day: the ones a check can run from.
servedRegions :: DB es => UTCTime -> Eff es [Text]
servedRegions now = Hasql.interp [HI.sql| SELECT DISTINCT region FROM apis.check_results WHERE kind = 'uptime' AND checked_at > #{now} - interval '1 day' |]


data CheckResult = CheckResult
  { projectId :: Projects.ProjectId
  , checkId :: UUID.UUID
  , kind :: ResultKind
  , region :: Text
  , checkedAt :: UTCTime
  , ok :: Bool
  , statusCode :: Maybe Int
  , durationMs :: Maybe Int
  , dnsMs :: Maybe Int
  , message :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- | Idempotent per (check, region, instant), so re-evaluating the same cron window records it once.
recordResult :: DB es => CheckResult -> Eff es ()
recordResult CheckResult{..} =
  void
    $ Hasql.interpExecute
      [HI.sql| INSERT INTO apis.check_results (project_id, check_id, kind, region, checked_at, ok, status_code, duration_ms, dns_ms, message)
               VALUES (#{projectId}, #{checkId}, #{kind}, #{region}, #{checkedAt}, #{ok}, #{statusCode}, #{durationMs}, #{dnsMs}, #{message})
               ON CONFLICT (check_id, region, checked_at) DO NOTHING |]


data DayBucket = DayBucket {checkId :: UUID.UUID, day :: Day, total :: Int, failed :: Int, avgMs :: Maybe Double}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data DayHealth = NoData | Healthy | Degraded | Outage
  deriving stock (Eq, Ord, Show)


-- | A day with any failure is no longer healthy; one where a twentieth or more failed is an outage.
--
-- >>> map (uncurry dayHealth) [(0, 0), (100, 0), (100, 1), (100, 5)]
-- [NoData,Healthy,Degraded,Outage]
dayHealth :: Int -> Int -> DayHealth
dayHealth total failed
  | total == 0 = NoData
  | failed == 0 = Healthy
  | failed * 20 < total = Degraded
  | otherwise = Outage


dailyBuckets :: DB es => [UUID.UUID] -> UTCTime -> Eff es [DayBucket]
dailyBuckets ids since =
  Hasql.interp
    [HI.sql| SELECT check_id, (checked_at AT TIME ZONE 'UTC')::date, count(*)::bigint, count(*) FILTER (WHERE NOT ok)::bigint, avg(duration_ms) FILTER (WHERE ok)::float8
             FROM apis.check_results WHERE check_id = ANY(#{ids}) AND checked_at >= #{since} GROUP BY 1, 2 |]


-- | >>> uptimePercent 0 0
-- Nothing
-- >>> uptimePercent 2000 1
-- Just 99.95
uptimePercent :: Int -> Int -> Maybe Double
uptimePercent total failed = guard (total > 0) $> fromIntegral (total - failed) * 100 / fromIntegral total


-- | Probe counts over the last day, week, month and quarter.
data WindowUptime = WindowUptime {day :: (Int, Int), week :: (Int, Int), month :: (Int, Int), quarter :: (Int, Int)}


windowUptime :: DB es => UUID.UUID -> UTCTime -> Eff es WindowUptime
windowUptime cid now =
  maybe (WindowUptime z z z z) (\(a, b, c, d, e, f, g, h) -> WindowUptime (a, b) (c, d) (e, f) (g, h))
    <$> Hasql.interpOne
      [HI.sql| SELECT count(*) FILTER (WHERE checked_at > #{now} - interval '1 day')::bigint, count(*) FILTER (WHERE checked_at > #{now} - interval '1 day' AND NOT ok)::bigint,
                      count(*) FILTER (WHERE checked_at > #{now} - interval '7 days')::bigint, count(*) FILTER (WHERE checked_at > #{now} - interval '7 days' AND NOT ok)::bigint,
                      count(*) FILTER (WHERE checked_at > #{now} - interval '30 days')::bigint, count(*) FILTER (WHERE checked_at > #{now} - interval '30 days' AND NOT ok)::bigint,
                      count(*)::bigint, count(*) FILTER (WHERE NOT ok)::bigint
               FROM apis.check_results WHERE check_id = #{cid} AND checked_at > #{now} - interval '90 days' |]
  where
    z = (0, 0)


-- | Response times over a window: Apdex counts at T = 500 ms (satisfied under T, tolerating
-- under 4T, failures frustrated), and the counts in each band of 'latencyBands'.
data LatencyStats = LatencyStats
  { total :: Int
  , avgMs :: Maybe Double
  , p95Ms :: Maybe Double
  , satisfied :: Int
  , tolerating :: Int
  , bands :: [Int]
  }


latencyStats :: DB es => UUID.UUID -> UTCTime -> Eff es LatencyStats
latencyStats cid since =
  maybe (LatencyStats 0 Nothing Nothing 0 0 []) (\(t, a, p, s, tol, b1, b2, b3, b4, b5, b6, b7) -> LatencyStats t a p s tol [b1, b2, b3, b4, b5, b6, b7])
    <$> Hasql.interpOne
      [HI.sql| SELECT count(*)::bigint, avg(duration_ms)::float8, percentile_cont(0.95) WITHIN GROUP (ORDER BY duration_ms)::float8,
                      count(*) FILTER (WHERE ok AND duration_ms <= 500)::bigint, count(*) FILTER (WHERE ok AND duration_ms > 500 AND duration_ms <= 2000)::bigint,
                      count(*) FILTER (WHERE ok AND duration_ms < 250)::bigint, count(*) FILTER (WHERE ok AND duration_ms >= 250 AND duration_ms < 500)::bigint,
                      count(*) FILTER (WHERE ok AND duration_ms >= 500 AND duration_ms < 1000)::bigint, count(*) FILTER (WHERE ok AND duration_ms >= 1000 AND duration_ms < 2000)::bigint,
                      count(*) FILTER (WHERE ok AND duration_ms >= 2000 AND duration_ms < 4000)::bigint, count(*) FILTER (WHERE ok AND duration_ms >= 4000)::bigint,
                      count(*) FILTER (WHERE NOT ok)::bigint
               FROM apis.check_results WHERE check_id = #{cid} AND checked_at > #{since} |]


data RegionStats = RegionStats {checkId :: UUID.UUID, region :: Text, total :: Int, failed :: Int, avgMs :: Maybe Double, avgDnsMs :: Maybe Double, lastAt :: UTCTime, lastOk :: Bool}
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


regionStats :: DB es => [UUID.UUID] -> UTCTime -> Eff es [RegionStats]
regionStats ids since =
  Hasql.interp
    [HI.sql| SELECT check_id, region, count(*)::bigint, count(*) FILTER (WHERE NOT ok)::bigint, avg(duration_ms)::float8, avg(dns_ms)::float8,
                    max(checked_at), (array_agg(ok ORDER BY checked_at DESC))[1]
             FROM apis.check_results WHERE check_id = ANY(#{ids}) AND checked_at > #{since} GROUP BY check_id, region ORDER BY region |]


data HourBucket = HourBucket {at :: UTCTime, total :: Int, failed :: Int, avgMs :: Maybe Double}
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


hourlyBuckets :: DB es => UUID.UUID -> UTCTime -> Eff es [HourBucket]
hourlyBuckets cid since =
  Hasql.interp
    [HI.sql| SELECT time_bucket('1 hour', checked_at), count(*)::bigint, count(*) FILTER (WHERE NOT ok)::bigint, avg(duration_ms) FILTER (WHERE ok)::float8
             FROM apis.check_results WHERE check_id = #{cid} AND checked_at > #{since} GROUP BY 1 ORDER BY 1 |]


-- | A run of consecutive failures, and the first success after it (none while ongoing). The
-- last failure's status code says what went wrong without its message's client detail.
data Incident = Incident {checkId :: UUID.UUID, startedAt :: UTCTime, lastFailedAt :: UTCTime, failures :: Int, recoveredAt :: Maybe UTCTime, reason :: Maybe Text, statusCode :: Maybe Int}
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


-- | Every failure run of each check; callers keep the runs long enough to be incidents.
incidents :: DB es => [UUID.UUID] -> UTCTime -> Eff es [Incident]
incidents ids since =
  Hasql.interp
    [HI.sql| WITH r AS (SELECT check_id, checked_at, ok, message, status_code, count(*) FILTER (WHERE ok) OVER (PARTITION BY check_id ORDER BY checked_at) AS grp
                        FROM apis.check_results WHERE check_id = ANY(#{ids}) AND checked_at > #{since})
             SELECT check_id, min(checked_at), max(checked_at), count(*)::bigint,
                    (SELECT min(checked_at) FROM r ok_r WHERE ok_r.check_id = f.check_id AND ok_r.ok AND ok_r.grp = f.grp + 1),
                    (array_agg(message ORDER BY checked_at DESC))[1], (array_agg(status_code ORDER BY checked_at DESC))[1]::bigint
             FROM r f WHERE NOT ok GROUP BY check_id, grp ORDER BY 2 DESC |]


recentResults :: DB es => UUID.UUID -> Int -> Eff es [CheckResult]
recentResults cid limit =
  Hasql.interp
    [HI.sql| SELECT project_id, check_id, kind, region, checked_at, ok, status_code::bigint, duration_ms::bigint, dns_ms::bigint, message
             FROM apis.check_results WHERE check_id = #{cid} ORDER BY checked_at DESC LIMIT #{limit} |]


-- | (check, first day of the month, probes, failures) for the last six months.
monthlyUptime :: DB es => [UUID.UUID] -> UTCTime -> Eff es [(UUID.UUID, Day, Int, Int)]
monthlyUptime ids now =
  Hasql.interp
    [HI.sql| SELECT check_id, date_trunc('month', checked_at AT TIME ZONE 'UTC')::date, count(*)::bigint, count(*) FILTER (WHERE NOT ok)::bigint
             FROM apis.check_results WHERE check_id = ANY(#{ids}) AND checked_at > #{now} - interval '6 months' GROUP BY 1, 2 ORDER BY 2 DESC |]


data Visibility = VPublic | VInternal
  deriving stock (Bounded, Enum, Eq, Read, Show)
  deriving (FromHttpApiData, HI.DecodeValue, HI.EncodeValue, ToHttpApiData) via WrappedEnumSC 'Nothing "V" Visibility


type StatusPageId = UUIDId "status_page"


-- | A page of checks shown as components. A public page is served without login at
-- @/status/<slug>@; an internal one only inside the project.
data StatusPage = StatusPage
  { id :: StatusPageId
  , projectId :: Projects.ProjectId
  , createdAt :: UTCTime
  , slug :: Text
  , title :: Text
  , description :: Text
  , visibility :: Visibility
  , checkIds :: V.Vector UUID.UUID
  }
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


statusPageCols :: HI.Sql
statusPageCols = [HI.sql| SELECT id, project_id, created_at, slug, title, description, visibility, check_ids FROM apis.status_pages |]


statusPagesByProject :: DB es => Projects.ProjectId -> Eff es [StatusPage]
statusPagesByProject pid = Hasql.interp (statusPageCols <> [HI.sql| WHERE project_id = #{pid} ORDER BY created_at |])


statusPageById :: DB es => Projects.ProjectId -> StatusPageId -> Eff es (Maybe StatusPage)
statusPageById pid sid = Hasql.interpOne (statusPageCols <> [HI.sql| WHERE project_id = #{pid} AND id = #{sid} |])


statusPageBySlug :: DB es => Text -> Eff es (Maybe StatusPage)
statusPageBySlug slug = Hasql.interpOne (statusPageCols <> [HI.sql| WHERE slug = #{slug} |])


-- | 0 when the slug is taken: slugs are global, being public URLs.
insertStatusPage :: DB es => Projects.ProjectId -> Text -> Text -> Text -> Visibility -> [UUID.UUID] -> Eff es Int64
insertStatusPage pid slug title description visibility checks =
  Hasql.interpExecute
    [HI.sql| INSERT INTO apis.status_pages (project_id, slug, title, description, visibility, check_ids)
             VALUES (#{pid}, #{slug}, #{title}, #{description}, #{visibility}, #{checks}) ON CONFLICT (slug) DO NOTHING |]


-- | 0 when another page holds the slug.
updateStatusPage :: DB es => Projects.ProjectId -> StatusPageId -> Text -> Text -> Text -> Visibility -> [UUID.UUID] -> Eff es Int64
updateStatusPage pid sid slug title description visibility checks =
  Hasql.interpExecute
    [HI.sql| UPDATE apis.status_pages SET slug = #{slug}, title = #{title}, description = #{description}, visibility = #{visibility}, check_ids = #{checks}
             WHERE project_id = #{pid} AND id = #{sid} AND NOT EXISTS (SELECT 1 FROM apis.status_pages o WHERE o.slug = #{slug} AND o.id <> #{sid}) |]


deleteStatusPage :: DB es => Projects.ProjectId -> StatusPageId -> Eff es Int64
deleteStatusPage pid sid = Hasql.interpExecute [HI.sql| DELETE FROM apis.status_pages WHERE project_id = #{pid} AND id = #{sid} |]
