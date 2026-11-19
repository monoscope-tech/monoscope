-- | Check history and status pages. Every uptime probe and cron outcome is one
-- @apis.check_results@ row stamped with the region it ran from; the day bars, dashboards
-- and status pages are aggregates over it.
module Models.Apis.Uptime (
  ResultKind (..),
  Region (..),
  regions,
  regionLabel,
  regionOf,
  servedRegions,
  downtimeAfter,
  ProbeFailure (..),
  probeFailure,
  probeReason,
  CheckResult (..),
  recordResult,
  DayBucket (..),
  DayHealth (..),
  dayHealth,
  dailyBuckets,
  uptimePercent,
  WindowUptime (..),
  windowUptime,
  RegionStats (..),
  regionStats,
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
import Data.Text qualified as T
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
-- @UPTIME_PROBE_REGION@ names. @country@ (ISO 3166-1 alpha-2) places its probes on the geomap.
data Region = Region {code :: Text, city :: Text, continent :: Text, country :: Text}


regions :: [Region]
regions =
  [ Region "us-east" "Virginia" "North America" "US"
  , Region "us-west" "Oregon" "North America" "US"
  , Region "sa-east" "São Paulo" "South America" "BR"
  , Region "eu-west" "France" "Europe" "FR"
  , Region "eu-central" "Frankfurt" "Europe" "DE"
  , Region "eu-north" "Helsinki" "Europe" "FI"
  , Region "af-south" "Johannesburg" "Africa" "ZA"
  , Region "ap-south" "Mumbai" "Asia" "IN"
  , Region "ap-southeast" "Singapore" "Asia" "SG"
  , Region "ap-northeast" "Tokyo" "Asia" "JP"
  , Region "au-east" "Sydney" "Oceania" "AU"
  ]


-- | >>> regionLabel "eu-central"
-- "Frankfurt, Europe"
-- >>> regionLabel "global"
-- "global"
regionLabel :: Text -> Text
regionLabel c = maybe c (\r -> r.city <> ", " <> r.continent) $ regionOf c


regionOf :: Text -> Maybe Region
regionOf c = find ((== c) . (.code)) regions


-- | Consecutive failed probes that make an uptime check down: one alone is a blip.
downtimeAfter :: Int
downtimeAfter = 2


-- | Why a probe's answer (status, body) counts as down; 'Nothing' is up.
--
-- >>> probeFailure 200 Nothing (Right (200, ""))
-- Nothing
-- >>> map (fmap probeReason . probeFailure 200 (Just "healthy")) [Right (200, "status: degraded"), Right (503, "healthy"), Left "timeout"]
-- [Just "response lacks \"healthy\"",Just "status 503, expected 200",Just "request failed: timeout"]
data ProbeFailure = RequestFailed Text | WrongStatus {got :: Int, expected :: Int} | MissingText Text
  deriving stock (Eq, Show)


probeFailure :: Int -> Maybe Text -> Either Text (Int, Text) -> Maybe ProbeFailure
probeFailure expected contains = \case
  Left err -> Just $ RequestFailed err
  Right (got, body)
    | got /= expected -> Just WrongStatus{got, expected}
    | otherwise -> MissingText <$> mfilter (not . (`T.isInfixOf` body)) contains


probeReason :: ProbeFailure -> Text
probeReason = \case
  RequestFailed err -> "request failed: " <> err
  WrongStatus{got, expected} -> "status " <> show got <> ", expected " <> show expected
  MissingText t -> "response lacks \"" <> t <> "\""


-- | Regions a check can run from: this node's, and any a worker probed from in the last day.
servedRegions :: DB es => Text -> UTCTime -> Eff es [Text]
servedRegions here now = (here :) <$> Hasql.interp [HI.sql| SELECT DISTINCT region FROM apis.check_results WHERE kind = 'uptime' AND checked_at > #{now} - interval '1 day' |]


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


-- | The last 90 days, the span every bar shows.
dailyBuckets :: DB es => [UUID.UUID] -> UTCTime -> Eff es [DayBucket]
dailyBuckets ids now =
  Hasql.interp
    [HI.sql| SELECT check_id, (checked_at AT TIME ZONE 'UTC')::date, count(*)::bigint, count(*) FILTER (WHERE NOT ok)::bigint, avg(duration_ms) FILTER (WHERE ok)::float8
             FROM apis.check_results WHERE check_id = ANY(#{ids}) AND checked_at >= #{now} - interval '90 days' GROUP BY 1, 2 |]


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


data RegionStats = RegionStats {checkId :: UUID.UUID, region :: Text, total :: Int, failed :: Int, avgMs :: Maybe Double, avgDnsMs :: Maybe Double, lastAt :: UTCTime, lastOk :: Bool}
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


regionStats :: DB es => [UUID.UUID] -> UTCTime -> Eff es [RegionStats]
regionStats ids since =
  Hasql.interp
    [HI.sql| SELECT check_id, region, count(*)::bigint, count(*) FILTER (WHERE NOT ok)::bigint, avg(duration_ms)::float8, avg(dns_ms)::float8,
                    max(checked_at), (array_agg(ok ORDER BY checked_at DESC))[1]
             FROM apis.check_results WHERE check_id = ANY(#{ids}) AND checked_at > #{since} GROUP BY check_id, region ORDER BY region |]


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
