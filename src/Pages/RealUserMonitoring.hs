-- | Real User Monitoring projects browser OpenTelemetry and session recordings into one
-- investigation surface. The dashboard template is the customizable aggregate view; this page
-- owns the user/session workflow that a dashboard cannot express.
module Pages.RealUserMonitoring (
  rumGetH,
  RumGet (..),
  RumData (..),
  RumSummary (..),
  RumSession (..),
  RumLinks (..),
  SessionFilter (..),
  Vital (..),
  VitalRating (..),
  classifyVital,
  vitalKey,
  classifyUserAgent,
  argmaxPayload,
  pageLabel,
) where

import Data.Cache qualified as Cache
import Data.Default (def)
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Data.Fixed (mod')
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime)
import Data.UUID qualified as UUID
import Effectful (Eff, (:>))
import Effectful.Concurrent.Async (pooledForConcurrently)
import Effectful.Labeled (Labeled)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx (hxGet_, hxIndicator_, hxPushUrl_, hxSelect_, hxSwap_, hxTarget_)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.RUM (PageVitalPoint (..), ReplaySession (..), RumBreakdown (..), RumBucket (..), RumCacheKey (..), RumError (..), RumPage (..), RumQuery (..), RumQueryResult (..), RumSession (..), RumSummary (..), RumTrend (..), VitalSample (..), VitalTrendPoint (..))
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, navTabAttrs)
import Pages.Components (Deferred (..), EmptyStateAction (..), EmptyStateCfg (..), EmptyStateSize (..), withDeferredBody)
import Pages.Components qualified as Components
import Pages.Containers (showFFloat')
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.DeriveUtils (DB, decodeEnumSC, encodeEnumSC)
import Pkg.ErrorFingerprint (normalizeMessage)
import Relude
import System.Clock (TimeSpec (TimeSpec))
import System.Config (AuthContext (..), EnvConfig (enableTimefusionReads))
import System.Logging qualified as Log
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import UnliftIO (tryAny)
import Utils (faSprite_, fmtDate)


data RumTab = Overview | Sessions | Performance
  deriving stock (Bounded, Enum, Eq, Read, Show)


data SessionFilter = AllSessionRows | ErrorSessionRows | ReplaySessionRows
  deriving stock (Bounded, Enum, Eq)


renderBucket :: RumBucket -> Text
renderBucket = \case
  FiveMinutes -> "5 minutes"
  OneHour -> "1 hour"
  SixHours -> "6 hours"


parseTab :: Maybe Text -> RumTab
parseTab tabM = fromMaybe Overview $ decodeEnumSC @"" . toString =<< tabM


tabParam :: RumTab -> Text
tabParam = toText . encodeEnumSC @""


tabLabel :: RumTab -> Text
tabLabel = T.toTitle . tabParam


parseSessionFilter :: Maybe Text -> SessionFilter
parseSessionFilter paramM = fromMaybe AllSessionRows $ find ((== paramM) . sessionFilterParam) [minBound .. maxBound]


sessionFilterParam :: SessionFilter -> Maybe Text
sessionFilterParam = \case
  AllSessionRows -> Nothing
  ErrorSessionRows -> Just "errors"
  ReplaySessionRows -> Just "replays"


data VitalRating = Good | NeedsImprovement | Poor | Unknown
  deriving stock (Eq, Show)


data Vital = Vital
  { name :: Text
  , label :: Text
  , description :: Text
  , value :: Maybe Double
  , samples :: Int64
  , unit :: Text
  , goodAt :: Double
  , poorAt :: Double
  , rating :: VitalRating
  }
  deriving stock (Show)


-- | Web Vitals use Google's standard field thresholds. The boundary values are good/needs
-- improvement (not poor), which prevents a value exactly at 2.5s or 200ms being overstated.
--
-- >>> classifyVital 2500 4000 (Just 2500)
-- Good
-- >>> classifyVital 2500 4000 (Just 4000)
-- NeedsImprovement
-- >>> classifyVital 2500 4000 Nothing
-- Unknown
classifyVital :: Double -> Double -> Maybe Double -> VitalRating
classifyVital good poor = \case
  Nothing -> Unknown
  Just value | value <= good -> Good
  Just value | value <= poor -> NeedsImprovement
  Just _ -> Poor


-- | Unmeasured definitions; 'vitalsFromSamples' fills in value/samples/rating.
vitalDefinitions :: [Vital]
vitalDefinitions =
  [ mk "lcp" "Largest Contentful Paint" "When the main content becomes visible" "ms" 2500 4000
  , mk "inp" "Interaction to Next Paint" "How quickly interactions produce visual feedback" "ms" 200 500
  , mk "cls" "Cumulative Layout Shift" "Visual stability while the page loads" "" 0.1 0.25
  , mk "fcp" "First Contentful Paint" "When the first content appears" "ms" 1800 3000
  , mk "ttfb" "Time to First Byte" "Server and network response before rendering" "ms" 800 1800
  ]
  where
    mk name label description unit goodAt poorAt = Vital{name, label, description, unit, goodAt, poorAt, value = Nothing, samples = 0, rating = Unknown}


-- | The metric names each emitter uses for a web vital.
--
-- The OpenTelemetry browser SDK dots them (@browser.web_vital.lcp@); k6's browser module
-- underscores them behind its own prefix (@k6.browser_web_vital_lcp@). Only the first spelling
-- was matched, so a project whose vitals come from k6 — which is what the demo emits — showed
-- "No data, 0 samples" on both the Overview panel and the Performance table while the metrics
-- sat in the store. Listed exactly rather than matched with LIKE, so the IN still routes.
vitalMetricNames :: [Text]
vitalMetricNames = [prefix <> v.name | v <- vitalDefinitions, prefix <- ["browser.web_vital.", "k6.browser_web_vital_"]]


-- | The vital a metric name refers to: everything after the last separator, lowercased.
--
-- Derived rather than stripped per known prefix, so a third emitter's spelling maps itself.
--
-- >>> map vitalKey ["browser.web_vital.lcp", "k6.browser_web_vital_ttfb", "CLS"]
-- ["lcp","ttfb","cls"]
vitalKey :: Text -> Text
vitalKey = T.toLower . T.takeWhileEnd (\c -> c /= '.' && c /= '_')


vitalsFromSamples :: [VitalSample] -> [Vital]
vitalsFromSamples samples = map measure vitalDefinitions
  where
    byName = M.fromListWith (<>) [(vitalKey s.metricName, [(s.value, s.samples)]) | s <- samples]
    measure vital =
      let points = M.findWithDefault [] vital.name byName
          measured = weightedPercentile 0.75 points
       in vital{value = measured, samples = sum $ map snd points, rating = classifyVital vital.goodAt vital.poorAt measured}


weightedPercentile :: Double -> [(Double, Int64)] -> Maybe Double
weightedPercentile percentile points = do
  guard $ total > 0
  let wanted = max 1 $ ceiling (fromIntegral total * percentile)
  fst <$> find ((>= wanted) . snd) (scanl1 (\(_, n) (value, count) -> (value, n + count)) $ sortWith fst points)
  where
    total = sum $ map snd points


-- | What every RUM read is scoped to. Bundled rather than passed as five positional
-- arguments because a filter that reached one panel and not another would render a page
-- whose own numbers disagree — the summary counting a service the table below it excludes.
data RumScope = RumScope
  { useTf :: Bool
  , pid :: Projects.ProjectId
  , fromTime :: UTCTime
  , toTime :: UTCTime
  , environment :: Maybe Text
  , service :: Maybe Text
  }


-- | Project, window, environment and service: true of any table. An absent filter matches
-- everything, so clearing one is just dropping the query parameter.
scopePredicate :: RumScope -> HI.Sql
scopePredicate scope =
  let projectId = scope.pid.toText
      fromTime = scope.fromTime
      toTime = scope.toTime
      environment = scope.environment
      service = scope.service
   in [HI.sql|project_id = #{projectId}
        AND timestamp >= #{fromTime} AND timestamp <= #{toTime}
        AND (#{environment}::text IS NULL OR resource___deployment___environment___name = #{environment})
        AND (#{service}::text IS NULL OR resource___service___name = #{service})|]


-- | The span tables additionally have to be narrowed to what a browser sent; the metrics table
-- identifies its browser data by metric name instead.
--
-- Laddered, because no single marker covers real browser telemetry. @telemetry.sdk.language@
-- alone — which every RUM query used to filter on by itself — matches /nothing/ in production:
-- the OpenTelemetry browser SDKs leave it unset, so the page was scoped to the empty set while
-- three browser applications were reporting. The remaining rungs are what those three actually
-- carry: the browser resource detector's user agent, the standard browser instrumentation span
-- names, and our own SDK's page-view naming.
--
-- @resourceFetch@ is deliberately absent: it is one span per script, stylesheet and image, 40%
-- of all browser rows here, and it carries nothing RUM shows — its sessions and users are
-- already on the page-level spans beside it.
browserScope :: RumScope -> HI.Sql
browserScope scope =
  scopePredicate scope
    <> [HI.sql| AND (resource___telemetry___sdk___language IN ('webjs', 'javascript', 'js')
         OR resource___user_agent___original IS NOT NULL
         OR name IN ('documentLoad', 'documentFetch')
         OR |]
    <> pageViewPredicate
    <> [HI.sql|)|]


-- | The page a browser event was on. Our SDK sets @url.path@; the OpenTelemetry browser SDK
-- sets only @url.full@ and leaves @url.path@ null, which would otherwise collapse every row of
-- the Top Pages table onto a single blank path.
pagePath :: HI.Sql
pagePath = [HI.sql|COALESCE(NULLIF(attributes___url___path, ''), NULLIF(attributes___url___full, ''), replace(name, 'Pageview · ', ''))|]


-- | What counts as one page view. @documentLoad@ is the OpenTelemetry browser SDK's page-load
-- span; @Pageview ·@ is ours. Shared so the summary, the trend, the table and the per-session
-- counts cannot disagree about what they are counting.
pageViewPredicate :: HI.Sql
pageViewPredicate = [HI.sql|(name LIKE 'Pageview %' OR name = 'documentLoad')|]


-- | What counts as a browser error. Shared for the same reason as 'pageViewPredicate': the
-- summary, the trend, the error list and the per-session counts must agree.
errorPredicate :: HI.Sql
errorPredicate = [HI.sql|(status_code = 'ERROR' OR lower(COALESCE(level, '')) = 'error' OR attributes___exception___type IS NOT NULL)|]


-- | Services that actually emit browser telemetry in this window, busiest first. Deliberately
-- not the project's whole service list: offering a backend service in a RUM picker scopes the
-- page to something that can never have a page view, and the user cannot tell why it is empty.
rumServices :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> Eff es [Text]
rumServices scope =
  Hasql.withHasqlTimefusion scope.useTf
    $ map HI.getOneColumn
    <$> Hasql.interp
      ( [HI.sql|SELECT resource___service___name FROM otel_logs_and_spans WHERE |]
          -- Unscoped by service on purpose: a picker filtered by its own selection can only
          -- ever offer the value already chosen, so you could never switch away from it.
          <> browserScope scope{service = Nothing}
          <> [HI.sql| AND resource___service___name IS NOT NULL AND resource___service___name <> ''
               GROUP BY 1 ORDER BY COUNT(*) DESC LIMIT 100|]
      )


-- | Summary counts and the activity trend from one scan. These used to be two separate
-- 24-hour scans of the same rows — the summary alone cost ~7s cold — so they are rolled up
-- together: the grand-total row (NULL bucket) is the summary, with distinct counts computed
-- across the whole window rather than summed per bucket; the rest is the trend.
rumPulse :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> RumBucket -> Eff es (RumSummary, [RumTrend])
rumPulse scope bucket = do
  let bucketLit = fromString $ toString $ "'" <> renderBucket bucket <> "'"
      bucketExpr = [HI.sql|time_bucket(|] <> bucketLit <> [HI.sql|, timestamp)|]
  rows :: [(Maybe UTCTime, Int64, Int64, Int64, Int64)] <-
    Hasql.withHasqlTimefusion scope.useTf
      $ Hasql.interp
        ( [HI.sql|SELECT |]
            <> bucketExpr
            <> [HI.sql|,
            COUNT(DISTINCT NULLIF(attributes___session___id, ''))::bigint,
            COUNT(*) FILTER (WHERE |]
            <> pageViewPredicate
            <> [HI.sql|)::bigint,
            COUNT(DISTINCT NULLIF(COALESCE(attributes___user___id, attributes___user___email), ''))::bigint,
            COUNT(*) FILTER (WHERE |]
            <> errorPredicate
            <> [HI.sql|)::bigint
          FROM otel_logs_and_spans
          WHERE |]
            <> browserScope scope
            <> [HI.sql| GROUP BY ROLLUP(|]
            <> bucketExpr
            <> [HI.sql|) ORDER BY 1|]
        )
  let summary = fromMaybe (RumSummary 0 0 0 0) $ listToMaybe [RumSummary sessions views users errors | (Nothing, sessions, views, users, errors) <- rows]
      trend = [RumTrend bucketTime views errors | (Just bucketTime, _, views, _, errors) <- rows]
  pure (summary, trend)


rumPages :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> Eff es [RumPage]
rumPages scope =
  Hasql.withHasqlTimefusion scope.useTf
    $ Hasql.interp
      ( [HI.sql|
        SELECT
          |]
          <> pagePath
          <> [HI.sql|,
          COUNT(*)::bigint,
          (approx_percentile(0.75, percentile_agg(duration)) / 1000000.0)::float8,
          MAX(timestamp)
        FROM otel_logs_and_spans
        WHERE |]
          <> browserScope scope
          <> [HI.sql| AND |]
          <> pageViewPredicate
          <> [HI.sql| AND duration IS NOT NULL
        GROUP BY 1 ORDER BY COUNT(*) DESC LIMIT 20|]
      )


-- | Recent raw error rows; 'groupErrors' folds them into issues at render time. 400 recent
-- rows rather than 20: the panel shows groups, and a flat 20 of a hot error would hide every
-- other issue behind twenty copies of the loudest one.
rumErrors :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> Eff es [RumError]
rumErrors scope =
  Hasql.withHasqlTimefusion scope.useTf
    $ Hasql.interp
      ( [HI.sql|
        SELECT timestamp,
          COALESCE(attributes___exception___type, status_message, 'Browser error'),
          COALESCE(attributes___exception___message, status_message, name, 'No error message'),
          attributes___session___id,
          COALESCE(attributes___user___id, attributes___user___email),
          |]
          <> pagePath
          <> [HI.sql|
        FROM otel_logs_and_spans
        WHERE |]
          <> browserScope scope
          <> [HI.sql| AND |]
          <> errorPredicate
          <> [HI.sql| ORDER BY timestamp DESC LIMIT 400|]
      )


-- | The last page is the argmax of timestamp over page views, done portably: both stores
-- render a session's timestamps in one fixed-width lexicographically ordered text form, so
-- @MAX(concat(timestamp, '|', path))@ carries the newest page's path behind the first bar.
-- A plain @MAX(path)@ here picked the alphabetically largest URL over /all/ browser spans —
-- which on real traffic is a third-party font URL from a fetch span, not a page.
--
-- >>> argmaxPayload "2026-09-05 19:33:37.42+00|/checkout"
-- "/checkout"
argmaxPayload :: Text -> Text
argmaxPayload = T.drop 1 . T.dropWhile (/= '|')


otelSessions :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> Eff es [RumSession]
otelSessions scope =
  Hasql.withHasqlTimefusion scope.useTf
    $ map (\s -> s{lastPage = argmaxPayload <$> s.lastPage})
    <$> Hasql.interp
      ( [HI.sql|
        SELECT attributes___session___id,
          MIN(timestamp), MAX(timestamp), COUNT(*)::bigint,
          COUNT(*) FILTER (WHERE |]
          <> errorPredicate
          <> [HI.sql|)::bigint,
          COUNT(*) FILTER (WHERE |]
          <> pageViewPredicate
          <> [HI.sql|)::bigint,
          MAX(attributes___user___id), MAX(attributes___user___full_name), MAX(attributes___user___email),
          MAX(resource___service___name),
          MAX(concat(CAST(timestamp AS TEXT), '|', |]
          <> pagePath
          <> [HI.sql|)) FILTER (WHERE |]
          <> pageViewPredicate
          <> [HI.sql|),
          false
        FROM otel_logs_and_spans
        WHERE |]
          <> browserScope scope
          <> [HI.sql| AND attributes___session___id IS NOT NULL AND attributes___session___id <> ''
        GROUP BY attributes___session___id ORDER BY MAX(timestamp) DESC LIMIT 200|]
      )


-- | P75 of each vital per time bucket AND per page, from one scan. These are two views of
-- the same rows: a full-window read of otel_metrics is the expensive part (concurrent with
-- the span panels it ran 20–48s on the demo project), so both groupings share it via
-- GROUPING SETS. The bucket rows make a regression visible as a change over time; the page
-- rows find the page it happened on — a site-wide P75 hides exactly the page being hunted.
-- Our SDK stamps @page.url@ on every vital datapoint; k6's browser module stamps @url@.
rumVitalsDetail :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> RumBucket -> Eff es ([VitalTrendPoint], [PageVitalPoint])
rumVitalsDetail scope bucket = do
  let bucketExpr = [HI.sql|time_bucket(|] <> fromString (toString $ "'" <> renderBucket bucket <> "'") <> [HI.sql|, timestamp)|]
      -- Both nested ({"page":{"url":..}}) and flat ({"page.url":..}) spellings, because the
      -- two stores may not agree on how a dotted attribute key was flattened at ingest.
      pageExpr = [HI.sql|COALESCE(attributes->'page'->>'url', attributes->>'page.url', attributes->>'url')|]
  rows :: [(Maybe UTCTime, Maybe Text, Text, Double, Int64)] <-
    Hasql.withHasqlTimefusion scope.useTf
      $ Hasql.interp
        ( [HI.sql|SELECT |]
            <> bucketExpr
            <> [HI.sql|, |]
            <> pageExpr
            <> [HI.sql|, metric_name,
            approx_percentile(0.75, percentile_agg(COALESCE(value, distribution_sum / NULLIF(distribution_count, 0))))::float8,
            COUNT(*)::bigint
          FROM otel_metrics
          WHERE |]
            <> scopePredicate scope
            <> [HI.sql| AND metric_name IN |]
            <> fromString (toString $ "(" <> T.intercalate ", " ["'" <> n <> "'" | n <- vitalMetricNames] <> ")")
            <> [HI.sql| AND COALESCE(value, distribution_sum / NULLIF(distribution_count, 0)) IS NOT NULL
          GROUP BY GROUPING SETS ((|]
            <> bucketExpr
            <> [HI.sql|, metric_name), (|]
            <> pageExpr
            <> [HI.sql|, metric_name)) ORDER BY 1|]
        )
  let trend = [VitalTrendPoint bucketTime metric p75 | (Just bucketTime, _, metric, p75, _) <- rows]
      -- Bucket NULL and page NULL together is the page grouping's "vitals without a page
      -- attribute" bucket; a page column can't represent it, so it is dropped.
      byPage = [PageVitalPoint page metric p75 samples | (Nothing, Just page, metric, p75, samples) <- rows]
  pure (trend, take 150 $ sortWith (Down . (.samples)) byPage)


-- | Traffic per user agent string, busiest first. Classification into browser, OS and
-- device happens in 'classifyUserAgent': the store only groups, so a new browser release
-- needs no query change to show up.
rumBreakdown :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> Eff es [RumBreakdown]
rumBreakdown scope =
  Hasql.withHasqlTimefusion scope.useTf
    $ Hasql.interp
      ( [HI.sql|
        SELECT COALESCE(NULLIF(attributes___user_agent___original, ''), resource___user_agent___original),
          COUNT(DISTINCT NULLIF(attributes___session___id, ''))::bigint,
          COUNT(*) FILTER (WHERE |]
          <> pageViewPredicate
          <> [HI.sql|)::bigint,
          COUNT(*) FILTER (WHERE |]
          <> errorPredicate
          <> [HI.sql|)::bigint
        FROM otel_logs_and_spans
        WHERE |]
          <> browserScope scope
          <> [HI.sql| AND COALESCE(NULLIF(attributes___user_agent___original, ''), resource___user_agent___original) IS NOT NULL
        GROUP BY 1 ORDER BY 2 DESC LIMIT 100|]
      )


-- | Browser, operating system and device class of a user agent string. Deliberately a
-- coarse family classifier, not a full UA parser: RUM breakdowns answer "is this
-- Safari-only?" and "is mobile worse?", for which families are exactly enough.
--
-- Order matters everywhere: Edge and Opera embed "Chrome", Chrome embeds "Safari",
-- Android embeds "Linux", and iPads identify as tablets while iPhones are mobile.
--
-- >>> classifyUserAgent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/151.0.0.0 Safari/537.36 Edg/151.0"
-- ("Edge","Windows","Desktop")
-- >>> classifyUserAgent "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Mobile/15E148 Safari/604.1"
-- ("Safari","iOS","Mobile")
-- >>> classifyUserAgent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) HeadlessChrome/130.0.0.0 Safari/537.36"
-- ("Chrome","Linux","Desktop")
-- >>> classifyUserAgent "Mozilla/5.0 (Linux; Android 14; SM-S921B) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/151.0.0.0 Mobile Safari/537.36"
-- ("Chrome","Android","Mobile")
-- >>> classifyUserAgent "curl/8.6.0"
-- ("Other","Other","Desktop")
classifyUserAgent :: Text -> (Text, Text, Text)
classifyUserAgent ua = (browser, os, device)
  where
    has needle = needle `T.isInfixOf` ua
    browser
      | has "Edg/" || has "Edge/" = "Edge"
      | has "OPR/" || has "Opera" = "Opera"
      | has "SamsungBrowser" = "Samsung Internet"
      | has "Firefox/" = "Firefox"
      | has "Chrome/" || has "CriOS/" || has "HeadlessChrome" = "Chrome"
      | has "Safari/" = "Safari"
      | otherwise = "Other"
    os
      | has "Windows" = "Windows"
      | has "iPhone" || has "iPad" || has "iPod" = "iOS"
      | has "Mac OS X" || has "Macintosh" = "macOS"
      | has "Android" = "Android"
      | has "CrOS" = "ChromeOS"
      | has "Linux" || has "X11" = "Linux"
      | otherwise = "Other"
    device
      | has "iPad" || (has "Android" && not (has "Mobile")) = "Tablet"
      | has "Mobile" || has "iPhone" = "Mobile"
      | otherwise = "Desktop"


replaySessions :: DB es => RumScope -> Eff es [ReplaySession]
replaySessions scope =
  let projectId = scope.pid
      fromTime = scope.fromTime
      toTime = scope.toTime
   in Hasql.interp
        [HI.sql|
          SELECT session_id, created_at, last_event_at, user_id, user_name, user_email
          FROM projects.replay_sessions
          WHERE project_id = #{projectId} AND last_event_at >= #{fromTime} AND created_at <= #{toTime}
            AND (event_file_count > 0 OR cardinality(file_keys) > 0 OR cardinality(shard_keys) > 0)
          ORDER BY last_event_at DESC LIMIT 200
        |]


-- A recent 2,000-point sample keeps field-vital navigation bounded. Returning the previous
-- 10,000 raw rows made this one panel dominate the page while adding negligible p75 precision.
vitalSamples :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> Eff es [VitalSample]
vitalSamples scope =
  Hasql.withHasqlTimefusion scope.useTf
    $ Hasql.interp
      ( [HI.sql|
        SELECT metric_name,
          COALESCE(value, distribution_sum / NULLIF(distribution_count, 0))::float8,
          COALESCE(distribution_count, 1)::bigint
        FROM otel_metrics
        WHERE |]
          <> scopePredicate scope
          <> [HI.sql| AND metric_name IN |]
          <> fromString (toString $ "(" <> T.intercalate ", " ["'" <> n <> "'" | n <- vitalMetricNames] <> ")")
          <> [HI.sql|
          AND COALESCE(value, distribution_sum / NULLIF(distribution_count, 0)) IS NOT NULL
        ORDER BY timestamp DESC LIMIT 2000|]
      )


-- | Recordings are stored per session and carry no service attribution. Unscoped, a recording
-- with no matching span is still a real session and gets a row of its own. Under a service
-- scope it is only trustworthy where a span already places that session in the service — so it
-- attaches to sessions that survived the filter and adds none, which keeps the replay badge
-- working without smuggling another team's sessions back in.
mergeSessions :: Maybe Text -> [RumSession] -> [ReplaySession] -> [RumSession]
mergeSessions serviceScope otel replays = sortWith (Down . (.endedAt)) $ M.elems $ foldl' addReplay (M.fromList [(s.id, s) | s <- otel]) replays
  where
    addReplay sessions replay =
      let sid = UUID.toText replay.id
       in if isJust serviceScope
            then M.adjust (attachReplay replay) sid sessions
            else M.alter (Just . maybe (fromReplay replay) (attachReplay replay)) sid sessions
    fromReplay replay =
      RumSession
        { id = UUID.toText replay.id
        , startedAt = replay.startedAt
        , endedAt = replay.endedAt
        , events = 0
        , errors = 0
        , views = 0
        , userId = replay.userId
        , userName = replay.userName
        , userEmail = replay.userEmail
        , service = Nothing
        , lastPage = Nothing
        , hasReplay = True
        }
    attachReplay replay session =
      session
        { startedAt = min session.startedAt replay.startedAt
        , endedAt = max session.endedAt replay.endedAt
        , userId = session.userId <|> replay.userId
        , userName = session.userName <|> replay.userName
        , userEmail = session.userEmail <|> replay.userEmail
        , hasReplay = True
        }


-- | Everything a RUM self-link has to preserve for the page you land on to still be the page
-- you were looking at. Passed as one value so a new scope dimension cannot be added to the
-- handler and silently forgotten by half the links.
data RumLinks = RumLinks
  { pid :: Projects.ProjectId
  , window :: TimePicker.TimeWindow
  , service :: Maybe Text
  }


-- | One independently-loaded region of the page. Each panel is a separate scan of the
-- window, costing 0.5–4s on its own, and the panels together contend badly enough that six
-- concurrently take 10–28s. Loading them as one unit made the whole page wait for the
-- slowest; each panel now fetches itself, so a panel appears as soon as /its/ query lands.
data RumPanel = PanelServices | PanelPulse | PanelPages | PanelVitals | PanelVitalTrend | PanelErrors | PanelSessions | PanelAudience
  deriving stock (Eq, Read, Show)


panelParam :: RumPanel -> Text
panelParam = toText . encodeEnumSC @"Panel"


-- | Ids are the swap contract: 'deferredShell_' selects @#id@ out of the panel response, so
-- the shell and the rendered panel must agree on it.
panelId :: RumPanel -> Text
panelId panel = "rum-panel-" <> panelParam panel


parsePanel :: Text -> Maybe RumPanel
parsePanel = decodeEnumSC @"Panel" . toString


data RumData = RumData
  { links :: RumLinks
  , tab :: RumTab
  , panel :: Maybe RumPanel
  -- ^ Which panel this response carries. 'Nothing' is the page skeleton: every panel
  -- renders as a shell that fetches itself.
  , summary :: RumSummary
  , trend :: [RumTrend]
  , pages :: [RumPage]
  , errors :: [RumError]
  , sessions :: [RumSession]
  , vitals :: [Vital]
  , vitalTrend :: [VitalTrendPoint]
  , pageVitals :: [PageVitalPoint]
  , breakdown :: [RumBreakdown]
  , query :: Maybe Text
  , services :: [Text]
  , sessionFilter :: SessionFilter
  , selectedSession :: Maybe Text
  , degradedPanels :: [Text]
  }


newtype RumGet = RumGet (PageCtx (Deferred RumData))


instance ToHtml RumGet where
  toHtml (RumGet page) = toHtml page
  toHtmlRaw = toHtml


-- Empty panels are not cached: browser telemetry can arrive immediately after SDK setup, and
-- pinning an empty first visit would hide it until expiry.
cacheableRumResult :: RumQueryResult -> Bool
cacheableRumResult = \case
  PulseResult summary trend -> summary.sessions > 0 || summary.pageViews > 0 || summary.users > 0 || summary.errors > 0 || not (null trend)
  PagesResult rows -> not $ null rows
  ErrorsResult rows -> not $ null rows
  SessionsResult rows -> not $ null rows
  ReplaySessionsResult rows -> not $ null rows
  VitalSamplesResult rows -> not $ null rows
  VitalsDetailResult trend byPage -> not (null trend) || not (null byPage)
  ServicesResult names -> not $ null names
  BreakdownResult rows -> not $ null rows


-- | Every RUM panel is a separate scan of a 24-hour window, and a tab click re-runs all of
-- them. Holding the page chrome hostage to the slowest one is what makes switching tabs feel
-- broken, so the first request renders the tab strip, time picker and a skeleton, and the
-- panels arrive on the request the skeleton fires.
rumSkeleton_ :: RumTab -> Html ()
rumSkeleton_ Sessions =
  div_ [class_ "grid min-h-[calc(100vh-7.5rem)] grid-cols-5 bg-bgBase", role_ "status", Aria.label_ "Loading sessions"] do
    section_ [class_ "col-span-2 min-w-0 border-r border-strokeWeak max-xl:col-span-5"] $ Components.tableSkeleton_ 8
    section_ [class_ "col-span-3 min-w-0 bg-bgSunken max-xl:col-span-5"] mempty
rumSkeleton_ _ = div_ [class_ "min-h-full space-y-5 bg-bgSunken p-4", role_ "status", Aria.label_ "Loading real user monitoring"] do
  div_ [class_ "grid grid-cols-4 gap-px border-y border-strokeWeak bg-bgBase max-md:grid-cols-2"]
    $ replicateM_ 4
    $ div_ [class_ "flex flex-col gap-2 px-4 py-3"] do
      div_ [class_ "h-6 w-16 rounded skeleton-shimmer"] ""
      div_ [class_ "h-3 w-24 rounded skeleton-shimmer"] ""
  div_ [class_ "grid grid-cols-[minmax(0,1.65fr)_minmax(18rem,0.75fr)] gap-4 max-xl:grid-cols-1"] do
    div_ [class_ "rounded-lg border border-strokeWeak bg-bgBase p-4"] Components.chartSkeleton_
    div_ [class_ "rounded-lg border border-strokeWeak bg-bgBase"] $ Components.tableSkeleton_ 5
  div_ [class_ "rounded-lg border border-strokeWeak bg-bgBase"] $ Components.tableSkeleton_ 6


rumGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders RumGet)
rumGetH pid tabM queryM sessionFilterM fromM toM sinceM selectedM serviceM panelM deferredM = do
  (session, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  let tab = parseTab tabM
      panel = panelM >>= parsePanel
      sessionFilter = parseSessionFilter sessionFilterM
      since = sinceM <|> Just "24H"
      window = TimePicker.mkTimeWindow now fromM toM since
      environment = session.environment
      -- An empty select option clears the filter, and "" is not a service name.
      serviceFilter = find (not . T.null) serviceM
      scope = RumScope{useTf = appCtx.env.enableTimefusionReads, pid, fromTime = window.fromTime, toTime = window.toTime, environment, service = serviceFilter}
      links = RumLinks{pid, window, service = serviceFilter}
      bucket
        | diffUTCTime window.toTime window.fromTime <= 6 * 3600 = FiveMinutes
        | diffUTCTime window.toTime window.fromTime <= 3 * 86400 = OneHour
        | otherwise = SixHours
      cacheKey query = RumCacheKey pid query environment serviceFilter fromM toM since
      panelTtl = TimePicker.cacheTtl window
      pulseQ = ("experience" :: Text, cacheKey $ PulseQuery bucket, panelTtl, uncurry PulseResult <$> rumPulse scope bucket)
      pagesQ = ("pages" :: Text, cacheKey PagesQuery, panelTtl, PagesResult <$> rumPages scope)
      errorsQ = ("errors" :: Text, cacheKey ErrorsQuery, panelTtl, ErrorsResult <$> rumErrors scope)
      sessionsQ = ("sessions" :: Text, cacheKey SessionsQuery, panelTtl, SessionsResult <$> otelSessions scope)
      replaysQ = ("replays" :: Text, cacheKey ReplaySessionsQuery, panelTtl, ReplaySessionsResult <$> replaySessions scope)
      vitalsQ = ("web vitals" :: Text, cacheKey VitalSamplesQuery, panelTtl, VitalSamplesResult <$> vitalSamples scope)
      -- Held longer than the panel TTL: this is the heaviest scan on the page, and per-page
      -- P75s move on deploys, not by the minute.
      vitalsDetailQ = ("web vitals detail" :: Text, cacheKey $ VitalsDetailQuery bucket, max panelTtl (TimeSpec 900 0), uncurry VitalsDetailResult <$> rumVitalsDetail scope bucket)
      breakdownQ = ("audience" :: Text, cacheKey BreakdownQuery, panelTtl, BreakdownResult <$> rumBreakdown scope)
      -- The picker is on every tab, so its options are read on every tab.
      -- Keyed unscoped, matching the query: the option list is the same whichever service is
      -- selected, so scoping the key would cache one identical list per service.
      -- Held for 15 minutes rather than the panel TTL: which services emit browser telemetry
      -- changes on deploys, not by the minute, and this scan is one of the most expensive.
      servicesQ = ("services" :: Text, RumCacheKey pid ServicesQuery environment Nothing fromM toM since, max panelTtl (TimeSpec 900 0), ServicesResult <$> rumServices scope)
      -- Only the requested panel's queries run. The skeleton request runs none at all, so the
      -- page chrome is free and each panel pays only for itself.
      --
      -- These all used to run together on one request. Measured on the demo project over 24h
      -- (scripts/local/rum-perf-2026-08-30.md): panels cost 0.5–4.1s individually but six
      -- concurrently take 10–28s — contention eats most of the parallelism, and the page still
      -- waited for the slowest. Per-panel fetching trades that for a first paint at the cost
      -- of one panel.
      panelQueries = case panel of
        Just PanelServices -> [servicesQ]
        Just PanelPulse -> [pulseQ]
        Just PanelPages -> [pagesQ]
        Just PanelVitals -> [vitalsQ]
        Just PanelVitalTrend -> [vitalsDetailQ]
        Just PanelErrors -> [errorsQ]
        Just PanelSessions -> [sessionsQ, replaysQ]
        Just PanelAudience -> [breakdownQ]
        Nothing -> []
      runQuery (label, key, ttl, action) =
        tryAny
          ( liftIO (Cache.lookup appCtx.rumCache key)
              >>= maybe (action >>= \fresh -> fresh <$ when (cacheableRumResult fresh) (liftIO $ Cache.insert' appCtx.rumCache (Just ttl) key fresh)) pure
          )
          >>= either (\err -> Left label <$ Log.logAttention "RUM panel query failed" (label, displayException err)) (pure . Right)
      deferredUrl =
        TimePicker.windowUrl
          ("/p/" <> pid.toText <> "/rum")
          ([(key, value) | (key, Just value) <- [("tab", tabM), ("q", queryM), ("filter", sessionFilterM), ("session", selectedM), ("service", serviceFilter)]] <> [("deferred", "1")])
          window
  body <- withDeferredBody deferredM "rum-page" deferredUrl (rumSkeleton_ tab) do
    outcomes <- pooledForConcurrently panelQueries runQuery
    let (degradedPanels, results) = partitionEithers outcomes
        summary = fromMaybe (RumSummary 0 0 0 0) $ listToMaybe [value | PulseResult value _ <- results]
        trend = fold [value | PulseResult _ value <- results]
        pages = fold [value | PagesResult value <- results]
        errors = fold [value | ErrorsResult value <- results]
        sessions = mergeSessions serviceFilter (fold [value | SessionsResult value <- results]) (fold [value | ReplaySessionsResult value <- results])
        vitals = vitalsFromSamples $ fold [value | VitalSamplesResult value <- results]
        vitalTrend = fold [value | VitalsDetailResult value _ <- results]
        pageVitals = fold [value | VitalsDetailResult _ value <- results]
        services = fold [value | ServicesResult value <- results]
        breakdown = fold [value | BreakdownResult value <- results]
    pure RumData{links, tab, panel, summary, trend, pages, errors, sessions, vitals, vitalTrend, pageVitals, services, breakdown, query = queryM, sessionFilter, selectedSession = selectedM, degradedPanels}
  let conf =
        bw
          { pageTitle = "Real User Monitoring"
          , menuItem = Just "Real User Monitoring"
          , navTabs = Just $ rumNavTabs_ links tab
          , pageActions = Just $ rumActions_ links
          , docsLink = Just "https://monoscope.tech/docs/sdks/browser/"
          }
  addRespHeaders $ RumGet $ PageCtx conf body


rumNavTabs_ :: RumLinks -> RumTab -> Html ()
rumNavTabs_ links active = nav_ [class_ "tabs tabs-box tabs-outline items-center max-md:overflow-x-auto max-md:flex-nowrap", Aria.label_ "Real User Monitoring views", term "hx-preload" "mouseover"] do
  forM_ [minBound .. maxBound] \tab -> do
    let url = rumUrl links [("tab", tabParam tab)]
    a_
      ( [ href_ url
        , class_ $ "tab h-auto! whitespace-nowrap" <> bool "" " tab-active text-textStrong" (tab == active)
        , term "aria-current" $ bool "false" "page" (tab == active)
        ]
          <> navTabAttrs
      )
      $ toHtml
      $ tabLabel tab


rumActions_ :: RumLinks -> Html ()
rumActions_ links = div_ [class_ "inline-flex items-center gap-2", data_ "default-window" "24H"] do
  a_ [href_ $ "/p/" <> links.pid.toText <> "/rum/dashboard", class_ "btn btn-sm gap-1.5 max-md:hidden"] do
    faSprite_ "chart-line" "regular" "h-3.5 w-3.5"
    "Open RUM dashboard"
  TimePicker.timepicker_ Nothing links.window.currentRange Nothing
  TimePicker.refreshButton_


instance ToHtml RumData where
  toHtml = toHtmlRaw . rumPage_
  toHtmlRaw = toHtml


-- | A panel slot. On the skeleton response every slot is a shell that fetches only its own
-- panel; on a panel response the matching slot carries content and the rest stay shells —
-- @hx-select@ discards them, so rendering the surrounding layout costs nothing.
slot_ :: RumData -> RumPanel -> Html () -> Html () -> Html ()
slot_ page panel skeleton content
  | page.panel == Just panel = div_ [id_ $ panelId panel, class_ "w-full"] content
  | otherwise =
      Components.deferredShell_
        (panelId panel)
        -- Search, session filter and selected session ride along so a deep link still
        -- renders as the page it addressed; without them the sessions panel came back
        -- unfiltered with nothing selected.
        ( rumUrl page.links
            $ [("tab", tabParam page.tab), ("panel", panelParam panel), ("deferred", "1")]
            <> [(key, value) | (key, Just value) <- [("q", page.query), ("filter", sessionFilterParam page.sessionFilter), ("session", page.selectedSession)]]
        )
        -- Deliberately NOT serialised with @hx-sync ... queue@. The panels do contend in
        -- TimeFusion, but measured cold over 24h that is still the better trade: concurrent
        -- paints the first panel at 2.6s and finishes at 24.7s, queued paints the first at
        -- 5.0s and finishes at 28.7s (scripts/local/rum-perf-2026-08-30.md). Contention costs
        -- less than the queue wait it would replace.
        []
        skeleton


rumPage_ :: RumData -> Html ()
rumPage_ page = main_ [id_ "rum-page", class_ "min-h-full bg-bgSunken"] do
  unless (null page.degradedPanels) $ degradedBanner_ page.degradedPanels
  slot_ page PanelServices mempty $ servicePicker_ page
  case page.tab of
    Overview -> overview_ page
    Sessions -> sessions_ page
    Performance -> performance_ page


-- | Scopes every panel to one browser service. A project with several teams reports several
-- services into one project, and without this the page averages them together — a checkout
-- regression hidden by a healthy marketing site.
--
-- Rendered even when scoped to a service with no data, because that is exactly when the user
-- needs the control that got them there in order to get back out.
servicePicker_ :: RumData -> Html ()
servicePicker_ page
  | null page.services && isNothing page.links.service = mempty
  | otherwise = form_
      [ method_ "get"
      , action_ $ "/p/" <> page.links.pid.toText <> "/rum"
      , class_ "flex flex-wrap items-center gap-2 border-b border-strokeWeak bg-bgBase px-4 py-2 max-md:px-3"
      ]
      do
        input_ [type_ "hidden", name_ "tab", value_ $ tabParam page.tab]
        TimePicker.timeHiddenInputs_ page.links.window.fromQuery page.links.window.toQuery page.links.window.sinceQuery
        label_ [Lucid.for_ "rum-service", class_ "text-xs text-textWeak"] "Service"
        select_
          [ id_ "rum-service"
          , name_ "service"
          , class_ "select select-sm min-w-52 cursor-pointer border-strokeWeak bg-bgBase text-sm text-textStrong max-sm:h-11"
          , onchange_ "this.form.requestSubmit()"
          ]
          do
            option_ ([value_ ""] <> [selected_ "" | isNothing page.links.service]) "All services"
            -- A service selected before its telemetry aged out of the window is still offered,
            -- so the select cannot silently jump the user back to "All services".
            forM_ (ordNub $ page.services <> maybeToList page.links.service) \name ->
              option_ ([value_ name] <> [selected_ "" | page.links.service == Just name]) $ toHtml name
        whenJust page.links.service \name ->
          span_ [class_ "text-xs text-textWeak"] $ toHtml $ "Every panel below is scoped to " <> name <> "."


-- | A service filter that matched nothing is not an uninstrumented project. The unscoped
-- empty state pitches installing the browser SDK, which here would tell a user with working
-- telemetry to re-instrument a working app. The way out is to widen the scope, so that is
-- what this offers.
scopedEmptyState_ :: RumLinks -> Text -> Html ()
scopedEmptyState_ links name =
  div_ [class_ "mx-auto flex min-h-[40vh] max-w-2xl flex-col justify-center px-6 py-12"]
    $ Components.emptyState_
      def{icon = Just "web", action = ESLink (rumUrl links{service = Nothing} []) "Show all services"}
      ("No browser telemetry for " <> name <> " in this range")
      "This service reported no page views, errors or Web Vitals in the selected window. Widen the time range, or choose another service."


rumEmptyState_ :: Projects.ProjectId -> Html ()
rumEmptyState_ pid = div_ [class_ "mx-auto flex min-h-[60vh] max-w-2xl flex-col justify-center px-6 py-12"] do
  Components.emptyState_
    def
      { icon = Just "web"
      , action =
          ESCustom $ div_ [class_ "flex flex-wrap justify-center gap-2"] do
            a_ [href_ "https://monoscope.tech/docs/sdks/browser/", target_ "_blank", rel_ "noopener noreferrer", class_ "btn btn-sm btn-primary"] "Install the browser SDK"
            a_ [href_ $ "/p/" <> pid.toText <> "/rum/dashboard", class_ "btn btn-sm"] "Open RUM dashboard"
      }
    "No browser telemetry yet"
    "Install the browser SDK to send page loads, interactions, network spans, errors, and Core Web Vitals through OpenTelemetry. Enable session replay to connect those signals to the user's exact experience."
  Components.factGrid_
    "grid-cols-3 bg-bgBase max-sm:grid-cols-1 max-sm:divide-x-0 max-sm:divide-y"
    [ ("OpenTelemetry", "Portable traces and metrics")
    , ("Web Vitals", "LCP, INP, CLS, FCP, TTFB")
    , ("Session Replay", "DOM, console, and network context")
    ]


overview_ :: RumData -> Html ()
overview_ page = div_ [class_ "space-y-5 p-4 max-md:p-3"] do
  slot_ page PanelPulse pulseSkeleton_ $ pulseOrEmpty_ page
  div_ [class_ "grid grid-cols-[minmax(0,1.65fr)_minmax(18rem,0.75fr)] gap-4 max-xl:grid-cols-1"] do
    div_ [class_ "min-w-0 space-y-4"] do
      slot_ page PanelPages (panelSkeleton_ $ Components.tableSkeleton_ 5) $ topPages_ page.links page.pages
      slot_ page PanelAudience (panelSkeleton_ $ Components.tableSkeleton_ 3) $ audiencePanel_ page.breakdown
    aside_ [class_ "min-w-0 space-y-4"] do
      slot_ page PanelVitals (panelSkeleton_ $ Components.tableSkeleton_ 4) $ vitalsPanel_ page.vitals
      slot_ page PanelErrors (panelSkeleton_ $ Components.tableSkeleton_ 4) $ recentErrors_ page.links page.errors
  slot_ page PanelSessions (panelSkeleton_ $ Components.tableSkeleton_ 6) $ recentSessions_ page


-- | The onboarding pitch belongs to the summary: it is the panel that knows whether the
-- project has any browser telemetry at all. Panels with no rows of their own still render
-- their individual empty states. Summary and activity render together because they are one
-- scan — see 'rumPulse'.
pulseOrEmpty_ :: RumData -> Html ()
pulseOrEmpty_ page
  | page.summary.sessions > 0 || page.summary.pageViews > 0 = div_ [class_ "space-y-5"] do
      pulse_ page
      trendPanel_ page.trend
  | otherwise = maybe (rumEmptyState_ page.links.pid) (scopedEmptyState_ page.links) page.links.service


pulseSkeleton_ :: Html ()
pulseSkeleton_ = div_ [class_ "space-y-5", role_ "status", Aria.label_ "Loading summary"] do
  div_ [class_ "grid grid-cols-4 gap-px border-y border-strokeWeak bg-bgBase max-md:grid-cols-2"]
    $ replicateM_ 4
    $ div_ [class_ "flex flex-col gap-2 px-4 py-3"] do
      div_ [class_ "h-6 w-16 rounded skeleton-shimmer"] ""
      div_ [class_ "h-3 w-24 rounded skeleton-shimmer"] ""
  panelSkeleton_ Components.chartSkeleton_


panelSkeleton_ :: Html () -> Html ()
panelSkeleton_ = div_ [class_ "rounded-lg border border-strokeWeak bg-bgBase"]


pulse_ :: RumData -> Html ()
pulse_ page = section_ [Aria.label_ "Experience pulse", class_ "grid grid-cols-4 divide-x divide-strokeWeak border-y border-strokeWeak bg-bgBase max-md:grid-cols-2 max-md:divide-x-0"] do
  pulseCell "users" (show page.summary.sessions) "sessions" "text-iconNeutral"
  pulseCell "file-lines" (show page.summary.pageViews) "page views" "text-iconNeutral"
  pulseCell "user" (show page.summary.users) "identified users" "text-iconNeutral"
  pulseCell "triangle-exclamation" (show page.summary.errors) (bool "browser errors" "browser error" (page.summary.errors == 1)) (bool "text-textWeak" "text-textError" (page.summary.errors > 0))
  where
    pulseCell icon value label colour = div_ [class_ "flex min-w-0 items-center gap-3 px-4 py-3 max-md:border-b max-md:border-strokeWeak"] do
      faSprite_ icon "regular" $ "h-4 w-4 shrink-0 " <> colour
      div_ [class_ "min-w-0"] do
        strong_ [class_ "block text-lg font-semibold tabular-nums text-textStrong"] $ toHtml value
        span_ [class_ "block truncate text-xs text-textWeak"] $ toHtml label


trendPanel_ :: [RumTrend] -> Html ()
trendPanel_ points = section_ [class_ "rounded-lg border border-strokeWeak bg-bgBase"] do
  panelHeader_ "Experience activity" "Page views with browser errors marked in red" Nothing
  if null points
    then panelEmpty_ "No activity in this time range"
    else figure_ [class_ "p-4", Aria.label_ "Page views and browser errors over time"] do
      div_ [class_ "flex h-44 items-end gap-1 border-b border-strokeWeak"] $ forM_ points \point -> do
        let pageHeight = max 4 $ round (fromIntegral point.pageViews / fromIntegral maxViews * 100 :: Double)
            errorHeight = min pageHeight $ round (fromIntegral point.errors / fromIntegral maxViews * 100 :: Double)
            label = fmtDate "%d %b %H:%M" point.bucket <> ": " <> show point.pageViews <> " page views, " <> show point.errors <> " errors"
        div_ [class_ "group relative flex h-full min-w-1 flex-1 items-end", data_ "tippy-content" label] do
          div_ [class_ "relative w-full rounded-t-sm bg-fillInformation-strong/35", style_ $ "height:" <> show pageHeight <> "%"] do
            when (point.errors > 0) $ span_ [class_ "absolute inset-x-0 bottom-0 rounded-t-sm bg-fillError-strong", style_ $ "height:" <> show (max 4 errorHeight) <> "%", Aria.hidden_ "true"] ""
      figcaption_ [class_ "mt-2 flex items-center justify-between text-xs text-textWeak"] do
        span_ $ toHtml $ maybe "" (fmtDate "%d %b %H:%M" . (.bucket)) (listToMaybe points)
        span_ [class_ "inline-flex items-center gap-3"] do
          legendDot "bg-fillInformation-strong/60" "Page views"
          legendDot "bg-fillError-strong" "Errors"
        span_ $ toHtml $ maybe "" (fmtDate "%d %b %H:%M" . (.bucket)) (viaNonEmpty last points)
  where
    maxViews = foldl' max 1 $ map (.pageViews) points
    legendDot colour label = span_ [class_ "inline-flex items-center gap-1.5"] $ span_ [class_ $ "h-2 w-2 rounded-sm " <> colour, Aria.hidden_ "true"] "" >> toHtml label


topPages_ :: RumLinks -> [RumPage] -> Html ()
topPages_ links pages = section_ [class_ "overflow-hidden rounded-lg border border-strokeWeak bg-bgBase"] do
  panelHeader_ "Top pages" "Traffic and real-user load latency" $ Just ("Explore all events", logsUrl links browserPageViewKql)
  if null pages
    then panelEmpty_ "No page views in this time range"
    else div_ [class_ "overflow-x-auto"] $ table_ [class_ "table table-sm w-full"] do
      thead_ $ tr_ $ th_ "Page" >> th_ [class_ "text-right"] "Views" >> th_ [class_ "text-right"] "P75 load" >> th_ [class_ "text-right max-sm:hidden"] "Last seen"
      tbody_ $ forM_ pages \page -> tr_ do
        td_ [class_ "max-w-xl"] $ a_ [href_ $ logsUrl links (browserPageViewKql <> " and " <> pagePathKql page.path), class_ "block truncate font-medium text-textBrand"] $ toHtml page.path
        td_ [class_ "text-right tabular-nums"] $ toHtml $ show page.views
        td_ [class_ "text-right tabular-nums"] $ toHtml $ maybe "—" formatMilliseconds page.p75LoadMs
        td_ [class_ "text-right text-xs text-textWeak max-sm:hidden"] $ toHtml $ fmtDate "%d %b %H:%M" page.lastSeen


vitalsPanel_ :: [Vital] -> Html ()
vitalsPanel_ vitals = section_ [class_ "rounded-lg border border-strokeWeak bg-bgBase"] do
  panelHeader_ "Core Web Vitals" "P75 of the most recent samples against Google's thresholds" Nothing
  div_ [class_ "divide-y divide-strokeWeak"] $ forM_ vitals vitalRow_


vitalRow_ :: Vital -> Html ()
vitalRow_ vital = div_ [class_ "px-3 py-3"] do
  div_ [class_ "flex items-start justify-between gap-3"] do
    div_ [class_ "min-w-0"] do
      h3_ [class_ "truncate text-sm font-medium text-textStrong"] $ toHtml vital.label
      p_ [class_ "mt-0.5 text-xs text-textWeak"] $ toHtml vital.description
    div_ [class_ "shrink-0 text-right"] do
      strong_ [class_ $ "block text-sm font-semibold tabular-nums " <> (ratingStyle vital.rating).textClass] $ toHtml $ formatVital vital
      span_ [class_ "text-xs text-textWeak"] $ toHtml $ show vital.samples <> " samples"
  div_ [class_ "mt-2 grid grid-cols-3 gap-1", Aria.label_ $ vital.label <> " rating: " <> (ratingStyle vital.rating).label] do
    forM_ [Good, NeedsImprovement, Poor] (`ratingBand` vital.rating)


ratingBand :: VitalRating -> VitalRating -> Html ()
ratingBand band active = span_ [class_ $ "h-1.5 rounded-full " <> if band == active then (ratingStyle band).fillClass else "bg-fillWeak", Aria.hidden_ "true"] ""


-- | One issue: every raw error whose type and normalized message agree. Grouped at render
-- time so the cache keeps raw rows; the representative message and page are the newest.
data RumErrorGroup = RumErrorGroup
  { errorType :: Text
  , message :: Text
  , count :: Int
  , sessions :: Int
  , lastSeen :: UTCTime
  , path :: Maybe Text
  , sessionId :: Maybe Text
  }


-- | Fold raw error rows (newest first) into issues, loudest first. Messages are grouped
-- normalized — UUIDs, numbers and other identifiers masked — so "id=123" and "id=456"
-- are one issue, exactly the collapse Sentry and Datadog error tracking perform.
groupErrors :: [RumError] -> [RumErrorGroup]
groupErrors errors =
  sortWith (\g -> (Down g.count, Down g.lastSeen))
    $ map summarise
    $ M.elems
    $ M.fromListWith (flip (<>)) [((e.errorType, normalizeMessage e.message), pure @NonEmpty e) | e <- errors]
  where
    summarise issue@(latest :| _) =
      RumErrorGroup
        { errorType = latest.errorType
        , message = latest.message
        , count = length issue
        , sessions = length $ ordNub $ mapMaybe (.sessionId) $ toList issue
        , lastSeen = latest.timestamp
        , path = asum $ map (.path) $ toList issue
        , sessionId = asum $ map (.sessionId) $ toList issue
        }


recentErrors_ :: RumLinks -> [RumError] -> Html ()
recentErrors_ links errors = section_ [class_ "rounded-lg border border-strokeWeak bg-bgBase"] do
  panelHeader_ "Browser errors" "Grouped by signature; counts and sessions are within this range" $ Just ("View errors", logsUrl links (browserKql <> " and status_code == \"ERROR\""))
  if null errors
    then div_ [class_ "flex items-center gap-2 px-3 py-5 text-sm text-textWeak"] $ faSprite_ "circle-check" "regular" "h-4 w-4 text-textSuccess" >> "No browser errors in this range"
    else ul_ [class_ "divide-y divide-strokeWeak"] $ forM_ (take 6 $ groupErrors errors) \issue -> li_ [class_ "px-3 py-2.5"] do
      div_ [class_ "flex items-start gap-2"] do
        faSprite_ "triangle-exclamation" "solid" "mt-0.5 h-3.5 w-3.5 shrink-0 text-iconError"
        div_ [class_ "min-w-0 flex-1"] do
          div_ [class_ "flex items-center justify-between gap-2"] do
            div_ [class_ "flex min-w-0 items-center gap-1.5"] do
              strong_ [class_ "truncate text-sm font-medium text-textStrong"] $ toHtml issue.errorType
              when (issue.count > 1) $ span_ [class_ "badge badge-sm badge-error badge-outline shrink-0 tabular-nums"] $ toHtml $ "×" <> show issue.count
            time_ [datetime_ $ show issue.lastSeen, class_ "shrink-0 text-xs text-textWeak"] $ toHtml $ fmtDate "%H:%M" issue.lastSeen
          p_ [class_ "mt-0.5 line-clamp-2 text-xs text-textWeak"] $ toHtml issue.message
          div_ [class_ "mt-1 flex flex-wrap items-center gap-x-2 text-xs text-textWeak"] do
            forM_ issue.path $ span_ [class_ "max-w-56 truncate font-mono"] . toHtml
            when (issue.sessions > 0) $ span_ [class_ "tabular-nums"] $ toHtml $ show issue.sessions <> bool " sessions" " session" (issue.sessions == 1)
            forM_ issue.sessionId \sid -> a_ [href_ $ sessionLogsUrl links sid, class_ "font-medium text-textBrand hover:underline"] "Latest occurrence"


data AudienceRow = AudienceRow
  { name :: Text
  , sessions :: Int64
  , views :: Int64
  , errors :: Int64
  }


-- | Collapse per-user-agent traffic onto one classified dimension. A session using two
-- user agent strings would count once per string; families make that vanishingly rare.
audienceBy :: ((Text, Text, Text) -> Text) -> [RumBreakdown] -> [AudienceRow]
audienceBy pick rows =
  sortWith (Down . (.sessions))
    $ map (\(name, (sessions, views, errors)) -> AudienceRow{name, sessions, views, errors})
    $ M.toList
    $ M.fromListWith
      (\(a, b, c) (x, y, z) -> (a + x, b + y, c + z))
      [(pick $ classifyUserAgent row.userAgent, (row.sessions, row.views, row.errors)) | row <- rows]


-- | Who the traffic is: the first question of "is this bug Safari-only?" and "is mobile
-- slower?", which an aggregate summary cannot answer. Every RUM product leads with this.
audiencePanel_ :: [RumBreakdown] -> Html ()
audiencePanel_ breakdown = section_ [class_ "rounded-lg border border-strokeWeak bg-bgBase"] do
  panelHeader_ "Audience" "Sessions by browser, operating system, and device class — errors highlight where failures concentrate" Nothing
  if null breakdown
    then panelEmpty_ "No user agent data in this time range"
    else div_ [class_ "grid grid-cols-3 divide-x divide-strokeWeak max-md:grid-cols-1 max-md:divide-x-0 max-md:divide-y"] do
      audienceColumn_ "Browser" $ audienceBy (\(b, _, _) -> b) breakdown
      audienceColumn_ "Operating system" $ audienceBy (\(_, os, _) -> os) breakdown
      audienceColumn_ "Device" $ audienceBy (\(_, _, d) -> d) breakdown


audienceColumn_ :: Text -> [AudienceRow] -> Html ()
audienceColumn_ title rows = div_ [class_ "min-w-0 px-3 py-2.5"] do
  h3_ [class_ "text-xs font-medium uppercase tracking-wide text-textWeak"] $ toHtml title
  ul_ [class_ "mt-2 space-y-2"] $ forM_ (take 5 rows) \row -> li_ [class_ "min-w-0"] do
    div_ [class_ "flex items-baseline justify-between gap-2 text-sm"] do
      span_ [class_ "truncate font-medium text-textStrong"] $ toHtml row.name
      span_ [class_ "shrink-0 text-xs tabular-nums text-textWeak"] do
        when (row.errors > 0) do
          span_ [class_ "font-medium text-textError"] $ toHtml $ show row.errors <> bool " errors" " error" (row.errors == 1)
          " · "
        toHtml $ show row.sessions <> bool " sessions" " session" (row.sessions == 1)
    div_ [class_ "mt-1 h-1.5 w-full overflow-hidden rounded-full bg-fillWeak", Aria.hidden_ "true"]
      $ div_ [class_ "h-full rounded-full bg-fillInformation-strong/60", style_ $ "width:" <> show (share row) <> "%"] ""
  where
    maxSessions = foldl' max 1 $ map (.sessions) rows
    share row = max 2 $ round @Double @Int $ fromIntegral row.sessions / fromIntegral maxSessions * 100


recentSessions_ :: RumData -> Html ()
recentSessions_ page = section_ [class_ "overflow-hidden rounded-lg border border-strokeWeak bg-bgBase"] do
  panelHeader_ "Recent sessions" "Open a recording or inspect its correlated telemetry" $ Just ("View all sessions", sessionsUrl page.links Nothing AllSessionRows Nothing)
  sessionTable_ False page.links (take 8 page.sessions) Nothing AllSessionRows


sessions_ :: RumData -> Html ()
sessions_ page = slot_ page PanelSessions (Components.tableSkeleton_ 8) do
  let filtered = filterSessions page.query page.sessionFilter page.sessions
      selected = page.selectedSession >>= \sid -> find ((== sid) . (.id)) page.sessions
  div_ [class_ "grid min-h-[calc(100vh-7.5rem)] grid-cols-5 bg-bgBase"] do
    section_ [class_ "col-span-2 min-w-0 border-r border-strokeWeak max-xl:col-span-5 max-xl:border-b max-xl:border-r-0"] do
      sessionsToolbar_ page.links page.query page.sessionFilter (length filtered)
      sessionTable_ True page.links filtered page.query page.sessionFilter
    section_ [id_ "rum-replay-workspace", class_ "col-span-3 min-w-0 bg-bgSunken max-xl:col-span-5", Aria.label_ "Session replay workspace"] $ replayWorkspace_ page.links selected


sessionsToolbar_ :: RumLinks -> Maybe Text -> SessionFilter -> Int -> Html ()
sessionsToolbar_ links query sessionFilter count = div_ [class_ "space-y-2 border-b border-strokeWeak p-3"] do
  form_ [method_ "get", action_ $ "/p/" <> links.pid.toText <> "/rum", class_ "flex items-center gap-2"] do
    input_ [type_ "hidden", name_ "tab", value_ "sessions"]
    whenJust links.service \value -> input_ [type_ "hidden", name_ "service", value_ value]
    TimePicker.timeHiddenInputs_ links.window.fromQuery links.window.toQuery links.window.sinceQuery
    label_ [Lucid.for_ "rum-session-search", class_ "sr-only"] "Search sessions"
    div_ [class_ "relative min-w-0 flex-1"] do
      faSprite_ "magnifying-glass" "regular" "pointer-events-none absolute left-2.5 top-2.5 h-3.5 w-3.5 text-iconNeutral"
      input_ [id_ "rum-session-search", name_ "q", value_ $ fromMaybe "" query, placeholder_ "User, session, page, or service", class_ "input input-sm w-full pl-8"]
    button_ [type_ "submit", class_ "btn btn-sm"] "Search"
  div_ [class_ "flex items-center justify-between gap-2"] do
    div_ [class_ "tabs tabs-box tabs-outline tabs-sm", role_ "tablist", Aria.label_ "Session filters"] do
      filterLink AllSessionRows "All sessions"
      filterLink ErrorSessionRows "With errors"
      filterLink ReplaySessionRows "With replay"
    span_ [class_ "text-xs tabular-nums text-textWeak", role_ "status", Aria.live_ "polite"] $ toHtml $ show count <> " sessions"
  where
    filterLink value label =
      a_
        ( [ href_ $ sessionsUrl links query value Nothing
          , role_ "tab"
          , class_ $ "tab h-auto!" <> bool "" " tab-active text-textStrong" (sessionFilter == value)
          , Aria.selected_ $ bool "false" "true" (sessionFilter == value)
          ]
            <> navTabAttrs
        )
        $ toHtml label


filterSessions :: Maybe Text -> SessionFilter -> [RumSession] -> [RumSession]
filterSessions query sessionFilter = filter (\s -> matchesQuery s && matchesFilter s)
  where
    needle = T.toCaseFold $ fromMaybe "" query
    matchesQuery session = T.null needle || any (T.isInfixOf needle . T.toCaseFold) (session.id : catMaybes [session.userId, session.userName, session.userEmail, session.service, session.lastPage])
    matchesFilter session = case sessionFilter of
      AllSessionRows -> True
      ErrorSessionRows -> session.errors > 0
      ReplaySessionRows -> session.hasReplay


-- | @workspace@ marks the Sessions tab, where the replay panel sits beside the table: there a
-- row swaps only that panel instead of re-rendering the page around it. The Overview's recent
-- list has no panel to swap, so its rows navigate.
sessionTable_ :: Bool -> RumLinks -> [RumSession] -> Maybe Text -> SessionFilter -> Html ()
sessionTable_ workspace links sessions query sessionFilter =
  if null sessions
    then panelEmpty_ $ bool "No sessions in this time range" "No sessions match this filter" (sessionFilter /= AllSessionRows)
    else div_ [class_ "overflow-x-auto"] $ table_ [class_ "table table-sm w-full"] do
      thead_ $ tr_ $ th_ "User / session" >> th_ "Last page" >> th_ [class_ "text-right"] "Signals" >> th_ [class_ "text-right"] "Duration" >> th_ [class_ "w-12"] ""
      tbody_ $ forM_ sessions \session -> tr_ [class_ "hover:bg-fillHover"] do
        td_ [class_ "max-w-64"] do
          a_ (sessionLinkAttrs session.id <> [class_ "block truncate font-medium text-textStrong hover:text-textBrand"]) $ toHtml $ sessionIdentity session
          span_ [class_ "block truncate font-mono text-xs text-textWeak"] $ toHtml session.id
        td_ [class_ "max-w-56"] do
          -- A recording with no correlated spans is a real session, not a mystery: say what
          -- it is instead of stacking "Unknown page" over "0 views · 0 events".
          span_ [class_ $ "block truncate text-xs " <> bool "text-textStrong" "text-textWeak" (replayOnly session)] $ toHtml $ fromMaybe (bool "Unknown page" "Recording only — no telemetry events" (replayOnly session)) session.lastPage
          forM_ session.service $ span_ [class_ "block truncate text-xs text-textWeak"] . toHtml
        td_ [class_ "text-right"] do
          div_ [class_ "flex justify-end gap-1"] do
            when (session.errors > 0) $ span_ [class_ "badge badge-sm badge-error gap-1"] $ faSprite_ "triangle-exclamation" "solid" "h-2.5 w-2.5" >> toHtml (show session.errors)
            when session.hasReplay $ span_ [class_ "badge badge-sm badge-ghost gap-1"] $ faSprite_ "video" "regular" "h-2.5 w-2.5" >> "Replay"
          unless (replayOnly session) $ span_ [class_ "mt-0.5 block text-xs tabular-nums text-textWeak"] $ toHtml $ show session.views <> " views · " <> show session.events <> " events"
        td_ [class_ "text-right text-xs tabular-nums text-textWeak"] $ toHtml $ formatSessionDuration session
        td_ [class_ "text-right"] do
          if session.hasReplay
            then a_ (sessionLinkAttrs session.id <> [class_ "btn btn-ghost btn-xs", Aria.label_ $ "Watch replay for " <> sessionIdentity session, data_ "tippy-content" "Watch replay"]) $ faSprite_ "circle-play" "regular" "h-4 w-4"
            else a_ [href_ $ sessionLogsUrl links session.id, class_ "btn btn-ghost btn-xs", Aria.label_ $ "Inspect telemetry for " <> sessionIdentity session, data_ "tippy-content" "Inspect telemetry"] $ faSprite_ "arrow-up-right" "regular" "h-3.5 w-3.5"
  where
    sessionLinkAttrs sid =
      let url = sessionsUrl links query sessionFilter $ Just sid
       in href_ url
            : if workspace
              then
                [ -- @panel=sessions@ is what makes the response carry the workspace at all: a
                  -- deferred request without a panel renders every slot as a shell, so the
                  -- hx-select found nothing and the outerHTML swap deleted the workspace.
                  hxGet_ $ url <> "&panel=sessions&deferred=1"
                , hxTarget_ "#rum-replay-workspace"
                , hxSelect_ "#rum-replay-workspace"
                , hxSwap_ "outerHTML"
                , hxPushUrl_ url
                , hxIndicator_ "#rum-replay-workspace"
                , term "hx-sync" "#rum-replay-workspace:replace"
                ]
              else []


replayWorkspace_ :: RumLinks -> Maybe RumSession -> Html ()
replayWorkspace_ links = \case
  Just session | session.hasReplay -> div_ [class_ "min-h-full"] do
    header_ [class_ "flex flex-wrap items-start justify-between gap-3 border-b border-strokeWeak bg-bgBase px-4 py-3"] do
      div_ [class_ "min-w-0"] do
        h2_ [class_ "truncate text-sm font-semibold text-textStrong"] $ toHtml $ sessionIdentity session
        p_ [class_ "mt-0.5 truncate font-mono text-xs text-textWeak"] $ toHtml session.id
      a_ [href_ $ sessionLogsUrl links session.id, class_ "btn btn-sm gap-1.5"] do
        faSprite_ "magnifying-glass-chart" "regular" "h-3.5 w-3.5"
        "Inspect telemetry"
    termRaw "session-replay" [id_ "rumSessionReplay", term "initialSession" session.id, term "consoleOpen" "true", term "fullWidth" "true", class_ "block min-h-[34rem] w-full", term "projectId" links.pid.toText, term "containerId" "rum-replay-workspace"] ("" :: Text)
  Just session -> replayPrompt_ "No recording for this session" "Open its correlated OpenTelemetry events to inspect navigation, network, and error spans." $ Just ("Inspect telemetry", sessionLogsUrl links session.id)
  Nothing -> replayPrompt_ "Select a session" "Choose a row with Replay to watch the user's experience beside its errors and OpenTelemetry context." Nothing


replayPrompt_ :: Text -> Text -> Maybe (Text, Text) -> Html ()
replayPrompt_ title description action =
  div_ [class_ "flex min-h-[34rem] flex-col items-center justify-center p-8"]
    $ Components.emptyState_ def{icon = Just "video", action = maybe ESNone (\(label, url) -> ESLink url label) action} title description


performance_ :: RumData -> Html ()
performance_ page = div_ [class_ "space-y-4 p-4 max-md:p-3"] do
  slot_ page PanelVitals (panelSkeleton_ $ Components.tableSkeleton_ 6) $ vitalsTable_ page
  slot_ page PanelVitalTrend (panelSkeleton_ Components.chartSkeleton_) do
    vitalTrendPanel_ page.vitalTrend
    div_ [class_ "mt-4"] $ pageVitalsTable_ page.pageVitals
  div_ [class_ "grid grid-cols-2 gap-4 max-lg:grid-cols-1"] do
    slot_ page PanelPages (panelSkeleton_ $ Components.tableSkeleton_ 5) $ topPages_ page.links page.pages
    slot_ page PanelErrors (panelSkeleton_ $ Components.tableSkeleton_ 5) $ recentErrors_ page.links page.errors
  slot_ page PanelAudience (panelSkeleton_ $ Components.tableSkeleton_ 3) $ audiencePanel_ page.breakdown


-- | One strip per vital: P75 per interval, colored by its own rating. A regression reads
-- as the strip turning amber mid-window — visible in a way two aggregate numbers never are.
-- Where two emitters report the same vital in a bucket, the worse P75 is shown.
vitalTrendPanel_ :: [VitalTrendPoint] -> Html ()
vitalTrendPanel_ points = section_ [class_ "rounded-lg border border-strokeWeak bg-bgBase"] do
  panelHeader_ "Web Vitals over time" "P75 per interval, colored by Google's thresholds" Nothing
  if null points
    then panelEmpty_ "No web vital samples in this time range"
    else div_ [class_ "grid grid-cols-2 gap-px bg-strokeWeak max-lg:grid-cols-1"] $ forM_ vitalDefinitions \vital -> do
      let series = sortWith fst $ M.toList $ M.fromListWith max [(p.bucket, p.p75) | p <- points, vitalKey p.metricName == vital.name]
          maxValue = foldl' max vital.poorAt $ map snd series
      unless (null series) $ figure_ [class_ "bg-bgBase px-3 py-2.5", Aria.label_ $ vital.label <> " P75 over time"] do
        div_ [class_ "flex items-baseline justify-between gap-2"] do
          h3_ [class_ "text-sm font-medium text-textStrong"] $ toHtml vital.label
          forM_ (viaNonEmpty last series) \(_, latest) ->
            span_ [class_ $ "text-xs font-semibold tabular-nums " <> (ratingStyle $ classifyVital vital.goodAt vital.poorAt $ Just latest).textClass]
              $ toHtml
              $ formatVitalThreshold vital latest
        div_ [class_ "mt-1.5 flex h-12 items-end gap-px"] $ forM_ series \(bucketTime, value) -> do
          let barHeight = max 12 $ round @Double @Int $ value / maxValue * 100
              rating = classifyVital vital.goodAt vital.poorAt $ Just value
          div_
            [ class_ $ "min-w-1 flex-1 rounded-t-sm " <> (ratingStyle rating).fillClass
            , style_ $ "height:" <> show barHeight <> "%"
            , data_ "tippy-content" $ fmtDate "%d %b %H:%M" bucketTime <> ": " <> formatVitalThreshold vital value
            ]
            ""


-- | Path of a page URL for display; the full URL stays in the tooltip.
--
-- >>> pageLabel "https://shop.example/cart"
-- "/cart"
-- >>> pageLabel "https://shop.example"
-- "/"
-- >>> pageLabel "/checkout"
-- "/checkout"
pageLabel :: Text -> Text
pageLabel url
  | "://" `T.isInfixOf` url = "/" <> T.intercalate "/" (drop 3 $ T.splitOn "/" url)
  | otherwise = url


-- | Sentry's signature vitals view: one row per page, P75 per vital, each judged on its
-- own thresholds. Rows are ordered by sample count so the busiest pages lead.
pageVitalsTable_ :: [PageVitalPoint] -> Html ()
pageVitalsTable_ points = section_ [class_ "overflow-hidden rounded-lg border border-strokeWeak bg-bgBase"] do
  panelHeader_ "Web Vitals by page" "P75 per page — a site-wide average hides the page that regressed" Nothing
  if null points
    then panelEmpty_ "No page-attributed web vital samples in this time range"
    else div_ [class_ "overflow-x-auto"] $ table_ [class_ "table table-sm w-full"] do
      thead_ $ tr_ do
        th_ "Page"
        forM_ vitalDefinitions $ th_ [class_ "text-right"] . toHtml . T.toUpper . (.name)
        th_ [class_ "text-right"] "Samples"
      tbody_ $ forM_ pageRows \(page, byVital, sampleTotal) -> tr_ do
        td_ [class_ "max-w-xs"] $ span_ [class_ "block truncate font-medium text-textStrong", data_ "tippy-content" page] $ toHtml $ pageLabel page
        forM_ vitalDefinitions \vital -> td_ [class_ "text-right"] $ case M.lookup vital.name byVital of
          Nothing -> span_ [class_ "text-textWeak"] "—"
          Just value ->
            span_ [class_ $ "font-medium tabular-nums " <> (ratingStyle $ classifyVital vital.goodAt vital.poorAt $ Just value).textClass]
              $ toHtml
              $ formatVitalThreshold vital value
        td_ [class_ "text-right tabular-nums text-textWeak"] $ toHtml $ show sampleTotal
  where
    pageRows =
      take 12
        $ sortWith (\(_, _, sampleTotal) -> Down sampleTotal)
        $ map (\(page, cells) -> (page, M.fromListWith max [(vitalKey p.metricName, p.p75) | p <- cells], sum $ map (.samples) cells))
        $ M.toList
        $ M.fromListWith (<>) [(p.page, [p]) | p <- points]


vitalsTable_ :: RumData -> Html ()
vitalsTable_ page = do
  section_ [class_ "overflow-hidden rounded-lg border border-strokeWeak bg-bgBase"] do
    panelHeader_ "Web Vitals field performance" "P75 of the most recent samples — what most real users experience; thresholds follow the Core Web Vitals assessment model" Nothing
    div_ [class_ "overflow-x-auto"] $ table_ [class_ "table table-sm w-full"] do
      thead_ $ tr_ $ th_ "Metric" >> th_ [class_ "text-right"] "P75" >> th_ [class_ "text-right"] "Good" >> th_ [class_ "text-right"] "Poor" >> th_ "Assessment" >> th_ [class_ "text-right"] "Samples"
      tbody_ $ forM_ page.vitals \vital -> tr_ do
        td_ do
          strong_ [class_ "block text-sm font-medium text-textStrong"] $ toHtml vital.label
          span_ [class_ "text-xs text-textWeak"] $ toHtml vital.description
        td_ [class_ $ "text-right font-semibold tabular-nums " <> (ratingStyle vital.rating).textClass] $ toHtml $ formatVital vital
        td_ [class_ "text-right tabular-nums text-textWeak"] $ toHtml $ formatVitalThreshold vital vital.goodAt
        td_ [class_ "text-right tabular-nums text-textWeak"] $ toHtml $ formatVitalThreshold vital vital.poorAt
        td_ $ span_ [class_ $ "inline-flex items-center gap-1.5 rounded-md px-2 py-1 text-xs font-medium " <> (ratingStyle vital.rating).badgeClass] do
          span_ [class_ $ "h-2 w-2 rounded-full " <> (ratingStyle vital.rating).fillClass, Aria.hidden_ "true"] ""
          toHtml $ (ratingStyle vital.rating).label
        td_ [class_ "text-right tabular-nums text-textWeak"] $ toHtml $ show vital.samples


panelHeader_ :: Text -> Text -> Maybe (Text, Text) -> Html ()
panelHeader_ title subtitle action = header_ [class_ "flex items-start justify-between gap-3 border-b border-strokeWeak px-3 py-2.5"] do
  div_ [class_ "min-w-0"] do
    h2_ [class_ "text-sm font-semibold text-textStrong"] $ toHtml title
    p_ [class_ "mt-0.5 text-xs text-textWeak"] $ toHtml subtitle
  forM_ action $ \(label, url) -> a_ [href_ url, class_ "shrink-0 text-xs font-medium text-textBrand hover:underline"] $ toHtml label


panelEmpty_ :: Text -> Html ()
panelEmpty_ message = Components.emptyState_ def{size = ESCompact} message ""


sessionIdentity :: RumSession -> Text
sessionIdentity session = fromMaybe session.id $ session.userName <|> session.userEmail <|> session.userId


-- | A recording whose session id never appeared on a span: it has a replay and nothing else.
replayOnly :: RumSession -> Bool
replayOnly session = session.hasReplay && session.events == 0


formatSessionDuration :: RumSession -> Text
formatSessionDuration session
  | seconds < 1 = "<1s"
  | seconds < 60 = show (round seconds :: Int) <> "s"
  | seconds < 3600 = show (floor (seconds / 60) :: Int) <> "m " <> show (round (seconds `mod'` 60) :: Int) <> "s"
  | otherwise = show (floor (seconds / 3600) :: Int) <> "h " <> show (floor ((seconds `mod'` 3600) / 60) :: Int) <> "m"
  where
    seconds = realToFrac (diffUTCTime session.endedAt session.startedAt) :: Double


formatMilliseconds :: Double -> Text
formatMilliseconds value
  | value >= 1000 = showFFloat' 2 (value / 1000) <> " s"
  | otherwise = showFFloat' 0 value <> " ms"


formatVital :: Vital -> Text
formatVital vital = maybe "No data" (formatVitalThreshold vital) vital.value


formatVitalThreshold :: Vital -> Double -> Text
formatVitalThreshold vital value
  | vital.unit == "ms" = formatMilliseconds value
  | otherwise = showFFloat' 3 value


data RatingStyle = RatingStyle {label :: Text, textClass :: Text, fillClass :: Text, badgeClass :: Text}


ratingStyle :: VitalRating -> RatingStyle
ratingStyle = \case
  Good -> RatingStyle "Good" "text-textSuccess" "bg-fillSuccess-strong" "bg-fillSuccess-weak text-textSuccess"
  NeedsImprovement -> RatingStyle "Needs improvement" "text-textWarning" "bg-fillWarning-strong" "bg-fillWarning-weak text-textWarning"
  Poor -> RatingStyle "Poor" "text-textError" "bg-fillError-strong" "bg-fillError-weak text-textError"
  Unknown -> RatingStyle "No data" "text-textWeak" "bg-fillNeutral-strong" "bg-fillWeak text-textWeak"


-- | The KQL counterparts of 'browserScope' and 'pageViewPredicate'. A link that filtered on
-- @telemetry.sdk.language == "webjs"@ sent the user to an Explorer view of nothing, because no
-- browser SDK in production sets it — the same mistake the queries themselves made.
browserKql :: Text
browserKql = "(resource.telemetry.sdk.language == \"webjs\" or resource.user_agent.original != \"\" or name in (\"documentLoad\", \"documentFetch\") or name startswith \"Pageview \")"


browserPageViewKql :: Text
browserPageViewKql = browserKql <> " and (name == \"documentLoad\" or name startswith \"Pageview \")"


-- | Matches 'pagePath': our SDK sets url.path, the browser SDK only url.full.
pagePathKql :: Text -> Text
pagePathKql path = "(attributes.url.path == " <> kqlValue path <> " or attributes.url.full == " <> kqlValue path <> ")"


-- | KQL string literal: backslashes first, then quotes, so a value can't break out of the literal.
kqlValue :: Text -> Text
kqlValue value = "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" value) <> "\""


-- | A RUM URL carrying the scope the page is under. 'TimePicker.windowUrl' URI-encodes each
-- value, so nothing here may encode first: a pre-encoded KQL query arrives at the Log
-- Explorer double-escaped (@%2520@ for a space) and parses as one meaningless token.
rumUrl :: RumLinks -> [(Text, Text)] -> Text
rumUrl links extras = TimePicker.windowUrl ("/p/" <> links.pid.toText <> "/rum") (extras <> [("service", s) | s <- maybeToList links.service]) links.window


-- | Log Explorer link for a browser query. The service scope is appended to the KQL rather
-- than passed alongside it, so what the Explorer lists is what the panel counted.
logsUrl :: RumLinks -> Text -> Text
logsUrl links query =
  TimePicker.windowUrl
    ("/p/" <> links.pid.toText <> "/log_explorer")
    [("query", query <> foldMap (\value -> " and resource.service.name == " <> kqlValue value) links.service)]
    links.window


-- | Log Explorer link for one session's correlated events.
sessionLogsUrl :: RumLinks -> Text -> Text
sessionLogsUrl links sid = logsUrl links $ "attributes.session.id == " <> kqlValue sid


sessionsUrl :: RumLinks -> Maybe Text -> SessionFilter -> Maybe Text -> Text
sessionsUrl links query sessionFilter sessionM =
  rumUrl links
    $ [("tab", "sessions")]
    <> maybeToList (("q",) <$> query)
    <> maybeToList (("filter",) <$> sessionFilterParam sessionFilter)
    <> maybeToList (("session",) <$> sessionM)


degradedBanner_ :: [Text] -> Html ()
degradedBanner_ panels = div_ [role_ "alert", class_ "flex items-start gap-2 border-b border-strokeWarning-strong bg-fillWarning-weak px-4 py-2.5 text-sm text-textStrong"] do
  faSprite_ "triangle-exclamation" "solid" "mt-0.5 h-4 w-4 shrink-0 text-iconWarning"
  div_ do
    strong_ "Some RUM data could not be loaded."
    span_ [class_ "ml-1 text-textWeak"] $ toHtml $ "Retry or narrow the time range. Unavailable: " <> T.intercalate ", " panels <> "."
