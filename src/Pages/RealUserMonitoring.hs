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
) where

import Data.Cache qualified as Cache
import Data.Default (def)
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Data.Fixed (mod')
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
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
import Models.Telemetry.RUM (ReplaySession (..), RumBucket (..), RumCacheKey (..), RumError (..), RumPage (..), RumQuery (..), RumQueryResult (..), RumSession (..), RumSummary (..), RumTrend (..), VitalSample (..))
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, navTabAttrs)
import Pages.Components (Deferred (..), EmptyStateAction (..), EmptyStateCfg (..), EmptyStateSize (..), withDeferredBody)
import Pages.Components qualified as Components
import Pages.Containers (showFFloat')
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.DeriveUtils (DB)
import Relude
import Relude.Extra.Foldable1 (maximum1)
import System.Config (AuthContext (..), EnvConfig (enableTimefusionReads))
import System.Logging qualified as Log
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import UnliftIO (tryAny)
import Utils (faSprite_)


data RumTab = Overview | Sessions | Performance
  deriving stock (Eq, Show)


data SessionFilter = AllSessionRows | ErrorSessionRows | ReplaySessionRows
  deriving stock (Eq)


renderBucket :: RumBucket -> Text
renderBucket = \case
  FiveMinutes -> "5 minutes"
  OneHour -> "1 hour"
  SixHours -> "6 hours"


parseTab :: Maybe Text -> RumTab
parseTab = \case
  Just "sessions" -> Sessions
  Just "performance" -> Performance
  _ -> Overview


tabParam :: RumTab -> Text
tabParam = \case
  Overview -> "overview"
  Sessions -> "sessions"
  Performance -> "performance"


tabLabel :: RumTab -> Text
tabLabel = T.toTitle . tabParam


parseSessionFilter :: Maybe Text -> SessionFilter
parseSessionFilter = \case
  Just "errors" -> ErrorSessionRows
  Just "replays" -> ReplaySessionRows
  _ -> AllSessionRows


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


vitalsFromSamples :: [VitalSample] -> [Vital]
vitalsFromSamples samples = map measure vitalDefinitions
  where
    byName = M.fromListWith (<>) [(T.toLower $ fromMaybe s.metricName $ T.stripPrefix "browser.web_vital." s.metricName, [(s.value, s.samples)]) | s <- samples]
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


rumSummary :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> Eff es RumSummary
rumSummary scope =
  fromMaybe (RumSummary 0 0 0 0)
    <$> Hasql.withHasqlTimefusion
      scope.useTf
      ( Hasql.interpOne
          ( [HI.sql|
            SELECT
              COUNT(DISTINCT NULLIF(attributes___session___id, ''))::bigint,
              COUNT(*) FILTER (WHERE |]
              <> pageViewPredicate
              <> [HI.sql|)::bigint,
              COUNT(DISTINCT NULLIF(COALESCE(attributes___user___id, attributes___user___email), ''))::bigint,
              COUNT(*) FILTER (WHERE status_code = 'ERROR' OR lower(COALESCE(level, '')) = 'error' OR attributes___exception___type IS NOT NULL)::bigint
            FROM otel_logs_and_spans
            WHERE |]
              <> browserScope scope
          )
      )


rumTrend :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> RumBucket -> Eff es [RumTrend]
rumTrend scope bucket =
  Hasql.withHasqlTimefusion scope.useTf
    $ Hasql.interp
      ( [HI.sql|SELECT time_bucket(|]
          <> fromString (toString $ "'" <> renderBucket bucket <> "'")
          <> [HI.sql|, timestamp),
          COUNT(*) FILTER (WHERE |]
          <> pageViewPredicate
          <> [HI.sql|)::bigint,
          COUNT(*) FILTER (WHERE status_code = 'ERROR' OR lower(COALESCE(level, '')) = 'error' OR attributes___exception___type IS NOT NULL)::bigint
        FROM otel_logs_and_spans
        WHERE |]
          <> browserScope scope
          <> [HI.sql| GROUP BY 1 ORDER BY 1|]
      )


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
          <> [HI.sql| AND (status_code = 'ERROR' OR lower(COALESCE(level, '')) = 'error' OR attributes___exception___type IS NOT NULL)
        ORDER BY timestamp DESC LIMIT 20|]
      )


otelSessions :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> Eff es [RumSession]
otelSessions scope =
  Hasql.withHasqlTimefusion scope.useTf
    $ Hasql.interp
      ( [HI.sql|
        SELECT attributes___session___id,
          MIN(timestamp), MAX(timestamp), COUNT(*)::bigint,
          COUNT(*) FILTER (WHERE status_code = 'ERROR' OR lower(COALESCE(level, '')) = 'error' OR attributes___exception___type IS NOT NULL)::bigint,
          COUNT(*) FILTER (WHERE |]
          <> pageViewPredicate
          <> [HI.sql|)::bigint,
          MAX(attributes___user___id), MAX(attributes___user___full_name), MAX(attributes___user___email),
          MAX(resource___service___name), MAX(|]
          <> pagePath
          <> [HI.sql|),
          false
        FROM otel_logs_and_spans
        WHERE |]
          <> browserScope scope
          <> [HI.sql| AND attributes___session___id IS NOT NULL AND attributes___session___id <> ''
        GROUP BY attributes___session___id ORDER BY MAX(timestamp) DESC LIMIT 200|]
      )


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
          <> [HI.sql| AND metric_name IN ('browser.web_vital.lcp', 'browser.web_vital.inp', 'browser.web_vital.cls', 'browser.web_vital.fcp', 'browser.web_vital.ttfb')
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


data RumData = RumData
  { links :: RumLinks
  , tab :: RumTab
  , summary :: RumSummary
  , trend :: [RumTrend]
  , pages :: [RumPage]
  , errors :: [RumError]
  , sessions :: [RumSession]
  , vitals :: [Vital]
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
  SummaryResult summary -> summary.sessions > 0 || summary.pageViews > 0 || summary.users > 0 || summary.errors > 0
  TrendResult rows -> not $ null rows
  PagesResult rows -> not $ null rows
  ErrorsResult rows -> not $ null rows
  SessionsResult rows -> not $ null rows
  ReplaySessionsResult rows -> not $ null rows
  VitalSamplesResult rows -> not $ null rows
  ServicesResult names -> not $ null names


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


rumGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders RumGet)
rumGetH pid tabM queryM sessionFilterM fromM toM sinceM selectedM serviceM deferredM = do
  (session, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  let tab = parseTab tabM
      sessionFilter = parseSessionFilter sessionFilterM
      window = TimePicker.mkTimeWindow now fromM toM (sinceM <|> Just "24H")
      environment = session.environment
      -- An empty select option clears the filter, and "" is not a service name.
      serviceFilter = serviceM >>= \value -> value <$ guard (not $ T.null value)
      scope = RumScope{useTf = appCtx.env.enableTimefusionReads, pid, fromTime = window.fromTime, toTime = window.toTime, environment, service = serviceFilter}
      links = RumLinks{pid, window, service = serviceFilter}
      bucket
        | diffUTCTime window.toTime window.fromTime <= 6 * 3600 = FiveMinutes
        | diffUTCTime window.toTime window.fromTime <= 3 * 86400 = OneHour
        | otherwise = SixHours
      cacheKey query = RumCacheKey pid query environment serviceFilter fromM toM (sinceM <|> Just "24H")
      summaryQ = ("summary" :: Text, cacheKey SummaryQuery, SummaryResult <$> rumSummary scope)
      trendQ = ("activity" :: Text, cacheKey $ TrendQuery bucket, TrendResult <$> rumTrend scope bucket)
      pagesQ = ("pages" :: Text, cacheKey PagesQuery, PagesResult <$> rumPages scope)
      errorsQ = ("errors" :: Text, cacheKey ErrorsQuery, ErrorsResult <$> rumErrors scope)
      sessionsQ = ("sessions" :: Text, cacheKey SessionsQuery, SessionsResult <$> otelSessions scope)
      replaysQ = ("replays" :: Text, cacheKey ReplaySessionsQuery, ReplaySessionsResult <$> replaySessions scope)
      vitalsQ = ("web vitals" :: Text, cacheKey VitalSamplesQuery, VitalSamplesResult <$> vitalSamples scope)
      -- The picker is on every tab, so its options are read on every tab.
      -- Keyed unscoped, matching the query: the option list is the same whichever service is
      -- selected, so scoping the key would cache one identical list per service.
      servicesQ = ("services" :: Text, RumCacheKey pid ServicesQuery environment Nothing fromM toM (sinceM <|> Just "24H"), ServicesResult <$> rumServices scope)
      -- Run together. These were once batched two at a time on the finding that TimeFusion
      -- degrades when they compete — but that was measured while every one of them matched
      -- zero rows, because the browser filter was broken. Re-measured against real data, four
      -- concurrent scans finish in 7.4s and the same four in two batches take 8.1s: the
      -- contention is real but far cheaper than serialising.
      tabQueries = case tab of
        Overview -> [summaryQ, trendQ, pagesQ, errorsQ, sessionsQ, replaysQ, vitalsQ, servicesQ]
        Sessions -> [sessionsQ, replaysQ, servicesQ]
        Performance -> [pagesQ, errorsQ, vitalsQ, servicesQ]
      runQuery (label, key, action) =
        tryAny
          ( liftIO (Cache.lookup appCtx.rumCache key)
              >>= maybe (action >>= \fresh -> fresh <$ when (cacheableRumResult fresh) (liftIO $ Cache.insert appCtx.rumCache key fresh)) pure
          )
          >>= either (\err -> Left label <$ Log.logAttention "RUM panel query failed" (label, displayException err)) (pure . Right)
      deferredUrl =
        TimePicker.windowUrl
          ("/p/" <> pid.toText <> "/rum")
          ([(key, value) | (key, Just value) <- [("tab", tabM), ("q", queryM), ("filter", sessionFilterM), ("session", selectedM), ("service", serviceFilter)]] <> [("deferred", "1")])
          window
  body <- withDeferredBody deferredM "rum-page" deferredUrl (rumSkeleton_ tab) do
    outcomes <- pooledForConcurrently tabQueries runQuery
    let results = rights outcomes
        degradedPanels = lefts outcomes
        summary = fromMaybe (RumSummary 0 0 0 0) $ listToMaybe [value | SummaryResult value <- results]
        trend = fold [value | TrendResult value <- results]
        pages = fold [value | PagesResult value <- results]
        errors = fold [value | ErrorsResult value <- results]
        sessions = mergeSessions serviceFilter (fold [value | SessionsResult value <- results]) (fold [value | ReplaySessionsResult value <- results])
        vitals = vitalsFromSamples $ fold [value | VitalSamplesResult value <- results]
    let services = fold [value | ServicesResult value <- results]
    pure RumData{links, tab, summary, trend, pages, errors, sessions, vitals, services, query = queryM, sessionFilter, selectedSession = selectedM, degradedPanels}
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
  forM_ [Overview, Sessions, Performance] \tab -> do
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


rumPage_ :: RumData -> Html ()
rumPage_ page = main_ [id_ "rum-page", class_ "min-h-full bg-bgSunken"] do
  unless (null page.degradedPanels) $ degradedBanner_ page.degradedPanels
  servicePicker_ page
  if hasRumData page
    then case page.tab of
      Overview -> overview_ page
      Sessions -> sessions_ page
      Performance -> performance_ page
    else maybe (rumEmptyState_ page.links.pid) (scopedEmptyState_ page.links) page.links.service


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


hasRumData :: RumData -> Bool
hasRumData page = page.summary.sessions > 0 || page.summary.pageViews > 0 || any (isJust . (.value)) page.vitals || any (.hasReplay) page.sessions


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
  pulse_ page
  div_ [class_ "grid grid-cols-[minmax(0,1.65fr)_minmax(18rem,0.75fr)] gap-4 max-xl:grid-cols-1"] do
    div_ [class_ "min-w-0 space-y-4"] do
      trendPanel_ page.trend
      topPages_ page.links page.pages
    aside_ [class_ "min-w-0 space-y-4"] do
      vitalsPanel_ page.vitals
      recentErrors_ page.links page.errors
  recentSessions_ page


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
    maxViews = max 1 $ fromMaybe 1 $ viaNonEmpty maximum1 $ map (.pageViews) points
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
  panelHeader_ "Core Web Vitals" "75th percentile against standard thresholds" Nothing
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
    ratingBand Good vital.rating
    ratingBand NeedsImprovement vital.rating
    ratingBand Poor vital.rating


ratingBand :: VitalRating -> VitalRating -> Html ()
ratingBand band active = span_ [class_ $ "h-1.5 rounded-full " <> if band == active then (ratingStyle band).fillClass else "bg-fillWeak", Aria.hidden_ "true"] ""


recentErrors_ :: RumLinks -> [RumError] -> Html ()
recentErrors_ links errors = section_ [class_ "rounded-lg border border-strokeWeak bg-bgBase"] do
  panelHeader_ "Recent browser errors" "Errors stay linked to user and session context" $ Just ("View errors", logsUrl links (browserKql <> " and status_code == \"ERROR\""))
  if null errors
    then div_ [class_ "flex items-center gap-2 px-3 py-5 text-sm text-textWeak"] $ faSprite_ "circle-check" "regular" "h-4 w-4 text-textSuccess" >> "No browser errors in this range"
    else ul_ [class_ "divide-y divide-strokeWeak"] $ forM_ (take 6 errors) \err -> li_ [class_ "px-3 py-2.5"] do
      div_ [class_ "flex items-start gap-2"] do
        faSprite_ "triangle-exclamation" "solid" "mt-0.5 h-3.5 w-3.5 shrink-0 text-iconError"
        div_ [class_ "min-w-0 flex-1"] do
          div_ [class_ "flex items-center justify-between gap-2"] do
            strong_ [class_ "truncate text-sm font-medium text-textStrong"] $ toHtml err.errorType
            time_ [datetime_ $ show err.timestamp, class_ "shrink-0 text-xs text-textWeak"] $ toHtml $ fmtDate "%H:%M" err.timestamp
          p_ [class_ "mt-0.5 line-clamp-2 text-xs text-textWeak"] $ toHtml err.message
          div_ [class_ "mt-1 flex flex-wrap gap-x-2 text-xs text-textWeak"] do
            forM_ err.path $ span_ [class_ "font-mono"] . toHtml
            forM_ (err.userId <|> err.sessionId) $ span_ . toHtml


recentSessions_ :: RumData -> Html ()
recentSessions_ page = section_ [class_ "overflow-hidden rounded-lg border border-strokeWeak bg-bgBase"] do
  panelHeader_ "Recent sessions" "Open a recording or inspect its correlated telemetry" $ Just ("View all sessions", sessionsUrl page.links Nothing AllSessionRows Nothing)
  sessionTable_ False page.links (take 8 page.sessions) Nothing AllSessionRows


sessions_ :: RumData -> Html ()
sessions_ page = do
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
          span_ [class_ "block truncate text-xs text-textStrong"] $ toHtml $ fromMaybe "Unknown page" session.lastPage
          forM_ session.service $ span_ [class_ "block truncate text-xs text-textWeak"] . toHtml
        td_ [class_ "text-right"] do
          div_ [class_ "flex justify-end gap-1"] do
            when (session.errors > 0) $ span_ [class_ "badge badge-sm badge-error gap-1"] $ faSprite_ "triangle-exclamation" "solid" "h-2.5 w-2.5" >> toHtml (show session.errors)
            when session.hasReplay $ span_ [class_ "badge badge-sm badge-ghost gap-1"] $ faSprite_ "video" "regular" "h-2.5 w-2.5" >> "Replay"
          span_ [class_ "mt-0.5 block text-xs tabular-nums text-textWeak"] $ toHtml $ show session.views <> " views · " <> show session.events <> " events"
        td_ [class_ "text-right text-xs tabular-nums text-textWeak"] $ toHtml $ formatSessionDuration session
        td_ [class_ "text-right"] do
          if session.hasReplay
            then a_ (sessionLinkAttrs session.id <> [class_ "btn btn-ghost btn-xs", Aria.label_ $ "Watch replay for " <> sessionIdentity session, data_ "tippy-content" "Watch replay"]) $ faSprite_ "circle-play" "regular" "h-4 w-4"
            else a_ [href_ $ logsUrl links ("attributes.session.id == " <> kqlValue session.id), class_ "btn btn-ghost btn-xs", Aria.label_ $ "Inspect telemetry for " <> sessionIdentity session, data_ "tippy-content" "Inspect telemetry"] $ faSprite_ "arrow-up-right" "regular" "h-3.5 w-3.5"
  where
    sessionLinkAttrs sid =
      let url = sessionsUrl links query sessionFilter $ Just sid
       in href_ url
            : if workspace
              then
                [ hxGet_ $ url <> "&deferred=1"
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
      a_ [href_ $ logsUrl links ("attributes.session.id == " <> kqlValue session.id), class_ "btn btn-sm gap-1.5"] do
        faSprite_ "magnifying-glass-chart" "regular" "h-3.5 w-3.5"
        "Inspect telemetry"
    termRaw "session-replay" [id_ "rumSessionReplay", term "initialSession" session.id, term "consoleOpen" "true", term "fullWidth" "true", class_ "block min-h-[34rem] w-full", term "projectId" links.pid.toText, term "containerId" "rum-replay-workspace"] ("" :: Text)
  Just session -> replayPrompt_ "No recording for this session" "Open its correlated OpenTelemetry events to inspect navigation, network, and error spans." $ Just ("Inspect telemetry", logsUrl links ("attributes.session.id == " <> kqlValue session.id))
  Nothing -> replayPrompt_ "Select a session" "Choose a row with Replay to watch the user's experience beside its errors and OpenTelemetry context." Nothing


replayPrompt_ :: Text -> Text -> Maybe (Text, Text) -> Html ()
replayPrompt_ title description action = div_ [class_ "flex min-h-[34rem] flex-col items-center justify-center p-8 text-center"] do
  faSprite_ "video" "regular" "mb-3 h-8 w-8 text-iconNeutral"
  h2_ [class_ "text-base font-semibold text-textStrong"] $ toHtml title
  p_ [class_ "mt-1 max-w-md text-sm text-textWeak"] $ toHtml description
  forM_ action $ \(label, url) -> a_ [href_ url, class_ "btn btn-sm mt-4"] $ toHtml label


performance_ :: RumData -> Html ()
performance_ page = div_ [class_ "space-y-4 p-4 max-md:p-3"] do
  section_ [class_ "overflow-hidden rounded-lg border border-strokeWeak bg-bgBase"] do
    panelHeader_ "Web Vitals field performance" "P75 reflects what most real users experience; thresholds follow the Core Web Vitals assessment model" Nothing
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
  div_ [class_ "grid grid-cols-2 gap-4 max-lg:grid-cols-1"] do
    topPages_ page.links page.pages
    recentErrors_ page.links page.errors


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


formatSessionDuration :: RumSession -> Text
formatSessionDuration session
  | seconds < 1 = "<1s"
  | seconds < 60 = show (round seconds :: Int) <> "s"
  | seconds < 3600 = show (floor (seconds / 60) :: Int) <> "m " <> show (round (seconds `mod'` 60) :: Int) <> "s"
  | otherwise = show (floor (seconds / 3600) :: Int) <> "h " <> show (floor ((seconds `mod'` 3600) / 60) :: Int) <> "m"
  where
    seconds = realToFrac (diffUTCTime session.endedAt session.startedAt) :: Double


fmtDate :: Text -> UTCTime -> Text
fmtDate format = toText . formatTime defaultTimeLocale (toString format)


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
