-- | Real User Monitoring projects browser OpenTelemetry and session recordings into one
-- investigation surface. The dashboard template is the customizable aggregate view; this page
-- owns the user/session workflow that a dashboard cannot express.
module Pages.RealUserMonitoring (
  rumGetH,
  RumGet (..),
  RumData (..),
  RumSummary (..),
  RumSession (..),
  SessionFilter (..),
  Vital (..),
  VitalRating (..),
  classifyVital,
) where

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
import Models.Projects.Projects qualified as Projects
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
import Utils (faSprite_, toUriStr)


data RumTab = Overview | Sessions | Performance
  deriving stock (Eq, Show)


data RumBucket = FiveMinutes | OneHour | SixHours


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


data RumSummary = RumSummary
  { sessions :: Int64
  , pageViews :: Int64
  , users :: Int64
  , errors :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data RumTrend = RumTrend
  { bucket :: UTCTime
  , pageViews :: Int64
  , errors :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data RumPage = RumPage
  { path :: Text
  , views :: Int64
  , p75LoadMs :: Maybe Double
  , lastSeen :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data RumError = RumError
  { timestamp :: UTCTime
  , errorType :: Text
  , message :: Text
  , sessionId :: Maybe Text
  , userId :: Maybe Text
  , path :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data ReplaySession = ReplaySession
  { id :: UUID.UUID
  , startedAt :: UTCTime
  , endedAt :: UTCTime
  , userId :: Maybe Text
  , userName :: Maybe Text
  , userEmail :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data RumSession = RumSession
  { id :: Text
  , startedAt :: UTCTime
  , endedAt :: UTCTime
  , events :: Int64
  , errors :: Int64
  , views :: Int64
  , userId :: Maybe Text
  , userName :: Maybe Text
  , userEmail :: Maybe Text
  , service :: Maybe Text
  , lastPage :: Maybe Text
  , hasReplay :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (HI.DecodeRow)


data RumQueryResult
  = SummaryResult RumSummary
  | TrendResult [RumTrend]
  | PagesResult [RumPage]
  | ErrorsResult [RumError]
  | SessionsResult [RumSession]
  | ReplaySessionsResult [ReplaySession]
  | VitalSamplesResult [VitalSample]


data VitalSample = VitalSample
  { metricName :: Text
  , value :: Double
  , samples :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


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


rumSummary :: (DB es, Labeled "timefusion" Hasql :> es) => Bool -> Projects.ProjectId -> UTCTime -> UTCTime -> Maybe Text -> Eff es RumSummary
rumSummary useTf pid fromTime toTime environment =
  fromMaybe (RumSummary 0 0 0 0)
    <$> Hasql.withHasqlTimefusion
      useTf
      ( Hasql.interpOne
          [HI.sql|
            SELECT
              COUNT(DISTINCT NULLIF(attributes___session___id, ''))::bigint,
              COUNT(*) FILTER (WHERE name LIKE 'Pageview · %')::bigint,
              COUNT(DISTINCT NULLIF(COALESCE(attributes___user___id, attributes___user___email), ''))::bigint,
              COUNT(*) FILTER (WHERE status_code = 'ERROR' OR lower(COALESCE(level, '')) = 'error' OR attributes___exception___type IS NOT NULL)::bigint
            FROM otel_logs_and_spans
            WHERE project_id = #{pid.toText}
              AND timestamp >= #{fromTime} AND timestamp <= #{toTime}
              AND (#{environment}::text IS NULL OR resource___deployment___environment___name = #{environment})
              AND resource___telemetry___sdk___language IN ('webjs', 'javascript', 'js')
          |]
      )


rumTrend :: (DB es, Labeled "timefusion" Hasql :> es) => Bool -> Projects.ProjectId -> UTCTime -> UTCTime -> Maybe Text -> RumBucket -> Eff es [RumTrend]
rumTrend useTf pid fromTime toTime environment bucket =
  Hasql.withHasqlTimefusion useTf
    $ Hasql.interp
      ( [HI.sql|SELECT time_bucket(|]
          <> fromString (toString $ "'" <> renderBucket bucket <> "'")
          <> [HI.sql|, timestamp),
          COUNT(*) FILTER (WHERE name LIKE 'Pageview · %')::bigint,
          COUNT(*) FILTER (WHERE status_code = 'ERROR' OR lower(COALESCE(level, '')) = 'error' OR attributes___exception___type IS NOT NULL)::bigint
        FROM otel_logs_and_spans
        WHERE project_id = #{pid.toText}
          AND timestamp >= #{fromTime} AND timestamp <= #{toTime}
          AND (#{environment}::text IS NULL OR resource___deployment___environment___name = #{environment})
          AND resource___telemetry___sdk___language IN ('webjs', 'javascript', 'js')
        GROUP BY 1 ORDER BY 1|]
      )


rumPages :: (DB es, Labeled "timefusion" Hasql :> es) => Bool -> Projects.ProjectId -> UTCTime -> UTCTime -> Maybe Text -> Eff es [RumPage]
rumPages useTf pid fromTime toTime environment =
  Hasql.withHasqlTimefusion useTf
    $ Hasql.interp
      [HI.sql|
        SELECT
          COALESCE(NULLIF(attributes___url___path, ''), replace(name, 'Pageview · ', '')),
          COUNT(*)::bigint,
          (approx_percentile(0.75, percentile_agg(duration)) / 1000000.0)::float8,
          MAX(timestamp)
        FROM otel_logs_and_spans
        WHERE project_id = #{pid.toText}
          AND timestamp >= #{fromTime} AND timestamp <= #{toTime}
          AND (#{environment}::text IS NULL OR resource___deployment___environment___name = #{environment})
          AND resource___telemetry___sdk___language IN ('webjs', 'javascript', 'js')
          AND name LIKE 'Pageview · %' AND duration IS NOT NULL
        GROUP BY 1 ORDER BY COUNT(*) DESC LIMIT 20
      |]


rumErrors :: (DB es, Labeled "timefusion" Hasql :> es) => Bool -> Projects.ProjectId -> UTCTime -> UTCTime -> Maybe Text -> Eff es [RumError]
rumErrors useTf pid fromTime toTime environment =
  Hasql.withHasqlTimefusion useTf
    $ Hasql.interp
      [HI.sql|
        SELECT timestamp,
          COALESCE(attributes___exception___type, status_message, 'Browser error'),
          COALESCE(attributes___exception___message, status_message, name, 'No error message'),
          attributes___session___id,
          COALESCE(attributes___user___id, attributes___user___email),
          attributes___url___path
        FROM otel_logs_and_spans
        WHERE project_id = #{pid.toText}
          AND timestamp >= #{fromTime} AND timestamp <= #{toTime}
          AND (#{environment}::text IS NULL OR resource___deployment___environment___name = #{environment})
          AND resource___telemetry___sdk___language IN ('webjs', 'javascript', 'js')
          AND (status_code = 'ERROR' OR lower(COALESCE(level, '')) = 'error' OR attributes___exception___type IS NOT NULL)
        ORDER BY timestamp DESC LIMIT 20
      |]


otelSessions :: (DB es, Labeled "timefusion" Hasql :> es) => Bool -> Projects.ProjectId -> UTCTime -> UTCTime -> Maybe Text -> Eff es [RumSession]
otelSessions useTf pid fromTime toTime environment =
  Hasql.withHasqlTimefusion useTf
    $ Hasql.interp
      [HI.sql|
        SELECT attributes___session___id,
          MIN(timestamp), MAX(timestamp), COUNT(*)::bigint,
          COUNT(*) FILTER (WHERE status_code = 'ERROR' OR lower(COALESCE(level, '')) = 'error' OR attributes___exception___type IS NOT NULL)::bigint,
          COUNT(*) FILTER (WHERE name LIKE 'Pageview · %')::bigint,
          MAX(attributes___user___id), MAX(attributes___user___full_name), MAX(attributes___user___email),
          MAX(resource___service___name), MAX(attributes___url___path),
          false
        FROM otel_logs_and_spans
        WHERE project_id = #{pid.toText}
          AND timestamp >= #{fromTime} AND timestamp <= #{toTime}
          AND (#{environment}::text IS NULL OR resource___deployment___environment___name = #{environment})
          AND resource___telemetry___sdk___language IN ('webjs', 'javascript', 'js')
          AND attributes___session___id IS NOT NULL AND attributes___session___id <> ''
        GROUP BY attributes___session___id ORDER BY MAX(timestamp) DESC LIMIT 200
      |]


replaySessions :: DB es => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es [ReplaySession]
replaySessions pid fromTime toTime =
  Hasql.interp
    [HI.sql|
      SELECT session_id, created_at, last_event_at, user_id, user_name, user_email
      FROM projects.replay_sessions
      WHERE project_id = #{pid} AND last_event_at >= #{fromTime} AND created_at <= #{toTime}
        AND (event_file_count > 0 OR cardinality(file_keys) > 0 OR cardinality(shard_keys) > 0)
      ORDER BY last_event_at DESC LIMIT 200
    |]


vitalSamples :: (DB es, Labeled "timefusion" Hasql :> es) => Bool -> Projects.ProjectId -> UTCTime -> UTCTime -> Maybe Text -> Eff es [VitalSample]
vitalSamples useTf pid fromTime toTime environment =
  Hasql.withHasqlTimefusion useTf
    $ Hasql.interp
      [HI.sql|
        SELECT metric_name,
          COALESCE(value, distribution_sum / NULLIF(distribution_count, 0))::float8,
          COALESCE(distribution_count, 1)::bigint
        FROM otel_metrics
        WHERE project_id = #{pid.toText}
          AND timestamp >= #{fromTime} AND timestamp <= #{toTime}
          AND (#{environment}::text IS NULL OR resource___deployment___environment___name = #{environment})
          AND metric_name IN ('browser.web_vital.lcp', 'browser.web_vital.inp', 'browser.web_vital.cls', 'browser.web_vital.fcp', 'browser.web_vital.ttfb')
          AND COALESCE(value, distribution_sum / NULLIF(distribution_count, 0)) IS NOT NULL
        ORDER BY timestamp DESC LIMIT 10000
      |]


mergeSessions :: [RumSession] -> [ReplaySession] -> [RumSession]
mergeSessions otel replays = sortWith (Down . (.endedAt)) $ M.elems $ foldl' addReplay (M.fromList [(s.id, s) | s <- otel]) replays
  where
    addReplay sessions replay =
      let sid = UUID.toText replay.id
       in M.alter (Just . maybe (fromReplay replay) (attachReplay replay)) sid sessions
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


data RumData = RumData
  { pid :: Projects.ProjectId
  , tab :: RumTab
  , summary :: RumSummary
  , trend :: [RumTrend]
  , pages :: [RumPage]
  , errors :: [RumError]
  , sessions :: [RumSession]
  , vitals :: [Vital]
  , window :: TimePicker.TimeWindow
  , query :: Maybe Text
  , sessionFilter :: SessionFilter
  , selectedSession :: Maybe Text
  , degradedPanels :: [Text]
  }


newtype RumGet = RumGet (PageCtx (Deferred RumData))


instance ToHtml RumGet where
  toHtml (RumGet page) = toHtml page
  toHtmlRaw = toHtml


-- | Every RUM panel is a separate scan of a 24-hour window, and a tab click re-runs all of
-- them. Holding the page chrome hostage to the slowest one is what makes switching tabs feel
-- broken, so the first request renders the tab strip, time picker and a skeleton, and the
-- panels arrive on the request the skeleton fires.
rumSkeleton_ :: Html ()
rumSkeleton_ = div_ [id_ "rum-page", class_ "min-h-full space-y-5 bg-bgSunken p-4", role_ "status", Aria.label_ "Loading real user monitoring"] do
  div_ [class_ "grid grid-cols-4 gap-px border-y border-strokeWeak bg-bgBase max-md:grid-cols-2"]
    $ replicateM_ 4
    $ div_ [class_ "flex flex-col gap-2 px-4 py-3"] do
      div_ [class_ "h-6 w-16 rounded skeleton-shimmer"] ""
      div_ [class_ "h-3 w-24 rounded skeleton-shimmer"] ""
  div_ [class_ "grid grid-cols-[minmax(0,1.65fr)_minmax(18rem,0.75fr)] gap-4 max-xl:grid-cols-1"] do
    div_ [class_ "rounded-lg border border-strokeWeak bg-bgBase p-4"] Components.chartSkeleton_
    div_ [class_ "rounded-lg border border-strokeWeak bg-bgBase"] $ Components.tableSkeleton_ 5
  div_ [class_ "rounded-lg border border-strokeWeak bg-bgBase"] $ Components.tableSkeleton_ 6


rumGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders RumGet)
rumGetH pid tabM queryM sessionFilterM fromM toM sinceM selectedM deferredM = do
  (session, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  let tab = parseTab tabM
      sessionFilter = parseSessionFilter sessionFilterM
      window = TimePicker.mkTimeWindow now fromM toM (sinceM <|> Just "24H")
      environment = session.environment
      useTf = appCtx.env.enableTimefusionReads
      bucket
        | diffUTCTime window.toTime window.fromTime <= 6 * 3600 = FiveMinutes
        | diffUTCTime window.toTime window.fromTime <= 3 * 86400 = OneHour
        | otherwise = SixHours
      summaryQ = ("summary", SummaryResult <$> rumSummary useTf pid window.fromTime window.toTime environment)
      trendQ = ("activity", TrendResult <$> rumTrend useTf pid window.fromTime window.toTime environment bucket)
      pagesQ = ("pages", PagesResult <$> rumPages useTf pid window.fromTime window.toTime environment)
      errorsQ = ("errors", ErrorsResult <$> rumErrors useTf pid window.fromTime window.toTime environment)
      sessionsQ = ("sessions", SessionsResult <$> otelSessions useTf pid window.fromTime window.toTime environment)
      replaysQ = ("replays", ReplaySessionsResult <$> replaySessions pid window.fromTime window.toTime)
      vitalsQ = ("web vitals", VitalSamplesResult <$> vitalSamples useTf pid window.fromTime window.toTime environment)
      tabQueries = case tab of
        Overview -> [summaryQ, trendQ, pagesQ, errorsQ, sessionsQ, replaysQ, vitalsQ]
        Sessions -> [sessionsQ, replaysQ]
        Performance -> [pagesQ, errorsQ, vitalsQ]
      runQuery (label, action) =
        tryAny action >>= either (\err -> Left label <$ Log.logAttention "RUM panel query failed" (label, displayException err)) (pure . Right)
      deferredUrl =
        TimePicker.windowUrl
          ("/p/" <> pid.toText <> "/rum")
          ([(key, value) | (key, Just value) <- [("tab", tabM), ("q", queryM), ("filter", sessionFilterM), ("session", selectedM)]] <> [("deferred", "1")])
          window
  body <- withDeferredBody deferredM "rum-page" deferredUrl rumSkeleton_ do
    outcomes <- pooledForConcurrently tabQueries runQuery
    let results = rights outcomes
        degradedPanels = lefts outcomes
        summary = fromMaybe (RumSummary 0 0 0 0) $ listToMaybe [value | SummaryResult value <- results]
        trend = fold [value | TrendResult value <- results]
        pages = fold [value | PagesResult value <- results]
        errors = fold [value | ErrorsResult value <- results]
        sessions = mergeSessions (fold [value | SessionsResult value <- results]) (fold [value | ReplaySessionsResult value <- results])
        vitals = vitalsFromSamples $ fold [value | VitalSamplesResult value <- results]
    pure RumData{pid, tab, summary, trend, pages, errors, sessions, vitals, window, query = queryM, sessionFilter, selectedSession = selectedM, degradedPanels}
  let conf =
        bw
          { pageTitle = "Real User Monitoring"
          , menuItem = Just "Real User Monitoring"
          , navTabs = Just $ rumNavTabs_ pid tab window
          , pageActions = Just $ rumActions_ pid window
          , docsLink = Just "https://monoscope.tech/docs/sdks/browser/"
          }
  addRespHeaders $ RumGet $ PageCtx conf body


rumNavTabs_ :: Projects.ProjectId -> RumTab -> TimePicker.TimeWindow -> Html ()
rumNavTabs_ pid active window = nav_ [class_ "tabs tabs-box tabs-outline items-center max-md:overflow-x-auto max-md:flex-nowrap", Aria.label_ "Real User Monitoring views", term "hx-preload" "mouseover"] do
  forM_ [Overview, Sessions, Performance] \tab -> do
    let url = TimePicker.windowUrl ("/p/" <> pid.toText <> "/rum") [("tab", tabParam tab)] window
    a_
      ( [ href_ url
        , class_ $ "tab h-auto! whitespace-nowrap" <> bool "" " tab-active text-textStrong" (tab == active)
        , term "aria-current" $ bool "false" "page" (tab == active)
        ]
          <> navTabAttrs
      )
      $ toHtml
      $ tabLabel tab


rumActions_ :: Projects.ProjectId -> TimePicker.TimeWindow -> Html ()
rumActions_ pid window = div_ [class_ "inline-flex items-center gap-2", data_ "default-window" "24H"] do
  a_ [href_ $ "/p/" <> pid.toText <> "/rum/dashboard", class_ "btn btn-sm gap-1.5 max-md:hidden"] do
    faSprite_ "chart-line" "regular" "h-3.5 w-3.5"
    "Open RUM dashboard"
  TimePicker.timepicker_ Nothing window.currentRange Nothing
  TimePicker.refreshButton_


instance ToHtml RumData where
  toHtml = toHtmlRaw . rumPage_
  toHtmlRaw = toHtml


rumPage_ :: RumData -> Html ()
rumPage_ page = main_ [id_ "rum-page", class_ "min-h-full bg-bgSunken"] do
  unless (null page.degradedPanels) $ degradedBanner_ page.degradedPanels
  if hasRumData page
    then case page.tab of
      Overview -> overview_ page
      Sessions -> sessions_ page
      Performance -> performance_ page
    else rumEmptyState_ page.pid


hasRumData :: RumData -> Bool
hasRumData page = page.summary.sessions > 0 || page.summary.pageViews > 0 || any (isJust . (.value)) page.vitals || any (.hasReplay) page.sessions


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
      topPages_ page.pid page.pages page.window
    aside_ [class_ "min-w-0 space-y-4"] do
      vitalsPanel_ page.vitals
      recentErrors_ page.pid page.errors page.window
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


topPages_ :: Projects.ProjectId -> [RumPage] -> TimePicker.TimeWindow -> Html ()
topPages_ pid pages window = section_ [class_ "overflow-hidden rounded-lg border border-strokeWeak bg-bgBase"] do
  panelHeader_ "Top pages" "Traffic and real-user load latency" $ Just ("Explore all events", logsUrl pid "resource.telemetry.sdk.language == \"webjs\" and name startswith \"Pageview · \"" window)
  if null pages
    then panelEmpty_ "No page views in this time range"
    else div_ [class_ "overflow-x-auto"] $ table_ [class_ "table table-sm w-full"] do
      thead_ $ tr_ $ th_ "Page" >> th_ [class_ "text-right"] "Views" >> th_ [class_ "text-right"] "P75 load" >> th_ [class_ "text-right max-sm:hidden"] "Last seen"
      tbody_ $ forM_ pages \page -> tr_ do
        td_ [class_ "max-w-xl"] $ a_ [href_ $ logsUrl pid ("resource.telemetry.sdk.language == \"webjs\" and attributes.url.path == " <> kqlValue page.path) window, class_ "block truncate font-medium text-textBrand"] $ toHtml page.path
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


recentErrors_ :: Projects.ProjectId -> [RumError] -> TimePicker.TimeWindow -> Html ()
recentErrors_ pid errors window = section_ [class_ "rounded-lg border border-strokeWeak bg-bgBase"] do
  panelHeader_ "Recent browser errors" "Errors stay linked to user and session context" $ Just ("View errors", logsUrl pid "resource.telemetry.sdk.language == \"webjs\" and status_code == \"ERROR\"" window)
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
  panelHeader_ "Recent sessions" "Open a recording or inspect its correlated telemetry" $ Just ("View all sessions", sessionsUrl page.pid page.window Nothing AllSessionRows Nothing)
  sessionTable_ page.pid page.window (take 8 page.sessions) Nothing AllSessionRows


sessions_ :: RumData -> Html ()
sessions_ page = do
  let filtered = filterSessions page.query page.sessionFilter page.sessions
      selected = page.selectedSession >>= \sid -> find ((== sid) . (.id)) page.sessions
  div_ [class_ "grid min-h-[calc(100vh-7.5rem)] grid-cols-5 bg-bgBase"] do
    section_ [class_ "col-span-2 min-w-0 border-r border-strokeWeak max-xl:col-span-5 max-xl:border-b max-xl:border-r-0"] do
      sessionsToolbar_ page.pid page.window page.query page.sessionFilter (length filtered)
      sessionTable_ page.pid page.window filtered page.query page.sessionFilter
    section_ [id_ "rum-replay-workspace", class_ "col-span-3 min-w-0 bg-bgSunken max-xl:col-span-5", Aria.label_ "Session replay workspace"] $ replayWorkspace_ page.pid page.window selected


sessionsToolbar_ :: Projects.ProjectId -> TimePicker.TimeWindow -> Maybe Text -> SessionFilter -> Int -> Html ()
sessionsToolbar_ pid window query sessionFilter count = div_ [class_ "space-y-2 border-b border-strokeWeak p-3"] do
  form_ [method_ "get", action_ $ "/p/" <> pid.toText <> "/rum", class_ "flex items-center gap-2"] do
    input_ [type_ "hidden", name_ "tab", value_ "sessions"]
    TimePicker.timeHiddenInputs_ window.fromQuery window.toQuery window.sinceQuery
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
        ( [ href_ $ sessionsUrl pid window query value Nothing
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


sessionTable_ :: Projects.ProjectId -> TimePicker.TimeWindow -> [RumSession] -> Maybe Text -> SessionFilter -> Html ()
sessionTable_ pid window sessions query sessionFilter =
  if null sessions
    then panelEmpty_ $ bool "No sessions in this time range" "No sessions match this filter" (sessionFilter /= AllSessionRows)
    else div_ [class_ "overflow-x-auto"] $ table_ [class_ "table table-sm w-full"] do
      thead_ $ tr_ $ th_ "User / session" >> th_ "Last page" >> th_ [class_ "text-right"] "Signals" >> th_ [class_ "text-right"] "Duration" >> th_ [class_ "w-12"] ""
      tbody_ $ forM_ sessions \session -> tr_ [class_ "hover:bg-fillHover"] do
        td_ [class_ "max-w-64"] do
          a_ [href_ $ sessionsUrl pid window query sessionFilter (Just session.id), class_ "block truncate font-medium text-textStrong hover:text-textBrand"] $ toHtml $ sessionIdentity session
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
            then a_ [href_ $ sessionsUrl pid window query sessionFilter (Just session.id), class_ "btn btn-ghost btn-xs", Aria.label_ $ "Watch replay for " <> sessionIdentity session, data_ "tippy-content" "Watch replay"] $ faSprite_ "circle-play" "regular" "h-4 w-4"
            else a_ [href_ $ logsUrl pid ("attributes.session.id == " <> kqlValue session.id) window, class_ "btn btn-ghost btn-xs", Aria.label_ $ "Inspect telemetry for " <> sessionIdentity session, data_ "tippy-content" "Inspect telemetry"] $ faSprite_ "arrow-up-right" "regular" "h-3.5 w-3.5"


replayWorkspace_ :: Projects.ProjectId -> TimePicker.TimeWindow -> Maybe RumSession -> Html ()
replayWorkspace_ pid window = \case
  Just session | session.hasReplay -> div_ [class_ "min-h-full"] do
    header_ [class_ "flex flex-wrap items-start justify-between gap-3 border-b border-strokeWeak bg-bgBase px-4 py-3"] do
      div_ [class_ "min-w-0"] do
        h2_ [class_ "truncate text-sm font-semibold text-textStrong"] $ toHtml $ sessionIdentity session
        p_ [class_ "mt-0.5 truncate font-mono text-xs text-textWeak"] $ toHtml session.id
      a_ [href_ $ logsUrl pid ("attributes.session.id == " <> kqlValue session.id) window, class_ "btn btn-sm gap-1.5"] do
        faSprite_ "magnifying-glass-chart" "regular" "h-3.5 w-3.5"
        "Inspect telemetry"
    termRaw "session-replay" [id_ "rumSessionReplay", term "initialSession" session.id, term "consoleOpen" "true", term "fullWidth" "true", class_ "block min-h-[34rem] w-full", term "projectId" pid.toText, term "containerId" "rum-replay-workspace"] ("" :: Text)
  Just session -> replayPrompt_ "No recording for this session" "Open its correlated OpenTelemetry events to inspect navigation, network, and error spans." $ Just ("Inspect telemetry", logsUrl pid ("attributes.session.id == " <> kqlValue session.id) window)
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
    topPages_ page.pid page.pages page.window
    recentErrors_ page.pid page.errors page.window


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


-- | KQL string literal: backslashes first, then quotes, so a value can't break out of the literal.
kqlValue :: Text -> Text
kqlValue value = "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" value) <> "\""


logsUrl :: Projects.ProjectId -> Text -> TimePicker.TimeWindow -> Text
logsUrl pid query = TimePicker.windowUrl ("/p/" <> pid.toText <> "/log_explorer") [("query", toUriStr query)]


sessionsUrl :: Projects.ProjectId -> TimePicker.TimeWindow -> Maybe Text -> SessionFilter -> Maybe Text -> Text
sessionsUrl pid window query sessionFilter sessionM =
  TimePicker.windowUrl
    ("/p/" <> pid.toText <> "/rum")
    ([("tab", "sessions")] <> maybeToList (("q",) . toUriStr <$> query) <> maybeToList (("filter",) <$> sessionFilterParam sessionFilter) <> maybeToList (("session",) <$> sessionM))
    window


degradedBanner_ :: [Text] -> Html ()
degradedBanner_ panels = div_ [role_ "alert", class_ "flex items-start gap-2 border-b border-strokeWarning-strong bg-fillWarning-weak px-4 py-2.5 text-sm text-textStrong"] do
  faSprite_ "triangle-exclamation" "solid" "mt-0.5 h-4 w-4 shrink-0 text-iconWarning"
  div_ do
    strong_ "Some RUM data could not be loaded."
    span_ [class_ "ml-1 text-textWeak"] $ toHtml $ "Retry or narrow the time range. Unavailable: " <> T.intercalate ", " panels <> "."
