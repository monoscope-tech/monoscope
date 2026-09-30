-- | Real User Monitoring projects browser OpenTelemetry and session recordings into one
-- investigation surface. The dashboard template is the customizable aggregate view; this page
-- owns the user/session workflow that a dashboard cannot express.
module Pages.RealUserMonitoring (
  rumGetH,
  rumGetScopedH,
  RumGet (..),
  RumData (..),
  RumSession (..),
  RumLinks (..),
  SessionFilter (..),
  Vital (..),
  VitalRating (..),
  classifyVital,
  pageLabel,
  binnedSql,
  activitySql,
  sessionsSql,
  p75Sql,
  browserSql,
  pageViewSql,
  errorSql,
) where

import Data.Aeson qualified as AE
import Data.Cache qualified as Cache
import Data.Char (isDigit, isLetter, isLower)
import Data.Default (def)
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Data.Fixed (mod')
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Concurrent.Async (pooledForConcurrently)
import Effectful.Labeled (Labeled)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx (hxGet_, hxIndicator_, hxPushUrl_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.RUM (PageVitalPoint (..), ReplaySession (..), RumBreakdown (..), RumBucket (..), RumCacheKey (..), RumError (..), RumPage (..), RumPulse (..), RumQuery (..), RumQueryResult (..), RumSession (..), SessionFilter (..), VitalGrouping (..), VitalMeasurement (..), VitalPopulation (..), VitalTrendPoint (..), rumPanelCacheGetStale, rumPanelCacheSet)
import Models.Telemetry.RUM qualified as RUM
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, navTabAttrs)
import Pages.Components (Deferred (..), EmptyStateAction (..), EmptyStateCfg (..), EmptyStateSize (..), withDeferredBody)
import Pages.Components qualified as Components
import Pkg.Components.Table qualified as Table
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (DB, decodeEnumSC, encodeEnumSC, escapeRegex, rawSql)
import Pkg.ErrorFingerprint (normalizeMessage)
import Pkg.Parser (ScopedQuery (..), applyScopedKqlContext, mkScopedQuery)
import Pkg.QueryCache qualified as QueryCache
import Relude
import Relude.Extra.Foldable1 (maximum1, minimum1)
import System.Clock (TimeSpec (..))
import System.Config (AuthContext (..), EnvConfig (enableTimefusionReads))
import System.Logging qualified as Log
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import UnliftIO (tryAny, withRunInIO)
import Utils (classifyUserAgent, countNoun, faSprite_, getDurationNSMS, nonEmptyT, prettyTimeShort, replaceAllFormats, showFFloat', toXXHash)


data RumTab = Overview | Sessions | Performance
  deriving stock (Bounded, Enum, Eq, Read, Show)


parseTab :: Maybe Text -> RumTab
parseTab tabM = fromMaybe Overview $ decodeEnumSC @"" . toString =<< tabM


tabParam :: RumTab -> Text
tabParam = toText . encodeEnumSC @""


tabLabel :: RumTab -> Text
tabLabel = T.toTitle . tabParam


-- | Accepts both the canonical short values this page links with ("errors", "replays") and
-- the tab labels the shared Table's filter tabs put in the URL ("With errors").
parseSessionFilter :: Maybe Text -> SessionFilter
parseSessionFilter paramM = fromMaybe AllSessionRows $ find matches [minBound .. maxBound]
  where
    normalized = T.toLower <$> paramM
    matches value = normalized `elem` [sessionFilterParam value, Just $ T.toLower $ sessionFilterLabel value]


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
  , measurement :: VitalMeasurement
  , unit :: Text
  , goodAt :: Double
  , poorAt :: Double
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


-- | Field definitions acquire a measurement from the shared population.
vitalDefinitions :: [Vital]
vitalDefinitions =
  [ mk "lcp" "Largest Contentful Paint" "When the main content becomes visible" "ms" 2500 4000
  , mk "inp" "Interaction to Next Paint" "How quickly interactions produce visual feedback" "ms" 200 500
  , mk "cls" "Cumulative Layout Shift" "Visual stability while the page loads" "" 0.1 0.25
  , mk "fcp" "First Contentful Paint" "When the first content appears" "ms" 1800 3000
  , mk "ttfb" "Time to First Byte" "Server and network response before rendering" "ms" 800 1800
  ]
  where
    mk name label description unit goodAt poorAt = Vital{name, label, description, unit, goodAt, poorAt, measurement = Unmeasured}


vitalRating :: Vital -> VitalRating
vitalRating vital = case vital.measurement of
  Measured _ estimate -> case RUM.estimateRange estimate of
    Nothing -> classifyVital vital.goodAt vital.poorAt (Just $ RUM.estimateValue estimate)
    Just (lower, upper)
      | upper <= vital.goodAt -> Good
      | lower >= vital.poorAt -> Poor
      | lower >= vital.goodAt && upper <= vital.poorAt -> NeedsImprovement
      | otherwise -> Unknown
  Unmeasured -> Unknown
  Unavailable _ _ -> Unknown


-- | What every RUM read is scoped to. Bundled rather than passed as five positional
-- arguments because a filter that reached one panel and not another would render a page
-- whose own numbers disagree — the summary counting a service the table below it excludes.
data RumScope = RumScope
  { useTf :: Bool
  , queryScope :: ScopedQuery
  }


-- | Project, window, environment and service: true of any table. An absent filter matches
-- everything, so clearing one is just dropping the query parameter.
scopePredicate :: RumScope -> HI.Sql
scopePredicate scope =
  let projectId = scope.queryScope.projectId.toText
      (fromTime, toTime) = scope.queryScope.timeRange
      environment = scope.queryScope.environment
      service = scope.queryScope.service
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
browserScope scope = scopePredicate scope <> " AND " <> rawSql browserSql


-- | The predicates as text, shared by the Hasql queries and the summary widgets' SQL.
-- TimeFusion's @rum_*@ rollup measures declare these strings verbatim and serve a widget
-- only while its filter matches exactly, so an edit here must be mirrored in TimeFusion's
-- @schemas/otel_logs_and_spans.yaml@.
browserSql, pageViewSql, errorSql :: Text
browserSql =
  "(resource___telemetry___sdk___language IN ('webjs', 'javascript', 'js') OR resource___user_agent___original IS NOT NULL"
    <> " OR name IN ('documentLoad', 'documentFetch') OR "
    <> pageViewSql
    <> ")"
pageViewSql = "(name LIKE 'Pageview %' OR name = 'documentLoad')"
errorSql = "(status_code = 'ERROR' OR lower(COALESCE(level, '')) = 'error' OR attributes___exception___type IS NOT NULL)"


-- | The page a browser event was on. Our SDK sets @url.path@; the OpenTelemetry browser SDK
-- sets only @url.full@ and leaves @url.path@ null, which would otherwise collapse every row of
-- the Top Pages table onto a single blank path.
pagePath :: HI.Sql
pagePath = [HI.sql|COALESCE(NULLIF(attributes___url___path, ''), NULLIF(attributes___url___full, ''), replace(name, 'Pageview · ', ''))|]


-- | What counts as one page view. @documentLoad@ is the OpenTelemetry browser SDK's page-load
-- span; @Pageview ·@ is ours. Shared so the summary, the trend, the table and the per-session
-- counts cannot disagree about what they are counting.
pageViewPredicate :: HI.Sql
pageViewPredicate = rawSql pageViewSql


-- | What counts as a browser error. Shared for the same reason as 'pageViewPredicate': the
-- summary, the trend, the error list and the per-session counts must agree.
errorPredicate :: HI.Sql
errorPredicate = rawSql errorSql


-- | Presence plus the two headline numbers that have no sparkline, from one scan of the
-- window: the session and P75 tiles then arrive with the panel (and share its cache) instead
-- of each spinning through its own fetch. The page-view and error tiles keep fetching: their
-- sparkline series is the same request as their number. @HAVING@ turns an empty window into
-- no row, so presence is the row's existence.
rumPulse :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> Eff es (Maybe RumPulse)
rumPulse scope =
  Hasql.withHasqlTimefusion scope.useTf
    $ Hasql.interpOne
    $ [HI.sql|SELECT COUNT(DISTINCT NULLIF(attributes___session___id, ''))::bigint,
        (approx_percentile(0.75, percentile_agg(CASE WHEN |]
    <> pageViewPredicate
    <> [HI.sql| THEN duration END)) / 1000000.0)::float8
      FROM otel_logs_and_spans WHERE |]
    <> browserScope scope
    <> [HI.sql| HAVING COUNT(*) > 0|]


rumPages :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> Eff es [RumPage]
rumPages scope =
  Hasql.withHasqlTimefusion scope.useTf
    $ Hasql.interpForTimefusion
      scope.useTf
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
          -- No 'browserScope' here: both page-view markers are already disjuncts of it, so
          -- @browserScope AND pageView@ is just @pageView@ — and the bare two-name predicate
          -- is ~25x more selective than the four-branch OR ladder (0.13% vs 3.3% of the
          -- window on the demo project), which measured 1.3-2.3x faster over 24h
          -- (scripts/local/rum-perf-goal-candidates.sh).
          <> scopePredicate scope
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
    $ Hasql.interpForTimefusion
      scope.useTf
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


-- Text is matched against complete sessions before LIMIT, preserving their event totals.
-- That path stays raw because page text participates in HAVING. The common list,
-- error list and exact-id reads use a mergeable core, then enrich only the at-most
-- 200 selected ids with page and user-agent expressions.
otelSessionRows :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> SessionMatch -> SessionFilter -> Eff es [RumSession]
otelSessionRows scope match@(SessionText (Just _)) sessionFilter = otelSessionRowsRaw scope match sessionFilter
otelSessionRows scope match sessionFilter = otelSessionCoreRows scope match sessionFilter >>= enrichSessionRows scope


otelSessionRowsRaw :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> SessionMatch -> SessionFilter -> Eff es [RumSession]
otelSessionRowsRaw scope match sessionFilter =
  Hasql.withHasqlTimefusion scope.useTf
    $ Hasql.interpForTimefusion
      scope.useTf
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
          (ARRAY_AGG(|]
          <> pagePath
          <> [HI.sql| ORDER BY timestamp DESC, id DESC) FILTER (WHERE |]
          <> pageViewPredicate
          <> [HI.sql|))[1],
          MAX(COALESCE(NULLIF(attributes___user_agent___original, ''), resource___user_agent___original)),
          false
        FROM otel_logs_and_spans
        WHERE |]
          <> browserScope scope
          <> [HI.sql| AND attributes___session___id IS NOT NULL AND attributes___session___id <> ''
        GROUP BY attributes___session___id HAVING |]
          <> sessionMatchPredicate match
          <> (if sessionFilter == ErrorSessionRows then [HI.sql| AND COUNT(*) FILTER (WHERE |] <> errorPredicate <> [HI.sql|) > 0|] else mempty)
          <> [HI.sql| ORDER BY MAX(timestamp) DESC LIMIT 200|]
      )


otelSessionCoreRows :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> SessionMatch -> SessionFilter -> Eff es [RumSession]
otelSessionCoreRows scope match sessionFilter =
  Hasql.withHasqlTimefusion scope.useTf
    $ Hasql.interpForTimefusion
      scope.useTf
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
          NULL::text AS last_page, NULL::text AS user_agent, false AS has_replay
        FROM otel_logs_and_spans
        WHERE |]
          <> browserScope scope
          <> [HI.sql| AND attributes___session___id IS NOT NULL AND attributes___session___id <> ''|]
          <> sessionCoreWhere match
          <> [HI.sql|
        GROUP BY attributes___session___id HAVING true|]
          <> (if sessionFilter == ErrorSessionRows then [HI.sql| AND COUNT(*) FILTER (WHERE |] <> errorPredicate <> [HI.sql|) > 0|] else mempty)
          <> [HI.sql| ORDER BY MAX(timestamp) DESC LIMIT 200|]
      )


sessionCoreWhere :: SessionMatch -> HI.Sql
sessionCoreWhere (SessionIds ids) = [HI.sql| AND attributes___session___id = ANY(#{ids})|]
sessionCoreWhere (SessionText Nothing) = mempty
sessionCoreWhere (SessionText (Just _)) = error "text search must use the raw session query"


-- | Every follow-up read of sessions already in hand scans only their own span. The newest
-- 200 sessions of a 24h window cover ~11h on a busy project, and the enrichment scan costs
-- 1.7s over that span against 9.7s over the day (scripts/local/rum-sessions-2026-09-29.md).
--
-- >>> import Data.Time (UTCTime (..))
-- >>> let day = UTCTime (toEnum 60000) 0
-- >>> let scope = RumScope True (mkScopedQuery Projects.demoProjectId (Just day, Just (addUTCTime 86400 day)) Nothing Nothing)
-- >>> (spanScope 0 [(addUTCTime 100 day, addUTCTime 200 day), (addUTCTime 50 day, addUTCTime 150 day)] scope).queryScope.timeRange == (Just (addUTCTime 50 day), Just (addUTCTime 200 day))
-- True
-- >>> (spanScope 900 [] scope).queryScope.timeRange == scope.queryScope.timeRange
-- True
-- >>> (spanScope 900 [(day, addUTCTime 86400 day)] scope).queryScope.timeRange == scope.queryScope.timeRange
-- True
spanScope :: NominalDiffTime -> [(UTCTime, UTCTime)] -> RumScope -> RumScope
spanScope pad spans scope = case nonEmpty spans of
  Nothing -> scope
  Just found ->
    let (from, to) = scope.queryScope.timeRange
        clamp bound f x = maybe x (f x) bound
     in scope{queryScope = scope.queryScope{timeRange = (Just $ clamp from max $ addUTCTime (-pad) $ minimum1 $ fst <$> found, Just $ clamp to min $ addUTCTime pad $ maximum1 $ snd <$> found)}}


enrichSessionRows :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> [RumSession] -> Eff es [RumSession]
enrichSessionRows _ [] = pure []
enrichSessionRows scope rows = do
  let ids = map (.id) rows
  details :: [(Text, Maybe Text, Maybe Text)] <-
    Hasql.withHasqlTimefusion scope.useTf
      $ Hasql.interpForTimefusion
        scope.useTf
        ( [HI.sql|
          SELECT attributes___session___id,
            (ARRAY_AGG(|]
            <> pagePath
            <> [HI.sql| ORDER BY timestamp DESC, id DESC) FILTER (WHERE |]
            <> pageViewPredicate
            <> [HI.sql|))[1],
            MAX(COALESCE(NULLIF(attributes___user_agent___original, ''), resource___user_agent___original))
          FROM otel_logs_and_spans
          WHERE |]
            <> browserScope (spanScope 0 [(row.startedAt, row.endedAt) | row <- rows] scope)
            <> [HI.sql| AND attributes___session___id = ANY(#{ids})
          GROUP BY attributes___session___id|]
        )
  let byId = M.fromList [(sid, (lastPage, userAgent)) | (sid, lastPage, userAgent) <- details]
  pure
    [ maybe row (\(lastPage, userAgent) -> row{lastPage, userAgent}) $ M.lookup row.id byId
    | row <- rows
    ]


-- The same lookup mode is used for both stores so recordings can enrich matched
-- telemetry, and recording-only identities can discover their associated spans.
data SessionMatch = SessionText (Maybe Text) | SessionIds [Text]


searchPattern :: Text -> Text
searchPattern query = "%" <> T.replace "_" "\\_" (T.replace "%" "\\%" (T.replace "\\" "\\\\" query)) <> "%"


sessionMatchPredicate :: SessionMatch -> HI.Sql
sessionMatchPredicate (SessionIds ids) = [HI.sql| attributes___session___id = ANY(#{ids}) |]
sessionMatchPredicate (SessionText Nothing) = [HI.sql| true |]
sessionMatchPredicate (SessionText (Just query)) =
  let needle = searchPattern query
   in [HI.sql| COUNT(*) FILTER (WHERE
          attributes___session___id ILIKE #{needle}
          OR attributes___user___id ILIKE #{needle}
          OR attributes___user___full_name ILIKE #{needle}
          OR attributes___user___email ILIKE #{needle}
          OR resource___service___name ILIKE #{needle}
          OR (|]
        <> pagePath
        <> [HI.sql|) ILIKE #{needle}) > 0 |]


-- | Traffic per user agent string, busiest first. Classification into browser, OS and
-- device happens in 'classifyUserAgent': the store only groups, so a new browser release
-- needs no query change to show up.
rumBreakdown :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> Eff es [RumBreakdown]
rumBreakdown scope =
  Hasql.withHasqlTimefusion scope.useTf
    $ Hasql.interpForTimefusion
      scope.useTf
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


replaySessionRows :: DB es => RumScope -> SessionMatch -> Eff es [ReplaySession]
replaySessionRows scope match =
  let projectId = scope.queryScope.projectId
      (fromTime, toTime) = scope.queryScope.timeRange
   in Hasql.interp
        ( [HI.sql|
          SELECT session_id, created_at, last_event_at, user_id, user_name, user_email
          FROM projects.replay_sessions
          WHERE project_id = #{projectId} AND last_event_at >= #{fromTime} AND created_at <= #{toTime}
            AND (event_file_count > 0 OR cardinality(file_keys) > 0 OR cardinality(shard_keys) > 0)
          AND |]
            <> replayMatchPredicate match
            <> [HI.sql|
          ORDER BY last_event_at DESC LIMIT 200
        |]
        )


replayMatchPredicate :: SessionMatch -> HI.Sql
replayMatchPredicate (SessionIds ids) = [HI.sql| CAST(session_id AS TEXT) = ANY(#{ids}) |]
replayMatchPredicate (SessionText Nothing) = [HI.sql| true |]
replayMatchPredicate (SessionText (Just query)) =
  let needle = searchPattern query
   in [HI.sql| (CAST(session_id AS TEXT) ILIKE #{needle}
        OR user_id ILIKE #{needle} OR user_name ILIKE #{needle} OR user_email ILIKE #{needle}) |]


searchSessions :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> Maybe Text -> SessionFilter -> Eff es [RumSession]
searchSessions scope query sessionFilter = do
  spans <- otelSessionRows scope (SessionText query) sessionFilter
  recordings <- if sessionFilter == ErrorSessionRows then pure [] else replaySessionRows scope (SessionText query)
  let recordingIds = map (UUID.toText . (.id)) recordings
      spanIds = map (.id) spans
      -- Cross-enrich by exact id: filtering each store independently otherwise turns a
      -- page match into "No replay", or a recording-name match into "No telemetry".
      -- Only ids the other store did NOT already return need the extra look-up — a session
      -- both searches found carries complete aggregates already (the text predicate is a
      -- HAVING filter, not a row filter). On an unfiltered load the sets overlap almost
      -- entirely, which skips a second full-window scan of the span table.
      newRecordings = filter ((`notElem` spanIds) . UUID.toText . (.id)) recordings
      newRecordingIds = map (UUID.toText . (.id)) newRecordings
      newSpanIds = filter (`notElem` recordingIds) spanIds
  -- A recording's spans sit inside it, give or take the flush lag either side. Only a text
  -- search pays for this: on the plain list a recording missing from the newest 200 span
  -- sessions almost always has no browser spans at all, and the id lookup across the
  -- recordings' combined span (most of the window) cost 34s on TimeFusion for that nothing.
  extraSpans <- if null newRecordings || isNothing query then pure [] else otelSessionRows (spanScope recordingPad [(r.startedAt, r.endedAt) | r <- newRecordings] scope) (SessionIds newRecordingIds) sessionFilter
  extraRecordings <- if null newSpanIds then pure [] else replaySessionRows scope (SessionIds newSpanIds)
  pure $ take 200 $ filterSessions sessionFilter $ mergeSessions scope.queryScope (extraSpans <> spans) (recordings <> extraRecordings)


sessionDetail :: (DB es, Labeled "timefusion" Hasql :> es) => RumScope -> Text -> Eff es (Maybe RumSession)
sessionDetail scope sid = do
  recordings <- replaySessionRows scope (SessionIds [sid])
  spans <- otelSessionRows (spanScope recordingPad [(r.startedAt, r.endedAt) | r <- recordings] scope) (SessionIds [sid]) AllSessionRows
  pure $ listToMaybe $ mergeSessions scope.queryScope spans recordings


recordingPad :: NominalDiffTime
recordingPad = 15 * 60


-- | Recordings carry no service or environment attribution. Under either scope they
-- may enrich matching telemetry, but cannot introduce recording-only sessions.
mergeSessions :: ScopedQuery -> [RumSession] -> [ReplaySession] -> [RumSession]
mergeSessions scope otel replays = sortWith (Down . (.endedAt)) $ M.elems $ foldl' addReplay (M.fromList [(s.id, s) | s <- otel]) replays
  where
    addReplay sessions replay =
      let sid = UUID.toText replay.id
       in if isJust scope.service || isJust scope.environment
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
        , userAgent = Nothing
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
  { queryScope :: ScopedQuery
  , window :: TimePicker.TimeWindow
  }


-- | One independently-loaded region of the page. Each panel is a separate scan of the
-- window, costing 0.5–4s on its own, and the panels together contend badly enough that six
-- concurrently take 10–28s. Loading them as one unit made the whole page wait for the
-- slowest; each panel now fetches itself, so a panel appears as soon as /its/ query lands.
data RumPanel = PanelPulse | PanelPages | PanelVitals | PanelVitalTrend | PanelErrors | PanelSessions | PanelSessionDetail | PanelAudience
  deriving stock (Eq, Read, Show)


panelParam :: RumPanel -> Text
panelParam = toText . encodeEnumSC @"Panel"


-- | Ids are the swap contract: 'deferredShell_' selects @#id@ out of the panel response, so
-- the shell and the rendered panel must agree on it.
panelId :: RumPanel -> Text
panelId PanelSessionDetail = "rum-replay-workspace"
panelId panel = "rum-panel-" <> panelParam panel


parsePanel :: Text -> Maybe RumPanel
parsePanel = decodeEnumSC @"Panel" . toString


data RumData = RumData
  { links :: RumLinks
  , tab :: RumTab
  , panel :: Maybe RumPanel
  -- ^ Which panel this response carries. 'Nothing' is the page skeleton: every panel
  -- renders as a shell that fetches itself.
  , servedStale :: Bool
  -- ^ At least one panel query was served from the stale band of the shared cache. The
  -- panel re-fetches itself with @refresh@ so the viewer always converges on fresh data
  -- without ever waiting on a cold scan for first paint.
  , now :: UTCTime
  , pulse :: Maybe RumPulse
  , pages :: [RumPage]
  , errors :: [RumError]
  , sessions :: [RumSession]
  , vitals :: [Vital]
  , vitalTrend :: [VitalTrendPoint]
  , pageVitals :: [PageVitalPoint]
  , breakdown :: [RumBreakdown]
  , query :: Maybe Text
  , sessionFilter :: SessionFilter
  , selectedSession :: Maybe Text
  , selectedSessionData :: Maybe RumSession
  , degradedPanels :: [RumQuery]
  }


newtype RumGet = RumGet (PageCtx (Deferred RumData))


instance ToHtml RumGet where
  toHtml (RumGet page@(PageCtx _ body)) = case body of
    DeferredBody loaded -> if isJust loaded.panel then toHtml loaded else toHtml page
    DeferredShell{} -> toHtml page
  toHtmlRaw = toHtml


data RumCachePolicy = SkipCache | CachePopulated | CacheEmptySearch
  deriving stock (Eq)


-- | Every RUM panel is a separate scan of a 24-hour window, and a tab click re-runs all of
-- them. Holding the page chrome hostage to the slowest one is what makes switching tabs feel
-- broken, so the first request renders the tab strip, time picker and a skeleton, and the
-- panels arrive on the request the skeleton fires.
rumSkeleton_ :: RumTab -> Html ()
rumSkeleton_ Sessions = div_ [class_ "flex flex-col bg-bgBase xl:h-full xl:min-h-0"] do
  div_ [class_ "flex shrink-0 items-center border-b border-strokeWeak px-4 py-1 max-md:px-3"]
    $ div_ [class_ "h-8 w-full max-w-[22rem] rounded-lg skeleton-shimmer"] ""
  sessionsSkeleton_
rumSkeleton_ _ = div_ [class_ "min-h-full space-y-5 bg-bgBase p-4", role_ "status", Aria.label_ "Loading real user monitoring"] do
  div_ [class_ "grid grid-cols-4 gap-px border-y border-strokeWeak bg-bgBase max-md:grid-cols-2"]
    $ replicateM_ 4
    $ div_ [class_ "flex flex-col gap-2 px-4 py-3"] do
      div_ [class_ "h-6 w-16 rounded skeleton-shimmer"] ""
      div_ [class_ "h-3 w-24 rounded skeleton-shimmer"] ""
  div_ [class_ "grid grid-cols-[minmax(0,1.65fr)_minmax(18rem,0.75fr)] gap-4 max-xl:grid-cols-1"] do
    div_ [class_ "rounded-lg border border-strokeWeak surface-raised p-4"] Components.chartSkeleton_
    div_ [class_ "rounded-lg border border-strokeWeak surface-raised"] $ Components.tableSkeleton_ 5
  div_ [class_ "rounded-lg border border-strokeWeak surface-raised"] $ Components.tableSkeleton_ 6


-- | Backwards-compatible programmatic entry point. Browser routes call
-- 'rumGetScopedH' so a shared link can name its environment, while existing internal
-- callers retain the authenticated session as their environment default.
rumGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders RumGet)
rumGetH pid tabM queryM sessionFilterM fromM toM sinceM selectedM serviceM panelM deferredM refreshM =
  rumGetScopedH pid tabM queryM sessionFilterM fromM toM sinceM selectedM serviceM panelM deferredM refreshM Nothing


-- | Route entry point with an explicit environment carried by a shared URL.
rumGetScopedH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders RumGet)
rumGetScopedH pid tabM queryM sessionFilterM fromM toM sinceM selectedM _serviceM panelM deferredM refreshM environmentM = do
  (session, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  let tab = parseTab tabM
      panel = panelM >>= parsePanel
      sessionFilter = parseSessionFilter sessionFilterM
      searchQuery = mfilter (not . T.null) $ T.strip <$> queryM
      since = sinceM <|> ("24H" <$ guard (all (isNothing . nonEmptyT) [fromM, toM]))
      window = TimePicker.mkTimeWindow now fromM toM since
      -- A shared investigation link names its environment explicitly. The sticky selection
      -- remains the default for a hand-entered RUM URL, but must not silently rewrite a link
      -- that an on-call engineer opened from an alert or issue.
      environment = mfilter (not . T.null) environmentM <|> session.environment
      serviceFilter = session.service
      scope = RumScope{useTf = appCtx.env.enableTimefusionReads, queryScope = mkScopedQuery pid (Just window.fromTime, Just window.toTime) environment serviceFilter}
      links = RumLinks{queryScope = scope.queryScope, window}
      bucket
        | diffUTCTime window.toTime window.fromTime <= 6 * 3600 = FiveMinutes
        | diffUTCTime window.toTime window.fromTime <= 3 * 86400 = OneHour
        | otherwise = SixHours
      cacheKey query = RumCacheKey pid query environment serviceFilter (nonEmptyT fromM) (nonEmptyT toM) window.sinceQuery
      panelTtl = TimePicker.cacheTtl window
      pulseQ = (cacheKey PresenceQuery, panelTtl, PulseResult <$> rumPulse scope, Nothing)
      pagesQ = (cacheKey PagesQuery, panelTtl, PagesResult <$> rumPages scope, Nothing)
      errorsQ = (cacheKey ErrorsQuery, panelTtl, ErrorsResult <$> rumErrors scope, Nothing)
      -- A cold list over a wide window paints the newest three hours first — a 24h scan of the
      -- session shape is 7-24s on a busy project against 0.2s for 3h
      -- (scripts/local/rum-sessions-2026-09-29.md) — and the stale-serve revalidation then
      -- fetches the full range. Only the plain newest-first list qualifies: a text search or a
      -- filter must see the whole window or it silently answers "no match".
      recent = spanScope 0 [(addUTCTime (-(3 * 3600)) window.toTime, window.toTime)] scope <$ guard (diffUTCTime window.toTime window.fromTime > 6 * 3600)
      -- The Overview's recent sessions are the Sessions tab's unfiltered list: one cache entry, one scan.
      searchAt s = SessionsResult <$> searchSessions s searchQuery sessionFilter
      sessionSearchQ = (cacheKey $ SessionSearchQuery searchQuery sessionFilter, panelTtl, searchAt scope, searchAt <$> (guard (isNothing searchQuery && sessionFilter == AllSessionRows) *> recent))
      sessionDetailQs = [(cacheKey $ SessionDetailQuery sid, panelTtl, SessionDetailResult <$> sessionDetail scope sid, Nothing) | sid <- maybeToList selectedM, not $ T.null sid]
      vitalsQ = (cacheKey $ VitalPopulationQuery bucket, panelTtl, VitalPopulationResult <$> RUM.rumVitalPopulation scope.useTf scope.queryScope window bucket, Nothing)
      breakdownQ = (cacheKey BreakdownQuery, panelTtl, BreakdownResult <$> rumBreakdown scope, Nothing)
      -- Only the requested panel's queries run. The skeleton request runs none at all, so the
      -- page chrome is free and each panel pays only for itself.
      --
      -- These all used to run together on one request. Measured on the demo project over 24h
      -- (scripts/local/rum-perf-2026-08-30.md): panels cost 0.5–4.1s individually but six
      -- concurrently take 10–28s — contention eats most of the parallelism, and the page still
      -- waited for the slowest. Per-panel fetching trades that for a first paint at the cost
      -- of one panel.
      panelQueries = case panel of
        Just PanelPulse -> [pulseQ]
        Just PanelPages -> [pagesQ]
        Just PanelVitals -> [vitalsQ]
        Just PanelVitalTrend -> [vitalsQ]
        Just PanelErrors -> [errorsQ]
        Just PanelSessions -> sessionSearchQ : sessionDetailQs
        Just PanelSessionDetail -> sessionDetailQs
        Just PanelAudience -> [breakdownQ]
        Nothing -> []
      -- Concurrent requests share lookup, computation and publication within a replica.
      -- Completed entries are shared across replicas through rum_panel_cache.
      --
      -- The shared layer also serves STALE populated entries (past expiry, inside the prune horizon):
      -- an expired panel answers instantly with its last-known data and the page re-fetches
      -- itself with @refresh@, which bypasses the stale band and recomputes. First paint
      -- stops waiting on a cold scan after the very first visit. Stale payloads are never
      -- copied into the memory cache — its TTL would re-classify them as fresh and suppress
      -- the revalidation they exist to trigger.
      --
      -- A failed recompute falls back to populated stale data rather than blanking the panel:
      -- last-known data plus no revalidation trigger, so the page neither lies forever nor
      -- collapses to zero states while the store is down. A payload from an older code
      -- shape fails to decode; that is a cache miss, not an error.
      runQuery (key, ttl, action, quick) = withRunInIO \unlift -> fmap snd $ QueryCache.coalesceQuery appCtx.rumQueryFlights (key, refresh) $ unlift do
        -- First SDK visits remain uncached; an explicit missing search gets a brief fresh-only hit.
        let label = rumQueryLabel key.query
            policy result
              | SessionSearchQuery (Just _) _ <- key.query, SessionsResult [] <- result = CacheEmptySearch
              | populated = CachePopulated
              | otherwise = SkipCache
              where
                populated = case result of
                  PulseResult pulse -> isJust pulse
                  PagesResult rows -> not $ null rows
                  ErrorsResult rows -> not $ null rows
                  SessionsResult rows -> not $ null rows
                  SessionDetailResult row -> isJust row
                  VitalPopulationResult rows -> not $ null rows
                  BreakdownResult rows -> not $ null rows
            usable (value, stale) = policy value /= CacheEmptySearch || not (stale || refresh)
        l1 <- liftIO $ Cache.lookup appCtx.rumCache key
        case l1 of
          Just hit | usable (hit, False) -> pure $ Right (hit, False)
          _ -> do
            let dbKey =
                  toXXHash $ show key <> case key.query of
                    -- Older replicas would promote brief empty hits with the full positive TTL.
                    SessionSearchQuery (Just _) _ -> ":attributed-recordings-v2:negative-search-v1"
                    SessionSearchQuery{} -> ":attributed-recordings-v2"
                    SessionDetailQuery{} -> ":attributed-recordings-v2"
                    PresenceQuery -> ":pulse-v1"
                    PagesQuery -> ""
                    ErrorsQuery -> ""
                    VitalPopulationQuery{} -> ":vital-population-v1"
                    BreakdownQuery -> ""
            staleEntryM <- mfilter usable . fromRight Nothing <$> tryAny (rumPanelCacheGetStale dbKey)
            outcome <-
              tryAny
                ( case staleEntryM of
                    Just (shared, False) -> do
                      -- Shared empty searches keep their database expiry rather than extend it in L1.
                      when (policy shared /= CacheEmptySearch) $ liftIO $ Cache.insert' appCtx.rumCache (Just ttl) key shared
                      pure (shared, False)
                    Just (shared, True) | not refresh -> pure (shared, True)
                    -- Never cached: a cached slice would be served as the final answer whenever the
                    -- full revalidation fails. The revalidation computes and caches the full window.
                    Nothing | Just partial <- quick, not refresh -> (,True) <$> partial
                    _ -> do
                      fresh <- action
                      when (policy fresh /= SkipCache) do
                        let lifetime = if policy fresh == CacheEmptySearch then min ttl (TimeSpec 30 0) else ttl
                        liftIO $ Cache.insert' appCtx.rumCache (Just lifetime) key fresh
                        either (\err -> Log.logAttention "RUM panel cache write failed" (label, displayException err)) pure =<< tryAny (rumPanelCacheSet dbKey lifetime.sec fresh)
                      pure (fresh, False)
                )
            case outcome of
              Right res -> pure $ Right res
              Left err -> case staleEntryM of
                Just (stale, _) -> Right (stale, False) <$ Log.logAttention "RUM panel query failed; served stale cache" (label, displayException err)
                Nothing -> Left key.query <$ Log.logAttention "RUM panel query failed" (label, displayException err)
      refresh = isJust refreshM
      deferredUrl = rumUrl links ([(key, value) | (key, Just value) <- [("tab", tabM), ("q", queryM), ("filter", sessionFilterM), ("session", selectedM)]] <> [("deferred", "1")])
  body <- withDeferredBody deferredM "rum-page" deferredUrl (rumSkeleton_ tab) do
    outcomes <- pooledForConcurrently panelQueries runQuery
    let (degradedPanels, served) = partitionEithers outcomes
        results = map fst served
        servedStale = any snd served
        pulse = asum [value | PulseResult value <- results]
        pages = fold [value | PagesResult value <- results]
        errors = fold [value | ErrorsResult value <- results]
        sessions = fold [value | SessionsResult value <- results]
        selectedSessionData = asum [value | SessionDetailResult value <- results]
        populations = fold [value | VitalPopulationResult value <- results]
        fieldMeasurements = M.fromList [(population.metricName, population.measurement) | population <- populations, population.grouping == FieldVital]
        vitals = [(vital :: Vital){measurement = M.findWithDefault Unmeasured vital.name fieldMeasurements} | vital <- vitalDefinitions]
        vitalTrend = [VitalTrendPoint time population.metricName population.measurement | population <- populations, TrendVital time <- [population.grouping]]
        pageVitals = [PageVitalPoint url population.metricName population.measurement | population <- populations, PageVital url <- [population.grouping]]
        breakdown = fold [value | BreakdownResult value <- results]
    pure RumData{links, tab, panel, servedStale, now, pulse, pages, errors, sessions, vitals, vitalTrend, pageVitals, breakdown, query = queryM, sessionFilter, selectedSession = selectedM, selectedSessionData, degradedPanels}
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
rumNavTabs_ links active = nav_ [class_ "tabs tabs-box tabs-outline flex-nowrap items-center", Aria.label_ "Real User Monitoring views", term "hx-preload" "mouseover"] do
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
  a_ [href_ $ "/p/" <> links.queryScope.projectId.toText <> "/rum/dashboard", class_ "btn btn-sm gap-1.5 max-md:hidden"] do
    faSprite_ "chart-line" "regular" "h-3.5 w-3.5"
    "Open RUM dashboard"
  TimePicker.liveDataControls_ Nothing links.window.currentRange Nothing TimePicker.RefreshOnly


instance ToHtml RumData where
  toHtml = toHtmlRaw . rumPage_
  toHtmlRaw = toHtml


-- | A panel slot. On the skeleton response every slot is a shell that fetches only its own
-- panel; on a panel response the matching slot carries content and the rest stay shells —
-- @hx-select@ discards them, so rendering the surrounding layout costs nothing.
--
-- Stale-served content re-fetches itself once, with @refresh@, shortly after landing: the
-- viewer sees last-known data instantly and the fresh result swaps in when the recompute
-- lands. The refreshed response is fresh by construction (refresh bypasses the stale band),
-- so it carries no trigger of its own and the cycle always terminates.
--
-- Rendered content also re-fetches itself on the time transport's live tick, which is what
-- makes the LIVE badge tell the truth on this page: the panels hold every number on it, and
-- nothing else here was listening for the tick.
slot_ :: RumData -> RumPanel -> Html () -> Html () -> Html ()
slot_ page panel skeleton content
  | page.panel == Just panel = div_ ([id_ $ panelId panel, class_ $ "w-full" <> if panel == PanelSessions then " flex-1 min-h-0" else ""] <> liveAttrs) do
      if sessionList || null page.degradedPanels then content else degradedBanner_ page panel
      unless sessionList $ panelRevalidation_ page panel
  | otherwise =
      div_
        ([id_ $ panelId panel, class_ "w-full", data_ "deferred-shell" ""] <> panelSwapAttrs page panel False "load, update-query[event.detail?.source!='auto-refresh'] from:window" (Just "replace"))
        skeleton
  where
    -- One panel's worth of work per tick, swapped in place: the page chrome, the scroll
    -- position and any open replay stay exactly as they were, and a tick that lands inside
    -- the panel's cache TTL costs a cache read.
    -- Automatic ticks leave in-flight work running; manual changes still replace it.
    -- Populated summary widgets refresh themselves; replacing their parent resets loaded values.
    liveAttrs
      | panel == PanelPulse && isJust page.pulse = []
      | otherwise = panelSwapAttrs page panel False "update-query[event.detail?.source!='auto-refresh'||!this.matches('.htmx-request,:has(.htmx-request),#rum-page:has(#rum-session-search-form.htmx-request) #rum-panel-sessions')] from:window" (Just "replace")
    -- On the Sessions tab the panel also carries the replay workspace; swapping the whole
    -- panel would restart a replay the viewer just opened. Only the list is re-fetched
    -- there — a selection made while the refresh is in flight survives.
    sessionList = page.tab == Sessions && panel == PanelSessions


panelRevalidation_ :: RumData -> RumPanel -> Html ()
panelRevalidation_ page panel =
  -- A populated pulse is four widgets that fetch and refresh themselves; re-rendering the
  -- panel around them only restarts them, which is the spinner flash on first load.
  when (page.servedStale && not (panel == PanelPulse && isJust page.pulse)) $ div_ (class_ "hidden" : panelSwapAttrs page panel True "load delay:600ms" ("abort" <$ guard (page.tab == Sessions && panel == PanelSessions || panel == PanelSessionDetail))) mempty


-- | A rendered session list syncs with the search form that also replaces it; any other
-- panel syncs with itself.
panelSwapAttrs :: RumData -> RumPanel -> Bool -> Text -> Maybe Text -> [Attribute]
panelSwapAttrs page panel refresh trigger syncMode =
  -- Rendered content morphs, so a live tick patches numbers into the table the viewer is
  -- reading instead of rebuilding it under their pointer; only the skeleton is replaced.
  [hxGet_ url, hxTrigger_ trigger, hxTarget_ refreshTarget, hxSelect_ refreshTarget, hxSwap_ $ if isJust page.panel then "outerMorph" else "outerHTML"]
    <> [term "hx-sync" $ bool ("#" <> panelId panel) "#rum-session-search-form" sessionList <> ":" <> mode | mode <- toList syncMode]
    <> [term "hx-include" "#rum-session-search-form" | sessionList]
    <> [Components.timeWindowVals_ "24H", term "hx-preload" "false"]
  where
    sessionList = page.panel == Just panel && page.tab == Sessions && panel == PanelSessions
    refreshTarget = if sessionList then "#rum-sessions-list" else "#" <> panelId panel
    url =
      rumUrl page.links
        $ [("tab", tabParam page.tab), ("panel", panelParam panel), ("deferred", "1")]
        <> [(key, value) | (key, Just value) <- [("q", page.query), ("filter", sessionFilterParam page.sessionFilter), ("session", page.selectedSession)]]
        <> [("refresh", "1") | refresh]


rumPage_ :: RumData -> Html ()
rumPage_ page | page.panel == Just PanelSessionDetail = sessionWorkspace_ page
rumPage_ page = div_ [id_ "rum-page", class_ $ "bg-bgBase " <> if page.tab == Sessions then "flex flex-col xl:h-full xl:min-h-0 [&>#rum-panel-sessions]:flex-1 [&>#rum-panel-sessions]:min-h-0" else "min-h-full"] do
  when (page.tab == Sessions) $ div_ [class_ "flex shrink-0 items-center border-b border-strokeWeak px-4 py-1 max-md:px-3"] $ sessionSearch_ page
  case page.tab of
    Overview -> overview_ page
    Sessions -> sessions_ page
    Performance -> performance_ page


-- | Keep search outside the scrolling list. HTMX replaces only the list, so typing
-- never interrupts the recording. The GET form also works without JavaScript.
sessionSearch_ :: RumData -> Html ()
sessionSearch_ page = form_
  [ id_ "rum-session-search-form"
  , method_ "get"
  , action_ route
  , hxGet_ route
  , hxTrigger_ "input delay:300ms, submit"
  , hxTarget_ "#rum-sessions-list"
  , hxSelect_ "#rum-sessions-list"
  , hxSwap_ "outerMorph"
  , term "hx-sync" "this:replace"
  , term "hx-on::before:request" "event.detail.ctx.replace = this.action + '?' + new URLSearchParams(new FormData(this))"
  , term "hx-vals" "{\"panel\":\"sessions\",\"deferred\":\"1\"}"
  , class_ "flex min-w-0 flex-[1_1_22rem] items-center gap-2"
  ]
  do
    input_ [type_ "hidden", name_ "tab", value_ "sessions"]
    input_ [type_ "hidden", name_ "session", value_ $ fromMaybe "" page.selectedSession]
    TimePicker.timeHiddenInputs_ page.links.window.fromQuery page.links.window.toQuery page.links.window.sinceQuery
    forM_ page.links.queryScope.environment $ \environment -> input_ [type_ "hidden", name_ "environment", value_ environment]
    forM_ page.links.queryScope.service $ \service -> input_ [type_ "hidden", name_ "service_scope", value_ service]
    input_ [id_ "rum-session-filter", type_ "hidden", name_ "filter", value_ $ fromMaybe "" $ sessionFilterParam page.sessionFilter]
    label_ [class_ "input input-sm flex min-w-0 flex-1 items-center gap-2 border-strokeWeak bg-bgBase shadow-none max-sm:h-11"] do
      faSprite_ "magnifying-glass" "regular" "h-4 w-4 shrink-0 text-textWeak"
      input_
        [ id_ "rum-session-search"
        , type_ "search"
        , name_ "q"
        , value_ $ fromMaybe "" page.query
        , placeholder_ "User, session, page, or service"
        , Aria.label_ "Search sessions"
        , term "aria-keyshortcuts" "/"
        , class_ "min-w-0 grow"
        , onkeydown_ "if (event.key === 'Escape') { event.preventDefault(); event.stopPropagation(); this.blur(); }"
        , term "_" "on keydown[key == '/' and not ctrlKey and not metaKey and not altKey and not (the event's target matches <input, textarea, select, [contenteditable]/>) and no <dialog[open]/> and no <[popover]:popover-open/>] from window halt the event then call me.focus() end"
        ]
      span_ [class_ "flex shrink-0 items-center gap-1 text-xs text-textWeak max-sm:hidden"] do
        kbd_ [class_ "kbd kbd-xs"] "/"
        "to focus"
  where
    route = "/p/" <> page.links.queryScope.projectId.toText <> "/rum"


-- | A service filter that matched nothing is not an uninstrumented project. The unscoped
-- empty state pitches installing the browser SDK, which here would tell a user with working
-- telemetry to re-instrument a working app. The way out is to widen the scope, so that is
-- what this offers.
scopedEmptyState_ :: RumLinks -> Text -> Html ()
scopedEmptyState_ _links name =
  div_ [class_ "mx-auto flex min-h-[40vh] max-w-2xl flex-col justify-center px-6 py-12"]
    $ Components.emptyState_
      def{icon = Just "web", action = ESNone}
      ("No browser telemetry for " <> name <> " in this range")
      "This service reported no page views, errors or Web Vitals in the selected window. Widen the time range, or choose another service from the global scope picker."


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
overview_ page = div_ [class_ "space-y-2 px-4 pb-4 pt-2 max-md:px-3"] do
  slot_ page PanelPulse pulseSkeleton_ $ pulseOrEmpty_ page
  div_ [class_ "grid grid-cols-[minmax(0,1.65fr)_minmax(18rem,0.75fr)] gap-4 max-xl:grid-cols-1"] do
    div_ [class_ "min-w-0 space-y-2"] do
      slot_ page PanelPages (panelSkeleton_ $ Components.tableSkeleton_ 5) $ topPages_ page.links page.pages
      slot_ page PanelAudience (panelSkeleton_ $ Components.tableSkeleton_ 3) $ audiencePanel_ page.breakdown
    aside_ [class_ "min-w-0 space-y-2"] do
      slot_ page PanelVitals (panelSkeleton_ $ Components.tableSkeleton_ 4) $ vitalsPanel_ page.vitals
      slot_ page PanelErrors (panelSkeleton_ $ Components.tableSkeleton_ 4) $ recentErrors_ page.links page.errors
  slot_ page PanelSessions (panelSkeleton_ $ Components.tableSkeleton_ 6) $ recentSessions_ page


-- | The onboarding pitch belongs here: this is the panel that knows whether the project has
-- any browser telemetry at all. With telemetry present, the numbers and the activity chart
-- are the same Widget components dashboards are built from — they fetch their own data
-- through the chart pipeline, so they look, behave and cache exactly like every other
-- number and chart in the product.
pulseOrEmpty_ :: RumData -> Html ()
pulseOrEmpty_ page
  | Just pulse <- page.pulse = div_ [class_ "space-y-4"] do
      rumStatWidgets_ page.links pulse
      rumActivityWidget_ page.links
  | otherwise = maybe (rumEmptyState_ page.links.queryScope.projectId) (scopedEmptyState_ page.links) page.links.queryScope.service


pulseSkeleton_ :: Html ()
pulseSkeleton_ = div_ [class_ "space-y-4", role_ "status", Aria.label_ "Loading summary"] do
  div_ [class_ "grid grid-cols-4 gap-3 max-md:grid-cols-2"]
    $ replicateM_ 4
    $ div_ [class_ "flex h-28 flex-col justify-center gap-2 rounded-lg border border-strokeWeak surface-raised px-4"] do
      div_ [class_ "h-6 w-16 rounded skeleton-shimmer"] ""
      div_ [class_ "h-3 w-24 rounded skeleton-shimmer"] ""
  panelSkeleton_ Components.chartSkeleton_


panelSkeleton_ :: Html () -> Html ()
panelSkeleton_ = div_ [class_ "rounded-lg border border-strokeWeak surface-raised"]


rumStatWidgets_ :: RumLinks -> RumPulse -> Html ()
rumStatWidgets_ links pulse = div_ [class_ "grid grid-cols-4 gap-3 max-md:grid-cols-2"] do
  statSlot_
    $ served (Just $ fromIntegral pulse.sessions)
    $ statWidget "rum-stat-sessions" Widget.WTStat "Sessions" "users" "sessions" sessionsSql
  statSlot_
    -- The page-view predicate alone: its two disjuncts are already inside 'browserSql''s
    -- OR, so @browserSql AND pageView@ is just @pageView@ — the same subset collapse
    -- 'rumPages' applies, and the cheaper predicate plans measurably faster.
    $ statWidget "rum-stat-pageviews" Widget.WTTimeseriesStat "Page views" "file-lines" "views"
    $ binnedSql "count(*)" pageViewSql
  statSlot_
    $ ( statWidget "rum-stat-errors" Widget.WTTimeseriesStat "Browser errors" "triangle-exclamation" "errors"
          $ binnedSql "count(*)" (browserSql <> " AND " <> errorSql)
      )
      { Widget.seriesIntent = Just "error"
      }
  statSlot_
    $ served pulse.p75LoadMs
    $ statWidget "rum-stat-p75" Widget.WTStat "P75 page load" "gauge" "ms" p75Sql
  where
    statSlot_ = div_ [class_ "h-28 min-h-28"] . Widget.widget_
    -- A value rendered with the panel; the widget still refreshes itself on live ticks.
    served value widget = widget{Widget.dataset = Just (def :: Widget.WidgetDataset){Widget.value = value}}
    statWidget wid wType title icon unit sql =
      def
        { Widget.wType = wType
        , Widget.id = Just wid
        , Widget.title = Just title
        , Widget.icon = Just icon
        , Widget.unit = Just unit
        , Widget.sql = Just sql
        , Widget.query = Just $ scopedKql links ""
        , Widget.summarizeBy = Just Widget.SBSum
        , Widget._projectId = Just links.queryScope.projectId
        , Widget.standalone = Just True
        , Widget.hideSubtitle = Just True
        }


-- | A RUM series as SQL rather than KQL: TimeFusion serves it from the @rum_*@ rollup measures
-- only when its filter is exactly the declared predicate, which KQL's lowering never produces
-- (@startswith@ becomes a regex, @exception.type != null@ a JSON search).
binnedSql :: Text -> Text -> Text
binnedSql aggregate predicate =
  "SELECT extract(epoch from time_bucket('{{rollup_interval}}', timestamp))::integer, 'value', "
    <> aggregate
    <> "::float FROM otel_logs_and_spans WHERE {{query_ast_filters}} AND "
    <> predicate
    <> " GROUP BY time_bucket('{{rollup_interval}}', timestamp) ORDER BY time_bucket('{{rollup_interval}}', timestamp) DESC"


sessionsSql :: Text
sessionsSql =
  "SELECT distinct_count(approx_count_distinct(attributes___session___id))::float FROM otel_logs_and_spans WHERE {{query_ast_filters}} AND "
    <> browserSql
    <> " AND attributes___session___id IS NOT NULL"


p75Sql :: Text
p75Sql =
  "SELECT (approx_percentile(0.75, percentile_agg(duration)) / 1000000)::float FROM otel_logs_and_spans WHERE {{query_ast_filters}} AND "
    <> pageViewSql
    <> " AND duration IS NOT NULL"


-- | One aggregate unpivoted to (time, series, value): an @iff@ series key is not a rollup
-- dimension, while two filtered counts read two rollup measures. A page view that errored
-- counts in both series.
activitySql :: Text
activitySql =
  "SELECT a.time, s.series, CASE s.series WHEN 'Page views' THEN a.page_views ELSE a.errors END FROM ("
    <> "SELECT extract(epoch from time_bucket('{{rollup_interval}}', timestamp))::integer AS time, count(*) FILTER (WHERE "
    <> pageViewSql
    <> ")::float AS page_views, count(*) FILTER (WHERE "
    <> browserSql
    <> " AND "
    <> errorSql
    <> ")::float AS errors FROM otel_logs_and_spans WHERE {{query_ast_filters}} GROUP BY time_bucket('{{rollup_interval}}', timestamp)) a"
    <> " CROSS JOIN (VALUES ('Page views'), ('Errors')) AS s(series) ORDER BY a.time DESC"


rumActivityWidget_ :: RumLinks -> Html ()
rumActivityWidget_ links =
  div_ [class_ "h-80 min-h-80"]
    $ Widget.widget_
      def
        { Widget.wType = Widget.WTTimeseries
        , Widget.id = Just "rum-activity"
        , Widget.title = Just "Page views and errors"
        , Widget.sql = Just activitySql
        , Widget.query = Just $ scopedKql links ""
        , Widget._projectId = Just links.queryScope.projectId
        , Widget.standalone = Just True
        , Widget.hideSubtitle = Just True
        , Widget.legendPosition = Just "top-right"
        , Widget.legendSize = Just "xs"
        , Widget.allowZoom = Just True
        }


-- | Config every embedded RUM table shares. They sit inside a panel that already carries
-- the card frame, so the table adds no second surface; fixed layout makes long URLs and
-- session ids truncate instead of forcing the panel to scroll sideways.
rumTableConfig :: Text -> Table.Config
rumTableConfig elemId = def{Table.elemID = elemId, Table.renderAsTable = True, Table.noSurface = True, Table.tableClasses = "table table-sm w-full table-fixed"}


rightCol :: Text -> (a -> Html ()) -> Table.Column a
rightCol name render = (Table.col name render){Table.align = Just "text-right tabular-nums"}


tableZero_ :: Text -> Table.ZeroState
tableZero_ title = Table.ZeroState{icon = "web", title, description = "", action = ESNone}


topPages_ :: RumLinks -> [RumPage] -> Html ()
topPages_ links pages = rumPanel_ "Top pages" "Traffic and real-user load latency" (Just ("Explore all events", logsUrl links browserPageViewKql)) do
  toHtml
    Table.Table
      { config = rumTableConfig "rumTopPages"
      , columns =
          [ (Table.col "Page" \page -> a_ [href_ $ logsUrl links (browserPageViewKql <> " and " <> routeKql page.path), class_ "block truncate font-medium text-textBrand"] $ toHtml page.path){Table.attrs = [class_ "w-[46%]"]}
          , rightCol "Views" $ toHtml . show . (.views)
          , -- A page's P75 judged on the LCP field thresholds (2.5s / 4s), so the colour says
            -- at a glance which rows hurt, not just which are busiest.
            rightCol "P75 load" \page -> case page.p75LoadMs of
              Nothing -> "—"
              Just ms -> let style = ratingStyle $ classifyVital 2500 4000 (Just ms) in ratedValue_ style style.label $ toHtml $ getDurationNSMS $ round $ ms * 1e6
          , (rightCol "Last seen" $ Components.localTimeFmt_ "dd MMM HH:mm" . (.lastSeen)){Table.attrs = [class_ "max-sm:hidden"]}
          ]
      , rows = V.fromList $ sortWith (Down . (.views)) $ map merge $ M.elems $ M.fromListWith (<>) [(pageRoute p.path, pure @NonEmpty p) | p <- pages]
      , features = def{Table.zeroState = Just $ tableZero_ "No page views in this time range"}
      }
  where
    browserPageViewKql = browserKql <> " and " <> pageViewKql
    -- One row per route; views add up, the worst P75 wins (same worse-wins rule the
    -- vitals table applies across emitters), and the route replaces the raw URL.
    merge routePages@(newest :| _) =
      RumPage
        { path = pageRoute newest.path
        , views = sum $ (.views) <$> routePages
        , p75LoadMs = viaNonEmpty maximum1 $ mapMaybe (.p75LoadMs) $ toList routePages
        , lastSeen = maximum1 $ (.lastSeen) <$> routePages
        }


vitalsPanel_ :: [Vital] -> Html ()
vitalsPanel_ vitals = rumPanel_ "Core Web Vitals" "P75 of intervals ending in this range; histogram values are bucket estimates" Nothing do
  div_ [class_ "divide-y divide-strokeWeak"] $ forM_ vitals vitalRow_


vitalRow_ :: Vital -> Html ()
vitalRow_ vital = div_ [class_ "px-3 py-3"] do
  div_ [class_ "flex items-start justify-between gap-3"] do
    div_ [class_ "min-w-0"] do
      h3_ [class_ "truncate text-sm font-medium text-textStrong"] $ toHtml vital.label
      p_ [class_ "mt-0.5 text-xs text-textWeak"] $ toHtml vital.description
      p_ [class_ "mt-0.5 text-xs text-textWeak"] $ toHtml $ measurementNote vital
    div_ [class_ "shrink-0 text-right"] do
      strong_ [class_ $ "block text-sm font-semibold tabular-nums " <> style.textClass] $ toHtml $ formatVital vital
      span_ [class_ "text-xs text-textWeak"] $ toHtml $ observationLabel vital.measurement
  div_ [class_ "mt-2 grid grid-cols-3 gap-1", Aria.label_ $ vital.label <> " rating: " <> style.label] do
    forM_ [Good, NeedsImprovement, Poor] (`ratingBand` vitalRating vital)
  where
    style = ratingStyle $ vitalRating vital


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
recentErrors_ links errors = rumPanel_ "Browser errors" "Grouped by signature; counts and sessions are within this range" (Just ("View errors", logsUrl links (browserKql <> " and status_code == \"ERROR\""))) do
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
            span_ [class_ "shrink-0 text-xs text-textWeak"] $ Components.localTimeFmt_ "HH:mm" issue.lastSeen
          p_ [class_ "mt-0.5 line-clamp-2 text-xs text-textWeak"] $ toHtml issue.message
          div_ [class_ "mt-1 flex flex-wrap items-center gap-x-2 text-xs text-textWeak"] do
            forM_ issue.path $ span_ [class_ "max-w-56 truncate font-mono"] . toHtml
            when (issue.sessions > 0) $ span_ [class_ "tabular-nums"] $ toHtml $ countNoun issue.sessions "session"
            forM_ issue.sessionId \sid -> do
              a_ [href_ $ sessionsUrl links Nothing AllSessionRows (Just sid), class_ "font-medium text-textBrand hover:underline"] "View session"
              a_ [href_ $ sessionLogsUrl links sid, class_ "font-medium text-textBrand hover:underline"] "Telemetry"


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
audiencePanel_ breakdown = rumPanel_ "Audience" "Sessions by browser, operating system, and device class — errors highlight where failures concentrate" Nothing do
  if null breakdown
    then panelEmpty_ "No user agent data in this time range"
    else div_ [class_ "grid grid-cols-3 divide-x divide-strokeWeak max-md:grid-cols-1 max-md:divide-x-0 max-md:divide-y"] do
      audienceColumn_ "Browser" (Just browserHue) $ audienceBy (\(b, _, _) -> b) breakdown
      audienceColumn_ "Operating system" (Just osHue) $ audienceBy (\(_, os, _) -> os) breakdown
      audienceColumn_ "Device" (Just deviceHue) $ audienceBy (\(_, _, d) -> d) breakdown


-- | Fixed hues per family, so the colour becomes the recognition cue: Safari is the same
-- colour on the audience bars, the session rows and every future view.
osHue :: Text -> Int
osHue = \case
  "Windows" -> 230
  "macOS" -> 261
  "iOS" -> 300
  "Android" -> 200
  "ChromeOS" -> 215
  "Linux" -> 320
  _ -> 261


deviceHue :: Text -> Int
deviceHue = \case
  "Mobile" -> 200
  "Tablet" -> 300
  _ -> 230


audienceColumn_ :: Text -> Maybe (Text -> Int) -> [AudienceRow] -> Html ()
audienceColumn_ title hueM rows = div_ [class_ "min-w-0 px-3 py-2.5"] do
  h3_ [class_ "text-xs font-medium uppercase tracking-wide text-textWeak"] $ toHtml title
  ul_ [class_ "mt-2 space-y-2"] $ forM_ (take 5 rows) \row -> li_ [class_ "min-w-0"] do
    div_ [class_ "flex items-baseline justify-between gap-2 text-sm"] do
      span_ [class_ "flex min-w-0 items-center gap-1.5"] do
        forM_ hueM \hue -> span_ [class_ "rum-chip flex h-4 w-4 shrink-0 items-center justify-center rounded text-2xs font-bold", style_ $ "--hue:" <> show (hue row.name), Aria.hidden_ "true"] $ toHtml $ T.take 1 row.name
        span_ [class_ "truncate font-medium text-textStrong"] $ toHtml row.name
      span_ [class_ "shrink-0 text-xs tabular-nums text-textWeak"] do
        when (row.errors > 0) do
          span_ [class_ "font-medium text-textError"] $ toHtml $ countNoun row.errors "error"
          " · "
        toHtml $ countNoun row.sessions "session"
    div_ [class_ "mt-1 h-1.5 w-full overflow-hidden rounded-full bg-fillWeak", Aria.hidden_ "true"]
      $ div_ [class_ "rum-bar h-full rounded-full", style_ $ "width:" <> show (share row) <> "%" <> maybe "" (\hue -> ";--hue:" <> show (hue row.name)) hueM] ""
  where
    maxSessions = foldl' max 1 $ map (.sessions) rows
    share row = max 2 $ round @Double @Int $ fromIntegral row.sessions / fromIntegral maxSessions * 100


recentSessions_ :: RumData -> Html ()
recentSessions_ page = rumPanel_ "Recent sessions" "Open a recording or inspect its correlated telemetry" (Just ("View all sessions", sessionsUrl page.links Nothing AllSessionRows Nothing)) do
  when page.servedStale refreshingHint_
  sessionsTable_ False page.now page.links Nothing AllSessionRows Nothing (take 8 page.sessions)


refreshingHint_ :: Html ()
refreshingHint_ = p_ [class_ "flex items-center gap-2 border-b border-strokeWeak px-3 py-1.5 text-xs text-textWeak", role_ "status"] do
  span_ [class_ "loading loading-spinner loading-xs", Aria.hidden_ "true"] ""
  "Refreshing the full time range…"


-- | Mirrors the split layout 'sessions_' renders — list left, replay workspace right — so
-- neither the first paint nor the panel shell flashes a different frame than the content
-- that replaces it. No fake search bar: the real one is already in the toolbar above.
sessionsSkeleton_ :: Html ()
sessionsSkeleton_ = div_ [class_ "grid bg-bgBase xl:h-full xl:min-h-0 xl:grid-cols-[minmax(32rem,35%)_minmax(0,1fr)]", role_ "status", Aria.label_ "Loading sessions"] do
  section_ [class_ "min-w-0 overflow-y-auto border-strokeWeak xl:border-e max-xl:max-h-[45svh] max-xl:border-b"] do
    div_ [class_ "flex items-center gap-1.5 border-b border-strokeWeak px-3 py-1.5"]
      $ replicateM_ 3
      $ div_ [class_ "h-5 w-20 rounded-full skeleton-shimmer"] ""
    div_ [class_ "flex flex-col gap-4 p-3"]
      $ replicateM_ 8
      $ div_ [class_ "flex items-center gap-2.5"] do
        div_ [class_ "h-8 w-8 shrink-0 rounded-full skeleton-shimmer"] ""
        div_ [class_ "flex min-w-0 grow flex-col gap-1.5"] do
          div_ [class_ "h-3 w-2/5 rounded skeleton-shimmer"] ""
          div_ [class_ "h-2.5 w-3/5 rounded skeleton-shimmer"] ""
        div_ [class_ "h-3 w-14 shrink-0 rounded skeleton-shimmer"] ""
  section_ [class_ "min-w-0 bg-bgBase"] mempty


sessions_ :: RumData -> Html ()
sessions_ page = slot_ page PanelSessions sessionsSkeleton_ do
  let filtered = page.sessions
  div_ [class_ "grid bg-bgBase xl:h-full xl:min-h-0 xl:grid-cols-[minmax(32rem,35%)_minmax(0,1fr)]"] do
    section_ [id_ "rum-sessions-list", Aria.label_ "Sessions", tabindex_ "0", class_ "min-w-0 overflow-y-auto overscroll-contain border-strokeWeak xl:min-h-0 xl:border-e max-xl:max-h-[45svh] max-xl:border-b"] do
      when page.servedStale refreshingHint_
      if any (\case SessionSearchQuery{} -> True; _ -> False) page.degradedPanels then degradedBanner_ page PanelSessions else sessionsTable_ True page.now page.links page.query page.sessionFilter page.selectedSession filtered
      panelRevalidation_ page PanelSessions
    sessionWorkspace_ page


sessionWorkspace_ :: RumData -> Html ()
sessionWorkspace_ page =
  section_ [id_ "rum-replay-workspace", class_ "min-w-0 bg-bgBase xl:min-h-0 xl:overflow-y-auto xl:overscroll-contain", Aria.label_ "Session details"] do
    if any (\case SessionDetailQuery{} -> True; _ -> False) page.degradedPanels then degradedBanner_ page PanelSessionDetail else replayWorkspace_ page.links selected
    when (page.panel == Just PanelSessionDetail) $ panelRevalidation_ page PanelSessionDetail
  where
    selected = page.selectedSessionData <|> (page.selectedSession >>= \sid -> find ((== sid) . (.id)) page.sessions)


filterSessions :: SessionFilter -> [RumSession] -> [RumSession]
filterSessions sessionFilter = filter $ \session -> case sessionFilter of
  AllSessionRows -> True
  ErrorSessionRows -> session.errors > 0
  ReplaySessionRows -> session.hasReplay


sessionFilterLabel :: SessionFilter -> Text
sessionFilterLabel = \case
  AllSessionRows -> "All sessions"
  ErrorSessionRows -> "With errors"
  ReplaySessionRows -> "With replay"


-- | @workspace@ marks the Sessions tab, where the replay panel sits beside the table: there
-- a row swaps only that panel instead of re-rendering the page around it, and the shared
-- Table contributes the filter tabs and the zero state. The Overview's recent list
-- has no panel to swap, so its rows navigate and it carries none of the chrome.
sessionsTable_ :: Bool -> UTCTime -> RumLinks -> Maybe Text -> SessionFilter -> Maybe Text -> [RumSession] -> Html ()
sessionsTable_ workspace now links query sessionFilter selectedSession sessions =
  toHtml
    Table.Table
      { config = (rumTableConfig $ bool "rumRecentSessions" "rumSessions" workspace){Table.containerClasses = "w-full mx-auto space-y-0", Table.tableClasses = "table table-sm w-full table-fixed sm:min-w-[28rem]"}
      , columns =
          [ ( Table.col "Session" \session -> div_ [class_ "flex items-center gap-2.5"] do
                sessionAvatar_ session
                div_ [class_ "min-w-0 flex-1"] do
                  div_ [class_ "flex items-center gap-1.5"] do
                    a_
                      ( sessionLinkAttrs session.id
                          <> [ id_ $ "rum-session-" <> toXXHash session.id
                             , class_ "rum-session-link block truncate font-medium text-textStrong hover:text-textBrand aria-[current=true]:text-textBrand focus-visible:outline-none"
                             , data_ "session-id" session.id
                             , term "aria-current" $ bool "false" "true" (selectedSession == Just session.id)
                             , Aria.label_ $ bool "Open session for " "Watch replay for " session.hasReplay <> sessionIdentity session
                             , title_ $ sessionIdentity session
                             , onkeydown_ "if (event.key === 'Enter') event.stopPropagation()"
                             ]
                          -- A shared link lands with its row in view; the list morphs on refresh, so this fires once.
                          <> [term "_" "init call me.scrollIntoView({block: 'center'})" | selectedSession == Just session.id]
                      )
                      $ toHtml
                      $ if sessionIdentity session == session.id then "Session " <> T.take 8 session.id else sessionIdentity session
                    when (isActive now session)
                      $ span_ [class_ "rum-pulse-dot shrink-0", title_ "Active now", Aria.label_ "Session active now"] ""
                  div_ [class_ "mt-0.5 flex items-center gap-1.5 text-xs text-textWeak"] do
                    forM_ (classifyUserAgent <$> session.userAgent) \(browser, os, device) -> do
                      envChip_ browser
                      span_ $ toHtml os
                      faSprite_ (deviceIcon device) "solid" "h-3 w-3 shrink-0 text-iconNeutral"
                    span_ [class_ "truncate font-mono", title_ session.id] $ toHtml $ T.take 8 session.id
            )
              { Table.attrs = [class_ $ bool "w-[38%]" "w-[46%]" workspace <> " px-2 py-2"]
              , Table.headerExtra = Just $ span_ [class_ "md:hidden"] "Session"
              }
          , ( Table.col "Last page" \session -> do
                -- Display the distinguishing path; keep the full URL available on hover.
                span_ [class_ $ "block text-sm text-textStrong" <> bool "" " truncate" (isJust session.lastPage), title_ $ fromMaybe "" session.lastPage]
                  $ toHtml
                  $ maybe (bool "No page views" "Recording only" (replayOnly session)) pageLabel session.lastPage
                span_ [class_ "mt-0.5 block text-xs text-textWeak", title_ $ show session.endedAt]
                  $ toHtml
                  $ prettyTimeShort now session.endedAt
            )
              { Table.attrs = [class_ $ bool "w-[22%]" "w-[20%]" workspace <> " px-2 py-2"]
              }
          , ( Table.col "Activity" \session -> do
                when (session.errors > 0) $ span_ [class_ "mb-1 inline-flex items-center gap-1 rounded-full bg-fillError-weak px-1.5 py-0.5 text-xs font-medium text-textError"] do
                  faSprite_ "triangle-exclamation" "solid" "h-2.5 w-2.5 shrink-0"
                  toHtml $ countNoun session.errors "error"
                if replayOnly session
                  then span_ [class_ "text-xs text-textWeak"] "No telemetry"
                  else div_ [class_ "flex items-center gap-2.5 text-xs tabular-nums"] do
                    -- sr-only carries the unit so the icon+number readout stays accessible.
                    span_ [class_ "flex items-center gap-1 text-textStrong", title_ "Page views"] do
                      faSprite_ "file-lines" "regular" "h-3 w-3 text-iconBrand"
                      toHtml $ show session.views
                      span_ [class_ "sr-only"] $ toHtml $ " " <> countNoun session.views "view"
                    span_ [class_ "flex items-center gap-1 text-textWeak", title_ "Events"] do
                      faSprite_ "bolt" "regular" "h-3 w-3 text-iconNeutral"
                      toHtml $ show session.events
                      span_ [class_ "sr-only"] $ toHtml $ " " <> countNoun session.events "event"
            )
              { Table.attrs = [class_ $ bool "w-[22%]" "w-[18%]" workspace <> " px-2 py-2 max-sm:hidden"]
              }
          , ( Table.col "Duration" \session -> do
                span_ [class_ "block text-sm tabular-nums text-textStrong"] $ toHtml $ formatSessionDuration session
                if session.hasReplay
                  then span_ [class_ "mt-0.5 inline-flex items-center gap-1 rounded-full bg-fillSuccess-weak px-1.5 py-0.5 text-xs font-medium text-textSuccess"] do
                    faSprite_ "circle-play" "regular" "h-2.5 w-2.5 shrink-0"
                    "Replay"
                  else span_ [class_ "mt-0.5 block text-xs text-textWeak"] "No replay"
            )
              { Table.attrs = [class_ $ bool "w-[18%]" "w-[16%]" workspace <> " max-sm:w-[20%] px-2 py-2.5"]
              }
          ]
      , rows = V.fromList sessions
      , features =
          def
            { Table.rowAttrs =
                Just
                  $ const
                    [ class_ " cursor-pointer [&:has(a[aria-current=true])]:bg-fillBrand-weak [&:has(a:focus-visible)]:outline-2 [&:has(a:focus-visible)]:-outline-offset-2 [&:has(a:focus-visible)]:outline-strokeFocus"
                    , onclick_ "if (!event.target.closest('a, button, input, select, textarea') && !window.getSelection()?.toString()) { const link = this.querySelector('.rum-session-link'); if (event.ctrlKey || event.metaKey) window.open(link.href, '_blank', 'noopener'); else link.click(); }"
                    ]
            , Table.header =
                guard workspace
                  $> div_ [class_ "sticky top-0 z-10 flex flex-wrap items-center justify-between gap-2 border-b border-strokeWeak bg-bgBase px-3 py-1"] do
                    nav_ [class_ "tabs tabs-box tabs-outline tabs-xs items-center", Aria.label_ "Filter sessions"] $ forM_ [minBound .. maxBound] $ \value -> do
                      let url = sessionsUrl links query value selectedSession
                          filterValue = fromMaybe "" $ sessionFilterParam value
                      a_
                        [ href_ url
                        , hxGet_ $ "/p/" <> links.queryScope.projectId.toText <> "/rum"
                        , term "hx-include" "#rum-session-search-form"
                        , term "hx-vals" $ "{\"panel\":\"sessions\",\"deferred\":\"1\",\"filter\":\"" <> filterValue <> "\"}"
                        , hxTarget_ "#rum-sessions-list"
                        , hxSelect_ "#rum-sessions-list"
                        , hxSwap_ "outerMorph"
                        , hxPushUrl_ url
                        , term "hx-sync" "#rum-session-search-form:replace"
                        , data_ "filter" filterValue
                        , term "hx-on::before:request" "document.getElementById('rum-session-filter').value = this.dataset.filter"
                        , term "aria-current" $ bool "false" "page" (value == sessionFilter)
                        , class_ $ "tab h-auto! " <> bool "" "tab-active text-textStrong" (value == sessionFilter)
                        ]
                        $ toHtml
                        $ sessionFilterLabel value
                    span_ [class_ "flex shrink-0 items-center gap-2.5"] do
                      let activeCount = length $ filter (isActive now) sessions
                      when (activeCount > 0)
                        $ span_ [class_ "inline-flex items-center gap-1.5 rounded-full bg-fillSuccess-weak px-2 py-0.5 text-xs font-medium text-textSuccess"] do
                          span_ [class_ "rum-pulse-dot", Aria.hidden_ "true"] ""
                          toHtml $ show activeCount <> " active now"
                      span_ [class_ "text-xs tabular-nums text-textWeak", role_ "status", Aria.live_ "polite"] $ toHtml $ (if length sessions == 200 then "Newest " else "") <> countNoun (length sessions) "session"
            , Table.zeroState = Just $ tableZero_ $ bool "No sessions in this time range" "No sessions match this filter" (sessionFilter /= AllSessionRows || maybe False (not . T.null) query)
            }
      }
  where
    sessionLinkAttrs sid =
      let url = sessionsUrl links query sessionFilter $ Just sid
       in href_ url
            : if workspace
              then
                [ hxGet_ $ url <> "&panel=session_detail&deferred=1"
                , hxTarget_ "#rum-replay-workspace"
                , term "hx-on::after:request" "if (event.detail.ctx.response.status >= 200 && event.detail.ctx.response.status < 300) { document.querySelectorAll('.rum-session-link').forEach(link => link.setAttribute('aria-current', String(link.dataset.sessionId === this.dataset.sessionId))); document.getElementById('rum-session-search').form.elements.session.value = this.dataset.sessionId; }"
                , hxSelect_ "#rum-replay-workspace"
                , hxSwap_ "outerHTML"
                , hxPushUrl_ url
                , hxIndicator_ "#rum-replay-workspace"
                , term "hx-sync" "#rum-replay-workspace:replace"
                ]
              else []


replayWorkspace_ :: RumLinks -> Maybe RumSession -> Html ()
replayWorkspace_ links = \case
  Just session -> div_ [class_ "min-h-full"] do
    header_ [class_ "border-b border-strokeWeak bg-bgBase px-4 py-3"] do
      div_ [class_ "flex flex-wrap items-center justify-between gap-3"] do
        div_ [class_ "flex min-w-0 items-center gap-3"] do
          sessionAvatar_ session
          div_ [class_ "min-w-0"] do
            div_ [class_ "flex items-center gap-2"] do
              h2_ [class_ "truncate text-sm font-semibold text-textStrong"] $ toHtml $ sessionIdentity session
              forM_ (classifyUserAgent <$> session.userAgent) \(browser, os, device) -> do
                envChip_ browser
                span_ [class_ "text-xs text-textWeak"] $ toHtml os
                faSprite_ (deviceIcon device) "solid" "h-3 w-3 text-iconNeutral"
            p_ [class_ "mt-0.5 truncate font-mono text-xs text-textWeak"] $ toHtml session.id
        a_ [href_ $ sessionLogsUrl links session.id, class_ "btn btn-sm gap-1.5"] do
          faSprite_ "magnifying-glass-chart" "regular" "h-3.5 w-3.5"
          "Inspect telemetry"
      -- The facts the list row carries, so a shared link answers "how long, how many pages,
      -- any errors" before the recording has loaded. Errors are the only tinted value.
      dl_ [class_ "mt-2 flex flex-wrap items-baseline gap-x-4 gap-y-1 text-xs tabular-nums"] do
        let fact :: Text -> Text -> Maybe Text -> Html ()
            fact label value hint = div_ [class_ "flex min-w-0 items-baseline gap-1"] do
              dt_ [class_ "shrink-0 text-textWeak"] $ toHtml label
              dd_ (class_ "truncate font-medium text-textStrong" : [title_ h | Just h <- [hint]]) $ toHtml value
        fact "Duration" (formatSessionDuration session) Nothing
        if replayOnly session
          then div_ do
            dt_ [class_ "sr-only"] "Telemetry"
            dd_ [class_ "text-textWeak"] "No telemetry"
          else do
            fact "Page views" (show session.views) Nothing
            fact "Events" (show session.events) Nothing
            if session.errors > 0
              then div_ do
                dt_ [class_ "sr-only"] "Errors"
                dd_ [class_ "inline-flex items-center gap-1 rounded-full bg-fillError-weak px-1.5 py-0.5 font-medium text-textError"] do
                  faSprite_ "triangle-exclamation" "solid" "h-2.5 w-2.5 shrink-0"
                  toHtml $ countNoun session.errors "error"
              else fact "Errors" "0" Nothing
        forM_ session.lastPage \url -> fact "Last page" (pageLabel url) (Just url)
        forM_ session.service \service -> fact "Service" service (Just service)
    if session.hasReplay
      then termRaw "session-replay" [id_ "rumSessionReplay", term "initialSession" session.id, term "consoleOpen" "true", term "fullWidth" "true", class_ "block min-h-[34rem] w-full", term "projectId" links.queryScope.projectId.toText, term "containerId" "rum-replay-workspace"] ("" :: Text)
      else div_ [class_ "space-y-1 p-4"] do
        h3_ [class_ "text-sm font-medium text-textStrong"] "No recording for this session"
        p_ [class_ "text-sm text-textWeak"] "Inspect telemetry to follow navigation, network requests, and errors."
  Nothing ->
    div_ [class_ "flex min-h-[34rem] flex-col items-center justify-center p-8"]
      $ Components.emptyState_ def{icon = Just "video"} "Select a session" "Choose a session to inspect its activity or watch an available recording."


performance_ :: RumData -> Html ()
performance_ page = div_ [class_ "space-y-2 px-4 pb-4 pt-2 max-md:px-3"] do
  -- A project that reports no Web Vitals at all gets one explanation, not five "No data"
  -- rows plus two more empty panels below them.
  let noVitals = all ((== Unmeasured) . (.measurement)) page.vitals
  slot_ page PanelVitals (panelSkeleton_ $ Components.tableSkeleton_ 6)
    $ if noVitals
      then
        rumPanel_ "Web Vitals" "P75 of LCP, INP, CLS, FCP and TTFB against Google's thresholds" Nothing
          $ Components.emptyState_
            def
              { icon = Just "gauge"
              , action = ESCustom $ div_ [class_ "flex flex-wrap items-center justify-center gap-3"] do
                  a_ [href_ $ rumUrl page.links{window = TimePicker.mkTimeWindow page.now Nothing Nothing (Just "7D")} [("tab", "performance")], class_ "btn btn-sm btn-primary"] "Widen to 7 days"
                  a_ [href_ "https://monoscope.tech/docs/sdks/browser/", target_ "_blank", rel_ "noopener noreferrer", class_ "link text-sm text-textBrand"] "Check the SDK metrics export"
              }
            "No Web Vitals in this time range"
            "Nothing sent a browser.web_vital.* metric for this scope. The browser SDK reports LCP, INP, CLS, FCP and TTFB automatically once its metrics export is on."
      else vitalsTable_ page.vitals
  slot_ page PanelVitalTrend (panelSkeleton_ Components.chartSkeleton_) $ unless noVitals do
    vitalTrendPanel_ page.links.window page.vitalTrend
    div_ [class_ "mt-4"] $ pageVitalsTable_ page.links page.pageVitals
  div_ [class_ "grid grid-cols-2 gap-4 max-lg:grid-cols-1"] do
    slot_ page PanelPages (panelSkeleton_ $ Components.tableSkeleton_ 5) $ topPages_ page.links page.pages
    slot_ page PanelErrors (panelSkeleton_ $ Components.tableSkeleton_ 5) $ recentErrors_ page.links page.errors
  slot_ page PanelAudience (panelSkeleton_ $ Components.tableSkeleton_ 3) $ audiencePanel_ page.breakdown


-- | One platform chart per vital, with Google's good/poor marks as the widget's own
-- threshold lines. The data uses the shared server-side population because
-- histogram-backed vitals are not yet expressible in the widget KQL pipeline (@value@ is
-- NULL on histogram datapoints); the dataset is embedded, so the widget renders like every
-- other chart in the product without fetching anything of its own.
vitalTrendPanel_ :: TimePicker.TimeWindow -> [VitalTrendPoint] -> Html ()
vitalTrendPanel_ window points = rumPanel_ "Web Vitals over time" "P75 of intervals ending in each bucket; unavailable coverage appears as gaps" Nothing do
  if null points
    then panelEmpty_ "No web vital samples in this time range"
    else div_ [class_ "grid grid-cols-2 gap-3 p-3 max-lg:grid-cols-1"] $ forM_ vitalDefinitions \vital -> do
      let series = sortWith fst [(p.bucket, RUM.measurementValue p.measurement) | p <- points, p.metricName == vital.name]
          -- Milliseconds, matching what the chart endpoint serves (Charts.convertTimestampsToMs):
          -- the axis reads epoch-ms, and seconds silently render as a 1970 timeline.
          sourceRows = AE.toJSON (["timestamp", "P75"] :: [Text]) : [AE.toJSON (floor (1000 * utcTimeToPOSIXSeconds (max window.fromTime bucketTime)) :: Int64, value) | (bucketTime, value) <- series]
      unless (null series)
        $ div_ [class_ "h-52 min-h-52"]
        $ Widget.widget_
          def
            { Widget.wType = Widget.WTTimeseriesLine
            , Widget.id = Just $ "rum-vital-trend-" <> vital.name
            , Widget.title = Just vital.label
            , Widget.unit = Just vital.unit
            , Widget.standalone = Just True
            , Widget.hideSubtitle = Just True
            , Widget.hideLegend = Just True
            , Widget.hideValue = Just True
            , Widget.warningThreshold = Just vital.goodAt
            , Widget.alertThreshold = Just vital.poorAt
            , Widget.showThresholdLines = Just "always"
            , Widget.dataset =
                Just
                  (def :: Widget.WidgetDataset)
                    { Widget.source = AE.toJSON sourceRows
                    , Widget.from = Just $ floor $ 1000 * utcTimeToPOSIXSeconds window.fromTime
                    , Widget.to = Just $ floor $ 1000 * utcTimeToPOSIXSeconds window.toTime
                    }
            }


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


-- | Collapse page URLs into routes, the grouping Datadog performs into view names: query
-- and fragment dropped, and any path segment 'replaceAllFormats' finds variable content in
-- (uuid, number, hash — the masks error grouping uses) becomes @:id@ wholesale, so SKUs
-- like @2ZYFJ3GM2N@ don't leave partial-mask confetti. Short segments such as @v2@ stay
-- literal.
--
-- A wholly id-like segment keeps its inferred type — "/lookup/{uuid}" vs
-- "/lookup/{integer}" is the key shape of the endpoint, learned without any route
-- template. @:id@ is reserved for SKU-style tokens where no type is detectable.
--
-- >>> pageRoute "https://shop.example/cart/checkout/c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd?order=x"
-- "/cart/checkout/{uuid}"
-- >>> pageRoute "/product/2ZYFJ3GM2N"
-- "/product/:id"
-- >>> pageRoute "/products/12345"
-- "/products/{integer}"
-- >>> pageRoute "/api/v2/cart"
-- "/api/v2/cart"
--
-- Host-style or wordy segments keep their identity even when they contain a digit:
--
-- >>> pageRoute "l2.example/cached"
-- "l2.example/cached"
pageRoute :: Text -> Text
pageRoute = T.intercalate "/" . map maskSegment . T.splitOn "/" . T.takeWhile (`notElem` ("?#" :: String)) . pageLabel
  where
    maskSegment seg
      | masked `elem` ["{uuid}", "{integer}", "{hex}", "{sha1}", "{sha256}", "{md5}"] = masked
      | T.length seg >= 8 && T.any isDigit seg && not (T.any isLower seg) = ":id"
      | otherwise = seg
      where
        masked = replaceAllFormats seg


-- | Explorer filter for a masked route: exact path match when nothing was masked, else a
-- prefix match on the static part before the first mask.
routeKql :: Text -> Text
routeKql route
  -- Both spellings, matching 'pagePath': our SDK sets url.path, the browser SDK only url.full.
  | prefix == route = "(attributes.url.path == " <> kqlValue route <> " or attributes.url.full matches regex " <> kqlValue routeRegex <> ")"
  | otherwise = "(attributes.url.path startswith " <> kqlValue prefix <> " or attributes.url.full contains " <> kqlValue prefix <> ")"
  where
    prefix = fst $ T.breakOn "{" $ fst $ T.breakOn ":id" route
    origin = "[A-Za-z][A-Za-z0-9+.-]*://[^/?#]+"
    routeRegex = "^" <> (if route == "/" then "(" <> origin <> "/?|/)" else "(" <> origin <> ")?" <> escapeRegex route) <> "([?#].*)?$"


-- | Traffic-weighted room for improvement, after Sentry's "Opportunity" ranking: sample
-- count times how far each vital's P75 sits past its "good" threshold, normalized per
-- vital so CLS (unitless) and the millisecond vitals weigh equally.
--
-- The invariant this exists for — a quiet page with a poor vital outranks a busy page
-- whose vitals are all good:
--
-- >>> opportunityScore [(2500, 4000, 2000)] 100000
-- 0.0
-- >>> opportunityScore [(2500, 4000, 4750)] 10 > opportunityScore [(2500, 4000, 2000)] 100000
-- True
opportunityScore :: [(Double, Double, Double)] -> Natural -> Double
opportunityScore vitals sampleTotal = fromIntegral sampleTotal * sum [max 0 ((p75 - goodAt) / (poorAt - goodAt)) | (goodAt, poorAt, p75) <- vitals]


-- | Sentry's signature vitals view: one row per page, P75 per vital, each judged on its
-- own thresholds. Rows lead with the largest 'opportunityScore' (ties broken by traffic),
-- so the fix that would move the site-wide experience most is always on top.
pageVitalsTable_ :: RumLinks -> [PageVitalPoint] -> Html ()
pageVitalsTable_ links points = rumPanel_ "Web Vitals by page" "Exact page URLs; histogram P75s are estimates within the shown bucket range" Nothing do
  toHtml
    Table.Table
      { config = (rumTableConfig "rumPageVitals"){Table.tableClasses = "table table-sm w-full table-fixed max-sm:[&_thead]:sr-only max-sm:[&_td]:block max-sm:[&_td]:min-w-0 max-sm:[&_td]:border-0 max-sm:[&_td]:py-1 max-sm:[&_td]:text-left"}
      , columns =
          ( Table.col "Page" \(url, _, _) -> a_ [href_ $ logsUrl links (browserKql <> " and " <> exactPageKql url), class_ "block truncate font-medium text-textBrand max-sm:min-h-11 max-sm:whitespace-normal max-sm:break-all", title_ url, Aria.label_ url] do
              toHtml $ pageLabel url
              when (pageLabel url /= url) $ span_ [class_ "block truncate text-xs text-textWeak max-sm:whitespace-normal max-sm:break-all"] $ toHtml url
          )
            { Table.attrs = [class_ "sm:w-[28%] max-sm:col-span-2"]
            }
            : [ rightCol (T.toUpper vital.name) \(_, byVital, _) -> do
                  span_ [class_ "block text-xs text-textWeak sm:hidden", Aria.hidden_ "true"] $ toHtml $ T.toUpper vital.name <> " P75"
                  vitalCell vital byVital
              | vital <- vitalDefinitions
              ]
              <> [ rightCol "Observations" \(_, _, sampleTotal) -> do
                     span_ [class_ "block text-xs text-textWeak sm:hidden", Aria.hidden_ "true"] "Observations"
                     toHtml $ show sampleTotal
                 ]
      , rows = V.fromList pageRows
      , features = def{Table.zeroState = Just $ tableZero_ "No page-attributed web vital observations in this time range", Table.rowAttrs = Just $ const [class_ " max-sm:grid max-sm:grid-cols-2 max-sm:gap-x-4 max-sm:border-b max-sm:border-strokeWeak max-sm:py-3 max-sm:last:border-0"]}
      }
  where
    exactPageKql url
      | "://" `T.isInfixOf` url = "attributes.url.full == " <> kqlValue url
      | otherwise = routeKql url
    vitalCell :: Vital -> M.Map Text VitalMeasurement -> Html ()
    vitalCell vital byVital = case M.lookup vital.name byVital of
      Nothing -> span_ [class_ "text-textWeak"] "—"
      Just measurement -> do
        let measured = (vital :: Vital){measurement}
        ratedValue_ (ratingStyle $ vitalRating measured) (measurementNote measured) $ toHtml $ formatVital measured
        span_ [class_ "block text-xs text-textWeak sm:hidden"] $ toHtml $ measurementNote measured
    score byVital = opportunityScore [(v.goodAt, v.poorAt, value) | v <- vitalDefinitions, Just measurement <- [M.lookup v.name byVital], Just value <- [RUM.measurementValue measurement]]
    pageRows =
      take 12
        $ sortWith
          (\(_, byVital, sampleTotal) -> Down (score byVital sampleTotal, sampleTotal))
          [(url, M.fromList [(point.metricName, point.measurement) | point <- cells], sum $ map (RUM.measurementSamples . (.measurement)) cells) | (url, cells) <- M.toList $ M.fromListWith (<>) [(point.page, [point]) | point <- points]]


vitalsTable_ :: [Vital] -> Html ()
vitalsTable_ vitals = do
  rumPanel_ "Web Vitals field performance" "Intervals ending in this range can include earlier observations. Histogram estimates assume uniform values within a bucket." Nothing do
    div_ [class_ "overflow-x-auto"] $ table_ [class_ "table table-sm w-full max-sm:[&_td]:block max-sm:[&_td]:min-w-0 max-sm:[&_td]:border-0 max-sm:[&_td]:py-1"] do
      thead_ [class_ "max-sm:sr-only"] $ tr_ $ th_ "Metric" >> th_ [class_ "text-right"] "P75" >> th_ [class_ "text-right"] "Good" >> th_ [class_ "text-right"] "Poor" >> th_ "Assessment" >> th_ "Coverage" >> th_ [class_ "text-right"] "Observations"
      tbody_ $ forM_ vitals \vital -> tr_ [class_ "max-sm:grid max-sm:grid-cols-2 max-sm:border-b max-sm:border-strokeWeak max-sm:py-3 max-sm:last:border-0"] do
        let style = ratingStyle $ vitalRating vital
        td_ [class_ "max-sm:col-span-2"] do
          strong_ [class_ "block text-sm font-medium text-textStrong"] $ toHtml vital.label
          span_ [class_ "text-xs text-textWeak"] $ toHtml vital.description
        td_ [class_ $ "text-right font-semibold tabular-nums max-sm:order-1 max-sm:text-left " <> style.textClass] do
          span_ [class_ "mr-2 text-xs font-normal text-textWeak sm:hidden", Aria.hidden_ "true"] "P75"
          toHtml $ formatVital vital
        forM_ ([("Good", vital.goodAt, False), ("Poor", vital.poorAt, True)] :: [(Text, Double, Bool)]) \(label, threshold, poor) -> td_ [class_ $ "text-right tabular-nums text-textWeak max-sm:text-left " <> bool "max-sm:order-5" "max-sm:order-6" poor] do
          span_ [class_ "mr-2 text-xs sm:hidden", Aria.hidden_ "true"] $ toHtml label
          toHtml $ formatVitalThreshold vital threshold
        td_ [class_ "max-sm:order-2 max-sm:text-right"] $ span_ [class_ $ "inline-flex items-center gap-1.5 rounded-md px-2 py-1 text-xs font-medium " <> style.badgeClass] do
          span_ [class_ $ "h-2 w-2 rounded-full " <> style.fillClass, Aria.hidden_ "true"] ""
          toHtml style.label
        td_ [class_ "text-xs text-textWeak max-sm:order-4 max-sm:col-span-2"] $ toHtml $ measurementNote vital
        td_ [class_ "text-right tabular-nums text-textWeak max-sm:order-3 max-sm:col-span-2 max-sm:text-left"] $ toHtml $ observationLabel vital.measurement


-- | Every RUM card is the shared 'Components.panel_' flush-card variant.
rumPanel_ :: Text -> Text -> Maybe (Text, Text) -> Html () -> Html ()
rumPanel_ title subtitle action = Components.panel_ def{Components.flushCard = True, Components.subtitle = Just subtitle, Components.action = action} title


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


-- | A session counts as live while its newest event is fresher than the ingest lag.
isActive :: UTCTime -> RumSession -> Bool
isActive now session = diffUTCTime now session.endedAt < 300


-- | Deterministic hue for one identity, so a user's avatar keeps its colour across renders.
-- A curated list rather than hash-mod-360: arbitrary hues include the muddy ones.
--
-- >>> identityHue "ada@example.com" == identityHue "ada@example.com"
-- True
identityHue :: Text -> Int
identityHue ident = hues V.! (T.foldl' (\acc c -> acc * 31 + fromEnum c) 7 ident `mod` V.length hues)
  where
    hues = V.fromList [261, 205, 162, 120, 69, 21, 350, 300]


-- | Up to two letters for the avatar: the initials of a name, else the identity's first
-- letters.
--
-- >>> sessionMonogram' (Just "Ada Lovelace") "x"
-- "AL"
-- >>> sessionMonogram' Nothing "ada@example.com"
-- "AD"
sessionMonogram' :: Maybe Text -> Text -> Text
sessionMonogram' (Just name) _
  | (w1 : w2 : _) <- words name = T.toUpper $ T.take 1 w1 <> T.take 1 w2
sessionMonogram' _ ident = T.toUpper $ T.take 2 $ T.filter isLetter ident


-- | Coloured monogram avatar for the session's identity. Anonymous sessions (no name,
-- email or id attribute — identity falls back to the session UUID) get a neutral user
-- glyph rather than two hex letters pretending to be initials.
sessionAvatar_ :: RumSession -> Html ()
sessionAvatar_ session
  | anonymous = span_ [class_ "flex h-8 w-8 shrink-0 items-center justify-center rounded-full bg-fillWeak text-textWeak", Aria.hidden_ "true"] $ faSprite_ "user" "regular" "h-3.5 w-3.5"
  | otherwise =
      span_ [class_ "rum-avatar flex h-8 w-8 shrink-0 items-center justify-center rounded-full text-xs font-semibold", style_ $ "--hue:" <> show (identityHue ident), Aria.hidden_ "true"]
        $ toHtml
        $ sessionMonogram' session.userName ident
  where
    ident = sessionIdentity session
    anonymous = ident == session.id


-- | One coloured letter per browser family, fixed per family so the colour itself becomes
-- the recognition cue across rows.
browserHue :: Text -> Int
browserHue = \case
  "Chrome" -> 230
  "Safari" -> 205
  "Firefox" -> 340
  "Edge" -> 195
  "Opera" -> 330
  "Samsung Internet" -> 280
  _ -> 261


envChip_ :: Text -> Html ()
envChip_ browser = span_ [class_ "rum-chip flex h-4 w-4 shrink-0 items-center justify-center rounded text-2xs font-bold", style_ $ "--hue:" <> show (browserHue browser), title_ browser] $ toHtml $ T.take 1 browser


deviceIcon :: Text -> Text
deviceIcon = \case
  "Mobile" -> "mobile"
  "Tablet" -> "mobile"
  _ -> "laptop"


formatVital :: Vital -> Text
formatVital vital = case vital.measurement of
  Unmeasured -> "No data"
  Unavailable _ _ -> "Unavailable"
  Measured _ estimate -> (if isJust (RUM.estimateRange estimate) then "≈ " else "") <> formatVitalThreshold vital (RUM.estimateValue estimate)


measurementNote :: Vital -> Text
measurementNote vital = case vital.measurement of
  Unmeasured -> "No observations"
  Measured _ estimate -> maybe "Exact quantile" (\(lower, upper) -> "Estimated within " <> formatVitalThreshold vital lower <> "–" <> formatVitalThreshold vital upper) $ RUM.estimateRange estimate
  -- Each note names the export defect and its fix (SDK, collector, or time range).
  Unavailable _ issue -> case issue of
    RUM.MissingBaseline -> "Cumulative histogram with no earlier export in range; widen the range"
    RUM.UnknownStart -> "Cumulative histogram exported without a start time; use delta temporality"
    RUM.UnknownTemporality -> "Histogram exported without an aggregation temporality"
    RUM.UnsupportedPopulation -> "Unsupported population"
    RUM.InvalidValue -> "Observation is negative or infinite"
    RUM.InvalidBuckets -> "Histogram bucket counts do not match its bounds"
    RUM.InvalidReset -> "Cumulative count went backwards without a new interval"
    RUM.OverlappingIntervals -> "Report intervals overlap; the exporter is double-counting"
    RUM.UnboundedBucket -> "P75 falls in the histogram's open top bucket; add a higher bound"
    RUM.InterruptedSeries -> "Series restarted mid-range without a baseline"


observationLabel :: VitalMeasurement -> Text
observationLabel measurement =
  countNoun (RUM.measurementSamples measurement) $ case measurement of
    Unavailable{} -> "known observation"
    _ -> "observation"


formatVitalThreshold :: Vital -> Double -> Text
formatVitalThreshold vital value
  | vital.unit == "ms" = getDurationNSMS $ round $ value * 1e6
  | otherwise = showFFloat' 3 value


data RatingStyle = RatingStyle {label :: Text, textClass :: Text, fillClass :: Text, badgeClass :: Text}


-- | A rated number carries its rating as a dot and as text, never as colour alone.
ratedValue_ :: RatingStyle -> Text -> Html () -> Html ()
ratedValue_ style title value = span_ [class_ $ "inline-flex items-center gap-1.5 font-medium tabular-nums " <> style.textClass, title_ title] do
  span_ [class_ $ "h-2 w-2 shrink-0 rounded-full " <> style.fillClass, Aria.hidden_ "true"] ""
  span_ [class_ "sr-only"] $ toHtml $ style.label <> ": "
  value


ratingStyle :: VitalRating -> RatingStyle
ratingStyle = \case
  Good -> RatingStyle "Good" "text-textSuccess" "bg-fillSuccess-strong" "bg-fillSuccess-weak text-textSuccess"
  NeedsImprovement -> RatingStyle "Needs improvement" "text-textWarning" "bg-fillWarning-strong" "bg-fillWarning-weak text-textWarning"
  Poor -> RatingStyle "Poor" "text-textError" "bg-fillError-strong" "bg-fillError-weak text-textError"
  Unknown -> RatingStyle "Not assessed" "text-textWeak" "bg-fillNeutral-strong" "bg-fillWeak text-textWeak"


-- | The KQL counterparts of 'browserScope' and 'pageViewPredicate'. A link that filtered on
-- @telemetry.sdk.language == "webjs"@ sent the user to an Explorer view of nothing, because no
-- browser SDK in production sets it — the same mistake the queries themselves made.
browserKql :: Text
browserKql = "(resource.telemetry.sdk.language == \"webjs\" or resource.user_agent.original != \"\" or name in (\"documentLoad\", \"documentFetch\") or name startswith \"Pageview \")"


-- | The KQL twin of 'pageViewPredicate'.
pageViewKql :: Text
pageViewKql = "(name == \"documentLoad\" or name startswith \"Pageview \")"


-- | A first-stage KQL filter carrying the page's investigation boundary. The scope has to
-- land before any pipe — or a piped query would append it to the summarize — and must retain
-- both the selected environment and service when the reader opens the Explorer.
scopedKql :: RumLinks -> Text -> Text
scopedKql links = applyScopedKqlContext links.queryScope


-- | KQL string literal: backslashes first, then quotes, so a value can't break out of the literal.
kqlValue :: Text -> Text
kqlValue value = "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" value) <> "\""


-- | A RUM URL carrying the scope the page is under. 'TimePicker.windowUrl' URI-encodes each
-- value, so nothing here may encode first: a pre-encoded KQL query arrives at the Log
-- Explorer double-escaped (@%2520@ for a space) and parses as one meaningless token.
rumUrl :: RumLinks -> [(Text, Text)] -> Text
rumUrl links extras =
  TimePicker.windowUrl
    ("/p/" <> links.queryScope.projectId.toText <> "/rum")
    ( extras
        <> [("environment", environment) | environment <- maybeToList links.queryScope.environment]
        <> [("service_scope", service) | service <- maybeToList links.queryScope.service]
    )
    links.window


-- | Log Explorer link for a browser query. The service scope is appended to the KQL rather
-- than passed alongside it, so what the Explorer lists is what the panel counted.
logsUrl :: RumLinks -> Text -> Text
logsUrl links query =
  TimePicker.windowUrl
    ("/p/" <> links.queryScope.projectId.toText <> "/log_explorer")
    [("query", scopedKql links query)]
    links.window


-- | Log Explorer link for one session's correlated events.
sessionLogsUrl :: RumLinks -> Text -> Text
sessionLogsUrl links sid = logsUrl links $ "attributes.session.id == " <> kqlValue sid


sessionsUrl :: RumLinks -> Maybe Text -> SessionFilter -> Maybe Text -> Text
sessionsUrl links query sessionFilter sessionM =
  rumUrl links
    $ ("tab", "sessions")
    : [(key, value) | (key, Just value) <- [("q", query), ("filter", sessionFilterParam sessionFilter), ("session", sessionM)]]


degradedBanner_ :: RumData -> RumPanel -> Html ()
degradedBanner_ page panel = div_ [role_ "alert", class_ "flex items-start gap-2 border-b border-strokeWarning-strong bg-fillWarning-weak px-4 py-2.5 text-sm text-textStrong"] do
  faSprite_ "triangle-exclamation" "solid" "mt-0.5 h-4 w-4 shrink-0 text-iconWarning"
  div_ do
    strong_ "Some RUM data could not be loaded."
    span_ [class_ "ml-1 text-textWeak"] $ toHtml $ "Retry or narrow the time range. Unavailable: " <> T.intercalate ", " (map rumQueryLabel page.degradedPanels) <> "."
  button_ ([type_ "button", class_ "btn btn-sm shrink-0"] <> panelSwapAttrs page panel True "click" (Just "replace")) "Retry"


rumQueryLabel :: RumQuery -> Text
rumQueryLabel = \case
  PresenceQuery -> "experience"
  PagesQuery -> "pages"
  ErrorsQuery -> "errors"
  SessionSearchQuery{} -> "sessions"
  SessionDetailQuery{} -> "session"
  VitalPopulationQuery{} -> "web vitals"
  BreakdownQuery -> "audience"
