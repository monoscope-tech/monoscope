-- | Uptime checks, cron monitors and status pages: lists with day bars, a dashboard per
-- check, and status pages (public at @/status/<slug>@, internal inside the project).
module Pages.Uptime (
  MonitorSection (..),
  monitorSections_,
  UptimeChecks,
  uptimeChecksGetH,
  UptimeForm (..),
  UptimeRejection (..),
  UptimeList,
  uptimeCheckPostH,
  uptimeCheckToggleH,
  uptimeCheckDeleteH,
  UptimeCheckPage,
  uptimeCheckGetH,
  Redirected,
  uptimeCheckEditH,
  CronMonitors,
  cronMonitorsGetH,
  CronForm (..),
  CronRejection (..),
  CronList,
  cronMonitorPostH,
  cronMonitorDeleteH,
  CronMonitorPage,
  cronMonitorGetH,
  CronEditForm (..),
  cronMonitorEditH,
  StatusPages,
  statusPagesGetH,
  StatusPageForm (..),
  StatusPageRejection (..),
  statusPagePostH,
  statusPageEditH,
  statusPageDeleteH,
  StatusPageView,
  statusPageGetH,
  statusAllGetH,
  publicStatusPageGetH,
) where

import Data.Aeson qualified as AE
import Data.Char (isAsciiLower, isDigit)
import Data.Default (def)
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Ord (clamp)
import Data.Text qualified as T
import Data.Time (Day, UTCTime (..), addDays, addUTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful (Eff, type (:>))
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (ask)
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.PrometheusScrapeConfigs qualified as PromCfg
import Models.Apis.Uptime qualified as Uptime
import Models.Projects.DashboardTemplates qualified as DashboardTemplates
import Models.Projects.ProjectMembers qualified as ManageMembers
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx)
import Pages.Components (BadgeColor (..), EmptyStateCfg (..), EmptyStateSize (..), FieldCfg (..), FieldSize (..), PanelCfg (..), PanelStyle (..), ResourceRow (..), agoText, callout_, copyField_, dangerAction_, detailTab_, emptyState_, factGrid_, formCheckbox_, formField_, formSelectField_, options_, panel_, resourceList_, sectionLabel_, statFigure_, tabPanel_, toneDot_, tonePill_)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Relude hiding (ask)
import Servant qualified
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, ATBaseCtx, ActionResult, DB, Done, Outcome (..), Refusal (..), Rendered (..), RespHeaders, act, addRespHeaders, addSuccessToast, addTriggerEvent, done, successToast)
import Utils (TabStrip (..), countNoun, faSprite_, hostPath, navTabStrip_, safeScrapeUrl, showFFloat')
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (toUrlPiece)
import Web.Links (CookieProtectedRoutes (..), MonitorsRoutes' (..), urls)


data MonitorSection = SAlerts | SUptime | SCron | SStatusPages
  deriving stock (Eq)


-- | The monitor types, as section tabs on every monitors page.
monitorSections_ :: Projects.ProjectId -> MonitorSection -> Html ()
monitorSections_ pid current =
  navTabStrip_ NavLinks "Monitor types" "tabs-sm max-w-full overflow-x-auto" $ sections <&> \(s, icon, label, href) ->
    (span_ [class_ "inline-flex items-center gap-1.5"] (faSprite_ icon "regular" "size-3.5" >> label), href, s == current)
  where
    m = urls.monitors pid
    sections = [(SAlerts, "bell", "Alerts", m.listGet Nothing Nothing), (SUptime, "heart-pulse", "Uptime", m.uptimeGet), (SCron, "clock", "Cron", m.cronGet), (SStatusPages, "satellite-dish", "Status pages", m.statusPagesGet)]


requireEdit :: Projects.ProjectId -> ATAuthCtx (Projects.Session, Projects.Project)
requireEdit = ManageMembers.sessionAndProjectAtLeast ManageMembers.PEdit


shell :: Monad m => BWConfig -> Html () -> HtmlT m ()
shell bw = toHtml . PageCtx bw


-- | Every monitors page is titled Monitors; the section tabs say which kind.
sectionPage :: Projects.ProjectId -> MonitorSection -> ATAuthCtx BWConfig
sectionPage pid section = do
  (_, _, bw) <- mkPageCtx pid
  pure bw{pageTitle = "Monitors", menuItem = Just "Monitors", docsLink = Just "https://monoscopehq.com/docs/monitors/", navTabs = Just $ monitorSections_ pid section}


-- | The uptime dashboard, opened on the view a page summarises.
openDashboard_ :: Projects.ProjectId -> [(Text, Maybe Text)] -> Html ()
openDashboard_ pid params = a_ [class_ "btn btn-sm gap-1.5", href_ $ urls.uptimeDashboardRedirect pid params] $ faSprite_ "dashboard" "regular" "size-3.5" >> "Open as dashboard"


dashboardAction :: Projects.ProjectId -> Text -> BWConfig -> BWConfig
dashboardAction pid tab bw = bw{pageActions = Just $ openDashboard_ pid [("activeTabSlug", Just tab)]}


-- Day bars ------------------------------------------------------------------------------

-- | Days shown in bars and status pages, like hosted status pages.
barDays :: Integer
barDays = 90


-- | One bar per day, oldest first. A day with failures stands full height and in colour; a clean
-- day is shorter, so health never rests on hue alone. Narrow screens show the last 30 days.
dayBars_ :: UTCTime -> [(Day, (Int, Int))] -> Html ()
dayBars_ now days = div_ [class_ "relative z-10 flex h-8 items-end gap-[2px]", role_ "img", Aria.label_ summary] $ forM_ @[] [barDays - 1, barDays - 2 .. 0] \ago -> do
  let d = addDays (negate ago) today
      (total, failed) = fromMaybe (0, 0) $ lookup d days
      tip = dayText d <> " · " <> maybe "No data" (\p -> showPct p <> " up" <> bool "" (" · " <> show failed <> " failed") (failed > 0)) (Uptime.uptimePercent total failed)
  span_ [class_ $ barCls (Uptime.dayHealth total failed) (ago >= 30), term "data-tippy-content" tip] mempty
  where
    today = utctDay now
    dayText = toText . formatTime defaultTimeLocale "%b %e"
    bad = [d <> " (" <> show f <> " failed)" | (day, (_, f)) <- sortWith fst days, f > 0, let d = dayText day]
    summary = maybe "No checks in the last 90 days" (\p -> showPct p <> " uptime over 90 days" <> bool ("; failures on " <> T.intercalate ", " (take 10 bad)) "; no failures" (null bad)) (uncurry Uptime.uptimePercent $ sumCounts $ map snd days)
    barCls h older = (<> bool "" " max-md:hidden" older) case h of
      Uptime.NoData -> "h-3/4 flex-1 min-w-[2px] rounded-[2px] bg-fillWeak"
      Uptime.Healthy -> "h-3/4 flex-1 min-w-[2px] rounded-[2px] bg-fillSuccess-strong hover:opacity-70"
      Uptime.Degraded -> "h-full flex-1 min-w-[2px] rounded-[2px] bg-fillWarning-strong hover:opacity-70"
      Uptime.Outage -> "h-full flex-1 min-w-[2px] rounded-[2px] bg-fillError-strong hover:opacity-70"


-- | Bars with their scale and the period's uptime underneath.
barsWithScale_ :: UTCTime -> [(Day, (Int, Int))] -> Html ()
barsWithScale_ now days = div_ [class_ "space-y-1.5"] do
  dayBars_ now days
  div_ [class_ "flex items-center gap-3 text-xs text-textWeak"] do
    span_ [class_ "max-md:hidden"] $ show barDays <> " days ago"
    span_ [class_ "md:hidden"] "30 days ago"
    span_ [class_ "h-px flex-1 bg-strokeWeak"] mempty
    span_ [class_ "tabular-nums max-md:hidden"] $ uptimeLabel days >> " · 90 days"
    span_ [class_ "tabular-nums md:hidden"] $ uptimeLabel [x | x@(d, _) <- days, d >= addDays (-29) (utctDay now)] >> " · 30 days"
    span_ [class_ "h-px flex-1 bg-strokeWeak"] mempty
    span_ "Today"


uptimeLabel :: [(Day, (Int, Int))] -> Html ()
uptimeLabel = toHtml . maybe "No data" ((<> " uptime") . showPct) . uncurry Uptime.uptimePercent . sumCounts . map snd


sumCounts :: [(Int, Int)] -> (Int, Int)
sumCounts = foldl' (\(a, b) (c, d) -> (a + c, b + d)) (0, 0)


-- | >>> map showPct [100, 99.955, 99.5, 87.25]
-- ["100%","99.96%","99.50%","87.25%"]
showPct :: Double -> Text
showPct p = if p >= 100 then "100%" else show2 p <> "%"


-- | >>> map showMs [Nothing, Just 182.4, Just 1530] == ["—", "182 ms", "1.53 s"]
-- True
showMs :: Maybe Double -> Text
showMs = maybe "—" \ms -> if ms >= 1000 then show2 (ms / 1000) <> " s" else show (round ms :: Int) <> " ms"


-- | Buckets of one check, as (day, (probes, failures)).
daysOf :: UUID.UUID -> [Uptime.DayBucket] -> [(Day, (Int, Int))]
daysOf cid bs = [(b.day, (b.total, b.failed)) | b <- bs, b.checkId == cid]


-- | Per-day sums across checks, for the cumulative bar.
combinedDays :: [Uptime.DayBucket] -> [(Day, (Int, Int))]
combinedDays bs = Map.toList $ Map.fromListWith (\(a, b) (c, d) -> (a + c, b + d)) [(b.day, (b.total, b.failed)) | b <- bs]


-- Check state ---------------------------------------------------------------------------

data CheckState = Operational | Failing | Paused | Pending
  deriving stock (Eq)


uptimeState :: PromCfg.PrometheusScrapeConfig -> CheckState
uptimeState c
  | not c.enabled = Paused
  | isNothing c.lastStatus = Pending
  | c.consecutiveFailures >= Uptime.downtimeAfter = Failing
  | otherwise = Operational


cronState :: Monitors.CronMonitor -> UTCTime -> CheckState
cronState m now = case m.lastStatus of
  Just Monitors.CSError -> Failing
  _ | now > addUTCTime (fromIntegral $ m.intervalSecs + m.graceSecs) (fromMaybe m.createdAt m.lastCheckinAt) -> Failing
  Just Monitors.CSOk -> Operational
  Nothing -> Pending


stateTone :: CheckState -> (BadgeColor, Text)
stateTone = \case
  Operational -> (SuccessBadge, "Operational")
  Failing -> (ErrorBadge, "Down")
  Paused -> (NeutralBadge, "Paused")
  Pending -> (NeutralBadge, "Awaiting first result")


statePill_ :: CheckState -> Html ()
statePill_ = uncurry tonePill_ . stateTone


-- | The banner over a set of checks: what a reader needs first during an incident.
-- @detailM@ replaces the operational line, for readers who need no check counts.
overallBanner_ :: Text -> Maybe Text -> [CheckState] -> Html ()
overallBanner_ noun detailM states = div_ [class_ $ "flex items-center gap-3 rounded-xl border px-4 py-3 " <> cls] do
  faSprite_ icon "solid" $ "size-5 shrink-0 " <> iconCls
  div_ [class_ "min-w-0"] do
    p_ [class_ "font-semibold text-textStrong"] $ toHtml headline
    p_ [class_ "text-sm text-textWeak"] $ toHtml $ if down == 0 && active > 0 && pending == 0 then fromMaybe detail detailM else detail
  where
    down = length $ filter (== Failing) states
    active = length $ filter (`notElem` ([Paused, Pending] :: [CheckState])) states
    pending = length $ filter (== Pending) states
    (cls, iconCls, icon, headline, detail)
      | null states = ("border-strokeWeak bg-fillWeak", "text-iconNeutral", "circle-check", "Nothing monitored yet" :: Text, "Add a check to start tracking it." :: Text)
      | active == 0 = ("border-strokeWeak bg-fillWeak", "text-iconNeutral", "clock", "Waiting for first results", "No " <> noun <> " has reported yet")
      | down == 0 = ("border-strokeSuccess-weak bg-fillSuccess-weak", "text-iconSuccess", "circle-check", "All systems operational", if pending > 0 then countNoun pending noun <> " awaiting a first result" else show active <> " of " <> countNoun (length states) noun <> " reporting")
      | down == active = ("border-strokeError-weak bg-fillError-weak", "text-iconError", "circle-exclamation", "Major outage", "Every active " <> noun <> " is failing")
      | otherwise = ("border-strokeError-weak bg-fillError-weak", "text-iconError", "circle-exclamation", "Partial outage", show down <> " of " <> countNoun active noun <> " failing")


-- Uptime list ---------------------------------------------------------------------------

data UptimeOverview = UptimeOverview {pid :: Projects.ProjectId, now :: UTCTime, checks :: V.Vector PromCfg.PrometheusScrapeConfig, buckets :: [Uptime.DayBucket]}


newtype UptimeChecks = UptimeChecks (PageCtx UptimeOverview)


instance ToHtml UptimeChecks where
  toHtml (UptimeChecks (PageCtx bw o)) = shell bw $ div_ [class_ "mx-auto max-w-6xl space-y-5 p-4"] do
    overallBanner_ "check" Nothing $ map uptimeState $ V.toList o.checks
    unless (V.null o.checks) $ div_ [class_ "surface-raised space-y-4 rounded-xl p-4"] do
      div_ [class_ "grid grid-cols-2 gap-4 md:grid-cols-4"] do
        let states = map uptimeState $ V.toList o.checks
            recent = filter (\b -> b.day >= addDays (-29) (utctDay o.now)) o.buckets
            lastDay = filter (\b -> b.day == utctDay o.now) o.buckets
        statFigure_ "" [] "Checks" $ toHtml (show (V.length o.checks) :: Text)
        statFigure_ "" [] "Down now" $ toHtml (show (length $ filter (== Failing) states) :: Text)
        statFigure_ "" [] "Uptime, 30 days" $ toHtml $ maybe "—" showPct $ uncurry Uptime.uptimePercent $ sumCounts [(b.total, b.failed) | b <- recent]
        statFigure_ "" [] "Avg response, today" $ toHtml $ showMs $ weightedAvg lastDay
      div_ [class_ "space-y-1"] do
        sectionLabel_ "All checks"
        barsWithScale_ o.now $ combinedDays o.buckets
    uptimeChecksList_ o
  toHtmlRaw = toHtml


weightedAvg :: [Uptime.DayBucket] -> Maybe Double
weightedAvg bs = case [(fromIntegral (b.total - b.failed), a) | b <- bs, b.total > b.failed, Just a <- [b.avgMs]] of
  [] -> Nothing
  xs -> Just $ sum (map (uncurry (*)) xs) / sum (map fst xs)


-- | The add form and the check rows, swapped together: a successful add returns the form blank,
-- a rejected one swaps nothing so the form keeps what was typed.
uptimeChecksList_ :: UptimeOverview -> Html ()
uptimeChecksList_ o = div_ [id_ "uptime-checks", class_ "space-y-3", term "hx-target:inherited" "this", term "hx-swap:inherited" "outerHTML"] do
  panel_ (Collapsible $ V.null o.checks) def{icon = Just "plus", subtitle = Just "Requested on its interval; two failures in a row open a downtime issue"} "New uptime check"
    $ form_ [class_ "grid items-end gap-2 md:grid-cols-[1fr_2fr_auto_auto_auto]", hxPost_ (urls.monitors o.pid).uptimePost] do
      formField_ FieldSm def{placeholder = "Checkout API"} "Name" "name" True Nothing
      formField_ FieldSm def{inputType = "url", placeholder = "https://shop.example.com/health"} "URL" "url" True Nothing
      formSelectField_ FieldSm "Every" "interval" False $ options_ Nothing intervalOptions
      formField_ FieldSm def{inputType = "number", value = "200", extraAttrs = [min_ "100", max_ "599"]} "Expected status" "expectedStatus" False Nothing
      button_ [type_ "submit", class_ "btn btn-sm btn-primary"] "Add check"
  if V.null o.checks
    then emptyState_ def{size = ESCompact} "No uptime checks yet" "Add a URL above. Monoscope requests it on its interval and records every result."
    else ul_ [class_ "surface-raised divide-y divide-strokeWeak overflow-hidden rounded-xl"] $ forM_ o.checks \c -> do
      let days = daysOf c.id.unUUIDId o.buckets
          recent = sumCounts [n | (d, n) <- days, d >= addDays (-29) (utctDay o.now)]
      li_ [class_ "relative grid items-center gap-x-6 gap-y-2 px-4 py-3 hover:bg-fillWeak md:grid-cols-[minmax(0,22rem)_minmax(0,1fr)_auto]"] do
        div_ [class_ "min-w-0"] do
          a_ [href_ ((urls.monitors o.pid).uptimeCheckGet c.id Nothing), class_ "flex items-center gap-2 font-medium text-textStrong after:absolute after:inset-0 hover:text-textBrand"] do
            stateDot_ (uptimeState c)
            span_ [class_ "truncate"] $ toHtml c.name
          p_ [class_ "mt-0.5 truncate text-xs text-textWeak"] $ toHtml $ c.url <> " · every " <> intervalLabel c.scrapeIntervalSeconds
        dayBars_ o.now days
        dl_ [class_ "relative z-10 flex items-center gap-5 text-sm tabular-nums md:w-48 md:justify-end"] do
          div_ [class_ "md:text-right"] $ dt_ [class_ "text-2xs text-textWeak"] "30d uptime" >> dd_ [class_ "font-medium text-textStrong"] (toHtml $ maybe "—" showPct $ uncurry Uptime.uptimePercent recent)
          div_ [class_ "md:text-right"] $ dt_ [class_ "text-2xs text-textWeak"] "30d avg" >> dd_ [class_ "text-textStrong"] (toHtml $ showMs $ weightedAvg [b | b <- o.buckets, b.checkId == c.id.unUUIDId, b.day >= addDays (-29) (utctDay o.now)])


stateDot_ :: CheckState -> Html ()
stateDot_ s = span_ [class_ dotCls, Aria.label_ (snd $ stateTone s), role_ "img"] mempty
  where
    dotCls = case s of
      Operational -> "size-2 shrink-0 rounded-full bg-fillSuccess-strong"
      Failing -> "size-2 shrink-0 rounded-full bg-fillError-strong ring-4 ring-fillError-weak"
      Paused -> "size-2 shrink-0 rounded-full bg-fillStrong"
      Pending -> "size-2 shrink-0 rounded-full border border-strokeStrong"


intervalOptions :: [(Text, Text)]
intervalOptions = [("60", "1 min"), ("300", "5 min"), ("900", "15 min"), ("3600", "1 hour")]


-- | >>> map intervalLabel [60, 300, 3600, 86400, 90]
-- ["1 min","5 min","1 hour","1 day","90 s"]
intervalLabel :: Int -> Text
intervalLabel s
  | s `mod` 86400 == 0 = plural (s `div` 86400) "day"
  | s `mod` 3600 == 0 = plural (s `div` 3600) "hour"
  | s `mod` 60 == 0 = show (s `div` 60) <> " min"
  | otherwise = show s <> " s"
  where
    plural n w = show n <> " " <> w <> bool "" "s" (n /= 1)


uptimeOverview :: Projects.ProjectId -> ATAuthCtx UptimeOverview
uptimeOverview pid = do
  now <- Time.currentTime
  checks <- PromCfg.configsByProjectId pid PromCfg.CKUptime
  buckets <- Uptime.dailyBuckets (map (.id.unUUIDId) $ V.toList checks) now
  pure UptimeOverview{pid, now, checks, buckets}


uptimeChecksGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders UptimeChecks)
uptimeChecksGetH pid = do
  bw <- dashboardAction pid "overview" <$> sectionPage pid SUptime
  addRespHeaders . UptimeChecks . PageCtx bw =<< uptimeOverview pid


-- | Adding sends no regions; an empty list means the deployment's own.
data UptimeForm = UptimeForm {name :: Text, url :: Text, interval :: Maybe Int, expectedStatus :: Maybe Int, regions :: [Text]}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data UptimeRejection = UnnamedCheck | UnsafeUrl | UnservedRegion Text
  deriving stock (Eq, Show)


instance Refusal UptimeRejection where
  refusalMessage = \case
    UnnamedCheck -> "Name the check"
    UnsafeUrl -> "URL must be a public http:// or https:// endpoint"
    UnservedRegion r -> "No worker probes from " <> Uptime.regionLabel r <> " yet, so the check would never run there"


newtype UptimeList = UptimeList UptimeOverview
  deriving (Outcome) via Rendered UptimeList


instance ToHtml UptimeList where
  toHtml (UptimeList o) = toHtml $ uptimeChecksList_ o
  toHtmlRaw = toHtml


uptimeCheckPostH :: Projects.ProjectId -> UptimeForm -> ATAuthCtx (ActionResult UptimeRejection UptimeList)
uptimeCheckPostH pid form = act do
  _ <- requireEdit pid
  case validUptime form of
    Left r -> pure $ Left r
    Right v -> do
      void $ PromCfg.insertConfig pid PromCfg.CKUptime v.name v.url v.interval Nothing (AE.object []) (Just v.expected)
      addSuccessToast "Uptime check added" Nothing
      addTriggerEvent "monitorSaved" AE.Null
      Right . UptimeList <$> uptimeOverview pid


data ValidUptime = ValidUptime {name :: Text, url :: Text, interval :: Int, expected :: Int}


validUptime :: UptimeForm -> Either UptimeRejection ValidUptime
validUptime form
  | T.null name = Left UnnamedCheck
  | not (safeScrapeUrl url) = Left UnsafeUrl
  | otherwise = Right ValidUptime{name, url, interval = max 60 $ fromMaybe 60 form.interval, expected = clamp (100, 599) $ fromMaybe 200 form.expectedStatus}
  where
    name = T.strip form.name
    url = T.strip form.url


-- | Pause or resume; answered by reloading whichever page asked, list or dashboard.
uptimeCheckToggleH :: Projects.ProjectId -> PromCfg.PrometheusScrapeConfigId -> ATAuthCtx (RespHeaders (Done Redirected))
uptimeCheckToggleH pid cid = requireEdit pid >> PromCfg.toggleEnabled pid cid >> done (Redirected ((urls.monitors pid).uptimeCheckGet cid Nothing) "Check updated")


uptimeCheckDeleteH :: Projects.ProjectId -> PromCfg.PrometheusScrapeConfigId -> ATAuthCtx (RespHeaders (Done Redirected))
uptimeCheckDeleteH pid cid = requireEdit pid >> PromCfg.deleteConfig pid cid >> done (Redirected (urls.monitors pid).uptimeGet "Check deleted")


-- | A change, answered by sending the browser to the page that shows its result.
data Redirected = Redirected Text Text


instance Outcome Redirected where
  outcomeRedirect (Redirected url _) = Just url
  outcomeToast (Redirected _ msg) = Just $ successToast msg


-- Uptime dashboard ----------------------------------------------------------------------

data UptimeDashboard = UptimeDashboard
  { pid :: Projects.ProjectId
  , now :: UTCTime
  , since :: Text
  , check :: PromCfg.PrometheusScrapeConfig
  , incidents :: [Uptime.Incident]
  , recent :: [Uptime.CheckResult]
  , served :: [Text]
  }


newtype UptimeCheckPage = UptimeCheckPage (PageCtx UptimeDashboard)


-- | The template's views of this check (uptime.yaml, var-check fixed), then what only this page has.
instance ToHtml UptimeCheckPage where
  toHtml (UptimeCheckPage (PageCtx bw d)) = shell bw $ div_ [class_ "group/uc mx-auto max-w-6xl space-y-4 p-4"] do
    div_ [class_ "flex flex-wrap items-end justify-between gap-3"] do
      checkHeader_ (urls.monitors d.pid).uptimeGet "Uptime" (uptimeState d.check) d.check.name do
        a_ [href_ d.check.url, target_ "_blank", rel_ "noopener noreferrer", class_ "truncate font-mono text-textBrand hover:underline"] $ toHtml d.check.url
        span_ $ "every " <> toHtml (intervalLabel d.check.scrapeIntervalSeconds)
        span_ $ "expects " <> show (fromMaybe 200 d.check.expectedStatus)
        span_ $ toHtml $ maybe "not checked yet" (("checked " <>) . agoText d.now) d.check.lastScrapedAt
      div_ [class_ "flex items-center gap-2"] do
        TimePicker.liveDataControls_ Nothing (Just (d.since, "")) Nothing TimePicker.RefreshOnly
        openDashboard_ d.pid [("var-check", Just d.check.id.toText), ("since", Just d.since)]
    div_ [class_ "flex overflow-x-auto whitespace-nowrap border-b border-strokeWeak", [__|on click halt the event's bubbling|]] do
      detailTab_ "uc" "t-overview" "" True "Overview"
      detailTab_ "uc" "t-performance" "" False "Performance"
      detailTab_ "uc" "t-errors" "" False "Errors"
      detailTab_ "uc" "t-incidents" "" False $ "Incidents" >> countBadge_ d.incidents
      detailTab_ "uc" "t-log" "" False "Check log"
      detailTab_ "uc" "t-settings" "" False "Settings"
    let view tab = Widget.embeddedWidgets_ d.pid $ DashboardTemplates.templateTab "uptime.yaml" tab [("check", d.check.id.toText)]
    tabPanel_ "group-has-[.t-overview:checked]/uc:block" "uc-overview" $ view "Overview"
    tabPanel_ "group-has-[.t-performance:checked]/uc:block" "uc-performance" $ view "Performance"
    tabPanel_ "group-has-[.t-errors:checked]/uc:block" "uc-errors" $ view "Errors"
    tabPanel_ "group-has-[.t-incidents:checked]/uc:block space-y-4" "uc-incidents" $ incidentsTable_ d.now "No downtime in the last 90 days" d.incidents
    tabPanel_ "group-has-[.t-log:checked]/uc:block space-y-4" "uc-log" $ resultsTable_ d.now True d.recent
    tabPanel_ "group-has-[.t-settings:checked]/uc:block space-y-4" "uc-settings" $ uptimeSettings_ d
  toHtmlRaw = toHtml


checkHeader_ :: Text -> Text -> CheckState -> Text -> Html () -> Html ()
checkHeader_ backHref backLabel st title meta = header_ [class_ "space-y-2"] do
  a_ [href_ backHref, class_ "inline-flex items-center gap-1 text-xs text-textWeak hover:text-textBrand"] $ faSprite_ "arrow-left" "regular" "size-3" >> toHtml backLabel
  div_ [class_ "flex flex-wrap items-center gap-3"] do
    h1_ [class_ "text-xl font-semibold text-textStrong"] $ toHtml title
    statePill_ st
  div_ [class_ "flex flex-wrap items-center gap-x-4 gap-y-1 text-sm text-textWeak"] meta


-- | Red only while one is ongoing; resolved history is just a count.
countBadge_ :: [Uptime.Incident] -> Html ()
countBadge_ is = unless (null is) $ span_ [class_ $ if any (isNothing . (.recoveredAt)) is then "ml-1.5 rounded-full bg-fillError-weak px-1.5 text-2xs font-semibold text-textError tabular-nums" else "ml-1.5 rounded-full bg-fillWeak px-1.5 text-2xs font-semibold text-textWeak tabular-nums"] $ show (length is)


windowsGrid_ :: Text -> Uptime.WindowUptime -> Html ()
windowsGrid_ noun w = div_ [class_ "grid grid-cols-2 gap-3 md:grid-cols-4"] $ forM_ @[] [("Last 24 hours", w.day), ("Last 7 days", w.week), ("Last 30 days", w.month), ("Last 90 days", w.quarter)] \(label :: Text, (total, failed)) ->
  div_ [class_ "surface-raised rounded-xl p-4"] do
    statFigure_ "" [] label $ toHtml $ maybe "—" showPct $ Uptime.uptimePercent total failed
    p_ [class_ "mt-2 text-xs text-textWeak tabular-nums"] $ toHtml $ if total == 0 then "None yet" else countNoun total noun <> " · " <> show failed <> " failed"


show2 :: Double -> Text
show2 = showFFloat' 2


monthsTable_ :: [(Day, Int, Int)] -> Html ()
monthsTable_ [] = p_ [class_ "text-sm text-textWeak"] "No history yet"
monthsTable_ ms = table_ [class_ "w-full text-sm"] $ tbody_ $ forM_ ms \(m, total, failed) -> tr_ [class_ "border-b border-strokeWeak last:border-0"] do
  td_ [class_ "py-1.5 text-textStrong"] $ toHtml $ formatTime defaultTimeLocale "%B %Y" m
  td_ [class_ "py-1.5 text-right font-medium tabular-nums text-textStrong"] $ toHtml $ maybe "—" showPct $ Uptime.uptimePercent total failed
  td_ [class_ "py-1.5 pl-4 text-right text-xs text-textWeak tabular-nums"] $ show failed <> " failed"


incidentsTable_ :: UTCTime -> Text -> [Uptime.Incident] -> Html ()
incidentsTable_ _ emptyText [] = emptyState_ def{size = ESCompact} emptyText "Failure runs appear here with how long they lasted and why."
incidentsTable_ now _ is = div_ [class_ "surface-raised overflow-x-auto rounded-xl"] $ table_ [class_ "w-full text-sm"] do
  thead_ $ tr_ [class_ "text-left text-xs text-textWeak"] $ forM_ @[] ["Started", "Duration", "Failures", "Last error", "" :: Text] $ th_ [class_ "px-4 py-2 font-medium"] . toHtml
  tbody_ $ forM_ is \i -> tr_ [class_ "border-t border-strokeWeak"] do
    td_ [class_ "whitespace-nowrap px-4 py-2 tabular-nums text-textStrong"] $ toHtml $ formatTime defaultTimeLocale "%b %e, %H:%M UTC" i.startedAt
    td_ [class_ "whitespace-nowrap px-4 py-2 tabular-nums"] $ toHtml $ durationLabel $ diffUTCTime (fromMaybe now i.recoveredAt) i.startedAt
    td_ [class_ "px-4 py-2 tabular-nums"] $ show i.failures
    td_ [class_ "max-w-md truncate px-4 py-2 text-textWeak", term "data-tippy-content" (fromMaybe "" i.reason)] $ toHtml $ fromMaybe "" i.reason
    td_ [class_ "px-4 py-2 text-right"] $ maybe (tonePill_ ErrorBadge "Ongoing") (const $ tonePill_ SuccessBadge "Resolved") i.recoveredAt


-- | >>> map durationLabel [42, 300, 5400, 90000]
-- ["42s","5m","1h 30m","1d 1h"]
durationLabel :: Real a => a -> Text
durationLabel d
  | s < 60 = show s <> "s"
  | s < 3600 = show (s `div` 60) <> "m"
  | s < 86400 = show (s `div` 3600) <> "h" <> unit ((s `mod` 3600) `div` 60) "m"
  | otherwise = show (s `div` 86400) <> "d" <> unit ((s `mod` 86400) `div` 3600) "h"
  where
    s = round (toRational d) :: Int
    unit n u = if n == 0 then "" else " " <> show n <> u


resultsTable_ :: UTCTime -> Bool -> [Uptime.CheckResult] -> Html ()
resultsTable_ _ _ [] = emptyState_ def{size = ESCompact} "No results yet" "Each check or check-in is listed here as it happens."
resultsTable_ now probes rs = div_ [class_ "surface-raised overflow-x-auto rounded-xl"] $ table_ [class_ "w-full text-sm"] do
  thead_ $ tr_ [class_ "text-left text-xs text-textWeak"] $ forM_ @[] (["Time", "Result"] <> bool ["Detail"] ["Status", "Response", "Region"] probes) $ th_ [class_ "px-4 py-2 font-medium"] . toHtml @Text
  tbody_ $ forM_ rs \r -> tr_ [class_ "border-t border-strokeWeak"] do
    td_ [class_ "whitespace-nowrap px-4 py-2 tabular-nums text-textStrong", term "data-tippy-content" (toText $ formatTime defaultTimeLocale "%F %T UTC" r.checkedAt)] $ toHtml $ agoText now r.checkedAt
    td_ [class_ "px-4 py-2"] $ toneDot_ (bool ErrorBadge SuccessBadge r.ok) (bool "Failed" "OK" r.ok)
    if probes
      then do
        td_ [class_ "px-4 py-2 tabular-nums"] $ toHtml $ maybe "—" show r.statusCode
        td_ [class_ "px-4 py-2 tabular-nums"] $ toHtml $ showMs $ fromIntegral <$> r.durationMs
        td_ [class_ "px-4 py-2 text-textWeak"] $ toHtml r.region
      else td_ [class_ "px-4 py-2 text-textWeak"] $ toHtml $ fromMaybe "" r.message


uptimeSettings_ :: UptimeDashboard -> Html ()
uptimeSettings_ d = do
  form_ [class_ "surface-raised space-y-4 rounded-xl p-4", hxPost_ ((urls.monitors d.pid).uptimeCheckEdit d.check.id), hxSwap_ "none"] do
    div_ [class_ "grid gap-3 md:grid-cols-[1fr_2fr]"] do
      formField_ FieldSm def{value = d.check.name} "Name" "name" True Nothing
      formField_ FieldSm def{inputType = "url", value = d.check.url} "URL" "url" True Nothing
    div_ [class_ "grid gap-3 md:grid-cols-2"] do
      formSelectField_ FieldSm "Every" "interval" False $ options_ (Just $ show d.check.scrapeIntervalSeconds) intervalOptions
      formField_ FieldSm def{inputType = "number", value = show (fromMaybe 200 d.check.expectedStatus), extraAttrs = [min_ "100", max_ "599"]} "Expected status" "expectedStatus" False Nothing
    fieldset_ [class_ "space-y-2"] do
      legend_ [class_ "text-sm font-medium text-textStrong"] "Regions"
      p_ [class_ "text-xs text-textWeak"] "None selected runs from this deployment's region. A region can be picked once a worker probes from it."
      div_ [class_ "grid grid-cols-2 gap-2 md:grid-cols-4"] $ forM_ Uptime.regions \r -> do
        let chosen = r.code `V.elem` d.check.regions
            served = r.code `elem` d.served
        formCheckbox_ FieldSm (r.city <> " · " <> r.code <> bool " · no worker" "" served) "regions" ([value_ r.code] <> [checked_ | chosen] <> [disabled_ "" | not (served || chosen)])
    div_ [class_ "flex justify-end"] $ button_ [type_ "submit", class_ "btn btn-sm btn-primary"] "Save changes"
  section_ [class_ "surface-raised flex flex-wrap items-center justify-between gap-3 rounded-xl p-4"] do
    div_ do
      p_ [class_ "font-medium text-textStrong"] $ bool "Resume checking" "Pause checking" d.check.enabled
      p_ [class_ "text-sm text-textWeak"] "A paused check keeps its history and opens no issues."
    button_ [type_ "button", class_ "btn btn-sm", hxPost_ ((urls.monitors d.pid).uptimeToggle d.check.id), hxSwap_ "none"] $ bool "Resume" "Pause" d.check.enabled
  section_ [class_ "flex flex-wrap items-center justify-between gap-3 rounded-xl border border-strokeError-weak p-4"] do
    div_ do
      p_ [class_ "font-medium text-textStrong"] "Delete this check"
      p_ [class_ "text-sm text-textWeak"] "Its history stays for 400 days but is no longer shown."
    dangerAction_ "uc-delete" "trash" "Delete" ("Delete " <> d.check.name <> "?") "The check stops and is removed from status pages." [hxPost_ ((urls.monitors d.pid).uptimeDelete d.check.id), hxSwap_ "none"] "Delete check"


uptimeCheckGetH :: Projects.ProjectId -> PromCfg.PrometheusScrapeConfigId -> Maybe Text -> ATAuthCtx (RespHeaders UptimeCheckPage)
uptimeCheckGetH pid cid sinceM = do
  check <- PromCfg.getConfigByProject pid cid >>= maybe (throwError Servant.err404) pure
  now <- Time.currentTime
  region <- (.config.uptimeProbeRegion) <$> ask @AuthContext
  let uid = cid.unUUIDId
      since = fromMaybe "7D" sinceM
  bw <- sectionPage pid SUptime
  incidents <- incidentsOf (UptimeCheck check) <$> Uptime.incidents [uid] (addUTCTime (-(90 * 86400)) now)
  served <- Uptime.servedRegions region now
  recent <- Uptime.recentResults uid 100
  addRespHeaders $ UptimeCheckPage $ PageCtx bw UptimeDashboard{..}


-- | A region is accepted only once a worker probes from it: with one shared lease per check, a
-- check limited to unserved regions would simply stop being probed.
uptimeCheckEditH :: Projects.ProjectId -> PromCfg.PrometheusScrapeConfigId -> UptimeForm -> ATAuthCtx (ActionResult UptimeRejection Redirected)
uptimeCheckEditH pid cid form = act do
  _ <- requireEdit pid
  here <- (.config.uptimeProbeRegion) <$> ask @AuthContext
  served <- Uptime.servedRegions here =<< Time.currentTime
  let regions = ordNub $ filter (`elem` map (.code) Uptime.regions) form.regions
  case (validUptime form, find (`notElem` served) regions) of
    (Left r, _) -> pure $ Left r
    (_, Just unserved) -> pure $ Left $ UnservedRegion unserved
    (Right v, Nothing) -> do
      void $ PromCfg.updateUptime pid cid v.name v.url v.interval v.expected regions
      pure $ Right $ Redirected ((urls.monitors pid).uptimeCheckGet cid Nothing) "Check saved"


-- Cron ----------------------------------------------------------------------------------

data CronOverview = CronOverview {pid :: Projects.ProjectId, now :: UTCTime, monitors :: [Monitors.CronMonitor], buckets :: [Uptime.DayBucket]}


newtype CronMonitors = CronMonitors (PageCtx CronOverview)


instance ToHtml CronMonitors where
  toHtml (CronMonitors (PageCtx bw o)) = shell bw $ div_ [class_ "mx-auto max-w-6xl space-y-5 p-4"] do
    overallBanner_ "monitor" Nothing $ (`cronState` o.now) <$> o.monitors
    unless (null o.monitors) $ panel_ Raised def "All cron monitors" $ barsWithScale_ o.now $ combinedDays o.buckets
    cronMonitorsList_ o
  toHtmlRaw = toHtml


cronMonitorsList_ :: CronOverview -> Html ()
cronMonitorsList_ o = div_ [id_ "cron-monitors", class_ "space-y-3", term "hx-target:inherited" "this", term "hx-swap:inherited" "outerHTML"] do
  panel_ (Collapsible $ null o.monitors) def{icon = Just "plus", subtitle = Just "A missed window or an error run opens a cron issue"} "New cron monitor"
    $ form_ [class_ "grid items-end gap-2 md:grid-cols-[1fr_1fr_auto_auto_auto]", hxPost_ (urls.monitors o.pid).cronPost] do
      -- `-` last: browsers compile `pattern` with the v flag, where a bare `-` mid-class is a syntax error.
      formField_ FieldSm def{placeholder = "nightly-billing", extraAttrs = [pattern_ "[a-z0-9_.\\-]+"]} "Slug" "slug" True Nothing
      formField_ FieldSm def{placeholder = "Nightly billing run"} "Name" "name" True Nothing
      formSelectField_ FieldSm "Runs every" "interval" False $ options_ Nothing cronIntervals
      formSelectField_ FieldSm "Grace" "grace" False $ options_ Nothing graceOptions
      button_ [type_ "submit", class_ "btn btn-sm btn-primary"] "Add monitor"
  if null o.monitors
    then emptyState_ def{size = ESCompact} "No cron monitors yet" "Add one above, then send a check-in from the job."
    else ul_ [class_ "surface-raised divide-y divide-strokeWeak overflow-hidden rounded-xl"] $ forM_ o.monitors \m -> do
      let days = daysOf m.id o.buckets
      li_ [class_ "relative grid items-center gap-x-6 gap-y-2 px-4 py-3 hover:bg-fillWeak md:grid-cols-[minmax(0,22rem)_minmax(0,1fr)_auto]"] do
        div_ [class_ "min-w-0"] do
          a_ [href_ ((urls.monitors o.pid).cronMonitorGet m.id), class_ "flex items-center gap-2 font-medium text-textStrong after:absolute after:inset-0 hover:text-textBrand"] do
            stateDot_ (cronState m o.now)
            span_ [class_ "truncate"] $ toHtml m.name
          p_ [class_ "mt-0.5 truncate font-mono text-xs text-textWeak"] $ toHtml $ m.slug <> " · every " <> intervalLabel m.intervalSecs
        dayBars_ o.now days
        div_ [class_ "relative z-10 text-sm tabular-nums text-textWeak md:w-48 md:text-right"] $ toHtml $ maybe "No check-in yet" (("Last run " <>) . agoText o.now) m.lastCheckinAt


cronIntervals, graceOptions :: [(Text, Text)]
cronIntervals = [("300", "5 min"), ("900", "15 min"), ("3600", "1 hour"), ("21600", "6 hours"), ("86400", "1 day"), ("604800", "1 week")]
graceOptions = [("300", "5 min"), ("900", "15 min"), ("3600", "1 hour")]


cronOverview :: Projects.ProjectId -> ATAuthCtx CronOverview
cronOverview pid = do
  now <- Time.currentTime
  monitors <- Monitors.cronMonitorsByProject pid
  buckets <- Uptime.dailyBuckets (map (.id) monitors) now
  pure CronOverview{pid, now, monitors, buckets}


cronMonitorsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders CronMonitors)
cronMonitorsGetH pid = do
  bw <- dashboardAction pid "cron" <$> sectionPage pid SCron
  addRespHeaders . CronMonitors . PageCtx bw =<< cronOverview pid


data CronForm = CronForm {slug :: Text, name :: Text, interval :: Int, grace :: Int}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data CronRejection = MissingSlugOrName | InvalidSlug | DuplicateSlug Text
  deriving stock (Eq, Show)


instance Refusal CronRejection where
  refusalMessage = \case
    MissingSlugOrName -> "Slug and name are required"
    InvalidSlug -> "Slug may only contain a-z, 0-9, _, . and -"
    DuplicateSlug slug -> "A monitor with slug " <> slug <> " already exists"


newtype CronList = CronList CronOverview
  deriving (Outcome) via Rendered CronList


instance ToHtml CronList where
  toHtml (CronList o) = toHtml $ cronMonitorsList_ o
  toHtmlRaw = toHtml


cronMonitorPostH :: Projects.ProjectId -> CronForm -> ATAuthCtx (ActionResult CronRejection CronList)
cronMonitorPostH pid form = act do
  _ <- requireEdit pid
  let slug = T.toLower (T.strip form.slug)
      name = T.strip form.name
  if
    | T.null slug || T.null name -> pure $ Left MissingSlugOrName
    | not (T.all (\c -> isAsciiLower c || isDigit c || c `elem` ("_.-" :: String)) slug) -> pure $ Left InvalidSlug
    | otherwise ->
        (Monitors.insertCronMonitor pid slug name (max 60 form.interval) (max 60 form.grace) =<< Time.currentTime) >>= \case
          0 -> pure $ Left $ DuplicateSlug slug
          _ -> addSuccessToast "Cron monitor added" Nothing >> Right . CronList <$> cronOverview pid


cronMonitorDeleteH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders (Done Redirected))
cronMonitorDeleteH pid mid = requireEdit pid >> Monitors.deleteCronMonitor pid mid >> done (Redirected (urls.monitors pid).cronGet "Monitor deleted")


data CronDashboard = CronDashboard
  { pid :: Projects.ProjectId
  , now :: UTCTime
  , monitor :: Monitors.CronMonitor
  , days :: [(Day, (Int, Int))]
  , windows :: Uptime.WindowUptime
  , incidents :: [Uptime.Incident]
  , recent :: [Uptime.CheckResult]
  , ingestUrl :: Text
  }


newtype CronMonitorPage = CronMonitorPage (PageCtx CronDashboard)


instance ToHtml CronMonitorPage where
  toHtml (CronMonitorPage (PageCtx bw d)) = shell bw $ div_ [class_ "group/cm mx-auto max-w-6xl space-y-4 p-4"] do
    let m = d.monitor
        nextDue = addUTCTime (fromIntegral $ m.intervalSecs + m.graceSecs) (fromMaybe m.createdAt m.lastCheckinAt)
    checkHeader_ (urls.monitors d.pid).cronGet "Cron" (cronState m d.now) m.name do
      span_ [class_ "font-mono"] $ toHtml m.slug
      span_ $ "every " <> toHtml (intervalLabel m.intervalSecs) <> ", " <> toHtml (intervalLabel m.graceSecs) <> " grace"
      span_ $ toHtml $ maybe "no check-in yet" (("last run " <>) . agoText d.now) m.lastCheckinAt
    div_ [class_ "flex overflow-x-auto whitespace-nowrap border-b border-strokeWeak", [__|on click halt the event's bubbling|]] do
      detailTab_ "cm" "t-overview" "" True "Overview"
      detailTab_ "cm" "t-runs" "" False "Check-ins"
      detailTab_ "cm" "t-incidents" "" False $ "Incidents" >> countBadge_ d.incidents
      detailTab_ "cm" "t-setup" "" False "Setup"
      detailTab_ "cm" "t-settings" "" False "Settings"
    tabPanel_ "group-has-[.t-overview:checked]/cm:block space-y-4" "cm-overview" do
      windowsGrid_ "run" d.windows
      panel_ Raised def "Daily runs" $ barsWithScale_ d.now d.days
      panel_ Raised def "Schedule" $ factGrid_ "grid-cols-2 md:grid-cols-3" [("Last run", maybe "—" (agoText d.now) m.lastCheckinAt), ("Last result", maybe "—" (bool "Error" "OK" . (== Monitors.CSOk)) m.lastStatus), ("Next run due by", toText $ formatTime defaultTimeLocale "%b %e, %H:%M UTC" nextDue)]
    tabPanel_ "group-has-[.t-runs:checked]/cm:block" "cm-runs" $ resultsTable_ d.now False d.recent
    tabPanel_ "group-has-[.t-incidents:checked]/cm:block" "cm-incidents" $ incidentsTable_ d.now "No missed or failed runs in the last 90 days" d.incidents
    tabPanel_ "group-has-[.t-setup:checked]/cm:block space-y-3" "cm-setup" $ panel_ Raised def "Send a check-in when the job finishes" do
      p_ [class_ "text-sm text-textWeak"] do
        "Emit a span or log named "
        code_ "cron.checkin"
        " with "
        code_ "monitor.slug"
        " and "
        code_ "monitor.status"
        " (ok or error) through any OpenTelemetry SDK, or post one to the OTLP endpoint:"
      copyField_ "cm-otlp" d.ingestUrl
      callout_ BrandBadge Nothing "A run that reports error opens a failed issue at once. No check-in within the interval plus grace opens a missed issue; the next ok resolves it."
    tabPanel_ "group-has-[.t-settings:checked]/cm:block space-y-4" "cm-settings" do
      form_ [class_ "surface-raised grid items-end gap-3 rounded-xl p-4 md:grid-cols-[1fr_auto_auto_auto]", hxPost_ ((urls.monitors d.pid).cronMonitorEdit m.id), hxSwap_ "none"] do
        formField_ FieldSm def{value = m.name} "Name" "name" True Nothing
        formSelectField_ FieldSm "Runs every" "interval" False $ options_ (Just $ show m.intervalSecs) cronIntervals
        formSelectField_ FieldSm "Grace" "grace" False $ options_ (Just $ show m.graceSecs) graceOptions
        button_ [type_ "submit", class_ "btn btn-sm btn-primary"] "Save changes"
      section_ [class_ "flex flex-wrap items-center justify-between gap-3 rounded-xl border border-strokeError-weak p-4"] do
        div_ do
          p_ [class_ "font-medium text-textStrong"] "Delete this monitor"
          p_ [class_ "text-sm text-textWeak"] "Check-ins with its slug are then ignored."
        dangerAction_ "cm-delete" "trash" "Delete" ("Delete " <> m.name <> "?") "The monitor is removed from status pages." [hxPost_ ((urls.monitors d.pid).cronDelete m.id), hxSwap_ "none"] "Delete monitor"
  toHtmlRaw = toHtml


cronMonitorGetH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders CronMonitorPage)
cronMonitorGetH pid mid = do
  monitor <- Monitors.cronMonitorById pid mid >>= maybe (throwError Servant.err404) pure
  now <- Time.currentTime
  host <- (.config.hostUrl) <$> ask @AuthContext
  bw <- sectionPage pid SCron
  days <- daysOf mid <$> Uptime.dailyBuckets [mid] now
  windows <- Uptime.windowUptime mid now
  incidents <- incidentsOf (CronCheck monitor) <$> Uptime.incidents [mid] (addUTCTime (-(90 * 86400)) now)
  recent <- Uptime.recentResults mid 100
  let ingestUrl = hostPath host "/v1/traces"
  addRespHeaders $ CronMonitorPage $ PageCtx bw CronDashboard{..}


data CronEditForm = CronEditForm {name :: Text, interval :: Int, grace :: Int}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


cronMonitorEditH :: Projects.ProjectId -> UUID.UUID -> CronEditForm -> ATAuthCtx (ActionResult CronRejection Redirected)
cronMonitorEditH pid mid form = act do
  _ <- requireEdit pid
  if T.null (T.strip form.name)
    then pure $ Left MissingSlugOrName
    else Monitors.updateCronMonitor pid mid (T.strip form.name) (max 60 form.interval) (max 60 form.grace) $> Right (Redirected ((urls.monitors pid).cronMonitorGet mid) "Monitor saved")


-- Status pages --------------------------------------------------------------------------

-- | What a status page can list.
data Check = UptimeCheck PromCfg.PrometheusScrapeConfig | CronCheck Monitors.CronMonitor


checkId :: Check -> UUID.UUID
checkId = \case UptimeCheck c -> c.id.unUUIDId; CronCheck m -> m.id


checkName :: Check -> Text
checkName = \case UptimeCheck c -> c.name; CronCheck m -> m.name


checkState :: UTCTime -> Check -> CheckState
checkState now = \case UptimeCheck c -> uptimeState c; CronCheck m -> cronState m now


-- | Uptime counts downtime from 'Uptime.downtimeAfter' failures in a row, matching when it opens an
-- issue; one missed or failed cron run is already an incident.
minFailures :: Check -> Int
minFailures = \case UptimeCheck _ -> Uptime.downtimeAfter; CronCheck _ -> 1


projectChecks :: DB es => Projects.ProjectId -> Eff es [Check]
projectChecks pid = do
  uptime <- PromCfg.configsByProjectId pid PromCfg.CKUptime
  crons <- Monitors.cronMonitorsByProject pid
  pure $ map UptimeCheck (V.toList uptime) <> map CronCheck crons


-- | The checks a page names, in the page's order.
pageChecks :: Uptime.StatusPage -> [Check] -> [Check]
pageChecks page checks = mapMaybe (\i -> find ((== i) . checkId) checks) $ V.toList page.checkIds


data Component = Component {name :: Text, state :: CheckState, days :: [(Day, (Int, Int))], avgMs :: Maybe Double, months :: [(Day, Int, Int)], byRegion :: [Uptime.RegionStats]}


-- | @refresh@ is the board's own URL, polled so an open page follows an incident live.
data StatusBoard = StatusBoard {title :: Text, description :: Text, now :: UTCTime, refresh :: Text, components :: [Component], incidents :: [(Text, Text, Uptime.Incident)]}


-- | The page itself, shared by the public URL and the in-app view.
statusBoard_ :: StatusBoard -> Html ()
statusBoard_ b = div_ [id_ "status-board", class_ "mx-auto max-w-3xl space-y-6 px-4 py-8", hxGet_ b.refresh, hxTrigger_ "every 120s", hxSelect_ "#status-board", hxSwap_ "outerHTML"] do
  header_ [class_ "space-y-1"] do
    h1_ [class_ "text-2xl font-semibold text-textStrong"] $ toHtml b.title
    unless (T.null b.description) $ p_ [class_ "text-textWeak"] $ toHtml b.description
  overallBanner_ "component" (Just "Every component is running normally") $ map (.state) b.components
  if null b.components
    then emptyState_ def{size = ESCompact} "No components" "Add checks to this status page to show them here."
    else ul_ [class_ "surface-raised divide-y divide-strokeWeak rounded-xl"] $ forM_ b.components \c -> li_ [class_ "space-y-3 p-4"] do
      div_ [class_ "flex items-center justify-between gap-3"] do
        span_ [class_ "flex min-w-0 items-baseline gap-2"] do
          span_ [class_ "truncate font-medium text-textStrong"] $ toHtml c.name
          whenJust c.avgMs \ms -> span_ [class_ "shrink-0 text-xs text-textWeak tabular-nums"] $ toHtml $ showMs (Just ms) <> " avg · 90 days"
        span_ [class_ "shrink-0 text-sm"] $ statePill_ c.state
      barsWithScale_ b.now c.days
      details_ [class_ "group/cd text-sm"] do
        summary_ [class_ "flex w-fit cursor-pointer list-none items-center gap-1 text-xs text-textWeak hover:text-textBrand"] do
          faSprite_ "chevron-right" "regular" "size-3 transition-transform group-open/cd:rotate-90"
          "History and locations"
        div_ [class_ "mt-3 grid gap-4 md:grid-cols-2"] do
          div_ $ sectionLabel_ "Monthly uptime" >> monthsTable_ c.months
          unless (null c.byRegion) $ div_ do
            sectionLabel_ "Locations, last 24 hours"
            ul_ [class_ "mt-1 divide-y divide-strokeWeak"] $ forM_ c.byRegion \r -> li_ [class_ "flex items-center justify-between gap-3 py-1.5 tabular-nums"] do
              span_ [class_ "text-textStrong"] $ toHtml $ Uptime.regionLabel r.region
              span_ [class_ "text-textWeak"] $ toHtml $ maybe "—" showPct (Uptime.uptimePercent r.total r.failed) <> " · " <> showMs r.avgMs
  section_ [class_ "space-y-3"] do
    h2_ [class_ "text-lg font-semibold text-textStrong"] "Past incidents"
    if null b.incidents
      then p_ [class_ "text-sm text-textWeak"] "No incidents in the last 14 days."
      else ul_ [class_ "space-y-2"] $ forM_ b.incidents \(name, reason, i) -> li_ [class_ "surface-raised flex flex-wrap items-center justify-between gap-2 rounded-xl px-4 py-3"] do
        div_ [class_ "min-w-0"] do
          p_ [class_ "font-medium text-textStrong"] $ toHtml name
          p_ [class_ "text-sm text-textWeak tabular-nums"] $ toHtml $ toText (formatTime defaultTimeLocale "%b %e, %H:%M UTC" i.startedAt) <> " · " <> durationLabel (diffUTCTime (fromMaybe b.now i.recoveredAt) i.startedAt) <> " · " <> reason
        maybe (tonePill_ ErrorBadge "Ongoing") (const $ tonePill_ SuccessBadge "Resolved") i.recoveredAt
  footer_ [class_ "border-t border-strokeWeak pt-4 text-center text-xs text-textWeak"] do
    "Updated " <> toHtml (formatTime defaultTimeLocale "%b %e, %H:%M UTC" b.now) <> " · Powered by "
    a_ [href_ "https://monoscope.tech", class_ "text-textBrand hover:underline"] "Monoscope"


-- | What a status page says went wrong: an uptime failure by its status code, never the client
-- error (it names the URL); a cron run by its own outcome.
publicReason :: Check -> Uptime.Incident -> Text
publicReason c i = case c of
  UptimeCheck _ -> maybe "Request failed" (("HTTP " <>) . show) i.statusCode
  CronCheck _ -> fromMaybe "Run failed" i.reason


-- | A check's failure runs long enough to count as incidents.
incidentsOf :: Check -> [Uptime.Incident] -> [Uptime.Incident]
incidentsOf c = filter \i -> i.checkId == checkId c && i.failures >= minFailures c


-- | A board of @checks@, in order, with their history: four queries whatever the page's size.
statusBoard :: (DB es, Time :> es) => Text -> Text -> Text -> [Check] -> Eff es StatusBoard
statusBoard refresh title description checks = do
  now <- Time.currentTime
  let ids = map checkId checks
  buckets <- Uptime.dailyBuckets ids now
  months <- Uptime.monthlyUptime ids now
  -- Cron outcomes are evaluated centrally, so only uptime checks have locations.
  byRegion <- Uptime.regionStats [checkId c | c@UptimeCheck{} <- checks] (addUTCTime (-86400) now)
  runs <- Uptime.incidents ids (addUTCTime (-(14 * 86400)) now)
  let component c = Component{name = checkName c, state = checkState now c, days = daysOf i buckets, avgMs = weightedAvg [b | b <- buckets, b.checkId == i], months = [(d, t, f) | (m, d, t, f) <- months, m == i], byRegion = filter ((== i) . (.checkId)) byRegion}
        where
          i = checkId c
  pure StatusBoard{title, description, now, refresh, components = map component checks, incidents = sortWith (\(_, _, i) -> Down i.startedAt) [(checkName c, publicReason c i, i) | c <- checks, i <- incidentsOf c runs]}


data StatusPagesData = StatusPagesData {pid :: Projects.ProjectId, now :: UTCTime, host :: Text, pages :: [Uptime.StatusPage], checks :: [Check], board :: StatusBoard}


newtype StatusPages = StatusPages (PageCtx StatusPagesData)


instance ToHtml StatusPages where
  toHtml (StatusPages (PageCtx bw d)) = shell bw do
    statusBoard_ d.board
    section_ [class_ "mx-auto max-w-3xl space-y-3 px-4 pb-8"] do
      h2_ [class_ "text-lg font-semibold text-textStrong"] "Published pages"
      p_ [class_ "text-sm text-textWeak"] "Share a chosen set of checks with 90 days of daily health and recent incidents. Public pages need no login; internal ones are visible only to this project."
      panel_ (Collapsible False) def{icon = Just "plus"} "New status page" $ statusPageForm_ (urls.monitors d.pid).statusPagePost d.host Nothing d.checks "Create status page"
      resourceList_ "status-pages" Nothing (emptyState_ def{size = ESCompact} "No status pages yet" "Create one to share live health outside the app.") $ d.pages <&> \p ->
        ResourceRow
          { health = case p.visibility of Uptime.VPublic -> (NeutralBadge, "globe", "Public"); Uptime.VInternal -> (NeutralBadge, "lock", "Internal")
          , title = a_ [href_ ((urls.monitors d.pid).statusPageGet p.id), class_ "truncate font-medium text-textStrong hover:text-textBrand"] $ toHtml p.title
          , details =
              [ if Failing `elem` map (checkState d.now) (pageChecks p d.checks) then tonePill_ ErrorBadge "Incident" else tonePill_ SuccessBadge "Operational"
              , toHtml $ countNoun (V.length p.checkIds) "component"
              , bool mempty (a_ [href_ (hostPath d.host ("/status/" <> p.slug)), target_ "_blank", class_ "text-textBrand hover:underline"] $ toHtml $ "/status/" <> p.slug) (p.visibility == Uptime.VPublic)
              ]
          , actions = dangerAction_ ("sp-delete-" <> UUID.toText p.id.unUUIDId) "trash" "Delete" ("Delete " <> p.title <> "?") (bool "Only this project's members could see it." ("/status/" <> p.slug <> " stops working for everyone.") (p.visibility == Uptime.VPublic)) [hxPost_ ((urls.monitors d.pid).statusPageDelete p.id), hxSwap_ "none"] "Delete page"
          }
  toHtmlRaw = toHtml


-- | Create or edit a status page; editing starts from @pageM@.
statusPageForm_ :: Text -> Text -> Maybe Uptime.StatusPage -> [Check] -> Text -> Html ()
statusPageForm_ action host pageM checks submit = form_ [class_ "space-y-4", hxPost_ action] do
  div_ [class_ "grid gap-3 md:grid-cols-2"] do
    formField_ FieldSm def{placeholder = "Acme status", value = foldMap (.title) pageM} "Title" "title" True Nothing
    formField_ FieldSm def{placeholder = "acme", value = foldMap (.slug) pageM, extraAttrs = [pattern_ "[a-z0-9\\-]+"]} "Slug" "slug" True $ Just $ p_ [class_ "text-xs text-textWeak"] $ toHtml $ "Public URL: " <> hostPath host "/status/<slug>"
  formField_ FieldSm def{placeholder = "Live status of Acme's API and jobs", value = foldMap (.description) pageM} "Description" "description" False Nothing
  fieldset_ [class_ "space-y-2"] do
    legend_ [class_ "text-sm font-medium text-textStrong"] "Visibility"
    div_ [class_ "flex flex-wrap gap-4"] $ forM_ @[] [(Uptime.VPublic, "Public, anyone with the link"), (Uptime.VInternal, "Internal, project members only")] \(v, label) ->
      label_ [class_ "flex items-center gap-2 text-sm"] $ input_ ([type_ "radio", name_ "visibility", value_ (toUrlPiece v), class_ "radio radio-sm"] <> [checked_ | v == maybe Uptime.VPublic (.visibility) pageM]) >> label
  fieldset_ [class_ "space-y-2"] do
    legend_ [class_ "text-sm font-medium text-textStrong"] "Components"
    if null checks
      then p_ [class_ "text-sm text-textWeak"] "Add an uptime check or cron monitor first."
      else div_ [class_ "grid gap-2 md:grid-cols-2"] $ forM_ checks \c ->
        formCheckbox_ FieldSm (checkName c <> case c of UptimeCheck _ -> " · uptime"; CronCheck _ -> " · cron") "checks" ([value_ (UUID.toText $ checkId c)] <> [checked_ | maybe True ((checkId c `V.elem`) . (.checkIds)) pageM])
  div_ [class_ "flex justify-end"] $ button_ [type_ "submit", class_ "btn btn-sm btn-primary"] $ toHtml submit


statusPagesGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders StatusPages)
statusPagesGetH pid = do
  bw <- sectionPage pid SStatusPages
  host <- (.config.hostUrl) <$> ask @AuthContext
  now <- Time.currentTime
  pages <- Uptime.statusPagesByProject pid
  checks <- projectChecks pid
  board <- projectBoard pid checks
  addRespHeaders $ StatusPages $ PageCtx bw StatusPagesData{..}


data StatusPageForm = StatusPageForm {title :: Text, slug :: Text, description :: Maybe Text, visibility :: Uptime.Visibility, checks :: [UUID.UUID]}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data StatusPageRejection = UntitledPage | InvalidPageSlug | TakenSlug Text
  deriving stock (Eq, Show)


instance Refusal StatusPageRejection where
  refusalMessage = \case
    UntitledPage -> "Give the page a title"
    InvalidPageSlug -> "Slug may only contain a-z, 0-9 and -"
    TakenSlug s -> "The URL /status/" <> s <> " is taken"


statusPagePostH :: Projects.ProjectId -> StatusPageForm -> ATAuthCtx (ActionResult StatusPageRejection Redirected)
statusPagePostH pid form = act do
  _ <- requireEdit pid
  validPage form & either (pure . Left) \(slug, title) ->
    Uptime.insertStatusPage pid slug title (maybe "" T.strip form.description) form.visibility form.checks <&> \case
      0 -> Left $ TakenSlug slug
      _ -> Right $ Redirected (urls.monitors pid).statusPagesGet "Status page created"


statusPageEditH :: Projects.ProjectId -> Uptime.StatusPageId -> StatusPageForm -> ATAuthCtx (ActionResult StatusPageRejection Redirected)
statusPageEditH pid sid form = act do
  _ <- requireEdit pid
  validPage form & either (pure . Left) \(slug, title) ->
    Uptime.updateStatusPage pid sid slug title (maybe "" T.strip form.description) form.visibility form.checks <&> \case
      0 -> Left $ TakenSlug slug
      _ -> Right $ Redirected ((urls.monitors pid).statusPageGet sid) "Status page saved"


validPage :: StatusPageForm -> Either StatusPageRejection (Text, Text)
validPage form
  | T.null title = Left UntitledPage
  | T.null slug || not (T.all (\c -> isAsciiLower c || isDigit c || c == '-') slug) = Left InvalidPageSlug
  | otherwise = Right (slug, title)
  where
    slug = T.toLower (T.strip form.slug)
    title = T.strip form.title


statusPageDeleteH :: Projects.ProjectId -> Uptime.StatusPageId -> ATAuthCtx (RespHeaders (Done Redirected))
statusPageDeleteH pid sid = requireEdit pid >> Uptime.deleteStatusPage pid sid >> done (Redirected (urls.monitors pid).statusPagesGet "Status page deleted")


-- | What a status page URL shows: nothing, the board to anyone, or the board with a member's edit form.
data StatusPageView
  = PageMissing BWConfig
  | PublicBoard BWConfig StatusBoard
  | MemberBoard BWConfig StatusBoard (Maybe StatusPageEdit)


-- | The edit form's data; the built-in board of every check has none.
data StatusPageEdit = StatusPageEdit {pid :: Projects.ProjectId, host :: Text, page :: Uptime.StatusPage, checks :: [Check]}


instance ToHtml StatusPageView where
  toHtml = \case
    PageMissing bw -> shell bw $ div_ [class_ "p-8"] $ emptyState_ def "Status page not found" "It may have been deleted, or it is not public."
    PublicBoard bw b -> shell bw $ statusBoard_ b
    MemberBoard bw b e -> shell bw do
      statusBoard_ b
      whenJust e \StatusPageEdit{pid, host, page, checks} -> div_ [class_ "mx-auto max-w-3xl px-4 pb-8"] $ panel_ (Collapsible False) def{icon = Just "pen-to-square"} "Edit this page" $ statusPageForm_ ((urls.monitors pid).statusPageEdit page.id) host (Just page) checks "Save changes"
  toHtmlRaw = toHtml


statusPageGetH :: Projects.ProjectId -> Uptime.StatusPageId -> ATAuthCtx (RespHeaders StatusPageView)
statusPageGetH pid sid = do
  page <- Uptime.statusPageById pid sid >>= maybe (throwError Servant.err404) pure
  bw <- sectionPage pid SStatusPages
  host <- (.config.hostUrl) <$> ask @AuthContext
  checks <- projectChecks pid
  board <- statusBoard ((urls.monitors pid).statusPageGet sid) page.title page.description (pageChecks page checks)
  addRespHeaders $ MemberBoard bw board $ Just StatusPageEdit{pid, host, page, checks}


-- | Every uptime check and cron monitor on one board: the project's own status at a glance.
statusAllGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders StatusPageView)
statusAllGetH pid = do
  bw <- sectionPage pid SStatusPages
  board <- projectBoard pid =<< projectChecks pid
  addRespHeaders $ MemberBoard bw board Nothing


-- | Every uptime check and cron monitor on one members-only board.
projectBoard :: (DB es, Time :> es) => Projects.ProjectId -> [Check] -> Eff es StatusBoard
projectBoard pid = statusBoard (urls.monitors pid).statusAllGet "Project status" "Every uptime check and cron monitor in this project, visible only to its members."


-- | The public page: only a page marked public is served; anything else reads as missing.
publicStatusPageGetH :: Text -> ATBaseCtx StatusPageView
publicStatusPageGetH slug = do
  cfg <- (.config) <$> ask @AuthContext
  let bw title = (def :: BWConfig){pageTitle = title, config = cfg}
  Uptime.statusPageBySlug slug >>= \case
    Just page | page.visibility == Uptime.VPublic -> PublicBoard (bw page.title) <$> (statusBoard ("/status/" <> slug) page.title page.description . pageChecks page =<< projectChecks page.projectId)
    _ -> pure $ PageMissing (bw "Status")
