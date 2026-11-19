-- | A project's Home: the first page after login. It answers "what needs me today?" from across
-- the platform — an AI ask box, the golden signals, today's new issues beside today's
-- improvements, what AI routines flagged, charts from starred dashboards, and a direct line to
-- the Monoscope team over Slack Connect.
module Pages.Home (homeGetH, HomeGet (..), HomeData (..), highlightDismissPostH, HighlightDismissed, slackConnectPostH, SlackConnectCard (..)) where

import Data.Aeson qualified as AE
import Data.CaseInsensitive qualified as CI
import Data.List (partition)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, zonedTimeToUTC)
import Effectful.Reader.Static (ask, asks)
import Effectful.Time qualified as Time
import GHC.Records (HasField (..))
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxIndicator_, hxPost_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Apis.Integrations qualified as Integrations
import Models.Apis.Issues qualified as Issues
import Models.Projects.DashboardTemplates (askTemplates, loadDashboardFromVM, templateList, templateTab)
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.Projects qualified as Projects
import Pages.AIThreads (promptField_, threadUrl)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx)
import Pages.Components (agoText)
import Pages.Dashboards (dashboardUrl)
import Pages.Issues (issueDetailUrl, issuePreview_, issuesListUrl, renderIssueTitle_)
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (SortOrder (..), UUIDId (..))
import Relude hiding (ask, asks)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging (logAttention)
import System.Types (ATAuthCtx, Outcome (..), RespHeaders, Toast (..), ToastLevel (..), addRespHeaders, done)
import System.Types qualified
import Text.Slugify (slugify)
import UnliftIO.Exception (tryAny)
import Utils (countNoun, faSprite_, navTabAttrs)
import Web.Links (CookieProtectedRoutes (..), urls)


newtype HomeGet = HomePage (PageCtx HomeData)


data HomeData = HomeData
  { pid :: Projects.ProjectId
  , firstName :: Text
  , now :: UTCTime
  , newIssues :: [Issues.IssueL]
  , newIssueCount :: Int
  , openIssueCount :: Int
  , resolved :: [Issues.ResolvedIssue]
  , highlights :: [Issues.HomeHighlight]
  , routines :: [Issues.ConversationSummary]
  , recentThreads :: [Issues.ConversationSummary]
  , glance :: [Widget.Widget]
  , starred :: [(Dashboards.DashboardVM, [Widget.Widget])]
  , slack :: Maybe SlackConnectCard
  -- ^ Absent on the demo project and on servers with no Slack bot (self-hosted), where no channel can exist.
  }


instance ToHtml HomeGet where
  toHtml (HomePage (PageCtx conf d)) = toHtml $ PageCtx conf $ home_ d
  toHtmlRaw = toHtml


homeGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders HomeGet)
homeGetH pid = do
  (sess, _, bw) <- mkPageCtx pid
  now <- Time.currentTime
  let dayAgo = addUTCTime (-86400) now
      inbox = Issues.defIssueFilters{Issues.ack = Issues.IsNull, Issues.archive = Issues.IsNull, Issues.spam = Issues.IsNull, Issues.hideLowSeverity = True}
  (newIssues, newIssueCount) <- Issues.selectIssues pid Issues.PIssueL inbox{Issues.timeRange = Just (dayAgo, now), Issues.period = "24h", Issues.limit = 6}
  (_, openIssueCount) <- Issues.selectIssues pid Issues.PIssue inbox{Issues.limit = 0}
  resolved <- Issues.resolvedIssuesSince pid dayAgo
  highlights <- Issues.liveHighlights pid
  templates <- templateList <$> askTemplates
  dashboards <- filter (isJust . (.starredSince)) <$> Dashboards.selectDashboardsSortedBy pid Desc Dashboards.DSUpdatedAt
  slackConnect <- Integrations.getSlackConnect pid
  slackConfigured <- asks @AuthContext $ not . T.null . (.config.slackBotToken)
  let glance = take 1 $ templateTab "_overview.yaml" "Overview" []
      -- A widget's id names its DOM elements, so one shown twice (a starred overview repeats the
      -- golden signals) loads into the first copy and leaves the second a skeleton forever.
      pick seen vm = let ws = take 4 [w | w <- mapMaybe (homeWidget vm) $ foldMap Dashboards.allWidgets $ loadDashboardFromVM templates vm, all (`S.notMember` seen) (domId w)] in (seen <> S.fromList (mapMaybe domId ws), (vm, ws))
      starred = take 2 $ filter (not . null . snd) $ snd $ mapAccumL pick (S.fromList $ mapMaybe domId $ foldMap flat glance) dashboards
      flat w = w : foldMap (concatMap flat) w.children
      domId w = w.id <|> (slugify <$> w.title)
      (routines, chats) = partition (.routineActive) bw.conversations
      email = T.toLower $ CI.original sess.user.email
  addRespHeaders
    $ HomePage
    $ PageCtx bw{pageTitle = "Home", menuItem = Just "Home"}
    $ HomeData{pid, firstName = sess.user.firstName, now, newIssues, newIssueCount, openIssueCount, resolved, highlights, routines, recentThreads = take 4 chats, glance, starred, slack = SlackConnectCard pid email slackConnect Nothing <$ guard (slackConfigured && pid /= Projects.demoProjectId)}
  where
    -- Charts that render on their own: a widget needing its dashboard's variables or constants would only show an error here.
    homeWidget vm w = do
      guard $ w.wType `elem` [Widget.WTTimeseries, Widget.WTTimeseriesLine, Widget.WTTimeseriesStat, Widget.WTStat]
      guard $ not $ any (\q -> any (`T.isInfixOf` q) ["{{var-", "{{const-"]) (catMaybes [w.query, w.sql])
      pure w{Widget._projectId = Just pid, Widget._dashboardId = Just vm.id.toText, Widget.standalone = Just True}


home_ :: HomeData -> Html ()
home_ d = div_ [class_ "w-full h-full overflow-y-auto c-scroll"] $ div_ [class_ "mx-auto max-w-5xl px-4 sm:px-8 pt-10 pb-16 flex flex-col gap-12"] do
  section_ [class_ "flex flex-col gap-5", Aria.label_ "Ask Monoscope AI"] do
    header_ [class_ "flex flex-col gap-1"] do
      p_ [class_ "text-3xl font-semibold tracking-tight text-textStrong text-balance"] $ toHtml $ "Hello" <> foldMap (", " <>) (guarded (not . T.null) d.firstName) <> "."
      p_ [class_ "text-sm text-textWeak"] $ toHtml $ formatTime defaultTimeLocale "%A, %-d %B" d.now
    askBox_ d
    statusBar_ d
  whenJust (mfilter (not . (.invited)) d.slack) slackConnectCard_
  let flagged kind = filter ((== kind) . (.kind)) d.highlights
  div_ [class_ "grid grid-cols-1 lg:grid-cols-2 gap-x-6 gap-y-10"] do
    column_ "Today's issues" (d.newIssueCount + length (flagged Issues.HKIssue)) (Just ("All issues", issuesListUrl d.pid)) do
      if null d.newIssues && null (flagged Issues.HKIssue)
        then quiet_ "circle-check" "Nothing new broke in the last 24 hours."
        else do
          forM_ (flagged Issues.HKIssue) $ highlightCard_ d.pid d.now
          forM_ d.newIssues $ issueCard_ d.pid d.now
    column_ "Today's improvements" (length d.resolved + length (flagged Issues.HKImprovement)) Nothing do
      if null d.resolved && null (flagged Issues.HKImprovement)
        then quiet_ "arrow-trend-up" "Resolved issues and AI-confirmed wins from the last 24 hours land here."
        else do
          forM_ (flagged Issues.HKImprovement) $ highlightCard_ d.pid d.now
          forM_ d.resolved $ resolvedCard_ d.pid d.now
  unless (null $ flagged Issues.HKInsight) $ section_ [class_ "flex flex-col gap-2", Aria.label_ "For you today"] do
    sectionHead_ "For you today" (Just $ length $ flagged Issues.HKInsight) $ Just ("AI routines", urls.aiRoutinesGet d.pid)
    div_ [class_ "flex flex-col"] $ forM_ (flagged Issues.HKInsight) $ feedRow_ d.pid d.now
  section_ [class_ "flex flex-col gap-3", Aria.label_ "Service health"] do
    sectionHead_ "Service health" Nothing $ Just ("Overview dashboard", urls.dashboardRedirectGet d.pid ([] :: [(Text, Maybe Text)]))
    Widget.embeddedWidgets_ d.pid d.glance
  forM_ d.starred \(vm, ws) -> section_ [class_ "flex flex-col gap-3", Aria.label_ vm.title] do
    sectionHead_ (faSprite_ "star" "solid" "w-3.5 h-3.5 text-iconWarning" >> toHtml vm.title) Nothing $ Just ("Open dashboard", dashboardUrl d.pid vm.id Nothing [])
    Widget.embeddedWidgets_ d.pid ws
  jumpBackIn_ d


askBox_ :: HomeData -> Html ()
askBox_ d = div_ [class_ "flex flex-col gap-2.5"] do
  form_ [id_ "home-ask-form", hxPost_ $ urls.aiThreadsPost d.pid, hxTarget_ "this", hxSwap_ "none", hxIndicator_ "#home-ask-loader"] do
    promptField_ "home-ask" "Ask about your system — “why is checkout slower today?”" [autofocus_]
    input_ [type_ "hidden", name_ "mode", value_ "chat"]
  div_ [class_ "flex flex-wrap gap-2"] $ forM_ suggestions \q ->
    button_ [type_ "button", class_ "rounded-full bg-fillWeak px-3 py-1.5 text-xs text-textWeak hover:text-textStrong hover:bg-fillHover active:scale-[0.96] transition-[color,background-color,scale] duration-100 cursor-pointer", data_ "q" q, [__|on click set #home-ask.value to @data-q then call #home-ask-form.requestSubmit()|]] $ toHtml q
  where
    suggestions =
      [ "What changed in the last 24 hours?"
      , "Which endpoints got slower today?"
      , "Summarize today's errors"
      , "Are any services close to their limits?"
      ]


-- | The day in five numbers, each a door to the list behind it.
statusBar_ :: HomeData -> Html ()
statusBar_ d = div_ [class_ "grid grid-cols-2 sm:grid-cols-5 gap-px overflow-hidden rounded-xl bg-strokeWeak shadow-[0_0_0_1px_var(--color-strokeWeak)]"] do
  stat_ (issuesListUrl d.pid) d.openIssueCount "Open issues" ""
  stat_ (issuesListUrl d.pid) d.newIssueCount "New today" (bool "" "text-textError" (d.newIssueCount > 0))
  stat_ (issuesListUrl d.pid) (length d.resolved) "Resolved today" (bool "" "text-textSuccess" (not $ null d.resolved))
  stat_ (urls.aiRoutinesGet d.pid) (length d.highlights) "Flagged by AI" ""
  case length d.routines of
    0 -> a_ ([href_ $ urls.aiRoutinesGet d.pid, class_ "col-span-2 sm:col-span-1 flex flex-col justify-center gap-0.5 bg-bgRaised px-4 py-3 hover:bg-fillWeak transition-colors"] <> navTabAttrs) do
      span_ [class_ "inline-flex items-center gap-1.5 text-sm font-medium text-textBrand"] $ faSprite_ "sparkles" "regular" "w-3.5 h-3.5" >> "Set up a routine"
      span_ [class_ "text-xs text-textWeak"] "AI that watches for you"
    n -> stat_ (urls.aiRoutinesGet d.pid) n (bool "AI routines watching" "AI routine watching" (n == 1)) ""
  where
    stat_ :: Text -> Int -> Html () -> Text -> Html ()
    stat_ href n label tone = a_ ([href_ href, class_ "flex flex-col gap-0.5 bg-bgRaised px-4 py-3 hover:bg-fillWeak transition-colors"] <> navTabAttrs) do
      span_ [class_ $ "text-xl font-semibold tabular-nums text-textStrong " <> tone] $ toHtml (show @Text n)
      span_ [class_ "text-xs text-textWeak"] label


sectionHead_ :: Html () -> Maybe Int -> Maybe (Text, Text) -> Html ()
sectionHead_ title countM linkM = div_ [class_ "flex items-center gap-2"] do
  h2_ [class_ "inline-flex items-center gap-1.5 text-base font-semibold text-textStrong"] title
  whenJust countM $ span_ [class_ "rounded-md bg-fillWeak px-1.5 text-xs font-medium tabular-nums text-textWeak"] . show
  whenJust linkM \(label, href) -> a_ ([href_ href, class_ "ml-auto inline-flex items-center gap-1 text-xs text-textWeak hover:text-textBrand"] <> navTabAttrs) do
    toHtml label
    faSprite_ "arrow-right" "regular" "w-3 h-3"


column_ :: Html () -> Int -> Maybe (Text, Text) -> Html () -> Html ()
column_ title n linkM body = section_ [class_ "flex flex-col gap-3 min-w-0"] do
  sectionHead_ title (Just n) linkM
  div_ [class_ "flex flex-col gap-3"] body


quiet_ :: Text -> Text -> Html ()
quiet_ icon msg = div_ [class_ "flex items-center gap-3 rounded-2xl border border-dashed border-strokeWeak px-4 py-5 text-sm text-textWeak"] do
  span_ [class_ "flex size-8 shrink-0 items-center justify-center rounded-full bg-fillWeak"] $ faSprite_ icon "regular" "w-4 h-4"
  toHtml msg


cardCls :: Text
cardCls = "group/card relative flex flex-col gap-2.5 min-w-0 rounded-2xl surface-raised p-4 shadow-[0_0_0_1px_var(--color-strokeWeak)] hover:shadow-[0_0_0_1px_var(--color-strokeStrong)] transition-shadow"


-- | A card's identity row: a toned dot and id (the dot's meaning is repeated in text for screen readers), then context.
cardMeta_ :: Text -> Text -> Text -> Html () -> Html ()
cardMeta_ dot dotLabel ident context = div_ [class_ "flex items-center gap-2 min-w-0 text-xs text-textWeak"] do
  span_ [class_ "inline-flex shrink-0 items-center gap-1.5 rounded-md bg-fillWeak px-1.5 py-0.5 font-medium tabular-nums text-textStrong"] do
    span_ [class_ $ "size-1.5 rounded-full " <> dot] ""
    span_ [class_ "sr-only"] $ toHtml dotLabel
    toHtml ident
  context


issueCard_ :: Projects.ProjectId -> UTCTime -> Issues.IssueL -> Html ()
issueCard_ pid now i = a_ ([href_ $ issueDetailUrl pid i.base.id, class_ cardCls] <> navTabAttrs) do
  let (dot, label) = case i.base.severity of
        Issues.Critical -> ("bg-fillError-strong", "Critical")
        Issues.Warning -> ("bg-fillWarning-strong", "Warning")
        Issues.Info -> ("bg-fillInformation-strong", "Info")
        Issues.Low -> ("bg-fillNeutral-strong", "Low")
  cardMeta_ dot label ("#" <> show i.base.seqNum) do
    span_ [class_ "font-medium text-textStrong"] $ toHtml label
    span_ [class_ "ml-auto shrink-0"] $ toHtml $ agoText now $ zonedTimeToUTC i.base.createdAt
  p_ [class_ "text-[15px] leading-snug font-medium text-textStrong line-clamp-2 break-words"] $ renderIssueTitle_ i
  issuePreview_ Nothing i
  let buckets = toList i.activityBuckets
  unless (all (== 0) buckets) $ activityStrip_ (bool "bg-fillWarning-strong" "bg-fillError-strong" (i.base.severity == Issues.Critical)) buckets
  when (i.eventCount > 0) $ p_ [class_ "text-xs text-textWeak tabular-nums"] $ toHtml $ countNoun i.eventCount "event" <> bool "" (" · " <> countNoun i.usersCount "user") (i.usersCount > 0)


-- | Twenty-four hourly bars: when it started, and whether it is still going.
activityStrip_ :: Text -> [Int] -> Html ()
activityStrip_ fill buckets = div_ [class_ "flex h-6 items-end gap-0.5", role_ "img", Aria.label_ "Events per hour over the last 24 hours"] $ forM_ buckets \n ->
  span_ [class_ $ "flex-1 rounded-[2px] " <> bool "bg-fillWeak" fill (n > 0), style_ $ "height:" <> show @Text (bool 12 (max 25 $ n * 100 `div` peak) (n > 0)) <> "%"] ""
  where
    peak = foldr max 1 buckets


resolvedCard_ :: Projects.ProjectId -> UTCTime -> Issues.ResolvedIssue -> Html ()
resolvedCard_ pid now r = a_ ([href_ $ issueDetailUrl pid r.id, class_ cardCls] <> navTabAttrs) do
  cardMeta_ "bg-fillSuccess-strong" "Resolved" ("#" <> show r.seqNum) do
    span_ [class_ "inline-flex items-center gap-1 text-textSuccess"] $ faSprite_ "circle-check" "regular" "w-3 h-3" >> bool "Resolved" "Auto-resolved" (r.event == Issues.IEAutoResolved)
    whenJust r.service $ span_ [class_ "truncate"] . toHtml
    span_ [class_ "ml-auto shrink-0"] $ toHtml $ agoText now r.resolvedAt
  p_ [class_ "text-[15px] leading-snug font-medium text-textStrong line-clamp-2 break-words"] $ toHtml r.title


-- | An agent-flagged issue or win, in the same card as the issues beside it.
highlightCard_ :: Projects.ProjectId -> UTCTime -> Issues.HomeHighlight -> Html ()
highlightCard_ pid now h = div_ [id_ $ "highlight-" <> h.id.toText, class_ cardCls] do
  let (dot, label) = case h.kind of
        Issues.HKIssue -> ("bg-fillError-strong", "Flagged issue")
        Issues.HKImprovement -> ("bg-fillSuccess-strong", "Improvement")
        Issues.HKInsight -> ("bg-fillInformation-strong", "Insight")
  cardMeta_ dot label "AI" do
    highlightSource_ pid h
    span_ [class_ "ml-auto shrink-0 pe-6"] $ toHtml $ agoText now h.updatedAt
  highlightTitle_ "text-[15px] leading-snug" h
  unless (T.null h.detail) $ p_ [class_ "text-sm text-textWeak line-clamp-3 text-pretty"] $ toHtml h.detail
  dismiss_ pid h


-- | An insight as a quiet feed row rather than a card: context, not an alarm.
feedRow_ :: Projects.ProjectId -> UTCTime -> Issues.HomeHighlight -> Html ()
feedRow_ pid now h = div_ [id_ $ "highlight-" <> h.id.toText, class_ "group/card relative -mx-3 flex gap-3 rounded-xl px-3 py-3 hover:bg-fillWeak transition-colors"] do
  span_ [class_ "flex size-8 shrink-0 items-center justify-center rounded-lg bg-fillInformation-weak"] $ faSprite_ "lightbulb" "regular" "w-4 h-4 text-iconInformation"
  div_ [class_ "min-w-0 flex-1 flex flex-col gap-0.5 pe-8"] do
    highlightTitle_ "text-sm" h
    unless (T.null h.detail) $ p_ [class_ "text-sm text-textWeak line-clamp-2 text-pretty"] $ toHtml h.detail
    div_ [class_ "flex items-center gap-1.5 text-xs text-textWeak"] do
      highlightSource_ pid h
      span_ [class_ "opacity-40"] "·"
      toHtml $ agoText now h.updatedAt
  dismiss_ pid h


highlightTitle_ :: Text -> Issues.HomeHighlight -> Html ()
highlightTitle_ size h = maybe title (\l -> a_ [href_ l, class_ "hover:underline underline-offset-2"] title) h.link
  where
    title = p_ [class_ $ size <> " font-medium text-textStrong line-clamp-2 break-words"] $ toHtml h.title


highlightSource_ :: Projects.ProjectId -> Issues.HomeHighlight -> Html ()
highlightSource_ pid h = span_ [class_ "inline-flex min-w-0 items-center gap-1"] do
  faSprite_ "sparkles" "regular" "w-3 h-3 shrink-0"
  maybe (span_ [class_ "truncate"] $ toHtml h.source) (\c -> a_ [href_ $ threadUrl pid c, class_ "truncate hover:text-textBrand"] $ toHtml h.source) h.conversationId


dismiss_ :: Projects.ProjectId -> Issues.HomeHighlight -> Html ()
dismiss_ pid h =
  button_
    [ type_ "button"
    , Aria.label_ $ "Dismiss: " <> h.title
    , class_ "absolute top-2 right-2 rounded-lg p-1.5 text-iconNeutral opacity-0 group-hover/card:opacity-100 focus-visible:opacity-100 [@media(hover:none)]:opacity-100 hover:bg-fillHover transition-opacity cursor-pointer"
    , hxPost_ $ urls.homeHighlightDismissPost pid h.id
    , hxTarget_ $ "#highlight-" <> h.id.toText
    , hxSwap_ "outerHTML"
    ]
    $ faSprite_ "xmark" "regular" "w-3.5 h-3.5"


jumpBackIn_ :: HomeData -> Html ()
jumpBackIn_ d = unless (null items) $ section_ [class_ "flex flex-col gap-3", Aria.label_ "Jump back in"] do
  sectionHead_ "Jump back in" Nothing Nothing
  div_ [class_ "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-2"] $ forM_ items \(icon, title, sub, href) ->
    a_ [href_ href, class_ "flex items-center gap-3 rounded-xl p-2 -ms-2 hover:bg-fillWeak transition-colors min-w-0"] do
      span_ [class_ "flex size-9 shrink-0 items-center justify-center rounded-lg surface-raised shadow-[0_0_0_1px_var(--color-strokeWeak)]"] $ faSprite_ icon "regular" "w-4 h-4 text-iconNeutral"
      span_ [class_ "flex flex-col min-w-0"] do
        span_ [class_ "text-sm font-medium text-textStrong truncate"] $ toHtml title
        span_ [class_ "text-xs text-textWeak truncate"] $ toHtml sub
  where
    slackItem = [("slack", "#" <> c.channelName, "Shared with the Monoscope team", Integrations.slackConnectUrl c) | Just s <- [d.slack], s.invited, Just c <- [s.connect]]
    items =
      take 8
        $ slackItem
        <> [("dashboard", vm.title, "Starred dashboard", dashboardUrl d.pid vm.id Nothing []) | (vm, _) <- d.starred]
        <> [("sparkles", t.title, agoText d.now t.updatedAt, threadUrl d.pid t.conversationId) | t <- d.recentThreads]


-- | The Slack Connect channel between this project and the Monoscope team, from the viewer's side.
data SlackConnectCard = SlackConnectCard {pid :: Projects.ProjectId, email :: Text, connect :: Maybe Integrations.SlackConnect, failure :: Maybe Text}


instance HasField "invited" SlackConnectCard Bool where
  getField c = maybe False (any (\i -> i.email == c.email && isJust i.inviteId) . (.invites)) c.connect


instance ToHtml SlackConnectCard where
  toHtml = toHtml . slackConnectCard_
  toHtmlRaw = toHtml


instance Outcome SlackConnectCard where
  outcomeBody = slackConnectCard_
  outcomeToast c
    | c.invited = Just $ Toast SuccessToast ("Slack Connect invite sent to " <> c.email) (Just "Accept it in Slack to join the channel.") Nothing
    | otherwise = Nothing


slackConnectCard_ :: SlackConnectCard -> Html ()
slackConnectCard_ c = div_ [id_ "slack-connect-card", class_ "flex flex-wrap items-center gap-3 rounded-xl border border-strokeWeak bg-bgRaised p-4"] do
  span_ [class_ "flex h-9 w-9 shrink-0 items-center justify-center rounded-lg bg-fillWeak"] $ faSprite_ "slack" "solid" "w-4 h-4"
  div_ [class_ "min-w-0 flex-1 flex flex-col gap-0.5"] do
    p_ [class_ "text-sm font-medium text-textStrong"] $ case c.connect of
      Just ch | c.invited -> toHtml $ "Invite to #" <> ch.channelName <> " sent to " <> c.email
      Just ch -> toHtml $ "Your team has a shared Slack channel: #" <> ch.channelName
      Nothing -> "Talk to Monoscope engineers in Slack"
    p_ [class_ "text-xs text-textWeak"] $ toHtml case (c.failure, c.connect) of
      (Just err, _) -> err
      (_, Just ch) | Just e <- (.lastError) =<< find ((== c.email) . (.email)) ch.invites -> "Your last invite failed (" <> e <> "). Try again, or email hello@monoscope.tech."
      (_, Just _) | c.invited -> "Accept it in Slack. If your workspace needs admin approval, your Slack admin gets a request."
      (_, Just _) -> "Get an invite to join your teammates and our engineers there."
      (_, Nothing) -> "A private Slack Connect channel between your team and ours: setup help, alert tuning, feature requests. Every project member is invited by email."
  button_
    [ type_ "button"
    , class_ "btn btn-sm btn-primary"
    , hxPost_ $ urls.slackConnectPost c.pid
    , hxTarget_ "#slack-connect-card"
    , hxSwap_ "outerHTML"
    , term "hx-disabled-elt" "this"
    ]
    $ case c.connect of
      Nothing -> "Create shared channel"
      Just _ | c.invited -> "Resend invite"
      Just _ -> "Get invited"
  whenJust (guarded (const c.invited) =<< c.connect) \ch ->
    a_ [href_ $ Integrations.slackConnectUrl ch, target_ "_blank", rel_ "noopener", class_ "btn btn-sm btn-ghost"] "Open in Slack"


slackConnectPostH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (System.Types.Done SlackConnectCard))
slackConnectPostH pid = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  users <- Projects.usersByProjectId pid
  let email = T.toLower $ CI.original sess.user.email
      cfg = appCtx.config
  -- Slack transport failures and rate limits throw; the card still answers with what went wrong.
  result <-
    tryAny (Integrations.syncSlackConnect cfg.slackBotToken (filter (not . T.null) cfg.slackConnectTeamUserIds) pid project.title (pid /= Projects.demoProjectId) (map (CI.original . (.email)) users) (Just email))
      >>= either (\e -> Left "slack_unreachable" <$ logAttention "Slack Connect request failed" (AE.object ["project_id" AE..= pid, "error" AE..= show @Text e])) pure
  connect <- Integrations.getSlackConnect pid
  done $ SlackConnectCard pid email connect $ either (Just . failureText) (const Nothing) result
  where
    failureText = \case
      "slack_bot_token_unset" -> "Slack Connect isn't set up on this server yet. Email hello@monoscope.tech and we'll add you by hand."
      err -> "Slack couldn't set this up (" <> err <> "). We've been alerted; email hello@monoscope.tech if it's urgent."


data HighlightDismissed = HighlightDismissed


instance Outcome HighlightDismissed


highlightDismissPostH :: Projects.ProjectId -> UUIDId "home_highlight" -> ATAuthCtx (RespHeaders (System.Types.Done HighlightDismissed))
highlightDismissPostH pid hid = do
  _ <- Projects.sessionAndProject pid
  Issues.dismissHighlight pid hid
  done HighlightDismissed
