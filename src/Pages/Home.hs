-- | A project's Home: the first page after login. It answers "what needs me today?" from across
-- the platform — an AI ask box, the golden signals, today's new issues beside today's
-- improvements, what AI routines flagged, charts from starred dashboards, and a direct line to
-- the Monoscope team over Slack Connect.
module Pages.Home (homeGetH, HomeGet (..), HomeData (..), highlightDismissPostH, HighlightDismissed, slackConnectPostH, SlackConnectCard (..)) where

import Data.Aeson qualified as AE
import Data.CaseInsensitive qualified as CI
import Data.List (partition)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime)
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
import Pages.Components (BadgeColor (..), agoText, tonePill_)
import Pages.Dashboards (dashboardUrl)
import Pages.Issues (issueCardCompact_, issueDetailUrl, issuesListUrl)
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (SortOrder (..), UUIDId (..))
import Relude hiding (ask, asks)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging (logAttention)
import System.Types (ATAuthCtx, Outcome (..), RespHeaders, Toast (..), ToastLevel (..), addRespHeaders, done)
import System.Types qualified
import UnliftIO.Exception (tryAny)
import Utils (faSprite_, navTabAttrs)
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
  let starred = take 2 [(vm, ws) | vm <- dashboards, d <- maybeToList $ loadDashboardFromVM templates vm, let ws = take 4 $ mapMaybe (homeWidget vm) $ Dashboards.allWidgets d, not (null ws)]
      (routines, chats) = partition (.routineActive) bw.conversations
      email = T.toLower $ CI.original sess.user.email
  addRespHeaders
    $ HomePage
    $ PageCtx bw{pageTitle = "Home", menuItem = Just "Home"}
    $ HomeData{pid, firstName = sess.user.firstName, now, newIssues, newIssueCount, openIssueCount, resolved, highlights, routines, recentThreads = take 4 chats, starred, slack = SlackConnectCard pid email slackConnect Nothing <$ guard (slackConfigured && pid /= Projects.demoProjectId)}
  where
    -- Charts that render on their own: a widget needing its dashboard's variables or constants would only show an error here.
    homeWidget vm w = do
      guard $ w.wType `elem` [Widget.WTTimeseries, Widget.WTTimeseriesLine, Widget.WTTimeseriesStat, Widget.WTStat]
      guard $ not $ any (\q -> any (`T.isInfixOf` q) ["{{var-", "{{const-"]) (catMaybes [w.query, w.sql])
      pure w{Widget._projectId = Just pid, Widget._dashboardId = Just vm.id.toText, Widget.standalone = Just True}


home_ :: HomeData -> Html ()
home_ d = div_ [class_ "w-full h-full overflow-y-auto c-scroll"] $ div_ [class_ "mx-auto max-w-6xl px-4 sm:px-6 py-8 flex flex-col gap-10"] do
  header_ [class_ "flex flex-col gap-1"] do
    h1_ [class_ "text-2xl font-semibold text-textStrong tracking-tight"] $ toHtml $ "Hello" <> foldMap (", " <>) (guarded (not . T.null) d.firstName) <> "."
    p_ [class_ "text-sm text-textWeak"] $ toHtml $ formatTime defaultTimeLocale "%A, %-d %B" d.now
  askBox_ d
  whenJust (mfilter (not . (.invited)) d.slack) slackConnectCard_
  section_ [class_ "flex flex-col gap-3", Aria.label_ "At a glance"] do
    sectionHead_ "At a glance" Nothing $ Just ("Overview dashboard", urls.dashboardRedirectGet d.pid ([] :: [(Text, Maybe Text)]))
    glanceStrip_ d
    Widget.embeddedWidgets_ d.pid $ take 1 $ templateTab "_overview.yaml" "Overview" []
  div_ [class_ "grid grid-cols-1 lg:grid-cols-2 gap-8"] do
    section_ [class_ "flex flex-col gap-3 min-w-0", Aria.label_ "Today's issues"] do
      sectionHead_ "Today's issues" (Just d.newIssueCount) $ Just ("All issues", issuesListUrl d.pid)
      if null d.newIssues && not (any ((== Issues.HKIssue) . (.kind)) d.highlights)
        then quiet_ "check" "No new issues in the last 24 hours."
        else div_ [class_ "flex flex-col gap-2"] do
          forM_ (filter ((== Issues.HKIssue) . (.kind)) d.highlights) $ highlight_ d.pid
          forM_ d.newIssues $ issueCardCompact_ d.pid d.now
    section_ [class_ "flex flex-col gap-3 min-w-0", Aria.label_ "Today's improvements"] do
      let wins = filter ((== Issues.HKImprovement) . (.kind)) d.highlights
      sectionHead_ "Today's improvements" (Just $ length d.resolved + length wins) Nothing
      if null d.resolved && null wins
        then quiet_ "chart-line" "Resolved issues and AI-confirmed wins from the last 24 hours show up here."
        else div_ [class_ "flex flex-col gap-2"] do
          forM_ wins $ highlight_ d.pid
          forM_ d.resolved $ resolvedCard_ d.pid d.now
  let insights = filter ((== Issues.HKInsight) . (.kind)) d.highlights
  unless (null insights) $ section_ [class_ "flex flex-col gap-3", Aria.label_ "Flagged by AI"] do
    sectionHead_ "Flagged by AI" (Just $ length insights) $ Just ("Routines", urls.aiRoutinesGet d.pid)
    div_ [class_ "grid grid-cols-1 lg:grid-cols-2 gap-2"] $ forM_ insights $ highlight_ d.pid
  forM_ d.starred \(vm, ws) -> section_ [class_ "flex flex-col gap-3", Aria.label_ vm.title] do
    sectionHead_ vm.title Nothing $ Just ("Open dashboard", dashboardUrl d.pid vm.id Nothing [])
    div_ [class_ "grid grid-cols-1 md:grid-cols-2 gap-3"] $ forM_ ws \w ->
      div_ [class_ $ bool "h-64" "h-28" (Widget.isStatWidget w)] $ Widget.widget_ w
  jumpBackIn_ d


askBox_ :: HomeData -> Html ()
askBox_ d = section_ [class_ "flex flex-col gap-3", Aria.label_ "Ask Monoscope AI"] do
  form_ [id_ "home-ask-form", hxPost_ $ urls.aiThreadsPost d.pid, hxTarget_ "this", hxSwap_ "none", hxIndicator_ "#home-ask-loader"] do
    promptField_ "home-ask" "Ask about your system — “why is checkout slower today?”" [autofocus_]
    input_ [type_ "hidden", name_ "mode", value_ "chat"]
  div_ [class_ "flex flex-wrap gap-2"] $ forM_ suggestions \q ->
    button_ [type_ "button", class_ "rounded-full border border-strokeWeak px-3 py-1 text-xs text-textWeak hover:text-textStrong hover:bg-fillWeak transition-colors cursor-pointer", data_ "q" q, [__|on click set #home-ask.value to @data-q then call #home-ask-form.requestSubmit()|]] $ toHtml q
  where
    suggestions =
      [ "What changed in the last 24 hours?"
      , "Which endpoints got slower today?"
      , "Summarize today's errors and their likely causes"
      , "Are any services close to their limits?"
      ]


-- | Counts that frame the day: work waiting, work done, and how much the AI is watching.
glanceStrip_ :: HomeData -> Html ()
glanceStrip_ d = div_ [class_ "flex flex-wrap items-center gap-x-6 gap-y-2 text-sm text-textWeak"] do
  stat_ (issuesListUrl d.pid) "bug" (show d.openIssueCount) "open issues"
  stat_ (issuesListUrl d.pid) "circle-exclamation" (show d.newIssueCount) "new today"
  stat_ (issuesListUrl d.pid) "circle-check" (show $ length d.resolved) "resolved today"
  stat_ (urls.aiRoutinesGet d.pid) "sparkles" (show $ length d.routines) (bool "AI routines watching" "AI routine watching" (length d.routines == 1))
  where
    stat_ href icon n label = a_ ([href_ href, class_ "inline-flex items-center gap-1.5 hover:text-textStrong"] <> navTabAttrs) do
      faSprite_ icon "regular" "w-3.5 h-3.5"
      span_ [class_ "font-semibold text-textStrong tabular-nums"] $ toHtml (n :: Text)
      toHtml (label :: Text)


sectionHead_ :: Text -> Maybe Int -> Maybe (Text, Text) -> Html ()
sectionHead_ title countM linkM = div_ [class_ "flex items-center gap-2"] do
  h2_ [class_ "text-sm font-semibold text-textStrong"] $ toHtml title
  whenJust countM $ span_ [class_ "rounded bg-fillWeak px-1.5 text-xs tabular-nums text-textWeak"] . show
  whenJust linkM \(label, href) -> a_ ([href_ href, class_ "ml-auto inline-flex items-center gap-1 text-xs text-textWeak hover:text-textBrand"] <> navTabAttrs) do
    toHtml label
    faSprite_ "arrow-right" "regular" "w-3 h-3"


quiet_ :: Text -> Text -> Html ()
quiet_ icon msg = div_ [class_ "flex items-center gap-2 rounded-xl border border-dashed border-strokeWeak px-4 py-6 text-sm text-textWeak"] do
  faSprite_ icon "regular" "w-4 h-4 shrink-0"
  toHtml msg


-- | An item an agent put on Home: its kind's tone, the flagging routine, and a dismiss control.
highlight_ :: Projects.ProjectId -> Issues.HomeHighlight -> Html ()
highlight_ pid h = div_ [id_ $ "highlight-" <> h.id.toText, class_ "group/hl relative flex gap-3 rounded-xl border border-strokeWeak p-3 hover:bg-bgRaised transition-colors"] do
  div_ [class_ "shrink-0 pt-0.5"] $ faSprite_ icon "regular" $ "w-4 h-4 " <> iconColor
  div_ [class_ "min-w-0 flex-1 flex flex-col gap-1"] do
    let title = span_ [class_ "text-sm font-medium text-textStrong"] $ toHtml h.title
    maybe title (\l -> a_ [href_ l, class_ "hover:underline"] title) h.link
    unless (T.null h.detail) $ p_ [class_ "text-xs text-textWeak line-clamp-2"] $ toHtml h.detail
    div_ [class_ "flex items-center gap-2 text-xs text-textWeak"] do
      faSprite_ "sparkles" "regular" "w-3 h-3"
      maybe (toHtml h.source) (\c -> a_ [href_ $ threadUrl pid c, class_ "hover:text-textBrand"] $ toHtml h.source) h.conversationId
  button_
    [ type_ "button"
    , Aria.label_ "Dismiss"
    , class_ "absolute top-2 right-2 rounded p-1 text-iconNeutral opacity-0 group-hover/hl:opacity-100 focus-visible:opacity-100 hover:bg-fillWeak cursor-pointer"
    , hxPost_ $ urls.homeHighlightDismissPost pid h.id
    , hxTarget_ $ "#highlight-" <> h.id.toText
    , hxSwap_ "outerHTML"
    ]
    $ faSprite_ "xmark" "regular" "w-3 h-3"
  where
    (icon, iconColor) = case h.kind of
      Issues.HKIssue -> ("triangle-exclamation", "text-iconError")
      Issues.HKImprovement -> ("arrow-trend-up", "text-iconSuccess")
      Issues.HKInsight -> ("lightbulb", "text-iconBrand")


resolvedCard_ :: Projects.ProjectId -> UTCTime -> Issues.ResolvedIssue -> Html ()
resolvedCard_ pid now r = a_ ([href_ $ issueDetailUrl pid r.id, class_ "flex items-center gap-2 rounded-xl border border-strokeWeak p-3 hover:bg-bgRaised transition-colors min-w-0"] <> navTabAttrs) do
  faSprite_ "circle-check" "regular" "w-4 h-4 shrink-0 text-iconSuccess"
  span_ [class_ "text-xs text-textWeak shrink-0 tabular-nums"] $ toHtml $ "#" <> show @Text r.seqNum
  span_ [class_ "text-sm font-medium text-textStrong truncate min-w-0"] $ toHtml r.title
  tonePill_ SuccessBadge $ bool "RESOLVED" "AUTO-RESOLVED" (r.event == Issues.IEAutoResolved)
  span_ [class_ "text-xs text-textWeak shrink-0 ml-auto"] $ toHtml $ agoText now r.resolvedAt


jumpBackIn_ :: HomeData -> Html ()
jumpBackIn_ d = unless (null items) $ section_ [class_ "flex flex-col gap-3", Aria.label_ "Jump back in"] do
  sectionHead_ "Jump back in" Nothing Nothing
  div_ [class_ "grid grid-cols-2 lg:grid-cols-4 gap-3"] $ forM_ items \(icon, title, sub, href) ->
    a_ [href_ href, class_ "flex items-center gap-3 rounded-xl p-2 hover:bg-fillWeak transition-colors min-w-0"] do
      span_ [class_ "flex h-8 w-8 shrink-0 items-center justify-center rounded-lg bg-fillWeak"] $ faSprite_ icon "regular" "w-4 h-4 text-iconBrand"
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
