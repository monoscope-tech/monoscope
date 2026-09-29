module Pages.RealUserMonitoringSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Cache qualified as Cache
import Data.Default (def)
import Data.List (lookup)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.UUID.Quasi (uuid)
import Database.PostgreSQL.Simple qualified as PG
import Lucid qualified
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.RUM qualified as RUMData
import Network.HTTP.Types.URI (parseQueryText)
import Pages.BodyWrapper (PageCtx (..))
import Pages.Components (Deferred (..))
import Pages.LogExplorer.Log qualified as Log
import Pages.RealUserMonitoring qualified as RUM
import Pkg.TestUtils
import Relude
import System.Config (AuthContext (..))
import System.Timeout (timeout)
import Test.Hspec
import UnliftIO.Async (wait, withAsync)
import UnliftIO.Exception qualified as E
import Utils (toXXHash)


replayUuid :: UUID.UUID
replayUuid = [uuid|00000000-0000-0000-0000-000000000042|]


emptyReplayUuid :: UUID.UUID
emptyReplayUuid = [uuid|00000000-0000-0000-0000-000000000043|]


mergedReplayUuid :: UUID.UUID
mergedReplayUuid = [uuid|00000000-0000-0000-0000-000000000044|]


sessionId :: Text
sessionId = UUID.toText replayUuid


browserSpan :: Text -> Text -> Text -> [(Text, Text)] -> Text -> Text -> Maybe Text -> Text -> TestResources -> IO ()
browserSpan apiKey trId spId extras name sid parentM service = browserSpanAt apiKey trId spId extras name sid parentM service frozenTime


browserSpanAt :: Text -> Text -> Text -> [(Text, Text)] -> Text -> Text -> Maybe Text -> Text -> UTCTime -> TestResources -> IO ()
browserSpanAt apiKey trId spId extras name sid parentM service at tr =
  ingestSpanReq tr $ mkSpanRequest trId spId parentM name [] Nothing (map (uncurry mkAttr) $ ("session.id", sid) : extras) (mkResource apiKey [mkAttr "telemetry.sdk.language" "webjs", mkAttr "service.name" service]) at


-- | A page load exactly as the OpenTelemetry browser SDK sends it, which is what production
-- actually looks like: span named @documentLoad@, the page in @url.full@ rather than
-- @url.path@, a user agent on the resource, and — the part that broke RUM — no
-- @telemetry.sdk.language@ at all.
otelBrowserSpan :: Text -> Text -> Text -> Text -> Text -> TestResources -> IO ()
otelBrowserSpan apiKey trId spId sid service tr =
  ingestSpanReq tr
    $ mkSpanRequest
      trId
      spId
      Nothing
      "documentLoad"
      []
      Nothing
      [mkAttr "session.id" sid, mkAttr "url.full" "https://shop.example/cart"]
      (mkResource apiKey [mkAttr "service.name" service, mkAttr "user_agent.original" "Mozilla/5.0 (X11; Linux x86_64) Chrome/151"])
      frozenTime


renderPage :: TestResources -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO Text
renderPage tr tab query sessionFilterM selected = renderScoped tr tab query sessionFilterM selected Nothing


-- | Every panel fetches itself, so the page a user ends up looking at is the concatenation of
-- the panel responses. Asserting against that keeps these tests about what is on screen rather
-- than about which request delivered it.
renderScoped :: TestResources -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO Text
renderScoped tr tab query sessionFilterM selected service =
  fmap fold . forM ["pulse", "pages", "vitals", "errors", "sessions", "audience"] $ \panel ->
    renderPanel tr tab query sessionFilterM selected service (Just panel)


renderPanel :: TestResources -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO Text
renderPanel tr tab query sessionFilterM selected service panel = do
  let scoped = tr{trSessAndHeader = fmap (\session -> session{Projects.service = service}) tr.trSessAndHeader}
  (_, page) <- testServant scoped $ RUM.rumGetH testPid tab query sessionFilterM Nothing Nothing (Just "24H") selected Nothing panel (Just "1") Nothing
  pure $ toStrict $ Lucid.renderText $ Lucid.toHtml page


-- | Both cache layers: the shared rum_panel_cache table outlives a memory purge by design,
-- which is exactly what these examples must not inherit from each other.
purgeRumCaches :: TestResources -> IO ()
purgeRumCaches tr = do
  Cache.purge tr.trATCtx.rumCache
  withResource tr.trPool \conn -> void $ PG.execute_ conn "DELETE FROM rum_panel_cache"


spec :: Spec
spec = sequential $ aroundAll withTestResources do
  describe "Real User Monitoring" do
    it "emptyProject_explainsBrowserTelemetryAndOffersTheDashboard" \tr -> do
      (_, shell) <- testServant tr $ RUM.rumGetH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing Nothing Nothing Nothing
      toStrict (Lucid.renderText $ Lucid.toHtml shell) `shouldContainAll` ["hx-trigger=\"load\"", "deferred=1", "skeleton-shimmer", "tabs tabs-box tabs-outline"]
      -- The shell stands in for the tab it is loading, so switching tabs does not reflow.
      (_, sessionsShell) <- testServant tr $ RUM.rumGetH testPid (Just "sessions") Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing Nothing Nothing Nothing
      toStrict (Lucid.renderText $ Lucid.toHtml sessionsShell) `shouldContainAll` ["Loading sessions", "skeleton-shimmer"]
      html <- renderPage tr Nothing Nothing Nothing Nothing
      html `shouldContainAll` ["No browser telemetry yet", "Install the browser SDK", "Open RUM dashboard", "empty-state"]

    it "browserTelemetry_correlatesExperienceVitalsErrorsAndReplaySessions" \tr -> do
      apiKey <- createTestAPIKey tr testPid "rum-browser-key"
      browserSpan apiKey "10000000000000000000000000000001" "1000000000000001" [("url.path", "/checkout"), ("user.id", "usr-42"), ("user.full_name", "Ada Lovelace")] "Pageview · /checkout" sessionId Nothing "storefront" tr
      browserSpan apiKey "10000000000000000000000000000001" "1000000000000002" [("url.path", "/checkout"), ("error.type", "TypeError"), ("error.message", "Cannot read cart")] "TypeError · /checkout" sessionId (Just "1000000000000001") "storefront" tr
      browserSpan apiKey "20000000000000000000000000000002" "2000000000000001" [("url.path", "/search"), ("user.id", "usr-9")] "Pageview · /search" "session-search" Nothing "storefront" tr
      ingestTrace tr apiKey "GET /backend-only" frozenTime
      ingestMetric tr apiKey [] [] "browser.web_vital.lcp" 2200 frozenTime
      ingestMetric tr apiKey [] [] "browser.web_vital.cls" 0.08 frozenTime

      withResource tr.trPool \conn -> do
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_id, user_name) VALUES (?, ?, ?, ?, 1, ?, ?) ON CONFLICT (session_id) DO UPDATE SET created_at = EXCLUDED.created_at, last_event_at = EXCLUDED.last_event_at" (replayUuid, testPid, frozenTime, addUTCTime 60 frozenTime, "usr-42" :: Text, "Ada Lovelace" :: Text)
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name) VALUES (?, ?, ?, ?, 0, ?) ON CONFLICT (session_id) DO UPDATE SET created_at = EXCLUDED.created_at, last_event_at = EXCLUDED.last_event_at, event_file_count = 0, file_keys = '{}', shard_keys = '{}'" (emptyReplayUuid, testPid, frozenTime, addUTCTime 60 frozenTime, "No recording" :: Text)
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, shard_keys, user_name) VALUES (?, ?, ?, ?, 0, ARRAY['00000000-0000-0000-0000-000000000044/merged.json.gz'], ?) ON CONFLICT (session_id) DO UPDATE SET created_at = EXCLUDED.created_at, last_event_at = EXCLUDED.last_event_at, event_file_count = 0, shard_keys = EXCLUDED.shard_keys" (mergedReplayUuid, testPid, frozenTime, addUTCTime 60 frozenTime, "Merged replay" :: Text)

      (_, RUM.RumGet (PageCtx _ overviewBody)) <- testServant tr $ RUM.rumGetH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing (Just "pulse") (Just "1") Nothing
      overviewData <- case overviewBody of
        DeferredBody loaded -> pure loaded
        DeferredShell{} -> fail "RUM answered with the deferred shell when asked for the body"
      overviewData.hasTelemetry `shouldBe` True
      overviewData.degradedPanels `shouldBe` []
      overview <- renderPage tr Nothing Nothing Nothing Nothing
      -- The unscoped read caches under an unscoped key; `service` is part of that key so a
      -- scoped page can never be served these rows.
      isJust <$> Cache.lookup tr.trATCtx.rumCache (RUMData.RumCacheKey testPid RUMData.VitalSamplesQuery Nothing Nothing Nothing Nothing (Just "24H")) `shouldReturn` True
      -- The numbers and activity chart are dashboard Widget components that fetch their own
      -- data through the chart pipeline; the page ships their queries, not their values.
      overview `shouldContainAll` ["Page views", "Browser errors", "bin_auto(timestamp)", "rum-activity", "Largest Contentful Paint", "2.2 s", "/checkout", "Ada Lovelace"]
      -- The Overview warms the Performance tab's heaviest scan in the background, with the
      -- response discarded — so opening Performance answers from cache.
      overview `shouldContainAll` ["panel=vital_trend", "hx-swap=\"none\""]
      -- The LIVE badge is only honest if something listens for the time transport's tick, and
      -- the panels hold every number on this page. Each re-fetches itself in place.
      overview `shouldContainAll` ["hx-trigger=\"update-query from:window\"", "hx-sync=\"this:replace\""]
      -- The tab strip and time picker must not wait on six 24-hour scans: the request that
      -- paints the page answers with a skeleton that fetches the panels itself. Panel data
      -- appearing here again would mean a tab click is back to seconds of blank page.
      (_, shellPage) <- testServant tr $ RUM.rumGetH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing Nothing Nothing Nothing
      let shell = toStrict $ Lucid.renderText $ Lucid.toHtml shellPage
      shell `shouldContainAll` ["Real User Monitoring", "tabs tabs-box tabs-outline", "id=\"rum-page\"", "hx-trigger=\"load\"", "deferred=1"]
      T.isInfixOf "Ada Lovelace" shell `shouldBe` False

      sessions <- renderPage tr (Just "sessions") Nothing (Just "errors") (Just sessionId)
      sessions `shouldContainAll` ["With errors", "Ada Lovelace", "Watch replay", "initialSession=\"00000000-0000-0000-0000-000000000042\"", "hx-target=\"#rum-replay-workspace\"", "hx-sync=\"#rum-replay-workspace:replace\""]
      T.isInfixOf "/search" sessions `shouldBe` False
      -- A row click must fetch the sessions panel: a deferred request without a panel renders
      -- only shells, hx-select finds no workspace in it, and the outerHTML swap deletes the
      -- workspace from the page.
      sessions `shouldContainAll` ["panel=sessions&amp;deferred=1"]
      -- And the shells a deep link renders must carry its filter and selection, or the panel
      -- they fetch comes back unfiltered with nothing selected.
      (_, deepShell) <- testServant tr $ RUM.rumGetH testPid (Just "sessions") Nothing (Just "errors") Nothing Nothing (Just "24H") (Just sessionId) Nothing Nothing (Just "1") Nothing
      toStrict (Lucid.renderText $ Lucid.toHtml deepShell) `shouldContainAll` ["filter=errors", "session=00000000-0000-0000-0000-000000000042"]

      replaySessions <- renderPage tr (Just "sessions") Nothing (Just "replays") Nothing
      T.isInfixOf "No recording" replaySessions `shouldBe` False
      T.isInfixOf "Merged replay" replaySessions `shouldBe` True

    it "deferredRUMPanel_omitsPageChrome_preservesPanelMarkup" \tr -> do
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-panel-body-key"
      otelBrowserSpan apiKey "88000000000000000000000000000008" "8800000000000001" "session-panel-body" "panel-body-browser" tr
      let scoped = tr{trSessAndHeader = fmap (\session -> session{Projects.service = Just "panel-body-browser"}) tr.trSessAndHeader}
      (_, page@(RUM.RumGet (PageCtx _ body))) <- testServant scoped $ RUM.rumGetH testPid (Just "performance") Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing (Just "pages") (Just "1") Nothing
      let rendered = Lucid.renderText $ Lucid.toHtml page
      (rendered == Lucid.renderText (Lucid.toHtml body)) `shouldBe` True
      toStrict rendered `shouldContainAll` ["id=\"rum-page\"", "id=\"rum-panel-pages\"", "Top pages", "/cart"]
      T.isInfixOf "<html" (toStrict rendered) `shouldBe` False
      (_, fullPage) <- testServant scoped $ RUM.rumGetH testPid (Just "performance") Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing Nothing (Just "1") Nothing
      T.isInfixOf "<html" (toStrict $ Lucid.renderText $ Lucid.toHtml fullPage) `shouldBe` True

    it "panelLinks_carryKqlTheLogExplorerCanParse" \tr -> do
      -- windowUrl URI-encodes every parameter it is given, so a caller that encodes first
      -- ships a double-escaped query: a space arrives as %2520, the Explorer sees one
      -- meaningless token, and the link silently returns nothing.
      overview <- renderPage tr Nothing Nothing Nothing Nothing
      -- Pinned as the property rather than one exact query string: a space encoded once as
      -- %20, a quote once as %22, and no %25 anywhere on the page — %25 is the signature of a
      -- second pass, since it is what a literal % becomes.
      overview `shouldContainAll` ["/log_explorer?since=24H&amp;query=", "%20and%20", "%22documentLoad%22"]
      T.isInfixOf "%25" overview `shouldBe` False

    it "serviceFilter_scopesEveryPanelToOneBrowserService" \tr -> do
      -- Several teams report into one project. Averaging their services together hides a
      -- checkout regression behind a healthy marketing site, so the page has to be scopeable.
      -- Panels are cached for 15s across requests, and earlier examples already populated the
      -- unscoped key. Without this the assertions below read a snapshot taken before the span
      -- this example ingests.
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-service-key"
      -- Reuses the session that already has a recording, so the scoped page can be checked
      -- for the replay badge as well as the rows.
      browserSpan apiKey "30000000000000000000000000000003" "3000000000000001" [("url.path", "/admin/users")] "Pageview · /admin/users" (UUID.toText mergedReplayUuid) Nothing "admin-console" tr

      unscoped <- renderPage tr Nothing Nothing Nothing Nothing
      unscoped `shouldContainAll` ["/admin/users", "/checkout"]

      scoped <- renderScoped tr Nothing Nothing Nothing Nothing (Just "admin-console")
      scoped `shouldContainAll` ["admin-console", "/admin/users"]
      -- The other team's pages are the whole point: if they survive the filter it does nothing.
      T.isInfixOf "/checkout" scoped `shouldBe` False
      -- Explorer receives the same global scope predicate as the SQL-backed panels.
      -- The compact KQL spelling is intentional: it is parsed and URL-encoded only once.
      scoped `shouldContainAll` ["service%3D%3D%22admin-console%22"]
      T.isInfixOf "&amp;service=admin-console" scoped `shouldBe` False
      T.isInfixOf "?service=admin-console" scoped `shouldBe` False
      -- A recording carries no service, so it can only be trusted where a span already places
      -- the session in this service — attached to that row, never added as a row of its own.
      scoped `shouldContainAll` ["Replay"]
      T.isInfixOf "No recording" scoped `shouldBe` False

      -- A stale global selection still resolves, and an empty result must read as "this filter
      -- matched nothing", never as "you never installed the SDK".
      ghost <- renderScoped tr Nothing Nothing Nothing Nothing (Just "ghost-service")
      ghost `shouldContainAll` ["No browser telemetry for ghost-service in this range", "global scope picker"]
      T.isInfixOf "Install the browser SDK" ghost `shouldBe` False

    it "sharedRumLinks_keepTheirEnvironmentInsteadOfUsingTheRecipientDefault" \tr -> do
      -- An alert recipient may have a different sticky environment selected. A RUM link is
      -- investigation evidence, so its explicit environment has to win and remain present on
      -- the deferred request and every self-link it renders.
      purgeRumCaches tr
      (_, scopedPage) <- testServant tr $ RUM.rumGetScopedH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing (Just "admin-console") Nothing (Just "1") Nothing (Just "missing-environment")
      let scopedHtml = toStrict $ Lucid.renderText $ Lucid.toHtml scopedPage
      scopedHtml `shouldContainAll` ["environment=missing-environment"]
      T.isInfixOf "&amp;service=admin-console" scopedHtml `shouldBe` False
      T.isInfixOf "?service=admin-console" scopedHtml `shouldBe` False

      (_, RUM.RumGet (PageCtx _ pulseBody)) <- testServant tr $ RUM.rumGetScopedH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing (Just "admin-console") (Just "pulse") (Just "1") Nothing (Just "missing-environment")
      pulse <- case pulseBody of
        DeferredBody loaded -> pure loaded
        DeferredShell{} -> fail "RUM answered with the deferred shell when asked for the panel"
      -- The project has browser telemetry from the preceding examples. It must not leak into
      -- a link scoped to another environment merely because this test session's default is
      -- unscoped.
      pulse.hasTelemetry `shouldBe` False

      (_, RUM.RumGet (PageCtx _ sessionsBody)) <- testServant tr $ RUM.rumGetScopedH testPid (Just "sessions") Nothing Nothing Nothing Nothing (Just "24H") (Just sessionId) Nothing (Just "sessions") (Just "1") Nothing (Just "missing-environment")
      case sessionsBody of
        DeferredBody loaded -> do
          loaded.sessions `shouldBe` []
          loaded.selectedSessionData `shouldBe` Nothing
        DeferredShell{} -> expectationFailure "expected the sessions panel"

    it "environmentSessionCache_ignoresLegacyUnattributedRecordings" \tr -> do
      purgeRumCaches tr
      let sid = "00000000-0000-0000-0000-000000000046"
          environment = Just "cache-environment"
          load env refreshM = do
            (_, RUM.RumGet (PageCtx _ body)) <- testServant tr $ RUM.rumGetScopedH testPid (Just "sessions") (Just "Legacy replay") Nothing Nothing Nothing (Just "24H") (Just sid) Nothing (Just "sessions") (Just "1") refreshM env
            case body of
              DeferredBody loaded -> pure loaded
              DeferredShell{} -> fail "expected the sessions panel"
          legacyKey query = toXXHash $ show $ RUMData.RumCacheKey testPid query environment Nothing Nothing Nothing (Just "24H")
      withResource tr.trPool $ \conn ->
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name) VALUES (?::uuid, ?, ?, ?, 1, 'Legacy replay')" (sid, testPid, frozenTime, addUTCTime 60 frozenTime)
      legacy <- load Nothing Nothing
      map (.id) legacy.sessions `shouldBe` [sid]
      runQueryEffect tr do
        RUMData.rumPanelCacheSet (legacyKey $ RUMData.SessionSearchQuery (Just "Legacy replay") RUMData.AllSessionRows) 300 (RUMData.SessionsResult legacy.sessions)
        RUMData.rumPanelCacheSet (legacyKey $ RUMData.SessionDetailQuery sid) 300 (RUMData.SessionDetailResult legacy.selectedSessionData)
      Cache.purge tr.trATCtx.rumCache
      scoped <- load environment Nothing
      scoped.sessions `shouldBe` []
      scoped.selectedSessionData `shouldBe` Nothing
      apiKey <- createTestAPIKey tr testPid "rum-environment-cache-key"
      ingestSpanReq tr $ mkSpanRequest "83000000000000000000000000000008" "8300000000000001" Nothing "documentLoad" [] Nothing [mkAttr "session.id" sid] (mkResource apiKey [mkAttr "service.name" "legacy-browser", mkAttr "telemetry.sdk.language" "webjs", mkAttr "deployment.environment.name" "cache-environment"]) frozenTime
      attributed <- load environment (Just "1")
      map (.events) attributed.sessions `shouldBe` [1]
      map (.hasReplay) attributed.sessions `shouldBe` [True]
      (.hasReplay) <$> attributed.selectedSessionData `shouldBe` Just True

    it "browserSdkWithoutSdkLanguage_isStillSeenAndGloballyScopeable" \tr -> do
      -- The OpenTelemetry browser SDKs leave telemetry.sdk.language unset, and RUM used to
      -- filter on that alone. Every browser application in production was therefore invisible:
      -- no page views, no sessions, and nothing for the global scope to narrow to.
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-otel-browser-key"
      otelBrowserSpan apiKey "40000000000000000000000000000004" "4000000000000001" "session-otel" "checkout-web" tr

      page <- renderPage tr Nothing Nothing Nothing Nothing
      page `shouldContainAll` ["https://shop.example/cart"]

      -- And the global scope can narrow to it.
      scoped <- renderScoped tr Nothing Nothing Nothing Nothing (Just "checkout-web")
      scoped `shouldContainAll` ["checkout-web", "https://shop.example/cart"]
      T.isInfixOf "/admin/users" scoped `shouldBe` False

    it "sessionLastPage_isTheLatestPageView_notALexicographicResourceUrl" \tr -> do
      -- MAX(path) over every browser span used to pick the alphabetically largest URL: on
      -- real traffic that is a third-party font fetched by the page, shown as the page.
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-lastpage-key"
      browserSpanAt apiKey "50000000000000000000000000000005" "5000000000000001" [("url.path", "/alpha")] "Pageview · /alpha" "session-lastpage" Nothing "storefront" (addUTCTime (-120) frozenTime) tr
      browserSpanAt apiKey "50000000000000000000000000000005" "5000000000000002" [("url.full", "https://zzz-fonts.example/css2")] "HTTP GET" "session-lastpage" Nothing "storefront" (addUTCTime (-60) frozenTime) tr
      browserSpanAt apiKey "50000000000000000000000000000005" "5000000000000003" [("url.path", "/beta")] "Pageview · /beta" "session-lastpage" Nothing "storefront" (addUTCTime (-30) frozenTime) tr
      row <- renderPanel tr (Just "sessions") (Just "session-lastpage") Nothing Nothing Nothing (Just "sessions")
      row `shouldContainAll` ["/beta"]
      T.isInfixOf "zzz-fonts.example" row `shouldBe` False
      T.isInfixOf "/alpha" row `shouldBe` False

    it "selectedSession_withoutRecording_keepsIdentityAndTelemetryInWorkspace" \tr -> do
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-telemetry-workspace-key"
      let sid = "session-without-recording"
      browserSpanAt apiKey "84000000000000000000000000000008" "8400000000000001" [("url.path", "/profile"), ("user.full_name", "Telemetry visitor")] "documentLoad" sid Nothing "workspace-browser" (addUTCTime (-65) frozenTime) tr
      browserSpan apiKey "84000000000000000000000000000008" "8400000000000002" [("exception.type", "TypeError"), ("exception.message", "Form failed")] "TypeError" sid (Just "8400000000000001") "workspace-browser" tr
      html <- renderPanel tr (Just "sessions") (Just sid) Nothing (Just sid) Nothing (Just "sessions")
      let workspace = fst $ T.breakOn "</section>" $ snd $ T.breakOn "<section id=\"rum-replay-workspace\"" html
          headings = fst $ T.breakOn "</thead>" $ snd $ T.breakOn "<thead" html
      workspace `shouldContainAll` ["Telemetry visitor", sid, "Duration", "1m 5s", "Page views", "Events", "Errors", "/profile", "workspace-browser", "No recording for this session", "Inspect telemetry"]
      T.isInfixOf ">2</dd>" workspace `shouldBe` True
      headings `shouldContainAll` ["Last page"]
      T.isInfixOf "Landing page" headings `shouldBe` False

    it "replayOnlySessions_sayWhatTheyAre_insteadOfUnknownPageAndZeroCounts" \tr -> do
      -- A recording whose session id never appears on a span is a real session; stacking
      -- "Unknown page" over "0 views · 0 events" reads as broken data, not as what it is.
      purgeRumCaches tr
      let replayOnlyUuid = [uuid|00000000-0000-0000-0000-000000000045|]
      withResource tr.trPool \conn ->
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name) VALUES (?, ?, ?, ?, 1, ?) ON CONFLICT (session_id) DO UPDATE SET created_at = EXCLUDED.created_at, last_event_at = EXCLUDED.last_event_at" (replayOnlyUuid, testPid, frozenTime, addUTCTime 45 frozenTime, "Replay only user" :: Text)
      rows <- renderPanel tr (Just "sessions") (Just "Replay only user") Nothing Nothing Nothing (Just "sessions")
      rows `shouldContainAll` ["Replay only user", "Recording only", "No telemetry"]
      T.isInfixOf "Unknown page" rows `shouldBe` False
      T.isInfixOf "0 views" rows `shouldBe` False

    it "browserErrors_groupBySignature_withOccurrenceAndSessionCounts" \tr -> do
      -- Twenty copies of the loudest error used to fill the whole panel; issues with masked
      -- identifiers keep every distinct failure visible with its blast radius.
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-errors-key"
      browserSpan apiKey "60000000000000000000000000000006" "6000000000000001" [("exception.type", "TypeError"), ("exception.message", "Cannot read cart item 123")] "TypeError" "session-err-a" Nothing "storefront" tr
      browserSpan apiKey "60000000000000000000000000000006" "6000000000000002" [("exception.type", "TypeError"), ("exception.message", "Cannot read cart item 456")] "TypeError" "session-err-b" Nothing "storefront" tr
      panel <- renderPanel tr Nothing Nothing Nothing Nothing Nothing (Just "errors")
      panel `shouldContainAll` ["TypeError", "×2", "2 sessions", "View session", "Telemetry"]
      panel `shouldSatisfy` T.isInfixOf "tab=sessions"
      -- Grouped, not listed: the message renders once for the pair.
      T.count "Cannot read cart item" panel `shouldBe` 1

    it "panelCache_writeFailure_preservesFreshTelemetry" \tr -> do
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-cache-write-key"
      otelBrowserSpan apiKey "81000000000000000000000000000008" "8100000000000001" "session-cache-write" "cache-write-browser" tr
      let alter sql = withResource tr.trPool $ \conn -> void $ PG.execute_ conn sql
      E.bracket_
        (alter "ALTER TABLE rum_panel_cache ADD CONSTRAINT reject_panel_cache_write CHECK (false)")
        (alter "ALTER TABLE rum_panel_cache DROP CONSTRAINT reject_panel_cache_write")
        do
          (_, RUM.RumGet (PageCtx _ body)) <- testServant tr $ RUM.rumGetH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing (Just "pages") (Just "1") Nothing
          case body of
            DeferredBody loaded -> do
              loaded.degradedPanels `shouldBe` []
              map (.path) loaded.pages `shouldSatisfy` elem "https://shop.example/cart"
            DeferredShell{} -> expectationFailure "expected the pages panel"

    it "panelCache_concurrentColdRequests_shareOneLookupAndPublication" \tr -> do
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-flight-key"
      otelBrowserSpan apiKey "82000000000000000000000000000008" "8200000000000001" "session-flight" "flight-browser" tr
      let alter sql = withResource tr.trPool $ \conn -> void $ PG.execute_ conn sql
          fetch = renderPanel tr Nothing Nothing Nothing Nothing (Just "flight-browser") (Just "pages")
          waitForReaders expected = do
            [PG.Only readers] <- withResource tr.trPool $ \conn -> PG.query_ conn "SELECT count(*)::bigint FROM pg_stat_activity WHERE datname = current_database() AND wait_event_type = 'Lock' AND query LIKE '%rum_panel_cache%'"
            unless (readers >= (expected :: Int64)) $ threadDelay 10000 >> waitForReaders expected
      E.bracket_
        (alter "CREATE SEQUENCE rum_flight_writes; CREATE FUNCTION count_rum_flight_write() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN PERFORM nextval('rum_flight_writes'); RETURN NEW; END $$; CREATE TRIGGER count_rum_flight_write BEFORE INSERT ON rum_panel_cache FOR EACH ROW EXECUTE FUNCTION count_rum_flight_write()")
        (alter "DROP TRIGGER count_rum_flight_write ON rum_panel_cache; DROP FUNCTION count_rum_flight_write(); DROP SEQUENCE rum_flight_writes")
        $ withResource tr.trPool \conn ->
          E.bracket_
            (void $ PG.execute_ conn "BEGIN; LOCK TABLE rum_panel_cache IN ACCESS EXCLUSIVE MODE")
            (void $ PG.execute_ conn "ROLLBACK")
            $ withAsync fetch \leader -> withAsync fetch \follower -> do
              observed <-
                E.finally
                  ((,) <$> timeout 5000000 (waitForReaders 1) <*> timeout 1000000 (waitForReaders 2))
                  (void $ PG.execute_ conn "ROLLBACK")
              responses <- traverse wait [leader, follower]
              observed `shouldBe` (Just (), Nothing)
              map (T.isInfixOf "/cart") responses `shouldBe` [True, True]
              writes <- withResource tr.trPool $ \cacheConn -> PG.query_ cacheConn "SELECT last_value, is_called FROM rum_flight_writes" :: IO [(Int64, Bool)]
              writes `shouldBe` [(1, True)]

    it "emptySessionSearch_isBrieflyShared_andRefreshOrExpiryShowsNewTelemetry" \tr -> do
      purgeRumCaches tr
      let alter sql = withResource tr.trPool $ \conn -> void $ PG.execute_ conn sql
          load query refreshM = do
            (_, RUM.RumGet (PageCtx _ body)) <- testServant tr $ RUM.rumGetH testPid (Just "sessions") (Just query) Nothing Nothing Nothing (Just "24H") Nothing Nothing (Just "sessions") (Just "1") refreshM
            case body of
              DeferredBody page -> map (.id) page.sessions <$ (page.degradedPanels `shouldBe` [])
              DeferredShell{} -> fail "expected session search results"
          refreshSearch = "session-negative-refresh"
          expirySearch = "session-negative-expiry"
      E.bracket_
        (alter "CREATE SEQUENCE rum_search_writes; CREATE FUNCTION count_rum_search_write() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN PERFORM nextval('rum_search_writes'); RETURN NEW; END $$; CREATE TRIGGER count_rum_search_write BEFORE INSERT ON rum_panel_cache FOR EACH ROW EXECUTE FUNCTION count_rum_search_write()")
        (alter "DROP TRIGGER count_rum_search_write ON rum_panel_cache; DROP FUNCTION count_rum_search_write(); DROP SEQUENCE rum_search_writes")
        do
          load refreshSearch Nothing `shouldReturn` []
          -- A fresh replica must reuse the empty result without scanning telemetry.
          observed <- withResource tr.trPool \conn ->
            E.bracket_
              (void $ PG.execute_ conn "BEGIN; LOCK TABLE otel_logs_and_spans IN ACCESS EXCLUSIVE MODE")
              (void $ PG.execute_ conn "ROLLBACK")
              $ withAsync (do memory <- load refreshSearch Nothing; Cache.purge tr.trATCtx.rumCache; shared <- load refreshSearch Nothing; pure (memory, shared)) \request -> do
                result <- E.finally (timeout 2000000 $ wait request) (void $ PG.execute_ conn "ROLLBACK")
                void $ wait request
                pure result
          observed `shouldBe` Just ([], [])
          writes <- withResource tr.trPool $ \conn -> PG.query_ conn "SELECT last_value, is_called FROM rum_search_writes" :: IO [(Int64, Bool)]
          writes `shouldBe` [(1, True)]
          -- Older replicas promote this namespace with the full positive TTL.
          let oldKey = toXXHash $ show (RUMData.RumCacheKey testPid (RUMData.SessionSearchQuery (Just refreshSearch) RUMData.AllSessionRows) Nothing Nothing Nothing Nothing (Just "24H")) <> ":attributed-recordings-v2"
          [PG.Only oldEntries] <- withResource tr.trPool $ \conn -> PG.query conn "SELECT count(*)::bigint FROM rum_panel_cache WHERE cache_key = ?" (PG.Only oldKey)
          oldEntries `shouldBe` (0 :: Int64)
          [PG.Only shortExpiry] <- withResource tr.trPool $ \conn -> PG.query_ conn "SELECT expires_at > now() AND expires_at <= now() + interval '30 seconds' FROM rum_panel_cache"
          shortExpiry `shouldBe` True
          purgeRumCaches tr
          load refreshSearch Nothing `shouldReturn` []
          apiKey <- createTestAPIKey tr testPid "rum-negative-search-key"
          otelBrowserSpan apiKey "85000000000000000000000000000008" "8500000000000001" refreshSearch "negative-search-browser" tr
          load refreshSearch Nothing `shouldReturn` []
          load refreshSearch (Just "1") `shouldReturn` [refreshSearch]
          [PG.Only positiveExpiry] <- withResource tr.trPool $ \conn -> PG.query_ conn "SELECT expires_at > now() + interval '4 minutes' FROM rum_panel_cache"
          positiveExpiry `shouldBe` True
          load expirySearch Nothing `shouldReturn` []
          otelBrowserSpan apiKey "86000000000000000000000000000008" "8600000000000001" expirySearch "negative-search-browser" tr
          Cache.purge tr.trATCtx.rumCache
          alter "UPDATE rum_panel_cache SET expires_at = now() - interval '1 minute'"
          load expirySearch Nothing `shouldReturn` [expirySearch]

    it "emptySessionList_withoutSearch_doesNotHideNewSdkTelemetry" \tr -> do
      purgeRumCaches tr
      let service = "negative-cache-onboarding"
          fetch = renderPanel tr (Just "sessions") Nothing Nothing Nothing (Just service) (Just "sessions")
      forM_ ([Nothing, Just " \t "] :: [Maybe Text]) $ \query -> void $ renderPanel tr (Just "sessions") query Nothing Nothing (Just service) (Just "sessions")
      [PG.Only entries] <- withResource tr.trPool $ \conn -> PG.query_ conn "SELECT count(*)::bigint FROM rum_panel_cache"
      entries `shouldBe` (0 :: Int64)
      apiKey <- createTestAPIKey tr testPid "rum-negative-onboarding-key"
      otelBrowserSpan apiKey "87000000000000000000000000000008" "8700000000000001" "session-first-sdk-arrival" service tr
      fetch >>= (`shouldContainAll` ["session-first-sdk-arrival", "/cart"])

    it "panelCache_isSharedAcrossReplicas_notPerProcessMemory" \tr -> do
      -- A fresh replica has an empty memory cache; the shared rum_panel_cache table must
      -- still answer, proven by deleting the underlying rows so a recompute could not.
      -- The span is documentLoad-shaped: TimeFusion's text-index prefilter currently loses
      -- LIKE-matched ("Pageview \183 ") names inside narrow windows, which is a store bug
      -- this example must not depend on.
      apiKey <- createTestAPIKey tr testPid "rum-l2-key"
      ingestSpanReq tr $ mkSpanRequest "80000000000000000000000000000008" "8000000000000001" Nothing "documentLoad" [] Nothing [mkAttr "session.id" "session-l2", mkAttr "url.full" "https://l2.example/cached"] (mkResource apiKey [mkAttr "service.name" "storefront", mkAttr "user_agent.original" "Mozilla/5.0 L2"]) frozenTime
      -- A window no earlier example used, so the shared table has no entry yet for this key.
      let renderPages since refreshM = do
            (_, page) <- testServant tr $ RUM.rumGetH testPid Nothing Nothing Nothing Nothing Nothing (Just since) Nothing Nothing (Just "pages") (Just "1") refreshM
            pure $ toStrict $ Lucid.renderText $ Lucid.toHtml page
      firstRender <- renderPages "6H" Nothing
      -- Top pages renders path-only routes ('pageRoute'), so the host is stripped.
      firstRender `shouldContainAll` ["/cached"]
      -- Memory only — the shared table entry is exactly what a fresh replica would find.
      Cache.purge tr.trATCtx.rumCache
      withResource tr.trPool \conn ->
        void $ PG.execute conn "DELETE FROM otel_logs_and_spans WHERE project_id = ? AND attributes___session___id = ?" (testPid, "session-l2" :: Text)
      secondRender <- renderPages "6H" Nothing
      secondRender `shouldContainAll` ["/cached"]
      -- A fresh entry answers and asks for nothing more; only a stale one may schedule a refetch.
      T.isInfixOf "refresh=1" secondRender `shouldBe` False

      -- Past expiry but inside the prune horizon: the panel must still paint its last-known
      -- data rather than a cold scan, and carry the hidden refresh trigger that revalidates
      -- it. Without the trigger the page would show aged data forever.
      Cache.purge tr.trATCtx.rumCache
      withResource tr.trPool \conn ->
        void $ PG.execute_ conn "UPDATE rum_panel_cache SET expires_at = now() - interval '1 minute'"
      staleRender <- renderPages "6H" Nothing
      staleRender `shouldContainAll` ["/cached", "refresh=1"]
      -- And the revalidation terminates: refresh bypasses the stale band, so its response
      -- carries no trigger of its own.
      Cache.purge tr.trATCtx.rumCache
      refreshRender <- renderPages "6H" (Just "1")
      T.isInfixOf "refresh=1" refreshRender `shouldBe` False

    it "audiencePanel_classifiesUserAgentsIntoBrowserOsAndDevice" \tr -> do
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-audience-key"
      let chromeUa = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/151.0.0.0 Safari/537.36"
          iphoneUa = "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Mobile/15E148 Safari/604.1"
      browserSpan apiKey "70000000000000000000000000000007" "7000000000000001" [("url.path", "/a"), ("user_agent.original", chromeUa)] "Pageview · /a" "session-ua-1" Nothing "storefront" tr
      browserSpan apiKey "70000000000000000000000000000007" "7000000000000002" [("url.path", "/a"), ("user_agent.original", chromeUa)] "Pageview · /a" "session-ua-2" Nothing "storefront" tr
      browserSpan apiKey "70000000000000000000000000000007" "7000000000000003" [("url.path", "/b"), ("user_agent.original", iphoneUa)] "Pageview · /b" "session-ua-3" Nothing "storefront" tr
      panel <- renderPanel tr Nothing Nothing Nothing Nothing Nothing (Just "audience")
      panel `shouldContainAll` ["Audience", "Chrome", "Windows", "Safari", "iOS", "Mobile", "Desktop", "2 sessions"]

    it "sessionSearch_matchesBeforeLimits_andKeepsCrossStoreContext" \tr -> do
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-search-limit-key"
      let oldId = "00000000-0000-0000-000b-000000000001"
          oldTime = addUTCTime (-3600) frozenTime
          recentId :: Int -> Text
          recentId n = "00000000-0000-0000-000a-" <> T.justifyRight 12 '0' (show n)
      browserSpanAt apiKey "e0000000000000000000000000000001" "e000000000000001" [("url.path", "/needle%_path")] "documentLoad" oldId Nothing "bulk-ui" oldTime tr
      browserSpanAt apiKey "e0000000000000000000000000000001" "e000000000000002" [("url.path", "/checkout"), ("exception.type", "TypeError")] "documentLoad" oldId Nothing "bulk-ui" (addUTCTime 1 oldTime) tr
      forM_ ([1 .. 201] :: [Int]) $ \n ->
        browserSpan apiKey ("f" <> T.justifyRight 31 '0' (show n)) (T.justifyRight 16 '0' (show n)) [("url.path", "/recent")] "documentLoad" (recentId n) Nothing "bulk-ui" tr
      withResource tr.trPool $ \conn -> do
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name) VALUES (?::uuid, ?, ?, ?, 1, 'Archive analyst')" (oldId, testPid, oldTime, addUTCTime 60 oldTime)
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name) SELECT ('00000000-0000-0000-000a-' || lpad(i::text, 12, '0'))::uuid, ?, ?, ?, 1, 'Recent shopper' FROM generate_series(1, 201) i" (testPid, frozenTime, addUTCTime 60 frozenTime)
      -- Both a historical page and recording-only identity must find the old session,
      -- retain all its events, and attach its recording, despite 201 newer rows.
      forM_ ["needle%_path", "ARCHIVE ANALYST", oldId] $ \query -> do
        rows <- renderPanel tr (Just "sessions") (Just query) Nothing Nothing (Just "bulk-ui") (Just "sessions")
        rows `shouldContainAll` ["Archive analyst", "2 views", "2 events", "Replay", "/checkout"]
        T.count "class=\"rum-session-link" rows `shouldBe` 1
      errors <- renderPanel tr (Just "sessions") Nothing (Just "errors") Nothing (Just "bulk-ui") (Just "sessions")
      errors `shouldContainAll` ["Archive analyst", "1 error"]
      -- Selecting an old deep link does not depend on it surviving the list's limit.
      detail <- renderPanel tr (Just "sessions") Nothing Nothing (Just oldId) (Just "bulk-ui") (Just "sessions")
      detail `shouldContainAll` ["Newest 200 sessions", "initialSession=\"" <> oldId <> "\""]

    it "pageLinks_findAbsoluteUrlEventsWithoutWideningTheirRouteOrScope" \tr -> do
      apiKey <- createTestAPIKey tr testPid "rum-page-links-key"
      let service = "route-links"
          environment = "route-links-env"
          ingest index attribute url svc env at =
            ingestSpanReq tr $ mkSpanRequest ("d" <> T.justifyRight 31 '0' (show index)) ("d" <> T.justifyRight 15 '0' (show index)) Nothing "documentLoad" [] Nothing [mkAttr attribute url] (mkResource apiKey [mkAttr "service.name" svc, mkAttr "user_agent.original" "Mozilla/5.0", mkAttr "deployment.environment.name" env]) at
      forM_ (zip ([1 ..] :: [Int]) ["/cart", "/cart?coupon=x", "/cart#section", "/cartoon", "/other/cart", "/search?q=/cart", "/Cart", "/cart/"]) \(index, path) ->
        ingest index "url.full" ("https://shop.example" <> path) service environment frozenTime
      ingest 9 "url.path" "/cart" service environment frozenTime
      ingest 10 "url.full" "https://shop.example/cart" "other-service" environment frozenTime
      ingest 11 "url.full" "https://shop.example/cart" service "other-environment" frozenTime
      ingest 12 "url.full" "https://shop.example/cart" service environment $ addUTCTime (-120) frozenTime
      ingestMetric tr apiKey [mkAttr "service.name" service, mkAttr "deployment.environment.name" environment] [mkAttr "page.url" "https://shop.example/cart"] "browser.web_vital.lcp" 1200 frozenTime
      forM_ (zip ([13 ..] :: [Int]) ["https://shop.example", "https://shop.example/", "/?q=x", "/cart", "https://shop.example/a.b+(x)", "https://shop.example/aZbxxx", "https://shop.example/literal%2Fsegment"]) \(index, url) ->
        ingest index "url.full" url service environment frozenTime
      forM_ ["/", "/cart/", "/a.b+(x)", "/literal%2Fsegment"] \route ->
        ingestMetric tr apiKey [mkAttr "service.name" service, mkAttr "deployment.environment.name" environment] [mkAttr "page.url" $ "https://shop.example" <> route] "browser.web_vital.lcp" 1200 frozenTime
      let from = toText $ iso8601Show $ addUTCTime (-60) frozenTime
          to = toText $ iso8601Show $ addUTCTime 60 frozenTime
          scoped = tr{trSessAndHeader = fmap (\session -> session{Projects.service = Just service}) tr.trSessAndHeader}
      forM_ ["pages", "vital_trend"] \panel -> do
        (_, page) <- testServant scoped $ RUM.rumGetScopedH testPid (Just "performance") Nothing Nothing (Just from) (Just to) Nothing Nothing (Just service) (Just panel) (Just "1") Nothing (Just environment)
        let html = toStrict $ Lucid.renderText $ Lucid.toHtml page
        forM_ ([("/cart", 5), ("/", 3), ("/cart/", 1), ("/a.b+(x)", 1), ("/literal%2Fsegment", 1)] :: [(Text, Int)]) \(route, count) -> do
          let label = ">" <> route <> "</a>"
          html `shouldContainAll` [label]
          let href = T.takeWhile (/= '"') $ snd $ T.breakOnEnd "href=\"" $ fst $ T.breakOn label html
              params = parseQueryText $ encodeUtf8 $ T.replace "&amp;" "&" $ snd $ T.breakOn "?" href
          lookup "from" params `shouldBe` Just (Just from)
          lookup "to" params `shouldBe` Just (Just to)
          query <- maybe (fail "RUM page link omitted its Explorer query") pure $ join $ lookup "query" params
          (_, events) <- testServant tr $ Log.logExplorerDataH testPid def{Log.query = Just query, Log.from = Just from, Log.to = Just to}
          events.error `shouldBe` Nothing
          events.queryResultCount `shouldBe` count
          forM_ [events.nextUrl, events.resetLogsUrl, events.recentUrl] \url ->
            join (lookup "query" $ parseQueryText $ encodeUtf8 $ snd $ T.breakOn "?" url) `shouldBe` Just query
