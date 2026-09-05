module Pages.RealUserMonitoringSpec (spec) where

import Data.Cache qualified as Cache
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.Quasi (uuid)
import Database.PostgreSQL.Simple qualified as PG
import Lucid qualified
import Models.Telemetry.RUM qualified as RUMData
import Pages.BodyWrapper (PageCtx (..))
import Pages.Components (Deferred (..))
import Pages.RealUserMonitoring qualified as RUM
import Pkg.TestUtils
import Relude
import System.Config (AuthContext (..))
import Test.Hspec


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
  fmap fold . forM ["services", "pulse", "pages", "vitals", "errors", "sessions", "audience"] $ \panel ->
    renderPanel tr tab query sessionFilterM selected service (Just panel)


renderPanel :: TestResources -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO Text
renderPanel tr tab query sessionFilterM selected service panel = do
  (_, page) <- testServant tr $ RUM.rumGetH testPid tab query sessionFilterM Nothing Nothing (Just "24H") selected service panel (Just "1")
  pure $ toStrict $ Lucid.renderText $ Lucid.toHtml page


shouldContainAll :: Text -> [Text] -> Expectation
shouldContainAll haystack needles = case filter (not . (`T.isInfixOf` haystack)) needles of
  [] -> pass
  missing -> expectationFailure $ "missing from rendered page: " <> show missing


spec :: Spec
spec = sequential $ aroundAll withTestResources do
  describe "Real User Monitoring" do
    it "emptyProject_explainsBrowserTelemetryAndOffersTheDashboard" \tr -> do
      (_, shell) <- testServant tr $ RUM.rumGetH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing Nothing Nothing
      toStrict (Lucid.renderText $ Lucid.toHtml shell) `shouldContainAll` ["hx-trigger=\"load\"", "deferred=1", "skeleton-shimmer"]
      -- The shell stands in for the tab it is loading, so switching tabs does not reflow.
      (_, sessionsShell) <- testServant tr $ RUM.rumGetH testPid (Just "sessions") Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing Nothing Nothing
      toStrict (Lucid.renderText $ Lucid.toHtml sessionsShell) `shouldContainAll` ["grid-cols-5", "skeleton-shimmer"]
      html <- renderPage tr Nothing Nothing Nothing Nothing
      html `shouldContainAll` ["No browser telemetry yet", "Install the browser SDK", "Open RUM dashboard", "tabs tabs-box tabs-outline", "empty-state"]

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

      (_, RUM.RumGet (PageCtx _ overviewBody)) <- testServant tr $ RUM.rumGetH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing (Just "pulse") (Just "1")
      overviewData <- case overviewBody of
        DeferredBody loaded -> pure loaded
        DeferredShell{} -> fail "RUM answered with the deferred shell when asked for the body"
      overviewData.summary.sessions `shouldBe` 2
      overviewData.summary.pageViews `shouldBe` 2
      overviewData.summary.errors `shouldBe` 1
      overviewData.degradedPanels `shouldBe` []
      overview <- renderPage tr Nothing Nothing Nothing Nothing
      -- The unscoped read caches under an unscoped key; `service` is part of that key so a
      -- scoped page can never be served these rows.
      isJust <$> Cache.lookup tr.trATCtx.rumCache (RUMData.RumCacheKey testPid RUMData.VitalSamplesQuery Nothing Nothing Nothing Nothing (Just "24H")) `shouldReturn` True
      overview `shouldContainAll` ["page views", "browser error", "Largest Contentful Paint", "2.20 s", "/checkout", "Ada Lovelace"]

      -- The tab strip and time picker must not wait on seven 24-hour scans: the request that
      -- paints the page answers with a skeleton that fetches the panels itself. Panel data
      -- appearing here again would mean a tab click is back to seconds of blank page.
      (_, shellPage) <- testServant tr $ RUM.rumGetH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing Nothing Nothing
      let shell = toStrict $ Lucid.renderText $ Lucid.toHtml shellPage
      shell `shouldContainAll` ["Real User Monitoring", "tabs tabs-box tabs-outline", "id=\"rum-page\"", "hx-trigger=\"load\"", "deferred=1"]
      T.isInfixOf "Ada Lovelace" shell `shouldBe` False

      sessions <- renderPage tr (Just "sessions") Nothing (Just "errors") (Just sessionId)
      sessions `shouldContainAll` ["With errors", "Ada Lovelace", "Watch replay", "initialSession=\"00000000-0000-0000-0000-000000000042\"", "hx-target=\"#rum-replay-workspace\"", "hx-sync=\"#rum-replay-workspace:replace\""]
      T.isInfixOf "/search" sessions `shouldBe` False

      replaySessions <- renderPage tr (Just "sessions") Nothing (Just "replays") Nothing
      T.isInfixOf "No recording" replaySessions `shouldBe` False
      T.isInfixOf "Merged replay" replaySessions `shouldBe` True

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
      Cache.purge tr.trATCtx.rumCache
      apiKey <- createTestAPIKey tr testPid "rum-service-key"
      -- Reuses the session that already has a recording, so the scoped page can be checked
      -- for the replay badge as well as the rows.
      browserSpan apiKey "30000000000000000000000000000003" "3000000000000001" [("url.path", "/admin/users")] "Pageview · /admin/users" (UUID.toText mergedReplayUuid) Nothing "admin-console" tr

      unscoped <- renderPage tr Nothing Nothing Nothing Nothing
      unscoped `shouldContainAll` ["All services", ">storefront<", ">admin-console<", "/admin/users", "/checkout"]

      scoped <- renderScoped tr Nothing Nothing Nothing Nothing (Just "admin-console")
      scoped `shouldContainAll` ["Every panel below is scoped to admin-console.", "/admin/users"]
      -- The other team's pages are the whole point: if they survive the filter it does nothing.
      T.isInfixOf "/checkout" scoped `shouldBe` False
      -- The scope has to ride every self-link, or one click puts the user back on all services.
      scoped `shouldContainAll` ["service=admin-console", "resource.service.name%20%3D%3D%20%22admin-console%22"]
      -- A recording carries no service, so it can only be trusted where a span already places
      -- the session in this service — attached to that row, never added as a row of its own.
      scoped `shouldContainAll` ["Replay"]
      T.isInfixOf "No recording" scoped `shouldBe` False

      -- A stale selection still resolves: the picker keeps offering it, and an empty result
      -- must read as "this filter matched nothing", never as "you never installed the SDK".
      ghost <- renderScoped tr Nothing Nothing Nothing Nothing (Just "ghost-service")
      ghost `shouldContainAll` ["No browser telemetry for ghost-service in this range", "Show all services", "All services", ">ghost-service<"]
      T.isInfixOf "Install the browser SDK" ghost `shouldBe` False

    it "browserSdkWithoutSdkLanguage_isStillSeenAndPickable" \tr -> do
      -- The OpenTelemetry browser SDKs leave telemetry.sdk.language unset, and RUM used to
      -- filter on that alone. Every browser application in production was therefore invisible:
      -- no page views, no sessions, and an empty service picker with nothing to narrow to.
      Cache.purge tr.trATCtx.rumCache
      apiKey <- createTestAPIKey tr testPid "rum-otel-browser-key"
      otelBrowserSpan apiKey "40000000000000000000000000000004" "4000000000000001" "session-otel" "checkout-web" tr

      page <- renderPage tr Nothing Nothing Nothing Nothing
      page `shouldContainAll` [">checkout-web<", "https://shop.example/cart"]

      -- And it is narrowable, which is the whole point of the picker.
      scoped <- renderScoped tr Nothing Nothing Nothing Nothing (Just "checkout-web")
      scoped `shouldContainAll` ["Every panel below is scoped to checkout-web.", "https://shop.example/cart"]
      T.isInfixOf "/admin/users" scoped `shouldBe` False

    it "sessionLastPage_isTheLatestPageView_notALexicographicResourceUrl" \tr -> do
      -- MAX(path) over every browser span used to pick the alphabetically largest URL: on
      -- real traffic that is a third-party font fetched by the page, shown as the page.
      Cache.purge tr.trATCtx.rumCache
      apiKey <- createTestAPIKey tr testPid "rum-lastpage-key"
      browserSpanAt apiKey "50000000000000000000000000000005" "5000000000000001" [("url.path", "/alpha")] "Pageview · /alpha" "session-lastpage" Nothing "storefront" (addUTCTime (-120) frozenTime) tr
      browserSpanAt apiKey "50000000000000000000000000000005" "5000000000000002" [("url.full", "https://zzz-fonts.example/css2")] "HTTP GET" "session-lastpage" Nothing "storefront" (addUTCTime (-60) frozenTime) tr
      browserSpanAt apiKey "50000000000000000000000000000005" "5000000000000003" [("url.path", "/beta")] "Pageview · /beta" "session-lastpage" Nothing "storefront" (addUTCTime (-30) frozenTime) tr
      row <- renderPanel tr (Just "sessions") (Just "session-lastpage") Nothing Nothing Nothing (Just "sessions")
      row `shouldContainAll` ["/beta"]
      T.isInfixOf "zzz-fonts.example" row `shouldBe` False
      T.isInfixOf "/alpha" row `shouldBe` False

    it "replayOnlySessions_sayWhatTheyAre_insteadOfUnknownPageAndZeroCounts" \tr -> do
      -- A recording whose session id never appears on a span is a real session; stacking
      -- "Unknown page" over "0 views · 0 events" reads as broken data, not as what it is.
      Cache.purge tr.trATCtx.rumCache
      let replayOnlyUuid = [uuid|00000000-0000-0000-0000-000000000045|]
      withResource tr.trPool \conn ->
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name) VALUES (?, ?, ?, ?, 1, ?) ON CONFLICT (session_id) DO UPDATE SET created_at = EXCLUDED.created_at, last_event_at = EXCLUDED.last_event_at" (replayOnlyUuid, testPid, frozenTime, addUTCTime 45 frozenTime, "Replay only user" :: Text)
      rows <- renderPanel tr (Just "sessions") (Just "Replay only user") Nothing Nothing Nothing (Just "sessions")
      rows `shouldContainAll` ["Replay only user", "Recording only — no telemetry events"]
      T.isInfixOf "Unknown page" rows `shouldBe` False
      T.isInfixOf "0 views" rows `shouldBe` False

    it "browserErrors_groupBySignature_withOccurrenceAndSessionCounts" \tr -> do
      -- Twenty copies of the loudest error used to fill the whole panel; issues with masked
      -- identifiers keep every distinct failure visible with its blast radius.
      Cache.purge tr.trATCtx.rumCache
      apiKey <- createTestAPIKey tr testPid "rum-errors-key"
      browserSpan apiKey "60000000000000000000000000000006" "6000000000000001" [("exception.type", "TypeError"), ("exception.message", "Cannot read cart item 123")] "TypeError" "session-err-a" Nothing "storefront" tr
      browserSpan apiKey "60000000000000000000000000000006" "6000000000000002" [("exception.type", "TypeError"), ("exception.message", "Cannot read cart item 456")] "TypeError" "session-err-b" Nothing "storefront" tr
      panel <- renderPanel tr Nothing Nothing Nothing Nothing Nothing (Just "errors")
      panel `shouldContainAll` ["TypeError", "×2", "2 sessions", "Latest occurrence"]
      -- Grouped, not listed: the message renders once for the pair.
      T.count "Cannot read cart item" panel `shouldBe` 1

    it "audiencePanel_classifiesUserAgentsIntoBrowserOsAndDevice" \tr -> do
      Cache.purge tr.trATCtx.rumCache
      apiKey <- createTestAPIKey tr testPid "rum-audience-key"
      let chromeUa = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/151.0.0.0 Safari/537.36"
          iphoneUa = "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Mobile/15E148 Safari/604.1"
      browserSpan apiKey "70000000000000000000000000000007" "7000000000000001" [("url.path", "/a"), ("user_agent.original", chromeUa)] "Pageview · /a" "session-ua-1" Nothing "storefront" tr
      browserSpan apiKey "70000000000000000000000000000007" "7000000000000002" [("url.path", "/a"), ("user_agent.original", chromeUa)] "Pageview · /a" "session-ua-2" Nothing "storefront" tr
      browserSpan apiKey "70000000000000000000000000000007" "7000000000000003" [("url.path", "/b"), ("user_agent.original", iphoneUa)] "Pageview · /b" "session-ua-3" Nothing "storefront" tr
      panel <- renderPanel tr Nothing Nothing Nothing Nothing Nothing (Just "audience")
      panel `shouldContainAll` ["Audience", "Chrome", "Windows", "Safari", "iOS", "Mobile", "Desktop", "2 sessions"]
