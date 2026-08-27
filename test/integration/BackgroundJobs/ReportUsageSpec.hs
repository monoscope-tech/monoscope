module BackgroundJobs.ReportUsageSpec (spec) where

-- ReportUsage is the job that bills people. It had no test.
--
-- The invariants worth holding are all about /not/ charging wrongly: a free project must
-- never produce a usage submission, a window must never reach back past the current cycle
-- (charging this month for last month's events is our mistake, not the customer's), and a
-- paid project whose billing provider is unusable must leave an auditable failed row rather
-- than a quietly-submitted one. The job also must not throw: odd-jobs would retry the whole
-- thing and re-submit chunks that already succeeded.

import BackgroundJobs qualified
import Data.List qualified as L
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Pkg.TestUtils
import Relude
import System.Config qualified as Config
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldMatchList, shouldNotSatisfy, shouldSatisfy)


-- | Put the project on a known plan/provider footing. `billing_day` anchors the cycle, and
-- `usage_last_reported` is the watermark the job is supposed to clamp.
-- `sub_id` is what decides the billing provider (a `sub_` prefix means Stripe), and no
-- provider means no chunks are ever cut — which is what silently emptied the row-level
-- assertions here the first time round.
setBilling :: TestResources -> Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> UTCTime -> IO ()
setBilling tr pid plan subItem subId lastReported = withResource tr.trPool \conn ->
  void
    $ PGS.execute
      conn
      [sql| UPDATE projects.projects
               SET payment_plan = ?, first_sub_item_id = ?, sub_id = ?, billing_day = ?, usage_last_reported = ?
             WHERE id = ? |]
      (plan, subItem, subId, billingAnchor, lastReported, pid)


stripeSub :: Maybe Text
stripeSub = Just "sub_test_reportusage"


submissions :: TestResources -> Projects.ProjectId -> IO [(UTCTime, UTCTime, Int, Text)]
submissions tr pid = withResource tr.trPool \conn ->
  PGS.query
    conn
    [sql| SELECT window_start, window_end, quantity::INT, status
            FROM projects.usage_report_submissions WHERE project_id = ? ORDER BY created_at |]
    (PGS.Only pid)


-- | (meter_kind, quantity, status) per chunk — the meter dimension is what the
-- three-meter pricing turns on, so most assertions below are about it.
meteredSubmissions :: TestResources -> Projects.ProjectId -> IO [(Text, Int, Text)]
meteredSubmissions tr pid = withResource tr.trPool \conn ->
  PGS.query
    conn
    [sql| SELECT meter_kind, quantity::INT, status
            FROM projects.usage_report_submissions WHERE project_id = ? ORDER BY meter_kind, created_at |]
    (PGS.Only pid)


-- | (total_requests, total_metrics, total_replays) recorded for the window.
dailyUsage :: TestResources -> Projects.ProjectId -> IO [(Int, Int, Int)]
dailyUsage tr pid = withResource tr.trPool \conn ->
  PGS.query
    conn
    [sql| SELECT total_requests::INT, total_metrics::INT, total_replays::INT
            FROM apis.daily_usage WHERE project_id = ? ORDER BY window_end |]
    (PGS.Only pid)


-- | Stripe meter events are addressed by customer + meter event name, so a
-- project without a customer id is unaddressable however many meters are on.
setStripeCustomer :: TestResources -> Projects.ProjectId -> Maybe Text -> IO ()
setStripeCustomer tr pid cust = withResource tr.trPool \conn ->
  void $ PGS.execute conn [sql| UPDATE projects.projects SET customer_id = ? WHERE id = ? |] (cust, pid)


-- | Turn meters on for one run. Dormancy is config, so this is the whole switch.
-- AuthContext carries two copies of EnvConfig (`env` and `config`); set both so
-- the switch works regardless of which one a read site happens to use.
-- | Insert replay sessions started inside the window. `created_at` is what
-- billing counts; it is written once on a session's first chunk and never moves.
seedReplays :: TestResources -> Projects.ProjectId -> Int -> UTCTime -> IO ()
seedReplays tr pid n at = withResource tr.trPool \conn ->
  for_ [1 .. n] \(i :: Int) ->
    void
      $ PGS.execute
        conn
        [sql| INSERT INTO projects.replay_sessions (session_id, project_id, last_event_at, merged, created_at, updated_at)
              VALUES (gen_random_uuid(), ?, ?, FALSE, ?, ?) |]
        (pid, at, addUTCTime (fromIntegral i) at, at)


seedMetrics :: TestResources -> Projects.ProjectId -> Int -> IO ()
seedMetrics tr pid n = do
  apiKey <- createTestAPIKey tr pid "report-usage-metrics-key"
  for_ [1 .. n] \(i :: Int) -> ingestMetric tr apiKey [] [] ("usage.metric." <> show i) (fromIntegral i) frozenTime


-- | Run the job with provider calls recorded instead of sent, so the drain path
-- reaches its success branch without touching api.stripe.com. Returns (url, body)
-- per request — the body is where the meter identity actually rides, so it is the
-- only place "what did we bill, and against which meter" is observable.
runReportRecording :: TestResources -> Projects.ProjectId -> IO [(Text, Text)]
runReportRecording tr pid = do
  (reqs, _) <- runTestBgRecordingHTTP frozenTime tr $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.ReportUsage pid)
  pure [(u, decodeUtf8 b) | (u, b) <- reqs]


postsTo :: Text -> [(Text, Text)] -> [Text]
postsTo host reqs = [b | (u, b) <- reqs, host `T.isInfixOf` u]


lastReportedOf :: TestResources -> Projects.ProjectId -> IO UTCTime
lastReportedOf tr pid = withResource tr.trPool \conn -> do
  [PGS.Only t] <- PGS.query conn [sql| SELECT usage_last_reported FROM projects.projects WHERE id = ? |] (PGS.Only pid)
  pure t


-- | Usage only exists if telemetry does: with none, totalUsage is 0, no chunks are cut and
-- no submission rows are written — which silently turned the window assertions below into
-- assertions about an empty list.
seedUsage :: TestResources -> Projects.ProjectId -> IO ()
seedUsage tr pid = do
  apiKey <- createTestAPIKey tr pid "report-usage-key"
  for_ ([1 .. 3] :: [Int]) \i -> ingestLog tr apiKey ("usage line " <> show i) frozenTime
  n <- withResource tr.trPool \conn -> do
    [PGS.Only c] <- PGS.query conn [sql| SELECT COUNT(*)::INT FROM otel_logs_and_spans WHERE project_id = ?::text |] (PGS.Only pid) :: IO [PGS.Only Int]
    pure c
  n `shouldSatisfy` (> 0)


runReport :: TestResources -> Projects.ProjectId -> IO ()
runReport tr pid = void $ runTestBg frozenTime tr $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.ReportUsage pid)


-- A billing day well before frozenTime's day-of-month, so the current cycle started this
-- month and any watermark older than that must be clamped forward.
billingAnchor :: UTCTime
billingAnchor = UTCTime (fromGregorian 2023 1 2) 0


spec :: Spec
-- Every example bills a project of its own. Postgres is per-example, but TimeFusion is one
-- shared instance for the whole run keyed only by project id, so billing the shared demo
-- project counted every row every other spec in the shard had ingested — the events quantity
-- then depended on test ordering and drifted upwards (3 → 13 → 24) in CI.
spec = around (\f -> withTestResources \tr -> createTestProject tr "report-usage" >>= \pid -> f (tr, pid)) do
  describe "ReportUsage" do
    it "reports nothing for a free project" \(tr, pid) -> do
      setBilling tr pid "Free" (Just "sub_item_free") stripeSub (addUTCTime (-86400 * 5) frozenTime)

      runReport tr pid

      submissions tr pid >>= (`shouldBe` [])

    -- A paid plan with no subscription item has nothing to report against; reporting anyway
    -- would be a submission we cannot attribute.
    it "reports nothing for a paid project with no subscription item" \(tr, pid) -> do
      setBilling tr pid "GraduatedPricing" Nothing stripeSub (addUTCTime (-86400 * 5) frozenTime)

      runReport tr pid

      submissions tr pid >>= (`shouldBe` [])

    -- The invariant that protects customers: a stale watermark must not drag the window
    -- back into a cycle that has already been invoiced.
    it "never opens a window earlier than the current cycle start" \(tr, pid) -> do
      let ancient = UTCTime (fromGregorian 2023 6 1) 0
      seedUsage tr pid
      setBilling tr pid "GraduatedPricing" (Just "sub_item_1") stripeSub ancient

      runReport tr pid

      rows <- submissions tr pid
      length rows `shouldSatisfy` (> 0)
      for_ rows \(wStart, _, _, _) -> wStart `shouldSatisfy` (> ancient)

    -- Bookkeeping is committed before any provider call, so the watermark advances whether
    -- or not the provider could be reached. Without this, a provider outage would replay
    -- the same window forever.
    it "advances the reporting watermark even when the provider is unusable" \(tr, pid) -> do
      let before = addUTCTime (-86400 * 3) frozenTime
      setBilling tr pid "GraduatedPricing" (Just "sub_item_1") stripeSub before

      runReport tr pid

      after <- lastReportedOf tr pid
      after `shouldSatisfy` (> before)

    -- A chunk whose provider call did not succeed must stay visibly unsubmitted, so the
    -- next tick retries it. Marking it submitted regardless would silently drop revenue.
    it "never marks a chunk submitted when the provider call did not succeed" \(tr, pid) -> do
      seedUsage tr pid
      setBilling tr pid "GraduatedPricing" (Just "sub_item_1") stripeSub (addUTCTime (-86400 * 3) frozenTime)

      runReport tr pid

      rows <- submissions tr pid
      length rows `shouldSatisfy` (> 0)
      for_ rows \(_, _, _, status) -> status `shouldSatisfy` (/= "submitted")

    it "is idempotent enough to run twice without throwing" \(tr, pid) -> do
      setBilling tr pid "GraduatedPricing" (Just "sub_item_1") stripeSub (addUTCTime (-86400 * 3) frozenTime)

      runReport tr pid
      runReport tr pid

      -- The point is that neither run threw; odd-jobs retrying the whole job is what
      -- double-submits chunks that already succeeded.
      lastReportedOf tr pid >>= \t -> t `shouldSatisfy` (<= frozenTime)

  -- Pricing v2: events, metric datapoints and session replays are metered and
  -- priced separately. Each is counted always; each submits only once its meter
  -- is confirmed to exist provider-side.
  describe "ReportUsage / three metered dimensions" do
    -- Paid Stripe project that is fully addressable: chunks reach the drain's
    -- success branch (against the recording HTTP interpreter, never the network).
    let paid tr pid = do
          setBilling tr pid "GraduatedPricing" (Just "sub_item_1") stripeSub (addUTCTime (-86400 * 3) frozenTime)
          setStripeCustomer tr pid (Just "cus_test_reportusage")
        -- Same, but with no Stripe customer: enabled meters are unaddressable.
        paidUnaddressable tr pid = do
          setBilling tr pid "GraduatedPricing" (Just "sub_item_1") stripeSub (addUTCTime (-86400 * 3) frozenTime)
          setStripeCustomer tr pid Nothing

    it "records and submits all three dimensions for an addressable Stripe project" \(tr0, pid) -> do
      seedUsage tr0 pid
      seedMetrics tr0 pid 4
      seedReplays tr0 pid 3 (addUTCTime (-3600) frozenTime)
      paid tr0 pid

      void $ runReportRecording tr0 pid

      [(_, metrics, replays)] <- dailyUsage tr0 pid
      -- Counted independently of whether they submitted, which is what makes any
      -- unbilled window reconcilable by hand afterwards.
      metrics `shouldBe` 4
      replays `shouldBe` 3

      -- There is no enable list: a dimension submits wherever it is addressable.
      -- The off-switch lives provider-side, where a meter with no price attached
      -- receives events and bills nothing.
      kinds <- L.nub . map (\(k, _, _) -> k) <$> meteredSubmissions tr0 pid
      kinds `shouldBe` ["events", "metric_datapoints", "session_replays"]

    -- A meter that is enabled but has nowhere to send to is a misconfig on money
    -- we are owed, NOT dormancy: the chunk must still exist and be visibly failed
    -- so it is retried and can be audited. Only a disabled meter cuts no chunks.
    it "still cuts an auditable failed chunk when an enabled meter is unaddressable" \(tr0, pid) -> do
      let tr = tr0
      seedReplays tr pid 2 (addUTCTime (-3600) frozenTime)
      paidUnaddressable tr pid

      -- Unaddressable throws before any HTTP, so this never touches the network.
      reqs <- runReportRecording tr pid
      reqs `shouldBe` []

      meteredSubmissions tr pid >>= (`shouldBe` [("session_replays", 2, "failed")])

    it "submits every dimension to its own Stripe meter once all three are enabled" \(tr0, pid) -> do
      let tr = tr0
      seedUsage tr pid
      seedMetrics tr pid 4
      seedReplays tr pid 3 (addUTCTime (-3600) frozenTime)
      paid tr pid

      reqs <- runReportRecording tr pid

      rows <- meteredSubmissions tr pid
      map (\(k, q, _) -> (k, q)) rows `shouldMatchList` [("events", 3), ("metric_datapoints", 4), ("session_replays", 3)]
      -- Every chunk drained: the recording interpreter answers 200, so the
      -- success branch is the one under test here.
      for_ rows \(_, _, status) -> status `shouldBe` "submitted"

      -- The point of the whole feature: each dimension rides its OWN Stripe meter.
      -- Asserting only on row counts would pass even if all three POSTs carried
      -- event_name=events_usage, which is the bug this pins.
      -- wreq form-encodes the keys, so payload[value] arrives as payload%5Bvalue%5D.
      let bodies = T.replace "%5B" "[" . T.replace "%5D" "]" <$> postsTo "stripe.com" reqs
          meterOf b = find (`T.isInfixOf` b) ["events_usage", "metrics_usage", "session_replays_usage"]
          valueOf b = viaNonEmpty head $ mapMaybe (T.stripPrefix "payload[value]=") (T.splitOn "&" b)
      length bodies `shouldBe` 3
      mapMaybe (\b -> (,) <$> meterOf b <*> valueOf b) bodies
        `shouldMatchList` [("events_usage", "3"), ("metrics_usage", "4"), ("session_replays_usage", "3")]

    -- The condition that makes replays different from events and metrics: a
    -- project can have replays and no telemetry at all. The old guard keyed on
    -- events+metrics only, so such a window recorded nothing and billed nothing.
    it "records and bills a replay-only window with no events or metrics" \(tr0, pid) -> do
      let tr = tr0
      seedReplays tr pid 2 (addUTCTime (-3600) frozenTime)
      paid tr pid

      void $ runReportRecording tr pid

      dailyUsage tr pid >>= (`shouldBe` [(0, 0, 2)])
      meteredSubmissions tr pid >>= (`shouldBe` [("session_replays", 2, "submitted")])

    -- Dormancy must be a decision not to submit, not a backlog that fires all at
    -- once the day a meter becomes billable. With no enable list, the remaining
    -- dormant case is a Lemon Squeezy project with no subscription item at all —
    -- nothing on it has anywhere to bill.
    it "cuts no chunks for a dormant meter, so making it billable later cannot charge the dormant window" \(tr0, pid) -> do
      seedReplays tr0 pid 3 (addUTCTime (-3600) frozenTime)
      setBilling tr0 pid "GraduatedPricing" Nothing (Just "812345") (addUTCTime (-86400 * 3) frozenTime)

      void $ runReportRecording tr0 pid
      meteredSubmissions tr0 pid >>= (`shouldBe` [])

      -- Now give it an item. The window that was dormant stays unbilled: only usage
      -- after the watermark can be charged. Buffering the dormant period and draining
      -- it the day a meter goes live is the backlog leak we have already hit.
      void $ runTestBg frozenTime tr0 $ Projects.setMeterSubItemId pid Projects.SessionReplays "912999"
      void $ runReportRecording tr0 pid
      after <- meteredSubmissions tr0 pid
      map (\(_, q, _) -> q) after `shouldNotSatisfy` elem 3000

    -- An LS subscription carries one item billed at the events rate, so every
    -- dimension rides it restated in that meter's units. The arithmetic IS the price:
    -- get the factor wrong and the customer is charged 10x or a 1000th of the right
    -- amount, silently, which is why these are exact rather than "greater than zero".
    it "restates every Lemon Squeezy dimension in events units on the one item" \(tr0, pid) -> do
      let tr = tr0
      seedUsage tr pid
      seedMetrics tr pid 40
      seedReplays tr pid 3 (addUTCTime (-3600) frozenTime)
      -- A numeric sub_id is what marks the project as Lemon Squeezy.
      setBilling tr pid "GraduatedPricing" (Just "912345") (Just "812345") (addUTCTime (-86400 * 3) frozenTime)

      reqs <- runReportRecording tr pid

      -- 40 datapoints at $1/10M is 4 events-units at $1/1M; 3 replays at $1/1,000 is
      -- 3,000. Events are already in the meter's own unit and pass through: seedUsage
      -- ingests 3 logs.
      rows <- meteredSubmissions tr pid
      map (\(k, q, _) -> (k, q)) rows `shouldMatchList` [("events", 3), ("metric_datapoints", 4), ("session_replays", 3000)]
      -- All three against the one item, because there is only one.
      postsTo "lemonsqueezy.com" reqs `shouldSatisfy` \bs -> length bs == 3 && all (T.isInfixOf "912345") bs

    it "bills a Lemon Squeezy meter once its subscription item is recorded" \(tr0, pid) -> do
      let tr = tr0
      seedReplays tr pid 2 (addUTCTime (-3600) frozenTime)
      setBilling tr pid "GraduatedPricing" (Just "912345") (Just "812345") (addUTCTime (-86400 * 3) frozenTime)
      void $ runTestBg frozenTime tr $ Projects.setMeterSubItemId pid Projects.SessionReplays "912999"

      reqs <- runReportRecording tr pid

      -- A per-meter item is priced for its own dimension, so the raw count goes over
      -- the wire — no restating into events units, which is only for the shared item.
      meteredSubmissions tr pid >>= (`shouldBe` [("session_replays", 2, "submitted")])
      -- Billed against the replays subscription item, NOT the events item that
      -- first_sub_item_id falls back to — a fallback that leaked past its own
      -- meter would charge replays against the events variant.
      case postsTo "lemonsqueezy.com" reqs of
        [body] -> do
          body `shouldSatisfy` T.isInfixOf "912999"
          body `shouldSatisfy` (not . T.isInfixOf "912345")
        other -> fail $ "expected exactly one Lemon Squeezy usage record, got " <> show (length other)

    -- A succeeded chunk must never be re-sent: that is a duplicate charge.
    it "does not re-submit a chunk that already succeeded" \(tr0, pid) -> do
      let tr = tr0
      seedReplays tr pid 2 (addUTCTime (-3600) frozenTime)
      paid tr pid

      first_ <- runReportRecording tr pid
      length (postsTo "stripe.com" first_) `shouldBe` 1

      -- Second tick: the window is empty now (watermark advanced) and the earlier
      -- chunk is 'submitted', so nothing should be posted at all.
      second_ <- runReportRecording tr pid
      postsTo "stripe.com" second_ `shouldBe` []
      meteredSubmissions tr pid >>= (`shouldBe` [("session_replays", 2, "submitted")])

    -- Turning a meter off between recording a chunk and draining it must not
    -- churn: the row stays pending and drains cleanly on re-enable.
    it "drains a chunk that was unaddressable when it was cut, once it can be addressed" \(tr0, pid) -> do
      let tr = tr0
      seedReplays tr pid 2 (addUTCTime (-3600) frozenTime)
      -- Cut the chunk, and leave it unsubmitted without any network call.
      paidUnaddressable tr pid
      void $ runReportRecording tr pid
      pendingBefore <- meteredSubmissions tr pid
      map (\(k, _, s) -> (k, s)) pendingBefore `shouldBe` [("session_replays", "failed")]

      -- A misconfig cuts a chunk on purpose, so money we are actually owed leaves an
      -- auditable, retriable row rather than vanishing. Once addressable, it drains
      -- — and is submitted exactly once.
      setStripeCustomer tr0 pid (Just "cus_test_reportusage")
      void $ runReportRecording tr pid
      meteredSubmissions tr pid >>= (`shouldBe` [("session_replays", 2, "submitted")])
      reqs <- runReportRecording tr pid
      postsTo "stripe.com" reqs `shouldBe` []

    -- Replays are counted on an immutable created_at over a half-open window, so
    -- a session must land in exactly one billing window.
    it "counts a replay session in exactly one window" \(tr0, pid) -> do
      let tr = tr0
          earlier = addUTCTime (-86400 * 2) frozenTime
      paid tr pid
      seedReplays tr pid 2 earlier

      -- First run bills both; the watermark then sits at frozenTime.
      void $ runReportRecording tr pid
      meteredSubmissions tr pid >>= (`shouldBe` [("session_replays", 2, "submitted")])

      -- Same sessions, later window: they must not be counted again.
      void $ runReportRecording tr pid
      meteredSubmissions tr pid >>= (`shouldBe` [("session_replays", 2, "submitted")])
      map (\(_, _, r) -> r) <$> dailyUsage tr pid >>= \replays -> sum replays `shouldBe` 2
