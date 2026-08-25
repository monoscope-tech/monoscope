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
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import System.Config qualified as Config
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldMatchList, shouldSatisfy)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


-- | Put the project on a known plan/provider footing. `billing_day` anchors the cycle, and
-- `usage_last_reported` is the watermark the job is supposed to clamp.
-- `sub_id` is what decides the billing provider (a `sub_` prefix means Stripe), and no
-- provider means no chunks are ever cut — which is what silently emptied the row-level
-- assertions here the first time round.
setBilling :: TestResources -> Text -> Maybe Text -> Maybe Text -> UTCTime -> IO ()
setBilling tr plan subItem subId lastReported = withResource tr.trPool \conn ->
  void
    $ PGS.execute
      conn
      [sql| UPDATE projects.projects
               SET payment_plan = ?, first_sub_item_id = ?, sub_id = ?, billing_day = ?, usage_last_reported = ?
             WHERE id = ? |]
      (plan, subItem, subId, billingAnchor, lastReported, pid)


stripeSub :: Maybe Text
stripeSub = Just "sub_test_reportusage"


submissions :: TestResources -> IO [(UTCTime, UTCTime, Int, Text)]
submissions tr = withResource tr.trPool \conn ->
  PGS.query
    conn
    [sql| SELECT window_start, window_end, quantity::INT, status
            FROM projects.usage_report_submissions WHERE project_id = ? ORDER BY created_at |]
    (PGS.Only pid)


-- | (meter_kind, quantity, status) per chunk — the meter dimension is what the
-- three-meter pricing turns on, so most assertions below are about it.
meteredSubmissions :: TestResources -> IO [(Text, Int, Text)]
meteredSubmissions tr = withResource tr.trPool \conn ->
  PGS.query
    conn
    [sql| SELECT meter_kind, quantity::INT, status
            FROM projects.usage_report_submissions WHERE project_id = ? ORDER BY meter_kind, created_at |]
    (PGS.Only pid)


-- | (total_requests, total_metrics, total_replays) recorded for the window.
dailyUsage :: TestResources -> IO [(Int, Int, Int)]
dailyUsage tr = withResource tr.trPool \conn ->
  PGS.query
    conn
    [sql| SELECT total_requests::INT, total_metrics::INT, total_replays::INT
            FROM apis.daily_usage WHERE project_id = ? ORDER BY window_end |]
    (PGS.Only pid)


-- | Stripe meter events are addressed by customer + meter event name, so a
-- project without a customer id is unaddressable however many meters are on.
setStripeCustomer :: TestResources -> Maybe Text -> IO ()
setStripeCustomer tr cust = withResource tr.trPool \conn ->
  void $ PGS.execute conn [sql| UPDATE projects.projects SET customer_id = ? WHERE id = ? |] (cust, pid)


-- | Turn meters on for one run. Dormancy is config, so this is the whole switch.
-- AuthContext carries two copies of EnvConfig (`env` and `config`); set both so
-- the switch works regardless of which one a read site happens to use.
withMeters :: [Projects.MeterKind] -> TestResources -> TestResources
withMeters ms tr =
  tr
    { trATCtx =
        tr.trATCtx
          { Config.env = tr.trATCtx.env{Config.enabledUsageMeters = ms}
          , Config.config = tr.trATCtx.config{Config.enabledUsageMeters = ms}
          }
    }


-- | Insert replay sessions started inside the window. `created_at` is what
-- billing counts; it is written once on a session's first chunk and never moves.
seedReplays :: TestResources -> Int -> UTCTime -> IO ()
seedReplays tr n at = withResource tr.trPool \conn ->
  for_ [1 .. n] \(i :: Int) ->
    void
      $ PGS.execute
        conn
        [sql| INSERT INTO projects.replay_sessions (session_id, project_id, last_event_at, merged, created_at, updated_at)
              VALUES (gen_random_uuid(), ?, ?, FALSE, ?, ?) |]
        (pid, at, addUTCTime (fromIntegral i) at, at)


seedMetrics :: TestResources -> Int -> IO ()
seedMetrics tr n = do
  apiKey <- createTestAPIKey tr pid "report-usage-metrics-key"
  for_ [1 .. n] \(i :: Int) -> ingestMetric tr apiKey ("usage.metric." <> show i) (fromIntegral i) frozenTime


-- | Run the job with provider calls recorded instead of sent, so the drain path
-- reaches its success branch without touching api.stripe.com. Returns the URLs
-- posted to, which is the only place "what did we actually bill" is observable.
runReportRecording :: TestResources -> IO [Text]
runReportRecording tr = do
  (reqs, _) <- runTestBgRecordingHTTP frozenTime tr $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.ReportUsage pid)
  pure (map fst reqs)


lastReportedOf :: TestResources -> IO UTCTime
lastReportedOf tr = withResource tr.trPool \conn -> do
  [PGS.Only t] <- PGS.query conn [sql| SELECT usage_last_reported FROM projects.projects WHERE id = ? |] (PGS.Only pid)
  pure t


-- | Usage only exists if telemetry does: with none, totalUsage is 0, no chunks are cut and
-- no submission rows are written — which silently turned the window assertions below into
-- assertions about an empty list.
seedUsage :: TestResources -> IO ()
seedUsage tr = do
  apiKey <- createTestAPIKey tr pid "report-usage-key"
  for_ ([1 .. 3] :: [Int]) \i -> ingestLog tr apiKey ("usage line " <> show i) frozenTime
  n <- withResource tr.trPool \conn -> do
    [PGS.Only c] <- PGS.query conn [sql| SELECT COUNT(*)::INT FROM otel_logs_and_spans WHERE project_id = ?::text |] (PGS.Only pid) :: IO [PGS.Only Int]
    pure c
  n `shouldSatisfy` (> 0)


runReport :: TestResources -> IO ()
runReport tr = void $ runTestBg frozenTime tr $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.ReportUsage pid)


-- A billing day well before frozenTime's day-of-month, so the current cycle started this
-- month and any watermark older than that must be clamped forward.
billingAnchor :: UTCTime
billingAnchor = UTCTime (fromGregorian 2023 1 2) 0


spec :: Spec
spec = around withTestResources do
  describe "ReportUsage" do
    it "reports nothing for a free project" \tr -> do
      setBilling tr "Free" (Just "sub_item_free") stripeSub (addUTCTime (-86400 * 5) frozenTime)

      runReport tr

      submissions tr >>= (`shouldBe` [])

    -- A paid plan with no subscription item has nothing to report against; reporting anyway
    -- would be a submission we cannot attribute.
    it "reports nothing for a paid project with no subscription item" \tr -> do
      setBilling tr "GraduatedPricing" Nothing stripeSub (addUTCTime (-86400 * 5) frozenTime)

      runReport tr

      submissions tr >>= (`shouldBe` [])

    -- The invariant that protects customers: a stale watermark must not drag the window
    -- back into a cycle that has already been invoiced.
    it "never opens a window earlier than the current cycle start" \tr -> do
      let ancient = UTCTime (fromGregorian 2023 6 1) 0
      seedUsage tr
      setBilling tr "GraduatedPricing" (Just "sub_item_1") stripeSub ancient

      runReport tr

      rows <- submissions tr
      length rows `shouldSatisfy` (> 0)
      for_ rows \(wStart, _, _, _) -> wStart `shouldSatisfy` (> ancient)

    -- Bookkeeping is committed before any provider call, so the watermark advances whether
    -- or not the provider could be reached. Without this, a provider outage would replay
    -- the same window forever.
    it "advances the reporting watermark even when the provider is unusable" \tr -> do
      let before = addUTCTime (-86400 * 3) frozenTime
      setBilling tr "GraduatedPricing" (Just "sub_item_1") stripeSub before

      runReport tr

      after <- lastReportedOf tr
      after `shouldSatisfy` (> before)

    -- A chunk whose provider call did not succeed must stay visibly unsubmitted, so the
    -- next tick retries it. Marking it submitted regardless would silently drop revenue.
    it "never marks a chunk submitted when the provider call did not succeed" \tr -> do
      seedUsage tr
      setBilling tr "GraduatedPricing" (Just "sub_item_1") stripeSub (addUTCTime (-86400 * 3) frozenTime)

      runReport tr

      rows <- submissions tr
      length rows `shouldSatisfy` (> 0)
      for_ rows \(_, _, _, status) -> status `shouldSatisfy` (/= "submitted")

    it "is idempotent enough to run twice without throwing" \tr -> do
      setBilling tr "GraduatedPricing" (Just "sub_item_1") stripeSub (addUTCTime (-86400 * 3) frozenTime)

      runReport tr
      runReport tr

      -- The point is that neither run threw; odd-jobs retrying the whole job is what
      -- double-submits chunks that already succeeded.
      lastReportedOf tr >>= \t -> t `shouldSatisfy` (<= frozenTime)

  -- Pricing v2: events, metric datapoints and session replays are metered and
  -- priced separately. Each is counted always; each submits only once its meter
  -- is confirmed to exist provider-side.
  describe "ReportUsage / three metered dimensions" do
    -- Paid Stripe project that is fully addressable: chunks reach the drain's
    -- success branch (against the recording HTTP interpreter, never the network).
    let paid tr = do
          setBilling tr "GraduatedPricing" (Just "sub_item_1") stripeSub (addUTCTime (-86400 * 3) frozenTime)
          setStripeCustomer tr (Just "cus_test_reportusage")
        -- Same, but with no Stripe customer: enabled meters are unaddressable.
        paidUnaddressable tr = do
          setBilling tr "GraduatedPricing" (Just "sub_item_1") stripeSub (addUTCTime (-86400 * 3) frozenTime)
          setStripeCustomer tr Nothing

    it "records all three dimensions but only submits the meters that are enabled" \tr0 -> do
      seedUsage tr0
      seedMetrics tr0 4
      seedReplays tr0 3 (addUTCTime (-3600) frozenTime)
      paid tr0

      -- Default config: only the events meter exists in Stripe today.
      void $ runReportRecording tr0

      [(_, metrics, replays)] <- dailyUsage tr0
      -- Counted regardless of dormancy — this is what makes a dormant window
      -- reconcilable by hand once the meter goes live.
      metrics `shouldBe` 4
      replays `shouldBe` 3

      kinds <- L.nub . map (\(k, _, _) -> k) <$> meteredSubmissions tr0
      kinds `shouldBe` ["events"]

    -- A meter that is enabled but has nowhere to send to is a misconfig on money
    -- we are owed, NOT dormancy: the chunk must still exist and be visibly failed
    -- so it is retried and can be audited. Only a disabled meter cuts no chunks.
    it "still cuts an auditable failed chunk when an enabled meter is unaddressable" \tr0 -> do
      let tr = withMeters [minBound .. maxBound] tr0
      seedReplays tr 2 (addUTCTime (-3600) frozenTime)
      paidUnaddressable tr

      -- Unaddressable throws before any HTTP, so this never touches the network.
      urls <- runReportRecording tr
      urls `shouldBe` []

      meteredSubmissions tr >>= (`shouldBe` [("session_replays", 2, "failed")])

    it "submits every dimension to its own Stripe meter once all three are enabled" \tr0 -> do
      let tr = withMeters [minBound .. maxBound] tr0
      seedUsage tr
      seedMetrics tr 4
      seedReplays tr 3 (addUTCTime (-3600) frozenTime)
      paid tr

      urls <- runReportRecording tr

      rows <- meteredSubmissions tr
      map (\(k, q, _) -> (k, q)) rows `shouldMatchList` [("events", 3), ("metric_datapoints", 4), ("session_replays", 3)]
      -- Every chunk drained: the recording interpreter answers 200, so the
      -- success branch is the one under test here.
      for_ rows \(_, _, status) -> status `shouldBe` "submitted"
      length (filter (T.isInfixOf "stripe.com") urls) `shouldBe` 3

    -- The condition that makes replays different from events and metrics: a
    -- project can have replays and no telemetry at all. The old guard keyed on
    -- events+metrics only, so such a window recorded nothing and billed nothing.
    it "records and bills a replay-only window with no events or metrics" \tr0 -> do
      let tr = withMeters [minBound .. maxBound] tr0
      seedReplays tr 2 (addUTCTime (-3600) frozenTime)
      paid tr

      void $ runReportRecording tr

      dailyUsage tr >>= (`shouldBe` [(0, 0, 2)])
      meteredSubmissions tr >>= (`shouldBe` [("session_replays", 2, "submitted")])

    -- Dormancy must be a decision not to submit, not a backlog that fires all at
    -- once the day a meter is switched on.
    it "cuts no chunks for a dormant meter, so enabling it later cannot bill the dormant window" \tr0 -> do
      seedMetrics tr0 4
      paid tr0

      void $ runReportRecording tr0
      meteredSubmissions tr0 >>= (`shouldBe` [])

      -- Now enable metrics and re-run: only usage after the watermark is billable.
      let tr = withMeters [minBound .. maxBound] tr0
      void $ runReportRecording tr
      rows <- meteredSubmissions tr
      map (\(k, q, _) -> (k, q)) rows `shouldBe` []

    -- Lemon Squeezy addresses a usage record only by subscription item, so a
    -- second and third metered variant must exist before those meters can bill —
    -- enabling them in config alone must not POST anything.
    it "keeps a Lemon Squeezy project's extra meters dormant until it has a subscription item for them" \tr0 -> do
      let tr = withMeters [minBound .. maxBound] tr0
      seedUsage tr
      seedMetrics tr 4
      seedReplays tr 3 (addUTCTime (-3600) frozenTime)
      -- A numeric sub_id is what marks the project as Lemon Squeezy.
      setBilling tr "GraduatedPricing" (Just "912345") (Just "812345") (addUTCTime (-86400 * 3) frozenTime)

      urls <- runReportRecording tr

      -- Events falls back to first_sub_item_id; the other two have no item.
      rows <- meteredSubmissions tr
      map (\(k, _, _) -> k) rows `shouldBe` ["events"]
      length (filter (T.isInfixOf "lemonsqueezy.com") urls) `shouldBe` 1

    it "bills a Lemon Squeezy meter once its subscription item is recorded" \tr0 -> do
      let tr = withMeters [minBound .. maxBound] tr0
      seedReplays tr 2 (addUTCTime (-3600) frozenTime)
      setBilling tr "GraduatedPricing" (Just "912345") (Just "812345") (addUTCTime (-86400 * 3) frozenTime)
      void $ runTestBg frozenTime tr $ Projects.setMeterSubItemId pid Projects.SessionReplays "912999"

      void $ runReportRecording tr

      meteredSubmissions tr >>= (`shouldBe` [("session_replays", 2, "submitted")])

    -- A succeeded chunk must never be re-sent: that is a duplicate charge.
    it "does not re-submit a chunk that already succeeded" \tr0 -> do
      let tr = withMeters [minBound .. maxBound] tr0
      seedReplays tr 2 (addUTCTime (-3600) frozenTime)
      paid tr

      first_ <- runReportRecording tr
      length (filter (T.isInfixOf "stripe.com") first_) `shouldBe` 1

      -- Second tick: the window is empty now (watermark advanced) and the earlier
      -- chunk is 'submitted', so nothing should be posted at all.
      second_ <- runReportRecording tr
      filter (T.isInfixOf "stripe.com") second_ `shouldBe` []
      meteredSubmissions tr >>= (`shouldBe` [("session_replays", 2, "submitted")])

    -- Turning a meter off between recording a chunk and draining it must not
    -- churn: the row stays pending and drains cleanly on re-enable.
    it "leaves an unsubmitted chunk alone when its meter is disabled before the drain" \tr0 -> do
      let tr = withMeters [minBound .. maxBound] tr0
      seedReplays tr 2 (addUTCTime (-3600) frozenTime)
      -- Cut the chunk, and leave it unsubmitted without any network call.
      paidUnaddressable tr
      void $ runReportRecording tr
      pendingBefore <- meteredSubmissions tr
      map (\(k, _, s) -> (k, s)) pendingBefore `shouldBe` [("session_replays", "failed")]

      -- Meter switched off: the drain must neither submit nor re-mark it.
      setStripeCustomer tr0 (Just "cus_test_reportusage")
      urls <- runReportRecording tr0
      filter (T.isInfixOf "stripe.com") urls `shouldBe` []
      meteredSubmissions tr0 >>= (`shouldBe` pendingBefore)

      -- Re-enabled and now addressable: the backlog drains.
      void $ runReportRecording tr
      meteredSubmissions tr >>= (`shouldBe` [("session_replays", 2, "submitted")])

    -- Replays are counted on an immutable created_at over a half-open window, so
    -- a session must land in exactly one billing window.
    it "counts a replay session in exactly one window" \tr0 -> do
      let tr = withMeters [minBound .. maxBound] tr0
          earlier = addUTCTime (-86400 * 2) frozenTime
      paid tr
      seedReplays tr 2 earlier

      -- First run bills both; the watermark then sits at frozenTime.
      void $ runReportRecording tr
      meteredSubmissions tr >>= (`shouldBe` [("session_replays", 2, "submitted")])

      -- Same sessions, later window: they must not be counted again.
      void $ runReportRecording tr
      meteredSubmissions tr >>= (`shouldBe` [("session_replays", 2, "submitted")])
      map (\(_, _, r) -> r) <$> dailyUsage tr >>= \replays -> sum replays `shouldBe` 2
