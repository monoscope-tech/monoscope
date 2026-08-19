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
import Data.Pool (withResource)
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)


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
