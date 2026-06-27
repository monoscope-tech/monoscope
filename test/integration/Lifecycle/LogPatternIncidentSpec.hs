module Lifecycle.LogPatternIncidentSpec (spec) where

import BackgroundJobs qualified
import Data.Pool (withResource)
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Servant qualified
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


spec :: Spec
spec = around withTestResources do
  describe "Log Pattern Incident Lifecycle" do
    it "ingest -> baseline -> spike -> notify -> ack (24h cooldown) -> re-spike suppressed -> expire -> re-spike fires" \tr -> do
      runTestBg frozenTime tr pass

      let patHash = "incident-lifecycle-hash" :: Text
          srcField = "summary" :: Text
          uid = (Servant.getResponse tr.trSessAndHeader).user.id

      -- 1. Seed pattern + 50h of stable baseline at 600/hr
      void $ runTestBg frozenTime tr $ LogPatterns.upsertLogPattern LogPatterns.UpsertPattern
        { projectId = pid, logPattern = "Lifecycle <*> pattern", hash = patHash
        , sourceField = srcField, serviceName = Just "test-svc", logLevel = Just "ERROR"
        , traceId = Nothing, sampleMessage = Just "lifecycle pattern", eventCount = 600
        , isError = True }
      forM_ ([-50 .. -1] :: [Int]) \h ->
        void $ runTestBg frozenTime tr $ LogPatterns.upsertHourlyStat pid srcField patHash
          (addUTCTime (fromIntegral h * 3600) frozenTime) 600
      runTestBg frozenTime tr $ BackgroundJobs.calculateLogPatternBaselines pid

      -- 2. Spike in the current hour: raw 600 -> projected 600*4 = 2400 vs baseline ~600
      void $ runTestBg frozenTime tr $ LogPatterns.upsertHourlyStat pid srcField patHash frozenTime 600
      runTestBg frozenTime tr $ BackgroundJobs.detectLogPatternSpikes pid frozenTime tr.trATCtx
      runTestBg frozenTime tr $ BackgroundJobs.detectLogPatternSpikes pid frozenTime tr.trATCtx

      issues1 <- withResource tr.trPool \conn ->
        PGS.query conn
          [sql| SELECT id FROM apis.issues
                WHERE project_id = ? AND target_hash = ? AND issue_type = 'log_pattern_rate_change'
                  AND acknowledged_at IS NULL
                ORDER BY created_at DESC LIMIT 1 |]
          (pid, patHash) :: IO [Only Issues.IssueId]
      let issueId = case issues1 of (Only iid : _) -> iid; _ -> error "no issue fired on initial spike"

      -- 3. Acknowledge -> opens 24h cooldown
      runTestBgNoReset tr $ Issues.acknowledgeIssue issueId uid

      -- 4. +12h: seed fresh spike in the new hour; cooldown should suppress
      advanceHours tr 12
      t12 <- getTestTime tr.trTestClock
      void $ runTestBgNoReset tr $ LogPatterns.upsertHourlyStat pid srcField patHash t12 600
      runTestBgNoReset tr $ BackgroundJobs.detectLogPatternSpikes pid t12 tr.trATCtx
      runTestBgNoReset tr $ BackgroundJobs.detectLogPatternSpikes pid t12 tr.trATCtx
      countAfter12 <- countOpenIssues tr patHash
      countAfter12 `shouldBe` 0

      -- 5. +25h since ack: cooldown expired, new spike fires
      advanceHours tr 13
      t25 <- getTestTime tr.trTestClock
      void $ runTestBgNoReset tr $ LogPatterns.upsertHourlyStat pid srcField patHash t25 600
      runTestBgNoReset tr $ BackgroundJobs.detectLogPatternSpikes pid t25 tr.trATCtx
      runTestBgNoReset tr $ BackgroundJobs.detectLogPatternSpikes pid t25 tr.trATCtx
      countAfter25 <- countOpenIssues tr patHash
      countAfter25 `shouldSatisfy` (>= 1)


countOpenIssues :: TestResources -> Text -> IO Int
countOpenIssues tr h = withResource tr.trPool \conn -> do
  [Only n] <- PGS.query conn
    [sql| SELECT COUNT(*)::INT FROM apis.issues
          WHERE project_id = ? AND target_hash = ? AND issue_type = 'log_pattern_rate_change'
            AND acknowledged_at IS NULL |]
    (pid, h) :: IO [Only Int]
  pure n
