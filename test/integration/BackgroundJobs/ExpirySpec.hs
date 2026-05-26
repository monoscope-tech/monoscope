module BackgroundJobs.ExpirySpec (spec) where

import BackgroundJobs qualified
import Data.Pool (withResource)
import Data.Time (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


countShareRow :: TestResources -> UUID.UUID -> IO Int
countShareRow tr i = withResource tr.trPool \conn -> do
  [PGS.Only n] <- PGS.query conn
    [sql| SELECT COUNT(*)::INT FROM apis.share_events WHERE id = ? |]
    (PGS.Only i) :: IO [PGS.Only Int]
  pure n


countReplay :: TestResources -> UUID.UUID -> IO Int
countReplay tr sid = withResource tr.trPool \conn -> do
  [PGS.Only n] <- PGS.query conn
    [sql| SELECT COUNT(*)::INT FROM projects.replay_sessions WHERE session_id = ? |]
    (PGS.Only sid) :: IO [PGS.Only Int]
  pure n


spec :: Spec
spec = aroundAll withTestResources do
  describe "Background expiry jobs" do
    -- 30d retention + 48h grace = 32d cutoff. Below boundary: row survives;
    -- above boundary: row deleted. Cutoff is computed from Time.currentTime.
    it "ExpireShareEvents respects 30d+48h boundary on test clock" \tr -> do
      runTestBg frozenTime tr pass  -- reset clock for this test
      i <- UUID.nextRandom
      ev <- UUID.nextRandom
      withResource tr.trPool \conn ->
        void $ PGS.execute conn
          [sql| INSERT INTO apis.share_events (id, project_id, created_at, event_id, event_type, event_created_at)
                VALUES (?, ?, ?, ?, 'log', ?) |]
          (i, pid, frozenTime, ev, frozenTime)

      -- At +31d the share row is still inside the 32d retention window.
      advanceDays tr 31
      runTestBgNoReset tr $ BackgroundJobs.processBackgroundJob tr.trATCtx BackgroundJobs.ExpireShareEvents
      surviving <- countShareRow tr i
      surviving `shouldBe` 1

      -- +2d more (total +33d) takes it past the 32d cutoff.
      advanceDays tr 2
      runTestBgNoReset tr $ BackgroundJobs.processBackgroundJob tr.trATCtx BackgroundJobs.ExpireShareEvents
      gone <- countShareRow tr i
      gone `shouldBe` 0

    -- replayRetentionDays = 30. The expiry query joins projects.projects on s3_bucket IS NULL.
    -- The demo project meets that condition; the BYO-bucket case is excluded by design.
    it "ExpireReplayData respects 30d retention boundary on test clock" \tr -> do
      runTestBg frozenTime tr pass  -- reset clock for this test
      sid <- UUID.nextRandom
      withResource tr.trPool \conn ->
        void $ PGS.execute conn
          [sql| INSERT INTO projects.replay_sessions
                  (session_id, project_id, last_event_at)
                VALUES (?, ?, ?) |]
          (sid, pid, frozenTime)

      advanceDays tr 29
      runTestBgNoReset tr $ BackgroundJobs.processBackgroundJob tr.trATCtx BackgroundJobs.ExpireReplayData
      surviving <- countReplay tr sid
      surviving `shouldBe` 1

      advanceDays tr 2
      runTestBgNoReset tr $ BackgroundJobs.processBackgroundJob tr.trATCtx BackgroundJobs.ExpireReplayData
      gone <- countReplay tr sid
      gone `shouldBe` 0
