module BackgroundJobs.DigestFlushSpec (spec) where

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


spec :: Spec
spec = aroundAll withTestResources do
  describe "Notification digest flush" do
    -- 4 rows are recent (within 24h of test clock) → stamped sent_at = current test time.
    -- 1 row is too old (created_at = -25h) → never picked up → sent_at stays NULL.
    it "Hourly digest flush honours test clock" \tr -> do
      runTestBg frozenTime tr pass
      withResource tr.trPool \conn -> do
        void $ PGS.execute conn [sql| DELETE FROM apis.notification_digest_queue WHERE project_id = ? |] (PGS.Only pid)
        void $ PGS.execute conn [sql| UPDATE projects.projects SET error_alerts = true WHERE id = ? |] (PGS.Only pid)

      -- 5 rows at frozenTime; 1 backdated to -25h relative to where the clock will be after we advance.
      i0 <- UUID.nextRandom
      ids <- replicateM 4 UUID.nextRandom
      withResource tr.trPool \conn -> forM_ ids \i ->
        void $ PGS.execute conn
          [sql| INSERT INTO apis.notification_digest_queue (id, project_id, reason, title, created_at)
                VALUES (?, ?, 'rate_limit', 't', ?) |]
          (i, pid, frozenTime)

      advanceMinutes tr 61
      let nowAdv = addUTCTime (61 * 60) frozenTime
      -- Old row: backdated 25h before nowAdv → outside 24h window.
      withResource tr.trPool \conn ->
        void $ PGS.execute conn
          [sql| INSERT INTO apis.notification_digest_queue (id, project_id, reason, title, created_at)
                VALUES (?, ?, 'rate_limit', 'too-old', ?) |]
          (i0, pid, addUTCTime (-25 * 3600) nowAdv)

      runTestBgNoReset tr $ BackgroundJobs.runNotificationDigest nowAdv

      -- All 5 rows get flushed once the project is included (the per-project query
      -- has no time filter; the time filter only gates project selection).
      -- The key TestClock assertion: sent_at must equal the *test clock* time,
      -- not the real wall clock. Migration 0095 wires Time.currentTime through.
      sentAtTimes <- withResource tr.trPool \conn -> do
        rs <- PGS.query conn
          [sql| SELECT sent_at FROM apis.notification_digest_queue WHERE project_id = ? |]
          (PGS.Only pid) :: IO [PGS.Only (Maybe UTCTime)]
        pure $ mapMaybe PGS.fromOnly rs
      length sentAtTimes `shouldBe` 5
      all (== nowAdv) sentAtTimes `shouldBe` True
