module BackgroundJobs.DashboardAutoProvisionSpec (spec) where

import BackgroundJobs qualified
import Data.Pool (withResource)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, around, describe, it, shouldBe)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


seedMetric :: TestResources -> Text -> IO ()
seedMetric tr metricName = withResource tr.trPool \conn ->
  void
    $ PGS.execute
      conn
      [sql| INSERT INTO otel_metrics_meta (project_id, metric_name, metric_type, first_seen_at, last_seen_at, first_timestamp, last_timestamp)
            VALUES (?, ?, 'SUM', now(), now(), now(), now()) ON CONFLICT DO NOTHING |]
      (pid, metricName)


-- Restricted to the templates this spec exercises so dashboards seeded by other
-- fixtures (e.g. _overview.yaml) can't break the assertions.
provisionedTemplates :: TestResources -> IO [Text]
provisionedTemplates tr = withResource tr.trPool \conn ->
  fmap PGS.fromOnly <$> PGS.query conn [sql| SELECT base_template FROM projects.dashboards WHERE project_id = ? AND base_template IN ('postgresql.yaml', 'redis.yaml', 'mysql.yaml') ORDER BY base_template |] (PGS.Only pid)


runProvision :: TestResources -> IO ()
runProvision tr = void $ runTestBg frozenTime tr $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.DashboardsAutoProvision frozenTime)


spec :: Spec
spec = around withTestResources do
  describe "DashboardsAutoProvision" do
    it "creates dashboards for detected metric namespaces, idempotently, and respects deletion as opt-out" \tr -> do
      -- postgresql metrics present -> postgresql dashboard created (and only that one)
      seedMetric tr "postgresql.backends"
      runProvision tr
      afterFirst <- provisionedTemplates tr
      afterFirst `shouldBe` ["postgresql.yaml"]

      -- re-running creates no duplicates; a newly detected namespace adds its dashboard
      seedMetric tr "redis.memory.used"
      runProvision tr
      runProvision tr
      afterSecond <- provisionedTemplates tr
      afterSecond `shouldBe` ["postgresql.yaml", "redis.yaml"]

      -- deleting an auto-created dashboard is a respected opt-out: never recreated
      withResource tr.trPool \conn ->
        void $ PGS.execute conn [sql| DELETE FROM projects.dashboards WHERE project_id = ? AND base_template = 'postgresql.yaml' |] (PGS.Only pid)
      runProvision tr
      afterDelete <- provisionedTemplates tr
      afterDelete `shouldBe` ["redis.yaml"]

    it "adopts an existing manually-created template dashboard instead of duplicating it" \tr -> do
      seedMetric tr "mysql.threads"
      withResource tr.trPool \conn ->
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO projects.dashboards (project_id, created_by, base_template, title)
                  VALUES (?, '00000000-0000-0000-0000-000000000001', 'mysql.yaml', 'My MySQL') |]
            (PGS.Only pid)
      runProvision tr
      dashboards <- provisionedTemplates tr
      dashboards `shouldBe` ["mysql.yaml"]
