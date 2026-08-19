module System.MigrationRunnerSpec (spec) where

-- The migration runner, and specifically its failure mode.
--
-- Migrations are append-only because the runner checksums each file: edit one that has
-- already run and the library stops at that file, so every migration after it never applies
-- and the app boots on a half-built schema. That is silent, it is not fixable after the fact
-- (renaming a file changes its checksum too), and it has already cost one crash loop.
--
-- These run against a scratch directory rather than static/migrations, so they can edit an
-- "applied" file — the thing nobody may do to the real ones.

import Data.Pool (withResource)
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.Migration qualified as Migrations
import Pkg.TestUtils
import Relude
import System.Config (runPendingMigrations)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, anyException, around, describe, it, shouldBe, shouldSatisfy, shouldThrow)


-- | Distinct table names per file so "did this migration run?" is answerable by looking for
-- the table, independent of what the runner reports.
writeMigration :: FilePath -> String -> Text -> IO ()
writeMigration dir name body = writeFileText (dir <> "/" <> name) body


createsTable :: Text -> Text
createsTable t = "CREATE TABLE IF NOT EXISTS " <> t <> " (id int);"


tableExists :: TestResources -> Text -> IO Bool
tableExists tr t = withResource tr.trPool \conn -> do
  [PGS.Only n] <-
    PGS.query conn [sql| SELECT COUNT(*)::INT FROM information_schema.tables WHERE table_name = ? |] (PGS.Only t)
      :: IO [PGS.Only Int]
  pure (n > 0)


isSuccess :: Migrations.MigrationResult String -> Bool
isSuccess = \case
  Migrations.MigrationSuccess -> True
  Migrations.MigrationError _ -> False


run :: TestResources -> FilePath -> IO (Migrations.MigrationResult String)
run tr dir = withResource tr.trPool \conn -> runPendingMigrations conn dir


spec :: Spec
spec = around withTestResources do
  describe "runPendingMigrations" do
    it "applies every pending file, and is a no-op on the second run" \tr -> do
      withSystemTempDirectory "migr" \dir -> do
        writeMigration dir "9001_first.sql" (createsTable "zz_migr_first")
        writeMigration dir "9002_second.sql" (createsTable "zz_migr_second")

        firstRun <- run tr dir
        firstRun `shouldSatisfy` isSuccess
        tableExists tr "zz_migr_first" >>= (`shouldBe` True)
        tableExists tr "zz_migr_second" >>= (`shouldBe` True)

        -- Nothing is pending now, so this must not re-run anything or fail.
        secondRun <- run tr dir
        secondRun `shouldSatisfy` isSuccess

    it "applies a file added after the others" \tr -> do
      withSystemTempDirectory "migr" \dir -> do
        writeMigration dir "9001_first.sql" (createsTable "zz_add_first")
        void $ run tr dir

        writeMigration dir "9002_later.sql" (createsTable "zz_add_later")
        result <- run tr dir

        result `shouldSatisfy` isSuccess
        tableExists tr "zz_add_later" >>= (`shouldBe` True)

    -- The incident in one test: editing an applied file makes the runner stop there, and the
    -- file after it never runs. The runner must report the error so the caller can refuse to
    -- boot — the old behaviour logged and continued, which is what hid it.
    it "reports an error when an already-applied file is edited, and does not apply the rest" \tr -> do
      withSystemTempDirectory "migr" \dir -> do
        writeMigration dir "9001_first.sql" (createsTable "zz_edit_first")
        void $ run tr dir

        -- Edit the applied file (changing its checksum) and add a new one after it.
        writeMigration dir "9001_first.sql" (createsTable "zz_edit_first" <> " -- edited")
        writeMigration dir "9002_after.sql" (createsTable "zz_edit_after")

        result <- run tr dir

        result `shouldSatisfy` (not . isSuccess)
        -- This is the damage the error is warning about: the later migration is skipped.
        tableExists tr "zz_edit_after" >>= (`shouldBe` False)

    -- Invalid SQL surfaces as an exception rather than a MigrationError return. Either way
    -- it cannot pass silently, which is the property that matters: startup must not continue
    -- onto a half-built schema.
    it "does not let a migration containing invalid SQL pass" \tr -> do
      withSystemTempDirectory "migr" \dir -> do
        writeMigration dir "9001_broken.sql" "THIS IS NOT SQL;"

        run tr dir `shouldThrow` anyException
