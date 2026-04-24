-- Behavior tests for the invariant: @everyone.notify_emails is the single source of
-- truth for who gets paged. Adding a member appends their email; removing strips it;
-- manual edits to the list persist across further member changes.
module Pages.Projects.EveryoneSyncSpec (spec) where

import Data.Pool (withResource)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.ProjectMembers qualified as PM
import Models.Projects.Projects (UserId (..))
import Models.Projects.Projects qualified as Projects
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


newUser :: Projects.UserId -> Text -> TestResources -> IO ()
newUser uid email tr = withResource tr.trPool \conn ->
  void $ PGS.execute conn
    [sql| INSERT INTO users.users (id, first_name, last_name, email)
          VALUES (?, 'T', 'U', ?)
          ON CONFLICT (id) DO UPDATE SET email = EXCLUDED.email |]
    (uid, email)


-- | Reset everyone.notify_emails to [] and drop every project_member except the
-- default test user so each test starts from a known baseline.
resetState :: TestResources -> IO ()
resetState tr = withResource tr.trPool \conn -> do
  void $ PGS.execute conn
    [sql| DELETE FROM projects.project_members WHERE project_id = ? AND user_id <> '00000000-0000-0000-0000-000000000001' |]
    (PGS.Only testPid)
  void $ PGS.execute conn
    [sql| UPDATE projects.project_members SET active = TRUE, deleted_at = NULL
          WHERE project_id = ? AND user_id = '00000000-0000-0000-0000-000000000001' |]
    (PGS.Only testPid)
  void $ PGS.execute conn
    [sql| INSERT INTO projects.teams (project_id, name, handle, is_everyone, notify_emails)
          VALUES (?, 'Everyone', 'everyone', TRUE, '{}')
          ON CONFLICT (project_id, handle) DO UPDATE SET notify_emails = '{}', deleted_at = NULL |]
    (PGS.Only testPid)


fetchEmails :: TestResources -> IO [Text]
fetchEmails tr = do
  Just t <- runTestBg frozenTime tr $ PM.getEveryoneTeam testPid
  pure $ V.toList t.notify_emails


uidFor :: Int -> Projects.UserId
uidFor n = UserId (Unsafe.fromJust $ UUID.fromString $ "00000000-0000-0000-0000-00000000000" <> show n)


spec :: Spec
spec = aroundAll withTestResources $ describe "@everyone notify_emails is authoritative" do
  it "insertProjectMembers appends the member's (lower-cased) email" \tr -> do
    resetState tr
    let u1 = uidFor 2
    newUser u1 "Alice@Example.com" tr
    void $ runTestBg frozenTime tr $ PM.insertProjectMembers [PM.CreateProjectMembers testPid u1 PM.PView]
    emails <- fetchEmails tr
    emails `shouldSatisfy` ("alice@example.com" `elem`)

  it "resolveTeamEmails returns only notify_emails (no implicit member expansion)" \tr -> do
    -- After the previous test, alice is a member AND in notify_emails. Wipe notify_emails
    -- directly and prove resolveTeamEmails no longer returns alice.
    withResource tr.trPool \conn -> void $ PGS.execute conn
      [sql| UPDATE projects.teams SET notify_emails = '{}' WHERE project_id = ? AND is_everyone = TRUE |]
      (PGS.Only testPid)
    Just t <- runTestBg frozenTime tr $ PM.getEveryoneTeam testPid
    PM.resolveTeamEmails t `shouldBe` []

  it "manual removal from @everyone persists across further member additions" \tr -> do
    resetState tr
    let u1 = uidFor 2
        u2 = uidFor 3
    newUser u1 "alice@example.com" tr
    newUser u2 "bob@example.com" tr
    void $ runTestBg frozenTime tr $ PM.insertProjectMembers [PM.CreateProjectMembers testPid u1 PM.PView]
    -- Simulate the user editing the integrations page and removing alice.
    runTestBg frozenTime tr $ PM.setEveryoneTeamEmails testPid V.empty
    -- Add a different member; alice must NOT come back just because she's still on the project.
    void $ runTestBg frozenTime tr $ PM.insertProjectMembers [PM.CreateProjectMembers testPid u2 PM.PView]
    emails <- fetchEmails tr
    emails `shouldSatisfy` ("bob@example.com" `elem`)
    emails `shouldNotSatisfy` ("alice@example.com" `elem`)

  it "softDeleteProjectMembers strips the removed member's email" \tr -> do
    resetState tr
    let u1 = uidFor 2
    newUser u1 "alice@example.com" tr
    void $ runTestBg frozenTime tr $ PM.insertProjectMembers [PM.CreateProjectMembers testPid u1 PM.PView]
    -- Look up the project_members row id for u1 and soft-delete.
    memberId <- withResource tr.trPool \conn -> do
      rs <- PGS.query conn
        [sql| SELECT id FROM projects.project_members WHERE project_id = ? AND user_id = ? |]
        (testPid, u1) :: IO [PGS.Only UUID.UUID]
      case rs of
        [PGS.Only mid] -> pure mid
        _ -> fail "expected exactly one project_members row for u1"
    runTestBg frozenTime tr $ PM.softDeleteProjectMembers (memberId :| [])
    emails <- fetchEmails tr
    emails `shouldNotSatisfy` ("alice@example.com" `elem`)

  it "externally-added emails survive deactivateNonOwnerMembers" \tr -> do
    resetState tr
    let u1 = uidFor 2
    newUser u1 "alice@example.com" tr
    void $ runTestBg frozenTime tr $ PM.insertProjectMembers [PM.CreateProjectMembers testPid u1 PM.PView]
    -- Manually add an oncall@ address that isn't tied to any member.
    runTestBg frozenTime tr $ PM.setEveryoneTeamEmails testPid (V.fromList ["oncall@example.com", "alice@example.com"])
    runTestBg frozenTime tr $ PM.deactivateNonOwnerMembers testPid
    emails <- fetchEmails tr
    emails `shouldSatisfy` ("oncall@example.com" `elem`)
    emails `shouldNotSatisfy` ("alice@example.com" `elem`)
