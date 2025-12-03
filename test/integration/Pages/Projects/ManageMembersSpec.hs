module Pages.Projects.ManageMembersSpec (spec) where

import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Transact qualified as PGT
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.ProjectMembers ( TeamMemberVM (..), TeamVM (..))
import Models.Projects.Projects qualified as Projects

import Pages.BodyWrapper
import Pkg.DeriveUtils (UUIDId (..))
import Pages.Projects qualified as ManageMembers
import Pages.Projects (TeamForm (..))
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec
import Models.Users.Users (UserId(..))
import Models.Users.Users qualified as Users

testPid :: Projects.ProjectId
testPid = Unsafe.fromJust $ UUIDId <$> UUID.fromText "00000000-0000-0000-0000-000000000000"

userID :: Users.UserId
userID = Users.UserId testPid.unUUIDId

spec :: Spec
spec = aroundAll withTestResources do
  describe "Members Creation, Update and Consumption" do
    it "Create member" \tr -> do
      -- Update project to a paid plan to allow multiple members
      _ <- withPool tr.trPool $ PGT.execute [sql|UPDATE projects.projects SET payment_plan = 'PAID' WHERE id = ?|] (Only testPid)
      pass
      let member =
            ManageMembers.ManageMembersForm
              { emails = ["example@gmail.com"]
              , permissions = [ProjectMembers.PAdmin]
              }
      (_, pg) <-
        testServant tr $ ManageMembers.manageMembersPostH testPid Nothing member
      -- Check if the response contains the newly added member
      case pg of
        ManageMembers.ManageMembersPost p -> do
          "example@gmail.com" `shouldSatisfy` (`elem` (p & V.toList & map (.email)))
        _ -> fail "Expected ManageMembersPost response"

    it "Update member permissions" \tr -> do
      let member =
            ManageMembers.ManageMembersForm
              { emails = ["example@gmail.com"]
              , permissions = [ProjectMembers.PView]
              }
      (_, pg) <-
        testServant tr $ ManageMembers.manageMembersPostH testPid Nothing member

      -- Check if the member's permission is updated
      case pg of
        ManageMembers.ManageMembersPost projMembers -> do
          let memberM = projMembers & V.toList & find (\pm -> pm.email == "example@gmail.com")
          isJust memberM `shouldBe` True
          let mem = memberM & Unsafe.fromJust
          mem.permission `shouldBe` ProjectMembers.PView
          pass
        _ -> fail "Expected ManageMembersPost response"

    it "Get members" \tr -> do
      (_, pg) <-
        testServant tr $ ManageMembers.manageMembersGetH testPid

      -- Check if the response contains the expected members
      case pg of
        ManageMembers.ManageMembersGet (PageCtx _ projMembers) -> do
          let emails = projMembers & V.toList & map (.email)
          "example@gmail.com" `shouldSatisfy` (`elem` emails)
          length projMembers `shouldBe` 1
        _ -> fail "Expected ManageMembersGet response"

    it "Delete member" \tr -> do
      let member =
            ManageMembers.ManageMembersForm
              { emails = []
              , permissions = []
              }
      (_, pg) <-
        testServant tr $ ManageMembers.manageMembersPostH testPid Nothing member

      -- Check if the member is deleted
      case pg of
        ManageMembers.ManageMembersPost projMembers -> do
          let emails = projMembers & V.toList & map (.email)
          "example@gmail.com" `shouldNotSatisfy` (`elem` emails)
        _ -> fail "Expected ManageMembersPost response"

    it "Should add member after deletion" \tr -> do
      let member =
            ManageMembers.ManageMembersForm
              { emails = ["example@gmail.com"]
              , permissions = [ProjectMembers.PAdmin]
              }
      (_, pg) <-
        testServant tr $ ManageMembers.manageMembersPostH testPid Nothing member
      -- Check if the response contains the newly added member
      case pg of
        ManageMembers.ManageMembersPost p -> do
          "example@gmail.com" `shouldSatisfy` (`elem` (p & V.toList & map (.email)))
        _ -> fail "Expected ManageMembersPost response"

  describe "Teams Creation, Update and Consumption" do
    let team = ManageMembers.TeamForm {
          teamName = "Hello", 
          teamDescription = "", 
          teamHandle = "hello", 
          notifEmails = [], 
          teamMembers = [], 
          discordChannels = [],
          slackChannels = [],
          teamId = Nothing
          }
  
    it "Should create team" \tr -> do
      (_, pg) <-
          testServant tr $ ManageMembers.manageTeamPostH testPid team Nothing
      case pg of 
          ManageMembers.ManageTeamsGet' (pid, members, slackChannels, discordChannels, teams) -> do
              length teams `shouldBe` 1
          _ -> fail "Expected ManageTeamsGet' response"
  
    it "Should not create team with same handle" \tr -> do 
      (_, pg) <-
          testServant tr $ ManageMembers.manageTeamPostH testPid team Nothing
      case pg of 
          ManageMembers.ManageTeamsPostError message -> do
              message `shouldBe` "Team handle already exists for this project."
          _ -> fail "Expected ManageTeamsPostError response"
  
    it "Should list and update teams" \tr -> do 
      (_, pg) <-
          testServant tr $ ManageMembers.manageTeamPostH testPid team Nothing
      case pg of 
          ManageMembers.ManageTeamsGet' (pid, members, slackChannels, discordChannels, teams') -> do
              V.length teams' `shouldBe` 1
              let team' = V.head teams'
              let updatedTeam' = team { teamName = "Updated Team", teamDescription = "Updated description", teamId = Just team'.id }
              traceShowM updatedTeam'
              (_, pg') <-
                  testServant tr $ ManageMembers.manageTeamPostH testPid updatedTeam' (Just "hello")
              case pg' of
                  ManageMembers.ManageTeamsGet' (pid', members', slackChannels', discordChannels', teams) -> do
                      let updated = find (\t -> t.handle == "hello") teams
                      isJust updated `shouldBe` True
                      let updatedTeam = Unsafe.fromJust updated
                      updatedTeam.name `shouldBe` "Updated Team"
                      updatedTeam.description `shouldBe` "Updated description"
                  _ -> fail "Expected ManageTeamsGet' response"

          _ -> fail "Expected ManageTeamsGet' response"
  
    it "Should delete a team" \tr -> do
      (_, pg) <-
          testServant tr $ ManageMembers.manageTeamDeleteH testPid "hello" Nothing
      case pg of
          ManageMembers.ManageTeamsGet' (pid, members, slackChannels, discordChannels, teams) -> do
              length teams `shouldBe` 0
              let deleted = find (\t -> t.handle == "hello") teams
              isNothing deleted `shouldBe` True
          _ -> fail "Expected ManageTeamsGet' response"
  
    it "Should not create team with invalid handle" \tr -> do
      let invalidTeam = team { teamHandle = "invalid handle with spaces" }
      (_, pg) <-
          testServant tr $ ManageMembers.manageTeamPostH testPid invalidTeam Nothing
      case pg of
          ManageMembers.ManageTeamsPostError message -> do
              message `shouldBe` "Handle must be lowercase, no spaces, and hyphens only"
          _ -> fail "Expected ManageTeamsPostError response"
  
    it "Should add members to a team" \tr -> do
      let teamWithMembers = team { teamMembers = [userID] }
      (_, pg) <-
          testServant tr $ ManageMembers.manageTeamPostH testPid teamWithMembers Nothing
      case pg of
          ManageMembers.ManageTeamsGet' (pid, members, slackChannels, discordChannels, teams) -> do
              let createdTeam = find (\t -> t.handle == "hello") teams
              isJust createdTeam `shouldBe` True
              let created = Unsafe.fromJust createdTeam
              length created.members `shouldBe` 1 
              let fm = V.head (created.members)
              fm.memberEmail `shouldBe` "hello@monoscope.tech"
          _ -> fail "Expected ManageTeamsGet' response"
