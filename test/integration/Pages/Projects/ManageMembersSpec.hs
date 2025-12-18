module Pages.Projects.ManageMembersSpec (spec) where

import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact qualified as PGT
import Models.Projects.ProjectMembers (TeamMemberVM (..), TeamVM (..))
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects

import Models.Users.Users (UserId (..))
import Models.Users.Users qualified as Users
import Pages.BodyWrapper
import Pages.Projects (TBulkActionForm (..), TeamForm (..))
import Pages.Projects qualified as ManageMembers
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


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
    let team =
          ManageMembers.TeamForm
            { teamName = "Hello"
            , teamDescription = ""
            , teamHandle = "hello"
            , notifEmails = []
            , teamMembers = []
            , discordChannels = []
            , slackChannels = []
            , teamId = Nothing
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
        testServant tr $ ManageMembers.manageTeamsGetH testPid (Just "")
      case pg of
        ManageMembers.ManageTeamsGet' (pid, members, slackChannels, discordChannels, teams') -> do
          V.length teams' `shouldBe` 1
          let team' = V.head teams'
          let updatedTeam' = team{teamName = "Updated Team", teamDescription = "Updated description", teamId = Just team'.id}
          (_, pg') <-
            testServant tr $ ManageMembers.manageTeamPostH testPid updatedTeam' Nothing
          case pg' of
            ManageMembers.ManageTeamsGet' (pid', members', slackChannels', discordChannels', teams) -> do
              let updated = find (\t -> t.handle == "hello") teams
              isJust updated `shouldBe` True
              let updatedTeam = Unsafe.fromJust updated
              updatedTeam.name `shouldBe` "Updated Team"
              updatedTeam.description `shouldBe` "Updated description"
            _ -> fail "Expected ManageTeamsGet' response"
        _ -> fail "Expected ManageTeamsGet' response"

    it "Should not create team with invalid handle" \tr -> do
      let invalidTeam = team{teamHandle = "invalid handle with spaces"}
      (_, pg) <-
        testServant tr $ ManageMembers.manageTeamPostH testPid invalidTeam Nothing
      case pg of
        ManageMembers.ManageTeamsPostError message -> do
          message `shouldBe` "Handle must be lowercase, no spaces, and hyphens only"
        _ -> fail "Expected ManageTeamsPostError response"

    it "Should add members to a team" \tr -> do
      (_, pg) <-
        testServant tr $ ManageMembers.manageTeamsGetH testPid (Just "")
      case pg of
        ManageMembers.ManageTeamsGet' (pid, members, slackChannels, discordChannels, teams) -> do
          let createdTeam = find (\t -> t.handle == "hello") teams
          isJust createdTeam `shouldBe` True
          let created = Unsafe.fromJust createdTeam
          let teamWithMembers = team{teamMembers = [userID], teamId = Just created.id}
          (_, pg') <-
            testServant tr $ ManageMembers.manageTeamPostH testPid teamWithMembers Nothing
          case pg' of
            ManageMembers.ManageTeamsGet' (pid', members', slackChannels', discordChannels', teams') -> do
              let updatedTeamM = find (\t -> t.handle == "hello") teams'
              isJust updatedTeamM `shouldBe` True
              let updatedTeam = Unsafe.fromJust updatedTeamM
              length updatedTeam.members `shouldBe` 1
              let fm = V.head updatedTeam.members
              fm.memberEmail `shouldBe` "hello@monoscope.tech"
            _ -> fail "Expected ManageTeamsGet' response"
        _ -> fail "Expected ManageTeamsGet' response"

    it "Should not create a team with an empty name" \tr -> do
      let invalidTeam = team{teamName = ""}
      (_, pg) <- testServant tr $ ManageMembers.manageTeamPostH testPid invalidTeam Nothing
      case pg of
        ManageMembers.ManageTeamsPostError message -> do
          message `shouldBe` "Team name is required"
        _ -> fail "Expected ManageTeamsPostError response"

    it "Should not create a team with an invalid handle" \tr -> do
      let invalidTeam = team{teamHandle = "Invalid Handle!"}
      (_, pg) <- testServant tr $ ManageMembers.manageTeamPostH testPid invalidTeam Nothing
      case pg of
        ManageMembers.ManageTeamsPostError message -> do
          message `shouldBe` "Handle must be lowercase, no spaces, and hyphens only"
        _ -> fail "Expected ManageTeamsPostError response"

    it "Should update team notifications" \tr -> do
      (_, pg) <- testServant tr $ ManageMembers.manageTeamsGetH testPid (Just "")
      case pg of
        ManageMembers.ManageTeamsGet' (pid, members, slackChannels, discordChannels, teams) -> do
          let createdTeam = find (\t -> t.handle == "hello") teams
          isJust createdTeam `shouldBe` True
          let created = Unsafe.fromJust createdTeam
          let updatedTeam =
                team
                  { teamId = Just created.id
                  , notifEmails = ["team@example.com"]
                  , slackChannels = ["slack-channel-id"]
                  , discordChannels = ["discord-channel-id"]
                  }
          (_, pg') <- testServant tr $ ManageMembers.manageTeamPostH testPid updatedTeam Nothing
          case pg' of
            ManageMembers.ManageTeamsGet' (_, _, _, _, updatedTeams) -> do
              let updated = find (\t -> t.handle == "hello") updatedTeams
              isJust updated `shouldBe` True
              let updatedTeam' = Unsafe.fromJust updated
              updatedTeam'.notify_emails `shouldBe` ["team@example.com"]
              updatedTeam'.slack_channels `shouldBe` ["slack-channel-id"]
              updatedTeam'.discord_channels `shouldBe` ["discord-channel-id"]
            _ -> fail "Expected ManageTeamsGet' response"
        _ -> fail "Expected ManageTeamsGet' response"

    it "Should delete a team" \tr -> do
      (_, pg) <-
        testServant tr $ ManageMembers.manageTeamsGetH testPid (Just "")
      case pg of
        ManageMembers.ManageTeamsGet' (pid, members, slackChannels, discordChannels, teams') -> do
          V.length teams' `shouldBe` 1
          let team' = V.head teams'
          let teamForm =
                TBulkActionForm{teamId = [team'.id]}
          (_, pg') <-
            testServant tr $ ManageMembers.manageTeamBulkActionH testPid "delete" teamForm (Just "hello")
          case pg' of
            ManageMembers.ManageTeamsDelete -> do
              (_, pg'') <- testServant tr $ ManageMembers.manageTeamsGetH testPid (Just "")
              case pg'' of
                ManageMembers.ManageTeamsGet' (_, _, _, _, teams'') -> do
                  V.length teams'' `shouldBe` 0
                _ -> fail "Expected ManageTeamsGet' response"
            _ -> fail "Expected ManageTeamsDelete response"
        _ -> fail "Expected ManageTeamsGet' response"

    it "Should handle bulk delete of teams" \tr -> do
      -- Create multiple teams
      let team1 = team{teamName = "Team 1", teamHandle = "team-1"}
      let team2 = team{teamName = "Team 2", teamHandle = "team-2"}
      (_, pg1) <- testServant tr $ ManageMembers.manageTeamPostH testPid team1 Nothing
      (_, pg2) <- testServant tr $ ManageMembers.manageTeamPostH testPid team2 Nothing
      case (pg1, pg2) of
        (ManageMembers.ManageTeamsGet' (_, _, _, _, teams1), ManageMembers.ManageTeamsGet' (_, _, _, _, teams2)) -> do
          let createdTeam1' = find (\t -> t.handle == "team-1") teams1
          let createdTeam2' = find (\t -> t.handle == "team-2") teams2
          isJust createdTeam1' `shouldBe` True
          isJust createdTeam2' `shouldBe` True
          let createdTeam1 = Unsafe.fromJust createdTeam1'
          let createdTeam2 = Unsafe.fromJust createdTeam2'

          let teamIds = [createdTeam1.id, createdTeam2.id]
          let bulkActionForm = ManageMembers.TBulkActionForm{teamId = teamIds}
          (_, pg') <- testServant tr $ ManageMembers.manageTeamBulkActionH testPid "delete" bulkActionForm Nothing
          case pg' of
            ManageMembers.ManageTeamsDelete -> do
              (_, pg'') <- testServant tr $ ManageMembers.manageTeamsGetH testPid (Just "")
              case pg'' of
                ManageMembers.ManageTeamsGet' (_, _, _, _, teams) -> do
                  V.length teams `shouldBe` 0
                _ -> fail "Expected ManageTeamsGet' response"
            _ -> fail "Expected ManageTeamsDelete response"
        _ -> fail "Expected ManageTeamsGet' response"
