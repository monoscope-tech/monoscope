module Pages.Projects.ManageMembersSpec (spec) where

import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Transact qualified as PGT
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper
import Pkg.DeriveUtils (UUIDId (..))
import Pages.Projects qualified as ManageMembers
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


testPid :: Projects.ProjectId
testPid = Unsafe.fromJust $ UUIDId <$> UUID.fromText "00000000-0000-0000-0000-000000000000"


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
