module Pages.Projects.ManageMembersSpec (spec) where

import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact qualified as PGT
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.Projects qualified as ManageMembers
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


postMembers :: ManageMembers.ManageMembers -> IO (V.Vector ProjectMembers.ProjectMemberWithStatusVM)
postMembers (ManageMembers.ManageMembersPost (_, members, _, _)) = pure members
postMembers _ = fail "Expected ManageMembersPost response"


userID :: Projects.UserId
userID = Projects.UserId (Unsafe.fromJust $ UUID.fromText "00000000-0000-0000-0000-000000000001")


spec :: Spec
spec = sequential $ aroundAll withTestResources do
  describe "Members Creation, Update and Consumption" do
    it "creates, updates, gets, deletes, and re-adds a member" \tr -> do
      void $ withPool tr.trPool $ PGT.execute [sql|UPDATE projects.projects SET payment_plan = 'PAID' WHERE id = ?|] (Only testPid)
      void $ withPool tr.trPool $ PGT.execute [sql|DELETE FROM projects.project_members WHERE project_id = ? AND user_id != '00000000-0000-0000-0000-000000000001'|] (Only testPid)
      let saveWith permissions = postMembers . snd =<< testServant tr (ManageMembers.manageMembersPostH testPid Nothing (ManageMembers.ManageMembersForm{emails = ["example@gmail.com"], permissions}))
          hasExample members = "example@gmail.com" `elem` (members <&> (.email))

      saveWith [ProjectMembers.PAdmin] >>= (`shouldSatisfy` hasExample)
      updated <- saveWith [ProjectMembers.PView]
      (find ((== "example@gmail.com") . (.email)) (V.toList updated) <&> (.permission)) `shouldBe` Just ProjectMembers.PView

      (_, ManageMembers.ManageMembersGet (PageCtx _ (_, listed, _, _))) <- testServant tr $ ManageMembers.manageMembersGetH testPid
      listed `shouldSatisfy` hasExample
      length listed `shouldBe` 2

      deleted <- postMembers . snd =<< testServant tr (ManageMembers.manageMembersPostH testPid Nothing (ManageMembers.ManageMembersForm{emails = [], permissions = []}))
      deleted `shouldNotSatisfy` hasExample
      saveWith [ProjectMembers.PAdmin] >>= (`shouldSatisfy` hasExample)

  describe "Team validation" do
    it "rejects duplicate handles and invalid customer input" \tr -> do
      let team =
            ManageMembers.TeamForm
              { teamName = "Hello"
              , teamDescription = ""
              , teamHandle = "hello"
              , notifEmails = V.empty
              , teamMembers = [userID]
              , discordChannels = V.empty
              , slackChannels = V.empty
              , phoneNumbers = V.empty
              , pagerdutyServices = V.empty
              , teamId = Nothing
              }
          post form = snd <$> testServant tr (ManageMembers.manageTeamPostH testPid form Nothing)
          expectError form expected = post form >>= \case
            ManageMembers.ManageTeamsPostError actual -> actual `shouldBe` expected
            _ -> expectationFailure "Expected ManageTeamsPostError response"
          invalidCases :: [(ManageMembers.TeamForm, Text)]
          invalidCases =
            [ (team{ManageMembers.teamName = ""}, "Team name is required")
            , (team{ManageMembers.teamHandle = "invalid handle"}, "Handle must be lowercase, no spaces, and hyphens only")
            , (team{ManageMembers.notifEmails = ["not-an-email"]}, "Invalid email format: not-an-email")
            ]

      expectError team ""
      expectError team "Team handle already exists for this project."
      for_ invalidCases $ uncurry expectError
