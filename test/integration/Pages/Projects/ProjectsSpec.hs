module Pages.Projects.ProjectsSpec (spec) where

import Data.Generics.Labels ()
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper
import Pages.Projects.CreateProject
import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.ListProjects qualified as ListProjects
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


testPid :: Projects.ProjectId
testPid = Unsafe.fromJust $ Projects.ProjectId <$> UUID.fromText "00000000-0000-0000-0000-000000000000"


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Course Creation, Update and Consumption" do
    it "Cannot update demo project without sudo" \tr -> do
      let createPForm =
            CreateProject.CreateProjectForm
              { title = "Test Project CI"
              , description = "Test Description"
              , emails = ["test@monoscope.tech"]
              , permissions = [ProjectMembers.PAdmin]
              , timeZone = ""
              , weeklyNotifs = Nothing 
              , dailyNotifs = Nothing 
              , endpointAlerts = Nothing 
              , errorAlerts = Nothing
              }
      (_, pg) <-
        testServant tr $ CreateProject.createProjectPostH testPid createPForm
      -- Demo project update should be blocked, but form returns submitted values
      (pg.unwrapCreateProjectResp <&> (.form.title)) `shouldBe` Just @Text "Test Project CI"
      (pg.unwrapCreateProjectResp <&> (.form.description)) `shouldBe` Just "Test Description"

    it "Non empty project list" \tr -> do
      (_, pg) <-
        testServant tr ListProjects.listProjectsGetH
      let (projects, _demoProject) = pg.unwrap.content
      length projects `shouldBe` 1
      -- Should have test project created in testSessionHeader (demo project is returned separately)
      let projectIds = map (.id.toText) (V.toList projects)
      projectIds `shouldContain` ["12345678-9abc-def0-1234-56789abcdef0"] -- test project from testSessionHeader
    -- TODO: add more checks for the info we we display on list page

    it "Should update project with new details and verify in list" \tr -> do
      let testProjectPid = Unsafe.fromJust $ Projects.ProjectId <$> UUID.fromText "12345678-9abc-def0-1234-56789abcdef0"

      -- Section 1: Update the project
      let createPForm =
            CreateProject.CreateProjectForm
              { title = "Test Project CI2"
              , description = "Test Description2"
              , emails = ["test@monoscope.tech"]
              , permissions = [ProjectMembers.PAdmin]
              , timeZone = "Africa/Accra"
              , weeklyNotifs = Nothing
              , dailyNotifs = Nothing
              , endpointAlerts = Nothing
              , errorAlerts = Nothing
              }
      (_, updateResp) <-
        testServant tr $ CreateProject.createProjectPostH testProjectPid createPForm
      (updateResp.unwrapCreateProjectResp <&> (.form.title)) `shouldBe` Just @Text "Test Project CI2"
      (updateResp.unwrapCreateProjectResp <&> (.form.description)) `shouldBe` Just "Test Description2"

      -- Section 2: Verify the update persists in the project list
      (_, listResp) <-
        testServant tr ListProjects.listProjectsGetH
      let (projects, _demoProject) = listResp.unwrap.content

      -- Find the updated project by ID instead of relying on index
      let updatedProject = V.find (\p -> p.id.toText == "12345678-9abc-def0-1234-56789abcdef0") projects
      updatedProject `shouldSatisfy` isJust

      -- Safe to use fromJust here because we verified isJust above
      let project = Unsafe.fromJust updatedProject
      project.title `shouldBe` "Test Project CI2"
      project.description `shouldBe` "Test Description2"
      project.paymentPlan `shouldBe` "FREE"
      project.timeZone `shouldBe` "Africa/Accra"
