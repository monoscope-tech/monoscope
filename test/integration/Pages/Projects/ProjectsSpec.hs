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
    it "Cannot update demo project without sudo" \TestResources{..} -> do
      let createPForm =
            CreateProject.CreateProjectForm
              { title = "Test Project CI"
              , description = "Test Description"
              , emails = ["test@apitoolkit.io"]
              , permissions = [ProjectMembers.PAdmin]
              , timeZone = ""
              }
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ CreateProject.createProjectPostH testPid createPForm
      -- Demo project update should be blocked, but form returns submitted values
      (pg.unwrapCreateProjectResp <&> (.form.title)) `shouldBe` Just @Text "Test Project CI"
      (pg.unwrapCreateProjectResp <&> (.form.description)) `shouldBe` Just "Test Description"

    it "Non empty project list" \TestResources{..} -> do
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger ListProjects.listProjectsGetH
      let (projects, _demoProject) = pg.unwrap.content
      length projects `shouldBe` 1
      -- Should have the test project created in testSessionHeader (user doesn't have access to demo project)
      let projectIds = map (.id.toText) (V.toList projects)
      projectIds `shouldContain` ["12345678-9abc-def0-1234-56789abcdef0"] -- test project from testSessionHeader
    -- TODO: add more checks for the info we we display on list page

    it "Should update project with new details" \TestResources{..} -> do
      let createPForm =
            CreateProject.CreateProjectForm
              { title = "Test Project CI2"
              , description = "Test Description2"
              , emails = ["test@apitoolkit.io"]
              , permissions = [ProjectMembers.PAdmin]
              , timeZone = "Africa/Accra"
              }
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ CreateProject.createProjectPostH testPid createPForm
      (pg.unwrapCreateProjectResp <&> (.form.title)) `shouldBe` Just @Text "Test Project CI2"
      (pg.unwrapCreateProjectResp <&> (.form.description)) `shouldBe` Just "Test Description2"

    -- FIXME: marked as pending with xit. Test is faily and should be investigated
    xit "Project in list should have new details" \TestResources{..} -> do
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger ListProjects.listProjectsGetH
      let (projects, _demoProject) = pg.unwrap.content
      length projects `shouldBe` 2
      (projects V.! 0).id.toText `shouldBe` "00000000-0000-0000-0000-000000000001"
      (projects V.! 0).title `shouldBe` "Test Project CI2"
      (projects V.! 0).description `shouldBe` "Test Description2"
      (projects V.! 0).paymentPlan `shouldBe` "Free"
      (projects V.! 0).timeZone `shouldBe` "Africa/Accra"
