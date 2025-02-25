module Pages.Projects.ProjectsSpec (spec) where

import Control.Lens (Traversal', preview, (^?))
import Data.UUID qualified as UUID
import Control.Lens (preview, (^?), Traversal')
import Data.UUID qualified as UUID
import Data.Generics.Labels ()
import Data.Generics.Product (HasField (field))
import Data.Generics.Sum (AsConstructor (_Ctor), _As)
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
    it "Create Project" \TestResources{..} -> do
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
      (pg.unwrapCreateProjectResp <&> (.form.title)) `shouldBe` (Just @Text "Test Project CI")
      (pg.unwrapCreateProjectResp <&> (.form.description)) `shouldBe` (Just "Test Description")

    it "Non empty project list" \TestResources{..} -> do
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger ListProjects.listProjectsGetH
      length pg.unwrap.content `shouldBe` 2
      -- default demo project created in migrations
      (pg.unwrap.content V.! 1).id.toText `shouldBe` "00000000-0000-0000-0000-000000000000"
      -- the project we actually created
      (pg.unwrap.content V.! 0).id.toText `shouldBe` "00000000-0000-0000-0000-000000000001"
      (pg.unwrap.content V.! 0).title `shouldBe` "Test Project CI"
      (pg.unwrap.content V.! 0).description `shouldBe` "Test Description"
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
      (pg.unwrapCreateProjectResp <&> (.form.title)) `shouldBe` (Just @Text "Test Project CI2")
      (pg.unwrapCreateProjectResp <&> (.form.description)) `shouldBe` (Just "Test Description2")

    -- FIXME: marked as pending with xit. Test is faily and should be investigated
    xit "Project in list should have new details" \TestResources{..} -> do
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger ListProjects.listProjectsGetH
      length pg.unwrap.content `shouldBe` 2
      (pg.unwrap.content V.! 0).id.toText `shouldBe` "00000000-0000-0000-0000-000000000001"
      (pg.unwrap.content V.! 0).title `shouldBe` "Test Project CI2"
      (pg.unwrap.content V.! 0).description `shouldBe` "Test Description2"
      (pg.unwrap.content V.! 0).paymentPlan `shouldBe` "Free"
      (pg.unwrap.content V.! 0).timeZone `shouldBe` "Africa/Accra"
