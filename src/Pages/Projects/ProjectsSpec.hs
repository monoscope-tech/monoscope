module Pages.Projects.ProjectsSpec (spec) where

import Data.Vector qualified as V
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper
import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.ListProjects qualified as ListProjects
import Pkg.TestUtils
import Servant qualified
import Servant.Server qualified as ServantS

import Relude
import System.Types (atAuthToBase, effToServantHandlerTest)
import Test.Hspec


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
              , isUpdate = False
              , projectId = ""
              , paymentPlan = "Free"
              , timeZone = ""
              , orderId = Nothing
              }
      pg <-
        CreateProject.createProjectPostH createPForm
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      pg.form.title `shouldBe` "Test Project CI"
      pg.form.description `shouldBe` "Test Description"

    it "Non empty project list" \TestResources{..} -> do
      pg <-
        ListProjects.listProjectsGetH
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
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
              , isUpdate = True
              , projectId = "00000000-0000-0000-0000-000000000001"
              , paymentPlan = "Free"
              , timeZone = "Africa/Accra"
              , orderId = Nothing
              }
      pg <-
        CreateProject.createProjectPostH createPForm
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      pg.form.title `shouldBe` "Test Project CI2"
      pg.form.description `shouldBe` "Test Description2"

    -- FIXME: marked as pending with xit. Test is faily and should be investigated
    xit "Project in list should have new details" \TestResources{..} -> do
      pg <-
        ListProjects.listProjectsGetH
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      length pg.unwrap.content `shouldBe` 2
      (pg.unwrap.content V.! 0).id.toText `shouldBe` "00000000-0000-0000-0000-000000000001"
      (pg.unwrap.content V.! 0).title `shouldBe` "Test Project CI2"
      (pg.unwrap.content V.! 0).description `shouldBe` "Test Description2"
      (pg.unwrap.content V.! 0).paymentPlan `shouldBe` "Free"
      (pg.unwrap.content V.! 0).timeZone `shouldBe` "Africa/Accra"
