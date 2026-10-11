module Pages.Projects.ProjectsSpec (spec) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Control.Lens ((.~))
import Data.Default (def)
import Data.Generics.Labels ()
import Data.Generics.Product.Fields (field)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.Error.Static (catchError)
import Lucid qualified
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Pages.CommandPalette qualified as CommandPalette
import Pages.BodyWrapper
import Pages.Projects
import Pages.Projects qualified as CreateProject
import Pages.Projects qualified as ListProjects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant (ServerError (..), getResponse)
import Test.Hspec
import UnliftIO.Exception (bracket_)


spec :: Spec
spec = around withTestResources do
  describe "Project settings permissions" do
    it "projectSettingsViewer_cannotChangeDetailsNotificationsOrDeleteProject" \tr -> do
      let uid = (getResponse tr.trSessAndHeader).user.id
          setPermission permission = void $ withResource tr.trPool \conn -> PG.execute conn [sql|UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?|] (permission :: Text, testPid, uid)
      bracket_ (setPermission "view") (setPermission "admin") do
        statuses <- forM
          [ void $ CreateProject.createProjectPostH testPid def{title = "Forbidden", timeZone = "UTC"}
          , void $ CreateProject.updateNotificationsChannel testPid (NotifListForm [] [] [] [] Nothing)
          , void $ CreateProject.deleteProjectGetH testPid
          ]
          \mutation -> runAuthHandler tr $ catchError @ServerError (mutation $> 200) (\_ err -> pure err.errHTTPCode)
        statuses `shouldBe` [403, 403, 403]
        project <- runQueryEffect tr (Projects.projectById testPid) >>= maybe (fail "Project was deleted") pure
        project.title `shouldNotBe` "Forbidden"

    it "projectEditor_cannotDeleteProject" \tr -> do
      let uid = (getResponse tr.trSessAndHeader).user.id
          setPermission permission = void $ withResource tr.trPool \conn -> PG.execute conn [sql|UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?|] (permission :: Text, testPid, uid)
      bracket_ (setPermission "edit") (setPermission "admin") do
        status <- runAuthHandler tr $ catchError @ServerError (CreateProject.deleteProjectGetH testPid $> 200) (\_ err -> pure err.errHTTPCode)
        status `shouldBe` 403
        runQueryEffect tr (Projects.projectById testPid) >>= (`shouldSatisfy` isJust)

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

    -- Neither handler checked membership: any signed-in user could delete any project by its
    -- id, or read its issue/monitor/dashboard titles and write into its palette history.
    it "deleteProjectGetH_nonMember_cannotDelete" \tr -> do
      victim <- createTestProject tr "victim"
      assertNonMemberDenied tr victim $ CreateProject.deleteProjectGetH victim
      runQueryEffect tr (Projects.activeProjectById victim) >>= (`shouldSatisfy` isJust)

    -- Deletion is an admin action (like team management); membership alone is not enough.
    it "deleteProjectGetH_viewer_cannotDelete" \tr -> do
      -- The session's cached non-demo project (testPid is the demo, which never deletes).
      let pid = UUIDId $ UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef0
          setRole :: Text -> IO ()
          setRole r = withResource tr.trPool \conn -> void $ PG.execute conn [sql|UPDATE projects.project_members SET permission = ?::projects.project_permissions WHERE project_id = ?|] (r, pid)
      setRole "view"
      -- An explicit membership role binds sudo users too; sudo only stands in for a missing membership.
      res <- forM [False, True] \sudo -> try @ErrorCall $ runAuthHandler tr{trSessAndHeader = tr.trSessAndHeader <&> field @"user" . field @"isSudo" .~ sudo} (CreateProject.deleteProjectGetH pid) >>= evaluate
      setRole "admin"
      map (either (\(ErrorCall m) -> toText m) (const "")) res `shouldSatisfy` all (T.isInfixOf "403")
      runQueryEffect tr (Projects.activeProjectById pid) >>= (`shouldSatisfy` isJust)

    it "commandPalette_nonMember_cannotReadOrWrite" \tr -> do
      victim <- createTestProject tr "victim"
      assertNonMemberDenied tr victim $ CommandPalette.commandPaletteItemsH victim
      assertNonMemberDenied tr victim $ CommandPalette.commandPaletteRecentPostH victim (CommandPalette.RecentForm "planted" "/evil" "page")
      (_, items) <- testServant tr $ CommandPalette.commandPaletteItemsH victim
      LT.toStrict (Lucid.renderText items) `shouldSatisfy` (not . T.isInfixOf "/evil")

    it "Non empty project list" \tr -> do
      (_, pg) <-
        testServant tr ListProjects.listProjectsGetH
      let (projects, _demoProject, _showDemoProject) = pg.unwrap.content
      -- User is member of both demo project and test project (added in testSessionHeader)
      length projects `shouldBe` 2
      let projectIds = map (.id.toText) (V.toList projects)
      projectIds `shouldContain` [testPid.toText] -- test project from testSessionHeader
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml pg
      html `shouldSatisfy` (not . T.isInfixOf "id=\"mobile-nav-toggle\"")
      html `shouldSatisfy` (not . T.isInfixOf "for=\"mobile-nav-toggle\"")
    it "Should update project with new details and verify in list" \tr -> do
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
        testServant tr $ CreateProject.createProjectPostH testPid createPForm
      (updateResp.unwrapCreateProjectResp <&> (.form.title)) `shouldBe` Just @Text "Test Project CI2"
      (updateResp.unwrapCreateProjectResp <&> (.form.description)) `shouldBe` Just "Test Description2"

      -- Section 2: Verify the update persists in the project list
      (_, listResp) <-
        testServant tr ListProjects.listProjectsGetH
      let (projects, _demoProject, _showDemoProject) = listResp.unwrap.content

      -- Find the updated project by ID instead of relying on index
      let updatedProject = V.find (\p -> p.id.toText == testPid.toText) projects
      updatedProject `shouldSatisfy` isJust

      -- Safe to use fromJust here because we verified isJust above
      let project = Unsafe.fromJust updatedProject
      project.title `shouldBe` "Test Project CI2"
      project.description `shouldBe` "Test Description2"
      project.paymentPlan `shouldBe` "GraduatedPricing"
      project.timeZone `shouldBe` "Africa/Accra"
