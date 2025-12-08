module Pages.DashboardsSpec (spec) where

import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Models.Projects.Dashboards (DashboardVM (..))
import Models.Projects.ProjectMembers (TeamVM (..))
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.Dashboards (DashboardForm (title))
import Pages.Dashboards qualified as Dashboards
import Pages.Projects (TeamForm (..))
import Pages.Projects qualified as ManageMembers
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


testPid :: Projects.ProjectId
testPid = UUIDId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Dashboards Tests" do
    let dashboard =
          Dashboards.DashboardForm
            { title = "Test Dashboard"
            , file = "overview.yaml"
            , teams = []
            }

    it "Should create a dashboard" \tr -> do
      (_, pg) <- testServant tr do
        let dashboard1 = dashboard{title = "Dashboard 1"} :: Dashboards.DashboardForm
        let dashboard2 = dashboard{title = "Dashboard 2"} :: Dashboards.DashboardForm
        _ <- Dashboards.dashboardsPostH testPid dashboard1
        _ <- Dashboards.dashboardsPostH testPid dashboard2
        Dashboards.dashboardsPostH testPid dashboard
      case pg of
        Dashboards.DashboardNoContent -> 1 `shouldBe` 1
        _ -> fail "Expected DashboardGet' response"

    it "Should not create a dashboard with an empty title" \tr -> do
      let invalidDashboard = (dashboard :: Dashboards.DashboardForm){title = ""}
      (_, pg) <- testServant tr $ Dashboards.dashboardsPostH testPid invalidDashboard
      case pg of
        Dashboards.DashboardPostError message -> do
          message `shouldBe` "Dashboard title is required"
        _ -> fail "Expected DashboardPostError response"

    it "Should update a dashboard" \tr -> do
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing
      case pg of
        Dashboards.DashboardsGet (PageCtx _ Dashboards.DashboardsGetD{dashboards}) -> do
          let createdDashboard = Unsafe.fromJust $ V.find (\x -> x.title == "Test Dashboard") dashboards
          let fm = Dashboards.DashboardRenameForm{title = "Updated Dashboard"}
          _ <- testServant tr $ Dashboards.dashboardRenamePatchH testPid createdDashboard.id fm
          (_, pg') <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing
          case pg' of
            Dashboards.DashboardsGet (PageCtx _ d) -> do
              let updated = V.find (\x -> x.title == "Updated Dashboard") d.dashboards
              isJust updated `shouldBe` True
            _ -> fail "Expected DashboardGet' response"
        _ -> fail "Expected DashboardGet' response"

    it "Should delete a dashboard" \tr -> do
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing
      case pg of
        Dashboards.DashboardsGet (PageCtx _ d) -> do
          let createdDashboard = V.find (\db -> db.title == "Dashboard 1") d.dashboards & Unsafe.fromJust
          (_, pg') <- testServant tr $ Dashboards.dashboardDeleteH testPid createdDashboard.id
          case pg' of
            Dashboards.DashboardNoContent -> do
              (_, pg'') <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing
              case pg'' of
                Dashboards.DashboardsGet (PageCtx _ dd) -> do
                  let v = V.find (\db -> db.title == "Dashboard 1") dd.dashboards
                  isNothing v `shouldBe` True
                _ -> fail "Expected DashboardGet' response"
            _ -> fail "Expected DashboardDelete response"
        _ -> fail "Expected DashboardGet' response"

    it "Should duplicate dashboard" \tr -> do
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing
      case pg of
        Dashboards.DashboardsGet (PageCtx _ Dashboards.DashboardsGetD{dashboards}) -> do
          let createdDashboard = Unsafe.fromJust $ V.find (\db -> db.title == "Updated Dashboard") dashboards
          _ <- testServant tr $ Dashboards.dashboardDuplicatePostH testPid createdDashboard.id
          (_, pg2) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing
          case pg2 of
            Dashboards.DashboardsGet (PageCtx _ d) -> do
              let duplicatedDashboard = V.find (\db -> db.title == "Updated Dashboard (Copy)") d.dashboards
              isJust duplicatedDashboard `shouldBe` True
            _ -> fail "Expected DashboardGet' response"
        _ -> fail "Expected DashboardGet' response"

    it "Should handle bulk add teams to dashboards" \tr -> do
      let team = ManageMembers.TeamForm{teamName = "Hello", teamDescription = "", teamHandle = "hello", notifEmails = [], teamMembers = [], discordChannels = [], slackChannels = [], teamId = Nothing}
      _ <- testServant tr $ ManageMembers.manageTeamPostH testPid team Nothing
      _ <- testServant tr $ ManageMembers.manageTeamPostH testPid team{teamHandle = "hi"} Nothing
      _ <- testServant tr $ ManageMembers.manageTeamPostH testPid team{teamHandle = "broo"} Nothing
      (_, tm) <- testServant tr $ ManageMembers.manageTeamsGetH testPid (Just "")
      case tm of
        ManageMembers.ManageTeamsGet' (pid, members, slackChannels, discordChannels, teams') -> do
          let teamIds = V.toList $ V.map (\t -> t.id) $ V.filter (\t -> t.handle /= "broo") teams'
          let brooId = V.find (\t -> t.handle == "broo") teams' & Unsafe.fromJust & \t -> t.id
          (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing
          case pg of
            Dashboards.DashboardsGet (PageCtx _ d) -> do
              let dIds = V.toList $ V.map (\db -> db.id) $ V.filter (\db -> db.title /= "Updated Dashboard (Copy)") d.dashboards
              let bulkActionForm = Dashboards.DashboardBulkActionForm{dashboardId = dIds, teamHandles = teamIds}
              _ <- testServant tr $ Dashboards.dashboardBulkActionPostH testPid "add_teams" bulkActionForm
              (_, pg') <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing
              case pg' of
                Dashboards.DashboardsGet (PageCtx _ dd) -> do
                  length dd.dashboards `shouldBe` 3
                  forM_ dd.dashboards $ \db -> do
                    if db.title == "Updated Dashboard (Copy)"
                      then length db.teams `shouldBe` 0
                      else length db.teams `shouldBe` 2
                    length (filter (== brooId) (V.toList db.teams)) `shouldBe` 0
                    unless (db.title == "Updated Dashboard (Copy)") $ do
                      True `shouldBe` all (`elem` V.toList db.teams) teamIds
                _ -> fail "Expected DashboardDelete response"
            _ -> fail "Expected DashboardGet' response"
        _ -> fail "Expected ManageTeamsGet' response"

    it "Should handle bulk delete of dashboards" \tr -> do
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing
      case pg of
        Dashboards.DashboardsGet (PageCtx _ d) -> do
          let dIds = V.toList $ V.map (\db -> db.id) d.dashboards
          let bulkActionForm = Dashboards.DashboardBulkActionForm{dashboardId = dIds, teamHandles = []}
          _ <- testServant tr $ Dashboards.dashboardBulkActionPostH testPid "delete" bulkActionForm
          (_, pg') <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing
          case pg' of
            Dashboards.DashboardsGet (PageCtx _ dd) -> do
              length dd.dashboards `shouldBe` 0
            _ -> fail "Expected DashboardDelete response"
        _ -> fail "Expected DashboardGet' response"

-- it "Should star a dashboard" \tr -> do
--   (_, pg) <- testServant tr $ Dashboards.dashboardPostH testPid dashboard Nothing
--   case pg of
--     Dashboards.DashboardGet' (_, dashboards) -> do
--       let createdDashboard = head dashboards
--       (_, pg') <- testServant tr $ Dashboards.dashboardStarH testPid createdDashboard.id
--       case pg' of
--         Dashboards.DashboardStar -> do
--           (_, pg'') <- testServant tr $ Dashboards.dashboardGetH testPid
--           case pg'' of
--             Dashboards.DashboardGet' (_, starredDashboards) -> do
--               let starred = head starredDashboards
--               starred.starred `shouldBe` True
--             _ -> fail "Expected DashboardGet' response"
--         _ -> fail "Expected DashboardStar response"
--     _ -> fail "Expected DashboardGet' response"
