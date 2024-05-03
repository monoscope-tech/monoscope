module Pages.Projects.Server (server) where

import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.ListProjects qualified as ListProjects
import Pages.Projects.ManageMembers qualified as ManageMembers
import Pages.Projects.Integrations qualified as Integrations
import Pages.Projects.Routes (Routes, Routes' (..))
import Servant qualified
import System.Types (ATAuthCtx)


server :: Servant.ServerT Routes ATAuthCtx
server =
  Routes'
    { listGet = ListProjects.listProjectsGetH
    , createGet = CreateProject.createProjectGetH
    , createPost = CreateProject.createProjectPostH
    , settingsGet = CreateProject.projectSettingsGetH
    , deleteGet = CreateProject.deleteProjectGetH
    , notificationsUpdateChannelPost = Integrations.updateNotificationsChannel
    , integrationSettingsGet = Integrations.integrationSettingGetH
    , deleteProjectGet = CreateProject.deleteProjectGetH
    , membersManageGet = ManageMembers.manageMembersGetH
    , membersManagePost = ManageMembers.manageMembersPostH
    , manageSubscriptionGet = ManageMembers.manageSubGetH
    }
