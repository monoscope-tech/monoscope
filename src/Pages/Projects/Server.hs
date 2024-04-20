module Pages.Projects.Server (server) where

import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.ListProjects qualified as ListProjects
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
    , notificationsUpdateChannelPost = CreateProject.updateNotificationsChannel
    , deleteProjectGet = CreateProject.deleteProjectGetH
    }
