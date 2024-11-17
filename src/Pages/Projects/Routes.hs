module Pages.Projects.Routes (Routes, Routes' (..)) where

import Lucid (Html)
import Models.Projects.Projects qualified as Projects
import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.Integrations qualified as Integrations
import Pages.Projects.ListProjects qualified as ListProjects
import Pages.Projects.ManageMembers qualified as ManageMembers
import Servant (Capture, FormUrlEncoded, GenericMode (type (:-)), Get, NamedRoutes, Post, ReqBody, type (:>))
import Servant.HTML.Lucid (HTML)
import System.Types (RespHeaders)
import Relude


type role Routes' nominal


type Routes = NamedRoutes Routes'

type Routes' :: forall {k}. k -> Type
data Routes' mode = Routes'
  { listGet :: mode :- Get '[HTML] (RespHeaders ListProjects.ListProjectsGet)
  , createGet :: mode :- "p" :> "new" :> Get '[HTML] (RespHeaders CreateProject.CreateProject) -- p represents project
  , createPost :: mode :- "p" :> "new" :> ReqBody '[FormUrlEncoded] CreateProject.CreateProjectForm :> Post '[HTML] (RespHeaders CreateProject.CreateProject)
  , settingsGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "settings" :> Get '[HTML] (RespHeaders CreateProject.CreateProject)
  , integrationGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "integrations" :> Get '[HTML] (RespHeaders (Html ()))
  , deleteGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "delete" :> Get '[HTML] (RespHeaders CreateProject.CreateProject)
  , notificationsUpdateChannelPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "notifications-channels" :> ReqBody '[FormUrlEncoded] Integrations.NotifListForm :> Post '[HTML] (RespHeaders (Html ()))
  , deleteProjectGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "delete" :> Get '[HTML] (RespHeaders CreateProject.CreateProject)
  , membersManageGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> Get '[HTML] (RespHeaders ManageMembers.ManageMembers)
  , membersManagePost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> ReqBody '[FormUrlEncoded] ManageMembers.ManageMembersForm :> Post '[HTML] (RespHeaders ManageMembers.ManageMembers)
  , manageSubscriptionGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_subscription" :> Get '[HTML] (RespHeaders (Html ()))
  }
  deriving stock (Generic)
