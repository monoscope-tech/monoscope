module Pages.Projects.Routes (Routes, Routes' (..)) where

import GHC.Generics (Generic)
import Lucid (Html)
import Models.Projects.Projects qualified as Projects
import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.Integrations qualified as Integrations
import Pages.Projects.ManageMembers qualified as ManageMembers
import Servant (Capture, FormUrlEncoded, GenericMode (type (:-)), Get, NamedRoutes, Post, ReqBody, type (:>))
import Servant.HTML.Lucid (HTML)
import System.Types (RespHeaders)


type role Routes' nominal


type Routes = NamedRoutes Routes'
data Routes' mode = Routes'
<<<<<<< HEAD
  { listGet :: mode :- UVerb 'GET '[HTML] GetOrRedirect
  , createGet :: mode :- "p" :> "new" :> Get '[HTML] (Html ()) -- p represents project
  , createPost :: mode :- "p" :> "new" :> ReqBody '[FormUrlEncoded] CreateProject.CreateProjectForm :> Post '[HTML] (Headers '[HXTrigger, HXRedirect] (Html ()))
  , settingsGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "settings" :> Get '[HTML] (Html ())
  , integrationSettingsGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "integrations" :> Get '[HTML] (Html ())
  , deleteGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "delete" :> Get '[HTML] (Headers '[HXTrigger, HXRedirect] (Html ()))
  , notificationsUpdateChannelPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "notifications-channels" :> ReqBody '[FormUrlEncoded] Integrations.NotifListForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , deleteProjectGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "delete" :> Get '[HTML] (Headers '[HXTrigger, HXRedirect] (Html ()))
  , membersManageGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> Get '[HTML] (Html ())
  , membersManagePost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> ReqBody '[FormUrlEncoded] ManageMembers.ManageMembersForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , manageSubscriptionGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_subscription" :> Get '[HTML] (Headers '[HXTrigger, HXRedirect] (Html ()))
=======
  { listGet :: mode :- Get '[HTML] (RespHeaders (Html ()))
  , createGet :: mode :- "p" :> "new" :> Get '[HTML] (RespHeaders (Html ())) -- p represents project
  , createPost :: mode :- "p" :> "new" :> ReqBody '[FormUrlEncoded] CreateProject.CreateProjectForm :> Post '[HTML] (RespHeaders (Html ()))
  , settingsGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "settings" :> Get '[HTML] (RespHeaders (Html ()))
  , deleteGet :: mode :- "p" :> Capture "projectID" Projects.ProjectId :> "delete" :> Get '[HTML] (RespHeaders (Html ()))
  , notificationsUpdateChannelPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "notifications-channels" :> ReqBody '[FormUrlEncoded] CreateProject.NotifListForm :> Post '[HTML] (RespHeaders (Html ()))
  , deleteProjectGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "delete" :> Get '[HTML] (RespHeaders (Html ()))
  , membersManageGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> Get '[HTML] (RespHeaders (Html ()))
  , membersManagePost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> ReqBody '[FormUrlEncoded] ManageMembers.ManageMembersForm :> Post '[HTML] (RespHeaders (Html ()))
  , manageSubscriptionGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_subscription" :> Get '[HTML] (RespHeaders (Html ()))
>>>>>>> 79a0175357bf23f3e10923b005a24c12a0fbb7f7
  }
  deriving stock (Generic)
