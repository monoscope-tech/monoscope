module Pages.Projects.Routes (Routes, Routes' (..)) where

import GHC.Generics
import Lucid (Html)
import Models.Projects.Projects qualified as Projects
import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.ManageMembers qualified as ManageMembers
import Pages.Projects.Integrations qualified as Integrations
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Htmx
import Utils


type role Routes' nominal


type Routes = NamedRoutes Routes'
data Routes' mode = Routes'
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
  }
  deriving stock (Generic)
