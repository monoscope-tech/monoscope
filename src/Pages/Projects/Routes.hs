module Pages.Projects.Routes (Routes, Routes' (..)) where

import Lucid (Html)
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx)
import Pages.Onboarding.Onboarding qualified as Onboarding
import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.Integrations qualified as Integrations
import Pages.Projects.ListProjects qualified as ListProjects
import Pages.Projects.ManageMembers qualified as ManageMembers
import Pkg.RouteUtils (QPT)
import Relude
import Servant (Capture, FormUrlEncoded, GenericMode (type (:-)), Get, JSON, NamedRoutes, Post, ReqBody, type (:>))
import Servant.HTML.Lucid (HTML)
import System.Types (RespHeaders)


type role Routes' nominal


type Routes = NamedRoutes Routes'


type Routes' :: Type -> Type
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
  , membersManagePost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_members" :> QPT "onboarding" :> ReqBody '[FormUrlEncoded] ManageMembers.ManageMembersForm :> Post '[HTML] (RespHeaders ManageMembers.ManageMembers)
  , manageSubscriptionGet :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "manage_subscription" :> Get '[HTML] (RespHeaders (Html ()))
  , onboading :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> QPT "step" :> Get '[HTML] (RespHeaders (PageCtx (Html ())))
  , onboardingInfoPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "info" :> ReqBody '[FormUrlEncoded] Onboarding.OnboardingInfoForm :> Post '[HTML] (RespHeaders (Html ()))
  , onboardingConfPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "survey" :> ReqBody '[FormUrlEncoded] Onboarding.OnboardingConfForm :> Post '[HTML] (RespHeaders (Html ()))
  , onboardingDiscordPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "discord" :> ReqBody '[FormUrlEncoded] Onboarding.DiscordForm :> Post '[HTML] (RespHeaders (Html ()))
  , onboardingPhoneEmailsPost :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "phone-emails" :> ReqBody '[JSON] Onboarding.NotifChannelForm :> Post '[HTML] (RespHeaders (Html ()))
  , onboardingIntegrationCheck :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "integration-check" :> Capture "language" Text :> Get '[HTML] (RespHeaders (Html ()))
  , onboardingPricingUpdate :: mode :- "p" :> Capture "projectId" Projects.ProjectId :> "onboarding" :> "pricing" :> ReqBody '[FormUrlEncoded] CreateProject.PricingUpdateForm :> Post '[HTML] (RespHeaders (Html ()))
  }
  deriving stock (Generic)
