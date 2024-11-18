module Pages.Projects.Routes (Routes, Routes' (..)) where

import GHC.Generics (Generic)
import Lucid (Html)
import Models.Projects.Projects qualified as Projects
import Pages.Onboarding.Handler qualified as OnboardingH
import Pages.Onboarding.Types qualified as OnboardingT
import Pages.Onboarding.Views qualified as OnboardingV
import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.Integrations qualified as Integrations
import Pages.Projects.ListProjects qualified as ListProjects
import Pages.Projects.ManageMembers qualified as ManageMembers
import Servant (Capture, FormUrlEncoded, GenericMode (type (:-)), Get, NamedRoutes, Post, ReqBody, type (:>))
import Servant.HTML.Lucid (HTML)
import System.Types (RespHeaders)


type role Routes' nominal


type Routes = NamedRoutes Routes'
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
  , -- Onboarding routes
    onboardingSignupGet :: mode :- "onboarding" :> "signup" :> Get '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , -- , onboardingSignupPost :: mode :- "onboarding" :> "signup" :> ReqBody '[FormUrlEncoded] Onboarding.SignupForm :> Post '[HTML] (RespHeaders OnboardingH.OnboardingResponse)
    onboardingLoginGet :: mode :- "onboarding" :> "login" :> Get '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , -- , onboardingLoginPost :: mode :- "onboarding" :> "login" :> ReqBody '[FormUrlEncoded] Onboarding.LoginForm :> Post '[HTML] (RespHeaders OnboardingH.OnboardingResponse)
    onboardingProfileGet :: mode :- "onboarding" :> "profile" :> Get '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingProfilePost :: mode :- "onboarding" :> "profile" :> ReqBody '[FormUrlEncoded] OnboardingT.ProfileForm :> Post '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingHostingGet :: mode :- "onboarding" :> "hosting" :> Get '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingHostingPost :: mode :- "onboarding" :> "hosting" :> ReqBody '[FormUrlEncoded] OnboardingT.HostingLocation :> Post '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingUsageGet :: mode :- "onboarding" :> "usage" :> Get '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingUsagePost :: mode :- "onboarding" :> "usage" :> ReqBody '[FormUrlEncoded] OnboardingT.UsagePreferences :> Post '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingUrlMonitorGet :: mode :- "onboarding" :> "url-monitor" :> Get '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingUrlMonitorPost :: mode :- "onboarding" :> "url-monitor" :> ReqBody '[FormUrlEncoded] OnboardingT.URLMonitorConfig :> Post '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingNotificationsGet :: mode :- "onboarding" :> "notifications" :> Get '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingNotificationsPost :: mode :- "onboarding" :> "notifications" :> ReqBody '[FormUrlEncoded] OnboardingT.NotificationSettings :> Post '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingTeamGet :: mode :- "onboarding" :> "team" :> Get '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingTeamPost :: mode :- "onboarding" :> "team" :> ReqBody '[FormUrlEncoded] OnboardingT.TeamInvitationList :> Post '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingPricingGet :: mode :- "onboarding" :> "pricing" :> Get '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingPricingPost :: mode :- "onboarding" :> "pricing" :> ReqBody '[FormUrlEncoded] OnboardingT.PricingPlan :> Post '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingCheckInbox :: mode :- "onboarding" :> "check-inbox" :> Get '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingFrameworkGet :: mode :- "onboarding" :> "framework" :> Get '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingNotificationSentGet :: mode :- "onboarding" :> "notification-sent" :> Get '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  , onboardingNotificationSentPost :: mode :- "onboarding" :> "notification-sent" :> ReqBody '[FormUrlEncoded] OnboardingT.NotificationConfirmation :> Post '[HTML] (RespHeaders OnboardingV.OnboardingResponse)
  }
  deriving stock (Generic)
