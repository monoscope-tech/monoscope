module Pages.Projects.Server (server) where

import Pages.Onboarding.Onboarding qualified as Onboarding
import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.Integrations qualified as Integrations
import Pages.Projects.ListProjects qualified as ListProjects
import Pages.Projects.ManageMembers qualified as ManageMembers
import Pages.Projects.Routes (Routes, Routes' (..))
import Servant qualified
import System.Types (ATAuthCtx)


server :: Servant.ServerT Routes ATAuthCtx
server =
  Routes'
    { listGet = ListProjects.listProjectsGetH
    , onboardingProject = CreateProject.projectOnboarding
    , -- , createGet = CreateProject.createProjectGetH
      createPost = CreateProject.createProjectPostH
    , settingsGet = CreateProject.projectSettingsGetH
    , integrationGet = Integrations.integrationsSettingsGetH
    , deleteGet = CreateProject.deleteProjectGetH
    , notificationsUpdateChannelPost = Integrations.updateNotificationsChannel
    , deleteProjectGet = CreateProject.deleteProjectGetH
    , membersManageGet = ManageMembers.manageMembersGetH
    , membersManagePost = ManageMembers.manageMembersPostH
    , manageSubscriptionGet = ManageMembers.manageSubGetH
    , onboading = Onboarding.onboardingGetH
    , onboardingInfoPost = Onboarding.onboardingInfoPost
    , onboardingConfPost = Onboarding.onboardingConfPost
    , onboardingDiscordPost = Onboarding.discorPostH
    , onboardingPhoneEmailsPost = Onboarding.phoneEmailPostH
    , onboardingIntegrationCheck = Onboarding.checkIntegrationGet
    , onboardingPricingUpdate = CreateProject.pricingUpdateH
    }
