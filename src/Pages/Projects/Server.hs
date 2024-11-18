module Pages.Projects.Server (server) where

import Pages.Onboarding.Handler qualified as OnboardingH
import Pages.Onboarding.Types qualified as OnboardingT
import Pages.Onboarding.Views qualified as OnboardingV
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
    , createGet = CreateProject.createProjectGetH
    , createPost = CreateProject.createProjectPostH
    , settingsGet = CreateProject.projectSettingsGetH
    , integrationGet = Integrations.integrationsSettingsGetH
    , deleteGet = CreateProject.deleteProjectGetH
    , notificationsUpdateChannelPost = Integrations.updateNotificationsChannel
    , deleteProjectGet = CreateProject.deleteProjectGetH
    , membersManageGet = ManageMembers.manageMembersGetH
    , membersManagePost = ManageMembers.manageMembersPostH
    , manageSubscriptionGet = ManageMembers.manageSubGetH
    , -- Onboarding routes
      onboardingSignupGet = OnboardingH.onboardingSignupGetH
    , -- , onboardingSignupPost = OnboardingH.onboardingSignupPostH
      onboardingLoginGet = OnboardingH.onboardingLoginGetH
    , -- , onboardingLoginPost = OnboardingH.onboardingLoginPostH
      onboardingProfileGet = OnboardingH.onboardingProfileGetH
    , onboardingProfilePost = OnboardingH.onboardingProfilePostH
    , onboardingHostingGet = OnboardingH.onboardingHostingGetH
    , onboardingHostingPost = OnboardingH.onboardingHostingPostH
    , onboardingUsageGet = OnboardingH.onboardingUsageGetH
    , onboardingUsagePost = OnboardingH.onboardingUsagePostH
    , onboardingUrlMonitorGet = OnboardingH.onboardingUrlMonitorGetH
    , onboardingUrlMonitorPost = OnboardingH.onboardingUrlMonitorPostH
    , onboardingNotificationsGet = OnboardingH.onboardingNotificationsGetH
    , onboardingNotificationsPost = OnboardingH.onboardingNotificationsPostH
    , onboardingTeamGet = OnboardingH.onboardingTeamGetH
    , onboardingTeamPost = OnboardingH.onboardingTeamPostH
    , onboardingPricingGet = OnboardingH.onboardingPricingGetH
    , onboardingPricingPost = OnboardingH.onboardingPricingPostH
    , onboardingCheckInbox = OnboardingH.onboardingCheckInboxGetH
    , onboardingFrameworkGet = OnboardingH.onboardingFrameworkGetH
    , onboardingNotificationSentGet = OnboardingH.onboardingNotificationSentGetH
    , onboardingNotificationSentPost = OnboardingH.onboardingNotificationSentPostH
    }
