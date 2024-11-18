module Pages.Onboarding.SessionManager (
  -- Types
  OnboardingSessionKeys (..),
  OnboardingStep (..),
  defaultSessionKeys,
  -- Session storage functions
  storeInSession,
  getFromSession,
  -- Step management
  getCurrentStep,
  setCurrentStep,
  getNextStep,
  updateAndProgress,
  -- Store functions
  storeProfile,
  storeHosting,
  storeUsage,
  storeMonitor,
  storeNotifications,
  storeTeam,
  storePricing,
  -- Retrieve functions
  getProfile,
  getHosting,
  getUsage,
  getMonitor,
  getNotifications,
  getTeam,
  getPricing,
) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy qualified as LBS -- Add this import
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Models.Users.Sessions qualified as Sessions
import Pages.Onboarding.Types
import Relude


-- | Keys used for storing different onboarding sections in session
data OnboardingSessionKeys = OnboardingSessionKeys
  { profileKey :: Text
  , hostingKey :: Text
  , usageKey :: Text
  , monitorKey :: Text
  , notificationsKey :: Text
  , teamKey :: Text
  , pricingKey :: Text
  , integrationKey :: Text
  , currentStepKey :: Text
  }


-- | Default session keys
defaultSessionKeys :: OnboardingSessionKeys
defaultSessionKeys =
  OnboardingSessionKeys
    { profileKey = "onboarding_profile"
    , hostingKey = "onboarding_hosting"
    , usageKey = "onboarding_usage"
    , monitorKey = "onboarding_monitor"
    , notificationsKey = "onboarding_notifications"
    , teamKey = "onboarding_team"
    , pricingKey = "onboarding_pricing"
    , integrationKey = "onboarding_integration"
    , currentStepKey = "onboarding_current_step"
    }


-- | Onboarding steps
data OnboardingStep
  = ProfileStep
  | HostingStep
  | UsageStep
  | MonitorStep
  | NotificationsStep
  | TeamStep
  | PricingStep
  | IntegrationStep
  | CompletedStep
  deriving (Show, Eq, Generic, FromJSON, ToJSON)


-- | Helper function to store JSON-serializable data in session
storeInSession
  :: ToJSON a
  => Sessions.PersistentSession
  -> Text
  -- ^ Key
  -> a
  -- ^ Value
  -> Sessions.SessionData
storeInSession session key value =
  let encodedValue = decodeUtf8 $ encode value
      currentData = Sessions.getSessionData session.sessionData
   in Sessions.SessionData $ Map.insert key encodedValue currentData


-- | Helper function to retrieve JSON-serializable data from session
getFromSession
  :: FromJSON a
  => Sessions.PersistentSession
  -> Text
  -- ^ Key
  -> Maybe a
getFromSession session key = do
  value <- Map.lookup key (Sessions.getSessionData session.sessionData)
  -- Convert strict ByteString to lazy ByteString before decoding
  decode $ encodeUtf8 value


-- | Get current onboarding step
getCurrentStep :: Sessions.PersistentSession -> OnboardingStep
getCurrentStep session =
  fromMaybe ProfileStep $ getFromSession session defaultSessionKeys.currentStepKey


-- | Set current onboarding step
setCurrentStep :: Sessions.PersistentSession -> OnboardingStep -> Sessions.SessionData
setCurrentStep session step = storeInSession session defaultSessionKeys.currentStepKey step


-- | Store profile data
storeProfile :: Sessions.PersistentSession -> ProfileForm -> Sessions.SessionData
storeProfile session = storeInSession session defaultSessionKeys.profileKey


-- | Store hosting location
storeHosting :: Sessions.PersistentSession -> HostingLocation -> Sessions.SessionData
storeHosting session = storeInSession session defaultSessionKeys.hostingKey


-- | Store usage preferences
storeUsage :: Sessions.PersistentSession -> UsagePreferences -> Sessions.SessionData
storeUsage session = storeInSession session defaultSessionKeys.usageKey


-- | Store URL monitor config
storeMonitor :: Sessions.PersistentSession -> URLMonitorConfig -> Sessions.SessionData
storeMonitor session = storeInSession session defaultSessionKeys.monitorKey


-- | Store notification settings
storeNotifications :: Sessions.PersistentSession -> NotificationSettings -> Sessions.SessionData
storeNotifications session = storeInSession session defaultSessionKeys.notificationsKey


-- | Store team invitations
storeTeam :: Sessions.PersistentSession -> TeamInvitationList -> Sessions.SessionData
storeTeam session = storeInSession session defaultSessionKeys.teamKey


-- | Store pricing plan
storePricing :: Sessions.PersistentSession -> PricingPlan -> Sessions.SessionData
storePricing session = storeInSession session defaultSessionKeys.pricingKey


-- | Get the next step based on current step
getNextStep :: OnboardingStep -> OnboardingStep
getNextStep = \case
  ProfileStep -> HostingStep
  HostingStep -> UsageStep
  UsageStep -> MonitorStep
  MonitorStep -> NotificationsStep
  NotificationsStep -> TeamStep
  TeamStep -> PricingStep
  PricingStep -> IntegrationStep
  IntegrationStep -> CompletedStep
  CompletedStep -> CompletedStep


-- | Update session and move to next step
updateAndProgress
  :: Sessions.PersistentSession
  -> (Sessions.PersistentSession -> a -> Sessions.SessionData)
  -- ^ Store function
  -> a
  -- ^ Data to store
  -> Sessions.SessionData
updateAndProgress session storeFunc data_ =
  let currentStep = getCurrentStep session
      nextStep = getNextStep currentStep
      sessionWithData = storeFunc session data_
      sessionData = Sessions.getSessionData sessionWithData
   in Sessions.SessionData $ Map.insert defaultSessionKeys.currentStepKey (decodeUtf8 $ encode nextStep) sessionData


-- Add these functions to SessionManager.hs

-- | Get profile data from session
getProfile :: Sessions.PersistentSession -> Maybe ProfileForm
getProfile session = getFromSession session defaultSessionKeys.profileKey


-- | Get hosting location from session
getHosting :: Sessions.PersistentSession -> Maybe HostingLocation
getHosting session = getFromSession session defaultSessionKeys.hostingKey


-- | Get usage preferences from session
getUsage :: Sessions.PersistentSession -> Maybe UsagePreferences
getUsage session = getFromSession session defaultSessionKeys.usageKey


-- | Get URL monitor config from session
getMonitor :: Sessions.PersistentSession -> Maybe URLMonitorConfig
getMonitor session = getFromSession session defaultSessionKeys.monitorKey


-- | Get notification settings from session
getNotifications :: Sessions.PersistentSession -> Maybe NotificationSettings
getNotifications session = getFromSession session defaultSessionKeys.notificationsKey


-- | Get team invitations from session
getTeam :: Sessions.PersistentSession -> Maybe TeamInvitationList
getTeam session = getFromSession session defaultSessionKeys.teamKey


-- | Get pricing plan from session
getPricing :: Sessions.PersistentSession -> Maybe PricingPlan
getPricing session = getFromSession session defaultSessionKeys.pricingKey
