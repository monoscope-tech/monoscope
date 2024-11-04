module Pages.Onboarding.Handler (
  onboardingSignupGetH,
  onboardingLoginGetH,
  onboardingProfileGetH,
  onboardingHostingGetH,
  onboardingUsageGetH,
  onboardingUrlMonitorGetH,
  onboardingNotificationsGetH,
  onboardingTeamGetH,
  onboardingPricingGetH,
  onboardingCheckInboxGetH,
  onboardingFrameworkGetH,
  onboardingNotificationSentGetH,
) where

import BackgroundJobs qualified
import Control.Lens ((^.))
import Data.Aeson qualified as AE
import Data.Base64.Types qualified as B64
import Data.ByteString.Base64 qualified as B64
import Data.Default (Default (..), def)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Transact (DBT)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid hiding (for_)
import Lucid.Htmx (hxPost_, hxSwap_, hxTarget_, hxTrigger_)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import OddJobs.Job (createJob)
import Pages.BodyWrapper
import Pages.Onboarding.Types
import Pages.Onboarding.Views
import Relude hiding (ask, asks)
import System.Config
import System.Types
import Utils (faSprite_, isDemoAndNotSudo)
import Web.FormUrlEncoded (FromForm)


-- | Initial handler
onboardingSignupGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingSignupGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , pageTitle = "Signup"
          }
  addRespHeaders $ SignupR $ PageCtx bwconf (sess.persistentSession, appCtx.config, def, Valid def)


-- -- | Login Handlers
onboardingLoginGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingLoginGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , pageTitle = "Login"
          }
  addRespHeaders $ LoginR $ PageCtx bwconf (sess.persistentSession, appCtx.config, def, Valid def)


-- | Profile Handlers
onboardingProfileGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingProfileGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  --   onboardingState <- dbtToEff $ getOnboardingState sess.user.id
  -- Since onboardingState is Nothing, let's just use the default ProfileForm
  let form = def :: ProfileForm
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , pageTitle = "Complete Profile"
          }
  addRespHeaders $ ProfileR $ PageCtx bwconf (sess.persistentSession, appCtx.config, form, Valid form)


-- -- | Hosting Location Handlers
onboardingHostingGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingHostingGetH = do
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Choose Hosting Location"}
  let location = def :: HostingLocation
  addRespHeaders $ HostingR $ PageCtx bwconf (def, appCtx.config, location)


-- -- | Usage Preferences Handlers
onboardingUsageGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingUsageGetH = do
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Usage Preferences"}
  let usage = def :: UsagePreferences
  addRespHeaders $ UsageR $ PageCtx bwconf (def, appCtx.config, usage)


-- -- | URL Monitor Handlers
onboardingUrlMonitorGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingUrlMonitorGetH = do
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Configure URL Monitor"}
  let monitor = def :: URLMonitorConfig
  addRespHeaders $ URLMonitorR $ PageCtx bwconf (def, appCtx.config, monitor)


-- -- | Notification Settings Handlers
onboardingNotificationsGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingNotificationsGetH = do
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Notification Settings"}
  let notifications = def :: NotificationSettings
  addRespHeaders $ NotificationsR $ PageCtx bwconf (def, appCtx.config, notifications)


-- -- | Team Invitation Handlers
onboardingTeamGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingTeamGetH = do
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Invite Team Members"}
  let team = def :: TeamInvitationList
  addRespHeaders $ TeamR $ PageCtx bwconf (def, appCtx.config, team)


-- -- | Pricing Plan Handlers
onboardingPricingGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingPricingGetH = do
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Choose Plan"}
  let pricing = def :: PricingPlan
  addRespHeaders $ PricingR $ PageCtx bwconf (def, appCtx.config, pricing)


-- -- | Check Inbox Handler
onboardingCheckInboxGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingCheckInboxGetH = do
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Check Your Inbox"}
  addRespHeaders $ CheckInboxR $ PageCtx bwconf (def, appCtx.config)


-- | Framework Integration Handler
onboardingFrameworkGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingFrameworkGetH = do
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Framework Integration"}
  let framework = def :: FrameworkIntegration
  addRespHeaders $ FrameworkR $ PageCtx bwconf (def, appCtx.config, framework)


-- | Notification Sent Handler
onboardingNotificationSentGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingNotificationSentGetH = do
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Notification Sent"}
  addRespHeaders $ NotificationSentR $ PageCtx bwconf (def, appCtx.config)


-- POST Request Handlers;

-- | Signup Handler
-- onboardingSignupPostH :: SignupForm -> ATAuthCtx (RespHeaders OnboardingResponse)
-- onboardingSignupPostH form = do
--   sess <- Sessions.getSession
--   appCtx <- ask @AuthContext
--   case validateSignupForm form of
--     Valid validForm -> do
--       userM <- dbtToEff $ Users.createUser validForm.signupEmail validForm.signupPassword
--       case userM of
--         Just user -> do
--           now <- liftIO getCurrentTime
--           let onboardingState = initOnboardingState form now
--           dbtToEff $ saveOnboardingState user.id onboardingState
--           addSuccessToast "Signup successful" Nothing
--           redirectCS "/onboarding/profile"
--           addRespHeaders $ NoContent ""
--         Nothing -> do
--           addErrorToast "Email already exists" Nothing
--           addRespHeaders $ SignupR $ PageCtx def (sess.persistentSession, appCtx.config, form, Invalid [FormError "email" "Email already exists"])
--     Invalid errs ->
--       addRespHeaders $ SignupR $ PageCtx def (sess.persistentSession, appCtx.config, form, Invalid errs)

-- -- | Login Handler
-- onboardingLoginPostH :: LoginForm -> ATAuthCtx (RespHeaders OnboardingResponse)
-- onboardingLoginPostH form = do
--   sess <- Sessions.getSession
--   appCtx <- ask @AuthContext
--   let userM = Nothing
--   -- userM <- dbtToEff $ Users. form.loginEmail form.loginPassword
--   case userM of
--     Just user -> do
--       -- Create session and redirect
--       addSuccessToast "Login successful" Nothing
--       redirectCS "/onboarding/profile"
--       addRespHeaders $ NoContent ""
--     Nothing -> do
--       addErrorToast "Invalid credentials" Nothing
--       addRespHeaders $ LoginR $ PageCtx def (sess.persistentSession, appCtx.config, form, Invalid [FormError "credentials" "Invalid email or password"])

-- -- | Profile Handler
-- onboardingProfilePostH :: ProfileForm -> ATAuthCtx (RespHeaders OnboardingResponse)
-- onboardingProfilePostH form = do
--   sess <- Sessions.getSession
--   appCtx <- ask @AuthContext
--   case validateProfileForm form of
--     Valid validForm -> do
--       -- Update onboarding state
--       dbtToEff $ updateOnboardingProfile sess.user.id validForm
--       addSuccessToast "Profile updated" Nothing
--       redirectCS "/onboarding/hosting"
--       addRespHeaders $ NoContent ""
--     Invalid errs ->
--       addRespHeaders $ ProfileR $ PageCtx def (sess.persistentSession, appCtx.config, form, Invalid errs)

-- -- | Hosting Location Handler
-- onboardingHostingPostH :: HostingLocation -> ATAuthCtx (RespHeaders OnboardingResponse)
-- onboardingHostingPostH location = do
--   sess <- Sessions.getSession
--   dbtToEff $ updateOnboardingHosting sess.user.id location
--   addSuccessToast "Hosting location selected" Nothing
--   redirectCS "/onboarding/usage"
--   addRespHeaders $ NoContent ""

-- -- | Usage Preferences Handler
-- onboardingUsagePostH :: UsagePreferences -> ATAuthCtx (RespHeaders OnboardingResponse)
-- onboardingUsagePostH prefs = do
--   sess <- Sessions.getSession
--   dbtToEff $ updateOnboardingUsage sess.user.id prefs
--   addSuccessToast "Usage preferences saved" Nothing
--   redirectCS "/onboarding/url-monitor"
--   addRespHeaders $ NoContent ""

-- -- | URL Monitor Handler
-- onboardingUrlMonitorPostH :: URLMonitorConfig -> ATAuthCtx (RespHeaders OnboardingResponse)
-- onboardingUrlMonitorPostH config = do
--   sess <- Sessions.getSession
--   dbtToEff $ updateOnboardingMonitor sess.user.id config
--   addSuccessToast "URL Monitor configured" Nothing
--   redirectCS "/onboarding/notifications"
--   addRespHeaders $ NoContent ""

-- -- | Notification Settings Handler
-- onboardingNotificationsPostH :: NotificationSettings -> ATAuthCtx (RespHeaders OnboardingResponse)
-- onboardingNotificationsPostH settings = do
--   sess <- Sessions.getSession
--   appCtx <- ask @AuthContext

--   -- Process notification channels (Slack, Discord)
--   forM_ settings.notificationChannels \case {}

--   dbtToEff $ updateOnboardingNotifications sess.user.id settings
--   addSuccessToast "Notification settings saved" Nothing
--   redirectCS "/onboarding/team"
--   addRespHeaders $ NoContent ""

-- -- | Team Invitation Handler
-- onboardingTeamPostH :: TeamInvitation -> ATAuthCtx (RespHeaders OnboardingResponse)
-- onboardingTeamPostH invitation = do
--   sess <- Sessions.getSession
--   appCtx <- ask @AuthContext

--   -- Process team invitations
--   forM_ invitation.teamMembers \TeamMember{memberEmail, memberRole} -> do
--     userIdM <- dbtToEff $ Users.userIdByEmail memberEmail
--     userId <- case userIdM of
--       Just id -> pure id
--       Nothing -> dbtToEff $ Users.createEmptyUser memberEmail

--     -- Create background job for sending invitation
--     liftIO $ withResource appCtx.pool \conn ->
--       createJob conn "background_jobs"
--         $ BackgroundJobs.InviteUserToProject
--           sess.user.id
--           (Projects.ProjectId "temp") -- You'll need the actual project ID
--           memberEmail
--           "New Project" -- You'll need the actual project title
--   dbtToEff $ updateOnboardingTeam sess.user.id invitation
--   addSuccessToast "Team invitations sent" Nothing
--   redirectCS "/onboarding/pricing"
--   addRespHeaders $ NoContent ""

-- -- | Pricing Plan Handler
-- onboardingPricingPostH :: PricingPlan -> ATAuthCtx (RespHeaders OnboardingResponse)
-- onboardingPricingPostH plan = do
--   sess <- Sessions.getSession
--   appCtx <- ask @AuthContext

--   -- Get the current onboarding state
--   onboardingState <- dbtToEff $ getOnboardingState sess.user.id

--   case onboardingState of
--     Nothing -> do
--       addErrorToast "Onboarding state not found" Nothing
--       redirectCS "/onboarding/signup"
--       addRespHeaders $ NoContent ""
--     Just state -> do
--       pass

-- -- | Database operations
-- saveOnboardingState :: Users.UserId -> OnboardingState -> DBT IO ()
-- saveOnboardingState userId state = pass -- TODO: Implement

-- getOnboardingState :: Users.UserId -> DBT IO (Maybe OnboardingState)
-- getOnboardingState userId = pass -- TODO: Implement

-- updateOnboardingProfile :: Users.UserId -> ProfileForm -> DBT IO ()
-- updateOnboardingProfile userId form = pass -- TODO: Implement

-- -- | Helper functions
-- validateSignupForm :: SignupForm -> FormValidationResult SignupForm
-- validateSignupForm form =
--   case (validateEmail form.signupEmail, validatePassword form.signupPassword) of
--     (Nothing, Nothing) -> Valid form
--     (emailErr, passErr) -> Invalid $ catMaybes [emailErr, passErr]

-- validateProfileForm :: ProfileForm -> FormValidationResult ProfileForm
-- validateProfileForm = undefined -- TODO: Implement