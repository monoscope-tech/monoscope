{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pages.Onboarding.Handler (
  onboardingSignupGetH,
  onboardingLoginGetH,
  onboardingProfileGetH,
  onboardingHostingGetH,
  onboardingHostingPostH,
  onboardingUsageGetH,
  onboardingUsagePostH,
  onboardingUrlMonitorGetH,
  onboardingUrlMonitorPostH,
  onboardingNotificationsGetH,
  onboardingNotificationsPostH,
  onboardingTeamGetH,
  onboardingTeamPostH,
  onboardingPricingGetH,
  onboardingPricingPostH,
  onboardingCheckInboxGetH,
  onboardingFrameworkGetH,
  onboardingProfilePostH,
  onboardingNotificationSentGetH,
  onboardingNotificationSentPostH,
) where

import BackgroundJobs (BgJobs (..))
import BackgroundJobs qualified
import Control.Lens ((^.))
import Data.Aeson (encode)
import Data.Char (isLetter, isNumber, isSpace)
import Data.Default (Default (..), def)
import Data.Map.Strict qualified as Map
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Transact (DBT)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Models.Apis.Slack (insertAccessToken)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import Pages.BodyWrapper
import Pages.Onboarding.SessionManager
import Pages.Onboarding.Types
import Pages.Onboarding.Views
import Relude hiding (ask, asks)
import System.Config
import System.Types
import Utils (faSprite_, isDemoAndNotSudo)


-- | Initial handlers
onboardingSignupGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingSignupGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Signup"}
  addRespHeaders $ SignupR $ PageCtx bwconf (sess.persistentSession, appCtx.config, def, Valid def)


onboardingLoginGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingLoginGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Login"}
  addRespHeaders $ LoginR $ PageCtx bwconf (sess.persistentSession, appCtx.config, def, Valid def)


-- | Profile Handlers
onboardingProfileGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingProfileGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let form = fromMaybe def $ getProfile sess.persistentSession
  let bwconf = (def :: BWConfig){pageTitle = "Complete Profile"}
  addRespHeaders $ ProfileR $ PageCtx bwconf (sess.persistentSession, appCtx.config, form, Valid form)


-- -- | Profile Handler
onboardingProfilePostH :: ProfileForm -> ATAuthCtx (RespHeaders OnboardingResponse)
onboardingProfilePostH form = do
  -- traceShowM form
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  case validateProfileForm form of
    Valid validForm -> do
      -- Convert form to session data
      let profileKey = "onboarding_profile"
          profileValue = decodeUtf8 $ encode validForm
          updatedSessionData =
            Sessions.SessionData
              $ Map.insert profileKey profileValue
              $ Sessions.getSessionData sess.persistentSession.sessionData

      -- Update session with new profile data
      void
        $ Sessions.updateSession
          sess.persistentSession.id
          sess.persistentSession.userId
          updatedSessionData

      -- Add success notification and redirect
      addSuccessToast "Profile updated" Nothing
      redirectCS "/onboarding/hosting"
      addRespHeaders $ NoContent ""
    Invalid errs ->
      addRespHeaders $ ProfileR $ PageCtx def (sess.persistentSession, appCtx.config, form, Invalid errs)


-- onboardingProfilePostH :: ProfileForm -> ATAuthCtx (RespHeaders OnboardingResponse)
-- onboardingProfilePostH form = do
--   sess <- Sessions.getSession
--   appCtx <- ask @AuthContext
--   case validateProfileForm form of
--     Valid validForm -> do
--       -- Store form data in session and progress to next step
--       let sessionData = updateAndProgress sess.persistentSession storeProfile validForm
--       -- Update session
--       void $ Sessions.updateSession sess.persistentSession.id sess.persistentSession.userId sessionData
--       addSuccessToast "Profile updated" Nothing
--       redirectCS "/onboarding/hosting"
--       addRespHeaders $ NoContent ""
--     Invalid errs ->
--       addRespHeaders $ ProfileR $ PageCtx def (sess.persistentSession, appCtx.config, form, Invalid errs)

-- | Hosting Location Handlers
onboardingHostingGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingHostingGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Choose Hosting Location"}
  let location = fromMaybe def $ getHosting sess.persistentSession
  addRespHeaders $ HostingR $ PageCtx bwconf (sess.persistentSession, appCtx.config, location)


onboardingHostingPostH :: HostingLocation -> ATAuthCtx (RespHeaders OnboardingResponse)
onboardingHostingPostH location = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let sessionData = updateAndProgress sess.persistentSession storeHosting location
  void $ Sessions.updateSession sess.persistentSession.id sess.persistentSession.userId sessionData
  addSuccessToast "Hosting location selected" Nothing
  redirectCS "/onboarding/usage"
  addRespHeaders $ NoContent ""


-- | Usage Preferences Handlers
onboardingUsageGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingUsageGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Usage Preferences"}
  let usage = fromMaybe def $ getUsage sess.persistentSession
  addRespHeaders $ UsageR $ PageCtx bwconf (sess.persistentSession, appCtx.config, usage)


onboardingUsagePostH :: UsagePreferences -> ATAuthCtx (RespHeaders OnboardingResponse)
onboardingUsagePostH prefs = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let sessionData = updateAndProgress sess.persistentSession storeUsage prefs
  void $ Sessions.updateSession sess.persistentSession.id sess.persistentSession.userId sessionData
  addSuccessToast "Usage preferences saved" Nothing
  redirectCS "/onboarding/url-monitor"
  addRespHeaders $ NoContent ""


-- | URL Monitor Handlers
onboardingUrlMonitorGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingUrlMonitorGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Configure URL Monitor"}
  let monitor = fromMaybe def $ getMonitor sess.persistentSession
  addRespHeaders $ URLMonitorR $ PageCtx bwconf (sess.persistentSession, appCtx.config, monitor)


onboardingUrlMonitorPostH :: URLMonitorConfig -> ATAuthCtx (RespHeaders OnboardingResponse)
onboardingUrlMonitorPostH config = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let sessionData = updateAndProgress sess.persistentSession storeMonitor config
  void $ Sessions.updateSession sess.persistentSession.id sess.persistentSession.userId sessionData
  addSuccessToast "URL Monitor configured" Nothing
  redirectCS "/onboarding/notifications"
  addRespHeaders $ NoContent ""


-- | Notification Settings Handlers
onboardingNotificationsGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingNotificationsGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Notification Settings"}
  let notifications = fromMaybe def $ getNotifications sess.persistentSession
  addRespHeaders $ NotificationsR $ PageCtx bwconf (sess.persistentSession, appCtx.config, notifications)


-- onboardingNotificationsPostH :: NotificationSettings -> ATAuthCtx (RespHeaders OnboardingResponse)
-- onboardingNotificationsPostH settings = do
--   sess <- Sessions.getSession
--   appCtx <- ask @AuthContext
--   let sessionData = updateAndProgress sess.persistentSession storeNotifications settings
--   void $ Sessions.updateSession sess.persistentSession.id sess.persistentSession.userId sessionData
--   addSuccessToast "Notification settings saved" Nothing
--   redirectCS "/onboarding/notification-sent"
--   addRespHeaders $ NoContent ""

-- | Post handler for notification settings
onboardingNotificationsPostH :: NotificationSettings -> ATAuthCtx (RespHeaders OnboardingResponse)
onboardingNotificationsPostH settings = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext

  -- Handle Slack integrations
  forM_ settings.notificationChannels $ \channel -> case channel of
    SlackChannel{..} -> void $ dbtToEff $ insertAccessToken [channelName] webhookUrl
    _ -> pure ()

  -- Update session with notification settings
  let sessionData =
        updateAndProgress
          sess.persistentSession
          storeNotifications
          settings

  -- Store in session
  void
    $ Sessions.updateSession
      sess.persistentSession.id
      sess.persistentSession.userId
      sessionData

  -- Send test notifications
  sendTestNotifications settings

  -- Redirect with success message
  addSuccessToast "Notification settings saved" Nothing
  redirectCS "/onboarding/notification-sent"
  addRespHeaders $ NoContent ""


-- | Notification Sent Handlers
onboardingNotificationSentGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingNotificationSentGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Notification Sent"}
  addRespHeaders $ NotificationSentR $ PageCtx bwconf (sess.persistentSession, appCtx.config)


onboardingNotificationSentPostH :: NotificationConfirmation -> ATAuthCtx (RespHeaders OnboardingResponse)
onboardingNotificationSentPostH confirmation = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let sessionData = Sessions.SessionData $ Map.insert "notification_confirmed" (decodeUtf8 $ encode confirmation) (Sessions.getSessionData sess.persistentSession.sessionData)
  void $ Sessions.updateSession sess.persistentSession.id sess.persistentSession.userId sessionData
  addSuccessToast "Notifications confirmed" Nothing
  redirectCS "/onboarding/team"
  addRespHeaders $ NoContent ""


-- | Team Invitation Handlers
onboardingTeamGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingTeamGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Invite Team Members"}
  let team = fromMaybe def $ getTeam sess.persistentSession
  addRespHeaders $ TeamR $ PageCtx bwconf (sess.persistentSession, appCtx.config, team)


onboardingTeamPostH :: TeamInvitationList -> ATAuthCtx (RespHeaders OnboardingResponse)
onboardingTeamPostH invitations = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let sessionData = updateAndProgress sess.persistentSession storeTeam invitations
  void $ Sessions.updateSession sess.persistentSession.id sess.persistentSession.userId sessionData
  addSuccessToast "Team invitations saved" Nothing
  redirectCS "/onboarding/pricing"
  addRespHeaders $ NoContent ""


-- | Pricing Plan Handlers
onboardingPricingGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingPricingGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Choose Plan"}
  let pricing = fromMaybe def $ getPricing sess.persistentSession
  addRespHeaders $ PricingR $ PageCtx bwconf (sess.persistentSession, appCtx.config, pricing)


onboardingPricingPostH :: PricingPlan -> ATAuthCtx (RespHeaders OnboardingResponse)
onboardingPricingPostH plan = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let sessionData = updateAndProgress sess.persistentSession storePricing plan
  void $ Sessions.updateSession sess.persistentSession.id sess.persistentSession.userId sessionData
  addSuccessToast "Plan selected successfully" Nothing
  redirectCS "/onboarding/framework"
  addRespHeaders $ NoContent ""


-- | Check Inbox Handler
onboardingCheckInboxGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingCheckInboxGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Check Your Inbox"}
  addRespHeaders $ CheckInboxR $ PageCtx bwconf (sess.persistentSession, appCtx.config)


-- | Framework Integration Handler
onboardingFrameworkGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingFrameworkGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){pageTitle = "Framework Integration"}
  let framework = def :: FrameworkIntegration
  addRespHeaders $ FrameworkR $ PageCtx bwconf (sess.persistentSession, appCtx.config, framework)


-- Helper functions
validateProfileForm :: ProfileForm -> FormValidationResult ProfileForm
validateProfileForm form =
  let errors =
        catMaybes
          [ validateName "firstName" form.profileFirstName
          , validateName "lastName" form.profileLastName
          , validateCompanyName form.profileCompanyName
          , validateReferralSource form.profileReferralSource
          ]
   in if null errors then Valid form else Invalid errors


validateName :: Text -> Text -> Maybe FormError
validateName fieldName name
  | T.null (T.strip name) = Just $ FormError fieldName "Name cannot be empty"
  | T.length name > 50 = Just $ FormError fieldName "Name cannot be longer than 50 characters"
  | not (T.all (\c -> isLetter c || isSpace c) name) = Just $ FormError fieldName "Name can only contain letters and spaces"
  | otherwise = Nothing


validateCompanyName :: Text -> Maybe FormError
validateCompanyName name
  | T.null (T.strip name) = Just $ FormError "companyName" "Company name cannot be empty"
  | T.length name > 100 = Just $ FormError "companyName" "Company name cannot be longer than 100 characters"
  | T.all isSpace name = Just $ FormError "companyName" "Company name cannot be only whitespace"
  | otherwise = Nothing


validateReferralSource :: Text -> Maybe FormError
validateReferralSource source
  | T.null (T.strip source) = Just $ FormError "referralSource" "Please select how you heard about us"
  | source `notElem` validSources = Just $ FormError "referralSource" "Invalid referral source"
  | otherwise = Nothing
  where
    validSources = ["google", "social_media", "friend", "other"]


-- | Helper to send test notifications
sendTestNotifications :: NotificationSettings -> ATAuthCtx ()
sendTestNotifications NotificationSettings{..} = do
  -- Send email notifications
  forM_ notificationEmails $ \email ->
    sendTestEmail email

  -- Send SMS if phone number provided
  forM_ notificationPhone $ \phone ->
    forM_ notificationCountryCode $ \countryCode ->
      sendTestSMS countryCode phone

  -- Send channel notifications (Slack/Discord)
  forM_ notificationChannels $ \channel ->
    case channel of
      SlackChannel name url ->
        sendTestSlackMessage name url
      DiscordChannel name url ->
        sendTestDiscordMessage name url


-- | Helper to send test email
sendTestEmail :: Text -> ATAuthCtx ()
sendTestEmail email = do
  -- TODO: Implement email sending
  -- You can use your email service here
  pure ()


-- | Helper to send test SMS
sendTestSMS :: Text -> Text -> ATAuthCtx ()
sendTestSMS countryCode phone = do
  -- TODO: Implement SMS sending
  -- You can use your SMS service here
  pure ()


-- | Helper to send test Slack message
sendTestSlackMessage :: Text -> Text -> ATAuthCtx ()
sendTestSlackMessage channelName webhookUrl = do
  -- TODO: Implement Slack message sending
  -- You can use your Slack integration here
  pure ()


-- | Helper to send test Discord message
sendTestDiscordMessage :: Text -> Text -> ATAuthCtx ()
sendTestDiscordMessage channelName webhookUrl = do
  -- TODO: Implement Discord message sending
  -- You can use your Discord integration here
  pure ()


-- | Validation helper for notification settings
validateNotificationSettings :: NotificationSettings -> FormValidationResult NotificationSettings
validateNotificationSettings settings =
  let errors =
        catMaybes
          [ validateEmails settings.notificationEmails
          , validatePhone settings.notificationPhone
          , validateChannels settings.notificationChannels
          ]
   in if null errors then Valid settings else Invalid errors


validateEmails :: [Text] -> Maybe FormError
validateEmails emails
  | null emails = Just $ FormError "emails" "At least one email address is required"
  | any T.null emails = Just $ FormError "emails" "Email addresses cannot be empty"
  | otherwise = Nothing


validatePhone :: Maybe Text -> Maybe FormError
validatePhone Nothing = Nothing
validatePhone (Just phone)
  | T.null phone = Just $ FormError "phone" "Phone number cannot be empty if provided"
  | T.length phone < 10 = Just $ FormError "phone" "Phone number must be at least 10 digits"
  | not (T.all isNumber phone) = Just $ FormError "phone" "Phone number can only contain digits"
  | otherwise = Nothing


validateChannels :: [NotificationChannel] -> Maybe FormError
validateChannels channels
  | null channels = Nothing -- Channels are optional
  | otherwise = Nothing -- Add specific channel validation if needed
