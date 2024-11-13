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
) where

import BackgroundJobs (BgJobs (..))
import BackgroundJobs qualified
import Control.Lens ((^.))
import Data.Aeson (encode)
import Data.Base64.Types qualified as B64
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isLetter, isSpace)
import Data.Default (Default (..), def)
import Data.Map.Strict qualified as Map
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
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
import PyF
import Relude hiding (ask, asks)
import System.Config
import System.Types
import Utils (faSprite_, isDemoAndNotSudo)
import Web.FormUrlEncoded (FromForm (..), ToForm (..), lookupUnique)


-- | Initial handler
onboardingSignupGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingSignupGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf =
        (def :: BWConfig)
          { pageTitle = "Signup"
          }
  addRespHeaders $ SignupR $ PageCtx bwconf (sess.persistentSession, appCtx.config, def, Valid def)


-- -- | Login Handlers
onboardingLoginGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingLoginGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let bwconf =
        (def :: BWConfig)
          { pageTitle = "Login"
          }
  addRespHeaders $ LoginR $ PageCtx bwconf (sess.persistentSession, appCtx.config, def, Valid def)


-- | Profile Handlers
onboardingProfileGetH :: ATAuthCtx (RespHeaders OnboardingResponse)
onboardingProfileGetH = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  let form = def :: ProfileForm
  let bwconf = (def :: BWConfig){pageTitle = "Complete Profile"}
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
  traceShowM framework
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

-- | Login Handler
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

-- | Profile Handler
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


-- | Hosting Location Handler
onboardingHostingPostH :: HostingLocation -> ATAuthCtx (RespHeaders OnboardingResponse)
onboardingHostingPostH location = do
  traceShowM location
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  -- Convert HostingLocation to session data
  let locationKey = "hosting_location"
      locationValue = decodeUtf8 $ encode location -- Store the actual HostingLocation value
      updatedSessionData =
        Sessions.SessionData
          $ Map.insert locationKey locationValue
          $ Sessions.getSessionData sess.persistentSession.sessionData

  -- Update session with new hosting location
  void
    $ Sessions.updateSession
      sess.persistentSession.id
      sess.persistentSession.userId
      updatedSessionData

  -- Add success notification and redirect
  addSuccessToast "Hosting location selected" Nothing
  redirectCS "/onboarding/usage"
  addRespHeaders $ NoContent ""


-- | Usage Preferences Handler
onboardingUsagePostH :: UsagePreferences -> ATAuthCtx (RespHeaders OnboardingResponse)
onboardingUsagePostH usagePrefs = do
  traceShowM usagePrefs
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext

  -- Store usage preferences in session
  let usageKey = "usage_preferences"
      usageValue = decodeUtf8 (encode usagePrefs)
      updatedSessionData =
        Sessions.SessionData
          $ Map.insert usageKey usageValue
          $ Sessions.getSessionData sess.persistentSession.sessionData

  -- Update session with usage preferences
  void
    $ Sessions.updateSession
      sess.persistentSession.id
      sess.persistentSession.userId
      updatedSessionData

  -- Add success notification and redirect
  addSuccessToast "Usage preferences saved" Nothing
  redirectCS "/onboarding/url-monitor"
  addRespHeaders $ NoContent ""


-- | URL Monitor Handler
onboardingUrlMonitorPostH :: URLMonitorConfig -> ATAuthCtx (RespHeaders OnboardingResponse)
onboardingUrlMonitorPostH config = do
  sess <- Sessions.getSession
  -- todo
  addSuccessToast "URL Monitor configured" Nothing
  redirectCS "/onboarding/notifications"
  addRespHeaders $ NoContent ""


-- | Notification Settings Handler
-- onboardingNotificationsPostH :: NotificationSettings -> ATAuthCtx (RespHeaders OnboardingResponse)
-- onboardingNotificationsPostH settings = do
--   sess <- Sessions.getSession
--   appCtx <- ask @AuthContext

--   -- Process notification channels (Slack, Discord)
--   forM_ settings.notificationChannels \case {}

-- dbtToEff $ updateOnboardingNotifications sess.user.id settings
-- addSuccessToast "Notification settings saved" Nothing
-- redirectCS "/onboarding/team"
-- addRespHeaders $ NoContent ""

-- | Notification Settings Handler
onboardingNotificationsPostH :: NotificationSettings -> ATAuthCtx (RespHeaders OnboardingResponse)
onboardingNotificationsPostH settings = do
  traceShowM settings
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext

  --   -- Validate notification settings
  --   case validateNotificationSettings settings of
  --     Invalid errors -> do
  --       addErrorToast "Invalid notification settings" Nothing
  --       addRespHeaders $ NotificationsR $ PageCtx def (sess.persistentSession, appCtx.config, settings)

  --     Valid validSettings -> do
  --       -- Process and store notification channels
  --       processResult <- processNotificationChannels sess.user.id validSettings.notificationChannels
  --       case processResult of
  --         Left error -> do
  --           addErrorToast ("Failed to configure channels: " <> error) Nothing
  --           addRespHeaders $ NotificationsR $ PageCtx def (sess.persistentSession, appCtx.config, settings)

  --         Right _ -> do
  --           -- Store notification settings in session
  --           let notificationKey = "notification_settings"
  --               notificationValue = decodeUtf8 $ encode settings
  --               updatedSessionData = Sessions.SessionData $
  --                 Map.insert notificationKey notificationValue $
  --                 Sessions.getSessionData sess.persistentSession.sessionData

  --           -- Update session with new notification settings
  --           void $ Sessions.insertSession
  --             sess.persistentSession.id
  --             sess.persistentSession.userId
  --             updatedSessionData

  --           -- Set up test notifications for all configured channels
  --           whenJustM (dbtToEff $ Projects.projectById sess.project.id) \project -> do
  --             let testMsg = [fmtTrim|🤖 Test notification from APIToolkit for {project.title}|]

  --             -- Process email notifications
  --             forM_ settings.notificationEmails \email ->
  --               liftIO $ withResource appCtx.jobsPool \conn ->
  --                 createJob conn "background_jobs" $
  --                   InviteUserToProject
  --                     sess.user.id
  --                     sess.project.id
  --                     email
  --                     "Test Notification"

  --             -- Process Slack/Discord notifications
  --             forM_ settings.notificationChannels \case
  --               SlackChannel _ webhookUrl ->
  --                 liftIO $ withResource appCtx.jobsPool \conn ->
  --                   createJob conn "background_jobs" $
  --                     SendDiscordData
  --                       sess.user.id
  --                       sess.project.id
  --                       project.title
  --                       ["Test notification"]
  --                       webhookUrl
  --               DiscordChannel _ webhookUrl ->
  --                 liftIO $ withResource appCtx.jobsPool \conn ->
  --                   createJob conn "background_jobs" $
  --                     SendDiscordData
  --                       sess.user.id
  --                       sess.project.id
  --                       project.title
  --                       ["Test notification"]
  --                       webhookUrl

  -- Add success notification and redirect
  addSuccessToast "Notification settings saved and test notifications queued" Nothing
  redirectCS "/onboarding/notification-sent"
  addRespHeaders $ NoContent ""


-- -- | Helper function to validate notification settings
-- validateNotificationSettings :: NotificationSettings -> FormValidationResult NotificationSettings
-- validateNotificationSettings settings = do
--   let emailErrors = mapMaybe validateEmail settings.notificationEmails
--       phoneErrors = maybeToList $ (validatePhoneNumber =<< settings.notificationPhone)
--       channelErrors = mapMaybe validateChannel settings.notificationChannels

--       allErrors = emailErrors ++ phoneErrors ++ channelErrors

--   if null allErrors
--     then Valid settings
--     else Invalid allErrors

-- -- | Helper function to validate a notification channel
-- validateChannel :: NotificationChannel -> Maybe FormError
-- validateChannel channel =
--   if isValidWebhookUrl (getWebhookUrl channel)
--     then Nothing
--     else Just $ FormError "webhook" "Invalid webhook URL format"

-- -- | Helper function to check if a webhook URL is valid
-- isValidWebhookUrl :: Text -> Bool
-- isValidWebhookUrl url =
--   let urlText = T.toLower url
--   in T.isPrefixOf "https://" urlText &&
--      (T.isInfixOf "slack.com/services/" urlText ||
--       T.isInfixOf "discord.com/api/webhooks/" urlText)

-- -- | Helper function to process notification channels
-- processNotificationChannels :: Users.UserId -> [NotificationChannel] -> ATAuthCtx (Either Text ())
-- processNotificationChannels userId channels = do
--   results <- forM channels $ \channel -> do
--     -- Validate webhook URL
--     if isValidWebhookUrl (getWebhookUrl channel)
--       then do
--         -- Store channel configuration in database
--         dbtToEff $ storeNotificationChannel userId channel
--         pure $ Right ()
--       else pure $ Left $ "Invalid webhook URL for " <> getChannelName channel

--   -- Combine results, returning first error if any
--   pure $ sequence_ results

-- -- | Helper functions to extract channel information
-- getWebhookUrl :: NotificationChannel -> Text
-- getWebhookUrl (SlackChannel _ url) = url
-- getWebhookUrl (DiscordChannel _ url) = url

-- getChannelName :: NotificationChannel -> Text
-- getChannelName (SlackChannel name _) = name
-- getChannelName (DiscordChannel name _) = name

-- -- | Database operation to store notification channel
-- storeNotificationChannel :: Users.UserId -> NotificationChannel -> DBT IO ()
-- storeNotificationChannel userId channel = do
--   -- TODO: Implement actual database storage
--   -- This is a placeholder for the actual database operation
--   pass

-- | Team Invitation Handler
onboardingTeamPostH :: TeamInvitationList -> ATAuthCtx (RespHeaders OnboardingResponse)
onboardingTeamPostH invitation = do
  -- traceShowM invitation
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  -- Process team invitations
  -- forM_ invitation.teamMembers \TeamMember{memberEmail, memberRole} -> do
  --   userIdM <- dbtToEff $ Users.userIdByEmail memberEmail
  --   userId <- case userIdM of
  --     Just uid -> pure  $ Just uid
  --     Nothing -> dbtToEff $ Users.createEmptyUser memberEmail
  --   -- Create background job for sending invitation
  --   liftIO $ withResource appCtx.pool \conn ->
  --     createJob conn "background_jobs" $
  --       BackgroundJobs.InviteUserToProject
  --         sess.user.id
  --         (Projects.ProjectId "temp") -- You'll need the actual project ID
  --         memberEmail
  --         "New Project" -- You'll need the actual project title
  -- _ <- dbtToEff $ updateOnboardingTeam sess.user.id invitation
  addSuccessToast "Team invitations sent" Nothing
  redirectCS "/onboarding/pricing"
  addRespHeaders $ NoContent ""


-- | Pricing Plan Handler
onboardingPricingPostH :: PricingPlan -> ATAuthCtx (RespHeaders OnboardingResponse)
onboardingPricingPostH plan = do
  traceShowM plan
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext

  -- Store pricing plan in session
  let pricingKey = "pricing_plan"
      pricingValue = decodeUtf8 (encode plan)
      updatedSessionData =
        Sessions.SessionData
          $ Map.insert pricingKey pricingValue
          $ Sessions.getSessionData sess.persistentSession.sessionData

  -- Update session with pricing plan
  void
    $ Sessions.updateSession
      sess.persistentSession.id
      sess.persistentSession.userId
      updatedSessionData

  -- Add success notification and redirect
  addSuccessToast "Plan selected successfully" Nothing -- Fixed the toast message
  redirectCS "/onboarding/framework"
  addRespHeaders $ NoContent ""


-- onboardingFrameworkPostH :: FrameworkIntegration -> ATAuthCtx (RespHeaders OnboardingResponse)
-- onboardingFrameworkPostH framework = do
--   traceShowM framework
--   sess <- Sessions.getSession
--   appCtx <- ask @AuthContext

--   -- Store framework selection in session
--   let frameworkKey = "framework_integration"
--       frameworkValue = decodeUtf8 (encode framework)
--       updatedSessionData =
--         Sessions.SessionData
--           $ Map.insert frameworkKey frameworkValue
--           $ Sessions.getSessionData sess.persistentSession.sessionData

--   -- Update session with framework selection
--   void $ Sessions.updateSession
--     sess.persistentSession.id
--     sess.persistentSession.userId
--     updatedSessionData

--   -- Add success notification and redirect
--   addSuccessToast "Framework integration saved" Nothing
--   redirectCS "/onboarding/signup"  -- or wherever you want to redirect after framework selection
--   addRespHeaders $ NoContent ""

-- | Database operations
-- saveOnboardingState :: Users.UserId -> OnboardingState -> DBT IO ()
-- saveOnboardingState userId state = pass -- TODO: Implement

-- getOnboardingState :: Users.UserId -> DBT IO (Maybe OnboardingState)
-- getOnboardingState userId = pass -- TODO: Implement

updateOnboardingProfile :: Users.UserId -> ProfileForm -> DBT IO ()
updateOnboardingProfile userId form = pass -- TODO: Implement


-- | Helper functions
validateSignupForm :: SignupForm -> FormValidationResult SignupForm
validateSignupForm form =
  case (validateEmail form.signupEmail, validatePassword form.signupPassword) of
    (Nothing, Nothing) -> Valid form
    (emailErr, passErr) -> Invalid $ catMaybes [emailErr, passErr]


validateProfileForm :: ProfileForm -> FormValidationResult ProfileForm
validateProfileForm form =
  let errors =
        catMaybes
          [ validateName "firstName" form.profileFirstName
          , validateName "lastName" form.profileLastName
          , validateCompanyName form.profileCompanyName
          , validateReferralSource form.profileReferralSource
          ]
   in if null errors
        then Valid form
        else Invalid errors


-- | Validate a name field (first name or last name)
validateName :: Text -> Text -> Maybe FormError
validateName fieldName name
  | T.null (T.strip name) = Just $ FormError fieldName "Name cannot be empty"
  | T.length name > 50 = Just $ FormError fieldName "Name cannot be longer than 50 characters"
  | not (T.all (\c -> isLetter c || isSpace c) name) = Just $ FormError fieldName "Name can only contain letters and spaces"
  | otherwise = Nothing


-- | Validate company name
validateCompanyName :: Text -> Maybe FormError
validateCompanyName name
  | T.null (T.strip name) = Just $ FormError "companyName" "Company name cannot be empty"
  | T.length name > 100 = Just $ FormError "companyName" "Company name cannot be longer than 100 characters"
  | T.all isSpace name = Just $ FormError "companyName" "Company name cannot be only whitespace"
  | otherwise = Nothing


-- | Parse company size from text (e.g., "50 employees" -> Just 50)
parseCompanySize :: Text -> Maybe Int
parseCompanySize text = do
  let parts = words $ T.toLower text
  case parts of
    (n : _) -> readMaybe (toString n)
    _ -> Nothing


-- | Validate referral source
validateReferralSource :: Text -> Maybe FormError
validateReferralSource source
  | T.null (T.strip source) = Just $ FormError "referralSource" "Please select how you heard about us"
  | source `notElem` validSources = Just $ FormError "referralSource" "Invalid referral source"
  | otherwise = Nothing
  where
    validSources = ["google", "social_media", "friend", "other"]
