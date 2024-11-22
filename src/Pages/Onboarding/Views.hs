module Pages.Onboarding.Views (
  OnboardingResponse (..),
  renderSignupPage,
  renderLoginForm,
  renderUserFormPage,
  renderNotificationSettingsFormPage,
  renderUrlMonitorFormPage,
  renderPricingPlanPage,
  renderTeamInvitePage,
  renderCheckInboxPage,
  renderDataLocationSelectPage,
  renderNotificationSentPage,
  renderFrameworkIntegrationPage,
) where

import BackgroundJobs qualified
import Control.Lens ((^.))
import Data.Aeson qualified as AE
import Data.Base64.Types qualified as B64
import Data.ByteString.Base64 qualified as B64
import Data.Char (toUpper)
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
import Lucid
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx (hxDelete_, hxGet_, hxInclude_, hxPost_, hxPut_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import OddJobs.Job (createJob)
import Pages.BodyWrapper
import Pages.Onboarding.Components
import Pages.Onboarding.Helpers
import Pages.Onboarding.Types
import Relude hiding (ask, asks)
import System.Config
import System.Types
import Utils (faSprite_, isDemoAndNotSudo)
import Web.FormUrlEncoded (FromForm)


-- | Core response types
data OnboardingResponse
  = SignupR (PageCtx (Sessions.PersistentSession, EnvConfig, SignupForm, FormValidationResult SignupForm))
  | LoginR (PageCtx (Sessions.PersistentSession, EnvConfig, LoginForm, FormValidationResult LoginForm))
  | ProfileR (PageCtx (Sessions.PersistentSession, EnvConfig, ProfileForm, FormValidationResult ProfileForm))
  | HostingR (PageCtx (Sessions.PersistentSession, EnvConfig, HostingLocation))
  | UsageR (PageCtx (Sessions.PersistentSession, EnvConfig, UsagePreferences))
  | URLMonitorR (PageCtx (Sessions.PersistentSession, EnvConfig, URLMonitorConfig))
  | NotificationsR (PageCtx (Sessions.PersistentSession, EnvConfig, NotificationSettings))
  | TeamR (PageCtx (Sessions.PersistentSession, EnvConfig, TeamInvitationList))
  | PricingR (PageCtx (Sessions.PersistentSession, EnvConfig, PricingPlan))
  | CheckInboxR (PageCtx (Sessions.PersistentSession, EnvConfig))
  | FrameworkR (PageCtx (Sessions.PersistentSession, EnvConfig, FrameworkIntegration))
  | NotificationSentR (PageCtx (Sessions.PersistentSession, EnvConfig))
  | NoContent Text
  deriving stock (Show)


-- | Basic ToHtml instance just to get things working
instance ToHtml OnboardingResponse where
  toHtml (SignupR (PageCtx bwconf (sess, cfg, form, validation))) =
    toHtml $ PageCtx bwconf $ renderSignupPage form validation
  toHtml (LoginR (PageCtx bwconf (sess, cfg, form, validation))) =
    toHtml $ PageCtx bwconf $ renderLoginForm form validation
  toHtml (ProfileR (PageCtx bwconf (sess, cfg, form, validation))) =
    toHtml $ PageCtx bwconf $ renderUserFormPage form validation
  toHtml (HostingR (PageCtx bwconf (sess, cfg, location))) =
    toHtml $ PageCtx bwconf $ renderDataLocationSelectPage location
  toHtml (UsageR (PageCtx bwconf (sess, cfg, usage))) =
    toHtml $ PageCtx bwconf $ renderUsageSelectionForm usage
  toHtml (URLMonitorR (PageCtx bwconf (sess, cfg, monitor))) =
    toHtml $ PageCtx bwconf renderUrlMonitorFormPage
  toHtml (NotificationsR (PageCtx bwconf (sess, cfg, notifications))) =
    toHtml $ PageCtx bwconf $ renderNotificationSettingsFormPage notifications
  toHtml (TeamR (PageCtx bwconf (sess, cfg, team))) =
    toHtml $ PageCtx bwconf $ renderTeamInvitePage team
  toHtml (PricingR (PageCtx bwconf (sess, cfg, pricing))) =
    toHtml $ PageCtx bwconf $ renderPricingPlanPage pricing
  toHtml (CheckInboxR (PageCtx bwconf (sess, cfg))) =
    toHtml $ PageCtx bwconf renderCheckInboxPage
  toHtml (FrameworkR (PageCtx bwconf (sess, cfg, framework))) =
    toHtml $ PageCtx bwconf $ renderFrameworkIntegrationPage framework
  toHtml (NotificationSentR (PageCtx bwconf (sess, cfg))) =
    toHtml $ PageCtx bwconf renderNotificationSentPage
  toHtml (NoContent msg) =
    div_ [] $ toHtml msg
  toHtmlRaw = toHtml


renderSignupPage :: SignupForm -> FormValidationResult SignupForm -> Html ()
renderSignupPage form validation =
  div_ [class_ "max-w-md space-y-8 mx-auto pt-32"] $ do
    -- Logo Position
    div_ [class_ "text-3xl"]
      $ img_ [src_ "/public/assets/svgs/logo_min.svg", alt_ ""]

    -- Header
    div_ [class_ "text-start"] $ do
      h2_ [class_ "text-3xl font-semibold text-slate-900"] $ do
        "Create a \nfree"
        br_ []
        "APIToolkit account"
      p_ [class_ "mt-2 text-sm text-slate-600"] $ do
        "Already have an account? "
        a_ [href_ "/onboarding/login", class_ "text-blue-600 hover:text-blue-500 underline"] "Login"

    -- Form
    form_ [class_ "mt-8 space-y-6"] $ do
      div_ [class_ "space-y-4"] $ do
        -- Email field
        div_ [] $ do
          label_ [class_ "block text-sm font-medium text-slate-700"] "Email Address"
          div_ [class_ "mt-1 relative"] $ do
            div_ [class_ "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none"] $ do
              faSprite_ "mail-envelope" "regular" "h-5 w-5 text-slate-500"
            input_
              [ type_ "email"
              , name_ "email"
              , placeholder_ "hello.john@email.com"
              , class_ "pl-10 w-full px-3 py-3  bg-slate-50 text-slate-500 text-sm font-normal font-['Inter'] leading-snug border border-slate-300 rounded-xl shadow-sm placeholder-slate-400 focus:outline-none focus:ring-blue-500 focus:border-blue-500"
              ]

        -- Password field
        div_ [] $ do
          div_ [class_ "flex items-center justify-start"] $ do
            label_ [class_ "block text-sm font-medium text-slate-700"] "Password"

          div_ [class_ "mt-1 relative"] $ do
            div_ [class_ "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none"] $ do
              faSprite_ "lock-unlocked" "regular" "h-5 w-5 text-slate-500"

            input_
              [ type_ "password"
              , name_ "password"
              , placeholder_ "Enter 8 digit password"
              , class_ "pl-10 w-full px-3 py-3  bg-slate-50 text-slate-500 text-sm font-normal font-['Inter'] leading-snug border border-slate-300 rounded-xl shadow-sm placeholder-slate-400 focus:outline-none focus:ring-blue-500 focus:border-blue-500"
              ]

            button_
              [ type_ "button"
              , class_ "absolute inset-y-0 right-0 pr-3 flex items-center"
              , hxTrigger_ "click"
              , hxSwap_ "outerHTML"
              ]
              $ do
                faSprite_ "eye-view" "regular" "h-5 w-5 text-slate-500"

        -- Password Requirements
        div_ [class_ "flex flex-wrap gap-2 text-sm text-slate-600"] $ do
          span_ [class_ "px-3 py-1 rounded-3xl border border-slate-300 justify-center items-center gap-2.5 text-slate-500 text-sm font-normal"] "8 characters"
          span_ [class_ "px-3 py-1 rounded-3xl border border-slate-300 justify-center items-center gap-2.5 text-slate-500 text-sm font-normal"] "Number"
          span_ [class_ "px-3 py-1 rounded-3xl border border-slate-300 justify-center items-center gap-2.5 text-slate-500 text-sm font-normal"] "Lowercase"
          span_ [class_ "px-3 py-1 rounded-3xl border border-slate-300 justify-center items-center gap-2.5 text-slate-500 text-sm font-normal"] "Uppercase"
          span_ [class_ "px-3 py-1 rounded-3xl border border-slate-300 justify-center items-center gap-2.5 text-slate-500 text-sm font-normal"] "1 special character"

      -- New Account Button
      renderPrimaryButton POST "" "Create free account"

      -- Divider
      div_ [class_ "relative w-full py-1"] $ do
        div_ [class_ "absolute inset-0 flex items-center"]
          $ div_ [class_ "w-full border-t border-slate-300"] mempty
        div_ [class_ "relative flex justify-center"]
          $ span_ [class_ "bg-white px-2 py-2 bg-slate-50 text-sm text-slate-500 rounded-full border border-slate-300"] "OR"

      -- Social Login Buttons
      div_ [class_ "space-y-3"] $ do
        -- Google Login Button
        button_
          [ type_ "button"
          , class_ "w-full py-2 px-4  py-3 border border-slate-300 shadow-sm bg-slate-50 rounded-2xl text-sm font-medium text-slate-700 hover:bg-slate-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 flex items-center justify-center space-x-2"
          ]
          $ do
            img_ [src_ "/public/assets/svgs/onboarding/google.svg", alt_ "Google", class_ "w-5 h-5"]
            span_ "Continue with Google"

        -- GitHub Login Button
        button_
          [ type_ "button"
          , class_ "w-full py-2 px-4 py-3  border border-slate-300 shadow-sm bg-slate-50 rounded-2xl text-sm font-medium text-slate-700 hover:bg-slate-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 flex items-center justify-center space-x-2"
          ]
          $ do
            img_ [src_ "/public/assets/svgs/onboarding/github.svg", alt_ "Github", class_ "w-5 h-5"]
            span_ "Continue with GitHub"


renderLoginForm :: LoginForm -> FormValidationResult LoginForm -> Html ()
renderLoginForm form validation = do
  div_ [class_ "w-full max-w-md space-y-8 mx-auto pt-32"] $ do
    -- Logo and welcome section
    div_ [class_ "text-start"] $ do
      div_ [class_ "text-3xl mb-4"]
        $ img_ [src_ "/public/assets/svgs/logo_min.svg", alt_ ""]

      h2_ [class_ "text-3xl font-semibold text-slate-900"] $ do
        "Welcome back"
        br_ []
        "APIToolkit"

      p_ [class_ "mt-2 text-sm text-slate-600"] $ do
        "Don't have an account? "
        a_ [href_ "/onboarding/signup", class_ "text-blue-600 hover:text-blue-500 underline"] "Sign up for free"

    -- Form
    form_
      [ class_ "mt-8 space-y-6"
      , hxPost_ "/api/auth/login"
      , hxSwap_ "outerHTML"
      , hxTarget_ "#login-form"
      ]
      $ do
        -- Email field
        div_ [] $ do
          label_ [class_ "block text-sm font-medium text-slate-700"] "Email Address"
          div_ [class_ "mt-1 relative"] $ do
            div_ [class_ "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none"] $ do
              faSprite_ "mail-envelope" "regular" "h-5 w-5 text-slate-500"
            input_
              [ type_ "email"
              , name_ "email"
              , placeholder_ "hello.john@email.com"
              , class_ "pl-10 w-full px-3 py-3  bg-slate-50 text-slate-500 text-sm font-normal font-['Inter'] leading-snug border border-slate-300 rounded-xl shadow-sm placeholder-slate-400 focus:outline-none focus:ring-blue-500 focus:border-blue-500"
              ]

        -- Password field
        div_ [] $ do
          div_ [class_ "flex items-center justify-start"] $ do
            label_ [class_ "block text-sm font-medium text-slate-700"] "Password"

          div_ [class_ "mt-1 relative"] $ do
            div_ [class_ "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none"] $ do
              faSprite_ "lock-unlocked" "regular" "h-5 w-5 text-slate-500"

            input_
              [ type_ "password"
              , name_ "password"
              , placeholder_ "Enter 8 digit password"
              , class_ "pl-10 w-full px-3 py-3  bg-slate-50 text-slate-500 text-sm font-normal font-['Inter'] leading-snug border border-slate-300 rounded-xl shadow-sm placeholder-slate-400 focus:outline-none focus:ring-blue-500 focus:border-blue-500"
              ]

            button_
              [ type_ "button"
              , class_ "absolute inset-y-0 right-0 pr-3 flex items-center"
              , hxTrigger_ "click"
              , hxSwap_ "outerHTML"
              ]
              $ do
                faSprite_ "eye-view" "regular" "h-5 w-5 text-slate-500"

          div_ [class_ "flex items-center justify-end pt-2"] $ do
            a_ [href_ "#", class_ "text-sm text-slate-600 hover:text-slate-500 underline"] "Forgot Password"

        -- Sign In button
        renderPrimaryButton POST "" "Sign In"

        -- Divider
        div_ [class_ "relative w-full py-1"] $ do
          div_ [class_ "absolute inset-0 flex items-center"]
            $ div_ [class_ "w-full border-t border-slate-300"] mempty
          div_ [class_ "relative flex justify-center"]
            $ span_ [class_ "bg-white px-2 py-2 bg-slate-50 text-sm text-slate-500 rounded-full border border-slate-300"] "OR"

        -- Social login buttons
        div_ [class_ "space-y-3"] $ do
          -- Google Login Button
          button_
            [ type_ "button"
            , class_ "w-full py-2 px-4  py-3 border border-slate-300 shadow-sm bg-slate-50 rounded-2xl text-sm font-medium text-slate-700 hover:bg-slate-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 flex items-center justify-center space-x-2"
            ]
            $ do
              img_ [src_ "/public/assets/svgs/onboarding/google.svg", alt_ "Google", class_ "w-5 h-5"]
              span_ "Continue with Google"

          -- GitHub Login Button
          button_
            [ type_ "button"
            , class_ "w-full py-2 px-4 py-3  border border-slate-300 shadow-sm bg-slate-50 rounded-2xl text-sm font-medium text-slate-700 hover:bg-slate-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 flex items-center justify-center space-x-2"
            ]
            $ do
              img_ [src_ "/public/assets/svgs/onboarding/github.svg", alt_ "Github", class_ "w-5 h-5"]
              span_ "Continue with GitHub"


renderUserFormPage :: ProfileForm -> FormValidationResult ProfileForm -> Html ()
renderUserFormPage form validation = do
  renderOnboardingWrapper Nothing $ do
    div_ [class_ "space-y-8"] $ do
      renderFormQuestion "Tell us a little bit" (Just "about you") Nothing

      form_
        [ class_ "space-y-4"
        , hxPost_ "/onboarding/profile"
        , hxSwap_ "outerHTML"
        , hxTrigger_ "submit"
        ]
        $ do
          div_ [class_ "grid grid-cols-2 gap-4"] $ do
            renderInput
              "First Name"
              "profileFirstName"
              "e.g Mike"
              ""
              False

            renderInput
              "Last"
              "profileLastName"
              "E.g Abel"
              ""
              False

          renderInput
            "Company Name"
            "profileCompanyName"
            "Apple, Inc"
            "user-circle"
            False

          renderSelect
            "Company Size"
            "profileCompanySize"
            [ ("1-2", "1-2 employees")
            , ("2-10", "2-10 employees")
            , ("10-50", "10-50 employees")
            , ("50+", "50+ employees")
            ]

          renderSelect
            "Where did you hear about us?"
            "profileReferralSource"
            [ ("", "Select an option")
            , ("google", "Google")
            , ("linked_in", "LinkedIn")
            , ("twitter", "Twitter/X")
            , ("friend", "Friend/Colleague")
            , ("others", "Others")
            ]

          renderPrimaryButton POST "" "Proceed"


renderNotificationSettingsFormPage :: NotificationSettings -> Html ()
renderNotificationSettingsFormPage settings =
  renderOnboardingWrapper Nothing $ do
    -- Add Tagify CSS from CDN
    link_ [rel_ "stylesheet", type_ "text/css", href_ "https://unpkg.com/@yaireo/tagify/dist/tagify.css"]

    -- custom Tagify styles
    style_ [type_ "text/css"] $ toHtmlRaw customTagifyStyles

    div_ [class_ "space-y-8"] $ do
      renderFormQuestion "How should we let you" (Just "know when something") (Just "goes wrong?")

      form_
        [ method_ "POST"
        , action_ ""
        , id_ "notification-settings"
        , onsubmit_ "return handleSubmit(event)"
        ]
        $ do
          div_ [class_ "space-y-4"]
            $ do
              section_ [] $ do
                h2_ [id_ "slack-section-title", class_ "sr-only"] "Slack Connections"
                div_ [class_ "space-y-4"] $ do
                  -- Each Row
                  div_ [class_ "justify-between grid grid-cols-2 gap-3"] $ do
                    connectionBox_ False "slack" "workspace-1"
                    connectionBox_ True "slack" "workspace-2"

                  -- Each Row
                  div_ [class_ "justify-between grid grid-cols-2 gap-3"] $ do
                    connectionBox_ False "slack" "workspace-3"
                    connectionBox_ True "slack" "workspace-4"

              -- Phone Number Input Section
              section_ [] $ div_ [class_ "space-y-2"] $ do
                label_
                  [ id_ "phone-section-title"
                  , class_ "text-[#475467] text-sm font-medium font-['Inter'] leading-snug"
                  ]
                  "Notify phone number"
                -- div_ [class_ "relative"] $ do
                div_ [class_ "flex items-center w-full p-2 border px-3 py-2 rounded-xl"] $ do
                  div_ [class_ "w-1/6 flex items-center shrink-0 gap-1"] $ do
                    countrySelect fullCountryCodes
                  span_ [class_ "text-slate-400 mx-4"] "("
                  input_
                    [ type_ "tel"
                    , id_ "phone-input"
                    , name_ "phone"
                    , value_ (fromMaybe "" settings.notificationPhone)
                    , class_ "flex-1 outline-none text-slate-700 bg-slate-50"
                    , pattern_ "[0-9]{10}"
                    , placeholder_ "Enter phone number"
                    , required_ ""
                    ]

              -- Email notifications section
              section_ [] $ div_ [class_ "space-y-2 h-32"] $ do
                label_
                  [ id_ "email-section-title"
                  , class_ "block text-md text-slate-600"
                  ]
                  "Notify the following email addresses"
                input_
                  [ type_ "text"
                  , id_ "email-tags"
                  , name_ "email-tags"
                  , class_ "tagify-email w-full flex flex-wrap mb-4 bg-slate-50 flex rounded-xl"
                  , placeholder_ "Add email addresses..."
                  , value_ (T.intercalate "," settings.notificationEmails)
                  ]

          div_ [class_ ""] do renderPrimaryButton POST "" "Send test notifications"
    -- Add Tagify script from CDN
    script_ [src_ "https://unpkg.com/@yaireo/tagify"] ("" :: Text)

    -- Initialize Tagify
    script_ [] $ toHtmlRaw emailTagsScript

    script_
      [type_ "text/javascript"]
      [text|
        function handleSubmit(event) {
          event.preventDefault();
          
          const form = event.target;
          const input = document.getElementById('email-tags');
          
          // Get emails directly as a comma-separated list
          const tagify = input.tagify;
          if (!tagify || !tagify.value) {
            console.error('Tagify not initialized properly');
            return false;
          }
          
          // Extract just the email values 
          const emailValues = tagify.value.map(tag => tag.value);
          
          // Create hidden input with plain emails
          const emailInput = document.createElement('input');
          emailInput.type = 'hidden';
          emailInput.name = 'email-tags'; // Match the field name in Haskell FromForm instance
          emailInput.value = emailValues.join(',');
          
          // Remove any existing input to avoid duplicates
          const existing = form.querySelector('input[name="email-tags"]');
          if (existing) {
            existing.remove();
          }
          
          form.appendChild(emailInput);
          form.submit();
          return false;
        }

        // Connect handler to the form
        document.addEventListener('DOMContentLoaded', function() {
          const form = document.getElementById('notification-settings');
          if (form) {
            form.addEventListener('submit', handleSubmit);
          }
        });
      |]

    script_
      [type_ "text/javascript"]
      [text|
        document.addEventListener('DOMContentLoaded', function() {
        const select = document.querySelector('select');
    
          select.addEventListener('change', function() {
              if (this.value) {
                  this.options[this.selectedIndex].textContent = this.value;
              }
          });

          select.addEventListener('focus', function() {
              const selectedOption = this.options[this.selectedIndex];
              if (this.value) {
                  selectedOption.textContent = selectedOption.dataset.full;
              }
          });

          select.addEventListener('blur', function() {
              if (this.value) {
                  this.options[this.selectedIndex].textContent = this.value;
              }
          });
      });
      |]
  where
    connectionBox_ :: Bool -> Text -> Text -> Html ()
    connectionBox_ isConnected serviceName workspaceId =
      div_ [class_ "flex-1 px-3 py-2 bg-slate-50 rounded-xl border border-slate-300 justify-between items-center inline-flex"] $ do
        div_ [class_ "flex-1 flex"]
          $ div_ [class_ "flex items-center gap-1"]
          $ do
            img_
              [ src_ "/public/assets/svgs/onboarding/slack.svg"
              , alt_ (serviceName <> " icon")
              , class_ "w-16 h-4"
              ]
        div_ [class_ "flex-1 flex"] ""
        div_ [class_ "flex-1 flex"]
          $ if isConnected
            then
              button_
                [ type_ "button"
                , disabled_ ""
                , class_ "px-2 py-1 text-white text-sm font-medium bg-green-500 rounded-lg"
                ]
                "Connected"
            else
              button_
                [ type_ "button"
                , class_ "px-2 py-0 text-blue-600 text-sm font-medium border border-blue-600 rounded-lg hover:bg-blue-50"
                ]
                "Connect"


renderUsageSelectionForm :: UsagePreferences -> Html ()
renderUsageSelectionForm prefs = do
  renderOnboardingWrapper Nothing $ do
    div_ [class_ "space-y-8"] $ do
      renderFormQuestion "How would you like" (Just "to use APIToolkit") Nothing

      form_
        [ method_ "POST"
        , enctype_ "application/x-www-form-urlencoded"
        , hxPost_ "/onboarding/usage"
        , hxSwap_ "outerHTML"
        , hxTrigger_ "submit"
        ]
        $ do
          div_ [class_ "grid grid-cols-2 gap-4"] $ do
            renderOptionBox
              "uptimeMonitoring"
              "Uptime monitoring"
              "Check if your website works correctly"
              (uptimeMonitoring prefs)
            renderOptionBox
              "errorTracking"
              "Error Tracking"
              "Track and analyze errors in your API"
              (errorTracking prefs)
            renderOptionBox
              "performanceMonitoring"
              "Perf. monitoring"
              "Monitor API performance metrics"
              (performanceMonitoring prefs)
            renderOptionBox
              "securityTesting"
              "Security Testing"
              "Test API security and vulnerabilities"
              (securityTesting prefs)

          renderPrimaryButton POST "" "Proceed"
  where
    renderOptionBox :: Text -> Text -> Text -> Bool -> Html ()
    renderOptionBox name title description isChecked = do
      -- Inline styles
      style_ [type_ "text/css"] ".card.selected { background-color: #f0f9ff; border-color: #3b82f6; }"

      -- Card container
      div_ [class_ cardClass, onclick_ ("toggleCard('" <> name <> "')"), id_ name] $ do
        div_ [class_ "flex justify-between items-center mb-2"] $ do
          h2_ [class_ "text-black text-sm font-medium font-['Inter'] leading-snug"] $ toHtml title
          input_
            ( [ type_ "checkbox"
              , name_ name
              , value_ "true"
              , class_ "h-4 w-4"
              , onclick_ "event.stopPropagation()"
              ]
                ++ [checked_ | isChecked]
            )
        p_ [class_ "text-[#475467] text-sm font-normal font-['Inter'] leading-snug"] $ toHtml description

      script_ [type_ "text/javascript"] "function toggleCard(id) { const card = document.getElementById(id); card.classList.toggle('selected'); const checkbox = card.querySelector('input[type=\"checkbox\"]'); checkbox.checked = !checkbox.checked; }"
      where
        cardClass =
          "card border rounded-xl cursor-pointer transition-all p-4 bg-slate-50 border-slate-300"
            <> if isChecked
              then " selected border-blue-500 bg-blue-50"
              else ""


renderUrlMonitorFormPage :: Html ()
renderUrlMonitorFormPage = do
  renderOnboardingWrapper (Just "md:w-2/3 px-40") $ do
    div_ [class_ "space-y-8"] $ do
      -- Question
      renderFormQuestion "Let's create your" (Just "first URL Monitor") Nothing

      -- Answer
      form_
        [ id_ "url_monitor_test_confirmation"
        , enctype_ "application/x-www-form-urlencoded"
        , hxPost_ "/onboarding/url-monitor"
        , hxSwap_ "outerHTML"
        , hxTrigger_ "submit"
        ]
        $ do
          div_ [] $ do
            -- Add hidden input for monitor_tested
            input_ [type_ "hidden", name_ "monitor_tested", value_ "true"]
            div_ [class_ "overflow-y-hidden flex-1 "] $ termRaw "assertion-builder" [id_ ""] ""
            div_ [class_ "overflow-y-hidden flex-1 "] $ termRaw "steps-editor" [id_ "stepsEditor"] ""

            script_ [src_ "/public/assets/testeditor-utils.js"] ("" :: Text)
            script_ [type_ "module", src_ "/public/assets/steps-editor.js"] ("" :: Text)
            script_ [type_ "module", src_ "/public/assets/steps-assertions.js"] ("" :: Text)
            script_
              [text|

                function codeToggle(e) {
                  if(e.target.checked) {
                      window.updateEditorVal()
                    }
                }
                function addToAssertions(event, assertion, operation) {
                    const parent = event.target.closest(".tab-content")
                    const step = Number(parent.getAttribute('data-step'));
                    const target = event.target.parentNode.parentNode.parentNode
                    const path = target.getAttribute('data-field-path');
                    const value = target.getAttribute('data-field-value');
                    let expression = "$.resp.json." + path
                    if(operation) {
                      expression +=  ' ' + operation + ' ' + value;
                      }
                    window.updateStepAssertions(assertion, expression, step);
                }

            function saveStepData()  {
              const data = document.getElementById('stepsEditor').collectionSteps
              const parsedData = validateYaml(data)
              if(parsedData === undefined) {
                  return undefined
                }
              return parsedData;
              }

              function getTags() {
                const tag = window.tagify.value
                return tag.map(tag => tag.value);
              }
            |]
            let res = toText "respJson" -- todo
            script_
              [text|
                window.collectionResults = $res;
                document.addEventListener('DOMContentLoaded', function(){{
                    window.updateCollectionResults($res);
                }})
              |]

          div_ [class_ "w-2/3 flex flex-start justify-start"]
            $ renderPrimaryButton POST "" "Proceed"


renderPricingPlanPage :: PricingPlan -> Html ()
renderPricingPlanPage plan =
  renderOnboardingWrapper Nothing $ do
    div_ [class_ "space-y-8"] $ do
      renderFormQuestion "Choose a plan for" (Just "your team") Nothing

      form_
        [ hxPost_ "/onboarding/pricing"
        , hxSwap_ "outerHTML"
        ]
        $ do
          div_ [class_ "space-y-8"] $ do
            input_ [type_ "hidden", name_ "planType", value_ "pay-as-you-go"]

            div_ [class_ "rounded-xl border p-6 space-y-6"] $ do
              h2_ [class_ "text-slate-950 text-sm font-medium font-['Inter'] leading-snug", id_ "pricing-section"] "Pay as you use"
              div_ [class_ "space-y-2"] $ do
                div_ [class_ "flex items-baseline"] $ do
                  span_ [class_ "text-[#067cff] text-3xl font-semibold font-['Inter'] leading-8"] "$34/"
                  span_ [class_ "text-slate-950 text-base font-medium font-['Inter'] leading-normal"]
                    $ " "
                    <> show (planRequestVolume plan)
                    <> "k requests per month"
                div_ [class_ "text-[#475467] text-sm font-medium font-['Inter'] leading-3"] "then $1 per 20k requests"

              div_ [class_ "mt-4"] $ do
                label_ [Lucid.for_ "requestVolume"] "Select request volume"
                div_ [class_ "flex items-center gap-4"] $ do
                  input_
                    [ type_ "range"
                    , name_ "requestVolume"
                    , class_ "w-full"
                    , min_ "0"
                    , max_ "1000000"
                    , value_ (show $ planRequestVolume plan)
                    , hxTrigger_ "input"
                    , hxPost_ "/update-price"
                    , hxTarget_ "#price-display"
                    ]

            div_ [class_ "rounded-xl border p-6 space-y-6"] $ do
              h2_ [class_ "text-slate-950 text-sm font-medium font-['Inter'] leading-snug", id_ "features-section"] "Features"
              ul_ [class_ "space-y-3"] $ do
                forM_ (planFeatures plan) renderPlanFeature

            renderPrimaryButton POST "" "Start Free Trial"
  where
    renderPlanFeature :: PlanFeature -> Html ()
    renderPlanFeature feature =
      li_ [class_ "flex items-center"] $ do
        div_ [class_ "w-5 h-5 bg-blue-500 rounded-full mr-3 flex items-center justify-center"] $ do
          img_ [src_ "/public/assets/svgs/onboarding/check.svg"]
        p_ [class_ "text-[#475467] text-sm font-normal font-['Inter'] leading-snug"] $ do
          toHtml $ case feature of
            UnlimitedTeamMembers -> "Max Unlimited team members"
            DataRetentionDays days -> show days <> " days data retention"
            ApiTestingPipelines -> "API testing pipelines"
            SwaggerHosting -> "API swagger/OpenAPI hosting"
            CustomMonitors -> "API dmetrics custom monitors"
            AIValidations -> "API live traffic AI-based validations"


renderTeamInvitePage :: TeamInvitationList -> Html ()
renderTeamInvitePage (TeamInvitationList members) =
  renderOnboardingWrapper Nothing $ do
    div_ [class_ "space-y-8"] $ do
      renderFormQuestion "Invite your team" (Just "Member") Nothing

      form_
        [ id_ "team-invite-form"
        , enctype_ "application/x-www-form-urlencoded"
        , hxPost_ "/onboarding/team"
        , hxSwap_ "outerHTML"
        , hxTrigger_ "submit"
        ]
        $ do
          div_ [class_ "space-y-8"] $ do
            div_ [class_ "space-y-6"] $ do
              div_ [] $ do
                label_ [class_ "text-slate-950 text-sm font-medium font-['Inter'] leading-snug"] "Add team members"
                div_ [class_ "flex items-center gap-2 border border-slate-200 rounded-xl p-1"] $ do
                  input_
                    [ type_ "email"
                    , id_ "new-member-email"
                    , name_ "new-email"
                    , placeholder_ "exodustimothy@gmail.com"
                    , class_ "flex-1 px-4 py-2 text-slate-600 text-[#475467] text-sm font-normal font-['Inter'] leading-snug placeholder-slate-600 bg-transparent focus:outline-none"
                    ]
                  button_
                    [ id_ "add-member-btn"
                    , class_ "px-6 py-2 bg-blue-500 text-white text-sm font-medium rounded-lg hover:bg-blue-600 focus:outline-none"
                    , type_ "button"
                    ]
                    "Add"

              hr_ []

              template_ [id_ "member-template"] $ do
                div_ [class_ "flex items-center gap-2 member-row"] $ do
                  div_ [class_ "flex-1"] $ do
                    div_ [class_ "border border-slate-200 rounded-lg px-4 py-2"] $ do
                      input_
                        [ type_ "hidden"
                        , name_ "members[]"
                        , class_ "member-email"
                        ]
                      span_ [class_ "text-slate-600 text-sm member-email-display"] mempty
                  select_
                    [ class_ "text-slate-600 text-sm border border-slate-200 rounded-lg py-2 pl-4 shadow pr-8 hover:border-slate-300 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                    , name_ "roles[]"
                    ]
                    $ do
                      option_ [value_ "Admin"] "Admin"
                      option_ [value_ "Member"] "Member"
                      option_ [value_ "Viewer", selected_ ""] "Viewer"
                  div_ [class_ "flex items-center justify-center w-8 h-8 rounded-full bg-white border border-slate-200 shadow px-2"] $ do
                    button_
                      [ type_ "button"
                      , class_ "text-red-500 hover:text-red-600"
                      , onclick_ "this.closest('.member-row').remove()"
                      ]
                      $ img_ [src_ "/public/assets/svgs/onboarding/trash.svg", alt_ "delete"]

              div_ [id_ "team-members-list", class_ "space-y-1"] $ do
                forM_ members renderTeamMember

            div_ [class_ "space-y-4"] $ do
              renderPrimaryButton POST "" "Proceed"
              renderSecondaryButton "" "I'll invite colleagues later"

    script_
      [type_ "text/javascript"]
      [text|
document.addEventListener('DOMContentLoaded', function() {
    console.log('Team invite page initialized');
    
    const addButton = document.getElementById('add-member-btn');
    const membersList = document.getElementById('team-members-list');
    const newEmailInput = document.getElementById('new-member-email');
    const template = document.getElementById('member-template');

    if (addButton && membersList && newEmailInput && template) {
        addButton.addEventListener('click', function(e) {
            e.preventDefault();
            
            const email = newEmailInput.value.trim();
            
            // Basic email validation
            const emailPattern = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
            if (!email || !emailPattern.test(email)) {
                alert('Please enter a valid email address');
                return;
            }
            
            // Check for duplicate emails
            const existingEmails = Array.from(membersList.querySelectorAll('.member-email'))
                .map(input => input.value);
            if (existingEmails.includes(email)) {
                alert('This email has already been added');
                return;
            }

            // Clone the template
            const clone = template.content.cloneNode(true);

            // Set the email in both the hidden input and display span
            const memberEmailInput = clone.querySelector('.member-email');
            const memberEmailDisplay = clone.querySelector('.member-email-display');
            
            memberEmailInput.value = email;
            memberEmailDisplay.textContent = email;

            // Add the new member row
            membersList.appendChild(clone);

            // Clear the input field - force it to empty string
            document.getElementById('new-member-email').value = '';
            
            // Optional: Focus back on the input for next entry
            document.getElementById('new-member-email').focus();
        });
    }
});
|]


renderTeamMember :: TeamMember -> Html ()
renderTeamMember member =
  div_ [class_ "flex items-center gap-2 member-row"] $ do
    div_ [class_ "flex-1"] $ do
      div_ [class_ "border border-slate-200 rounded-lg px-4 py-2"] $ do
        input_
          [ type_ "hidden"
          , name_ "members[]"
          , value_ member.memberEmail
          , class_ "member-email"
          ]
        span_ [class_ "text-slate-600 text-sm"] $ toHtml member.memberEmail

    select_
      [ class_ "text-slate-600 text-sm border border-slate-200 rounded-lg py-2 pl-4 shadow pr-8 hover:border-slate-300 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
      , name_ "roles[]"
      ]
      $ do
        option_
          (value_ "Admin" : [selected_ "" | member.memberRole == Admin])
          "Admin"
        option_
          (value_ "Member" : [selected_ "" | member.memberRole == Member])
          "Member"
        option_
          (value_ "Viewer" : [selected_ "" | member.memberRole == Viewer])
          "Viewer"

    div_ [class_ "flex items-center justify-center w-8 h-8 rounded-full bg-white border border-slate-200 shadow px-2"] $ do
      button_
        [ type_ "button"
        , class_ "text-red-500 hover:text-red-600"
        , onclick_ "this.closest('.member-row').remove()"
        ]
        $ img_ [src_ "/public/assets/svgs/onboarding/trash.svg", alt_ "delete"]


renderCheckInboxPage :: Html ()
renderCheckInboxPage =
  div_ [class_ "max-w-md mx-auto p-6 text-start my-3 pt-40"] $ do
    -- Logo
    div_ [class_ "text-3xl mb-4 "]
      $ img_ [src_ "/public/assets/svgs/logo_min.svg", alt_ "APIToolkit Mini Logo"]

    -- Header
    renderFormQuestion "Check your inbox" Nothing Nothing

    -- Email clients
    div_ [class_ "flex justify-start gap-3 mt-8 mb-8"] $ do
      renderEmailClient "https://gmail.com" "gmail.svg" "Gmail"
      renderEmailClient "https://outlook.com" "outlook.svg" "Outlook"
      renderEmailClient "https://superhuman.com/" "super_human.svg" "Superhuman"

    -- Help text
    p_ [class_ "text-[#475467] text-sm font-medium font-['Inter'] leading-5"] $ do
      "Can't see the e-mail? Please check the"
      br_ []
      "spam folder. Wrong e-mail? "
      a_ [href_ "#", class_ "text-[#067cff] text-sm font-medium font-['Inter'] underline leading-5 hover:text-blue-700 underline"] "Change"
  where
    renderEmailClient :: Text -> Text -> Text -> Html ()
    renderEmailClient url icon alt =
      a_
        [ href_ url
        , class_ "p-6 border rounded-2xl hover:border-blue-500 transition-colors duration-200"
        ]
        $ img_ [src_ ("/public/assets/svgs/onboarding/" <> icon), alt_ alt]


renderDataLocationSelectPage :: HostingLocation -> Html ()
renderDataLocationSelectPage location = do
  renderOnboardingWrapper Nothing $ do
    style_
      [type_ "text/css"]
      [text| 
        input[type="radio"] {
    appearance: none;
    -webkit-appearance: none;
    width: 20px;
    height: 20px;
    background-color: #fff;
    border-radius: 50%;
    margin: 0;
    border: 2px solid #e2e8f0;
    position: relative;
    cursor: pointer;
}

input[type="radio"]:checked {
    background-color: white;
    border-color: #3b82f6;
}

input[type="radio"]:checked::after {
    content: "";
    display: block;
    width: 10px;
    height: 10px;
    background-color: #3b82f6;
    border-radius: 50%;
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
}

input[type="radio"]:checked + label,
label:has(input[type="radio"]:checked) {
    border-color: #3b82f6;
    background-color: white;
}

input[type="radio"]:focus {
    outline: none;
}
        |]
    div_ [class_ "space-y-8"] $ do
      renderFormQuestion "Where should your" (Just "data be hosted?") Nothing

      form_
        [ class_ "space-y-4"
        , hxPost_ "/onboarding/hosting"
        , id_ "pick_data_center_form"
        , hxSwap_ "outerHTML"
        , hxTrigger_ "submit"
        , method_ "POST"
        , enctype_ "application/x-www-form-urlencoded"
        ]
        $ do
          renderLocation "Europe (EU)" (location == Europe)
          renderLocation "United State (US)" (location == UnitedStates)
          renderLocation "Asia" (location == Asia)

          renderPrimaryButton POST "" "Proceed"
  where
    renderLocation :: Text -> Bool -> Html ()
    renderLocation name isChecked = do
      label_
        [ class_ "w-full px-6 py-4 bg-white rounded-xl border border-gray-200 flex justify-between items-center hover:border-blue-100 hover:bg-blue-50 transition-all duration-200 cursor-pointer"
        ]
        $ do
          span_
            [class_ "font-medium"]
            (toHtml name)
          input_
            ( [ type_ "radio"
              , name_ "location"
              , value_ name
              , class_ "w-5 h-5 rounded-full border-2 border-gray-200 text-blue-500 focus:ring-blue-500"
              ]
                ++ ([checked_ | isChecked])
            )

renderNotificationSentPage :: Html ()
renderNotificationSentPage = do
  div_ [class_ "max-w-md mx-auto p-auto pt-40"] $ do
    -- Back button
    renderBackButton "Back"

    -- Main content
    renderFormQuestion "Test notification sent" (Just "to configured channels") Nothing

    -- Paper plane image
    div_ [class_ "flex justify-center py-12"]
      $ img_ [src_ "/public/assets/svgs/onboarding/sent.svg", alt_ "Notification sent"]

    -- Action buttons - Now wrapped in a form
    form_
      [ hxPost_ "/onboarding/notification-sent"
      , hxSwap_ "outerHTML"
      ]
      $ do
        input_ [type_ "hidden", name_ "confirmed", value_ "true"]
        div_ [class_ "space-y-6"] $ do
          renderPrimaryButton POST "" "Confirmed"
          renderSecondaryButton "/onboarding/notifications" "Back to notification channels"


renderFrameworkIntegrationPage :: FrameworkIntegration -> Html ()
renderFrameworkIntegrationPage integration =
  div_ [class_ "max-w-7xl mx-auto p-6 pt-40 flex"]
    $ div_ [class_ "grid grid-cols-6  items-start"]
    $ do
      -- Left Column
      div_ [class_ "col-span-2 space-y-6"] $ do
        renderBackButton "Back"
        renderTitle
        renderInfoMessage "Integrate at least one service to proceed (dev/local instance is sufficient)."
        renderSearchBox
        renderFrameworksList
        renderActionButtons

      -- Separating Column
      div_ [class_ "col-span-1"] ""

      -- Right Column
      div_ [class_ "col-span-3 space-y-6"] $ do
        h2_
          [class_ "text-3xl text-slate-950 font-semibold font-['Inter'] leading-10"]
          "Configure Express SDK"
        renderTabs
        renderConfigSection
  where
    renderTitle =
      renderFormQuestion "What language/" (Just "frameworks will you be") (Just "integrating?")

    renderSearchBox =
      div_ [class_ "relative w-full"] $ do
        div_ [class_ "absolute inset-y-0 left-4 flex items-center pointer-events-none"]
          $ img_ [src_ "/public/assets/svgs/onboarding/search.svg", alt_ "search", class_ "w-5 h-5"]
        input_
          [ type_ "search"
          , placeholder_ "Search language/framework"
          , class_ "w-full pl-12 pr-4 py-2 bg-slate-50 border-slate-300 border-2 rounded-2xl shadow-sm placeholder-slate-500 font-normal font-['Inter'] leading-snug focus:outline-none focus:ring-2 focus:ring-blue-100 focus:border-blue-200 text-lg text-slate-600"
          ]

    renderFrameworksList =
      div_ [class_ "space-y-1 overflow-y-auto scrollbar-thin scrollbar-thumb-slate-300 scrollbar-track-slate-100 hover:scrollbar-thumb-slate-400"] $ do
        -- Using the integration value to highlight selected framework
        let isSelected fw = integration.framework == fw
        renderFrameworkItem "Go" "go_logo.svg" "Native" (isSelected Native)
        renderFrameworkItem "JS" "angular_logo.svg" "Express" (isSelected Express)
        renderFrameworkItem "Python" "django_logo.svg" "Django" (isSelected Django)

    renderActionButtons =
      div_ [] $ do
        p_
          [class_ "text-slate-500 text-sm font-medium font-['Inter'] leading-none"]
          "This button below will check if APItoolkit has received telemetry data from the integration."
        div_ [class_ "space-y-4 pt-4"] $ do
          renderPrimaryButton POST "" "Confirm Integration and Proceed"
          div_
            [class_ "text-center"]
            $ do
              button_
                [ class_ "text-center text-slate-500 text-sm font-medium font-['Inter'] underline leading-snug"
                , hxGet_ "https://app.apitoolkit.io/"
                , hxTrigger_ "intersect once, click"
                ]
                "Skip integration"

    renderFrameworkItem :: Text -> Text -> Text -> Bool -> Html ()
    renderFrameworkItem lang logo framework isHighlighted =
      div_ [class_ "flex items-center justify-between p-3"] $ do
        div_ [class_ "flex items-center space-x-3"] $ do
          input_
            ( [ type_ "checkbox"
              , name_ lang
              , class_ "w-4 h-4 rounded border border-slate-300"
              -- ,
              ]
                ++ ([checked_ | isHighlighted])
            )
          div_ [class_ "bg-slate-200 border rounded-lg p-1"]
            $ img_ [src_ ("/public/assets/svgs/onboarding/" <> logo), alt_ lang, class_ "w-6 h-6"]
          div_ [] $ do
            span_ [class_ txtColor] (toHtml lang)
            span_ [class_ $ txtColor <> " mx-2 w-4 h-4"] "›"
            span_ [class_ $ txtColor <> if isHighlighted then mempty else " text-black"] (toHtml framework)
        if not isHighlighted
          then button_ [class_ "text-sm hover:bg-slate-50 h-6 px-2 py-md rounded-lg border border-slate-500 justify-center items-center gap-1"] "View doc"
          else span_ [] mempty
      where
        txtColor =
          if isHighlighted
            then "text-[#067cff] text-sm font-medium font-['Inter'] leading-snug"
            else "text-[#475467] text-sm font-medium font-['Inter'] leading-snug"
    renderTabs =
      div_ [class_ "flex space-x-6 border-b"] $ do
        button_ [class_ "px-1 py-2 border-b-2 border-blue-600 text-blue-600"] "Request monitoring"
        button_ [class_ "px-1 py-2 text-slate-500 hover:text-slate-700"] "Error Reporting"
        button_ [class_ "px-1 py-2 text-slate-500 hover:text-slate-700"] "Outgoing request monitoring"

    renderConfigSection =
      div_ [class_ "col-span-3 space-y-4 pl-5"] $ do
        h2_ [class_ "text-lg font-semibold"] "Configure Express SDK"
        p_ [class_ "text-slate-600"] "Install the APItoolkit express SDK using npm/bun/pnpm"

        -- Installation command
        div_ [class_ "p-3 flex justify-start w-full h-10 px-3 py-2 bg-slate-100 rounded-xl items-center gap-2"] $ do
          code_ [class_ "text-sm text-slate-700"] "$ npm install apitoolkit-express"
          button_ [class_ "text-slate-400 hover:text-slate-600"]
            $ faSprite_ "copy" "regular" "w-4 h-4"

        -- Code example
        div_ [class_ "flex bg-slate-100 p-4 rounded-md items-start justify-between w-full"] $ do
          pre_ [class_ "text-slate-500 text-sm font-medium font-['Inter'] leading-snug bg-slate-100"]
            $ code_ []
            $ toHtml expressExample
          button_ [class_ "text-slate-400 hover:text-slate-600 bg-slate-100"]
            $ faSprite_ "copy" "regular" "w-4 h-4"

        div_ [class_ "space-y-2 w-full"] $ do
          div_
            [class_ "text-[#131a25] text-xl font-semibold font-['Inter'] leading-7"]
            "Configure Express SDK"
          div_
            [class_ "text-slate-500 text-sm font-medium font-['Inter'] leading-snug "]
            "Install the APItookit express SDK using npm/bun/pnpm"

    expressExample :: Text
    expressExample =
      unlines
        [ "import express from 'express';"
        , "import { APIToolkit } from 'apitoolkit-express';"
        , ""
        , "const app = express();"
        , "const port = 3000;"
        , ""
        , "app.use(express.json());"
        , "app.use(express.urlencoded({ extended: true }));"
        , ""
        , "const apitoolkitClient = APIToolkit.NewClient({ "
        , "  apiKey: 'xd5P3MtWaHwZIdJL0B2YTZrOBDjE1tWe7IPuTOpepjoGoFjA' "
        , "});"
        , "app.use(apitoolkitClient.expressMiddleware);"
        , ""
        , "app.get('/', (req, res) => {"
        , "  res.json({message:'Hello World!'})"
        , "});"
        , ""
        , "app.listen(port, () => {"
        , "  console.log(`Example app listening on port ${port}`);"
        , "});"
        ]
