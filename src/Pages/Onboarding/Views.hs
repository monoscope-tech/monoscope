module Pages.Onboarding.Views (
  OnboardingResponse (..), -- Export the type and all its constructors
  renderSignupPage,
  renderLoginForm,
  renderUserForm,
  renderNotificationSettingsForm,
  renderUrlMonitorForm,
  renderPricingPlan,
  renderTeamInvite,
  renderCheckInbox,
  renderDataLocationSelect,
  renderNotificationSent,
  renderFrameworkIntegration,
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
import Lucid hiding (for_)
import Lucid.Htmx (hxDelete_, hxGet_, hxPost_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import OddJobs.Job (createJob)
import Pages.BodyWrapper
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
    toHtml $ PageCtx bwconf $ renderUserForm form validation
  toHtml (HostingR (PageCtx bwconf (sess, cfg, location))) =
    toHtml $ PageCtx bwconf $ renderDataLocationSelect
  toHtml (UsageR (PageCtx bwconf (sess, cfg, usage))) =
    toHtml $ PageCtx bwconf $ renderUsageSelectionForm
  toHtml (URLMonitorR (PageCtx bwconf (sess, cfg, monitor))) =
    toHtml $ PageCtx bwconf $ renderUrlMonitorForm
  toHtml (NotificationsR (PageCtx bwconf (sess, cfg, notifications))) =
    toHtml $ PageCtx bwconf $ renderNotificationSettingsForm
  toHtml (TeamR (PageCtx bwconf (sess, cfg, team))) =
    toHtml $ PageCtx bwconf $ renderTeamInvite
  toHtml (PricingR (PageCtx bwconf (sess, cfg, pricing))) =
    toHtml $ PageCtx bwconf $ renderPricingPlan
  toHtml (CheckInboxR (PageCtx bwconf (sess, cfg))) =
    toHtml $ PageCtx bwconf $ renderCheckInbox
  toHtml (FrameworkR (PageCtx bwconf (sess, cfg, framework))) =
    toHtml $ PageCtx bwconf $ renderFrameworkIntegration
  toHtml (NotificationSentR (PageCtx bwconf (sess, cfg))) =
    toHtml $ PageCtx bwconf $ renderNotificationSent
  toHtml (NoContent msg) =
    div_ [] $ toHtml msg
  toHtmlRaw = toHtml


renderSignupPage :: SignupForm -> FormValidationResult SignupForm -> Html ()
renderSignupPage form validation =
  div_ [class_ "max-w-md space-y-8 mx-auto"] $ do
    -- Logo Position
    div_ [class_ "text-2xl m-4"] $
      img_ [src_ "/public/assets/svgs/logo_mini.svg", alt_ ""]

    -- Header
    div_ [class_ "text-start"] $ do
      h2_ [class_ "text-2xl font-semibold text-gray-900"] "Create a free\nAPIToolkit account"
      p_ [class_ "mt-2 text-sm text-gray-600"] $ do
        "Already have an account? "
        a_ [href_ "#", class_ "text-blue-600 hover:text-blue-500"] "Login"

    -- Form
    form_ [class_ "mt-8 space-y-6"] $ do
      div_ [class_ "space-y-4"] $ do
        -- Email Field
        div_ $ do
          label_ [class_ "block text-sm font-medium text-gray-700"] "Email Address"
          div_ [class_ "mt-1"] $
            input_
              [ type_ "email"
              , placeholder_ "hello.john@email.com"
              , class_ "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-blue-500 focus:border-blue-500"
              ]

        -- Password Field
        div_ $ do
          label_ [class_ "block text-sm font-medium text-gray-700"] "Password"
          div_ [class_ "mt-1 relative"] $ do
            input_
              [ type_ "password"
              , placeholder_ "Enter 8 digit password"
              , class_ "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-blue-500 focus:border-blue-500"
              ]
            button_ [type_ "button", class_ "absolute inset-y-0 right-0 pr-3 flex items-center"] $
              do
                faSprite_ "visible" "regular" "h-5 w-5 text-slate-950"

        -- Password Requirements
        div_ [class_ "flex flex-wrap gap-2 text-sm text-gray-600"] $ do
          span_ [class_ "px-2 py-1 bg-gray-100 rounded-md"] "8 characters"
          span_ [class_ "px-2 py-1 bg-gray-100 rounded-md"] "Number"
          span_ [class_ "px-2 py-1 bg-gray-100 rounded-md"] "Lowercase"
          span_ [class_ "px-2 py-1 bg-gray-100 rounded-md"] "Uppercase"
          span_ [class_ "px-2 py-1 bg-gray-100 rounded-md"] "1 special character"

      -- New Account Button
      button_
        [ type_ "submit"
        , class_ "w-full px-6 py-4 bg-gradient-to-b from-[#256BF6] to-[#1055DE] text-white text-2xl rounded-2xl shadow-inner shadow-black/[0.14] hover:opacity-90 transition-opacity duration-300 ease-out"
        ]
        "Create free account"

      -- Divider
      div_ [class_ "relative"] $ do
        div_ [class_ "absolute inset-0 flex items-center"] $
          div_ [class_ "w-full border-t border-gray-300"] ""
        div_ [class_ "relative flex justify-center text-sm"] $
          span_ [class_ "px-2 bg-white text-gray-500"] "or"

      -- Social Login Buttons
      div_ [class_ "space-y-3"] $ do
        -- Google Login Button
        button_
          [ type_ "button"
          , class_ "w-full py-2 px-4 border border-gray-300 rounded-md shadow-sm bg-slate-50 rounded-2xl text-sm font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 flex items-center justify-center space-x-2"
          ]
          $ do
            img_ [src_ "https://www.google.com/favicon.ico", alt_ "Google", class_ "w-5 h-5"]
            span_ "Continue with Google"

        -- GitHub Login Button
        button_
          [ type_ "button"
          , class_ "w-full py-2 px-4 border border-gray-300 bg-slate-50 rounded-2xl text-sm font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 flex items-center justify-center space-x-2"
          ]
          $ do
            faSprite_ "github" "regular" "h-5 w-5 text-slate-950 "
            span_ "Continue with GitHub"


renderUserForm :: ProfileForm -> FormValidationResult ProfileForm -> Html ()
renderUserForm form validation = do
  -- Logo section
  div_ [class_ "text-2xl mb-4 m-4"] $
    img_ [src_ "../assets/images/logo.png", alt_ "Logo"]

  div_ [class_ "max-w-md mx-auto p-6 pt-40"] $ do
    -- Back button with placeholder image instead of SVG
    button_ [class_ "flex items-center gap-2 mb-8 group"] $ do
      div_ [class_ "w-8 h-8 flex items-center justify-center bg-blue-500 rounded-full"] $
        faSprite_ "chevron-left-simple" "regular" "w-3 h-3 text-white"
      span_ [class_ "text-gray-600 text-lg"] "Back"

    -- Form section
    div_ [class_ "space-y-8"] $ do
      h1_ [class_ "text-3xl font-bold"] $ do
        "Tell us a little bit"
        br_ []
        "about you"

      -- Form with HTMX attributes
      form_
        [ class_ "space-y-6"
        , hxPost_ "/onboarding/profile"
        , hxSwap_ "outerHTML"
        , hxTrigger_ "submit"
        ]
        $ do
          -- Name fields
          div_ [class_ "grid grid-cols-2 gap-4"] $ do
            div_ [] $ do
              label_ [class_ "block text-sm text-gray-600 mb-1"] "First Name"
              input_
                [ type_ "text"
                , placeholder_ "e.g Mike"
                , name_ "profileFirstName"
                , value_ (profileFirstName form)
                , class_ "w-full px-3 py-2 border shadow-sm rounded-xl border-slate-300 justify-start items-start gap-2.5 inline-flex placeholder-gray-400 focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                ]

            div_ [] $ do
              label_ [class_ "block text-sm text-gray-600 mb-1"] "Last"
              input_
                [ type_ "text"
                , placeholder_ "E.g Abel"
                , name_ "profileLastName"
                , class_ "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                ]

          -- Company name
          div_ [] $ do
            label_ [class_ "block text-sm text-gray-600 mb-1"] "Company name"
            input_
              [ type_ "text"
              , placeholder_ "Apple, Inc"
              , name_ "profileCompanyName"
              , class_ "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-blue-500 focus:border-blue-500"
              ]

          -- Company Size
          div_ [] $ do
            label_ [class_ "block text-sm text-gray-600 mb-1"] "Company Size"
            input_
              [ type_ "text"
              , placeholder_ "6 employees"
              , name_ "profileCompanySize"
              , class_ "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-blue-500 focus:border-blue-500"
              ]

          -- Where did you hear about us
          div_ [] $ do
            label_ [class_ "block text-sm text-gray-600 mb-1"] "Where did you hear about us?"
            div_ [class_ "relative"] $ do
              select_
                [ class_ "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm appearance-none bg-white focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                , name_ "profileReferralSource"
                ]
                $ do
                  option_ [selected_ "", disabled_ ""] "Select an option"
                  option_ [] "Google"
                  option_ [] "Social Media"
                  option_ [] "Friend/Colleague"

              -- Dropdown arrow as placeholder image instead of SVG
              div_ [class_ "absolute inset-y-0 right-0 flex items-center pr-3 pointer-events-none"] $
                img_
                  [ src_ "/api/placeholder/20/20"
                  , alt_ "Dropdown arrow"
                  , class_ "h-5 w-5 text-gray-400"
                  ]

          -- Submit button
          button_
            [ type_ "submit"
            , class_ "h-3.7 w-full shadow-inner py-4 px-6 border-2 border-transparent rounded-2xl text-white bg-gradient-to-b from-[#256BF6] to-[#1055DE] hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 mt-8 justify-center items-center gap-2.5 inline-flex"
            ]
            "Proceed"


renderLoginForm :: LoginForm -> FormValidationResult LoginForm -> Html ()
renderLoginForm form validation = do
  div_ [class_ "w-full max-w-md space-y-8 mx-auto"] $ do
    -- Logo and welcome section
    div_ [class_ "text-start"] $ do
      div_ [class_ "text-2xl mb-4"] $
        img_ [src_ "../assets/svgs/logo_mini.svg", alt_ ""]

      h2_ [class_ "text-3xl font-bold text-gray-900"] $ do
        "Welcome back"
        br_ []
        "APIToolkit"

      p_ [class_ "mt-2 text-sm text-gray-600"] $ do
        "Don't have an account? "
        a_ [href_ "#", class_ "text-blue-600 hover:text-blue-500"] "Sign up for free."

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
          label_ [class_ "block text-sm font-medium text-gray-700"] "Email Address"
          div_ [class_ "mt-1 relative"] $ do
            div_ [class_ "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none"] $ do
              faSprite_ "envelope" "regular" "h-5 w-5 text-gray-400"
            input_
              [ type_ "email"
              , name_ "email"
              , placeholder_ "hello.john@email.com"
              , class_ "pl-10 w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-blue-500 focus:border-blue-500"
              ]

        -- Password field
        div_ [] $ do
          div_ [class_ "flex items-center justify-start"] $ do
            label_ [class_ "block text-sm font-medium text-gray-700"] "Password"

          div_ [class_ "mt-1 relative"] $ do
            div_ [class_ "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none"] $ do
              faSprite_ "lock-closed" "regular" "h-5 w-5 text-gray-400"

            input_
              [ type_ "password"
              , name_ "password"
              , placeholder_ "Enter 8 digit password"
              , class_ "pl-10 w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-blue-500 focus:border-blue-500"
              ]

            button_
              [ type_ "button"
              , class_ "absolute inset-y-0 right-0 pr-3 flex items-center"
              , hxTrigger_ "click"
              , hxSwap_ "outerHTML"
              ]
              $ do
                faSprite_ "visible" "regular" "h-5 w-5 text-gray-400"

          div_ [class_ "flex items-center justify-end pt-2"] $ do
            a_ [href_ "#", class_ "text-sm text-gray-600 hover:text-gray-500"] "Forgot Password"

        -- Sign In button
        button_
          [ type_ "submit"
          , class_ "w-full py-2 px-4 border border-transparent rounded-md shadow-sm text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
          ]
          "Sign In"

        -- Divider
        div_ [class_ "relative"] $ do
          div_ [class_ "absolute inset-0 flex items-center"] $ do
            div_ [class_ "w-full border-t border-gray-300"] ""
          div_ [class_ "relative flex justify-center text-sm"] $ do
            span_ [class_ "px-2 bg-white text-gray-500"] "OR"

        -- Social login buttons
        div_ [class_ "space-y-3"] $ do
          -- Google login with placeholder image instead of faSprite
          button_
            [ type_ "button"
            , class_ "w-full py-2 px-4 border border-gray-300 rounded-md shadow-sm bg-white text-sm font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 flex items-center justify-center space-x-2"
            , hxPost_ "/api/auth/google"
            ]
            $ do
              faSprite_ "google" "regular" "h-5 w-5 text-gray-400"
              span_ [] "Continue with Google"

          -- GitHub login
          button_
            [ type_ "button"
            , class_ "w-full py-2 px-4 border border-gray-300 rounded-2xl bg-white text-sm font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 flex items-center justify-center space-x-2"
            , hxPost_ "/api/auth/github"
            ]
            $ do
              faSprite_ "github" "regular" "w-5 h-5"
              span_ [] "Continue with GitHub"


renderNotificationSettingsForm :: Html ()
renderNotificationSettingsForm = do
  -- Logo
  div_ [class_ "text-2xl mb-4 m-4"] $
    img_ [src_ "../assets/svgs/logo.svg", alt_ ""]

  div_ [class_ "max-w-md mx-auto"] $ do
    -- Back button
    button_ [class_ "flex items-center gap-2 mb-8 group"] $ do
      div_ [class_ "w-8 h-8 flex items-center justify-center bg-blue-500 rounded-full"] $
        faSprite_ "chevron-left" "regular" "w-3 h-3 text-white"
      span_ [class_ "text-gray-600 text-lg"] "Back"

    -- Main content
    div_ [class_ "space-y-8"] $ do
      div_ [class_ "w-[393px] text-slate-950 text-2xl font-semibold font-['Inter'] leading-10"] $ do
        "How should we let you"
        br_ []
        "know when something"
        br_ []
        "goes wrong?"

      div_ [class_ "space-y-4"] $ do
        -- Slack connections section
        div_ [class_ "space-y-4"] $ do
          -- First Row
          div_ [class_ "justify-between grid grid-cols-2 gap-3"] $ do
            -- Left Connection
            connectionBox_ False "slack"
            -- Right Connection
            connectionBox_ True "slack"

          -- Second Row
          div_ [class_ "justify-between grid grid-cols-2 gap-3"] $ do
            connectionBox_ False "slack"
            connectionBox_ True "slack"

          -- Phone Number Input
          div_ [class_ "space-y-2"] $ do
            p_ [class_ "text-gray-600"] "Notify phone number"
            div_ [class_ "relative"] $ do
              div_ [class_ "flex items-center w-full p-2 border-2 rounded-2xl shadow-sm"] $ do
                button_ [class_ "flex items-center gap-1 text-gray-600"] $ do
                  span_ [] "+234"
                  faSprite_ "chevron-down" "regular" "h-5 w-5"
                span_ [class_ "text-gray-400 mx-2"] "("
                input_
                  [ type_ "tel"
                  , class_ "flex-1 outline-none text-gray-700 bg-sl"
                  ]

        -- Email notifications section
        div_ [class_ "space-y-2"] $ do
          label_ [class_ "block text-md text-gray-600"] "Notify the following email addresses"
          div_ [class_ "p-2 border-2 rounded-xl"] $ do
            div_ [class_ "flex flex-wrap gap-2 mb-4"] $ do
              -- Email chips
              emailChip_ "exodustimthy@gmail.com"
              emailChip_ "exodustimthy@gmail.com"
              emailChip_ "gethasalondarea..."
              emailChip_ "gethasalondared@gmail.com"
              emailChip_ "docterman223@gmail.com"

            input_
              [ type_ "email"
              , placeholder_ "Add more emails..."
              , class_ "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-blue-500 focus:border-blue-500"
              ]

      -- Send test notifications button
      button_
        [ type_ "button"
        , class_ "w-full py-2 px-4 border-2 border-transparent rounded-2xl text-white bg-gradient-to-b from-[#256BF6] to-[#1055DE] hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 mt-8"
        ]
        "Send test notifications"
  where
    -- Helper for connection boxes
    connectionBox_ :: Bool -> Text -> Html ()
    connectionBox_ isConnected serviceName =
      div_ [class_ "flex-1 flex items-center justify-start px-3 py-1 border-2 rounded-xl"] $ do
        div_ [class_ "flex-1 flex"] $
          div_ [class_ "flex items-center gap-1"] $
            do
              -- does fasprite have alt tags?
              img_ [src_ "/api/placeholder/16/16", alt_ serviceName, class_ "w-4 h-4"]
              span_ [class_ "text-gray-900 font-medium"] (toHtml serviceName)
        div_ [class_ "flex-1 flex"] ""
        div_ [class_ "flex-1 flex"] $
          if isConnected
            then
              button_ [] $
                span_ [class_ "px-2 py-1 text-white text-sm font-medium bg-green-500 rounded-lg"] "Connected"
            else button_ [class_ "px-2 py-0 text-blue-600 text-sm font-medium border border-blue-600 rounded-lg hover:bg-blue-50"] "Connect"

    -- Helper for email chips
    emailChip_ :: Text -> Html ()
    emailChip_ email =
      div_ [class_ "flex bg-gray-200 rounded-full justify-around items-center"] $ do
        span_ [class_ "px-1 text-xs"] (toHtml email)
        faSprite_ "xmark" "regular" "w-4 h-4"


renderUsageSelectionForm :: Html ()
renderUsageSelectionForm = do
  div_ [class_ "text-2xl mb-4 m-4"] $
    img_ [src_ "./../assets/svgs/logo.svg", alt_ ""]

  div_ [class_ "max-w-lg mx-auto p-6 pt-20"] $ do
    -- Back button
    button_ [class_ "flex items-center gap-2 mb-8 group"] $ do
      div_ [class_ "w-8 h-8 flex items-center justify-center bg-blue-500 rounded-full"] $
        faSprite_ "chevron-left" "regular" "w-3 h-3 text-white"
      span_ [class_ "text-gray-600 text-lg"] "Back"

    -- Main content
    div_ [class_ "space-y-8"] $ do
      h1_ [class_ "text-3xl font-normal"] $ do
        "How would you like"
        br_ []
        "to use APIToolkit"

      -- Selection grid
      div_ [class_ "grid grid-cols-2 gap-4"] $ do
        renderOptionBox "Uptime monitoring" "Check if your website works correctly"
        renderOptionBox "Uptime monitoring" "Check if your website works correctly"
        renderOptionBox "Uptime monitoring" "Check if your website works correctly"
        renderOptionBox "Uptime monitoring" "Check if your website works correctly"

      -- Proceed button
      button_
        [ type_ "button"
        , class_ "w-full py-2 px-4 border-2 border-transparent rounded-2xl text-white bg-gradient-to-b from-[#256BF6] to-[#1055DE] hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 mt-8"
        ]
        "Proceed"
  where
    renderOptionBox :: Text -> Text -> Html ()
    renderOptionBox title description =
      label_ [class_ "relative cursor-pointer"] $ do
        input_ [type_ "checkbox", class_ "peer sr-only"]
        div_ [class_ "p-4 pb-8 border rounded-lg hover:border-blue-500 peer-checked:border-blue-500 peer-checked:border-2"] $ do
          div_ [class_ "flex justify-between items-start mb-2"] $ do
            h3_ [class_ "font-medium"] (toHtml title)
            div_ [class_ "w-5 h-5 border rounded-md peer-checked:bg-blue-500 peer-checked:border-blue-500"] ""
          p_ [class_ "text-sm text-gray-600"] (toHtml description)


renderUrlMonitorForm :: Html ()
renderUrlMonitorForm = do
  header_ [class_ "p-4"] $
    img_ [src_ "./../assets/svgs/logo.svg", alt_ "APIToolkit Logo", class_ "h-8"]

  main_ [class_ "max-w-3xl mx-auto p-6 pt-32"] $ do
    -- Back Button
    button_ [class_ "flex items-center gap-2 mb-8 group"] $ do
      div_ [class_ "w-8 h-8 flex items-center justify-center bg-blue-500 rounded-full"] $
        faSprite_ "chevron-left" "regular" "w-3 h-3 text-white"
      span_ [class_ "text-gray-600 text-lg"] "Back"

    h1_ [class_ "text-3xl mb-6 text-slate-950 font-semibold font-['Inter'] leading-10"] $ do
      "Let's create your"
      br_ []
      "first URL Monitor"

    form_ [class_ "space-y-6"] $ do
      -- URL Input Section
      div_ [class_ "space-y-4"] $ do
        label_ [class_ "block text-sm font-medium text-gray-700"] "URL"
        div_ [class_ "flex gap-2"] $ do
          select_ [class_ "w-24 px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:ring-blue-500 focus:border-blue-500"] $ do
            option_ [] "Get"
            option_ [] "Post"
            option_ [] "Put"
            option_ [] "Delete"
          input_
            [ type_ "url"
            , value_ "https://jsonplaceholder.typicode.com/todos/1"
            , class_ "flex-1 px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:ring-blue-500 focus:border-blue-500"
            ]

      -- Advanced Options Section
      section_ [class_ "space-y-4"] $ do
        div_ [class_ "flex items-center gap-2"] $ do
          span_ [class_ "text-sm font-medium text-gray-700"] "Advanced Options (0 configured)"
          div_ [class_ "flex items-center justify-center w-5 h-5 rounded-full shadow-lg bg-[#067cff] gap-2.5"] $
            button_ [type_ "button", class_ "flex items-center rounded-full justify-center w-full h-full text-white"] $
              faSprite_ "chevron-down" "regular" "w-3 h-3"

        renderRequestPanel

      -- Test Request Button
      button_
        [ type_ "button"
        , class_ "h-8 p-4 bg-gradient-to-b from-[#256bf6] to-[#1055de] justify-center items-center gap-2.5 inline-flex px-4 py-2 text-white bg-blue-600 rounded-md hover:bg-blue-700 focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 text-sm font-medium font-['Inter'] leading-snug"
        ]
        "Send"

      renderResponseSection
      renderAssertionsSection

      -- Submit Button
      button_
        [ type_ "button"
        , class_ "w-2/3 py-2 px-4 border-2 border-transparent rounded-2xl text-white bg-gradient-to-b from-[#256BF6] to-[#1055DE] hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 mt-8"
        ]
        "Proceed"
  where
    renderRequestPanel =
      div_ [class_ "border rounded-lg"] $ do
        -- Tab Navigation
        div_ [class_ "flex border-b"] $ do
          button_ [class_ "px-4 py-2 text-sm font-medium text-gray-600 border-b-2 border-blue-500"] "Request Options"
          button_ [class_ "px-4 py-2 text-sm font-medium text-gray-600"] "Request Body"

        -- Request Headers
        div_ [class_ "p-4"] $ do
          h3_ [class_ "mb-3 text-sm font-medium text-gray-700"] "Request Headers"
          div_ [class_ "flex gap-2 mb-4"] $ do
            input_
              [ type_ "text"
              , placeholder_ "Key"
              , class_ "flex-1 px-3 py-2 text-sm border border-gray-300 rounded-md shadow-sm focus:ring-blue-500 focus:border-blue-500"
              ]
            input_
              [ type_ "text"
              , placeholder_ "Value"
              , class_ "flex-[2] px-3 py-2 text-sm border border-gray-300 rounded-md shadow-sm focus:ring-blue-500 focus:border-blue-500"
              ]
            div_ [class_ "flex items-center justify-center w-8 h-8 rounded-full border border-gray-200 shadow px-2"] $
              button_ [type_ "button", class_ "text-red-500 hover:text-red-600"] $
                faSprite_ "trash" "regular" "w-4 h-4"

    renderResponseSection =
      section_ [class_ "space-y-4"] $ do
        div_ [class_ "text-sm text-gray-600"] "The request responded with a status of 200 and took 2304.00 ms"

        div_ [class_ "border rounded-2xl"] $ do
          -- Response Tabs
          div_ [class_ "flex gap-6 border-b"] $ do
            button_ [class_ "px-4 py-2 text-sm font-medium text-gray-600 border-b-2 border-blue-500"] "Response Headers"
            button_ [class_ "px-4 py-2 text-sm font-medium text-gray-600"] "Response Body"
            button_ [class_ "px-4 py-2 text-sm font-medium text-gray-600"] "Response Status Code"

          -- Response Content
          div_ [class_ "p-4"] $ do
            div_ [class_ "space-y-2 text-sm text-gray-600"] $ do
              div_ [class_ "flex items-center gap-0"] $ do
                div_ [class_ "flex items-center justify-center w-8 h-8"] $
                  faSprite_ "circle-info" "regular" "w-4 h-4"
                span_ [] "Click below to add as an assertion"

              div_ [] $
                mapM_
                  renderResponseHeaderItem
                  [ "cache-control: max-age=43200"
                  , "content-type: application/json; charset=utf-8"
                  , "expires: -1"
                  , "pragma: no-cache"
                  ]

    renderResponseHeaderItem :: Text -> Html ()
    renderResponseHeaderItem header =
      div_ [class_ "flex items-center gap-2"] $ do
        span_ [] (toHtml header)
        div_ [class_ "flex items-center justify-center w-5 h-5 rounded-full border border-gray-200 shadow"] $
          button_ [class_ "text-gray-400 hover:text-gray-600"] $
            faSprite_ "plus" "regular" "w-3 h-3"

    renderAssertionsSection =
      section_ [class_ "space-y-4"] $ do
        div_ [class_ "flex items-center justify-start gap-2"] $ do
          span_ [class_ "text-sm font-medium text-gray-700"] "Add Assertion (Optional)"
          div_ [class_ "flex items-center justify-center w-5 h-5 rounded-full shadow-lg"] $
            button_ [type_ "button", class_ "flex items-center rounded-full justify-center w-full h-full text-white bg-slate-500"] $
              faSprite_ "chevron-down" "regular" "w-3 h-3"

        div_ [class_ "p-4 border rounded-2xl"] $ do
          p_ [class_ "mb-4 text-sm text-gray-600"] "Your step is successful:"
          div_ [class_ "flex items-center gap-2 mb-4"] $ do
            span_ [] "When"
            select_ [class_ "px-3 py-2 text-sm border border-gray-300 rounded-md shadow-sm focus:ring-blue-500 focus:border-blue-500"] $ do
              option_ [] "Status code"
              option_ [] "Header"
              option_ [] "Others"
            select_ [class_ "px-3 py-2 text-sm border border-gray-300 rounded-md shadow-sm focus:ring-blue-500 focus:border-blue-500"] $ do
              option_ [] "less than"
              option_ [] "equals to"
              option_ [] "greater than"
            input_ [type_ "text", class_ "flex-1 px-3 py-2 text-sm border border-gray-300 rounded-md shadow-sm focus:ring-blue-500 focus:border-blue-500"]
            span_ [class_ "inline-flex items-center px-3 py-0.5 text-sm font-medium text-green-800 bg-green-100 rounded-full"] "Passed"
            div_ [class_ "flex items-center justify-center w-8 h-8 rounded-full border border-gray-200 shadow px-2"] $
              button_ [type_ "button", class_ "text-red-500 hover:text-red-600"] $
                faSprite_ "trash" "regular" "w-4 h-4"

        button_
          [ type_ "button"
          , class_ "flex items-center px-4 py-2 hover:text-blue-700 rounded-lg border border-slate-500 justify-center gap-1 text-slate-500 text-sm font-medium font-['Inter'] leading-snug"
          ]
          $ do
            faSprite_ "plus" "regular" "w-4 h-4 mr-1"
            "New Assertion"


renderPricingPlan :: Html ()
renderPricingPlan = do
  div_ [class_ "text-2xl mb-4 m-4"] $
    img_ [src_ "./../assets/svgs/logo.svg", alt_ ""]

  div_ [class_ "max-w-md mx-auto p-6"] $ do
    -- Back button
    button_ [class_ "flex items-center gap-2 mb-8 group"] $ do
      div_ [class_ "w-8 h-8 flex items-center justify-center bg-blue-500 rounded-full"] $
        faSprite_ "chevron-left" "regular" "w-3 h-3 text-white"
      span_ [class_ "text-gray-600 text-lg"] "Back"

    -- Main content
    div_ [class_ "space-y-8"] $ do
      h1_ [class_ "text-3xl font-normal"] $ do
        "Choose a plan for"
        br_ []
        "your team"

      -- Pricing section
      div_ [class_ "rounded-xl border p-6 space-y-6"] $ do
        h2_ [class_ "font-medium mb-4"] "Pay as you use"
        div_ [class_ "space-y-2"] $ do
          div_ [class_ "flex items-baseline"] $ do
            span_ [class_ "text-3xl font-bold text-blue-600"] "$34"
            span_ [class_ "text-gray-600 ml-2"] "/ 400k requests per month"
          div_ [class_ "text-sm text-gray-500"] "then $1 per 20k requests"

        -- Slider
        div_ [class_ "mt-4"] $
          div_ [class_ "relative w-full h-2 bg-blue-100 rounded"] $
            do
              div_ [class_ "absolute left-0 w-1/3 h-full bg-blue-500 rounded"] ""
              div_ [class_ "absolute left-1/3 -translate-x-1/2 top-1/2 -translate-y-1/2 w-4 h-4 bg-blue-500 rounded-full cursor-pointer"] ""

      -- Features section
      div_ [class_ "rounded-xl border p-6 space-y-6"] $ do
        h2_ [class_ "font-medium mb-4"] "Features"
        ul_ [class_ "space-y-3"] $ do
          renderFeature "Max Unlimited team members"
          renderFeature "14 days data retention"
          renderFeature "API testing pipelines"
          renderFeature "API swagger/OpenAPI hosting"
          renderFeature "API metrics custom monitors"
          renderFeature "API live traffic AI-based validations"

      -- Start Free Trial button
      button_
        [ type_ "button"
        , class_ "w-full py-2 px-4 border-2 border-transparent rounded-2xl text-white bg-gradient-to-b from-[#256BF6] to-[#1055DE] hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 mt-8"
        ]
        "Start Free Trial"
  where
    renderFeature :: Text -> Html ()
    renderFeature text =
      li_ [class_ "flex items-center text-gray-700"] $ do
        faSprite_ "circle-check" "solid" "w-5 h-5 mr-3 text-blue-500"
        toHtml text


renderTeamInvite :: Html ()
renderTeamInvite = do
  div_ [class_ "text-2xl mb-4 m-4"] $
    img_ [src_ "./../assets/svgs/logo.svg", alt_ ""]

  div_ [class_ "max-w-lg mx-auto p-6"] $ do
    -- Back button
    button_ [class_ "flex items-center gap-2 mb-8 group"] $ do
      div_ [class_ "w-8 h-8 flex items-center justify-center bg-blue-500 rounded-full"] $
        faSprite_ "chevron-left" "regular" "w-3 h-3 text-white"
      span_ [class_ "text-gray-600 text-lg"] "Back"

    -- Main content
    div_ [class_ "space-y-8"] $ do
      h1_ [class_ "text-3xl font-normal"] $ do
        "Invite a team"
        br_ []
        "member"

      -- Invite form
      div_ [class_ "space-y-6"] $ do
        div_ [] $ do
          label_ [class_ "block text-sm font-medium text-gray-600 mb-2"] "Add team members"
          div_ [class_ "flex items-center gap-2 border border-gray-200 rounded-xl p-1"] $ do
            input_
              [ type_ "email"
              , placeholder_ "exodustimothy@gmail.com"
              , class_ "flex-1 px-4 py-2 text-gray-600 placeholder-gray-400 bg-transparent focus:outline-none"
              ]
            button_ [class_ "px-8 py-2 bg-blue-500 text-white text-sm font-medium rounded-lg hover:bg-blue-600 focus:outline-none"] "Add"

        hr_ []

        -- Team members list
        div_ [class_ "space-y-1"] $ do
          -- Render multiple team members
          mapM_ renderTeamMember ["exodustimothy@gmail.com", "exodustimothy@gmail.com", "exodustimothy@gmail.com"]

      -- Action buttons
      div_ [class_ "space-y-4"] $ do
        button_
          [ type_ "button"
          , class_ "w-full py-2 px-4 border-2 border-transparent rounded-2xl text-white bg-gradient-to-b from-[#256BF6] to-[#1055DE] hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 mt-8"
          ]
          "Proceed"

        button_
          [ type_ "button"
          , class_ "w-full text-center text-gray-600 hover:text-gray-500 underline"
          ]
          "I'll invite colleagues later"
  where
    renderTeamMember :: Text -> Html ()
    renderTeamMember email =
      div_ [class_ "flex items-center gap-2 p-2"] $ do
        div_ [class_ "flex-1"] $
          div_ [class_ "border border-gray-200 rounded-lg px-4 py-2"] $
            span_ [class_ "text-gray-600 text-sm"] (toHtml email)

        select_ [class_ "text-gray-600 text-sm border border-gray-200 rounded-lg py-2 pl-4 pr-8 hover:border-gray-300 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"] $ do
          option_ [] "Admin"
          option_ [] "Member"
          option_ [] "Viewer"

        div_ [class_ "flex items-center justify-center w-8 h-8 rounded-full bg-gray-100 border border-gray-200 shadow px-2"] $
          button_ [type_ "button", class_ "text-red-500 hover:text-red-600"] $
            faSprite_ "trash" "regular" "w-4 h-4"


renderCheckInbox :: Html ()
renderCheckInbox =
  div_ [class_ "max-w-xs mx-auto p-6 text-start my-3 pt-40"] $ do
    -- Logo
    div_ [class_ "text-2xl mb-4"] $
      img_ [src_ "./components/svg/logo_mini.svg", alt_ ""]

    -- Header
    h1_ [class_ "text-3xl font-normal mb-12"] "Check your inbox"

    -- Email clients
    div_ [class_ "flex justify-between gap-6 mb-8"] $ do
      renderEmailClient "https://gmail.com" "gmail.svg" "Gmail"
      renderEmailClient "https://outlook.com" "outlook.svg" "Outlook"
      renderEmailClient "https://superhuman.com/" "super_human.svg" "Superhuman"

    -- Help text
    p_ [class_ "text-gray-600 text-sm"] $ do
      "Can't see the e-mail? Please check the"
      br_ []
      "spam folder. Wrong e-mail? "
      a_ [href_ "#", class_ "text-blue-600 hover:text-blue-700 underline"] "Change"
  where
    renderEmailClient :: Text -> Text -> Text -> Html ()
    renderEmailClient url icon alt =
      a_
        [ href_ url
        , class_ "p-6 border rounded-2xl hover:border-blue-500 transition-colors duration-200 shadow-sm"
        ]
        $ img_ [src_ ("./components/svg/" <> icon), alt_ alt]


renderDataLocationSelect :: Html ()
renderDataLocationSelect = do
  div_ [class_ "text-2xl mb-4 m-4"] $
    img_ [src_ "./../assets/svgs/logo.svg", alt_ ""]

  div_ [class_ "max-w-md mx-auto p-6 pt-40"] $ do
    -- Back button
    button_ [class_ "flex items-center gap-2 mb-8 group"] $ do
      div_ [class_ "w-8 h-8 flex items-center justify-center bg-blue-500 rounded-full"] $
        faSprite_ "chevron-left" "regular" "w-3 h-3 text-white"
      span_ [class_ "text-gray-600 text-lg"] "Back"

    -- Main content
    div_ [class_ "space-y-2"] $ do
      h1_ [class_ "text-3xl font-semibold"] $ do
        "Where should your"
        br_ []
        "data be hosted"

      -- Location options
      renderLocation "Europe (EU)" True
      renderLocation "United State (US)" False
      renderLocation "Asia" False

      -- Proceed button
      button_
        [ type_ "button"
        , class_ "w-full py-2 px-4 border-2 border-transparent rounded-2xl text-white bg-gradient-to-b from-[#256BF6] to-[#1055DE] hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 mt-8"
        ]
        "Proceed"
  where
    renderLocation :: Text -> Bool -> Html ()
    renderLocation name isChecked =
      label_ [class_ "relative block"] $ do
        input_
          [ type_ "radio"
          , name_ "location"
          , class_ "sr-only peer"
          -- , checked_ isChecked
          ]
        div_ [class_ "flex items-center justify-between p-4 border rounded-lg cursor-pointer hover:border-blue-500 peer-checked:border-blue-500"] $ do
          span_ [class_ "font-medium"] (toHtml name)
          div_
            [ class_ $
                "w-6 h-6 border-2 rounded-full flex items-center justify-center "
                  <> if isChecked then "border-blue-500" else "border-gray-200"
            ]
            $ do
              div_
                [ class_ $
                    "w-3 h-3 rounded-full "
                      <> if isChecked then "bg-blue-500" else "peer-checked:bg-blue-500"
                ]
                ""


renderNotificationSent :: Html ()
renderNotificationSent = do
  div_ [class_ "max-w-md mx-auto p-auto pt-40"] $ do
    -- Back button
    button_ [class_ "flex items-center gap-2 mb-8 group"] $ do
      div_ [class_ "w-8 h-8 flex items-center justify-center bg-blue-500 rounded-full"] $
        faSprite_ "chevron-left" "regular" "w-3 h-3 text-white"
      span_ [class_ "text-gray-600 text-lg"] "Back"

    -- Main content
    div_ [class_ "text-start space-y-8"] $ do
      div_ [class_ "text-slate-950 text-2xl font-semibold font-['Inter'] leading-10"] $ do
        "Test notification sent"
        br_ []
        "to configured channels"

      -- Paper plane image
      div_ [class_ "flex justify-center py-12"] $
        img_ [src_ "./components/svg/sent.svg", alt_ "Notification sent"]

      -- Action buttons
      div_ [class_ "space-y-4"] $ do
        button_
          [ type_ "button"
          , class_ "w-full py-2 px-4 border-2 border-transparent rounded-2xl shadow-sm text-white bg-gradient-to-b from-[#256BF6] to-[#1055DE] hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 mt-8"
          ]
          "Confirmed"

        a_
          [ href_ "#"
          , class_ "block text-center text-gray-600 hover:text-gray-500 underline"
          ]
          "Back to notification channels"


renderFrameworkIntegration :: Html ()
renderFrameworkIntegration =
  div_ [class_ " mx-auto p-6 pt-40"] $
    div_ [class_ "grid grid-cols-6  items-start"] $
      do
        -- Left Column
        div_ [class_ "col-span-2 space-y-6 w-80"] $ do
          renderBackButton
          renderTitle
          renderInfoMessage
          renderSearchBox
          renderFrameworksList
          renderActionButtons

        div_ [class_ "col-span-1"] ""

        -- Right Column
        div_ [class_ "col-span-3 space-y-6 p-40"] $ do
          h2_
            [class_ "text-3xl text-slate-950 font-semibold font-['Inter'] leading-10"]
            "Configure Express SDK"
          renderTabs
          renderConfigSection
  where
    renderBackButton =
      button_ [class_ "flex items-center gap-2 mb-8 group mt-8"] $ do
        div_ [class_ "w-8 h-8 flex items-center justify-center bg-blue-500 rounded-full"] $
          faSprite_ "chevron-left" "regular" "w-3 h-3 text-white"
        span_ [class_ "text-gray-600 text-lg"] "Back"

    renderTitle =
      h1_ [class_ "self-stretch text-slate-950 text-2xl font-semibold font-['Inter'] leading-10"] $ do
        "What language/"
        br_ []
        "frameworks will you be"
        br_ []
        "integrating?"

    renderInfoMessage =
      div_ [class_ "flex items-start space-x-2 text-sm text-gray-600 border p-5 bg-slate-100 rounded-2xl justify-start gap-2"] $ do
        faSprite_ "circle-info" "regular" "w-5 h-5 text-gray-400 mt-0.5"
        div_
          [class_ "text-slate-500 text-sm font-medium font-['Inter'] leading-snug"]
          "Integrate at least one service to proceed (dev/local instance is sufficient)."

    renderSearchBox =
      div_ [class_ "relative w-full flex flex-col justify-between items-start"] $ do
        div_ [class_ "absolute inset-y-0 left-4 flex items-center pointer-events-none px-4"] $
          faSprite_ "magnifying-glass" "regular" "h-5 w-5 text-gray-400"
        input_
          [ type_ "search"
          , placeholder_ "Search language/framework"
          , class_ "w-full pl-12 pr-4 py-2 bg-slate-50 border-slate-300 border-2 rounded-2xl shadow-sm placeholde-slate-500 font-normal font-['Inter'] leading-snug focus:outline-none focus:ring-2 focus:ring-blue-100 focus:border-blue-200 text-lg text-gray-600"
          ]

    renderFrameworksList =
      div_ [class_ "space-y-1 h-96 overflow-y-auto scrollbar-thin scrollbar-thumb-gray-300 scrollbar-track-gray-100 hover:scrollbar-thumb-gray-400"] $ do
        renderFrameworkItem "Go" "go_logo.svg" "Native" True
        renderFrameworkItem "JS" "angular_logo.svg" "Angular" False
        renderFrameworkItem "JS" "angular_logo.svg" "Angular" False

    renderFrameworkItem :: Text -> Text -> Text -> Bool -> Html ()
    renderFrameworkItem lang logo framework isHighlighted =
      div_ [class_ "flex items-center justify-between p-3"] $ do
        _ <- div_ [class_ "flex items-center space-x-3"] $ do
          _ <- input_ [type_ "checkbox", name_ lang, class_ "w-4 h-4 rounded border border-slate-300"]
          _ <-
            div_ [class_ "bg-slate-200 border rounded-lg p-1"] $
              img_ [src_ ("./components/svg/" <> logo), alt_ lang, class_ "w-6 h-6"]
          div_ [] $ do
            _ <- span_ [class_ txtColor] (toHtml lang)
            _ <- span_ [class_ $ txtColor <> " mx-2"] "›"
            span_ [class_ txtColor] (toHtml framework)
        button_
          [class_ "text-sm hover:bg-gray-50 h-6 px-2 py-[18px] rounded-lg border border-slate-500 justify-center items-center gap-1"]
          "View doc"
      where
        txtColor =
          if isHighlighted
            then "text-[#067cff] text-sm font-medium font-['Inter'] leading-snug"
            else "text-slate-500 text-sm font-medium font-['Inter'] leading-snug"

    renderActionButtons =
      div_ [] $ do
        p_
          [class_ "text-slate-500 text-xs font-medium font-['Inter'] leading-none"]
          "This button below will check if APItoolkit has received telemetry data from the integration."
        div_ [class_ "space-y-4 pt-4"] $ do
          button_
            [ type_ "button"
            , class_ "w-full py-2 px-4 border-2 border-transparent rounded-2xl text-white bg-gradient-to-b from-[#256BF6] to-[#1055DE] hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 mt-8"
            ]
            "Confirm Integration and Proceed"
          div_
            [class_ "text-center text-slate-500 text-sm font-medium font-['Inter'] underline leading-snug"]
            "Skip integration"

    renderTabs =
      div_ [class_ "flex space-x-6 border-b"] $ do
        button_ [class_ "px-1 py-2 border-b-2 border-blue-600 text-blue-600"] "Request monitoring"
        button_ [class_ "px-1 py-2 text-gray-500 hover:text-gray-700"] "Error Reporting"
        button_ [class_ "px-1 py-2 text-gray-500 hover:text-gray-700"] "Outgoing request monitoring"

    renderConfigSection =
      div_ [class_ "col-span-3 space-y-4"] $ do
        h2_ [class_ "text-lg font-semibold"] "Configure Express SDK"
        p_ [class_ "text-gray-600"] "Install the APItoolkit express SDK using npm/bun/pnpm"

        -- Installation command
        div_ [class_ "p-3 flex justify-start w-[593px] h-10 px-3 py-2 bg-slate-100 rounded-xl items-center gap-2"] $ do
          code_ [class_ "text-sm text-gray-700"] "$ npm install apitoolkit-express"
          button_ [class_ "text-gray-400 hover:text-gray-600"] $
            faSprite_ "copy" "regular" "w-4 h-4"

        -- Code example
        div_ [class_ "flex bg-slate-100 p-4 rounded-md items-start justify-between"] $ do
          pre_ [class_ "text-slate-500 text-sm font-medium font-['Inter'] leading-snug"] $
            code_ [] $
              toHtml expressExample
          button_ [class_ "text-gray-400 hover:text-gray-600"] $
            faSprite_ "copy" "regular" "w-4 h-4"

        div_ [class_ "space-y-2"] $ do
          div_
            [class_ "text-slate-950 text-xl font-semibold font-['Inter'] leading-7"]
            "Configure Express SDK"
          div_
            [class_ "text-slate-500 text-sm font-medium font-['Inter'] leading-snug"]
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
