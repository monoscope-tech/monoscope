module Pages.Onboarding.Types where

-- import Data.Default (Default (..), def)
-- import Data.Text (Text)
-- import Data.Time (UTCTime)
-- import Data.Vector (Vector)
-- import Data.Aeson (FromJSON, ToJSON)
-- import GHC.Generics ( Generic, Generic )

-- import GHC.Base
-- import GHC.Show
-- import Web.FormUrlEncoded (
--   FromForm (fromForm),
--   ToForm,
--   lookupUnique,
--  )
-- import Web.Internal.FormUrlEncoded (ToForm (toForm))
-- import Prelude (Either (..))
-- import Deriving.Aeson (CustomJSON, OmitNothingFields)
import Data.Default (Default (..), def)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)  -- Remove duplicate Generic import
import Web.FormUrlEncoded (
  FromForm(fromForm),
  ToForm(toForm),
  lookupUnique,
 )
-- Remove duplicate ToForm import since it's already in Web.FormUrlEncoded
-- import Web.Internal.FormUrlEncoded (ToForm (toForm))  
import Prelude (Either(..))
import Deriving.Aeson qualified as DA  -- Qualified import for clarity
import GHC.Show (Show)
import GHC.Base (Eq)
import GHC.Base (Bool)
import GHC.Base (Int)
import GHC.Base (Maybe)
import GHC.Base (Maybe(Nothing))
import GHC.Base (undefined)
import GHC.Base


-- | Signup form data
data SignupForm = SignupForm
  { signupEmail :: Text
  , signupPassword :: Text
  }
  deriving (Show, Eq)


-- | Login form data
data LoginForm = LoginForm
  { loginEmail :: Text
  , loginPassword :: Text
  }
  deriving (Show, Eq)


-- | Profile/Company details form
data ProfileForm = ProfileForm
  { profileFirstName :: Text
  , profileLastName :: Text
  , profileCompanyName :: Text
  , profileCompanySize :: Text
  , profileReferralSource :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


-- | How the user heard about the service

-- | Data hosating location selection
data HostingLocation
  = Europe
  | UnitedStates
  | Asia
  deriving (Show, Eq, Generic)


-- Convert form data to HostingLocation
instance FromForm HostingLocation where
  fromForm form = do
    locationStr <- lookupUnique "location" form
    case locationStr of
      "Europe (EU)" -> Right Europe
      "United State (US)" -> Right UnitedStates
      "Asia" -> Right Asia
      _ -> Left "Invalid hosting location. Must be one of: Europe (EU), United State (US), Asia"


-- Convert HostingLocation to form data
instance ToForm HostingLocation where
  toForm location =
    case location of
      Europe -> [("location", "Europe (EU)")]
      UnitedStates -> [("location", "United State (US)")]
      Asia -> [("location", "Asia")]


-- | Usage preferences form
-- data UsagePreferences = UsagePreferences
--   { uptimeMonitoring :: Bool
--   , errorTracking :: Bool
--   , performanceMonitoring :: Bool
--   , securityTesting :: Bool
--   }
--   deriving (Show, Eq)
--   -- Update the UsagePreferences data type:
-- data UsagePreferences = UsagePreferences
--   { uptimeMonitoring :: Bool
--   , errorTracking :: Bool
--   , performanceMonitoring :: Bool
--   , securityTesting :: Bool
--   }
--   deriving (Show, Eq, Generic)
--   deriving (ToJSON, FromJSON) via CustomJSON '[OmitNothingFields] UsagePreferences
--   deriving anyclass (FromForm)
data UsagePreferences = UsagePreferences
  { uptimeMonitoring :: Bool
  , errorTracking :: Bool
  , performanceMonitoring :: Bool
  , securityTesting :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] UsagePreferences
  deriving anyclass (FromForm)

-- | URL Monitor configuration
data URLMonitorConfig = URLMonitorConfig
  { urlMethod :: HTTPMethod
  , urlEndpoint :: Text
  , urlHeaders :: [(Text, Text)]
  , urlAssertions :: [URLAssertion]
  }
  deriving (Show, Eq)


-- | HTTP Methods
data HTTPMethod
  = GET
  | POST
  | PUT
  | DELETE
  deriving (Show, Eq)


-- | URL Monitor assertions
data URLAssertion = URLAssertion
  { assertionType :: AssertionType
  , assertionComparison :: AssertionComparison
  , assertionValue :: Text
  }
  deriving (Show, Eq)


data AssertionType
  = StatusCode
  | Header
  | ResponseBody
  deriving (Show, Eq)


data AssertionComparison
  = LessThan
  | EqualTo
  | GreaterThan
  deriving (Show, Eq)


-- | Framework integration preferences
data FrameworkIntegration = FrameworkIntegration
  { framework :: Framework
  , language :: ProgrammingLanguage
  }
  deriving (Show, Eq)


data Framework
  = Express
  | Django
  | Rails
  | Laravel
  | Native
  deriving (Show, Eq)


data ProgrammingLanguage
  = JavaScript
  | Python
  | Ruby
  | PHP
  | Go
  deriving (Show, Eq)


-- | Notification settings
data NotificationSettings = NotificationSettings
  { notificationChannels :: [NotificationChannel]
  , notificationEmails :: [Text]
  , notificationPhone :: Maybe Text
  }
  deriving (Show, Eq)


data NotificationChannel
  = SlackChannel
      { channelName :: Text
      , webhookUrl :: Text
      }
  | DiscordChannel
      { channelName :: Text
      , webhookUrl :: Text
      }
  deriving (Show, Eq)


-- | Team member invitation
newtype TeamInvitationList = TeamInvitationList [TeamMember]
  deriving (Show, Eq)


data TeamMember = TeamMember
  { memberEmail :: Text
  , memberRole :: TeamRole
  }
  deriving (Show, Eq)


data TeamRole
  = Admin
  | Member
  | Viewer
  deriving (Show, Eq)


-- | Pricing plan selection
data PricingPlan = PricingPlan
  { planRequestVolume :: Int -- requests per month
  , planFeatures :: [PlanFeature]
  }
  deriving (Show, Eq)


data PlanFeature
  = UnlimitedTeamMembers
  | DataRetentionDays Int
  | ApiTestingPipelines
  | SwaggerHosting
  | CustomMonitors
  | AIValidations
  deriving (Show, Eq)


-- | Helper type for form validation results
data FormValidationResult a
  = Valid a
  | Invalid [FormError]
  deriving (Show, Eq)


-- | Form validation error
data FormError = FormError
  { errorField :: Text
  , errorMessage :: Text
  }
  deriving (Show, Eq)


-- | Combined onboarding state
data OnboardingState = OnboardingState
  { onboardingSignup :: SignupForm
  , onboardingProfile :: Maybe ProfileForm
  , onboardingHosting :: Maybe HostingLocation
  , onboardingUsage :: Maybe UsagePreferences
  , onboardingMonitor :: Maybe URLMonitorConfig
  , onboardingIntegration :: Maybe FrameworkIntegration
  , onboardingNotifications :: Maybe NotificationSettings
  , onboardingTeam :: Maybe TeamInvitationList
  , onboardingPricing :: Maybe PricingPlan
  , onboardingCompleted :: Bool
  , onboardingLastUpdated :: UTCTime
  }
  deriving (Show)


-- Utility functions for form validation
validateEmail :: Text -> Maybe FormError
validateEmail email = undefined -- Implement email validation


validatePassword :: Text -> Maybe FormError
validatePassword pwd = undefined -- Implement password validation


validatePhoneNumber :: Text -> Maybe FormError
validatePhoneNumber phone = undefined -- Implement phone validation


-- Function to create a new onboarding state
initOnboardingState :: SignupForm -> UTCTime -> OnboardingState
initOnboardingState signup now =
  OnboardingState
    { onboardingSignup = signup
    , onboardingProfile = Nothing
    , onboardingHosting = Nothing
    , onboardingUsage = Nothing
    , onboardingMonitor = Nothing
    , onboardingIntegration = Nothing
    , onboardingNotifications = Nothing
    , onboardingTeam = Nothing
    , onboardingPricing = Nothing
    , onboardingCompleted = False
    , onboardingLastUpdated = now
    }


-- | Basic type instances
instance Default SignupForm where
  def = SignupForm "" ""


instance Default LoginForm where
  def = LoginForm "" ""


instance Default ProfileForm where
  def =
    ProfileForm
      { profileFirstName = ""
      , profileLastName = ""
      , profileCompanyName = ""
      , profileCompanySize = ""
      , profileReferralSource = "other"
      }


instance Default HostingLocation where
  def = Europe


instance Default UsagePreferences where
  def =
    UsagePreferences
      { uptimeMonitoring = False
      , errorTracking = False
      , performanceMonitoring = False
      , securityTesting = False
      }


instance Default URLMonitorConfig where
  def =
    URLMonitorConfig
      { urlMethod = GET
      , urlEndpoint = ""
      , urlHeaders = []
      , urlAssertions = []
      }


instance Default HTTPMethod where
  def = GET


instance Default URLAssertion where
  def =
    URLAssertion
      { assertionType = StatusCode
      , assertionComparison = EqualTo
      , assertionValue = ""
      }


instance Default AssertionType where
  def = StatusCode


instance Default AssertionComparison where
  def = EqualTo


instance Default FrameworkIntegration where
  def =
    FrameworkIntegration
      { framework = Express
      , language = JavaScript
      }


instance Default Framework where
  def = Express


instance Default ProgrammingLanguage where
  def = JavaScript


instance Default NotificationSettings where
  def =
    NotificationSettings
      { notificationChannels = []
      , notificationEmails = []
      , notificationPhone = Nothing
      }


instance Default NotificationChannel where
  def =
    SlackChannel
      { channelName = ""
      , webhookUrl = ""
      }


instance Default TeamInvitationList where
  def = TeamInvitationList []


instance Default TeamMember where
  def =
    TeamMember
      { memberEmail = ""
      , memberRole = Member
      }


instance Default TeamRole where
  def = Member


instance Default PricingPlan where
  def =
    PricingPlan
      { planRequestVolume = 0
      , planFeatures = []
      }


instance Default PlanFeature where
  def = UnlimitedTeamMembers


instance Default FormError where
  def =
    FormError
      { errorField = ""
      , errorMessage = ""
      }
