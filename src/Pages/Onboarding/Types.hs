module Pages.Onboarding.Types where

import Data.Aeson (FromJSON, ToJSON, withText)
import Data.Default (Default (..), def)
import Data.Either (either)
import Data.List (concatMap, lookup, zip, zip3)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Traversable (traverse)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Web.FormUrlEncoded (
  FromForm (fromForm),
  ToForm (toForm),
  lookupAll,
  lookupUnique,
  toListStable,
 )

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Aeson.Types (ToJSON (toJSON))
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid (mconcat)
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace (traceShowM)
import Deriving.Aeson qualified as DA
import GHC.Base
import GHC.Show (Show)
import Gogol.Data.JSON (FromJSON (parseJSON))
import Relude.String (ToString (..))
import Text.Read (readMaybe)
import Web.HttpApiData (FromHttpApiData)
import Web.Internal.FormUrlEncoded (parseUnique)
import Web.Internal.HttpApiData (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))
import Prelude (Either (..))


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


-- | Company size ranges
data CompanySize
  = Size1To2
  | Size2To10
  | Size10To50
  | Size50Plus
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] CompanySize


-- Add FromHttpApiData instance for CompanySize
instance FromHttpApiData CompanySize where
  parseUrlPiece text = case text of
    "1-2" -> Right Size1To2
    "2-10" -> Right Size2To10
    "10-50" -> Right Size10To50
    "50+" -> Right Size50Plus
    _ -> Left "Invalid company size. Must be one of: 1-2, 2-10, 10-50, 50+"


-- Add ToHttpApiData instance for CompanySize
instance ToHttpApiData CompanySize where
  toUrlPiece size = case size of
    Size1To2 -> "1-2"
    Size2To10 -> "2-10"
    Size10To50 -> "10-50"
    Size50Plus -> "50+"


-- | Profile/Company details form
data ProfileForm = ProfileForm
  { profileFirstName :: Text
  , profileLastName :: Text
  , profileCompanyName :: Text
  , profileCompanySize :: CompanySize
  , profileReferralSource :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)
  deriving
    (ToJSON, FromJSON)
    via DA.CustomJSON '[DA.OmitNothingFields] ProfileForm


-- | How the user heard about the service
data ReferralSource
  = Google
  | SocialMedia
  | FriendColleague
  | Other Text
  deriving (Show, Eq)


instance FromForm ReferralSource where
  fromForm form = do
    source <- lookupUnique "referralSource" form
    case source of
      "google" -> pure Google
      "social_media" -> pure SocialMedia
      "friend_colleague" -> pure FriendColleague
      other -> pure (Other other)


-- | Data hosting location selection
data HostingLocation
  = Europe
  | UnitedStates
  | Asia
  deriving (Show, Eq, Generic)
  deriving
    (ToJSON, FromJSON)
    via DA.CustomJSON '[DA.OmitNothingFields] HostingLocation


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
data UsagePreferences = UsagePreferences
  { uptimeMonitoring :: Bool
  , errorTracking :: Bool
  , performanceMonitoring :: Bool
  , securityTesting :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] UsagePreferences


instance FromForm UsagePreferences where
  fromForm form = do
    traceShowM ("Form data received:", form)

    let getBoolValue name = case Data.List.lookup name (Web.FormUrlEncoded.toListStable form) of
          Just "true" -> True
          _ -> False

    pure
      UsagePreferences
        { uptimeMonitoring = getBoolValue "uptimeMonitoring"
        , errorTracking = getBoolValue "errorTracking"
        , performanceMonitoring = getBoolValue "performanceMonitoring"
        , securityTesting = getBoolValue "securityTesting"
        }


-- | URL Monitor configuration
-- data URLMonitorConfig = URLMonitorConfig
--   { urlMethod :: HTTPMethod
--   , urlEndpoint :: Text
--   , urlHeaders :: [(Text, Text)]
--   , urlAssertions :: [URLAssertion]
--   }
--   deriving (Show, Eq, Generic)
--   deriving
--     (ToJSON, FromJSON)
--     via DA.CustomJSON '[DA.OmitNothingFields] URLMonitorConfig
newtype URLMonitorConfig = URLMonitorConfig
  { monitorTested :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving
    (ToJSON, FromJSON)
    via DA.CustomJSON '[DA.OmitNothingFields] URLMonitorConfig


-- | HTTP Methods
data HTTPMethod
  = GET
  | POST
  | PUT
  | PATCH
  | DELETE
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] HTTPMethod


-- | URL Monitor assertions
data URLAssertion = URLAssertion
  { assertionType :: AssertionType
  , assertionComparison :: AssertionComparison
  , assertionValue :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] URLAssertion


data AssertionType
  = StatusCode
  | Header
  | ResponseBody
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] AssertionType


data AssertionComparison
  = LessThan
  | EqualTo
  | GreaterThan
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] AssertionComparison


-- | Framework integration preferences
data FrameworkIntegration = FrameworkIntegration
  { framework :: Framework
  , language :: ProgrammingLanguage
  }
  deriving (Show, Eq)


--   deriving stock (Generic)
--   deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] FrameworkIntegration

-- instance ToJSON Framework where
--   toJSON = \case
--     Express -> "express"
--     Django -> "django"
--     Rails -> "rails"
--     Laravel -> "laravel"
--     Native -> "native"

-- instance FromJSON Framework where
--   parseJSON = withText "Framework" $ \case
--     "express" -> pure Express
--     "django" -> pure Django
--     "rails" -> pure Rails
--     "laravel" -> pure Laravel
--     "native" -> pure Native
--     invalid -> Left $ "Invalid framework: " <> show invalid <> ". Expected one of: express, django, rails, laravel, native"

-- instance ToJSON ProgrammingLanguage where
--   toJSON = \case
--     JavaScript -> "javascript"
--     Python -> "python"
--     Ruby -> "ruby"
--     PHP -> "php"
--     Go -> "go"

-- instance FromJSON ProgrammingLanguage where
--   parseJSON = withText "ProgrammingLanguage" $ \case
--     "javascript" -> pure JavaScript
--     "python" -> pure Python
--     "ruby" -> pure Ruby
--     "php" -> pure PHP
--     "go" -> pure Go
--     invalid -> Left $ "Invalid programming language: " <> show invalid <> ". Expected one of: javascript, python, ruby, php, go"

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


-- | Notification settings configuration
data NotificationSettings = NotificationSettings
  { notificationChannels :: [NotificationChannel]
  , notificationEmails :: [Text]
  , notificationPhone :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] NotificationSettings


-- | Supported notification channels
data NotificationChannel
  = SlackChannel
      { channelName :: Text
      , webhookUrl :: Text
      }
  | DiscordChannel
      { channelName :: Text
      , webhookUrl :: Text
      }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] NotificationChannel


-- | Parse channel type from form data
parseChannelType :: Text -> Either Text (Text -> Text -> NotificationChannel)
parseChannelType channelType = case T.toLower channelType of
  "slack" -> Right SlackChannel
  "discord" -> Right DiscordChannel
  _ -> Left "Invalid channel type. Must be either 'slack' or 'discord'"


-- Form handling instances
instance FromForm NotificationChannel where
  fromForm f = do
    channelType <- parseUnique "type" f
    name <- parseUnique "channelName" f
    webhook <- parseUnique "webhookUrl" f
    constructor <- either throwError pure $ parseChannelType channelType
    pure $ constructor name webhook


instance FromForm NotificationSettings where
  fromForm f = do
    -- Parse notification channels
    let channelTypes = lookupAll "type" f
        channelNames = lookupAll "channelName" f
        webhookUrls = lookupAll "webhookUrl" f

    -- Zip channel data together
    let channelData =
          zip3
            (toList channelTypes)
            (toList channelNames)
            (toList webhookUrls)

    -- Parse channels
    channels <- traverse parseChannel channelData

    -- Get emails and phone
    let emails = toList $ lookupAll "notificationEmail" f
    let phoneNumbers = toList $ lookupAll "notificationPhone" f
    let phone = case phoneNumbers of
          (p : _) -> Just p
          [] -> Nothing

    pure
      NotificationSettings
        { notificationChannels = channels
        , notificationEmails = emails
        , notificationPhone = phone
        }
    where
      parseChannel (typ, name, webhook) = do
        constructor <- either throwError pure $ parseChannelType typ
        pure $ constructor name webhook


instance ToForm NotificationChannel where
  toForm channel = case channel of
    SlackChannel name url ->
      [ ("type", "slack")
      , ("channelName", name)
      , ("webhookUrl", url)
      ]
    DiscordChannel name url ->
      [ ("type", "discord")
      , ("channelName", name)
      , ("webhookUrl", url)
      ]


instance ToForm NotificationSettings where
  toForm NotificationSettings{..} =
    mconcat
      [ mconcat (map toForm notificationChannels)
      , mconcat (map (\email -> [("notificationEmail", email)]) notificationEmails)
      , maybe [] (\phone -> [("notificationPhone", phone)]) notificationPhone
      ]


-- | Team member invitation list
newtype TeamInvitationList = TeamInvitationList [TeamMember]
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] TeamInvitationList


-- | Team member data
data TeamMember = TeamMember
  { memberEmail :: Text
  , memberRole :: TeamRole
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] TeamMember


-- | Team role types
data TeamRole
  = Admin
  | Member
  | Viewer
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] TeamRole


-- | Parse TeamRole from Text
parseTeamRole :: Text -> Either Text TeamRole
parseTeamRole role = case T.toLower role of
  "admin" -> Right Admin
  "member" -> Right Member
  "viewer" -> Right Viewer
  _ -> Left "Invalid team role"


-- | Convert TeamRole to Text
teamRoleToText :: TeamRole -> Text
teamRoleToText Admin = "admin"
teamRoleToText Member = "member"
teamRoleToText Viewer = "viewer"


-- Form instances
instance FromForm TeamRole where
  fromForm f = do
    roleText <- parseUnique "role" f
    case parseTeamRole roleText of
      Right role -> pure role
      Left err -> throwError err


instance FromForm TeamMember where
  fromForm f =
    TeamMember
      <$> parseUnique "email" f
      <*> fromForm f


instance FromForm TeamInvitationList where
  fromForm f = do
    -- Get email and role values and convert to lists
    let emails = toList $ lookupAll "email" f
    let roles = toList $ lookupAll "role" f

    -- Parse each member
    members <- traverse parseMemberPair (zip emails roles)
    pure $ TeamInvitationList members
    where
      parseMemberPair (email, roleText) =
        case parseTeamRole roleText of
          Right role -> Right $ TeamMember email role
          Left err -> throwError err


instance ToForm TeamRole where
  toForm role = [(("role"), teamRoleToText role)]


instance ToForm TeamMember where
  toForm TeamMember{..} =
    [("email", memberEmail), ("role", teamRoleToText memberRole)]


instance ToForm TeamInvitationList where
  toForm (TeamInvitationList members) = mconcat (map toForm members)


-- | Pricing plan selection
data PricingPlan = PricingPlan
  { planRequestVolume :: Int -- requests per month
  , planFeatures :: [PlanFeature]
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] PricingPlan


data PlanFeature
  = UnlimitedTeamMembers
  | DataRetentionDays Int
  | ApiTestingPipelines
  | SwaggerHosting
  | CustomMonitors
  | AIValidations
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via DA.CustomJSON '[DA.OmitNothingFields] PlanFeature


-- Convert form feature string to PlanFeature
parsePlanFeature :: Text -> Maybe PlanFeature
parsePlanFeature feature = case feature of
  "unlimited_members" -> Just UnlimitedTeamMembers
  "data_retention" -> Just (DataRetentionDays 14) -- Default to 14 days
  "api_testing" -> Just ApiTestingPipelines
  "swagger_hosting" -> Just SwaggerHosting
  "custom_monitors" -> Just CustomMonitors
  "ai_validations" -> Just AIValidations
  _ -> Nothing


-- Parse text to Int for form handling
parseIntFromText :: Text -> Either Text Int
parseIntFromText t = case readMaybe (toString t) of
  Just n -> Right n
  Nothing -> Left "Invalid number format"


instance FromForm PricingPlan where
  fromForm form = do
    traceShowM form
    -- Parse request volume (required)
    volumeText <- lookupUnique "requestVolume" form
    volume <- parseIntFromText volumeText

    -- Parse planFeatures as a list of feature strings
    let featureTexts = lookupAll "planFeatures" form
    let features = mapMaybe parsePlanFeature featureTexts

    pure
      PricingPlan
        { planRequestVolume = volume
        , planFeatures = features
        }


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


-- | All Type Default instances
instance Default SignupForm where
  def = SignupForm "" ""


instance Default LoginForm where
  def = LoginForm "" ""


-- Convert form data to CompanySize
instance FromForm CompanySize where
  fromForm form = do
    sizeStr <- lookupUnique "profileCompanySize" form
    case sizeStr of
      "1-2" -> Right Size1To2
      "2-10" -> Right Size2To10
      "10-50" -> Right Size10To50
      "50+" -> Right Size50Plus
      _ -> Left "Invalid company size. Must be one of: 1-2, 2-10, 10-50, 50+"


instance Default ProfileForm where
  def =
    ProfileForm
      { profileFirstName = ""
      , profileLastName = ""
      , profileCompanyName = ""
      , profileCompanySize = Size1To2
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


-- instance FromForm URLMonitorConfig where
--   fromForm form = do
--     let tested = fromMaybe False $ do
--           val <- lookupUnique "monitor_tested" form
--           case val of
--             "true" -> Just True
--             _ -> Just False

--     pure URLMonitorConfig
--       { monitorTested = tested
--       }
instance FromForm URLMonitorConfig where
  fromForm form = do
    tested <- case lookupUnique "monitor_tested" form of
      Right val -> Right $ case val of
        "true" -> True
        _ -> False
      Left _ -> Right False

    pure
      URLMonitorConfig
        { monitorTested = tested
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
      { framework = Native
      , language = Go
      }


instance Default Framework where
  def = Express


instance Default ProgrammingLanguage where
  def = JavaScript


instance Default NotificationSettings where
  def =
    NotificationSettings
      { notificationChannels = []
      , notificationEmails = ["exodustimthy@gmail.com", "exodustimthy@gmail.com", "gethasalondarea...", "gethasalondared@gmail.com", "docterman223@gmail.com"]
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
      , planFeatures =
          [ UnlimitedTeamMembers
          , DataRetentionDays 14
          , ApiTestingPipelines
          , SwaggerHosting
          , CustomMonitors
          , AIValidations
          ]
      }


instance Default URLMonitorConfig where
  def =
    URLMonitorConfig
      { monitorTested = False
      }


instance Default PlanFeature where
  def = UnlimitedTeamMembers


instance Default FormError where
  def =
    FormError
      { errorField = ""
      , errorMessage = ""
      }
