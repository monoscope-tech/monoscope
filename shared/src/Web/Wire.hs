-- | Wire types shared verbatim between the server and the @monoscope-cli@
-- package. They live here — away from "Web.ApiTypes" / "Web.Auth" /
-- "Pkg.SchemaLearning.Catalog", each of which drags in the DB stack — so the
-- CLI can compile the same declarations without linking libpq. The owning
-- modules re-export them, so server-side call sites are unchanged.
module Web.Wire (
  ApiProblem (..),
  ApiProblemBody (..),
  ProblemCode (..),
  DeviceCodeResponse (..),
  DeviceTokenResponse (..),
  FacetValue (..),
  Paged (..),
  ProjectInfo (..),
  IngestionKey (..),
) where

import Data.Aeson qualified as AE
import Data.OpenApi (ToSchema)
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAES
import Pkg.Deriving (CamelSchema (..), JsonValueSchema (..), WrappedEnumSC (..))
import Relude


-- | Why an @/api/v1@ request was refused. The snake_case tag (@not_found@, @unknown_field@, …)
-- is the wire contract the CLI and agents branch on, so renaming a constructor is a breaking change.
data ProblemCode
  = PNotFound
  | PNoActiveIngestionKey
  | PUserNotCreated
  | PUnknownBulkAction
  | PNonPositiveTimeWindow
  | PRequired
  | PInvalidValue
  | PEveryoneTeamNotUpdatable
  | PEveryoneTeamNotDeletable
  | PReservedHandle
  | PHandleInUse
  | PMemberIdentityMissing
  | PAuthenticationRequired
  | PProjectInactive
  | PUnauthorized
  | PInvalidRequest
  | PInvalidQuery
  | PUnknownField
  | PQueryFailed
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via WrappedEnumSC 'Nothing "P" ProblemCode


-- | The body of every @/api/v1@ refusal.
data ApiProblem = ApiProblem
  { code :: ProblemCode
  , message :: Text
  , field :: Maybe Text
  , suggestion :: Maybe Text
  , details :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving (ToSchema) via CamelSchema ApiProblem
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] ApiProblem


-- | Wire shape: @{"error": ApiProblem}@.
newtype ApiProblemBody = ApiProblemBody {error :: ApiProblem}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, ToSchema)


newtype IngestionKey = IngestionKey {key :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (ToSchema) via JsonValueSchema IngestionKey


-- | Envelope every paginated @/api/v1@ listing returns.
data Paged a = Paged
  { items :: [a]
  , totalCount :: Int
  , page :: Int
  , perPage :: Int
  , hasMore :: Bool
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake (Paged a)
  deriving (ToSchema) via JsonValueSchema (Paged a)


-- | One value of a faceted field plus how often it was seen.
data FacetValue = FacetValue
  { value :: Text
  , count :: Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake FacetValue


-- OAuth-style device authorization grant, used by @monoscope auth login@.

data DeviceCodeResponse = DeviceCodeResponse
  { deviceCode :: Text
  , userCode :: Text
  , verificationUri :: Text
  , expiresIn :: Int
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake DeviceCodeResponse


data ProjectInfo = ProjectInfo {id :: Text, name :: Text}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] ProjectInfo


data DeviceTokenResponse = DeviceTokenResponse
  { sessionId :: Maybe Text
  , projects :: Maybe [ProjectInfo]
  , err :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.Rename "err" "error"]] DeviceTokenResponse
