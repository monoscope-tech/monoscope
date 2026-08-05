-- | Wire types shared verbatim between the server and the @monoscope-cli@
-- package. They live here — away from "Web.ApiTypes" / "Web.Auth" /
-- "Pkg.SchemaLearning.Catalog", each of which drags in the DB stack — so the
-- CLI can compile the same declarations without linking libpq. The owning
-- modules re-export them, so server-side call sites are unchanged.
module Web.Wire (
  DeviceCodeResponse (..),
  DeviceTokenResponse (..),
  FacetValue (..),
  Paged (..),
  ProjectInfo (..),
) where

import Data.Aeson qualified as AE
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAES
import Relude


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
