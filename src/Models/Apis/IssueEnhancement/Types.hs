module Models.Apis.IssueEnhancement.Types (IssueEnhancement (..), IssueEnrichment (..)) where

import Data.Aeson qualified as AE
import Data.Time (UTCTime)
import Pkg.DeriveUtils (UUIDId)
import Relude


data IssueEnhancement = IssueEnhancement
  { issueId :: UUIDId "issue"
  , enhancedTitle :: Text
  , enhancedDescription :: Text
  , recommendedAction :: Text
  , migrationComplexity :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data IssueEnrichment = IssueEnrichment
  { enhancement :: IssueEnhancement
  , classification :: Maybe (Bool, Natural, Natural)
  , analysis :: Maybe (Text, Text)
  , analyzedAt :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
