module Data.Effectful.GitPublication.Types (Stage (..), Failure (..), Authorization (..), MediaUrls (..), Receipt (..), Visibility (..), PullState (..)) where

import Data.Aeson qualified as AE
import Data.Effectful.CodeReview.Types (Revision)
import Pkg.DeriveUtils (UUIDId, WrappedEnumSC (..))
import Pkg.Git (PullState (..), Visibility (..))
import Relude


data Stage = Prepare | Discover | Branch | Create | AttachVideo | AttachDiagram | Complete
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" Stage


data Failure = InvalidRequest | IdentityChanged | AccessRevoked | PathChanged | NativeUnavailable | SourceChanged | VisibilityChanged | PullRequestChanged | UploadUncertain | StateChanged | GitHubFailure | PushFailed | NotConfigured | ProtocolFailure | DeadlineExceeded | InvalidReceipt
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Authorization = Authorization
  { projectId :: UUIDId "project"
  , runId :: UUIDId "issue_fix"
  , head :: Revision
  , source :: Text
  , mediaSha256 :: Text
  , branch :: Text
  , targetBranch :: Text
  , repo :: Text
  , visibility :: Visibility
  , description :: Text
  , stage :: Stage
  }
  deriving stock (Eq, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data MediaUrls = MediaUrls {video :: Text, diagram :: Text}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Receipt = Receipt {number :: Word64, url :: Text, head :: Revision, source :: Text, mediaSha256 :: Text, assets :: MediaUrls, draft :: Bool, state :: PullState}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
