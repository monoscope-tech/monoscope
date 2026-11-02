module Pkg.AgentOperation (AgentAwaitingReview (..), OperationCall (..), NativeOperation (..), WikiWrite (..), ReviewComment (..), sameOperation) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Time (UTCTime)
import Deriving.Aeson qualified as DAE
import Langchain.LLM.Core qualified as LLM
import Models.Apis.IssueEnhancement.Types qualified as Enhancement
import Models.Projects.ProjectMemory qualified as Memory
import Pkg.DeriveUtils (UUIDId)
import Relude


-- $setup
-- >>> import Data.Aeson qualified as AE
-- >>> import Data.Either (isLeft)
-- >>> import Data.Map.Strict qualified as Map
-- >>> import Langchain.LLM.Core qualified as LLM
-- >>> let call = LLM.ToolCall "legacy" "function" (LLM.ToolFunction "save_artifact" Map.empty)


data AgentAwaitingReview = AgentAwaitingReview (UUIDId "agent_operation") UTCTime
  deriving stock (Show)
  deriving anyclass (Exception)


data WikiWrite = WikiWrite {attemptId :: UUIDId "agent_attempt", edit :: Memory.Edit}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data ReviewComment = ReviewComment {reviewId :: UUIDId "pr_review", body :: Text}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data NativeOperation = WriteWiki WikiWrite | PublishPullRequestComment ReviewComment | EnrichIssue Enhancement.IssueEnrichment
  deriving stock (Eq, Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.TagSingleConstructors] NativeOperation


data OperationCall = ModelCall LLM.ToolCall | NativeCall NativeOperation
  deriving stock (Eq, Generic, Show)


-- | Model proposals keep their historical wire format; native calls carry a tag.
--
-- >>> let legacy = "{\"id\":\"legacy\",\"type\":\"function\",\"function\":{\"name\":\"save_artifact\",\"arguments\":{}}}"
-- >>> AE.eitherDecode @OperationCall legacy == Right (ModelCall call)
-- True
-- >>> AE.eitherDecode @AE.Value (AE.encode (ModelCall call)) == AE.eitherDecode @AE.Value legacy
-- True
-- >>> isLeft (AE.eitherDecode @OperationCall "{\"tag\":\"UnsupportedNativeCall\"}")
-- True
instance AE.ToJSON OperationCall where
  toJSON (ModelCall call) = AE.toJSON call
  toJSON call@NativeCall{} = AE.genericToJSON AE.defaultOptions call


instance AE.FromJSON OperationCall where
  parseJSON value@(AE.Object fields)
    | not (KM.member "tag" fields) = ModelCall <$> AE.parseJSON value
  parseJSON value = AE.genericParseJSON AE.defaultOptions value


-- Transport call IDs and worker attempts do not create another domain action.
sameOperation :: OperationCall -> OperationCall -> Bool
sameOperation (ModelCall left) (ModelCall right) = left.toolCallFunction == right.toolCallFunction
sameOperation (NativeCall (WriteWiki left)) (NativeCall (WriteWiki right)) = left.edit == right.edit
sameOperation (NativeCall left) (NativeCall right) = left == right
sameOperation _ _ = False
