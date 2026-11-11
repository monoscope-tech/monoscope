-- | The typed ids most modules need, kept out of "Models.Projects.Projects" so that
-- editing it does not recompile the ~40 modules that use nothing else from it.
module Models.Ids (ProjectId, UserId, pattern UserId) where

import Data.UUID qualified as UUID
import Pkg.Deriving (UUIDId (..))


type ProjectId = UUIDId "project"


-- | Aliased to 'UUIDId' rather than given its own newtype so it inherits the shared
-- encode/decode/schema instances and the @.toText@/@.unwrap@ accessors.
type UserId = UUIDId "user"


pattern UserId :: UUID.UUID -> UserId
pattern UserId u = UUIDId u


{-# COMPLETE UserId #-}
