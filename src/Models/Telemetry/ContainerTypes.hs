module Models.Telemetry.ContainerTypes (
  ContainerRow (..),
  Scope (..),
  Runtime (..),
  ContainerSnapshotKey,
) where

import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (WrappedEnumSC (..))
import Relude


data ContainerRow = ContainerRow
  { containerName :: Text
  , scope :: Scope
  , podName :: Maybe Text
  , namespace :: Maybe Text
  , nodeName :: Maybe Text
  , cluster :: Maybe Text
  , provider :: Maybe Text
  , region :: Maybe Text
  , osType :: Maybe Text
  , architecture :: Maybe Text
  , image :: Maybe Text
  , imageTag :: Maybe Text
  , workload :: Maybe Text
  , cpuCores :: Maybe Double
  , cpuLimit :: Maybe Double
  , cpuRequest :: Maybe Double
  , memBytes :: Maybe Double
  , memLimit :: Maybe Double
  , memRequest :: Maybe Double
  , load1 :: Maybe Double
  , storagePct :: Maybe Double
  , uptime :: Maybe Double
  , restarts :: Maybe Double
  , ready :: Maybe Double
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (HI.DecodeRow, NFData)


data Scope = ScopeContainer | ScopePod | ScopeHost
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass (NFData)
  deriving (HI.DecodeValue) via WrappedEnumSC 'Nothing "Scope" Scope


data Runtime = Kubernetes | Docker | Host
  deriving stock (Bounded, Enum, Eq, Ord, Show)


-- | The resolved URL time parameters identify one short-lived infrastructure snapshot. Relative
-- ranges use their canonical query value rather than a moving timestamp so default pages, sibling
-- links and drawers reuse the same read while a user navigates between them.
type ContainerSnapshotKey = (Projects.ProjectId, Maybe Text, Maybe Text, Maybe Text)
