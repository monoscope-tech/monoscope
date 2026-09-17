module Models.Projects.Activation (
  ActivationMilestone (..),
  recordActivationMilestone,
) where

import Data.Effectful.Hasql qualified as Hasql
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (DB, WrappedEnumSC (..))
import Relude


-- | A durable, project-level activation funnel. These values deliberately contain no user,
-- device, endpoint, or query information: the funnel answers whether a project reached value,
-- not who did it or what they observed.
data ActivationMilestone
  = IngestVerified
  | DashboardCreated
  | MonitorCreated
  | NotificationTestSent
  deriving stock (Bounded, Enum, Eq, Show)
  deriving (HI.EncodeValue) via WrappedEnumSC 'Nothing "" ActivationMilestone


-- | First occurrence wins. Repeating a successful notification test should improve operator
-- confidence, but it must not inflate the funnel's project count or reset its elapsed time.
recordActivationMilestone :: DB es => Projects.ProjectId -> ActivationMilestone -> Eff es ()
recordActivationMilestone projectId milestone =
  void
    $ Hasql.interpExecute
      [HI.sql|
        INSERT INTO projects.activation_milestones (project_id, milestone)
        VALUES (#{projectId}, #{milestone})
        ON CONFLICT (project_id, milestone) DO NOTHING
      |]
