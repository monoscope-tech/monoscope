module Models.Projects.Activation (
  ActivationMilestone (..),
  recordActivationMilestone,
  recordActivationMilestones,
) where

import Data.Effectful.Hasql qualified as Hasql
import Data.Time (UTCTime)
import Data.Vector qualified as V
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
recordActivationMilestone projectId milestone = void $ recordActivationMilestones milestone Nothing [projectId]


-- | Batch form, returning the projects that reached the milestone in this call. @createdAfter@
-- limits it to projects created since then, so a caller sweeping every active project records
-- (and reports as ad conversions) new projects only, not years of existing ones.
recordActivationMilestones :: DB es => ActivationMilestone -> Maybe UTCTime -> V.Vector Projects.ProjectId -> Eff es [Projects.ProjectId]
recordActivationMilestones milestone createdAfter projectIds =
  Hasql.interp
    [HI.sql|
      INSERT INTO projects.activation_milestones (project_id, milestone)
      SELECT id, #{milestone} FROM projects.projects
      WHERE id = ANY(#{projectIds}::uuid[]) AND (#{createdAfter}::timestamptz IS NULL OR created_at > #{createdAfter})
      ON CONFLICT (project_id, milestone) DO NOTHING
      RETURNING project_id
    |]
