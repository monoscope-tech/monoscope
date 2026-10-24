module Models.Projects.AgentUsage (Run (..), Scope (..), startAttempt, ownsAttempt, recordCall, recordUsage, recordAllocation, recordCheck, hasCheck, UsageCost (..), ResourceUsage (..), UsageTotals (..), ProjectUsage (..), usageTotals, customerUsage) where

import Data.Aeson qualified as AE
import Data.Effectful.CodeReview.Types qualified as Agent
import Data.Effectful.Hasql qualified as Hasql
import Data.Scientific (Scientific)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Effectful (Eff)
import GHC.Generics (Generically (..))
import Hasql.Interpolate qualified as HI
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects (ProjectId)
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId)
import Relude
import System.Types (DB)


data Run kind where
  Review :: UUIDId "pr_review" -> Run "pr_review"
  Fix :: UUIDId "issue_fix" -> Run "issue_fix"


data Scope kind where
  Reviews :: Scope "pr_review"
  Fixes :: Scope "issue_fix"


targetIds :: Run kind -> (Maybe (UUIDId "pr_review"), Maybe (UUIDId "issue_fix"))
targetIds = \case Review rid -> (Just rid, Nothing); Fix fid -> (Nothing, Just fid)


-- Pin billing before launching a worker, including attempts that return no usage.
startAttempt :: DB es => ProjectId -> Run kind -> Text -> Text -> Eff es (Maybe (UUIDId "agent_attempt"))
startAttempt pid target provider modelSpec = do
  let (rid, fid) = targetIds target
  Hasql.interpOne
    [HI.sql|
    INSERT INTO projects.pr_review_attempts (run_id, fix_run_id, project_id, billing_provider, customer_id, provider, model_spec)
    SELECT #{rid}, #{fid}, p.id, p.billing_provider, p.customer_id, #{provider}, #{modelSpec}
    FROM projects.projects p WHERE p.id = #{pid} AND p.deleted_at IS NULL AND (
      EXISTS (SELECT 1 FROM projects.pr_review_runs r JOIN projects.pr_review_threads t ON t.id = r.thread_id WHERE r.id = #{rid} AND t.project_id = p.id)
      OR EXISTS (SELECT 1 FROM projects.issue_fix_runs f WHERE f.id = #{fid} AND f.project_id = p.id))
    RETURNING id
  |]


recordCall :: DB es => ProjectId -> Run kind -> Agent.ModelCall -> Eff es Bool
recordCall pid target call = do
  let (rid, fid) = targetIds target
  fromMaybe False
    <$> Hasql.interpOne
      [HI.sql|
    INSERT INTO projects.agent_model_calls (id, attempt_id, receipt)
    SELECT #{call.id}, a.id, #{Aeson call} FROM projects.pr_review_attempts a
    WHERE a.id = #{call.attemptId} AND a.project_id = #{pid} AND a.provider = #{call.provider}
      AND a.run_id IS NOT DISTINCT FROM #{rid} AND a.fix_run_id IS NOT DISTINCT FROM #{fid}
      AND length(#{call.model}) BETWEEN 1 AND 256
    ON CONFLICT (id) DO UPDATE SET id = EXCLUDED.id
      WHERE agent_model_calls.attempt_id = EXCLUDED.attempt_id AND agent_model_calls.receipt = EXCLUDED.receipt
    RETURNING TRUE
  |]


-- Estimates are deliberately separate from billable AI usage. Unknown attempts
-- are accepted only for legacy reviews, with explicit receipt-time attribution.
recordUsage :: DB es => ProjectId -> Run kind -> Agent.ModelUsage -> Eff es Bool
recordUsage pid target usage = do
  let (rid, fid) = targetIds target
      amount = fromIntegral <$> usage.costNanousd :: Maybe Scientific
  fromMaybe False
    <$> Hasql.interpOne
      [HI.sql|
    INSERT INTO projects.pr_review_usage (id, run_id, fix_run_id, project_id, billing_provider, customer_id, receipt, cost_nanousd, attempt_id, model_call_id)
    SELECT #{usage.id}, #{rid}, #{fid}, p.id, CASE WHEN a.id IS NULL THEN p.billing_provider ELSE a.billing_provider END,
      CASE WHEN a.id IS NULL THEN p.customer_id ELSE a.customer_id END, #{Aeson $ AE.toJSON usage}, #{amount}, a.id, #{usage.callId}
    FROM projects.projects p LEFT JOIN projects.pr_review_attempts a ON a.id = #{usage.attemptId}
    WHERE p.id = #{pid} AND (
      EXISTS (SELECT 1 FROM projects.pr_review_runs r JOIN projects.pr_review_threads t ON t.id = r.thread_id WHERE r.id = #{rid} AND t.project_id = p.id)
      OR EXISTS (SELECT 1 FROM projects.issue_fix_runs f WHERE f.id = #{fid} AND f.project_id = p.id))
      AND ((a.id IS NULL AND #{rid} IS NOT NULL) OR (a.run_id IS NOT DISTINCT FROM #{rid} AND a.fix_run_id IS NOT DISTINCT FROM #{fid} AND a.project_id = p.id AND a.provider = #{usage.provider}))
      AND (#{usage.callId} IS NOT NULL
        OR NOT EXISTS (SELECT 1 FROM projects.agent_model_calls c WHERE c.attempt_id = a.id)
        OR EXISTS (SELECT 1 FROM projects.pr_review_usage u WHERE u.id = #{usage.id}))
      AND (#{usage.callId} IS NULL OR EXISTS (SELECT 1 FROM projects.agent_model_calls c
        WHERE c.id = #{usage.callId} AND c.attempt_id = a.id AND c.receipt->>'provider' = #{usage.provider}))
      AND NOT EXISTS (SELECT 1 FROM projects.pr_review_usage u WHERE u.model_call_id = #{usage.callId} AND u.id <> #{usage.id})
    ON CONFLICT (id) DO UPDATE SET id = EXCLUDED.id
      WHERE pr_review_usage.run_id IS NOT DISTINCT FROM EXCLUDED.run_id
        AND pr_review_usage.fix_run_id IS NOT DISTINCT FROM EXCLUDED.fix_run_id
        AND pr_review_usage.project_id = EXCLUDED.project_id AND ('{"callId":null}'::jsonb || pr_review_usage.receipt) = EXCLUDED.receipt
    RETURNING TRUE
  |]


recordAllocation :: DB es => ProjectId -> Run kind -> Agent.AllocationReceipt -> Eff es Bool
recordAllocation pid target receipt = do
  let (rid, fid) = targetIds target
      allocation = Agent.allocationId receipt
      attempt = Agent.allocationAttempt receipt
  fromMaybe False
    <$> Hasql.interpOne
      [HI.sql|
    INSERT INTO projects.agent_allocations (id, attempt_id, receipt)
    SELECT #{allocation}, a.id, #{Aeson receipt} FROM projects.pr_review_attempts a
    WHERE a.id = #{attempt} AND a.project_id = #{pid}
      AND a.run_id IS NOT DISTINCT FROM #{rid} AND a.fix_run_id IS NOT DISTINCT FROM #{fid}
    ON CONFLICT (id) DO UPDATE SET id = EXCLUDED.id
      WHERE agent_allocations.attempt_id = EXCLUDED.attempt_id AND agent_allocations.receipt = EXCLUDED.receipt
    RETURNING TRUE
  |]


recordCheck :: DB es => ProjectId -> Run kind -> Agent.CheckReceipt -> Eff es Bool
recordCheck pid target receipt = do
  let (rid, fid) = targetIds target
  fromMaybe False
    <$> Hasql.interpOne
      [HI.sql|
    INSERT INTO projects.pr_review_checks (id, run_id, fix_run_id, project_id, billing_provider, customer_id, receipt, attempt_id)
    SELECT #{receipt.id}, #{rid}, #{fid}, p.id, CASE WHEN a.id IS NULL THEN p.billing_provider ELSE a.billing_provider END,
      CASE WHEN a.id IS NULL THEN p.customer_id ELSE a.customer_id END, #{Aeson $ AE.toJSON receipt}, a.id
    FROM projects.projects p LEFT JOIN projects.pr_review_attempts a ON a.id = #{receipt.attemptId}
    WHERE p.id = #{pid} AND (
      EXISTS (SELECT 1 FROM projects.pr_review_runs r JOIN projects.pr_review_threads t ON t.id = r.thread_id WHERE r.id = #{rid} AND t.project_id = p.id)
      OR EXISTS (SELECT 1 FROM projects.issue_fix_runs f WHERE f.id = #{fid} AND f.project_id = p.id))
      AND ((a.id IS NULL AND #{rid} IS NOT NULL) OR (a.run_id IS NOT DISTINCT FROM #{rid} AND a.fix_run_id IS NOT DISTINCT FROM #{fid} AND a.project_id = p.id))
    ON CONFLICT (id) DO UPDATE SET id = EXCLUDED.id
      WHERE pr_review_checks.run_id IS NOT DISTINCT FROM EXCLUDED.run_id
        AND pr_review_checks.fix_run_id IS NOT DISTINCT FROM EXCLUDED.fix_run_id
        AND pr_review_checks.project_id = EXCLUDED.project_id
        AND ('{"regression":null,"workspace":null}'::jsonb || pr_review_checks.receipt) = EXCLUDED.receipt
    RETURNING TRUE
  |]


data UsageCost = UsageCost {calls :: Sum Int64, failedCalls :: Sum Int64, unpricedCalls :: Sum Int64, lateAttributedCalls :: Sum Int64, unreportedAttempts :: Sum Int64, unreportedCalls :: Sum Int64, costNanousd :: Sum Scientific}
  deriving stock (Generic, Show)
  deriving (Semigroup) via Generically UsageCost
  deriving (HI.DecodeValue) via HI.CompositeValue UsageCost


data ResourceUsage = ResourceUsage {checks :: Sum Int64, vcpuMs :: Sum Scientific, memoryMiBMs :: Sum Scientific, logBytes :: Sum Scientific, explanations :: Sum Int64, renderMs :: Sum Scientific, mediaBytes :: Sum Scientific, allocations :: Sum Int64, cleanupFailures :: Sum Int64, unreportedAllocations :: Sum Int64}
  deriving stock (Generic, Show)
  deriving (Semigroup) via Generically ResourceUsage
  deriving (HI.DecodeValue) via HI.CompositeValue ResourceUsage


data UsageTotals kind = UsageTotals {runId :: Maybe (UUIDId kind), usage :: UsageCost, resources :: ResourceUsage}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


usageTotals :: DB es => Scope kind -> ProjectId -> Eff es [UsageTotals kind]
usageTotals scope pid = do
  let fixes = case scope of Reviews -> False; Fixes -> True
  Hasql.interp
    [HI.sql|
  WITH scoped AS (
    SELECT *, CASE WHEN #{fixes} THEN fix_run_id ELSE run_id END AS target_id
    FROM projects.agent_usage_activity WHERE project_id = #{pid}
      AND (CASE WHEN #{fixes} THEN fix_run_id ELSE run_id END) IS NOT NULL
  )
  SELECT target_id,
    ROW(COALESCE(sum(calls), 0)::bigint, COALESCE(sum(failed_calls), 0)::bigint,
      COALESCE(sum(unpriced_calls), 0)::bigint, COALESCE(sum(late_attributed_calls), 0)::bigint,
      COALESCE(sum(unreported_attempts), 0)::bigint, COALESCE(sum(unreported_calls), 0)::bigint, COALESCE(sum(cost_nanousd), 0)),
    ROW(COALESCE(sum(checks), 0)::bigint, COALESCE(sum(vcpu_ms), 0), COALESCE(sum(memory_mib_ms), 0),
      COALESCE(sum(log_bytes), 0), COALESCE(sum(explanations), 0)::bigint, COALESCE(sum(render_ms), 0), COALESCE(sum(media_bytes), 0), COALESCE(sum(allocations), 0)::bigint, COALESCE(sum(cleanup_failures), 0)::bigint, COALESCE(sum(unreported_allocations), 0)::bigint)
  FROM scoped GROUP BY ROLLUP(target_id)
  HAVING GROUPING(target_id) = 1 OR target_id IS NOT NULL
|]


-- Customer subtotals must never reveal projects through a shared payment ID.
data ProjectUsage = ProjectUsage {projectId :: ProjectId, title :: Text, billingProvider :: Projects.BillingProvider, customerId :: Maybe Text, usage :: UsageCost, resources :: ResourceUsage}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


customerUsage :: DB es => Projects.UserId -> Eff es [ProjectUsage]
customerUsage uid =
  Hasql.interp
    [HI.sql|
  SELECT p.id, p.title, u.billing_provider, u.customer_id,
    ROW(sum(u.calls)::bigint, sum(u.failed_calls)::bigint, sum(u.unpriced_calls)::bigint,
      sum(u.late_attributed_calls)::bigint, sum(u.unreported_attempts)::bigint, sum(u.unreported_calls)::bigint, sum(u.cost_nanousd)),
    ROW(sum(u.checks)::bigint, sum(u.vcpu_ms), sum(u.memory_mib_ms), sum(u.log_bytes),
      sum(u.explanations)::bigint, sum(u.render_ms), sum(u.media_bytes), sum(u.allocations)::bigint, sum(u.cleanup_failures)::bigint, sum(u.unreported_allocations)::bigint)
  FROM projects.agent_usage_activity u JOIN projects.projects p ON p.id = u.project_id
  WHERE p.deleted_at IS NULL AND EXISTS (
    SELECT 1 FROM projects.project_members pm JOIN users.users person ON person.id = pm.user_id
    WHERE pm.project_id = p.id AND pm.user_id = #{uid} AND person.active AND person.deleted_at IS NULL
      AND pm.permission = #{ProjectMembers.PAdmin} AND pm.active AND pm.deleted_at IS NULL)
  GROUP BY p.id, p.title, u.billing_provider, u.customer_id
  ORDER BY CASE WHEN u.customer_id IS NULL THEN p.id END, u.billing_provider, u.customer_id, p.id
|]


ownsAttempt :: DB es => ProjectId -> Run kind -> UUIDId "agent_attempt" -> Eff es Bool
ownsAttempt pid target attempt = do
  let (rid, fid) = targetIds target
  fromMaybe False
    <$> Hasql.interpOne
      [HI.sql| SELECT EXISTS (SELECT 1 FROM projects.pr_review_attempts
    WHERE id = #{attempt} AND project_id = #{pid} AND run_id IS NOT DISTINCT FROM #{rid} AND fix_run_id IS NOT DISTINCT FROM #{fid}) |]


hasCheck :: DB es => ProjectId -> Run kind -> Agent.CheckReceipt -> Eff es Bool
hasCheck pid target receipt = do
  let (rid, fid) = targetIds target
  fromMaybe False
    <$> Hasql.interpOne
      [HI.sql| SELECT EXISTS (SELECT 1 FROM projects.pr_review_checks
    WHERE id = #{receipt.id} AND project_id = #{pid} AND run_id IS NOT DISTINCT FROM #{rid} AND fix_run_id IS NOT DISTINCT FROM #{fid}
      AND ('{"regression":null,"workspace":null}'::jsonb || receipt) = #{Aeson receipt}) |]
