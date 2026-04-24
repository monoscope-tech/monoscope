#!/usr/bin/env bash
#
# One-off cleanup: archive (do not delete) open log_pattern /
# log_pattern_rate_change issues that match known noise patterns.
#
# Paired with:
#   - src/BackgroundJobs.hs: widened detector log-level gate
#     (only ERROR/WARN/FATAL/CRITICAL produce rate-change issues)
#   - src/BackgroundJobs.hs: pruneStaleLogPatterns now auto-archives
#     opens older than staleLogPatternIssueDays (14 days)
#
# Noise predicates — a row must satisfy at least one to be archived:
#   (a) issue_type='log_pattern' with no updated_at activity in 7 days
#   (b) issue_type='log_pattern_rate_change' with a pre-0075 title shape
#       ("Log Pattern Drop" prefix, or stray ";badge-info" markup)
#   (c) issue_type='log_pattern_rate_change' with severity='low' — these
#       are already auto-demoted silent drops (Issues.hs:865-868); the
#       Inbox hides them but they linger on Acknowledged/Archived tabs
#       and inflate counts.
#   (d) issue_type='log_pattern_rate_change' whose direction is 'drop',
#       current rate collapsed to 0, and no updates for 7d. Deploy/
#       pod-restart noise.
#
# Rows are only archived if they are still open
# (acknowledged_at IS NULL AND archived_at IS NULL), preserving audit.
#
# Usage:
#   DATABASE_URL=postgres://… ./scripts/archive-noisy-log-pattern-issues.sh dry-run
#   DATABASE_URL=postgres://… ./scripts/archive-noisy-log-pattern-issues.sh write
#
# Safety:
#   - Default mode prints counts only; archival requires the `write` arg.
#   - The write path is wrapped in BEGIN/COMMIT so a connection drop leaves
#     the table untouched.

set -euo pipefail

MODE="${1:-dry-run}"

if [[ -z "${DATABASE_URL:-}" ]]; then
  echo "DATABASE_URL is required (set it in your env or source .env)" >&2
  exit 1
fi

if ! command -v psql >/dev/null 2>&1; then
  echo "psql not found on PATH" >&2
  exit 1
fi

PREDICATE=$(cat <<'SQL'
  acknowledged_at IS NULL
  AND archived_at IS NULL
  AND (
    -- (a) New-pattern zombies: no activity in 7d.
    (issue_type = 'log_pattern'
      AND updated_at < NOW() - INTERVAL '7 days')
    OR
    -- (b) Rate-change rows from the pre-0075 detector (legacy title shape).
    (issue_type = 'log_pattern_rate_change'
      AND (title ILIKE '%Log Pattern Drop%'
        OR title ILIKE '%badge-info%'))
    OR
    -- (c) Auto-demoted silent drops (severity='low' set by
    -- createLogPatternRateChangeIssue for drops on unknown services).
    (issue_type = 'log_pattern_rate_change' AND severity = 'low')
    OR
    -- (d) Drops where volume collapsed to 0 and no updates for 7d.
    (issue_type = 'log_pattern_rate_change'
      AND (issue_data->>'change_direction') = 'drop'
      AND COALESCE((issue_data->>'current_rate_per_hour')::float, 0) = 0
      AND updated_at < NOW() - INTERVAL '7 days')
  )
SQL
)

case "$MODE" in
  dry-run)
    echo "DRY RUN — no rows will be changed. Counts per (project_id, issue_type):"
    echo
    psql "$DATABASE_URL" -v ON_ERROR_STOP=1 <<SQL
SELECT project_id,
       issue_type,
       COUNT(*) AS candidate_count
FROM apis.issues
WHERE $PREDICATE
GROUP BY project_id, issue_type
ORDER BY candidate_count DESC;

SELECT '-- total candidate rows' AS label, COUNT(*) AS total
FROM apis.issues
WHERE $PREDICATE;
SQL
    echo
    echo "Re-run with: $0 write"
    ;;
  write)
    echo "WRITE mode — archiving candidate rows."
    psql "$DATABASE_URL" -v ON_ERROR_STOP=1 <<SQL
BEGIN;

WITH archived AS (
  UPDATE apis.issues
  SET archived_at = NOW()
  WHERE $PREDICATE
  RETURNING project_id, issue_type
)
SELECT project_id,
       issue_type,
       COUNT(*) AS archived_count
FROM archived
GROUP BY project_id, issue_type
ORDER BY archived_count DESC;

COMMIT;
SQL
    echo
    echo "Done. Re-run dry-run to confirm counts have collapsed."
    ;;
  *)
    echo "Usage: $0 {dry-run|write}" >&2
    exit 2
    ;;
esac
