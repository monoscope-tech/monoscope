# Measure the activation funnel

Use this report before changing onboarding. It counts projects, not people, and
reads only the four fixed milestones in `projects.activation_milestones`.

Run it against production with an agreed cohort start and end time. The cohort
is projects created in that period; a project can count at most once per step.

```sql
WITH cohort AS (
  SELECT id, created_at
  FROM projects.projects
  WHERE created_at >= TIMESTAMPTZ '2026-09-01 00:00:00Z'
    AND created_at <  TIMESTAMPTZ '2026-10-01 00:00:00Z'
    AND deleted_at IS NULL
), milestones AS (
  SELECT
    cohort.id,
    cohort.created_at,
    MAX(occurred_at) FILTER (WHERE milestone = 'ingest_verified') AS ingest_at,
    MAX(occurred_at) FILTER (WHERE milestone = 'dashboard_created') AS dashboard_at,
    MAX(occurred_at) FILTER (WHERE milestone = 'monitor_created') AS monitor_at,
    MAX(occurred_at) FILTER (WHERE milestone = 'notification_test_sent') AS notification_at
  FROM cohort
  LEFT JOIN projects.activation_milestones ON activation_milestones.project_id = cohort.id
  GROUP BY cohort.id, cohort.created_at
)
SELECT
  COUNT(*) AS projects_created,
  COUNT(*) FILTER (WHERE ingest_at IS NOT NULL) AS ingest_verified,
  COUNT(*) FILTER (WHERE dashboard_at IS NOT NULL) AS dashboard_created,
  COUNT(*) FILTER (WHERE monitor_at IS NOT NULL) AS monitor_created,
  COUNT(*) FILTER (WHERE notification_at IS NOT NULL) AS notification_test_sent,
  ROUND(100.0 * COUNT(*) FILTER (WHERE notification_at IS NOT NULL) / NULLIF(COUNT(*), 0), 1) AS full_activation_pct,
  percentile_cont(0.5) WITHIN GROUP (ORDER BY EXTRACT(EPOCH FROM notification_at - created_at) / 3600)
    FILTER (WHERE notification_at IS NOT NULL) AS median_hours_to_full_activation
FROM milestones;
```

Interpret the first weak transition as the next experiment. For example, a high
ingest-to-dashboard drop calls for dashboard creation help, not more ingestion
instructions. Do not use this report to identify users or change billing.
