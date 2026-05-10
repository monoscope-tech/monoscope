-- In-memory schema-learning pipeline persistence layer.
--
-- Replaces apis.shapes / apis.fields / apis.formats / apis.facet_summaries.
-- Two-tier:
--   * apis.schema_template — instance-wide, structure-only, dedup'd by hash
--     (autoinstrumentation spans collapse to a few templates shared by every
--     tenant — no examples or values, so safe to share).
--   * apis.schema_catalog  — per-project, references a template + holds the
--     tenant-private bits (values, counts, first-seen, anomaly state).
--   * apis.schema_summary  — materialized AI/query-editor doc per project.

CREATE TYPE apis.schema_key_kind AS ENUM ('http_endpoint', 'span_identity');

CREATE TABLE apis.schema_template (
  template_hash text PRIMARY KEY,
  key_kind      apis.schema_key_kind NOT NULL,
  fields        jsonb NOT NULL,
  ref_count     bigint NOT NULL DEFAULT 0,
  created_at    timestamptz NOT NULL DEFAULT now(),
  last_seen_at  timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX schema_template_kind_lastseen_idx
  ON apis.schema_template (key_kind, last_seen_at DESC);

CREATE TABLE apis.schema_catalog (
  project_id    uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  key_kind      apis.schema_key_kind NOT NULL,
  key_hash      text NOT NULL,
  template_hash text NOT NULL REFERENCES apis.schema_template(template_hash),
  scope         jsonb NOT NULL,
  values_delta  jsonb NOT NULL DEFAULT '{}'::jsonb,
  counts        jsonb NOT NULL DEFAULT '{}'::jsonb,
  sample_count  bigint NOT NULL DEFAULT 0,
  first_seen    timestamptz NOT NULL,
  last_seen     timestamptz NOT NULL,
  updated_at    timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (project_id, key_hash)
);

CREATE INDEX schema_catalog_project_lastseen_idx
  ON apis.schema_catalog (project_id, last_seen DESC);
CREATE INDEX schema_catalog_project_kind_idx
  ON apis.schema_catalog (project_id, key_kind);
CREATE INDEX schema_catalog_template_idx
  ON apis.schema_catalog (template_hash);
CREATE INDEX schema_catalog_host_idx
  ON apis.schema_catalog ((scope->>'host'))
  WHERE key_kind = 'http_endpoint';

CREATE TABLE apis.schema_summary (
  project_id   uuid PRIMARY KEY REFERENCES projects.projects(id) ON DELETE CASCADE,
  doc          jsonb NOT NULL,
  generated_at timestamptz NOT NULL DEFAULT now()
);
