-- A node that can run reviews checks in every minute, so the web can tell whether a queued review will run.
CREATE TABLE IF NOT EXISTS projects.review_workers (
  node text PRIMARY KEY,
  agents integer NOT NULL,
  seen_at timestamptz NOT NULL
);
