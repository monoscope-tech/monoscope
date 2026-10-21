-- Free-tier usage email dedup: the day and highest notice already sent (noticeRank: 1 warning, 2 exceeded).
CREATE TABLE IF NOT EXISTS projects.free_tier_notices (
  project_id uuid PRIMARY KEY REFERENCES projects.projects(id) ON DELETE CASCADE,
  day        date     NOT NULL,
  level      smallint NOT NULL
);
