-- Episodes are distinct from issue fingerprints: a recovered signal can recur.
CREATE TABLE apis.incident_episodes (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  source_kind TEXT NOT NULL CHECK (source_kind IN ('monitor', 'issue')),
  source_id UUID NOT NULL,
  issue_id UUID REFERENCES apis.issues(id) ON DELETE SET NULL,
  phase TEXT NOT NULL CHECK (phase IN ('active', 'recovered', 'resolved')),
  started_at TIMESTAMPTZ NOT NULL,
  last_event_at TIMESTAMPTZ NOT NULL,
  closed_at TIMESTAMPTZ,
  UNIQUE (id, project_id),
  CHECK ((phase = 'active') = (closed_at IS NULL)),
  CHECK (last_event_at >= started_at),
  CHECK (closed_at IS NULL OR closed_at >= started_at)
);

CREATE UNIQUE INDEX incident_one_active_episode
  ON apis.incident_episodes(project_id, source_kind, source_id) WHERE phase = 'active';
CREATE INDEX incident_source_history
  ON apis.incident_episodes(project_id, source_kind, source_id, started_at DESC);

CREATE TABLE apis.incident_events (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  episode_id UUID NOT NULL,
  project_id UUID NOT NULL,
  source_kind TEXT NOT NULL CHECK (source_kind IN ('monitor', 'issue')),
  source_id UUID NOT NULL,
  event_kind TEXT NOT NULL CHECK (event_kind IN ('alert', 'reminder', 'recovered', 'resolved')),
  observed_at TIMESTAMPTZ NOT NULL,
  actor_id UUID REFERENCES users.users(id),
  root_payload JSONB NOT NULL CHECK (jsonb_typeof(root_payload) = 'object'),
  reply_payload JSONB NOT NULL CHECK (jsonb_typeof(reply_payload) = 'object'),
  FOREIGN KEY (episode_id, project_id) REFERENCES apis.incident_episodes(id, project_id) ON DELETE CASCADE,
  UNIQUE (project_id, source_kind, source_id, observed_at, event_kind),
  UNIQUE (id, episode_id),
  CHECK ((event_kind = 'resolved') = (actor_id IS NOT NULL))
);

CREATE TABLE apis.slack_incident_roots (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  episode_id UUID NOT NULL REFERENCES apis.incident_episodes(id) ON DELETE CASCADE,
  team_id TEXT NOT NULL CHECK (team_id <> ''),
  channel_id TEXT NOT NULL CHECK (channel_id <> ''),
  first_event_id UUID NOT NULL,
  message_ts TEXT CHECK (message_ts IS NULL OR message_ts ~ '^[0-9]+\.[0-9]+$'),
  FOREIGN KEY (first_event_id, episode_id) REFERENCES apis.incident_events(id, episode_id),
  UNIQUE (episode_id, team_id, channel_id),
  UNIQUE (id, episode_id)
);

-- Each event posts a reply and updates the root, after the original root exists.
-- sequence defines delivery order even when two events share a wall-clock time.
CREATE TABLE apis.slack_incident_deliveries (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  sequence BIGINT GENERATED ALWAYS AS IDENTITY UNIQUE,
  root_id UUID NOT NULL,
  episode_id UUID NOT NULL,
  event_id UUID NOT NULL,
  operation TEXT NOT NULL CHECK (operation IN ('post_root', 'post_reply', 'update_root')),
  state TEXT NOT NULL DEFAULT 'pending'
    CHECK (state IN ('pending', 'sending', 'waiting_root', 'uncertain', 'delivered', 'failed')),
  available_at TIMESTAMPTZ NOT NULL,
  lease_token UUID,
  lease_until TIMESTAMPTZ,
  attempts INTEGER NOT NULL DEFAULT 0 CHECK (attempts >= 0),
  message_ts TEXT,
  last_error TEXT,
  FOREIGN KEY (root_id, episode_id) REFERENCES apis.slack_incident_roots(id, episode_id) ON DELETE CASCADE,
  FOREIGN KEY (event_id, episode_id) REFERENCES apis.incident_events(id, episode_id) ON DELETE CASCADE,
  UNIQUE (root_id, event_id, operation),
  CHECK (operation = 'post_root' OR state <> 'waiting_root'),
  CHECK (state <> 'sending' OR (lease_token IS NOT NULL AND lease_until IS NOT NULL))
);

CREATE INDEX slack_delivery_pending ON apis.slack_incident_deliveries(available_at, sequence) WHERE state = 'pending';
CREATE INDEX slack_delivery_root_order ON apis.slack_incident_deliveries(root_id, sequence);
CREATE INDEX slack_delivery_expired ON apis.slack_incident_deliveries(lease_until) WHERE state = 'sending';
