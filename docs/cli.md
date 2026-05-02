# Monoscope CLI

The `monoscope` CLI provides terminal access to your observability platform — search logs and traces, query metrics, manage monitors and dashboards, triage issues, and more.

## Installation

```bash
curl -fsSL https://monoscope.tech/install.sh | bash
```

Detects your platform (Linux/macOS, x86_64/arm64), downloads the latest release, verifies the SHA256 checksum, and installs `monoscope` to `~/.local/bin`.

**Specific version:**

```bash
curl -fsSL https://monoscope.tech/install.sh | bash -s v1.2.3
```

**Custom install directory:**

```bash
MONOSCOPE_INSTALL_DIR=/usr/local/bin curl -fsSL https://monoscope.tech/install.sh | bash
```

**Upgrade to latest** (re-running the install script upgrades automatically if a newer version exists):

```bash
curl -fsSL https://monoscope.tech/install.sh | bash
```

If the installed version is already current, the script exits with "already up to date". To force reinstall the same version, pass `--force`:

```bash
curl -fsSL https://monoscope.tech/install.sh | bash -s -- --force
```

## Authentication

**Browser-based login** (interactive — opens browser automatically):

```bash
monoscope auth login
```

**Token login** (non-interactive / CI):

```bash
monoscope auth login --token <your-api-key>
```

**Check auth state:**

```bash
monoscope auth status
```

**Log out:**

```bash
monoscope auth logout
```

## Configuration

### Environment variables

| Variable | Description |
|---|---|
| `MONOSCOPE_API_KEY` | API key — takes precedence over stored token |
| `MONOSCOPE_PROJECT` | Default project UUID |
| `MONOSCOPE_API_URL` | API base URL (default: `https://api.monoscope.tech`) |
| `MONOSCOPE_AGENT_MODE` | Set to `1` to force JSON output (also triggered by `CI` or `CLAUDE_CODE`) |
| `MONOSCOPE_DEBUG` | Set to `1` to print every outgoing request to stderr (same as `--debug`) |

### Config files

Config is resolved in this order (later values win):

1. `~/.config/monoscope/config.yaml` — global config
2. `.monoscope.yaml` — project-local config (searched up from cwd)
3. Environment variables

**Interactive setup wizard:**

```bash
monoscope config init
```

**Set/get individual values:**

```bash
monoscope config set project <uuid>
monoscope config set api_url https://your-instance.example.com
monoscope config get            # show all
monoscope config get project    # show one value
```

Valid config keys: `api_url`, `project`, `api_key`.

## Global flags

Every command accepts these flags:

| Flag | Description |
|---|---|
| `--project/-p <uuid>` | Override the project for this invocation |
| `--output/-o json\|yaml\|table` | Force output format |
| `--json` | Shorthand for `--output json` |
| `--agent` | Force JSON output (same as `MONOSCOPE_AGENT_MODE=1`) |
| `--debug` | Print every outgoing request URL to stderr |

## Output modes

- `table` — default when stdout is a TTY; human-readable aligned columns
- `json` — default when stdout is a pipe or in CI; ideal for piping into `jq`
- `yaml` — config-friendly; round-trips cleanly through `apply`

---

## Logs & Traces

Logs and traces share the same event storage. `logs` and `traces` are aliases for `events` with a kind filter pre-applied.

### Search

The positional `QUERY` argument is **KQL** (the same query language used in the web UI). Equality uses `==` with double-quoted strings; combine with `and`/`or`/parens.

```bash
# All events from a service in the last hour (default --since=1H, matching the UI)
monoscope logs search --service checkout-api

# Only error-level logs in the last 30 minutes
monoscope logs search --level error --since 30m --limit 50

# Free-text search inside the log body
monoscope logs search 'body contains "payment failed"' --since 1h

# Filter by attribute (any field from `monoscope schema`)
monoscope events search 'attributes.http.response.status_code >= 500' --since 1h

# Absolute time range
monoscope traces search --service auth \
  --from 2026-04-01T00:00:00Z --to 2026-04-02T00:00:00Z
```

Options:

| Flag | Description |
|---|---|
| `QUERY` | KQL search query (positional). Run `monoscope schema` to see available fields. |
| `--since <duration>` | Relative lookback (default: `1H`). Examples: `10s`, `30m`, `2h`, `7d` |
| `--from <timestamp>` | Start time (ISO 8601) — pairs with `--to`, overrides `--since` |
| `--to <timestamp>` | End time (ISO 8601) |
| `--kind log\|trace` | Filter by event kind (mapped to the `source` query param) |
| `--service <name>` | Shorthand for `resource.service.name=="<name>"` |
| `--level <level>` | Shorthand for `severity.text=="<level>"` |
| `--limit/-n <N>` | Max results to return |
| `--fields <f1,f2>` | Comma-separated columns to keep in JSON / table output |
| `--cursor <value>` | Pagination: pass the `cursor` field from a previous response |
| `--first` | Return only the first matching event (the JSON envelope still carries `count`/`has_more`) |
| `--id-only` | Print just the first event's `id` to stdout — natural input for `events get`/`share-link`. Implies `--first` |

The JSON output envelope is stable: `{events: [...], count, has_more, cursor}`. Use `cursor` to drive pagination loops, `count` for the total matching, and `has_more` to terminate.

### Get a single event or trace

```bash
# Single event by id (default --since=24H)
monoscope events get <event-id>

# All spans of a trace
monoscope traces get <trace-id> --tree
```

### Live tail

Stream events in real-time (polls every 2 seconds):

```bash
monoscope events tail
monoscope logs tail --service api --level error
monoscope logs tail --grep "timeout"       # client-side substring filter
```

### Context window

Show events around a specific timestamp:

```bash
monoscope events context --at 2026-04-15T12:34:56Z --service api --window 10m

# Per-trace summary alongside raw events (recommended for incident triage)
monoscope events context --at 2026-04-15T12:34:56Z --window 10m --summary
```

With `--summary`, the JSON envelope is augmented with a `traces` array:

```json
{
  "events": [...],
  "count": 42,
  "has_more": false,
  "traces": [
    { "trace_id": "abc...", "services": ["checkout", "payments"], "span_count": 12, "error_count": 1 }
  ]
}
```

---

## Metrics

The `metrics` endpoint runs **KQL** queries against the same store as the web UI.
Use `summarize` for aggregations and `by` for grouping.

### Query

```bash
# Event count grouped by service (last 1H, default)
monoscope metrics query 'summarize count() by resource.service.name'

# p99 latency per service over the last 30 minutes
monoscope metrics query 'summarize percentile(duration, 99) by resource.service.name' --since 30m

# Assertion (CI-friendly: exits non-zero if the numeric result fails the condition)
monoscope metrics query 'summarize count()' --since 30m --assert '< 1000'
```

Options: `--since` (default `1H`), `--from`, `--to`, `--assert <condition>`.

### Chart (sparkline)

```bash
monoscope metrics chart 'summarize count() by bin(timestamp, 1m)' --since 2h
monoscope metrics chart 'summarize avg(duration) by bin(timestamp, 1m)' --watch 30s
```

---

## Services

```bash
monoscope services list                  # services active in last 24h
monoscope services list --since 7d
```

---

## Monitors

```bash
monoscope monitors list
monoscope monitors get <id>

# Create / update from file (YAML or JSON)
monoscope monitors create monitor.yaml
monoscope monitors update <id> monitor.yaml   # full replace (PUT)
monoscope monitors patch <id> patch.yaml      # partial update (PATCH)

# Apply a file or directory (idempotent upsert keyed by name)
monoscope monitors apply monitors/

# Lifecycle
monoscope monitors mute <id> --for 30         # mute for 30 minutes
monoscope monitors unmute <id>
monoscope monitors resolve <id>
monoscope monitors toggle-active <id>
monoscope monitors delete <id>

# Bulk actions: delete, activate, deactivate, mute, unmute, resolve
monoscope monitors bulk mute --ids id1,id2,id3 --duration 60
monoscope monitors bulk resolve --ids id1,id2
```

---

## Dashboards

```bash
monoscope dashboards list
monoscope dashboards get <id>

monoscope dashboards create dashboard.yaml
monoscope dashboards update <id> dashboard.yaml
monoscope dashboards patch <id> patch.yaml
monoscope dashboards delete <id>

monoscope dashboards star <id>
monoscope dashboards unstar <id>
monoscope dashboards duplicate <id>

# Dump as YAML (for editing and re-applying)
monoscope dashboards yaml <id> > dashboard.yaml

# Idempotent upsert from file or directory
monoscope dashboards apply dashboards/

# Bulk: delete
monoscope dashboards bulk delete --ids id1,id2

# Widget operations
monoscope dashboards widget upsert <dashboard-id> widget.yaml
monoscope dashboards widget delete <dashboard-id> <widget-id>
monoscope dashboards widget reorder <dashboard-id> --tab overview positions.json
```

`positions.json` shape:

```json
{
  "widget-id-1": { "x": 0, "y": 0, "w": 6, "h": 4 },
  "widget-id-2": { "x": 6, "y": 0, "w": 6, "h": 4 }
}
```

---

## API Keys

```bash
monoscope api-keys list
monoscope api-keys get <id>
monoscope api-keys create "my-key-name"     # prints plaintext key once
monoscope api-keys activate <id>
monoscope api-keys deactivate <id>          # soft disable
monoscope api-keys delete <id>              # permanent removal
```

**Rotating a key:**

```bash
monoscope api-keys create "ops-2026-05" -o json | jq -r .key > ~/.monoscope-key-new
export MONOSCOPE_API_KEY=$(cat ~/.monoscope-key-new)
monoscope api-keys deactivate <old-key-id>
```

---

## Issues

Issues are the user-facing representation of detected problems.

```bash
monoscope issues list                              # open issues by default
monoscope issues list --status acknowledged        # open | acknowledged | archived | all
monoscope issues list --type runtime_exception     # filter by issue type
monoscope issues list --service checkout-api       # filter by service
monoscope issues get <id>
monoscope issues ack <id>
monoscope issues unack <id>
monoscope issues archive <id>
monoscope issues unarchive <id>

# Bulk: acknowledge, unack, archive, unarchive
monoscope issues bulk acknowledge --ids id1,id2,id3
```

---

## API Catalog (Endpoints)

```bash
monoscope endpoints list
monoscope endpoints list --search '/v1/' --per-page 100
monoscope endpoints list --outgoing             # only outgoing calls
monoscope endpoints get <id>
```

---

## Log Patterns

```bash
monoscope log-patterns list --per-page 50
monoscope log-patterns get <id>
monoscope log-patterns ack <id>

# Bulk: ack, ignore
monoscope log-patterns bulk ack --ids 1,2,3
monoscope log-patterns bulk ignore --ids 4,5
```

---

## Teams

```bash
monoscope teams list
monoscope teams get <id>
monoscope teams create team.yaml       # { name, handle, description, notify_emails, slack_channels }
monoscope teams update <id> team.yaml  # full replace (PUT)
monoscope teams patch <id> patch.yaml  # partial update (PATCH)
monoscope teams delete <id>
monoscope teams bulk delete --ids id1,id2
```

The `everyone` handle is reserved — the built-in everyone team cannot be updated or deleted.

---

## Members

```bash
monoscope members list
monoscope members get <user-id>

# Add by email (creates a stub user if unknown) or by existing user ID
monoscope members add --email teammate@example.com --permission view
monoscope members add --user-id <uuid> --permission edit

# Permissions: view | edit | admin
monoscope members patch <user-id> admin

monoscope members remove <user-id>
```

---

## Project

```bash
monoscope me                          # show current project identity
monoscope project get
monoscope project patch patch.yaml    # PATCH any subset of: title, description, time_zone,
                                      # daily_notif, weekly_notif, endpoint_alerts, error_alerts
```

---

## Share Links

Create a 48-hour share link for a stored event:

```bash
monoscope share-link create \
  --event-id <uuid> \
  --created-at 2026-04-15T00:00:00Z \
  --type log    # request | log | span
```

Returns `{ id, url }`.

---

## Schema

Fetch the telemetry field schema for the current project. The full payload
is large; use `--search`/`--limit` so an LLM agent doesn't have to load
hundreds of field names into context to answer a single question.

```bash
monoscope schema                            # all fields
monoscope schema --search service           # only fields whose name contains "service"
monoscope schema --search http --limit 20   # http-related fields, capped at 20
monoscope schema -o json | jq '.fields | keys[]'   # bare field names
```

The response shape is `{fields: { <field_name>: { field_type, description, ... }, ... }, ...}`.
Note `fields` is an object keyed by field name, **not** an array — `jq '.fields[]'` works,
`jq '.fields[0]'` does not.

---

## Facets

`facets` returns precomputed top-N values per field — the platform tracks
this for every faceted column (service name, severity, HTTP method, status
code, etc.) so an agent can answer "what's actually in this project?"
without paying for an aggregation query each time.

```bash
# Dump every faceted field with its top values
monoscope facets

# Drill into a single field
monoscope facets resource.service.name
monoscope facets severity.text --top 5

# Widen the lookback (default 24h)
monoscope facets --since 7d

# Pipe into a search
SVC=$(monoscope facets resource.service.name --top 1 \
        | jq -r '.["resource.service.name"][0].value')
monoscope events search '' --service "$SVC" --since 1h
```

Response shape:

```json
{
  "resource.service.name": [
    { "value": "checkout-api", "count": 1284 },
    { "value": "payments",     "count":  812 },
    ...
  ],
  "severity.text": [...],
  "attributes.http.response.status_code": [...]
}
```

Each field's value list is sorted by `count` descending. With `--top N`,
the CLI trims each field's list to N entries (server returns the full
top-N already, the flag is a client-side cap when you want fewer).

When facets haven't been generated yet (new project, or the background
job hasn't run), the response is `{}` — not 404. Agents can rely on the
shape regardless.

---

## Auth status (machine-readable)

`auth status` emits structured output in agent mode (`--agent`, `--json`,
`MONOSCOPE_AGENT_MODE=1`, `CI=1`, or `CLAUDE_CODE=1`):

```bash
monoscope --agent auth status
# {
#   "authenticated": true,
#   "method": "token",          # "token" | "env" | null
#   "api_url": "https://api.monoscope.tech",
#   "project": "00000000-..."
# }
```

In agent mode `auth login` (without `--token`) refuses to start the
interactive device-code flow and exits non-zero — pass `--token <key>` from
your secret store instead.

---

## Shell Completions

```bash
monoscope completion bash >> ~/.bashrc
monoscope completion zsh  >> ~/.zshrc
monoscope completion fish >> ~/.config/fish/completions/monoscope.fish
```

Unknown shells exit non-zero with a clear error rather than silently
emitting a bash script.

---

## JSON output shape

In agent / pipe mode the CLI emits stable, documented JSON. For event-shaped commands (`events search`, `logs search`, `traces search`, `events get`, `events context`) the response is:

```json
{
  "events": [{ "id": "...", "timestamp": "...", "service": "...", "summary": "...", "trace_id": "...", "kind": "...", ... }],
  "count": 501,
  "has_more": true,
  "cursor": "2026-05-02T17:34:00.053333Z"
}
```

Each event is a flat object — no `colIdxMap` / `logsData` indirection. Use `--fields a,b,c` to project a subset.

For `services list`:

```json
{ "services": [{ "name": "checkout-api", "events": 1284, "last_seen": "..." }], "count": 12 }
```

For every other list command (`monitors list`, `dashboards list`, `api-keys list`, `teams list`, `members list`, `issues list`, `endpoints list`, `log-patterns list`) the CLI normalises to a single shape:

```json
{
  "data": [...],
  "pagination": {
    "has_more": false,
    "total": 42,           // null if the server doesn't report total_count
    "cursor": null,
    "page": 0,             // null if the server uses cursor-only pagination
    "per_page": 20
  }
}
```

This normalisation happens client-side — the underlying API still returns its native shape (some endpoints emit a `Paged` envelope `{items, page, per_page, total_count, has_more}`, others a bare array). `jq '.data[] | .id'` works for every list command, regardless of which the server returned.

> **Migration note:** the previous shapes (`{items, total_count, ...}` for issues/endpoints/log-patterns; bare arrays for monitors/dashboards/api-keys/teams/members) are no longer emitted. Pipelines that read `.items` should switch to `.data`. The `events search` envelope (`{events, count, has_more, cursor}`) is unchanged — those commands remain on their event-specific shape because they expose `--cursor`/`--first`/`--id-only`.

## Agentic incident workflow

A typical incident investigation, all in JSON, suitable for piping into `jq` or feeding to an LLM agent:

```bash
# 1. What's on fire? List open issues, sorted by recency.
monoscope issues list --status open -o json | jq '.items[] | {id,title,service,severity}'

# 2. Which services are active? (and which are quiet)
monoscope services list -o json | jq '.services'

# 3. Drill in: errors from the suspect service in the last 30 minutes
monoscope logs search --service checkout-api --level error --since 30m -o json \
  | jq '.events[] | {timestamp, summary, trace_id}'

# 4. Pick a trace and pull the full tree
monoscope traces get <trace-id> --tree

# 5. Find similar errors via log patterns
monoscope log-patterns list --per-page 20 -o json \
  | jq '.items[] | select(.state=="open") | {id, occurrence_count, service_name}'

# 6. Check a specific symptom: which endpoints serve 5xx?
monoscope events search 'attributes.http.response.status_code >= 500' --since 1h -o json \
  | jq -r '.events[] | "\(.timestamp) \(.service) \(.summary)"'

# 7. Acknowledge, and keep moving
monoscope issues ack <issue-id>
```

Every command honours `--debug` (or `MONOSCOPE_DEBUG=1`) to print the outgoing request URL — invaluable when an agent gets a 4xx and needs to inspect what it actually sent. Server validation errors (e.g. invalid KQL) are forwarded verbatim from the response body, including line/column markers for parse failures.

## CI / Automation

Export these in CI environments:

```bash
export MONOSCOPE_API_KEY=<your-api-key>
export MONOSCOPE_PROJECT=<project-uuid>
```

JSON is the default when stdout is not a TTY or `CI` is set. Use `jq` for scripting:

```bash
# Error-rate gate in CI (count of error-level events in the last 30 min)
monoscope metrics query 'summarize count() | where severity.text=="error"' --since 30m --assert '< 100'

# Get open issue count
monoscope issues list --status open -o json | jq '.items | length'

# Apply all monitors from a directory (idempotent)
monoscope monitors apply .monoscope/monitors/
```

---

## Testing & regression coverage

The CLI ships with three layers of tests; pick the one that matches the change you're making.

### 1. Doctests for pure helpers

`cli/CLI/Validate.hs` carries doctests for `validateDuration`, `validateUuid`,
`validateKind`, and `normalizeKind`. They run as part of the lib's doctest
suite once exposed; until then they document expected behavior in-source.
Add a doctest for any new pure helper — they are the cheapest regression
guard you have.

### 2. Binary smoke tests (no server)

`test/integration/CLI/CLIBinarySpec.hs` runs `monoscope --help` for every
subcommand and asserts exit code 0. This catches optparse parser
construction errors before they reach a user. **Add an entry to `helpCases`
when you add a new `command "..."` in `cli/Main.hs`** — the test catches
copy-paste typos in metavar / parser wiring instantly.

### 3. End-to-end against a real server

`test/integration/CLI/CLIE2ESpec.hs` runs the actual `monoscope` binary
against an HTTP server. This is what catches wire-format regressions
(KQL operator changes, JSON envelope drift, missing query params, broken
auth headers) — the bug class that motivated the audit.

The suite reuses the **same env vars the CLI itself reads** (`MONOSCOPE_API_URL` /
`MONOSCOPE_API_KEY` / `MONOSCOPE_PROJECT`) — so if you've configured your shell
to drive `monoscope` against a local server for debugging, the e2e tests
automatically pick that up. When the env vars are absent, every test marks
itself `pending` so ordinary `make test` doesn't depend on a live server.

To turn it on against your local dev server:

```bash
# 1. Start the server: in a separate pane, run `make live-reload` and wait
#    until `Starting Monoscope server on http://localhost:8080` appears.
# 2. Mint an API key for an existing project (one-time):
monoscope --agent api-keys create "e2e-tests" | jq -r .key > /tmp/e2e.key
# 3. Run the e2e suite:
MONOSCOPE_API_URL=http://localhost:8080 \
MONOSCOPE_API_KEY=$(cat /tmp/e2e.key) \
MONOSCOPE_PROJECT=00000000-0000-0000-0000-000000000000 \
  USE_EXTERNAL_DB=true cabal test integration-tests \
    --test-options='--match "CLI binary E2E"'
```

Each spec checks a single audit finding (KQL operator, kind→source mapping,
error-body surfacing, list envelope, agent-mode JSON, validation messages,
etc.) — adding a new feature? Add the regression test alongside.

The same env vars work against any deployment: a staging instance, a
docker-compose stack, or production (`MONOSCOPE_API_URL=https://api.monoscope.tech`).

### When you add a new CLI feature

1. Add the parser entry in `cli/Main.hs` (the binary smoke test catches
   wiring errors automatically).
2. Implement in `cli/CLI/Commands.hs` or `cli/CLI/Resource.hs` and any pure
   helper goes in `cli/CLI/Validate.hs` (with doctests).
3. Add an `it "<feature>" $ withReachableServer $ \cfg -> ...` in
   `test/integration/CLI/CLIE2ESpec.hs` that asserts the wire-level
   behaviour — the JSON envelope shape, the exit code, the error message.
4. Run the e2e suite locally before merging if the change touches the
   request URL, the query params, or the response shape.
