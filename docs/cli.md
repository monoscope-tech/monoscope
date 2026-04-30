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
MONO_INSTALL_DIR=/usr/local/bin curl -fsSL https://monoscope.tech/install.sh | bash
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
| `MONO_API_KEY` | API key — takes precedence over stored token |
| `MONO_PROJECT` | Default project UUID |
| `MONO_API_URL` | API base URL (default: `https://api.monoscope.tech`) |
| `MONO_AGENT_MODE` | Set to `1` to force JSON output (also triggered by `CI` or `CLAUDE_CODE`) |

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
| `--agent` | Force JSON output (same as `MONO_AGENT_MODE=1`) |

## Output modes

- `table` — default when stdout is a TTY; human-readable aligned columns
- `json` — default when stdout is a pipe or in CI; ideal for piping into `jq`
- `yaml` — config-friendly; round-trips cleanly through `apply`

---

## Logs & Traces

Logs and traces share the same event storage. `logs` and `traces` are aliases for `events` with a kind filter pre-applied.

### Search

```bash
monoscope events search "payment failed" --since 1h
monoscope logs search "error" --service checkout-api --level error --limit 50
monoscope traces search --service auth --from 2026-04-01T00:00:00Z --to 2026-04-02T00:00:00Z
```

Options:

| Flag | Description |
|---|---|
| `QUERY` | KQL search query (positional) |
| `--since <duration>` | Relative lookback: `10s`, `30m`, `2h`, `7d` |
| `--from <timestamp>` | Start time (ISO 8601) |
| `--to <timestamp>` | End time (ISO 8601) |
| `--kind log\|trace` | Filter by event kind |
| `--service <name>` | Filter by `resource.service.name` |
| `--level <level>` | Filter by severity level |
| `--limit/-n <N>` | Max results to return |
| `--fields <f1,f2>` | Comma-separated columns to display |

### Get a single event or trace

```bash
# By event ID or trace ID
monoscope events get <id>

# Show full trace tree
monoscope traces get <trace-id> --tree
```

### Live tail

Stream events in real-time (polls every 2 seconds):

```bash
monoscope events tail
monoscope logs tail --service api --level error
monoscope logs tail --grep "timeout"       # client-side regex filter on message
```

### Context window

Show events around a specific timestamp:

```bash
monoscope events context --at 2026-04-15T12:34:56Z --service api --window 10m
```

---

## Metrics

### Query

```bash
monoscope metrics query "avg(http.server.duration) by service"
monoscope metrics query "error_rate" --since 30m --assert "< 0.01"
```

The `--assert <condition>` flag exits non-zero if the condition fails — useful in CI:

```bash
monoscope metrics query "p99(http.server.duration)" --assert "< 500" || echo "latency too high"
```

Options: `--since`, `--from`, `--to`, `--assert <condition>`.

### Chart (sparkline)

```bash
monoscope metrics chart "request_rate" --since 2h
monoscope metrics chart "cpu_usage" --watch 30s    # refresh every 30 seconds
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
export MONO_API_KEY=$(cat ~/.monoscope-key-new)
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

Fetch the telemetry field schema for the current project:

```bash
monoscope schema
monoscope schema -o json | jq '.fields[].name'
```

---

## Shell Completions

```bash
monoscope completion bash >> ~/.bashrc
monoscope completion zsh  >> ~/.zshrc
monoscope completion fish >> ~/.config/fish/completions/monoscope.fish
```

---

## CI / Automation

Export these in CI environments:

```bash
export MONO_API_KEY=<your-api-key>
export MONO_PROJECT=<project-uuid>
```

JSON is the default when stdout is not a TTY or `CI` is set. Use `jq` for scripting:

```bash
# Check error rate in CI
monoscope metrics query "error_rate" --assert "< 0.01"

# Get open issue count
monoscope issues list --status open -o json | jq '.total'

# Apply all monitors from a directory (idempotent)
monoscope monitors apply .monoscope/monitors/
```
