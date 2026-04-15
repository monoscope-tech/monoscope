# Monoscope CLI Cookbook

The `monoscope` CLI is a thin wrapper over the REST API (`/api/v1/*`). It uses
API-key authentication (`Bearer`) via `MONO_API_KEY` and scopes requests to a
project via `MONO_PROJECT` (UUID). Every subcommand supports
`--output/-o {json|yaml|table}` and `--project/-p <pid>` overrides.

## Provisioning from scratch

```bash
# 1. Create an API key for this project (print once, stash it).
export MONO_PROJECT=<project-uuid>
monoscope api-keys create "ops-cli" -o json | jq -r .key > ~/.monoscope-key
export MONO_API_KEY=$(cat ~/.monoscope-key)

# 2. Apply dashboards (idempotent — upsert keyed by file_path).
monoscope dashboards apply docs/examples/dashboards/

# 3. Apply monitors — accepts a file or a directory of .yaml/.yml/.json.
# Monitor files with an `id:` field replace (PUT) that row; without an id,
# every apply creates a new monitor, so pin ids for idempotency.
monoscope monitors apply docs/examples/monitors/
```

Verify:

```bash
monoscope dashboards list -o table
monoscope monitors list -o table
```

## Rotating an API key

```bash
# Issue a new one, start using it, then deactivate the old one.
monoscope api-keys create "ops-cli-2025-04" -o json | jq -r .key > ~/.monoscope-key-new
export MONO_API_KEY=$(cat ~/.monoscope-key-new)
monoscope api-keys deactivate <old-key-id>
```

`api-keys revoke` is kept as a deprecated alias for `deactivate`. To fully
remove a deactivated row, use `api-keys delete`.

## On-call triage (monitors)

```bash
# Mute a flapping monitor for 30 minutes.
monoscope monitors mute <monitor-id> --for 30

# Resolve a batch.
monoscope monitors bulk resolve --ids id1,id2,id3

# Toggle a monitor on/off.
monoscope monitors toggle-active <monitor-id>
```

## Dashboard editing

```bash
# Dump current state for editing.
monoscope dashboards yaml <dashboard-id> > dash.yaml

# Replace full dashboard (PUT).
monoscope dashboards update <dashboard-id> dash.yaml

# Or PATCH only changed fields.
monoscope dashboards patch <dashboard-id> patch.yaml

# Widget-level operations.
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

## Sharing a single event

```bash
monoscope share-link create \
  --event-id $(uuidgen) \
  --created-at 2026-04-15T00:00:00Z \
  --type log
```

Returns `{ id, url }`; the share URL is valid for 48 hours.

## Bulk operations

Every bulk endpoint takes `--ids` (comma-separated). Server-side enums:

| Resource   | Supported actions                                            |
|------------|--------------------------------------------------------------|
| monitors   | delete, activate, deactivate, mute, unmute, resolve          |
| dashboards | delete                                                       |

```bash
monoscope monitors bulk mute --ids id1,id2 --duration 60
monoscope dashboards bulk delete --ids id1,id2
```

(Anomalies bulk acknowledge/archive is available via the API at `POST /api/v1/anomalies/bulk` but is not yet surfaced as a CLI subcommand.)

## Output modes

- `--output json` — machine-readable; ideal for piping into `jq`.
- `--output yaml` — config-friendly; round-trips through `apply`.
- `--output table` — default when stdout is a TTY.

Agents/CI should export `MONO_AGENT_MODE=1` (or rely on `CI`) to force JSON output
without the `--output json` flag on every call.
