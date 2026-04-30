---
name: triage
description: Triage open issues, log patterns, and alert monitors using the monoscope CLI. Use when asked to do an on-call sweep, review alerts, acknowledge issues, silence noisy monitors, bulk-dismiss log patterns, or clear the alert queue. Requires MONO_API_KEY and MONO_PROJECT environment variables.
allowed-tools: Bash
---

# Monoscope: Triage

Use the `monoscope` CLI to sweep open issues, noisy log patterns, and misfiring monitors — acknowledging, archiving, muting, or resolving as appropriate.

## Prerequisites

```bash
export MONO_API_KEY=<your-api-key>
export MONO_PROJECT=<your-project-uuid>
```

Verify with: `monoscope auth status`

## Instructions

### 1. Review open issues

```bash
# All open issues
monoscope issues list --status open

# Filter by service or type
monoscope issues list --status open --service payment-api
monoscope issues list --status open --type runtime_exception

# Get full detail on a specific issue
monoscope issues get <issue-id>
```

**Disposition options:**
- **Acknowledge** — you've seen it and are working on it
- **Archive** — not actionable, resolved externally, or won't fix

```bash
monoscope issues ack <issue-id>
monoscope issues archive <issue-id>

# Bulk operations (comma-separated IDs)
monoscope issues bulk acknowledge --ids id1,id2,id3
monoscope issues bulk archive --ids id4,id5
```

### 2. Review log patterns

Log patterns are automatically extracted recurring patterns in your logs. Noisy or expected patterns should be acknowledged or ignored:

```bash
# List patterns (most frequent first)
monoscope log-patterns list --per-page 50

# Inspect a specific pattern
monoscope log-patterns get <pattern-id>

# Acknowledge (mark as seen/known)
monoscope log-patterns ack <pattern-id>

# Bulk acknowledge or ignore
monoscope log-patterns bulk ack --ids 1,2,3
monoscope log-patterns bulk ignore --ids 4,5,6
```

### 3. Review monitors and alerts

```bash
# See all monitors and their current state
monoscope monitors list

# Check a specific monitor
monoscope monitors get <monitor-id>
```

**Mute a flapping monitor:**

```bash
# Mute for 30 minutes while investigating
monoscope monitors mute <monitor-id> --for 30

# Mute multiple monitors
monoscope monitors bulk mute --ids id1,id2 --duration 60

# Unmute when resolved
monoscope monitors unmute <monitor-id>
```

**Resolve a firing monitor:**

```bash
monoscope monitors resolve <monitor-id>
```

**Disable a monitor that's no longer relevant:**

```bash
monoscope monitors toggle-active <monitor-id>
```

**Bulk actions on monitors:**

```bash
# Bulk resolve, activate, deactivate, mute, or unmute
monoscope monitors bulk resolve --ids id1,id2,id3
monoscope monitors bulk deactivate --ids id4,id5
```

### 4. Review API endpoints (optional)

If triage involves endpoint-level anomalies:

```bash
monoscope endpoints list --search '/payments'
monoscope endpoints get <endpoint-id>
```

## Output

After triage, produce a structured summary:

1. **Issues** — count reviewed, how many acknowledged vs archived, any that need follow-up
2. **Log patterns** — patterns silenced or acknowledged, any new patterns worth investigating
3. **Monitors** — muted/resolved/toggled, and why
4. **Needs human review** — anything too complex or ambiguous to action automatically

## Guidelines

- **Acknowledge** issues you're actively working; **archive** those that are resolved or noise
- **Ignore** log patterns that are expected background noise (health checks, periodic jobs); **ack** known issues being tracked
- Only **mute** a monitor with a time limit (`--for N`) — never silence indefinitely without a reason
- **Resolve** a monitor only when you've confirmed the underlying condition is gone
- Use bulk operations freely for clear-cut cases; single operations for anything ambiguous
- Always produce a triage summary so the team knows what was actioned
