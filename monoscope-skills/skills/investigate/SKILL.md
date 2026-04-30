---
name: investigate
description: Investigate production incidents and debug issues using the monoscope CLI. Use when asked to look into an error, find logs, search traces, check metrics, debug a service, investigate an anomaly, or understand what happened during an incident. Requires MONO_API_KEY and MONO_PROJECT environment variables. For KQL query syntax, see the kql-reference skill.
allowed-tools: Bash
---

# Monoscope: Investigate

Use the `monoscope` CLI to investigate production issues — searching logs, inspecting traces, checking metrics, and correlating events across services.

## Prerequisites

```bash
export MONO_API_KEY=<your-api-key>
export MONO_PROJECT=<your-project-uuid>
```

Verify with: `monoscope auth status`

## Instructions

### 1. Orient — discover active services and schema

Before searching, understand what's running and what fields are available:

```bash
# See which services reported activity recently
monoscope services list --since 24h

# Discover available telemetry fields for this project
monoscope schema -o json | jq '.fields[].name'
```

### 2. Search logs

Use KQL queries. The query argument is positional; filters compose automatically with `--service` and `--level`:

```bash
# Recent errors across all services
monoscope logs search "error" --since 1h --level error

# Errors in a specific service
monoscope logs search "payment failed" --service checkout-api --since 2h

# Time-bounded window
monoscope logs search "timeout" --from 2026-04-15T10:00:00Z --to 2026-04-15T11:00:00Z

# Limit results and pipe to jq
monoscope logs search "exception" --since 30m --limit 50 -o json | jq '.[].message'
```

### 3. Inspect a specific trace

If you have a trace ID or event ID from a log entry:

```bash
# Get the full trace tree (spans in hierarchy)
monoscope traces get <trace-id> --tree

# Get a single event
monoscope events get <event-id>
```

### 4. Get temporal context

Understand what was happening around a specific moment:

```bash
# All events within 5 minutes of a timestamp
monoscope events context --at 2026-04-15T10:34:22Z --service checkout-api --window 5m

# Wider window across all services
monoscope events context --at 2026-04-15T10:34:22Z --window 15m
```

### 5. Live tail (if the issue is ongoing)

```bash
# Stream all events for a service
monoscope logs tail --service payment-api

# Filter to errors only
monoscope logs tail --service payment-api --level error

# Client-side pattern filter
monoscope logs tail --grep "connection refused"
```

### 6. Check metrics

```bash
# Error rate in the last hour
monoscope metrics query "error_rate" --since 1h

# Latency by service
monoscope metrics query "avg(http.server.duration) by resource.service.name" --since 1h

# Check against an SLO threshold
monoscope metrics query "error_rate" --since 30m --assert "< 0.01"

# Sparkline chart of request rate
monoscope metrics chart "request_rate" --since 2h
```

### 7. Check for related issues

```bash
# Open issues for the service
monoscope issues list --status open --service checkout-api

# Get full details on a specific issue
monoscope issues get <issue-id>
```

## Output

After investigation, produce a structured summary:

1. **What was searched** — time range, services, queries used
2. **What was found** — key log lines, error patterns, affected trace IDs
3. **Metrics state** — error rate, latency, any SLO breaches
4. **Likely cause** — hypothesis based on evidence
5. **Recommended action** — fix, escalate, or monitor

## Guidelines

- Always start with a time-bounded search (`--since`) rather than open-ended queries
- Use `monoscope schema` to find the right field names before constructing KQL filters
- Use `-o json | jq` when you need to extract specific fields for further processing
- When a trace ID appears in logs, always follow up with `--tree` to see the full span hierarchy
- If the issue is ongoing, `events tail` gives a live view; use `--grep` to reduce noise
- Check metrics alongside logs — a spike in error rate often pinpoints the blast radius
- Output table format is default on TTY; use `-o json` for programmatic processing
