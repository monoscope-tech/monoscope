# Monoscope Skills

This plugin provides agent skills for working with the Monoscope observability platform via the `monoscope` CLI.

## Available Skills

### `kql-reference`

Full reference for Monoscope's KQL dialect — operators, aggregations, time binning, string functions, scalar functions, schema fields, and example queries. Consult this before writing any KQL query. This skill is the source of truth for query syntax.

**Triggers:** any time you need to write, read, or explain a KQL query in monoscope

---

### `investigate`

Use when debugging a production incident, searching logs, inspecting traces, or checking metrics. Teaches the agent to orient with `services list`, search with `logs search` / `traces get --tree`, monitor live with `events tail`, and check SLOs with `metrics query --assert`.

**Triggers:** "investigate the error in...", "look into why...", "find logs for...", "what happened at...", "check the error rate for..."

### `triage`

Use for on-call sweeps: reviewing open issues, silencing noisy log patterns, muting flapping monitors, and bulk-actioning alert queues.

**Triggers:** "triage open issues", "do an on-call sweep", "acknowledge the alerts", "silence the noisy monitors", "clear the issue queue"

## Prerequisites

Both skills require:

```bash
export MONO_API_KEY=<your-api-key>
export MONO_PROJECT=<your-project-uuid>
```

The agent will remind you if these are missing. You can also set them permanently:

```bash
monoscope config set api_key <your-api-key>
monoscope config set project <your-project-uuid>
```

## Installing the CLI

```bash
curl -fsSL https://monoscope.tech/install.sh | bash
monoscope auth login
```
