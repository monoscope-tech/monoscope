# monoscope-skills

Claude Code skills for the [Monoscope](https://monoscope.tech) observability platform. Gives Claude the knowledge to use the `monoscope` CLI to investigate production incidents, triage issues and alerts, and manage your observability resources.

## Installation

### Claude Code

```bash
claude plugin marketplace add monoscope-tech/skills
claude plugin install monoscope-skills@monoscope-skills
```

### npx skills ecosystem

```bash
npx skills add monoscope-tech/skills
```

Works with Claude Code, Cursor, Cline, and GitHub Copilot.

## Setup

Install the CLI and authenticate:

```bash
curl -fsSL https://monoscope.tech/install.sh | bash
monoscope auth login
```

Set your project context (or use env vars per-session):

```bash
monoscope config set project <your-project-uuid>
```

## Skills

### `kql-reference`

Complete KQL syntax reference for Monoscope's query dialect — operators (`has`, `contains`, `startswith`, `matches`, `in`, `has_any` …), aggregations (`count`, `percentile`, `dcount` …), time binning (`bin_auto`, `bin`), scalar functions, schema fields, and 15+ example queries. Use this any time you need to write or understand a Monoscope query.

### `investigate`

Investigate production incidents — search logs, inspect traces, check metrics, watch live events.

**Example prompts:**
- "Investigate the 500 errors in the payment service over the last hour"
- "Find logs related to the timeout we saw at 10:34 UTC"
- "Check the error rate for checkout-api and show me what's failing"
- "What happened to the auth service between 14:00 and 14:30?"

### `triage`

Sweep open issues, acknowledge log patterns, mute flapping monitors, and bulk-action alert queues.

**Example prompts:**
- "Triage all open issues for the payment service"
- "Do an on-call sweep and clear the alert queue"
- "Mute the noisy database connection monitor for 30 minutes"
- "Bulk acknowledge the runtime exception issues from last night"

## Environment variables

| Variable | Description |
|---|---|
| `MONO_API_KEY` | API key for authentication |
| `MONO_PROJECT` | Default project UUID |
| `MONO_API_URL` | API base URL (default: `https://api.monoscope.tech`) |

## License

Apache-2.0
