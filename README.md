<div align="center">

<img src="/docs/monoscope.svg" alt="Monoscope Logo" width="250" />

### Open-source observability platform with S3 storage

Ingest and explore logs, traces, and metrics stored in your S3 buckets. Query with natural language. Create AI agents that detect anomalies and send daily/weekly reports to your inbox.

[![GitHub Release](https://img.shields.io/github/v/release/monoscope-tech/monoscope?include_prereleases)](https://github.com/monoscope-tech/monoscope/releases)
[![License](https://img.shields.io/badge/license-AGPL--3.0-blue.svg)](LICENSE)
[![Haskell](https://img.shields.io/badge/Built%20with-Haskell-5e5086?logo=haskell)](https://www.haskell.org/)
[![Discord](https://img.shields.io/discord/904634773329297429?color=7289da&logo=discord&logoColor=white)](https://discord.gg/BSFCaUHxt4)

[**Website**](https://monoscope.tech) • [**Playground**](https://app.monoscope.tech/p/00000000-0000-0000-0000-000000000000/log_explorer) • [**Discord**](https://discord.gg/BSFCaUHxt4) • [**Twitter**](https://x.com/monoscope-tech) • [**Documentation**](https://docs.monoscope.tech)

</div>

<div align="center" style="margin-top: 1em; margin-bottom: 1em;">
<a href="#what-is-monoscope">What is Monoscope?</a> • <a href="#cloud-vs-self-hosted">Cloud vs Self-hosted</a> • <a href="#quick-start">Quick Start</a> • <a href="#ai-agents--reports">AI Agents</a> • <a href="#roadmap">Roadmap</a>
</div>

<br />

<div align="center">
  <img width="100%" src="https://github.com/user-attachments/assets/6175c23b-f3ac-450a-9ae3-4b86371f4f54" alt="Monoscope Dashboard" />
</div>

<br/>

## What is Monoscope?

Monoscope is an open-source observability platform that stores your telemetry data in S3-compatible storage. Self-host it or use our [cloud offering](https://monoscope.tech).

**Core capabilities:**

- 💰 **S3 storage** — Store years of logs, metrics, and traces affordably in your own S3 buckets
- 💬 **Natural language queries** — Search your data using plain English via LLMs
- 🤖 **AI agents** — Create agents that run on a schedule to detect anomalies and surface insights
- 📧 **Email reports** — Receive daily/weekly summaries of important events and anomalies
- 🔭 **OpenTelemetry native** — 750+ integrations out of the box
- ⚡ **Live tail** — Stream logs and traces in real-time
- 🕵️ **Unified view** — Correlate logs, metrics, traces, and session replays in one place

<br/>

## Cloud vs Self-hosted

In both options, you bring your own S3 buckets—your data stays yours.

| | Cloud | Self-hosted |
|---|---|---|
| **Storage** | Your S3 buckets | Your S3 buckets |
| **Compute** | Managed by us | You manage |
| **Auth & SSO** | Built-in | DIY |
| **Alert channels** | Slack, PagerDuty, etc. | Basic email |
| **Pricing** | [Usage-based](https://monoscope.tech/pricing) | Free (AGPL-3.0) |

→ [Start free on Cloud](https://monoscope.tech) or continue below to self-host.

<br/>

## Quick Start

```bash
git clone https://github.com/monoscope-tech/monoscope.git
cd monoscope
docker-compose up
```

Visit `http://localhost:8080` (default: admin/changeme)

### Send Test Data

Populate your dashboard with test telemetry:

```bash
# Install telemetrygen
go install github.com/open-telemetry/opentelemetry-collector-contrib/cmd/telemetrygen@latest

# Send test traces (replace YOUR_API_KEY from the UI)
telemetrygen traces --otlp-endpoint localhost:4317 --otlp-insecure \
  --otlp-header 'Authorization="Bearer YOUR_API_KEY"' --traces 10
```

<br/>

## CLI

Manage your Monoscope project from the terminal — search logs, query metrics, manage monitors and dashboards, triage issues, and more.

```bash
curl -fsSL https://monoscope.tech/install.sh | bash
monoscope auth login
```

See the [CLI reference](docs/cli.md) for the full command list.

### Agentic pipeline

Every CLI command emits a stable JSON envelope, so you can chain discovery →
search → triage without manual munging. This is the same pipeline a Claude
Code skill runs end-to-end:

```bash
# 1. What services exist? (Precomputed; no aggregation query needed.)
SVC=$(monoscope facets resource.service.name --top 1 \
        | jq -r '.["resource.service.name"][0].value')

# 2. Grab one error event from that service — id only, ready to chain.
ID=$(monoscope logs search 'severity.text=="error"' \
        --service "$SVC" --first --id-only)

# 3. Pull the surrounding 5 minutes of traffic, with a per-trace summary
#    showing which other services were affected.
monoscope events context --window 5m --summary \
  --at "$(monoscope events get "$ID" | jq -r .timestamp)" \
  | jq '.traces | sort_by(-.error_count) | .[0:3]'

# 4. Acknowledge the open issue once you have a hypothesis.
monoscope issues list --service "$SVC" --status open \
  | jq -r '.data[].id' | head -3 \
  | xargs -I {} monoscope issues ack {}
```

Each step's output shape is documented and stable:

| Command | Envelope |
|---|---|
| `facets [FIELD]` | `{<field_path>: [{value, count}, ...]}` |
| `events search` (and `logs`/`traces`) | `{events: [...], count, has_more, cursor}` |
| `events context --summary` | `{events, count, traces: [{trace_id, services, span_count, error_count}]}` |
| `issues list`, `monitors list`, ... | `{data: [...], pagination: {has_more, total, cursor, page, per_page}}` |
| `auth status` (agent mode) | `{authenticated, method, api_url, project}` |

Set `MONOSCOPE_AGENT_MODE=1` (or run with `--agent`) to force JSON output and
disable interactive prompts — auto-detected when `CI` or `CLAUDE_CODE` is set.

### Claude Code Skills

Let Claude investigate incidents, triage alerts, and write KQL queries using the `monoscope` CLI — install the skills plugin:

```bash
# Claude Code
claude plugin marketplace add monoscope-tech/skills
claude plugin install monoscope-skills@monoscope-skills

# or via npx (Cursor, Cline, Copilot, and other agents)
npx skills add monoscope-tech/skills
```

Restart Claude Code after installation. Skills activate automatically when relevant — e.g. *"investigate the 500 errors in payment-api"* or *"do an on-call sweep"*.

See [github.com/monoscope-tech/skills](https://github.com/monoscope-tech/skills) for the full skill list and documentation.

<br/>

## MCP Server

Monoscope exposes itself as a [Model Context Protocol](https://modelcontextprotocol.io/) server at `/api/v1/mcp`, so any MCP-aware client (Claude Desktop, Cursor, Cline, custom agents) can search events, manage monitors and dashboards, triage issues, and run composite workflows like `search_events_nl` (natural-language → KQL) and `analyze_issue` (LLM-assisted root-cause).

```json
{
  "mcpServers": {
    "monoscope": {
      "url": "https://api.monoscope.tech/api/v1/mcp",
      "headers": { "Authorization": "Bearer YOUR_API_KEY" }
    }
  }
}
```

Every public REST endpoint is auto-registered as a verb-first tool (`list_monitors`, `search_events`, `mute_monitor`, …). See the [MCP reference](docs/mcp.md) for the full protocol, tool catalog, and examples.

<br/>

## Integration

### Auto-instrument your apps

<details>
<summary><b>Python</b></summary>

```bash
pip install opentelemetry-distro opentelemetry-exporter-otlp
opentelemetry-bootstrap -a install
OTEL_SERVICE_NAME="my-app" \
OTEL_EXPORTER_OTLP_ENDPOINT="http://localhost:4317" \
opentelemetry-instrument python myapp.py
```

</details>

<details>
<summary><b>Node.js</b></summary>

```bash
npm install --save @opentelemetry/auto-instrumentations-node
OTEL_SERVICE_NAME="my-app" \
OTEL_EXPORTER_OTLP_ENDPOINT="http://localhost:4317" \
node --require @opentelemetry/auto-instrumentations-node/register app.js
```

</details>

<details>
<summary><b>Java</b></summary>

```bash
curl -L https://github.com/open-telemetry/opentelemetry-java-instrumentation/releases/latest/download/opentelemetry-javaagent.jar -o otel-agent.jar
OTEL_SERVICE_NAME="my-app" \
OTEL_EXPORTER_OTLP_ENDPOINT="http://localhost:4317" \
java -javaagent:otel-agent.jar -jar myapp.jar
```

</details>

<details>
<summary><b>Kubernetes</b></summary>

```bash
# Install OpenTelemetry Operator
kubectl apply -f https://github.com/open-telemetry/opentelemetry-operator/releases/latest/download/opentelemetry-operator.yaml

# Configure auto-instrumentation
kubectl apply -f - <<EOF
apiVersion: opentelemetry.io/v1alpha1
kind: Instrumentation
metadata:
  name: my-instrumentation
spec:
  exporter:
    endpoint: http://monoscope:4317
  propagators:
    - tracecontext
    - baggage
EOF

# Annotate your deployments for auto-instrumentation
kubectl patch deployment my-app -p \
  '{"spec":{"template":{"metadata":{"annotations":{"instrumentation.opentelemetry.io/inject-java":"my-instrumentation"}}}}}'
```

</details>

<br/>

## Natural Language Queries

Query your telemetry data in plain English:

- "Show me all errors in the payment service in the last hour"
- "What caused the spike in response time yesterday?"
- "Which endpoints have the highest p99 latency?"

<br/>

## AI Agents & Reports

Create AI agents that monitor your systems on a schedule:

- **Scheduled analysis** — Agents run at intervals you define (hourly, daily, weekly)
- **Anomaly detection** — Automatically surface unusual patterns in logs, metrics, and traces
- **Email reports** — Receive summaries of important events and insights directly in your inbox
- **Customizable focus** — Configure agents to watch specific services, error types, or metrics

<br/>

## Architecture

```mermaid
graph LR
    A[Your Apps] -->|Logs/Metrics/Traces| B[Ingestion API]
    B --> C[TimeFusion Engine]
    C --> D[(S3 Storage)]
    D --> E[Query Engine]
    E --> F[Dashboards]
    D --> G[AI Agent Scheduler]
    G -->|LLM Analysis| H[Anomaly Detection]
    H --> I[Email Reports]
    H --> J[Alert Channels]
```

### Powered by TimeFusion

Monoscope is built on [**TimeFusion**](https://github.com/monoscope-tech/timefusion), our open-source time-series database for observability workloads.

| | |
|---|---|
| 🗄️ **S3-native** | Data lives in your S3 buckets—no vendor lock-in |
| 🐘 **PostgreSQL compatible** | Use any Postgres client or driver |
| ⚡ **500K+ events/sec** | Columnar storage with Apache Arrow |
| 💵 **Pay only for S3** | No expensive proprietary storage fees |

<br/>

## How It Compares

| Feature                | Monoscope   | Datadog   | Elastic | Prometheus |
| ---------------------- | ----------- | --------- | ------- | ---------- |
| S3/Object Storage      | ✅ Native   | ❌        | ✅      | ✅         |
| Natural Language Query | ✅          | ❌        | ❌      | ❌         |
| AI Agents & Reports    | ✅ Built-in | ❌ Add-on | ❌      | ❌         |
| Open Source            | ✅ AGPL-3.0 | ❌        | ✅      | ✅         |
| Self-hostable          | ✅          | ❌        | ✅      | ✅         |

<br/>

## Screenshots

### Log Explorer - Unified View
Logs and trace spans displayed together in context for complete observability.

<img src="https://github.com/user-attachments/assets/6175c23b-f3ac-450a-9ae3-4b86371f4f54" alt="Log Explorer Main View" width="100%" />

<br/>

### Trace Context Integration
See detailed trace information alongside logs for debugging complex distributed systems.

<img src="https://github.com/user-attachments/assets/baf78e6f-2b75-4b6f-be56-94a0b3da9f31" alt="Log Explorer with Trace Context" width="100%" />

<br/>

### Dashboard Analytics
Real-time metrics and performance monitoring with AI-powered insights.

<img src="https://github.com/user-attachments/assets/3dae136a-e627-4278-91a1-81c5af5f8cd6" alt="Dashboard Analytics View" width="100%" />

<br/>

## Trusted by Leading Companies

<div align="center">
  <table>
    <tr>
      <td align="center" width="120" height="60"><img src="https://monoscope.tech/assets/img/customers/andela.svg" alt="Andela" height="30" /></td>
      <td align="center" width="120" height="60"><img src="https://monoscope.tech/assets/img/customers/partna.svg" alt="Partna" height="30" /></td>
      <td align="center" width="120" height="60"><img src="https://monoscope.tech/assets/img/customers/grovepay.svg" alt="GrovePay" height="30" /></td>
      <td align="center" width="120" height="60"><img src="https://monoscope.tech/assets/img/customers/sameday.svg" alt="SameDay" height="30" /></td>
    </tr>
    <tr>
      <td align="center" width="120" height="60"><img src="https://monoscope.tech/assets/img/customers/platnova.png" alt="Platnova" height="30" /></td>
      <td align="center" width="120" height="60"><img src="https://monoscope.tech/assets/img/customers/payfonte.svg" alt="PayFonte" height="30" /></td>
      <td align="center" width="120" height="60"><img src="https://monoscope.tech/assets/img/customers/thepeer.svg" alt="ThePeer" height="30" /></td>
      <td align="center" width="120" height="60"><img src="https://monoscope.tech/assets/img/customers/blockradar-full.svg" alt="BlockRadar" height="30" /></td>
    </tr>
  </table>
</div>

> "Monoscope notifies us about any slight change on the system. Features that would cost us a lot more elsewhere." — **Samuel Joseph, Woodcore**

<br/>

## Documentation

- [Getting Started Guide](docs/getting-started.md)
- [CLI Reference](docs/cli.md)
- [MCP Server](docs/mcp.md)
- [Configuration](docs/configuration.md)
- [Kubernetes Guide](docs/kubernetes.md)
- [Development Guide](docs/DEVELOPMENT.md)

<br/>

## Roadmap

- [x] Custom dashboards builder
- [ ] More out-of-the-box dashboards
- [ ] AIOps workflow builder
- [ ] Full migration to TimeFusion storage engine
- [ ] Metrics aggregation rules
- [ ] Multi-tenant workspace support
- [ ] More alert channel integrations

See our [public roadmap](https://github.com/monoscope-tech/monoscope/projects) for details and to vote on features.

<br/>

## Community

💬 [Discord](https://discord.gg/BSFCaUHxt4) • 🐛 [Issues](https://github.com/monoscope-tech/monoscope/issues) • 🐦 [Twitter](https://x.com/monoscope-tech)

<br/>

## License

AGPL-3.0. See [LICENSE](LICENSE) for details.

For commercial licensing options, contact us at [hello@monoscope.tech](mailto:hello@monoscope.tech).

---

<div align="center">
  <a href="https://github.com/monoscope-tech/monoscope"><img src="https://img.shields.io/github/stars/monoscope-tech/monoscope?style=social" alt="Star on GitHub" /></a>
</div>

