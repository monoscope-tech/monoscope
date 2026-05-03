# Monoscope MCP Server

Monoscope exposes itself as a [Model Context Protocol](https://modelcontextprotocol.io/) server at:

```
POST https://api.monoscope.tech/api/v1/mcp
```

(For self-hosted, swap the host — the path is the same.)

Every operation in the public REST API is surfaced as a typed MCP tool, so any
MCP-aware client (Claude Desktop, Cursor, Cline, custom agents) can search
events, manage monitors and dashboards, triage issues, and more — without
shelling out to the CLI.

## Authentication

Pass an API key as a bearer token. Mint one with `monoscope api-keys create`
(or via the UI under Settings → API Keys).

```
Authorization: Bearer <your-api-key>
```

Requests without a valid key get `401`.

## Transport

HTTP JSON-RPC 2.0. Standard MCP methods are supported:

| Method | Purpose |
|---|---|
| `initialize` | Handshake — returns `protocolVersion` (`2025-06-18`) and server info |
| `tools/list` | List every available tool with its JSON Schema |
| `tools/call` | Invoke a tool by name with arguments |
| `notifications/*` | Acknowledged with an empty body (HTTP transport semantics) |

## Client setup

### Claude Desktop / Claude Code

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

### Cursor / Cline / other MCP clients

Use the same URL + bearer header. Most clients accept either an `mcp.json`
config or a settings UI for adding HTTP MCP servers.

## Tools

Tool names follow the verb-first snake_case convention used by Sentry,
Grafana, Datadog, and Honeycomb (`list_monitors`, `search_events`,
`mute_monitor`, …). Run `tools/list` for the live, authoritative set.

**REST-derived tools** mirror the OpenAPI spec — `list_events`,
`search_events`, `get_schema`, `list_facets`, `list_monitors`,
`create_monitor`, `get_monitor`, `mute_monitor`, `delete_monitor`,
`list_dashboards`, `apply_dashboard`, `get_dashboard_yaml`, `list_api_keys`,
`whoami`, `get_project`, `list_endpoints`, `list_log_patterns`,
`list_issues`, `get_issue`, `list_teams`, `create_team`, `list_members`,
`create_share_link`, … — every public route becomes a tool automatically.

**Composite (workflow) tools** bundle several internal calls into a single,
high-leverage action:

| Tool | What it does |
|---|---|
| `find_error_patterns` | Top established log patterns ranked by current-hour count |
| `search_events_nl` | Natural-language → KQL via the agentic query planner, then runs the search |
| `analyze_issue` | Fetches an issue and asks the LLM for probable cause, signals, and next steps |

## Example: list tools

```bash
curl -s https://api.monoscope.tech/api/v1/mcp \
  -H "Authorization: Bearer $MONOSCOPE_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | jq '.result.tools[].name'
```

## Example: call a tool

```bash
curl -s https://api.monoscope.tech/api/v1/mcp \
  -H "Authorization: Bearer $MONOSCOPE_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/call",
    "params": {
      "name": "search_events",
      "arguments": {
        "body": { "query": "severity.text==\"error\"", "since": "1h" }
      }
    }
  }' | jq
```

REST tool responses include a 64 KB-truncated text view in `content` and the
full untruncated JSON in `structuredContent` for typed clients.

## Errors

- Tool execution errors surface as MCP tool results with `isError: true`
  (not JSON-RPC errors) — agents can read the message and retry.
- Unknown tool names return `isError: true` rather than a JSON-RPC error,
  matching the MCP spec.
- Each tool call has a 30-second internal timeout so a hung sub-handler
  cannot block the endpoint.
