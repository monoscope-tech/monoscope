---
name: kql-reference
description: Reference for Monoscope's KQL (Kusto Query Language) dialect used in log and trace search queries. Use when writing, explaining, or debugging KQL queries in monoscope — including the monoscope CLI search commands, the Log Explorer UI, and AI query generation. Covers operators, aggregations, time binning, string functions, and visualization types.
allowed-tools: Bash
---

# Monoscope KQL Reference

Monoscope uses a KQL (Kusto Query Language) dialect for filtering and aggregating logs, traces, and spans. This is the same syntax used by `monoscope logs search`, `monoscope events search`, `monoscope traces search`, and the Log Explorer UI.

## Operators

### Comparison
```
==   !=   >   <   >=   <=
```

### Set operations
```
in      !in
```
```
level in ("ERROR", "FATAL")
attributes.http.request.method !in ("GET", "HEAD")
```

### Text search
```
has       — case-insensitive whole-word match
!has      — negated
has_any   — matches any word from a list
has_all   — matches all words from a list
```
```
body has "timeout"
attributes.exception.message has_any ["timeout", "connection", "network"]
```

### String operations
```
contains      !contains      — substring match
startswith    !startswith    — prefix match
endswith      !endswith      — suffix match
matches  =~                  — regex match
```
```
attributes.url.path startswith "/api/"
attributes.user.email endswith "@company.com"
name contains "database"
attributes.user.email matches /.*@corp\.com/
```

### Logical
```
AND   OR   (also lowercase: and, or)
```
Parentheses for grouping:
```
(level == "ERROR" OR duration > 5s) AND resource.service.name != null
```

### Null checks
```
field != null    — field exists and has a value
field == null    — field is absent or null
```

## Duration values

Durations are written as a number followed by a unit. They represent nanoseconds internally.

| Unit | Suffix | Example |
|------|--------|---------|
| Nanoseconds | ns | `100ns` |
| Microseconds | us | `500us` |
| Milliseconds | ms | `200ms` |
| Seconds | s | `5s` |
| Minutes | m | `2m` |
| Hours | h | `1h` |
| Days | d | `7d` |

```
duration > 1s               — spans taking longer than 1 second
duration > 500ms            — slower than 500ms
duration > 5s AND kind == "span"
```

## Aggregations (summarize)

Pipe a filter into a `summarize` clause for aggregation:

```
<filter> | summarize <agg>() by <grouping>
```

### Aggregation functions

| Function | Description |
|---|---|
| `count()` | Total count |
| `countif(expr)` | Count where expr is true |
| `dcount(field)` | Distinct count |
| `sum(field)` | Sum |
| `avg(field)` | Average |
| `min(field)` | Minimum |
| `max(field)` | Maximum |
| `median(field)` | Median |
| `stdev(field)` | Standard deviation |
| `percentile(field, N)` | Nth percentile (e.g., p99 = `percentile(duration, 99)`) |
| `percentiles(field, p1, p2, ...)` | Multiple percentiles at once |

### Examples

```
level == "ERROR" | summarize count() by resource.service.name
| summarize avg(duration) by resource.service.name
| summarize percentile(duration, 99) by bin_auto(timestamp)
| summarize count(), avg(duration) by resource.service.name
```

## Time binning

Use `bin_auto(timestamp)` by default — the system picks the interval automatically based on the selected time range:

```
level == "ERROR" | summarize count() by bin_auto(timestamp)
```

Use `bin(timestamp, interval)` only when the user explicitly specifies an interval:

```
| summarize count() by bin(timestamp, 1h)    — "show by hour"
| summarize count() by bin(timestamp, 30s)   — "per 30 seconds"
| summarize count() by bin(timestamp, 5m)    — "in 5 minute buckets"
```

**For categorical grouping (by service, method, status, etc.) — do NOT use `bin()`:**

```
| summarize count() by resource.service.name          ✓ distribution
| summarize count() by bin_auto(timestamp)             ✓ timeseries
| summarize count() by resource.service.name, bin_auto(timestamp)  ✓ both
```

## Scalar functions

```
coalesce(field1, field2, ...)    — first non-null value
strcat(a, b, ...)                — string concatenation
iff(condition, then, else)       — conditional
toint(field)                     — cast to integer
tofloat(field)                   — cast to float
tostring(field)                  — cast to string
tobool(field)                    — cast to boolean
isnull(field)                    — true if null
isnotnull(field)                 — true if not null
isempty(field)                   — true if null or empty string
isnotempty(field)                — true if not null and not empty
```

## Field paths

Fields use dot notation. Array indexing is supported:

```
attributes.http.response.status_code
resource.service.name
context.trace_id
attributes.exception.message
body.message[0].tags
request_body.items[*].id
```

## Core fields (schema reference)

| Field | Type | Notes |
|---|---|---|
| `timestamp` | string | When the event occurred |
| `duration` | duration | Span duration in nanoseconds |
| `level` | string | `TRACE` `DEBUG` `INFO` `WARN` `ERROR` `FATAL` |
| `kind` | string | `logs` `span` `request` |
| `status_code` | string | `OK` `ERROR` `UNSET` |
| `name` | string | Span or operation name |
| `body` | object | Log body content |
| `parent_id` | string | Parent span ID (null for root spans) |
| `context.trace_id` | string | Trace correlation ID |
| `context.span_id` | string | Span ID |
| `resource.service.name` | string | Service name |
| `resource.service.version` | string | Service version |
| `attributes.http.request.method` | string | `GET` `POST` `PUT` `DELETE` … |
| `attributes.http.response.status_code` | number | HTTP status (200, 404, 500 …) |
| `attributes.url.path` | string | URL path |
| `attributes.url.full` | string | Full URL |
| `attributes.exception.type` | string | Exception class name |
| `attributes.exception.message` | string | Exception message |
| `attributes.exception.stacktrace` | string | Stack trace |
| `attributes.error.type` | string | Error type |
| `attributes.db.system.name` | string | `postgresql` `mysql` … |
| `attributes.db.operation.name` | string | `SELECT` `INSERT` … |
| `attributes.db.query.text` | string | Raw query text |
| `attributes.user.id` | string | User ID |
| `attributes.user.email` | string | User email |
| `attributes.session.id` | string | Session ID |
| `severity.text` | string | `TRACE` `DEBUG` `INFO` `WARN` `ERROR` `FATAL` |

Use `monoscope schema` to discover the full field list for your project.

## Example queries

```
# All errors
level == "ERROR"

# HTTP 5xx errors
attributes.http.response.status_code >= 500

# Slow spans
duration > 1s AND kind == "span"

# Errors in a specific service
level == "ERROR" AND resource.service.name == "checkout-api"

# Errors over time (timeseries chart)
level == "ERROR" | summarize count() by bin_auto(timestamp)

# Request count by service (distribution chart)
| summarize count() by resource.service.name

# p99 latency by service
| summarize percentile(duration, 99) by resource.service.name

# Network-related exceptions
attributes.exception.message has_any ["timeout", "connection", "network"]

# Successful POST requests
attributes.http.request.method == "POST" AND attributes.http.response.status_code < 400

# Database queries taking over 200ms
attributes.db.operation.name != null AND duration > 200ms

# Auth service excluding debug noise
resource.service.name startswith "auth" AND level !in ("DEBUG", "TRACE")

# Root spans only (no parent)
kind == "span" AND parent_id == null

# Errors or slow requests from named services
(level == "ERROR" OR duration > 5s) AND resource.service.name != null
```

## Important constraints

- **Do not invent field names.** Only use fields from the schema. Use `monoscope schema` to discover available fields.
- **Do not filter by timestamp in KQL.** Time range is controlled separately via `--since`, `--from`, `--to` flags on the CLI, or the time picker in the UI.
- **`has` is for word search**, `contains` is for substring. `has "error"` won't match `"errors"` but `contains "error"` will.
- **Duration fields** (`duration`) compare in nanoseconds but accept human-readable suffixes (`1s`, `200ms`).

## CLI usage

```bash
# Basic search
monoscope logs search "level == \"ERROR\"" --since 1h

# With service filter (composes with the query)
monoscope logs search "duration > 1s" --service payment-api

# Aggregation query
monoscope logs search "level == \"ERROR\" | summarize count() by resource.service.name" -o json

# Discover available fields
monoscope schema -o json | jq '.fields | keys[]'
```
