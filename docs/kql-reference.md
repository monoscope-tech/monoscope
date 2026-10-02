# KQL Reference Guide

This document provides a complete reference for the Kusto Query Language (KQL) implementation in Monoscope. Use this guide to write powerful queries for searching and analyzing your logs, traces, and metrics.

---

## Table of Contents

- [Overview](#overview)
- [Query Structure](#query-structure)
- [Data Sources](#data-sources)
- [Comparison Operators](#comparison-operators)
- [String Operators](#string-operators)
- [Membership Operators](#membership-operators)
- [Logical Operators](#logical-operators)
- [Pipe Operators](#pipe-operators)
  - [where](#where)
  - [summarize](#summarize)
  - [extend](#extend)
  - [project](#project)
  - [sort by / order by](#sort-by--order-by)
  - [take / limit](#take--limit)
- [Aggregation Functions](#aggregation-functions)
  - [Basic Aggregations](#basic-aggregations)
  - [Percentile Functions](#percentile-functions)
  - [Conditional Aggregations](#conditional-aggregations)
- [Scalar Functions](#scalar-functions)
  - [String Functions](#string-functions)
  - [Conditional Functions](#conditional-functions)
  - [Type Casting Functions](#type-casting-functions)
  - [Formatting Functions](#formatting-functions)
- [Date and Time Functions](#date-and-time-functions)
  - [now()](#now)
  - [ago()](#ago)
  - [bin()](#bin)
  - [bin_auto()](#bin_auto)
- [Field References](#field-references)
  - [Simple Fields](#simple-fields)
  - [Nested JSON Fields](#nested-json-fields)
  - [Array Access](#array-access)
  - [OpenTelemetry Attributes](#opentelemetry-attributes)
- [Value Types](#value-types)
- [Named Aggregations](#named-aggregations)
- [Query Examples](#query-examples)
- [Limitations & Notes](#limitations--notes)

---

## Overview

Monoscope implements a subset of the Kusto Query Language (KQL) to provide a familiar and powerful way to query telemetry data. KQL uses a pipe-based syntax where data flows through a series of operations.

```kusto
spans
| where method == "GET" and status_code >= 400
| summarize count() by status_code
| sort by status_code desc
| take 10
```

---

## Query Structure

A KQL query consists of:

1. **Data source** (optional) - The table to query (`spans`, `metrics`)
2. **Filter expression** - Initial filtering conditions
3. **Pipe operators** - Additional transformations applied in sequence

```
[source] [filter_expression] [| pipe_operator]...
```

Each pipe (`|`) passes the output of one operation to the next.

---

## Data Sources

| Source | Aliases | Description |
|--------|---------|-------------|
| Spans/Logs | `spans`, `otlp_logs_and_spans` | OpenTelemetry logs and spans |
| Metrics | `metrics`, `telemetry.metrics` | Time-series metrics data |

**Examples:**
```kusto
spans | where method == "POST"

metrics | where metric_name == "http_requests_total" | summarize rate(value) by bin_auto(timestamp)
```

If no source is specified, the query applies to the default context (usually spans/logs).

---

## Comparison Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `==` | Equal to | `method == "GET"` |
| `!=` | Not equal to | `status_code != 200` |
| `>` | Greater than | `duration > 1000` |
| `<` | Less than | `response_time < 500` |
| `>=` | Greater than or equal | `status_code >= 400` |
| `<=` | Less than or equal | `duration <= 5000` |

**Examples:**
```kusto
status_code == 500
duration > 1000
response_time >= 100 and response_time <= 500
```

---

## String Operators

| Operator | Description | Case-sensitive match? | Example |
|----------|-------------|----------------|---------|
| `has` | Contains word (token match) | No | `message has "error"` |
| `!has` | Does not contain word | No | `message !has "debug"` |
| `contains` | Contains substring (any position) | No | `url contains "/api"` |
| `!contains` | Does not contain substring | No | `path !contains "/health"` |
| `startswith` | Starts with string | No | `endpoint startswith "/v1/"` |
| `!startswith` | Does not start with string | No | `path !startswith "/internal"` |
| `endswith` | Ends with string | No | `filename endswith ".json"` |
| `!endswith` | Does not end with string | No | `url !endswith ".css"` |
| `matches regex` | Matches a quoted regex pattern | Yes | `name matches regex "^GET /Cart$"` |
| `matches` | Matches a slash regex pattern | No | `email matches /.*@company\.com/` |
| `=~` | Matches a slash regex pattern (alternate syntax) | No | `body =~ /^ERROR:.*/` |

`Yes` means case-sensitive matching; `No` means case-insensitive matching. In particular, `matches regex` preserves case, while slash-regex `matches` and `=~` ignore it.

**`has` vs `contains`:**
- `has` matches whole words/tokens (bounded by non-alphanumeric characters)
- `contains` matches any substring regardless of word boundaries

```kusto
// has matches whole words only
"error code 500" has "error"      // ✓ matches (word boundary)
"errorcode 500" has "error"       // ✗ no match (no word boundary)

// contains matches any substring
"error code 500" contains "error" // ✓ matches
"errorcode 500" contains "error"  // ✓ matches
```

**Examples:**
```kusto
message has "timeout"
url contains "/api/v2"
service_name startswith "payment-"
request_body.msg =~ /^[A-Z]{3}-[0-9]+/
```

---

## Membership Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `in` | Value is in list | `method in ("GET", "POST")` |
| `!in` | Value is not in list | `status_code !in (200, 201, 204)` |
| `has_any` | Contains any of the values (OR) | `tags has_any ["error", "critical"]` |
| `has_all` | Contains all of the values (AND) | `message has_all ["user", "login"]` |

**Examples:**
```kusto
method in ("GET", "POST", "PUT")
status_code !in (200, 201, 204)
tags has_any ["urgent", "high-priority", "critical"]
description has_all ["database", "connection", "timeout"]
```

---

## Logical Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `and` / `AND` | Logical AND | `status >= 400 and method == "POST"` |
| `or` / `OR` | Logical OR | `status == 500 or status == 503` |
| `()` | Grouping | `(status >= 500) or (duration > 5000)` |

**Precedence:** Parentheses `()` > `and` > `or`

**Examples:**
```kusto
method == "GET" and status_code >= 400
status_code == 500 or status_code == 502 or status_code == 503
(method == "POST" and status_code >= 400) or (duration > 10000)
```

---

## Pipe Operators

### where

Filters results based on a condition.

```kusto
| where condition
```

**Examples:**
```kusto
spans | where method == "GET"
| where status_code >= 400 and duration > 1000
```

### summarize

Aggregates data using aggregation functions, optionally grouped by fields.

```kusto
| summarize aggregation [by grouping_fields]
```

**Examples:**
```kusto
| summarize count()
| summarize count() by method
| summarize avg(duration), max(duration) by service_name, method
| summarize count() by bin(timestamp, 1h)
```

### extend

Adds computed columns to the result set while preserving all existing columns.

```kusto
| extend column_name = expression [, column_name2 = expression2, ...]
```

**Examples:**
```kusto
// Add error category based on status code
| extend error_category = case(
    status_code >= 500, "5xx",
    status_code >= 400, "4xx",
    "ok"
  )

// Add duration in milliseconds
| extend duration_ms = round(duration / 1000000, 2)

// Add multiple columns
| extend
    is_error = status_code >= 400,
    status_text = tostring(status_code)
```

**After a `summarize`**, `extend` computes from the aggregates by name. The
computed column is added in front of the aggregates, so a time-series chart draws
it:

```kusto
| summarize errors = countif(status_code >= 400), total = count() by bin_auto(timestamp)
| extend error_pct = 100.0 * errors / total
```

A monitor alerts on the largest of all the aggregates. To alert on a ratio, write
it as the only aggregate: `| summarize 100.0 * countif(status_code >= 400) / count()`.

### project

Selects and renames specific columns, removing all others.

```kusto
| project column_name = expression [, column_name2 = expression2, ...]
```

**Examples:**
```kusto
// Select specific columns with renamed aliases
| project
    service = resource.service.name,
    total = count(),
    error_rate = round(countif(status_code == "ERROR") * 100.0 / count(), 2)

// Simple column selection
| project service_name = resource.service.name, method, status_code
```

### sort by / order by

Orders results by one or more fields.

```kusto
| sort by field [asc|desc]
| order by field [asc|desc]
```

**Examples:**
```kusto
| sort by timestamp desc
| order by duration desc, status_code asc
```

### take / limit

Limits the number of results returned.

```kusto
| take N
| limit N
```

**Examples:**
```kusto
| take 100
| limit 1000
```

---

## Aggregation Functions

### Basic Aggregations

| Function | Description | Example |
|----------|-------------|---------|
| `count()` | Count of records | `summarize count()` |
| `count(field)` | Count of non-null values | `summarize count(user_id)` |
| `sum(field)` | Sum of values | `summarize sum(bytes)` |
| `avg(field)` | Average of values | `summarize avg(duration)` |
| `min(field)` | Minimum value | `summarize min(response_time)` |
| `max(field)` | Maximum value | `summarize max(response_time)` |
| `median(field)` | Median value (50th percentile) | `summarize median(latency)` |
| `stdev(field)` | Standard deviation | `summarize stdev(duration)` |
| `range(field)` | Range (max - min) | `summarize range(values)` |

**Examples:**
```kusto
| summarize count() by method
| summarize total_bytes=sum(bytes), avg_duration=avg(duration) by service_name
| summarize min(response_time), max(response_time), avg(response_time)
```

### Metric Series Functions (`metrics` source only)

Counters and gauges are stored as points per **series**: one metric name plus one
exact set of attributes and resource attributes (the `series_id` column). These
functions work on each series first, then add the series together within each
`by` group. They only work on the `metrics` source.

| Function | Result per bin | Use for |
|----------|----------------|---------|
| `rate(value)` | Increase per second | Counters: requests/s, rows/s, bytes/s |
| `increase(value)` | Total increase in the bin | Counters: requests per bin, errors in the window |
| `last(value)` | Newest value in the bin | Gauges: memory in use, queue depth, connections |
| `rateif(value, predicate)` | `rate(value)` of the series that match | One part of a ratio of counters |
| `increaseif(value, predicate)` | `increase(value)` of the series that match | |
| `lastif(value, predicate)` | `last(value)` of the series that match | One part of a ratio of gauges |

How the increase is computed:

- **Cumulative counters** (the OTLP and Prometheus default): the increase of a
  point is its value minus the previous point of the same series. The previous
  point can be in an earlier bin, or up to 5 minutes before the time range, so a
  bin with one point still has a correct value.
- **Counter resets**: when a value is lower than the previous one, the process
  restarted. The new value is the increase. A restart does not make a negative or
  very large spike.
- **Delta counters** (`aggregation_temporality = DELTA`): each point is its own
  increase.
- **`rate()`** divides each series' increase in a bin by the larger of the bin
  width and the time that its points cover. A bin narrower than the export
  interval still shows the true rate, not a multiple of it.
- **Binned charts of `rate()`/`increase()` alone**, grouped by plain columns
  such as `metric_name` or by `attributes.*` keys, compare the highest value of
  each 5-minute cell with the cell before, so wide ranges read pre-aggregated
  data. Totals stay exact. Increments a series makes after a restart in the same
  cell show in the next cell, and a series that restarts twice in one cell loses
  the run between the restarts. Charts grouped by `resource.*` keys or by an
  expression read every point.
- **`last()`** takes the newest point of each series in the bin and adds the
  series together. Filter to one series (or group `by` its attributes) to see one
  value.

**Filtered series (`rateif`, `increaseif`, `lastif`).** The predicate selects
which series are added together for that one aggregate. Each series still
computes its increase from its own points, so two filtered aggregates in one
`summarize` can divide each other. Write the predicate on columns that are the
same for every point of a series: `metric_name`, `attributes.*`, `resource.*`.
A predicate on `value` or `timestamp` selects points, not series, and gives
increases that are not correct. Keep the `where` wide enough to read every
series that the aggregates use:

```kusto
// Rollup hit rate in percent: hits / (hits + misses)
metrics
| where metric_name in ("rollup.hits", "rollup.misses")
| summarize hits = rateif(value, metric_name == "rollup.hits"), total = rate(value) by bin_auto(timestamp)
| extend hit_pct = 100.0 * hits / total

// The same as one expression
metrics
| where metric_name in ("rollup.hits", "rollup.misses")
| summarize 100.0 * rateif(value, metric_name == "rollup.hits") / rate(value) by bin_auto(timestamp)

// Memory in use as a percent of the limit (two gauges)
metrics
| where metric_name in ("memory.used_bytes", "memory.limit_bytes")
| summarize 100.0 * lastif(value, metric_name == "memory.used_bytes") / lastif(value, metric_name == "memory.limit_bytes") by bin_auto(timestamp)

// Share of requests that are 5xx, filtered on an attribute
metrics
| where metric_name == "http.server.requests"
| summarize 100.0 * rateif(value, attributes.http.response.status_code >= 500) / rate(value) by bin_auto(timestamp)
```

A division by 0 gives 0.

`rate()` and `increase()` are not useful on gauges: a gauge that goes down is
read as a reset. Use `last()`, `avg()` or `max()` for gauges.

```kusto
// Requests per second, one line per service
metrics | where metric_name == "http.server.requests" | summarize rate(value) by bin_auto(timestamp), resource.service.name

// Per minute: multiply the rate
metrics | where metric_name == "jobs.failed" | summarize rate(value) * 60 by bin_auto(timestamp)

// Total increase over the whole time range (one number)
metrics | where metric_name == "jobs.failed" | summarize increase(value)

// Newest value of a gauge
metrics | where metric_name == "process.memory.usage" | summarize last(value) by bin_auto(timestamp)
```

**Which one to use:**

| You want | Use |
|----------|-----|
| A rate (per second or per minute) | `rate(value)` (`* 60` for per minute) |
| How many in each bin, or in the whole range | `increase(value)` |
| The current level of a gauge | `last(value)` |
| A ratio of counters or gauges | `rateif(value, …) / rate(value)`, `lastif(value, …) / lastif(value, …)` |
| The spread of a gauge in a bin | `range(value)` |

Do not use `range(value)` or `sum(value)` on a cumulative counter. `range()` is
0 when a bin has one point, and it is wrong across a restart. `sum()` adds
running totals together.

On dashboards, a `timeseries_stat` tile shows one number for the time range. Set
`summarize_by` to match the function: `sum` for `increase()` (the total increase),
`mean` or `max` for `rate()`, and `last` for `last()` (the newest bin). Do not use
`summarize_by: rate` with `rate()`: it divides by the minutes in the range a
second time.

### Percentile Functions

| Function | Description | Example |
|----------|-------------|---------|
| `p50(field)` | 50th percentile | `summarize p50(duration)` |
| `p75(field)` | 75th percentile | `summarize p75(duration)` |
| `p90(field)` | 90th percentile | `summarize p90(duration)` |
| `p95(field)` | 95th percentile | `summarize p95(duration)` |
| `p99(field)` | 99th percentile | `summarize p99(duration)` |
| `p100(field)` | 100th percentile (max) | `summarize p100(duration)` |
| `percentile(field, N)` | Nth percentile | `summarize percentile(duration, 95)` |
| `percentiles(field, N1, N2, ...)` | Multiple percentiles | `summarize percentiles(duration, 50, 90, 99)` |

**Arithmetic in Percentiles:**

You can perform unit conversions directly in percentile functions:

```kusto
// Convert nanoseconds to milliseconds
| summarize percentiles(duration / 1e6, 50, 75, 90, 95, 99) by bin(timestamp, 1h)

// Scale values
| summarize p95(response_time * 1000)
```

**Examples:**
```kusto
| summarize p95(duration) by service_name
| summarize percentile(response_time, 99) by endpoint
| summarize percentiles(duration, 50, 75, 90, 95, 99) by bin(timestamp, 5m)
```

### Conditional Aggregations

| Function | Description | Example |
|----------|-------------|---------|
| `countif(condition)` | Count where condition is true | `summarize countif(status >= 400)` |
| `dcount(field)` | Distinct count | `summarize dcount(user_id)` |
| `dcount(field, accuracy)` | Distinct count with accuracy hint (0-4) | `summarize dcount(session_id, 2)` |

**Examples:**
```kusto
| summarize
    total=count(),
    errors=countif(status_code >= 400),
    server_errors=countif(status_code >= 500)
  by service_name

| summarize unique_users=dcount(user_id) by bin(timestamp, 1h)
```

---

## Scalar Functions

### String Functions

| Function | Description | Example |
|----------|-------------|---------|
| `strcat(s1, s2, ...)` | Concatenate strings | `strcat(method, " ", path)` |
| `coalesce(v1, v2, ...)` | First non-null value | `coalesce(user_name, "anonymous")` |

**Examples:**
```kusto
| summarize count() by full_path=strcat(host, path)
| summarize count() by user=coalesce(user_id, session_id, "unknown")
```

### Conditional Functions

| Function | Description | Example |
|----------|-------------|---------|
| `iff(condition, then, else)` | If-then-else | `iff(status >= 400, "error", "ok")` |
| `case(pred1, val1, ..., else)` | Multi-branch conditional | `case(status >= 500, "5xx", status >= 400, "4xx", "ok")` |

**Examples:**
```kusto
| summarize count() by status_category=iff(status_code >= 400, "error", "success")

| summarize count() by status_group=case(
    status_code >= 500, "server_error",
    status_code >= 400, "client_error",
    status_code >= 300, "redirect",
    "success"
  )
```

### Type Casting Functions

| Function | Aliases | Description | Example |
|----------|---------|-------------|---------|
| `tofloat(value)` | `todouble(value)` | Convert to float | `tofloat(count_val)` |
| `toint(value)` | `tolong(value)` | Convert to integer | `toint(duration)` |
| `tostring(value)` | | Convert to text | `tostring(status_code)` |

**Note:** These functions are best used with `extend` or `project` operators for value transformation.

**Examples:**
```kusto
// Convert duration to integer milliseconds
| extend duration_ms = toint(duration / 1000000)

// Add formatted status text column
| extend status_text = tostring(status_code)

// Convert count to float for percentage calculation
| extend error_rate = tofloat(error_count) / tofloat(total_count) * 100
```

### Formatting Functions

| Function | Description | Example |
|----------|-------------|---------|
| `round(value, decimals)` | Round to N decimal places | `round(error_rate, 2)` |

**Examples:**
```kusto
// Round error rate to 2 decimal places
| summarize error_rate = round(countif(status >= 400) * 100.0 / count(), 2)

// Round duration to integer
| extend duration_rounded = round(duration / 1e6, 0)

// Calculate and round percentages
| summarize
    total = count(),
    errors = countif(status_code >= 400),
    error_pct = round(countif(status_code >= 400) * 100.0 / count(), 2)
  by service_name
```

---

## Date and Time Functions

### now()

Returns the current timestamp.

```kusto
timestamp <= now()
```

### ago()

Returns a timestamp relative to the current time.

```kusto
ago(timespan)
```

**Timespan Units:**

| Unit | Description | Example |
|------|-------------|---------|
| `ns` | Nanoseconds | `ago(100ns)` |
| `us` / `µs` | Microseconds | `ago(500us)` |
| `ms` | Milliseconds | `ago(100ms)` |
| `s` | Seconds | `ago(30s)` |
| `m` | Minutes | `ago(5m)` |
| `h` | Hours | `ago(2h)` |
| `d` | Days | `ago(7d)` |
| `w` | Weeks | `ago(2w)` |

**Examples:**
```kusto
timestamp >= ago(1h)
timestamp >= ago(7d) and timestamp <= ago(1d)
```

### bin()

Groups timestamps into fixed-width time buckets.

```kusto
bin(timestamp_field, interval)
```

**Examples:**
```kusto
| summarize count() by bin(timestamp, 1m)
| summarize avg(duration) by bin(timestamp, 1h), service_name
| summarize sum(bytes) by bin(timestamp, 5m)
```

### bin_auto()

Automatically calculates the optimal bin width based on the query time range.

```kusto
bin_auto(timestamp_field)
```

**Auto Bin Width Calculation:**

| Time Range | Bin Width |
|------------|-----------|
| 0 - 2 minutes | 1 second |
| 2 - 5 minutes | 5 seconds |
| 5 - 15 minutes | 10 seconds |
| 15 minutes - 1 hour | 30 seconds |
| 1 - 6 hours | 1 minute |
| 6 - 14 hours | 5 minutes |
| 14 - 48 hours | 10 minutes |
| 2 - 7 days | 1 hour |
| 7 - 30 days | 6 hours |
| 30+ days | 1 day |

**Examples:**
```kusto
| summarize count() by bin_auto(timestamp)
| summarize avg(duration) by bin_auto(timestamp), method
| summarize count() by bin_auto(timestamp), method, status_code
```

A chart draws one series for each combination of the `by` columns after the
time bin. The series label joins the values with ` / `, for example `GET / 200`.

---

## Field References

### Simple Fields

Reference top-level fields directly by name:

```kusto
method == "GET"
status_code >= 400
duration > 1000
```

### Nested JSON Fields

Access nested JSON properties using dot notation:

```kusto
request_body.message has "error"
response.data.user_id == "12345"
config.settings.enabled == true
```

**Deep Nesting:**
```kusto
request_body.level1.level2.value == "test"
```

### Array Access

Access array elements by index:

```kusto
errors[0].message has "timeout"
items[0].price > 100
```

**Wildcard Array Access:**

Use `[*]` to match any element in an array:

```kusto
errors[*].type == "ValidationError"
data[*].items[0].id == "abc123"
```

### OpenTelemetry Attributes

Common OTel attribute paths are flattened for easy access:

| KQL Path | Description |
|----------|-------------|
| `attributes.http.request.method` | HTTP request method |
| `attributes.http.response.status_code` | HTTP response status |
| `attributes.url.path` | URL path |
| `attributes.url.full` | Full URL |
| `attributes.url.query` | URL query string |
| `attributes.user_agent.original` | User agent string |
| `attributes.db.system.name` | Database system |
| `attributes.db.operation.name` | Database operation |
| `attributes.db.query.text` | Database query |
| `context.trace_id` | Trace ID |
| `context.span_id` | Span ID |
| `resource.service.name` | Service name |
| `resource.service.version` | Service version |

**Examples:**
```kusto
attributes.http.request.method == "POST"
resource.service.name == "payment-service"
context.trace_id == "abc123def456"
```

---

## Value Types

| Type | Syntax | Example |
|------|--------|---------|
| String (double quotes) | `"value"` | `method == "GET"` |
| String (single quotes) | `'value'` | `method == 'POST'` |
| Number | `123`, `45.67` | `duration > 1000` |
| Boolean | `true`, `false` | `is_error == true` |
| Null | `null` | `user_id == null` |
| List | `["a", "b"]`, `[1, 2]` | `method in ("GET", "POST")` |
| Duration | `100ms`, `5s`, `1h` | `timestamp >= ago(7d)` |
| Regex | `/pattern/` | `email matches /.*@example\.com/` |

---

## Named Aggregations

Assign custom names to aggregation results:

```kusto
alias=aggregation_function(...)
```

**Default Column Names:**

When no alias is provided, aggregation functions automatically generate column names following KQL conventions:

| Function | Default Column Name |
|----------|---------------------|
| `count()` | `count_` |
| `count(field)` | `count_field` |
| `sum(field)` | `sum_field` |
| `avg(field)` | `avg_field` |
| `min(field)` | `min_field` |
| `max(field)` | `max_field` |
| `p50(field)` | `p50_field` |
| `p95(field)` | `p95_field` |
| `dcount(field)` | `dcount_field` |
| `countif(...)` | `countif_` |

**Examples:**
```kusto
// Without alias - uses default column name count_
| summarize count() by message
| sort by count_ desc

// With explicit aliases
| summarize
    total_count=count(),
    error_count=countif(status_code >= 400),
    avg_latency=avg(duration),
    p99_latency=p99(duration)
  by service_name

| summarize TotalBytes=sum(bytes), RequestCount=count() by method
```

---

## Query Examples

### Basic Filtering

```kusto
// Find all POST requests
method == "POST"

// Find errors in the last hour
timestamp >= ago(1h) and status_code >= 400

// Find slow requests
duration > 5000 and method in ("GET", "POST")
```

### Text Search

```kusto
// Search for errors in logs
message has "error" or message has "exception"

// Find API calls
url contains "/api/v2" and method == "GET"

// Pattern matching
email matches /.*@company\.com$/
name matches regex "^GET /Cart$"
```

### Time-Based Analysis

```kusto
// Requests per minute over the last hour
timestamp >= ago(1h)
| summarize count() by bin(timestamp, 1m)

// Average response time per hour
| summarize avg(duration) by bin(timestamp, 1h), service_name
| sort by timestamp desc
```

### Error Analysis

```kusto
// Error rate by service
timestamp >= ago(24h)
| summarize
    total=count(),
    errors=countif(status_code >= 400),
    error_rate=countif(status_code >= 400) * 100.0 / count()
  by service_name
| sort by error_rate desc

// Top error messages (count_ is the default column name for count())
status_code >= 500
| summarize count() by message
| sort by count_ desc
| take 10
```

### Performance Analysis

```kusto
// Latency percentiles by endpoint
timestamp >= ago(1h)
| summarize
    p50=p50(duration),
    p90=p90(duration),
    p95=p95(duration),
    p99=p99(duration)
  by endpoint

// Latency over time (in milliseconds)
timestamp >= ago(6h)
| summarize percentiles(duration / 1e6, 50, 90, 95, 99) by bin(timestamp, 5m)
```

### Cardinality Analysis

```kusto
// Unique users per day
timestamp >= ago(7d)
| summarize unique_users=dcount(user_id) by bin(timestamp, 1d)

// Unique sessions per service
| summarize session_count=dcount(session_id) by service_name
```

### Complex Queries

```kusto
// Full request analysis pipeline
spans
| where timestamp >= ago(24h)
| where method in ("GET", "POST", "PUT", "DELETE")
| where status_code >= 200
| summarize
    request_count=count(),
    error_count=countif(status_code >= 400),
    avg_duration=avg(duration),
    p95_duration=p95(duration)
  by service_name, method
| sort by request_count desc
| take 20

// Status code distribution with categories
timestamp >= ago(1h)
| summarize count() by status_category=case(
    status_code >= 500, "5xx Server Error",
    status_code >= 400, "4xx Client Error",
    status_code >= 300, "3xx Redirect",
    status_code >= 200, "2xx Success",
    "Other"
  )
```

### Metrics Queries

```kusto
// Requests per second from a cumulative counter
metrics
| where metric_name == "http_requests_total"
| summarize rate(value) by bin_auto(timestamp)

// Requests per bin, one series per route and method
metrics
| where metric_name == "http_requests_total"
| summarize increase(value) by bin_auto(timestamp), attributes.http.route, attributes.http.request.method

// Current value of a gauge
metrics
| where metric_name == "process_resident_memory_bytes"
| summarize last(value) by bin_auto(timestamp)

// Filter by metric labels
metrics
| where metric_name == "request_duration_seconds"
| where labels.method == "GET"
| summarize avg(value) by bin(timestamp, 1m)
```

---

## Quick Reference Card

### Filtering
```
==, !=, >, <, >=, <=
has, !has, contains, !contains
startswith, !startswith, endswith, !endswith
matches, =~
in, !in, has_any, has_all
and, or, ()
```

### Pipe Operations
```
| where <condition>
| summarize <agg> [by <fields>]
| sort by <field> [asc|desc]
| take <N>
```

### Time Functions
```
now(), ago(1h), ago(7d)
bin(timestamp, 1h), bin_auto(timestamp)
```

### Aggregations
```
count(), sum(), avg(), min(), max()
median(), stdev(), range()
rate(value), increase(value), last(value)   // metrics only, per series
rateif(value, pred), increaseif(value, pred), lastif(value, pred)
p50(), p75(), p90(), p95(), p99()
percentile(field, N), percentiles(field, N1, N2, ...)
countif(condition), dcount(field)
```

### Scalar Functions
```
strcat(s1, s2, ...), coalesce(v1, v2, ...)
iff(cond, then, else), case(p1, v1, ..., else)
```

---

## Limitations & Notes

### Unsupported KQL Features
This implementation supports a subset of KQL. Notable differences from Azure Data Explorer KQL:
- No `join`, `union`, or subquery support
- No `let` statements for variable binding
- No `mv-expand` operator
- No `render` operator (visualization is handled separately)

### Null Handling
- Comparisons with `null` fields return `false` (field must exist)
- Use `coalesce(field, default)` to handle missing values
- Aggregations skip null values

### Type Coercion
- String-to-number comparisons: strings are not auto-converted
- Use explicit values: `status_code == 200` (number) not `status_code == "200"` (string)

### Performance Tips
- Filter by time range first (`timestamp >= ago(1h)`) to limit data scanned
- Use `has` over `contains` when possible (more efficient for indexed fields)
- Limit results with `take` when exploring data
- Percentile calculations are approximate for performance
