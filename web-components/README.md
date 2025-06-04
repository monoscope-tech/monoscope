# APIToolkit Query Language (AQL) - Complete Grammar Reference

This document provides a comprehensive reference for the APIToolkit Query Language (AQL), used for querying telemetry data including spans, metrics, and logs.

# Quick Start: AQL vs Standard KQL

────────────────────────────────

AQL is inspired by KQL but designed specifically for observability data with some key differences:

```aql
// AQL supports deep nested field access with dots
parent.child.grandchild == "value"

// Tables are optional (uses UI-selected default table)
status_code >= 400

// 'where' is optional for initial filters when using default table
method == "GET" and duration > 100ms

// But required after other operations
summarize count() by endpoint | where count_value > 100

// Direct duration comparisons
duration > 100ms  // No need for special duration functions

// Native JSON array support
errors[*].code == 500  // Any error has code 500
tags[0] == "production"  // First tag is "production"
```

# 1 · Lexical Elements

─────────────────────

## 1.1 Comments

• Line comment → `// anything until end-of-line`  
• Block comment → `/* anything (incl. newlines) */`

## 1.2 Identifiers & Case-Sensitivity

• Field names, keywords, and function names are **case-insensitive**  
  (`method`, `METHOD`, `Method` are equivalent).  
• Strings inside quotes remain case-sensitive.

## 1.3 Literals

• **String** → `"abc"`, `"he said \"hi\""`  
• **Number** → `123`, `45.67`, `6.02e23`  
• **Boolean** → `true`, `false` (also `TRUE` / `FALSE`)  
• **Null** → `null`  
• **Array** → `[1, 2, 3]`, `["a", "b"]`, `("GET", "POST")`  
• **Duration** → `100ns`, `250µs`, `5ms`, `30s`, `10m`, `2h`, `1d`, `1w`  
  (internally stored as an **integer** nanosecond count)  
• **Timespan** → `1s`, `5m`, `1h`, `1d`, `1w` – used inside `bin(timestamp, timespan)` functions for time binning

## 1.4 Operators

### Comparison Operators
• `==` (equals)  
• `!=` (not equals)  
• `>` (greater than)  
• `<` (less than)  
• `>=` (greater than or equal)  
• `<=` (less than or equal)

### Set Operations
• `in` (value in list) - e.g., `method in ("GET", "POST")`  
• `!in` (value not in list) - e.g., `status !in (200, 404)`

### Text Search Operations
• `has` (case-insensitive whole word search)  
• `!has` (does NOT have the word)  
• `has_any` (has any of the specified words) - e.g., `tags has_any ["urgent", "critical"]`  
• `has_all` (has all of the specified words) - e.g., `message has_all ["user", "login"]`

### String Operations
• `contains` (case-insensitive substring search)  
• `!contains` (does NOT contain substring)  
• `startswith` (starts with string)  
• `!startswith` (does NOT start with string)  
• `endswith` (ends with string)  
• `!endswith` (does NOT end with string)

### Pattern Matching
• `matches regex` (regex pattern matching) - e.g., `email matches regex ".*@company\.com"`

### Logical Operators
• `and` (logical AND)  
• `or` (logical OR)  
• `not` (logical NOT)

### Special Operators
• `exists` (field exists, even if null)  
• `!exists` (field does not exist)  
• `|` (pipe separator for query stages)

## 1.5 Keywords

**Data Sources:**  
`spans`, `metrics`, `logs`

**Query Commands:**  
`where`, `summarize`, `extend`, `project`, `project-away`, `sort`, `take`, `limit`

**Modifiers:**  
`by`, `asc`, `desc`

**Aggregation Functions:**  
`count`, `sum`, `avg`, `min`, `max`, `median`, `stdev`, `range`  
`percentile`, `p50`, `p75`, `p90`, `p95`, `p99`, `p100`

# 2 · Grammar Specification

───────────────────────

## 2.1 Complete Grammar (EBNF)

```ebnf
(* Main Query Structure *)
query = section ( PIPE section )* ;

section = sourceSection
        | searchSection  
        | summarizeSection
        | extendSection
        | projectSection
        | sortSection
        | takeSection ;

(* Data Source Selection *)
sourceSection = ( "spans" | "metrics" | "logs" ) ;

(* Filter Expressions *)
searchSection = [ "where" ] expr ;

expr = term
     | expr ( "and" | "or" ) expr
     | "not" expr
     | "(" expr ")" ;

term = subject operator value
     | subject "matches" "regex" STRING
     | subject "exists"
     | subject "!exists" ;

(* Field Path Specification *)
subject = fieldPath | function ;

fieldPath = IDENTIFIER ( fieldAccess )* ;

fieldAccess = "." IDENTIFIER
            | "[" arrayIndex "]"
            | "[" "*" "]" ;

arrayIndex = NUMBER | "*" ;

(* Operators *)
operator = "==" | "!=" | ">" | "<" | ">=" | "<="
         | "in" | "!in"
         | "has" | "!has" | "has_any" | "has_all"
         | "contains" | "!contains"
         | "startswith" | "!startswith"
         | "endswith" | "!endswith" ;

(* Values *)
value = STRING
      | NUMBER
      | BOOLEAN
      | NULL
      | arrayLiteral
      | durationLiteral
      | function ;

arrayLiteral = "[" ( value ( "," value )* )? "]"
             | "(" ( value ( "," value )* )? ")" ;

durationLiteral = NUMBER DURATION_UNIT ;

(* Functions *)
function = IDENTIFIER "(" [ expr ( "," expr )* ] ")" ;

(* Aggregation Commands *)
summarizeSection = "summarize" aggFunction ( "," aggFunction )* [ byClause ] ;

aggFunction = [ alias "=" ] aggName "(" [ "*" | subject ] [ "," NUMBER ] ")" ;

aggName = "count" | "sum" | "avg" | "min" | "max" | "median" | "stdev" | "range"
        | "percentile" | "p50" | "p75" | "p90" | "p95" | "p99" | "p100" ;

byClause = "by" grouping ( "," grouping )* ;

grouping = subject | binFunction ;

binFunction = "bin(" subject "," (NUMBER | durationLiteral) ")" 
            | "bin_auto(" subject ")" ;

(* Extend Command *)
extendSection = "extend" alias "=" expr ( "," alias "=" expr )* ;

(* Project Commands *)
projectSection = "project" subject ( "," subject )* 
               | "project-away" subject ( "," subject )* ;

(* Sort Command *)
sortSection = "sort" "by" sortExpr ( "," sortExpr )* ;

sortExpr = subject [ "asc" | "desc" ] ;

(* Take/Limit Command *)
takeSection = ( "take" | "limit" ) NUMBER ;

alias = IDENTIFIER ;

(* Lexical Rules *)
IDENTIFIER = [a-zA-Z_] [a-zA-Z0-9_]* ;
STRING = '"' ( ESC_SEQ | [^"\\] )* '"' ;
NUMBER = DIGIT+ ( "." DIGIT+ )? ( [eE] [+-]? DIGIT+ )? ;
BOOLEAN = "true" | "false" | "TRUE" | "FALSE" ;
NULL = "null" | "NULL" ;
DURATION_UNIT = "ns" | "µs" | "us" | "ms" | "s" | "m" | "h" | "d" | "w" ;
PIPE = "|" ;
```

## 2.2 Operator Precedence

1. `()` explicit parentheses (highest)
2. `NOT` / `!` / `exists` / `!exists`
3. Comparison operators (`==`, `!=`, `>`, `<`, `>=`, `<=`)
4. Text/pattern operators (`has`, `contains`, `matches`, etc.)
5. `AND`
6. `OR` (lowest)

Left-associative within the same precedence level.

## 2.3 Default Table Behavior

The default table is selected via a dropdown in the UI. When no table is explicitly specified in the query, this pre-selected default table is used automatically. This allows for cleaner, more concise queries when working with a single data source.

# 3 · Field Path Syntax

──────────────────────

## 3.1 Simple Fields
```aql
method              // Simple field access
status_code         // Underscore-separated field names
level               // Severity level field
```

## 3.2 Nested JSON Fields
```aql
request_body.message        // Nested field access
attributes.http.method      // Deep nesting
resource.service.name       // OpenTelemetry resource fields
```

## 3.3 Array Access
```aql
errors[0]                   // First element
tags[*]                     // All elements (wildcard)
request_body.roles[1]       // Nested array access
events[*].name              // Field in all array elements
```

## 3.4 Complex Paths
```aql
request_body.tags[*].name   // Field in all array elements
errors[0].message           // Field in specific array element
attributes.db.query.text    // Deep nested field
```

## 3.5 Array Operations Detail

### Array Contains Any Element
```aql
tags[*] == "production"          // Any tag equals "production"
// → SQL: EXISTS (SELECT 1 FROM jsonb_array_elements_text(tags) AS t WHERE t = 'production')
```

### Array Field Access
```aql
errors[*].code == 500            // Any error has code 500
// → SQL: EXISTS (SELECT 1 FROM jsonb_array_elements(errors) AS e WHERE e->>'code' = '500')
```

### Array Length
```aql
array_length(tags) > 3           // More than 3 tags
// → SQL: jsonb_array_length(tags) > 3
```

# 4 · Value Types & Literals

─────────────────────────────

## 4.1 Strings
```aql
"GET"                       // Simple string
"user said \"hello\""       // Escaped quotes
"/api/v1/users"            // Path strings
```

## 4.2 Numbers
```aql
123                        // Integer
45.67                      // Float
6.02e23                    // Scientific notation
-42                        // Negative number
```

## 4.3 Booleans
```aql
true                       // Boolean true
false                      // Boolean false
TRUE                       // Case-insensitive
FALSE                      // Case-insensitive
```

## 4.4 Arrays
```aql
[1, 2, 3]                  // Number array
["GET", "POST", "PUT"]     // String array
(200, 404, 500)            // Alternative syntax
["urgent", "critical"]     // Mixed content
```

## 4.5 Duration Values
```aql
100ns                      // 100 nanoseconds
250µs                      // 250 microseconds (or 250us)
5ms                        // 5 milliseconds
30s                        // 30 seconds
10m                        // 10 minutes
2h                         // 2 hours
1d                         // 1 day
1w                         // 1 week
```

## 4.6 Null Values
```aql
null                       // Null value
NULL                       // Case-insensitive
```

# 5 · Query Examples

────────────────────

## 5.1 Basic Filtering
```aql
// Simple equality
method == "GET"

// Inequality with boolean
request_body.is_customer == true and method != "DELETE"

// Numeric comparison
status_code >= 400

// Null checks
response_body.error_code != null
```

## 5.2 Set Operations
```aql
// Value in list
method in ("GET", "POST", "PUT")

// Value not in list
status_code !in (200, 201, 204)

// Complex list with mixed types
user_role in ("admin", "moderator")
```

## 5.3 Text Search Operations
```aql
// Word search (case-insensitive)
message has "error"
log_level !has "debug"

// Multiple word matching
tags has_any ["urgent", "critical", "high"]
description has_all ["user", "authentication"]

// Substring search
url contains "/api/"
path !contains "/internal/"

// Prefix/suffix matching
endpoint startswith "/api/v1/"
filename endswith ".log"
user_agent !startswith "bot"
url !endswith ".css"
```

## 5.4 Pattern Matching
```aql
// Regex matching
email matches regex ".*@company\.com"
error_code matches regex "^HTTP_[45]\d{2}$"
log_message matches regex "\berror\b"

// Complex patterns
request_id matches regex "^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$"
```
## 5.5 Duration Comparisons
```aql
// Performance monitoring
duration > 500ms
response_time <= 2s
processing_time >= 100ms

// Complex duration queries
duration > 1s AND status_code >= 400
latency > 250ms OR timeout == true
```

## 5.6 Array and Nested Field Queries
```aql
// Array element access
errors[0].type == "ValidationError"
tags[*] == "production"

// Nested field with wildcard
request_body.items[*].price > 100
response.data[*].status == "active"

// Complex nested paths
attributes.http.request.headers[*].name == "authorization"
events[*].attributes.user.roles[*] in ("admin", "super_admin")
```

## 5.7 Existence Checks
```aql
// Field presence
user.timezone exists
request_body.optional_field !exists

// Combined with other conditions
user.email exists AND user.verified == true
request_body.payment_info exists AND status_code == 400
```

## 5.8 Aggregation Queries

### Basic Aggregations
```aql
// Simple count
summarize count()

// Named aggregations
summarize total_requests = count()

// Multiple aggregations
summarize 
    request_count = count(),
    avg_duration = avg(duration),
    p95_latency = percentile(duration, 95)
```

### Group By Operations
```aql
// Group by single field
summarize count() by service_name

// Group by multiple fields
summarize avg(duration) by service_name, endpoint

// Group by nested fields
summarize count() by attributes.http.method, response.status_code
```

### Time-Series Aggregation
```aql
// Auto time binning
summarize count() by bin_auto(timestamp)

// Fixed time intervals
summarize avg(duration) by bin(timestamp, 5m)
summarize sum(bytes) by bin(timestamp, 1h)

// Complex time-series with percentiles
summarize 
    p95_latency = percentile(duration, 95),
    request_count = count()
    by endpoint, bin(timestamp, 15m)

// Multi-dimensional time-series
summarize 
    total_bytes = sum(bytes)
    by service_name, region, bin(timestamp, 1h)
```

## 5.9 Extend Operations
```aql
// Calculate new fields
extend response_time_ms = duration / 1ms

// Multiple calculations
extend 
    error_rate = errors / total * 100,
    avg_item_value = total_value / item_count

// Using functions
extend 
    hour_of_day = datetime_part("hour", timestamp),
    is_weekend = dayofweek(timestamp) in (0, 6)
```

## 5.10 Project Operations
```aql
// Select specific fields
project timestamp, service_name, duration

// Rename fields
project 
    time = timestamp,
    service = service_name,
    latency = duration

// Remove fields
project-away request_body, response_body
```

## 5.11 Sorting
```aql
// Simple sort
sort by timestamp desc

// Multi-field sort
sort by status_code asc, duration desc

// Sort after aggregation
summarize count() by endpoint
| sort by count_value desc
```

## 5.12 Multi-Stage Pipeline Queries
```aql
// Source selection with filtering and aggregation
spans
| where service_name == "checkout" and duration > 100ms
| summarize p90_duration = percentile(duration, 90) by endpoint
| where p90_duration > 400ms
| sort by p90_duration desc

// Complex pipeline with multiple stages
metrics
| where cpu_usage > 80 and memory_usage > 70
| summarize 
    avg_cpu = avg(cpu_usage),
    peak_memory = max(memory_usage)
    by host, region
| extend alert_score = (avg_cpu + peak_memory) / 2
| where alert_score > 85
| sort by alert_score desc
| take 10

// Time-series pipeline
spans
| where method == "POST" and status_code >= 400
| summarize 
    error_count = count(), 
    p95_latency = percentile(duration, 95) 
    by service_name, bin(timestamp, 10m)
| extend error_rate = error_count * 100.0 / total_count
| where error_rate > 5 or p95_latency > 1s
```

## 5.13 Advanced Filter Combinations
```aql
// Complex logical combinations
(method == "POST" or method == "PUT") 
and status_code >= 400 
and duration > 1s

// Mixed operator types
user_agent contains "mobile" 
and (geo_location.country in ("US", "CA", "MX"))
and session_duration > 5m

// Nested conditions with arrays
(errors[*].severity has_any ["critical", "high"])
or (warnings[*].type == "security" and user.role != "admin")

// Performance and error correlation
duration > 2s 
and (
  status_code >= 500 
  or errors[*].type has "timeout"
  or response_body contains "service unavailable"
)
```

# 6 · Data Source Specifications

──────────────────────────────

## 6.1 Spans Data Source
```aql
spans                      // OpenTelemetry spans

// Common span fields
spans | where name == "database_query"
spans | where kind == "server"
spans | where status_code >= 400
```

## 6.2 Metrics Data Source  
```aql
metrics                    // OpenTelemetry metrics

// Common metric operations
metrics | where metric_name == "cpu_usage"
metrics | where value > 80
```

## 6.3 Logs Data Source
```aql
logs                       // Application logs

// Common log operations
logs | where level == "error"
logs | where message contains "exception"
```

# 7 · Query Language Nuances

────────────────────────────────

## 7.1 The `where` Keyword

AQL follows KQL conventions for the `where` keyword:

### Rules for the `where` keyword:

1. When a table/source is explicitly specified, a `where` clause must follow:
   ```aql
   spans | where status_code >= 400
   metrics | where cpu_usage > 80
   ```

2. When using the default table (no table specified), the `where` keyword becomes optional:
   ```aql
   // These are equivalent when using the default table:
   status_code >= 400
   where status_code >= 400
   ```

3. Subsequent filter conditions after other sections always require the `where` keyword:
   ```aql
   // Filter after summarize requires 'where'
   status_code >= 400 | summarize count() by endpoint | where count_value > 100
   spans | where method == "POST" | summarize count() | where count_value > 10
   ```

# 8 · Scalar Functions

────────────────────

## 8.1 String Functions
- `tolower(field)` - Convert to lowercase
- `toupper(field)` - Convert to uppercase  
- `strlen(field)` - String length
- `substring(field, start, length)` - Extract substring
- `replace(field, old, new)` - Replace text
- `trim(field)` - Remove leading/trailing whitespace
- `split(field, delimiter)` - Split string into array

## 8.2 Time Functions  
- `now()` - Current timestamp
- `ago(duration)` - Time in the past (e.g., `ago(1h)`)
- `datetime(string)` - Parse datetime string
- `datetime_part(part, timestamp)` - Extract part (year, month, day, hour, etc.)
- `dayofweek(timestamp)` - Day of week (0=Sunday, 6=Saturday)
- `dayofmonth(timestamp)` - Day of month (1-31)
- `dayofyear(timestamp)` - Day of year (1-366)

## 8.3 Math Functions
- `round(value, precision)` - Round number
- `floor(value)` - Round down
- `ceil(value)` - Round up
- `abs(value)` - Absolute value
- `sqrt(value)` - Square root
- `pow(base, exponent)` - Power
- `log(value)` - Natural logarithm
- `log10(value)` - Base-10 logarithm

## 8.4 Type Functions
- `isnull(field)` - Check if null
- `isnotnull(field)` - Check if not null
- `isempty(field)` - Check if empty string or null
- `isnotempty(field)` - Check if not empty
- `typeof(field)` - Get field type
- `tostring(field)` - Convert to string
- `tonumber(field)` - Convert to number
- `tobool(field)` - Convert to boolean

## 8.5 Array Functions
- `array_length(field)` - Get array length
- `array_concat(arr1, arr2)` - Concatenate arrays
- `array_contains(array, value)` - Check if array contains value
- `array_distinct(array)` - Get unique values

# 9 · Common Query Patterns

────────────────────────────

## 9.1 Find Slow Database Queries
```aql
attributes.db.statement exists 
and duration > 500ms 
and attributes.db.system == "postgresql"
| project timestamp, service_name, attributes.db.statement, duration
| sort by duration desc
```

## 9.2 API Error Rate by Endpoint
```aql
// Calculate error rate percentage
summarize 
    error_count = countif(status_code >= 400),
    total_count = count() 
  by endpoint
| extend error_rate = round(error_count * 100.0 / total_count, 2)
| where error_rate > 5
| sort by error_rate desc
```

## 9.3 Trace Through Nested Request Data
```aql
request_body.user.permissions[*] has "admin"
and request_body.action.type == "delete"
and request_body.action.target.sensitive == true
| project timestamp, user.id, request_body.action
```

## 9.4 Service Health Dashboard Query
```aql
// Get service health metrics for last hour
where timestamp > ago(1h)
| summarize 
    avg_latency = avg(duration),
    p99_latency = percentile(duration, 99),
    error_rate = countif(status_code >= 500) * 100.0 / count(),
    request_count = count()
  by service_name, bin(timestamp, 5m)
| extend health_score = case(
    error_rate > 10, "critical",
    error_rate > 5 or p99_latency > 2s, "warning",
    "healthy"
  )
```

## 9.5 User Session Analysis
```aql
// Analyze user session patterns
attributes.user.id exists
| summarize 
    session_duration = max(timestamp) - min(timestamp),
    page_views = count(),
    unique_pages = dcount(page.url),
    has_error = max(case(status_code >= 400, 1, 0))
  by attributes.user.session_id
| where session_duration > 30s
| extend pages_per_minute = round(page_views / (session_duration / 60s), 2)
```

## 9.6 Resource Usage Anomalies
```aql
// Find resource usage spikes
metrics
| where metric_name in ("cpu_usage", "memory_usage", "disk_io")
| summarize 
    current_value = avg(value),
    baseline = percentile(value, 50)
  by host, metric_name, bin(timestamp, 10m)
| extend deviation_percent = abs(current_value - baseline) * 100.0 / baseline
| where deviation_percent > 50
| project timestamp, host, metric_name, current_value, baseline, deviation_percent
```

# 10 · SQL Translation & Implementation

──────────────────────────────────────

The AQL parser translates queries into optimized PostgreSQL with JSON support:

## 10.1 Field Access Translation
- Simple fields: `method` → `method`
- Nested JSON: `request_body.message` → `request_body->>'message'`
- Array element: `errors[0].type` → `errors->0->>'type'`
- Array wildcard: `tags[*]` → `jsonb_array_elements(tags)`
- Deep nesting: `a.b.c.d` → `a->'b'->'c'->>'d'`

## 10.2 Operator Translation
- `has` → PostgreSQL `~* '\y' || value || '\y'` (word boundary regex)
- `contains` → PostgreSQL `ILIKE '%' || value || '%'`
- `startswith` → PostgreSQL `ILIKE value || '%'`
- `endswith` → PostgreSQL `ILIKE '%' || value`
- `in` → PostgreSQL `IN (value1, value2, ...)`
- `matches regex` → PostgreSQL `~` operator
- Duration comparisons → Nanosecond integer comparisons

## 10.3 Performance Optimizations
- Flattened OpenTelemetry attributes for common fields
- GIN indexes on JSONB columns for fast searches
- Automatic time-based partitioning for spans/metrics/logs
- Query result caching for repeated aggregations
- Parallel query execution for time-series data

## 10.4 Index Strategy
```sql
-- Default indexes created automatically
CREATE INDEX idx_timestamp ON spans(timestamp);
CREATE INDEX idx_service_name ON spans(service_name);
CREATE INDEX idx_status_code ON spans(status_code);
CREATE INDEX idx_duration ON spans(duration);

-- GIN indexes for JSON search
CREATE INDEX idx_attributes ON spans USING gin(attributes);
CREATE INDEX idx_resource ON spans USING gin(resource);

-- Composite indexes for common patterns
CREATE INDEX idx_service_endpoint ON spans(service_name, endpoint);
CREATE INDEX idx_time_service ON spans(timestamp, service_name);
```

# 11 · Performance Guidelines & Limits

──────────────────────────────────

## 11.1 Query Limits
- Maximum query execution time: 30 seconds
- Maximum result set: 10,000 rows (use `| take N` or `| limit N` to override)
- Maximum query length: 10,000 characters
- Maximum nested field depth: 10 levels
- Maximum IN clause values: 1,000 items
- Maximum GROUP BY fields: 20 fields

## 11.2 Performance Best Practices

1. **Always filter by time range when possible**
   ```aql
   // Good: Limits data scan
   where timestamp > ago(1h) and status_code >= 500
   
   // Bad: Scans entire table
   where status_code >= 500
   ```

2. **Use specific field paths instead of wildcards**
   ```aql
   // Good: Direct path
   where errors[0].code == 500
   
   // Less efficient: Wildcard search
   where errors[*].code == 500
   ```

3. **Limit results early in the pipeline**
   ```aql
   // Good: Filter before aggregation
   where service_name == "api" 
   | summarize count() by endpoint
   
   // Bad: Aggregate everything then filter
   summarize count() by service_name, endpoint
   | where service_name == "api"
   ```

4. **Use indexed fields in WHERE clauses**
   - Prefer: `timestamp`, `service_name`, `status_code`, `method`
   - Avoid: Deep JSON paths in primary filters

5. **Optimize time binning**
   ```aql
   // Good: Appropriate bin size for time range
   where timestamp > ago(1h)
   | summarize count() by bin(timestamp, 1m)
   
   // Bad: Too granular for long time range
   where timestamp > ago(7d)
   | summarize count() by bin(timestamp, 1s)
   ```

# 12 · Error Handling & Validation

──────────────────────────────────

## 12.1 Common Error Messages

### Syntax Errors
```
Query: status_code = 400
Error: Invalid operator '=' at line 1, column 12. Did you mean '=='?

Query: where method == GET
Error: Unquoted string 'GET' at line 1, column 17. Strings must be quoted: "GET"

Query: duration > "100ms"
Error: Cannot compare duration (number) with string "100ms". Did you mean: duration > 100ms
```

### Field Access Errors
```
Query: request.boddy.message == "error"
Error: Field 'boddy' not found in 'request'. Did you mean 'body'?

Query: attributes[user][id] == 123
Error: Invalid array syntax. Use dot notation: attributes.user.id
```

### Type Errors
```
Query: timestamp > "yesterday"
Error: Invalid datetime value "yesterday". Use ago() function: timestamp > ago(1d)

Query: count > "many"
Error: Cannot compare numeric field 'count' with string "many"
```

### Aggregation Errors
```
Query: summarize count(*) by 
Error: Missing grouping field after 'by' keyword

Query: summarize avg(non_existent_field)
Error: Field 'non_existent_field' not found in schema
```

## 12.2 Validation Rules

1. **Duration units must be valid**: ns, µs, us, ms, s, m, h, d, w
2. **Array indices must be non-negative integers or '*'**
3. **Regular expressions must compile successfully**
4. **Aggregation functions require numeric fields (except count)**
5. **Time bin intervals must be positive durations**
6. **Field paths cannot exceed 10 levels of nesting**

# 13 · Version & Compatibility

─────────────────────────────

## Current Version: 1.0

## Compatibility Notes
- Inspired by Kusto Query Language (KQL) with modifications for observability
- PostgreSQL 14+ required for full JSONB support
- OpenTelemetry semantic conventions 1.20+ supported
- Compatible with OpenTelemetry data model

## Key Differences from Standard KQL
1. Deep nested field access using dot notation
2. Optional table specification (uses UI-selected default)
3. Optional `where` keyword for initial filters on default table
4. Native duration literals (100ms, 2s, etc.)
5. Simplified array access syntax

## Future Roadmap
- Graph traversal operators for distributed tracing
- Machine learning functions for anomaly detection
- Extended statistical functions
- Query federation across multiple data sources
- Custom function definitions
- Saved query templates

---

This grammar supports the full range of observability queries needed for monitoring, debugging, and analyzing telemetry data in production systems.