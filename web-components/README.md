# APIToolkit Query Language (AQL) - Complete Grammar Reference

This document provides a comprehensive reference for the APIToolkit Query Language (AQL), used for querying telemetry data including spans, metrics, and logs.

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
• **Timespan** → `[1s]`, `[5m]`, `[1h]` – used only inside `timechart [...]` rollups

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
• `matches` (regex pattern matching) - e.g., `email matches /.*@company\.com/`  
• `=~` (regex matching with ECMA-262 flavour)

### Logical Operators
• `AND` / `and` (logical AND)  
• `OR` / `or` (logical OR)  
• `NOT` / `not` (logical NOT)

### Special Operators
• `exists` (field exists, even if null)  
• `!exists` (field does not exist)  
• `|` (pipe separator for query stages)

## 1.5 Keywords

**Data Sources:**  
`spans`, `metrics`

**Aggregation Commands:**  
`stats`, `timechart`

**Modifiers:**  
`by`, `as`, `limit`

**Aggregation Functions:**  
`count`, `sum`, `avg`, `min`, `max`, `median`, `stdev`, `range`  
`p50`, `p75`, `p90`, `p95`, `p99`, `p100`

# 2 · Grammar Specification

───────────────────────

## 2.1 Complete Grammar (EBNF)

```ebnf
(* Main Query Structure *)
query = section ( PIPE section )* ;

section = sourceSection
        | searchSection  
        | statsSection
        | timechartSection ;

(* Data Source Selection *)
sourceSection = ( "spans" | "metrics" ) ( "limit" NUMBER )? ;

(* Filter Expressions *)
searchSection = expr ;

expr = term
     | expr ( "AND" | "OR" ) expr
     | "NOT" expr
     | "(" expr ")" ;

term = subject operator value
     | subject "=~" REGEX_LITERAL
     | subject "matches" REGEX_LITERAL
     | subject "exists"
     | subject "!exists" ;

(* Field Path Specification *)
subject = fieldPath ;

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
      | durationLiteral ;

arrayLiteral = "[" ( value ( "," value )* )? "]"
             | "(" ( value ( "," value )* )? ")" ;

durationLiteral = NUMBER DURATION_UNIT ;

(* Aggregation Commands *)
statsSection = "stats" aggFunction ( "," aggFunction )* byClause? ;

timechartSection = "timechart" aggFunction ( "," aggFunction )* byClause? rollup? ;

aggFunction = aggName "(" ( "*" | subject ) ")" ( "as" alias )? ;

aggName = "count" | "sum" | "avg" | "min" | "max" | "median" | "stdev" | "range"
        | "p50" | "p75" | "p90" | "p95" | "p99" | "p100" ;

byClause = "by" subject ( "," subject )* ;

rollup = "[" timePeriod "]" ;

timePeriod = NUMBER TIME_UNIT ;

alias = IDENTIFIER ;

(* Lexical Rules *)
IDENTIFIER = [a-zA-Z_] [a-zA-Z0-9_]* ;
STRING = '"' ( ESC_SEQ | [^"\\] )* '"' ;
REGEX_LITERAL = "/" ( REGEX_ESC | [^/\r\n] )+ "/" [iIsS]* ;
NUMBER = DIGIT+ ( "." DIGIT+ )? ( [eE] [+-]? DIGIT+ )? ;
BOOLEAN = "true" | "false" | "TRUE" | "FALSE" ;
NULL = "null" | "NULL" ;
DURATION_UNIT = "ns" | "µs" | "us" | "ms" | "s" | "m" | "h" | "d" | "w" ;
TIME_UNIT = "s" | "m" | "h" | "d" | "w" ;
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
request_body.is_customer == true AND method != "DELETE"

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
email matches /.*@company\.com/
error_code =~ /^HTTP_[45]\d{2}$/
log_message matches /\berror\b/i

// Complex patterns
request_id =~ /^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$/
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

### Stats Commands
```aql
// Basic counting
stats count(*)
stats count(*) by service_name

// Aggregation functions
stats sum(bytes_transferred) as total_bytes,
      avg(duration) as avg_duration,
      max(response_time) as max_response

// Percentile calculations
stats p95(duration) as p95_latency,
      p99(response_time) as p99_response
      by endpoint

// Multiple grouping
stats count(*) by service_name, environment, region
```

### Timechart Commands
```aql
// Time-series aggregation
timechart count(*) [5m]
timechart avg(duration) by service_name [1h]

// Complex time-series with percentiles
timechart p95(duration) as p95_latency,
          count(*) as request_count
          by endpoint [15m]

// Multi-dimensional time-series
timechart sum(bytes) as total_bytes
          by service_name, region [1h]
```

## 5.9 Multi-Stage Pipeline Queries
```aql
// Source selection with filtering and aggregation
spans
| service_name == "checkout" AND duration > 100ms
| stats p90(duration) as p90_duration by endpoint
| p90_duration > 400ms

// Complex pipeline with multiple stages
metrics
| cpu_usage > 80 AND memory_usage > 70
| stats avg(cpu_usage) as avg_cpu,
        max(memory_usage) as peak_memory
        by host, region
| avg_cpu > 85 OR peak_memory > 90

// Time-series pipeline
spans
| method == "POST" AND status_code >= 400
| timechart count(*) as error_count,
            p95(duration) as p95_latency
            by service_name [10m]
```

## 5.10 Advanced Filter Combinations
```aql
// Complex logical combinations
(method == "POST" OR method == "PUT") 
AND status_code >= 400 
AND duration > 1s

// Mixed operator types
user_agent contains "mobile" 
AND (geo_location.country in ("US", "CA", "MX"))
AND session_duration > 5m

// Nested conditions with arrays
(errors[*].severity has_any ["critical", "high"])
OR (warnings[*].type == "security" AND user.role != "admin")

// Performance and error correlation
duration > 2s 
AND (
  status_code >= 500 
  OR errors[*].type has "timeout"
  OR response_body contains "service unavailable"
)
```

# 6 · Data Source Specifications

──────────────────────────────

## 6.1 Spans Data Source
```aql
spans                      // OpenTelemetry spans
spans limit 1000          // Limited span results

// Common span fields
spans | name == "database_query"
spans | kind == "server"
spans | status_code >= 400
```

## 6.2 Metrics Data Source  
```aql
metrics                    // OpenTelemetry metrics
metrics limit 500         // Limited metric results

// Common metric operations
metrics | metric_name == "cpu_usage"
metrics | value > 80
```

# 7 · SQL Translation & Implementation

─────────────────────────────────────

The AQL parser translates queries into optimized PostgreSQL with JSON support:

## 7.1 Field Access Translation
- Simple fields: `method` → `method`
- Nested JSON: `request_body.message` → `request_body->>'message'`
- Arrays: `errors[0].type` → `errors->0->>'type'`
- Wildcards: `tags[*].name` → JSONPath queries

## 7.2 Operator Translation
- `has` → PostgreSQL `~*` (case-insensitive regex)
- `contains` → PostgreSQL `ILIKE '%value%'`
- `in` → PostgreSQL `IN (value1, value2)`
- Duration comparisons → Nanosecond integer comparisons

## 7.3 Performance Optimizations
- Flattened OpenTelemetry attributes for common fields
- Automatic index usage for frequently queried fields
- Optimized JSONPath queries for wildcard operations

# 8 · Error Handling & Validation

─────────────────────────────────

## 8.1 Parse Errors
- Invalid syntax reporting with line/column information
- Helpful error messages for common mistakes
- Suggestions for similar valid syntax

## 8.2 Type Validation
- Duration unit validation
- Array syntax validation
- Field path existence checking (when schema available)

## 8.3 Runtime Validation
- SQL injection prevention through parameterized queries
- Resource limit enforcement
- Query complexity analysis

---

This grammar supports the full range of observability queries needed for monitoring, debugging, and analyzing telemetry data in production systems.