# APIToolkit Query Language Grammar

# 1 · Lexical Elements

─────────────────────

## 1.1 Comments

• Line comment → // anything until end-of-line  
 • Block comment → /_anything (incl. newlines)_/

## 1.2 Identifiers & Case-Sensitivity

• Field names, keywords, and function names are **case-insensitive**  
 (`method`, `METHOD`, `Method` are equivalent).  
 • Strings inside quotes remain case-sensitive.

## 1.3 Literals

• String → "abc", "he said \"hi\""  
 • Number → 123, 45.67, 6.02e23  
 • Boolean → true, false (also TRUE / FALSE)  
 • Null → null  
 • Array → [1, 2, 3], ["a", "b"]  
 • Duration → 100ns 250µs 5ms 30s 10m 2h 1d 1w  
 (internally stored as an **integer** nanosecond count)  
 • Timespan → [1s] [5m] [1h] – used only inside `timechart [...]` rollups

## 1.4 Operators

• Comparison → == != > < >= <=  
 • Regex-match → =~ (ECMA-262 flavour)  
 • Logical → AND OR (also lowercase)  
 • Existence → EXISTS / !EXISTS (unary)  
 • Pipe → | separates stages

## 1.5 Keywords

spans metrics stats timechart  
 by as  
 LIMIT (optional projection cap)

# 2 · Expression Syntax

───────────────────────

## 2.1 Grammar (ANTLR 4)

grammar AQL;

// Parser rules
query
: section (PIPE section)\* EOF
;

section
: sourceSection
| searchSection
| statsSection
| timechartSection
;

sourceSection
: (SPANS | METRICS) (LIMIT NUMBER)?
;

searchSection
: expr
;

expr
: expr (AND | OR) expr # binaryExpr
| NOT expr # notExpr
| LPAREN expr RPAREN # parenExpr
| term # termExpr
;

term
: subject operator value # compTerm
| subject REGEX_MATCH REGEX_LITERAL # regexTerm
| subject EXISTS # existsTerm
| subject NOT_EXISTS # notExistsTerm
;

subject
: fieldPath
;

fieldPath
: IDENT ( (DOT IDENT)
| (LBRACK arrayIdx RBRACK)
| (LBRACK STAR RBRACK)
)\*
;

arrayIdx
: NUMBER
| STAR
;

operator
: EQ | NEQ | GT | LT | GTE | LTE
;

value
: STRING
| NUMBER
| BOOLEAN
| NULL_LITERAL
| arrayLiteral
| durationLiteral
;

arrayLiteral
: LBRACK (value (COMMA value)\*)? RBRACK
;

durationLiteral
: NUMBER DURATION_UNIT
;

statsSection
: STATS aggFn (COMMA aggFn)\* byClause?
;

timechartSection
: TIMECHART aggFn (COMMA aggFn)\* byClause? rollup?
;

aggFn
: aggName LPAREN (STAR | subject) RPAREN (AS alias)?
;

aggName
: COUNT | SUM | AVG | MIN | MAX | MEDIAN | STDEV | RANGE
| P50 | P75 | P90 | P95 | P99 | P100
;

byClause
: BY subject (COMMA subject)\*
;

rollup
: LBRACK timePeriod RBRACK
;

timePeriod
: NUMBER TIME_UNIT
;

alias
: IDENT
;

// Lexer rules
SPANS : [sS][pP][aA][nN][sS] ;
METRICS : [mM][eE][tT][rR][iI][cC][sS] ;
STATS : [sS][tT][aA][tT][sS] ;
TIMECHART : [tT][iI][mM][eE][cC][hH][aA][rR][tT] ;
BY : [bB][yY] ;
AS : [aA][sS] ;
LIMIT : [lL][iI][mM][iI][tT] ;
EXISTS : [eE][xX][iI][sS][tT][sS] ;
NOT_EXISTS : '!' EXISTS ;
AND : [aA][nN][dD] ;
OR : [oO][rR] ;
NOT : [nN][oO][tT] ;
COUNT : [cC][oO][uU][nN][tT] ;
SUM : [sS][uU][mM] ;
AVG : [aA][vV][gG] ;
MIN : [mM][iI][nN] ;
MAX : [mM][aA][xX] ;
MEDIAN : [mM][eE][dD][iI][aA][nN] ;
STDEV : [sS][tT][dD][eE][vV] ;
RANGE : [rR][aA][nN][gG][eE] ;
P50 : [pP] '50' ;
P75 : [pP] '75' ;
P90 : [pP] '90' ;
P95 : [pP] '95' ;
P99 : [pP] '99' ;
P100 : [pP] '100' ;

EQ : '==' ;
NEQ : '!=' ;
GT : '>' ;
LT : '<' ;
GTE : '>=' ;
LTE : '<=' ;
REGEX_MATCH : '=~' ;

PIPE : '|' ;
LPAREN : '(' ;
RPAREN : ')' ;
LBRACK : '[' ;
RBRACK : ']' ;
COMMA : ',' ;
DOT : '.' ;
STAR : '\*' ;

BOOLEAN : [tT][rR][uU][eE]
| [fF][aA][lL][sS][eE]
;
NULL_LITERAL : [nN][uU][lL][lL] ;

STRING : '"' ( ESC_SEQ | ~["\\] )\* '"' ;
fragment ESC_SEQ
: '\\' ["'\\/bfnrt]
| UNICODE_ESC
;
fragment UNICODE_ESC
: '\\' 'u' HEX HEX HEX HEX
;

REGEX_LITERAL : '/' ( REGEX_ESC | ~[\\/\r\n] )+ '/' [iIsS]\* ;
fragment REGEX_ESC
: '\\' .
;

NUMBER : DIGIT+ ('.' DIGIT+)? ([eE] [+-]? DIGIT+)? ;

fragment DIGIT : [0-9] ;

DURATION_UNIT : 'ns' | 'µs' | 'us' | 'ms' | 's' | 'm' | 'h' | 'd' | 'w' ;
TIME_UNIT : 's' | 'm' | 'h' | 'd' | 'w' ;

IDENT : [a-zA-Z_] [a-zA-Z0-9_]\* ;

LINE*COMMENT : '//' ~[\r\n]\* -> skip ;
BLOCK_COMMENT : '/*' .\*? '\_/' -> skip ;
WS : [ \t\r\n]+ -> skip ;

## 2.2 Operator Precedence

1. () explicit parentheses
2. NOT
3. AND
4. OR  
   Left-associative within the same level.

# 3 · Semantics

──────────────

- Duration literals compare numerically after conversion:  
  `5ms == 5_000_000ns`, `1h > 3000s` → true.
- `EXISTS` / `!EXISTS` test for field presence (even if `null`).
- Regex uses `/pattern/` with optional flags `/pattern/i`.  
  Use `\\` to escape `/` inside the pattern.
- `LIMIT N` after a source section prunes rows **before** later pipes.

# 4 · Example Queries

────────────────────

### 4.1 Basic Filters

// request method
method == "GET"

// inequality + Boolean
request_body.is_customer == true AND method != "DELETE"

// nested field with wildcard
request.tags[*].name == "alpha"

### 4.2 Durations

// spans longer than 250 ms
duration > 250ms

// Metrics where p95 latency exceeds 1 s
metrics
| stats p95(latency_ms) as p95_lat
| p95_lat > 1s // 1s auto-normalises → 1 000 000 000 ns

### 4.3 Array & Regex

errors[*].type =~ /^HTTP\_[45]\d{2}$/
response.roles[0] == "admin"

### 4.4 Existence

// any payload that _omits_ user.timezone
NOT user.timezone EXISTS

### 4.5 Aggregations

stats count(\*) by service_name

stats sum(bytes) as total_bytes,
avg(duration) as mean_dur
| total_bytes > 10GB // numeric compare after sum

### 4.6 Time-series Roll-ups

timechart count(\*) by status_code [5m]

spans
| duration > 100ms
| timechart p95(duration) as p95_dur [1h]

### 4.7 Multi-Stage Pipeline

spans
| service == "checkout" AND duration > 100ms
| stats p90(duration) as p90_dur by endpoint
| p90_dur > 400ms
| LIMIT 100

### 4.8 Mixed Sources

metrics | stats max(cpu_usage) by host
spans | method == "POST" | stats count(\*)

### 4.9 Complex multi-stage pipeline

method == "GET"
| stats sum(duration) as total_duration by service
| timechart [1h]

