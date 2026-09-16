# `hashes[*]=="x"` should lower to `array_has` on TimeFusion

## Finding

KQL lowers a bare wildcard equality to
`jsonb_path_exists(to_jsonb(hashes), '$[*] ? (@ == "x")'::jsonpath)`.
On TimeFusion, DataFusion's native `array_has` computes the same answer ~2.3x faster.

Measured 2026-09-16 on prod TF, project `28f62f01…`, 24h window, three interleaved
runs with a different hash each time (ordering works *against* `array_has` — it ran
first each round, so it paid any cold cost):

| predicate | run 1 | run 2 | run 3 |
|---|---|---|---|
| `array_has(hashes,'…')` | 3248ms | 2857ms | 2635ms |
| `jsonb_path_exists(to_jsonb(hashes),…)` | 7246ms | 6471ms | 6808ms |
| `hashes @> ARRAY['…']` | 6072ms | 5549ms | 4265ms |

Note `@>` is **not** the win — it is within noise of jsonpath. Only `array_has` is.

The cost is predicate evaluation, not scan volume: over 3 days (11.2M rows) a bare
`count(*)` is 3.5s, and the same count with the hash predicate is 25s.

Equivalence: identical counts for `array_has` vs jsonpath across 3 hashes × 2 windows
(146/146, 523/523, 6158/6158, 29741/29741, 18/18, 167/167).

## Why it wasn't done in one sitting

`renderJsonpath` is reached through the `Display`/`displayPrec` instances for `Expr`,
which are pure `Expr -> Text` with no `SqlQueryCfg` in scope, so the TF flag cannot be
threaded to the renderer. The obvious hook, `rewriteSectionsForSource` (which already
receives `metricJsonAsVariant`), rewrites **Subjects only** — it cannot replace an `Eq`
with a different constructor.

## Shape of the change

1. Add an `Expr` constructor for the array-membership predicate (e.g. `ArrayHas Text Text`).
   Every exhaustive match over `Expr` needs a case — at minimum `traverseSubjects.goE`
   in `shared/src/Pkg/Parser/Stats.hs` and the `Display` instance in `Expr.hs`.
2. Add an `Expr`-level rewrite beside `rewriteSectionsForSource`, gated on the same TF
   flag, mapping `Eq (Subject _ col [ArrayWildcard ""]) (Str v) -> ArrayHas col v`.
3. Render `ArrayHas col v` as `array_has(col, '<escaped v>')` (reuse `sqlStringLit`).

## Constraints — each is a correctness bug if violated

- **Equality only.** `hashes[*] != "x"` in jsonpath is "∃ element ≠ x"; `NOT array_has`
  is "∄ element = x". Different predicates. Same for `in`, regex and ordered comparisons.
- **Bare wildcard only** — path exactly `[ArrayWildcard ""]`. `$[*]."key"` stays jsonpath.
- **Real array columns only** (`hashes`, and whatever else the schema types as a list).
  An attributes/variant path must never take this branch.
- **TimeFusion only.** Postgres has no `array_has`; the PG lowering must stay byte-identical
  (the bug-2 regression test in `DashboardWidgetsSpec` runs `hashes[*]==` against postgres).
  Default the flag to today's behaviour so the CLI and un-threaded callers are unaffected.

## Open verification

- NULL/empty-array equivalence is **not** yet verified on real rows: the customer project
  has no NULL `hashes` in-window, and an unbounded cross-project probe is cancelled by TF's
  statement timeout. Pin it with doctests for both polarities and a bounded probe against a
  project that has them.
- Add the agreement check to the executable TF contract suite so it outlives the change.

## Expected effect

Roughly halves every `hashes[*]==` query: the endpoint dashboards, and the error-fingerprint
charts that use `hashes[*]=="err:…"`. It does **not** fix the 3-day case on its own
(≈25s → ≈11s); that still needs the TF-side indexing work.
