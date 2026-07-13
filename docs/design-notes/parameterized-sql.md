# Design note: full parameterized SQL in the KQL query engine (deferred)

_Scoped 2026-07-12. Decision: **defer phases 1–5**; the only slice worth doing is phase 6 (below), and even that is not urgent. This note captures the investigation so we don't re-derive it._

## What "full param SQL" would mean

Today the KQL→SQL generator (`src/Pkg/Parser.hs`, `src/Pkg/Parser/Expr.hs`, `src/Pkg/Parser/Stats.hs`) builds SQL as `Text` by concatenation, splicing user-supplied values inline (escaped via the canonical `sqlStringLit`, `Expr.hs:891`). "Full param SQL" = promote those inline values to real bind parameters (`$1`, `$2`, …) carried separately from the SQL skeleton.

## The finding that reframes it: the inline-Text engine is permanent

Generated SQL has **four** consumers; three structurally require literal inline-value `Text`, not placeholders:

| Consumer | Path | Bind params? |
|---|---|---|
| Execute — log explorer, sessions, alerts | Hasql `interp` (`Sql` monoid) — `LogQueries.hs:312/361/539/663/857`, `BackgroundJobs.hs:3372` | ✅ TF accepts params over PG wire; these already mix `rawSql` text + `[HI.sql\| #{x} \|]` |
| Execute — charts/metrics | postgresql-simple `query_ conn (Query bytes)` — `Charts.hs:343-370` | ❌ raw bytes; no params without a client rewrite |
| Store — `query_monitors.log_query_as_sql` | `Text` column; re-executed later; filtered `!= ''` | ❌ needs literal text |
| Render/observe — UI SQL preview, `db.query.text` OTEL attr, debug logs | `sqlBlock_` (`Dashboards.hs:2128`), span attrs | ❌ a `$1` placeholder is useless to a human |

**Consequence:** even a complete parameterization leaves the inline-`Text` generator alive — for storage, the UI preview, the OTEL attribute, and the whole Charts path. Full param SQL is therefore **additive, not a replacement**: you build and maintain a *second* rendering of every splice site, gated to the Hasql execute paths only. It roughly doubles the query-engine surface instead of hardening the one we have.

## Why the ROI is low

1. **Injection is already prevented** — SQL is built from the parsed AST (not raw user text), and every value site funnels through `sqlStringLit` (doubled single-quotes, `standard_conforming_strings`). The old `validateSqlQuery` blocklist isn't even on the main `selectLogTable` path.
2. **No Postgres plan-cache win** — each user query is unique, so prepared-statement reuse doesn't apply.
3. **The text engine survives regardless** (above), so this is pure addition.

## The scope surface (≈20 value-splice classes across 3 files)

- **String scalars** — `Display Values`/`Str` → `sqlStringLit` (`Expr.hs:897`); funnel for `=`/`!=`/`<`/`>` (`Expr.hs:1128`)
- **IN / NOT IN** — `Expr.hs:1113-1114`; `constantToSQLList` (`Parser.hs:666`) — *variable-arity, the hard case*
- **LIKE / regex** — `has`/`contains`/`startswith`/`endswith` via `reTerm` (`Expr.hs:1118-1138`); raw `like_regex` (`Expr.hs:1307`)
- **Numerics** — `Num`/`Duration` (`Expr.hs:896/901`), percentile fractions (`Stats.hs:144/165`, `Parser.hs:351`), `ROUND` decimals (`Stats.hs:171`), arith operands (`Stats.hs:99`), LIMIT (`Parser.hs:107/286`), `alertLookbackMins` (`Parser.hs:393`)
- **JSONPath** — body `sqlStringLit`-wrapped (`Expr.hs:1197`); inner strings via `jsonString`; path keys inlined un-escaped (`Expr.hs:1200`, column-side but user-supplied)
- **Timestamps** — `buildDateRange` (`Parser.hs:68-73`), `variablePresets` (`Parser.hs:631-633`), `TimestampLit` raw `'…'::timestamptz` (`Expr.hs:910`)
- **project_id** — `Parser.hs:277/401/409`
- **Blind placeholder subst** — `replacePlaceholders` `{{key}}`→value, no escaping (`Parser.hs:621-622`), fed by `variablePresets`/`allParams`

## The gating unknown (spike before committing)

`hasql-interpolate`'s `Sql` type is quasiquoter-oriented and does **not** cleanly expose a programmatic, dynamic-arity param builder (arbitrary predicate trees produce an unknown param count). Two ways out, both real work — a ~1-day spike decides:

- **(A)** custom `SqlFragment { skeleton :: Builder, params :: [SqlParam] }` monoid + drop to raw `hasql` `Statement`/`Encoders.Params` for the execute paths (clean, total, but a new execution path alongside the `Hasql` effect).
- **(B)** array-encoding within `interp` — collect params into typed arrays, bind one `#{textArr}`, reference `($1::text[])[k]` (index-fragile; jsonpath/regex don't fit).

## Phased plan (~7–10 focused days, needs the query test suite = CI)

1. Spike hasql-interpolate dynamic params → choose (A)/(B). DoD: one predicate executes end-to-end with a real bind param through both main and TF pools.
2. `SqlFragment` + dual renderer (`toHasql`, `toText` reusing `sqlStringLit`); doctest the two renderings agree.
3. Migrate `Display Values`/`displayExprHelper` (Expr.hs core) — highest value + risk.
4. Migrate `Stats.hs` aggregations + `Parser.hs` assemblers.
5. Charts postgresql-simple path — migrate to Hasql or explicitly leave on `toText` (documented).
6. Close the two `sqlStringLit` bypasses (see below).

Requires the query test suite throughout; `live-test-dev` is currently blocked by a GHCi interactive-linker bug, so this must run in CI.

## The one slice worth doing (Phase 6, ~0.5d, ghcid-verifiable)

Independent of everything above:

- Route `TimestampLit` (`Expr.hs:910`, raw `'…'::timestamptz`) through `sqlStringLit`. Source is server-side `iso8601Show`, so currently safe — but inconsistent with the canonical path.
- **Audit `replacePlaceholders` (`Parser.hs:621`) / `variablePresets` `allParams`**: determine whether these carry user-controlled dashboard variables. If yes, this blind `{{key}}`→value substitution is the one place a *live* injection gap could exist. Route through `sqlStringLit` if so.

Do Phase 6 (after the audit) if/when we touch this area again; leave phases 1–5 shelved behind the spike unless a driver appears (compliance requirement, or sending this SQL to an untrusted engine).
