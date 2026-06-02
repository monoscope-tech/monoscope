# Plan: OverloadedRecordDot support in `hasql-interpolate`

Goal: make `[HI.sql| select #{ s.createdAt } from t |]` parse, so we can drop
the `let pidText = pid.toText` shim that currently wraps every dotted access
before splicing.

---

## Confirmed facts (from research pass 2026-06-02)

| Item | Value |
| --- | --- |
| Upstream repo | `github.com/awkward-squad/hasql-interpolate` |
| Latest Hackage | `1.1.0.1` |
| Version in monoscope | unpinned in `package.yaml:229,392`; solver picks `1.1.0.1` |
| Splice parser file | `lib/Hasql/Interpolate/Internal/TH.hs` |
| Splice parser entry | `sqlExprParser` (L107), `param` (L222-233), `splice` (L234-241) |
| Parse call | `Language.Haskell.Meta.parseExp content` (no mode/extension support) |
| Transitive parser | `haskell-src-meta ^>= 0.8` → `haskell-src-exts >= 1.21 && < 1.24` |
| `RecordDotSyntax` in haskell-src-exts | present in 1.23.x as `KnownExtension` |
| `haskell-src-meta` mode-aware API | `Language.Haskell.Meta.Parse.parseResultToEither` + `Language.Haskell.Meta.Syntax.Translate.toExp` (we don't need a fork of haskell-src-meta) |
| Test framework upstream | `tasty + tasty-hunit`, single `test/Main.hs`, integration-only — no parser unit tests today |
| Existing `source-repository-package` style | `cabal.project` already uses `tonyalaribe/effectful-postgresql` with `subdir:` — mirror that |

The original plan's "Phase 5 fallback" is not needed: haskell-src-exts can
parse `s.field` when `RecordDotSyntax` is enabled. What needs to change is
the call into `haskell-src-meta` — `parseExp` is the no-extension shortcut;
we must drop to the `parseHsExpWithMode`-style API.

---

## Phase 0 — Reproduce the failure (15 min)

Create a scratch test that currently fails:

```haskell
let s = Subscription{..}
Hasql.interpOne [HI.sql| insert into subs (id, created_at)
                         values (#{ s.id }, #{ s.createdAt }) |]
```

Expected: parser error from `parseExp` ("parse error on input '.'" or
similar). Capture the exact message — it becomes the regression test.

Touch the `pid.toText` workarounds we already wrote (Utils.hs:506) to
remind ourselves of the call shape.

## Phase 1 — Local fork of `hasql-interpolate`

1. `gh repo fork awkward-squad/hasql-interpolate --org apitoolkit --clone`
   (or fork into `tonyalaribe/` to match `effectful-postgresql`).
2. Branch: `record-dot-syntax`.
3. Keep upstream as `upstream` remote so the PR can rebase later.

## Phase 2 — Patch `Internal/TH.hs`

Replace the bare `parseExp` with a mode-aware wrapper. The smallest diff
keeps everything else identical:

```haskell
-- new imports
import qualified Language.Haskell.Exts as HSE
import qualified Language.Haskell.Meta.Syntax.Translate as Meta

parseExpRDS :: String -> Either String TH.Exp
parseExpRDS s =
  case HSE.parseExpWithMode mode s of
    HSE.ParseOk e         -> Right (Meta.toExp e)
    HSE.ParseFailed _ err -> Left err
  where
    mode = HSE.defaultParseMode
      { HSE.extensions =
          map (HSE.EnableExtension)
            [ HSE.RecordDotSyntax
            , HSE.OverloadedRecordDot   -- alias on newer hse, harmless if missing
            ]
          ++ HSE.extensions HSE.defaultParseMode
      }
```

Then in `param` (L228) and `splice` (L240) swap `parseExp content` for
`parseExpRDS content`. Net change ≈ 15 lines.

**Caveat:** check whether the installed `haskell-src-exts` actually exports
`OverloadedRecordDot`. If only `RecordDotSyntax` exists in 1.23.x, drop the
`OverloadedRecordDot` line — `RecordDotSyntax` is what controls the lexer.

## Phase 3 — Tests in the fork

Upstream has no parser-level tests; add one. Create
`test/RecordDotSpec.hs` (or fold into `test/Main.hs`):

```haskell
recordDotParses :: TestTree
recordDotParses = testCase "splice accepts record-dot syntax" $ do
  -- Compile-time assertion: this TH splice must succeed.
  let _ = [sql| select #{ s.foo }, #{ s.bar.baz } from t |] :: ()
  pure ()
```

A compile-time TH success is the test — if the parser regresses, the test
file fails to build.

Also add a tiny pure test that calls `parseExpRDS "s.foo"` directly and
asserts `Right (VarE ...)` shape — gives a readable failure if upstream
flips back.

## Phase 4 — Wire the fork into monoscope

Append to `/Users/tonyalaribe/Projects/apitoolkit/monoscope/cabal.project`:

```
source-repository-package
  type: git
  location: https://github.com/tonyalaribe/hasql-interpolate
  tag: <commit sha after Phase 2 lands on the fork>
```

(No `subdir:` — the lib is at repo root, unlike the `effectful-postgresql`
case.)

Then:

1. `hpack` (no .hs file changes, but keeps the cabal in sync).
2. Read `build.log` after ghcid recompiles. Do **not** run `cabal build`.
3. Once green, refactor one call site as a smoke test — `Utils.hs:506`
   `let pidText = pid.toText` → inline `#{ pid.toText }`. Re-read
   `build.log`.

## Phase 5 — Sweep monoscope call sites

After the fork compiles, search for the workaround patterns and inline
them. Grep targets:

```
rg -n "let .*= .*\.[a-zA-Z]+\s*$" src/ | rg -B1 'HI\.sql'
rg -n "\.toText" src/Models src/Pages | rg -B1 -A1 'HI\.sql'
```

Likely cleanups (verify each):

- `src/Utils.hs:506` — `pidText = pid.toText`
- subscription / billing code (`Models/Projects/Projects.hs`)
- any `pid.toText`, `uid.toText`, `mid.toText` immediately before a
  `[HI.sql|...|]` block

Each conversion is one line removed plus the splice rewrite. Keep them in
a single commit — easy to revert if the fork goes sideways.

## Phase 6 — Upstream PR

1. Open issue on `awkward-squad/hasql-interpolate` describing the failure
   (`parseExp` can't see `RecordDotSyntax`) with a 4-line repro.
2. PR the Phase 2 + Phase 3 diff. Include CHANGELOG bump.
3. Note in PR body: extension is *opt-in at parse time*, no behavioural
   change for users who don't write dotted splices.

When upstream merges + releases, drop the `source-repository-package`
stanza and pin the new Hackage version in `package.yaml`.

---

## Risk register

| Risk | Likelihood | Mitigation |
| --- | --- | --- |
| `haskell-src-exts` can't actually parse `s.field` despite the extension constructor existing | low — extension shipped in 1.23 (2021) | Phase 0 also runs the scratch parse; if it fails, fall back to the original plan's Phase 5 pre-pass hack |
| Upstream maintainer rejects the PR | medium | We're already unblocked via fork; keep fork in `tonyalaribe/` indefinitely |
| `haskell-src-meta`'s `toExp` translation loses fidelity on dot-chains | low | Phase 3 pure test catches this; if hit, write a small TH rewriter that turns `HSE.OverloadedDot` → `GetField` literal |
| Future GHC syntax drift past haskell-src-exts coverage | high (years out) | Out of scope; only fix is ghc-lib-parser migration — track separately |

## Time budget

| Phase | Effort |
| --- | --- |
| 0 reproduce | 15 min |
| 1 fork | 10 min |
| 2 patch | 20 min |
| 3 tests | 30 min |
| 4 wire into monoscope | 20 min |
| 5 call-site sweep | 45 min |
| 6 upstream PR | 30 min |
| **Total** | **~2.5 h** |

## Out of scope

- Switching hasql-interpolate to `ghc-lib-parser`. Correct long-term, but a
  multi-day refactor and not required to unblock dotted splices today.
- Touching `haskell-src-meta` itself. Its existing public API is enough;
  no fork needed there.
