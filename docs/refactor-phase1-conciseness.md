# Phase 1 Conciseness Plan — Quick, Safe, High-LOC

**Goal:** remove ~1,400–1,900 LOC of mechanical boilerplate without changing behaviour, and make the
codebase easier to grok by routing repeated markup/instances/wrappers through named, reusable definitions.

**Branch:** `claude/blissful-pasteur-ZlPQl` (cut one PR per workstream below; they are independent).

**Guardrails (apply to every PR):**
- No behaviour change. These are pure refactors — rendered HTML, JSON wire format, and effect semantics must be byte-identical.
- After editing `package.yaml`/deps, regenerate cabal: `hpack` (or `make` equivalent).
- Format: `fourmolu -i <changed files>` (config in `fourmolu.yaml`).
- Lint: `make lint` (`hlint src`). Build: `cabal build all`. Tests: `make test-unit` then `make test`.
- Keep each PR to one workstream so review/diff stays mechanical and reversible.

**Estimated impact by workstream**

| # | Workstream | Files | Est. LOC | Risk |
|---|---|---|---:|---|
| A | View-layer combinators | `src/Pages/**`, `src/Pkg/Components/**` | 1,050–1,500 | Low |
| B | `makeEffect` for effects | `src/Data/Effectful/{LLM,Wreq}.hs` | 30–40 | Low |
| C | JSON deriving alias | `src/Pkg/DeriveUtils.hs` + 52 sites | 50–80 (+readability) | Low |
| D | `lookupVec*` + text-display | `src/Utils.hs` + ~10 files | 200–250 | Low |

---

## Workstream A — View-layer combinators (~1,050–1,500 LOC)

**Why:** `Pages/Dashboards.hs` (2,518), `Pages/Anomalies.hs` (1,917), `Pages/Projects.hs`, `Pages/Settings.hs`
rebuild the same markup inline. A combinator library already exists in `src/Pages/Components.hs` (e.g. `panel_`,
`formField_`, `iconBadge_`, `modal_`, `settingsSection_`) and `src/Pkg/Components/Table.hs` — they are just not
used consistently. This is the largest and safest LOC win.

**Sub-tasks (ship as 4 small PRs — they can land in parallel):**

### A1. Button combinators (~200–300 LOC)
- `class_ "btn btn-primary ..."` literals recur ~52×.
- Add to `src/Pages/Components.hs` and export:
  ```haskell
  primaryButton_ :: Monad m => [Attribute] -> HtmlT m () -> HtmlT m ()
  primaryButton_ attrs = button_ (class_ "btn btn-primary btn-sm" : attrs)

  ghostButton_ :: Monad m => [Attribute] -> HtmlT m () -> HtmlT m ()
  ghostButton_ attrs = button_ (class_ "btn btn-ghost btn-sm" : attrs)
  ```
  (There is already `modalCloseButton_` at `Components.hs:327` — reuse it for the ✕ buttons.)
- Find sites: `rg 'button_ \[class_ "btn ' src/Pages`. Replace inline calls; keep any extra classes via the
  `attrs` list so the rendered output is identical.

### A2. Layout-row combinators (~100–150 LOC)
- `"flex items-center justify-between"` appears **83×**.
- Add `headerRow_`, `headerRowPad_`, `sectionHeader_` (one-line `div_ [class_ "..."]` wrappers) to `Components.hs`.
- Find sites: `rg '"flex items-center justify-between' src/Pages`. Preserve any trailing padding classes by
  picking the matching combinator (`headerRowPad_` for `... px-4 py-3`).

### A3. Tables → `Pkg/Components/Table.hs` (~200–300 LOC)
- `Settings.hs`, `Monitors.hs`, `Telemetry.hs` hand-roll `table_ … thead_ … tbody_`. The `Table` abstraction
  (`Pkg/Components/Table.hs:64-293`) already supports columns/rows/features.
- Convert each bespoke renderer to `Table { columns = [...], rows = V.fromList xs, ... }`.
- **Verify carefully:** column order, header labels, and per-cell classes must match the old markup. This is the
  highest-care sub-task in A; diff the rendered HTML in a golden/integration test if available.

### A4. Form fields + handler scaffolding (~150–250 LOC)
- Replace inline `fieldset_/label_/input_` stacks with `formField_` (`Components.hs:440`); extend `FieldCfg` with an
  optional icon if a site needs the leading `faSprite_` (small additive change, keep default `Nothing`).
- Handler boilerplate `(_, _, bw) <- mkPageCtx pid; addRespHeaders $ bodyWrapper bw{...} content` recurs per page.
  Add to `src/Pages/BodyWrapper.hs`:
  ```haskell
  withPageWrapper :: Projects.ProjectId -> Text -> (BWConfig -> Html ()) -> ATAuthCtx (RespHeaders (Html ()))
  withSettingsPage :: Projects.ProjectId -> Text -> Html () -> ATAuthCtx (RespHeaders (Html ()))
  ```
  Migrate page handlers to call these. Confirm the `BWConfig` flags (`isSettingsPage`, `pageTitle`, etc.) are
  threaded identically.

**A verification:** `cabal build all`; run integration/golden page tests (`make test-integration` if the env is
available, else `make test`). Spot-check 2–3 rendered pages locally.

---

## Workstream B — `makeEffect` to delete send-wrapper boilerplate (~30–40 LOC)

**Template already in repo:** `src/Data/Effectful/UUID.hs:26` and `Notify.hs:162` use
`import Effectful.TH` + `makeEffect ''T`. Apply the same pattern.

### B1. `LLM` (clean win, ~9 LOC) — do this first
- `src/Data/Effectful/LLM.hs` hand-writes `callLLM`, `callAgenticChat`, `embedDocuments` as `send` wrappers.
- `makeEffect ''LLM` generates exactly those names (`CallLLM` → `callLLM`, etc.).
- Steps: add `import Effectful.TH`; delete the three `callLLM/callAgenticChat/embedDocuments = send (...)` defs;
  add `makeEffect ''LLM` after the `DispatchOf` instance. Export list stays the same (the generated names match).

### B2. `Wreq` (~30 LOC) — **has a name-clash gotcha**
- `src/Data/Effectful/Wreq.hs` hand-writes ~13 `send` wrappers.
- **Caveat:** the module deliberately re-exports `W.options`, `W.head_`, `W.optionsWith`, `W.headWith` from
  `Network.Wreq` instead of generating wrappers, but the GADT still has `Options`/`Head`/`OptionsWith`/`HeadWith`
  constructors. `makeEffect ''HTTP` will generate `options`/`head`/`optionsWith`/`headWith` smart constructors that
  **collide** with those re-exports.
- Resolution options (pick one):
  1. Keep `makeEffect` and drop the `W.options`/`W.head_`/… re-exports from the module's export list, exporting the
     generated send-functions instead (check callers — `rg 'Wreq.options|Wreq.head_'`). Simplest if nothing depends on
     the wreq-level helpers.
  2. Use `makeEffect_` (generates nothing) — no benefit; skip.
  3. Leave the four `*Options/*Head` constructors hand-wrapped and only `makeEffect` the rest — not possible
     selectively, so fall back to (1) or just hand-trim duplication.
- Recommendation: **(1)**, after confirming callers don't rely on the raw `W.options`/`W.head_` re-exports.

**B verification:** `cabal build all`; `make test-unit`. The golden interpreters (`runHTTPGolden`, `runLLMGolden`)
exercise these — make sure they still compile and pass.

---

## Workstream C — Single JSON deriving alias (~50–80 LOC + big readability)

**Why:** **52** types repeat
`deriving (FromJSON, ToJSON) via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] T`.

- `src/Pkg/DeriveUtils.hs` already hosts deriving newtypes (`SnakeSchema`, `WrappedEnum`, `WrappedEnumSC`). Add the
  alias there and export it:
  ```haskell
  -- A deriving-via alias: snake_case fields, omit Nothing.
  type SnakeOmit = CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]]
  ```
  (A `type` alias over `deriving-aeson`'s `CustomJSON` is enough — no new instances to write.)
- Replace each site:
  ```haskell
  -- before
  deriving (FromJSON, ToJSON) via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] Foo
  -- after
  deriving (FromJSON, ToJSON) via SnakeOmit Foo
  ```
- Find sites: `rg "OmitNothingFields, .*FieldLabelModifier '\[.*CamelToSnake" src` (52 hits). Import `SnakeOmit`
  from `Pkg.DeriveUtils` where needed.
- **Leave alone:** the 30× `DAE.Snake T` sites (already minimal) and the genuinely-custom codecs in
  `Pkg/Parser/Expr.hs` (`Values`, `Subject`) and `Slack.hs`/`Discord.hs` discriminated unions — those are a separate,
  non-mechanical task (Phase 2).

**C verification:** wire format must be unchanged — `cabal build all` then run any JSON round-trip/golden tests
(`make test-unit`, `make test`). If a `git diff` of a serialized fixture changes, the alias doesn't match a site's
original options; revert that site.

---

## Workstream D — De-dup `lookupVec*` and standardize text formatting (~200–250 LOC)

### D1. `lookupVec*` consolidation (~50 LOC)
- `src/Utils.hs:418-440` has 5 near-identical helpers (`lookupVecText`, `lookupVecInt`, `lookupVecTextByKey`,
  `lookupVecBoolByKey`, `lookupVecIntByKey`). Collapse to 2 generic helpers (a positional `lookupVec` returning
  `Maybe a` from an `AE.Value` decoder, and a key-indexed `lookupVecBy`), keeping thin typed shims if callers want them.
- Callers: `rg 'lookupVec' src` (e.g. `Pages/Anomalies.hs`, `Pages/Onboarding.hs`, `Pages/Projects.hs`). Keep
  signatures stable or update call sites in the same PR.

### D2. Standardize on `text-display` (~150–200 LOC)
- Three formatters coexist: `text-display` (15 files, the target), `PyF` (9 files), `Fmt` (`Utils.hs`,
  `Pages/Projects.hs`, `Pages/Settings.hs`).
- Migrate `Fmt`/`PyF` interpolation sites to `text-display` + `<>`/`Display` instances; define `Display` instances for
  recurring numeric patterns (Rate, Percentage, Duration) to kill `show x <> "/hr"`-style code (`Issues.hs:193`).
- Once a library has zero importers, remove it from `hpack-includes/lib-deps.yaml` and run `hpack`.
- **Do this last / incrementally** — it touches the most files; land it file-group by file-group.

**D verification:** `cabal build all`; `make test-unit`. Spot-check that formatted strings (durations, percentages,
byte sizes) are unchanged in rendered output.

---

## Library hygiene (bundle into D's PR, verified facts)

- **Drop `attoparsec`** — only 1 direct importer in `src/`; confirm that one site and remove from `lib-deps.yaml`.
- **KEEP `generic-lens`** (used in 10 files via `Data.Generics.*`), **KEEP `regex`/`regex-tdfa`** (8 files use `=~`).
  (An earlier analysis wrongly flagged these as unused — verified false.)

---

## Suggested order & parallelism

1. **B1 (LLM makeEffect)** — smallest, proves the loop end-to-end.
2. **C (JSON alias)** — mechanical, isolated to `DeriveUtils` + 52 one-line edits.
3. **A1/A2 (buttons + layout rows)** — biggest LOC, low risk, fully parallelizable across pages.
4. **D1 (lookupVec)** then **A4 (forms/handlers)**, **A3 (tables)** — slightly more care.
5. **B2 (Wreq)** — after resolving the export clash.
6. **D2 (text-display) + attoparsec drop** — last, most files, incremental.

## Definition of done (per PR)
- `hpack` (if deps/`package.yaml` changed) → `fourmolu -i` → `make lint` → `cabal build all` → `make test-unit` → `make test`.
- Net negative diff (lines removed > added) for A/D; readability-positive for B/C.
- No change to rendered HTML / JSON wire format / effect behaviour (golden + integration tests green).
