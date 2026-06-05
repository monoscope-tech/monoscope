# Phase 2 Conciseness Plan — Control-Flow & Error-Handling (medium risk)

**Goal:** consolidate the duplicated control-flow and error-handling in the background-job subsystem —
chiefly `src/BackgroundJobs.hs` (3,603 LOC) — into a small set of named combinators, and replace ad-hoc
`Either`/`Maybe`/`tryAny` plumbing with standard primitives. Target **~350–570 LOC removed** plus a sizeable
grokability win: `processBackgroundJob` becomes a readable dispatch table over well-named handlers.

**Branch:** `claude/blissful-pasteur-ZlPQl` (one PR per workstream; E→F→G→H ordered, I optional).

**Guardrails (every PR):**
- **Behaviour-preserving.** `BackgroundJobs` is a hot path with real side effects (email, Slack, Postgres,
  retries). Preserve: retry counts/backoff, "log-and-continue" isolation semantics, and every `Log.logAttention`
  on failure. A refactor that swallows or reorders failures is a regression.
- Build `cabal build all`; test `make test-unit` then `make test` (and `make test-integration` if the env is up).
- `fourmolu -i <changed>` · `make lint` (`hlint src`) · `hpack` if deps change.
- Keep diffs mechanical and per-workstream so review stays easy and reverts are cheap.
- **Out of scope (deferred to Phase 3):** the dual `hasql`/`postgresql-simple` stack and `barbies`/HKD. Do not
  touch SQL execution semantics here.

**Impact by workstream**

| # | Workstream | Anchor | Est. LOC | Risk |
|---|---|---|---:|---|
| E | Unify the retry / try-step primitives | `BackgroundJobs.hs:825,1558,1582,2069` | 60–90 | Low |
| F | Email-job combinator | `BackgroundJobs.hs:243,268,274,280,2410,2567` | 40–70 | Low |
| G | Declarative sequenced steps | `BackgroundJobs.hs:572,633-728,1813,2075,2676` | 40–80 | Low |
| H | Error-primitive sweep (`tryAny`/`either`/nested `case`) | `BackgroundJobs.hs`, `Web/ApiHandlers.hs` | 150–250 | Med |
| I | Dispatch-table altitude (optional) | `processBackgroundJob:238` | 50–120 | Med |

---

## Workstream E — One retry primitive + one try-step primitive (~60–90 LOC)

**Problem:** `retryOnDeadlock` is duplicated **verbatim** in ≥3 places (`:1558`, `:1582`, and the `dualExecPgTf`
arm), differing only in the log string. Separately, `tryLog` (`:825`) and `tryOp` (`:2069`) are near-identical
"run, catch `SomeException`, log, continue" combinators — and `tryLog` hardcodes
`"LogPattern pipeline step failed: "` even though it's reused for unrelated steps (`:1813` `propagateMergedCountsBatch`,
`:2676` `errorPatternEmbedding`), producing **misleading logs**.

### E1. Single `retryOnDeadlock` (use the `retry` library — already a dep)
- Lift one definition to module top-level, parametrised on a label, and implement it with `Control.Retry`
  (`retry` is in `hpack-includes/lib-deps.yaml`) instead of hand-rolled recursion:
  ```haskell
  -- BackgroundJobs.hs (top-level)
  retryOnDeadlock :: (DB es, Log :> es) => Text -> Eff es a -> Eff es a
  retryOnDeadlock label =
    recovering
      (constantDelay 50_000 <> limitRetries 2)        -- 50ms, matches current backoff
      [const $ Handler \e -> case fromException @Hasql.HasqlException e of
         Just he | Hasql.isDeadlockError he -> True <$ Log.logAttention (label <> " deadlock, retrying") (show @Text e)
         _ -> pure False]
      (const action)
  ```
  (Keep the simple hand-rolled version if you prefer zero new call surface — the point is **one** definition.)
- Delete the two `where`-bound copies; update `pgOnlyExec`/`dualExecPgTf` to call the shared one with their label.
- **Care:** preserve the exact backoff (50ms then 100ms) and that non-deadlock exceptions still `throwIO`.

### E2. Unify `tryLog` + `tryOp` into `tryStep`
- One combinator, accurate per-call label:
  ```haskell
  tryStep :: Text -> ATBackgroundCtx () -> ATBackgroundCtx ()
  tryStep label = (`catch` \(e :: SomeException) -> Log.logAttention ("step failed: " <> label) (show e))
  ```
- Replace all `tryOp` and `tryLog` call sites (≈21 sites) with `tryStep`. This also **fixes the wrong log prefix**
  on the non-logpattern steps.
- **Care:** `tryOp` and `tryLog` both currently catch `SomeException` and continue — keep that semantics exactly.

**E verification:** `make test-unit`; grep that no `retryOnDeadlock`/`tryOp`/`tryLog` definitions remain besides the
single canonical ones (`rg 'retryOnDeadlock|tryOp|tryLog =' src/BackgroundJobs.hs`).

---

## Workstream F — Email-job combinator (~40–70 LOC)

**Problem:** ≥6 arms repeat *fetch user → guard `Just` → build `(subj, html)` via `ET.*` → `sendRenderedEmail`*
(`:243`, `:268`, `:274`, `:280`, `:2410`, `:2567`).

- Add one combinator:
  ```haskell
  -- Fetch a user, and if present, send the email produced from them.
  emailUser :: Projects.UserId -> (Projects.User -> (Text, Html ())) -> ATBackgroundCtx ()
  emailUser uid mkMail = whenJustM (Projects.userById uid) \user ->
    let (subj, html) = mkMail user
    in sendRenderedEmail (CI.original user.email) subj (ET.renderEmail subj html)
  ```
- Rewrite the simple arms, e.g. `CreatedProjectSuccessfully`:
  ```haskell
  CreatedProjectSuccessfully userId projectId reciever projectTitle ->
    emailUser userId \u -> ET.projectCreatedEmail u.firstName projectTitle (projectUrl projectId)
  ```
  (Add a tiny `projectUrl pid = authCtx.env.hostUrl <> "p/" <> pid.toText` helper — that literal recurs too.)
- `ErrorAssigned` (`:280`) has a 3-way `case (projectM, errM, userM)`; convert to `do`-notation in `MaybeT` or
  chained `whenJustM`, only sending when all three resolve (preserve the `err.projectId == pid` guard and the
  `_ -> pass` fall-through).

**F verification:** rendered email subjects/bodies must be unchanged — assert via existing email/golden tests or a
manual `ET.renderEmail` diff on one sample per template.

---

## Workstream G — Declarative sequenced steps (~40–80 LOC)

**Problem:** several blocks are long runs of independent `tryLog "name" $ action` lines (`:572-574`, `:633-728`,
`:1813-1814`, `:2075-2077`, `:2676-2677`).

- Express each block as data + a single traversal:
  ```haskell
  traverse_ (uncurry tryStep)
    [ ("calculateLogPatternBaselines", calculateLogPatternBaselines pid)
    , ("processNewLogPatterns",        processNewLogPatterns pid authCtx)
    , ("pruneStaleLogPatterns",        pruneStaleLogPatterns pid)
    ]
  ```
- This keeps the isolate-and-continue behaviour (each step independently caught) while making the pipeline a
  readable list. For the `do`-block variants (`:633-728`) that build report sections, only fold the ones that are
  pure `tryLog step` sequences — leave blocks with interleaved logic alone.
- **Category-theory note (optional, behaviour-changing — flag for review):** if you ever want to *aggregate*
  step failures instead of only logging each, `Validation`/`[Either]` + `foldMap` is the principled form. Current
  behaviour is fire-and-log; keep it unless product wants aggregation.

**G verification:** `make test-unit`; confirm step order is preserved (traversal over the list is left-to-right).

---

## Workstream H — Error-primitive sweep (~150–250 LOC, the bulk)

**Problem:** broad ad-hoc plumbing — `tryAny ... >>= \case Right/Left` (×66 in src), nested
`case (Just, Just, Just)`, `either (\e -> ...) (\x -> ...)`, and `fromMaybe` chains (×639). Scope this PR to
`src/BackgroundJobs.hs` and `src/Web/ApiHandlers.hs` (largest concentrations) to keep it reviewable.

- `tryAny act >>= \case Left e -> log; Right x -> use x` → `tryStep`/a `Maybe`-returning `tryLogValue`:
  ```haskell
  tryValue :: Text -> ATBackgroundCtx a -> ATBackgroundCtx (Maybe a)
  tryValue label act = tryAny act >>= either (\e -> Nothing <$ Log.logAttention label (show e)) (pure . Just)
  ```
  then consume with `whenJust`/`traverse_`.
- `whenLeft_`/`whenJust`/`whenJustM` (relude) for the manual `case Left e -> ...` and `case Just x -> ...` arms
  (×18 `whenLeft`, ×195 `whenJust` already in use — make the stragglers consistent).
- `note`/`hush`/`exceptT` from the **`errors`** library (already a dep) for `Maybe`→`Either` and `Either` collapse.
- `for_`/`traverse_`/`traverse` to replace manual `forM_`-then-unwrap and the usage-report `either` block (`:480-507`).
- Default extraction: collapse `fromMaybe d0 (fromMaybe d1 ...)` ladders and the per-field `fromMaybe ac.x patch.x`
  record merges (`ApiHandlers.hs:202-218`) — note these record merges are the natural Phase 3 `barbies` candidate,
  so just tidy them here, don't redesign.

**H verification:** highest-care PR. `make test`; for each touched job/handler confirm the failure path still logs
and the success path is unchanged. Prefer many small commits (one pattern family at a time) over one large diff.

---

## Workstream I — Dispatch-table altitude (optional, ~50–120 LOC)

**Problem:** `processBackgroundJob` (`:238`) is ~38 arms; many already delegate to a named helper
(`generateOtelFacetsBatch`, `newAnomalyJob`), but several inline 5–20-line blocks.

- Extract each remaining inline arm into a top-level `handleX :: Config.AuthContext -> ... -> ATBackgroundCtx ()`
  so the `case` becomes a one-line-per-constructor dispatch table. This is mostly *moving* code (modest net LOC) but
  is the biggest readability lever for the file and makes each handler unit-testable in isolation.
- Do this **after** E/F/G so the extracted handlers already use the new combinators.
- **Care:** keep `where`-bound locals (e.g. `withAdvisoryLock` for `DailyJob`) with their handler.

**I verification:** pure move-refactor — diff should show equal logic relocated; `make test`.

---

## Suggested order
1. **E** (retry + try-step) — unblocks F/G/H by giving them the canonical combinators.
2. **F** (email) — small, self-contained, obvious win.
3. **G** (sequenced steps) — small, declarative.
4. **H** (error sweep) — the bulk; land incrementally, one pattern family per commit.
5. **I** (dispatch altitude) — optional polish once the handlers are clean.

## Definition of done (per PR)
- `hpack` (if deps changed) → `fourmolu -i` → `make lint` → `cabal build all` → `make test-unit` → `make test`.
- Net-negative diff for E/F/G/H; net-neutral but readability-positive for I.
- No change to side-effect behaviour: retries, failure logging, email/Slack output, and step ordering all identical.
- Single canonical definitions remain for `retryOnDeadlock` and `tryStep` (no duplicates).
