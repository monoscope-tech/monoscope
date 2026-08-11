# Next steps — as of 2026-08-11 evening

Everything below is open. Ordered by "what bites first if ignored", not by size.
Companion to [README.md](README.md) (the overnight batch) and
`timefusion/docs/plans/2026-08-11-certification-survival.md`.

## P0 — watch, because something just changed under production

- [ ] **Verify the two deploys landed.** monoscope `5f23b3946` (code-context / github
      credentials, includes migration **0124**) and TimeFusion `0308015`+ (dedup certification
      persistence). ~28 min each. Migration 0124 is destructive-ish — it drops
      `code_mappings.github_sync_id` after carrying rows across — so confirm the runner
      actually reached it rather than halting on an earlier checksum mismatch.
- [ ] **Watch `count(*)` correctness for a day.** TF certification persistence is now ON, which
      makes the read-side dedup skip fire at a real rate for the first time. The skip removes
      `DedupExec`; if the sweep's certification rule has a latent flaw, this is what surfaces
      it. Compare a few dashboard counts against a known reference.
      Kill switches, in order: `timefusion_dedup_certification_persist=false` (cold cache per
      process) → `timefusion_read_dedup_skip_swept=false` (removes the skip entirely). Reach
      for the second if a count is doubted.
- [ ] **Confirm the parity guard is meaningful now.** `count_is_identical_with_and_without_the_dedup_skip`
      only began genuinely exercising the skip today — before that it ran without a time bound
      and never engaged it. The rule is now covered by a test that is hours old, not by a
      production track record. That is the actual risk in the item above.

## P1 — the measurement everything else waits on

- [ ] **Read Phase 0.** After **≥24h with no TF deploy**:
      ```sql
      SELECT key, value FROM timefusion_stats WHERE component='scan' AND key LIKE 'dedup_denied%';
      SELECT key, value FROM timefusion_stats WHERE component='scan' AND key LIKE 'cert_%';
      ```
      Counters reset on restart, so diffing two scrapes is only valid inside one lifetime — and
      TF deploys several times a day, so this needs arranging deliberately.
      Decide from `dedup_denied_never_certified_pct` and `cert_dwell_p50_secs`:
      - `fp_moved` dominates → persistence earns little. Set the flag false and close it out.
      - `never_certified` still dominates → find out what is still blocking certification.
      - `dedup_skipped_pct` is now the headline. It was 0.2–0.5%; if it has not moved
        materially, persistence did not help and should be turned back off.

## P2 — known-incomplete work from the overnight batch

- [ ] **Issue view still renders a raw `<pre>` stack trace.** `Pages.Anomalies` was left alone
      because `RuntimeException` issues carry a *synthesised* stack (see
      `haskell_backtraces_unavailable`), a different shape from a span's
      `exception.stacktrace`. Pointing `stackTrace_` at it without checking those strings would
      be guessing. Needs someone to look at real payloads first.
- [ ] **`fetchSnippet` has no cache.** One GitHub API call per frame opened. `intersect once`
      plus one-frame-open-by-default keeps it near one call per error *viewed*, but a hot issue
      viewed repeatedly re-fetches. Add a `Pkg.QueryCache` layer keyed on `(repo, ref, path)`
      before this is on by default for large projects — a rate-limit stall would present as the
      panel silently not filling in.
- [ ] **Source-map / debug-file symbolication is still out of scope.** Minified JS and stripped
      native frames resolve to nothing. `fetchSnippet` is the seam it plugs into. Only worth
      doing if customers actually hit it.
- [ ] **`plans/06-tf-dedup-skip-incident.md` is stale.** It still reads as an open incident with
      an unverified patch; the fix shipped (`f8d7278`) and the service map recovered. Either
      mark it resolved or fold it into the TF plan doc and delete it.

## P3 — hygiene and follow-ups

- [ ] **Three integration tests fail locally, pre-existing** (confirmed against `4aa402626`):
      the `preload="false"` assertion vs the shell's `hx-preload="false"`, and two that need a
      local TimeFusion. The first is a real (small) bug in either the test or the shell; the
      other two are environmental.
- [ ] **`object_store_cache::tests::test_basic_operations` is flaky in TF.** Failed once in a
      full parallel run, passed in isolation and on a clean rerun of the whole suite. Shared
      cache dir across parallel tests is the likely cause. Not caused by recent work.
- [ ] **Environment switcher is shipped but unexercised.** The TF column
      (`resource___deployment___environment___name`) is committed and the selector defaults to
      nothing selected, so no query filters on it yet. Worth sending real staging traffic
      through and confirming the picker actually partitions it.
- [ ] **Latency-column rethink was one pass, not a conclusion.** `plans/03` explored directions;
      the by-service/by-kind breakdown shipped, but the child-row UX at depth is still the
      weak part. Watch how it reads on a genuinely deep trace before iterating further.

## Standing risk, not a task

**Two Claude sessions share these working trees.** Earlier today a `git reset --hard` in the
TimeFusion tree discarded another session's uncommitted edits (`src/database.rs`,
`src/metrics.rs`, `tests/e2e/hot_tail_sorted_footer.rs`); committed work was safe, and
surviving stash commits `6650dd74` / `10a95a8e` / `58b3c53c` / `54bec045` still hold most of
it. It has not been confirmed whether anything was permanently lost.

Whatever the outcome there, the practice that follows is: **work in a `git worktree`, not in
the shared checkout** — which is how the certification work above was done, and why it could
be rebased onto the other session's rollup commits without either side losing anything.
