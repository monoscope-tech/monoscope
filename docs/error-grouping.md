# Error grouping: why we re-notify, and the loop that fixes it

Status: **shipped 2026-09-03**, running with auto-apply off.
Measurements are 30 days of prod `apis.error_patterns` taken the same day.

| what | flag | state |
|---|---|---|
| review + refute, records verdicts | `enableErrorGroupReview` | **on** |
| merges rows on a confirmed verdict | `enableErrorGroupAutoApply` | off |
| distils a merge into an ingest-time mask | `enableErrorMaskPromotion` | off |

First live run: 6,019 of 23,397 patterns given a shape, **9 groups reviewed and all 9
survived refutation** — shapes the model named were "RPC hostname", "route ID",
"product SKU", "query parameter values". Nothing merged: every group sits at one
confirmation and `errorGroupEvidenceMet` needs two, which is the gate working.

Turn the flags on in the order above, reading a day of verdicts between each.

## The complaint

We notify about errors we have seen before as if they were new.

## How a notification actually happens

There are **four** paths to a notification, and `canonical_id` gates only one.

```
span with exception
  -> computeErrorHashes  (narrow = the fingerprint, broad = parent_hash)
  -> error_patterns upsert ON CONFLICT (project_id, hash)
  -> NEW row      -> findCanonicalMatch            <- the only merge-aware path
                       hit  -> set canonical_id, no issue, no notification
                       miss -> createIssueForError -> notifyIssue
  -> REGRESSED    -> handleRegression / createIssueForError   (consults neither
                     findCanonicalMatch nor canonical_id)
  -> sweep        -> claimDueErrorNotifications    (BackgroundJobs.hs:1839)
  -> spikes       -> getErrorPatternsWithCurrentRates (ErrorPatterns.hs:397)
```

Four consequences that decide everything below:

1. **A new hash is a new issue is a new notification.** The 24h dedup is keyed on
   `issue.id`, so it never fires for a hash we have not seen. The fingerprint *is*
   the notification gate for new patterns.
2. **`findCanonicalMatch` matches on `(project_id, service, error_type, message)` with
   `message` byte-equal**, and only runs when the pattern row is first created. So
   merging *after the fact* — all the LLM loop does — sets `canonical_id` on rows
   whose issues already exist and have already notified.
3. **The sweep and spike detection do not filter on `canonical_id` or
   `merge_override` at all.** Verified: `claimDueErrorNotifications`' 60-line query
   contains neither identifier, and `getErrorPatternsWithCurrentRates` filters only
   `state != 'resolved' AND NOT is_ignored`. A post-hoc-merged pattern keeps its
   pre-merge issue, keeps accruing hourly stats, and keeps firing ongoing reminders,
   subscriptions and spike alerts. The asymmetry matters: ingest-pre-merged rows
   never got an issue so the sweep's `JOIN … i.target_hash = e.hash` excludes them —
   the leak hits *exactly* the population the LLM loop produces.
4. **The regression path bypasses merges entirely.** `UORegressed` consults neither
   `findCanonicalMatch` nor `canonical_id`: with no prior issue it mints one and
   notifies immediately. Phase 3 will merge thousands of already-*resolved* historical
   patterns, and every one that recurs hits this. Not deferrable — Phase 3 feeds it.

**Together, (2)–(4) mean a merge loop on its own changes nothing a user would
notice.** Anything that reduces notifications has to change ingest *and* teach the
other three paths what a merge is.

## What is actually wrong

### Fixed: the fingerprint shattered on hex

`hexDigest` recognised only lengths 64/40/32/24. Any other hex run fell through to
`integerFb`, which emitted `{integer}` for the leading digits and rescanned the
letters — so `SB-3FDBA2D0A0A2` and `SB-90770E7BA9FD` normalised to *different*
strings. Every order id became its own group, issue and notification.

| | distinct groups (30d) |
|---|---|
| before | 1486 |
| generic `{hex}` at ≥8 | 770 |
| **≥12, shipped** | **783** |
| ≥16 | 1134 |

Shipped as `e00e7ec78`. It rehashes every open pattern, so expect one final
duplication wave as the corrected groups form.

### Remaining: hand-written masks have almost no headroom

| | groups |
|---|---|
| after the hex fix | 783 |
| + URL query stripped | 761 |
| + JSON blobs masked | 761 |
| + SQL value-lists masked | 745 |

5%. Writing more regexes is not the answer.

### Remaining: the headroom is semantic, and it is large

Of the 783, **662 are singletons**. They cluster hard by shape:

| error_type | singleton groups | distinct openings |
|---|---|---|
| Error | 427 | 162 |
| console_error | 54 | 30 |
| TypeError | 52 | 21 |
| AbortError | 13 | 5 |
| **SomeAsyncException** | **11** | **1** |

Eleven issue rows and eleven notifications for one bug. No regex can see that: it
requires knowing that a hostname is a variable while `already cancelled` versus
`already completed` is not.

Estimated reachable: **783 → ~430**.

### Remaining: the tier built for this has never run

`patternEmbeddingAndMerge` is wired and scheduled every 15 minutes
(embeddings → centroid auto-merge → small-LLM judge). Prod:

| table | rows | with embedding |
|---|---|---|
| `apis.error_patterns` | 23,372 | **0** |
| `apis.log_patterns` | 4,071,862 | **0** |

Never, in either table. The 4,261 `canonical_id`s that exist come from
`setCanonicalId` — the *manual* merge a user clicks in the UI. Migration `0121`
recorded the symptom months ago ("canonical_id IS NULL filters NOTHING today,
3,980,129 of 3,980,129 rows") as a query-planning note.

Ruled out: the upsert does not wipe `embedding`; `updateErrorEmbeddings` targets the
right column; `seedJobs` bakes `run_at == scheduledTime` so `unlessStale` is not
eating jobs; zero jobs of this tag have ever failed.

Likely: endpoint LLM verdicts *do* exist on prod, so `callLLM` and the key work in
the container. Only `embedDocuments` produces nothing. `.env` carries
`OPENAI_BASE_URL`; a gateway implementing chat completions but not `/embeddings`
gives exactly this split, with the `logAttention` swallowed by prod's log level.
**Check the prod base URL.** Either way this plan does not depend on embeddings.

## Prior art, and where we cannot copy it

### Sentry

Groups by **stack trace first, exception type second, message last** — the fallback
chain, not a blend. On top of that it ships *grouping enhancements*: built-in
fingerprinting rules for error families known to fan out, plus per-project custom
fingerprint rules, plus automatic detection and skipping of framework and middleware
frames so two runs of the same user code group together.

**The part we cannot use is the part it leans on hardest.** 1245 of our 1486 error
patterns — **84%** — have no stack trace at all, so Sentry's primary signal is absent
for five errors in six and everything rides on the message. That single number is why
this document is mostly about message normalisation and not about frame matching.

**The part we copy directly** is the fallback chain, which `computeErrorHashes`
already implements (stack → type+message → message), and the idea that known-noisy
families deserve declared rules rather than cleverness — `isFrameworkTransportError`
is that mechanism, and Phase 4's learned masks are the same idea with the rules
derived instead of hand-written.

### Drain3 (log template mining)

A streaming template miner: a fixed-depth parse tree plus a similarity threshold
(`sim_th`, default 0.4), and **masking rules** that replace UUIDs, IPs and timestamps
before comparison. Its own documentation is blunt that the two parameters that matter
most are the masking rules and the threshold, and that accuracy improves when you feed
it only the unstructured free-text part of a message.

**We already run Drain** — `Pkg.Drain`, for log patterns — and it is where the
observation that our two normalisation vocabularies had drifted came from: Drain knew
seven token classes `replaceAllFormats` did not.

**Where we deliberately diverge:** Drain's similarity threshold decides "close enough"
with a number. We use an LLM instead, for one measured reason — the residue after
masking is not lexically close. `SomeAsyncException` had 11 groups sharing a single
opening line, and `Error` had 427 singletons across 162 openings; those differ by
hostname, table name and free-text tail, which a token-overlap threshold either misses
or, tuned low enough to catch, starts merging genuinely different bugs. A threshold
cannot tell `already cancelled` from `already completed`. That is the judgement we are
paying a model for, and the reason the loop is propose-and-refute rather than
cluster-by-distance.

### Elastic / Datadog

Both lean on categorisation of the same shape — mask the variable parts, cluster the
rest. Nothing here changes the analysis above.

### What this means for the ordering

Prior art says: normalise hard first, declare rules for known-noisy families, and only
then reach for judgement. That is why the hex fix shipped first (1486 → 783 on its
own), why hand-written masks were measured before being written (5% more, so not
worth it), and why the LLM is aimed at the semantic residue instead of the whole
corpus.

## The design

Port the endpoint loop, which already exists and has already been calibrated:
propose → evidence gate → apply → quarantine → *refute* → revert → distil to
deterministic rules. Errors are **safer to merge than endpoints**: merging endpoints
deletes issues irreversibly, merging errors sets a `canonical_id` that
`unmergeErrorPattern` can unset. Thresholds may start looser for that reason.

### Phase 0 — teach every notification path what a merge is

Without this the rest is decoration. Four parts, all required:

**(a) A shape key at ingest.** New column `shape_hash` on `error_patterns`,
`toXXHash(error_type <> normalised message)`, computed in `computeErrorHashes`
alongside narrow and broad. **A new column, not repurposed `parent_hash`** — that
column holds broad-hash values on all 23,372 existing rows, refreshed only on
resolve, so a shape-keyed lookup would match nothing against pre-deploy rows and
suppression would be silently off during exactly the duplication wave it exists to
absorb. Backfill the new column in the same migration.

**(b) `findCanonicalMatch` resolves on `shape_hash`, gated on a confirmed review —
not unconditionally.** Unconditional shape-suppression is a deterministic ingest-time
auto-merge with none of this document's safety apparatus, and it contradicts the
fingerprint's own definition: when a stack trace exists the message is not hashed at
all, so same-shape-different-stack is *two bugs by our own rules*, and suppressing
the second swallows its first notification. **Keep the `service` dimension** the
current match enforces; dropping it is a cross-service widening nobody asked for.
This means Phase 0 suppresses nothing until Phase 2 verdicts exist — that is the
honest sequencing, and it is why Phase 0 alone is not shippable value.

**(c) Teach the other three paths.** Add `canonical_id IS NULL` to
`claimDueErrorNotifications` and `getErrorPatternsWithCurrentRates`, and make the
`UORegressed` branch consult `canonical_id` before minting or reopening an issue.
Without this, merging is invisible to three of the four notification paths.

**(d) Decide the merged row's existing issue.** Endpoints remap issues to the
canonical during cleanup; errors have no analogue, so today a merged row's
`apis.issues` row simply persists. Either remap to the canonical's issue or close it
with an activity entry — but choose, because "tidies the issue list" is otherwise
only half true (`getErrorPatterns` hides merged patterns; the issues remain).

**Reviews are keyed by shape, not by pattern hash.** The hex fix is re-keying every
open pattern right now; reviews keyed on hash would be orphaned by that wave, while
shape is computed from content the rehash does not change. Verified: a changed hash
INSERTs a new row and orphans the old one with its `canonical_id` and open issue
intact — review rows keyed on shape with `applied_canonical_ids` as row UUIDs survive
that, and the orphaned rows are cleaned by (c)/(d) rather than by the review layer.

### Phase 1 — safety net

New `apis.error_group_reviews`, mirroring `apis.endpoint_group_reviews`:
`group_key` (shape hash), `members_hash`, `member_count`, `first_member_count`,
`verdict`, `confirmations`, `applied_at`, `applied_canonical_ids uuid[]`,
`reverted_at`.

- `inQuarantineSql` equivalent: applied, not reverted, within 24h.
- Revert sets `canonical_id = NULL` and `merge_override = TRUE`, so a group the model
  both merged and disowned stops being re-litigated.
- Unlike endpoints there is no destructive cleanup to gate — nothing deletes error
  patterns — so quarantine here only has to keep the un-assign reachable.

Config: `enableErrorGroupReview` (default on) and `enableErrorGroupAutoApply`
(**default off** for the first deploy, flipped after a day of eyeballed verdicts).
Same rollout the endpoint work used.

Every skip and failure path logs at `logAttention` or emits a metric. The embedding
tier died silently for months behind `logTrace`; that is the failure mode to design
against.

### Phase 2 — propose, then refute

1. Cluster patterns by shape (free, deterministic). No embeddings.
2. Batch 30 shape representatives **per project** into one prompt: "which of these
   are one bug?"
3. **Second call, prompted to refute** the surviving proposals — the fault, not the
   question. Only merges that survive both passes proceed.
4. `mergeEvidenceMet`-style gating: confirmations across passes, membership
   stability via `members_hash`, **plus a mechanical veto**. The endpoint gates that
   are injection-*proof* are the mechanical ones — `routeWordFraction` and
   `shapeAgreementOk` — because they do not consult the model. Confirmations and the
   refute pass both re-read the same attacker-controlled text, so a persistent
   crafted message can clear them. `errorCanMerge` (`PatternMerge.hs:495`) is the
   existing candidate to extend for this.
5. Apply, quarantine, and still re-check on the existing challenge schedule.

**Use the key-echo parser (`parseGroupReview`), not `parseJudgeResponse`.** An error
judge already exists but is index-keyed ("same order as the input"), and the
codebase's own note at `PatternMerge.hs:162` records that small models renumber
batched items — which is why endpoints moved to key echo. Two incompatible parsers
exist; say which one this ports so an implementer does not pick the disqualified one.

**Fix the duplicate-key override first — it affects the shipped endpoint path
today.** `BackgroundJobs.hs:3809` builds verdicts with `HM.fromList`, which is
last-wins, and every batch key is printed in the prompt. A crafted message can emit a
second JSONL line for a *sibling* group and override that sibling's genuine verdict.
First-conflict-wins, or reject duplicate keys outright.

### Phase 3 — sweep the whole corpus

`getUnembeddedErrorPatterns` only ever saw new rows; the 23,372 existing ones have
never been reviewed. ~3k shapes ÷ 30 per batch ≈ 100 calls, one-off.

**Resumable with a cursor, bounded per run.** We have a live incident of exactly the
opposite (`replay_merge_succeeds_without_merging`: a job that marked the whole
session done after its first 25-file page and stranded 183 sessions). Progress marker
per shape batch; idempotent re-runs.

### Phase 4 — distil to deterministic rules

Mirror `promoteConfirmedIdRules`: derive a mask from a confirmed group, safety-test
it, persist it, apply it at ingest for free thereafter. Ships last.

Per-rule safety test, mechanical and cheap: anchored (no bare wildcard), scoped to
project + error_type, and **applied to the 30-day corpus it must not merge shapes the
judge kept separate**. New events only — a learned mask changes hashes at ingest, and
old patterns are already handled by the shape-level merge, so no backfill rehash.

**Known wrinkle, accepted:** a learned mask changes the normalised message and
therefore the *shape*, so old and new rows of one logical group end up under
different shape keys and reviews keyed on the old shape go stale. That is the same
orphaning Phase 0 solved for the hex wave, reappearing per rule. It is bounded — one
rule affects one narrow family — and the shape-level canonical merge still unifies
them, but a rule promotion should re-key its affected reviews rather than pretend the
problem does not exist.

## Risks

- **Customer data leaves for OpenAI.** This is the first time *error messages* would
  be sent; endpoint prompts send URL paths today. Mitigated by sending the
  **normalised shape** (placeholders already substituted for ids, timestamps, hex,
  emails, JWTs) rather than raw text — which is what we cluster on anyway. Called out
  explicitly so it is a conscious acceptance, not a side effect.
- **Prompt injection.** Error messages are attacker-writable text entering a prompt;
  a crafted message can argue for its own merge. Inherited mitigations: strict output
  parsing, evidence gates, the refute pass, quarantine, and `merge_override` as the
  human veto. No prompt output is trusted as a command.
- **Cross-tenant leakage.** Prompts are per project, always. Batching shapes from
  different projects into one call would leak between tenants inside our own vendor
  calls.
- **A wrong merge hides a real error.** Bounded by quarantine + refute + revert, and
  by the fact that nothing is deleted.

## Testing

`runLLMGolden` exists. Integration tests through the real job with golden LLM
responses, one per loop stage: propose→apply, refute-blocks-apply, quarantine-revert
restores `canonical_id = NULL` + `merge_override = TRUE`, sweep-resumes-from-cursor.

## Deferred

- Reviving the embedding tier (separate thread; check the prod base URL first).
- Redesigning `parent_hash` as a general hierarchical grouping key beyond Phase 0.
- Regression-state semantics across a merged family when a canonical resolves
  mid-quarantine.
