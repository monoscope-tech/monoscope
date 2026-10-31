# Open PR integration for the unified agent

Status: twenty related PR heads consolidated locally, 2026-10-30. Draft #619 is the canonical integration PR.
The remote branch has not been updated. Combined verification and unified-plan implementation remain in progress.
This review covers all 28 open PRs returned by GitHub at the review snapshot.
It maps dependencies and inspects relevant source changes. It is not a merge approval or full correctness review.

The 1 November GitHub recheck still lists 28 open PRs. All twenty selected incoming heads remain ancestors of the local branch, including the impact stack through #709, divergent runtime #692, production evidence #661, Slack delivery #571, and health budget #568. The published #619 head is also retained. No selected head needs another merge, and the open PR set is unchanged at this recheck. This confirms integration identity, not a correctness verdict; the combined behavior still requires the tests and release checks below.

## Recommendation

Reconcile existing agent work before implementing the [unified plan](2026-10-30-unified-agent.md).
Keep the current branch's shared tools, routine authorization, run history, and conversation panel.
Reuse the impact-agent stack's wiki tools, attempts, receipts, usage records, and retained artifacts.
Resolve the two sandbox runtime designs into one implementation before exposing their capabilities here.
Do not merge every open PR into the Slack branch.

The large impact-agent work remains a separate product scope.
The unified agent can reuse its execution infrastructure without enabling code changes or deployments through Slack.

## Snapshot and evidence

| Reference | Reviewed commit |
| --- | --- |
| Current Slack branch | `4c8e115f746b65f6220db1851f5da8dab94e0e1d` |
| Latest fetched master | `c71ce414c80221f32f254629100157a3f39a4b33` |
| Published #619 | `0b5dc1b5caa47c43550e4febde26e75bdf888055` |
| Impact checkpoint #693 | `9a37e4cb15601ea7396bac7bc4fd436f710157ec` |
| Impact stack tip #709 | `6586264829` (commit prefix) |
| Haskell runtime refactor #692 | `104bc1909c3e0773bfe381c02e1f65fd05bfa198` |

The published Slack PR does not contain all current local routine and panel work.
Rebased history also differs, so commit ancestry alone cannot identify equivalent Slack changes.
Use the local tree and behavior regressions during reconciliation.

Evidence includes PR descriptions, checks, changed-file inventories, fetched Git history, and selected source diffs.
The large PRs exceed GitHub's 100-file list in the initial query. Local Git comparisons cover their complete trees.
The review also uses `git merge-tree` previews against the pinned local HEAD.
These previews create Git objects but do not change the branch, index, or worktree.
They exclude uncommitted plan files and do not establish runtime compatibility.

The initial review was read-only. The local integration and verification that followed are recorded below.

## Existing impact-agent stack

Git ancestry establishes this exact parent-to-child chain:

```text
#693 -> #694 -> #696 -> #697 -> #698 -> #699 -> #700 -> #701
     -> #702 -> #703 -> #704 -> #705 -> #706 -> #707 -> #708 -> #709
```

The tip includes all parent changes. It is not sixteen independent implementations.
Use the stack tip for combined integration testing.
The user chose consolidation. Incorporate the stack tip once into #619; do not land its sixteen drafts separately.

| PR | Contribution | Treatment |
| --- | --- | --- |
| [#693](https://github.com/monoscope-tech/monoscope/pull/693) | Pi/Gondolin review and fix infrastructure, durable receipts, usage, artifacts, cache maintenance, and conversational wiki tools. | Reuse the shared infrastructure. Preserve the Slack branch's newer routine and authorization contracts. |
| [#694](https://github.com/monoscope-tech/monoscope/pull/694) | Haskell impact-evaluation orchestration. | Keep within the stack. Do not create another evaluation runner here. |
| [#696](https://github.com/monoscope-tech/monoscope/pull/696) | Haskell source-review comparisons and validation. | Keep its common validation and evidence contracts. |
| [#697](https://github.com/monoscope-tech/monoscope/pull/697) | Offline guest localhost resolution. | Retain the guest correction and regression. |
| [#698](https://github.com/monoscope-tech/monoscope/pull/698) | Host mappings and regression input guidance. | Preserve through runtime reconciliation. |
| [#699](https://github.com/monoscope-tech/monoscope/pull/699) | Complete regression patches and explicit parse failures. | Preserve complete input transport and useful failures. |
| [#700](https://github.com/monoscope-tech/monoscope/pull/700) | Caller tracing and review-premise guidance. | Reuse existing policy text rather than adding another prompt system. |
| [#701](https://github.com/monoscope-tech/monoscope/pull/701) | Frozen regression patches from complete test files. | Retain the Haskell input boundary and regression constraints. |
| [#702](https://github.com/monoscope-tech/monoscope/pull/702) | Isolated scratch clones of protected source. | Preserve source protection and private workspace behavior. |
| [#703](https://github.com/monoscope-tech/monoscope/pull/703) | Fingerprints for tracked submodule directories. | Retain the source inventory correction. |
| [#704](https://github.com/monoscope-tech/monoscope/pull/704) | Native advisory file tools selected by Haskell policy. | Preserve the role policy through the runtime merge. |
| [#705](https://github.com/monoscope-tech/monoscope/pull/705) | Native Pi guidance and one review response schema. | Retain the shared schema and native guidance. |
| [#706](https://github.com/monoscope-tech/monoscope/pull/706) | Shell deadlines that preserve the review workspace. | Required regression for the reconciled runtime. |
| [#707](https://github.com/monoscope-tech/monoscope/pull/707) | Typed evaluation modes and selectors. | Keep typed selectors and the shared runner. |
| [#708](https://github.com/monoscope-tech/monoscope/pull/708) | Screenshot image bytes through native file reads. | Required regression for the reconciled runtime. |
| [#709](https://github.com/monoscope-tech/monoscope/pull/709) | Review evidence guidance and recorded model comparisons. | Preserve research records. Do not treat comparison results as proof of general accuracy. |

All sixteen PRs are drafts at the snapshot.
Their Gate checks report success, with several downstream jobs skipped.
Their descriptions record local signoff and separate VM evidence.
Those results do not establish that the newly combined Slack tree passes.

## The divergent runtime: #692

[PR #692](https://github.com/monoscope-tech/monoscope/pull/692) is not a descendant of #693.
Neither tip includes the other. The trees share earlier work but contain different subsequent improvements.

#692 moves production VM ownership and fix orchestration into Haskell.
It adds `AgentWorkspace.Guest`, `CodeReview.Repair`, and `pi-host.mjs`.
The #693/#709 line contains later outbox, Git isolation, chat memory, and evaluation work.
It retains a different worker entry point and lacks the two Haskell runtime modules above.

Use #709 as the recorded product checkpoint, then reconcile #692's native runtime changes into it.
Do not restore complete #692 files over newer stack files.
Keep one production VM owner, one protocol, and one set of host tool handlers.
Remove replaced JavaScript workflow paths after equivalent Haskell behavior passes.
Adapt #706's timeout recovery and #708's image transport to that same runtime.

The smaller conflict count for #692 does not make it a complete replacement for #693.
#692's description reports failed container SDK cases and unresolved environment coverage.
Its snapshot also contains a failed CodeQL check. A later check-run inspection identifies incomplete HTML-comment sanitization in `scripts/fa-add.mjs:44`.
That unrelated icon-script rewrite is excluded from this integration; the existing Python helper remains.
This explains that imported PR check failure but does not establish a CodeQL pass for the combined tree.
The combined runtime requires provisioned Linux/KVM coverage, protocol bounds, receipt replay, and cleanup evidence.

## Other open PRs

| PR | Relevance and decision |
| --- | --- |
| [#661](https://github.com/monoscope-tech/monoscope/pull/661) | Targeted production evidence and matching/total time series for PR reviews. Reconcile its behavior with the stack's telemetry query tool. Reuse scoped query parsing and chart rendering. Do not keep two model query planners. Preserve aggregate-publication opt-in and historical decoding. |
| [#619](https://github.com/monoscope-tech/monoscope/pull/619) | This Slack implementation's published PR. Retain local routines, snapshots, member attribution, shared effects, and conversation panel. Its current description and remote tip lag local work. Update only after integration and final signoff. |
| [#611](https://github.com/monoscope-tech/monoscope/pull/611) | Older Slack plan already represented here. Mark it superseded or close it after maintainers review the unified plan. Do not merge the old document over local history. |
| [#571](https://github.com/monoscope-tech/monoscope/pull/571) | Direct dependency for Slack delivery acceptance. It restores threadless fallback, bounded root recovery, delivery diagnostics, reconnect notices, and suggested prompts. Port the behavior and regressions into current receipt/run handling. Preserve private account-link delivery. Its static prompts are a reusable starting point, not complete dynamic prompts. |
| [#568](https://github.com/monoscope-tech/monoscope/pull/568) | Bounds infra-health work to five minutes. Relevant to shared worker availability. Reconcile as a small independent fix with cancellation evidence. Do not add broader legacy-PG reconciliation during the TF migration. |
| [#483](https://github.com/monoscope-tech/monoscope/pull/483) | Generic OIDC. Keep as an independent authentication PR. Preserve user IDs and current membership semantics at the agent boundary. Include Slack linking, callback session continuity, logout, and revoked access in joint regression coverage. It is not a prerequisite for the agent features. |
| [#486](https://github.com/monoscope-tech/monoscope/pull/486) | Tooltip confinement. Independent shared-chart correction. Rebase against current widget code and regenerate Cabal output. Do not build an agent-specific tooltip implementation. |
| [#593](https://github.com/monoscope-tech/monoscope/pull/593) | Dockerfile SARIF scan. Independent CI decision. It adds a workflow outside `ci/checks.tsv`. Reconcile with the repository's single check definition before adoption. It scans Dockerfile configuration, not every new worker image. |
| [#421](https://github.com/monoscope-tech/monoscope/pull/421) | Hardware visualization prototype. Defer from this integration. It touches route and frontend registration but supplies no required agent foundation. Snapshot frontend/UI checks fail. |
| [#333](https://github.com/monoscope-tech/monoscope/pull/333) | Protobuf UTF-8 repair. Independent ingestion work with failed build/lint checks. Its schema-blind rewriting of length-delimited bytes needs separate correctness review. Do not combine it with agent rollout. |
| [#265](https://github.com/monoscope-tech/monoscope/pull/265) | Broad roadmap document. Keep separate from the executable agent plan. Old failed test checks do not provide evidence about current agent behavior. |

## Concrete consolidation points

### Tool and authority contracts

The Slack branch supplies `ProjectTools`, operation discovery, and explicit member-based operation policy.
The impact stack supplies role-scoped schemas and telemetry/wiki tools through `CodeReview.Policy`.
Extract shared capability descriptors without replacing domain-specific role restrictions.
Keep runtime and provider presentation as adapters around those descriptors.

The impact stack adds `SessionAccess`, `InteractiveWithMemory`, and `memory_granted_by`.
The Slack branch already has `MemberAccess`, event-run authorization, and routine owners.
Do not preserve both member constructors or add memory as another invocation mode.
Resolve memory authority through the unified execution policy.
Keep already-applied grant migrations and interpret legacy grants conservatively during conversion.
Do not apply interactive sudo fallback to background or Slack work.

### Runs, attempts, usage, and artifacts

Reuse routine runs as the user-visible execution record.
Keep review/fix records for their domain proofs and retained outputs.
Reuse the stack's typed agent attempts and durable usage/model-call receipts.
Add typed links between these records where necessary. Do not cast IDs to simulate identity.

Reuse `CodeReview.Outbox` for its supported receipt lifecycle.
Extend shared receipt machinery only where scope and acknowledgement semantics agree.
Slack publication reservations still represent uncertain message delivery.
They must not disappear merely because an accounting outbox also exists.

The stack's receipt handling does not establish safe retries for arbitrary project writes.
The unified plan still needs operation identities, transactional completion, and external reconciliation.

### Memory

Port the existing list/read/write contracts and bounded wiki documents from the stack.
Reuse optimistic revision handling and current permission checks.
Keep historical `Conversation` authors readable where the stack introduced them.
For new executions, use real attempt attribution where available.
Do not invent an agent attempt by casting a conversation or routine ID.

### Review evidence

#661 adds bounded filter planning and matching/total series.
The impact stack adds interactive scoped telemetry queries and shared PNGs.
Use one project/service/time-scoped evidence boundary and one chart renderer.
Keep query failures distinct from zero matches.
Keep publication visibility independent of investigation permission.

### Existing master changes

Integrate current master before resolving agent-stack conflicts.
It includes shared saved-view changes and removal of the unauthenticated `/chart_data_shot` route.
Preserve that route removal during conflict resolution.
Keep the existing shared widget PNG path for agent evidence.

### Migrations

Keep existing filenames and contents for applied migrations.
The impact branches introduce `0229`–`0247` and conversation/grant migrations with `0254`/`0255` prefixes.
The local branch also contains unrelated migrations with those prefixes and event routines through `0258`.
Different filenames can coexist, but migration prerequisites require inspection in full filename order.
Do not renumber old files to make the combined list look sequential.
Allocate new authority or linking migrations only after inspecting current master and the selected agent stack.

## Merge previews

These are predicted conflicting file counts against the pinned local HEAD, not semantic conflict counts.

| Incoming tree | Conflicting files |
| --- | ---: |
| Current master | 3 |
| #693 or #709 | 19 |
| #692 | 8 |
| #661 | 2 |
| #571 | 3 |
| #568 | 1 |
| #483 | 6 |
| #486 | 2 |
| #611 | 1 |

Master conflicts occur in `Web/Routes.hs`, `AgenticSpec.hs`, and `ErrorPatternsSpec.hs`.
The agent checkpoint conflicts also cover routines, memory, member permissions, AI dispatch, review handlers, and CI.
Regenerate `monoscope.cabal` after resolving source declarations rather than selecting one generated side.
Repeat previews after master integration because conflict counts and boundaries can change.

## Execution order and acceptance

1. Preserve the current local plan and implementation changes.
2. Integrate current master and exercise affected route, authentication, saved-view, and bot regressions.
3. Reconcile the #693–#709 stack into this branch on an integration branch.
4. Port #692's runtime improvements without losing later stack behavior.
5. Reconcile #661's measured evidence within the selected evidence path.
6. Port #571's delivery behavior and prompts into current Slack adapters.
7. Reconcile #568 independently before long-running agent workloads share the worker pool.
8. Run the shared capability, authority, memory, receipt, Linux/KVM, and publication regression matrix.
9. Resume the unified plan at the remaining work, with replaced implementations removed.

For the runtime reconciliation, require native file tools, image bytes, shell-timeout recovery, and role restrictions.
Also require interrupted receipt replay, frozen checks, private source isolation, VM cleanup, and usage attribution.
For the Slack reconciliation, require routine activation, cancellation, private linking, threadless recovery, and duplicate delivery handling.
For evidence, require project isolation, bounded results, publication privacy, and historical artifact readability.

Keep OIDC, tooltip, scanning, hardware, ingestion, and roadmap PRs outside this mandatory merge chain.
Their owners can progress them independently with the shared compatibility tests above.
Refresh PR status before any merge decision. A draft or an old green check is not current integration evidence.

Run the required review loops, simplification, local signoff, and image checks only after implementation finishes.
This review does not authorize posting PR comments, closing PRs, changing live Slack configuration, or production deployment.

## Current-head refresh (31 October)

The refreshed inventory contains 28 open PRs. All twenty selected current heads remain ancestors of this integration branch: #568, #571, #619, #692, #693, #694, and #696–#709. The #661 behavior remains deliberately adapted through shared query and evidence code rather than merged as a second planner. #695 is outside that agent chain; its RUM/dashboard squash merge was already included.

Fetched master now ends at `3c34f3c83`, two commits beyond the previously integrated baseline. This local merge retains #758's per-table SQL isolation and signed browser SQL, plus #759's TLS/dependency update. Conflict resolution preserves this branch's typed issue request identity alongside the new SQL signature, and keeps the required qualified Base16 import without restoring an unused wire import.

The web-components watcher rebuilt successfully and TypeScript checking passes. The server watcher compiles the combined sources; application startup still fails against the pre-existing configured database credentials. All 87 dashboard/widget integration examples pass on the merged tree, including SQL isolation, signed browser SQL, and template compatibility. A duplicate clock import reported after the merge is removed; the rerun is warning-free. The subsequent authority-loss slice passes all 413 bot, 76 API, and 41 incident-delivery examples on the merged tree. No PR update, remote merge, app configuration change, or deployment has occurred. Final signoff and live acceptance remain outstanding.

## Local consolidation progress

- `39d50b1a1` integrates master through `c71ce414c`, including chart-route removal and the current shared-view changes.
- The #693–#709 stack is combined locally. Source conflicts preserve routine runs, cancellation, instruction snapshots, and the current repository UI.
- Browser and background members share one access constructor with an explicit origin. Only browser sessions retain demo/sudo behavior.
- The imported memory-only invocation mode is removed. Wiki tools reuse the existing action mode until unified routine policies replace the legacy grants.
- PR agent callbacks recheck event-run authorization. Review attempts and usage records remain linked to their existing domain run.
- The server watcher compiles the combined sources. Application startup still fails against the existing configured database credentials.
- CI self-tests pass. After reconciliation, all 149 bot examples and all 23 incident-delivery examples pass.
- The model-budget regression first failed on a 200,133-character message. It now passes with an 8,192-character cap and full retained evidence.
- `ad6b16e93` integrates #568. `9efe7415a` integrates #571. Their branch heads and the #709 stack tip are ancestors of this branch.
- #692 runtime reconciliation resolves 26 conflicting paths, including modify/delete cases. Haskell owns guest lifetime and the existing later fix state machine. The old JavaScript VM proxy and fix adapter are removed.
- Shared workspace maintenance, frozen test files, Git isolation, outbox accounting, source comparisons, shell deadlines, and image detection are retained. Verified peer files stay in the guest instead of a second encoded worker payload.
- The native worker protocol suite passes all 14 cases. CI self-tests pass. The shared workspace and guest modules compile; the combined server and review suite are still being verified.
- #661 uses the existing scoped query tool for matching/total daily series. The same saved query observation and shared PNG renderer retain its evidence; a second planning LLM loop, evidence product, and Mermaid renderer are omitted. Its regression exercises matching counts, the denominator, project isolation, and private samples through the existing end-to-end handler test. Guest-dependent acceptance and final signoff remain outstanding.
- The impact-review watcher is focused on the full review suite. Guest-dependent cases require a prepared guest image; no replacement evidence is claimed.
- The pinned Pi/Gondolin packages install, and both SDK imports pass under the available Node 25 runtime.
- No PR was updated or closed. Full signoff and live acceptance remain outstanding.

### Canonical pull request

Keep draft [#619](https://github.com/monoscope-tech/monoscope/pull/619) as the single integration PR.
Its published tip is `0b5dc1b5c`; the local Slack implementation was rebased before this integration.
`git range-diff` matches the published plan and four implementation commits to `217eff334`, `4df0e0077`, `0b509d469`, `fe3cd1967`, and `a348ea767`.
The remaining published commit only formats that older tree.
The combined branch retains the later shared tools, current authority checks, routines, and their regressions.
A history-only merge records the published branch as integrated without restoring its older source tree.
This allows a later fast-forward update of #619 after review and local signoff, without force-pushing the rebased branch.
No remote PR state has changed.

All twenty selected incoming agent PR heads are now ancestors of this branch: the sixteen-PR stack, #692, #661, #571, and #568.
A fresh 31 October inventory during user-OAuth implementation still lists 28 open PRs. All twenty current selected heads were rechecked with `git merge-base --is-ancestor` and remain included.
The server watcher compiles the reconciled native runtime. Startup still fails against the existing configured production database credentials.
All five VM identity and cleanup regressions pass without a guest image. The complete impact-review suite is still running.

The 33-example review run completed with one count assertion to update for the new eighth evidence query and eleven image-dependent examples pending.
The corrected end-to-end publication, isolation, matching/total count, and recovery regression passes.
The worker suite still passes all fourteen cases after removing the obsolete file-tool mode from the protocol; builtin tool selection now has one policy field.
Prepared guest assets were subsequently found in the `monoscope-impact-worker-floor` worktree.
Ignored local asset directories reference those cached files for a focused VM rerun; no guest asset or image was added to Git.

### Native VM verification follow-up

The cached minimal image predates the localhost initialization change, so a fresh minimal guest was built locally with OrbStack.
The SDK rejects asset symlinks outside an image root. Local assets now use real files; the cached larger profile uses APFS clones.
The fresh minimal guest passes offline localhost resolution and shutdown after parent-pipe loss.
Interrupted verifier recovery passes for both cancellation and caller loss.
A missing SDK export first failed a VM-free bridge regression; the corrected bridge passes all fifteen worker examples.
The real SDK admission regression also passes after restoring shared artifact bounds at the admission boundary.
Admission setup hides its unchecked constructor and accepts artifacts through the same validator used by regression decoding.
These checks use macOS/libkrun. They do not claim provisioned Linux/KVM coverage or final signoff.

The real SDK fix-flow regression now passes, including frozen reproduction, candidate verification, independent review, retained artifacts, and cancellation.
The removed Node wrapper had initialized the private checks directory. `Outbox.withOutbox` now creates and verifies that directory once for all native callers.
The submit adapter extracts the existing summary field and uses the existing visual-summary validator.
Oversized patch files retain the explicit 256 KiB error; unreadable files remain a separate failure.
Full combined VM-suite verification remains in progress. No PR update, closure, signoff, or deployment has run.


### Combined-runtime regression fixes

The full VM suite ran all 33 examples, with no pending cases, and exposed three integration regressions.
All three now pass individually:

- Invalid dependency-archive configuration fails before a model call instead of silently dropping the archive.
- Shell deadlines preserve the workspace and distinguish real supervisor timeouts from a command returning exit code 137. Retained output includes the supervisor diagnostic.
- Production and research callers report the same host-measured workspace checkpoint facts.

The SDK exploration regression also verifies native tool guidance and screenshot bytes.
The host now appends its policy to the SDK prompt instead of replacing the SDK guidance.
The fifteen worker tests pass under Node 25. The complete VM suite also passes: 33 examples, zero failures, no pending cases (321.4 seconds).
These are local macOS/libkrun checks. Final reviews, signoff, publication, and Linux/KVM coverage remain outstanding.
