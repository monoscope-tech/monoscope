# Unified agent design and verification review

Updated: 2 November 2026. This record consolidates the branch's implementation reviews.
The [main plan](../../plans/2026-10-30-unified-agent.md) retains the product contract and invariants.
The [interactive artifact](2026-10-30-unified-agent.html) explains the architecture and user experience.
[Slack setup](../slack/README.md) and [worker operations](../agent-operations.md) cover configuration.

## Design and review scope

| Boundary | Implementation and retained guarantee |
| --- | --- |
| Capabilities | AgentCatalog supplies discovery, schemas, policy classes and dispatch metadata. API, MCP and server CLI operations reuse domain implementations. |
| Authority | Current account, project, membership, policy and original role ceiling apply at admission, execution and disclosure. Routine adoption cannot silently increase authority. |
| Operations | One ledger retains immutable proposals, review decisions and receipts. Database mutations commit with receipts; uncertain external outcomes require reconciliation. |
| Memory | ProjectMemory retains revision checks and actual attempt attribution. Shared run links identify the writing run without casting attempt IDs. |
| Native execution | Haskell owns admission, reproduction, candidate verification, independent review and publication. The SDK child forwards model tools to the existing guest session. |
| Recovery | Paired checkpoints retain transcript, VM state, budgets and workflow. Missing manifests fail closed. Completed replay revalidates retained results without another model call. |
| Artifacts | Bounded reads, digests, private object keys and explicit manifests protect retained packages. Backend receipts do not grant application authority. |
| Slack | Signed receipts retain requester and destination. The adapter reuses shared runs, authority, checkpoints, progress and publication records. |
| Evidence | One typed audience covers model input, conversations, navigation, runs, artifacts and publication. Private evidence cannot become a shared mutation. |
| User interface | Run inspection, review controls, visualizations and artifacts remain inside the existing conversation. Existing theme tokens and widget rendering apply. |
| Packaging | The public CLI stays DB-free. Production images use this Mac's local OrbStack builder with Rosetta AMD64 emulation. |

Repeated hs-distill, hs-evasion-review and hs-lob-review passes covered 72 runtime inputs, including all 70 changed runtime Haskell files.
The final input audit accounts for later cleanup, Slack recovery and master test-prelude changes.
Material findings were fixed and reviewed again. No material finding remained at that recorded source snapshot.
The interface passes applied emil-design-eng and the accessibility, layout, writing, typography, color and UI checks.
No installed `/simplify` command was found. The final simplification pass was manual.
The completed consolidation passes the affected checks and final local release gate.

The sampled [PR review](https://github.com/monoscope-tech/monoscope/pull/619#issuecomment-6565176540) requests splitting the work. The user explicitly requested one integrated PR, so the architecture artifact remains the way to review its separate boundaries. Keep the dependency lockfile for reproducibility, the requested HTML artifact, and the runtime skill subset with its upstream revision, checksums and MIT licence. Ten vendored files match the recorded checksums. The separate product contract, migration runbook and operations reference have distinct purposes; overlapping review diaries were already removed.

Three catch-all matches in run authorization now name every supported claim or source constructor. A new variant therefore requires a compiler-checked decision about its run context and native access. Relative test paths already enter through the opaque `TestFiles` decoder; repository URLs and absolute local repository paths have a different contract. A second relative-path wrapper would add no boundary guarantee. The verifier's local references retain partial measurements for exception reporting; resource acquisition remains inside the existing lock, temporary-directory and sandbox scopes.
The authorization refinement compiles and passes 25 focused examples in 9.4124 seconds, including revoked permissions, retained role ceilings, stale worker leases and native job completion. Two scoped Haskell review rounds and the manual simplification pass find no further material change.

Follow-up passes extend explicit constructor coverage to native workflow progress, tool dispatch, receipt replay, fix verification, publication recovery, retained checkpoints, evidence audiences, Slack access and inspector states. The issue prompt reuses the existing typed conversation subject. Nested review controls preserve the running/unstarted/pending condition without listing a Cartesian product. The native/authority pass completes 74 examples; the final access, delivery, inspector and issue-page pass completes 532 examples with no failures. Compilation, scoped HLint, formatting and the manifest pass. All eight final files retain the same string-literal multisets, including interface text and theme tokens. Two further review rounds find no material change. Remaining wildcard arms handle open wire values, foreign containers, timeout/error results and unsupported boundary combinations. The later release gate passes at `1b8ea825e`; the artifact contains the corresponding explicit-state excerpts.

The shared host Git builder strips inherited credentials and configuration, disables hooks and filesystem monitors, and restricts protocols. Its integration test exercises local, anonymous GitHub and authenticated GitHub access. Test-patch generation also disables external diff and text conversion; baseline preparation creates a fresh repository instead of copying customer Git configuration. Slack access and refresh tokens use the shared authenticated encryption envelope. These checks support the specific boundaries; they do not establish a general sandbox escape proof.

The migration concern requires an explicit exception: production had applied 21 originals. The guarded transition preserves those records and adds a baseline record; it does not pretend they were unapplied. The schema comparisons and retained-data rollback test below cover fresh and observed production-shaped upgrades. Release changes remain in this integrated PR and retain their dedicated selftests and local image verification.

## Fixes retained by regression tests

| Area | Defect reproduced before the fix | Resulting behavior |
| --- | --- | --- |
| Atomic authority | Wiki mutation could follow an authority or Slack-link change before transaction entry. | The transaction rechecks the original requester, current authority, policy and audience before claiming the operation. |
| Model validation | The fake review interpreter ignored its validator; admission accepted empty rationale. | All review modes invoke the caller's validator. Admission uses opaque `MemoryText 1200`. |
| Guest timeout | Negative or overflowing SDK timeouts reached guest execution. | SDK schema and native `CommandTimeout` reject invalid transport values before conversion. |
| OTLP routing | Header-only exports returned success without storing records; a sibling resource could inherit authority. | Logs and traces share verified routing. Resource credentials retain precedence; uncredentialed siblings cannot inherit a project. |
| Widget safety | Stored widgets bypassed validation, column links accepted executable templates, and model IDs collided. | Stored and fresh widgets share ModelWidget validation, safe columns and indexed DOM IDs. Invalid evidence remains inspectable as escaped data. |
| Widget layout | Enlarged text could leave a zero-height chart. | Shared frames reserve text-relative minimum height in the conversation inspector. |
| Stat PNG | Different scalar results produced identical blank PNGs. | The existing converter formats missing data, zero and numeric values in both appearances. |
| Checkout recovery | Abrupt host loss left a truncated disposable Git cache. | Recovery repairs the cache and retains the original run and attempts. |
| Stream recovery | Lost acknowledgements, idle deadlines and a full journal could leave a stream open. | Saved task identities reconcile the original stream. Uncertain append/stop states close through the existing sender without another answer. |
| Bridge cleanup | A Close acknowledgement could precede EOF drain. | Close input and await process exit inside the existing cleanup deadline. |
| Early suspension | A knowledge callback could suspend before checkout without graceful child shutdown. | The outer exchange also uses the existing stop action; inner shutdown still precedes VM release. |
| CI shard gate | A passing test name containing “Interrupted” caused a false failure. | The gate checks complete shard summaries and process exits. Regression cases still reject real interruptions and shutdown failures. |
| Loading fixture | Aborting the editor chunk invoked stale-chunk recovery during a layout measurement. | Hold the request pending while measuring the skeleton; retain both layout assertions. The document-navigation probe and 24 focused repetitions pass. |

A later lost-store regression records a model call, returns no native context, and observes an unwanted executor entry (expected zero, actual one). The shared native PR/fix guard now checks the existing usage records before creating another attempt. A separate `NativeSessionLost` constructor retains recovery guidance instead of blind-retry text. The job lifecycle then passes with zero executor entries, one original attempt and no completed result. Its fake interpreter now captures typed fix context; the wiki retry requires the original attempt ID. The expanded watcher passes 593 examples with zero failures. The same committed runtime passes the selected 16 Linux/KVM cases with 26 guests observed. Final release verification is recorded separately in canonical PR 619 and its exact-input attestations.

No fix adds a parallel executor, weaker effect row, warning suppression, or authorization bypass.
The final Weeder pass also removed two lookup helpers orphaned by this branch's table-renderer replacement.

## Local release verification

All eleven configured checks pass for implementation snapshot `4ef2f337e`, including the lost-session guard, migration consolidation and master PRs #764 and #765. Native and KVM acceptance retain their recorded source scopes.

| Check | Observed result | Scope and limits |
| --- | --- | --- |
| Canonical integration | 1,589 examples, zero failures, 25 pending VM cases; twelve successful shards. | Real-service attestation published. Master #765 consolidates dashboard cases into twelve lifecycles. Pending VM cases have separate native evidence. |
| Historical native recovery group | 28 cases, zero failures. | Earlier acceptance includes the early-suspension EOF-drain regression and missing-manifest guards; current-source Linux acceptance is listed below. |
| Fresh Linux/KVM | 16 application cases, zero failures; 26 actual KVM guests observed. | Runtime `cd177d0ff`, including the lost-store guard; controlled model, GitHub and media interpreters. |
| Paired-checkpoint virtual-host loss | One case passes in 526.5371 seconds, including natural lease expiry. | Pinned `2d2c9d2c5` binary; all native runtime and worker inputs match `4ef2f337e`. All 17 retained files survive unchanged. The original run, attempts, regression and wiki action survive; completed replay adds no inference. |
| Loss before the first main checkpoint | One Linux case passes in 482.6013 seconds after abrupt host loss and natural lease expiry. | Runtime `cd177d0ff`; zero new inference, attempts, checks, wiki revisions or operations; no false completed result. Uncheckpointed edits are not claimed to resume. |
| Complete doctests | 2,105 examples, zero errors or failures. | Actual supported target; later canonical gate also passes. |
| Browser suite | 172 pass, one skip. | Application browser tests; physical devices and screen readers remain unverified. |
| UI tests | 1,041 tests across 62 files pass. | Canonical local gate. |
| CLI | Build, package, signature, runtime version and portable linkage checks pass. | Preserves the CLI/shared package boundary. |
| Native worker image | `monoscope-impact-worker:a93ad68cf` builds as Linux AMD64 on this Mac. Its non-root CLI, SDK imports, skills, private storage and runtime tools pass smoke checks. | Separate from the standard server image and real KVM acceptance. No worker deployment. |
| Unit tests | 315 examples, zero failures. | The parser's 22 SQL cases share one pinned golden corpus. |
| Other canonical checks | HLint, frontend, build and Weeder pass. | All eleven configured checks have matching attestations. |
| Review artifact | 104 steps pass at 320, 390, 768 and 1280 pixels, without overflow or browser errors. | 111 implementation excerpts match current source; six historical examples remain labeled. |

The literal local `ci-signoff` passes. The AMD64 image for `4ef2f337e` is `sha256:57a9f7feb3040cac0ca6310248e3bcf094f646183b2d87b0d6d257ca2ca67fb2`, built on this Mac through the local OrbStack builder. The integration lane also runs the PostgreSQL baseline-adoption regression; changes to its helper invalidate that check's fingerprint.
An earlier browser run timed out waiting for a dashboard drawer save; focused checks and the full rerun pass on unchanged code, concurrency and timeouts. The post-merge loading-fixture failure is fixed by holding the module pending instead of aborting it. The current complete browser run passes; failed runs produced no browser attestations.
Canonical PR 619 is the review destination. Production baseline adoption added the single ledger record described below. Merge and deployment remain unauthorized.

## Slack acceptance

Testing uses only the Monoscope workspace and its isolated “Monoscope Local QA” app.
Credentials remain in ignored owner-only files. No credential belongs in this report.

| Flow | Observed behavior |
| --- | --- |
| Requests and controls | Real DMs, private mentions, slash commands, message shortcuts, account linking, Stop and rename complete through signed callbacks. Duplicate event replay adds no work. |
| Autonomous write | An authorized request stores a wiki revision through the shared operation ledger and retains its writing run. |
| Charts | The real model produces a line chart and stat from 44 synthetic PostgreSQL rows, grouped 12/18/14. Slack displays both PNGs and the retained inspector exposes both widgets. |
| Schema-only replies | Schema discovery does not force an unnecessary visualization. Row results use the existing table rendering boundary. |
| Proactive lifecycle | Actual header-only protobuf HTTP ingestion stores three logs. Measured alert 3 and later recovery 0 produce two shared Observe analyses in one Slack thread. Repeated sweeps add no work. |
| Opt-out | After native pause, a fresh alert and recovery deliver normal notifications without another AI run or artifact operation. |
| Personal evidence | Actual consent, encrypted storage, canonical Slack Connect channel/thread reads and provider citations pass. Another project administrator receives 403 for personal retained evidence. |
| Channel evidence | Private and public canonical reads retain their exact channel audience. Owner/other-administrator/anonymous access returns 200/403/302. Replay preserves retained state. |
| Destination confinement | The controlled matrix passes 64 cases across channel types and command/thread destinations. |
| Streaming | Real requests retain one completed stream and one final answer. Lost-start acknowledgement recovery survives abrupt application loss and natural lease expiry without a diagnostic reload. |
| Throttling | A controlled one-shot HTTP 429 preserves cooldown and replay identity. This is not a provider-generated rate-limit observation. |

The OTLP fixture proves PostgreSQL ingestion and actual Slack delivery, not TimeFusion parity.
The chart fixture uses directly seeded PostgreSQL rows, not production telemetry.
Uncertain text append outcomes do not have a universal exactly-once guarantee.

## User experience and media

Run and artifact navigation preserve the conversation, draft and current theme.
Browser checks cover loading, empty, failure, cancellation, keyboard use, narrow widths and actual 200% text.
Native evidence appears inside the run inspector. Missing explanations have an explicit state.
Generated media acceptance uses the SDK, actual KVM and local object storage with controlled model/GitHub/media boundaries.
Chrome verifies a 36-second H264 video, captions, diagram and seven-asset package.
Authenticated range requests return 206; anonymous retained media access is denied.

The existing [review Page](https://chatgpt.com/space/page_9c75b6b56b0481918e666edd7a89a919) is updated in place.
Current excerpts use reachable source commits and corrected line anchors; all 111 implementation excerpts match the files.
Hosted source readback matches the repository artifact at its recorded checkpoint.
Hosted iframe interaction, native Slack new-tab clicks, VoiceOver and physical-device behavior remain unverified.
Virtual-host loss is not physical Mac power loss. Live GitHub publication is not claimed.

## Rebase and evidence records

The final actual rebase onto master `d842c8dc3` preserves all twenty selected PR heads and the published branch head.
Its tree exactly matches an independent merge: only the incoming golden-test helper, parser table, golden fixture and repository guidance change from the verified branch.
Canonical TestUtils/prelude imports retain the original regression bodies and stronger assertions.
A subsequent non-rewriting merge of master `d16b57f00` preserves published ancestry and adopts its five independent error-pattern lifecycles. The grouping lifecycle retains the activated enrichment routine, deterministic LLM interpreter, successful shared runs and saved analysis assertions. Its new snapshot retains the existing request identity and form-local HTMX behavior. The five lifecycles and seven notification cases pass twice: 12 examples, zero failures.
The following merge of master `603e33aa0` adopts its twelve dashboard lifecycles and shared helpers while preserving the branch's scalar PNG regression and actual handler owner. The dashboard, error-pattern, issue and notification group passes 31 examples repeatedly.
Master migration names and contents remain unchanged.
The user's later request explicitly permits consolidation of this branch's unreleased migration files.
Production inspection and any necessary transition must precede production migration changes.

Detailed iteration logs remain in Git history; private receipts remain under ignored `scripts/local/`.
Useful source and result indexes include:

- `unified-agent-final-reviewed-input-audit.json`: reviewed input coverage.
- `native-review-suspension-independent-review.json`: failing-first shutdown fix and repeated reviews.
- `native-review-suspension-integration-rerun-result.json`: canonical integration result.
- `linux-native-suspension-acceptance.json`: fresh KVM acceptance.
- `linux-hostloss-v6-acceptance.json`: current-native-runtime paired-checkpoint host-loss proof.
- `linux-uncheckpointed-v2-independent-result.json`: current missing-first-checkpoint host-loss proof.
- `native-worker-current-ci-result.json`: final pre-consolidation gate results.
- `native-worker-artifact-source-refresh-page.json`: exact hosted artifact readback.
- `master-762-integration-preview.json`: rebase, reconciliation and ancestry.

Receipt paths are local evidence, not portable release attestations.
Publish only after consolidation, affected verification, repeated reviews and final Mac-only image/signoff complete.

## File and migration consolidation

The user requested a necessity audit before final publication.
Ten overlapping review reports are combined here; superseded design plans and historical model-comparison exports are removed.
The native design and operations references replace prototype histories and obsolete commands.
The operations reference now requires Mac-only OrbStack/Rosetta image generation and removes the SSH-builder example.
The shared product contract, runtime skills, working comparison fixtures, regression coverage and interactive review artifact remain.
An unrelated generated design snapshot and accidental icon-script permission change are restored to master.

Forty-eight feature migrations become five dependency-ordered files; all master migration bytes remain unchanged.
The legacy Slack-evidence backfill stays independent so its existing repeat-upgrade regression remains meaningful.
Fresh and production-shaped local upgrades retain the original schema, and baseline safety tests preserve existing data/history.
The final focused watcher passes 69 migration, legacy-evidence, native-linkage, review-policy and issue-page golden examples in 20.3173 seconds.
The existing runner lifecycle now also proves that historical records survive file consolidation.
The isolated baseline test passes repeat adoption, the production shape, unrelated history/data retention and seven rejection scenarios.
A null-checksum case first fails, then passes with null-safe comparison. Formatting and the generated manifest pass.
Two scoped Haskell review passes retain the existing tests and effect boundaries; final simplification merges the runner guard into its existing lifecycle.
The fresh, original and production-shaped schemas have identical dumps after excluding migration bookkeeping.

Production inspection finds 21 original feature migrations with matching checksums and zero rows in the nine native feature tables.
The [production transition](../agent-migrations.md) adds one native baseline record and preserves all 273 earlier rows and timestamps. No schema DDL, feature-data mutation or application deployment occurs.
Final local checks and the immutable image pass. The new Haskell template matches all 253 current migration checksums; full schema, ownership and privileges match the pre-consolidation template. An explicit rollback test also preserves the retained native-recovery fixture across eight tables while adopting the current checksums. Production schema and feature data remain unchanged; native baseline bookkeeping is complete.

## Integrated pull requests

Canonical [PR 619](https://github.com/monoscope-tech/monoscope/pull/619) contains twenty selected incoming heads from the recorded 28-PR inventory.
The sixteen-PR impact stack ends at #709. All twenty selected heads remain ancestors after rebasing onto master `17e9e0128` (#772).
The superseded PRs are closed. Unrelated OIDC, tooltip, SARIF, hardware-visualization and protobuf work stays separate.

| PR | Contribution | Reconciliation |
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
| [#709](https://github.com/monoscope-tech/monoscope/pull/709) | Review evidence guidance and recorded model comparisons. | Retain runtime guidance and comparison commands. Historical research exports remain in Git history; they do not prove general accuracy. |
| #692 | Haskell VM ownership, fix orchestration and SDK host protocol. Reconciled with later stack receipts and artifacts; replaced JavaScript workflow paths removed. | Reuse the shared runtime and authority boundaries. |
| #661 | Matching and total production-evidence series through the existing scoped query tool and PNG renderer. No second model query planner. | Reuse the shared runtime and authority boundaries. |
| #571 | Slack threadless fallback, bounded recovery, diagnostics, reconnect guidance and prompts through the current receipt/run path. | Reuse the shared runtime and authority boundaries. |
| #568 | Five-minute infrastructure-health work budget and cancellation coverage. | Reuse the shared runtime and authority boundaries. |

The runtime retains Haskell guest ownership, typed review/fix proofs, frozen regressions, private receipts and current member authority.
Slack activation grants no publication or deployment authority. Browser-only demo/sudo behavior stays outside background actors.
Original requester, evidence audience, routine policy and operation identity survive retries.
The unrelated icon-script rewrite from #692 remains excluded.

## Rebase and reduction on 2 November

The first rebase adopts master #769; a second adopts #772 after master advances during verification. Backup refs retain the published head and both intermediate trees.
This supersedes the earlier ancestry-preserving merges: publication requires a lease against the old branch head.
Master's lifecycle suites own the shared baseline behavior; unique branch regressions remain alongside them.

| Consolidation | Retained owner |
| --- | --- |
| Duplicate incident suite | Monitoring lifecycle suite, including autonomous analysis, standalone delivery and signed title linkage |
| Duplicate bot fixtures and recorded-query seeds | Canonical test prelude, shared bot helpers and bot lifecycle suite |
| Duplicate Slack, routine and conversation tests | Bot lifecycle suite; authority and recovery matrices remain in Agentic/Workflows |
| Separate native design note | Operations guide, including workflow ownership and checkpoint recovery |
| Separate integration and superseded UX notes | This review and the existing visual artifact |
| Unrelated import-only edits | Restore master imports to reduce changed-file noise |

The worker lockfile pins SDK dependencies. Bundled skills and their license are runtime inputs.
Native fixtures exercise distinct protocol boundaries; the interactive artifact is the requested review surface.
Small runtime modules retain their capability, persistence or interpreter boundaries; concatenating them would save little code.
Production runtime and applied migration bytes are unchanged by this reduction.

The affected watcher passes 693 examples with zero failures in 628.5473 seconds, including native SDK recovery.
After helper simplification, 545 focused examples pass in 206.9954 seconds. The conversation snapshot detects the wrong page target, then passes after restoration.
The local CI self-test passes with the explicit OrbStack context and builder. An invocation without the builder correctly fails its guard.
Repeated Haskell reviews and final manual simplification retain the effect, authority, budget and client-side boundaries; Weeder also identifies three orphan test helpers, which are removed.
Exact-input release results and image provenance are recorded in PR 619; earlier results above retain their stated source scopes.

| Cleanup against master #769, before the second rebase | Before cleanup | After cleanup |
| --- | ---: | ---: |
| Changed files | 193 | 176 |
| Added lines | 39,765 | 37,137 |
| Deleted lines | 3,531 | 4,087 |

The older +36,112 display used a different master revision. These figures use one base so the reduction is comparable.

Master #772 consolidates ingestion tests. Its new lifecycle suite retains the branch's HTTP header-only, sibling-isolation and credential-precedence assertions in one example.
All 15 ingestion lifecycles pass. The branch's typed project-routing implementation remains unchanged; master now includes the corresponding header-only fix.
The second rebase retains every selected head and removes the superseded ingestion spec. Native runtime inputs remain unchanged from the prior acceptance scope.
