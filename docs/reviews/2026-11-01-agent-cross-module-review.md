# Unified agent design and verification review

Date: 1 November 2026. This record consolidates the branch's implementation reviews.
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

Follow-up passes extend explicit constructor coverage to native workflow progress, tool dispatch, receipt replay, fix verification, publication recovery, retained checkpoints, evidence audiences, Slack access and inspector states. The issue prompt reuses the existing typed conversation subject. Nested review controls preserve the running/unstarted/pending condition without listing a Cartesian product. The native/authority pass completes 74 examples; the final access, delivery, inspector and issue-page pass completes 532 examples with no failures. Compilation, scoped HLint, formatting and the manifest pass. All eight final files retain the same string-literal multisets, including interface text and theme tokens. Two further review rounds find no material change. Remaining wildcard arms handle open wire values, foreign containers, timeout/error results and unsupported boundary combinations. Final release verification and artifact refresh must use this later runtime snapshot.

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

No fix adds a parallel executor, weaker effect row, warning suppression, or authorization bypass.
The final Weeder pass also removed two lookup helpers orphaned by this branch's table-renderer replacement.

## Local release verification

All eleven configured checks pass for runtime snapshot `d5fa93873`, after migration consolidation and master PR #763. Native and KVM acceptance retain their recorded source scopes; this cleanup changes no agent runtime module.

| Check | Observed result | Scope and limits |
| --- | --- | --- |
| Canonical integration | 1,695 examples, zero failures, 25 pending VM cases; twelve successful shards. | Real-service attestation published. Pending VM cases have separate native evidence. |
| Native recovery group | 28 cases, zero failures; 590 recorded inputs unchanged. | Includes the early-suspension EOF-drain regression and missing-manifest guards. |
| Fresh Linux/KVM | 16 application cases, zero failures; 26 actual KVM guests observed. | Controlled model, GitHub and media interpreters. |
| Current-binary virtual-host loss | One case passes in 494.0081 seconds, including natural lease expiry. | Same binary before and after abrupt local Linux VM loss. Original run and attempts survive; completed replay adds no inference. |
| Complete doctests | 2,105 examples, zero errors or failures. | Actual supported target; later canonical gate also passes. |
| Browser suite | 172 pass, one skip. | Application browser tests; physical devices and screen readers remain unverified. |
| UI tests | 1,041 tests across 62 files pass. | Canonical local gate. |
| CLI | Build, package, signature, runtime version and portable linkage checks pass. | Preserves the CLI/shared package boundary. |
| Native worker image | The optional `impact-worker` target builds as Linux AMD64 on this Mac. Its non-root CLI, SDK imports, skills, private storage and runtime tools pass smoke checks. | Separate from the standard server image and real KVM acceptance. No worker deployment. |
| Unit tests | 315 examples, zero failures. | The parser's 22 SQL cases share one pinned golden corpus. |
| Other canonical checks | HLint, frontend, build and Weeder pass. | All eleven configured checks have matching attestations. |
| Review artifact | 104 steps pass at 320, 390, 768 and 1280 pixels, without overflow or browser errors. | 111 implementation excerpts match current source; six historical examples remain labeled. |

The literal local `ci-signoff` passes. The AMD64 image for `d5fa93873` is `sha256:56a726b6cc32ea05f87031c5b66be20f6c0a595a13d1265dd59fb343f71297e6`, built on this Mac through the local OrbStack builder. The integration lane also runs the PostgreSQL baseline-adoption regression; changes to its helper invalidate that check's fingerprint.
Canonical PR 619 is the review destination. Merge, production baseline adoption and deployment remain separate actions.

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
Master migration names and contents remain unchanged.
The user's later request explicitly permits consolidation of this branch's unreleased migration files.
Production inspection and any necessary transition must precede production migration changes.

Detailed iteration logs remain in Git history; private receipts remain under ignored `scripts/local/`.
Useful source and result indexes include:

- `unified-agent-final-reviewed-input-audit.json`: reviewed input coverage.
- `native-review-suspension-independent-review.json`: failing-first shutdown fix and repeated reviews.
- `native-review-suspension-integration-rerun-result.json`: canonical integration result.
- `linux-native-suspension-acceptance.json`: fresh KVM acceptance.
- `linux-hostloss-v4-independent-result.json`: current-binary virtual-host-loss proof.
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
Final local checks and the immutable image pass. The new Haskell template matches all 253 current migration checksums; full schema, ownership and privileges match the pre-consolidation template. An explicit rollback test also preserves the retained native-recovery fixture across eight tables while adopting the current checksums. Production schema and feature data remain unchanged; native baseline bookkeeping is prepared.
