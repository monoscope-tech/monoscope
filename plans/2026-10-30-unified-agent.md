# Unified Monoscope agent

Status: implementation in progress, 1 November 2026. This plan controls the complete branch scope; the checklist below does not replace the product contract.

Consolidate Slack, web, scheduled, and event work around one capability catalog, authority evaluator, executor, operation ledger, conversation, and artifact experience. Newly enabled background routines act autonomously within their member's current authority. Optional selected review must not become a mandatory permission loop.

Prefer small algebraic interfaces, opaque proofs, precise effect rows, established libraries, and internal eDSLs that reduce total maintained code. Keep invalid states out of constructors. Remove replaced implementations in the same slice. Keep fewer integrated pages; inspect work through the existing conversation and run panel.

The [visual review](https://chatgpt.com/space/page_9c75b6b56b0481918e666edd7a89a919) is the review destination. Its [repository artifact](../docs/reviews/2026-10-30-unified-agent.html) contains the same diagrams, walkthroughs, code excerpts, and roadmap.

## Current implementation and remaining work

All twenty selected incoming PR heads remain included. A fresh inventory found 28 open PRs with no selected head missing. The [integration plan](2026-10-30-agent-pr-integration.md) defines that selection. Published PR 619 and remote master `3c34f3c83` are ancestors of this branch. The latest local changes have not been pushed or deployed.

| Requirement | Current implementation | Work still required |
| --- | --- | --- |
| Shared capabilities | One typed API/agent descriptor governs discovery, policy, schema, dispatch, and bounded model responses. CLI server operations use the public API; client-only operations remain local. | Final whole-branch catalog and package-boundary verification. |
| Autonomous authority | Observe, Autonomous, and selected review share the current-role evaluator, immutable actor grants, original role ceilings, and monotonic restrictions. New routine activation defaults to Autonomous; adopted legacy read-only work stays Observe. | Final whole-branch role, revocation, pause, stop, and legacy-adoption regression run. |
| Recoverable operations | One ledger retains immutable proposals, decisions, action identities, receipts, and uncertain outcomes. Domain mutations and receipts commit together where possible; external actions reconcile before retry. | Whole-branch interruption coverage and arbitrary native host-loss acceptance; no universal exactly-once claim. |
| Requested conversations | Web, issue chat, slash commands, Slack events, schedules, enrichment, and incidents queue existing jobs and use shared execution/completion. Origins, request IDs, titles, and replies survive retry. | Private-channel mention and autonomous wiki write verified with the isolated QA app; complete the remaining cross-entry-point regressions. |
| Shared memory | Wiki read/search/write use versioned ProjectMemory, the common capability policy, transactional edit recipe, and operation ledger. Native attempts retain their actual IDs and revisions link to the shared writing run. | Final cross-entry-point and author-attribution regression run. |
| Optional selected review | Web and signed Slack controls use the same decision state and continuation queue. Decisions, expiry, revocation, pause, deletion, and terminal cleanup preserve the original request. | Final whole-branch continuation acceptance. |
| Native review and fix integration | PR analysis, fixes, admission, and independent review share retained SDK/VM sessions and artifact validation. Actual whole-application/SDK/VM loss proves both fail-closed uncheckpointed retention and successful paired-checkpoint continuation on the original run and attempts. | Linux/KVM SDK, CLI, and nine selected application cases pass. Physical-host loss and final regressions remain open. |
| Slack streaming | Persisted start/append/stop state and task-ID reconciliation reuse the existing progress publication and refresh job. Five focused recovery cases and all 486 bot examples pass. Streams close before the provider deadline or journal rollover, then use the existing snapshot refresh. | Complete live interruption, rate limits, native Stop-button interaction, and artifact-link acceptance, plus whole-branch review and final release gates. A real private-channel request completed one stream/final reply; a separate locally signed stop cancelled its active run without a late answer. |
| Proactive work | Channel opt-in, incident analysis, measured recovery/resolution reports, and authorized remediation reuse event routines, the common worker, and incident outbox. Analysis defaults to Observe. Native QA activation, repeat activation, private confirmations, and pause now pass through the registered command name. | Actual header-only OTLP ingestion now drives measured alert and recovery, two successful shared Observe analyses, retained report pages, one conversation artifact receipt, and the same real Slack thread. Synthetic data-gap acceptance remains separate. Fresh telemetry alert/recovery after native opt-out delivers normal notifications without an AI run. Final authority regressions and release gates remain. |
| Slack evidence | Requester-bound OAuth, authenticated encryption, refresh/revocation, remote MCP reads, typed audiences, and retained provenance use shared systems. Provider writes stay excluded. | Real requester consent, encrypted grant storage, canonical discovery/thread read, retained channel audience, and replay pass. The canonical provider citation now opens the correct source thread from the published Slack reply. The current runtime, including the credential-failure follow-up, also passes a fresh live request and signed replay. Complete Slack Connect and remaining private/public audience acceptance. |
| Integrated experience | Conversations, run inspectors, review controls, and artifacts share the existing page. Ten Chromium cases cover loading, empty, failure, keyboard, light/dark and narrow-width states, including measured 200% text. Native fix media and package controls now appear inside the run inspector; eight visual states, written artifact navigation, and missing explanations pass. | Hosted visualization interaction, physical-device and VoiceOver acceptance remain unverified. A separate native SDK job now verifies generated video playback, captions, real local object storage, protected package downloads, and anonymous denial in Chrome; physical-device and live-publication acceptance remain separate. |
| Publication | Work stays on the PR branch; existing local verification checkpoints are recorded below. | Repeated whole-branch reviews, final simplification, local CI/image signoff, and canonical PR 619 update. Repeat doctests if their inputs change. |

Streaming, Slack evidence, and proactive work remain part of this full plan even though their rollout boundaries are separate. A narrower autonomous-launch boundary does not establish completion of the plan.

## Latest verified checkpoint

- Fresh Linux virtual-host crash and recovery now passes with the same corrected binary before shutdown and after completion. The crash again truncates the disposable Git cache. After natural lease expiry, the application repairs it and preserves the original shared run, attempts, regression, wiki revision, and operation. Completed replay adds no inference. Four recovery guests use real KVM. The case passes in 565.6467 seconds, including the lease wait; all 22 recorded native runtime inputs and 28 worker inputs match. The VM is stopped after cleanup. See [checkout recovery](../docs/reviews/2026-11-01-checkout-and-tool-schemas.md#fresh-crash-with-the-corrected-binary). The external local test database survives. Physical Mac power loss and live GitHub publication are not claimed.

- Incident-delivery and public API follow-up passes all 118 examples in 31.0630 seconds, including paused/revoked report authority, ambiguous delivery reconciliation, and actual MCP route calls. The CI runner self-test also passes all 23 release tests and its capability/fingerprint assertions. These checks do not replace final local release signoff. See the [cross-module review](../docs/reviews/2026-11-01-agent-cross-module-review.md#incident-delivery-and-api-follow-up).

- Review artifact refreshed in place: the existing Page now includes measured incident recovery and the real QA thread link. Its updated visualization preserves all styles and scripts. Saved readback at sequence 73 preserves the previous 18 blocks and adds four native explanation blocks. The [repository artifact](../docs/reviews/2026-10-30-unified-agent.html) matches the 238,501-byte hosted visualization. Hosted interaction, physical-device and VoiceOver checks remain open.

- Real telemetry-fed proactive acceptance: fresh header-only protobuf HTTP stores three logs; the production checker measures 3 and later 0 after the two-minute window expires. One real Slack incident thread retains the initial and closing analyses from two successful shared Observe runs. Both authenticated run pages return HTTP 200. Repeated sweeps add no work. One common-ledger ConversationWrite saves the investigation document; no project mutation occurs. The [Slack audit](../docs/reviews/2026-11-01-slack-live-configuration.md) records the actual run/episode IDs, corrected fixture configuration, source overlay, and PG-only scope. After native opt-out, a fresh telemetry alert and its measured recovery send normal notifications without another investigation or artifact operation. The channel is left paused.

- Real proactive acceptance exposed and fixed OTLP routing: header-only log exports previously returned success without storing records. Logs and traces now share explicit verified header/resource routing; a resource cannot inherit a sibling’s project. Both failures are reproduced first, all six focused authorization cases pass, and the updated isolated QA service stores all three logs submitted by actual header-only protobuf HTTP. Runtime source grows by four lines. The [routing review](../docs/reviews/2026-11-01-otlp-routing.md) records scope and evidence. The subsequent accepted telemetry-to-Slack lifecycle is recorded above; whole-branch release gates remain.

- Native channel controls: the existing slash dispatcher now applies built-in controls to registered agent command names and uses that name in its private disconnect hint. Both configured-name lifecycle cases first fail, then pass; all 511 canonical bot examples pass in 189.3967 seconds with unchanged recorded inputs. Actual `/monoscope-qa incidents on`, repeated activation, and `incidents off` show private confirmations, retain one Observe routine, and add no investigation, operation, publication, or queued job. The control conversation returns authenticated HTTP 200. The [Slack audit](../docs/reviews/2026-11-01-slack-live-configuration.md) records the runtime, role/evidence scope, preserved paused QA fixture, and remaining telemetry-fed acceptance.

- Generated native media now passes a real browser check through protected application routes and local MinIO.
  Chrome plays the 36-second explanation with four caption cues and downloads a package whose seven media digests match.
  Desktop light and mobile dark inspectors fit, preserve the conversation and draft, and reject anonymous media/package requests.
  The isolated SDK application case passes with controlled model responses; unchanged snapshot assets supply the private harness's missing static layer.
  The [interface review](2026-10-29-routine-interface-review.md) records the harness failures, exact scope, receipts, and remaining physical-device/live-publication limits.
  The review Page preserves prior content and adds the actual generated-media capture at sequence 70.

- Bounded Slack explanations: one private, closed-limit splitter uses the existing text library for sections and messages. Long plain and chart answers retain all text in order through the existing reply batch; whitespace boundaries, astral emoji, unbroken tokens, blank answers, retained history, and replay pass in 24 cases. All 509 canonical bot examples pass in 180.3071 seconds with unchanged recorded inputs. A real provider-only QA probe accepts the maximum section sizes and is removed afterward. The [widget review](../docs/reviews/2026-11-01-slack-widgets.md) records the failing-first cases, repeated scoped reviews, provider scope, and final doctest/release requirements.

- Paired native recovery: after a real SDK/VM/workflow checkpoint, the worker resumes active work and then loses its whole application, SDK, and VM to SIGKILL. Natural lease expiry permits the original run to finish. Both original attempts and the regression receipt survive, one independent peer brings the attempt total to three, and there is still only one wiki revision and operation receipt. Completed-job replay adds no inference. The application case and independent observer pass with unchanged recorded runtime/image/harness inputs. The [native audit](../docs/reviews/2026-11-01-native-continuation-audit.md) records the 563.7112-second case, including lease wait, and its physical-host/Linux limits.

- Native lease construction: the hidden `Lease` no longer exposes generic JSON or row construction. The existing claim SQL returns a tuple and constructs the private proof at that boundary; runtime line count is unchanged. Failing-first constructor rejection, all 2,098 doctests, and both native application lifecycles pass. The two application cases take 26.4531 seconds and retain their recorded source hashes. Repeated scoped distillation, evasion, and locality reviews, pinned formatting, HLint, and whitespace checks pass. The [native audit](../docs/reviews/2026-11-01-native-continuation-audit.md) records the intermediate test-oracle correction and release limits.

- Uncheckpointed native loss: an actual SIGKILL removes the application, SDK, and VM after a committed wiki write, admitted regression, and successful guest source edit. After restart and natural expiry of the original ten-minute lease, the missing candidate checkpoint stops inference. The original run, attempts, check, operation receipt, wiki revision, and fourteen retained files remain intact; no result is fabricated. The automatic-completion oracle fails, while the independent fail-closed observer passes. This proves the specified missing-manifest safety behavior, not successful recovery. The [native audit](../docs/reviews/2026-11-01-native-continuation-audit.md) records scope, setup failures, and private evidence. The separate persistent Slack fixture is restored and its signed public callback again returns HTTP 200.

- Slack row and query consolidation: `c5e27c6c2` removes the separate legacy-query branch and uses the existing widget shape and shared column formatter for row results. `f9370f71d` parses primary model queries into an opaque KQL type; invalid optional prose no longer creates a query-error publication beside a useful answer. The nineteen-case queued-command matrix, all 504 canonical bot examples, and all 2,089 doctests pass. The unchanged native schema request now calls `get_schema` once and publishes exactly one explanatory reply, with no widget or external operation. Completed-job replay changes no usage, journal, operation, or publication counts. The [widget review](../docs/reviews/2026-11-01-slack-widgets.md) records source scope and evidence. The persistent local QA fixture retains its database and historical conversation across watcher reloads. Its expired temporary tunnel was replaced, all QA callback URLs were read back, and the new signed public challenge returns HTTP 200. Normal reload and logical restore do not establish arbitrary host-loss acceptance; that requirement remains open.

- Native fix inspector: a failing browser case reproduced missing evidence inside the conversation. The existing protected evidence view now serves the run inspector and standalone page. A second regression covers missing explanations without hiding run details. All ten permanent conversation cases pass in 25.6 seconds; the native publication/access lifecycle passes after removing the temporary adapter. The [interface review](2026-10-29-routine-interface-review.md) records the six-domain scope, seventeen captures, source hashes, and media-transport limits.

- Shared operation admission and routing: `4b9b16924` closes the claim-time authority race using the existing transactional evaluator; `ada067673` keeps compound instructions out of the saved-report shortcut. The unchanged real QA request now invokes `send_to_slack`, completes one Autonomous/NoReview operation, posts once, and retains the exact Slack acknowledgement under the linked actor and run. It requires no personal search grant. Signed replay leaves one run, eight journal entries, one requested final publication, and one operation unchanged. The progress stream is closed; authenticated conversation access returns 200 and anonymous access redirects. The final combined bot/GitSync run passes all 491 examples in 179.6252 seconds with unchanged recorded inputs, and all 2,076 doctests pass. Live evidence is `scripts/local/slack-admission-live-acceptance.json`; the committed QA snapshot is `ada067673`. This is scoped acceptance, not final release signoff.

- Repository credential input: an empty new-connection token first reproduced an uncaught `EmptySecret` exception. New connections now use the existing `mkGitConn` boundary and its normal form error. Edits can still retain the saved token. The extended handler lifecycle requires visible error feedback, no saved connection or queued work for blank tokens, and successful creation and editing. All five settings cases and the full bot suite pass together: 491 examples in 179.6252 seconds. Logs and unchanged source hashes use `scripts/local/git-sync-missing-token-*`. Scoped distillation reuses the existing constructor and copy; the repeated evasion and locality passes find no further material issue in this fix.

- Review interpreter contract: `45817fea9` makes fake repository reviews honor the caller's validator and shares admission rationale bounds with production. Both defects first failed in regressions. All 50 native review cases and three Slack workflow cases then pass: 53 examples in 347.5340 seconds. The supported doctest run passes all 2,076 examples. The [scoped audit](../docs/reviews/2026-11-01-review-interpreter-contract.md) records source hashes, the earlier mixed failure, and the remaining release boundaries. These results are local checkpoints, not final signoff.

- Slack source citations: `b8d22c46d` supplies the validated provider permalink; `7e4229eb0` discards evidence on credential rejection while preserving successful reads when optional metadata is unavailable. Both failure paths were reproduced first. At that live checkpoint, canonical runtime sources and migrations matched the QA snapshot. Fresh run `c2753e4c-2f85-4cbb-ba26-4bdf3fcdabba` completes the real requester-bound MCP read and publishes the verified source once. Signed replay leaves all counts unchanged; authenticated conversation access returns 200 and anonymous access redirects. All 479 bot examples, 41 incident-delivery examples, and 2,068 doctests pass with unchanged recorded Haskell inputs. These are scoped checkpoints, not release attestations. See the [Slack audit](../docs/reviews/2026-11-01-slack-live-configuration.md).
- Proactive Slack: an explicitly synthetic alert produces one Observe analysis; a measurement gap leaves the episode active without another analysis; explicit synthetic recovery produces one closing analysis in the same real Slack thread. Repeated sweeps add no work. After opt-out, a second alert/recovery still delivers normal notifications but starts no new investigation. Both run links return HTTP 200 with their reports. This exercises existing incident recording, the production worker/model/project-tool adapter, and real Slack delivery. It does not establish telemetry ingestion or native slash-command delivery. The private receipt is `scripts/local/slack-proactive-live-acceptance.json`; source hashes are `scripts/local/slack-proactive-live-source.json`.

- Native eligibility and interface: migration `0282_issue_fix_project_eligibility.sql` closes retained media, listing, publication, and page access for inactive projects and free-plan non-owner members. The extended native lifecycle first reproduced both page HTTP 200 responses, then passed with no retained reads, publication `AccessRevoked`, and page HTTP 404. Eligible owner access and restored plan access still pass. Eight native-page browser states also pass with actual handlers and generated fixture media; the combined watcher run passes 33 examples in 34.6194 seconds. The temporary browser adapter is removed. See the [native audit](../docs/reviews/2026-11-01-native-continuation-audit.md) and [interface review](2026-10-29-routine-interface-review.md). Full native regression and release checks remain required.

- Conversation interface recheck: all nine permanent browser cases pass in 24.0 seconds against isolated real handlers and a temporary database. The new case covers run and artifact details at 320px/1280px in both themes. Sixteen font measurements confirm actual 200% text; keyboard focus, close-control bounds, horizontal fit, and draft restoration pass. The same watcher run includes fourteen Slack audience cases, for fifteen Hspec examples with zero failures. The temporary adapter is removed. Evidence is `scripts/local/interface-text-review-green.log` and `scripts/local/interface-text-review-source.json`; the [interface review](2026-10-29-routine-interface-review.md) records the six-domain scope and remaining device/viewer boundaries. This is scoped browser acceptance, not whole-branch release signoff.
- Native integration: all 50 cases pass in 351.1398 seconds on the final native resource wrapper, including the missing-manifest fix and abrupt SDK/VM-bridge loss. The saved full-run log is `/tmp/native-runtime-group-final-50-green.log`. A failing-first regression covers selected review, provider interruption, and accepted-result retention: both context loading and execution reject orphaned retained files without inference or transcript changes, then resume after manifest repair. The shared loader adds two lines; the existing SDK/VM fixture covers all three states. Logs are `/tmp/native-missing-manifest-red.log`, `/tmp/native-missing-manifest-three-green.log`, and `/tmp/native-missing-manifest-49-green.log`; these are session evidence, not release attestations.
- Runtime static scan: HLint reports zero hints across all 86 changed `.hs` files under `src/` and `shared/src/` relative to local `master`. The diff scan finds no added warning suppression, `unsafeEff`, `unsafePerformIO`, `unsafeCoerce`, `fromJust`, or `undefined`. This does not establish whole-branch semantic or interface review; tests, CLI modules, and generated sources are outside this scan. The JSON result is `/tmp/unified-agent-runtime-hlint.json`.
- SDK worker: all 19 tests pass on Node 25.2.1.
- Native process loss: SIGKILL of both the SDK worker and its VM-owning bridge passes through the real application job after a committed wiki write. The failing-first regression exposed an orphaned VM runner; the common process wrapper now creates and cleans its private process group. The runner exits, the failed close remains attributable as one cleanup failure, and retry from the earlier paired checkpoint retains one wiki revision, its original writing run, original evidence/window/notes, and one publication. Six model requests include one unreported call with unknown cost. The final bracket passes all seven continuation cases in 29.9992 seconds, five allocation lifecycle cases in 5.5057 seconds, and both SDK caller-loss/cancellation cases in 10.0647 seconds. The final complete 50-case run also passes in 351.1398 seconds after the refactor to release each process once. The failing and final focused logs are `/tmp/native-runtime-orphan-red.log` and `/tmp/native-runtime-group-final-*-green.log`. Whole-machine and uncheckpointed-work recovery remain unverified.
- Doctests: `make test-doctests` passes all 2,048 examples with zero errors or failures after the renderer and shared-runtime changes (`/tmp/native-runtime-group-doctests.log`). The affected integration component compiles with its direct OpenAI dependency from `c3a7cd566`.
- Mac CLI packaging: after `make cli-build`, the existing package script passes signing, strict signature verification, dependency linkage, and `--version` on a temporary copy of the rebuilt CLI binary (`/tmp/native-runtime-group-cli-build.log` and `/tmp/native-runtime-group-cli-package.log`). It replaces the development binary's Homebrew zlib path with `/usr/lib/libz.1.dylib`. This is a packaging-path check, not a fresh release build or Linux attestation.
- Linux/KVM prerequisite: the local OrbStack daemon is Linux/aarch64, uses this Mac’s Unix socket, and cannot attach `/dev/kvm` because that device does not exist. Its current CI runner intentionally exposes no KVM device. This probe is not Linux native acceptance; that still requires a provisioned host. Evidence is `/tmp/unified-agent-orbstack-kvm-probe.log`.
- Server: the watcher compiles all 167 modules. Its configured production database rejects authentication, so compilation does not establish live application acceptance.
- Browser artifact: all 101 steps in twelve flows pass at widths 320/390/768/1280, including expanded source. All 113 source panels match their cited Git revisions. Checked text/surface pairs have at least 7.09:1 contrast; no horizontal overflow or browser errors were observed. Keyboard selection retains focus. Local Chrome inspected the uploaded HTML; the hosted viewer and screen-reader interaction were not tested.
- Hosted review: the existing Page visualization was replaced and read back at sequence 55. Its HTML is 185,066 bytes, SHA-256 `d25874281e71bfe3081308ed82e3230876271bedbfe454dd62aa01bb9f776c34`. Readiness now includes the 50 native cases, eight conversation browser cases, and 39 Slack-evidence cases. All 101 steps in twelve walkthroughs pass at widths 320/390/768/1280, with keyboard focus, expanded sources, no horizontal overflow, and no browser errors. Every source panel and the walkthrough script remain unchanged. The Page title and surrounding prose remain unchanged. The verification counts now align within each row. Local Chrome checked the artifact HTML; hosted viewer and screen-reader acceptance remain open. Saved checks are `scripts/local/unified-artifact-refresh-checks.json`.
- Shared API and incident acceptance: all 76 API examples pass in 20.7662 seconds; all 41 incident-delivery examples pass in 13.7290 seconds. These runs exercise the current combined implementation, including real HTTP catalog dispatch and proactive incident replay, authority withdrawal, and ambiguous-delivery reconciliation. Saved logs are `scripts/local/unified-agent-api-76-green.log` and `scripts/local/unified-agent-incidents-41-green.log`.
- Conversation browser acceptance: `f79e95cd7` fixes artifact headings that collided with conversation headings and lost keyboard focus after navigation. All six permanent browser cases pass in 23.6 seconds against an isolated live server and database. They include local/external link behavior, keyboard navigation and Escape, draft retention, twelve terminal-state combinations, lifecycle, action history, and access revocation. The [interface review](2026-10-29-routine-interface-review.md) records both failing-first regressions and scoped skill reviews. Sixteen captures and the successful log are under ignored `scripts/local/artifact-contents-review/` and `scripts/local/artifact-contents-green.log`. Physical assistive-device acceptance remains unverified; the follow-up below covers delayed requests and failures.
- Source and CI checks: the pinned formatter passes all 62 changed Haskell source files. HLint found five expression simplifications; `0ad8a4d8f` applies them, and the affected files pass the follow-up check. The missing-manifest interpreter also passes formatting and HLint. `make ci-selftest` passes, including all twelve release-decision tests with a local Git remote and fake registry. This is not full CI, an image build, or an attestation. Its saved log is `scripts/local/unified-agent-ci-selftest.log`.
- Inspector request recovery: all seven permanent browser cases pass in 23.1 seconds after adding a full-opacity toast assertion. Server failures, dropped connections, delayed retries, and cancelled confirmations preserve drafts and release loading feedback. The failing-first regression is saved in `scripts/local/inspector-requests-red.log`; the final pass is `scripts/local/inspector-requests-final-green.log`. Final captures and source hashes are under `scripts/local/inspector-request-review/`. The temporary live-server adapter has been removed. The scoped [interface review](2026-10-29-routine-interface-review.md) records the changes, review decisions, and remaining limitations.
- Selected review browser acceptance: all eight permanent cases pass in 22.9 seconds. The new case covers approval in the desktop light inspector and decline in the mobile dark inspector, rejects an administrator-only decision after a role downgrade with HTTP 403, retains the pending decision, and preserves composer focus, draft, and conversation URL. It uses the actual HTTP review handler; it does not execute the resumed model. Captures/source hashes are in `scripts/local/selected-review-browser/`, with log `scripts/local/selected-review-browser-green.log`. No product code changed; the temporary adapter is removed.
- Review decision failure feedback: a failing-first browser regression reproduces a silent rejection from a stale review form. The form now uses the existing error toast for HTTP failures and dropped connections, with copy that does not assume an uncertain decision failed to save. All eight browser cases pass in 22.6 seconds, including enabled controls after failure and successful retry. Logs are `scripts/local/review-decision-feedback-{red,green}.log`; captures and source hashes are under `scripts/local/review-decision-feedback/`. No authorization or receipt guard changed; the temporary adapter is removed.
- Slack evidence review: all 39 focused examples pass in 12.8914 seconds on the current branch. The source review covers requester grants, destination confinement, response identity, schema changes, rejected catalogs, session release, and provider cooldowns. It finds no further material issue in those paths. The log is `scripts/local/slack-evidence-review-39-green.log`. These test interpreters do not establish live MCP eligibility or current provider schemas. The streaming section now records the provider's total text limit, fixed timeout, and destination constraints.
- Atomic write authority: a failing-first regression reproduced wiki mutations after authority changed between the cached check and transaction entry. The shared atomic path now rechecks run authority, selected-review policy, and evidence scope inside its transaction. It rolls back the claim on rejection and uses the existing stopped-run path. All 19 operation examples pass in 6.2257 seconds, including five role/policy/audience regressions. Logs are `scripts/local/atomic-wiki-authority-five-red.log` and `scripts/local/atomic-wiki-authority-operations-19-green.log`. The [native audit](../docs/reviews/2026-11-01-native-continuation-audit.md) records the scope. All 61 selected-review examples pass in 17.0456 seconds. Native application-job recovery passes in 21.5561 seconds, and the PR checkout/wiki harness passes in 6.5214 seconds. Their logs use the same `scripts/local/atomic-wiki-authority-` prefix. This is focused acceptance, not whole-branch signoff.
- Streaming review in progress: the uncommitted application stream path passes four focused cases in 15.9038 seconds. They cover uncertain start, append, and stop recovery plus an idle deadline. The idle case first failed in 12.5486 seconds: an unchanged snapshot skipped the timeout check. The progress loop now polls active and uncertain streams even without new activity; the isolated case passes in 4.5194 seconds. Logs are `scripts/local/slack-stream-idle-{red,green}.log` and `scripts/local/slack-stream-recovery-four-green.log`. This is fixture-backed application acceptance, not live acceptance of the new application stream path.
- Streaming type boundary: failing-first doctests reproduced generic decoding of an invalid timestamp and a frame with an empty title. The two private types now omit `Generic`; the explicit frame encoder preserves the existing `title` and `tasks` JSON fields. All 1,040 focused doctest examples pass, including constructor-bypass rejection and fixed JSON fixtures. Logs are `scripts/local/slack-stream-refined-types-{red,green}.log`. All four stream-recovery cases pass again in 15.6021 seconds; the log is `scripts/local/slack-stream-refined-recovery-four-green.log`. HLint reports no hints across the four changed streaming runtime modules. Broader bot regressions and the complete streaming review remain in progress.
- Broader streaming checkpoint: all 459 bot examples pass in 162.2230 seconds, including the four stream-recovery cases and the scoped artifact-heading expectation. The complete log is `scripts/local/slack-stream-bots-459-green.log`; `scripts/local/slack-stream-bots-459-source.json` pins the tested source hashes. Subsequent authority-boundary work requires fresh focused verification. This is not final whole-branch signoff or live streaming acceptance. Earlier 39-case Slack-evidence acceptance retains its recorded scope.
- Atomic Slack-identity checkpoint: a failing-first regression reproduced a wiki write after the requester unlinked immediately before the claim. Commit `cf2a56c6c` carries the original access through retries and reuses the identity resolver inside the mutation transaction. All 20 operation cases pass in 6.3377 seconds; all 56 cases selected by `agentReview_` pass in 16.4971 seconds. Native application recovery passes in 22.5406 seconds and the PR checkout/wiki harness in 5.0798 seconds. The broader bot suite then passes all 460 examples in 173.5638 seconds, with no input hash changes during the run. Its log is `scripts/local/atomic-slack-identity-bots-460-green.log`; source hashes are in `scripts/local/atomic-slack-identity-bots-run-source.json`. The [native audit](../docs/reviews/2026-11-01-native-continuation-audit.md) records the scoped reviews and verification limits. Whole-branch signoff and live streaming acceptance remain open.
- Streaming review follow-up: the single-caller reconciliation helper now captures its publication context beside its caller. All four recovery cases pass in 16.0106 seconds; the broader suite then passes all 460 cases in 174.8782 seconds with unchanged source hashes. A subsequent failing-first application regression shows the stream staying open after its journal reaches the fifty-event window. It now closes with the last acknowledged frame and refreshes the message with the bounded current snapshot and its existing “recent activity only” notice. The new case fails before the fix in 13.3036 seconds and passes afterward in 4.5489 seconds. All five stream cases pass in 20.7497 seconds; `scripts/local/slack-stream-five-green.log` and `scripts/local/slack-stream-five-source.json` record that checkpoint. The combined broader suite then passes all 461 examples in 179.3629 seconds, including the shared wiki-link fix in `57f00e4ef`. Its log and matching observed input hashes are `scripts/local/slack-stream-bots-461-green.log` and `scripts/local/slack-stream-bots-461-source.json`. The scoped three-skill loop has no further material finding in the streaming slice. The [Slack audit](../docs/reviews/2026-11-01-slack-live-configuration.md) records the source and live-acceptance boundaries.
- Live Slack configuration: the [1 November audit](../docs/reviews/2026-11-01-slack-live-configuration.md) compares the authenticated app manifest with the branch template. It identifies missing Messages-tab configuration, two bot scopes, eleven event subscriptions, and the message shortcut. No live configuration changed. MCP eligibility remains unproven; the submission overview offers preparation instructions without a publication status.
- Isolated Slack acceptance: the user authorized creation and installation on 1 November. “Monoscope Local QA” (`A0CDFCWNBNK`) is installed in Monoscope workspace `T0AP82L853K`. The installed-app portal and real `auth.test` verify the workspace, bot identity, and all eleven requested bot scopes. Credentials are Git-ignored and owner-only; the non-secret receipt is `scripts/local/monoscope-local-qa-installation.json`. Slack verified the isolated callback URL, and the portal confirmed saved direct-message and mention event subscriptions. A real linked plain-text DM now completes through the real model and shared run, with one metadata-confirmed Slack final reply and an authenticated retained conversation. A signed duplicate replay returns 200 without another receipt, run, or publication. The [audit](../docs/reviews/2026-11-01-slack-live-configuration.md) records the source snapshot and fixture boundaries. Private-channel, streaming, proactive, and MCP acceptance remain pending. This authorization concerns the QA app; no personal search grant, shared-app callback change, or production deployment occurred.

The new `nativeFix_applicationResumesSdkSessionsWithoutReplacingItsRunOrAttempts` case passes in 20.6481 seconds through the real application worker, SDK, VM, and database. It retains one shared run, three original attempts, three regression receipts, and one result across admission, candidate, and peer interruptions; completed replay makes no new inference call. GitHub exchange and object retention use existing test interpreters. The expanded full suite now passes all 49 cases. The subsequent Effectful adapter reduction passes the same application-job case and all five PR continuation cases, using the existing production interpreter and original callback effect rows. Browser acceptance of the integrated routine flow passes six cases at `f79e95cd7`. Whole-branch review remains pending; preserve concurrent active edits and test focus.

The [native continuation audit](../docs/reviews/2026-11-01-native-continuation-audit.md) records the current scoped review. Earlier detailed implementation narratives remain available through `git show d881b701b:plans/2026-10-30-unified-agent.md`. Their historical test counts and unfinished notes do not override this checklist or the contract below.

## Product contract

Monoscope investigates telemetry, uses project tools, remembers team knowledge, and follows up on its work.
Slack, the web app, schedules, and project events provide inputs to the same system.
Every execution has a project, an authorizing member, a conversation, a run, and retained evidence.

Background routines act automatically within their configured authority.
Users do not approve every tool call or routine run.
An enabled routine grants continuing authority until someone pauses it or changes its policy.
The authorizing member's current role limits that authority. Each request also retains its original role ceiling; upgrades apply to new work.
Slack identity linking establishes identity and project access. It does not replace routine authorization.

The first launch covers reliable requested investigations and autonomous project operations.
Optional channel investigations, recovery follow-ups, and Slack search extend the same system later.
Code changes and deployments remain outside this scope.

## Sources and current baseline

- [Slack consolidation plan](2026-09-30-slack-agent-consolidation.md): historical decisions, regressions, and implementation evidence.
- Coworker proposal: `../monoscope/docs/plans/2026-10-30-slack-coworker-agent.md` in the sibling checkout.
  Its phases cover actions, native Slack features, Slack search, memory, and proactive work.
  The sibling file is currently an untracked proposal, not a separate implementation to merge.

This branch already contains:

- Signed Slack receipts, thread locks, checkpoints, reply reservations, history reconciliation, and stop handling.
- MCP-derived tool discovery, schema loading, and a shared tool adapter supplied by the application host.
- Explicit tool policies, current member authorization, member attribution, and metric catalog discovery.
- Activated event routines for Slack, PR reviews, and issue enrichment.
- Run history, instruction snapshots, retained artifacts, and a conversation detail panel.
- Versioned project wiki storage with member and agent authors.

Fresh Slack requests now use the routine's configured policy and the shared owner/requester role ceiling.
Legacy Slack checkpoints retain Observe authority during adoption into the common run checkpoint.
Interactive, scheduled, and Slack administrator actions use the common Autonomous policy and durable operation ledger.
Native wiki, GitHub publication, and issue enrichment use that ledger.
Current native Slack behavior still needs controlled workspace acceptance.
The silent private-channel mention needs a traced reproduction. Metric discovery alone does not explain that failure.

## Consolidation rules

1. Keep one capability catalog for API, MCP, CLI server operations, and agent tools.
2. Keep one operation implementation and authorization contract for each capability.
3. Keep one agent executor for interactive and background conversations.
4. Keep one run ledger for schedules, Slack, PR reviews, enrichment, and proactive work.
5. Keep one authority evaluator for every agent entry point.
6. Keep one conversation and artifact interface across entry points.
7. Keep Slack protocol and delivery code in the Slack adapter.
8. Remove replaced code in the same implementation slice.

Consolidation means fewer implementations, not fewer domain types or weaker guarantees.
Transport receipts, tool checkpoints, and run summaries represent different facts.
They can reference one run without duplicating their records or losing their recovery guarantees.
Do not build a universal workflow framework before a second real caller needs it.

## Shared architecture

```text
Slack / web / schedule / project event
                 |
       receipt or trigger adapter
                 |
    conversation + routine + run
                 |
 shared executor and authority evaluator
                 |
     capability catalog and dispatcher
                 |
        existing domain operations
                 |
  checkpoints + results + artifact snapshots
                 |
        Slack / web / GitHub delivery
```

### Capabilities

Use the existing OpenAPI/MCP catalog as the baseline.
Add agent-only capabilities through the same catalog contract.
These include incident context, repository evidence, conversation artifacts, and wiki access.
Do not add fake REST routes solely to register agent tools.

Each capability describes its schema, project scope, required role, effect, and retry behavior.
Effects describe reads, project changes, external publication, and administration.
HTTP methods do not determine effects. `POST /events/query` is a read.
MCP annotations and agent discovery derive from the same descriptors.
Transport-specific serialization stays outside domain operations.

Keep task search and schema loading so each model turn receives a small tool set.
Move existing tools onto the shared dispatcher without changing their result meaning.
Delete duplicated definitions and dispatch cases after their replacement tests pass.

The current `ProjectTools` adapter preserves the caller's effect interpreters.
Extend that adapter instead of introducing service credentials or a second interpreter stack.
Keep member identity through API handlers and activity records.
CLI login, local configuration, completion, and terminal rendering remain client operations.

### Authority and autonomous execution

Use a typed execution policy with three modes:

| Mode | Behavior |
| --- | --- |
| Observe | Read evidence and store conversation results. Do not change project resources. |
| Autonomous | Execute enabled capabilities automatically within current member permissions. |
| Review selected actions | Execute ordinary capabilities automatically. Request approval for explicitly selected capability classes. |

Autonomous is the product default for newly enabled background routines.
Do not impose mandatory approval solely because an operation changes state or requires an administrator.
An administrator-owned routine can execute enabled administrative capabilities automatically.
A reader cannot gain write authority through Slack, a prompt, or an administrator's conversation reply.
External destinations and resource scope remain part of the routine configuration.

Resolve authority from the actor, routine policy, operation descriptor, and current project access.
Instructions, Slack history, wiki content, and tool output cannot grant authority.
Retain operation-level preconditions from the existing product APIs.
Before each tool and delivery boundary, evaluate current authority and cancellation.

Replace scattered `actionsAllowed`, invocation-mode exceptions, and confirmation branches with this evaluator.
Invocation mode still describes execution context. It must not become an independent authorization system.
Keep routine owner authorization distinct from the requesting member's identity.
Interactive requests use the current member. Scheduled work uses the routine's authorizing member.
Slack event work also retains the linked requester and current membership requirements.

Persist the effective policy snapshot with each run for audit.
Apply current restrictions before each operation. A historical snapshot cannot preserve revoked access.
If configuration changes increase authority, apply that increase to future runs.
If configuration changes reduce authority, apply that restriction to active work immediately.

Migrate existing installations without silently increasing their authority.
Preserve existing read-only behavior until an editor explicitly adopts the autonomous policy.
New installations show their automatic actions once during activation.
Keep pause, stop, policy changes, and action history accessible in the existing routine UI.

### Execution, recovery, and optional review

Keep the existing conversation worker and durable checkpoints.
Introduce a persisted operation identity for every mutating tool step.
Retries reuse that identity rather than creating another action.
Record proposed arguments, actor, effective policy, outcome, and external publication references under the run.

Use operation-specific recovery:

- For changes inside one database, commit the change and its execution record in one transaction where possible.
- For external APIs, use supported idempotency keys or reconcile the observed result before retry.
- For ambiguous outcomes, retain an explicit unknown state and expose it in the run.
- For operations without safe retry or reconciliation, stop automatic replay and report the unresolved result.

A claimed row alone does not establish exactly-once effects.
Do not claim that every external operation supports exactly-once execution.
Independent threads can proceed while one operation awaits reconciliation.

Optional approvals extend this same operation record.
Do not create a separate Slack action executor or approval-only run ledger.
Bind approval to the operation, immutable arguments, project, requester, expiry, and configured approver policy.
Slack and web controls submit decisions to the same queue and evaluator.
Approval never bypasses the execution actor's current role.
Repeated clicks cannot create another operation.
Persist waiting work without holding a worker or thread lock during human review.
Resume through the existing executor after a decision.

### Results, conversations, and memory

Keep complete evidence in existing checkpoints and run artifacts.
Give the model bounded result summaries, explicit truncation markers, and pagination or artifact references.
Keep complete MCP envelopes out of model history.
The combined branch now caps model tool messages at 8,192 characters and retains complete envelopes as evidence.
Keep that regression while consolidating the remaining operation responses.
Use existing query formatters and bounded operation responses before adding new formatters.

One conversation contains the user inputs, tool outcomes, decisions, and replies across Slack and the web app.
Run artifacts retain immutable snapshots. Conversation documents remain the editable workspace.
Project wiki pages hold durable knowledge across conversations.
These records have different lifetimes and must not become interchangeable stores.

Add wiki read, search, and write capabilities over `ProjectMemory`.
Use its existing revision checks, path types, limits, and agent author identity.
Retain the actual agent attempt in each native operation and link wiki authorship to its shared writing run. Never cast between attempt and run IDs.
Autonomous writes follow the same policy as other project changes.
Optional review uses the same operation record and controls.
Do not add a separate exception that lets routines append to arbitrary pages.
Load relevant titles and bounded bodies on demand. Wiki content remains evidence, not execution authority.

### Slack experience and delivery

Treat Slack as an input and output adapter for the shared conversation.
Keep mentions, DMs, slash commands, incident threads, and native Agent sessions.
Add suggested prompts, generated titles, and a message shortcut through existing signed receipt handling.
Respect a user's renamed title and existing project bindings.
Unlinked users receive a private account-link action.
Missing or paused installations receive private activation guidance.

Keep the durable reply publication path as the delivery authority.
Add streaming only after interruption and ambiguous-start recovery work in a real workspace.
Slack documents metadata on `chat.stopStream`, but not in the `chat.startStream` argument list.
Do not assume that saving the returned timestamp closes the lost-acknowledgement window.
Keep progress and stream presentation separate from stored execution state.

The 1 November [live QA probe](../docs/reviews/2026-11-01-slack-live-configuration.md#streaming-provider-recovery-probe) now verifies start/append/stop and recovery after separate workers exit before reading start and append response bodies. Caller-supplied task IDs survive in history while the stream is open. Matching the app, bot, thread, and reserved task ID recovers the single stream without a saved start timestamp; a distinct append task ID recovers an accepted append without repeating it. Final stop retains one completed message and matching publication metadata. This is provider-level evidence for task chunks, not application implementation or arbitrary text-append idempotency. Preserve uncertain operations when history is missing or inconclusive.

The [append documentation](https://docs.slack.dev/reference/methods/chat.appendStream/) sets a 12,000-character limit for the complete stream.
An append does not extend the ten-minute timeout.
The [start documentation](https://docs.slack.dev/reference/methods/chat.startStream/) limits top-level streams to channels that represent one session.
Ordinary threadless slash replies retain their existing destination contract.
Channel streams require the recipient user and team identifiers.

During implementation, add acceptance cases for total text length, timeout, and unsupported threadless destinations.
For a lost append acknowledgement, retain the uncertain append before any retry.
Do not infer that an append failed because its acknowledgement is missing.
The [stop documentation](https://docs.slack.dev/reference/methods/chat.stopStream/) exposes metadata only at completion.
Completion metadata alone cannot identify an uncertain start or append.
Optional approvals can use Slack cards or the existing web conversation panel.

### Proactive work and Slack evidence

Model automatic incident investigations and closing updates as event routines.
Channel opt-in selects a destination and trigger. It does not create another background scheduler.
Use the same policy, executor, run ledger, cancellation, and delivery reconciliation.
Default proactive analysis to Observe. Projects can explicitly enable autonomous remediation.
Do not infer recovery from missing errors or missing measurements.

Add Slack search as another evidence capability after app eligibility and user authorization work.
Use the requesting user's token, encrypted with the existing secret mechanism.
Never substitute another user's token.
Token revocation must remove that user's Slack capabilities.

Enforce destination visibility before Slack content enters model context.
Current-channel evidence is the initial channel-safe scope.
Broader evidence requires a demonstrated audience rule, including Slack Connect and restricted conversations.
The user's private DM with Monoscope can use their opted-in personal scope.
Do not reuse private evidence in another audience's conversation or shared artifact.
Keep Slack write tools excluded from the remote MCP catalog. Publish through Monoscope's audited delivery path.
Slack's [official search reference](https://github.com/slackapi/slack-skills-plugin/blob/main/skills/slack-search/SKILL.md) confirms `slack_search_public`, `slack_search_public_and_private`, `slack_read_channel`, and `slack_read_thread`. The QA diagnostic user grant now initializes Slack MCP and captures live schemas for those four tools plus an intentionally excluded channel-search tool. A controlled private-thread read succeeds. The fixed application OAuth, encrypted store, canonical thread read, retained channel audience, and replay now also pass. Canonical source-link handling now supplies the provider permalink and the published citation opens the correct workspace and source thread. Slack Connect and the remaining audience boundaries still require acceptance; a fresh live request on the current runtime, including the credential-failure follow-up, now also passes.

## Implementation sequence

First complete the [open-PR integration prerequisite](2026-10-30-agent-pr-integration.md).
The existing impact-agent stack already supplies wiki tools, attempts, receipts, usage, and retained artifacts.
Reuse that work instead of implementing these systems again.
Reconcile the divergent Haskell runtime before adding another executor or worker protocol.
Preserve specialized review and fix records through typed links to the shared run ledger.
The imported code does not extend Slack authority to code changes or deployments.

Each slice includes implementation, replacement cleanup, and focused regression evidence.
Complete each slice before final signoff. Do not queue expensive signoff during implementation.

| Slice | Work | Removal and reuse | Required evidence |
| --- | --- | --- | --- |
| 1. Consolidate capabilities | Inventory API/MCP/CLI coverage. Unify descriptors, policy metadata, discovery, dispatch, and bounded model results. | Remove duplicate read definitions and dispatch cases. Reuse metric catalog and query formatters. | Catalog coverage, equivalent results, project isolation, preserved effects, bounded large responses. |
| 2. Consolidate authority | Add typed policies and member-scoped autonomous execution. Add activation UI and conservative migration. | Replace hard-coded write bans and scattered mode predicates. Preserve event-run cancellation and owner checks. | Automatic editor write, automatic enabled admin action, denied reader write, revoked access, pause, stop, policy reduction, unchanged legacy authority. |
| 3. Make actions recoverable | Persist operation identities and outcomes. Add transactional and external recovery where supported. | Extend tool checkpoints and the existing run ledger. Reuse domain mutation handlers and member activity records. | Crash before execution, crash after mutation, duplicate job, repeated tool call, external timeout, ambiguous outcome, correct attribution. |
| 4. Complete requested Slack work | Trace the silent private-channel mention. Add prompts, titles, and shortcut intake. | Reuse signed receipts, identity linking, conversation binding, and publication reservations. | Linked/unlinked visible outcomes, automatic requested action and result, retry without duplicate effects, preserved user title, incident continuity. |
| 5. Consolidate shared memory | Integrate the impact stack's wiki capabilities and relevant knowledge discovery. | Reuse versioned wiki storage and agent attempt identity. Remove duplicate schemas, dispatch, and memory-specific authorization modes. | Save knowledge automatically, retrieve it in another thread, stale revision, cross-project denial, correct author and run evidence. |
| 6. Add selective review | Add optional per-class review and web/Slack decision controls. | Extend the same operation state and executor. No parallel approval worker. | No prompts in Autonomous mode, selected review waits, expiry, unauthorized click, repeated click, revoked actor, pause during wait. |
| 7. Add streaming | Extend delivery records for stream start, append, completion, and reconciliation. | Reuse publication claims and progress events. Remove replaced snapshot presentation after acceptance. | Real Slack start/append/stop, lost acknowledgements, worker crash, rate limit, stop, final artifact links, no duplicate final reply. |
| 8. Add proactive routines | Add channel opt-in, incident analysis, and closing updates. | Reuse event routines and incident delivery identities. No new scheduler. | Opt-out silence, one analysis per event, one closing update, authorized remediation, pause, replay, correct recovery evidence. |
| 9. Add Slack evidence | Add user OAuth, remote MCP reads, revocation, and audience enforcement. | Reuse HTTP effects, capability descriptors, and encrypted credential storage. | App eligibility, MCP session, requester scope, revoked token, Slack Connect, private/public audience separation, evidence provenance. |

Slices 2 and 3 form one release boundary. Automatic writes must not ship before their recovery behavior passes.
Slice 6 is optional product policy. It does not block autonomous launch or become a mandatory approval layer.
Streaming and Slack search remain separate release boundaries.

## Verification and rollout

Write failing behavior regressions before bug fixes and missing-feature implementations.
Exercise signed handlers, real domain operations, the database, and delivery boundaries.
Fake nondeterminism through the existing effect interpreters.
Use table-driven cases for permission, policy, retry, and entry-point coverage.

Use `build.log` and `build-test-dev.log` for development feedback.
Change `.test-args` to focus the existing watcher.
For shared-package changes, use the repository's documented watcher restart procedure.
For UI changes, exercise the existing conversation panel in both themes, at narrow widths, and with keyboard controls.

Before each release boundary:

1. Run the focused regressions and catalog coverage tests.
2. Remove replaced implementations and temporary adapters.
3. Record any remaining duplicate path and its concrete reason.
4. Repeat the required Haskell reviews until no material finding remains.
5. Run the final simplification pass.
6. Run local signoff after implementation and focused verification finish.
7. Run the local image check for affected assets or frontend builds.
8. Update the pull request only after the required local results pass.

Use the local OrbStack builder as repository instructions require.
Keep applied migration filenames and contents unchanged.
Choose new migration filenames after inspecting current master.
Group related, unapplied schema changes into one migration per coherent implementation slice.
Do not squash imported PR migrations without evidence that no supported database has applied them.
The local migration ledger confirms that agent migrations `0257`–`0260` have already run.
Their filenames and checksums must remain stable. Fewer migrations are a goal for new work, not a reason to break upgrades.
Report unavailable checks without claiming that they passed.
No production deployment or live app configuration change is authorized by this plan.

Native Slack acceptance requires a controlled workspace and recorded app configuration.
The coworker proposal reports Agent configuration changes. This branch contains no acceptance evidence for those claims.
Record actual grants, subscriptions, app eligibility, session behavior, and publication results before rollout.

## Completion criteria

- One operation descriptor governs schemas, policy, discovery, and execution behavior across callers.
- One executor and run ledger support all agent entry points.
- Autonomous routines complete enabled work without repeated human permission prompts.
- Current roles, project boundaries, pause, stop, and reduced authority constrain active work.
- Mutating retries preserve operation identity and expose ambiguous outcomes.
- Every visible action has an actor, run, result, and evidence link.
- Slack and web users inspect and continue the same conversation.
- New features reuse existing state before introducing another store or worker.
- Each slice records the duplicate implementations it removed.

## Native and requester-evidence invariants

### Capability, request, and review boundaries

Capability constructors must enforce intrinsic editor/administrator floors for writes. Service invocations retain ordinary viewer reads and conversation evidence, without administrative reads or project-write authority. Discovery, execution, and evidence classification interpret the same descriptors. Preserve the caller's existing effect interpreters and the CLI/shared package dependency boundary.

Queued ConversationRun work retains a durable member grant. Historical browser InteractiveRun requests cannot silently become background grants. Issue context uses the shared required-project builder, without a simulated browser session or duplicate issue guide. Slash intake commits input, requester, grant, destination, and job together; acknowledgement performs no inference. Delivery uses the original channel/DM and the real root run ID, with no fabricated message timestamp. A slash reply remains an unthreaded channel-visible message; delivery failure cannot redirect its audience or fall back to the response URL.

Legacy Slack adoption preserves its original model request, system context, completed results, accepted steering, and pending intent under Observe/viewer ceilings. Reuse a known mutation response. An unresolved or unrecorded mutation becomes LegacyUncertain and an unknown operation, rather than being replayed. A relinked identity cannot adopt another member's retained request. Busy-thread followups select the stored requester before applying the message limit; another member's receipts cannot steer or starve that request.

Selected review uses a nonempty collection of action classes and one reviewer requirement. Conversation evidence storage and ordinary reads stay automatic. Decisions retain the reviewer's authority at decision time as well as current membership; a later reviewer upgrade cannot strengthen an earlier vote. Review arguments, actor, audience, deadline, and decision cannot change on retry. Expiry before execution prevents mutation even after approval. Decision capabilities stay outside the model's tool catalog.

Pending review releases the worker, thread lock, and execution lease while retaining the original request. Approval/expiry wakes the existing job. Deferral locks the proposal and cannot postpone a decision already recorded; stale leases or duplicate jobs cannot consume a continuation twice. Request-scoped lookup retains the original proposal attempt while finding its latest admitted continuation. Signed stop and routine controls cancel through the same transactional finalizer. Preserve uncertain started effects, proposals, votes, and artifacts; post-commit notice cleanup cannot restore execution authority or call the model. Resume enables future work, never the cancelled request.

Maintenance retires actorless installations and detached attempts through the same completion transaction and artifact snapshots. It must not invent a replacement actor, expand a null grant, or cancel a healthy current lease during reauthorization.

External publication reconciliation binds the stored destination, GitHub App identity, and exact retained body. A wrong app or edited body cannot close an uncertain action. A fresh request must resolve an older pending publication before making another change; without an observed receipt, that action stays stopped.

### Native fixes and retained sessions

A native fix has one dedicated conversation, shared run, immutable administrator grant, and existing fix job. Duplicate admission keeps the original request. Only its current native lease constructs the opaque run context; an ordinary conversation claim or replaced lease cannot borrow that authority. Account or role loss, withdrawn setup/credentials, and revoked grants prevent execution. Observed revocation cancels the original request; later restoration of access cannot revive it. Expired but still-authorized leases remain recoverable.

Main, admission, and peer attempts retain their real typed attempt IDs. Every restored attempt must belong to the exact project and fix. Wiki authors point to the actual shared writing run; never cast an attempt ID to a run ID. Repeated identical wiki actions reuse the saved response and revision. Native artifact bookkeeping and common completion commit in one transaction; failure on either side rolls back both, including the shared result artifact.

The shared run inspector exposes a generic report. Specialized native output remains behind its existing administrator-only handlers. An administrator link may open that view; viewers must neither receive the link nor its private output. No additional fix-history page, worker, scheduler, or permission store is required.

PR, fix, admission, and peer continuation use one retained-session interpreter. Preserve the original prompt, model specification, evidence window, evidence, notes, attempt, tool budgets, completed tool IDs, and pending parent/child tool. Resolve current authority and credentials again. A retry of the same verified comparison retains its original peer decision and reviewer attempt; a new independent review requires a separate root. Admission binds the exact regression artifact.

Publish the SDK transcript/VM/workflow checkpoint manifest last. A missing manifest means a fresh session only when its directory is empty; retained files without a manifest fail before inference and remain available for repair. Validate identity and transcript fingerprints before restore. Restore a private guest copy; keep its live disk separate from the current clean verifier baseline. Fix continuation retains the verifier-session identity and phase receipts rather than restarting reproduction. An unresolved child wait must not consume its admission budget again on every wake. End-of-input cancels the SDK before another model start; cancellation is not a completed tool failure.

Saved candidate descriptors are not verification proofs. Only capture or checked restore can construct CapturedCandidate; it has no Generic or FromJSON bypass. CandidatePatch also requires its existing smart constructor at JSON decode. Restore validates admission metadata, native reports, logs, source fingerprints, image identity, Git history, patch/bundle hashes, and comparison receipts through the shared verifier. Corruption or changed inputs fail before another model call. Completed recovery revalidates the actual result and reconstructs the reviewed proof without model inference. Durable completion releases obsolete suspended disks while retaining its transcript and result.

Cooperative provider-failure and typed review-wait recovery are implemented. A failure after an accepted result returns that checked result only if no later tool invalidated it. Failed calls remain attributable in usage. Abrupt loss of the SDK worker and VM-owning bridge after a committed wiki write is verified with an existing paired checkpoint and the shared operation ledger. Each bridge owns a private process group; cleanup terminates its remaining children after attempting the normal close protocol. The killed bridge retains a cleanup-failure receipt rather than claiming a successful close. Whole-machine loss and live work that was never checkpointed remain separate acceptance requirements; a cooperative snapshot is not evidence of them. The installed Gondolin `VM.checkpoint` stops the guest, so saving at inference boundaries would require verified resume semantics and cost measurements, not just another call to the current suspension helper. Cover the actual background-job path and Linux/KVM before claiming native production acceptance.

Migration `0280_native_fix_runs.sql` groups native linkage and original-requester backfill and has already run locally. It must remain unchanged.

### Request origins, OAuth, and evidence audiences

Retain the immutable signed Slack destination and requester before queueing or reading history. Deleting/recreating web conversation metadata cannot change the request's audience, origin, or retry identity. Late authority loss rejects a replay without creating a fresh request. Terminal hooks and maintenance use the same finalizer; completed work cannot become eligible to execute more operations.

One typed audience and evaluator govern initial transcripts, navigation, titles, documents, retained runs, artifacts, model context, and publication. Personal evidence requires its author and current eligible project membership. Channel evidence requires the current linked channel membership. Signed receipt provenance takes precedence over mutable conversation metadata; missing or conflicting provenance stays restricted, including after deletion/recreation. Shared mutations and publication cannot launder private evidence into another audience.

Keep execution permission distinct from result-disclosure permission. Raise the conservative conversation/run requirement before a restricted tool executes, and retain its strongest scope across new attempts, metadata updates, deletion, and recreation. That scope also covers older findings and tool-result views, checkpoints, model history, documents, titles, and future answers; their final disclosure audit remains required. Domain role ordering must not depend on PostgreSQL enum ordering.

Personal consent binds project, initiating account, linked Slack user/workspace, expiring receipt, and S256 verifier. It cannot replace the workspace bot installation or another user's grant. Access/refresh tokens use shared authenticated encryption and have no diagnostic encoders that expose them. Existing legacy credential reads remain compatible without weakening new writes.

Grant creation, refresh claims, disconnect, and signed upstream revocation share the member lock. HTTP stays outside database transactions. Conditional writes bind the exact credential and claim, so disconnect, membership loss, workspace replacement, or later reconnection cannot resurrect an old grant. Rotating grants keep refresh token/expiry together. An uncertain or abandoned refresh claim cannot automatically replay a single-use token; definite rate limits use the existing cooldown.

Signed revocation must match its immutable receipt subject and target workspace/user without deleting the bot installation or another account's grant. Receipt completion and grant/consent deletion commit together. Replays and delayed workers cannot revoke a later reconnection. Acquire member locks in stable order and keep expired-consent cleanup in that member/project scope.

Remote MCP reads validate initialization/response identity and advertised schemas, release sessions, and use only the requesting user's current grant. Current-channel reads bind the signed channel and reject unrecognized locator arguments. Workspace-wide search requires the requester's personal DM or web context. Provider write tools remain excluded even if annotated read-only. Discovery denial, throttling, schema changes, or provider failure leave core project work available; requested reads respect cooldown without repeating provider requests.

### Slack sessions and proactive delivery

Generated titles initialize an untitled conversation/session and cannot overwrite a user's rename. Signed rename intake and its existing worker update thread metadata and the app conversation atomically, with stale-event and current-member checks. Messages-tab opens refresh contextual suggestions through the same signed identity and DM lock, including first visits without an assistant-thread row. Ordinary thread stops retain explicit confirmation and status cleanup; do not substitute a different code-channel lifecycle contract.

An incident installation binds a real workspace/channel pair and excludes DMs and malformed channel IDs. Admission and publication recheck project, workspace, channel, issue conversation, installation, activation boundary, authority, and public evidence scope. Reauthorization/resume excludes pre-activation events. Only an alert, measured recovery, or explicit resolution can start the corresponding report; a data gap cannot create a closing report. Event identities, retained completion, and the existing outbox prevent repeated inference or duplicate publication after an uncertain acknowledgement.

## Remaining execution order and release evidence

1. Add meaningful arbitrary-host-loss coverage through the existing ownership and checkpoint boundaries. Preserve the original run, attempts, model, comparison receipts, and inference-free completed replay established by the SDK application-job and PR continuation cases.
2. Repeat hs-distill, hs-evasion-review, and hs-lob-review over the complete branch; implement material findings and repeat until settled. Review runtime callers and opaque proofs as well as changed declarations. Use existing library interpreters/combinators where they delete code; do not weaken types or effect discipline to reduce line count.
3. Apply emil-design-eng, better-interface, and better-ui to the integrated conversation/run/review/artifact flows. Exercise actual handlers and browser interactions, including loading/failure states, both themes, narrow widths, and keyboard use. Keep fixture transport, authenticated application, physical-device, and live Slack evidence distinct.
4. Complete controlled Monoscope Slack acceptance: app grants/subscriptions/eligibility, the silent private mention, linked/unlinked outcomes, native sessions and renames, prompts, shortcut intake, Slack Connect, actual MCP schemas, and audience containment. The missing connector is not proof that no browser session exists; inspect available tools before asking for information already available. The installed QA app and private two-member channel provide the controlled destination; private mentions, linking, an autonomous wiki write, completed streaming, and duplicate delivery have now passed at the isolated checkpoint recorded in the Slack audit. Native Stop during inference and native thread rename now also pass at committed snapshot `ada067673`, with processed provider receipts, an empty callback queue, no late answer, and no duplicate work on replay. The ignored QA harness now separates its worker pool after the original shared three-connection fixture stalled; this is not evidence of a production pool defect. The [Slack audit](../docs/reviews/2026-11-01-slack-live-configuration.md#native-stop-and-rename-through-the-installed-qa-app) records the failed attempt, preserved fixture, replacement source, and acceptance limits. Issue-based prompt freshness also passes: acknowledging and restoring the synthetic issue through authenticated handlers removes and restores its native Messages suggestion, with fresh processed home events and no new run. Native slash intake now passes through the installed QA command, and message-shortcut intake passes at runtime `619472c58`, with thread-confined acknowledged replies and duplicate replay without new work. A failing-first invalid-block rejection regression now passes with all 490 bot examples. The audit records the original oversized-header failure, the corrected native retry, and a verified logical backup restore. Schema-only requests still produce unnecessary query/chart output; clean response quality, channel-context prompts, Slack Connect, and remaining audience boundaries stay open.
5. Establish real interruption and ambiguous-start recovery, then implement and verify streaming through the current publication state. Saving a returned timestamp alone cannot close a lost-start acknowledgement window.
6. Run the required doctests, focused/full regression gates, CLI-linkage check, and Linux/KVM acceptance with truthful capabilities. Run simplify as the last code-review pass. Only after implementation, reviews, and focused verification finish, run final local signoff and the required image check using `DOCKER_CONTEXT=orbstack MONOSCOPE_BUILDER=monoscope-local-amd64`. Follow `docs/local-ci.md` and AGENTS.md; do not publish an attestation for an unpassed check.
7. Push/update canonical PR 619 after required local checks pass. Review and merge through GitHub. This plan does not authorize a production deployment or live app configuration change.

Applied migration filenames and contents remain immutable, including the locally applied native-fix migration. Consolidate related future, unapplied schema work into one coherent migration; do not squash previously applied files to reduce the file count.

## Branch review checkpoint, 1 November

The shared operation review reproduced an admission race in external actions.
A role downgrade, Observe policy, newly selected review, audience restriction,
raised evidence permission, or Slack identity unlink could occur after the tool
check but before its claim, and `send_to_slack` still posted. All six external
cases first fail on an actual captured publication (`expected 0, got 1`), while
the existing atomic wiki cases pass. The same table now covers both operations.

Operation admission now requires a transaction guard. The executor reuses its
existing role, policy, audience, and linked-identity evaluator under the claim
locks for external actions as well as atomic writes. Rejection rolls back the
claim and uses the common stopped-run outcome. No migration, store, interpreter,
or permission prompt is added. All twelve cases pass in the final 485-example
bot run (172.9788 seconds), with unchanged recorded runtime and bot inputs.
All 2,068 doctests also pass at that checkpoint. The subsequent full native group passes with all three queued Slack workflow cases: 53 examples, zero failures in 347.5340 seconds (`scripts/local/agent-effect-admission-native-53-green.log`).
Logs are `/tmp/agent-effect-admission-six-red.log`,
`scripts/local/agent-effect-admission-bots-485-green.log`, and
`/tmp/agent-effect-admission-doctests.log`. Runtime HLint has zero hints; no hints
occur on changed test lines (122 pre-existing test hints remain). Pinned
formatting and whitespace checks pass. The repeated scoped three-skill
review has no further material finding. This does not close whole-branch review;
the live QA checkpoint above predates this admission fix.

The controlled publication retry exposed a separate routing defect: substring
matching treated a freeform `send_to_slack ... report completion` instruction
as a saved daily-report request. Run `5ce569e9-41d0-446c-90e5-516f96e0571a`
returned “No daily report found” without invoking the agent or an operation.
That is a failed acceptance attempt despite its terminal run status. The private
receipt is `scripts/local/slack-admission-before-report-routing-fix.json`.

The existing queued-command table reproduces the same wrong reply before the
fix (`/tmp/slack-report-routing-red.log`). The shared classifier now recognizes
complete saved-report commands and parses their period through the existing
`ReportType` parser. Other instructions use the agent. Three doctests cover
substring rejection, compound instructions, and a polite weekly-report request.
All three queued-command cases pass in the native checkpoint above. Runtime
HLint and formatting pass; scoped distillation, evasion, and locality review find
no further material issue. No new parser module, dependency, configuration, or
execution path is added. The final broad watcher passes 491 examples (486 bot
and five GitSync cases) in 179.6252 seconds, with unchanged recorded inputs.
All 2,076 doctests pass. Evidence is
`scripts/local/slack-report-routing-bots-gitsync-491-green.log`,
`scripts/local/slack-report-routing-source.json`, and
`/tmp/slack-report-routing-doctests.log`.

The unchanged native Slack request then passes on committed QA snapshot
`ada067673`: one real `send_to_slack` call, one completed operation with NoReview,
one exact message, and no personal search grant. The retained receipt matches
the real Slack timestamp, actor, and run. Replay adds no work. The progress
stream closes and one final reply links the retained conversation. The private
receipt is `scripts/local/slack-admission-live-acceptance.json`; hashes are in
`scripts/local/slack-report-routing-live-source.json`. The negative admission
cases remain effect-backed application tests; arbitrary host loss, Slack
Connect, and final release verification remain open.

The current review continues against `origin/master`; the whole-branch review
remains open. The completed source-reading slice covers `Pkg.AgentCatalog`,
`Pkg.AgentOperation`, `Pkg.SlackOAuth`, `Pkg.SlackMCP`,
`Data.Effectful.AgentArtifacts`, `Data.Effectful.GitPublication`, and its `Types`
module. The distillation and evasion checks retain the existing catalog,
requester grant, precise HTTP/UUID/time effects, opaque publication capabilities,
and production/test interpreter pairs. No further material finding was identified
in those modules during this slice. Their application callers and native
interpreters still belong to the remaining whole-branch review. Additional source
reads cover the native publisher, checkout wrapper, and shared host Git command.
Their raw IO remains inside the existing interpreter boundary; temporary resources
use scoped wrappers, and host Git filters the environment and disables external
hooks, credential helpers, global configuration, and redirects. These reads do not
prove abrupt host-loss behavior or Linux/KVM acceptance. The shared
request-indicator JavaScript retains its request-identity set because overlapping
requests and cancelled confirmations need independent lifecycle tracking; a
visibility toggle cannot replace that behavior.

The manifest review removed a second `base64` entry from
`shared/monoscope-shared.cabal`. A dependency-set comparison proves that this
removes no dependency. `cabal check` reads the package successfully but reports
the existing parent-directory license path, `-Werror`, and package metadata/bounds
issues; it is not a passing package check. The license path and warning policy
were confirmed in the preceding committed manifest and remain unchanged.

The cached development CLI fails the portability check because it links Homebrew
zlib. Running the existing release packager on a temporary copy succeeds: it
uses system zlib, passes strict code-signature and linkage verification, and
prints `monoscope 0.6.31`. The temporary copy is removed automatically. Evidence
is `scripts/local/cli-release-review-20261101.{json,log}`; the JSON records the
source binary, packager, and linkage-check hashes. This verifies packaging of
that cached executable, not a fresh build of the final branch.

The next model slice covers `AgentUsage`, `ProjectMemory`, `IssueFixes`, and
`ImpactReviews`. It found the retained-fix eligibility gap fixed by migration
`0282` and its failing-first lifecycle regression. The full native group then
passes with Slack destination/citation coverage: 82 examples in 370.1305 seconds,
with unchanged recorded source inputs. The two componentwise usage sums remain
explicit; a new generic wrapper would add machinery for two short instances.
Their typed attempt ownership, immutable billing attribution, wiki revision
locking, and native publication capabilities remain intact. This is another
review slice, not whole-branch signoff.

Release-path review found that the documented local-image policy was not enforced.
Commit `c7dd262f2` rejects remote, missing, and CI application-image builders;
checks the actual local socket and sole OrbStack node; removes SSH builder setup;
and leaves CI able to alias an existing image. Seven forbidden-build scenarios
first failed in the isolated registry fixture. The final 23-case release suite
and `ci-selftest` pass, including local setup, no SSH attempts, correct AMD64
arguments, and merge reuse without CI compilation. The actual local-builder guard
also passes. The cold-cache fixture now asserts an empty unique mount before
testing retained additions, avoiding global cache resets; three independent trials
pass. Logs and input hashes are `scripts/local/local-image-policy-*` and
`scripts/local/local-cold-cache-*`. These are focused checks, not final release
attestations or a production application image build.

The existing visual review now includes the live synthetic incident and verified
citation checkpoints. Its hosted HTML readback matches the local 232,609-byte
file at Page sequence 60. All 102 steps across twelve walkthroughs pass at four
widths; the 114 embedded source entries and all walkthrough steps are preserved. The actual hosted Page opens in Chrome and its desktop image decodes;
interaction within its embedded visualization remains unverified. See the
[interface evidence](2026-10-29-routine-interface-review.md) for the exact scope.

The Slack widget-array gap now has a focused implementation and regression check.
Chart replies preserve widget configuration and link to the retained conversation.
An explicit widget array takes precedence over a legacy primary query.
The opaque model-widget decoder rejects SQL, executable content, and server context before signing.
All 489 bot examples pass, including retained charts and replay without duplicate delivery.
All 2,083 doctest examples also pass, including the seven new parser examples.
Live Slack image display remains pending at this checkpoint.
See [Slack chart replies](../docs/reviews/2026-11-01-slack-widgets.md).

The review Page now includes the native fix inspector capture and chart behavior.
Hosted sequence 62 exactly matches the 236,711-byte local visualization.
All twelve flows and 102 steps pass at four widths; source entries total 115.
Both application captures decode in Chrome. Hosted embedded interaction remains unverified.
See the [interface record](2026-10-29-routine-interface-review.md) for the preservation and verification scope.

Retained run evidence now renders interactive widgets inside the existing inspector.
Chat and run views share the widget renderer, text-relative minimum sizes, and indexed top-level IDs.
New and stored model widgets reject executable column links while preserving valid siblings.
The failing-first handler lifecycle and all eleven conversation browser cases pass at the recorded UI checkpoint.
The temporary browser adapter is removed. All 504 bot cases and 2,089 doctest examples pass.
The final bot run preserves all 280 recorded Haskell and manifest input hashes.
The review Page includes the synthetic chart capture and retained-widget source step at sequence 67.
All 103 steps across twelve flows pass at four widths; all 115 prior source entries remain, with one addition.
Hosted source readback matches the 237,838-byte local artifact. Whole-branch reviews and final release gates remain open.
See [retained widgets](../docs/reviews/2026-11-01-retained-widgets.md) for the scoped review and exact evidence.

The guest command review closes an unchecked timeout conversion.
The SDK schema rejects invalid bounds without a guest operation; native decoding uses an opaque positive Word32 timeout.
All 20 worker tests, the native SDK exploration case, and all 2,095 doctest examples pass.
The fresh make cli-build binary also passes Mac packaging, signing, portable linkage, version and help.
The shared-package change required one reload per root watcher; ordinary test refocusing still uses .test-args.
See [guest timeout bounds](../docs/reviews/2026-11-01-guest-command-timeouts.md) for failing-first proof and limits.
Whole-branch review, remaining live acceptance, Linux/KVM, arbitrary host loss, final local signoff, and publication remain open.

Populated chart delivery now passes in the actual Monoscope Slack QA thread.
A native request and follow-up each produce two retained widgets and three acknowledged reply parts.
The line chart shows synthetic batches of 12, 18, and 14; the stat visibly displays 44.
The live check exposed a blank stat PNG, now covered by a failing-first signed-endpoint regression.
Both PNG export cases pass in light and dark appearance, including zero and missing data.
The web inspector retains both widgets, anonymous access redirects, and duplicate replay changes no counts.
The original routine policy is restored, and the temporary test-server adapter is removed.
The review Page preserves its prior content and adds the actual Slack capture at sequence 68.
See [populated Slack charts](../docs/reviews/2026-11-01-slack-populated-charts.md).
Whole-branch reviews, remaining live scenarios, Linux/KVM, host loss, final signoff, and publication remain open.

The full shared-workspace review reuses the existing guest command wrapper for preparation and verification.
The fresh Mac CLI passes build, packaging, signing, linkage, version and help checks.
The complete native review/fix group passes 50 examples in 348.4111 seconds; all 2,098 doctest examples pass.
A separate local Lima VM now provides ARM64 Linux/KVM without project or home mounts.
The pinned SDK boots and restores a retained edit under UID 1000; all twenty worker tests pass in Linux.
The current Linux CLI also builds locally through OrbStack using the shared caches.
Its actual KVM preparation and frozen comparison pass, while a weakened candidate test is blocked.
Five observed CLI guest processes use KVM with two CPUs and 1024 MiB.
The fresh remote inventory still includes all twenty selected heads and adapted #661; current master is already an ancestor.
See the [workspace interpreter review](../docs/reviews/2026-11-01-workspace-interpreter.md) for scope and receipts.
Full Linux application jobs, remaining host-loss and live acceptance, whole-branch review, final gates, and publication remain open at that checkpoint.

The selected Linux application cases now pass: seven PR-review recovery cases and two fix-job cases,
9 examples with zero failures in 110.2487 seconds. All 2,098 Linux doctest examples also pass.
The observer confirms 26 guest processes use real KVM, with two CPUs and 1024 MiB each.
The first run exposes missing VM media tools and HOST_URL; applying the existing CI prerequisites fixes the harness.
Application and interpreter inputs match the snapshot; later concurrent Slack edits need their own checks.
This closes the selected Linux application gap, not full-suite, live GitHub, or physical-host power-loss acceptance.
See the [workspace review](../docs/reviews/2026-11-01-workspace-interpreter.md) for evidence and limits.
Remaining live scenarios, host-loss limits, whole-branch review, final gates, and publication remain open.

The Linux virtual-host-loss probe exposes a truncated disposable Git mirror after the abrupt stop.
The checkout interpreter now rebuilds that cache under its existing lock and retries once with current source access.
The same interrupted run then resumes from its retained checkpoint: one case passes in 19.2457 seconds.
Its original attempts, regression, wiki revision, and operation remain; completed replay adds no inference.
Four observed guests use real KVM. The recovery binary includes the fix, so this is not an unchanged-binary crash cycle.
The same review fixes unresolved MCP input references using reachable local schema definitions.
Both regressions fail first, then pass within the 76-case focused run; all 2,098 Linux doctests also pass.
See [checkout and tool schemas](../docs/reviews/2026-11-01-checkout-and-tool-schemas.md) for receipts and controlled-provider limits.
Whole-branch review, remaining live acceptance, physical-host-loss limits, final gates, and publication remain open.

## External references

- [Slack MCP overview](https://docs.slack.dev/ai/slack-mcp-server/): eligibility and user authorization requirements.
- [User-token exchange](https://docs.slack.dev/reference/methods/oauth.v2.user.access/) and [authorization metadata](https://mcp.slack.com/.well-known/oauth-authorization-server): endpoints, client-secret POST, and S256 PKCE.
- [Token rotation](https://docs.slack.dev/authentication/using-token-rotation/): paired refresh/expiry metadata and single-use refresh tokens.
- [Upstream token revocation](https://docs.slack.dev/reference/events/tokens_revoked/): workspace-bound OAuth user IDs; signed receipt handling and recovery are implemented.
- [Message shortcuts](https://docs.slack.dev/reference/interaction-payloads/shortcuts-interaction-payload/): signed invocation, selected-message context, and callback identity.
- [Suggested prompts](https://docs.slack.dev/reference/methods/assistant.threads.setSuggestedPrompts/): Agent apps omit the legacy thread timestamp and provide at most four prompts.
- [Agent session status](https://docs.slack.dev/reference/methods/agents.sessions.setStatus/): initial titles are ignored once a session exists.
- [Agent sessions](https://docs.slack.dev/ai/agent-sessions/): native status, renaming, and title-change events.
- [Stream start](https://docs.slack.dev/reference/methods/chat.startStream/): stream arguments and recipient requirements.
- [Stream completion](https://docs.slack.dev/reference/methods/chat.stopStream/): final blocks and metadata.

External API assumptions require another review during their implementation slice.
