# Unified Monoscope agent

Status: implementation in progress, 2026-11-01. All twenty selected incoming PR heads and published #619 remain included. Native recovery, UX acceptance, and final verification are being consolidated locally; the remote PR has not been updated.
This document controls future agent work on this branch.
It combines the Slack consolidation work and the Slack coworker proposal.
It incorporates the requested default of autonomous background work without repeated permission prompts.
This document does not claim that unfinished features or live Slack acceptance are complete.

Morning review priorities: keep one integrated conversation, run, review, and artifact experience with fewer separate pages. Prefer compiler-enforced invariants, small algebraic interfaces, and shared domain operations. Use existing libraries and internal eDSLs when they reduce total maintained code, including their setup. Remove replaced logic instead of leaving parallel paths. Preserve autonomous background operation as the default.

Repeat `hs-distill`, `hs-evasion-review`, and `hs-lob-review` over each implementation and its revisions. Apply `emil-design-eng`, `better-interface`, and `better-ui` to the integrated agent flows. Verify real interactions, keyboard use, loading and failure states, both themes, and narrow layouts; record the exact boundary of browser and live-service evidence. These priorities remain active while the user is away and at the morning review.

Evidence uses one typed audience and one evaluator across web navigation, threads, retained runs, artifacts, and agent access. Personal reads require ownership and current eligible project membership. Channel reads require current linked Slack membership. Shared mutations and publication reject private evidence.

Requester-bound OAuth, encryption, rotating-token refresh, and revocation share the existing credential path. Fetched Slack history receives its scope before model use; a data-only upgrade classifies older evidence from stored receipts and retained authorship. Ambiguous provenance remains restricted, including after conversation deletion. The cancellation heartbeat uses local authority checks rather than polling Slack.

Current review boundary: web, issue, slash-command, scheduled, Slack-event, enrichment, incident, and retained PR-publication decisions resume their existing jobs. Native PR analysis now pairs its SDK transcript, live guest disk, original inputs, queried evidence, notes, and remaining budgets. The real-SDK/VM cases pass for approval, rejection, and expiry. The current native-fix consolidation passes all 43 native integration and 450 bot cases. The unchanged SDK worker tests previously passed all 19 cases. The shared-panel change is committed as `ec35f5fd9`; its twelve focused cases and browser checks pass.

Native fix, admission, and peer wiki writes now use the shared ledger, as described below. Remaining work is explicit: broader native fix session recovery needs acceptance; streaming needs implementation and acceptance; Slack app eligibility, Slack Connect, and the silent private mention need live verification. The existing visual review now includes source `0a19031b5`, paired native recovery, the shared decision panel, and the native-fix consolidation. Whole-branch review, doctests, Linux/KVM coverage, local release signoff, and the canonical PR update remain open. Autonomous remains the default; these changes add no mandatory permission prompts.

The current tool session has browser automation but no installed Slack connector. Plugin discovery confirms the Slack integration is available and was already suggested. This limits live workspace acceptance; it does not prevent local implementation, fixture integration tests, or browser checks. No live app settings were changed.

## Native fixes use the shared run and wiki ledger

A queued native fix now owns one shared run and dedicated conversation. Duplicate admission retains that run. The existing fix lease constructs an opaque `RunContext`; the ordinary conversation worker cannot borrow it. A replaced lease loses authority even if it still holds an old context. Fix, admission, and independent-review attempts keep their actual attempt IDs, while their wiki revisions link to the shared writing run. All four writes use the same native-operation ledger and transactional wiki recipe as other agent work. Repeated identical calls return their retained response instead of creating another revision.

Current native eligibility remains administrator-only. Account deactivation, role loss, lost setup/credentials, or withdrawal of the shared grant prevents execution. Observed revocation closes the original request through the common finalizer; restoring access cannot revive it. Expired leases remain recoverable without cancelling a still-authorized request. Specialized fix artifacts keep their existing administrator-only handlers. Shared completion stores a generic report and links administrators to those artifacts from the existing run inspector; it does not copy private native output into the shared report.

Native artifact bookkeeping and common completion commit in one transaction. Failure on either side rolls back both, including the shared result artifact. No new worker, scheduler, page, permission store, or Haskell module is introduced. One grouped migration, `0280_native_fix_runs.sql`, links existing jobs to shared runs and preserves their original requester and known administrator ceiling. Its real populated backfill is covered by an isolated database test. This migration has now run locally and must remain unchanged.

The original regression expected a shared run after duplicate admission and found none. The two focused integration cases pass, including the actual application worker, main/admission/peer wiki receipts, foreign-attempt rejection, replaced leases, immutable linkage, account and role revocation, sticky cancellation, migration backfill, and injected failures at both completion boundaries. The final source passes all 43 native integration examples, including the real SDK/VM cases. All 450 bot integration examples also pass against the same source. Formatting and diff checks pass. A fresh inventory still has 28 open PRs; all twenty selected incoming heads and published #619 remain ancestors of this branch, with no head changes since the preceding inventory.

Scoped `hs-distill`, `hs-evasion-review`, and `hs-lob-review` were repeated after the direct-lambda simplification. The shared wiki recipe has two production callers; the native access predicate is reused by execution and retirement. No new warning suppression, raw-IO implementation seam, weakened identifier, separate lifecycle, or client-side state machine was added. HLint reports four pre-existing hints on unchanged expressions.

The UI review applies `emil-design-eng`, `better-interface`, and the six owning interface skills to the completed native-fix run inspector. Scope: actual handler output, existing Lucid/Tailwind components, administrator and viewer states, light/dark themes, widths 390 and 1280. Chrome uses captured handler HTML and project assets through fixture HTTP transport. This is not live authenticated deployment or physical-device acceptance.

| Domain | Evidence and result |
| --- | --- |
| Accessibility | Native disclosure opens with Enter; the administrator link receives keyboard focus with a visible outline. Existing status text conveys completion. |
| Layout | Eight role/theme/width combinations have no horizontal overflow. The shared inspector and action history remain one integrated surface. |
| Writing | Code fix, Fix workflow, and Review verified fix identify the source, verification requirements, and next action. Viewer completion explains who can inspect private artifacts. |
| Typography | Existing readable text sizes and wrapping are retained; code proposals stay collapsed until requested. |
| Colors | Existing semantic tokens work in both inspected themes. Light-theme mobile and dark-theme desktop focus and status were inspected visually; no new palette or contrast claim is introduced. |
| UI | One ordinary link opens the existing artifact view. Completed run panels stop polling; no motion or JavaScript is added. |

| Priority | Location | Before | After |
| --- | --- | --- | --- |
| Medium | `src/Pages/AIThreads.hs`, shared run inspector | Native fixes had no source label, workflow explanation, or integrated artifact link. | The common inspector identifies code fixes and gives current administrators the existing verified-fix link. Handler assertions reject that link for viewers. |

| Rejected candidate | Reason |
| --- | --- |
| A separate fix-run history page | Duplicates the existing inspector and navigation. |
| Raw fix output in the shared report | Specialized artifacts already have administrator-only access; the generic report is sufficient. |
| Custom disclosure, navigation, or animation | Native HTML provides the complete interaction without another implementation. |

Verdict: approve within this inspected completed-run flow. Browser checks report no errors; loading/failure behavior of other surfaces retains its previous evidence. Broader native fix SDK/VM interruption recovery, streaming, controlled live Slack acceptance, whole-branch reviews, and final release gates remain open.

## Queued web and issue conversations

Member-authorized web requests now commit their input, immutable grant, conversation, optional routine, and background job together. The HTTP handler returns without model inference. The existing dispatcher, conversation lease, model pipeline, checkpoint, operation ledger, and finalizer execute the request. Repeated delivery publishes one answer. Review decisions and deadline wakes resume that same request. Permission loss cancels it before inference.

`ConversationRun` records a durable member grant. Historical `InteractiveRun` requests retain browser authority and cannot silently become background grants. Issue chat uses the same queue and one shared issue-context builder. The chart interpreter now exposes a required-project entry point, so background context building needs neither a simulated browser session nor an interpreted HTTP error effect. The issue guide is included once.

Migration `0277_member_conversation_runs.sql` groups the new source constraints in one append-only file. It adds no table, scheduler, or permission store. Applied migrations remain unchanged.

The original regression failed with one model call during HTTP submission. The current implementation passes 420 bot, 76 API, 41 incident-delivery, 87 dashboard/widget, and 16 chart-cache examples. Six focused web cases cover admission, duplicate delivery, authority loss, approval, rejection, expiry, and enqueue rollback. The issue case verifies queued execution and retained context. Real authenticated Chrome checks at widths 390 and 1280 verify automatic answer display, terminal polling removal, and preservation of an unsent draft and its request ID. The temporary browser fixture was restored after acceptance. Formatting and diff checks pass. Repeated scoped distillation, constraint, and client-behavior reviews found no remaining material finding in this slice; HLint reports only the two pre-existing hints in unchanged AI code.

The asynchronous title and retained slash binding are covered below. Remaining work includes native fix/admission/peer consolidation, broader crash recovery, streaming, live Slack acceptance, and final branch reviews and release checks. This checkpoint does not establish completion of the unified plan.

## Queued slash commands and shared Slack publication

Slash intake now commits the original input, requester, grant, destination, and `RunConversation` job in one transaction. The HTTP response acknowledges the retained request without model inference. Review decisions wake the same job. Native events and commands share reply rendering, the existing publication reservation ledger, reconciliation, and finalization. Commands use the actual retained root run identity; they never fabricate a Slack message timestamp.

Replies use `chat.postMessage` in the original channel or DM, with no thread for a slash command. This preserves the original channel-visible reply audience. The callback URL is no longer the execution or delivery mechanism. The existing app grant covers public-channel posting; private channels and DMs must be accessible to the app. A destination failure stays visible and retryable; it does not silently redirect the result elsewhere.

The slash consolidation checkpoint passed 442 bot, 76 API, and 41 incident-delivery examples; terminal-hook verification below extends the bot suite to 446 examples. Focused cases cover atomic admission and enqueue rollback, channel and DM replies, ambiguous acknowledgements, reconciliation without another model call or mutation, approval, rejection, expiry, membership removal, Slack unlinking, pause, cached reports, and fresh web access checks. These are deterministic integration checks, not live Slack acceptance.

Failing-first regressions reproduced lost replies after deleting web history and refused reads of the command's own channel. Commands now retain their typed original workspace, destination, and Slack user on the existing run record. The smart constructor has read-only field access; SQL rejects changes to the binding and mismatched recovery attempts. Legacy requests remain unbound rather than guessing from current identities. Delivery, recovery, and conversation readers use the retained origin even after transcript deletion.

Native events and slash commands share destination-bound Slack evidence reads. Channel requests cannot broaden their destination or use personal workspace search. DM requests use the existing DM-owner check. The same parameterized cases verify credentials, provenance, disconnection, membership loss, schema changes, wrong DM peers, invalid locators, and rejected broader searches for both adapters. Deletion, original-input recovery, and SQL immutability regressions pass. Formatting and diff checks pass; scoped HLint has no new findings. Repeated scoped distillation, constraint, and client-behavior reviews leave no material finding in this checkpoint.

The asynchronous web title is covered below. Native fix/admission/peer consolidation, streaming, live Slack acceptance, and final branch gates remain open.

## Terminal queued failures

Both OddJobs terminal hooks use the existing run finalizer. A retry leaves the request active. A permanently failed exception records a failed run; an exhausted timeout records a timed-out run. Both retain saved artifacts and action history and add one execution message. A completed or cancelled run is not reopened. Other job types retain their existing behavior.

The existing hourly recovery step closes requests whose failed job was saved before a database outage or process crash prevented its callback. It correlates the exact queue payload with the retained run and considers only its latest job. A newer queued attempt prevents an older failure from closing active work. Recovery uses the same authority-bound finalizer and failure message without requeueing work. When the typed timeout callback was lost, recovery records the known job failure without guessing the exception type from diagnostic text.

Eight failing-first lifecycle cases cover web and slash requests, actual OddJobs retry and terminal transitions, missed exception and timeout callbacks, repeated hourly recovery, newer queued work, retained artifacts, one execution message, and zero model or HTTP calls during recovery. All 450 bot examples pass on the final source. Formatting, diff checks, and scoped HLint pass. Repeated scoped distillation, constraint, and client-behavior reviews leave no material finding. This adds no table, migration, scheduler, attempt counter, or model executor. The job and run commits remain separate; the retained states support eventual recovery through the existing scheduler.

## Queued conversation titles

Title generation now finishes before the shared conversation finalizer marks the request complete. The results poll selects the existing page-title header from the server response as an out-of-band update. This preserves the composer, draft, and next request identity and lets the final poll update the title before polling stops. The redundant access check was removed because title generation already checks access before and after its work.

The existing queued-conversation regression first reproduced the missing header selection, then a title call observing `RunSucceeded` instead of `RunRunning`. It now verifies the corrected ordering, saved title, terminal polling, one model execution, and one published answer. All 450 bot, 76 API, and 41 incident-delivery examples pass on the final source. Chrome using HTML captured from the real handlers also verifies the header and document title, stopped polling, preserved draft, request ID, and keyboard focus, with light and dark themes at 390 and 1280 pixels. This browser check uses captured handler responses, not a live authenticated server. Formatting, diff checks, scoped HLint, and repeated scoped reviews leave no new findings.

## Native PR analysis continuation

The production host now opens the existing Pi SDK session file rather than an in-memory session. On a selected-review wait, the same guest owner freezes the live disk and host-owned `/run/monoscope-*` inputs and logs. The host atomically publishes one typed checkpoint manifest after the immutable disk and transcript are durable. No new table, worker, VM manager, migration, or dependency is added.

`SessionIdentity` distinguishes isolated evaluation from a retained original request. `NativeProgress` distinguishes waiting work from an accepted finished draft. Hidden checkpoint constructors validate remaining query and submission budgets on decode. `ReviewContext` carries original refined source revisions, model configuration, prompt, evidence window, changed files, queried evidence, and notes. Current credentials and authority are resolved again; credentials are excluded from stored inputs. The clean verifier baseline stays separate from live edits.

Recovery restores a private verified disk copy and the matching frozen SDK transcript. It invokes the exact pending tool through the SDK runner and current schema hooks, skips completed tools, and resumes without another user prompt. A completed recovery revalidates the retained draft without starting a VM or calling the model. A different request or changed input cannot inherit the checkpoint. The checkout owner now propagates callback exceptions after cleanup instead of hiding a review wait as a checkout failure.

Failing-first cases reproduced lost suspension, duplicate inference, missing `/run` tool logs, and discarded application evidence after a clock advance. The paired SDK/VM fixture now retains a completed file append and its full tool log, uses five model responses instead of seven, replays its finished result without inference, rejects changed input, and starts fresh for a different root. Three application cases exercise approval, rejection, and expiry through the real web decision handler, SQL, SDK, and VM. They preserve query citations and notes, publish once, and attribute the approved wiki revision to the original run. The worker suite passes all 19 tests on Node 22.23.2. The first broad native run exposed an unnecessary context lookup for unmapped repositories. Reusing the same mapped-service predicate for context and analysis restores their no-agent path without weakening the existing regression. The final full native suite passes all 42 cases, including fixes, independent review, calibration, screenshots, compaction, cache integrity, metering, and publication recovery. Formatting and diff checks pass; scoped HLint has only the two unchanged hints in publication code. Repeated scoped `hs-distill`, `hs-evasion-review`, and `hs-lob-review` passes leave no further material slice finding. Whole-branch reviews and release signoff remain pending.

This establishes selected-review interruption and continuation, not a snapshot after every arbitrary process crash. Only the current immutable snapshot generation is retained while waiting. After the accepted manifest is durable, obsolete suspended disks are removed; completed recovery retries cleanup. Transcript and accepted result remain. The failing-first real-SDK/VM resource regression passes; cleanup failure cannot undo a durable result. The fix/admission/peer paths remain open. `Pkg.ImpactReview.projectMemory` still writes directly for those roles; the PR-review ledger restriction does not make those other roles read-only. Consolidate their authority and receipts through typed links to the shared run ledger before claiming native mutation consolidation complete. Streaming follows recovery acceptance.

## Shared review controls: interface review

Full review scope: the shared run inspector's waiting, submitted, and failed-decision states. Stack: Lucid, HTMX 4, Tailwind/DaisyUI, existing semantic theme tokens. This is not a review of every agent screen or live Slack.

| Domain | Evidence inspected | Result |
| --- | --- | --- |
| Accessibility | Named native buttons; keyboard Enter; disabled request state; visible focus in Chrome | Controls work at both widths; no screen-reader session performed |
| Layout | Actual handler HTML at 390/1280, waiting and acknowledged states | No horizontal overflow; same scoped panel update |
| Writing | Run heading, source label, pending and resolved action text | Misleading generic “Routine run” fallback replaced with “Run” |
| Typography | Rendered headings, action names, deadline, details | Existing type hierarchy remains legible in both themes |
| Colors | Rendered light/dark states and focus treatment | Sampled heading, secondary text, and decision labels exceed 4.5:1: minimum 5.79 light / 5.19 dark; not a full WCAG audit |
| UI | Immediate acknowledgement, transient HTTP 500, polling boundaries | Both controls disable in flight and recover after failure; no added animation |

| # | Severity | Domain | Location | Before | After | Why |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | HIGH | Accessibility | `src/Pages/AIThreads.hs:412` | Source whitelist hides web/slash review buttons | Render from pending state and current reviewer role | Supported requests could not be decided from their run panel |
| 2 | MEDIUM | UI | `src/Pages/AIThreads.hs:419`, `:826` | Empty POST response leaves stale controls until polling | Return the shared renderer, morph its run panel, disable both buttons during POST | Makes a saved decision visible immediately and prevents competing submissions |
| 3 | MEDIUM | Writing | `src/Pages/AIThreads.hs:350` | Untitled web run says “Routine run” | Shared fallback says “Run” | Source metadata already distinguishes web, schedule, Slack, and native work |

| Location | Considered change | Rejected because |
| --- | --- | --- |
| `operationGetH` | Add a new combined review page | It already delegates to the canonical run page |
| Shared decision form | Add a JavaScript refresh listener | HTMX and the existing renderer express the entire update |
| Conversation polling | Refresh the entire page after a decision | Scoped morphing preserves the composer and draft |

Verification: nine failing web/slash cases reproduced missing controls. Four subsequent cases reproduced the empty acknowledgement. All twelve focused review cases pass after their fixes. Chrome with real handler HTML and project assets verifies Enter submission, both buttons disabled in flight, retry after an injected HTTP 500, immediate Approved state, no overflow or page errors, both themes, and widths 390/1280. The desktop draft is preserved, the shared “Run” heading is visible, and sampled normal text has a minimum 5.79:1 light / 5.19:1 dark contrast ratio. The browser transport uses captured handler responses; backend decisions are separately exercised through real handlers and SQL. Earlier authenticated live-server acceptance is recorded below. Live Slack, physical mobile devices, screen-reader use, and a full WCAG audit are not verified here.

Verdict: Approve — no actionable findings remain within this inspected run-decision flow. The broader agent implementation and release checks remain open.

## Retried Slack requests after authority loss

Before treating a retained receipt as new work, the adapter now loads its original actor through the existing event claim and evaluates current linked identity and shared run authority. A running request whose authority is unavailable closes through the common finalizer; an already-cancelled request only retries cleanup. Completed and failed attempts retain their existing delivery and recovery paths. No schema, authority evaluator, executor, or worker is added.

Three regressions first reproduced `RunRunning` after membership removal, Slack unlinking, and owner downgrade. All fifteen focused Slack lifecycle cases now pass. They verify the same cancelled request, preserved proposals and votes, refused unstarted action, completed receipt, zero extra inference or mutation, and no Slack HTTP request for an unlinked or removed requester. Notice refresh still requires current access and cannot republish private progress after revocation. Closure occurs when the retained receipt is retried; this does not introduce a membership-change scheduler.

Formatting, diff checks, and scoped HLint pass. Repeated scoped distillation, constraint, and client-behavior reviews found no material remaining issue. The final source passes all 413 bot, 76 API, and 41 incident-delivery examples. The merged dashboard/widget path also passes 87 examples. Remaining web/slash/native continuation, streaming acceptance, controlled live Slack verification, and whole-branch release gates remain open.

## Routine deletion during review

Deletion now uses the existing pause and notice-wake operations before removing the installation. This closes its requests while their routine binding is still available, then queues the existing cleanup worker. A queue failure leaves the installation paused rather than reviving authority or detaching unfinished work. The change adds no model function, state, migration, or worker.

The new regression first reproduced a deleted routine leaving its Slack continuation deferred. All twelve Slack review lifecycle cases pass after the one-handler fix. The deletion case verifies actual removal, request cancellation, retained proposal and review, zero extra model calls or mutations, and the updated existing notice. Formatting, diff checks, and scoped HLint pass. Repeated scoped distillation, constraint, and client-behavior reviews found no material remaining issue. This deletion path also passes in the final 413-example bot suite; neighboring API and incident suites pass all 76 and 41 examples. Whole-branch release gates remain pending.

## Signed Slack stop and waiting-notice cleanup

Signed stop intake now records the authorized stop marker and cancels covered requests in the same transaction. Routine controls and signed stops use the same run finalizer. Its scope comes from retained signed receipts and the authorized thread marker; an unlinked stop actor cannot expand that scope. Cancellation closes unstarted actions and preserves uncertain started effects, original proposals, review decisions, and artifacts.

Cleanup uses the existing source worker and progress refresh job. One typed queue timing helper pulls waiting jobs forward or defers them, replacing the duplicated queue SQL. A paused routine can finish notice cleanup without regaining execution authority. Cleanup does not call the model or replay an action. Wake-up runs after cancellation commits: a queue failure remains retryable but cannot restore execution authority.

The existing progress card reads the canonical operation outcome. Ended notices retain a read-only history link and remove voting controls. A started action says its outcome is uncertain; an unstarted action says it was not completed. The typed pending/ended notice view adds no stored state, worker, table, or migration.

Failing-first regressions exposed missing immediate wake-up, delayed refresh, cancellation rollback on enqueue failure, and misleading claimed-action messaging. All eleven focused Slack review lifecycle examples now pass, covering decisions, expiry, recovery, signed stop, pause, cancel, stop during notice publication, a claimed action, repeated controls, unknown actors, injected queue failure, unchanged model-call count, and no duplicate mutation. The final source passes all 409 bot, 76 API, and 41 incident-delivery examples. Formatting and diff checks pass; HLint has no hints on changed production lines. Repeated scoped distillation, constraint, and client-behavior reviews found no material remaining issue. Whole-branch reviews and release gates remain pending.

Queued web, slash, and paired native PR-analysis continuation are implemented above. Native fix/admission/peer writes now share the run and operation ledger. Broader crash recovery, streaming, and controlled live Slack acceptance remain open. The hosted visual review includes these working and pending paths with exact source from `0a19031b5`.

## Routine pause and stop while awaiting review

Routine pause and stop now cancel running requests before returning. The routine update and shared run finalizer commit together. Unstarted actions receive the existing request-ended refusal; already-started effects retain an uncertain outcome. Original proposals, review receipts, and artifact snapshots remain available for audit.

Resume enables future work. It cannot revive the cancelled request or let its retained context execute another tool. The existing schedule-recovery sweep releases the completed slot and admits a fresh scheduled request; no cancellation worker, migration, or separate operation cleanup was added.

All twelve failing-first cases now pass through real routine handlers. They cover scheduled and issue-enrichment requests, pause and stop, pending/approved/started actions, repeated controls, preserved decisions, rejected stale execution, and fresh scheduled work. An injected operation-closure failure proves that pause rolls back with the run transition. Recovery fixtures seed historical paused-but-running rows directly because the current pause handler correctly closes them.

The final code passes 404 bot, 76 API, and 41 incident-delivery examples. Formatting and diff checks pass; HLint reports no hints in the changed production module. Repeated scoped distillation, constraint, and client-behavior reviews found no remaining material issue in this slice.

Signed Slack stop and notice cleanup are implemented above. Other source continuations, native model-session continuation, live acceptance, streaming, and final branch gates remain open. This checkpoint establishes routine-control cancellation, not completion of every waiting lifecycle.

## Initial Slack transcript visibility

Initial Slack requests now receive scoped readers before any history or remote evidence is fetched. Direct messages require their retained author and current project membership. Channel conversations require current linked Slack membership. Direct access and navigation share one query fragment and the existing typed audience evaluator. Signed receipt provenance takes precedence over conversation metadata. Missing or conflicting provenance remains restricted, including after conversation deletion and recreation.

An owner's instruction can still make ordinary autonomous project changes. Fetched private evidence retains the existing publication restrictions. The Slack DM peer check now lives alongside channel membership checks in the shared evaluator and runs only when personal access requires it. This adds no table, migration, stored scope field, worker, or executor.

The initial failing regression reproduced an unrelated administrator passing the read check. Its parameterized form covers a DM and a private channel, one real dashboard creation, owner access to a saved document, denial of another administrator's transcript/document/title/run access, filtered navigation, conversation recreation, and membership loss. A further failing regression caught an older DM author being lost when no routine-run row existed; retained checkpoint and answer authors now participate in the same scope query.

The final code passes 392 bot, 76 API, and 41 incident-delivery examples. Affected fixtures now use valid Slack channel identities and explicit membership responses. Formatting and diff checks pass. Scoped distillation, constraint, and client-behavior reviews found no remaining material findings. HLint's remaining suggestions refer to unchanged expressions, including fixture lines whose IDs or indentation changed; they were left alone to keep this change focused. These are deterministic integration checks, not controlled live Slack acceptance. Waiting cancellation, remaining continuations, streaming, and final branch gates remain open.

## Slack MCP integration checkpoint

The remote client now discovers Slack's four supported read operations through the shared capability catalog. It uses the requester's encrypted grant, validates initialization and response identities, releases provider sessions, and checks the advertised schema again before each read. Provider write tools remain excluded even if they claim to be read-only.

The common dispatcher applies personal or verified current-channel scope before a read and retains the result with its requester and arguments through existing tool evidence. Channel reads override the destination from the signed context and reject unrecognized locator arguments. Workspace-wide search requires the requester's personal DM or web context. Membership loss, disconnect, or schema changes cannot expose a new result to the model.

Slack discovery is optional: credential rejection, app-access denial, server failures, and throttling leave core project work available. A requested Slack read during cooldown uses the existing retry deadline without another provider request. This slice adds no migration, ledger, executor, scheduler, or rate-limit table.

The discovery regression first failed because Slack reads were absent from the common catalog. The availability regression then failed on both throttling and server errors. The final source passes all 329 bot, 76 API, and 38 incident-delivery examples. Formatting and diff checks pass; HLint has no suggestions on changed lines. Repeated scoped distillation, constraint, and client-behavior reviews found no remaining material issue.

These are deterministic integration checks against a protocol fixture, not live Slack acceptance. Actual app eligibility, provider schemas, Slack Connect behavior, streaming acceptance, remaining review continuations, and final branch release gates remain open.

The [visual review](https://chatgpt.com/space/page_9c75b6b56b0481918e666edd7a89a919) shows source snapshot `0a19031b5`. Its twelve walkthroughs cover the shared conversation, Slack, authority, memory, review, and recovery paths, including paired native PR analysis and native fixes. All 108 source excerpts come from their recorded Git files and line positions. Chrome verifies each step and exact code display, keyboard navigation, collapsed comparisons, widths 390/768/1280, and all six native-fix steps with expanded code at 390/768. Text contrast ranges from 7.6:1 to 16.3:1. The same Page, introduction, title, and snapshot date are retained.

The uploaded HTML is 179,487 bytes, SHA-256 `616bff6b7b94b7d711cad4ea82a85c4174b2674f897c66726ea699a6f1bd7684`. Page sequence 47 references this visualization; readback confirms the preserved blocks and file size. The final source passes all 43 native and 450 bot integration cases. The unchanged SDK worker suite previously passed all 19 cases. Current fix-panel browser checks cover both viewer roles and both themes at widths 390/1280 using real handler HTML and project assets with fixture HTTP transport. Earlier API and incident-delivery checks passed 76 and 41 cases. Broader native fix interruption recovery, streaming, live Slack acceptance, whole-branch reviews, and final release checks remain open.

## Selective review ledger foundation

The existing operation record now separates an unstarted action from an action whose execution outcome is uncertain. A review request and its eventual receipt stay on that record. Waiting returns an operation ID rather than an execution claim. The common atomic and external-operation interpreters retain waiting work before any mutation; the model/native boundary can suspend without advancing the pending tool checkpoint.

One decision function checks the reviewer's current project role, the capability's minimum role, and the proposal's evidence audience. It locks the operation so concurrent and repeated decisions retain one receipt. A retry cannot replace the original request or extend its deadline. Rejected or expired requests retain a typed refusal without running the mutation; an approval that expires before execution also cannot run it. Model tools render that reason as a tool result. Native operations return the refusal through their existing error contract, without treating it as a malformed native response.

The first regression failed because the real artifact writer executed before review. Twelve focused cases now pass: nine ledger cases cover approval, rejection, expiry before review, expiry after approval, reviewer downgrade, administrator-only review, private evidence, an already-expired proposal, and concurrent decisions. They also verify wrong-project decisions and retries that try to remove review, lower the approver requirement, or extend the deadline. Three native issue-update cases verify a real approved mutation, rejection, and expiry, including repeated calls and retained response contracts. The final runtime passes all 341 bot examples. The preceding revision also passed 33 native review/SDK examples; the final native approval/refusal paths pass three focused integration cases. The same final source passes 76 API and 38 incident-delivery examples. Formatting, diff checks, and scoped HLint pass with no hints on changed lines. Repeated scoped distillation, constraint, and client-behavior reviews found no remaining material issue in this foundation.

Migration `0275_agent_operation_review.sql` adds one review field to the existing ledger; it adds no table or worker. The run inspector shows waiting state and retained review metadata. Observe and Autonomous preserve their existing behavior; selected review now uses this foundation through the shared interpreter. Routine configuration, web/Slack decision controls, expiry/resume scheduling, pause and actor-revocation acceptance still need connection. This foundation does not establish a working selective-review mode.

## Selected-review policy backend

`ExecutionPolicy` now represents Observe, Autonomous, or selected review with a nonempty collection of action classes and a reviewer requirement. Existing JSON values for Observe and Autonomous remain unchanged. One catalog classification maps capabilities to knowledge changes, project changes, external posts, or administrative changes. Ordinary reads and conversation-result storage do not require review.

The shared interpreter derives review requirements from the same effective policy snapshot used for authorization. Selected operations wait before effects; unselected operations stay automatic. Approval resumes the stored tool checkpoint without repeating inference or the completed mutation. Review receipts retain the reviewer's authority at decision time. Execution checks both that proof and current membership, so a later role upgrade cannot strengthen an earlier vote and revoked approval authority cannot execute a write.

Migration `0276_agent_review_policy.sql` stores policies as JSONB and adds a monotonic policy limit to the existing run. Original grants remain immutable. Policy edits constrain current work; later relaxation does not expand an older run. One database meet function supplies both limit retention and current policy evaluation. Review request and decision immutability share this migration. No table, executor, or scheduler is added.

The policy-selection regression first failed because a selected dashboard action executed immediately. Eighteen focused review cases now pass, including selected/unselected execution, approval and checkpoint resumption, policy tightening and relaxation, invalid storage, original-grant rewrite rejection, and reviewer authority changes. The final source passes all 347 bot, 76 API, and 38 incident-delivery examples. Policy codec doctests are added; their final suite still needs to run. Formatting and diff checks pass; scoped HLint has no suggestions on changed lines. Repeated scoped distillation, constraint, and client-behavior reviews found no material issue in this backend slice; human controls and waiting lifecycle remain required.

The database policy column changes from text to JSONB. Deploy the updated server and schema together; older SQL decoders expect text. This is a rollout requirement, not deployment authorization. Human configuration controls, web/Slack decision controls, and durable waiting/resume lifecycle remain unfinished. Autonomous remains the default.


## Scheduled review suspension

A scheduled worker now returns normally when the shared interpreter retains an action for review. It leaves the run and tool checkpoint open instead of writing a failure or advancing the schedule. The existing operation record is the waiting state; no additional queue, worker, table, or migration is introduced.

One lease predicate is shared by scheduled admission and recovery. Pending, unexpired review blocks admission even after the ordinary lease becomes stale. A decision or expiry makes the existing scheduled slot recoverable. The claim moves the lease beyond the operation creation time, so duplicate jobs cannot consume that wake-up again. Claim timestamps use the database wall clock after lock acquisition.

The failing-first regression reproduced `RunFailed` on review suspension. Three focused cases now pass for approval, rejection, and expiry. They exercise the real dashboard tool, retained model checkpoint, recovery discovery, duplicate admission before execution, unchanged run identity, exactly two model calls, and at most one mutation. All 350 bot examples pass against the final source. Both watchers compile successfully. Formatting and diff checks pass; scoped HLint reports no suggestions on changed lines. Repeated scoped distillation, constraint, and client-behavior reviews found no remaining material issue in this scheduled-worker slice.

This connects scheduled work only. Scheduled web decisions now enqueue continuation immediately, as described below. Event, Slack, and web suspension, Slack decision controls, policy configuration, and the user-facing waiting summary remain unfinished. The existing recovery sweep also discovers ready scheduled work.


## Scheduled web decisions

The run inspector now renders Approve and Decline forms for waiting scheduled actions. The POST endpoint uses the shared decision evaluator and queue helper. Reviewer permission floors live in the canonical catalog and are reused by the evaluator and UI. The evaluator rechecks current membership and evidence access; hiding a form is not authorization.

The shared human-review entry point now commits the decision and typed continuation in one database transaction. It pulls an existing queued/retry job forward, or inserts the existing job type. Queue rejection rolls back the vote; repeat delivery retains the original decision. Scheduled work uses `AIRoutineRun`; Slack messages use `ProcessSlackEvent`. Expired reviews can also wake the existing worker to retain their refusal. No migration, second executor, or approval worker is added.

The immediate-continuation regression first failed with zero queued jobs after both approval and rejection. Three focused integration cases now pass through the actual web handler. They cover real dashboard execution, repeat submissions, reader denial and hidden controls, an injected queue failure followed by rollback and retry, expiry, and unchanged checkpoint/run identity. The production code passes all 350 bot and 76 API examples. The final focused cases also verify that expired control submissions leave review metadata unchanged and enqueue only refusal processing. Both watchers compile successfully; formatting and diff checks pass. Scoped HLint has no suggestions on changed lines. Repeated scoped distillation, constraint, and client-behavior reviews found no material remaining issue in this slice. This endpoint does not yet establish complete selective review: Slack controls, policy selection, and suspension/resume for other sources remain unfinished.


## Slack review controls and suspension

Slack investigations now suspend on the same retained operation as scheduled work. A typed journal event distinguishes waiting from failure or interruption. The existing progress message renders a pending step, a link to the stored proposal, and Approve/Decline controls. Rendering and action parsing share the typed decision codec. The [official Slack task-card type](https://docs.slack.dev/tools/node-slack-sdk/reference/types/interfaces/TaskCardBlock/) includes the pending status used in the existing plan block.

Signed action callbacks derive the project from the operation and current workspace installation. The clicking user must have a current linked identity, project membership, reviewer role, and evidence access. The same evaluator and transactional queue entry point serve web and Slack. The proposal page uses the existing run inspector and its authorization. No decision capability enters the model's tool catalog.

A waiting receipt remains unprocessed while the worker and thread lock return. Its existing job wakes at expiry. An unconfirmed notice retries the receipt within a minute, reusing the progress reservation and reconciliation path. Approval pulls the existing continuation forward. Pending-review deferral locks the operation and cannot postpone an already-approved job. Normal review continuation retains the same model checkpoint, run, and action identity.

Failing-first regressions reproduced review being treated as a failed Slack investigation, a vote surviving a rejected enqueue, and an unconfirmed notice waiting until the review deadline. Five focused Slack cases now pass: approval, rejection, expiry, approval while the notice is publishing, and a definite publication rejection followed by retry. They cover signed bytes, wrong-workspace and unlinked-user denial, repeat clicks and receipt processing, real proposal inspection, unchanged run identity, exactly two model calls, and at most one dashboard mutation. The scheduled rollback cases also pass. The broader run reproduced a missing final progress update on asynchronous cancellation. The shared journal now uses the repository’s exception-cleanup combinator; the unchanged signed-stop regression passes. All 23 focused review examples pass. The final source passes all 355 bot, 76 API, and 38 incident-delivery examples. Policy/control codec doctests and final release checks remain pending. Formatting and diff checks pass; scoped HLint has no findings on changed lines. Repeated scoped distillation, constraint, and client-behavior reviews found no material remaining issue in this slice.

Policy configuration, suspension for web/slash/native sources, waiting stop/pause acceptance and live Slack acceptance remain unfinished. This slice does not establish complete selected review or completion of the unified plan.


## Review recovery across failed attempts

Proposal links, review decisions, and continuation jobs now share one request-scoped lookup. It retains the proposal's original attempt while selecting the latest admitted attempt in the same project, conversation, and request root. A new vote requires a live attempt whose requester still matches the stored proposal. Completed proposal links resolve to the latest result; the inspector still links the original attempt for audit.

The lookup uses the existing project/request-root index. No migration, queue, or executor is added. Failed-attempt recovery initially returned the old run ID from the proposal link. Six signed Slack scenarios now pass, including failure while waiting, denial before retry admission, recovery without new inference, original-actor/proposal retention, one real dashboard mutation, and completed-result inspection. The final source passes all 356 bot, 76 API, and 38 incident-delivery examples. Both watchers compile successfully. Formatting and diff checks pass; scoped HLint reports no findings on changed lines. Repeated scoped distillation, constraint, and client-behavior reviews found no material remaining issue. Final branch checks and the other unfinished plan items remain pending.


## Native issue-enrichment review

Issue enrichment now returns normally while its retained metadata update awaits review. Scheduled, Slack, and enrichment workers use the same transactional scheduling function. A private wake type distinguishes bringing reviewed work forward from deferring a still-pending proposal; a deferred job cannot postpone a recorded decision. All three use existing job constructors, the request-scoped operation lookup, and the shared evaluator/executor. The run inspector offers the same review controls for enrichment.

The periodic scanner skips terminal enrichment requests and unexpired review waits. It still discovers an ordinary analysis interrupted before a proposal was stored. Source equality and request-root joins use existing indexes. A busy enrichment lease requeues its existing job instead of consuming the continuation. Metadata reports distinguish analysis-only from applied results independently of execution policy; authorization retains the real effective policy.

The new integration cases first failed on uncaught suspension. Further checks reproduced scanner starvation of interrupted analysis and loss of a continuation while its real advisory lock was held. Approval, rejection, and expiry now pass through the actual worker and web controls. For the target enrichment, repeated waiting/resume calls retain the original three inference results. Only approval changes metadata. The same cases cover fresh-issue discovery, interrupted-analysis rediscovery, busy-lease retry, repeated completion, and terminal suppression. All 27 focused review examples pass. The final source passes 359 bot, 35 error-pattern, 76 API, and 38 incident-delivery examples. Formatting and diff checks pass; scoped HLint has no findings on changed lines. Repeated scoped distillation, constraint, and client-behavior reviews found no material remaining issue in this slice.

No migration, new worker type, or executor is added. Policy configuration, waiting stop/pause acceptance, other native/web/slash worker paths, controlled live acceptance, and final branch checks remain unfinished.

## PR publication review

PR publication now suspends through the same operation ledger and continuation scheduler. Its completed analysis remains in the existing review result. A decision resumes that result without starting another model attempt. A publication refusal has a distinct internal outcome and cancels the shared run instead of reporting an execution failure. Web controls use the same reviewer evaluator.

The native review interpreter preserves typed review suspension from its callbacks. The suspension type lives beside shared operation types and remains re-exported by the agent runtime. No new effect, executor, worker, migration, or queue is introduced.

Failing-first cases reproduced publication being classified as a failed review and typed callback suspension becoming a generic execution error. A further regression reproduced loss of the continuation while the same native worker lease was held. That lease now requeues the existing job. Seven focused native review examples pass, including the three enrichment cases. The full impact-review run passed 36 examples, including real SDK/VM admission, fixes, calibration, screenshots, evaluation, native artifact recovery, and application jobs. Its remaining queue assertion assumed busy same-run work disappears. The updated case now explicitly requires that retry while checking webhook deduplication and newer-revision isolation; it passes separately. The current code also passes 359 bot, 76 API, and 38 incident-delivery examples. Formatting and diff checks pass; scoped HLint has no findings on changed lines. Repeated scoped distillation, constraint, and client-behavior reviews found no remaining material issue in this slice. Mid-analysis wiki review still needs durable model-session continuation; preserving its typed suspension alone does not prove that lifecycle. This does not establish complete selected review for every source or live Slack acceptance.

## Incident review continuation

Incident investigations now suspend on the shared operation record and use the same continuation scheduler as scheduled, Slack, enrichment, and retained PR-publication work. Web decisions enqueue the existing incident job. Pending review leaves the run and checkpoint open; exception cleanup still closes interrupted workers. No worker, table, migration, or executor is added.

Approval, rejection, and expiry first failed because suspension became an investigation failure and unconditional cleanup closed the run. All 41 incident-delivery examples now pass. The three new cases verify real web controls, one queued continuation, no report or repeated inference while waiting, the same run and attempt after continuation, one published report, and exactly one permitted dashboard mutation only after approval. Formatting and diff checks pass, and scoped HLint has no findings on changed lines. Repeated scoped distillation, constraint, and client-behavior reviews found no material remaining issue in this slice. Configuration, waiting stop/pause cleanup, other entry points, native model-session continuation, live acceptance, and final release checks remain unfinished.

## Terminal request cleanup

The shared run finalizer now closes unstarted actions when their request ends. An exhaustive Haskell match distinguishes terminal scheduled/interactive work from failed or timed-out event attempts that can recover. SQL applies the decision to the same project, conversation, and request root, including proposals retained on older attempts. Running or uncertain effects keep their separate uncertainty path. Proposals and review receipts remain unchanged.

A typed refusal records that the request ended before execution. The run inspector shows its reason instead of leaving the action labelled as awaiting review. No migration, worker, or store is added. Thirty failing-first cases cover three run sources, five outcomes, and both pending and approved-but-unexecuted actions. They also cover a later event attempt closing an earlier retained proposal. All 30 cases and the full 389-example bot suite pass. The same code passes 76 API, 41 incident-delivery, 35 error-pattern, and seven focused native-review examples. Formatting and diff checks pass; scoped HLint has no findings on changed lines. Repeated scoped distillation, constraint, and client-behavior reviews found no material remaining issue in this slice. Waiting pause/stop semantics and the other unfinished plan items remain open.

## Review state display and browser acceptance

An unstarted action now displays its actual review state: not started, awaiting review, approved, or declined. Fifteen failing-first cases reproduced approved actions still being labelled as awaiting review. The 30 lifecycle cases pass with the corrected display, including all three run sources. The restored permanent fixture passes all 30 cases after browser acceptance. Formatting and diff checks pass; scoped HLint has no findings on changed lines. Repeated scoped distillation, constraint, and client-behavior reviews found no material remaining issue in this display change.

Chrome exercised the authenticated application through the existing `withLiveServer` helper and an isolated test database. At widths 390 and 1280, both light and dark themes displayed the controls and terminal reason without horizontal overflow or browser errors. Keyboard approval reached the real POST handler, retained the decision, and removed the approval controls after refresh. Cancellation then displayed the typed request-ended reason. Eight screenshots and the successful local acceptance log are retained under `/tmp/review-lifecycle-*`; the temporary browser fixture is removed from the test suite.

This checks the review lifecycle UI, not complete policy configuration or live Slack. The browser run also confirmed that the existing two-choice settings form renders Observe for a stored selected-review policy. The typed policy configuration below resolves that display and editing gap; the other open plan items remain unfinished.

## Typed policy configuration

The routine form now displays and saves Observe, Autonomous, and selected review through the existing execution-policy type. Selected review exposes the four canonical action classes and the required reviewer role. Native CSS shows those controls only in review mode. Read-only views use the same class and reviewer labels. Existing Observe/Autonomous and JSON-policy form submissions remain supported.

The form decoder constructs a nonempty selection or an explicit missing-selection form state. The handler renders that state through the existing error toast and leaves the stored policy unchanged. Unknown classes, unknown reviewers, and duplicate policy fields remain parse errors. The executor receives the same precise domain policy; no separate configuration model, client script, table, or migration is added.

The first regression reproduced a stored selected policy being displayed as Observe. Handler integration now covers both reviewer levels, every class, both legacy modes, and unchanged policy after an empty selection. Chrome verifies the stored selection, conditional controls, saves and reloads, both themes at widths 390/1280, and visible error feedback for an empty selection. The browser check exposed the generic parser error hiding that feedback; the typed form state resolves it. The permanent 390-example bot suite passes after removing the temporary browser fixture. The same source passes 76 API and 41 incident-delivery examples. Form-codec doctests are added; their final suite remains pending. Formatting and diff checks pass; scoped HLint has no findings on changed lines. Repeated scoped distillation, constraint, and client-behavior reviews found no material remaining issue in this slice. Remaining worker continuations, waiting pause/stop behavior, initial-DM privacy acceptance, native model-session continuation, live Slack acceptance, streaming, and final release checks remain open.

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
Slack's [official search reference](https://github.com/slackapi/slack-skills-plugin/blob/main/skills/slack-search/SKILL.md) confirms `slack_search_public`, `slack_search_public_and_private`, `slack_read_channel`, and `slack_read_thread`. Their full input/output schemas and this app's live eligibility still need verification; these names alone do not establish a working integration.

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

## Implementation evidence

The [visual review](../docs/reviews/2026-10-30-unified-agent.html) shows the architecture, request flows, source excerpts, and remaining work.
Each source panel identifies its reviewed commit, including shared completion and typed capability policies.
The review now uses one main diagram, a guided request walkthrough, and collapsed code comparisons.
Text contrast exceeds 7:1 for the tested color pairs. The layout passes browser checks at 390, 768, and 1280 pixels.
The [hosted review](https://chatgpt.com/space/page_9c75b6b56b0481918e666edd7a89a919) now includes seven scenarios, including automatic incident investigation and closing reports, at source snapshot `00a2da3aa`. All scenarios, four code comparisons, nine roadmap items, keyboard controls, and responsive checks pass. Hosted HTML readback matches the local artifact exactly.

The related PR runtime integration passes 33 review integration examples and fifteen worker examples.
Operation names and agent permission policies now live together in the API tool catalog.
MCP read hints, schema discovery, and agent execution use each tool's policy.
The separate name-indexed agent policy registry has been removed.
Twenty MCP examples pass, including POST-read annotations and privileged reads.
The agent regression also denies an unclassified operation with a previously known name.
Fifteen duplicate resource-read schemas, their fallback dispatch, and their response formatters have been removed.
Discord and WhatsApp now receive the host-supplied catalog through a viewer-read projection.
Their project integrations do not gain administrative reads or writes without a linked member.
The API issue response includes typed activity records so agent issue reads retain their history.
Resource reads use canonical API pagination and response shapes.
Query-only contexts retain the smaller telemetry tool set without a second resource API.
Bounded model results preserve both ends of large evidence, including the latest investigation status.
API tools and 23 typed agent-only operations now use one descriptor structure and one model adapter.
The separate OpenAI schema builders no longer exist.
Discovery, schema access, and dispatch use the same catalog.
A common role gate applies before each operation. Conversation documents retain their existing write behavior in Observe mode.
An unclassified API entry blocks a local operation with the same name.
All 151 bot integration examples and all 75 API v1 examples pass after this consolidation.
Operation retry metadata and recoverable action execution remain incomplete. Slice 1 is not complete.

Slack investigations, event runs, and scheduled runs now use the same checkpoint store and executor.
Each scheduled invocation carries its run ID rather than its installation ID.
Event input and its first checkpoint commit in one transaction under the run lock.
Checkpoint initialization verifies the project, conversation, and requesting member against the run.
Retries reuse completed reads and the original model request.
Scheduled runs now use the same active-run and membership checks as event runs.
The regression first demonstrated a repeated project read after interruption.
Both event and scheduled recovery cases now pass, including conversation isolation and a single retained user message.
All 153 bot integration examples pass, including existing Slack checkpoint rollback and conflict cases.
All 75 API v1 integration examples also pass after the checkpoint migration.
General web conversations now use the shared run ledger and checkpoint executor.
Both web forms submit a request ID. Legacy clients receive a new ID for each submission.
A run retains its input, origin, authorizing member, and typed Observe or Autonomous policy.
Retries cannot change that identity or increase its policy.
The common role gate still applies to each tool.

A new request creates its conversation and optional routine in one transaction.
Routine creation reuses the existing routine SQL and retains the defining instructions.
Retries do not change the routine or restore a deleted conversation.
The normalized assistant message and run completion also commit together.
These transactions reuse existing routine, completion, and result-message SQL.
The unused web action wrapper no longer exists.

The web regression first demonstrated a repeated project read after interruption.
Additional failing cases exposed routine reactivation and conversation resurrection during retry.
Three handler regressions now cover existing conversations, new conversations, and new routines.
They cover completion rollback, retained reads, one answer, immutable input and policy, pause, deletion, and revoked access.
All 156 bot integration examples and all 75 API v1 examples pass after these changes.
New interval JSON doctests remain part of the final doctest verification.

Issue chat and its global conversation route now retain the same request identity.
Both routes use the shared checkpoint executor and completion function in `Pkg.AI`.
The issue adapter retains its issue prompt and read-only policy.
Its separate answer parser and message-save path no longer exist.
The panel renders canonical stored history and returns a fresh form after success.
A failed request retains its current form and request identity.

Five handler regressions cover general chat, new conversations, new routines, issue chat, and the issue conversation route.
The issue cases first failed because retries repeated a completed project read.
The regressions also cover completion rollback, issue context, fresh form identities, deletion, and revoked access.
All 158 bot examples, all 45 issue examples, and all 75 API v1 examples pass.
The focused doctest attempt did not execute examples because the local package database lacks the `monoscope` library registration.
Final doctest verification remains pending.
Web, issue chat, and scheduled results now share `completeRun` and one answer normalizer.
Typed completion outcomes distinguish publication, quiet evidence, failure, timeout, and cancellation.
The old interactive completion, scheduler cleanup, cancellation query, and response-target wrapper have been removed.
Scheduled publication, retained report, run status, artifact snapshots, and lease advancement commit in one transaction.
A failed database write rolls them all back; recovery reuses the checkpointed model answer.
Lease checks reject an older worker before it writes any result.
Cancellation, pause, and withdrawn owner access suppress publication at completion.
Scheduled recovery retains the original authorizing member, instructions, and typed execution policy.
Increasing installation authority affects future runs; it does not elevate an in-flight run.

The regression first exposed publication and scheduler advancement after a failed run-status write.
Six completion cases cover run-status failure, scheduler-lease failure, stale workers, cancellation, pause, and revoked access.
All 163 bot examples, all 45 issue examples, and all 75 API v1 examples pass after this consolidation.
The formatter and diff checks pass. Scope reviews found no remaining material distillation, type-evasion, or client-behavior issue in this slice.
Event-specific completion and legacy stranded leases still require consolidation and recovery checks.
This transaction does not make external actions or project mutations exactly-once.

The catalog now supplies one interpreter for minimum roles, project writes, and wiki write grants.
Typed denial reasons are rendered at the dispatcher boundary; the wiki handler no longer checks roles separately.
The policy sum describes reads, conversation evidence, project knowledge, project changes, external publication, and administration.
Read capabilities can require a privileged role. Write capabilities have intrinsic editor or administrator minimum roles.
The old independent role/action record and separate action enum no longer exist.
A reader-level project-write descriptor cannot be constructed.
The boolean `actionsAllowed` predicate and its discovery-specific member exceptions have been removed.
Service invocations retain viewer reads and conversation evidence, but cannot acquire administrator reads or project-write authority.
Every interactive, scheduled, and event tool boundary binds the current member and conversation to the stored run.
The scoped background check reuses the same SQL as cancellation and completion checks.

Seven new regressions first exposed service-role bypass and missing actor or conversation binding.
They now pass, including valid reads after each rejected mismatch and ordinary service reads after denied privileged access.
All 170 bot examples, all 75 API examples, and all 45 issue examples pass with the final policy sum.
The formatter and diff checks pass. The repeated scope reviews found no material distillation, type-evasion, or client-behavior finding.
The two new policy-floor doctests remain part of final doctest verification.
The shared evaluator still interprets legacy routine action and wiki-grant fields.
Persisted installation policies, conservative adoption, default autonomous activation, and selected review remain unfinished.
Administrator writes retain the existing unavailable behavior until operation recovery is implemented; no new approval prompts were added.
A checkpoint does not establish exactly-once mutation or external publication.
Operation identities and safe write recovery remain necessary before autonomous writes reach release.


### Durable operation recovery (current implementation slice)

Mutating capabilities now use one typed operation ledger under the existing run.
A pending checkpoint supplies the stable operation position; the model does not supply its identity.
The proposal retains the original tool call, authorizing role, execution policy, and capability policy.
Current authority is checked again before accessing a retained result or starting a change.
The shared executor distinguishes a retained response from an uncertain outcome in its result type.
An uncertain outcome ends the model turn and suppresses subsequent mutations.
Identical function-and-argument mutations within one source request reuse the original response, even when the model changes the tool-call ID.
A new user request can intentionally perform the same operation again.

Artifact writes compose their existing transaction with the operation claim and outcome.
A failed outcome write rolls back the artifact change and claim together.
Opaque API and external writes retain the claim before execution and the response afterward.
Recovery does not repeat a claim whose response is missing; a legitimate late response can improve the retained record.
This is an explicit uncertain result, not a claim of universal exactly-once execution.
Slack publication retains the native response and configured channel without retaining authorization headers.
Tool schema discovery derives the recovery contract from the actual execution representation.
There is no second retry-policy registry or approval executor.

Run details show action status and bounded proposal/outcome previews using native details and HTMX refresh.
Preview disclosure checks the viewer's current role against the catalog policy.
Older chat findings and tool-result views still need a complete disclosure audit before sensitive administrator operations are enabled.
Legacy calls without a durable cursor remain a compatibility path and must be retired before broad autonomous activation.
Event attempt/root continuity, indexed root lookup, and cross-attempt concurrency still require explicit integration before event writes are enabled.
Run completion converts outstanding operation records to uncertain in the same fenced SQL statement. A stale worker cannot change an operation; an original worker can still retain a legitimate late response. No cleanup worker or additional migration is required.

One migration, `0261_agent_operations.sql`, contains this slice's operation schema.
Previously applied migration files remain unchanged because migration history is checksummed.
Nine new handler/DB regressions cover checkpoint failure, uncertain responses, repeated model proposals, atomic rollback, Slack publication, concurrent recovery, terminal run state, and recovery discovery.
All nine recovery regressions pass. The expanded focused suites pass after the terminal-state fix: 179 bot examples, 75 API v1 examples, and 45 issue examples. The timeout regression first failed on a retained OperationRunning state, then passed with the shared completion change.
A real Chrome test against the current WAI application and an isolated test database passes.
It covers uncertain-to-retained outcome refresh, a current-role downgrade, unchanged conversation URL, and preserved follow-up draft.
The test first exposed stale disclosure after refresh; disabling link prefetch fixes the issue without extra browser code.
The permanent end-to-end regression remains in `e2e/tests/routines.spec.ts`; the temporary browser-launch bridge was removed from the integration suite.
Slack recovery tests use the HTTP interpreter, not a controlled live workspace.
No Slack connector is exposed in this session, so live Slack acceptance remains unverified.
The formatter and diff checks pass. Repeated slice reviews found no remaining material distillation, type-evasion, or client-behavior findings.
Final whole-branch review, doctests, release signoff, and PR update remain pending.


### Persisted routine authority (current implementation slice)

Scheduled installations now have one typed `ExecutionPolicy` shared with their run snapshots, form decoder, database codec, and operation audit.
The legacy `allow_actions` and `memory_granted_by` columns, wiki-specific grant functions, denial reason, and boolean action wrapper are removed.
Project knowledge writes use the same Autonomous policy and current role check as other project changes.
Destination changes only select delivery; they do not grant authority or transfer the authorizing member.

New scheduled activations default to Autonomous, with disclosure in the routine catalog, schedule control, and new-routine composer.
Existing installations migrate to Observe, including previously enabled legacy action grants, until an editor explicitly adopts the unified policy.
Rescheduling and resuming preserve the stored policy.
The conversation has a typed Observe/Autonomous control guarded by current editor membership.
Saving it updates in place with a success toast and preserves the follow-up draft.

An authority revision fences withdrawn grants without rewriting the run's original policy or actor.
Reducing Autonomous to Observe advances that revision. Changing the authorizing member also advances it.
Runs retain the revision they started with, including through scheduler recovery.
Restoring Autonomous does not restore writes to an already restricted run; future runs capture the new authority.
The active-run and completion predicates reuse the same member-scoped policy query.

Eight focused authority cases pass, including automatic dashboard creation through the real shared operation, policy reduction/restoration, future-run authority, reader denial, pause, project knowledge, and actor/conversation binding. Rescheduling or resuming the active routine cannot restore withdrawn writes.
A standalone migration check passed on an isolated old-schema clone with both legacy Observe and legacy action-enabled routines.
It verifies conservative adoption, retained historical run policy, old-run restriction, removed legacy columns, and the new Autonomous default.
The browser regression first reproduced draft loss on policy save, then passed after the handler returned an in-place result.
The permanent browser case also retains the earlier action-history refresh and role-restriction checks.
The final focused suites pass: 180 bot examples, 75 API v1 examples, and 45 issue examples. The formatter and diff checks pass. Repeated slice reviews found no material distillation, type-evasion, or client-behavior findings. The production Haskell change adds eight net lines, including the policy UI, while removing the separate grant paths. Whole-branch review, doctests, live Slack acceptance, release signoff, and PR update remain pending.

Event execution still uses its conservative Observe ceiling until stable request-root continuity and event authority are integrated.
Legacy invocation constructors, administrator capability enablement and disclosure, and selected action review remain unfinished.
This slice does not claim that the entire authority milestone is complete.


### Request roots and event recovery (31 October)

Each run now has a non-null `root_run_id` referencing the existing run ledger.
Interactive requests and scheduled instants start as their own roots.
An event recovery creates another attempt under the original request root.
An explicit restart creates a new root and captures the current instructions, input, and authority.
No request table, scheduler, or second checkpoint store was added.

Event initialization locks the installation only while creating or resolving the attempt.
Concurrent duplicate workers receive the same attempt. After failure, concurrent recovery workers receive one new attempt under the same root.
Recovery keeps the original actor, conversation, input, instructions, policy, and authority revision.
A different actor or conversation cannot silently take over the request.
The shared executor loads retained event input before constructing the model request.
It resolves run checkpoints through the scoped root lookup while retaining each attempt's separate history.

Operation identities follow the root checkpoint position; operation evidence still names the attempt that executed it.
Known responses are reused within the request. Unresolved actions from the same source also block blind repetition after an explicit restart.
Historical attempts migrate as their own roots, preserving existing checkpoint keys.
The root foreign key, non-null constraint, and root/source indexes are in one coherent migration, `0263_agent_request_roots.sql`.
Applied migrations remain unchanged.

The regression first exposed a repeated project read after a failed event attempt.
Recovery now reuses the original model request and completed reads across attempts.
A second case interrupts an artifact append after commit but before checkpoint advancement; recovery retains the single append and response.
The cases also verify original input despite changed worker arguments, unchanged instructions and actor, sticky policy restriction, and foreign actor/conversation rejection.
A concurrent initialization case verifies one initial attempt, one recovery attempt, and the shared root.
The existing explicit-restart regression verifies a fresh root and retained prior artifacts.

The focused suites pass: 183 bot examples, 75 API v1 examples, and 45 issue examples.
The native review publication/recovery/retry integration case also passes.
An isolated old-schema migration check verifies retained checkpoint identity, null/unknown root rejection, and project cleanup with child attempts.
Both permanent browser scenarios pass against the current WAI application and an isolated database: policy/action history and the full routine lifecycle, including mobile themes, deleted conversations, and repository settings. The longer lifecycle capture needed a 180-second local test budget; it completed in 78 seconds with no product-code change. The repository CI timeout remains unchanged. The temporary browser launcher was removed. The formatter and diff checks pass; repeated slice reviews found no material distillation, type-evasion, or client-behavior findings. Final whole-branch review, doctests, live Slack acceptance, release signoff, and PR update remain pending.

Event actions still use the conservative Observe ceiling until invocation authority, role/scope ceilings, and publication audience controls are complete.
Service recovery adapters must retain the request's original authorizing member when installation ownership changes.
The next slice addresses scheduled worker lease fencing. Sensitive history disclosure and native completion consolidation remain release work.
This slice establishes request continuity; it does not claim the full unified agent is complete.

### One stored invocation context and worker lease (31 October)

Web, issue chat, Slack commands, Slack events, and scheduled jobs now load the same opaque `RunContext` from the existing run ledger.
It retains the original actor, project, conversation, source, policy, input, instructions, request root, and worker start time.
The public type has read-only field access, with no exported constructor, record updates, generic reconstruction, or decoder.
Callers can no longer assemble an interactive, scheduled, or event invocation from independent IDs and policy values.
The legacy `InteractiveWithActions` constructor is removed; the remaining modes are the read-only compatibility adapter and a stored run.

One context check now guards model boundaries, tool execution, and answer publication.
Interactive claims cannot load a background run. Background claims must provide their actual start time.
The same worker start time is checked under the run-row lock when claiming a mutation.
A recovered scheduler worker can retain the same run ID and checkpoint without granting its predecessor a fresh lease.
Already claimed external actions may still return late and improve the operation evidence; recovery never blindly repeats them.

The scheduled-worker regression first reproduced a real dashboard creation by the old worker after lease recovery.
It now checks rejection of stale and wrongly classified claims, no old-worker mutation, rejection of a direct uncheckpointed write, and one dashboard from the recovered worker.
The actor/conversation matrix also reproduced five missing publication checks across interactive, scheduled, and event runs.
The common context guard now rejects all six combinations before publishing an assistant message.
The existing wiki conflict case uses a distinct stale proposal, since an identical proposal correctly reuses its retained response within a request.

Event actions retain the existing conservative Observe ceiling in the common authority evaluator, including historical Autonomous snapshots.
Fresh event runs persist that ceiling; recovery still retains the original snapshot.
Enabling event autonomy remains dependent on the planned role/scope and publication audience work.
At this snapshot, old read-only chat adapters could still save conversation documents without a run. The following slice removes that compatibility path.
Native adapter completion and sensitive history disclosure are also unfinished.

No migration or new Haskell module was needed for this slice.
Applied migration files remain immutable, including those already used in local development.
SQL consolidation uses shared query fragments and the existing run/checkpoint/operation tables instead of adding another ledger.

The final focused suites pass: 184 bot examples, 75 API v1 examples, and 45 issue examples. The native review publication/recovery/retry case passes. Both permanent Chrome scenarios pass against the current WAI application and an isolated test database, including the full routine lifecycle and the policy/action-history flow. They completed in 107 seconds under the existing 180-second local browser budget; the repository CI timeout is unchanged. The temporary browser-launch bridge was removed. Formatter and diff checks pass. Repeated slice reviews found and fixed the missing publication guard and redundant context reconstruction, with no remaining material slice findings. Whole-branch review, doctests, controlled live Slack acceptance, release signoff, and PR update remain pending.

### Durable document writes (31 October)

Every catalogued write, including conversation artifacts, now requires both a stored run context and a durable checkpoint position.
The last uncheckpointed `ConversationWrite` fallback is removed.
Read-only compatibility adapters retain document reads, but no longer advertise document writes or receive write instructions in their model prompt.
Stored Observe runs can still retain conversation evidence through the normal checkpoint and atomic operation transaction; no extra approval is introduced.

The regression first reproduced a document saved through a direct uncheckpointed tool call.
It now checks both the legacy adapter and a stored run without a checkpoint, verifies the unchanged artifact list, and checks discovery and prompt behavior.
Existing document append, capacity, truncation, and rendering tests now execute writes through the real shared loop.
Their fixture reads the full response from the operation ledger and requires exactly one returned operation per request.
The legacy Slack-only artifact restriction was removed from that fixture; authenticated Slack artifact recovery is covered by the event-artifact integration case.

No schema changes or new Haskell modules were required.
Native enrichment and review still have specialized mutation and completion paths outside this dispatcher; those remain the next consolidation boundary.
The trace identifies separate artifact, chat, and finish calls in Slack, enrichment, and impact review. Their replacement must preserve the distinction between a completed computation and confirmed transport delivery.
The suites pass: 185 bot examples, 75 API v1 examples, and 45 issue examples, plus the native review publication/recovery case. The final prompt distillation also passes all three document regressions. Formatter and diff checks pass. Repeated slice reviews found no remaining material distillation, type-evasion, or client-behavior findings. The previous slice's browser acceptance remains the latest browser run; final release checks and live Slack acceptance remain pending.

### One completion interpreter for durable runs (31 October)

Interactive chat, schedules, Slack commands, Slack investigations, issue enrichment, and impact review now call `completeRun` with the same opaque stored context.
The separate `CompletionTarget` constructors and public `finishRoutineRun` / `insertRunResultMessage` entry points are removed.
Member claims bind the actor and conversation; native event claims bind the event reference and worker start time and recover the stored actor.
Native model calls and progress reports also use the stored context instead of independently supplied project, run, and instruction values.

The result algebra separates success/no-findings from conversation publication/retention.
This preserves a published no-findings review, a quiet scheduled result, and a Slack answer already stored before delivery without inventing another conversation message.
Agent evidence contains the typed `AgenticChatResult`; its answer and tool JSON derive from that one value in the completion interpreter.
Native reports carry an optional native payload, preserving the absence of structured findings without a synthetic model response.
Failure notices distinguish run-only diagnostics from conversation notices.
A single typed admission query distinguishes allowed work, unavailable member access, and explicit stops; model boundaries and completion share that query.
Temporary access loss can retain a recoverable delivery failure, while cancellation, pause, and deletion stay cancelled.

Final report snapshots, conversation publication, terminal status, outstanding-operation uncertainty, and any schedule advance commit together.
Native delivery acknowledgements and pre-delivery evidence retain their own records.
A matched scheduled run whose installation disappeared closes as cancelled without publishing or advancing another schedule; an older worker cannot close a replacement lease.
The enrichment regression first exposed a report already visible after the final status write failed. It now rolls that publication back and retries to one report and one answer.
Additional failing-first regressions cover deleted installations and explicit stop/pause/delete becoming retryable failures.
The historical snapshot assertion now targets the run inspector, since the completed retry correctly publishes its newer answer in the current conversation. The browser scenario also checks that separation.

Native business mutations still need operation-ledger and policy admission, and native initialization still needs recovery through installation ownership changes.
Sensitive history disclosure, event action scopes, selected review, remaining Slack features, and controlled live workspace acceptance remain unfinished.
No schema migration or new Haskell module was required for this slice.
Verification: 190 bot, 75 API v1, and 45 issue integration examples pass. The eleven completion regressions pass after the final cleanup. Both Chrome scenarios pass against the current WAI application and isolated test database, including policy controls, action history, retained drafts, and run-inspector boundaries. The native review/SDK suite passed 33 examples during this slice; its publication/recovery regression passed again after the final admission change. Formatting and diff checks pass. Whole-branch review, live Slack acceptance, and release signoff remain pending.


### Native request recovery after owner changes (31 October)

A routine owner change used to strand a running native event: the recovery adapter supplied the new installer, while the saved request required its original member. This affected both issue enrichment and PR reviews.

- `EventRequest` carries a member for Slack messages and commands. Native review and enrichment request constructors take only their source identity; callers cannot substitute a new member during recovery.
- The existing initialization query derives a new native request's owner from the locked installation. Continued attempts retain the root request's actor, input, instructions, and authority version.
- Slack requests retain exact-member matching. Native recovery still checks both the current installer and the original actor's current access through the shared authority query.
- Explicit restarts create a new root with the current owner and instructions, even when the adapter holds an older installation snapshot.
- No new migration, alternate executor, or recovery table was added.

Both native-source regressions failed before the fix. They cover resuming a running attempt, retrying a failed attempt, retained request evidence, revoked original-member access, and an explicit restart. The focused regressions pass, followed by 192 bot examples, 33 native review/SDK examples, 75 API v1 examples, and 45 issue examples. Formatting and diff checks pass; repeated slice reviews find no material distillation, constraint-evasion, or client-behavior issues. Full-branch review, live Slack acceptance, and release signoff remain pending.

The next disclosure slice must cover the same evidence at every boundary: `AIThreads` currently gates operation previews, while the Findings/Tool results panels, run artifacts, conversation messages, and model history still use less specific access checks. Sensitive reads already have catalog role requirements, so blocking administrator mutations alone does not complete this work. Retained answers and documents derived from a sensitive read must carry that requirement too; hiding a tool preview alone is insufficient. Use one provenance representation and evaluator across model input, tool results, documents, and publication, including role changes during a run. Reuse the capability catalog for legacy evidence; do not introduce a second list of privileged tool names. Keep any required append-only schema changes together in one coherent migration.


### Shared evidence requirements and a pure capability catalog (31 October)

A handler regression reproduced a private administrator-derived answer remaining visible after the member became a reader. Additional failing regressions exposed copied native instructions losing their source restriction, a private installation being hidden from activation lookup, and an answer being returned after access changed during title generation.

- `Pkg.AgentCatalog` now owns the shared descriptor, API aliases and policies, local operation algebra and schemas, and composite operation algebra and schemas. MCP and the conversational executor interpret those same descriptors. Evidence classification uses that catalog; it has no separate list of privileged operation names.
- `disclosurePermission` separates the role required to perform a mutation from the audience of its result. Ordinary project mutations retain project-readable results. Restricted reads and administrative results retain their stronger audience.
- One append-only migration, `0264_agent_evidence_scope.sql`, stores requirements on conversations and retained runs. Database triggers preserve their strongest recorded requirement across updates, new attempts, deletion, and recreation. PostgreSQL's historical permission-enum order differs from the domain order; a shared rank function and exhaustive integration comparisons make that distinction explicit.
- A conversation's requirement rises before a restricted tool executes. Existing messages, model checkpoints, documents, run snapshots, titles, and future answers remain within that conversation requirement. This deliberately uses one conservative conversation scope rather than independent per-message rules.
- Model, tool, title, cached-answer, web-history, document, navigation, run-inspector, Slack delivery, wiki, GitHub review, and shared-issue publication boundaries use that requirement. Current role and delivery audience both apply. The existing project-view Slack audience does not acquire administrator evidence merely because the linked requester is an administrator.
- Historical tool metadata and checkpoints are parsed through the existing wire types. Copied routine-instruction dependencies propagate the strongest known role; malformed evidence, retired capabilities, and missing historical sources remain administrator-only. Classification retains already recorded restrictions.
- Native event initialization copies the installation's requirement along with its instructions and retains any stronger original-request requirement. A private existing installation is still found during activation, so hiding its presentation does not create an orphan conversation or cause a duplicate-key failure.
- Web sudo and public-demo behavior remain supported. Background project-member access still requires actual membership. Session reads and atomic interactive completion share the same permission query.
- `CompletionResult` now carries a `RunTermination`, whose constructors exclude running work. Callers distinguish an actually cancelled transaction from a published answer. Title generation checks access before and after its model call and before returning a result; the former exported single-caller title wrapper is removed.
- Trace logs retain tool names and result sizes, without raw arguments or response previews. Full evidence remains in the protected records.

The first handler failure, native instruction failure, activation failure, historical-instruction failure, and title-return failure were reproduced before their fixes. Ten focused disclosure regressions pass. They also cover role changes during inference, cached-answer and document access, late completion cancellation, navigation titles, project isolation, legacy unknown schemas, scope preservation, and ordinary denied reads that must not make a public conversation private. The broader suites pass 201 bot, 75 API v1, 45 issue, and 33 native review/SDK examples. All three Chrome scenarios pass against the current WAI application and an isolated database, including the new role-downgrade case. The temporary browser launcher is removed. Formatting and diff checks pass. Final whole-branch review, doctests, release signoff, and controlled live Slack acceptance remain pending.

A subsequent isolated PostgreSQL 16 benchmark reproduced the scope-update cost with 5,000 retained runs: the original trigger took 15.0 seconds. One indexed SQL lookup now supplies both the inheritance trigger and the Haskell permission check. The same update took 0.29 seconds; 50,000 runs took 3.0 seconds, with the measured individual lookup below 0.1 ms. These are temporary-table measurements, not production latency guarantees. Partial indexes also cover pending historical classification. The existing role-order and recreation integration test now checks both editor and administrator requirements against attempted downgrades. The coherent append-only follow-up is `0265_agent_evidence_lookup.sql`; `0264` was already applied and remains unchanged. The ten focused disclosure examples and all 201 bot examples pass after the lookup change. Formatting and diff checks pass. Repeated slice reviews found no remaining material distillation, constraint-evasion, or client-behavior findings; whole-branch review and release signoff remain pending.

This work adds no approval prompts. Native business mutations still need the common operation ledger, event autonomy remains conservatively capped, optional selected review and broader Slack audiences remain unfinished, and controlled live Slack acceptance and final release signoff are still pending.


### Requester-bound busy-thread followups (31 October)

The existing interrupted Slack investigation regression now includes 21 earlier replies from other linked readers and administrators. Before the fix, those replies reached the model as user instructions inside the original requester's run. The regression failed on that exact cross-requester instruction injection.

Pending-followup selection now requires the run's stored requesting member before applying the existing 20-message limit. The fresh Slack-principal check also requires that same member, so a relinked identity cannot enter the run between selection and acceptance. The original requester's steering still survives interruption and replay exactly once. Other members' receipts remain unprocessed for their own requests; their earlier replies cannot starve the requester's later followup. No new migration, permission prompt, executor, or message store was added.

The focused regression and all 201 bot examples pass after the two-line runtime change. Formatting and diff checks pass; repeated slice reviews found no remaining material distillation, constraint-evasion, or client-behavior findings. Existing already-accepted historical steering must still be considered at the event-autonomy transition: this change prevents new cross-requester acceptance, and does not rewrite old model checkpoints. The temporary event Observe ceiling remains until the effective owner/requester authority and transition behavior are verified.


### Shared effective owner/requester authority (31 October)

A failing integration regression reproduced an administrator requester executing `list_api_keys` through an editor-owned routine. The previous check stopped the run only after the read and after raising the conversation's evidence requirement.

`RunAuthority` now carries execution policy and effective project role together. The existing admission SQL returns the lower of the current routine owner's and original requester's roles, using the canonical permission ranking. Interactive admission retains current web-session and public-demo behavior. The same admission query still governs atomic completion; it does not acquire a parallel role evaluator.

The executor carries that authority into the catalog decision before execution. Its transport identity check supplies the transport's role cap, and the catalog no longer performs a second requester-only role lookup. The private `AgentAuthority` sum is removed: its only meaningful result was the permission already supplied by the existing identity checks. Role denials now describe the run's role rather than incorrectly attributing an owner's restriction to the requester's own membership.

All three owner/requester combinations pass: administrator/editor and editor/administrator are denied before dispatch without raising evidence scope; administrator/administrator retains the authorized private read. All 204 bot examples pass, including public-demo, sudo, current-role revocation, durable completion, and Slack steering coverage. All 33 native review/SDK examples also pass after the admission-query change. Formatting and diff checks pass; repeated slice reviews found no remaining material distillation, constraint-evasion, or client-behavior findings. No migration or permission prompt was added.

The broader event-autonomy transition remains incomplete. It must preserve immutable role/policy ceilings through recovery and account for pre-upgrade Slack checkpoints that already contain accepted steering. `origin/master` has the legacy Slack checkpoint/followup implementation without the new event-run root; a first new event run must not silently give such retained work broader authority. Native mutation-ledger integration and administrative result visibility also remain release work. The temporary event Observe ceiling has not been removed in this slice.


### Automatic administrative operations and protected mutation audiences (31 October)

The existing durable dashboard-mutation regression now covers both editor creation and administrator deletion. Administrator deletion initially failed because of the blanket administrative-action ban. It now uses the same Autonomous policy, current effective role, operation identity, retained outcome, and uncertain-recovery stop as other mutations. No additional approval mechanism or action executor was added; the obsolete `ActionRecoveryRequired` denial is removed.

`Administration` carries the visibility of its resources and results. Its minimum execution role remains fixed at administrator regardless of that visibility. Ordinary project administration retains project-readable results. The API-key lifecycle retains an administrator-only scope, including credential creation. The existing descriptor provides this distinction to authorization, historical classification, and schema discovery; discovery now reports `evidence_permission` separately from `minimum_permission`.

Legacy administrative snapshots used a constructor without an audience. Their decoder retains the administrator requirement, and historical classification now combines the stored capability policy with the current catalog requirement. A failing regression caught a downgrade to viewer scope after a legacy operation was reclassified; the same cases now decode and retain administrator scope. Unknown policies remain conservative.

An expanded failing model/tool regression also reproduced restricted content being copied into a project-visible dashboard and team. The executor's existing publication check now covers every project mutation, using the descriptor's resource/result visibility. Conversation documents remain within their conversation scope. The fixed regression observes zero leaked resources for both current and legacy histories; this adds no separate taint store or per-tool privacy list.

All six creation/deletion recovery cases pass: lost checkpoint, uncertain response, and repeated model call for each operation. The shared-audience cases cover a reader's denial, Observe denial, private administrator-read/credential denial, and permitted public administrative dispatch. A real private API-key creation test verifies credential retention, cached replay without another write, and denial after role downgrade. All 212 bot and 75 API v1 examples pass after these changes. Formatting and diff checks pass; repeated slice reviews found no remaining material distillation, constraint-evasion, or client-behavior findings. New codec doctests await the final doctest run. Original role ceilings, legacy event-checkpoint adoption, native operation-ledger integration, broader Slack audiences, and final release checks remain open.


### Immutable role ceilings across admission and recovery (31 October)

Five failing regressions reproduced a membership upgrade increasing authority inside existing work: interactive, scheduled, and event requests, plus independently upgraded owners and requesters.

One coherent append-only migration, `0266_agent_role_ceiling.sql`, stores `permission_ceiling` on each run. Existing unknown grants default to viewer scope. New admissions capture current eligible permissions; event requests capture the lower current owner/requester role. Recovery retains the original root's ceiling. Database triggers reject changing a ceiling or inserting a recovery attempt with a different ceiling. PostgreSQL's enum order remains isolated behind the existing rank and new least-permission function; a missing role contributes viewer scope, without making an absent member eligible.

`memberPermissionsSql` consolidates account, project, plan, and membership eligibility for capture and current checks. The opaque run context carries its stored ceiling, and its scope predicate includes that value. The shared evaluator now combines current actor, current owner, and the saved ceiling before both execution and evidence access. Interactive web-session sudo and public-demo behavior remain supported; background membership does not borrow sudo authority. The existing run inspector displays its saved role limit.

The expanded source-parameterized regression verifies initial capture, rejected in-place grant changes, interactive replay, scheduled stale-lease recovery, event continuation, and fresh work after an upgrade. Recovered work stays at editor scope and executes no privileged API read; new work captures administrator scope and performs that read. The owner/requester cases verify upgrades on either side. An overly broad test-file replacement was caught during diff review and corrected; every pre-existing test was restored before the full verification run.

All 215 bot examples, 75 API v1 examples, and 33 native review/SDK examples pass. All three Chrome scenarios pass against the current application and an isolated database, including current-role boundaries, retained artifacts, themes/mobile layout, repository settings, and restricted history after a downgrade. The temporary browser launcher is removed. The 14 focused authority examples pass after launcher cleanup. Formatting and diff checks pass; repeated slice reviews found no remaining material distillation, constraint-evasion, or client-behavior findings. Whole-branch review, doctests, release signoff, and controlled live Slack acceptance remain pending.

The event Observe ceiling remains intentionally temporary. Legacy Slack checkpoints can predate event roots and can contain an unrecorded pending mutation, including the append-capable artifact tool present on `origin/master`. Their adoption must not gain authority or blindly repeat a prior write. Native business mutations still need the shared operation ledger and capability decision; a run being active is not itself permission to mutate resources. The rest of the nine-part plan remains in scope.


### Retain completed Slack answers in their checkpoint (31 October)

Slack previously copied `AnswerReady` into `slack_turn_answers` and deleted its checkpoint. Completed answers now remain in the checkpoint, as they do for other durable conversations. The historical answer table remains readable, but receives no new production writes. Recording the conversation message is idempotent and runs on cached answers too, so an interruption between checkpoint completion and message insertion recovers without another model call.

The existing interrupted-read integration test first failed because completion deleted the checkpoint. It now also checks that a new answer creates no historical-table row, cached replay restores a missing conversation message exactly once, and historical answers remain readable without model replay. This slice adds no migration. Applied migrations through `0266` remain immutable; future schema changes should be grouped by a coherent invariant before application rather than added one column at a time. Slack request-root identity and legacy pending-mutation adoption remain separate unfinished work.

All 215 bot examples pass after this change. Formatting and diff checks pass. Repeated slice reviews found no material distillation, constraint-evasion, or client-behavior findings. Whole-branch reviews, release signoff, and controlled live Slack acceptance remain pending.

### Native issue mutation and completion share a transaction (31 October)

Extending the existing native completion regression reproduced an issue title changing even though run completion failed. The previous test checked only report and message rollback, leaving the business mutation outside its proof.

The shared completion implementation now accepts database-only work and executes it only for an accepted successful result, inside the same transaction as the run status, report, and conversation message. Issue title, recommendation, complexity, severity, error-pattern analysis, and the enhancement timestamp compose as `Tx.Transaction` operations. These helpers no longer open independent effectful writes. The ordinary completion API delegates to this same implementation with no additional database work. Redundant successful-path activity checks are removed; the fenced completion query governs whether the updates execute.

The regression now compares the complete issue before and after rejected completion. Existing pause, cancel, and deletion cases also submit a mutation and verify that the issue stays unchanged. Native capability-policy evaluation and operation-ledger attribution remain required; this transaction change does not claim to complete those parts.

All 215 bot examples and 35 error-pattern examples pass. The latter includes the full enhancement, embedding, and merge pipeline. Formatting and diff checks pass. Repeated slice reviews found no further transaction-composition or type-safety shortcuts; the native capability and ledger work remains an explicit open requirement. No migration or new module was added.

### Shared capability decisions for native metadata and publication (31 October)

Failing regressions reproduced two Observe-policy violations: issue enrichment changed issue metadata, and a production-impact review updated a GitHub comment. Both native paths previously treated an active run as sufficient authority.

`authorizeCapability` now owns the role and execution-policy decision for model tools and native project mutations. Capability classes declare whether they require Autonomous policy in the existing catalog. The existing audience and provenance evaluator is shared through `authorizeRunOperation`; native callers use their stored request context and ordinary project membership, without web-session sudo or service credentials.

Fresh enrichment and PR-review requests capture the installation's policy. Continued attempts retain their original grant. The blanket event Observe clamp now applies only to Slack requests awaiting legacy-checkpoint adoption. Existing native grants remain unchanged. Observe enrichment retains its report and explains that metadata was not changed. Observe reviews retain their report and explain that GitHub was not updated. Autonomous native work uses the same capability decisions without approval prompts.

`completeRunWithProjectMutation` also rechecks the current capability decision and evidence scope inside the fenced completion transaction. A policy reduction, insufficient original role ceiling, or newly restricted evidence prevents both the project update and successful-result publication. The ordinary result-only completion path does not acquire a project-write requirement or an extra scope lookup.

The five focused native-policy cases pass, covering Observe and Autonomous enrichment plus all three commit-boundary restrictions. The native PR workflow regression passes after first reproducing the unauthorized GitHub publication. Its additional timing cases introduce a policy reduction or restricted evidence during GitHub comment lookup; each reaches that boundary and makes no subsequent write. Tool denial messages and operation-policy wire formats remain unchanged.

All 220 bot examples, 33 native review/SDK examples, 35 error-pattern examples, and 75 API v1 examples pass. The extended PR workflow also passes after the timing assertions were added. Formatting and diff checks pass. Repeated slice reviews found no further material policy, transaction-composition, or type-safety findings within these changes. New capability-decision doctests await the final doctest run. Whole-branch review and release signoff remain pending.

Native wiki writes and other retained knowledge, operation-ledger attribution, Slack legacy adoption, and the rest of the plan remain open. These checks do not claim to supply action identity or exactly-once external publication.

### Native wiki policy uses the catalog declaration (31 October)

The native review-worker regression reproduced a successful wiki write under Observe. Native wiki callbacks now pass through the same authority and provenance evaluator as other agent capabilities. They retain the existing repository/service checks, wiki revision handling, and actual agent-attempt attribution.

`localPolicy` is the single total declaration of local operation policies. Both tool descriptors and native wiki calls use it. Its explicit constructor cases force future local operations to receive a policy; there is no default read grant. A single descriptor constructor also applies that policy to both ordinary and memory tools. Native recall remains a read, while staged review conclusions remain conversation results.

The extended application workflow passes: Autonomous wiki writes still read back correctly, Observe can list pages but cannot create one, and an Autonomous run loses wiki-write authority after a policy reduction during the worker call. Both denied cases leave the page count unchanged and make no GitHub write. All 220 bot and 75 API v1 examples pass after catalog consolidation. Formatting and diff checks pass. Repeated slice reviews found no further material distillation, type-safety, or client-behavior findings in these changes. The new catalog-policy doctest awaits the final doctest run. No migration or new module was added.

Native mutations still need common operation identities, retained proposals/results, and interruption recovery through the action ledger. Generic conversation wiki writes also still need actual agent-attempt attribution. These remain part of the full plan.

### Typed native wiki requests in the shared action ledger (31 October)

A handler-level regression reproduced native wiki writes missing from run action history. `OperationCall` now distinguishes model calls from typed native requests; native wiki writes retain their actual agent attempt and bounded edit. Model calls keep their historical JSON format, and the decoder rejects unsupported tagged calls. Operation names and native policies resolve through the existing catalog.

The checkpoint type now represents a pending native operation and a ready native worker. Both model and native mutations use the same claim, retained-result, ambiguity, and completion code. Native checkpoints retain the proposal until its response is recorded; completed native responses live in the operation ledger. Native trigger input stays on the run rather than becoming a fabricated user chat message. No new migration or parallel ledger was added.

Native admission checks the stored run authority and validates the agent attempt against its project and PR review before persisting a proposal. Recovery checks the retained proposal and current authority again. An uncertain result stops the native worker. A known result replays without another backend invocation.

The application workflow now verifies action-history rendering, exact repeated-call results, rejection of an invalid attempt before checkpoint creation, an in-flight call becoming uncertain, retention of its legitimate late response, and replay after deleting its checkpoint. The recovered wiki stays at one revision and the backend runs once. All 220 bot, 75 API v1, and 33 native review/SDK examples pass. Formatting and diff checks pass. Repeated slice reviews found no further material distillation, constraint-evasion, or client-behavior findings. New wire-format doctests and final release checks remain pending.

Adding the module exposed stale watcher state: both watchers reran tests against old object files without loading the new Cabal module. Their existing tmux panes were restarted after confirming newer source timestamps and missing module objects. The reloaded watchers compiled and exercised the new implementation. Pre-existing ReplaySpec imports and package warnings remain; no warning suppression was added.

Native GitHub publication and issue enrichment still need operation identities in this same ledger. Slack legacy adoption, selected review, actual attempt attribution for generic conversation wiki writes, and the remaining phases stay in scope.

### Native GitHub publication and shared reconciliation (31 October)

A handler regression reproduced GitHub comment publication missing from action history. `PublishPullRequestComment` now retains a typed review ID and the exact proposed body in the same operation ledger as wiki and model mutations. Scope checks bind it to the stored project's PR review. No model tool call or new migration is needed.

`OperationExecution` generalizes the existing execution representation over its result. Its constructors distinguish a database transaction, an external action without reconciliation, and an external action with a reconciler. A derived `Functor` preserves those semantics while adapting native results to the retained wire response. Claimed, retained, and unresolved decisions are separate constructors; a retained decision can only carry a known response.

A lost GitHub response remains uncertain. Reconciliation reads comments from the stored repository and PR, then requires the configured GitHub App and exact retained body. A wrong app or edited body cannot close the record. Both native wiki and publication use this same reconciler, so a fresh request can resolve an earlier publication before making another change. A matching proposal returns its observed receipt directly; another proposal proceeds through the ordinary claim path. Actions without an observed receipt stay stopped.

Automatic publication recovery reuses the saved validated review instead of invoking the agent again. Explicit Retry clears that saved result and requests fresh analysis. Comment links use the stable request root. Recreated checkpoints start above the request's recorded operation positions; this fixes a failing case where revision zero collided with an earlier wiki operation after checkpoint loss. Existing model proposal JSON stays unchanged.

The application workflow passes after first reproducing missing history, checkpoint collision, and fresh-retry dependencies. It verifies wrong-app and edited-body rejection, exact-body recovery without another agent or GitHub write, cached publication after checkpoint deletion, and one new update for an explicit fresh request. Both invalid wiki attempts and invalid review IDs are rejected before checkpoint creation.

A further failing timing case changed the PR head during comment lookup. The existing revision check now runs after that lookup and before the operation is claimed. The policy, revision, and restricted-evidence timing cases all reach their intended boundary and make no subsequent write. All 220 bot, 75 API v1, and 33 native review/SDK examples pass. Formatting and diff checks pass. Repeated slice reviews found no further material distillation, constraint-evasion, or client-behavior findings. The redundant DeriveFunctor pragma was removed because GHC2024 already enables it; the focused workflow passes after that cleanup. Whole-branch review and final release checks remain pending.

Issue enrichment still needs operation-ledger attribution. Atomic wiki mutation/result composition, generic conversation agent-attempt attribution, Slack legacy adoption, selected review, and the remaining plan phases stay in scope. HTTP fixtures prove application behavior; they do not claim live GitHub or Slack acceptance.


### Slack autonomy and historical checkpoint adoption (31 October)

Fresh signed Slack messages and slash commands use the installation's policy through the same authority evaluator as web and scheduled work. They retain the linked requester's identity and immutable role ceiling. A reader cannot inherit the routine owner's editor or administrator powers. No per-action approval prompt was added.

All durable invocations now resume the request-root checkpoint through one initializer and executor. The stored run source identifies Slack input even when recovery starts from member access in the web application. User messages retain their Slack timestamp, making input recording idempotent. Completed answers remain in the common checkpoint; historical answer rows are read-only compatibility data.

Slack can deliver an ordinary message event and an app mention for the same message. Admission and lookup share one receipt-identity query. It prefers an already admitted request, including when a later receipt has an earlier timestamp, so retries reuse the original root and operation results instead of repeating writes.

Historical checkpoints and answers do not gain autonomous authority on upgrade. Admission fixes their grant to Observe and viewer authority. Adoption retains the original model request, system context, completed results, and pending intent. Reads can resume; a known mutation response is reused. An unrecorded or unresolved mutation becomes an explicit `LegacyUncertain` checkpoint and an unknown operation record. It is not replayed. Relinking the Slack identity to another member cannot adopt the original member's historical request.

The adoption transaction locks the request root and historical checkpoint, creates the common checkpoint, and removes the replaced historical checkpoint together. The operation response and tool-summary functions are shared with the normal executor. There is no second tool dispatcher or recovery loop.

The only new schema file is `0267_slack_message_identity.sql`, containing the index for receipt lookup. Its index definition was verified inside the migrated integration-test database. Applied migrations through `0266` remain unchanged: merging their files would invalidate the migration checksums. New schema work should continue to be grouped by a coherent invariant before application.

Failing regressions reproduced duplicate model execution across event aliases, missing input during member-access recovery, and adoption after identity relinking. All 232 bot and 75 API v1 examples pass. The Slack cases use the production adapter and project tools, covering administrator/editor writes, reader denial, interrupted inference and delivery, event aliases, historical append uncertainty, known-response reuse, original model context, and account relinking. Formatting, focused HLint, and diff checks pass. Repeated slice reviews found no remaining material distillation, constraint-evasion, or client-behavior findings. Whole-branch review and final release checks remain pending.

Controlled live Slack acceptance, issue-enrichment operation attribution, atomic wiki/result composition, generic wiki attempt attribution, and the remaining plan phases stay open. HTTP fixtures prove application behavior; this session has no Slack connector and does not claim a live workspace test.


### Atomic wiki changes and composable reconciliation (31 October)

Native review and model-issued wiki writes now use the same `writePageTx` as the ordinary wiki wrapper. The shared operation interpreter commits the wiki revision and retained response in one transaction. A rejected receipt rolls back the mutation and claim together; a later retry can safely execute the original intent.

`ReconcilableOperation` wraps an execution rather than assuming an external action. It can therefore reconcile an earlier GitHub publication before a new atomic wiki write. The same three execution constructors and derived `Functor` serve both paths. Uncertain results retain their opaque claim and original typed proposal, giving the shared reconciler the actual unresolved intent. Existing result JSON stays unchanged.

The interpreter refreshes authority after reconciliation before retaining or acting on its result. Each caller supplies its original access check, preserving Slack identity and stop checks, web access, or native member authority. Native wiki recovery also rechecks repository mappings after the external lookup. Neither a role downgrade nor a revoked mapping during reconciliation can authorize the next write.

Access revocation uses the existing `AgentAccessDenied` exception, so the shared interpreter declares `IOE`; both callers already provide that effect.

Model wiki writes rely on the existing durable run and lease binding for their conversation. Their separate conversation-existence query was removed. Page creation reports a path conflict within the transaction, while the ordinary wiki wrapper retains its existing rename-conflict handling. This slice adds no migration or additional execution constructor. Historical uncertain wiki operations remain stopped unless their outcome is known.

Failing handler regressions reproduced both native and model wiki revisions surviving receipt rejection, followed by writes after role and mapping revocation during reconciliation. All 233 bot, 75 API v1, 33 native review/SDK, and two wiki UI examples pass. Formatting and diff checks pass. HLint reports the same five hints as `c55a71b88`, with none introduced by this slice. Repeated slice reviews found no additional material distillation, constraint-evasion, or client-behavior findings. Whole-branch review and final release checks remain pending.

Generic conversation wiki attempt attribution, native issue-enrichment operation attribution, the remaining plan phases, and controlled live Slack acceptance stay open.


### Wiki authors link to the actual shared run (31 October)

Chat, scheduled, and native review wiki revisions now retain a typed `Run` author from the current opaque run context. A recovery worker records its actual run ID, rather than its conversation or request root. The existing run row supplies the actor, original grant, request root, and action history.

Wiki history renders this author as a link to the existing run inspector. Historical member, SDK-attempt, and conversation authors remain readable. Native review proposals retain the real SDK attempt that supplied the edit, while the wiki author identifies the shared worker that committed it. SDK workflows without a shared run retain their real SDK-attempt authors. New model writes no longer create conversation-only authors.

The only schema change is the `Run` alternative in the existing author constraint, added by `0268_wiki_run_author.sql`. It adds no table or copied actor metadata. Earlier applied migrations remain unchanged.

The regression first failed on the stored conversation author. It now uses a failed request followed by a distinct recovery worker, verifies the actual writer ID, follows the wiki's run link through the real handler, checks its action history, and retrieves the knowledge from another conversation. Scheduled authority and denial cases verify the same author contract. A native workflow regression first failed on its SDK-only author, then passed with the shared run reference.

All 233 bot and 75 API v1 examples pass. Fourteen focused wiki examples, both wiki UI examples, and the native review and issue-fix workflows also pass. Formatting and diff checks pass. HLint reports the same four hints as `e707208ae` across the changed application modules, with none introduced. Repeated slice reviews found no remaining material distillation, constraint-evasion, or client-behavior findings. The open-PR refresh still lists 28 PRs; all twenty selected incoming heads and published #619 remain included, so no additional selected merge is needed.

The new codec doctest awaits the final doctest run. Native issue-enrichment operation attribution, the remaining plan phases, whole-branch review, controlled live Slack acceptance, and release checks stay open.


### Native enrichment uses the shared action ledger (31 October)

Issue enrichment now retains a typed native intent with its issue ID, enhancement, classification, error analysis, and analysis time. The existing checkpoint stores that intent before mutation. Recovery reuses it instead of repeating completed inference. Nonnegative classification counts use `Natural` at the parsing boundary and in the retained intent.

The existing completion implementation is now also available as a transaction. The shared atomic-operation interpreter composes it with the issue mutation and operation receipt. Metadata, classification, linked error analysis, report, answer, receipt, and final run state therefore commit together. The target issue is locked within its project before the completion guard runs. There is one completion implementation and one operation ledger, with no new table or migration.

The canonical native policy is `ProjectWrite`. Observe retains analysis without a mutation record. Autonomous work uses the original actor and current authority. Native scope checks reject an intent for another issue before dispatch, including when a valid earlier intent is already pending. The timestamp identifies completed analysis; it does not claim that a pending mutation has already committed.

The regression reproduces missing action history on the original mutation path. It then rejects either final completion or receipt storage and verifies full rollback. Both recovery cases apply the metadata once, publish one answer, retain one typed native receipt with the real actor and run, and use only the original three inference calls. Five policy cases and the source-scope rejection also pass.

All 234 bot, 75 API v1, and 35 error-pattern pipeline examples pass. The native review workflow also passes, covering atomic wiki receipts and GitHub recovery through the extended operation type. Formatting and diff checks pass. HLint reports the same seven hints as the baseline across the changed application modules, with none introduced. Repeated slice reviews replaced a Boolean decision with the existing execution-policy type and found no further material distillation, constraint-evasion, or client-behavior findings.

The test watcher was restarted once after it demonstrably continued running an old test name and old source locations. The corrected failure and subsequent passing evidence come from the refreshed module graph. Existing warnings in unrelated test files remain unchanged. Whole-branch reviews, final doctests, controlled live Slack acceptance, and the remaining plan phases stay open.


### Relevant wiki discovery shares the existing list tool (31 October)

The existing `list_project_memory` capability accepts an optional bounded query. It returns at most 100 current-page summaries matching a case-insensitive literal substring in the title, path, or body. An omitted query preserves the recent-page listing. Search uses the wiki browser's existing `pageWindow` query across the project, rather than filtering the latest 100 results in memory.

Chat, scheduled conversations, and native reviews use one parser and lookup. A typed `Search (MemoryText 200)` request carries the parsed native query; existing `List` requests and their JSON remain valid. Search uses the existing viewer-read policy, with native access rechecked before returning results. It adds no tool name, index, migration, search service, or authorization mode.

The regression first reproduced an ignored query returning an unrelated page. It covers title, body, path, literal wildcard characters, current revisions, another project's page with the same ID, omitted queries, and rejected malformed or oversized input. Native review passes search under Observe, including a policy reduction during execution. The real SDK exploration test passes both legacy list and typed search requests through the shared schema and parser.

All 235 bot and 75 API v1 examples pass. Formatting and diff checks pass. HLint reports the same four hints as the baseline across the changed application modules, with none introduced. Repeated slice reviews consolidated native listing and search response handling and found no remaining material distillation, constraint-evasion, or client-behavior findings. A fresh inventory still lists 28 open PRs, with no changed heads or additional selected integration work. Whole-branch reviews, controlled live Slack acceptance, final release checks, and the remaining plan phases stay open.

### Recovery retires unowned routines without replacing their actors (31 October)

The existing hourly routine recovery sweep now retires installations whose authorizing member is absent. In one transaction it clears their schedule and lease, cancels their running attempts, marks unresolved operations unknown, and freezes conversation artifacts into run history. Normal completion and retirement share `finishRunRowsTx` for operation retirement and artifact snapshots. Returned operation results, original operation actors, and conversation history remain intact. A null run actor remains null; maintenance never invents a replacement member.

The entry point is named `recoverDueRoutines` because it now performs recovery before returning due schedules. The existing hourly job calls it; there is no additional timer, queue, or migration. Its run lookup uses the existing routine/run index.

A small `RunFinalization` sum selects a completed member run or retirement of unowned routines. The member case requires an opaque run context and a terminal status. Callers cannot pass an arbitrary SQL prefix; the shared transaction helper owns both statements and their common evidence handling.

The regression reproduces a paused, unowned installation whose attempt still appears to be running. It also reproduced missing artifact snapshots on retirement. It rejects the operation-state update to verify that run cancellation, lease cleanup, and snapshots roll back together, then repeats successful recovery to check idempotence and preserved history. All 236 bot examples and the native review workflow pass. Formatting, HLint on the changed application modules, and diff checks pass. Repeated slice reviews consolidated the shared finalization and replaced its arbitrary SQL input with the typed sum, with no remaining material slice findings.

Interactive orphan attempts, a reassigned installation with an already unowned attempt, and legacy terminal-run leases remain separate lifecycle cases. Whole-branch review and release checks remain pending.

### New routine grants retire unowned current attempts atomically (31 October)

Resume, interval scheduling, template installation, and interactive routine creation now share `prepareRoutineGrantTx`. It locks the project-scoped installation, reads its schedule, and retires unowned current work through the existing finalization interpreter before writing the replacement grant. Retirement and the new grant commit together.

`RoutineRetirement` distinguishes the unowned-installation sweep from reauthorization of one project conversation. Callers cannot supply an arbitrary SQL scope. Reauthorization retires a missing installation owner or an unowned scheduled attempt holding the current slot. Healthy active leases stay intact. The replaced timing-query wrapper is removed.

The replacement worker gets a new request root, its actual member, and the current role ceiling. The old run keeps its missing actor, original policy, role ceiling, and frozen evidence. Existing Observe policies remain Observe during reactivation. Nothing transfers the old request to the new member, and no approval step, queue, table, or migration is added.

Six regressions first failed across resume, interval scheduling, and template installation, both before and after installation ownership had been reassigned. They verify a fresh background invocation, preserved evidence, an unchanged old actor, a failed replacement grant rolling back retirement, and another project's matching conversation remaining untouched. The fixtures then detach the paused installations and delete the original administrator account, exercising the real run-actor foreign key. The replacement editor's run has its own root and editor ceiling; the old run's actor stays null and its administrator ceiling stays unchanged.

All 242 bot examples pass. The strengthened account-deletion cases and native review workflow also pass. Formatting, HLint on the changed application module, and diff checks pass. Repeated slice reviews consolidated preparation across all grant writers and removed the replaced timing wrapper, with no remaining material slice findings. A fresh inventory still lists 28 open PRs, with no changed heads or missing selected work.

Interactive and detached orphan attempts, historical orphan attempts outside the current scheduled slot, and terminal-run lease recovery remain open, along with the remaining plan phases and final release checks.

### Slack sessions share generated titles and preserve human renames (31 October)

Slack and ordinary chat use the same `ensureConversationTitle` helper, model selection, usage attribution, and access checks. A Slack investigation names an untitled conversation before starting its native session. The existing status call carries that title only as initial session metadata. Slack documents that this field is ignored for an existing session, so an automatic title cannot replace a human's Slack rename. This replaces the proposal's after-answer rename with a creation-only operation; no title-delivery ledger or migration is needed.

An existing name avoids another model call. A manual name already recorded before app conversation creation seeds that conversation. Report retrieval uses a request-derived title without introducing model calls. Recovery uses the retained run input for naming and query dispatch. The superseded title-presence query is removed.

Accepted signed Slack rename events update the thread metadata and app conversation title in one transaction, both at intake and through the existing queued worker. The existing incident/thread resolver now has a transaction form, so an alert rename reaches the issue's conversation too. Duplicate, older, unlinked, and revoked-member events cannot reapply a name. A failed conversation write rolls back the metadata update; the retained receipt and existing job allow recovery.

The opaque `SlackSessionTitle` type parses the 1–200-character, nonblank, null-free contract at JSON and database boundaries. The same type is used in native status payloads. Cleanup status updates omit the title field. The original Monoscope title writer keeps its existing 100-character normalization and conditional generated-title update, so a rename during inference wins. Authority is checked again before the native status call.

The regressions first failed because analysis started before a shared title existed and a conversation-write rejection did not affect Slack metadata. The focused checks now cover generation, a rename during inference, a name recorded before conversation creation, one successful generation call, replay without further HTTP, malformed signed titles, ordered and authorized renames, and atomic rollback. All 245 bot, 75 API v1, and 23 incident-delivery examples pass, including signed rename propagation to an existing issue conversation without a parallel Slack conversation. Formatting and diff checks pass. HLint reports the same two hints as the baseline across the changed application modules, with none introduced. Repeated slice reviews removed the obsolete title-presence query, retained canonical thread mapping, and kept inference and dispatch on the saved run input, with no remaining material slice findings. The new codec doctest awaits final doctest verification.

Dynamic prompts, shortcut intake, session-lifecycle acceptance, streaming, proactive work, Slack evidence audiences, controlled live Slack acceptance, and final release checks remain open.

### Preserve session cleanup after uncertain startup acknowledgements (31 October)

A failing regression reproduced a completed Slack investigation deleting its queued status reset after a transport failure. Slack may already have accepted `processing`, so the completed receipt cannot prove the session is idle. The adapter now keeps the existing reset job for uncertain startup results. Definite rejections, including an unavailable Agent feature, still remove unnecessary cleanup. The private `SlackSessionFailure` sum distinguishes rejection from uncertainty; no exception-message matching is used.

Status parsing reuses the existing acknowledgement envelope and definite-rejection classifier. Transport failures, malformed bodies, missing error details, and Slack internal errors remain uncertain. General API calls, status updates, and reserved publications share one HTTP request helper. Reset retries retain their job until an acknowledged status update; receipt replay does not repeat the investigation. No new worker, migration, or delivery ledger was added.

The existing private-mention integration test now covers all four uncertain responses, failed reset attempts, successful deferred cleanup, and replay, while retaining the known-feature-rejection case. The initial regression failed with zero cleanup jobs instead of one. The focused regression and all 245 bot examples pass, including signed stops, status cleanup, publication reconciliation, and rate limits. Formatting and diff checks pass. HLint reports no hints in the changed application module, matching the baseline. Repeated slice reviews consolidated the request transport and acknowledgement parsing, with no remaining material distillation, constraint-evasion, or client-behavior findings. Whole-branch review, controlled live acceptance, and final release checks remain pending.

Slack's thread session documentation explicitly requires the app to confirm stops and transition status away from `processing`. Its code-channel lifecycle has a different automatic-stop contract. This adapter handles ordinary threads, so its stop confirmation and status cleanup remain unchanged. These changes apply to ordinary thread sessions; controlled live session acceptance remains open.

### Contextual Slack prompts use the shared project stores (31 October)

Assistant start and context-change events now share one typed event branch and the existing event-lock mechanism, scoped to the private DM where prompts appear. Both refresh up to four suggestions from the current project: the selected channel's latest delivered alert, an active incident, recent unacknowledged issues, and generic fallback questions. The issue list and incident list reuse their existing filters and queries. An older event reads the latest retained context across that DM’s threads, so neither a delayed context change nor an older thread can restore an obsolete channel suggestion. No model call or new SQL migration is needed.

Suggestions are sent only to the private Messages surface after resolving the linked member and any existing thread/project binding. Access is checked again after data retrieval. Unlinked or revoked users get only account-link guidance. Titles are bounded; prompts identify existing issue and incident records instead of embedding their content as instructions. The original signed receipt supplies replay protection.

The Agent prompt API now omits `thread_ts`. Slack documents that this legacy assistant parameter makes Agent-app prompt calls silently fail. This implements the original coworker proposal's dynamic suggestions while retaining the canonical identity and conversation boundaries.

The handler-level regression first failed because an unlinked user received generic telemetry prompts. Its added ordering case then reproduced an older thread restoring an obsolete channel alert. It now passes linked/unlinked/revoked cases, project and archived-issue isolation, active incidents, latest channel alerts, bounded titles and prompt count, context changes, delayed events, and receipt replay. All 246 bot examples pass. Formatting, focused HLint, and diff checks pass. Repeated slice reviews merged the two event variants, kept suggestions in their owning worker, and serialized updates at the DM surface, with no remaining material distillation, constraint-evasion, or client-behavior findings. Message shortcut intake, live Slack acceptance, and the remaining plan phases and release checks remain open.

### Agent Messages opens refresh the same prompts (31 October)

The current Slack Agent experience sends `app_home_opened` for the Messages tab. That event now uses the same prompt refresh as legacy assistant start/context events, sharing the DM lock and signed member identity. A first visit does not require an assistant-thread row: it uses the linked default project and generic project suggestions. A later visit can reuse that user's latest retained thread context and binding.

The manifest template removes static prompts, which Slack documents as incompatible with dynamic Messages-tab updates. The rollout guide records the corresponding existing-app setting change. The application does not apply a live manifest migration.

The onboarding regression first failed because a Messages visit sent only its account-link message. It now verifies prompt refresh on opening and reopening, one-time link delivery, linked and unlinked content, omitted legacy thread timestamps, receipt replay, and no investigation creation. The contextual prompt test also checks a Messages reopen retains the selected project context. All 246 bot examples pass. Formatting, focused HLint, manifest JSON checks, and diff checks pass. Repeated slice reviews reuse one refresh for both native and legacy surfaces and remove the required-context assumption, with no remaining material slice findings. Live manifest acceptance and final rollout checks remain pending.

### Message shortcuts enter the canonical Slack receipt worker (31 October)

The signed interactivity endpoint accepts `monoscope-investigate-message` and immediately stores its receipt and existing worker job together. It decodes a typed selected message and shortcut identity, retaining text, author, blocks, and original thread references. The normalized event has an explicit shortcut kind and a typed source. The worker rejects a normalized input that differs from that source. Callback, workspace, identity, and timestamp checks happen before queue insertion.

The shortcut asks for investigation and quotes the selected message as untrusted evidence. The clicking member supplies authority; the selected author supplies none. A selected reply continues its original thread. The same project binding, identity-link guidance, routine activation, role ceilings, checkpoint, tool dispatcher, reply reservations, progress updates, stop checks, and reset worker handle the request. It adds no approval prompt or separate executor.

The existing receipt insertion is shared with signed Events API intake. A stable shortcut trigger identifies retries. Shortcut identity uses that receipt directly; message/mention alias reconciliation keeps its original predicates and partial index. Shared admission and accepted-followup queries recognize the new request kind. No migration or index replacement is needed. Dashboard actions retain their existing implementation under the same signed route.

The manifest declares the message shortcut and its callback. The rollout guide explains its thread and identity behavior. The handler regression first failed because message shortcuts were rejected with HTTP 400. Both root-message and selected-reply cases now pass through signed form intake, duplicate intake, queue processing, model context, run attribution, Autonomous policy, final thread publication, and receipt replay. They also verify quoted blocks, unknown callbacks, invalid timestamps, unlinked members, and revoked members, with no extra model calls for denied or replayed work. All 248 bot examples pass. Formatting, manifest checks, and diff checks pass. HLint reports the same one hint as the baseline across changed application modules, with none introduced. Repeated slice reviews retain typed source validation, keep the dashboard handler local to signed intake, narrow the shared receipt effect row, and preserve the existing alias index path, with no remaining material slice findings. Whole-branch reviews and live workspace acceptance remain pending.

### The shared maintenance sweep retires detached and actorless attempts (31 October)

The hourly recovery transaction now extends the same run-finalization algebra to abandoned attempts. It cancels running work whose requesting account disappeared, including interactive and historical attempts outside a routine's current slot. It also cancels scheduled and event work whose installation was deleted, while retaining a still-current requesting actor. Healthy interactive attempts do not need an installation and remain running.

The existing finalizer marks unresolved operations unknown and snapshots retained artifacts. Known operation responses and original operation actors stay unchanged. This cleanup leaves a healthy routine's replacement owner, activation, next schedule, and lease untouched. It adds no scheduler, worker, table, migration, or substitute actor.

Four failing-first regressions use actual account deletion or installation deletion, and an independent healthy interactive attempt. Each now passes cancellation, rollback on operation-result rejection, actor preservation, known-response preservation, artifact retention, repeated maintenance, and unchanged healthy grants. All 252 bot examples pass. Formatting, focused HLint, and diff checks pass. Repeated slice reviews retain the existing finalizer and transaction, distinguish healthy interactive work from detached installation work, and preserve healthy replacement grants, with no remaining material slice findings. Final release checks and terminal-run lease recovery remain open.

### Normal completion and maintenance share schedule advancement (31 October)

A finished scheduled run could retain its routine's lease and original scheduled instant. Retrying that instant could not restart the terminal run, but did refresh the routine lease. The maintenance sweep now repairs the finished slot, including leases already refreshed by older code. `claimRoutine` rejects terminal or actorless slots before changing their lease.

Normal completion and recovery use one `advanceRoutineScheduleTx` interpreter and the existing interval/calendar calculation. `ScheduleCompletion` carries the routine id, scheduled instant, captured lease, and optional historical finish time. Both paths lock the routine and compare its slot and lease before advancing. Recovery preserves a known completion timestamp; if an old record lacks that timestamp, the previous known value remains. Paused installations release their lease without scheduling another run. Live attempts retain their current lease.

The recovery function uses the clock effect for its cadence and due/stale queries. It skips missed intervals, returns the repaired future slot to the existing queue, and does not enqueue it again on a repeated sweep. Abandoned-attempt retirement and lease repair commit in the same maintenance transaction. No scheduler, migration, or alternate timing implementation was added.

Fourteen parameterized cases cover all terminal outcomes, live attempts, paused installations, missing historical finish times, skipped intervals, rollback, repeated recovery, and a fresh authorized next run. Twelve initially failed. Further regressions reproduced rejected stale claims refreshing their lease for both terminal and actorless attempts. The focused cases now pass. The real account-deletion fixture also covers reauthorization retaining an active lease, followed by one sweep cancelling the abandoned run and advancing that slot without changing its replacement grant. All 267 bot examples pass, including the previous orphan, reauthorization, stale-worker, native completion, and interval/calendar regressions. Formatting, focused HLint, and diff checks pass. Repeated slice reviews share schedule advancement, preserve known historical times, mirror reclaim eligibility before changing a lease, and keep recovery on the existing clock and queue, with no remaining material slice findings. Whole-branch reviews, live acceptance, and final release checks remain pending.

The refreshed inventory still lists 28 open pull requests, with no new, closed, or changed heads. All twenty selected incoming heads and published #619 remain ancestors of this branch. No additional selected merge is required.

### API, MCP, CLI, and contextual capability inventory (31 October)

The coverage audit verifies 75 published API operations, three MCP workflows, and 23 contextual agent tools. Their 101 names are distinct. Browser replay ingestion remains the one explicit conversational exclusion, leaving 100 classified discoverable capabilities. Native wiki writes reuse the wiki capability; issue enrichment and PR comment publication retain their typed native intents and the same policy and operation ledger.

The permanent generated-spec regression compares every published HTTP method/path, excluding the MCP endpoint itself, with both the emitted MCP bindings and the canonical metadata map. It catches missing or stale descriptors and name collisions that a key-only registry check could miss. It also requires one descriptor for every constructor in the local operation algebra and checks the CLI's nine resource-list families against the same published paths. The existing agent-adapter regression now compares the complete structured response with its direct API result, and pins POST event queries as reads.

The remaining adapters have concrete transport or context duties. `CLI.Resource` selects paths and terminal rendering while calling the public API; its binary remains independent of the server library. CLI credentials, local files, completion, picking, and ingestion remain client operations. Local KQL/SQL tools preserve the current run's time range, limits, and formatted result while reusing the existing query interpreter. Cached facet context needs no new query. Thread history, incident context, repository evidence, conversation artifacts, and wiki tools retain their explicit conversation or project boundaries. Composite workflows bundle reads or inference under the caller's effects. These adapters add no second domain mutation implementation.

All 76 API examples pass, including exact catalog coverage, MCP HTTP routing, response equivalence, and metric discovery. The latest unchanged production code also passed all 267 bot examples. Formatting and diff checks pass. No production implementation, dependency, shared-package module, or migration was added in this audit. Whole-branch reviews and final release verification remain open.

### Live Slack access remains an external acceptance requirement (31 October)

Plugin discovery confirms that the Slack integration is available and not connected in this session. The desktop app rejects UI automation without assistive access. A subsequent browser navigation confirmed a signed-in Monoscope Slack workspace; absence of an existing Slack tab was insufficient evidence that browser access was unavailable. The controlled workspace/channel target has been requested while independent implementation continues. No live agent behavior is counted as verified by the signed HTTP fixtures or the local browser artifact checks.

### Channel-scoped incident installations (31 October)

`/monoscope incidents on` and `incidents off` now configure one event routine for the signed workspace and channel. They reuse account linking, current editor access, the routine installer, policy controls, conversation titles, and pause behavior. The initial policy is Observe. Repeating activation preserves the owner's policy and instructions; a replacement owner starts with Observe and advances the authority version. Each channel has its own grant and conversation. Activation does not start a model call or a scheduled job.

The installer accepts a catalog template or a typed incident channel. Both use one installation transaction. The channel constructor rejects direct messages and malformed identifiers. The database requires a complete workspace/channel pair for incident routines, forbids that pair on other routine kinds, and prevents duplicate channel installations. One migration, `0269_channel_incident_routines.sql`, holds this schema change. The retired generic `scope` column from migration `0197` is not restored. Previously applied files remain unchanged.

The regression first failed because activation created no routine. It now covers replay, two separate channels, pause/resume, retained titles and instructions, explicit autonomous policy retention, reader and unlinked-member denial, replacement ownership, invalid database bindings, restricted-evidence denial, and no execution during activation. All 268 bot examples pass, including the existing template, authority, cancellation, schedule recovery, and Slack delivery regressions. Formatting, focused HLint, and diff checks pass. Repeated slice reviews found no remaining material distillation, type-evasion, or client-behavior findings. Doctests, whole-branch review, and final release checks remain pending.

The execution and delivery portion follows below. Channel installation alone does not establish live Slack acceptance or release readiness.

### Proactive incidents share execution and delivery (31 October)

Opted-in channels now receive automatic incident analysis and closing reports. The existing incident-delivery pass queues a typed `InvestigateIncident` job after the real root timestamp is known. Queue identities are unique; completed or already queued events do not consume each subsequent batch. Activation timestamps exclude older events and reset when the channel is resumed or reauthorized. There is no new scheduler.

`IncidentEvent` retains the real event and root IDs. Fresh and resumed attempts use one admission query. The source must belong to the installation's project, workspace, channel, and issue conversation, with a current Slack installation and public evidence scope. Only alert, measured recovery, and explicit resolution events qualify. A data gap cannot create a closing report. The same binding is rechecked during execution and before publication.

The worker uses the existing thread lease, run-root checkpoint, agent executor, capability catalog, operation ledger, and completion transaction. Its member context limits evidence to the shared audience. Observe permits analysis and its fixed report; it does not enable arbitrary write tools. An explicit Autonomous grant uses the existing API handlers and action receipts for remediation. Runs and changes retain the authorizing member, including across recovery.

Completion retains the full result and conversation answer. An idempotent insert then adds a typed analysis reply to the existing incident outbox. If that insert fails, recovery loads the completed result without another model call. Slack receives a bounded summary and run link. The existing sender rechecks current authority; the existing metadata and history reconciliation handles lost acknowledgements. Completed-result checks do not make completed runs eligible to execute more operations.

One coherent migration, `0270_incident_agent_delivery.sql`, adds the activation boundary, report/run link and payload constraint, delivery variant, and unique queue identity. It leaves the already applied channel-installation migration unchanged. Report reconciliation uses the clock effect for its initial retry time.

All 38 incident-delivery, 268 bot, and 76 API examples pass against the current implementation. New coverage includes both closing sources, opt-out silence, private-evidence denial in both policies, attributed autonomous remediation, interruption after a real mutation, failure after completion but before outbox insertion, source/context forgery, resumed-request validation, pause and revocation during work or before delivery, installation replacement, bounded summaries with full retained artifacts, and ambiguous-send reconciliation without duplicate inference or publication. The missing worker, revoked-worker retry, reconciliation clock, and resumed-admission regressions each failed before their fixes. Formatting and diff checks pass. Scoped HLint has no new findings; existing hints in the AI and routine-page modules remain unchanged. Repeated slice reviews found no remaining material distillation, constraint-evasion, or client-behavior findings.

Controlled Slack acceptance and final whole-branch checks remain open. Optional selected review, streaming, and user-authorized Slack evidence are separate unfinished slices. No push, merge, deployment, or live Slack configuration change is included in this slice.

### Shared authenticated credential storage (31 October)

Slack user authorization will use the shared provider-secret helper rather than introduce another encryption implementation. Existing Git credential writes now use AES-256-GCM with a fresh nonce from the UUID effect, an authenticated version marker, and typed failures for invalid keys, empty secrets, malformed envelopes, and authentication failure. Existing API-key encoding remains unchanged. Git credential reads retain legacy encrypted-token compatibility; unknown secret versions and corrupted authenticated tokens fail closed. No migration or credential rewrite is needed.

The integration regression first exposed identical ciphertext for repeated plaintext, then exposed the empty-secret invariant lost by envelope encoding. Both failures are fixed. All 63 code-context, 268 bot, 76 API, and 38 incident-delivery examples pass with the new helper and shared deterministic UUID interpreter. Scoped HLint, formatting, and diff checks pass. Repeated slice reviews found no material distillation, constraint-evasion, or client-behavior issue. New invariant doctests and final whole-branch gates remain pending.

This is the storage prerequisite, not completed Slack evidence support. User OAuth, requester-bound credential lifecycle, remote MCP reads, revocation, and audience containment remain unfinished. Slack's current metadata advertises its user-only authorization endpoint, confidential client-secret POST exchange, and S256 PKCE; app eligibility and live MCP configuration still require acceptance evidence.

### Requester-bound personal Slack consent (31 October)

Personal OAuth and workspace installation now share the expiring receipt store and registered callback. A closed authorization sum selects the user-only or workspace exchange; a personal response cannot overwrite the shared bot token or alert destination. Personal consent binds the initiating Monoscope account, project, signed linked Slack identity, workspace, and S256 verifier. Current membership and installation are rechecked when claiming the receipt, saving the grant, and reading it. Linked readers can authorize their own reads without obtaining installation-admin authority.

`/monoscope search connect` returns a private consent link. `/monoscope search disconnect` removes only the requesting account's grants and pending consent for the current project. The authenticated web disconnect uses the same operation. A member-row transaction lock serializes creation, storage, and disconnect. Its separate lock and mutation statements ensure disconnect sees a grant that committed while it waited. No transaction spans Slack's HTTP exchange.

Access and refresh tokens use the shared authenticated encryption helper. The grant type distinguishes permanent access from a refresh-token/expiry pair, and the clock effect controls expiry checks. Tokens and consent have no Show or JSON instances that could put credentials into diagnostics or agent artifacts. The manifest declares the same read-only user scopes as the consent request; a regression checks that contract. No live manifest or app settings were changed.

One coherent migration, `0271_slack_user_authorization.sql`, extends the receipt store and adds requester-bound credentials with foreign keys, read-scope constraints, and token-lifetime constraints. Earlier migrations remain unchanged. The old installation-only callback consumer has been removed.

The regressions first exposed rejection of personal consent by the bot callback, inconsistent credential configuration, database-clock expiry, a disconnect/write snapshot race, and slash controls falling through to routine activation guidance. All 275 bot examples now pass, including seven personal-OAuth examples. They cover the full start/exchange/storage path, S256 parameters, missing-code handling, reader access, another initiating account, wrong returned user/workspace, write-scope denial, malformed tokens, rotation metadata, repeated callbacks, workspace replacement, and a controlled concurrent insertion/disconnect. All 76 API and 38 incident-delivery examples also pass. Formatting, focused HLint, and diff checks pass; repeated slice reviews found no material remaining distillation, constraint-evasion, or client-behavior issue.

A fresh inventory still lists 28 open PRs, and all twenty selected current heads remain ancestors of this branch. Remote MCP reads, automatic refresh, upstream revocation events, audience containment, app eligibility, and live Slack acceptance remain open. The credentials are not yet exposed to agent tools. Streaming, selective review, and final whole-branch gates also remain unfinished; this slice does not establish completion of the unified plan.

### Signed token revocation uses the existing receipt and worker (31 October)

Slack's `tokens_revoked` callback now parses OAuth user IDs separately from bot token IDs. Signed intake applies revocation immediately. It uses the same credential-selection algebra, member lock, and transactional cleanup as manual disconnect. Scope remains the named workspace and Slack users; another Slack account linked to the same Monoscope user retains its grant, and the project bot installation remains configured.

The stored receipt's immutable subject must match the typed callback subject before cleanup can run. Receipt completion and deletion of grants and pending consent commit together. Duplicate intake and a delayed worker cannot repeat cleanup after a later reconnection. A database failure retains the pending receipt and existing queued job; that same worker recovers without model or Slack API calls. Member locks are acquired in a stable project/user order. Expired-consent cleanup now stays within the same member/project scope.

The regression first failed because a signed revocation left its credential readable. Nine personal-OAuth examples now pass, including subject/workspace isolation, replay after reconnection, an injected database failure with rollback, worker recovery, and a forged subject passed against an immutable receipt. The existing concurrent insertion/disconnect regression now covers both manual disconnect and signed upstream revocation through one table-driven fixture. All 277 bot, 76 API, and 38 incident-delivery examples also pass. Formatting, focused HLint, and diff checks pass. Repeated slice reviews found no remaining material distillation, constraint-evasion, or client-behavior finding.

The manifest template now includes `tokens_revoked`. No live app settings changed. This slice adds no migration, table, scheduler, executor, or separate revocation worker. Remote MCP reads, automatic refresh, audience containment, controlled live acceptance, and the other unfinished unified-plan slices remain open.

### Automatic refresh shares the OAuth client and credential state (31 October)

Code exchange and refresh now use `Pkg.SlackOAuth`: one request algebra, form encoder, token endpoint, response parser, identity check, and read-scope contract. The installation-only bot exchange remains separate because it returns a workspace installation, not personal evidence authority. Duplicate personal-response parsing has been removed from the Slack handler. Credentials and requests still have no Show or JSON encoders that could expose secrets.

The credential resolver renews rotating grants within one minute of expiry or on their next use after expiry. Fresh and non-expiring grants make no OAuth request. One durable refresh claim is stored on the existing credential. Other callers can use an unexpired access token or receive a retry deadline while a claim runs. An uncertain exchange, including a stale claim left by a crashed worker, never automatically replays its single-use refresh token. A definite rate limit releases the unspent claim through the existing cooldown mechanism. Reconnection clears the old state.

Claim, completion, and revocation share the existing member lock. HTTP runs outside database transactions. Conditional writes bind the exact credential version and claim, and one scope query checks current identity, installation, account, project, and membership. Disconnect cannot resurrect a credential; membership loss or workspace replacement prevents its return; a later reconnection wins over both a successful and a rejected older refresh. Access token, refresh token, scopes, expiry, and ready state change together. One shared sealer handles initial grants and refreshed grants.

Migration `0272_slack_token_refresh.sql` adds one typed JSON state column and its shape constraint. It adds no table, worker, or scheduler and leaves applied migrations unchanged. The schema represents idle, claimed, and uncertain states with the claim owner and timestamp kept together.

The new expired-grant test first failed with `SlackNotConnected`. Further regressions exposed conflicting top-level/nested scopes being accepted and a late rejection being returned after a successful reconnection. Both are fixed. All 25 focused personal-OAuth examples, 293 bot examples, and 76 API examples pass against the final source. They cover single refresh, cached reuse, concurrent callers, disconnect/reconnect, membership loss, workspace replacement, lost acknowledgement, stale claims, rate limits, revoked refresh tokens, malformed responses, identity mismatch, and scope escalation. All 38 incident-delivery examples also pass. Formatting, focused HLint, and diff checks pass; repeated slice reviews found no remaining material distillation, constraint-evasion, or client-behavior finding.

Adding the shared module exposed stale watcher module graphs: a new named test initially matched zero examples, and its compiled object lacked the new name. Those results were discarded. The test watcher was restarted once and loaded the new module; the failing and passing regressions above ran with the expected example counts. The server watcher was refreshed once for the same stale graph and now compiles the shared client. Runtime startup still fails against the pre-existing configured database credentials; compilation is not counted as live application acceptance.

This implements credential refresh support; the agent has no remote Slack evidence capability yet. Audience containment and remote MCP reads remain required before these credentials enter agent execution. Selective review, streaming, controlled live Slack acceptance, the final visual-review refresh, and whole-branch release checks remain unfinished.

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
