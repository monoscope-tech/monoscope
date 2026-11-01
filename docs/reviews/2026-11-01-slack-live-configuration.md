# Monoscope Slack configuration audit

Observed on 1 November 2026 through the authenticated Slack app portal. This audit made no live configuration changes.

The existing app is `A065WU6AW5A`. Its owner workspace is `T06672NKS8Z`.
Acceptance uses only the Monoscope workspace, `T0AP82L853K`, at <https://monoscope-workspace.slack.com/>.
The owner workspace and acceptance workspace serve different purposes.

The [branch manifest](../slack/agent-manifest.json) describes the intended configuration.
The live manifest exposes these differences:

| Configuration | Live observation | Effect on acceptance |
| --- | --- | --- |
| Agent view | Present, with an empty `suggested_prompts` array | Agent configuration exists; this does not prove session availability. |
| Messages tab | No `app_home` declaration | The configuration does not declare the writable Messages tab required by the branch. |
| Bot scopes | `assistant:write` is present; `im:read` and `mpim:read` are absent | The configured scopes do not cover every conversation lookup expected by the branch. |
| Events | Only `app_mention` | DM messages, ordinary thread replies, stop events, context changes, and revocation events cannot reach their handlers through these subscriptions. |
| Message shortcut | None | “Ask Monoscope about this” is unavailable. |
| MCP | `is_mcp_enabled` is true | Eligibility and authenticated tool discovery remain unproven. |
| Public distribution | Enabled | Distribution does not prove Marketplace approval. |
| Installed grants | Portal requests reinstallation after scope changes | Configured scopes do not establish the installed token's grants. |
| Marketplace panel | Slack displays a load error | The panel does not establish publication status. |

The missing event subscriptions are:

```text
message.channels
message.groups
message.im
message.mpim
app_home_opened
tokens_revoked
app_context_changed
agent_session_stopped
agent_session_title_changed
assistant_thread_started
assistant_thread_context_changed
```

These differences do not explain the earlier silent private-channel mention: `app_mention` is already subscribed.
The existing deployment also differs from this branch. A live reply from that deployment cannot establish branch acceptance.

The branch regression `firstPrivateMention_recordsVisibleRoutineRunAndArtifactWhenAgentStatusIsUnavailable` exercises a signed private mention through shared execution.
It asserts one reply, one successful visible run, retained artifacts, and no duplicate work on replay.
Its provider and Slack transport use test interpreters; it does not prove live workspace behavior.

## Configuration boundary

The live app declares additional user scopes, including write scopes. This audit did not remove or exercise them.
The branch's personal evidence flow requests its own read-only scope set. Existing app configuration does not prove consent for that flow.

Slack permits hosted MCP access for internal or Marketplace-published apps. Public distribution alone does not establish that requirement.
See [Slack's MCP requirements](https://docs.slack.dev/ai/slack-mcp-server/).
The [manifest reference](https://docs.slack.dev/reference/app-manifest/) defines Messages-tab configuration and event subscriptions.

The local evidence file is `scripts/local/monoscope-slack-configuration-audit-20261101.json`.
It contains selected configuration fields, identifiers, and observation time. It contains no credentials.
The file is ignored session evidence, not a release attestation.

## Acceptance sequence

1. Establish MCP eligibility through the app's actual publication status or an authenticated MCP response.
2. Compare the branch manifest with the latest live manifest before any approved configuration change.
3. Preserve unrelated app configuration and notification destinations.
4. Use an isolated callback installation for branch acceptance, or deploy the reviewed branch through the normal authorized PR process.
5. Verify a private mention, DM, thread follow-up, account link, stop event, and revocation event in Monoscope Slack.
6. Verify the same conversation, run, artifacts, and action history through the web interface.
7. Verify interruption and ambiguous acknowledgements before streaming acceptance.

Changing this shared app affects its installations. This audit does not authorize a global manifest update or a branch deployment.

## Authorized QA installation

The user authorized creating and installing the separate QA app on 1 November 2026.
Slack installed **Monoscope Local QA** (`A0CDFCWNBNK`) in **Monoscope** (`T0AP82L853K`).
The installed-app portal offers “Reinstall to Monoscope”; real `auth.test` verifies bot user `U0CE0F6UDC4`, bot `B0CDWJNP6MQ`, and all eleven requested bot scopes.
The bot-only installation adds no personal OAuth grant. Credentials remain in an ignored local file with mode `0600`.
The receipt is `scripts/local/monoscope-local-qa-installation.json`; it contains no credentials.
Slack verified the QA `/slack/events` callback through the local tunnel. The settings portal confirmed “Success!” after saving `message.im` and `app_mention` subscriptions; a disabled Save Changes button confirms no unsaved changes. The isolated server runs against a temporary test database, with the QA installation associated through the existing token persistence function. This does not prove the project OAuth association flow or live agent behavior. Full live acceptance remains pending. The existing shared app and production deployment are unchanged.

## First live callback acceptance

A real message from the current human account to the QA app reached the signed callback, retained one event receipt, and completed its background job. Slack accepted the private identity-link response. The actual identity-link HTTP GET and POST handlers returned 200 against the isolated database, and the routine installation HTTP handler created an active Slack routine with Autonomous policy. The installation association itself was seeded through the existing token persistence function, so this does not verify project OAuth installation.

The first linked request reached the real model and posted progress, but final publication entered reconciliation. Its retained chart and artifact URLs revealed a QA host missing its required trailing slash. The isolated harness now supplies the canonical host configuration; that first attempt is not a passing live investigation.

The identity-link construction also independently assumed a trailing slash. Commit `6ea22f317` normalizes that link, matching the existing Slack evidence-link convention. The existing authenticated-link integration test first failed on `https://qa.monoscope.testslack/link/…`, then passed after the one-line fix. All three personal-linking cases pass in 0.9323 seconds; logs are `/tmp/slack-identity-host-red.log`, `/tmp/slack-identity-host-green.log`, and `/tmp/slack-personal-linking-three-green.log`. The pinned formatter passes both changed files. Scoped distillation, constraint-evasion, and client-locality reviews found no material follow-up for this change; no client behavior or effect contract changed. These checks are not final whole-branch signoff.


With the corrected QA configuration, a real plain-text request completed through the signed callback, production background worker, real model, shared run, and Slack publication. The run succeeded. `conversations.replies` returned one final “Hello!” message with metadata publication `1b75fa71-49e2-4736-a99c-11f93cca119c`, matching the confirmed database timestamp `1793528504.437829`; its conversation/artifact link uses the correct host. An authenticated GET of that conversation returned 200 and retained the answer.

A locally generated, correctly signed replay of the exact accepted Slack payload returned 200 and left all counts unchanged: seven event receipts, one run, one publication, one confirmed final reply. This tests duplicate signed delivery; it is not a provider-induced retry or worker crash. The private local receipt is `scripts/local/slack-live-qa-acceptance.json`. The QA source is snapshot `a046ab8c8` plus the identity-link fix from `6ea22f317`; later unrelated canonical-branch changes were not silently included. Private-channel mention, streamed delivery, selected review, MCP eligibility, and proactive recovery still require live acceptance.


## Streaming provider recovery probe

Real calls to `chat.startStream`, `chat.appendStream`, and `chat.stopStream` succeed in the controlled QA DM thread. Slack's [start reference](https://docs.slack.dev/reference/methods/chat.startStream/) supports caller-supplied task IDs in chunks. Live `conversations.replies` exposes those IDs as `task_card.task_id` while `streaming_state` is `in_progress`. This supplies a correlation marker before completion metadata exists.

A separate Python worker reserved task ID `f92ab818-e8dc-494e-a86d-f74e5e0acf8a`, sent `chat.startStream`, then exited with code 42 before reading its response body. Its durable record held no timestamp. A fresh process recovered exactly one open stream by matching that task ID, app, bot, and thread in live history. It then reserved a different append task ID and ran another worker that exited before reading the append response. A fresh process found that completed append task once, did not retry the append, and stopped the same stream. Final history contains one completed stream with the matching publication metadata. The local receipt is `scripts/local/slack-stream-interruption.json`; the normal start/append/stop probe is `scripts/local/slack-stream-probe.json`.

This proves provider-level correlation for task chunks under lost response bodies. It does not implement application streaming, prove arbitrary markdown append deduplication, or verify delayed/absent history, pagination, rate limits, user cancellation, and timeout. The application must persist each uncertain operation before sending it, retain inconclusive outcomes, and reuse the existing publication authority. Task IDs provide evidence to inspect; a missing history result does not authorize a duplicate start. These QA probe messages are separate from the earlier single agent final reply.


## Application streaming review

The idle-deadline regression first failed because an unchanged progress snapshot skipped the stream deadline check.
The progress loop now polls active and uncertain streams without new activity.
The isolated regression passes in 4.5194 seconds.
The four application recovery cases pass together in 15.6021 seconds.
They cover uncertain start, append, and stop recovery plus the idle deadline.
These cases use the actual application and database with controlled Slack responses.
They do not establish live acceptance of the application streaming path.

Type review found that two private types exposed `Generic` instances.
Generic decoding constructed an invalid `SlackTimestamp` and an empty-title `StreamFrame` despite their smart constructors.
Two failing-first doctests reproduce those bypasses.
The private types now omit `Generic`.
An explicit frame encoder preserves the existing `title` and `tasks` fields.
The fixed JSON fixtures, rejection cases, and round trips pass.
All 1,040 examples in the focused doctest dependency graph pass.

The focused runner uses the repository doctest arguments with `Models.Apis.Investigations` as its target.
It selects the current Cabal plan's Relude unit to avoid duplicate installed units.
It does not rebuild the libraries or change project build flags.
The runner is `scripts/local/FocusedDoctests.hs`.
Logs are `scripts/local/slack-stream-refined-types-{red,green}.log`,
`scripts/local/slack-stream-idle-{red,green}.log`, and
`scripts/local/slack-stream-refined-recovery-four-green.log`.
HLint reports no hints across the four changed streaming runtime modules.
Source hashes are in `scripts/local/slack-stream-refined-source.json`.
These files are ignored local evidence, not release attestations.
The broader bot suite passes all 459 examples in 162.2230 seconds.
Its complete log is `scripts/local/slack-stream-bots-459-green.log`.
The accompanying `scripts/local/slack-stream-bots-459-source.json` records the tested source hashes.
This checkpoint includes the scoped artifact-heading assertion and predates the subsequent atomic Slack-identity regression.
Full streaming signoff remains open.

After the atomic Slack-identity fix, all 460 bot examples pass in 173.5638 seconds.
The log is `scripts/local/atomic-slack-identity-bots-460-green.log`.
The input hashes in `scripts/local/atomic-slack-identity-bots-run-source.json` match after the run.
This checkpoint also includes the narrower start-rejection helper, which cannot delete an append or stop reservation.
It remains fixture-backed application acceptance; the QA app has not yet exercised this application streaming path live.

## Bounded journal and stream locality

The post-locality checkpoint passes all 460 bot examples in 174.8782 seconds.
The log is `scripts/local/slack-stream-locality-bots-460-green.log`.
The hashes in `scripts/local/slack-stream-locality-source.json` match after that run.
The single-caller reconciliation helper now lives beside its caller and captures its publication context.
All four recovery cases pass after that reduction in 16.0106 seconds.

Slack's [streaming reference](https://docs.slack.dev/reference/methods/chat.startStream/) describes task updates by ID and a lifetime limit of fifty unique tasks.
The application's journal retains only fifty recent events.
A new application regression confirms that the stream previously stayed open beyond that window.
The open stream could therefore receive task slots reassigned by the bounded projection.
The failing-first regression reports `expected: True, but got: False` in 13.3036 seconds.

The stream now closes when the journal window fills.
Its stop request retains the last acknowledged task frame.
The existing message refresh replaces that frame with the current bounded snapshot and its “recent activity only” notice.
The regression verifies that transition before the model returns its answer and checks one start, one stop, and one final reply.
It passes in 4.5489 seconds.
Logs are `scripts/local/slack-stream-journal-limit-{red,green}.log`.
HLint reports no hints in the four streaming runtime modules.
All five stream cases pass together in 20.7497 seconds.
The log is `scripts/local/slack-stream-five-green.log`; input hashes are in `scripts/local/slack-stream-five-source.json`.
The combined broader suite then passes all 461 bot examples in 179.3629 seconds, including the wiki-result link fix in `57f00e4ef`.
The log is `scripts/local/slack-stream-bots-461-green.log`; matching observed source hashes are in `scripts/local/slack-stream-bots-461-source.json`.

The scoped `hs-distill`, `hs-evasion-review`, and `hs-lob-review` loop found no further material issue in this streaming slice.
The frame encoder stays explicit because exposing `Generic` would bypass its invariant.
The task types and history reader are shared; there is no second worker or publication store.
The reconciliation helper stays beside its only caller.
No new browser behavior or styling was introduced.
Whole-branch review and remaining live application acceptance remain open.


## Private-channel application acceptance

An independent API readback confirms one completed progress stream and one final reply from QA app `A0CDFCWNBNK` and bot `B0CDWJNP6MQ` in the controlled thread.
Their metadata types are `monoscope_investigation_progress` and `monoscope_investigation_reply`.
The live source snapshot matches the current model and integration modules.
It predates the local reconciliation cleanup, journal-window guard, and wiki-result URL improvement; those differences remain covered by the recorded application regressions, not by this live readback.

The separate QA app is installed in the private channel `monoscope-local-qa-20261101` (`C0CERB6P8QY`). Slack confirms that its only two members are the QA bot and the current human account. The temporary channel-creation scope was removed; the temporary token was revoked and the replacement installation retains only the original eleven bot scopes. The current human account restored the bot's channel membership through Slack's own invite command.

A real human app mention reached the signed callback and production job runner. The unlinked-user response was accepted as an ephemeral message; its visual appearance was not established. The actual identity-link GET and POST handlers and routine activation handler returned HTTP 200. The resulting routine uses Autonomous policy. The isolated fixture seeds the installation association and member session; this does not verify project OAuth association.

A second real mention requested one wiki page. The real model and shared executor saved **Slack QA acceptance**, version one, with the requested body and the writing run as author. Run `add3c313-5dbd-4a61-b84b-9ecc31a855f3` succeeded. Live Slack history shows one completed progress stream and one final reply in thread `1793531889.016649`. Authenticated conversation and wiki GETs both returned HTTP 200 with the saved title.

Replaying the exact accepted payload with a locally generated valid Slack signature returned HTTP 200. Before and after counts remained two event receipts, one run, one wiki revision, one operation receipt, one final publication, one progress record, and sixteen journal entries. This establishes duplicate-delivery handling at that checkpoint, not provider-induced retry or application host-loss recovery. The private local receipt is `scripts/local/slack-private-stream-acceptance.json`; the source snapshot is `scripts/local/slack-stream-live-source.json`.

The response exposed a usability issue: it returned the raw wiki ID and a conversation link, rather than a direct wiki link. Shared wiki read/write results now include the canonical wiki URL, using the saved page path and existing host-path helper. The existing recovery integration test first failed because the URL was absent, then passed in 0.7442 seconds. It checks both read and write results, retains the existing title field, rejects cross-project disclosure, and preserves the original writing run and stale-edit behavior. Logs are `/tmp/wiki-tool-link-{red,green}.log`. The server watcher compiles all 167 modules; HLint reports no hints for `Pkg.AI`, and the pinned formatter passes both changed files. Scoped distillation, evasion, and locality reviews found no further material issue: one local renderer serves both read and write, no authority or effect contract changed, and no new client behavior was introduced. All 461 bot examples then passed in 179.3629 seconds with unchanged recorded source hashes. The log is `scripts/local/wiki-tool-link-bots-461-green.log`; hashes are in `scripts/local/wiki-tool-link-bots-source.json`. This is focused regression evidence, not whole-branch release signoff or live verification of the improved reply text. Live cancellation, application interruption, rate limits, selected review, proactive recovery, and MCP eligibility remain separate acceptance work. The local server and temporary tunnel must remain running for callbacks.


## Live cancellation checkpoint

A real human mention in the private QA channel started a read-only investigation. After the database acknowledged its stream as `StreamReady` and the shared run was `running`, the probe delivered a locally signed `agent_session_stopped` event through the public QA callback. Run `0730b020-133b-40f0-81df-52fcdfd9df20` became `cancelled`. Slack history shows one completed progress stream and one “Investigation stopped. Send another message to continue.” confirmation.

The retained journal contains one model start/return, one tool start/return, and `InvestigationInterrupted`, preceded by initialization: six entries total. The stop occurred between model/tool steps, not during an in-flight model request. No final-answer publication was created, and the project still has one wiki revision. Replaying the accepted stop event with a fresh valid signature returned HTTP 200 and retained exactly one processed stop receipt, the cancelled run, six journal entries, one model start, zero final publications, one progress stream, and one stop confirmation. The private receipt is `scripts/local/slack-live-cancellation.json`.

An earlier read-only probe finished successfully before the stop was injected because Slack converted the numeric correlation marker into a phone link. It is not counted as cancellation evidence. The repeated probe used a plain-text marker. These checks use the existing isolated QA snapshot and real Slack/model/worker; the stop source is generated locally. Native Stop-button interaction, in-flight provider interruption, and arbitrary host-loss acceptance remain open.

## Streaming semantic review checkpoint

Repeated scoped hs-distill, hs-evasion-review, and hs-lob-review checks find no material remaining issue in the streaming implementation. Progress, replies, and incidents share history pagination and correlation; the stream uses the existing progress reservation and refresh job, while final answers retain their existing publication ledger. Timestamp parsing and task types are consolidated. Opaque frame/timestamp types reject constructor bypasses; uncertain start/append/stop requests are persisted before HTTP. Definite rejection is the only path that releases a reservation. Current-role checks remain at publication and reconciliation boundaries. No client script, effect escape, warning suppression, or duplicate executor was added.

The final scoped formatter passes all five changed Haskell files. HLint reports zero hints across the four runtime modules (`/tmp/slack-stream-commit-hlint.json`), and `git diff --check` passes. All 461 bot cases pass at the source checkpoint recorded in `scripts/local/wiki-tool-link-bots-461-green.log`. This is a scoped implementation checkpoint; whole-branch review, release signoff, and the remaining live acceptance items are still required.


## QA MCP prerequisite inspection

The QA app's authenticated Agents settings show both “Mark this app as an agent app” and “Enable Slack MCP Server” switched on. This is configuration evidence, not a successful MCP session. Slack's current [sample-app instructions](https://docs.slack.dev/ai/slack-mcp-server/developing/) identify the Agents switch as the prerequisite; the [MCP overview](https://docs.slack.dev/ai/slack-mcp-server/) requires a user OAuth token from an internal or Marketplace app. The installed QA bot token is not a substitute for the requester's user grant. Actual requester consent, live schemas/session behavior, and audience-containment acceptance remain open.


## Live requester OAuth and MCP diagnosis

The actual requester authorization start returned HTTP 302 with eight read scopes and an S256 PKCE challenge. Slack first rejected the temporary callback URI because the QA app had no matching redirect. The QA app now registers `https://finishing-joint-ballet-glad.trycloudflare.com/slack/oauth/callback`; no shared app configuration changed. Native Slack consent showed only identity, search, and message-history reads, and the current human account approved that QA user grant.

The application callback then returned HTTP 400. A fresh diagnostic authorization and direct exchange established the cause: Slack's live user endpoint returns `token_type: "Bearer"`, top-level `user_id` and `scope`, and a `team` object, with no nested `authed_user`. Our parser expected `token_type: "user"`. The callback integration regression reproduced the same HTTP 400 before the fix (`/tmp/slack-oauth-bearer-red.log`). Commit `eda7d9e07` accepts the observed shape only with an explicit user identity, rejects conflicting or empty subjects, and retains the existing requester/workspace, read-scope, refresh, and consent checks. All 25 focused OAuth examples pass in 6.7619 seconds, including callback storage, unchanged bot credentials, refresh, wrong subjects/workspaces, conflicting identity fields, and write-scope rejection. The log is `scripts/local/slack-oauth-bearer-25-green.log`. Server compilation, pinned formatting, HLint, and `git diff --check` pass. Repeated scoped distillation/evasion/locality reviews find no further material issue; the existing parser and grant type remain the only path, with no effect or client behavior change.

The diagnostic requester token matches the QA app, workspace, and current human account. Real MCP initialization and `tools/list` return HTTP 200 from Slack MCP 1.0.0. The live catalog advertises `slack_search_public`, `slack_search_public_and_private`, `slack_search_channels`, `slack_read_channel`, and `slack_read_thread`. The canonical allowlist intentionally excludes the additional channel-search tool. A direct `slack_read_thread` call for the private QA cancellation thread returns HTTP 200 with text content; it reads no unrelated conversation. The captured schemas use `channel_id` and `message_ts`; the result wraps its formatted messages in a JSON text block. Private diagnostic credentials and responses remain under ignored `scripts/local/` files with mode `0600`.

This proves provider eligibility, native consent, and the diagnostic transport/read shape. It does not yet prove the fixed callback against a reloaded QA server, encrypted live-grant storage, execution through the canonical application MCP client, Slack Connect, or full audience containment. The original QA server still uses its recorded snapshot. Those acceptance items remain open.

The subsequent full doctest attempt failed before running doctests: a concurrently edited temporary browser adapter lacked its Hasql imports when Cabal read it. The current watcher later compiles that adapter successfully. The failed run is `/tmp/slack-stream-final-doctests.log`, not a passing doctest result; doctests must be rerun after the OAuth fix and stable test inputs.


## Fixed callback and channel-read integration

The isolated QA watcher was reloaded with the canonical OAuth fix and current server sources. Its old temporary database was saved to a private logical backup before recreation; restoration of that backup has not been tested. A fresh real human mention produced a delivered identity-link request. The actual identity-link GET/POST and routine installation handlers returned HTTP 200. Native Slack requester consent then completed through the fixed application callback, which returned HTTP 302 to the integrations page. The database retains one encrypted grant bound to the expected workspace and human Slack user, with the eight requested read scopes. The private receipt is `scripts/local/slack-evidence-qa-fixed-oauth.json`.

A real request to read only the earlier private QA cancellation thread reached the canonical application MCP discovery path. It found Slack's current tools and schemas. However, run `63c05436-92bc-411d-9d64-22ba6a6c8f5a` did not complete the read: the model lacked the current channel ID and fell back to workspace search. The audience guard rejected that broader search before provider execution. This is a failed read-acceptance attempt, not a working thread read. The private journal is `scripts/local/slack-evidence-application-missing-context.json`.

A failing-first integration regression reproduced the missing authenticated origin in the actual model system message. The shared prompt now carries the original workspace/channel/thread from the existing typed access context for both Slack threads and slash commands. Slash requests retain a null thread timestamp. No new authority source or capability schema layer was introduced. All fourteen destination-confinement cases passed in 4.4042 seconds; `/tmp/slack-request-context-{red,fourteen-green}.log` retain that checkpoint.

After reloading that fix and repeating native requester OAuth, run `8057bf5e-870b-4b57-8389-5c72c154e285` supplied the correct channel and called `slack_read_thread`, but the channel argument guard rejected Slack's advertised `response_format: detailed` option. Its successful run status does not establish a successful evidence read. The preserved result is `scripts/local/slack-evidence-application-acceptance.json` at this checkpoint. The regression then reproduced `expected: Right "Channel evidence read."; got: Right "Slack read unavailable."` before the next fix.

Commit `da1c23735` combines the authenticated request context and support for the declared presentation option in the existing argument filter. It keeps the destination override, live membership checks, timestamp validation, read-only catalog, schema freshness check, and requester grant unchanged. The expanded tests verify that the presentation option reaches the provider while attempted foreign channel/workspace arguments remain bound to the authorized destination. All fourteen cases pass in 4.3691 seconds. Logs are `/tmp/slack-read-format-{red,fourteen-green}.log`. Pinned formatting, HLint, and `git diff --check` pass; repeated scoped hs-distill, hs-evasion-review, and hs-lob-review checks find no further material issue. The broader bot watcher run is in progress with input hashes in `scripts/local/slack-request-scope-bots-source.json`.

The final format correction still needs to be copied into the QA snapshot and verified with the same real read request. The callback/encrypted-store acceptance is complete at its isolated checkpoint; the canonical provider read, final source link/publication, Slack Connect, and remaining audience tests remain open. The current server and temporary tunnel continue to host only the controlled QA project.


## Canonical requester-bound read acceptance

After the final format correction, the QA server's tracked server/shared sources and migrations match the canonical branch. Fresh native requester consent completes through the actual callback and encrypted storage again. The same real human request, without a channel ID supplied in its text, now succeeds through production job execution, the real model, canonical tool discovery, and `slack_read_thread`. Run `40e0fd72-94e5-4d9f-ad44-4dde5463e977` succeeds. Its retained evidence records requester Slack user `U0ANYNKCL2G`, workspace `T0AP82L853K`, channel `C0CERB6P8QY`, parent timestamp `1793532802.157869`, `response_format: detailed`, and `ReadSucceeded`. It contains the requested stop confirmation. The conversation retains `SlackChannelAudience` for that exact workspace/channel.

Slack history contains one completed progress stream and one confirmed final reply quoting the stop message. A locally signed replay of the accepted event returns HTTP 200 and leaves one run, sixteen journal entries, one progress record, one confirmed final publication, zero wiki revisions, and zero mutating operation records unchanged. The authenticated conversation view returns HTTP 200 with the quote; anonymous access redirects with HTTP 302. This establishes the successful scoped read and replay checkpoint, not arbitrary worker interruption, all audience boundaries, or Slack Connect. The private receipt is `scripts/local/slack-evidence-application-acceptance.json`; source hashes are in `scripts/local/slack-evidence-live-source.json`.

All 461 bot examples pass in 175.7491 seconds after the OAuth, request-context, and format fixes. Their recorded Haskell inputs remain unchanged. The log is `scripts/local/slack-request-scope-bots-461-green.log`, with hashes in `scripts/local/slack-request-scope-bots-source.json`. The subsequent `make test-doctests` run passes all 2,068 examples with zero errors or failures; its log is `/tmp/slack-evidence-final-doctests.log`. This is a source checkpoint, not whole-branch release signoff.

The final citation is still a usability failure. The model generated `https://slack.com/archives/…`, and the authenticated browser opened the unrelated default workspace's glitch page. The MCP text result contains message timestamps but no source URL. Slack's [deep-linking reference](https://docs.slack.dev/interactivity/deep-linking/#link-to-messages) recommends `chat.getPermalink` for message links. A real call with the same diagnostic requester grant and its existing read scopes returns the correct workspace-specific permalink for the controlled thread. The application must provide a canonical citation rather than leaving the model to invent its host. The read itself and final publication are verified; usable citation acceptance remains open. The server and temporary tunnel remain running.
