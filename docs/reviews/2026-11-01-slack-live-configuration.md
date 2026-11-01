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

Slack history contains one completed progress stream and one confirmed final reply quoting the stop message. A locally signed replay of the accepted event returns HTTP 200 and leaves one run, sixteen journal entries, one progress record, one confirmed final publication, zero wiki revisions, and zero mutating operation records unchanged. The authenticated conversation view returns HTTP 200 with the quote; anonymous access redirects with HTTP 302. This establishes the successful scoped read and replay checkpoint, not arbitrary worker interruption, all audience boundaries, or Slack Connect. That checkpoint is preserved in `scripts/local/slack-evidence-before-permalink.json`; source hashes are in `scripts/local/slack-evidence-live-source.json`.

All 461 bot examples pass in 175.7491 seconds after the OAuth, request-context, and format fixes. Their recorded Haskell inputs remain unchanged. The log is `scripts/local/slack-request-scope-bots-461-green.log`, with hashes in `scripts/local/slack-request-scope-bots-source.json`. The subsequent `make test-doctests` run passes all 2,068 examples with zero errors or failures; its log is `/tmp/slack-evidence-final-doctests.log`. This is a source checkpoint, not whole-branch release signoff.

At that checkpoint, the final citation was a usability failure. The model generated `https://slack.com/archives/…`, and the authenticated browser opened the unrelated default workspace's glitch page. The MCP text result contains message timestamps but no source URL. Slack's [deep-linking reference](https://docs.slack.dev/interactivity/deep-linking/#link-to-messages) recommends `chat.getPermalink` for message links. A real call with the same diagnostic requester grant and its existing read scopes returns the correct workspace-specific permalink for the controlled thread. This exposed the missing canonical citation described below. The successful read and final publication alone did not establish a usable source link.


## Verified source-link acceptance

Commit `b8d22c46d` obtains `chat.getPermalink` with the same requester grant after a successful canonical thread read. It accepts only an HTTPS Slack URL for the bound channel and timestamp, rejects conflicting destination query parameters, and checks the current grant again before releasing evidence. The opaque permalink has no decoding or `Generic` bypass. The existing typed evidence record retains the source; the bounded model response includes it with an instruction to cite it verbatim. No new executor, operation ledger, scope, dependency, or schema migration is introduced.

Real run `366a4798-0a67-4054-a07e-65260696cd75` succeeds through the production worker, real model, canonical MCP discovery/read, and requester-bound permalink lookup. Its final reply quotes the controlled stop confirmation and uses the provider's `monoscope-workspace.slack.com` source URL. The authenticated browser opens the published reply, clicks its actual rendered source hyperlink, and displays the original QA cancellation request and stop confirmation in the correct private channel. The rendered query has `&cid`, without a literal HTML entity. The private receipt is `scripts/local/slack-evidence-application-acceptance.json`; the QA source checkpoint is `scripts/local/slack-permalink-live-source.json`. A fresh locally signed replay of this actual event returns HTTP 200 and retains one run, twenty journal entries, one final publication, zero mutating operations, and zero wiki revisions. The authenticated conversation returns HTTP 200 with the quote; anonymous access redirects with HTTP 302. This replay does not simulate provider retry or process loss.

Scoped review then found two failure-path defects: rejected credentials could be treated as absent citation metadata, and a metadata HTTP 503 raised an untyped exception that discarded an otherwise successful read. Both fail first in `/tmp/slack-permalink-authority-red.log`. Commit `7e4229eb0` gives the existing rate-limit wrapper an opaque HTTP-status exception and handles it through the existing effect row. Credential rejection discards private evidence; rate limits, transport failures, invalid links, and metadata service failures leave a successful read available without an invented citation. The handler catches only the declared provider/HTTP exceptions and preserves cancellation.

All 32 parameterized thread/slash confinement and citation cases pass alongside the complete native-review group: 82 examples, zero failures in 370.1305 seconds. The log is `scripts/local/slack-citation-native-82-green.log`. Coverage includes wrong target/query/host, missing source, oversized content, rate limiting, grant removal, body-level credential rejection, HTTP 401, and HTTP 503 through the real rate-limit wrapper. Pinned formatting, HLint, and whitespace checks pass for the changed runtime/test files. Repeated scoped distillation, evasion, and client-locality reviews found no further material change; the explicit private representation and narrow exception handling are retained.

The live QA snapshot contains `b8d22c46d`, not the subsequent credential-failure follow-up. The broader bot run passes all 479 examples in 173.1273 seconds with unchanged recorded Haskell inputs. Its log and input record are `scripts/local/slack-citation-bots-479-green.log` and `scripts/local/slack-citation-bots-479-source.json`. `make test-doctests` passes all 2,068 examples with zero errors or failures (`/tmp/slack-citation-final-doctests.log`). All 41 incident-delivery cases also pass in 13.1111 seconds (`scripts/local/slack-citation-incidents-41-green.log`), including proactive policy, recorded recovery/resolution, selected-review continuation, and delivery reconciliation. Recorded Haskell inputs still match after both runs. These checkpoints do not establish whole-branch signoff, Slack Connect, all audience boundaries, or arbitrary application host-loss recovery.


## Proactive channel controls against the QA server

Locally signed slash-command HTTP requests against the isolated QA installation pass opt-in, duplicate opt-in, and opt-out. An invalid signature returns HTTP 401 without changing state. Valid requests return ephemeral responses, retain one channel routine in Observe, and do not create an investigation. Opt-out leaves that same routine paused. The owner-only receipt is `scripts/local/slack-proactive-controls-acceptance.json`. This is real handler/database acceptance with locally generated signatures; native Slack slash delivery, incident execution, measured recovery, and closing reports remain unverified live.


## Current-runtime proactive and citation acceptance

The QA watcher now loads the canonical runtime and migrations, including `7e4229eb0` and migration `0282`. Hash comparison confirms those files match. Its worker now receives `Routes.mcpProjectTools`, as production startup does. The temporary harness adds only loopback fixture controls; the public tunnel returns HTTP 404 for those controls. All fixture code stays outside the canonical branch. The prior temporary database has an owner-only logical backup; restore remains unverified. Source evidence is `scripts/local/slack-proactive-live-source.json`.

Project `e11c591d-5346-49bd-9572-3b1be4350c99` uses the same installed QA app and two-member private channel. Fresh identity linking, routine installation, native requester consent, and the real OAuth callback succeed again; one encrypted, correctly bound user grant is stored. Locally signed opt-in creates an Observe incident routine. The fixture records explicitly synthetic values through the existing incident domain functions, then uses the production worker, real model, shared run/completion, and real Slack outbox delivery:

- Synthetic alert value 84 above threshold 60 produces one analysis and one real thread reply. The report explicitly identifies the test and refuses to infer production impact or recovery.
- A measurement gap leaves the episode active, with no new analysis or closing report.
- Explicit synthetic recovery value 18 produces one closing analysis in the same thread. Its report identifies the recorded recovery and preserves the boundary from production telemetry.
- Repeated delivery sweeps retain exactly two incident runs and two analysis publications, with no project mutations. Both retained run links return HTTP 200 with their results.
- After opt-out, a second synthetic alert/recovery still delivers normal notifications, starts no additional analysis, and leaves both episodes recovered with no pending deliveries. The incident routine remains paused.

Runs `7c0bda8a-1a76-4517-a7a9-8804a3523e32` and `4978d15c-04d8-4681-98cb-20a56dec2ae2` retain those analyses. The receipt is `scripts/local/slack-proactive-live-acceptance.json`. This establishes synthetic event-to-Slack acceptance, replay, and opt-out through the shared application path. It does not establish telemetry ingestion, monitor measurement, native slash delivery, or arbitrary host interruption.

A fresh real human request also repeats canonical requester-bound discovery/thread read and permalink lookup on this current runtime. Run `c2753e4c-2f85-4cbb-ba26-4bdf3fcdabba` succeeds. Its single final reply quotes the controlled stop confirmation and uses the exact verified provider source URL. A locally signed replay leaves three total runs (two incident, one requested), sixteen journal entries, one requested final publication, zero operation records, and zero wiki revisions unchanged. The authenticated conversation returns HTTP 200 with the quote; anonymous access redirects with HTTP 302. Evidence is `scripts/local/slack-current-source-citation-acceptance.json`. The earlier 479/41/2,068 verification still matches the runtime inputs. Slack Connect, remaining audience boundaries, native controls, and application interruption acceptance remain open.


The existing [visual review](https://chatgpt.com/space/page_9c75b6b56b0481918e666edd7a89a919) now reflects these proactive and citation checkpoints. Page sequence 60 retains the same review structure, twelve walkthroughs, 102 steps, and 114 source entries. Local Chromium passes all steps at 320, 390, 768, and 1280 pixels with no overflow or JavaScript errors; both fixture captures decode. Hosted source readback exactly matches the 232,609-byte local artifact. Evidence is `scripts/local/agent-artifact-proactive-refresh.json` and `scripts/local/unified-artifact-refresh-checks.json`. Hosted embedded interaction remains separately unverified.


## Autonomous publication after shared admission and routing fixes

Commit `4b9b16924` reuses the atomic operation's authority evaluator when admitting external actions. A failing-first application table shows a captured Slack post after each of six restrictions: role loss, Observe policy, newly selected review, a narrower audience, increased evidence permission, and identity unlinking. All twelve wiki/publication cases now pass. Claim denial rolls back its record instead of describing an action that never started as uncertain. No new authority store, executor, migration, interpreter, or approval prompt is added.

The first live publication attempt did not exercise that guard. Run `5ce569e9-41d0-446c-90e5-516f96e0571a` returned “No daily report found” without an operation because substring-based report detection intercepted the compound instruction. Its terminal succeeded status is not passing publication acceptance. The preserved private receipt is `scripts/local/slack-admission-before-report-routing-fix.json`. The queued-command regression reproduces that exact reply (`/tmp/slack-report-routing-red.log`).

Commit `ada067673` recognizes complete saved-report commands and uses the existing typed report-period parser. Compound instructions stay on the shared agent path. The broader watcher passes 491 examples (486 bot plus five GitSync) in 179.6252 seconds with unchanged recorded inputs. All 2,076 doctests pass. The full native group also passes with all three queued Slack cases: 53 examples, zero failures in 347.5340 seconds. Logs are `scripts/local/slack-report-routing-bots-gitsync-491-green.log`, `scripts/local/agent-effect-admission-native-53-green.log`, and `/tmp/slack-report-routing-doctests.log`. Runtime HLint, pinned formatting, whitespace checks, and repeated scoped three-skill review have no remaining material finding.

The QA server now loads committed snapshot `ada067673`, including the matching bounded-rationale native fixture. Project `a6b28521-b8f7-465f-bef3-f9f0c792fbf2` uses the same two-member private channel and QA bot. Actual identity-link GET/POST and routine-install handlers establish the fixture member and an Autonomous routine. A locally signed administrator command sets only this QA project's notification destination to that channel. No personal Slack search grant is connected.

The human sends the unchanged original publication instruction through native Slack. Run `e25a1b92-8094-4e62-9c05-c7d4b3a2119d` now calls `send_to_slack` and succeeds with one OperationReturned receipt, NoReview, and one real “QA operation claim gate stable authority cedar” message. The operation and run retain the linked requester's member ID; the saved acknowledgement timestamp exactly matches the Slack message. A locally signed replay leaves one run, eight journal entries, one requested final publication, and one operation unchanged. Slack history retains one final reply and the progress state is StreamClosed. The authenticated conversation returns HTTP 200 with the message; anonymous access redirects with HTTP 302.

The owner-only receipt is `scripts/local/slack-admission-live-acceptance.json`; source hashes are `scripts/local/slack-report-routing-live-source.json`. The logical pre-reload backups remain unverified for restoration. This is a real stable-authority publication and replay checkpoint; claim-time revocation is covered by deterministic application tests. It does not establish arbitrary host-loss recovery, native slash delivery, Slack Connect, all audience boundaries, or final release signoff.

## Native Stop and rename through the installed QA app

The isolated app now has all twelve bot event subscriptions from the branch manifest, including native Stop and title-change events. Interactivity points to the isolated `/actions/slack` callback. Saving through Slack's manifest editor and reading back in a fresh tab confirms the exact configuration. A fresh `auth.test` confirms the original bot/workspace and eleven installed scopes; no reinstall or added bot scope was needed. The shared production app was not changed. Evidence is `scripts/local/slack-qa-native-configuration-acceptance.json`.

The first native Stop cancelled its run and closed the stream, but eight callback jobs remained locked and its Stop receipt stayed unprocessed. This attempt is incomplete, not passing acceptance. The temporary harness used one three-connection test pool for both requests and OddJobs, unlike production's separate pools. Its database and ready record were preserved as `scripts/local/slack-live-qa-before-worker-pool.dump` and `scripts/local/slack-live-qa-before-worker-pool-ready.json`; restoration is unverified.

Only the ignored QA harness changed: a bracketed, separate ten-connection OddJobs pool replaces that shared pool. The runtime remains committed snapshot `ada067673`. Source and harness details are in `scripts/local/slack-qa-worker-pool-source.json`. This change resolved the reproduction; it does not prove a production pool defect or establish behavior under arbitrary pool exhaustion.

The replacement fixture is project `7ed08a92-5ee7-46cb-9450-fcb738a7089d`, with the same linked QA member, private channel, Autonomous routine, and zero personal search grants. A native DM asks for sequential read-only tools. While run `4d148ddd-41ed-4a6f-a7c5-6b4783c3f553` has ModelPending, a last ModelStarted event, and a ready progress stream, the browser clicks Slack's actual “Stop Monoscope Local QA” control. The signed native event cancels the run during inference. The stream closes, exactly one stop confirmation arrives, and no final answer or external operation follows. The journal ends ModelStarted → InvestigationInterrupted. All fourteen callbacks are processed and the job queue drains completely. Replaying the original stop with a fresh local signature returns 200 without new runs, journal entries, publications, operations, or progress records. The observer waits for worker completion rather than requiring the confirmation immediately after stream closure. Receipt: `scripts/local/slack-worker-pool-native-stop-acceptance.json`.

Slack's actual “Edit thread name” dialog then renames that thread to “QA willow stopped investigation review”. The signed native title event is processed, the same retained web conversation shows the title over authenticated HTTP 200, and run/operation counts remain one/zero. Replaying the title event creates no run. Receipt: `scripts/local/slack-native-rename-acceptance.json`.

Contextual-prompt freshness also passes through native Messages navigation. Initially Slack shows “Investigate: QA ONLY synthetic incident” plus three general suggestions. Acknowledging that issue through the authenticated application handler and reopening Messages removes the issue suggestion. Unacknowledging through the handler and reopening restores it. Fresh, real app_home_opened events are processed at both transitions; the run count stays one, all callbacks finish, and the queue stays empty. The issue is restored to its original unacknowledged state. No prompt button is submitted. Receipt: `scripts/local/slack-native-prompt-freshness-acceptance.json`.

These checks close native Stop, rename, and issue-based prompt freshness at this isolated snapshot. Channel-context prompts, shortcut and native slash intake, Slack Connect, remaining audience boundaries, abrupt host loss, and final release checks remain open.

## Native slash and message-shortcut intake

The isolated app now registers `/monoscope-qa` and the message shortcut “Ask Monoscope QA”, using the branch's existing callback ID and routes. The QA command name avoids replacing the shared app's `/monoscope` registration. The manifest editor saves successfully and fresh readback matches. Native reinstallation adds only `commands`; a fresh `auth.test` verifies the same bot/workspace and twelve installed bot scopes. The original token remains valid. Evidence: `scripts/local/slack-qa-native-intake-installation.json`.

Selecting the actual QA slash autocomplete entry and sending a read-only schema request starts run `80cf1f79-7176-451e-945d-93b8e88f1568` through the production command handler and worker at `ada067673`. The native acknowledgement links to that run. It succeeds with the linked requester and the private-channel command origin, no external operation, and an authenticated retained conversation. Both Slack replies match their publication IDs and saved acknowledgement timestamps. The model also puts prose in its optional query field, causing an unnecessary parse-error reply before the schema explanation. Intake passes; response quality remains open. Receipt: `scripts/local/slack-native-slash-acceptance.json`.

The first native shortcut attempt does not pass. Posting the controlled ordinary channel message starts no run. Selecting its More actions → Connect to apps → Ask Monoscope QA item starts exactly one shared run, with the selected message explicitly wrapped as untrusted evidence. However, the old renderer places that full wrapped input in a 655-character chart header, exceeding Slack's [150-character header limit](https://docs.slack.dev/reference/block-kit/blocks/header-block/). The first publication remains unacknowledged and the callback retries reconciliation. Its provider error body was not retained, so the precise provider rejection is not proven. Receipt: `scripts/local/slack-native-shortcut-acceptance.json`.

The renderer consolidation in `d33caabe5` bounds chart headers and preserves the shared widget path. Separately, a failing-first extension of the existing reply-batch lifecycle shows that an explicit `invalid_blocks` rejection was treated as uncertain delivery. Commit `619472c58` adds that documented rejection to the existing classification; it adds no new executor or retry system. The same lifecycle covers rate limiting and invalid blocks, preserving the first delivered part, one model call, immutable saved parts, and completed replay without another post. Before the fix, two cases produce one failure; afterward both pass in 1.0268 seconds. Logs: `scripts/local/slack-invalid-blocks-red.log` and `scripts/local/slack-invalid-blocks-two-green.log`. Pinned formatting, whitespace checks, and runtime HLint pass; repeated scoped distillation, evasion, and locality review finds no material issue. A follow-up keeps the table type explicit while using the repository’s preferred string conversion; the modified lifecycle has no HLint hint. The complete bot group passes all 490 examples in 181.7659 seconds, with unchanged recorded source hashes. Log: `scripts/local/slack-invalid-blocks-bots-490-green.log`; source: `scripts/local/slack-invalid-blocks-bots-source.json`.

The failed fixture is preserved in `scripts/local/slack-live-qa-before-native-intake-fixes.dump`. A full restore to a separate local database, with Timescale pre/post restore hooks, verifies the project, failed shortcut run, original publication reservation, and zero delivered parts. That database is removed after verification. Receipt: `scripts/local/slack-native-intake-backup-restore.json`. This establishes logical backup restoration only; it is not application or host-loss recovery acceptance. The QA watcher now runs committed runtime `619472c58`, with its separate worker-pool harness retained; hashes are in `scripts/local/slack-qa-intake-current-source.json`.


The replacement fixture is project `62e80416-6fe3-464a-9191-b69c6f0ca46b`, with the same linked member, private destination, Autonomous routine, and no personal search grant. A first retry selected another controlled QA message because the observer used a global action-button selector; it is recorded separately in `scripts/local/slack-native-shortcut-current-wrong-target.json`, not counted as reacceptance. The corrected observer matches the action button’s message timestamp to the exact original target before submitting the native shortcut.

The corrected run `6f8aa59a-38e8-4892-afa6-a72e804ffd87` succeeds for the original selected message. Both parts are acknowledged with matching metadata and timestamps in its original thread; there is one run for that target, no external operation, a processed shortcut receipt, an authenticated retained conversation, and an empty callback/job queue. Locally signing and replaying the original shortcut fields returns 200 without another run, journal entry, publication, operation, or progress record. The saved outbound chart header is exactly 150 characters. Slack’s normalized readback expands the emoji name, so it is not used to measure the outbound limit. The signed image URL returns HTTP 200 and a 900×300 PNG without a session cookie. Visual inspection shows an empty chart: this fixture contains no ingested telemetry and the request asks about schema, so chart relevance remains a response-quality issue. The native thread pane remains on its loading state during observation; actual in-client image display is not attested. Receipt: `scripts/local/slack-native-shortcut-current-acceptance.json`.

Native ordinary slash intake and message-shortcut intake are now verified at the recorded snapshots. Clean schema-only answers, context-selected prompts, Slack Connect, remaining audience boundaries, host-loss recovery, and final release signoff remain open.

## Persistent QA fixture and schema-only live acceptance

The isolated local database `monoscope_qa_agent_20261101` retains project `f656f807-fd5b-48b2-9352-a8a89e87f022`, its installation, linked identity, historical run, and authenticated conversation across watcher reloads. The fixture was restored from `slack-live-qa-before-model-query.dump`; the private `slack-qa-persistence-acceptance.json` records actual restored history and a subsequent reload. Primary and worker pools remain separate and bracketed. This ignored harness is not production code. Logical restore and normal reload do not prove arbitrary host-loss recovery.

A native command submitted during the expired callback tunnel failed with Slack's `dispatch_unknown_error` and created no run. The replacement tunnel now serves the existing callback whitelist. The QA manifest was saved and freshly read back with its callback, command, and OAuth URLs updated and scopes unchanged. The runtime uses the same host; a signed public URL-verification challenge returns HTTP 200 with the exact challenge. `auth.test` confirms the installed original bot and all twelve scopes. The shared app is unchanged. The private receipt is `slack-qa-tunnel-repair-acceptance.json`.

Resubmitting the original unchanged schema request creates run `33242503-50b6-4ee8-8e99-3d80bc7a5d13` on runtime snapshot `87c8bce79`. It calls `get_schema` once, succeeds, and publishes exactly one explanation with matching Slack metadata and timestamp. There is no query-error reply, chart, retained widget, or external operation. Authenticated conversation access returns HTTP 200. The raw provider still returns prose in its optional query field; the shared opaque KQL decoder rejects that field and retains the explanation. This closes the previously recorded schema-only response-quality failure.

A duplicate production `RunConversation` job for that completed run is consumed without changing run, journal, publication, operation, or model-usage counts. This verifies worker replay, not a duplicate native slash callback. The owner-only `slack-schema-typed-live-acceptance.json` contains the actual checkpoint, published reply, and counts; `slack-schema-typed-current-source.json` pins the runtime sources. The [widget review](2026-11-01-slack-widgets.md) records failing-first application coverage and the canonical 504-case bot run. Whole-branch signoff and the remaining plan acceptance are still open.


## Populated chart and stat acceptance

The native Monoscope QA DM now passes useful two-widget delivery and a follow-up in the same conversation.
Synthetic PostgreSQL batches of 12, 18, and 14 produce a populated line chart and a stat of 44.
The live check exposed a blank stat PNG; the signed-endpoint regression fails before the fix and passes afterward.
Both corrected images visibly display in the actual Slack thread.
The authenticated run inspector retains both widgets; anonymous access redirects.
Duplicate-event replay adds no run, journal entry, publication, or action.
The callback queue drains, there are no mutating operations, and the original routine policy is restored.
See [populated Slack charts](2026-11-01-slack-populated-charts.md) for the exact test and live evidence.
