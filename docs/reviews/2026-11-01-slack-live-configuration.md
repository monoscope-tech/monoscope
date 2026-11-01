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
