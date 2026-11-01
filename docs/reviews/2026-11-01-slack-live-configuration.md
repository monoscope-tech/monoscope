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
The QA callback and live branch behavior still need verification. The existing shared app and production deployment are unchanged.
