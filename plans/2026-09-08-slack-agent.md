# Monoscope Slack agent: redesign and delivery scope

Status: proposed design, September 8, 2026. This document scopes the product and implementation. It does not claim shipped behavior.

Implementation is now in progress. Actual changes and verification are tracked in the [implementation log](2026-09-08-slack-agent-implementation.md).

## Product direction

Monoscope is the SRE agent that turns an alert into an investigation, a next action, and a verified recovery.

The primary user is an on-call engineer inside Slack. The interface must explain what changed, show evidence, and preserve the incident conversation.
The visitor mode is Operate. Monoscope's existing calm, precise design remains the authority. Slack supplies the interface components and typography.

The central rule is **one incident episode, one root message per destination, one investigation thread**.
The root shows current status. The thread preserves findings, decisions, reminders, and recovery.
Long-term autonomy grows from this incident record.

## Evidence from the current product

| Evidence | Finding | Design consequence |
| --- | --- | --- |
| Screenshot 1 | Monitor alerts and recoveries appear as separate channel posts. The chart shows no visible series. | Thread the lifecycle. Diagnose missing chart data separately from presentation. |
| Screenshot 2 | A repeated error contains a large stack excerpt and chart, with a generic `value` legend. | Put the error summary and current change first. Keep repeated updates compact. |
| Screenshots 3–4 | The reference agent explains evidence and offers actions inside a conversation. | Use contextual actions and persist decisions in the incident thread. |
| Screenshot 5 | Measurements begin partway through the selected time window. | Distinguish an empty window from missing telemetry. Never invent a line to fill the width. |
| `notifyQueryMonitorStatusChange`, `dispatchTeamNotifications` in `src/BackgroundJobs.hs` | Monitor delivery uses `Fanout{threads = Nothing}`. Recovery links to the monitor list. | Persist the episode and its original issue link across transitions. |
| `ThreadRefs`, `fanOutToTeam` in `src/BackgroundJobs.hs` | Fan-out carries one Slack timestamp across channels. | Store roots by workspace and channel. A timestamp from another channel is invalid context. |
| `sendSlackAlertWith` in `src/Pkg/Mail.hs` | The default channel uses a webhook and drops thread context. | Preserve private-channel delivery during migration. |
| `slackEventsPostH` in `src/Pages/Bots/Slack.hs` | Threaded queries work. A message without `thread_ts` receives a generic error. | Start a conversation from the incoming message timestamp. |
| `runBotQuery` in `src/Pages/Bots/Utils.hs` | Reports, queries, charts, and stored conversations exist. Backfilled Slack messages lose author roles. | Reuse the query engine. Preserve roles, evidence, and full assistant answers. |
| `allToolDefs` in `src/Pkg/AI.hs` | Tools cover telemetry queries, schema, services, and facets. | Investigation needs additional incident, deployment, code, and action tools. |

## App to agent

The public name remains **Monoscope**. Integration copy uses **Monoscope Slack agent** and **Connect Slack agent**.
The description is: “Investigate production issues, understand what changed, and coordinate recovery in Slack.”

The Slack installation remains an app technically. Slack's native Agents feature supplies the agent experience.
The current guide specifies `assistant:write`, `chat:write`, an agent overview, and Agent messaging events.
Some features require a paid Slack plan. Installation capabilities need an explicit check.
See [Slack's agent guide](https://docs.slack.dev/ai/developing-agents/).

Implement the current Agent messaging experience and its session lifecycle. Keep channel mentions and incident threads as first-class entry points.
New scopes require an installation upgrade. Existing installs retain working notifications during that transition.
The rendered Slack badge requires a workspace check. A copy change alone does not prove the native agent experience works.
See [agent sessions](https://docs.slack.dev/ai/agent-sessions/).

Suggested prompts:

- What needs attention?
- Investigate this alert
- What changed before this started?
- Show the relevant dashboard

An ordinary channel mention starts a reply under that message. Direct messages start an agent session.
Replies in an active investigation continue its context. Unrelated channel conversation does not trigger unsolicited responses.

## Incident message and lifecycle

Illustrative content below uses invented measurements. It specifies hierarchy, not production findings.

```text
Monoscope
ALERTING · MemBuffer oldest bucket age
timefusion · production
Oldest bucket: 84s · Alert threshold: 60s · Started 19:26 UTC

[Compact chart: oldest bucket age, seconds, 19:06–19:26 UTC]

[Investigate]  [Open monitor]  [More]

  Thread
  19:27  Investigating bucket age and flush failures…
  19:28  Evidence: bucket age rose alongside flush failures.
         Cause is unconfirmed. Next check: failed flush traces.
         [View evidence]
  19:42  Recovered · 16m duration
         Oldest bucket: 18s. The recovery condition passed.
         [Open incident]
```

After recovery, the root shows `RECOVERED`, the end time, and the incident link. It retains the original onset and alert snapshot.
“Investigate” runs the agent in the thread. “Open monitor” opens Monoscope. A link must not masquerade as an agent action.
The More menu contains implemented actions such as acknowledge and mute. Unavailable actions stay absent.

| Event | Root behavior | Thread behavior |
| --- | --- | --- |
| First actionable alert | Create once per destination. | Start investigation when requested or enabled by policy. |
| Still firing | Update current value and last observation. | Post only at configured reminder cadence or for material changes. |
| Warning becomes critical | Update severity in the same episode. | Explain the change. Broadcast only under an explicit escalation policy. |
| Recovery condition passes | Update status and end time. | Post one recovery summary. No new channel post. |
| Manual resolution | Show who resolved it and when. | Distinguish operator resolution from measured recovery. |
| Telemetry stops | Show data unavailable. | Explain the missing evidence. Never infer recovery from silence. |
| Alert returns | Create a new episode after confirmed recovery. | Link to the previous episode. |
| Parent is deleted or inaccessible | Record failed delivery or create one clearly labeled replacement when access permits. | Preserve the incident link and recovery context. |

Existing monitor recovery and hysteresis rules remain authoritative. An unresolved warning-to-critical transition does not create another episode.
Repeated errors remain in their active episode. A new occurrence after resolution opens a new linked episode.
Cross-monitor incident grouping is later work. Similar titles alone are insufficient evidence for grouping.

## Thread delivery design

The proposed records are:

- `incident_episode`: project, source kind, source ID, episode ID, issue ID, status, onset, and recovery time.
- `slack_incident_root`: episode, installation/workspace, channel, root timestamp, transport, and delivery state.
- `incident_delivery`: episode event, destination, idempotency key, attempt state, and Slack result.
- `investigation_run`: episode, requester, scope, evidence references, status, tool results, and pending actions.

Unique constraints serialize root creation for each episode and destination. Team routing deduplicates overlapping channel destinations.
Transitions and delivery use a durable outbox. Recovery waits for a pending root delivery instead of racing it.
Retries keep the original root timestamp. A reply's timestamp never replaces it.
Ambiguous network outcomes require reconciliation before another root post. Local deduplication alone does not guarantee exactly-once Slack delivery.

Prefer `chat.postMessage` for roots because it returns `ts`. Replies use that root as `thread_ts`, with no broadcast by default.
Root updates use `chat.update`. See [message posting](https://docs.slack.dev/reference/methods/chat.postmessage).

The repository comments incorrectly state that incoming webhooks cannot thread.
Webhooks accept `thread_ts`, but do not return a root timestamp. Slack documents Events API or message retrieval for that timestamp.
See [webhook thread replies](https://docs.slack.dev/messaging/sending-messages-using-incoming-webhooks/#posting-your-message-as-a-reply-in-a-thread).

Existing private-channel webhooks must keep delivery until bot membership is available.
The migration checks membership and scopes, captures roots through supported events or retrieval, and exposes notification-only status when conversation access is unavailable.
It must not silently lose alerts by forcing every installation onto the bot-token transport.
Old timestamps require validated channel provenance. Never copy an unscoped timestamp to every destination.
Workspace replacement invalidates old delivery bindings. Historical records remain available for audit.

## Charts for Slack

Use a dedicated compact export profile within the existing renderer. A proposed starting size is 960 × 320 pixels.
The alert text carries the current value, threshold, and time range even if Slack cannot load the image.
Charts have a neutral background, readable sans-serif labels, restrained grid lines, meaningful units, and descriptive alt text.
Remove a single-series `value` legend. Name the measurement above the chart.
Use bars for event counts and lines for measured gauges. Threshold lines include labels and units.

The requested window maps to the full plot width. First and last timestamps remain readable without clipping boundary bars.
Tick labels need not coincide with every observation. Grid margins still reserve space for labels.
Missing measurements remain gaps. Complete count buckets can show zero only when query coverage proves zero events.
Partial and missing coverage need explicit labels. A failed query produces “Chart unavailable” and a working link, not an empty successful chart.

Current code already supplies dataset `from` and `to` to the x-axis in `src/Pkg/Components/Widget.hs`.
It also sets a time-axis boundary gap of `[0, 0.01]`.
`web-components/src/chart-png-options.ts` adjusts export margins and removes a fixed grid width.
These facts do not establish the cause of the screenshot spacing or blank series.

The implementation must capture the signed request, returned dataset, final ECharts options, and PNG for each failing chart.
Check seconds versus milliseconds, query bounds, bucket timestamps, nulls, series encoding, and plot margins together.
The monitor path passes `monitor.logQuery` to a timeseries widget. Scalar monitor expressions need an explicit historical measurement strategy.
Appending event counts to an arbitrary monitor query can change its meaning. Prefer recorded evaluation values when the query has no valid timeseries form.

The chart in screenshot 5 has observations only near the end. Its image alone cannot distinguish a real onset from incomplete telemetry.
The redesign must preserve that distinction. It must not stretch or interpolate measurements across unobserved time.

The separate [hash-chart research](2026-09-08-hash-chart-research.md) addresses query cost and coverage.
That work is a dependency for expensive historical investigations, not proof that these Slack chart defects are fixed.

## Investigation behavior and the SRE roadmap

An investigation answer contains: current impact, observed facts, ranked hypotheses, missing evidence, and the next useful action.
Every material claim links to its query, trace, deployment, code revision, or incident record.
Hypotheses remain labeled until evidence supports a cause. Customer impact requires observed customer identifiers and appropriate access.
The agent reports progress, accepts cancellation, and preserves partial findings after a tool failure.

| Release | User outcome | Required work | Completion evidence |
| --- | --- | --- | --- |
| 1. Incident foundation | Follow the complete incident in one thread and read its chart. | Episode records, destination roots, durable delivery, recovery updates, chart diagnosis and export profile. | Lifecycle replay produces correct roots and replies across multiple channels. Screenshot cases render correctly. |
| 2. Native Slack agent | Mention Monoscope or open its agent experience to investigate. | Installation upgrade, typed events, sessions, progress, cancellation, incident binding, full conversation history. | Sandbox checks cover mentions, DMs, follow-ups, retries, cancellation, and old installations. |
| 3. Evidence-led investigations | Understand likely causes and useful next checks. | Incident tools, traces, metrics, before/after comparisons, dashboards, deployment and code context, runbooks, similar incidents. | A representative incident set tests evidence accuracy, attribution, useful next steps, and missing-data behavior. |
| 4. Reviewable action drafts | Review a fix, communication, or response plan. | Repository integration, sandbox execution, patch tests, communication drafts, explicit approval records. | A draft PR contains a justified diff and actual test results. Draft communication uses verified incident facts. |
| 5. Always-on SRE | Receive useful investigations before asking. | Policy-triggered runs, incident memory, change correlation, investigation budgets, outcome feedback. | Shadow-mode evaluation shows useful findings and acceptable noise before channel publication is enabled. |

“Suggest next steps” and “Pull relevant dashboards” fit release 3.
“Surface similar incidents” requires retained outcomes and evidence-backed similarity, also in release 3.
“Draft the comms” and “Draft a PR” fit release 4. A draft is reviewable before publication.
Repository permissions and code retrieval are separate from Slack permissions.
The long-term design supports bounded remediation policies, with explicit authority for each action class.
Automatic production deployment, broad shell access, and automatic incident closure are not first-release behavior.

## Engineering boundaries and acceptance

Slack event decoding must distinguish message, mention, bot, edit, context, session, and unknown event types.
Event IDs deduplicate retries. Signed request checks precede parsing and queueing. Bot messages must not trigger loops.
Project authorization applies to every tool and action, including buttons in old messages.
Workspace membership alone does not establish access to every Monoscope project.
Telemetry, code, and Slack history are evidence inputs, not instructions that grant the agent additional authority.

The first release requires these checks:

- Two channels receive distinct roots. Shared team destinations receive no duplicate root.
- Warning, critical, reminders, and recovery retain the correct episode and channel binding.
- Delayed roots, duplicate jobs, rate limits, ambiguous sends, deleted parents, and revoked tokens preserve delivery state.
- Default webhooks and bot-token destinations retain working notifications during installation upgrades.
- Count, gauge, empty, sparse, partial, failed, and single-point charts show truthful data and readable bounds.
- Text-only fallback preserves the alert when image delivery fails.
- Desktop and mobile Slack show readable messages, chart labels, action labels, and status without relying on color alone.
- Investigation links retain project, incident, and exact time range.

Track root duplication, unthreaded lifecycle messages, failed delivery, chart failures, time to first evidence, and operator-rated usefulness.
Release 1 targets zero duplicate roots and zero standalone recoveries in the controlled lifecycle suite.
Production rollout starts with an explicitly selected workspace and channels after sandbox acceptance.

## Suggested implementation slices

1. Incident storage, per-destination thread delivery, and lifecycle regression tests.
2. Chart data diagnosis, Slack export profile, and rendered fixtures.
3. Agent installation configuration, event handling, sessions, and conversation context.
4. Investigation tools and evidence format, followed by evaluation fixtures.
5. Draft actions, repository integration, and proactive investigation policy.

Relevant files include `src/BackgroundJobs.hs`, `src/Pkg/Mail.hs`, `src/Data/Effectful/Notify.hs`, and `src/Pages/Bots/Slack.hs`.
Charts span `src/Pkg/Components/Widget.hs`, `src/Web/Routes.hs`, and `web-components/src/chart-png-options.ts`.
Existing notification, Slack, agentic, and PNG tests provide the starting fixtures.

Before pushing implementation changes, run `make ci-signoff` with the relevant checks from `ci/checks.tsv`.
For combined Haskell and chart work, the proposed command is:

```sh
make ci-signoff CHECKS="frontend build doctests unit-tests integration-tests weeder hlint ui-tests"
```

The command's final status determines the remaining GitHub checks. The PR records actual commands, results, and unavailable services.
No attestation represents an unrun or failed check.

## Current delivery and remaining decisions

This pass inspected all five screenshots, repository behavior, chart options, and current official Slack documentation.
It produced this implementation brief. It changed no runtime code, Slack settings, or external messages.
Documentation checks cover formatting and local links. Runtime CI does not validate a planning document.

The proposed defaults are manual investigation first, one thread per episode and channel, and a new episode after confirmed recovery.
Remaining product choices are proactive investigation triggers, reminder cadence, and the first repository host for draft PRs.
The live installation's scopes, Slack plan, agent configuration, and channel access remain unverified.

## Design coverage review

The completed deliverable is the redesign and implementation scope requested in this task.
The release acceptance criteria above apply to future implementation, not to this document's completion.

| Requested outcome | Completed design coverage |
| --- | --- |
| Change from Slack app to Slack agent | Product naming, native Agent migration, installation compatibility, entry points, and session behavior. |
| Put resolution under the original issue | Lifecycle table, recovery example, destination-specific roots, retries, and migration rules. |
| Address chart bounds | Full-window plot contract, truthful gaps, current code findings, and a concrete diagnostic sequence. |
| Improve charts in Slack | Compact export profile, measurement labels, thresholds, chart-type choices, accessibility, and failure states. |
| Scope an always-on SRE | Five releases with dependencies, evidence requirements, action authority, and acceptance criteria. |
| Draft PRs, suggest next steps, find similar incidents, draft communications, and pull dashboards | Each capability has an explicit release and its required supporting systems. |
| Use the supplied references | All five screenshots inform the evidence table and proposed behavior. |

Repository paths, local document links, and whitespace checks passed.
Runtime code and external Slack settings remain unchanged. The chart root cause and live installation capabilities remain implementation investigations.
