# Slack agent installation

Install or reconnect Slack from the project's integrations page or onboarding.
The initiating Monoscope user must be an active project admin and stay signed in
as the same user when Slack returns to `/slack/oauth/callback`.
Installation requests expire after 15 minutes and can be used once. If the
exchange fails or the request expires, start again from Monoscope.
Do not construct an OAuth URL with a project ID as `state`; the callback rejects it.

The OAuth request includes `assistant:write` alongside the existing scopes. The
callback stores the scopes returned by Slack. Installations with missing or
unknown `assistant:write`/`chat:write` grants receive a private reconnect prompt
before native investigation work. Existing credentials, webhook destinations,
and notification paths stay configured. The integrations page also offers the
existing admin-authorized reconnect flow. Stored grants establish permissions,
not live Agent availability or acceptance.

[`agent-manifest.json`](agent-manifest.json) is the reviewable app configuration
template. Replace every `https://YOUR_MONOSCOPE_HOST` with the deployed origin,
then compare it with the current app manifest before applying it in the selected
sandbox. It includes `agent_view`, the four suggested prompts, commands, signed
callback routes, event subscriptions, incoming webhooks, and incident metadata.
It retains the existing notification scopes. Preserve unrelated live app settings
when merging the template. The Agent declaration is required in addition to OAuth
grants; see [Slack's Agent guide](https://docs.slack.dev/ai/developing-agents/).

Slack documents migration from `assistant_view` to `agent_view` as irreversible.
This repository change does not apply that migration. Live manifest validation, plan availability, and workspace acceptance
remain rollout gates. See [the manifest reference](https://docs.slack.dev/reference/app-manifest/).

Opening the Agent's Messages tab also starts personal linking for an unlinked
user. Repeat opens suppress another prompt while a successfully delivered,
unconsumed link remains valid in that conversation. After expiry, a new visit
can issue a new link. Already-linked users and visits to other tabs receive no
onboarding prompt. App Home visits do not start investigations. A rejected prompt
leaves the receipt pending; delivery is recorded only after Slack acknowledges it.

To link a personal account, mention Monoscope or message it directly in Slack.
An unlinked user receives an ephemeral link addressed to that Slack user. Sign in
to Monoscope, choose a project, and submit the form. The project list only includes
current memberships connected to the requesting Slack workspace. The link expires
after 15 minutes and works once. This follows [Slack's account-binding flow](https://docs.slack.dev/authentication/binding-accounts-across-services/).

A Slack identity cannot be reassigned to a different Monoscope account through
this flow. A new link can change the same user's default project. Existing
investigation threads retain their original project. Signed-event investigations
recheck access before model calls, before and after tools, and before delivery.
Slash commands use the linked user's current default project. `/monoscope-here`
requires project-admin permission and changes only that project's notification
channel. Moving the channel clears the old channel-bound incoming webhook so
notifications use the bot API. Unlinked or revoked users receive a private prompt
to link or check their access.

Dashboard modals retain their project and requesting Slack user. Each selection,
preview, and share rechecks current membership. Widget definitions come from the
project's saved dashboard or its known template; modal metadata carries no file
path or chart URL. A changed widget requires a new selection. Modals opened before
this metadata upgrade must be reopened with `/dashboard`. Native Agent lifecycle
and the remaining acceptance gates still apply before rollout.

Native `app_context_changed` events retain navigation hints separately for each
workspace, app conversation, and Slack user. Newer empty context clears old hints;
delayed events cannot restore them. Context does not grant access, bind a project,
or start an investigation. Using these hints as scoped investigation evidence
remains future work.

`agent_session_title_changed` updates the stored title of an existing project
thread only for a linked current member. Its workspace must match the signed
envelope. Delayed title events cannot replace newer titles, and title updates do
not alter cancellation state. See [Slack's title event](https://docs.slack.dev/reference/events/agent_session_title_changed/).

Threaded follow-ups retain user/assistant roles and complete model answers.
Previous conversation text is not added to the system prompt. The model receives
the latest 200 stored messages in chronological order. Slack backfill ends before
the triggering event, using the documented [reply timestamp boundary](https://docs.slack.dev/reference/methods/conversations.replies/).
Only messages attributed to this app become assistant history. Backfill follows
all returned cursors and rechecks access before and after each page. A failed or
incomplete fetch stops the investigation before any model call and leaves the
receipt pending for retry. Durable tool-message replay remains incomplete.

Signed-event investigations set the native session to `processing` before work
and attempt to return it to `active` on completion or failure, using
[`agents.sessions.setStatus`](https://docs.slack.dev/reference/methods/agents.sessions.setStatus/).
A rejected startup status prevents the investigation and retains the pending
receipt. Failed status cleanup is logged without replaying a completed answer.
Signed `agent_session_stopped` events persist a cancellation cutoff for an existing
project thread. Only a linked, current project member can stop its investigation.
Running work checks that cutoff at access boundaries and every half second while
waiting for model/tool work. Cancellation attempts to clear the processing status
and posts a threaded confirmation. Later questions can start new work. Subscribe
to [the stop event](https://docs.slack.dev/reference/events/agent_session_stopped/)
before enabling this flow. Workers serialize human messages within each workspace,
channel, and thread. A competing worker leaves its receipt pending for retry;
other threads can proceed. The lock covers status cleanup and receipt completion,
and a lost lock connection interrupts blocked work. Durable status and delivery
reconciliation remain rollout gates.

Set `SLACK_SIGNING_SECRET` and `SLACK_APP_ID` for the Slack app that posts incident alerts.
The signing secret verifies incoming requests. The app ID verifies the author of a captured message.
Events, slash commands, actions, and external-option requests verify the original body
before decoding. Missing or invalid signatures return 401; an empty signing secret
makes these endpoints return 503. An empty app ID prevents root capture.

The Agent manifest includes the metadata configuration below. For a metadata-only
upgrade, merge `incident-metadata.json` into the existing app manifest before enabling incident messages.
It registers the `monoscope_incident_root` event and its `root_id` field.
Slack requires metadata schema registration; unregistered metadata can be ignored. See [Slack message metadata](https://docs.slack.dev/messaging/message-metadata/).

Configure the Events API callback as `/slack/events` on the application host.
Subscribe to message events for the selected channels and grant the corresponding history scopes.
The app must receive its own posted message, including metadata, before an accepted webhook root can release queued replies.
Slack documents the Events API as one way to obtain a webhook message timestamp. See [incoming webhooks](https://docs.slack.dev/messaging/sending-messages-using-incoming-webhooks/).

The application checks the saved signed event's author app, workspace, channel, root ID, and timestamp.
The receiving app ID alone is insufficient. A human-authored message cannot claim a root by copying its metadata.
A root timestamp cannot be replaced by a conflicting observation.

These files describe configuration; they do not apply it to a live app.
Controlled workspace acceptance and the remaining Slack-agent plan gates are required before rollout.

Error patterns remain open when error observations stop. Quiet counters do not
establish recovery or trigger automatic resolution. An operator can resolve an
error explicitly. Monitor recovery still requires a measured value that passes
its recovery condition; missing or failed evaluations show DATA UNAVAILABLE in
existing incident threads and retain the last verified reading.
