# Incident message capture

Install or reconnect Slack from the project's integrations page or onboarding.
The initiating Monoscope user must be an active project admin and stay signed in
as the same user when Slack returns to `/slack/oauth/callback`.
Installation requests expire after 15 minutes and can be used once. If the
exchange fails or the request expires, start again from Monoscope.
Do not construct an OAuth URL with a project ID as `state`; the callback rejects it.

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
Stop-event handling and durable status reconciliation remain rollout gates.

Set `SLACK_SIGNING_SECRET` and `SLACK_APP_ID` for the Slack app that posts incident alerts.
The signing secret verifies incoming requests. The app ID verifies the author of a captured message.
Events, slash commands, actions, and external-option requests verify the original body
before decoding. Missing or invalid signatures return 401; an empty signing secret
makes these endpoints return 503. An empty app ID prevents root capture.

Merge `incident-metadata.json` into the app manifest's metadata configuration before enabling incident messages.
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
