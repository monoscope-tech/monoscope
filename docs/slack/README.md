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
investigation threads retain their original project, and new incoming questions
recheck membership and installation access. Per-tool checks during long-running
investigations and the remaining native Agent lifecycle gates are still required
before rollout.

Set `SLACK_SIGNING_SECRET` and `SLACK_APP_ID` for the Slack app that posts incident alerts.
The signing secret verifies incoming requests. The app ID verifies the author of a captured message.
An empty signing secret makes the events endpoint return 503. An empty app ID prevents root capture.

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
