# Populated Slack charts and stat images

Date: 1 November 2026. This review covers populated replies, retained widgets, and the stat PNG fix.
Whole-branch review and final release gates remain open.

## Behavior and fix

The actual Monoscope Slack app returns a populated line chart and a stat image.
The explicit model response contains two widgets and one explanation.
A native follow-up creates another visible run in the same conversation.
Both widgets remain available in that conversation's run inspector.

The first live check exposed a blank stat PNG, despite its query returning 44.
The ECharts converter handled plotted series, but stat data contains a scalar.
The converter now renders that scalar with the existing formatStatValue function.
The PNG handler supplies the requested appearance. Missing data displays N/A; zero displays 0.

## Repeated reviews

The hs-distill pass reuses the shared converter, unit formatter, signed endpoint, and PNG process.
It adds one stat case without another renderer or dependency.
The hs-evasion-review pass follows widget decoding, scalar acquisition, signing,
the complete PNG handler, and the complete shared ECharts converter.
The change adds no warning suppression, type weakening, effect escape, or alternate execution path.
Appearance changes presentation, without altering the signed query or data.
The hs-lob-review pass finds no new browser interaction or JavaScript glue.
ECharts receives declarative text graphics through its existing renderer.
The handler regression checks actual PNG output rather than mirroring the converter's object structure.
Repeated passes find no further material issue within these changed functions.

The visual review follows the emil-design-eng format:

| Before | After | Why |
| --- | --- | --- |
| A blank stat image beside a populated line chart | A centered, formatted 44 | The image conveys the measured total. |
| No stat foreground treatment for an appearance override | Readable text in light and dark images | The value remains legible in either appearance. |

## Verification

The new endpoint regression fails before the fix: two distinct values produce identical PNGs.
After the fix, missing data, zero, and 44 produce three distinct images.
It covers standard and Slack profiles, each in light and dark appearance.
Both PNG endpoint cases pass: **2 examples, 0 failures, 8.5822 seconds**, including the existing table regression.
Logs: scripts/local/slack-stat-png-red.log and slack-stat-png-final-green.log.
Pinned formatting, HLint, and whitespace checks pass for the changed files.

Live acceptance uses only the Monoscope workspace and its isolated local QA app.
The fixture contains 44 synthetic PostgreSQL logs in batches of 12, 18, and 14.
These are directly seeded fixtures, not production telemetry or an ingestion test.
The real authenticated chart endpoint independently returns 44 before the agent request.

Both the native request and follow-up succeed through Slack callbacks and the real model.
The first run is 727bb633-75b8-46c7-9ef0-7cf65bb1f9a8; the corrected follow-up is a6f49f39-5f2e-45ad-bf8a-625856135752.
Both belong to conversation 1595fb94-2c2a-512c-a52b-dc210dad540f.
Each has three acknowledged parts: a line chart, a stat, and an explanation.
Publication metadata and Slack timestamps match the retained delivery records.
Both corrected image URLs return HTTP 200 and 900 by 300 PNGs without a session cookie.
Visual inspection confirms the plotted 12, 18, and 14 values and the 44 stat.
The actual Slack thread visibly displays both corrected images.
The stat's native background image loads at 900 by 300 pixels.

The authenticated run inspector returns HTTP 200 with both named visualizations; anonymous access returns HTTP 302.
There are no mutating agent operations, and the callback job queue is empty.
Replaying the original follow-up event with a fresh local signature returns HTTP 200.
Run, journal, publication, and operation counts remain unchanged.
This proves duplicate-event handling, not provider retry or host loss.

The routine uses Observe during acceptance. Its original Autonomous policy is restored through the authenticated handler.
The 44 fixture rows remain in the isolated QA database. No production configuration changes.
The temporary root test-server adapter is removed; focused tests pass afterward.
The other QA watcher's test selection is preserved.
Private receipts and credentials stay in ignored local files, including scripts/local/slack-chart-live-acceptance.json.
The review Page includes a capture of only the controlled Slack thread at sequence 68.
Its prior walkthrough and captures remain intact.
The actual Page preview loads the new image at 1036 by 1888 pixels.

## Decision and limits

Approve the populated chart and stat PNG behavior within this scope.
Live mobile Slack display, other widget families, and remaining audience scenarios need separate checks.
The full branch still needs its remaining reviews, local signoff, and pull-request update.
