# Slack chart replies

Date: 1 November 2026.
This record covers the widget response boundary and bot chart dispatch.
Whole-branch signoff and live Slack chart delivery remain open.

## Behavior

Dashboard sharing and the older query/visualization response could produce chart images.
The bot renderer ignored the newer widgets array and could publish only its explanation.
The failing queued-command regression records that missing output.
See `scripts/local/slack-widgets-array-red.log`.

The renderer now publishes each retained widget as a chart image, followed by its explanation when present.
Replies link to the same Monoscope conversation that retains the interactive widgets.
An explicit widget array takes precedence over the older primary query.
Both retention and bot publication use the existing ten-widget limit.
The older plain-query table path remains available.

Slack receives image blocks and conversation links.
Interactive chart controls remain in Monoscope.
Headers, alternative text, and image URLs respect Slack's documented block limits.
An oversized image URL leaves the conversation link available.

## Model boundary

Signing the full model-supplied Widget would also authorize its raw SQL fields.
The failing boundary regression demonstrated that SQL was accepted before this change.
See `scripts/local/slack-widgets-model-boundary-red.log`.

The opaque ModelWidget type parses visualization configuration once.
It rejects SQL, executable HTML, callbacks, server routing, and server context.
The same restriction applies to nested children and multi-query SQL fields.
Callers can unwrap a parsed value but cannot construct an unchecked ModelWidget.
The partial response decoder retains valid widgets when another widget is invalid.
Pure round-trip, rejection, and response-precedence examples live beside their definitions.

## Scoped reviews and checks

The hs-distill pass reuses responseWidgets and the existing image delivery path.
It extends the queued-command matrix instead of introducing a second workflow harness.
The second pass retains the parser wrapper because it enforces a distinct trust boundary.

The hs-evasion-review pass inspected Widget, its nested types, PNG signing, and widget query execution.
The wrapper hides its constructor and exposes no Generic instance that could bypass parsing.
The change adds no dependency, warning suppression, raw IO, or weaker effect constraint.
The second pass found no further material issue in these changed functions.

The hs-lob-review pass adds no browser behavior or JavaScript.
The renderer remains local to its existing dispatcher.
The pure examples remain in Haddock; the integration matrix exercises handlers, queues, retention, and replay.
A second pass found no client-tier or locality issue in this scope.

The focused watcher run passes: **6 examples, 0 failures**, in 1.8716 seconds.
It covers text, report selection, two charts with explanation, mixed response formats, and charts without explanation.
Chart cases also assert retained widgets, conversation links, unsafe-widget rejection, and replay without new posts or model calls.
These tests use a fake Slack transport; they do not prove that Slack fetched or displayed the PNG.
The log is `scripts/local/slack-widgets-array-six-green.log`.
The source hashes are `scripts/local/slack-widgets-source.json`.
The pinned formatter accepts the four changed Haskell files.
HLint reports no hints in the three runtime modules.
The full bot group passes: **489 examples, 0 failures**, in 173.7507 seconds.
The log is `scripts/local/slack-widgets-bots-489-green.log`.
The four source hashes remain unchanged.
The supported `make test-doctests` run passes: **2,083 tried, 0 errors, 0 failures**.
The previous checkpoint contained 2,076 examples; all seven new examples ran.
The log is `scripts/local/slack-widgets-doctests.log`.
The runtime source hashes remain unchanged through both runs.
A concurrent edit extended a separate reply-retry test during the doctest run.
That edit remains outside this commit; the 489-case result covers the recorded source hashes.

## Provider references

Slack documents the [header limit](https://docs.slack.dev/reference/block-kit/blocks/header-block/) and [image block limits](https://docs.slack.dev/reference/block-kit/blocks/image-block/).
