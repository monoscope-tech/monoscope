# Conversation interface review

The [interactive artifact](../docs/reviews/2026-10-30-unified-agent.html) shows the current experience and alternatives.
The [unified plan](2026-10-30-unified-agent.md) defines the product contract.
The [consolidated review](../docs/reviews/2026-11-01-agent-cross-module-review.md) records current verification and limits.
Earlier iteration reports remain in Git history.

## Design decisions

| Interaction | Current behavior | Reason |
| --- | --- | --- |
| Runs | Open an inspector inside the current conversation. | Preserve the user's context and draft. |
| Artifacts | Open retained output through the same conversation surface. | Avoid a disconnected artifact page. |
| Review | Show selected operation decisions beside the run and resume through the shared queue. | Keep decisions attached to their actual operation. |
| Charts | Reuse the existing chat widget renderer and project theme. | Preserve query behavior and consistent presentation. |
| Native evidence | Show explanations, video, captions and package controls inside the run inspector. | Keep verification evidence next to the work it explains. |
| Failures | Show a local error and retry control without clearing the draft. | Permit recovery from dropped or delayed requests. |
| Navigation | Keep conversation state through inspector open/close and artifact selection. | Prevent accidental loss of current work. |

## Findings fixed

- Match project theme tokens instead of introducing separate visual styling.
- Keep run and artifact access inside the conversation rather than navigating to another page.
- Provide explicit loading, empty, failure, cancellation and missing-explanation states.
- Preserve focus and keyboard closing for the inspector.
- Keep review-decision failure feedback beside its control.
- Reserve text-relative chart height so 200% text cannot collapse the canvas.
- Assign scoped widget IDs and reject unsafe model-defined column links.
- Decode stored widgets through the same boundary as fresh model output.
- Preserve retained evidence when a widget cannot be displayed.
- Keep media downloads behind the same current role and evidence-audience checks.

## Verification boundary

The browser checks use actual application handlers with explicitly identified fixture chart transport.
They cover both themes, narrow widths, keyboard use, request failures and actual 200% text.
Generated media acceptance separately uses the SDK, KVM and real local object storage.
The artifact walkthrough passes 104 steps at 320, 390, 768 and 1280 pixels.
Current canonical browser and UI results are recorded in the consolidated review.

Hosted iframe interaction, physical-device use and VoiceOver remain unverified.
A browser fixture does not establish production ingestion or model accuracy.
Live Monoscope Slack chart acceptance is recorded separately from synthetic browser data.
