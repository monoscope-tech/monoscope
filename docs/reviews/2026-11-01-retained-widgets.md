# Retained widgets in conversation runs

Date: 1 November 2026.
This review covers retained run visualizations, shared chat widget frames,
and the model widget decoding boundary.
It does not establish whole-branch signoff or live two-chart Slack delivery.

## Findings and fixes

| # | Severity | Domain | Location | Before | After | Why |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | HIGH | Layout | `src/Pages/AIThreads.hs:443` | RetainAnswer evidence had no interactive widgets in the inspector | Render the authorized retained visualizations | Slack's conversation link needs a usable review surface |
| 2 | HIGH | Layout | `src/Pages/Issues.hs:2095` | Aspect ratio alone left a zero-height canvas with 200% text | Reserve a text-relative minimum height | The chart must remain visible when text grows |
| 3 | MEDIUM | UI | `src/Pages/Issues.hs:2095` | Model IDs could repeat within a render batch | Assign indexed IDs under the existing answer prefix or run ID | Repeated IDs can send rendering and controls to the wrong widget |
| 4 | HIGH | UI | `src/Pkg/Components/Widget.hs:372` | Model table columns accepted executable link templates | Reject model-supplied column links | Untrusted output must not define executable links |
| 5 | HIGH | UI | `src/Pages/Issues.hs:2175` | Stored chat widgets bypassed ModelWidget | Decode stored entries through the same boundary | Invalid historical entries must not bypass the current rendering contract |

All five findings are fixed. Failing evidence is retained in
`scripts/local/retained-widgets-inspector-red.log`,
`retained-widgets-browser-resize-red.log`, `retained-widgets-model-id-red.log`,
`retained-widgets-model-link-red.log`, and `retained-widgets-stored-link-red.log`.


The retained result remains outside chat history until its existing publication rule permits it.
No conversation, run, artifact, or authority record is copied.
Current conversation access checks run before visualizations are rendered.
Widget endpoints keep their existing project and session checks.
The raw findings remain available as escaped JSON for inspection.
Rejecting a visualization does not erase its retained evidence.

## Repeated Haskell reviews

The hs-distill pass extracts the existing widget rendering and tool-data reuse into aiChatWidgets_.
Chat turns and run evidence call that one renderer.
The stored-widget decoder stays local to the two branches that use it.
The second pass finds no further material reduction in this scope.

The hs-evasion-review pass follows retained completion, evidence visibility,
AgenticChatResult decoding, ModelWidget construction, and widget query execution.
It found and fixed the stored-JSON bypass and executable column links.
The parser wrapper retains its hidden constructor and has no Generic bypass.
The change introduces no raw IO, authority shortcut, dependency, or warning suppression.
The second pass retains the existing role and audience gates.

The hs-lob-review pass uses the existing inspector and widget behavior.
Minimum heights are ordinary Tailwind utilities at the element.
The change adds no JavaScript, separate page, modal, or navigation state.
The second pass finds no material client-tier or locality issue.

## Interface scope and coverage

Mode: full within the retained-widget flow and its shared frame.
The application uses Lucid, HTMX, the existing widget engine, and project theme tokens.
The review applies emil-design-eng and all six better-interface owners.

| Domain | Evidence inspected | Result |
| --- | --- | --- |
| Accessibility | Named section and heading, keyboard opening and closing, focus, 200% text, role denial | Verified through handler and browser checks; physical screen readers remain unverified |
| Layout | 320px and 1280px, both themes, 100% and 200% text, canvas and stat dimensions, overflow | The zero-height finding is fixed |
| Writing | Visualizations heading, retained evidence, inspector controls and fixture labels | Existing product wording and explicit fixture labels retained |
| Typography | Shared heading and widget type, text scaling, statistic visibility | Text-relative frame minimums preserve plot space |
| Colors | Existing light and dark tokens, shared chart and stat component | Existing theme behavior retained |
| UI | Existing inspector, draft retention, shared widget controls, stable IDs | No extra navigation or motion introduced |

## Considered but rejected

| Candidate | Reason |
| --- | --- |
| Publish every retained Slack answer into chat history | Retention and publication carry different visibility and recovery contracts |
| Create a second run-specific chart renderer | Reusing the existing renderer preserves query-data reuse and widget behavior |
| Fix the plot with viewport-independent pixel height | A fixed pixel frame does not reserve more space when text is enlarged |
| Keep model IDs as DOM identifiers | Those IDs have no authority to claim a shared document namespace |

## Verification

The permanent handler lifecycle fails before the inspector change.
It now covers two valid widgets, repeated model IDs, an unsafe sibling,
legacy stored widgets, no implicit chat publication, and non-member denial.
The test uses a separate project rather than the public demo project.

The permanent browser case uses retained fixture rows and synthetic chart data.
It verifies actual canvas rendering, a visible statistic, both themes,
320px and 1280px, actual 100% and 200% text, no horizontal overflow,
keyboard closing, unchanged conversation path, and the retained draft.
The application read handlers are real; the chart response is a browser fixture.
It does not establish live telemetry, general model accuracy, or physical-device behavior.

An initial command accidentally selected the whole e2e suite.
It failed on an unrelated inactive Stripe test price and is not a passing check.
The corrected command explicitly selects e2e/tests/routines.spec.ts.
An intermediate corrected run passes all eleven cases in 27.6 seconds.
The saved log is `scripts/local/retained-widgets-browser-eleven-green.log`.
A subsequent rerun encountered a Hasql pool acquisition timeout.
That failure remains recorded separately and is not treated as green.
A fresh rerun then passes the handler lifecycle and all eleven browser cases in 27.0 seconds.
The outer Hspec run passes two cases in 28.1864 seconds.
The final browser log is `scripts/local/retained-widgets-browser-final-eleven-green.log`.
Desktop light and enlarged mobile dark captures received visual inspection after chart loading completed.
The temporary adapter is removed; only the permanent handler and browser tests remain.
The supported `make test-doctests` run passes: **2,089 tried, 0 errors, 0 failures**.
Its log is `scripts/local/retained-widgets-doctests.log`.
The first full bot run reports 504 examples and three failures in query fixtures after the concurrent ModelQuery change.
Those failures are recorded in `scripts/local/retained-widgets-bots-504-mixed.log`.
Valid KQL replaces the malformed or unknown-field fixture queries in `9bb232244`.
All three repaired cases pass with the retained-widget lifecycle: **4 examples, 0 failures**, in 1.5586 seconds.
The final full bot rerun passes: **504 examples, 0 failures**, in 185.2532 seconds.
The log is `scripts/local/retained-widgets-bots-504-green.log`.
All 280 recorded Haskell and manifest inputs remain unchanged through the final run.
The five owned inputs also remain unchanged.
The input records and final summary are under `scripts/local/retained-widgets-*`.
The owned source hashes are `scripts/local/retained-widgets-source.json`.

## Live observation boundary

A read-only observation of the current private QA shortcut finds one acknowledged image block.
Its signed PNG returns HTTP 200 at 900 by 300 pixels.
The real model chose a legacy log visualization for a schema explanation, and the plot is empty.
This observation does not pass useful-chart or modern widget-array acceptance.
It sends no additional Slack message or agent request.
The private receipt is `scripts/local/slack-live-widget-readonly-acceptance.json`.


## Review Page

The existing review Page now includes the interactive chart inspector capture.
At Page sequence 67, the original title is retained and the Page contains twelve blocks after insertion.
Chrome decodes the new 1280 by 900 image and displays the native section heading.
The caption identifies the synthetic values and keeps live Slack delivery separate.
Hosted visualization interaction remains unverified.


## Verdict

Approve
