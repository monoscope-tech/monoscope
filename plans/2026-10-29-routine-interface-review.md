# Routine interface review

## Scope and coverage

Full review of the routine catalog, definition conversation, run detail, artifact
retention, issue-enrichment activation, and repository review activation flow. Stack: Lucid, HTMX 4, native
HTML disclosures, Tailwind, DaisyUI, and existing semantic color tokens.
The review uses emil-design-eng and all six better-interface owner skills.
It covers the implemented Slack, PR review and issue-enrichment flows. Live
Slack workspace acceptance remains outside this review.

| Domain | Evidence inspected | Result |
| --- | --- | --- |
| Accessibility | Browser accessible names, keyboard disclosure, instruction labels, status announcements, editor/reader handler tests, project isolation | Applied findings below; browser checks pass |
| Layout | Catalog, definition, run and repository views at desktop, 390px and 320px; empty, running, failed and completed states | Applied narrow repository form fix; no content overflow in checked views |
| Writing | Activation, pause, stop, failure, deleted conversation and immutable instruction copy; repository readiness | Applied findings below |
| Typography | Existing Inter typography, heading order, readable report measure, wrapped requests, captions and metadata | No remaining actionable finding in inspected states |
| Colors | Light/dark renders and computed contrast for cancellation, instructions and failure text | Sampled contrast 6.03–12.78; existing semantic tokens retained |
| UI | Native details, HTMX forms, stable polling, completed artifacts, action feedback and follow-up links | Applied findings below; browser checks pass |

## Findings

These findings were implemented and checked. The table records the change scope,
not outstanding defects.

| # | Severity | Domain | Location | Before | After | Why |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | HIGH | Layout | `src/Pages/AIThreads.hs:140`, `src/Pages/BodyWrapper.hs:852` | Installation classification depended on a non-null interval | Use the routine trigger kind | Event routines appeared as chats and disappeared from installed routines |
| 2 | HIGH | Accessibility | `src/Pages/AIThreads.hs:146` | Mobile hid the only text naming the primary action | Add an explicit accessible name; name catalog add buttons with their template | An icon alone did not identify the action to assistive technology |
| 3 | MEDIUM | UI | `src/Pages/AIThreads.hs:612` | Polling removed a user's open instructions disclosure | Skip morphing the immutable instruction snapshot | A browser regression reproduced the disclosure closing every five seconds |
| 4 | MEDIUM | Layout | `src/Pages/CodeContext.hs:803` | Three flex items compressed field labels and selects into overlapping columns | Use a one-column grid, then two columns when space permits | The 320px browser regression reproduced overlapping review controls |
| 5 | MEDIUM | Writing | `src/Pages/CodeContext.hs:630`, `src/Pages/CodeContext.hs:795` | Repository readiness ignored routine activation | Show the missing activation and link to the review template | A connected repository could claim readiness without an authorized executor |
| 6 | MEDIUM | UI | `src/Pages/AIThreads.hs:182`, `src/Pages/AIThreads.hs:627` | A deleted title removed the run link; follow-up led to a deleted conversation | Keep the run link and retained artifacts; explain when the conversation is gone | Deleting a conversation must not make retained results unreachable |
| 7 | MEDIUM | Accessibility | `src/Pages/AIThreads.hs:418`, `src/Pages/AIThreads.hs:636` | Routine mutations accepted any project member | Require edit permission and hide editing controls for readers | Read access must not authorize automation or instruction changes |
| 8 | LOW | Colors | `src/Pages/AIThreads.hs:608` | User cancellation reasons used error coloring | Use neutral text for cancellation, warning for timeout and error for failure | The message's visual meaning must agree with the run status |
| 9 | LOW | Writing | `src/Pages/AIThreads.hs:185` | Recent runs showed the scheduling timestamp | Show execution time consistently with run details | Event execution history must describe when work ran |
| 10 | MEDIUM | Writing | `src/Pages/AIThreads.hs:155`, `src/Pages/AIThreads.hs:188` | The introduction omitted issue events and said results stayed only in Monoscope | Describe schedule and project events; state that Slack and PR reviews also post replies | Activation must explain where AI results go |
| 11 | MEDIUM | Writing and layout | `src/Pages/AIThreads.hs:607` | A member without profile names rendered an empty requester label | Use the email when the trimmed name is empty and wrap long identifiers | The permanent browser regression failed on the missing requester and passes with a 64-character local part at 320px |
| 12 | HIGH | Layout and UI | `src/Pages/AIThreads.hs:239`, `src/Pages/AIThreads.hs:360`, `src/Pages/Issues.hs:2252` | Run links opened a separate page; document selection replaced the chat; saved-artifact cards navigated away | Open runs and artifacts in a shared conversation detail panel; direct run links render the same conversation shell | The conversation, scroll position, and unsent draft must survive inspecting evidence |
| 13 | MEDIUM | Accessibility | `src/Pages/AIThreads.hs:402` | Detail navigation discarded the composer context | Use a named panel, focus its contents, support Close and Escape, hide the underlying chat on narrow screens, and return focus to the preserved composer | Desktop and narrow-screen users can inspect evidence and resume the same draft |
| 14 | MEDIUM | UI | `src/Pages/AIThreads.hs:289`, `src/Pages/AIThreads.hs:363` | A completed panel could leave the conversation's run badge showing Running | Refresh that run's badge alongside its panel using an HTMX out-of-band swap | Two simultaneously visible statuses must agree without replacing the conversation |
| 15 | MEDIUM | UI | `src/Pages/AIThreads.hs:414`, `src/Web/Routes.hs:423` | Orphaned runs offered a return to a deleted conversation; inspecting a historical run could discard its caller context | Preserve the typed caller conversation in inspector and polling requests; offer All routines when no conversation remains | Closing a historical run must preserve the current draft, and retained evidence needs a valid way back |

## Considered but rejected

| Location | Candidate | Rejected because |
| --- | --- | --- |
| Shared page shell | Change sidebar width or shared layout to address an intermediate screenshot | Browser geometry showed the title and main area correctly inside the viewport; waiting for the existing sidebar transition removed the capture artifact |
| Run and catalog pages | Add enter animations or animated status transitions | This is a frequently inspected operational surface; instant content and stable reading position are more useful |
| Semantic text tokens | Introduce local colors for errors and captions | Computed light/dark contrast passed; local colors would reduce theme consistency |
| Disclosure and form controls | Replace native details and HTMX with a client state component | Existing HTML and declarative attributes express the behavior and preserve its location |

## Verification

- Long-lived test watcher: all 133 bot examples, 73 API/MCP examples, three PR
  review examples and 62 repository examples passed. These include activation,
  instruction snapshots, retained evidence, retry and delivery-failure behavior. Reader writes return 403; another project's run returns 404.
- Local browser adapter: `scripts/local/routine-ui-review.cjs` ran against
  `withLiveServer` and an ephemeral integration database. It checked keyboard
  disclosure, HTTP instruction save/reload, immutable results, active polling,
  completion updates, polling termination, empty/failed states and mobile action
  names. It activated the PR-review template from repository settings and observed
  the readiness change. No browser errors occurred.
- Permanent regression: `e2e/node_modules/.bin/playwright test
  --config=e2e/playwright.config.ts routines.spec.ts --project=chromium
  --reporter=line` passed against the same isolated live-server mechanism. It
  covers activation, instruction editing, disclosure state across polls, completed
  artifacts, immutable history, deleted conversations and 320px repository forms.
  The latest run passed in 22 seconds. It also verifies requester identity for unnamed users with long email addresses. It also checks revoked membership and
  issue-enrichment activation at 320px. Both polling-state loss and form overlap
  failed before their fixes.
- `make post-css` passed. Captures are under ignored
  `scripts/local/routine-ui/`; the permanent test saves screenshots in Playwright's
  test-results directory.
- VoiceOver hardware testing and live Slack workspace acceptance were not run.
  This review makes no claim about those environments.

## Conversation-panel revision

User review found that the earlier isolated run page did not fit the conversation
experience. The revised layout keeps the chat and composer visible beside run
or artifact details on desktop. At 320px, the panel occupies the conversation
area; Close or Escape returns to the preserved draft. It uses existing surface,
stroke, type, and semantic color tokens, without a new animation or modal.

The permanent browser regression first failed when a run navigated away. It also
reproduced navigation from an inline artifact card and a stale run badge before
their fixes. The final focused run passed in 19 seconds. It verifies unchanged
conversation URLs, draft preservation, direct run links, panel position, artifact
cards, immutable run artifacts, polling, matching statuses, keyboard dismissal,
focus return, membership revocation, deletion retention, and light/dark 320px
layouts. Screenshots were inspected after rebuilding the local CSS.

Repeated Haskell distillation, evasion, and locality reviews found no further
material issue in the revision. Focused HLint reports no hints. The final
simplification keeps one conversation renderer and declarative fragment swaps.
All 133 bot examples pass after the revision, including canonical artifact links.
The server watcher compiles successfully and formatting passes. Fresh full
signoff and production image verification are required before push.

## Verdict

Approve the revised conversation layout; final signoff and live Slack acceptance remain separate gates.


The retained-run extension first failed on the dead conversation-return control,
then on a deleted source conversation opened from an existing routine. The final
browser run passes in 18.6 seconds, including caller-context preservation,
retained artifacts, the All routines link, and Escape navigation. Its temporary
live-server harness was removed. Inspector context is a typed conversation ID;
project scope and authorization remain fixed by the route.

All 34 Agentic examples, formatting, and focused HLint pass after this correction.
The final review retains the small typed URI parameter and declarative navigation;
it adds no client state framework or new authorization capability.

## Mobile controls and input hints (1 November)

Full review scope for this revision: routine creation, schedule editing, execution policy,
reviewer selection, routine instructions, and the conversation composer. The stack remains
Lucid, native form controls, HTMX, Tailwind, DaisyUI, and the existing theme tokens.
This scoped revision does not establish final whole-branch interface signoff.

| Domain | Evidence inspected | Result |
| --- | --- | --- |
| Accessibility | Accessible names for all seven controls; existing Escape, focus-return, and access-revocation browser flows | Checked paths pass. Physical iOS and VoiceOver remain unverified. |
| Layout | Creation and editing at 320px and 390px, both themes; desktop at 1280px | No horizontal overflow. The short composer hint fits its row. |
| Writing | New-routine instructions and the composer hint | The composer now says “Ask or delegate a task…” without clipping. |
| Typography | 28 mobile font measurements and seven desktop size assertions | Mobile controls use at least 16px. Desktop sizes remain 12px or 14px as before. |
| Colors | Eight rendered placeholder foreground/background pairs, composited through their ancestor backgrounds | All pass the review's 4.5:1 target. Existing theme tokens supply the colors. |
| UI | Native controls, existing surfaces, conversation drafts, run panels, retained artifacts, and action history | All four browser cases pass. No product JavaScript or motion was added. |

| Severity | Domain | Location | Before | After | Why |
| --- | --- | --- | --- | --- | --- |
| MEDIUM | Typography | `src/Pages/AIThreads.hs`, `src/Pages/BodyWrapper.hs` | Seven active controls rendered at 12–14px on narrow screens | Use 16px on narrow screens and retain the original desktop sizes | The rendered regression exposed small mobile input text. |
| MEDIUM | Writing | Conversation composer | The long hint wrapped beyond its single visible row | Use “Ask or delegate a task…” | The complete hint now fits at both checked narrow widths. |
| MEDIUM | Colors | New-routine and conversation input hints | Rendered contrast ranged from 3.05:1 to 4.21:1 | Use `textWeak` for active hints | Disabled styling made active guidance too faint. No palette value changed. |

| Location | Considered change | Rejected because |
| --- | --- | --- |
| Desktop controls | Increase every control to 16px | The existing compact density remains useful. The regression pins the original 12px and 14px sizes. |
| Composer | Add JavaScript sizing to accommodate the long hint | A concise hint fits the existing native control and needs no extra behavior. |

The permanent browser regression first failed on the measured mobile sizes, the clipped
composer hint, and all eight placeholder contrast pairs. The final run passes all four
cases in `e2e/tests/routines.spec.ts` in 18.7 seconds. It uses the actual application
through `withLiveServer` and an isolated integration database. Existing cases also verify
draft preservation, inline run/artifact navigation, policy saves, and role restrictions.

The broader run exposed a stale fixture for migration `0276`: execution policies now use
JSON. The fixture now supplies a JSON string and decodes stored policy values before its
assertion. No migration or application policy behavior changed for this correction.

`make post-css` passes. The repository-pinned Fourmolu accepts both changed UI modules,
and HLint reports no hints in them. The watchers compile the current source.
Before/after captures and logs are under ignored `scripts/local/routine-mobile-control-review/`.
The temporary browser harness is removed after verification; the permanent regression remains.

Repeated `hs-distill`, `hs-evasion-review`, `hs-lob-review`, and interface passes leave no
further material finding in this revision. Styling stays inline in the templates and uses
CSS responsive utilities. Final release checks and controlled Slack acceptance remain open.

Verdict for this scoped revision: Approve.

## Artifact navigation and terminal states (1 November)

Full review of artifact navigation and terminal run inspectors in the existing conversation.
The stack remains Lucid, MMark, native HTML, HTMX, and the existing Monoscope theme.

| Domain | Evidence inspected | Result |
| --- | --- | --- |
| Accessibility | Keyboard contents links, local links, heading focus, Escape, and restored composer focus | Two findings fixed |
| Layout | Desktop and 320px artifact and terminal-state captures | Clear within this scope; no horizontal overflow |
| Writing | Failed, timed-out, and cancelled run messages; missing-artifact explanation | Clear; the existing interface displays the recorded reason |
| Typography | Artifact headings, visible focus markers, and wrapped terminal messages | Clear within the inspected captures |
| Colors | Light/dark captures and existing semantic tokens | Clear for the changed markup; no palette change |
| UI | Native fragment navigation, inspector closure, and draft retention | Clear after both fixes |

| Severity | Location | Before | After | Why |
| --- | --- | --- | --- | --- |
| MEDIUM | `src/Pages/AIThreads.hs:434`, `src/Pages/AIThreads.hs:585` | Artifact and conversation headings shared fragment IDs | Scope artifact headings and local links by conversation or run and artifact key | Contents links must scroll to the artifact's heading |
| MEDIUM | `src/Utils.hs:1662` | A fragment jump left keyboard focus outside the inspector | Give scoped headings `tabindex="-1"` | Native navigation retains focus inside the inspector, so Escape closes it and restores the draft |

The first browser run reproduced the incorrect scroll target before the fix.
The next run exposed the focus problem after anchor isolation.
The final run passes all six permanent cases in 23.6 seconds against an isolated live server and integration database.
It checks contents and local links at 1280px and 320px in both themes.
External links and links to other pages retain their destinations.
Twelve terminal-state combinations cover three outcomes across both widths and themes.
The other cases retain lifecycle, mobile-control, action-history, and role-revocation coverage.

`renderMarkdownTocWithPrefix` reuses MMark's extension API and its existing heading identifiers.
The legacy renderer uses an empty prefix and retains its original rendering path.
The direct `modern-uri` dependency exposes MMark's existing URI type; no new runtime package enters the dependency tree.
No product JavaScript, scroll state, or global keyboard listener was added.

| Candidate | Decision |
| --- | --- |
| Add a client scroll handler | Rejected: scoped IDs and native fragment navigation solve the problem |
| Suppress the heading focus marker | Rejected: keyboard readers need a visible reading position |
| Change the catalog's 15px text after the detector advisory | Rejected: the unchanged catalog text is outside this navigation fix |

The detector reports no blocking finding and one advisory about the existing catalog type size.
Pinned Fourmolu and HLint pass for `Utils.hs` and `AIThreads.hs`.
Repeated distill, evasion, and locality reviews leave no further material finding in this patch.
VoiceOver, physical-device testing, and throttled loading states remain unverified.

Saved evidence is under ignored `scripts/local/artifact-contents-review/`, with sixteen captures.
Logs are `scripts/local/artifact-contents-red.log`, `scripts/local/artifact-contents-focus-red.log`, and `scripts/local/artifact-contents-green.log`.
The temporary live-server adapter is removed; both new browser regressions remain in `e2e/tests/routines.spec.ts`.
This review does not establish whole-branch signoff or live Slack acceptance.

Verdict for this scoped revision: Approve.


## Inspector request recovery (1 November)

The permanent browser regression first reproduced missing request-failure feedback and
loading indicators that remained active after a dropped connection. The fix retains
the existing inspector, error toast, and progress bar.

| Severity | Location | Before | After |
| --- | --- | --- | --- |
| MEDIUM | `src/Pages/AIThreads.hs`, `inspectorAttrs` | Failed detail requests provided no recovery message | A colocated hyperscript listener sends the existing error toast |
| MEDIUM | `web-components/src/page-chrome.ts`, request lifecycle | Failed fetches skipped cleanup; detached sources missed body listeners | Document-level finalization clears busy state and loading feedback |
| MEDIUM | `web-components/src/page-chrome.ts`, progress tracking | Cancelling a confirmation could decrement another pending request | A set tracks the actual request contexts and removes only issued requests |

| Domain | Evidence | Result |
| --- | --- | --- |
| Accessibility | Alert role, dismiss button, source busy state, keyboard retry, Escape and restored composer focus | Covered by the browser regression; VoiceOver remains unverified |
| Layout | 1280px light and 320px dark request failures and pending retries | Conversation and existing artifact remain available during failures |
| Writing | “Could not load the details. Try again.” | Gives the next action without exposing implementation details |
| Typography | Existing toast and conversation text | No typography change |
| Colors | Existing semantic toast/progress tokens in both themes | No new palette or hard-coded colors |
| UI | Server 503, dropped connection, delayed retry, cancelled confirmation, draft retention | Four failure/retry scenarios supplement the six existing browser cases |

The first complete run passed all seven browser cases in 30.4 seconds against an
isolated live application and database. The saved log is
`scripts/local/inspector-requests-green.log`; the failing-first log is
`scripts/local/inspector-requests-red.log`. This is fixture-backed application acceptance,
not live Slack acceptance. A follow-up capture checks full toast opacity before taking
the screenshot, so its appearance is not judged midway through the 180ms entrance.
That final run passes all seven cases in 23.1 seconds; its log is
`scripts/local/inspector-requests-final-green.log`. Final desktop and mobile captures
are saved under `scripts/local/inspector-request-review/`. The temporary live-server
adapter is removed; the permanent browser regression remains.

The Haskell change adds no domain state, type weakening, IO, suppression, or new effect.
Distill and evasion review found no material issue in this scoped patch. Locality review
keeps failure feedback on the initiating links. The existing shared JavaScript lifecycle
manager retains request identity across concurrent requests, cancelled confirmations,
failed fetches, and detached source elements. Native CSS indicators alone do not preserve
that shared lifecycle contract. No second loading subsystem or global error handler was added.

| Candidate | Decision |
| --- | --- |
| Add a new inspector error modal or retry state | Rejected: the existing toast and original link provide recovery |
| Replace request tracking with a simple counter | Rejected: cancelled confirmations finalize without issuing a request |
| Change the catalog's existing 15px text after the detector advisory | Outside this fix; no suppression added |
| Change `SFMono-Regular` in the existing multiline code tooltip | Retain the technical monospace fallback; unrelated to request lifecycle; no suppression added |

The detector reports only the unchanged catalog type-size advisory. Pinned Fourmolu
and HLint passed the changed Haskell markup. The Vite watcher rebuilt the frontend.
Physical-device and assistive-technology acceptance, final whole-branch reviews, and
release signoff remain open.


## Selected review in the conversation (1 November)

A permanent browser case exercises the actual review handler from the run inspector.
It covers administrator approval at 1280px in light mode and decline at 320px in dark mode.
Each case first downgrades the reviewer to editor, submits the retained form, and expects
HTTP 403 with the original pending decision intact. After role restoration, the visible
button saves the chosen decision and original reviewer, then disappears. Escape returns
to the retained composer draft and conversation URL.

| Domain | Checked behavior |
| --- | --- |
| Accessibility | Named action buttons, keyboard Escape, composer focus restoration |
| Layout | Existing inspector at 1280px and 320px; wrapped requester identity |
| Writing | Awaiting review, Approved, Declined, and Decision saved states |
| Typography | Existing heading, badge, metadata, and action text; no new styling |
| Colors | Existing light and dark themes; no new tokens |
| UI | Real handler decisions, stale reviewer rejection, retained draft and URL |

This closes a browser coverage gap; it adds no product code. The first fixture used
PostgreSQL timestamp formatting, which correctly failed the immutable-review guard when
the application normalized its JSON. The corrected fixture uses the application's canonical
timestamp representation and retains the guard unchanged. Its administrator role ceiling
matches the proposed write. The case does not run the resumed model or execute the action;
those continuation semantics remain covered by the separate integration suite.

The scoped distill, evasion, and locality passes found no material issue: this is a real
HTTP/browser flow rather than a mirrored implementation assertion, and it uses existing
fixtures and application handlers. No lower-tier product behavior or new abstraction is added.
Whole-branch reviews, live Slack behavior, and physical assistive-device acceptance remain open.

All eight permanent browser cases pass in 22.9 seconds on the final fixture.
The successful log is `scripts/local/selected-review-browser-green.log`; the two
inspected captures and source hashes are under `scripts/local/selected-review-browser/`.
The temporary live-server adapter is removed. No product change was needed.


## Review decision failure feedback (1 November)

The selected-review browser case now clicks the visible decision button after the
reviewer's role changes. It reproduced a silent HTTP 403: permission enforcement worked,
but the form gave no recovery feedback. A listener on that form now sends the existing
error toast for response errors and failed requests.

| Before | After | Evidence |
| --- | --- | --- |
| A stale review form silently rejected a decision | A visible alert explains that the decision could not be confirmed and directs the user to refresh action history | Real HTTP 403 after administrator-to-editor downgrade |
| Dropped-connection feedback was untested | The same alert appears and decision buttons become usable again | Aborted request, followed by a successful real retry |

The copy deliberately says “Could not confirm the decision” rather than claiming the
server did not save it. A lost acknowledgement can leave a successful decision uncertain.
The existing immutable decision receipt remains authoritative. No permission rule,
review deadline, operation outcome, or database guard changed.

Accessibility coverage includes the alert role, named dismiss/action buttons, usable
buttons after failure, Escape, and restored composer focus. Layout, typography, and
color reuse the existing toast and inspector at desktop/light and mobile/dark sizes.
The error copy gives one concrete recovery action. Drafts, pending decisions, chosen
reviewer, and conversation URLs are checked throughout the browser flow.

Distill and evasion review find no new type, effect, IO, suppression, or duplicate state.
Locality review keeps the listener on the submitting form and uses the existing toast;
no JavaScript manager, modal, or separate error page is added. The follow-up review finds
no further material issue in this patch. Pinned Fourmolu and HLint pass `AIThreads.hs`.
This remains scoped browser acceptance, not whole-branch signoff or live Slack acceptance.

All eight permanent cases pass in 22.6 seconds. The failing and final logs are
`scripts/local/review-decision-feedback-red.log` and
`scripts/local/review-decision-feedback-green.log`. The inspected mobile error capture,
desktop capture, decision captures, and source hashes are in
`scripts/local/review-decision-feedback/`. The temporary browser adapter is removed.


## Hosted review artifact refresh (1 November)

The existing review Page now shows the 50-case native checkpoint, eight conversation
browser cases, and 39 Slack-evidence cases. Its remaining-work list distinguishes
verified worker/bridge loss from whole-machine and Linux/KVM acceptance. The artifact
retains the same twelve walkthroughs, alternatives, and source excerpts. One CSS rule
removes paragraph margins from the verification counts so each row aligns.

All 101 walkthrough steps pass at 320, 390, 768, and 1280 pixels. Keyboard step selection
retains focus. Expanded sources introduce no horizontal overflow or browser errors.
The walkthrough script and all source panels remain byte-identical to the prior artifact.
The saved result is `scripts/local/unified-artifact-refresh-checks.json`.

The existing Page embed was replaced and read back at sequence 55. Its title and both
surrounding prose blocks remain unchanged. The uploaded HTML matches the local file:
185,066 bytes, SHA-256 `d25874281e71bfe3081308ed82e3230876271bedbfe454dd62aa01bb9f776c34`.
Local Chromium checked the same HTML. The hosted viewer and screen-reader interaction
remain unverified. This update does not change product code or establish release signoff.


## Text resize and final conversation browser pass (1 November)

### Scope and coverage

Full mode covers the routine catalog, conversation, run inspector, action review,
artifacts, and request recovery in Chromium. The application uses Lucid, HTMX,
hyperscript, Tailwind, and the existing theme tokens. The review uses isolated
HTTP handlers and a temporary database. Physical devices, VoiceOver, RTL,
the hosted review viewer, and the specialized native-fix view remain outside
this verification checkpoint.

| Domain | Evidence inspected | Result |
| --- | --- | --- |
| Accessibility | Automatic inspector focus, Tab to Close, Escape, restored composer focus, accessible control names, retained drafts | Checked paths pass; VoiceOver remains unverified |
| Layout | Run and artifact details at 320px and 1280px, both themes, with text enlarged to 200%; existing loading, empty and failure states | Close stays within the viewport; inspector content has no horizontal overflow |
| Writing | Recovery alerts, terminal-state explanations, empty-state guidance, action outcomes and selected-review controls | Clear within the reviewed flows |
| Typography | Sixteen close-control font measurements across eight run/artifact scenarios; 28 existing mobile input measurements and seven desktop assertions | Text doubles as requested; existing input-size checks pass |
| Colors | Existing light/dark tokens and captures; eight composited placeholder/background pairs | The measured pairs pass the 4.5:1 review target; no palette adjustment is needed |
| UI | In-conversation navigation, polling completion, draft retention, stale-role rejection and failed-request recovery | All nine permanent browser cases pass |

### Findings

No actionable interface findings in this scoped browser pass.
The new regression closes a verification gap rather than changing the product layout.

| Before | After | Why |
| --- | --- | --- |
| Text enlargement had no explicit browser regression | Run and artifact controls are exercised at twice their rendered font size | A root CSS setting alone would not prove that the text actually enlarged |
| Inspector resize coverage relied on ordinary viewport checks | Keyboard focus, close-control bounds, overflow and draft restoration are checked together | Reading details must preserve the path back to the conversation |

### Considered but rejected

| Location | Candidate | Rejected because |
| --- | --- | --- |
| `src/Pages/AIThreads.hs:565` | Add an entrance animation to the inspector | This is frequent keyboard navigation; the immediate transition preserves context and focus |
| `src/Pages/AIThreads.hs:573` | Add elevation shadows to the panel header | The existing raised surface and structural border match the application shell |
| `src/Pages/AIThreads.hs:625` | Enlarge artifact metadata above every Markdown heading | The metadata identifies the artifact; the document retains its authored heading hierarchy and format |

### Verification

The existing `live-test-dev` watcher ran a temporary `routineBrowserReview`
adapter using `withLiveServer`. It invoked
`playwright test --config=e2e/playwright.config.ts routines.spec.ts --project=chromium`.
All nine browser cases pass in 24.0 seconds. The same watcher run also passes
fourteen Slack audience examples, for fifteen Hspec examples with zero failures.
The temporary adapter and its imports are removed.

The new case measures the Close link before and after text enlargement in each
run/artifact, width, and theme combination. It checks keyboard entry and exit,
visible control bounds, horizontal fit, and the retained conversation draft.
The fixture uses the typed `WebConversation` origin JSON object.

The successful log is `scripts/local/interface-text-review-green.log`.
Captures are under `scripts/local/interface-text-review/`; tested source hashes
are in `scripts/local/interface-text-review-source.json`.
The pinned formatter accepts the temporary Haskell adapter cleanup, and
`git diff --check` passes. Scoped distillation, evasion, and locality review find
no material follow-up: this is one parameterized HTTP/browser regression using
the existing fixtures and database helper.

Physical-device and screen-reader interaction are **Not verified**.
The administrator-only native-fix screen and hosted Page viewer still require
separate acceptance. This verdict covers the browser paths described above.

### Verdict

Approve

## Streaming review artifact and hosted readback (1 November)

The existing review Page now contains twelve walkthroughs with 102 steps and
114 source panels. All 113 previous source panels retain their contents. The
additional panel shows the durable Slack streaming implementation at commit
`bf9fb3a44`. Each excerpt matches its cited Git revision after removing the
common indentation used by the excerpt renderer.

The checkpoint panel records nine conversation browser cases, 461 bot examples,
and 2,068 doctests. These are scoped checkpoints, not final release signoff.
The remaining-work panel retains the open live Slack and host-loss acceptance.
Two actual mobile application captures show the artifact inspector in light and
dark themes. A native desktop image precedes the interactive walkthrough.

All 102 steps pass at 320, 390, 768, and 1280 pixels, with keyboard selection,
matching expanded source text, no horizontal overflow, and no browser errors.
Both embedded mobile images decode at their original 320-by-844 resolution.
Results are in `scripts/local/unified-artifact-refresh-checks.json`.

The Page title is unchanged. The introduction and dated checkpoint now describe
the integrated experience. A fresh Page read confirms all six blocks at sequence
57. Reading the uploaded HTML reference returns 232,534 bytes and SHA-256
`f4070d6ed1db2b6d433e14eabbb3806ae0b2e9c8e1c3676beeadc1014959c8ef`,
matching the local file. Chrome opens the actual hosted Page with the expected
text, visualization embed, and decoded 1280-pixel desktop image. Interaction
inside the hosted visualization remains unverified; the complete interaction
checks above ran against the identical local HTML in Chromium.

This refresh changes the review artifact and evidence only. It does not establish
product release signoff, screen-reader acceptance, or physical-device acceptance.

## Completed native-fix page review (1 November)

### Scope and coverage

Full mode covers the authenticated completed-fix page, its retained media,
captions link, package control, and recorded usage at 320px and 1280px in both
themes with normal and doubled text. The native lifecycle also checks the page
handler's unavailable response after project eligibility changes. The page uses
the existing Lucid shell, Tailwind/DaisyUI controls, and theme tokens.

| Domain | Inspected evidence |
| --- | --- |
| Accessibility | Named video player, caption track and link, diagram alt text, focused package control, and measured text enlargement |
| Layout | Eight viewport/theme/text combinations; video, diagram, title, and download control remain within the viewport |
| Writing | Verified candidate and inferred cause remain distinct; the package is explicitly prepared rather than published; unavailable costs are labelled |
| Typography | Candidate revision, body text, headings, and wrapped control label remain legible at 200% text |
| Colors | Both application themes use the existing text, border, and link tokens; inspected dark focus ring remains visible |
| UI | Native player controls, consistent package control, restrained surfaces, and the existing application navigation |

### Findings

No actionable interface findings.

| Before | After | Why |
| --- | --- | --- |
| The completed native-fix screen had no rendered review checkpoint | Eight browser states verify the existing layout, media metadata, text size, and package-control focus | Retained evidence must remain readable without adding a parallel interface |

### Considered but rejected

| Location | Candidate | Rejected because |
| --- | --- | --- |
| `src/Pages/CodeContext.hs:107` | Autoplay the explanation video | Controlled playback avoids unsolicited motion and preserves the user's reading position |
| `src/Pages/CodeContext.hs:98` | Add a raised card and shadow around the whole report | The report already uses the application shell and structural media borders; another surface adds visual weight without improving grouping |
| `src/Pages/CodeContext.hs:117` | Force the package label onto one line at enlarged text | The existing two-line label fits on mobile and retains the complete action name |

### Verification

A temporary adapter in the existing native publication lifecycle used
`withLiveServer tr` and invoked
`node scripts/local/check-native-fix-interface.cjs` with the isolated server,
fixture IDs, session, generated media directory, and retained summary. The actual
authenticated HTML handler returns HTTP 200. Playwright supplies the same native
fixture's generated media through its route handler; this is fixture transport,
not S3 or media-authorization acceptance.

All eight states pass. Video metadata loads, the caption track is present, the
diagram decodes, the download has the expected package URL and filename, and
its focused state and complete label remain visible. Actual computed font
sizes double. There is no page overflow, clipped package text, or browser error.
The same watcher run passes 32 Slack citation cases, for 33 examples with zero
failures in 34.6194 seconds. The temporary adapter is removed.

The checks, sixteen captures, log, and source hashes are under
`scripts/local/native-fix-interface/`. The compiled CSS matches the prior
conversation browser checkpoint and predates these captures. The enlarged mobile
and normal desktop captures were visually inspected in both themes.

Physical devices, screen readers, keyboard traversal, playback controls, storage failures,
S3 delivery, and package download remain **Not verified by this browser pass**.
The existing native/backend acceptance evidence remains separate. No product
layout or behavior changed during this visual review.

### Verdict

Approve

## Native fix evidence inside the conversation — 1 November 2026

### Scope and coverage

Full review of the native fix run inspector, written artifact navigation,
and the missing-explanation state. The implementation reuses Lucid, HTMX,
the existing inspector, and application theme tokens.

| Domain | Evidence inspected | Result |
| --- | --- | --- |
| Accessibility | Keyboard opening, inspector focus, Escape, restored composer focus, named video, captions, diagram alternative text, and heading hierarchy | Clear |
| Layout | Eight combinations of 320/1280 pixels, light/dark appearance, and 100/200% root text size; package bounds and inspector overflow | Clear |
| Writing | Verified candidate, inferred cause, unpublished package, and missing-explanation message | Clear |
| Typography | Existing type tokens, complete wrapped package label, and the permanent measured text-enlargement regression | Clear |
| Colors | Existing application surfaces, borders, text, links, and both themes | Clear |
| UI | Run and written artifact opening, draft preservation, close behavior, and shared native HTML media controls | Clear |

### Findings

| # | Severity | Domain | Location | Before | After | Why |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | MEDIUM | Layout | `src/Pages/AIThreads.hs:321` | The verified-fix action leaves the conversation for another page | The existing run inspector includes the protected video, diagram, and package link | Evidence review must preserve the conversation and draft |
| 2 | MEDIUM | UI | `src/Pages/CodeContext.hs:107` | Missing retained media prevents the inline run details from loading | A status message explains the missing explanation while the run details remain available | Historical results must remain reviewable without an explanation |

Both findings are resolved. The two browser failures are saved as
`scripts/local/native-fix-inline-red.log` and
`scripts/local/native-fix-inline-missing-media-red.log`.

### Considered but rejected

| Location | Candidate | Rejected because |
| --- | --- | --- |
| `src/Pages/AIThreads.hs` | Copy the native media markup into the inspector | One pure view now serves both entry points, preserving the same URLs, captions, and content |
| `src/Pages/AIThreads.hs` | Add another JavaScript modal or navigation state | The existing HTMX inspector already preserves focus, drafts, and conversation identity |
| `src/Pages/CodeContext.hs` | Autoplay the explanation when the inspector opens | Explicit playback preserves the user's reading position and control |

### Verification

The permanent Playwright case uses the actual authenticated handlers and database
authorization view. Retained result rows and media responses are browser fixtures.
This case does not establish native verification, playback, S3 delivery, or package download.

All ten cases in `e2e/tests/routines.spec.ts` pass in 25.6 seconds through
the temporary `withLiveServer` adapter and Chromium. The new case covers all
eight visual combinations, written artifact navigation, retained drafts,
keyboard closing, and missing explanations. Seventeen captures, the complete
browser log, and source hashes are under `scripts/local/native-fix-inline*`.
Desktop light and enlarged mobile dark captures received visual inspection.

The temporary adapter is removed, and its original Haskell source is restored.
The real native publication and project-eligibility lifecycle then passes
in 19.7781 seconds. This confirms the existing standalone handler and access checks
after the shared-view extraction. The pinned formatter accepts both runtime files.
HLint reports no hints in those files.

The application-job acceptance case also passes in 3.0557 seconds.
Its administrator assertions now require the inline evidence and named video.
The view-only member still reads the retained result without private media URLs.
The saved log is `scripts/local/native-fix-inline/native-application-authority-green.log`.

The scoped `hs-distill` loop reuses the existing evidence view instead of copying it.
The evasion pass retains current membership checks and the demo-session denial.
The locality pass adds no client behavior or separate navigation state.
The repeated scoped pass finds no further material change.

Physical devices, screen readers, media transport failures, playback, S3 delivery,
and package download remain **Not verified by this browser pass**.
The full-branch reviews and release checks remain separate requirements.

### Verdict

Approve
