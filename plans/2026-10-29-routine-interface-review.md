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
