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
