# Onboarding Redesign: Progressive Dual-Track with `pi-durable`

Date: 2026-11-24
Status: Revised (Ready for review)

## Context & Problem

Monoscope's current onboarding (`src/Pages/Onboarding.hs`) is a linear 5-step blocking wizard:

```
Signup -> [1. Info Form] -> [2. Survey] -> [3. Slack/Phone/Email + Invite Modal] -> [4. Manual SDK Docs] -> [5. Pricing] -> [Complete] -> Home
```

Key activation leaks:
- **Upfront form tax (Steps 1–2):** 5 required fields (name, company, size, attribution) plus a survey whose answers (`functionality`, `location`) are stored in `projects.questions` without adapting the flow.
- **Premature commitment (Step 3):** Asks for Slack OAuth, phone numbers, and team invites before any data or monitor exists.
- **High-friction wall (Step 4):** Manual SDK docs proxied from `monoscopehq.com` require switching to an IDE, installing packages, configuring env vars, and redeploying. Neither Monoscope's GitHub App + durable coding agent (`pi-durable` / `CodeReview`) nor zero-code Uptime checks (`Pages.Uptime`) are surfaced here.
- **Pricing wall before value (Step 5):** Users who skip Step 4 still hit a plan picker before seeing their project.

## Goals

1. Cut time-to-first-value (TTFV) to **~10 seconds** on the zero-code URL path and **<2 minutes** (hands-off, end-to-end verified) on the autonomous GitHub PR path.
2. Verify every auto-instrumented PR **end-to-end before marking integration done**: inject a generated project API key ephemerally in the `pi-durable` sandbox, emit a real test span/log to Monoscope, confirm ingestion, and open a secret-free PR.
3. Preserve **Data region** (`US` / `EU` / `Asia`) on Screen 1 so it is ready for upcoming regional routine routing.
4. Keep the UI idiomatic to Monoscope's stack: Lucid + HTMX 4 + Tailwind v4 + DaisyUI v5 semantic tokens (`light` & `dark`), reusing `Pages.Components` primitives.

## Non-Goals

- Replacing the full manual SDK docs library (it remains accessible under Screen 2C's `Manual SDK docs` tab).
- Rewriting `Pages.GitSync`, `Pages.Uptime`, or `Data.Effectful.CodeReview` internals; this flow composes them.

---

## Interface Standards (Applied Across All Screens)

- **Vocabulary & casing:** Sentence case on all buttons and headings (`Connect GitHub`, `Monitor a URL`, `Start check`, `Create uptime checks`, `Copy env vars`, `Go to project`). Natural-case badge copy (`Recommended`, `10 seconds · Zero code`) styled with `uppercase tracking-wider` in CSS. User-facing copy says **Setup agent** (never internal codename `pi-durable`).
- **One primary action per view:** Only the recommended/primary CTA uses `btn-primary`; sibling actions use neutral secondary styling (`btn` / bordered).
- **Forms & accessibility:** Every input has a visible `<label for>`, `text-base sm:text-sm` (prevents iOS Safari zoom), and inline `aria-invalid`/`aria-describedby` errors. Radio groups use `<fieldset><legend>`. Async status changes announce via a stable `role="status"` polite live region; failures use `role="alert"`.
- **Typography & overflow:** `tabular-nums` on latency, status codes, and elapsed timers; `min-w-0 break-all` or `truncate` (with full value reachable) on URLs, repo names, and API keys.
- **Surfaces, spacing & motion:** Group with negative space (`space-y-8` between sections vs `space-y-3` within) rather than `<hr>` rules. Concentric radii (`rounded-xl` outer with `p-4`/`p-5`, `rounded-lg` inner controls/diff). Icon crossfades (`copyField_`, spinner-to-check) use CSS `opacity`/`scale`/`blur` with `cubic-bezier(0.2, 0, 0, 1)` wrapped in `@media (prefers-reduced-motion: no-preference)`. Status indicators pair icons + text with Monoscope semantic tokens (`text-textSuccess`, `text-textError`, `text-textWeak`, `bg-fillWeak`, `border-strokeWeak`) verified in both `light` and `dark` themes.

---

## Proposed Flow (2 Screens -> Project Home)

### Screen 1: Quick Start (~15 seconds)

One shared form owns `Project name` and `Data region`, above two **symmetric, equal-height launch cards** (`flex-col justify-between` with aligned bottom CTAs). Choosing a card submits/saves the project settings and routes to Screen 2A or 2B.

```
+-----------------------------------------------------------------------+
|  monoscope                                            [Skip for now]  |
|                                                                       |
|  Create your project                                                  |
|                                                                       |
|  Project name                            Data region                  |
|  [ Acme API                          ]   (o) US   ( ) EU   ( ) Asia   |
|  (stacks vertically on narrow widths)                                 |
|                                                                       |
|  How would you like to get your first data flowing?                   |
|                                                                       |
|  +-------------------------------+  +-------------------------------+ |
|  | [GitHub] Auto-instrument repo |  | [Pulse] Monitor a URL         | |
|  | Recommended                   |  | 10 seconds · Zero code        | |
|  |                               |  |                               | |
|  | Connect a repo. Our setup     |  | Check any website or API      | |
|  | agent adds OpenTelemetry,     |  | endpoint for instant uptime,  | |
|  | verifies live ingestion, and  |  | response time, and downtime   | |
|  | opens a ready-to-merge PR.    |  | alerts.                       | |
|  |                               |  |                               | |
|  | [ Connect GitHub -> ]         |  | [ Monitor a URL -> ]          | |
|  | (btn-primary)                 |  | (secondary btn)               | |
|  +-------------------------------+  +-------------------------------+ |
|                                                                       |
|  Already using an AI coding agent locally?                            |
|  [Copy prompt for Claude Code / Cursor]   ·   [Manual SDK / CLI]      |
+-----------------------------------------------------------------------+
```

**Behavior:**
- **Form ownership:** A single `<form>` wraps `Project name` (pre-filled from work email domain/OAuth, editable, inline validation) and `<fieldset><legend>Data region</legend>` (`US` / `EU` / `Asia`). Clicking `Connect GitHub`, `Monitor a URL`, or either secondary link saves `title` + `location` first (via `hx-include` / `formaction`), so edits are never silently dropped.
- **Card 1 (`Connect GitHub` — `btn-primary`):** Starts GitHub App installation / repo selection (`Pages.GitSync`) with `?onboarding=true` -> Screen 2A.
- **Card 2 (`Monitor a URL` — secondary `btn`):** Transitions to Screen 2B (URL entry + instant probe).
- **Secondary links:** Route to Screen 2C (`AI agent prompt` tab or `Manual SDK docs` tab).
- **`Skip for now`:** Defaults project to Free/Trial and redirects to `Pages.Home`.

---

### Screen 2A: GitHub + Setup Agent (`pi-durable`) — Live Split-Screen

After picking a repo, `pi-durable` runs a durable sandboxed job: clones the repo, detects stack/framework, adds OpenTelemetry instrumentation, **injects a newly generated project API key ephemerally in the sandbox to run tests and verify a real test span reaches Monoscope**, and opens a secret-free PR.

- **Responsive layout:** Two columns on desktop (`lg:grid-cols-[1.2fr_1fr]`); stacks vertically on narrow widths with Setup agent progress first and optional setup cards below.
- **Stable chrome:** Primary actions (`View PR #42 on GitHub`, `Go to project`) sit in the sticky top/bottom bar with safe-area padding so expanding diffs never clip them.
- **Right pane:** Unnumbered, independent optional cards (never implies mandatory step order).

```
+-----------------------------------------------------------------------+
|  monoscope  >  Acme API  >  Auto-instrument                           |
|  Repo: [ acme/backend (main) v ]  [+ Add repo]         [Go to project]|
+-----------------------------------------------------------------------+
|  Setup agent                           |  Optional while it runs      |
|  (role="status" polite live region)    |                              |
|                                        |  +-------------------------+ |
|  [x] Cloned acme/backend               |  | Production env vars     | |
|  [x] Detected Node 22 + Fastify        |  | Needed when you deploy  | |
|  [x] Added @opentelemetry/sdk          |  | PR #42:                 | |
|  [x] Tests passed (`npm test`)         |  | OTEL_EXPORTER_OTLP_...  | |
|  [x] Verified live test span in        |  | MONOSCOPE_API_KEY=...   | |
|      Monoscope (200 OK · 41ms)         |  | [Copy env vars]         | |
|  [*] Opening Pull Request...           |  +-------------------------+ |
|                                        |                              |
|  +--- Diff preview (max-h-56 scroll) -+|  +-------------------------+ |
|  | + import './telemetry.ts'          ||  | Alert channels          | |
|  | + // reads MONOSCOPE_API_KEY       ||  | [Slack]   [Connect]     | |
|  +------------------------------------+|  | [Discord] [Connect]     | |
|                                        |  +-------------------------+ |
|  Safe to leave — this keeps running    |                              |
|  and emails you when the PR is ready.  |  +-------------------------+ |
|                                        |  | Suggested uptime checks | |
|                                        |  | [x] api.acme.com/health | |
|                                        |  | [ ] app.acme.com        | |
|                                        |  | [Create uptime checks]  | |
|                                        |  +-------------------------+ |
+-----------------------------------------------------------------------+
|  [ View PR #42 on GitHub -> ] (btn-primary, enabled once PR opens)    |
+-----------------------------------------------------------------------+
```

**End-to-End Verification Contract (Required Before Done):**
1. Generate/fetch an active project API key on the server.
2. Pass the key to the `pi-durable` sandbox **only as an ephemeral runtime env var** (never written to tracked files or git history).
3. After patching and running existing unit tests, the agent starts/invokes the instrumented app or smoke test in the sandbox to emit a real OTel span/log to Monoscope.
4. Monoscope confirms ingestion of that verification event (`otel_logs_and_spans` match for `project_id`) **before** marking the integration verified and opening the PR.
5. The PR body includes the exact env var names (`OTEL_EXPORTER_OTLP_ENDPOINT`, `MONOSCOPE_API_KEY`) without secret values; the user copies the values directly from the right-pane `Production env vars` card.

**Failure / Fallback State (`role="alert"`):**
If tests fail or the verification span never arrives, show a plain 1-line summary (e.g. *"Tests failed while verifying OpenTelemetry setup. Review the log below or finish locally with your coding agent."*) above a collapsible log viewer, with a 1-click button to open Screen 2C pre-seeded with what the agent discovered.

---

### Screen 2B: Monitor a URL (10-Second Instant Win -> Soft Bridge)

Handles both the initial URL entry and the live probe result in one calm, single-column view (no `<hr>` dividers; sections separated by `space-y-8`).

```
+-----------------------------------------------------------------------+
|  monoscope  >  Acme API  >  Monitor a URL              [Go to project]|
+-----------------------------------------------------------------------+
|                                                                       |
|  Monitor your first endpoint                                          |
|                                                                       |
|  URL to monitor                                                       |
|  [ https://api.acme.com/health                     ] [ Start check ]  |
|  (visible <label>, type="url", text-base sm:text-sm)   (btn-primary)  |
|                                                                       |
|  -- After probe runs (role="status" / role="alert" on error) --       |
|                                                                       |
|  +------------------------------------------------------------------+ |
|  | [check] https://api.acme.com/health is up                        | |
|  |         200 OK · 78 ms · US-East · Checking every 60s            | |
|  +------------------------------------------------------------------+ |
|  (Error state: "Couldn't reach this URL (DNS lookup failed). Check    |
|   that it's publicly accessible and try again." + inline retry)       |
|                                                                       |
|  Downtime alerts                                                      |
|  [Slack: Connect]   [Email: you@acme.com (active)]                    |
|                                                                       |
|  Want logs, traces, and AI root-cause debugging too?                  |
|  [Auto-instrument a GitHub repo]   [Copy Claude Code / Cursor prompt] |
|                                                                       |
+-----------------------------------------------------------------------+
```

---

### Screen 2C: Local AI Agent / Manual SDK / CLI Path

Uses Monoscope's semantic tab strip (`navTabStrip_` / `detailTab_` with full APG keyboard support).

```
+-----------------------------------------------------------------------+
|  monoscope  >  Acme API  >  Instrument                 [Go to project]|
+-----------------------------------------------------------------------+
|                                                                       |
|  | AI agent prompt |   Manual SDK docs   |   CLI   |                  |
|  -------------------------------------------------------------------  |
|                                                                       |
|  Paste this prompt into Claude Code, Cursor, or Codex:                |
|  +------------------------------------------------------------------+ |
|  | Instrument this repo with OpenTelemetry for Monoscope.           | |
|  | Endpoint: https://otel.monoscopehq.com                           | |
|  | API key:  ms_live_xxxx... (editor+ only)                         | |
|  | Run the app, emit one test span, and confirm it succeeds. [Copy] | |
|  +------------------------------------------------------------------+ |
|                                                                       |
|  Live ingestion check (role="status", 5s HTMX poller)                 |
|  (*) Waiting for first event...  ->  [check] First event received     |
|                                                                       |
|  [Send test event via CLI]                                            |
+-----------------------------------------------------------------------+
```

---

### Post-Onboarding: In-App Home Checklist

Users who skip onboarding or only complete one track see a dismissible compact card on `Pages.Home` (backed by existing `onboardingDismissChecklist`):

- `[x]` First data connected (Uptime check or verified OTel event)
- `[ ]` Auto-instrument a repository (or live link to open Setup agent PR + `Copy env vars`)
- `[ ]` Connect Slack / Discord alerts
- `[ ]` Invite teammates

Billing defaults to Free / Trial on project creation; upgrades surface contextually (inviting teammates on Free tier, approaching event/retention limits, or billing settings).

---

## Implementation Outline

1. **Data & Routes:**
   - Simplify `OnboardingStep` enum (keep backwards-compatible redirect for legacy `?step=` URLs).
   - Default new projects to Free/Trial on creation instead of `'ONBOARDING'` gating.
   - Persist `location` (`US` / `EU` / `Asia`) from Screen 1 for upcoming regional routine routing.
   - Add `?onboarding=true` return path to GitHub App callback (`Pages.GitSync`) -> Screen 2A.
2. **Backend / `pi-durable` Job:**
   - Add an `OnboardingInstrument` job that clones the repo in a sandbox, detects stack, patches OTel, injects an ephemeral generated project API key to emit & verify a live test event in `otel_logs_and_spans`, extracts candidate uptime URLs, and opens a secret-free PR via `GitPublication`.
   - Expose an HTMX polling/SSE endpoint for Screen 2A live phases + bounded diff preview.
3. **Frontend (`src/Pages/Onboarding.hs` + `src/Pages/Home.hs`):**
   - Build Screen 1 and Screens 2A / 2B / 2C with `Pages.Components` primitives (`panel_`, `formField_`, `copyField_`, `callout_`, `tonePill_`, `navTabStrip_`).
   - Add dismissible setup progress card on `Pages.Home`.

## Open Questions

1. Should `pi-durable` onboarding run on Free tier (e.g. 1 free onboarding PR per project) to maximize activation, or be gated behind Trial?
2. What is the desired UX when a user installs the GitHub App on a monorepo with multiple services (e.g. pick a subfolder/service before launching the agent)?
