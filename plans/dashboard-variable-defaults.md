# Finding: a required dashboard variable blocks the dashboard and empties it

Found 2026-08-25 while auditing the dashboards feature. Not fixed — the fix is a change
to query semantics, not a bug fix, and is written up here rather than started mid-stream.

## What happens

Open a dashboard whose variable is `required` and unset (e.g. Endpoint Analytics, or the
demo project's `025e255c-…`). A full-screen command palette covers the page
(`var-picker-backdrop`, `z-index: 99999`, `Pages/Dashboards.hs` `variablePickerModal_`).

Dismissing it does not help. Every widget then reads "No data in the selected time range",
because an unset variable does not mean "no filter" — it interpolates to the empty string:

    Pkg/Parser.hs  variablePresets:  allParams <&> second maybeToMonoid

So `WHERE domain = '{{var-domain}}'` becomes `WHERE domain = ''`, which matches nothing.
The reader gets a modal they must answer, and an empty dashboard if they decline.

## How Datadog does it

Template variables default to `*` — no filter. The dashboard **always** renders, unfiltered,
and the reader narrows afterwards from dropdowns in the header (we already render those).
Datadog never blocks a dashboard behind a modal on load.

## Why this is not a small fix

To default to "all" we need `{{var-x}}` to expand to something that means *match everything*
in the position where it appears. Today the placeholder is substituted as a bare value inside
a quoted equality, so there is no expansion of it that means "no filter" — the clause itself
has to disappear. That needs one of:

- a `{{var-x-filter}}`-style placeholder that expands to a whole clause (`AND domain = '…'`)
  or to nothing, the way `{{time_filter}}` already does in `variablePresets` — and every
  dashboard template rewritten to use it; or
- interpolation that understands the surrounding SQL well enough to drop the predicate,
  which we should not attempt.

The first option is the right one and follows a pattern the codebase already has. It is a
template-wide migration, so it wants its own change.

## Scope note

`findVarToPrompt` distinguishes two sources of prompting:

- `variable.required` — a filter that has no value yet. This is the case that should default
  to "all" and stop blocking.
- `tab.requires` — a drill-down tab that is genuinely parameterized ("which endpoint?").
  There is no all-endpoints rendering of such a page, so blocking there is correct. Keep it.

## Test to write first

The e2e suite can reproduce this without touching production: create a dashboard from a
template that declares variables via the template picker, open it, and assert no
`.var-picker-backdrop` is present on load and that widgets render. Note that
`e2e/tests/dashboard-grid.spec.ts` `openFirstDashboard` currently contains a
backdrop-dismissal loop and a comment describing the blocking behaviour — both go away with
the fix and should be removed in the same commit.
