# Required dashboard variables: what was wrong, and what it was not

Opened 2026-08-25 as "a required variable blocks the dashboard; copy Datadog and default
it to `*`". **Resolved 2026-08-26, but not that way** — the match-all recommendation below
was wrong, and this records why so nobody implements it later.

## What the original finding got right

Opening a dashboard whose required variable was unset covered the page with a full-screen
picker (`var-picker-backdrop`, `z-index: 99999`). Behind it, every widget had already run
with the variable interpolated to the empty string — `variablePresets` maps an unset param
through `maybeToMonoid` — so `WHERE domain = '{{var-domain}}'` became `WHERE domain = ''`
and each one reported "No data in the selected time range". Dismissing the picker left
that as the whole page: a grid of charts all claiming, falsely, that there was no data.

## What it got wrong

The proposed fix was Datadog's: default template variables to `*`, render unfiltered, let
the reader narrow from the header. That is right for Datadog's dashboards and wrong here,
because of who actually uses `variable.required`.

Grepping the templates: the only one is `endpoint-stats.yaml`, **Endpoint Analytics** —
`host` (Domain), then `endpointHash` (Endpoint) which depends on it. Every widget on it is
about one endpoint. There is no all-endpoints rendering of an Endpoint Analytics page; that
page already exists and is called Endpoints. So the *ask* was correct and only its *shape*
was wrong.

`_overview.yaml`'s `service`/`database`/`resource` variables are not `required` at all —
they gate tabs via `tab.requires`, which the original note already said to leave blocking.
So there was never a dashboard that wanted match-all.

## What shipped instead

The prompt is now the tab's content rather than a modal over it, and widget processing is
skipped entirely while a required variable is unanswered — previously every widget ran a
real query that could only come back empty. One path now serves both `variable.required`
and `tab.requires`, so the "keep tab.requires blocking" caveat needs no second mechanism.

An empty option list says so explicitly instead of rendering a search box over nothing —
worth having, because a project with no endpoints yet is the first-run case.

Not done, deliberately: pre-selecting when only one option exists. It reads as an obvious
nicety, but the data says otherwise — of the projects with endpoints, only one has a single
domain, so it would almost never fire.

Covered by `e2e/tests/dashboard-add-widget.spec.ts`, "a dashboard needing a variable asks
for it instead of covering itself", which pins both halves: no backdrop, and no widget
shells claiming emptiness.
