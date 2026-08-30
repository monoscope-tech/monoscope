# Color signal hierarchy — phase 2

## Status

**Planned.** This document turns the current Log Explorer and Containers critique into implementation tasks.

Target pages:

- `/p/00000000-0000-0000-0000-000000000000/log_explorer`
- `/p/00000000-0000-0000-0000-000000000000/infrastructure/containers`

Primary source targets:

- `src/Pages/LogExplorer/Log.hs`
- `src/Pages/Containers.hs`
- `static/public/assets/css/tailwind.css`
- `web-components/src/colorMapping.ts`
- `web-components/src/log-list.ts`
- `web-components/src/widgets.ts`

This plan starts after `plans/light-mode-color-contrast.md`. That first phase fixed structural contrast, selected states, semantic status signals, mobile reflow, and light/dark parity. This phase must not undo those guarantees.

## Goal

Make the interface feel bolder by improving signal hierarchy, not by adding color everywhere.

The color budget must follow these rules:

1. Red and amber identify errors and threshold breaches.
2. Stable categorical colors identify services.
3. Brand blue identifies action, focus, selection, and active investigation state.
4. Normal telemetry and utility chrome remain calm.
5. Every semantic color has a text, icon, label, position, pattern, or shape as a second signal.

## Current design health

| # | Heuristic | Score | Current issue |
|---|---|---:|---|
| 1 | Visibility of system status | 3 | LIVE state, loading indicators, active tabs, and counts are clear. Chart loading is visually weak. |
| 2 | Match system / real world | 3 | The language fits observability work. Some compact icons assume expert knowledge. |
| 3 | User control and freedom | 3 | Filters, pause, time navigation, and clear-all are visible. Log Explorer has too many adjacent controls. |
| 4 | Consistency and standards | 4 | Tokens, tables, navigation, and selected states are cohesive. |
| 5 | Error prevention | 3 | Query and filter constraints help. Dense controls still increase misclick risk. |
| 6 | Recognition rather than recall | 3 | Quick queries, labels, legends, and counts help. The icon rail depends on tooltips. |
| 7 | Flexibility and efficiency | 4 | KQL, facets, saved queries, view modes, export, and customization serve experts well. |
| 8 | Aesthetic and minimalist design | 3 | The hierarchy is better, but broad light surfaces remain restrained while latency cells use too much saturation. |
| 9 | Error recovery | 2 | Errors are visible. Recovery guidance is not visible in the reviewed default states. |
| 10 | Help and documentation | 3 | Docs, help, query examples, and tooltips are present. Much help remains external. |
| **Total** |  | **31/40** | **Good foundation; signal hierarchy needs another pass.** |

## Design-specificity finding

The core workspace now feels specific to an observability product. The KQL editor, facet rail, telemetry charts, event table, service identity, readiness labels, and incident-time controls form a coherent operational surface.

The remaining generic quality is concentrated in:

- The outer application shell
- Large neutral panel fields
- Generic chart legends
- Utility controls with equal visual weight

The next phase must preserve the dense operational character. It must not turn the product into a decorative dashboard.

## What already works

- [ ] Preserve the brand-blue selected tabs.
- [ ] Preserve distinct canvas, panel, overlay, alternate, and sunken surfaces.
- [ ] Preserve readiness text on container rows.
- [ ] Preserve severity badges and error rails on log rows.
- [ ] Preserve numeric latency and utilization values.
- [ ] Preserve dash patterns and non-color chart distinctions.
- [ ] Preserve mobile result counts, container identity, CPU, and readiness.
- [ ] Preserve light and dark theme parity.
- [ ] Preserve the existing 3:1 essential-control boundary target.
- [ ] Preserve the existing 4.5:1 normal-text target.

## Decisions required before implementation

### Boldness source

Choose one primary direction. Do not mix all three at equal strength.

- [x] **Recommended — signal-first:** Errors, thresholds, and active investigation state own the strongest color.
- [ ] **Alternative — service-rich:** Stable service colors appear more prominently across rows, charts, and facets.
- [ ] **Alternative — brand-led:** Blue-tinted analytical surfaces carry more of the visual identity.

### Normal latency treatment

- [ ] **Recommended — threshold-only:** Normal values use a neutral bar. Amber and red appear only after thresholds.
- [ ] **Alternative — continuous heat scale:** Every value maps to a continuous semantic scale.
- [x] **Alternative — service color:** Normal values use the stable color of the service and child segments retain their own stable breakdown colors.

### Delivery scope

- [ ] **Recommended — top three issues on both pages:** Latency hierarchy, chart readability, and stable service identity.
- [x] **Alternative — Log Explorer first:** Finish investigation signal hierarchy before Containers.
- [ ] **Alternative — full phase:** Complete every task in this document.

Record the selected decisions here before implementation:

- Boldness source: `Signal-first`
- Normal latency treatment: `Service/kind breakdown colors with separate threshold markers`
- Delivery scope: `Log Explorer first`

## Priority 1 — Fix latency signal dominance

### Finding

The Log Explorer latency column uses repeated saturated green, lime, and red blocks. The column occupies a large area and can attract more attention than errors, service identity, and query results. Normal latency can look urgent even when nothing is wrong.

### User impact

An on-call engineer can mistake visual intensity for operational severity. This delays the first useful hypothesis during an incident.

### Tasks

- [x] Write a failing regression test for the new latency-state mapping.
- [x] Define typed latency states instead of selecting colors directly from raw values.
- [x] Include at least `Normal`, `Warning`, `Critical`, and `Missing` states.
- [x] Keep the numeric duration visible in every non-missing state.
- [x] Render normal latency with the selected service/kind breakdown treatment.
- [x] Add amber warning markers without replacing breakdown colors.
- [x] Add red critical markers without replacing breakdown colors.
- [x] Keep missing latency as an em dash or explicit missing state.
- [x] Add a shape, marker, icon, or label for warning and critical states.
- [x] Make sure latency colors are documented as service/kind identity, not success or readiness.
- [x] Make sure that missing telemetry cannot resemble zero latency.
- [x] Verify the treatment in flat logs, trace trees, and session rows.
- [x] Verify the treatment in light and dark themes.
- [x] Verify the treatment at desktop and mobile widths.

### Acceptance criteria

- [x] Normal latency no longer dominates a page of healthy events.
- [x] Warning and critical latency remain visible during rapid scanning.
- [x] Warning and critical states remain identifiable without color.
- [x] Error rows remain more prominent than normal latency bars.
- [x] Numeric latency remains readable at every supported width.

## Priority 2 — Strengthen light-mode analytical regions

### Finding

Light mode now has distinct surfaces, but the Containers charts, filter rail, and inventory table still read as one cool administrative plane. The page feels correct rather than decisive.

### Tasks

- [ ] Write a failing token or rendered-class test before changing analytical surfaces.
- [ ] Define one analytical-region surface role if the current tokens cannot express it.
- [ ] Use a restrained blue-violet tint for chart regions.
- [ ] Keep the main data table neutral enough for dense reading.
- [ ] Strengthen chart-header strips without creating heavy cards.
- [ ] Keep filter controls visually subordinate to the data.
- [ ] Render active filters as compact brand-tinted chips.
- [ ] Include a clear remove action on each active-filter chip.
- [ ] Keep inactive filters neutral.
- [ ] Do not tint the full page canvas.
- [ ] Do not tint every table row.
- [ ] Do not use gradients.
- [ ] Tune the dark analytical surface independently.

### Acceptance criteria

- [ ] Charts, filters, and inventory read as separate regions in light mode.
- [ ] The result data remains the primary visual focus.
- [ ] Active filters are visible without opening the filter rail.
- [ ] Passive chrome does not compete with active investigation state.
- [ ] Dark mode retains equal or better depth.

## Priority 3 — Improve chart legends and series readability

### Finding

Both pages use small legends and thin chart marks. Container charts can show many tiny colored labels. On mobile, the legend requires too much horizontal scanning.

### Tasks

- [ ] Write failing tests for legend reduction and stable series ordering.
- [ ] Show the top four or five series by default.
- [ ] Group the remaining series under `Other` or a `+N` disclosure.
- [ ] Keep the selected series visible when it falls outside the top set.
- [ ] Increase the primary series line weight.
- [ ] Reduce secondary-series opacity without hiding data.
- [ ] Keep the existing dash-pattern distinction.
- [ ] Add direct labeling for the selected or hovered series where the chart supports it.
- [ ] Keep tooltip text and values readable in both themes.
- [ ] Keep legends keyboard accessible.
- [ ] Add an accessible summary of the chart state outside the canvas.
- [ ] Verify that the mobile legend does not create document overflow.
- [ ] Verify the empty, loading, one-series, and many-series states.

### Acceptance criteria

- [ ] A user can identify the primary series in less than five seconds.
- [ ] The legend remains readable on a 390px viewport.
- [ ] The chart remains understandable under common color-vision deficiencies.
- [ ] Secondary series remain available without overwhelming the default view.
- [ ] Loading and empty states occupy the same stable chart shell.

## Priority 4 — Guarantee cross-surface service identity

### Finding

A service must keep the same color in charts, facets, legends, and rows. Any drift forces users to relearn the scene after each context switch.

### Tasks

- [ ] Inventory every service-color call site in Haskell and TypeScript.
- [ ] Route all service colors through one stable service hash.
- [ ] Use one theme-aware categorical palette per theme.
- [ ] Keep series order deterministic.
- [ ] Use the service name, not operation name or span name, as the hash key.
- [ ] Define how missing service names map to the fallback color.
- [ ] Keep semantic error, warning, success, and information colors outside the service palette.
- [ ] Define when semantic status overrides service identity.
- [ ] Preserve service identity with a rail, marker, label, or pattern when status color overrides it.
- [ ] Add regression tests for stable assignment.
- [ ] Add regression tests for theme changes.
- [ ] Keep the Chartshot palette synchronized with the browser palette.
- [ ] Verify Log Explorer charts, facets, rows, and Containers charts together.

### Acceptance criteria

- [ ] The same service has the same categorical color everywhere in one theme.
- [ ] Theme changes preserve service identity while using theme-appropriate values.
- [ ] Status colors never become accidental service colors.
- [ ] Server-rendered charts match browser-rendered charts.

## Priority 5 — Reduce Log Explorer control competition

### Finding

The first viewport exposes visualization modes, quick queries, monitor controls, time controls, facets, chart actions, result-view controls, and query actions at once. Grouping is good, but progressive disclosure is weak.

### Tasks

- [ ] Record the primary action for each investigation state.
- [ ] Count visible decision options at each stage.
- [ ] Keep the query and search action dominant before results load.
- [ ] Keep result evidence dominant after results load.
- [ ] Move secondary chart actions into an existing options menu where possible.
- [ ] Keep the monitor action available but visually subordinate until the query is valid.
- [ ] Review whether all five visualization modes must remain visible.
- [ ] Keep frequently used modes visible and disclose the rest.
- [ ] Keep quick-query chips to three or fewer.
- [ ] Preserve keyboard shortcuts and power-user paths.
- [ ] Preserve current location and selected-state cues.
- [ ] Verify that progressive disclosure survives HTMX morph swaps.

### Acceptance criteria

- [ ] The first viewport has one obvious primary action.
- [ ] No decision point presents more than four equally weighted choices.
- [ ] Power-user efficiency does not regress.
- [ ] Secondary controls remain discoverable.

## Priority 6 — Add one functional Monoscope signature

### Finding

The telemetry workspace is product-specific. The global header and icon rail remain category-generic.

### Constraints

The signature must be functional. Do not add decorative gradients, mascots, glowing effects, or ornamental illustrations.

### Candidate directions

- [ ] A thin active-workspace rail that carries the current product area.
- [ ] A deployment-environment accent that helps prevent production/staging confusion.
- [ ] A restrained telemetry pulse motif tied to live ingestion state.

### Tasks

- [ ] Choose one signature direction only.
- [ ] Explain the operational meaning of the chosen direction.
- [ ] Reuse semantic tokens instead of adding a one-off color.
- [ ] Keep the signature quiet in normal states.
- [ ] Make the signature more visible only when it carries active state.
- [ ] Verify the signature in light and dark themes.
- [ ] Verify the signature with the side navigation collapsed and expanded.
- [ ] Verify the signature at mobile width.

### Acceptance criteria

- [ ] The shell feels recognizably Monoscope.
- [ ] The signature communicates useful state.
- [ ] The signature does not compete with errors or active investigation state.

## Accessibility tasks

- [ ] Check normal text at 4.5:1 or higher.
- [ ] Check large text at 3:1 or higher.
- [ ] Check essential control boundaries at 3:1 or higher.
- [ ] Check essential chart marks at 3:1 or higher.
- [ ] Keep visible focus indicators on every interactive control.
- [ ] Check focus indicators against every adjacent surface.
- [ ] Ensure that chart legends and summaries are available to screen readers.
- [ ] Ensure that chart selection is keyboard accessible.
- [ ] Increase compact checkbox and icon-button hit areas where they fall below the project target.
- [ ] Keep every error and warning state identifiable without color.
- [ ] Test at 200% browser zoom.
- [ ] Test common color-vision deficiencies.
- [ ] Respect reduced-motion preferences for chart and state transitions.

## Persona checks

### Alex — power user

- [ ] Complete the primary query and filter flow using only the keyboard.
- [ ] Confirm that KQL, facets, quick filters, saved queries, and result modes remain fast.
- [ ] Confirm that progressive disclosure does not hide frequent expert actions.
- [ ] Measure the number of focus steps from query input to results.

### Sam — keyboard and low-vision user

- [ ] Complete the primary flow without a pointer.
- [ ] Confirm visible focus on query, filters, chart controls, and result actions.
- [ ] Confirm that chart state is available without seeing the canvas.
- [ ] Confirm that compact controls remain operable at 200% zoom.

### Maya — on-call SRE

- [ ] Show the failing service and impact scope near the result header.
- [ ] Confirm that normal latency cannot impersonate urgency.
- [ ] Confirm that error and warning rows dominate normal telemetry.
- [ ] Confirm that the first useful hypothesis is visible without opening a row.

## Minor findings

- [ ] Keep brand blue off passive labels.
- [ ] Keep readiness labels on mobile container rows.
- [ ] Keep the query example quieter than valid KQL.
- [ ] Strengthen light-mode chart axis labels by one step if contrast permits.
- [ ] Tune dark values independently instead of mirroring light values.
- [ ] Keep help and recovery text close to the action that can fail.
- [ ] Add specific recovery guidance to empty and query-error states.

## Test plan

### Regression-first requirements

- [ ] Write a failing test before every behavior change.
- [ ] Keep each regression test named after the user-visible failure.
- [ ] Prefer integration tests for rendered Haskell pages.
- [ ] Use focused Vitest tests for TypeScript color and chart behavior.
- [ ] Avoid snapshot tests that only record large HTML strings without semantic assertions.

### Haskell verification

- [ ] Add or update Log Explorer integration tests.
- [ ] Add or update Containers integration tests.
- [ ] Read `build.log` for compile results.
- [ ] Read `build-test-dev.log` for integration-test results.
- [ ] Do not run `cabal build` or `cabal test` directly.

### Frontend verification

- [ ] Run the focused color-mapping tests.
- [ ] Run chart and log-list tests affected by the change.
- [ ] Run the full web-components test suite.
- [ ] Run TypeScript typecheck.
- [ ] Read `web-components.log` for the Vite watcher result.
- [ ] Rebuild Tailwind with `make post-css`.

### Quality gates

- [ ] Run `make fmt`.
- [ ] Run `make lint`.
- [ ] Run `git diff --check`.
- [ ] Run the Impeccable detector once after all UI edits.
- [ ] Verify that no existing unrelated worktree changes were overwritten.

## Visual verification

Use one bounded inspection round after implementation, one grouped correction pass, and one confirmation round.

### First inspection round

- [ ] Capture Log Explorer at 1440×1000 in light mode.
- [ ] Capture Log Explorer at 390×844 in light mode.
- [ ] Capture Containers at 1440×1000 in light mode.
- [ ] Capture Containers at 390×844 in light mode.
- [ ] Capture both pages at 1440×1000 in dark mode.
- [ ] Inspect default, hover, focus, active, selected, disabled, loading, empty, error, and missing-data states.
- [ ] Batch every discovered issue into one correction pass.

### Confirmation round

- [ ] Re-capture all affected viewports.
- [ ] Confirm no document-level horizontal overflow.
- [ ] Confirm that selected, hover, and disabled states remain distinct.
- [ ] Confirm that normal latency is subordinate to errors.
- [ ] Confirm that active filters are visible.
- [ ] Confirm that primary chart series are identifiable.
- [ ] Confirm dark-mode parity.
- [ ] Stop after the confirmation round unless a blocking defect remains.

## Final acceptance checklist

### Signal hierarchy

- [ ] Errors and threshold breaches own red and amber.
- [ ] Services own stable categorical colors.
- [ ] Brand blue identifies action, focus, and selection.
- [ ] Normal telemetry remains calm.
- [ ] The most important incident signal dominates the first viewport.

### Log Explorer

- [ ] Normal latency does not overpower severity.
- [ ] Error and warning rows remain identifiable without color.
- [ ] Service identity remains visible when status color appears.
- [ ] The query remains the primary action before results load.
- [ ] Results become the primary focus after loading.
- [ ] Secondary actions use progressive disclosure.

### Containers

- [ ] Charts, filters, and inventory read as separate regions.
- [ ] Active filters remain visible outside the filter drawer.
- [ ] The primary series is easy to identify.
- [ ] Readiness and utilization states remain visible on mobile.
- [ ] Missing telemetry remains distinct from zero.

### Accessibility and themes

- [ ] Light mode feels decisive without becoming colorful noise.
- [ ] Dark mode retains equal hierarchy and contrast.
- [ ] Every semantic color has a second signal.
- [ ] Text, controls, and chart marks meet their contrast targets.
- [ ] Keyboard and screen-reader users can access the primary workflow.

## Critique record

- Design health: **31/40 — Good**
- Cognitive load: **Moderate on Log Explorer; low-to-moderate on Containers**
- Deterministic detector result at planning time: `[]`
- Critique snapshot: `.impeccable/critique/2026-08-29T07-03-31Z__src-pages-logexplorer-log-hs.md`
- Previous implementation plan: `plans/light-mode-color-contrast.md`

## Latency hierarchy implementation record

Completed the Log Explorer-first colorize and polish scope on 2026-08-29.

- The latency track and child segments use stable service/kind colors so the bar shows where time was spent.
- 1–5 second latency adds a triangle; latency from 5 seconds adds a circle-exclamation marker without replacing the breakdown palette.
- Missing latency renders as an em dash.
- Desktop bars and compact mobile badges retain the numeric duration and accessible state label.
- Non-error trace extenders and the grouped query shell retain explicit weak boundaries.
- The query editor remains the strong primary input; focus and parse-error states retain brand and semantic emphasis.
- Scroll performance: the virtualizer health check runs once after scrolling settles instead of forcing layout for every wheel event, and unwrapped summary rows are capped at 1200px instead of 3600px.
- Verified tree, list, sessions, desktop, mobile, light, dark, hover, focus, disabled, missing, warning, critical, and error states.
- Follow-up critique: `.impeccable/critique/2026-08-29T10-01-00Z__src-pages-logexplorer-log-hs-dark-mode.md` (**32/40**).
