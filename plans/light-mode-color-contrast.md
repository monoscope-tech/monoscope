# Light-mode color and contrast plan

## Scope

This plan covers the color system and visual contrast on these surfaces:

- `/p/00000000-0000-0000-0000-000000000000/log_explorer`
- `/p/00000000-0000-0000-0000-000000000000/infrastructure/containers`

The review used desktop and mobile screenshots in light mode. It also used desktop screenshots in dark mode.

The work must preserve the current Instrument Panel design direction. It must not add decorative color, gradients, glass effects, or neon effects.

## Summary

Light mode feels dull because its structural contrast is compressed. The brand and semantic colors are already strong.

Most surfaces use nearly identical near-white values. Panels, controls, tables, and the page canvas merge into one plane.

Dark mode has clearer surface steps. Its values move from L8 to L12.5, L18, and L22. Light mode does not have an equivalent range.

The main correction is not a larger palette. The interface needs clearer surfaces, stronger boundaries, and more useful semantic signals.

## Current assessment

| Area | Light mode | Dark mode |
|---|---:|---:|
| Visual hierarchy | 6/10 | 8/10 |
| Color semantics | 7/10 | 8/10 |
| Structural contrast | 5/10 | 8/10 |
| Data visualization | 6/10 | 7/10 |
| Mobile clarity | 5/10 | Not reviewed |

## Token evidence

The values below come from `static/public/assets/css/tailwind.css`. The ratios use standard WCAG contrast calculations.

| Pair | Approximate contrast | Finding |
|---|---:|---|
| `bgSunken` on `bgBase` | 1.04:1 | The surface step is almost invisible. |
| `fillWeaker` on `bgBase` | 1.04:1 | Raised panels merge into the canvas. |
| `fillWeak` on `bgBase` | 1.08:1 | Hover and selected fills are too quiet. |
| `strokeWeak` on `bgBase` | 1.23:1 | The stroke is too weak for essential control boundaries. |
| `strokeStrong` on `bgBase` | 3.05:1 | This stroke can support essential boundaries. |
| `textWeak` on `bgBase` | 12.58:1 | Secondary text has strong contrast. |
| `textStrong` on `bgBase` | 15.77:1 | Primary text has strong contrast. |
| Brand blue on `bgBase` | 4.64:1 | The brand color is not the source of the dull appearance. |
| White on brand blue | 4.76:1 | Primary button content passes WCAG AA. |
| Information text on `bgBase` | 5.95:1 | Information text passes WCAG AA. |
| Warning text on `bgBase` | 4.92:1 | Warning text passes WCAG AA. |
| Success text on `bgBase` | 5.83:1 | Success text passes WCAG AA. |
| Error text on `bgBase` | 6.15:1 | Error text passes WCAG AA. |

### Root cause

These light tokens are identical or almost identical:

- `bgBase`, `bgRaised`, and `bgOverlay` use the same value.
- `bgSunken` and `bgAlternate` differ from the base by only 1.04:1.
- `.surface-raised` and `.surface-table` use `fillWeaker` instead of a surface token.
- `fillWeaker` creates only 1.04:1 contrast against the base.
- `strokeWeak` creates only 1.23:1 contrast against the base.

The text palette is not weak. The surface and boundary palette is weak.

## Color direction

### Emotional temperature

Use a cool and technical temperature. Preserve the blue-violet brand hue and cool graphite neutrals.

### Dominant relationship

Use signal blue against cool neutral surfaces. Use semantic colors for operational state.

### Contrast range

Use high structural contrast and controlled color saturation. The result must remain calm during an incident.

### Color dosage

Use neutral color for the canvas and passive content. Use strong color for interaction, selection, health, severity, and identity.

Do not add color to elements that have no operational meaning.

## Design-system conflict

`DESIGN.md` contains conflicting rules for information blue:

- The One-Blue Rule says that information blue must move to hue 261.
- The Do section says that information blue and brand blue must remain distinct.

This conflict can produce more palette drift. The implementation must resolve it first.

The preferred direction is one blue hue family. Interaction and information can use different lightness and chroma values.

## Cross-surface findings

### What works

- Brand blue creates a clear primary action.
- Text contrast is strong in both themes.
- Error, warning, and success colors pass normal-text contrast requirements.
- Dark mode has clear surface levels.
- The density supports experienced operators.
- Existing semantic tokens provide a good base for the work.

### What does not work

- Light-mode surfaces merge into one pale plane.
- Essential controls often rely on `strokeWeak` as their only boundary.
- Selected states use fills that are close to invisible.
- Bright colors occupy small and inconsistent areas.
- Charts use colors that work better on dark surfaces than white surfaces.
- The query editor can become louder than the investigation results.
- Mobile layouts remove useful color signals and expose clipping problems.

## Log Explorer findings

### Strengths

- The query and search action are easy to find.
- Service and severity colors carry real meaning.
- The information density suits incident work.
- Dark mode gives the data region clear structure.

### Problems

- The blue query outline dominates the page when the query editor has focus.
- The query controls are visually louder than the results.
- Chart cards merge into the page canvas.
- Chart axes and grid lines are too faint in light mode.
- Service markers are too thin for fast scanning.
- Bright latency blocks dominate row-level severity.
- Mobile mode hides the timeline and removes a useful source of visual context.
- The mobile header truncates labels and creates fragmented navigation.

### Recommended changes

- Use a neutral query boundary when the editor is not focused.
- Keep the blue perimeter for the real focus state.
- Give chart cards a real surface token and a clearer panel boundary.
- Increase the contrast of chart axes and essential grid lines.
- Increase service markers to a modest 3px or 4px signal.
- Add a semantic row state for errors and warnings.
- Pair each semantic row color with an icon, label, or status text.
- Make the selected visualization tab more visible.
- Preserve a compact summary signal on mobile when the full timeline is hidden.
- Correct mobile header truncation and control crowding.

## Containers findings

### Strengths

- The table density supports fleet inspection.
- CPU and memory charts provide useful context.
- The Infrastructure navigation is understandable.
- The table already has data for readiness and utilization states.

### Problems

- The chart area, table, filter rail, and canvas use almost the same color.
- Several thin chart lines have weak contrast against white.
- The 20-color legend creates noise before it creates recognition.
- All chart series receive equal emphasis.
- Rows provide little health or utilization information at a glance.
- Active filters depend on faint fills and strokes.
- Mobile navigation and chart legends clip horizontally.
- The mobile table loses important context at the right edge.

### Recommended changes

- Put charts and tables on distinct light-mode surfaces.
- Use a darker chart palette in light mode.
- Use a lighter chart palette in dark mode.
- Emphasize the most important series.
- Reduce the opacity of secondary series.
- Add semantic readiness and utilization signals to rows.
- Pair health colors with text or icons.
- Give active filters a visible brand-tinted state.
- Correct mobile legend and navigation overflow.
- Preserve container identity and the primary metric in mobile rows.

## Chart palette findings

`web-components/src/colorMapping.ts` uses a fixed palette of Tailwind `-400` colors. The same palette serves both themes.

Several colors have weak contrast against a white surface. Amber, yellow, cyan, lime, and other high-lightness colors are the main risk.

The fixed palette explains part of the theme difference. The colors look vivid on dark surfaces but washed out on white surfaces.

The file also contains hard-coded status, percentile, and fallback colors. Some colors do not use the semantic token system.

The source contains a synchronization warning for the external `chartshot` copy. Any palette change must include that follow-up.

## Proposed token changes

The exact values require visual tuning and contrast calculations. These ranges define the intended structure.

### Surfaces

- Keep the primary content surface near white.
- Move the page canvas and sunken regions toward L96.5 to L97.5.
- Use a small cool chroma near the brand hue.
- Give `bgRaised` a distinct value from `bgBase`.
- Give `bgOverlay` a distinct value from `bgRaised`.
- Map `.surface-raised` and `.surface-table` to semantic surface tokens.

### Strokes

- Keep `strokeWeak` for decorative dividers.
- Use `strokeStrong` for essential controls until a dedicated token exists.
- Add `strokeControl` only if the existing names cannot express the role.
- Make sure that essential control boundaries reach 3:1 against adjacent surfaces.
- Use a middle-strength panel stroke for tables and charts.

### Weak fills

- Increase `fillBrand-weak` from 5% toward 10% to 12% in light mode.
- Increase semantic weak fills from 5% toward 8% to 10% in light mode.
- Keep hover fills quieter than selected fills.
- Keep disabled fills visually separate from selected fills.

### Charts

- Define separate light and dark categorical palettes.
- Use darker colors for light-mode data marks.
- Use lighter colors for dark-mode data marks.
- Make sure that essential data marks reach 3:1 against the chart surface.
- Use labels, shapes, or patterns when color is the only current distinction.

## Implementation plan

### Phase 0: Record the contract and baseline

- [x] Resolve the information-blue conflict in `DESIGN.md`.
- [x] Record one blue hue family as the preferred direction, if approved.
- [x] Record the roles of canvas, base, raised, overlay, and sunken surfaces.
- [x] Record the difference between decorative and essential strokes.
- [x] Record the minimum contrast targets for text, controls, and charts.
- [x] Capture baseline screenshots for both target pages.
- [x] Capture desktop and mobile screenshots in light mode.
- [x] Capture desktop screenshots in dark mode.
- [x] Add a small automated contrast calculation for the core tokens.

### Phase 1: Correct the light-mode token system

Primary file: `static/public/assets/css/tailwind.css`

- [x] Write a failing token contrast test before the token changes.
- [x] Give `bgBase`, `bgRaised`, and `bgOverlay` distinct values.
- [x] Increase the difference between `bgBase` and `bgSunken`.
- [x] Increase the difference between `bgBase` and `bgAlternate`.
- [x] Map `.surface-raised` to a semantic surface token.
- [x] Map `.surface-table` to a semantic surface token.
- [x] Reserve `strokeWeak` for decorative separators.
- [x] Use `strokeStrong` for essential control boundaries.
- [x] Add a panel stroke role only if existing tokens cannot express it.
- [x] Increase the selected-state brand fill.
- [x] Increase the semantic weak fills.
- [x] Keep hover fills quieter than selected fills.
- [x] Make sure that disabled states do not resemble selected states.
- [x] Update `DESIGN.md` with the final token values and role rules.

### Phase 2: Add theme-aware chart colors

Primary files:

- `web-components/src/colorMapping.ts`
- `web-components/src/widgets.ts`
- `web-components/test/color-mapping.test.ts`

- [x] Write failing light-mode palette contrast tests first.
- [x] Add separate categorical palettes for light and dark mode.
- [x] Preserve stable hash-based service color assignment.
- [x] Keep adjacent series visually distinct.
- [x] Move status colors toward semantic CSS tokens.
- [x] Move percentile colors toward semantic CSS tokens.
- [x] Remove legacy fallback hues when a semantic token exists.
- [x] Keep all essential chart marks at 3:1 or more against the chart surface.
- [x] Add a non-color distinction where adjacent hues remain difficult to identify.
- [x] Invalidate cached colors after a theme change.
- [x] Update the external `chartshot` copy after the browser palette is final.
- [x] Add regression tests for theme changes and stable color assignment.

### Phase 3: Improve Log Explorer color hierarchy

Primary files:

- `src/Pages/LogExplorer/Log.hs`
- `src/Pkg/Components/LogQueryBox.hs`
- `web-components/src/log-list.ts`
- `test/integration/Pages/LogExplorer/LogSpec.hs`

- [x] Write a failing regression test for each changed state first.
- [x] Make the query boundary neutral when the editor is not focused.
- [x] Keep the strong brand boundary for keyboard and pointer focus.
- [x] Strengthen the selected visualization tab.
- [x] Give timeline cards a real raised surface.
- [x] Increase essential chart-axis contrast.
- [x] Increase service marker width to 3px or 4px.
- [x] Add a subtle error-row fill.
- [x] Add a subtle warning-row fill.
- [x] Pair each error and warning color with text or an icon.
- [x] Keep normal rows neutral.
- [x] Review the dominance of latency blocks against severity signals.
- [x] Preserve a compact summary signal on mobile.
- [x] Correct mobile header truncation and control crowding.
- [x] Make sure that no new color implies an action when the element is not interactive.

### Phase 4: Improve Containers color hierarchy

Primary files:

- `src/Pages/Containers.hs`
- `src/Pkg/Components/Table.hs`
- `web-components/src/widgets.ts`
- `test/integration/Pages/ContainersSpec.hs`

- [x] Write failing regression tests for changed states first.
- [x] Give the chart region a distinct surface.
- [x] Give the inventory table a distinct surface.
- [x] Strengthen the table header background.
- [x] Strengthen essential table and filter boundaries.
- [x] Add a visible active-filter state.
- [x] Add readiness text or icons with semantic color.
- [x] Add warning treatment near utilization limits.
- [x] Add error treatment at critical utilization levels.
- [x] Keep missing telemetry as an em dash.
- [x] Do not infer a healthy state from missing telemetry.
- [x] Emphasize the most important chart series.
- [x] Reduce secondary-series opacity without hiding data.
- [x] Correct mobile legend overflow.
- [x] Correct mobile navigation overflow.
- [x] Preserve container identity and the primary metric on mobile.

### Phase 5: Dark-mode parity and system regression

- [x] Make sure that light-mode changes do not reduce dark-mode quality.
- [x] Make sure that each semantic token has a deliberate dark value.
- [x] Check all default, hover, focus, active, selected, disabled, and error states.
- [x] Check empty, loading, and missing-data states.
- [x] Check chart tooltips and overlays in both themes.
- [x] Check focus indicators against every adjacent surface.
- [x] Simulate common color-vision deficiencies.
- [x] Add text, icons, labels, or shapes where color is the only signal.

## Acceptance criteria

### System criteria

- [x] Light mode has visible canvas, base, raised, overlay, and sunken levels.
- [x] `bgBase`, `bgRaised`, and `bgOverlay` are not identical.
- [x] Essential control boundaries reach at least 3:1 contrast.
- [x] Normal text reaches at least 4.5:1 contrast.
- [x] Large text reaches at least 3:1 contrast.
- [x] Essential chart marks reach at least 3:1 contrast.
- [x] Selected states remain distinct from hover and disabled states.
- [x] Every error state has a non-color signal.
- [x] Dark mode has full parity.

### Log Explorer criteria

- [x] The results remain the primary visual focus.
- [x] The query editor uses strong blue only for focus or action.
- [x] Service identity is visible during rapid scanning.
- [x] Error and warning rows are identifiable without color.
- [x] Charts are clear against their panels.
- [x] Mobile mode keeps useful summary context.

### Containers criteria

- [x] The filters, charts, and table read as separate regions.
- [x] Chart series remain readable on white and dark surfaces.
- [x] Readiness and utilization states are visible without opening a row.
- [x] Missing telemetry remains distinct from zero.
- [x] Active filters are easy to identify.
- [x] Mobile mode preserves identity and the primary metric.

## Verification plan

Use bounded visual review passes.

1. Implement the complete token and component changes.
2. Capture desktop and mobile screenshots in one batch.
3. Capture light and dark themes in the same batch.
4. Record all defects from that batch.
5. Correct all recorded defects in one edit batch.
6. Run one final screenshot batch.
7. Stop visual tuning after the final batch.

Run these project checks:

- [x] Read `build.log` for the Haskell compile result.
- [x] Read `build-test-dev.log` for integration-test results.
- [x] Read `web-components.log` for the frontend build result.
- [x] Run the focused Vitest tests for the color mapping.
- [x] Run `make fmt`.
- [x] Run `make lint`.
- [x] Run the Impeccable detector once after all UI edits.
- [x] Use `node /Users/tonyalaribe/.agents/skills/impeccable/scripts/detect.mjs --json <changed targets>`.

Do not run `cabal build` or `cabal test` for this work. Use the live watchers and their log files.

## Non-goals

- Do not replace the current brand identity.
- Do not add decorative color to neutral content.
- Do not add a second accent hue.
- Do not add gradients, glass effects, or neon effects.
- Do not add shadows to resting cards.
- Do not reduce information density.
- Do not make all rows or panels colorful.
- Do not infer health from missing telemetry.
- Do not change dark mode by mechanically inverting light-mode values.

## Expected result

The final light theme must feel decisive rather than colorful. It must have clear planes, strong controls, and readable data marks.

The strongest colors must identify actions and operational state. Neutral surfaces must provide the structure that lets those signals work.

## Completion record

Completed on 2026-08-29.

- The light theme now has distinct canvas, raised, overlay, alternate, and sunken surfaces.
- Browser contrast scans found no text or enabled-control failures on either target page in light or dark mode.
- Light and dark chart palettes pass the 3:1 data-mark tests. Container charts also use line patterns and secondary-series opacity.
- Log rows now use severity tints with badges or error rails. Container rows use readiness labels and threshold bars.
- Desktop and mobile screenshots were checked in both themes. Mobile pages have no document overflow and retain the active tab, result count, identity, CPU, and readiness.
- `web-components`: 46 files and 847 tests passed. The focused color suite has 35 passing tests.
- Haskell live tests passed: 7 Containers examples and the Log Explorer query-focus regression.
- `make fmt`, `make lint`, TypeScript typecheck, Vite watch build, and the Impeccable detector passed.
- The external Chartshot palette sync was merged in [monoscope-tech/chartshot#1](https://github.com/monoscope-tech/chartshot/pull/1) at `61f8f4a`. Its TypeScript typecheck passed.
- Final screenshots are in `/tmp/monoscope-color-final-proof/` for this work session.
