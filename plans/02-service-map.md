# 02 — Service map: prod render failure + node context menu

Doing this one first: it's a live prod regression and it is small.

## Findings

### A. Hyperscript parse error kills the filter input

`src/Pages/ServiceMap.hs:78`

```haskell
[__|on input send "service-map-filter"(q: my value) to the closest <div/> then halt|]
```

The deployed build shows the unquoted form in the DOM (`send service-map-filter(...)`),
i.e. the attribute value reaches the browser without the quotes. Either way the event
name is the problem: `_hyperscript`'s `send` command parses the event name with
`dotOrColonPath`, which is an identifier path — **a hyphen terminates it**, and a string
literal is not an identifier either. `service-map-filter` tokenizes as
`service` `-` `map` `-` `filter`, so the parser is left staring at `(q:` and reports
`Unexpected Token :`.

**Fix:** stop routing this through hyperscript's `send` at all. The event needs to reach
`document` (the TS listener is `document.addEventListener(FILTER_EVENT, …)`), yet the
hyperscript says `to the closest <div/>` — which only works because the event bubbles.
A plain `on input` handler that calls a documented interop function is both shorter and
survives a dashed event name:

- Keep the wire event name `service-map-filter` (the TS contract, and dashed custom event
  names are the convention everywhere else).
- Emit it with hyperscript's `trigger`… no — same grammar. Use the escape hatch
  hyperscript provides for exactly this: `js` interop, i.e. `call
  window.serviceMapFilter(me.value)` — one call, at the use site, no dashed identifier in
  hyperscript's grammar.
- `serviceMapFilter` is registered by `service-map.ts` next to `getServiceMapHandle`, so
  the contract lives with the renderer.

### B. `Cannot read properties of null (reading 'classList')`

`web-components/src/service-map.ts` uses non-null assertions (`querySelector(...)!`) for
every template hook. `!` is erased at runtime, so any hook missing from the Lucid template
throws mid-`buildCards`, and because `buildCards` runs inside `rebuild()` **before**
`fit()`, the whole map ends up blank — exactly the prod symptom.

The template (`Pkg/Components/ServiceMap.hs:147`) does carry every hook today, so the
likelier trigger is the asset-hash skew from `727819dee` (`hash static asset URLs by path`)
serving a stale bundle against a new template. Either way the renderer must not be able to
blank the map over one missing node:

**Fix:** resolve the hooks once per clone and skip the decoration when a hook is absent,
rather than asserting. Same for `[data-node-health]` / `[data-node-icon]` /
`[data-node-errors]` in `paint()`.

### C. "View in trace search" is broken and "View logs" is redundant

`service-map.ts:586` links to `?query=…&viz_type=traces`. **There is no `traces` viz
type** — `visTypes` (`Pkg/Components/LogQueryBox.hs:402`) is
`logs | timeseries | timeseries_line | patterns | sessions`. An unknown value falls
through to the default, so the link silently lands on the plain log list — indistinguishable
from "View logs", which is the user's complaint.

Logs and spans genuinely are one table and one surface here (`pSource` only knows
`spans` and `metrics`), so two menu entries pointing at the same view is the bug. The
useful distinction is **kind**, exactly as the user guessed:

| Menu item | Query |
|---|---|
| View traces | `resource.service.name=="X" AND kind!="log"` |
| View logs | `resource.service.name=="X" AND kind=="log"` |

Both at `viz_type=logs`. That gives two menu entries that actually differ, and both
resolve to a valid viz type.

## Plan

1. `ServiceMap.hs` — replace the `send`-with-dashes hyperscript with a `call` into a
   renderer-owned interop function; keep the wire event name.
2. `service-map.ts` — export/register `window.serviceMapFilter`; drop the `!` assertions in
   `buildCards`/`paint` for optional decoration hooks.
3. `service-map.ts` — fix the two menu hrefs to the kind-filtered queries above; rename the
   `traces` label so the two entries read as a real choice.
4. Vitest coverage in the existing web-components harness: a filter call reaches the
   handle, and the menu hrefs are the kind-filtered ones.
