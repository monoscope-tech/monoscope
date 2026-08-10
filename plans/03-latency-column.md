# 03 — Log-list latency column: break down by service/kind, rethink the UX

## What it does today

`web-components/src/log-list.ts:2179` (`latency_breakdown` cell) → `spanLatencyBreakdown()`
at `:3126`. Per row it draws a mini-waterfall on a **trace-wide** axis:

- one bar for the row's own span, positioned `start - traceStart` wide `duration`,
- one bar per entry in `rowData.childrenTimeSpans`,
- for child rows (`depth > 0`) and expanded roots, an extra "frame" overlay: two vertical
  brand-colored edges plus a horizontal rule across the full bar width.

## Why it doesn't read

1. **The colour is keyed on the wrong field.** `this.serviceColors[…'span_name']` — both for
   the row (`:2186`) and every child (`:2190`). But `serviceColors` is built server-side
   from `span_name` too (`Log.hs:569`: `getServiceColors $ V.mapMaybe (colOf "span_name")`),
   so the palette is a per-*operation* palette wearing the name "service colors". Two spans
   in the same service get different colours; the same operation in two services gets one.
   The column is therefore **already** a breakdown — just by the least useful dimension.
   `service` is projected on every row (`Pkg/Parser.hs:579`,
   `resource___service___name as service`) and is free to switch to.
2. **Child rows re-draw the whole trace.** A child bar is placed on the same
   `0 … traceEnd` axis as the root, so a 3ms child inside a 2s trace is a sub-pixel sliver,
   and the "frame" overlay drawn around it is three decorative lines that carry no data —
   it marks the bar's own bounds, which are already the bar. This is the "child ones are
   difficult to make sense of" complaint.
3. **No numbers, ever.** The column is 120px of pure geometry with no duration text and no
   tooltip, so a reader cannot answer "how long?" without opening the row.
4. **`bg-slate-400` fallback.** Any row whose `span_name` misses the palette is grey, which
   is most of them once the palette is keyed on operations.

## Directions considered

**A. Colour by service, keep the geometry.** Smallest change; fixes (1) and (4). Doesn't
fix the child-row problem.

**B. Root = composition bar, child = self-relative bar.** At depth 0 the bar stops being a
timeline and becomes a **stacked composition**: 100% of the row's duration, segmented by
where the time went, ordered by contribution. At depth > 0 the bar keeps the timeline
meaning but is scaled to the **parent's** window, not the trace's — which is the comparison
a reader of a child row is actually making. Drops the frame overlay entirely.

**C. Dimension is a user choice.** A small control in the column header: *by service* /
*by kind* / *timeline*. Persisted like the other log-list column prefs.

**Chosen: B + C, with A folded in.** The composition bar is what makes the root row
answerable at a glance ("62% of this request was in `postgres`"), the parent-relative axis
is what makes a child row answerable, and the dimension switch is cheap once the breakdown
is computed from a named field rather than hardcoded to `span_name`.

`kind` is already projected on every row, so *by kind* (server / client / internal /
producer / consumer / log) needs no new query work — and it is the one breakdown that
answers "is this my code or my dependencies?".

## Plan

1. Extract the breakdown from `spanLatencyBreakdown`'s parameters into a
   `breakdownOf(row, dimension)` that returns `{label, colour, ns}[]`, keyed on a named
   column (`service` | `kind` | none). Both the row's own span and `childrenTimeSpans`
   feed it.
2. Server: build `serviceColors` from the `service` column, not `span_name`
   (`Log.hs:569`). Keep the payload key — it is the contract with the renderer and it will
   finally be true to its name.
3. Root rows render the stacked composition bar with a "self" remainder segment
   (duration minus accounted children) so the segments always sum to the whole.
4. Child rows render on the parent window; delete the frame overlay.
5. Duration text in the cell, right-aligned, tabular-nums — the number the column has never
   shown.
6. Header control for the dimension, persisted with the existing column prefs.
7. Vitest: composition segments sum to the row duration; switching dimension re-partitions
   the same total; a child row's bar is parent-relative.

## Note

Steps 1–5 are the user-visible fix. Step 6 is the part to drop first if the column-pref
plumbing turns out to be expensive — *by service* is the right default either way.
