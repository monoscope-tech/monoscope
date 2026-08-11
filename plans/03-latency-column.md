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

**Chosen: B + C, with A folded in — and B simplified to one rule for every row.**

Rather than two geometries (composition at depth 0, timeline below it), *the row is the
axis*. Every row's bar is exactly its own duration; its **direct** children are laid out
inside it at their real offsets, coloured by the chosen dimension; the gaps between them
are its self time. That is one rule, it reads the same at every depth, and it makes a root
row answerable ("most of this request is one `postgres` block two thirds of the way in")
without a second mental model for children.

Two things fall out of it:

- The frame overlay goes. It marked the bar's own bounds — three decorative lines
  restating the bar.
- Trace position is no longer encoded in the bar's geometry, because that is exactly what
  made small children invisible. It moves into the title/`aria-label`
  (`+1.2ms into the trace`), which costs no pixels and, unlike a colour, is readable by a
  screen reader. The trace waterfall remains where trace-relative geometry belongs.

`kind` is already projected on every row, so *by kind* (server / client / internal /
producer / consumer / log) needs no new query work — and it is the one breakdown that
answers "is this my code or my dependencies?". Its colours are fixed rather than hashed,
because they mean something; service colours stay hashed, because they only identify.

## What shipped

1. `latencySegments(row, children)` — pure, exported, tested. Interval **intersection**
   against the row window, not an offset clamp: a child whose clock skewed it entirely
   before its parent contributes nothing, rather than being pinned to the start of the bar
   as though it happened there.
2. `latencyTitle(dim, row, segments)` — the tooltip and `aria-label`: total, trace offset,
   self time, and the per-label split ordered by contribution.
3. Server: `serviceColors` built from the `service` column, not `span_name`
   (`Log.hs:569`). The payload key is unchanged — it is the contract with the renderer, and
   it is finally true to its name.
4. Dimension switch in the latency column's existing header dropdown, persisted to
   `localStorage` (guarded — jsdom has no `localStorage`, and a reading preference must
   never be what stops the list from mounting).
5. Duration text was **not** added: `generateSummary` already emits a `duration`
   right-badge that renders in this very cell, so the number the column "never showed" was
   already there.

## Correction (2026-08-11)

"The row is the axis" was applied to *every* row, which flattened the trace breakdown an
expanded row is there to show: child rows lost their position and the `|---[]---|` frame,
so the rows no longer read as one request. Restored, scoped by expansion state
(`latencyBar`): an expanded trace puts every one of its rows back on the **trace** axis,
with the frame marking the trace bounds — a short span must read as short *and placed*.
A collapsed row keeps the row-relative composition, which is what fixed the sub-pixel
sliver: it has no siblings to line up with, so the trace axis buys it nothing.
