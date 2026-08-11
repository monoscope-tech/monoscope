---
target: service map page
total_score: 19
p0_count: 2
p1_count: 2
timestamp: 2026-08-09T20-37-42Z
slug: src-pkg-components-servicemap-hs
---
⚠️ DEGRADED: single-context (session policy forbids unrequested Agent/sub-agent calls; Assessments A and B were run sequentially in this context)

Target: `src/Pkg/Components/ServiceMap.hs` + `web-components/src/service-map.ts` + `src/Pages/ServiceMap.hs`
Evidence: user screenshot (150-node graph, dark theme, ~1900px viewport), source read, `detect.mjs` on the server-rendered HTML of the demo project. Browser automation unavailable (Chrome not running with a debug port; the target project URL 302s to auth), so no live overlay was injected — no user-visible overlay exists for this run.

#### Design Health Score

| # | Heuristic | Score | Key Issue |
|---|-----------|-------|-----------|
| 1 | Visibility of System Status | 2 | No zoom level, no "pannable canvas" affordance, no feedback that click-isolation is active. Truncation notice is good but 12px and top-left. |
| 2 | Match System / Real World | 2 | Caller/Callee/Requests/Errors/p95 is exactly right. The picture isn't: 149 third-party hostnames are drawn with the same weight and geometry as your one real service. |
| 3 | User Control and Freedom | 2 | `roam` pan/zoom exists but is undiscoverable; no fit/reset control, no Esc, no way to change the 150 cap or the hop depth. |
| 4 | Consistency and Standards | 3 | Service colors match the waterfall, sprite icons and DaisyUI controls are consistent, legend teaches shape+dash. |
| 5 | Error Prevention | 2 | Nothing destructive, but the design *guarantees* the illegible state for any real fan-out topology. |
| 6 | Recognition Rather Than Recall | 2 | The node menu is excellent. But nothing in the blob is recognizable — you must already know a service name to type it into the filter. |
| 7 | Flexibility and Efficiency | 1 | Canvas is entirely unreachable by keyboard. No shortcuts, no grouping/depth controls, no click-through from table row to node. |
| 8 | Aesthetic and Minimalist Design | 1 | The hero visualization is unreadable, and it occupies ~25% of a 520×1900 canvas with 75% void beside it. |
| 9 | Error Recovery | 2 | Server-rendered error and empty states are genuinely well written. "Map is illegible" isn't modeled as a state, so there's no recovery path. |
| 10 | Help and Documentation | 2 | Legend covers shapes; nothing explains what dashed/inferred means or why 150 is the cap. |
| **Total** | | **19/40** | **Poor — the supporting chrome is good; the primary visualization does not work** |

#### Anti-Patterns Verdict

**LLM assessment**: Not AI slop. There is no gradient text, no glassmorphism, no eyebrow scaffolding, no identical-card grid. Comments show real intent ("hollow, like Datadog", "health first"), the empty/error copy is written by someone who has been on call, and the a11y fallback table is a genuinely uncommon, correct decision. The failure here is not taste — it's that the layout algorithm was designed for a DAG and the data is a **star**.

**Deterministic scan**: `detect.mjs` over the server-rendered HTML returned one warning — `flat-type-hierarchy` (12/14/16/20px, ratio 1.7:1). **False positive** for this register: the product reference explicitly prescribes a tight 1.125–1.2 scale for dense product UI. Running the detector directly on the `.hs` sources returns `[]` — it can't parse Lucid, so it contributes no coverage of the Haskell layer.

**Visual overlays**: Not available. Injection was not attempted because no debuggable browser instance was reachable and the target route requires auth. The fallback signal is the user's screenshot plus source arithmetic below.

#### Overall Impression

Everything around the map is better than the map. The dependency table, the empty states, the node menu, the shape-not-just-color legend, the search-dims-never-removes rule — that's careful work. Then the canvas renders a gray smear with a pink-and-blue hairbrush attached, and the feature fails at the only thing it exists to do.

The single biggest opportunity: **stop trying to draw 150 nodes.** Datadog's map doesn't look better because its renderer is prettier — it looks better because it never puts 149 siblings in one column. It groups third-party dependencies and defaults to a bounded neighborhood. Fix the topology handling and 80% of the visual complaint disappears without touching a single color.

#### What's Working

- **The dependency table is a first-class representation, not an afterthought.** Sorted by requests, tabular-nums, error column with a text second-signal beyond red. It is currently the most usable part of the page, and it works with no JS, on a phone, in a shared link. Keep it; promote it.
- **Health beats identity in the ring color.** `ringColor` returning `errorColor` before the hash color is the right call and is documented as such. A failing dependency can't hide behind a pretty color.
- **Search dims instead of filtering.** The `filterSelection` intersection rule (query ∩ isolation, edge lit when both endpoints survive isolation and *either* matches) means a filtered view never silently severs a path. That's a subtle correctness decision most maps get wrong.

#### Priority Issues

**[P0] The layout crushes every fan-out graph into a line**
This graph is a star: 150 services, 150 dependencies, and the legend lists exactly one service color — meaning one instrumented service (`production-shipbubble-api`) and ~149 inferred peers. `layerGraph` puts all 149 in layer 1, `assignCoords` spaces them at a fixed `Y_GAP = 96`, producing ~14,300px of virtual height. `layout:'none'` fits that extent into a `h-[520px]` box (≈456px of drawing area), so node centers land roughly **3px apart while the symbols themselves stay 26–64px wide**. Every node overlaps its neighbors ~15 deep. That is the gray blob, exactly.
*Why it matters*: the map is the feature. At 3 AM the on-call engineer gets a picture they cannot read and falls back to the table — so the canvas is pure cost.
*Fix*: (a) group inferred peers by kind + registrable domain into single count-badged nodes (`*.myshopify.com ×40`) that expand on click — this turns 149 rows into ~10 and is precisely what Datadog does with third-party endpoints; (b) inside a layer, wrap beyond ~12 rows into sub-columns rather than one infinite column; (c) clamp effective row spacing so it can never fall below `symbolSize + 8` — if the graph can't fit, zoom to a neighborhood instead of shrinking to fit; (d) default the initial view to the busiest service's 1-hop neighborhood with an explicit "expand hops" control, not fit-all-150.
*Suggested command*: `/impeccable shape` (the grouping/hop-depth model needs designing before code), then `/impeccable layout`

**[P0] Every edge is the same color, so 150 connections read as one object**
`lineStyle.color: 'source'` colors an edge by its source node. In a star, all 150 edges share one source, so all 150 are the same purple. Add `curveness: 0.06` on every edge (`autoCurveness: false`), widths up to 6px from `log2(requests)`, `opacity: 0.55`, and 9px arrowheads — at 3px spacing, edges are wider than the gap between them. They fuse into a band. The red error edges you deliberately colored are lost inside it.
*Why it matters*: "hard to even see the connections" is the user's own words, and this is the direct mechanism. Errors — the one thing the map must surface — are invisible.
*Fix*: neutral edges by default (`strokeStrong` at ~0.3 opacity), width clamped 1–3px, error red only above threshold and drawn **last** so it sits on top; drop arrowheads below a zoom threshold and show them on hover/focus only; enable `autoCurveness` so parallel edges spread instead of stacking. Reserve saturated color for the focused/hovered path.
*Suggested command*: `/impeccable colorize`

**[P1] Labels are always on with no overlap suppression**
`label: { show: true, position: 'right' }` at 11px for all 150 nodes. Right-positioned labels in a vertical fan collide by construction — that's the gray text mass in the screenshot, with only the longest hostname (`...r2.cloudflarestorage.com`) legible because it escapes the pile. echarts ships `labelLayout: { hideOverlap: true }`; it isn't used.
*Fix*: `labelLayout: { hideOverlap: true, moveOverlap: 'shiftY' }`; show labels only for nodes above an importance threshold plus the hovered/focused node; move labels below-center for fan layouts.
*Suggested command*: `/impeccable typeset`

**[P1] The canvas has no controls and no keyboard path**
`roam: true` and `draggable: true` are enabled with zero affordance that panning or zooming exists. There is no fit/reset button, no zoom in/out, no minimap, no Esc to clear isolation (only a click on empty canvas), and no keyboard route to any node — the entire graph is a canvas element.
*Why it matters*: Alex can't drive it fast, Sam can't drive it at all. It also strands users in the crushed state with no obvious way to zoom out of it.
*Fix*: add zoom −/+/fit controls bottom-right of the canvas; bind Esc to `clearIsolation`; make the dependency table rows the keyboard-accessible entry point — focusing/activating a row isolates that node on the map, which reuses the accessible representation you already built instead of bolting ARIA onto a canvas.
*Suggested command*: `/impeccable harden`

**[P2] The filter dims but doesn't re-fit, and the legend reads broken**
Typing into "Filter services" dims 149 nodes to 0.12 opacity but leaves the geometry untouched — you're left with a 3px-spaced blob that's now also faint. Meanwhile "Service colors ● production-shipbubble-api" renders a labeled swatch list with exactly one entry, which looks like a bug rather than a legend. The canvas also runs full-width (`halfW` floors at `box.width - 80`) while the content occupies the middle-left ~25%, leaving a large void.
*Fix*: after a filter, re-fit the viewport to the lit subgraph (dimming stays — the rule is right — but the camera should follow). Hide the "Service colors" row when there's ≤1 entry, or fold it into the kind legend. Stop floor-ing the virtual extent at container width.
*Suggested command*: `/impeccable polish`

#### Persona Red Flags

**Alex (Impatient Power User)**: Cannot complete "which dependency is erroring" in under 60s from the map — has to abandon it and scan the table. No keyboard shortcuts anywhere. No way to bulk-collapse the 149 externals. Zoom/pan exist but he has to guess. He will use the table permanently and stop opening the map.

**Sam (Accessibility-Dependent)**: The graph is a `<canvas>` with no focusable children — the entire primary visualization is inert to keyboard and screen reader. The dependency table saves this page from being a total failure, but it's behind a collapsed `<details>` and there's no link between a table row and the map. In the map, node type is carried by symbol shape (good) but the error signal is carried by ring color and border-width alone — 3px vs 2.5px is not a perceivable second signal.

**Priya (On-call SRE, 3 AM — project persona)**: Lands on a shared service-map link mid-incident. Sees a gray smear. Her actual question — "which downstream is failing right now" — is answered by the table's Errors column (`api.getbumpa.com 250 (5.58%)`), which is below the fold. The map cost her the first 20 seconds of the incident and answered nothing. The 12px "Showing the 150 busiest services; quieter ones are hidden" note means she also can't trust that the failing dependency is even on screen.

#### Minor Observations

- Cognitive-load checklist: **6 of 8 fail** (single focus, chunking, grouping, visual hierarchy, minimal choices, progressive disclosure) → high load, critical band. Only working-memory and one-thing-at-a-time pass.
- `<details>` for the dependency table defaults closed, so the most useful element on the page is one click away during an incident. Open it by default when the node count exceeds a legibility threshold.
- The node menu has 5 items (Inspect / traces / logs / metrics / monitors) — right at the working-memory boundary and the right five. No change needed, just don't add a sixth.
- `emphasis.focus: 'adjacency'` + `blur.opacity 0.14` is well-tuned but does nothing when the geometry is already collapsed — it'll start paying off the moment P0-1 lands.
- The comment "Hollow, like Datadog" is aspirationally true but Datadog fills the ring with a health arc; a transparent center at 3px spacing means overlapping rings produce moiré rather than depth.
- `resolveColor(n.key, colors)` looks up by node **key** while `getServiceColors` builds the map from node **label**. Harmless today (they coincide for real services; inferred nodes take the neutral branch), but it's a latent mismatch worth aligning on one field.

#### Questions to Consider

- What if the default view were **one service and its immediate neighbors**, and "the whole system" were the thing you opted into — rather than the reverse?
- Does a third-party HTTP endpoint deserve the same visual vocabulary as an instrumented service, or is it a different class of object that should collapse into a group by default?
- If the dependency table already answers the incident question faster than the map, what is the map's actual job — and should it be the top-of-page hero, or the thing you open after the table tells you where to look?
- What would this look like if you designed for the star topology first (which is what most real projects have) and treated deep DAGs as the special case?
