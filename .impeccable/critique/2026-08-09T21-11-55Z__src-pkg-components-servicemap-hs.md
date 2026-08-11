---
target: service map page (post-grouping)
total_score: 19
p0_count: 2
p1_count: 3
timestamp: 2026-08-09T21-11-55Z
slug: src-pkg-components-servicemap-hs
---
⚠️ DEGRADED: single-context (session policy forbids unrequested Agent/sub-agent calls; Assessments A and B ran sequentially in this context)

Target: the service map after commit `b60e737eb`, against Datadog's Request Flow Map as the reference.
Evidence: your two screenshots, the current source, `detect.mjs` over the server-rendered demo page. Browser automation still unavailable (no debuggable Chrome; the project route 302s to auth) — no live overlay for this run.

#### Design Health Score

| # | Heuristic | Score | Key Issue |
|---|-----------|-------|-----------|
| 1 | Visibility of System Status | 1 | **Regressed.** "234 services · 234 dependencies" now counts collapsed members that are never drawn. The header, the truncation banner, and the table all miscount. |
| 2 | Match System / Real World | 2 | 233 merchant domains are drawn with the same weight as your one real service. |
| 3 | User Control and Freedom | 2 | Esc and expand-on-click landed; still no scope control, no fit/reset, no way to say "show me less". |
| 4 | Consistency and Standards | 3 | Tokens, icons, controls remain coherent. |
| 5 | Error Prevention | 2 | The illegible state is still reachable by default on a real project. |
| 6 | Recognition Rather Than Recall | 2 | `hideOverlap` is set and labels still collide — `abayalagos.com`/`api-topship.com`/`api.paystack.co` overprint into one string. |
| 7 | Flexibility and Efficiency | 1 | No keyboard path, no scope/depth control, no bulk collapse. |
| 8 | Aesthetic and Minimalist Design | 2 | Improved: nodes are discrete and legibly spaced. Still a rash of ~200 near-identical red dashed triangles. |
| 9 | Error Recovery | 2 | Unchanged. |
| 10 | Help and Documentation | 2 | Unchanged. |
| **Total** | | **19/40** | **Poor — no net change. The mechanism is fixed; the outcome is not.** |

#### Anti-Patterns Verdict

**LLM assessment**: Still not AI slop, and the fixes did what they claimed — grouping is visibly working (`myshopify.com ×71`, `googleapis.com ×2`, `geeatg.com ×3`), the row-pitch floor holds, and nothing is crushed. But I fixed the symptom I could measure instead of the disease, and the screenshot says so plainly. **The score did not move.**

**Deterministic scan**: one warning, `flat-type-hierarchy` — same false positive as last run (the product register prescribes this tight scale).

**Visual overlays**: not available; fallback signal is your screenshot plus source.

#### Overall Impression

My last diagnosis was wrong in an important way. I assumed the 149 peers were per-tenant subdomains of a few brands, so grouping by registrable domain would collapse them. Your data is the opposite: `chicdrip.com`, `deefurse.com`, `hingees.com`, `karposshoes.com` — **hundreds of distinct merchant domains**, one per Shipbubble customer. eTLD+1 grouping cannot collapse them because there is nothing shared to collapse on. It caught the one genuine case (`myshopify.com ×71`) and left ~160 singletons behind.

The Datadog screenshot is the tell, and it isn't about rendering. Look at the header: **"Showing 32 services from distributed traces matching this query"**, with `Env:prod` and `Service:product-recommendation` as active filter chips and a facet sidebar down the left. Datadog never draws your universe. It draws a *query*. Our v1 spec made "no arbitrary filtering of the global map" an explicit non-goal because the rollup can't answer arbitrary predicates — and that decision is precisely what forces this page to render all 234.

The second tell: their node is a **card carrying four facts** (name, error %, latency, req/s, plus a health bar). Ours is a hollow ring carrying one (name), with the numbers hidden in a tooltip. Our own PRODUCT.md says "density is respect." Theirs is dense; ours is sparse and therefore requires hovering 234 times to learn anything.

#### What's Working

- **The layout floor holds.** Nothing is crushed; rows are legibly spaced and wrapping into sub-columns behaved exactly as designed. That machinery is sound and should stay.
- **Grouping works where grouping applies.** `myshopify.com ×71` is one node instead of 71, and the count badge reads correctly at a glance.
- **The neutral-edge change is right in principle** — the red edges now stand out from the grey ones, which is more than the previous single-purple bundle managed.

#### Priority Issues

**[P0] The map draws the universe; Datadog draws a query**
234 nodes is not a rendering problem, it's a scoping problem, and no amount of layout work fixes it. The honest structure for a star topology with a long tail: rank dependencies by volume, draw the top ~10 named, and fold everything below into **one** `225 more dependencies ▸` node that expands or hands off to the table. Your top 5 carry the overwhelming majority of traffic; the other 225 are each <0.1% and belong in a sorted list, not on a canvas.
*Why it matters*: this is the difference between the two screenshots. Datadog shows 32 because it asked a question first.
*Fix*: (a) long-tail folding by volume with an explicit "N more" node — this works regardless of whether domains share a suffix, which is where eTLD+1 grouping failed; (b) the hop-scope default I deferred last round, promoted to P0; (c) revisit the "no filtering on the global map" non-goal — even a service picker and an env filter would let this page ask a question. The rollup can answer *those* two predicates if they're rollup dimensions.
*Suggested command*: `/impeccable shape` (this is a scoping model, not a styling pass)

**[P1] Everything is red, so nothing is**
`ringColor` flags a node when `error_rate > 0` — literally one error in an hour. Across 234 third-party integrations essentially everything qualifies, which is why the canvas is a field of red triangles and why a genuinely broken dependency is now invisible. This is worse than the old single-purple bundle: it looks like signal and isn't.
*Fix*: a three-step health scale on a meaningful threshold (healthy / elevated ≥1% / failing ≥5%, tuned to your data), rendered the way Datadog does it — a small health bar on the node, not the node's entire identity. `> 0` should never drive colour on an aggregate view.
*Suggested command*: `/impeccable colorize`

**[P2 → P0 for trust] I introduced a counting regression**
The header reads "234 services · 234 dependencies", the banner "Showing the 234 busiest services", and the table "Dependencies (234)". All three count `graph.nodes` / `graph.edges`, which now include **collapsed members that are never drawn** and **both levels of every edge**. So: services are over-counted by the number of hidden members, dependencies are double-counted for every grouped peer, and `dependencyTable_` will render both the `→ myshopify.com` aggregate row *and* all 71 member rows, silently double-counting requests in a table people read during incidents.
*Fix*: count drawn top-level nodes and collapsed edges only; keep member rows in the table nested under their group row rather than as siblings. This one is mine and it's the most damaging of the three, because a wrong number read at 3 AM is worse than an ugly picture.
*Suggested command*: `/impeccable harden`

**[P1] The node carries one fact where it should carry four**
Datadog's card: name, `47.9% errors`, `44.3 ms latency`, `6.23 req/s`, health bar. Ours: a ring and a name, everything else behind a hover. On a map whose job is "which dependency is failing", making the numbers hover-only means the map can't answer its own question without 234 interactions.
*Fix*: replace the hollow ring with a compact card once node count is bounded (which P0 makes possible) — name, error %, p95, req/s, and a health bar. Fewer nodes is what buys the room for this; the two fixes are the same fix.
*Suggested command*: `/impeccable layout`

**[P1] `autoCurveness` made the fan worse, and `hideOverlap` isn't taking effect**
I enabled `autoCurveness` to spread parallel edges — but it spreads edges between the *same pair*, and here every edge goes to a different target. The result is 234 long sweeping arcs crossing the entire canvas. And despite `labelLayout: { hideOverlap: true }`, labels are clearly overprinting in your screenshot; echarts isn't applying it to this series as configured.
*Fix*: `autoCurveness: false` with a small fixed curveness, and verify `hideOverlap` in the browser rather than trusting the option — if the graph series ignores it, gate labels on node importance and hover instead.
*Suggested command*: `/impeccable polish`

#### Persona Red Flags

**Priya (On-call SRE, 3 AM)**: Opens the map. Everything is red, so she learns nothing from colour. The numbers she needs are hover-only across 234 nodes. She reads "234 dependencies", which is not true. She opens the table and finds `myshopify.com` counted once as a group and 71 more times as members. Every one of these pushes her back to the log explorer.

**Alex (Power User)**: Grouping gave him one win (`×71`) and 160 singletons. He has no way to say "just show me the top 10", no scope control, no keyboard path. He wants the filter chips from the Datadog screenshot and there is nowhere to put a query.

**Sam (Accessibility-Dependent)**: Unchanged and still the weakest surface — canvas with no focusable children. The dependency table remains the only path, and it now contains duplicated rows.

#### Minor Observations

- The legend still renders "Service colors ● production-shipbubble-api" — one swatch, which reads as a bug. Flagged as P2 last round, still true.
- Wireframe triangles at ~26px don't read as "external" at this density; Datadog puts a small icon *inside* a card, which survives scale in a way an outline glyph doesn't.
- `127.0.0.1` and `redis` are sitting on the map as peers. Loopback is not a dependency worth a node.
- The truncation banner says "quieter ones are hidden" while displaying 234 of them — the copy and the cap no longer describe the same thing.
- Positive: the `Dependencies` table now defaults collapsed, which is the wrong default during an incident but at least consistent.

#### Questions to Consider

- What question should this page answer *before* it draws anything? Datadog answers "how does this service's traffic flow, in this env, right now." Ours currently answers "what exists," which nobody asks.
- If 5 dependencies carry 99% of traffic, what is the other 229's job on a canvas — versus in a sorted table one scroll below?
- Is the global unfiltered map a product at all, or is the real product a *service-scoped* map you reach by clicking a service?
