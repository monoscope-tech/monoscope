'use strict';
// Datadog-style service map: deterministic left-to-right layered DAG drawn with
// the echarts `graph` series at `layout:'none'` (we supply every coordinate).
// The layout half is pure and DOM-free so it can be unit tested.
import { getContrastTextColor, resolveColor } from './colorMapping';
import { ensureECharts, subscribeChartTheme, registerChartDisposer, sharedResizeObserver, getChartStyles } from './widgets';

// --- wire types (snake_case: server payload verbatim) ---
export type MapStats = {
  requests: number; errors: number; error_rate: number;
  p50_ns: number; p95_ns: number; p99_ns: number; throughput_per_sec: number;
};
export type NodeKind = 'entry' | 'service' | 'database' | 'queue' | 'external' | 'unknown';
export type ServiceNode = {
  key: string; label: string; kind: NodeKind; inferred: boolean;
  duration_share: number | null; stats: MapStats;
  // Set by the server's collapse passes. Both can be set at once: a domain head that is
  // itself in the long tail is a head *and* a member, which is what makes expansion nest.
  member_count: number | null;  // collapsed head, standing in for this many peers
  group_key: string | null;     // member of that collapsed head
};
export type ServiceEdge = { source: string; target: string; stats: MapStats };
export type ServiceGraph = {
  nodes: ServiceNode[]; edges: ServiceEdge[];
  range_seconds: number; truncated: boolean; error: string | null;
};

// --- pure layout -------------------------------------------------------------
export type Edge = { source: string; target: string };
export type Point = { x: number; y: number };

// One key shape for the two edge sets (back-edges, lit-edges) so they cannot drift apart.
// The separator is escaped, never a literal NUL byte: a NUL makes git treat this whole
// file as binary, which silently costs every diff and review on it.
export const edgeKey = (source: string, target: string): string => `${source}\u0000${target}`;

const X_GAP = 190;
const Y_GAP = 96;
// A layer wider than the canvas wraps into sub-columns rather than growing one infinite
// column; 0.45 keeps the wrapped pair visibly one layer rather than reading as two.
const SUBCOL_GAP = X_GAP * 0.45;

/**
 * The payload carries both levels: collapsed group heads plus the members that would
 * appear if you expanded them. This picks the set actually drawn, so expansion is a
 * re-layout of already-loaded data rather than a fetch.
 *
 * A head disappears when expanded and its members take its place. Heads nest — expanding
 * "225 more dependencies" can reveal a "myshopify.com x71" head that expands again — so the
 * two conditions are independent: a node shows when its parent is open, and hides when it
 * is itself open. Pure, so every expansion permutation is testable without a DOM.
 */
export function visibleGraph<
  N extends { key: string; member_count: number | null; group_key: string | null },
  E extends Edge,
>(
  nodes: N[],
  edges: E[],
  expanded: ReadonlySet<string>,
): { nodes: N[]; edges: E[] } {
  const shown = nodes.filter(n =>
    (n.group_key === null || expanded.has(n.group_key)) && !(n.member_count && expanded.has(n.key)));
  const keys = new Set(shown.map(n => n.key));
  return { nodes: shown, edges: edges.filter(e => keys.has(e.source) && keys.has(e.target)) };
}

/**
 * DFS gray-set cycle break. Returns the acyclic edge subset plus the back-edges
 * removed (drawn later as curved edges, so no information is lost). Node and
 * edge input order fully determines the result.
 */
export function breakCycles(keys: string[], edges: Edge[]): { acyclic: Edge[]; back: Edge[] } {
  const out = new Map<string, Edge[]>(keys.map(k => [k, []]));
  for (const e of edges) out.get(e.source)?.push(e);

  const WHITE = 0, GRAY = 1, BLACK = 2;
  const color = new Map<string, number>(keys.map(k => [k, WHITE]));
  const back = new Set<Edge>();

  const visit = (k: string) => {
    // Explicit stack: real graphs can be deeper than the JS call stack is wide.
    const stack: Array<{ key: string; i: number }> = [{ key: k, i: 0 }];
    color.set(k, GRAY);
    while (stack.length) {
      const top = stack[stack.length - 1];
      const adj = out.get(top.key)!;
      if (top.i >= adj.length) { color.set(top.key, BLACK); stack.pop(); continue; }
      const e = adj[top.i++];
      const c = color.get(e.target);
      if (c === undefined) continue; // edge to a node not in `keys`
      if (c === GRAY) back.add(e);   // includes self-loops
      else if (c === WHITE) { color.set(e.target, GRAY); stack.push({ key: e.target, i: 0 }); }
    }
  };
  for (const k of keys) if (color.get(k) === WHITE) visit(k);

  return { acyclic: edges.filter(e => !back.has(e) && color.has(e.source) && color.has(e.target)), back: [...back] };
}

/** Longest-path layering from in-degree-0 roots: `layer v = 1 + max(layer preds)`. */
export function layerGraph(keys: string[], acyclic: Edge[]): Map<string, number> {
  const preds = new Map<string, string[]>(keys.map(k => [k, []]));
  const succs = new Map<string, string[]>(keys.map(k => [k, []]));
  const indeg = new Map<string, number>(keys.map(k => [k, 0]));
  for (const e of acyclic) {
    if (!preds.has(e.target) || !succs.has(e.source)) continue;
    preds.get(e.target)!.push(e.source);
    succs.get(e.source)!.push(e.target);
    indeg.set(e.target, indeg.get(e.target)! + 1);
  }
  const layer = new Map<string, number>(keys.map(k => [k, 0]));
  const queue = keys.filter(k => indeg.get(k) === 0);
  for (let i = 0; i < queue.length; i++) {
    const k = queue[i];
    for (const s of succs.get(k)!) {
      layer.set(s, Math.max(layer.get(s)!, layer.get(k)! + 1));
      indeg.set(s, indeg.get(s)! - 1);
      if (indeg.get(s) === 0) queue.push(s);
    }
  }
  return layer;
}

/**
 * Barycenter/median ordering, `sweeps` alternating down/up passes. Nodes with no
 * neighbours in the sweep direction hold their position; ties break on node key
 * so two runs over the same input are byte-identical.
 */
export function orderLayers(layer: Map<string, number>, edges: Edge[], sweeps = 4): string[][] {
  const depth = Math.max(0, ...layer.values());
  const layers: string[][] = Array.from({ length: depth + 1 }, () => []);
  for (const k of [...layer.keys()].sort()) layers[layer.get(k)!].push(k);

  const preds = new Map<string, string[]>([...layer.keys()].map(k => [k, [] as string[]]));
  const succs = new Map<string, string[]>([...layer.keys()].map(k => [k, [] as string[]]));
  for (const e of edges) {
    if (!layer.has(e.source) || !layer.has(e.target) || e.source === e.target) continue;
    preds.get(e.target)!.push(e.source);
    succs.get(e.source)!.push(e.target);
  }

  const median = (ks: string[] | undefined, pos: Map<string, number>): number | null => {
    const ps = (ks ?? []).map(k => pos.get(k)).filter((p): p is number => p !== undefined).sort((a, b) => a - b);
    if (!ps.length) return null;
    const m = ps.length >> 1;
    return ps.length % 2 ? ps[m] : (ps[m - 1] + ps[m]) / 2;
  };

  for (let s = 0; s < sweeps; s++) {
    const down = s % 2 === 0;
    const pos = new Map<string, number>();
    layers.forEach(l => l.forEach((k, i) => pos.set(k, i)));
    const idx = layers.map((_, i) => i);
    for (const li of down ? idx : idx.slice().reverse()) {
      const bary = new Map<string, number>();
      layers[li].forEach((k, i) => bary.set(k, median(down ? preds.get(k) : succs.get(k), pos) ?? i));
      layers[li] = layers[li].slice().sort((a, b) => bary.get(a)! - bary.get(b)! || (a < b ? -1 : a > b ? 1 : 0));
      layers[li].forEach((k, i) => pos.set(k, i));
    }
  }
  return layers;
}

export type LayoutOpts = {
  /** Vertical distance between node centres. The renderer floors this at symbol + 8 so a
   *  dense layer can never be squeezed below the point where rings overlap. */
  rowPitch?: number;
  /** Rows a layer may use before it wraps into a second sub-column. */
  maxRows?: number;
};

/**
 * `x = layer * 190` (plus a sub-column offset when the layer wrapped),
 * `y = (row - (rows-1)/2) * pitch`.
 *
 * Wrapping is what keeps a fan-out finite: 149 peers in one layer used to mean a 14,000px
 * virtual column that fit-to-extent then crushed to ~3px per row. Wrapped at the number of
 * rows the canvas can actually show, the same layer stays at full pitch and grows sideways.
 */
export function assignCoords(layers: string[][], opts: LayoutOpts = {}): Map<string, Point> {
  const pitch = opts.rowPitch ?? Y_GAP;
  const maxRows = Math.max(1, opts.maxRows ?? Infinity);
  const coords = new Map<string, Point>();
  layers.forEach((l, li) => {
    const rows = Math.min(l.length, maxRows);
    l.forEach((k, i) => coords.set(k, {
      x: li * X_GAP + Math.floor(i / rows) * SUBCOL_GAP,
      y: ((i % rows) - (rows - 1) / 2) * pitch,
    }));
  });
  return coords;
}

/**
 * Which nodes and edges stay lit. Search dims, never removes, so a path is never
 * silently severed (spec §2).
 *
 * Precedence — the two dimming sources INTERSECT, they do not override each
 * other: a node is lit only if it both matches the query and is inside the
 * click-isolation set. A blank query matches everything, `isolated == null`
 * isolates nothing, so either mechanism alone behaves as if the other is off,
 * and re-running one never resurrects what the other dimmed (no flicker).
 * An edge stays lit when isolation keeps BOTH its endpoints and the query
 * matches AT LEAST ONE of them — i.e. "edges not incident to a match are dimmed".
 */
export function filterSelection(
  nodes: Array<{ key: string; label: string }>,
  edges: Edge[],
  query: string,
  isolated: Set<string> | null = null,
): { litNodes: Set<string>; litEdges: Set<string> } {
  const q = query.trim().toLowerCase();
  const labels = new Map(nodes.map(n => [n.key, n.label.toLowerCase()]));
  const matches = (k: string) => !q || (labels.get(k) ?? '').includes(q);
  const inIso = (k: string) => !isolated || isolated.has(k);
  const litNodes = new Set(nodes.filter(n => matches(n.key) && inIso(n.key)).map(n => n.key));
  const litEdges = new Set(
    edges.filter(e => inIso(e.source) && inIso(e.target) && (matches(e.source) || matches(e.target)))
      .map(e => edgeKey(e.source, e.target)),
  );
  return { litNodes, litEdges };
}

/** breakCycles → layerGraph → orderLayers → assignCoords. */
export function layoutGraph(
  keys: string[],
  edges: Edge[],
  // Given the widest layer's row count, decide pitch and wrapping. A callback rather than a
  // value because only the layering knows how wide the graph got, and running the pipeline
  // twice to find out would be a second copy of it.
  fit: LayoutOpts | ((widestLayer: number) => LayoutOpts) = {},
): { coords: Map<string, Point>; back: Set<string> } {
  const { acyclic, back } = breakCycles(keys, edges);
  const layers = orderLayers(layerGraph(keys, acyclic), acyclic);
  const opts = typeof fit === 'function' ? fit(Math.max(0, ...layers.map(l => l.length))) : fit;
  return { coords: assignCoords(layers, opts), back: new Set(back.map(e => edgeKey(e.source, e.target))) };
}

/**
 * Row pitch and row budget for a canvas `height` px tall holding a widest layer of `rows`
 * nodes whose largest symbol is `maxSymbol` px across.
 *
 * Spacing stays generous while the layer fits, tightens to rings-just-touching when it
 * doesn't, and never goes below that — past the floor the layer wraps instead. That is
 * what turns the old failure mode (149 rows crushed to ~3px apart in a 520px box) into a
 * graph that grows sideways and stays readable.
 */
export function fitRows(height: number, rows: number, maxSymbol: number): { rowPitch: number; maxRows: number } {
  const floor = maxSymbol + 8;
  const usable = Math.max(0, height - 64);
  const roomy = Math.floor(usable / Y_GAP) + 1;
  return rows <= roomy
    ? { rowPitch: Y_GAP, maxRows: roomy }
    : { rowPitch: floor, maxRows: Math.max(1, Math.floor(usable / floor) + 1) };
}

// --- formatting --------------------------------------------------------------
const fmtDur = (ns: number): string => {
  if (!Number.isFinite(ns) || ns <= 0) return '0';
  if (ns < 1e3) return `${Math.round(ns)}ns`;
  if (ns < 1e6) return `${(ns / 1e3).toPrecision(3)}µs`;
  if (ns < 1e9) return `${(ns / 1e6).toPrecision(3)}ms`;
  return `${(ns / 1e9).toPrecision(3)}s`;
};
const fmtNum = (n: number): string =>
  n >= 1e9 ? `${(n / 1e9).toFixed(1)}B` : n >= 1e6 ? `${(n / 1e6).toFixed(1)}M`
    : n >= 1e3 ? `${(n / 1e3).toFixed(1)}k` : `${Math.round(n)}`;
const esc = (s: string) => s.replace(/[<>&]/g, c => ({ '<': '&lt;', '>': '&gt;', '&': '&amp;' }[c]!));

export const statsTip = (title: string, st: MapStats, ring: string, extra = ''): string =>
  // Deliberately the Datadog shape: a coloured name header, then Requests / Latency /
  // Errors. Three numbers is what you can actually read while hovering a dense graph;
  // percentiles belong in the detail view, not here.
  `<div style="margin:-6px -10px 6px;padding:5px 10px;border-radius:3px 3px 0 0;`
  + `background:${ring};color:${ring.startsWith('#') ? getContrastTextColor(ring) : '#fff'};font-weight:700">${esc(title)}</div>${extra}`
  + `<div style="display:flex;gap:16px">`
  + `<div><div style="opacity:.65;font-size:11px">Requests</div>`
  + `<div><b>${st.throughput_per_sec < 10 ? st.throughput_per_sec.toFixed(2) : fmtNum(Math.round(st.throughput_per_sec))}</b>`
  + `<span style="opacity:.65"> req/s</span></div></div>`
  + `<div><div style="opacity:.65;font-size:11px">Latency</div>`
  + `<div><b>${fmtDur(st.p95_ns)}</b><span style="opacity:.65"> p95</span></div></div>`
  + `<div><div style="opacity:.65;font-size:11px">Errors</div>`
  + `<div><b>${(st.error_rate * 100).toFixed(2)}</b><span style="opacity:.65"> %</span></div></div>`
  + `</div>`;

// Health thresholds. "error_rate > 0" is not a health signal on an aggregate view: across
// hundreds of third-party integrations essentially everything has had one error in an hour,
// so flagging that paints the whole map red and hides the dependency that is actually down.
// Three steps, the way Datadog grades a service.
export const ERR_ELEVATED = 0.01;
export const ERR_FAILING = 0.05;
export const healthColor = (rate: number, st: ReturnType<typeof getChartStyles>, ok: string): string =>
  rate >= ERR_FAILING ? st.errorColor : rate >= ERR_ELEVATED ? st.warningColor : ok;

// The ring is the node's whole identity: red when it is actually failing, the service's
// own colour otherwise (neutral for inferred peers, which have no service colour).
// Health first — a failing dependency must not be hidden behind a pretty hash colour.
const ringColor = (
  n: { key: string; inferred: boolean; stats: MapStats },
  st: ReturnType<typeof getChartStyles>,
  colors: Record<string, string> | undefined,
): string =>
  healthColor(n.stats.error_rate, st, n.inferred ? st.tooltipBorderColor : resolveColor(n.key, colors ?? {}));

// --- rendering ---------------------------------------------------------------
const KIND_SYMBOL: Record<NodeKind, string> = {
  entry: 'pin', service: 'circle', database: 'diamond',
  queue: 'roundRect', external: 'triangle', unknown: 'circle',
};

// layout:'none' fits the supplied coordinate extent to the box, so a 1-2 node
// graph would be blown up to fill the canvas. Invisible corner nodes pin a
// minimum virtual extent (3 layers × 4 rows).
const MIN_W = 2 * X_GAP;
const MIN_H = 3 * Y_GAP;

type Reach = { up: Map<string, string[]>; down: Map<string, string[]> };

/** Live controls for a rendered map. `serviceMapChart` stays `void` because
 *  rendering waits on the lazy echarts load, so the handle only exists later. */
export type ServiceMapHandle = {
  id: string;
  filter: (query: string) => void;
  clearIsolation: () => void;
  dispose: () => void;
};

export const FILTER_EVENT = 'service-map-filter';
const handles = new Map<string, ServiceMapHandle>();
export const getServiceMapHandle = (containerId: string): ServiceMapHandle | undefined => handles.get(containerId);

type ServiceMapRenderer = typeof serviceMapChart;

export function hydrateServiceMaps(root: ParentNode = document, renderMap: ServiceMapRenderer = serviceMapChart): void {
  for (const el of root.querySelectorAll<HTMLElement>('[data-service-map]')) {
    if (el.dataset.serviceMapHydrated) continue;
    const graph = document.getElementById(`${el.id}-data`);
    const colors = document.getElementById(`${el.id}-colors`);
    if (!graph || !colors) continue;
    el.dataset.serviceMapHydrated = 'true';
    renderMap(el.id, JSON.parse(graph.textContent ?? ''), {
      colors: JSON.parse(colors.textContent ?? ''),
    });
  }
}

const reachableFrom = (start: string, adj: Map<string, string[]>): Set<string> => {
  const seen = new Set([start]);
  const stack = [start];
  while (stack.length) for (const n of adj.get(stack.pop()!) ?? []) if (!seen.has(n)) { seen.add(n); stack.push(n); }
  return seen;
};

export function serviceMapChart(
  containerId: string,
  graph: ServiceGraph,
  opts: { colors?: Record<string, string>; onNodeClick?: (key: string) => void } = {},
): void {
  const el = document.getElementById(containerId);
  if (!el) return;
  // Error / empty states are rendered server-side in Lucid; just leave the
  // container alone rather than painting a blank canvas over the message.
  if (graph.error || !graph.nodes?.length) { registerChartDisposer(containerId, () => {}); return; }
  void ensureECharts().then(() => render(el, containerId, graph, opts));
}

function render(
  el: HTMLElement,
  containerId: string,
  graph: ServiceGraph,
  opts: { colors?: Record<string, string>; onNodeClick?: (key: string) => void },
): void {
  const echarts = (window as any).echarts;
  registerChartDisposer(containerId, () => {}); // tear down whatever was rendered here before
  const controller = new AbortController();
  const signal = controller.signal;

  // A hidden/zero-size container renders a blank canvas. Wait until it has a box.
  if (!el.clientWidth || !el.clientHeight) {
    const retry = () => { ro.disconnect(); serviceMapChart(containerId, graph, opts); };
    const ro = new ResizeObserver(() => { if (el.clientWidth && el.clientHeight) retry(); });
    ro.observe(el);
    el.closest('.a-tab-content')?.addEventListener('tab-visible', () => requestAnimationFrame(retry), { signal });
    registerChartDisposer(containerId, () => { ro.disconnect(); controller.abort(); });
    return;
  }

  const colors = opts.colors ?? {};
  const allNodes = graph.nodes;
  const byKey = new Map(allNodes.map(n => [n.key, n]));
  const members = new Map<string, ServiceNode[]>();
  for (const n of allNodes) if (n.group_key) members.set(n.group_key, [...(members.get(n.group_key) ?? []), n]);

  echarts.getInstanceByDom(el)?.dispose();
  const chart = echarts.init(el);

  const expanded = new Set<string>();
  let isolated: Set<string> | null = null;
  let query = '';

  // Everything geometry-dependent is derived, because expanding a group changes which
  // nodes exist and therefore the whole layout. `paint` only re-styles; `derive` re-places.
  const derive = () => {
    const s = getChartStyles();
    const { nodes, edges: candidates } = visibleGraph(allNodes, graph.edges, expanded);
    const keys = nodes.map(n => n.key);
    const index = new Map(keys.map((k, i) => [k, i]));
    const edges = candidates.filter(e => index.has(e.source) && index.has(e.target));

    const maxReq = Math.max(1, ...nodes.map(n => n.stats.requests));
    const share = (n: ServiceNode) => n.duration_share ?? n.stats.requests / maxReq;
    const sizeOf = (n: ServiceNode) => Math.min(64, 26 + 24 * Math.sqrt(Math.max(0, Math.min(1, share(n)))));
    const maxSymbol = Math.max(26, ...nodes.map(sizeOf));

    const box = el.getBoundingClientRect();
    const { coords, back } = layoutGraph(keys, edges, widest => fitRows(box.height, widest, maxSymbol));

    // Transitive isolation sets — echarts' focus:'adjacency' is 1-hop only.
    const reach: Reach = { up: new Map(), down: new Map() };
    for (const k of keys) { reach.up.set(k, []); reach.down.set(k, []); }
    for (const e of edges) { reach.down.get(e.source)!.push(e.target); reach.up.get(e.target)!.push(e.source); }

    const xs = [...coords.values()].map(p => p.x), ys = [...coords.values()].map(p => p.y);
    const cx = (Math.min(...xs) + Math.max(...xs)) / 2, cy = (Math.min(...ys) + Math.max(...ys)) / 2;
    // Only a floor, never the container width: padding the virtual extent out to the box
    // is what parked a small graph in the left quarter of an otherwise empty canvas.
    const halfW = Math.max(MIN_W, Math.max(...xs) - Math.min(...xs)) / 2;
    const halfH = Math.max(MIN_H, Math.max(...ys) - Math.min(...ys)) / 2;
    const pads = [{ x: cx - halfW, y: cy - halfH }, { x: cx + halfW, y: cy + halfH }]
      .map((p, i) => ({ ...p, id: `__pad${i}`, name: `__pad${i}`, fixed: true, symbol: 'none', symbolSize: 0, silent: true, label: { show: false }, tooltip: { show: false } }));

    const nodeData = nodes.map((n, i) => ({
      // NB: not `label` — echarts owns data[i].label and rewrites it into a label config
      // object, which is why the formatter rendered "[object Object]".
      id: String(i), name: n.key || '(entry)', key: n.key, displayName: n.label, kind: n.kind,
      group: n.member_count ?? 0,
      value: n.stats.requests, tip: statsTip(n.label || 'entry', n.stats, ringColor(n, s, colors), groupTip(n)),
      x: coords.get(n.key)!.x, y: coords.get(n.key)!.y, fixed: true,
      symbol: KIND_SYMBOL[n.kind] ?? 'circle', symbolSize: sizeOf(n),
    }));

    const linkData = edges.map(e => ({
      source: String(index.get(e.source)), target: String(index.get(e.target)),
      sourceKey: e.source, targetKey: e.target,
      value: e.stats.requests, back: back.has(edgeKey(e.source, e.target)),
      inferred: byKey.get(e.target)!.inferred, errorRate: e.stats.error_rate,
      tip: statsTip(`${byKey.get(e.source)!.label || 'entry'} → ${byKey.get(e.target)!.label}`, e.stats,
        healthColor(e.stats.error_rate, s, resolveColor(e.source, colors))),
    }));
    // Failing edges paint last so they sit on top of the healthy bundle instead of under it.
    linkData.sort((a, b) => a.errorRate - b.errorRate);

    return { nodes, edges, nodeData, linkData, pads, reach };
  };

  // A collapsed group must never hide a failing member behind a healthy-looking average:
  // one endpoint at 100% inside 42 healthy ones is ~2% aggregate, which is exactly the
  // failure the map exists to surface. Say how many are erroring, in words.
  function groupTip(n: ServiceNode): string {
    if (!n.member_count) return n.duration_share != null ? `<div>${(n.duration_share * 100).toFixed(1)}% of trace time</div>` : '';
    const bad = (members.get(n.key) ?? []).filter(m => m.stats.error_rate >= ERR_ELEVATED).length;
    return `<div style="opacity:.65">${n.member_count} endpoints${bad ? ` · ${bad} returning errors` : ''}</div>`;
  }

  let view = derive();

  const paint = () => {
    const s = getChartStyles();
    // One selection pass drives both dimming sources, so search and click-isolation
    // can never disagree about a node's opacity.
    const lit = filterSelection(view.nodes, view.edges, query, isolated);
    const dim = (k: string) => (lit.litNodes.has(k) ? 1 : 0.12);
    const dense = view.edges.length > 60;
    chart.setOption({
      animation: false,
      backgroundColor: 'transparent',
      tooltip: {
        trigger: 'item', appendToBody: true, backgroundColor: s.tooltipBg,
        borderColor: s.tooltipBorderColor, borderWidth: 1, textStyle: { color: s.tooltipTextColor, fontSize: 11 },
        formatter: (p: any) => p.data?.tip ?? '',
      },
      series: [{
        type: 'graph', layout: 'none', left: 40, right: 40, top: 32, bottom: 32,
        roam: true, scaleLimit: { min: 0.35, max: 4 }, draggable: true, cursor: 'pointer',
        // Arrowheads are noise once the bundle is dense; direction still reads left-to-right
        // from the layering, and hovering an edge brings its own emphasis.
        edgeSymbol: dense ? ['none', 'none'] : ['none', 'arrow'], edgeSymbolSize: [0, 7],
        // autoCurveness spreads edges that share a *pair*; in a fan every edge has a
        // different target, so it only bent 200 straight lines into sweeping arcs that
        // crossed the whole canvas. A small fixed curveness is what the fan actually wants.
        autoCurveness: false,
        label: {
          show: true, position: 'right', distance: 7, fontSize: 11, color: s.textColor,
          formatter: (p: any) => (p.data?.group ? `${p.data.displayName}  ×${p.data.group}` : p.data?.displayName ?? ''),
        },
        // Labels that collide are dropped rather than overprinted — 150 right-hand labels
        // stacked into a grey mass is unreadable in a way that "some labels hidden" is not.
        labelLayout: { hideOverlap: true },
        edgeLabel: { show: false, fontSize: 10, color: s.textColor, formatter: (p: any) => p.data?.tip ?? '' },
        lineStyle: { color: s.textColor, width: 1.5, opacity: 0.3, curveness: 0.06 },
        emphasis: { focus: 'adjacency', scale: 1.08, lineStyle: { width: 3, opacity: 1 }, label: { fontWeight: 'bold' } },
        blur: { itemStyle: { opacity: 0.14 }, lineStyle: { opacity: 0.06 }, label: { opacity: 0.14 } },
        data: [
          ...view.nodeData.map(n => {
            const st = byKey.get(n.key)!;
            return {
              ...n,
              itemStyle: {
                // Hollow, like Datadog: the ring carries health and the hole keeps a dense
                // graph readable, so size (requests) stays the only filled signal.
                color: 'transparent',
                opacity: dim(n.key),
                borderType: st.inferred ? 'dashed' : 'solid',
                borderColor: ringColor(st, s, colors),
                borderWidth: st.stats.error_rate >= ERR_ELEVATED ? 3 : 2.5,
                // A collapsed group is drawn as a stacked ring: "more than one" read
                // pre-attentively from shape, without spending a colour on it.
                ...(n.group ? { shadowColor: ringColor(st, s, colors), shadowBlur: 0, shadowOffsetX: 3, shadowOffsetY: 3 } : {}),
              },
              label: { opacity: dim(n.key) },
            };
          }),
          ...view.pads,
        ],
        links: view.linkData.map(l => ({
          ...l,
          lineStyle: {
            // Capped at 3: at close row pitch a 6px edge is wider than the gap between the
            // rows it runs between, which is how 150 edges fused into one object.
            width: Math.max(1, Math.min(3, 1 + Math.log2(1 + l.value) / 2)),
            // Neutral until it matters. Colouring by source is meaningless in a fan-out —
            // every edge shares one source — and it drowns the red that does mean something.
            color: healthColor(l.errorRate, s, s.textColor),
            type: l.inferred ? 'dashed' : 'solid',
            curveness: l.back ? 0.2 : undefined,
            opacity: lit.litEdges.has(edgeKey(l.sourceKey, l.targetKey)) ? (l.errorRate >= ERR_ELEVATED ? 0.85 : 0.3) : 0.06,
          },
        })),
      }],
    }, { notMerge: true, lazyUpdate: false });
  };
  paint();

  // Expanding a group replaces it with its members, so the geometry is rebuilt, not restyled.
  const rebuild = () => { view = derive(); paint(); };

  // The menu markup is rendered once by Lucid inside the container; we only position it and
  // point its links at this node. Building menu HTML here would put a second renderer in JS.
  const menu = el.querySelector<HTMLElement>('[data-service-menu]');
  const pid = el.dataset.mapBase ?? '';
  const hideMenu = () => menu?.classList.add('hidden');

  const showMenu = (key: string, label: string, inferred: boolean, x: number, y: number) => {
    if (!menu) return;
    const title = menu.querySelector<HTMLElement>('[data-menu-title]');
    if (title) title.textContent = label;
    // An inferred peer has no telemetry of its own, so the only honest action is Inspect.
    const q = encodeURIComponent(`resource.service.name=="${key}"`);
    for (const a of menu.querySelectorAll<HTMLAnchorElement>('[data-menu-action]')) {
      const action = a.dataset.menuAction!;
      const hide = inferred && action !== 'inspect';
      a.classList.toggle('hidden', hide);
      a.href =
        action === 'traces' ? `${pid}/log_explorer?query=${q}&viz_type=traces`
        : action === 'logs' ? `${pid}/log_explorer?query=${q}`
        : action === 'metrics' ? `${pid}/metrics?metric_source=${encodeURIComponent(key)}`
        : action === 'monitors' ? `${pid}/monitors`
        : '#';
    }
    menu.style.left = `${x}px`;
    menu.style.top = `${y}px`;
    menu.classList.remove('hidden');
  };

  menu?.addEventListener('click', e => {
    const a = (e.target as HTMLElement).closest<HTMLAnchorElement>('[data-menu-action]');
    if (a?.dataset.menuAction === 'inspect') e.preventDefault();
    hideMenu();
  });

  chart.on('click', (p: any) => {
    if (p.dataType !== 'node' || p.data?.key === undefined) return;
    const key = p.data.key as string;
    // A collapsed group's only honest action is "show me what is in it": the per-service
    // menu items cannot be scoped to a set of endpoints.
    if (p.data.group) { expanded.add(key); isolated = null; hideMenu(); rebuild(); return; }
    isolated = new Set([...reachableFrom(key, view.reach.up), ...reachableFrom(key, view.reach.down)]);
    paint();
    const box = el.getBoundingClientRect();
    showMenu(key, p.data.displayName || 'entry', !!byKey.get(key)?.inferred,
      p.event.event.clientX - box.left, p.event.event.clientY - box.top);
    opts.onNodeClick?.(key);
  });
  // Clicking the background, or Escape, is the one gesture that returns the map to its
  // default view: focus cleared and every expanded group collapsed again.
  const resetView = () => {
    hideMenu();
    const wasExpanded = expanded.size > 0;
    expanded.clear();
    isolated = null;
    if (wasExpanded) rebuild(); else paint();
  };
  chart.getZr().on('click', (e: any) => { if (!e.target) resetView(); });
  document.addEventListener('keydown', e => {
    if (e.key === 'Escape' && (isolated || expanded.size)) resetView();
  }, { signal });

  const unsubscribeTheme = subscribeChartTheme(() => paint());
  // The row budget is a function of the box height, so a resize re-fits, not just rescales.
  const resize = () => { if (!chart.isDisposed()) { chart.resize(); rebuild(); } };
  ['resize', 'toggle-sidebar', 'loglist-resize'].forEach(ev => window.addEventListener(ev, resize, { signal }));
  sharedResizeObserver.observe(el);

  const handle: ServiceMapHandle = {
    id: containerId,
    filter: (q: string) => { query = q ?? ''; paint(); },
    clearIsolation: resetView,
    dispose: () => registerChartDisposer(containerId, () => {}),
  };
  handles.set(containerId, handle);
  (el as any).__serviceMap = handle;

  // The search input is a sibling-descendant, not an ancestor, so the bubbling
  // `service-map-filter` event never reaches the map container — listen on
  // document instead. `detail.id`, when present, addresses one specific map.
  document.addEventListener(FILTER_EVENT, (ev: Event) => {
    const d = (ev as CustomEvent<{ q?: string; id?: string }>).detail ?? {};
    if (d.id && d.id !== containerId) return;
    handle.filter(d.q ?? '');
  }, { signal });

  registerChartDisposer(containerId, () => {
    controller.abort();
    unsubscribeTheme();
    sharedResizeObserver.unobserve(el);
    handles.delete(containerId);
    delete (el as any).__serviceMap;
    if (!chart.isDisposed()) chart.dispose();
  });
}

(window as any).serviceMapChart = serviceMapChart;
(window as any).getServiceMapHandle = getServiceMapHandle;
