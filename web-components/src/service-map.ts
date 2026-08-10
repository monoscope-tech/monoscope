'use strict';
// Datadog-style service map: deterministic left-to-right layered DAG drawn with
// the echarts `graph` series at `layout:'none'` (we supply every coordinate).
// The layout half is pure and DOM-free so it can be unit tested.
import { resolveColor } from './colorMapping';
import { subscribeChartTheme, registerChartDisposer, getChartStyles } from './widgets';
import ELKConstructor from 'elkjs/lib/elk.bundled.js';

// One instance, created on first use. ELK is ~1.5 MB and only this chunk imports it, so it
// travels with the map rather than with the app.
let elkInstance: InstanceType<typeof ELKConstructor> | null = null;
const elk = () => (elkInstance ??= new ELKConstructor());

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

// Card geometry, matched to Datadog's: a compact box whose height is set by four lines of
// 11px text plus padding, with a full-height health bar down its right edge.
export const CARD_W = 150;
export const CARD_H = 62;
// Vertical room between one hop and the next, and horizontal room between siblings.
export const LAYER_GAP = 78;
export const NODE_GAP = 26;

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
 * Everything reachable from `key` in either direction, plus `key` itself: the connected
 * slice of the system that one service actually participates in.
 *
 * This is what a service scope means on a map served from a rollup. We cannot answer an
 * arbitrary predicate the way Datadog's query bar does — the rollup has no env or tag
 * dimension — but "this service and what it touches" is pure graph traversal, needs no
 * schema, and is the scope people reach for during an incident anyway.
 */
export function scopeTo<N extends { key: string }, E extends Edge>(
  nodes: N[],
  edges: E[],
  key: string | null,
): { nodes: N[]; edges: E[] } {
  if (!key || !nodes.some(n => n.key === key)) return { nodes, edges };
  const up = new Map<string, string[]>(), down = new Map<string, string[]>();
  for (const e of edges) {
    down.set(e.source, [...(down.get(e.source) ?? []), e.target]);
    up.set(e.target, [...(up.get(e.target) ?? []), e.source]);
  }
  const walk = (adj: Map<string, string[]>) => {
    const seen = new Set([key]), stack = [key];
    while (stack.length) for (const n of adj.get(stack.pop()!) ?? []) if (!seen.has(n)) { seen.add(n); stack.push(n); }
    return seen;
  };
  const keep = new Set([...walk(up), ...walk(down)]);
  return { nodes: nodes.filter(n => keep.has(n.key)), edges: edges.filter(e => keep.has(e.source) && keep.has(e.target)) };
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

export type LayoutOpts = {
  /** Distance between one hop and the next, down the page. */
  layerGap?: number;
  /** Distance between siblings across a row. */
  nodeGap?: number;
  /** Card footprint ELK reserves per node. */
  cardW?: number;
  cardH?: number;
};

/**
 * Node placement, delegated to ELK's `layered` algorithm.
 *
 * This used to be four hand-written passes — longest-path layering, barycenter crossing
 * reduction, coordinate assignment and row wrapping. They worked, but every one of them was
 * a graph-layout library we were maintaining by hand, and each shipped a bug: a wrapped row
 * placed into the next hop's lane, a sub-column narrower than the card it was making room
 * for, and a pixel floor silently undone by fit-to-extent. ELK is node-size aware, so those
 * classes of bug are simply not expressible here.
 *
 * Determinism is load-bearing — a shared map link must not reshuffle between refreshes —
 * so model order is fed to ELK as a tie-break and the input is sorted before it is handed
 * over. `breakCycles` still runs: ELK breaks cycles internally to layer them, but it does
 * not tell us which edges it reversed, and we draw back-edges curved.
 */
export async function layoutGraph(
  keys: string[],
  edges: Edge[],
  opts: LayoutOpts = {},
): Promise<{ coords: Map<string, Point>; back: Set<string> }> {
  const { back } = breakCycles(keys, edges);
  const backKeys = new Set(back.map(e => edgeKey(e.source, e.target)));
  if (!keys.length) return { coords: new Map(), back: backKeys };

  const present = new Set(keys);
  const laid = await elk().layout({
    id: 'root',
    layoutOptions: {
      'elk.algorithm': 'layered',
      'elk.direction': 'DOWN',
      'elk.layered.spacing.nodeNodeBetweenLayers': String(opts.layerGap ?? LAYER_GAP),
      'elk.spacing.nodeNode': String(opts.nodeGap ?? NODE_GAP),
      // Routing channels are what make a layered graph sprawl: every edge crossing a layer
      // reserves its own lane. Kept tight, because the map is read by scanning rows, not by
      // following individual wires across the page.
      'elk.layered.spacing.edgeNodeBetweenLayers': '2',
      'elk.spacing.edgeNode': '2',
      'elk.spacing.edgeEdge': '2',
      // No wrapping. ELK can fold a wide graph into bands, but a folded band puts a later
      // hop *above* an earlier one, which is exactly the reading order top-down flow exists
      // to give. A wide map scrolls sideways instead — the same trade Datadog's makes.
      // ELK is deterministic for identical input on its own; model order is not needed for
      // that, and forcing it produces longer edges, which cost a routing lane each.
      'elk.layered.cycleBreaking.strategy': 'DEPTH_FIRST',
      // BRANDES_KOEPF aligns children under parents beautifully and pays for it in width —
      // ~3000px for 31 nodes, most of it whitespace holding alignments the reader never sees.
      // SIMPLE packs rows instead, which is what a map you scan rather than trace wants.
      'elk.layered.nodePlacement.strategy': 'SIMPLE',
      // No routing. ECharts' graph series draws straight or curved edges and cannot consume
      // bend points, so reserving a lane per crossing edge buys nothing and costs the map
      // roughly twice its width — we would pay for routes we then throw away.
      'elk.edgeRouting': 'UNDEFINED',
    },
    children: keys.map(id => ({ id, width: opts.cardW ?? CARD_W, height: opts.cardH ?? CARD_H })),
    edges: edges
      .filter(e => present.has(e.source) && present.has(e.target) && e.source !== e.target)
      .map((e, i) => ({ id: `e${i}`, sources: [e.source], targets: [e.target] })),
  });

  // ELK reports a node's top-left; echarts positions a symbol by its centre.
  const coords = new Map<string, Point>();
  const w = opts.cardW ?? CARD_W, h = opts.cardH ?? CARD_H;
  for (const n of laid.children ?? []) coords.set(n.id, { x: (n.x ?? 0) + w / 2, y: (n.y ?? 0) + h / 2 });
  // A node ELK dropped (it never should) still needs a coordinate rather than a crash.
  for (const k of keys) if (!coords.has(k)) coords.set(k, { x: 0, y: 0 });
  return { coords, back: backKeys };
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

// Health thresholds. "error_rate > 0" is not a health signal on an aggregate view: across
// hundreds of third-party integrations essentially everything has had one error in an hour,
// so flagging that paints the whole map red and hides the dependency that is actually down.
// Three steps, the way Datadog grades a service.
export const ERR_ELEVATED = 0.01;
export const ERR_FAILING = 0.05;
export const healthColor = (rate: number, st: ReturnType<typeof getChartStyles>, ok: string): string =>
  rate >= ERR_FAILING ? st.errorColor : rate >= ERR_ELEVATED ? st.warningColor : ok;

// --- rendering ---------------------------------------------------------------
// A React-Flow-shaped renderer with no framework: a pane the wheel and drag transform, an
// SVG layer of bezier edges beneath, and the nodes as real DOM cards cloned from a Lucid
// template. Nothing is painted to a bitmap, so cards carry design tokens, sprite icons and
// focus rings, and the tests can assert on the DOM instead of a chart option object.

// Kind marks, as inline SVG so they scale with the card and need no sprite fetch per node.
const KIND_ICON: Record<NodeKind, string> = {
  service: '<svg viewBox="0 0 16 16" width="11" height="11" fill="none" stroke="currentColor" stroke-width="1.5"><path d="M8 1.6 13.6 4.8v6.4L8 14.4 2.4 11.2V4.8z"/></svg>',
  database: '<svg viewBox="0 0 16 16" width="11" height="11" fill="none" stroke="currentColor" stroke-width="1.5"><ellipse cx="8" cy="4" rx="5" ry="2"/><path d="M3 4v8c0 1.1 2.2 2 5 2s5-.9 5-2V4"/></svg>',
  queue: '<svg viewBox="0 0 16 16" width="11" height="11" fill="none" stroke="currentColor" stroke-width="1.5"><path d="M8 2 14 5 8 8 2 5z"/><path d="M2 8.5 8 11.5 14 8.5"/></svg>',
  external: '<svg viewBox="0 0 16 16" width="11" height="11" fill="none" stroke="currentColor" stroke-width="1.5"><path d="M4.5 12h7a2.8 2.8 0 0 0 .3-5.6A4 4 0 0 0 4.2 7 2.5 2.5 0 0 0 4.5 12z"/></svg>',
  entry: '<svg viewBox="0 0 16 16" width="11" height="11" fill="none" stroke="currentColor" stroke-width="1.5"><path d="M2 8h9"/><path d="M8 5l3 3-3 3"/><path d="M13 2v12"/></svg>',
  unknown: '<svg viewBox="0 0 16 16" width="11" height="11" fill="none" stroke="currentColor" stroke-width="1.5"><circle cx="8" cy="8" r="6"/></svg>',
};

/**
 * The edge Datadog draws: out of the bottom of the caller, into the top of the callee, as a
 * cubic bezier whose control points are pulled vertically. Vertical control points are what
 * make the curve leave and arrive perpendicular to the card, so an edge never appears to
 * graze a card it has nothing to do with.
 */
export function edgePath(sx: number, sy: number, tx: number, ty: number): string {
  const dy = Math.max(24, Math.abs(ty - sy) * 0.5);
  return `M ${sx},${sy} C ${sx},${sy + dy} ${tx},${ty - dy} ${tx},${ty}`;
}

type Reach = { up: Map<string, string[]>; down: Map<string, string[]> };

/** Live controls for a rendered map. */
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
    renderMap(el.id, JSON.parse(graph.textContent ?? ''), { colors: JSON.parse(colors.textContent ?? '') });
  }
}

const reachableFrom = (start: string, adj: Map<string, string[]>): Set<string> => {
  const seen = new Set([start]);
  const stack = [start];
  while (stack.length) for (const n of adj.get(stack.pop()!) ?? []) if (!seen.has(n)) { seen.add(n); stack.push(n); }
  return seen;
};

const fmtRps = (v: number) => (v < 10 ? v.toFixed(2) : fmtNum(Math.round(v)));

export function serviceMapChart(
  containerId: string,
  graph: ServiceGraph,
  opts: { colors?: Record<string, string>; onNodeClick?: (key: string) => void } = {},
): void {
  const el = document.getElementById(containerId);
  if (!el) return;
  // Error / empty states are rendered server-side in Lucid; leave the container alone rather
  // than painting over the message.
  if (graph.error || !graph.nodes?.length) { registerChartDisposer(containerId, () => {}); return; }
  void render(el, containerId, graph, opts);
}

async function render(
  el: HTMLElement,
  containerId: string,
  graph: ServiceGraph,
  opts: { colors?: Record<string, string>; onNodeClick?: (key: string) => void },
): Promise<void> {
  const pane = el.querySelector<HTMLElement>('[data-map-pane]');
  const svg = el.querySelector<SVGSVGElement>('[data-map-edges]');
  const layer = el.querySelector<HTMLElement>('[data-map-nodes]');
  const tpl = el.querySelector<HTMLTemplateElement>('[data-node-card]');
  const panel = el.parentElement ?? el;
  if (!pane || !svg || !layer || !tpl) return;

  registerChartDisposer(containerId, () => {});
  const controller = new AbortController();
  const { signal } = controller;

  const colors = opts.colors ?? {};
  const allNodes = graph.nodes;
  const byKey = new Map(allNodes.map(n => [n.key, n]));
  const members = new Map<string, ServiceNode[]>();
  for (const n of allNodes) if (n.group_key) members.set(n.group_key, [...(members.get(n.group_key) ?? []), n]);

  const expanded = new Set<string>();
  let isolated: Set<string> | null = null;
  let scope: string | null = null;
  let hovered: string | null = null;
  let query = '';
  let cards = new Map<string, HTMLElement>();
  let paths = new Map<string, SVGPathElement>();
  let reach: Reach = { up: new Map(), down: new Map() };
  let drawn: { nodes: ServiceNode[]; edges: ServiceEdge[] } = { nodes: [], edges: [] };

  // --- viewport: one transform, the way React Flow does it ---------------------
  let tx = 0, ty = 0, k = 1;
  const applyTransform = () => { pane.style.transform = `translate(${tx}px, ${ty}px) scale(${k})`; };

  const fit = () => {
    if (!drawn.nodes.length) return;
    const xs = drawn.nodes.map(n => pos.get(n.key)?.x ?? 0);
    const ys = drawn.nodes.map(n => pos.get(n.key)?.y ?? 0);
    const w = Math.max(...xs) - Math.min(...xs) + CARD_W;
    const h = Math.max(...ys) - Math.min(...ys) + CARD_H;
    const box = el.getBoundingClientRect();
    k = Math.min(1, (box.width - 48) / w, (box.height - 48) / h);
    tx = (box.width - w * k) / 2 - Math.min(...xs) * k;
    ty = (box.height - h * k) / 2 - Math.min(...ys) * k;
    applyTransform();
  };

  const zoomAt = (factor: number, cx: number, cy: number) => {
    const next = Math.min(2.5, Math.max(0.15, k * factor));
    // Keep the point under the cursor fixed, which is what makes wheel-zoom feel anchored
    // rather than teleporting.
    tx = cx - (cx - tx) * (next / k);
    ty = cy - (cy - ty) * (next / k);
    k = next;
    applyTransform();
  };

  el.addEventListener('wheel', e => {
    e.preventDefault();
    const box = el.getBoundingClientRect();
    zoomAt(e.deltaY < 0 ? 1.12 : 1 / 1.12, e.clientX - box.left, e.clientY - box.top);
  }, { passive: false, signal });

  let drag: { x: number; y: number; tx: number; ty: number } | null = null;
  el.addEventListener('pointerdown', e => {
    if ((e.target as HTMLElement).closest('[data-node], [data-map-zoom]')) return;
    drag = { x: e.clientX, y: e.clientY, tx, ty };
    el.setPointerCapture(e.pointerId);
    el.style.cursor = 'grabbing';
  }, { signal });
  el.addEventListener('pointermove', e => {
    if (!drag) return;
    tx = drag.tx + (e.clientX - drag.x);
    ty = drag.ty + (e.clientY - drag.y);
    applyTransform();
  }, { signal });
  for (const ev of ['pointerup', 'pointercancel'] as const)
    el.addEventListener(ev, () => { drag = null; el.style.cursor = ''; }, { signal });

  for (const btn of el.querySelectorAll<HTMLElement>('[data-map-zoom]'))
    btn.addEventListener('click', () => {
      const box = el.getBoundingClientRect();
      const a = btn.dataset.mapZoom;
      if (a === 'fit') fit();
      else zoomAt(a === 'zoom-in' ? 1.25 : 1 / 1.25, box.width / 2, box.height / 2);
    }, { signal });

  // --- painting ----------------------------------------------------------------
  const pos = new Map<string, Point>();

  const styleNode = (n: ServiceNode, card: HTMLElement, lit: boolean, onPath: boolean) => {
    const s = getChartStyles();
    const failing = n.stats.error_rate >= ERR_FAILING;
    const elevated = n.stats.error_rate >= ERR_ELEVATED;
    card.style.opacity = lit ? '1' : '0.25';
    card.style.borderColor = onPath ? s.brandColor : failing ? s.errorColor : elevated ? s.warningColor : s.tooltipBorderColor;
    card.style.borderWidth = onPath || elevated ? '2px' : '1px';
    card.style.borderStyle = n.inferred ? 'dashed' : 'solid';
    const health = card.querySelector<HTMLElement>('[data-node-health]')!;
    health.style.background = failing ? s.errorColor : elevated ? s.warningColor : (!n.inferred && colors[n.key] ? resolveColor(n.key, colors) : s.tooltipBorderColor);
    const err = card.querySelector<HTMLElement>('[data-node-errors]')!;
    err.style.color = failing ? s.errorColor : elevated ? s.warningColor : '';
  };

  const buildCards = () => {
    layer.textContent = '';
    cards = new Map();
    for (const n of drawn.nodes) {
      const card = tpl.content.firstElementChild!.cloneNode(true) as HTMLElement;
      card.dataset.key = n.key;
      card.querySelector('[data-node-icon]')!.innerHTML = KIND_ICON[n.kind] ?? KIND_ICON.unknown;
      card.querySelector('[data-node-name]')!.textContent = n.label || 'Entry point';
      const count = card.querySelector<HTMLElement>('[data-node-count]')!;
      if (n.member_count && !n.key.startsWith('rest:')) { count.textContent = `×${n.member_count}`; count.classList.remove('hidden'); }
      card.querySelector('[data-node-errors]')!.textContent = `${(n.stats.error_rate * 100).toFixed(2)}% errors`;
      card.querySelector('[data-node-latency]')!.textContent =
        n.duration_share != null ? `${(n.duration_share * 100).toFixed(1)}% of trace` : `${fmtDur(n.stats.p95_ns)} latency`;
      card.querySelector('[data-node-rps]')!.textContent = `${fmtRps(n.stats.throughput_per_sec)} req/s`;
      card.title = n.label || 'Entry point';
      layer.appendChild(card);
      cards.set(n.key, card);
    }
  };

  const place = () => {
    for (const [key, card] of cards) {
      const p = pos.get(key);
      if (p) card.style.transform = `translate(${p.x}px, ${p.y}px)`;
    }
  };

  const buildEdges = () => {
    svg.querySelectorAll('path[data-edge]').forEach(p => p.remove());
    paths = new Map();
    for (const e of drawn.edges) {
      const a = pos.get(e.source), b = pos.get(e.target);
      if (!a || !b) continue;
      const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
      path.setAttribute('data-edge', '');
      path.setAttribute('fill', 'none');
      path.setAttribute('d', edgePath(a.x + CARD_W / 2, a.y + CARD_H, b.x + CARD_W / 2, b.y));
      svg.appendChild(path);
      paths.set(edgeKey(e.source, e.target), path);
    }
  };

  const paint = () => {
    const s = getChartStyles();
    const lit = filterSelection(drawn.nodes, drawn.edges, query, isolated);
    const onPath = new Set<string>();
    const pathEnds = new Set<string>();
    if (hovered) for (const e of drawn.edges) {
      if (e.source === hovered || e.target === hovered) { onPath.add(edgeKey(e.source, e.target)); pathEnds.add(e.source); pathEnds.add(e.target); }
    }
    for (const n of drawn.nodes) {
      const card = cards.get(n.key);
      if (card) styleNode(n, card, lit.litNodes.has(n.key), pathEnds.has(n.key));
    }
    for (const e of drawn.edges) {
      const key = edgeKey(e.source, e.target);
      const path = paths.get(key);
      if (!path) continue;
      const hot = onPath.has(key);
      const failing = e.stats.error_rate >= ERR_FAILING;
      path.setAttribute('stroke', hot ? s.brandColor : failing ? s.errorColor : e.stats.error_rate >= ERR_ELEVATED ? s.warningColor : s.tooltipBorderColor);
      path.setAttribute('stroke-width', hot ? '2' : '1.25');
      path.setAttribute('stroke-dasharray', byKey.get(e.target)?.inferred ? '4 3' : '');
      path.setAttribute('opacity', String(hot ? 1 : lit.litEdges.has(key) ? 0.75 : 0.12));
      path.setAttribute('marker-end', `url(#${containerId}-arrow)`);
    }
  };

  const rebuild = async () => {
    const scoped = scopeTo(allNodes, graph.edges, scope);
    const vis = visibleGraph(scoped.nodes, scoped.edges, expanded);
    drawn = { nodes: vis.nodes, edges: vis.edges };
    const keys = drawn.nodes.map(n => n.key);
    const { coords } = await layoutGraph(keys, drawn.edges, { layerGap: LAYER_GAP, nodeGap: NODE_GAP, cardW: CARD_W, cardH: CARD_H });
    pos.clear();
    // ELK reports centres; the DOM positions a card by its top-left.
    for (const [key, p] of coords) pos.set(key, { x: p.x - CARD_W / 2, y: p.y - CARD_H / 2 });

    reach = { up: new Map(), down: new Map() };
    for (const key of keys) { reach.up.set(key, []); reach.down.set(key, []); }
    for (const e of drawn.edges) { reach.down.get(e.source)?.push(e.target); reach.up.get(e.target)?.push(e.source); }

    buildCards();
    place();
    buildEdges();
    paint();
  };

  await rebuild();
  fit();

  // --- interaction --------------------------------------------------------------
  const menu = panel.querySelector<HTMLElement>('[data-service-menu]');
  const chip = panel.querySelector<HTMLElement>('[data-map-scope]');
  const pid = el.dataset.mapBase ?? '';
  const hideMenu = () => menu?.classList.add('hidden');

  const showMenu = (key: string, label: string, inferred: boolean, x: number, y: number) => {
    if (!menu) return;
    menu.dataset.nodeKey = key;
    const title = menu.querySelector<HTMLElement>('[data-menu-title]');
    if (title) title.textContent = label;
    const q = encodeURIComponent(`resource.service.name=="${key}"`);
    for (const a of menu.querySelectorAll<HTMLAnchorElement>('[data-menu-action]')) {
      const action = a.dataset.menuAction!;
      a.classList.toggle('hidden', inferred && action !== 'inspect' && action !== 'focus');
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

  const setScope = (key: string | null) => {
    scope = key;
    expanded.clear();
    isolated = null;
    if (chip) {
      const node = key ? byKey.get(key) : undefined;
      chip.classList.toggle('hidden', !node);
      chip.classList.toggle('flex', !!node);
      const label = chip.querySelector<HTMLElement>('[data-scope-label]');
      if (label && node) label.textContent = node.label || 'Entry point';
    }
    void rebuild().then(() => {
      fit();
      const count = chip?.querySelector<HTMLElement>('[data-scope-count]');
      if (count) count.textContent = key ? `Showing ${drawn.nodes.length} services from traces through this service.` : '';
    });
  };
  chip?.querySelector('[data-scope-clear]')?.addEventListener('click', () => setScope(null), { signal });

  menu?.addEventListener('click', e => {
    const a = (e.target as HTMLElement).closest<HTMLAnchorElement>('[data-menu-action]');
    const action = a?.dataset.menuAction;
    if (action === 'inspect' || action === 'focus') e.preventDefault();
    if (action === 'focus' && menu.dataset.nodeKey) setScope(menu.dataset.nodeKey);
    hideMenu();
  }, { signal });

  // Delegated: cards are rebuilt on every layout, so per-card listeners would leak.
  layer.addEventListener('click', e => {
    const card = (e.target as HTMLElement).closest<HTMLElement>('[data-node]');
    const key = card?.dataset.key;
    if (!key) return;
    const node = byKey.get(key);
    if (node?.member_count) { expanded.add(key); isolated = null; hideMenu(); void rebuild().then(fit); return; }
    isolated = new Set([...reachableFrom(key, reach.up), ...reachableFrom(key, reach.down)]);
    paint();
    const box = panel.getBoundingClientRect();
    showMenu(key, node?.label || 'Entry point', !!node?.inferred, e.clientX - box.left, e.clientY - box.top);
    opts.onNodeClick?.(key);
  }, { signal });

  const setHover = (key: string | null) => { if (hovered !== key) { hovered = key; paint(); } };
  layer.addEventListener('pointerover', e => setHover((e.target as HTMLElement).closest<HTMLElement>('[data-node]')?.dataset.key ?? null), { signal });
  layer.addEventListener('pointerout', e => {
    if (!(e.relatedTarget as HTMLElement | null)?.closest?.('[data-node]')) setHover(null);
  }, { signal });
  // Keyboard reaches the same path highlight, which is the whole point of DOM nodes.
  layer.addEventListener('focusin', e => setHover((e.target as HTMLElement).closest<HTMLElement>('[data-node]')?.dataset.key ?? null), { signal });

  const resetView = () => {
    hideMenu();
    const wasExpanded = expanded.size > 0;
    expanded.clear();
    isolated = null;
    if (wasExpanded) void rebuild().then(fit); else paint();
  };
  el.addEventListener('click', e => { if (!(e.target as HTMLElement).closest('[data-node], [data-map-zoom]')) resetView(); }, { signal });
  document.addEventListener('keydown', e => { if (e.key === 'Escape' && (isolated || expanded.size)) resetView(); }, { signal });

  const unsubscribeTheme = subscribeChartTheme(() => paint());
  ['resize', 'toggle-sidebar', 'loglist-resize'].forEach(ev => window.addEventListener(ev, fit, { signal }));
  // Its own observer, not the shared chart one: that batcher resolves each element through
  // `window.echarts`, which this page no longer loads at all.
  const ro = new ResizeObserver(() => fit());
  ro.observe(el);

  const handle: ServiceMapHandle = {
    id: containerId,
    filter: (q: string) => { query = q ?? ''; paint(); },
    clearIsolation: resetView,
    dispose: () => registerChartDisposer(containerId, () => {}),
  };
  handles.set(containerId, handle);
  (el as any).__serviceMap = handle;

  document.addEventListener(FILTER_EVENT, (ev: Event) => {
    const d = (ev as CustomEvent<{ q?: string; id?: string }>).detail ?? {};
    if (d.id && d.id !== containerId) return;
    handle.filter(d.q ?? '');
  }, { signal });

  registerChartDisposer(containerId, () => {
    controller.abort();
    unsubscribeTheme();
    ro.disconnect();
    handles.delete(containerId);
    delete (el as any).__serviceMap;
  });
}

(window as any).serviceMapChart = serviceMapChart;
(window as any).getServiceMapHandle = getServiceMapHandle;
