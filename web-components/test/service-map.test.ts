import { describe, it, expect, vi } from 'vitest';
import { breakCycles, layerGraph, orderLayers, assignCoords, layoutGraph, filterSelection, hydrateServiceMaps, statsTip, visibleGraph, fitRows, edgeKey, cardLabel, X_GAP, Y_GAP, type Edge } from '../src/service-map';

const e = (source: string, target: string): Edge => ({ source, target });
const snapshot = (keys: string[], edges: Edge[]) => {
  const { coords, back } = layoutGraph(keys, edges);
  return JSON.stringify({ coords: [...coords], back: [...back].sort() });
};

describe('hydrateServiceMaps', () => {
  it('hydrates each server-rendered map once from its embedded payload', () => {
    const graph = { nodes: [], edges: [], range_seconds: 3600, truncated: false, error: null };
    const colors = { gateway: 'bg-blue-400' };
    document.body.innerHTML = `
      <div id="global-service-map" data-service-map></div>
      <script id="global-service-map-data" type="application/json">${JSON.stringify(graph)}</script>
      <script id="global-service-map-colors" type="application/json">${JSON.stringify(colors)}</script>
    `;
    const render = vi.fn();

    hydrateServiceMaps(document, render);
    hydrateServiceMaps(document, render);

    expect(render).toHaveBeenCalledOnce();
    expect(render).toHaveBeenCalledWith('global-service-map', graph, { colors });
  });
});

describe('statsTip', () => {
  const stats = { requests: 1, errors: 0, error_rate: 0, p50_ns: 1, p95_ns: 1, p99_ns: 1, throughput_per_sec: 1 };

  it('uses a dark foreground on a light service color', () => {
    expect(statsTip('currency', stats, '#facc15')).toContain('background:#facc15;color:#493e12');
  });

  it('uses a light foreground on a dark status color', () => {
    expect(statsTip('failed', stats, '#111827')).toContain('background:#111827;color:#d4dcec');
  });
});

describe('cardLabel', () => {
  const node = (over: Partial<Parameters<typeof cardLabel>[0]> = {}) => ({
    key: 'api.paystack.co', label: 'api.paystack.co', kind: 'external' as const, inferred: true,
    duration_share: null, member_count: null, group_key: null,
    stats: { requests: 4500, errors: 0, error_rate: 0, p50_ns: 1e8, p95_ns: 1.48e9, p99_ns: 2e9, throughput_per_sec: 1.25 },
    ...over,
  });

  it('carries all four facts, so the map answers without a hover', () => {
    const card = cardLabel(node());
    expect(card).toContain('{name|api.paystack.co}');
    expect(card).toContain('0.00% errors');
    expect(card).toContain('1.48s p95');
    expect(card).toContain('1.25 req/s');
    expect(card).toContain('{tag|EXT}');
  });

  it('grades the error line in three steps rather than flagging any error at all', () => {
    const rate = (r: number) => cardLabel(node({ stats: { ...node().stats, error_rate: r } })).split('\n')[1];
    expect(rate(0.001)).toContain('{muted|');  // one error in an hour is not an incident
    expect(rate(0.02)).toContain('{warn|');
    expect(rate(0.2)).toContain('{bad|');
  });

  it('names a collapsed head for what it stands for', () => {
    expect(cardLabel(node({ label: 'myshopify.com', member_count: 71 }))).toContain('myshopify.com  ×71');
  });

  it('strips rich-text metacharacters so a hostname cannot break the card', () => {
    expect(cardLabel(node({ label: 'we{ird}|host' }))).toContain('{name|weirdhost}');
  });

  it('shows trace-time share instead of p95 on a trace map', () => {
    const card = cardLabel(node({ duration_share: 0.42 }));
    expect(card).toContain('42.0% of trace time');
    expect(card).not.toContain('p95');
  });
});

describe('visibleGraph', () => {
  // One collapsed head (×3) plus its members, and one ungrouped peer alongside.
  const n = (key: string, member_count: number | null = null, group_key: string | null = null) => ({ key, member_count, group_key });
  const nodes = [
    n('api'), n('grp:http:shop.com', 3), n('http:a.shop.com', null, 'grp:http:shop.com'),
    n('http:b.shop.com', null, 'grp:http:shop.com'), n('http:c.shop.com', null, 'grp:http:shop.com'), n('db:redis'),
  ];
  const edges = [e('api', 'grp:http:shop.com'), e('api', 'http:a.shop.com'), e('api', 'http:b.shop.com'), e('api', 'http:c.shop.com'), e('api', 'db:redis')];

  it('draws heads and hides their members while collapsed', () => {
    const v = visibleGraph(nodes, edges, new Set());
    expect(v.nodes.map(x => x.key)).toEqual(['api', 'grp:http:shop.com', 'db:redis']);
    // The member edges travel in the payload but must not be drawn on top of the collapsed edge.
    expect(v.edges).toEqual([e('api', 'grp:http:shop.com'), e('api', 'db:redis')]);
  });

  it('swaps a head for its members when expanded, leaving other groups collapsed', () => {
    const v = visibleGraph(nodes, edges, new Set(['grp:http:shop.com']));
    expect(v.nodes.map(x => x.key)).toEqual(['api', 'http:a.shop.com', 'http:b.shop.com', 'http:c.shop.com', 'db:redis']);
    expect(v.edges).not.toContainEqual(e('api', 'grp:http:shop.com'));
    expect(v.edges).toHaveLength(4);
  });

  it('nests: a head that is itself folded only appears once its parent opens', () => {
    // The long-tail fold can swallow a domain head, so heads have parents too.
    const nested = [
      n('api'), n('rest:api', 2),
      n('grp:http:shop.com', 3, 'rest:api'), n('db:redis', null, 'rest:api'),
      n('http:a.shop.com', null, 'grp:http:shop.com'), n('http:b.shop.com', null, 'grp:http:shop.com'),
    ];
    const es = [e('api', 'rest:api'), e('api', 'grp:http:shop.com'), e('api', 'db:redis'), e('api', 'http:a.shop.com')];

    expect(visibleGraph(nested, es, new Set()).nodes.map(x => x.key)).toEqual(['api', 'rest:api']);
    expect(visibleGraph(nested, es, new Set(['rest:api'])).nodes.map(x => x.key))
      .toEqual(['api', 'grp:http:shop.com', 'db:redis']);
    // Opening the inner head as well swaps it for its own members, one level deeper.
    expect(visibleGraph(nested, es, new Set(['rest:api', 'grp:http:shop.com'])).nodes.map(x => x.key))
      .toEqual(['api', 'db:redis', 'http:a.shop.com', 'http:b.shop.com']);
  });

  it('expanding an unknown group changes nothing', () => {
    expect(visibleGraph(nodes, edges, new Set(['grp:http:nope.com']))).toEqual(visibleGraph(nodes, edges, new Set()));
  });
});

describe('fitRows', () => {
  it('keeps generous spacing while the layer fits', () => {
    expect(fitRows(520, 4, 64)).toMatchObject({ rowPitch: Y_GAP });
  });

  it('never spaces rows closer than the symbols are wide', () => {
    // The regression: 149 rows in a 520px box used to fit-to-extent down to ~3px apart.
    const { rowPitch, maxRows } = fitRows(520, 149, 64);
    expect(rowPitch).toBeGreaterThanOrEqual(64 + 8);
    expect(maxRows).toBeLessThan(149); // so the rest wraps instead of being crushed
  });

  it('still yields one usable row for a canvas smaller than a node', () => {
    expect(fitRows(20, 10, 64).maxRows).toBe(1);
  });
});

describe('assignCoords', () => {
  it('wraps a layer past maxRows into offset sub-columns', () => {
    const coords = assignCoords([['a', 'b', 'c', 'd', 'e']], { rowPitch: 10, maxRows: 2 });
    // Two rows per sub-column: a,b | c,d | e — each sub-column shifted right, not down.
    expect(coords.get('a')).toEqual({ x: 0, y: -5 });
    expect(coords.get('b')).toEqual({ x: 0, y: 5 });
    expect(coords.get('c')!.x).toBeGreaterThan(0);
    expect(coords.get('c')!.y).toBe(-5);
    expect(coords.get('e')!.x).toBeGreaterThan(coords.get('c')!.x);
    // Whatever the wrapping, no two nodes ever share a point.
    expect(new Set([...coords.values()].map(p => `${p.x},${p.y}`)).size).toBe(5);
  });

  it('advances the next layer past a wrapped layer, not into it', () => {
    // The collision this pins: layer 0 wraps to two columns, so layer 1 must start clear of
    // both. Placing layer n at n * X_GAP put the wrapped column 44px from the next layer's
    // cards, which are 176px wide.
    const coords = assignCoords([['a', 'b', 'c'], ['z']], { rowPitch: 10, maxRows: 2 });
    const wrapped = coords.get('c')!.x;
    expect(wrapped).toBeGreaterThan(0);
    expect(coords.get('z')!.x).toBeGreaterThanOrEqual(wrapped + X_GAP);
  });

  it('leaves an unwrapped layer centred on zero', () => {
    const coords = assignCoords([['a', 'b', 'c']], { rowPitch: 10 });
    expect([...coords.values()].map(p => p.y)).toEqual([-10, 0, 10]);
  });
});

describe('breakCycles', () => {
  it('leaves a DAG untouched', () => {
    const edges = [e('a', 'b'), e('b', 'c'), e('a', 'c')];
    const { acyclic, back } = breakCycles(['a', 'b', 'c'], edges);
    expect(back).toEqual([]);
    expect(acyclic).toEqual(edges);
  });

  it('records exactly the back-edge of a cycle', () => {
    const { acyclic, back } = breakCycles(['a', 'b', 'c'], [e('a', 'b'), e('b', 'c'), e('c', 'a')]);
    expect(back).toEqual([e('c', 'a')]);
    expect(acyclic).toEqual([e('a', 'b'), e('b', 'c')]);
  });

  it('treats a self-loop as a back-edge', () => {
    expect(breakCycles(['a'], [e('a', 'a')]).back).toEqual([e('a', 'a')]);
  });
});

describe('layerGraph', () => {
  it('uses the longest path, not the first one found', () => {
    // a→d directly, and a→b→c→d: d must land on layer 3, not 1.
    const layer = layerGraph(['a', 'b', 'c', 'd'], [e('a', 'd'), e('a', 'b'), e('b', 'c'), e('c', 'd')]);
    expect([...layer]).toEqual([['a', 0], ['b', 1], ['c', 2], ['d', 3]]);
  });

  it('layers each disconnected component from its own root', () => {
    const layer = layerGraph(['a', 'b', 'x', 'y'], [e('a', 'b'), e('x', 'y')]);
    expect(layer.get('a')).toBe(0);
    expect(layer.get('x')).toBe(0);
    expect(layer.get('b')).toBe(1);
    expect(layer.get('y')).toBe(1);
  });
});

describe('assignCoords', () => {
  it('spaces layers on x and centres each layer on y', () => {
    const coords = assignCoords([['a'], ['b', 'c']]);
    expect(coords.get('a')).toEqual({ x: 0, y: 0 });
    expect(coords.get('b')).toEqual({ x: X_GAP, y: -Y_GAP / 2 });
    expect(coords.get('c')).toEqual({ x: X_GAP, y: Y_GAP / 2 });
  });
});

describe('layoutGraph', () => {
  it('places a single node at the origin with no back-edges', () => {
    const { coords, back } = layoutGraph(['solo'], []);
    expect(coords.get('solo')).toEqual({ x: 0, y: 0 });
    expect(back.size).toBe(0);
  });

  it('lays out disconnected components without overlapping rows', () => {
    const { coords } = layoutGraph(['a', 'b', 'x', 'y'], [e('a', 'b'), e('x', 'y')]);
    expect(coords.get('a')!.x).toBe(0);
    expect(coords.get('x')!.x).toBe(0);
    expect(coords.get('a')!.y).not.toBe(coords.get('x')!.y);
    expect(coords.get('b')!.x).toBe(X_GAP);
    expect(coords.get('y')!.x).toBe(X_GAP);
  });

  it('lays out a wide fan-out symmetrically about the root', () => {
    const leaves = Array.from({ length: 9 }, (_, i) => `leaf${i}`);
    const { coords } = layoutGraph(['root', ...leaves], leaves.map(l => e('root', l)));
    const ys = leaves.map(l => coords.get(l)!.y);
    expect(new Set(leaves.map(l => coords.get(l)!.x))).toEqual(new Set([X_GAP]));
    expect(new Set(ys).size).toBe(9);
    expect(ys.reduce((a, b) => a + b, 0)).toBeCloseTo(0);
  });

  it('is deterministic — two runs are byte-identical', () => {
    const keys = ['gateway', 'checkout', 'auth', 'db:orders', 'queue:orders.v1', 'redis'];
    const edges = [
      e('gateway', 'checkout'), e('gateway', 'auth'), e('checkout', 'db:orders'),
      e('checkout', 'queue:orders.v1'), e('auth', 'redis'), e('checkout', 'auth'),
      e('redis', 'gateway'), // back-edge
    ];
    expect(snapshot(keys, edges)).toBe(snapshot(keys, edges));
  });

  it('after layering every edge points forward except the recorded back-edges', () => {
    const keys = ['a', 'b', 'c', 'd', 'e'];
    const edges = [e('a', 'b'), e('b', 'c'), e('c', 'd'), e('d', 'b'), e('a', 'e'), e('e', 'd')];
    const { acyclic, back } = breakCycles(keys, edges);
    const layer = layerGraph(keys, acyclic);
    const backSet = new Set(back.map(b => `${b.source} ${b.target}`));

    expect(backSet.size).toBeGreaterThan(0);
    for (const edge of edges) {
      const forward = layer.get(edge.target)! > layer.get(edge.source)!;
      expect(forward || backSet.has(`${edge.source} ${edge.target}`)).toBe(true);
    }
  });
});

describe('orderLayers', () => {
  it('sorts a layer by the median of its predecessors, tie-breaking on key', () => {
    // Layer 1 seeded alphabetically as [p, q]; p follows the lower-placed parent
    // (a) and q follows (b), so barycenters keep them in that order.
    const keys = ['a', 'b', 'p', 'q'];
    const edges = [e('a', 'p'), e('b', 'q')];
    const layers = orderLayers(layerGraph(keys, edges), edges);
    expect(layers[0]).toEqual(['a', 'b']);
    expect(layers[1]).toEqual(['p', 'q']);
  });

  it('reduces crossings on a swapped bipartite layer', () => {
    const keys = ['a', 'b', 'x', 'y'];
    const edges = [e('a', 'y'), e('b', 'x')];
    const layers = orderLayers(layerGraph(keys, edges), edges);
    const pos = (k: string) => layers[1].indexOf(k);
    // 'y' hangs off 'a' (first in layer 0) so it must be ordered before 'x'.
    expect(pos('y')).toBeLessThan(pos('x'));
  });
});

describe('filterSelection', () => {
  // `db:orders` is an inferred node whose label has the prefix stripped, so the
  // search must match the label, never the key.
  const nodes = [
    { key: 'gateway', label: 'gateway' },
    { key: 'checkout', label: 'checkout' },
    { key: 'auth', label: 'auth' },
    { key: 'db:orders', label: 'orders' },
  ];
  const edges = [e('gateway', 'checkout'), e('gateway', 'auth'), e('checkout', 'db:orders')];

  it('lights everything for a blank or whitespace query', () => {
    for (const q of ['', '   ']) {
      const { litNodes, litEdges } = filterSelection(nodes, edges, q);
      expect(litNodes.size).toBe(4);
      expect(litEdges.size).toBe(3);
    }
  });

  it('matches labels case-insensitively as a substring, and keeps incident edges lit', () => {
    const { litNodes, litEdges } = filterSelection(nodes, edges, 'CHECK');
    expect([...litNodes]).toEqual(['checkout']);
    // Both edges touching checkout stay lit so the path is never severed.
    expect([...litEdges].sort()).toEqual([edgeKey('checkout', 'db:orders'), edgeKey('gateway', 'checkout')]);
  });

  it('matches on label, not on the inferred key prefix', () => {
    expect([...filterSelection(nodes, edges, 'orders').litNodes]).toEqual(['db:orders']);
    expect(filterSelection(nodes, edges, 'db:').litNodes.size).toBe(0);
  });

  it('dims everything when nothing matches', () => {
    const { litNodes, litEdges } = filterSelection(nodes, edges, 'nope');
    expect(litNodes.size).toBe(0);
    expect(litEdges.size).toBe(0);
  });

  it('intersects with click-isolation rather than overriding it', () => {
    const isolated = new Set(['gateway', 'checkout', 'db:orders']);
    // Isolation alone: auth and its edge go dark.
    const iso = filterSelection(nodes, edges, '', isolated);
    expect([...iso.litNodes].sort()).toEqual(['checkout', 'db:orders', 'gateway']);
    expect([...iso.litEdges].sort()).toEqual([edgeKey('checkout', 'db:orders'), edgeKey('gateway', 'checkout')]);

    // Adding a query narrows further; it never resurrects the isolated-out node.
    const both = filterSelection(nodes, edges, 'gateway', isolated);
    expect([...both.litNodes]).toEqual(['gateway']);
    expect([...both.litEdges]).toEqual([edgeKey('gateway', 'checkout')]);

    // A query that only matches an isolated-out node lights nothing.
    expect(filterSelection(nodes, edges, 'auth', isolated).litNodes.size).toBe(0);
  });

  it('is order-independent for the same inputs', () => {
    const a = filterSelection(nodes, edges, 'a');
    const b = filterSelection([...nodes].reverse(), edges, 'a');
    expect([...a.litNodes].sort()).toEqual([...b.litNodes].sort());
  });
});
