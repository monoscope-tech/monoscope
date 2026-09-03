import { describe, it, expect, vi } from 'vitest';
import { breakCycles, layoutGraph, filterSelection, hydrateServiceMaps, visibleGraph, edgeKey, edgePath, scopeTo, menuHref, serviceMapFilter, FILTER_EVENT, CARD_W, CARD_H, type Edge } from '../src/service-map';

const e = (source: string, target: string): Edge => ({ source, target });
const snapshot = async (keys: string[], edges: Edge[]) => {
  const { coords, back } = await layoutGraph(keys, edges);
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

describe('scopeTo', () => {
  // gateway -> checkout -> db, and an unrelated worker -> queue alongside it.
  const nodes = ['gateway', 'checkout', 'db', 'worker', 'queue'].map(key => ({ key }));
  const edges = [e('gateway', 'checkout'), e('checkout', 'db'), e('worker', 'queue')];

  it('keeps the whole chain a service participates in, upstream and down', () => {
    const v = scopeTo(nodes, edges, 'checkout');
    expect(v.nodes.map(n => n.key)).toEqual(['gateway', 'checkout', 'db']);
    expect(v.edges).toEqual([e('gateway', 'checkout'), e('checkout', 'db')]);
  });

  it('is a no-op for no scope or a scope that is not on the map', () => {
    expect(scopeTo(nodes, edges, null).nodes).toHaveLength(5);
    expect(scopeTo(nodes, edges, 'gone').nodes).toHaveLength(5);
  });
});

describe('edgePath', () => {
  it('runs right, down a shared mid-lane, then right again', () => {
    // Orthogonal plumbing preserves a caller-to-callee route through a dense column.
    const d = edgePath(100, 60, 300, 200);
    expect(d.startsWith('M 100,60')).toBe(true);
    expect(d.endsWith('L 300,200')).toBe(true);
    // The vertical run sits at the midpoint between the cards.
    expect(d).toContain('200');
    expect(d).toMatch(/Q 200,60|Q 200,200/);
  });

  it('collapses to a straight line when the cards are horizontally aligned', () => {
    expect(edgePath(200, 50, 300, 50)).toBe('M 200,50 L 300,50');
  });

  it('shrinks its corner radius rather than overshooting a short hop', () => {
    // A 10px corner on a 6px run would double back on itself.
    const d = edgePath(100, 0, 106, 4);
    const nums = [...d.matchAll(/-?\d+(?:\.\d+)?/g)].map(m => Number(m[0]));
    expect(Math.max(...nums)).toBeLessThanOrEqual(106);
    expect(Math.min(...nums)).toBeGreaterThanOrEqual(0);
  });

  it('turns the correct way for a callee above its caller', () => {
    const below = edgePath(0, 0, 200, 100);
    const above = edgePath(0, 100, 200, 0);
    expect(below).toContain('Q 100,0 100,10');
    expect(above).toContain('Q 100,100 100,90');
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

describe('layoutGraph', () => {
  it('uses the compact Datadog card footprint for every layout and connector', () => {
    expect([CARD_W, CARD_H]).toEqual([150, 62]);
  });

  it('places a lone node and reports no back-edges', async () => {
    const { coords, back } = await layoutGraph(['solo'], []);
    expect(coords.get('solo')).toBeDefined();
    expect(back.size).toBe(0);
  });

  it('puts every callee to the right of its caller', async () => {
    const { coords } = await layoutGraph(['a', 'b', 'x', 'y'], [e('a', 'b'), e('x', 'y')]);
    expect(coords.get('b')!.x).toBeGreaterThan(coords.get('a')!.x);
    expect(coords.get('y')!.x).toBeGreaterThan(coords.get('x')!.x);
    // Disconnected roots share the leftmost column rather than stacking.
    expect(coords.get('a')!.x).toBe(coords.get('x')!.x);
  });

  it('never overlaps two cards, however wide the fan-out', async () => {
    const leaves = Array.from({ length: 12 }, (_, i) => `leaf${i}`);
    const { coords } = await layoutGraph(['root', ...leaves], leaves.map(l => e('root', l)));
    const boxes = [...coords.values()];
    for (let i = 0; i < boxes.length; i++)
      for (let j = i + 1; j < boxes.length; j++) {
        const overlap = Math.abs(boxes[i].x - boxes[j].x) < CARD_W && Math.abs(boxes[i].y - boxes[j].y) < CARD_H;
        expect(overlap).toBe(false);
      }
  });

  it('is deterministic — two runs are byte-identical', async () => {
    // A shared map link must not reshuffle between refreshes, so ELK's tie-breaks are
    // pinned to model order. This is the assertion that guards that.
    const keys = ['gateway', 'checkout', 'auth', 'db:orders', 'queue:orders.v1', 'redis'];
    const edges = [
      e('gateway', 'checkout'), e('gateway', 'auth'), e('checkout', 'db:orders'),
      e('checkout', 'queue:orders.v1'), e('auth', 'redis'), e('checkout', 'auth'),
      e('redis', 'gateway'), // back-edge
    ];
    expect(await snapshot(keys, edges)).toBe(await snapshot(keys, edges));
  });

  it('still records back-edges, which ELK breaks internally but does not report', async () => {
    const keys = ['a', 'b', 'c'];
    const { back } = await layoutGraph(keys, [e('a', 'b'), e('b', 'c'), e('c', 'a')]);
    expect(back.has(edgeKey('c', 'a'))).toBe(true);
    expect(back.size).toBe(1);
  });

  it('lays out a node whose key contains characters ELK ids must survive', async () => {
    const keys = ['grp:http:my-shop.com', 'db:orders', ''];
    const { coords } = await layoutGraph(keys, [e('', 'db:orders'), e('db:orders', 'grp:http:my-shop.com')]);
    expect([...coords.keys()].sort()).toEqual(keys.slice().sort());
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

describe('serviceMapFilter', () => {
  it('dispatches the dashed wire event the renderer listens for', () => {
    // The Lucid input reaches this through hyperscript `call`, not `send`: hyperscript parses
    // an event name as an identifier path, so `send service-map-filter(...)` was a parse
    // error that dropped the whole attribute (and, in prod, the filter with it).
    const seen: any[] = [];
    const onFilter = (ev: Event) => seen.push((ev as CustomEvent).detail);
    document.addEventListener(FILTER_EVENT, onFilter);
    try {
      serviceMapFilter('checkout');
      serviceMapFilter('db', 'trace-map');
    } finally {
      document.removeEventListener(FILTER_EVENT, onFilter);
    }
    expect(seen).toEqual([{ q: 'checkout', id: undefined }, { q: 'db', id: 'trace-map' }]);
  });
});

describe('menuHref', () => {
  const base = '/p/proj';
  const query = (href: string) => decodeURIComponent(new URL(href, 'https://x').searchParams.get('query') ?? '');

  it('shows all events or only logs for the selected service', () => {
    const events = menuHref(base, 'events', 'checkout');
    const logs = menuHref(base, 'logs', 'checkout');
    expect(query(events)).toBe('resource.service.name=="checkout"');
    expect(query(logs)).toBe('resource.service.name=="checkout" AND kind=="log"');
    expect(events).not.toBe(logs);
  });

  it('only ever asks for a viz type that exists', () => {
    // visTypes is logs | timeseries | timeseries_line | patterns | sessions; `traces` is not
    // one, and an unknown value falls through to the default instead of erroring.
    for (const action of ['events', 'logs'])
      expect(new URL(menuHref(base, action, 'svc'), 'https://x').searchParams.get('viz_type')).toBe('logs');
  });

  it('routes the non-explorer actions and refuses to invent a link for the rest', () => {
    expect(menuHref(base, 'metrics', 'a/b')).toBe('/p/proj/metrics?metric_source=a%2Fb');
    expect(menuHref(base, 'monitors', 'svc')).toBe('/p/proj/monitors');
    for (const action of ['inspect', 'focus', 'nonsense']) expect(menuHref(base, action, 'svc')).toBe('#');
  });
});
