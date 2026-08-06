import { describe, it, expect } from 'vitest';
import { breakCycles, layerGraph, orderLayers, assignCoords, layoutGraph, filterSelection, type Edge } from '../src/service-map';

const e = (source: string, target: string): Edge => ({ source, target });
const snapshot = (keys: string[], edges: Edge[]) => {
  const { coords, back } = layoutGraph(keys, edges);
  return JSON.stringify({ coords: [...coords], back: [...back].sort() });
};

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
    expect(coords.get('b')).toEqual({ x: 190, y: -48 });
    expect(coords.get('c')).toEqual({ x: 190, y: 48 });
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
    expect(coords.get('b')!.x).toBe(190);
    expect(coords.get('y')!.x).toBe(190);
  });

  it('lays out a wide fan-out symmetrically about the root', () => {
    const leaves = Array.from({ length: 9 }, (_, i) => `leaf${i}`);
    const { coords } = layoutGraph(['root', ...leaves], leaves.map(l => e('root', l)));
    const ys = leaves.map(l => coords.get(l)!.y);
    expect(new Set(leaves.map(l => coords.get(l)!.x))).toEqual(new Set([190]));
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
    expect([...litEdges].sort()).toEqual(['checkout db:orders', 'gateway checkout']);
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
    expect([...iso.litEdges].sort()).toEqual(['checkout db:orders', 'gateway checkout']);

    // Adding a query narrows further; it never resurrects the isolated-out node.
    const both = filterSelection(nodes, edges, 'gateway', isolated);
    expect([...both.litNodes]).toEqual(['gateway']);
    expect([...both.litEdges]).toEqual(['gateway checkout']);

    // A query that only matches an isolated-out node lights nothing.
    expect(filterSelection(nodes, edges, 'auth', isolated).litNodes.size).toBe(0);
  });

  it('is order-independent for the same inputs', () => {
    const a = filterSelection(nodes, edges, 'a');
    const b = filterSelection([...nodes].reverse(), edges, 'a');
    expect([...a.litNodes].sort()).toEqual([...b.litNodes].sort());
  });
});
