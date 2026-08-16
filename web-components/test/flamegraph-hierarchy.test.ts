// The flamegraph's span tree.
//
// `parentId` is whatever the customer's instrumentation emitted. A span that parents
// itself, or two spans that parent each other, used to be attached as somebody's child
// and never reached from a root — so those spans silently vanished from the flamegraph
// while the trace still looked complete. Missing spans on the screen someone is debugging
// from is the worst kind of data loss, because nothing signals it.
//
// The walk below carries a hard step budget so these also stand as termination guards on
// the recursive render.
import { describe, test, expect } from 'vitest';
import { buildHierarchy } from '../src/charts';

const span = (spanId: string, parentId: string | null = null) =>
  ({ spanId, parentId, name: spanId, label: spanId, serviceName: 'svc', hasErrors: false, start: 0, value: 1, children: [] }) as any;

// Walk the tree with a hard step budget: a cycle exhausts it instead of hanging the test.
const walk = (roots: any[], budget = 10_000): string[] => {
  const seen: string[] = [];
  const queue = [...roots];
  while (queue.length) {
    if (seen.length > budget) throw new Error('cycle: traversal did not terminate');
    const node = queue.shift();
    seen.push(node.spanId);
    queue.push(...(node.children ?? []));
  }
  return seen;
};

describe('buildHierarchy', () => {
  test('nests children under their parent and returns only true roots', () => {
    const roots = buildHierarchy([span('root'), span('a', 'root'), span('b', 'a')]);

    expect(roots.map((r) => r.spanId)).toEqual(['root']);
    expect(walk(roots)).toEqual(['root', 'a', 'b']);
  });

  test('several independent traces each keep their own root', () => {
    const roots = buildHierarchy([span('r1'), span('r2'), span('c1', 'r1'), span('c2', 'r2')]);

    expect(roots.map((r) => r.spanId).sort()).toEqual(['r1', 'r2']);
    expect(walk(roots).sort()).toEqual(['c1', 'c2', 'r1', 'r2']);
  });

  test('a span whose parent was never ingested is shown as a root, not dropped', () => {
    const roots = buildHierarchy([span('orphan', 'never-sent')]);
    expect(walk(roots)).toEqual(['orphan']);
  });

  test('a self-parented span is rendered once, at the root', () => {
    const roots = buildHierarchy([span('self', 'self')]);

    expect(walk(roots)).toEqual(['self']);
    expect(roots[0].children).toEqual([]);
  });

  // The fatal shape: a real root leading into a two-span cycle. Walking it recursed
  // forever and blew the stack.
  test('a cycle below a real root terminates', () => {
    const roots = buildHierarchy([span('root'), span('a', 'root'), span('b', 'a'), { ...span('a2', 'b'), spanId: 'a', parentId: 'b' }]);
    expect(() => walk(roots)).not.toThrow();
  });

  test('a cycle with no root at all still terminates and keeps its spans', () => {
    const roots = buildHierarchy([span('a', 'b'), span('b', 'a')]);

    expect(() => walk(roots)).not.toThrow();
    expect(walk(roots).sort()).toEqual(['a', 'b']);
  });

  test('every input span appears exactly once in the output tree', () => {
    const input = [span('root'), span('a', 'root'), span('b', 'a'), span('c', 'root'), span('orphan', 'gone')];
    const seen = walk(buildHierarchy(input));

    expect(seen.sort()).toEqual(['a', 'b', 'c', 'orphan', 'root']);
    expect(new Set(seen).size).toBe(seen.length);
  });

  test('a deep chain nests to full depth without losing spans', () => {
    const chain = [span('s0'), ...Array.from({ length: 200 }, (_, i) => span(`s${i + 1}`, `s${i}`))];
    expect(walk(buildHierarchy(chain))).toHaveLength(201);
  });

  test('an empty trace produces no roots rather than throwing', () => {
    expect(buildHierarchy([])).toEqual([]);
  });

  // A whole trace is what this renders, so both shapes it can take at scale have to come
  // out right: one parent with thousands of siblings, and a long ancestor chain. This is a
  // correctness check, not a complexity one — wall-clock budgets do not discriminate
  // reliably enough here to be worth asserting on.
  test('a wide fan-out attaches every sibling exactly once', () => {
    const wide = [span('root'), ...Array.from({ length: 4000 }, (_, i) => span(`s${i}`, 'root'))];

    const roots = buildHierarchy(wide);

    expect(roots).toHaveLength(1);
    expect(roots[0].children).toHaveLength(4000);
    expect(new Set(roots[0].children.map((c: any) => c.spanId)).size).toBe(4000);
  });

  // structuredClone: the renderer mutates children while zooming, and the caller's
  // array is the cached query result.
  test('does not mutate the spans it was given', () => {
    const input = [span('root'), span('a', 'root')];
    buildHierarchy(input);
    expect(input.every((s) => s.children.length === 0)).toBe(true);
  });
});
