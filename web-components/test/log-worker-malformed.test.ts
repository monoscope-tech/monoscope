// Regression guards for trace shapes a malformed/misinstrumented SDK can emit. Each of these
// used to take the tab down rather than render a bad-looking trace: groupSpans recursed the
// server-supplied parent→children adjacency with no cycle guard, no depth bound, and a
// per-node copy of the entire descendant subtree.
import { describe, expect, test } from 'vitest';
import { groupSpans } from '../src/log-worker-functions';

const COLS = { trace_id: 0, latency_breakdown: 1, parent_id: 2, timestamp: 3, duration: 4, start_time_ns: 5, errors: 6, summary: 7, kind: 8, id: 9 };
const row = (id: string, parent: string | null, startNs = 0, duration = 10) =>
  Object.assign([], { 0: 'tr', 1: id, 2: parent, 3: '2024-01-01T00:00:00Z', 4: duration, 5: startNs, 6: false, 7: [], 8: 'span', 9: id });
const trace = (root: string, children: Record<string, string[]>) => [{ trace_id: 'tr', start_time: 0, duration: 100, trace_start_time: null, root, children }];

const totalSegments = (tree: any[]) => tree.reduce((a, r) => a + (r.childrenTimeSpans?.length ?? 0), 0);

describe('groupSpans survives malformed trace adjacency', () => {
  test('a parent cycle terminates instead of overflowing the stack', () => {
    // a → b → a. Previously: RangeError (Maximum call stack size exceeded), after allocating
    // a quadratic number of subtree objects on the way down.
    const tree = groupSpans([row('a', 'b'), row('b', 'a')], COLS as any, { tr: true }, false, trace('a', { a: ['b'], b: ['a'] }));
    expect(tree.map((r) => r.id)).toEqual(['a', 'b']); // each span emitted exactly once
  });

  test('a self-parent span terminates', () => {
    const tree = groupSpans([row('a', 'a')], COLS as any, { tr: true }, false, trace('a', { a: ['a'] }));
    expect(tree.map((r) => r.id)).toEqual(['a']);
  });

  test('a span reachable from two parents is emitted once, not duplicated', () => {
    // Duplicate rows would carry duplicate ids, which the list's seenIds dedupe then drops
    // anyway — but only after the whole subtree was built twice.
    const tree = groupSpans(
      [row('root', null), row('l', 'root'), row('r', 'root'), row('shared', 'l')],
      COLS as any,
      { tr: true },
      false,
      trace('root', { root: ['l', 'r'], l: ['shared'], r: ['shared'] })
    );
    expect(tree.filter((r) => r.id === 'shared')).toHaveLength(1);
  });

  // A chain of `n` spans, each the child of the one before it.
  const chain = (n: number) => {
    const rows = Array.from({ length: n }, (_, i) => row(`s${i}`, i === 0 ? null : `s${i - 1}`, i, n - i));
    const children: Record<string, string[]> = {};
    for (let i = 1; i < n; i++) children[`s${i - 1}`] = [`s${i}`];
    return { rows, traces: trace('s0', children) };
  };

  test('a deep chain stays linear in memory rather than quadratic', () => {
    // 2000 spans nested 2000 deep. Before: 1,999,000 childrenTimeSpans objects, every one of
    // them structured-cloned worker→main, because each node held a copy of its whole subtree.
    const n = 2000;
    const { rows, traces } = chain(n);
    const tree = groupSpans(rows, COLS as any, { tr: true }, false, traces);
    // Linear, not n^2/2 — the old code produced ~2,000,000 segments here.
    expect(totalSegments(tree)).toBeLessThan(n * 70);
  });

  test('depths beyond anything real are still rendered as rows', () => {
    // 400 deep is already absurd but under the malformed-data cutoff, so nothing is dropped.
    const { rows, traces } = chain(400);
    expect(groupSpans(rows, COLS as any, { tr: true }, false, traces)).toHaveLength(400);
  });

  test('shallow real-world traces keep every descendant segment exactly', () => {
    // The bound must not perturb normal data: production traces run ~3 deep.
    const tree = groupSpans(
      [row('api', null, 0, 1000), row('svc', 'api', 100, 800), row('db', 'svc', 200, 500)],
      COLS as any,
      { tr: true },
      false,
      trace('api', { api: ['svc'], svc: ['db'] })
    );
    const spans = (id: string) => tree.find((r: any) => r.id === id)!.childrenTimeSpans.map((c: any) => ({ depth: c.depth, startNs: c.startNs, duration: c.duration }));
    expect(spans('api')).toEqual([
      { depth: 1, startNs: 100, duration: 800 },
      { depth: 2, startNs: 200, duration: 500 },
    ]);
    expect(spans('svc')).toEqual([{ depth: 1, startNs: 200, duration: 500 }]);
  });
});
