import { describe, test, expect, vi } from 'vitest';
import { row, fakeTransport, deferredTransport, stubFetch, ids, mountList } from './log-list-harness';
import { shouldBufferRecent, cursorFromTimestamp } from '../src/log-list-utils';

describe('LogList — LOWER', () => {
  // Lo2: a resized-then-hidden column must not retain its width (which would
  // resurrect on re-add and grow columnMaxWidthMap unboundedly).
  test('hideColumn prunes its stored width', async () => {
    const el = await mountList();
    (el as any).columnMaxWidthMap = { service: 400, summary: 300 };
    el.hideColumn('service');
    expect((el as any).columnMaxWidthMap.service).toBeUndefined();
    expect((el as any).columnMaxWidthMap.summary).toBe(300);
  });
  test('handleColumnsChanged prunes widths for removed columns', async () => {
    const el = await mountList();
    (el as any).columnMaxWidthMap = { a: 100, b: 200, c: 300 };
    el.handleColumnsChanged({ detail: ['a', 'c'] });
    expect(Object.keys((el as any).columnMaxWidthMap).sort()).toEqual(['a', 'c']);
  });

  // Lo5: patterns/sessions pagination must not stop at page 1 when the server
  // omits hasMore on a full page.
  test('aggregate fetch infers hasMore from row presence when server omits it', async () => {
    const el = await mountList({ mode: 'patterns' } as any);
    const restore = stubFetch({ logsData: [['x'], ['y']], cols: ['id'], colIdxMap: { id: 0 } }); // no hasMore
    try {
      const { meta } = await (el as any).workerFetch('u');
      expect(meta.hasMore).toBe(true);
    } finally { restore(); }
  });
});

describe('cursorFromTimestamp', () => {
  test('ISO string round-trips with the offset', () => {
    expect(cursorFromTimestamp('2026-06-01T00:00:00.000Z', -10)).toBe('2026-05-31T23:59:59.990Z');
  });
  test('numeric epoch-ns is not misread as ms (no year-55000 cursor)', () => {
    const iso = cursorFromTimestamp(1700000000000000000, 0); // 1.7e18 ns ≈ Nov 2023
    expect(new Date(iso).getUTCFullYear()).toBe(2023);
  });
  test('numeric epoch-µs (>1e14) is scaled to ms', () => {
    const iso = cursorFromTimestamp(1700000000000000, 0); // 1.7e15 µs ≈ Nov 2023
    expect(new Date(iso).getUTCFullYear()).toBe(2023);
  });
  test('numeric epoch-ms (<1e14) passes through unscaled', () => {
    const iso = cursorFromTimestamp(1700000000000, 0); // 1.7e12 ms ≈ Nov 2023
    expect(new Date(iso).getUTCFullYear()).toBe(2023);
  });
});

describe('LogList — MED correctness', () => {
  // M3: loadedCount over-counted because queryResultCount re-reports the
  // dedup-dropped boundary row on every page. It should match visible rows.
  test('loadedCount equals visible row count after an overlapping load-more', async () => {
    const el = await mountList();
    el.transport = fakeTransport({ tree: [row('1'), row('2'), row('3')] }, { tree: [row('3'), row('4')] });
    await el.fetchData('init', false, false, false);
    await el.fetchData('lm', false, false, true);
    expect((el as any).loadedCount).toBe((el as any).spanListTree.length);
    expect((el as any).loadedCount).toBe(4);
  });

  // M2: a refresh must drop inline-expanded aggregate children, else stale rows
  // from the previous query render under a key that survives the new query.
  test('refresh clears expandedAggregates', async () => {
    const el = await mountList();
    (el as any).expandedAggregates = { hash1: { rows: [['x']], cols: ['id'], colIdxMap: { id: 0 }, hasMore: false, loading: false, skip: 1 } };
    el.transport = fakeTransport({ tree: [row('1')] });
    await el.fetchData('newquery', true, false, false);
    expect(Object.keys((el as any).expandedAggregates)).toHaveLength(0);
  });

  // M1: buffered "new" rows must lose the highlight after they merge, even if
  // fetchedNew was already cleared by an intervening spanListTree change.
  test('isNew highlight clears after merge regardless of fetchedNew', async () => {
    vi.useFakeTimers();
    try {
      const el = await mountList();
      const n1 = { ...row('n1'), isNew: true };
      (el as any).spanListTree = [n1, row('o1')];
      (el as any).fetchedNew = false; // the broken precondition
      (el as any).updated(new Map([['spanListTree', []]])); // lifecycle fires on the merge
      vi.advanceTimersByTime(4000);
      expect((el as any).spanListTree.find((r: any) => r.id === 'n1').isNew).toBe(false);
    } finally {
      vi.useRealTimers();
    }
  });

  // M5: live-tail on a bounded range must keep the upper `to` bound.
  test('buildRecentFetchUrl preserves the to bound', async () => {
    const el = await mountList();
    window.history.replaceState({}, '', '/log_explorer?to=2026-06-01T00%3A00%3A00.000Z&query=x');
    (el as any).colIdxMap = { timestamp: 0 };
    (el as any).spanListTree = [row('1', ['2026-05-01T00:00:00.000Z'])];
    const url = new URL((el as any).buildRecentFetchUrl(), 'http://localhost');
    expect(url.searchParams.get('to')).toBe('2026-06-01T00:00:00.000Z');
    expect(url.searchParams.has('cursor')).toBe(false);
  });
});

// M4: buffering decision (pure) — buffer whenever scrolled off the insertion edge.
describe('shouldBufferRecent', () => {
  test('newest-first: buffers as soon as scrolled off the top (>0), not at 30px', () => {
    expect(shouldBufferRecent(true, 0, true, false)).toBe(false); // at top → insert
    expect(shouldBufferRecent(true, 10, false, false)).toBe(true); // scrolled → buffer (was false at <=30)
  });
  test('oldest-first: buffers unless pinned to the bottom', () => {
    expect(shouldBufferRecent(true, 999, true, true)).toBe(false); // at bottom → insert
    expect(shouldBufferRecent(true, 999, false, true)).toBe(true); // scrolled up → buffer
  });
  test('never buffers when not live streaming', () => {
    expect(shouldBufferRecent(false, 500, false, false)).toBe(false);
  });
});

describe('LogList — lifecycle cleanup (no leaks across disconnect / remount)', () => {
  // Bug: the #streamLiveData change listener + pagehide were added in the
  // CONSTRUCTOR and never removed → after an HTMX-morph remount, the old
  // (disconnected) instance's closure still fires on the shared global checkbox,
  // stacking orphaned 5s setInterval polling loops.
  test('live-stream listener + interval do not leak across disconnect/remount', async () => {
    const btn = document.createElement('input');
    btn.type = 'checkbox'; btn.id = 'streamLiveData';
    document.body.appendChild(btn);
    const intervals = new Set<any>();
    const realSet = globalThis.setInterval, realClear = globalThis.clearInterval;
    (globalThis as any).setInterval = (fn: any) => { const id = realSet(fn, 1e7); intervals.add(id); return id; };
    (globalThis as any).clearInterval = (id: any) => { intervals.delete(id); realClear(id); };
    const toggle = (on: boolean) => { btn.checked = on; btn.dispatchEvent(new Event('change')); };
    try {
      const a = await mountList();
      toggle(true);
      expect(intervals.size).toBe(1);     // A polling
      a.remove();                          // disconnect A
      expect(intervals.size).toBe(0);      // its interval cleared

      toggle(false); toggle(true);         // A's orphaned listener must NOT restart polling
      expect(intervals.size).toBe(0);

      const b = await mountList();
      toggle(true);
      expect(intervals.size).toBe(1);      // exactly one (B) — not stacked with A
      b.remove();
      expect(intervals.size).toBe(0);
    } finally {
      (globalThis as any).setInterval = realSet;
      (globalThis as any).clearInterval = realClear;
      btn.remove();
    }
  });

  // Bug: in-flight worker callbacks were never cleared on disconnect; the 120s
  // timeout could later reject onto a dead component (DOM touch after teardown).
  test('pending worker callbacks are dropped on disconnect, not rejected', async () => {
    const el = await mountList();
    let rejected = false;
    (el as any).workerCallbacks.set(99, { resolve() {}, reject() { rejected = true; } });
    el.remove();
    expect((el as any).workerCallbacks.size).toBe(0);
    expect(rejected).toBe(false);
  });
});

// Regression tests for bugs surfaced by the deep log-list audit. Each reproduces a
// concrete user-visible defect; see web_components_test_harness memory for the map.

describe('LogList — columns survive background refetches', () => {
  // Bug: hideColumn edits logsColumns locally, but fetchData overwrites it with
  // meta.cols on EVERY fetch — so a load-more or 5s live-stream tick restores the
  // hidden column. Column hiding is effectively broken under live/paginated views.
  test('a hidden column is not restored by a load-more refetch', async () => {
    const el = await mountList();
    el.transport = fakeTransport(
      { tree: [row('1')], meta: { cols: ['id', 'service', 'summary'] } },
      { tree: [row('2')], meta: { cols: ['id', 'service', 'summary'] } },
    );
    await el.fetchData('initial', false, false, false);
    el.hideColumn('service');
    expect((el as any).logsColumns).not.toContain('service');

    await el.fetchData('loadmore', false, false, true); // e.g. scroll / live tick
    expect((el as any).logsColumns).not.toContain('service'); // stays hidden
  });
});

describe('LogList — aggregate (patterns) child pagination', () => {
  // Bug: fetchAggregateChildren stores skip = queryResultCount (per-page count, e.g.
  // 3) instead of a cumulative offset, and merges with no dedup. "Load more" on a
  // pattern's children refetches the same window → duplicate child rows and later
  // pages become unreachable.
  test('child skip advances cumulatively across pages (no duplicate refetch)', async () => {
    const el = await mountList({ mode: 'patterns' } as any);
    (el as any).colIdxMap = { pattern_hash: 0, summary: 1 };
    const parent = row('p1', ['hash1', 'a pattern']);

    const restore = stubFetch(
      { rows: [['r1'], ['r2'], ['r3']], cols: ['id'], colIdxMap: { id: 0 }, queryResultCount: 3, hasMore: true },
      { rows: [['r4'], ['r5'], ['r6']], cols: ['id'], colIdxMap: { id: 0 }, queryResultCount: 3, hasMore: true },
    );
    try {
      await (el as any).toggleAggregateRow(parent); // first open → fetch skip=0
      const key = 'hash1';
      expect((el as any).expandedAggregates[key].skip).toBe(3);

      // "Load more" fetches from the stored skip.
      await (el as any).fetchAggregateChildren(key, (el as any).expandedAggregates[key].skip);
      // Next offset must be 6, not pinned at the per-page count (3).
      expect((el as any).expandedAggregates[key].skip).toBe(6);
      expect((el as any).expandedAggregates[key].rows).toHaveLength(6);
    } finally {
      restore();
    }
  });
});

describe('LogList — concurrent refresh vs load-more', () => {
  // Bug: the three loading guards are independent, so a refresh (new query) and an
  // in-flight load-more run concurrently. If the load-more resolves AFTER the
  // refresh, its older rows from the PREVIOUS query are merged into the new query's
  // results — cross-query contamination with no visible signal.
  test('a load-more resolving after a refresh does not contaminate the new query', async () => {
    const el = await mountList();
    const tx = deferredTransport();
    el.transport = tx as any;

    // page of query A is on screen
    await (async () => { const t = fakeTransport({ tree: [row('a1'), row('a2')] }); el.transport = t as any; await el.fetchData('A', false, false, false); el.transport = tx as any; })();

    const loadMore = el.fetchData('A-loadmore', false, false, true); // in flight (deferred)
    const refresh = el.fetchData('B-newquery', true, false, false); // new query, also deferred

    tx.settle(1, [row('b1'), row('b2')]); // refresh (query B) resolves first
    await refresh;
    tx.settle(0, [row('a3'), row('a4')]); // stale load-more (query A) resolves later
    await loadMore;

    // Must show ONLY query B's rows — A's older page must not be appended.
    expect(ids(el)).toEqual(['b1', 'b2']);
  });

  test('finishing one fetch kind does not clear another kind\'s in-flight guard', async () => {
    const el = await mountList();
    const tx = deferredTransport();
    el.transport = tx as any;

    const loadMore = el.fetchData('lm', false, false, true); // isLoadingMore = true
    const recent = el.fetchData('recent', false, true, false); // isFetchingRecent = true

    tx.settle(1, [row('r1')]); // recent resolves first → its finally runs
    await recent;
    expect((el as any).isLoadingMore).toBe(true); // load-more still in flight

    tx.settle(0, [row('r2')]);
    await loadMore;
    expect((el as any).isLoadingMore).toBe(false);
  });
});
