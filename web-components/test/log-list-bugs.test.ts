import { describe, test, expect, vi } from 'vitest';
import { row, serverTransport, logPage, treeFromLogs, COLS, deferredTransport, stubFetch, ids, mountList } from './log-list-harness';
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
    el.transport = serverTransport(logPage(['1', '2', '3']), logPage(['3', '4'])); // page 2 re-sends boundary row 3
    await el.fetchData('init', false, false, false);
    await el.fetchData('lm', false, false, true);
    expect((el as any).loadedCount).toBe((el as any).spanListTree.length);
    expect((el as any).loadedCount).toBe(4);
  });

  // Dedup must hold across many overlapping pages (inclusive-cursor boundary row
  // recurs each page) now that the merge filters via a persistent seenIds set
  // instead of re-deduping the whole tree.
  test('paginated overlapping pages dedupe to a unique, ordered tree', async () => {
    const el = await mountList();
    el.transport = serverTransport(
      logPage(['1', '2', '3']),
      logPage(['3', '4', '5']), // 3 overlaps prior page
      logPage(['5', '6']), // 5 overlaps prior page
    );
    await el.fetchData('init', false, false, false);
    await el.fetchData('lm1', false, false, true);
    await el.fetchData('lm2', false, false, true);
    expect(ids(el)).toEqual(['1', '2', '3', '4', '5', '6']);
  });

  // A refresh must reset the dedup state, else an id seen by the previous query
  // would be wrongly dropped from the new query's results.
  test('refresh resets dedup state so a previously-seen id reappears', async () => {
    const el = await mountList();
    el.transport = serverTransport(logPage(['1', '2']), logPage(['2']));
    await el.fetchData('init', false, false, false);
    await el.fetchData('newquery', true, false, false); // refresh / new query
    expect(ids(el)).toEqual(['2']);
  });

  // Live-tail on a bounded range must stop (not hang silently) once the newest
  // loaded row reaches `to`: from=newest+10ms ≥ to → every tick fetches empty.
  test('buildRecentFetchUrl stops live-tail (with toast) at the upper bound', async () => {
    const el = await mountList();
    window.history.replaceState({}, '', '/log_explorer?to=2026-06-01T00%3A00%3A00.000Z&query=x');
    el.transport = serverTransport(logPage([['1', '2026-06-01T00:00:00.000Z']])); // newest loaded row is AT `to`
    await el.fetchData('/log_explorer?to=2026-06-01T00%3A00%3A00.000Z&query=x&json=true', false, false, false);
    (el as any).isLiveStreaming = true;
    (el as any).liveStreamInterval = setInterval(() => {}, 1e7);
    const btn = document.createElement('input');
    btn.type = 'checkbox';
    btn.checked = true;
    (el as any).liveBtn = btn;
    let toast: string | undefined;
    const onToast = (e: any) => (toast = e.detail.value[0]);
    document.body.addEventListener('errorToast', onToast);
    try {
      (el as any).buildRecentFetchUrl();
      expect((el as any).isLiveStreaming).toBe(false);
      expect((el as any).liveStreamInterval).toBeNull();
      expect(btn.checked).toBe(false);
      expect(toast).toMatch(/end of the selected time range/);
    } finally {
      document.body.removeEventListener('errorToast', onToast);
    }
  });

  test('buildRecentFetchUrl keeps live-tail running while below the upper bound', async () => {
    const el = await mountList();
    window.history.replaceState({}, '', '/log_explorer?to=2026-06-01T00%3A00%3A00.000Z&query=x');
    el.transport = serverTransport(logPage([['1', '2026-05-01T00:00:00.000Z']])); // well below `to`
    await el.fetchData('/log_explorer?to=2026-06-01T00%3A00%3A00.000Z&query=x&json=true', false, false, false);
    (el as any).isLiveStreaming = true;
    (el as any).buildRecentFetchUrl();
    expect((el as any).isLiveStreaming).toBe(true);
  });

  // M2: a refresh must drop inline-expanded aggregate children, else stale rows
  // from the previous query render under a key that survives the new query.
  test('refresh clears expandedAggregates', async () => {
    const el = await mountList();
    (el as any).expandedAggregates = { hash1: { rows: [['x']], cols: ['id'], colIdxMap: { id: 0 }, hasMore: false, loading: false, skip: 1 } };
    el.transport = serverTransport(logPage(['1']));
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
    el.transport = serverTransport(logPage([['1', '2026-05-01T00:00:00.000Z']]));
    await el.fetchData('/log_explorer?to=2026-06-01T00%3A00%3A00.000Z&query=x&json=true', false, false, false);
    const url = new URL((el as any).buildRecentFetchUrl(), 'http://localhost');
    expect(url.searchParams.get('to')).toBe('2026-06-01T00:00:00.000Z');
    expect(url.searchParams.has('cursor')).toBe(false);
  });

  // buildRecentFetchUrl must apply the ns/µs/ms tolerance too (it used raw new Date()).
  test('buildRecentFetchUrl tolerates a nanosecond-epoch timestamp (no year-55000 from)', async () => {
    const el = await mountList();
    window.history.replaceState({}, '', '/log_explorer?query=x');
    el.transport = serverTransport(logPage([['1', 1700000000000000000]])); // ns ≈ Nov 2023
    await el.fetchData('/log_explorer?query=x&json=true', false, false, false);
    const url = new URL((el as any).buildRecentFetchUrl(), 'http://localhost');
    expect(new Date(url.searchParams.get('from')!).getUTCFullYear()).toBe(2023);
  });

  // expandTimeRangeUrl ("Show earlier events") had the same raw-String(ns) cursor bug.
  test('expandTimeRangeUrl tolerates a nanosecond-epoch timestamp in the cursor', async () => {
    const el = await mountList();
    window.history.replaceState({}, '', '/log_explorer?since=1H&query=x');
    el.transport = serverTransport(logPage([['1', 1700000000000000000]]));
    await el.fetchData('/log_explorer?since=1H&query=x&json=true', false, false, false);
    const url = new URL((el as any).expandTimeRangeUrl(), 'http://localhost');
    expect(new Date(url.searchParams.get('cursor')!).getUTCFullYear()).toBe(2023);
  });

  // An empty live-tail tick must NOT flip expandTimeRange on (would flash "Show
  // earlier events" every quiet 5s tick even though history isn't exhausted).
  test('an empty recent fetch does not turn on expandTimeRange', async () => {
    const el = await mountList();
    el.transport = serverTransport(logPage(['1', '2']), logPage([])); // 2nd tick returns nothing
    await el.fetchData('initial', false, false, false);
    (el as any).expandTimeRange = false;
    await el.fetchData('recent', false, true, false); // isRecentFetch, returns nothing
    expect((el as any).expandTimeRange).toBe(false);
  });

  // Switching to an aggregate view must null the interval, not just clear it — else
  // handleLiveToggle's `!liveStreamInterval` guard skips setInterval on switch-back.
  test('mode-switch to aggregate nulls liveStreamInterval (re-enable works later)', async () => {
    const el = await mountList();
    (el as any).liveStreamInterval = setInterval(() => {}, 1e7);
    (el as any).isLiveStreaming = true;
    (el as any).mode = 'patterns';
    (el as any).updated(new Map([['mode', 'logs']]));
    expect((el as any).liveStreamInterval).toBeNull();
    expect((el as any).isLiveStreaming).toBe(false);
  });
});

// Pagination workflow: the cursor for "earlier"/load-more must page strictly
// before the OLDEST loaded row. Reported symptom: last visible row was 16:54:45
// but the triggered request used cursor=16:55:02 (NEWER) → "earlier" re-fetched
// rows already on screen.
//
// Driven end-to-end through the real worker pipeline (serverTransport runs the
// real groupSpans): a server page returns a trace whose root (16:54:45) is the
// oldest row but whose child span (16:55:02) is later. flattenSpanTree appends
// the root FIRST then its child, and traces sort newest-start-first, so the last
// array element is the newer child leaf — not the oldest row. Deriving the cursor
// from spanListTree[length-1] therefore picked the child's (newer) timestamp.
describe('LogList — earlier/load-more pagination cursor (worker pipeline)', () => {
  const tsRoot = '2026-06-26T16:54:45.000Z'; // oldest row: the trace root (visually last)
  const tsChild = '2026-06-26T16:55:02.644Z'; // its later child span, flattened AFTER the root
  // A trace (root + one later child) — the shape logPage can't express. Indexed by COLS.
  const rootRow = [tsRoot, 'span-root', 'trace-1', '', 'server', 'id-root', 100, 1_750_000_485_000_000_000];
  const childRow = [tsChild, 'span-child', 'trace-1', 'span-root', 'client', 'id-child', 50, 1_750_000_502_644_000_000];
  const traces = [{ trace_id: 'trace-1', start_time: 1_750_000_485_000_000_000, duration: 100, trace_start_time: tsRoot, root: 'span-root', children: { 'span-root': ['span-child'] } }];
  const page = (over: any = {}) => ({ logsData: [rootRow, childRow], colIdxMap: COLS, traces, ...over });

  test('load-more requests cursor older than the oldest row (not the trailing child leaf)', async () => {
    const el = await mountList();
    window.history.replaceState({}, '', '/log_explorer?query=x');
    const t = serverTransport(page({ nextUrl: '/log_explorer?query=x&layout=loadmore', hasMore: true }), page());
    el.transport = t;

    await el.fetchData('/log_explorer?query=x&json=true', false, false, false);

    // The trap the bug fell into: the flattened tree ends on the newer child, while
    // the oldest VISIBLE (depth-0) row is the root. The cursor must follow the root.
    const tree = (el as any).spanListTree;
    expect(tree[tree.length - 1].data[0]).toBe(tsChild); // last array elem is the newer leaf
    expect(tree.find((r: any) => r.depth === 0).data[0]).toBe(tsRoot); // oldest visible row

    // Fire the load-more and inspect the request that actually hit the endpoint.
    await el.fetchData((el as any).buildLoadMoreUrl(), false, false, true);
    const sent = new URL(t.urls[1], 'http://localhost');
    expect(sent.searchParams.get('cursor')).toBe(cursorFromTimestamp(tsRoot, -10));
  });

  test('"Show earlier events" (expandTimeRangeUrl) requests cursor from the oldest row', async () => {
    const el = await mountList();
    window.history.replaceState({}, '', '/log_explorer?since=1H&query=x');
    // hasMore:false → after the initial fetch the component shows "Show earlier events".
    el.transport = serverTransport(page({ hasMore: false }));

    await el.fetchData('/log_explorer?since=1H&query=x&json=true', false, false, false);
    expect((el as any).expandTimeRange).toBe(true);

    const url = new URL((el as any).expandTimeRangeUrl(), 'http://localhost');
    expect(url.searchParams.get('cursor')).toBe(cursorFromTimestamp(tsRoot, 0));
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
    const cols = ['id', 'service', 'summary'];
    el.transport = serverTransport(logPage(['1'], { cols }), logPage(['2'], { cols }));
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
    // Query A's first page is on screen (built through the real pipeline)...
    el.transport = serverTransport(logPage(['a1', 'a2']));
    await el.fetchData('A', false, false, false);

    // ...then a load-more and a refresh race, both resolving out of order.
    const tx = deferredTransport();
    el.transport = tx as any;
    const loadMore = el.fetchData('A-loadmore', false, false, true); // in flight (deferred)
    const refresh = el.fetchData('B-newquery', true, false, false); // new query, also deferred

    tx.settle(1, treeFromLogs(['b1', 'b2'])); // refresh (query B) resolves first
    await refresh;
    tx.settle(0, treeFromLogs(['a3', 'a4'])); // stale load-more (query A) resolves later
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

    tx.settle(1, treeFromLogs(['r1'])); // recent resolves first → its finally runs
    await recent;
    expect((el as any).isLoadingMore).toBe(true); // load-more still in flight

    tx.settle(0, treeFromLogs(['r2']));
    await loadMore;
    expect((el as any).isLoadingMore).toBe(false);
  });
});
