import { describe, test, expect, vi } from 'vitest';
import { row, serverTransport, serverTransportFlipped, logPage, treeFromLogs, COLS, deferredTransport, stubFetch, ids, mountList, fakeLiveTransport, stubContainer, stubVirtualizer } from './log-list-harness';
import { DenseRowFlowLayout, virtualItemKey, MAX_RETAINED_ROWS, HISTORY_PREFETCH_ROWS } from '../src/log-list';
import { shouldBufferRecent, atInsertionEdge, cursorFromTimestamp } from '../src/log-list-utils';

describe('LogList — LOWER', () => {
  test('prefetches history before the virtual load-more row is mounted', async () => {
    const el = await mountList();
    (el as any).virtualListItems = Array.from({ length: 200 }, (_, i) => row(`r${i}`));
    (el as any).hasMore = true;
    const fetch = vi.spyOn(el, 'fetchData').mockResolvedValue(undefined);
    vi.spyOn(el as any, 'buildLoadMoreUrl').mockReturnValue('next-page');

    el.handleVisibilityChange({ first: 130, last: 150 });
    el.handleVisibilityChange({ first: 140, last: 200 - 1 - HISTORY_PREFETCH_ROWS });

    expect(fetch).toHaveBeenCalledOnce();
    expect(fetch).toHaveBeenCalledWith('next-page', false, false, true);
  });

  test('does not prefetch away from the history edge or while a page is in flight', async () => {
    const el = await mountList();
    (el as any).virtualListItems = Array.from({ length: 200 }, (_, i) => row(`r${i}`));
    (el as any).hasMore = true;
    const fetch = vi.spyOn(el, 'fetchData').mockResolvedValue(undefined);

    el.handleVisibilityChange({ first: 50, last: 80 });
    (el as any).isLoadingMore = true;
    el.handleVisibilityChange({ first: 150, last: 190 });

    expect(fetch).not.toHaveBeenCalled();
  });

  test('prefetches from the start of an oldest-first virtual list, including index zero', async () => {
    const el = await mountList();
    (el as any).virtualListItems = Array.from({ length: 200 }, (_, i) => row(`r${i}`));
    (el as any).flipDirection = true;
    (el as any).hasMore = true;
    const fetch = vi.spyOn(el, 'fetchData').mockResolvedValue(undefined);
    vi.spyOn(el as any, 'buildLoadMoreUrl').mockReturnValue('older-page');

    el.handleVisibilityChange({ first: 60, last: 80 });
    el.handleVisibilityChange({ first: 0, last: 20 });

    expect(fetch).toHaveBeenCalledWith('older-page', false, false, true);
    expect((el as any).lastVisibilityRange).toEqual({ first: 0, last: 20 });
  });

  test('does not cascade another prefetch when a page merge changes visibility while stationary', async () => {
    const el = await mountList();
    (el as any).virtualListItems = Array.from({ length: 100 }, (_, i) => row(`r${i}`));
    (el as any).prefetchBaseline = { first: 50, last: 70 };
    (el as any).hasMore = true;
    const fetch = vi.spyOn(el, 'fetchData').mockResolvedValue(undefined);

    el.handleVisibilityChange({ first: 50, last: 70 });

    expect(fetch).not.toHaveBeenCalled();
  });

  // Prepending newer rows raises every index by the page size. Measured against the
  // previous range that reads as a scroll toward history, and the prefetch cascaded
  // through pages the reader never approached.
  test('a merge that renumbers rows is not read as a scroll toward the history edge', async () => {
    const el = await mountList();
    const rows = Array.from({ length: 100 }, (_, i) => row(`r${i}`));
    (el as any).spanListTree = rows;
    (el as any).seenIds = new Set(rows.map((r) => r.id));
    (el as any).hasMore = true;
    (el as any).updateVisibleItems();
    const fetch = vi.spyOn(el, 'fetchData').mockResolvedValue(undefined);

    el.handleVisibilityChange({ first: 50, last: 70 }); // reader parks near the history edge
    (el as any).spanListTree = (el as any).mergeIntoTree([row('newer')], true);
    (el as any).updateVisibleItems();
    el.handleVisibilityChange({ first: 51, last: 71 }); // same rows, shifted by the prepend

    expect(fetch).not.toHaveBeenCalled();
  });

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

  // The virtualizer already bounds the DOM; forcing its scroll surface into a
  // compositor layer or deferring paint on its small row runway causes blank flashes.
  test('virtualizer does not force compositing or defer visible row paint', async () => {
    const el = await mountList();
    expect(el.querySelector('style')?.textContent).not.toMatch(/lit-virtualizer\s*{[^}]*will-change:/);
    expect(el.querySelector('style')?.textContent).not.toMatch(/lit-virtualizer > tr\s*{[^}]*content-visibility:/);
    expect(el.innerHTML).not.toContain('will-change-scroll');
  });

  test('pointer row activation does not focus-scroll the virtual table', async () => {
    const el = await mountList();
    const plainCell = document.createElement('td');
    const plainMouseDown = new MouseEvent('mousedown', { button: 0, cancelable: true });
    Object.defineProperty(plainMouseDown, 'target', { value: plainCell });
    (el as any).preserveGridFocusOnPointerRowClick(plainMouseDown);
    expect(plainMouseDown.defaultPrevented).toBe(true);

    const button = document.createElement('button');
    const buttonMouseDown = new MouseEvent('mousedown', { button: 0, cancelable: true });
    Object.defineProperty(buttonMouseDown, 'target', { value: button });
    (el as any).preserveGridFocusOnPointerRowClick(buttonMouseDown);
    expect(buttonMouseDown.defaultPrevented).toBe(false);
  });

  // Paint containment clips overflowing row tooltips, so it must not be permanent. During
  // active scrolling there is no useful hover interaction, however, and bounding paint to one
  // 28px row substantially reduces the virtualizer's paint invalidation area.
  test('rows add paint containment only while the list is actively scrolling', async () => {
    const el = await mountList();
    const css = el.querySelector('style')?.textContent ?? '';
    expect(css).toMatch(/contain:\s*layout style\s*;/);
    expect(css).toMatch(/\.is-scrolling \.contain-layout-style\s*{[^}]*contain:\s*layout style paint/s);
    expect(el.innerHTML).not.toContain('contain-layout-style-paint');

    // happy-dom reparents the deliberately non-standard virtualized table structure, so use a
    // small scroll surface to exercise the handler itself.
    const container = document.createElement('div');
    container.id = 'logs_list_container_inner';
    el.appendChild(container);
    Object.defineProperty(el, 'logsContainer', { value: container, configurable: true });
    (el as any).handleListScroll();
    expect(container.classList.contains('is-scrolling')).toBe(true);
    await vi.waitFor(() => expect(container.classList.contains('is-scrolling')).toBe(false), { timeout: 250 });
  });

  // FlowLayout defaults to 100px before its first measurement, but logs are fixed
  // at 28px. The inflated estimate caused oversized scroll gaps.
  test('virtualizer starts with the dense log-row height and bounded overhang', () => {
    const layout = new DenseRowFlowLayout(() => {}, {});
    expect((layout as any)._itemSize.height).toBe(28);
    expect((layout as any)._overhang).toBe(200);
  });

  // 2026-08-06: LogResult carries the failure as `error`; the client read
  // `data.message` first, so every server message (e.g. the unknown-field hint)
  // was replaced by the generic fallback in the toast and the query box stayed
  // empty. Assert the real message reaches both.
  test('a server query error surfaces its own message, not a generic fallback', async () => {
    const el = await mountList({ mode: 'patterns' } as any);
    const msg = 'Unknown field "attribute". Did you mean "attributes"?';
    const seen: string[] = [];
    const onParseError = (e: Event) => seen.push((e as CustomEvent).detail);
    document.body.addEventListener('showParseError', onParseError);
    const restore = stubFetch({ error: msg });
    try {
      await expect((el as any).workerFetch('u')).rejects.toThrow(msg);
      expect(seen).toEqual([msg]);
    } finally {
      restore();
      document.body.removeEventListener('showParseError', onParseError);
    }
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
  test('a page containing duplicate rows retains each id once', async () => {
    const el = await mountList();
    el.transport = serverTransport(logPage(['1', '1', '2']), logPage(['3', '3', '4']));
    await el.fetchData('init', false, false, false);
    expect(ids(el)).toEqual(['1', '2']);
    await el.fetchData('refresh', true, false, false);
    expect(ids(el)).toEqual(['3', '4']);
  });

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
    expect((el as any).cachedServerTraces.map((t: any) => t.trace_id)).toEqual(['1', '2', '3', '4', '5', '6']);
  });

  test('pagination bounds retained rows and reopens the evicted edge', async () => {
    const el = await mountList();
    const initial = Array.from({ length: MAX_RETAINED_ROWS }, (_, i) => row(`r${i}`));
    (el as any).spanListTree = initial;
    (el as any).seenIds = new Set(initial.map((r) => r.id));

    (el as any).spanListTree = (el as any).mergeIntoTree([row('older')], false);
    expect(ids(el)).toHaveLength(MAX_RETAINED_ROWS);
    expect(ids(el)[0]).toBe('r1');
    expect((el as any).hasNewer).toBe(true);
    expect((el as any).seenIds.has('r0')).toBe(false);

    (el as any).spanListTree = (el as any).mergeIntoTree([row('r0')], true);
    expect(ids(el)).toHaveLength(MAX_RETAINED_ROWS);
    expect(ids(el)[0]).toBe('r0');
    expect(ids(el)).not.toContain('older');
    expect((el as any).hasMore).toBe(true);
  });

  test('window trimming captures and restores the visible row before loading', async () => {
    const el = await mountList();
    const initial = Array.from({ length: MAX_RETAINED_ROWS }, (_, i) => row(`r${i}`));
    (el as any).spanListTree = initial;
    (el as any).seenIds = new Set(initial.map((r) => r.id));
    const anchor = { id: `r${MAX_RETAINED_ROWS - 10}`, offset: 7 };
    const capture = vi.spyOn(el as any, 'captureScrollAnchor').mockReturnValueOnce(anchor).mockReturnValue(null);
    const restore = vi.spyOn(el as any, 'restoreScrollAnchor').mockResolvedValue(undefined);
    const transport = deferredTransport();
    el.transport = transport;

    const request = el.fetchData('older', false, false, true);
    expect(capture).toHaveBeenCalledOnce();
    transport.settle(0, [row('older')]);
    await request;

    expect(ids(el)).toContain(anchor.id);
    expect(restore).toHaveBeenCalledWith(anchor);
  });

  test('ordinary newest-first load-more appends without correcting the scroll anchor', async () => {
    const el = await mountList();
    (el as any).spanListTree = [row('visible')];
    (el as any).seenIds = new Set(['visible']);
    const anchor = { id: 'visible', offset: 7 };
    vi.spyOn(el as any, 'captureScrollAnchor').mockReturnValue(anchor);
    const restore = vi.spyOn(el as any, 'restoreScrollAnchor').mockResolvedValue(undefined);
    el.transport = serverTransport(logPage(['older']));

    await el.fetchData('older', false, false, true);

    expect(ids(el)).toEqual(['visible', 'older']);
    expect(restore).not.toHaveBeenCalled();
  });

  test('oldest-first load-more still restores after prepending older rows', async () => {
    const el = await mountList({ flipDirection: true });
    (el as any).spanListTree = [row('visible')];
    (el as any).seenIds = new Set(['visible']);
    const anchor = { id: 'visible', offset: 7 };
    vi.spyOn(el as any, 'captureScrollAnchor').mockReturnValue(anchor);
    const restore = vi.spyOn(el as any, 'restoreScrollAnchor').mockResolvedValue(undefined);
    el.transport = serverTransportFlipped(logPage(['older']));

    await el.fetchData('older', false, false, true);

    expect(ids(el)).toEqual(['older', 'visible']);
    expect(restore).toHaveBeenCalledWith(anchor);
  });

  test('scroll anchoring falls back to the virtualizer range while rows are recycling', async () => {
    const el = await mountList();
    Object.defineProperty(el, 'logsContainer', {
      value: stubContainer(),
    });
    (el as any).virtualListItems = [{ type: 'fetchRecent' }, row('visible'), { type: 'loadMore' }];
    (el as any).lastVisibilityRange = { first: 1, last: 2 };

    // index is carried too: it is what puts the reader back when the retention window
    // cuts the anchor row itself out of the list.
    expect((el as any).captureScrollAnchor()).toEqual({ id: 'visible', offset: 0, index: 1, scrollTop: 0 });
  });

  test('anchor restoration adjusts a rendered row in place without first snapping it to the top', async () => {
    const el = await mountList();
    const scrollToIndex = vi.fn();
    const renderedRow = { dataset: { rowId: 'visible' }, getBoundingClientRect: () => ({ top: 12 }) };
    const container = stubContainer({ querySelector: () => renderedRow });
    Object.defineProperty(el, 'logsContainer', { value: container });
    (el as any).virtualListItems = [row('visible')];
    vi.spyOn(el, 'querySelector').mockReturnValue(stubVirtualizer({ scrollToIndex }) as any);

    await (el as any).restoreScrollAnchor({ id: 'visible', offset: 2, index: 0, scrollTop: 0 });

    expect(scrollToIndex).not.toHaveBeenCalled();
    expect(container.scrollTop).toBe(10);
  });

  test('anchor restoration asks the virtualizer for a row that has been recycled', async () => {
    const el = await mountList();
    const scrollToIndex = vi.fn();
    const renderedRow = { dataset: { rowId: 'visible' }, getBoundingClientRect: () => ({ top: 12 }) };
    const container = stubContainer({ querySelector: vi.fn().mockReturnValueOnce(null).mockReturnValue(renderedRow) });
    Object.defineProperty(el, 'logsContainer', { value: container });
    (el as any).virtualListItems = [row('visible')];
    vi.spyOn(el, 'querySelector').mockReturnValue(stubVirtualizer({ scrollToIndex }) as any);

    await (el as any).restoreScrollAnchor({ id: 'visible', offset: 2, index: 0, scrollTop: 0 });
    await new Promise((resolve) => requestAnimationFrame(resolve));

    expect(scrollToIndex).toHaveBeenCalledWith(0, 'start');
    expect(container.scrollTop).toBe(10);
  });

  test('an oversized trace cannot collapse the retained window to zero rows', async () => {
    const el = await mountList();
    const traceRows = Array.from({ length: MAX_RETAINED_ROWS }, (_, i) => ({ ...row(`t${i}`), traceId: 'oversized-trace' }));
    (el as any).spanListTree = traceRows;
    (el as any).seenIds = new Set(traceRows.map((r) => r.id));

    (el as any).spanListTree = (el as any).mergeIntoTree([{ ...row('tail'), traceId: 'oversized-trace' }], false);
    expect(ids(el)).toHaveLength(MAX_RETAINED_ROWS);
    expect(ids(el)[0]).toBe('t1');
  });

  test('explicit load newer reveals the fetched rows instead of preserving the old anchor', async () => {
    const el = await mountList();
    (el as any).spanListTree = [row('old')];
    (el as any).seenIds = new Set(['old']);
    (el as any).updateVisibleItems();
    const container = stubContainer({ scrollTop: 100, clientHeight: 100 });
    Object.defineProperty(el, 'logsContainer', { value: container });
    vi.spyOn(el as any, 'captureScrollAnchor').mockReturnValue({ id: 'old', offset: 0 });
    const restore = vi.spyOn(el as any, 'restoreScrollAnchor').mockResolvedValue(undefined);
    el.transport = serverTransport(logPage(['new']));

    await (el as any).fetchData('newer', false, true, false, true);
    await new Promise((resolve) => requestAnimationFrame(resolve));

    expect(ids(el)[0]).toBe('new');
    expect(container.scrollTop).toBe(0);
    expect(restore).not.toHaveBeenCalled();
  });

  test('newer pagination uses an adjacent forward cursor', async () => {
    const el = await mountList();
    window.history.replaceState({}, '', '/log_explorer?since=1H&query=x');
    el.transport = serverTransport(logPage([['middle', '2026-06-01T00:00:00.000Z']]));
    await el.fetchData('init', false, false, false);
    const url = new URL((el as any).buildRecentFetchUrl(), 'http://localhost');
    expect(url.searchParams.get('direction')).toBe('newer');
    expect(url.searchParams.get('cursor')).toBe('2026-06-01T00:00:00.010Z');
    expect(url.searchParams.has('from')).toBe(false);
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
    const live = fakeLiveTransport();
    (el as any).isLiveStreaming = true;
    await (el as any).startLiveStream();
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
      expect(live.openCount()).toBe(0); // the connection is closed, not just the flag flipped
      expect(btn.checked).toBe(false);
      expect(toast).toMatch(/end of the selected time range/);
    } finally {
      document.body.removeEventListener('errorToast', onToast);
      live.restore();
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

  // Symmetric to the load-more cursor bug: the NEWEST timestamp must be a scan, not
  // spanListTree[0]. The newest trace's child span starts later than its root but is
  // flattened AFTER it, so positional access undercounts `from` and the to-boundary
  // stop-check never fires → live-tail polls an empty [from,to] window forever.
  test('buildRecentFetchUrl stops live-tail using the newest row (later child span), not the trace root', async () => {
    const el = await mountList();
    const tRoot = '2026-06-01T00:00:00.000Z';
    const tChild = '2026-06-01T00:00:05.000Z'; // newer child of the same trace (sits after root in the tree)
    const to = '2026-06-01T00:00:02.000Z'; // between root+10ms and child+10ms
    const rootNs = Date.parse(tRoot) * 1e6;
    const rootRow = [tRoot, 's-root', 'tr', '', 'server', 'i-root', 100, rootNs];
    const childRow = [tChild, 's-child', 'tr', 's-root', 'client', 'i-child', 50, Date.parse(tChild) * 1e6];
    const traces = [{ trace_id: 'tr', start_time: rootNs, duration: 100, trace_start_time: tRoot, root: 's-root', children: { 's-root': ['s-child'] } }];
    window.history.replaceState({}, '', `/log_explorer?to=${encodeURIComponent(to)}&query=x`);
    el.transport = serverTransport({ logsData: [rootRow, childRow], colIdxMap: COLS, traces });
    await el.fetchData(`/log_explorer?to=${encodeURIComponent(to)}&query=x&json=true`, false, false, false);
    (el as any).isLiveStreaming = true;
    (el as any).buildRecentFetchUrl();
    expect((el as any).isLiveStreaming).toBe(false); // newest=child → from=child+10ms ≥ to → stops
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
    expect(url.searchParams.get('cursor')).toBe('2026-05-01T00:00:00.010Z');
    expect(url.searchParams.get('direction')).toBe('newer');
  });

  // buildRecentFetchUrl must apply the ns/µs/ms tolerance to its forward cursor.
  test('buildRecentFetchUrl tolerates a nanosecond-epoch timestamp (no year-55000 cursor)', async () => {
    const el = await mountList();
    window.history.replaceState({}, '', '/log_explorer?query=x');
    el.transport = serverTransport(logPage([['1', 1700000000000000000]])); // ns ≈ Nov 2023
    await el.fetchData('/log_explorer?query=x&json=true', false, false, false);
    const url = new URL((el as any).buildRecentFetchUrl(), 'http://localhost');
    expect(new Date(url.searchParams.get('cursor')!).getUTCFullYear()).toBe(2023);
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

  // Switching to an aggregate view must null the stream, not just stop it — else
  // handleLiveToggle's `!liveStream?.isRunning` guard is fine but the stale object lingers,
  // and a switch-back re-enables against a connection that is already gone.
  test('mode-switch to aggregate closes the live stream (re-enable works later)', async () => {
    const el = await mountList();
    const live = fakeLiveTransport();
    try {
      (el as any).isLiveStreaming = true;
      await (el as any).startLiveStream();
      expect(live.openCount()).toBe(1);

      (el as any).mode = 'patterns';
      (el as any).updated(new Map([['mode', 'logs']]));
      expect((el as any).liveStream).toBeNull();
      expect((el as any).isLiveStreaming).toBe(false);
      expect(live.openCount()).toBe(0);

      (el as any).mode = 'logs';
      await (el as any).startLiveStream();
      expect(live.openCount()).toBe(1);
    } finally {
      live.restore();
    }
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
  const rootNs = Date.parse(tsRoot) * 1e6;
  const rootRow = [tsRoot, 'span-root', 'trace-1', '', 'server', 'id-root', 100, rootNs];
  const childRow = [tsChild, 'span-child', 'trace-1', 'span-root', 'client', 'id-child', 50, Date.parse(tsChild) * 1e6];
  const traces = [{ trace_id: 'trace-1', start_time: rootNs, duration: 100, trace_start_time: tsRoot, root: 'span-root', children: { 'span-root': ['span-child'] } }];
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

// serverTransportFlipped groups oldest-first so flipped-component tests get a tree
// order matching what the component renders; serverTransport stays newest-first.
describe('serverTransport flip variants', () => {
  test('serverTransportFlipped groups oldest-first; serverTransport stays newest-first', async () => {
    const def = await serverTransport(logPage(['1', '2', '3']))('u'); // id 1 = newest
    expect(def.tree.map((r: any) => r.id)).toEqual(['1', '2', '3']);
    const flipped = await serverTransportFlipped(logPage(['1', '2', '3']))('u');
    expect(flipped.tree.map((r: any) => r.id)).toEqual(['3', '2', '1']);
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
  test('live-stream listener + connection do not leak across disconnect/remount', async () => {
    const btn = document.createElement('input');
    btn.type = 'checkbox'; btn.id = 'streamLiveData';
    document.body.appendChild(btn);
    const live = fakeLiveTransport();
    // The toggle is a shared global checkbox, so a leaked listener shows up as a second
    // connection opening for a component that is no longer on the page.
    const toggle = async (on: boolean) => { btn.checked = on; btn.dispatchEvent(new Event('change')); await Promise.resolve(); await Promise.resolve(); };
    try {
      const a = await mountList();
      await toggle(true);
      expect(live.openCount()).toBe(1);     // A streaming
      a.remove();                            // disconnect A
      expect(live.openCount()).toBe(0);      // its connection closed

      await toggle(false); await toggle(true); // A's orphaned listener must NOT reconnect
      expect(live.openCount()).toBe(0);

      const b = await mountList();
      await toggle(true);
      expect(live.openCount()).toBe(1);      // exactly one (B) — not stacked with A
      b.remove();
      expect(live.openCount()).toBe(0);
    } finally {
      live.restore();
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

  // Bug: live tail stranded — the "N new" pill counted past 200 while the list, visibly at
  // the top, never took another row. A fractional scrollTop (trackpad, zoom, anchor restore)
  // read as "scrolled away" under a bare `> 0`, and nothing ever put it back.
  test('a sub-pixel scroll offset is still the top, not a scroll-off', () => {
    expect(atInsertionEdge(0.5, false, false)).toBe(true);
    expect(shouldBufferRecent(true, 0.5, false, false)).toBe(false); // inserts, does not strand
    expect(atInsertionEdge(10, false, false)).toBe(false); // a real scroll still buffers
  });
});

describe('LogList — live tail resumes when scrolled back to the edge', () => {
  // The buffer exists so a batch never yanks the viewport mid-read. Back at the edge there is
  // nothing to protect, so it must flush on its own — clicking the pill was the only exit.
  // The scroll position is stubbed rather than scrolled: under jsdom every height is 0, so a
  // real container reads as "pinned to the bottom" and the two directions can't be told apart.
  // Newest-first (flipDirection false) is the case in the report — rows enter at the top.
  // Stubbing the container is what makes the flipped direction expressible at all: under jsdom
  // every height is 0, so `scrollTop + clientHeight >= scrollHeight - 1` holds for any real
  // container and oldest-first always reads as "pinned to the bottom".
  const withBuffer = async (scrollTop: number, over: Partial<Record<string, unknown>> = {}, scrollHeight = 5000) => {
    const el = await mountList();
    Object.assign(el as any, { isLiveStreaming: true, flipDirection: false, recentDataToBeAdded: [row('n1'), row('n2')], ...over });
    Object.defineProperty(el, 'logsContainer', { configurable: true, get: () => stubContainer({ scrollTop, scrollHeight }) });
    return el;
  };

  test('flushes the buffer once the viewport is back at the insertion edge', async () => {
    const el = await withBuffer(0);
    (el as any).resumeLiveTailAtEdge();
    expect((el as any).recentDataToBeAdded).toHaveLength(0);
    expect(ids(el)).toEqual(expect.arrayContaining(['n1', 'n2']));
  });

  test('a sub-pixel offset counts as the edge, so live tail is never stranded 1px down', async () => {
    const el = await withBuffer(0.5);
    (el as any).resumeLiveTailAtEdge();
    expect((el as any).recentDataToBeAdded).toHaveLength(0);
  });

  // One rule decides three things — buffer, flush, and whether to anchor the scroll. A second
  // spelling of "at the edge" (there was briefly a private `atNewRowEdge` using `<= 0` beside
  // the shared `<= 2`) makes them disagree in the gap: the batch inserts because it is at the
  // edge, then anchors because it isn't, scrolling the old top row back over the rows just
  // streamed in. Inserting at the edge must never capture an anchor.
  // Both insertion paths, because they decide separately: a recent fetch and an SSE push.
  test('inserting at a sub-pixel offset does not anchor the scroll (no bounce)', async () => {
    const el = await withBuffer(0.5, { recentDataToBeAdded: [] });
    const capture = vi.spyOn(el as any, 'captureScrollAnchor');

    el.transport = serverTransport(logPage(['n1', 'n2']));
    await el.fetchData('recent', false, true, false);
    expect(capture).not.toHaveBeenCalled();

    (el as any).colIdxMap = COLS;
    (el as any).handleLiveRows([
      {
        shape: 'table',
        cols: { timestamp: '2026-06-01T00:00:00.000Z', latency_breakdown: 'p1', trace_id: 'p1', parent_id: '', kind: 'log', id: 'p1', duration: 0, start_time_ns: Date.parse('2026-06-01T00:00:00.000Z') * 1e6 },
      },
    ]);
    expect(capture).not.toHaveBeenCalled();
    expect(ids(el)).toContain('p1'); // and the pushed row actually landed
  });

  test('holds the buffer while the reader is scrolled away', async () => {
    const el = await withBuffer(400);
    (el as any).resumeLiveTailAtEdge();
    expect((el as any).recentDataToBeAdded).toHaveLength(2);
  });

  test('does nothing when live tail is off — a paused stream keeps its pill', async () => {
    const el = await withBuffer(0, { isLiveStreaming: false });
    (el as any).resumeLiveTailAtEdge();
    expect((el as any).recentDataToBeAdded).toHaveLength(2);
  });

  // Oldest-first inverts the edge: rows land at the BOTTOM, so scrollTop 0 is the far end of
  // the list, not the edge. The predicate itself was covered; this path was not, because it
  // reads a container, and under jsdom's zero heights every container looks pinned to the
  // bottom — a flipped component test would have passed whatever the code did.
  test('oldest-first flushes at the bottom, not at the top', async () => {
    const atTop = await withBuffer(0, { flipDirection: true });
    (atTop as any).resumeLiveTailAtEdge();
    expect((atTop as any).recentDataToBeAdded).toHaveLength(2); // top is the far end when flipped

    const atBottom = await withBuffer(4500, { flipDirection: true }); // 4500 + 500 === scrollHeight
    (atBottom as any).resumeLiveTailAtEdge();
    expect((atBottom as any).recentDataToBeAdded).toHaveLength(0);
  });

  test('oldest-first buffers unless pinned to the bottom', () => {
    expect(shouldBufferRecent(true, 0, false, true)).toBe(true); // scrolled up → buffer
    expect(shouldBufferRecent(true, 4500, true, true)).toBe(false); // at the bottom → insert
    expect(atInsertionEdge(0, true, true)).toBe(true); // flipped ignores scrollTop entirely
    expect(atInsertionEdge(9999, false, true)).toBe(false);
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
  test('a newer full fetch supersedes an in-flight empty initial response', async () => {
    const el = await mountList();
    const tx = deferredTransport();
    el.transport = tx as any;

    // The component starts a head-preloaded request, then query/time setup emits
    // update-query before it completes. Previously the isLoading guard silently
    // discarded this second request.
    const initial = el.fetchData('initial-url', false, false, false);
    const current = el.fetchData('current-url', true, false, false);
    expect(tx.pending.map((p) => p.url)).toEqual(['initial-url', 'current-url']);

    tx.settle(1, treeFromLogs(['current-1']), { hasMore: false });
    await current;
    tx.settle(0, [], { hasMore: false, count: 0 });
    await initial;

    expect(ids(el)).toEqual(['current-1']);
    expect((el as any).isLoading).toBe(false);
  });

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


  // The "N new" pill is a promise about what clicking it does. It counted every buffered
  // EventLine — including collapsed trace children, which never become rows, and rows already
  // merged in, which mergeIntoTree drops — so it advertised "72 new" and inserted a fraction.
  test('the "N new" pill counts exactly the rows it inserts', async () => {
    const el = await mountList();
    (el as any).spanListTree = [row('onscreen')];
    (el as any).seenIds = new Set(['onscreen']);
    (el as any).updateVisibleItems();
    const collapsedChild = { ...row('child'), depth: 1, show: false, traceId: 'root' };

    (el as any).recentDataToBeAdded = (el as any).addWithFlipDirection([], [row('onscreen'), row('root'), collapsedChild], true);

    const promised = (el as any).recentCount;
    const before = (el as any).visibleItems.length;
    (el as any).handleRecentConcatenation();

    expect(promised).toBe(1);
    expect((el as any).visibleItems.length - before).toBe(promised);
  });

  // A row carries a `type` of its own ('log' | 'span'), so keying on `'type' in item` would
  // give every event the same key — the virtualizer would recycle rows onto the wrong data.
  test('virtual rows are keyed by identity, not by list position', async () => {
    expect(virtualItemKey(row('abc'))).toBe('abc');
    expect(virtualItemKey({ type: 'loadMore' })).toBe('loadMore');
    expect(virtualItemKey({ type: 'aggregateChildren', parentKey: 'k' })).toBe('aggregateChildren:k');
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
