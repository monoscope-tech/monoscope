// Shared harness for high-level LogList behavior tests: mount the real component,
// feed canned {tree, meta} via the transport seam (no Web Worker / network), and
// drive fetchData / events as a user would. See web_components_test_harness memory.
import { render, html } from 'lit';
import { LogList } from '../src/log-list';
import { groupSpans } from '../src/log-worker-functions';

// Minimal EventLine-ish row; only fields the merge/render path touches.
// Prefer the server-shaped helpers below (logPage / treeFromLogs) for behavior
// tests — this is for the few lifecycle tests that inject tree state directly.
export const row = (id: string, data: any[] = [id]) => ({
  id, data, depth: 0, children: 0, traceId: id, parentIds: [],
  show: true, expanded: false, isLastChild: true, siblingsArr: [],
  childErrors: false, hasErrors: false, isNew: false,
  startNs: 0, duration: 0, traceStart: 0, traceEnd: 0, childrenTimeSpans: [], type: 'log' as const,
});

// ── Server-shaped fixtures ───────────────────────────────────────────────────
// The columns the server emits (subset groupSpans keys off). Row arrays below are
// indexed by this map, exactly like the JSON the log_explorer endpoint returns.
export const COLS = { timestamp: 0, latency_breakdown: 1, trace_id: 2, parent_id: 3, kind: 4, id: 5, duration: 6, start_time_ns: 7 } as const;

// Deterministic per-id timestamp: numeric ids sort newest-first (id "1" newest),
// and the SAME id maps to the SAME time across pages — so overlapping-page dedup
// behaves as it does against the real server.
const TS_BASE = Date.parse('2024-01-01T00:00:00.000Z'); // stable past anchor
// Position-weighted char sum so permutation ids (e.g. 'a2' vs 'b1') don't collide to the same time.
const idToTs = (id: string) => new Date(TS_BASE - (/^\d+$/.test(id) ? Number(id) : [...id].reduce((a, c, i) => a + c.charCodeAt(0) * (i + 1), 0)) * 1000).toISOString();
// Fixture-only: numeric input is taken as already-nanoseconds (the server's start_time_ns),
// strings as ISO → ns. Deliberately NOT tsToMs (which detects unit + converts to ms).
const startNs = (ts: string | number) => (typeof ts === 'number' ? ts : Date.parse(ts) * 1e6);
// numeric input is ns (same convention as startNs); strings pass through as ISO.
const tsIso = (ts: string | number) => (typeof ts === 'string' ? ts : new Date(ts / 1e6).toISOString());

// One server-shaped standalone log row (kind='log'). Positional, in COLS order.
// Internal to the page builders below — tests construct fixtures via logPage.
const logRow = (id: string, ts: string | number = idToTs(id)): any[] => [ts, id, id, '', 'log', id, 0, startNs(ts)];
// The trace-adjacency entry the server emits for a standalone log (its own trace).
const logTrace = (id: string, ts: string | number = idToTs(id)) => ({ trace_id: id, start_time: startNs(ts), duration: 0, trace_start_time: tsIso(ts), root: id, children: {} });

type RowSpec = string | [string, string | number]; // id, or [id, explicit-timestamp]
const norm = (r: RowSpec): [string, string | number] => (Array.isArray(r) ? r : [r, idToTs(r)]);

// A server JSON page of standalone logs (newest-first). `over` sets meta fields
// (nextUrl, recentUrl, hasMore, cols, count, …) the way a real response would.
export const logPage = (rows: RowSpec[], over: any = {}) => {
  const specs = rows.map(norm);
  return { logsData: specs.map((s) => logRow(...s)), colIdxMap: COLS, traces: specs.map((s) => logTrace(...s)), ...over };
};
// The flattened tree groupSpans produces for those rows — for deferredTransport.settle.
// flipDirection defaults newest-first; pass true to match a component with flipDirection set.
export const treeFromLogs = (rows: RowSpec[], flipDirection = false) => { const p = logPage(rows); return groupSpans(p.logsData, p.colIdxMap as any, {}, flipDirection, p.traces); };

export const meta = (over: any = {}) => ({
  serviceColors: {}, nextUrl: '', recentUrl: '', cols: ['id'], colIdxMap: { id: 0 },
  count: 100, traces: [], hasMore: true, queryResultCount: 0, ...over,
});

// Queues canned pre-built trees; replaces the worker transport. Records requested urls.
// Injects the tree directly (bypasses groupSpans) — use for lifecycle/state tests only.
// For behavior tests that depend on grouping/cursor logic, use serverTransport instead.
export const fakeTransport = (...pages: { tree: any[]; meta?: any }[]) => {
  const q = [...pages];
  const urls: string[] = [];
  const fn = async (url: string) => {
    urls.push(url);
    const p = q.shift() ?? { tree: [], meta: {} };
    return { tree: p.tree, meta: meta({ ...p.meta, queryResultCount: p.tree.length }) };
  };
  return Object.assign(fn, { urls });
};

// A transport that mimics the production Web Worker (log-worker.ts): it takes
// server-shaped JSON pages (raw logsData arrays + colIdxMap + trace adjacency)
// and runs the REAL groupSpans to flatten them into the tree. Tests using it
// exercise the same fetch → group → merge → build-next-url pipeline the browser
// does, instead of hand-feeding a pre-built tree. Records the urls requested.
const makeServerTransport = (flipDirection: boolean, pages: any[]) => {
  const q = [...pages];
  const urls: string[] = [];
  const fn = async (url: string) => {
    urls.push(url);
    const d = q.shift() ?? { logsData: [] };
    const colIdxMap = d.colIdxMap ?? {};
    const traces = d.traces ?? [];
    const n = d.logsData?.length ?? 0;
    const tree = n ? groupSpans(d.logsData, colIdxMap, {}, flipDirection, traces) : [];
    return {
      tree,
      meta: meta({
        nextUrl: d.nextUrl ?? '', recentUrl: d.recentUrl ?? '', cols: d.cols ?? Object.keys(colIdxMap),
        // Mirrors the worker: empty page → false, full page → true. So a single
        // full page leaves hasMore=true; pass { hasMore: false } to fire expandTimeRange.
        colIdxMap, count: d.count ?? 0, traces, hasMore: d.hasMore ?? n > 0, queryResultCount: n,
      }),
    };
  };
  return Object.assign(fn, { urls });
};
// Groups newest-first (matches a component with flipDirection unset); use
// serverTransportFlipped for a flipDirection=true component so the tree order matches.
export const serverTransport = (...pages: any[]) => makeServerTransport(false, pages);
export const serverTransportFlipped = (...pages: any[]) => makeServerTransport(true, pages);

// Transport whose responses are resolved manually, to test out-of-order / concurrent fetches.
export const deferredTransport = () => {
  const pending: Array<{ resolve: (v: any) => void; url: string }> = [];
  const fn = (url: string) => new Promise<any>((resolve) => pending.push({ url, resolve }));
  // settle(index, tree, metaOverrides?)
  const settle = (i: number, tree: any[], over: any = {}) =>
    pending[i].resolve({ tree, meta: meta({ ...over, queryResultCount: tree.length }) });
  return Object.assign(fn, { pending, settle });
};

export const ids = (el: LogList) => (el as any).spanListTree.map((r: any) => r.id);

// updateTimePicker is installed by main.ts in the real app; expandTimeRangeUrl
// calls it to sync the picker label. Default no-op stub — tests that assert on
// it (log-list-chart-anchor) install their own vi.fn over this.
(window as any).updateTimePicker ??= () => '';

// Mount with the mount-time auto-fetch disabled so tests drive fetchData explicitly.
export const mountList = async (props: Partial<LogList> = {}) => {
  const el = new LogList();
  (el as any).fetchInitialData = async () => {};
  Object.assign(el, props);
  document.body.appendChild(el);
  await el.updateComplete;
  return el;
};

// ── Scroll / layout simulation ───────────────────────────────────────────────
// jsdom computes no layout, so until now every scroll-anchoring test had to mock
// captureScrollAnchor/restoreScrollAnchor — i.e. mock out the exact code that
// decides where the user ends up after a page merge. Those tests could not have
// caught "clicking Show earlier events throws you back to the top".
//
// This models the geometry the real component reads: dense fixed-height rows, a
// fixed viewport, a scrollTop the browser clamps to the content, and a virtualizer
// that only mounts rows inside its overhang runway. `keyed(virtualizerEpoch)`
// destroys and recreates that virtualizer, and a fresh one reports a zero-height
// scroll range for a frame — which is what clamps the user's scrollTop to 0. That
// frame is reproduced here because it is the bug.
export const ROW_H = 28; // DenseRowFlowLayout._itemSize.height
const OVERHANG = 200; // DenseRowFlowLayout._overhang

export type ScrollSim = ReturnType<typeof scrollHarness>;

export const scrollHarness = (el: LogList, { viewportHeight = 560, virtualizerScrollToIndexWorks = true } = {}) => {
  let scrollTop = 0;
  let sentinelHost: HTMLElement | null = null;
  // A keyed remount swaps in a virtualizer that has not laid out yet: no rows
  // mounted, zero scroll range. Cleared when it lays out (rAF / layoutComplete).
  let remounting = false;
  const items = () => (el as any).virtualListItems as any[];
  const contentHeight = () => (remounting ? 0 : items().length * ROW_H);
  const maxScroll = () => Math.max(0, contentHeight() - viewportHeight);
  const clamp = (v: number) => Math.max(0, Math.min(maxScroll(), v));
  // The selector the component builds is `[data-row-id="<CSS.escape(id)>"]`.
  const idFromSelector = (sel: string) => sel.match(/\[data-row-id="(.*)"\]/)?.[1].replace(/\\(.)/g, '$1') ?? null;

  const rowEl = (i: number) => {
    const item = items()[i];
    const top = i * ROW_H - scrollTop;
    return { dataset: { rowId: item.id }, getBoundingClientRect: () => ({ top, bottom: top + ROW_H, height: ROW_H, left: 0, right: 0 }) };
  };
  // Only rows inside the runway exist in the DOM — the recycling the anchor
  // fallback path is there to survive.
  const mountedRows = () =>
    remounting
      ? []
      : items().reduce<any[]>((acc, item, i) => {
          const top = i * ROW_H - scrollTop;
          if ('id' in item && top + ROW_H > -OVERHANG && top < viewportHeight + OVERHANG) acc.push(rowEl(i));
          return acc;
        }, []);

  const container: any = {
    get scrollTop() { return scrollTop; },
    set scrollTop(v: number) { scrollTop = clamp(v); },
    get scrollHeight() { return contentHeight(); },
    clientHeight: viewportHeight,
    getBoundingClientRect: () => ({ top: 0, bottom: viewportHeight, height: viewportHeight, left: 0, right: 0 }),
    querySelectorAll: (sel: string) => (sel.includes('data-row-id') ? mountedRows() : []),
    querySelector: (sel: string) => mountedRows().find((r) => r.dataset.rowId === idFromSelector(sel)) ?? null,
    classList: { add() {}, remove() {} },
  };
  Object.defineProperty(el, 'logsContainer', { configurable: true, get: () => container });

  const settle = () => { remounting = false; };
  const virtualizer = {
    // lit-virtualizer 2.1 can leave scrollToIndex as a no-op after an external-scroller
    // remount. Tests can reproduce that real-browser failure instead of granting the
    // component a perfect API that hid the jump-to-edge bug.
    scrollToIndex: (i: number, _pos: string) => {
      settle();
      if (virtualizerScrollToIndexWorks) container.scrollTop = i * ROW_H;
    },
    get layoutComplete() { settle(); return Promise.resolve(); },
    querySelectorAll: (sel: string) => container.querySelectorAll(sel),
  };
  const origQuery = el.querySelector.bind(el);
  (el as any).querySelector = (sel: string) => (sel === 'lit-virtualizer' ? virtualizer : origQuery(sel));

  // The remount happens during Lit's update, before `updateComplete` resolves —
  // so a restore that awaits updateComplete always resumes on the collapsed frame.
  const origUpdated = (el as any).updated.bind(el);
  (el as any).updated = (changed: Map<string, any>) => {
    if (changed.has('virtualizerEpoch')) {
      remounting = true;
      scrollTop = 0; // the browser clamps to a zero-height scroll range
      requestAnimationFrame(settle); // the new virtualizer lays out on the next frame
    }
    origUpdated(changed);
  };

  return {
    container,
    virtualizer,
    get scrollTop() { return scrollTop; },
    get maxScroll() { return maxScroll(); },
    get atTop() { return scrollTop <= 0; },
    // Move the viewport the way a user does, then run what a real scroll runs.
    scrollTo(v: number) { container.scrollTop = v; el.handleListScroll?.(); },
    scrollToBottom() { this.scrollTo(maxScroll()); },
    // The virtualizer reports its rendered range; drive the same event it fires.
    emitVisibility() {
      const first = Math.max(0, Math.floor((scrollTop - OVERHANG) / ROW_H));
      const last = Math.min(items().length - 1, Math.ceil((scrollTop + viewportHeight + OVERHANG) / ROW_H));
      el.handleVisibilityChange({ first, last });
    },
    // Index of the first/last row the user can actually see (sentinels included).
    get firstVisibleIndex() { return Math.floor(scrollTop / ROW_H); },
    get lastVisibleIndex() { return Math.min(items().length - 1, Math.floor((scrollTop + viewportHeight - 1) / ROW_H)); },
    settle,
    // lit-virtualizer mounts nothing under jsdom, so the sentinel rows — and with
    // them the IntersectionObservers that auto-page the list — never existed in any
    // test. Render the real templates into a host so their refs resolve and their
    // observers register exactly as in the browser; `fireSentinel` then drives the
    // production callback rather than a re-implementation of it.
    async mountSentinels() {
      const host = sentinelHost ?? (sentinelHost = el.appendChild(document.createElement('tbody')));
      render(html`${(el as any).renderFetchRecentButton()}${(el as any).renderLoadMoreButton()}`, host);
      await new Promise((r) => requestAnimationFrame(r)); // observers are registered in a rAF
      const rows = [...host.querySelectorAll('tr')];
      return {
        host,
        recent: rows.find((r) => r.id === 'recent-logs') ?? null,
        loadMore: rows.find((r) => r.id !== 'recent-logs') ?? null,
      };
    },
  };
};

// Container / virtualizer stubs carrying every member the component touches during
// ordinary operation. Ad-hoc literals that omitted one (classList for the scroll-paint
// toggle, rects for anchoring, querySelectorAll for the blank-list watchdog) threw from
// timers *after* their test had finished — vitest reported them as unhandled errors, and
// a genuine failure could hide in that noise.
export const stubContainer = (over: Record<string, any> = {}) => ({
  scrollTop: 0,
  clientHeight: 500,
  scrollHeight: 1000,
  classList: { add() {}, remove() {} },
  getBoundingClientRect: () => ({ top: 0, bottom: 500, height: 500, left: 0, right: 0 }),
  querySelector: () => null,
  querySelectorAll: () => [],
  ...over,
});
// Reports one row inside the viewport by default, so the blank-list watchdog reads the
// list as healthy and does not nudge scrollTop out from under an assertion. Override
// querySelectorAll with [] to exercise the stuck-blank recovery itself.
export const stubVirtualizer = (over: Record<string, any> = {}) => ({
  scrollToIndex: () => {},
  layoutComplete: Promise.resolve(),
  querySelectorAll: () => [{ getBoundingClientRect: () => ({ top: 0, bottom: 28 }) }],
  ...over,
});

// Replace only the selectors you name, delegating everything else to the real DOM.
//
// The component renders into its light DOM, so `@query('#logs_list_container_inner')`
// goes through `el.querySelector` too — a blanket mock hands the virtualizer stub back
// as the scroll container, and the background blank-list watchdog then throws from a
// timer after the test has finished.
export const stubQuery = (el: LogList, stubs: Record<string, unknown>) => {
  const real = el.querySelector.bind(el);
  (el as any).querySelector = (sel: string) => (sel in stubs ? stubs[sel] : real(sel));
};

// The browser reporting a sentinel as visible. `el` narrows it to one sentinel.
export const fireSentinel = (el?: Element | null) => (globalThis as any).triggerIntersection(el ?? undefined);

// Let queued rAF callbacks and microtasks run — anchor restoration is async.
export const flushFrames = async (n = 3) => {
  for (let i = 0; i < n; i++) await new Promise((r) => requestAnimationFrame(r));
};

// ── Live push transport ──────────────────────────────────────────────────────
// Stand-in for the SSE seam: a stubbed registration endpoint plus an EventSource that
// records whether it is still open. Live tail is a server push, so "is this tab watching?"
// is answered by open connections, not by timers — tests assert on `openCount()`.
export const fakeLiveTransport = (registerBody: any = { subscription_id: 's1', stream_url: '/stream/s1' }, status = 200) => {
  const sources: FakeEventSource[] = [];
  const calls: { url: string; method?: string; body?: any }[] = [];
  class FakeEventSource {
    listeners: Record<string, ((e: any) => void)[]> = {};
    onerror: (() => void) | null = null;
    closed = false;
    constructor(public url: string) {
      sources.push(this);
    }
    addEventListener(type: string, fn: (e: any) => void) {
      (this.listeners[type] ??= []).push(fn);
    }
    close() {
      this.closed = true;
    }
    emit(type: string, data: unknown) {
      for (const fn of this.listeners[type] ?? []) fn({ data: JSON.stringify(data) });
    }
  }
  const orig = { fetch: globalThis.fetch, EventSource: (globalThis as any).EventSource };
  const install = () => {
    (globalThis as any).EventSource = FakeEventSource;
    (globalThis as any).fetch = async (url: string, init?: any) => {
      calls.push({ url, method: init?.method, body: init?.body ? JSON.parse(init.body) : undefined });
      return { ok: status < 400, status, json: async () => registerBody };
    };
  };
  install();
  return {
    calls,
    sources,
    get last() {
      return sources.at(-1) ?? null;
    },
    openCount: () => sources.filter((s) => !s.closed).length,
    // Change what the registration/renew endpoint answers mid-test (e.g. a lease that 404s).
    respond: (body: any, code = 200) => {
      registerBody = body;
      status = code;
      install();
    },
    restore: () => {
      (globalThis as any).fetch = orig.fetch;
      (globalThis as any).EventSource = orig.EventSource;
      sources.length = 0;
    },
  };
};

// Stub global fetch with a queue of JSON bodies (for paths that bypass transport,
// e.g. aggregate-children expand). Returns a restore fn.
export const stubFetch = (...bodies: any[]) => {
  const q = [...bodies];
  const orig = globalThis.fetch;
  (globalThis as any).fetch = async () => ({ ok: true, status: 200, json: async () => q.shift() ?? {} });
  return () => { (globalThis as any).fetch = orig; };
};
