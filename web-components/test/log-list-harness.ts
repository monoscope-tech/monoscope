// Shared harness for high-level LogList behavior tests: mount the real component,
// feed canned {tree, meta} via the transport seam (no Web Worker / network), and
// drive fetchData / events as a user would. See web_components_test_harness memory.
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
const idToTs = (id: string) => new Date(TS_BASE - (/^\d+$/.test(id) ? Number(id) : [...id].reduce((a, c) => a + c.charCodeAt(0), 0)) * 1000).toISOString();
const startNs = (ts: string | number) => (typeof ts === 'number' ? ts : Date.parse(ts) * 1e6);

// One server-shaped standalone log row (kind='log'). Positional, in COLS order.
// Internal to the page builders below — tests construct fixtures via logPage.
const logRow = (id: string, ts: string | number = idToTs(id)): any[] => [ts, id, id, '', 'log', id, 0, startNs(ts)];
// The trace-adjacency entry the server emits for a standalone log (its own trace).
const logTrace = (id: string, ts: string | number = idToTs(id)) => ({ trace_id: id, start_time: startNs(ts), duration: 0, trace_start_time: typeof ts === 'string' ? ts : null, root: id, children: {} });

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

// Queues canned pages; replaces the worker transport. Records requested urls.
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
// Groups newest-first by default; pass a leading `true` to match a component
// with flipDirection set (e.g. `serverTransport(true, page1, page2)`), else its
// tree order won't match what the component renders.
export const serverTransport = (...pages: any[]) => {
  const flipDirection = typeof pages[0] === 'boolean' ? (pages.shift() as boolean) : false;
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
        colIdxMap, count: d.count ?? 0, traces, hasMore: d.hasMore ?? n > 0, queryResultCount: n, // worker: empty→false, full→true
      }),
    };
  };
  return Object.assign(fn, { urls });
};

// Transport whose responses are resolved manually, to test out-of-order / concurrent fetches.
export const deferredTransport = () => {
  const pending: Array<{ resolve: (v: any) => void; url: string }> = [];
  const fn = (url: string) => new Promise<any>((resolve) => pending.push({ url, resolve }));
  // settle(index, {tree, metaOverrides})
  const settle = (i: number, tree: any[], over: any = {}) =>
    pending[i].resolve({ tree, meta: meta({ ...over, queryResultCount: tree.length }) });
  return Object.assign(fn, { pending, settle });
};

export const ids = (el: LogList) => (el as any).spanListTree.map((r: any) => r.id);

// Mount with the mount-time auto-fetch disabled so tests drive fetchData explicitly.
export const mountList = async (props: Partial<LogList> = {}) => {
  const el = new LogList();
  (el as any).fetchInitialData = async () => {};
  Object.assign(el, props);
  document.body.appendChild(el);
  await el.updateComplete;
  return el;
};

// Stub global fetch with a queue of JSON bodies (for paths that bypass transport,
// e.g. aggregate-children expand). Returns a restore fn.
export const stubFetch = (...bodies: any[]) => {
  const q = [...bodies];
  const orig = globalThis.fetch;
  (globalThis as any).fetch = async () => ({ ok: true, status: 200, json: async () => q.shift() ?? {} });
  return () => { (globalThis as any).fetch = orig; };
};
