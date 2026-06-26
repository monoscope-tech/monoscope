// Shared harness for high-level LogList behavior tests: mount the real component,
// feed canned {tree, meta} via the transport seam (no Web Worker / network), and
// drive fetchData / events as a user would. See web_components_test_harness memory.
import { LogList } from '../src/log-list';

// Minimal EventLine-ish row; only fields the merge/render path touches.
export const row = (id: string, data: any[] = [id]) => ({
  id, data, depth: 0, children: 0, traceId: id, parentIds: [],
  show: true, expanded: false, isLastChild: true, siblingsArr: [],
  childErrors: false, hasErrors: false, isNew: false,
  startNs: 0, duration: 0, traceStart: 0, traceEnd: 0, childrenTimeSpans: [], type: 'log' as const,
});

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
