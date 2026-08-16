// The real transport, which every other log-list test replaces.
//
// Those tests inject a fake `transport` so they can drive merge/scroll/pagination
// deterministically — which means the production path they stand in for has no coverage
// at all. It has three branches (server-preloaded rows, the head's early-fetch promise,
// and the Web Worker) plus request correlation and a 2-minute timeout, and a fault in any
// of them shows up as a list that never loads.
import { describe, test, expect, vi, beforeEach, afterEach } from 'vitest';
import { mountList, logPage, COLS } from './log-list-harness';

// One server-shaped page, in the wire format each branch receives — built by the same
// helper the rest of the suite uses, so it carries real trace adjacency.
const PAGE = logPage(['a'], {
  cols: Object.keys(COLS),
  serviceColors: {},
  nextUrl: '/next',
  recentUrl: '/recent',
  count: 1,
  hasMore: true,
  queryResultCount: 1,
});

// The worker as the component uses it: capture what is posted, reply on demand.
const fakeWorker = () => {
  const posted: any[] = [];
  let instance: any;
  class FakeWorker {
    onmessage: ((e: any) => void) | null = null;
    onerror: ((e: any) => void) | null = null;
    constructor() {
      instance = this;
    }
    postMessage(msg: any) {
      posted.push(msg);
    }
    terminate() {}
    addEventListener() {}
    removeEventListener() {}
  }
  const original = globalThis.Worker;
  (globalThis as any).Worker = FakeWorker;
  return {
    posted,
    reply: (data: any) => instance.onmessage?.({ data }),
    restore: () => ((globalThis as any).Worker = original),
  };
};

// A list with the real transport in place (mountList only disables the auto-fetch).
const realTransport = async (props: Record<string, any> = {}) => {
  const el = await mountList(props as any);
  return (url: string) => (el as any).transport(url) as Promise<any>;
};

beforeEach(() => {
  delete (window as any).logDataPromise;
  delete (window as any).logsPreloadedData;
});
afterEach(() => vi.restoreAllMocks());

describe('the head early-fetch promise', () => {
  // The server starts the row request in <head> so it is in flight before the component
  // upgrades. Taking that result is what makes first paint fast.
  test('is consumed instead of asking the worker', async () => {
    const worker = fakeWorker();
    (window as any).logDataPromise = Promise.resolve(PAGE);
    const fetchOnce = await realTransport();

    const { tree, meta } = await fetchOnce('/p/x/log_explorer/data');

    expect(tree.map((r: any) => r.id)).toEqual(['a']);
    expect(meta.nextUrl).toBe('/next');
    expect(worker.posted).toHaveLength(0);
    worker.restore();
  });

  // It answers exactly one request; a second read would replay the first page and the
  // list would never advance past it.
  test('is used once, then the worker takes over', async () => {
    const worker = fakeWorker();
    (window as any).logDataPromise = Promise.resolve(PAGE);
    const fetchOnce = await realTransport();
    await fetchOnce('/first');

    void fetchOnce('/second');
    await Promise.resolve();

    expect((window as any).logDataPromise).toBeNull();
    expect(worker.posted.map((m) => m.url)).toEqual(['/second']);
    worker.restore();
  });

  // Otherwise the reader waits out the full 2-minute worker timeout and is then shown
  // "Worker timeout" instead of the query error that actually happened.
  test('a server error surfaces immediately rather than as a timeout', async () => {
    const worker = fakeWorker();
    (window as any).logDataPromise = Promise.resolve({ error: 'unknown field: srvice.name' });
    const fetchOnce = await realTransport();

    await expect(fetchOnce('/p/x/log_explorer/data')).rejects.toThrow(/srvice\.name/);
    expect(worker.posted).toHaveLength(0);
    worker.restore();
  });
});

describe('the worker path', () => {
  test('posts the request with the state the worker needs to group rows', async () => {
    const worker = fakeWorker();
    const fetchOnce = await realTransport({ flipDirection: true });

    void fetchOnce('/p/x/log_explorer/data?cursor=1');

    expect(worker.posted[0]).toMatchObject({ type: 'fetch', url: '/p/x/log_explorer/data?cursor=1', flipDirection: true });
    expect(typeof worker.posted[0].id).toBe('number');
    worker.restore();
  });

  test('resolves the request that its reply is addressed to', async () => {
    const worker = fakeWorker();
    const fetchOnce = await realTransport();
    const first = fetchOnce('/one');
    const second = fetchOnce('/two');
    const [idOne, idTwo] = worker.posted.map((m) => m.id);

    // Reply out of order: the correlation id, not arrival order, decides.
    worker.reply({ type: 'success', id: idTwo, tree: [{ id: 'two' }], meta: {} });
    worker.reply({ type: 'success', id: idOne, tree: [{ id: 'one' }], meta: {} });

    expect((await first).tree).toEqual([{ id: 'one' }]);
    expect((await second).tree).toEqual([{ id: 'two' }]);
    worker.restore();
  });

  test('a failure reply rejects that request with the worker\'s message', async () => {
    const worker = fakeWorker();
    const fetchOnce = await realTransport();
    const request = fetchOnce('/boom');

    worker.reply({ type: 'error', id: worker.posted[0].id, error: 'upstream exploded' });

    await expect(request).rejects.toThrow('upstream exploded');
    worker.restore();
  });

  // A reply for a request that already settled (or never existed) must be ignored, not
  // resolve someone else's promise.
  test('an unknown correlation id is ignored', async () => {
    const worker = fakeWorker();
    const fetchOnce = await realTransport();
    const request = fetchOnce('/one');

    worker.reply({ type: 'success', id: 9999, tree: [{ id: 'stray' }], meta: {} });
    worker.reply({ type: 'success', id: worker.posted[0].id, tree: [{ id: 'one' }], meta: {} });

    expect((await request).tree).toEqual([{ id: 'one' }]);
    worker.restore();
  });

  test('a request that never gets a reply times out rather than hanging forever', async () => {
    vi.useFakeTimers();
    const worker = fakeWorker();
    const fetchOnce = await realTransport();
    const request = fetchOnce('/silent');
    const settled = expect(request).rejects.toThrow(/timeout/i);

    vi.advanceTimersByTime(120_000);
    await settled;

    worker.restore();
    vi.useRealTimers();
  });

  test('a timed-out request is forgotten, so a late reply cannot resolve it', async () => {
    vi.useFakeTimers();
    const worker = fakeWorker();
    const fetchOnce = await realTransport();
    const request = fetchOnce('/late');
    const settled = expect(request).rejects.toThrow(/timeout/i);
    vi.advanceTimersByTime(120_000);
    await settled;

    // The reply arrives after the fact; it must find no callback and do nothing.
    expect(() => worker.reply({ type: 'success', id: worker.posted[0].id, tree: [], meta: {} })).not.toThrow();

    worker.restore();
    vi.useRealTimers();
  });
});
