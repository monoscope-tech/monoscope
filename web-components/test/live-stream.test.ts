import { describe, test, expect, vi, afterEach } from 'vitest';
import { LiveStream, tableRowToArray } from '../src/live-stream';
import { mountList, COLS, ids, row, fakeLiveTransport, stubContainer, serverTransport, logPage } from './log-list-harness';

// Live mode is a server push, not a poll — polling could never beat TimeFusion's
// write-visibility latency, since a row has to clear its ingest batch and land before any
// query returns it. These cover the two seams that carries: the shared connection lifecycle,
// and the point where a pushed row becomes a rendered table row.
//
// Deliberately few and high-level. The row's journey through grouping, dedup and scroll
// anchoring is the *same* `groupSpans`/`mergeIntoTree` path a fetch uses and is covered by the
// pagination tests; the only new risk is the boundary where pushed rows enter it.

// The registration endpoint + EventSource stand-in lives in the harness, so these tests and
// the component-level ones observe live connections through the same seam.
let live: ReturnType<typeof fakeLiveTransport>;
const withFakes = (registerBody: any, status = 200) => (live = fakeLiveTransport(registerBody, status)).calls;

afterEach(() => {
  vi.useRealTimers();
  live?.restore();
});

describe('LiveStream lifecycle', () => {
  test('registers, streams rows, and reports server drops', async () => {
    withFakes({ subscription_id: 's1', stream_url: '/stream/s1', expires_at: new Date(Date.now() + 45000).toISOString() });
    const rows: unknown[][] = [];
    let dropped = 0;
    let state = '';
    const s = new LiveStream({
      projectId: 'p1',
      leaseSecs: 45,
      body: () => ({ all_signals: true }),
      onRows: r => rows.push(r),
      onDropped: n => (dropped = n),
      onState: st => (state = st),
    });
    await s.start();

    live.last!.emit('ready', {});
    expect(state).toBe('live');

    live.last!.emit('log', [{ shape: 'table', cols: { kind: 'log' } }]);
    expect(rows).toHaveLength(1);

    // Dropping under load is expected for Events — it has no service gate — so the count
    // reaching the UI is the thing that must not silently break.
    live.last!.emit('dropped', { count: 42 });
    expect(dropped).toBe(42);

    s.stop();
    expect(live.openCount()).toBe(0);
  });

  test('surfaces the server refusal instead of a generic message', async () => {
    // The server names the actual cause (service gate, bad filter, limit hit); replacing it
    // with our own text would hide which of those the user has to fix.
    withFakes({ error: 'Select a service before starting the tail.' }, 400);
    let detail = '';
    const s = new LiveStream({
      projectId: 'p1',
      leaseSecs: 45,
      body: () => ({}),
      onRows: () => {},
      onState: (_st, d) => (detail = d ?? ''),
    });
    await s.start();
    expect(detail).toBe('Select a service before starting the tail.');
    expect(s.isRunning).toBe(false);
  });

  test('a server notice stops the stream instead of retrying a permanent fault', async () => {
    // The server's own voice, distinct from a transport blip. It arrives as `notice` rather
    // than `error` precisely so it does NOT land on EventSource's built-in error handler —
    // which retries, and retrying a filter that no longer compiles is an infinite loop that
    // looks to the user exactly like a quiet service.
    withFakes({ subscription_id: 's1', stream_url: '/stream/s1' });
    let state = '';
    let detail = '';
    const s = new LiveStream({
      projectId: 'p1',
      leaseSecs: 45,
      body: () => ({}),
      onRows: () => {},
      onState: (st, d) => {
        state = st;
        detail = d ?? '';
      },
    });
    await s.start();
    live.last!.emit('notice', { message: "This live tail's filter is no longer valid." });
    expect(state).toBe('error');
    expect(detail).toContain('no longer valid');
    expect(s.isRunning).toBe(false);
  });

  test('reports connecting, live and reconnecting as the connection moves through them', async () => {
    // The states are the only thing telling an on-call engineer whether an empty tail means
    // "nothing is happening" or "you are not connected". A silent transition is a lie.
    vi.useFakeTimers();
    withFakes({ subscription_id: 's1', stream_url: '/stream/s1' });
    const seen: string[] = [];
    const s = new LiveStream({
      projectId: 'p1',
      leaseSecs: 45,
      body: () => ({}),
      onRows: () => {},
      onState: st => seen.push(st),
    });
    await s.start();
    live.last!.emit('ready', {});
    // A transport failure is a blip: it must reconnect, not surface as a permanent error.
    live.last!.onerror!();
    expect(seen).toEqual(['connecting', 'live', 'reconnecting']);
    s.stop();
  });

  test('an expired lease stops the stream rather than reconnecting into a gap', async () => {
    // Reconnecting would look like an unbroken stream while rows were in fact missed.
    withFakes({ subscription_id: 's1', stream_url: '/stream/s1' });
    let state = '';
    const s = new LiveStream({
      projectId: 'p1',
      leaseSecs: 45,
      body: () => ({}),
      onRows: () => {},
      onState: st => (state = st),
    });
    await s.start();
    live.respond({}, 404); // the renew call now 404s
    await s.renew();
    expect(state).toBe('expired');
    expect(s.isRunning).toBe(false);
  });

  // The browser holds the lease alive, so a renewal that never fires is a tail that dies at
  // the lease length with no error — the failure mode the whole schedule exists to prevent.
  test('renews at a third of the lease, driven by the server expiry rather than a constant', async () => {
    vi.useFakeTimers();
    vi.setSystemTime(new Date('2026-06-01T00:00:00.000Z'));
    const calls = withFakes({ subscription_id: 's1', stream_url: '/stream/s1', expires_at: '2026-06-01T00:00:45.000Z' });
    const s = new LiveStream({ projectId: 'p1', leaseSecs: 999, body: () => ({}), onRows: () => {}, onState: () => {} });
    await s.start();

    // Server said 45s, so renewal is due at 15s — not at leaseSecs, which is the fallback only.
    await vi.advanceTimersByTimeAsync(14_000);
    expect(calls.some(c => c.url.includes('/renew'))).toBe(false);
    await vi.advanceTimersByTimeAsync(2_000);
    expect(calls.filter(c => c.url.includes('/renew'))).toHaveLength(1);
    s.stop();
  });

  test('a short or skewed lease cannot become a renewal storm', async () => {
    vi.useFakeTimers();
    vi.setSystemTime(new Date('2026-06-01T00:00:00.000Z'));
    // Already expired by the server's clock: a third of "remaining" would be negative.
    const calls = withFakes({ subscription_id: 's1', stream_url: '/stream/s1', expires_at: '2026-05-31T23:59:00.000Z' });
    const s = new LiveStream({ projectId: 'p1', leaseSecs: 45, body: () => ({}), onRows: () => {}, onState: () => {} });
    await s.start();
    await vi.advanceTimersByTimeAsync(4_000);
    expect(calls.filter(c => c.url.includes('/renew'))).toHaveLength(0); // floored at 5s, not immediate
    await vi.advanceTimersByTimeAsync(2_000);
    expect(calls.filter(c => c.url.includes('/renew'))).toHaveLength(1);
    s.stop();
  });

  // A fixed retry against a broken backend is a self-inflicted load test; the point of the
  // ladder is that a long outage costs progressively fewer attempts.
  test('reconnect backs off further on each successive failure', async () => {
    vi.useFakeTimers();
    withFakes({ subscription_id: 's1', stream_url: '/stream/s1' });
    const s = new LiveStream({ projectId: 'p1', leaseSecs: 45, body: () => ({}), onRows: () => {}, onState: () => {} });
    await s.start();

    const fail = async () => {
      live.last!.onerror!();
      await vi.advanceTimersByTimeAsync(0);
    };
    const reconnectsAfter = async (ms: number) => {
      const before = live.last;
      await vi.advanceTimersByTimeAsync(ms - 1);
      const early = live.last === before;
      await vi.advanceTimersByTimeAsync(2);
      return early && live.last !== before;
    };

    await fail();
    expect(await reconnectsAfter(1000)).toBe(true); // first retry is prompt
    await fail();
    expect(await reconnectsAfter(2000)).toBe(true); // then it steps back
    s.stop();
  });

  test('stopping during registration cannot reopen the obsolete stream', async () => {
    withFakes({ subscription_id: 's1', stream_url: '/stream/s1' });
    const immediateFetch = globalThis.fetch;
    let release!: (response: any) => void;
    globalThis.fetch = ((url: string, init?: any) =>
      init?.method === 'POST' && url.endsWith('/subscriptions')
        ? new Promise((resolve) => (release = resolve))
        : immediateFetch(url, init)) as any;
    const s = new LiveStream({ projectId: 'p1', leaseSecs: 45, body: () => ({}), onRows: () => {}, onState: () => {} });

    const starting = s.start();
    await Promise.resolve();
    s.stop();
    release({ ok: true, status: 200, json: async () => ({ subscription_id: 's1', stream_url: '/stream/s1' }) });
    await starting;

    expect(s.isRunning).toBe(false);
    expect(live.openCount()).toBe(0);
  });

  test('stopping releases the lease instead of leaving it to expire on the ingest pods', async () => {
    const calls = withFakes({ subscription_id: 's1', stream_url: '/stream/s1' });
    const s = new LiveStream({ projectId: 'p1', leaseSecs: 45, body: () => ({}), onRows: () => {}, onState: () => {} });
    await s.start();
    s.stop();
    expect(calls.some(c => c.method === 'DELETE' && c.url.endsWith('/subscriptions/s1'))).toBe(true);
    expect(live.last!.closed).toBe(true);
  });
});

describe('pushed Events rows', () => {
  test('changing the query replaces the live subscription before old-query rows can leak in', async () => {
    window.history.replaceState({}, '', '/log_explorer?query=service%3D%3D%22old%22');
    const el = await mountList();
    el.transport = serverTransport(logPage(['old-row']));
    await el.fetchData('old', true);
    live = fakeLiveTransport();

    try {
      (el as any).isLiveStreaming = true;
      await (el as any).startLiveStream();
      expect(live.calls.filter((c) => c.method === 'POST').map((c) => c.body.query)).toEqual(['service=="old"']);

      window.history.replaceState({}, '', '/log_explorer?query=service%3D%3D%22new%22');
      el.transport = serverTransport(logPage(['new-row']));
      await el.fetchData('new', true);
      await Promise.resolve();
      await Promise.resolve();

      expect(live.openCount()).toBe(1);
      expect(live.calls.filter((c) => c.method === 'POST').map((c) => c.body.query)).toEqual(['service=="old"', 'service=="new"']);
      expect(live.sources[0].closed).toBe(true);
    } finally {
      el.remove();
    }
  });

  test('positions columns by name and leaves unresolvable ones empty', () => {
    // The server sends only what it could resolve from the in-memory record; a column that
    // only SQL could compute is absent and must render blank, not shift the other columns.
    const out = tableRowToArray({ kind: 'log', id: 'x' }, COLS as any);
    expect(out[COLS.kind]).toBe('log');
    expect(out[COLS.id]).toBe('x');
    expect(out[COLS.duration]).toBeNull();
    expect(out).toHaveLength(Object.keys(COLS).length);
  });

  test('a pushed row reaches the rendered tree through the normal merge path', async () => {
    const el = await mountList();
    (el as any).colIdxMap = COLS;
    (el as any).isLiveStreaming = true;

    const before = (el as any).spanListTree.length;
    (el as any).handleLiveRows([
      {
        shape: 'table',
        cols: { id: 'live-1', trace_id: 'live-1', kind: 'log', timestamp: new Date().toISOString(), latency_breakdown: 'live-1' },
      },
    ]);

    // Either merged straight in or buffered behind the "N new" pill — both are the real
    // recent-fetch behaviour, and which one depends on scroll position. What matters is that
    // the row was accepted rather than dropped at the boundary.
    const after = (el as any).spanListTree.length + (el as any).recentDataToBeAdded.length;
    expect(after).toBeGreaterThan(before);
  });

  test('the new-row pill counts a pushed trace batch as one collapsed row', async () => {
    const el = await mountList();
    (el as any).colIdxMap = COLS;
    (el as any).isLiveStreaming = true;
    Object.defineProperty(el, 'logsContainer', {
      value: stubContainer({ scrollTop: 400, scrollHeight: 5000 }),
    });
    const timestamp = new Date().toISOString();
    const pushed = (id: string, parent_id: string | null) => ({
      shape: 'table',
      cols: { id, latency_breakdown: id, trace_id: 'trace-1', parent_id, kind: 'span', timestamp },
    });

    (el as any).handleLiveRows([pushed('root', null), pushed('child-1', 'root'), pushed('child-2', 'child-1')]);

    expect((el as any).recentDataToBeAdded).toHaveLength(3);
    expect((el as any).recentCount).toBe(1);
  });

  test('rows arriving while the list sits at the newest edge do not re-anchor the viewport', async () => {
    // At the top, the row under the user's eye is the one being pushed down on purpose.
    // Anchoring there scrolled the previous top row back into view on every tick — the list
    // visibly bounced, and the rows just streamed in were pushed out of sight.
    const el = await mountList();
    (el as any).colIdxMap = COLS;
    (el as any).isLiveStreaming = true;
    (el as any).spanListTree = [row('already-read')];
    (el as any).seenIds = new Set(['already-read']);
    (el as any).updateVisibleItems();
    Object.defineProperty(el, 'logsContainer', {
      value: stubContainer({ clientHeight: 100 }),
    });
    const restore = vi.spyOn(el as any, 'restoreScrollAnchor').mockResolvedValue(undefined);

    (el as any).handleLiveRows([
      { shape: 'table', cols: { id: 'live-1', trace_id: 'live-1', kind: 'log', timestamp: new Date().toISOString(), latency_breakdown: 'live-1' } },
    ]);

    expect(ids(el)).toContain('live-1');
    expect(restore).not.toHaveBeenCalled();
  });

  test('ignores a row shape it does not understand', async () => {
    // Ingest and web pods roll separately, so a pod can briefly see the other version's
    // envelope. Dropping it must not throw and must not corrupt the table.
    const el = await mountList();
    (el as any).colIdxMap = COLS;
    const before = (el as any).spanListTree.length + (el as any).recentDataToBeAdded.length;
    expect(() => (el as any).handleLiveRows([{ shape: 'log', log: { body: 'not for this table' } }])).not.toThrow();
    expect((el as any).spanListTree.length + (el as any).recentDataToBeAdded.length).toBe(before);
  });
});
