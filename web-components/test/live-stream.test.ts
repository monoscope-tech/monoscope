import { describe, test, expect, vi, afterEach } from 'vitest';
import { LiveStream, tableRowToArray } from '../src/live-stream';
import { mountList, COLS } from './log-list-harness';

// Live mode is a server push, not a poll — polling could never beat TimeFusion's
// write-visibility latency, since a row has to clear its ingest batch and land before any
// query returns it. These cover the two seams that carries: the shared connection lifecycle,
// and the point where a pushed row becomes a rendered table row.
//
// Deliberately few and high-level. The row's journey through grouping, dedup and scroll
// anchoring is the *same* `groupSpans`/`mergeIntoTree` path a fetch uses and is covered by the
// pagination tests; the only new risk is the boundary where pushed rows enter it.

// Minimal EventSource stand-in: records listeners so a test can push server frames.
class FakeEventSource {
  static last: FakeEventSource | null = null;
  listeners: Record<string, ((e: any) => void)[]> = {};
  onerror: (() => void) | null = null;
  closed = false;
  constructor(public url: string) {
    FakeEventSource.last = this;
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

const withFakes = (registerBody: any, status = 200) => {
  const calls: { url: string; method?: string }[] = [];
  (globalThis as any).EventSource = FakeEventSource;
  (globalThis as any).fetch = async (url: string, init?: any) => {
    calls.push({ url, method: init?.method });
    return { ok: status < 400, status, json: async () => registerBody };
  };
  return calls;
};

afterEach(() => {
  vi.useRealTimers();
  FakeEventSource.last = null;
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

    FakeEventSource.last!.emit('ready', {});
    expect(state).toBe('live');

    FakeEventSource.last!.emit('log', [{ shape: 'table', cols: { kind: 'log' } }]);
    expect(rows).toHaveLength(1);

    // Dropping under load is expected for Events — it has no service gate — so the count
    // reaching the UI is the thing that must not silently break.
    FakeEventSource.last!.emit('dropped', { count: 42 });
    expect(dropped).toBe(42);

    s.stop();
    expect(FakeEventSource.last!.closed).toBe(true);
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
    withFakes({}, 404); // the renew call now 404s
    await s.renew();
    expect(state).toBe('expired');
    expect(s.isRunning).toBe(false);
  });
});

describe('pushed Events rows', () => {
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
