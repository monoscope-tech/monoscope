import { describe, test, expect, afterEach } from 'vitest';
import '../src/live-tail';

// The Live Tail component owns exactly two things `LiveStream` does not: the capped row buffer
// and what pause means. Both are the kind of logic that looks obviously right and is wrong in a
// way nothing else notices — an uncapped buffer is an out-of-memory crash that only appears on
// a busy service, and a pause that stops the stream is backpressure the server has to absorb.
//
// The selectors filling from the real schema response is covered end to end in
// `e2e/tests/live-tail.spec.ts`, against the endpoint's actual shape. Here they are stubbed,
// so nothing in this file asserts the server's contract.

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

/** Registration bodies the component sent, so a reconnect can be compared with the first try. */
const posted: any[] = [];

const stubServer = () => {
  posted.length = 0;
  (globalThis as any).EventSource = FakeEventSource;
  (globalThis as any).fetch = async (url: string, init?: any) => {
    if (init?.body) posted.push(JSON.parse(init.body));
    return {
      ok: true,
      status: 200,
      json: async () =>
        url.includes('/schema')
          ? { fields: { 'resource.service.name': { examples: ['checkout'] }, 'resource.deployment.environment.name': { examples: ['prod'] } } }
          : { subscription_id: 's1', stream_url: '/stream/s1' },
    };
  };
};

/** Let pending fetches and the render they trigger finish. */
const settle = async () => {
  await new Promise(r => setTimeout(r, 0));
  await new Promise(r => setTimeout(r, 0));
};

const mount = async () => {
  stubServer();
  const el = document.createElement('live-tail') as any;
  el.dataset.projectId = 'p1';
  el.dataset.leaseSecs = '45';
  document.body.appendChild(el);
  await el.updateComplete;
  await Promise.resolve(); // let loadFacets settle
  await el.updateComplete;
  return el;
};

const logRow = (n: number) => ({
  id: `r${n}`,
  timestamp: '2024-01-01T00:00:00.000Z',
  level: 'info',
  service: 'checkout',
  trace_id: null,
  span_id: null,
  name: null,
  body: `row ${n}`,
  truncated: false,
});

/** Rows as they arrive over the wire — wrapped in the envelope's `log` shape. */
const pushed = (from: number, to: number) => Array.from({ length: to - from }, (_, i) => ({ shape: 'log', log: logRow(from + i) }));

afterEach(() => {
  document.body.innerHTML = '';
  FakeEventSource.last = null;
});

describe('Live Tail row buffer', () => {
  test('caps the buffer and counts what it dropped', async () => {
    // 1200 rows into a 1000-row cap. The count matters as much as the cap: rows vanishing with
    // no number attached is indistinguishable from a filter that stopped matching.
    const el = await mount();
    el.appendRows(pushed(0, 1200).map((r: any) => r.log));
    await el.updateComplete;

    expect(el.rows).toHaveLength(1000);
    expect(el.droppedClient).toBe(200);
    // Drops the oldest, so the live edge is what survives — showing the start of a burst while
    // discarding what is happening now would be worse than showing fewer rows.
    expect(el.rows[el.rows.length - 1].body).toBe('row 1199');
  });

  test('pause freezes the display without stopping the stream', async () => {
    // The distinction the whole design rests on. If pausing stopped delivery, the server would
    // have to buffer for us — which is the unbounded queue this feature exists to avoid.
    const el = await mount();
    el.appendRows(pushed(0, 3).map((r: any) => r.log));
    await el.updateComplete;
    el.togglePause();

    el.appendRows(pushed(3, 10).map((r: any) => r.log));
    await el.updateComplete;
    expect(el.rows).toHaveLength(3); // display frozen
    expect(el.buffer).toHaveLength(10); // stream still flowing

    // Resuming jumps to the live edge rather than replaying the gap.
    el.togglePause();
    await el.updateComplete;
    expect(el.rows).toHaveLength(10);
    expect(el.rows[el.rows.length - 1].body).toBe('row 9');
  });
});

describe('Live Tail reconnection', () => {
  test('re-registers with the same selectors the user chose', async () => {
    // A reconnect that silently widened or narrowed the filter would show rows the user never
    // asked for, or hide the ones they did, with nothing on screen to say the query changed.
    const el = await mount();
    el.querySelector('[data-service]').value = 'checkout';
    el.querySelector('[data-environment]').value = 'prod';
    el.querySelector('[data-query]').value = 'level == "error"';

    el.start();
    await settle(); // registration POST resolves, then the EventSource opens
    FakeEventSource.last!.emit('ready', {});

    // A transport blip: EventSource fails, LiveStream re-registers from scratch.
    FakeEventSource.last!.onerror!();
    await new Promise(r => setTimeout(r, 1100)); // first backoff step
    await settle();

    expect(posted.length).toBeGreaterThanOrEqual(2);
    expect(posted[posted.length - 1]).toMatchObject({
      service: 'checkout',
      environment: 'prod',
      query: 'level == "error"',
    });
  });
});
