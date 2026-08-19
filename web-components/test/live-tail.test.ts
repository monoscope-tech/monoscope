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
    await settle(); // the schema fetch fills the selects; without it there is nothing to pick

    // Drive the controls the component actually exposes. There are no data-* hooks: the filter
    // lives in properties fed by these `change` handlers, and the query arrives as an
    // `update-query` event from the shared editor.
    const pick = (label: string, value: string) => {
      const select = el.querySelector(`select[aria-label="${label}"]`) as HTMLSelectElement;
      select.value = value;
      expect(select.value).toBe(value); // guards against the option not existing
      select.dispatchEvent(new Event('change'));
    };
    pick('Service', 'checkout');
    pick('Environment', 'prod');
    el.querySelector('query-editor')!.dispatchEvent(new CustomEvent('update-query', { detail: { value: 'level == "error"' } }));
    await el.updateComplete;

    // No explicit start: every filter change restarts the stream, which is the behaviour under
    // test — the last of those restarts is the registration a reconnect has to reproduce.
    await settle(); // registration POST resolves, then the EventSource opens
    FakeEventSource.last!.emit('ready', {});
    const selectors = { service: 'checkout', environment: 'prod', query: 'level == "error"' };
    expect(posted.at(-1)).toMatchObject(selectors); // the filter reached the server at all
    const beforeReconnect = posted.length;

    // A transport blip: EventSource fails, LiveStream re-registers from scratch.
    FakeEventSource.last!.onerror!();
    await new Promise(r => setTimeout(r, 1100)); // first backoff step
    await settle();

    // Counted from the reconnect, not from zero: the filter changes above each restart the
    // stream too, so a total of ">= 2" would pass even if the blip re-registered nothing.
    expect(posted.length).toBeGreaterThan(beforeReconnect);
    expect(posted.at(-1)).toMatchObject(selectors);
  });
});

// The connection state machine and what a restart resets. Live tail is watched during an
// incident, so "is this still receiving?" has to be answerable from the screen — a stream
// that stopped while showing its last rows is indistinguishable from a quiet service.
describe('Live Tail connection state', () => {
  test('only connecting, live and reconnecting count as running', async () => {
    const el = await mount();
    for (const [state, isRunning] of [
      ['idle', false],
      ['connecting', true],
      ['live', true],
      ['reconnecting', true],
      ['stopped', false],
      ['expired', false],
    ] as const) {
      el.streamState = state;
      expect([state, el.running]).toEqual([state, isRunning]);
    }
  });

  // The status line is what the reader checks. A state change that left the previous
  // state's message behind would describe a connection that no longer exists.
  test('the status message belongs to the current state', async () => {
    const el = await mount();
    el.restart();
    const stream = (el as any).stream;

    stream.opts.onState('reconnecting', 'lost the connection');
    await el.updateComplete;
    expect([el.streamState, el.statusMessage]).toEqual(['reconnecting', 'lost the connection']);

    stream.opts.onState('live', undefined);
    await el.updateComplete;
    expect([el.streamState, el.statusMessage]).toEqual(['live', '']);
  });

  test('server-side drops are reported as a total, not accumulated twice', async () => {
    const el = await mount();
    el.restart();
    const stream = (el as any).stream;

    stream.opts.onState('live', undefined);
    stream.opts.onDropped(12);
    stream.opts.onDropped(30); // the server reports a running total
    await el.updateComplete;

    expect(el.droppedServer).toBe(30);
  });
});

describe('Live Tail restart', () => {
  // Restarting means a new filter: rows matched against the old one are not results for
  // the new one, and carrying over a drop count would attribute them to the wrong query.
  test('clears the rows and drop counts from the previous filter', async () => {
    const el = await mount();
    el.restart();
    el.appendRows(pushed(0, 5).map((r: any) => r.log));
    (el as any).droppedServer = 7;
    (el as any).droppedClient = 3;
    await el.updateComplete;

    el.restart();
    await el.updateComplete;

    expect(el.rows).toEqual([]);
    expect(el.buffer).toEqual([]);
    expect(el.droppedServer).toBe(0);
    expect(el.droppedClient).toBe(0);
  });

  test('replaces the old connection rather than leaving two open', async () => {
    const el = await mount();
    el.restart();
    const first = (el as any).stream;

    el.restart();

    expect((el as any).stream).not.toBe(first);
    expect(first.isRunning).toBe(false);
  });

  // A plain /live_tail link must stay plain: only a filter the reader actually chose
  // belongs in the URL they might copy.
  test('writes only the filters that differ from the defaults into the URL', async () => {
    const el = await mount();
    window.history.replaceState({}, '', '/p/p1/live_tail');
    const param = (k: string) => new URLSearchParams(window.location.search).get(k);

    el.service = 'checkout';
    el.environment = 'prod';
    el.kind = 'spans';
    el.restart();
    expect([param('service'), param('env'), param('kind')]).toEqual(['checkout', 'prod', 'spans']);

    // Cleared filters are removed rather than left as empty keys, and `logs` is the
    // default kind so it never appears — a plain /live_tail link stays plain.
    el.service = '';
    el.environment = '';
    el.kind = 'logs';
    el.restart();
    expect([param('service'), param('env'), param('kind')]).toEqual([null, null, null]);
  });

  test('the registration body carries the chosen filters', async () => {
    const el = await mount();
    el.service = 'checkout';
    el.environment = 'prod';
    el.query = 'level == "error"';

    el.restart();
    const body = (el as any).stream.opts.body();

    expect(body).toMatchObject({ service: 'checkout', environment: 'prod', query: 'level == "error"' });
  });

  test('an unset filter is sent as null, not an empty string', async () => {
    const el = await mount();
    el.service = '';
    el.environment = '';
    el.query = '';

    el.restart();

    expect((el as any).stream.opts.body()).toMatchObject({ service: null, environment: null, query: null });
  });
});

// Following the live edge.
//
// The rule is asymmetric and that asymmetry is the whole point: rows arriving must pin the
// view to the bottom, but only while the reader left it there. Scrolling up to read something
// is exactly when a yank back to the edge is most destructive — it is the standalone-component
// twin of the log-list bug where an eviction remount threw the reader to the top.
describe('Live Tail follows the edge only while the reader is at it', () => {
  const GEOM = { clientHeight: 400, scrollHeight: 4000 };

  /** jsdom lays nothing out, so the scroller's geometry is supplied. */
  const withScroller = (el: any) => {
    const list = el.querySelector('[data-rows]') as HTMLElement;
    Object.defineProperty(list, 'clientHeight', { value: GEOM.clientHeight, configurable: true });
    Object.defineProperty(list, 'scrollHeight', { value: GEOM.scrollHeight, configurable: true });
    return list;
  };

  const scrollTo = async (el: any, list: HTMLElement, top: number) => {
    list.scrollTop = top;
    list.dispatchEvent(new Event('scroll'));
    await el.updateComplete;
  };

  test('rows arriving keep the view pinned to the newest', async () => {
    const el = await mount();
    const list = withScroller(el);

    el.appendRows(pushed(0, 5).map((r: any) => r.log));
    await el.updateComplete;

    expect(list.scrollTop).toBe(GEOM.scrollHeight);
  });

  test('scrolled up to read, an arriving batch does not move the viewport', async () => {
    const el = await mount();
    const list = withScroller(el);
    await scrollTo(el, list, 1000); // well above the bottom
    expect(el.stickToBottom).toBe(false);

    el.appendRows(pushed(0, 20).map((r: any) => r.log));
    await el.updateComplete;

    expect(list.scrollTop).toBe(1000);
  });

  test('returning to the bottom re-engages following', async () => {
    const el = await mount();
    const list = withScroller(el);
    await scrollTo(el, list, 1000);

    await scrollTo(el, list, GEOM.scrollHeight - GEOM.clientHeight);
    expect(el.stickToBottom).toBe(true);

    el.appendRows(pushed(0, 5).map((r: any) => r.log));
    await el.updateComplete;
    expect(list.scrollTop).toBe(GEOM.scrollHeight);
  });

  // The 40px slack is what keeps following from breaking on fractional row heights, where
  // scrollTop + clientHeight lands a pixel short of scrollHeight and never reads as "bottom".
  test('a few pixels short of the bottom still counts as the bottom', async () => {
    const el = await mount();
    const list = withScroller(el);

    await scrollTo(el, list, GEOM.scrollHeight - GEOM.clientHeight - 39);
    expect(el.stickToBottom).toBe(true);

    await scrollTo(el, list, GEOM.scrollHeight - GEOM.clientHeight - 41);
    expect(el.stickToBottom).toBe(false);
  });
});
