import { describe, test, expect, afterEach, vi } from 'vitest';
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

const nextFrame = () => new Promise<void>(resolve => requestAnimationFrame(() => resolve()));
/** Past the component's publish window, so a busy buffer has reached `rows`. */
const published = () => new Promise<void>(resolve => setTimeout(resolve, 300));

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
  localStorage.clear();
  window.history.replaceState({}, '', '/p/p1/live_tail');
  FakeEventSource.last = null;
});

describe('Live Tail row buffer', () => {
  test('keeps the full buffer but mounts only a viewport-sized row window', async () => {
    const el = await mount();
    el.appendRows(pushed(0, 1000).map((r: any) => r.log));
    await published();
    await el.updateComplete;

    expect(el.rows).toHaveLength(1000);
    expect(el.querySelectorAll('[data-row]').length).toBeLessThan(100);
  });

  // Tall rows broke following for good: the virtualizer's remeasure scrolled the list short
  // of the bottom, and that scroll read as the reader leaving. Pinning the last row keeps the
  // bottom through remeasures, and the virtualizer itself reports a reader's scroll.
  test('liveTail_virtualizedFollow_survivesRemeasureAndYieldsToTheReader', async () => {
    const el = await mount();
    el.appendRows(pushed(0, 200).map((r: any) => r.log));
    await published();
    await el.updateComplete;
    const list = el.querySelector('lit-virtualizer') as any;
    Object.defineProperty(list, 'clientHeight', { value: 400, configurable: true });
    Object.defineProperty(list, 'scrollHeight', { value: 4000, configurable: true });
    expect(list.layout).toEqual({ pin: { index: Number.MAX_SAFE_INTEGER, block: 'end' } });

    // Handler called directly: in jsdom the virtualizer reads any dispatched scroll as the reader's.
    list.scrollTop = 1000;
    el.trackScrollPosition({ currentTarget: list });
    await el.updateComplete;
    expect(el.stickToBottom).toBe(true);

    list.dispatchEvent(new Event('unpinned'));
    await el.updateComplete;
    expect(el.stickToBottom).toBe(false);
    expect(list.layout).toEqual({});
  });

  // lit-virtualizer re-renders mounted rows only when handed a new items array or renderItem;
  // jsdom mounts none, so assert the hand-off itself.
  test('liveTail_virtualizedRows_reRenderWhenAFieldIsToggled', async () => {
    const el = await mount();
    el.appendRows(pushed(0, 200).map((r: any) => r.log));
    await published();
    await el.updateComplete;
    const list = el.querySelector('lit-virtualizer') as any;
    const before = list.renderItem;

    el.toggleField('attributes.order.id', true);
    await el.updateComplete;
    expect(list.renderItem).not.toBe(before);
  });

  // lit-virtualizer rejects `layoutComplete` with 'disconnected' when torn down; Clear, a
  // restart or navigating away mid-follow left that rejection unhandled.
  test('liveTail_virtualizerRemovedMidFollow_leavesNoUnhandledRejection', async () => {
    const rejections: unknown[] = [];
    const onRejection = (reason: unknown) => rejections.push(reason);
    process.on('unhandledRejection', onRejection);
    const el = await mount();
    el.appendRows(pushed(0, 200).map((r: any) => r.log));
    await published();
    await el.updateComplete;
    el.remove();
    for (let i = 0; i < 3; i++) await nextFrame();
    await new Promise(r => setTimeout(r, 20));
    process.off('unhandledRejection', onRejection);
    expect(rejections).toEqual([]);
  });

  test('caps the buffer and counts what it dropped', async () => {
    // 1200 rows into a 1000-row cap. The count matters as much as the cap: rows vanishing with
    // no number attached is indistinguishable from a filter that stopped matching.
    const el = await mount();
    el.appendRows(pushed(0, 1200).map((r: any) => r.log));
    await published();
    await el.updateComplete;

    expect(el.rows).toHaveLength(1000);
    expect(el.droppedClient).toBe(200);
    // Drops the oldest, so the live edge is what survives — showing the start of a burst while
    // discarding what is happening now would be worse than showing fewer rows.
    expect(el.rows[el.rows.length - 1].body).toBe('row 1199');
  });

  // Customer report 2026-10-28: the page and its query editor froze on weaker machines. Each
  // 100-row SSE frame re-rendered the list; at 4x CPU those frames land in separate tasks.
  test('liveTail_burstyFrames_publishRowsAtMostFourTimesASecond', async () => {
    const el = await mount();
    vi.useFakeTimers();
    el.appendRows(pushed(0, 100).map((r: any) => r.log));
    let publishes = 0;
    let last = el.rows;
    for (let n = 100; n < 100 + 63 * 10; n += 10) {
      el.appendRows(pushed(n, n + 10).map((r: any) => r.log));
      vi.advanceTimersByTime(16);
      if (el.rows !== last) (publishes++, (last = el.rows));
    }
    vi.useRealTimers();
    expect(publishes).toBeGreaterThan(0);
    expect(publishes).toBeLessThanOrEqual(4);
  });

  test('liveTail_hiddenTab_publishesNothingUntilShownAgain', async () => {
    const el = await mount();
    el.appendRows(pushed(0, 99).map((r: any) => r.log));
    const shown = el.rows;
    expect(shown).toHaveLength(99);
    Object.defineProperty(document, 'hidden', { value: true, configurable: true });
    el.appendRows(pushed(99, 200).map((r: any) => r.log));
    await published();
    expect(el.rows).toBe(shown);

    Object.defineProperty(document, 'hidden', { value: false, configurable: true });
    document.dispatchEvent(new Event('visibilitychange'));
    expect(el.rows).toHaveLength(200);
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

describe('Live Tail query edits', () => {
  // A restart clears the rows, so restarting on every typing pause wiped the stream while the
  // reader was still mid-query. Only a submitted query (Enter, AI search, a facet) restarts.
  test('liveTail_typingPause_doesNotRestartTheStream', async () => {
    const el = await mount();
    await settle();
    el.appendRows(pushed(0, 5).map((r: any) => r.log));
    const editor = el.querySelector('query-editor') as HTMLElement;
    const registrations = posted.length;
    const send = (detail: object) => editor.dispatchEvent(new CustomEvent('update-query', { detail, bubbles: true }));

    send({ value: 'level == "err', source: 'typing' });
    await settle();
    expect(posted).toHaveLength(registrations);
    expect(el.rows).toHaveLength(5);

    send({ value: 'level == "error"' });
    await settle();
    expect(posted).toHaveLength(registrations + 1);
    expect(posted.at(-1).query).toBe('level == "error"');
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
  test('initializes the stream from the canonical global environment key', async () => {
    window.history.replaceState({}, '', '/p/p1/live_tail?environment=production&service_scope=checkout');
    const el = await mount();
    expect(el.environment).toBe('production');
    expect(el.service).toBe('checkout');
    expect((el as any).stream.opts.body()).toMatchObject({ service: 'checkout', environment: 'production' });
  });

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

describe('Live Tail fields and details', () => {
  // Shipbubble-rate load (2026-10-20): 8,000-char bodies rendered whole kept the main thread
  // ~27% busy laying out text. Rows preview the body; the drawer keeps all of it.
  test('liveTail_longBody_rowsRenderAPreview', async () => {
    const el = await mount();
    const body = 'Deadlock found when trying to get lock; ' + 'x'.repeat(7960);
    el.appendRows([{ ...logRow(1), body, truncated: true }, { ...logRow(2), body: JSON.stringify({ msg: 'boom', stack: 'y'.repeat(7000) }) }]);
    await el.updateComplete;
    const messages = [...el.querySelectorAll('[data-row] [data-message]')].map((m: Element) => m.textContent ?? '');
    expect(messages).toHaveLength(2);
    for (const text of messages) expect(text.length).toBeLessThan(1200);
    expect(messages[0]).toContain('Deadlock found');
    expect(messages[0]).toContain('…truncated');
    expect(messages[1]).not.toContain('…truncated');
  });

  test('keeps service and timestamp as the only fixed columns and groups selected fields as one message', async () => {
    const el = await mount();
    el.appendRows([{ ...logRow(1), fields: { 'attributes.order.id': 'ord-42' } }, logRow(2)]);
    el.toggleField('attributes.order.id', true);
    await el.updateComplete;

    const rows = el.querySelectorAll('[data-row]');
    const row = rows[0];
    expect(row.querySelector('[data-service]')!.textContent).toContain('checkout');
    expect(row.querySelector('[data-time]')!.textContent).toContain('00:00:00.000');
    expect(row.querySelector('[data-message]')!.textContent).toContain('level="info"');
    expect(row.querySelector('[data-message]')!.textContent).toContain('attributes.order.id="ord-42"');
    expect(rows[1].querySelector('[data-message]')!.textContent).not.toContain('attributes.order.id=');
    expect((el as any).selectedFields).toEqual(['level', 'body', 'attributes.order.id']);
    expect(el.querySelector('input[aria-label*="service in each row"]')).toBeNull();
    expect(el.querySelector('input[aria-label*="timestamp in each row"]')).toBeNull();
  });

  // With the picker open, every row batch re-sorted and re-rendered ~2,000 field labels:
  // 87% of the main thread at 4x CPU (customer report 2026-10-28).
  test('liveTail_fieldPicker_rendersOnlyWhileOpenAndHoldsStillUnderLoad', async () => {
    const el = await mount();
    el.appendRows([{ ...logRow(1), fields: { 'attributes.order.id': 'ord-42' } }]);
    await el.updateComplete;
    const toggles = () => [...el.querySelectorAll('[aria-label="Visible live-tail fields"] input')].map((i: Element) => i.getAttribute('aria-label'));
    expect(toggles()).toEqual([]);

    const picker = el.querySelector('details') as HTMLDetailsElement;
    picker.open = true;
    picker.dispatchEvent(new Event('toggle'));
    await el.updateComplete;
    const opened = toggles();
    expect(opened).toContain('Show attributes.order.id in each row');

    el.appendRows([{ ...logRow(2), fields: { 'attributes.cart.size': 3 } }]);
    await el.updateComplete;
    expect(toggles()).toEqual(opened);
  });

  test('liveTail_fieldPicker_rendersAWindowAndSearchesTheRest', async () => {
    const el = await mount();
    el.schemaFields = Array.from({ length: 500 }, (_, i) => `attributes.f${i}`);
    const picker = el.querySelector('details') as HTMLDetailsElement;
    picker.open = true;
    picker.dispatchEvent(new Event('toggle'));
    await el.updateComplete;
    const list = () => el.querySelector('[aria-label="Visible live-tail fields"]')!;
    expect(list().querySelectorAll('input').length).toBe(200);
    expect(list().textContent).toContain('Search to find the other');

    el.fieldSearch = 'attributes.f499';
    await el.updateComplete;
    expect(list().querySelector('input[aria-label="Show attributes.f499 in each row"]')).not.toBeNull();
  });

  test('renders structured bodies as readable key-value pairs in the message column', async () => {
    const el = await mount();
    el.appendRows([{ ...logRow(1), body: '{"message":"charged","user":{"id":42}}' }]);
    await el.updateComplete;

    const message = el.querySelector('[data-message]')!.textContent!;
    expect(message).toContain('message="charged"');
    expect(message).toContain('user.id=42');
    expect(message).not.toContain('{"message"');
  });

  test('uses the rich summary as the primary span content', async () => {
    window.history.replaceState({}, '', '/p/p1/live_tail?kind=spans');
    const el = await mount();
    const summary = ['request_type;badge-neutral⇒incoming', 'method;badge-POST⇒POST', 'route;text-textStrong⇒/checkout'];
    el.appendRows([{ ...logRow(1), level: null, body: '', name: 'POST /checkout', fields: { summary } }]);
    await el.updateComplete;

    expect((el as any).stream.opts.body().columns).toContain('summary');
    expect((el as any).selectedFields).toEqual(['summary']);
    expect(el.querySelector('[data-row]')!.textContent).toContain('incoming');
    expect(el.querySelector('[data-row]')!.textContent).toContain('POST');
    expect(el.querySelector('[data-row]')!.textContent).toContain('/checkout');
    expect(el.querySelector('[data-row]')!.textContent).not.toContain('summary=');
  });

  test('adds and removes projected fields without discarding the visible tail', async () => {
    const el = await mount();
    const row = {
      ...logRow(1),
      fields: { attributes___http___request___method: 'GET' },
    };
    el.appendRows([row]);
    await el.updateComplete;

    el.toggleField('attributes___http___request___method', true);
    await el.updateComplete;

    expect((el as any).stream.opts.body().columns).toContain('attributes___http___request___method');
    expect(el.querySelector('[data-row]')!.textContent).toContain('attributes.http.request.method="GET"');
    expect(el.rows).toHaveLength(1);

    el.toggleField('attributes___http___request___method', false);
    await el.updateComplete;
    expect(el.querySelector('[data-row]')!.textContent).not.toContain('attributes.http.request.method');
  });

  test('opens AI search and applies its generated query to the live stream', async () => {
    const el = await mount();
    const fetchStub = globalThis.fetch;
    let requestBody: any;
    (globalThis as any).fetch = async (url: string, init?: any) => {
      if (url.includes('/ai_search')) {
        requestBody = JSON.parse(init.body);
        return { ok: true, status: 200, json: async () => ({ query: 'level == "ERROR"' }) };
      }
      return fetchStub(url, init);
    };

    el.querySelector('query-editor')!.dispatchEvent(new CustomEvent('open-ai-search', { bubbles: true }));
    await el.updateComplete;
    const prompt = el.querySelector('[aria-label="AI search prompt"]') as HTMLInputElement;
    expect(prompt).not.toBeNull();
    expect(prompt.autocomplete).toBe('new-password');
    prompt.value = 'errors in checkout';
    prompt.dispatchEvent(new Event('input'));
    await el.updateComplete;
    (el.querySelector('[aria-label="Submit AI search"]') as HTMLButtonElement).click();
    await settle();
    await el.updateComplete;

    expect(requestBody.input).toBe('errors in checkout');
    expect((el as any).stream.opts.body().query).toBe('level == "ERROR"');
    expect(el.querySelector('[aria-label="AI search prompt"]')).toBeNull();
  });

  test('loads the complete durable record into the drawer instead of stopping at streamed fields', async () => {
    const el = await mount();
    const fetchStub = globalThis.fetch;
    (globalThis as any).fetch = async (url: string, init?: any) =>
      url.includes('/live_tail/records/')
        ? {
            ok: true,
            status: 200,
            json: async () => ({
              id: '00000000-0000-4000-8000-000000000001',
              timestamp: '2024-01-01T00:00:00.000Z',
              body: { message: 'complete record' },
              resource: { service: { name: 'checkout' } },
              attributes: { order: { id: 'ord-full' } },
            }),
          }
        : fetchStub(url, init);
    el.appendRows([{ ...logRow(1), id: '00000000-0000-4000-8000-000000000001', body: 'streamed preview' }]);
    el.openDetails(el.rows[0]);
    await settle();
    await el.updateComplete;

    const dialog = el.querySelector('dialog') as HTMLDialogElement;
    expect(dialog.textContent).toContain('Complete stored JSON');
    expect(dialog.textContent).toContain('complete record');
    expect(dialog.textContent).toContain('attributes.order.id');
    expect(dialog.textContent).toContain('ord-full');
    expect(dialog.querySelector('input[aria-label*="service in each row"]')).toBeNull();
    expect(dialog.textContent).toContain('Pinned');
  });

  test('opens an accessible details drawer with raw JSON, field switches and zebra rows', async () => {
    const el = await mount();
    el.appendRows([
      { ...logRow(1), body: '{"message":"charged","user":{"id":42}}' },
      { ...logRow(2), body: 'plain text' },
    ]);
    await el.updateComplete;

    const rowClasses = [...el.querySelectorAll('[data-row]')].map((row: Element) => row.className);
    expect(rowClasses[0]).not.toBe(rowClasses[1]);

    const row = el.querySelector('[data-row]') as HTMLButtonElement;
    expect(row.tagName).toBe('BUTTON');
    expect(row.getAttribute('aria-haspopup')).toBe('dialog');
    row.click();
    await el.updateComplete;
    const dialog = el.querySelector('dialog') as HTMLDialogElement;
    expect(dialog.open).toBe(true);
    expect(dialog.getAttribute('aria-labelledby')).toBe('live-tail-detail-title');
    expect(dialog.textContent).toContain('Raw record');
    expect(dialog.textContent).toContain('body.user.id');
    expect(dialog.querySelector('input[aria-label="Show body.user.id in each row"]')).not.toBeNull();
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
    expect([param('service'), param('environment'), param('env'), param('kind')]).toEqual(['checkout', 'prod', null, 'spans']);

    // Cleared filters are removed rather than left as empty keys, and `logs` is the
    // default kind so it never appears — a plain /live_tail link stays plain.
    el.service = '';
    el.environment = '';
    el.kind = 'logs';
    el.restart();
    expect([param('service'), param('environment'), param('env'), param('kind')]).toEqual([null, null, null, null]);
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

  test('jumping to live preserves the selected service without restarting or navigating', async () => {
    const el = await mount();
    await settle();
    const service = el.querySelector('select[aria-label="Service"]') as HTMLSelectElement;
    service.value = 'checkout';
    service.dispatchEvent(new Event('change'));
    await settle();
    await el.updateComplete;

    const registrations = posted.length;
    const url = window.location.href;
    const list = withScroller(el);
    await scrollTo(el, list, 1000);
    const jump = el.querySelector<HTMLButtonElement>('[data-jump-to-live]');
    expect(jump).toBeTruthy();

    jump!.click();
    await el.updateComplete;

    expect(el.service).toBe('checkout');
    expect((el.querySelector('select[aria-label="Service"]') as HTMLSelectElement).value).toBe('checkout');
    expect(window.location.href).toBe(url);
    expect(posted).toHaveLength(registrations);
    expect(list.scrollTop).toBe(GEOM.scrollHeight);
  });
});
