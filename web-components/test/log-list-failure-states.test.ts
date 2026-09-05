// What the reader sees when a query or a page fails.
//
// The design rule is that an error must never be silent, and it must land where the reader
// is looking: a bad field belongs under the query box next to the editor's own squiggles,
// while a failed *page* of an otherwise-working query must not blank the rows already on
// screen. Getting that split wrong during an incident either hides the cause or throws away
// the evidence. None of these paths were tested.
import { describe, test, expect, vi, beforeEach, afterEach } from 'vitest';
import { serverTransport, logPage, deferredTransport, mountList, ids, treeFromLogs } from './log-list-harness';

// The two channels the component reports on: a parse error routed to the query box, and a
// transient toast. Both are CustomEvents on document.body.
const listen = () => {
  const parseErrors: string[] = [];
  const toasts: string[] = [];
  const onParse = (e: any) => parseErrors.push(e.detail);
  const onToast = (e: any) => toasts.push(...e.detail.value);
  document.body.addEventListener('showParseError', onParse);
  document.body.addEventListener('errorToast', onToast);
  return {
    parseErrors,
    toasts,
    stop: () => {
      document.body.removeEventListener('showParseError', onParse);
      document.body.removeEventListener('errorToast', onToast);
    },
  };
};

const loaded = async (props: Record<string, any> = {}) => {
  const el = await mountList(props as any);
  el.transport = serverTransport(logPage(['a', 'b']));
  await el.fetchData('first', true);
  return el;
};

let events: ReturnType<typeof listen>;
beforeEach(() => (events = listen()));
afterEach(() => {
  events.stop();
  vi.restoreAllMocks();
});

describe('a query the server rejects', () => {
  // It belongs under the query box, beside the editor's client-side squiggles — the same
  // channel the server's HX-Trigger uses — not only in the row area.
  test('is routed to the query box', async () => {
    const el = await mountList();
    (window as any).logDataPromise = Promise.resolve({ error: 'unknown field: srvice.name' });

    await el.fetchData('/p/x/log_explorer/data', true);

    expect(events.parseErrors).toEqual(['unknown field: srvice.name']);
    delete (window as any).logDataPromise;
  });

  test('is shown inline when there is nothing else on screen', async () => {
    const el = await mountList();
    el.transport = async () => {
      throw new Error('unknown field: srvice.name');
    };

    await el.fetchData('bad', true);

    expect((el as any).fetchError).toBe('unknown field: srvice.name');
    await el.updateComplete;
    expect(el.textContent).toContain('Could not load events');
    expect(el.textContent).not.toContain('No events match');

    el.transport = serverTransport(logPage([], { hasMore: false }));
    (el.querySelector('[role="alert"] button') as HTMLButtonElement).click();
    await vi.waitFor(() => expect(el.textContent).toContain('No events match'));
    expect(el.textContent).not.toContain('Could not load events');
  });
});

describe('a page that fails after rows are already showing', () => {
  test('labels retained rows when a replacement query fails', async () => {
    const el = await loaded();
    el.transport = async () => { throw new Error('query failed'); };
    await el.fetchData('replacement', true);
    await el.updateComplete;
    expect(ids(el)).toEqual(['a', 'b']);
    expect(el.textContent).toContain('Previously loaded events are still shown.');
    expect(el.textContent).not.toContain('No events match');
  });
  // Blanking the list would throw away the evidence the reader is working from; the failure
  // belongs in a toast instead.
  test('keeps the rows and reports the failure as a toast', async () => {
    const el = await loaded();
    el.transport = async () => {
      throw new Error('upstream exploded');
    };

    await el.fetchData('more', false, false, true);

    expect(ids(el)).toEqual(['a', 'b']);
    expect((el as any).fetchError).toBeNull();
    expect(events.toasts).toEqual(['upstream exploded']);
  });

  // A bare `new Error()` (and some DOM exceptions) carry an empty message. Passing that
  // straight through renders an empty toast: a failure the reader is told nothing about.
  test('a failure without a message still says something', async () => {
    const el = await loaded();
    el.transport = async () => {
      throw new Error();
    };

    await el.fetchData('more', false, false, true);

    expect(events.toasts).toEqual(['Network error']);
  });

  test('a non-Error throw is reported too', async () => {
    const el = await loaded();
    el.transport = async () => {
      throw 'a bare string';
    };

    await el.fetchData('more', false, false, true);

    expect(events.toasts).toEqual(['Network error']);
  });

  // The in-flight guard has to be released on the error path too, or the list refuses
  // every later page and looks permanently stuck at the bottom.
  test('the list can page again after a failure', async () => {
    const el = await loaded();
    el.transport = async () => {
      throw new Error('transient');
    };
    await el.fetchData('more', false, false, true);
    expect((el as any).isLoadingMore).toBe(false);

    el.transport = serverTransport(logPage(['c']));
    await el.fetchData('retry', false, false, true);

    expect(ids(el)).toContain('c');
  });

  test('a failed recent fetch does not strand the live-tail guard either', async () => {
    const el = await loaded();
    el.transport = async () => {
      throw new Error('transient');
    };

    await el.fetchData('newer', false, true);

    expect((el as any).isFetchingRecent).toBe(false);
  });
});

describe('a superseded request', () => {
  // Latest-request-wins: the reader has already moved on, so neither the stale rows nor a
  // stale error may replace what the newer query put on screen.
  test('cannot report its error over the newer query', async () => {
    const el = await mountList();
    const transport = deferredTransport();
    el.transport = transport;

    const stale = el.fetchData('old-query', true);
    const fresh = el.fetchData('new-query', true);
    transport.settle(1, treeFromLogs(['fresh']));
    await fresh;
    transport.pending[0].resolve(Promise.reject(new Error('stale failure')));
    await stale;

    expect(ids(el)).toEqual(['fresh']);
    expect((el as any).fetchError).toBeNull();
    expect(events.toasts).toEqual([]);
  });
});

describe('an empty result', () => {
  test('clears the previous query rather than leaving its rows on screen', async () => {
    const el = await loaded();
    el.transport = serverTransport(logPage([], { hasMore: false }));

    await el.fetchData('new-query', true);

    expect(ids(el)).toEqual([]);
    expect((el as any).seenIds.size).toBe(0);
  });

  // An empty page at the end of history is not an empty result set: the rows already
  // loaded stay, and only that edge closes.
  test('at the end of history it closes the edge and keeps the rows', async () => {
    const el = await loaded();
    el.transport = serverTransport(logPage([]));

    await el.fetchData('older', false, false, true);

    expect(ids(el)).toEqual(['a', 'b']);
    expect((el as any).hasMore).toBe(false);
  });

  // Live tail ticks every few seconds and is usually quiet; a quiet tick must not be
  // mistaken for "history exhausted" and flash the widen-the-range prompt.
  test('a quiet live-tail tick does not flash the widen-time-range prompt', async () => {
    const el = await loaded();
    (el as any).expandTimeRange = false;
    el.transport = serverTransport(logPage([]));

    await el.fetchData('newer', false, true);

    expect((el as any).expandTimeRange).toBe(false);
    expect((el as any).hasNewer).toBe(false);
  });
});
