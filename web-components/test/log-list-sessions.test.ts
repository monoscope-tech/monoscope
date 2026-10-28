// Sessions mode: a session row expands by fetching its events on demand.
//
// Unlike a trace, a session's children are not in the page — clicking fetches them,
// splices them under the parent and adopts them into the dedup set. That makes it the one
// expansion path with a network call, a loading flag and a rollback, and none of it was
// tested. A failed rollback is the nasty case: the row looks expanded, so the next click
// takes the collapse branch and the reader can never retry.
import { describe, test, expect, vi, afterEach } from 'vitest';
import { row, mountList, stubFetch } from './log-list-harness';

const SESSION = 'sess-1';

const sessionsList = async () => {
  const el = await mountList({ mode: 'sessions', projectId: 'proj-1' } as any);
  const parent = { ...row(SESSION), children: 3 };
  (el as any).spanListTree = [parent, row('other-session')];
  (el as any).seenIds = new Set([SESSION, 'other-session']);
  (el as any).updateVisibleItems();
  await el.updateComplete;
  return el;
};

// The child payload the expand endpoint returns.
const childPayload = (ids: string[]) => ({
  colIdxMap: { timestamp: 0, latency_breakdown: 1, trace_id: 2, parent_id: 3, kind: 4, id: 5, duration: 6, start_time_ns: 7 },
  rows: ids.map((id) => ['2024-01-01T00:00:00.000Z', id, id, '', 'log', id, 0, Date.parse('2024-01-01T00:00:00Z') * 1e6]),
  traces: ids.map((id) => ({ trace_id: id, start_time: 0, duration: 0, trace_start_time: '2024-01-01T00:00:00.000Z', root: id, children: {} })),
});

const ids = (el: any) => el.spanListTree.map((r: any) => r.id);
const shown = (el: any) => (el.virtualListItems as any[]).filter((i) => 'id' in i).map((i) => i.id);

afterEach(() => vi.restoreAllMocks());

describe('expanding a session', () => {
  test('fetches its events and shows them under the parent', async () => {
    const el = await sessionsList();
    const restore = stubFetch(childPayload(['e1', 'e2']));

    await (el as any).expandSessionTrace(SESSION);

    expect(ids(el)).toEqual([SESSION, 'e1', 'e2', 'other-session']);
    expect(shown(el)).toContain('e1');
    restore();
  });

  test('adopts the fetched events into the dedup set so a later page cannot duplicate them', async () => {
    const el = await sessionsList();
    const restore = stubFetch(childPayload(['e1', 'e2']));

    await (el as any).expandSessionTrace(SESSION);

    expect((el as any).seenIds.has('e1')).toBe(true);
    expect((el as any).seenIds.has('e2')).toBe(true);
    restore();
  });

  // The list is built from a rollup count that can disagree with what actually loaded.
  test('the parent reports the number of events it really has', async () => {
    const el = await sessionsList();
    const restore = stubFetch(childPayload(['e1', 'e2']));

    await (el as any).expandSessionTrace(SESSION);

    expect((el as any).spanListTree[0].children).toBe(2);
    restore();
  });

  test('collapsing hides the events without refetching them', async () => {
    const el = await sessionsList();
    const restore = stubFetch(childPayload(['e1', 'e2']));
    await (el as any).expandSessionTrace(SESSION);
    restore();

    const noFetch = vi.fn();
    const undo = stubFetch();
    (globalThis as any).fetch = noFetch;
    await (el as any).expandSessionTrace(SESSION);

    expect(shown(el)).not.toContain('e1');
    expect(ids(el)).toContain('e1'); // still loaded, just hidden
    expect(noFetch).not.toHaveBeenCalled();
    undo();
  });

  test('re-expanding shows the loaded events again without a second request', async () => {
    const el = await sessionsList();
    const restore = stubFetch(childPayload(['e1', 'e2']));
    await (el as any).expandSessionTrace(SESSION);
    restore();
    await (el as any).expandSessionTrace(SESSION); // collapse

    const noFetch = vi.fn();
    (globalThis as any).fetch = noFetch;
    await (el as any).expandSessionTrace(SESSION);

    expect(shown(el)).toContain('e1');
    expect(noFetch).not.toHaveBeenCalled();
  });

  test('only the requested session expands', async () => {
    const el = await sessionsList();
    const restore = stubFetch(childPayload(['e1']));

    await (el as any).expandSessionTrace(SESSION);

    expect((el as any).spanListTree[0].expanded).toBe(true);
    expect((el as any).spanListTree.find((r: any) => r.id === 'other-session').expanded).toBe(false);
    restore();
  });
});

describe('when the fetch fails', () => {
  const failing = (init: () => void) => {
    const original = globalThis.fetch;
    (globalThis as any).fetch = async () => {
      init();
      return { ok: false, status: 503, json: async () => ({}) };
    };
    return () => ((globalThis as any).fetch = original);
  };

  // Rollback matters: leaving the parent marked expanded sends the next click down the
  // collapse branch, which is a no-op — so the reader can never retry the fetch.
  test('the row rolls back so the next click retries instead of no-opping', async () => {
    const el = await sessionsList();
    const attempts: number[] = [];
    const restore = failing(() => attempts.push(1));

    await (el as any).expandSessionTrace(SESSION);

    expect((el as any).spanListTree[0].expanded).toBe(false);
    expect((el as any).expandedTraces[SESSION]).toBeUndefined();

    await (el as any).expandSessionTrace(SESSION);
    expect(attempts).toHaveLength(2); // it really tried again
    restore();
  });

  test('the loading flag is cleared whether the fetch succeeds or fails', async () => {
    const el = await sessionsList();
    const restoreFail = failing(() => {});
    await (el as any).expandSessionTrace(SESSION);
    expect((el as any).loadingSessions[SESSION]).toBeUndefined();
    restoreFail();

    const restoreOk = stubFetch(childPayload(['e1']));
    await (el as any).expandSessionTrace(SESSION);
    expect((el as any).loadingSessions[SESSION]).toBeUndefined();
    restoreOk();
  });

  test('the list is left exactly as it was', async () => {
    const el = await sessionsList();
    const before = ids(el);
    const restore = failing(() => {});

    await (el as any).expandSessionTrace(SESSION);

    expect(ids(el)).toEqual(before);
    restore();
  });
});
