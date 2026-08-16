// Expanding and collapsing a trace in the log list.
//
// Tree view shows one row per trace until the reader opens it. That toggle mutates `show`
// and `expanded` on rows in place and maintains a parallel `expandedTraces` map that the
// worker re-reads on every regroup — so a disagreement between them either hides rows the
// reader opened or leaks another trace's children into the list. Untested until now.
import { describe, test, expect } from 'vitest';
import { serverTransport, logPage, mountList, COLS } from './log-list-harness';
import { groupSpans } from '../src/log-worker-functions';

// A trace: one root server span with two child spans, plus an unrelated standalone log.
const TRACE = 'trace-1';
const ts = (n: number) => new Date(Date.parse('2024-01-01T00:00:00Z') + n).toISOString();
const spanRow = (id: string, parent: string, kind = 'server') => [ts(0), id, TRACE, parent, kind, id, 10, Date.parse(ts(0)) * 1e6];

const tracePage = () => ({
  logsData: [spanRow('root', ''), spanRow('child-a', 'root', 'client'), spanRow('child-b', 'root', 'client')],
  colIdxMap: COLS,
  traces: [
    {
      trace_id: TRACE,
      start_time: Date.parse(ts(0)) * 1e6,
      duration: 10,
      trace_start_time: ts(0),
      root: 'root',
      children: { root: ['child-a', 'child-b'] },
    },
  ],
});

const withTrace = async () => {
  const el = await mountList();
  el.transport = async () => {
    const p = tracePage();
    return {
      tree: groupSpans(p.logsData, p.colIdxMap as any, {}, false, p.traces as any),
      meta: {
        serviceColors: {}, nextUrl: '', recentUrl: '', cols: Object.keys(COLS),
        colIdxMap: COLS, count: 3, traces: p.traces, hasMore: false, queryResultCount: 3,
      } as any,
    };
  };
  await el.fetchData('first', true);
  return el;
};

// What the list would actually render: collapsed children are filtered out in tree view.
const renderedIds = (el: any) => (el.virtualListItems as any[]).filter((i) => 'id' in i).map((i) => i.id);
const rowById = (el: any, id: string) => (el.spanListTree as any[]).find((r) => r.id === id);

describe('expanding a trace', () => {
  test('a collapsed trace contributes only its root row', async () => {
    const el = await withTrace();
    expect(renderedIds(el)).toEqual(['root']);
  });

  test('expanding reveals the children beneath the root', async () => {
    const el = await withTrace();

    (el as any).expandTrace(TRACE, 'root');

    expect(renderedIds(el)).toEqual(['root', 'child-a', 'child-b']);
    expect(rowById(el, 'root').expanded).toBe(true);
  });

  test('collapsing hides them again and the row count returns to one', async () => {
    const el = await withTrace();
    (el as any).expandTrace(TRACE, 'root');

    (el as any).expandTrace(TRACE, 'root');

    expect(renderedIds(el)).toEqual(['root']);
    expect(rowById(el, 'root').expanded).toBe(false);
  });

  // The map is what the worker re-reads when a new page is merged; if it disagrees with
  // the in-place `show` flags, a regroup silently re-collapses what the reader opened.
  test('the expanded map agrees with what is on screen', async () => {
    const el = await withTrace();

    (el as any).expandTrace(TRACE, 'root');
    const openIds = Object.entries((el as any).expandedTraces).filter(([, v]) => v).map(([k]) => k);
    expect(openIds.sort()).toEqual(['child-a', 'child-b', 'root']);

    (el as any).expandTrace(TRACE, 'root');
    const stillOpen = Object.entries((el as any).expandedTraces).filter(([, v]) => v).map(([k]) => k);
    expect(stillOpen).toEqual([]);
  });

  test('expanding does not pin an oldest-first list back to the newest edge', async () => {
    const el = await mountList({ flipDirection: true, shouldScrollToBottom: true } as any);
    (el as any).expandTrace(TRACE, 'root');
    expect(el.shouldScrollToBottom).toBe(false);
  });

  test('toggling a trace that is not loaded changes nothing and does not throw', async () => {
    const el = await withTrace();
    const before = renderedIds(el);

    expect(() => (el as any).expandTrace('no-such-trace', 'no-such-span')).not.toThrow();

    expect(renderedIds(el)).toEqual(before);
  });
});

describe('expansion survives more data arriving', () => {
  test('an expanded trace stays open when a later page is merged', async () => {
    const el = await withTrace();
    (el as any).expandTrace(TRACE, 'root');
    expect(renderedIds(el)).toHaveLength(3);

    el.transport = serverTransport(logPage(['older-1']));
    await el.fetchData('older', false, false, true);

    expect(renderedIds(el)).toContain('child-a');
    expect(renderedIds(el)).toContain('older-1');
  });

  test('list view shows every span regardless of expansion state', async () => {
    const el = await withTrace();
    el.changeView('list');
    expect(renderedIds(el)).toEqual(['root', 'child-a', 'child-b']);

    el.changeView('tree');
    expect(renderedIds(el)).toEqual(['root']);
  });
});
