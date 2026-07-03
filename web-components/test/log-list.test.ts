import { describe, test, expect, beforeEach } from 'vitest';
import { dedupeById } from '../src/log-list-utils';
import { LogList } from '../src/log-list';
import { row, fakeTransport, ids, mountList } from './log-list-harness';

describe('dedupeById', () => {
  test('keeps first occurrence, preserves order, drops repeats', () => {
    const r = dedupeById([row('a'), row('b'), row('a'), row('c'), row('b')]);
    expect(r.map((x) => x.id)).toEqual(['a', 'b', 'c']);
  });
  test('handles empty + all-unique unchanged', () => {
    expect(dedupeById([])).toEqual([]);
    expect(dedupeById([row('a'), row('b')]).map((x) => x.id)).toEqual(['a', 'b']);
  });
});

describe('LogList load-more', () => {
  let el: LogList;
  beforeEach(async () => {
    el = await mountList();
  });

  // Regression: cursor pagination uses an inclusive timestamp boundary, so page 2
  // re-returns the last row of page 1. The merge must not duplicate that row in
  // the table (the reported "load more pulls duplicate data" bug).
  test('does not duplicate the boundary row across pages', async () => {
    el.transport = fakeTransport(
      { tree: [row('1'), row('2'), row('3')] },
      { tree: [row('3'), row('4'), row('5')] }, // '3' repeats the boundary
    );
    await el.fetchData('initial', false, false, false);
    await el.fetchData('loadmore', false, false, true);

    expect(ids(el)).toEqual(['1', '2', '3', '4', '5']);
    expect(new Set(ids(el)).size).toBe(ids(el).length); // no dups
  });

  test('clean page (no overlap) appends all rows in order', async () => {
    el.transport = fakeTransport({ tree: [row('1'), row('2')] }, { tree: [row('3'), row('4')] });
    await el.fetchData('initial', false, false, false);
    await el.fetchData('loadmore', false, false, true);
    expect(ids(el)).toEqual(['1', '2', '3', '4']);
  });

  // Regression: an empty load-more page must stop pagination (hasMore=false) even
  // when the server still reports hasMore:true. Otherwise the load-more sentinel
  // keeps re-firing and refetches the same window — which re-appended the page
  // ("server returns nothing → duplicates the page and puts it in again").
  test('empty load-more page stops pagination and does not re-add rows', async () => {
    el.transport = fakeTransport(
      { tree: [row('1'), row('2'), row('3')] },
      { tree: [], meta: { hasMore: true } }, // server falsely claims more, sends nothing
      { tree: [row('1'), row('2'), row('3')] }, // a stray refetch would re-serve the window
    );
    await el.fetchData('initial', false, false, false);
    await el.fetchData('loadmore', false, false, true);
    expect((el as any).hasMore).toBe(false); // pagination halted → sentinel/observer gone
    expect(ids(el)).toEqual(['1', '2', '3']);

    // Even if a refetch slips through, rows are never duplicated.
    await el.fetchData('loadmore', false, false, true);
    expect(ids(el)).toEqual(['1', '2', '3']);
  });

  // Regression: with rows on screen, running a new query (refresh) that returns
  // nothing must clear the list and show the empty state — not leave the previous
  // query's results persisted, which reads as "these are results for the new query".
  test('refresh with empty result clears stale rows (shows empty, not old data)', async () => {
    el.transport = fakeTransport(
      { tree: [row('1'), row('2'), row('3')] },
      { tree: [], meta: { hasMore: false, count: 0 } }, // new query → no matches
    );
    await el.fetchData('initial', false, false, false);
    expect(ids(el)).toEqual(['1', '2', '3']);

    await el.fetchData('newquery', true, false, false); // isRefresh
    expect(ids(el)).toEqual([]);
    expect((el as any).loadedCount).toBe(0);
  });
});

// The `cols` URL param is a DELTA over the server's default column set: a bare token
// adds a column, `-token` hides a default. Toggles must be exact inverses so the param
// stays minimal and reversible, and no client state can write a table-collapsing list.
// Regression for the popover-migration breakage (add/remove column stopped working).
describe('LogList toggleColumnOnTable (URL delta)', () => {
  let el: LogList;
  beforeEach(async () => {
    el = await mountList();
    el.transport = fakeTransport({ tree: [] }); // absorb the refetch toggle kicks off
    window.history.replaceState({}, '', '/p/x/log_explorer');
  });
  const cols = () => new URLSearchParams(window.location.search).get('cols');
  const setShown = (c: string[]) => ((el as any).logsColumns = c);

  test('adding a hidden column appends a bare token', () => {
    setShown(['id', 'timestamp']);
    expect(el.toggleColumnOnTable('resource.service.name')).toBe(true);
    expect(cols()).toBe('resource.service.name');
  });

  test('removing a shown default appends a -token', () => {
    setShown(['id', 'timestamp', 'resource.service.name']);
    expect(el.toggleColumnOnTable('resource.service.name')).toBe(false);
    expect(cols()).toBe('-resource.service.name');
  });

  test('removing a column that was explicitly added round-trips to empty', () => {
    window.history.replaceState({}, '', '/p/x/log_explorer?cols=route');
    setShown(['id', 'route']);
    expect(el.toggleColumnOnTable('route')).toBe(false);
    expect(cols()).toBe(null); // dropped, not left as `-route`
  });

  test('re-adding a hidden default drops its -token (not a bare add)', () => {
    window.history.replaceState({}, '', '/p/x/log_explorer?cols=-service');
    setShown(['id', 'timestamp']); // service currently hidden
    expect(el.toggleColumnOnTable('service')).toBe(true);
    expect(cols()).toBe(null);
  });

  test('preserves unrelated tokens and dedupes', () => {
    window.history.replaceState({}, '', '/p/x/log_explorer?cols=route%2C-service');
    setShown(['id', 'route']); // route shown, service hidden, duration hidden
    expect(el.toggleColumnOnTable('duration')).toBe(true);
    expect(cols()).toBe('route,-service,duration');
  });
});
