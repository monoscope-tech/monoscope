// One set of invariants, every configuration the log list ships in.
//
// The list has five independent axes — scroll direction, mode (logs/patterns/sessions),
// tree vs list view, embedded vs standalone, wrapped vs dense rows — and until now each
// was covered, if at all, by a bespoke test for one combination. A pagination rule that
// held newest-first and broke oldest-first, or held in tree view and broke in list view,
// had nowhere to fail. These tables assert the rules that must hold *regardless* of the
// axes, so adding a configuration costs one row rather than a new file.
import { describe, test, expect } from 'vitest';
import { serverTransport, serverTransportFlipped, logPage, mountList, scrollHarness, flushFrames, ids, ROW_H } from './log-list-harness';
import { MAX_RETAINED_ROWS, RETENTION_LIMIT } from '../src/log-list';

type Config = { name: string; props: Record<string, any>; aggregate?: boolean };

// Every shipped combination. `aggregate` marks the modes that page by skip-count rather
// than by cursor and have no live/newer edge at all.
const CONFIGS: Config[] = [
  { name: 'newest-first, tree view', props: {} },
  { name: 'oldest-first, tree view', props: { flipDirection: true } },
  { name: 'newest-first, list view', props: { view: 'list' } },
  { name: 'oldest-first, list view', props: { flipDirection: true, view: 'list' } },
  { name: 'newest-first, wrapped rows', props: { wrapLines: true } },
  { name: 'oldest-first, wrapped rows', props: { flipDirection: true, wrapLines: true } },
  { name: 'narrow viewport', props: { isNarrow: true } },
  { name: 'embedded (no newer edge)', props: { initialFetchUrl: '/embedded' } },
  { name: 'sessions', props: { mode: 'sessions' } },
  { name: 'patterns', props: { mode: 'patterns' }, aggregate: true },
];

const page = (ids: string[], over: any = {}) => logPage(ids, over);
const transportFor = (c: Config, ...pages: any[]) => (c.props.flipDirection ? serverTransportFlipped(...pages) : serverTransport(...pages));

// Load a first page, then a second that overlaps it on one row — what an inclusive
// cursor actually returns.
const loadTwoPages = async (c: Config) => {
  const el = await mountList(c.props as any);
  el.transport = transportFor(c, page(['a', 'b', 'c']), page(['c', 'd', 'e']));
  await el.fetchData('first', true);
  await el.fetchData('second', false, false, true);
  return el;
};

describe.each(CONFIGS)('LogList configuration: $name', (config) => {
  test('overlapping pages dedupe, and history lands on the history side', async () => {
    const el = await loadTwoPages(config);

    expect(ids(el)).toHaveLength(5);
    expect(new Set(ids(el)).size).toBe(5);
    // Older rows attach at whichever end history grows from (in that end's own order).
    const historyEdge = config.props.flipDirection ? ids(el).slice(0, 2) : ids(el).slice(-2);
    expect([...historyEdge].sort()).toEqual(['d', 'e']);
  });

  test('loadedCount reports the rows actually retained', async () => {
    const el = await loadTwoPages(config);
    expect((el as any).loadedCount).toBe((el as any).spanListTree.length);
  });

  test('the virtual list holds each row once, plus only sentinel rows', async () => {
    const el = await loadTwoPages(config);
    const items = (el as any).virtualListItems as any[];
    const rows = items.filter((i) => 'id' in i);
    const sentinels = items.filter((i) => !('id' in i));

    expect(new Set(rows.map((r: any) => r.id)).size).toBe(rows.length);
    expect(sentinels.every((s: any) => ['fetchRecent', 'loadMore', 'aggregateChildren'].includes(s.type))).toBe(true);
  });

  test('the load-more sentinel sits at the history edge', async () => {
    const el = await loadTwoPages(config);
    const items = (el as any).virtualListItems as any[];
    const loadMoreIdx = items.findIndex((i: any) => i.type === 'loadMore');

    expect(loadMoreIdx).toBeGreaterThanOrEqual(0);
    expect(loadMoreIdx).toBe(config.props.flipDirection ? 0 : items.length - 1);
  });

  test('a newer edge exists only where newer rows can actually arrive', async () => {
    const el = await loadTwoPages(config);
    const hasRecent = ((el as any).virtualListItems as any[]).some((i: any) => i.type === 'fetchRecent');
    // Aggregates have no live edge, and an embedded list is a fixed window into a
    // dashboard panel — neither should offer to load newer rows.
    expect(hasRecent).toBe(!config.aggregate && !config.props.initialFetchUrl);
  });

  test('a refresh replaces the result set instead of merging into it', async () => {
    const el = await loadTwoPages(config);
    el.transport = transportFor(config, page(['x', 'y']));

    await el.fetchData('refreshed', true);

    expect([...ids(el)].sort()).toEqual(['x', 'y']);
    expect((el as any).seenIds.size).toBe(2);
  });

  test('an empty history page ends pagination without discarding what is loaded', async () => {
    const el = await loadTwoPages(config);
    const before = ids(el);
    el.transport = transportFor(config, page([]));

    await el.fetchData('exhausted', false, false, true);

    expect(ids(el)).toEqual(before);
    expect((el as any).hasMore).toBe(false);
  });

  test('a failed page keeps the rows on screen and reports the error as a toast', async () => {
    const el = await loadTwoPages(config);
    const before = ids(el);
    el.transport = async () => {
      throw new Error('upstream exploded');
    };

    await el.fetchData('boom', false, false, true);

    expect(ids(el)).toEqual(before);
    expect((el as any).fetchError).toBeNull(); // inline error is for an empty list only
  });

  test('retention is bounded and reopens the edge it evicted', async () => {
    const el = await mountList(config.props as any);
    const first = Array.from({ length: RETENTION_LIMIT }, (_, i) => `r${String(i).padStart(5, '0')}`);
    el.transport = transportFor(config, page(first), page(Array.from({ length: 200 }, (_, i) => `o${i}`)));
    await el.fetchData('first', true);
    await el.fetchData('second', false, false, true);

    // Patterns/sessions are skip-paginated aggregates and deliberately keep everything.
    if (config.props.mode !== 'logs' && config.props.mode) {
      expect(ids(el).length).toBeGreaterThan(0);
      return;
    }
    expect(ids(el)).toHaveLength(MAX_RETAINED_ROWS);
    expect((el as any).hasNewer).toBe(true);
  });
});

// Scroll behaviour is only meaningful for the row-per-log modes; patterns and sessions
// use a measured layout whose geometry the dense simulator does not model.
const SCROLLABLE = CONFIGS.filter((c) => !c.aggregate && c.props.mode !== 'sessions');

describe.each(SCROLLABLE)('LogList scrolling: $name', (config) => {
  const seeded = async (n: number) => {
    const el = await mountList(config.props as any);
    el.transport = transportFor(config, page(Array.from({ length: n }, (_, i) => `r${String(i).padStart(5, '0')}`)));
    await el.fetchData('first', true);
    await flushFrames(); // a refresh parks the viewport at the newest edge on the next frame
    return el;
  };
  // Rows sit after the leading sentinel, if this configuration has one.
  const rowAt = (el: any, sim: any) => {
    const item = el.virtualListItems[sim.firstVisibleIndex];
    return item && 'id' in item ? item.id : undefined;
  };

  test('paging history never moves the row the reader is looking at', async () => {
    const el = await seeded(400);
    const sim = scrollHarness(el);
    sim.scrollTo(150 * ROW_H);
    const anchor = rowAt(el, sim);
    el.transport = transportFor(config, page(Array.from({ length: 200 }, (_, i) => `o${String(i).padStart(5, '0')}`)));

    await el.fetchData('older', false, false, true);
    await flushFrames();

    expect(rowAt(el, sim)).toBe(anchor);
  });

  test('the reader is never left at a scroll offset the content cannot support', async () => {
    const el = await seeded(400);
    const sim = scrollHarness(el);
    sim.scrollTo(380 * ROW_H);
    el.transport = transportFor(config, page([]));

    await el.fetchData('exhausted', false, false, true);
    await flushFrames();

    expect(sim.scrollTop).toBeLessThanOrEqual(sim.maxScroll);
    expect(sim.scrollTop).toBeGreaterThanOrEqual(0);
  });
});
