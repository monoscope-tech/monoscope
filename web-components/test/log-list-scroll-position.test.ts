// Where does the reader end up after a page merge?
//
// Every other log-list test mocks captureScrollAnchor/restoreScrollAnchor, so none
// of them could observe the answer. These run the real anchoring code against the
// simulated layout in `scrollHarness`, and assert the only thing the user judges
// the list by: the row under their eyes did not move.
//
// Regression origin: "scroll down to Show earlier events, click it, and instead of
// the list continuing you are thrown back to the top and the load-newer row at the
// top fires." The retention window evicting rows remounts the virtualizer
// (`keyed(virtualizerEpoch)`), a fresh virtualizer has a zero-height scroll range
// for one frame, and the browser clamps scrollTop to 0 on that frame.
import { describe, test, expect, vi } from 'vitest';
import { row, serverTransport, serverTransportFlipped, deferredTransport, logPage, treeFromLogs, mountList, scrollHarness, flushFrames, fireSentinel, ROW_H, ids } from './log-list-harness';
import { MAX_RETAINED_ROWS } from '../src/log-list';

// The row the reader is looking at: first data row at the top of the viewport.
const topRowId = (el: any, sim: any): string | undefined => {
  const item = el.virtualListItems[sim.firstVisibleIndex];
  return item && 'id' in item ? item.id : undefined;
};

// A list already holding `n` rows, newest-first, with more history available.
const seeded = async (n: number, props: any = {}) => {
  const el = await mountList(props);
  const initial = Array.from({ length: n }, (_, i) => row(`r${String(i).padStart(5, '0')}`));
  (el as any).spanListTree = initial;
  (el as any).seenIds = new Set(initial.map((r) => r.id));
  (el as any).hasMore = true;
  (el as any).updateVisibleItems();
  await el.updateComplete;
  return el;
};

const olderPage = (from: number, count: number) =>
  logPage(Array.from({ length: count }, (_, i) => `o${String(from + i).padStart(5, '0')}`));

describe('LogList — the reader keeps their place across a load-more', () => {
  test('an ordinary newest-first load-more leaves the viewport exactly where it was', async () => {
    const el = await seeded(300);
    const sim = scrollHarness(el);
    sim.scrollTo(200 * ROW_H);
    const before = { top: sim.scrollTop, id: topRowId(el, sim) };
    el.transport = serverTransport(olderPage(0, 200));

    await el.fetchData('older', false, false, true);
    await flushFrames();

    expect(sim.scrollTop).toBe(before.top); // appended below the fold: nothing moves
    expect(topRowId(el, sim)).toBe(before.id);
  });

  test('a load-more that evicts the retained window keeps the reader on the same row', async () => {
    const el = await seeded(MAX_RETAINED_ROWS);
    const sim = scrollHarness(el);
    sim.scrollToBottom();
    const anchorId = topRowId(el, sim);
    el.transport = serverTransport(olderPage(0, 400));

    await el.fetchData('older', false, false, true);
    await flushFrames();

    // Eviction dropped the newest 400 rows, so the absolute offset must change —
    // but the row the reader was reading must still be the row at the top.
    expect(ids(el)).toHaveLength(MAX_RETAINED_ROWS);
    expect(topRowId(el, sim)).toBe(anchorId);
    expect(sim.atTop).toBe(false);
  });

  test('restores after the real virtualizer leaves scrollToIndex a no-op', async () => {
    const el = await seeded(MAX_RETAINED_ROWS);
    const sim = scrollHarness(el, { virtualizerScrollToIndexWorks: false });
    sim.scrollToBottom();
    const anchorId = topRowId(el, sim);
    el.transport = serverTransport(olderPage(0, 400));

    await el.fetchData('older', false, false, true);
    await flushFrames(6);

    expect(topRowId(el, sim)).toBe(anchorId);
    expect(sim.atTop).toBe(false);
  });

  test('does not trust stale pre-remount row geometry', async () => {
    const el = await seeded(MAX_RETAINED_ROWS);
    const sim = scrollHarness(el);
    sim.scrollToBottom();
    const anchorId = topRowId(el, sim);
    const align = (el as any).alignAnchor.bind(el);
    vi.spyOn(el as any, 'alignAnchor').mockImplementation((anchor) => (sim.atTop ? true : align(anchor)));
    el.transport = serverTransport(olderPage(0, 400));

    await el.fetchData('older', false, false, true);
    await flushFrames(6);

    // During a keyed child update the old <tr> can survive one host update. Its rectangle
    // looks valid even though the new virtualizer is about to replace it with an empty range.
    expect(topRowId(el, sim)).toBe(anchorId);
    expect(sim.atTop).toBe(false);
  });

  test('"Show earlier events" continues the list instead of jumping to the top', async () => {
    // hasMore=false is what swaps the load-more row for "Show earlier events".
    const el = await seeded(MAX_RETAINED_ROWS);
    (el as any).hasMore = false;
    (el as any).expandTimeRange = true;
    (el as any).updateVisibleItems();
    await el.updateComplete;
    const sim = scrollHarness(el);
    sim.scrollToBottom();
    const anchorId = topRowId(el, sim);
    el.transport = serverTransport(olderPage(0, 400));

    // Exactly what the button's @click does.
    el.renderExpandTimeRangeButton();
    await el.fetchData('expanded', false, false, true);
    await flushFrames();

    expect(topRowId(el, sim)).toBe(anchorId);
    expect(sim.atTop).toBe(false);
  });

  test('oldest-first load-more holds position while older rows are prepended', async () => {
    const el = await seeded(300, { flipDirection: true });
    const sim = scrollHarness(el);
    sim.scrollTo(50 * ROW_H);
    const anchorId = topRowId(el, sim);
    el.transport = serverTransport(olderPage(0, 200));

    await el.fetchData('older', false, false, true);
    await flushFrames();

    expect(topRowId(el, sim)).toBe(anchorId);
    expect(sim.scrollTop).toBeGreaterThan(50 * ROW_H); // 200 rows inserted above
  });
});

describe('LogList — newer rows arriving above the viewport', () => {
  test('the 15-second live-range refresh merges new rows without replacing a deep reader', async () => {
    const el = await seeded(1_200);
    const sim = scrollHarness(el);
    sim.scrollTo(800 * ROW_H);
    const anchorId = topRowId(el, sim);
    el.transport = serverTransport(logPage(['live-new']));

    window.dispatchEvent(new CustomEvent('update-query', { detail: { source: 'auto-refresh' } }));
    await vi.waitFor(() => expect((el.transport as any).urls).toHaveLength(1));
    await flushFrames();

    expect(ids(el)[0]).toBe('live-new');
    expect(ids(el)).toContain(anchorId);
    expect(topRowId(el, sim)).toBe(anchorId);
  });

  test('a recent fetch merged mid-list keeps the reader on the same row', async () => {
    const el = await seeded(300);
    (el as any).recentFetchUrl = 'newer';
    const sim = scrollHarness(el);
    sim.scrollTo(120 * ROW_H);
    const anchorId = topRowId(el, sim);
    el.transport = serverTransport(logPage(['n1', 'n2', 'n3', 'n4', 'n5']));

    await el.fetchData('newer', false, true);
    await flushFrames();

    expect(topRowId(el, sim)).toBe(anchorId);
    expect(sim.scrollTop).toBe((120 + 5) * ROW_H); // pushed down by exactly the 5 new rows
  });

  test('a recent fetch while parked at the top leaves the newest rows in view', async () => {
    const el = await seeded(300);
    const sim = scrollHarness(el);
    sim.scrollTo(0);
    el.transport = serverTransport(logPage(['n1', 'n2', 'n3']));

    await el.fetchData('newer', false, true);
    await flushFrames();

    expect(sim.atTop).toBe(true);
    expect(ids(el).slice(0, 3)).toEqual(['n1', 'n2', 'n3']);
  });
});

describe('LogList — "Show earlier events" feedback', () => {
  test('the row stays mounted and busy while its page is in flight', async () => {
    const el = await seeded(50);
    (el as any).hasMore = false;
    (el as any).expandTimeRange = true;
    (el as any).updateVisibleItems();
    await el.updateComplete;
    const sim = scrollHarness(el);
    const transport = deferredTransport();
    el.transport = transport;

    const idle = (await sim.mountSentinels()).loadMore;
    expect(idle?.textContent).toContain('Show earlier events');

    idle!.click(); // the real @click handler, not fetchData directly
    await el.updateComplete;
    const during = (await sim.mountSentinels()).loadMore;

    // The reader clicked a row that reports its own progress. Dropping it mid-flight
    // left them with no indication anything was happening, and shifted the list by
    // the row's height at the same time.
    expect(during?.textContent).toContain('Show earlier events');
    expect(during?.getAttribute('aria-busy')).toBe('true');

    transport.settle(0, treeFromLogs(['o1', 'o2']));
  });

  test('clicking the busy row again does not widen the time range twice', async () => {
    const el = await seeded(50);
    (el as any).hasMore = false;
    (el as any).expandTimeRange = true;
    (el as any).updateVisibleItems();
    await el.updateComplete;
    const sim = scrollHarness(el);
    el.transport = deferredTransport();
    // expandTimeRangeUrl widens `since` and rewrites history *before* fetchData's
    // in-flight guard can bail, so an impatient second click doubled the range.
    const expand = vi.spyOn(el as any, 'expandTimeRangeUrl');

    (await sim.mountSentinels()).loadMore!.click();
    await el.updateComplete;
    (await sim.mountSentinels()).loadMore!.click();

    expect(expand).toHaveBeenCalledOnce();
  });

  test('the "N new" pill scrolls its own list, not the first one on the page', async () => {
    const other = document.createElement('div'); // an earlier list's container
    other.id = 'logs_list_container_inner';
    other.scrollTop = 0;
    document.body.prepend(other);
    const el = await seeded(300);
    const sim = scrollHarness(el);
    sim.scrollTo(120 * ROW_H);
    (el as any).recentDataToBeAdded = [row('n1')];

    el.handleRecentClick();

    expect(sim.atTop).toBe(true);
    expect(ids(el)).toContain('n1');
    other.remove();
  });
});

describe('LogList — oldest-first stays pinned only while the reader is at the newest edge', () => {
  test('scrolling up to page history unpins the list', async () => {
    const el = await seeded(300, { flipDirection: true, shouldScrollToBottom: true });
    const sim = scrollHarness(el);
    sim.scrollToBottom();
    expect(el.shouldScrollToBottom).toBe(true);

    sim.scrollTo(40 * ROW_H); // reader scrolls up toward "Load more"

    expect(el.shouldScrollToBottom).toBe(false);
  });

  test('a load-more requested from mid-list does not snap back to the newest edge', async () => {
    const el = await seeded(300, { flipDirection: true, shouldScrollToBottom: true });
    const sim = scrollHarness(el);
    sim.scrollToBottom();
    sim.scrollTo(40 * ROW_H);
    const anchorId = topRowId(el, sim);
    el.transport = serverTransport(olderPage(0, 200));

    await el.fetchData('older', false, false, true);
    await flushFrames();

    expect(topRowId(el, sim)).toBe(anchorId);
    expect(sim.scrollTop).toBeLessThan(sim.maxScroll);
  });

  test('scrolling back to the newest edge re-pins it', async () => {
    const el = await seeded(300, { flipDirection: true, shouldScrollToBottom: true });
    const sim = scrollHarness(el);
    sim.scrollTo(40 * ROW_H);
    expect(el.shouldScrollToBottom).toBe(false);

    sim.scrollToBottom();

    expect(el.shouldScrollToBottom).toBe(true);
  });
});

describe('LogList — automatic fetches stand down while the list is repositioning', () => {
  // The reported bug, end to end: page back through history until the retention
  // window evicts, and the top "Load newer events" sentinel — which only becomes
  // live *because* of that eviction — fires on the one frame where the remounted
  // virtualizer has no height and therefore has both edges inside the viewport.
  test('the eviction remount does not fire the top load-newer sentinel', async () => {
    const el = await seeded(MAX_RETAINED_ROWS);
    const sim = scrollHarness(el);
    sim.scrollToBottom();
    const anchorId = topRowId(el, sim);
    el.transport = serverTransport(olderPage(0, 400));

    await el.fetchData('older', false, false, true);
    expect(el.hasNewer).toBe(true); // eviction reopened the newer edge, arming the sentinel
    const sentinels = await sim.mountSentinels();
    const recent = vi.spyOn(el as any, 'buildRecentFetchUrl');
    fireSentinel(sentinels.recent);
    await flushFrames();

    expect(recent).not.toHaveBeenCalled();
    expect(sim.atTop).toBe(false);
    expect(topRowId(el, sim)).toBe(anchorId);
  });

  test('an auto-loaded newer page never yanks the viewport to the top', async () => {
    // Only an explicit click on "Load newer events" should reveal the new rows;
    // the observer firing means the reader is already at that edge.
    const el = await seeded(400);
    (el as any).hasNewer = true;
    const sim = scrollHarness(el);
    sim.scrollTo(120 * ROW_H);
    el.transport = serverTransport(logPage(['n1', 'n2']));
    const sentinels = await sim.mountSentinels();

    fireSentinel(sentinels.recent);
    await flushFrames();

    expect(sim.atTop).toBe(false);
  });

  test('proximity prefetch does not fire off the collapsed post-eviction range', async () => {
    const el = await seeded(MAX_RETAINED_ROWS);
    const sim = scrollHarness(el);
    sim.scrollToBottom();
    sim.emitVisibility();
    el.transport = serverTransport(olderPage(0, 400));

    await el.fetchData('older', false, false, true);
    const fetchSpy = vi.spyOn(el, 'fetchData');
    sim.emitVisibility(); // the collapsed frame reports a range starting at 0
    await flushFrames();

    expect(fetchSpy).not.toHaveBeenCalled();
  });

  test('the remount recovery range is not mistaken for another page of user scrolling', async () => {
    const el = await seeded(MAX_RETAINED_ROWS);
    const sim = scrollHarness(el);
    sim.scrollToBottom();
    sim.emitVisibility();
    el.transport = serverTransport(olderPage(0, 400));

    await el.fetchData('older', false, false, true);
    const fetchSpy = vi.spyOn(el, 'fetchData');
    // A real keyed remount reports its collapsed range and then its restored deep range.
    // Neither event came from the reader moving toward history.
    el.handleVisibilityChange({ first: 0, last: 0 });
    await flushFrames();
    sim.emitVisibility();

    expect(fetchSpy).not.toHaveBeenCalled();
  });

  test('once the list has settled, the top sentinel loads newer events again', async () => {
    const el = await seeded(MAX_RETAINED_ROWS);
    const sim = scrollHarness(el);
    sim.scrollToBottom();
    el.transport = serverTransport(olderPage(0, 400));
    await el.fetchData('older', false, false, true);
    await flushFrames();

    const recent = vi.spyOn(el as any, 'buildRecentFetchUrl').mockReturnValue('newer');
    sim.scrollTo(0);
    const sentinels = await sim.mountSentinels();
    fireSentinel(sentinels.recent);
    await flushFrames();

    expect(recent).toHaveBeenCalled(); // suspension is a frame, not a permanent latch
  });

  test('the bottom sentinel still pages history after an eviction settles', async () => {
    const el = await seeded(MAX_RETAINED_ROWS);
    const sim = scrollHarness(el);
    sim.scrollToBottom();
    el.transport = serverTransport(olderPage(0, 400), olderPage(400, 100));
    await el.fetchData('older', false, false, true);
    await flushFrames();

    const sentinels = await sim.mountSentinels();
    fireSentinel(sentinels.loadMore);
    await new Promise((r) => setTimeout(r, 350)); // the sentinel's 300ms debounce
    await flushFrames();

    expect((el.transport as any).urls.length).toBeGreaterThan(1);
  });

  test('range prefetch and the bottom sentinel do not load two pages for one scroll', async () => {
    const el = await seeded(300);
    const sim = scrollHarness(el);
    el.transport = serverTransport(olderPage(0, 100), olderPage(100, 100));
    const sentinels = await sim.mountSentinels();

    // The sentinel queues its debounced fallback just before the visibility-range prefetch
    // starts. A fast response can finish before that 300ms timer fires; the stale timer must
    // not interpret the same scroll as a request for another page.
    fireSentinel(sentinels.loadMore);
    await el.fetchData('range-prefetch', false, false, true);
    await new Promise((r) => setTimeout(r, 350));
    await flushFrames();

    expect((el.transport as any).urls).toEqual(['range-prefetch']);
  });
});

describe('LogList — eviction that removes the row the reader was anchored to', () => {
  test('live-tail eviction at the history edge does not dump the reader at the top', async () => {
    // Newest-first: a recent fetch prepends newer rows and evicts the OLDEST ones —
    // exactly where a reader who has paged deep into history is sitting.
    const el = await seeded(MAX_RETAINED_ROWS);
    (el as any).recentFetchUrl = 'newer';
    const sim = scrollHarness(el);
    sim.scrollToBottom();
    el.transport = serverTransport(logPage(Array.from({ length: 400 }, (_, i) => `n${String(i).padStart(5, '0')}`)));

    await el.fetchData('newer', false, true);
    await flushFrames();

    expect(sim.atTop).toBe(false);
  });
});

// The suspension counter is mine, and its failure mode is worse than the bug it fixes: if
// it ever fails to return to zero, every automatic fetch is disabled for the life of the
// page and the list simply stops loading more, with nothing on screen to say why.
describe('LogList — the repositioning guard always releases', () => {
  const settling = (el: any) => el.scrollSettling;

  test('returns to zero after a plain load-more', async () => {
    const el = await seeded(300);
    scrollHarness(el);
    el.transport = serverTransport(olderPage(0, 100));

    await el.fetchData('older', false, false, true);
    await flushFrames();

    expect(settling(el)).toBe(0);
  });

  test('returns to zero after an eviction remount', async () => {
    const el = await seeded(MAX_RETAINED_ROWS);
    const sim = scrollHarness(el);
    sim.scrollToBottom();
    el.transport = serverTransport(olderPage(0, 400));

    await el.fetchData('older', false, false, true);
    await flushFrames();

    expect(settling(el)).toBe(0);
  });

  test('does not drift across many merges', async () => {
    const el = await seeded(300);
    scrollHarness(el);

    for (let i = 0; i < 8; i++) {
      el.transport = serverTransport(olderPage(i * 50, 50));
      await el.fetchData(`older-${i}`, false, false, true);
    }
    await flushFrames();

    expect(settling(el)).toBe(0);
  });

  // A restore that throws mid-flight must still hand the counter back, or one bad frame
  // disables paging permanently. It must also not escape: every caller is fire-and-forget,
  // so a rejection here would surface as an unhandled rejection rather than anything the
  // reader or an error reporter can act on.
  test('releases and reports, rather than rejecting, when the restore fails', async () => {
    const el = await seeded(300, { flipDirection: true });
    scrollHarness(el);
    const reported = vi.spyOn(console, 'error').mockImplementation(() => {});
    vi.spyOn(el as any, 'alignAnchor').mockImplementation(() => {
      throw new Error('layout blew up');
    });
    el.transport = serverTransportFlipped(olderPage(0, 100));

    await el.fetchData('older', false, false, true);
    await flushFrames();

    expect(settling(el)).toBe(0);
    expect(reported).toHaveBeenCalledWith('[log-list] scroll restore failed', expect.any(Error));
  });

  test('releases when the virtualizer never resolves layoutComplete', async () => {
    const el = await seeded(300, { flipDirection: true });
    const sim = scrollHarness(el);
    Object.defineProperty(sim.virtualizer, 'layoutComplete', { get: () => new Promise<void>(() => {}) });
    el.transport = serverTransportFlipped(olderPage(0, 100));

    await el.fetchData('older', false, false, true);
    await new Promise((resolve) => setTimeout(resolve, 350));
    await flushFrames();

    // A permanently pending layout promise used to leave scrollSettling > 0 forever,
    // silently disabling both edge observers and proximity pagination for this list.
    expect(settling(el)).toBe(0);
  });
});

// Reported: "scrolling down quickly triggers Show earlier events, and that works, but
// sometimes it instead triggers the load-newer row at the TOP of the list and the reader
// loses their place." The top sentinel firing is correct behaviour — it fires because the
// reader IS at the top. What put them there is a load-more eviction whose anchor restore
// gave up: the remount clamps scrollTop to 0, and nothing moves them back.
describe('LogList — a load-more eviction never abandons the reader at the top', () => {
  test('restores position when the anchor row is itself evicted', async () => {
    // The eviction cuts the newest 400 rows; the captured anchor is one of them. During a
    // fast scroll the capture runs after the network await against a lagging rendered range,
    // so the id it returns can be a row the merge is about to drop.
    const el = await seeded(MAX_RETAINED_ROWS);
    const sim = scrollHarness(el);
    sim.scrollToBottom();
    vi.spyOn(el as any, 'captureScrollAnchor').mockReturnValue({ id: 'r00001', offset: 0 });
    el.transport = serverTransport(olderPage(0, 400));

    await el.fetchData('older', false, false, true);
    await flushFrames();

    expect(sim.atTop).toBe(false);
  });

  test('restores position when no anchor could be captured at all', async () => {
    const el = await seeded(MAX_RETAINED_ROWS);
    const sim = scrollHarness(el);
    sim.scrollToBottom();
    sim.emitVisibility(); // the rendered range is the only record of where they were
    vi.spyOn(el as any, 'captureScrollAnchor').mockReturnValue(null);
    el.transport = serverTransport(olderPage(0, 400));

    await el.fetchData('older', false, false, true);
    await flushFrames();

    expect(sim.atTop).toBe(false);
  });
});
