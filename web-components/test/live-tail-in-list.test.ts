// Live tail as the reader experiences it: rows arriving continuously into a list they
// are scrolled somewhere inside of.
//
// live-stream.test.ts covers the SSE connection (leases, backoff, drops). What was never
// covered is the half that decides whether a reader gets interrupted: buffer vs insert,
// where the viewport ends up, and what happens when the query changes mid-stream. Those
// need real geometry, so they run against the scroll simulator.
import { describe, test, expect } from 'vitest';
import { row, serverTransport, serverTransportFlipped, logPage, mountList, scrollHarness, flushFrames, ids, ROW_H } from './log-list-harness';

const DIRECTIONS = [
  { name: 'newest-first (rows arrive at the top)', flip: false },
  { name: 'oldest-first (rows arrive at the bottom)', flip: true },
];

const seeded = async (flip: boolean, n = 400) => {
  const el = await mountList({ flipDirection: flip, isLiveStreaming: true } as any);
  const t = flip ? serverTransportFlipped : serverTransport;
  el.transport = t(logPage(Array.from({ length: n }, (_, i) => `r${String(i).padStart(5, '0')}`)));
  await el.fetchData('first', true);
  await flushFrames();
  return el;
};

// A batch pushed down the live connection, in the wire shape handleLiveRows parses.
const pushed = (id: string, timestamp = new Date().toISOString()) => ({
  shape: 'table',
  cols: { id, latency_breakdown: id, trace_id: id, parent_id: '', kind: 'log', timestamp },
});

describe.each(DIRECTIONS)('live tail: $name', ({ flip }) => {
  // Where new rows enter, and therefore where the reader must be parked for them to
  // be inserted rather than buffered.
  const parkAtEdge = (sim: any) => (flip ? sim.scrollToBottom() : sim.scrollTo(0));
  const parkAway = (sim: any) => sim.scrollTo(150 * ROW_H);

  test('parked at the arrival edge, rows insert straight away', async () => {
    const el = await seeded(flip);
    const sim = scrollHarness(el);
    parkAtEdge(sim);

    (el as any).handleLiveRows([pushed('live-1'), pushed('live-2')]);
    await flushFrames();

    expect(ids(el)).toContain('live-1');
    expect((el as any).recentDataToBeAdded).toHaveLength(0);
    expect(el.recentCount).toBe(0);
  });

  test('scrolled away, rows buffer behind the pill and the viewport does not move', async () => {
    const el = await seeded(flip);
    const sim = scrollHarness(el);
    parkAway(sim);
    const before = sim.scrollTop;

    (el as any).handleLiveRows([pushed('live-1'), pushed('live-2')]);
    await flushFrames();

    expect(ids(el)).not.toContain('live-1');
    expect(el.recentCount).toBe(2);
    expect(sim.scrollTop).toBe(before);
  });

  test('scrolling back to the edge flushes the buffer', async () => {
    const el = await seeded(flip);
    const sim = scrollHarness(el);
    parkAway(sim);
    (el as any).handleLiveRows([pushed('live-1')]);
    await flushFrames();

    parkAtEdge(sim);
    await flushFrames();

    expect(ids(el)).toContain('live-1');
    expect(el.recentCount).toBe(0);
  });

  test('the pill counts exactly the rows it will insert', async () => {
    const el = await seeded(flip);
    const sim = scrollHarness(el);
    parkAway(sim);

    (el as any).handleLiveRows([pushed('a'), pushed('b'), pushed('c')]);
    await flushFrames();
    const promised = el.recentCount;
    const lengthBefore = ids(el).length;
    (el as any).handleRecentConcatenation();

    expect(ids(el).length - lengthBefore).toBe(promised);
  });

  test('a row already on screen is never buffered a second time', async () => {
    const el = await seeded(flip);
    const sim = scrollHarness(el);
    parkAway(sim);
    const existing = ids(el)[0];

    (el as any).handleLiveRows([pushed(existing), pushed('genuinely-new')]);
    await flushFrames();

    expect(el.recentCount).toBe(1);
  });

  test('pausing the stream keeps the buffer rather than discarding it', async () => {
    const el = await seeded(flip);
    const sim = scrollHarness(el);
    parkAway(sim);
    (el as any).handleLiveRows([pushed('live-1')]);
    await flushFrames();

    (el as any).isLiveStreaming = false;
    sim.scrollTo(sim.scrollTop); // a scroll while paused must not silently flush

    expect(el.recentCount).toBe(1);
    expect(ids(el)).not.toContain('live-1');
  });

  test('a page of history loaded while buffering leaves the buffer intact', async () => {
    const el = await seeded(flip);
    const sim = scrollHarness(el);
    parkAway(sim);
    (el as any).handleLiveRows([pushed('live-1')]);
    await flushFrames();

    const t = flip ? serverTransportFlipped : serverTransport;
    el.transport = t(logPage(['o1', 'o2']));
    await el.fetchData('older', false, false, true);
    await flushFrames();

    expect(el.recentCount).toBe(1);
    expect(ids(el)).toContain('o1');
  });

  test('many history pages followed by a live batch keep the row being read', async () => {
    const el = await seeded(flip);
    const sim = scrollHarness(el);
    parkAway(sim);

    for (let page = 0; page < 6; page++) {
      const t = flip ? serverTransportFlipped : serverTransport;
      el.transport = t(logPage(Array.from({ length: 500 }, (_, i) => `old-${page}-${i}`)));
      await el.fetchData(`older-${page}`, false, false, true);
      await flushFrames();
    }

    parkAway(sim);
    const anchor = (el as any).virtualListItems[sim.firstVisibleIndex]?.id;
    const before = sim.scrollTop;
    const retained = ids(el);
    (el as any).handleLiveRows(Array.from({ length: 200 }, (_, i) => pushed(`live-${i}`)));
    await flushFrames();

    expect(sim.scrollTop).toBe(before);
    expect(ids(el)).toEqual(retained);
    expect((el as any).virtualListItems[sim.firstVisibleIndex]?.id).toBe(anchor);
    expect(el.recentCount).toBe(200);
  });

  test('a delayed live row is inserted at its timestamp, not at the latest edge', async () => {
    const el = await mountList({ flipDirection: flip, isLiveStreaming: true } as any);
    const t = flip ? serverTransportFlipped : serverTransport;
    el.transport = t(
      logPage([
        ['newest', '2026-06-01T00:00:03.000Z'],
        ['oldest', '2026-06-01T00:00:01.000Z'],
      ]),
    );
    await el.fetchData('first', true);
    const sim = scrollHarness(el);
    parkAtEdge(sim);

    (el as any).handleLiveRows([pushed('middle', '2026-06-01T00:00:02.000Z')]);
    await flushFrames();

    expect(ids(el)).toEqual(flip ? ['oldest', 'middle', 'newest'] : ['newest', 'middle', 'oldest']);
  });

  test('changing the query drops rows buffered from the previous one', async () => {
    const el = await seeded(flip);
    const sim = scrollHarness(el);
    parkAway(sim);
    (el as any).handleLiveRows([pushed('from-old-query')]);
    await flushFrames();
    expect(el.recentCount).toBe(1);

    const t = flip ? serverTransportFlipped : serverTransport;
    el.transport = t(logPage(['new1', 'new2']));
    await el.fetchData('new-query', true);
    await flushFrames();

    // The pill promised rows matching the *previous* filter. Carrying them over
    // offers to insert results the current query excludes.
    expect(el.recentCount).toBe(0);
    expect((el as any).recentDataToBeAdded).toHaveLength(0);
    (el as any).handleRecentConcatenation();
    expect(ids(el)).not.toContain('from-old-query');
  });

  test('an empty replacement query also drops the previous live buffer', async () => {
    const el = await seeded(flip);
    const sim = scrollHarness(el);
    parkAway(sim);
    (el as any).handleLiveRows([pushed('from-old-query')]);
    expect(el.recentCount).toBe(1);

    const t = flip ? serverTransportFlipped : serverTransport;
    el.transport = t(logPage([]));
    await el.fetchData('empty-new-query', true);

    expect(el.recentCount).toBe(0);
    expect((el as any).recentDataToBeAdded).toHaveLength(0);
  });

  test('the dropped-rows warning resets with the query it described', async () => {
    const el = await seeded(flip);
    (el as any).liveDropped = 4200;

    const t = flip ? serverTransportFlipped : serverTransport;
    el.transport = t(logPage(['new1']));
    await el.fetchData('new-query', true);

    expect((el as any).liveDropped).toBe(0);
  });
});
