import { describe, test, expect, beforeEach } from 'vitest';
import { render } from 'lit';
import { dedupeById } from '../src/log-list-utils';
import { LogList, exclusiveSegments, latencyBar, latencySegments, latencyTitle, latencyTooltip, spanLatencyBreakdown } from '../src/log-list';
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

// Regression: entering the loading state used to swap the label OUT and the spinner
// IN as two different subtrees, so the row went briefly empty (and changed size,
// reflowing the list) between click and spinner. Both nodes must stay mounted and
// only toggle `invisible`.
describe('LogList load row loading state', () => {
  test('label and spinner both stay mounted; only visibility flips', async () => {
    const el = await mountList();
    el.transport = fakeTransport({ tree: [row('1')] });
    await el.fetchData('initial', false, false, false);

    const tbody = document.createElement('tbody');
    document.body.appendChild(document.createElement('table')).appendChild(tbody);
    render(el.renderLoadMoreButton(), tbody);
    const label = tbody.querySelector('span')!;
    const spinner = tbody.querySelector('div.loading')!;
    expect(label.textContent).toContain('Load more');
    expect(label.className).not.toContain('invisible');
    expect(spinner.className).toContain('invisible');

    (el as any).isLoadingMore = true;
    render(el.renderLoadMoreButton(), tbody);
    expect(label.isConnected).toBe(true); // same nodes, not re-created
    expect(spinner.isConnected).toBe(true);
    expect(label.className).toContain('invisible');
    expect(spinner.className).not.toContain('invisible');
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

describe('latencySegments', () => {
  // The row is the axis, not the trace. On the trace-wide axis a 3ms child inside a 2s trace
  // was a sub-pixel sliver, which is why child rows were unreadable.
  const row = { startNs: 1_000, duration: 1_000 };
  const child = (startNs: number, duration: number, label = 'db') => ({ startNs, duration, label, color: `bg-${label}` });

  test('places children as a percentage of the row, not of the trace', () => {
    expect(latencySegments(row, [child(1_250, 500)])).toEqual([
      { leftPct: 25, widthPct: 50, color: 'bg-db', label: 'db', ns: 500 },
    ]);
  });

  test('gaps between children are the row self time, so segments never sum past the row', () => {
    const segs = latencySegments(row, [child(1_000, 200, 'db'), child(1_600, 300, 'cache')]);
    const accounted = segs.reduce((a, s) => a + s.ns, 0);
    expect(accounted).toBe(500);
    expect(row.duration - accounted).toBe(500); // self
    expect(segs.every((s) => s.leftPct + s.widthPct <= 100)).toBe(true);
  });

  test('intersects with the row window rather than clamping an offset', () => {
    // Clock skew and a child outliving its parent are both real. A child that ended before
    // its parent began contributes nothing — pinning it to the start of the bar would claim
    // time the parent never spent — while a child that overruns is still time it waited on.
    const segs = latencySegments(row, [child(500, 300), child(1_900, 5_000, 'slow')]);
    expect(segs).toEqual([{ leftPct: 90, widthPct: 10, color: 'bg-slow', label: 'slow', ns: 100 }]);
  });

  test('a child overlapping the row start is measured from the row start', () => {
    expect(latencySegments(row, [child(900, 300)])).toEqual([
      { leftPct: 0, widthPct: 20, color: 'bg-db', label: 'db', ns: 200 },
    ]);
  });

  test('drops children with no measurable width, and a zero-duration row has no bar to draw', () => {
    expect(latencySegments(row, [child(1_400, 0), child(1_400, -5)])).toEqual([]);
    expect(latencySegments({ startNs: 0, duration: 0 }, [child(0, 10)])).toEqual([]);
  });
});

describe('exclusiveSegments', () => {
  const row = { startNs: 1_000, duration: 1_000 };
  const span = (startNs: number, duration: number, depth: number, label: string) => ({ startNs, duration, depth, label, color: `bg-${label}` });

  test('the deepest span covering an instant owns it, so a grandchild is not hidden under its parent', () => {
    // The bug: `api → svc → db` where api and svc are the same service painted one flat
    // colour, and the db time the row was actually waiting on was invisible.
    const segs = exclusiveSegments(row, [span(1_100, 800, 1, 'svc'), span(1_200, 500, 2, 'db')]);
    expect(segs).toEqual([
      { leftPct: 10, widthPct: 10, color: 'bg-svc', label: 'svc', ns: 100 },
      { leftPct: 20, widthPct: 50, color: 'bg-db', label: 'db', ns: 500 },
      { leftPct: 70, widthPct: 20, color: 'bg-svc', label: 'svc', ns: 200 },
    ]);
  });

  test('segments are disjoint and never bill the same time twice, so the parts sum to the whole', () => {
    const segs = exclusiveSegments(row, [span(1_000, 1_000, 1, 'svc'), span(1_100, 200, 2, 'db'), span(1_500, 300, 2, 'db')]);
    expect(segs.reduce((a, s) => a + s.ns, 0)).toBe(1_000); // the child fills the row: no self time
    expect(segs.every((s, i) => i === 0 || s.leftPct >= segs[i - 1].leftPct + segs[i - 1].widthPct)).toBe(true);
    // The tooltip sums per label off these, so exclusivity is what makes "db 500ns" true.
    expect(segs.filter(s => s.label === 'db').reduce((a, s) => a + s.ns, 0)).toBe(500);
  });

  test('adjacent runs of the same service merge into one segment', () => {
    expect(exclusiveSegments(row, [span(1_000, 200, 1, 'db'), span(1_200, 300, 1, 'db')])).toEqual([
      { leftPct: 0, widthPct: 50, color: 'bg-db', label: 'db', ns: 500 },
    ]);
  });

  test('clips to the row window and drops what falls outside it', () => {
    expect(exclusiveSegments(row, [span(500, 300, 1, 'early'), span(1_900, 5_000, 1, 'slow')])).toEqual([
      { leftPct: 90, widthPct: 10, color: 'bg-slow', label: 'slow', ns: 100 },
    ]);
    expect(exclusiveSegments({ startNs: 0, duration: 0 }, [span(0, 10, 1, 'db')])).toEqual([]);
  });
});

describe('latencyBar', () => {
  // Expanding a trace is what makes the column a waterfall again: the child rows only mean
  // something as a breakdown of the one request, which needs a shared axis.
  const row = { startNs: 1_500, duration: 500, traceStart: 1_000, traceEnd: 2_000, label: 'api', color: 'bg-api' };
  const child = (startNs: number, duration: number, label = 'db') => ({ startNs, duration, label, color: `bg-${label}` });

  test('expanded rows lay their own span on the trace axis, framed by the trace bounds', () => {
    const { track, segments, frame } = latencyBar(true, row, [child(1_600, 200)]);
    expect(track).toBe('bg-fillWeak'); // the track is the trace, not the row
    expect(frame).toBe(true); // |---[]---| : without it a short span is just a small block
    expect(segments).toEqual([
      { leftPct: 25, widthPct: 25, color: 'bg-api', label: 'api', ns: 500 },
      { leftPct: 30, widthPct: 10, color: 'bg-db', label: 'db', ns: 200 },
    ]);
  });

  test('a collapsed row is its own axis: full-width track, children inside it', () => {
    const { track, segments, frame } = latencyBar(false, row, [child(1_600, 200)]);
    expect(track).toBe('bg-api');
    // No frame: the bar already fills the range, so the rule would restate its own bounds.
    expect(frame).toBe(false);
    expect(segments).toEqual([{ leftPct: 20, widthPct: 40, color: 'bg-db', label: 'db', ns: 200 }]);
  });

  test('falls back to the row axis when the trace has no measurable span', () => {
    expect(latencyBar(true, { ...row, traceEnd: 0 }, []).track).toBe('bg-api');
  });

  test('a log has no duration but still gets a mark at its place in the trace', () => {
    // Dropping it — which the accounting pass does, correctly, since it costs no time — is
    // how a row reads as "ignores its timing and sits at the beginning".
    const [own] = latencyBar(true, { ...row, duration: 0 }, []).segments;
    expect(own).toEqual({ leftPct: 25, widthPct: 0, color: 'bg-api', label: 'api', ns: 0 });
  });

  test('a row past the axis end pins to it rather than escaping the bar', () => {
    // The axis is sized by spans, so a log seconds after the last span lands outside it.
    const [own] = latencyBar(true, { ...row, startNs: 9_000, duration: 0 }, []).segments;
    expect(own.leftPct).toBe(100);
  });
});

describe('latencyTooltip', () => {
  // The card replaces a native `title=`, which could only say the numbers. Its job is to tie
  // each number to the swatch colour the bar is painted in.
  const seg = (label: string, ns: number, leftPct: number) => ({ leftPct, widthPct: ns / 100, color: `bg-${label}`, label, ns });
  const row = { duration: 1_000, startNs: 1_500, traceStart: 1_000, color: 'bg-api' };
  const card = (segments: any[], r = row) => {
    const host = document.createElement('div');
    render(latencyTooltip(r, segments), host);
    return host;
  };
  const lines = (host: HTMLElement) =>
    [...host.querySelectorAll('.latency-card-body > div')].slice(1).map((d) => d.textContent!.replace(/\s+/g, ' ').trim());

  test('each service carries its own swatch, so the card explains the colours in the bar', () => {
    const host = card([seg('postgres', 500, 0), seg('redis', 100, 60)]);
    expect([...host.querySelectorAll('.latency-card-body span[class*="rounded-xs"]')].map((s) => s.className)).toEqual(
      expect.arrayContaining([expect.stringContaining('bg-postgres'), expect.stringContaining('bg-redis'), expect.stringContaining('bg-api')])
    );
  });

  test('rows are sorted by time and include self, so the parts visibly sum to the whole', () => {
    // 500 postgres + 100 redis + 400 self = the row's 1000. Omitting self would make the
    // listed parts look like they should add up when they don't.
    expect(lines(card([seg('postgres', 500, 0), seg('redis', 100, 60)]))).toEqual(['postgres 500ns 50%', 'self 400ns 40%', 'redis 100ns 10%']);
  });

  test('the same service in two places is one row, not two', () => {
    expect(lines(card([seg('db', 200, 0), seg('db', 300, 50)]))).toEqual(['db 500ns 50%', 'self 500ns 50%']);
  });

  test('caps the list and says how much it left out rather than silently truncating', () => {
    const many = ['a', 'b', 'c', 'd', 'e', 'f', 'g'].map((l, i) => seg(l, 100, i * 10));
    const host = card(many);
    expect(lines(host)).toHaveLength(7); // 6 services + the "+n more" line
    expect(host.textContent).toContain('+2 more'); // 7 services + self = 8 rows, 6 shown
  });

  test('a row that spent all its time itself still reads as a breakdown', () => {
    expect(lines(card([]))).toEqual(['self 1µs 100%']);
  });

});

describe('spanLatencyBreakdown wrapper', () => {
  // The card is a top-layer popover because the row's `contain: paint` clips everything else,
  // and it opens with no script at all. Both halves of that are attribute contracts: lose
  // `interestfor`/`popover` and it never opens; lose `anchor-name`/`position-anchor` and it
  // opens somewhere unrelated to the bar it describes.
  const wrap = (card: { id: string; body: any } | null) => {
    const host = document.createElement('div');
    render(spanLatencyBreakdown({ track: 'bg-api', segments: [], title: '1.2s total', card, barWidth: 108, frame: false }), host);
    return host;
  };

  test('a row with a breakdown gets a button wired to its own popover, anchored to its own bar', () => {
    const host = wrap({ id: 'lat-abc', body: latencyTooltip({ duration: 1_000, startNs: 0, traceStart: 0, color: 'bg-api' }, []) });
    const btn = host.querySelector('button')!;
    const pop = host.querySelector('[popover]')!;
    expect(btn.getAttribute('interestfor')).toBe('lat-abc');
    expect(pop.id).toBe('lat-abc'); // interestfor resolves by id — a mismatch silently never opens
    expect(btn.getAttribute('style')).toContain('anchor-name:--lat-abc');
    expect(pop.getAttribute('style')).toContain('position-anchor:--lat-abc');
    expect(pop.getAttribute('popover')).toBe('hint');
    expect(pop.getAttribute('aria-hidden')).toBe('true'); // the button's aria-label says it instead
  });

  test('a row with nothing to break down is not a control: no popover, no tab stop', () => {
    const host = wrap(null);
    expect(host.querySelector('button')).toBeNull();
    expect(host.querySelector('[popover]')).toBeNull();
    expect(host.querySelector('[role="img"]')!.getAttribute('aria-label')).toBe('1.2s total');
  });
});

describe('latencyTitle', () => {
  test('says in words what the colours say, including the trace offset the bar no longer encodes', () => {
    const title = latencyTitle(
      'service',
      { startNs: 1_500_000, duration: 2_000_000, traceStart: 500_000 },
      [
        { leftPct: 0, widthPct: 25, color: 'x', label: 'postgres', ns: 500_000 },
        { leftPct: 30, widthPct: 10, color: 'y', label: 'redis', ns: 200_000 },
      ]
    );
    expect(title).toContain('2ms total');
    expect(title).toContain('+1ms into the trace');
    expect(title).toContain('self 1ms');
    // Biggest contributor first — that is the one worth reading.
    expect(title).toContain('by service: postgres 500µs, redis 200µs');
  });

  test('a leaf span is all self time and names no dimension', () => {
    const title = latencyTitle('kind', { startNs: 0, duration: 3_000_000, traceStart: 0 }, []);
    expect(title).toBe('3ms total · +0ns into the trace · self 3ms');
  });
});
