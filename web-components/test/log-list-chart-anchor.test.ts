import { describe, test, expect, beforeEach, vi } from 'vitest';
import { render } from 'lit';
import { LogList } from '../src/log-list';
import { mountList, serverTransport, logPage } from './log-list-harness';

// Render the bottom-of-list affordance template to a detached node and read its
// text. The component virtualizes its rows off the scroll container's height,
// which is 0 in jsdom, so the live DOM stays empty — but renderLoadMoreButton()
// holds the actual "which button" decision (Load more vs Show earlier events),
// so rendering it directly exercises that branch deterministically.
const affordanceText = (el: LogList): string => {
  const c = document.createElement('div');
  render((el as any).renderLoadMoreButton(), c);
  return c.textContent ?? '';
};

// Guards the user-visible symptom behind the "Show earlier events" bug:
//   1. A full page must offer "Load more" (in-window cursor paging), NOT
//      "Show earlier events" (which WIDENS the window). The backend bug flipped
//      hasMore false on any page carrying a synth orphan header, surfacing the
//      widen button after one page → the chart appeared to zoom out.
//   2. "Show earlier events" only appears once the window is genuinely exhausted.
//   3. Load-more preserves since/from/to AND never rewrites the browser URL — the
//      chart reads window.location.search, so paging must leave it untouched.
//   4. When the widen button IS used, it expands `since` and syncs the picker
//      label (the URL-says-3H-but-dropdown-says-1H bug).
describe('LogList pagination affordance + chart anchor', () => {
  let el: LogList;
  beforeEach(async () => {
    el = await mountList();
  });

  test('a full page offers "Load more", not "Show earlier events"', async () => {
    el.transport = serverTransport(logPage(['1', '2', '3'], { hasMore: true, count: 50 }));
    await el.fetchData('initial', false, false, false);
    const text = affordanceText(el);
    expect(text).toContain('Load more');
    expect(text).not.toContain('Show earlier events');
  });

  test('"Show earlier events" appears only when the window is exhausted (hasMore=false)', async () => {
    el.transport = serverTransport(logPage(['1', '2', '3'], { hasMore: false, count: 3 }));
    await el.fetchData('initial', false, false, false);
    expect(affordanceText(el)).toContain('Show earlier events');
  });

  test('load-more preserves since/from/to and leaves the browser URL (chart anchor) untouched', async () => {
    el.transport = serverTransport(
      logPage(['1', '2', '3'], { hasMore: true, count: 50, nextUrl: '/p/x/log_explorer?since=1H&json=true' }),
    );
    await el.fetchData('initial', false, false, false);

    const before = window.location.href;
    const loadMoreUrl = (el as any).buildLoadMoreUrl();
    const params = new URL(loadMoreUrl, window.location.origin).searchParams;

    expect(params.get('since')).toBe('1H'); // window unchanged …
    expect(params.get('cursor')).toBeTruthy(); // … only a cursor is added
    expect(loadMoreUrl).not.toMatch(/since=(3H|6H|12H|24H)/); // never widened
    expect(window.location.href).toBe(before); // chart anchor (window.location) not rewritten
  });

  test('"Show earlier events" widens since AND syncs the picker label', async () => {
    const updateTimePicker = vi.fn(() => '3H');
    (window as any).updateTimePicker = updateTimePicker;

    el.transport = serverTransport(logPage(['1', '2', '3'], { hasMore: false, count: 3, nextUrl: '/p/x/log_explorer?since=1H&json=true' }));
    await el.fetchData('initial', false, false, false);

    const expandUrl = (el as any).expandTimeRangeUrl();
    expect(new URL(expandUrl, window.location.origin).searchParams.get('since')).toBe('3H'); // 1H → 3H
    expect(updateTimePicker).toHaveBeenCalledWith({ since: '3H' }, { skipSetParams: true }); // dropdown synced
  });
});
