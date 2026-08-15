// Once the retention window starts evicting, lit-virtualizer can settle into a state where it
// renders no rows at all while `items` is non-empty — and it never recovers on its own, because
// rendering nothing means nothing changes, so no further update or visibilityChanged event ever
// arrives to notice. Users saw the log list go blank and stay blank until they reloaded.
import { describe, expect, test, vi, beforeEach } from 'vitest';
import { mountList } from './log-list-harness';
import { row } from './log-list-harness';

const stubVirtualizer = (el: any, renderedRows: number, visible = true) => {
  const rows = Array.from({ length: renderedRows }, () => ({
    getBoundingClientRect: () => (visible ? { top: 10, bottom: 38 } : { top: -100, bottom: -72 }),
  }));
  const virtualizer = {
    querySelectorAll: () => rows,
    scrollToIndex: vi.fn(),
    layoutComplete: Promise.resolve(),
  };
  vi.spyOn(el, 'querySelector').mockReturnValue(virtualizer as any);
  return virtualizer;
};

const container = () => ({ scrollTop: 5000, getBoundingClientRect: () => ({ top: 0, bottom: 500 }) });

describe('blank virtualizer recovery', () => {
  beforeEach(() => vi.restoreAllMocks());

  test('resets the scroll origin and returns to the anchored row when nothing rendered', async () => {
    const el = await mountList();
    const c = container();
    Object.defineProperty(el, 'logsContainer', { value: c, configurable: true });
    (el as any).virtualListItems = Array.from({ length: 900 }, (_, i) => row(`r${i}`));
    (el as any).lastVisibilityRange = { first: 400, last: 418 };
    const v = stubVirtualizer(el, 0);

    (el as any).healBlankVirtualizer();

    expect(c.scrollTop).toBe(0); // forces the layout to re-measure from a known-good origin
    await Promise.resolve();
    await Promise.resolve();
    expect(v.scrollToIndex).toHaveBeenCalledWith(400, 'start');
  });

  test('does nothing while rows are rendering', async () => {
    const el = await mountList();
    const c = container();
    Object.defineProperty(el, 'logsContainer', { value: c, configurable: true });
    (el as any).virtualListItems = Array.from({ length: 900 }, (_, i) => row(`r${i}`));
    const v = stubVirtualizer(el, 33);

    (el as any).healBlankVirtualizer();

    expect(c.scrollTop).toBe(5000);
    expect(v.scrollToIndex).not.toHaveBeenCalled();
  });

  test('recovers when rows exist but every row is positioned outside the viewport', async () => {
    const el = await mountList();
    const c = container();
    Object.defineProperty(el, 'logsContainer', { value: c, configurable: true });
    (el as any).virtualListItems = Array.from({ length: 900 }, (_, i) => row(`r${i}`));
    (el as any).lastVisibilityRange = { first: 400, last: 418 };
    const v = stubVirtualizer(el, 20, false);

    (el as any).healBlankVirtualizer();

    expect(c.scrollTop).toBe(0);
    await Promise.resolve();
    await Promise.resolve();
    expect(v.scrollToIndex).toHaveBeenCalledWith(400, 'start');
  });

  test('the retry budget is per eviction, not per component lifetime', async () => {
    // The empty frames that occur *during* an eviction would otherwise spend the whole budget
    // before the user ever sees the stuck state, leaving the real blank unrecoverable.
    const el = await mountList();
    Object.defineProperty(el, 'logsContainer', { value: container(), configurable: true });
    (el as any).virtualListItems = Array.from({ length: 900 }, (_, i) => row(`r${i}`));
    (el as any).lastVisibilityRange = { first: 400, last: 418 };
    stubVirtualizer(el, 0);

    const attempts = () => (el as any).blankHealAttempts;
    for (let i = 0; i < 5; i++) {
      (el as any).blankHealAt = 0; // bypass the rate limit, not the budget
      (el as any).healBlankVirtualizer();
    }
    expect(attempts()).toBe(3); // capped, so a persistent failure cannot fight the user's scroll

    (el as any).virtualizerEpoch++; // next eviction
    (el as any).blankHealAt = 0;
    (el as any).healBlankVirtualizer();
    expect(attempts()).toBe(1); // budget refreshed
  });

  test('the watchdog is cleared when the component goes away', async () => {
    const el = await mountList();
    expect((el as any).blankWatchdog).not.toBeNull();
    el.remove();
    expect((el as any).blankWatchdog).toBeNull();
  });
});
