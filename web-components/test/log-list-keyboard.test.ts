// Keyboard navigation of the log grid.
//
// The table is an ARIA grid driven by `aria-activedescendant`: focus never leaves the
// table, and a roving marker moves instead. That marker has to skip the sentinel rows
// (which are not events), clamp at both ends, and survive the list changing underneath
// it. None of it was tested, and it is the whole keyboard/screen-reader path.
import { describe, test, expect, vi, beforeEach } from 'vitest';
import { row, mountList, stubVirtualizer, stubQuery } from './log-list-harness';

const seeded = async (n = 5, props: Record<string, any> = {}) => {
  const el = await mountList(props as any);
  const rows = Array.from({ length: n }, (_, i) => row(`r${i}`));
  (el as any).spanListTree = rows;
  (el as any).seenIds = new Set(rows.map((r) => r.id));
  (el as any).updateVisibleItems();
  await el.updateComplete;
  return el;
};

const press = (el: any, key: string, target?: Element) => {
  const event = new KeyboardEvent('keydown', { key, bubbles: true, cancelable: true });
  Object.defineProperty(event, 'target', { value: target ?? document.createElement('td') });
  el.handleGridKeydown(event);
  return event;
};

const focused = (el: any) => el.focusedRowId;

beforeEach(() => vi.restoreAllMocks());

describe('roving focus', () => {
  test('the first Arrow Down lands on the first row, not the sentinel above it', async () => {
    const el = await seeded();
    // Newest-first puts a "load newer" sentinel at index 0; the marker must skip it.
    expect(((el as any).virtualListItems[0] as any).type).toBe('fetchRecent');

    press(el, 'ArrowDown');
    await el.updateComplete;

    expect(focused(el)).toBe('r0');
  });

  test('arrow keys walk one row at a time in both directions', async () => {
    const el = await seeded();
    press(el, 'ArrowDown');
    await el.updateComplete;
    press(el, 'ArrowDown');
    await el.updateComplete;
    expect(focused(el)).toBe('r1');

    press(el, 'ArrowUp');
    await el.updateComplete;
    expect(focused(el)).toBe('r0');
  });

  test('paging moves ten rows and clamps rather than running off the end', async () => {
    const el = await seeded(30);
    press(el, 'ArrowDown');
    await el.updateComplete;
    press(el, 'PageDown');
    await el.updateComplete;
    expect(focused(el)).toBe('r10');

    press(el, 'PageUp');
    await el.updateComplete;
    expect(focused(el)).toBe('r0'); // clamped at the top, not negative
  });

  test('Home and End jump to the real first and last events', async () => {
    const el = await seeded(30);

    press(el, 'End');
    await el.updateComplete;
    expect(focused(el)).toBe('r29');

    press(el, 'Home');
    await el.updateComplete;
    expect(focused(el)).toBe('r0');
  });

  test('the marker never lands on a sentinel row at either end', async () => {
    const el = await seeded(3);
    const sentinelIds = new Set(
      ((el as any).virtualListItems as any[]).filter((i) => !('id' in i)).map((i) => i.type)
    );
    expect(sentinelIds.size).toBeGreaterThan(0);

    for (const key of ['Home', 'End', 'ArrowDown', 'ArrowUp', 'PageDown', 'PageUp']) {
      press(el, key);
      await el.updateComplete;
      expect((el as any).spanListTree.map((r: any) => r.id)).toContain(focused(el));
    }
  });

  test('navigation keys are consumed so the page does not scroll as well', async () => {
    const el = await seeded();
    for (const key of ['ArrowDown', 'ArrowUp', 'PageDown', 'PageUp', 'Home', 'End']) {
      expect(press(el, key).defaultPrevented).toBe(true);
    }
  });

  test('an empty list has nothing to focus and does not throw', async () => {
    const el = await mountList();
    expect(() => press(el, 'ArrowDown')).not.toThrow();
    expect(focused(el)).toBeNull();
  });
});

describe('activation', () => {
  test('Enter and Space open the focused row', async () => {
    const el = await seeded();
    press(el, 'ArrowDown');
    await el.updateComplete;
    const click = vi.fn();
    stubQuery(el, { '[data-row-id="r0"]': { click } });

    for (const key of ['Enter', ' ']) {
      expect(press(el, key).defaultPrevented).toBe(true);
    }
    await new Promise((r) => requestAnimationFrame(r));

    expect(click).toHaveBeenCalled();
  });

  test('activation with nothing focused is a no-op, not an error', async () => {
    const el = await seeded();
    expect(press(el, 'Enter').defaultPrevented).toBe(false);
  });
});

describe('controls inside a row keep their own keys', () => {
  // A button's Enter must not also open the row behind it, and typing in a header
  // filter must not page the list out from under the cursor.
  test.each(['button', 'a', 'input', 'textarea', 'select'])('%s owns its keystrokes', async (tag) => {
    const el = await seeded();
    const control = document.createElement(tag);

    const down = press(el, 'ArrowDown', control);
    const enter = press(el, 'Enter', control);

    expect(down.defaultPrevented).toBe(false);
    expect(enter.defaultPrevented).toBe(false);
    expect(focused(el)).toBeNull();
  });

  test('an editable cell owns its keystrokes too', async () => {
    const el = await seeded();
    const editable = document.createElement('div');
    editable.setAttribute('contenteditable', 'true');

    expect(press(el, 'ArrowDown', editable).defaultPrevented).toBe(false);
    expect(focused(el)).toBeNull();
  });
});

describe('bringing the focused row into view', () => {
  test('Home and End pin the list to that edge rather than scrolling the minimum', async () => {
    const el = await seeded(200);
    const scrollToIndex = vi.fn();
    stubQuery(el, { 'lit-virtualizer': stubVirtualizer({ scrollToIndex }) });

    press(el, 'End');
    await el.updateComplete;
    expect(scrollToIndex).toHaveBeenLastCalledWith(expect.any(Number), 'end');

    press(el, 'Home');
    await el.updateComplete;
    expect(scrollToIndex).toHaveBeenLastCalledWith(expect.any(Number), 'start');
  });

  test('stepping through rows only scrolls as far as it needs to', async () => {
    const el = await seeded(200);
    const scrollToIndex = vi.fn();
    stubQuery(el, { 'lit-virtualizer': stubVirtualizer({ scrollToIndex }) });

    press(el, 'ArrowDown');
    await el.updateComplete;

    expect(scrollToIndex).toHaveBeenLastCalledWith(expect.any(Number), 'nearest');
  });
});
