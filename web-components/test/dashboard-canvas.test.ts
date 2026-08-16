// The dashboard canvas → backend contract.
//
// `buildWidgetOrder` walks the GridStack DOM and produces the patch body sent to
// PATCH /dashboards/:id/widgets. That handler rebuilds the widget list *purely* from the
// patch — anything missing is deleted. So a serializer that skips a widget, or throws
// halfway through, is not a cosmetic bug: it drops widgets off the dashboard. Nothing
// tested it.
import { describe, test, expect, beforeEach } from 'vitest';
import '../src/widgets';

const buildWidgetOrder = (el: HTMLElement) => (window as any).buildWidgetOrder(el);
const getActiveGrid = () => (window as any).getActiveGrid();

// A GridStack item as it exists in the live DOM: the element plus the `gridstackNode`
// GridStack attaches to it once it has adopted the element.
const item = (id: string, node: { x: number; y: number; w: number; h: number } | null) => {
  const el = document.createElement('div');
  el.className = 'grid-stack-item';
  el.id = `${id}_widgetEl`;
  if (node) (el as any).gridstackNode = node;
  return el;
};

const grid = (...children: HTMLElement[]) => {
  const g = document.createElement('div');
  g.className = 'grid-stack';
  children.forEach((c) => g.appendChild(c));
  document.body.appendChild(g);
  return g;
};

beforeEach(() => {
  document.body.innerHTML = '';
});

describe('buildWidgetOrder', () => {
  test('serializes each widget id with the position and size GridStack holds', () => {
    const g = grid(item('alpha', { x: 0, y: 0, w: 6, h: 3 }), item('beta', { x: 6, y: 0, w: 6, h: 4 }));

    expect(buildWidgetOrder(g)).toEqual({
      alpha: { x: 0, y: 0, w: 6, h: 3 },
      beta: { x: 6, y: 0, w: 6, h: 4 },
    });
  });

  test('keeps a widget parked at the origin rather than dropping its zeroes', () => {
    const g = grid(item('origin', { x: 0, y: 0, w: 1, h: 1 }));
    expect(buildWidgetOrder(g).origin).toEqual({ x: 0, y: 0, w: 1, h: 1 });
  });

  test('only direct children count, so a nested grid is not flattened into its parent', () => {
    const group = item('group', { x: 0, y: 0, w: 12, h: 6 });
    const nested = document.createElement('div');
    nested.className = 'nested-grid';
    nested.appendChild(item('child', { x: 0, y: 0, w: 3, h: 2 }));
    group.appendChild(nested);
    const g = grid(group, item('sibling', { x: 0, y: 6, w: 6, h: 3 }));

    const order = buildWidgetOrder(g);

    expect(Object.keys(order).sort()).toEqual(['group', 'sibling']);
    expect(order.group.children).toEqual({ child: { x: 0, y: 0, w: 3, h: 2 } });
  });

  test('an empty nested grid contributes no children key', () => {
    const group = item('group', { x: 0, y: 0, w: 12, h: 6 });
    const nested = document.createElement('div');
    nested.className = 'nested-grid';
    group.appendChild(nested);

    expect(buildWidgetOrder(grid(group)).group.children).toBeUndefined();
  });

  test('elements that are not widgets are ignored', () => {
    const stray = document.createElement('div');
    stray.className = 'grid-stack-item';
    stray.id = 'placeholder'; // no _widgetEl suffix
    (stray as any).gridstackNode = { x: 0, y: 0, w: 1, h: 1 };
    const unnamed = document.createElement('div');
    unnamed.className = 'grid-stack-item';
    (unnamed as any).gridstackNode = { x: 0, y: 0, w: 1, h: 1 };

    expect(buildWidgetOrder(grid(stray, unnamed, item('real', { x: 1, y: 1, w: 2, h: 2 })))).toEqual({
      real: { x: 1, y: 1, w: 2, h: 2 },
    });
  });

  // GridStack attaches gridstackNode when it adopts an element. A widget swapped in by
  // HTMX is in the DOM for a moment before that happens. Reading `.x` off undefined threw,
  // which aborted the whole serialization — and the patch that did get sent (or the one
  // that never did) is what decides which widgets survive.
  test('a widget GridStack has not adopted yet is skipped, not fatal', () => {
    const g = grid(item('adopted', { x: 0, y: 0, w: 6, h: 3 }), item('pending', null));

    expect(() => buildWidgetOrder(g)).not.toThrow();
    expect(buildWidgetOrder(g)).toEqual({ adopted: { x: 0, y: 0, w: 6, h: 3 } });
  });

  test('a grid with nothing on it serializes to an empty patch', () => {
    expect(buildWidgetOrder(grid())).toEqual({});
  });
});

// The serializer's selectors are a contract with the Haskell renderer: Widget.hs emits
// `.grid-stack-item` elements whose id ends in `_widgetEl`, carrying gs-x/gs-y/gs-w/gs-h.
// DashboardWidgetsSpec asserts every widget type renders exactly that; these assert the
// client still reads exactly that, so neither side can drift alone. If they ever disagree,
// the affected widget is invisible to the patch — and the reorder handler rebuilds the
// dashboard purely from the patch, so it is deleted on the next drag.
describe('the markup contract with the server renderer', () => {
  test('a widget is identified by the _widgetEl id suffix, and the suffix is stripped', () => {
    const g = grid(item('some-widget-id', { x: 0, y: 0, w: 1, h: 1 }));
    expect(Object.keys(buildWidgetOrder(g))).toEqual(['some-widget-id']);
  });

  test('only .grid-stack-item children are considered', () => {
    const notAnItem = document.createElement('div');
    notAnItem.id = 'ghost_widgetEl';
    (notAnItem as any).gridstackNode = { x: 0, y: 0, w: 1, h: 1 };
    const g = grid(item('real', { x: 0, y: 0, w: 1, h: 1 }));
    g.appendChild(notAnItem);

    expect(Object.keys(buildWidgetOrder(g))).toEqual(['real']);
  });

  test('all four gs-* dimensions round-trip into the patch', () => {
    const g = grid(item('w', { x: 3, y: 2, w: 6, h: 4 }));
    expect(buildWidgetOrder(g).w).toEqual({ x: 3, y: 2, w: 6, h: 4 });
  });
});

describe('getActiveGrid', () => {
  test('prefers the visible grid when a tabbed dashboard keeps others mounted', () => {
    const hidden = grid(item('a', { x: 0, y: 0, w: 1, h: 1 }));
    hidden.classList.add('hidden');
    const visible = grid(item('b', { x: 0, y: 0, w: 1, h: 1 }));

    expect(getActiveGrid()).toBe(visible);
  });

  test('falls back to the only grid even if it is hidden mid-transition', () => {
    const only = grid();
    only.classList.add('hidden');
    expect(getActiveGrid()).toBe(only);
  });

  test('returns null when no dashboard grid is mounted', () => {
    expect(getActiveGrid()).toBeNull();
  });
});
