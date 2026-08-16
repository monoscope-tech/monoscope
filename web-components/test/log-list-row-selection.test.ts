// Selecting a row to open its detail panel.
//
// The open row used to be marked by adding `bg-fillBrand-strong` in the click handler and
// searching the DOM to unmark the previous one. That search also matched the latency bar
// *inside* a row (same class), so it stripped a bar's colour while leaving the old row
// marked — two rows looking selected at once. And because the virtualizer destroys a row's
// element when it scrolls out of the runway, the mark did not survive scrolling away and
// back. Selection is now state the template renders, so both problems are unrepresentable.
import { describe, test, expect, vi, beforeEach, afterEach } from 'vitest';
import { mountList } from './log-list-harness';

const SELECTED = 'bg-fillBrand-strong';

// A rendered row: the <tr> the click lands on, plus the latency bar inside it that shares
// the selection class.
const buildRows = (n: number) => {
  const tbody = document.createElement('tbody');
  const rows = Array.from({ length: n }, (_, i) => {
    const tr = document.createElement('tr');
    tr.dataset.rowId = `r${i}`;
    const bar = document.createElement('div');
    bar.className = `h-full ${SELECTED} rounded-sm`; // the latency bar
    tr.appendChild(bar);
    tbody.appendChild(tr);
    return { tr, bar };
  });
  document.body.appendChild(tbody);
  return rows;
};

const clickRow = (el: any, tr: HTMLElement, id: string) => {
  const event = { currentTarget: tr, stopPropagation: () => {} } as any;
  el.toggleLogRow(event, [id, '2024-01-01T00:00:00Z', 'spans'], 'proj-1');
};

let ajax: ReturnType<typeof vi.fn>;

// The chrome the handler reaches for: the detail panel it swaps into, its resizer, and the
// loading indicator. main.ts installs updateUrlState as a window global in production.
const mountChrome = () => {
  for (const id of ['log_details_container', 'resizer-details_width-wrapper', 'details_indicator']) {
    const node = document.createElement('div');
    node.id = id;
    document.body.appendChild(node);
  }
};

beforeEach(() => {
  document.body.innerHTML = '';
  window.history.replaceState({}, '', '/p/proj-1/log_explorer');
  mountChrome();
  (window as any).updateUrlState = (key: string, value: string) => {
    const p = new URLSearchParams(window.location.search);
    p.set(key, value);
    window.history.replaceState({}, '', `${window.location.pathname}?${p}`);
  };
  ajax = vi.fn().mockResolvedValue(undefined);
  (window as any).htmx = { ...(window as any).htmx, ajax };
});
afterEach(() => vi.restoreAllMocks());

describe('selecting a row', () => {
  test('records the clicked row as the open one', async () => {
    const el = await mountList();
    const rows = buildRows(2);

    clickRow(el, rows[0].tr, 'r0');

    expect((el as any).openRowId).toBe('r0');
  });

  // Exactly one row can be open, because it is a single value rather than a class the
  // handler has to remember to remove from somewhere.
  test('opening another row replaces the first', async () => {
    const el = await mountList();
    const rows = buildRows(4);

    clickRow(el, rows[1].tr, 'r1');
    clickRow(el, rows[3].tr, 'r3');
    clickRow(el, rows[0].tr, 'r0');

    expect((el as any).openRowId).toBe('r0');
  });

  // The same class paints the latency bar inside every row. The old DOM search stripped it.
  test('leaves the latency bars inside rows alone', async () => {
    const el = await mountList();
    const rows = buildRows(3);

    clickRow(el, rows[0].tr, 'r0');
    clickRow(el, rows[2].tr, 'r2');

    expect(rows.every((r) => r.bar.classList.contains(SELECTED))).toBe(true);
  });

  // The virtualizer destroys and recreates row elements as they leave and re-enter its
  // runway. State survives that; a class added to the element did not.
  test('the open row survives the list re-rendering around it', async () => {
    const el = await mountList();
    const rows = buildRows(2);
    clickRow(el, rows[0].tr, 'r0');

    document.body.innerHTML = ''; // every row element is gone
    buildRows(2); // ...and rebuilt, as the virtualizer would
    await el.updateComplete;

    expect((el as any).openRowId).toBe('r0');
  });

  test('re-clicking the open row keeps it open', async () => {
    const el = await mountList();
    const rows = buildRows(2);

    clickRow(el, rows[0].tr, 'r0');
    clickRow(el, rows[0].tr, 'r0');

    expect((el as any).openRowId).toBe('r0');
  });
});

describe('the request it issues', () => {
  test('asks for that row\'s detail and records it in the URL so the view is shareable', async () => {
    const el = await mountList();
    const rows = buildRows(1);

    clickRow(el, rows[0].tr, 'evt-9');

    expect(ajax).toHaveBeenCalledWith('GET', expect.stringContaining('/p/proj-1/log_explorer/evt-9/'), expect.objectContaining({ target: '#log_details_container' }));
    expect(new URLSearchParams(window.location.search).get('target_event')).toContain('evt-9');
  });
});
