// Selecting a row to open its detail panel.
//
// The clicked row is marked with `bg-fillBrand-strong` and the previously selected one is
// unmarked. That same class is also what paints the latency bar *inside* every row and a
// handful of other in-row elements, so "find the previously selected row" has to mean the
// row, not the first descendant that happens to share the class. Getting it wrong leaves
// two rows looking selected and strips the colour off a latency bar.
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
  test('marks the clicked row', async () => {
    const el = await mountList();
    const rows = buildRows(2);

    clickRow(el, rows[0].tr, 'r0');

    expect(rows[0].tr.classList.contains(SELECTED)).toBe(true);
  });

  // The bug: scoping the "previously selected" lookup to descendants matched a latency bar
  // long before it reached the previous row, so the old row stayed marked.
  test('unmarks the previously selected row', async () => {
    const el = await mountList();
    const rows = buildRows(3);

    clickRow(el, rows[0].tr, 'r0');
    clickRow(el, rows[2].tr, 'r2');

    expect(rows[2].tr.classList.contains(SELECTED)).toBe(true);
    expect(rows[0].tr.classList.contains(SELECTED)).toBe(false);
  });

  test('exactly one row is ever selected', async () => {
    const el = await mountList();
    const rows = buildRows(4);

    clickRow(el, rows[1].tr, 'r1');
    clickRow(el, rows[3].tr, 'r3');
    clickRow(el, rows[0].tr, 'r0');

    expect(rows.filter((r) => r.tr.classList.contains(SELECTED)).map((_, i) => i)).toHaveLength(1);
    expect(rows[0].tr.classList.contains(SELECTED)).toBe(true);
  });

  // The same class paints the latency bar. Stripping it turns a bar colourless — and the
  // bar is the row's whole point in a waterfall.
  test('leaves the latency bars inside rows alone', async () => {
    const el = await mountList();
    const rows = buildRows(3);

    clickRow(el, rows[0].tr, 'r0');
    clickRow(el, rows[2].tr, 'r2');

    expect(rows.every((r) => r.bar.classList.contains(SELECTED))).toBe(true);
  });

  test('re-clicking the selected row keeps it selected', async () => {
    const el = await mountList();
    const rows = buildRows(2);

    clickRow(el, rows[0].tr, 'r0');
    clickRow(el, rows[0].tr, 'r0');

    expect(rows[0].tr.classList.contains(SELECTED)).toBe(true);
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
