// <local-time> — the only thing that makes server-rendered timestamps agree with the
// client-formatted ones in the log list.
//
// Haskell knows the instant, not the viewer's zone, so every server-rendered timestamp
// ships as UTC and this element rewrites it locally. It has to survive HTMX swaps and
// morphs, which is why it re-renders on attribute change rather than only on insert.
import { describe, test, expect, beforeEach } from 'vitest';
import { format } from 'date-fns';
import '../src/local-time';

const ISO = '2024-06-16T03:38:47.176Z';

const mount = (attrs: Record<string, string>, fallback = '') => {
  const el = document.createElement('local-time');
  Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, v));
  el.textContent = fallback;
  document.body.appendChild(el);
  return el;
};

beforeEach(() => {
  document.body.innerHTML = '';
});

describe('local-time', () => {
  test('is registered as a custom element', () => {
    expect(customElements.get('local-time')).toBeDefined();
  });

  test('rewrites the UTC timestamp into the viewer\'s own zone', () => {
    const el = mount({ datetime: ISO });
    expect(el.textContent).toBe(format(new Date(ISO), 'MMM dd, HH:mm:ss'));
  });

  test('an explicit format attribute wins', () => {
    const el = mount({ datetime: ISO, format: 'yyyy-MM-dd' });
    expect(el.textContent).toBe(format(new Date(ISO), 'yyyy-MM-dd'));
  });

  // HTMX morphs update attributes in place rather than replacing the element, so a
  // timestamp that only formatted on insert would keep showing the previous row's time.
  test('re-renders when the timestamp is swapped in place', () => {
    const el = mount({ datetime: ISO });
    const next = '2025-01-02T10:11:12.000Z';

    el.setAttribute('datetime', next);

    expect(el.textContent).toBe(format(new Date(next), 'MMM dd, HH:mm:ss'));
  });

  test('re-renders when only the format changes', () => {
    const el = mount({ datetime: ISO });
    el.setAttribute('format', 'HH:mm');
    expect(el.textContent).toBe(format(new Date(ISO), 'HH:mm'));
  });

  test('setting the same value again does not need to change anything', () => {
    const el = mount({ datetime: ISO });
    const before = el.textContent;
    el.setAttribute('datetime', ISO);
    expect(el.textContent).toBe(before);
  });

  // The server-rendered UTC text is the no-JS fallback: a broken or missing datetime
  // must leave it standing rather than blanking the cell.
  test('keeps the server-rendered fallback when the timestamp is unusable', () => {
    expect(mount({}, 'Jun 16, 03:38:47').textContent).toBe('Jun 16, 03:38:47');
    expect(mount({ datetime: 'not-a-date' }, 'Jun 16, 03:38:47').textContent).toBe('Jun 16, 03:38:47');
    expect(mount({ datetime: '' }, 'Jun 16, 03:38:47').textContent).toBe('Jun 16, 03:38:47');
  });

  test('an element that was never connected does not render on attribute change', () => {
    const el = document.createElement('local-time');
    el.textContent = 'untouched';
    el.setAttribute('datetime', ISO);
    expect(el.textContent).toBe('untouched');
  });
});
