// The time picker and URL state, shared by the log explorer and every dashboard.
//
// `updateTimePicker` is the single writer of the range label, the hidden range input and
// the URL's since/from/to — and the log list calls it directly when "Show earlier events"
// widens the window. Getting the label wrong is cosmetic; leaving a stale `since` next to
// a new `from`/`to` is not, because the server reads both and the reader gets a window
// they did not ask for.
import { describe, test, expect, beforeEach } from 'vitest';
import '../src/main';

const updateTimePicker = (...args: Parameters<typeof window.updateTimePicker>) => window.updateTimePicker(...args);
const params = () => new URLSearchParams(window.location.search);

// The picker writes into elements the server rendered; `n` is the default prefix.
const mountPicker = (prefix = 'n') => {
  const label = document.createElement('span');
  label.id = `${prefix}-currentRange`;
  const input = document.createElement('input');
  input.id = `${prefix}-custom_range_input`;
  document.body.append(label, input);
  return { label, input };
};

beforeEach(() => {
  document.body.innerHTML = '';
  window.history.replaceState({}, '', '/p/proj/log_explorer');
});

describe('relative ranges', () => {
  test.each([
    ['1H', 'Last 1 Hour'],
    ['24H', 'Last 24 Hours'],
    ['1D', 'Last 1 Day'],
    ['7D', 'Last 7 Days'],
    ['30M', 'Last 30 Minutes'],
    ['45S', 'Last 45 Seconds'],
  ])('%s reads as "%s"', (since, expected) => {
    mountPicker();
    expect(updateTimePicker({ since })).toBe(expected);
  });

  test('singular and plural are not mixed up', () => {
    mountPicker();
    expect(updateTimePicker({ since: '1M' })).toBe('Last 1 Minute');
    expect(updateTimePicker({ since: '2M' })).toBe('Last 2 Minutes');
  });

  test('a range the picker does not recognise still renders readably', () => {
    mountPicker();
    expect(updateTimePicker({ since: 'all-time' })).toBe('Last all-time');
  });

  test('an explicit label overrides the derived one', () => {
    mountPicker();
    expect(updateTimePicker({ since: '1H' }, { label: 'Since deploy' })).toBe('Since deploy');
  });

  test('writes the label and the hidden input the form submits', () => {
    const { label, input } = mountPicker();
    updateTimePicker({ since: '6H' });
    expect(label.innerText).toBe('Last 6 Hours');
    expect(input.value).toBe('6H');
  });
});

describe('URL state', () => {
  // A relative range and an absolute one are mutually exclusive; the server reads both,
  // so leaving the old one behind silently narrows or widens the window.
  test('choosing a relative range clears any absolute one', () => {
    mountPicker();
    window.history.replaceState({}, '', '/p/proj/log_explorer?from=2024-01-01T00:00:00Z&to=2024-01-02T00:00:00Z');

    updateTimePicker({ since: '1H' });

    expect(params().get('since')).toBe('1H');
    expect(params().get('from')).toBe('');
    expect(params().get('to')).toBe('');
  });

  test('choosing an absolute range clears the relative one', () => {
    mountPicker();
    window.history.replaceState({}, '', '/p/proj/log_explorer?since=1H');

    updateTimePicker({ from: '2024-01-01T00:00:00Z', to: '2024-01-02T00:00:00Z' });

    expect(params().get('from')).toBe('2024-01-01T00:00:00Z');
    expect(params().get('to')).toBe('2024-01-02T00:00:00Z');
    expect(params().get('since')).toBe('');
  });

  // The log list widens the range itself and owns the URL for that request, so it asks
  // the picker to update the label only — writing params here would fight it.
  test('skipSetParams updates the label without touching the URL', () => {
    const { label } = mountPicker();
    window.history.replaceState({}, '', '/p/proj/log_explorer?since=1H&query=abc');

    const shown = updateTimePicker({ since: '3H' }, { skipSetParams: true });

    expect(shown).toBe('Last 3 Hours');
    expect(label.innerText).toBe('Last 3 Hours');
    expect(params().get('since')).toBe('1H'); // untouched
    expect(params().get('query')).toBe('abc');
  });

  // The app is an HTMX/morph SPA: main.ts loads once, and other paths (the query editor,
  // the column and facet sync, chart zoom) write params straight through
  // history.replaceState afterwards. Reading the URL only once at load meant the next
  // time-range change rewrote it from that stale snapshot — silently dropping the query
  // and column selection the reader had just made, including from a link they then shared.
  test('params written after page load survive a range change', () => {
    mountPicker();
    window.history.replaceState({}, '', '/p/proj/log_explorer?query=status%3D%3D500&cols=a,b');

    updateTimePicker({ since: '1H' });

    expect(params().get('query')).toBe('status==500');
    expect(params().get('cols')).toBe('a,b');
  });

  test('two range changes in a row do not resurrect a param the second removed', () => {
    mountPicker();
    updateTimePicker({ from: '2024-01-01T00:00:00Z', to: '2024-01-02T00:00:00Z' });
    updateTimePicker({ since: '1H' });

    expect(params().get('since')).toBe('1H');
    expect(params().get('from')).toBe('');
    expect(params().get('to')).toBe('');
  });
});

describe('malformed input', () => {
  // A dashboard can be saved with a half-filled range. It must leave the current window
  // alone rather than blanking the picker or writing a partial range to the URL.
  test('neither since nor a complete from/to changes anything', () => {
    const { label, input } = mountPicker();
    label.innerText = 'Last 1 Hour';
    input.value = '1H';
    window.history.replaceState({}, '', '/p/proj/log_explorer?since=1H');

    expect(updateTimePicker({})).toBe('');
    expect(updateTimePicker({ from: '2024-01-01T00:00:00Z' })).toBe('');
    expect(updateTimePicker({ to: '2024-01-02T00:00:00Z' })).toBe('');

    expect(label.innerText).toBe('Last 1 Hour');
    expect(input.value).toBe('1H');
    expect(params().get('since')).toBe('1H');
  });

  test('a picker that is not on the page does not throw', () => {
    expect(() => updateTimePicker({ since: '1H' })).not.toThrow();
    expect(params().get('since')).toBe('1H');
  });

  test('a prefixed picker is addressed by its own ids', () => {
    const other = mountPicker('n');
    const mine = mountPicker('alertPr');

    updateTimePicker({ since: '2H' }, { targetPr: 'alertPr' });

    expect(mine.label.innerText).toBe('Last 2 Hours');
    expect(other.label.innerText ?? '').toBe(''); // the default picker is left alone
  });
});

describe('updateUrlState', () => {
  test('sets and deletes keys without disturbing the path', () => {
    window.history.replaceState({}, '', '/p/proj/dashboards/d1?a=1&b=2');

    window.updateUrlState('a', '9');
    expect(params().get('a')).toBe('9');
    expect(window.location.pathname).toBe('/p/proj/dashboards/d1');

    window.updateUrlState('b', '', 'delete');
    expect(params().has('b')).toBe(false);
  });

  test('applies the same action to a whole group of keys at once', () => {
    window.history.replaceState({}, '', '/p/proj/dashboards/d1?since=1H&from=x&to=y');

    window.updateUrlState(['from', 'to'], '', 'delete');

    expect(params().has('from')).toBe(false);
    expect(params().has('to')).toBe(false);
    expect(params().get('since')).toBe('1H');
  });
});
