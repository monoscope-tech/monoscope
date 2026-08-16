// Dashboard auto-refresh.
//
// The refresh dropdown (TimePicker.hs) sends `setRefreshInterval` to window, and the
// timer it starts is what re-runs every widget on the dashboard. A leaked timer here
// doubles the query load on every panel silently, and a timer that never starts leaves
// a dashboard the user believes is live showing stale numbers.
import { describe, test, expect, vi, beforeEach, afterEach } from 'vitest';
import '../src/widgets';

const selectInterval = (interval: unknown) => window.dispatchEvent(new CustomEvent('setRefreshInterval', { detail: { interval } }));

let refreshes: number;
const countRefreshes = () => refreshes++;

beforeEach(() => {
  vi.useFakeTimers();
  refreshes = 0;
  window.addEventListener('update-query', countRefreshes);
});

afterEach(() => {
  window.removeEventListener('update-query', countRefreshes);
  selectInterval(0); // stop any timer this test started
  vi.useRealTimers();
});

describe('auto-refresh interval', () => {
  test('choosing an interval refreshes the dashboard on that cadence', () => {
    selectInterval(30_000);

    vi.advanceTimersByTime(90_000);

    expect(refreshes).toBe(3);
  });

  // Each selection must replace the previous timer. Leaking one doubles the query
  // volume against every widget, and the dashboard gets quietly more expensive the
  // more times the user changes their mind.
  test('changing the interval replaces the timer rather than stacking another', () => {
    selectInterval(30_000);
    selectInterval(10_000);

    vi.advanceTimersByTime(30_000);

    expect(refreshes).toBe(3); // 3 x 10s, not 3 + 1
  });

  test('pausing stops refreshing', () => {
    selectInterval(10_000);
    vi.advanceTimersByTime(10_000);
    selectInterval(0);

    vi.advanceTimersByTime(120_000);

    expect(refreshes).toBe(1);
  });

  test('a value that is not a number does not start a runaway timer', () => {
    selectInterval('not-a-number');

    vi.advanceTimersByTime(120_000);

    expect(refreshes).toBe(0);
  });

  test('an event with no detail is ignored', () => {
    window.dispatchEvent(new CustomEvent('setRefreshInterval'));

    vi.advanceTimersByTime(120_000);

    expect(refreshes).toBe(0);
  });

  // The dropdown label and the actual cadence have to agree, and the only shared
  // state is window.dashboardRefreshInterval — it was declared, initialised once, and
  // then never updated, so anything reading it saw "Paused" no matter the selection.
  test('the exposed interval reflects what is actually running', () => {
    selectInterval(15_000);
    expect(window.dashboardRefreshInterval).toBe(15_000);

    selectInterval(0);
    expect(window.dashboardRefreshInterval).toBe(0);
    expect(window.dashboardRefreshTimer).toBeNull();
  });
});
