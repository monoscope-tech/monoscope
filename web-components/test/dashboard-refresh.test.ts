// Dashboard auto-refresh.
//
// The refresh dropdown (TimePicker.hs) sends `setRefreshInterval` to window, and the
// timer it starts is what re-runs every widget on the dashboard. A leaked timer here
// doubles the query load on every panel silently, and a timer that never starts leaves
// a dashboard the user believes is live showing stale numbers.
import { describe, test, expect, vi, beforeEach, afterEach } from 'vitest';
import '../src/main';

const selectInterval = (interval: unknown) => window.dispatchEvent(new CustomEvent('setRefreshInterval', { detail: { interval } }));
const mountTransport = (live: boolean, defaultWindow?: string) => {
  document.body.innerHTML = `
    <div ${defaultWindow ? `data-default-window="${defaultWindow}"` : ''}>
      ${live ? '<span data-live-range="true"></span>' : ''}
      <div data-time-transport>
        <button data-live-toggle></button>
        <button data-next-window></button>
        <span data-pause-icon></span>
        <span data-play-icon></span>
      </div>
      <span data-live-badge></span>
    </div>`;
  return document.querySelector<HTMLElement>('[data-time-transport]')!;
};

let refreshes: number;
const countRefreshes = () => refreshes++;

beforeEach(() => {
  vi.useFakeTimers();
  refreshes = 0;
  document.body.innerHTML = '';
  window.history.replaceState({}, '', '/p/proj/log_explorer?since=15M');
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

  test('marks timer ticks so a log list can merge rather than replace its rows', () => {
    const sources: unknown[] = [];
    const recordSource = (event: Event) => sources.push((event as CustomEvent).detail?.source);
    window.addEventListener('update-query', recordSource);
    selectInterval(15_000);

    vi.advanceTimersByTime(15_000);

    window.removeEventListener('update-query', recordSource);
    expect(sources).toEqual(['auto-refresh']);
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

  test('leaving a timed page stops its global refresh timer', () => {
    selectInterval(10_000);
    document.body.innerHTML = '';
    document.dispatchEvent(new CustomEvent('htmx:after:swap'));

    vi.advanceTimersByTime(30_000);

    expect(refreshes).toBe(0);
    expect(window.dashboardRefreshTimer).toBeNull();
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

  // The bundle is deferred, so a transport rendered above it fires `on load`
  // before window.initTimeTransport exists — three "'window.initTimeTransport'
  // is null" criticals on 2026-08-28, each one a dashboard that then never
  // refreshed. Adoption is what makes the missed hook harmless.
  test('a transport whose on-load hook never ran is adopted, and one that ran is not re-initialised', () => {
    const transport = mountTransport(true); // mounted WITHOUT calling initTimeTransport
    expect(transport.dataset.live).toBeUndefined();

    document.dispatchEvent(new CustomEvent('htmx:after:swap'));

    expect(transport.dataset.live).toBe('true');
    vi.advanceTimersByTime(15_000);
    expect(refreshes).toBe(1);

    // Already initialised: a second swap must not restart or double the timer.
    document.dispatchEvent(new CustomEvent('htmx:after:swap'));
    vi.advanceTimersByTime(15_000);
    expect(refreshes).toBe(2);
  });

  test('entering a historical window stops the live timer and updates its controls', () => {
    window.initTimeTransport(mountTransport(true));
    vi.advanceTimersByTime(15_000);
    expect(refreshes).toBe(1);

    const historical = mountTransport(false);
    window.initTimeTransport(historical);
    vi.advanceTimersByTime(60_000);

    expect(refreshes).toBe(1);
    expect(window.dashboardRefreshInterval).toBe(0);
    expect(window.dashboardRefreshTimer).toBeNull();
    expect(historical.querySelector<HTMLButtonElement>('[data-live-toggle]')!.ariaLabel).toBe('Return to live');
    expect(historical.querySelector<HTMLButtonElement>('[data-next-window]')!.disabled).toBe(false);
    expect(document.querySelector<HTMLElement>('[data-live-badge]')!.textContent).toBe('PAUSED');
  });

  test('pause and resume keep a live range on the selected cadence', () => {
    const transport = mountTransport(true);
    window.initTimeTransport(transport);

    window.toggleLiveRefresh(transport);
    expect(window.dashboardRefreshInterval).toBe(0);
    expect(transport.querySelector<HTMLButtonElement>('[data-live-toggle]')!.ariaLabel).toBe('Resume live updates');

    window.toggleLiveRefresh(transport);
    vi.advanceTimersByTime(30_000);
    expect(window.dashboardRefreshInterval).toBe(15_000);
    expect(refreshes).toBe(2);
  });

  test('return to live replaces the absolute range without dropping other URL state', () => {
    window.history.replaceState({}, '', '/p/proj/infrastructure/hosts?from=2024-01-01T00:00:00Z&to=2024-01-01T01:00:00Z&provider=aws&cols=cpu,memory');
    const setParams = vi.spyOn(window, 'setParams').mockImplementation(() => undefined);

    window.toggleLiveRefresh(mountTransport(false, '5M'));

    expect(setParams).toHaveBeenCalledWith({ since: '5M', from: '', to: '' }, true);
    setParams.mockRestore();
  });
});
