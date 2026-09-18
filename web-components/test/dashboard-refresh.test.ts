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
      <button data-live-range="${live}"><span data-live-badge></span></button>
      <div data-time-transport>
        <button data-live-toggle></button>
        <button data-next-window></button>
        <span data-pause-icon></span>
        <span data-play-icon></span>
        <span data-refresh-label></span>
      </div>
    </div>`;
  return document.querySelector<HTMLElement>('[data-time-transport]')!;
};

const mountCoordinatedLiveData = () => {
  document.body.innerHTML = `
    <div data-live-data>
      <button data-live-data-trigger><span data-live-data-indicator></span><span data-live-data-label></span><span data-live-data-label-short></span><span data-live-data-announcer></span></button>
      <span data-row-stream-status></span>
      <input type="checkbox" data-row-stream-toggle>
      <button data-query-refresh-toggle><span data-query-refresh-status></span><span data-query-refresh-state></span></button>
      <button data-live-range="true"><span data-live-badge></span></button>
      <div data-time-transport>
        <button data-live-toggle><span data-pause-icon></span><span data-play-icon></span><span data-refresh-label></span></button>
        <button data-next-window></button>
        <button data-refresh-option data-value="0"></button>
        <button data-refresh-option data-value="15000"></button>
        <select data-refresh-select><option value="0">Off</option><option value="15000">15 seconds</option></select>
      </div>
    </div>`;
  const liveData = document.querySelector<HTMLElement>('[data-live-data]')!;
  return {
    liveData,
    transport: liveData.querySelector<HTMLElement>('[data-time-transport]')!,
    rows: liveData.querySelector<HTMLInputElement>('[data-row-stream-toggle]')!,
  };
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
  test('coordinates row streaming and query refresh behind one trustworthy status', () => {
    const control = mountCoordinatedLiveData();
    window.initTimeTransport(control.transport);

    expect(control.liveData.dataset.state).toBe('stream-paused');
    expect(control.transport.dataset.state).toBe('paused');
    expect(control.transport.dataset.interval).toBe('15000');

    window.toggleLiveData(control.liveData, control.transport);
    expect(control.rows.checked).toBe(true);
    expect(control.liveData.dataset.state).toBe('live');
    expect(control.transport.dataset.state).toBe('live');

    window.toggleLiveData(control.liveData, control.transport);
    expect(control.rows.checked).toBe(false);
    expect(window.dashboardRefreshInterval).toBe(0);
    expect(control.liveData.dataset.state).toBe('paused');
    expect(control.transport.dataset.interval).toBe('0');
  });

  test('names the independently paused mechanism', () => {
    const control = mountCoordinatedLiveData();
    control.rows.checked = true;
    window.initTimeTransport(control.transport);

    window.toggleLiveRefresh(control.transport);
    expect(control.liveData.dataset.state).toBe('refresh-paused');

    window.toggleLiveRefresh(control.transport);
    control.rows.checked = false;
    window.syncTimeTransports();
    expect(control.liveData.dataset.state).toBe('stream-paused');
  });

  test('resyncs the unified status when row streaming stops outside the toggle', () => {
    const control = mountCoordinatedLiveData();
    control.rows.checked = true;
    window.initTimeTransport(control.transport);
    expect(control.liveData.dataset.state).toBe('live');

    control.rows.checked = false;
    window.syncTimeTransports();

    expect(control.liveData.dataset.state).toBe('stream-paused');
  });

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
    const transport = mountTransport(true);
    window.initTimeTransport(transport);
    window.setTimeRefreshInterval(transport, 10_000);
    window.destroyTimeTransport(transport);
    document.body.innerHTML = '';

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

  test('local transport initialization is idempotent', () => {
    const transport = mountTransport(true);
    window.initTimeTransport(transport);
    expect(transport.dataset.live).toBe('true');
    vi.advanceTimersByTime(15_000);
    expect(refreshes).toBe(1);

    window.initTimeTransport(transport);
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
    expect(historical.dataset.state).toBe('historical');
    expect(historical.dataset.interval).toBe('0');
  });

  test('pause and resume keep a live range on the selected cadence', () => {
    const transport = mountTransport(true);
    window.initTimeTransport(transport);

    window.toggleLiveRefresh(transport);
    expect(window.dashboardRefreshInterval).toBe(0);
    expect(transport.dataset.state).toBe('paused');

    window.toggleLiveRefresh(transport);
    vi.advanceTimersByTime(30_000);
    expect(window.dashboardRefreshInterval).toBe(15_000);
    expect(refreshes).toBe(2);
  });

  test('the dashboard live-data wrapper routes Resume through refresh-only behavior', () => {
    const transport = mountTransport(true);
    const wrapper = transport.parentElement!;
    wrapper.dataset.liveData = '';
    wrapper.dataset.liveMode = 'refresh-only';
    const toggle = vi.spyOn(window, 'toggleLiveRefresh');
    window.initTimeTransport(transport);

    window.toggleLiveData(wrapper, transport);

    expect(toggle).toHaveBeenCalledOnce();
    expect(toggle).toHaveBeenCalledWith(transport);
    expect(window.dashboardRefreshInterval).toBe(0);
    expect(transport.dataset.state).toBe('paused');
    toggle.mockRestore();
  });

  test('return to live replaces the absolute range without dropping other URL state', () => {
    window.history.replaceState({}, '', '/p/proj/infrastructure/hosts?from=2024-01-01T00:00:00Z&to=2024-01-01T01:00:00Z&provider=aws&cols=cpu,memory');
    const setParams = vi.spyOn(window, 'setParams').mockImplementation(() => undefined);
    let updates = 0;
    const onUpdate = () => { updates += 1; };
    window.addEventListener('update-query', onUpdate);

    window.toggleLiveRefresh(mountTransport(false, '5M'));

    // Applied in place rather than reloaded; the widgets pick the new range up themselves.
    expect(setParams).toHaveBeenCalledWith({ since: '5M', from: '', to: '' });
    expect(updates).toBe(1);
    window.removeEventListener('update-query', onUpdate);
    setParams.mockRestore();
  });
});
