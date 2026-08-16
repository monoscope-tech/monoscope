// A log list embedded as a dashboard widget.
//
// On the log explorer the list owns the page: it reads the browser URL and the time
// picker triggers refetches on `document`. As a dashboard widget it owns nothing — the
// dashboard's time picker, variables and auto-refresh drive it, and its query comes from
// the widget definition via `initialFetchUrl`. That second mode had no tests at all, and
// it is the one the user reported as broken.
import { describe, test, expect, vi, beforeEach, afterEach } from 'vitest';
import { serverTransport, logPage, mountList, ids } from './log-list-harness';

const setPageUrl = (search: string) => window.history.replaceState({}, '', `/p/proj-1/dashboards/dash-1${search}`);

// The element as `renderLogsWidget` emits it: a project id and a widget-scoped fetch URL.
const embedded = async (props: Record<string, any> = {}) =>
  mountList({
    projectId: 'proj-1',
    initialFetchUrl: '/p/proj-1/log_explorer/data?json=true&layout=1&query=name%20!%3D%20null',
    ...props,
  } as any);

beforeEach(() => setPageUrl(''));
afterEach(() => vi.restoreAllMocks());

describe('embedded log widget: fetch URL', () => {
  test('keeps the widget query rather than reading the dashboard URL', async () => {
    const el = await embedded();
    el.transport = serverTransport(logPage(['a']));

    const url = new URL((el as any).buildJsonUrl());

    expect(url.pathname).toBe('/p/proj-1/log_explorer/data');
    expect(url.searchParams.get('query')).toBe('name != null');
  });

  test('adopts the dashboard time range from the page URL', async () => {
    setPageUrl('?since=24H&other=ignored');
    const el = await embedded();

    const url = new URL((el as any).buildJsonUrl());

    expect(url.searchParams.get('since')).toBe('24H');
    expect(url.searchParams.get('other')).toBeNull(); // only time params are adopted
  });

  test('an absolute dashboard range overrides the widget default', async () => {
    setPageUrl('?from=2024-01-01T00:00:00Z&to=2024-01-02T00:00:00Z');
    const el = await embedded();

    const url = new URL((el as any).buildJsonUrl());

    expect(url.searchParams.get('from')).toBe('2024-01-01T00:00:00Z');
    expect(url.searchParams.get('to')).toBe('2024-01-02T00:00:00Z');
  });

  test('a standalone list ignores initialFetchUrl and derives the path from its mode', async () => {
    setPageUrl('?since=1H');
    const el = await mountList({ projectId: 'proj-1', mode: 'patterns' } as any);

    expect((el as any).buildJsonUrl()).toContain('/p/proj-1/log_explorer/patterns');
  });
});

describe('embedded log widget: refresh triggers', () => {
  // The dashboard time picker (TimePicker.hs) triggers on `document`.
  test('refetches when the dashboard time picker fires', async () => {
    const el = await embedded();
    const refetch = vi.spyOn(el as any, 'refetchLogs').mockResolvedValue(undefined);

    document.dispatchEvent(new CustomEvent('update-query', { detail: {} }));

    // waitFor, not a fixed sleep: the refetch is debounced 50ms and a hard timeout
    // races that under parallel load.
    await vi.waitFor(() => expect(refetch).toHaveBeenCalled());
  });

  // Regression: the dashboard auto-refresh timer and the variable fallback both
  // dispatch on `window`. A window-dispatched event never reaches a `document`
  // listener, so a logs widget sat frozen on a dashboard set to refresh every 30s
  // while every chart around it updated.
  test('refetches when the dashboard auto-refresh fires on window', async () => {
    const el = await embedded();
    const refetch = vi.spyOn(el as any, 'refetchLogs').mockResolvedValue(undefined);

    window.dispatchEvent(new CustomEvent('update-query'));

    await vi.waitFor(() => expect(refetch).toHaveBeenCalled());
  });

  test('a widened-time-range refetch from the list itself does not loop', async () => {
    const el = await embedded();
    const refetch = vi.spyOn(el as any, 'refetchLogs').mockResolvedValue(undefined);

    window.dispatchEvent(new CustomEvent('update-query', { detail: { source: 'expand-timerange' } }));
    await new Promise((r) => setTimeout(r, 300)); // well past the 50ms debounce

    expect(refetch).not.toHaveBeenCalled();
  });

  test('stops listening once the widget is removed from the dashboard', async () => {
    const el = await embedded();
    const refetch = vi.spyOn(el as any, 'refetchLogs').mockResolvedValue(undefined);
    el.remove();

    window.dispatchEvent(new CustomEvent('update-query'));
    document.dispatchEvent(new CustomEvent('update-query', { detail: {} }));
    await new Promise((r) => setTimeout(r, 300)); // well past the 50ms debounce

    expect(refetch).not.toHaveBeenCalled();
  });
});

describe('embedded log widget: chrome', () => {
  test('offers no "load newer" edge — a widget is a window, not a live tail', async () => {
    const el = await embedded();
    el.transport = serverTransport(logPage(['a', 'b']));
    await el.fetchData('first', true);

    expect(((el as any).virtualListItems as any[]).some((i) => i.type === 'fetchRecent')).toBe(false);
    expect(ids(el)).toEqual(['a', 'b']);
  });

  test('still pages history, so a widget is not capped at one page', async () => {
    const el = await embedded();
    el.transport = serverTransport(logPage(['a', 'b']), logPage(['c']));
    await el.fetchData('first', true);
    await el.fetchData('more', false, false, true);

    expect(ids(el)).toEqual(['a', 'b', 'c']);
  });
});
