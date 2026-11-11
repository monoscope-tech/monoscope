import { afterEach, expect, test, vi } from 'vitest';
import { chartWidget } from '../src/widgets';

afterEach(() => {
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
  delete (window as any).echarts;
  document.body.innerHTML = '';
});

// Regression: a cut-off world-map load ("Unexpected end of JSON input") was console.error'd, so
// the browser SDK filed it as an issue; the loader resets and the next render retries.
test('an unreadable world map is warned, not filed as an exception', async () => {
  document.body.innerHTML = '<div id="geo"></div>';
  (window as any).echarts = { getMap: () => undefined, getInstanceByDom: () => null };
  vi.stubGlobal('fetch', vi.fn(async () => new Response('', { status: 200 })));
  const errors = vi.spyOn(console, 'error').mockImplementation(() => {});
  const warnings = vi.spyOn(console, 'warn').mockImplementation(() => {});
  chartWidget({ chartType: 'map', chartId: 'geo', opt: { dataset: {}, series: [] }, pid: 'p', query: '', widgetType: 'map' } as any);
  await vi.waitFor(() => expect(warnings).toHaveBeenCalledWith('world map failed to load', expect.anything()));
  expect(errors).not.toHaveBeenCalled();
});
