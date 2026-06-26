import { describe, test, expect } from 'vitest';
import { beginChartFetch } from '../src/chart-fetch-seq';

// Regression: dashboard widgets showed the "Query execution failed" overlay on
// top of freshly rendered bars. A failed /chart_data response arriving AFTER a
// newer successful one re-applied the error overlay. The sequencer must mark the
// older (first-issued) fetch as stale so its (error) result is discarded.
describe('beginChartFetch', () => {
  test('older fetch is stale once a newer one starts, regardless of resolve order', () => {
    const isStaleA = beginChartFetch('chart1'); // first issued (will fail, resolves last)
    const isStaleB = beginChartFetch('chart1'); // newer (succeeds)

    expect(isStaleB()).toBe(false); // latest fetch always applies its result
    expect(isStaleA()).toBe(true); // superseded — discard even though it resolves later
  });

  test('latest fetch wins after several in-flight requests', () => {
    const checks = Array.from({ length: 4 }, () => beginChartFetch('chart2'));
    checks.slice(0, -1).forEach((isStale) => expect(isStale()).toBe(true));
    expect(checks.at(-1)!()).toBe(false);
  });

  test('sequences are independent per chart', () => {
    const isStaleX = beginChartFetch('chartX');
    beginChartFetch('chartY'); // bumping another chart must not stale chartX
    expect(isStaleX()).toBe(false);
  });
});
