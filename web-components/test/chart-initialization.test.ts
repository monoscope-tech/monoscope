import { describe, expect, test } from 'vitest';
import { isNearChartViewport } from '../src/chart-initialization';

describe('isNearChartViewport', () => {
  test('does not initialize charts beyond the small prefetch boundary', () => {
    expect(isNearChartViewport({ top: 1_250, bottom: 1_450 }, 1_000)).toBe(false);
  });

  test('keeps charts just below the viewport ready for scrolling', () => {
    expect(isNearChartViewport({ top: 1_149, bottom: 1_349 }, 1_000)).toBe(true);
  });
});
