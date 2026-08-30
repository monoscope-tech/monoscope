import { describe, expect, test } from 'vitest';
import { collapseLongTail, createSeriesConfig } from '../src/widgets';

describe('collapseLongTail', () => {
  test('keeps the eight largest series and preserves the remainder as Other', () => {
    const data = [
      ['timestamp', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'],
      [1, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1],
      [2, 20, 18, 16, 14, 12, 10, 8, 6, null, null],
    ];

    expect(collapseLongTail(data)).toEqual([
      ['timestamp', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'Other (2)'],
      [1, 10, 9, 8, 7, 6, 5, 4, 3, 3],
      [2, 20, 18, 16, 14, 12, 10, 8, 6, null],
    ]);
  });

  test('leaves eight or fewer series untouched', () => {
    const data = [['timestamp', 'a', 'b'], [1, 2, 1]];

    expect(collapseLongTail(data)).toBe(data);
  });
});

describe('line-series hierarchy', () => {
  test('gives only the leading line a soft area fill', () => {
    (window as any).echarts = {
      color: { modifyAlpha: (color: string, opacity: number) => `${color}/${opacity}` },
      graphic: { LinearGradient: class { constructor(_: number, __: number, ___: number, ____: number, public stops: unknown) {} } },
    };

    const widget = { chartType: 'line', widgetType: 'timeseries', chartId: 'containers-cpu' } as any;
    const leading = createSeriesConfig(widget, 'leading', 0, {});
    const secondary = createSeriesConfig(widget, 'secondary', 1, {});

    expect(leading.areaStyle).toBeDefined();
    expect(secondary.areaStyle).toBeUndefined();
  });
});
