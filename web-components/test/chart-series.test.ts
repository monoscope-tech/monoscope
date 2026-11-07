import { describe, expect, test } from 'vitest';
import { collapseLongTail, createSeriesConfig, isLogAxis, updateChartConfiguration } from '../src/widgets';

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

  test('folds a non-additive tail by its max: a sum of p95 latencies is no latency at all', () => {
    const data = [['t', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'], [1, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]];
    expect(collapseLongTail(data, (vs) => Math.max(...vs)).at(-1)!.at(-1)).toBe(2);
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

describe('sparse line series', () => {
  test('a series scraped coarser than its bins is bridged; a dense or gappy one is not', () => {
    (window as any).echarts = {
      color: { modifyAlpha: (color: string) => color },
      graphic: { LinearGradient: class {} },
    };
    const data = [['t', 'everyOther', 'dense'], [1, 5, 1], [2, null, 2], [3, 6, 3], [4, null, null], [5, 7, null], [6, null, 4]];
    const opt = updateChartConfiguration({ chartType: 'line', widgetType: 'timeseries_line', chartId: 'c' } as any, { yAxis: {}, legend: {}, dataset: {} }, data);
    expect(opt.series.map((s: any) => s.connectNulls)).toEqual([true, false]);
  });
});

describe('isLogAxis', () => {
  test('latency lines read on a log axis; counts, bars and stat sparklines stay linear', () => {
    const w = (chartType: string, widgetType: string, unit: string) => isLogAxis({ chartType, widgetType, unit } as any);
    expect([w('line', 'timeseries_line', 'ns'), w('line', 'timeseries_line', 'ms'), w('line', 'timeseries_line', 'reqs'), w('bar', 'timeseries', 'ms'), w('line', 'timeseries_stat', 'ms')]).toEqual([true, true, false, false, false]);
  });
});
