import { describe, expect, test } from 'vitest';
import { collapseLongTail, createSeriesConfig, heatmapOptions, updateChartConfiguration, visibleMax } from '../src/widgets';

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

describe('visibleMax', () => {
  test('rescales to the series left after hiding one: lines by their peak point, stacks by their tallest row', () => {
    const source = [['t', 'p50', 'p95'], [1, 4, 90], [2, 6, 120]];
    expect([visibleMax(source, true), visibleMax(source, true, { p95: false }), visibleMax(source, false, { p50: true }), visibleMax(source, true, { p50: false, p95: false })]).toEqual([120, 6, 126, undefined]);
  });
});

describe('heatmapOptions', () => {
  test('keeps empty log steps, sums a cell across statuses, and draws cells with errors as their own layer', () => {
    (window as any).echarts = { color: { lerp: (r: number, [a, b]: string[]) => `${a}>${b}@${r}` }, time: { format: (t: number) => String(t) } };
    const source = [['timestamp', '100000.0 / ok', '1000000.0 / ok', '1000000.0 / error', '10000000.0 / ok'], [2, 4, 9, 1, null], [1, null, 5, null, 2]];
    const o = heatmapOptions({}, source as any, 'ns', { brandColor: 'blue', textColor: 'grey', errorColor: 'red', tooltipBg: 'black' });
    expect(o.yAxis.data).toEqual(['100μs', '177.8μs', '316.2μs', '562.3μs', '1ms', '1.8ms', '3.2ms', '5.6ms', '10ms']);
    expect(o.xAxis.data).toEqual([1, 2]);
    const cells = (i: number) => o.series[i].data.map((c: number[]) => c.slice(0, 4));
    expect(cells(0)).toEqual([[0, 4, 5, 0], [0, 8, 2, 0], [1, 0, 4, 0]]);
    expect(cells(1)).toEqual([[1, 4, 10, 1]]);
    // Solid mixes into the background, so the scale bar starts at the shade the faintest cell has.
    expect(o.visualMap.map((v: any) => v.inRange.color)).toEqual([['black>blue@0.3', 'blue'], ['black>red@0.75', 'red']]);
    expect([o.legend.data, o.visualMap[0].text]).toEqual([['Errors', 'OK'], ['10 (max)', 'count: 1']]);
  });
});
