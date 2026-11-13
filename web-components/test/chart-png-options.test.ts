import { describe, expect, it, vi } from 'vitest';
import * as echarts from 'echarts';
import monitorFixture from './fixtures/slack-monitor-gap/request.json';
import { legendLayout } from '../src/chart-legend';
import { preparePngOptions } from '../src/chart-png-options';

describe('PNG chart layout', () => {
  it.each([false, true])('renders aggregate table cells without treating counts as timestamps (dark=%s)', (darkMode) => {
    const options: any = {
      dataset: { source: [['events', 'percent'], [0, 100]] },
      xAxis: { type: 'time' }, yAxis: { type: 'value' },
      series: [{ type: 'bar', encode: { x: 0, y: 1 } }],
    };
    preparePngOptions(options, 960, 320, darkMode, 'slack', 'table');
    const chart = echarts.init(null, darkMode ? 'dark' : undefined, { renderer: 'svg', ssr: true, width: 960, height: 320 });
    try {
      chart.setOption(options);
      const svg = chart.renderToSVGString();
      expect(svg).toMatch(/>events<\/text>/);
      expect(svg).toContain('percent');
      expect(svg).toMatch(/>0<\/text>/);
      expect(svg).toMatch(/>100<\/text>/);
      expect(svg).not.toContain('1970');
    } finally { chart.dispose(); }
  });

  it.each([
    { name: 'no rows', source: [['events']], text: 'No rows returned' },
    { name: 'unknown value', source: [['events'], [null]], text: '—' },
    { name: 'clipped results', source: [Array.from({ length: 10 }, (_, i) => `Column ${i}`), ...Array.from({ length: 200 }, () => Array(10).fill(1))], text: 'Showing 1 of 200 rows · 2 of 10 columns' },
  ])('labels $name in a table PNG', ({ source, text }) => {
    const options: any = { dataset: { source } };
    preparePngOptions(options, 320, 160, false, 'slack', 'table');
    const chart = echarts.init(null, undefined, { renderer: 'svg', ssr: true, width: 320, height: 160 });
    try {
      chart.setOption(options);
      expect(chart.renderToSVGString()).toContain(text);
    } finally { chart.dispose(); }
  });

  it.each([false, true])('keeps a 36px legend reserve and fills the image (dark=%s)', (darkMode) => {
    const options = {
      grid: { width: '100%', left: '0%', top: 8, bottom: 36, containLabel: true },
      legend: { bottom: 2 },
      xAxis: { type: 'category', data: ['Mon', 'Tue'] },
      yAxis: { type: 'value' },
      series: [{ name: 'Events', type: 'bar', data: [80, 100] }, { name: 'Errors', type: 'bar', data: [5, 8] }],
    };
    preparePngOptions(options, 900, 300, darkMode);
    const chart = echarts.init(null, darkMode ? 'dark' : undefined, { renderer: 'svg', ssr: true, width: 900, height: 300 });
    try {
      chart.setOption(options);
      // Regression: the old renderer turned 36px into 37%, shrinking the plot to ~140px.
      const zero = chart.convertToPixel({ yAxisIndex: 0 }, 0) as number;
      const top = chart.convertToPixel({ yAxisIndex: 0 }, 100) as number;
      expect(zero - top).toBeGreaterThan(210);
      expect(zero).toBeGreaterThan(230);
      expect(zero).toBeLessThan(270);
      const svg = chart.renderToSVGString();
      expect(svg).toContain(darkMode ? '#111827' : '#ffffff');
      if (darkMode) expect(svg).toContain('#cbd5e1');
    } finally { chart.dispose(); }
  });
  it('preserves percentage and zero insets', () => {
    const options = { grid: { left: '10%', right: 0, top: 0, bottom: '2%' } };
    preparePngOptions(options, 900, 300, false);
    expect(options.grid).toEqual({ left: 99, right: 9, top: 3, bottom: 9 });
  });

  it('preserves configured bounds for a stacked threshold chart', () => {
    const options: any = {
      grid: {}, dataset: { source: [['timestamp', 'value'], [0, 20]] },
      xAxis: { type: 'time' }, yAxis: { type: 'value', min: -100, max: 100 },
      series: [{ type: 'line', stack: 'counts', markLine: { data: [{ yAxis: 60 }] } }],
    };
    preparePngOptions(options, 960, 320, false);
    expect(options.yAxis).toMatchObject({ min: -100, max: 100 });
  });

  it.each([
    { name: 'late onset', rows: [[780000, 18], [840000, 22], [900000, 24]], threshold: 60 },
    { name: 'missing checks', rows: [[0, 84], [60000, null], [900000, 18]], threshold: 60 },
    { name: 'single negative reading', rows: [[900000, -12]], threshold: -5 },
  ])('renders the full window and threshold for $name without inventing readings', ({ rows, threshold }) => {
    const options: any = {
      grid: { left: 0, top: 8, bottom: 8, containLabel: true },
      tooltip: { show: true }, toolbox: { show: true },
      dataset: { source: [['timestamp', 'Monitor value'], ...rows] },
      xAxis: { type: 'time', min: 0, max: 900000, boundaryGap: [0, 0.01] },
      yAxis: { type: 'value', min: 0 },
      series: [{ type: 'line', encode: { x: 0, y: 1 }, markLine: { data: [{ yAxis: threshold }] } }],
    };
    preparePngOptions(options, 960, 320, false);
    const chart = echarts.init(null, undefined, { renderer: 'svg', ssr: true, width: 960, height: 320 });
    try {
      chart.setOption(options);
      const start = chart.convertToPixel({ xAxisIndex: 0 }, 0) as number;
      const end = chart.convertToPixel({ xAxisIndex: 0 }, 900000) as number;
      expect(end - start).toBeGreaterThan(850);
      expect(start).toBeGreaterThan(0);
      expect(end).toBeLessThan(960);
      const thresholdY = chart.convertToPixel({ yAxisIndex: 0 }, threshold) as number;
      expect(thresholdY).toBeGreaterThan(8);
      expect(thresholdY).toBeLessThan(290);
      expect(options.dataset.source.slice(1)).toEqual(rows);
      expect(options.series[0]).toMatchObject({ connectNulls: false, smooth: false, showSymbol: true });
      expect(options.xAxis.axisLabel).toMatchObject({ showMinLabel: true, showMaxLabel: true });
      expect(chart.renderToSVGString()).not.toContain('NaN');
    } finally { chart.dispose(); }
  });
});

describe('PNG one-series legend', () => {
  it.each([[['p95'], false], [['p95', 'p99'], true]])('shows a legend for %j only when it names more than one series', (names, shown) => {
    const options: any = { legend: { show: true }, xAxis: { type: 'time' }, yAxis: { type: 'value' }, series: names.map((name) => ({ type: 'line', name, data: [] })) };
    preparePngOptions(options, 960, 320, false);
    expect(options.legend.show !== false).toBe(shown);
  });
});

describe('Slack PNG profile', () => {
  it.each(['bar', 'line'])('keeps the full window and readable labels for %s measurements', (type) => {
    const rows = [['timestamp', 'Events'], [0, 3], [900000, 8]];
    const options: any = {
      grid: { left: 0, right: 0, top: 8, bottom: 36, containLabel: true },
      legend: { show: true }, dataset: { source: rows },
      xAxis: { type: 'time', min: 0, max: 900000 }, yAxis: { type: 'value' },
      series: [{ type, name: 'Events', encode: { x: 0, y: 1 } }],
    };
    preparePngOptions(options, 960, 320, false, 'slack');
    expect(options.legend.show).toBe(false);
    expect(options.xAxis.axisLabel.fontSize).toBeGreaterThanOrEqual(14);
    expect(options.dataset.source).toEqual(rows);
    const chart = echarts.init(null, undefined, { renderer: 'svg', ssr: true, width: 960, height: 320 });
    try {
      chart.setOption(options);
      const start = chart.convertToPixel({ xAxisIndex: 0 }, 0) as number;
      const end = chart.convertToPixel({ xAxisIndex: 0 }, 900000) as number;
      expect(end - start).toBeGreaterThan(800);
      expect(start).toBeGreaterThan(24);
      expect(end).toBeLessThanOrEqual(936);
      expect(chart.renderToSVGString()).not.toContain('NaN');
      if (type === 'bar') {
        expect(options.series[0].clip).toBe(false);
        expect(options.series[0].barMaxWidth / 2).toBeLessThan(Math.min(start, 960 - end));
      }
    } finally { chart.dispose(); }
  });
});

// Use the producer's actual options, including its default stack setting.
it.each([[84, 100, 60], [-12, -10, -5], [18, 20, 60]])('keeps captured gauge readings %s/%s and threshold %s visible', (first, last, threshold) => {
  const options: any = structuredClone(monitorFixture.renderer_input.echarts);
  options.dataset.source[1][1] = first;
  options.dataset.source[3][1] = last;
  options.series[0].markLine.data[0].yAxis = threshold;
  const source = structuredClone(options.dataset.source);
  preparePngOptions(options, 960, 320, false, 'slack');
  const chart = echarts.init(null, undefined, { renderer: 'svg', ssr: true, width: 960, height: 320 });
  try {
    chart.setOption(options);
    for (const value of [first, last, threshold]) {
      const y = chart.convertToPixel({ yAxisIndex: 0 }, value) as number;
      expect(y).toBeGreaterThan(16);
      expect(y).toBeLessThan(275);
    }
    expect(options.dataset.source).toEqual(source);
  } finally { chart.dispose(); }
});

it.each([
  { name: 'empty', rows: [], text: 'No observations returned for this window' },
  { name: 'all missing', rows: [[0, null], [900000, null]], text: 'No observations returned for this window' },
  { name: 'gap', rows: [[0, 84], [60000, null], [900000, 18]], text: 'Missing measurements · gaps are not interpolated' },
  { name: 'late start', rows: [[840000, 84], [900000, 18]], text: 'No earlier measurements in this window' },
  { name: 'unordered complete start', rows: [[900000, 18], [0, 84]], text: undefined },
  { name: 'single point', rows: [[900000, 18]], text: 'No earlier measurements in this window' },
  { name: 'recorded zero', rows: [[0, 0], [900000, 0]], text: undefined },
])('labels $name without changing observations or claiming count coverage', ({ rows, text }) => {
  for (const type of ['line', 'bar']) {
    const options: any = {
      dataset: { source: [['timestamp', 'Measurement'], ...rows] },
      xAxis: { type: 'time', min: 0, max: 900000 }, yAxis: { type: 'value' },
      series: [{ type, encode: { x: 0, y: 1 } }],
    };
    preparePngOptions(options, 960, 320, false, 'slack');
    const chart = echarts.init(null, undefined, { renderer: 'svg', ssr: true, width: 960, height: 320 });
    try {
      chart.setOption(options);
      const expected = type === 'bar' && rows.some(row => row[1] !== null) ? undefined : text;
      const svg = chart.renderToSVGString();
      if (expected) expect(svg).toContain(expected);
      else expect(options.graphic).toBeUndefined();
      expect(options.dataset.source.slice(1)).toEqual(rows);
      expect(options.xAxis).toMatchObject({ min: 0, max: 900000 });
    } finally { chart.dispose(); }
  }
});

describe('PNG legend', () => {
  it('lays every legend entry onto a PNG, which cannot scroll, as a grid without the shared prefix, clear of the plot', () => {
    const names = Array.from({ length: 9 }, (_, i) => `oteldemo.ProductCatalogService/GetProduct-${i}`);
    const options: any = {
      grid: { left: '0%', top: 8, bottom: 36, containLabel: true },
      legend: { type: 'plain', bottom: 2, data: names },
      xAxis: { type: 'category', data: ['a', 'b'] }, yAxis: { type: 'value' },
      series: names.map((name) => ({ name, type: 'bar', stack: 's', data: [1, 2] })),
    };
    preparePngOptions(options, 900, 300, false);
    const chart = echarts.init(null, undefined, { renderer: 'svg', ssr: true, width: 900, height: 300 });
    try {
      chart.setOption(options);
      const svg = chart.renderToSVGString();
      names.forEach((_, i) => expect(svg).toContain(`GetProduct-${i}`));
      expect(svg).not.toContain('oteldemo.ProductCatalogService/');
      expect(options.grid.bottom).toBeGreaterThan(36 + 18);
      const zero = chart.convertToPixel({ yAxisIndex: 0 }, 0) as number;
      expect(zero).toBeLessThan(300 - options.grid.bottom + 20);
    } finally { chart.dispose(); }
  });
});

describe('legend grid', () => {
  it('drops the prefix every name shares, keeps Other whole, and gives every entry one column width', () => {
    const names = ['srv-captain--timescaledb.1.w9cqbjq', 'srv-captain--redpanda-2.1.mups2ld', 'srv-captain--monoscope.3.oxuci4y', 'Other (16)'];
    const { legend } = legendLayout({ legend: { bottom: 0, data: names }, series: names.map((name) => ({ name })) }, 900, 300);
    expect(names.map(legend.formatter)).toEqual(['{n|timescaledb.1.w9cqbjq}', '{n|redpanda-2.1.mups2ld}', '{n|monoscope.3.oxuci4y}', '{n|Other (16)}']);
    expect(legend).toMatchObject({ left: 4, textStyle: { rich: { n: { width: expect.any(Number) } } } });
  });
});

// The explorer drew "p…" for p50: a later chart's legend had re-set the shared canvas font.
it('measures each legend in its own font, whatever the last legend set', async () => {
  vi.resetModules();
  vi.stubGlobal('OffscreenCanvas', class {
    getContext() { const c = { font: '', measureText: (t: string) => ({ width: t.length * parseFloat(c.font) * 0.6 }) }; return c; }
  });
  const fresh = (await import('../src/chart-legend')).legendLayout;
  const at = (fontSize: number) => fresh({ legend: { data: ['p50', 'p95'], textStyle: { fontSize } }, series: [{ name: 'p50' }, { name: 'p95' }] }, 900, 300).legend;
  const small = at(10);
  at(20);
  expect(small.formatter('p50')).toBe('{n|p50}');
  vi.unstubAllGlobals();
});

// The explorer's level legend read "ERR…": twelve floored column shares left 36px for a 36.1px name.
it('never cuts the widest name when the row has room for it', async () => {
  vi.resetModules();
  vi.stubGlobal('OffscreenCanvas', class { getContext() { return { font: '', measureText: (t: string) => ({ width: t.length * 7.222 }) }; } });
  const fresh = (await import('../src/chart-legend')).legendLayout;
  const names = ['ERROR', 'INFO', 'OK', 'UNSET', 'WARN', 'null'];
  const { legend } = fresh({ legend: { top: 0, data: names, textStyle: { fontSize: 10 } }, series: names.map((name) => ({ name })) }, 924, 200);
  expect(names.map(legend.formatter)).toEqual(names.map((n) => `{n|${n}}`));
  vi.unstubAllGlobals();
});

describe('PNG time axis over days', () => {
  it('labels a multi-day window, edges included, as dates rather than times of day', () => {
    const to = Date.UTC(2026, 10, 13, 15, 33, 20), from = to - 7 * 86400000;
    const options: any = {
      dataset: { source: [['timestamp', 'Errors'], [from, 3], [to, 8]] },
      xAxis: { type: 'time', min: from, max: to }, yAxis: { type: 'value' },
      series: [{ type: 'bar', encode: { x: 0, y: 1 } }],
    };
    preparePngOptions(options, 560, 200, false);
    const chart = echarts.init(null, undefined, { renderer: 'svg', ssr: true, width: 560, height: 200 });
    try {
      chart.setOption(options);
      const texts = [...chart.renderToSVGString().matchAll(/<text[^>]*>([^<]+)<\/text>/g)].map((m) => m[1]);
      const dates = texts.filter((t) => /^[A-Z][a-z]{2} \d{1,2}$/.test(t));
      expect(texts.filter((t) => /\d:\d\d/.test(t))).toEqual([]);
      expect(dates.length).toBeGreaterThanOrEqual(2);
      expect(new Set(dates).size).toBe(dates.length);
    } finally { chart.dispose(); }
  });
});

describe('Email PNG profile', () => {
  it('reads at the width an email shows it, on the email surface rather than Slack’s', () => {
    const options: any = { textStyle: {}, xAxis: { type: 'time' }, yAxis: { type: 'value' }, series: [{ type: 'line', data: [] }] };
    preparePngOptions(options, 600, 240, true, 'email');
    expect([options.xAxis.axisLabel.fontSize, options.yAxis.axisLabel.fontSize]).toEqual([14, 14]);
    expect(options.backgroundColor).not.toBe('#1a1d21');
    const sparse: any = { textStyle: {}, dataset: { source: [['timestamp', 'p95'], [60_000, 3], [120_000, 4]] }, xAxis: { type: 'time', min: 0, max: 120_000 }, yAxis: { type: 'value' }, series: [{ type: 'line', encode: { x: 0, y: 1 } }] };
    preparePngOptions(sparse, 600, 240, false, 'email');
    expect(sparse.graphic).toBeUndefined();
  });
});

describe('one-series legend', () => {
  it('drops a legend that would only repeat the lone series name, but keeps a value table', () => {
    const chart = (names: string[], orient?: string) => legendLayout({ legend: { show: true, orient, bottom: 0 }, series: names.map((name) => ({ name })) }, 600, 240).legend.show !== false;
    expect([chart(['count_']), chart(['p95', 'p99']), chart(['count_'], 'vertical')]).toEqual([false, true, true]);
    expect(legendLayout({ grid: { bottom: 36 }, legend: { show: true, bottom: 2 }, series: [{ name: 'count_' }] }, 600, 240).grid.bottom).toBe(8);
  });
});
