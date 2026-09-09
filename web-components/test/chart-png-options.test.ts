import { describe, expect, it } from 'vitest';
import * as echarts from 'echarts';
import monitorFixture from './fixtures/slack-monitor-gap/request.json';
import { preparePngOptions } from '../src/chart-png-options';

describe('PNG chart layout', () => {
  it.each([false, true])('keeps a 36px legend reserve and fills the image (dark=%s)', (darkMode) => {
    const options = {
      grid: { width: '100%', left: '0%', top: 8, bottom: 36, containLabel: true },
      legend: { bottom: 2 },
      xAxis: { type: 'category', data: ['Mon', 'Tue'] },
      yAxis: { type: 'value' },
      series: [{ name: 'Events', type: 'bar', data: [80, 100] }],
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
