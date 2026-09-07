import { describe, expect, it } from 'vitest';
import * as echarts from 'echarts';
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
});
