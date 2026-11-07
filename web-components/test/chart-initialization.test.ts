import { describe, expect, test } from 'vitest';
import * as echarts from 'echarts';
import { chartLayoutForSize, isNearChartViewport, registerChartDisposer } from '../src/widgets';

test('disposes a chart removed by an HTMX morph navigation', () => {
  const chart = document.createElement('div');
  chart.id = 'detached-chart';
  chart.dataset.chartWidget = '';
  document.body.append(chart);
  let disposals = 0;
  registerChartDisposer(chart.id, () => disposals++);

  chart.remove();
  document.dispatchEvent(new CustomEvent('htmx:after:swap'));

  expect(disposals).toBe(1);
});

describe('isNearChartViewport', () => {
  test('does not initialize charts beyond the small prefetch boundary', () => {
    expect(isNearChartViewport({ top: 1_250, bottom: 1_450 }, 1_000)).toBe(false);
  });

  test('keeps charts just below the viewport ready for scrolling', () => {
    expect(isNearChartViewport({ top: 1_149, bottom: 1_349 }, 1_000)).toBe(true);
  });
});

// The screenshot that prompted this: a 1000x170 "Requests by Endpoint" widget drew its 23:00 tick
// on top of a scrolling legend. Every entry must render, below the axis labels, never over them.
describe('legend layout', () => {
  const names = ['POST /api/cart', 'GET /api/recommendations', 'GET /api/data', 'GET /api/cart', 'POST /ofrep/v1/evaluate/flags/loadGeneratorTraffic',
    'POST /oteldemo.CartService/GetCart', 'GET /api/products', 'POST /api/checkout', 'Other (2)'];
  const draw = (legend: object, width: number, height: number) => {
    const options = {
      grid: { left: '0%', right: 16, top: 8, bottom: 36, containLabel: true },
      legend: { show: true, bottom: 2, itemWidth: 9, itemHeight: 9, itemGap: 9, padding: [3, 6, 3, 6], textStyle: { fontSize: 12 }, data: names, ...legend },
      xAxis: { type: 'time', axisLabel: { margin: 8 } }, yAxis: { type: 'value' },
      dataset: { source: [['t', ...names], ...[0, 1, 2].map((i) => [i * 60_000, ...names.map((_, j) => 10 * i + j)])] },
      series: names.map((name, i) => ({ name, type: 'bar', stack: 's', encode: { x: 0, y: i + 1 } })),
    };
    const layout = chartLayoutForSize(options, width, height);
    const chart = echarts.init(null, undefined, { renderer: 'svg', ssr: true, width, height });
    chart.setOption(layout);
    const rect = (type: string) => {
      const view = (chart as any).getViewOfComponentModel((chart as any).getModel().getComponent(type));
      const r = view.group.getBoundingRect().clone();
      r.applyTransform(view.group.getLocalTransform());
      return r;
    };
    const result = { layout, legend: rect('legend'), axis: rect('xAxis'), svg: chart.renderToSVGString() };
    chart.dispose();
    return result;
  };

  test.each([[1000, 170], [1000, 300], [420, 300]])('wraps below the axis labels at %ix%i', (width, height) => {
    const { layout, legend, axis, svg } = draw({}, width, height);
    expect(layout.legend.type).toBe('plain');
    expect(legend.y).toBeGreaterThanOrEqual(axis.y + axis.height);
    expect(legend.y + legend.height).toBeLessThanOrEqual(height);
    expect(svg).toContain('Other (2)');
  });

  test('right puts a name/total table beside the plot, and falls back to wrapping when too short', () => {
    const { layout, legend, axis, svg } = draw({ orient: 'vertical', bottom: undefined }, 1000, 300);
    expect(legend.x).toBeGreaterThanOrEqual(axis.x + axis.width);
    expect(layout.grid.right).toBeGreaterThan(200);
    expect(svg).toContain('>45<'); // POST /api/cart: 0 + 10 + 20 + ... summed over the three bins
    const short = draw({ orient: 'vertical', bottom: undefined }, 1000, 120);
    expect(short.layout.legend.orient).toBe('horizontal');
    expect(short.legend.y).toBeGreaterThanOrEqual(short.axis.y + short.axis.height);
  });
});
