import { describe, expect, it } from 'vitest';
import { reviveFunction, reviveFunctions } from '../src/chart-functions';

// Regression: dataset cells, legend and series names come from telemetry or a client widget.
// Reviving by key name at any depth let a `formatter` cell inside dataset.source run code in
// the viewer's browser and in the chart-cli PNG renderer.
describe('reviveFunctions', () => {
  const payload = 'function(){}, (globalThis.__chartPwn = 1), 0';
  const opts = {
    tooltip: { valueFormatter: 'function(value) { return value + "ms"; }', formatter: payload },
    yAxis: { min: 'function(v) { return v.min - 1; }', axisLabel: { formatter: 'function(value, index) { return "x" + value; }' } },
    legend: { data: [payload], formatter: payload },
    series: [
      { name: payload, type: 'line', data: [{ formatter: payload, label: { formatter: payload } }],
        markLine: { data: [{ label: { formatter: 'function(p) { return "t" + p.value; }' } }] } },
    ],
    dataset: { source: [['timestamp', payload], [1700000000, 1], { formatter: payload, min: payload, valueFormatter: payload }] },
  };

  it('revives only server option paths and leaves data inert', () => {
    delete (globalThis as any).__chartPwn;
    const out = reviveFunctions(JSON.parse(JSON.stringify(opts)));
    expect(out.tooltip.valueFormatter(5)).toBe('5ms');
    expect(out.yAxis.min({ min: 3 })).toBe(2);
    expect(out.yAxis.axisLabel.formatter(2)).toBe('x2');
    expect(out.series[0].markLine.data[0].label.formatter({ value: 4 })).toBe('t4');
    expect(out.tooltip.formatter).toBe(payload);
    expect(out.legend.data[0]).toBe(payload);
    expect(out.series[0].name).toBe(payload);
    expect(out.series[0].data[0].label.formatter).toBe(payload);
    expect(out.dataset.source[0][1]).toBe(payload);
    expect(out.dataset.source[2].formatter).toBe(payload);
    expect((globalThis as any).__chartPwn).toBeUndefined();
  });

  it('a payload under a formatter path that is not a function stays a string', () => {
    expect(reviveFunction('yAxis.axisLabel.formatter', '{value} ms')).toBe('{value} ms');
  });
});
