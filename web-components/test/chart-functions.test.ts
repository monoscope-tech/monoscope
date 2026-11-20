import { describe, expect, it } from 'vitest';
import { reviveFunction, reviveFunctions } from '../src/chart-functions';

// Regression: dataset cells, legend and series names come from telemetry. Reviving every
// 'function'-prefixed string let anyone who could write a group-by value run code in the
// viewer's browser and in the chart-cli PNG renderer.
describe('reviveFunctions', () => {
  const payload = 'function(){}(globalThis.__chartPwn = 1)';
  const opts = {
    tooltip: { valueFormatter: 'function(value) { return value + "ms"; }' },
    yAxis: { min: 'function(v) { return v.min - 1; }', axisLabel: { formatter: 'function(value, index) { return "x" + value; }' } },
    legend: { data: [payload] },
    series: [{ name: payload, type: 'line' }],
    dataset: { source: [['timestamp', payload], [1700000000, 1]] },
  };

  it('revives only formatter keys and leaves telemetry strings inert', () => {
    delete (globalThis as any).__chartPwn;
    const out = reviveFunctions(opts);
    expect(typeof out.tooltip.valueFormatter).toBe('function');
    expect(out.tooltip.valueFormatter(5)).toBe('5ms');
    expect(typeof out.yAxis.min).toBe('function');
    expect(out.yAxis.axisLabel.formatter(2)).toBe('x2');
    expect(out.legend.data[0]).toBe(payload);
    expect(out.series[0].name).toBe(payload);
    expect(out.dataset.source[0][1]).toBe(payload);
    expect((globalThis as any).__chartPwn).toBeUndefined();
  });

  it('works as a JSON.parse reviver the same way', () => {
    delete (globalThis as any).__chartPwn;
    const out = JSON.parse(JSON.stringify(opts), reviveFunction);
    expect(typeof out.yAxis.axisLabel.formatter).toBe('function');
    expect(out.dataset.source[0][1]).toBe(payload);
    expect((globalThis as any).__chartPwn).toBeUndefined();
  });

  it('a payload under a formatter key that is not a function stays a string', () => {
    expect(reviveFunction('formatter', '{value} ms')).toBe('{value} ms');
  });
});
