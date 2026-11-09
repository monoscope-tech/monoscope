import { describe, expect, it } from 'vitest';
import { bindFunctionsToObjects } from '../src/widgets';

// Regression for "RangeError: Maximum call stack size exceeded" on dashboards: the option's
// tooltip.appendTo is a DOM element whose htmx expando refers back to the element.
describe('bindFunctionsToObjects', () => {
  it('binds nested formatters without walking into DOM elements', () => {
    const el = document.createElement('div') as any;
    el._htmx = { listeners: [{ fromElt: el }] };
    const opt: any = { series: [{ label: { formatter() { return this; } } }], tooltip: { appendTo: el } };
    bindFunctionsToObjects(opt, opt);
    expect(opt.series[0].label.formatter()).toBe(opt);
    expect(opt.tooltip.appendTo).toBe(el);
  });
});
