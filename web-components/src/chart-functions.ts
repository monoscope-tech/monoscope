// The server sends ECharts formatters as `function(...)` strings, only at these option paths
// (array indices dropped). Everything else, dataset cells above all, is data: reviving by key
// name at any depth ran a `formatter` key inside dataset.source in the viewer's browser and
// in the chart-cli PNG renderer.
export const FUNCTION_PATHS = new Set([
  'tooltip.valueFormatter',
  'yAxis.min',
  'yAxis.splitLine.interval',
  'yAxis.axisLabel.formatter',
  'series.markLine.data.label.formatter',
]);

export const reviveFunction = (path: string, value: unknown): unknown => {
  if (typeof value !== 'string' || !FUNCTION_PATHS.has(path) || !value.trim().startsWith('function')) return value;
  try { return new Function('return (' + value + ')')(); } catch { return value; }
};

export const reviveFunctions = (obj: any, path = ''): any =>
  Array.isArray(obj) ? obj.map(v => reviveFunctions(v, path))
  : obj !== null && typeof obj === 'object' ? Object.fromEntries(Object.entries(obj).map(([k, v]) => [k, reviveFunctions(v, path ? `${path}.${k}` : k)]))
  : reviveFunction(path, obj);
