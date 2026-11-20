// The server sends ECharts formatters as `function(...)` strings. Only these keys carry code:
// dataset cells, legend and series names are telemetry, and reviving them ran attacker
// strings in the viewer's browser and in the PNG renderer.
export const FUNCTION_KEYS = new Set(['formatter', 'valueFormatter', 'min', 'max', 'interval']);

export const reviveFunction = (key: string, value: unknown): unknown => {
  if (typeof value !== 'string' || !FUNCTION_KEYS.has(key) || !value.trim().startsWith('function')) return value;
  try { return new Function('return (' + value + ')')(); } catch { return value; }
};

export const reviveFunctions = (obj: any, key = ''): any =>
  Array.isArray(obj) ? obj.map(v => reviveFunctions(v))
  : obj !== null && typeof obj === 'object' ? Object.fromEntries(Object.entries(obj).map(([k, v]) => [k, reviveFunctions(v, k)]))
  : reviveFunction(key, obj);
