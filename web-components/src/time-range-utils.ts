/** Pure time-range calculation helpers (no DOM deps, fully testable) */

const SINCE_EXPANSION_MAP: Record<string, string> = {
  '5M': '15M', '15M': '30M', '30M': '1H', '1H': '3H', '3H': '6H',
  '6H': '12H', '12H': '24H', '24H': '3D', '3D': '7D', '7D': '14D',
};

export function expandSince(since: string): string {
  return SINCE_EXPANSION_MAP[since] ?? '14D';
}

/** Expand a from/to range by moving `from` earlier — doubles the range, minimum 15 min */
export function expandFromToRange(from: string | null, to: string): string {
  const toMs = new Date(to).getTime();
  const fromMs = from ? new Date(from).getTime() : toMs - 3600000;
  const expansion = Math.max(toMs - fromMs, 15 * 60 * 1000);
  return new Date(fromMs - expansion).toISOString();
}

/** Parse chart zoom batch params into ISO strings, returning null if invalid */
export function parseChartZoom(batch?: { startValue: string; endValue: string }[]): { from: string; to: string } | null {
  const zoom = batch?.[0];
  if (!zoom || zoom.startValue === undefined || zoom.endValue === undefined) return null;
  return { from: new Date(zoom.startValue).toISOString(), to: new Date(zoom.endValue).toISOString() };
}
