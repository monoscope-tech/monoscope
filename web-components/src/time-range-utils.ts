/** Pure time-range calculation helpers (no DOM deps, fully testable) */

/** The params that carry the page's time window. The server prefers `since` over from/to. */
export const TIME_PARAMS = ['since', 'from', 'to'] as const;

/**
 * Carry `keys` from the page's params onto a request/nav URL. Skips empty values: a key
 * that is absent and a key set to '' both mean "this URL says nothing about it", and
 * writing '' over a value the target already carries would silently widen its window.
 */
export function copyParams(source: URLSearchParams, target: URLSearchParams, keys: readonly string[] = TIME_PARAMS): void {
  for (const key of keys) {
    const value = source.get(key);
    if (value) target.set(key, value);
  }
}

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
