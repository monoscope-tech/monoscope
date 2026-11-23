/** Pure time-range calculation helpers (no DOM deps, fully testable) */

/** The params that carry the page's time window. The server prefers `since` over from/to. */
export const TIME_PARAMS = ['since', 'from', 'to'] as const;

/** Global telemetry scope carried by the page URL. */
export const SCOPE_PARAMS = ['service_scope', 'environment'] as const;

/** Context an embedded telemetry view must inherit from its containing page. */
export const PAGE_CONTEXT_PARAMS = [...TIME_PARAMS, ...SCOPE_PARAMS] as const;

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

export const SINCE_UNITS = {
  S: { label: 'Second', ms: 1000 },
  M: { label: 'Minute', ms: 60_000 },
  H: { label: 'Hour', ms: 3_600_000 },
  D: { label: 'Day', ms: 86_400_000 },
} as const;
type SinceUnit = keyof typeof SINCE_UNITS;

export function parseSince(since: string): { count: number; unit: SinceUnit } | null {
  const m = since.match(/^(\d+)\s*([SMHD])$/i);
  return m ? { count: Number(m[1]), unit: m[2].toUpperCase() as SinceUnit } : null;
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

/** Read dashboard constants from the nearest `[data-constants]` attribute. */
export const getDashboardConstants = (el?: Element | null): Record<string, string> => {
  const constantsEl = el?.closest('[data-constants]') ?? document.querySelector('[data-constants]');
  if (!constantsEl) return {};
  try {
    return JSON.parse(constantsEl.getAttribute('data-constants') || '{}');
  } catch {
    return {};
  }
};
