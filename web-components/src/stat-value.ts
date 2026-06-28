// Pure helpers for a timeseries_stat widget's big number and for chart number
// formatting. Kept side-effect free so they can be unit-tested directly and
// reused by widgets.ts. The big number is computed entirely client-side; the
// SummarizeBy tags ('rate'/'mean'/…) match the Haskell enum in
// src/Pkg/Components/Widget.hs that the dashboard YAML's summarize_by parses to.

/** Format numbers with magnitude suffixes for chart/stat display. */
export const formatNumber = (n: number | null | undefined): string => {
  if (n == null || Number.isNaN(n)) return 'N/A'; // guard first: NaN (e.g. Number(undefined)) must not reach the DOM as "NaN". Loose type matches untyped JS callers.
  if (n >= 1_000_000_000) return `${(n / 1_000_000_000).toFixed(1)}B`;
  if (n >= 1_000_000) return `${(n / 1_000_000).toFixed(1)}M`;
  if (n >= 1_000) return `${(n / 1_000).toFixed(1)}K`;
  if (!Number.isInteger(n)) {
    if (n >= 100) return Math.round(n).toString();
    if (n >= 10) return parseFloat(n.toFixed(1)).toString();
    return parseFloat(n.toFixed(2)).toString();
  }
  return n.toString();
};

/** Convert a value in a time unit (h, m, s, ms, μs/us, ns) to nanoseconds. */
export const convertToNanoseconds = (value: number, unit: string): number => {
  const f: Record<string, number> = { h: 3_600_000_000_000, m: 60_000_000_000, s: 1_000_000_000, ms: 1_000_000, μs: 1_000, us: 1_000, ns: 1 };
  return value * (f[unit] || 1);
};

// Descending [threshold-ns, suffix] steps; first step whose threshold ns clears wins.
const DURATION_STEPS: [number, string][] = [
  [3_600_000_000_000, 'h'],
  [60_000_000_000, 'm'],
  [1_000_000_000, 's'],
  [1_000_000, 'ms'],
  [1_000, 'μs'],
];

/** Format a nanosecond duration into a human-readable unit (matches Utils.hs). */
export const formatDuration = (ns: number): string => {
  if (ns == null || Number.isNaN(ns)) return 'N/A'; // guard first, mirroring formatNumber, so NaN never renders as "NaNns"
  const step = DURATION_STEPS.find(([t]) => ns >= t);
  return step ? `${(ns / step[0]).toFixed(1)}${step[1]}` : `${ns.toFixed(0)}ns`;
};

// Units that format as a duration. NB: 'm' here is minutes, not meters.
const DURATION_UNITS = new Set(['ns', 'μs', 'us', 'ms', 's', 'm', 'h']);

export type StatAggregates = { min: number; max: number; sum: number; count: number; mean: number };

/**
 * Pick the representative scalar for a timeseries_stat's big number. Summing
 * per-bin values only makes sense for additive counts; rates/percentages/
 * latencies must be averaged ('mean') or rate-derived ('rate', events/min over
 * the window), never summed. `from`/`to` are in ms.
 */
export const statScalar = (stats: Partial<StatAggregates>, summarizeBy: string, from?: number, to?: number): number => {
  switch (summarizeBy) {
    case 'rate': {
      const minutes = from != null && to != null && to > from ? (to - from) / 60000 : 0;
      return minutes > 0 ? Number(stats.sum) / minutes : NaN; // degenerate window → N/A, not a wrong-unit raw sum
    }
    case 'mean': return Number(stats.mean);
    case 'max': return Number(stats.max);
    case 'min': return Number(stats.min);
    case 'count': return Number(stats.count);
    default: return Number(stats.sum);
  }
};

/** Format a stat scalar for display, appending the unit (duration-aware). */
export const formatStatValue = (value: number, unit: string): string => {
  if (DURATION_UNITS.has(unit)) return formatDuration(convertToNanoseconds(value, unit));
  const suffix = ({ '': '', '1': '', '{}': '', By: ' bytes' } as Record<string, string>)[unit] ?? ` ${unit}`;
  return `${formatNumber(value)}${suffix}`;
};
