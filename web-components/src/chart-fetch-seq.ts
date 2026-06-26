// Per-chart request sequencer. A chart can have several /chart_data fetches in
// flight at once (5s refresh + intersection observer + time-range change), and
// they can resolve out of order. beginChartFetch bumps the chart's sequence and
// returns an isStale() predicate that becomes true once a *newer* fetch starts,
// so a slow/failed response can be discarded instead of clobbering a newer
// success — otherwise the error overlay reappears over freshly rendered bars.
// Keyed by chartId and never evicted; fine because chart IDs are stable for the
// page's lifetime (a fixed dashboard layout), not generated per render.
const chartFetchSeq: Record<string, number> = {};

export const beginChartFetch = (chartId: string): (() => boolean) => {
  const seq = (chartFetchSeq[chartId] = (chartFetchSeq[chartId] || 0) + 1);
  return () => chartFetchSeq[chartId] !== seq;
};
