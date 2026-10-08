'use strict';
import { getSeriesColor, invalidateLogLevelColors } from './colorMapping';
import { ChartQueryError, readChartResponse } from './chart-stream';
import { shouldReloadForStaleChunk } from './stale-chunk-reload';
import { beginChartFetch } from './chart-fetch-seq';
import { chartLayoutForSize, isNearChartViewport } from './chart-initialization';
import { formatNumber, formatBytes, convertToNanoseconds, formatDuration, statScalar, formatStatValue, type StatAggregates } from './stat-value';
import { echartsUrls } from './assets';
import { copyParams } from './time-range-utils';
import debounce from 'lodash/debounce';
const INITIAL_FETCH_INTERVAL = 5000;
const $ = (id: string) => document.getElementById(id);
const params = () => ({ ...Object.fromEntries(new URLSearchParams(location.search)) });

// --- ECharts loads only once a chart enters the viewport ---
let echartsLoad: Promise<void> | undefined;

const loadScript = (src: string) => new Promise<void>((resolve, reject) => {
  const script = document.createElement('script');
  script.src = src;
  script.onload = () => resolve();
  script.onerror = () => reject(new Error(`Failed to load ${src}`));
  document.head.append(script);
});

const ensureECharts = () => {
  if (window.echarts) return Promise.resolve();
  if (!echartsLoad) {
    const assets = echartsUrls();
    echartsLoad = loadScript(assets.echarts).then(() => loadScript(assets.theme));
  }
  return echartsLoad;
};

// --- Concurrency limiter for response bodies (max 4 in flight) ---
const MAX_CONCURRENT_FETCHES = 4;
let activeFetches = 0;
const fetchQueue: Array<() => void> = [];

const limitedFetch = <T>(url: string, consume: (response: Response) => Promise<T>, signal?: AbortSignal): Promise<T> => {
  return new Promise((resolve, reject) => {
    const run = () => {
      if (signal?.aborted) {
        reject(signal.reason);
        fetchQueue.shift()?.();
        return;
      }
      activeFetches++;
      // Accept marks this as a data request, so an expired session answers 401 JSON
      // instead of 302-ing to the login page — which fetch follows transparently,
      // handing us 200 HTML that res.json() then chokes on. See Web.Auth.challengeFor.
      fetch(url, { signal, headers: { Accept: 'application/x-ndjson, application/json' } }).then(consume).then(resolve, reject).finally(() => {
        activeFetches--;
        if (fetchQueue.length > 0) fetchQueue.shift()!();
      });
    };
    if (activeFetches < MAX_CONCURRENT_FETCHES) run();
    else fetchQueue.push(run);
  });
};

// --- Staggered chart initialization ---
// One ECharts instance is created per animation frame so visible charts stay responsive.
const initQueue: Array<{ fn: () => void; el: HTMLElement | null }> = [];
let initScheduled = false;

const processInitQueue = () => {
  if (initQueue.length > 0) {
    initQueue.shift()!.fn(); // 1 chart per frame
  }
  if (initQueue.length > 0) {
    requestAnimationFrame(processInitQueue);
  } else {
    initScheduled = false;
  }
};

const ensureScheduled = () => {
  if (!initScheduled && initQueue.length > 0) {
    initScheduled = true;
    requestAnimationFrame(processInitQueue);
  }
};

// Observe queued chart containers - when one scrolls into view, move it to front
const visibilityObserver = new IntersectionObserver((entries) => {
  for (const entry of entries) {
    if (!entry.isIntersecting) continue;
    const idx = initQueue.findIndex(item => item.el === entry.target);
    if (idx > 0) {
      const [item] = initQueue.splice(idx, 1);
      initQueue.unshift(item);
    }
    visibilityObserver.unobserve(entry.target);
  }
});

// Deferred init for off-screen charts — only init just before they enter the viewport.
const deferredInits = new Map<HTMLElement, () => void>();
const deferredInitObserver = new IntersectionObserver((entries) => {
  for (const entry of entries) {
    if (!entry.isIntersecting) continue;
    const fn = deferredInits.get(entry.target as HTMLElement);
    if (fn) {
      deferredInits.delete(entry.target as HTMLElement);
      initQueue.push({ fn, el: entry.target as HTMLElement });
      ensureScheduled();
    }
    deferredInitObserver.unobserve(entry.target);
  }
}, { rootMargin: '150px' });

const queueChartInit = (fn: () => void, chartId?: string) => {
  const el = chartId ? document.getElementById(chartId) : null;
  if (!el) {
    initQueue.push({ fn, el: null });
    ensureScheduled();
    return;
  }
  // Keep a small look-ahead window without instantiating the whole metric grid.
  const rect = el.getBoundingClientRect();
  if (isNearChartViewport(rect, window.innerHeight)) {
    void ensureECharts().then(() => {
      initQueue.push({ fn, el });
      visibilityObserver.observe(el);
      ensureScheduled();
    });
  } else {
    deferredInits.set(el, () => void ensureECharts().then(fn));
    deferredInitObserver.observe(el);
  }
};

// --- Shared ResizeObserver for all charts ---
const sharedResizeObserver = new ResizeObserver((entries) => {
  for (const entry of entries) {
    const el = entry.target as HTMLElement;
    if (el.id) queueChartResize(el.id);
  }
});

// --- Shared MutationObserver for theme changes ---
type ThemeCallback = (isDark: boolean, styles: Record<string, string>) => void;
const themeCallbacks: Set<ThemeCallback> = new Set();
let themeChangeScheduled = false;

const sharedThemeObserver = new MutationObserver((mutations) => {
  if (themeCallbacks.size === 0) return;
  const themeChanged = mutations.some((m) => m.type === 'attributes' && m.attributeName === 'data-theme');
  if (!themeChanged || themeChangeScheduled) return;
  themeChangeScheduled = true;
  requestAnimationFrame(() => {
    const isDarkMode = document.body.getAttribute('data-theme') === 'dark';
    const computedStyle = getComputedStyle(document.body);
    const styles = {
      textColor: computedStyle.getPropertyValue('--color-textWeak').trim(),
      tooltipBg: computedStyle.getPropertyValue('--color-bgRaised').trim(),
      tooltipTextColor: computedStyle.getPropertyValue('--color-textStrong').trim(),
      tooltipBorderColor: computedStyle.getPropertyValue('--color-borderWeak').trim(),
    };
    themeCallbacks.forEach(cb => cb(isDarkMode, styles));
    themeChangeScheduled = false;
  });
});
sharedThemeObserver.observe(document.body, { attributes: true, attributeFilter: ['data-theme'] });

// Subscribe to the shared theme observer from other modules; returns an unsubscribe.
export const subscribeChartTheme = (cb: ThemeCallback): (() => void) => {
  themeCallbacks.add(cb);
  return () => { themeCallbacks.delete(cb); };
};

// Convert any CSS color (including oklch) to hex for ECharts.
// Modern browsers keep oklch in fillStyle, so we render a pixel and read back RGB.
const _colorCanvas = document.createElement('canvas');
_colorCanvas.width = 1; _colorCanvas.height = 1;
const _colorCtx = _colorCanvas.getContext('2d', { willReadFrequently: true })!;
export const toEChartsColor = (cssColor: string): string => {
  if (!cssColor) return '';
  _colorCtx.clearRect(0, 0, 1, 1);
  _colorCtx.fillStyle = cssColor;
  _colorCtx.fillRect(0, 0, 1, 1);
  const [r, g, b, a] = _colorCtx.getImageData(0, 0, 1, 1).data;
  return a < 255
    ? `rgba(${r},${g},${b},${(a / 255).toFixed(2)})`
    : '#' + [r, g, b].map(v => v.toString(16).padStart(2, '0')).join('');
};

// Read chart-related CSS tokens from the design system. Cached per theme: getComputedStyle
// interleaved with echarts' DOM writes forced a full-page style recalc on every series of
// every render (~1/3 of the log explorer's chart startup). Reading the key forces nothing.
let chartStylesCache: { key: string; styles: ReturnType<typeof readChartStyles> } | undefined;
export const getChartStyles = () => {
  const key = `${document.body.getAttribute('data-theme')}:${matchMedia('(prefers-color-scheme: dark)').matches}`;
  if (chartStylesCache?.key !== key) chartStylesCache = { key, styles: readChartStyles() };
  return chartStylesCache.styles;
};
const readChartStyles = () => {
  const cs = getComputedStyle(document.body);
  const get = (prop: string) => toEChartsColor(cs.getPropertyValue(prop).trim());
  return {
    textColor: get('--color-textWeak'),
    tooltipBg: get('--color-bgRaised'),
    tooltipTextColor: get('--color-textStrong'),
    // --color-borderWeak has never existed; the token is strokeWeak. The empty string
    // this returned made echarts fall back to its own palette, which is why bordered chart
    // marks came out in rotating chart colours instead of the neutral stroke.
    tooltipBorderColor: get('--color-strokeWeak'),
    chartBg: get('--color-chartBg'),
    chartMask: get('--color-chartMask'),
    errorColor: get('--color-textError'),
    warningColor: get('--color-textWarning'),
    successColor: get('--color-fillSuccess-strong'),
    strokeStrong: get('--color-strokeStrong'),
    brandColor: get('--color-fillBrand-strong'),
    // Shading for a subject's own interval on a wider chart (applyHighlightBand).
    // The weak brand fill is already the token for "this region, not a value",
    // and carries its own alpha in both themes.
    highlightBandColor: get('--color-fillBrand-weak'),
  };
};

// A failed widget states so on one line at the top of its own card — the banner
// Widget.hs renders — rather than painting the message over the chart it is
// describing, which left both unreadable. Whatever the chart last drew stays
// visible underneath, which is usually the context you want while reading why
// the refresh failed. Only the legitimately-empty range keeps a centred overlay.
export const showChartError = (chartId: string, message: string, retry?: () => Promise<void>) => {
  hideNoDataOverlay(chartId);
  const msg = $(`${chartId}_errorMsg`);
  if (!msg) return;
  msg.textContent = message;
  msg.title = message;
  const button = document.getElementById(`${chartId}_retry`) as HTMLButtonElement | null;
  if (button) {
    button.hidden = !retry;
    button.onclick = retry ? async () => {
      button.disabled = true;
      try { await retry(); } finally { button.disabled = false; }
    } : null;
  }
  $(`${chartId}_error`)?.classList.remove('hidden');
};

const hideChartError = (chartId: string) => $(`${chartId}_error`)?.classList.add('hidden');

export const showNoDataOverlay = (chartId: string) => $(`${chartId}_empty`)?.classList.remove('hidden');

export const hideNoDataOverlay = (chartId: string) => $(`${chartId}_empty`)?.classList.add('hidden');


// Loader + spotlight border move together, and both writes are deferred to a frame so a
// fetch never forces layout in the middle of a scroll.
const setChartLoading = (chartId: string, on: boolean) =>
  requestAnimationFrame(() => {
    $(`${chartId}_loader`)?.classList.toggle('hidden', !on);
    $(`${chartId}_bordered`)?.classList.toggle('spotlight-border', on);
  });

// The parts of an echarts option that follow the theme rather than the data. Applied on
// construction and again on every theme change, so the two can't drift apart. Returns the
// styles it read, since both callers need more of them.
const applyChartTheme = (opt: Record<string, any>) => {
  const styles = getChartStyles();
  if (styles.textColor) {
    opt.legend = opt.legend || {};
    opt.legend.textStyle = { ...opt.legend.textStyle, color: styles.textColor };
  }
  opt.tooltip = {
    ...opt.tooltip,
    backgroundColor: styles.tooltipBg,
    textStyle: { ...opt.tooltip?.textStyle, color: styles.tooltipTextColor },
    borderColor: styles.tooltipBorderColor,
    borderWidth: 1,
  };
  opt.backgroundColor = 'transparent';
  return styles;
};

/** One cell of a chart dataset row: the bin timestamp, then one value per series. */
type ChartCell = string | number | null;
type ChartRow = ChartCell[];

/** The /chart_data response body. */
type ChartDataResponse = {
  from?: number;
  to?: number;
  headers?: string[];
  dataset?: ChartRow[];
  rows_per_min?: number;
  stats?: (Partial<StatAggregates> & { max_group_sum?: number }) | null;
  error?: string;
};

const MAX_VISIBLE_SERIES = 8;

// Keep dense charts legible without changing their totals: rank series by peak value,
// retain the leaders, and fold the rest into one explicitly labelled series.
export const collapseLongTail = (data: ChartRow[]): ChartRow[] => {
  if (!Array.isArray(data) || !Array.isArray(data[0])) return data;
  const [headers, ...rows] = data;
  const names = headers.slice(1) as string[];
  if (names.length <= MAX_VISIBLE_SERIES) return data;

  const ranked = names
    .map((name: string, index: number) => ({
      name,
      index,
      peak: Math.max(...rows.map((row) => (Number.isFinite(row?.[index + 1]) ? (row[index + 1] as number) : -Infinity))),
    }))
    .sort((a, b) => b.peak - a.peak || a.index - b.index);
  const visible = ranked.slice(0, MAX_VISIBLE_SERIES);
  const hidden = ranked.slice(MAX_VISIBLE_SERIES);

  return [
    [headers[0], ...visible.map((series) => series.name), `Other (${hidden.length})`],
    ...rows.map((row) => {
      if (!Array.isArray(row)) return row;
      const hiddenValues = hidden.map((series) => row[series.index + 1]).filter(Number.isFinite) as number[];
      return [row[0], ...visible.map((series) => row[series.index + 1]), hiddenValues.length ? hiddenValues.reduce((sum, value) => sum + value, 0) : null];
    }),
  ];
};

export const createSeriesConfig = (widgetData: WidGetData, name: string, i: number) => {
  // A thresholded stat (e.g. Error Rate) is treated as a caution metric — color
  // it red so the signal isn't an arbitrary hash color, rather than leaving it to
  // getSeriesColor's hash. Assumes higher = worse; revisit (explicit intent field)
  // if a "good when high" or latency-SLO stat ever needs a non-red threshold.
  // Otherwise generic stat columns use the brand color; named series use their mapping.
  const isErrorStat = widgetData.widgetType === 'timeseries_stat' && widgetData.alertThreshold != null;
  // A lone aggregate column has no identity to encode, whatever the widget type.
  // getSeriesColor hashes the series *name*, which is what gives a service a stable
  // colour everywhere — but hashing the literal string "count(*)" yields an arbitrary
  // hue that reads as signal when it carries none. It rendered healthy checkout
  // throughput in the same red as an error-volume chart, on a page whose first design
  // principle is "colour is signal, not decoration". Grouped series are named by their
  // group value (service, status, ...) and still hash, which is the case the hash is for.
  const isGenericAggregate = name === 'value' || name.startsWith('count') || !name;
  // The page can say what a bare aggregate means when its name cannot (error volume vs
  // healthy throughput are both "count(*)"). Explicit intent, never a hash.
  const isErrorSeries = isGenericAggregate && widgetData.seriesIntent === 'error';
  const styles = getChartStyles(); // one getComputedStyle read per series, not three
  const paletteColor = isErrorStat || isErrorSeries ? styles.errorColor : isGenericAggregate ? styles.brandColor : getSeriesColor(name);

  const gradientColor = (opacity: number) => new window.echarts.graphic.LinearGradient(0, 0, 0, 1, [
    { offset: 0, color: window.echarts.color.modifyAlpha(paletteColor, opacity) },
    { offset: 1, color: window.echarts.color.modifyAlpha(paletteColor, 0) },
  ]);

  const backgroundStyle = { color: styles.chartBg };

  const inventorySeriesOpacity = widgetData.chartId?.startsWith('containers-') && i > 0 ? 0.72 : 1;
  const seriesOpt: any = {
    type: widgetData.chartType,
    name,
    stack: widgetData.chartType === 'line' ? undefined : widgetData.yAxisLabel || 'units',
    showSymbol: false,
    showBackground: true,
    backgroundStyle,
    barMaxWidth: '10',
    barMinHeight: '1',
    encode: { x: 0, y: i + 1 },
    itemStyle: { color: paletteColor, opacity: inventorySeriesOpacity },
  };

  // For line charts, also set lineStyle color
  if (widgetData.chartType === 'line') {
    const inventoryLineType = widgetData.chartId?.startsWith('containers-') ? ['solid', 'dashed', 'dotted'][i % 3] : 'solid';
    seriesOpt.lineStyle = { color: paletteColor, opacity: inventorySeriesOpacity, width: inventorySeriesOpacity === 1 ? 2.25 : 1.5, type: inventoryLineType };
    if (i === 0) seriesOpt.areaStyle = { color: gradientColor(0.24) };
    // For line charts in dark mode, override the symbol to avoid white centers
    if (document.body.getAttribute('data-theme') === 'dark') {
      seriesOpt.symbol = 'circle'; // Use filled circle instead of empty circle
      seriesOpt.symbolSize = 4;
    }
  }

  if (widgetData.widgetType == 'timeseries_stat') {
    seriesOpt.itemStyle = { color: gradientColor(1) };
    seriesOpt.areaStyle = { color: gradientColor(1) };
  }

  return seriesOpt;
};

const updateChartConfiguration = (widgetData: WidGetData, opt: any, data: any) => {
  if (!data) return opt;

  const source = collapseLongTail(data);
  opt.dataset = { ...opt.dataset, source };
  // Counts must not get fractional ticks (0.2, 0.6) when the peak is small.
  if (opt.yAxis && !Array.isArray(opt.yAxis))
    opt.yAxis.minInterval = source.slice(1).every((r: ChartCell[]) => r.slice(1).every((v) => v == null || Number.isInteger(v))) ? 1 : undefined;

  // Avoid unnecessary updates if data structure hasn't changed
  const cols = source[0]?.slice(1);
  const currentLegendData = opt.legend?.data;

  // Only update if legend data has actually changed
  if (JSON.stringify(cols) !== JSON.stringify(currentLegendData)) {
    opt.series = cols?.map((n: ChartCell, i: number) => createSeriesConfig(widgetData, String(n ?? ''), i));
    opt.legend.data = cols;
  }

  opt.series?.forEach((series: any, i: number) => {
    if (series.type === 'line') {
      const first = source.findIndex((row: ChartRow, index: number) => index > 0 && Number.isFinite(row[i + 1]));
      series.showSymbol = first !== -1 && !source.some((row: ChartRow, index: number) => index > first && Number.isFinite(row[i + 1]));
    }
  });

  // Merge threshold markLines into first series to avoid a second setOption call
  const thresholds: Record<string, number> = {};
  if (widgetData.alertThreshold != null && Number.isFinite(widgetData.alertThreshold)) thresholds.alert = widgetData.alertThreshold;
  if (widgetData.warningThreshold != null && Number.isFinite(widgetData.warningThreshold)) thresholds.warning = widgetData.warningThreshold;
  if (Object.keys(thresholds).length > 0 && opt.series?.length) {
    opt.series[0].markLine = { silent: true, symbol: 'none', data: createThresholdMarkLines(thresholds, widgetData.unit ?? '') };
    // ECharts excludes markLines from automatic extents. Include thresholds in
    // the data-derived bounds, including streamed updates, with room for labels.
    const values = Object.values(thresholds);
    const bounds = ({ min, max }: { min: number; max: number }) => {
      // ECharts passes ±Infinity extents for an empty dataset; NaN bounds break the axis.
      const low = Math.min(0, Number.isFinite(min) ? min : 0, ...values);
      const high = Math.max(Number.isFinite(max) ? max : 0, ...values);
      const padding = (high - low || 1) * 0.05;
      return { min: low < 0 ? low - padding : 0, max: high + padding };
    };
    opt.yAxis.min = (data: { min: number; max: number }) => bounds(data).min;
    opt.yAxis.max = (data: { min: number; max: number }) => bounds(data).max;
  }

  // Release markers ride in the option itself: a data response replaces the whole option (notMerge).
  const marks = (widgetData.markers ?? []).map(m => ({ xAxis: new Date(m.at).getTime(), name: m.label })).filter(d => Number.isFinite(d.xAxis));
  opt.series = (opt.series ?? []).filter((s: any) => s.id !== '__markers');
  if (marks.length) {
    const markLine = { silent: true, symbol: 'none', lineStyle: { color: getChartStyles().strokeStrong, type: 'dotted', width: 1 }, label: { formatter: '{b}', position: 'insideEndTop', fontSize: 10 }, data: marks };
    opt.series.push({ id: '__markers', type: 'line', data: [], silent: true, animation: false, markLine });
  }

  return opt;
};

// Fill a timeseries_stat's big number from the chart-data fetch: the
// representative scalar for its unit, or a no-data dash (stats null / empty
// range) so the loading spinner always resolves rather than spinning under an
// error overlay. Eager stat widgets keep their server-rendered value (no fetch).
const NO_DATA_VALUE = '—';
const setStatValue = (widgetData: WidGetData, stats: ChartDataResponse['stats'], from?: number, to?: number) => {
  const value = $(`${widgetData.chartId}Value`);
  if (!value) return;
  if (widgetData.hideValue) {
    value.textContent = '';
    value.classList.add('hidden');
    return;
  }
  if (stats == null) {
    // Fetch error: clear the spinner, but leave the chart's error overlay as the
    // sole failure signal rather than revealing a redundant "—" badge above it.
    value.textContent = '';
    return;
  }
  // textContent (not innerHTML): values are plain text, and the max/min prefix is a literal "<"/">".
  if ((stats.count ?? 0) > 0) {
    const formatted = formatStatValue(statScalar(stats, widgetData.summarizeBy, from, to), widgetData.unit || '');
    value.textContent = widgetData.summarizeByPrefix ? `${widgetData.summarizeByPrefix} ${formatted}` : formatted;
  } else {
    value.textContent = NO_DATA_VALUE; // empty range
  }
  value.classList.remove('hidden');
};

// The /chart_data URL for a widget. Shared by the initial prefetch and every later
// refetch, so the two can't drift — the prefetch is only honoured when the URL it was
// issued against still matches (see takePrefetched).
export const chartDataUrl = ({
  chartId,
  query,
  querySQL,
  rollupSQL,
  rollupFrom,
  pid,
  chartType,
  dbSource,
  timeFrom,
  timeTo,
  dashboardId,
}: Pick<WidGetData, 'chartId' | 'query' | 'querySQL' | 'rollupSQL' | 'rollupFrom' | 'pid' | 'chartType' | 'dbSource' | 'timeFrom' | 'timeTo' | 'dashboardId'>): string => {
  const params = new URLSearchParams(window.location.search);
  params.set('pid', pid);
  if (dashboardId) params.set('dashboard_id', dashboardId);
  // A widget carrying its own window is about that window, not the page's. `since`
  // has to go with them: the server prefers it over from/to, so leaving it behind
  // would silently widen the request back to the page range.
  if (timeFrom && timeTo) {
    params.delete('since');
    params.set('from', timeFrom);
    params.set('to', timeTo);
  } else if (!['since', 'from', 'to'].some((key) => params.get(key))) {
    // Infrastructure pages resolve an absent range to their local default server-side.
    // Carry that same default into /chart_data instead of falling back to its one-hour range.
    const defaultWindow = document.querySelector<HTMLElement>('[data-default-window]')?.dataset.defaultWindow;
    if (defaultWindow) params.set('since', defaultWindow);
  }
  // Lets the server size bin_auto buckets for how this widget renders: a line
  // chart carries twice the points a bar chart can show legibly.
  if (chartType) params.set('chart_type', chartType);
  // SQL widgets can explicitly target Postgres. Leaving this out makes the
  // server select TimeFusion by default, where application rollup tables do
  // not exist.
  if (dbSource) params.set('db_source', dbSource);

  // The swapped panel's resolved constants supersede values carried by older links.
  const constants = window.getDashboardConstants?.(document.getElementById(chartId)) ?? {};
  Object.entries(constants).forEach(([key, value]) => {
    params.set(key, value as string);
  });

  // Default query to use when no query is provided
  const DEFAULT_QUERY = 'summarize count(*) by bin_auto(timestamp)';

  if (!query || query === 'null' || query === '') {
    params.set('query', DEFAULT_QUERY);
  } else {
    // Every production chart config comes from Widget.renderChart, which shapes its query.
    // Preserve that aggregation when constructing refresh, prefetch, and retry URLs.
    const hasSummarize = /summarize\s+/i.test(query);
    params.set('query', hasSummarize ? query : query + ' | ' + DEFAULT_QUERY);
  }

  if (querySQL && querySQL !== 'null') params.set('query_sql', querySQL);
  if (rollupSQL && rollupFrom) {
    params.set('rollup_sql', rollupSQL);
    params.set('rollup_from', rollupFrom);
  }

  return `/chart_data?${params}`;
};

// A widget whose data the server computed and embedded has no project to query — RUM's
// Web Vitals trends are one (Pages.RealUserMonitoring.vitalTrendPanel_). Fetching for one
// asks /chart_data for a default `count(*)` against `pid=null`, which answers 401, and a
// 401 on a chart fetch reloads the page: the 15s live tick was rebuilding the entire page
// — skeletons and all — instead of updating data in place.
const canFetchData = (widgetData: WidGetData) => !!widgetData.pid && widgetData.pid !== 'null';

// A widget's first request used to wait on echarts loading, the stagger queue, chart
// construction and an IntersectionObserver — on the log explorer that put it ~2.8s into
// the page. Nothing about the request depends on any of that, so it starts as soon as this
// module evaluates and the chart picks up the in-flight promise when it's ready.
type ChartPrefetch = { url: string; response: Promise<ChartDataResponse | null>; controller: AbortController; latest?: ChartDataResponse; partial?: (data: ChartDataResponse) => void };
const chartDataPrefetch = new Map<string, ChartPrefetch>();
const streamUrl = (url: string) => url.replace('/chart_data?', '/chart_data/stream?');
class ChartSessionError extends Error {}
// /chart_data intermittently answers 401 while the page's session is fine (server-side
// lookup, cause unknown); reloading on each one looped the page every ~2s. One re-fetch
// after a second confirms it, shared by every chart that hit it at the same time.
let sessionCheck: Promise<boolean> | undefined;
const sessionExpired = (url: string) => (sessionCheck ??= new Promise((resolve) => setTimeout(resolve, 1000))
  .then(() => fetch(url, { headers: { Accept: 'application/json' } }).then((res) => res.status === 401, () => false))
  .finally(() => { sessionCheck = undefined; }));

const checkChartResponse = async (res: Response) => {
  if (res.status === 401) {
    if (await sessionExpired(res.url) && shouldReloadForStaleChunk(Date.now(), sessionStorage, 60_000, 'monoscope:session-reload')) window.location.reload();
    throw new ChartSessionError('Session expired. Retry reloads the page.');
  }
  if (!res.ok) throw new Error(`widget request failed: ${res.status} ${res.statusText}`);
  return res;
};

export const prefetchChartData = (widgetData: WidGetData) => {
  const { chartId } = widgetData;
  if (!chartId || !canFetchData(widgetData) || chartDataPrefetch.has(chartId)) return;
  // Same viewport gate as queueChartInit. Off-screen widgets deliberately don't fetch
  // until scrolled to, so prefetching every one would turn a 40-widget dashboard into
  // 40 requests on load. Absent element (not yet in the DOM) counts as near.
  const el = $(chartId);
  if (el && !isNearChartViewport(el.getBoundingClientRect(), window.innerHeight)) return;
  const url = chartDataUrl(widgetData);
  // A failed prefetch resolves to null so the consumer falls back to a live fetch and
  // surfaces the real error, rather than inheriting a rejection nobody is awaiting yet.
  const entry: ChartPrefetch = { url, response: Promise.resolve(null), controller: new AbortController() };
  entry.response = limitedFetch(streamUrl(url), res => checkChartResponse(res).then(res => readChartResponse<ChartDataResponse>(res, data => {
    entry.latest = data;
    entry.partial?.(data);
  })), entry.controller.signal).catch(() => null);
  chartDataPrefetch.set(chartId, entry);
};

// Single-use, and only for the URL it was issued against: if the query changed between
// prefetch and first render, the stale body must not be adopted.
const takePrefetched = async (chartId: string, url: string, partial: (data: ChartDataResponse) => void, signal: AbortSignal): Promise<ChartDataResponse | null> => {
  const entry = chartDataPrefetch.get(chartId);
  if (!entry) return null;
  chartDataPrefetch.delete(chartId);
  if (entry.url !== url) { entry.controller.abort(); return null; }
  const abort = () => entry.controller.abort();
  signal.addEventListener('abort', abort, { once: true });
  entry.partial = partial;
  if (entry.latest) partial(entry.latest);
  try { return await entry.response; }
  finally { signal.removeEventListener('abort', abort); }
};

// `showLoader` is false for a refresh nobody asked for. A timer tick that swaps the loader in
// over a chart that is already drawn reads as the page breaking, several times a minute, and it
// tells the reader nothing they need — the previous rendering stays correct right up until the
// new data replaces it. Loaders belong to the initial load and to explicit user actions.
const chartRefreshState = new WeakMap<object, { url: string; hasData: boolean; failures: number; retryAt: number }>();
const BACKGROUND_FAILURE_THRESHOLD = 3;
// A failing query (a TimeFusion timeout costs 90s each) must not be re-issued on every live tick.
const BACKOFF_BASE_MS = 15_000;
const BACKOFF_MAX_MS = 600_000;
const chartRequests = new WeakMap<object, AbortController>();
const PARTIAL_DRAW_DELAY_MS = 300;

// Every response snapshot uses the same scale, dimensions and series mapping.
// A stream's partial frames often repeat the final answer exactly; each full rebuild costs
// ~100ms of main thread at 4x CPU, so an unchanged snapshot is not redrawn.
const drawnResponse = new WeakMap<object, string>();
// Whether the drawn option is chartLayoutForSize's compact variant; every draw records it so a
// resize only rebuilds the chart when compactness actually flips.
const compactLayouts = new WeakMap<object, boolean>();
const applyChartResponse = (chart: any, opt: any, widgetData: WidGetData, data: ChartDataResponse) => {
  const el = $(widgetData.chartId);
  const width = el?.clientWidth ?? 0, height = el?.clientHeight ?? 0;
  const key = JSON.stringify([width, height, data.from, data.to, data.headers, data.stats, data.dataset]);
  if (drawnResponse.get(chart) === key) return;
  drawnResponse.set(chart, key);
  const headers = data.headers?.map(h => h === 'timestamp' || h === 'created_at'
    ? h : h.substring(0, 75) + (h.length > 75 ? '...' : ''));
  opt.xAxis = { ...opt.xAxis, min: data.from, max: data.to };
  opt.dataset.source = [headers || [], ...(data.dataset || [])];
  const maximum = widgetData.chartType === 'line' ? data.stats?.max : data.stats?.max_group_sum;
  opt.yAxis = { ...opt.yAxis, max: maximum != null && Number.isFinite(maximum) && maximum > 0 ? maximum : undefined };
  const configured = updateChartConfiguration(widgetData, opt, opt.dataset.source);
  const layout = chartLayoutForSize(configured, width, height);
  chart.setOption(layout, true);
  compactLayouts.set(chart, layout !== configured);
};

const updateChartData = async (chart: any, opt: any, shouldFetch: boolean, widgetData: WidGetData, lifetimeSignal: AbortSignal, showLoader = true) => {
  if (!shouldFetch || !canFetchData(widgetData) || lifetimeSignal.aborted) return;
  const { chartId } = widgetData;
  const url = chartDataUrl(widgetData);
  let state = chartRefreshState.get(chart);
  if (!state || state.url !== url) {
    state = { url, hasData: false, failures: 0, retryAt: 0 };
    chartRefreshState.set(chart, state);
  }
  // A timer must not replace an unfinished stream with a blocking refresh, nor retry before the backoff.
  if (!showLoader && (chartRequests.has(chart) || Date.now() < state.retryAt)) return;
  chartRequests.get(chart)?.abort();
  const request = new AbortController();
  chartRequests.set(chart, request);
  const signal = request.signal;
  const abort = () => request.abort();
  lifetimeSignal.addEventListener('abort', abort, { once: true });

  const isStale = beginChartFetch(chartId);
  let receivedPartial = false;
  const subtitle = $(`${chartId}Subtitle`);
  const retry = () => updateChartData(chart, opt, true, widgetData, lifetimeSignal);
  const reportFailure = (message: string, action = retry) => {
    state.failures++;
    state.retryAt = Date.now() + Math.min(BACKOFF_MAX_MS, BACKOFF_BASE_MS * 2 ** (state.failures - 1));
    chart.hideLoading();
    if (!showLoader && state.hasData) {
      if (state.failures >= BACKGROUND_FAILURE_THRESHOLD) {
        showChartError(chartId, 'Updates delayed. Showing last successful data.', action);
      }
      return;
    }
    if (receivedPartial) {
      message = `Incomplete results. ${message}`;
      if (subtitle) subtitle.textContent = 'Incomplete results';
    } else if (showLoader && subtitle) subtitle.textContent = '';
    showChartError(chartId, message, action);
    if (!state.hasData) setStatValue(widgetData, null);
  };
  // Batch DOM updates before fetch. The stat value rendered by the server (or the previous
  // fetch) stays up until the response replaces it; blanking it read as the tile breaking.
  if (showLoader) {
    hideChartError(chartId);
    if (subtitle) subtitle.textContent = 'Loading…';
    $(chartId)?.removeAttribute('data-chart-partial');
    $(chartId)?.setAttribute('aria-busy', 'true');
    setChartLoading(chartId, true);
  }

  // The first partial draws at once; later ones only when the final answer is slow. Each is
  // a full ~100ms rebuild at 4x CPU, and a fast stream would otherwise draw three times.
  let partialTimer: ReturnType<typeof setTimeout> | undefined;
  try {
    const drawPartial = (data: ChartDataResponse) => {
      if (signal.aborted || isStale()) return;
      if (subtitle) subtitle.textContent = receivedPartial ? 'Loading partial results…' : 'Loading…';
      if (data.dataset?.length) chart.hideLoading();
      hideNoDataOverlay(chartId);
      applyChartResponse(chart, opt, widgetData, data);
      // Keep incomplete totals out of stat tiles. The loading state distinguishes
      // a partial chart from an empty or complete result.
      const element = $(chartId);
      element?.setAttribute('aria-busy', 'true');
      element?.setAttribute('data-chart-partial', 'true');
    };
    const partial = (data: ChartDataResponse) => {
      if (signal.aborted || isStale() || !showLoader) return;
      const first = !receivedPartial;
      receivedPartial = !!data.dataset?.length;
      clearTimeout(partialTimer);
      if (first) drawPartial(data);
      else partialTimer = setTimeout(() => drawPartial(data), PARTIAL_DRAW_DELAY_MS);
    };
    const data: ChartDataResponse =
      (await takePrefetched(chartId, url, partial, signal)) ??
      (await limitedFetch(showLoader ? streamUrl(url) : url, res => checkChartResponse(res).then(res => readChartResponse<ChartDataResponse>(res, partial)), signal));
    clearTimeout(partialTimer);
    const { from, to, dataset, rows_per_min, stats, error } = data;
    if (signal.aborted || isStale()) return; // a newer fetch already won; don't overwrite its state
    if (error) {
      // Server-reported SQL failure: the error banner, not the "no data" overlay,
      // so the user can distinguish a broken widget from an empty range.
      reportFailure(error);
      return;
    }
    if (subtitle) subtitle.textContent = rows_per_min == null ? '' : `${window.formatNumber(rows_per_min)}/min`;

    // Representative scalar for the unit (rate/mean/…), not a blind sum of
    // per-bin values — see statScalar. from/to are ms, needed for rate. count<1
    // (empty range) renders the no-data dash, matching the chart overlay below.
    setStatValue(widgetData, stats, from, to);

    chart.hideLoading();
    hideChartError(chartId); // this refresh succeeded; clear the previous failure
    if (!dataset || dataset.length === 0) {
      showNoDataOverlay(chartId);
    } else {
      hideNoDataOverlay(chartId);
    }
    applyChartResponse(chart, opt, widgetData, data);
    $(chartId)?.removeAttribute('data-chart-partial');
    $(chartId)?.setAttribute('aria-busy', 'false');
    state.hasData = !!dataset?.length;
    state.failures = 0;
    state.retryAt = 0;
    if ((window as any).barChart) {
      (window as any).barChart.dispatchAction({
        type: 'takeGlobalCursor',
        key: 'dataZoomSelect',
        dataZoomSelectActive: true,
      });
    }
    window.dispatchEvent(new CustomEvent('chart-updated', { detail: { chartId, total: sumTimeseriesValues(dataset) } }));
  } catch (e) {
    clearTimeout(partialTimer);
    if (signal.aborted || isStale()) return;
    console.error('Failed to fetch new data:', e);
    if (e instanceof ChartSessionError) reportFailure(e.message, async () => window.location.reload());
    else reportFailure(e instanceof ChartQueryError ? e.message : "Couldn't load this chart. Retry to fetch it again.");
  } finally {
    lifetimeSignal.removeEventListener('abort', abort);
    if (chartRequests.get(chart) === request) chartRequests.delete(chart);
    if (!signal.aborted && !isStale()) {
      setChartLoading(chartId, false);
      $(chartId)?.setAttribute('aria-busy', 'false');
    }
  }
};

// A chart-data timeseries is [timestamp, seriesA, seriesB, ...]. Summing every
// numeric series cell gives the event total already fetched for the chart,
// without issuing a second count query from Log Explorer.
export const sumTimeseriesValues = (dataset: unknown): number | null => {
  if (!Array.isArray(dataset)) return null;
  let total = 0;
  for (const row of dataset) {
    if (!Array.isArray(row)) continue;
    for (const value of row.slice(1)) {
      if (typeof value === 'number' && Number.isFinite(value)) total += value;
    }
  }
  return total;
};

declare global {
  interface Window {
    formatNumber: (num: number | null | undefined) => string;
    formatBytes: (num: number | null | undefined) => string;
    convertToNanoseconds: (value: number, unit: string) => number;
    formatDuration: (ns: number) => string;
    setVariable: (key: string, value: string) => void;
    getVariable: (key: string) => string;
    // Installed by the dashboard page; supplies the dashboard's constant substitutions.
    getDashboardConstants?: (el?: Element | null) => Record<string, string>;
  }
}

type WidGetData = {
  chartType: string;
  opt: Record<string, any>;
  chartId: string;
  query: string;
  sql: string;
  querySQL: string;
  rollupSQL?: string | null;
  rollupFrom?: string | null;
  dbSource?: string | null;
  theme: string;
  yAxisLabel: string;
  pid: string;
  summarizeBy: string;
  summarizeByPrefix: string;
  widgetType: string;
  queryAST: string;
  legendPosition?: string;
  unit?: string;
  alertThreshold?: number | null;
  seriesIntent?: string | null;
  warningThreshold?: number | null;
  hideValue?: boolean;
  // Pins the widget's own query window instead of inheriting the page's, and shades
  // the subject's extent inside it. Both are ISO-8601; absent on ordinary widgets.
  timeFrom?: string | null;
  timeTo?: string | null;
  highlightFrom?: string | null;
  highlightTo?: string | null;
  // Labelled instants (e.g. releases) drawn as vertical lines; `at` is ISO-8601.
  markers?: { label: string; at: string }[] | null;
  dashboardId?: string | null;
};

/**
 * Shade the subject's own interval on a chart whose window is wider than it.
 *
 * This is the detail every vendor surveyed treats as the point of showing the chart
 * at all: a span's metrics are only meaningful once you can see whether the span sat
 * inside the spike or merely near it. The band is drawn on a throwaway series rather
 * than on series[0] so it survives a data refresh replacing the real series, and it
 * is non-interactive so it never steals a tooltip from the data.
 */
const applyHighlightBand = (chart: any, { highlightFrom, highlightTo, timeFrom, timeTo }: WidGetData) => {
  if (!highlightFrom || !highlightTo) return;
  const from = new Date(highlightFrom).getTime();
  const to = new Date(highlightTo).getTime();
  if (!Number.isFinite(from) || !Number.isFinite(to)) return;

  // Most spans are milliseconds inside a window of minutes, where a band drawn to
  // scale is narrower than a pixel — an overlay nobody can see is the same as no
  // overlay. Below that threshold say *where* rather than *how long*: a line marks
  // the instant honestly, whereas widening the band to be visible would overstate
  // the duration, which is the one thing the reader is here to judge.
  const windowMs =
    timeFrom && timeTo ? new Date(timeTo).getTime() - new Date(timeFrom).getTime() : NaN;
  const tooNarrow = Number.isFinite(windowMs) && windowMs > 0 && (to - from) / windowMs < 0.01;
  const styles = getChartStyles();
  const color = styles.highlightBandColor || 'rgba(99,102,241,0.12)';
  const mark = tooNarrow
    ? {
        markLine: {
          silent: true,
          symbol: 'none',
          lineStyle: { color: styles.brandColor, type: 'dashed', width: 1 },
          label: { show: false },
          data: [{ xAxis: from }],
        },
      }
    : {
        markArea: {
          silent: true,
          itemStyle: { color },
          data: [[{ xAxis: from }, { xAxis: to }]],
        },
      };
  chart.setOption(
    { series: [{ id: '__highlight', type: 'line', data: [], silent: true, animation: false, ...mark }] },
    { replaceMerge: [] },
  );
};

type Exemplar = { trace_id: string; timestamp: string; value: number; metric_name: string; url: string };

/**
 * Grafana's exemplar diamonds: one marker per representative trace, drawn over the
 * series at (the exemplar's own timestamp, its value). Clicking one opens the trace
 * it was recorded in.
 *
 * The exemplar timestamp is deliberately not the metric row's — a cumulative
 * histogram re-exports a weeks-old exemplar for every bucket it has not hit since —
 * so a marker can legitimately land outside the fetched series. The server already
 * filters to the requested window; anything left is real.
 */
const attachExemplars = async (chart: any, url: string, signal: AbortSignal) => {
  const qs = new URLSearchParams();
  copyParams(new URLSearchParams(location.search), qs);
  const res = await fetch(qs.size ? `${url}?${qs}` : url, { headers: { Accept: 'application/json' }, signal });
  if (!res.ok) return;
  const exemplars: Exemplar[] = await res.json();
  if (!exemplars.length || chart.isDisposed()) return;

  const series = (chart.getOption()?.series ?? []).slice();
  series.push({
    type: 'scatter',
    name: 'Exemplars',
    symbol: 'diamond',
    symbolSize: 9,
    z: 10,
    itemStyle: { color: getChartStyles().brandColor, borderColor: '#fff', borderWidth: 1 },
    data: exemplars.map((e) => [new Date(e.timestamp).getTime(), e.value, e]),
    tooltip: {
      formatter: (p: { data: [number, number, Exemplar] }) =>
        `<b>${p.data[2].metric_name}</b><br/>${p.data[2].value}<br/><span style="font-family:monospace">${p.data[2].trace_id}</span><br/>Click to open the trace`,
    },
  });
  chart.setOption({ series }, false);
  chart.on('click', (p: { seriesName?: string; data?: [number, number, Exemplar] }) =>
    p.seriesName === 'Exemplars' && p.data?.[2]?.url ? (window.location.href = p.data[2].url) : undefined
  );
};

const chartDisposers = new Map<string, () => void>();
const chartUpdaters = new Map<string, (widgetData: WidGetData) => void>();
const chartLayoutUpdaters = new Map<string, () => void>();
const DISPOSABLE_CHARTS = '[data-chart-widget], [data-service-map]';

const disposeChart = (chartId: string) => {
  chartDataPrefetch.get(chartId)?.controller.abort();
  chartDataPrefetch.delete(chartId);
  const dispose = chartDisposers.get(chartId);
  if (!dispose) return;
  chartDisposers.delete(chartId);
  dispose();
};

// Registers (and takes over) teardown for a chart container id. Any previously
// registered disposer for the id runs first, so re-rendering is idempotent.
export const registerChartDisposer = (chartId: string, dispose: () => void) => {
  chartDisposers.get(chartId)?.();
  chartDisposers.set(chartId, dispose);
};

const disposeChartsIn = (root: Element) => {
  const charts = root.matches(DISPOSABLE_CHARTS) ? [root] : [...root.querySelectorAll(DISPOSABLE_CHARTS)];
  charts.forEach((chart) => disposeChart((chart as HTMLElement).id));
};

// Use the actual swap tasks, including OOB targets. The compatibility shim sets
// detail.target to the source link, which need not contain the outgoing charts.
document.addEventListener('htmx:before:swap', (event) => {
  const e = event as CustomEvent<{
    tasks?: Array<{ target?: unknown; swapSpec?: { style?: string } }>;
    target?: unknown;
    ctx?: { target?: unknown };
  }>;
  const tasks = e.detail?.tasks ?? [{ target: e.detail?.ctx?.target ?? e.detail?.target ?? e.target }];
  for (const task of tasks) {
    if (['none', 'beforebegin', 'afterbegin', 'beforeend', 'afterend'].includes(task.swapSpec?.style ?? '')) continue;
    const target = typeof task.target === 'string' ? document.querySelector(task.target) : task.target;
    if (!(target instanceof Element)) continue;
    // A morph keeps the chart element (hx-morph-skip in the server markup); chartWidget then updates it in place.
    if (!/morph/i.test(task.swapSpec?.style ?? '')) disposeChartsIn(target);
  }
});

// Morph navigation can replace the target without exposing it in before:swap, and a morph
// keeps live charts whose element then leaves with the fragment. Sweep only registrations
// whose container is now gone; same-id replacements are taken over by chartWidget. Both
// events: an outerMorph settles without ever firing after:swap.
for (const event of ['htmx:after:swap', 'htmx:after:settle']) document.addEventListener(event, () => {
  [...new Set([...chartDisposers.keys(), ...chartDataPrefetch.keys()])].forEach((chartId) => {
    if (!document.getElementById(chartId)?.matches(DISPOSABLE_CHARTS)) disposeChart(chartId);
  });
  initializeChartWidgets();
});

// Global resize queue to batch chart resize operations
const resizeQueue = new Set<string>();
let resizeFrameScheduled = false;

const processResizeQueue = () => {
  if (resizeQueue.size === 0) {
    resizeFrameScheduled = false;
    return;
  }

  // Process all queued resizes in a single frame
  resizeQueue.forEach((chartId) => {
    const chartEl = $(chartId);
    if (chartEl) {
      // A page may observe an element without ever loading echarts — the service map is DOM,
      // not canvas — and a missing global must not throw for every other chart in the queue.
      const chart = window.echarts?.getInstanceByDom(chartEl);
      if (chart && !chart.isDisposed()) {
        chartLayoutUpdaters.get(chartId)?.();
        chart.resize();
      }
    }
  });

  resizeQueue.clear();
  resizeFrameScheduled = false;
};

const queueChartResize = (chartId: string) => {
  resizeQueue.add(chartId);

  if (!resizeFrameScheduled) {
    resizeFrameScheduled = true;
    requestAnimationFrame(processResizeQueue);
  }
};

export const chartWidget = (widgetData: WidGetData) => {
  const { chartType, chartId } = widgetData,
    chartEl = $(chartId),
    liveStreamCheckbox = $('streamLiveData') as HTMLInputElement;
  const settling = chartEl?.closest('.htmx-settling');
  if (settling) {
    settling.addEventListener('htmx:after:settle', () => chartWidget(widgetData), { once: true });
    return;
  }
  // A re-rendered widget whose chart element survived (morph swap) keeps its instance,
  // listeners and observers; only the option changes. A replaced element has no instance.
  const existingChart = window.echarts.getInstanceByDom(chartEl);
  const update = chartUpdaters.get(chartId);
  if (existingChart && !existingChart.isDisposed() && update) return update(widgetData);

  let intervalId: NodeJS.Timeout | null = null;
  const controller = new AbortController();
  let { opt } = widgetData;

  chartDisposers.get(chartId)?.();
  existingChart?.dispose();

  const isDarkMode = document.body.getAttribute('data-theme') === 'dark';
  const theme = isDarkMode ? 'dark' : widgetData.theme || 'default';
  const chart = window.echarts.init(chartEl, theme);
  chart.group = 'default';
  const sizedOptions = () => chartLayoutForSize(opt, chartEl?.clientWidth ?? 0, chartEl?.clientHeight ?? 0);
  chartLayoutUpdaters.set(chartId, () => {
    const layout = sizedOptions();
    if ((layout !== opt) !== (compactLayouts.get(chart) ?? false)) {
      chart.setOption(layout);
      compactLayouts.set(chart, layout !== opt);
    }
  });

  let baseQuery = widgetData.query;
  const updateQuery = () => {
    const uq = params().query;
    widgetData.query = (uq && uq !== 'null') ? (baseQuery ? uq + ' | ' + baseQuery : uq) : baseQuery;
  };
  updateQuery();

  (window as any)[`${chartType}Chart`] = chart;

  const render = (notMerge = false) => {
    const styles = applyChartTheme(opt);
    if (opt.series?.[0]?.backgroundStyle) {
      opt.series[0].backgroundStyle = { color: styles.chartBg };
    }
    const layout = chartLayoutForSize(updateChartConfiguration(widgetData, opt, opt.dataset.source), chartEl?.clientWidth ?? 0, chartEl?.clientHeight ?? 0);
    chart.setOption(layout, notMerge);
    drawnResponse.delete(chart);
    compactLayouts.set(chart, layout !== opt);
    chartRefreshState.set(chart, { url: chartDataUrl(widgetData), hasData: (opt.dataset.source?.length ?? 0) > 1, failures: 0, retryAt: 0 });
    applyHighlightBand(chart, widgetData);
  };
  render();
  (chartEl as any).applyThresholds = (thresholds: Record<string, number>) => applyThresholds(chart, thresholds, widgetData.unit ?? '');
  chartUpdaters.set(chartId, (next) => {
    widgetData = next;
    opt = next.opt;
    baseQuery = next.query;
    updateQuery();
    // A changed query replaces the drawn data (and any fetch still in flight for the old one);
    // an unchanged lazy widget keeps its drawing and refreshes quietly.
    const urlChanged = chartRefreshState.get(chart)?.url !== chartDataUrl(widgetData);
    if (opt.dataset.source || urlChanged) render(true);
    if (!opt.dataset.source) updateChartData(chart, opt, true, widgetData, controller.signal, urlChanged);
    attach();
  });

  // Opt-in per chart: the Lucid container declares data-exemplars-url next to the
  // chart it decorates (see Pages.Telemetry.metricDetailChart).
  const exemplarUrl = chartEl?.closest('[data-exemplars-url]')?.getAttribute('data-exemplars-url');
  const attach = () => exemplarUrl && attachExemplars(chart, exemplarUrl, controller.signal).catch(() => {});
  attach();

  // Use shared ResizeObserver instead of per-widget
  if (chartEl) sharedResizeObserver.observe(chartEl);

  liveStreamCheckbox?.addEventListener('change', () => {
    if (liveStreamCheckbox.checked) {
      intervalId = setInterval(() => updateChartData(chart, opt, true, widgetData, controller.signal, false), INITIAL_FETCH_INTERVAL);
    } else if (intervalId) {
      clearInterval(intervalId);
      intervalId = null;
    }
  }, { signal: controller.signal });

  let dataObserver: IntersectionObserver | undefined;
  if (!opt.dataset.source && chartEl) {
    dataObserver = new IntersectionObserver(
      (entries, observer) =>
        entries[0]?.isIntersecting && (updateChartData(chart, opt, true, widgetData, controller.signal), observer.disconnect())
    );
    dataObserver.observe(chartEl);
  }

  // A chart refreshes its own data and nothing else. It used to also call
  // `logListTable.refetchLogs()`, which made every chart on the page order a full replacement of
  // the log list: two charts, two replacements per event, each one throwing away the reader's
  // rows and their scroll position. The list listens for these same events itself and knows
  // which ones deserve an incremental newer-cursor fetch rather than a replacement — that
  // decision belongs to it, not to whatever charts happen to be mounted beside it.
  ['submit', 'add-query'].forEach((event) => {
    const selector = event === 'submit' ? '#log_explorer_form' : '#filterElement';
    document.querySelector(selector)?.addEventListener(event, () => {
      updateQuery();
      updateChartData(chart, opt, true, widgetData, controller.signal);
    }, { signal: controller.signal });
  });

  window.addEventListener('update-query', (e: Event) => {
    updateQuery();
    const detail = (e as CustomEvent<{ ast?: string; source?: string }>).detail;
    if (detail?.ast) widgetData.queryAST = detail.ast;
    updateChartData(chart, opt, true, widgetData, controller.signal, detail?.source !== 'auto-refresh');
  }, { signal: controller.signal });

  // Register with shared theme observer instead of per-widget MutationObserver
  const onThemeChange: ThemeCallback = (_isDark, _cbStyles) => {
    invalidateLogLevelColors();
    const freshStyles = applyChartTheme(opt);
    opt.series?.forEach((s: any) => {
      if (s.backgroundStyle) s.backgroundStyle = { color: freshStyles.chartBg };
      if (widgetData.widgetType !== 'timeseries_stat') {
        const color = getSeriesColor(s.name || '');
        s.itemStyle = { ...s.itemStyle, color };
        if (s.type === 'line') s.lineStyle = { ...s.lineStyle, color };
      }
    });
    chart.setOption(sizedOptions(), false);
  };
  themeCallbacks.add(onThemeChange);

  chartDisposers.set(chartId, () => {
    if (intervalId) clearInterval(intervalId);
    dataObserver?.disconnect();
    controller.abort();
    if (chartEl) sharedResizeObserver.unobserve(chartEl);
    themeCallbacks.delete(onThemeChange);
    if (!chart.isDisposed()) chart.dispose();
    if ((window as any)[`${chartType}Chart`] === chart) delete (window as any)[`${chartType}Chart`];
    chartDisposers.delete(chartId);
    chartUpdaters.delete(chartId);
    chartLayoutUpdaters.delete(chartId);
  });
};

/**
 * Number/duration formatting now lives in ./stat-value (pure + unit-tested);
 * re-export onto window for the inline chart formatters that reference them.
 */
window.formatNumber = formatNumber;
window.convertToNanoseconds = convertToNanoseconds;
window.formatDuration = formatDuration;
window.formatBytes = formatBytes;

// Recursively build the widget order from a grid container.
// It looks for direct children with the class "grid-stack-item" and
// expects their ids to end with "_widgetEl". If an item contains a nested grid
// (an element with class "nested-grid"), its order is built recursively.
function buildWidgetOrder(container: HTMLElement) {
  // Use :scope to select only direct children.
  const items = container.querySelectorAll(':scope > .grid-stack-item') as NodeListOf<HTMLElement & { gridstackNode: Record<string, any> }>;
  const order: Record<string, any> = {};

  // Batch read all layout properties first
  const itemsData: Array<{ el: HTMLElement; id: string; node: any; nestedGrid: HTMLElement | null }> = [];

  items.forEach((el) => {
    if (!el.id || !el.id.endsWith('_widgetEl')) return;
    // GridStack attaches gridstackNode when it adopts an element, which is a frame or two
    // after HTMX puts it in the DOM. Reading a position off a not-yet-adopted item threw,
    // and the throw aborted the whole walk — so the user's drag was silently never saved.
    // It has no position to report yet; the next save picks it up.
    if (!el.gridstackNode) return;
    const widgetId = el.id.slice(0, -'_widgetEl'.length);
    const nestedGrid = el.querySelector('.nested-grid') as HTMLElement | null;

    itemsData.push({
      el,
      id: widgetId,
      node: el.gridstackNode,
      nestedGrid,
    });
  });

  // Now process the collected data without further DOM reads
  itemsData.forEach(({ id, node, nestedGrid }) => {
    const reorderItem: any = {
      x: node.x,
      y: node.y,
      w: node.w,
      h: node.h,
    };

    if (nestedGrid) {
      const childOrder = buildWidgetOrder(nestedGrid);
      if (Object.keys(childOrder).length > 0) {
        reorderItem.children = childOrder;
      }
    }
    order[id] = reorderItem;
  });

  return order;
}

function getActiveGrid(): HTMLElement | null {
  return document.querySelector('.grid-stack:not(.hidden)') || document.querySelector('.grid-stack');
}

(window as any).buildWidgetOrder = buildWidgetOrder;
(window as any).getActiveGrid = getActiveGrid;

(window as any).debounce = debounce;

function bindFunctionsToObjects(rootObj: any, obj: any) {
  if (!obj || typeof obj !== 'object') return;

  Object.keys(obj).forEach((key) => {
    const value = obj[key];
    if (typeof value === 'function') {
      obj[key] = value.bind(rootObj);
    } else if (value && typeof value === 'object') {
      bindFunctionsToObjects(rootObj, value);
    }
  });

  return obj;
}

// Global delegated click handler for table rows with on_row_click
document.addEventListener('click', (e) => {
  const tr = (e.target as HTMLElement).closest('tr[data-row]') as HTMLElement | null;
  if (!tr) return;
  const table = tr.closest('table[data-on-row-click]') as HTMLElement | null;
  if (!table) return;
  try {
    const onRowClick = JSON.parse(table.dataset.onRowClick!);
    const rowData = JSON.parse(tr.dataset.row!);
    const varName = onRowClick.set_variable;
    const value = onRowClick.value
      ? onRowClick.value.replace(/\{\{row\.(\w+)\}\}/g, (_: string, field: string) => rowData[field])
      : Object.values(rowData)[0] as string;
    if (onRowClick.navigate_to_tab) {
      const tabs = document.querySelectorAll('#dashboard-tabs-container [role="tab"]') as NodeListOf<HTMLAnchorElement>;
      for (const tab of tabs) {
        // Match by slug in href path (e.g. /tab/databases) for robustness
        if (tab.href?.includes('/tab/' + onRowClick.navigate_to_tab.toLowerCase().replace(/\s+/g, '-'))) {
          const tabUrl = new URL(tab.href, window.location.origin);
          if (varName) tabUrl.searchParams.set('var-' + varName, value);
          window.location.href = tabUrl.pathname + tabUrl.search;
          return;
        }
      }
    }
    if (varName) window.setVariable(varName, value);
  } catch { /* ignore malformed data attributes */ }
});

// Create threshold markLines for ECharts (reads semantic colors from CSS tokens).
// `unit` is the widget's, so a threshold prints the way its axis does — "Alert: 1.8s"
// against an axis of seconds, not the raw 1800 the label used to carry.
const createThresholdMarkLines = (thresholds: Record<string, number>, unit = '') => {
  const styles = getChartStyles();
  const thresholdStyles: Record<string, { color: string; label: string }> = {
    alert: { color: styles.errorColor, label: 'Alert' },
    warning: { color: styles.warningColor, label: 'Warning' },
  };
  return Object.entries(thresholds)
    .filter(([_, value]) => !isNaN(value))
    .map(([type, value]) => {
      const color = thresholdStyles[type]?.color || styles.textColor;
      return {
        yAxis: value,
        name: type,
        label: {
          // Formatted here rather than left to echarts' `{c}`: that prints the raw number,
          // which read "Alert: 1800" under an axis labelled "1.8s".
          formatter: `${thresholdStyles[type]?.label || type}: ${formatStatValue(value, unit)}`,
          // Below the line rather than above it. The axis headroom around a threshold is 5%
          // of the range (see updateChartConfiguration) — less than a line of text — so the
          // topmost threshold's label was drawn half outside the grid and clipped.
          position: 'insideEndBottom',
          // Left unstyled, the label inherited echarts' mark defaults: white, stroked in the
          // mark colour, which over a dashed line of that same colour reads as doubled text.
          // Say the colour, kill the stroke, and sit it on a chip of the chart background so
          // it stays readable where the series crosses it.
          color,
          fontSize: 10,
          fontWeight: 600,
          textBorderWidth: 0,
          textShadowBlur: 0,
          // The raised-surface token, not the chart's translucent overlay: the label has to
          // stay readable where the series itself passes under it.
          backgroundColor: styles.tooltipBg,
          padding: [2, 4],
          borderRadius: 3,
        },
        lineStyle: { color, width: 2, type: 'dashed' },
      };
    });
};

// Apply thresholds to a chart
const applyThresholds = (chart: any, thresholds: Record<string, number>, unit = '') => {
  const option = chart?.getOption();
  if (!option?.series?.length) return;

  chart.setOption({
    series: option.series.map((s: any) => ({
      ...s,
      markLine: { silent: true, symbol: 'none', data: createThresholdMarkLines(thresholds, unit) },
    })),
  });
};

const chartConfigurations = new WeakMap<HTMLElement, string>();
function initializeChartWidgets() {
  [...document.querySelectorAll<HTMLElement>('[data-chart-config]')].sort((a, b) => b.clientHeight - a.clientHeight).forEach(host => {
    const configJSON = host.dataset.chartConfig!;
    const signature = configJSON + location.search + JSON.stringify(window.getDashboardConstants?.(host));
    if (chartConfigurations.get(host) === signature) return;
    chartConfigurations.set(host, signature);
    const config = JSON.parse(configJSON);
    const opt = JSON.parse(config.echartOpt, (_key, value) => {
      if (typeof value === 'string' && value.trim().startsWith('function(')) {
        try { return (0, eval)('(' + value + ')'); } catch { return value; }
      }
      return value;
    });
    const widgetData = { ...config, opt };
    if (!opt.dataset.source) prefetchChartData(widgetData);
    queueChartInit(() => {
      if (!host.isConnected || chartConfigurations.get(host) !== signature) return;
      opt.tooltip.appendTo = host.closest('.dashboard-grid-wrapper') || 'body';
      bindFunctionsToObjects(opt, opt);
      chartWidget(widgetData);
    }, config.chartId);
  });
}
initializeChartWidgets();
document.addEventListener('htmx:after:process', initializeChartWidgets);
