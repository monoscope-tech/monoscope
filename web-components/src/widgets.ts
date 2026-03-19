'use strict';
import { params } from './main';
import { getSeriesColor } from './colorMapping';
const DEFAULT_BACKGROUND_STYLE = { color: 'rgba(240,248,255, 0.4)' };
const DARK_BACKGROUND_STYLE = { color: 'rgba(30, 38, 52, 0.18)' };
const INITIAL_FETCH_INTERVAL = 5000;
const $ = (id: string) => document.getElementById(id);
const DEFAULT_PALETTE = ['#1A74A8', '#067A57CC', '#EE6666', '#FAC858', '#73C0DE', '#3BA272', '#FC8452', '#9A60B4', '#ea7ccc'];

// --- Concurrency limiter for fetch requests (max 3 in-flight) ---
const MAX_CONCURRENT_FETCHES = 4;
let activeFetches = 0;
const fetchQueue: Array<() => void> = [];

const limitedFetch = (url: string): Promise<Response> => {
  return new Promise((resolve, reject) => {
    const run = () => {
      activeFetches++;
      fetch(url).then(resolve, reject).finally(() => {
        activeFetches--;
        if (fetchQueue.length > 0) fetchQueue.shift()!();
      });
    };
    if (activeFetches < MAX_CONCURRENT_FETCHES) run();
    else fetchQueue.push(run);
  });
};

// --- Staggered chart initialization, paused during scroll ---
// Each echarts.init+setOption takes ~30-35ms which can't be split further.
// We process 1 chart per rAF frame but pause entirely during active scrolling
// to keep scroll buttery smooth. Charts resume init when scrolling stops.
const initQueue: Array<{ fn: () => void; el: HTMLElement | null }> = [];
let initScheduled = false;
let isScrolling = false;
let scrollTimer: ReturnType<typeof setTimeout> | null = null;

// Detect scroll on the main content container (or any parent that scrolls)
const trackScroll = () => {
  const scrollTarget = document.getElementById('main-content') || window;
  scrollTarget.addEventListener('scroll', () => {
    isScrolling = true;
    if (scrollTimer) clearTimeout(scrollTimer);
    scrollTimer = setTimeout(() => {
      isScrolling = false;
      ensureScheduled(); // resume init queue after scroll settles
    }, 150);
  }, { passive: true });
};
if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', trackScroll);
else trackScroll();

const processInitQueue = () => {
  if (isScrolling) {
    // Don't init charts while scrolling — reschedule for after scroll ends
    initScheduled = false;
    return;
  }
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
  if (!initScheduled && initQueue.length > 0 && !isScrolling) {
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

// Deferred init for off-screen charts — only init when scrolled near viewport.
// 600px rootMargin starts loading ahead of scroll so charts are ready before visible.
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
}, { rootMargin: '600px' });

const queueChartInit = (fn: () => void, chartId?: string) => {
  const el = chartId ? document.getElementById(chartId) : null;
  if (!el) {
    initQueue.push({ fn, el: null });
    ensureScheduled();
    return;
  }
  // Check if near viewport — init immediately; otherwise defer until scrolled near
  const rect = el.getBoundingClientRect();
  if (rect.top < window.innerHeight + 600 && rect.bottom > -200) {
    initQueue.push({ fn, el });
    visibilityObserver.observe(el);
    ensureScheduled();
  } else {
    deferredInits.set(el, fn);
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

const showNoDataOverlay = (chartId: string, message?: string) => {
  const el = $(`${chartId}`);
  if (!el) return;
  const parent = el.parentElement;
  if (!parent) return;
  let overlay = parent.querySelector('.chart-no-data') as HTMLElement;
  const msg = message || 'No data for the selected time range';
  if (!overlay) {
    overlay = document.createElement('div');
    overlay.className = 'chart-no-data absolute inset-0 flex items-center justify-center z-10 pointer-events-none';
    parent.style.position = 'relative';
    parent.appendChild(overlay);
  }
  overlay.innerHTML = `<div class="text-center text-textWeak"><svg class="w-6 h-6 mx-auto mb-2 opacity-30" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M3 13h4l3-8 4 16 3-8h4"/></svg><p class="text-sm">${msg}</p></div>`;
  overlay.classList.remove('hidden');
};

const hideNoDataOverlay = (chartId: string) => {
  const el = $(`${chartId}`);
  const overlay = el?.parentElement?.querySelector('.chart-no-data') as HTMLElement;
  if (overlay) overlay.classList.add('hidden');
};


const createSeriesConfig = (widgetData: WidGetData, name: string, i: number, opt: any) => {
  // For timeseries_stat widgets with generic column names, use the default blue color
  // This ensures stat widgets (total requests, etc.) show in blue as expected
  const isGenericStatColumn = widgetData.widgetType === 'timeseries_stat' &&
    (name === 'value' || name.startsWith('count') || name === '' || !name);
  const paletteColor = isGenericStatColumn ? DEFAULT_PALETTE[0] : getSeriesColor(name);

  const gradientColor = new (window as any).echarts.graphic.LinearGradient(0, 0, 0, 1, [
    { offset: 0, color: (window as any).echarts.color.modifyAlpha(paletteColor, 1) },
    { offset: 1, color: (window as any).echarts.color.modifyAlpha(paletteColor, 0) },
  ]);

  const isDarkMode = document.body.getAttribute('data-theme') === 'dark';
  const backgroundStyle = isDarkMode ? DARK_BACKGROUND_STYLE : DEFAULT_BACKGROUND_STYLE;

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
    // Set the color explicitly for all series types
    itemStyle: { color: paletteColor },
  };

  // For line charts, also set lineStyle color
  if (widgetData.chartType === 'line') {
    seriesOpt.lineStyle = { color: paletteColor };
    // For line charts in dark mode, override the symbol to avoid white centers
    if (isDarkMode) {
      seriesOpt.symbol = 'circle'; // Use filled circle instead of empty circle
      seriesOpt.symbolSize = 4;
    }
  }

  if (widgetData.widgetType == 'timeseries_stat') {
    seriesOpt.itemStyle = { color: gradientColor };
    seriesOpt.areaStyle = { color: gradientColor };
  }

  return seriesOpt;
};

const updateChartConfiguration = (widgetData: WidGetData, opt: any, data: any) => {
  if (!data) return opt;

  // Avoid unnecessary updates if data structure hasn't changed
  const cols = data[0]?.slice(1);
  const currentLegendData = opt.legend?.data;

  // Only update if legend data has actually changed
  if (JSON.stringify(cols) !== JSON.stringify(currentLegendData)) {
    opt.series = cols?.map((n: any, i: number) => createSeriesConfig(widgetData, n, i, opt));
    opt.legend.data = cols;
  }

  // Merge threshold markLines into first series to avoid a second setOption call
  const thresholds: Record<string, number> = {};
  if (widgetData.alertThreshold != null && !isNaN(widgetData.alertThreshold)) thresholds.alert = widgetData.alertThreshold;
  if (widgetData.warningThreshold != null && !isNaN(widgetData.warningThreshold)) thresholds.warning = widgetData.warningThreshold;
  if (Object.keys(thresholds).length > 0 && opt.series?.length) {
    opt.series[0].markLine = { silent: true, symbol: 'none', data: createThresholdMarkLines(thresholds) };
  }

  return opt;
};

const updateChartData = async (chart: any, opt: any, shouldFetch: boolean, widgetData: WidGetData) => {
  if (!shouldFetch) return;

  const { query, querySQL, pid, chartId, summarizeBy, summarizeByPrefix } = widgetData;
  // Batch DOM updates before fetch
  requestAnimationFrame(() => {
    const loader = $(`${chartId}_loader`);
    const borderedItem = $(`${chartId}_bordered`);

    if (loader) loader.classList.remove('hidden');
    if (borderedItem) borderedItem.classList.add('spotlight-border');
  });

  try {
    const params = new URLSearchParams(window.location.search);
    params.set('pid', pid);

    // Add dashboard constants (from data-constants attribute)
    const constants = (window as any).getDashboardConstants?.() || {};
    Object.entries(constants).forEach(([key, value]) => {
      if (!params.has(key)) params.set(key, value as string);
    });

    // Default query to use when no query is provided
    const DEFAULT_QUERY = 'summarize count(*) by bin_auto(timestamp)';

    // Handle query parameter
    if (!query || query === 'null' || query === '') {
      // If query is empty or null, use the default query
      params.set('query', DEFAULT_QUERY);
    } else {
      // Check if query contains any summarize clause with binning
      const hasSummarize = /summarize\s+/i.test(query);
      const hasBinning = /\s+by\s+bin/i.test(query) || /\s+by\s+.*\(.*\)/i.test(query);

      // If no summarize clause or no binning in the query, add default summarization
      if (!hasSummarize || !hasBinning) {
        params.set('query', query + ' | ' + DEFAULT_QUERY);
      } else {
        // Ensure we preserve the original query exactly as written
        // This ensures grouping by fields like 'kind' works correctly
        params.set('query', query);
      }
    }

    if (querySQL && querySQL !== 'null') {
      params.set('query_sql', querySQL);
    }

    const { from, to, headers, dataset, rows_per_min, stats } = await limitedFetch(`/chart_data?${params}`).then((res) => res.json());
    const trmHeaders = headers?.map((h: string) => {
      if (h === 'timestamp' || h === 'created_at') {
        return h;
      }
      return h.substring(0, 75) + (h.length > 75 ? '...' : '');
    });
    opt.xAxis = opt.xAxis || {};
    opt.xAxis.min = from;  // Already in ms from server
    opt.xAxis.max = to;
    opt.dataset.source = [trmHeaders || [], ...dataset];
    if (stats) {
      opt.yAxis.max = stats.max;
      if (widgetData.chartType != 'line') {
        opt.yAxis.max = stats.max_group_sum;
      }
    }

    const subtitle = $(`${chartId}Subtitle`);
    subtitle && (subtitle.innerHTML = `${window.formatNumber(rows_per_min)}/min`);

    const value = $(`${chartId}Value`);
    if (value && stats) {
      const durationUnits = ['ns', 'μs', 'us', 'ms', 's', 'm', 'h'];
      const unit = widgetData.unit || '';
      const isDuration = durationUnits.includes(unit);
      const formattedValue = isDuration
        ? window.formatDuration(window.convertToNanoseconds(Number(stats[summarizeBy]), unit))
        : window.formatNumber(Number(stats[summarizeBy]));
      const displayUnit = {'': '', '1': '', '{}': '', 'By': ' bytes'}[unit] ?? ` ${unit}`;
      const unitSuffix = isDuration ? '' : displayUnit;
      value.innerHTML = `${summarizeByPrefix} ${formattedValue}${unitSuffix}`;
      value.classList.remove('hidden');
    }

    chart.hideLoading();
    if (!dataset || dataset.length === 0) {
      showNoDataOverlay(chartId);
    } else {
      hideNoDataOverlay(chartId);
    }
    chart.setOption(updateChartConfiguration(widgetData, opt, opt.dataset.source), true);
    if ((window as any).barChart) {
      (window as any).barChart.dispatchAction({
        type: 'takeGlobalCursor',
        key: 'dataZoomSelect',
        dataZoomSelectActive: true,
      });
    }
    window.dispatchEvent(new CustomEvent('chart-updated', { detail: { chartId } }));
  } catch (e) {
    console.error('Failed to fetch new data:', e);
    chart.hideLoading();
    showNoDataOverlay(chartId, 'Failed to load data');
  } finally {
    // Batch DOM updates after fetch completes
    requestAnimationFrame(() => {
      const loader = $(`${chartId}_loader`);
      const borderedItem = $(`${chartId}_bordered`);

      if (loader) loader.classList.add('hidden');
      if (borderedItem) borderedItem.classList.remove('spotlight-border');
    });
  }
};

declare global {
  interface Window {
    formatNumber: (num: number) => string;
    convertToNanoseconds: (value: number, unit: string) => number;
    formatDuration: (ns: number) => string;
    logListTable?: any;
    dashboardRefreshTimer: NodeJS.Timeout | null;
    dashboardRefreshInterval: number;
    bindFunctionsToObjects: any;
    setVariable: (key: string, value: string) => void;
    getVariable: (key: string) => string;
  }
}

type WidGetData = {
  chartType: string;
  opt: Record<string, any>;
  chartId: string;
  query: string;
  sql: string;
  querySQL: string;
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
  warningThreshold?: number | null;
};

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
      const chart = (window as any).echarts.getInstanceByDom(chartEl);
      if (chart && !chart.isDisposed()) {
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

const chartWidget = (widgetData: WidGetData) => {
  const { chartType, opt, chartId } = widgetData,
    chartEl = $(chartId),
    liveStreamCheckbox = $('streamLiveData') as HTMLInputElement;
  let intervalId: NodeJS.Timeout | null = null;

  // Dispose of any existing chart instance before creating a new one
  const existingChart = (window as any).echarts.getInstanceByDom(chartEl);
  if (existingChart) existingChart.dispose();

  const isDarkMode = document.body.getAttribute('data-theme') === 'dark';
  const theme = isDarkMode ? 'dark' : widgetData.theme || 'default';
  const chart = (window as any).echarts.init(chartEl, theme);
  chart.group = 'default';

  const baseQuery = widgetData.query;
  const userQuery = params().query;
  widgetData.query = (userQuery && userQuery !== 'null')
    ? (baseQuery ? userQuery + ' | ' + baseQuery : userQuery)
    : baseQuery;

  (window as any)[`${chartType}Chart`] = chart;

  // Read computed styles once (shared across all charts via cache)
  const computedStyle = getComputedStyle(document.body);
  const styles = {
    textColor: computedStyle.getPropertyValue('--color-textWeak').trim(),
    tooltipBg: computedStyle.getPropertyValue('--color-bgRaised').trim(),
    tooltipTextColor: computedStyle.getPropertyValue('--color-textStrong').trim(),
    tooltipBorderColor: computedStyle.getPropertyValue('--color-borderWeak').trim(),
  };

  if (styles.textColor) {
    opt.legend = opt.legend || {};
    opt.legend.textStyle = { ...opt.legend.textStyle, color: styles.textColor };
  }
  opt.tooltip = {
    ...opt.tooltip,
    backgroundColor: styles.tooltipBg || (isDarkMode ? 'rgba(50, 50, 50, 0.9)' : 'rgba(255, 255, 255, 0.9)'),
    textStyle: { ...opt.tooltip?.textStyle, color: styles.tooltipTextColor || (isDarkMode ? '#e0e0e0' : '#333') },
    borderColor: styles.tooltipBorderColor || (isDarkMode ? '#555' : '#ccc'),
    borderWidth: 1,
  };
  opt.backgroundColor = 'transparent';
  if (opt.series?.[0]?.backgroundStyle) {
    opt.series[0].backgroundStyle = isDarkMode ? DARK_BACKGROUND_STYLE : DEFAULT_BACKGROUND_STYLE;
  }

  chart.setOption(updateChartConfiguration(widgetData, opt, opt.dataset.source));
  (chartEl as any).applyThresholds = (thresholds: Record<string, number>) => applyThresholds(chart, thresholds);

  // Use shared ResizeObserver instead of per-widget
  if (chartEl) sharedResizeObserver.observe(chartEl);

  liveStreamCheckbox?.addEventListener('change', () => {
    if (liveStreamCheckbox.checked) {
      intervalId = setInterval(() => updateChartData(chart, opt, true, widgetData), INITIAL_FETCH_INTERVAL);
    } else if (intervalId) {
      clearInterval(intervalId);
      intervalId = null;
    }
  });

  if (!opt.dataset.source && chartEl) {
    chart.showLoading({
      text: 'Loading...',
      color: isDarkMode ? '#3B82F6' : '#1A74A8',
      textColor: isDarkMode ? '#e0e0e0' : '#333',
      maskColor: isDarkMode ? 'rgba(25, 30, 42, 0.85)' : 'rgba(255, 255, 255, 0.8)',
      zlevel: 0,
    });
    new IntersectionObserver(
      (entries, observer) => entries[0]?.isIntersecting && (updateChartData(chart, opt, true, widgetData), observer.disconnect())
    ).observe(chartEl);
  }

  const updateQuery = () => {
    const uq = params().query;
    widgetData.query = (uq && uq !== 'null') ? (baseQuery ? uq + ' | ' + baseQuery : uq) : baseQuery;
  };

  ['submit', 'add-query'].forEach((event) => {
    const selector = event === 'submit' ? '#log_explorer_form' : '#filterElement';
    document.querySelector(selector)?.addEventListener(event, (e: any) => {
      updateQuery();
      if (window.logListTable && e.detail?.source !== 'expand-timerange') (window.logListTable as any).refetchLogs();
      updateChartData(chart, opt, true, widgetData);
    });
  });

  window.addEventListener('update-query', (e: any) => {
    updateQuery();
    if (e.detail?.ast) widgetData.queryAST = e.detail.ast;
    if (window.logListTable && e.detail?.source !== 'expand-timerange' && e.detail?.source !== 'chart-zoom')
      (window.logListTable as any).refetchLogs();
    updateChartData(chart, opt, true, widgetData);
  });

  // Register with shared theme observer instead of per-widget MutationObserver
  const onThemeChange: ThemeCallback = (isDark, styles) => {
    if (styles.textColor) {
      opt.legend = opt.legend || {};
      opt.legend.textStyle = { ...opt.legend.textStyle, color: styles.textColor };
    }
    opt.tooltip = {
      ...opt.tooltip,
      backgroundColor: styles.tooltipBg || (isDark ? 'rgba(50, 50, 50, 0.9)' : 'rgba(255, 255, 255, 0.9)'),
      textStyle: { ...opt.tooltip?.textStyle, color: styles.tooltipTextColor || (isDark ? '#e0e0e0' : '#333') },
      borderColor: styles.tooltipBorderColor || (isDark ? '#555' : '#ccc'),
      borderWidth: 1,
    };
    opt.backgroundColor = 'transparent';
    opt.series?.forEach((s: any) => {
      if (s.backgroundStyle) s.backgroundStyle = isDark ? DARK_BACKGROUND_STYLE : DEFAULT_BACKGROUND_STYLE;
    });
    chart.setOption(opt, false);
  };
  themeCallbacks.add(onThemeChange);

  window.addEventListener('pagehide', () => {
    if (intervalId) clearInterval(intervalId);
    if (chartEl) sharedResizeObserver.unobserve(chartEl);
    themeCallbacks.delete(onThemeChange);
  });
};

// Expose both direct and queued init
(window as any).chartWidget = chartWidget;
(window as any).queueChartInit = queueChartInit;

/**
 * Format numbers with appropriate suffixes and precision
 * for display in charts and tooltips
 */
window.formatNumber = (n: number): string => {
  if (n >= 1_000_000_000) return `${Math.floor(n / 1_000_000_000)}.${Math.floor((n % 1_000_000_000) / 100_000_000)}B`;
  if (n >= 1_000_000) return `${Math.floor(n / 1_000_000)}.${Math.floor((n % 1_000_000) / 100_000)}M`;
  if (n >= 1_000) return `${Math.floor(n / 1_000)}.${Math.floor((n % 1_000) / 100)}K`;
  if (n === null) return 'N/A';

  // Format decimals appropriately based on magnitude
  if (!Number.isInteger(n) && !Number.isNaN(n)) {
    if (n >= 100) return Math.round(n).toString();
    if (n >= 10) return parseFloat(n.toFixed(1)).toString();
    return parseFloat(n.toFixed(2)).toString();
  }

  return n.toString();
};

/**
 * Convert a value from a specified time unit to nanoseconds
 * @param value - The numeric value to convert
 * @param unit - The unit of the input value (h, m, s, ms, μs, us, ns)
 * @returns Value converted to nanoseconds
 */
window.convertToNanoseconds = (value: number, unit: string): number => {
  const conversionFactors: Record<string, number> = {
    h: 3_600_000_000_000, // hours to nanoseconds
    m: 60_000_000_000, // minutes to nanoseconds
    s: 1_000_000_000, // seconds to nanoseconds
    ms: 1_000_000, // milliseconds to nanoseconds
    μs: 1_000, // microseconds to nanoseconds
    us: 1_000, // microseconds to nanoseconds (alt)
    ns: 1, // already nanoseconds
  };
  return value * (conversionFactors[unit] || 1);
};

/**
 * Format duration values from nanoseconds into human-readable time units
 * Matches the backend's prettyPrintDuration function in Utils.hs
 * @param ns - Duration in nanoseconds
 * @returns Formatted duration string with appropriate unit
 */
window.formatDuration = (ns: number): string => {
  if (ns >= 3_600_000_000_000) {
    // >= 1 hour
    return `${(ns / 3_600_000_000_000).toFixed(1)}h`;
  } else if (ns >= 60_000_000_000) {
    // >= 1 minute
    return `${(ns / 60_000_000_000).toFixed(1)}m`;
  } else if (ns >= 1_000_000_000) {
    // >= 1 second
    return `${(ns / 1_000_000_000).toFixed(1)}s`;
  } else if (ns >= 1_000_000) {
    // >= 1 millisecond
    return `${(ns / 1_000_000).toFixed(1)}ms`;
  } else if (ns >= 1_000) {
    // >= 1 microsecond
    return `${(ns / 1_000).toFixed(1)}μs`;
  } else {
    // nanoseconds
    return `${ns.toFixed(0)}ns`;
  }
};

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

function debounce(func: any, wait: number) {
  let timeout: NodeJS.Timeout;
  return (...args: any[]) => {
    if (timeout) {
      clearTimeout(timeout);
    }
    timeout = setTimeout(() => func(...args), wait);
  };
}
(window as any).debounce = debounce;

/**
 * Auto-refresh functionality for dashboards and widgets
 */
const DEFAULT_REFRESH_INTERVAL = 0; // Default to Off

// Global variable to store the refresh timer
window.dashboardRefreshTimer = null;
window.dashboardRefreshInterval = DEFAULT_REFRESH_INTERVAL;

function setRefreshInterval(detail: { interval: string }) {
  if (window.dashboardRefreshTimer) clearInterval(window.dashboardRefreshTimer);
  const interval = parseInt(detail.interval);
  if (interval > 0) {
    window.dashboardRefreshTimer = setInterval(() => {
      window.dispatchEvent(new CustomEvent('update-query'));
    }, interval);
  }
}

// Custom event handler for setting the refresh interval programmatically
window.addEventListener('setRefreshInterval', function (e: any) {
  if (e.detail !== undefined) {
    setRefreshInterval(e.detail);
  }
});

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
window.bindFunctionsToObjects = bindFunctionsToObjects;

// Table sorting function
(window as any).sortTable = (tableId: string, col: number, th: HTMLElement) => {
  const table = document.getElementById(tableId) as HTMLTableElement;
  if (!table) return;

  const tbody = table.querySelector('tbody');
  if (!tbody) return;

  const rows = Array.from(tbody.rows);
  const currentDir = th.dataset.sortDirection;
  const dir = currentDir === 'asc' ? 'desc' : 'asc';

  // Reset all headers to default state
  table.querySelectorAll('th').forEach((header) => {
    const arrow = header.querySelector('.sort-arrow');
    if (arrow) {
      header.dataset.sortDirection = 'none';
      arrow.textContent = '↕';
      arrow.setAttribute('data-sort', 'none');
      // Remove opacity-100 and add opacity-0 for non-sorted columns
      arrow.classList.remove('opacity-100');
      arrow.classList.add('opacity-0');
    }
  });

  // Update clicked header
  th.dataset.sortDirection = dir;
  const arrow = th.querySelector('.sort-arrow');
  if (arrow) {
    arrow.textContent = dir === 'asc' ? '↑' : '↓';
    arrow.setAttribute('data-sort', dir);
    // Make the arrow always visible for sorted column
    arrow.classList.remove('opacity-0');
    arrow.classList.add('opacity-100');
  }

  // Sort rows
  rows.sort((a, b) => {
    const aVal = a.cells[col]?.textContent?.trim() || '';
    const bVal = b.cells[col]?.textContent?.trim() || '';

    // Try numeric comparison first
    const aNum = parseFloat(aVal.replace(/[^\d.-]/g, ''));
    const bNum = parseFloat(bVal.replace(/[^\d.-]/g, ''));

    if (!isNaN(aNum) && !isNaN(bNum)) {
      return dir === 'asc' ? aNum - bNum : bNum - aNum;
    }

    // Then try date comparison
    const aDate = new Date(aVal);
    const bDate = new Date(bVal);
    if (!isNaN(aDate.getTime()) && !isNaN(bDate.getTime())) {
      return dir === 'asc' ? aDate.getTime() - bDate.getTime() : bDate.getTime() - aDate.getTime();
    }

    // Fall back to string comparison
    return dir === 'asc' ? aVal.localeCompare(bVal) : bVal.localeCompare(aVal);
  });

  tbody.append(...rows);
};

// Threshold line configurations
const THRESHOLDS = {
  alert: { color: '#dc2626', formatter: 'Alert: {c}' },
  warning: { color: '#f59e0b', formatter: 'Warning: {c}' },
} as const;

// Create threshold markLines for ECharts
export const createThresholdMarkLines = (thresholds: Record<string, number>) =>
  Object.entries(thresholds)
    .filter(([_, value]) => !isNaN(value))
    .map(([type, value]) => ({
      yAxis: value,
      name: type,
      label: { formatter: THRESHOLDS[type as keyof typeof THRESHOLDS]?.formatter || `${type}: {c}`, position: 'end' },
      lineStyle: { color: THRESHOLDS[type as keyof typeof THRESHOLDS]?.color || '#999', width: 2, type: 'dashed' },
    }));

// Apply thresholds to a chart
export const applyThresholds = (chart: any, thresholds: Record<string, number>) => {
  const option = chart?.getOption();
  if (!option?.series?.length) return;

  chart.setOption({
    series: option.series.map((s: any) => ({
      ...s,
      markLine: { silent: true, symbol: 'none', data: createThresholdMarkLines(thresholds) },
    })),
  });
};

// Signal that widget dependencies are ready
(window as any).widgetDepsReady = true;
