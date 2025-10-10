'use strict';
import { params } from './main';
const DEFAULT_BACKGROUND_STYLE = { color: 'rgba(240,248,255, 0.4)' };
const DARK_BACKGROUND_STYLE = { color: 'rgba(40, 40, 40, 0.15)' };
const INITIAL_FETCH_INTERVAL = 5000;
const $ = (id: string) => document.getElementById(id);
const DEFAULT_PALETTE = ['#1A74A8', '#067A57CC', '#EE6666', '#FAC858', '#73C0DE', '#3BA272', '#FC8452', '#9A60B4', '#ea7ccc'];

const createSeriesConfig = (widgetData: WidGetData, name: string, i: number, opt: any) => {
  const palette = opt.color || DEFAULT_PALETTE;
  const paletteColor = palette[i % palette.length];

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
  };

  // For line charts in dark mode, override the symbol to avoid white centers
  if (widgetData.chartType === 'line' && isDarkMode) {
    seriesOpt.symbol = 'circle'; // Use filled circle instead of empty circle
    seriesOpt.symbolSize = 4;
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

    const { from, to, headers, dataset, rows_per_min, stats } = await fetch(`/chart_data?${params}`).then((res) => res.json());

    opt.xAxis = opt.xAxis || {};
    opt.xAxis.min = from * 1000;
    opt.xAxis.max = to * 1000;
    opt.dataset.source = [headers, ...dataset.map((row: any) => [row[0] * 1000, ...row.slice(1)])];
    opt.yAxis.max = stats.max;
    if (widgetData.chartType != 'line') {
      opt.yAxis.max = stats.max_group_sum;
    }

    const subtitle = $(`${chartId}Subtitle`);
    subtitle && (subtitle.innerHTML = `${window.formatNumber(rows_per_min)} rows/min`);

    const value = $(`${chartId}Value`);
    value &&
      ((value.innerHTML = `${summarizeByPrefix} ${window.formatNumber(Number(stats[summarizeBy]))}`), value.classList.remove('hidden'));

    chart.hideLoading();
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
    logListTable?: any;
    dashboardRefreshTimer: NodeJS.Timeout | null;
    dashboardRefreshInterval: number;
    bindFunctionsToObjects: any;
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
  const { chartType, opt, chartId, query, querySQL } = widgetData,
    chartEl = $(chartId),
    liveStreamCheckbox = $('streamLiveData') as HTMLInputElement;
  let intervalId: NodeJS.Timeout | null = null;

  // Dispose of any existing chart instance before creating a new one
  const existingChart = (window as any).echarts.getInstanceByDom(chartEl);
  if (existingChart) {
    existingChart.dispose();
  }

  // Determine theme based on current mode
  const isDarkMode = document.body.getAttribute('data-theme') === 'dark';
  const theme = isDarkMode ? 'dark' : widgetData.theme || 'default';

  // Initialize new chart
  const chart = (window as any).echarts.init(chartEl, theme);
  chart.group = 'default';

  // Store the original base query to avoid stacking
  const baseQuery = widgetData.query;
  const userQuery = params().query;

  // Ensure we don't use 'null' string as a query
  if (userQuery && userQuery !== 'null') {
    widgetData.query = baseQuery ? userQuery + ' | ' + baseQuery : userQuery;
  } else {
    widgetData.query = baseQuery;
  }

  opt.dataset.source = opt.dataset?.source?.map((row: any) => [row[0] * 1000, ...row.slice(1)]) ?? null;

  (window as any)[`${chartType}Chart`] = chart;

  // Get CSS variable values from body (where theme is applied)
  // Cache all computed style reads to minimize reflows
  const computedStyle = getComputedStyle(document.body);
  const styles = {
    textColor: computedStyle.getPropertyValue('--color-textWeak').trim(),
    tooltipBg: computedStyle.getPropertyValue('--color-bgRaised').trim(),
    tooltipTextColor: computedStyle.getPropertyValue('--color-textStrong').trim(),
    tooltipBorderColor: computedStyle.getPropertyValue('--color-borderWeak').trim(),
  };

  if (styles.textColor) {
    opt.legend = opt.legend || {};
    opt.legend.textStyle = opt.legend.textStyle || {};
    opt.legend.textStyle.color = styles.textColor;
  }

  // Configure tooltip styling
  opt.tooltip = opt.tooltip || {};
  opt.tooltip.backgroundColor = styles.tooltipBg || (isDarkMode ? 'rgba(50, 50, 50, 0.9)' : 'rgba(255, 255, 255, 0.9)');
  opt.tooltip.textStyle = opt.tooltip.textStyle || {};
  opt.tooltip.textStyle.color = styles.tooltipTextColor || (isDarkMode ? '#e0e0e0' : '#333');
  opt.tooltip.borderColor = styles.tooltipBorderColor || (isDarkMode ? '#555' : '#ccc');
  opt.tooltip.borderWidth = 1;

  // Override server's background style with theme-appropriate one
  if (opt.series?.[0]?.backgroundStyle) {
    opt.series[0].backgroundStyle = isDarkMode ? DARK_BACKGROUND_STYLE : DEFAULT_BACKGROUND_STYLE;
  }

  chart.setOption(updateChartConfiguration(widgetData, opt, opt.dataset.source));

  // Expose threshold functionality on chart element
  (chartEl as any).applyThresholds = (thresholds: Record<string, number>) => applyThresholds(chart, thresholds);

  // Use global resize queue to batch resize operations
  const resizeObserver = new ResizeObserver(() => {
    queueChartResize(chartId);
  });
  if (chartEl) {
    resizeObserver.observe(chartEl);
  }

  liveStreamCheckbox &&
    liveStreamCheckbox.addEventListener('change', () => {
      if (liveStreamCheckbox.checked) {
        intervalId = setInterval(() => updateChartData(chart, opt, true, widgetData), INITIAL_FETCH_INTERVAL);
      } else {
        if (intervalId) clearInterval(intervalId), (intervalId = null);
      }
    });

  if (!opt.dataset.source && chartEl) {
    const isDarkMode = document.body.getAttribute('data-theme') === 'dark';
    chart.showLoading({
      text: 'Loading...',
      color: isDarkMode ? '#1A74A8' : '#1A74A8',
      textColor: isDarkMode ? '#e0e0e0' : '#333',
      maskColor: isDarkMode ? 'rgba(30, 30, 30, 0.8)' : 'rgba(255, 255, 255, 0.8)',
      zlevel: 0,
    });
    new IntersectionObserver(
      (entries, observer) => entries[0]?.isIntersecting && (updateChartData(chart, opt, true, widgetData), observer.disconnect())
    ).observe(chartEl);
  }

  ['submit', 'add-query', 'update-query'].forEach((event) => {
    const selector = event === 'submit' ? '#log_explorer_form' : '#filterElement';
    document.querySelector(selector)?.addEventListener(event, (e) => {
      const userQuery = params().query;
      if (userQuery && userQuery !== 'null') {
        widgetData.query = baseQuery ? userQuery + ' | ' + baseQuery : userQuery;
      } else {
        widgetData.query = baseQuery;
      }

      if (window.logListTable) {
        (window.logListTable as any).refetchLogs();
      }

      updateChartData(chart, opt, true, widgetData);
    });
  });
  window.addEventListener('update-query', (e: any) => {
    if (e.detail?.ast) {
      widgetData.queryAST = e.detail.ast;
    }
    updateChartData(chart, opt, true, widgetData);
  });

  window.addEventListener('pagehide', () => {
    if (intervalId) clearInterval(intervalId);
    resizeObserver.disconnect();
  });

  // Listen for theme changes
  let themeChangeScheduled = false;
  const observer = new MutationObserver((mutations) => {
    const themeChanged = mutations.some((m) => m.type === 'attributes' && m.attributeName === 'data-theme');

    if (themeChanged && !themeChangeScheduled) {
      themeChangeScheduled = true;

      requestAnimationFrame(() => {
        const isDarkMode = document.body.getAttribute('data-theme') === 'dark';

        // Get CSS variable values from body (where theme is applied)
        // Cache all computed style reads to minimize reflows
        const computedStyle = getComputedStyle(document.body);
        const styles = {
          textColor: computedStyle.getPropertyValue('--color-textWeak').trim(),
          tooltipBg: computedStyle.getPropertyValue('--color-bgRaised').trim(),
          tooltipTextColor: computedStyle.getPropertyValue('--color-textStrong').trim(),
          tooltipBorderColor: computedStyle.getPropertyValue('--color-borderWeak').trim(),
        };

        // Update theme-related options
        if (styles.textColor) {
          opt.legend = opt.legend || {};
          opt.legend.textStyle = opt.legend.textStyle || {};
          opt.legend.textStyle.color = styles.textColor;
        }

        // Configure tooltip styling
        opt.tooltip = opt.tooltip || {};
        opt.tooltip.backgroundColor = styles.tooltipBg || (isDarkMode ? 'rgba(50, 50, 50, 0.9)' : 'rgba(255, 255, 255, 0.9)');
        opt.tooltip.textStyle = opt.tooltip.textStyle || {};
        opt.tooltip.textStyle.color = styles.tooltipTextColor || (isDarkMode ? '#e0e0e0' : '#333');
        opt.tooltip.borderColor = styles.tooltipBorderColor || (isDarkMode ? '#555' : '#ccc');
        opt.tooltip.borderWidth = 1;

        // Update background style for series
        if (opt.series?.[0]) {
          opt.series.forEach((s: any) => {
            if (s.backgroundStyle) {
              s.backgroundStyle = isDarkMode ? DARK_BACKGROUND_STYLE : DEFAULT_BACKGROUND_STYLE;
            }
          });
        }

        // Apply updated options without recreating the chart
        chart.setOption(opt, false);
        themeChangeScheduled = false;
      });
    }
  });

  observer.observe(document.body, {
    attributes: true,
    attributeFilter: ['data-theme'],
  });
};

(window as any).chartWidget = chartWidget;

/**
 * Format numbers with appropriate suffixes and precision
 * for display in charts and tooltips
 */
window.formatNumber = (n: number): string => {
  if (n >= 1_000_000_000) return `${Math.floor(n / 1_000_000_000)}.${Math.floor((n % 1_000_000_000) / 100_000_000)}B`;
  if (n >= 1_000_000) return `${Math.floor(n / 1_000_000)}.${Math.floor((n % 1_000_000) / 100_000)}M`;
  if (n >= 1_000) return `${Math.floor(n / 1_000)}.${Math.floor((n % 1_000) / 100)}K`;

  // Format decimals appropriately based on magnitude
  if (!Number.isInteger(n)) {
    if (n >= 100) return Math.round(n).toString();
    if (n >= 10) return parseFloat(n.toFixed(1)).toString();
    return parseFloat(n.toFixed(2)).toString();
  }

  return n.toString();
};

// Update the widget order on the server.
export const updateWidgetOrder = (projectId: string, dashboardId: string) => () => {
  const mainContainer = document.querySelector('.grid-stack') as HTMLElement;
  const widgetOrder = buildWidgetOrder(mainContainer);
  fetch(`/p/${projectId}/dashboards/${dashboardId}/widgets_order`, {
    method: 'PATCH',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(widgetOrder),
  })
    .then((response) => {
      if (!response.ok) {
        throw new Error(`HTTP error! Status: ${response.status}`);
      }
    })
    .catch((error) => console.error('Error updating widget order:', error));
};

(window as any).updateWidgetOrder = updateWidgetOrder;

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

(window as any).buildWidgetOrder = buildWidgetOrder;

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
