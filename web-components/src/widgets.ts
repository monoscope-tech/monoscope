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
  const cols = data[0]?.slice(1);
  opt.series = cols?.map((n: any, i: number) => createSeriesConfig(widgetData, n, i, opt));
  opt.legend.data = cols;
  return opt;
};

const updateChartData = async (chart: any, opt: any, shouldFetch: boolean, widgetData: WidGetData) => {
  if (!shouldFetch) return;

  const { query, querySQL, pid, chartId, summarizeBy, summarizeByPrefix } = widgetData;
  const loader = $(`${chartId}_loader`);
  // Show loader before fetch
  if (loader) loader.classList.remove('hidden');

  const borderedItem = $(`${chartId}_bordered`);
  if (borderedItem) borderedItem.classList.add('spotlight-border');

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
    // Hide loader after fetch completes (success or failure)
    if (loader) loader.classList.add('hidden');
    if (borderedItem) borderedItem.classList.remove('spotlight-border');
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
  const computedStyle = getComputedStyle(document.body);
  const textColor = computedStyle.getPropertyValue('--color-textWeak').trim();
  const tooltipBg = computedStyle.getPropertyValue('--color-bgRaised').trim();
  const tooltipTextColor = computedStyle.getPropertyValue('--color-textStrong').trim();
  const tooltipBorderColor = computedStyle.getPropertyValue('--color-borderWeak').trim();
  
  if (textColor) {
    opt.legend = opt.legend || {};
    opt.legend.textStyle = opt.legend.textStyle || {};
    opt.legend.textStyle.color = textColor;
  }
  
  // Configure tooltip styling
  opt.tooltip = opt.tooltip || {};
  opt.tooltip.backgroundColor = tooltipBg || (isDarkMode ? 'rgba(50, 50, 50, 0.9)' : 'rgba(255, 255, 255, 0.9)');
  opt.tooltip.textStyle = opt.tooltip.textStyle || {};
  opt.tooltip.textStyle.color = tooltipTextColor || (isDarkMode ? '#e0e0e0' : '#333');
  opt.tooltip.borderColor = tooltipBorderColor || (isDarkMode ? '#555' : '#ccc');
  opt.tooltip.borderWidth = 1;
  
  // Override server's background style with theme-appropriate one
  if (opt.series?.[0]?.backgroundStyle) {
    opt.series[0].backgroundStyle = isDarkMode ? DARK_BACKGROUND_STYLE : DEFAULT_BACKGROUND_STYLE;
  }
  
  chart.setOption(updateChartConfiguration(widgetData, opt, opt.dataset.source));

  // Expose threshold functionality on chart element
  (chartEl as any).applyThresholds = (thresholds: Record<string, number>) => applyThresholds(chart, thresholds);

  const resizeObserver = new ResizeObserver(() => requestAnimationFrame(() => (window as any).echarts.getInstanceByDom(chartEl).resize()));
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
      zlevel: 0
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
  const observer = new MutationObserver((mutations) => {
    mutations.forEach((mutation) => {
      if (mutation.type === 'attributes' && mutation.attributeName === 'data-theme') {
        const isDarkMode = document.body.getAttribute('data-theme') === 'dark';
        const newTheme = isDarkMode ? 'dark' : widgetData.theme || 'default';
        
        // Dispose and reinitialize chart with new theme
        chart.dispose();
        const newChart = (window as any).echarts.init(chartEl, newTheme);
        newChart.group = 'default';
        (window as any)[`${chartType}Chart`] = newChart;
        
        // Get CSS variable values from body (where theme is applied)
        const computedStyle = getComputedStyle(document.body);
        const textColor = computedStyle.getPropertyValue('--color-textWeak').trim();
        const tooltipBg = computedStyle.getPropertyValue('--color-bgRaised').trim();
        const tooltipTextColor = computedStyle.getPropertyValue('--color-textStrong').trim();
        const tooltipBorderColor = computedStyle.getPropertyValue('--color-borderWeak').trim();
        
        if (textColor) {
          opt.legend = opt.legend || {};
          opt.legend.textStyle = opt.legend.textStyle || {};
          opt.legend.textStyle.color = textColor;
        }
        
        // Configure tooltip styling
        opt.tooltip = opt.tooltip || {};
        opt.tooltip.backgroundColor = tooltipBg || (isDarkMode ? 'rgba(50, 50, 50, 0.9)' : 'rgba(255, 255, 255, 0.9)');
        opt.tooltip.textStyle = opt.tooltip.textStyle || {};
        opt.tooltip.textStyle.color = tooltipTextColor || (isDarkMode ? '#e0e0e0' : '#333');
        opt.tooltip.borderColor = tooltipBorderColor || (isDarkMode ? '#555' : '#ccc');
        opt.tooltip.borderWidth = 1;
        
        // Restore the chart with current options and data
        newChart.setOption(updateChartConfiguration(widgetData, opt, opt.dataset.source));
      }
    });
  });

  observer.observe(document.body, {
    attributes: true,
    attributeFilter: ['data-theme']
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
  items.forEach((el) => {
    if (!el.id || !el.id.endsWith('_widgetEl')) return;
    const widgetId = el.id.slice(0, -'_widgetEl'.length);
    const reorderItem: any = {
      x: el.gridstackNode.x,
      y: el.gridstackNode.y,
      w: el.gridstackNode.w,
      h: el.gridstackNode.h,
    };
    // Check for a nested grid within this grid item.
    const nestedGridContainer = el.querySelector('.nested-grid') as HTMLElement | null;
    if (nestedGridContainer) {
      const childOrder = buildWidgetOrder(nestedGridContainer);
      if (Object.keys(childOrder).length > 0) {
        reorderItem.children = childOrder;
      }
    }
    order[widgetId] = reorderItem;
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
  warning: { color: '#f59e0b', formatter: 'Warning: {c}' }
} as const;

// Create threshold markLines for ECharts
export const createThresholdMarkLines = (thresholds: Record<string, number>) =>
  Object.entries(thresholds)
    .filter(([_, value]) => !isNaN(value))
    .map(([type, value]) => ({
      yAxis: value,
      name: type,
      label: { formatter: THRESHOLDS[type as keyof typeof THRESHOLDS]?.formatter || `${type}: {c}`, position: 'end' },
      lineStyle: { color: THRESHOLDS[type as keyof typeof THRESHOLDS]?.color || '#999', width: 2, type: 'dashed' }
    }));

// Apply thresholds to a chart
export const applyThresholds = (chart: any, thresholds: Record<string, number>) => {
  const option = chart?.getOption();
  if (!option?.series?.length) return;
  
  chart.setOption({
    series: option.series.map((s: any) => ({
      ...s,
      markLine: { silent: true, symbol: 'none', data: createThresholdMarkLines(thresholds) }
    }))
  });
};
