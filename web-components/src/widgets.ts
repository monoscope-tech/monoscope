'use strict';

const DEFAULT_BACKGROUND_STYLE = { color: 'rgba(240,248,255, 0.4)' };
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

  const seriesOpt: any = {
    type: widgetData.chartType,
    name,
    stack: widgetData.chartType === 'line' ? undefined : widgetData.yAxisLabel || 'units',
    showSymbol: false,
    showBackground: true,
    backgroundStyle: DEFAULT_BACKGROUND_STYLE,
    barMaxWidth: '10',
    barMinHeight: '1',
    encode: { x: 0, y: i + 1 },
  };

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
    params.set('query', query);
    if (querySQL) params.set('query_sql', querySQL);

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
    subtitle && (subtitle.innerHTML = `${rows_per_min.toFixed(2)} rows/min`);

    const value = $(`${chartId}Value`);
    value && ((value.innerHTML = `${summarizeByPrefix} ${Number(stats[summarizeBy]).toLocaleString()}`), value.classList.remove('hidden'));

    chart.hideLoading();
    chart.setOption(updateChartConfiguration(widgetData, opt, opt.dataset.source), true);
  } catch (e) {
    console.error('Failed to fetch new data:', e);
  } finally {
    // Hide loader after fetch completes (success or failure)
    if (loader) loader.classList.add('hidden');
    if (borderedItem) borderedItem.classList.remove('spotlight-border');
  }
};

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
};

const chartWidget = (widgetData: WidGetData) => {
  const { chartType, opt, chartId, query, querySQL, theme } = widgetData,
    chartEl = $(chartId),
    chart = (window as any).echarts.init(chartEl, theme),
    liveStreamCheckbox = $('streamLiveData') as HTMLInputElement;
  let intervalId: NodeJS.Timeout | null = null;
  chart.group = 'default';

  // Store the original base query to avoid stacking
  const baseQuery = widgetData.query;
  widgetData.query = params().query ? (baseQuery ? params().query + ' | ' + baseQuery : params().query) : baseQuery;

  opt.dataset.source = opt.dataset?.source?.map((row: any) => [row[0] * 1000, ...row.slice(1)]) ?? null;

  const isLogExlorerPage = window.location.pathname.includes('/log_explorer');
  if (isLogExlorerPage) {
    (window as any)[`${chartType}Chart`] = chart;
  }

  chart.setOption(updateChartConfiguration(widgetData, opt, opt.dataset.source));

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
    chart.showLoading();
    new IntersectionObserver(
      (entries, observer) => entries[0]?.isIntersecting && (updateChartData(chart, opt, true, widgetData), observer.disconnect())
    ).observe(chartEl);
  }

  ['submit', 'add-query', 'update-query'].forEach((event) => {
    const selector = event === 'submit' ? '#log_explorer_form' : '#filterElement';
    document.querySelector(selector)?.addEventListener(event, (e) => {
      if (params().query) {
        widgetData.query = params().query ? (baseQuery ? params().query + ' | ' + baseQuery : params().query) : baseQuery;
      }
      if (window.logListTable) {
        console.log('wwwwwwwwwwwwwwwwwwwwwwwww-------------------');
        (window.logListTable as any).refetchLogs();
        console.log('lllllllllllllllll---------------');
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
};

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

function formatNumber(num: number) {
  if (Number.isInteger(num)) {
    return num.toString();
  } else {
    // toFixed returns a string with exactly 2 decimals, so parseFloat removes trailing zeros.
    return parseFloat(num.toFixed(2)).toString();
  }
}

// Update the widget order on the server.
const updateWidgetOrder = (projectId: string, dashboardId: string) => () => {
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

/**
 * Auto-refresh functionality for dashboards and widgets
 */
const DEFAULT_REFRESH_INTERVAL = 0; // Default to Off

// Global variable to store the refresh timer
window.dashboardRefreshTimer = null;
window.dashboardRefreshInterval = DEFAULT_REFRESH_INTERVAL;

/**
 * Sets up auto-refresh functionality for the page
 * @param {number} interval - Refresh interval in milliseconds (0 to disable)
 */
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
