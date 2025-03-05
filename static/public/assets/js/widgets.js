'use strict'

const DEFAULT_BACKGROUND_STYLE = { color: 'rgba(240,248,255, 0.4)' }
const INITIAL_FETCH_INTERVAL = 5000
const $ = id => document.getElementById(id)

const createSeriesConfig = (chartType, name, i, yAxisLabel) => ({
  type: chartType,
  name,
  stack: chartType === 'line' ? undefined : yAxisLabel || 'units',
  showSymbol: false,
  showBackground: true,
  backgroundStyle: DEFAULT_BACKGROUND_STYLE,
  barMaxWidth: '10',
  barMinHeight: '1',
  encode: { x: 0, y: i + 1 },
})

const updateChartConfiguration = ({ chartType, yAxisLabel }, opt, data) => {
  if (!data) return opt
  const cols = data[0]?.slice(1)
  opt.series = cols?.map((n, i) => createSeriesConfig(chartType, n, i, yAxisLabel))
  opt.legend.data = cols
  return opt
}

const updateChartData = async (chart, opt, shouldFetch, widgetData) => {
  if (!shouldFetch) return
  try {
    const { query, querySQL, queryAST, pid, chartId, summarizeBy, summarizeByPrefix } = widgetData,
      params = new URLSearchParams(window.location.search)
    params.set('pid', pid)
    params.set('query_raw', query)
    params.set('queryAST', JSON.stringify(queryAST))
    if (querySQL) params.set('query_sql', querySQL)

    const { from, to, headers, dataset, rows_per_min, stats } = await fetch(`/chart_data?${params}`).then(res => res.json())

    opt.xAxis = opt.xAxis || {}
    opt.xAxis.min = from * 1000
    opt.xAxis.max = to * 1000
    opt.dataset.source = [headers, ...dataset.map(row => [row[0] * 1000, ...row.slice(1)])]

    $(`${chartId}Subtitle`).innerHTML = `${rows_per_min.toFixed(2)} rows/min`
    $(`${chartId}Value`).innerHTML = `${summarizeByPrefix} ${Number(stats[summarizeBy]).toLocaleString()}`
    $(`${chartId}Value`).classList.remove('hidden')

    chart.hideLoading()
    chart.setOption(updateChartConfiguration(widgetData, opt, opt.dataset.source))
  } catch (e) {
    console.error('Failed to fetch new data:', e)
  }
}

/**
 * Initializes a chart widget.
 * @param {Object} widgetData - Chart config.
 * @property {string} widgetData.chartType
 * @property {Object} widgetData.opt - ECharts options.
 * @property {string} widgetData.chartId - DOM id.
 * @property {string} widgetData.query - Data query.
 * @property {string} widgetData.sql - Data query.
 * @property {string} [widgetData.querySQL]
 * @property {string} widgetData.theme - ECharts theme.
 * @property {string} widgetData.yAxisLabel
 * @property {string} widgetData.pid
 * @property {string} widgetData.summarizeBy
 * @property {string} widgetData.summarizeByPrefix
 */
const chartWidget = widgetData => {
  const { chartType, opt, chartId, query, querySQL, theme } = widgetData,
    chartEl = $(chartId),
    chart = echarts.init(chartEl, theme),
    liveStreamCheckbox = $('streamLiveData')
  let intervalId = null
  chart.group = 'default'

  if (params().queryAST) {
    widgetData.queryAST = JSON.parse(params().queryAST)
    console.log('widgetData ', widgetData)
  }

  opt.dataset.source = opt.dataset?.source?.map(row => [row[0] * 1000, ...row.slice(1)]) ?? null
  chart.setOption(updateChartConfiguration(widgetData, opt, opt.dataset.source))

  const resizeObserver = new ResizeObserver(() => requestAnimationFrame(() => echarts.getInstanceByDom(chartEl).resize()))
  resizeObserver.observe(chartEl)

  liveStreamCheckbox &&
    liveStreamCheckbox.addEventListener('change', () =>
      liveStreamCheckbox.checked
        ? (intervalId = setInterval(() => updateChartData(chart, opt, true, widgetData), INITIAL_FETCH_INTERVAL))
        : (clearInterval(intervalId), (intervalId = null)),
    )

  if (!opt.dataset.source) {
    chart.showLoading()
    new IntersectionObserver(
      (entries, observer) => entries[0]?.isIntersecting && (updateChartData(chart, opt, true, widgetData), observer.disconnect()),
    ).observe(chartEl)
  }

  ;['submit', 'add-query', 'update-query'].forEach(event => {
    const selector = event === 'submit' ? '#log_explorer_form' : '#filterElement'
    document.querySelector(selector)?.addEventListener(event, e => {
      if (e.detail?.ast) {
        widgetData.queryAST = e.detail.ast
      }
      updateChartData(chart, opt, true, widgetData)
    })
  })

  window.addEventListener('unload', () => (clearInterval(intervalId), resizeObserver.disconnect()))
}
