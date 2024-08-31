function throughputEChart(renderAt, data, gb, showLegend, theme) {
  let backgroundStyle = {
    color: 'rgba(240,248,255, 0.4)',
  }
  let series = {
    name: 'Throughput',
    type: 'bar',
    showBackground: true,
    backgroundStyle: backgroundStyle,
    barWidth: '60%',
    barMinHeight: '1',
    encode: {
      x: 'timestamp',
      y: 'throughput',
    },
    data: data,
  }
  if (gb.length > 0) {
    const newData = data.reduce((mp, curr) => {
      if (!mp[curr[1]]) mp[curr[1]] = []
      mp[curr[1]].push([curr[0], curr[2]])
      return mp
    }, {})
    series = Object.entries(newData).map(([k, v]) => ({
      name: k,
      type: 'bar',
      stack: 'Endpoints',
      showBackground: true,
      backgroundStyle: backgroundStyle,
      barWidth: '60%',
      encode: {
        x: 'timestamp',
        y: 'throughput',
      },
      data: v,
    }))
  }

  const myChart = echarts.init(document.getElementById(renderAt), theme)
  const option = {
    legend: { show: showLegend, type: 'scroll', top: 'bottom' },
    grid: {
      width: '100%',
      left: '0%',
      top: '5%',
      bottom: '1.8%',
      containLabel: true,
    },
    tooltip: {
      trigger: 'axis',
    },
    xAxis: { show: showLegend, type: 'time', scale: true },
    yAxis: { show: showLegend, scale: true },
    series: series,
  }
  if (showLegend) {
    option.grid.bottom = '9%'
  }
  console.log(option)
  myChart.setOption(option)
}

function stackedChart(title, series, _data, interp, width = 800, height = 400) {
  let { opts, data } = getStackedOpts(title, series, _data, interp)
  opts.title = title
  opts.width = width
  opts.height = height
  /*
    Object.assign(opts.scales.x, {
      ori: 1,
      dir: -1,
    });
    Object.assign(opts.scales.y, {
      ori: 0,
      dir: 1,
    });
  */
  return new uPlot(opts, data, document.body)
}

function defaultFormatter(params) {
  let result = ''
  if (params.length > 0) {
    let dateV = params[0].axisValueLabel
    result += `<div>${dateV}</div>`
  }
  params.forEach((param) => {
    // Check if data (value) is not zero or null
    if (param.value !== null && param.value[1] !== null) {
      result += `<div >
                            <div class="monospace flex flex-row space-between">
                                <div class="flex-1">${param.marker}${param.seriesName}</div>
                                <strong class="shrink pl-3 font-bold">${param.value[1]}</strong>
                              </div>
                        </div>`
    }
  })
  return result
}

function durationFormatter(params) {
  let result = ''
  if (params.length > 0) {
    let dateV = params[0].axisValueLabel
    result += `<div>${dateV}</div>`
  }
  params.forEach((param) => {
    let index = param.encode.y[0]
    // Check if data (value) is not zero or null
    if (param.value !== null && param.value[index] !== null) {
      let prettyVal = `${Math.trunc(param.value[index])}ms`
      if (param.value[index] > 1000) {
        prettyVal = `${Math.trunc(param.value[index] / 1000)}s`
      }
      result += `<div >
                            <div class="monospace flex flex-row space-between">
                                <div class="flex-1">${param.marker}${param.seriesName}</div>
                                <strong class="shrink pl-3 font-bold">${prettyVal}</strong>
                              </div>
                        </div>`
    }
  })
  return result
}

function throughputEChartTable(renderAt, categories, data, gb, showLegend, theme, from, to, chartType) {
  let backgroundStyle = {
    color: 'rgba(240,248,255, 0.4)',
  }
  let maxValue = 0
  const getSeriesData = (data) => {
    return data.slice(1).map((seriesData, index) => {
      return {
        name: categories[index],
        type: chartType,
        stack: 'Endpoints',
        showBackground: true,
        backgroundStyle: backgroundStyle,
        barMaxWidth: '10',
        barMinHeight: '1',
        encode: {
          x: 'timestamp',
          y: 'throughput',
        },
        data: seriesData.map((value, dataIndex) => {
          maxValue = value > maxValue ? value : maxValue
          return [data[0][dataIndex] * 1000, value]
        }),
      }
    })
  }

  let fmter = defaultFormatter
  if (chartType == 'line') {
    // Temporary workaround until js knows what kind of chart it is, or the units
    fmter = durationFormatter
  }

  const option = {
    tooltip: {
      trigger: 'axis',
      axisPointer: {
        type: 'shadow',
      },
      formatter: fmter,
    },
    legend: {
      show: showLegend,
      type: 'scroll',
      top: 'bottom',
      data: categories.slice(0, data.length - 1),
    },
    grid: {
      width: '100%',
      left: '0%',
      top: '2%',
      bottom: showLegend ? '22%' : '1.8%',
      containLabel: true,
    },
    xAxis: {
      show: true,
      scale: true,
      type: 'time',
      min: from,
      max: to,
      boundaryGap: [0, 0.01],
      axisLabel: {
        show: showLegend,
      },
    },
    yAxis: {
      show: showLegend,
      maxInterval: 3600 * 1000 * 24, // 1day
      type: 'value',
      min: 0,
      axisLine: {
        show: true,
      },
      axisLabel: {
        show: true,
      },
    },
    series: getSeriesData(data),
  }

  if (chartType == 'line') {
    option.yAxis.axisLabel = {
      formatter: function (params) {
        if (params >= 1000) {
          return `${Math.trunc(params / 1000)}s`
        }
        return `${Math.trunc(params)}ms`
      },
      show: true,
      position: 'inside',
    }
  } else {
    option.yAxis.axisLabel.formatter = function (params) {
      if (params >= 1000) {
        return `${Math.trunc(params / 1000)}k`
      }
      return `${Math.trunc(params)}`
    }
  }
  if (!showLegend) {
    option.yAxis.axisLabel = {
      formatter: function (value, index) {
        // Only show the label for the maximum value
        return value === maxValue ? maxValue : ''
      },
      show: true,
      // inside: true,
      showMaxLabel: true,
    }
  }

  const myChart = echarts.init(document.getElementById(renderAt), theme)
  myChart.setOption(option)
}

function latencyHistogram(renderAt, pc, data) {
  const myChart = echarts.init(document.getElementById(renderAt))
  const option = {
    grid: { width: '100%', left: '1%', right: '-1%', top: '10%', bottom: '1.5%', containLabel: true },
    xAxis: {
      show: true,
      type: 'value',
      scale: true,
      splitLine: { show: false },
      axisLabel: {
        formatter: function (params) {
          if (params > 1000) {
            return `${params / 1000}s`
          }
          return `${params}ms`
        },
        show: true,
        position: 'inside',
      },
    },
    yAxis: { show: true, type: 'value', scale: true },
    series: {
      name: 'Direct',
      type: 'bar',
      barWidth: '60%',
      data: data,
      markLine: {
        data: [
          [
            { name: 'p50', coord: [pc.p50, 0] },
            { name: 'end', coord: [pc.p50, 'max'] },
          ],
          [
            { name: 'p75', coord: [pc.p75, 0] },
            { name: 'end', coord: [pc.p75, 'max'] },
          ],
          [
            { name: 'p90', coord: [pc.p90, 0] },
            { name: 'end', coord: [pc.p90, 'max'] },
          ],
          [
            { name: 'p95', coord: [pc.p95, 0] },
            { name: 'end', coord: [pc.p95, 'max'] },
          ],
          [
            { name: 'p99', coord: [pc.p99, 0] },
            { name: 'end', coord: [pc.p99, 'max'] },
          ],
          [
            { name: 'max', coord: [pc.max, 0] },
            { name: 'end', coord: [pc.max, 'max'] },
          ],
        ],
      },
    },
  }
  myChart.setOption(option)
}

function flameGraphChart(data, renderAt) {
  const myChart = echarts.init(document.getElementById(renderAt))
  myChart.showLoading()
  const flameGraphColors = [
    '#FCA5A5', // Red-300
    '#FCD34D', // Amber-300
    '#FDBA74', // Orange-300
    '#FDE047', // Yellow-300
    '#BEF264', // Lime-300
    '#86EFAC', // Green-300
    '#5EEAD4', // Teal-300
    '#67E8F9', // Cyan-300
    '#93C5FD', // Blue-300
    '#D8B4FE', // Purple-300
  ]

  const filterJson = (json, id) => {
    if (id == null) {
      return json
    }
    if (Array.isArray(json)) {
      return json.filter((item) => item.name === id)
    }
    const recur = (item, id) => {
      if (item.name === id) {
        return item
      }
      for (const child of item.children || []) {
        const temp = recur(child, id)
        if (temp) {
          item.children = [temp]
          item.value = temp.value // change the parents' values
          return item
        }
      }
    }
    return recur(json, id) || json
  }
  const recursionJson = (jsonObj, id) => {
    const data = []
    const filteredJson = filterJson(structuredClone(jsonObj), id)
    const rootVal = filteredJson.sort((a, b) => b.value - a.value)[0].value || 1
    const recur = (item, start = 0, level = 0) => {
      const color = flameGraphColors[Math.floor(Math.random() * flameGraphColors.length)]
      const temp = {
        name: item.name,
        value: [level, item.start - start, item.value, item.name, (item.value / rootVal) * 100],
        itemStyle: {
          color,
        },
      }
      data.push(temp)
      for (const child of item.children || []) {
        recur(child, start, level + 1)
      }
    }
    filteredJson.forEach((item) => {
      recur(item, item.start)
    })
    return data
  }
  const heightOfJson = (json) => {
    const recur = (item, level = 0) => {
      if ((item.children || []).length === 0) {
        return level
      }
      let maxLevel = level
      for (const child of item.children) {
        const tempLevel = recur(child, level + 1)
        maxLevel = Math.max(maxLevel, tempLevel)
      }
      return maxLevel
    }
    return recur(json)
  }

  const renderItem = (item, renderAt, rootVal) => {
    const [level, xStart, xEnd] = item.value
    const container = document.getElementById(renderAt)

    if (!container) return

    const containerWidth = container.offsetWidth
    const startPix = (containerWidth * xStart) / rootVal
    const width = (containerWidth * xEnd) / rootVal

    const height = 25
    const yStart = height * level + (level + 1) * 3

    const div = elt('div', { class: 'absolute hover:z-[999] flex items-center justify-between flex-nowrap overflow-hidden hover:border hover:border-black' })
    div.style.left = `${startPix}px`
    div.style.top = `${yStart}px`
    div.style.width = `${width}px`
    div.style.height = `${height}px`
    div.style.backgroundColor = item.itemStyle.color

    const text = elt('span', { class: 'text-black ml-1 shrink-0 mr-4 text-xs' }, item.name)
    const [t, u] = formatDuration(item.value[2])
    const tim = elt('span', { class: 'text-black text-xs shrink-0' }, `${Math.floor(t)} ${u}`)
    div.appendChild(text)
    div.appendChild(tim)

    container.appendChild(div)
  }

  function flameGraph(stackTrace, target) {
    myChart.hideLoading()
    const rootVal = stackTrace.sort((a, b) => b.value - a.value)[0].value || 1
    generateTimeIntervals(rootVal, target)
    const data = recursionJson(stackTrace)

    // const canvas = document.getElementById('c-' + target)

    // canvas.width = 800
    // canvas.height = 400

    // const flameChart = new window.FlameChart({
    //   canvas, // mandatory
    //   data,
    //   marks: [
    //     {
    //       shortName: 'DCL',
    //       fullName: 'DOMContentLoaded',
    //       timestamp: 500,
    //     },
    //   ],
    //   waterfall: {
    //     /* ... */
    //   },
    //   timeseries: [
    //     /* ... */
    //   ],
    //   timeframeTimeseries: [
    //     /* ... */
    //   ],
    //   colors: {
    //     task: '#FFFFFF',
    //     'sub-task': '#000000',
    //   },
    //   settings: {
    //     hotkeys: {
    //       active: true, // enable navigation using arrow keys
    //       scrollSpeed: 0.5, // scroll speed (ArrowLeft, ArrowRight)
    //       zoomSpeed: 0.001, // zoom speed (ArrowUp, ArrowDown, -, +)
    //       fastMultiplayer: 5, // speed multiplier when zooming and scrolling (activated by Shift key)
    //     },
    //     options: {
    //       tooltip: () => {
    //         /*...*/
    //       }, // see section "Custom Tooltip" below
    //       timeUnits: 'ms',
    //     },
    //     styles: customStyles, // see section "Styles" below
    //   },
    // })

    data.forEach((item) => {
      renderItem(item, target, rootVal)
    })
  }

  const fData = modifySpansForFlameGraph(data)

  flameGraph(fData, renderAt)
}

function modifySpansForFlameGraph(data) {
  const spans = buildHierachy(JSON.parse(JSON.stringify(data)))
  return spans
}

function buildHierachy(spans) {
  const spanMap = new Map()
  spans.forEach((span) => {
    span.children = []
    spanMap.set(span.span_id, span)
  })
  const roots = []
  spans.forEach((span) => {
    if (span.parent_id) {
      const parent = spanMap.get(span.parent_id)
      if (parent) {
        parent.children.push(span)
      } else {
        roots.push(span)
      }
    } else {
      roots.push(span)
    }
  })
  return roots
}

function generateTimeIntervals(duration, target) {
  const container = document.getElementById('time-container-' + target)
  const [durationF, unit] = formatDuration(duration)
  container.innerHTML = ''
  const containerWidth = container.offsetWidth
  const intervalWidth = containerWidth / 11
  const intervals = []
  for (let i = 0; i < 12; i++) {
    const time = Math.floor((i * durationF) / 11)
    intervals.push(`
              <div class="absolute bottom-0 text-gray-700 border-left" style="width: ${intervalWidth}px; left: ${i * intervalWidth}px;">
               <div class="relative" style="height:10px">
                <div class="bg-gray-300"  style="width:1px; height:10px;"></div>
                <span class="absolute  left-0 -translate-x-1/2 text-xs" style="top:-13px">${time} ${unit}</span>
               </div>
              </div>
      `)
  }

  container.innerHTML = intervals.join('')
}

function formatDuration(duration) {
  if (duration >= 1000000000) {
    return [(duration / 1000000000).toFixed(2), 's']
  } else if (duration >= 1000000) {
    return [(duration / 1000000).toFixed(2), 'ms']
  } else if (duration >= 1000) {
    return [(duration / 1000).toFixed(2), 'µs']
  } else {
    return [duration, 'ns']
  }
}

function elt(type, props, ...children) {
  let dom = document.createElement(type)
  if (props) {
    for (let prop in props) {
      if (prop === 'class') {
        dom.className = props[prop]
      } else if (prop.startsWith('on') && typeof props[prop] === 'function') {
        const eventName = prop.substring(2).toLowerCase()
        dom.addEventListener(eventName, props[prop])
      } else {
        dom.setAttribute(prop, props[prop])
      }
    }
  }
  for (let child of children) {
    if (typeof child != 'string') dom.appendChild(child)
    else dom.appendChild(document.createTextNode(child))
  }
  return dom
}
