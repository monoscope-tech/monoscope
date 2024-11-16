'use strict'

function throughputEChart(renderAt, data, gb, showLegend, showAxes, theme) {
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
    xAxis: { show: showAxes, type: 'time', scale: true },
    yAxis: { show: showAxes, scale: true },
    series: series,
  }
  if (showLegend) {
    option.grid.bottom = '9%'
  }
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

function throughputEChartTable(renderAt, categories, data, gb, showLegend, showAxes, theme, from, to, chartType) {
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
        show: showAxes,
      },
    },
    yAxis: {
      show: showAxes,
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
  if (!showAxes) {
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

const SCROLL_BAR_WIDTH = 7
const getErrorIndicator = () =>
  elt(
    'span',
    { class: 'bg-red-600 rounded-l h-full w-5 flex justify-center items-center rounded-r shrink-0 font-bold' },
    elt('span', { class: 'text-white text-xs h-3 w-3 flex items-center justify-center rounded-full border border-white p-1' }, '!')
  )
function flameGraphChart(data, renderAt, colorsMap) {
  const filterJson = (json, id) => {
    if (id == null) {
      return json
    }
    if (Array.isArray(json)) {
      for (const item of json) {
        const data = filterJson(item, id)
        if (data.length > 0) {
          return data
        }
      }
    }
    const recur = (item, id) => {
      if (item.name === id) {
        return [item]
      }
      for (const child of item.children || []) {
        const temp = recur(child, id)
        if (temp && temp.length > 0) {
          const temp2 = temp[0]
          temp.start = 0
          item.children = [temp2]
          item.value = temp2.value // change the parents' values
          item.start = temp2.start
          return [item]
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
      const color = colorsMap[item.service_name] || 'bg-black'
      const temp = {
        name: item.name,
        hasErrors: item.has_errors,
        span_id: item.span_id,
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

  const fData = modifySpansForFlameGraph(data)

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
    const container = document.querySelector('#' + renderAt)

    if (!container) return
    const containerWidth = container.offsetWidth - SCROLL_BAR_WIDTH
    const startPix = (containerWidth * xStart) / rootVal
    const width = (containerWidth * xEnd) / rootVal
    const height = 20
    const yStart = height * level + (level + 1) * 3

    const div = elt('div', {
      class: item.itemStyle.color + ' absolute hover:z-[999] flex rounded items-center span-filterble cursor-pointer gap-1 flex-nowrap overflow-hidden hover:border hover:border-black',
      id: item.span_id,
      onclick: (e) => {
        const data = filterJson(structuredClone(fData), item.name)
        flameGraph(data, renderAt)
        const target = document.getElementById(item.span_id)
        if (target) {
          target.scrollIntoView()
        }
        htmx.trigger('#trigger-span-' + item.span_id, 'click')
      },
    })
    div.style.left = `${startPix}px`
    div.style.top = `${yStart}px`
    div.style.width = `${width}px`
    div.style.height = `${height}px`
    if (item.hasErrors) {
      div.appendChild(getErrorIndicator())
    }
    const text = elt('span', { class: 'text-black shrink-0 mr-4 text-xs' }, item.name)
    const [t, u] = formatDuration(item.value[2])
    const tim = elt('span', { class: 'text-black text-xs shrink-0 ml-auto' }, `${Math.floor(t)} ${u}`)
    div.appendChild(text)
    div.appendChild(tim)
    container.appendChild(div)
  }

  let maxDuration = 0
  function flameGraph(stackTrace, target) {
    const container = document.querySelector('#' + target)
    container.innerHTML = ''
    const rootVal = stackTrace.sort((a, b) => b.value - a.value)[0].value || 1
    maxDuration = rootVal
    generateTimeIntervals(rootVal, 'time-container')
    const data = recursionJson(stackTrace)
    const sortedData = data.sort((a, b) => b.value[2] - a.value[2])
    sortedData.forEach((item) => {
      renderItem(item, target, rootVal)
    })
  }

  flameGraph(fData, renderAt)

  const flameGraphContainer = document.querySelector('#flame-graph-container')

  flameGraphContainer.addEventListener('mousemove', (e) => {
    const boundingX = e.currentTarget.getBoundingClientRect().x
    const lineContainer = document.querySelector('#time-bar-indicator')
    const time = document.querySelector('#line-time')
    const container = document.querySelector('#time-container')
    if (container) {
      const left = e.clientX - boundingX
      lineContainer.style.left = `${left}px`
      const containerWidth = container.offsetWidth - SCROLL_BAR_WIDTH
      const currTime = (maxDuration * (left - 8)) / containerWidth
      const [f, u] = formatDuration(currTime)
      time.textContent = `${f}${u}`
      if (left < 9 || left > containerWidth + 8) {
        lineContainer.style.display = 'none'
      } else {
        lineContainer.style.display = 'block'
      }
    }
  })
  flameGraphContainer.addEventListener('mouseleave', (e) => {
    const lineContainer = document.querySelector('#time-bar-indicator')
    lineContainer.style.display = 'none'
  })
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
  const container = document.querySelector('#' + target)
  container.innerHTML = ''
  const containerWidth = target === 'waterfall-time-container' ? 550 : container.offsetWidth - SCROLL_BAR_WIDTH
  const intervalWidth = containerWidth / 9
  const intervals = []
  for (let i = 0; i < 10; i++) {
    const t = Math.floor((i * duration) / 9)
    let [durationF, unit] = formatDuration(t)
    const time = durationF
    unit = t === 0 ? '' : unit
    intervals.push(`
              <div class="absolute bottom-0 text-gray-700 border-left overflow-x-visible" style="width: ${intervalWidth}px; left: ${i * intervalWidth}px;">
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
    return [(duration / 1000000000).toFixed(1), 's']
  } else if (duration >= 1000000) {
    return [(duration / 1000000).toFixed(1), 'ms']
  } else if (duration >= 1000) {
    return [(duration / 1000).toFixed(1), 'µs']
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

function waterFallGraphChart(fData, renderAt, serviceColors) {
  const { min, max } = getMinMax(fData)
  const maxDuration = max - min
  generateTimeIntervals(maxDuration, 'waterfall-time-container')
  buildWaterfall(fData, renderAt, serviceColors, min, maxDuration)
}

function buildWaterfall(spans, target, serviceColors, start, maxDuration) {
  const container = document.querySelector('#' + target)
  const containerWidth = 550
  container.innerHTML = ''
  spans.forEach((span) => {
    container.appendChild(buildTree(span, serviceColors, start, maxDuration, containerWidth))
  })
}

function buildTree(span, serviceColors, start, rootVal, containerWidth) {
  const startCurr = span.spanRecord.startTime
  const st = startCurr - start
  const startPix = (containerWidth * st) / rootVal
  const width = (containerWidth * span.spanRecord.spanDurationNs) / rootVal
  const parentDiv = elt('div', {
    class: 'flex flex-col span-filterble',
  })
  const spanId = span.spanRecord.spanId
  const color = serviceColors[span.spanRecord.serviceName] || 'bg-black'
  const div = elt('div', {
    class: color + ' flex rounded items-center cursor-pointer  h-5 grow-0 justify-between flex-nowrap overflow-x-visible hover:border hover:border-black',
    id: 'waterfall-chart-' + spanId,
    onclick: (event) => {
      event.stopPropagation()
      event.currentTarget.nextSibling.classList.toggle('hidden')
      const treeTarget = document.querySelector('#waterfall-tree-' + spanId)
      if (treeTarget) treeTarget.classList.toggle('hidden')
    },
  })
  parentDiv.style.marginLeft = `${startPix}px`
  div.style.width = `${width}px`
  const childDiv = elt('div', { class: 'flex flex-col gap-2 mt-2 gap-1', id: 'waterfall-child-' + spanId })
  span.children.forEach((child) => {
    childDiv.appendChild(buildTree(child, serviceColors, startCurr, rootVal, containerWidth))
  })
  const text = elt('span', { class: 'text-black ml-1 shrink-0 mr-4 text-xs hidden' }, span.spanRecord.serviceName + span.spanRecord.spanName)
  const [t, u] = formatDuration(span.spanRecord.spanDurationNs)
  const tim = elt('span', { class: 'text-black text-xs shrink-0 ml-auto' }, `${Math.floor(t)} ${u}`)
  if (span.spanRecord.hasErrors) {
    div.appendChild(getErrorIndicator())
  }
  div.appendChild(text)
  div.appendChild(tim)
  parentDiv.appendChild(div)
  if (span.children.length > 0) {
    parentDiv.appendChild(childDiv)
  }
  return parentDiv
}

function getMinMax(arr) {
  let min = Infinity
  let max = -Infinity
  function traverse(array) {
    for (let i = 0; i < array.length; i++) {
      const nano = array[i].spanRecord.startTime
      const nano2 = array[i].spanRecord.endTime
      if (nano < min) min = nano
      if (nano2 > max) max = nano2
      if (array[i].children) traverse(array[i].children)
    }
  }
  traverse(arr)
  return { min, max }
}
