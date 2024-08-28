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
  const fData = modifySpansForFlameGraph(data)
  const ColorTypes = {
    root: '#8fd3e8',
    genunix: '#d95850',
    unix: '#eb8146',
    ufs: '#ffb248',
    FSS: '#f2d643',
    namefs: '#ebdba4',
    doorfs: '#fcce10',
    lofs: '#b5c334',
    zfs: '#1bca93',
  }
  const filterJson = (json, id) => {
    if (id == null) {
      return json
    }
    const recur = (item, id) => {
      if (item.id === id) {
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
    const rootVal = filteredJson.value
    const recur = (item, start = 0, level = 0) => {
      const temp = {
        name: item.name,
        // [level, start_val, end_val, name, percentage]
        value: [level, start, start + item.value, item.name, (item.value / rootVal) * 100],
        itemStyle: {
          color: ColorTypes[item.name.split(' ')[0]],
        },
      }
      data.push(temp)
      let prevStart = start
      for (const child of item.children || []) {
        recur(child, prevStart, level + 1)
        prevStart = prevStart + child.value
      }
    }
    recur(filteredJson)
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
  const renderItem = (params, api) => {
    const level = api.value(0)
    const start = api.coord([api.value(1), level])
    const end = api.coord([api.value(2), level])
    const height = ((api.size && api.size([0, 1])) || [0, 20])[1]
    const width = end[0] - start[0]
    return {
      type: 'rect',
      transition: ['shape'],
      shape: {
        x: start[0],
        y: start[1] - height / 2,
        width,
        height: height - 2 /* itemGap */,
        r: 2,
      },
      style: {
        fill: api.visual('color'),
      },
      emphasis: {
        style: {
          stroke: '#000',
        },
      },
      textConfig: {
        position: 'insideLeft',
      },
      textContent: {
        style: {
          text: api.value(3),
          fontFamily: 'Verdana',
          fill: '#000',
          width: width - 4,
          overflow: 'truncate',
          ellipsis: '..',
          truncateMinChar: 1,
        },
        emphasis: {
          style: {
            stroke: '#000',
            lineWidth: 0.5,
          },
        },
      },
    }
  }
  myChart.showLoading()

  function flameGraph(stackTrace) {
    myChart.hideLoading()

    const levelOfOriginalJson = heightOfJson(stackTrace)
    option = {
      backgroundColor: {
        type: 'linear',
        x: 0,
        y: 0,
        x2: 0,
        y2: 1,
        colorStops: [
          {
            offset: 0.05,
            color: '#eee',
          },
          {
            offset: 0.95,
            color: '#eeeeb0',
          },
        ],
      },
      tooltip: {
        formatter: (params) => {
          const samples = params.value[2] - params.value[1]
          return `${params.marker} ${params.value[3]}: (${echarts.format.addCommas(samples)} samples, ${+params.value[4].toFixed(2)}%)`
        },
      },
      title: [
        {
          text: 'Flame Graph',
          left: 'center',
          top: 10,
          textStyle: {
            fontFamily: 'Verdana',
            fontWeight: 'normal',
            fontSize: 20,
          },
        },
      ],
      toolbox: {
        feature: {
          restore: {},
        },
        right: 20,
        top: 10,
      },
      xAxis: {
        show: false,
      },
      yAxis: {
        show: false,
        max: levelOfOriginalJson,
      },
      series: [
        {
          type: 'custom',
          renderItem,
          encode: {
            x: [0, 1, 2],
            y: 0,
          },
          data: recursionJson(stackTrace),
        },
      ],
    }
    myChart.setOption(option)
    myChart.on('click', (params) => {
      const data = recursionJson(stackTrace, params.data.name)
      const rootValue = data[0].value[2]
      myChart.setOption({
        xAxis: { max: rootValue },
        series: [{ data }],
      })
    })
  }
  console.log(fData)
  flameGraph(fData)
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
    if (span.parent_id != '') {
      const parent = spanMap.get(span.parent_id)
      if (parent) {
        parent.children.push(span)
      }
    } else {
      roots.push(span)
    }
  })
  return roots
}
