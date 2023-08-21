function throughputEChart(renderAt, data, gb, showLegend, theme) {
  let backgroundStyle = {
    color: 'rgba(240,248,255, 0.4)'
  }
  let series = {
    name: "Throughput",
    type: 'bar',
    showBackground: true,
    backgroundStyle: backgroundStyle,
    barWidth: '60%',
    barMinHeight: "1",
    encode: {
      x: 'timestamp',
      y: 'throughput',
    },
    data: data,
  }
  if (gb.length > 0) {
    const newData = data.reduce((mp, curr) => {
      if (!mp[curr[1]]) mp[curr[1]] = [];
      mp[curr[1]].push([curr[0], curr[2]]);
      return mp;
    }, {});
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
    }));
  }

  const myChart = echarts.init(document.getElementById(renderAt), theme);
  const option = {
    legend: { show: showLegend, type: 'scroll', top: 'bottom' },
    grid: {
      width: '100%',
      left: '0%',
      top: '5%',
      bottom: '1.8%',
      containLabel: true
    },
    tooltip: {
      trigger: 'axis',
    },
    xAxis: { show: showLegend, type: 'time', scale: true },
    yAxis: { show: showLegend, scale: true },
    series: series,
  };
  if (showLegend) {
    option.grid.bottom = '9%'
  }
  console.log(option)
  myChart.setOption(option);
}


function stackedChart(title, series, _data, interp, width = 800, height = 400) {
  let { opts, data } = getStackedOpts(title, series, _data, interp);
  opts.title = title;
  opts.width = width;
  opts.height = height;
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
  return new uPlot(opts, data, document.body);
}

function throughputEChartTable(renderAt, categories, data, gb, showLegend, theme) {
  let backgroundStyle = {
    color: 'rgba(240,248,255, 0.4)'
  }
  const getSeriesData = (data) => {
    return data.slice(1).map((seriesData, index) => {
      return {
        name: categories[index],
        type: 'bar',
        stack: "Endpoints",
        showBackground: true,
        backgroundStyle: backgroundStyle,
        barWidth: '60%',
        data: seriesData.map((value, dataIndex) => [data[0][dataIndex] * 1000, value])
      };
    });
  };

  const option = {
    tooltip: {
      trigger: 'axis',
      axisPointer: {
        type: 'shadow'
      },
      formatter: function(params) {
        let result = '';
        params.forEach(param => {
          // Check if data (value) is not zero or null
          if (param.value !== null && param.value[1] !== null) {
            result += `<div >
                    <div class="monospace flex flex-row space-between">
                                  <div class="flex-1">${param.marker}${param.seriesName}:</div>
                                  <strong class="shrink pl-3 font-bold">${param.value[1]}</strong>
                                </div>
                          </div>`;
          }
        });

        return result;
      }
    },
    legend: {
      type: 'scroll',
      top: 'bottom',
      data: categories.slice(0, data.length - 1)
    },
    grid: {
      width: '100%',
      left: '0%',
      top: '5%',
      bottom: '9%',
      containLabel: true
    },
    xAxis: {
      type: 'time',
      boundaryGap: [0, 0.01],
    },
    yAxis: {
      type: 'value',
      show: true,
      min: 0
    },
    series: getSeriesData(data)
  };

  const myChart = echarts.init(document.getElementById(renderAt), theme);
  myChart.setOption(option);
}

function throughputEChartTable3(renderAt, categories, _data, gb, showLegend, theme) {
  const series = categories.map(x => ({ type: 'bar', label: x, width: 2 }));
  series.unshift({});
  console.dir(series)
  // Prepend empty string for the header row
  categories.unshift("");
  console.table(_data);
  const tableData = _data.map((row, index) => [categories[index], ...row]);
  console.table(tableData);

  const opts = {
    title: "My Chart",
    id: "chart1",
    class: "my-chart",
    width: 800,
    height: 600,
    series: series
  }
  console.dir(opts)

  return new uPlot(opts, _data, document.getElementById(renderAt));
}

function throughputEChartTable2(renderAt, categories, _data, gb, showLegend, theme) {
  const series = categories.map(_ => ({ type: 'bar' }));
  console.dir(series)
  // Prepend empty string for the header row
  categories.unshift("");

  const tableData = _data.map((row, index) => [categories[index], ...row]);
  console.table(tableData);
  let { opts, data } = getStackedOpts("Bars stacked", series, tableData, null);
  opts.title = "Bar Stacked";
  opts.width = 1600;
  opts.height = 400;
  console.log("uPlot called")
  return new uPlot(opts, data, document.getElementById(renderAt));
}

function throughputEChartTable2(renderAt, categories, data, gb, showLegend, theme) {
  const series = categories.map(_ => ({ type: 'bar' }));
  console.dir(series)
  // Prepend empty string for the header row
  categories.unshift("");

  const normalizedData = data.map((row, index) => [categories[index], ...row]);
  console.log(normalizedData)

  const myChart = echarts.init(document.getElementById(renderAt), theme);
  const option = {
    dataset: {
      source: normalizedData
    },
    legend: { show: showLegend, type: 'scroll', top: 'bottom', data: categories.slice(0, data.length - 1) },
    grid: {
      width: '100%',
      left: '0%',
      top: '5%',
      bottom: '1.8%',
      containLabel: true
    },
    tooltip: {
      trigger: 'axis',
    },
    xAxis: { show: showLegend, type: 'time' },
    yAxis: { show: showLegend, type: 'value' },
    series: series,
  };
  if (showLegend) {
    option.grid.bottom = '9%'
  }
  console.log(option)
  myChart.setOption(option);
}


function latencyEChart(renderAt, data, theme) {
  const showLegend = true;
  const option = {
    tooltip: {
      trigger: 'axis'
    },
    legend: { show: showLegend, type: 'scroll', top: 'bottom' },
    xAxis: {
      type: 'category',
      scale: true
    },
    yAxis: {
      type: 'value',
      scale: true,
      boundaryGap: ['5%', '5%'],
      axisLabel: {
        formatter: function(params) {
          if (params > 1000) {
            return `$${params / 1000}s`
          }
          return `$${params}ms`
        },
        show: true,
        position: 'inside'
      }
    },
    grid: {
      width: '100%',
      left: '0%',
      top: '5%',
      bottom: '12%',
      containLabel: true
    },
    dataset: {
      dimensions: ['date', 'p50', 'p75', 'p90'],
      source: data
    },
    series: [
      {
        type: 'line',
        name: 'p50',
        encode: {
          x: 'date',
          y: 'p50'
        },
        smooth: true
      },
      {
        type: 'line',
        name: 'p75',
        encode: {
          x: 'date',
          y: 'p75'
        },
        smooth: true
      },
      {
        type: 'line',
        name: 'p90',
        encode: {
          x: 'date',
          y: 'p90'
        },
        smooth: true
      }
    ]
  };
  const myChart = echarts.init(document.getElementById(renderAt), theme);
  myChart.setOption(option);
}


function latencyHistogram(renderAt, pc, data) {
  const myChart = echarts.init(document.getElementById(renderAt));
  const option = {
    grid: { width: '100%', width: '100%', left: '1%', right: '-1%', top: '10%', bottom: '1.5%', containLabel: true },
    xAxis: {
      show: true, type: 'value', scale: true, splitLine: { show: false },
      axisLabel: {
        formatter: function(params) {
          if (params > 1000) {
            return `$${params / 1000}s`
          }
          return `$${params}ms`
        },
        show: true,
        position: 'inside'
      },
    },
    yAxis: { show: true, type: 'value', scale: true, },
    series: {
      name: 'Direct',
      type: 'bar',
      barWidth: '60%',
      data: data,
      markLine: {
        data: [
          [
            { name: "p50", coord: [pc.p50, 0] },
            { name: "end", coord: [pc.p50, 'max'] },
          ],
          [
            { name: "p75", coord: [pc.p75, 0] },
            { name: "end", coord: [pc.p75, 'max'] },
          ],
          [
            { name: "p90", coord: [pc.p90, 0] },
            { name: "end", coord: [pc.p90, 'max'] },
          ],
          [
            { name: "p95", coord: [pc.p95, 0] },
            { name: "end", coord: [pc.p95, 'max'] },
          ],
          [
            { name: "p99", coord: [pc.p99, 0] },
            { name: "end", coord: [pc.p99, 'max'] },
          ],
          [
            { name: "max", coord: [pc.max, 0] },
            { name: "end", coord: [pc.max, 'max'] },
          ]
        ],
      },
    },
  };
  myChart.setOption(option);
}
