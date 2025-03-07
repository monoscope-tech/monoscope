/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
;(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['exports', 'echarts'], factory)
  } else if (typeof exports === 'object' && typeof exports.nodeName !== 'string') {
    // CommonJS
    factory(exports, require('echarts/lib/echarts'))
  } else {
    // Browser globals
    factory({}, root.echarts)
  }
})(this, function (exports, echarts) {
  var log = function (msg) {
    if (typeof console !== 'undefined') {
      console && console.error && console.error(msg)
    }
  }
  if (!echarts) {
    log('ECharts is not Loaded')
    return
  }

  // Roma theme color palette
  var romaColorPalette = [
    '#E01F54',
    '#001852',
    '#f5e8c8',
    '#b8d2c7',
    '#c6b38e',
    '#a4d8c2',
    '#f3d999',
    '#d3758f',
    '#dcc392',
    '#2e4783',
    '#82b6e9',
    '#ff6347',
    '#a092f1',
    '#0a915d',
    '#eaf889',
    '#6699FF',
    '#ff6666',
    '#3cb371',
    '#d5b158',
    '#38b6b6',
  ]

  // Default theme color palette (ECharts default colors)
  var defaultColorPalette = [
    '#1A74A8',
    '#91cc75',
    '#fac858',
    '#ee6666',
    '#73c0de',
    '#3ba272',
    '#fc8452',
    '#9a60b4',
    '#ea7ccc',
    '#37a2da',
    '#32c5e9',
    '#67e0e3',
    '#9fe6b8',
    '#ffdb5c',
    '#ff9f7f',
    '#fb7293',
    '#e062ae',
    '#e690d1',
    '#e7bcf3',
    '#9d96f5',
  ]

  // Roma theme configuration
  var romaTheme = {
    color: romaColorPalette,
    visualMap: {
      color: ['#e01f54', '#e7dbc3'],
      textStyle: {
        color: '#333',
      },
    },
    candlestick: {
      itemStyle: {
        color: '#e01f54',
        color0: '#001852',
      },
      lineStyle: {
        width: 1,
        color: '#f5e8c8',
        color0: '#b8d2c7',
      },
      areaStyle: {
        color: '#a4d8c2',
        color0: '#f3d999',
      },
    },
    graph: {
      itemStyle: {
        color: '#a4d8c2',
      },
      linkStyle: {
        color: '#f3d999',
      },
    },
    gauge: {
      axisLine: {
        lineStyle: {
          color: [
            [0.2, '#E01F54'],
            [0.8, '#b8d2c7'],
            [1, '#001852'],
          ],
          width: 8,
        },
      },
    },
  }

  // Default theme configuration (with overrides)
  var defaultTheme = {
    color: defaultColorPalette,
    textStyle: {
      fontFamily: '-apple-system, BlinkMacSystemFont, Segoe UI, Roboto, Oxygen, Ubuntu, Cantarell, Fira Sans, Droid Sans, Helvetica Neue, sans-serif',
      fontSize: 12,
      color: '#333',
    },
    title: {
      textStyle: {
        color: '#333',
        fontSize: 16,
        fontWeight: 'bold',
      },
      subtextStyle: {
        color: '#999',
        fontSize: 14,
      },
    },
    line: {
      itemStyle: {
        borderWidth: 2,
      },
      lineStyle: {
        width: 2,
      },
      symbolSize: 4,
      symbol: 'emptyCircle',
      smooth: false,
    },
    radar: {
      itemStyle: {
        borderWidth: 2,
      },
      lineStyle: {
        width: 2,
      },
      symbolSize: 4,
      symbol: 'emptyCircle',
      smooth: false,
    },
    bar: {
      itemStyle: {
        barBorderWidth: 0,
        barBorderColor: '#ccc',
      },
    },
    pie: {
      itemStyle: {
        borderWidth: 0,
        borderColor: '#ccc',
      },
    },
    scatter: {
      itemStyle: {
        borderWidth: 0,
        borderColor: '#ccc',
      },
    },
    boxplot: {
      itemStyle: {
        borderWidth: 1,
        borderColor: '#ccc',
      },
    },
    parallel: {
      itemStyle: {
        borderWidth: 0,
        borderColor: '#ccc',
      },
    },
    sankey: {
      itemStyle: {
        borderWidth: 0,
        borderColor: '#ccc',
      },
    },
    funnel: {
      itemStyle: {
        borderWidth: 0,
        borderColor: '#ccc',
      },
    },
    gauge: {
      itemStyle: {
        borderWidth: 0,
        borderColor: '#ccc',
      },
    },
    candlestick: {
      itemStyle: {
        color: '#eb5454',
        color0: '#47b262',
        borderColor: '#eb5454',
        borderColor0: '#47b262',
        borderWidth: 1,
      },
    },
    graph: {
      itemStyle: {
        borderWidth: 0,
        borderColor: '#ccc',
      },
      lineStyle: {
        width: 1,
        color: '#aaa',
      },
      symbolSize: 4,
      symbol: 'emptyCircle',
      smooth: false,
      color: defaultColorPalette,
    },
    map: {
      itemStyle: {
        areaColor: '#eee',
        borderColor: '#444',
        borderWidth: 0.5,
      },
      label: {
        color: '#000',
      },
      emphasis: {
        itemStyle: {
          areaColor: 'rgba(255,215,0,0.8)',
          borderColor: '#444',
          borderWidth: 1,
        },
        label: {
          color: '#000',
        },
      },
    },
    geo: {
      itemStyle: {
        areaColor: '#eee',
        borderColor: '#444',
        borderWidth: 0.5,
      },
      label: {
        color: '#000',
      },
      emphasis: {
        itemStyle: {
          areaColor: 'rgba(255,215,0,0.8)',
          borderColor: '#444',
          borderWidth: 1,
        },
        label: {
          color: '#000',
        },
      },
    },
    categoryAxis: {
      axisLine: {
        show: true,
        lineStyle: {
          color: '#999',
        },
      },
      axisTick: {
        show: true,
        lineStyle: {
          color: '#999',
        },
      },
      axisLabel: {
        show: true,
        color: '#666',
      },
      splitLine: {
        show: false,
        lineStyle: {
          color: ['#eee'],
        },
      },
      splitArea: {
        show: false,
        areaStyle: {
          color: ['rgba(250,250,250,0.3)', 'rgba(200,200,200,0.3)'],
        },
      },
    },
    valueAxis: {
      axisLine: {
        show: true,
        lineStyle: {
          color: '#999',
        },
      },
      axisTick: {
        show: true,
        lineStyle: {
          color: '#999',
        },
      },
      axisLabel: {
        show: true,
        color: '#666',
      },
      splitLine: {
        show: true,
        lineStyle: {
          color: ['#eee'],
        },
      },
      splitArea: {
        show: false,
        areaStyle: {
          color: ['rgba(250,250,250,0.3)', 'rgba(200,200,200,0.3)'],
        },
      },
    },
    logAxis: {
      axisLine: {
        show: true,
        lineStyle: {
          color: '#999',
        },
      },
      axisTick: {
        show: true,
        lineStyle: {
          color: '#999',
        },
      },
      axisLabel: {
        show: true,
        color: '#666',
      },
      splitLine: {
        show: true,
        lineStyle: {
          color: ['#eee'],
        },
      },
      splitArea: {
        show: false,
        areaStyle: {
          color: ['rgba(250,250,250,0.3)', 'rgba(200,200,200,0.3)'],
        },
      },
    },
    timeAxis: {
      axisLine: {
        show: true,
        lineStyle: {
          color: '#999',
        },
      },
      axisTick: {
        show: true,
        lineStyle: {
          color: '#999',
        },
      },
      axisLabel: {
        show: true,
        color: '#666',
      },
      splitLine: {
        show: true,
        lineStyle: {
          color: ['#eee'],
        },
      },
      splitArea: {
        show: false,
        areaStyle: {
          color: ['rgba(250,250,250,0.3)', 'rgba(200,200,200,0.3)'],
        },
      },
    },
    toolbox: {
      iconStyle: {
        borderColor: '#999',
      },
      emphasis: {
        iconStyle: {
          borderColor: '#666',
        },
      },
    },
    legend: {
      textStyle: {
        color: '#333',
      },
    },
    tooltip: {
      axisPointer: {
        lineStyle: {
          color: '#ccc',
          width: 1,
        },
        crossStyle: {
          color: '#ccc',
          width: 1,
        },
      },
    },
    timeline: {
      lineStyle: {
        color: '#293c55',
        width: 1,
      },
      itemStyle: {
        color: '#293c55',
        borderWidth: 1,
      },
      controlStyle: {
        color: '#293c55',
        borderColor: '#293c55',
        borderWidth: 0.5,
      },
      checkpointStyle: {
        color: '#e43c59',
        borderColor: '#c23531',
      },
      label: {
        color: '#293c55',
      },
      emphasis: {
        itemStyle: {
          color: '#a9334c',
        },
        controlStyle: {
          color: '#293c55',
          borderColor: '#293c55',
          borderWidth: 0.5,
        },
        label: {
          color: '#293c55',
        },
      },
    },
    visualMap: {
      color: ['#1790cf', '#a2d4e6'],
    },
    dataZoom: {
      backgroundColor: 'rgba(47,69,84,0)',
      dataBackgroundColor: 'rgba(47,69,84,0.3)',
      fillerColor: 'rgba(167,183,204,0.4)',
      handleColor: '#a7b7cc',
      handleSize: '100%',
      textStyle: {
        color: '#333',
      },
    },
    markPoint: {
      label: {
        color: '#eee',
      },
      emphasis: {
        label: {
          color: '#eee',
        },
      },
    },
  }

  // Register both themes
  echarts.registerTheme('roma', romaTheme)
  echarts.registerTheme('default', defaultTheme)
})
