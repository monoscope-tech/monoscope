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
  var defaultTheme = pallete => ({
    color: pallete,
    visualMap: {
      color: ['#e01f54', '#e7dbc3'],
      textStyle: {
        color: '#333',
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
  })

  // Register both themes
  echarts.registerTheme('roma', defaultTheme(romaColorPalette))
  echarts.registerTheme('default', defaultTheme(defaultColorPalette))
})
