'use strict'

function durationFormatter(params) {
  let result = ''
  if (params.length > 0) {
    let dateV = params[0].axisValueLabel
    result += `<div>${dateV}</div>`
  }
  params.forEach(param => {
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

const SCROLL_BAR_WIDTH = 7
const getErrorIndicator = () =>
  elt(
    'span',
    {
      class: 'bg-red-600 rounded-l h-full w-5 flex justify-center items-center rounded-r shrink-0 font-bold',
    },
    elt(
      'span',
      {
        class: 'text-white text-xs h-3 w-3 flex items-center justify-center rounded-full border border-white p-1',
      },
      '!',
    ),
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
    filteredJson.forEach(item => {
      recur(item, item.start)
    })
    return data
  }

  const fData = modifySpansForFlameGraph(data)

  const heightOfJson = json => {
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
      class:
        item.itemStyle.color +
        ' absolute hover:z-999 flex rounded-sm items-center span-filterble cursor-pointer gap-1 flex-nowrap overflow-hidden hover:border hover:border-black',
      id: item.span_id,
      onclick: e => {
        e.stopPropagation()
        const data = filterJson(structuredClone(fData), item.name)
        flameGraph(data, renderAt)
        // const target = document.getElementById(item.span_id)
        // if (target) {
        //   target.scrollIntoView()
        // }
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
    sortedData.forEach(item => {
      renderItem(item, target, rootVal)
    })
  }

  flameGraph(fData, renderAt)

  const flameGraphContainer = document.querySelector('#flame-graph-container')

  flameGraphContainer.addEventListener('mousemove', e => {
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
  flameGraphContainer.addEventListener('mouseleave', e => {
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
  spans.forEach(span => {
    span.children = []
    spanMap.set(span.span_id, span)
  })
  const roots = []
  spans.forEach(span => {
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
  spans.forEach(span => {
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
    class: color + ' flex rounded-sm items-center cursor-pointer  h-5 grow-0 justify-between flex-nowrap overflow-x-visible hover:border hover:border-black',
    id: 'waterfall-chart-' + spanId,
    onclick: event => {
      event.stopPropagation()
      event.currentTarget.nextSibling.classList.toggle('hidden')
      const treeTarget = document.querySelector('#waterfall-tree-' + spanId)
      if (treeTarget) treeTarget.classList.toggle('hidden')
    },
  })
  parentDiv.style.marginLeft = `${startPix}px`
  div.style.width = `${width}px`
  const childDiv = elt('div', {
    class: 'flex flex-col gap-2 mt-2 gap-1',
    id: 'waterfall-child-' + spanId,
  })
  span.children.forEach(child => {
    childDiv.appendChild(buildTree(child, serviceColors, startCurr, rootVal, containerWidth))
  })
  const text = elt(
    'span',
    {
      class: 'text-black ml-1 shrink-0 mr-4 text-xs hidden',
    },
    span.spanRecord.serviceName + span.spanRecord.spanName,
  )
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
