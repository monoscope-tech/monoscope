import { getSeriesColor, tailwindToHex } from './colorMapping';

const SCROLL_BAR_WIDTH = 7;

// Simple debounce utility
function debounce<T extends (...args: any[]) => any>(func: T, wait: number): T {
  let timeout: NodeJS.Timeout;
  return ((...args: Parameters<T>) => {
    clearTimeout(timeout);
    timeout = setTimeout(() => func(...args), wait);
  }) as T;
}

const getErrorIndicator = () =>
  elt(
    'span',
    {
      class: 'bg-fillError-strong rounded-l h-full w-5 flex justify-center items-center rounded-r shrink-0 font-bold',
    },
    elt(
      'span',
      {
        class: 'text-textInverse-strong text-xs h-3 w-3 flex items-center justify-center rounded-full border border-white p-1',
      },
      '!'
    )
  );

type FlameGraphItem = {
  spanId: string;
  value: number;
  start: number;
  parentId: string;
  name: string;
  serviceName: string;
  hasErrors: boolean;
  children: FlameGraphItem[];
};

type ItemsWithStyle = Pick<FlameGraphItem, 'name' | 'hasErrors' | 'spanId'> & {
  itemStyle: { color: string };
  value: [number, number, number, string, number];
};

export function flameGraphChart(data: FlameGraphItem[], renderAt: string, colorsMap: Record<string, string>) {
  const filterJson = (json: FlameGraphItem | FlameGraphItem[], id: string | null = null): FlameGraphItem[] => {
    if (id == null) {
      if (Array.isArray(json)) return json;
      return [json];
    }
    if (Array.isArray(json)) {
      for (const item of json) {
        const data = filterJson(item, id);
        if (data.length > 0) {
          return data;
        }
      }
    } else {
      const recur = (item: FlameGraphItem, id: string) => {
        if (item.name === id) {
          return [item];
        }
        for (const child of item.children) {
          const temp = recur(child, id);
          if (temp && temp.length > 0) {
            const temp2 = temp[0];
            item.children = [temp2];
            item.value = temp2.value; // change the parents' values
            item.start = temp2.start;
            return [item];
          }
        }
        return [];
      };
      return recur(json, id) || json;
    }
    return [];
  };
  const recursionJson = (jsonObj: FlameGraphItem[], id: string | null = null) => {
    const data: ItemsWithStyle[] = [];
    const filteredJson = filterJson(structuredClone(jsonObj), id);
    const rootVal = filteredJson.sort((a, b) => b.value - a.value)[0]?.value || 1;
    const recur = (item: FlameGraphItem, start = 0, level = 0) => {
      // Use deterministic color based on service name
      const tailwindColor = colorsMap[item.serviceName] || getSeriesColor(item.serviceName, 'service');
      const color = tailwindColor.startsWith('#') ? tailwindColor : tailwindToHex(tailwindColor);
      const temp: ItemsWithStyle = {
        name: item.name,
        hasErrors: item.hasErrors,
        spanId: item.spanId,
        value: [level, item.start - start, item.value, item.name, (item.value / rootVal) * 100],
        itemStyle: {
          color,
        },
      };
      data.push(temp);
      for (const child of item.children || []) {
        recur(child, start, level + 1);
      }
    };
    filteredJson.forEach((item) => {
      recur(item, item.start);
    });
    return data;
  };

  const fData = modifySpansForFlameGraph(data);

  const renderItem = (item: ItemsWithStyle, renderAt: string, rootVal: number, containerWidth?: number) => {
    const [level, xStart, xEnd] = item.value;

    // Get container - we need it for appending the element
    const container = document.querySelector('#a' + renderAt) as HTMLElement;
    if (!container) return;

    // Use cached containerWidth if provided, otherwise calculate it
    const actualWidth = containerWidth || container.offsetWidth - SCROLL_BAR_WIDTH;

    const startPix = (actualWidth * xStart) / rootVal;
    const width = (actualWidth * xEnd) / rootVal;
    const height = 20;
    const yStart = height * level + (level + 1) * 3;

    const div = elt('div', {
      class:
        item.itemStyle.color +
        ' absolute hover:z-999 flex rounded-sm items-center span-filterble cursor-pointer gap-1 flex-nowrap overflow-hidden hover:border hover:border-black',
      id: item.spanId,
      style: `background-color: ${item.itemStyle.color};`,
      onclick: (e: any) => {
        e.stopPropagation();
        const data = filterJson(structuredClone(fData), item.name);
        flameGraph(data, renderAt);
        // const target = document.getElementById(item.span_id)
        // if (target) {
        //   target.scrollIntoView()
        // }
        (window as any).htmx.trigger('#trigger-span-' + item.spanId, 'click');
      },
    });
    div.style.left = `${startPix}px`;
    div.style.top = `${yStart}px`;
    div.style.width = `${width}px`;
    div.style.height = `${height}px`;
    if (item.hasErrors) {
      div.appendChild(getErrorIndicator());
    }
    const text = elt('span', { class: 'text-black shrink-0 mr-4 text-xs' }, item.name);
    const [t, u] = formatDuration(item.value[2]);
    const tim = elt('span', { class: 'text-black text-xs shrink-0 ml-auto' }, `${Math.floor(Number(t))} ${u}`);
    div.appendChild(text);
    div.appendChild(tim);
    container.appendChild(div);
  };

  let maxDuration = 0;
  function flameGraph(stackTrace: FlameGraphItem[], target: string) {
    const container = document.querySelector('#a' + target);
    if (container) {
      container.innerHTML = '';
      const rootVal = stackTrace.sort((a, b) => b.value - a.value)[0].value || 1;
      maxDuration = rootVal;
      generateTimeIntervals(rootVal, 'time-container-' + target);

      // Cache container width to avoid multiple reflows
      const containerWidth = container.offsetWidth - SCROLL_BAR_WIDTH;
      const data = recursionJson(stackTrace);
      const sortedData = data.sort((a, b) => b.value[2] - a.value[2]);
      sortedData.forEach((item) => {
        renderItem(item, target, rootVal, containerWidth);
      });
    }
  }

  flameGraph(fData, renderAt);

  const flameGraphContainer = document.querySelector('#flame-graph-container-' + renderAt)!;

  // Cache DOM elements and values outside the mousemove handler
  const lineContainer = document.querySelector('#time-bar-indicator-' + renderAt) as HTMLElement;
  const timeElement = document.querySelector('#line-time-' + renderAt) as HTMLElement;
  const timeContainer = document.querySelector('#time-container-' + renderAt) as HTMLElement;
  let cachedBoundingX: number | null = null;
  let cachedContainerWidth: number | null = null;

  // Update cache when needed
  const updateCache = () => {
    if (flameGraphContainer) {
      cachedBoundingX = flameGraphContainer.getBoundingClientRect().x;
    }
    if (timeContainer) {
      cachedContainerWidth = timeContainer.offsetWidth - SCROLL_BAR_WIDTH;
    }
  };

  // Initial cache update
  updateCache();

  // Update cache on resize
  window.addEventListener('resize', debounce(updateCache, 250));

  flameGraphContainer.addEventListener('mousemove', (e) => {
    if (e.currentTarget && cachedBoundingX !== null && cachedContainerWidth !== null) {
      const left = (e as MouseEvent).clientX - cachedBoundingX;

      requestAnimationFrame(() => {
        lineContainer.style.left = `${left}px`;
        const currTime = (maxDuration * (left - 8)) / cachedContainerWidth;
        const [f, u] = formatDuration(currTime);
        timeElement.textContent = `${f}${u}`;

        if (left < 9 || left > cachedContainerWidth + 8) {
          lineContainer.style.display = 'none';
        } else {
          lineContainer.style.display = 'block';
        }
      });
    }
  });
  flameGraphContainer.addEventListener('mouseleave', (e) => {
    const lineContainer = document.querySelector('#time-bar-indicator-' + renderAt) as HTMLElement;
    if (lineContainer) {
      lineContainer.style.display = 'none';
    }
  });
}

function modifySpansForFlameGraph(data: FlameGraphItem[]) {
  const spans = buildHierachy(structuredClone(data));
  return spans;
}

function buildHierachy(spans: FlameGraphItem[]) {
  const spanMap = new Map<string, FlameGraphItem>();
  spans.forEach((span) => {
    span.children = [];
    spanMap.set(span.spanId, span);
  });
  const roots: FlameGraphItem[] = [];
  spans.forEach((span) => {
    if (span.parentId) {
      const parent = spanMap.get(span.parentId);
      if (parent) {
        parent.children.push(span);
      } else {
        roots.push(span);
      }
    } else {
      roots.push(span);
    }
  });
  return roots;
}

function generateTimeIntervals(duration: number, target: string) {
  const container = document.querySelector('#' + target) as HTMLElement;
  if (!container) return;
  console.log(container.offsetWidth);
  // Cache width calculation
  const containerWidth = target === 'waterfall-time-container' ? 550 : container.offsetWidth - SCROLL_BAR_WIDTH;
  const intervalWidth = containerWidth / 9;

  // Clear container after reading width
  container.innerHTML = '';
  const intervals = [];
  for (let i = 0; i < 10; i++) {
    const t = Math.floor((i * duration) / 9);
    let [durationF, unit] = formatDuration(t);
    const time = durationF;
    unit = t === 0 ? '' : unit;
    intervals.push(`
              <div class="absolute bottom-0 text-textStrong border-left overflow-x-visible" style="width: ${intervalWidth}px; left: ${
      i * intervalWidth
    }px;">
               <div class="relative" style="height:10px">
                <div class="bg-fillPress"  style="width:1px; height:10px;"></div>
                <span class="absolute  left-0 -translate-x-1/2 text-xs" style="top:-13px">${time} ${unit}</span>
               </div>
              </div>
      `);
  }
  container.innerHTML = intervals.join('');
}

function formatDuration(duration: number): string[] {
  if (duration >= 1000000000) {
    return [(duration / 1000000000).toFixed(1), 's'];
  } else if (duration >= 1000000) {
    return [(duration / 1000000).toFixed(1), 'ms'];
  } else if (duration >= 1000) {
    return [(duration / 1000).toFixed(1), 'µs'];
  } else {
    return [duration.toFixed(1), 'ns'];
  }
}

function elt(type: string, props: Record<string, any>, ...children: (HTMLElement | string)[]) {
  let dom = document.createElement(type);
  if (props) {
    for (let prop in props) {
      if (prop === 'class') {
        dom.className = props[prop];
      } else if (prop.startsWith('on') && typeof props[prop] === 'function') {
        const eventName = prop.substring(2).toLowerCase();
        dom.addEventListener(eventName, props[prop]);
      } else {
        dom.setAttribute(prop, props[prop]);
      }
    }
  }
  for (let child of children) {
    if (typeof child != 'string') dom.appendChild(child);
    else dom.appendChild(document.createTextNode(child));
  }
  return dom;
}

type WaterfallItem = {
  spanRecord: {
    parentSpanId?: string;
    spanName: string;
    spanId: string;
    spanDurationNs: number;
    uSpandId: boolean;
    hasErrors: boolean;
    serviceName: number;
    startTime: number;
    endTime: number;
    timestamp: string;
  };
  children: WaterfallItem[];
};

export function waterFallGraphChart(fData: WaterfallItem[], renderAt: string, serviceColors: Record<string, string>) {
  const { min, max } = getMinMax(fData);
  const maxDuration = max - min;
  generateTimeIntervals(maxDuration, 'waterfall-time-container-' + renderAt);
  buildWaterfall(fData, renderAt, serviceColors, min, maxDuration);
}

function buildWaterfall(spans: WaterfallItem[], target: string, serviceColors: Record<string, string>, start: number, maxDuration: number) {
  const container = document.querySelector('#ba' + target);
  if (container) {
    const containerWidth = 550;
    container.innerHTML = '';
    spans.forEach((span) => {
      container.appendChild(buildTree(span, serviceColors, start, maxDuration, containerWidth));
    });
  }
}

function buildTree(span: WaterfallItem, serviceColors: Record<string, string>, start: number, rootVal: number, containerWidth: number) {
  const startCurr = span.spanRecord.startTime;
  const st = startCurr - start;
  const startPix = (containerWidth * st) / rootVal;
  const width = (containerWidth * span.spanRecord.spanDurationNs) / rootVal;
  const parentDiv = elt('div', {
    class: 'flex flex-col span-filterble',
  });
  const spanId = span.spanRecord.spanId;
  // Use deterministic color based on service name
  const tailwindColor = serviceColors[span.spanRecord.serviceName] || getSeriesColor(span.spanRecord.serviceName, 'service');
  const color = tailwindColor.startsWith('bg-') ? tailwindColor : `bg-[${tailwindColor}]`;
  const div = elt('div', {
    class:
      color +
      ' flex rounded-sm items-center cursor-pointer  h-5 grow-0 justify-between flex-nowrap overflow-x-visible hover:border hover:border-black',
    id: 'waterfall-chart-' + spanId,
    onclick: (event: any) => {
      event.stopPropagation();
      event.currentTarget.nextSibling.classList.toggle('hidden');
      const treeTarget = document.querySelector('#waterfall-tree-' + spanId);
      if (treeTarget) treeTarget.classList.toggle('hidden');
    },
  });
  parentDiv.style.marginLeft = `${startPix}px`;
  div.style.width = `${width}px`;
  const childDiv = elt('div', {
    class: 'flex flex-col gap-2 mt-2 gap-1',
    id: 'waterfall-child-' + spanId,
  });
  span.children.forEach((child) => {
    childDiv.appendChild(buildTree(child, serviceColors, startCurr, rootVal, containerWidth));
  });
  const text = elt(
    'span',
    {
      class: 'text-black ml-1 shrink-0 mr-4 text-xs hidden',
    },
    span.spanRecord.serviceName + span.spanRecord.spanName
  );
  const [t, u] = formatDuration(span.spanRecord.spanDurationNs);
  const tim = elt('span', { class: 'text-black text-xs shrink-0 ml-auto' }, `${Math.floor(Number(t))} ${u}`);
  if (span.spanRecord.hasErrors) {
    div.appendChild(getErrorIndicator());
  }
  div.appendChild(text);
  div.appendChild(tim);
  parentDiv.appendChild(div);
  if (span.children.length > 0) {
    parentDiv.appendChild(childDiv);
  }
  return parentDiv;
}

function getMinMax(arr: WaterfallItem[]) {
  let min = Infinity;
  let max = -Infinity;
  function traverse(array: WaterfallItem[]) {
    for (let i = 0; i < array.length; i++) {
      const nano = array[i].spanRecord.startTime;
      const nano2 = array[i].spanRecord.endTime;
      if (nano < min) min = nano;
      if (nano2 > max) max = nano2;
      if (array[i].children) traverse(array[i].children);
    }
  }
  traverse(arr);
  return { min, max };
}
