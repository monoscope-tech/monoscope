import { getSeriesColor, tailwindToHex, getContrastTextColor } from './colorMapping';

// Calculate actual trace time bounds (minStart, maxEnd) across root spans
const getTimeBounds = (spans: FlameGraphItem[]): { minStart: number; range: number } => {
  let minStart = Infinity, maxEnd = -Infinity;
  for (const item of spans) {
    minStart = Math.min(minStart, item.start);
    maxEnd = Math.max(maxEnd, item.start + item.value);
  }
  return { minStart, range: maxEnd - minStart || 1 };
};

const SCROLL_BAR_WIDTH = 7;
const FLAME_PAD_LEFT = 4;
const RESIZE_EVENTS = ['resize', 'toggle-sidebar', 'loglist-resize'];

const resolveColor = (service: string, colorsMap: Record<string, string>): string => {
  const tw = colorsMap[service] || getSeriesColor(service, 'service');
  return tw.startsWith('#') ? tw : tailwindToHex(tw);
};

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
      title: 'Error',
      'aria-label': 'Error',
    },
    elt(
      'span',
      {
        class: 'text-textInverse-strong text-xs h-3 w-3 flex items-center justify-center rounded-full border border-white p-1',
      },
      '!'
    )
  );

// Tooltip for hovering over small spans
let tooltipEl: HTMLElement | null = null;
function showTooltip(e: MouseEvent, text: string) {
  if (!tooltipEl) {
    tooltipEl = document.createElement('div');
    tooltipEl.className = 'fixed z-[9999] px-2 py-1 text-xs rounded bg-bgOverlay text-textStrong border border-strokeWeak shadow-md pointer-events-none whitespace-nowrap';
    document.body.appendChild(tooltipEl);
  }
  tooltipEl.textContent = text;
  tooltipEl.style.display = 'block';
  tooltipEl.style.left = `${e.clientX + 12}px`;
  tooltipEl.style.top = `${e.clientY - 8}px`;
}
function hideTooltip() {
  if (tooltipEl) tooltipEl.style.display = 'none';
}

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

// Abort controllers keyed by renderAt to clean up listeners on re-init
const abortControllers = new Map<string, AbortController>();
function acquireSignal(key: string): AbortSignal {
  abortControllers.get(key)?.abort();
  const ac = new AbortController();
  abortControllers.set(key, ac);
  return ac.signal;
}

function flameGraphChart(data: FlameGraphItem[], renderAt: string, colorsMap: Record<string, string>) {
  const signal = acquireSignal('flame-' + renderAt);
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
    const filteredJson = id != null ? filterJson(structuredClone(jsonObj), id) : jsonObj;
    const { minStart: globalMinStart, range: rootVal } = getTimeBounds(filteredJson);
    const recur = (item: FlameGraphItem, level = 0) => {
      const color = resolveColor(item.serviceName, colorsMap);
      data.push({
        name: item.name,
        hasErrors: item.hasErrors,
        spanId: item.spanId,
        value: [level, item.start - globalMinStart, item.value, item.name, (item.value / rootVal) * 100],
        itemStyle: { color },
      });
      for (const child of item.children || []) {
        recur(child, level + 1);
      }
    };
    filteredJson.forEach((item) => recur(item));
    return data;
  };

  const fData = buildHierarchy(data);
  const flameContainer = document.querySelector('#a' + renderAt) as HTMLElement;
  const resetBtn = document.getElementById('reset-zoom-btn');

  const renderItem = (item: ItemsWithStyle, container: HTMLElement, rootVal: number, containerWidth: number) => {
    const [level, xStart, xEnd] = item.value;
    const drawWidth = containerWidth - FLAME_PAD_LEFT;
    const startPix = Math.max(FLAME_PAD_LEFT, FLAME_PAD_LEFT + (drawWidth * xStart) / rootVal);
    const width = Math.max((drawWidth * xEnd) / rootVal, 3);
    const height = 20;
    const yStart = height * level + (level + 1) * 3;

    const [t, u] = formatDuration(item.value[2]);
    const pct = item.value[4];
    const div = elt('div', {
      class: 'absolute hover:z-999 flex rounded-sm items-center span-filterble cursor-pointer gap-1 flex-nowrap overflow-hidden hover:border hover:border-black',
      id: item.spanId,
      style: `background-color: ${item.itemStyle.color};`,
      onclick: (e: any) => {
        e.stopPropagation();
        // Highlight selected span
        document.querySelectorAll('.timeline-selected').forEach((el) => {
          el.classList.remove('ring-2', 'ring-strokeBrand-strong', 'timeline-selected');
        });
        (e.currentTarget as HTMLElement).classList.add('ring-2', 'ring-strokeBrand-strong', 'timeline-selected');
        const data = filterJson(structuredClone(fData), item.name);
        flameGraph(data, renderAt);
        resetBtn?.classList.remove('hidden');
        (window as any).htmx.trigger('#trigger-span-' + item.spanId, 'click');
      },
      onmouseenter: (e: MouseEvent) => {
        showTooltip(e, `${item.name} — ${Math.floor(Number(t))} ${u} (${pct.toFixed(1)}%)`);
      },
      onmouseleave: () => hideTooltip(),
    });
    div.style.left = `${startPix}px`;
    div.style.top = `${yStart}px`;
    div.style.width = `${width}px`;
    div.style.height = `${height}px`;
    if (item.hasErrors) {
      div.appendChild(getErrorIndicator());
    }
    const textColor = getContrastTextColor(item.itemStyle.color);
    const text = elt('span', { class: 'ml-1 shrink-0 mr-4 text-xs', style: `color: ${textColor}` }, item.name);
    const tim = elt('span', { class: 'text-xs shrink-0 ml-auto mr-1 tabular-nums', style: `color: ${textColor}` }, `${Math.floor(Number(t))} ${u}`);
    div.appendChild(text);
    div.appendChild(tim);
    container.appendChild(div);
  };

  let maxDuration = 0;
  function flameGraph(stackTrace: FlameGraphItem[], target: string) {
    if (!flameContainer) return;
    flameContainer.innerHTML = '';
    const rootVal = getTimeBounds(stackTrace).range;
    maxDuration = rootVal;
    generateTimeIntervals(rootVal, 'time-container-' + target, FLAME_PAD_LEFT);

    const containerWidth = flameContainer.offsetWidth - SCROLL_BAR_WIDTH;
    const data = recursionJson(stackTrace);
    data.sort((a, b) => b.value[2] - a.value[2]);
    data.forEach((item) => renderItem(item, flameContainer, rootVal, containerWidth));
  }

  flameGraph(fData, renderAt);

  // Reset zoom button
  if (resetBtn) {
    resetBtn.addEventListener('click', () => {
      flameGraph(fData, renderAt);
      resetBtn.classList.add('hidden');
    }, { signal });
  }

  const flameGraphContainer = document.querySelector('#flame-graph-container-' + renderAt) as HTMLElement;

  // Cache DOM elements and values outside the mousemove handler
  const lineContainer = document.querySelector('#time-bar-indicator-' + renderAt) as HTMLElement;
  const timeElement = document.querySelector('#line-time-' + renderAt) as HTMLElement;
  const timeContainer = document.querySelector('#time-container-' + renderAt) as HTMLElement;
  let cachedBoundingX: number | null = null;
  let cachedContainerWidth: number | null = null;

  // Update cache when needed
  const updateCache = () => {
    if (!flameGraphContainer || !timeContainer) return;
    cachedBoundingX = flameGraphContainer.getBoundingClientRect().x;
    cachedContainerWidth = timeContainer.offsetWidth - SCROLL_BAR_WIDTH;
  };

  // Initial cache update
  updateCache();

  const redraw = () => { updateCache(); flameGraph(fData, renderAt); };
  const debouncedRedraw = debounce(redraw, 100);
  RESIZE_EVENTS.forEach((e) => window.addEventListener(e, debouncedRedraw, { signal }));

  // Re-render when tab becomes visible via navigatable()
  const flameTab = flameGraphContainer.closest('.a-tab-content');
  if (flameTab) {
    flameTab.addEventListener('tab-visible', () => requestAnimationFrame(redraw), { signal });
  }

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
  flameGraphContainer.addEventListener('mouseleave', () => {
    if (lineContainer) lineContainer.style.display = 'none';
  });
}

window.flameGraphChart = flameGraphChart;

function buildHierarchy(spans: FlameGraphItem[]): FlameGraphItem[] {
  const spanMap = new Map<string, FlameGraphItem>();
  const roots: FlameGraphItem[] = [];
  for (const span of structuredClone(spans)) {
    span.children = [];
    spanMap.set(span.spanId, span);
  }
  for (const span of spanMap.values()) {
    const parent = span.parentId ? spanMap.get(span.parentId) : null;
    parent ? parent.children.push(span) : roots.push(span);
  }
  return roots;
}

function generateTimeIntervals(duration: number, target: string, padLeft = 0) {
  const container = document.querySelector('#' + target) as HTMLElement;
  if (!container) return;
  const drawWidth = container.offsetWidth - SCROLL_BAR_WIDTH - padLeft;
  const intervalWidth = drawWidth / 9;

  // Clear container after reading width
  container.innerHTML = '';
  const intervals = [];
  for (let i = 0; i < 10; i++) {
    const t = Math.floor((i * duration) / 9);
    let [time, unit] = formatDuration(t);
    unit = t === 0 ? '' : unit;
    intervals.push(`
              <div class="absolute bottom-0 text-textStrong border-left overflow-x-visible" style="width: ${intervalWidth}px; left: ${
      padLeft + i * intervalWidth
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
    const v = duration / 1000000000;
    return [v >= 10 ? Math.round(v).toString() : v.toFixed(1), 's'];
  } else if (duration >= 1000000) {
    const v = duration / 1000000;
    return [v >= 10 ? Math.round(v).toString() : v.toFixed(1), 'ms'];
  } else if (duration >= 1000) {
    const v = duration / 1000;
    return [v >= 10 ? Math.round(v).toString() : v.toFixed(1), 'µs'];
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

function waterFallGraphChart(renderAt: string, serviceColors: Record<string, string>) {
  const container = document.querySelector('#waterfall-rows-' + renderAt);
  if (!container) return;

  const signal = acquireSignal('waterfall-' + renderAt);

  // Parse bar data once from DOM attributes, pre-compute colors
  const barEls = container.querySelectorAll<HTMLElement>('[id^="waterfall-bar-"]');
  const barData: { el: HTMLElement; start: number; duration: number; color: string; textColor: string; label: string; hasErrors: boolean; spanName: string; service: string }[] = [];
  let min = Infinity, max = -Infinity;
  barEls.forEach((el) => {
    const start = Number(el.dataset.start);
    const duration = Number(el.dataset.duration);
    const service = el.dataset.service || '';
    const color = resolveColor(service, serviceColors);
    const [t, u] = formatDuration(duration);
    min = Math.min(min, start);
    max = Math.max(max, start + duration);
    barData.push({ el, start, duration, color, textColor: getContrastTextColor(color), label: `${Math.floor(Number(t))} ${u}`, hasErrors: el.dataset.hasErrors === 'true', spanName: el.dataset.spanName || '', service });
  });
  const maxDuration = max - min || 1;

  const renderBars = () => {
    const firstWidth = barData[0]?.el.clientWidth;
    if (!firstWidth) return;
    for (const { el, start, duration, color, textColor, label, hasErrors, spanName, service } of barData) {
      const offset = start - min;
      const leftPx = Math.max(0, (firstWidth * offset) / maxDuration);
      const widthPx = Math.max((firstWidth * duration) / maxDuration, 2);
      const pct = (duration / maxDuration) * 100;

      el.innerHTML = '';
      const bar = elt('div', {
        class: 'absolute top-1 bottom-1 rounded-sm flex items-center overflow-hidden',
        style: `left:${leftPx}px;width:${widthPx}px;background-color:${color};`,
        onmouseenter: (e: MouseEvent) => showTooltip(e, `${service} · ${spanName} — ${label} (${pct.toFixed(1)}%)`),
        onmouseleave: () => hideTooltip(),
      });
      if (hasErrors) bar.appendChild(getErrorIndicator());
      const tim = elt('span', { class: 'text-xs shrink-0 mr-1 ml-auto tabular-nums', style: `color:${textColor}` }, label);
      bar.appendChild(tim);
      el.appendChild(bar);
    }
  };

  const fullRender = () => {
    generateTimeIntervals(maxDuration, 'waterfall-time-container-' + renderAt);
    renderBars();
  };

  fullRender();

  // Re-render when tab becomes visible via navigatable()
  const waterfallTab = (container as HTMLElement).closest('.a-tab-content');
  if (waterfallTab) {
    waterfallTab.addEventListener('tab-visible', () => requestAnimationFrame(fullRender), { signal });
  }

  const debouncedRender = debounce(fullRender, 150);
  [...RESIZE_EVENTS, 'waterfallResize'].forEach((e) => window.addEventListener(e, debouncedRender, { signal }));

  // Time cursor indicator
  const wfContainer = document.querySelector('#waterfall-container-' + renderAt) as HTMLElement;
  const wfIndicator = document.querySelector('#wf-time-indicator-' + renderAt) as HTMLElement;
  const wfTimeLabel = document.querySelector('#wf-time-label-' + renderAt) as HTMLElement;
  const wfTimeRuler = document.querySelector('#waterfall-time-container-' + renderAt) as HTMLElement;
  if (wfContainer && wfIndicator && wfTimeLabel && wfTimeRuler) {
    let wfCachedLeft: number | null = null;
    let wfCachedWidth: number | null = null;
    const updateWfCache = () => {
      const rect = wfTimeRuler.getBoundingClientRect();
      wfCachedLeft = rect.left;
      wfCachedWidth = rect.width - SCROLL_BAR_WIDTH;
    };
    updateWfCache();
    RESIZE_EVENTS.forEach((e) => window.addEventListener(e, updateWfCache, { signal }));
    window.addEventListener('waterfallResize', updateWfCache, { signal });

    wfContainer.addEventListener('mousemove', (e) => {
      if (wfCachedLeft === null || wfCachedWidth === null) return;
      const x = e.clientX - wfCachedLeft;
      requestAnimationFrame(() => {
        if (x < 0 || x > wfCachedWidth!) {
          wfIndicator.style.display = 'none';
        } else {
          wfIndicator.style.display = 'block';
          wfIndicator.style.left = `${wfTimeRuler.offsetLeft + x}px`;
          const currTime = (maxDuration * x) / wfCachedWidth!;
          const [f, u] = formatDuration(currTime);
          wfTimeLabel.textContent = `${f}${u}`;
        }
      });
    }, { signal });
    wfContainer.addEventListener('mouseleave', () => { wfIndicator.style.display = 'none'; }, { signal });
  }
}

window.waterFallGraphChart = waterFallGraphChart;
