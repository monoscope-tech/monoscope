// Dashboard GridStack lifecycle, layout-shift telemetry, widget adoption, and widget editor chrome.
// Imported eagerly from index.ts so server-rendered dashboard pages invoke thin 1-line initializers.

const w = window as any;

const debounce = <T extends (...args: any[]) => void>(fn: T, wait: number) => {
  let t: ReturnType<typeof setTimeout> | undefined;
  return (...args: Parameters<T>) => {
    clearTimeout(t);
    t = setTimeout(() => fn(...args), wait);
  };
};

function compactGrid(grid: any, el: Element | null): void {
  if (!el) return;
  const items = Array.from(el.querySelectorAll<HTMLElement & { gridstackNode?: any }>(':scope > .grid-stack-item')).sort(
    (a, b) => (a.gridstackNode?.y || 0) - (b.gridstackNode?.y || 0),
  );
  const rows: Record<number, Array<HTMLElement & { gridstackNode?: any }>> = {};
  items.forEach(item => {
    const y = item.gridstackNode?.y || 0;
    (rows[y] = rows[y] || []).push(item);
  });
  let nextY = 0;
  let needsUpdate = false;
  const updates: Array<{ item: HTMLElement; y: number }> = [];
  Object.keys(rows)
    .map(Number)
    .sort((a, b) => a - b)
    .forEach(y => {
      rows[y].forEach(item => {
        if (item.gridstackNode?.y !== nextY) {
          updates.push({ item, y: nextY });
          needsUpdate = true;
        }
      });
      nextY += Math.max(...rows[y].map(item => item.gridstackNode?.h || 1));
    });
  if (needsUpdate) {
    grid.batchUpdate();
    updates.forEach(({ item, y }) => grid.update(item, { y }));
    grid.batchUpdate(false);
  }
}

function hookDashboardDocumentOnce(): void {
  if (w.__dashboardDocHooked) return;
  w.__dashboardDocHooked = true;
  document.addEventListener('widget-remove-requested', (e: Event) => {
    const detail = (e as CustomEvent).detail;
    const widgetEl = document.getElementById(`${detail.widgetId}_widgetEl`);
    const gridEl = widgetEl?.closest<HTMLElement & { gridstack?: any }>('.grid-stack');
    if (widgetEl && gridEl?.gridstack) gridEl.gridstack.removeWidget(widgetEl, true);
  });

  document.addEventListener('change', (e: Event) => {
    const target = e.target as HTMLInputElement | null;
    if (!target?.classList?.contains('wgt-collapse')) return;
    const parentWidget = target.closest<HTMLElement>('.grid-stack-item');
    const grid = w.gridStackInstance;
    if (!parentWidget || !grid) return;
    requestAnimationFrame(() => {
      const isCollapsed = target.checked;
      const mainGridEl = document.querySelector('.grid-stack:not(.nested-grid)');
      parentWidget.dataset.collapseAction = 'true';
      if (isCollapsed) {
        grid.update(parentWidget, { h: 1 });
      } else {
        const nestedInstance = (parentWidget.querySelector('.nested-grid') as any)?.gridstack;
        const items: any[] = nestedInstance?.getGridItems() || [];
        const maxRow = Math.max(1, ...items.map(item => (item.gridstackNode?.y || 0) + (item.gridstackNode?.h || 1)));
        grid.update(parentWidget, { h: 1 + maxRow });
      }
      compactGrid(grid, mainGridEl);
    });
  });
}

export function initDashboardPage(gridStackReady: Promise<unknown>, fullLoad: boolean): void {
  hookDashboardDocumentOnce();
  gridStackReady.then(() => {
    const GridStack = w.GridStack;
    const request = fullLoad
      ? null
      : (performance.getEntriesByType('resource') as PerformanceResourceTiming[])
          .filter(e => e.initiatorType === 'fetch' && e.name.includes('/dashboards/') && !e.name.includes('/settled'))
          .at(-1);
    let dashboardStartedAt = request?.startTime ?? performance.now();
    let dashboardSettledSent = false;

    function reportDashboardSettled(navigation: string, responseBytes: number): void {
      if (dashboardSettledSent) return;
      const page = document.getElementById('dashboardPage');
      const url = page?.dataset.settledUrl;
      const durationMs = Math.round(performance.now() - dashboardStartedAt);
      if (!url || durationMs > 600000) return;
      dashboardSettledSent = true;
      const body = JSON.stringify({
        navigation,
        duration_ms: durationMs,
        response_bytes: Math.max(0, Math.round(responseBytes || 0)),
      });
      fetch(url, { method: 'POST', credentials: 'same-origin', keepalive: true, headers: { 'Content-Type': 'application/json' }, body }).catch(() => {});
    }

    w.interpolateVarTemplates = w.interpolateVarTemplates || (() => {});
    GridStack.renderCB = (el: HTMLElement, widget: any) => {
      el.innerHTML = widget.content;
      Array.from(el.querySelectorAll('script')).forEach(oldScript => {
        const newScript = document.createElement('script');
        Array.from(oldScript.attributes).forEach(attr => newScript.setAttribute(attr.name, attr.value));
        if (oldScript.textContent) newScript.textContent = oldScript.textContent;
        oldScript.parentNode?.replaceChild(newScript, oldScript);
      });
    };

    w.adoptDashboardWidget = (html: string, existingEl?: HTMLElement & { gridstackNode?: any }) => {
      const template = document.createElement('template');
      template.innerHTML = html.trim();
      const widgetEl = template.content.querySelector('.grid-stack-item');
      const grid = w.gridStackInstance;
      if (!(widgetEl instanceof HTMLElement) || !grid) return null;
      const position = existingEl?.gridstackNode
        ? (({ x, y, w: width, h }) => ({ x, y, w: width, h }))(existingEl.gridstackNode)
        : null;
      if (existingEl) grid.removeWidget(existingEl, true, false);
      grid.el.appendChild(widgetEl);
      grid.makeWidget(widgetEl);
      if (position) grid.update(widgetEl, position);
      w.htmx?.process(widgetEl);
      w.evalScriptsFromContent?.(widgetEl);
      return widgetEl;
    };

    function initializeGrids(): void {
      document.querySelectorAll<HTMLElement & { _userInteracted?: boolean }>('.grid-stack:not(.nested-grid)').forEach(gridEl => {
        if (gridEl.classList.contains('grid-stack-initialized')) return;
        try {
          const grid = GridStack.init(
            {
              column: 12,
              cellHeight: '5rem',
              margin: '1rem 0.5rem',
              handleClass: 'grid-stack-handle',
              styleInHead: true,
              float: false,
              animate: false,
              columnOpts: {
                breakpointForWindow: false,
                breakpoints: gridEl.dataset.minWidth ? [{ w: Number(gridEl.dataset.minWidth), c: 1 }] : [{ w: 800, c: 6 }, { w: 400, c: 1 }],
                layout: 'list',
              },
            },
            gridEl,
          );
          let lastCol = grid.getColumn();
          if (lastCol === 6) grid.compact();
          const readOnly = !!gridEl.closest('[data-read-only]');
          if (lastCol !== 12 || readOnly) grid.setStatic(true);
          new ResizeObserver(() => {
            const col = grid.getColumn();
            if (col !== lastCol) {
              lastCol = col;
              if (col === 6) grid.compact();
              grid.setStatic(col !== 12 || readOnly);
            }
          }).observe(gridEl);

          grid.on('dragstart resizestart', () => { gridEl._userInteracted = true; });
          grid.on(
            'removed change',
            debounce(() => {
              if (grid.getColumn() !== 12 || gridEl.offsetParent === null) return;
              const collapsingWidget = gridEl.querySelector<HTMLElement>('[data-collapse-action]');
              if (collapsingWidget) {
                delete collapsingWidget.dataset.collapseAction;
                return;
              }
              if (!gridEl._userInteracted) return;
              w.htmx?.trigger(document.body, 'widget-order-changed');
            }, 500),
          );
          grid.on('removed', () => { gridEl._userInteracted = true; });
          gridEl.classList.add('grid-stack-initialized');
          requestAnimationFrame(() => requestAnimationFrame(() => grid.setAnimation(true)));
          w.gridStackInstance = grid;
        } finally {
          w.interpolateVarTemplates();
        }
      });

      document.querySelectorAll<HTMLElement & { _userInteracted?: boolean }>('.nested-grid').forEach(nestedEl => {
        if (nestedEl.classList.contains('grid-stack-initialized')) return;
        const parentWidget = nestedEl.closest<HTMLElement & { gridstackNode?: any }>('.grid-stack-item');
        const collapseInput = parentWidget?.querySelector<HTMLInputElement>('.wgt-collapse');
        if (parentWidget) parentWidget.dataset.originalH = parentWidget.getAttribute('gs-h') || '0';

        const nestedInstance = GridStack.init(
          {
            column: 12,
            acceptWidgets: true,
            cellHeight: '5rem',
            margin: '1rem 0.5rem',
            handleClass: 'nested-grid-stack-handle',
            styleInHead: true,
            animate: false,
            columnOpts: {
              breakpointForWindow: false,
              breakpoints: nestedEl.dataset.minWidth ? [{ w: Number(nestedEl.dataset.minWidth), c: 1 }] : [{ w: 600, c: 6 }, { w: 400, c: 1 }],
              layout: 'list',
            },
          },
          nestedEl,
        );
        let lastNestedCol = nestedInstance.getColumn();
        if (lastNestedCol === 6) nestedInstance.compact();
        const nestedReadOnly = !!nestedEl.closest('[data-read-only]');
        if (lastNestedCol !== 12 || nestedReadOnly) nestedInstance.setStatic(true);

        function autoFitGroupToChildren(): void {
          const items: any[] = nestedInstance.getGridItems();
          const node = parentWidget?.gridstackNode;
          if (!node || !parentWidget || collapseInput?.checked) return;
          const isFullWidth = node.w === 12;
          const maxRow = Math.max(1, ...items.map(item => (item.gridstackNode?.y || 0) + (item.gridstackNode?.h || 1)));
          const requiredHeight = 1 + maxRow;
          const yamlHeight = parseInt(parentWidget.dataset.originalH || '', 10) || requiredHeight;
          const targetHeight = isFullWidth ? requiredHeight : Math.max(yamlHeight, requiredHeight);
          if (node.h !== targetHeight && w.gridStackInstance) {
            w.gridStackInstance.update(parentWidget, { h: targetHeight });
          }
        }

        nestedInstance.on('change added removed', autoFitGroupToChildren);
        requestAnimationFrame(autoFitGroupToChildren);
        new ResizeObserver(() => {
          const col = nestedInstance.getColumn();
          if (col !== lastNestedCol) {
            lastNestedCol = col;
            if (col === 6) nestedInstance.compact();
            nestedInstance.setStatic(col !== 12 || nestedReadOnly);
            autoFitGroupToChildren();
          }
        }).observe(nestedEl);
        nestedInstance.on('dragstart resizestart removed', () => { nestedEl._userInteracted = true; });
        nestedInstance.on(
          'removed change',
          debounce(() => {
            if (nestedInstance.getColumn() !== 12 || nestedEl.offsetParent === null) return;
            const collapsingWidget = nestedEl.closest<HTMLElement>('[data-collapse-action]');
            if (collapsingWidget) {
              delete collapsingWidget.dataset.collapseAction;
              return;
            }
            if (!nestedEl._userInteracted) return;
            w.htmx?.trigger(document.body, 'widget-order-changed');
          }, 500),
        );
        nestedEl.classList.add('grid-stack-initialized');
        requestAnimationFrame(() => requestAnimationFrame(() => nestedInstance.setAnimation(true)));
      });
    }

    initializeGrids();
    const loaded = (request ?? performance.getEntriesByType('navigation')[0]) as PerformanceResourceTiming | PerformanceNavigationTiming | undefined;
    requestAnimationFrame(() =>
      requestAnimationFrame(() => reportDashboardSettled(fullLoad ? 'initial' : 'link', loaded?.transferSize || loaded?.encodedBodySize || 0)),
    );

    if (!w.__dashboardLayoutShifts && typeof PerformanceObserver !== 'undefined' && PerformanceObserver.supportedEntryTypes?.includes('layout-shift')) {
      w.__dashboardLayoutShifts = [];
      new PerformanceObserver(list => {
        for (const entry of list.getEntries() as any[]) {
          if (entry.hadRecentInput) continue;
          const page = document.getElementById('dashboardPage');
          const sample = {
            value: entry.value,
            dashboardId: page?.dataset.dashboardId || 'unknown',
            widgetCount: Number(page?.dataset.widgetCount || 0),
            gridRelated: entry.sources?.some((source: any) => source.node?.closest?.('.grid-stack')) || false,
            chartRelated: entry.sources?.some((source: any) => source.node?.closest?.('[data-chart-widget]')) || false,
          };
          w.__dashboardLayoutShifts.push(sample);
          window.dispatchEvent(new CustomEvent('dashboard-layout-shift', { detail: sample }));
        }
      }).observe({ type: 'layout-shift', buffered: true });
    }

    w.initializeGrids = initializeGrids;
    if (!w.__dashboardSwapHooked) {
      w.__dashboardSwapHooked = true;
      ['htmx:after:swap', 'htmx:after:settle'].forEach(name => {
        document.addEventListener(name, (event: Event) => {
          const el = event.target;
          if (!(el instanceof Element) || !(el.matches('.grid-stack') || el.querySelector('.grid-stack'))) return;
          w.initializeGrids();
          w.interpolateVarTemplates();
        });
      });
      document.addEventListener('htmx:before:request', (event: Event) => {
        const trigger = (event as CustomEvent).detail?.elt;
        if (!(trigger instanceof Element) || !trigger.closest('#dashboard-tabs-container')) return;
        dashboardStartedAt = performance.now();
        dashboardSettledSent = false;
      });
      document.addEventListener('htmx:after:settle', (event: Event) => {
        const target = (event as CustomEvent).detail?.target;
        if (!(target instanceof Element) || target.id !== 'dashboard-tabs-content') return;
        const resource = (performance.getEntriesByType('resource') as PerformanceResourceTiming[])
          .filter(entry => entry.name.includes('/dashboards/') && entry.name.includes('/tab/'))
          .at(-1);
        const bytes = resource?.transferSize || resource?.encodedBodySize || 0;
        w.initializeGrids();
        requestAnimationFrame(() => requestAnimationFrame(() => reportDashboardSettled('tab', bytes)));
      });
    }
  });
}

export function initWidgetEditor(editor: HTMLElement): void {
  const pane = editor.closest<HTMLElement & { _widgetPreviewResize?: ResizeObserver; _widgetPreviewAbort?: AbortController }>('[role="dialog"]');
  const header = editor.querySelector<HTMLElement>('.widget-editor-header');
  if (!pane || !header) return;
  const update = () => {
    const compact = editor.classList.contains('is-compact');
    const next = pane.scrollTop > 0;
    if (next !== compact) editor.classList.toggle('is-compact', next);
  };
  const resize = () => editor.style.setProperty('--widget-header-height', `${header.offsetHeight}px`);
  pane.onscroll = update;
  pane._widgetPreviewResize?.disconnect();
  pane._widgetPreviewResize = new ResizeObserver(resize);
  pane._widgetPreviewResize.observe(header);
  pane._widgetPreviewAbort?.abort();
  pane._widgetPreviewAbort = new AbortController();
  const initialHeight = window.visualViewport?.height;
  window.visualViewport?.addEventListener(
    'resize',
    () => {
      if (!initialHeight || !window.visualViewport) return;
      const keyboardOpen = initialHeight - window.visualViewport.height > 140;
      editor.classList.toggle('keyboard-open', keyboardOpen);
      pane.style.height = keyboardOpen ? `${window.visualViewport.height}px` : '';
    },
    { signal: pane._widgetPreviewAbort.signal },
  );
  editor.addEventListener('change', (e: Event) => {
    if ((e.target as Element)?.matches('.page-drawer-tab-edit, .page-drawer-tab-monitors')) requestAnimationFrame(update);
  });
  resize();
  update();
}

export function submitWidgetWithMonitor(alertFormId: string, widgetFormId: string, hintId: string): void {
  const form = document.getElementById(alertFormId) as HTMLFormElement | null;
  const filter = document.querySelector<HTMLElement & { getValue?: () => string }>('#filterElement');
  if (!filter?.getValue?.().trim()) {
    const hint = document.getElementById(hintId);
    if (hint) hint.hidden = false;
    document.querySelector<HTMLElement>('#filterElement .cm-content')?.focus();
    return;
  }
  if (!form?.reportValidity()) return;
  if (form.dataset.widgetSaved === 'true') {
    form.requestSubmit();
    return;
  }
  for (const [from, to] of [
    ['alertThreshold', 'widgetAlertThreshold'],
    ['warningThreshold', 'widgetWarningThreshold'],
    ['unit', 'widgetUnit'],
  ]) {
    const dst = document.getElementById(to) as HTMLInputElement | null;
    const src = form.querySelector<HTMLInputElement>(`[name="${from}"]`);
    if (dst && src) dst.value = src.value;
  }
  (document.getElementById(widgetFormId) as HTMLFormElement | null)?.requestSubmit();
}

function mountDashboardAndEditors(fullLoad: boolean): void {
  const page = document.getElementById('dashboardPage');
  if (page && !page.dataset.dashboardInit) {
    page.dataset.dashboardInit = '1';
    initDashboardPage(w.loadDeps('GridStack'), fullLoad);
  }
  document.querySelectorAll<HTMLElement>('.widget-editor:not([data-widget-editor-init])').forEach(el => {
    el.dataset.widgetEditorInit = '1';
    initWidgetEditor(el);
  });
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', () => mountDashboardAndEditors(true), { once: true });
} else {
  mountDashboardAndEditors(true);
}
document.addEventListener('htmx:after:settle', () => mountDashboardAndEditors(false));

Object.assign(window, { submitWidgetWithMonitor });
