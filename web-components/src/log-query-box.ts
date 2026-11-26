// LogQueryBox client runtime: visualization switching, AI search response routing,
// summary-chart tooltips, sessions bucket filtering, and inline query parse errors.
// Imported eagerly from index.ts so inline Lucid handlers can invoke these globals immediately.

const w = window as any;

let parseErrorListenersHooked = false;

export function updateVizTypeInUrl(
  vizType: string,
  shouldUpdateUrl = true,
  editor: HTMLElement | null = document.getElementById('filterElement'),
): Promise<void> {
  w.currentVisualizationType = vizType;
  return new Promise(resolve =>
    requestAnimationFrame(() => {
      const isWidgetMode = Boolean(editor && editor.hasAttribute('target-widget-preview'));
      if (shouldUpdateUrl && !isWidgetMode) {
        const url = new URL(window.location.href);
        url.searchParams.set('viz_type', vizType);
        history.replaceState({}, '', url.toString());
      }
      Promise.resolve(w.queryEditorCallFor?.(editor, 'handleVisualizationChange', vizType)).finally(resolve);
    }),
  );
}

export function handleVisualizationUpdate(vizType: string, widgetId?: string): void {
  window.requestAnimationFrame(() => {
    const preview = document.getElementById(widgetId || 'visualization-widget-container');
    const root: ParentNode = preview?.closest('[class~="group/wgtexp"]') || document;
    const radio = root.querySelector<HTMLInputElement>(`#visualizationTabs input[value='${vizType}']`);
    if (radio && !radio.checked) radio.click();
  });
}

export function applyQuery(
  q: string,
  replace = true,
  closeAiSearch = true,
  editor: HTMLElement | null = document.getElementById('filterElement'),
): void {
  const chk = editor?.closest('form')?.querySelector<HTMLInputElement>('.ai-search');
  if (chk && closeAiSearch) chk.checked = false;
  w.queryEditorCallFor?.(editor, 'handleAddQuery', q, replace);
}

export function applyAiSearchResult(evt: CustomEvent<any>, el: HTMLElement): void {
  const ctx = evt.detail && evt.detail.ctx;
  const text: string = (ctx && ctx.text) || '';
  if (!ctx || !(ctx.response && ctx.response.status < 400)) {
    if (text.includes('INVALID_QUERY_ERROR')) {
      document.body.dispatchEvent(
        new CustomEvent('errorToast', {
          bubbles: true,
          detail: {
            value: ['Could not generate a query. Try being more specific, e.g. "show errors from payment-service in the last 2 hours"'],
          },
        }),
      );
    }
    return;
  }
  let result: any;
  try {
    result = JSON.parse(text);
  } catch {
    return;
  }
  if (result.time_range) w.updateTimePicker?.(result.time_range);
  const form = el.closest('form');
  const editor = form?.querySelector<HTMLElement>('query-editor') ?? null;
  if (result.query) applyQuery(result.query, true, false, editor);
  else if (result.time_range) form?.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
  if (result.visualization_type && !form?.querySelector('query-editor[target-widget-preview]')) {
    handleVisualizationUpdate(result.visualization_type, el.dataset.containerId || 'visualization-widget-container');
  }
}

export function __sessionsBucketFilter(fromEpoch: number, toEpoch: number): void {
  if (!Number.isFinite(fromEpoch) || !Number.isFinite(toEpoch) || fromEpoch <= 0) return;
  const from = new Date(fromEpoch * 1000).toISOString();
  const to = new Date(toEpoch * 1000).toISOString();
  if (w.updateTimePicker) {
    w.updateTimePicker({ from, to }, { skipSetParams: true });
  } else {
    console.warn('[sessions-header] updateTimePicker missing; picker UI will not reflect filter');
  }
  const p = new URLSearchParams(window.location.search);
  p.set('from', from);
  p.set('to', to);
  p.delete('since');
  const url = window.location.pathname + '?' + p.toString() + window.location.hash;
  window.history.replaceState({}, '', url);
  document.dispatchEvent(
    new CustomEvent('update-query', { bubbles: true, detail: { source: 'sessions-header-bar', timeRange: `${from} \u2192 ${to}` } }),
  );
}

export function formatSummaryChart(root: ParentNode | null): void {
  const chart = root?.querySelector<HTMLElement>('[data-summary-chart]');
  if (!chart) return;
  const start = +(chart.dataset.bucketStart ?? '');
  const width = +(chart.dataset.bucketWidth ?? '');
  if (!Number.isFinite(start) || !Number.isFinite(width) || width <= 0) return;
  const bars = Array.from(chart.querySelectorAll<HTMLElement>('[data-bi]'));
  if (!bars.length) return;
  const firstEpoch = start + +(bars[0].dataset.bi ?? 0) * width;
  const endEpoch = start + (+(bars[bars.length - 1].dataset.bi ?? 0) + 1) * width;
  const spanDays = (endEpoch - firstEpoch) / 86400;
  const opts: Intl.DateTimeFormatOptions =
    spanDays >= 1
      ? { month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit' }
      : { hour: '2-digit', minute: '2-digit' };
  const fmt = (e: number) => new Date(e * 1000).toLocaleString([], opts);
  const note = bars.length === 1 ? chart.dataset.note || '' : '';
  bars.forEach(bar => {
    const from = start + +(bar.dataset.bi ?? 0) * width;
    const base = bar.getAttribute('data-count') || '';
    const label = `${fmt(from)} \u2013 ${fmt(from + width)}${base ? ` \u00b7 ${base}` : ''}${note ? ` \u00b7 ${note}` : ''}`;
    bar.setAttribute('data-tippy-content', label);
    bar.setAttribute('aria-label', label);
  });
}

export function swapSessionsRegionIfNeeded(newViz: string, prevViz: string): void {
  const isSummary = (v: string) => v === 'sessions' || v === 'patterns';
  if (!isSummary(newViz) && !isSummary(prevViz)) return;
  const region = document.getElementById('page-summary-region');
  if (!region) return;
  const tpl = document.getElementById(isSummary(newViz) ? 'sessions-summary-skeleton' : 'chart-summary-skeleton');
  if (tpl) {
    region.setAttribute('aria-busy', 'true');
    region.innerHTML = tpl.innerHTML;
  }
  if (isSummary(newViz)) return;
  if (!w.htmx) return;
  const url = new URL(window.location.href);
  url.searchParams.set('viz_type', newViz);
  w.htmx.ajax('GET', url.pathname + url.search, {
    target: '#page-summary-region',
    select: '#page-summary-region',
    swap: 'outerHTML',
  });
}

export function showQueryParseError(msg = ''): void {
  const el = document.getElementById('query-parse-error');
  if (!el || el.dataset.msg === msg) return;
  el.dataset.msg = msg;
  const m = document.getElementById('query-parse-error-msg');
  if (m) m.textContent = msg;
  document.getElementById('queryBox')?.setAttribute('data-query-state', msg ? 'error' : 'ok');
}

export const clearQueryParseError = () => showQueryParseError('');

export function initLogQueryBox(vizType: string, chartVizTypes: string[], popularQueries: unknown): void {
  w.currentVisualizationType = vizType;
  w.chartVizTypes = chartVizTypes;
  customElements.whenDefined('query-editor').then(() => {
    const editor = document.getElementById('filterElement') as (HTMLElement & Record<string, any>) | null;
    if (!editor || !w.schemaManager?.setSchemaData) return;
    editor.setQueryLibrary?.(w.queryLibraryData || []);
    editor.setPopularSearches?.(popularQueries);
  });
  if (!parseErrorListenersHooked) {
    parseErrorListenersHooked = true;
    window.addEventListener('update-query', () => clearQueryParseError());
    document.body.addEventListener('showParseError', (e: Event) => {
      const detail = (e as CustomEvent).detail;
      const msg = Array.isArray(detail) ? detail[0] : detail?.value || detail || 'Invalid query syntax';
      showQueryParseError(String(msg));
    });
  }
}

function mountLogQueryBoxes(): void {
  document.querySelectorAll<HTMLScriptElement>('script[data-log-query-box-init]:not([data-init])').forEach(el => {
    el.dataset.init = '1';
    try {
      const cfg = JSON.parse(el.textContent || '{}');
      initLogQueryBox(cfg.vizType || '', cfg.chartVizTypes || [], cfg.popularQueries || []);
    } catch {
      // Ignore malformed config payloads
    }
  });
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', mountLogQueryBoxes, { once: true });
} else {
  mountLogQueryBoxes();
}
document.addEventListener('htmx:after:settle', mountLogQueryBoxes);

Object.assign(window, {
  updateVizTypeInUrl,
  handleVisualizationUpdate,
  applyQuery,
  applyAiSearchResult,
  __sessionsBucketFilter,
  formatSummaryChart,
  swapSessionsRegionIfNeeded,
  showQueryParseError,
  clearQueryParseError,
});
