// htmx 4 replaced defineExtension({onEvent}) with registerExtension(name, hooks), where each
// hook is named after its event with ':' → '_' (htmx:config:request → htmx_config_request) and
// is called as (elt, detail). Every request-lifecycle detail is `{ctx}` — the mutable request
// lives at `detail.ctx.request`, NOT `detail.request`; reading the wrong one silently no-ops.
// Registration is global — v4 dropped hx-ext as the activation mechanism — so each hook gates
// itself on the hx-ext marker attribute the call sites already carry.
import { copyParams, SCOPE_PARAMS, TIME_PARAMS } from './time-range-utils';

const htmx4 = (window as any).htmx;

// Navigation can match an issue's embedded list to the explorer's list by id.
// Morphing the empty server element over Lit's light DOM deletes its part markers
// and leaves the list unable to render. Use a fresh instance with the incoming
// page's attributes, worker and visibility observer. Morph hooks in HTMX 4 are
// extension callbacks, not DOM events.
htmx4.registerExtension('log-list-navigation', {
  htmx_before_morph_node(elt: Element, { newNode }: { newNode: Element }) {
    if (elt.localName !== 'log-list') return;
    const replacement = newNode.cloneNode(true);
    // HTMX still needs the old node to traverse siblings until the morph finishes.
    queueMicrotask(() => {
      if (elt.isConnected) elt.replaceWith(replacement);
    });
    return false;
  },
});

// Lucid renders every htmx attribute in the `data-` form and call sites comma-separate
// multiple extensions, so neither a bare `hx-ext` nor a `~=` token match would do.
const optedIn = (elt: Element | null | undefined, ext: string) => {
  const host = elt?.closest?.('[hx-ext],[data-hx-ext]');
  const val = host?.getAttribute('hx-ext') ?? host?.getAttribute('data-hx-ext') ?? '';
  return val.split(/[\s,]+/).includes(ext);
};

// Helper to get dashboard constants from data attribute
const getDashboardConstants = (el?: Element | null): Record<string, string> => {
  const constantsEl = el?.closest('[data-constants]') ?? document.querySelector('[data-constants]');
  if (!constantsEl) return {};
  try {
    return JSON.parse(constantsEl.getAttribute('data-constants') || '{}');
  } catch {
    return {};
  }
};
(window as any).getDashboardConstants = getDashboardConstants;

// HTMX extension to forward current page query parameters and dashboard constants to GET/POST requests
htmx4.registerExtension('forward-page-params', {
  htmx_config_request: function (elt: Element, detail: any) {
    if (!optedIn(elt, 'forward-page-params')) return;
    const req = detail.ctx.request;
    const method = String(req.method ?? 'get').toLowerCase();
    if (method !== 'get' && method !== 'post') return;
    const url = new URL(req.action, window.location.origin);

    // Forward URL params first (they take precedence)
    new URLSearchParams(window.location.search).forEach((value, key) => {
      if (!url.searchParams.has(key)) url.searchParams.set(key, value);
    });
    // Dashboard constants are the fallback (only if not already in the URL)
    Object.entries(getDashboardConstants(elt)).forEach(([key, value]) => {
      if (!url.searchParams.has(key)) url.searchParams.set(key, value);
    });

    req.action = url.origin === window.location.origin ? url.pathname + url.search : url.href;
  },
});

// Global scope lives in the document URL, so every fragment request must inherit it even
// when the rendered hx-get/hx-post URL knows nothing about the page shell.
htmx4.registerExtension('global-scope', {
  htmx_config_request: function (_elt: Element, detail: any) {
    const req = detail.ctx.request;
    const url = new URL(req.action, window.location.origin);
    const page = new URLSearchParams(window.location.search);
    for (const key of SCOPE_PARAMS) {
      const value = page.get(key);
      if (value) url.searchParams.set(key, value);
      else url.searchParams.delete(key);
    }
    req.action = url.origin === window.location.origin ? url.pathname + url.search : url.href;
  },
});

// htmx 4 has no json-enc extension and its hx-encoding only chooses multipart vs urlencoded,
// so the JSON body these endpoints expect is ported here. It must hook before:request, not
// config:request: after config:request htmx unconditionally does
// `request.body = new URLSearchParams(request.body)` for anything not multipart, which would
// re-parse a JSON string into one garbage form field.
htmx4.registerExtension('json-enc', {
  htmx_before_request: function (elt: Element, detail: any) {
    if (!optedIn(elt, 'json-enc')) return;
    const req = detail.ctx.request;
    if (!(req.body instanceof URLSearchParams || req.body instanceof FormData)) return;
    // htmx flattens hx-vals into the FormData body with `set()`, which stringifies objects and
    // arrays to "[object Object]" — so the nested payloads (`js:{...widgetJSON}`, `{teams: [...]}`)
    // must be recovered from ctx.vals, where htmx keeps them unflattened, and overlaid on the form.
    req.body = JSON.stringify({ ...Object.fromEntries((req.body as any).entries()), ...(detail.ctx.vals ?? {}) });
    req.headers['Content-Type'] = 'application/json';
    // htmx 4 hardcodes `Accept: text/html`, which Servant rejects with 406 on a `Post '[JSON]`
    // route — and it checks Accept before Content-Type, so that 406 masks the body encoding
    // entirely. json-enc call sites are mixed (ai_search is JSON, /widget and manage_teams are
    // HTML), so prefer JSON and keep HTML acceptable at a lower quality.
    req.headers['Accept'] = 'application/json, text/html;q=0.9';
  },
});

// Params that stop making sense once a given filter changes. `cursor` is the general
// case and was previously missing everywhere: it is an offset into one filtered result
// set, so carrying it into a differently-filtered one lands the reader on an unrelated
// page. Everything under `source` additionally describes a selection within the old
// result set.
const INVALIDATED_BY: Record<string, readonly string[]> = {
  source: ['queryAST', 'query', 'cols', 'target-spans', 'details_width', 'target_event', 'showTrace', 'cursor'],
  metric_source: ['cursor'],
  metric_prefix: ['cursor'],
};

window.setQueryParamAndReload = (key: string, value: string) => {
  const url = new URL(window.location.href);
  if (value) url.searchParams.set(key, value);
  else url.searchParams.delete(key);
  for (const stale of INVALIDATED_BY[key] ?? []) url.searchParams.delete(stale);
  window.location.href = url.toString();
};

window.downloadJson = function (event: any) {
  event.stopPropagation();
  const json = event.currentTarget.closest('.json-tree-container')?.dataset.reqjson ?? event.currentTarget.dataset.reqjson;
  var blob = new Blob([json], { type: 'application/json' });
  var a = document.createElement('a');
  a.href = URL.createObjectURL(blob);
  a.download = 'request-data-' + new Date().toString() + '.json';
  a.textContent = '';
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
};

window.evalScriptsFromContent = function (container: HTMLElement) {
  container.querySelectorAll('script').forEach((oldScript) => {
    const newScript = document.createElement('script');
    newScript.text = oldScript.textContent || oldScript.innerHTML;

    // Copy attributes using the spread operator
    [...oldScript.attributes].forEach((attr) => newScript.setAttribute(attr.name, attr.value));

    // Append and remove to execute
    document.body.append(newScript);
    newScript.remove();
  });
};

export const params = () => ({ ...Object.fromEntries(new URLSearchParams(location.search)) });
window.params = params;

window.getTimeRange = function () {
  const rangeInput = document.getElementById('custom_range_input') as HTMLInputElement | undefined;
  if (rangeInput) {
    const range = rangeInput.value.split('/');
    if (range.length == 2) {
      return { from: range[0], to: range[1], since: '' };
    }
    if (range[0] != '') {
      return { since: range[0], from: '', to: '' };
    }
    // No explicit pick: send empty and let the server fill its default (see
    // defaultSince in TimePicker.hs). The frontend never names a default range.
    return { since: params().since || '', from: params().from, to: params().to };
  }

  const fromInput = document.querySelector('input[name="from"]') as HTMLInputElement | null;
  const toInput = document.querySelector('input[name="to"]') as HTMLInputElement | null;

  return {
    since: '',
    from: fromInput ? fromInput.value || '' : '',
    to: toInput ? toInput.value || '' : '',
  };
};

// Merge `newState` over the params currently in the URL.
//
// This used to close over a snapshot taken when main.ts loaded. The app navigates by HTMX
// morph, so main.ts loads once per session while other paths — the query editor, the
// column and facet sync, chart zoom — keep writing params straight through
// history.replaceState. Rebuilding the URL from that stale snapshot silently dropped every
// one of them the next time the time range changed: the reader's query and column
// selection vanished, including from a link they had already shared.
//
// null/undefined values are dropped; an empty string is kept, which is how the time picker
// clears `since` against `from`/`to` without losing the key.
window.setParams = (newState: Record<string, unknown>, load = false) => {
  const merged = { ...Object.fromEntries(new URLSearchParams(window.location.search)), ...newState };
  const url =
    '?' +
    new URLSearchParams(
      Object.entries(merged)
        .filter(([_key, value]) => value != null)
        .sort(([keyA], [keyB]) => keyA.localeCompare(keyB)) as [string, string][]
    ).toString();

  load ? window.location.assign(url) : history.replaceState(null, '', url);
};

// A time-range change refreshes the page's widgets in place; only the log explorer,
// which passes a form to the picker, still submits.
window.dispatchQueryUpdate = () => window.dispatchEvent(new CustomEvent('update-query'));

window.applyTimeRange = (timeRange, opts) => {
  window.updateTimePicker(timeRange, opts);
  window.dispatchQueryUpdate();
};

window.updateTimePicker = function (
  timeRange: { since?: string; from?: string; to?: string },
  opts?: { targetPr?: string; label?: string; skipSetParams?: boolean }
): string {
  const tp = opts?.targetPr || 'n';
  const rangeEl = document.getElementById(tp + '-currentRange');
  const picker = rangeEl?.closest<HTMLElement>('[data-live-range]');
  const inputEl = document.getElementById(tp + '-custom_range_input') as HTMLInputElement | null;
  let displayLabel = '';

  if (timeRange.since) {
    if (picker) picker.dataset.liveRange = 'true';
    if (inputEl) inputEl.value = timeRange.since;
    if (!opts?.skipSetParams) window.setParams({ since: timeRange.since, from: '', to: '' });
    if (opts?.label) {
      displayLabel = opts.label;
    } else {
      const units: Record<string, string> = { S: 'Second', M: 'Minute', H: 'Hour', D: 'Day' };
      const m = timeRange.since.match(/^(\d+)\s*([SMHD])$/i);
      displayLabel = m ? `Last ${m[1]} ${units[m[2].toUpperCase()] || m[2]}${m[1] !== '1' ? 's' : ''}` : 'Last ' + timeRange.since;
    }
    if (rangeEl) rangeEl.innerText = displayLabel;
  } else if (timeRange.from && timeRange.to) {
    if (picker) picker.dataset.liveRange = 'false';
    if (inputEl) inputEl.value = timeRange.from + '/' + timeRange.to;
    if (!opts?.skipSetParams) window.setParams({ from: timeRange.from, to: timeRange.to, since: '' });
    displayLabel = opts?.label ?? window.formatTimeRange(timeRange.from, timeRange.to);
    if (rangeEl) rangeEl.innerText = displayLabel;
  } else {
    console.warn('updateTimePicker: malformed timeRange — expected "since" or "from"+"to"', timeRange);
    return displayLabel;
  }
  const transport = picker?.parentElement?.querySelector<HTMLElement>('[data-time-transport]');
  if (transport) {
    transport.dataset.live = String(Boolean(timeRange.since));
    if (!timeRange.since && window.dashboardRefreshInterval > 0) window.setTimeRefreshInterval(transport, 0);
    else syncTimeTransports();
  }
  return displayLabel;
};

window.dashboardRefreshInterval = 0;
window.dashboardRefreshTimer = null;

const timeTransports = new Set<HTMLElement>();

const syncTimeTransports = () => {
  const running = window.dashboardRefreshInterval > 0;
  timeTransports.forEach((transport) => {
    if (!transport.isConnected) {
      timeTransports.delete(transport);
      return;
    }
    const liveRange = transport.dataset.live === 'true';
    const liveData = transport.closest<HTMLElement>('[data-live-data]');
    const rowToggle = liveData?.querySelector<HTMLInputElement>('[data-row-stream-toggle]');
    const rowsAvailable = Boolean(rowToggle && !rowToggle.disabled);
    const rowsRunning = !rowsAvailable || Boolean(rowToggle?.checked);
    const allLive = liveRange && running && rowsRunning;
    const transportRunning = liveData ? allLive : running;
    const state = !liveRange ? 'historical' : transportRunning ? 'live' : 'paused';
    transport.dataset.state = state;
    transport.dataset.interval = String(window.dashboardRefreshInterval);
    const picker = transport.parentElement?.querySelector<HTMLElement>('[data-live-range]');
    if (picker) picker.dataset.state = state;
    if (liveData) liveData.dataset.state = !liveRange ? 'historical' : allLive ? 'live' : !running && rowsAvailable && !rowsRunning ? 'paused' : !running ? 'refresh-paused' : 'stream-paused';
  });
};
window.syncTimeTransports = syncTimeTransports;

window.setTimeRefreshInterval = (_transport, interval) => {
  if (window.dashboardRefreshTimer) clearInterval(window.dashboardRefreshTimer);
  const running = interval > 0;
  window.dashboardRefreshInterval = running ? interval : 0;
  window.dashboardRefreshTimer = running
    ? setInterval(() => window.dispatchEvent(new CustomEvent('update-query', { detail: { source: 'auto-refresh' } })), interval)
    : null;
  syncTimeTransports();
};

window.initTimeTransport = (transport) => {
  timeTransports.add(transport);
  const live = transport.parentElement?.querySelector('[data-live-range="true"]') != null;
  transport.dataset.live = String(live);
  if (!live) window.setTimeRefreshInterval(transport, 0);
  else if (!window.dashboardRefreshTimer) window.setTimeRefreshInterval(transport, 15000);
  else syncTimeTransports();
};

window.destroyTimeTransport = (transport) => {
  timeTransports.delete(transport);
  if (timeTransports.size === 0 && window.dashboardRefreshInterval > 0) window.setTimeRefreshInterval(null, 0);
};
window.dispatchEvent(new CustomEvent('monoscope:time-transport-ready'));

const defaultTimeWindow = (transport?: HTMLElement | null) =>
  transport?.closest<HTMLElement>('[data-default-window]')?.dataset.defaultWindow || '15M';

window.toggleLiveRefresh = (transport) => {
  if (!transport) return;
  if (transport.dataset.live !== 'true') {
    window.applyTimeRange({ since: defaultTimeWindow(transport) });
    return;
  }
  window.setTimeRefreshInterval(transport, window.dashboardRefreshInterval > 0 ? 0 : 15000);
};

window.toggleLiveData = (liveData, transport) => {
  if (!liveData || liveData.dataset.liveMode === 'refresh-only') {
    window.toggleLiveRefresh(transport);
    return;
  }
  const rowToggle = liveData.querySelector<HTMLInputElement>('[data-row-stream-toggle]');
  const rowsAvailable = Boolean(rowToggle && !rowToggle.disabled);
  const rowsRunning = !rowsAvailable || Boolean(rowToggle?.checked);
  const allLive = transport?.dataset.live === 'true' && window.dashboardRefreshInterval > 0 && rowsRunning;
  if (allLive) {
    if (rowsAvailable && rowToggle?.checked) {
      rowToggle.checked = false;
      rowToggle.dispatchEvent(new Event('change', { bubbles: true }));
    }
    window.setTimeRefreshInterval(transport, 0);
    return;
  }
  if (rowsAvailable && rowToggle && !rowToggle.checked) {
    rowToggle.checked = true;
    rowToggle.dispatchEvent(new Event('change', { bubbles: true }));
  }
  if (transport?.dataset.live !== 'true') window.applyTimeRange({ since: defaultTimeWindow(transport) });
  window.setTimeRefreshInterval(transport, 15000);
};

window.shiftTimeRange = (direction, transport) => {
  const params = new URLSearchParams(window.location.search);
  const now = Date.now();
  const fromParam = Date.parse(params.get('from') || '');
  const toParam = Date.parse(params.get('to') || '');
  const since = params.get('since') || defaultTimeWindow(transport);
  const match = since.match(/^(\d+)\s*([SMHD])$/i);
  const unitMs: Record<string, number> = { S: 1000, M: 60000, H: 3600000, D: 86400000 };
  const relativeMs = match ? Number(match[1]) * unitMs[match[2].toUpperCase()] : 900000;
  const from = Number.isFinite(fromParam) ? fromParam : now - relativeMs;
  const to = Number.isFinite(toParam) ? toParam : now;
  const duration = to - from;
  const shiftedFrom = from + direction * duration;
  const shiftedTo = to + direction * duration;
  if (direction > 0 && shiftedTo >= now) window.applyTimeRange({ since });
  else window.applyTimeRange({ from: new Date(shiftedFrom).toISOString(), to: new Date(shiftedTo).toISOString() });
};

// Carry the page's time range and telemetry scope onto a nav link before it is followed.
// Rewrite just-in-time because either context can change after the link rendered.
function preservePageContext(target: EventTarget | null) {
  const link = (target as Element | null)?.closest?.('a[data-preserve-page-context]') as HTMLAnchorElement | null;
  if (!link) return;
  const next = new URL(link.href);
  const source = new URLSearchParams(window.location.search);
  if (TIME_PARAMS.some((key) => source.get(key))) {
    for (const key of TIME_PARAMS) next.searchParams.delete(key);
    copyParams(source, next.searchParams);
  }
  copyParams(source, next.searchParams, SCOPE_PARAMS);
  link.href = next.toString();
}

for (const type of ['pointerover', 'focusin', 'pointerdown'] as const) {
  document.addEventListener(type, (e) => preservePageContext(e.target), { capture: true });
}
document.addEventListener(
  'keydown',
  (e) => {
    if (e.key === 'Enter' || e.key === ' ') preservePageContext(e.target);
  },
  { capture: true }
);

window.addEventListener('setRefreshInterval', (event) => {
  const interval = Number((event as CustomEvent<{ interval?: number | string }>).detail?.interval);
  if (Number.isFinite(interval)) window.setTimeRefreshInterval(null, interval);
});

window.exportTableCsv = (selector, filename) => {
  const table = document.querySelector<HTMLTableElement>(selector);
  if (!table) return;
  const quote = (value: string) => `"${value.replaceAll('"', '""')}"`;
  const csv = Array.from(table.querySelectorAll('tr'))
    .filter((row) => getComputedStyle(row).display !== 'none')
    .map((row) =>
      Array.from(row.querySelectorAll<HTMLElement>('th,td'))
        .filter((cell) => getComputedStyle(cell).display !== 'none')
        .map((cell) => quote(cell.innerText.trim().replaceAll(/\s+/g, ' ')))
        .join(',')
    )
    .join('\n');
  const url = URL.createObjectURL(new Blob([csv], { type: 'text/csv;charset=utf-8' }));
  const link = Object.assign(document.createElement('a'), { href: url, download: filename });
  link.click();
  URL.revokeObjectURL(url);
};

function updateUrlState(key: string | string[], value: string, action: 'set' | 'delete' = 'set') {
  const params = new URLSearchParams(window.location.search);
  for (const k of Array.isArray(key) ? key : [key]) {
    if (action === 'delete') {
      params.delete(k);
    } else {
      params.set(k, value);
    }
  }
  // Keep the fragment: log-list.ts does the same on its own replaceState calls, and
  // dropping it here silently discarded any in-page anchor on every param update.
  window.history.replaceState({}, '', `${window.location.pathname}?${params}${window.location.hash}`);
}
window.updateUrlState = updateUrlState;

// Simple variable setter that updates the tablist element
window.setVariable = (key: string, value: string) => {
  // Find the variable tablist element by its data attribute or id
  const varElement = document.querySelector(`[data-variable="${key}"], #var-${key}`) as HTMLInputElement | HTMLSelectElement;

  if (varElement) {
    // Update the element's value
    varElement.value = value;

    // Trigger change event to update URL and notify other components
    varElement.dispatchEvent(new Event('change', { bubbles: true }));
  } else {
    // Fallback: directly update URL if element not found
    updateUrlState(`var-${key}`, value);
    window.dispatchEvent(new Event('update-query'));
  }
};

// Helper to get current variable value
window.getVariable = (key: string) => {
  const params = new URLSearchParams(window.location.search);
  return params.get(`var-${key}`) || '';
};

window.createTagify = (selectorOrElement: string | Element, options: any = {}) => {
  const defaultOptions = {
    skipInvalid: true,
    templates: {
      tag: tagifyTemplateFunc,
      dropdownItemNoMatch: (data: any) => `No match for: ${data.value}`,
    },
    editTags: { clicks: 2, keepInvalid: false },
    dropdown: {
      enabled: 0,
      maxItems: 50,
      fuzzySearch: true,
      position: 'input',
      caseSensitive: false,
      mapValueTo: 'name',
      searchKeys: ['value', 'name'],
    },
  };
  const element = typeof selectorOrElement === 'string' ? document.querySelector(selectorOrElement) : selectorOrElement;
  const merged = { ...defaultOptions, ...options, dropdown: { ...defaultOptions.dropdown, ...options.dropdown } };
  // editTags crashes in select mode (no tags to edit → closest() on undefined)
  if (merged.mode === 'select') merged.editTags = false;
  const tagify = new (window as any).Tagify(element, merged);
  // Position dropdown relative to scope, not hidden input (fixes top-left corner positioning)
  if (tagify.settings.mode === 'select') tagify.settings.dropdown.appendTarget = tagify.DOM.scope;
  return tagify;
};

function tagifyTemplateFunc(
  this: { settings: { classNames: { tag: string; tagX: string; tagText: string } }; getAttributes: (data: TagifyTagData) => string },
  tagData: TagifyTagData
) {
  return `<tag title="${tagData.value || tagData.email}"
               contenteditable='false'
               spellcheck='false'
               tabIndex="-1"
               class="${this.settings.classNames.tag} ${tagData.class || ''}"
               ${this.getAttributes(tagData)}>
                <x title='' class="${this.settings.classNames.tagX}" role='button' aria-label='remove tag'></x>
                <div><span class="${this.settings.classNames.tagText}">${tagData.name || tagData.value || tagData}</span></div>
       </tag>`;
}

window.tagifyTemplateFunc = tagifyTemplateFunc;

// Auto-initialize tagify inputs from data attributes
// Uses data-tagify-* prefix to avoid collision with Tagify's built-in data attribute handling
function initTagifyElement(el: HTMLElement) {
  const existing = (el as any)._tagifyInstance;
  // An htmx morph can swap Tagify's wrapper out from under the textarea while the
  // textarea itself is reused, leaving a live instance bound to detached DOM: the
  // field still accepts typing but shows no suggestions. Re-init when that happens.
  if (existing) {
    if (existing.DOM?.scope?.isConnected) {
      if (el.classList.contains('dash-variable-input')) reconcileVariableTag(el, existing);
      return;
    }
    try {
      existing.destroy();
    } catch {
      /* already gone */
    }
    (el as any)._tagifyInstance = null;
  }
  try {
    const hasInitialValue = Boolean((el as HTMLInputElement).value);
    const options: any = {};
    const wl = el.getAttribute('data-tagify-whitelist');
    if (wl) {
      try {
        options.whitelist = JSON.parse(wl);
      } catch (e) {
        console.error('[Tagify auto-init] Failed to parse whitelist:', el.id, e);
      }
    }
    if (el.hasAttribute('data-tagify-enforce-whitelist')) options.enforceWhitelist = true;
    if (el.hasAttribute('data-tagify-mode')) options.mode = el.getAttribute('data-tagify-mode');
    if (el.hasAttribute('data-tagify-text-prop')) options.tagTextProp = el.getAttribute('data-tagify-text-prop');

    const tagify = window.createTagify(el, options);
    (el as any)._tagifyInstance = tagify;

    const initial = el.getAttribute('data-tagify-initial');
    if (initial) {
      try {
        const tags = JSON.parse(initial);
        if (el.hasAttribute('data-tagify-resolve') && options.whitelist) {
          tagify.addTags(tags.map((id: any) => options.whitelist.find((v: any) => v.value === id || v.value == id)).filter(Boolean));
        } else {
          tagify.addTags(tags);
        }
      } catch (e) {
        console.error('[Tagify auto-init] Failed to parse initial tags:', el.id, e);
      }
    }

    // Lazy dashboard variables: options aren't server-rendered (scoped/dependent
    // vars skip the render-time scan). Resolve a selected raw value immediately;
    // otherwise defer the query until the dropdown opens. Coalesce the first request,
    // retain a populated result, and let an empty or failed result retry on open.
    if (
      el.classList.contains('dash-variable-input') &&
      !options.whitelist?.length &&
      (el.getAttribute('data-tagify-query-sql') || el.getAttribute('data-tagify-query'))
    ) {
      let fetched = false;
      const fetchOptions = () => {
        if (fetched) return;
        fetched = true;
        (window as any).reloadVarWhitelist(el).finally(() => {
          if (!tagify.settings.whitelist?.length) fetched = false;
        });
      };
      if (hasInitialValue) fetchOptions();
      tagify.on('dropdown:show', fetchOptions);
    }

    // Dashboard variable: sync tagify changes to URL params and fire update-query
    if (el.classList.contains('dash-variable-input')) {
      tagify.on('change', (e: any) => {
        if (suppressVarChange.has(el)) return;
        publishVarValue(el, e.detail?.tagify?.value[0]?.value || '');
      });
    }
  } catch (e) {
    console.error('[Tagify auto-init] Failed to init element:', el.id, e);
  }
}

// Interpolate {{var-*}} placeholders in elements with data-var-template
let _cachedSearch = '',
  _cachedParams: URLSearchParams | null = null,
  _interpolatePending = false;
(window as any).interpolateVarTemplates = function () {
  if (_interpolatePending) return;
  _interpolatePending = true;
  requestAnimationFrame(() => {
    _interpolatePending = false;
    if (window.location.search !== _cachedSearch) {
      _cachedSearch = window.location.search;
      _cachedParams = new URLSearchParams(_cachedSearch);
    }
    document.querySelectorAll('[data-var-template]').forEach((el: any) => {
      let text = el.dataset.varTemplate;
      _cachedParams!.forEach((value, key) => {
        if (key.startsWith('var-')) text = text.replaceAll('{{' + key + '}}', value || '');
      });
      el.textContent = text;
    });
  });
};

// Fetch a dashboard variable's option whitelist from /chart_data, resolving the
// variable's SQL/KQL against the current URL params (so scoped/dependent vars
// like Resource-by-Service stay correct). Server-side rendering skips computing
// these to keep the multi-second DISTINCT scan off the page critical path, so we
// load them client-side: lazily on first dropdown open, and again on update-query.
// Writing a variable's value to the URL is what makes every widget refetch, so a
// programmatic reconcile must publish once (the final value) rather than once per
// tagify mutation — removeAllTags + addTags would otherwise send every widget on a
// round trip for the empty value first.
const suppressVarChange = new WeakSet<HTMLElement>();
function reconcileVariableTag(input: HTMLElement, tagify: any) {
  const name = input.getAttribute('name');
  if (!name) return;
  const expected = new URL(location.href).searchParams.get(`var-${name}`) || '';
  const selected = tagify.value?.[0];
  if (String(selected?.value ?? '') === expected && (!expected || tagify.DOM.scope.querySelector('tag'))) return;
  const replacement = tagify.settings.whitelist?.find((option: any) => String(typeof option === 'object' ? option.value : option) === expected) ?? { value: expected, name: expected };
  suppressVarChange.add(input);
  try {
    tagify.removeAllTags();
    if (expected) tagify.addTags([replacement]);
  } finally {
    suppressVarChange.delete(input);
  }
}

function publishVarValue(input: HTMLElement, value: string) {
  const url = new URL(window.location.href);
  const key = 'var-' + input.getAttribute('name');
  if (url.searchParams.get(key) === value) return;
  url.searchParams.set(key, value);
  history.pushState({}, '', url.toString());
  window.dispatchEvent(new Event('update-query'));
}

const variableRefreshes = new WeakMap<HTMLElement, { url: string; request: Promise<void> }>();
const variableLastRefresh = new WeakMap<HTMLElement, { url: string; startedAt: number }>();
const VARIABLE_REFRESH_INTERVAL = 5 * 60_000;

function reloadVarWhitelist(input: HTMLElement, background = false): Promise<void> {
  const querySql = input.getAttribute('data-tagify-query-sql') || '';
  const query = input.getAttribute('data-tagify-query') || '';
  if (!querySql && !query) return Promise.resolve();
  const tgfy = (input as any)._tagifyInstance;
  if (!tgfy) return Promise.resolve();
  const params = new URLSearchParams({
    ...Object.fromEntries(new URLSearchParams(location.search)),
    pid: input.dataset.projectId || '',
    query,
    query_sql: querySql,
    data_type: 'text',
  });
  if (input.dataset.dashboardId) params.set('dashboard_id', input.dataset.dashboardId);
  // The statement belongs to one store. Omitting this routed postgres-only variable
  // queries (apis.endpoints) at TimeFusion, which answers "table not found".
  const dbSource = input.getAttribute('data-tagify-db-source');
  if (dbSource) params.set('db_source', dbSource);
  const url = `/chart_data?${params}`;
  const active = variableRefreshes.get(input);
  if (active?.url === url) return active.request;
  const last = variableLastRefresh.get(input);
  if (background && last?.url === url && Date.now() - last.startedAt < VARIABLE_REFRESH_INTERVAL) return Promise.resolve();
  // Pace failed attempts too, so an unavailable backend is not retried on every tick.
  variableLastRefresh.set(input, { url, startedAt: Date.now() });
  const refresh = { url, request: Promise.resolve() };
  variableRefreshes.set(input, refresh);
  refresh.request = (async () => {
    try {
      const response = await fetch(url);
      if (!response.ok) throw new Error(`Variable options request failed: ${response.status}`);
      const { data_text, error } = await response.json();
      if (error) throw new Error(error);
      if (!Array.isArray(data_text)) throw new Error('Variable options response is missing data');
      if (variableRefreshes.get(input) !== refresh || (input as any)._tagifyInstance !== tgfy || !input.isConnected) return;
      const valueOf = (option: any) => String(typeof option === 'object' ? option.value : option);
      const dedupe = (options: any[]) => {
        const seen = new Set<string>();
        return options.filter((o) => !seen.has(valueOf(o)) && (seen.add(valueOf(o)), true));
      };
      const fetched = data_text.filter((row: unknown) => Array.isArray(row) && row.length > 0).map((row: any[]) => (row.length === 1 ? row[0] : { value: row[0], name: row[1] }));
      if (background) {
        // Keep the picker usable, including its open menu, search text and selected tags.
        // A quiet time window must not remove values the user may still need to select.
        const existing = tgfy.settings.whitelist ?? [];
        tgfy.settings.whitelist = dedupe([...existing, ...fetched]);
      } else {
        // An explicit change (a parent variable, the time range) rescopes the list: the
        // previous options belong to the value the user just replaced, so merging them in
        // left e.g. the Endpoint picker offering the *old* domain's endpoints.
        const options = dedupe(fetched);
        tgfy.settings.whitelist = options;
        // Replace an invalid selection, or rebuild a valid raw value so newly fetched
        // display fields (such as an endpoint's method and path) become visible.
        const selected = tgfy.value?.[0];
        const held = selected?.value;
        const matching = options.find((o: any) => valueOf(o) === String(held));
        if (held !== undefined && (!matching || Object.entries(matching).some(([key, value]) => selected[key] !== value))) {
          const replacement = matching ?? options[0];
          suppressVarChange.add(input);
          try {
            tgfy.removeAllTags();
            if (replacement) tgfy.addTags([replacement]);
          } finally {
            suppressVarChange.delete(input);
          }
          if (!matching) publishVarValue(input, replacement ? valueOf(replacement) : '');
        }
        if (tgfy.state?.dropdown?.visible) tgfy.dropdown.show(tgfy.state.inputText || '');
      }
    } catch (e) {
      console.error(`Error fetching data for ${(input as any).name}:`, e);
    } finally {
      if (variableRefreshes.get(input) === refresh) variableRefreshes.delete(input);
    }
  })();
  return refresh.request;
}
(window as any).reloadVarWhitelist = reloadVarWhitelist;

// Reload whitelist for dashboard variables with data-tagify-reload-on-change on update-query
window.addEventListener('update-query', (event) => {
  const background = (event as CustomEvent).detail?.source === 'auto-refresh';
  document.querySelectorAll<HTMLElement>('.dash-variable-input[data-tagify-reload-on-change="true"]').forEach((input) => reloadVarWhitelist(input, background));
  (window as any).interpolateVarTemplates();
});

// Reflect the query editor's current KQL onto Log Explorer facet checkboxes,
// matching each `data-field == "data-value"` fragment. Runs on query changes and
// after HTMX swaps (facets are swapped in via morph, so DOMContentLoaded won't do).
// The fragment's value is quote-terminated but its field prefix is not, so a bare
// substring test lets `status == "ok"` match inside `http_status == "ok"`; require a
// left token boundary (start, or a non field-name char) before the field.
function fragmentInQuery(query: string, fragment: string): boolean {
  for (let i = query.indexOf(fragment); i >= 0; i = query.indexOf(fragment, i + 1)) {
    if (!/[\w.]/.test(query[i - 1] ?? '')) return true;
  }
  return false;
}
function syncFacetCheckboxes(root: Document | Element = document) {
  // Before upgrade, read the editable server input so early typing is reflected in facets.
  const el = document.getElementById('filterElement') as any;
  const query = el?.getValue?.() ?? el?.querySelector('textarea[data-query-input]')?.value ?? el?.getAttribute('default-value') ?? '';
  root.querySelectorAll<HTMLInputElement>('input[type="checkbox"][data-field][data-value]').forEach((cb) => {
    cb.checked = fragmentInQuery(query, `${cb.dataset.field} == "${cb.dataset.value}"`);
  });
}
window.addEventListener('update-query', () => syncFacetCheckboxes());

function initAllTagifyInputs(root: Document | Element = document) {
  root.querySelectorAll<HTMLElement>('[data-tagify]').forEach(initTagifyElement);
}

window.getTagValues = (selector: string): string[] => {
  const el = document.querySelector(selector);
  return (el as any)?._tagifyInstance?.value?.map((t: any) => t.value || t) || [];
};

// Init tagify elements - run now, on DOMContentLoaded, and after HTMX swaps
initAllTagifyInputs();
(window as any).interpolateVarTemplates();
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', () => {
    initAllTagifyInputs();
    (window as any).interpolateVarTemplates();
  });
}
document.addEventListener('htmx:after:swap', (e: any) => {
  // Out-of-band fragments (such as dashboard variables) sit outside the main
  // swap target in e.detail.elt, so include them in the post-swap scan.
  initAllTagifyInputs();
  (window as any).interpolateVarTemplates();
  syncFacetCheckboxes(e.detail?.elt || document);
});
