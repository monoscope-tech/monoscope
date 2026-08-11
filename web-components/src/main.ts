// htmx 4 replaced defineExtension({onEvent}) with registerExtension(name, hooks), where each
// hook is named after its event with ':' → '_' (htmx:config:request → htmx_config_request) and
// is called as (elt, detail). Every request-lifecycle detail is `{ctx}` — the mutable request
// lives at `detail.ctx.request`, NOT `detail.request`; reading the wrong one silently no-ops.
// Registration is global — v4 dropped hx-ext as the activation mechanism — so each hook gates
// itself on the hx-ext marker attribute the call sites already carry.
const htmx4 = (window as any).htmx;
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

window.setQueryParamAndReload = (key: string, value: string) => {
  const url = new URL(window.location.href);
  url.searchParams.set(key, value);
  if (key === 'source') {
    url.searchParams.delete('queryAST');
    url.searchParams.delete('query');
    url.searchParams.delete('cols');
    url.searchParams.delete('target-spans');
    url.searchParams.delete('details_width');
    url.searchParams.delete('target_event');
    url.searchParams.delete('showTrace');
  }
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

window.setParams = (
  (state = { ...Object.fromEntries(new URLSearchParams(window.location.search)) }) =>
  (newState: any, load = false) => {
    Object.assign(state, newState);

    const url =
      '?' +
      new URLSearchParams(
        Object.entries(state)
          .filter(([_key, value]) => value != null)
          .sort(([keyA], [keyB]) => keyA.localeCompare(keyB))
      ).toString();

    load ? window.location.assign(url) : history.replaceState(null, '', url);
  }
)();

window.updateTimePicker = function (
  timeRange: { since?: string; from?: string; to?: string },
  opts?: { targetPr?: string; label?: string; skipSetParams?: boolean }
): string {
  const tp = opts?.targetPr || 'n';
  const rangeEl = document.getElementById(tp + '-currentRange');
  const inputEl = document.getElementById(tp + '-custom_range_input') as HTMLInputElement | null;
  const formatLocal = (d: string) => new Date(d).toLocaleString();
  let displayLabel = '';

  if (timeRange.since) {
    if (inputEl) inputEl.value = timeRange.since;
    if (!opts?.skipSetParams) window.setParams({ since: timeRange.since, from: '', to: '' });
    if (opts?.label) {
      displayLabel = opts.label;
    } else {
      const units: Record<string, string> = { S: 'Second', M: 'Minute', H: 'Hour', D: 'Day' };
      const m = timeRange.since.match(/^(\d+)\s*([SMHD])$/i);
      displayLabel = m
        ? `Last ${m[1]} ${units[m[2].toUpperCase()] || m[2]}${m[1] !== '1' ? 's' : ''}`
        : 'Last ' + timeRange.since;
    }
    if (rangeEl) rangeEl.innerText = displayLabel;
  } else if (timeRange.from && timeRange.to) {
    if (inputEl) inputEl.value = timeRange.from + '/' + timeRange.to;
    if (!opts?.skipSetParams) window.setParams({ from: timeRange.from, to: timeRange.to, since: '' });
    displayLabel = opts?.label ?? (formatLocal(timeRange.from) + ' - ' + formatLocal(timeRange.to));
    if (rangeEl) rangeEl.innerText = displayLabel;
  } else {
    console.warn('updateTimePicker: malformed timeRange — expected "since" or "from"+"to"', timeRange);
  }
  return displayLabel;
};

// Convert CSS color (including oklch) to hex via pixel rendering
const _maCanvas = document.createElement('canvas');
_maCanvas.width = 1; _maCanvas.height = 1;
const _maCtx = _maCanvas.getContext('2d', { willReadFrequently: true })!;
const cssToHex = (token: string, fallback: string): string => {
  const val = getComputedStyle(document.body).getPropertyValue(token).trim() || fallback;
  _maCtx.clearRect(0, 0, 1, 1);
  _maCtx.fillStyle = val;
  _maCtx.fillRect(0, 0, 1, 1);
  const [r, g, b] = _maCtx.getImageData(0, 0, 1, 1).data;
  return '#' + [r, g, b].map(v => v.toString(16).padStart(2, '0')).join('');
};

window.updateMarkAreas = function (chartId: string, warningVal: string, incidentVal: string) {
  const warning = parseInt(warningVal, 10),
    incident = parseInt(incidentVal, 10),
    myChart = (window as any).echarts.getInstanceByDom(document.getElementById(chartId)),
    options = myChart.getOption(),
    modAlpha = (window as any).echarts.color.modifyAlpha,
    warningColor = modAlpha(cssToHex('--color-fillWarning-strong', '#ffd400'), 0.4),
    errorColor = modAlpha(cssToHex('--color-fillError-strong', '#ffadb1'), 0.5);

  options.series.forEach((series: any) => {
    series.markArea = {
      label: { show: false },
      data: [
        ...(!isNaN(warning)
          ? [
              [
                {
                  name: 'Warning',
                  yAxis: warning,
                  itemStyle: { color: warningColor },
                },
                { yAxis: incident },
              ],
            ]
          : []),
        [
          {
            name: 'Incident',
            yAxis: incident,
            itemStyle: { color: errorColor },
          },
          { yAxis: 'max' },
        ],
      ],
    };
  });
  myChart.setOption({ series: options.series }, false);
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
  window.history.replaceState({}, '', `${window.location.pathname}?${params}`);
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
      tag: window.tagifyTemplateFunc,
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

function tagifyTemplateFunc(tagData: any) {
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

(window as any).tagifyTemplateFunc = tagifyTemplateFunc;

// Auto-initialize tagify inputs from data attributes
// Uses data-tagify-* prefix to avoid collision with Tagify's built-in data attribute handling
function initTagifyElement(el: HTMLElement) {
  if ((el as any)._tagifyInstance) return;
  try {
    const options: any = {};
    const wl = el.getAttribute('data-tagify-whitelist');
    if (wl) {
      try { options.whitelist = JSON.parse(wl); } catch (e) {
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
    // vars skip the render-time scan). Fetch the whitelist the first time the
    // dropdown opens, so opening the picker is what pays the query cost, not the
    // page load. Guarded so it only fetches once per instance.
    if (el.classList.contains('dash-variable-input') && !options.whitelist?.length && (el.getAttribute('data-tagify-query-sql') || el.getAttribute('data-tagify-query'))) {
      let fetched = false;
      tagify.on('dropdown:show', () => {
        if (fetched) return;
        fetched = true;
        (window as any).reloadVarWhitelist(el);
      });
    }

    // Dashboard variable: sync tagify changes to URL params and fire update-query
    if (el.classList.contains('dash-variable-input')) {
      tagify.on('change', (e: any) => {
        const varName = e.detail.tagify.DOM.originalInput.getAttribute('name');
        const url = new URL(window.location.href);
        url.searchParams.set('var-' + varName, e.detail?.tagify?.value[0]?.value || '');
        history.pushState({}, '', url.toString());
        window.dispatchEvent(new Event('update-query'));
      });
    }
  } catch (e) {
    console.error('[Tagify auto-init] Failed to init element:', el.id, e);
  }
}

// Interpolate {{var-*}} placeholders in elements with data-var-template
let _cachedSearch = '', _cachedParams: URLSearchParams | null = null, _interpolatePending = false;
(window as any).interpolateVarTemplates = function () {
  if (_interpolatePending) return;
  _interpolatePending = true;
  requestAnimationFrame(() => {
    _interpolatePending = false;
    if (window.location.search !== _cachedSearch) { _cachedSearch = window.location.search; _cachedParams = new URLSearchParams(_cachedSearch); }
    document.querySelectorAll('[data-var-template]').forEach((el: any) => {
      let text = el.dataset.varTemplate;
      _cachedParams!.forEach((value, key) => { if (key.startsWith('var-')) text = text.replaceAll('{{' + key + '}}', value || ''); });
      el.textContent = text;
    });
  });
};

// Fetch a dashboard variable's option whitelist from /chart_data, resolving the
// variable's SQL/KQL against the current URL params (so scoped/dependent vars
// like Resource-by-Service stay correct). Server-side rendering skips computing
// these to keep the multi-second DISTINCT scan off the page critical path, so we
// load them client-side: lazily on first dropdown open, and again on update-query.
async function reloadVarWhitelist(input: HTMLElement) {
  const querySql = input.getAttribute('data-tagify-query-sql') || '';
  const query = input.getAttribute('data-tagify-query') || '';
  if (!querySql && !query) return;
  const tgfy = (input as any)._tagifyInstance;
  try {
    tgfy?.loading(true);
    const params = new URLSearchParams({ ...Object.fromEntries(new URLSearchParams(location.search)), query, query_sql: querySql, data_type: 'text' });
    const { data_text } = await fetch(`/chart_data?${params}`).then(res => res.json());
    if (tgfy) { tgfy.settings.whitelist = data_text.map((i: any) => i.length === 1 ? i[0] : { value: i[0], name: i[1] }); tgfy.loading(false); }
  } catch (e) { console.error(`Error fetching data for ${(input as any).name}:`, e); }
}
(window as any).reloadVarWhitelist = reloadVarWhitelist;

// Reload whitelist for dashboard variables with data-tagify-reload-on-change on update-query
window.addEventListener('update-query', async () => {
  document.querySelectorAll<HTMLElement>('.dash-variable-input[data-tagify-reload-on-change="true"]').forEach(reloadVarWhitelist);
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
  const query = (document.getElementById('filterElement') as any)?.editor?.getValue() ?? '';
  root.querySelectorAll<HTMLInputElement>('input[type="checkbox"][data-field][data-value]').forEach(cb => {
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
  document.addEventListener('DOMContentLoaded', () => { initAllTagifyInputs(); (window as any).interpolateVarTemplates(); });
}
document.addEventListener('htmx:after:swap', (e: any) => { initAllTagifyInputs(e.detail?.elt || document); (window as any).interpolateVarTemplates(); syncFacetCheckboxes(e.detail?.elt || document); });
