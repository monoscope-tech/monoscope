(window as any).htmx.defineExtension('debug', {
  onEvent: function (name: string, evt: any) {
    if (console.debug) {
      console.debug(name, evt);
    } else if (console) {
      console.log('DEBUG:', name, evt);
    } else {
      throw new Error('NO CONSOLE SUPPORTED');
    }
  },
});

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
(window as any).htmx.defineExtension('forward-page-params', {
  onEvent: function (name: string, evt: any) {
    if (name === 'htmx:configRequest') {
      // Process GET and POST requests (add params to URL query string)
      if (evt.detail.verb === 'get' || evt.detail.verb === 'post') {
        const url = new URL(evt.detail.path, window.location.origin);
        const currentParams = new URLSearchParams(window.location.search);

        // Forward URL params first (they take precedence)
        currentParams.forEach((value: string, key: string) => {
          if (!url.searchParams.has(key)) {
            url.searchParams.set(key, value);
          }
        });

        // Add dashboard constants as fallback (only if not in URL)
        const constants = getDashboardConstants(evt.detail.elt);
        Object.entries(constants).forEach(([key, value]) => {
          if (!url.searchParams.has(key)) {
            url.searchParams.set(key, value);
          }
        });

        // Update the path with merged parameters
        evt.detail.path = url.pathname + url.search;
      }
    }
    return true;
  },
});

// Attach functions to the window object
window.buildCurlRequest = function (event: any) {
  const { request_headers, request_body, method, host, raw_url } = JSON.parse(event.currentTarget?.dataset.reqjson);
  let curlCommand = `curl -X ${method} https://${host}${raw_url} \\\n `;

  const curlHeaders =
    typeof request_headers === 'object'
      ? Object.entries(request_headers)
          .map(([key, value]) => `-H "${key} ${value}" \\\n`)
          .join('')
      : '';
  curlCommand += curlHeaders;

  const reqBody =
    method.toLowerCase() !== 'get'
      ? typeof request_body === 'object'
        ? ` -d '${JSON.stringify(request_body)}' \\\n`
        : `-data-raw "${request_body}"  \\\n`
      : '';
  if (reqBody) curlCommand += reqBody;

  navigator.clipboard.writeText(curlCommand).then(() => {
    document.querySelector('body')!.dispatchEvent(
      new CustomEvent('successToast', {
        detail: { value: ['Curl command copied'] },
        bubbles: true,
        composed: true,
      })
    );
  });
};

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
  const json = event.currentTarget.dataset.reqjson;
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
    if (params().since == '') {
      return { since: '14D', from: params().from, to: params().to };
    }
    return { since: params().since, from: params().from, to: params().to };
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
  opts?: { targetPr?: string; label?: string }
) {
  const tp = opts?.targetPr || 'n';
  const rangeEl = document.getElementById(tp + '-currentRange');
  const inputEl = document.getElementById(tp + '-custom_range_input') as HTMLInputElement | null;
  const formatLocal = (d: string) => new Date(d).toLocaleString();

  if (timeRange.since) {
    if (inputEl) inputEl.value = timeRange.since;
    window.setParams({ since: timeRange.since, from: '', to: '' });
    if (rangeEl) {
      if (opts?.label) {
        rangeEl.innerText = opts.label;
      } else {
        const units: Record<string, string> = { S: 'Second', M: 'Minute', H: 'Hour', D: 'Day' };
        const m = timeRange.since.match(/^(\d+)\s*([SMHD])$/i);
        rangeEl.innerText = m
          ? `Last ${m[1]} ${units[m[2].toUpperCase()] || m[2]}${m[1] !== '1' ? 's' : ''}`
          : 'Last ' + timeRange.since;
      }
    }
  } else if (timeRange.from && timeRange.to) {
    if (inputEl) inputEl.value = timeRange.from + '/' + timeRange.to;
    window.setParams({ from: timeRange.from, to: timeRange.to, since: '' });
    if (rangeEl) rangeEl.innerText = opts?.label ?? (formatLocal(timeRange.from) + ' - ' + formatLocal(timeRange.to));
  } else {
    console.warn('updateTimePicker: malformed timeRange — expected "since" or "from"+"to"', timeRange);
  }
};

window.updateMarkAreas = function (chartId: string, warningVal: string, incidentVal: string) {
  const warning = parseInt(warningVal, 10),
    incident = parseInt(incidentVal, 10),
    myChart = (window as any).echarts.getInstanceByDom(document.getElementById(chartId)),
    options = myChart.getOption();

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
                  itemStyle: { color: 'rgba(255, 212, 0, 0.4)' },
                },
                { yAxis: incident },
              ],
            ]
          : []),
        [
          {
            name: 'Incident',
            yAxis: incident,
            itemStyle: { color: 'rgba(255, 173, 177, 0.5)' },
          },
          { yAxis: 'max' },
        ],
      ],
    };
  });
  myChart.setOption({ series: options.series }, false);
};

function updateUrlState(key: string, value: string, action: 'set' | 'delete' = 'set') {
  const params = new URLSearchParams(window.location.search);
  if (action === 'delete') {
    params.delete(key);
  } else {
    params.set(key, value);
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
      dropdownItemNoMatch: (data: any) => `No suggestion found for: ${data.value}`,
    },
    editTags: { clicks: 2, keepInvalid: false },
    dropdown: {
      enabled: 0,
      maxItems: 50,
      fuzzySearch: true,
      position: 'input',
      place: 'parent',
      caseSensitive: false,
      mapValueTo: 'name',
      searchKeys: ['value', 'name'],
      // appendTarget: function () {
      //   return this.DOM.scope;
      // },
    },
  };
  const element = typeof selectorOrElement === 'string' ? document.querySelector(selectorOrElement) : selectorOrElement;
  return new (window as any).Tagify(element, { ...defaultOptions, ...options });
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

// Reload whitelist for dashboard variables with data-tagify-reload-on-change on update-query
window.addEventListener('update-query', async () => {
  document.querySelectorAll<HTMLElement>('.dash-variable-input[data-tagify-reload-on-change="true"]').forEach(async (input) => {
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
  });
  (window as any).interpolateVarTemplates();
});

function initAllTagifyInputs(root: Document | Element = document) {
  root.querySelectorAll<HTMLElement>('[data-tagify]').forEach(initTagifyElement);
}

window.getTagValues = (selector: string): string[] => {
  const el = document.querySelector(selector);
  return (el as any)?._tagifyInstance?.value?.map((t: any) => t.value || t) || [];
};

// Signal that web components are ready
(window as any).widgetDepsReady = true;
window.dispatchEvent(new CustomEvent('widgetDepsReady'));

// Init tagify elements - run now, on DOMContentLoaded, and after HTMX swaps
initAllTagifyInputs();
(window as any).interpolateVarTemplates();
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', () => { initAllTagifyInputs(); (window as any).interpolateVarTemplates(); });
}
document.addEventListener('htmx:afterSettle', (e: any) => { initAllTagifyInputs(e.detail?.elt || document); (window as any).interpolateVarTemplates(); });
