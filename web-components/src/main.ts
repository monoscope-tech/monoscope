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

function bindFunctionsToObjects(rootObj: any, obj: any) {
  if (!obj || typeof obj !== 'object') return;

  Object.keys(obj).forEach((key) => {
    const value = obj[key];
    if (typeof value === 'function') {
      obj[key] = value.bind(rootObj);
    } else if (value && typeof value === 'object') {
      bindFunctionsToObjects(rootObj, value);
    }
  });

  return obj;
}
window.bindFunctionsToObjects = bindFunctionsToObjects;

function getUTCOffset() {
  const now = new Date();
  const offset = now.getTimezoneOffset();
  const sign = offset > 0 ? '-' : '+';
  const absOffset = Math.abs(offset);
  const hours = String(Math.floor(absOffset / 60)).padStart(2, '0');
  const minutes = String(absOffset % 60).padStart(2, '0');
  return `UTC${sign}${hours}:${minutes}`;
}

window.getUTCOffset = getUTCOffset;
