import { html, TemplateResult } from 'lit';
import { faSprite_, replaceVariables } from './test-editor-utils';
import { Assertion, HttpMethod, Step } from '../types/types';

// ------------------------
// Types
// ------------------------

type HeadersObject = Record<string, string>;

interface JsonResponse {
  [key: string]: any;
}

interface RequestMetadata {
  http_method: HttpMethod;
  headers: HeadersObject;
  json: JsonResponse | null;
  raw: string | null;
}

interface ResponseMetadata {
  status: number;
  duration_ms: string;
  headers: HeadersObject;
  json: JsonResponse;
  raw: string;
}

interface ProcessedResponse {
  req: RequestMetadata;
  resp: ResponseMetadata;
}

export async function makeRequestAndProcessResponse(requestObject: Step): Promise<ProcessedResponse> {
  try {
    const url = new URL(replaceVariables(requestObject._url || ''));
    const method = requestObject._method;
    const headers = requestObject.headers || {};
    const rawBody = requestObject._requestBody ?? requestObject._json ?? null;

    const options: RequestInit = {
      method,
      headers: {
        ...headers,
        contentType: requestObject._requestType ?? '',
      },
      redirect: requestObject.followRedirects ? 'follow' : 'manual',
    };

    if (method !== 'GET') {
      options.body = rawBody;
    }

    const injectedHeaders: HeadersObject = {
      'user-agent': navigator.userAgent || 'CustomAgent/1.0',
      accept: '*/*',
      connection: 'keep-alive',
      host: url.host,
    };

    const startTime = performance.now();
    const response = await fetch(url, options);
    const endTime = performance.now();
    const duration_ms = endTime - startTime;

    const status = response.status;
    const responseHeaders: HeadersObject = {};
    response.headers.forEach((value, key) => {
      responseHeaders[key] = value;
    });

    const rawResponse = await response.text();
    let jsonResponse: JsonResponse = {};
    try {
      jsonResponse = JSON.parse(rawResponse);
    } catch {}

    return {
      resp: {
        status,
        duration_ms: duration_ms.toFixed(2),
        headers: responseHeaders,
        json: jsonResponse,
        raw: rawResponse,
      },
      req: {
        http_method: method!,
        headers,
        json: rawBody ? JSON.parse(rawBody) : null,
        raw: rawBody,
      },
    };
  } catch (error) {
    return Promise.reject(error);
  }
}

// ------------------------
// Preview Generator
// ------------------------

export function generateRequestPreviewFromObject(requestObject: Step): string {
  let url: URL | string = '';
  try {
    url = new URL(replaceVariables(requestObject._url || ''));
  } catch {
    url = requestObject._url || '';
  }

  const method = requestObject._requestType === 'raw' ? 'POST' : 'GET';
  const userHeaders = requestObject.headers || {};
  const httpVersion = requestObject.httpVersion || 'HTTP/1.1';

  let requestPreview = `<p>${method.toUpperCase()} ${typeof url === 'string' ? url : url.href} ${httpVersion}<p/>`;

  let headersPreview = '';
  if (httpVersion === 'HTTP/2.0' && typeof url !== 'string') {
    headersPreview += `<p>:authority: ${url.host}<p/>`;
    headersPreview += `<p>:method: ${method.toUpperCase()}<p/>`;
    headersPreview += `<p>:path: ${url.pathname}<p/>`;
  }

  const injectedHeaders: HeadersObject = {
    'user-agent': navigator.userAgent || 'CustomAgent/1.0',
    accept: '*/*',
    connection: 'keep-alive',
    host: typeof url === 'string' ? '' : url.host,
  };

  const allHeaders = { ...injectedHeaders, ...userHeaders };

  for (const [key, value] of Object.entries(allHeaders)) {
    headersPreview += `<p>${key.toLowerCase()}: ${value}<p/>`;
  }

  return requestPreview + headersPreview;
}

// ------------------------
// JSON Renderer
// ------------------------

export function renderJsonWithIndentation(
  json: JsonResponse,
  addAssertion: (e: Event, assertion: Assertion) => void,
  path = '',
  depth = 0
): TemplateResult {
  const padding = `${depth * 20}px`;
  return html`
    ${Object.entries(json).map(([key, value]) => {
      const currentPath = Array.isArray(json) ? `${path}.[${key}]` : `${path}.${key}`;
      const assertionObj: Assertion = {
        type: 'body',
        operation: 'jsonpath',
        jsonpath: currentPath,
        subOperation: 'equals',
        value: typeof value !== 'object' ? value : '',
        status: 'PASSED',
      };
      return html`
        <div style="padding-left: ${padding};">
          <div class="flex items-center gap-4">
            <span> ${key}: ${typeof value === 'object' && value ? '' : JSON.stringify(value)} </span>
            <button
              class="rounded-full border fill-textDisabled shadow-[0px_4px_4px_0px_rgba(0,0,0,0.06)] border-strokeWeak shadown-sm p-1.5 bg-bgBase"
              @pointerdown="${(e: Event) => addAssertion(e, assertionObj)}"
            >
              ${faSprite_('plus', 'regular', 'w-3 h-3')}
            </button>
          </div>
          ${typeof value === 'object' && value ? renderJsonWithIndentation(value, addAssertion, currentPath, depth + 1) : ''}
        </div>
      `;
    })}
  `;
}
