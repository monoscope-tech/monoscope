import { html, TemplateResult } from 'lit-html';
import { Assertion, AssertType, HttpMethod, Step, TestkitStep } from '../types/types';
import * as yaml from 'js-yaml';

export function faSprite_(iconName: string, kind: string, classes: string): TemplateResult {
  return html`<svg class="${classes}"><use href="/public/assets/svgs/fa-sprites/${kind}.svg#${iconName}"></use></svg>`;
}

('use strict');

type ToastEventDetail = { value: string[] };

type ExportItem = {
  key: string;
  value: string;
  category: 'body' | 'header' | 'status' | 'responseTime';
};

type ErrorMap = Record<string, any>;

export function getEvent(eventName: string, value: ToastEventDetail): CustomEvent<ToastEventDetail> {
  return new CustomEvent(eventName, {
    detail: value,
    bubbles: true,
    composed: true,
  });
}

const METHODS: HttpMethod[] = ['GET', 'POST', 'PATCH', 'PUT', 'DELETE', 'HEAD', 'OPTIONS', 'CONNECT', 'TRACE'];
const ASSERTS: AssertType[] = ['exists', 'number', 'string', 'array', 'date', 'boolean', 'ok', 'empty', 'notEmpty', 'null'];

export function triggerToastEvent(event: CustomEvent): void {
  document.querySelector('body')?.dispatchEvent(event);
}

export function mergeErrors(errors?: ErrorMap, errs?: ErrorMap): ErrorMap | undefined {
  if (!errs) return errors;
  return { ...(errors || {}), ...errs };
}

export function isValidStep(step: Step): ErrorMap | undefined {
  let errors: ErrorMap | undefined;
  errors = mergeErrors(errors, getMethodAndUrlErrors(step));
  if (step.asserts) errors = mergeErrors(errors, getAssertErrors(step.asserts));
  if (step.headers) errors = mergeErrors(errors, getHeaderErrors(step.headers));
  return errors;
}

export function getHeaderErrors(headers: any): ErrorMap | undefined {
  if (typeof headers !== 'object' || headers === null || Array.isArray(headers)) {
    return { headers: 'Header must be key value object pair' };
  }
  const errors: any[] = [];
  for (const [key, value] of Object.entries(headers)) {
    let err: Record<string, string> | undefined = undefined;
    if (typeof key !== 'string' || key.trim() === '') {
      err = { key: 'Header key must be a non empty string' };
    }
    if (typeof value !== 'string') {
      err = { ...(err || {}), value: 'Header value must be a non empty string' };
    }
    if (err) errors.push(err);
  }
  return errors.length > 0 ? { headers: errors } : undefined;
}

export function getMethodAndUrlErrors(obj: any): ErrorMap | undefined {
  if (typeof obj !== 'object' || obj === null || Array.isArray(obj)) {
    return { method: 'No valid http method found' };
  }
  const keys = Object.keys(obj).filter((key): key is HttpMethod => METHODS.includes(key as HttpMethod));
  if (keys.length === 0) return { method: 'No valid http method found' };
  if (typeof obj[keys[0]] !== 'string') return { url: 'No valid url found' };
  return undefined;
}

export function validateYaml(data: any) {
  data = convertCollectionStepsToTestkitFormat(data);
  try {
    if (!Array.isArray(data)) {
      triggerToastEvent(getEvent('errorToast', { value: ['Array of steps expected'] }));
      return undefined;
    }
    let errors = [];
    for (let [_, step] of data.entries()) {
      errors.push(isValidStep(step));
    }
    window.updateStepsWithErrors(errors);
    if (errors.filter((err) => err !== undefined).length > 0) {
      triggerToastEvent(getEvent('errorToast', { value: ['Saving failed. Please fix the errors and try again.'] }));
      return;
    }
    data.map((step) => {
      if (step.json) {
        step.json = typeof step.json === 'string' ? step.json : JSON.stringify(step.json);
      }
    });
    return data;
  } catch (error) {
    const event = getEvent('errorToast', { value: ['Invalid yaml'] });
    triggerToastEvent(event);
    return undefined;
  }
}
window.validateYaml = validateYaml;

export function getAssertErrors(asserts: any): ErrorMap | undefined {
  if (!Array.isArray(asserts)) return { asserts: 'Asserts must be an array' };

  const errors: any[] = [];
  for (const item of asserts) {
    let err: Record<string, string> | undefined = undefined;
    if (typeof item !== 'object' || item === null || Array.isArray(item)) {
      err = { value: 'Assert must be an object' };
    }

    const keys = Object.keys(item);
    const key = keys[0];
    if (!ASSERTS.includes(key as AssertType)) {
      err = { ...(err || {}), key: 'Invalid assert key' };
    }

    if (typeof item[key] !== 'string') {
      err = { ...(err || {}), value: 'Invalid assert value' };
    }

    if (err) errors.push(err);
  }

  return errors.length > 0 ? { asserts: errors } : undefined;
}

export function getStepMethod(step: Step): HttpMethod {
  const httpMethodKeys = Object.keys(step).filter((key): key is HttpMethod => METHODS.includes(key as HttpMethod));
  return httpMethodKeys[0] || 'GET';
}

function getTextOperation(operation: string): string | undefined {
  switch (operation) {
    case '==':
      return 'equals';
    case '!=':
      return 'notEquals';
    case '>':
      return 'greaterThan';
    case '<':
      return 'lessThan';
    case '>=':
      return 'greaterThanOrEqual';
    case '<=':
      return 'lessThanOrEqual';
  }
}

export function getOperationFromText(operation: string): string {
  switch (operation) {
    case 'equals':
      return '==';
    case 'notEquals':
      return '!=';
    case 'greaterThan':
      return '>';
    case 'lessThan':
      return '<';
    case 'greaterThanOrEqual':
      return '>=';
    case 'lessThanOrEqual':
      return '<=';
    default:
      return '==';
  }
}

export function convertTestkitToCollectionSteps(testkitSteps: any): Step[] {
  const collectionSteps: Step[] = [];
  if (Array.isArray(testkitSteps)) {
    testkitSteps.forEach((step) => {
      const assertions: Assertion[] = [];
      if (step.asserts && Array.isArray(step.asserts)) {
        step.asserts.forEach((assertion: any) => {
          const tka = convertTestkitAssertions(assertion);
          if (tka) {
            assertions.push(tka);
          }
        });
      }

      const exports = Object.entries(step.exports || {}).map(([key, value]: [string, any]) => {
        let category = 'body';
        let prfix = '$.resp.json';
        if (value) {
          if (value.startsWith('$.resp.headers')) {
            category = 'header';
            prfix = '$.resp.headers.';
          }
          if (value.startsWith('$.resp.status')) {
            category = 'status';
            prfix = '$.resp.status';
          }
          if (value.startsWith('$.resp.responseTime')) {
            category = 'responseTime';
            prfix = '$.resp.responseTime';
          }
        }
        return { key: key, value: value.replace(prfix, ''), category: category } as Record<string, string>;
      });
      const method = getStepMethod(step);
      const collectionStep = {
        title: step.title || '',
        _method: method,
        _url: step[method],
        headers: step.headers || {},
        params: step.params || {},
        _assertions: assertions,
        exports: exports,
        _json: step.json,
        _requestBody: step.requestBody,
        disabled: step.disabled || false,
        followRedirects: step.followRedirects || true,
        allowRedirects: step.allowRedirects || true,
        ignoreSSLErrors: step.ignoreSSLErrors || false,
        timeout: step.timeout || 60,
        httpVersion: step.httpVersion || 'http1',
      };
      collectionSteps.push(collectionStep);
    });
  }
  return collectionSteps;
}

export function convertCollectionStepsToTestkitFormat(collectionSteps: Step[]): TestkitStep[] {
  const testkitSteps: TestkitStep[] = [];
  if (Array.isArray(collectionSteps)) {
    collectionSteps.forEach((step) => {
      const assertions: Record<string, string>[] = [];
      step._assertions?.forEach((assertion) => {
        assertions.push(convertToTestkitAssertion(assertion));
      });
      const exports = (step.exports || []).reduce((acc, ex) => {
        let prefix = '$.resp.json';
        if (ex.category === 'header') {
          prefix = '$.resp.headers.';
        }
        if (ex.category === 'status') {
          prefix = '$.resp.status';
        }
        if (ex.category === 'responseTime') {
          prefix = '$.resp.responseTime';
        }
        const val = ex.value || '';
        return { ...acc, [ex.key]: prefix + val };
      }, {});
      const testkitStep = {
        title: step.title || '',
        [step._method || 'GET']: step._url || '',
        headers: step.headers || {},
        exports: exports,
        asserts: assertions || [],
        params: step.params || {},
        disabled: step.disabled || false,
        timeout: 30,
        followRedirect: false,
        ignoreSSLErrors: false,
        requestBody: {},
      };
      if (!step._requestType) {
        step._requestType = 'application/json';
      }
      if (step._requestType === 'application/json') {
        testkitStep.json = step._json;
      } else if (step._requestType === 'application/x-www-form-urlencoded') {
        let requestBody: Record<string, any> = {};
        const keys = Object.keys(step._requestBody ? step._requestBody : {});
        if (keys.length > 0) {
          keys.forEach((key) => {
            requestBody[key] = step._requestBody[key];
          });
          testkitStep.requestBody = requestBody;
        }
      }
      if (step.followRedirects) {
        testkitStep.followRedirects = true;
      }
      if (step.ignoreSSLErrors) {
        testkitStep.ignoreSSLErrors = true;
      }
      if (step.timeout && step.timeout !== 60) {
        testkitStep.timeout = step.timeout;
      }
      if (step.httpVersion && step.httpVersion !== 'http2-http1') {
        testkitStep.httpVersion = step.httpVersion;
      }
      testkitSteps.push(testkitStep);
    });
  }
  return testkitSteps;
}

export function convertTestkitAssertions(assertion: Record<string, string>): Assertion | undefined {
  const keys = Object.keys(assertion);
  if (keys.length === 0) return undefined;

  const key = keys[0];
  const value = assertion[key];

  if (key === 'ok') {
    const [jsonpath, evalOperation, val] = value.split(' ');
    const operation = getTextOperation(evalOperation)!;
    if (jsonpath.startsWith('$.resp.json')) {
      return { type: 'body', operation: 'jsonpath', jsonpath: jsonpath.replace('.resp.json', ''), subOperation: operation, value: val };
    } else if (jsonpath.startsWith('$.resp.headers')) {
      return { type: 'header', operation, headerName: jsonpath.replace('$.resp.headers.', ''), value: val };
    }
    return { type: 'statusCode', operation, value: val };
  } else {
    const operation = key;
    const jsonpath = value;
    if (jsonpath.startsWith('$.resp.json')) {
      return { type: 'body', operation: 'jsonpath', jsonpath: jsonpath.replace('.resp.json', ''), subOperation: operation, value: '' };
    } else if (jsonpath.startsWith('$.resp.headers')) {
      return { type: 'header', operation, headerName: jsonpath.replace('$.resp.headers.', ''), value: '' };
    }
    return { type: 'statusCode', operation, value: '' };
  }
}

export function convertToTestkitAssertion(assertion: Assertion): Record<string, string> {
  let jsonpath = '';
  let operation = 'ok';

  if (assertion.type === 'header') {
    jsonpath = `$.resp.headers.${assertion.headerName}`;
  } else if (assertion.type === 'body' && assertion.jsonpath) {
    jsonpath = `$.resp.json.${assertion.jsonpath.substring(2)}`;
  } else if (assertion.type === 'statusCode') {
    jsonpath = `$.resp.status`;
  } else if (assertion.type === 'responseTime') {
    jsonpath = `$.resp.duration_ms`;
  }

  if (
    ['equals', 'notEquals', 'greaterThan', 'lessThan', 'greaterThanOrEqual', 'lessThanOrEqual'].includes(
      assertion.operation === 'jsonpath' ? assertion.subOperation! : assertion.operation
    )
  ) {
    const evalOperation = getOperationFromText(assertion.operation === 'jsonpath' ? assertion.subOperation! : assertion.operation);
    return {
      [operation]: `${jsonpath} ${evalOperation} ${assertion.value}`,
    };
  } else {
    const operation = assertion.operation;
    return {
      [operation === 'jsonpath' ? assertion.subOperation! : operation]: `${jsonpath}`,
    };
  }
}

export function replaceVariables(expression: string) {
  // regex to match {{VALUE}}
  const regex = /{{(.*?)}}/g;
  const matches = expression.match(regex);
  const variables = window.testVariables || [];
  if (matches) {
    for (const match of matches) {
      const variable = match.replace('{{', '').replace('}}', '');
      if (variables && Array.isArray(variables)) {
        const variableValue = variables.find((v) => v.variableName === variable);
        if (variableValue) {
          expression = expression.replace(match, variableValue.variableValue);
        }
      }
    }
  }
  return expression;
}
