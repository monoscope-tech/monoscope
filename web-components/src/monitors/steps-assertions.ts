import { html, TemplateResult } from 'lit';
import jsonpath from 'jsonpath';
import { faSprite_ } from './test-editor-utils';
import { AssertionBuilderProps, AssertionResult } from '../types/types';

type Assertion = {
  type: 'body' | 'header' | 'statusCode' | 'responseTime';
  operation?: string;
  subOperation?: string;
  value?: string;
  jsonpath?: string;
  headerName?: string;
  includeDNS?: boolean;
  status?: string;
};

type Response = {
  status: number;
  headers: Record<string, string>;
  json: any;
  raw: string;
  duration_ms: number;
};

const baseOperations = [
  { value: 'equals', label: 'equals' },
  { value: 'notEquals', label: 'does not equal' },
  { value: 'greaterThan', label: 'is greater than' },
  { value: 'lessThan', label: 'is less than' },
  { value: 'greaterThanOrEqual', label: 'is greater than' },
  { value: 'lessThanOrEqual', label: 'is less than' },
  { value: 'contains', label: 'contains' },
  { value: 'notContains', label: 'does not contain' },
  { value: 'null', label: 'is null' },
  { value: 'exists', label: 'exists' },
  { value: 'empty', label: 'is empty' },
  { value: 'notEmpty', label: 'is not empty' },
  { value: 'boolean', label: 'is boolean' },
  { value: 'number', label: 'is number' },
  { value: 'string', label: 'is string' },
  { value: 'array', label: 'is array' },
  { value: 'date', label: 'is date' },
];

const bodyAdditionalOperations = [{ value: 'jsonpath', label: 'jsonpath' }];

const renderInput = (
  type: string,
  value: string,
  onChange: (e: Event) => void,
  placeholder: string,
  className = ''
): TemplateResult => html`
  <input type=${type} placeholder=${placeholder} class="input input-sm grow shadow-none ${className}" .value=${value} @input=${onChange} />
`;

const renderTextarea = (value: string, onChange: (e: Event) => void, placeholder: string): TemplateResult => html`
  <textarea placeholder=${placeholder} class="textarea textarea-sm h-24 grow shadow-none" .value=${value} @input=${onChange}></textarea>
`;

const renderDropdown = (
  options: { value: string; label: string }[],
  value: string,
  onChange: (e: Event) => void,
  className = ''
): TemplateResult => html`
  <select class="select select-sm max-w-xs shadow-none ${className}" .value=${value} @change=${onChange}>
    ${options.map((option) => html`<option value=${option.value} ?selected=${option.value === value}>${option.label}</option>`)}
  </select>
`;

function renderInputs(
  assertion: Assertion,
  index: number,
  updateAssertion: (index: number, update: Partial<Assertion>) => void
): TemplateResult {
  const update = (field: keyof Assertion) => (e: Event) => updateAssertion(index, { [field]: (e.target as HTMLInputElement).value });

  const inputs: TemplateResult[] = [];

  if (assertion.type === 'responseTime') {
    inputs.push(
      renderDropdown(
        [
          { value: 'includeDNS', label: 'Including DNS' },
          { value: 'excludeDNS', label: 'Excluding DNS' },
        ],
        assertion.includeDNS ? 'includeDNS' : 'excludeDNS',
        (e: Event) =>
          updateAssertion(index, {
            includeDNS: (e.target as HTMLSelectElement).value === 'includeDNS',
          }),
        'select w-48'
      )
    );
  }

  if (assertion.operation === 'jsonpath') {
    inputs.push(
      renderInput('text', assertion.jsonpath || '', update('jsonpath'), 'JSON path'),
      renderDropdown(baseOperations, assertion.subOperation || '', update('subOperation'))
    );
    if (assertion.subOperation !== 'isUndefined') {
      inputs.push(renderInput('text', assertion.value || '', update('value'), 'Value'));
    }
  } else if (assertion.operation === 'jsonschema') {
    inputs.push(renderTextarea(assertion.value || '', update('value'), 'JSON Schema'));
  } else if (assertion.operation !== 'isUndefined') {
    const inputConfig: Record<string, { type: string; placeholder: string; className: string }> = {
      header: { type: 'text', placeholder: 'Header Value', className: 'w-5' },
      statusCode: { type: 'number', placeholder: 'Status Code', className: 'w-5' },
      responseTime: { type: 'number', placeholder: 'Time (ms)', className: 'w-32' },
      default: { type: 'text', placeholder: 'Value', className: 'w-64' },
    };

    const config = inputConfig[assertion.type] || inputConfig.default;

    inputs.push(
      assertion.type == 'body'
        ? renderTextarea(assertion.value || '', update('value'), 'Body content')
        : renderInput(config.type, assertion.value || '', update('value'), config.placeholder, config.className)
    );
  }

  return html`<div class="${assertion.type === 'body' ? 'basis-full' : ''} grow inline-flex gap-2">${inputs}</div>`;
}

function renderAssertionContent(
  assertion: Assertion,
  index: number,
  updateAssertion: (index: number, update: any) => void
): TemplateResult {
  const isBodyType = assertion.type === 'body';
  const operationOptions = isBodyType ? [...baseOperations, ...bodyAdditionalOperations] : baseOperations;

  const inputs: TemplateResult[] = [];

  if (assertion.type === 'header') {
    inputs.push(
      renderInput(
        'text',
        assertion.headerName || '',
        (e) => updateAssertion(index, { headerName: (e.target as HTMLInputElement).value }),
        'Header name'
      )
    );
  }

  inputs.push(
    renderDropdown(operationOptions, assertion.operation || '', (e) =>
      updateAssertion(index, { operation: (e.target as HTMLSelectElement).value })
    )
  );

  inputs.push(renderInputs(assertion, index, updateAssertion));

  return html`${inputs}`;
}

function evaluateAssertion(assertion: Assertion, result?: AssertionResult, response?: Response): boolean {
  try {
    if (response) {
      const getOp = (op?: string) => getOperationFromText(op || 'equals');
      if (assertion.type === 'statusCode') {
        return eval(`${response.status} ${getOp(assertion.operation)} ${assertion.value}`);
      }
      if (assertion.type === 'header') {
        return eval(`"${response.headers[assertion.headerName ?? '']}" ${getOp(assertion.operation)} "${assertion.value}"`);
      }
      if (assertion.type === 'body') {
        if (assertion.operation === 'jsonpath') {
          const op = getOp(assertion.subOperation);
          const val = jsonpath.query(response.json, assertion.jsonpath || '$');
          return val.some((v: any) => eval(`${v} ${op} ${assertion.value}`));
        }
        return eval(`${response.raw} ${getOp(assertion.operation)} ${assertion.value}`);
      }
      if (assertion.type === 'responseTime') {
        return eval(`${response.duration_ms} ${getOp(assertion.operation)} ${assertion.value}`);
      }
    }
    if (result?.ok === true) return true;
    return assertion.status === 'PASSED';
  } catch {
    return false;
  }
}

export function renderAssertionBuilder({
  assertions,
  result,
  updateAssertion,
  addAssertion,
  removeAssertion,
}: AssertionBuilderProps): TemplateResult {
  return html`
    <div>
      ${assertions.map((assertion, index) => {
        const aResult = result?.assert_results?.[index];
        const passed = evaluateAssertion(assertion, aResult, result?.resp as any);
        const error = passed ? '' : (aResult?.err?.advice ?? '');

        return html`
          <div class="pt-2">
            <div class="flex gap-2 items-center">
              <span class="block w-12 shrink-0 pt-2">${index === 0 ? 'When' : 'And'}</span>
              <div class="flex flex-wrap items-center gap-2 grow">
                ${renderDropdown(
                  [
                    { value: 'body', label: 'body' },
                    { value: 'header', label: 'header' },
                    { value: 'statusCode', label: 'status code' },
                    { value: 'responseTime', label: 'response time' },
                  ],
                  assertion.type,
                  (e) =>
                    updateAssertion(index, {
                      type: (e.target as HTMLSelectElement).value as Assertion['type'],
                      operation: '',
                      jsonpath: undefined,
                      subOperation: undefined,
                      value: undefined,
                    })
                )}
                ${renderAssertionContent(assertion, index, updateAssertion)}
              </div>
              <div class="shrink-0">
                <div class="flex gap-3 pt-2 items-center">
                  ${passed ? html`<span class="badge badge-success">Passed</span>` : html`<span class="badge badge-error">Failed</span>`}
                  <a class="cursor-pointer text-textWeak" @pointerdown=${() => removeAssertion(index)}>
                    ${faSprite_('trash', 'regular', 'rounded-full border bg-bgRaised shadow-sm p-1 w-5 h-5 stroke-iconError')}
                  </a>
                </div>
              </div>
            </div>
            <p class="text-sm text-textError text-center">${error}</p>
          </div>
        `;
      })}
      <button class="flex items-center gap-1 mt-4 text-sm" @pointerdown=${addAssertion}>
        ${faSprite_('plus', 'regular', 'w-4 h-4 text-iconNeutral')}
        <span class="underline text-textWeak font-semibold">New assertion</span>
      </button>
    </div>
  `;
}

// You need to implement or import this function:
function getOperationFromText(op: string): string {
  // Example mapping
  switch (op) {
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
    case 'contains':
      return '.includes';
    default:
      return '==';
  }
}
