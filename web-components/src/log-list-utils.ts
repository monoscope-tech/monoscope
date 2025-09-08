import { format, isValid } from 'date-fns';
import clsx from 'clsx';
import { html, TemplateResult } from 'lit';
import { get } from 'lodash';
import { ColIdxMap, EventLine } from './types/types';
import { AnsiUp } from 'ansi-up';
// Configuration objects
export const COLUMN_WIDTHS = {
  status: 'w-[12ch] shrink-0',
  method: 'w-[12ch] shrink-0',
  status_code: 'w-[12ch] shrink-0',
  raw_url: 'w-[25ch] shrink-0 overflow-hidden',
  url_path: 'w-[25ch] shrink-0 overflow-hidden',
  summary: 'w-3/4 shrink-1',
  timestamp: 'w-[17ch] shrink-0',
  created_at: 'w-[17ch] shrink-0',
  latency_breakdown: 'sticky right-0 shrink-0',
  service: 'w-[16ch] shrink-0',
} as const;

export const STYLE_MAPPINGS = {
  'info-strong': 'badge-info',
  'info-weak': 'badge-neutral',
  'error-strong': 'badge-error',
  'error-weak': 'badge-4xx',
  'warning-strong': 'badge-warning',
  'warning-weak': 'badge-3xx',
  'success-strong': 'badge-success',
  'success-weak': 'badge-2xx',
  neutral: 'badge-neutral',
  right: 'ml-auto badge-neutral',
  'text-weak': '',
  'text-textWeak': '',
  'text-textStrong': '',
} as const;

export const SKELETON_COLUMN_WIDTHS = ['w-[17ch]', 'w-[16ch]', 'w-[25ch]', 'w-[12ch]', 'w-[450px]'] as const;

// Column width calculation constants
export const COLUMN_DEFAULTS = {
  summary: 450 * 8.5,
  latency_breakdown: 100,
} as const;

export const CHAR_WIDTHS = {
  timestamp: 6.5,
  default: 8.5,
} as const;

export const MIN_COLUMN_WIDTH = 12;

// Calculate dynamic column width based on content
export const calculateColumnWidth = (content: string, column: string): number => {
  const charWidth = CHAR_WIDTHS[column as keyof typeof CHAR_WIDTHS] || CHAR_WIDTHS.default;
  return content.length * charWidth;
};

// Parse summary element format: "field;style⇒value"
export const parseSummaryElement = (
  element: string
): { type: 'plain'; content: string } | { type: 'formatted'; field: string; style: string; value: string } => {
  const separatorIndex = element.indexOf('⇒');
  if (separatorIndex === -1) {
    return { type: 'plain', content: element };
  }

  const semicolonIndex = element.indexOf(';');
  if (semicolonIndex === -1 || semicolonIndex > separatorIndex) {
    return { type: 'plain', content: element };
  }

  return {
    type: 'formatted',
    field: element.substring(0, semicolonIndex),
    style: element.substring(semicolonIndex + 1, separatorIndex),
    value: element.substring(separatorIndex + 1),
  };
};

function colorizeJsonValues(jsonStr: string): string {
  return jsonStr.replace(/:\s*("[^"]*"|\d+|true|false|null)/g, (match, value) => {
    return `: <span class="text-textStrong font-medium">${value}</span>`;
  });
}

const ansi_up = new AnsiUp();
ansi_up.escapeForHtml = false;

// Unescape JSON strings
export const unescapeJsonString = (str: string): string => {
  return ansi_up.ansi_to_html(colorizeJsonValues(str.replace(/\\"/g, '"').replace(/\\\\/g, '\\')));
};

// Pure utility functions
export const formatTimestamp = (input: string): string => {
  const date = new Date(input);
  return isValid(date) ? format(date, 'MMM dd HH:mm:ss') + `.${date.getUTCMilliseconds().toString().padStart(3, '0')}` : '';
};

export const lookupVecValue = <T = any>(vec: any[], colIdxMap: ColIdxMap, key: string): T => get(vec, colIdxMap[key] ?? -1, '') as T;

export const getErrorClassification = (reqVec: any[], colIdxMap: ColIdxMap) => {
  const hasErrors = lookupVecValue(reqVec, colIdxMap, 'errors');
  const statusCode = lookupVecValue<any>(reqVec, colIdxMap, 'http_attributes')?.status_code || 0;
  const errorStatus = lookupVecValue(reqVec, colIdxMap, 'status');
  // DO NOT DELETE: class="bg-strokeError-strong bg-strokeError-strong bg-strokeBrand-weak bg-strokeBrand-strong"
  return {
    statusCode,
    hasErrors,
    className: clsx('w-1', {
      'bg-strokeError-strong': hasErrors || errorStatus === 'ERROR',
      'bg-strokeWarning-strong': !hasErrors && statusCode >= 400,
      'bg-strokeBrand-weak status-indicator': !hasErrors && statusCode < 400,
    }),
  };
};

// Template helper functions
export const faSprite = (iconName: string, kind: string, classes: string): TemplateResult =>
  html`<svg class="${classes}"><use href="/public/assets/svgs/fa-sprites/${kind}.svg#${iconName}"></use></svg>`;

export const renderBadge = (classes: string, title: string, tooltip?: string): TemplateResult =>
  html`<span
    class=${clsx(classes, 'relative transition-all duration-200 hover:shadow-sm', {
      'tooltip tooltip-right': tooltip,
    })}
    ${tooltip ? html`data-tip=${tooltip}` : ''}
    >${title}</span
  >`;

export const renderIconWithTooltip = (classes: string, tooltip: string, icon: TemplateResult): TemplateResult =>
  html`<span class=${clsx('shrink-0 inline-flex tooltip tooltip-right', classes)} data-tip=${tooltip}>${icon}</span>`;

export const generateId = (): string => Math.random().toString(36).substring(2, 15);

export const getColumnWidth = (column: string): string =>
  COLUMN_WIDTHS[column as keyof typeof COLUMN_WIDTHS] || (column === 'id' ? '' : 'w-[16ch] shrink-0');

export const getSkeletonColumnWidth = (idx: number): string => SKELETON_COLUMN_WIDTHS[idx % SKELETON_COLUMN_WIDTHS.length] + ' shrink-0';

export const getStyleClass = (style: string): string => {
  if (style.startsWith('badge-')) return style;
  if (style.startsWith('right-')) return style.substring(6);
  return STYLE_MAPPINGS[style as keyof typeof STYLE_MAPPINGS] || 'badge-neutral';
};

export function binarySearchByStartNs(array: EventLine[], target: number) {
  let low = 0;
  let high = array.length - 1;

  while (low <= high) {
    const mid = (low + high) >> 1; // bitwise floor divide by 2
    console.log(mid);
    const midVal = array[mid].startNs;
    console.log(array[mid].startNs, target);

    if (midVal === target) {
      return mid;
    }
    if (midVal < target) {
      low = mid + 1;
    } else {
      high = mid - 1;
    }
  }

  return 0;
}
