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
  timestamp: 'w-[20ch] shrink-0', // Increased for "MMM dd HH:mm:ss.SSS" format
  created_at: 'w-[20ch] shrink-0', // Increased for "MMM dd HH:mm:ss.SSS" format
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
  timestamp: 7.2, // Adjusted for better timestamp display
  created_at: 7.2, // Adjusted for better timestamp display
  default: 8.5,
} as const;

export const MIN_COLUMN_WIDTH = 12;

// Calculate dynamic column width based on content
export const calculateColumnWidth = (content: string, column: string): number => {
  const charWidth = CHAR_WIDTHS[column as keyof typeof CHAR_WIDTHS] || CHAR_WIDTHS.default;
  return content.length * charWidth;
};

// Constants for parsing
const SEPARATOR = '⇒';
const SEMICOLON = ';';

// Parse summary element format: "field;style⇒value"
export const parseSummaryElement = (
  element: string
): { type: 'plain'; content: string } | { type: 'formatted'; field: string; style: string; value: string } => {
  // Early exit for common case
  const separatorIndex = element.indexOf(SEPARATOR);
  if (separatorIndex === -1) {
    return { type: 'plain', content: element };
  }

  const semicolonIndex = element.indexOf(SEMICOLON);
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

// Pre-compiled regex for better performance
const JSON_VALUE_REGEX = /:\s*("[^"]*"|\d+|true|false|null)/g;

function colorizeJsonValues(jsonStr: string): string {
  return jsonStr.replace(JSON_VALUE_REGEX, ': <span class="text-textStrong font-medium">$1</span>');
}

const ansi_up = new AnsiUp();
ansi_up.escapeForHtml = false;

// Pre-compiled regex patterns
const ESCAPED_QUOTE_REGEX = /\\"/g;
const ESCAPED_BACKSLASH_REGEX = /\\\\/g;

// Unescape JSON strings - optimized with early exits
export const unescapeJsonString = (str: string): string => {
  // Early exit if string doesn't need processing
  if (!str.includes('\\') && !str.includes('\x1b') && !str.includes(':')) {
    return str;
  }
  
  // Only unescape if needed
  let result = str;
  if (str.includes('\\"') || str.includes('\\\\')) {
    result = result.replace(ESCAPED_QUOTE_REGEX, '"').replace(ESCAPED_BACKSLASH_REGEX, '\\');
  }
  
  // Only process ANSI if likely present
  if (result.includes('\x1b')) {
    result = ansi_up.ansi_to_text(result);
  }
  
  // Only colorize if there are colons (JSON-like content)
  if (result.includes(':')) {
    result = colorizeJsonValues(result);
  }
  
  return result;
};

// Pure utility functions
export const formatTimestamp = (input: string): string => {
  const date = new Date(input);
  return isValid(date) ? format(date, 'MMM dd HH:mm:ss') + `.${date.getUTCMilliseconds().toString().padStart(3, '0')}` : '';
};

export const lookupVecValue = <T = any>(vec: any[], colIdxMap: ColIdxMap, key: string): T => {
  const idx = colIdxMap[key];
  // Direct array access is faster than lodash.get for simple array lookups
  return (idx !== undefined && idx >= 0 && idx < vec.length ? vec[idx] : '') as T;
};

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

// Icon configurations for summary elements
export const SUMMARY_ICON_CONFIGS = {
  request_type: (value: string) => ({
    className: 'w-4',
    tooltip: `${value} Request`,
    iconName: value === 'incoming' ? 'arrow-down-left' : 'arrow-up-right',
    iconType: 'solid' as const,
    iconClass: value === 'incoming' ? 'h-3 fill-iconNeutral' : 'h-3 fill-iconBrand'
  }),
  kind: (value: string) => value === 'internal' ? ({
    className: 'w-4 ml-2',
    tooltip: 'Internal span',
    iconName: 'function',
    iconType: 'regular' as const,
    iconClass: 'h-3 w-3'
  }) : null,
  'db.system': (value: string) => ({
    className: 'w-4 ml-2',
    tooltip: value,
    iconName: 'database',
    iconType: 'regular' as const,
    iconClass: 'h-3 w-3 fill-iconNeutral'
  })
} as const;

// Optimized icon renderer with caching
export const createCachedIconRenderer = () => {
  const cache = new Map<string, TemplateResult>();
  
  return (field: string, value: string): TemplateResult | null => {
    const cacheKey = `${field}:${value}`;
    
    // Check cache first
    if (cache.has(cacheKey)) {
      return cache.get(cacheKey)!;
    }
    
    // Get configuration
    const configFn = SUMMARY_ICON_CONFIGS[field as keyof typeof SUMMARY_ICON_CONFIGS];
    if (!configFn) {
      cache.set(cacheKey, null as any);
      return null;
    }
    
    const config = configFn(value);
    if (!config) {
      cache.set(cacheKey, null as any);
      return null;
    }
    
    // Render icon
    const result = renderIconWithTooltip(
      config.className,
      config.tooltip,
      faSprite(config.iconName, config.iconType, config.iconClass)
    );
    
    // Bounded cache - prevent memory leaks
    if (cache.size >= 512) {
      const firstKey = cache.keys().next().value;
      cache.delete(firstKey);
    }
    
    cache.set(cacheKey, result);
    return result;
  };
};

export const generateId = (): string => Math.random().toString(36).substring(2, 15);

export const getColumnWidth = (column: string): string =>
  COLUMN_WIDTHS[column as keyof typeof COLUMN_WIDTHS] || (column === 'id' ? '' : 'w-[16ch] shrink-0');

export const getSkeletonColumnWidth = (idx: number): string => SKELETON_COLUMN_WIDTHS[idx % SKELETON_COLUMN_WIDTHS.length] + ' shrink-0';

export const getStyleClass = (style: string): string => {
  if (style.startsWith('badge-')) return style;
  if (style.startsWith('right-')) return style.substring(6);
  return STYLE_MAPPINGS[style as keyof typeof STYLE_MAPPINGS] || 'badge-neutral';
};

export const calculateAutoBinWidth = (durationMs: number) => {
  const seconds = durationMs / 1000;
  const minutes = seconds / 60;
  const hours = minutes / 60;
  const days = hours / 24;

  if (hours <= 1) {
    return 30 * 1000;
  } else if (hours <= 6) {
    return 60 * 1000;
  } else if (hours <= 14) {
    return 5 * 60_000;
  } else if (hours <= 48) {
    return 10 * 60_000;
  } else if (days < 7) {
    return 60 * 60 * 1000;
  } else if (days < 30) {
    return 6 * 60 * 60 * 1000;
  } else {
    return 24 * 60 * 60 * 1000;
  }
};

// Set of weak text styles for O(1) lookup
export const WEAK_TEXT_STYLES = new Set(['text-weak', 'text-textWeak']);

// Pre-compiled regex for performance
export const RIGHT_PREFIX_REGEX = /^right-/;
