import { TemplateResult } from 'lit';
import type { eventWithTime, pluginEvent } from '@rrweb/types';

export {};
declare global {
  interface Window {
    logListTable?: Element | null;
    dashboardRefreshInterval: number;
    dashboardRefreshTimer: ReturnType<typeof setInterval> | null;
    initTimeTransport: (transport: HTMLElement) => void;
    setTimeRefreshInterval: (transport: HTMLElement | null, interval: number) => void;
    toggleLiveRefresh: (transport: HTMLElement | null) => void;
    shiftTimeRange: (direction: -1 | 1, transport?: HTMLElement | null) => void;
    exportTableCsv: (selector: string, filename: string) => void;
    bindFunctionsToObjects: (rootObj: any, obj: any) => any;
    evalScriptsFromContent: (container: HTMLElement) => void;
    params: () => Record<string, string>;
    setQueryParamAndReload: (key: string, value: string) => void;
    setParams: (state: Record<string, any>, load?: boolean) => void;
    flameGraphChart: any;
    waterFallGraphChart: any;
    serviceMapChart: any;
    serviceMapFilter: (q: string, id?: string) => void;
    downloadJson: (event: any) => void;
    getTimeRange: () => { from: string; to: string; since: string } | undefined;
    formatNumber: (num: number | null | undefined) => string;
    getUTCOffset: () => string;
    createTagify: (selectorOrElement: string | Element, options?: any) => any;
    tagifyTemplateFunc: (this: TagifyTemplateContext, tagData: TagifyTagData) => string;
    getTagValues: (selector: string) => string[];
    updateTimePicker: (timeRange: { since?: string; from?: string; to?: string }, opts?: { targetPr?: string; label?: string; skipSetParams?: boolean }) => string;
    // Server-rendered <head> script starts the first log_explorer request before this
    // bundle evaluates; the list adopts the in-flight promise and clears the slot.
    logDataPromise?: Promise<LogDataResponse> | null;
    // Installed by the log_explorer page init; formats the injected sessions summary chart.
    formatSummaryChart?: (region: HTMLElement) => void;
    htmx: {
      ajax: (method: string, url: string, opts: Record<string, unknown>) => Promise<void> | void;
      trigger: (target: string | Element, event: string, detail?: unknown) => void;
    };
    // The echarts UMD global, loaded from a <script> tag rather than bundled — there is
    // no module to import its types from at this seam.
    echarts: any;
  }
  type TagifyTagData = { value?: string; email?: string; class?: string; name?: string };
  type TagifyTemplateContext = {
    settings: { classNames: { tag: string; tagX: string; tagText: string } };
    getAttributes: (tagData: TagifyTagData) => string;
  };

  var monaco: typeof import('monaco-editor/esm/vs/editor/editor.api.js');

  function updateUrlState(key: string | string[], value: string, action?: 'set' | 'delete'): void;
}

export type ColIdxMap = Record<string, number>;

export interface APTEvent {
  id: string;
  startNs: number;
  hasErrors: boolean;
  duration: number;
  children: APTEvent[];
  parent: string | null;
  data: any[];
  type: 'span' | 'log';
  timestamp?: string;
}

export type EventLine = {
  depth: number;
  traceStart: number;
  traceEnd: number;
  traceId: string;
  childErrors: boolean;
  isNew: boolean;
  parentIds: string[];
  show: boolean;
  expanded: boolean;
  isLastChild: boolean;
  siblingsArr: boolean[];
  children: number;
  childrenTimeSpans: ChildrenForLatency[];
  _summaryCache?: { content: TemplateResult[]; wrapLines: boolean };
  _latencyCache?: { content: TemplateResult; width: number; expanded: boolean; dim: string };
} & Omit<APTEvent, 'children'>;

/**
 * The JSON body the log_explorer data/patterns/sessions endpoints return. Every field is
 * optional because the three endpoints answer with different subsets of it, and the readers
 * below already defend with `??`.
 */
export type LogDataResponse = {
  error?: string;
  logsData?: any[][];
  summaryHtml?: string;
  serviceColors?: Record<string, string>;
  nextUrl?: string;
  recentUrl?: string;
  cols?: string[];
  colIdxMap?: ColIdxMap;
  count?: number;
  totalPatterns?: number;
  totalSessions?: number;
  traces?: ServerTraceEntry[];
  hasMore?: boolean;
  queryResultCount?: number;
};

/** What the log list's fetch seam returns alongside the grouped rows. */
export type FetchMeta = Omit<LogDataResponse, 'error' | 'logsData' | 'summaryHtml' | 'cols' | 'colIdxMap' | 'nextUrl'> & {
  nextUrl: string;
  cols: string[];
  colIdxMap: ColIdxMap;
};

export interface ServerTraceEntry {
  trace_id: string;
  start_time: number;
  duration: number;
  trace_start_time: string | null;
  root: string;
  children: Record<string, string[]>;
}

/** A descendant of a row, for its latency bar. `depth` is relative to that row: 1 = direct child. */
export type ChildrenForLatency = { startNs: number; duration: number; data: any[]; depth: number };

export type ConsoleEvent = eventWithTime & pluginEvent<{ level: 'error' | 'warn' | 'info'; trace: string[]; payload: string[] }>;
