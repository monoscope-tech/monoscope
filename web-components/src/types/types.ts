import { TemplateResult } from 'lit';
import type { eventWithTime, pluginEvent } from '@rrweb/types';

export {};
declare global {
  interface Window {
    virtualListData: {
      requestVecs: any[];
      cols: any;
      colIdxMap: Record<string, number>;
      serviceColors: Record<string, string>;
      nextFetchUrl: string;
      recentFetchUrl: string;
      projectId: string;
    } | null;
    testVariables: any;
    logListTable: Element | null;
    collectionResults: any[] | undefined;
    dashboardRefreshInterval: number;
    dashboardRefreshTimer: NodeJS.Timeout | null;
    updateStepsWithErrors: (errors: any[]) => void;
    updateCollectionResults: (results: any[]) => void;
    updateEditorVal: () => void;
    bindFunctionsToObjects: (rootObj: any, obj: any) => any;
    validateYaml: (data: any) => void;
    evalScriptsFromContent: (container: HTMLElement) => void;
    params: () => Record<string, string>;
    setQueryParamAndReload: (key: string, value: string) => void;
    setParams: (state: Record<string, any>, load: boolean) => void;
    flameGraphChart: any;
    waterFallGraphChart: any;
    buildCurlRequest: (event: any) => void;
    downloadJson: (event: any) => void;
    getTimeRange: () => { from: string; to: string; since: string } | undefined;
    updateMarkAreas: (chartId: string, warningVal: string, incidentVal: string) => void;
    formatNumber: (num: number) => string;
    getUTCOffset: () => string;
    createTagify: (selector: string, options?: any) => any;
  }
  function updateUrlState(key: string, value: string): void;
}

export type Result = {
  assert_results: AssertionResult[];
  resp: Response;
};

export type AssertionBuilderProps = {
  assertions: Assertion[];
  result?: {
    assert_results: AssertionResult[];
    resp: Response;
  };
  updateAssertion: (index: number, updatedFields: Assertion) => void;
  addAssertion: (e: any) => void;
  removeAssertion: (index: number) => void;
};

export type AssertionResult = {
  ok?: boolean;
  err?: {
    advice?: string;
  };
};

export type HttpMethod = 'GET' | 'POST' | 'PATCH' | 'PUT' | 'DELETE' | 'HEAD' | 'OPTIONS' | 'CONNECT' | 'TRACE';
export type AssertType = 'exists' | 'number' | 'string' | 'array' | 'date' | 'boolean' | 'ok' | 'empty' | 'notEmpty' | 'null';

export type Assertion = {
  type: 'body' | 'header' | 'statusCode' | 'responseTime';
  operation: string;
  jsonpath?: string;
  subOperation?: string;
  headerName?: string;
  value: any;
  status?: 'PASSED' | 'FAILED' | 'PENDING';
};

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

export type TraceData = Omit<Trace, 'spans'> & { spans: Map<string, APTEvent>; minStart: number; trace_start_time: Date | null };

export type TraceDataMap = Map<string, TraceData>;

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
  _summaryCache?: { content: any; wrapLines: boolean };
  _latencyCache?: { content: any; width: number; expanded: boolean };
} & Omit<APTEvent, 'children'>;

export type Trace = {
  traceId: string;
  spans: APTEvent[];
  startTime: number;
  duration: number;
};

export type ChildrenForLatency = { startNs: number; duration: number; data: any[] };

export type ConsoleEvent = eventWithTime & pluginEvent<{ level: 'error' | 'warn' | 'info'; trace: string[]; payload: string[] }>;
