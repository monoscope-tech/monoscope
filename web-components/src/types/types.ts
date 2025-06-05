import { TemplateResult } from 'lit';

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
    collectionSteps: any[];
    collectionResults: any[] | undefined;
    dashboardRefreshInterval: number;
    dashboardRefreshTimer: NodeJS.Timeout | null;
    updateStepAssertions: (assertion: string, expression: string, step: number) => void;
    updateStepsWithErrors: (errors: any[]) => void;
    updateCollectionResults: (results: any[]) => void;
    updateEditorVal: () => void;
    bindFunctionsToObjects: (rootObj: any, obj: any) => any;
    addCollectionStep: () => void;
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

export type Step = {
  [key: string]: any;
  title?: string;
  asserts?: Record<string, string>[];
  headers?: Record<string, string>;
  params?: Record<string, string>;
  json?: string | object;
  requestBody?: Record<string, string>;
  exports?: Record<string, string>[];
  disabled?: boolean;
  followRedirects?: boolean;
  allowRedirects?: boolean;
  ignoreSSLErrors?: boolean;
  timeout?: number;
  httpVersion?: string;
  _method?: HttpMethod;
  _url?: string;
  _assertions?: Assertion[];
  _requestType?: string;
};

export type TestkitStep = {
  title: string;
  headers: Record<string, string>;
  exports: Record<string, string>;
  asserts: Record<string, string>[];
  params: Record<string, string>;
  disabled: boolean;
  requestBody: Record<string, any>;
  followRedirect: boolean;
  ignoreSSLErrors: boolean;
  timeout: number;
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
}

export type TraceData = Omit<Trace, 'spans'> & { spans: Map<string, APTEvent>; minStart: number; trace_start_time: Date | null };

export type TraceDataMap = Map<string, TraceData>;

export type EventLine = {
  depth: number;
  traceStart: number;
  traceEnd: number;
  traceId: string;
  childErrors: boolean;
  parentIds: string[];
  isNew: boolean;
  show: boolean;
  expanded: boolean;
  isLastChild: boolean;
  siblingsArr: boolean[];
  children: number;
  childrenTimeSpans: ChildrenForLatency[];
} & Omit<APTEvent, 'children'>;

export type Trace = {
  traceId: string;
  spans: APTEvent[];
  startTime: number;
  duration: number;
};

export type ChildrenForLatency = { startNs: number; duration: number; data: any[] };
