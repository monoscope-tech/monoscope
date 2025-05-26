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

    logListTable: Element | null;
  }
}

export type ColIdxMap = Record<string, number>;
export declare function updateUrlState(key: string, value: string): void;

export declare const htmx: {
  ajax: (verb: string, path: string, context: any) => void;
};

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
