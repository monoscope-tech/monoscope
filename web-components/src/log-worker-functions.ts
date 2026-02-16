// Testable worker functions
type ColIdxMap = Record<string, number>;
type APTEvent = any;
type EventLine = any;
type Trace = any;
type ServerTraceEntry = { trace_id: string; start_time: number; duration: number; trace_start_time: string | null; root: string; children: Record<string, string[]> };

// Inline generateId to avoid importing log-list-utils (which has DOM dependencies)
export function generateId() {
  return Math.random().toString(36).substring(2, 15);
}

export function groupSpans(data: any[][], colIdxMap: ColIdxMap, expandedTraces: Record<string, boolean>, flipDirection: boolean, serverTraces: ServerTraceEntry[]) {
  const keys = ['trace_id', 'latency_breakdown', 'parent_span_id', 'timestamp', 'duration', 'start_time_ns', 'errors', 'summary', 'kind', 'id'];
  const idx: ColIdxMap = {};
  keys.forEach((key) => {
    if (colIdxMap[key] !== undefined) idx[key] = colIdxMap[key];
  });

  // Map raw arrays to APTEvent objects (still needed for rendering)
  const mapped = data.map((span) => {
    span[idx.trace_id] ||= generateId();
    span[idx.latency_breakdown] ||= generateId();
    const isLog = span[idx.kind] === 'log';
    return {
      id: isLog ? span[idx.id] : span[idx.latency_breakdown],
      startNs: span[idx.start_time_ns],
      hasErrors: isLog ? false : span[idx.errors] || (span[idx.summary]?.some((el: string) => el.includes('ERROR')) ?? false),
      duration: isLog ? 0 : span[idx.duration],
      children: [] as APTEvent[],
      parent: isLog ? span[idx.latency_breakdown] : span[idx.parent_span_id],
      data: span,
      type: isLog ? 'log' : 'span',
    };
  });

  // Build ID → APTEvent lookup
  const spanById = new Map(mapped.map((s) => [s.id, s]));

  // Link children from server-provided adjacency map and build traces
  const traces = serverTraces
    .map((entry) => {
      for (const [parentId, childIds] of Object.entries(entry.children)) {
        const parent = spanById.get(parentId);
        if (parent) parent.children = childIds.map((id) => spanById.get(id)).filter(Boolean);
      }
      const rootSpan = spanById.get(entry.root);
      return {
        traceId: entry.trace_id,
        spans: rootSpan ? [rootSpan] : [],
        startTime: entry.start_time,
        duration: entry.duration,
        trace_start_time: entry.trace_start_time ? new Date(entry.trace_start_time) : null,
      };
    })
    .filter((t) => t.spans.length > 0)
    .sort((a, b) => {
      const aStart = a.startTime || 0;
      const bStart = b.startTime || 0;
      return flipDirection ? aStart - bStart : bStart - aStart;
    });

  return flattenSpanTree(traces, expandedTraces);
}

export function flattenSpanTree(traceArr: Trace[], expandedTraces: Record<string, boolean> = {}): EventLine[] {
  const result: EventLine[] = [];

  function traverse(
    span: APTEvent,
    traceId: string,
    parentIds: string[],
    traceStart: number,
    traceEnd: number,
    depth = 0,
    isLastChild = false,
    hasSiblingsArr: boolean[] = []
  ): [number, boolean] {
    let childrenCount = span.children.length;
    let childErrors = false;

    const spanInfo: EventLine = {
      depth,
      traceStart,
      traceEnd,
      traceId,
      childErrors,
      isNew: false,
      parentIds: parentIds,
      show: expandedTraces[traceId] || depth === 0,
      expanded: expandedTraces[traceId],
      isLastChild,
      siblingsArr: hasSiblingsArr,
      ...span,
      children: childrenCount,
      childrenTimeSpans: span.children.map((child) => ({
        startNs: child.startNs,
        duration: child.duration,
        data: child.data,
      })),
    };
    result.push(spanInfo);
    const hasSibling = span.children.length > 1;
    span.children.forEach((child, index) => {
      childErrors = child.hasErrors || childErrors;
      const lastChild = index === span.children.length - 1;
      const newSiblingsArr = hasSibling && !lastChild ? [...hasSiblingsArr, true] : [...hasSiblingsArr, false];
      const [count, errors] = traverse(child, traceId, [...parentIds, span.id], traceStart, traceEnd, depth + 1, lastChild, newSiblingsArr);
      childrenCount += count;
      childErrors = childErrors || errors;
    });
    spanInfo.children = childrenCount;
    spanInfo.childErrors = childErrors;
    return [childrenCount, childErrors];
  }

  traceArr.forEach((trace) => {
    trace.spans.forEach((span) => {
      traverse(span, trace.traceId, [], trace.startTime, trace.duration, 0);
    });
  });
  return result;
}
