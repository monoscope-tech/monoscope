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

// Whether a row counts as an error for the purpose of propagating the red error
// badge to ancestor rows. Spans use the explicit `errors` flag or an ERROR marker
// in their styled `summary` segments. Logs have no `errors`/summary-marker, so we
// derive error-ness from severity (text or OTel number ≥17 = ERROR/FATAL); we do
// NOT scan a log's summary for "ERROR" since that's the raw body and false-positives.
function rowHasError(span: any[], idx: ColIdxMap, isLog: boolean): boolean {
  if (span[idx.errors]) return true;
  const sevText = span[idx.severity_text];
  if (typeof sevText === 'string' && /^(error|fatal|critical|alert|emerg)/i.test(sevText.trim())) return true;
  const sevNum = span[idx.severity_number];
  if (typeof sevNum === 'number' && sevNum >= 17) return true;
  return isLog ? false : (span[idx.summary]?.some((el: string) => el.includes('ERROR')) ?? false);
}

export function groupSpans(data: any[][], colIdxMap: ColIdxMap, expandedTraces: Record<string, boolean>, flipDirection: boolean, serverTraces: ServerTraceEntry[]) {
  const keys = ['trace_id', 'latency_breakdown', 'parent_id', 'timestamp', 'duration', 'start_time_ns', 'errors', 'summary', 'severity_text', 'severity_number', 'kind', 'id'];
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
      hasErrors: rowHasError(span, idx, isLog),
      duration: isLog ? 0 : span[idx.duration],
      children: [] as APTEvent[],
      parent: span[idx.parent_id],
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
