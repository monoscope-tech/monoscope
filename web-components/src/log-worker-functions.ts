// Testable worker functions
type ColIdxMap = Record<string, number>;
type APTEvent = any;
type EventLine = any;
type Trace = any;
type ChildrenForLatency = { startNs: number; duration: number; data: any[]; depth: number };
type ServerTraceEntry = { trace_id: string; start_time: number; duration: number; trace_start_time: string | null; root: string; children: Record<string, string[]> };

// Inline generateId to avoid importing log-list-utils (which has DOM dependencies)
export function generateId() {
  return Math.random().toString(36).substring(2, 15);
}

// Whether a row counts as an error for the purpose of propagating the red error
// badge to ancestor rows. The server-computed `errors` flag already folds in log
// severity (see defaultSelectSqlQuery), so logs are covered by it; for spans we
// additionally honour an ERROR marker in the styled `summary` segments (status-only
// error spans carry no `errors` payload). We do NOT scan a log's summary, since that
// is the raw body and would false-positive on the word "ERROR".
function rowHasError(span: any[], idx: ColIdxMap, isLog: boolean): boolean {
  if (span[idx.errors]) return true;
  return isLog ? false : (span[idx.summary]?.some((el: string) => el.includes('ERROR')) ?? false);
}

const IDENTITY_SUMMARY_FIELDS = new Set(['session', 'user email', 'user name', 'user id']);

function identityField(element: unknown): string | null {
  if (typeof element !== 'string') return null;
  const separator = element.indexOf(';');
  if (separator < 0 || !element.includes('⇒')) return null;
  const field = element.slice(0, separator);
  return IDENTITY_SUMMARY_FIELDS.has(field) ? field : null;
}

// Older rows may have received promoted session/user columns from the trace
// backfill after their persisted summary was built. Bubble the first available
// identity value from descendants into each ancestor's in-memory summary so a
// collapsed trace identifies the user without changing historical storage.
function inheritIdentitySummary(parent: any[], child: any[], summaryIdx: number | undefined) {
  if (summaryIdx === undefined) return;
  const parentSummary = Array.isArray(parent[summaryIdx]) ? parent[summaryIdx] : [];
  const childSummary = Array.isArray(child[summaryIdx]) ? child[summaryIdx] : [];
  const present = new Set(parentSummary.map(identityField).filter(Boolean));
  const inherited = childSummary.filter((element: unknown) => {
    const field = identityField(element);
    if (!field || present.has(field)) return false;
    present.add(field);
    return true;
  });
  if (inherited.length) parent[summaryIdx] = [...parentSummary, ...inherited];
}

function bubbleIdentity(span: APTEvent, summaryIdx: number | undefined) {
  for (const child of span.children) {
    bubbleIdentity(child, summaryIdx);
    inheritIdentitySummary(span.data, child.data, summaryIdx);
  }
}

export function groupSpans(data: any[][], colIdxMap: ColIdxMap, expandedTraces: Record<string, boolean>, flipDirection: boolean, serverTraces: ServerTraceEntry[]) {
  const keys = ['trace_id', 'latency_breakdown', 'parent_id', 'timestamp', 'duration', 'start_time_ns', 'errors', 'summary', 'kind', 'id'];
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

  for (const trace of traces) {
    for (const root of trace.spans) bubbleIdentity(root, idx.summary);
  }

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
  ): [number, boolean, ChildrenForLatency[]] {
    let childrenCount = span.children.length;
    let childErrors = false;
    // The whole subtree, not just the direct children: a collapsed row's bar is a summary of
    // where its request spent time, and the service that spent it is usually a grandchild —
    // an API span whose only child is another span of the same service reads as one colour
    // when only one level is available. `depth` is relative to this row, so the waterfall (which
    // draws every descendant as its own row) can still ask for just the first level.
    const subtree: ChildrenForLatency[] = [];

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
      childrenTimeSpans: subtree,
    };
    result.push(spanInfo);
    const hasSibling = span.children.length > 1;
    span.children.forEach((child, index) => {
      childErrors = child.hasErrors || childErrors;
      const lastChild = index === span.children.length - 1;
      const newSiblingsArr = hasSibling && !lastChild ? [...hasSiblingsArr, true] : [...hasSiblingsArr, false];
      const [count, errors, childSubtree] = traverse(
        child,
        traceId,
        [...parentIds, span.id],
        traceStart,
        traceEnd,
        depth + 1,
        lastChild,
        newSiblingsArr
      );
      childrenCount += count;
      childErrors = childErrors || errors;
      subtree.push({ startNs: child.startNs, duration: child.duration, data: child.data, depth: 1 });
      for (const d of childSubtree) subtree.push({ ...d, depth: d.depth + 1 });
    });
    spanInfo.children = childrenCount;
    spanInfo.childErrors = childErrors;
    return [childrenCount, childErrors, subtree];
  }

  traceArr.forEach((trace) => {
    trace.spans.forEach((span) => {
      traverse(span, trace.traceId, [], trace.startTime, trace.duration, 0);
    });
  });
  return result;
}
