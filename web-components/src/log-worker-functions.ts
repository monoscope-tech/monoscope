// Testable worker functions
type ColIdxMap = Record<string, number>;
type APTEvent = any;
type EventLine = any;
type Trace = any;

// Inline generateId to avoid importing log-list-utils (which has DOM dependencies)
export function generateId() {
  return Math.random().toString(36).substring(2, 15);
}

export function groupSpans(data: any[][], colIdxMap: ColIdxMap, expandedTraces: Record<string, boolean>, flipDirection: boolean, queryResultCount: number = data.length) {
  // Inline pick
  const keys = [
    'trace_id',
    'latency_breakdown',
    'parent_span_id',
    'timestamp',
    'duration',
    'start_time_ns',
    'errors',
    'summary',
    'kind',
    'id',
  ];
  const idx: ColIdxMap = {};
  keys.forEach((key) => {
    if (colIdxMap[key] !== undefined) idx[key] = colIdxMap[key];
  });

  // Map spans
  const mapped = data.map((span, index) => {
    span[idx.trace_id] ||= generateId();
    span[idx.latency_breakdown] ||= generateId();
    const isLog = span[idx.kind] === 'log';
    return {
      traceId: span[idx.trace_id],
      span: {
        id: isLog ? span[idx.id] : span[idx.latency_breakdown],
        startNs: span[idx.start_time_ns],
        hasErrors: isLog ? false : span[idx.errors] || (span[idx.summary]?.some((el: string) => el.includes('ERROR')) ?? false),
        duration: isLog ? 0 : span[idx.duration],
        children: [],
        parent: isLog ? span[idx.latency_breakdown] : span[idx.parent_span_id],
        data: span,
        type: isLog ? 'log' : 'span',
        isQueryResult: index < queryResultCount,
      },
      timestamp: new Date(span[idx.timestamp]),
      startTime: span[idx.start_time_ns],
      duration: span[idx.duration],
    };
  });

  // Group by traceId
  const grouped: Record<string, any[]> = {};
  mapped.forEach((item) => {
    if (!grouped[item.traceId]) grouped[item.traceId] = [];
    grouped[item.traceId].push(item);
  });

  // Map values
  const traces = Object.values(grouped)
    .map((traceSpans) => {
      // Build spans and metadata
      const spanMap = new Map(traceSpans.map((s) => [s.span.id, s.span]));
      const metadata = traceSpans.reduce(
        (acc, s) => ({
          minStart: Math.min(acc.minStart, s.startTime),
          duration: Math.max(acc.duration, s.duration),
          trace_start_time: !acc.trace_start_time || s.timestamp < acc.trace_start_time ? s.timestamp : acc.trace_start_time,
        }),
        { minStart: Infinity, duration: 0, trace_start_time: null }
      );

      // Build tree - separate true roots, query results, and orphaned children
      const roots: APTEvent[] = [];
      const orphansByParent = new Map<string, APTEvent[]>();
      spanMap.forEach((span) => {
        if (!span.parent) {
          roots.push(span);
        } else {
          const parent = spanMap.get(span.parent);
          if (parent) {
            parent.children.push(span);
          } else if (span.isQueryResult) {
            roots.push(span);
          } else {
            const existing = orphansByParent.get(span.parent);
            if (existing) existing.push(span);
            else orphansByParent.set(span.parent, [span]);
          }
        }
      });
      // Create virtual parent nodes for orphan groups
      orphansByParent.forEach((orphans, parentId) => {
        const earliest = orphans.reduce((a, b) => ((a.startNs || 0) < (b.startNs || 0) ? a : b));
        roots.push({
          id: `virtual-${parentId}`,
          startNs: Math.min(...orphans.map((o) => o.startNs || Infinity)) || 0,
          hasErrors: orphans.some((o) => o.hasErrors),
          duration: Math.max(...orphans.map((o) => o.duration || 0)),
          children: orphans,
          parent: null,
          data: earliest.data, // Use earliest child's data for timestamp/service columns
          type: 'virtual-parent',
          missingParentId: parentId,
        });
      });

      // Sort all children by startNs (execution order) instead of timestamp
      spanMap.forEach((span) => {
        if (span.children.length > 1) {
          span.children.sort((a, b) => a.startNs - b.startNs);
        }
      });

      return {
        traceId: traceSpans[0].traceId,
        spans: roots,
        startTime: metadata.minStart,
        duration: metadata.duration,
        trace_start_time: metadata.trace_start_time,
      };
    })
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
