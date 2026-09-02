// Testable worker functions.
// Type-only import: erased at compile time, so the worker bundle stays free of the
// DOM-dependent modules types.ts references.
import type { APTEvent, ChildrenForLatency, ColIdxMap, EventLine, ServerTraceEntry } from './types/types';

type Trace = { traceId: string; spans: APTEvent[]; startTime: number; duration: number; trace_start_time: Date | null };

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

// Walks the same server-supplied adjacency as flattenSpanTree, so it needs the same guard: a
// cyclic parent chain otherwise overflows the stack here, before the tree is ever built.
function bubbleIdentity(span: APTEvent, summaryIdx: number | undefined, seen: Set<APTEvent>) {
  if (seen.has(span)) return;
  seen.add(span);
  for (const child of span.children) {
    bubbleIdentity(child, summaryIdx, seen);
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
    const isLog = span[idx.kind] === 'log';
    return {
      // Alerts and other system events have a normal row id but no span id.
      // Only spans are identified by latency_breakdown; otherwise the server's
      // trace root cannot resolve this row and the virtual list is empty.
      id: isLog ? span[idx.id] : span[idx.latency_breakdown] || span[idx.id] || generateId(),
      startNs: span[idx.start_time_ns],
      hasErrors: rowHasError(span, idx, isLog),
      duration: isLog ? 0 : span[idx.duration],
      children: [] as APTEvent[],
      parent: span[idx.parent_id],
      data: span,
      type: isLog ? ('log' as const) : ('span' as const),
    };
  });

  // Build ID → APTEvent lookup
  const spanById = new Map(mapped.map((s) => [s.id, s]));

  // Link children from server-provided adjacency map and build traces
  const traces = serverTraces
    .map((entry) => {
      for (const [parentId, childIds] of Object.entries(entry.children)) {
        const parent = spanById.get(parentId);
        if (parent) parent.children = childIds.flatMap((id) => {
          const child = spanById.get(id);
          return child ? [child] : [];
        });
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

  const bubbled = new Set<APTEvent>();
  for (const trace of traces) {
    for (const root of trace.spans) bubbleIdentity(root, idx.summary, bubbled);
  }

  return flattenSpanTree(traces, expandedTraces);
}

/**
 * Bounds on what a single trace may cost the tab.
 *
 * `parent_id` comes from instrumentation, so the adjacency the server derives from it is only
 * as well-formed as the SDK that emitted it: cycles, self-parents and chains thousands deep are
 * all reachable, and each one used to be fatal rather than ugly — an unguarded recursion that
 * overflowed the stack (or exhausted memory first) while every ancestor accumulated a full copy
 * of its descendant subtree.
 *
 * MAX_SUBTREE_SEGMENTS caps that per-node copy. A collapsed row's latency bar is ~120px wide and
 * `exclusiveSegments` merges overlapping spans before painting, so the segments that survive to
 * the screen are far fewer than this; keeping the longest-running descendants preserves what is
 * actually visible while turning O(rows x depth) retained objects into O(rows). Traces at real
 * depths (~3) never reach the cap and are unaffected.
 */
const MAX_SUBTREE_SEGMENTS = 64;
/**
 * Depth past which a trace is treated as malformed rather than merely deep.
 *
 * Two orders of magnitude beyond anything real (production traces run ~3 deep, and a genuine
 * call chain of 512 would itself be a bug worth seeing). It bounds both the recursion — which
 * overflows the JS stack somewhere past ~10k frames — and `parentIds`, which is the full
 * ancestor chain per row and so costs O(rows x depth) on its own. Truncation is reported, not
 * silent: the rows are still counted, they just stop nesting.
 */
const MAX_TREE_DEPTH = 512;

export function flattenSpanTree(traceArr: Trace[], expandedTraces: Record<string, boolean> = {}): EventLine[] {
  const result: EventLine[] = [];
  // One span belongs to one place in one trace, so visiting it twice means the adjacency is
  // cyclic or diamond-shaped. Emitting it once is both the termination condition and the
  // correct output: duplicate rows carry duplicate ids, which the list dedupes downstream
  // anyway — only after paying to build the subtree twice.
  const visited = new Set<APTEvent>();
  let truncatedAtDepth = 0;

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
    if (visited.has(span)) return [0, false, []];
    if (depth > MAX_TREE_DEPTH) {
      truncatedAtDepth++;
      return [0, false, []];
    }
    visited.add(span);
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
      // Trim on a hysteresis so the sort is amortised, and by duration so the descendants that
      // own visible width survive. Below the cap this never runs and order is untouched.
      if (subtree.length > MAX_SUBTREE_SEGMENTS * 2) {
        subtree.sort((a, b) => b.duration - a.duration);
        subtree.length = MAX_SUBTREE_SEGMENTS;
      }
    });
    if (subtree.length > MAX_SUBTREE_SEGMENTS) {
      subtree.sort((a, b) => b.duration - a.duration);
      subtree.length = MAX_SUBTREE_SEGMENTS;
    }
    spanInfo.children = childrenCount;
    spanInfo.childErrors = childErrors;
    return [childrenCount, childErrors, subtree];
  }

  traceArr.forEach((trace) => {
    trace.spans.forEach((span) => {
      traverse(span, trace.traceId, [], trace.startTime, trace.duration, 0);
    });
  });
  if (truncatedAtDepth) console.warn(`[trace] ${truncatedAtDepth} span(s) below depth ${MAX_TREE_DEPTH} not nested — parent chain is cyclic or malformed`);
  return result;
}
