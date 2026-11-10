'use strict';
import { groupSpans, incompleteResponse } from './log-worker-functions';

self.onmessage = async (e: MessageEvent) => {
  const { type, url, colIdxMap, expandedTraces, flipDirection, id } = e.data;

  if (type !== 'fetch') return;

  // Every failure up to a parsed body is the server's or the network's, shown to the reader
  // and flagged `expected` so the list does not console.error it.
  const fail = (error: string, queryError = false) => self.postMessage({ type: 'error', error, queryError, expected: true, id });
  let data;
  try {
    const response = await fetch(url, {
      method: 'GET',
      headers: { Accept: 'application/json' },
      credentials: 'include',
    });
    // A gateway timeout answers with an HTML page; parsing it would surface a JSON error.
    if (!response.ok && !response.headers.get('content-type')?.includes('json')) {
      return fail([504, 524].includes(response.status) ? 'The query timed out. Try a shorter time range or a narrower filter.' : `Request failed (HTTP ${response.status})`);
    }
    data = await response.json().catch(() => null);
    if (!data) return fail(incompleteResponse(response.status));
  } catch (error) {
    return fail(error instanceof Error ? error.message : 'Network error');
  }
  try {
    // `error` is the only failure field LogResult carries — reading `message`
    // first silently swallowed every server message into the generic fallback.
    if (data.error) return fail(data.error, true);

    const { logsData, serviceColors, nextUrl, recentUrl, cols, count, traces, hasMore, queryResultCount, viz } = data;

    if (!Array.isArray(logsData) || logsData.length === 0) {
      self.postMessage({
        type: 'success',
        tree: [],
        meta: { serviceColors, nextUrl, recentUrl, cols, colIdxMap: data.colIdxMap, count, traces: traces || [], hasMore: hasMore ?? false, queryResultCount: 0, viz },
        id,
      });
      return;
    }

    // Use the colIdxMap from the server response, not the one from the message
    const tree = groupSpans(logsData, data.colIdxMap, expandedTraces, flipDirection, traces || []);

    if (tree.length === 0) {
      console.error('[Worker] Tree is empty after processing', logsData.length, 'items');
    }

    self.postMessage({
      type: 'success',
      tree,
      meta: { serviceColors, nextUrl, recentUrl, cols, colIdxMap: data.colIdxMap, count, traces: traces || [], hasMore: hasMore ?? true, queryResultCount: queryResultCount ?? logsData.length, viz },
      id,
    });
  } catch (error) {
    console.error('[Worker] Error:', error);
    self.postMessage({
      type: 'error',
      error: error instanceof Error ? error.message : 'Network error',
      id,
    });
  }
};
