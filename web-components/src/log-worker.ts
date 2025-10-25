'use strict';
import { groupSpans } from './log-worker-functions';

self.onmessage = async (e: MessageEvent) => {
  const { type, url, colIdxMap, expandedTraces, flipDirection, id } = e.data;

  if (type !== 'fetch') return;

  try {
    const response = await fetch(url, {
      method: 'GET',
      headers: { Accept: 'application/json' },
      credentials: 'include',
    });

    const data = await response.json();

    if (data.error) {
      self.postMessage({ type: 'error', error: data.message || 'Failed to fetch logs', id });
      return;
    }

    const { logsData, serviceColors, nextUrl, recentUrl, cols, count } = data;

    if (!Array.isArray(logsData) || logsData.length === 0) {
      self.postMessage({
        type: 'success',
        tree: [],
        meta: { serviceColors, nextUrl, recentUrl, cols, colIdxMap: data.colIdxMap, count, hasMore: logsData.length !== 0 },
        id,
      });
      return;
    }

    // Use the colIdxMap from the server response, not the one from the message
    const tree = groupSpans(logsData, data.colIdxMap, expandedTraces, flipDirection);

    if (tree.length === 0) {
      console.error('[Worker] Tree is empty after processing', logsData.length, 'items');
    }

    self.postMessage({
      type: 'success',
      tree,
      meta: { serviceColors, nextUrl, recentUrl, cols, colIdxMap: data.colIdxMap, count, hasMore: true },
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
