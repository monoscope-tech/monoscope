// Re-export everything from query-editor
export * from './query-editor/query-editor';

// Re-export the config functions
export { initializeDefaultSchema, getPopularQueries } from './query-editor/query-editor-config';
export * from './log-list';
// Expose getPopularQueries globally for Haskell initialization
import { getPopularQueries } from './query-editor/query-editor-config';
(window as any).getPopularQueries = getPopularQueries;
