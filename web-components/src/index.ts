// Re-export everything from query-editor
export * from './query-editor/query-editor';

// Re-export the config functions
export { initializeDefaultSchema } from './query-editor/query-editor-config';

// Note: Popular queries are now provided directly from Haskell backend
// instead of being exposed from TypeScript