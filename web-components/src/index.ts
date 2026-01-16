// Import query-editor first to ensure globalThis.monaco is set
import './query-editor/query-editor';
// Then import yaml-editor which registers YAML language on the global monaco instance
import './yaml-editor';

// Re-export everything from query-editor
export * from './query-editor/query-editor';
export * from './query-editor/query-builder';
export * from './session-replay';
// Re-export the config functions
export { initializeDefaultSchema } from './query-editor/query-editor-config';
export * from './log-list';
export * from './widgets';
export * from './charts';
export * from './main';
export * from './yaml-editor';

export * from './session-replay';

// Note: Popular queries are now provided directly from Haskell backend
// instead of being exposed from TypeScript
