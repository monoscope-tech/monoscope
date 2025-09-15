// Re-export everything from query-editor
export * from './query-editor/query-editor';
export * from './query-editor/query-builder';
export * from './session-replay';
// Re-export the config functions
export { initializeDefaultSchema } from './query-editor/query-editor-config';
export * from './log-list';
export * from './widgets';
import { flameGraphChart, waterFallGraphChart } from './charts';
export * from './main';

export * from './session-replay';

if (typeof window !== 'undefined') {
  window.flameGraphChart = flameGraphChart;
  window.waterFallGraphChart = waterFallGraphChart;
}

// Note: Popular queries are now provided directly from Haskell backend
// instead of being exposed from TypeScript
