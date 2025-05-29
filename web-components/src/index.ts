import { flameGraphChart, waterFallGraphChart } from './charts';

// Re-export everything from query-editor
export * from './query-editor/query-editor';

// Re-export the config functions
export { initializeDefaultSchema } from './query-editor/query-editor-config';
export * from './log-list';
export * from './monitors/steps-editor';
export * from './monitors/steps-assertions';

if (typeof window !== 'undefined') {
  window.flameGraphChart = flameGraphChart;
  window.waterFallGraphChart = waterFallGraphChart;
}

// Note: Popular queries are now provided directly from Haskell backend
// instead of being exposed from TypeScript
