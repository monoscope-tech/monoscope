// Re-export everything from query-editor
export * from './query-editor/query-editor';
export * from './query-editor/query-builder';

// Re-export the config functions
export { initializeDefaultSchema } from './query-editor/query-editor-config';
export * from './log-list';
export * from './monitors/steps-editor';
export * from './monitors/steps-assertions';
export * from './widgets';
import { flameGraphChart, waterFallGraphChart } from './charts';
export * from './main';

if (typeof window !== 'undefined') {
  window.flameGraphChart = flameGraphChart;
  window.waterFallGraphChart = waterFallGraphChart;
}

// Export validateYaml and related utility functions
export {
  validateYaml,
  convertCollectionStepsToTestkitFormat,
  isValidStep,
  getEvent,
  triggerToastEvent,
} from './monitors/test-editor-utils';
// Note: Popular queries are now provided directly from Haskell backend
// instead of being exposed from TypeScript
