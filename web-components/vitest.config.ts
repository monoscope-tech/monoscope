import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // Happy-DOM is faster than jsdom and works better with Monaco
    environment: 'happy-dom',
    globals: true,
    // Use existing setup file
    setupFiles: './test/setup.ts',
    deps: {
      optimizer: {
        web: {
          // force Vitest to bundle Monaco instead of trying to stub it
          include: ['monaco-editor'],
        }
      }
    },
  },
  optimizeDeps: {
    include: [
      'monaco-editor',
      'monaco-editor/esm/vs/language/json/json.worker',
      'monaco-editor/esm/vs/language/css/css.worker', 
      'monaco-editor/esm/vs/language/html/html.worker',
      'monaco-editor/esm/vs/language/typescript/ts.worker',
      'monaco-editor/esm/vs/editor/editor.worker',
    ],
  },
});