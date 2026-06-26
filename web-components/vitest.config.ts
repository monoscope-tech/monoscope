import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // jsdom has better compatibility with Monaco Editor
    environment: 'jsdom',
    globals: true,
    // vitest-canvas-mock gives a realistic 2D context (echarts/canvas run for real);
    // then our own setup (globals + controllable observers).
    setupFiles: ['vitest-canvas-mock', './test/setup.ts'],
    deps: {
      optimizer: {
        web: {
          // force Vitest to bundle Monaco instead of trying to stub it
          include: ['monaco-editor', 'vitest-canvas-mock'],
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