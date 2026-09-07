import { resolve } from 'node:path';
import { defineConfig } from '@playwright/test';
export default defineConfig({
  testDir: '.',
  testMatch: 'editor.spec.ts',
  workers: 1,
  timeout: 30000,
  projects: ['chromium', 'firefox', 'webkit'].map((browserName) => ({
    name: browserName,
    use: { browserName: browserName as 'chromium' | 'firefox' | 'webkit' },
  })),
  use: { baseURL: 'http://127.0.0.1:3099', headless: true },
  webServer: {
    cwd: resolve(import.meta.dirname, '..'),
    command:
      'node node_modules/vite/bin/vite.js build --config bench/vite.config.ts && node node_modules/vite/bin/vite.js preview --config bench/vite.config.ts --port 3099',
    url: 'http://127.0.0.1:3099/bench/editor.html',
    reuseExistingServer: true,
  },
});
