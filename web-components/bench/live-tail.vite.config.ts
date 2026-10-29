import { defineConfig } from 'vite';
import { resolve } from 'node:path';
export default defineConfig({
  root: resolve(import.meta.dirname, '..'),
  publicDir: resolve(import.meta.dirname, '../../static/public/assets/css'),
  preview: { port: 3098, host: '127.0.0.1' },
  build: { target: 'esnext', outDir: '/tmp/monoscope-live-tail-bench-dist', emptyOutDir: true, rollupOptions: { input: resolve(import.meta.dirname, 'live-tail.html') } },
});
