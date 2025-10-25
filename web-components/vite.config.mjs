import { defineConfig } from 'vite';
import { nodePolyfills } from 'vite-plugin-node-polyfills';
import { resolve } from 'path';
import { fileURLToPath } from 'url';

const __dirname = fileURLToPath(new URL('.', import.meta.url));

export default defineConfig({
  base: '/public/assets/web-components/dist/',
  server: {
    port: 3000,
    open: true,
    watch: {
      // Rebuild on file changes
      usePolling: true,
    },
    hmr: {
      // Hot Module Replacement settings
      overlay: true,
    },
  },
  plugins: [
    nodePolyfills({ overrides: { fs: null } }), // for @kusto/language-service
  ],
  resolve: {
    alias: [
      // Fix the monaco-editor alias to properly resolve imports
      { find: 'monaco-editor', replacement: resolve(__dirname, 'node_modules/monaco-editor') },
      { find: /^monaco-editor\/esm\/vs\/(.*)/, replacement: resolve(__dirname, 'node_modules/monaco-editor/esm/vs/$1') },
    ],
  },
  optimizeDeps: {
    include: ['monaco-editor', 'xregexp'],
    exclude: ['monaco-editor/esm/vs/editor/editor.worker'],
  },
  build: {
    target: 'esnext',
    outDir: '../static/public/assets/web-components/dist',
    emptyOutDir: true,
    assetsDir: 'assets',
    sourcemap: true, // Enable source maps for debugging
    chunkSizeWarningLimit: 10000, // Increased chunk size limit (in kB)
    rollupOptions: {
      output: {
        entryFileNames: `js/[name].js`,
        chunkFileNames: `js/[name].[hash].js`,
        assetFileNames: (assetInfo) => {
          const info = assetInfo.name.split('.');
          const ext = info[info.length - 1];
          if (/\.(css)$/.test(assetInfo.name)) {
            return `css/[name].[ext]`;
          }
          if (/\.(woff2?|eot|ttf|otf)$/.test(assetInfo.name)) {
            return `fonts/[name].[ext]`;
          }
          return `[ext]/[name].[ext]`;
        },
      },
    },
  },
});
