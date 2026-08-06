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
    // The entry filename carries its own content hash (see entryFileNames below), so
    // the server needs the manifest to know which file to reference.
    // Not `true`: that writes .vite/manifest.json, and CI's upload-artifact drops hidden
    // files, so the Haskell build would lose the manifest it splices at compile time.
    manifest: 'manifest.json',
    sourcemap: true, // Enable source maps for debugging
    cssCodeSplit: false,
    chunkSizeWarningLimit: 10000, // Increased chunk size limit (in kB)
    rollupOptions: {
      output: {
        // Hash the entry in its *filename*, never via a ?v= query: chunks import the
        // entry back as a bare `./index.js` (shared code lives in the entry chunk), so a
        // queried entry URL is a second module identity and the whole graph — every
        // custom element, worker and htmx listener — gets evaluated twice.
        entryFileNames: `js/[name].[hash].js`,
        chunkFileNames: `js/[name].[hash].js`,
        assetFileNames: (assetInfo) => {
          const info = assetInfo.name.split('.');
          const ext = info[info.length - 1];
          if (/\.(css)$/.test(assetInfo.name)) {
            return `css/index.[ext]`;
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
