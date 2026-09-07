import { defineConfig } from 'vite';
import { execFileSync } from 'node:child_process';
import ts from 'typescript';
import { resolve } from 'node:path';
// Fixed revision makes the comparison reproducible after the rewrite is committed.
const baseline = 'd1344345b9f38dd6e948afd1b8f719be6f02cfa2';
export default defineConfig({
  root: resolve(import.meta.dirname, '..'),
  server: { port: 3099, host: '127.0.0.1' },
  plugins: [
    {
      name: 'query-baseline',
      resolveId(id) {
        if (id.startsWith('virtual:baseline-') || id === 'virtual:optimized-editor') return '\0' + id;
      },
      load(id) {
        if (!id.startsWith('\0virtual:baseline-') && id !== '\0virtual:optimized-editor') return;
        const file = id.endsWith('config') ? 'query-editor-config.ts' : id.endsWith('completion') ? 'completion.ts' : 'query-editor.ts';
        let source = execFileSync('git', ['show', `${baseline}:web-components/src/query-editor/${file}`], { encoding: 'utf8' })
          .replaceAll("from './completion'", "from 'virtual:baseline-completion'")
          .replaceAll("from './query-editor'", "from 'virtual:baseline-editor'")
          .replaceAll("from '../log-list-utils'", "from '/src/log-list-utils.ts'")
          .replaceAll("@customElement('query-editor')", "@customElement('query-editor-baseline')");
        if (id === '\0virtual:optimized-editor')
          source =
            "import { schemaManager as optimizedSchemaManager } from '/src/query-editor/schema-manager.ts';\n" +
            source
              .replace('const schemaManager = new SchemaManager();', 'const schemaManager = optimizedSchemaManager;')
              .replace("@customElement('query-editor-baseline')", "@customElement('query-editor-monaco')")
              .replace('await computeSuggestions(text, this.schemaAccess)', "await schemaManager.complete(text, 'spans', '', 1)");
        return ts.transpileModule(source, {
          compilerOptions: {
            target: ts.ScriptTarget.ES2022,
            module: ts.ModuleKind.ESNext,
            experimentalDecorators: true,
            useDefineForClassFields: false,
          },
        }).outputText;
      },
    },
  ],
  build: { outDir: '/tmp/monoscope-query-bench-dist', rollupOptions: { input: resolve(import.meta.dirname, 'editor.html') } },
});
