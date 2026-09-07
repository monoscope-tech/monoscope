import { execFileSync } from 'node:child_process';
import { performance } from 'node:perf_hooks';
import { Worker } from 'node:worker_threads';
import { writeFileSync } from 'node:fs';
import ts from 'typescript';
import { SchemaIndex } from '../src/query-editor/schema-index.ts';
const source = execFileSync(
  'git',
  ['show', 'd1344345b9f38dd6e948afd1b8f719be6f02cfa2:web-components/src/query-editor/query-editor-config.ts'],
  { encoding: 'utf8' }
).replace(/^import .*$/m, '');
const js = ts
  .transpileModule(source, { compilerOptions: { target: ts.ScriptTarget.ES2022, module: ts.ModuleKind.ES2022 } })
  .outputText.replace('export function', 'function');
async function baseline(fields, prefix) {
  const worker = new Worker(
    `const {parentPort,workerData}=require('node:worker_threads'); let resolve;
    const manager={setSchemas(){},setDefaultSchema(){},setNestedResolver(fn){resolve=fn},setValueResolver(){},getSchemaData(){return {fields:workerData.fields}}};
    new Function('schemaManager',workerData.js+';initializeDefaultSchema();')(manager);
    const start=performance.now();resolve('spans',workerData.prefix).then(result=>parentPort.postMessage({ms:performance.now()-start,count:result.length}));`,
    { eval: true, workerData: { js, fields, prefix } }
  );
  return new Promise((resolve) => {
    const timer = setTimeout(() => {
      void worker.terminate();
      resolve({ timeoutMs: 5000 });
    }, 5000);
    worker.once('message', (result) => {
      clearTimeout(timer);
      void worker.terminate();
      resolve(result);
    });
  });
}
const results = [];
for (const shape of ['flat', 'nested', 'deep'])
  for (const count of [100, 1000, 5000, 10000, 100000]) {
    const prefix = shape === 'flat' ? '' : shape === 'nested' ? 'attributes' : 'attributes.a.b.c.d.e.f.g.h';
    const fields = Object.fromEntries(
      Array.from({ length: count }, (_, i) => [(prefix ? prefix + '.' : '') + `field_${i}`, { type: 'string' }])
    );
    const start = performance.now();
    const index = new SchemaIndex({ fields });
    const buildMs = performance.now() - start;
    const lookupStart = performance.now();
    const found = index.fields(prefix);
    const lookupMs = performance.now() - lookupStart;
    const old = count <= 5000 ? await baseline(fields, prefix) : { skipped: 'Quadratic baseline capped at 5,000 fields' };
    const result = { shape, count, buildMs, lookupMs, found: found.length, baseline: old };
    results.push(result);
    console.log(JSON.stringify(result));
  }
writeFileSync(
  'bench/schema-results.json',
  JSON.stringify({ runtime: process.version, platform: `${process.platform}/${process.arch}`, results }, null, 2) + '\n'
);
