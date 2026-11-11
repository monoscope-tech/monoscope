#!/usr/bin/env node
// Observe supervisor cleanup ordering; assertions live in Haskell.
import { statSync, writeFileSync } from 'node:fs';
import { createInterface } from 'node:readline';

const lines = createInterface({ input: process.stdin })[Symbol.asyncIterator]();
JSON.parse((await lines.next()).value);
console.log(JSON.stringify({ tag: 'Checkout', contents: [] }));
const { Right: { directory, repositorySkills } } = JSON.parse((await lines.next()).value);
process.once('SIGTERM', () => {
  writeFileSync(process.argv[2], JSON.stringify({ directory, repositorySkills,
    checkoutPresent: statSync(directory, { throwIfNoEntry: false })?.isDirectory() ?? false,
    metadataPresent: statSync(repositorySkills, { throwIfNoEntry: false })?.isDirectory() ?? false }));
  process.exit(0);
});
const mode = process.argv[3];
if (mode === 'plan') {
  console.log(JSON.stringify({ tag: 'Tool', contents: { name: 'plan_proofs', arguments: { claims: ['Shows the retry count', 'Retries once'] } } }));
  writeFileSync(process.argv[2], (await lines.next()).value);
  process.exit(0);
}
console.log(JSON.stringify(mode === 'callback' ? { tag: 'Validate', contents: {} }
  : mode === 'cancel' ? { tag: 'Tool', contents: { name: 'query_telemetry', arguments: { service: 'fixture', query: 'logs | take 1' } } }
  : { tag: 'InvalidFixtureMessage', contents: [] }));
setInterval(() => {}, 60_000);
