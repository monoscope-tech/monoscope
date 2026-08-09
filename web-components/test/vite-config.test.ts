import { readFileSync } from 'node:fs';
import { describe, expect, test } from 'vitest';

const pkg = JSON.parse(readFileSync('package.json', 'utf8'));
const config = readFileSync('vite.config.mjs', 'utf8');
const makefile = readFileSync('../Makefile', 'utf8');

describe('Vite build output', () => {
  test('watch builds retain old hashed chunks until the server reloads its manifest', () => {
    expect(pkg.scripts.watch).toContain('--mode development');
    expect(config).toMatch(/emptyOutDir:\s*mode !== 'development'/);
  });

  test('starting the watcher stops stale watchers from this checkout', () => {
    expect(makefile).toMatch(/^web-components-watch:\s+kill-web-components-watch$/m);
  });
});
