#!/usr/bin/env node
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { chmodSync, existsSync, mkdirSync, mkdtempSync, readdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { test } from 'node:test';
import { fileURLToPath } from 'node:url';

const script = fileURLToPath(new URL('./disk-guard.sh', import.meta.url));

function setupFixture(totalMiB = 1000, usedMiB = 500) {
  const root = mkdtempSync(join(tmpdir(), 'disk-guard-test-'));
  const bin = join(root, 'bin'), stateDir = join(root, 'state'), reserve = join(stateDir, 'ballast'), mount = join(root, 'mnt');
  mkdirSync(bin); mkdirSync(mount);
  const diskFile = join(root, 'disk.json'), eventsFile = join(root, 'events.jsonl');
  writeFileSync(diskFile, JSON.stringify({ totalKiB: totalMiB * 1024, baseUsedKiB: usedMiB * 1024, cacheKiB: 0 }));
  const writeStub = (name, body) => { const p = join(bin, name); writeFileSync(p, `#!/usr/bin/env node\n${body}\n`); chmodSync(p, 0o755); };
  writeStub('timeout', `const { spawnSync } = require('node:child_process'); process.exit(spawnSync(process.argv[3], process.argv.slice(4), { stdio: 'inherit' }).status ?? 1);`);
  writeStub('df', `
    const fs = require('node:fs'), path = require('node:path');
    const target = process.argv.at(-1), d = JSON.parse(fs.readFileSync(${JSON.stringify(diskFile)}, 'utf8'));
    const reserve = ${JSON.stringify(reserve)};
    let ballastKiB = 0;
    if (fs.existsSync(reserve)) for (const f of fs.readdirSync(reserve)) if (f.startsWith('ballast-') && f.endsWith('.bin')) ballastKiB += Math.ceil(fs.statSync(path.join(reserve, f)).size / 1024);
    const used = d.baseUsedKiB + (d.cacheKiB ?? 0) + ballastKiB, avail = Math.max(0, d.totalKiB - used);
    const pct = Math.min(100, Math.ceil((used * 100) / d.totalKiB));
    const dev = process.env.SEPARATE_RESERVE_MOUNT && target === reserve ? '/dev/sdb1' : '/dev/sda1';
    console.log('Filesystem 1024-blocks Used Available Capacity Mounted on');
    console.log(\`\${dev} \${d.totalKiB} \${used} \${avail} \${pct}% /\`);
  `);
  writeStub('fallocate', `
    const fs = require('node:fs');
    const bytes = Number(process.argv[3]), file = process.argv[4];
    fs.writeFileSync(file, ''); // O_CREAT happens before allocation fails on ENOSPC
    if (process.env.FAIL_FALLOCATE) process.exit(1);
    fs.truncateSync(file, bytes);
  `);
  writeStub('rm', `
    const fs = require('node:fs');
    fs.appendFileSync(${JSON.stringify(eventsFile)}, JSON.stringify({ op: 'rm', args: process.argv.slice(2) }) + '\\n');
    if (process.env.FAIL_RM && process.argv.some((a) => a.includes('ballast-'))) process.exit(1);
    for (const a of process.argv.slice(2)) if (!a.startsWith('-')) fs.rmSync(a, { force: true });
  `);
  writeStub('docker', `
    const fs = require('node:fs');
    fs.appendFileSync(${JSON.stringify(eventsFile)}, JSON.stringify({ op: 'docker', args: process.argv.slice(2) }) + '\\n');
    if (process.env.FAIL_DOCKER) process.exit(1);
    const d = JSON.parse(fs.readFileSync(${JSON.stringify(diskFile)}, 'utf8'));
    d.cacheKiB = 0;
    fs.writeFileSync(${JSON.stringify(diskFile)}, JSON.stringify(d));
  `);
  writeStub('logger', `require('node:fs').appendFileSync(${JSON.stringify(eventsFile)}, JSON.stringify({ op: 'logger', args: process.argv.slice(2) }) + '\\n');`);
  writeStub('curl', `
    const fs = require('node:fs');
    const stdin = fs.readFileSync(0, 'utf8');
    fs.appendFileSync(${JSON.stringify(eventsFile)}, JSON.stringify({ op: 'curl', args: process.argv.slice(2), stdin }) + '\\n');
    if (process.env.FAIL_CURL) process.exit(22);
  `);
  const setDisk = (patch) => writeFileSync(diskFile, JSON.stringify({ ...JSON.parse(readFileSync(diskFile, 'utf8')), ...patch }));
  const events = () => existsSync(eventsFile) ? readFileSync(eventsFile, 'utf8').trim().split('\n').filter(Boolean).map(JSON.parse) : [];
  const clearEvents = () => rmSync(eventsFile, { force: true });
  const run = (extraEnv = {}) => spawnSync('bash', [script], {
    encoding: 'utf8',
    env: { PATH: `${bin}:${process.env.PATH}`, MOUNT: mount, STATE_DIRECTORY: stateDir, BALLAST: '2', BALLAST_MIB: '100', ...extraEnv },
  });
  return { root, reserve, stateDir, setDisk, events, clearEvents, run, cleanup: () => rmSync(root, { recursive: true, force: true }) };
}

test('disk-guard refills safely with headroom, cleans failed fallocate temp files, and enforces same-mount ballast', () => {
  const fx = setupFixture(1000, 700); // 70% used, 100 MiB = 10% ballast
  try {
    assert.equal(fx.run().status, 0);
    assert.deepEqual(readdirSync(fx.reserve), ['ballast-01.bin']); // 70% -> 80% (< WARN=85%)

    // At 78% base + 0 ballast, +10% would land at 88% >= WARN(85%), so refill is skipped instead of causing warn/reclaim thrash.
    rmSync(join(fx.reserve, 'ballast-01.bin'));
    fx.setDisk({ baseUsedKiB: 780 * 1024 });
    assert.equal(fx.run().status, 0);
    assert.deepEqual(readdirSync(fx.reserve), []);

    // Failed fallocate exits 1 and leaves NO 0-byte ballast-*.bin or .tmp behind.
    fx.setDisk({ baseUsedKiB: 500 * 1024 });
    assert.equal(fx.run({ FAIL_FALLOCATE: '1' }).status, 1);
    assert.deepEqual(readdirSync(fx.reserve), []);

    // Separate mount for RESERVE fails closed instead of pretending ballast protects MOUNT.
    assert.equal(fx.run({ SEPARATE_RESERVE_MOUNT: '1' }).status, 1);
  } finally { fx.cleanup(); }
});

test('disk-guard unlinks ballast BEFORE docker prune at >=95%, bounds alerts across sustained pressure, and hides webhook URL from argv', () => {
  const fx = setupFixture(1000, 600);
  try {
    const webhook = 'https://hooks.slack.com/services/T000/B000/SECRET';
    assert.equal(fx.run({ SLACK_WEBHOOK_URL: webhook }).status, 0); // 60% -> 70%, ballast-01 created
    assert.equal(fx.run({ SLACK_WEBHOOK_URL: webhook }).status, 0); // 70% -> 80%, ballast-02 created
    assert.deepEqual(readdirSync(fx.reserve).sort(), ['ballast-01.bin', 'ballast-02.bin']);
    fx.clearEvents();

    // Jump to 96% (760 base + 200 ballast) with broken docker: ballast STILL unlinks FIRST (<1ms) and drops usage to 86%.
    fx.setDisk({ baseUsedKiB: 760 * 1024 });
    assert.equal(fx.run({ SLACK_WEBHOOK_URL: webhook }).status, 0);
    const ops = fx.events();
    const curls = ops.filter((e) => e.op === 'curl');
    assert.equal(curls.length, 1);
    assert.ok(!curls[0].args.some((a) => a.includes('SECRET')), 'webhook secret must not appear in curl argv');
    assert.match(curls[0].stdin, /SECRET/);
    assert.match(curls[0].args.at(-1), /released ballast-02\.bin -> 86%/);
    fx.clearEvents();

    // Hysteresis at 84% (within 2% of WARN=85%): stays at level 1, zero alerts.
    fx.setDisk({ baseUsedKiB: 740 * 1024 }); // 740 + 100 ballast = 84%
    assert.equal(fx.run({ SLACK_WEBHOOK_URL: webhook }).status, 0);
    assert.equal(fx.events().filter((e) => e.op === 'curl').length, 0);

    // Sustained 91% non-Docker usage (0% reclaimed): alerts ONCE on transition to level 2, then 0 webhooks on repeat ticks.
    fx.setDisk({ baseUsedKiB: 810 * 1024, cacheKiB: 0 }); // 810 + 100 = 91%
    assert.equal(fx.run({ ALERT_WEBHOOK_URL: webhook }).status, 0);
    assert.equal(fx.events().filter((e) => e.op === 'curl').length, 1);
    fx.clearEvents();
    assert.equal(fx.run({ ALERT_WEBHOOK_URL: webhook }).status, 0);
    assert.equal(fx.events().filter((e) => e.op === 'curl').length, 0);
    fx.clearEvents();

    // >=95% with Docker cache AND ballast: rm runs BEFORE docker, dropping 98% -> 92% -> 85%!
    fx.setDisk({ baseUsedKiB: 850 * 1024, cacheKiB: 70 * 1024 }); // 850 + 70 cache + 100 ballast -> 100%
    assert.equal(fx.run({ ALERT_WEBHOOK_URL: webhook }).status, 0);
    const order = fx.events().filter((e) => e.op === 'rm' || e.op === 'docker').map((e) => e.op);
    assert.deepEqual(order, ['rm', 'docker', 'docker']);
    assert.equal(fx.events().filter((e) => e.op === 'curl').length, 1); // single combined webhook, not duplicate!
    fx.clearEvents();

    // 96% with 0 ballast left and hung/failing dockerd: exits 1, alerts ONCE on transition to level 4, silent on repeat ticks.
    fx.setDisk({ baseUsedKiB: 960 * 1024, cacheKiB: 0 });
    assert.equal(fx.run({ ALERT_WEBHOOK_URL: webhook, FAIL_DOCKER: '1' }).status, 1);
    const exhausted = fx.events().filter((e) => e.op === 'curl');
    assert.equal(exhausted.length, 1);
    assert.match(exhausted[0].args.at(-1), /NO ballast left.*docker prune failed/);
    fx.clearEvents();
    assert.equal(fx.run({ ALERT_WEBHOOK_URL: webhook, FAIL_DOCKER: '1' }).status, 1);
    assert.equal(fx.events().filter((e) => e.op === 'curl').length, 0);
  } finally { fx.cleanup(); }
});
