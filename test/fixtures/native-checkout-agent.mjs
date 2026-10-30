#!/usr/bin/env node
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { appendFile, readFile, access, writeFile } from 'node:fs/promises';
import { join, isAbsolute } from 'node:path';
import { createInterface } from 'node:readline';
const lines = createInterface({ input: process.stdin, crlfDelay: Infinity });
const input = lines[Symbol.asyncIterator]();
try {
  const envelope = JSON.parse((await input.next()).value);
  const request = envelope.tag === 'FixTask' ? envelope.contents.request : envelope;
  await appendFile(process.argv[2] + '.process', 'started\n');
  if (process.argv.includes('--skip-checkout')) {
    process.stdout.write(JSON.stringify({ tag: 'Validate', contents: {} }) + '\n');
    await input.next();
    throw new Error('Host must reject validation before checkout');
  }
  if (process.argv.includes('--replay-call') || process.argv.includes('--journal-exit')) {
    const frame = { tag: 'Call', contents: { id: '00000000-0000-4000-8000-000000000042', attemptId: request.attemptId, provider: request.provider, model: request.model } };
    await writeFile(join(process.env.IMPACT_REVIEW_OUTBOX_DIR, frame.contents.id + '.json'), JSON.stringify(frame));
    if (process.argv.includes('--journal-exit')) process.exit(1);
    process.stdout.write(JSON.stringify(frame) + '\n');
    assert.ok(Object.hasOwn(JSON.parse((await input.next()).value), 'Right'));
  }
  process.stdout.write(JSON.stringify({ tag: 'Checkout', contents: [] }) + '\n');
  const reply = JSON.parse((await input.next()).value);
  const prepared = reply.Right;
  await appendFile(process.argv[2], 'launched\n');
  assert.equal(request.gitToken, '');
  assert.ok(isAbsolute(prepared.directory));
  assert.equal(typeof prepared.repositorySkills, 'string', 'Haskell must supply committed skill metadata');
  assert.ok(isAbsolute(prepared.repositorySkills));
  assert.equal(prepared.rootInstructions, process.argv.includes('--override-instructions') ? 'Override rule.\n'.repeat(3000).slice(0, 32768) : 'Read actual callers before reviewing.\n');
  assert.equal(await readFile(join(prepared.repositorySkills, '.agents/skills/review/SKILL.md'), 'utf8'), 'Committed skill\n');
  assert.equal(await readFile(join(prepared.repositorySkills, '.pi/skills/review/reference.md'), 'utf8'), 'Committed reference\n');
  for (const path of ['.agents/skills/linked/SKILL.md', '.agents/skills/untracked/SKILL.md', '.agents/skills/review/execute.sh']) {
    await assert.rejects(access(join(prepared.repositorySkills, path)), { code: 'ENOENT' });
  }
  for (const field of ['projectId', 'repository', 'base', 'head']) assert.equal(prepared[field], request[field]);
  const git = (...args) => execFileSync('git', ['-C', prepared.directory, ...args], { encoding: 'utf8' }).trim();
  assert.equal(git('rev-parse', 'HEAD'), request.head);
  git('cat-file', '-e', request.base + '^{commit}');
  const config = await readFile(join(prepared.directory, '.git/config'), 'utf8');
  assert.ok(!config.includes('fixture-secret-token') && !config.includes('extraheader'));
  await assert.rejects(access(join(prepared.directory, '.git/objects/info/alternates')), { code: 'ENOENT' });
  const instructions = await readFile(join(prepared.directory, 'AGENTS.md'), 'utf8');
  const draft = { ...prepared, instructions };
  process.stdout.write(JSON.stringify({ tag: 'Validate', contents: draft }) + '\n');
  const accepted = JSON.parse((await input.next()).value);
  if (process.argv.includes('--fail') || Object.hasOwn(accepted, 'Left')) process.exitCode = 1;
  else process.stdout.write(JSON.stringify({ tag: 'Result', contents: draft }) + '\n');
} finally { lines.close(); process.stdin.destroy(); }
