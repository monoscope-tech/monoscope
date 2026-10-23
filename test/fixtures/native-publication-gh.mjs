#!/usr/bin/env node
// Local gh fixture: all state stays beside this copied executable.
import assert from 'node:assert/strict';
import { appendFileSync, readFileSync, realpathSync, statSync, writeFileSync } from 'node:fs';
import { basename, dirname, join } from 'node:path';

const root = dirname(realpathSync(process.argv[1])), stateFile = join(root, 'github.json');
const state = JSON.parse(readFileSync(stateFile)), args = process.argv.slice(2), mode = state.mode ?? '';
const value = (flag) => args[args.indexOf(flag) + 1];
const save = () => writeFileSync(stateFile, JSON.stringify(state));
const emit = (value) => console.log(JSON.stringify(value));
assert.equal(process.env.GH_TOKEN, 'ghp_fixture');
assert.equal(process.env.GH_HOST, 'github.com');
assert.ok(process.env.GH_CONFIG_DIR.startsWith(process.cwd()));
assert.ok(!args.join(' ').includes('ghp_fixture'));
appendFileSync(join(root, 'commands.jsonl'), JSON.stringify(args) + '\n');
if (state.pr && mode === 'fork') state.pr.head.repo.full_name = 'impact-org/fork';
if (state.pr && mode === 'merged') { Object.assign(state.pr, { state: 'closed', merged: true, draft: false }); save(); }
if (JSON.stringify(args) === JSON.stringify(['pr', 'edit', '--help'])) console.log('Usage: gh pr edit --attach FILE');
else if (args[0] === 'pr' && args[1] === 'edit') {
  const asset = value('--attach'), name = basename(asset), body = readFileSync(value('--body-file'), 'utf8');
  assert.ok(statSync(asset).isFile() && statSync(asset).size > 0);
  if (mode === 'uncertain') process.exit(1);
  const video = name === 'explanation.mp4', url = 'https://github.com/user-attachments/assets/00000000-0000-4000-8000-00000000000' + (video ? '1' : '2');
  state.pr.body = body.replaceAll(video ? '![](explanation.mp4)' : '(cause.png)', video ? url : '(' + url + ')');
  (state.writes ??= []).push(name); save();
  if (mode === 'after-attachment') process.exit(1);
  console.log('updated');
} else if (args[0] === 'api') {
  const [method, path] = args.slice(args.indexOf('--method') + 1);
  if (path.includes('/git/matching-refs/heads/')) {
    const ref = path.includes('monoscope') ? state.branch : 'main';
    const sha = ref !== 'main' || mode === 'moved' ? state.head : state.base;
    emit(mode === 'merged' ? [] : [[{ ref: 'refs/heads/' + ref, object: { sha } }]]);
  } else if (path.includes('/pulls?')) emit([state.pr ? [state.pr] : []]);
  else if (method === 'POST') {
    const body = JSON.parse(readFileSync(value('--input')));
    assert.ok(body.draft === true && body.head === state.branch && body.base === 'main');
    state.pr = { number: 7, html_url: 'https://github.com/impact-org/checkout/pull/7', body: body.body, draft: true, state: 'open', merged: false,
      head: { sha: state.head, ref: state.branch, repo: { full_name: 'impact-org/checkout' } },
      base: { sha: state.base, ref: 'main', repo: { full_name: 'impact-org/checkout' } } };
    (state.writes ??= []).push('create'); save();
    if (mode === 'after-create') process.exit(1);
    emit(state.pr);
  } else if (path.includes('/pulls/')) emit(state.pr);
  else emit({ full_name: 'impact-org/checkout', visibility: mode === 'visibility' ? 'public' : 'private', permissions: { push: mode !== 'denied' } });
} else assert.fail(JSON.stringify(args));
