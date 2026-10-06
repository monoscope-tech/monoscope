#!/usr/bin/env python3
"""Exercise release decisions with a local Git remote and a fake image registry."""
import json
import os
import shutil
from pathlib import Path
import subprocess
import tempfile
import tarfile
import unittest

SCRIPT = Path(__file__).with_name('release.sh')


class ReleaseTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.repo = self.root / 'repo'
        self.repo.mkdir()
        (self.repo / 'scripts/ci').mkdir(parents=True)
        shutil.copy2(SCRIPT, self.repo / 'scripts/ci/release.sh')
        self.env = dict(os.environ, REGISTRY=str(self.root / 'registry.json'),
                        CALLS=str(self.root / 'calls'), CI_ROOT=str(self.repo), GITHUB_OUTPUT=str(self.root / 'outputs'))
        self.git('init', '-q')
        self.git('config', 'user.email', 'test@example.com')
        self.git('config', 'user.name', 'test')
        (self.repo / 'source').write_text('original')
        self.git('add', '.')
        self.git('commit', '-qm', 'first')
        self.sha = self.git('rev-parse', 'HEAD').strip()
        subprocess.run(['git', 'init', '--bare', '-q', str(self.root / 'remote')], check=True)
        self.git('remote', 'add', 'origin', str(self.root / 'remote'))
        self.git('push', '-q', 'origin', 'HEAD:master')
        self.bin = self.root / 'bin'
        self.bin.mkdir()
        docker = self.bin / 'docker'
        docker.write_text('''#!/usr/bin/env python3
import json, os, pathlib, sys
args = sys.argv[1:]
with open(os.environ['CALLS'], 'a') as f: f.write(json.dumps(args) + '\\n')
p = pathlib.Path(os.environ['REGISTRY'])
state = json.loads(p.read_text()) if p.exists() else {}
if args[:2] == ['manifest', 'inspect']:
    sys.exit(0 if args[2] in state else 1)
elif args[:3] == ['buildx', 'imagetools', 'inspect']:
    ref = args[3]
    if ref == 'ghcr.io/monoscope-tech/monoscope-deps:latest': digest = state.get('deps', 'sha256:' + 'a'*64)
    elif ref == 'debian:12-slim': digest = 'sha256:' + 'b'*64
    elif ref in state: digest = state[ref]
    else: sys.exit(1)
    print(json.dumps({'digest': digest}))
elif args[:2] == ['buildx', 'build']:
    pathlib.Path(os.environ['CALLS']+'-context.tar').write_bytes(sys.stdin.buffer.read())
    if os.environ.get('FAIL_BUILD'): sys.exit(2)
    digest = 'sha256:' + 'c'*64
    for i, arg in enumerate(args):
        if arg == '-t': state[args[i+1]] = digest
    p.write_text(json.dumps(state))
    if '--metadata-file' in args: pathlib.Path(args[args.index('--metadata-file')+1]).write_text(json.dumps({'containerimage.digest':digest}))
elif args[:3] == ['buildx', 'imagetools', 'create']:
    state[args[args.index('-t')+1]] = args[-1].split('@')[1]
    p.write_text(json.dumps(state))
else: sys.exit(1)
''')
        docker.chmod(0o755)
        self.env['PATH'] = str(self.bin) + os.pathsep + self.env['PATH']

    def git(self, *args):
        return subprocess.check_output(['git', *args], cwd=self.repo, text=True, stderr=subprocess.DEVNULL)

    def run_release(self, *args, success=True):
        command = ['bash', str(SCRIPT), *args]
        result = subprocess.run(command, cwd=self.repo,
                                env=self.env, text=True, capture_output=True)
        if success:
            self.assertEqual(result.returncode, 0, result.stderr)
        else:
            self.assertNotEqual(result.returncode, 0, result.stdout)
        return result

    def calls(self):
        p = self.root / 'calls'
        return [json.loads(line) for line in p.read_text().splitlines()] if p.exists() else []

    def test_squash_merge_reuses_digest_without_rebuilding_or_moving_latest(self):
        self.run_release('build')
        self.git('commit', '--allow-empty', '-qm', 'squash metadata')
        self.run_release('build')
        builds = [c for c in self.calls() if c[:2] == ['buildx', 'build']]
        self.assertEqual(len(builds), 1)
        self.assertFalse(any(':latest' in c for c in builds))
        state = json.loads((self.root / 'registry.json').read_text())
        self.assertEqual(state['ghcr.io/monoscope-tech/monoscope:' + self.git('rev-parse', 'HEAD').strip()], 'sha256:'+'c'*64)

    def test_source_and_toolchain_changes_both_rebuild(self):
        self.run_release('build')
        (self.repo / 'source').write_text('changed')
        self.git('add', '.')
        self.git('commit', '-qm', 'source change')
        self.run_release('build')
        p = self.root / 'registry.json'
        state = json.loads(p.read_text())
        state['deps'] = 'sha256:' + 'd'*64
        p.write_text(json.dumps(state))
        self.run_release('build')
        self.assertEqual(sum(c[:2] == ['buildx', 'build'] for c in self.calls()), 3)
        build = [c for c in self.calls() if c[:2] == ['buildx', 'build']][-1]
        self.assertIn('DEPS_IMAGE=ghcr.io/monoscope-tech/monoscope-deps@sha256:'+'d'*64, build)

    def test_stale_commit_cannot_publish_latest(self):
        self.run_release('build')
        self.git('commit', '--allow-empty', '-qm', 'new master')
        self.git('push', '-q', 'origin', 'HEAD:master')
        self.run_release('latest', self.sha, 'sha256:'+'c'*64, success=False)
        self.assertFalse(any(c[:3] == ['buildx', 'imagetools', 'create'] and ':latest' in c for c in self.calls()))
        self.run_release('latest', self.git('rev-parse', 'HEAD').strip(), 'sha256:'+'c'*64)
        self.assertTrue(any('ghcr.io/monoscope-tech/monoscope:latest' in c for c in self.calls()))

    def test_unreachable_master_and_dirty_tree_fail_closed(self):
        self.git('remote', 'set-url', 'origin', str(self.root / 'missing'))
        self.run_release('current-master', self.sha, success=False)
        (self.repo / 'source').write_text('dirty')
        self.run_release('build', success=False)
        self.assertEqual(self.calls(), [])

    def test_stale_local_deploy_never_calls_caprover(self):
        self.run_release('build')
        self.git('commit', '--allow-empty', '-qm', 'new master')
        self.git('push', '-q', 'origin', 'HEAD:master')
        curl = self.bin / 'curl'
        curl.write_text('#!/bin/sh\necho called > "$CALLS-caprover"\nexit 1\n')
        curl.chmod(0o755)
        env = dict(self.env, CAPROVER_URL='https://example.invalid',
                   CAPROVER_APP='test', CAPROVER_APP_TOKEN='test')
        result = subprocess.run(['bash', str(SCRIPT.with_name('ci.sh')), 'deploy', self.sha],
                                cwd=self.repo, env=env, capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('stale', result.stderr)
        self.assertFalse((self.root / 'calls-caprover').exists())

    def test_ignored_local_settings_are_not_part_of_build_context(self):
        (self.repo / '.gitignore').write_text('cabal.project.local\n')
        self.git('add', '.')
        self.git('commit', '-qm', 'ignore local settings')
        (self.repo / 'cabal.project.local').write_text('unreviewed build flags')
        self.run_release('build')
        with tarfile.open(self.root / 'calls-context.tar') as context:
            self.assertIn('source', context.getnames())
            self.assertNotIn('cabal.project.local', context.getnames())

    def test_failed_build_does_not_publish_a_digest_or_promote(self):
        self.env['FAIL_BUILD'] = 'true'
        self.run_release('build', success=False)
        self.assertFalse((self.root / 'registry.json').exists())
        self.assertFalse((self.root / 'outputs').exists())

    def test_unavailable_native_builder_falls_back(self):
        self.env['MONOSCOPE_BUILDER'] = 'missing'
        self.run_release('build')
        build = next(c for c in self.calls() if c[:2] == ['buildx', 'build'])
        self.assertNotIn('--builder', build)

    def test_requested_revision_must_match_checkout(self):
        self.git('commit', '--allow-empty', '-qm', 'new')
        self.run_release('build', self.sha, success=False)
        self.assertEqual(self.calls(), [])


if __name__ == '__main__':
    unittest.main()
