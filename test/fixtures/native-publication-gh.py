#!/usr/bin/env python3
"""Local gh fixture: all state stays beside this copied executable."""
import json
import os
from pathlib import Path
import sys

root = Path(__file__).resolve().parent
state_file = root / 'github.json'
state = json.loads(state_file.read_text())
args = sys.argv[1:]
assert os.environ['GH_TOKEN'] == 'ghp_fixture'
assert os.environ['GH_HOST'] == 'github.com'
assert os.environ['GH_CONFIG_DIR'].startswith(os.getcwd())
assert 'ghp_fixture' not in ' '.join(args)
with (root / 'commands.jsonl').open('a') as log:
    log.write(json.dumps(args) + '\n')
mode = state.get('mode', '')
if state.get('pr') and mode == 'fork':
    state['pr']['head']['repo']['full_name'] = 'impact-org/fork'
if state.get('pr') and mode == 'merged':
    state['pr'].update(state='closed', merged=True, draft=False)
    state_file.write_text(json.dumps(state))
if args == ['pr', 'edit', '--help']:
    print('Usage: gh pr edit --attach FILE')
elif args[:2] == ['pr', 'edit']:
    asset = Path(args[args.index('--attach') + 1])
    assert asset.is_file() and asset.stat().st_size > 0
    body = Path(args[args.index('--body-file') + 1]).read_text()
    if mode == 'uncertain':
        sys.exit(1)
    url = 'https://github.com/user-attachments/assets/00000000-0000-4000-8000-00000000000' + ('1' if asset.name == 'explanation.mp4' else '2')
    state['pr']['body'] = body.replace('![](explanation.mp4)' if asset.name == 'explanation.mp4' else '(cause.png)', url if asset.name == 'explanation.mp4' else '(' + url + ')')
    state.setdefault('writes', []).append(asset.name)
    state_file.write_text(json.dumps(state))
    if mode == 'after-attachment':
        sys.exit(1)
    print('updated')
elif args[0] == 'api':
    path = args[args.index('--method') + 2]
    method = args[args.index('--method') + 1]
    if '/git/matching-refs/heads/' in path:
        ref = state['branch'] if 'monoscope' in path else 'main'
        sha = state['head'] if ref != 'main' else state['base']
        if mode == 'moved' and ref == 'main':
            sha = state['head']
        print(json.dumps([] if mode == 'merged' else [[{'ref': 'refs/heads/' + ref, 'object': {'sha': sha}}]]))
    elif '/pulls?' in path:
        print(json.dumps([[state['pr']] if state.get('pr') else []]))
    elif method == 'POST':
        body = json.loads(Path(args[args.index('--input') + 1]).read_text())
        assert body['draft'] is True and body['head'] == state['branch'] and body['base'] == 'main'
        state['pr'] = {'number': 7, 'html_url': 'https://github.com/impact-org/checkout/pull/7', 'body': body['body'], 'draft': True, 'state': 'open', 'merged': False,
                       'head': {'sha': state['head'], 'ref': state['branch'], 'repo': {'full_name': 'impact-org/checkout'}},
                       'base': {'sha': state['base'], 'ref': 'main', 'repo': {'full_name': 'impact-org/checkout'}}}
        state.setdefault('writes', []).append('create')
        state_file.write_text(json.dumps(state))
        if mode == 'after-create':
            sys.exit(1)
        print(json.dumps(state['pr']))
    elif '/pulls/' in path:
        print(json.dumps(state['pr']))
    else:
        print(json.dumps({'full_name': 'impact-org/checkout', 'visibility': 'public' if mode == 'visibility' else 'private', 'permissions': {'push': mode != 'denied'}}))
else:
    raise AssertionError(args)
