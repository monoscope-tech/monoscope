#!/usr/bin/env python3
"""Observe supervisor cleanup ordering; assertions live in Haskell."""
import json
from pathlib import Path
import signal
import sys

json.loads(sys.stdin.readline())
print(json.dumps({"tag": "Checkout", "contents": []}), flush=True)
prepared = json.loads(sys.stdin.readline())["Right"]

def stopped(_signal, _frame):
    observation = {key: prepared[key] for key in ["directory", "repositorySkills"]}
    observation.update(checkoutPresent=Path(prepared["directory"]).is_dir(),
                       metadataPresent=Path(prepared["repositorySkills"]).is_dir())
    Path(sys.argv[1]).write_text(json.dumps(observation))
    raise SystemExit(0)

signal.signal(signal.SIGTERM, stopped)
mode = sys.argv[2]
message = ({"tag": "Validate", "contents": {}} if mode == "callback" else
           {"tag": "Query", "contents": {"service": "fixture", "query": "logs | take 1"}} if mode == "cancel" else
           {"tag": "InvalidFixtureMessage", "contents": []})
print(json.dumps(message), flush=True)
while True:
    signal.pause()
