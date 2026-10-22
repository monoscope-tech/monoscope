#!/usr/bin/env python3
"""Launch the real SDK worker with test-owned configuration and no provider secrets."""
import os
import argparse
from pathlib import Path
import sys

parser = argparse.ArgumentParser()
for name in ("models", "usage", "cache", "image", "diagnostic"):
    parser.add_argument(name)
for name in ("prepare", "dependencies", "cli", "temp"):
    parser.add_argument("--" + name)
parser.add_argument("--skill", action="append", default=[])
args = parser.parse_args()
environment = {key: value for key, value in os.environ.items()
               if key in {"PATH", "HOME", "TMPDIR", "IMPACT_REVIEW_OUTBOX_DIR"}}
environment.update(IMPACT_REVIEW_MODELS=args.models, IMPACT_REVIEW_USAGE_DIR=args.usage,
                   IMPACT_REVIEW_CACHE_DIR=args.cache, IMPACT_REVIEW_GUEST_IMAGE=args.image,
                   IMPACT_REVIEW_FIXTURE_SECRET="host-only-fixture")
for key, value in (("IMPACT_REVIEW_PREPARE_SCRIPT", args.prepare),
                   ("IMPACT_REVIEW_DEPENDENCY_ARCHIVE", args.dependencies),
                   ("IMPACT_REVIEW_NATIVE_CLI", args.cli), ("TMPDIR", args.temp)):
    if value is not None:
        environment[key] = value
if sys.platform == "darwin":
    environment["GONDOLIN_VMM"] = "krun"
with open(args.diagnostic, "ab", buffering=0) as output:
    os.dup2(output.fileno(), 2)
worker = Path(__file__).resolve().parents[2] / "impact-review-worker" / "cli.mjs"
os.execvpe("node", ["node", str(worker), *args.skill], environment)
