#!/bin/sh
set -eu
while IFS= read -r command; do
  case "$command" in
    *'"tag":"Resolve"'*)
      printf '%s\n%s\n' "$command" '{"tag":"Close","contents":[]}' | "$GONDOLIN_STAGE_TEST_BRIDGE" | sed -n '1p'
      ;;
    *'"tag":"InspectCheckpoint"'*)
      : > "$GONDOLIN_STAGE_TEST_READY"
      while [ ! -e "$GONDOLIN_STAGE_TEST_RELEASE" ]; do sleep 0.02; done
      exit 0
      ;;
    *) exit 1 ;;
  esac
done
