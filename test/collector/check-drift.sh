#!/usr/bin/env bash
# Assert that the body-promotion list in transform/tier1 is a
# priority-ordered *prefix* of `Utils.messageKeys` in src/Utils.hs.
#
# Tier-1 deliberately carries only the top-N most-common message aliases
# (long tail is handled at display time by extractMessageFromLog). What
# matters for correctness is:
#   1. Every key in Tier-1 also exists in Utils.messageKeys.
#   2. They appear in the same priority order — otherwise a Tier-1 match
#      would disagree with the display-time fallback for the same log.
#
# Concretely: the list of keys extracted from otelcol.yaml must equal the
# first N entries of Utils.messageKeys, where N = how many keys are in the
# YAML. Run standalone, or via `make test-collector-drift`.

set -euo pipefail
cd "$(dirname "$0")/../.."

# Keys from otelcol.yaml — every `set(body, attributes["KEY"])` line in order.
yaml_keys=$(grep -oE 'set\(body, attributes\["[^"]+"\]\)' static/public/otelcol.yaml \
            | sed -E 's/.*"([^"]+)".*/\1/')

# Keys from src/Utils.hs — the messageKeys list, in source order.
hs_keys=$(awk '
  /^messageKeys *=/                   { in_list = 1; next }
  in_list && /^[[:space:]]*\]/        { exit }
  in_list                             { print }
' src/Utils.hs | grep -oE '"[^"]+"' | tr -d '"')

yaml_count=$(echo "$yaml_keys" | grep -c .)
hs_prefix=$(echo "$hs_keys" | head -n "$yaml_count")

if diff -u <(echo "$hs_prefix") <(echo "$yaml_keys") >/dev/null; then
  echo "PASS — transform/tier1 (${yaml_count} keys) is a priority-ordered prefix of Utils.messageKeys ($(echo "$hs_keys" | wc -l | tr -d ' ') keys total)"
  exit 0
fi

echo "FAIL — transform/tier1 is not a priority-ordered prefix of Utils.messageKeys"
echo "  - = first ${yaml_count} entries of src/Utils.hs (messageKeys)"
echo "  + = static/public/otelcol.yaml (transform/tier1)"
diff -u <(echo "$hs_prefix") <(echo "$yaml_keys")
exit 1
