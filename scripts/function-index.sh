#!/usr/bin/env bash
# Extracts top-level function type signatures and type declarations from Haskell source files.
# Useful for spotting duplication — functions with similar signatures across modules.
# Usage: ./scripts/function-index.sh [optional-grep-pattern]

set -euo pipefail

filter="${1:-}"

for file in $(find src -name '*.hs' | sort); do
  # Extract type signatures (column 0, lowercase start, has ::) and type/data/class declarations
  output=$(awk '
    # Top-level type signature (starts with lowercase letter or underscore at column 0)
    /^[a-z_][a-zA-Z0-9_'\'']*[[:space:]]*::/ {
      line = $0
      # Collect continuation lines (indented lines that follow)
      while ((getline next_line) > 0) {
        if (next_line ~ /^[[:space:]]+[^-]/ && next_line !~ /^[a-z_]/) {
          gsub(/^[[:space:]]+/, " ", next_line)
          line = line next_line
        } else {
          print line
          # Check if this line itself is a new signature or declaration
          if (next_line ~ /^[a-z_][a-zA-Z0-9_'\'']*[[:space:]]*::/) {
            line = next_line
            continue
          }
          if (next_line ~ /^(data|newtype|type|class) /) {
            print next_line
          }
          break
        }
      }
      next
    }
    # Type-level declarations
    /^(data|newtype|type|class) / { print }
  ' "$file" 2>/dev/null)

  if [ -n "$output" ]; then
    if [ -z "$filter" ]; then
      echo "=== $file ==="
      echo "$output"
      echo
    else
      matched=$(echo "$output" | grep -i "$filter" || true)
      if [ -n "$matched" ]; then
        echo "=== $file ==="
        echo "$matched"
        echo
      fi
    fi
  fi
done
