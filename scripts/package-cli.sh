#!/usr/bin/env bash
set -euo pipefail
source_binary=${1:?usage: package-cli.sh SOURCE OUTPUT}
output=${2:?usage: package-cli.sh SOURCE OUTPUT}
output="$(cd "$(dirname "$output")" && pwd)/$(basename "$output")"
cp "$source_binary" "$output"
case "$(uname -s)" in
  Darwin)
    strip -x "$output"
    zlib_links=$(otool -L "$output" | awk 'NR > 1 && $1 ~ /\/libz\..*dylib$/ && $1 !~ /^\/usr\/lib\// {print $1}')
    while IFS= read -r dependency; do
      [ -n "$dependency" ] || continue
      install_name_tool -change "$dependency" /usr/lib/libz.1.dylib "$output"
    done <<< "$zlib_links"
    codesign -f -s - "$output"
    codesign --verify --strict "$output"
    ;;
  *) strip -s "$output" ;;
esac
"$(dirname "$0")/check-cli-linkage.sh" "$output"
DYLD_BIND_AT_LAUNCH=1 "$output" --version
