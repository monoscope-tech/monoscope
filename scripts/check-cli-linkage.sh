#!/usr/bin/env bash
# Fail if the monoscope CLI binary links a library a user would have to install.
#
# The CLI is packaged as a single download with no prerequisites. That only
# holds while it stays off lib:monoscope, which pulls in libpq, librdkafka,
# grpc, protobuf, snappy, lz4 and zstd. A stray `import Pkg.DeriveUtils` (or a
# new monoscope-shared dependency that reaches the DB stack) puts them back
# without any other visible symptom — so this check runs in CI on every build.
#
# Usage: scripts/check-cli-linkage.sh path/to/monoscope
set -euo pipefail

BIN=${1:?usage: check-cli-linkage.sh <binary>}

# libgmp and zlib are not listed: GHC always links libgmp dynamically on Linux,
# and TLS needs zlib. Both ship with every mainstream distro and with macOS.
FORBIDDEN='libpq|librdkafka|libgrpc|libprotobuf|libsnappy|liblz4|libzstd|libsasl|libcrypto|libssl'

case "$(uname -s)" in
  Darwin) LINKED=$(otool -L "$BIN" | tail -n +2 | awk '{print $1}') ;;
  *)      LINKED=$(ldd "$BIN" 2>/dev/null | awk '{print $1}') ;;
esac

echo "$BIN links:"
echo "$LINKED" | sed 's/^/  /'

# A Homebrew path is just as fatal as a forbidden library: the user's Mac
# almost certainly does not have /opt/homebrew populated the way the runner did.
if BAD=$(echo "$LINKED" | grep -E "$FORBIDDEN|^/opt/homebrew|^/usr/local/opt"); then
  echo
  echo "ERROR: the CLI binary links libraries users would have to install:"
  echo "$BAD" | sed 's/^/  /'
  echo
  echo "The CLI must depend only on monoscope-shared, never lib:monoscope."
  echo "A /opt/homebrew path means the build linked a keg-only copy of a"
  echo "library the target machine will not have; repoint it at /usr/lib."
  echo "Check any module added to cli/ or shared/ for a DB/Kafka/gRPC import."
  exit 1
fi

echo
echo "OK: no libpq/librdkafka/grpc/protobuf/compression linkage."
ls -lh "$BIN" | awk '{print "size: " $5}'
