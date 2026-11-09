# Frozen proof check. The host prepends `set -- BASE64_COMMAND BASE64_SETUP`. The setup runs first,
# unrecorded; the command is recorded with asciinema, and the cast returns base64-encoded between
# markers in this check's own log.
set -u
export HOME="${HOME:-/tmp}" TERM=xterm-256color LANG=C.UTF-8 LC_ALL=C.UTF-8
command -v asciinema >/dev/null || exit 127
if [ -n "${2:-}" ]; then
  printf %s "$2" | base64 -d > /tmp/monoscope-setup.sh
  if ! bash /tmp/monoscope-setup.sh > /tmp/monoscope-setup.log 2>&1; then
    tail -c 4000 /tmp/monoscope-setup.log
    echo "monoscope-proof-failed: setup exited nonzero"
    exit 1
  fi
fi
printf %s "$1" | base64 -d > /tmp/monoscope-proof.sh
asciinema rec --quiet --overwrite --cols 110 --rows 32 --idle-time-limit 2 \
  --command 'bash /tmp/monoscope-proof.sh; echo $? > /tmp/monoscope-proof.status' \
  /tmp/monoscope-proof.cast </dev/null >/dev/null 2>/tmp/monoscope-proof.err || cat /tmp/monoscope-proof.err
status=$(cat /tmp/monoscope-proof.status 2>/dev/null || echo 125)
if [ -s /tmp/monoscope-proof.cast ] && [ "$(stat -c %s /tmp/monoscope-proof.cast)" -le 524288 ]; then
  printf '\n::monoscope-cast::\n'
  base64 -w0 /tmp/monoscope-proof.cast
  printf '\n::monoscope-cast-end::\n'
fi
[ "$status" = 0 ] || echo "monoscope-proof-failed: exit $status"
exit "$status"
