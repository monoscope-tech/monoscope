# Frozen proof check. The host prepends `set -- BASE64_COMMAND BASE64_SETUP`. The setup runs first,
# unrecorded, and must leave the commit and tracked files as they were, so the recording shows the
# revision under review; the command is recorded with asciinema, and the cast returns base64-encoded
# between markers in this check's own log. A UI proof also writes screenshots (*.png, named by what
# they show), one video (*.webm or *.mp4), optional chapters.tsv (seconds<TAB>label) and page.tsv
# (title<TAB>url, for the browser frame) to $PROOF_MEDIA; those return the same way.
set -u
export HOME="${HOME:-/tmp}" TERM=xterm-256color LANG=C.UTF-8 LC_ALL=C.UTF-8 PROOF_MEDIA=/tmp/monoscope-proof-media
mkdir -p "$PROOF_MEDIA"
emit() { printf '\n::monoscope-%s::%s\n' "$1" "$2"; base64 -w0 "$3"; printf '\n::monoscope-%s-end::\n' "$1"; }
command -v asciinema >/dev/null || exit 127
if [ -n "${2:-}" ]; then
  printf %s "$2" | base64 -d > /tmp/monoscope-setup.sh
  source_state() { git -c safe.directory='*' rev-parse HEAD 2>&1; git -c safe.directory='*' status --porcelain --untracked-files=no 2>&1; }
  before=$(source_state)
  if ! bash /tmp/monoscope-setup.sh > /tmp/monoscope-setup.log 2>&1; then
    tail -c 4000 /tmp/monoscope-setup.log
    echo "monoscope-proof-failed: setup exited nonzero"
    exit 1
  fi
  if [ "$(source_state)" != "$before" ]; then
    echo "monoscope-proof-failed: setup changed the commit or tracked files; seed data and start services without editing source"
    exit 1
  fi
fi
printf %s "$1" | base64 -d > /tmp/monoscope-proof.sh
asciinema rec --quiet --overwrite --cols 110 --rows 32 --idle-time-limit 2 \
  --command 'bash /tmp/monoscope-proof.sh; echo $? > /tmp/monoscope-proof.status' \
  /tmp/monoscope-proof.cast </dev/null >/dev/null 2>/tmp/monoscope-proof.err || cat /tmp/monoscope-proof.err
status=$(cat /tmp/monoscope-proof.status 2>/dev/null || echo 125)
if [ -s /tmp/monoscope-proof.cast ] && [ "$(stat -c %s /tmp/monoscope-proof.cast)" -le 524288 ]; then
  emit cast "" /tmp/monoscope-proof.cast
fi
shots=0
for shot in "$PROOF_MEDIA"/*.png; do
  [ -f "$shot" ] && [ "$shots" -lt 4 ] && [ "$(stat -c %s "$shot")" -le 2097152 ] || continue
  shots=$((shots + 1))
  emit shot "$(basename "$shot" .png | tr -c 'A-Za-z0-9 _.-' ' ' | cut -c1-80)" "$shot"
done
video=$(ls "$PROOF_MEDIA"/*.webm "$PROOF_MEDIA"/*.mp4 2>/dev/null | head -n 1)
[ -n "$video" ] && [ "$(stat -c %s "$video")" -le 12582912 ] && emit video "${video##*.}" "$video"
[ -s "$PROOF_MEDIA/chapters.tsv" ] && [ "$(stat -c %s "$PROOF_MEDIA/chapters.tsv")" -le 4096 ] && emit chapters "" "$PROOF_MEDIA/chapters.tsv"
[ -s "$PROOF_MEDIA/page.tsv" ] && [ "$(stat -c %s "$PROOF_MEDIA/page.tsv")" -le 2048 ] && emit page "" "$PROOF_MEDIA/page.tsv"
[ "$status" = 0 ] || echo "monoscope-proof-failed: exit $status"
exit "$status"
