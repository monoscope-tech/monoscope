#!/usr/bin/env bash
# Installs disk-guard on the CapRover host. Alerts need ALERT_WEBHOOK_URL (Slack or Discord webhook) in
# /etc/default/disk-guard there (left untouched if it exists).
set -euo pipefail
HOST=${HOST:-ubuntu@captain.s.past3.tech}
cd "$(dirname "$0")"
scp -q disk-guard.sh disk-guard.service disk-guard.timer "$HOST:/tmp/"
ssh "$HOST" 'sudo install -m 755 /tmp/disk-guard.sh /usr/local/bin/ &&
  sudo install -m 644 /tmp/disk-guard.service /tmp/disk-guard.timer /etc/systemd/system/ &&
  sudo touch /etc/default/disk-guard && sudo systemctl daemon-reload &&
  sudo systemctl enable --now disk-guard.timer && sudo systemctl start disk-guard.service &&
  systemctl list-timers disk-guard.timer --no-pager && journalctl -t disk-guard -n 5 --no-pager'
